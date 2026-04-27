{
    fpGUI IDE - Call Stack Panel Logic

    Copyright (C) 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Pure logic for the Call Stack debugger panel: parsing PDR-format
      call stack strings and building display text for each frame.

      PDR formats call stack frames as:
        #N FuncName at file.pas:line (0xADDR)  — with source info
        #N FuncName (0xADDR)                   — function known, no line
        #N <unknown> (0xADDR)                  — no debug info at all
}
unit ide.callstack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TCallStackFrame - data for a single call stack frame }

  TCallStackFrame = record
    Index:      Integer;  { 0-based frame number }
    FuncName:   String;   { function/procedure name, or '<unknown>' }
    FileName:   String;   { source filename (basename only; may be empty) }
    LineNumber: Integer;  { 1-based source line (0 = not available) }
    Address:    QWord;    { return address }
    HasSource:  Boolean;  { True when FileName and LineNumber are both valid }
  end;

  TCallStackFrameArray = array of TCallStackFrame;

  { TCallStackFrameData - heap-allocated frame data attached to tree nodes.
    Owned by TMainForm.FCallStackDataList. }

  TCallStackFrameData = class
    Frame: TCallStackFrame;
    constructor Create(const AFrame: TCallStackFrame);
  end;


{ Parse a single PDR-format frame string into a TCallStackFrame.
  On parse failure the result has Index=0, FuncName=AText, all other
  fields zeroed/False. }
function ParseCallStackFrame(const AText: string): TCallStackFrame;

{ Parse an array of PDR frame strings into a TCallStackFrameArray. }
function ParseCallStack(const ALines: array of string): TCallStackFrameArray;

{ Build a single-line display string for a frame:
    '#0  MyProc  (main.pas:42)'       — frame with source info
    '#0  MyProc  (0x00007F1234ABCD)'  — frame with address only
    '#0  <unknown>  (0x00007F...)'    — frame with no debug info  }
function CallStackFrameDisplay(const AFrame: TCallStackFrame): string;


implementation

{ TCallStackFrameData }

constructor TCallStackFrameData.Create(const AFrame: TCallStackFrame);
begin
  inherited Create;
  Frame := AFrame;
end;


{ ---------------------------------------------------------------------------
  Parsing helpers
  --------------------------------------------------------------------------- }

{ Find the last occurrence of APattern in AStr, returning its start position
  (1-based).  Returns 0 when not found. }
function LastPosOf(const APattern, AStr: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  if (APattern = '') or (Length(AStr) < Length(APattern)) then
    Exit;
  for i := Length(AStr) - Length(APattern) + 1 downto 1 do
    if Copy(AStr, i, Length(APattern)) = APattern then
    begin
      Result := i;
      Exit;
    end;
end;

{ Convert a hex digit string (no leading '$' or '0x') to a QWord.
  Returns 0 on any error. }
function HexToQWord(const AHex: string): QWord;
var
  i: Integer;
  Ch: Char;
  Digit: QWord;
begin
  Result := 0;
  for i := 1 to Length(AHex) do
  begin
    Ch := AHex[i];
    case Ch of
      '0'..'9': Digit := Ord(Ch) - Ord('0');
      'A'..'F': Digit := Ord(Ch) - Ord('A') + 10;
      'a'..'f': Digit := Ord(Ch) - Ord('a') + 10;
      else Exit(0);
    end;
    Result := (Result shl 4) or Digit;
  end;
end;


function ParseCallStackFrame(const AText: string): TCallStackFrame;
var
  Rest:     string;
  P:        Integer;
  AddrStr:  string;
  Body:     string;
  AtPos:    Integer;
  ColonPos: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.FuncName := AText;

  if AText = '' then
    Exit;

  Rest := AText;

  { Expect '#N ' at the start }
  if (Length(Rest) < 2) or (Rest[1] <> '#') then
    Exit;

  P := 2;
  while (P <= Length(Rest)) and (Rest[P] in ['0'..'9']) do
    Inc(P);

  if (P > Length(Rest)) or (Rest[P] <> ' ') then
    Exit;   { malformed — no space after digits }

  Result.Index := StrToIntDef(Copy(Rest, 2, P - 2), 0);
  Delete(Rest, 1, P);   { strip '#N ' }

  { Locate address tail: ' (0x<hex>)' — search from the right }
  P := LastPosOf(' (0x', Rest);
  if P > 0 then
  begin
    AddrStr := Copy(Rest, P + 4, Length(Rest));  { hex digits + ')' }
    if (Length(AddrStr) > 0) and (AddrStr[Length(AddrStr)] = ')') then
      Delete(AddrStr, Length(AddrStr), 1);
    Result.Address := HexToQWord(AddrStr);
    Body := Copy(Rest, 1, P - 1);
  end
  else
    Body := Rest;

  { Parse body: 'FuncName at file.pas:line'  or just  'FuncName' }
  AtPos := Pos(' at ', Body);
  if AtPos > 0 then
  begin
    Result.FuncName := Copy(Body, 1, AtPos - 1);
    Rest := Copy(Body, AtPos + 4, MaxInt);   { 'file.pas:line' }
    ColonPos := LastPosOf(':', Rest);
    if ColonPos > 0 then
    begin
      Result.FileName   := Copy(Rest, 1, ColonPos - 1);
      Result.LineNumber := StrToIntDef(Copy(Rest, ColonPos + 1, MaxInt), 0);
      Result.HasSource  := (Result.FileName <> '') and (Result.LineNumber > 0);
    end
    else
      Result.FileName := Rest;
  end
  else
    Result.FuncName := Body;
end;


function ParseCallStack(const ALines: array of string): TCallStackFrameArray;
var
  i: Integer;
begin
  SetLength(Result, Length(ALines));
  for i := 0 to High(ALines) do
    Result[i] := ParseCallStackFrame(ALines[i]);
end;


function CallStackFrameDisplay(const AFrame: TCallStackFrame): string;
var
  Location: string;
begin
  if AFrame.HasSource then
    Location := '(' + AFrame.FileName + ':' + IntToStr(AFrame.LineNumber) + ')'
  else if AFrame.Address <> 0 then
    Location := '(0x' + IntToHex(AFrame.Address, 16) + ')'
  else
    Location := '';

  Result := '#' + IntToStr(AFrame.Index) + '  ' + AFrame.FuncName;
  if Location <> '' then
    Result := Result + '  ' + Location;
end;


end.
