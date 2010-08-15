{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Some handly UTF8 functions copied from the Lazarus LCL. Comes from the
      LCLProc unit. Surely we can move this into FPC?
}

unit fpg_stringutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base;


function  UTF8CharacterLength(p: PChar): integer;
function  UTF8CharStart(UTF8Str: PChar; Len, Index: integer): PChar;
function  UTF8Copy(const s: string; StartCharIndex, CharCount: integer): string;
function  UTF8CStringToUTF8String(SourceStart: PChar; SourceLen: SizeInt): string;
function  UTF8Length(const s: string): integer;
function  UTF8Length(p: PChar; ByteCount: integer): integer;
function  UTF8Pos(const SearchForText, SearchInText: string): integer;
procedure UTF8Delete(var S: string; Index, Size: integer);
procedure UTF8Insert(const Source: string; var S: string; Index: integer);
function  UTF8CharAtByte(const s: string; const BytePos: integer; out aChar: string): integer;


// short form (alias or convenience) functions for the UTF8 ones above
function  Copy8(const s: string; StartCharIndex, CharCount: integer): string;
function  Length8(const s: string): integer;
function  Pos8(const SearchForText, SearchInText: string): integer;
procedure Delete8(var S: string; Index, Size: integer);
procedure Insert8(const Source: string; var S: string; Index: integer);

function  fpgCharAt(const s: TfpgString; Index: integer): TfpgChar;
function  fpgAppendPathDelim(const Path: TfpgString): TfpgString;
function  fpgRemovePathDelim(const Path: TfpgString): TfpgString;


implementation


{ If it's a multibyte character, the first byte specifies the amount of bytes
  used. All subsequent bytes will start with 10xxxxxx. It is assumed that p
  points to the beginning of a single or multibyte character. }
function UTF8CharacterLength(p: PChar): integer;
begin
  if p <> nil then
  begin
    if ord(p^) < %11000000 then  //  00000000-01111111
    begin
      // regular single byte character (#0 is a character, this is pascal ;)
      Result := 1;
    end
    else if ((ord(p^) and %11100000) = %11000000) then  //  11000010-11011111
    begin
      // could be 2 byte character
      if (ord(p[1]) and %11000000) = %10000000 then
        Result := 2
      else
        Result := 1;
    end
    else if ((ord(p^) and %11110000) = %11100000) then  //  11100000-11101111
    begin
      // could be 3 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
          and ((ord(p[2]) and %11000000) = %10000000) then
        Result := 3
      else
        Result := 1;
    end
    else if ((ord(p^) and %11111000) = %11110000) then  //  11110000-11110100
    begin
      // could be 4 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
          and ((ord(p[2]) and %11000000) = %10000000)
          and ((ord(p[3]) and %11000000) = %10000000) then
        Result := 4
      else
        Result := 1;
    end
    else
      Result := 1;
  end
  else
    Result := 0;
end;

{ Returns the character starting position as PChar in the UTF8Str string. }
function UTF8CharStart(UTF8Str: PChar; Len, Index: integer): PChar;
var
  CharLen: LongInt;
begin
  Result := UTF8Str;
  if Result <> nil then
  begin
    while (Index > 0) and (Len > 0) do
    begin
      CharLen := UTF8CharacterLength(Result);
      dec(Len, CharLen);
      dec(Index);
      inc(Result, CharLen);
    end;
    if (Index > 0) or (Len < 0) then
      Result := nil;
  end;
end;

// returns substring
function UTF8Copy(const s: string; StartCharIndex, CharCount: integer): string;
var
  StartBytePos: PChar;
  EndBytePos: PChar;
  MaxBytes: PtrInt;
begin
  Result := '';
  // Some sanity checks
  if (Length(s) = 0) then
    Exit; //==>
  if CharCount = 0 then
    Exit; //==>
    
  StartBytePos := UTF8CharStart(PChar(s),length(s),StartCharIndex-1);
  if StartBytePos = nil then
    Result := ''
  else
  begin
    MaxBytes := PtrInt(PChar(s)+length(s)-StartBytePos);
    EndBytePos := UTF8CharStart(StartBytePos,MaxBytes,CharCount);
    if EndBytePos = nil then
      Result := copy(s,StartBytePos-PChar(s)+1,MaxBytes)
    else
      Result := copy(s,StartBytePos-PChar(s)+1,EndBytePos-StartBytePos);
  end;
end;

function UTF8CStringToUTF8String(SourceStart: PChar; SourceLen: SizeInt): string;
var
  Source: PChar;
  Dest: PChar;
  SourceEnd: PChar;
  CharLen: integer;
  SourceCopied: PChar;

  // Copies from SourceStart till Source to Dest and updates Dest
  procedure CopyPart; inline;
  var
    CopyLength: SizeInt;
  begin
    CopyLength := Source - SourceCopied;
    if CopyLength=0 then exit;
    move(SourceCopied^ , Dest^, CopyLength);
    SourceCopied:=Source;
    inc(Dest, CopyLength);
  end;

begin
  SetLength(Result, SourceLen);
  if SourceLen=0 then
    Exit; //==>
  SourceCopied:=SourceStart;
  Source:=SourceStart;
  Dest:=PChar(Result);
  SourceEnd := Source + SourceLen;
  while Source<SourceEnd do
  begin
    CharLen := UTF8CharacterLength(Source);
    if (CharLen=1) and (Source^='\') then
    begin
      CopyPart;
      inc(Source);
      if Source^ in ['t', 'n', '"', '\'] then
      begin
        case Source^ of
         't' : Dest^ := #9;
         '"' : Dest^ := '"';
         '\' : Dest^ := '\';
         'n' :
         // fpc 2.1.1 stores string constants as array of char so maybe this
         // will work for without ifdef (once available in 2.0.x too):
         // move(lineending, dest^, sizeof(LineEnding));
{$IFDEF WINDOWS}
               begin
                 move(lineending[1], dest^, length(LineEnding));
                 inc(dest^, length(LineEnding)-1);
               end;
{$ELSE}
               Dest^ := LineEnding;
{$ENDIF}
        end;
        inc(Source);
        inc(Dest);
      end;
      SourceCopied := Source;
    end
    else
      Inc(Source, CharLen);
  end;
  CopyPart;
  SetLength(Result, Dest - PChar(Result));
end;

function UTF8Length(const s: string): integer;
begin
  if s = '' then
    Result := 0
  else
    Result := UTF8Length(PChar(s),length(s));
end;

function UTF8Length(p: PChar; ByteCount: integer): integer;
var
  CharLen: LongInt;
begin
  Result := 0;
  while (ByteCount > 0) do
  begin
    inc(Result);
    CharLen := UTF8CharacterLength(p);
    inc(p, CharLen);
    dec(ByteCount, CharLen);
  end;
end;

// returns the character index, where the SearchForText starts in SearchInText
function UTF8Pos(const SearchForText, SearchInText: string): integer;
var
 p: LongInt;
begin
  p := System.Pos(SearchForText, SearchInText);
  if p > 0 then
    Result := UTF8Length(PChar(SearchInText), p-1) + 1
  else
    Result := 0;
end;

procedure UTF8Delete(var S: string; Index, Size: integer);
var
  ls: integer;
  b: string;
  e: string;
begin
  ls := UTF8Length(S);
  if (Index > ls) or (Index <= 0) or (Size <= 0) then
    Exit; //==>
  b := UTF8Copy(S, 1, Index-1); // beginning string
  e := UTF8Copy(S, Index+Size, UTF8Length(S)-(Index+Size-1)); // ending string
  S := b + e;
end;

procedure UTF8Insert(const Source: string; var S: string; Index: integer);
var
  b: string;
  e: string;
begin
  if UTF8Length(Source) = 0 then
    Exit; //==>
  b := UTF8Copy(S, 1, Index-1); // beginning string
  e := UTF8Copy(S, Index, UTF8Length(S)-Index+1); // ending string
  S := b + Source + e;
end;

function UTF8CharAtByte(const s: string; const BytePos: integer; out aChar: string): integer;
var
  CharLen: Integer;
begin
  if BytePos > 0 then
  begin
    CharLen := UTF8CharacterLength(@s[BytePos]);
    aChar   := Copy(s, BytePos, CharLen);
    Result  := BytePos + CharLen;
  end else
  begin
    aChar   := '';
    Result  := 1;
  end;
end;

function Copy8(const s: string; StartCharIndex, CharCount: integer): string;
begin
  Result := UTF8Copy(s, StartCharIndex, CharCount);
end;

function Length8(const s: string): integer;
begin
  Result := UTF8Length(s);
end;

function Pos8(const SearchForText, SearchInText: string): integer;
begin
  Result := UTF8Pos(SearchForText, SearchInText);
end;

procedure Delete8(var S: string; Index, Size: integer);
begin
  UTF8Delete(S, Index, Size);
end;

procedure Insert8(const Source: string; var S: string; Index: integer);
begin
  UTF8Insert(Source, S, Index);
end;

function fpgCharAt(const s: TfpgString; Index: integer): TfpgChar;
begin
  Result := UTF8Copy(s, Index, 1);
end;

function fpgAppendPathDelim(const Path: TfpgString): TfpgString;
begin
  if (Path <> '') and (Path[Length(Path)] <> PathDelim) then
    Result := Path + PathDelim
  else
    Result := Path;
end;

function fpgRemovePathDelim(const Path: TfpgString): TfpgString;
begin
  if (Path <> '') and (Path[Length(Path)] = PathDelim) then
    Result := LeftStr(Path, Length(Path)-1)
  else
    Result := Path;
end;

end.

