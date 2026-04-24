(*
    fpGUI IDE - Variables Panel Logic

    Copyright (C) 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Pure logic for the Variables debugger panel: building node display
      text, detecting expandable composite values, and parsing child
      entries from PDR-formatted value strings.

      PDR formats composite values as:
        Record : TypeName { Field1: val1, Field2: val2 }
        Class  : TypeName(@$ADDR) { Field1: val1, Field2: val2 }
        Nil    : nil
        Simple : the value directly (integer, string, boolean, ...)
*)
unit ide.variables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TVarNodeData - data attached to each node in the Variables tree view }

  TVarNodeData = class
    Name:         string;
    Value:        string;
    TypeName:     string;
    FullPath:     string;   { dot-notation path for EvaluateExpression, e.g. 'myObj.FName' }
    IsExpandable: Boolean;
    constructor Create(const AName, AValue, ATypeName, AFullPath: string);
  end;

  TVarNodeDataArray = array of TVarNodeData;


{ Build the display text for a single tree node.
  When AShowType is True the result is "name = value : TypeName".
  When AShowType is False or ATypeName is empty the result is "name = value". }
function BuildVarNodeText(const AName, AValue, ATypeName: string;
    AShowType: Boolean): string;

(* Returns True when AValue represents an expandable composite (record or
  class) — i.e. it contains the substring ' { '. *)
function VarValueIsExpandable(const AValue: string): Boolean;

(* Extracts the Pascal type name embedded at the start of a composite value
  string.  Returns an empty string for simple (non-composite) values.
    'TMyRec { X: 1, Y: 2 }'           -> 'TMyRec'
    'TMyClass(@$1234ABCD) { F: 1 }'   -> 'TMyClass'
    '42'                               -> ''              *)
function ExtractVarTypeName(const AValue: string): string;

{ Parse the direct child entries from a composite value string.
  AParentPath is the dot-notation path of the parent node and is used to
  build the FullPath of each child (e.g. 'myVar' -> child FullPath 'myVar.X').
  Returns an empty array for simple or nil values. }
function ParseVarChildren(const AParentPath, AValue: string): TVarNodeDataArray;


implementation


{ TVarNodeData }

constructor TVarNodeData.Create(const AName, AValue, ATypeName,
    AFullPath: string);
begin
  inherited Create;
  Name         := AName;
  Value        := AValue;
  TypeName     := ATypeName;
  FullPath     := AFullPath;
  IsExpandable := VarValueIsExpandable(AValue);
end;


{ ---------------------------------------------------------------------------
  Public functions
  --------------------------------------------------------------------------- }

function BuildVarNodeText(const AName, AValue, ATypeName: string;
    AShowType: Boolean): string;
begin
  Result := AName + ' = ' + AValue;
  if AShowType and (ATypeName <> '') then
    Result := Result + ' : ' + ATypeName;
end;

function VarValueIsExpandable(const AValue: string): Boolean;
begin
  Result := Pos(' { ', AValue) > 0;
end;

function ExtractVarTypeName(const AValue: string): string;
var
  BracePos, ParenPos: Integer;
begin
  Result := '';
  BracePos := Pos(' { ', AValue);
  if BracePos <= 0 then
    Exit;
  Result := Copy(AValue, 1, BracePos - 1);
  { Strip pointer annotation like (@$1234ABCD) from class values }
  ParenPos := Pos('(', Result);
  if ParenPos > 0 then
    Result := TrimRight(Copy(Result, 1, ParenPos - 1));
end;


{ ---------------------------------------------------------------------------
  ParseVarChildren helpers
  --------------------------------------------------------------------------- }

(* Extract the text between the outermost { } of a composite value string.
  Returns True and sets AContent when braces are found and balanced.
  Returns False for simple values. *)
function ExtractBraceContent(const AValue: string; out AContent: string): Boolean;
var
  StartPos, EndPos, Depth, i: Integer;
begin
  Result   := False;
  AContent := '';
  StartPos := Pos('{', AValue);
  if StartPos = 0 then
    Exit;
  Depth    := 0;
  EndPos   := 0;
  for i := StartPos to Length(AValue) do
  begin
    if AValue[i] = '{' then
      Inc(Depth)
    else if AValue[i] = '}' then
    begin
      Dec(Depth);
      if Depth = 0 then
      begin
        EndPos := i;
        Break;
      end;
    end;
  end;
  if EndPos = 0 then
    Exit;
  AContent := Trim(Copy(AValue, StartPos + 1, EndPos - StartPos - 1));
  Result   := True;
end;

(* Split AContent into top-level field entries, respecting nested { } and
  Pascal string literals delimited by single quotes.
  Entries are separated by ', ' at brace/string depth zero. *)
procedure SplitFieldEntries(const AContent: string; AResult: TStringList);
var
  i, Len, BraceDepth: Integer;
  InString: Boolean;
  Ch: Char;
  Current: string;
begin
  AResult.Clear;
  if AContent = '' then
    Exit;
  BraceDepth := 0;
  InString   := False;
  Current    := '';
  Len        := Length(AContent);
  i          := 1;
  while i <= Len do
  begin
    Ch := AContent[i];
    if InString then
    begin
      Current := Current + Ch;
      if Ch = '''' then
      begin
        { '' inside a string is an escaped quote — stay in string mode }
        if (i < Len) and (AContent[i + 1] = '''') then
        begin
          Inc(i);
          Current := Current + '''';
        end
        else
          InString := False;
      end;
    end
    else
    begin
      case Ch of
        '''':
          begin
            InString := True;
            Current  := Current + Ch;
          end;
        '{':
          begin
            Inc(BraceDepth);
            Current := Current + Ch;
          end;
        '}':
          begin
            Dec(BraceDepth);
            Current := Current + Ch;
          end;
        ',':
          begin
            if (BraceDepth = 0) and (i < Len) and (AContent[i + 1] = ' ') then
            begin
              { Top-level separator ', ' found — emit current entry }
              AResult.Add(Trim(Current));
              Current := '';
              Inc(i);  { skip the space after the comma }
            end
            else
              Current := Current + Ch;
          end;
        else
          Current := Current + Ch;
      end;
    end;
    Inc(i);
  end;
  if Trim(Current) <> '' then
    AResult.Add(Trim(Current));
end;

{ Split a single 'fieldname: value' entry on the first ': ' occurrence. }
procedure SplitFieldEntry(const AEntry: string; out AName, AValue: string);
var
  ColonPos: Integer;
begin
  ColonPos := Pos(': ', AEntry);
  if ColonPos > 0 then
  begin
    AName  := Trim(Copy(AEntry, 1, ColonPos - 1));
    AValue := Trim(Copy(AEntry, ColonPos + 2, MaxInt));
  end
  else
  begin
    AName  := Trim(AEntry);
    AValue := '';
  end;
end;


{ ---------------------------------------------------------------------------
  ParseVarChildren
  --------------------------------------------------------------------------- }

function ParseVarChildren(const AParentPath, AValue: string): TVarNodeDataArray;
var
  Content:  string;
  Entries:  TStringList;
  i:        Integer;
  FldName,
  FldValue,
  FldType:  string;
  ChildPath: string;
begin
  SetLength(Result, 0);
  if not ExtractBraceContent(AValue, Content) then
    Exit;
  if Content = '' then
    Exit;
  Entries := TStringList.Create;
  try
    SplitFieldEntries(Content, Entries);
    SetLength(Result, Entries.Count);
    for i := 0 to Entries.Count - 1 do
    begin
      SplitFieldEntry(Entries[i], FldName, FldValue);
      FldType   := ExtractVarTypeName(FldValue);
      if AParentPath <> '' then
        ChildPath := AParentPath + '.' + FldName
      else
        ChildPath := FldName;
      Result[i] := TVarNodeData.Create(FldName, FldValue, FldType, ChildPath);
    end;
  finally
    Entries.Free;
  end;
end;


end.
