{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2026 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Breakpoint data model and persistence.

      TBreakpointList is a standalone, UI-independent list of breakpoints.
      It supports toggle (add/remove), querying, JSON persistence, and
      handle management for the active PDR debug session.

      Filenames are stored as absolute paths. Matching uses the basename
      only so that the comparison remains consistent with OPDF debug info,
      which stores only the source filename without directory.
}
unit ide.breakpoint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TBreakpoint = record
    FileName: String;   { absolute path to source file }
    Line: Integer;      { 1-based line number }
    Enabled: Boolean;
    Handle: Integer;    { PDR TBreakpointHandle when session active; -1 = none }
  end;

  { TBreakpointList }

  TBreakpointList = class(TObject)
  private
    FItems: array of TBreakpoint;
    FCount: Integer;
    function  SameFile(const A, B: String): Boolean;
  public
    function  FindIndex(const AFileName: String; ALine: Integer): Integer;
    procedure Toggle(const AFileName: String; ALine: Integer);
    function  HasBreakpoint(const AFileName: String; ALine: Integer): Boolean;
    function  Count: Integer;
    function  GetItem(AIndex: Integer): TBreakpoint;
    procedure SetHandle(AIndex: Integer; AHandle: Integer);
    procedure SetEnabled(AIndex: Integer; AEnabled: Boolean);
    procedure ClearHandles;
    procedure Clear;
    procedure SaveToFile(const AFileName: String);
    procedure LoadFromFile(const AFileName: String);
  end;

implementation

uses
  fpjson, jsonparser;

{ TBreakpointList }

function TBreakpointList.SameFile(const A, B: String): Boolean;
begin
  { Compare by lowercase basename to match OPDF debug info storage }
  Result := LowerCase(ExtractFileName(A)) = LowerCase(ExtractFileName(B));
end;

function TBreakpointList.FindIndex(const AFileName: String; ALine: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FCount - 1 do
    if (FItems[i].Line = ALine) and SameFile(FItems[i].FileName, AFileName) then
    begin
      Result := i;
      Exit;
    end;
end;

procedure TBreakpointList.Toggle(const AFileName: String; ALine: Integer);
var
  Idx, i: Integer;
begin
  Idx := FindIndex(AFileName, ALine);
  if Idx >= 0 then
  begin
    { Remove — shift remaining items left }
    for i := Idx to FCount - 2 do
      FItems[i] := FItems[i + 1];
    Dec(FCount);
    SetLength(FItems, FCount);
  end
  else
  begin
    { Add new breakpoint }
    SetLength(FItems, FCount + 1);
    FItems[FCount].FileName := AFileName;
    FItems[FCount].Line := ALine;
    FItems[FCount].Enabled := True;
    FItems[FCount].Handle := -1;
    Inc(FCount);
  end;
end;

function TBreakpointList.HasBreakpoint(const AFileName: String; ALine: Integer): Boolean;
begin
  Result := FindIndex(AFileName, ALine) >= 0;
end;

function TBreakpointList.Count: Integer;
begin
  Result := FCount;
end;

function TBreakpointList.GetItem(AIndex: Integer): TBreakpoint;
begin
  Result := FItems[AIndex];
end;

procedure TBreakpointList.SetHandle(AIndex: Integer; AHandle: Integer);
begin
  if (AIndex >= 0) and (AIndex < FCount) then
    FItems[AIndex].Handle := AHandle;
end;

procedure TBreakpointList.SetEnabled(AIndex: Integer; AEnabled: Boolean);
begin
  if (AIndex >= 0) and (AIndex < FCount) then
    FItems[AIndex].Enabled := AEnabled;
end;

procedure TBreakpointList.ClearHandles;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    FItems[i].Handle := -1;
end;

procedure TBreakpointList.Clear;
begin
  SetLength(FItems, 0);
  FCount := 0;
end;

procedure TBreakpointList.SaveToFile(const AFileName: String);
var
  Arr: TJSONArray;
  Obj: TJSONObject;
  i: Integer;
  FS: TFileStream;
  S: String;
  Dir: String;
begin
  Dir := ExtractFileDir(AFileName);
  if (Dir <> '') and not DirectoryExists(Dir) then
    ForceDirectories(Dir);

  Arr := TJSONArray.Create;
  try
    for i := 0 to FCount - 1 do
    begin
      Obj := TJSONObject.Create;
      Obj.Add('file', FItems[i].FileName);
      Obj.Add('line', FItems[i].Line);
      Obj.Add('enabled', FItems[i].Enabled);
      Arr.Add(Obj);
    end;
    S := Arr.FormatJSON([foUseTabchar], 1);
    FS := TFileStream.Create(AFileName, fmCreate);
    try
      if Length(S) > 0 then
        FS.Write(S[1], Length(S));
    finally
      FS.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TBreakpointList.LoadFromFile(const AFileName: String);
var
  JSONData: TJSONData;
  Arr: TJSONArray;
  Obj: TJSONObject;
  i: Integer;
  FS: TFileStream;
  BP: TBreakpoint;
begin
  Clear;
  if not FileExists(AFileName) then
    Exit;

  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    JSONData := GetJSON(FS);
  finally
    FS.Free;
  end;
  if JSONData = nil then
    Exit;

  try
    if JSONData.JSONType <> jtArray then
      Exit;
    Arr := TJSONArray(JSONData);
    for i := 0 to Arr.Count - 1 do
    begin
      if Arr.Types[i] <> jtObject then
        Continue;
      Obj := Arr.Objects[i];
      BP.FileName := '';
      BP.Line := 0;
      BP.Enabled := True;
      BP.Handle := -1;
      if Obj.IndexOfName('file') >= 0 then
        BP.FileName := Obj.Strings['file'];
      if Obj.IndexOfName('line') >= 0 then
        BP.Line := Obj.Integers['line'];
      if Obj.IndexOfName('enabled') >= 0 then
        BP.Enabled := Obj.Booleans['enabled'];
      if (BP.FileName <> '') and (BP.Line > 0) then
      begin
        SetLength(FItems, FCount + 1);
        FItems[FCount] := BP;
        Inc(FCount);
      end;
    end;
  finally
    JSONData.Free;
  end;
end;


end.
