{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2013 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      ---
}

unit idemacros;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,SysUtils
  ,fpg_base
  ,fpg_main
  ;

type
  TIDEMacro = class(TObject)
  public
    Name: string;
    Value: string;
    Description: string;
    constructor Create(AName, AValue, ADescription: TfpgString);
  end;


  TIDEMacroList = class(TObject)
  private
    FItems: TList;
    function    GetItems(AIndex: integer): TIDEMacro;
    procedure   SetItems(AIndex: integer; const AValue: TIDEMacro);
    procedure   AddDefaults;
    procedure   LoadSavedValues;
  public
    constructor Create;
    destructor  Destroy; override;
    function    Count: integer;
    function    FindByName(const MacroName: TfpgString): TIDEMacro;
    function    StrHasMacros(const s: TfpgString): boolean;
    function    ExpandMacro(const s: TfpgString): TfpgString;
    procedure   Add(NewMacro: TIDEMacro);
    procedure   Clear;
    procedure   Delete(AIndex: integer);
    procedure   SetValue(const MacroName, NewValue: TfpgString);
    procedure   ResetToDefaults;
    property    Items[AIndex: integer]: TIDEMacro read GetItems write SetItems; default;
  end;


// lazy-man singleton of IDE Macros
function GMacroList: TIDEMacroList;


implementation

uses
  ideconst
  ,fpg_iniutils;

var
  uIDEMacroList: TIDEMacroList;


function GMacroList: TIDEMacroList;
begin
  if not Assigned(uIDEMacroList) then
    uIDEMacroList := TIDEMacroList.Create;
  Result := uIDEMacroList;
end;


{ TIDEMacro }

constructor TIDEMacro.Create(AName, AValue, ADescription: TfpgString);
begin
  Name := AName;
  Value := AValue;
  Description := ADescription;
end;


{ TIDEMacroList }

function TIDEMacroList.GetItems(AIndex: integer): TIDEMacro;
begin
  Result := TIDEMacro(FItems[AIndex]);
end;

procedure TIDEMacroList.SetItems(AIndex: integer; const AValue: TIDEMacro);
begin
  FItems[AIndex] := AValue;
end;

procedure TIDEMacroList.AddDefaults;
var
  o: TIDEMacro;
begin
  o := TIDEMacro.Create(cMacro_FPCSrcDir, '', 'FPC source directory');
  Add(o);
  o := TIDEMacro.Create(cMacro_FPGuiDir, '', 'fpGUI root directory');
  Add(o);
  o := TIDEMacro.Create(cMacro_FPGuiLibDir, cMacro_FPGuiDir+'lib/'+cMacro_Target+'/', 'fpGUI compiled library directory');
  Add(o);
  o := TIDEMacro.Create(cMacro_SyntaxDefDir, cMacro_FPGuiDir+'examples/apps/fpgide/syntaxdefs/', 'Editor syntax highlighter definitions');
  Add(o);
  o := TIDEMacro.Create(cMacro_TemplateDir, cMacro_FPGuiDir+'examples/apps/fpgide/templates/', 'Project template directory');
  Add(o);
  o := TIDEMacro.Create(cMacro_Compiler, '', 'FPC Compiler to use');
  Add(o);
  o := TIDEMacro.Create(cMacro_Debugger, 'gdb', 'Location of GDB debugger');
  Add(o);
  o := TIDEMacro.Create(cMacro_ExeExt, {$IFDEF MSWINDOWS} '.exe' {$ENDIF} {$IFDEF UNIX} '' {$ENDIF}, 'Default executable extension');
  Add(o);
  o := TIDEMacro.Create(cMacro_Target, CPUTarget+'-'+OSTarget, 'Default target');
  Add(o);
end;

procedure TIDEMacroList.LoadSavedValues;
var
  s: TfpgString;
begin
  // we don't unnecessarily override the defaults setup in AddDefaults()
  SetValue(cMacro_FPCSrcDir, gINI.ReadString(cEnvironment, 'FPCSrcDir', ''));
  SetValue(cMacro_FPGuiDir, gINI.ReadString(cEnvironment, 'FPGuiDir', ''));
  s := gINI.ReadString(cEnvironment, 'FPGuiLibDir', '');
  if s <> '' then
    SetValue(cMacro_FPGuiLibDir, s);
  s := gINI.ReadString(cEnvironment, 'SyntaxDefDir', '');
  if s <> '' then
    SetValue(cMacro_SyntaxDefDir, s);
  s := gINI.ReadString(cEnvironment, 'TemplateDir', '');
  if s <> '' then
    SetValue(cMacro_TemplateDir, s);
  SetValue(cMacro_Compiler, gINI.ReadString(cEnvironment, 'Compiler', ''));
  s := gINI.ReadString(cEnvironment, 'Debugger', '');
  if s <> '' then
    SetValue(cMacro_Debugger, s);
  s := gINI.ReadString(cEnvironment, 'ExeExt', '');
  if s <> '' then
    SetValue(cMacro_ExeExt, s);
  s := gINI.ReadString(cEnvironment, 'Target', '');
  if s <> '' then
    SetValue(cMacro_Target, s);
end;

constructor TIDEMacroList.Create;
begin
  inherited Create;
  FItems := TList.Create;
  AddDefaults;
  LoadSavedValues;
end;

destructor TIDEMacroList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

function TIDEMacroList.Count: integer;
begin
  Result := FItems.Count;
end;

function TIDEMacroList.FindByName(const MacroName: TfpgString): TIDEMacro;
var
  l: Integer;
  r: Integer;
  m: Integer;
  cmp: Integer;
begin
  l := 0;
  r := FItems.Count-1;
  m := 0;
  while l <= r do
  begin
    m := (l+r) shr 1;
    Result := Items[m];
    cmp := AnsiCompareText(MacroName,Result.Name);
    if cmp < 0 then
      r := m-1
    else if cmp > 0 then
      l := m+1
    else
      exit;
  end;
  Result := nil;
end;

function TIDEMacroList.StrHasMacros(const s: TfpgString): boolean;
// search for ${
var
  p: Integer;
  Len: Integer;
begin
  Result := false;
  p := 1;
  Len := length(s);
  while (p < Len) do
  begin
    if s[p] = '$' then
    begin
      inc(p);
      if (p<Len) and (s[p]<>'$') then
      begin
        // skip macro function name
        while (p<Len) and (s[p]<>'{') do inc(p);
        if (p<Len) then
        begin
          Result:=true;
          exit;
        end;
      end
      else
      begin
        // $$ is not a macro
        inc(p);
      end;
    end else
      inc(p);
  end;
end;

function TIDEMacroList.ExpandMacro(const s: TfpgString): TfpgString;
var
  sub: TfpgString;
  pstart: integer;
  pend: integer;
  len: integer;
  m: TIDEMacro;
  r: TfpgString;
begin
  r := s;
  pstart := Pos('${', r);
  while (pstart > 0) do
  begin
    len := Length(r);
    pend := pstart + 2;
    while pend < len do
    begin
      if r[pend] = '}' then
        break
      else
        inc(pend);
    end;
    sub := Copy(r, pstart, (pend-pstart)+1);
    m := FindByName(sub);
    if not Assigned(m) then
      raise Exception.CreateFmt('The macro <%s> is not defined.', [sub]);
    r := StringReplace(r, sub, m.Value, [rfReplaceAll, rfIgnoreCase]);
    pstart := Pos('${', r);
  end;
  Result := r;
end;

procedure TIDEMacroList.Add(NewMacro: TIDEMacro);
var
  l: Integer;
  r: Integer;
  m: Integer;
  cmp: Integer;
begin
  l := 0;
  r := FItems.Count-1;
  m := 0;
  while l <= r do
  begin
    m := (l+r) shr 1;
    cmp := AnsiCompareText(NewMacro.Name, Items[m].Name);
    if cmp < 0 then
      r := m-1
    else if cmp > 0 then
      l := m + 1
    else
      break;
  end;
  if (m < FItems.Count) and (AnsiCompareText(NewMacro.Name, Items[m].Name) > 0) then
    inc(m);
  FItems.Insert(m, NewMacro);
end;

procedure TIDEMacroList.Clear;
var
  i: integer;
begin
  for i := 0 to FItems.Count-1 do Items[i].Free;
  FItems.Clear;
end;

procedure TIDEMacroList.Delete(AIndex: integer);
begin
  Items[AIndex].Free;
  FItems.Delete(AIndex);
end;

procedure TIDEMacroList.SetValue(const MacroName, NewValue: TfpgString);
var
  lMacro: TIDEMacro;
begin
  lMacro := FindByName(MacroName);
  if lMacro <> nil then
    lMacro.Value := NewValue;
end;

procedure TIDEMacroList.ResetToDefaults;
begin
  Clear;
  AddDefaults;
  LoadSavedValues;
end;


initialization
  uIDEMacroList := nil;

finalization
  uIDEMacroList.Free;

end.

