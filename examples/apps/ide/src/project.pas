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

unit Project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UnitList, fpg_base, fpg_iniutils;

type
  TBooleanGrid = array of array of Boolean;

  TProject = class(TObject)
  private
    FMakeOptionsGrid: TBooleanGrid;
    FProjectName: TfpgString;
    FMainUnit: TfpgString;
    FUnitDirs: TStringList;
    FUnitDirsGrid: TBooleanGrid;
    FUnitList: TUnitList;
    FIniFile: TfpgINIFile;
    FProjectDir: TfpgString;
    FTargetFile: TfpgString;
    FDefaultMake: integer;
    FMakeOptions: TStringList;
    FMacroNames: TStringList;
    FUnitOutputDir: TfpgString;
    procedure   MergeWithGlobalMacros;
  public
    constructor Create;
    destructor  Destroy; override;
    function    Save(const AFile: TfpgString = ''): Boolean;
    function    Load(AProjectFile: TfpgString): Boolean;
    function    GenerateCmdLine(const AShowOnly: Boolean = False; const ABuildMode: integer = -1): TfpgString;
    procedure   ClearAndInitMakeOptions(const ASize: integer);
    procedure   ClearAndInitUnitDirsGrid(const ASize: integer);
    procedure   ClearAndInitMacrosGrid(const ASize: integer);
    property    ProjectDir: TfpgString read FProjectDir write FProjectDir;
    property    ProjectName: TfpgString read FProjectName write FProjectName;
    property    MainUnit: TfpgString read FMainUnit write FMainUnit;
    property    TargetFile: TfpgString read FTargetFile write FTargetFile;
    property    UnitList: TUnitList read FUnitList;
    property    DefaultMake: integer read FDefaultMake write FDefaultMake;
    property    MakeOptions: TStringList read FMakeOptions;
    property    MakeOptionsGrid: TBooleanGrid read FMakeOptionsGrid write FMakeOptionsGrid;
    property    MacroNames: TStringList read FMacroNames;
    property    UnitDirs: TStringList read FUnitDirs;
    property    UnitOutputDir: TfpgString read FUnitOutputDir write FUnitOutputDir;
    property    UnitDirsGrid: TBooleanGrid read FUnitDirsGrid write FUnitDirsGrid;
  end;


// lazy-mans singleton
function GProject: TProject;

procedure FreeProject;


implementation

uses
  ideconst
  ,ideutils
  ,fpg_utils
  ,idemacros
  ;


var
  uProject: TProject;

function GProject: TProject;
begin
  if not Assigned(uProject) then
    uProject := TProject.Create;
  Result := uProject;
end;

procedure FreeProject;
begin
  uProject.Free;
  uProject := nil;
end;


{ TProject }

procedure TProject.MergeWithGlobalMacros;
var
  o: TIDEMacro;
  i: integer;
  n,v: TfpgString;
begin
  for i := 0 to MacroNames.Count-1 do
  begin
    MacroNames.GetNameValue(i, n, v);
    o := TIDEMacro.Create(cMacroPrefix + n + cMacroSuffix, v, '');
    GMacroList.Add(o);
  end;
end;

constructor TProject.Create;
begin
  inherited Create;
  FUnitList := TUnitList.Create;
  FMakeOptions := TStringList.Create;
  FMacroNames := TStringList.Create;
  FUnitDirs := TStringList.Create;
end;

destructor TProject.Destroy;
begin
  FUnitDirs.Free;
  FMacroNames.Free;
  FMakeOptions.Free;
  FUnitList.Free;
  FIniFile.Free;
  inherited Destroy;
end;

function TProject.Save(const AFile: TfpgString = ''): Boolean;
var
  c, j: integer;
  s: TfpgString;
  lDelim: TfpgString;

  procedure SaveList(AList: TStringList; const CName, IName: TfpgString);
  var
    i: integer;
  begin
    FIniFile.WriteInteger(cProjectOptions, CName, AList.Count);
    for i := 0 to AList.Count-1 do
      FIniFile.WriteString(cProjectOptions, IName + IntToStr(i+1), AList[i]);
  end;

begin
  Result := False;
  if (AFile = '') and (ProjectName = '') then
    raise Exception.Create('Project name has not been specified yet');

  if not Assigned(FIniFile) then
  begin
    if AFile = '' then
      FIniFile := TfpgINIFile.CreateExt(ProjectDir + ProjectName + cProjectExt)
    else
      FIniFile := TfpgINIFile.CreateExt(AFile);
  end
  else
  begin
    if AFile <> '' then
    begin
      FIniFile.Free;
      FIniFile := TfpgINIFile.CreateExt(AFile);
    end;
  end;

  if AFile <> '' then
    ProjectName := fpgExtractFileName(AFile);

  FIniFile.WriteString(cProjectOptions, 'ProjectName', ProjectName);
  FIniFile.WriteString(cProjectOptions, 'MainUnit', MainUnit);
  FIniFile.WriteString(cProjectOptions, 'TargetFile', TargetFile);
  FIniFile.WriteInteger(cProjectOptions, 'DefaultMake', DefaultMake);
  FIniFile.WriteString(cProjectOptions, 'UnitOutputDir', UnitOutputDir);

  // Process the Make (compiler param) options
  { first delete old items in ini file }
  c := FIniFile.ReadInteger(cProjectOptions, 'MakeOptionsCount', 0);
  for j := 1 to c do
    FIniFile.DeleteKey(cProjectOptions, cINIMakeOption + IntToStr(j));
  { no lets save new info }
  SaveList(MakeOptions, 'MakeOptionsCount', cINIMakeOption);
  for j := 0 to MakeOptions.Count-1 do
  begin
    s := '';
    lDelim := '';
    for c := 0 to 5 do
    begin
      if MakeOptionsGrid[c, j] then  // True = 1, False = 0
        s := s + lDelim + '1'
      else
        s := s + lDelim + '0';
      lDelim := ',';
    end;
    FIniFile.WriteString(cProjectOptions, cINIMakeOptionGrid + IntToStr(j+1), s);
  end;

  // macros definitions
  SaveList(MacroNames, 'MacroCount', 'Macro');

  // unit search directories
  { first delete old items in ini file }
  c := FIniFile.ReadInteger(cProjectOptions, 'UnitDirsCount', 0);
  for j := 1 to c do
    FIniFile.DeleteKey(cProjectOptions, cINIUnitDir + IntToStr(j));
  SaveList(UnitDirs, 'UnitDirsCount', cINIUnitDir);
  for j := 0 to UnitDirs.Count-1 do
  begin
    s := '';
    lDelim := '';
    for c := 0 to 9 do
    begin
      if UnitDirsGrid[c, j] then  // True = 1, False = 0
        s := s + lDelim + '1'
      else
        s := s + lDelim + '0';
      lDelim := ',';
    end;
    FIniFile.WriteString(cProjectOptions, cINIUnitDirGrid + IntToStr(j+1), s);
  end;

  // Unit file list
  FIniFile.WriteInteger(cUnits, 'UnitCount', UnitList.Count);
  for j := 0 to UnitList.Count-1 do
  begin
    s := UnitList[j].FileName;
    FIniFile.WriteString(cUnits, 'Unit' + IntToStr(j+1),
        Format('%s,%s', [ExtractRelativepath(ProjectDir, s), BoolToStr(UnitList[j].Opened, False)]));
  end;

  Result := True;
end;

function TProject.Load(AProjectFile: TfpgString): Boolean;
var
  a: string;
  s: TfpgString;
  j: integer;
  l: integer;
  sl: TStringList;
  u: TUnit;

  // CName = xxxCount & IName is the Item name
  procedure LoadList(ASection: TfpgString; AList: TStringList; const CName, IName: TfpgString);
  var
    c: integer;
    i: integer;
  begin
    c := FIniFile.ReadInteger(ASection, CName, 0);
    for i := 0 to c-1 do
    begin
      s := FIniFile.ReadString(ASection, IName + IntToStr(i+1), '');
      if s <> '' then
        AList.Add(s);
    end;
  end;

begin
  Result := False;
  if AProjectFile = '' then
    raise Exception.Create('You need to specify a Project filename');

  if not Assigned(FIniFile) then
    FIniFile := TfpgINIFile.CreateExt(AProjectFile);

  ProjectDir := fpgExtractFilePath(AProjectFile);
  fpgSetCurrentDir(ProjectDir);
  ProjectName := FIniFile.ReadString(cProjectOptions, 'ProjectName', fpgChangeFileExt(fpgExtractFileName(AProjectFile), ''));
  MainUnit := FIniFile.ReadString(cProjectOptions, 'MainUnit', '');
  TargetFile := FIniFile.ReadString(cProjectOptions, 'TargetFile', '');
  DefaultMake := FIniFile.ReadInteger(cProjectOptions, 'DefaultMake', 0);
  UnitOutputDir := FIniFile.ReadString(cProjectOptions, 'UnitOutputDir', 'units/'+cMacro_Target+'/');

  // Load make options
  LoadList(cProjectOptions, MakeOptions, 'MakeOptionsCount', 'MakeOption');
  sl := TStringList.Create;
  try
    LoadList(cProjectOptions, sl, 'MakeOptionsCount', cINIMakeOptionGrid);
    SetLength(FMakeOptionsGrid, 6, MakeOptions.Count);    // 6 columns by X rows
    for j := 0 to sl.Count-1 do
    begin
      s := sl[j];
      for l := 0 to 5 do  // we know we only have 6 columns
      begin
        a := tiToken(s, ',', l+1);
        MakeOptionsGrid[l, j] := Boolean(StrToInt(a));  // 1 = True, 0 = False
      end;
    end;
  finally
    sl.Free;
  end;

  // Load Macro definitions
  LoadList(cProjectOptions, MacroNames, 'MacroCount', 'Macro');
  if MacroNames.Count > 0 then
    GMacroList.ResetToDefaults;
  MergeWithGlobalMacros;

  // Load Unit search dirs
  LoadList(cProjectOptions, UnitDirs, 'UnitDirsCount', 'UnitDir');
  sl := TStringList.Create;
  try
    LoadList(cProjectOptions, sl, 'UnitDirsCount', 'UnitDirEnabled');
    SetLength(FUnitDirsGrid, 10, UnitDirs.Count);    // 10 columns by X rows
    for j := 0 to sl.Count-1 do
    begin
      s := sl[j];
      for l := 0 to 9 do  // we know we only have 10 columns
      begin
        a := tiToken(s, ',', l+1);
        UnitDirsGrid[l, j] := Boolean(StrToInt(a));  // 1 = True, 0 = False
      end;
    end;
  finally
    sl.Free;
  end;

  // Load Unit file list
  sl := TStringList.Create;
  try
    LoadList(cUnits, sl, 'UnitCount', 'Unit');
    for j := 0 to sl.Count-1 do
    begin
      u := TUnit.Create;
      s := tiToken(sl[j], ',', 1);
      u.FileName := fpgExpandFileName(ProjectDir + s);
      u.Opened := Boolean(StrToInt(tiToken(sl[j], ',', 2)));  // 1 = True, 0 = False
      UnitList.Add(u);
    end;
  finally
    sl.Free;
  end;

  Result := True;
end;

function TProject.GenerateCmdLine(const AShowOnly: Boolean; const ABuildMode: integer): TfpgString;
var
  c: TfpgString;
  b: integer;
  eol: TfpgString;
  i: integer;
begin
  if AShowOnly then
    eol := LineEnding
  else
    eol := '';
  if ABuildMode = -1 then
    b := DefaultMake
  else
    b := ABuildMode;

  // include dirs
  for i := 0 to UnitDirs.Count-1 do
    if UnitDirsGrid[b, i] and UnitDirsGrid[7, i] then
      c := c + ' -Fi' + UnitDirs[i] + eol;
  // unit dirs
  for i := 0 to UnitDirs.Count-1 do
    if UnitDirsGrid[b, i] and UnitDirsGrid[6, i] then
      c := c + ' -Fu' + UnitDirs[i] + eol;
  // unit output dir
  if UnitOutputDir <> '' then
    c := c + ' -FU' + UnitOutputDir + eol;
  // make option - compiler flags
  for i := 0 to MakeOptions.Count-1 do
    if MakeOptionsGrid[b, i] then
      c := c + ' ' + MakeOptions[i];
  // target output file
  if TargetFile <> '' then
    c := c + ' -o' + TargetFile;
  // unit to start compilation
  c := c + ' ' + MainUnit;

  Result := c;
end;

procedure TProject.ClearAndInitMakeOptions(const ASize: integer);
begin
  FMakeOptions.Clear;
  SetLength(FMakeOptionsGrid, 0, 0);    // free items
  SetLength(FMakeOptionsGrid, 6, ASize);    // 6 columns by X rows
end;

procedure TProject.ClearAndInitUnitDirsGrid(const ASize: integer);
begin
  FUnitDirs.Clear;
  SetLength(FUnitDirsGrid, 0, 0); // free items
  SetLength(FUnitDirsGrid, 10, ASize);   // 10 columns by X rows
end;

procedure TProject.ClearAndInitMacrosGrid(const ASize: integer);
begin
  FMacroNames.Clear;
end;


initialization
  uProject := nil;

finalization
  FreeProject;

end.

