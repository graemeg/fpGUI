{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2026 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Legacy .project INI-based project backend.
}

unit ide.project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ide.project.backend, ide.project.unitlist, fpg_base,
  fpg_iniutils;

type
  TBooleanGrid = array of array of Boolean;

  TLegacyProjectBackend = class(TIDEProjectBackend)
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
    constructor Create; override;
    destructor  Destroy; override;
    { TIDEProjectBackend overrides }
    function    Load(const AProjectFile: TfpgString): Boolean; override;
    function    Save(const AFile: TfpgString = ''): Boolean; override;
    function    GenerateCmdLine(const AShowOnly: Boolean = False; const ABuildMode: integer = -1): TfpgString; override;
    function    GetProjectName: TfpgString; override;
    procedure   SetProjectName(const AValue: TfpgString); override;
    function    GetProjectDir: TfpgString; override;
    procedure   SetProjectDir(const AValue: TfpgString); override;
    function    GetMainSource: TfpgString; override;
    procedure   SetMainSource(const AValue: TfpgString); override;
    function    GetTargetFile: TfpgString; override;
    procedure   SetTargetFile(const AValue: TfpgString); override;
    function    GetUnitOutputDir: TfpgString; override;
    procedure   SetUnitOutputDir(const AValue: TfpgString); override;
    function    GetUnitList: TUnitList; override;
    function    GetUnitDirs: TStringList; override;
    function    GetProjectFormat: TProjectFormat; override;
    { Legacy-specific methods }
    procedure   ClearAndInitMakeOptions(const ASize: integer);
    procedure   ClearAndInitUnitDirsGrid(const ASize: integer);
    procedure   ClearAndInitMacrosGrid(const ASize: integer);
    { Legacy-specific properties }
    property    DefaultMake: integer read FDefaultMake write FDefaultMake;
    property    MakeOptions: TStringList read FMakeOptions;
    property    MakeOptionsGrid: TBooleanGrid read FMakeOptionsGrid write FMakeOptionsGrid;
    property    MacroNames: TStringList read FMacroNames;
    property    UnitDirsGrid: TBooleanGrid read FUnitDirsGrid write FUnitDirsGrid;
  end;


// lazy-mans singleton
function GProject: TIDEProjectBackend;

// typed accessor for legacy-specific features (nil if not a legacy project)
function GLegacyProject: TLegacyProjectBackend;

// replace the global project instance
procedure SetProject(AProject: TIDEProjectBackend);

// create the appropriate backend based on the project filename
function CreateProjectBackend(const AFileName: TfpgString): TIDEProjectBackend;

// detect project format from filename
function DetectProjectFormat(const AFileName: TfpgString): TProjectFormat;

procedure FreeProject;


implementation

uses
  ide.consts
  ,ide.utils
  ,fpg_utils
  ,ide.macros
  ,ide.project.pasbuild
  ;


var
  uProject: TIDEProjectBackend;

function GProject: TIDEProjectBackend;
begin
  if not Assigned(uProject) then
    uProject := TLegacyProjectBackend.Create;
  Result := uProject;
end;

function GLegacyProject: TLegacyProjectBackend;
begin
  if GProject is TLegacyProjectBackend then
    Result := TLegacyProjectBackend(GProject)
  else
    Result := nil;
end;

procedure SetProject(AProject: TIDEProjectBackend);
begin
  if Assigned(uProject) then
    uProject.Free;
  uProject := AProject;
end;

function DetectProjectFormat(const AFileName: TfpgString): TProjectFormat;
begin
  if (fpgExtractFileName(AFileName) = 'project.xml') or
     (fpgExtractFileExt(AFileName) = '.xml') then
    Result := pfPasBuild
  else
    Result := pfLegacy;
end;

function CreateProjectBackend(const AFileName: TfpgString): TIDEProjectBackend;
begin
  if DetectProjectFormat(AFileName) = pfPasBuild then
    Result := TPasBuildProjectBackend.Create
  else
    Result := TLegacyProjectBackend.Create;
end;

procedure FreeProject;
begin
  uProject.Free;
  uProject := nil;
end;


{ TLegacyProjectBackend }

procedure TLegacyProjectBackend.MergeWithGlobalMacros;
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

constructor TLegacyProjectBackend.Create;
begin
  inherited Create;
  FUnitList := TUnitList.Create;
  FMakeOptions := TStringList.Create;
  FMacroNames := TStringList.Create;
  FUnitDirs := TStringList.Create;
end;

destructor TLegacyProjectBackend.Destroy;
begin
  FUnitDirs.Free;
  FMacroNames.Free;
  FMakeOptions.Free;
  FUnitList.Free;
  FIniFile.Free;
  inherited Destroy;
end;

function TLegacyProjectBackend.GetProjectName: TfpgString;
begin
  Result := FProjectName;
end;

procedure TLegacyProjectBackend.SetProjectName(const AValue: TfpgString);
begin
  FProjectName := AValue;
end;

function TLegacyProjectBackend.GetProjectDir: TfpgString;
begin
  Result := FProjectDir;
end;

procedure TLegacyProjectBackend.SetProjectDir(const AValue: TfpgString);
begin
  FProjectDir := AValue;
end;

function TLegacyProjectBackend.GetMainSource: TfpgString;
begin
  Result := FMainUnit;
end;

procedure TLegacyProjectBackend.SetMainSource(const AValue: TfpgString);
begin
  FMainUnit := AValue;
end;

function TLegacyProjectBackend.GetTargetFile: TfpgString;
begin
  Result := FTargetFile;
end;

procedure TLegacyProjectBackend.SetTargetFile(const AValue: TfpgString);
begin
  FTargetFile := AValue;
end;

function TLegacyProjectBackend.GetUnitOutputDir: TfpgString;
begin
  Result := FUnitOutputDir;
end;

procedure TLegacyProjectBackend.SetUnitOutputDir(const AValue: TfpgString);
begin
  FUnitOutputDir := AValue;
end;

function TLegacyProjectBackend.GetUnitList: TUnitList;
begin
  Result := FUnitList;
end;

function TLegacyProjectBackend.GetUnitDirs: TStringList;
begin
  Result := FUnitDirs;
end;

function TLegacyProjectBackend.GetProjectFormat: TProjectFormat;
begin
  Result := pfLegacy;
end;

function TLegacyProjectBackend.Save(const AFile: TfpgString = ''): Boolean;
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
  { now lets save new info }
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
  { first delete old items in ini file }
  c := FIniFile.ReadInteger(cProjectOptions, 'MacroCount', 0);
  for j := 1 to c do
    FIniFile.DeleteKey(cProjectOptions, 'Macro' + IntToStr(j));
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

function TLegacyProjectBackend.Load(const AProjectFile: TfpgString): Boolean;
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

  FProjectFile := AProjectFile;

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

function TLegacyProjectBackend.GenerateCmdLine(const AShowOnly: Boolean; const ABuildMode: integer): TfpgString;
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

procedure TLegacyProjectBackend.ClearAndInitMakeOptions(const ASize: integer);
begin
  FMakeOptions.Clear;
  SetLength(FMakeOptionsGrid, 0, 0);    // free items
  SetLength(FMakeOptionsGrid, 6, ASize);    // 6 columns by X rows
end;

procedure TLegacyProjectBackend.ClearAndInitUnitDirsGrid(const ASize: integer);
begin
  FUnitDirs.Clear;
  SetLength(FUnitDirsGrid, 0, 0); // free items
  SetLength(FUnitDirsGrid, 10, ASize);   // 10 columns by X rows
end;

procedure TLegacyProjectBackend.ClearAndInitMacrosGrid(const ASize: integer);
begin
  FMacroNames.Clear;
end;


initialization
  uProject := nil;

finalization
  FreeProject;

end.
