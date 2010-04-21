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
  public
    constructor Create;
    destructor  Destroy; override;
    function    Save: Boolean;
    function    Load(AProjectFile: TfpgString): Boolean;
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
  ,fpg_utils
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

function tiNumToken(const AValue, AToken : string): integer;
var
  i, iCount : integer;
  lsValue : string;
begin
  Result := 0;
  if AValue = '' then
    Exit; //==>

  iCount := 0;
  lsValue := AValue;
  i := pos(AToken, lsValue);
  while i <> 0 do begin
    delete(lsValue, i, length(AToken));
    inc(iCount);
    i := pos(AToken, lsValue);
  end;
  Result := iCount + 1;
end;


function tiToken(const AValue, AToken : string; const APos : integer): string;
var
  i, iCount, iNumToken : integer;
  lsValue : string;
begin
  result := '';

  iNumToken := tiNumToken(AValue, AToken);
  if APos = 1 then
  begin
    if pos(AToken, AValue) = 0 then
      result := AValue
    else
      result := copy(AValue, 1, pos(AToken, AValue)-1);
  end
  else if (iNumToken < APos-1) or (APos<1) then
  begin
    result := '';
  end
  else
  begin
    { Remove leading blocks }
    iCount := 1;
    lsValue := AValue;
    i := pos(AToken, lsValue);
    while (i<>0) and (iCount<APos) do
    begin
      delete(lsValue, 1, i + length(AToken) - 1);
      inc(iCount);
      i := pos(AToken, lsValue);
    end;

    if (i=0) and (iCount=APos) then
      result := lsValue
    else if (i=0) and (iCount<>APos) then
      result := ''
    else
      result := copy(lsValue, 1, i-1);
  end;
end;


{ TProject }

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

function TProject.Save: Boolean;

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
  if ProjectName = '' then
    raise Exception.Create('Project name has not been specified yet');

  if not Assigned(FIniFile) then
    FIniFile := TfpgINIFile.CreateExt(ProjectDir + ProjectName + cProjectExt);

  FIniFile.WriteString(cProjectOptions, 'ProjectDir', ProjectDir);
  FIniFile.WriteString(cProjectOptions, 'ProjectName', ProjectName);
  FIniFile.WriteString(cProjectOptions, 'MainUnit', MainUnit);
  FIniFile.WriteString(cProjectOptions, 'TargetFile', TargetFile);
  FIniFile.WriteInteger(cProjectOptions, 'DefaultMake', DefaultMake);

  // various make options
  SaveList(MakeOptions, 'MakeOptionsCount', cINIMakeOption);

  // macros definitions
  SaveList(MacroNames, 'MacroCount', 'Macro');

  // unit search directories
  SaveList(UnitDirs, 'UnitDirsCount', 'UnitDir');

  FIniFile.WriteString(cProjectOptions, 'UnitOutputDir', UnitOutputDir);

  Result := True;
end;

function TProject.Load(AProjectFile: TfpgString): Boolean;
var
  a: string;
  s: TfpgString;
  j: integer;
  l: integer;
  sl: TStringList;

  // CName = xxxCount & IName is the Item name
  procedure LoadList(AList: TStringList; const CName, IName: TfpgString);
  var
    c: integer;
    i: integer;
  begin
    c := FIniFile.ReadInteger(cProjectOptions, CName, 0);
    for i := 0 to c-1 do
    begin
      s := FIniFile.ReadString(cProjectOptions, IName + IntToStr(i+1), '');
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

  ProjectDir := FIniFile.ReadString(cProjectOptions, 'ProjectDir', fpgExtractFilePath(AProjectFile));
  ProjectName := FIniFile.ReadString(cProjectOptions, 'ProjectName', ChangeFileExt(fpgExtractFileName(AProjectFile), ''));
  MainUnit := FIniFile.ReadString(cProjectOptions, 'MainUnit', '');
  TargetFile := FIniFile.ReadString(cProjectOptions, 'TargetFile', '');
  DefaultMake := FIniFile.ReadInteger(cProjectOptions, 'DefaultMake', 0);

  // Load make options
  LoadList(MakeOptions, 'MakeOptionsCount', 'MakeOption');
  sl := TStringList.Create;
  try
    LoadList(sl, 'MakeOptionsCount', 'MakeOptionEnabled');
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
  LoadList(MacroNames, 'MacroCount', 'Macro');

  // Load Unit search dirs
  LoadList(UnitDirs, 'UnitDirsCount', 'UnitDir');
  sl := TStringList.Create;
  try
    LoadList(sl, 'UnitDirsCount', 'UnitDirEnabled');
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

  UnitOutputDir := FIniFile.ReadString(cProjectOptions, 'UnitOutputDir', 'units/i386-linux/');

  Result := True;
end;


initialization
  uProject := nil;

finalization
  uProject.Free;

end.

