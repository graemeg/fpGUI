unit Project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UnitList, fpg_base, fpg_iniutils;

type
  TProject = class(TObject)
  private
    FProjectName: TfpgString;
    FMainUnit: TfpgString;
    FUnitDirs: TStringList;
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
    property    MacroNames: TStringList read FMacroNames;
    property    UnitDirs: TStringList read FUnitDirs;
    property    UnitOutputDir: TfpgString read FUnitOutputDir write FUnitOutputDir;
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
  SaveList(MakeOptions, 'MakeOptionsCount', 'MakeOption');

  // macros definitions
  SaveList(MacroNames, 'MacroCount', 'Macro');

  // unit search directories
  SaveList(UnitDirs, 'UnitDirsCount', 'UnitDir');

  FIniFile.WriteString(cProjectOptions, 'UnitOutputDir', UnitOutputDir);

  Result := True;
end;

function TProject.Load(AProjectFile: TfpgString): Boolean;
var
  s: TfpgString;

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

  // Load Macro definitions
  LoadList(MacroNames, 'MacroCount', 'Macro');

  // Load Unit search dirs
  LoadList(UnitDirs, 'UnitDirsCount', 'unitdir');

  UnitOutputDir := FIniFile.ReadString(cProjectOptions, 'UnitOutputDir', 'units/i386-linux/');

  Result := True;
end;


initialization
  uProject := nil;

finalization
  uProject.Free;

end.

