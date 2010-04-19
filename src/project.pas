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
    FUnitList: TUnitList;
    FIniFile: TfpgINIFile;
    FProjectDir: TfpgString;
    FTargetFile: TfpgString;
    FDefaultMake: integer;
    FMakeOptions: TStringList;
    FMacroNames: TStringList;
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
end;

destructor TProject.Destroy;
begin
  FMacroNames.Free;
  FMakeOptions.Free;
  FUnitList.Free;
  FIniFile.Free;
  inherited Destroy;
end;

function TProject.Save: Boolean;
var
  i: integer;
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

  FIniFile.WriteInteger(cProjectOptions, 'MakeOptionsCount', MakeOptions.Count);
  for i := 0 to MakeOptions.Count-1 do
    FIniFile.WriteString(cProjectOptions, 'MakeOption' + IntToStr(i+1), MakeOptions[i]);

  FIniFile.WriteInteger(cProjectOptions, 'MacroCount', MacroNames.Count);
  for i := 0 to MacroNames.Count-1 do
    FIniFile.WriteString(cProjectOptions, 'Macro' + IntToStr(i+1), MacroNames[i]);

  Result := True;
end;

function TProject.Load(AProjectFile: TfpgString): Boolean;
var
  i: integer;
  c: integer;
  s: TfpgString;
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

  c := FIniFile.ReadInteger(cProjectOptions, 'MakeOptionsCount', 0);
  for i := 0 to c-1 do
  begin
    s := FIniFile.ReadString(cProjectOptions, 'MakeOption' + IntToStr(i+1), '');
    if s <> '' then
      MakeOptions.Add(s);
  end;

  c := FIniFile.ReadInteger(cProjectOptions, 'MacroCount', 0);
  for i := 0 to c-1 do
  begin
    s := FIniFile.ReadString(cProjectOptions, 'Macro' + IntToStr(i+1), '');
    if s <> '' then
      MacroNames.Add(s);
  end;

  Result := True;
end;


initialization
  uProject := nil;

finalization
  uProject.Free;

end.

