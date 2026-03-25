{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2026 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      PasBuild project.xml backend. Reads project metadata from XML,
      delegates path resolution and build execution to PasBuild CLI.
}

unit ide.project.pasbuild;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base,
  ide.project.backend, ide.project.unitlist;

type
  { Dependency info from pasbuild resolve output }
  TPasBuildDependency = class(TObject)
  private
    FName: TfpgString;
    FVersion: TfpgString;
    FDepType: TfpgString;      { 'local' or 'external' }
    FProjectDir: TfpgString;
    FSourceDir: TfpgString;
    FUnitDir: TfpgString;
  public
    property Name: TfpgString read FName write FName;
    property Version: TfpgString read FVersion write FVersion;
    property DepType: TfpgString read FDepType write FDepType;
    property ProjectDir: TfpgString read FProjectDir write FProjectDir;
    property SourceDir: TfpgString read FSourceDir write FSourceDir;
    property UnitDir: TfpgString read FUnitDir write FUnitDir;
  end;

  { Resolved data for a single module }
  TPasBuildModule = class(TObject)
  private
    FName: TfpgString;
    FProjectType: TfpgString;
    FProjectDir: TfpgString;
    FMainSource: TfpgString;
    FExecutableName: TfpgString;
    FCommandLine: TfpgString;
    FCompilerExe: TfpgString;
    FDefines: TStringList;
    FUnitPaths: TStringList;
    FIncludePaths: TStringList;
    FDependencies: TList;        { list of TPasBuildDependency }
    FOutputDir: TfpgString;
    FUnitOutputDir: TfpgString;
    FExecutablePath: TfpgString;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: TfpgString read FName write FName;
    property ProjectType: TfpgString read FProjectType write FProjectType;
    property ProjectDir: TfpgString read FProjectDir write FProjectDir;
    property MainSource: TfpgString read FMainSource write FMainSource;
    property ExecutableName: TfpgString read FExecutableName write FExecutableName;
    property CommandLine: TfpgString read FCommandLine write FCommandLine;
    property CompilerExe: TfpgString read FCompilerExe write FCompilerExe;
    property Defines: TStringList read FDefines;
    property UnitPaths: TStringList read FUnitPaths;
    property IncludePaths: TStringList read FIncludePaths;
    property Dependencies: TList read FDependencies;
    property OutputDir: TfpgString read FOutputDir write FOutputDir;
    property UnitOutputDir: TfpgString read FUnitOutputDir write FUnitOutputDir;
    property ExecutablePath: TfpgString read FExecutablePath write FExecutablePath;
  end;

  { PasBuild project.xml backend }
  TPasBuildProjectBackend = class(TIDEProjectBackend)
  private
    FProjectFile: TfpgString;
    FProjectDir: TfpgString;
    FProjectName: TfpgString;
    FVersion: TfpgString;
    FProjectType: TfpgString;    { 'application', 'library', 'pom' }
    FSourceDirectory: TfpgString; { from <sourceDirectory>, default 'src/main/pascal' }
    FMainSource: TfpgString;
    FTargetFile: TfpgString;
    FUnitOutputDir: TfpgString;
    FUnitList: TUnitList;
    FUnitDirs: TStringList;
    FAvailableProfiles: TStringList;
    FActiveProfiles: TStringList;
    FModuleNames: TStringList;   { for aggregator projects }
    FBuildOrder: TStringList;
    FModules: TList;             { list of TPasBuildModule }
    FActiveModule: TPasBuildModule;
    FResolved: Boolean;
    FPasBuildPath: TfpgString;   { cached path to pasbuild executable }
    FAggregatorDir: TfpgString;  { root dir if this module is part of an aggregator }
    FAggregatorModule: TfpgString; { this module's name within the aggregator }
    procedure ParseProjectXML(const AFileName: TfpgString);
    procedure DetectAggregatorParent;
    procedure ClearModules;
    procedure ClearResolveData;
    function  InvokePasBuildResolve(const AProfiles: TfpgString;
                const AModule: TfpgString = ''): TfpgString;
    procedure ParseSingleModuleJSON(const AJSON: TfpgString);
    procedure ParseAggregatorJSON(const AJSON: TfpgString);
    function  ParseModuleFromJSON(AObj: TObject): TPasBuildModule;
    procedure UpdateFromActiveModule;
    function  FindPasBuild: TfpgString;
  protected
    function  GetProjectName: TfpgString; override;
    procedure SetProjectName(const AValue: TfpgString); override;
    function  GetProjectDir: TfpgString; override;
    procedure SetProjectDir(const AValue: TfpgString); override;
    function  GetMainSource: TfpgString; override;
    procedure SetMainSource(const AValue: TfpgString); override;
    function  GetTargetFile: TfpgString; override;
    procedure SetTargetFile(const AValue: TfpgString); override;
    function  GetUnitOutputDir: TfpgString; override;
    procedure SetUnitOutputDir(const AValue: TfpgString); override;
    function  GetUnitList: TUnitList; override;
    function  GetUnitDirs: TStringList; override;
    function  GetProjectFormat: TProjectFormat; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function  Load(const AProjectFile: TfpgString): Boolean; override;
    function  Save(const AFile: TfpgString = ''): Boolean; override;
    function  GenerateCmdLine(const AShowOnly: Boolean = False;
                const ABuildMode: integer = -1): TfpgString; override;
    function  GenerateGoalCmdLine(const AGoal: TfpgString): TfpgString; override;
    { PasBuild-specific }
    procedure Resolve; overload;
    procedure Resolve(const AProfiles: TfpgString); overload;
    function  FindModuleForFile(const AFilePath: TfpgString): TPasBuildModule;
    function  IsAggregator: Boolean;
    function  GetBuildDir: TfpgString;
    { Properties }
    property  ProjectFile: TfpgString read FProjectFile;
    property  Version: TfpgString read FVersion;
    property  ProjectType: TfpgString read FProjectType;
    property  SourceDirectory: TfpgString read FSourceDirectory;
    property  AvailableProfiles: TStringList read FAvailableProfiles;
    property  ActiveProfiles: TStringList read FActiveProfiles;
    property  ModuleNames: TStringList read FModuleNames;
    property  BuildOrder: TStringList read FBuildOrder;
    property  Modules: TList read FModules;
    property  ActiveModule: TPasBuildModule read FActiveModule write FActiveModule;
    property  Resolved: Boolean read FResolved;
  end;


implementation

uses
  fpjson, jsonparser, DOM, XMLRead,
  fpg_utils, process;


{ TPasBuildModule }

constructor TPasBuildModule.Create;
begin
  inherited Create;
  FDefines := TStringList.Create;
  FUnitPaths := TStringList.Create;
  FIncludePaths := TStringList.Create;
  FDependencies := TList.Create;
end;

destructor TPasBuildModule.Destroy;
var
  I: Integer;
begin
  for I := 0 to FDependencies.Count - 1 do
    TPasBuildDependency(FDependencies[I]).Free;
  FDependencies.Free;
  FIncludePaths.Free;
  FUnitPaths.Free;
  FDefines.Free;
  inherited Destroy;
end;


{ TPasBuildProjectBackend }

constructor TPasBuildProjectBackend.Create;
begin
  inherited Create;
  FUnitList := TUnitList.Create;
  FUnitDirs := TStringList.Create;
  FAvailableProfiles := TStringList.Create;
  FActiveProfiles := TStringList.Create;
  FModuleNames := TStringList.Create;
  FBuildOrder := TStringList.Create;
  FModules := TList.Create;
  FActiveModule := nil;
  FResolved := False;
  FPasBuildPath := '';
end;

destructor TPasBuildProjectBackend.Destroy;
begin
  ClearModules;
  FModules.Free;
  FBuildOrder.Free;
  FModuleNames.Free;
  FActiveProfiles.Free;
  FAvailableProfiles.Free;
  FUnitDirs.Free;
  FUnitList.Free;
  inherited Destroy;
end;

procedure TPasBuildProjectBackend.ClearModules;
var
  I: Integer;
begin
  for I := 0 to FModules.Count - 1 do
    TPasBuildModule(FModules[I]).Free;
  FModules.Clear;
  FActiveModule := nil;
end;

procedure TPasBuildProjectBackend.ClearResolveData;
begin
  ClearModules;
  FBuildOrder.Clear;
  FUnitDirs.Clear;
  FResolved := False;
end;

function TPasBuildProjectBackend.FindPasBuild: TfpgString;
begin
  if FPasBuildPath <> '' then
    Result := FPasBuildPath
  else
    Result := 'pasbuild';  { TProcess resolves via PATH }
end;

function IsModuleOf(const AParentXML, AChildDir: TfpgString): Boolean;
var
  Doc: TXMLDocument;
  RootNode, Node, ChildNode: TDOMNode;
  ParentDir, ModulePath: TfpgString;
begin
  Result := False;
  Doc := nil;
  try
    ReadXMLFile(Doc, AParentXML);
    RootNode := Doc.DocumentElement;
    if RootNode = nil then
      Exit;
    Node := RootNode.FindNode('modules');
    if Node = nil then
      Exit;
    ParentDir := IncludeTrailingPathDelimiter(fpgExtractFileDir(AParentXML));
    ChildNode := Node.FirstChild;
    while Assigned(ChildNode) do
    begin
      if (ChildNode.NodeName = 'module') and Assigned(ChildNode.FirstChild) then
      begin
        ModulePath := UTF8Encode(ChildNode.FirstChild.NodeValue);
        if IncludeTrailingPathDelimiter(ParentDir + ModulePath) = AChildDir then
        begin
          Result := True;
          Exit;
        end;
      end;
      ChildNode := ChildNode.NextSibling;
    end;
  except
  end;
  if Assigned(Doc) then
    Doc.Free;
end;

procedure TPasBuildProjectBackend.DetectAggregatorParent;
var
  CurrentDir: TfpgString;
  ParentDir: TfpgString;
  ParentXML: TfpgString;
  RootDir: TfpgString;
  Doc: TXMLDocument;
  RootNode, Node, ProfileNode, IdNode: TDOMNode;
begin
  FAggregatorDir := '';
  FAggregatorModule := '';

  { Walk up the directory tree to find the root aggregator.
    Each level must contain a project.xml with <modules> listing
    the child directory. Stop when the parent no longer qualifies. }
  RootDir := '';
  CurrentDir := FProjectDir;
  repeat
    ParentDir := IncludeTrailingPathDelimiter(
      fpgExtractFileDir(ExcludeTrailingPathDelimiter(CurrentDir)));
    if ParentDir = CurrentDir then
      Break; { reached filesystem root }
    ParentXML := ParentDir + 'project.xml';
    if not fpgFileExists(ParentXML) then
      Break;
    if not IsModuleOf(ParentXML, CurrentDir) then
      Break;
    RootDir := ParentDir;
    CurrentDir := ParentDir;
  until False;

  if RootDir = '' then
    Exit;

  FAggregatorDir := RootDir;
  FAggregatorModule := FProjectName;

  { Read version and profiles from the root aggregator }
  Doc := nil;
  try
    ReadXMLFile(Doc, RootDir + 'project.xml');
    RootNode := Doc.DocumentElement;
    if RootNode = nil then
      Exit;

    { Inherit version if sub-module has none }
    if FVersion = '' then
    begin
      Node := RootNode.FindNode('version');
      if Assigned(Node) and Assigned(Node.FirstChild) then
        FVersion := UTF8Encode(Node.FirstChild.NodeValue);
    end;

    { Grab available profiles from the root aggregator }
    Node := RootNode.FindNode('profiles');
    if Assigned(Node) then
    begin
      FAvailableProfiles.Clear;
      ProfileNode := Node.FirstChild;
      while Assigned(ProfileNode) do
      begin
        if ProfileNode.NodeName = 'profile' then
        begin
          IdNode := ProfileNode.FindNode('id');
          if Assigned(IdNode) and Assigned(IdNode.FirstChild) then
            FAvailableProfiles.Add(UTF8Encode(IdNode.FirstChild.NodeValue));
        end;
        ProfileNode := ProfileNode.NextSibling;
      end;
    end;
  except
  end;
  if Assigned(Doc) then
    Doc.Free;
end;

procedure TPasBuildProjectBackend.ParseProjectXML(const AFileName: TfpgString);
var
  Doc: TXMLDocument;
  RootNode, Node, ChildNode, ProfileNode, ModuleNode: TDOMNode;
begin
  FAvailableProfiles.Clear;
  FModuleNames.Clear;

  ReadXMLFile(Doc, AFileName);
  try
    RootNode := Doc.DocumentElement;
    if RootNode = nil then
      Exit;

    { Project name }
    Node := RootNode.FindNode('name');
    if Assigned(Node) and Assigned(Node.FirstChild) then
      FProjectName := UTF8Encode(Node.FirstChild.NodeValue);

    { Version }
    Node := RootNode.FindNode('version');
    if Assigned(Node) and Assigned(Node.FirstChild) then
      FVersion := UTF8Encode(Node.FirstChild.NodeValue);

    { Build config }
    Node := RootNode.FindNode('build');
    if Assigned(Node) then
    begin
      ChildNode := Node.FindNode('packaging');
      if Assigned(ChildNode) and Assigned(ChildNode.FirstChild) then
        FProjectType := UTF8Encode(ChildNode.FirstChild.NodeValue)
      else
        FProjectType := 'application';

      ChildNode := Node.FindNode('sourceDirectory');
      if Assigned(ChildNode) and Assigned(ChildNode.FirstChild) then
        FSourceDirectory := UTF8Encode(ChildNode.FirstChild.NodeValue)
      else
        FSourceDirectory := 'src/main/pascal';

      ChildNode := Node.FindNode('mainSource');
      if Assigned(ChildNode) and Assigned(ChildNode.FirstChild) then
        FMainSource := UTF8Encode(ChildNode.FirstChild.NodeValue);

      ChildNode := Node.FindNode('executableName');
      if Assigned(ChildNode) and Assigned(ChildNode.FirstChild) then
        FTargetFile := UTF8Encode(ChildNode.FirstChild.NodeValue);
    end;

    { Profiles — extract IDs only }
    Node := RootNode.FindNode('profiles');
    if Assigned(Node) then
    begin
      ProfileNode := Node.FirstChild;
      while Assigned(ProfileNode) do
      begin
        if ProfileNode.NodeName = 'profile' then
        begin
          ChildNode := ProfileNode.FindNode('id');
          if Assigned(ChildNode) and Assigned(ChildNode.FirstChild) then
            FAvailableProfiles.Add(UTF8Encode(ChildNode.FirstChild.NodeValue));
        end;
        ProfileNode := ProfileNode.NextSibling;
      end;
    end;

    { Modules — for aggregator projects }
    Node := RootNode.FindNode('modules');
    if Assigned(Node) then
    begin
      ModuleNode := Node.FirstChild;
      while Assigned(ModuleNode) do
      begin
        if (ModuleNode.NodeName = 'module') and Assigned(ModuleNode.FirstChild) then
          FModuleNames.Add(UTF8Encode(ModuleNode.FirstChild.NodeValue));
        ModuleNode := ModuleNode.NextSibling;
      end;
    end;
  finally
    Doc.Free;
  end;
end;

function TPasBuildProjectBackend.InvokePasBuildResolve(
  const AProfiles: TfpgString; const AModule: TfpgString): TfpgString;
var
  P: TProcess;
  Buf: string;
  BytesRead, TotalRead: Integer;
const
  BufSize = 4096;
begin
  Result := '';
  P := TProcess.Create(nil);
  try
    P.Executable := FindPasBuild;
    P.Parameters.Add('resolve');
    if AProfiles <> '' then
    begin
      P.Parameters.Add('-p');
      P.Parameters.Add(AProfiles);
    end;
    if AModule <> '' then
    begin
      P.Parameters.Add('-m');
      P.Parameters.Add(AModule);
    end;
    P.CurrentDirectory := FProjectDir;
    P.Options := [poUsePipes, poStdErrToOutput];

    {$IFDEF DEBUG}
    WriteLn('DEBUG: TPasBuildProjectBackend.InvokePasBuildResolve');
    WriteLn('  Executable: ', P.Executable);
    WriteLn('  Parameters: ', P.Parameters.Text);
    WriteLn('  Directory:  ', P.CurrentDirectory);
    {$ENDIF}

    P.Execute;

    { Read all output }
    Buf := '';
    TotalRead := 0;
    SetLength(Buf, BufSize);
    repeat
      BytesRead := P.Output.Read(Buf[TotalRead + 1], BufSize);
      TotalRead := TotalRead + BytesRead;
      SetLength(Buf, TotalRead + BufSize);
    until BytesRead = 0;
    SetLength(Buf, TotalRead);

    P.WaitOnExit;

    if P.ExitCode <> 0 then
    begin
      {$IFDEF DEBUG}
      WriteLn('DEBUG: pasbuild resolve failed with exit code ', P.ExitCode);
      WriteLn('  Output: ', Buf);
      {$ENDIF}
      Exit;
    end;

    (* Strip [INFO] lines from output — JSON starts at first '{' *)
    Result := Copy(Buf, Pos('{', Buf), Length(Buf));
  finally
    P.Free;
  end;
end;

function TPasBuildProjectBackend.ParseModuleFromJSON(
  AObj: TObject): TPasBuildModule;
var
  JObj, DepObj, CompObj, OutputObj: TJSONObject;
  Arr: TJSONArray;
  I: Integer;
  Dep: TPasBuildDependency;
begin
  Result := TPasBuildModule.Create;
  JObj := TJSONObject(AObj);

  if JObj.IndexOfName('name') >= 0 then
    Result.Name := JObj.Strings['name'];
  if JObj.IndexOfName('projectType') >= 0 then
    Result.ProjectType := JObj.Strings['projectType'];
  if JObj.IndexOfName('projectDir') >= 0 then
    Result.ProjectDir := JObj.Strings['projectDir'];
  if JObj.IndexOfName('mainSource') >= 0 then
    Result.MainSource := JObj.Strings['mainSource'];
  if JObj.IndexOfName('executableName') >= 0 then
    Result.ExecutableName := JObj.Strings['executableName'];

  { Compiler section }
  if JObj.IndexOfName('compiler') >= 0 then
  begin
    CompObj := JObj.Objects['compiler'];
    if CompObj.IndexOfName('executable') >= 0 then
      Result.CompilerExe := CompObj.Strings['executable'];
    if CompObj.IndexOfName('commandLine') >= 0 then
      Result.CommandLine := CompObj.Strings['commandLine'];
  end;

  { Defines }
  if JObj.IndexOfName('defines') >= 0 then
  begin
    Arr := JObj.Arrays['defines'];
    for I := 0 to Arr.Count - 1 do
      Result.Defines.Add(Arr.Strings[I]);
  end;

  { Unit paths }
  if JObj.IndexOfName('unitPaths') >= 0 then
  begin
    Arr := JObj.Arrays['unitPaths'];
    for I := 0 to Arr.Count - 1 do
      Result.UnitPaths.Add(Arr.Strings[I]);
  end;

  { Include paths }
  if JObj.IndexOfName('includePaths') >= 0 then
  begin
    Arr := JObj.Arrays['includePaths'];
    for I := 0 to Arr.Count - 1 do
      Result.IncludePaths.Add(Arr.Strings[I]);
  end;

  { Dependencies }
  if JObj.IndexOfName('dependencies') >= 0 then
  begin
    Arr := JObj.Arrays['dependencies'];
    for I := 0 to Arr.Count - 1 do
    begin
      DepObj := Arr.Objects[I];
      Dep := TPasBuildDependency.Create;
      if DepObj.IndexOfName('name') >= 0 then
        Dep.Name := DepObj.Strings['name'];
      if DepObj.IndexOfName('version') >= 0 then
        Dep.Version := DepObj.Strings['version'];
      if DepObj.IndexOfName('type') >= 0 then
        Dep.DepType := DepObj.Strings['type'];
      if DepObj.IndexOfName('projectDir') >= 0 then
        Dep.ProjectDir := DepObj.Strings['projectDir'];
      if DepObj.IndexOfName('sourceDir') >= 0 then
        Dep.SourceDir := DepObj.Strings['sourceDir'];
      if DepObj.IndexOfName('unitDir') >= 0 then
        Dep.UnitDir := DepObj.Strings['unitDir'];
      Result.Dependencies.Add(Dep);
    end;
  end;

  { Output section }
  if JObj.IndexOfName('output') >= 0 then
  begin
    OutputObj := JObj.Objects['output'];
    if OutputObj.IndexOfName('directory') >= 0 then
      Result.OutputDir := OutputObj.Strings['directory'];
    if OutputObj.IndexOfName('unitDirectory') >= 0 then
      Result.UnitOutputDir := OutputObj.Strings['unitDirectory'];
    if OutputObj.IndexOfName('executable') >= 0 then
      Result.ExecutablePath := OutputObj.Strings['executable'];
  end;
end;

procedure TPasBuildProjectBackend.ParseSingleModuleJSON(const AJSON: TfpgString);
var
  JSONData: TJSONData;
  RootObj: TJSONObject;
  Arr: TJSONArray;
  Module: TPasBuildModule;
  I: Integer;
begin
  ClearResolveData;
  JSONData := GetJSON(AJSON);
  try
    RootObj := TJSONObject(JSONData);

    { Parse active/available profiles from resolve output }
    FActiveProfiles.Clear;
    if RootObj.IndexOfName('activeProfiles') >= 0 then
    begin
      Arr := RootObj.Arrays['activeProfiles'];
      for I := 0 to Arr.Count - 1 do
        FActiveProfiles.Add(Arr.Strings[I]);
    end;

    { The resolve data at the root level describes a single module }
    Module := ParseModuleFromJSON(RootObj);

    { Override name/projectDir from the project section if present }
    if RootObj.IndexOfName('project') >= 0 then
    begin
      if RootObj.Objects['project'].IndexOfName('name') >= 0 then
        Module.Name := RootObj.Objects['project'].Strings['name'];
      if RootObj.Objects['project'].IndexOfName('projectDir') >= 0 then
        Module.ProjectDir := RootObj.Objects['project'].Strings['projectDir'];
      if RootObj.Objects['project'].IndexOfName('projectType') >= 0 then
        Module.ProjectType := RootObj.Objects['project'].Strings['projectType'];
      if RootObj.Objects['project'].IndexOfName('mainSource') >= 0 then
        Module.MainSource := RootObj.Objects['project'].Strings['mainSource'];
      if RootObj.Objects['project'].IndexOfName('executableName') >= 0 then
        Module.ExecutableName := RootObj.Objects['project'].Strings['executableName'];
    end;

    FModules.Add(Module);
    FActiveModule := Module;
    FBuildOrder.Add(Module.Name);
    UpdateFromActiveModule;
    FResolved := True;
  finally
    JSONData.Free;
  end;
end;

procedure TPasBuildProjectBackend.ParseAggregatorJSON(const AJSON: TfpgString);
var
  JSONData: TJSONData;
  RootObj: TJSONObject;
  Arr: TJSONArray;
  Module: TPasBuildModule;
  I: Integer;
begin
  ClearResolveData;
  JSONData := GetJSON(AJSON);
  try
    RootObj := TJSONObject(JSONData);

    { Active profiles }
    FActiveProfiles.Clear;
    if RootObj.IndexOfName('activeProfiles') >= 0 then
    begin
      Arr := RootObj.Arrays['activeProfiles'];
      for I := 0 to Arr.Count - 1 do
        FActiveProfiles.Add(Arr.Strings[I]);
    end;

    { Build order }
    if RootObj.IndexOfName('buildOrder') >= 0 then
    begin
      Arr := RootObj.Arrays['buildOrder'];
      for I := 0 to Arr.Count - 1 do
        FBuildOrder.Add(Arr.Strings[I]);
    end;

    { Modules }
    if RootObj.IndexOfName('modules') >= 0 then
    begin
      Arr := RootObj.Arrays['modules'];
      for I := 0 to Arr.Count - 1 do
      begin
        Module := ParseModuleFromJSON(Arr.Objects[I]);
        FModules.Add(Module);
      end;
    end;

    { Set first non-pom module as active by default }
    FActiveModule := nil;
    for I := 0 to FModules.Count - 1 do
    begin
      Module := TPasBuildModule(FModules[I]);
      if Module.ProjectType <> 'pom' then
      begin
        FActiveModule := Module;
        Break;
      end;
    end;

    if FActiveModule <> nil then
      UpdateFromActiveModule;

    FResolved := True;
  finally
    JSONData.Free;
  end;
end;

procedure TPasBuildProjectBackend.UpdateFromActiveModule;
begin
  if FActiveModule = nil then
    Exit;

  { Sync abstract interface fields from active module }
  FMainSource := FActiveModule.MainSource;
  FTargetFile := FActiveModule.ExecutableName;
  FUnitOutputDir := FActiveModule.UnitOutputDir;

  { Update unit dirs from active module's resolved paths }
  FUnitDirs.Assign(FActiveModule.UnitPaths);
end;

function TPasBuildProjectBackend.Load(
  const AProjectFile: TfpgString): Boolean;
begin
  Result := False;
  FProjectFile := AProjectFile;
  FProjectDir := fpgExtractFileDir(AProjectFile);
  if FProjectDir = '' then
    FProjectDir := fpgGetCurrentDir;
  FProjectDir := IncludeTrailingPathDelimiter(FProjectDir);

  try
    ParseProjectXML(AProjectFile);
    DetectAggregatorParent;
    Result := True;
  except
    on E: Exception do
    begin
      {$IFDEF DEBUG}
      WriteLn('DEBUG: Failed to parse project.xml: ', E.Message);
      {$ENDIF}
    end;
  end;
end;

function TPasBuildProjectBackend.Save(const AFile: TfpgString): Boolean;
begin
  { PasBuild projects are not saved by the IDE — project.xml is
    maintained by the developer or PasBuild tooling. }
  Result := True;
end;

function TPasBuildProjectBackend.GenerateCmdLine(const AShowOnly: Boolean;
  const ABuildMode: integer): TfpgString;
begin
  { For PasBuild projects, build is delegated to pasbuild CLI.
    Return the full pasbuild compile command. }
  Result := GenerateGoalCmdLine('compile');
end;

function TPasBuildProjectBackend.GenerateGoalCmdLine(const AGoal: TfpgString): TfpgString;
begin
  Result := FindPasBuild + ' ' + AGoal;
  if FAggregatorDir <> '' then
    Result := Result + ' -f ' + FAggregatorDir + 'project.xml'
                     + ' -m ' + FAggregatorModule;
  { clean does not need profiles, but it does no harm to pass them }
  if FActiveProfiles.Count > 0 then
    Result := Result + ' -p ' + FActiveProfiles.CommaText;
end;

procedure TPasBuildProjectBackend.Resolve;
begin
  Resolve(FActiveProfiles.CommaText);
end;

procedure TPasBuildProjectBackend.Resolve(const AProfiles: TfpgString);
var
  JSONOutput: TfpgString;
begin
  JSONOutput := InvokePasBuildResolve(AProfiles);
  if JSONOutput = '' then
  begin
    {$IFDEF DEBUG}
    WriteLn('DEBUG: pasbuild resolve returned empty output');
    {$ENDIF}
    Exit;
  end;

  if IsAggregator then
    ParseAggregatorJSON(JSONOutput)
  else
    ParseSingleModuleJSON(JSONOutput);
end;

function TPasBuildProjectBackend.FindModuleForFile(
  const AFilePath: TfpgString): TPasBuildModule;
var
  I: Integer;
  Module: TPasBuildModule;
begin
  Result := nil;
  for I := 0 to FModules.Count - 1 do
  begin
    Module := TPasBuildModule(FModules[I]);
    if Pos(Module.ProjectDir, AFilePath) = 1 then
    begin
      { Prefer the most specific match (longest projectDir) }
      if (Result = nil) or
         (Length(Module.ProjectDir) > Length(Result.ProjectDir)) then
        Result := Module;
    end;
  end;
end;

function TPasBuildProjectBackend.IsAggregator: Boolean;
begin
  Result := FProjectType = 'pom';
end;

function TPasBuildProjectBackend.GetBuildDir: TfpgString;
begin
  if FAggregatorDir <> '' then
    Result := FAggregatorDir
  else
    Result := FProjectDir;
end;

function TPasBuildProjectBackend.GetProjectName: TfpgString;
begin
  Result := FProjectName;
end;

procedure TPasBuildProjectBackend.SetProjectName(const AValue: TfpgString);
begin
  FProjectName := AValue;
end;

function TPasBuildProjectBackend.GetProjectDir: TfpgString;
begin
  Result := FProjectDir;
end;

procedure TPasBuildProjectBackend.SetProjectDir(const AValue: TfpgString);
begin
  FProjectDir := AValue;
end;

function TPasBuildProjectBackend.GetMainSource: TfpgString;
begin
  Result := FMainSource;
end;

procedure TPasBuildProjectBackend.SetMainSource(const AValue: TfpgString);
begin
  FMainSource := AValue;
end;

function TPasBuildProjectBackend.GetTargetFile: TfpgString;
begin
  Result := FTargetFile;
end;

procedure TPasBuildProjectBackend.SetTargetFile(const AValue: TfpgString);
begin
  FTargetFile := AValue;
end;

function TPasBuildProjectBackend.GetUnitOutputDir: TfpgString;
begin
  Result := FUnitOutputDir;
end;

procedure TPasBuildProjectBackend.SetUnitOutputDir(const AValue: TfpgString);
begin
  FUnitOutputDir := AValue;
end;

function TPasBuildProjectBackend.GetUnitList: TUnitList;
begin
  Result := FUnitList;
end;

function TPasBuildProjectBackend.GetUnitDirs: TStringList;
begin
  Result := FUnitDirs;
end;

function TPasBuildProjectBackend.GetProjectFormat: TProjectFormat;
begin
  Result := pfPasBuild;
end;

end.
