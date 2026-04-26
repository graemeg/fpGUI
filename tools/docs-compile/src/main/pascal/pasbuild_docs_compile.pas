program pasbuild_docs_compile;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Process, XMLRead, DOM;

const
  LOG_PREFIX   = '[docs-compile] ';
  PLUGIN_PHASE = 'none';

var
  gVerbose: Boolean = False;

procedure Log(const AMsg: string);
begin
  WriteLn(LOG_PREFIX + AMsg);
end;

procedure LogVerbose(const AMsg: string);
begin
  if gVerbose then
    WriteLn(LOG_PREFIX + AMsg);
end;

{ Returns the full path to the wipfc binary and sets AWipfcDir to its containing
  directory. Checks the WIPFC env var first, then searches PATH. }
function FindWipfcBinary(out AWipfcDir: string): string;
var
  WipfcEnv, Candidate: string;
begin
  Result := '';
  AWipfcDir := '';
  WipfcEnv := GetEnvironmentVariable('WIPFC');
  if WipfcEnv <> '' then
  begin
    {$IFDEF WINDOWS}
    Candidate := IncludeTrailingPathDelimiter(WipfcEnv) + 'wipfc.exe';
    {$ELSE}
    Candidate := IncludeTrailingPathDelimiter(WipfcEnv) + 'wipfc';
    {$ENDIF}
    if FileExists(Candidate) then
    begin
      AWipfcDir := ExcludeTrailingPathDelimiter(WipfcEnv);
      Result := Candidate;
      Exit;
    end;
  end;
  {$IFDEF WINDOWS}
  Result := FileSearch('wipfc.exe', GetEnvironmentVariable('PATH'));
  {$ELSE}
  Result := FileSearch('wipfc', GetEnvironmentVariable('PATH'));
  {$ENDIF}
  if Result <> '' then
    AWipfcDir := ExcludeTrailingPathDelimiter(ExtractFilePath(Result));
end;

{ Copies the current process environment into AEnv, ensuring WIPFC is set. }
procedure PopulateEnv(AEnv: TStrings; const AWipfcDir: string);
var
  i: Integer;
  s: string;
  HasWipfc: Boolean;
begin
  HasWipfc := False;
  i := 1;
  repeat
    s := GetEnvironmentString(i);
    if s = '' then
      Break;
    AEnv.Add(s);
    if Pos('WIPFC=', s) = 1 then
      HasWipfc := True;
    Inc(i);
  until False;
  if not HasWipfc then
    AEnv.Add('WIPFC=' + AWipfcDir);
end;

{ Runs wipfc to compile AInputFile into AOutputFile.
  wipfc requires: wipfc -i <input.ipf> -o <output.inf> }
function RunWipfc(const AWipfcBin, AWipfcDir, AInputFile, AOutputFile: string): Boolean;
var
  Proc: TProcess;
begin
  LogVerbose('Exec: ' + AWipfcBin
    + ' -i ' + AInputFile
    + ' -o ' + AOutputFile);
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := AWipfcBin;
    Proc.Parameters.Add('-i');
    Proc.Parameters.Add(AInputFile);
    Proc.Parameters.Add('-o');
    Proc.Parameters.Add(AOutputFile);
    PopulateEnv(Proc.Environment, AWipfcDir);
    Proc.Options := [poWaitOnExit];
    Proc.Execute;
    Result := Proc.ExitCode = 0;
  finally
    Proc.Free;
  end;
end;

{ Parses project.xml and returns the list of module directories.
  For a POM (multi-module) project, returns each declared module path.
  For a single-module project, returns AProjDir itself. }
function GetModulePaths(const AProjDir: string): TStringList;
var
  Doc: TXMLDocument;
  ProjFile: string;
  Root, ModulesNode, ModuleNode: TDOMNode;
  i: Integer;
  ModulePath: string;
begin
  Result := TStringList.Create;
  ProjFile := AProjDir + PathDelim + 'project.xml';
  if not FileExists(ProjFile) then
  begin
    Result.Add(AProjDir);
    Exit;
  end;
  Doc := nil;
  try
    ReadXMLFile(Doc, ProjFile);
    Root := Doc.DocumentElement;
    ModulesNode := Root.FindNode('modules');
    if ModulesNode <> nil then
      for i := 0 to ModulesNode.ChildNodes.Count - 1 do
      begin
        ModuleNode := ModulesNode.ChildNodes[i];
        if ModuleNode.NodeName = 'module' then
        begin
          ModulePath := Trim(ModuleNode.TextContent);
          if ModulePath <> '' then
          begin
            ModulePath := StringReplace(ModulePath, '/', PathDelim, [rfReplaceAll]);
            Result.Add(AProjDir + PathDelim + ModulePath);
          end;
        end;
      end;
    if Result.Count = 0 then
      Result.Add(AProjDir);
  finally
    Doc.Free;
  end;
end;

{ Finds all *.ipf files in AModuleDir/src/main/ipf/ (non-recursive) and
  appends their full paths to AFiles. Returns the count found. }
function FindIPFFiles(const AModuleDir: string; AFiles: TStrings): Integer;
var
  IPFDir: string;
  SR: TSearchRec;
begin
  Result := 0;
  IPFDir := AModuleDir + PathDelim + 'src' + PathDelim + 'main' + PathDelim + 'ipf';
  if not DirectoryExists(IPFDir) then
    Exit;
  if FindFirst(IPFDir + PathDelim + '*.ipf', faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Attr and faDirectory) = 0 then
      begin
        AFiles.Add(IPFDir + PathDelim + SR.Name);
        Inc(Result);
      end;
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

procedure Main;
var
  ProjDir, WipfcBin, WipfcDir: string;
  Modules, IPFFiles: TStringList;
  ModDir, IPFFile, OutFile, TargetDir: string;
  TotalFiles, TotalErrors, i, j: Integer;
begin
  gVerbose := GetEnvironmentVariable('PASBUILD_VERBOSE') = '1';
  ProjDir := GetEnvironmentVariable('PASBUILD_PROJECT_DIR');
  if ProjDir = '' then
    ProjDir := GetCurrentDir;

  WipfcBin := FindWipfcBinary(WipfcDir);
  if WipfcBin = '' then
  begin
    Log('ERROR: wipfc binary not found.');
    Log('Set the WIPFC environment variable to the directory containing the wipfc binary.');
    Halt(1);
  end;
  LogVerbose('wipfc binary : ' + WipfcBin);
  LogVerbose('WIPFC res dir: ' + WipfcDir);

  TotalFiles  := 0;
  TotalErrors := 0;
  Modules  := GetModulePaths(ProjDir);
  IPFFiles := TStringList.Create;
  try
    for i := 0 to Modules.Count - 1 do
    begin
      ModDir := Modules[i];
      IPFFiles.Clear;
      if FindIPFFiles(ModDir, IPFFiles) = 0 then
        Continue;

      TargetDir := ModDir + PathDelim + 'target';
      if not ForceDirectories(TargetDir) then
      begin
        Log('ERROR: Cannot create target directory: ' + TargetDir);
        Halt(1);
      end;

      for j := 0 to IPFFiles.Count - 1 do
      begin
        IPFFile := IPFFiles[j];
        OutFile := TargetDir + PathDelim
          + ChangeFileExt(ExtractFileName(IPFFile), '.inf');
        Log('Compiling: ' + ExtractRelativePath(ProjDir + PathDelim, IPFFile));
        Inc(TotalFiles);
        if not RunWipfc(WipfcBin, WipfcDir, IPFFile, OutFile) then
        begin
          Log('ERROR: Failed to compile ' + ExtractFileName(IPFFile));
          Inc(TotalErrors);
        end
        else
          LogVerbose('Output   : ' + ExtractRelativePath(ProjDir + PathDelim, OutFile));
      end;
    end;
  finally
    IPFFiles.Free;
    Modules.Free;
  end;

  if TotalFiles = 0 then
  begin
    Log('No IPF files found.');
    Halt(0);
  end;

  if TotalErrors > 0 then
  begin
    Log(Format('Completed with %d error(s) out of %d file(s).', [TotalErrors, TotalFiles]));
    Halt(1);
  end
  else
    Log(Format('Successfully compiled %d IPF file(s).', [TotalFiles]));
end;

begin
  if (ParamCount >= 1) and (ParamStr(1) = '--pasbuild-phase') then
  begin
    WriteLn(PLUGIN_PHASE);
    Halt(0);
  end;
  Main;
end.
