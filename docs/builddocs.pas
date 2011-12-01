{
  Simple program to buid a documenation script.
  It looks for xml description files. Matches those to source files,
  and includes those matches in the documentation script.

  TODO:
    * Parameter to build script or execute directly fpdoc.
    * Make program build Linux shell scripts and Windows Batch scripts.
    * Add parameter to include all units, not just those that have documentation
    * Order of files are important to fpdoc, so we need some way of indicating the
      order in which files must be processed.
}
program builddocs;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, Process;

const
  cFPDOC = 'fpdoc';
  cFPDocParameters = ' --package=fpgui --format=ipf --output=fpgui.ipf --content=fpgui.cnt --duplinkeddoc ';
  cFilePath = '-Fi../src -Fu../src/corelib/x11/ -Fi../src/corelib/x11/ -Fu../src/gui/ -Fu../src/corelib/ %s';
  cFileLine = '  --input=''%s'' --descr=%s';

type
  TBuildDocsApp = class(TCustomApplication)
  private
    FXMLFiles: TStrings;
    FPasFiles: TStrings;
    FCommand: string;
    FDivider: string;
    procedure   FileSearch(SearchDir: string; ExtensionMask: string; var FileList: TStrings; Recursive: boolean = True);
    function    ExtractFileNameOnly(AFilename: string): string;
    procedure   BuildCommandLine;
    procedure   WriteScript;
    procedure   ExecuteFPDoc;
  protected
    procedure   DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   WriteHelp; virtual;
  end;

{ TBuildDocsApp }

procedure TBuildDocsApp.FileSearch(SearchDir: string; ExtensionMask: string;
  var FileList: TStrings; Recursive: boolean);
var
  Info : TSearchRec;
  ExtensionList: TStrings;
begin
  SearchDir := IncludeTrailingPathDelimiter(SearchDir);

  ExtensionList := TStringList.Create;
  ExtensionList.Delimiter := ';';
  ExtensionList.DelimitedText := ExtensionMask;

  if FindFirst(SearchDir+AllFilesMask, faAnyFile and faDirectory, Info) = 0 then
  begin
    repeat
      if Recursive then
        if ((Info.Attr and faDirectory) = faDirectory) and (Info.Name <> '.') and (Info.Name <> '..')then
          FileSearch(SearchDir + Info.Name, ExtensionMask, FileList, Recursive);

      if ExtensionList.IndexOf(ExtractFileExt(Info.Name)) <> -1 then
      begin
        if FileList.IndexOf(SearchDir + Info.Name) = -1 then
          FileList.Add(SearchDir + Info.Name);
      end;
    until FindNext(Info)<>0;
  end;
  FindClose(Info);

  ExtensionList.Free;
end;

function TBuildDocsApp.ExtractFileNameOnly(AFilename: string): string;
var
  p: integer;
begin
  Result := ExtractFileName(AFilename);
  p := Length(ExtractFileExt(Result));
  Result := Copy(Result, 1, Length(Result)-p);
end;

procedure TBuildDocsApp.BuildCommandLine;
var
  lPFile, lXFile: string;
  i, j: integer;
  lFPDoc: string;
begin
  lFPDoc := GetEnvironmentVariable('fpdoc');
  if lFPDoc = '' then
    lFPDoc := cFPDOC;
  FCommand := lFPDoc + cFPDocParameters;
  FileSearch('xml/', '.xml', FXMLFiles, True);
  FileSearch('../src/', '.pas', FPasFiles, True);

  for i := 0 to FXMLFiles.Count-1 do
  begin
    lXFile := ExtractFileNameOnly(FXMLFiles[i]);
    for j := 0 to FPasFiles.Count-1 do
    begin
      lPFile := ExtractFileNameOnly(FPasFiles[j]);
      if SameText(lXFile, lPFile) then
      begin
      // fix command line separators at the same time
        FCommand := FCommand + FDivider + Format(cFileLine, [SetDirSeparators(Format(cFilePath, [FPasFiles[j]])), FXMLFiles[i]]);
      end;
    end;
  end;

//  FCommand := SetDirSeparators(FCommand);
  {.$IFDEF Windows}
  FCommand := StringReplace(FCommand, '''', '"', [rfReplaceAll]);
  {.$ENDIF}
end;

procedure TBuildDocsApp.WriteScript;
begin
  FXMLFiles.Text := FCommand;
  {$IFDEF Windows}
  FXMLFiles.SaveToFile('runme2.bat');
  {$else}
  FXMLFiles.SaveToFile('runme2.sh');
  {$endif}
end;

procedure TBuildDocsApp.ExecuteFPDoc;
var
  p: TProcess;
begin
  p := TProcess.Create(nil);
  try
//    writeln('------------ START -------------------');
//    writeln(FCommand);
//    writeln('------------ END -------------------');
    p.CommandLine := FCommand;
    p.Options := [poWaitOnExit];
    p.Execute;
  finally
    p.Free;
  end;
end;
procedure TBuildDocsApp.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hs','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('s','script') then
    FDivider := {$ifdef unix}' \' + LineEnding {$else} '' {$endif}
  else
    FDivider := '';

  BuildCommandLine;

  if HasOption('s','script') then
    WriteScript
  else
    ExecuteFPDoc;

  // stop program loop
  Terminate;
end;

constructor TBuildDocsApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FXMLFiles := TStringList.Create;
  FPasFiles := TStringList.Create;
end;

destructor TBuildDocsApp.Destroy;
begin
  FXMLFiles.Free;
  FPasFiles.Free;
  inherited Destroy;
end;

procedure TBuildDocsApp.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
  writeln('');
  writeln('  -h        Show this help');
  writeln('  -s        Generate a script/batch file to run later. Recommended');
  writeln('            for Linux systems.');
  writeln('');
  writeln('If no command line parameters are specified, it will execute FPDoc.');
  writeln('This option is recommended for Windows systems.');
  writeln('');
end;

var
  Application: TBuildDocsApp;

begin
  Application:=TBuildDocsApp.Create(nil);
  Application.Title:='Build Docs App';
  Application.Run;
  Application.Free;
end.

