unit BuilderThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBuilderThread = class(TThread)
  private
    FBuildMode: integer;
  protected
    procedure Execute; override;
  public
    procedure AfterConstruction; override;
    property  BuildMode: integer read FBuildMode write FBuildMode;
  end;

implementation

uses
  project
  ,process
  ,fpg_base
  ,fpg_iniutils
  ,ideconst
  ,idemacros
  ;

{ TBuilderThread }

procedure TBuilderThread.AfterConstruction;
begin
  inherited AfterConstruction;
  FBuildMode := -1;  // signals use of project's default build mode
  FreeOnTerminate := True;
end;

procedure TBuilderThread.Execute;
var
  p: TProcess;
  c: TfpgString;
begin
  p := TProcess.Create(nil);
  p.ShowWindow := swoShowNormal;
  p.CurrentDirectory := GProject.ProjectDir;

  // build compilation string
  c := gINI.ReadString(cEnvironment, 'Compiler', '');
  c := c + GProject.GenerateCmdLine(False, BuildMode);
  c := GMacroList.ExpandMacro(c);

//  AddMessage('Compile command: ' + c);
  p.CommandLine := c;
  p.Execute;
//  AddMessage('Compilation complete');
  p.Free;

end;

end.

