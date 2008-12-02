unit regfpguitestrunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lazideintf, ProjectIntf, Controls, Forms;
  
Type

  { TfpGUITestRunnerProjectDescriptor }
  TfpGUITestRunnerProjectDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject) : TModalResult; override;
    function CreateStartFiles(AProject: TLazProject) : TModalResult; override;
  published
    { Published declarations }
  end;
  
var
  fpGUIFTestRunnerProjectDescriptor : TfpGUITestRunnerProjectDescriptor;
  
procedure Register;

implementation
{$define uselazideintf}
{$ifdef uselazideintf}
uses FPCUnitLazIDEIntf;
{$endif}

procedure Register;

begin
  fpGUIFTestRunnerProjectDescriptor:=TfpGUITestRunnerProjectDescriptor.Create;
  RegisterProjectDescriptor(fpGUIFTestRunnerProjectDescriptor);
end;

{ TfpGUITestRunnerProjectDescriptor }

constructor TfpGUITestRunnerProjectDescriptor.Create;
begin
  inherited Create;
  Name:='FPCUnit fpGUI Application';
end;
    
function TfpGUITestRunnerProjectDescriptor.GetLocalizedName: string;
begin
  Result:='FPGUITestRunner';
end;

function TfpGUITestRunnerProjectDescriptor.GetLocalizedDescription: string;
begin
  Result:='FPCUnit test runner application using a fpGUI front-end';
end;

function TfpGUITestRunnerProjectDescriptor.InitProject(AProject: TLazProject
  ): TModalResult;
  
Var
  L : TStrings;
  MainFile: TLazProjectFile;

begin
  inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('fpcunitproject1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  L:=TStringList.Create;
  try
    With L do
      begin
      Add('program fpcunitproject1;');
      Add('');
      Add('{$mode objfpc}{$H+}');
      Add('');
      Add('uses');
      Add('  {$IFDEF UNIX}{$IFDEF UseCThreads}');
      Add('  cthreads,');
      Add('  {$ENDIF}{$ENDIF}');
      Add('  Classes,');
      Add('  fpg_main, fpg_guitestrunner;');
      Add('');
      Add('procedure MainProc;');
      Add('');
      Add('var');
      Add('  frm: TGUITestRunnerForm;');
      Add('');
      Add('begin');
      Add('  fpgApplication.Initialize;');
      Add('  frm := TGUITestRunnerForm.Create(nil);');
      Add('  try');
      Add('    frm.Show;');
      Add('    fpgApplication.Run;');
      Add('  finally');
      Add('    frm.Free;');
      Add('  end;');
      Add('end;');
      Add('');
      Add('begin');
      Add('  MainProc;');
      Add('end.');
      end;
    AProject.MainFile.SetSourceText(L.text);
  finally
    L.Free;
  end;
  // add dependencies
  AProject.AddPackageDependency('FCL');
  AProject.AddPackageDependency('guitestrunner_fpgui');
  // Don't know if this is needed, actually.
  //  AProject.AddPackageDependency('FPCUnitTestRunner');
  Result:=mrOK;
end;

function TfpGUITestRunnerProjectDescriptor.CreateStartFiles(
  AProject: TLazProject): TModalResult;
begin
{$ifdef uselazideintf}
  LazarusIDE.DoNewEditorFile(FileDescriptorFPCUnitTestCase,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
{$endif}
  Result:=mrOK;
end;

end.

