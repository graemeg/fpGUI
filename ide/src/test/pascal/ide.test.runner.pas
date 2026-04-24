{
    fpGUI IDE - Runner Tests

    Tests for executable path resolution in ide.runner.thread.
}
unit ide.test.runner;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  ide.runner.thread,
  ide.project,
  ide.project.backend,
  ide.project.pasbuild;

type

  { TTestResolveExecutablePath }

  TTestResolveExecutablePath = class(TTestCase)
  protected
    procedure TearDown; override;
  published
    procedure TestNilProjectReturnsEmpty;
    procedure TestLegacyWithTargetFile;
    procedure TestLegacyWithEmptyTargetFile;
    procedure TestPasBuildWithActiveModulePath;
    procedure TestPasBuildDocviewModule;
  end;

implementation

{ TTestResolveExecutablePath }

procedure TTestResolveExecutablePath.TearDown;
begin
  { SetProject frees the previous project, so just reset to a fresh default }
  SetProject(TLegacyProjectBackend.Create);
end;

procedure TTestResolveExecutablePath.TestNilProjectReturnsEmpty;
begin
  SetProject(nil);
  AssertEquals('nil project should return empty', '', ResolveProjectExecutablePath);
end;

procedure TTestResolveExecutablePath.TestLegacyWithTargetFile;
var
  proj: TLegacyProjectBackend;
begin
  proj := TLegacyProjectBackend.Create;
  proj.ProjectDir := '/tmp/myproject/';
  proj.TargetFile := 'myapp';
  SetProject(proj);
  AssertEquals('legacy with target file',
    '/tmp/myproject/myapp', ResolveProjectExecutablePath);
end;

procedure TTestResolveExecutablePath.TestLegacyWithEmptyTargetFile;
var
  proj: TLegacyProjectBackend;
begin
  proj := TLegacyProjectBackend.Create;
  proj.ProjectDir := '/tmp/myproject/';
  proj.TargetFile := '';
  SetProject(proj);
  AssertEquals('legacy with empty target should return empty',
    '', ResolveProjectExecutablePath);
end;

procedure TTestResolveExecutablePath.TestPasBuildWithActiveModulePath;
var
  pb: TPasBuildProjectBackend;
  module: TPasBuildModule;
begin
  pb := TPasBuildProjectBackend.Create;
  module := TPasBuildModule.Create;
  module.ProjectDir := '/data/devel/fpgui/ide';
  module.OutputDir := 'target';
  module.ExecutableName := 'maximus';
  pb.ActiveModule := module;
  SetProject(pb);
  AssertEquals('projectDir + outputDir + executableName',
    '/data/devel/fpgui/ide/target/maximus',
    ResolveProjectExecutablePath);
end;

procedure TTestResolveExecutablePath.TestPasBuildDocviewModule;
var
  pb: TPasBuildProjectBackend;
  module: TPasBuildModule;
begin
  pb := TPasBuildProjectBackend.Create;
  module := TPasBuildModule.Create;
  module.ProjectDir := '/data/devel/fpgui/docview';
  module.OutputDir := 'target';
  module.ExecutableName := 'docview';
  pb.ActiveModule := module;
  SetProject(pb);
  AssertEquals('docview module resolves correctly',
    '/data/devel/fpgui/docview/target/docview',
    ResolveProjectExecutablePath);
end;


initialization
  RegisterTest(TTestResolveExecutablePath);

end.
