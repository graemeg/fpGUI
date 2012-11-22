program fpcunitproject;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpg_base, fpg_main, fpg_guitestrunner, tcTreeview;

procedure MainProc;

var
  frm: TGUITestRunnerForm;

begin
  fpgApplication.Initialize;
  frm := TGUITestRunnerForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.
