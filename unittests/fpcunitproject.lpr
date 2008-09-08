program fpcunitproject;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpgfx, fpg_guitestrunner, tcTreeview, tiOPFfpGUI;

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
