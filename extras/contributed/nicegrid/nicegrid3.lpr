program nicegrid3;

{$mode objfpc}
{$h+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpg_main, main3;

procedure MainProc;
begin
  fpgApplication.Initialize;
  frmMain := TfrmMain.Create(nil);
  frmMain.Show;
  fpgApplication.Run;
  frmMain.Free;
end;

begin
  MainProc;
end.
