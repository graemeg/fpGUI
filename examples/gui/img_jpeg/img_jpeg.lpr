program jpeg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpg_main, main;

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


