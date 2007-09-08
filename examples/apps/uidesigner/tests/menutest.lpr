program menutest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, frm_menutest, fpgfx;



procedure MainProc;
var
  frm: TfrmMain;
begin
  fpgApplication.Initialize;
  frm := TfrmMain.Create(nil);
  frm.Show;
  fpgApplication.Run;
end;

begin
  MainProc;
end.


