{
  This project is a prototype theme for use in Master Maths (Pty) Ltd.
  Designed by Graeme Geldenhuys.
}

program threedee;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpg_main, frm_threedee;


procedure MainProc;
var
  frm: TfrmMain;
begin
  fpgApplication.Initialize;
  frm := TfrmMain.Create(nil);
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


