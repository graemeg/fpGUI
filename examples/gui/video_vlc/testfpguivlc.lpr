program testfpguivlc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Math,
  Classes, frmvlcplayer, fpg_vlc, libvlc, vlc, fpg_main;

procedure MainProc;
var
  frm: TVLCPlayerDemoForm;
begin
  fpgApplication.Initialize;
  frm := TVLCPlayerDemoForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
  frm.Free;
end;


begin
  With TThread.Create(False) do
      Terminate;
  setexceptionmask([exInvalidOp, exDenormalized, exZeroDivide,
                     exOverflow, exUnderflow, exPrecision]);
  MainProc;
end.

