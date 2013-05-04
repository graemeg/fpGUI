program nanoedit;

{$mode objfpc}{$H+}
{$ifdef mswindows} {$apptype gui} {$endif}

uses
  Classes, fpg_main, mainfrm;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    frm.Width:=20;
    frm.Height:=20;
    frm.UpdatePosition;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.

