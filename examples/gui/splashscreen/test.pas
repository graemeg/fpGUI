program test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpg_main, fpg_form, frm_main, commands, frm_splashscreen;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  Randomize;
  frm := TMainForm.Create(nil);

  // This is needed otherwise Splashscreen becomes main form. Rules are, the
  // first form displayed is the main form.
  fpgApplication.MainForm := frm;
  
  // Now create and show the splashscreen before the main form.
  frmSplash := TSplashForm.Create(nil);
  frmSplash.Show;

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



