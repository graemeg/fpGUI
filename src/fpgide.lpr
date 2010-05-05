program fpgide;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, fpg_base, fpg_main, frm_main, frm_configureide, ideconst, idemacros, frm_debug,
  project, unitlist, frm_projectoptions, ideutils, builderthread;


procedure MainProc;
var
  frm: TMainForm;
begin
//  FPG_DEFAULT_FONT_DESC := 'DejaVu Sans-9';
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
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

