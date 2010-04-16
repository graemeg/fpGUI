program fpgide;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpg_base,
  fpg_main,
  frm_main, frm_configureide;


procedure MainProc;
var
  frm: TMainForm;
begin
  FPG_DEFAULT_FONT_DESC := 'DejaVu Sans-9:antialias=true';

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

