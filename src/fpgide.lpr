program fpgide;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpg_base, fpg_main, frm_main, frm_configureide, ideconst, idemacros, frm_debug,
  project, unitlist, frm_projectoptions;


procedure MainProc;
var
  frm: TMainForm;
begin
  FPG_DEFAULT_FONT_DESC := 'DejaVu Sans-9';

  fpgApplication.Initialize;

//  fpgSetNamedFont('Grid', 'DejaVu Sans-8');
//  fpgSetNamedFont('GridHeader', 'DejaVu Sans-8:bold');


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

