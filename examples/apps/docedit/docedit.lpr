program docedit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpg_main,
  frm_main, frm_options, model, FPDEUtil, doceditmsg, doceditopts;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  try
    frm := TMainForm.Create(nil);
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.


