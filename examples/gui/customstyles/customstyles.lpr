program customstyles;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpg_main, frm_test, mystyle;


procedure MainProc;
var
  frm: TTestForm;
begin
  fpgApplication.Initialize;
  frm := TTestForm.Create(nil);
  try
    // Free old and set new style
    if Assigned(fpgStyle) then
      fpgStyle.Free;
    fpgStyle := TMyStyle.Create;
    // now continue with the application
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.

