program demo_adrsbook_mgm;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpgfx, frmmain, frmcontactmaint, frmCityList, frmcitymaint,
  frmcountrylist, frmaddressmaint;



procedure MainProc;
var
  frm: TMainForm;
begin
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
  Randomize;
  MainProc;
end.


