program tabsheet_design;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpgfx, tabtest;


procedure MainProc;
var
  frm: TfrmTabTest;
begin
  fpgApplication.Initialize;
  frm := TfrmTabTest.Create(nil);
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


