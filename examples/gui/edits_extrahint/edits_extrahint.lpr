program edits_extrahint;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this },
  fpg_main, fpg_stylemanager, fpg_cmdlineparams, mainform, customedits;

procedure MainProc;
var
  frm: TfrmMain;
begin
  fpgApplication.Initialize;

  //if not gCommandLineParams.IsParam('style') then
  //begin
  //  if fpgStyleManager.SetStyle('Plastic Light Gray') then
  //    fpgStyle := fpgStyleManager.Style;
  //end;

  frm := TfrmMain.Create(nil);
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

