program newview_fpgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  {$IFDEF Timing}EpikTimer,{$ENDIF}
  fpg_main, frm_main, DataTypes, HelpFileHeader, HelpWindow,
  IPFEscapeCodes, HelpTopic, CompareWordUnit, SearchTable, TextSearchQuery,
  nvUtilities, nvNullObjects, HelpFile, SearchUnit;


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
  MainProc;
end.

