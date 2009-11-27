program docview;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpg_main, frm_main, IPFEscapeCodes, HelpTopic, CompareWordUnit, SearchTable,
  TextSearchQuery, nvUtilities, nvNullObjects, HelpFile, SearchUnit,
  fpg_cmdlineparams, IPFFileFormatUnit, HelpWindowDimensions,
  NewViewConstantsUnit, SettingsUnit, RichTextStyleUnit, CanvasFontManager,
  ACLStringUtility, RichTextDocumentUnit, RichTextView, RichTextLayoutUnit,
  RichTextDisplayUnit, dvconstants, dvHelpers, frm_configuration;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;

  // always load custom style for help viewer
  //if Assigned(fpgStyle) then
  //  fpgStyle.Free;
  //fpgStyle := TMyStyle.Create;

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

