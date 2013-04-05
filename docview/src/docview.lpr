program docview;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$apptype gui}{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpg_main, frm_main, IPFEscapeCodes, HelpTopic, CompareWordUnit,
  SearchTable, TextSearchQuery, nvUtilities, HelpFile, SearchUnit,
  fpg_cmdlineparams, IPFFileFormatUnit, HelpWindowDimensions, SettingsUnit,
  RichTextStyleUnit, CanvasFontManager, ACLStringUtility, RichTextDocumentUnit,
  RichTextView, RichTextLayoutUnit, RichTextDisplayUnit, dvconstants, dvHelpers,
  frm_configuration, HelpBitmap, frm_text, frm_note, HelpNote, HelpBookmark,
  frm_bookmarks, LZWDecompress;

{$IFDEF WINDOWS}
  {$R docview.rc}
{$ENDIF}

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

