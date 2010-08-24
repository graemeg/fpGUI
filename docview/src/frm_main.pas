unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_panel, fpg_tab,
  fpg_tree, fpg_splitter, fpg_menu, fpg_button, fpg_listbox,
  fpg_label, fpg_edit, fpg_radiobutton, fpg_progressbar, fpg_imagelist,
  fpg_imgfmt_bmp,
  HelpFile, RichTextView, HelpTopic;

type
  // Used by Index ListBox. We can generate a custom Index if the help file
  // doesn't contain it's own index entries.
  TListType = ( ltContents, ltIndex );

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    bvlStatusBar: TfpgBevel;
    ProgressBar: TfpgProgressBar;
    lblStatus: TfpgLabel;
    bvlBody: TfpgBevel;
    PageControl1: TfpgPageControl;
    tsContents: TfpgTabSheet;
    tvContents: TfpgTreeView;
    btnGo: TfpgButton;
    tsIndex: TfpgTabSheet;
    btnIndex: TfpgButton;
    lbIndex: TfpgListBox;
    IndexSearchEdit: TfpgEdit;
    tsSearch: TfpgTabSheet;
    Label1: TfpgLabel;
    edSearchText: TfpgEdit;
    Label2: TfpgLabel;
    RadioButton1: TfpgRadioButton;
    RadioButton2: TfpgRadioButton;
    RadioButton3: TfpgRadioButton;
    RadioButton4: TfpgRadioButton;
    RadioButton5: TfpgRadioButton;
    RadioButton6: TfpgRadioButton;
    lbSearchResults: TfpgListBox;
    Label3: TfpgLabel;
    btnSearch: TfpgButton;
    tsNotes: TfpgTabSheet;
    ListBox1: TfpgListBox;
    btnNotesAdd: TfpgButton;
    btnNotesEdit: TfpgButton;
    btnNotesDel: TfpgButton;
    btnNotesGoto: TfpgButton;
    tsHistory: TfpgTabSheet;
    lbHistory: TfpgListBox;
    Splitter1: TfpgSplitter;
    RichView: TRichTextView;
    MainMenu: TfpgMenuBar;
    miFile: TfpgPopupMenu;
    miSettings: TfpgPopupMenu;
    miBookmarks: TfpgPopupMenu;
    miHelp: TfpgPopupMenu;
    miDebug: TfpgPopupMenu;
    ToolBar: TfpgBevel;
    btnOpen: TfpgButton;
    btnBack: TfpgButton;
    btnFwd: TfpgButton;
    btnPrev: TfpgButton;
    btnNext: TfpgButton;
    btnHelp: TfpgButton;
    btnQuit: TfpgButton;
    Bevel1: TfpgBevel;
    Bevel2: TfpgBevel;
    {@VFD_HEAD_END: MainForm}
    miOpenRecentMenu: TfpgPopupMenu;
    miDebugHexInfo: TfpgMenuItem;
//    Files: TList; // current open help files.
    Debug: boolean;
    FFileOpenRecent: TfpgMenuItem;
    FHistorySelection: Boolean;
    FImages: TfpgImageList;

    LoadingFilenameList: TStringList;
    LoadingFileIndex: integer;
    LoadingTotalSize: longint;
    LoadingSizeSoFar: longint;
    AllFilesWordSequences: TList; // of lists; one per open file; of possible word sequences
    CurrentOpenFiles: TList; // current open help files.
    MainTitle: string;
    InIndexSearch: boolean; // true while searching index
    IndexLoaded: boolean;
    ContentsLoaded: boolean;
    DisplayedIndex: TStringList; // duplicate of index listbox, for fast case insensitive searching
    CurrentTopic: TTopic; // so we can get easy access to current topic viewed
    CurrentHistoryIndex: integer;

    procedure   btnBackHistClick(Sender: TObject);
    procedure   btnFwdHistClick(Sender: TObject);
    procedure   btnPrevClick(Sender: TObject);
    procedure   btnNextClick(Sender: TObject);
    procedure   RichViewClickLink(Sender: TRichTextView; Link: string);
    procedure   IndexSearchEditKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure   MainFormShow(Sender: TObject);
    procedure   MainFormDestroy(Sender: TObject);
    procedure   miFileQuitClicked(Sender: TObject);
    procedure   miFileOpenClicked(Sender: TObject);
    procedure   miFileOpenSpecialClicked(Sender: TObject);
    procedure   miFileCloseClicked(Sender: TObject);
    procedure   miConfigureClicked(Sender: TObject);
    procedure   miHelpProdInfoClicked(Sender: TObject);
    procedure   miHelpAboutFPGui(Sender: TObject);
    procedure   miHelpCmdLineParams(Sender: TObject);
    procedure   miHelpUsingDocView(Sender: TObject);
    procedure   miDebugHeader(Sender: TObject);
    procedure   miDebugHex(Sender: TObject);
    procedure   miFileSaveTopicAsIPF(Sender: TObject);
    procedure   OnMRUMenuItemClick(Sender: TObject);
    procedure   btnShowIndex(Sender: TObject);
    procedure   btnGoClicked(Sender: TObject);
    procedure   tvContentsChange(Sender: TObject);
    procedure   edSearchTextKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure   lbSearchResultsKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure   MainFormException(Sender: TObject; E: Exception);
    procedure   MainFormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure   PageControl1Change(Sender: TObject; NewActiveSheet: TfpgTabSheet);
    procedure   tvContentsDoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   lbIndexDoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   lbIndexKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure   lbSearchResultsDoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   lbHistoryDoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   lbHistoryKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure   btnSearchClicked(Sender: TObject);
    procedure   IndexSearchEditOnChange(Sender: TObject);
    procedure   DisplaySelectedSearchResultTopic;
    procedure   NavigateToHistoryIndex(AIndex: integer);
    procedure   UpdateLocationPanel;
    procedure   EnableControls;
    procedure   ClearAllWordSequences;
    procedure   DoSearch;
    procedure   SetWaitCursor;
    procedure   ClearWaitCursor;
    procedure   SetMainCaption;
    procedure   DisplayFiles(NewFiles: TList; var FirstContentsNode: TfpgTreeNode);
    procedure   FileOpen;
    function    LoadFiles(const aFileNames: TStrings; aHelpFiles: TList): boolean;
    function    OpenFiles(const FileNames: TStrings; const AWindowTitle: string; const DisplayFirstTopic: boolean): boolean;
    function    OpenFile(const AFileName: string; const AWindowTitle: string; const DisplayFirstTopic: boolean): boolean;
    procedure   CloseFile(const ADestroying: boolean = False);
    procedure   OnHelpFileLoadProgress(n, outof: integer; AMessage: string);
    procedure   LoadNotes(AHelpFile: THelpFile);
    procedure   LoadContents(AFiles: TList; var FirstNode: TfpgTreeNode);
    procedure   LoadIndex;
    // Used in loading contents
    procedure   AddChildNodes(AHelpFile: THelpFile; AParentNode: TfpgTreeNode; ALevel: longint; var ATopicIndex: longint );
    procedure   ClearNotes;
    procedure   ClearIndexComponents;
    procedure   SaveNotes(AHelpFile: THelpFile);
    procedure   DisplayTopic(ATopic: TTopic = nil);
    procedure   ResetProgress;
    procedure   SetStatus(const AText: TfpgString);
    function    TranslateEnvironmentVar(AFilenames: TfpgString): TfpgString;
    procedure   RefreshFontSubstitutions;
    procedure   DisplaySelectedContentsTopic;
    procedure   DisplaySelectedIndexTopic;
    procedure   ProcessCommandLineParams;
    procedure   SaveNavigatePoint;
    procedure   ShowCmdLineParamHelp;
    function    FindTopicForLink( Link: THelpLink ): TTopic;
    function    FindTopicByResourceID( ID: word ): TTopic;
    function    FindTopicByName(const AName: string): TTopic;
    procedure   AddCurrentToMRUFiles;
    procedure   CreateMRUMenuItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}


implementation

uses
  fpg_dialogs
  ,fpg_constants
  ,fpg_iniutils
  ,fpg_cmdlineparams
  ,fpg_utils
  ,fpg_stringutils
  ,nvUtilities
  ,ACLStringUtility
  ,TextSearchQuery
  ,SearchUnit
  ,dvconstants
  ,IPFFileFormatUnit
  ,SettingsUnit
  ,dvHelpers
  ,frm_configuration
  ;

const
  cLongName   = 'fpGUI Documentation Viewer';
  cCreatedBy  = 'Created by Graeme Geldenhuys';
  cVersion    = 'Version ' + FPGUI_VERSION;

{$I arrows.inc}
{$I missing.inc}

{@VFD_NEWFORM_IMPL}

procedure TMainForm.MainFormException(Sender: TObject; E: Exception);
begin
  TfpgMessageDialog.Critical('An unexpected error occurred.', E.Message);
end;

procedure TMainForm.lbIndexKeyPress(Sender: TObject; var KeyCode: word;
    var ShiftState: TShiftState; var Consumed: boolean);
begin
  if (KeyCode = keyReturn) or (KeyCode = keyPEnter) then
  begin
    Consumed := True;
    DisplayTopic(nil);
  end
end;

procedure TMainForm.btnBackHistClick(Sender: TObject);
begin
  if CurrentHistoryIndex > 0 then
  begin
    NavigateToHistoryIndex(CurrentHistoryIndex - 1);
    //lbHistory.FocusItem := CurrentHistoryIndex - 1;
    //DisplayTopic(TTopic(lbHistory.Items.Objects[lbHistory.FocusItem]));
  end;
end;

procedure TMainForm.btnFwdHistClick(Sender: TObject);
begin
  if CurrentHistoryIndex < lbHistory.Items.Count-1 then
  begin
    NavigateToHistoryIndex(CurrentHistoryIndex + 1);
    //lbHistory.FocusItem := CurrentHistoryIndex + 1;
    //DisplayTopic(TTopic(lbHistory.Items.Objects[lbHistory.FocusItem]));
  end;
end;

procedure TMainForm.btnPrevClick(Sender: TObject);
begin
  if CurrentOpenFiles.Count > 0 then
  begin
    tvContents.GotoNextNodeUp;
    tvContents.SetFocus;
    DisplaySelectedContentsTopic;
  end;
end;

procedure TMainForm.btnNextClick(Sender: TObject);
begin
  if CurrentOpenFiles.Count > 0 then
  begin
    tvContents.GotoNextNodeDown;
    tvContents.SetFocus;
    DisplaySelectedContentsTopic;
  end;
end;

procedure TMainForm.RichViewClickLink(Sender: TRichTextView; Link: string);
var
  LinkIndex: integer;
  lLink: THelpLink;
  lHelp: THelpFile;
  i: integer;
  lTopic: TTopic;
  lFound: Boolean;
  lURL: TfpgString;
begin
  // TODO: process other types of links (external, application etc...) too!
  if pos('external', Link) > 0 then
  begin
    TfpgMessageDialog.Warning('', 'External links are not supported in DocView yet. Please try again with a later build.')
  end
  else if pos('url', Link) > 0 then
  begin
    // we have a external URL of some kind
    // format is always:  'url "<uri>"'
//    ShowMessage('Found an external Link' + LineEnding + Link);
    lURL := StringReplace(Link, 'url "', '', []);
    lURL := UTF8Copy(lURL, 0, UTF8Length(lURL)-1);
    fpgOpenURL(lURL);
  end
  else
  begin
    // we have a internal INF file link
    LinkIndex := StrToInt( Link );
    lLink := THelpLink(CurrentTopic.Links[LinkIndex]);
    lTopic := FindTopicForLink(lLink);
    if lTopic <> nil then
      DisplayTopic(lTopic);
    exit;

    lHelp := THelpFile(lLink.HelpFile);
    lTopic := nil;
    lFound := False;
    for i := 0 to CurrentOpenFiles.Count-1 do
    begin
      lHelp := THelpFile(CurrentOpenFiles[i]);
      lTopic := lHelp.Topics[LinkIndex];
      if lTopic <> nil then
      begin
        lFound := True;
//        writeln('Found Topic! ', lTopic.Title);
        break;
      end;
      if lFound then
        break;
    end;
    if lTopic <> nil then
    begin
//      writeln('Displaying topic <', lTopic.Title, '>');
      DisplayTopic(lTopic);
    end;
  end;
end;

procedure TMainForm.IndexSearchEditKeyPress(Sender: TObject; var KeyCode: word;
  var ShiftState: TShiftState; var Consumed: boolean);
begin
  if CurrentOpenFiles.Count = 0 then
    Exit;

  if (KeyCode = keyEnter) or (KeyCode = keyPEnter) then
  begin
    Consumed := True;
    DisplaySelectedIndexTopic;
  end;
end;

procedure TMainForm.MainFormShow(Sender: TObject);
begin
  bvlBody.Realign;

  // restore previous window position and size
  gINI.ReadFormState(self);
  PageControl1.Width := gINI.ReadInteger('Options', 'SplitterLeft', 260);
  UpdateWindowPosition;

  Settings.NormalFont := fpgStyle.DefaultFont;
  Settings.FixedFont := fpgStyle.FixedFont;
  Settings.SearchDirectories := TStringList.Create;

  LogEvent(LogSettings, 'Loading settings');
  LoadSettings;
  CreateMRUMenuItems;
  ProcessCommandLineParams;

  RichView.Images := FImages;
end;

procedure TMainForm.MainFormDestroy(Sender: TObject);
begin
  ClearAllWordSequences;
  DisplayedIndex.Free;
  // save splitter position
  gINI.WriteInteger('Options', 'SplitterLeft', PageControl1.Width);
  // save form size and position
  gINI.WriteFormState(self);
  LogEvent(LogSettings, 'Save settings');
  SaveSettings;
  LogEvent(LogSettings, 'Save settings done');
end;

procedure TMainForm.miFileQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.miFileOpenClicked(Sender: TObject);
begin
  FileOpen;
end;

procedure TMainForm.miFileOpenSpecialClicked(Sender: TObject);
var
  s: TfpgString;
begin
  if fpgInputQuery('Open Special...', 'Enter environment variable to open', s) then
  begin
    if s <> '' then
      OpenFile(s, '', True);
  end;
end;

procedure TMainForm.miFileCloseClicked(Sender: TObject);
begin
  CloseFile;
end;

procedure TMainForm.miConfigureClicked(Sender: TObject);
begin
  ShowConfigForm;
end;

procedure TMainForm.miHelpProdInfoClicked(Sender: TObject);
var
  s: TfpgString;
begin
  s :=  cLongName + LineEnding + LineEnding
      + cCreatedBy + LineEnding
      + cVersion + '  -  '+  {$I %date%} + ' ' + {$I %time%};

  TfpgMessageDialog.Information('Product Information', s);
end;

procedure TMainForm.miHelpAboutFPGui(Sender: TObject);
begin
  TfpgMessageDialog.AboutFPGui;
end;

procedure TMainForm.miHelpCmdLineParams(Sender: TObject);
begin
  CloseFile(False);
  ShowCmdLineParamHelp;
end;

procedure TMainForm.miHelpUsingDocView(Sender: TObject);
begin
  CloseFile(False);
  OpenFile(OWN_HELP_MARKER, '', True);
end;

procedure TMainForm.miDebugHeader(Sender: TObject);
var
  f: THelpFile;
  i: integer;
  sl: TStringList;
begin
  RichView.Clear;
  sl := TStringList.Create;
  RichView.AddText(PChar('FileCount = ' + IntToStr(CurrentOpenFiles.Count)));
  for i := 0 to CurrentOpenFiles.Count-1 do
  begin
    f := THelpFile(CurrentOpenFiles[i]);
    sl.Clear;
    with sl do
    begin
      Add('<b><u>Filename:</u></b> <red>' + f.Filename + '<black>');
      Add('<b>Title:</b> ' + f.Title);
      Add('<b>File size:</b> ' + IntToStr(fpgFileSize(f.Filename)) + 'bytes');
      Add('<b>Dictionary count:</b> ' + IntToStr(f.DictionaryCount));
      Add('<b>Topic count:</b> ' + IntToStr(f.TopicCount));
      Add('<b>Index count:</b> ' + IntToStr(f.Index.Count));
      Add('<b>String resource id count:</b> ' + IntToStr(f.StringResourceIDCount));
      Add('<b>Numeric resource id count:</b> ' + IntToStr(f.NumericResourceIDCount));
      Add(' ');
      //Add('Dictionary contents:');
      //for i := 0 to f.DictionaryCount-1 do
      //  Add('[' + IntToStr(i) + '] = <' + f.DictionaryWords[i] + '>');
    end;
    RichView.AddParagraph(PChar(sl.Text));
  end;
  sl.Free;
end;

procedure TMainForm.miDebugHex(Sender: TObject);
begin
  Debug := not Debug;
  miDebugHexInfo.Checked := Debug;
  DisplayTopic(nil);
end;

procedure TMainForm.miFileSaveTopicAsIPF(Sender: TObject);
var
  F: TextFile;
  T: TTopic;
  dlg: TfpgFileDialog;
  H: THelpFile;
  i: integer;
  filename: string;
  imglist: TList;
begin
(*
  H := THelpFile(CurrentOpenFiles[0]);

  FileName := ChangeFileExt( ExtractFileName( H.Filename ), '.ipf' );
  if not DoSaveFileDialog( FileSaveTitle,
                       'IPF' + '|*.ipf',
                       Filename,
                       Settings.LastSaveDirectory,
                       Filename ) then
    exit;
  if FileExists( Filename ) then
    if not DoConfirmDlg( FileSaveTitle,
                         ReplaceFilePromptA
                         + Filename
                         + ReplaceFilePromptB ) then
      exit;

  ImageOffsets := TList.Create;

  AssignFile( F, FileName );
  Rewrite( F );
  WriteLn( F, ':userdoc.' );

  // We can't tell if some levels of the contents were
  // merged into the text of topics. So we just assume all are visible
  WriteLn( F, ':docprof toc=123456.' );

  ResourceIDs := TList.Create;

  WriteLn( F, ':title.' + H.Title );

  for i := 0 to H.TopicCount - 1 do
  begin
    T := H.Topics[ i ];

    SetProgress( i div 2, H.TopicCount , 'Saving text...' );

    WriteLn( F, '' );


    if T.ContentsLevel = 0 then
    begin
      // perhaps it means footnote?
      //      Level := 1;
      Write( F, ':fn id=fn' + IntToStr( i ) + '.' ); // use index as id

      T.SaveToIPF( F, ImageOffsets );

      WriteLn( F, '' );
      WriteLn( F, ':efn.' );
    end
    else
    begin
      Write( F, ':h' + IntToStr( T.ContentsLevel ) );
      Write( F, ' id=' + IntToStr( i ) ); // use index as id

      H.FindResourceIDsForTopic( T, ResourceIDs );
      if ResourceIDs.Count > 0 then
      begin
        Write( F, ' res=' + IntToStr( longint( ResourceIDs[ 0 ] ) ) );
      end;

      if not T.ShowInContents then
        Write( F, ' hide' );

      if T.ContentsGroupIndex > 0 then
        Write( F, ' group=' + IntToStr( T.ContentsGroupIndex ) );

      Write( F, '.' ); // end of header
      WriteLn( F, T.Title );

      T.SaveToIPF( F, ImageOffsets );
    end;



  end;

  ResourceIDs.Destroy;

  WriteLn( F, ':euserdoc.' );
  System.Close( F );

  // Now write images

  for i := 0 to ImageOffsets.Count - 1 do
  begin
    ImageOffset := longint( ImageOffsets[ i ] );

    SetProgress( i div 2 + ImageOffsets.Count div 2,
                 ImageOffsets.Count ,
                 'Saving images...' );

    Image := H.GetImage( ImageOffset );

    if Image <> nil then
    begin
      Image.SaveToFile( ExtractFilePath( Filename )
                        + 'img'
                        + IntToStr( i )
                        + '.bmp' );
      Image.Destroy;
    end;

  end;

  ResetProgress;
  SetStatus( 'Save complete' );
  ImageOffsets.Destroy;
*)



  //-----------------------------
  if tvContents.Selection = nil then
    Exit; //-->

  T := TTopic(tvContents.Selection.Data);
  if T <> nil then
  begin
    dlg := TfpgFileDialog.Create(nil);
    try
      dlg.FileName := T.Title + '.ipf';
      if dlg.RunSaveFile then
      begin
        imglist := TList.Create;
        AssignFile( F, dlg.FileName );
        Rewrite( F );
        T.SaveToIPF(F, imglist);
        System.Close(F);
        imglist.free;
        dlg.Close;
        dlg.Free;
      end;
    finally
    end;
  end;
end;

procedure TMainForm.OnMRUMenuItemClick(Sender: TObject);
var
  lTag: longint;
  MenuItem: TfpgMenuItem;
  MRUItem: TMRUItem;
begin
  MenuItem := Sender as TfpgMenuItem;
  lTag := MenuItem.Tag;
  MRUItem := TMRUItem(Settings.MRUList[ lTag ]);
  OpenFiles(MRUItem.FileNames, '', True);
end;

procedure TMainForm.btnShowIndex(Sender: TObject);
begin
  DisplaySelectedIndexTopic;
end;

procedure TMainForm.btnGoClicked(Sender: TObject);
begin
  if tvContents.Selection <> nil then
    DisplayTopic(nil);
end;

procedure TMainForm.tvContentsChange(Sender: TObject);
begin
  if CurrentOpenFiles.Count = 0 then
    Exit;

  DisplayTopic(nil);
end;

procedure TMainForm.edSearchTextKeyPress(Sender: TObject; var KeyCode: word;
  var ShiftState: TShiftState; var Consumed: boolean);
begin
  if CurrentOpenFiles.Count = 0 then
    Exit;

  if (KeyCode = keyReturn) or (KeyCode = keyPEnter) then
  begin
    Consumed := True;
    btnSearch.Click;
  end
  else if (KeyCode = keyDown) then
  begin
    Consumed := True;
    lbSearchResults.SetFocus;
  end;
end;

procedure TMainForm.lbSearchResultsKeyPress(Sender: TObject; var KeyCode: word;
  var ShiftState: TShiftState; var Consumed: boolean);
begin
  if CurrentOpenFiles.Count = 0 then
    Exit;

  if (KeyCode = keyReturn) or (KeyCode = keyPEnter) then
  begin
    Consumed := True;
    DisplayTopic(nil);
  end
end;

procedure TMainForm.MainFormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CloseFile(True);
end;

procedure TMainForm.PageControl1Change(Sender: TObject; NewActiveSheet: TfpgTabSheet);
begin
  if NewActiveSheet = tsIndex then
  begin
    if not IndexLoaded then
      LoadIndex;
    IndexSearchEdit.SetFocus;
  end
  else if NewActiveSheet = tsHistory then
    lbHistory.FocusItem := CurrentHistoryIndex;
end;

procedure TMainForm.tvContentsDoubleClick(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  if CurrentOpenFiles.Count = 0 then
    Exit;

  if tvContents.Selection <> nil then
    DisplayTopic(nil);
end;

procedure TMainForm.lbIndexDoubleClick(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  InIndexSearch := True;  // prevent edit.OnChange from executing too
  try
    IndexSearchEdit.Text := lbIndex.Items[lbIndex.FocusItem];
    DisplayTopic(nil);
  finally
    InIndexSearch := False;
  end;
end;

procedure TMainForm.lbSearchResultsDoubleClick(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  DisplaySelectedSearchResultTopic;
end;

procedure TMainForm.lbHistoryDoubleClick(Sender: TObject;
  AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  try
    FHistorySelection := True;
    DisplayTopic(nil);
    CurrentHistoryIndex := lbHistory.FocusItem;
  finally
    FHistorySelection := False;
  end;
end;

procedure TMainForm.lbHistoryKeyPress(Sender: TObject; var KeyCode: word;
  var ShiftState: TShiftState; var Consumed: boolean);
begin
  if CurrentOpenFiles.Count = 0 then
    Exit;

  if (KeyCode = keyReturn) or (KeyCode = keyPEnter) then
  begin
    Consumed := True;
    try
      FHistorySelection := True;
      DisplayTopic(nil);
      CurrentHistoryIndex := lbHistory.FocusItem;
    finally
      FHistorySelection := False;
    end;
  end
end;

procedure TMainForm.btnSearchClicked(Sender: TObject);
begin
  DoSearch;
end;

procedure TMainForm.IndexSearchEditOnChange(Sender: TObject);
var
  tmpMatchIndex: longint;
  tmpSearchText: string;
  i: longint;
begin
  if InIndexSearch then
    exit;

  tmpMatchIndex := -1;
  tmpSearchText := trim(IndexSearchEdit.Text);

  for i := 0 to DisplayedIndex.Count - 1 do
  begin
    if StrStartsWithIgnoringCase(DisplayedIndex[i], tmpSearchText) then
    begin
      tmpMatchIndex := i;
      break;
    end;
  end;

  if tmpMatchIndex = -1 then
    exit;

  InIndexSearch:= true;


  if lbIndex.FocusItem <> tmpMatchIndex then
    lbIndex.FocusItem := tmpMatchIndex;

  InIndexSearch:= false;
end;

procedure TMainForm.DisplaySelectedSearchResultTopic;
var
  Topic: TTopic;
begin
  if lbSearchResults.FocusItem = -1 then
    exit;
  if lbSearchResults.Items.Objects[lbSearchResults.FocusItem] = nil then
    // the "no results" place holder
    exit;
  Topic := lbSearchResults.Items.Objects[lbSearchResults.FocusItem] as TTopic;
  DisplayTopic( Topic );
end;

procedure TMainForm.NavigateToHistoryIndex(AIndex: integer);
begin
  try
    FHistorySelection := True;
    CurrentHistoryIndex := AIndex;
    lbHistory.FocusItem := AIndex;
    DisplayTopic(TTopic(lbHistory.Items.Objects[AIndex]));
  finally
    FHistorySelection := False;
  end;
end;

procedure TMainForm.UpdateLocationPanel;
var
  i: integer;
  s: string;
  n: TfpgTreeNode;
  sep: string;
begin
  s := '';
  sep := '';
  n := tvContents.Selection;
  while n.Parent <> nil do
  begin
    s := n.Parent.Text + sep + s;
    n := n.Parent;
    sep := ' > ';
  end;
  SetStatus(s);
end;

procedure TMainForm.EnableControls;
begin
  //
end;

procedure TMainForm.ClearAllWordSequences;
var
  i: longint;
  FileWordSequences: TList;
  HelpFile: THelpFile;
begin
  if AllFilesWordSequences = nil then
    exit;

  for i := 0 to AllFilesWordSequences.Count - 1 do
  begin
    FileWordSequences := TList(AllFilesWordSequences[ i ]);
    HelpFile := THelpFile(CurrentOpenFiles[ i ]);
    ClearWordSequences( FileWordSequences, HelpFile.DictionaryCount );
    FileWordSequences.Free;
  end;
  AllFilesWordSequences.Clear;
end;

procedure TMainForm.DoSearch;
var
  SearchResults: TList;
  SearchText: string;
  FileIndex: longint;
  HelpFile: THelpFile;
  TopicIndex: longint;
  Topic: TTopic;
  FileWordSequences: TList;
  Query: TTextSearchQuery;
begin
  if CurrentOpenFiles.Count = 0 then
    Exit;

  SearchText := Trim(edSearchText.Text);
  lbSearchResults.Items.Clear;

  if SearchText = '' then
    exit;

  lbSearchResults.Items.Add(rsDVSearchingMsg);
  SetStatus(rsDVSearchingMsg);

  try
    Query := TTextSearchQuery.Create( SearchText );
  except
    on e: ESearchSyntaxError do
    begin
      TfpgMessageDialog.Critical( rsSearch,  rsDVSearchSyntaxError + e.Message );
      exit;
    end;
  end;

  ClearAllWordSequences;

  SetWaitCursor;

  SearchResults := TList.Create;

  // Search open help file
  for FileIndex := 0 to CurrentOpenFiles.Count - 1 do
  begin
    HelpFile := THelpFile(CurrentOpenFiles[ FileIndex ]);
    FileWordSequences := TList.Create;
    try
      SearchHelpFile( HelpFile,
                      Query,
                      SearchResults,
                      FileWordSequences );
    except
      on E: EHelpFileException do
      begin
        TfpgMessageDialog.Critical(rsError , E.Message);
        Query.Destroy;
        ClearWaitCursor;
        exit;
      end;
    end;

    AllFilesWordSequences.Add( FileWordSequences );
  end;

  // Sort results across all files by relevance
  SearchResults.Sort( @TopicRelevanceCompare );

  // Load topics into search results list.
  lbSearchResults.Items.BeginUpdate;
  lbSearchResults.Items.Clear;

  for TopicIndex := 0 to SearchResults.Count - 1 do
  begin
    Topic := TTopic(SearchResults[ TopicIndex ]);
    lbSearchResults.Items.AddObject( Topic.Title
                                          + ' ['
                                          + IntToStr( Topic.SearchRelevance )
                                          + ']',
                                          Topic );
  end;

  EnableControls;
//  if lbSearchResults.Items.Count > 0 then
    // there are some search matches, so highlight words
//    ViewHighlightSearchWordsMI.Checked := true;

  lbSearchResults.FocusItem := -1;
  lbSearchResults.Items.EndUpdate;

  Query.Free;
  SearchResults.Free;

  if lbSearchResults.Items.Count > 0 then
  begin
    lbSearchResults.FocusItem := 0;
  end
  else
  begin
    lbSearchResults.Items.Add( Format(rsDVNoMatchesFound, [SearchText]));
//    RefreshWindows( Windows ); // update to remove old highlights
  end;
  SetStatus( Format(rsDVSearchFoundMsg, [lbSearchResults.Items.Count])
      + StrInDoubleQuotes(SearchText));

  ClearWaitCursor;
  DisplaySelectedSearchResultTopic;
end;

procedure TMainForm.SetWaitCursor;
begin
  //
end;

procedure TMainForm.ClearWaitCursor;
begin
  //
end;

procedure TMainForm.SetMainCaption;
begin
  WindowTitle:= MainTitle;
  fpgApplication.ProcessMessages;
end;

procedure TMainForm.DisplayFiles(NewFiles: TList; var FirstContentsNode: TfpgTreeNode);
var
  HelpFile: THelpFile;
  FileIndex: longint;
begin
  LogEvent(LogStartup, 'DisplayFiles' );
  // Now load the various parts of the file(s)
  // into the user interface
  ProgressBar.Position := 52;
  SetStatus( rsDVDisplaying );

  // Add our open files in the global filelist
  { TODO -ograeme : implement global filelist support }
  //for FileIndex := 0 to NewFiles.Count - 1 do
  //begin
  //  HelpFile := NewFiles[ FileIndex ];
  //  GlobalFilelist.AddFile( HelpFile.Filename, Frame.Handle );
  //  // LoadNotes( HelpFile );
  //  LoadBookmarks( HelpFile );
  //end;

  { TODO -ograeme : update notes display }
  //UpdateNotesDisplay;

  { TODO -ograeme : bookmarks }
  //BuildBookmarksMenu;
  //UpdateBookmarksForm;

  ProgressBar.Position := 55;

  ContentsLoaded := false;
  IndexLoaded := false;

  LoadContents( NewFiles, FirstContentsNode );

  ProgressBar.Position := 75;

  LoadIndex;

  ProgressBar.Position := 100;
  SetStatus( rsDVDone );

  LogEvent(LogStartup, 'DisplayFiles Done' );
end;

procedure TMainForm.FileOpen;
var
  dlg: TfpgFileDialog;
begin
  dlg := TfpgFileDialog.Create(nil);
  try
    dlg.InitialDir := Settings.LastOpenDirectory;
    dlg.WindowTitle := rsDVOpenHelpFile;
    dlg.Filter := rsDVHelpFiles + ' (*.hlp, *.inf)|*.inf;*.hlp ';
    // and a catch all filter
    dlg.Filter := dlg.Filter + '|(' + rsAllFiles + ' (*)|*';

    if dlg.RunOpenFile then
    begin
      Settings.LastOpenDirectory := fpgExtractFilePath(dlg.Filename);
      OpenFile(dlg.Filename, '', true);
    end;
    { TODO -oGraeme : Add support for multiple files. }
  finally
    dlg.Free;
  end;
end;

function TMainForm.LoadFiles(const aFileNames: TStrings; aHelpFiles: TList): boolean;
var
  HelpFile: THelpFile;
  FileIndex, i: longint;
  FileName: string;
  FullFilePath: string;
begin
  LogEvent(LogStartup, 'LoadFiles' );
  LoadingFilenameList := TStringList.Create;

// RBRi  TranslateIPFEnvironmentVars( FileNames, LoadingFilenameList );
  for i := 0 to aFileNames.Count - 1 do
    LoadingFilenameList.Add(aFileNames[i]);

  LogEvent(LogStartup, 'Finding files' );
  ProgressBar.Visible := True;

  // now find full file paths,
  // and also the total file size for progress display
  for FileIndex := 0 to LoadingFilenameList.Count - 1 do
  begin
    FileName := LoadingFilenameList[ FileIndex ];
    LogEvent(LogStartup, '  File: ' + FileName );

    // Find the help file, if possible
    if Filename = OWN_HELP_MARKER then
    begin
      FullFilePath := GetOwnHelpFileName;
    end
    else
    begin
      FullFilePath := FindHelpFile( Filename );
    end;

    if FullFilePath <> '' then
    begin
      LogEvent(LogStartup, '    Full path: ' + FullFilePath );
    end
    else
    begin
      LogEvent(LogStartup, '    File not found' );
      TfpgMessageDialog.Critical('Error', 'Failed to find the help file: ' + Filename);
      Abort;
    end;
    LoadingFilenameList[ FileIndex ] := FullFilePath;
  end;

  // Now actually load the files
  for FileIndex := 0 to LoadingFilenameList.Count - 1 do
  begin
    Filename := LoadingFilenameList[ FileIndex ];
    LogEvent(LogStartup, '  Loading: ' + Filename );
    try
      LoadingFileIndex := FileIndex;

      // load the file
      HelpFile := THelpFile.Create( FileName );
      if Settings.FixedFontSubstitution then
         HelpFile.SetupFontSubstitutes( Settings.FixedFontSubstitutes );

      aHelpFiles.Add( HelpFile );

    except
      on E: Exception do
      begin

        if E is EWindowsHelpFormatException then
        begin
          { TODO -ograeme -cnice to have : Implement opening Windows help }
          //OpenWindowsHelp( Filename );
        end
        else
        begin
          TfpgMessageDialog.Critical( rsDVOpenHelpFile,
                      Format(rsDVCouldNotOpen, [Filename])
                      + ': '
                      + E.Message );
        end;

        // back out of the load process
        Result := false;

        DestroyListObjects( aHelpFiles );

        LoadingFilenameList.Free;
        ResetProgress;
        exit;
      end
    end;
  end;

  LoadingFilenameList.Free;
  Result := true;
end;

{ Open the file or list of files in FileNames
  Set the window title if given, otherwise get it from first file }
function TMainForm.OpenFiles(const FileNames: TStrings;
  const AWindowTitle: string; const DisplayFirstTopic: boolean): boolean;
var
  tmpHelpFiles: TList;
  FirstContentsNode: TfpgTreeNode;
begin
  LogEvent(LogStartup, 'OpenFiles' );

  //if not OKToCloseFile then
  //  exit;

  SetWaitCursor;
  tmpHelpFiles := TList.Create;

  if not LoadFiles(FileNames, tmpHelpFiles) then
  begin
    ClearWaitCursor;
    tmpHelpFiles.Free;
    exit;
  end;

  Result := true;

  lbSearchResults.Items.Clear;
  lbHistory.Items.Clear;
  CurrentHistoryIndex := -1;

  // Now that we have successfully loaded the new help file(s)
  // close the existing one.
  CloseFile;

  AssignList(tmpHelpFiles, CurrentOpenFiles );

  if CurrentOpenFiles.Count = 0 then
   exit; // no help files found - nothing further to do

  ProgressBar.Position := 50;
  SetStatus( rsDVDisplaying );

  AddCurrentToMRUFiles;

  if AWindowTitle = '' then
    MainTitle := THelpFile( CurrentOpenFiles[ 0 ] ).Title
  else
    MainTitle := AWindowTitle;
  SetMainCaption;

  // Now load the various parts of the file(s)
  // into the user interface
  DisplayFiles( tmpHelpFiles, FirstContentsNode );

  //if CmdLineParameters.getHelpManagerFlag then
  //  ShowLeftPanel := Settings.ShowLeftPanel_Help
  //else
  //  ShowLeftPanel := Settings.ShowLeftPanel_Standalone;

  // Select first contents node if there is one
  if FirstContentsNode <> nil then
  begin
    LogEvent(LogStartup, '  Select first node' );
    tvContents.Selection := FirstContentsNode;
    tvContents.SetFocus;
  end;

  ClearWaitCursor;

  ResetProgress;

//  NotebookOnPageChanged( self ); // ensure e.g. index loaded

  EnableControls;

  if DisplayFirstTopic then
  begin
    LogEvent(LogStartup, 'Display first topic' );
    DisplaySelectedContentsTopic;
//    DisplayTopic(nil);
  end;

  LogEvent(LogStartup, 'OpenFiles complete' );
end;

{ Open a single file }
function TMainForm.OpenFile(const AFileName: string; const AWindowTitle: string;
    const DisplayFirstTopic: boolean): boolean;
var
  tmpFileNames: TStringList;
begin
  tmpFileNames := TStringList.Create;
  ParseAndExpandFileNames(AFileName, tmpFileNames);
  Result := OpenFiles( tmpFileNames, AWindowTitle, DisplayFirstTopic );
  tmpFileNames.Free;
end;

procedure TMainForm.CloseFile(const ADestroying: boolean = False);
var
  FileIndex: longint;
  lHelpFile: THelpFile;
begin
  tvContents.Selection := nil;
  tvContents.RootNode.Clear;

  lbSearchResults.Items.Clear;
  edSearchText.Clear;
  lbHistory.Items.Clear;

  RichView.Clear(ADestroying);
  if not ADestroying then
  begin
    WindowTitle := rsDVTitle + ' - ' + rsDVNoFile;
    tvContents.Invalidate;
  end;

  // First save notes. It's important we do this first
  // since we scan all notes each time to find the ones
  // belonging to this file.
  for FileIndex := 0 to CurrentOpenFiles.Count - 1 do
  begin
    lHelpFile := THelpFile(CurrentOpenFiles[FileIndex]);
    SaveNotes( lHelpFile );
  end;

  ClearIndexComponents;
  ClearAllWordSequences;

  // Now destroy help files
  for FileIndex := 0 to CurrentOpenFiles.Count - 1 do
  begin
    lHelpFile := THelpFile(CurrentOpenFiles[FileIndex]);
    lHelpFile.Free;
  end;

  CurrentOpenFiles.Clear;
  ClearNotes;
end;

procedure TMainForm.OnHelpFileLoadProgress(n, outof: integer; AMessage: string);
begin
  //
end;

procedure TMainForm.LoadNotes(AHelpFile: THelpFile);
begin
//  NotesFileName:= ChangeFileExt( HelpFile.FileName, '.nte' );

end;

procedure TMainForm.LoadContents(AFiles: TList; var FirstNode: TfpgTreeNode);
var
  FileIndex: integer;
  HelpFile: THelpFile;
  TopicIndex: integer;
  Node: TfpgTreeNode;
  Topic: TTopic;
begin
  LogEvent(LogStartup, 'Load contents outline');
  // we don't clear treeview here in case we need to load more files later.
  LogEvent(LogStartup, 'Loop files');

  FirstNode := nil;
  Node := nil;

  for FileIndex:= 0 to AFiles.Count - 1 do
  begin
    HelpFile:= THelpFile(AFiles[ FileIndex ]);
    ProfileEvent( 'File ' + IntToStr( FileIndex ) );
    TopicIndex:= 0;
    ProfileEvent('TopicCount=' + IntToStr(HelpFile.TopicCount));
    while TopicIndex < HelpFile.TopicCount do
    begin
      Topic := HelpFile.Topics[ TopicIndex ];
      if Topic.ShowInContents then
      begin
        if Topic.ContentsLevel = 1 then
        begin
          Node := tvContents.RootNode.AppendText(Topic.Title);
          Node.Data := Topic;
          if FirstNode = nil then
            FirstNode := Node;
          inc( TopicIndex );
        end
        else
        begin
          // child nodes
          AddChildNodes( HelpFile,
                         Node,
                         Topic.ContentsLevel,
                         TopicIndex );
          Node := nil;
        end;
      end
      else
      begin
        inc( TopicIndex );
      end;
    end;
  end;
  LogEvent(LogStartup, '  EndUpdate' );

  if Settings.OpenWithExpandedContents then
  begin
    LogEvent(LogStartup, '  Expand all contents' );
    tvContents.RootNode.Expand;
    node := tvContents.RootNode.Next;
    while node <> nil do
    begin
      node.Expand;
      node := tvContents.RootNode.Next;
    end;
  end
  else
  begin
    LogEvent(LogStartup, '  Expand first node' );
    // Contents has only one top level node... expand it
    FirstNode.Expand;
  end;

  ContentsLoaded := true;
  tvContents.Invalidate;
  LogEvent(LogStartup, '  Contents loaded' );
end;

// Gets the contents from each file. Sorts it alphabetically.
// Merges all the sorted contents and indexes together, alphabetically.
procedure TMainForm.LoadIndex;
var
  tmpHelpFile: THelpFile;
  tmpTextCompareResult: integer;
  FileIndex: longint;
  Contents: TList;
  ContentsLists: TList; // of tlist
  tmpIndexLists: TList; // of tstringlist
  ContentsNextIndex: array[ 0..255 ] of longint;
  IndexNextIndex: array[ 0..255 ] of longint;
  Topic: TTopic;
  ListEntry: string;
  LowestEntry: string;
  LastEntry: string;
  tmpLowestEntryListIndex: longint;
  tmpLowestEntryListType: TListType;
  tmpLowestEntryTopic: TTopic;
  tmpIndex: TStringList;
  i: longint;
begin
  if CurrentOpenFiles.Count = 0 then
    Exit;
  LogEvent(LogStartup, 'Create index' );
  SetWaitCursor;
  LogEvent(LogStartup, '  Get/sort lists' );

  ProgressBar.Position := 70;
  SetStatus( 'Building index... ' );

  ContentsLists := TList.Create;
  tmpIndexLists := TList.Create;

  // collect the contents and index lists from the files
  for FileIndex := 0 to CurrentOpenFiles.Count - 1 do
  begin
    tmpHelpFile := THelpFile(CurrentOpenFiles[ FileIndex ]);
    ProgressBar.Position := 70 + 10 * FileIndex div CurrentOpenFiles.Count;

    if Settings.IndexStyle in [ isAlphabetical, isFull ] then
    begin
      Contents := TList.Create;
      Contents.Capacity := tmpHelpFile.TopicCount;  // speeds up inserts
      // copy [contents] topic list
      for i := 0 to tmpHelpFile.TopicCount - 1 do
      begin
        Topic := tmpHelpFile.Topics[ i ];
        if Topic.ShowInContents then
          Contents.Add( Topic );
      end;
      // sort by title
      Contents.Sort( @TopicTitleCompare );
      ContentsLists.Add( Contents );
      // initialise list index
      ContentsNextIndex[ ContentsLists.Count - 1 ] := 0;
    end;

    if Settings.IndexStyle in [ isFileOnly, isFull ] then
    begin
      tmpIndexLists.Add(tmpHelpFile.Index.GetLabels);
      IndexNextIndex[ tmpIndexLists.Count - 1 ] := 0;
    end;
  end;

  // Unlike contents, we do clear the index (even if we are adding more files)
  // because we need to re-merge the whole thing
  DisplayedIndex.Clear;
  ProgressBar.Position := 80;

  LogEvent(LogStartup, '  Merge lists' );
  LastEntry := '';
  while true do
  begin
    LowestEntry := '';
    tmpLowestEntryListIndex := -1;
    // Find alphabetically lowest (remaining) topic
    // first, look in contents lists
    LogEvent(LogDebug, '  Merge contents' );
    for i := 0 to ContentsLists.Count - 1 do
    begin
      Contents := TList(ContentsLists.Items[i]);
      if ContentsNextIndex[i] < Contents.Count then
      begin
        // list is not yet finished, get next entry
        Topic := TTopic(Contents[ ContentsNextIndex[i] ]);
        ListEntry := Topic.Title;

        if LowestEntry <> '' then
          tmpTextCompareResult := CompareText( ListEntry, LowestEntry )
        else
          tmpTextCompareResult := -1;

        if tmpTextCompareResult < 0 then
        begin
          // this index entry comes before the lowest one so far
          LowestEntry := ListEntry;
          tmpLowestEntryListIndex := i;
          tmpLowestEntryListType := ltContents;
          tmpLowestEntryTopic := Topic;
        end;
      end;
    end;

    // look in indices
    LogEvent(LogDebug, '  Merge indices' );
    for i := 0 to tmpIndexLists.Count - 1 do
    begin
      LogEvent(LogDebug, '  Merge indices ' + IntToStr(i) );
      tmpIndex := TStringList(tmpIndexLists.Items[i]);
      if IndexNextIndex[i] < tmpIndex.Count then
      begin
        // list is not yet finished, get next entry
        ListEntry := tmpIndex.Strings[ IndexNextIndex[i] ];
        LogEvent(LogDebug, '    indices ListEntry=' + ListEntry );
        if LowestEntry <> '' then
          tmpTextCompareResult := CompareText( ListEntry, LowestEntry )
        else
          tmpTextCompareResult := -1;

        if tmpTextCompareResult < 0 then
        begin
          // this index entry comes before the lowest one so far
          LowestEntry := ListEntry;
          tmpLowestEntryListIndex := i;
          tmpLowestEntryListType := ltIndex;

          LogEvent(LogDebug, '  Merge indices ' + tmpIndex.Objects[ IndexNextIndex[i] ].ClassName);
          tmpLowestEntryTopic := TIndexEntry( tmpIndex.Objects[ IndexNextIndex[i] ] ).getTopic;
        end;
      end;
    end;

    if tmpLowestEntryListIndex = -1 then
      // we're out
      break;

    if LowestEntry <> LastEntry then
      // add, if different from last
      DisplayedIndex.AddObject( LowestEntry, tmpLowestEntryTopic );
    LastEntry := LowestEntry;

    if tmpLowestEntryListType = ltContents then
    begin
      inc( ContentsNextIndex[ tmpLowestEntryListIndex ] );
    end
    else
    begin
      // found in one of indices.
      // Check for subsequent indented strings
      tmpIndex := TStringList(tmpIndexLists[ tmpLowestEntryListIndex ]);

      i := IndexNextIndex[ tmpLowestEntryListIndex ] + 1;
      while i < tmpIndex.Count do
      begin
        ListEntry := tmpIndex.Strings[ i ];
        if ListEntry = '' then
          break;

        if ListEntry[ 1 ] <> ' ' then
          // not indented, stop looking
          break;

        // found one,
        Topic := TIndexEntry(tmpIndex.Objects[ i ]).getTopic;
        DisplayedIndex.AddObject( ListEntry, Topic );
        inc( i );
      end;
      IndexNextIndex[ tmpLowestEntryListIndex ] := i;
    end;
  end;

  ProgressBar.Position := 95;
  LogEvent(LogStartup, '  Display index (count = ' + IntToStr(DisplayedIndex.Count) + ')');

  // Now display the final index list
  lbIndex.BeginUpdate;
  lbIndex.Items.Assign( DisplayedIndex );
  lbIndex.EndUpdate;

  LogEvent(LogStartup, '  Tidy up' );
  tmpIndexLists.Free;
  DestroyListAndObjects( ContentsLists );
  IndexLoaded := true;

  ClearWaitCursor;

  SetStatus( 'Index loaded' );
  LogEvent(LogStartup, '  Done' );
end;

procedure TMainForm.AddChildNodes(AHelpFile: THelpFile; AParentNode: TfpgTreeNode;
  ALevel: longint; var ATopicIndex: longint);
var
  Topic: TTopic;
  Node: TfpgTreeNode;
begin
  Node := nil;
  while ATopicIndex < AHelpFile.TopicCount do
  begin
    Topic := AHelpFile.Topics[ ATopicIndex ];
    if Topic.ShowInContents then
    begin
      if Topic.ContentsLevel < ALevel then
        break;

      if Topic.ContentsLevel = ALevel then
      begin
        Node := AParentNode.AppendText(Topic.Title);
        Node.Data := Topic;
        inc( ATopicIndex );
      end
      else
      begin
        AddChildNodes( AHelpFile,
                       Node,
                       Topic.ContentsLevel,
                       ATopicIndex );
        Node := nil;
      end
    end
    else
    begin
      inc( ATopicIndex );
    end;
  end;  { while }
end;

procedure TMainForm.ClearNotes;
begin
  { TODO -oGraeme : Implement me }
end;

procedure TMainForm.ClearIndexComponents;
begin
  IndexSearchEdit.Clear;
  lbIndex.FocusItem := -1;
  lbIndex.Items.Clear;
  DisplayedIndex.Clear;
  IndexLoaded := False;
end;

procedure TMainForm.SaveNotes(AHelpFile: THelpFile);
begin
  { TODO -oGraeme : Implement me }
end;

procedure TMainForm.DisplayTopic(ATopic: TTopic);
var
  lText: String;
  ImageIndices: TList;
  LinkIndex: longint;
  Link: THelpLink;
  HelpFile: THelpFile;
  Topic: TTopic;
  HighlightWordSequences: TList;
  FileIndex: integer;
begin
  if CurrentOpenFiles.Count = 0 then
    Exit;

  ProfileEvent('DisplayTopic >>>>');
  if ATopic = nil then
  begin
    case PageControl1.ActivePageIndex of
      0:  begin // TOC tab
            if tvContents.Selection = nil then
            begin
              ShowMessage('You must select a topic first by clicking it.');
              Exit;  //==>
            end
            else
              Topic := TTopic(tvContents.Selection.Data);
            ProfileEvent('Got Topic from Treeview');
          end;
      1:  begin // Index tab
            if lbIndex.FocusItem = -1 then
            begin
              ShowMessage('You must select a index first by clicking it.');
              Exit;  //==>
            end
            else
              Topic := TTopic(lbIndex.Items.Objects[lbIndex.FocusItem]);
            ProfileEvent('Got Topic from Index listbox');
          end;
      2:  begin // Search tab
            if lbSearchResults.FocusItem = -1 then
            begin
              ShowMessage('You must select a search result first by clicking it.');
              Exit;  //==>
            end
            else
              Topic := TTopic(lbSearchResults.Items.Objects[lbSearchResults.FocusItem]);
            ProfileEvent('Got Topic from Search Results listbox');
          end;
      4:  begin // History tab
            if lbHistory.FocusItem = -1 then
            begin
              ShowMessage('You must select a history item first by clicking it.');
              Exit;  //==>
            end
            else
              Topic := TTopic(lbHistory.Items.Objects[lbHistory.FocusItem]);
            ProfileEvent('Got Topic from History listbox');
          end;
    end;
  end  // case..
  else
    Topic := ATopic;  // use topic passed in as a parameter

  if Topic = nil then
    raise Exception.Create('Unable to locate the Topic');

  CurrentTopic := Topic;

  RichView.Clear;
  ImageIndices := TList.Create;
  ProfileEvent('Cleared memo...');

  HelpFile := TopicFile(CurrentTopic);
  if HelpFile = nil then
    raise Exception.Create('Failed to get active HelpFile from Topic');

  if HelpFile.HighlightWords <> nil then
    ProfileEvent('highlightwords is ok');

  if (AllFilesWordSequences.Count > 0) // ie we have done a search...
     {and ViewHighlightSearchWordsMI.Checked} then
  begin
    FileIndex := CurrentOpenFiles.IndexOf( HelpFile );
    HighlightWordSequences := TList(AllFilesWordSequences[ FileIndex ]);
  end
  else
    HighlightWordSequences :=  nil;

  lText := '';
  ProfileEvent('Debug show hex values = ' + BoolToStr(Debug));
  if ImageIndices <> nil then
    ProfileEvent('ImageIndices initialized');

  CurrentTopic.GetText( HighlightWordSequences,
                  Debug {ShowCodes},
                  False {ShowWordIndices},
                  lText {TopicText},
                  ImageIndices,
                  nil {Highlights} );

  if ImageIndices.Count > 0 then
  begin
  { TODO -oGraeme : We do not support images yet }
//   THelpFile(CurrentTopic.HelpFile).GetImages(ImageIndices, FImages);
  end;

  ImageIndices.Free;

  //writeln(lText);
  //writeln('-----------------------------');
  RichView.AddText(PChar(lText));

  tvContents.Selection := tvContents.RootNode.FindSubNode(CurrentTopic, True);
  tvContents.Invalidate;
  SaveNavigatePoint;
  UpdateLocationPanel;
end;

procedure TMainForm.ResetProgress;
begin
  { TODO -oGraeme : implement ResetProgress }
  ProgressBar.Visible := False;
  ProgressBar.Position := 0;
end;

procedure TMainForm.SetStatus(const AText: TfpgString);
begin
  lblStatus.Text := AText;
end;

function TMainForm.TranslateEnvironmentVar(AFilenames: TfpgString): TfpgString;
var
  EnvironmentVarValue: string;
begin
  EnvironmentVarValue := GetEnvironmentVariable(UpperCase(AFilenames));
  if EnvironmentVarValue <> '' then
    Result := EnvironmentVarValue
  else
    Result := AFileNames;
end;

constructor TMainForm.Create(AOwner: TComponent);
var
  img: TfpgImage;
begin
  inherited Create(AOwner);
  fpgApplication.OnException  := @MainFormException;
  OnShow  := @MainFormShow;
  OnDestroy :=@MainFormDestroy;
//  Files := TList.Create;
  AllFilesWordSequences := TList.Create;
  CurrentOpenFiles := TList.Create;
  DisplayedIndex := TStringList.Create;
  CurrentHistoryIndex := -1;
  FHistorySelection := False;
  { TODO -oGraeme : Make Debug a menu option }
  Debug := False;

  FImages := TfpgImageList.Create;
  // store up to three images per Topic - while we don't support INF images
  img := CreateImage_BMP(@dv_missing, sizeof(dv_missing));
  FImages.AddImage(img);
  img := CreateImage_BMP(@dv_missing, sizeof(dv_missing));
  FImages.AddImage(img);
  img := CreateImage_BMP(@dv_missing, sizeof(dv_missing));
  FImages.AddImage(img);


  // load toolbar images
  fpgImages.AddMaskedBMP(
    'dv.arrowleft', @usr_arrow_left,
    sizeof(usr_arrow_left), 0, 0);

  fpgImages.AddMaskedBMP(
    'dv.arrowright', @usr_arrow_right,
    sizeof(usr_arrow_right), 0, 0);

  fpgImages.AddMaskedBMP(
    'dv.arrowup', @usr_arrow_up,
    sizeof(usr_arrow_up), 0, 0);

  fpgImages.AddMaskedBMP(
    'dv.arrowdown', @usr_arrow_down,
    sizeof(usr_arrow_down), 0, 0);

end;

destructor TMainForm.Destroy;
begin
  RichView.Images := nil;
  FImages.Free;
  CurrentTopic := nil;  // it was a reference only
  FFileOpenRecent := nil;   // it was a reference only
  miOpenRecentMenu.Free;
//  DestroyListAndObjects(Files);
  DestroyListAndObjects(AllFilesWordSequences);
  DestroyListAndObjects(CurrentOpenFiles);
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(602, 274, 654, 386);
  WindowTitle := 'fpGUI Documentation Viewer';
  Hint := '';
  ShowHint := True;
  WindowPosition := wpUser;
  OnCloseQuery  := @MainFormCloseQuery;

  bvlStatusBar := TfpgBevel.Create(self);
  with bvlStatusBar do
  begin
    Name := 'bvlStatusBar';
    SetPosition(0, 366, 653, 20);
    Anchors := [anLeft,anRight,anBottom];
    Hint := '';
    Style := bsLowered;
  end;

  ProgressBar := TfpgProgressBar.Create(bvlStatusBar);
  with ProgressBar do
  begin
    Name := 'ProgressBar';
    SetPosition(501, 2, 150, 16);
    Anchors := [anRight,anBottom];
    Hint := '';
  end;

  lblStatus := TfpgLabel.Create(bvlStatusBar);
  with lblStatus do
  begin
    Name := 'lblStatus';
    SetPosition(4, 2, 380, 16);
    Anchors := [anLeft,anRight,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    Text := '';
  end;

  bvlBody := TfpgBevel.Create(self);
  with bvlBody do
  begin
    Name := 'bvlBody';
    SetPosition(0, 55, 653, 310);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    Shape := bsSpacer;
  end;

  PageControl1 := TfpgPageControl.Create(bvlBody);
  with PageControl1 do
  begin
    Name := 'PageControl1';
    SetPosition(0, 16, 260, 276);
    ActivePageIndex := 0;
    Hint := '';
    TabOrder := 0;
    Align := alLeft;
    OnChange  := @PageControl1Change;
  end;

  tsContents := TfpgTabSheet.Create(PageControl1);
  with tsContents do
  begin
    Name := 'tsContents';
    SetPosition(3, 24, 254, 249);
    Text := 'Contents';
  end;

  tvContents := TfpgTreeView.Create(tsContents);
  with tvContents do
  begin
    Name := 'tvContents';
    SetPosition(4, 32, 242, 212);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    ScrollWheelDelta := 60;
    ShowImages := True;
    TabOrder := 0;
    OnChange  := @tvContentsChange;
    //OnDoubleClick  := @tvContentsDoubleClick;
  end;

  btnGo := TfpgButton.Create(tsContents);
  with btnGo do
  begin
    Name := 'btnGo';
    SetPosition(166, 4, 80, 24);
    Anchors := [anRight,anTop];
    Text := 'Go to';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    OnClick := @btnGoClicked;
  end;

  tsIndex := TfpgTabSheet.Create(PageControl1);
  with tsIndex do
  begin
    Name := 'tsIndex';
    SetPosition(3, 24, 254, 249);
    Text := 'Index';
  end;

  btnIndex := TfpgButton.Create(tsIndex);
  with btnIndex do
  begin
    Name := 'btnIndex';
    SetPosition(166, 4, 80, 24);
    Anchors := [anRight,anTop];
    Text := 'Go to';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    OnClick := @btnShowIndex;
  end;

  lbIndex := TfpgListBox.Create(tsIndex);
  with lbIndex do
  begin
    Name := 'lbIndex';
    SetPosition(4, 32, 242, 212);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#List';
    Hint := '';
    HotTrack := False;
    PopupFrame := False;
    TabOrder := 1;
    OnDoubleClick  := @lbIndexDoubleClick;
    OnKeyPress:=@lbIndexKeyPress;
  end;

  IndexSearchEdit := TfpgEdit.Create(tsIndex);
  with IndexSearchEdit do
  begin
    Name := 'IndexSearchEdit';
    SetPosition(4, 4, 152, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    Hint := '';
    TabOrder := 2;
    Text := '';
    FontDesc := '#Edit1';
    OnChange := @IndexSearchEditOnChange;
    OnKeyPress :=@IndexSearchEditKeyPress;
  end;

  tsSearch := TfpgTabSheet.Create(PageControl1);
  with tsSearch do
  begin
    Name := 'tsSearch';
    SetPosition(3, 24, 254, 301);
    Text := 'Search';
  end;

  Label1 := TfpgLabel.Create(tsSearch);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(4, 4, 120, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Search for:';
  end;

  edSearchText := TfpgEdit.Create(tsSearch);
  with edSearchText do
  begin
    Name := 'edSearchText';
    SetPosition(4, 20, 210, 26);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    Hint := '';
    TabOrder := 1;
    Text := '';
    FontDesc := '#Edit1';
    OnKeyPress :=@edSearchTextKeyPress;
  end;

  Label2 := TfpgLabel.Create(tsSearch);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(4, 48, 172, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Criteria:';
  end;

  RadioButton1 := TfpgRadioButton.Create(tsSearch);
  with RadioButton1 do
  begin
    Name := 'RadioButton1';
    SetPosition(12, 68, 192, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 3;
    Text := 'This section';
    Enabled := False;
  end;

  RadioButton2 := TfpgRadioButton.Create(tsSearch);
  with RadioButton2 do
  begin
    Name := 'RadioButton2';
    SetPosition(12, 88, 192, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 4;
    Text := 'Marked sections';
    Enabled := False;
  end;

  RadioButton3 := TfpgRadioButton.Create(tsSearch);
  with RadioButton3 do
  begin
    Name := 'RadioButton3';
    SetPosition(12, 108, 192, 20);
    Checked := True;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 5;
    Text := 'All sections';
    Enabled := False;
  end;

  RadioButton4 := TfpgRadioButton.Create(tsSearch);
  with RadioButton4 do
  begin
    Name := 'RadioButton4';
    SetPosition(12, 128, 192, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 6;
    Text := 'Index';
    Enabled := False;
  end;

  RadioButton5 := TfpgRadioButton.Create(tsSearch);
  with RadioButton5 do
  begin
    Name := 'RadioButton5';
    SetPosition(12, 148, 192, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 7;
    Text := 'Marked libraries';
    Enabled := False;
  end;

  RadioButton6 := TfpgRadioButton.Create(tsSearch);
  with RadioButton6 do
  begin
    Name := 'RadioButton6';
    SetPosition(12, 168, 192, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 8;
    Text := 'All libraries';
    Enabled := False;
  end;

  lbSearchResults := TfpgListBox.Create(tsSearch);
  with lbSearchResults do
  begin
    Name := 'lbSearchResults';
    SetPosition(4, 220, 242, 76);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#List';
    Hint := '';
    HotTrack := False;
    PopupFrame := False;
    TabOrder := 9;
    OnDoubleClick := @lbSearchResultsDoubleClick;
    OnKeyPress := @lbSearchResultsKeyPress;
  end;

  Label3 := TfpgLabel.Create(tsSearch);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(4, 200, 196, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Search results:';
  end;

  btnSearch := TfpgButton.Create(tsSearch);
  with btnSearch do
  begin
    Name := 'btnSearch';
    SetPosition(220, 20, 28, 26);
    Anchors := [anRight,anTop];
    Text := 'Go';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 11;
    OnClick := @btnSearchClicked;
  end;

  tsNotes := TfpgTabSheet.Create(PageControl1);
  with tsNotes do
  begin
    Name := 'tsNotes';
    SetPosition(3, 24, 254, 249);
    Text := 'Notes';
  end;

  ListBox1 := TfpgListBox.Create(tsNotes);
  with ListBox1 do
  begin
    Name := 'ListBox1';
    SetPosition(4, 32, 242, 212);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#List';
    Hint := '';
    HotTrack := False;
    PopupFrame := False;
    TabOrder := 0;
  end;

  btnNotesAdd := TfpgButton.Create(tsNotes);
  with btnNotesAdd do
  begin
    Name := 'btnNotesAdd';
    SetPosition(4, 4, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'stdimg.add';
    TabOrder := 1;
    Enabled := false;
  end;

  btnNotesEdit := TfpgButton.Create(tsNotes);
  with btnNotesEdit do
  begin
    Name := 'btnNotesEdit';
    SetPosition(32, 4, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'stdimg.edit';
    TabOrder := 2;
    Enabled := False;
  end;

  btnNotesDel := TfpgButton.Create(tsNotes);
  with btnNotesDel do
  begin
    Name := 'btnNotesDel';
    SetPosition(60, 4, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'stdimg.remove';
    TabOrder := 3;
    Enabled := False;
  end;

  btnNotesGoto := TfpgButton.Create(tsNotes);
  with btnNotesGoto do
  begin
    Name := 'btnNotesGoto';
    SetPosition(166, 4, 80, 24);
    Anchors := [anRight,anTop];
    Text := 'Go to';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
  end;

  tsHistory := TfpgTabSheet.Create(PageControl1);
  with tsHistory do
  begin
    Name := 'tsHistory';
    SetPosition(3, 24, 254, 249);
    Text := 'History';
  end;

  lbHistory := TfpgListBox.Create(tsHistory);
  with lbHistory do
  begin
    Name := 'lbHistory';
    SetPosition(4, 8, 242, 236);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#List';
    Hint := '';
    HotTrack := False;
    PopupFrame := False;
    TabOrder := 0;
    OnDoubleClick := @lbHistoryDoubleClick;
    OnKeyPress := @lbHistoryKeyPress;
  end;

  Splitter1 := TfpgSplitter.Create(bvlBody);
  with Splitter1 do
  begin
    Name := 'Splitter1';
    SetPosition(265, 120, 8, 168);
    Align := alLeft;
  end;

  RichView := TRichTextView.Create(bvlBody);
  with RichView do
  begin
    Name := 'RichView';
    SetPosition(368, 192, 244, 92);
    TabOrder := 2;
    Align := alClient;
    OnClickLink:=@RichViewClickLink;
  end;

  MainMenu := TfpgMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 654, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  miFile := TfpgPopupMenu.Create(self);
  with miFile do
  begin
    Name := 'miFile';
    SetPosition(292, 96, 132, 20);
    AddMenuItem('Open...', '', @miFileOpenClicked);
    AddMenuItem('Open Special...', '', @miFileOpenSpecialClicked);
    AddMenuItem('Save current Topic to IPF...', '', @miFileSaveTopicAsIPF);
    AddMenuItem('Close', '', @miFileCloseClicked);
    AddMenuitem('-', '', nil);
    FFileOpenRecent := AddMenuItem('Open Recent...', '', nil);
    AddMenuitem('-', '', nil);
    AddMenuItem('Quit', '', @miFileQuitClicked);
  end;

  miSettings := TfpgPopupMenu.Create(self);
  with miSettings do
  begin
    Name := 'miSettings';
    SetPosition(292, 120, 132, 20);
    AddMenuItem('Options...', '', @miConfigureClicked);
  end;

  miBookmarks := TfpgPopupMenu.Create(self);
  with miBookmarks do
  begin
    Name := 'miBookmarks';
    SetPosition(292, 144, 132, 20);
    AddMenuItem('Add..', '', nil);
    AddMenuItem('Show', '', nil);
  end;

  miHelp := TfpgPopupMenu.Create(self);
  with miHelp do
  begin
    Name := 'miHelp';
    SetPosition(292, 168, 132, 20);
    AddMenuItem('Help using DocView', 'F1', @miHelpUsingDocView);
    AddMenuItem('Command line parameters', '', @miHelpCmdLineParams);
    AddMenuItem('-', '', nil);
    AddMenuItem('Show help file header info', '', @miDebugHeader);
    AddMenuItem('-', '', nil);
    AddMenuItem('About fpGUI Toolkit...', '', @miHelpAboutFPGui);
    AddMenuItem('Product Information...', '', @miHelpProdInfoClicked);
  end;

  miDebug := TfpgPopupMenu.Create(self);
  with miDebug do
  begin
    Name := 'miDebug';
    SetPosition(292, 192, 132, 20);
    miDebugHexInfo := AddMenuItem('Toggle Hex INF Values in Contents', '', @miDebugHex);
  end;

  ToolBar := TfpgBevel.Create(self);
  with ToolBar do
  begin
    Name := 'ToolBar';
    SetPosition(0, 25, 654, 28);
    Anchors := [anLeft,anRight,anTop];
    Hint := '';
    Style := bsLowered;
    Shape := bsBottomLine;
  end;

  btnOpen := TfpgButton.Create(ToolBar);
  with btnOpen do
  begin
    Name := 'btnOpen';
    SetPosition(30, 1, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Open a new help file.';
    ImageMargin := -1;
    ImageName := 'stdimg.open';
    ImageSpacing := 0;
    TabOrder := 0;
    OnClick := @miFileOpenClicked;
    Focusable := False;
  end;

  btnBack := TfpgButton.Create(ToolBar);
  with btnBack do
  begin
    Name := 'btnBack';
    SetPosition(70, 1, 32, 24);
    Text := '<';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Previous history item.';
    ImageMargin := -1;
    ImageName := 'dv.arrowleft';
    ImageSpacing := 0;
    TabOrder := 2;
    Focusable := False;
    OnClick := @btnBackHistClick;
  end;

  btnFwd := TfpgButton.Create(ToolBar);
  with btnFwd do
  begin
    Name := 'btnFwd';
    SetPosition(104, 1, 32, 24);
    Text := '>';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Next history item.';
    ImageMargin := -1;
    ImageName := 'dv.arrowright';
    ImageSpacing := 0;
    TabOrder := 3;
    Focusable := False;
    OnClick := @btnFwdHistClick;
  end;

  btnPrev := TfpgButton.Create(ToolBar);
  with btnPrev do
  begin
    Name := 'btnPrev';
    SetPosition(138, 1, 32, 24);
    Text := 'prev';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Previous Topic.';
    ImageMargin := -1;
    ImageName := 'dv.arrowup';
    ImageSpacing := 0;
    TabOrder := 4;
    Focusable := False;
    OnClick := @btnPrevClick;
  end;

  btnNext := TfpgButton.Create(ToolBar);
  with btnNext do
  begin
    Name := 'btnNext';
    SetPosition(172, 1, 32, 24);
    Text := 'next';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Next Topic.';
    ImageMargin := -1;
    ImageName := 'dv.arrowdown';
    ImageSpacing := 0;
    TabOrder := 5;
    Focusable := False;
    OnClick :=@btnNextClick;
  end;

  btnHelp := TfpgButton.Create(ToolBar);
  with btnHelp do
  begin
    Name := 'btnHelp';
    SetPosition(218, 1, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Display Product Information.';
    ImageMargin := -1;
    ImageName := 'stdimg.help';
    ImageSpacing := 0;
    TabOrder := 6;
    Focusable := False;
    OnClick := @miHelpProdInfoClicked;
  end;

  btnQuit := TfpgButton.Create(ToolBar);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(4, 1, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := -1;
    ImageName := 'stdimg.quit';
    ImageSpacing := 0;
    TabOrder := 8;
    OnClick := @miFileQuitClicked;
    Focusable := False;
  end;

  Bevel1 := TfpgBevel.Create(ToolBar);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(61, 0, 6, 24);
    Hint := '';
    Style := bsLowered;
    Shape := bsLeftLine;
  end;

  Bevel2 := TfpgBevel.Create(ToolBar);
  with Bevel2 do
  begin
    Name := 'Bevel2';
    SetPosition(210, 0, 6, 24);
    Hint := '';
    Style := bsLowered;
    Shape := bsLeftLine;
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}

  // remove toolbar button text
  btnBack.Text := '';
  btnFwd.Text := '';
  btnNext.Text := '';
  btnPrev.Text := '';

  miOpenRecentMenu := TfpgPopupMenu.Create(nil);
  with miOpenRecentMenu do
  begin
    Name := 'miOpenRecentMenu';
    SetPosition(309, 52, 132, 20);
  end;

  // hook up the sub-menus.
  MainMenu.AddMenuItem('&File', nil).SubMenu := miFile;
  MainMenu.AddMenuItem('&Settings', nil).SubMenu := miSettings;
  MainMenu.AddMenuItem('&Bookmarks', nil).SubMenu := miBookmarks;
  MainMenu.AddMenuItem('&Help', nil).SubMenu := miHelp;
  MainMenu.AddMenuItem('&Debug', nil).SubMenu := miDebug;
  FFileOpenRecent.SubMenu := miOpenRecentMenu;

  // correct default visible tabsheet
  PageControl1.ActivePageIndex := 0;
end;

procedure TMainForm.RefreshFontSubstitutions;
var
  FileIndex: longint;
  HelpFile: THelpFile;
begin
  for FileIndex := 0 to CurrentOpenFiles.Count - 1 do
  begin
    HelpFile := THelpFile(CurrentOpenFiles[ FileIndex ]);

    if Settings.FixedFontSubstitution then
      HelpFile.SetupFontSubstitutes( Settings.FixedFontSubstitutes )
    else
      HelpFile.SetupFontSubstitutes( '' );
  end;
end;

procedure TMainForm.DisplaySelectedContentsTopic;
var
  Topic: TTopic;
Begin
  if tvContents.Selection = nil then
    Exit;
  Topic := TTopic(tvContents.Selection.Data);
  DisplayTopic(Topic);
End;

procedure TMainForm.DisplaySelectedIndexTopic;
var
  Topic: TTopic;
begin
  if lbIndex.FocusItem = -1 then
    exit;
  Topic := DisplayedIndex.Objects[ lbIndex.FocusItem ] as TTopic;
  DisplayTopic(Topic);
end;

procedure TMainForm.ProcessCommandLineParams;
var
  showtopic: boolean;
  t: TTopic;
begin
  if ParamCount > 0 then
  begin
    if gCommandLineParams.IsParam('h') then
    begin
      ShowCmdLineParamHelp;
      Exit; //==>
    end
    else if gCommandLineParams.IsParam('debuglog') then
      // do nothing
    else
    begin
//writeln('DEBUG:  TMainForm.ProcessCommandLineParams - Open file...');
      showtopic := not gCommandLineParams.IsParam('k');
      OpenFile(ParamStr(1), '', showtopic);
    end;
  end;

  // now process all other parameters
  if gCommandLineParams.IsParam('k') then
  begin
//writeln('DEBUG:  TMainForm.ProcessCommandLineParams - Keyword Search string');
    { Search for a string }
    edSearchText.Text := gCommandLineParams.GetParam('k');
    PageControl1.ActivePage := tsSearch;
    btnSearch.Click;
  end
  else if gCommandLineParams.IsParam('n') then
  begin
    { Display topic with numeric topic id }
//writeln('DEBUG:  TMainForm.ProcessCommandLineParams - Display numeric topic id');
    t := FindTopicByResourceID(StrToInt(gCommandLineParams.GetParam('n')));
//if not Assigned(t) then
//  writeln(Format('Failed to find topic <%s>', [gCommandLineParams.GetParam('n')]));
    DisplayTopic(t);
  end
  else if gCommandLineParams.IsParam('s') then
  begin
//writeln('DEBUG:  TMainForm.ProcessCommandLineParams - display string topic id');
    { Display topic with string topic id }
    t := FindTopicByName(gCommandLineParams.GetParam('s'));
//if not Assigned(t) then
//  writeln(Format('Failed to find topic <%s>', [gCommandLineParams.GetParam('k')]));
    DisplayTopic(t);
  end;
end;

procedure TMainForm.SaveNavigatePoint;
begin
  // if we selected an item from history listbox, don't record that save point
  if FHistorySelection then
    Exit;

  // delete rest of history
  while CurrentHistoryIndex < lbHistory.Items.Count-1 do
    lbHistory.Items.Delete(CurrentHistoryIndex + 1);

  if CurrentTopic <> nil then
    lbHistory.Items.AddObject(CurrentTopic.Title, CurrentTopic);

  inc(CurrentHistoryIndex);
end;

procedure TMainForm.ShowCmdLineParamHelp;
const
  le = LineEnding;
var
  s: string;
begin
  s := '<font "Arial" 12><b>' + cLongName + '</b></font>' + le
       + cVersion + le + le
       + 'Supported command line parameters:' + le + le
       + '<tt>'
       + '  <<filename>       Load the help file <<filename>' + le
       + '  -h               Show this help' + le
       + '  -k <<text>        Search for keyword <<text> in open help files' + le
       + '  -n <<id>          Open Topic with numeric ID equal to <<id>' + le
       + '  -s <<id>          Open Topic with string ID equal to <<id>' + le
       + '  -debuglog <<filename> Log information to a file' + le
       + '</tt>'
       ;
  RichView.AddText(PChar(s));
end;

// Find the target topic for the given link
function TMainForm.FindTopicForLink(Link: THelpLink): TTopic;
var
  HelpFile: THelpFile;
begin
  HelpFile := Link.HelpFile as THelpFile;
  if Link is TFootnoteHelpLink then
  begin
    Result := HelpFile.Topics[ TFootnoteHelpLink( Link ).TopicIndex ];
  end
  else if Link is TInternalHelpLink then
  begin
    Result := HelpFile.Topics[ TInternalHelpLink( Link ).TopicIndex ];
  end
  else if Link is THelpLinkByResourceID then
  begin
    Result := FindTopicByResourceID( THelpLinkByResourceID( Link ).ResourceID );
  end
end;

// Find topic specified by numeric resource ID, in all open files
function TMainForm.FindTopicByResourceID(ID: word): TTopic;
var
  FileIndex: longint;
  HelpFile: THelpFile;
begin
  for FileIndex := 0 to CurrentOpenFiles.Count - 1 do
  begin
    HelpFile := THelpFile(CurrentOpenFiles[ FileIndex ]);

    Result := HelpFile.FindTopicByResourceID( ID );
    if Result <> nil then
      // found
      exit;
  end;

  // not found.
  Result := nil;
end;

function TMainForm.FindTopicByName(const AName: string): TTopic;
var
  FileIndex: longint;
  HelpFile: THelpFile;
begin
  Result := nil;
  for FileIndex := 0 to CurrentOpenFiles.Count - 1 do
  begin
    HelpFile := THelpFile(CurrentOpenFiles[ FileIndex ]);
    Result := HelpFile.FindTopicByLocalName(AName);
    if Result <> nil then
      // found
      exit; //==>
  end;
  // not found.
  Result := nil;
end;

// Add the current list of open files as
// a Most Recently Used entry
procedure TMainForm.AddCurrentToMRUFiles;
var
  tmpFilenames: TStringList;
  i: longint;
  tmpHelpFile: THelpFile;
begin
  tmpFilenames := TStringList.Create;

  if CurrentOpenFiles.Count > 0 then
  begin
    for i := 0 to CurrentOpenFiles.Count - 1 do
    begin
      tmpHelpFile := THelpFile(CurrentOpenFiles[ i ]);
      tmpFilenames.Add(tmpHelpFile.Filename);
    end;

    // update most-recently-used file list
    tmpHelpFile := THelpFile(CurrentOpenFiles[ 0 ]);
    AddToMRUList(tmpHelpFile.Title, tmpFilenames);
  end;

  // recreate menu
  CreateMRUMenuItems;

  tmpFilenames.Destroy;
end;

procedure TMainForm.CreateMRUMenuItems;
var
  MenuItem: TfpgMenuItem;
  i: integer;
  FileName: TfpgString;
  FileNameIndex: longint;
  MRUText: TfpgString;
  MRUItem: TMRUItem;
begin
  FFileOpenRecent.SubMenu := nil;
  miOpenRecentMenu.Free;
  miOpenRecentMenu := TfpgPopupMenu.Create(nil);
  FFileOpenRecent.SubMenu := miOpenRecentMenu;

  // Add items for the MRU files
  for i:= 0 to Settings.MRUList.Count -1 do
  begin
    MRUItem := TMRUItem(Settings.MRUList[ i ]);
    MenuItem := miOpenRecentMenu.AddMenuItem('?', '', @OnMRUMenuItemClick);
//    MenuItem.Name := 'MRUItem' + IntToStr( i ) + 'MI';

    MRUText := MRUItem.Title;
    if Trim( MRUText ) = '' then
    begin
      // Take the filenames, less path, as caption...
      MRUText := '';
      for FileNameIndex := 0 to MRUItem.Filenames.Count - 1 do
      begin
        FileName := MRUItem.Filenames[ FileNameIndex ];
        FileName := fpgExtractFileName(FileName);
        FileName := ChangeFileExt( FileName, '' );// remove extension

        if FileNameIndex > 0 then
        begin
          MRUText := MRUText + HELP_FILE_DELIMITER;
        end;
        MRUText := MRUText + FileName;

        // stop after 50 chars
        if UTF8Length( MRUText ) > 50 then
        begin
          MRUText := MRUText + HELP_FILE_DELIMITER + ' ...';
          break;
        end;
      end;
    end;

    MenuItem.Text := '&'
                       + IntToStr( i + 1 )
                       + '. '
                       + MRUText;
    if MRUItem.Filenames.Count = 1 then
      MenuItem.Hint := MRUItem.Filenames[ 0 ]
    else
      MenuItem.Hint := MRUItem.Title
                       + ' ('
                       + IntToStr( MRUItem.Filenames.Count )
                       + ' '
                       + rsfiles
                       + ')';

    MenuItem.Tag := i;
  end;
end;


end.
