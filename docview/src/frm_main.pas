unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_panel, fpg_tab,
  fpg_tree, fpg_splitter, fpg_menu, fpg_button, fpg_listbox,
  fpg_label, fpg_edit, fpg_radiobutton, fpg_progressbar, fpg_imagelist,
  fpg_imgfmt_bmp, fpg_combobox,
  HelpFile, RichTextView, HelpTopic, HelpBookmark;

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
    NotesListBox: TfpgListBox;
    btnNotesAdd: TfpgButton;
    btnNotesEdit: TfpgButton;
    btnNotesDel: TfpgButton;
    btnNotesGoto: TfpgButton;
    tsHistory: TfpgTabSheet;
    lbHistory: TfpgListBox;
    Splitter1: TfpgSplitter;
    bvlContentArea: TfpgBevel;
    pnlTitle: TfpgPanel;
    RichView: TRichTextView;
    MainMenu: TfpgMenuBar;
    miFile: TfpgPopupMenu;
    miActions: TfpgPopupMenu;
    miSettings: TfpgPopupMenu;
    miBookmarks: TfpgPopupMenu;
    miView: TfpgPopupMenu;
    miTools: TfpgPopupMenu;
    miHelp: TfpgPopupMenu;
    ToolBar: TfpgBevel;
    btnQuit: TfpgButton;
    btnOpen: TfpgButton;
    Bevel1: TfpgBevel;
    btnBack: TfpgButton;
    btnFwd: TfpgButton;
    btnPrev: TfpgButton;
    btnNext: TfpgButton;
    Bevel2: TfpgBevel;
    btnTBNoteAdd: TfpgButton;
    btnBookmark: TfpgButton;
    Bevel3: TfpgBevel;
    btnHelp: TfpgButton;
    cbEncoding: TfpgComboBox;
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
    OpenAdditionalFile: boolean;
    Notes: TList; // Notes in current files.
    Bookmarks: TList;
    BookmarksMenuItems: TList;

    procedure   Splitter1DoubleClicked(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   btnTBNoteAddClick(Sender: TObject);
    procedure   RichViewOverLink(Sender: TRichTextView; Link: string);
    procedure   RichViewNotOverLink(Sender: TRichTextView; Link: string);
    procedure   NotesListBoxChange(Sender: TObject);
    procedure   NotesListBoxKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure   NotesListBoxDoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   btnNotesAddClick(Sender: TObject);
    procedure   btnNotesDelClick(Sender: TObject);
    procedure   btnNotesEditClick(Sender: TObject);
    procedure   UpdateRichViewFromSettings;
    procedure   btnBackHistClick(Sender: TObject);
    procedure   btnFwdHistClick(Sender: TObject);
    procedure   btnPrevClick(Sender: TObject);
    procedure   btnNextClick(Sender: TObject);
    procedure   btnBookmarkClick(Sender: TObject);
    procedure   RichViewClickLink(Sender: TRichTextView; Link: string);
    procedure   IndexSearchEditKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure   MainFormShow(Sender: TObject);
    procedure   MainFormDestroy(Sender: TObject);
    procedure   miFileQuitClicked(Sender: TObject);
    procedure   miFileOpenClicked(Sender: TObject);
    procedure   miFileOpenAdditionalFileClicked(Sender: TObject);
    procedure   miFileOpenSpecialClicked(Sender: TObject);
    procedure   miFileCloseClicked(Sender: TObject);
    procedure   miActionsContentsClicked(Sender: TObject);
    procedure   miActionsIndexClicked(Sender: TObject);
    procedure   miActionsSearchClicked(Sender: TObject);
    procedure   miActionsNotesClicked(Sender: TObject);
    procedure   miActionsHistoryClicked(Sender: TObject);
    procedure   miActionsBackClicked(Sender: TObject);
    procedure   miActionsForwardClicked(Sender: TObject);
    procedure   miActionsPrevTopicClicked(Sender: TObject);
    procedure   miActionsNextTopicClicked(Sender: TObject);
    procedure   miConfigureClicked(Sender: TObject);
    procedure   miViewExpandAllClicked(Sender: TObject);
    procedure   miViewCollapseAllClicked(Sender: TObject);
    procedure   miOpenBookmarksMenuClicked(Sender: TObject);
    procedure   miBookmarksMenuItemClicked(Sender: TObject);
    procedure   miHelpProdInfoClicked(Sender: TObject);
    procedure   miHelpAboutFPGui(Sender: TObject);
    procedure   miHelpCmdLineParams(Sender: TObject);
    procedure   miHelpUsingDocView(Sender: TObject);
    procedure   miDebugHeader(Sender: TObject);
    procedure   miDebugHex(Sender: TObject);
    procedure   miToolsFindByResourceID(Sender: TObject);
    procedure   miToolsFindTopifByName(Sender: TObject);
    procedure   miToolsShowEnvVariablesClicked(Sender: TObject);
    procedure   miTopicPropertiesClicked(Sender: TObject);
    procedure   miDumpDictionaryClicked(Sender: TObject);
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
    procedure   cbEncodingChanged(Sender: TObject);
    procedure   btnNotesGotoClicked(Sender: TObject);
    procedure   UpdateEncodingComboBox;
    procedure   IndexSearchEditOnChange(Sender: TObject);
    procedure   pnlTitleGradientPaint(Sender: TObject);
    procedure   DisplaySelectedSearchResultTopic;
    procedure   NavigateToHistoryIndex(AIndex: integer);
    procedure   UpdateLocationPanel;
    procedure   UpdateTitlePanel;
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
    procedure   LoadContents(AFiles: TList; var FirstNode: TfpgTreeNode);
    procedure   LoadIndex;
    // Used in loading contents
    procedure   AddChildNodes(AHelpFile: THelpFile; AParentNode: TfpgTreeNode; ALevel: longint; var ATopicIndex: longint );
    procedure   ClearIndexComponents;

    // Note manipulations --------------------------------
    // make sure that note insert positions are not in
    // the middle of tags due to help file or newview updates.
    procedure   CorrectNotesPositions(Topic: TTopic; AText: pchar);
    procedure   InsertNotesIntoTopicText(ATopic: TTopic; var AText: TfpgString);
    function    FindOriginalNoteCharIndex(NoteCharIndex: longword; Topic: TTopic): longword;
    function    FindActualNoteCharIndex(NoteCharIndex: longword; MaxNoteIndex: longword; Topic: TTopic): longword;
    procedure   RefreshNoteInsertInfo( NoteIndex: longword );
    procedure   ClearNotes;
    procedure   SaveNotesForFile(AHelpFile: THelpFile);
    procedure   LoadNotesForFile(AHelpFile: THelpFile);
    procedure   AddNote;
    procedure   EditNote(ANoteIndex: longint);
    procedure   DeleteNote(ANoteIndex: longint);
    procedure   SaveNotes;
    procedure   GotoCurrentNote;
    procedure   UpdateNotesDisplay;
    procedure   EnableNotesControls;

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
    function    DisplayTopicByResourceID( ID: word ): boolean;
    function    DisplayTopicByName(const TopicName: string): boolean;
    function    DisplayTopicByGlobalName(const TopicName: string): boolean;
    procedure   ViewSourceMIOnClick(Sender: TObject);
    procedure   AddCurrentToMRUFiles;
    procedure   CreateMRUMenuItems;
    procedure   LoadBookmarks(AHelpFile: THelpFile);
    procedure   SaveBookmarks;
    procedure   SaveBookmarksForFile(AHelpFile: THelpFile);
    procedure   AddBookmark;
    procedure   ClearBookmarks;
    procedure   OnBookmarksChanged(Sender: TObject);
    procedure   BuildBookmarksMenu;
    procedure   NavigateToBookmark(Bookmark: TBookmark);
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
  ,frm_text
  ,frm_note
  ,frm_bookmarks
  ,CanvasFontManager
  ,HelpNote
  ,RichTextDocumentUnit
  ;

const
  cLongName   = 'fpGUI Documentation Viewer';
  cShortName  = 'DocView';
  cCreatedBy  = 'Created by Graeme Geldenhuys';
  cVersion    = 'Version ' + FPGUI_VERSION;

{$I arrows.inc}
{$I missing.inc}
{$I images.inc}

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

procedure TMainForm.miActionsBackClicked(Sender: TObject);
begin
  btnBack.Click;
end;

procedure TMainForm.miActionsForwardClicked(Sender: TObject);
begin
  btnFwd.Click;
end;

procedure TMainForm.miActionsPrevTopicClicked(Sender: TObject);
begin
  btnPrev.Click;
end;

procedure TMainForm.miActionsNextTopicClicked(Sender: TObject);
begin
  btnNext.Click;
end;

procedure TMainForm.Splitter1DoubleClicked(Sender: TObject;
  AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  PageControl1.Visible := not PageControl1.Visible;
  bvlBody.Realign;
end;

procedure TMainForm.btnTBNoteAddClick(Sender: TObject);
begin
  AddNote;
end;

procedure TMainForm.RichViewOverLink(Sender: TRichTextView; Link: string);
begin
  if StrLeft(Link, 4 ) = PARAM_LINK_NOTE then
    SetStatus('Click to edit note');
end;

procedure TMainForm.RichViewNotOverLink(Sender: TRichTextView; Link: string);
begin
  UpdateLocationPanel;
end;

procedure TMainForm.NotesListBoxChange(Sender: TObject);
begin
  EnableNotesControls;
end;

procedure TMainForm.NotesListBoxKeyPress(Sender: TObject; var KeyCode: word;
  var ShiftState: TShiftState; var Consumed: boolean);
begin
  if (KeyCode = keyReturn) or (KeyCode = keyPEnter) then
  begin
    Consumed := True;
    GotoCurrentNote;
  end
  else if (KeyCode = keyDelete) then
  begin
    Consumed := True;
    DeleteNote(NotesListBox.FocusItem);
  end;
end;

procedure TMainForm.NotesListBoxDoubleClick(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  GotoCurrentNote;
end;

procedure TMainForm.btnNotesAddClick(Sender: TObject);
begin
  AddNote;
end;

procedure TMainForm.btnNotesDelClick(Sender: TObject);
begin
  if NotesListBox.FocusItem = -1 then
    exit;
  DeleteNote(NotesListBox.FocusItem);
end;

procedure TMainForm.btnNotesEditClick(Sender: TObject);
begin
  if NotesListBox.FocusItem = -1 then
    exit;
  EditNote(NotesListBox.FocusItem);
end;

procedure TMainForm.UpdateRichViewFromSettings;
begin
  RichView.RichTextSettings.NormalFont := fpgGetFont(Settings.NormalFontDesc);
  RichView.RichTextSettings.FixedFont := fpgGetFont(Settings.FixedFontDesc);
  RichView.ScrollDistance := Settings.ScrollDistance;
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

procedure TMainForm.btnBookmarkClick(Sender: TObject);
begin
  AddBookmark;
end;

procedure TMainForm.RichViewClickLink(Sender: TRichTextView; Link: string);
var
  LinkDetails: TfpgString;
  LinkIndex: integer;
  lLink: THelpLink;
  lHelp: THelpFile;
  f: THelpFile;
  lHelpFileName: TfpgString;
  i: integer;
  lTopic: TTopic;
  lFound: Boolean;
  lURL: TfpgString;
  lNoteIndex: integer;
begin
  if pos(PARAM_LINK_NOTE, Link) > 0 then
  begin
    lNoteIndex := StrToInt(StrRightFrom(Link, 5));
    NotesListBox.FocusItem := lNoteIndex;
    EditNote(lNoteIndex);
  end
  else if pos(PARAM_LINK_PROGRAM, Link) > 0 then
  begin
    TfpgMessageDialog.Warning('', 'Program links are not supported in DocView yet. Please try again with a later build.')
  end
  else if pos(PARAM_LINK_EXTERNAL, Link) > 0 then
  begin
    LinkDetails := StrRightFrom( Link, 10 );    // 10 is starting pos of data, after 'external '
    LinkIndex := StrToInt( ExtractNextValue( LinkDetails, ' ' ) );
    lHelp := CurrentTopic.HelpFile as THelpFile;

    lHelpFileName := lHelp.ReferencedFiles[ LinkIndex ];

    { Only open the external file once. So see if it is already openned. }
    lFound := False;
    for i := 0 to CurrentOpenFiles.Count-1 do
    begin
      f := THelpFile(CurrentOpenFiles[i]);
      if SameText(fpgExtractFileName(f.Filename), lHelpFileName) then
        lFound := True;
    end;
    if not lFound then
    begin
      OpenAdditionalFile := True;
      OpenFile(lHelpFileName, '', false);
      OpenAdditionalFile := False;
    end;

    { Not sure if we have an ID or Resource Name, so lets try both if possible }
    if TryStrToInt(LinkDetails, i) then
      DisplayTopicByResourceID(i)
    else
      DisplayTopicByName(LinkDetails);
  end
  else if pos(PARAM_LINK_URL, Link) > 0 then
  begin
    // we have an external URL of some kind
    // format is always:  'url "<uri>"'
    lURL := StringReplace(Link, 'url "', '', []);
    lURL := UTF8Copy(lURL, 0, UTF8Length(lURL)-1);
    fpgOpenURL(lURL);
  end
  else
  begin
    // we have a internal INF file link
    LinkIndex := StrToInt( Link );
    lLink     := THelpLink(CurrentTopic.Links[LinkIndex]);
    lTopic    := FindTopicForLink(lLink);

    if lTopic <> nil then
      DisplayTopic(lTopic);
    { TODO: we need to implement the remained of link support }
    //-----------------------
    exit; //==>

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
        break;
      end;
      if lFound then
        break;
    end;

    if lTopic <> nil then
      DisplayTopic(lTopic);
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
var
  lFilename: TfpgString;
begin
  bvlBody.Realign;

  // restore previous window position and size
  gINI.ReadFormState(self);
  PageControl1.Width := gINI.ReadInteger('Options', 'SplitterLeft', 260);
  UpdateWindowPosition;

  CreateMRUMenuItems;
  ProcessCommandLineParams;

  RichView.Images := FImages;
  UpdateRichViewFromSettings;

  if ParamCount = 0 then
  begin
    // user hasn't requested any particular file
    // at startup, so if the option is still set,
    // load the DocView help file
    if Settings.StartupHelp then
    begin
      lFilename := GetOwnHelpFileName;
      if FileExists(lFilename) then
        OpenFile(lFilename, '', true);
    end;
  end;
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
  OpenAdditionalFile := False;
  FileOpen;
end;

procedure TMainForm.miFileOpenAdditionalFileClicked(Sender: TObject);
begin
  OpenAdditionalFile := True;
  FileOpen;
  OpenAdditionalFile := False;
end;

procedure TMainForm.miFileOpenSpecialClicked(Sender: TObject);
var
  s: TfpgString;
begin
  OpenAdditionalFile := False;
  if fpgInputQuery('Open Special...', 'Enter Environment Variable or Directory Path to open', s) then
  begin
    if s <> '' then
      OpenFile(s, '', True);
  end;
end;

procedure TMainForm.miFileCloseClicked(Sender: TObject);
begin
  CloseFile;
end;

procedure TMainForm.miActionsContentsClicked(Sender: TObject);
begin
  PageControl1.ActivePage := tsContents;
end;

procedure TMainForm.miActionsIndexClicked(Sender: TObject);
begin
  PageControl1.ActivePage := tsIndex;
end;

procedure TMainForm.miActionsSearchClicked(Sender: TObject);
begin
  PageControl1.ActivePage := tsSearch;
end;

procedure TMainForm.miActionsNotesClicked(Sender: TObject);
begin
  PageControl1.ActivePage := tsNotes;
end;

procedure TMainForm.miActionsHistoryClicked(Sender: TObject);
begin
  PageControl1.ActivePage := tsHistory;
end;

procedure TMainForm.miConfigureClicked(Sender: TObject);
begin
  ShowConfigForm;
  UpdateRichViewFromSettings;
end;

procedure TMainForm.miViewExpandAllClicked(Sender: TObject);
begin
  tvContents.FullExpand;
end;

procedure TMainForm.miViewCollapseAllClicked(Sender: TObject);
begin
  tvContents.FullCollapse;
end;

procedure TMainForm.miOpenBookmarksMenuClicked(Sender: TObject);
var
  frm: TBookmarksForm;
begin
  frm := TBookmarksForm.Create(nil);
  try
    frm.BookmarkList := Bookmarks;
    frm.OnGotoBookmark := @NavigateToBookmark;
    frm.OnBookmarksChanged := @OnBookmarksChanged;
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TMainForm.miBookmarksMenuItemClicked(Sender: TObject);
var
  t: PtrInt;
  MenuItem: TfpgMenuItem;
  Bookmark: TBookmark;
begin
  MenuItem:= Sender as TfpgMenuItem;
  t:= MenuItem.Tag;
  Bookmark := TBookmark(Bookmarks[t]);
  NavigateToBookmark( Bookmark );
end;

procedure TMainForm.miHelpProdInfoClicked(Sender: TObject);
var
  s: TfpgString;
begin
  s :=  cShortName + '  -  ' + cLongName + LineEnding + LineEnding
      + cCreatedBy + LineEnding
      + cVersion + '  -  ' +  {$I %date%} + ' ' + {$I %time%};

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
      Add('<b><u>Filename:</u></b> <blue>' + f.Filename + '</blue>');
      Add('<b>Title:</b> ' + f.Title);
      Add('<b>File size:</b> ' + IntToStr(fpgFileSize(f.Filename)) + ' bytes');
      Add('<b>INF/HLP file version</b> ' + f.FormatVersion);
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

procedure TMainForm.miToolsFindByResourceID(Sender: TObject);
var
  ResourceIDString: string;
  ResourceID: word;
begin
  if not fpgInputQuery('Find Resource ID', 'Enter the resource ID to find', ResourceIDString) then
    exit;
  try
    ResourceID := StrToInt(ResourceIDString);
  except
    TfpgMessageDialog.Critical('Find Resource ID', 'Invalid resource ID entered');
    exit;
  end;

  if not DisplayTopicByResourceID( ResourceID ) then
    TfpgMessageDialog.Critical('Find Resource ID', 'Resource ID not found');
end;

procedure TMainForm.miToolsFindTopifByName(Sender: TObject);
var
  TopicNameString: string;
Begin
  if not fpgInputQuery( 'Find Topic By Name',
                       'Enter the topic name to search for',
                       TopicNameString ) then
    exit;

  if not DisplayTopicByName( TopicNameString ) then
    if not DisplayTopicByGlobalName( TopicNameString ) then
      TfpgMessageDialog.Critical( 'Find Topic By Name', 'Topic name not found' );
end;

procedure TMainForm.miToolsShowEnvVariablesClicked(Sender: TObject);
  function LGetEnvVarValue(const AVariable: string): string;
  begin
    Result := Format('%s = ''%s''', [AVariable, GetEnvironmentVariable(AVariable)]);
  end;
begin
  RichView.Clear;
  RichView.AddParagraph(PChar(LGetEnvVarValue(BookshelfEnvironmentVar)));
  RichView.AddParagraph(PChar(LGetEnvVarValue(HelpPathEnvironmentVar)));
end;

procedure TMainForm.miTopicPropertiesClicked(Sender: TObject);
const
  TopicInfoTitle = 'Topic Information';
  TopicInfoTopicTitle = 'Title: ';
  TopicInfoIndex = 'Index: ';
  TopicInfoFile = 'File:  ';
  TopicInfoResourceIDs = 'Resource IDs:';
  TopicInfoNoResourceIDs = '  (None)';
var
  Topic: TTopic;
  HelpFile: THelpFile;
  ResourceIDs: TList;
  i: longint;
  sl: TStringList;
begin
  if CurrentTopic = nil then
    Exit;
  Topic := CurrentTopic;
  HelpFile := Topic.HelpFile as THelpFile;

  ResourceIDs := TList.Create;
  HelpFile.FindResourceIDsForTopic(Topic, ResourceIDs);
  sl := TStringList.Create;
  try
    with sl do
    begin
      Clear;
      Add( TopicInfoTitle );
      Add( TopicInfoTopicTitle + Topic.Title );
      Add( TopicInfoIndex + IntToStr( Topic.Index ) );
      Add( TopicInfoFile + HelpFile.Filename );
      Add( TopicInfoResourceIDs );
      for i := 0 to ResourceIDs.Count - 1 do
        Add( '  ' + IntToStr( longint( ResourceIDs[ i ] ) ) );
      if ResourceIDs.Count = 0 then
        Add( TopicInfoNoResourceIDs );
    end;
    ResourceIDs.Destroy;
    ShowText(TopicInfoTitle, sl.Text);
  finally
    sl.Free;
  end;
End;

procedure TMainForm.miDumpDictionaryClicked(Sender: TObject);
var
  i: integer;
  j: integer;
  f: THelpFile;
  sl: TStringList;
begin
  for i := 0 to CurrentOpenFiles.Count-1 do
  begin
    f := THelpFile(CurrentOpenFiles[i]);
    sl := TStringList.Create;
    for j := 0 to f.DictionaryCount-1 do
    begin
      sl.Add('"' + f.DictionaryWords[j] + '"');
    end;
    sl.SaveToFile(GetTempDir + fpgExtractFileName(f.Filename) + '.dictionary');
  end;
  sl.Free;
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

procedure TMainForm.cbEncodingChanged(Sender: TObject);
begin
  Settings.Encoding := TFontEncoding(cbEncoding.FocusItem);
  DisplayTopic(CurrentTopic);
end;

procedure TMainForm.btnNotesGotoClicked(Sender: TObject);
begin
  GotoCurrentNote;
end;

procedure TMainForm.UpdateEncodingComboBox;
begin
  cbEncoding.FocusItem := Ord(Settings.Encoding);
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

procedure TMainForm.pnlTitleGradientPaint(Sender: TObject);
begin
  pnlTitle.Canvas.GradientFill(pnlTitle.GetClientRect, TfpgColor($ff4466d9),
      TfpgColor($ff63a0fe), gdHorizontal);
  pnlTitle.Canvas.DrawText(pnlTitle.Margin, 3, pnlTitle.Text);
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
  if Topic <> nil then
    DisplayTopic(Topic);
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
  if n = nil then
    Exit;
  while n.Parent <> nil do
  begin
    s := n.Parent.Text + sep + s;
    n := n.Parent;
    sep := ' > ';
  end;
  SetStatus(s);
  UpdateTitlePanel;
end;

procedure TMainForm.UpdateTitlePanel;
begin
  pnlTitle.Text := CurrentTopic.Title;
end;

procedure TMainForm.EnableControls;
begin
  { TODO: lots more need to be added here }
  EnableNotesControls;
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
  begin
    ClearAllWordSequences;
    exit;
  end;

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
    SetStatus( Format(rsDVSearchFoundMsg, [lbSearchResults.Items.Count])
        + StrInDoubleQuotes(SearchText));
  end
  else
  begin
    lbSearchResults.Items.Add( Format(rsDVNoMatchesFound, [SearchText]));
//    RefreshWindows( Windows ); // update to remove old highlights
    SetStatus('');
  end;

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
  for FileIndex := 0 to NewFiles.Count-1 do
  begin
    HelpFile := THelpFile(NewFiles[FileIndex]);
    { TODO -ograeme : implement global filelist support }
//    GlobalFilelist.AddFile( HelpFile.Filename, Frame.Handle );
    // LoadNotes( HelpFile );
    LoadBookmarks( HelpFile );
  end;

  UpdateNotesDisplay;

  { TODO -ograeme : bookmarks }
  BuildBookmarksMenu;
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
      Settings.Encoding := HelpFile.Encoding;
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
  UpdateEncodingComboBox;
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
  if not OpenAdditionalFile then
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
  SaveBookmarks;
  SaveNotes;

  ClearIndexComponents;
  ClearAllWordSequences;

  // Now destroy help files
  for FileIndex := 0 to CurrentOpenFiles.Count - 1 do
  begin
    lHelpFile := THelpFile(CurrentOpenFiles[FileIndex]);
    lHelpFile.Free;
  end;

  CurrentOpenFiles.Clear;
  ClearBookmarks;
  ClearNotes;
end;

procedure TMainForm.OnHelpFileLoadProgress(n, outof: integer; AMessage: string);
begin
  //
end;

procedure TMainForm.LoadNotesForFile(AHelpFile: THelpFile);
var
  NotesFileName: TfpgString;
  TopicIndex: longint;
  InsertPoint: longint;
  Note: THelpNote;

  NotesFile: TStringList;

  Paragraph: TfpgString;
  NotEOF: boolean;
  NoteText: TfpgString;
  i: integer;
  s: TfpgString;
  lReadTopicIndex: boolean;
  lReadInsertPoint: boolean;
  lNoteTextStarted: boolean;
begin
  ProfileEvent( 'Load notes for ' + AHelpFile.Filename );

  if AHelpFile.NotesLoaded then
    exit;

  AHelpFile.NotesLoaded := true;
  NotesFileName := fpgChangeFileExt(AHelpFile.FileName, NOTES_FILE_EXTENSION);

  if not fpgFileExists(NotesFileName) then
    exit; // no notes

  NotesFile := TStringList.Create;
  if NotesFile = nil then
  begin
    TfpgMessageDialog.Critical('Error', 'Error opening file: ' + NotesFileName);
    exit;
  end;
  NotesFile.LoadFromFile(NotesFileName);

  NoteText := '';
  NotEOF := true;
  i := 0;
  repeat
    { reset variables }
    lNoteTextStarted := False;
    TopicIndex := -1;
    InsertPoint := -1;
    NoteText := '';

    { Read the topic index }
    s := NotesFile[i];
    try
      TopicIndex := StrToInt(s);
    except
      TopicIndex := -1;
    end;
    inc(i);

    { Read the insert point }
    s := NotesFile[i];
    try
      InsertPoint := StrToInt(s);
    except
      InsertPoint := -1;
    end;
    inc(i);

    { Read note text until end marker is found }
    repeat
      s := NotesFile[i];
      inc(i);
      if s = '#ENDNOTE#' then
      begin
        // found end of note
        if (TopicIndex >= 0) and (InsertPoint >= 0) then
        begin
          Note := THelpNote.Create;
          Note.Topic := AHelpFile.Topics[TopicIndex];
          Note.InsertPoint := InsertPoint;
          Note.Text := NoteText;
          Notes.Add(Note);
        end;
        break;
      end
      else
      begin
        if lNoteTextStarted then
          NoteText := NoteText + LineEnding + s
        else
          NoteText := s;
        lNoteTextStarted := True;
      end;
    until s = '#ENDNOTE#';

  until i >= NotesFile.Count;

  NotesFile.Free;
  UpdateNotesDisplay;
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
    tvContents.FullExpand;
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
    for i := 0 to tmpIndexLists.Count - 1 do
    begin
      tmpIndex := TStringList(tmpIndexLists.Items[i]);
      if IndexNextIndex[i] < tmpIndex.Count then
      begin
        // list is not yet finished, get next entry
        ListEntry := tmpIndex.Strings[ IndexNextIndex[i] ];
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
  DestroyListObjects( Notes );
  Notes.Clear;
end;

procedure TMainForm.ClearIndexComponents;
begin
  IndexSearchEdit.Clear;
  lbIndex.FocusItem := -1;
  lbIndex.Items.Clear;
  DisplayedIndex.Clear;
  IndexLoaded := False;
end;

procedure TMainForm.CorrectNotesPositions(Topic: TTopic; AText: pchar);
var
  NoteIndex: longint;
  Note: THelpNote;
  p: pchar;
  NextP: pchar;
  Element: TTextElement;
  TextIndex: longint;
begin
  NoteIndex := 0;
  for NoteIndex := 0 to Notes.Count-1 do
  begin
    Note := THelpNote(Notes[NoteIndex]);
    if Note.Topic = Topic then
    begin
      // this note belongs to the specified topic.
      p := AText;

      while true do
      begin
        Element := ExtractNextTextElement( p, NextP );
        if Element.ElementType = teTextEnd then
          break;
        TextIndex := PCharDiff( p, AText );
        if TextIndex >= Note.InsertPoint then
        begin
          // found a safe point to insert
          if TextIndex <> Note.InsertPoint then
          begin
            // correct it.
            Note.InsertPoint := TextIndex;
          end;
          break;
        end;

        p := NextP;
      end;
    end;
  end;
end;

procedure TMainForm.InsertNotesIntoTopicText(ATopic: TTopic; var AText: TfpgString);
var
  NoteIndex: longint;
  Note: THelpNote;
  ActualInsertPoint: longword;
begin
  CorrectNotesPositions( ATopic, PChar(AText) );

  for NoteIndex := 0 to Notes.Count - 1 do
  begin
    Note := THelpNote(Notes[NoteIndex]);
    if Note.Topic = ATopic then
    begin
      // Adjust insert point for any notes we have already inserted.
      ActualInsertPoint := FindActualNoteCharIndex( Note.InsertPoint,
                                                    NoteIndex,
                                                    ATopic );
      RefreshNoteInsertInfo( NoteIndex );
      // DON'T USE UTF8Insert() HERE - THE OFFSET IS IN BYTES, NOT CHARACTERS!
      Insert(Note.InsertText, AText, ActualInsertPoint);
    end;
  end;
end;

function TMainForm.FindOriginalNoteCharIndex(NoteCharIndex: longword; Topic: TTopic): longword;
var
  NoteIndex: longint;
  Note: THelpNote;
begin
  Result := NoteCharIndex;
  for NoteIndex := 0 to Notes.Count-1 do
  begin
    Note := THelpNote(Notes[NoteIndex]);
    if Note.Topic = Topic then
      if Note.InsertPoint < NoteCharIndex then
        dec(Result, UTF8Length(Note.InsertText));
  end;
end;

function TMainForm.FindActualNoteCharIndex(NoteCharIndex: longword;
    MaxNoteIndex: longword; Topic: TTopic): longword;
var
  NoteIndex: longint;
  Note: THelpNote;
begin
  NoteIndex := 0;
  Result := NoteCharIndex;
  for NoteIndex := 0 to MaxNoteIndex-1 do
  begin
    Note := THelpNote(Notes[NoteIndex]);
    if Note.Topic = Topic then
      if Note.InsertPoint < NoteCharIndex then
        inc(Result, UTF8Length(Note.InsertText));
  end;
end;

procedure TMainForm.RefreshNoteInsertInfo(NoteIndex: longword);
var
  Note: THelpNote;
begin
  Note :=  THelpNote(Notes[ NoteIndex ]);
  if Note.Topic = nil then
    exit;
  with Note do
  begin
    InsertText := '<color #'
                   + IntToHex( Settings.Colors[ NotesTextColorIndex ], 6 )
                   + '><link note' + IntToStr( NoteIndex ) + '>';
    InsertText := InsertText + Text;
    InsertText := InsertText + '</link></color>';
  end;
end;

procedure TMainForm.SaveNotesForFile(AHelpFile: THelpFile);
var
  NotesFileName: TfpgString;
  FileNoteCount: integer;
  NoteIndex: integer;
  Note: THelpNote;
  NotesFile: TStringList;
  TopicIndex: integer;
  s: TfpgString;
begin
  ProfileEvent('Save notes for ' + AHelpFile.Filename);
  if not AHelpFile.NotesLoaded then
    // we never loaded the notes/displayed a topic from this file
    // so don't do anything.
    exit;

  ProfileEvent('Really saving');
  NotesFileName := fpgChangeFileExt(AHelpFile.FileName, NOTES_FILE_EXTENSION);

  FileNoteCount := 0;
  for  NoteIndex := 0 to Notes.Count-1 do
  begin
    Note := THelpNote(Notes[NoteIndex]);
    if Note.Topic.HelpFile = AHelpFile then
      inc(FileNoteCount);
  end;

  if FileNoteCount = 0 then
  begin
    // no notes. delete notes file if it already exists.
    if fpgFileExists( NotesFileName ) then
      fpgDeleteFile( NotesFileName );
    exit;
  end;

  NotesFile := TStringList.Create;

  for NoteIndex := 0 to Notes.Count-1 do
  begin
    Note := THelpNote(Notes[ NoteIndex ]);

    if Note.Topic.HelpFile <> AHelpFile then
      continue;

    TopicIndex := AHelpFile.IndexOfTopic(Note.Topic);

    NotesFile.Add(IntToStr(TopicIndex));
    NotesFile.Add(IntToStr(Note.InsertPoint));
    NotesFile.Add(Note.Text);
    NotesFile.Add('#ENDNOTE#');
  end;

  NotesFile.SaveToFile(NotesFileName);
  NotesFile.Free;
end;

procedure TMainForm.AddNote;
var
  Note: THelpNote;
  NoteForm: TNoteForm;
begin
  { check that the note position isn't within a note already }
  if RichView.LinkFromIndex(RichView.CursorIndex) <> '' then
  begin
    TfpgMessageDialog.Critical('Error', 'You can''t add a note within a link or another note' );
    exit;
  end;

  NoteForm := TNoteForm.Create(nil);
  NoteForm.Text := '';
  NoteForm.CanDelete := False;
  if NoteForm.ShowModal <> mrOK then
  begin
    NoteForm.Free;
    exit;
  end;

  // store note data
  Note := THelpNote.Create;
  Note.Text := NoteForm.Text;
  NoteForm.Free;

  // compensate for existing notes
  if RichView.CursorIndex <> -1 then
    Note.InsertPoint := FindOriginalNoteCharIndex(RichView.CursorIndex, CurrentTopic)
  else
    Note.InsertPoint := 0;
  Note.Topic := CurrentTopic;
  Notes.Add(Note);

  // redisplay topic
  DisplayTopic(CurrentTopic);
  //DisplayTopicInWindow( Window,
  //                      false, // don't follow links!
  //                      true ); // keep position

  //RichView.SelectionStart := FindActualNoteCharIndex(Note.InsertPoint,
  //                                                      Notes.Count - 1,
  //                                                      CurrentTopic);
  UpdateNotesDisplay;

  SaveNotes;
end;

procedure TMainForm.EditNote(ANoteIndex: longint);
var
  Note: THelpNote;
  NoteForm: TNoteForm;
begin
  Note := THelpNote(Notes[ANoteIndex]);
  if Note = nil then
    exit;
  NoteForm := TNoteForm.Create(nil);
  try
    NoteForm.Text := Note.Text;
    NoteForm.CanDelete := True;

    if NoteForm.ShowModal = mrCancel then
      exit;

    if NoteForm.ModalResult = mrAbort then
    begin
      DeleteNote(ANoteIndex);
      exit;
    end;

    Note.Text := NoteForm.Text;
    SaveNotes;
    DisplayTopic(CurrentTopic);
    UpdateNotesDisplay;
  finally
    NoteForm.Free;
  end;
end;

procedure TMainForm.DeleteNote(ANoteIndex: longint);
var
  Note: THelpNote;
begin
  if TfpgMessageDialog.Question(rsconfirmation, 'Are you sure you want to delete the seleted Note?') <> mbYes then
    Exit;
  { if we got here, we must delete the note }
  Note := THelpNote(Notes[ANoteIndex]);
  Notes.Delete(ANoteIndex);
  Note.Free;

  DisplayTopic(CurrentTopic);
  UpdateNotesDisplay;

  SaveNotes;
end;

procedure TMainForm.SaveNotes;
var
  FileIndex: integer;
  HelpFile: THelpFile;
begin
  ProfileEvent( 'Save notes' );
  for FileIndex := 0 to CurrentOpenFiles.Count-1 do
  begin
    HelpFile := THelpFile(CurrentOpenFiles[FileIndex]);
    SaveNotesForFile(HelpFile);
  end;
end;

procedure TMainForm.GotoCurrentNote;
var
  Note: THelpNote;
begin
  if NotesListBox.FocusItem = -1 then
    exit;
  Note := NotesListBox.Items.Objects[NotesListBox.FocusItem] as THelpNote;
  DisplayTopic(Note.Topic);
end;

procedure TMainForm.UpdateNotesDisplay;
var
  NoteIndex: longint;
  Note: THelpNote;
  NoteTitle: string;
begin
  NotesListBox.Items.Clear;
  for NoteIndex := 0 to Notes.Count-1 do
  begin
    Note := THelpNote(Notes[NoteIndex]);
    if Note.Topic <> nil then
      NoteTitle := Note.Topic.Title
    else
      NoteTitle := StrLeft(Note.Text, 100);
    NotesListBox.Items.AddObject(NoteTitle, Note);
  end;
  EnableNotesControls;
end;

procedure TMainForm.EnableNotesControls;
var
  NoteSelected: boolean;
begin
  NoteSelected := NotesListBox.FocusItem <> -1;
  btnNotesEdit.Enabled := NoteSelected;
  btnNotesGoto.Enabled := NoteSelected;
  btnNotesDel.Enabled := NoteSelected;
  btnNotesAdd.Enabled := CurrentOpenFiles.Count > 0;
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
    THelpFile(CurrentTopic.HelpFile).GetImages(ImageIndices, FImages);
  end;

  ImageIndices.Free;

  // apply encoding conversion
  case Settings.Encoding of
    encUTF8:      lText := IPFToUTF8(lText);
    encCP437:     lText := CP437ToUTF8(lText);
    encCP850:     lText := CP850ToUTF8(lText);
    encIBMGraph:  lText := IBMGraphToUTF8(lText);
  else
    lText := IPFToUTF8(lText);
  end;

  { Load and insert annotations / notes }
  if not HelpFile.NotesLoaded then
    LoadNotesForFile(HelpFile);
  InsertNotesIntoTopicText(Topic, lText);

  RichView.AddText(PChar(lText));

  if CurrentTopic.ShowInContents then
  begin
  tvContents.Selection := tvContents.RootNode.FindSubNode(CurrentTopic, True);
  tvContents.Invalidate;
  end
  else
  begin
    tvContents.Selection := nil;
    tvContents.Invalidate;
  end;
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
  fpgApplication.HelpFile := cDocViewHelpFile;
  OnShow  := @MainFormShow;
  OnDestroy := @MainFormDestroy;
//  Files := TList.Create;
  AllFilesWordSequences := TList.Create;
  CurrentOpenFiles := TList.Create;
  DisplayedIndex := TStringList.Create;
  Notes := TList.Create;
  Bookmarks := TList.Create;
  BookmarksMenuItems := TList.Create;
  CurrentHistoryIndex := -1;
  FHistorySelection := False;
  OpenAdditionalFile := False;
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

  fpgImages.AddMaskedBMP(
    'dv.notegreen', @usr_notegreen,
    sizeof(usr_notegreen), 0, 0);


  // load custom user settings like Fonts, Search Highlight Color etc.
  LoadSettings;
end;

destructor TMainForm.Destroy;
begin
  RichView.Images := nil;
  FImages.Free;
  CurrentTopic := nil;  // it was a reference only
  FFileOpenRecent := nil;   // it was a reference only
  miOpenRecentMenu.Free;
//  DestroyListAndObjects(Files);
  ClearBookmarks;
  Bookmarks.Free;
  DestroyListAndObjects(BookmarksMenuItems);
  DestroyListAndObjects(Notes);
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
  MinWidth := 430;
  MinHeight := 300;
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
    SetPosition(2, 2, 260, 306);
    Align := alLeft;
    Hint := '';
    TabOrder := 0;
    MinWidth := 120;
    OnChange  := @PageControl1Change;
  end;

  tsContents := TfpgTabSheet.Create(PageControl1);
  with tsContents do
  begin
    Name := 'tsContents';
    SetPosition(3, 24, 254, 279);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := 'Contents';
  end;

  tvContents := TfpgTreeView.Create(tsContents);
  with tvContents do
  begin
    Name := 'tvContents';
    SetPosition(4, 32, 242, 242);
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
    SetPosition(3, 24, 254, 279);
    Anchors := [anLeft,anRight,anTop,anBottom];
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
    SetPosition(4, 32, 242, 242);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#List';
    Hint := '';
    TabOrder := 1;
    OnDoubleClick  := @lbIndexDoubleClick;
    OnKeyPress := @lbIndexKeyPress;
  end;

  IndexSearchEdit := TfpgEdit.Create(tsIndex);
  with IndexSearchEdit do
  begin
    Name := 'IndexSearchEdit';
    SetPosition(4, 4, 152, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 2;
    Text := '';
    OnChange := @IndexSearchEditOnChange;
    OnKeyPress :=@IndexSearchEditKeyPress;
  end;

  tsSearch := TfpgTabSheet.Create(PageControl1);
  with tsSearch do
  begin
    Name := 'tsSearch';
    SetPosition(3, 24, 254, 279);
    Anchors := [anLeft,anRight,anTop,anBottom];
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
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 1;
    Text := '';
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
    Enabled := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 3;
    Text := 'This section';
  end;

  RadioButton2 := TfpgRadioButton.Create(tsSearch);
  with RadioButton2 do
  begin
    Name := 'RadioButton2';
    SetPosition(12, 88, 192, 20);
    Enabled := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 4;
    Text := 'Marked sections';
  end;

  RadioButton3 := TfpgRadioButton.Create(tsSearch);
  with RadioButton3 do
  begin
    Name := 'RadioButton3';
    SetPosition(12, 108, 192, 20);
    Checked := True;
    Enabled := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 5;
    Text := 'All sections';
  end;

  RadioButton4 := TfpgRadioButton.Create(tsSearch);
  with RadioButton4 do
  begin
    Name := 'RadioButton4';
    SetPosition(12, 128, 192, 20);
    Enabled := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 6;
    Text := 'Index';
  end;

  RadioButton5 := TfpgRadioButton.Create(tsSearch);
  with RadioButton5 do
  begin
    Name := 'RadioButton5';
    SetPosition(12, 148, 192, 20);
    Enabled := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 7;
    Text := 'Marked libraries';
  end;

  RadioButton6 := TfpgRadioButton.Create(tsSearch);
  with RadioButton6 do
  begin
    Name := 'RadioButton6';
    SetPosition(12, 168, 192, 20);
    Enabled := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 8;
    Text := 'All libraries';
  end;

  lbSearchResults := TfpgListBox.Create(tsSearch);
  with lbSearchResults do
  begin
    Name := 'lbSearchResults';
    SetPosition(4, 220, 242, 54);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#List';
    Hint := '';
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
    SetPosition(3, 24, 254, 279);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := 'Notes';
  end;

  NotesListBox := TfpgListBox.Create(tsNotes);
  with NotesListBox do
  begin
    Name := 'NotesListBox';
    SetPosition(4, 32, 242, 242);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#List';
    Hint := '';
    TabOrder := 0;
    OnDoubleClick  := @NotesListBoxDoubleClick;
    OnKeyPress  := @NotesListBoxKeyPress;
    OnChange  := @NotesListBoxChange;
  end;

  btnNotesAdd := TfpgButton.Create(tsNotes);
  with btnNotesAdd do
  begin
    Name := 'btnNotesAdd';
    SetPosition(4, 4, 24, 24);
    Text := '';
    Enabled := False;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'stdimg.add';
    TabOrder := 1;
    OnClick  := @btnNotesAddClick;
  end;

  btnNotesEdit := TfpgButton.Create(tsNotes);
  with btnNotesEdit do
  begin
    Name := 'btnNotesEdit';
    SetPosition(32, 4, 24, 24);
    Text := '';
    Enabled := False;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'stdimg.edit';
    TabOrder := 2;
    OnClick := @btnNotesEditClick;
  end;

  btnNotesDel := TfpgButton.Create(tsNotes);
  with btnNotesDel do
  begin
    Name := 'btnNotesDel';
    SetPosition(60, 4, 24, 24);
    Text := '';
    Enabled := False;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'stdimg.remove';
    TabOrder := 3;
    OnClick := @btnNotesDelClick;
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
    OnClick := @btnNotesGotoClicked;
  end;

  tsHistory := TfpgTabSheet.Create(PageControl1);
  with tsHistory do
  begin
    Name := 'tsHistory';
    SetPosition(3, 24, 254, 249);
    Anchors := [anLeft,anRight,anTop,anBottom];
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
    TabOrder := 0;
    OnDoubleClick := @lbHistoryDoubleClick;
    OnKeyPress := @lbHistoryKeyPress;
  end;

  Splitter1 := TfpgSplitter.Create(bvlBody);
  with Splitter1 do
  begin
    Name := 'Splitter1';
    SetPosition(262, 2, 8, 306);
    Align := alLeft;
    OnDoubleClick :=@Splitter1DoubleClicked;
  end;

  bvlContentArea := TfpgBevel.Create(bvlBody);
  with bvlContentArea do
  begin
    Name := 'bvlContentArea';
    SetPosition(270, 2, 381, 306);
    Align := alClient;
    Hint := '';
    Shape := bsSpacer;
  end;

  pnlTitle := TfpgPanel.Create(bvlContentArea);
  with pnlTitle do
  begin
    Name := 'pnlTitle';
    SetPosition(2, 2, 377, 20);
    Align := alTop;
    Alignment := taLeftJustify;
    BackgroundColor := TfpgColor($559DD4);
    FontDesc := '#Label2';
    Hint := '';
    Margin := 15;
    Style := bsFlat;
    Text := 'Panel';
    TextColor := TfpgColor($FFFFFF);
    OnPaint:=@pnlTitleGradientPaint;
  end;

  RichView := TRichTextView.Create(bvlContentArea);
  with RichView do
  begin
    Name := 'RichView';
    SetPosition(77, 188, 244, 92);
    TabOrder := 2;
    Align := alClient;
    OnOverLink  := @RichViewOverLink;
    OnNotOverLink  := @RichViewNotOverLink;
    OnClickLink := @RichViewClickLink;
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
    AddMenuItem('Open...', rsKeyCtrl+'O', @miFileOpenClicked);
    AddMenuItem('Open additional file...', rsKeyCtrl+rsKeyShift+'O', @miFileOpenAdditionalFileClicked);
    AddMenuItem('Open Special...', rsKeyCtrl+'L', @miFileOpenSpecialClicked);
    AddMenuItem('Save current Topic to IPF...', rsKeyCtrl+'S', @miFileSaveTopicAsIPF);
    AddMenuItem('Close', rsKeyCtrl+'W', @miFileCloseClicked);
    AddSeparator;
    FFileOpenRecent := AddMenuItem('Open Recent...', '', nil);
    AddMenuitem('-', '', nil);
    AddMenuItem('Quit', 'Ctrl+Q', @miFileQuitClicked);
  end;

  miActions := TfpgPopupMenu.Create(self);
  with miActions do
  begin
    Name := 'miActions';
    SetPosition(282, 96, 132, 20);
    AddMenuItem('Contents', 'F5', @miActionsContentsClicked);
    AddMenuItem('Index', 'F6', @miActionsIndexClicked);
    AddMenuItem('Search', 'F7', @miActionsSearchClicked);
    AddMenuItem('Notes', 'F8', @miActionsNotesClicked);
    AddMenuItem('History', 'F9', @miActionsHistoryClicked);
    AddSeparator;
    AddMenuItem('Back', rsKeyCtrl+'Left', @miActionsBackClicked);
    AddMenuItem('Forward', rsKeyCtrl+'Right', @miActionsForwardClicked);
    AddMenuItem('Previous Topic', rsKeyCtrl+'Up', @miActionsPrevTopicClicked);
    AddMenuItem('Next Topic', rsKeyCtrl+'Down', @miActionsNextTopicClicked);
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
    AddMenuItem('Add', rsKeyCtrl+'B', @btnBookmarkClick);
    AddMenuItem('Edit...', rsKeyCtrl+'D', @miOpenBookmarksMenuClicked);
    AddSeparator;
    AddMenuItem('Add note at cursor position', rsKeyCtrl+'M', @btnNotesAddClick);
  end;

  miView := TfpgPopupMenu.Create(self);
  with miView do
  begin
    Name := 'miView';
    SetPosition(292, 216, 132, 20);
    AddMenuItem('Expand All', '', @miViewExpandAllClicked);
    AddMenuItem('Collapse All', '', @miViewCollapseAllClicked);
    AddSeparator;
    AddMenuItem('Topic Properties', '', @miTopicPropertiesClicked);
  end;

  miTools := TfpgPopupMenu.Create(self);
  with miTools do
  begin
    Name := 'miTools';
    SetPosition(428, 96, 120, 20);
    AddMenuItem('Show file info', '', @miDebugHeader);
    AddMenuItem('Find topic by resource ID', '', @miToolsFindByResourceID);
    AddMenuItem('Find topic by resource name', '', @miToolsFindTopifByName);
    miDebugHexInfo := AddMenuItem('Toggle hex INF values in contents', '', @miDebugHex);
    AddMenuItem('View source of RichView component', '', @ViewSourceMIOnClick);
    AddMenuItem('Current topic properties', '', @miTopicPropertiesClicked);
    AddMenuItem('Dump dictionary to file in temp directory', '', @miDumpDictionaryClicked);
    AddMenuItem('Show DocView used environment variables', '', @miToolsShowEnvVariablesClicked);
  end;

  miHelp := TfpgPopupMenu.Create(self);
  with miHelp do
  begin
    Name := 'miHelp';
    SetPosition(292, 168, 132, 20);
    AddMenuItem('Help using DocView', rsKeyCtrl+'F1', @miHelpUsingDocView);
    AddMenuItem('Command line parameters', rsKeyCtrl+rsKeyShift+'F1', @miHelpCmdLineParams);
    AddSeparator;
    AddMenuItem('About fpGUI Toolkit...', '', @miHelpAboutFPGui);
    AddMenuItem('Product Information...', '', @miHelpProdInfoClicked);
  end;

  ToolBar := TfpgBevel.Create(self);
  with ToolBar do
  begin
    Name := 'ToolBar';
    SetPosition(0, 25, 654, 28);
    Anchors := [anLeft,anRight,anTop];
    Hint := '';
    Shape := bsBottomLine;
    Style := bsLowered;
  end;

  btnQuit := TfpgButton.Create(ToolBar);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(4, 1, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Quit the application';
    ImageMargin := -1;
    ImageName := 'stdimg.quit';
    ImageSpacing := 0;
    TabOrder := 8;
    OnClick := @miFileQuitClicked;
    Focusable := False;
  end;

  btnOpen := TfpgButton.Create(ToolBar);
  with btnOpen do
  begin
    Name := 'btnOpen';
    SetPosition(30, 1, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Open a new help file';
    ImageMargin := -1;
    ImageName := 'stdimg.open';
    ImageSpacing := 0;
    TabOrder := 0;
    OnClick := @miFileOpenClicked;
    Focusable := False;
  end;

  Bevel1 := TfpgBevel.Create(ToolBar);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(61, 0, 6, 24);
    Hint := '';
    Shape := bsLeftLine;
    Style := bsLowered;
  end;

  btnBack := TfpgButton.Create(ToolBar);
  with btnBack do
  begin
    Name := 'btnBack';
    SetPosition(70, 1, 32, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Previous history item';
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
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Next history item';
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
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Previous Topic';
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
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Next Topic';
    ImageMargin := -1;
    ImageName := 'dv.arrowdown';
    ImageSpacing := 0;
    TabOrder := 5;
    Focusable := False;
    OnClick := @btnNextClick;
  end;

  Bevel2 := TfpgBevel.Create(ToolBar);
  with Bevel2 do
  begin
    Name := 'Bevel2';
    SetPosition(210, 0, 6, 24);
    Hint := '';
    Shape := bsLeftLine;
    Style := bsLowered;
  end;

  btnTBNoteAdd := TfpgButton.Create(ToolBar);
  with btnTBNoteAdd do
  begin
    Name := 'btnTBNoteAdd';
    SetPosition(218, 1, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Add an annotation';
    ImageMargin := -1;
    ImageName := 'dv.notegreen';
    ImageSpacing := 0;
    TabOrder := 12;
    Focusable := False;
    OnClick := @btnTBNoteAddClick;
  end;

  btnBookmark := TfpgButton.Create(ToolBar);
  with btnBookmark do
  begin
    Name := 'btnBookmark';
    SetPosition(244, 1, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Add a bookmark';
    ImageMargin := -1;
    ImageName := 'stdimg.bookmark';
    ImageSpacing := 0;
    TabOrder := 5;
    Focusable := False;
    OnClick := @btnBookmarkClick;
  end;

  Bevel3 := TfpgBevel.Create(ToolBar);
  with Bevel3 do
  begin
    Name := 'Bevel3';
    SetPosition(275, 0, 6, 24);
    Hint := '';
    Shape := bsLeftLine;
    Style := bsLowered;
  end;

  btnHelp := TfpgButton.Create(ToolBar);
  with btnHelp do
  begin
    Name := 'btnHelp';
    SetPosition(283, 1, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Display Product Information';
    ImageMargin := -1;
    ImageName := 'stdimg.about';
    ImageSpacing := 0;
    TabOrder := 6;
    Focusable := False;
    OnClick := @miHelpProdInfoClicked;
  end;

  cbEncoding := TfpgComboBox.Create(ToolBar);
  with cbEncoding do
  begin
    Name := 'cbEncoding';
    SetPosition(524, 2, 124, 22);
    Anchors := [anRight,anTop];
    ExtraHint := '';
    FontDesc := '#List';
    Hint := '';
    Items.Add('UTF-8');
    Items.Add('CP437');
    Items.Add('CP850');
    Items.Add('IBM Graph (cp437)');
    FocusItem := 0;
    TabOrder := 10;
    OnChange  := @cbEncodingChanged;
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
  MainMenu.AddMenuItem('&Actions', nil).SubMenu := miActions;
  MainMenu.AddMenuItem('&Bookmarks', nil).SubMenu := miBookmarks;
  MainMenu.AddMenuItem('&Tools', nil).SubMenu := miTools;
  MainMenu.AddMenuItem('&Help', nil).SubMenu := miHelp;
  FFileOpenRecent.SubMenu := miOpenRecentMenu;

  tvContents.PopupMenu := miView;

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
  n: integer;
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
      showtopic := not gCommandLineParams.IsParam('k');
      { is the first parameter a known docview help, or some addition parameter }
      if Pos(ctiCommandLineParamPrefix, ParamStr(1)) = 1 then
        // command line parameter order seems wrong, so do nothing
      else
        OpenFile(ParamStr(1), '', showtopic);
    end;
  end;

  // now process all other parameters
  if gCommandLineParams.IsParam('k') then
  begin
    { Search for a string }
    edSearchText.Text := gCommandLineParams.GetParam('k');
    PageControl1.ActivePage := tsSearch;
    DoSearch;
  end
  else if gCommandLineParams.IsParam('n') then
  begin
    { Display topic with numeric topic id }
    try
      n := StrToInt(gCommandLineParams.GetParam('n'));
      t := FindTopicByResourceID(n);
      DisplayTopic(t);
    except
      on EConvertError do
        begin
          TfpgMessageDialog.Critical('Invalid Parameter Value',
            '<' + gCommandLineParams.GetParam('n') + '> is not an number.');
        end;
    end;
  end
  else if gCommandLineParams.IsParam('s') then
  begin
    { Display topic with string topic id }
    t := FindTopicByName(gCommandLineParams.GetParam('s'));
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
  s := '<font "' + DefaultTopicFontName + '" 12><b>' + cLongName + '</b></font>' + le
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

function TMainForm.DisplayTopicByResourceID(ID: word): boolean;
var
  Topic: TTopic;
begin
  Topic := FindTopicByResourceID( ID );
  if Topic = nil then
  begin
    Result := false;
    exit;
  end;
  result := true;
  DisplayTopic(Topic);
end;

function TMainForm.DisplayTopicByName(const TopicName: string): boolean;
var
  Topic: TTopic;
begin
  Topic := FindTopicByName(TopicName);
  if Topic = nil then
  begin
    Result := false;
    exit;
  end;
  result := true;
  DisplayTopic(Topic);
end;

function TMainForm.DisplayTopicByGlobalName(const TopicName: string): boolean;
begin
  Result := False;
  // TODO: implement me
end;

procedure TMainForm.ViewSourceMIOnClick(Sender: TObject);
begin
  ShowText('RichView Source Text', RichView.Text);
End;


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

procedure TMainForm.LoadBookmarks(AHelpFile: THelpFile);
var
  Bookmark: TBookmark;
  BookmarksFile: TextFile;
  BookmarksFileName: string;
  s: string;
begin
  ProfileEvent( 'Load bookmarks for ' + AHelpFile.Filename );

  BookmarksFileName := fpgChangeFileExt(AHelpFile.FileName, BOOKMARK_FILE_EXTENSION);

  if not fpgFileExists( BookmarksFileName ) then
    Exit;

  FileMode := fmInput;
  AssignFile( BookmarksFile, BookmarksFileName );
  try
    Reset( BookmarksFile );
    try
      while not Eof( BookmarksFile ) do
      begin
        ReadLn( BookmarksFile, s );
        if Trim( Uppercase( s ) ) = BOOKMARK_SECTION then
        begin
          Bookmark := TBookmark.Load( BookmarksFile, AHelpFile );
          Bookmarks.Add( Bookmark );
        end;
      end;
    finally
      System.Close( BookmarksFile );
    end;
  except
    on e: exception do
      TfpgMessageDialog.Critical('Load Bookmarks', 'Could not load bookmarks: ' + E.Message);
  end;
end;

procedure TMainForm.SaveBookmarks;
var
  FileIndex: integer;
  HelpFile: THelpFile;
begin
  for FileIndex := 0 to CurrentOpenFiles.Count - 1 do
  begin
    HelpFile := THelpFile(CurrentOpenFiles[FileIndex]);
    SaveBookmarksForFile(HelpFile);
  end;
end;

procedure TMainForm.SaveBookmarksForFile(AHelpFile: THelpFile);
var
  i: integer;
  Bookmark: TBookmark;
  BookmarksFile: TextFile;
  BookmarksFileName: TfpgString;
  BookmarkCount: integer;
begin
  ProfileEvent( 'Save bookmarks for ' + AHelpFile.Filename );

  BookmarksFileName:= fpgChangeFileExt(AHelpFile.FileName, BOOKMARK_FILE_EXTENSION);

  BookmarkCount := 0;
  for i := 0 to Bookmarks.Count - 1 do
  begin
    Bookmark := TBookmark(Bookmarks[i]);
    if Bookmark.ContentsTopic.HelpFile = AHelpFile then
      inc( BookmarkCount );
  end;

  if BookmarkCount = 0 then
  begin
    if fpgFileExists( BookmarksFileName ) then
      fpgDeleteFile( BookmarksFileName );
    Exit;
  end;

  AssignFile( BookmarksFile, BookmarksFileName );
  try
    Rewrite( BookmarksFile );
    try
      for i := 0 to Bookmarks.Count - 1 do
      begin
        Bookmark := TBookmark(Bookmarks[i]);
        if Bookmark.ContentsTopic.HelpFile = AHelpFile then
        begin
          WriteLn( BookmarksFile, BOOKMARK_SECTION );
          Bookmark.Save( BookmarksFile );
        end;
      end;
    finally
      System.Close( BookmarksFile );
    end;
  except
    on E: Exception do
      TfpgMessageDialog.Critical('Save Bookmarks', 'Could not save bookmarks: ' + E.Message);
  end;
end;

procedure TMainForm.AddBookmark;
var
  Bookmark: TBookmark;
begin
  Bookmark := TBookmark.Create;

  if tvContents.Selection <> nil then
  begin
    Bookmark.ContentsTopic := TTopic(tvContents.Selection.Data);
    Bookmark.Name := Bookmark.ContentsTopic.Title;
  end
  else
  begin
    { Not all topics appear in the Contents treeview, so we handle that here }
    Bookmark.ContentsTopic := nil;
    Bookmark.Name := rsDVUntitled;
  end;

  Bookmarks.Add( Bookmark );
  OnBookmarksChanged( self );
end;

procedure TMainForm.ClearBookmarks;
begin
  ClearListAndObjects( Bookmarks );
  BuildBookmarksMenu;
//  if Assigned( BookmarksForm ) then
//  begin
//    UpdateBookmarksForm; // clear bookmarks for next show
//    BookmarksForm.Hide;
//  end;
end;

procedure TMainForm.OnBookmarksChanged(Sender: TObject);
begin
  BuildBookmarksMenu;
  SaveBookmarks;
end;

procedure TMainForm.BuildBookmarksMenu;
var
  i: integer;
  Bookmark: TBookmark;
  MenuItem: TfpgMenuItem;
begin
  DestroyListObjects( BookmarksMenuItems );
  BookmarksMenuItems.Clear;

  if Bookmarks.Count > 0 then
  begin
    MenuItem := miBookmarks.AddMenuItem('-', '', nil);
    BookmarksMenuItems.Add( MenuItem );
  end;

  for i:= 0 to Bookmarks.Count -1 do
  begin
    Bookmark := TBookmark(Bookmarks[i]);
    MenuItem := miBookmarks.AddMenuItem(Bookmark.Name, '', @miBookmarksMenuItemClicked);
    MenuItem.Tag:= i;
    BookmarksMenuItems.Add( MenuItem );
  end;
end;

procedure TMainForm.NavigateToBookmark(Bookmark: TBookmark);
begin
  DisplayTopic(Bookmark.ContentsTopic);
end;



end.
