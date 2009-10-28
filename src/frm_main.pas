unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_panel, fpg_tab,
  fpg_tree, fpg_splitter, fpg_menu, fpg_button, fpg_listbox,
  fpg_label, fpg_edit, fpg_radiobutton, fpg_progressbar, fpg_mru,
  HelpFile, RichTextView, HelpTopic;

type
  // Used by Index ListBox. We can generate a custom Index if the help file
  // doesn't contain it's own index entries.
  TListType = ( ltContents, ltIndex );

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    bvlStatusBar: TfpgBevel;
    bvlBody: TfpgBevel;
    PageControl1: TfpgPageControl;
    tsContents: TfpgTabSheet;
    tsIndex: TfpgTabSheet;
    tsSearch: TfpgTabSheet;
    tsNotes: TfpgTabSheet;
    tsHistory: TfpgTabSheet;
    tvContents: TfpgTreeView;
    Splitter1: TfpgSplitter;
    RichView: TRichTextView;
    MainMenu: TfpgMenuBar;
    miFile: TfpgPopupMenu;
    miSettings: TfpgPopupMenu;
    miBookmarks: TfpgPopupMenu;
    miHelp: TfpgPopupMenu;
    miDebug: TfpgPopupMenu;
    miOpenRecentMenu: TfpgPopupMenu;
    btnIndex: TfpgButton;
    btnGo: TfpgButton;
    ListBox1: TfpgListBox;
    btnNotesAdd: TfpgButton;
    btnNotesEdit: TfpgButton;
    btnNotesDel: TfpgButton;
    btnNotesGoto: TfpgButton;
    lbHistory: TfpgListBox;
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
    ProgressBar: TfpgProgressBar;
    lblStatus: TfpgLabel;
    lbIndex: TfpgListBox;
    btnSearch: TfpgButton;
    IndexSearchEdit: TfpgEdit;
    {@VFD_HEAD_END: MainForm}
    Files: TList; // current open help files.
    Debug: boolean;
    mru: TfpgMRU;
    FFileOpenRecent: TfpgMenuItem;

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

    procedure   RichViewClickLink(Sender: TRichTextView; Link: string);
    procedure   MainFormShow(Sender: TObject);
    procedure   MainFormDestroy(Sender: TObject);
    procedure   miFileQuitClicked(Sender: TObject);
    procedure   miFileOpenClicked(Sender: TObject);
    procedure   miFileCloseClicked(Sender: TObject);
    procedure   miHelpProdInfoClicked(Sender: TObject);
    procedure   miHelpAboutFPGui(Sender: TObject);
    procedure   miDebugHeader(Sender: TObject);
    procedure   miDebugHex(Sender: TObject);
    procedure   miFileSaveTopicAsIPF(Sender: TObject);
    procedure   miMRUClick(Sender: TObject; const FileName: String);
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
    procedure   lbSearchResultsDoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   btnSearchClicked(Sender: TObject);
    procedure   DisplaySelectedSearchResultTopic;
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
    procedure   SaveNotes(AHelpFile: THelpFile);
    procedure   DisplayTopic(ATopic: TTopic = nil);
    procedure   ResetProgress;
    procedure   SetStatus(const AText: TfpgString);
    function    TranslateEnvironmentVar(AFilenames: TfpgString): TfpgString;
    procedure   RefreshFontSubstitutions;
    procedure   DisplaySelectedIndexTopic;
    procedure   ProcessCommandLineParams;
    procedure   ShowParamHelp;
    function    FindTopicForLink( Link: THelpLink ): TTopic;
    function    FindTopicByResourceID( ID: word ): TTopic;


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
  ,nvUtilities
  ,ACLStringUtility
  ,TextSearchQuery
  ,SearchUnit
  ,dvconstants
  ,IPFFileFormatUnit
  ,SettingsUnit
  ,dvHelpers
  ;

const
  cLongName   = 'fpGUI Documentation Viewer';
  cCreatedBy  = 'Created by Graeme Geldenhuys';
  cVersion    = 'Version 1.0';

{@VFD_NEWFORM_IMPL}

procedure TMainForm.MainFormException(Sender: TObject; E: Exception);
begin
  TfpgMessageDialog.Critical('An unexpected error occurred.', E.Message);
end;

procedure TMainForm.RichViewClickLink(Sender: TRichTextView; Link: string);
var
  LinkIndex: integer;
  lLink: THelpLink;
  lHelp: THelpFile;
  i: integer;
  lTopic: TTopic;
  lFound: Boolean;
begin
  // TODO: process other types of links (external, application etc...) too!

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
        writeln('Found Topic! ', lTopic.Title);
        break;

      end;
      if lFound then
        break;
    end;
    if lTopic <> nil then
    begin
      writeln('Displaying topic <', lTopic.Title, '>');
      DisplayTopic(lTopic);
    end;
    //lLink := SourceWindow.Topic.Links[ LinkIndex ];
    //
    //PostMsg( Self.Handle,
    //         WM_FOLLOWLINK,
    //         longint( Link ),
    //         longint( SourceWindow ) );

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

  ProcessCommandLineParams;
end;

procedure TMainForm.MainFormDestroy(Sender: TObject);
begin
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

procedure TMainForm.miFileCloseClicked(Sender: TObject);
begin
  CloseFile;
end;

procedure TMainForm.miHelpProdInfoClicked(Sender: TObject);
var
  s: TfpgString;
begin
  s :=  cLongName + LineEnding + LineEnding
      + cCreatedBy
      + cVersion + '  -  '+  {$I %date%} + ' ' + {$I %time%};

  TfpgMessageDialog.Information('Product Information', s);
end;

procedure TMainForm.miHelpAboutFPGui(Sender: TObject);
begin
  TfpgMessageDialog.AboutFPGui;
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
      Add('');
      Add('<b>Title:</b> ' + f.Title);
      Add('<b>Dictionary count:</b> ' + IntToStr(f.DictionaryCount));
      Add('<b>Topic count:</b> ' + IntToStr(f.TopicCount));
      Add('<b>Index count:</b> ' + IntToStr(f.Index.Count));
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

procedure TMainForm.miMRUClick(Sender: TObject; const FileName: String);
begin
  OpenFile(FileName, '', True);
end;

procedure TMainForm.btnShowIndex(Sender: TObject);
var
  Count: integer;
  i: integer;
  s: TfpgString;
  f: THelpFile;
begin
  f := THelpFile(Files[0]);
  lbIndex.Items.Clear;
  for i := 0 to f.Index.Count-1 do

    lbIndex.Items.AddObject(F.Index.GetTopic(i).Title, f.Topics[i]);
  lbIndex.Invalidate
end;

procedure TMainForm.btnGoClicked(Sender: TObject);
begin
  if tvContents.Selection <> nil then
    DisplayTopic(nil);
end;

procedure TMainForm.tvContentsChange(Sender: TObject);
begin
  DisplayTopic(nil);
end;

procedure TMainForm.edSearchTextKeyPress(Sender: TObject; var KeyCode: word;
  var ShiftState: TShiftState; var Consumed: boolean);
begin
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
  if (KeyCode = keyReturn) or (KeyCode = keyPEnter) then
  begin
    Consumed := True;
    DisplayTopic(nil);
  end
end;

procedure TMainForm.MainFormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
writeln('DEBUG:  TMainForm.MainFormCloseQuery >>>>>');
  CloseFile(True);
writeln('DEBUG:  TMainForm.MainFormCloseQuery <<<<<');
end;

procedure TMainForm.PageControl1Change(Sender: TObject; NewActiveSheet: TfpgTabSheet);
begin
  if NewActiveSheet = tsIndex then
  begin
      if not IndexLoaded then
        LoadIndex;
      IndexSearchEdit.SetFocus;
  end;
end;

procedure TMainForm.tvContentsDoubleClick(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  if tvContents.Selection <> nil then
    DisplayTopic(nil);
end;

procedure TMainForm.lbIndexDoubleClick(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  DisplayTopic(nil);
end;

procedure TMainForm.lbSearchResultsDoubleClick(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  DisplaySelectedSearchResultTopic;
end;

procedure TMainForm.btnSearchClicked(Sender: TObject);
begin
  DoSearch;
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

  Query.Destroy;
  SearchResults.Destroy;

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
      mru.AddItem(dlg.Filename);
      Settings.LastOpenDirectory := ExtractFilePath(dlg.Filename);
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
      FullFilePath := FileName; // we'll complain later.
    end;
    LoadingFilenameList[ FileIndex ] := FullFilePath;
  end;

  // Now actually load the files
  for FileIndex := 0 to LoadingFilenameList.Count - 1 do
  begin
    Filename := LoadingFilenameList[ FileIndex ];
    mru.AddItem(FileName);
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

// RBRi Translate
  if not LoadFiles(FileNames, tmpHelpFiles) then
  begin
    ClearWaitCursor;
    tmpHelpFiles.Free;
    exit;
  end;

  Result := true;

  lbSearchResults.Items.Clear;
  { TODO : page history support }
//  PageHistory.Clear;
//  CurrentHistoryIndex := -1;

  // Now that we have successfully loaded the new help file(s)
  // close the existing one.
  CloseFile;

  AssignList(tmpHelpFiles, CurrentOpenFiles );

  ProgressBar.Position := 50;
  SetStatus( rsDVDisplaying );

//  AddCurrentToMRUFiles;

  if AWindowTitle = '' then
    MainTitle := THelpFile( CurrentOpenFiles[ 0 ] ).Title
  else
    MainTitle := AWindowTitle;
  SetMainCaption;

  // Now load the various parts of the file(s)
  // into the user interface
  tvContents.RootNode.Clear;

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
    { TODO -oGraeme : Improved Display Topic method }
//    DisplaySelectedContentsTopic;
    DisplayTopic(nil);
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
  RichView.Clear(ADestroying);
  if not ADestroying then
  begin
    WindowTitle := rsDVTitle + ' - ' + rsDVNoFile;
    tvContents.Invalidate;
  end;

  // First save notes. It's important we do this first
  // since we scan all notes each time to find the ones
  // belonging to this file.
  for FileIndex := 0 to Files.Count - 1 do
  begin
    lHelpFile := THelpFile(Files[FileIndex]);
    SaveNotes( lHelpFile );
  end;

  DisplayedIndex.Clear;

  // Now destroy help files
  for FileIndex := 0 to Files.Count - 1 do
  begin
    lHelpFile := THelpFile(Files[FileIndex]);
    lHelpFile.Free;
  end;

  Files.Clear;
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
  lbIndex.Items.Assign( DisplayedIndex );

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
Begin
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
    end;
  end  // case..
  else
    Topic := ATopic;  // use topic passed in as a parameter

  if Topic = nil then
    raise Exception.Create('Unable to locate the Topic');

  CurrentTopic:= Topic;

  RichView.Clear;
  ImageIndices := TList.Create;
  ProfileEvent('Cleared memo...');

  HelpFile := TopicFile(CurrentTopic);
  if HelpFile = nil then
    raise Exception.Create('Failed to get active HelpFile from Topic');

  if HelpFile.HighlightWords <> nil then
    ProfileEvent('highlightwords is ok');
  lText := '';
  ProfileEvent('Debug show hex values = ' + BoolToStr(Debug));
  if ImageIndices <> nil then
    ProfileEvent('ImageIndices initialized');

  CurrentTopic.GetText( nil {HighlightWordSequences},
                  Debug {ShowCodes},
                  False {ShowWordIndices},
                  lText {TopicText},
                  ImageIndices,
                  nil {Highlights} );

  { TODO -oGraeme : We do not support images yet }
  ImageIndices.Free;

  //writeln(lText);
  //writeln('-----------------------------');
  RichView.AddText(PChar(lText));

  tvContents.Selection := tvContents.RootNode.FindSubNode(CurrentTopic, True);
  tvContents.Invalidate;
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
begin
  inherited Create(AOwner);
  fpgApplication.OnException  := @MainFormException;
  OnShow  := @MainFormShow;
  OnDestroy :=@MainFormDestroy;
  Files := TList.Create;
  AllFilesWordSequences := TList.Create;
  CurrentOpenFiles := TList.Create;
  DisplayedIndex := TStringList.Create;
  { TODO -oGraeme : Make Debug a menu option }
  Debug := False;
end;

destructor TMainForm.Destroy;
var
  FileIndex: integer;
  lHelpFile: THelpFile;
begin
writeln('DEBUG:  TMainForm.Destroy >>>>');
  CurrentTopic := nil;  // it was a reference only
  FFileOpenRecent := nil;   // it was a reference only
  DestroyListAndObjects(Files);
  DestroyListAndObjects(AllFilesWordSequences);
  DestroyListAndObjects(CurrentOpenFiles);
writeln('DEBUG:  TMainForm.Destroy   1');
  inherited Destroy;
writeln('DEBUG:  TMainForm.Destroy <<<<');
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(602, 274, 654, 386);
  WindowTitle := 'fpGUI Documentation Viewer';
  WindowPosition := wpUser;
  OnCloseQuery  := @MainFormCloseQuery;

  bvlStatusBar := TfpgBevel.Create(self);
  with bvlStatusBar do
  begin
    Name := 'bvlStatusBar';
    SetPosition(0, 366, 653, 20);
    Anchors := [anLeft,anRight,anBottom];
    Style := bsLowered;
  end;

  bvlBody := TfpgBevel.Create(self);
  with bvlBody do
  begin
    Name := 'bvlBody';
    SetPosition(0, 31, 653, 334);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Shape := bsSpacer;
  end;

  PageControl1 := TfpgPageControl.Create(bvlBody);
  with PageControl1 do
  begin
    Name := 'PageControl1';
    SetPosition(0, 0, 260, 328);
    ActivePageIndex := 2;
    TabOrder := 0;
    Align := alLeft;
    OnChange  := @PageControl1Change;
  end;

  tsContents := TfpgTabSheet.Create(PageControl1);
  with tsContents do
  begin
    Name := 'tsContents';
    SetPosition(3, 24, 254, 301);
    Text := 'Contents';
  end;

  tsIndex := TfpgTabSheet.Create(PageControl1);
  with tsIndex do
  begin
    Name := 'tsIndex';
    SetPosition(3, 24, 254, 301);
    Text := 'Index';
  end;

  tsSearch := TfpgTabSheet.Create(PageControl1);
  with tsSearch do
  begin
    Name := 'tsSearch';
    SetPosition(3, 24, 254, 301);
    Text := 'Search';
  end;

  tsNotes := TfpgTabSheet.Create(PageControl1);
  with tsNotes do
  begin
    Name := 'tsNotes';
    SetPosition(3, 24, 254, 301);
    Text := 'Notes';
  end;

  tsHistory := TfpgTabSheet.Create(PageControl1);
  with tsHistory do
  begin
    Name := 'tsHistory';
    SetPosition(3, 24, 254, 301);
    Text := 'History';
  end;

  tvContents := TfpgTreeView.Create(tsContents);
  with tvContents do
  begin
    Name := 'tvContents';
    SetPosition(4, 32, 242, 264);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Label1';
    ScrollWheelDelta := 60;
    ShowImages := True;
    TabOrder := 0;
    OnChange  := @tvContentsChange;
    //OnDoubleClick  := @tvContentsDoubleClick;
  end;

  Splitter1 := TfpgSplitter.Create(bvlBody);
  with Splitter1 do
  begin
    Name := 'Splitter1';
    SetPosition(265, 4, 8, 284);
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
    SetPosition(292, 28, 132, 20);
    AddMenuItem('Open...', '', @miFileOpenClicked);
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
    SetPosition(292, 76, 132, 20);
    AddMenuItem('Options...', '', nil);
  end;

  miBookmarks := TfpgPopupMenu.Create(self);
  with miBookmarks do
  begin
    Name := 'miBookmarks';
    SetPosition(292, 100, 132, 20);
    AddMenuItem('Add..', '', nil);
    AddMenuItem('Show', '', nil);
  end;

  miHelp := TfpgPopupMenu.Create(self);
  with miHelp do
  begin
    Name := 'miHelp';
    SetPosition(292, 124, 132, 20);
    AddMenuItem('Contents...', '', nil);
    AddMenuItem('Help using help', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('About fpGUI Toolkit', '', @miHelpAboutFPGui);
    AddMenuItem('Product Information...', '', @miHelpProdInfoClicked);
  end;

  miDebug := TfpgPopupMenu.Create(self);
  with miDebug do
  begin
    Name := 'miDebug';
    SetPosition(292, 148, 132, 20);
    AddMenuItem('Debug: Header', '', @miDebugHeader);
    AddMenuItem('Toggle Hex INF Values in Contents', '', @miDebugHex);
  end;

  miOpenRecentMenu := TfpgPopupMenu.Create(self);
  with miOpenRecentMenu do
  begin
    Name := 'miOpenRecentMenu';
    SetPosition(309, 52, 132, 20);
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

  ListBox1 := TfpgListBox.Create(tsNotes);
  with ListBox1 do
  begin
    Name := 'ListBox1';
    SetPosition(4, 32, 242, 264);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#List';
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

  lbHistory := TfpgListBox.Create(tsHistory);
  with lbHistory do
  begin
    Name := 'lbHistory';
    SetPosition(4, 8, 242, 288);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#List';
    HotTrack := False;
    PopupFrame := False;
    TabOrder := 0;
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

  ProgressBar := TfpgProgressBar.Create(bvlStatusBar);
  with ProgressBar do
  begin
    Name := 'ProgressBar';
    SetPosition(501, 2, 150, 16);
    Anchors := [anRight,anBottom];
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

  lbIndex := TfpgListBox.Create(tsIndex);
  with lbIndex do
  begin
    Name := 'lbIndex';
    SetPosition(4, 32, 242, 264);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#List';
    HotTrack := False;
    PopupFrame := False;
    TabOrder := 1;
    OnDoubleClick  := @lbIndexDoubleClick;
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

  IndexSearchEdit := TfpgEdit.Create(tsIndex);
  with IndexSearchEdit do
  begin
    Name := 'IndexSearchEdit';
    SetPosition(4, 4, 152, 24);
    Anchors := [anLeft,anRight,anTop];
    TabOrder := 2;
    Text := '';
    FontDesc := '#Edit1';
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}

  // hook up the sub-menus.
  MainMenu.AddMenuItem('&File', nil).SubMenu := miFile;
  MainMenu.AddMenuItem('&Settings', nil).SubMenu := miSettings;
  MainMenu.AddMenuItem('&Bookmarks', nil).SubMenu := miBookmarks;
  MainMenu.AddMenuItem('&Help', nil).SubMenu := miHelp;
  MainMenu.AddMenuItem('&Debug', nil).SubMenu := miDebug;
  FFileOpenRecent.SubMenu := miOpenRecentMenu;

  // correct default visible tabsheet
  PageControl1.ActivePageIndex := 0;

  // most recently used files
  mru := TfpgMRU.Create(self);
  mru.Name := 'MRU';
  mru.ParentMenuItem  := miOpenRecentMenu;
  mru.OnClick         :=@miMRUClick;
  mru.MaxItems        := gINI.ReadInteger('Options', 'MRUFileCount', 8);
  mru.ShowFullPath    := gINI.ReadBool('Options', 'ShowFullPath', True);
  mru.LoadMRU;
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

procedure TMainForm.DisplaySelectedIndexTopic;
var
  Topic: TTopic;
Begin
  if lbIndex.FocusItem = -1 then
    exit;
  Topic := DisplayedIndex.Objects[ lbIndex.FocusItem ] as TTopic;
  DisplayTopic( Topic );
end;

procedure TMainForm.ProcessCommandLineParams;
var
  showtopic: boolean;
begin
  if ParamCount > 0 then
  begin
    if gCommandLineParams.IsParam('h') then
    begin
      ShowParamHelp;
      Exit;
    end
    else if gCommandLineParams.IsParam('debuglog') then
      // do nothing
    else
    begin
      showtopic := not gCommandLineParams.IsParam('s');
      OpenFile(ParamStr(1), '', showtopic);
    end;
  end;

  // now process all other parameters
  if gCommandLineParams.IsParam('s') then
  begin
    edSearchText.Text := gCommandLineParams.GetParam('s');
    PageControl1.ActivePage := tsSearch;
    btnSearch.Click;
  end;
end;

procedure TMainForm.ShowParamHelp;
const
  le = LineEnding;
var
  s: string;
begin
  s := '<font "Arial" 12><b>' + cLongName + '</b></font>' + le
       + cVersion + le + le
       + 'Supported command line parameters:' + le + le
       + '<tt>'
       + '  <<filename>   Load the help file <<filename>' + le
       + '  -h           Show this help' + le
       + '  -s<<text>     Search for <<text> in open help files' + le
       + '  -t<<id>       Open Topic with ID equal to <<id>' + le
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
    HelpFile := THelpFile(CurrentOpenFiles[ id ]);

    Result := HelpFile.FindTopicByResourceID( ID );
    if Result <> nil then
      // found
      exit;
  end;

  // not found.
  Result := nil;
end;


end.
