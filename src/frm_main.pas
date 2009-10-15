unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_panel, fpg_tab,
  fpg_tree, fpg_splitter, fpg_menu, fpg_button, fpg_listbox,
  fpg_label, fpg_edit, fpg_radiobutton, fpg_progressbar, fpg_mru,
  HelpFile, RichTextView;

type

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

    procedure   MainFormShow(Sender: TObject);
    procedure   MainFormDestroy(Sender: TObject);
    procedure   miFileQuitClicked(Sender: TObject);
    procedure   miFileOpenClicked(Sender: TObject);
    procedure   miFileCloseClicked(Sender: TObject);
    procedure   miHelpProdInfoClicked(Sender: TObject);
    procedure   miHelpAboutFPGui(Sender: TObject);
    procedure   miDebugHeader(Sender: TObject);
    procedure   miDebugHex(Sender: TObject);
    procedure   miMRUClick(Sender: TObject; const FileName: String);
    procedure   btnShowIndex(Sender: TObject);
    procedure   btnGoClicked(Sender: TObject);
    procedure   tvContentsChange(Sender: TObject);
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
    // Used in loading contents
    procedure   AddChildNodes(AHelpFile: THelpFile; AParentNode: TfpgTreeNode; ALevel: longint; var ATopicIndex: longint );
    procedure   ClearNotes;
    procedure   SaveNotes(AHelpFile: THelpFile);
    procedure   DisplayTopic;
    procedure   ResetProgress;
    procedure   SetStatus(const AText: TfpgString);
    function    TranslateEnvironmentVar(AFilenames: TfpgString): TfpgString;
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
  ,nvUtilities
  ,ACLStringUtility
  ,HelpTopic
  {$IFDEF Timing}
  ,EpikTimer
  {$ENDIF}
  ,TextSearchQuery
  ,SearchUnit
  ,dvconstants
  ,IPFFileFormatUnit
  ,SettingsUnit
  ,dvHelpers
  ;


{@VFD_NEWFORM_IMPL}

procedure TMainForm.MainFormException(Sender: TObject; E: Exception);
begin
  TfpgMessageDialog.Critical('An unexpected error occurred.', E.Message);
end;

procedure TMainForm.MainFormShow(Sender: TObject);
{$IFDEF Timing}
var
  t: TEpikTimer;
{$ENDIF}
begin
  bvlBody.Realign;

  if Paramcount > 0 then
  begin
    {$IFDEF Timing}
    t := TEpikTimer.Create(nil);
    t.Start;
    {$ENDIF}
    OpenFile(ParamStr(1), '', True);
    {$IFDEF Timing}
    t.Stop;
    writeln(t.ElapsedDHMS);
    {$ENDIF}
  end;
  // restore previous window position and size
  gINI.ReadFormState(self);
  PageControl1.Width := gINI.ReadInteger('Options', 'SplitterLeft', 260);
  UpdateWindowPosition;

  Settings.NormalFont := fpgStyle.DefaultFont;
  Settings.FixedFont := fpgStyle.FixedFont;
  Settings.SearchDirectories := TStringList.Create;

end;

procedure TMainForm.MainFormDestroy(Sender: TObject);
begin
writeln('DEBUG:  TMainForm.MainFormDestroy >>>>');
  // save splitter position
  gINI.WriteInteger('Options', 'SplitterLeft', PageControl1.Width);
  // save form size and position
  gINI.WriteFormState(self);
writeln('DEBUG:  TMainForm.MainFormDestroy <<<<');
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
  s := 'fpGUI ' + rsDVTitle + LineEnding + LineEnding
      + 'Created by Graeme Geldenhuys' + LineEnding
      + 'Version 1.0  -  ' + {$I %date%} + ' ' + {$I %time%};
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
  f := THelpFile(Files[0]);
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
  RichView.AddText(PChar(sl.Text));
  sl.Free;
end;

procedure TMainForm.miDebugHex(Sender: TObject);
begin
  Debug := not Debug;
  DisplayTopic;
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
    DisplayTopic;
end;

procedure TMainForm.tvContentsChange(Sender: TObject);
begin
  DisplayTopic;
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
    if Files.Count > 0 then
      if lbIndex.Items.Count = 0 then
        btnShowIndex(nil);
  end;
end;

procedure TMainForm.tvContentsDoubleClick(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  if tvContents.Selection <> nil then
    DisplayTopic;
end;

procedure TMainForm.lbIndexDoubleClick(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  DisplayTopic;
end;

procedure TMainForm.lbSearchResultsDoubleClick(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  DisplayTopic;
end;

procedure TMainForm.btnSearchClicked(Sender: TObject);
begin
  DoSearch;
end;

procedure TMainForm.DisplaySelectedSearchResultTopic;
begin
  //
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

  // LoadIndex;

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
    { TODO -ograemeg -cSettings : Use settings.lastopendirectory }
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
    DisplayTopic;
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
  { TODO -ograemeg -copen files : Implement ParseAndExpandFilenames }
  tmpFileNames.Add(AFileName);
//  ParseAndExpandFileNames(FileName, tmpFileNames);
  Result := OpenFiles( tmpFileNames,
                       AWindowTitle,
                       DisplayFirstTopic );
  tmpFileNames.Destroy;
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
  LogEvent(LogStartup, '  Contents loaded' );
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

procedure TMainForm.DisplayTopic;
var
  lText: String;
  ImageIndices: TList;
  LinkIndex: longint;
  Link: THelpLink;
  HelpFile: THelpFile;
  Topic: TTopic;
Begin
  ProfileEvent('DisplayTopic >>>>');
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

  RichView.Clear;
  ImageIndices := TList.Create;
  ProfileEvent('Cleared memo...');

  HelpFile := TopicFile(Topic);
  if HelpFile = nil then
    raise Exception.Create('Failed to get active HelpFile from Topic');

  if HelpFile.HighlightWords <> nil then
    ProfileEvent('highlightwords is ok');
  lText := '';
  ProfileEvent('Debug show hex values = ' + BoolToStr(Debug));
  if ImageIndices <> nil then
    ProfileEvent('ImageIndices initialized');
  //Topic.GetText(HelpFile.HighlightWords,
  //              Debug,
  //              lText,
  //              ImageIndices);
  Topic.GetText( nil {HighlightWordSequences},
                  Debug {ShowCodes},
                  False {ShowWordIndices},
                  lText {TopicText},
                  ImageIndices,
                  nil {Highlights} );

  { TODO -oGraeme : We do not support images yet }
  ImageIndices.Free;

//  writeln(lText);
//  writeln('-----------------------------');
  RichView.AddText(PChar(lText));
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
  { TODO -oGraeme : Make Debug a menu option }
  Debug := False;
end;

destructor TMainForm.Destroy;
var
  FileIndex: integer;
  lHelpFile: THelpFile;
begin
writeln('DEBUG:  TMainForm.Destroy >>>>');
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
  WindowTitle := 'fpGUI Help Viewer';
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
    ActivePageIndex := 4;
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
    AddMenuItem('Debug: Header', '', @miDebugHeader);
    AddMenuItem('Toggle Hex INF Values in Contents', '', @miDebugHex);
    AddMenuItem('-', '', nil);
    AddMenuItem('About fpGUI Toolkit', '', @miHelpAboutFPGui);
    AddMenuItem('Product Information...', '', @miHelpProdInfoClicked);
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
  end;

  RadioButton3 := TfpgRadioButton.Create(tsSearch);
  with RadioButton3 do
  begin
    Name := 'RadioButton3';
    SetPosition(12, 108, 192, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    TabOrder := 5;
    Text := 'All sections';
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
    OnDoubleClick  := @lbSearchResultsDoubleClick;
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

  {@VFD_BODY_END: MainForm}
  {%endregion}

  // hook up the sub-menus.
  MainMenu.AddMenuItem('&File', nil).SubMenu := miFile;
  MainMenu.AddMenuItem('&Settings', nil).SubMenu := miSettings;
  MainMenu.AddMenuItem('&Bookmarks', nil).SubMenu := miBookmarks;
  MainMenu.AddMenuItem('&Help', nil).SubMenu := miHelp;
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


end.
