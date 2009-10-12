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

    // while loading... so owe can display progress
    LoadingFilenameList: TStringList;
    LoadingFileIndex: integer;
    LoadingTotalSize: longint;
    LoadingSizeSoFar: longint;

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
    procedure   FileOpen;
    function    OpenFile(const AFileNames: string): boolean;
    procedure   CloseFile(const ADestroying: boolean = False);
    procedure   OnHelpFileLoadProgress(n, outof: integer; AMessage: string);
    procedure   LoadNotes(AHelpFile: THelpFile);
    procedure   LoadContents;
    // Used in loading contents
    procedure   AddChildNodes(AHelpFile: THelpFile; AParentNode: TfpgTreeNode; ALevel: longint; var ATopicIndex: longint );
    procedure   ClearNotes;
    procedure   SaveNotes(AHelpFile: THelpFile);
    procedure   DisplayTopic;
    procedure   ResetProgress;
    procedure   SetStatus(const AText: TfpgString);
    function    TranslateEnvironmentVar(AFilenames: TfpgString): TfpgString;
    // Given a "filename" which may include a path, find it in various paths and extensions
    function    FindHelpFile(AFileName: TfpgString ): TfpgString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

const
  cTitle = 'Documentation Viewer';

implementation

uses
  fpg_dialogs
  ,fpg_constants
  ,fpg_iniutils
  ,nvUtilities
  ,HelpTopic
  {$IFDEF Timing}
  ,EpikTimer
  {$ENDIF}
  ,TextSearchQuery
  ,SearchUnit
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
    OpenFile(ParamStr(1));
    {$IFDEF Timing}
    t.Stop;
    writeln(t.ElapsedDHMS);
    {$ENDIF}
  end;
  // restore previous window position and size
  gINI.ReadFormState(self);
  PageControl1.Width := gINI.ReadInteger('Options', 'SplitterLeft', 260);
  UpdateWindowPosition;
end;

procedure TMainForm.MainFormDestroy(Sender: TObject);
begin
  // save splitter position
  gINI.WriteInteger('Options', 'SplitterLeft', PageControl1.Width);
  // save form size and position
  gINI.WriteFormState(self);
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
begin
  TfpgMessageDialog.Information('Product Information', 'Created by Graeme Geldenhuys');
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
  OpenFile(FileName);
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
    lbIndex.Items.AddObject(f.Index[i], f.Topics[i]);
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
  CloseFile(True);
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
var
  Query: TTextSearchQuery;
  SearchText: TfpgString;
  SearchResults: TList;
  TopicIndex: integer;
  Topic: TTopic;
  FileIndex: integer;
  HelpFile: THelpFile;
begin
  SearchText:= trim(edSearchText.Text);
  if SearchText = '' then
    Exit;  //==>

  try
    Query := TTextSearchQuery.Create( SearchText );
  except
    on e: ESearchSyntaxError do
    begin
      ShowMessage( 'Error in search syntax: '
                   + e.Message );
      exit;
    end;
  end;

  SearchResults := TList.Create;

  // Search open help file
  for FileIndex := 0 to Files.Count - 1 do
  begin
    HelpFile := THelpFile(Files[ FileIndex ]);
    SearchHelpFile( HelpFile,
                    Query,
                    SearchResults,             // SearchResults get populated
                    HelpFile.HighlightWords ); // HighlightWords get populate here!
  end;

  // Sort results across all files by relevance
  SearchResults.Sort( @TopicRelevanceCompare );

  // Load topics into search results list.
  lbSearchResults.BeginUpdate;
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

  lbSearchResults.FocusItem := -1;
  lbSearchResults.EndUpdate;
  fpgApplication.ProcessMessages; // make sure list gets displayed

  Query.Free;
  SearchResults.Free;

  if lbSearchResults.Items.Count > 0 then
    lbSearchResults.FocusItem := 0
  else
    lbSearchResults.Items.Add( '(No matches found for "' + SearchText + '")' );
end;

procedure TMainForm.FileOpen;
var
  dlg: TfpgFileDialog;
begin
  dlg := TfpgFileDialog.Create(nil);
  try
    dlg.WindowTitle := 'Open Help File';
    dlg.Filter := 'Help Files (*.hlp, *.inf)|*.inf;*.hlp ';
    // and a catch all filter
    dlg.Filter := dlg.Filter + '|(' + rsAllFiles + ' (*)|*';

    if dlg.RunOpenFile then
    begin
      mru.AddItem(dlg.Filename);
      OpenFile(dlg.Filename);
      { TODO -oGraeme : Add support for multiple files. }
//      OpenFile( ListToString( dlg.FileNames, '+' ) );
    end;
  finally
    dlg.Free;
  end;
end;

function TMainForm.OpenFile(const AFileNames: string): boolean;
var
  HelpFiles: TList;
  HelpFile: THelpFile;
  lFilename: string;
  FullFilePath: string;
  FileIndex: integer;
  lFileSize: longint;
  RemainingFileNames: string;
begin
  ProfileEvent('OpenFile >>>');
  lbHistory.Items.Clear;

  HelpFiles := TList.Create;
  ProfileEvent( 'Translate environment vars' );
  RemainingFileNames := TranslateEnvironmentVar(AFilenames);

  LoadingFilenameList := TStringList.Create;
  LoadingTotalSize := 0;

  while RemainingFileNames <> '' do
  begin
    lFileName := ExtractNextValue(RemainingFileNames, '+');
    ProfileEvent( '  File: ' + lFileName );
    FullFilePath := FindHelpFile(lFilename);
    if FullFilePath <> '' then
    begin
      lFileName := FullFilePath;
      lFileSize := GetFileSize(lFilename);
      inc(LoadingTotalSize, lFileSize);
    end;
    ProfileEvent( '  Full path: ' + FullFilePath );
    LoadingFilenameList.Values[lFileName] := IntToStr(lFileSize);
  end;

  LoadingSizeSoFar := 0;
  for FileIndex := 0 to LoadingFilenameList.Count - 1 do
  begin
    lFilename := LoadingFilenameList.Names[FileIndex];
    ProfileEvent( '  Loading: ' + lFilename );
    try
      LoadingFileIndex := FileIndex;
      HelpFile := THelpFile.Create(lFileName, @OnHelpFileLoadProgress);
      inc(LoadingSizeSoFar, StrToInt(LoadingFilenameList.Values[lFileName]));
      HelpFiles.Add(HelpFile);
    except
      on E: Exception do
      begin
        if E is EHelpFileException then
          ShowMessage( 'Could not open ' + lFileName + ': ' + E.Message )
        else
          ShowMessage( 'An error occurred loading ' + lFileName
                       + '. It may be a damaged help file '
                       + 'or there may be a bug in this program.' + #10 + #10 + E.Message );
        Result := False;
        // cleanup memory used
        while HelpFiles.Count > 0 do
        begin
          HelpFile := THelpFile(HelpFiles[0]);
          HelpFile.Free;
          HelpFiles.Delete(0);
        end;
        LoadingFilenameList.Free;
        HelpFiles.Free;
        ResetProgress;
        Exit;  //==>
      end { exception }
    end; { try/except }
  end;  { for FileIndex... }

  // Now that we have successfully loaded the new help file(s)
  // close the existing one.
  CloseFile;

  Files.Assign(HelpFiles);
//  AssignList(HelpFiles, Files);
  ProgressBar.Position:= 50;
  SetStatus( 'Displaying... ' );
  fpgApplication.ProcessMessages;

  LoadingFilenameList.Free;
  HelpFiles.Free;
  Result := True;

  WindowTitle := cTitle + ' - ' + THelpFile( Files[ 0 ] ).Title;
  fpgApplication.ProcessMessages;

  { TODO -oGraeme : Do MRU files list handling here }

  ProgressBar.Position := 51;
  SetStatus( 'Loading notes... ' );

  { TODO -oGraeme : Load previous notes here }
  for FileIndex:= 0 to Files.Count - 1 do
  begin
    HelpFile := THelpFile(Files[ FileIndex ]);
    LoadNotes( HelpFile );
  end;

  ProgressBar.Position := 55;
  SetStatus( 'Display contents... ' );
  LoadContents;

  if tvContents.RootNode.Count = 1 then
  begin
    ProfileEvent( '  Expand first node' );
    // Contents has only one top level node... expand it
    tvContents.RootNode.FirstSubNode.Expand;
  end;

  ProgressBar.Position:= 57;
  SetStatus( 'Display first topic... ' );
  ProfileEvent( '  Display first topic' );
  tvContents.Selection := tvContents.RootNode.FirstSubNode;
  tvContents.Invalidate;
  btnGoClicked(nil);

  { TODO -oGraeme : Load Index here }
  lbSearchResults.Items.Clear;

  ProgressBar.Position := 100;
  SetStatus( 'Done' );
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
    WindowTitle := cTitle + ' - No file';
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

procedure TMainForm.LoadContents;
var
  FileIndex: integer;
  HelpFile: THelpFile;
  TopicIndex: integer;
  Node: TfpgTreeNode;
  Topic: TTopic;
begin
  ProfileEvent( 'Load contents treeview' );

  tvContents.RootNode.Clear;

  ProfileEvent( 'Loop files' );

  Node := nil;

  for FileIndex:= 0 to Files.Count - 1 do
  begin
    HelpFile:= THelpFile(Files[ FileIndex ]);
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
  Topic.GetText(HelpFile.HighlightWords,
                Debug,
                lText,
                ImageIndices);

  { TODO -oGraeme : We do not support images yet }
  ImageIndices.Free;

  RichView.AddText(PChar(lText));
end;

procedure TMainForm.ResetProgress;
begin
  { TODO -oGraeme : implement ResetProgress }
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

function TMainForm.FindHelpFile(AFileName: TfpgString): TfpgString;
begin
  { TODO -oGraeme : Implement FindHelpFile()}
  Result := AFilename;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fpgApplication.OnException  := @MainFormException;
  OnShow  := @MainFormShow;
  OnDestroy :=@MainFormDestroy;
  Files := TList.Create;
  { TODO -oGraeme : Make Debug a menu option }
  Debug := False;
end;

destructor TMainForm.Destroy;
var
  FileIndex: integer;
  lHelpFile: THelpFile;
begin
  FFileOpenRecent := nil;   // it was a reference only
  if (Files <> nil) and (Files.Count > 0) then
  begin
    // Now destroy help files
    for FileIndex := 0 to Files.Count - 1 do
    begin
      lHelpFile := THelpFile(Files[FileIndex]);
      lHelpFile.Free;
    end;
  end;
  Files.Clear;
  Files.Free;
  inherited Destroy;
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
