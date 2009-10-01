unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_panel, fpg_tab,
  fpg_tree, fpg_splitter, fpg_menu, fpg_memo, fpg_button, fpg_listbox,
  fpg_label, fpg_edit, fpg_radiobutton,
  HelpFile;

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
    tvIndex: TfpgTreeView;
    Splitter1: TfpgSplitter;
    Memo1: TfpgMemo;
    MainMenu: TfpgMenuBar;
    miFile: TfpgPopupMenu;
    miSettings: TfpgPopupMenu;
    miBookmarks: TfpgPopupMenu;
    miHelp: TfpgPopupMenu;
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
    {@VFD_HEAD_END: MainForm}
    Files: TList; // current open help files.
    Debug: boolean;
    procedure   MainFormShow(Sender: TObject);
    procedure   miFileQuitClicked(Sender: TObject);
    procedure   miFileOpenClicked(Sender: TObject);
    procedure   miFileCloseClicked(Sender: TObject);
    procedure   miHelpProdInfoClicked(Sender: TObject);
    procedure   miHelpAboutFPGui(Sender: TObject);
    procedure   miDebugHeader(Sender: TObject);
    procedure   btnShowIndex(Sender: TObject);
    procedure   btnGoClicked(Sender: TObject);
    procedure   tvContentsChange(Sender: TObject);
    procedure   MainFormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure   FileOpen;
    function    OpenFile(const AFileNames: string): boolean;
    procedure   CloseFile;
    procedure   OnHelpFileLoadProgress(n, outof: integer; AMessage: string);
    procedure   LoadNotes(AHelpFile: THelpFile);
    procedure   LoadContents;
    // Used in loading contents
    procedure   AddChildNodes(AHelpFile: THelpFile; AParentNode: TfpgTreeNode; ALevel: longint; var ATopicIndex: longint );
    procedure   ClearNotes;
    procedure   SaveNotes(AHelpFile: THelpFile);
    procedure   DisplayTopic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

const
  cTitle = 'fpGUI Help Viewer';

implementation

uses
  fpg_dialogs, fpg_constants, nvUtilities, HelpTopic;


{@VFD_NEWFORM_IMPL}

procedure TMainForm.MainFormShow(Sender: TObject);
begin
  bvlBody.Realign;

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
begin
  Memo1.Lines.Clear;
  Memo1.Lines.BeginUpdate;
  f := THelpFile(Files[0]);
  with Memo1.Lines do
  begin
    Add('Filename: ' + f.Filename);
    Add('----------');
    Add('Title: ' + f.Title);
    Add('DictionaryCount:' + IntToStr(f.DictionaryCount));
    Add('TopicCount: ' + IntToStr(f.TopicCount));
    Add(' ');
    Add('Dictionary contents:');
    for i := 0 to f.DictionaryCount-1 do
      Add('[' + IntToStr(i) + '] = <' + f.DictionaryWords[i] + '>');
  end;
  Memo1.Lines.EndUpdate;
end;

procedure TMainForm.btnShowIndex(Sender: TObject);
var
  Count: integer;
  i: integer;
  s: TfpgString;
begin
//
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
  CloseFile;
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
//      FHelpFile := dlg.FileName;
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
  lFilename: string;
  FullFilePath: string;
  HelpFile: THelpFile;
  FileIndex: integer;
begin
  ProfileEvent('OpenFile >>>');
  lFilename := AFilenames;
  ProfileEvent( 'File: ' + lFileName );
  FullFilePath := ExpandFileName(lFilename);
  ProfileEvent( '  Full path: ' + FullFilePath );
  ProfileEvent( '  Loading: ' + lFilename );

  CloseFile;

  HelpFile := THelpFile.Create(lFileName, @OnHelpFileLoadProgress);
  Files.Add(HelpFile);

  WindowTitle := cTitle + ' - ' + THelpFile( Files[ 0 ] ).Title;
  fpgApplication.ProcessMessages;

  { TODO -oGraeme : Load previous notes here }
  for FileIndex:= 0 to Files.Count - 1 do
  begin
    HelpFile := THelpFile(Files[ FileIndex ]);
    LoadNotes( HelpFile );
  end;

  LoadContents;
  tvContents.Selection := tvContents.RootNode.FirstSubNode;
end;

procedure TMainForm.CloseFile;
var
  FileIndex: longint;
  lHelpFile: THelpFile;
begin
  WindowTitle := cTitle + ' - No file';
  tvContents.Selection := nil;
  tvContents.RootNode.Clear;
  tvContents.Invalidate;
  Memo1.Lines.Clear;

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
  text: PChar;
  ImageIndices: TList;
  LinkIndex: longint;
  Link: THelpLink;
  HelpFile: THelpFile;
  Topic: TTopic;
Begin
  writeln('DisplayTopic >>>>');
  if tvContents.Selection = nil then
  begin
    ShowMessage('You must select a topic first by clicking it.');
    Exit;  //==>
  end
  else
    Topic := TTopic(tvContents.Selection.Data);
  writeln('Got Topic from Treeview');

  Memo1.Lines.Clear;
  ImageIndices := TList.Create;

  writeln('Cleared memo...');

  HelpFile := TopicFile(Topic);
  if HelpFile = nil then
    raise Exception.Create('Failed to get active HelpFile from Topic');

  if HelpFile.HighlightWords <> nil then
    writeln('highlightwords is ok');
  Text := nil;
  writeln('Debug = ', Debug);
  if ImageIndices <> nil then
    writeln('ImageIndices initialized');
  Topic.GetText(HelpFile.HighlightWords,
                Debug,
                Text,
                ImageIndices );

  { TODO -oGraeme : We do not support images yet }
  ImageIndices.Free;

  Memo1.Lines.Text := Text;
  StrDispose( Text );
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnShow  := @MainFormShow;
  Files := TList.Create;
  { TODO -oGraeme : Make Debug a menu option }
  Debug := False;
end;

destructor TMainForm.Destroy;
begin
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
    ActivePageIndex := 0;
    TabOrder := 0;
    Align := alLeft;
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
    //    OnChange  := @tvContentsChange;
  end;

  tvIndex := TfpgTreeView.Create(tsIndex);
  with tvIndex do
  begin
    Name := 'tvIndex';
    SetPosition(4, 32, 242, 264);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Label1';
    TabOrder := 0;
  end;

  Splitter1 := TfpgSplitter.Create(bvlBody);
  with Splitter1 do
  begin
    Name := 'Splitter1';
    SetPosition(265, 4, 8, 284);
    Align := alLeft;
  end;

  Memo1 := TfpgMemo.Create(bvlBody);
  with Memo1 do
  begin
    Name := 'Memo1';
    SetPosition(276, 36, 244, 232);
    FontDesc := '#Edit1';
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
    AddMenuItem('Quit', '', @miFileQuitClicked);
  end;

  miSettings := TfpgPopupMenu.Create(self);
  with miSettings do
  begin
    Name := 'miSettings';
    SetPosition(292, 52, 132, 20);
    AddMenuItem('Options...', '', nil);
  end;

  miBookmarks := TfpgPopupMenu.Create(self);
  with miBookmarks do
  begin
    Name := 'miBookmarks';
    SetPosition(292, 76, 132, 20);
    AddMenuItem('Add..', '', nil);
    AddMenuItem('Show', '', nil);
  end;

  miHelp := TfpgPopupMenu.Create(self);
  with miHelp do
  begin
    Name := 'miHelp';
    SetPosition(292, 100, 132, 20);
    AddMenuItem('Contents...', '', nil);
    AddMenuItem('Help using help', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('Debug: Header', '', @miDebugHeader);
    AddMenuItem('-', '', nil);
    AddMenuItem('About fpGUI Toolkit', '', @miHelpAboutFPGui);
    AddMenuItem('Product Information...', '', @miHelpProdInfoClicked);
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
    SetPosition(4, 20, 242, 24);
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

  {@VFD_BODY_END: MainForm}
  {%endregion}

  // hook up the sub-menus.
  MainMenu.AddMenuItem('&File', nil).SubMenu := miFile;
  MainMenu.AddMenuItem('&Settings', nil).SubMenu := miSettings;
  MainMenu.AddMenuItem('&Bookmarks', nil).SubMenu := miBookmarks;
  MainMenu.AddMenuItem('&Help', nil).SubMenu := miHelp;

  // correct default visible tabsheet
  PageControl1.ActivePageIndex := 0;
end;


end.
