unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_menu, fpg_panel,
  fpg_button, fpg_splitter, fpg_tab, fpg_memo, fpg_label, fpg_grid,
  fpg_tree;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    pnlMenu: TfpgBevel;
    mainmenu: TfpgMenuBar;
    Bevel1: TfpgBevel;
    btnQuit: TfpgButton;
    btnOpen: TfpgButton;
    btnSave: TfpgButton;
    btnSaveAll: TfpgButton;
    pnlStatusBar: TfpgBevel;
    pnlClientArea: TfpgBevel;
    pnlWindow: TfpgPageControl;
    Splitter1: TfpgSplitter;
    pnlTool: TfpgPageControl;
    Splitter2: TfpgSplitter;
    pcEditor: TfpgPageControl;
    tsEditor1: TfpgTabSheet;
    tsEditor2: TfpgTabSheet;
    Memo1: TfpgMemo;
    lblStatus: TfpgLabel;
    mnuFile: TfpgPopupMenu;
    mnuEdit: TfpgPopupMenu;
    mnuSearch: TfpgPopupMenu;
    mnuView: TfpgPopupMenu;
    mnuProject: TfpgPopupMenu;
    mnuRun: TfpgPopupMenu;
    mnuTools: TfpgPopupMenu;
    mnuSettings: TfpgPopupMenu;
    mnuHelp: TfpgPopupMenu;
    tsMessages: TfpgTabSheet;
    tsScribble: TfpgTabSheet;
    tsTerminal: TfpgTabSheet;
    tsProject: TfpgTabSheet;
    tsFiles: TfpgTabSheet;
    tvProject: TfpgTreeView;
    grdFiles: TfpgFileGrid;
    grdMessages: TfpgStringGrid;
    memScribble: TfpgMemo;
    {@VFD_HEAD_END: MainForm}
    procedure   FormShow(Sender: TObject);
    procedure   btnQuitClicked(Sender: TObject);
    procedure   btnOpenFileClicked(Sender: TObject);
    procedure   miAboutFPGuiClicked(Sender: TObject);
    procedure   miAboutIDE(Sender: TObject);
    procedure   miConfigureIDE(Sender: TObject);
    procedure   miViewDebug(Sender: TObject);
    procedure   UpdateStatus(const AText: TfpgString);
    procedure   SetupProjectTree;
    procedure   SetupFilesGrid;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  fpg_dialogs
  ,fpg_utils
  ,frm_configureide
  ,frm_debug
  ;


const
  cFileFilterTemplate  = '%s (%s)|%s';
  cSourceFiles = '*.pas;*.pp;*.lpr';


{@VFD_NEWFORM_IMPL}

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnOpenFileClicked(Sender: TObject);
var
  s: TfpgString;
  f: TfpgString;
  n: TfpgTreeNode;
  i: integer;
  found: Boolean;
  ts: TfpgTabSheet;
begin
  s := SelectFileDialog(sfdOpen, Format(cFileFilterTemplate, ['Source Files', cSourceFiles, cSourceFiles]));
  if s <> '' then
  begin
    f := fpgExtractFileName(s);
    found := False;
    for i := 0 to pcEditor.PageCount-1 do
    begin
      if pcEditor.Pages[i].Text = f then
        found := True;
      if found then
        break;
    end;
    if found then
    begin
      // reuse existing tab
      TfpgMemo(pcEditor.Pages[i].Components[0]).Lines.BeginUpdate;
      TfpgMemo(pcEditor.Pages[i].Components[0]).Lines.LoadFromFile(s);
      TfpgMemo(pcEditor.Pages[i].Components[0]).Lines.EndUpdate;
      pcEditor.ActivePageIndex := i;
    end
    else
    begin
      // we need a new tabsheet
      ts := pcEditor.AppendTabSheet(f);
      CreateMemo(ts, 1, 1, 200, 20).Align := alClient;
      TfpgMemo(ts.Components[0]).Lines.BeginUpdate;
      TfpgMemo(ts.Components[0]).Lines.LoadFromFile(s);
      TfpgMemo(ts.Components[0]).Lines.EndUpdate;
    end;
    UpdateStatus(s);
//    pcEditor.ActivePage.Text := fpgExtractFileName(s);
    n := tvProject.RootNode.FindSubNode('Units', True);
    if Assigned(n) then
    begin
      n := n.AppendText(f);
      tvProject.Selection := n;
    end;
  end;
end;

procedure TMainForm.miAboutFPGuiClicked(Sender: TObject);
begin
  TfpgMessageDialog.AboutFPGui;
end;

procedure TMainForm.miAboutIDE(Sender: TObject);
begin
  TfpgMessageDialog.Information('About fpGUI IDE', 'Created by Graeme Geldenhuys');
end;

procedure TMainForm.miConfigureIDE(Sender: TObject);
begin
  DisplayConfigureIDE;
end;

procedure TMainForm.miViewDebug(Sender: TObject);
begin
  if not Assigned(DebugForm) then
    fpgApplication.CreateForm(TDebugForm, DebugForm);
  DebugForm.Show;
end;

procedure TMainForm.UpdateStatus(const AText: TfpgString);
begin
  lblStatus.Text := AText;
end;

procedure TMainForm.SetupProjectTree;
begin
  tvProject.RootNode.AppendText('Units');
  tvProject.RootNode.AppendText('Images');
  tvProject.RootNode.AppendText('Help Files');
  tvProject.RootNode.AppendText('Text');
  tvProject.RootNode.AppendText('Other');
end;

procedure TMainForm.SetupFilesGrid;
begin
  grdFiles.FileList.FileMask := AllFilesMask;
  grdFiles.FileList.ShowHidden := False;
  grdFiles.FileList.ReadDirectory;
  grdFiles.FileList.Sort(soFileName);
  grdFiles.Invalidate;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  // outer containers
  pnlMenu.Align := alTop;
  pnlStatusBar.Align := alBottom;
  pnlClientArea.Align := alClient;

  // inner containers
  pnlWindow.Align := alBottom;
  Splitter1.Align := alBottom;
  pnlTool.Align := alLeft;
  Splitter2.Align := alLeft;
  pcEditor.Align := alClient;

  pnlClientArea.Realign;

  SetupProjectTree;
  SetupFilesGrid;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnShow  := @FormShow;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(310, 206, 650, 358);
  WindowTitle := 'fpGUI IDE - %s';
  Hint := '';
  WindowPosition := wpOneThirdDown;

  pnlMenu := TfpgBevel.Create(self);
  with pnlMenu do
  begin
    Name := 'pnlMenu';
    SetPosition(0, 0, 648, 54);
    Hint := '';
    Shape := bsSpacer;
  end;

  mainmenu := TfpgMenuBar.Create(pnlMenu);
  with mainmenu do
  begin
    Name := 'mainmenu';
    SetPosition(0, 0, 648, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  Bevel1 := TfpgBevel.Create(pnlMenu);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(0, 24, 648, 28);
    Anchors := [anLeft,anRight,anTop];
    Hint := '';
    Shape := bsSpacer;
  end;

  btnQuit := TfpgButton.Create(Bevel1);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(4, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'stdimg.quit';
    TabOrder := 1;
    OnClick  := @btnQuitClicked;
  end;

  btnOpen := TfpgButton.Create(Bevel1);
  with btnOpen do
  begin
    Name := 'btnOpen';
    SetPosition(28, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'stdimg.open';
    TabOrder := 2;
    OnClick := @btnOpenFileClicked;
  end;

  btnSave := TfpgButton.Create(Bevel1);
  with btnSave do
  begin
    Name := 'btnSave';
    SetPosition(56, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'stdimg.save';
    TabOrder := 3;
  end;

  btnSaveAll := TfpgButton.Create(Bevel1);
  with btnSaveAll do
  begin
    Name := 'btnSaveAll';
    SetPosition(80, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'stdimg.saveall';
    TabOrder := 4;
  end;

  pnlStatusBar := TfpgBevel.Create(self);
  with pnlStatusBar do
  begin
    Name := 'pnlStatusBar';
    SetPosition(0, 338, 648, 20);
    Anchors := [anLeft,anRight,anBottom];
    Hint := '';
    Style := bsLowered;
  end;

  pnlClientArea := TfpgBevel.Create(self);
  with pnlClientArea do
  begin
    Name := 'pnlClientArea';
    SetPosition(0, 57, 648, 248);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    Shape := bsSpacer;
  end;

  pnlWindow := TfpgPageControl.Create(pnlClientArea);
  with pnlWindow do
  begin
    Name := 'pnlWindow';
    SetPosition(192, 156, 264, 84);
    ActivePageIndex := 0;
    Hint := '';
    TabOrder := 1;
    TabPosition := tpLeft;
  end;

  Splitter1 := TfpgSplitter.Create(pnlClientArea);
  with Splitter1 do
  begin
    Name := 'Splitter1';
    SetPosition(152, 140, 352, 8);
  end;

  pnlTool := TfpgPageControl.Create(pnlClientArea);
  with pnlTool do
  begin
    Name := 'pnlTool';
    SetPosition(12, 4, 140, 136);
    ActivePageIndex := 0;
    Hint := '';
    TabOrder := 3;
  end;

  Splitter2 := TfpgSplitter.Create(pnlClientArea);
  with Splitter2 do
  begin
    Name := 'Splitter2';
    SetPosition(160, 8, 8, 116);
  end;

  pcEditor := TfpgPageControl.Create(pnlClientArea);
  with pcEditor do
  begin
    Name := 'pcEditor';
    SetPosition(208, 8, 324, 120);
    ActivePageIndex := 0;
    Hint := '';
    TabOrder := 5;
    TabPosition := tpRight;
  end;

  tsEditor1 := TfpgTabSheet.Create(pcEditor);
  with tsEditor1 do
  begin
    Name := 'tsEditor1';
    SetPosition(3, 3, 267, 114);
    Text := 'Editor1';
  end;

  tsEditor2 := TfpgTabSheet.Create(pcEditor);
  with tsEditor2 do
  begin
    Name := 'tsEditor2';
    SetPosition(3, 3, 318, 114);
    Text := 'Editor2';
  end;

  Memo1 := TfpgMemo.Create(tsEditor1);
  with Memo1 do
  begin
    Name := 'Memo1';
    SetPosition(64, 16, 120, 52);
    Hint := '';
    FontDesc := '#Edit2';
    TabOrder := 1;
    Align := alClient;
  end;

  lblStatus := TfpgLabel.Create(pnlStatusBar);
  with lblStatus do
  begin
    Name := 'lblStatus';
    SetPosition(4, 2, 636, 16);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    Text := '';
  end;

  mnuFile := TfpgPopupMenu.Create(self);
  with mnuFile do
  begin
    Name := 'mnuFile';
    SetPosition(476, 56, 172, 20);
    AddMenuItem('New...', '', nil);
    AddMenuItem('Open...', '', nil);
    AddMenuItem('Open Recent', '', nil);
    AddMenuItem('Save...', '', nil);
    AddMenuItem('Save As...', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('Quit', '', @btnQuitClicked);
  end;

  mnuEdit := TfpgPopupMenu.Create(self);
  with mnuEdit do
  begin
    Name := 'mnuEdit';
    SetPosition(476, 24, 172, 20);
    AddMenuItem('Cut', '', nil);
    AddMenuItem('Copy', '', nil);
    AddMenuItem('Paste', '', nil);
  end;

  mnuSearch := TfpgPopupMenu.Create(self);
  with mnuSearch do
  begin
    Name := 'mnuSearch';
    SetPosition(476, 98, 172, 20);
    AddMenuItem('Find...', '', nil);
    AddMenuItem('Find in Files...', '', nil);
    AddMenuItem('Procedure List...', '', nil);
  end;

  mnuView := TfpgPopupMenu.Create(self);
  with mnuView do
  begin
    Name := 'mnuView';
    SetPosition(476, 119, 172, 20);
    AddMenuItem('Todo List...', '', nil);
    AddMenuItem('Debug Windows', '', @miViewDebug);
  end;

  mnuProject := TfpgPopupMenu.Create(self);
  with mnuProject do
  begin
    Name := 'mnuProject';
    SetPosition(476, 140, 172, 20);
    AddMenuItem('Options...', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('New (empty)...', '', nil);
    AddMenuItem('New from Template...', '', nil);
    AddMenuItem('Open...', '', nil);
    AddMenuItem('Open Recent', '', nil);
    AddMenuItem('Save...', '', nil);
    AddMenuItem('Save As...', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('View Source', '', nil);
    AddMenuItem('Add editor file to Project', '', nil);
  end;

  mnuRun := TfpgPopupMenu.Create(self);
  with mnuRun do
  begin
    Name := 'mnuRun';
    SetPosition(476, 161, 172, 20);
    AddMenuItem('Make', 'Ctrl+F9', nil);
    AddMenuItem('Build All', 'Ctrl+Shift+F9', nil);
    AddMenuItem('Make 1', 'Ctrl+Alt+1', nil);
    AddMenuItem('Make 2', 'Ctrl+Alt+2', nil);
    AddMenuItem('Make 3', 'Ctrl+Alt+3', nil);
    AddMenuItem('Make 4', 'Ctrl+Alt+4', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('Run', 'F9', nil);
    AddMenuItem('Run Parameters...', '', nil);
  end;

  mnuTools := TfpgPopupMenu.Create(self);
  with mnuTools do
  begin
    Name := 'mnuTools';
    SetPosition(476, 182, 172, 20);
    AddMenuItem('fpGUI UI Designer...', '', nil);
    AddMenuItem('fpGUI DocView...', '', nil);
  end;

  mnuSettings := TfpgPopupMenu.Create(self);
  with mnuSettings do
  begin
    Name := 'mnuSettings';
    SetPosition(476, 203, 172, 20);
    AddMenuItem('Configure IDE...', '', @miConfigureIDE);
  end;

  mnuHelp := TfpgPopupMenu.Create(self);
  with mnuHelp do
  begin
    Name := 'mnuHelp';
    SetPosition(476, 224, 172, 20);
    AddMenuItem('Contents...', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('About fpGUI Toolkit...', '', @miAboutFPGuiClicked);
    AddMenuItem('About fpGUI IDE...', '', @miAboutIDE);
  end;

  tsMessages := TfpgTabSheet.Create(pnlWindow);
  with tsMessages do
  begin
    Name := 'tsMessages';
    SetPosition(3, 24, 258, 57);
    Text := 'Messages';
  end;

  tsScribble := TfpgTabSheet.Create(pnlWindow);
  with tsScribble do
  begin
    Name := 'tsScribble';
    SetPosition(3, 24, 258, 57);
    Text := 'Scribble';
  end;

  tsTerminal := TfpgTabSheet.Create(pnlWindow);
  with tsTerminal do
  begin
    Name := 'tsTerminal';
    SetPosition(3, 24, 258, 57);
    Text := 'Terminal';
  end;

  tsProject := TfpgTabSheet.Create(pnlTool);
  with tsProject do
  begin
    Name := 'tsProject';
    SetPosition(3, 24, 134, 109);
    Text := 'Project';
  end;

  tsFiles := TfpgTabSheet.Create(pnlTool);
  with tsFiles do
  begin
    Name := 'tsFiles';
    SetPosition(3, 24, 134, 109);
    Text := 'Files';
  end;

  tvProject := TfpgTreeView.Create(tsProject);
  with tvProject do
  begin
    Name := 'tvProject';
    SetPosition(1, 1, 132, 107);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 1;
  end;

  grdFiles := TfpgFileGrid.Create(tsFiles);
  with grdFiles do
  begin
    Name := 'grdFiles';
    SetPosition(1, 1, 131, 106);
    Anchors := [anLeft,anRight,anTop,anBottom];
  end;

  grdMessages := TfpgStringGrid.Create(tsMessages);
  with grdMessages do
  begin
    Name := 'grdMessages';
    SetPosition(0, 4, 258, 52);
    Anchors := [anLeft,anRight,anTop,anBottom];
    AddColumn('New', 258, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 3;
    RowSelect := True;
    ShowHeader := False;
    TabOrder := 1;
  end;

  memScribble := TfpgMemo.Create(tsScribble);
  with memScribble do
  begin
    Name := 'memScribble';
    SetPosition(0, 4, 257, 52);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    Lines.Add('Make notes, use it as a clipboard');
    Lines.Add('or type whatever you want...');
    FontDesc := '#Edit2';
    TabOrder := 1;
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}

  mainmenu.AddMenuItem('&File', nil).SubMenu := mnuFile;
  mainmenu.AddMenuItem('&Edit', nil).SubMenu := mnuEdit;
  mainmenu.AddMenuItem('&Search', nil).SubMenu := mnuSearch;
  mainmenu.AddMenuItem('&View', nil).SubMenu := mnuView;
  mainmenu.AddMenuItem('&Project', nil).SubMenu := mnuProject;
  mainmenu.AddMenuItem('&Run', nil).SubMenu := mnuRun;
  mainmenu.AddMenuItem('&Tools', nil).SubMenu := mnuTools;
  mainmenu.AddMenuItem('&Settings', nil).SubMenu := mnuSettings;
  mainmenu.AddMenuItem('&Help', nil).SubMenu := mnuHelp;
end;


end.
