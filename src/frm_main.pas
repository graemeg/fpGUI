unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_menu, fpg_panel,
  fpg_button, fpg_splitter, fpg_tab, fpg_memo, fpg_label;

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
    pnlWindow: TfpgPanel;
    Splitter1: TfpgSplitter;
    pnlTool: TfpgPanel;
    Splitter2: TfpgSplitter;
    pcEditor: TfpgPageControl;
    tsEditor1: TfpgTabSheet;
    tsEditor2: TfpgTabSheet;
    Memo1: TfpgMemo;
    lblStatus: TfpgLabel;
    {@VFD_HEAD_END: MainForm}
    procedure   FormShow(Sender: TObject);
    procedure   btnQuitClicked(Sender: TObject);
    procedure   btnOpenFileClicked(Sender: TObject);
    procedure   UpdateStatus(const AText: TfpgString);
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  fpg_dialogs
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
begin
  s := SelectFileDialog(sfdOpen, Format(cFileFilterTemplate, ['Source Files', cSourceFiles, cSourceFiles]));
  if s <> '' then
  begin
    Memo1.Lines.BeginUpdate;
    Memo1.Lines.LoadFromFile(s);
    Memo1.Lines.EndUpdate;
    UpdateStatus(s);
  end;
end;

procedure TMainForm.UpdateStatus(const AText: TfpgString);
begin
  lblStatus.Text := AText;
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
  SetPosition(311, 121, 650, 358);
  WindowTitle := 'fpGUI IDE - %s';
  Hint := '';
  WindowPosition := wpOneThirdDown;

  pnlMenu := TfpgBevel.Create(self);
  with pnlMenu do
  begin
    Name := 'pnlMenu';
    SetPosition(0, 0, 648, 70);
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
    SetPosition(0, 53, 648, 280);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    Shape := bsSpacer;
  end;

  pnlWindow := TfpgPanel.Create(pnlClientArea);
  with pnlWindow do
  begin
    Name := 'pnlWindow';
    SetPosition(200, 192, 212, 80);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'WindowPanel';
  end;

  Splitter1 := TfpgSplitter.Create(pnlClientArea);
  with Splitter1 do
  begin
    Name := 'Splitter1';
    SetPosition(128, 172, 352, 8);
  end;

  pnlTool := TfpgPanel.Create(pnlClientArea);
  with pnlTool do
  begin
    Name := 'pnlTool';
    SetPosition(28, 24, 112, 136);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'ToolPanel';
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
    SetPosition(244, 28, 324, 120);
    ActivePageIndex := 0;
    Hint := '';
    TabOrder := 5;
  end;

  tsEditor1 := TfpgTabSheet.Create(pcEditor);
  with tsEditor1 do
  begin
    Name := 'tsEditor1';
    SetPosition(3, 24, 318, 93);
    Text := 'Editor1';
  end;

  tsEditor2 := TfpgTabSheet.Create(pcEditor);
  with tsEditor2 do
  begin
    Name := 'tsEditor2';
    SetPosition(3, 24, 318, 93);
    Text := 'Editor2';
  end;

  Memo1 := TfpgMemo.Create(tsEditor1);
  with Memo1 do
  begin
    Name := 'Memo1';
    SetPosition(64, 16, 120, 52);
    Hint := '';
    FontDesc := '#Edit1';
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

  {@VFD_BODY_END: MainForm}
  {%endregion}
end;


end.
