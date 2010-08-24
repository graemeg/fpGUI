unit frm_configuration;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_tab, fpg_button,
  fpg_label, fpg_edit, fpg_panel, fpg_combobox, fpg_listbox, fpg_checkbox,
  fpg_editbtn;

type

  TConfigurationForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: ConfigurationForm}
    PageControl1: TfpgPageControl;
    btnSave: TfpgButton;
    btnCancel: TfpgButton;
    tsGeneral: TfpgTabSheet;
    tsFontsColor: TfpgTabSheet;
    Label1: TfpgLabel;
    Label2: TfpgLabel;
    btnHelp: TfpgButton;
    pnlSearchHighlight: TfpgPanel;
    cbIndexStyle: TfpgComboBox;
    lblIndexStyle: TfpgLabel;
    lblSearchDirs: TfpgLabel;
    btnSearchDirAdd: TfpgButton;
    lbSearchDirs: TfpgListBox;
    btnSearchDirDelete: TfpgButton;
    chkEscapeIPFSymbols: TfpgCheckBox;
    chkStartupHelp: TfpgCheckBox;
    chkOpenTOC: TfpgCheckBox;
    btnColorHighlight: TfpgButton;
    btnResetColors: TfpgButton;
    edtFixedFont: TfpgFontEdit;
    edtNormalFont: TfpgFontEdit;
    {@VFD_HEAD_END: ConfigurationForm}
    procedure ConfigurationFormShow(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject; NewActiveSheet: TfpgTabSheet);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSearchDirAddClicked(Sender: TObject);
    procedure btnSearchHighlightClicked(Sender: TObject);
    procedure ResetColorsButtonOnClick(Sender: TObject);
    procedure SettingsToGui;
    procedure GuiToSettings;
    procedure UpdateColorPanels;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

procedure ShowConfigForm;

implementation

uses
  fpg_dialogs, SettingsUnit, dvConstants;

procedure ShowConfigForm;
var
  frm: TConfigurationForm;
begin
  frm := TConfigurationForm.Create(nil);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

{@VFD_NEWFORM_IMPL}

procedure TConfigurationForm.ConfigurationFormShow(Sender: TObject);
begin
  SettingsToGui;
  PageControl1.ActivePage := tsGeneral;
  // programatically seting a tab does not fire OnChange event, so we do it mantually
  PageControl1Change(self, tsGeneral);
end;

procedure TConfigurationForm.btnHelpClick(Sender: TObject);
begin
  ShowMessage(IntToStr(btnHelp.HelpContext));
end;

procedure TConfigurationForm.PageControl1Change(Sender: TObject;
  NewActiveSheet: TfpgTabSheet);
begin
  if NewActiveSheet = tsGeneral then
  begin
    btnHelp.HelpContext := hcConfigGeneralTab;
  end
  else if NewActiveSheet = tsFontsColor then
  begin
    btnHelp.HelpContext := hcConfigFontsColorTab;
  end;
end;

procedure TConfigurationForm.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TConfigurationForm.btnSaveClick(Sender: TObject);
begin
  GuiToSettings;
  SaveSettings;
  ModalResult := mrOK;
end;

procedure TConfigurationForm.btnSearchDirAddClicked(Sender: TObject);
var
  s: TfpgString;
begin
  s := SelectDirDialog('');
  if s <> '' then
    lbSearchDirs.Items.Add(s);
end;

procedure TConfigurationForm.btnSearchHighlightClicked(Sender: TObject);
begin
  pnlSearchHighlight.BackgroundColor := fpgSelectColorDialog(pnlSearchHighlight.BackgroundColor);
end;

procedure TConfigurationForm.ResetColorsButtonOnClick(Sender: TObject);
var
  i: longint;
Begin
  // restore default colors
  for i := 0 to NumColorSettings - 1 do
  begin
    Settings.Colors[i] := DefaultColors[i];
  end;
  UpdateColorPanels;
End;

procedure TConfigurationForm.SettingsToGui;
begin
  // General
  cbIndexStyle.FocusItem  := Ord(Settings.IndexStyle);
  lbSearchDirs.Items.Assign(Settings.SearchDirectories);
  chkEscapeIPFSymbols.Checked := Settings.IPFTopicSaveAsEscaped;
  chkStartupHelp.Checked  := Settings.StartupHelp;
  chkOpenTOC.Checked      := Settings.OpenWithExpandedContents;
  // Fonts & Color
  edtNormalFont.FontDesc  := Settings.NormalFont.FontDesc;
  edtFixedFont.FontDesc   := Settings.FixedFont.FontDesc;
  UpdateColorPanels;
end;

procedure TConfigurationForm.GuiToSettings;
begin
  // General
  Settings.IndexStyle := TIndexStyle(cbIndexStyle.FocusItem);
  Settings.SearchDirectories.Assign(lbSearchDirs.Items);
  Settings.IPFTopicSaveAsEscaped := chkEscapeIPFSymbols.Checked;
  Settings.StartupHelp := chkStartupHelp.Checked;
  Settings.OpenWithExpandedContents := chkOpenTOC.Checked;
  // Fonts & Color
  Settings.NormalFont.Free;
  Settings.NormalFont := fpgGetFont(edtNormalFont.FontDesc);
  Settings.FixedFont.Free;
  Settings.FixedFont := fpgGetFont(edtFixedFont.FontDesc);
  Settings.Colors[SearchHighlightTextColorIndex] := pnlSearchHighlight.BackgroundColor;
end;

procedure TConfigurationForm.UpdateColorPanels;
begin
  pnlSearchHighlight.BackgroundColor := Settings.Colors[SearchHighlightTextColorIndex];
end;

constructor TConfigurationForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnShow := @ConfigurationFormShow;
end;

procedure TConfigurationForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: ConfigurationForm}
  Name := 'ConfigurationForm';
  SetPosition(402, 189, 515, 439);
  WindowTitle := 'Configuration';
  Hint := '';
  WindowPosition := wpOneThirdDown;

  PageControl1 := TfpgPageControl.Create(self);
  with PageControl1 do
  begin
    Name := 'PageControl1';
    SetPosition(4, 4, 506, 388);
    Anchors := [anLeft,anRight,anTop,anBottom];
    ActivePageIndex := 0;
    Hint := '';
    TabOrder := 0;
    OnChange := @PageControl1Change;
  end;

  btnSave := TfpgButton.Create(self);
  with btnSave do
  begin
    Name := 'btnSave';
    SetPosition(344, 408, 80, 24);
    Text := 'Save';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 19;
    OnClick := @btnSaveClick;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(428, 408, 80, 24);
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 20;
    OnClick := @btnCancelClick;
  end;

  tsGeneral := TfpgTabSheet.Create(PageControl1);
  with tsGeneral do
  begin
    Name := 'tsGeneral';
    SetPosition(3, 24, 500, 361);
    Text := 'General';
  end;

  tsFontsColor := TfpgTabSheet.Create(PageControl1);
  with tsFontsColor do
  begin
    Name := 'tsFontsColor';
    SetPosition(3, 24, 500, 361);
    Text := 'Fonts & Color';
  end;

  Label1 := TfpgLabel.Create(tsFontsColor);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(12, 20, 108, 19);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Normal Font';
  end;

  Label2 := TfpgLabel.Create(tsFontsColor);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(12, 52, 104, 19);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Fixed Font';
  end;

  btnHelp := TfpgButton.Create(self);
  with btnHelp do
  begin
    Name := 'btnHelp';
    SetPosition(468, 356, 28, 24);
    Anchors := [anRight,anBottom];
    Text := '?';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 21;
    HelpType := htContext;
    OnClick := @btnHelpClick;
  end;

  pnlSearchHighlight := TfpgPanel.Create(tsFontsColor);
  with pnlSearchHighlight do
  begin
    Name := 'pnlSearchHighlight';
    SetPosition(12, 104, 360, 24);
    FontDesc := '#Label1';
    Hint := '';
    Style := bsLowered;
    Text := 'Search Highlight Color';
  end;

  cbIndexStyle := TfpgComboBox.Create(tsGeneral);
  with cbIndexStyle do
  begin
    Name := 'cbIndexStyle';
    SetPosition(12, 32, 160, 25);
    FontDesc := '#List';
    Hint := '';
    Items.Add('Alphabetical');
    Items.Add('FileOnly');
    Items.Add('Full');
    TabOrder := 2;
  end;

  lblIndexStyle := TfpgLabel.Create(tsGeneral);
  with lblIndexStyle do
  begin
    Name := 'lblIndexStyle';
    SetPosition(12, 11, 296, 19);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Index style';
  end;

  lblSearchDirs := TfpgLabel.Create(tsGeneral);
  with lblSearchDirs do
  begin
    Name := 'lblSearchDirs';
    SetPosition(12, 63, 308, 19);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Search directories';
  end;

  btnSearchDirAdd := TfpgButton.Create(tsGeneral);
  with btnSearchDirAdd do
  begin
    Name := 'btnSearchDirAdd';
    SetPosition(408, 84, 80, 24);
    Text := 'Add...';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 5;
    OnClick :=@btnSearchDirAddClicked;
  end;

  lbSearchDirs := TfpgListBox.Create(tsGeneral);
  with lbSearchDirs do
  begin
    Name := 'lbSearchDirs';
    SetPosition(12, 84, 388, 148);
    FontDesc := '#List';
    Hint := '';
    HotTrack := False;
    PopupFrame := False;
    TabOrder := 6;
    Items.Duplicates := dupIgnore;
  end;

  btnSearchDirDelete := TfpgButton.Create(tsGeneral);
  with btnSearchDirDelete do
  begin
    Name := 'btnSearchDirDelete';
    SetPosition(408, 116, 80, 24);
    Text := 'Remove...';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 7;
  end;

  chkEscapeIPFSymbols := TfpgCheckBox.Create(tsGeneral);
  with chkEscapeIPFSymbols do
  begin
    Name := 'chkEscapeIPFSymbols';
    SetPosition(12, 244, 480, 20);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 8;
    Text := 'Escape symbols when saving topics as IPF text';
  end;

  chkStartupHelp := TfpgCheckBox.Create(tsGeneral);
  with chkStartupHelp do
  begin
    Name := 'chkStartupHelp';
    SetPosition(12, 268, 480, 20);
    Anchors := [anLeft,anRight,anTop];
    Checked := True;
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 9;
    Text := 'Show DocView help at startup if no files opened';
  end;

  chkOpenTOC := TfpgCheckBox.Create(tsGeneral);
  with chkOpenTOC do
  begin
    Name := 'chkOpenTOC';
    SetPosition(12, 292, 476, 20);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 10;
    Text := 'Open files with contents expanded';
  end;

  btnColorHighlight := TfpgButton.Create(tsFontsColor);
  with btnColorHighlight do
  begin
    Name := 'btnColorHighlight';
    SetPosition(384, 104, 80, 24);
    Text := 'Color';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 17;
    OnClick := @btnSearchHighlightClicked;
  end;

  btnResetColors := TfpgButton.Create(tsFontsColor);
  with btnResetColors do
  begin
    Name := 'btnResetColors';
    SetPosition(12, 328, 100, 24);
    Text := 'Reset Colors';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 18;
    OnClick := @ResetColorsButtonOnClick;
  end;

  edtFixedFont := TfpgFontEdit.Create(tsFontsColor);
  with edtFixedFont do
  begin
    Name := 'edtFixedFont';
    SetPosition(124, 48, 340, 27);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '';
    TabOrder := 15;
  end;

  edtNormalFont := TfpgFontEdit.Create(tsFontsColor);
  with edtNormalFont do
  begin
    Name := 'edtNormalFont';
    SetPosition(124, 16, 340, 27);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '';
    TabOrder := 14;
  end;

  {@VFD_BODY_END: ConfigurationForm}
  {%endregion}

  // always reset pagecotrol
  PageControl1.ActivePageIndex := 0;
end;


end.
