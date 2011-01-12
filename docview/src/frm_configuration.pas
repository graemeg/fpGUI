unit frm_configuration;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_tab, fpg_button,
  fpg_label, fpg_edit, fpg_panel, fpg_combobox, fpg_listbox, fpg_checkbox,
  fpg_editbtn, fpg_radiobutton;

type

  TConfigurationForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: ConfigurationForm}
    pcSettings: TfpgPageControl;
    btnSave: TfpgButton;
    btnCancel: TfpgButton;
    tsGeneral: TfpgTabSheet;
    tsFontsColor: TfpgTabSheet;
    tsIndex: TfpgTabSheet;
    Label1: TfpgLabel;
    Label2: TfpgLabel;
    pnlSearchHighlight: TfpgPanel;
    pnlNotesColor: TfpgPanel;
    lblIndexStyle: TfpgLabel;
    lblSearchDirs: TfpgLabel;
    btnSearchDirAdd: TfpgButton;
    lbSearchDirs: TfpgListBox;
    btnSearchDirDelete: TfpgButton;
    chkEscapeIPFSymbols: TfpgCheckBox;
    chkStartupHelp: TfpgCheckBox;
    chkOpenTOC: TfpgCheckBox;
    btnColorHighlight: TfpgButton;
    btnColorNotes: TfpgButton;
    btnResetColors: TfpgButton;
    edtFixedFont: TfpgFontEdit;
    edtNormalFont: TfpgFontEdit;
    rbIndexOrig: TfpgRadioButton;
    rbIndexAlpha: TfpgRadioButton;
    rbIndexBoth: TfpgRadioButton;
    lblScrollDistance: TfpgLabel;
    edtScrollDistance: TfpgEditInteger;
    lblPixels: TfpgLabel;
    {@VFD_HEAD_END: ConfigurationForm}
    btnHelp: TfpgButton;
    procedure ConfigurationFormShow(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject; NewActiveSheet: TfpgTabSheet);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSearchDirAddClicked(Sender: TObject);
    procedure btnSearchHighlightClicked(Sender: TObject);
    procedure btnNotesColorClicked(Sender: TObject);
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
  pcSettings.ActivePage := tsGeneral;
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
//    btnHelp.HelpContext := hcConfigGeneralTab;
  end
  else if NewActiveSheet = tsFontsColor then
  begin
//    btnHelp.HelpContext := hcConfigFontsColorTab;
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

procedure TConfigurationForm.btnNotesColorClicked(Sender: TObject);
begin
  pnlNotesColor.BackgroundColor := fpgSelectColorDialog(pnlNotesColor.BackgroundColor);
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
  edtScrollDistance.Value := Settings.ScrollDistance;
  lbSearchDirs.Items.Assign(Settings.SearchDirectories);
  chkEscapeIPFSymbols.Checked := Settings.IPFTopicSaveAsEscaped;
  chkStartupHelp.Checked  := Settings.StartupHelp;
  chkOpenTOC.Checked      := Settings.OpenWithExpandedContents;
  // Fonts & Color
  edtNormalFont.FontDesc  := Settings.NormalFontDesc;
  edtFixedFont.FontDesc   := Settings.FixedFontDesc;
  UpdateColorPanels;
  // Index
  rbIndexOrig.Checked   := Settings.IndexStyle = isFileOnly;
  rbIndexAlpha.Checked  := Settings.IndexStyle = isAlphabetical;
  rbIndexBoth.Checked   := Settings.IndexStyle = isFull;
end;

procedure TConfigurationForm.GuiToSettings;
begin
  // General
  if edtScrollDistance.Value < 1 then
    edtScrollDistance.Value := 75; // default
  if edtScrollDistance.Value > 400 then
    edtScrollDistance.Value := 400;
  Settings.ScrollDistance := edtScrollDistance.Value;
  Settings.SearchDirectories.Assign(lbSearchDirs.Items);
  Settings.IPFTopicSaveAsEscaped := chkEscapeIPFSymbols.Checked;
  Settings.StartupHelp := chkStartupHelp.Checked;
  Settings.OpenWithExpandedContents := chkOpenTOC.Checked;
  // Fonts & Color
  Settings.NormalFontDesc := edtNormalFont.FontDesc;
  Settings.FixedFontDesc := edtFixedFont.FontDesc;
  Settings.Colors[SearchHighlightTextColorIndex] := pnlSearchHighlight.BackgroundColor;
  Settings.Colors[NotesTextColorIndex] := pnlNotesColor.BackgroundColor;
  // Index
  if rbIndexOrig.Checked then
    Settings.IndexStyle := isFileOnly
  else if rbIndexAlpha.Checked then
    Settings.IndexStyle := isAlphabetical
  else if rbIndexBoth.Checked then
    Settings.IndexStyle := isFull;
end;

procedure TConfigurationForm.UpdateColorPanels;
begin
  pnlSearchHighlight.BackgroundColor := Settings.Colors[SearchHighlightTextColorIndex];
  pnlNotesColor.BackgroundColor := Settings.Colors[NotesTextColorIndex];
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
  ShowHint := True;
  WindowPosition := wpOneThirdDown;
  MinWidth := 513;
  MinHeight := 398;

  pcSettings := TfpgPageControl.Create(self);
  with pcSettings do
  begin
    Name := 'pcSettings';
    SetPosition(4, 4, 506, 388);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    TabOrder := 0;
    OnChange := @PageControl1Change;
  end;

  btnSave := TfpgButton.Create(self);
  with btnSave do
  begin
    Name := 'btnSave';
    SetPosition(344, 408, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Save';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 25;
    OnClick := @btnSaveClick;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(428, 408, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 26;
    OnClick := @btnCancelClick;
  end;

  tsGeneral := TfpgTabSheet.Create(pcSettings);
  with tsGeneral do
  begin
    Name := 'tsGeneral';
    SetPosition(3, 24, 500, 361);
    Text := 'General';
  end;

  tsFontsColor := TfpgTabSheet.Create(pcSettings);
  with tsFontsColor do
  begin
    Name := 'tsFontsColor';
    SetPosition(3, 24, 500, 361);
    Text := 'Fonts & Color';
  end;

  tsIndex := TfpgTabSheet.Create(pcSettings);
  with tsIndex do
  begin
    Name := 'tsIndex';
    SetPosition(3, 24, 500, 361);
    Text := 'Index';
  end;

  Label1 := TfpgLabel.Create(tsFontsColor);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(12, 20, 108, 19);
    FontDesc := '#Label1';
    Hint := '';
    Layout := tlCenter;
    Text := 'Normal Font';
  end;

  Label2 := TfpgLabel.Create(tsFontsColor);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(12, 52, 104, 19);
    FontDesc := '#Label1';
    Hint := '';
    Layout := tlCenter;
    Text := 'Fixed Font';
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

  pnlNotesColor := TfpgPanel.Create(tsFontsColor);
  with pnlNotesColor do
  begin
    Name := 'pnlNotesColor';
    SetPosition(12, 134, 360, 24);
    FontDesc := '#Label1';
    Hint := '';
    Style := bsLowered;
    Text := 'Notes/Annotations Color';
  end;

  lblIndexStyle := TfpgLabel.Create(tsIndex);
  with lblIndexStyle do
  begin
    Name := 'lblIndexStyle';
    SetPosition(12, 12, 224, 17);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Index style';
  end;

  lblSearchDirs := TfpgLabel.Create(tsGeneral);
  with lblSearchDirs do
  begin
    Name := 'lblSearchDirs';
    SetPosition(12, 65, 308, 17);
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
    TabOrder := 3;
    OnClick :=@btnSearchDirAddClicked;
  end;

  lbSearchDirs := TfpgListBox.Create(tsGeneral);
  with lbSearchDirs do
  begin
    Name := 'lbSearchDirs';
    SetPosition(12, 84, 388, 148);
    FontDesc := '#List';
    Hint := '';
    TabOrder := 5;
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
    TabOrder := 4;
  end;

  chkEscapeIPFSymbols := TfpgCheckBox.Create(tsGeneral);
  with chkEscapeIPFSymbols do
  begin
    Name := 'chkEscapeIPFSymbols';
    SetPosition(12, 244, 480, 20);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 6;
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
    TabOrder := 7;
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
    TabOrder := 8;
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
    TabOrder := 15;
    OnClick := @btnSearchHighlightClicked;
  end;

  btnColorNotes := TfpgButton.Create(tsFontsColor);
  with btnColorNotes do
  begin
    Name := 'btnColorNotes';
    SetPosition(384, 134, 80, 24);
    Text := 'Color';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 15;
    OnClick := @btnNotesColorClicked;
  end;

  btnResetColors := TfpgButton.Create(tsFontsColor);
  with btnResetColors do
  begin
    Name := 'btnResetColors';
    SetPosition(12, 328, 100, 24);
    Anchors := [anLeft,anBottom];
    Text := 'Reset Colors';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 16;
    OnClick := @ResetColorsButtonOnClick;
  end;

  edtFixedFont := TfpgFontEdit.Create(tsFontsColor);
  with edtFixedFont do
  begin
    Name := 'edtFixedFont';
    SetPosition(124, 48, 340, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FontDesc := '';
    TabOrder := 14;
  end;

  edtNormalFont := TfpgFontEdit.Create(tsFontsColor);
  with edtNormalFont do
  begin
    Name := 'edtNormalFont';
    SetPosition(124, 16, 340, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FontDesc := '';
    TabOrder := 13;
  end;

  rbIndexOrig := TfpgRadioButton.Create(tsIndex);
  with rbIndexOrig do
  begin
    Name := 'rbIndexOrig';
    SetPosition(24, 28, 280, 20);
    FontDesc := '#Label1';
    GroupIndex := 1;
    Hint := '';
    TabOrder := 21;
    Text := 'File Only (only entries specified in file)';
  end;

  rbIndexAlpha := TfpgRadioButton.Create(tsIndex);
  with rbIndexAlpha do
  begin
    Name := 'rbIndexAlpha';
    SetPosition(24, 48, 280, 20);
    FontDesc := '#Label1';
    GroupIndex := 1;
    Hint := '';
    TabOrder := 22;
    Text := 'Alphabetical listing of topics';
  end;

  rbIndexBoth := TfpgRadioButton.Create(tsIndex);
  with rbIndexBoth do
  begin
    Name := 'rbIndexBoth';
    SetPosition(24, 68, 280, 20);
    FontDesc := '#Label1';
    GroupIndex := 1;
    Hint := '';
    TabOrder := 23;
    Text := 'Both';
  end;

  lblScrollDistance := TfpgLabel.Create(tsGeneral);
  with lblScrollDistance do
  begin
    Name := 'lblScrollDistance';
    SetPosition(12, 12, 280, 17);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Mouse Wheel Scroll Distance';
  end;

  edtScrollDistance := TfpgEditInteger.Create(tsGeneral);
  with edtScrollDistance do
  begin
    Name := 'edtScrollDistance';
    SetPosition(12, 32, 72, 24);
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 2;
    Value := 0;
  end;

  lblPixels := TfpgLabel.Create(tsGeneral);
  with lblPixels do
  begin
    Name := 'lblPixels';
    SetPosition(88, 36, 80, 17);
    FontDesc := '#Label1';
    Hint := '';
    Text := '(pixels)';
  end;

  {@VFD_BODY_END: ConfigurationForm}
  {%endregion}

  // always reset pagecotrol
  pcSettings.ActivePageIndex := 0;

  //btnHelp := TfpgButton.Create(self);
  //with btnHelp do
  //begin
  //  Name := 'btnHelp';
  //  SetPosition(468, 356, 28, 24);
  //  Anchors := [anRight,anBottom];
  //  Text := '?';
  //  FontDesc := '#Label1';
  //  Hint := '';
  //  ImageName := '';
  //  TabOrder := 21;
  //  HelpType := htContext;
  //  OnClick := @btnHelpClick;
  //end;
end;


end.
