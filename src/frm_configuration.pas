unit frm_configuration;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_tab, fpg_button,
  fpg_label, fpg_edit, fpg_panel, fpg_combobox, fpg_listbox;

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
    edtNormalFont: TfpgEdit;
    btnNormalFont: TfpgButton;
    Label2: TfpgLabel;
    edtFixedFont: TfpgEdit;
    btnFixedFont: TfpgButton;
    btnHelp: TfpgButton;
    Panel1: TfpgPanel;
    cbIndexStyle: TfpgComboBox;
    lblIndexStyle: TfpgLabel;
    lblSearchDirs: TfpgLabel;
    btnSearchDirAdd: TfpgButton;
    lbSearchDirs: TfpgListBox;
    btnSearchDirDelete: TfpgButton;
    {@VFD_HEAD_END: ConfigurationForm}
    procedure ConfigurationFormShow(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject; NewActiveSheet: TfpgTabSheet);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSearchDirAddClicked(Sender: TObject);
    procedure SettingsToGui;
    procedure GuiToSettings;
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

procedure TConfigurationForm.SettingsToGui;
begin
  // General
  cbIndexStyle.FocusItem := Ord(Settings.IndexStyle);
  lbSearchDirs.Items.Assign(Settings.SearchDirectories);
  // Fonts & Color
  edtNormalFont.Text := Settings.NormalFont.FontDesc;
  edtFixedFont.Text := Settings.FixedFont.FontDesc;
end;

procedure TConfigurationForm.GuiToSettings;
begin
  // General
  Settings.IndexStyle := TIndexStyle(cbIndexStyle.FocusItem);
  Settings.SearchDirectories.Assign(lbSearchDirs.Items);
  // Fonts & Color
  Settings.NormalFont.Free;
  Settings.NormalFont := fpgGetFont(edtNormalFont.Text);
  Settings.FixedFont.Free;
  Settings.FixedFont := fpgGetFont(edtFixedFont.Text);
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
  SetPosition(310, 157, 515, 439);
  WindowTitle := 'Configuration';
  WindowPosition := wpOneThirdDown;

  PageControl1 := TfpgPageControl.Create(self);
  with PageControl1 do
  begin
    Name := 'PageControl1';
    SetPosition(4, 4, 506, 388);
    Anchors := [anLeft,anRight,anTop,anBottom];
    ActivePageIndex := 1;
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
    TabOrder := 1;
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
    TabOrder := 2;
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
    SetPosition(12, 20, 108, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Normal Font';
  end;

  edtNormalFont := TfpgEdit.Create(tsFontsColor);
  with edtNormalFont do
  begin
    Name := 'edtNormalFont';
    SetPosition(124, 16, 248, 24);
    Anchors := [anLeft,anRight,anTop];
    TabOrder := 1;
    Text := '';
    FontDesc := '#Edit1';
  end;

  btnNormalFont := TfpgButton.Create(tsFontsColor);
  with btnNormalFont do
  begin
    Name := 'btnNormalFont';
    SetPosition(384, 16, 80, 24);
    Anchors := [anRight,anTop];
    Text := 'Select...';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
  end;

  Label2 := TfpgLabel.Create(tsFontsColor);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(12, 52, 104, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Fixed Font';
  end;

  edtFixedFont := TfpgEdit.Create(tsFontsColor);
  with edtFixedFont do
  begin
    Name := 'edtFixedFont';
    SetPosition(124, 48, 248, 24);
    Anchors := [anLeft,anRight,anTop];
    TabOrder := 4;
    Text := '';
    FontDesc := '#Edit1';
  end;

  btnFixedFont := TfpgButton.Create(tsFontsColor);
  with btnFixedFont do
  begin
    Name := 'btnFixedFont';
    SetPosition(384, 48, 80, 24);
    Anchors := [anRight,anTop];
    Text := 'Select...';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 5;
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
    TabOrder := 6;
    HelpType := htContext;
    OnClick := @btnHelpClick;
  end;

  Panel1 := TfpgPanel.Create(tsFontsColor);
  with Panel1 do
  begin
    Name := 'Panel1';
    SetPosition(128, 116, 204, 28);
    FontDesc := '#Label1';
    Style := bsLowered;
    Text := 'Panel';
  end;

  cbIndexStyle := TfpgComboBox.Create(tsGeneral);
  with cbIndexStyle do
  begin
    Name := 'cbIndexStyle';
    SetPosition(12, 32, 160, 22);
    FontDesc := '#List';
    Items.Add('Alphabetical');
    Items.Add('FileOnly');
    Items.Add('Full');
    TabOrder := 0;
  end;

  lblIndexStyle := TfpgLabel.Create(tsGeneral);
  with lblIndexStyle do
  begin
    Name := 'lblIndexStyle';
    SetPosition(12, 12, 212, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Index style';
  end;

  lblSearchDirs := TfpgLabel.Create(tsGeneral);
  with lblSearchDirs do
  begin
    Name := 'lblSearchDirs';
    SetPosition(12, 64, 216, 16);
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
    TabOrder := 4;
    OnClick :=@btnSearchDirAddClicked;
  end;

  lbSearchDirs := TfpgListBox.Create(tsGeneral);
  with lbSearchDirs do
  begin
    Name := 'lbSearchDirs';
    SetPosition(12, 84, 388, 148);
    FontDesc := '#List';
    HotTrack := False;
    PopupFrame := False;
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
    TabOrder := 5;
  end;

  {@VFD_BODY_END: ConfigurationForm}
  {%endregion}
end;


end.
