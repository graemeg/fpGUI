unit mainform;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_edit, fpg_button,
  fpg_editcombo, fpg_editbtn, fpg_combobox, fpg_grid, fpg_label, fpg_checkbox,
  customedits;

type

  { TfrmMain }

  TfrmMain = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: frmMain}
    edit1: TfpgEdit;
    btnQuit: TfpgButton;
    editCombo1: TfpgEditCombo;
    editBtn1: TfpgEditButton;
    editDir1: TfpgDirectoryEdit;
    combo1: TfpgComboBox;
    labelInfo: TfpgLabel;
    cbFocused: TfpgCheckBox;
    editColor: TCustomColorEdit;
    editAl: TCustomAlignmentEdit;
    editFont: TCustomFontEdit;
    {@VFD_HEAD_END: frmMain}
    procedure QuitClicked(Sender: TObject);
    procedure ChangeFocusState(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TfrmMain.ChangeFocusState(Sender: TObject);
begin
  if cbFocused.Checked
  then edit1.Options := edit1.Options + [eo_ExtraHintIfFocus]
  else edit1.Options := edit1.Options - [eo_ExtraHintIfFocus];

  editColor.Options := edit1.Options;
  editAl.Options := edit1.Options;
  editFont.Options := edit1.Options;
  editBtn1.EditOptions := edit1.Options;
  editDir1.EditOptions := edit1.Options;
end;

procedure TfrmMain.QuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: frmMain}
  Name := 'frmMain';
  SetPosition(610, 281, 636, 254);
  WindowTitle := 'Edits Extrahint';
  Hint := '';
  IconName := '';

  edit1 := TfpgEdit.Create(self);
  with edit1 do
  begin
    Name := 'edit1';
    SetPosition(8, 128, 236, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 1;
    Text := '';
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(540, 227, 92, 23);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.close';
    TabOrder := 4;
  end;

  editCombo1 := TfpgEditCombo.Create(self);
  with editCombo1 do
  begin
    Name := 'editCombo1';
    SetPosition(8, 224, 236, 24);
  end;

  editBtn1 := TfpgEditButton.Create(self);
  with editBtn1 do
  begin
    Name := 'editBtn1';
    SetPosition(252, 192, 236, 24);
    ExtraHint := '';
    TabOrder := 6;
    Text := '';
  end;

  editDir1 := TfpgDirectoryEdit.Create(self);
  with editDir1 do
  begin
    Name := 'editDir1';
    SetPosition(8, 192, 236, 24);
    Directory := '';
    ExtraHint := '';
    RootDirectory := '';
    TabOrder := 8;
  end;

  combo1 := TfpgComboBox.Create(self);
  with combo1 do
  begin
    Name := 'combo1';
    SetPosition(252, 224, 236, 24);
    ExtraHint := '';
    FontDesc := '#List';
    Hint := '';
    FocusItem := -1;
    TabOrder := 11;
  end;

  labelInfo := TfpgLabel.Create(self);
  with labelInfo do
  begin
    Name := 'labelInfo';
    SetPosition(8, 8, 621, 91);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    Text := 'label info';
    WrapText := True;
  end;

  cbFocused := TfpgCheckBox.Create(self);
  with cbFocused do
  begin
    Name := 'cbFocused';
    SetPosition(8, 104, 164, 19);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 9;
    Text := 'Hints visible if focused';
  end;

  editColor := TCustomColorEdit.Create(self);
  with editColor do
  begin
    Name := 'editColor';
    SetPosition(252, 128, 236, 24);
  end;

  editAl := TCustomAlignmentEdit.Create(self);
  with editAl do
  begin
    Name := 'editAl';
    SetPosition(8, 160, 236, 24);
  end;

  editFont := TCustomFontEdit.Create(self);
  with editFont do
  begin
    Name := 'editFont';
    SetPosition(252, 160, 236, 24);
  end;

  {@VFD_BODY_END: frmMain}
  {%endregion}

  WindowPosition := wpScreenCenter;

  labelInfo.Text :=
    'This example shows how ExtraHints (placeholder texts) works:'+#10+
    '- If you want custom font or color for individual edit, see file customedits.pas in this dir'+#10+
    '- If you want change ExtraHint globally (color, alignment, etc), see TfpgStyle.DrawPlaceholderText'+#10+
    '  method and clPlaceholderText color definition'+#10+
    '- To change ExtraHint visibility when edit is focused click checkbox bellow';

  edit1.ExtraHint := edit1.Name;

  editColor.ExtraHint := editColor.Name;
  editAl.ExtraHint := editAl.Name;
  editFont.ExtraHint := editFont.Name;
  editBtn1.ExtraHint := editBtn1.Name;
  editDir1.ExtraHint := editDir1.Name;
  editCombo1.Items.AddStrings(['Item 1', 'Item 2', 'Item 3']);
  //editCombo.AutoCompletion := True;
  editCombo1.AllowNew := anYes;
  editCombo1.ExtraHint := editCombo1.Name;
  combo1.Items.AddStrings(['Item 1', 'Item 2', 'Item 3']);
  combo1.ExtraHint := combo1.Name;

  btnQuit.OnClick := @QuitClicked;
  cbFocused.OnChange := @ChangeFocusState;
end;


end.
