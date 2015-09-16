unit mainform;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_edit, fpg_button,
  fpg_editcombo, fpg_editbtn, fpg_combobox;

type

  { TfrmMain }

  TfrmMain = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: frmMain}
    editFocusOn: TfpgEdit;
    editFocusOff: TfpgEdit;
    editCustomColor: TfpgEdit;
    btnQuit: TfpgButton;
    editCombo: TfpgEditCombo;
    editBtnFocusOn: TfpgEditButton;
    editBtnFocusOff: TfpgEditButton;
    editDirFocusOn: TfpgDirectoryEdit;
    editDirFocusOff: TfpgDirectoryEdit;
    editComboColor: TfpgEditCombo;
    combo: TfpgComboBox;
    comboColor: TfpgComboBox;
    editCustomFont: TfpgEdit;
    editComboFont: TfpgEditCombo;
    comboFont: TfpgComboBox;
    editAlignCenter: TfpgEdit;
    editAlignRight: TfpgEdit;
    comboAlignCenter: TfpgComboBox;
    comboAlignRight: TfpgComboBox;
    editComboAlignCenter: TfpgEditCombo;
    editComboAlignRight: TfpgEditCombo;
    {@VFD_HEAD_END: frmMain}
    procedure QuitClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TfrmMain.QuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: frmMain}
  Name := 'frmMain';
  SetPosition(610, 281, 552, 673);
  WindowTitle := 'Edits Extrahint';
  Hint := '';
  IconName := '';

  editFocusOn := TfpgEdit.Create(self);
  with editFocusOn do
  begin
    Name := 'editFocusOn';
    SetPosition(4, 4, 544, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    ExtraHintFontDesc := '#Edit1';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 1;
    Text := '';
  end;

  editFocusOff := TfpgEdit.Create(self);
  with editFocusOff do
  begin
    Name := 'editFocusOff';
    SetPosition(4, 36, 544, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    ExtraHintFontDesc := '#Edit1';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 2;
    Text := '';
  end;

  editCustomColor := TfpgEdit.Create(self);
  with editCustomColor do
  begin
    Name := 'editCustomColor';
    SetPosition(4, 68, 544, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    ExtraHintFontDesc := '#Edit1';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 3;
    Text := '';
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(4, 643, 544, 23);
    Anchors := [anLeft,anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.close';
    TabOrder := 4;
  end;

  editCombo := TfpgEditCombo.Create(self);
  with editCombo do
  begin
    Name := 'editCombo';
    SetPosition(4, 324, 544, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  editBtnFocusOn := TfpgEditButton.Create(self);
  with editBtnFocusOn do
  begin
    Name := 'editBtnFocusOn';
    SetPosition(4, 196, 544, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    ExtraHintFocused := False;
    ExtraHintColor := TfpgColor($20000000);
    ExtraHintFontDesc := '#Edit1';
    TabOrder := 6;
    Text := '';
  end;

  editBtnFocusOff := TfpgEditButton.Create(self);
  with editBtnFocusOff do
  begin
    Name := 'editBtnFocusOff';
    SetPosition(4, 228, 544, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    ExtraHintFocused := False;
    ExtraHintColor := TfpgColor($20000000);
    ExtraHintFontDesc := '#Edit1';
    TabOrder := 7;
    Text := '';
  end;

  editDirFocusOn := TfpgDirectoryEdit.Create(self);
  with editDirFocusOn do
  begin
    Name := 'editDirFocusOn';
    SetPosition(4, 260, 544, 24);
    Anchors := [anLeft,anRight,anTop];
    Directory := '';
    ExtraHint := '';
    ExtraHintFocused := False;
    ExtraHintColor := TfpgColor($20000000);
    ExtraHintFontDesc := '#Edit1';
    RootDirectory := '';
    TabOrder := 8;
  end;

  editDirFocusOff := TfpgDirectoryEdit.Create(self);
  with editDirFocusOff do
  begin
    Name := 'editDirFocusOff';
    SetPosition(4, 292, 544, 24);
    Anchors := [anLeft,anRight,anTop];
    Directory := '';
    ExtraHint := '';
    ExtraHintFocused := False;
    ExtraHintColor := TfpgColor($20000000);
    ExtraHintFontDesc := '#Edit1';
    RootDirectory := '';
    TabOrder := 9;
  end;

  editComboColor := TfpgEditCombo.Create(self);
  with editComboColor do
  begin
    Name := 'editComboColor';
    SetPosition(4, 356, 544, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  combo := TfpgComboBox.Create(self);
  with combo do
  begin
    Name := 'combo';
    SetPosition(4, 484, 544, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    ExtraHintColor := TfpgColor($20000000);
    ExtraHintFontDesc := '#List';
    FontDesc := '#List';
    Hint := '';
    FocusItem := -1;
    TabOrder := 11;
  end;

  comboColor := TfpgComboBox.Create(self);
  with comboColor do
  begin
    Name := 'comboColor';
    SetPosition(4, 516, 544, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    ExtraHintColor := TfpgColor($20000000);
    ExtraHintFontDesc := '#List';
    FontDesc := '#List';
    Hint := '';
    FocusItem := -1;
    TabOrder := 12;
  end;

  editCustomFont := TfpgEdit.Create(self);
  with editCustomFont do
  begin
    Name := 'editCustomFont';
    SetPosition(4, 100, 544, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    ExtraHintFontDesc := '#Edit1';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 13;
    Text := '';
  end;

  editComboFont := TfpgEditCombo.Create(self);
  with editComboFont do
  begin
    Name := 'editComboFont';
    SetPosition(4, 388, 544, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  comboFont := TfpgComboBox.Create(self);
  with comboFont do
  begin
    Name := 'comboFont';
    SetPosition(4, 548, 544, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    ExtraHintColor := TfpgColor($20000000);
    ExtraHintFontDesc := '#List';
    FontDesc := '#List';
    Hint := '';
    FocusItem := -1;
    TabOrder := 15;
  end;

  editAlignCenter := TfpgEdit.Create(self);
  with editAlignCenter do
  begin
    Name := 'editAlignCenter';
    SetPosition(4, 132, 544, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    ExtraHintFontDesc := '#Edit1';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 16;
    Text := '';
  end;

  editAlignRight := TfpgEdit.Create(self);
  with editAlignRight do
  begin
    Name := 'editAlignRight';
    SetPosition(4, 164, 544, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    ExtraHintFontDesc := '#Edit1';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 17;
    Text := '';
  end;

  comboAlignCenter := TfpgComboBox.Create(self);
  with comboAlignCenter do
  begin
    Name := 'comboAlignCenter';
    SetPosition(4, 580, 544, 24);
    ExtraHint := '';
    ExtraHintColor := TfpgColor($20000000);
    ExtraHintFontDesc := '#List';
    FontDesc := '#List';
    Hint := '';
    FocusItem := -1;
    TabOrder := 18;
  end;

  comboAlignRight := TfpgComboBox.Create(self);
  with comboAlignRight do
  begin
    Name := 'comboAlignRight';
    SetPosition(4, 612, 544, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    ExtraHintColor := TfpgColor($20000000);
    ExtraHintFontDesc := '#List';
    FontDesc := '#List';
    Hint := '';
    FocusItem := -1;
    TabOrder := 19;
  end;

  editComboAlignCenter := TfpgEditCombo.Create(self);
  with editComboAlignCenter do
  begin
    Name := 'editComboAlignCenter';
    SetPosition(4, 420, 544, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  editComboAlignRight := TfpgEditCombo.Create(self);
  with editComboAlignRight do
  begin
    Name := 'editComboAlignRight';
    SetPosition(4, 452, 544, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  {@VFD_BODY_END: frmMain}
  {%endregion}

  WindowPosition := wpScreenCenter;

  editFocusOn.ExtraHint := 'TfpgEdit ExtraHint ON';
  editFocusOn.ExtraHintFocused := True;

  editFocusOff.ExtraHint := 'TfpgEdit ExtraHint OFF';
  editFocusOff.ExtraHintFocused := False;

  editCustomColor.ExtraHint := 'TfpgEdit Custom Color';
  editCustomColor.ExtraHintColor := clGreen;

  editCustomFont.ExtraHint := 'TfpgEdit Custom Font';
  editCustomFont.ExtraHintFontDesc := 'DejaVu Sans Mono-10:bold:italic:antialias=true:underline';

  editAlignCenter.ExtraHint := 'TfpgEdit Center';
  editAlignCenter.ExtraHintAlignment := taCenter;

  editAlignRight.ExtraHint := 'TfpgEdit Right';
  editAlignRight.ExtraHintAlignment := taRightJustify;

  editBtnFocusOn.ExtraHint := 'TfpgEditButton ExtraHint ON';
  editBtnFocusOn.ExtraHintFocused := True;

  editBtnFocusOff.ExtraHint := 'TfpgEditButton ExtraHint OFF';
  editBtnFocusOff.ExtraHintFocused := False;

  editDirFocusOn.ExtraHint := 'TfpgDirectoryEdit ExtraHint ON';
  editDirFocusOn.ExtraHintFocused := True;

  editDirFocusOff.ExtraHint := 'TfpgDirectoryEdit ExtraHint OFF';
  editDirFocusOff.ExtraHintFocused := False;

  editCombo.Items.AddStrings(['Item 1', 'Item 2', 'Item 3']);
  //editCombo.AutoCompletion := True;
  editCombo.AllowNew := anYes;
  editCombo.ExtraHint := 'TfpgEditCombo Default';

  editComboColor.Items.AddStrings(['Item 1', 'Item 2', 'Item 3']);
  //editCombo.AutoCompletion := True;
  editComboColor.AllowNew := anYes;
  editComboColor.ExtraHint := 'TfpgEditCombo Custom Color';
  editComboColor.ExtraHintColor := clGreen;

  editComboFont.Items.AddStrings(['Item 1', 'Item 2', 'Item 3']);
  //editCombo.AutoCompletion := True;
  editComboFont.AllowNew := anYes;
  editComboFont.ExtraHint := 'TfpgEditCombo Custom Font';
  editComboFont.ExtraHintFontDesc := 'DejaVu Sans Mono-10:bold:italic:antialias=true:underline';

  editComboAlignCenter.Items.AddStrings(['Item 1', 'Item 2', 'Item 3']);
  //editCombo.AutoCompletion := True;
  editComboAlignCenter.AllowNew := anYes;
  editComboAlignCenter.ExtraHint := 'TfpgEditCombo Center';
  editComboAlignCenter.ExtraHintAlignment := taCenter;

  editComboAlignRight.Items.AddStrings(['Item 1', 'Item 2', 'Item 3']);
  //editCombo.AutoCompletion := True;
  editComboAlignRight.AllowNew := anYes;
  editComboAlignRight.ExtraHint := 'TfpgEditCombo Right';
  editComboAlignRight.ExtraHintAlignment := taRightJustify;

  combo.Items.AddStrings(['Item 1', 'Item 2', 'Item 3']);
  combo.ExtraHint := 'TfpgComboBox Default';

  comboColor.Items.AddStrings(['Item 1', 'Item 2', 'Item 3']);
  comboColor.ExtraHint := 'TfpgComboBox Custom Color';
  comboColor.ExtraHintColor := clGreen;

  comboFont.Items.AddStrings(['Item 1', 'Item 2', 'Item 3']);
  comboFont.ExtraHint := 'TfpgComboBox Custom Font';
  comboFont.ExtraHintFontDesc := 'DejaVu Sans Mono-10:bold:italic:antialias=true:underline';

  comboAlignCenter.Items.AddStrings(['Item 1', 'Item 2', 'Item 3']);
  comboAlignCenter.ExtraHint := 'TfpgComboBox Center';
  comboAlignCenter.ExtraHintAlignment := taCenter;

  comboAlignRight.Items.AddStrings(['Item 1', 'Item 2', 'Item 3']);
  comboAlignRight.ExtraHint := 'TfpgComboBox Right';
  comboAlignRight.ExtraHintAlignment := taRightJustify;

  btnQuit.OnClick := @QuitClicked;
end;


end.
