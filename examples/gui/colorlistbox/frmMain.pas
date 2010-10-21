unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main,
  fpg_widget, fpg_form, fpg_label, fpg_button,
  fpg_listbox, fpg_combobox, fpg_dialogs, fpg_checkbox;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    cbName1: TfpgComboBox;
    lblName4: TfpgLabel;
    btnName1: TfpgButton;
    lbColorPick: TfpgColorListBox;
    btnName2: TfpgButton;
    btnName3: TfpgButton;
    btnName4: TfpgButton;
    lblName1: TfpgLabel;
    chkColorName: TfpgCheckBox;
    chkReorder: TfpgCheckBox;
    {@VFD_HEAD_END: MainForm}
    procedure chkColorNameChange(Sender: TObject);
    procedure ckdReorderChanged(Sender: TObject);
    procedure cbName1Change(Sender: TObject);
    procedure btnName1Clicked(Sender: TObject);
    procedure SetBGColor(Sender: TObject);
    procedure PopulatePaletteColorCombo;
  public
    procedure AfterCreate; override;
  end;
  
  

{@VFD_NEWFORM_DECL}

implementation

uses
  TypInfo;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.chkColorNameChange(Sender: TObject);
begin
  lbColorPick.ShowColorNames := chkColorName.Checked;
end;

procedure TMainForm.ckdReorderChanged(Sender: TObject);
begin
  lbColorPick.DragToReorder := not lbColorPick.DragToReorder;
end;

procedure TMainForm.cbName1Change(Sender: TObject);
begin
  if cbName1.Text = 'cpStandardColors' then
    lbColorPick.ColorPalette := cpStandardColors
  else if cbName1.Text = 'cpSystemColors' then
    lbColorPick.ColorPalette := cpSystemColors
  else
    lbColorPick.ColorPalette := cpWebColors;
end;

procedure TMainForm.btnName1Clicked (Sender: TObject );
begin
  BackgroundColor := lbColorPick.Color;
end;

procedure TMainForm.SetBGColor (Sender: TObject );
begin
  lbColorPick.Color := TfpgButton(Sender).BackgroundColor;
  lbColorPick.SetFocus;
end;

procedure TMainForm.PopulatePaletteColorCombo;
begin
  cbName1.Items.Clear;
  cbName1.Items.Add('cpStandardColors');
  cbName1.Items.Add('cpSystemColors');
  cbName1.Items.Add('cpWebColors');
  cbName1.FocusItem := 0;
  cbName1.OnChange := @cbName1Change;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(286, 264, 413, 250);
  WindowTitle := 'Color ListBox Test';
  Hint := '';
  WindowPosition := wpScreenCenter;

  cbName1 := TfpgComboBox.Create(self);
  with cbName1 do
  begin
    Name := 'cbName1';
    SetPosition(12, 36, 172, 22);
    FontDesc := '#List';
    Hint := '';
    TabOrder := 0;
  end;

  lblName4 := TfpgLabel.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(12, 16, 168, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Predefined Color Palettes';
  end;

  btnName1 := TfpgButton.Create(self);
  with btnName1 do
  begin
    Name := 'btnName1';
    SetPosition(12, 192, 171, 24);
    Text := 'Set Form.Background';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 7;
    OnClick := @btnName1Clicked;
  end;

  lbColorPick := TfpgColorListBox.Create(self);
  with lbColorPick do
  begin
    Name := 'lbColorPick';
    SetPosition(216, 20, 184, 212);
    Anchors := [anLeft,anTop,anBottom];
    FontDesc := '#List';
    Hint := '';
    TabOrder := 4;
  end;

  btnName2 := TfpgButton.Create(self);
  with btnName2 do
  begin
    Name := 'btnName2';
    SetPosition(12, 96, 24, 24);
    Text := '';
    Embedded := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 9;
    BackgroundColor := clBlue;
    OnClick := @SetBGColor;
  end;

  btnName3 := TfpgButton.Create(self);
  with btnName3 do
  begin
    Name := 'btnName3';
    SetPosition(44, 96, 24, 24);
    Text := '';
    Embedded := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 10;
    BackgroundColor := clPurple;
    OnClick := @SetBGColor;
  end;

  btnName4 := TfpgButton.Create(self);
  with btnName4 do
  begin
    Name := 'btnName4';
    SetPosition(76, 96, 24, 24);
    Text := '';
    Embedded := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 11;
    BackgroundColor := clSteelBlue;
    OnClick := @SetBGColor;
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(12, 76, 164, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Set FocusItem Color';
  end;

  chkColorName := TfpgCheckBox.Create(self);
  with chkColorName do
  begin
    Name := 'chkColorName';
    SetPosition(12, 140, 180, 20);
    Checked := True;
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 8;
    Text := 'Show Color Names';
    OnChange := @chkColorNameChange;
  end;

  chkReorder := TfpgCheckBox.Create(self);
  with chkReorder do
  begin
    Name := 'chkReorder';
    SetPosition(12, 160, 180, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 9;
    Text := 'Drag to reorder';
    OnChange := @ckdReorderChanged;
  end;

  {@VFD_BODY_END: MainForm}
  PopulatePaletteColorCombo;

end;



end.
