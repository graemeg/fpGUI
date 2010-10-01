{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit contains the Color Selection dialog.
}


{%mainunit fpg_dialogs.pas}

{$IFDEF read_interface}

type

  TfpgColorSelectDialog = class(TfpgBaseDialog)
  private
    {@VFD_HEAD_BEGIN: ColorSelectDialog}
    pcColorSelect: TfpgPageControl;
    tsColorWheel: TfpgTabSheet;
    tsColorNames: TfpgTabSheet;
    cbColorPalette: TfpgComboBox;
    ColorListBox1: TfpgColorListBox;
    Label1: TfpgLabel;
    Label2: TfpgLabel;
    ColorWheel: TfpgColorWheel;
    ValueBar: TfpgValueBar;
    edR: TfpgSpinEdit;
    edG: TfpgSpinEdit;
    edB: TfpgSpinEdit;
    Label3: TfpgLabel;
    Label4: TfpgLabel;
    Label5: TfpgLabel;
    pnlColorPreview: TfpgBevel;
    {@VFD_HEAD_END: ColorSelectDialog}
    FViaRGB: Boolean;  // to prevent recursive changes
    function    GetSelectedColor: TfpgColor;
    procedure   SetSelectedColor(const AValue: TfpgColor);
    procedure   ColorChanged(Sender: TObject);
    procedure   RGBChanged(Sender: TObject);
    procedure   UpdateRGBComponents;
    procedure   PopulatePaletteColorCombo;
    procedure   cbColorPaletteChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
    property    SelectedColor: TfpgColor read GetSelectedColor write SetSelectedColor;
  end;


{$ENDIF read_interface}



{$IFDEF read_implementation}


function fpgSelectColorDialog(APresetColor: TfpgColor): TfpgColor;
var
  frm: TfpgColorSelectDialog;
begin
  Result := APresetColor;
  frm := TfpgColorSelectDialog.Create(nil);
  try
    frm.ColorWheel.SetSelectedColor(APresetColor);
    if frm.ShowModal = mrOK then
      Result := frm.SelectedColor;
  finally
    frm.Free;
  end;
end;

{ TfpgColorSelectDialog }

function TfpgColorSelectDialog.GetSelectedColor: TfpgColor;
begin
  if pcColorSelect.ActivePageIndex = 0 then
    Result := ValueBar.SelectedColor
  else
    Result := ColorListBox1.Color;
end;

procedure TfpgColorSelectDialog.SetSelectedColor(const AValue: TfpgColor);
begin
  ColorWheel.SetSelectedColor(AValue);
end;

procedure TfpgColorSelectDialog.ColorChanged(Sender: TObject);
begin
//  UpdateHSVComponents;
  if not FViaRGB then
    UpdateRGBComponents;
  pnlColorPreview.BackgroundColor := ValueBar.SelectedColor;
end;

procedure TfpgColorSelectDialog.RGBChanged(Sender: TObject);
var
  rgb: TFPColor;
  c: TfpgColor;
begin
  FViaRGB     := True;  // prevent recursive updates
  rgb.Red     := edR.Value;
  rgb.Green   := edG.Value;
  rgb.Blue    := edB.Value;
  c := FPColorTofpgColor(rgb);
  ColorWheel.SetSelectedColor(c);  // This will trigger ColorWheel and ValueBar OnChange event
  FViaRGB     := False;
end;

procedure TfpgColorSelectDialog.UpdateRGBComponents;
var
  rgb: TFPColor;
  c: TfpgColor;
begin
  c := ValueBar.SelectedColor;
  rgb := fpgColorToFPColor(c);
  edR.Value := rgb.Red;
  edG.Value := rgb.Green;
  edB.Value := rgb.Blue;
end;

procedure TfpgColorSelectDialog.PopulatePaletteColorCombo;
begin
  cbColorPalette.Items.Clear;
  cbColorPalette.Items.Add('cpStandardColors');
  cbColorPalette.Items.Add('cpSystemColors');
  cbColorPalette.Items.Add('cpWebColors');
  cbColorPalette.FocusItem := 0;
  cbColorPalette.OnChange := @cbColorPaletteChange;
end;

procedure TfpgColorSelectDialog.cbColorPaletteChange(Sender: TObject);
begin
  if cbColorPalette.Text = 'cpStandardColors' then
    ColorListBox1.ColorPalette := cpStandardColors
  else if cbColorPalette.Text = 'cpSystemColors' then
    ColorListBox1.ColorPalette := cpSystemColors
  else
    ColorListBox1.ColorPalette := cpWebColors;
  ColorListBox1.SetFocus;
end;

constructor TfpgColorSelectDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FViaRGB := false;
end;


procedure TfpgColorSelectDialog.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: ColorSelectDialog}
  Name := 'ColorSelectDialog';
  SetPosition(340, 164, 328, 375);
  WindowTitle := 'Color Select Dialog';
  Hint := '';
  WindowPosition := wpOneThirdDown;

  pcColorSelect := TfpgPageControl.Create(self);
  with pcColorSelect do
  begin
    Name := 'pcColorSelect';
    SetPosition(4, 4, 320, 332);
    Anchors := [anLeft,anRight,anTop,anBottom];
    ActivePageIndex := 0;
    Hint := '';
    TabOrder := 1;
  end;

  tsColorWheel := TfpgTabSheet.Create(pcColorSelect);
  with tsColorWheel do
  begin
    Name := 'tsColorWheel';
    SetPosition(3, 24, 314, 305);
    Text := 'Color Wheel';
  end;

  tsColorNames := TfpgTabSheet.Create(pcColorSelect);
  with tsColorNames do
  begin
    Name := 'tsColorNames';
    SetPosition(3, 24, 314, 305);
    Text := 'Predefined';
  end;

  cbColorPalette := TfpgComboBox.Create(tsColorNames);
  with cbColorPalette do
  begin
    Name := 'cbColorPalette';
    SetPosition(8, 24, 299, 22);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#List';
    Hint := '';
    TabOrder := 1;
  end;

  ColorListBox1 := TfpgColorListBox.Create(tsColorNames);
  with ColorListBox1 do
  begin
    Name := 'ColorListBox1';
    SetPosition(8, 72, 299, 224);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Color := TfpgColor($00FFFF);
    FontDesc := '#List';
    Hint := '';
    TabOrder := 2;
  end;

  Label1 := TfpgLabel.Create(tsColorNames);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(8, 6, 328, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Select a color palette';
  end;

  Label2 := TfpgLabel.Create(tsColorNames);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(8, 54, 328, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Available colors:';
  end;

  ColorWheel := TfpgColorWheel.Create(tsColorWheel);
  with ColorWheel do
  begin
    Name := 'ColorWheel';
    SetPosition(8, 8, 204, 204);
  end;

  ValueBar := TfpgValueBar.Create(tsColorWheel);
  with ValueBar do
  begin
    Name := 'ValueBar';
    SetPosition(240, 8, 64, 204);
    Value := 1;
    OnChange := @ColorChanged;
  end;

  edR := TfpgSpinEdit.Create(tsColorWheel);
  with edR do
  begin
    Name := 'edR';
    SetPosition(92, 216, 52, 24);
    MaxValue := 255;
    MinValue := 0;
    OnChange := @RGBChanged;
  end;

  edG := TfpgSpinEdit.Create(tsColorWheel);
  with edG do
  begin
    Name := 'edG';
    SetPosition(92, 244, 52, 24);
    MaxValue := 255;
    MinValue := 0;
    OnChange := @RGBChanged;
  end;

  edB := TfpgSpinEdit.Create(tsColorWheel);
  with edB do
  begin
    Name := 'edB';
    SetPosition(92, 272, 52, 24);
    MaxValue := 255;
    MinValue := 0;
    OnChange := @RGBChanged;
  end;

  Label3 := TfpgLabel.Create(tsColorWheel);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(8, 220, 80, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Red';
  end;

  Label4 := TfpgLabel.Create(tsColorWheel);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(8, 248, 80, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Green';
  end;

  Label5 := TfpgLabel.Create(tsColorWheel);
  with Label5 do
  begin
    Name := 'Label5';
    SetPosition(8, 276, 80, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Blue';
  end;

  pnlColorPreview := TfpgBevel.Create(tsColorWheel);
  with pnlColorPreview do
  begin
    Name := 'pnlColorPreview';
    SetPosition(248, 232, 52, 52);
    Hint := '';
  end;

  {@VFD_BODY_END: ColorSelectDialog}
  {%endregion}

  // link colorwheel and valuebar
  ColorWheel.ValueBar := ValueBar;

  // position standard dialog buttons
  btnCancel.Left  := Width - FDefaultButtonWidth - FSpacing;
  btnCancel.Top   := Height - btnCancel.Height - FSpacing;
  btnOK.Left      := btnCancel.Left - FDefaultButtonWidth - 6;
  btnOK.Top       := btnCancel.Top;
  
  PopulatePaletteColorCombo;
end;


{$ENDIF read_implementation}

