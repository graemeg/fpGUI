{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2015 See the file AUTHORS.txt, included in this
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

  TColorPickedEvent = procedure(Sender: TObject; const AMousePos: TPoint; const AColor: TfpgColor) of object;

  TPickerButton = class(TfpgButton)
  private
    FContinuousResults: Boolean;
    FOnColorPicked: TColorPickedEvent;
    FColorPos: TPoint;
    FColor: TfpgColor;
    FColorPicking: Boolean;
  private
    procedure   DoColorPicked;
  protected
    procedure   HandleLMouseDown(X, Y: integer; ShiftState: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    ContinuousResults: Boolean read FContinuousResults write FContinuousResults;
    property    OnColorPicked: TColorPickedEvent read FOnColorPicked write FOnColorPicked;
  end;

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
    lblRed: TfpgLabel;
    lblGreen: TfpgLabel;
    lblBlue: TfpgLabel;
    btnPicker: TPickerButton;
    chkContinuous: TfpgCheckBox;
    lblHex: TfpgLabel;
    edHex: TfpgEdit;
    {@VFD_HEAD_END: ColorSelectDialog}
    FViaRGB: Boolean;  // to prevent recursive changes
    FColorPicking: Boolean;
    procedure   btnColorPicked(Sender: TObject; const AMousePos: TPoint; const AColor: TfpgColor);
    procedure   chkContinuousChanged(Sender: TObject);
    function    GetSelectedColor: TfpgColor;
    procedure   SetSelectedColor(const AValue: TfpgColor);
    procedure   ColorChanged(Sender: TObject);
    procedure   NamedColorChanged(Sender: TObject);
    procedure   RGBChanged(Sender: TObject);
    procedure   UpdateRGBComponents;
    procedure   PopulatePaletteColorCombo;
    procedure   cbColorPaletteChange(Sender: TObject);
    procedure   OnTabChange(Sender: TObject; tab:TfpgTabSheet);
  protected
    procedure   SetupCaptions; override;
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


function ConvertToHex(Value: integer): string;
var
  ValH, ValL: integer;
begin
  ValH := Value div 16;
  ValL := Value mod 16;
  case ValH of
    15:
      Result := 'F';
    14:
      Result := 'E';
    13:
      Result := 'D';
    12:
      Result := 'C';
    11:
      Result := 'B';
    10:
      Result := 'A';
    else
      Result := IntToStr(ValH);
  end;
  case ValL of
    15:
      Result := Result + 'F';
    14:
      Result := Result + 'E';
    13:
      Result := Result + 'D';
    12:
      Result := Result + 'C';
    11:
      Result := Result + 'B';
    10:
      Result := Result + 'A';
    else
      Result := Result + IntToStr(ValL);
  end;
end;

function Hex(Red, Green, Blue: integer): string;
begin
  Result := '$' + ConvertToHex(Red) + ConvertToHex(Green) + ConvertToHex(Blue);
end;

{ TPickerButton }

procedure TPickerButton.DoColorPicked;
var
  pt: TPoint;
begin
  pt := WindowToScreen(self, FColorPos);
  FColor := fpgApplication.GetScreenPixelColor(pt);
  if Assigned(FOnColorPicked) then
    FOnColorPicked(self, FColorPos, FColor);
end;

procedure TPickerButton.HandleLMouseDown(X, Y: integer; ShiftState: TShiftState);
begin
  inherited HandleLMouseDown(X, Y, ShiftState);
  MouseCursor := mcCross;
  FColorPicking := True;
  CaptureMouse;
end;

procedure TPickerButton.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  ReleaseMouse;
  FColorPicking := False;
  MouseCursor := mcDefault;
  DoColorPicked;
end;

procedure TPickerButton.HandleMouseMove(x, y: integer; btnstate: word;
  shiftstate: TShiftState);
begin
  //inherited HandleMouseMove(x, y, btnstate, shiftstate);
  if not FColorPicking then
    Exit;
  FColorPos.x := x;
  FColorPos.y := y;
  if FContinuousResults then
    DoColorPicked;
end;

constructor TPickerButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorPicking := False;
  FContinuousResults := False;
end;

{ TfpgColorSelectDialog }

procedure TfpgColorSelectDialog.OnTabChange(Sender: TObject; tab:TfpgTabSheet);
begin
  if pcColorSelect.ActivePageIndex = 0 then
    RGBChanged(sender)
  else
   NamedColorChanged(sender) ;
end;

procedure TfpgColorSelectDialog.btnColorPicked(Sender: TObject; const AMousePos: TPoint; const AColor: TfpgColor);
begin
  ColorWheel.SetSelectedColor(AColor);
end;

procedure TfpgColorSelectDialog.chkContinuousChanged(Sender: TObject);
begin
  btnPicker.ContinuousResults := chkContinuous.Checked;
end;

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
end;

procedure   TfpgColorSelectDialog.NamedColorChanged(Sender: TObject);
var
  tred, tgreen, tblue: Byte;
begin
  tred := fpgGetRed(ColorListBox1.Color);
  tgreen := fpgGetGreen(ColorListBox1.Color);
  tblue := fpgGetBlue(ColorListBox1.Color);

  // keep text readable based on background color
  if (tred + tgreen + tblue) / (256*3) >0.60 then
    edHex.TextColor := clBlack
  else
    edHex.TextColor := clWhite ;

  edHex.BackgroundColor:=ColorListBox1.Color;
  edHex.Text := Hex(tred,tgreen,tblue);
end;

procedure TfpgColorSelectDialog.RGBChanged(Sender: TObject);
var
  rgb: fpg_base.TRGBTriple;
  c: TfpgColor;
begin
  FViaRGB     := True;  // prevent recursive updates
  rgb.Red     := edR.Value;
  rgb.Green   := edG.Value;
  rgb.Blue    := edB.Value;
  c := RGBTripleTofpgColor(rgb);
  ColorWheel.SetSelectedColor(c);  // This will trigger ColorWheel and ValueBar OnChange event
  FViaRGB     := False;
  // keep text readable based on background color
  if ValueBar.Value > 0.75 then
    edHex.TextColor := clBlack
  else
    edHex.TextColor := clWhite;
  edHex.BackgroundColor := c;
  edHex.Text := Hex(rgb.Red, rgb.Green, rgb.Blue);
end;

procedure TfpgColorSelectDialog.UpdateRGBComponents;
var
  rgb: fpg_base.TRGBTriple;
  c: TfpgColor;
begin
  c := ValueBar.SelectedColor;
  rgb := fpgColorToRGBTriple(c);
  edR.Value := rgb.Red;
  edG.Value := rgb.Green;
  edB.Value := rgb.Blue;
  // keep text readable based on background color
  if ValueBar.Value > 0.75 then
    edHex.TextColor := clBlack
  else
    edHex.TextColor := clWhite;
  edHex.BackgroundColor := c;
  edHex.Text := Hex(rgb.Red, rgb.Green, rgb.Blue);
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

procedure TfpgColorSelectDialog.SetupCaptions;
begin
  inherited SetupCaptions;
  tsColorWheel.Text := rsTabsheetColorWheel;
  tsColorNames.Text := rsTabPredefined;
  lblRed.Text := rsColorRed;
  lblGreen.Text := rsColorGreen;
  lblBlue.Text := rsColorBlue;
  chkContinuous.Text := rsContinuous;
  btnPicker.Hint := rsColorPickerHint;
  lblHex.Text := rsHexadecimal;
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
  SetPosition(340, 164, 328, 385);
  WindowTitle := 'Color Select Dialog';
  Hint := '';
  IconName := '';
  WindowPosition := wpOneThirdDown;

  pcColorSelect := TfpgPageControl.Create(self);
  with pcColorSelect do
  begin
    Name := 'pcColorSelect';
    SetPosition(4, 4, 320, 332);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    TabOrder := 1;
    OnChange := @OnTabChange;
  end;

  tsColorWheel := TfpgTabSheet.Create(pcColorSelect);
  with tsColorWheel do
  begin
    Name := 'tsColorWheel';
    SetPosition(3, 24, 314, 305);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := 'Color Wheel';
  end;

  tsColorNames := TfpgTabSheet.Create(pcColorSelect);
  with tsColorNames do
  begin
    Name := 'tsColorNames';
    SetPosition(3, 24, 314, 305);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := rsTabPredefined;
  end;

  cbColorPalette := TfpgComboBox.Create(tsColorNames);
  with cbColorPalette do
  begin
    Name := 'cbColorPalette';
    SetPosition(8, 24, 299, 22);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FontDesc := '#List';
    Hint := '';
    FocusItem := -1;
    TabOrder := 1;
    OnChange:= @NamedColorChanged;
  end;

  ColorListBox1 := TfpgColorListBox.Create(tsColorNames);
  with ColorListBox1 do
  begin
    Name := 'ColorListBox1';
    SetPosition(8, 72, 299, 224);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Color := TfpgColor($FF00FFFF);
    FontDesc := '#List';
    Hint := '';
    TabOrder := 2;
    OnChange:= @NamedColorChanged;
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

  lblRed := TfpgLabel.Create(tsColorWheel);
  with lblRed do
  begin
    Name := 'lblRed';
    SetPosition(8, 220, 80, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Red';
  end;

  lblGreen := TfpgLabel.Create(tsColorWheel);
  with lblGreen do
  begin
    Name := 'lblGreen';
    SetPosition(8, 248, 80, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Green';
  end;

  lblBlue := TfpgLabel.Create(tsColorWheel);
  with lblBlue do
  begin
    Name := 'lblBlue';
    SetPosition(8, 276, 80, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Blue';
  end;

  btnPicker := TPickerButton.Create(tsColorWheel);
  with btnPicker do
  begin
    Name := 'btnPicker';
    SetPosition(167, 230, 23, 23);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := -1;
    ImageName := 'stdimg.colpicker';
    FShowHint := True;
    TabOrder := 24;
    OnColorPicked := @btnColorPicked;
  end;

  chkContinuous := TfpgCheckBox.Create(tsColorWheel);
  with chkContinuous do
  begin
    Name := 'chkContinuous';
    SetPosition(167, 258, 130, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 25;
    Text := 'Continuous';
    OnChange := @chkContinuousChanged;
  end;

  lblHex := TfpgLabel.Create(self);
  with lblHex do
  begin
    Name := 'lblHex';
    SetPosition(25, 340, 100, 15);
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Hexadecimal';
  end;

  edHex := TfpgEdit.Create(self);
  with edHex do
  begin
    Name := 'edHex';
    SetPosition(25, 356, 100, 23);
    ExtraHint := '';
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 3;
    Text := '';
    MaxLength:= 7;
  end;

  {@VFD_BODY_END: ColorSelectDialog}
  {%endregion}

   FColorPicking := False;
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

