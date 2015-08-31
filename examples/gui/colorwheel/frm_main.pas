unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_widget,
  fpg_edit, fpg_form, fpg_label, fpg_button,
  fpg_dialogs, fpg_menu, fpg_checkbox, fpg_listbox, fpg_combobox,
  fpg_panel, fpg_ColorWheel, fpg_spinedit;

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


  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    Button1: TfpgButton;
    ColorWheel1: TfpgColorWheel;
    ValueBar1: TfpgValueBar;
    Bevel1: TfpgBevel;
    Label1: TfpgLabel;
    Label2: TfpgLabel;
    Label3: TfpgLabel;
    edH: TfpgEdit;
    edS: TfpgEdit;
    edV: TfpgEdit;
    Label4: TfpgLabel;
    Label5: TfpgLabel;
    Label6: TfpgLabel;
    edR: TfpgSpinEdit;
    edG: TfpgSpinEdit;
    edB: TfpgSpinEdit;
    Label10: TfpgLabel;
    cbColors: TfpgComboBox;
    lbColors: TfpgColorListBox;
    lblHex: TfpgLabel;
    eHex: TfpgEdit;
    Label7: TfpgLabel;
    Label8: TfpgLabel;
    Bevel2: TfpgBevel;
    Label9: TfpgLabel;
    chkCrossHair: TfpgCheckBox;
    chkBGColor: TfpgCheckBox;
    btnPicker: TPickerButton;
    chkContinuous: TfpgCheckBox;
    {@VFD_HEAD_END: MainForm}
    FViaRGB: Boolean; // to prevent recursive changes
    FColorPicking: Boolean;
    procedure   btnColorPicked(Sender: TObject; const AMousePos: TPoint; const AColor: TfpgColor);
    procedure   chkContinuousChanged(Sender: TObject);
    procedure   btnQuitClicked(Sender: TObject);
    procedure   chkCrossHairChange(Sender: TObject);
    procedure   chkBGColorChange(Sender: TObject);
    procedure   UpdateHSVComponents;
    procedure   UpdateRGBComponents;
    procedure   ColorChanged(Sender: TObject);
    procedure   RGBChanged(Sender: TObject);
    procedure   RGBChanging;
    procedure   ConvertToInt(Value: string);
    procedure   eHexKeyChar(Sender: TObject; AChar: TfpgChar; var Consumed: boolean);
    procedure   eHexKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
      var Consumed: boolean);
    procedure   PopulatePaletteColorCombo;
    procedure   cbColorsChange(Sender: TObject);
    procedure   lbColorsChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TMainForm.ConvertToInt(Value: string);
var
  iRed, iGreen, iBlue: integer;
  i, iTemp : integer;
  HexVal: string;
begin
  for i:= 2 to 7 do
  begin
    HexVal:= Copy(Value,i,1);
    case Uppercase(HexVal) of
      'F':
        iTemp:= 15;
      'E':
        iTemp:= 14;
      'D':
        iTemp:= 13;
      'C':
        iTemp:= 12;
      'B':
        iTemp:= 11;
      'A':
        iTemp:= 10
      else
        if (HexVal>= '0') and (HexVal<= '9') then
          iTemp:= StrToInt(HexVal);
    end;
    case i of
      2:
        iRed:= iTemp;
      3:
        iRed:= iRed * 16 +iTemp;
      4:
        iGreen:= iTemp;
      5:
        iGreen:= iGreen * 16 +iTemp;
      6:
        iBlue:= iTemp;
      7:
        iBlue:= iBlue * 16 +iTemp;
    end;
  end;
  edR.Value := iRed;
  edG.Value := iGreen;
  edB.Value := iBlue;
end;

function ConvertToHexa(Value: Integer): string;
var
  ValH,ValL: Integer;
begin
ValH:= Value div 16;
ValL:= Value mod 16;
case ValH of
  15:
    Result:= 'F';
  14:
    Result:= 'E';
  13:
    Result:= 'D';
  12:
    Result:= 'C';
  11:
    Result:= 'B';
  10:
    Result:= 'A';
  else
    Result:= IntToStr(ValH);
  end;
case ValL of
  15:
    Result:= Result+'F';
  14:
    Result:= Result+'E';
  13:
    Result:= Result+'D';
  12:
    Result:= Result+'C';
  11:
    Result:= Result+'B';
  10:
    Result:= Result+'A';
  else
    Result:= Result+IntToStr(ValL);
  end;
end;

function Hexa(Red,Green,Blue: Integer): string;
begin
Result:= '$'+ConvertToHexa(Red)+ConvertToHexa(Green)+ConvertToHexa(Blue);
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

procedure TMainForm.ColorChanged(Sender: TObject);
begin
  UpdateHSVComponents;
  if not FViaRGB then
    UpdateRGBComponents;
end;

procedure TMainForm.RGBChanged(Sender: TObject);
begin
  RGBChanging;
end;

procedure TMainForm.RGBChanging;
var
  rgb: TRGBTriple;
  c: TfpgColor;
begin
  FViaRGB := True;  // revent recursive updates
  rgb.Red := edR.Value;
  rgb.Green := edG.Value;
  rgb.Blue := edB.Value;
  c := RGBTripleTofpgColor(rgb);
  ColorWheel1.SetSelectedColor(c);  // This will trigger ColorWheel and ValueBar OnChange event
  FViaRGB := False;
  eHex.Text:= Hexa(rgb.Red,rgb.Green,rgb.Blue);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FViaRGB := False;
  FColorPicking := False;
end;

procedure TMainForm.btnColorPicked(Sender: TObject; const AMousePos: TPoint; const AColor: TfpgColor);
begin
  ColorWheel1.SetSelectedColor(AColor);
end;

procedure TMainForm.chkContinuousChanged(Sender: TObject);
begin
  btnPicker.ContinuousResults := chkContinuous.Checked;
end;

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.chkCrossHairChange(Sender: TObject);
begin
  if chkCrossHair.Checked then
    ColorWheel1.CursorSize := 400
  else
    ColorWheel1.CursorSize := 5;
end;

procedure TMainForm.chkBGColorChange(Sender: TObject);
begin
  if chkBGColor.Checked then
  begin
    ColorWheel1.BackgroundColor := clBrown;
    ValueBar1.BackgroundColor := clBrown;
  end
  else
  begin
    ColorWheel1.BackgroundColor := clWindowBackground;
    ValueBar1.BackgroundColor := clWindowBackground;
  end;
end;

procedure TMainForm.UpdateHSVComponents;
begin
  edH.Text := IntToStr(ColorWheel1.Hue);
  edS.Text := FormatFloat('##0.0', ColorWheel1.Saturation * 100);
  edV.Text := FormatFloat('##0.0', ValueBar1.Value * 100);
  Bevel1.BackgroundColor := ValueBar1.SelectedColor;
end;

procedure TMainForm.UpdateRGBComponents;
var
  rgb: TRGBTriple;
  c: TfpgColor;
begin
  c := ValueBar1.SelectedColor;
  rgb := fpgColorToRGBTriple(c);
  edR.Value := rgb.Red;
  edG.Value := rgb.Green;
  edB.Value := rgb.Blue;
  eHex.Text:= Hexa(rgb.Red,rgb.Green,rgb.Blue);
end;

procedure TMainForm.eHexKeyChar(Sender: TObject; AChar: TfpgChar; var Consumed: boolean);
begin
if Length(eHex.Text)= 0 then
begin
  if AChar<> '$' then
    Consumed:= True;
end
else
  if ((AChar< '0') or (AChar> '9')) and ((AChar< 'A') or (AChar> 'F')) and ((AChar< 'a') or (AChar> 'f')) then
    Consumed:= True;
end;

procedure TMainForm.eHexKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
          var Consumed: boolean);
begin
  if ((KeyCode= KeyReturn) or (KeyCode= KeyPEnter)) and (Length(eHex.Text)= 7) then
  begin
    ConvertToInt(eHex.Text);
    RGBChanging;
  end;
end;

procedure TMainForm.PopulatePaletteColorCombo;
begin
  with cbColors do
  begin
    Items.Clear;
    Items.Add('cpStandardColors');
    Items.Add('cpSystemColors');
    Items.Add('cpWebColors');
    FocusItem := 0;
    OnChange := @cbColorsChange;
  end;
end;

procedure TMainForm.cbColorsChange(Sender: TObject);
begin
  if cbColors.Text = 'cpStandardColors' then
    lbColors.ColorPalette := cpStandardColors
  else if cbColors.Text = 'cpSystemColors' then
    lbColors.ColorPalette := cpSystemColors
  else
    lbColors.ColorPalette := cpWebColors;
end;

procedure TMainForm.lbColorsChange(Sender: TObject);
var
  rgb: TRGBTriple;
  c: TfpgColor;
begin
  c := lbColors.Color;
  rgb := fpgColorToRGBTriple(c);
  edR.Value := rgb.Red;
  edG.Value := rgb.Green;
  edB.Value := rgb.Blue;
  eHex.Text:= Hexa(rgb.Red,rgb.Green,rgb.Blue);
  ConvertToInt(eHex.Text);
  RGBChanging;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(0, 0, 540, 420);
  WindowTitle := 'ColorWheel test app';
  Hint := '';
  IconName := '';
  WindowPosition := wpScreenCenter;

  Button1 := TfpgButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(447, 376, 80, 26);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 0;
    OnClick  := @btnQuitClicked;
  end;

  ColorWheel1 := TfpgColorWheel.Create(self);
  with ColorWheel1 do
  begin
    Name := 'ColorWheel1';
    SetPosition(12, 20, 272, 244);
  end;

  ValueBar1 := TfpgValueBar.Create(self);
  with ValueBar1 do
  begin
    Name := 'ValueBar1';
    SetPosition(290, 20, 52, 244);
    Value := 1;
    OnChange  := @ColorChanged;
  end;

  Bevel1 := TfpgBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(20, 288, 76, 56);
    Hint := '';
  end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(108, 284, 64, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Hue';
  end;

  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(108, 312, 64, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Saturation';
  end;

  Label3 := TfpgLabel.Create(self);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(108, 340, 64, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Brightness';
  end;

  edH := TfpgEdit.Create(self);
  with edH do
  begin
    Name := 'edH';
    SetPosition(176, 280, 44, 24);
    BackgroundColor := clWindowBackground;
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 8;
    Text := '';
  end;

  edS := TfpgEdit.Create(self);
  with edS do
  begin
    Name := 'edS';
    SetPosition(176, 308, 44, 24);
    BackgroundColor := clWindowBackground;
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 9;
    Text := '';
  end;

  edV := TfpgEdit.Create(self);
  with edV do
  begin
    Name := 'edV';
    SetPosition(176, 336, 44, 24);
    BackgroundColor := clWindowBackground;
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 10;
    Text := '';
  end;

  Label4 := TfpgLabel.Create(self);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(230, 284, 56, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Red';
  end;

  Label5 := TfpgLabel.Create(self);
  with Label5 do
  begin
    Name := 'Label5';
    SetPosition(230, 312, 56, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Green';
  end;

  Label6 := TfpgLabel.Create(self);
  with Label6 do
  begin
    Name := 'Label6';
    SetPosition(230, 340, 56, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Blue';
  end;

  edR := TfpgSpinEdit.Create(self);
  with edR do
  begin
    Name := 'edR';
    SetPosition(290, 280, 44, 24);
    TabOrder := 13;
    MinValue := 0;
    MaxValue := 255;
    Value := 255;
    FontDesc := '#Edit1';
    OnChange := @RGBChanged;
    OnExit  := @RGBChanged;
  end;

  edG := TfpgSpinEdit.Create(self);
  with edG do
  begin
    Name := 'edG';
    SetPosition(290, 308, 44, 24);
    TabOrder := 14;
    MinValue := 0;
    MaxValue := 255;
    Value := 255;
    FontDesc := '#Edit1';
    OnChange := @RGBChanged;
    OnExit := @RGBChanged;
  end;

  edB := TfpgSpinEdit.Create(self);
  with edB do
  begin
    Name := 'edB';
    SetPosition(290, 336, 44, 24);
    TabOrder := 15;
    MinValue := 0;
    MaxValue := 255;
    Value := 255;
    FontDesc := '#Edit1';
    OnChange := @RGBChanged;
    OnExit := @RGBChanged;
  end;

  Label10 := TfpgLabel.Create(self);
  with Label10 do
  begin
    Name := 'Label10';
    SetPosition(352, 100, 180, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Predefined Color Palettes';
  end;

  cbColors := TfpgComboBox.Create(self);
  with cbColors do
  begin
    Name := 'cbColors';
    SetPosition(352, 120, 180, 22);
    ExtraHint := '';
    FontDesc := '#List';
    Hint := '';
    FocusItem := -1;
    TabOrder := 18;
  end;

  lbColors := TfpgColorListBox.Create(self);
  with lbColors do
  begin
    Name := 'lbColors';
    SetPosition(352, 150, 180, 160);
    Color := TfpgColor($FF00FFFF);
    FontDesc := '#List';
    Hint := '';
    TabOrder := 19;
    ScrollbarPage := VisibleItems;
    OnChange := @lbColorsChange;
  end;

  lblHex := TfpgLabel.Create(self);
  with lblHex do
  begin
    Name := 'lblHex';
    SetPosition(375, 340, 120, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Hex = ';
  end;

  eHex := TfpgEdit.Create(self);
  with eHex do
  begin
    Name := 'eHex';
    SetPosition(420, 336, 65, 24);
    ExtraHint := '';
    FontDesc := '#Label2';
    Hint := '';
    TabOrder := 21;
    Text := '';
    MaxLength:= 7;
    OnKeyChar:= @eHexKeyChar;
    OnKeyPress:= @eHexKeyPress;
  end;

  Label7 := TfpgLabel.Create(self);
  with Label7 do
  begin
    Name := 'Label7';
    SetPosition(108, 3, 80, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'ColorWheel';
  end;

  Label8 := TfpgLabel.Create(self);
  with Label8 do
  begin
    Name := 'Label8';
    SetPosition(290, 3, 64, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'ValueBar';
  end;

  Bevel2 := TfpgBevel.Create(self);
  with Bevel2 do
  begin
    Name := 'Bevel2';
    SetPosition(388, 8, 2, 80);
    Hint := '';
    Style := bsLowered;
  end;

  Label9 := TfpgLabel.Create(self);
  with Label9 do
  begin
    Name := 'Label9';
    SetPosition(400, 3, 128, 16);
    Alignment := taCenter;
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Custom Options';
  end;

  chkCrossHair := TfpgCheckBox.Create(self);
  with chkCrossHair do
  begin
    Name := 'chkCrossHair';
    SetPosition(396, 32, 128, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 20;
    Text := 'Large CrossHair';
    OnChange  := @chkCrossHairChange;
  end;

  chkBGColor := TfpgCheckBox.Create(self);
  with chkBGColor do
  begin
    Name := 'chkBGColor';
    SetPosition(396, 56, 132, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 21;
    Text := 'New BG Color';
    OnChange  := @chkBGColorChange;
  end;

  btnPicker := TPickerButton.Create(self);
  with btnPicker do
  begin
    Name := 'btnPicker';
    SetPosition(116, 372, 80, 23);
    Text := 'Picker';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 24;
    OnColorPicked := @btnColorPicked;
  end;

  chkContinuous := TfpgCheckBox.Create(self);
  with chkContinuous do
  begin
    Name := 'chkContinuous';
    SetPosition(205, 375, 90, 19);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 25;
    Text := 'Continuous';
    OnChange := @chkContinuousChanged;
  end;

  {@VFD_BODY_END: MainForm}

  PopulatePaletteColorCombo;
  // link the two components
  ColorWheel1.ValueBar := ValueBar1;
//  ColorWheel1.BackgroundColor := clFuchsia;
//  ValueBar1.BackgroundColor := clFuchsia;
//  ColorWheel1.CursorSize := 400;
  UpdateHSVComponents;
  if not FViaRGB then
    UpdateRGBComponents;
end;


end.
