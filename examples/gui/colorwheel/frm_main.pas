unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_widget,
  fpg_edit, fpg_form, fpg_label, fpg_button,
  fpg_dialogs, fpg_menu, fpg_checkbox,
  fpg_panel, fpg_ColorWheel;

type

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
    edR: TfpgEdit;
    edG: TfpgEdit;
    edB: TfpgEdit;
    Label7: TfpgLabel;
    Label8: TfpgLabel;
    Bevel2: TfpgBevel;
    Label9: TfpgLabel;
    chkCrossHair: TfpgCheckBox;
    chkBGColor: TfpgCheckBox;
    {@VFD_HEAD_END: MainForm}
    FViaRGB: Boolean; // to prevent recursive changes
    procedure btnQuitClicked(Sender: TObject);
    procedure chkCrossHairChange(Sender: TObject);
    procedure chkBGColorChange(Sender: TObject);
    procedure UpdateHSVComponents;
    procedure UpdateRGBComponents;
    procedure ColorChanged(Sender: TObject);
    procedure RGBChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation


{@VFD_NEWFORM_IMPL}

procedure TMainForm.ColorChanged(Sender: TObject);
begin
  UpdateHSVComponents;
  if not FViaRGB then
    UpdateRGBComponents;
end;

procedure TMainForm.RGBChanged(Sender: TObject);
var
  rgb: TFPColor;
  c: TfpgColor;
begin
  FViaRGB := True;  // revent recursive updates
  rgb.Red := StrToInt(edR.Text);
  rgb.Green := StrToInt(edG.Text);
  rgb.Blue := StrToInt(edB.Text);
  c := FPColorTofpgColor(rgb);
  ColorWheel1.SetSelectedColor(c);  // This will trigger ColorWheel and ValueBar OnChange event
  FViaRGB := False;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FViaRGB := False;
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
  edS.Text := FormatFloat('0.000', ColorWheel1.Saturation);
  edV.Text := FormatFloat('0.000', ValueBar1.Value);
  Bevel1.BackgroundColor := ValueBar1.SelectedColor;
end;

procedure TMainForm.UpdateRGBComponents;
var
  rgb: TFPColor;
  c: TfpgColor;
begin
  c := ValueBar1.SelectedColor;
  rgb := fpgColorToFPColor(c);
  edR.Text := IntToStr(rgb.Red);
  edG.Text := IntToStr(rgb.Green);
  edB.Text := IntToStr(rgb.Blue);
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(349, 242, 537, 411);
  WindowTitle := 'ColorWheel test app';
  WindowPosition := wpUser;

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
    SetPosition(20, 20, 272, 244);
  end;

  ValueBar1 := TfpgValueBar.Create(self);
  with ValueBar1 do
  begin
    Name := 'ValueBar1';
    SetPosition(304, 20, 52, 244);
    OnChange  := @ColorChanged;
  end;

  Bevel1 := TfpgBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(20, 288, 76, 56);
  end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(116, 284, 52, 18);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Hue';
  end;

  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(116, 316, 52, 18);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Sat';
  end;

  Label3 := TfpgLabel.Create(self);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(116, 344, 52, 18);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Val';
  end;

  edH := TfpgEdit.Create(self);
  with edH do
  begin
    Name := 'edH';
    SetPosition(172, 280, 56, 26);
    TabOrder := 8;
    Text := '';
    FontDesc := '#Edit1';
    BackgroundColor := clWindowBackground;
  end;

  edS := TfpgEdit.Create(self);
  with edS do
  begin
    Name := 'edS';
    SetPosition(172, 308, 56, 26);
    TabOrder := 9;
    Text := '';
    FontDesc := '#Edit1';
    BackgroundColor := clWindowBackground;
  end;

  edV := TfpgEdit.Create(self);
  with edV do
  begin
    Name := 'edV';
    SetPosition(172, 336, 56, 26);
    TabOrder := 10;
    Text := '';
    FontDesc := '#Edit1';
    BackgroundColor := clWindowBackground;
  end;

  Label4 := TfpgLabel.Create(self);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(236, 284, 56, 18);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Red';
  end;

  Label5 := TfpgLabel.Create(self);
  with Label5 do
  begin
    Name := 'Label5';
    SetPosition(236, 316, 56, 18);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Green';
  end;

  Label6 := TfpgLabel.Create(self);
  with Label6 do
  begin
    Name := 'Label6';
    SetPosition(236, 344, 56, 18);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Blue';
  end;

  edR := TfpgEdit.Create(self);
  with edR do
  begin
    Name := 'edR';
    SetPosition(296, 280, 44, 26);
    TabOrder := 13;
    Text := '255';
    FontDesc := '#Edit1';
    OnExit  := @RGBChanged;
  end;

  edG := TfpgEdit.Create(self);
  with edG do
  begin
    Name := 'edG';
    SetPosition(296, 308, 44, 26);
    TabOrder := 14;
    Text := '255';
    FontDesc := '#Edit1';
    OnExit := @RGBChanged;
  end;

  edB := TfpgEdit.Create(self);
  with edB do
  begin
    Name := 'edB';
    SetPosition(296, 336, 44, 26);
    TabOrder := 15;
    Text := '255';
    FontDesc := '#Edit1';
    OnExit := @RGBChanged;
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
    SetPosition(304, 3, 64, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'ValueBar';
  end;

  Bevel2 := TfpgBevel.Create(self);
  with Bevel2 do
  begin
    Name := 'Bevel2';
    SetPosition(388, 8, 2, 260);
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
    TabOrder := 21;
    Text := 'New BG Color';
    OnChange  := @chkBGColorChange;
  end;

  {@VFD_BODY_END: MainForm}

  // link the two components
  ColorWheel1.ValueBar := ValueBar1;
//  ColorWheel1.BackgroundColor := clFuchsia;
//  ValueBar1.BackgroundColor := clFuchsia;
//  ColorWheel1.CursorSize := 400;
end;


end.
