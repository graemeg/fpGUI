unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_widget,
  fpg_edit, fpg_form, fpg_label, fpg_button,
  fpg_dialogs, fpg_menu,
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
    {@VFD_HEAD_END: MainForm}
    procedure btnQuitClicked(Sender: TObject);
    procedure btnGetColorClicked(Sender: TObject);
    procedure ColorChanged(Sender: TObject);
    procedure RGBChanged(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation


{@VFD_NEWFORM_IMPL}

procedure TMainForm.ColorChanged(Sender: TObject);
begin
  btnGetColorClicked(nil);
end;

procedure TMainForm.RGBChanged(Sender: TObject);
var
  rgb: TRGBTriple;
  c: TfpgColor;
begin
  rgb.Red := StrToInt(edR.Text);
  rgb.Green := StrToInt(edG.Text);
  rgb.Blue := StrToInt(edB.Text);
  c := RGBTripleTofpgColor(rgb);
  ColorWheel1.SetSelectedColor(c);
  btnGetColorClicked(nil);
end;

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnGetColorClicked(Sender: TObject);
begin
  edH.Text := IntToStr(ColorWheel1.Hue);
  edS.Text := FormatFloat('0.000', ColorWheel1.Saturation);
  edV.Text := FormatFloat('0.000', ValueBar1.Value);
  Bevel1.BackgroundColor := ValueBar1.SelectedColor;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(330, 202, 542, 411);
  WindowTitle := 'ColorWheel test app';
  WindowPosition := wpUser;

  Button1 := TfpgButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(448, 376, 80, 26);
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
    OnChange  := @ColorChanged;
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
    SetPosition(140, 284, 52, 18);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Hue';
  end;

  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(140, 316, 52, 18);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Sat';
  end;

  Label3 := TfpgLabel.Create(self);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(140, 344, 52, 18);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Val';
  end;

  edH := TfpgEdit.Create(self);
  with edH do
  begin
    Name := 'edH';
    SetPosition(196, 280, 56, 26);
    TabOrder := 8;
    Text := '';
    FontDesc := '#Edit1';
  end;

  edS := TfpgEdit.Create(self);
  with edS do
  begin
    Name := 'edS';
    SetPosition(196, 308, 56, 26);
    TabOrder := 9;
    Text := '';
    FontDesc := '#Edit1';
  end;

  edV := TfpgEdit.Create(self);
  with edV do
  begin
    Name := 'edV';
    SetPosition(196, 336, 56, 26);
    TabOrder := 10;
    Text := '';
    FontDesc := '#Edit1';
  end;

  Label4 := TfpgLabel.Create(self);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(284, 284, 56, 18);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Red';
  end;

  Label5 := TfpgLabel.Create(self);
  with Label5 do
  begin
    Name := 'Label5';
    SetPosition(284, 316, 56, 18);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Green';
  end;

  Label6 := TfpgLabel.Create(self);
  with Label6 do
  begin
    Name := 'Label6';
    SetPosition(284, 344, 56, 18);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Blue';
  end;

  edR := TfpgEdit.Create(self);
  with edR do
  begin
    Name := 'edR';
    SetPosition(364, 280, 44, 26);
    TabOrder := 13;
    Text := '255';
    FontDesc := '#Edit1';
    OnExit  := @RGBChanged;
  end;

  edG := TfpgEdit.Create(self);
  with edG do
  begin
    Name := 'edG';
    SetPosition(364, 308, 44, 26);
    TabOrder := 14;
    Text := '255';
    FontDesc := '#Edit1';
    OnExit := @RGBChanged;
  end;

  edB := TfpgEdit.Create(self);
  with edB do
  begin
    Name := 'edB';
    SetPosition(364, 336, 44, 26);
    TabOrder := 15;
    Text := '255';
    FontDesc := '#Edit1';
    OnExit := @RGBChanged;
  end;

  {@VFD_BODY_END: MainForm}

  // link the two components
  ColorWheel1.ValueBar := ValueBar1;
//  ColorWheel1.BackgroundColor := clFuchsia;
//  ValueBar1.BackgroundColor := clFuchsia;
//  ColorWheel1.CursorSize := 400;
end;


end.
