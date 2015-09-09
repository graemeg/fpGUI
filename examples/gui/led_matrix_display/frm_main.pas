unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_ledmatrix,
  fpg_edit, fpg_radiobutton, fpg_panel, fpg_label;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    LEDMatrix1: TfpgLEDMatrix;
    Edit1: TfpgEdit;
    GroupBox1: TfpgGroupBox;
    RadioButton1: TfpgRadioButton;
    RadioButton2: TfpgRadioButton;
    RadioButton3: TfpgRadioButton;
    RadioButton4: TfpgRadioButton;
    RadioButton5: TfpgRadioButton;
    Label1: TfpgLabel;
    {@VFD_HEAD_END: MainForm}
    procedure PresetChanged(Sender: TObject);
    procedure Edit1Changed(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TMainForm.PresetChanged(Sender: TObject);
var
  i: integer;
begin
  i := TfpgRadioButton(Sender).Tag;
  case i of
    1: begin
         // Amber
         LEDMatrix1.LEDOnColor := TfpgColor($FFFEB438);
         LEDMatrix1.LEDOffColor := TfpgColor($FF62410F);
         LEDMatrix1.BackgroundColor := clBlack;
       end;
    2: begin
         // Blue
         LEDMatrix1.LEDOnColor := TfpgColor($FF38B4FE);
         LEDMatrix1.LEDOffColor := TfpgColor($FF0F4162);
         LEDMatrix1.BackgroundColor := clBlack;
       end;
    3: begin
         // Green
         LEDMatrix1.LEDOnColor := TfpgColor($FF00FE30);
         LEDMatrix1.LEDOffColor := TfpgColor($FF006217);
         LEDMatrix1.BackgroundColor := clBlack;
       end;
    4: begin
         // Gray
         LEDMatrix1.LEDOnColor := TfpgColor($FF000000);
         LEDMatrix1.LEDOffColor := TfpgColor($FF939B41);
         LEDMatrix1.BackgroundColor := TfpgColor($FFA4B441);
       end;
    5: begin
         // Red
         LEDMatrix1.LEDOnColor := TfpgColor($FFFE3800);
         LEDMatrix1.LEDOffColor := TfpgColor($FF621700);
         LEDMatrix1.BackgroundColor := clBlack;
       end;
  end;  { case }
end;

procedure TMainForm.Edit1Changed(Sender: TObject);
begin
  LEDMatrix1.Text := Edit1.Text;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' }
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(464, 211, 490, 258);
  WindowTitle := 'LEDMatrix demo';
  Hint := '';
  IconName := '';
  WindowPosition := wpOneThirdDown;

  LEDMatrix1 := TfpgLEDMatrix.Create(self);
  with LEDMatrix1 do
  begin
    Name := 'LEDMatrix1';
    SetPosition(40, 205, 405, 30);
    Text := 'fpGUI Rocks!';
  end;

  Edit1 := TfpgEdit.Create(self);
  with Edit1 do
  begin
    Name := 'Edit1';
    SetPosition(95, 20, 330, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 2;
    Text := 'fpGUI Rocks!';
    OnChange := @Edit1Changed;
  end;

  GroupBox1 := TfpgGroupBox.Create(self);
  with GroupBox1 do
  begin
    Name := 'GroupBox1';
    SetPosition(50, 60, 150, 125);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Preset Color Styles';
  end;

  RadioButton1 := TfpgRadioButton.Create(GroupBox1);
  with RadioButton1 do
  begin
    Name := 'RadioButton1';
    SetPosition(15, 20, 120, 19);
    Checked := True;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 1;
    Text := 'Amber';
    OnChange := @PresetChanged;
    Tag := 1;
  end;

  RadioButton2 := TfpgRadioButton.Create(GroupBox1);
  with RadioButton2 do
  begin
    Name := 'RadioButton2';
    SetPosition(15, 40, 120, 19);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 2;
    Text := 'Blue';
    OnChange := @PresetChanged;
    Tag := 2;
  end;

  RadioButton3 := TfpgRadioButton.Create(GroupBox1);
  with RadioButton3 do
  begin
    Name := 'RadioButton3';
    SetPosition(15, 60, 120, 19);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 3;
    Text := 'Green';
    OnChange := @PresetChanged;
    Tag := 3;
  end;

  RadioButton4 := TfpgRadioButton.Create(GroupBox1);
  with RadioButton4 do
  begin
    Name := 'RadioButton4';
    SetPosition(15, 80, 120, 19);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 4;
    Text := 'Gray';
    OnChange := @PresetChanged;
    Tag := 4;
  end;

  RadioButton5 := TfpgRadioButton.Create(GroupBox1);
  with RadioButton5 do
  begin
    Name := 'RadioButton5';
    SetPosition(15, 100, 120, 19);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 4;
    Text := 'Red';
    OnChange := @PresetChanged;
    Tag := 5;
  end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(15, 25, 80, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Display text:';
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
end;


end.
