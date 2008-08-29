program edittest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, fpgfx, gui_form, gui_label, gui_edit, gui_button, gui_radiobutton,
  gui_listbox, gfxbase, gui_checkbox, gui_panel, gui_hint;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    procedure btnQuitClicked(Sender: TObject);
    procedure rbClicked(Sender: TObject);
    procedure lbChange(Sender: TObject);
    procedure edtIntegerChange(Sender: TObject);
    procedure edtFloatChange(Sender: TObject);
    procedure edtCurrencyChange(Sender: TObject);
    procedure chbPasswdChanged(Sender: TObject);
    procedure chbSpaceChange(Sender: TObject);
    procedure chbFloatDecChange(Sender: TObject);
    procedure chbShowHintChange(Sender: TObject);
    procedure rb_border_1Change(Sender: TObject);
    procedure rb_border_2Change(Sender: TObject);
    procedure rb_border_3Change(Sender: TObject);
    procedure rb_border_5Change(Sender: TObject);
    procedure rb_margin_1Change(Sender: TObject);
    procedure rb_margin_2Change(Sender: TObject);
    procedure rb_margin_3Change(Sender: TObject);
    procedure rb_margin_5Change(Sender: TObject);
    procedure rb_time_1Change(Sender: TObject);
    procedure rb_time_2Change(Sender: TObject);
    procedure rb_time_3Change(Sender: TObject);
    procedure rb_time_5Change(Sender: TObject);
    procedure rb_color_blackChange(Sender: TObject);
    procedure rb_color_redChange(Sender: TObject);
    procedure rb_color_greenChange(Sender: TObject);
    procedure rb_color_blueChange(Sender: TObject);
    procedure rb_bgcolor_yellowChange(Sender: TObject);
    procedure rb_bgcolor_whiteChange(Sender: TObject);
    procedure rb_bgcolor_greenChange(Sender: TObject);
    procedure rb_bgcolor_blueChange(Sender: TObject);
    procedure rb_shadowcolor_grayChange(Sender: TObject);
    procedure rb_shadowcolor_blackChange(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    lblName1: TfpgLabel;
    edtText: TfpgEdit;
    chbPasswd: TfpgCheckBox;
    lblName2: TfpgLabel;
    l_integervalue: TfpgLabel;
    lblName3: TfpgLabel;
    l_floatvalue: TfpgLabel;
    lblName4: TfpgLabel;
    l_currvalue: TfpgLabel;
    edtInteger: TfpgEditInteger;
    edtFloat: TfpgEditFloat;
    edtCurrency: TfpgEditCurrency;
    btnQuit: TfpgButton;
    rbPoint: TfpgRadioButton;
    rbComma: TfpgRadioButton;
    chbSpace: TfpgCheckBox;
    chbFloatDec: TfpgCheckBox;
    lbNegativeColor: TfpgColorListBox;
    lblNegativeColor: TfpgLabel;
    p_border: TfpgPanel;
    rb_border_1: TfpgRadioButton;
    rb_border_2: TfpgRadioButton;
    rb_border_3: TfpgRadioButton;
    rb_border_5: TfpgRadioButton;
    p_margin: TfpgPanel;
    rb_margin_1: TfpgRadioButton;
    rb_margin_2: TfpgRadioButton;
    rb_margin_3: TfpgRadioButton;
    rb_margin_5: TfpgRadioButton;
    p_time: TfpgPanel;
    rb_time_1: TfpgRadioButton;
    rb_time_2: TfpgRadioButton;
    rb_time_3: TfpgRadioButton;
    rb_time_5: TfpgRadioButton;
    p_color: TfpgPanel;
    rb_color_black: TfpgRadioButton;
    rb_color_red: TfpgRadioButton;
    rb_color_green: TfpgRadioButton;
    rb_color_blue: TfpgRadioButton;
    p_bgcolor: TfpgPanel;
    rb_bgcolor_yellow: TfpgRadioButton;
    rb_bgcolor_white: TfpgRadioButton;
    rb_bgcolor_green: TfpgRadioButton;
    rb_bgcolor_blue: TfpgRadioButton;
    p_shadowcolor: TfpgPanel;
    rb_shadowcolor_gray: TfpgRadioButton;
    rb_shadowcolor_black: TfpgRadioButton;
    chbShowHint: TfpgCheckBox;
    {@VFD_HEAD_END: MainForm}
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}


{@VFD_NEWFORM_IMPL}

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.rbClicked(Sender: TObject);
begin
  if Sender is TfpgRadioButton then
    case (Sender as TfpgRadioButton).tag of
      0:
        begin
          edtFloat.DecimalSeparator := '.';
          edtCurrency.DecimalSeparator := '.';
          if chbSpace.Checked then
          begin
            edtInteger.ThousandSeparator := ' ';
            edtFloat.ThousandSeparator := ' ';
            edtCurrency.ThousandSeparator := ' ';
          end
          else
          begin
            edtInteger.ThousandSeparator := ',';
            edtFloat.ThousandSeparator := ',';
            edtCurrency.ThousandSeparator := ',';
          end;
        end;
      1:
        begin
          edtFloat.DecimalSeparator := ',';
          edtCurrency.DecimalSeparator := ',';
          if chbSpace.Checked then
          begin
            edtInteger.ThousandSeparator := ' ';
            edtFloat.ThousandSeparator := ' ';
            edtCurrency.ThousandSeparator := ' ';
          end
          else
          begin
            edtInteger.ThousandSeparator := '.';
            edtFloat.ThousandSeparator := '.';
            edtCurrency.ThousandSeparator := '.';
          end;
        end;
      end;
end;

procedure TMainForm.lbChange(Sender: TObject);
begin
  edtFloat.NegativeColor := lbNegativeColor.Color;
  edtInteger.NegativeColor := lbNegativeColor.Color;
end;

procedure TMainForm.edtIntegerChange(Sender: TObject);
begin
  l_integervalue.Text := IntToStr(edtInteger.Value);
end;

procedure TMainForm.edtFloatChange(Sender: TObject);
begin
  l_floatvalue.Text := FloatToStr(edtFloat.Value);
end;

procedure TMainForm.edtCurrencyChange(Sender: TObject);
begin
  l_currvalue.Text := CurrToStr(edtCurrency.Value);
end;

procedure TMainForm.chbPasswdChanged(Sender: TObject);
begin
  edtText.PasswordMode := chbPasswd.Checked;
end;

procedure TMainForm.chbSpaceChange(Sender: TObject);
begin
  if chbSpace.Checked then
    begin
    edtInteger.ThousandSeparator := ' ';
    edtFloat.ThousandSeparator := ' ';
    edtCurrency.ThousandSeparator := ' ';
    end
  else
    if rbPoint.Checked then
      edtInteger.ThousandSeparator := ','
    else
      edtInteger.ThousandSeparator := '.';
end;

procedure TMainForm.chbFloatDecChange(Sender: TObject);
begin
  if chbFloatDec.Checked then
    edtFloat.Decimals := 3
  else
    edtFloat.Decimals := -1;
end;

procedure TMainForm.chbShowHintChange(Sender: TObject);
begin
  if chbShowHint.Checked then
  begin
    edtText.ShowHint := True;
    edtInteger.ShowHint := True;
    edtFloat.ShowHint := True;
    edtCurrency.ShowHint := True;
  end
  else
  begin
    edtText.ShowHint := False;
    edtInteger.ShowHint := False;
    edtFloat.ShowHint := False;
    edtCurrency.ShowHint := False;
  end;
end;

procedure TMainForm.rb_border_1Change(Sender: TObject);
begin
  if rb_border_1.Checked then
    F_Hint.Border := 1;
end;

procedure TMainForm.rb_border_2Change(Sender: TObject);
begin
  if rb_border_2.Checked then
    F_Hint.Border := 2;
end;

procedure TMainForm.rb_border_3Change(Sender: TObject);
begin
  if rb_border_3.Checked then
    F_Hint.Border := 3;
end;

procedure TMainForm.rb_border_5Change(Sender: TObject);
begin
  if rb_border_5.Checked then
    F_Hint.Border := 5;
end;

procedure TMainForm.rb_margin_1Change(Sender: TObject);
begin
  if rb_margin_1.Checked then
    F_Hint.Margin := 1;
end;

procedure TMainForm.rb_margin_2Change(Sender: TObject);
begin
  if rb_margin_2.Checked then
    F_Hint.Margin := 2;
end;

procedure TMainForm.rb_margin_3Change(Sender: TObject);
begin
  if rb_margin_3.Checked then
    F_Hint.Margin := 3;
end;

procedure TMainForm.rb_margin_5Change(Sender: TObject);
begin
  if rb_margin_5.Checked then
    F_Hint.Margin := 5;
end;

procedure TMainForm.rb_time_1Change(Sender: TObject);
begin
  if rb_time_1.Checked then
    F_Hint.Time := 1000;
end;

procedure TMainForm.rb_time_2Change(Sender: TObject);
begin
  if rb_time_2.Checked then
    F_Hint.Time := 2000;
end;

procedure TMainForm.rb_time_3Change(Sender: TObject);
begin
  if rb_time_3.Checked then
    F_Hint.Time := 3000;
end;

procedure TMainForm.rb_time_5Change(Sender: TObject);
begin
  if rb_time_5.Checked then
    F_Hint.Time := 5000;
end;

procedure TMainForm.rb_color_blackChange(Sender: TObject);
begin
  if rb_color_black.Checked then
    F_Hint.LTextColor := clBlack;
end;

procedure TMainForm.rb_color_redChange(Sender: TObject);
begin
  if rb_color_red.Checked then
    F_Hint.LTextColor := clRed;
end;

procedure TMainForm.rb_color_greenChange(Sender: TObject);
begin
  if rb_color_green.Checked then
    F_Hint.LTextColor := clGreen;
end;

procedure TMainForm.rb_color_blueChange(Sender: TObject);
begin
  if rb_color_blue.Checked then
    F_Hint.LTextColor := clBlue;
end;

procedure TMainForm.rb_bgcolor_yellowChange(Sender: TObject);
begin
  if rb_bgcolor_yellow.Checked then
    F_Hint.LBackgroundColor := TfpgColor($ffffbf); //clYellow;
end;

procedure TMainForm.rb_bgcolor_whiteChange(Sender: TObject);
begin
  if rb_bgcolor_white.Checked then
    F_Hint.LBackgroundColor := clWhite;
end;

procedure TMainForm.rb_bgcolor_greenChange(Sender: TObject);
begin
  if rb_bgcolor_green.Checked then
    F_Hint.LBackgroundColor := clPaleGreen;
end;

procedure TMainForm.rb_bgcolor_blueChange(Sender: TObject);
begin
  if rb_bgcolor_blue.Checked then
    F_Hint.LBackgroundColor := clLightBlue;
end;

procedure TMainForm.rb_shadowcolor_grayChange(Sender: TObject);
begin
  if rb_shadowcolor_gray.Checked then
    F_Hint.ShadowColor := clGray;
end;

procedure TMainForm.rb_shadowcolor_blackChange(Sender: TObject);
begin
  if rb_shadowcolor_black.Checked then
    F_Hint.ShadowColor := clBlack;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(376, 202, 392, 550);
  WindowTitle := 'Edit components';
  WindowPosition := wpScreenCenter;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(8, 8, 196, 16);
    FontDesc := '#Label1';
    Text := 'Text Edit';
  end;

  edtText := TfpgEdit.Create(self);
  with edtText do
  begin
    Name := 'edtText';
    SetPosition(24, 28, 120, 22);
    TabOrder := 1;
//    Text := 'Hello World!';
    FontDesc := '#Edit1';
    Hint := 'generic edit control'
  end;

  chbPasswd := TfpgCheckBox.Create(self);
  with chbPasswd do
  begin
    Name := 'chbPasswd';
    SetPosition(24, 55, 152, 20);
    FontDesc := '#Label1';
    TabOrder := 2;
    Text := 'Password Mode';
    OnChange :=@chbPasswdChanged;
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(8, 88, 80, 16);
    FontDesc := '#Label1';
    Text := 'Integer Edit';
  end;
  
  l_integervalue := TfpgLabel.Create(self);
  with l_integervalue do
  begin
    Name := 'l_integervalue';
    SetPosition(90, 88, 80, 16);
    FontDesc := '#Label1';
    Text := '';
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(8, 144, 80, 16);
    FontDesc := '#Label1';
    Text := 'Float Edit';
  end;

  l_floatvalue := TfpgLabel.Create(self);
  with l_floatvalue do
  begin
    Name := 'l_floatvalue';
    SetPosition(90, 144, 80, 16);
    FontDesc := '#Label1';
    Text := '';
  end;

  lblName4 := TfpgLabel.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(8, 200, 80, 16);
    FontDesc := '#Label1';
    Text := 'Currency Edit';
  end;

  l_currvalue := TfpgLabel.Create(self);
  with l_currvalue do
  begin
    Name := 'l_currvalue';
    SetPosition(90, 200, 80, 16);
    FontDesc := '#Label1';
    Text := '';
  end;

  edtInteger := TfpgEditInteger.Create(self);
  with edtInteger do
  begin
    Name := 'edtInteger';
    SetPosition(24, 108, 120, 22);
    ShowThousand := True;
    ThousandSeparator := ',';
    onChange := @edtIntegerChange;
    Hint := 'integer edit control';
  end;

  edtFloat := TfpgEditFloat.Create(self);
  with edtFloat do
  begin
    Name := 'edtFloat';
    SetPosition(24, 164, 120, 22);
    ShowThousand := True;
    ThousandSeparator := ',';
    onChange := @edtFloatChange;
    Hint := 'float edit control';
  end;

  edtCurrency := TfpgEditCurrency.Create(self);
  with edtCurrency do
  begin
    Name := 'edtCurrency';
    SetPosition(24, 220, 120, 22);
//    ShowThousand := True;
    ThousandSeparator := ',';
//    Decimals := 2;
    Value := -123.45;
    onChange := @edtCurrencyChange;
    Hint := 'currency edit control';
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(296, 520, 75, 24);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 6;
    OnClick := @btnQuitClicked;
  end;

  rbPoint := TfpgRadioButton.Create(self);
  with rbPoint do
  begin
    Name := 'rbPoint';
    SetPosition(170, 136, 184, 20);
    Checked := True;
    FontDesc := '#Label1';
    GroupIndex := 1;
    TabOrder := 7;
    Text := 'Point as DecimalSeparator';
    Tag := 0;
    OnChange := @rbClicked;
  end;

  rbComma := TfpgRadioButton.Create(self);
  with rbComma do
  begin
    Name := 'rbComma';
    SetPosition(170, 160, 196, 20);
    FontDesc := '#Label1';
    GroupIndex := 1;
    TabOrder := 8;
    Text := 'Comma as DecimalSeparator';
    Tag := 1;
    OnChange := @rbClicked;
  end;

  chbSpace := TfpgCheckBox.Create(self);
  with chbSpace do
  begin
    Name := 'chbSpace';
    SetPosition(170, 200, 200, 20);
    FontDesc := '#Label1';
    Text := 'Space as ThousandSeparator';
    OnChange := @chbSpaceChange;
  end;
  
  chbFloatDec := TfpgCheckBox.Create(Self);
  with chbFloatDec do
  begin
    Name := 'chbFloatDec';
    SetPosition(170, 220, 200, 20);
    FontDesc := '#Label1';
    Text := 'Limit EditFloat to 3 decimals';
    OnChange := @chbFloatDecChange;
  end;
  
  lbNegativeColor := TfpgColorListBox.Create(self);
  with lbNegativeColor do
  begin
    Name := 'lbNegativeColor';
    SetPosition(200, 28, 176, 92);
    Color := clRed;
    OnChange := @lbChange;
  end;

  lblNegativeColor := TfpgLabel.Create(self);
  with lblNegativeColor do
  begin
    Name := 'lblNegativeColor';
    SetPosition(196, 8, 188, 16);
    FontDesc := '#Label1';
    Text := 'Choose color for negative num.';
  end;
  
  p_border := TfpgPanel.Create(self);
  with p_border do
  begin
    name := 'p_border';
    SetPosition(10,260,180,80);
    Layout := tlTop;
    FontDesc := '#Label1';
    Text := 'Hint border';
  end;
  
  rb_border_1 := CreateRadioButton(p_border,10,25,'1 pixel');
  rb_border_1.Checked:= True;
  rb_border_1.OnChange:= @rb_border_1Change;

  rb_border_2 := CreateRadioButton(p_border,10,50,'2 pixels');
  rb_border_2.OnChange:= @rb_border_2Change;

  rb_border_3 := CreateRadioButton(p_border,80,25,'3 pixels');
  rb_border_3.OnChange:= @rb_border_3Change;

  rb_border_5 := CreateRadioButton(p_border,80,50,'5 pixels');
  rb_border_5.OnChange:= @rb_border_5Change;

  p_margin := TfpgPanel.Create(self);
  with p_margin do
  begin
    name := 'p_margin';
    SetPosition(10,350,180,80);
    Layout := tlTop;
    FontDesc := '#Label1';
    Text := 'Hint margin';
  end;

  rb_margin_1 := CreateRadioButton(p_margin,10,25,'1 pixel');
  rb_margin_1.OnChange:= @rb_margin_1Change;

  rb_margin_2 := CreateRadioButton(p_margin,10,50,'2 pixels');
  rb_margin_2.OnChange:= @rb_margin_2Change;

  rb_margin_3 := CreateRadioButton(p_margin,80,25,'3 pixels');
  rb_margin_3.Checked:= True;
  rb_margin_3.OnChange:= @rb_margin_3Change;

  rb_margin_5 := CreateRadioButton(p_margin,80,50,'5 pixels');
  rb_margin_5.OnChange:= @rb_margin_5Change;

  p_time := TfpgPanel.Create(self);
  with p_time do
  begin
    name := 'p_time';
    SetPosition(10,440,180,80);
    Layout := tlTop;
    FontDesc := '#Label1';
    Text := 'Hint delay';
  end;

  rb_time_1 := CreateRadioButton(p_time,10,25,'1 second');
  rb_time_1.OnChange:= @rb_time_1Change;

  rb_time_2 := CreateRadioButton(p_time,10,50,'2 seconds');
  rb_time_2.Checked:= True;
  rb_time_2.OnChange:= @rb_time_2Change;

  rb_time_3 := CreateRadioButton(p_time,80,25,'3 seconds');
  rb_time_3.OnChange:= @rb_time_3Change;

  rb_time_5 := CreateRadioButton(p_time,80,50,'5 seconds');
  rb_time_5.OnChange:= @rb_time_5Change;

  p_color := TfpgPanel.Create(self);
  with p_color do
  begin
    name := 'p_color';
    SetPosition(200,260,180,80);
    Layout := tlTop;
    FontDesc := '#Label1';
    Text := 'Hint text color';
  end;

  rb_color_black := CreateRadioButton(p_color,10,25,'Black');
  rb_color_black.Checked:= True;
  rb_color_black.OnChange:= @rb_color_blackChange;

  rb_color_red := CreateRadioButton(p_color,10,50,'Red');
  rb_color_red.OnChange:= @rb_color_redChange;

  rb_color_green := CreateRadioButton(p_color,80,25,'Green');
  rb_color_green.OnChange:= @rb_color_greenChange;

  rb_color_blue := CreateRadioButton(p_color,80,50,'Blue');
  rb_color_blue.OnChange:= @rb_color_blueChange;

  p_bgcolor := TfpgPanel.Create(self);
  with p_bgcolor do
  begin
    name := 'p_bgcolor';
    SetPosition(200,350,180,80);
    Layout := tlTop;
    FontDesc := '#Label1';
    Text := 'Hint background color';
  end;
  
  rb_bgcolor_yellow := CreateRadioButton(p_bgcolor,10,25,'Yellow');
  rb_bgcolor_yellow.Checked:= True;
  rb_bgcolor_yellow.OnChange:= @rb_bgcolor_yellowChange;

  rb_bgcolor_white := CreateRadioButton(p_bgcolor,10,50,'White');
  rb_bgcolor_white.OnChange:= @rb_bgcolor_whiteChange;

  rb_bgcolor_green := CreateRadioButton(p_bgcolor,80,25,'Green');
  rb_bgcolor_green.OnChange:= @rb_bgcolor_greenChange;

  rb_bgcolor_blue := CreateRadioButton(p_bgcolor,80,50,'Blue');
  rb_bgcolor_blue.OnChange:= @rb_bgcolor_blueChange;

  p_shadowcolor := TfpgPanel.Create(self);
  with p_shadowcolor do
  begin
    name := 'p_shadowcolor';
    SetPosition(200,440,180,50);
    Layout := tlTop;
    FontDesc := '#Label1';
    Text := 'Shadow color';
  end;

  rb_shadowcolor_gray := CreateRadioButton(p_shadowcolor,10,25,'Gray');
  rb_shadowcolor_gray.Checked:= True;
  rb_shadowcolor_gray.OnChange:= @rb_shadowcolor_grayChange;

  rb_shadowcolor_black := CreateRadioButton(p_shadowcolor,80,25,'Black');
  rb_shadowcolor_black.OnChange:= @rb_shadowcolor_blackChange;

  chbShowHint := CreateCheckBox(Self,200,500,'Show hint');
  chbShowhint.OnChange:= @chbShowHintChange;

  F_Hint := TF_Hint.Create(nil);
  F_Hint.Visible := False;

  {@VFD_BODY_END: MainForm}
  
  if edtFloat.DecimalSeparator = '.' then
    rbPoint.Checked := True
  else
    rbComma.Checked := True;
end;

procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.


