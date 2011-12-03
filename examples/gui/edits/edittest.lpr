program edittest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_label, fpg_edit, fpg_button, fpg_radiobutton,
  fpg_listbox, fpg_checkbox, fpg_panel;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
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
    pl_integer: TfpgPanel;
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
    {@VFD_HEAD_END: MainForm}
    procedure btnQuitClicked(Sender: TObject);
    procedure rbClicked(Sender: TObject);
    procedure lbChange(Sender: TObject);
    procedure edtIntegerChange(Sender: TObject);
    procedure edtFloatChange(Sender: TObject);
    procedure edtCurrencyChange(Sender: TObject);
    procedure chbPasswdChanged(Sender: TObject);
    procedure chbSpaceChange(Sender: TObject);
    procedure chbFloatDecChange(Sender: TObject);
  public
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
          edtFloat.CustomDecimalSeparator := '.';
          edtCurrency.CustomDecimalSeparator := '.';
          if chbSpace.Checked then
          begin
            edtInteger.CustomThousandSeparator := ' ';
            edtFloat.CustomThousandSeparator := ' ';
            edtCurrency.CustomThousandSeparator := ' ';
          end
          else
          begin
            edtInteger.CustomThousandSeparator := ',';
            edtFloat.CustomThousandSeparator := ',';
            edtCurrency.CustomThousandSeparator := ',';
          end;
        end;
      1:
        begin
          edtFloat.CustomDecimalSeparator := ',';
          edtCurrency.CustomDecimalSeparator := ',';
          if chbSpace.Checked then
          begin
            edtInteger.CustomThousandSeparator := ' ';
            edtFloat.CustomThousandSeparator := ' ';
            edtCurrency.CustomThousandSeparator := ' ';
          end
          else
          begin
            edtInteger.CustomThousandSeparator := '.';
            edtFloat.CustomThousandSeparator := '.';
            edtCurrency.CustomThousandSeparator := '.';
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
  edtInteger.TextColor:= clBlue;
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
    edtInteger.CustomThousandSeparator := ' ';
    edtFloat.CustomThousandSeparator := ' ';
    edtCurrency.CustomThousandSeparator := ' ';
  end
  else
  begin
    if rbPoint.Checked then
    begin
      edtInteger.CustomThousandSeparator := ',';
      edtFloat.CustomThousandSeparator := ',';
      edtCurrency.CustomThousandSeparator := ',';
    end
    else
    begin
      edtInteger.CustomThousandSeparator := '.';
      edtFloat.CustomThousandSeparator := '.';
      edtCurrency.CustomThousandSeparator := '.';
    end;
  end;
end;

procedure TMainForm.chbFloatDecChange(Sender: TObject);
begin
  if chbFloatDec.Checked then
    edtFloat.Decimals := 3
  else
    edtFloat.Decimals := -1;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(376, 202, 392, 300);
  WindowTitle := 'Edit components';
  WindowPosition := wpScreenCenter;
  fpgSetNamedColor(clText1,clBlue);

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(8, 8, 196, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Text Edit';
  end;

  edtText := TfpgEdit.Create(self);
  with edtText do
  begin
    Name := 'edtText';
    SetPosition(24, 28, 120, 22);
    TabOrder := 1;
    Text := 'Hello World!';
    FontDesc := '#Edit1';
  end;

  chbPasswd := TfpgCheckBox.Create(self);
  with chbPasswd do
  begin
    Name := 'chbPasswd';
    SetPosition(24, 55, 152, 20);
    FontDesc := '#Label1';
    TabOrder := 2;
    Text := 'Password Mode';
    OnChange := @chbPasswdChanged;
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(8, 88, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Integer Edit';
  end;

  l_integervalue := TfpgLabel.Create(self);
  with l_integervalue do
  begin
    Name := 'l_integervalue';
    SetPosition(90, 88, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := '';
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(8, 144, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Float Edit';
  end;

  l_floatvalue := TfpgLabel.Create(self);
  with l_floatvalue do
  begin
    Name := 'l_floatvalue';
    SetPosition(90, 144, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := '';
  end;

  lblName4 := TfpgLabel.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(8, 200, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Currency Edit';
  end;

  l_currvalue := TfpgLabel.Create(self);
  with l_currvalue do
  begin
    Name := 'l_currvalue';
    SetPosition(90, 200, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := '';
  end;

  pl_integer := TfpgPanel.Create(self);
  with pl_integer do
  begin
    Name := 'pl_integer';
    SetPosition(20, 104, 130, 34);
    FontDesc := '#Label1';
    TextColor:= clGreen;
  end;

  edtInteger := TfpgEditInteger.Create(pl_integer);
  with edtInteger do
  begin
    Name := 'edtInteger';
    SetPosition(4, 4, 120, 22);
    TabOrder := 9;
    FontDesc := '#Edit1';
    Value := 12345;
    OnChange := @edtIntegerChange;
  end;

  edtFloat := TfpgEditFloat.Create(self);
  with edtFloat do
  begin
    Name := 'edtFloat';
    SetPosition(24, 164, 120, 22);
    TabOrder := 10;
    FontDesc := '#Edit1';
    Value := 12345.1234;
    OnChange := @edtFloatChange;
  end;

  edtCurrency := TfpgEditCurrency.Create(self);
  with edtCurrency do
  begin
    Name := 'edtCurrency';
    SetPosition(24, 220, 120, 22);
    TabOrder := 11;
    FontDesc := '#Edit1';
    Value := 12345.12;
    OnChange := @edtCurrencyChange;
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(296, 260, 75, 24);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := '';
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
    TabOrder := 15;
    Text := 'Space as ThousandSeparator';
    OnChange := @chbSpaceChange;
  end;

  chbFloatDec := TfpgCheckBox.Create(self);
  with chbFloatDec do
  begin
    Name := 'chbFloatDec';
    SetPosition(170, 220, 200, 20);
    FontDesc := '#Label1';
    TabOrder := 16;
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
    Hint := '';
    Text := 'Choose color for negative num.';
  end;

  {@VFD_BODY_END: MainForm}
  
  if edtFloat.CustomDecimalSeparator = '.' then
    rbPoint.Checked := True
  else
    rbComma.Checked := True;
  lbNegativeColor.SetFocus;
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


