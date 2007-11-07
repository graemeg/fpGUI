{
   This is still under development!!!!!!!!!!!!!!!!!
}

program calendartest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, gfxbase, fpgfx, gui_form, gui_popupcalendar, gui_edit,
  gui_button, gui_label, gfx_popupwindow, gui_combobox;

type
  TMainForm = class(TfpgForm)
  private
    procedure   btnDownClicked(Sender: TObject);
    procedure   btnDateFormatClicked(Sender: TObject);
    procedure   btnTodayClicked(Sender: TObject);
    procedure   DoDropDown;
  public
    {@VFD_HEAD_BEGIN: MainForm}
    edtName1: TfpgEdit;
    btnName1: TfpgButton;
    lblName1: TfpgLabel;
    lblName2: TfpgLabel;
    cbName1: TfpgComboBox;
    cbCalendar: TfpgCalendarCombo;
    btnDateFormat: TfpgButton;
    edtDateFormat: TfpgEdit;
    lblName3: TfpgLabel;
    lblName4: TfpgLabel;
    lblName5: TfpgLabel;
    btnToday: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    FDropDown: TfpgPopupCalendar;
    procedure   AfterCreate; override;
  end;
  
{@VFD_NEWFORM_DECL}

{ TMainForm }

procedure TMainForm.btnDownClicked(Sender: TObject);
begin
  DoDropDown;
end;

procedure TMainForm.btnDateFormatClicked(Sender: TObject);
begin
  cbCalendar.DateFormat := edtDateFormat.Text;
end;

procedure TMainForm.btnTodayClicked(Sender: TObject);
begin
  cbCalendar.Value := Now;
end;

procedure TMainForm.DoDropDown;
begin
  if (not Assigned(FDropDown)) or (not FDropDown.HasHandle) then
  begin
    FDropDown     := TfpgPopupCalendar.Create(nil, edtName1);
    FDropDown.ShowAt(self, edtName1.Left, edtName1.Top+edtName1.Height);
    FDropDown.PopupFrame:= True;
  end
  else
  begin
    FDropDown.Close;
    FreeAndNil(FDropDown);
  end;
end;

procedure TMainForm.AfterCreate;
begin
  inherited AfterCreate;
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(286, 234, 504, 270);
  WindowTitle := 'fpGUI Calendar Test';
  WindowPosition := wpUser;

  edtName1 := TfpgEdit.Create(self);
  with edtName1 do
  begin
    Name := 'edtName1';
    SetPosition(16, 48, 120, 22);
    Text := '';
    FontDesc := '#Edit1';
  end;

  btnName1 := TfpgButton.Create(self);
  with btnName1 do
  begin
    Name := 'btnName1';
    SetPosition(136, 48, 19, 22);
    Text := '';
    FontDesc := '#Label1';
    ImageName := 'sys.sb.down';
    OnClick := @btnDownClicked;
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(16, 32, 80, 16);
    Text := 'Enter a date:';
    FontDesc := '#Label1';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(68, 100, 276, 16);
    Text := '*****   This still needs some testing  *****';
    FontDesc := '#Label2';
    Color := clRed;
  end;

  cbName1 := TfpgComboBox.Create(self);
  with cbName1 do
  begin
    Name := 'cbName1';
    SetPosition(132, 144, 120, 23);
    Items.Add('line1');
    Items.Add('line2');
    Items.Add('line3');
    Items.Add('line4');
    Items.Add('line5');
    Items.Add('line6');
    FontDesc := '#List';
  end;

  cbCalendar := TfpgCalendarCombo.Create(self);
  with cbCalendar do
  begin
    Name := 'cbCalendar';
    SetPosition(132, 196, 120, 23);
    DateFormat := 'yyyy-mm-dd';
  end;

  btnDateFormat := TfpgButton.Create(self);
  with btnDateFormat do
  begin
    Name := 'btnDateFormat';
    SetPosition(300, 168, 75, 23);
    Text := 'Set Format';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnDateFormatClicked;
  end;

  edtDateFormat := TfpgEdit.Create(self);
  with edtDateFormat do
  begin
    Name := 'edtDateFormat';
    SetPosition(300, 196, 120, 21);
    Text := 'yy-mm-d';
    FontDesc := '#Edit1';
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(160, 52, 336, 15);
    Text := '<----  This one is fake. It only used the calendar window part';
    FontDesc := '#Label1';
    Color := clBlue;
  end;

  lblName4 := TfpgLabel.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(12, 148, 96, 15);
    Text := 'Normal Combo:';
    FontDesc := '#Label1';
  end;

  lblName5 := TfpgLabel.Create(self);
  with lblName5 do
  begin
    Name := 'lblName5';
    SetPosition(12, 200, 104, 15);
    Text := 'Calendar Combo:';
    FontDesc := '#Label1';
  end;

  btnToday := TfpgButton.Create(self);
  with btnToday do
  begin
    Name := 'btnToday';
    SetPosition(384, 168, 75, 23);
    Text := 'Today';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnTodayClicked;
  end;

  {@VFD_BODY_END: MainForm}
end;


{@VFD_NEWFORM_IMPL}

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



