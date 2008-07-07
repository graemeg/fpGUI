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
  gui_button, gui_label, gfx_popupwindow, gui_combobox, gui_checkbox, dateutils,
  fpgui_toolkit;

type
  TMainForm = class(TfpgForm)
  private
    procedure   btnDownClicked(Sender: TObject);
    procedure   btnDateFormatClicked(Sender: TObject);
    procedure   btnTodayClicked(Sender: TObject);
    procedure   btnMinDateClicked(Sender: TObject);
    procedure   btnMaxDateClicked(Sender: TObject);
    procedure   DoDropDown;
    procedure   cbCloseOnSelectChanged(Sender: TObject);
    procedure   DrawCalendar(month, year: integer);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    edtName1: TfpgEdit;
    btnName1: TfpgButton;
    lblName1: TfpgLabel;
    lblName2: TfpgLabel;
    cbName1: TfpgComboBox;
    cal: TfpgCalendarCombo;
    btnDateFormat: TfpgButton;
    edtDateFormat: TfpgEdit;
    lblName3: TfpgLabel;
    lblName4: TfpgLabel;
    lblName5: TfpgLabel;
    btnToday: TfpgButton;
    lblName6: TfpgLabel;
    edtMinDate: TfpgEdit;
    edtMaxDate: TfpgEdit;
    btnMinDate: TfpgButton;
    btnMaxDate: TfpgButton;
    cbCloseOnSelect: TfpgCheckBox;
    {@VFD_HEAD_END: MainForm}
    FDropDown: TfpgPopupCalendar;
    procedure   AfterCreate; override;
  end;
  
{@VFD_NEWFORM_DECL}

{ TMainForm }

procedure TMainForm.cbCloseOnSelectChanged(Sender: TObject);
begin
  cal.CloseOnSelect := TfpgCheckBox(Sender).Checked;
end;

type
  TStartDay = (wdSun, wdMon, wdTue, wdWed, wdThu, wdFri, wdSat);

procedure TMainForm.DrawCalendar(month, year: integer);
var
  weekstartday: TStartDay;
  i: integer;
  dayIndex: Byte;
  startingPos: integer;
  days: integer;
  currentDay: integer;
  s: string;
begin
  weekstartday := wdMon;  // 0=Sun, 1=Mon, 2=Tue etc.
  weekstartday := TStartDay(Ord(cbName1.FocusItem-1));
  
  // Month and year
  writeln('');
  writeln(LongMonthNames[month], ' ', year);

  // draw day-of-week header
  for i := Ord(weekstartday) to Ord(weekstartday)+6 do
  begin
    if (i < 7) then
      dayIndex := i
    else
      dayIndex := i - 7;

    write(ShortDayNames[dayIndex+1] + ' ');
  end;
  writeln('');
  
  startingPos := DayOfTheWeek(EncodeDate(year, month, 1)-Ord(weekstartday))-1;
  days := DaysInAMonth(year, month);
//  writeln('startingpos:', startingPos, '  days:', days);

  // draw blanks before first of month
  if not (startingPos = 6) then
  for i := 0 to startingPos do
    write('    ');

  // draw days of month
  currentDay := 1;
  i := startingPos;
  //for i := startingPos to days do
  while currentDay <= days do
  begin
    i := i + 1;
    if ((i mod 7) = 0) and (currentDay <> 1) then
      writeln('');
    if currentDay <= 9 then
      s := ' '
    else
      s := '';
    write(currentDay, '  ' + s);
    currentDay := currentDay + 1;
  end;
  
  writeln('');
  writeln('-----');
end;

procedure TMainForm.btnDownClicked(Sender: TObject);
begin
  DoDropDown;
end;

procedure TMainForm.btnDateFormatClicked(Sender: TObject);
begin
  cal.DateFormat := edtDateFormat.Text;
end;

procedure TMainForm.btnTodayClicked(Sender: TObject);
begin
  cal.DateValue := Now;
end;

procedure TMainForm.btnMinDateClicked(Sender: TObject);
var
  old: string;
begin
  old := ShortDateFormat;
  ShortDateFormat := 'yyyy-mm-dd';
  try
    cal.MinDate := StrToDate(edtMinDate.Text);
  finally
    ShortDateFormat := old;
  end;
end;

procedure TMainForm.btnMaxDateClicked(Sender: TObject);
var
  old: string;
begin
{
  old := ShortDateFormat;
  ShortDateFormat := 'yyyy-mm-dd';
  try
    cal.MaxDate := StrToDate(edtMaxDate.Text);
  finally
    ShortDateFormat := old;
  end;
  }
  DrawCalendar(StrToInt(edtMaxDate.Text), 2008);
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
  SetPosition(286, 234, 470, 253);
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
    TabOrder := 1;
    OnClick := @btnDownClicked;
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(16, 32, 80, 16);
    FontDesc := '#Label1';
    Text := 'Enter a date:';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(16, 100, 276, 16);
    FontDesc := '#Label2';
    Text := '*****   This still needs some testing  *****';
    TextColor := clRed;
  end;

  cbName1 := TfpgComboBox.Create(self);
  with cbName1 do
  begin
    Name := 'cbName1';
    SetPosition(132, 144, 120, 23);
    FontDesc := '#List';
    Items.Add('Sun');
    Items.Add('Mon');
    Items.Add('Tue');
    Items.Add('Wed');
    Items.Add('Thu');
    Items.Add('Fri');
    Items.Add('Sat');
    TabOrder := 4;
  end;

  cal := TfpgCalendarCombo.Create(self);
  with cal do
  begin
    Name := 'cal';
    SetPosition(132, 196, 120, 23);
    FontDesc := '#List';
    TabOrder := 5;
    DateFormat := 'yyyy-mm-dd';
  end;

  btnDateFormat := TfpgButton.Create(self);
  with btnDateFormat do
  begin
    Name := 'btnDateFormat';
    SetPosition(388, 148, 75, 23);
    Text := 'Set Format';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 6;
    OnClick := @btnDateFormatClicked;
  end;

  edtDateFormat := TfpgEdit.Create(self);
  with edtDateFormat do
  begin
    Name := 'edtDateFormat';
    SetPosition(288, 148, 92, 22);
    TabOrder := 7;
    Text := 'yy-mm-d';
    FontDesc := '#Edit1';
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(160, 48, 287, 15);
    FontDesc := '#Label1';
    Text := '<----  This one is fake. It only used the';
    TextColor := clBlue;
  end;

  lblName4 := TfpgLabel.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(12, 148, 96, 15);
    FontDesc := '#Label1';
    Text := 'Normal Combo:';
  end;

  lblName5 := TfpgLabel.Create(self);
  with lblName5 do
  begin
    Name := 'lblName5';
    SetPosition(12, 200, 104, 15);
    FontDesc := '#Label1';
    Text := 'Calendar Combo:';
  end;

  btnToday := TfpgButton.Create(self);
  with btnToday do
  begin
    Name := 'btnToday';
    SetPosition(388, 120, 75, 23);
    Text := 'Today';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 11;
    OnClick := @btnTodayClicked;
  end;

  lblName6 := TfpgLabel.Create(self);
  with lblName6 do
  begin
    Name := 'lblName6';
    SetPosition(192, 63, 246, 16);
    FontDesc := '#Label1';
    Text := 'calendar window part.';
  end;

  edtMinDate := TfpgEdit.Create(self);
  with edtMinDate do
  begin
    Name := 'edtMinDate';
    SetPosition(288, 176, 92, 22);
    TabOrder := 13;
    Text := '2005-01-01';
    FontDesc := '#Edit1';
  end;

  edtMaxDate := TfpgEdit.Create(self);
  with edtMaxDate do
  begin
    Name := 'edtMaxDate';
    SetPosition(288, 204, 92, 22);
    TabOrder := 14;
    Text := '2009-01-01';
    FontDesc := '#Edit1';
  end;

  btnMinDate := TfpgButton.Create(self);
  with btnMinDate do
  begin
    Name := 'btnMinDate';
    SetPosition(388, 176, 75, 23);
    Text := 'Min Date';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 15;
    OnClick := @btnMinDateClicked;
  end;

  btnMaxDate := TfpgButton.Create(self);
  with btnMaxDate do
  begin
    Name := 'btnMaxDate';
    SetPosition(388, 204, 75, 23);
    Text := 'Max Date';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 16;
    OnClick := @btnMaxDateClicked;
  end;

  cbCloseOnSelect := TfpgCheckBox.Create(self);
  with cbCloseOnSelect do
  begin
    Name := 'cbCloseOnSelect';
    SetPosition(328, 88, 120, 20);
    Checked := True;
    FontDesc := '#Label1';
    TabOrder := 17;
    Text := 'Close on select';
    OnChange := @cbCloseOnSelectChanged;
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



