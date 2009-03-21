{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2008 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      A calendar component. Soon it would be possible to use it in a
      popup windows like Calender Combobox, or directly in a Form.
}

unit fpg_popupcalendar;

{$mode objfpc}{$H+}

{.$Define DEBUG} // while developing the component

{
    *****************************************************************
    **********   This is still under heavy development!   ***********
    *****************************************************************
}


{ todo: Support highlighting special days. }
{ todo: Support custom colors. }
{ todo: Create a TfpgDateTimeEdit component with options for Date, Time or Date & Time. }
{ todo: Changing months and checking min/max limits takes into account the
        original date, not the selected day in the grid. It should use the
        selected day in grid. }
{ todo: Paint days out of min/max range in grey. }

interface

uses
  SysUtils,
  Classes,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_popupwindow,
  fpg_edit,
  fpg_button,
  fpg_combobox,
  fpg_basegrid,
  fpg_grid,
  fpg_dialogs{,
  fpg_checkbox};

type

  TfpgOnDateSetEvent = procedure(Sender: TObject; const ADate: TDateTime) of object;
  

  TfpgPopupCalendar = class(TfpgPopupWindow)
  private
    {@VFD_HEAD_BEGIN: fpgPopupCalendar}
    edtYear: TfpgEdit;
    btnYearUp: TfpgButton;
    btnYearDown: TfpgButton;
    edtMonth: TfpgEdit;
    btnMonthUp: TfpgButton;
    btnMonthDown: TfpgButton;
    btnToday: TfpgButton;
    grdName1: TfpgStringGrid;
    {@VFD_HEAD_END: fpgPopupCalendar}
    FMonthOffset: integer;
    FDate: TDateTime;
    FMaxDate: TDateTime;
    FMinDate: TDateTime;
    FWeekStartDay: integer;
    FCallerWidget: TfpgWidget;
    FOnValueSet: TfpgOnDateSetEvent;
    FCloseOnSelect: boolean;
    FThisMonthDays: array[0..6,0..5] of boolean;
    FWeeklyHoliday: integer;
    FDayColor: TfpgColor;
    FHolidayColor: TfpgColor;
    FSelectedColor: TfpgColor;
    function    GetDateElement(Index: integer): Word;
    procedure   PopulateDays;
    procedure   CalculateMonthOffset;
    function    CalculateCellDay(const ACol, ARow: Integer): Integer;
    procedure   SetDateElement(Index: integer; const AValue: Word);
    procedure   SetDateValue(const AValue: TDateTime);
    procedure   SetMaxDate(const AValue: TDateTime);
    procedure   SetMinDate(const AValue: TDateTime);
    procedure   SetWeekStartDay(const AValue: integer);
    procedure   SetWeeklyHoliday(const AValue: integer);
    procedure   SetDayColor(const AValue: TfpgColor);
    procedure   SetHolidayColor(const AValue: TfpgColor);
    procedure   SetSelectedColor(const AValue: TfpgColor);
    procedure   SetCloseOnSelect(const AValue: boolean);
    procedure   UpdateCalendar;
    procedure   btnYearUpClicked(Sender: TObject);
    procedure   btnYearDownClicked(Sender: TObject);
    procedure   btnMonthUpClicked(Sender: TObject);
    procedure   btnMonthDownClicked(Sender: TObject);
    procedure   btnTodayClicked(Sender: TObject);
    procedure   grdName1DoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   grdName1KeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure   grdName1DrawCell(Sender: TObject; const ARow, ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
    procedure   TearDown;
  protected
    FntNorm, FntBold: TfpgFont;
    FOrigFocusWin: TfpgWidget;
    procedure   HandlePaint; override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleShow; override;
    procedure   HandleHide; override;
    property    CallerWidget: TfpgWidget read FCallerWidget write FCallerWidget;
  public
    constructor Create(AOwner: TComponent; AOrigFocusWin: TfpgWidget); reintroduce;
    destructor  Destroy; override;
    procedure   AfterCreate;
    property    CloseOnSelect: boolean read FCloseOnSelect write SetCloseOnSelect default True;
    property    Day: Word index 1 read GetDateElement write SetDateElement;
    property    Month: Word index 2 read GetDateElement write SetDateElement;
    property    Year: Word index 3 read GetDateElement write SetDateElement;
    property    OnValueSet: TfpgOnDateSetEvent read FOnValueSet write FOnValueSet;
  published
    property    DateValue: TDateTime read FDate write SetDateValue;
    property    MinDate: TDateTime read FMinDate write SetMinDate;
    property    MaxDate: TDateTime read FMaxDate write SetMaxDate;
    property    WeekStartDay: integer read FWeekStartDay write SetWeekStartDay default 0;
    property    WeeklyHoliday: integer read FWeeklyHoliday write SetWeeklyHoliday default -1;
    property    DayColor: TfpgColor read FDayColor write SetDayColor;
    property    HolidayColor: TfpgColor read FHolidayColor write SetHolidayColor;
    property    SelectedColor: TfpgColor read FSelectedColor write SetSelectedColor;
  end;
  
  
  TfpgCalendarCombo = class(TfpgBaseStaticCombo)
  private
    FDate: TDateTime;
    FDateFormat: string;
    FMaxDate: TDateTime;
    FMinDate: TDateTime;
    FWeekStartDay: integer;
    FWeeklyHoliday: integer;
    FDayColor: TfpgColor;
    FHolidayColor: TfpgColor;
    FSelectedColor: TfpgColor;
    FCloseOnSelect: boolean;
    procedure   SetDateFormat(const AValue: string);
    procedure   SetDateValue(const AValue: TDateTime);
    procedure   SetMaxDate(const AValue: TDateTime);
    procedure   SetMinDate(const AValue: TDateTime);
    procedure   SetWeekStartDay(const AValue: integer);
    procedure   SetWeeklyHoliday(const AValue: integer);
    procedure   SetDayColor(const AValue: TfpgColor);
    procedure   SetHolidayColor(const AValue: TfpgColor);
    procedure   SetSelectedColor(const AValue: TfpgColor);
    procedure   SetText(const AValue: string); override;
    function    GetText: string; override;
    procedure   SetCloseOnSelect(const AValue: boolean);
  protected
    procedure   InternalOnValueSet(Sender: TObject; const ADate: TDateTime); virtual;
    function    HasText: boolean; override;
    procedure   DoDropDown; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    BackgroundColor;
    property    DateFormat: string read FDateFormat write SetDateFormat;
    property    DateValue: TDateTime read FDate write SetDateValue;
    property    FontDesc;
    property    MinDate: TDateTime read FMinDate write SetMinDate;
    property    MaxDate: TDateTime read FMaxDate write SetMaxDate;
    property    WeekStartDay: integer read FWeekStartDay write SetWeekStartDay default 0;
    property    WeeklyHoliday: integer read FWeeklyHoliday write SetWeeklyHoliday default -1;
    property    DayColor: TfpgColor read FDayColor write SetDayColor;
    property    HolidayColor: TfpgColor read FHolidayColor write SetHolidayColor;
    property    SelectedColor: TfpgColor read FSelectedColor write SetSelectedColor;
    property    ParentShowHint;
    property    ShowHint;
    { Clicking on calendar Today button will close the popup calendar by default }
    property    CloseOnSelect: boolean read FCloseOnSelect write SetCloseOnSelect default True;
    property    TabOrder;
    property    OnChange;
    property    OnCloseUp;
    property    OnDropDown;
    property    OnEnter;
    property    OnExit;
  end;


  TfpgCalendarCheckCombo = class(TfpgCalendarCombo)
  private
//    FCheckBox: TfpgCheckbox;
    FChecked: boolean;
    FCheckBoxRect: TfpgRect;
    procedure   InternalCheckBoxChanged(Sender: TObject);
    procedure   SetChecked(const AValue: Boolean);
  protected
    procedure   DoDrawText(const ARect: TfpgRect); override;
    procedure   InternalOnValueSet(Sender: TObject; const ADate: TDateTime); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    Checked: Boolean read FChecked write SetChecked;
    property    OnKeyPress;
  end;


{@VFD_NEWFORM_DECL}

implementation

uses
  fpg_scrollbar,
  fpg_constants;


{@VFD_NEWFORM_IMPL}

procedure TfpgPopupCalendar.PopulateDays;
var
  r, c: integer;
  lCellDay: Integer;
begin
  grdName1.BeginUpdate;
  for r := -1 to 5 do
    for c := 0 to 6 do
    begin
      if r = -1 then
        grdName1.ColumnTitle[c] := ShortDayNames[Succ((c+FWeekStartDay) mod 7)]  // ShortDayNames is 1-based indexing
      else
      begin
        lCellDay := CalculateCellDay(c, r);
        grdName1.Cells[c, r] := IntToStr(lCellDay);
      end;
    end;
  grdName1.EndUpdate;
end;

procedure TfpgPopupCalendar.grdName1DoubleClick(Sender: TObject;
  AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  TearDown;
end;

procedure TfpgPopupCalendar.grdName1KeyPress(Sender: TObject;
  var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  // Pass the grid event on to the TfpgPopupCalender instance.
  HandleKeyPress(KeyCode, ShiftState, consumed);
  Consumed := True;
end;

procedure TfpgPopupCalendar.grdName1DrawCell(Sender: TObject; const ARow, ACol: Integer; const ARect: TfpgRect;
          const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
begin
  with grdName1 do
  begin
    if FThisMonthDays[ACol,ARow] then
      if (ACol = FocusCol) and (ARow = FocusRow) then
        Canvas.SetTextColor(FSelectedColor)
      else
        Canvas.SetTextColor(FDayColor)
    else
      if (ACol = FocusCol) and (ARow = FocusRow) then
        Canvas.SetTextColor(FSelectedColor)
      else
        Canvas.SetTextColor(clShadow1);
    if FWeeklyHoliday >= FWeekStartDay then
      if ACol = FWeeklyHoliday - FWeekStartDay then
      begin
        Canvas.Font := FntBold;
        Canvas.SetTextColor(FHolidayColor);
      end
      else
        Canvas.Font := FntNorm
    else
      if (FWeeklyHoliday > -1) and (ACol = FWeeklyHoliday - FWeekStartDay + 7) then
      begin
        Canvas.Font := FntBold;
        Canvas.SetTextColor(FHolidayColor);
      end
      else
        Canvas.Font := FntNorm;
  end;
end;

procedure TfpgPopupCalendar.TearDown;
var
  lD: Word;
  s: string;
  d: TDateTime;
begin
  s := grdName1.Cells[grdName1.FocusCol, grdName1.FocusRow];
  if s = '' then
    Exit; //==>
  lD := StrToInt(s);
  d := EncodeDate(Year, Month, lD);
  if (d >= FMinDate) and (d <= FMaxDate) then
  begin
    DateValue := d;
    if Assigned(OnValueSet) then
      OnValueSet(self, DateValue);
    {$IFDEF DEBUG}
    writeln('Selected date: ', FormatDateTime('yyyy-mm-dd', DateValue));
    {$ENDIF}
    if CloseOnSelect then
      Close;
  end;
end;

function TfpgPopupCalendar.GetDateElement(Index: integer): Word;
var
  lD, lM, lY: Word;
begin
  DecodeDate(FDate, lY, lM, lD);
  case Index of
    1: Result := lD;
    2: Result := lM;
    3: Result := lY;
  end;
end;

procedure TfpgPopupCalendar.CalculateMonthOffset;
var
  lD, lM, lY: Word;
  lTheFirst: TDateTime;
begin
  DecodeDate(FDate, lY, lM, lD);
  lTheFirst := EncodeDate(lY, lM, 1);
  FMonthOffset := 2 - DayOfWeek(lTheFirst);
end;

function TfpgPopupCalendar.CalculateCellDay(const ACol, ARow: Integer): Integer;
begin
  if (FMonthOffset + FWeekStartDay) > 1 then
    Result :=  FMonthOffset - 7 + FWeekStartDay + ACol + ARow * 7
  else
    Result :=  FMonthOffset + FWeekStartDay + ACol + ARow * 7;
  if Result < 1 then
  begin
    if Month > 1 then
      Result := MonthDays[IsLeapYear(Year), Pred(Month)] + Result
    else
      Result := 31 + Result;
    FThisMonthDays[ACol,ARow] := False;
  end
  else
    if Result > MonthDays[IsLeapYear(Year), Month] then
    begin
      Result := Result - MonthDays[IsLeapYear(Year), Month];
      FThisMonthDays[ACol,ARow] := False;
    end
    else
      FThisMonthDays[ACol,ARow] := True;
end;

procedure TfpgPopupCalendar.SetDateElement(Index: integer; const AValue: Word);
var
  lD, lM, lY: Word;
  lDate: TDateTime;
begin
  if AValue > 0 then
  begin
    DecodeDate(FDate, lY, lM, lD);
    case Index of
      1: lD := AValue;
      2: lM := AValue;
      3: lY := AValue;
    end;
    try
      lDate := EncodeDate(lY, lM, lD);
      SetDateValue(lDate);
    except
      // do nothing!  Not nice?
    end;
  end;
end;

procedure TfpgPopupCalendar.SetDateValue(const AValue: TDateTime);
begin
  if FDate = AValue then
    Exit; //==>

  if (trunc(FDate) >= trunc(FMinDate)) then
    {$IFDEF DEBUG}
    writeln('Passed min test')
    {$ENDIF}
  else
    exit;

  if (FDate <= FMaxDate) then
    {$IFDEF DEBUG}
    writeln('Passed max test')
    {$ENDIF}
  else
    exit;
    
  {$IFDEF DEBUG} writeln('SetDateValue: ', FormatDateTime('yyyy-mm-dd', AValue)); {$ENDIF}
  FDate := AValue;
  UpdateCalendar;
end;

procedure TfpgPopupCalendar.SetMaxDate(const AValue: TDateTime);
begin
  if FMaxDate = AValue then
    Exit; //==>
  FMaxDate := AValue;

  // correct min/max values
  if FMinDate > AValue then
    FMinDate := IncMonth(AValue, -12); // one year less

  if FDate > FMaxDate then
  begin
    FDate := FMaxDate;
    UpdateCalendar;
  end;
end;

procedure TfpgPopupCalendar.SetMinDate(const AValue: TDateTime);
begin
  if FMinDate = AValue then
    Exit; //==>
  FMinDate := AValue;

  // correct min/max values
  if AValue > FMaxDate then
    FMaxDate := IncMonth(AValue, 12); // one year more

  if FDate < FMinDate then
  begin
    FDate := FMinDate;
    UpdateCalendar;
  end;
end;

procedure TfpgPopupCalendar.SetWeekStartDay(const AValue: integer);
begin
  if FWeekStartDay <> AValue then
    FWeekStartDay := AValue;
end;

procedure TfpgPopupCalendar.SetWeeklyHoliday(const AValue: integer);
begin
  if FWeeklyHoliday <> AValue then
    FWeeklyHoliday := AValue;
end;

procedure TfpgPopupCalendar.SetDayColor(const AValue: TfpgColor);
begin
  if FDayColor <> AValue then
    FDayColor := AValue;
end;

procedure TfpgPopupCalendar.SetHolidayColor(const AValue: TfpgColor);
begin
  if FHolidayColor <> AValue then
    FHolidayColor := AValue;
end;

procedure TfpgPopupCalendar.SetSelectedColor(const AValue: TfpgColor);
begin
  if FSelectedColor <> AValue then
    FSelectedColor := AValue;
end;

procedure TfpgPopupCalendar.SetCloseOnSelect(const AValue: boolean);
begin
  if FCloseOnSelect = AValue then
    Exit;
  FCloseOnSelect := AValue;
end;

procedure TfpgPopupCalendar.UpdateCalendar;
var
  lD, lM, lY: Word;
begin
  try
    grdName1.BeginUpdate;
    if (FDate >= FMinDate) and (FDate <= FMaxDate) then
    begin
      CalculateMonthOffset;
      PopulateDays;
      edtYear.Text := IntToStr(Year);
      edtMonth.Text := LongMonthNames[Month];
      DecodeDate(FDate, lY, lM, lD);

      grdName1.FocusCol := (lD - FMonthOffset - FWeekStartDay) mod 7;
      grdName1.FocusRow := (lD - FMonthOffset - FWeekStartDay) div 7;
      if (FMonthOffset + FWeekStartDay) > 1 then
        grdName1.FocusRow := grdName1.FocusRow + 1;
//      grdName1.Invalidate;
    end;
  finally
    grdName1.EndUpdate;
  end;
end;

procedure TfpgPopupCalendar.btnYearUpClicked(Sender: TObject);
var
  d: TDateTime;
begin
  d := IncMonth(FDate, 12);
  if d <= FMaxDate then
    DateValue := d;
end;

procedure TfpgPopupCalendar.btnYearDownClicked(Sender: TObject);
var
  d: TDateTime;
begin
  d := IncMonth(FDate, -12);
  if d >= FMinDate then
    DateValue := d;
end;

procedure TfpgPopupCalendar.btnMonthUpClicked(Sender: TObject);
var
  d: TDateTime;
begin
  d := IncMonth(FDate);
  if d <= FMaxDate then
    DateValue := d;
end;

procedure TfpgPopupCalendar.btnMonthDownClicked(Sender: TObject);
var
  d: TDateTime;
begin
  d := IncMonth(FDate, -1);
  if d >= FMinDate then
    DateValue := d;
end;

procedure TfpgPopupCalendar.btnTodayClicked(Sender: TObject);
begin
  if Now >= FMinDate then
  begin
    DateValue := Now;
    TearDown;
  end;
end;

procedure TfpgPopupCalendar.HandlePaint;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;
  if PopupFrame then
    Canvas.SetClipRect(fpgRect(1, 1, Width-2, Height-2));
  Canvas.Clear(clWindowBackground);
  Canvas.ClearClipRect;
  Canvas.EndDraw;
end;

procedure TfpgPopupCalendar.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  case keycode of
    keyUp:
        begin
          if (ssCtrl in shiftstate) then
          begin
            btnYearUpClicked(nil);    // Ctrl+Up Arrow
            consumed := True;
          end;
        end;
    keyDown:
        begin
          if (ssCtrl in shiftstate) then
          begin
            btnYearDownClicked(nil);  // Ctrl+Down Arrow
            consumed := True;
          end;
        end;
    keyLeft:
        begin
          if (ssCtrl in shiftstate) then
          begin
            btnMonthDownClicked(nil); // Ctrl+Left Arrow
            consumed := True;
          end;
        end;
    keyRight:
        begin
          if (ssCtrl in shiftstate) then
          begin
            btnMonthUpClicked(nil);   // Ctrl+Right Arrow
            consumed := True;
          end;
        end;
    keyPageUp:
        begin
          if (ssCtrl in shiftstate) then
            btnYearUpClicked(nil)   // Ctrl+PageUp
          else
            btnMonthUpClicked(nil); // PageUp
          consumed := True;
        end;
    keyPageDown:
        begin
          if (ssCtrl in shiftstate) then
            btnYearDownClicked(nil)     // Ctrl+PageDown
          else
            btnMonthDownClicked(nil);   // PageDown
          consumed := True;
        end;
  end;
  
  if not consumed then
  begin
    if keycode = keyEnter then
    begin
      consumed := True;
      TearDown;
    end
    else if keycode = keyEscape then
    begin
      consumed := True;
      Close;
    end;
  end;
  
  if not consumed then
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfpgPopupCalendar.HandleShow;
begin
  inherited HandleShow;
  grdName1.SetFocus;
  {$IFDEF DEBUG}
  writeln('Min: ', FormatDateTime('yyyy-mm-dd', MinDate),
          '   Max: ', FormatDateTime('yyyy-mm-dd', MaxDate));
  {$ENDIF}
end;

procedure TfpgPopupCalendar.HandleHide;
begin
  FocusRootWidget := FOrigFocusWin;
  FOrigFocusWin := nil;
  inherited HandleHide;
  if Assigned(FocusRootWidget) then
    FocusRootWidget.SetFocus;
end;

constructor TfpgPopupCalendar.Create(AOwner: TComponent; AOrigFocusWin: TfpgWidget);
begin
  inherited Create(AOwner);
  FntNorm:= fpgApplication.GetFont('arial-9');
  FntBold:= fpgApplication.GetFont('arial-9:bold');

  FOrigFocusWin := AOrigFocusWin;
  AfterCreate;
  FDate := Date;
  FWeekStartDay := 0;
  FWeeklyHoliday := -1;
  FDayColor := clText1;
  FHolidayColor := clText1;
  FSelectedColor := clWhite;
  FMonthOffset := 0;
  FCloseOnSelect := True;
  UpdateCalendar;
end;

destructor TfpgPopupCalendar.Destroy;
begin
  FntBold.Free;
  FntNorm.Free;
  inherited Destroy;
end;

procedure TfpgPopupCalendar.AfterCreate;
begin
  {@VFD_BODY_BEGIN: fpgPopupCalendar}
  Name := 'fpgPopupCalendar';
  SetPosition(285, 249, 233, 142);
//  WindowTitle := 'fpgPopupCalendar';
//  Sizeable := False;
//  WindowPosition := wpUser;

  edtYear := TfpgEdit.Create(self);
  with edtYear do
  begin
    Name := 'edtYear';
    SetPosition(0, 0, 37, 22);
    Text := '';
    FontDesc := '#Edit1';
    Focusable := False;
    BorderStyle := ebsSingle;
  end;

  btnYearUp := TfpgButton.Create(self);
  with btnYearUp do
  begin
    Name := 'btnYearUp';
    SetPosition(37, 0, 13, 11);
    Text := '';
    Embedded := True;
    FontDesc := '#Label1';
    ImageMargin := 0;
    ImageName := 'sys.sb.up';
    Focusable := False;
    OnClick := @btnYearUpClicked;
  end;

  btnYearDown := TfpgButton.Create(self);
  with btnYearDown do
  begin
    Name := 'btnYearDown';
    SetPosition(37, 11, 13, 11);
    Text := '';
    Embedded := True;
    FontDesc := '#Label1';
    ImageMargin := 0;
    ImageName := 'sys.sb.down';
    Focusable := False;
    OnClick := @btnYearDownClicked;
  end;

  edtMonth := TfpgEdit.Create(self);
  with edtMonth do
  begin
    Name := 'edtMonth';
    SetPosition(50, 0, 100, 22);
    Text := '';
    FontDesc := '#Edit1';
    Focusable := False;
    BorderStyle := ebsSingle;
  end;

  btnMonthUp := TfpgButton.Create(self);
  with btnMonthUp do
  begin
    Name := 'btnMonthUp';
    SetPosition(150, 0, 13, 11);
    Text := '';
    Embedded := True;
    FontDesc := '#Label1';
    ImageMargin := 0;
    ImageName := 'sys.sb.up';
    Focusable := False;
    OnClick := @btnMonthUpClicked;
  end;

  btnMonthDown := TfpgButton.Create(self);
  with btnMonthDown do
  begin
    Name := 'btnMonthDown';
    SetPosition(150, 11, 13, 11);
    Text := '';
    Embedded := True;
    FontDesc := '#Label1';
    ImageMargin := 0;
    ImageName := 'sys.sb.down';
    Focusable := False;
    OnClick := @btnMonthDownClicked;
  end;
  
  btnToday := TfpgButton.Create(self);
  with btnToday do
  begin
    Name := 'btnToday';
    SetPosition(164, 0, 70, 22);
    Text := 'Today';
    FontDesc := '#Label1';
    Focusable := True;
    OnClick := @btnTodayClicked;
  end;

  grdName1 := TfpgStringGrid.Create(self);
  with grdName1 do
  begin
    Name := 'grdName1';
    SetPosition(0, 23, 233, 119);
    AddColumn('Sun', 33, taCenter);
    AddColumn('Mon', 32, taCenter);
    AddColumn('Tue', 33, taCenter);
    AddColumn('Wed', 32, taCenter);
    AddColumn('Thu', 33, taCenter);
    AddColumn('Fri', 32, taCenter);
    AddColumn('Sat', 33, taCenter);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    RowCount := 6;
    ScrollBarStyle := ssNone;
    OnDoubleClick := @grdName1DoubleClick;
    OnKeyPress := @grdName1KeyPress;
    OnDrawCell := @grdName1DrawCell;
  end;

  {@VFD_BODY_END: fpgPopupCalendar}
{
  // Setup localization
  // UI Designer doesn't support resource strings yet!
  grdName1.ColumnTitle[0] := rsShortSun;
  grdName1.ColumnTitle[1] := rsShortMon;
  grdName1.ColumnTitle[2] := rsShortTue;
  grdName1.ColumnTitle[3] := rsShortWed;
  grdName1.ColumnTitle[4] := rsShortThu;
  grdName1.ColumnTitle[5] := rsShortFri;
  grdName1.ColumnTitle[6] := rsShortSat;
}
  btnToday.Text := rsToday;
end;


{ TfpgCalendarCombo }

procedure TfpgCalendarCombo.SetDateValue(const AValue: TDateTime);
begin
  if FDate = AValue then
    Exit; //==>
  FDate := AValue;
  RePaint;
end;

procedure TfpgCalendarCombo.SetMaxDate(const AValue: TDateTime);
begin
  if FMaxDate = AValue then
    Exit; //==>
  FMaxDate := AValue;

  // correct min/max values
  if FMinDate > AValue then
    FMinDate := IncMonth(AValue, -12); // one year less

  if FDate > FMaxDate then
  begin
    FDate := FMaxDate;
    Repaint;
  end;
end;

procedure TfpgCalendarCombo.SetMinDate(const AValue: TDateTime);
begin
  if FMinDate = AValue then
    Exit; //==>
  FMinDate := AValue;
  
  // correct min/max values
  if AValue > FMaxDate then
    FMaxDate := IncMonth(AValue, 12); // one year more

  if FDate < FMinDate then
  begin
    FDate := FMinDate;
    Repaint;
  end;
end;

procedure TfpgCalendarCombo.SetWeekStartDay(const AValue: integer);
begin
  if FWeekStartDay <> AValue then
    FWeekStartDay := AValue;
end;

procedure TfpgCalendarCombo.SetWeeklyHoliday(const AValue: integer);
begin
  if FWeeklyHoliday <> AValue then
    FWeeklyHoliday := AValue;
end;

procedure TfpgCalendarCombo.SetSelectedColor(const AValue: TfpgColor);
begin
  if FSelectedColor <> AValue then
    FSelectedColor := AValue;
end;

procedure TfpgCalendarCombo.SetDayColor(const AValue: TfpgColor);
begin
  if FDayColor <> AValue then
    FDayColor := AValue;
end;

procedure TfpgCalendarCombo.SetHolidayColor(const AValue: TfpgColor);
begin
  if FHolidayColor <> AValue then
    FHolidayColor := AValue;
end;

procedure TfpgCalendarCombo.SetText(const AValue: string);
begin
  try
    FDate := StrToDateTime(AValue);
  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
    end;
  end;
end;

function TfpgCalendarCombo.GetText: string;
begin
  Result := FormatDateTime(FDateFormat, FDate);
end;

procedure TfpgCalendarCombo.SetCloseOnSelect(const AValue: boolean);
begin
  if FCloseOnSelect = AValue then
    Exit; //==>
  FCloseOnSelect := AValue;
end;

function TfpgCalendarCombo.HasText: boolean;
begin
  Result := FDate >= FMinDate;
end;

constructor TfpgCalendarCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMinDate := EncodeDate(1900, 01, 01);
  FMaxDate := EncodeDate(2100, 01, 31);
  FWeeklyHoliday := -1;
  FDate := Now;
  FCloseOnSelect := True;
  DateFormat := ShortDateFormat;
end;

procedure TfpgCalendarCombo.InternalOnValueSet(Sender: TObject;
  const ADate: TDateTime);
begin
  DateValue := ADate;
  if Assigned(OnChange) then
    OnChange(self);
  {$IFDEF DEBUG}
  writeln('New value: ', FormatDateTime(FDateFormat, ADate));
  {$ENDIF}
end;

procedure TfpgCalendarCombo.SetDateFormat(const AValue: string);
var
  OldFormat: string;
begin
  if FDateFormat = AValue then
    Exit; //==>
  OldFormat := FDateFormat;
  FDateFormat := AValue;
  try
    FormatDateTime(FDateFormat, FDate);
    RePaint;
  except
    on E: Exception do
    begin
      FDateFormat := OldFormat;
      fpgApplication.HandleException(self);
    end;
  end;
end;

procedure TfpgCalendarCombo.DoDropDown;
var
  ddw: TfpgPopupCalendar;
begin
  if (not Assigned(FDropDown)) or (not FDropDown.HasHandle) then
  begin
    FreeAndNil(FDropDown);  // safety measure
    FDropDown := TfpgPopupCalendar.Create(nil, FocusRootWidget);
    ddw := TfpgPopupCalendar(FDropDown);
    ddw.DontCloseWidget := self;
    { Set to false CloseOnSelect to leave opened popup calendar menu }
    ddw.CloseOnSelect := CloseOnSelect;
    ddw.CallerWidget  := self;

    if Assigned(OnDropDown) then
      OnDropDown(self);

    ddw.MinDate       := FMinDate;
    ddw.MaxDate       := FMaxDate;
    ddw.DateValue     := FDate;
    ddw.WeekStartDay  := FWeekStartDay;
    ddw.WeeklyHoliday := FWeeklyHoliday;
    ddw.DayColor        := FDayColor;
    ddw.HolidayColor    := FHolidayColor;
    ddw.SelectedColor   := FSelectedColor;
    ddw.ShowAt(Parent, Left, Top+Height);
    { I added this call to UpdateCalendar because sometimes after
      btnTodayClicked event, reopeing the dropdown menu gave an empty calendar }
    ddw.UpdateCalendar; //slapshot
    ddw.PopupFrame    := True;
    ddw.OnValueSet    := @InternalOnValueSet;
    ddw.OnClose       := @InternalOnClose;
  end
  else
  begin
    FBtnPressed := False;
    FDropDown.Close;
    FreeAndNil(FDropDown);
  end;
end;

{ TfpgCalendarCheckCombo }

procedure TfpgCalendarCheckCombo.InternalCheckBoxChanged(Sender: TObject);
begin
  RePaint;
end;

procedure TfpgCalendarCheckCombo.SetChecked(const AValue: Boolean);
begin
  if AValue = FChecked then
    Exit; //==>
  FChecked := Avalue;
  InternalCheckBoxChanged(nil);
end;

procedure TfpgCalendarCheckCombo.DoDrawText(const ARect: TfpgRect);
var
  lRect: TfpgRect;
  flags: TFTextFlags;
  lColor: TfpgColor;
begin
  lRect := ARect;
  lRect.Left := lRect.Left+FCheckBoxRect.Width + 1;
  lRect.Width := lRect.Width - (FCheckBoxRect.Width + 1) - FMargin;
  flags := [txtRight, txtVCenter];
  if HasText then
  begin
    if not FChecked then
      Canvas.SetTextColor(clShadow1)
    else
    begin
      if Focused then
        Canvas.SetTextColor(clSelectionText)
      else
        Canvas.SetTextColor(TextColor);
    end;
    fpgStyle.DrawString(Canvas, lRect.Left {FMargin+1}, {lRect.Top }FMargin, Text, Enabled);
  end
  else
  begin
    Canvas.SetTextColor(clShadow1);
    fpgStyle.DrawString(Canvas, lRect.Left {FMargin+1}, {lRect.Top} FMargin, ExtraHint, Enabled);
  end;
end;

procedure TfpgCalendarCheckCombo.InternalOnValueSet(Sender: TObject;
  const ADate: TDateTime);
begin
  inherited InternalOnValueSet(Sender, ADate);
  Checked := True;
//  InternalCheckBoxChanged(nil);
end;

procedure TfpgCalendarCheckCombo.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  if keycode = keyEscape then
  begin
    consumed := True;
    Checked := False;
  end;
  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfpgCalendarCheckCombo.HandlePaint;
var
  r: TfpgRect;
  img: TfpgImage;
  ix: integer;
begin
  inherited HandlePaint;

  r := FCheckBoxRect;
  OffsetRect(r, 2, 2);
//  r.SetRect(4, 4, 17, 17);
//  PrintRect(r);

  // calculate which image to paint.
  if Enabled then
  begin
    ix := Ord(FChecked);
    //if FIsPressed then
      //Inc(ix, 2);
  end
  else
    ix := (2 + (Ord(FChecked) * 2)) - Ord(FChecked);

  // paint the check (in this case a X)
//  tx := r.right + 8;
  img := fpgImages.GetImage('sys.checkboxes');    // Do NOT localize
  Canvas.DrawImagePart(r.Left, r.Top, img, ix*13, 0, 13, 13);
end;

constructor TfpgCalendarCheckCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChecked := True;
  FCheckBoxRect.SetRect(2, 2, 17, 17);
{
  FCheckBox := TfpgCheckBox.Create(self);
  with FCheckbox do
  begin
    Name := '_IntCheckBox';
    SetPosition(2, 2, 18, 17);
    Checked := True;
    FontDesc := '#Label1';
    Text := '';
//    BackgroundColor := self.BackgroundColor;
    BackgroundColor := clMagenta;
    Focusable := False;
    OnChange := @InternalCheckBoxChanged;
  end;
}
end;


end.
