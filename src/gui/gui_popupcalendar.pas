{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
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

unit gui_popupcalendar;

{$mode objfpc}{$H+}

{
    * This is still under development!!!!!!!!!
}
{.$Define DEBUG} // while developing the component


{ todo: Support highlighting special days. }
{ todo: Support custom colors. }
{ todo: Must be able to switch the first day of the week. }
{ todo: Create a TfpgDateTimeEdit component with options for Date, Time or Date & Time. }
{ todo: Create a 'Jump to Today'  property for TfpgCalendarCombo. }

interface

uses
  SysUtils, Classes, gfxbase, fpgfx, gui_edit, 
  gfx_widget, gui_button, gui_combobox, gui_grid,
  gui_dialogs, gfx_popupwindow;

type

  TfpgOnDateSetEvent = procedure(Sender: TObject; const ADate: TDateTime) of object;
  

  TfpgPopupCalendar = class(TfpgPopupWindow)
  private
    FMonthOffset: integer;
    FDate: TDateTime;
    FCallerWidget: TfpgWidget;
    FOnValueSet: TfpgOnDateSetEvent;
    {@VFD_HEAD_BEGIN: fpgPopupCalendar}
    edtYear: TfpgEdit;
    btnYearUp: TfpgButton;
    btnYearDown: TfpgButton;
    edtMonth: TfpgEdit;
    btnMonthUp: TfpgButton;
    btnMonthDown: TfpgButton;
    grdName1: TfpgStringGrid;
    {@VFD_HEAD_END: fpgPopupCalendar}
    function    GetDateElement(Index: integer): Word;
    procedure   PopulateDays;
    procedure   CalculateMonthOffset;
    function    CalculateCellDay(const ACol, ARow: LongWord): Word;
    procedure   SetDateElement(Index: integer; const AValue: Word);
    procedure   SetDateValue(const AValue: TDateTime);
    procedure   UpdateCalendar;
    procedure   btnYearUpClicked(Sender: TObject);
    procedure   btnYearDownClicked(Sender: TObject);
    procedure   btnMonthUpClicked(Sender: TObject);
    procedure   btnMonthDownClicked(Sender: TObject);
    procedure   grdName1DoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   grdName1KeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure   TearDown;
  protected
    FOrigFocusWin: TfpgWidget;
    procedure   HandlePaint; override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleShow; override;
    procedure   HandleHide; override;
    property    CallerWidget: TfpgWidget read FCallerWidget write FCallerWidget;
  public
    constructor Create(AOwner: TComponent; AOrigFocusWin: TfpgWidget); reintroduce;
    procedure   AfterCreate;
    property    Day: Word index 1 read GetDateElement write SetDateElement;
    property    Month: Word index 2 read GetDateElement write SetDateElement;
    property    Year: Word index 3 read GetDateElement write SetDateElement;
    property    OnValueSet: TfpgOnDateSetEvent read FOnValueSet write FOnValueSet;
  published
    property    Value: TDateTime read FDate write SetDateValue;
  end;
  
  
  TfpgCalendarCombo = class(TfpgAbstractComboBox)
  private
    FDate: TDateTime;
    FDateFormat: string;
    procedure   InternalOnValueSet(Sender: TObject; const ADate: TDateTime);
    procedure   SetDateFormat(const AValue: string);
    procedure   SetDateValue(const AValue: TDateTime);
    procedure   SetText(const AValue: string); override;
    function    GetText: string; override;
  protected
    function    HasText: boolean; override;
    procedure   DoDropDown; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    Value: TDateTime read FDate write SetDateValue;
    property    DateFormat: string read FDateFormat write SetDateFormat;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  gui_scrollbar;

{@VFD_NEWFORM_IMPL}

procedure TfpgPopupCalendar.PopulateDays;
var
  r, c: integer;
  lCellDay: Word;
begin
  grdName1.BeginUpdate;
  for r := 0 to 6 do
    for c := 1 to 7 do
    begin
      if r = 0 then
        grdName1.ColumnTitle[c] := ShortDayNames[c]
      else
      begin
        lCellDay := CalculateCellDay(c, r);
        if lCellDay = 0 then
          grdName1.Cells[c, r] := ''
        else
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

procedure TfpgPopupCalendar.TearDown;
var
  lD: Word;
  s: string;
begin
  s := grdName1.Cells[grdName1.FocusCol, grdName1.FocusRow];
  if s = '' then
    Exit; //==>
  lD := StrToInt(s);
  Value := EncodeDate(Year, Month, lD);
  if Assigned(OnValueSet) then
    OnValueSet(self, Value);
  {$IFDEF DEBUG}
  writeln('Selected date: ', FormatDateTime('yyyy-mm-dd', Value));
  {$ENDIF}
  Close;
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

function TfpgPopupCalendar.CalculateCellDay(const ACol, ARow: LongWord): Word;
begin
  Result :=  FMonthOffset + (ACol-1) + (ARow-1) * 7;
  if (Result < 1) or (Result > MonthDays[IsLeapYear(Year), Month]) then
    Result := 0;
end;

procedure TfpgPopupCalendar.SetDateElement(Index: integer; const AValue: Word);
var
  lD, lM, lY: Word;
begin
  if AValue > 0 then
  begin
    DecodeDate(FDate, lY, lM, lD);
    case Index of
      1: lD := AValue;
      2: lM := AValue;
      3: lY := AValue;
    end;
    FDate := EncodeDate(lY, lM, lD);
    UpdateCalendar;
  end;
end;

procedure TfpgPopupCalendar.SetDateValue(const AValue: TDateTime);
begin
  if FDate = AValue then
    Exit; //==>
  FDate := AValue;
  UpdateCalendar;
end;

procedure TfpgPopupCalendar.UpdateCalendar;
var
  lD, lM, lY: Word;
begin
  if FDate > 0 then
  begin
    CalculateMonthOffset;
    PopulateDays;
    edtYear.Text := IntToStr(Year);
    edtMonth.Text := LongMonthNames[Month];
    DecodeDate(FDate, lY, lM, lD);
//    writeln(' lD: ', lD);
    grdName1.FocusCol := (lD - FMonthOffset) mod 7 + 1;
    grdName1.FocusRow := (lD - FMonthOffset) div 7 + 1;
  end;
end;

procedure TfpgPopupCalendar.btnYearUpClicked(Sender: TObject);
begin
  Year := Year + 1;
end;

procedure TfpgPopupCalendar.btnYearDownClicked(Sender: TObject);
begin
  Year := Year - 1;
end;

procedure TfpgPopupCalendar.btnMonthUpClicked(Sender: TObject);
begin
  Value := IncMonth(FDate);
end;

procedure TfpgPopupCalendar.btnMonthDownClicked(Sender: TObject);
begin
  Value := IncMonth(FDate, -1);
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
            btnYearDownClicked(nil)   // Ctrl+PageUp
          else
            btnMonthDownClicked(nil); // PageUp
          consumed := True;
        end;
    keyPageDown:
        begin
          if (ssCtrl in shiftstate) then
            btnYearUpClicked(nil)     // Ctrl+PageDown
          else
            btnMonthUpClicked(nil);   // PageDown
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
end;

procedure TfpgPopupCalendar.HandleHide;
begin
  FocusRootWidget := FOrigFocusWin;
  FOrigFocusWin := nil;
  inherited HandleHide;
  if Assigned(FocusRootWidget) then
    FocusRootWidget.SetFocus;
end;

constructor TfpgPopupCalendar.Create(AOwner: TComponent;
  AOrigFocusWin: TfpgWidget);
begin
  inherited Create(AOwner);
  FOrigFocusWin := AOrigFocusWin;
  AfterCreate;
  FDate := Date;
  FMonthOffset := 0;
  UpdateCalendar;
end;

procedure TfpgPopupCalendar.AfterCreate;
begin
  {@VFD_BODY_BEGIN: fpgPopupCalendar}
  Name := 'fpgPopupCalendar';
  SetPosition(285, 249, 233, 142);
  //  WindowTitle := 'fpgPopupCalendar';
  //    WindowPosition := wpUser;
  //    Sizeable := False;

  edtYear := TfpgEdit.Create(self);
  with edtYear do
  begin
    Name := 'edtYear';
    SetPosition(0, 0, 72, 22);
    Text := '';
    FontDesc := '#Edit1';
    Focusable := False;
    BorderStyle := bsSingle;
  end;

  btnYearUp := TfpgButton.Create(self);
  with btnYearUp do
  begin
    Name := 'btnYearUp';
    SetPosition(72, 0, 13, 11);
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
    SetPosition(72, 11, 13, 11);
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
    SetPosition(85, 0, 135, 22);
    Text := '';
    FontDesc := '#Edit1';
    Focusable := False;
    BorderStyle := bsSingle;
  end;

  btnMonthUp := TfpgButton.Create(self);
  with btnMonthUp do
  begin
    Name := 'btnMonthUp';
    SetPosition(220, 0, 13, 11);
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
    SetPosition(220, 11, 13, 11);
    Text := '';
    Embedded := True;
    FontDesc := '#Label1';
    ImageMargin := 0;
    ImageName := 'sys.sb.down';
    Focusable := False;
    OnClick := @btnMonthDownClicked;
  end;

  grdName1 := TfpgStringGrid.Create(self);
  with grdName1 do
  begin
    Name := 'grdName1';
    SetPosition(0, 23, 233, 119);
    AddColumn('Mon', 33, taLeftJustify);
    AddColumn('Tue', 32, taLeftJustify);
    AddColumn('Wed', 33, taLeftJustify);
    AddColumn('Thu', 32, taLeftJustify);
    AddColumn('Fri', 33, taLeftJustify);
    AddColumn('Sat', 32, taLeftJustify);
    AddColumn('Sun', 33, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    RowCount := 6;
    ScrollBarStyle := ssNone;
    ColResizing := False;
    OnDoubleClick := @grdName1DoubleClick;
    OnKeyPress := @grdName1KeyPress;
  end;

  {@VFD_BODY_END: fpgPopupCalendar}
end;


{ TfpgCalendarCombo }

procedure TfpgCalendarCombo.SetDateValue(const AValue: TDateTime);
begin
  if FDate = AValue then
    Exit; //==>
  FDate := AValue;
  RePaint;
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

function TfpgCalendarCombo.HasText: boolean;
begin
  Result := FDate > 0;
end;

constructor TfpgCalendarCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDate := Now;
  DateFormat := ShortDateFormat;
end;

procedure TfpgCalendarCombo.InternalOnValueSet(Sender: TObject;
  const ADate: TDateTime);
begin
  Value := ADate;
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
      ShowMessage(E.Message);
    end;
  end;
end;

procedure TfpgCalendarCombo.DoDropDown;
var
  ddw: TfpgPopupCalendar;
begin
  if (not Assigned(FDropDown)) or (not FDropDown.HasHandle) then
  begin
    FDropDown := TfpgPopupCalendar.Create(nil, FocusRootWidget);
    ddw := TfpgPopupCalendar(FDropDown);
    ddw.CallerWidget := self;
    ddw.Value := FDate;
    ddw.ShowAt(Parent, Left, Top+Height);
    ddw.PopupFrame := True;
    ddw.OnValueSet := @InternalOnValueSet;
  end
  else
  begin
    FBtnPressed := False;
    FDropDown.Close;
    FreeAndNil(FDropDown);
  end;
end;

end.
