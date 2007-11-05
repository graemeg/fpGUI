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

{$Define DEBUG} // while developing the component

{
  TODO:
    * This is still under development!!!!!!!!!!!!!!!!!!!!!!
    * Support highlighting special days.
    * Support custom colors.
    * Must be able to switch the first day of the week.
    * Keyboard support.
}

interface

uses
  SysUtils, Classes, gfxbase, fpgfx, gui_edit, 
  gfx_widget, gui_form, gui_label, gui_button,
  gui_listbox, gui_memo, gui_combobox, gui_grid, 
  gui_dialogs, gui_checkbox, gui_tree, gui_trackbar, 
  gui_progressbar, gui_radiobutton, gui_tab, gui_menu,
  gui_bevel, gfx_popupwindow;

type

  TfpgPopupCalendar = class(TfpgPopupWindow)
  private
    FMonthOffset: integer;
    FDate: TDateTime;
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
  protected
    procedure   HandlePaint; override;
  public
    {@VFD_HEAD_BEGIN: fpgPopupCalendar}
    edtYear: TfpgEdit;
    btnYearUp: TfpgButton;
    btnYearDown: TfpgButton;
    edtMonth: TfpgEdit;
    btnMonthUp: TfpgButton;
    btnMonthDown: TfpgButton;
    grdName1: TfpgStringGrid;
    {@VFD_HEAD_END: fpgPopupCalendar}
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate;
    property    Day: Word index 1 read GetDateElement write SetDateElement;
    property    Month: Word index 2 read GetDateElement write SetDateElement;
    property    Year: Word index 3 read GetDateElement write SetDateElement;
  published
    property    Value: TDateTime read FDate write SetDateValue;
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
  lCellText: string;
begin
  lCellText := '';
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
var
  lD: Word;
  s: string;
begin
  s := grdName1.Cells[grdName1.FocusCol, grdName1.FocusRow];
  if s = '' then
    Exit; //==>
  lD := StrToInt(s);
  Value := EncodeDate(Year, Month, lD);
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
  if FDate > 0 then
  begin
    DecodeDate(FDate, lY, lM, lD);
    lTheFirst := EncodeDate(lY, lM, 1);
    FMonthOffset := 2 - DayOfWeek(lTheFirst);
 end;
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
begin
  CalculateMonthOffset;
  PopulateDays;
  edtYear.Text := IntToStr(Year);
  edtMonth.Text := LongMonthNames[Month];
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

constructor TfpgPopupCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AfterCreate;
  FDate := Date;
  FMonthOffset := 0;
  UpdateCalendar;
end;

procedure TfpgPopupCalendar.AfterCreate;
begin
  {@VFD_BODY_BEGIN: fpgPopupCalendar}
  Name := 'fpgPopupCalendar';
  SetPosition(100, 268, 233, 179);
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
  end;

  btnYearUp := TfpgButton.Create(self);
  with btnYearUp do
  begin
    Name := 'btnYearUp';
    SetPosition(72, 0, 13, 11);
    Text := '';
    FontDesc := '#Label1';
    ImageMargin := 0;
    ImageName := 'sys.sb.up';
    Embedded := True;
    OnClick := @btnYearUpClicked;
  end;

  btnYearDown := TfpgButton.Create(self);
  with btnYearDown do
  begin
    Name := 'btnYearDown';
    SetPosition(72, 11, 13, 11);
    Text := '';
    FontDesc := '#Label1';
    ImageMargin := 0;
    ImageName := 'sys.sb.down';
    Embedded := True;
    OnClick := @btnYearDownClicked;
  end;

  edtMonth := TfpgEdit.Create(self);
  with edtMonth do
  begin
    Name := 'edtMonth';
    SetPosition(85, 0, 136, 22);
    Text := '';
    FontDesc := '#Edit1';
    Focusable := False;
  end;

  btnMonthUp := TfpgButton.Create(self);
  with btnMonthUp do
  begin
    Name := 'btnMonthUp';
    SetPosition(220, 0, 13, 11);
    Text := '';
    FontDesc := '#Label1';
    ImageMargin := 0;
    ImageName := 'sys.sb.up';
    Embedded := True;
    OnClick := @btnMonthUpClicked;
  end;

  btnMonthDown := TfpgButton.Create(self);
  with btnMonthDown do
  begin
    Name := 'btnMonthDown';
    SetPosition(220, 11, 13, 11);
    Text := '';
    FontDesc := '#Label1';
    ImageMargin := 0;
    ImageName := 'sys.sb.down';
    Embedded := True;
    OnClick := @btnMonthDownClicked;
  end;

  grdName1 := TfpgStringGrid.Create(self);
  with grdName1 do
  begin
    Name := 'grdName1';
    SetPosition(0, 23, 233, 156);
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
    OnDoubleClick:=@grdName1DoubleClick;
  end;

  {@VFD_BODY_END: fpgPopupCalendar}
end;


end.
