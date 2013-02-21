unit u_editgrid;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_basegrid,
  fpg_customgrid,
  fpg_grid,
  fpg_edit,
  fpg_combobox,
  fpg_editcombo,
  fpg_checkbox,
  fpg_popupcalendar;

type

  TEditType = (etNone, etText, etInteger, etFloat, etCurrency, etComboBox, etEditCombo, etCheckBox, etCalendar);
  TEditing =(edRow, edColumn);

  TfpgColumnData = class(TObject)
  private
    FMaxSet: boolean;
    FminSet: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property    MaxSet: boolean read FMaxSet write FMaxSet;
    property    MinSet: boolean read FminSet write FminSet;
  end;

  TfpgNumericColumn = class(TfpgColumnData)
  private
    FMaxLimit: boolean;
    FMinLimit: boolean;
    FDecimals: integer;
    FDecimalseparator: TfpgChar;
    FThousandSeparator: TfpgChar;
    FShowThousand: boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    property    MaxLimit: boolean read FMaxLimit write FMaxLimit;
    property    MinLimit: boolean read FMinLimit write FMinLimit;
    property    Decimals: integer read FDecimals write FDecimals;
    property    DecimalSeparator: TfpgChar read FDecimalSeparator write FDecimalSeparator;
    property    ThousandSeparator: TfpgChar read FThousandSeparator write FThousandSeparator;
    property    ShowThousand: boolean read FShowThousand write FShowThousand;
  end;

  TfpgIntegerColumn = class(TfpgNumericColumn)
  private
    FMaxVal: integer;
    FMinVal: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    property    MaxVal: integer read FMaxVal write FMaxVal;
    property    MinVal: integer read FMinVal write FMinVal;
  end;

  TfpgFloatColumn = class(TfpgNumericColumn)
  private
    FMaxVal: extended;
    FMinVal: extended;
    FFixedDecimals: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    property    MaxVal: extended read FMaxVal write FMaxVal;
    property    MinVal: extended read FMinVal write FMinVal;
    property    FixedDecimals: integer read FFixedDecimals write FFixedDecimals;
  end;

  TfpgCurrencyColumn = class(TfpgNumericColumn)
  private
    FMaxVal: currency;
    FMinVal: currency;
  public
    constructor Create;
    destructor  Destroy; override;
    property    MaxVal: currency read FMaxVal write FMaxVal;
    property    MinVal: currency read FMinVal write FMinVal;
  end;

  TfpgComboBoxColumn = class(TfpgColumnData)
  private
    FItems: TStringList;
  public
    constructor Create;
    destructor  Destroy; override;
    property    Items: TStringList read FItems write FItems;
  end;

  TfpgEditComboColumn = class(TfpgColumnData)
  private
    FItems: TStringList;
    FAutoComplete: boolean;
    FAllowNew: TAllowNew;
  public
    constructor Create;
    destructor  Destroy; override;
    property    Items: TStringList read FItems write FItems;
    property    AutoComplete: boolean read FAutoComplete write FAutoComplete;
    property    AllowNew: TAllowNew read FAllowNew write FAllowNew;
  end;

  TfpgCheckBoxColumn = class(TfpgColumnData)
  private
    FChecked: string;
    FUnchecked: string;
    FBoxText: string;
  public
    constructor Create;
    destructor  Destroy; override;
    property    CheckedText: string read FChecked write FChecked;
    property    UncheckedText: string read FUnchecked write FUnchecked;
    property    BoxText: string read FBoxText write FBoxText;
  end;

  TDates = class
    FDate: TDateTime;
  end;

  TfpgCalendarColumn = class(TfpgColumnData)
  private
    FDatesList: TList;
    FGridDateFormat: string;
    FCalendarDateFormat: string;
    FDateValue: TDateTime;
    FMaxDate: TDateTime;
    FMinDate: TDateTime;
    FWeeklyHoliday: integer;
    FWeekStartDay: integer;
    FDayColor: TfpgColor;
    FHolidayColor: TfpgColor;
    FSingleClickSelect: boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    property    DatesList: TList read FDatesList write FDatesList;
    property    GridDateFormat: string read FGridDateFormat write FGridDateFormat;
    property    CalendarDateFormat: string read FCalendarDateFormat write FCalendarDateFormat;
    property    DateValue: TDateTime read FDateValue write FDateValue;
    property    MaxDate: TDateTime read FMaxDate write FMaxDate;
    property    MinDate: TDateTime read FMinDate write FMinDate;
    property    WeeklyHoliday: integer read FWeeklyHoliday write FWeeklyHoliday;
    property    WeekStartDay: integer read FWeekStartDay write FWeekStartDay;
    property    DayColor: TfpgColor read FDayColor write FDayColor;
    property    HolidayColor: TfpgColor read FHolidayColor write FHolidayColor;
    property    SingleClickSelect: boolean read FSingleClickSelect write FSingleClickSelect;
  end;

  TfpgEditColumn = class(TfpgStringColumn)
  private
    FEditType: TEditType;
    FData: TfpgColumnData;
  public
    property    EditType: TEditType read FEditType write FEditType;
    property    Data: TfpgColumnData read FData write FData;
  end;

  TfpgCustomEditgrid = class(TfpgCustomStringGrid)
  private
    FDates: TDates;
    FCellEditText: TFpgEdit;
    FCellEditInteger: TFpgEditInteger;
    FCellEditFloat: TFpgEditFloat;
    FCellEditCurrency: TFpgEditCurrency;
    FCellComboBox: TfpgComboBox;
    FCellEditCombo: TfpgEditCombo;
    FCellCheckBox: TfpgCheckBox;
    FCellCalendar: TfpgCalendarCombo;
//    FRefGrid: TfpgStringGrid;   // reference uniquement
    FFocusRect: TfpgRect;
    FEditing: boolean;
    FEditWay: TEditing;
    procedure   EditGridFocusChange(Sender: TObject; ARow,ACol: integer);
    procedure   EditGridDoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState;
        const AMousePos: TPoint);
    function    GetColumnEditType(AIndex: integer): TEditType;
    procedure   SetEditCell;
    procedure   IniTextCell;
    procedure   FCellEditTextKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
        var Consumed: boolean);
    procedure   FCellEditTextExit(Sender: TObject);
    function    GetNumericMaxLimit(AIndex: integer): boolean;
    procedure   SetNumericMaxLimit(AIndex: integer; const AValue: boolean);
    function    GetNumericMinLimit(AIndex: integer): boolean;
    procedure   SetNumericMinLimit(AIndex: integer; const AValue: boolean);
    function    GetNumericDecimals(AIndex: integer): integer;
    procedure   SetNumericDecimals(AIndex: integer; const AValue: integer);
    function    GetNumericDecimalSeparator(AIndex: integer): TfpgChar;
    procedure   SetNumericDecimalSeparator(AIndex: integer; const AValue: TfpgChar);
    function    GetNumericThousandSeparator(AIndex: integer): TfpgChar;
    procedure   SetNumericThousandSeparator(AIndex: integer; const AValue: TfpgChar);
    function    GetNumericShowThousand(AIndex: integer): boolean;
    procedure   SetNumericShowThousand(AIndex: integer; const AValue: boolean);
    procedure   IniIntegerCell;
    procedure   FCellEditIntegerKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
        var Consumed: boolean);
    procedure   FCellEditIntegerExit(Sender: TObject);
    function    GetMaxIntValue(AIndex: integer): integer;
    procedure   SetMaxIntValue(AIndex: integer; const AValue: integer);
    function    GetMinIntValue(AIndex: integer): integer;
    procedure   SetMinIntValue(AIndex: integer; const AValue: integer);
    procedure   IniFloatCell;
    procedure   FCellEditFloatKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
        var Consumed: boolean);
    procedure   FCellEditFloatExit(Sender: TObject);
    function    GetMaxFloatValue(AIndex: integer): extended;
    procedure   SetMaxFloatValue(AIndex: integer; const AValue: extended);
    function    GetMinFloatValue(AIndex: integer): extended;
    procedure   SetMinFloatValue(AIndex: integer; const AValue: extended);
    function    GetFloatFixedDecimals(AIndex: integer): integer;
    procedure   SetFloatFixedDecimals(AIndex: integer; const AValue: integer);
    procedure   IniCurrencyCell;
    procedure   FCellEditCurrencyKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
        var Consumed: boolean);
    procedure   FCellEditCurrencyExit(Sender: TObject);
    function    GetMaxCurrValue(AIndex: integer): currency;
    procedure   SetMaxCurrValue(AIndex: integer; const AValue: currency);
    function    GetMinCurrValue(AIndex: integer): currency;
    procedure   SetMinCurrValue(AIndex: integer; const AValue: currency);
    procedure   IniComboBoxCell;
    procedure   FCellComboBoxKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
        var Consumed: boolean);
    procedure   FCellComboBoxExit(Sender: TObject);
    procedure   IniEditComboCell;
    procedure   FCellEditComboKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
        var Consumed: boolean);
    procedure   FCellEditComboExit(Sender: TObject);
    function    GetAutoComplete(AIndex: integer): boolean;
    procedure   SetAutoComplete(AIndex: integer; const AValue: boolean);
    function    GetAllowNew(AIndex: integer): TAllowNew;
    procedure   SetAllowNew(AIndex: integer; AValue: TAllowNew);
    procedure   IniCheckBoxCell;
    procedure   FCellCheckBoxKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
        var Consumed: boolean);
    procedure   FCellCheckBoxExit(Sender: TObject);
    function    GetBoxCheckedText(AIndex: integer): string;
    procedure   SetBoxCheckedText(AIndex: integer; const AValue: string);
    function    GetBoxUncheckedText(AIndex: integer): string;
    procedure   SetBoxUncheckedText(AIndex: integer; const AValue: string);
    function    GetBoxDisplayText(AIndex: integer): string;
    procedure   SetBoxDisplayText(AIndex: integer; const AValue: string);
    procedure   IniCalendarCell;
    procedure   FCellCalendarKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
        var Consumed: boolean);
    procedure   FCellCalendarExit(Sender: TObject);
    function    GetDates(AIndex: integer): TDateTime;
    procedure   SetDates(AIndex: integer; const AValue: TDateTime);
    function    GetDatesList(AIndex: integer): TList;
    procedure   SetDatesList(AIndex: integer; const AValue: TList);
    function    GetGridDateFormat(AIndex: integer): string;
    procedure   SetGridDateFormat(AIndex: integer; const AValue: string);
    function    GetCalendarDateFormat(AIndex: integer): string;
    procedure   SetCalendarDateFormat(AIndex: integer; const AValue: string);
    function    GetDateValue(AIndex: integer): TDateTime;
    procedure   SetDateValue(AIndex: integer; const AValue: TDateTime);
    function    GetMaxDate(AIndex: integer): TDateTime;
    procedure   SetMaxdate(AIndex: integer; const AValue: TDateTime);
    function    GetMinDate(AIndex: integer): TDateTime;
    procedure   SetMinDate(AIndex: integer; const AValue: TDateTime);
    function    GetWeeklyHoliday(AIndex: integer): integer;
    procedure   SetWeeklyHoliday(AIndex: integer; const AValue: integer);
    function    GetWeekStartDay(AIndex: integer): integer;
    procedure   SetWeekStartDay(AIndex: integer; const AValue: integer);
    function    GetDayColor(AIndex: integer): TfpgColor;
    procedure   SetDayColor(AIndex: integer; const AValue: TfpgColor);
    function    GetHolidayColor(AIndex: integer): TfpgColor;
    procedure   SetHolidayColor(AIndex: integer; const AValue: TfpgColor);
    function    GetSingleClickSelect(AIndex: integer): boolean;
    procedure   SetSingleClickSelect(AIndex: integer; const AValue: boolean);
  protected
    function    GetColumns(AIndex: Integer): TfpgEditColumn; reintroduce;
    procedure   DrawCell(ARow, ACol: Integer; ARect: TfpgRect; AFlags: TfpgGridDrawState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    property    Columns[AIndex: Integer]: TfpgEditColumn read GetColumns;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    EditWay: TEditing read FEditWay write FEditWay;
    function    AddColumn(ATitle: string; AWidth: integer; AEditType: TEditType = etNone;
        AAlignment: TAlignment = taLeftJustify; AbackgroundColor: TfpgColor = clDefault; ATextColor: TfpgColor = clDefault): TfpgEditColumn; overload;
    property    ColumnEditType[AIndex: integer]: TEditType read GetColumnEditType;
    property    NumericMaxLimit[AIndex: integer]: boolean read GetNumericMaxLimit write SetNumericMaxLimit;
    property    NumericMinLimit[AIndex: integer]: boolean read GetNumericMinLimit write SetNumericMinLimit;
    property    NumericDecimals[AIndex: integer]: integer read GetNumericDecimals write SetNumericDecimals;
    property    NumericDecimalSeparator[AIndex: integer]: TfpgChar read GetNumericDecimalSeparator write SetNumericDecimalSeparator;
    property    NumericThousandSeparator[AIndex: integer]: TfpgChar read GetNumericThousandSeparator write SetNumericThousandSeparator;
    property    NumericShowThousand[AIndex: integer]: boolean read GetNumericShowThousand write SetNumericShowThousand;
    property    MaxIntValue[AIndex: integer]: integer read GetMaxIntValue write SetMaxIntValue;
    property    MinIntValue[AIndex: integer]: integer read GetMinIntValue write SetMinIntValue;
    property    MaxFloatValue[AIndex: integer]: extended read GetMaxFloatValue write SetMaxFloatValue;
    property    MinFloatValue[AIndex: integer]: extended read GetMinFloatValue write SetMinFloatValue;
    property    FloatFixedDecimals[AIndex: integer]: integer read GetFloatFixedDecimals write SetFloatFixedDecimals;
    property    MaxCurrValue[AIndex: integer]: currency read GetMaxCurrValue write SetMaxCurrValue;
    property    MinCurrValue[AIndex: integer]: currency read GetMinCurrValue write SetMinCurrValue;
    procedure   AddComboItem(AIndex: integer; const AValue: string);
    procedure   AddEditComboItem(AIndex: integer; const AValue: string);
    property    AutoComplete[AIndex: integer]: boolean read GetAutoComplete write SetAutoComplete;
    property    AllowNew[AIndex: integer]: TAllowNew read GetAllowNew write SetAllowNew;
    property    BoxCheckedText[AIndex: integer]: string read GetBoxCheckedText write SetBoxCheckedText;
    property    BoxUncheckedText[AIndex: integer]: string read GetBoxUncheckedText write SetBoxUncheckedText;
    property    BoxDisplayText[AIndex: integer]: string read GetBoxDisplayText write SetBoxDisplayText;
    property    Dates[AIndex: integer]: TDateTime read GetDates write SetDates;
    property    DatesList[AIndex: integer]: TList read GetDatesList write SetDatesList;
    property    GridDateFormat[AIndex: integer]: string read GetGridDateFormat write SetGridDateFormat;
    property    CalendarDateFormat[AIndex: integer]: string read GetCalendarDateFormat write SetCalendarDateFormat;
    property    DateValue[AIndex: integer]: TDateTime read GetDateValue write SetDatevalue;
    property    MaxDate[AIndex: integer]: TDateTime read GetMaxDate write SetMaxDate;
    property    MinDate[AIndex: integer]: TDateTime read GetMinDate write SetMinDate;
    property    WeeklyHoliday[AIndex: integer]: integer read GetWeeklyHoliday write SetWeeklyHoliday;
    property    WeekStartDay[AIndex: integer]: integer read GetWeekStartDay write SetWeekStartDay;
    property    DayColor[AIndex: integer]: TfpgColor read GetDayColor write SetDayColor;
    property    HolidayColor[AIndex: integer]: TfpgColor read GetHolidayColor write SetHolidayColor;
    property    SingleClickSelect[AIndex: integer]: boolean read GetSingleClickSelect write SetSingleClickSelect;
   end;

  TfpgEditGrid = class(TfpgCustomEditgrid)
  public
    property    Font;
  published
    property    Align;
    property    AlternateBGColor;
    property    BackgroundColor;
    property    BorderStyle;
//    property    ColResizing;
    property    ColumnCount;
    property    Columns;
    property    ColumnWidth;
    property    DefaultColWidth;
    property    DefaultRowHeight;
    property    Enabled;
    property    FocusCol;
    property    FocusRow;
    property    FontDesc;
    property    HeaderFontDesc;
    property    HeaderHeight;
    property    HeaderStyle;
    property    Hint;
    property    Options;
    property    ParentShowHint;
    property    PopupMenu;
    property    RowCount;
    property    RowSelect;
    property    ScrollBarStyle;
    property    ShowGrid;
    property    ShowHeader;
    property    ShowHint;
    property    TabOrder;
    property    TopRow;
    property    VisibleRows;
    property    OnCanSelectCell;
    property    OnClick;
    property    OnDoubleClick;
    property    OnDrawCell;
    property    OnFocusChange;
    property    OnKeyPress;
    property    OnMouseDown;
    property    OnMouseEnter;
    property    OnMouseExit;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnRowChange;
    property    OnShowHint;
  end;

function CreateEditGrid(AOwner: TComponent; x, y, w, h: Tfpgcoord; AColumnCount: integer = 0): TfpgEditGrid;

implementation

uses
  fpg_stringutils;

function CreateEditGrid(AOwner: TComponent; x, y, w, h: TfpgCoord; AColumnCount: integer = 0): TfpgEditGrid;
begin
  Result  := TfpgEditGrid.Create(AOwner);
  Result.Left         := x;
  Result.Top          := y;
  Result.Width        := w;
  Result.Height       := h;
  Result.ColumnCount  := AColumnCount;
end;

constructor TfpgColumnData.Create;
begin
  inherited Create;
  FMaxSet := False;
  FMinSet := False;
end;

destructor TfpgColumnData.Destroy;
begin
  inherited Destroy;
end;

constructor TfpgNumericColumn.Create;
begin
  inherited Create;
  FMaxLimit := False;
  FMinLimit := False;
  FDecimals := -1;
  FDecimalseparator := '.';
  FThousandSeparator := ' ';
  FShowThousand := True;
end;

destructor TfpgNumericColumn.Destroy;
begin
  inherited Destroy;
end;

constructor TfpgIntegerColumn.Create;
begin
  inherited Create;
  FMaxVal := 0;
  FMinVal := 0;
end;

destructor TfpgIntegerColumn.Destroy;
begin
  inherited Destroy;
end;

constructor TfpgFloatColumn.Create;
begin
  inherited Create;
  FMaxVal := 0;
  FMinVal := 0;
  FFixedDecimals := -1;
end;

destructor TfpgFloatColumn.Destroy;
begin
  inherited Destroy;
end;

constructor TfpgCurrencyColumn.Create;
begin
  inherited Create;
  FMaxVal := 0;
  FMinVal := 0;
end;

destructor TfpgCurrencyColumn.Destroy;
begin
  inherited Destroy;
end;

constructor TfpgComboBoxColumn.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
end;

destructor TfpgComboBoxColumn.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

constructor TfpgEditComboColumn.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
end;

destructor TfpgEditComboColumn.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

constructor TfpgCheckBoxColumn.Create;
begin
  inherited Create;
end;

destructor TfpgCheckBoxColumn.Destroy;
begin
  inherited Destroy;
end;

constructor TfpgCalendarColumn.Create;
begin
  inherited Create;
  FDatesList:= TList.Create;
end;

destructor TfpgCalendarColumn.Destroy;
var
  i: integer;
begin
  inherited Destroy;
  if FDatesList.Count> 0 then
    for i := 0 to Pred(FDatesList.Count) do
      TDates(FDatesList[i]).Free;
  FDatesList.Free;
end;

procedure TfpgCustomEditGrid.EditGridFocusChange(Sender: TObject; ARow,ACol: integer);
var
  i: integer;
begin
  for i := 0 to Pred(ColumnCount) do
    case Columns[i].EditType of
      etText:
        if Assigned(FCellEditText) then
          FCellEditText.Visible:= False;
      etInteger:
        if Assigned(FCellEditInteger) then
          FcellEditInteger.Visible:= False;
      etFloat:
        if Assigned(FCellEditFloat) then
          FcellEditFloat.Visible:= False;
      etCurrency:
        if Assigned(FCellEditCurrency) then
          FcellEditCurrency.Visible:= False;
      etComboBox:
        if Assigned(FCellComboBox) then
          FcellComboBox.Visible:= False;
      etEditCombo:
        if Assigned(FCellEditCombo) then
          FcellEditCombo.Visible:= False;
      etCheckBox:
        if Assigned(FCellCheckBox) then
          FCellCheckBox.Visible:= False;
      etCalendar:
        if Assigned(FCellCalendar) then
          FCellCalendar.Visible:= False;
    end;
end;

procedure TfpgCustomEditGrid.EditGridDoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState;
        const AMousePos: TPoint);
var
  lCol, lRow: integer;
begin
  MouseToCell(AMousePos.X, AMousePos.Y, lCol, lRow);
  case Columns[lCol].EditType of
    etText:
      IniTextCell;
    etInteger:
      IniIntegerCell;
    etFloat:
      IniFloatCell;
    etCurrency:
      IniCurrencyCell;
    etComboBox:
      IniComboBoxCell;
    etEditCombo:
      IniEditComboCell;
    etCheckBox:
      IniCheckBoxCell;
    etCalendar:
      IniCalendarCell;
  end;
  FEditing := True;
end;

function TfpgCustomEditGrid.GetColumnEditType(AIndex: integer): TEditType;
begin
  Result := TfpgEditColumn(Columns[AIndex]).EditType;
end;

procedure TfpgCustomEditGrid.SetEditCell;
begin
  case Columns[FocusCol].EditType of
    etText:
      IniTextCell;
    etInteger:
      IniIntegerCell;
    etFloat:
      IniFloatCell;
    etCurrency:
      IniCurrencyCell;
    etComboBox:
      IniComboBoxCell;
    etEditCombo:
      IniEditComboCell;
    etCheckBox:
      IniCheckBoxCell;
    etCalendar:
      IniCalendarCell;
  end;
end;

procedure TfpgCustomEditGrid.IniTextCell;
var
  Pt: TPoint;
begin
  if Assigned(FCellEditText) then
    FCellEditText.Free;
  FCellEditText := TfpgEdit.Create(Self);
  Pt.X := FFocusRect.Left;
  Pt.Y := FFocusRect.Top;
  with FCellEditText do
  begin
    Name := 'FCellEditText';
    SetPosition(Pt.X, Pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    Text := Cells[FocusCol, FocusRow];
    OnKeyPress := @FCellEditTextKeyPress;
    OnExit  := @FCellEditTextExit;
    SetFocus;
  end;
end;

procedure TfpgCustomEditGrid.FCellEditTextKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
          var Consumed: boolean);
begin
  case KeyCode of
    KeyReturn, KeyPEnter:
        begin
        Cells[FocusCol, FocusRow] := FCellEditText.Text;
        FCellEditText.Text := '';
        FCellEditText.Visible := False;
        case FEditWay of
          edColumn:
            if FocusCol < FColumns.Count then
              FocusCol := FocusCol + 1;
          edRow:
            if FocusRow < RowCount then
              FocusRow := FocusRow + 1;
        end;
        SetFocus;
        if FEditing then
          SetEditCell;
      end;
    KeyTab:
      begin
        FCellEditText.Text := '';
        FCellEditText.Visible := False;
        if FEditing then
          FEditing := False;
        if ssShift in ShiftState then
        begin
          if FocusCol > 0 then
            FocusCol := FocusCol - 1;
        end
        else
          if FocusCol < FColumns.Count then
            FocusCol := FocusCol + 1;
        FEditing := False;
        Consumed:= True;
      end;
    KeyEscape:
      begin
      FCellEditText.Text := '';
      FCellEditText.Visible := False;
      if FEditing then
        FEditing := False;
      Consumed:= True;
      end;
  end;
end;

procedure TfpgCustomEditGrid.FCellEditTextExit(Sender: TObject);
begin
  FCellEditText.Visible := False;
end;

function TfpgCustomEditGrid.GetNumericMaxLimit(AIndex: integer): boolean;
begin
  Result := TfpgNumericColumn(TfpgEditColumn(Columns[AIndex]).Data).MaxLimit;
end;

procedure TfpgCustomEditGrid.SetNumericMaxLimit(AIndex: integer; const AValue: boolean);
begin
  TfpgNumericColumn(TfpgEditColumn(Columns[AIndex]).Data).MaxLimit := AValue;
end;

function TfpgCustomEditGrid.GetNumericMinLimit(AIndex: integer): boolean;
begin
  Result := TfpgNumericColumn(TfpgEditColumn(Columns[AIndex]).Data).MinLimit;
end;

procedure TfpgCustomEditGrid.SetNumericMinLimit(AIndex: integer; const AValue: boolean);
begin
  TfpgNumericColumn(TfpgEditColumn(Columns[AIndex]).Data).MinLimit := AValue;
end;

function TfpgCustomEditGrid.GetNumericDecimals(AIndex: integer): integer;
begin
  Result := TfpgNumericColumn(TfpgEditColumn(Columns[AIndex]).Data).Decimals;
end;

procedure TfpgCustomEditGrid.SetNumericDecimals(AIndex: integer; const AValue: integer);
begin
  TfpgNumericColumn(TfpgEditColumn(Columns[AIndex]).Data).Decimals := AValue;
end;

function TfpgCustomEditGrid.GetNumericDecimalSeparator(AIndex: integer): TfpgChar;
begin
  Result := TfpgNumericColumn(TfpgEditColumn(Columns[AIndex]).Data).DecimalSeparator;
end;

procedure TfpgCustomEditGrid.SetNumericDecimalSeparator(AIndex: integer; const AValue: TfpgChar);
begin
  TfpgNumericColumn(TfpgEditColumn(Columns[AIndex]).Data).DecimalSeparator := AValue
end;

function TfpgCustomEditGrid.GetNumericThousandSeparator(AIndex: integer): TfpgChar;
begin
  Result := TfpgNumericColumn(TfpgEditColumn(Columns[AIndex]).Data).ThousandSeparator;
end;

procedure TfpgCustomEditGrid.SetNumericThousandSeparator(AIndex: integer; const AValue: TfpgChar);
begin
  TfpgNumericColumn(TfpgEditColumn(Columns[AIndex]).Data).ThousandSeparator := AValue;
end;

function TfpgCustomEditGrid.GetNumericShowThousand(AIndex: integer): boolean;
begin
  Result := TfpgNumericColumn(TfpgEditColumn(Columns[AIndex]).Data).ShowThousand;
end;

procedure TfpgCustomEditGrid.SetNumericShowThousand(AIndex: integer; const AValue: boolean);
begin
  TfpgNumericColumn(TfpgEditColumn(Columns[AIndex]).Data).ShowThousand := AValue;
end;

procedure TfpgCustomEditGrid.IniIntegerCell;
var
  Pt: TPoint;
begin
  if Assigned(FCellEditInteger) then
    FCellEditInteger.Free;
  FCellEditInteger := TfpgEditInteger.Create(Self);
  Pt.X := FFocusRect.Left;
  Pt.Y := FFocusRect.Top;
  with FCellEditInteger do
  begin
    Name := 'FCellEditInteger';
    SetPosition(Pt.X, Pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    Text := Cells[FocusCol, FocusRow];
    if TfpgColumnData(TfpgEditColumn(Columns[FocusCol]).Data).MaxSet then
      MaxValue := MaxIntValue[Focuscol];
    if TfpgColumnData(TfpgEditColumn(Columns[FocusCol]).Data).MinSet then
      MinValue := MinIntValue[FocusCol];
    CustomThousandSeparator := TfpgNumericColumn(TfpgEditColumn(Columns[FocusCol]).Data).ThousandSeparator;
    ShowThousand := TfpgNumericColumn(TfpgEditColumn(Columns[FocusCol]).Data).ShowThousand;
    OnKeyPress := @FCellEditIntegerKeyPress;
    OnExit  := @FCellEditIntegerExit;
    SetFocus;
  end;
end;

procedure TfpgCustomEditGrid.FCellEditIntegerKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
    var Consumed: boolean);
begin
  case KeyCode of
    KeyReturn, KeyPEnter:
      begin
        Cells[FocusCol, FocusRow] := FCellEditInteger.Text;
        FCellEditInteger.Text:= '';
        FCellEditInteger.Visible := False;
        case FEditWay of
          edColumn:
            if FocusCol < FColumns.Count then
              FocusCol := FocusCol + 1;
          edRow:
            if FocusRow < RowCount then
              FocusRow := FocusRow + 1;
        end;
        SetFocus;
        if FEditing then
          SetEditCell;
      end;
    KeyTab:
      begin
        FCellEditInteger.Text := '';
        FCellEditInteger.Visible := False;
        if FEditing then
          FEditing := False;
        if ssShift in ShiftState then
        begin
          if FocusCol > 0 then
            FocusCol := FocusCol - 1;
        end
        else
          if FocusCol < FColumns.Count then
            FocusCol := FocusCol + 1;
        Consumed:= True;
      end;
    KeyEscape:
      begin
        FCellEditInteger.Text := '';
        FCellEditInteger.Visible := False;
        if FEditing then
          FEditing := False;
        Consumed:= True;
      end;
  end;
end;

procedure TfpgCustomEditGrid.FCellEditIntegerExit(Sender: TObject);
begin
  FCellEditInteger.Visible := False;
end;

function TfpgCustomEditGrid.GetMaxIntValue(AIndex: integer): integer;
begin
  Result := TfpgIntegerColumn(TfpgEditColumn(Columns[AIndex]).Data).MaxVal;
end;

procedure TfpgCustomEditGrid.SetMaxIntValue(AIndex: integer; const AValue: integer);
begin
  with TfpgIntegerColumn(TfpgEditColumn(Columns[AIndex]).Data) do
  begin
    if AValue > MinVal then
      MaxVal := AValue
    else
      MaxVal := MinVal;
    MaxSet:= True;
  end;
end;

function TfpgCustomEditGrid.GetMinIntValue(AIndex: integer): integer;
begin
  Result := TfpgIntegerColumn(TfpgEditColumn(Columns[AIndex]).Data).MinVal;
end;

procedure TfpgCustomEditGrid.SetMinIntValue(AIndex: integer; const AValue: integer);
begin
  with TfpgIntegerColumn(TfpgEditColumn(Columns[AIndex]).Data) do
  begin
    if AValue < MaxVal then
      MinVal := AValue
    else
      MinVal := Maxval;
    MinSet := True;
  end;
end;

procedure TfpgCustomEditGrid.IniFloatCell;
var
  Pt: TPoint;
begin
  if Assigned(FCellEditFloat) then
    FCellEditFloat.Free;
  FCellEditFloat := TfpgEditFloat.Create(Self);
  Pt.X := FFocusRect.Left;
  Pt.Y := FFocusRect.Top;
  with FCellEditFloat do
  begin
    Name := 'FCellEditFloat';
    SetPosition(Pt.X, Pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    Text := Cells[FocusCol, FocusRow];
    if TfpgColumnData(TfpgEditColumn(Columns[FocusCol]).Data).MaxSet then
      MaxValue := MaxFloatValue[Focuscol];
    if TfpgColumnData(TfpgEditColumn(Columns[FocusCol]).Data).MinSet then
      MinValue := MinFloatValue[FocusCol];
    Decimals := TfpgNumericColumn(TfpgEditColumn(Columns[FocusCol]).Data).Decimals;
    FixedDecimals := TfpgFloatColumn(TfpgEditColumn(Columns[FocusCol]).Data).FixedDecimals;
    CustomDecimalSeparator := TfpgNumericColumn(TfpgEditColumn(Columns[FocusCol]).Data).DecimalSeparator;
    CustomThousandSeparator := TfpgNumericColumn(TfpgEditColumn(Columns[FocusCol]).Data).ThousandSeparator;
    ShowThousand := TfpgNumericColumn(TfpgEditColumn(Columns[FocusCol]).Data).ShowThousand;
    OnKeyPress := @FCellEditFloatKeyPress;
    OnExit  := @FCellEditFloatExit;
    SetFocus;
  end;
end;

procedure TfpgCustomEditGrid.FCellEditFloatKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
    var Consumed: boolean);
begin
  case KeyCode of
    KeyReturn, KeyPEnter:
      begin
        with FCellEditFloat do
        begin
          if FixedDecimals > -1 then
          begin
            if UTF8Pos(CustomDecimalSeparator, Text) > 0 then
            begin
              if (UTF8Length(Text) - UTF8Pos(CustomDecimalSeparator, Text)) > FixedDecimals then
                Text := Copy(Text, 1, UTF8Pos(CustomDecimalSeparator, Text) + FixedDecimals);
            end
            else
            begin
              if UTF8Pos(CustomDecimalSeparator, Text) = 0 then
                Text := Text + CustomDecimalSeparator;
              while (UTF8Length(Text) - (UTF8Pos(CustomDecimalSeparator, Text)) < FixedDecimals) do
                Text := Text +'0';
            end;
          end;
          if Decimals > -1 then
            if UTF8Pos(CustomDecimalSeparator, Text) > 0 then
              if (UTF8Length(Text) - UTF8Pos(CustomDecimalSeparator, Text)) > Decimals then
                Text := Copy(Text, 1, UTF8Pos(CustomDecimalSeparator, Text) + Decimals);
        end;
        Cells[FocusCol, FocusRow] := FCellEditFloat.Text;
        FCellEditFloat.Text:= '';
        FCellEditFloat.Visible := False;
        case FEditWay of
          edColumn:
            if FocusCol < FColumns.Count then
              FocusCol := FocusCol + 1;
          edRow:
            if FocusRow < RowCount then
              FocusRow := FocusRow + 1;
        end;
        SetFocus;
        if FEditing then
          SetEditCell;
      end;
    KeyTab:
      begin
        FCellEditFloat.Text := '';
        FCellEditFloat.Visible := False;
        if FEditing then
          FEditing := False;
        if ssShift in ShiftState then
        begin
          if FocusCol > 0 then
            FocusCol := FocusCol - 1;
        end
        else
          if FocusCol < FColumns.Count then
            FocusCol := FocusCol + 1;
        Consumed:= True;
      end;
    KeyEscape:
      begin
        FCellEditFloat.Text := '';
        FCellEditFloat.Visible := False;
        if FEditing then
          FEditing := False;
        Consumed:= True;
      end;
  end;
end;

procedure TfpgCustomEditGrid.FCellEditFloatExit(Sender: TObject);
begin
  FCellEditFloat.Visible := False;
end;

function TfpgCustomEditGrid.GetMaxFloatValue(AIndex: integer): extended;
begin
  Result := TfpgFloatColumn(TfpgEditColumn(Columns[AIndex]).Data).MaxVal;
end;

procedure TfpgCustomEditGrid.SetMaxFloatValue(AIndex: integer; const AValue: extended);
begin
  with TfpgFloatColumn(TfpgEditColumn(Columns[AIndex]).Data) do
  begin
    if AValue > MinVal then
      MaxVal := AValue
    else
      MaxVal := MinVal;
    MaxSet:= True;
  end;
end;

function TfpgCustomEditGrid.GetMinFloatValue(AIndex: integer): extended;
begin
  Result := TfpgFloatColumn(TfpgEditColumn(Columns[AIndex]).Data).MinVal;
end;

procedure TfpgCustomEditGrid.SetMinFloatValue(AIndex: integer; const AValue: extended);
begin
  with TfpgFloatColumn(TfpgEditColumn(Columns[AIndex]).Data) do
  begin
    if AValue < MaxVal then
      MinVal := AValue
    else
      MinVal := Maxval;
    MinSet := True;
  end;
end;

function TfpgCustomEditGrid.GetFloatFixedDecimals(AIndex: integer): integer;
begin
  Result := TfpgFloatColumn(TfpgEditColumn(Columns[AIndex]).Data).FFixedDecimals;
end;

procedure TfpgCustomEditGrid.SetFloatFixedDecimals(AIndex: integer; const AValue: integer);
begin
  TfpgFloatColumn(TfpgEditColumn(Columns[AIndex]).Data).FFixedDecimals := AValue;
end;

procedure TfpgCustomEditGrid.IniCurrencyCell;
var
  Pt: TPoint;
begin
  if Assigned(FCellEditCurrency) then
    FCellEditCurrency.Free;
  FCellEditCurrency := TfpgEditCurrency.Create(Self);
  Pt.X := FFocusRect.Left;
  Pt.Y := FFocusRect.Top;
  with FCellEditCurrency do
  begin
    Name := 'FCellEditCurrency';
    SetPosition(Pt.X, Pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    Text := Cells[FocusCol, FocusRow];
    if TfpgColumnData(TfpgEditColumn(Columns[FocusCol]).Data).MaxSet then
      MaxValue := MaxCurrValue[Focuscol];
    if TfpgColumnData(TfpgEditColumn(Columns[FocusCol]).Data).MinSet then
      MinValue := MinCurrValue[FocusCol];
    CustomDecimalSeparator := TfpgNumericColumn(TfpgEditColumn(Columns[FocusCol]).Data).DecimalSeparator;
    CustomThousandSeparator := TfpgNumericColumn(TfpgEditColumn(Columns[FocusCol]).Data).ThousandSeparator;
    ShowThousand := TfpgNumericColumn(TfpgEditColumn(Columns[FocusCol]).Data).ShowThousand;
    OnKeyPress := @FCellEditCurrencyKeyPress;
    OnExit  := @FCellEditCurrencyExit;
    SetFocus;
  end;
end;

procedure TfpgCustomEditGrid.FCellEditCurrencyKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
    var Consumed: boolean);
begin
  case KeyCode of
    KeyReturn, KeyPEnter:
      begin
        Cells[FocusCol, FocusRow] := FCellEditCurrency.Text;
        FCellEditCurrency.Text:= '';
        FCellEditCurrency.Visible := False;
        case FEditWay of
          edColumn:
            if FocusCol < FColumns.Count then
              FocusCol := FocusCol + 1;
          edRow:
            if FocusRow < RowCount then
              FocusRow := FocusRow + 1;
        end;
        SetFocus;
        if FEditing then
          SetEditCell;
      end;
    KeyTab:
      begin
        FCellEditCurrency.Text := '';
        FCellEditCurrency.Visible := False;
        if FEditing then
          FEditing := False;
        if ssShift in ShiftState then
        begin
          if FocusCol > 0 then
            FocusCol := FocusCol - 1;
        end
        else
          if FocusCol < FColumns.Count then
            FocusCol := FocusCol + 1;
        Consumed:= True;
      end;
    KeyEscape:
      begin
        FCellEditCurrency.Text := '';
        FCellEditCurrency.Visible := False;
        if FEditing then
          FEditing := False;
        Consumed:= True;
      end;
  end;
end;

procedure TfpgCustomEditGrid.FCellEditCurrencyExit(Sender: TObject);
begin
  FCellEditFloat.Visible := False;
end;

function TfpgCustomEditGrid.GetMaxCurrValue(AIndex: integer): currency;
begin
  Result := TfpgCurrencyColumn(TfpgEditColumn(Columns[AIndex]).Data).MaxVal;
end;

procedure TfpgCustomEditGrid.SetMaxCurrValue(AIndex: integer; const AValue: currency);
begin
  with TfpgCurrencyColumn(TfpgEditColumn(Columns[AIndex]).Data) do
  begin
    if AValue > MinVal then
      MaxVal := AValue
    else
      MaxVal := MinVal;
    MaxSet:= True;
  end;
end;

function TfpgCustomEditGrid.GetMinCurrValue(AIndex: integer): currency;
begin
  Result := TfpgCurrencyColumn(TfpgEditColumn(Columns[AIndex]).Data).MinVal;
end;

procedure TfpgCustomEditGrid.SetMinCurrValue(AIndex: integer; const AValue: currency);
begin
  with TfpgCurrencyColumn(TfpgEditColumn(Columns[AIndex]).Data) do
  begin
    if AValue < MaxVal then
      MinVal := AValue
    else
      MinVal := Maxval;
    MinSet := True;
  end;
end;

procedure TfpgCustomEditGrid.IniComboBoxCell;
var
  Pt: TPoint;
  i: integer;
begin
  if Assigned(FCellComboBox) then
    FCellComboBox.Free;
  FCellComboBox := TfpgComboBox.Create(Self);
  Pt.X := FFocusRect.Left;
  Pt.Y := FFocusRect.Top;
  with FCellComboBox do
  begin
    Name := 'FCellComboBox';
    SetPosition(Pt.X, Pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    Items.Assign(TfpgComboBoxColumn(TfpgEditColumn(Columns[FocusCol]).Data).FItems);
    for i := 0 to Pred(Items.Count) do
      if Items[i] = Cells[FocusCol, FocusRow] then
        Text := Items[i];
    OnKeyPress := @FCellComboBoxKeyPress;
    OnExit  := @FCellComboBoxExit;
    SetFocus;
  end;
end;

procedure TfpgCustomEditGrid.FCellComboBoxKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
    var Consumed: boolean);
begin
  case KeyCode of
    KeyReturn, KeyPEnter:
      begin
        Cells[FocusCol, FocusRow] := FCellComboBox.Text;
        FCellComboBox.Text:= '';
        FCellComboBox.Visible := False;
        case FEditWay of
          edColumn:
            if FocusCol < FColumns.Count then
              FocusCol := FocusCol + 1;
          edRow:
            if FocusRow < RowCount then
              FocusRow := FocusRow + 1;
        end;
        SetFocus;
        if FEditing then
          SetEditCell;
      end;
    KeyTab:
      begin
        FCellComboBox.Text := '';
        FCellComboBox.Visible := False;
        if FEditing then
          FEditing := False;
        if ssShift in ShiftState then
        begin
          if FocusCol > 0 then
            FocusCol := FocusCol - 1;
        end
        else
          if FocusCol < FColumns.Count then
            FocusCol := FocusCol + 1;
        Consumed:= True;
      end;
    KeyEscape:
      begin
        FCellComboBox.Text := '';
        FCellComboBox.Visible := False;
        if FEditing then
          FEditing := False;
        Consumed:= True;
      end;
  end;
end;

procedure TfpgCustomEditGrid.FCellComboBoxExit(Sender: TObject);
begin
  FCellComboBox.Visible:= False;
end;

procedure TfpgCustomEditGrid.IniEditComboCell;
var
  Pt: TPoint;
  i: integer;
begin
  if Assigned(FCellEditCombo) then
    FCellEditCombo.Free;
  FCellEditCombo := TfpgEditCombo.Create(Self);
  Pt.X := FFocusRect.Left;
  Pt.Y := FFocusRect.Top;
  with FCellEditCombo do
  begin
    Name := 'FCellEditCombo';
    SetPosition(Pt.X, Pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    Items.Assign(TfpgEditComboColumn(TfpgEditColumn(Columns[FocusCol]).Data).FItems);
    for i := 0 to Pred(Items.Count) do
      if Items[i] = Cells[FocusCol, FocusRow] then
        Text := Items[i];
    AutoCompletion := TfpgEditComboColumn(TfpgEditColumn(Columns[FocusCol]).Data).AutoComplete;
    AllowNew := TfpgEditComboColumn(TfpgEditColumn(Columns[FocusCol]).Data).AllowNew;
    OnKeyPress := @FCellEditComboKeyPress;
    OnExit  := @FCellEditComboExit;
    SetFocus;
  end;
end;

procedure TfpgCustomEditGrid.FCellEditComboKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
    var Consumed: boolean);
begin
  case KeyCode of
    KeyReturn, KeyPEnter:
      begin
        Cells[FocusCol, FocusRow] := FCellEditCombo.Text;
        if (AllowNew[FocusCol] = anAsk) or (AllowNew[FocusCol] = anYes) then
          TfpgEditComboColumn(TfpgEditColumn(Columns[FocusCol]).Data).FItems.Add(FCellEditCombo.Text);
        FCellEditCombo.Text:= '';
        FCellEditCombo.Visible := False;
        case FEditWay of
          edColumn:
            if FocusCol < FColumns.Count then
              FocusCol := FocusCol + 1;
          edRow:
            if FocusRow < RowCount then
              FocusRow := FocusRow + 1;
        end;
        SetFocus;
        if FEditing then
          SetEditCell;
      end;
    KeyTab:
      begin
        FCellEditCombo.Text := '';
        FCellEditCombo.Visible := False;
        if FEditing then
          FEditing := False;
        if ssShift in ShiftState then
        begin
          if FocusCol > 0 then
            FocusCol := FocusCol - 1;
        end
        else
          if FocusCol < FColumns.Count then
            FocusCol := FocusCol + 1;
        Consumed:= True;
      end;
    KeyEscape:
      begin
        FCellEditCombo.Text := '';
        FCellEditCombo.Visible := False;
        if FEditing then
          FEditing := False;
        Consumed:= True;
      end;
  end;
end;

procedure TfpgCustomEditGrid.FCellEditComboExit(Sender: TObject);
begin
  FCellEditCombo.Visible:= False;
end;

function TfpgCustomEditGrid.GetAutoComplete(AIndex: integer): boolean;
begin
  Result := TfpgEditComboColumn(TfpgEditColumn(Columns[AIndex]).Data).AutoComplete;
end;

procedure TfpgCustomEditGrid.SetAutoComplete(AIndex: integer; const AValue: boolean);
begin
  TfpgEditComboColumn(TfpgEditColumn(Columns[AIndex]).Data).AutoComplete := AValue;
end;

function TfpgCustomEditGrid.GetAllowNew(AIndex: integer): TAllowNew;
begin
  Result := TfpgEditComboColumn(TfpgEditColumn(Columns[AIndex]).Data).AllowNew;
end;

procedure TfpgCustomEditGrid.SetAllowNew(AIndex: integer; AValue: TAllowNew);
begin
  TfpgEditComboColumn(TfpgEditColumn(Columns[AIndex]).Data).AllowNew := AValue;
end;

procedure TfpgCustomEditGrid.IniCheckBoxCell;
var
  Pt: TPoint;
begin
  if Assigned(FCellCheckBox) then
    FCellCheckBox.Free;
  FCellCheckBox := TfpgCheckBox.Create(Self);
  Pt.X := FFocusRect.Left;
  Pt.Y := FFocusRect.Top;
  with FCellCheckBox do
  begin
    Name := 'FCellCheckBox';
    SetPosition(Pt.X, Pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    if Cells[FocusCol, FocusRow] = TfpgCheckBoxColumn(TfpgEditColumn(Columns[FocusCol]).Data).CheckedText then
      FCellCheckBox.Checked:= True
    else
      FCellCheckBox.Checked:= False;
    Text := TfpgCheckBoxColumn(TfpgEditColumn(Columns[FocusCol]).Data).BoxText;
    OnKeyPress := @FCellCheckBoxKeyPress;
    OnExit  := @FCellCheckBoxExit;
    SetFocus;
  end;
end;

procedure TfpgCustomEditGrid.FCellCheckBoxKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
    var Consumed: boolean);
begin
  case KeyCode of
    KeyReturn, KeyPEnter:
      begin
        if FCellCheckBox.Checked then
          Cells[FocusCol, FocusRow] := TfpgCheckBoxColumn(TfpgEditColumn(Columns[FocusCol]).Data).CheckedText
        else
          Cells[FocusCol, FocusRow] := TfpgCheckBoxColumn(TfpgEditColumn(Columns[FocusCol]).Data).UncheckedText;
        FCellCheckBox.Text:= '';
        FCellCheckBox.Visible := False;
        case FEditWay of
          edColumn:
            if FocusCol < FColumns.Count then
              FocusCol := FocusCol + 1;
          edRow:
            if FocusRow < RowCount then
              FocusRow := FocusRow + 1;
        end;
        SetFocus;
        if FEditing then
          SetEditCell;
      end;
    KeyTab:
      begin
        FCellCheckBox.Text := '';
        FCellCheckBox.Visible := False;
        if FEditing then
          FEditing := False;
        if ssShift in ShiftState then
        begin
          if FocusCol > 0 then
            FocusCol := FocusCol - 1;
        end
        else
          if FocusCol < FColumns.Count then
            FocusCol := FocusCol + 1;
        Consumed:= True;
      end;
    KeyEscape:
      begin
        FCellCheckBox.Text := '';
        FCellCheckBox.Visible := False;
        if FEditing then
          FEditing := False;
        Consumed:= True;
      end;
  end;
end;

procedure TfpgCustomEditGrid.FCellCheckBoxExit(Sender: TObject);
begin
  FCellCheckBox.Visible:= False;
end;

function TfpgCustomEditGrid.GetBoxCheckedText(AIndex: integer): string;
begin
  Result := TfpgCheckBoxColumn(TfpgEditColumn(Columns[AIndex]).Data).CheckedText;
end;

procedure TfpgCustomEditGrid.SetBoxCheckedText(AIndex: integer; const AValue: string);
begin
  TfpgCheckBoxColumn(TfpgEditColumn(Columns[AIndex]).Data).CheckedText := AValue;
end;

function TfpgCustomEditGrid.GetBoxUncheckedText(AIndex: integer): string;
begin
  Result := TfpgCheckBoxColumn(TfpgEditColumn(Columns[AIndex]).Data).UncheckedText;
end;

procedure TfpgCustomEditGrid.SetBoxUncheckedText(AIndex: integer; const AValue: string);
begin
  TfpgCheckBoxColumn(TfpgEditColumn(Columns[AIndex]).Data).UncheckedText := AValue;
end;

function TfpgCustomEditGrid.GetBoxDisplayText(AIndex: integer): string;
begin
  Result := TfpgCheckBoxColumn(TfpgEditColumn(Columns[AIndex]).Data).BoxText;
end;

procedure TfpgCustomEditGrid.SetBoxDisplayText(AIndex: integer; const AValue: string);
begin
  TfpgCheckBoxColumn(TfpgEditColumn(Columns[AIndex]).Data).BoxText := AValue;
end;

procedure TfpgCustomEditGrid.IniCalendarCell;
var
  Pt: TPoint;
begin
  if Assigned(FCellCalendar) then
    FCellCalendar.Free;
  FCellCalendar := TfpgCalendarCombo.Create(Self);
  Pt.X := FFocusRect.Left;
  Pt.Y := FFocusRect.Top;
  with FCellCalendar do
  begin
    Name := 'FCellCalendar';
    SetPosition(Pt.X, Pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    DateFormat := TfpgCalendarColumn(TfpgEditColumn(Columns[FocusCol]).Data).CalendarDateFormat;
    DateValue:= TDates(TfpgCalendarColumn(TfpgEditColumn(Columns[FocusCol]).Data).FDatesList[FocusRow]).FDate;
    WeeklyHoliday := TfpgCalendarColumn(TfpgEditColumn(Columns[FocusCol]).Data).WeeklyHoliday;
    WeekStartDay := TfpgCalendarColumn(TfpgEditColumn(Columns[FocusCol]).Data).WeekStartDay;
    DayColor := TfpgCalendarColumn(TfpgEditColumn(Columns[FocusCol]).Data).DayColor;
    HolidayColor := TfpgCalendarColumn(TfpgEditColumn(Columns[FocusCol]).Data).HolidayColor;
    SingleClickSelect := TfpgCalendarColumn(TfpgEditColumn(Columns[FocusCol]).Data).SingleClickSelect;
    OnKeyPress := @FCellCalendarKeyPress;
    OnExit  := @FCellCalendarExit;
    SetFocus;
  end;
end;

procedure TfpgCustomEditGrid.FCellCalendarKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
    var Consumed: boolean);
begin
  case KeyCode of
    KeyReturn, KeyPEnter:
      begin
        with TfpgCalendarColumn(TfpgEditColumn(Columns[FocusCol]).Data) do
        begin
          TDates(FDatesList[FocusRow]).FDate := FCellCalendar.DateValue;
          Cells[FocusCol, FocusRow] := FormatDateTime(GridDateFormat, TDates(FDatesList[FocusRow]).FDate);
        end;
        //FCellCalendar.Text := '';
        FCellCalendar.Visible := False;
        case FEditWay of
          edColumn:
            if FocusCol < FColumns.Count then
              FocusCol := FocusCol + 1;
          edRow:
            if FocusRow < RowCount then
              FocusRow := FocusRow + 1;
        end;
        SetFocus;
        if FEditing then
          SetEditCell;
      end;
    KeyTab:
      begin
        //FCellCalendar.Text := '';
        FCellCalendar.Visible := False;
        if FEditing then
          FEditing := False;
        if ssShift in ShiftState then
        begin
          if FocusCol > 0 then
            FocusCol := FocusCol - 1;
        end
        else
          if FocusCol < FColumns.Count then
            FocusCol := FocusCol + 1;
        Consumed:= True;
      end;
    KeyEscape:
      begin
        //FCellCalendar.Text := '';
        FCellCalendar.Visible := False;
        if FEditing then
          FEditing := False;
        Consumed:= True;
      end;
  end;
end;

procedure TfpgCustomEditGrid.FCellCalendarExit(Sender: TObject);
begin
  FCellCalendar.Visible:= False;
end;

function TfpgCustomEditGrid.GetDates(AIndex: integer): TDateTime;
begin
  Result := TDates(TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).DatesList[Succ(FocusRow)]).FDate;
end;

procedure TfpgCustomEditGrid.SetDates(AIndex: integer; const AValue: TDateTime);
begin
  FDates  := TDates.Create;
  FDates.FDate:= AValue;
  TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).DatesList.Add(FDates);
end;

function TfpgCustomEditGrid.GetDatesList(AIndex: integer): TList;
begin
  Result := TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).DatesList;
end;

procedure TfpgCustomEditGrid.SetDatesList(AIndex: integer; const AValue: TList);
begin
  TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).DatesList := AValue;
end;

function TfpgCustomEditGrid.GetGridDateFormat(AIndex: integer): string;
begin
  Result := TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).GridDateFormat;
end;

procedure TfpgCustomEditGrid.SetGridDateFormat(AIndex: integer; const AValue: string);
begin
  TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).GridDateFormat:= AValue;
end;

function TfpgCustomEditGrid.GetCalendarDateFormat(AIndex: integer): string;
begin
  Result := TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).CalendarDateFormat;
end;

procedure TfpgCustomEditGrid.SetCalendarDateFormat(AIndex: integer; const AValue: string);
begin
  TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).CalendarDateFormat:= AValue;
end;

function TfpgCustomEditGrid.GetDateValue(AIndex: integer): TDateTime;
begin
  Result := TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).DateValue;
end;

procedure TfpgCustomEditGrid.SetDateValue(AIndex: integer; const AValue: TDateTime);
begin
  TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).DateValue:= AValue;
end;

function TfpgCustomEditGrid.GetMaxDate(AIndex: integer): TDateTime;
begin
  Result := TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).MaxDate;
end;

procedure TfpgCustomEditGrid.SetMaxdate(AIndex: integer; const AValue: TDateTime);
begin
  TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).MaxDate:= AValue;
end;

function TfpgCustomEditGrid.GetMinDate(AIndex: integer): TDateTime;
begin
  Result := TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).MinDate;
end;

procedure TfpgCustomEditGrid.SetMinDate(AIndex: integer; const AValue: TDateTime);
begin
  TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).MinDate:= AValue;
end;

function TfpgCustomEditGrid.GetWeeklyHoliday(AIndex: integer): integer;
begin
  Result := TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).WeeklyHoliday;
end;

procedure TfpgCustomEditGrid.SetWeeklyHoliday(AIndex: integer; const AValue: integer);
begin
  TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).WeeklyHoliday:= AValue;
end;

function TfpgCustomEditGrid.GetWeekStartDay(AIndex: integer): integer;
begin
  Result := TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).WeekStartDay;
end;

procedure TfpgCustomEditGrid.SetWeekStartDay(AIndex: integer; const AValue: integer);
begin
  TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).WeekStartDay:= AValue;
end;

function TfpgCustomEditGrid.GetDayColor(AIndex: integer): TfpgColor;
begin
  Result := TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).DayColor;
end;

procedure TfpgCustomEditGrid.SetDayColor(AIndex: integer; const AValue: TfpgColor);
begin
  TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).DayColor:= AValue;
end;

function TfpgCustomEditGrid.GetHolidayColor(AIndex: integer): TfpgColor;
begin
  Result := TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).HolidayColor;
end;

procedure TfpgCustomEditGrid.SetHolidayColor(AIndex: integer; const AValue: TfpgColor);
begin
  TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).HolidayColor:= AValue;
end;

function TfpgCustomEditGrid.GetSingleClickSelect(AIndex: integer): boolean;
begin
  Result := TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).SingleClickSelect;
end;

procedure TfpgCustomEditGrid.SetSingleClickSelect(AIndex: integer; const AValue: boolean);
begin
  TfpgCalendarColumn(TfpgEditColumn(Columns[AIndex]).Data).SingleClickSelect:= AValue;
end;

function TfpgCustomEditGrid.GetColumns(AIndex: Integer): TfpgEditColumn;
begin
  if (AIndex < 0) or (AIndex > ColumnCount-1) then
    Result := nil
  else
    Result := TfpgEditColumn(FColumns.Items[AIndex]);
end;

procedure TfpgCustomEditGrid.DrawCell(ARow, ACol: Integer; ARect: TfpgRect; AFlags: TfpgGridDrawState);
begin
  inherited DrawCell(ARow, ACol, ARect, AFlags);
  if (gdSelected in AFlags) then
    FFocusRect:= ARect;
end;

procedure TfpgCustomEditGrid.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
var
  i: integer;
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);
  if not Enabled then
    consumed := False
  else
  begin
    consumed := True;

    case keycode of
      keyInsert:
        for i := 0 to Pred(ColumnCount) do
          if Columns[i].EditType = etCalendar then
          begin
            FDates:= TDates.Create;
            FDates.FDate:= TfpgCalendarColumn(TfpgEditColumn(Columns[i]).Data).DateValue;
            TfpgCalendarColumn(TfpgEditColumn(Columns[i]).Data).FDatesList.Add(FDates);
          end;
      keyDelete:
        if RowCount > 0 then
          begin
          // specific warning and action should be performed in descendant
          end;
      keyReturn, keyPEnter:
        begin
        // specific action should be performed in descendant
        end;
      keyF2:
        if RowCount > 0 then
          begin
          case Columns[FocusCol].EditType of
            etText:
              IniTextCell;
            etInteger:
              IniIntegerCell;
            etFloat:
              IniFloatCell;
            etCurrency:
              IniCurrencyCell;
            etComboBox:
              IniComboBoxCell;
            etEditCombo:
              IniEditComboCell;
            etCheckBox:
              IniCheckBoxCell;
            etCalendar:
              IniCalendarCell;
          end;
          FEditing := True;
          end;
    else
      Consumed := False;
    end;
  end;
end;

constructor TfpgCustomEditGrid.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  OnFocusChange := @EditGridFocusChange;
  OnDoubleClick := @EditGridDoubleClick;
  FEditing := False;
  FEditWay := edColumn;
end;

destructor TfpgCustomEditGrid.Destroy;
begin
  inherited Destroy;
end;

function TfpgCustomEditGrid.AddColumn(ATitle: string; AWidth: integer; AEditType: TEditType = etNone;
        AAlignment: TAlignment = taLeftJustify; AbackgroundColor: TfpgColor = clDefault; ATextColor: TfpgColor = clDefault): TfpgEditColumn;
begin
  Updating;
  Result := TfpgEditColumn(inherited AddColumn(ATitle, AWidth, AAlignment, ABackgroundColor, ATextColor));
  with Result do
  begin
    FEditType := AEditType;
    case FEditType of
      etInteger:
        Result.FData:= TfpgIntegerColumn.Create;
      etFloat:
        Result.FData:= TfpgFloatColumn.Create;
      etCurrency:
        Result.FData:= TfpgCurrencyColumn.Create;
      etComboBox:
        Result.FData:= TfpgComboBoxColumn.Create;
      etEditCombo:
        Result.FData:= TfpgEditComboColumn.Create;
      etCheckBox:
        Result.FData:= TfpgCheckBoxColumn.Create;
      etCalendar:
        Result.FData := TfpgCalendarColumn.Create;
      else
        Result.FData:= nil;
    end;
  end;

  if UpdateCount = 0 then
    Updated;
end;

procedure TfpgCustomEditGrid.AddComboItem(AIndex: integer; const AValue: string);
begin
  TfpgComboBoxColumn(TfpgEditColumn(Columns[AIndex]).Data).FItems.Add(AValue);
end;

procedure TfpgCustomEditGrid.AddEditComboItem(AIndex: integer; const AValue: string);
begin
  TfpgEditComboColumn(TfpgEditColumn(Columns[AIndex]).Data).FItems.Add(AValue);
end;

end.

