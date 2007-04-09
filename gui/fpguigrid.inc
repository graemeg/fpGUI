{
    fpGUI  -  Free Pascal GUI Library
    
    Grid class declarations
    
    Copyright (C) 2000 - 2006 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{%mainunit fpgui.pas}

{$IFDEF read_interface}

// -------------------------------------------------------------------
//   TFCustomGrid
// -------------------------------------------------------------------

  EInvalidGridOperation = class(Exception);

  TGridDrawState = set of (gdSelected, gdFocused, gdFixed);


//  PIntegerArray = ^TIntegerArray;
//  TIntegerArray = array[0..(MAXINT div SizeOf(Integer))-1] of Integer;


  TFCustomGrid = class(TFWidget)
  private
    FColCount: Integer;
    FRowCount: Integer;
    FFixedCols: Integer;
    FFixedRows: Integer;
    FDefaultColWidth: Integer;
    FDefaultRowHeight: Integer;
    FGridWidth: Integer;
    FGridHeight: Integer;
    FFixedWidth: Integer;
    FFixedHeight: Integer;
    FColWidths, FRowHeights: PIntegerArray;
    procedure SetColCount(AColCount: Integer);
    procedure SetRowCount(ARowCount: Integer);
    procedure SetFixedCols(AFixedCols: Integer);
    procedure SetFixedRows(AFixedRows: Integer);
    procedure SetDefaultColWidth(AWidth: Integer);
    procedure SetDefaultRowHeight(AHeight: Integer);
    function  GetColWidths(ACol: Integer): Integer;
    procedure SetColWidths(ACol, AWidth: Integer);
    function  GetRowHeights(ARow: Integer): Integer;
    procedure SetRowHeights(ARow, AHeight: Integer);
    procedure HorzScrollBarScroll(Sender: TObject; var APosition: Integer);
    procedure VerTFScrollBarScroll(Sender: TObject; var APosition: Integer);
  protected
    ScrollingSupport: TScrollingSupport;
    procedure Paint(Canvas: TFCanvas); override;
    function  ProcessEvent(Event: TEventObj): Boolean; override;
    function  DistributeEvent(Event: TEventObj): Boolean; override;
    procedure CalcSizes; override;
    procedure Resized; override;
    procedure ColWidthsChanged; dynamic;
    procedure DrawCell(ACanvas: TFCanvas; ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState); virtual; abstract;
    procedure RowHeightsChanged; dynamic;
    procedure SizeChanged(OldColCount, OldRowCount: Integer); dynamic;
    property  CanExpandWidth default True;
    property  CanExpandHeight default True;
    property  ColCount: Integer read FColCount write SetColCount default 5;
    property  RowCount: Integer read FRowCount write SetRowCount default 5;
    property  FixedCols: Integer read FFixedCols write SetFixedCols default 1;
    property  FixedRows: Integer read FFixedRows write SetFixedRows default 1;
    property  GridWidth: Integer read FGridWidth;
    property  GridHeight: Integer read FGridHeight;
    property  FixedWidth: Integer read FFixedWidth;
    property  FixedHeight: Integer read FFixedHeight;
    property  DefaultColWidth: Integer read FDefaultColWidth write SetDefaultColWidth default 64;
    property  DefaultRowHeight: Integer read FDefaultRowHeight write SetDefaultRowHeight default 24;
    property  ColWidths[ACol: Integer]: Integer read GetColWidths write SetColWidths;
    property  RowHeights[ARow: Integer]: Integer read GetRowHeights write SetRowHeights;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;


// -------------------------------------------------------------------
//   TFDrawGrid
// -------------------------------------------------------------------

  TDrawCellEvent = procedure(Sender: TObject; ACanvas: TFCanvas;
    ACol, ARow: Integer; Rect: TRect; State: TGridDrawState) of object;


  TFDrawGrid = class(TFCustomGrid)
  private
    FOnDrawCell: TDrawCellEvent;
  protected
    procedure DrawCell(ACanvas: TFCanvas; ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState); override;
  public
    function CellRect(ACol, ARow: Integer): TRect;
    property ColWidths;
    property RowHeights;
  published
    property CanExpandWidth;
    property CanExpandHeight;
    property ColCount;
    property RowCount;
    property FixedCols;
    property FixedRows;
    property DefaultColWidth;
    property DefaultRowHeight;
    property OnDrawCell: TDrawCellEvent read FOnDrawCell write FOnDrawCell;
  end;


// -------------------------------------------------------------------
//   TFStringGrid
// -------------------------------------------------------------------

  PCells = ^TCells;
  TCells = array[0..(1 shl 30) div SizeOf(AnsiString)] of AnsiString;


  TFStringGrid = class(TFDrawGrid)
  private
    CellStrings: PCells;
    function    GetCells(ACol, ARow: Integer): String;
    procedure   SetCells(ACol, ARow: Integer; const AValue: String);
  protected
    //function GetEditText(ACol, ARow: Integer): String; override;
    //procedure SetEditText(ACol, ARow: Integer; const AValue: String); override;
    procedure   SizeChanged(OldColCount, OldRowCount: Integer); override;
    //procedure ColumnMoved(AFrom, ATo: Integer); override;
    //procedure RowMoved(AFrom, ATo: Integer); override;
    procedure   DrawCell(ACanvas: TFCanvas; ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Cells[ACol, ARow: Integer]: String read GetCells write SetCells;
    property    Cols[Index: Integer]: TStrings;
    property    Objects[ACol, ARow: Integer]: TObject;
    property    Rows[Index: Integer]: TStrings;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}


resourcestring

  SGridIndexOutOfRange = 'Grid index out of range';

// ===================================================================
//   TFCustomGrid
// ===================================================================

// public methods

constructor TFCustomGrid.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsOpaque];
  FCanExpandWidth := True;
  FCanExpandHeight := True;
  ScrollingSupport := TScrollingSupport.Create(Self);
  ScrollingSupport.HorzScrollBar.OnScroll := @HorzScrollBarScroll;
  ScrollingSupport.VerTFScrollBar.OnScroll := @VerTFScrollBarScroll;

  FDefaultColWidth := 64;
  FDefaultRowHeight := 24;
  FColCount := 5;
  FRowCount := 5;
  FFixedCols := 1;
  FFixedRows := 1;
  GetMem(FColWidths, FColCount * SizeOf(Integer));
  GetMem(FRowHeights, FRowCount * SizeOf(Integer));

  for i := 0 to 4 do
  begin
    FColWidths^[i] := FDefaultColWidth;
    FRowHeights^[i] := FDefaultRowHeight;
  end;
  ColWidthsChanged;
  RowHeightsChanged;
end;

destructor TFCustomGrid.Destroy;
begin
  FreeMem(FRowHeights);
  FreeMem(FColWidths);
  ScrollingSupport.Free;
  inherited Destroy;
end;


// protected methods

procedure TFCustomGrid.Paint(Canvas: TFCanvas);
var
  x1, y1, x2, y2, x, y, Sum: Integer;
  CellRect: TRect;
  GridDrawState: TGridDrawState;
begin
  if not Canvas.IntersectClipRect(ScrollingSupport.ClientRect) then
    exit;

  with Canvas.GetClipRect do
  begin
    x1 := Left;
    y1 := Top;
    x2 := Right;
    y2 := Bottom;
  end;

  { Initialize these for the case that the cell drawing loop won't get
    executed at all: }
  CellRect.Left := 0;
  CellRect.Right := 0;

  // Draw the cells

  CellRect.Top := ScrollingSupport.ClientRect.Top;
  y := 0;
  while y < RowCount do
  begin
    CellRect.Bottom := CellRect.Top + RowHeights[y];
    if CellRect.Bottom > y1 then
    begin
      CellRect.Left := ScrollingSupport.ClientRect.Left;
      x := 0;
      while x < ColCount do
      begin
        CellRect.Right := CellRect.Left + ColWidths[x];
	if CellRect.Right > x1 then
	begin
	  GridDrawState := [];
	  if (x < FixedCols) or (y < FixedRows) then
	    Include(GridDrawState, gdFixed);

	  Canvas.SaveState;
	  if gdFixed in GridDrawState then
	  begin
	    with CellRect do
	      Style.DrawButtonFace(Canvas,
	        Rect(Left, Top, Right + 1, Bottom + 1), []);
	    Style.SetUIColor(Canvas, clBtnText);
	  end else
	  begin
	    Style.SetUIColor(Canvas, clWindow);
	    Canvas.FillRect(CellRect);
	    Style.SetUIColor(Canvas, clWindowText);
	  end;
	  DrawCell(Canvas, x, y, CellRect, GridDrawState);
	  Canvas.RestoreState;
	end;

	CellRect.Left := CellRect.Right + 1;
	if CellRect.Left >= x2 then
	  break;

        Inc(x);
        if x = FixedRows then
          Inc(x, ScrollingSupport.HorzScrollBar.Position);
      end;
    end;

    CellRect.Top := CellRect.Bottom + 1;
    if CellRect.Top >= y2 then
      break;

    Inc(y);
    if y = FixedRows then
      Inc(y, ScrollingSupport.VerTFScrollBar.Position);
  end;


  // Draw the grid lines

  Style.SetUIColor(Canvas, cl3DFace);

  y := 0;
  Sum := ScrollingSupport.ClientRect.Top;
  while y < RowCount do
  begin
    Inc(Sum, RowHeights[y]);
    if (y >= FixedRows) and (Sum >= y1) then
      Canvas.DrawLine(Point(ScrollingSupport.ClientRect.Left + FixedWidth, Sum),
        Point(CellRect.Right + 1, Sum));
    Inc(Sum);
    if Sum >= y2 then
      break;

    Inc(y);
    if y = FixedRows then
      Inc(y, ScrollingSupport.VerTFScrollBar.Position);
  end;

  x := 0;
  Sum := ScrollingSupport.ClientRect.Left;
  while x < ColCount do
  begin
    Inc(Sum, ColWidths[x]);
    if (x >= FixedCols) and (Sum >= x1) then
      Canvas.DrawLine(Point(Sum, ScrollingSupport.ClientRect.Top + FixedHeight),
        Point(Sum, CellRect.Bottom));
    Inc(Sum);
    if Sum >= x2 then
      break;

    Inc(x);
    if x = FixedCols then
      Inc(x, ScrollingSupport.HorzScrollBar.Position);
  end;


  // Clear the empty space right and below the grid itself

  Style.SetUIColor(Canvas, clWindow);
  if CellRect.Right + 1 < x2 then
    Canvas.FillRect(Rect(CellRect.Right + 1, y1, x2, y2));
  if CellRect.Bottom + 1 < y2 then
    Canvas.FillRect(Rect(x1, CellRect.Top, CellRect.Right + 1, y2));
end;

function TFCustomGrid.ProcessEvent(Event: TEventObj): Boolean;
begin
  Result := ScrollingSupport.ProcessEvent(Event) or
    inherited ProcessEvent(Event);
end;

function TFCustomGrid.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := ScrollingSupport.DistributeEvent(Event) or
    inherited DistributeEvent(Event);
end;

procedure TFCustomGrid.CalcSizes;
begin
  ScrollingSupport.CalcSizes;
end;

procedure TFCustomGrid.Resized;
begin
  ScrollingSupport.Resized;
end;

procedure TFCustomGrid.ColWidthsChanged;
var
  i: Integer;
begin
  FGridWidth := 0;
  for i := 0 to ColCount - 1 do
    Inc(FGridWidth, ColWidths[i] + 1);
  if FGridWidth > 0 then
    Dec(FGridWidth);

  FFixedWidth := 0;
  for i := 0 to FixedCols - 1 do
    Inc(FFixedWidth, ColWidths[i] + 1);
  if FFixedWidth > 0 then
    Dec(FFixedWidth);
end;

procedure TFCustomGrid.RowHeightsChanged;
var
  i: Integer;
begin
  FGridHeight := 0;
  for i := 0 to RowCount - 1 do
    Inc(FGridHeight, RowHeights[i] + 1);
  if FGridHeight > 0 then
    Dec(FGridHeight);

  FFixedHeight := 0;
  for i := 0 to FixedRows - 1 do
    Inc(FFixedHeight, RowHeights[i] + 1);
  if FFixedHeight > 0 then
    Dec(FFixedHeight);
end;

procedure TFCustomGrid.SizeChanged(OldColCount, OldRowCount: Integer);
begin
  Assert(OldColCount = OldColCount);
  Assert(OldRowCount = OldRowCount);
  // This dynamic method is only used for descendants of TFCustomGrid
end;


// private methods

procedure TFCustomGrid.SetColCount(AColCount: Integer);
var
  OldColCount, i: Integer;
begin
  if AColCount <> FColCount then
  begin
    OldColCount := FColCount;
    FColCount := AColCount;

    ReallocMem(FColWidths, FColCount * SizeOf(Integer));
    for i := OldColCount to FColCount - 1 do
      FColWidths^[i] := FDefaultColWidth;

    ScrollingSupport.HorzScrollBar.Max := ColCount -  FixedCols - 1;

    ColWidthsChanged;
    SizeChanged(OldColCount, FRowCount);
  end;
end;

procedure TFCustomGrid.SetRowCount(ARowCount: Integer);
var
  OldRowCount, i: Integer;
begin
  if ARowCount <> FRowCount then
  begin
    OldRowCount := FRowCount;
    FRowCount := ARowCount;

    ReallocMem(FRowHeights, FRowCount * SizeOf(Integer));
    for i := OldRowCount to FRowCount - 1 do
      FRowHeights^[i] := FDefaultRowHeight;

    ScrollingSupport.VerTFScrollBar.Max := RowCount - FixedRows - 1;

    RowHeightsChanged;
    SizeChanged(FColCount, OldRowCount);
  end;
end;

procedure TFCustomGrid.SetFixedCols(AFixedCols: Integer);
begin
  if AFixedCols <> FixedCols then
  begin
    FFixedCols := AFixedCols;
    ScrollingSupport.HorzScrollBar.Max := ColCount -  FixedCols - 1;
  end;
end;

procedure TFCustomGrid.SetFixedRows(AFixedRows: Integer);
begin
  if AFixedRows <> FixedRows then
  begin
    FFixedRows := AFixedRows;
    ScrollingSupport.VerTFScrollBar.Max := RowCount - FixedRows - 1;
  end;
end;

procedure TFCustomGrid.SetDefaultColWidth(AWidth: Integer);
var
  i: Integer;
begin
  if AWidth <> FDefaultColWidth then
  begin
    FDefaultColWidth := AWidth;
    for i := 0 to FColCount - 1 do
      FColWidths^[i] := AWidth;
    ColWidthsChanged;
  end;
end;

procedure TFCustomGrid.SetDefaultRowHeight(AHeight: Integer);
var
  i: Integer;
begin
  if AHeight <> FDefaultRowHeight then
  begin
    FDefaultRowHeight := AHeight;
    for i := 0 to FRowCount - 1 do
      FRowHeights^[i] := AHeight;
    RowHeightsChanged;
  end;
end;

function TFCustomGrid.GetColWidths(ACol: Integer): Integer;
begin
  if (ACol < 0) or (ACol >= FColCount) then
    raise EInvalidGridOperation(SGridIndexOutOfRange);
  Result := FColWidths^[ACol];
end;

procedure TFCustomGrid.SetColWidths(ACol, AWidth: Integer);
begin
  if (ACol < 0) or (ACol >= FColCount) then
    raise EInvalidGridOperation(SGridIndexOutOfRange);
  FColWidths^[ACol] := AWidth;
end;

function TFCustomGrid.GetRowHeights(ARow: Integer): Integer;
begin
  if (ARow < 0) or (ARow >= FRowCount) then
    raise EInvalidGridOperation(SGridIndexOutOfRange);
  Result := FRowHeights^[ARow];
end;

procedure TFCustomGrid.SetRowHeights(ARow, AHeight: Integer);
begin
  if (ARow < 0) or (ARow >= FRowCount) then
    raise EInvalidGridOperation(SGridIndexOutOfRange);
  FRowHeights^[ARow] := AHeight;
end;

procedure TFCustomGrid.HorzScrollBarScroll(Sender: TObject;
  var APosition: Integer);
var
  i, Delta: Integer;
  r: TRect;
begin
  Delta := 0;
  if APosition > ScrollingSupport.HorzScrollBar.Position then
    for i := ScrollingSupport.HorzScrollBar.Position to APosition - 1 do
      Dec(Delta, ColWidths[i + FixedCols] + 1)
  else
    for i := APosition to ScrollingSupport.HorzScrollBar.Position - 1 do
      Inc(Delta, ColWidths[i + FixedCols] + 1);

  // Scroll the horizontal fixed cells
  r := ScrollingSupport.ClientRect;
  Inc(r.Left, FixedWidth + 1);
  if r.Left <= r.Right then
  begin
    if Delta < 0 then		// Scrolling to the right side
      Dec(r.Left, Delta)
    else			// Scrolling to the left side
      Dec(r.Right, Delta);
    Scroll(r, Delta, 0);
  end;
end;

procedure TFCustomGrid.VerTFScrollBarScroll(Sender: TObject;
  var APosition: Integer);
var
  i, Delta: Integer;
  r: TRect;
begin
  Delta := 0;
  if APosition > ScrollingSupport.VerTFScrollBar.Position then
    for i := ScrollingSupport.VerTFScrollBar.Position to APosition - 1 do
      Dec(Delta, RowHeights[i + FixedRows] + 1)
  else
    for i := APosition to ScrollingSupport.VerTFScrollBar.Position - 1 do
      Inc(Delta, RowHeights[i + FixedRows] + 1);

  // Scroll the grid body
  r := ScrollingSupport.ClientRect;
  Inc(r.Top, FixedHeight + 1);
  if r.Top <= r.Bottom then
  begin
    if Delta < 0 then		// Scrolling downwards
      Dec(r.Top, Delta)
    else			// Scrolling upwards
      Dec(r.Bottom, Delta);
    Scroll(r, 0, Delta);
  end;
end;


// -------------------------------------------------------------------
//   TFDrawGrid
// -------------------------------------------------------------------

procedure TFDrawGrid.DrawCell(ACanvas: TFCanvas; ACol, ARow: Integer;
  ARect: TRect;
 AState: TGridDrawState);
begin
  if Assigned(OnDrawCell) then
    OnDrawCell(Self, ACanvas, ACol, ARow, ARect, AState);
end;

function TFDrawGrid.CellRect(ACol, ARow: Integer): TRect;
var
  i: Integer;
begin
  Result.Left := 0;
  for i := 0 to ACol - 1 do
    Inc(Result.Left, ColWidths[i]);
  Result.Right := Result.Left + ColWidths[ACol];

  Result.Top := 0;
  for i := 0 to ARow - 1 do
    Inc(Result.Top, RowHeights[i]);
  Result.Bottom := Result.Top + RowHeights[ARow];
end;


// -------------------------------------------------------------------
//   TFStringGrid
// -------------------------------------------------------------------

function TFStringGrid.GetCells(ACol, ARow: Integer): String;
begin
  if (ACol >= 0) and (ARow >= 0) and (ACol < ColCount) and (ARow < RowCount) then
    Result := CellStrings^[ARow * ColCount + ACol]
  else
    SetLength(Result, 0);
end;

procedure TFStringGrid.SetCells(ACol, ARow: Integer; const AValue: String);
begin
  if (ACol >= 0) and (ARow >= 0) and (ACol < ColCount) and (ARow < RowCount) then
    CellStrings^[ARow * ColCount + ACol] := AValue;
end;

procedure TFStringGrid.SizeChanged(OldColCount, OldRowCount: Integer);
var
  Count: Integer;
begin
  inherited SizeChanged(OldColCount, OldRowCount);
  ReallocMem(CellStrings, ColCount * RowCount * SizeOf(AnsiString));
  Count := ColCount * RowCount - OldColCount * OldRowCount;
  if Count > 0 then
    FillChar(CellStrings^[OldColCount * OldRowCount],
      Count * SizeOf(AnsiString), #0);
end;

procedure TFStringGrid.DrawCell(ACanvas: TFCanvas; ACol, ARow: Integer;
  ARect: TRect;
 AState: TGridDrawState);
var
  s: String;
begin
  // WriteLn('TFStringGrid.DrawCell(', ACol, ', ', ARow, ', ', Integer(AState), ');');
  s := Cells[ACol, ARow];
  if Length(s) > 0 then
    ACanvas.TextOut(ARect.TopLeft + Point(2, 2), s);
  if Assigned(OnDrawCell) then
    OnDrawCell(Self, ACanvas, ACol, ARow, ARect, AState);
end;

constructor TFStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.SizeChanged(0, 0);
end;

destructor TFStringGrid.Destroy;
var
  i: Integer;
begin
  for i := 0 to RowCount * ColCount - 1 do
    CellStrings^[i] := '';
  FreeMem(CellStrings);
  inherited Destroy;
end;


{$ENDIF read_implementation}

