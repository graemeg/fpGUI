unit fpg_miglayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  fpg_base,
  fpg_widget,
  fpg_layouttypes,
  fpg_layoutmanager,
  fpg_migconstraint;

type

  { TfpgMigLayoutManager }

  TfpgMigLayoutManager = class(TfpgBaseLayoutManager)
  private
    FColumnCount: Integer;
    FRowGap: Integer;
    FColumnGap: Integer;
  protected
    // TfpgBaseLayoutManager overrides
    function CreateDefaultConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint; override;
    procedure DoLayout(AContainer: TfpgWidgetBase); override;
    function DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize; override;
  public
    constructor Create; override;
  published
    property ColumnCount: Integer read FColumnCount write FColumnCount default 1;
    property RowGap: Integer read FRowGap write FRowGap default 6;
    property ColumnGap: Integer read FColumnGap write FColumnGap default 6;
  end;

implementation

type
  TfpgMigCell = class
  private
    FWidget: TfpgWidget;
    FRow, FCol: Integer;
    FSpanX, FSpanY: Integer;
    FConstraint: TfpgMigConstraint;
  public
    property Widget: TfpgWidget read FWidget;
    property Row: Integer read FRow;
    property Col: Integer read FCol;
    property SpanX: Integer read FSpanX;
    property SpanY: Integer read FSpanY;
    property Constraint: TfpgMigConstraint read FConstraint;
  end;

  TfpgMigCellList = specialize TObjectList<TfpgMigCell>;

  // Column/Row sizing information
  TfpgMigDimension = record
    MinSize: Integer;      // Minimum size (from widget constraints)
    PrefSize: Integer;     // Preferred size (from widget preferred sizes)
    MaxSize: Integer;      // Maximum size (from widget constraints)
    GrowPriority: Integer; // Priority for growing (higher = grows first)
    Size: Integer;         // Calculated actual size
    Position: Integer;     // Calculated position
  end;

  // Grid structure manager
  TfpgMigGrid = class
  private
    FColumnCount: Integer;
    FRowCount: Integer;
    FCells: TfpgMigCellList;
    FColumns: array of TfpgMigDimension;
    FRows: array of TfpgMigDimension;
    function GetCellCount: Integer;
  public
    constructor Create(AColumnCount: Integer);
    destructor Destroy; override;

    procedure AddWidget(AWidget: TfpgWidget; ARow, ACol, ASpanX, ASpanY: Integer;
                       AConstraint: TfpgMigConstraint);

    procedure CalculateColumnWidths(AAvailableWidth, AGap: Integer);
    procedure CalculateRowHeights(AAvailableHeight, AGap: Integer);

    function GetCellRect(ACell: TfpgMigCell): TfpgRect;
    function GetCell(AIndex: Integer): TfpgMigCell;

    property CellCount: Integer read GetCellCount;
    property RowCount: Integer read FRowCount;
  end;

{ TfpgMigGrid }

constructor TfpgMigGrid.Create(AColumnCount: Integer);
begin
  FColumnCount := AColumnCount;
  FCells := TfpgMigCellList.Create(True);
end;

destructor TfpgMigGrid.Destroy;
begin
  FCells.Free;
  inherited Destroy;
end;

procedure TfpgMigGrid.AddWidget(AWidget: TfpgWidget; ARow, ACol, ASpanX, ASpanY: Integer; AConstraint: TfpgMigConstraint);
var
  cell: TfpgMigCell;
begin
  cell := TfpgMigCell.Create;
  cell.FWidget := AWidget;
  cell.FRow := ARow;
  cell.FCol := ACol;
  cell.FSpanX := ASpanX;
  cell.FSpanY := ASpanY;
  cell.FConstraint := AConstraint;

  FCells.Add(cell);

  // Track maximum row count
  if ARow + ASpanY > FRowCount then
    FRowCount := ARow + ASpanY;
end;

procedure TfpgMigGrid.CalculateColumnWidths(AAvailableWidth, AGap: Integer);
var
  i, col: Integer;
  cell: TfpgMigCell;
  maxWidth: Integer;
begin
  SetLength(FColumns, FColumnCount);

  if FColumnCount = 1 then
  begin
    FColumns[0].PrefSize := AAvailableWidth - (2 * AGap);
  end
  else
  begin
    for col := 0 to FColumnCount - 1 do
    begin
      maxWidth := 0;
      for i := 0 to CellCount - 1 do
      begin
        cell := GetCell(i);
        if cell.Col = col then
        begin
          if cell.Widget.Width > maxWidth then
            maxWidth := cell.Widget.Width;
        end;
      end;
      FColumns[col].PrefSize := maxWidth;
    end;
  end;

  // Set position for each column
  FColumns[0].Position := AGap;
  for col := 1 to FColumnCount - 1 do
  begin
    FColumns[col].Position := FColumns[col-1].Position + FColumns[col-1].PrefSize + AGap;
  end;
end;

procedure TfpgMigGrid.CalculateRowHeights(AAvailableHeight, AGap: Integer);
var
  i, row: Integer;
  cell: TfpgMigCell;
  maxHeight: Integer;
begin
  SetLength(FRows, FRowCount);

  if FRowCount = 1 then
  begin
    FRows[0].PrefSize := AAvailableHeight - (2 * AGap);
  end
  else
  begin
    for row := 0 to FRowCount - 1 do
    begin
      maxHeight := 0;
      for i := 0 to CellCount - 1 do
      begin
        cell := GetCell(i);
        if cell.Row = row then
        begin
          if cell.Widget.Height > maxHeight then
            maxHeight := cell.Widget.Height;
        end;
      end;
      FRows[row].PrefSize := maxHeight;
    end;
  end;

  // Set position for each row
  if FRowCount > 0 then
  begin
    FRows[0].Position := AGap;
    for row := 1 to FRowCount - 1 do
    begin
      FRows[row].Position := FRows[row-1].Position + FRows[row-1].PrefSize + AGap;
    end;
  end;
end;

function TfpgMigGrid.GetCellRect(ACell: TfpgMigCell): TfpgRect;
var
  cellX, cellY, cellW, cellH: Integer;
  widgetW, widgetH: Integer;
  newX, newY: Integer;
begin
  cellX := FColumns[ACell.Col].Position;
  cellY := FRows[ACell.Row].Position;
  cellW := FColumns[ACell.Col].PrefSize;
  cellH := FRows[ACell.Row].PrefSize;

  widgetW := ACell.Widget.Width;
  widgetH := ACell.Widget.Height;

  // Handle horizontal alignment
  case ACell.Constraint.AlignX of
    axLeft: newX := cellX;
    axCenter: newX := cellX + (cellW - widgetW) div 2;
    axRight: newX := cellX + cellW - widgetW;
  else
    newX := cellX; // Default to left
  end;

  // Handle vertical alignment
  case ACell.Constraint.AlignY of
    ayTop: newY := cellY;
    ayCenter: newY := cellY + (cellH - widgetH) div 2;
    ayBottom: newY := cellY + cellH - widgetH;
  else
    newY := cellY; // Default to top
  end;

  Result.SetRect(newX, newY, widgetW, widgetH);
end;

function TfpgMigGrid.GetCell(AIndex: Integer): TfpgMigCell;
begin
  Result := FCells[AIndex];
end;

function TfpgMigGrid.GetCellCount: Integer;
begin
  Result := FCells.Count;
end;


{ TfpgMigLayoutManager }

constructor TfpgMigLayoutManager.Create;
begin
  inherited Create;
  FColumnCount := 1;
  FRowGap := 6;
  FColumnGap := 6;
end;

function TfpgMigLayoutManager.CreateDefaultConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint;
begin
  Result := TfpgMigConstraint.Create;
end;

procedure TfpgMigLayoutManager.DoLayout(AContainer: TfpgWidgetBase);
var
  grid: TfpgMigGrid;
  iter: ILayoutIterator;
  widget: TfpgWidget;
  constraint: TfpgMigConstraint;
  i, column, row: Integer;
begin
  grid := TfpgMigGrid.Create(FColumnCount);
  try
    iter := GetIterator(AContainer);
    column := 0;
    row := 0;
    while iter.HasNext do
    begin
      widget := iter.Next as TfpgWidget;
      constraint := GetConstraint(widget) as TfpgMigConstraint;
      grid.AddWidget(widget, row, column, constraint.SpanX, constraint.SpanY, constraint);

      Inc(column);
      if column >= FColumnCount then
      begin
        column := 0;
        Inc(row);
      end;
    end;

    grid.CalculateColumnWidths(AContainer.Width, FColumnGap);
    grid.CalculateRowHeights(AContainer.Height, FRowGap);

    for i := 0 to grid.CellCount - 1 do
    begin
      widget := grid.GetCell(i).Widget;
      with grid.GetCellRect(grid.GetCell(i)) do
        widget.SetPosition(Left, Top, Width, Height);
    end;

  finally
    grid.Free;
  end;
end;

function TfpgMigLayoutManager.DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize;
begin
  // To be implemented
  Result.SetSize(0, 0);
end;

end.
