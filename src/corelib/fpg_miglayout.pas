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
  TfpgMigLayoutManager = class(TfpgBaseLayoutManager)
  private
    FColumnCount: Integer;
    FRowGap: Integer;
    FColumnGap: Integer;
    procedure PositionWidgetInCell(AWidget: TfpgWidget; const ACellRect: TfpgRect; AConstraint: TfpgMigConstraint);
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
    FGrid: array of array of TfpgMigCell;
    FColumnGap, FRowGap: Integer;
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
var
  c, r: Integer;
begin
  FColumnCount := AColumnCount;
  FCells := TfpgMigCellList.Create(True);
  FRowCount := 1;
  SetLength(FGrid, FColumnCount, FRowCount);

  // Initialize grid to nil
  for c := 0 to FColumnCount - 1 do
    for r := 0 to FRowCount - 1 do
      FGrid[c, r] := nil;
end;

destructor TfpgMigGrid.Destroy;
begin
  FCells.Free;
  inherited Destroy;
end;

procedure TfpgMigGrid.AddWidget(AWidget: TfpgWidget; ARow, ACol, ASpanX, ASpanY: Integer; AConstraint: TfpgMigConstraint);
var
  cell: TfpgMigCell;
  r, c: Integer;
  oldRowCount: Integer;
begin
  cell := TfpgMigCell.Create;
  cell.FWidget := AWidget;
  cell.FRow := ARow;
  cell.FCol := ACol;
  cell.FSpanX := ASpanX;
  cell.FSpanY := ASpanY;
  cell.FConstraint := AConstraint;

  FCells.Add(cell);

  // Resize grid if needed
  if ARow + ASpanY > FRowCount then
  begin
    oldRowCount := FRowCount;
    FRowCount := ARow + ASpanY;
    SetLength(FGrid, FColumnCount, FRowCount);

    // Initialize new rows to nil
    for c := 0 to FColumnCount - 1 do
      for r := oldRowCount to FRowCount - 1 do
        FGrid[c, r] := nil;
  end;

  // Mark cells as occupied
  for r := ARow to ARow + ASpanY - 1 do
  begin
    for c := ACol to ACol + ASpanX - 1 do
    begin
      FGrid[c, r] := cell;
    end;
  end;
end;

procedure TfpgMigGrid.CalculateColumnWidths(AAvailableWidth, AGap: Integer);
var
  i, col: Integer;
  cell: TfpgMigCell;
  maxWidth: Integer;
  totalPrefWidth, totalGrow: Integer;
  extraSpace, growFactor: Double;
begin
  SetLength(FColumns, FColumnCount);

  if FColumnCount = 1 then
  begin
    FColumns[0].PrefSize := AAvailableWidth - (2 * AGap);
  end
  else
  begin
    // Calculate preferred width and grow weight for each column
    totalPrefWidth := 0;
    totalGrow := 0;
    for col := 0 to FColumnCount - 1 do
    begin
      maxWidth := 0;
      FColumns[col].GrowPriority := 0;
      for i := 0 to CellCount - 1 do
      begin
        cell := GetCell(i);
        if cell.Col = col then
        begin
          if cell.Widget.Width > maxWidth then
            maxWidth := cell.Widget.Width;
          if cell.Constraint.GrowX > FColumns[col].GrowPriority then
            FColumns[col].GrowPriority := cell.Constraint.GrowX;
        end;
      end;
      FColumns[col].PrefSize := maxWidth;
      totalPrefWidth := totalPrefWidth + maxWidth;
      totalGrow := totalGrow + FColumns[col].GrowPriority;
    end;

    // Distribute extra space
    extraSpace := AAvailableWidth - totalPrefWidth - (FColumnCount + 1) * AGap;
    if (extraSpace > 0) and (totalGrow > 0) then
    begin
      growFactor := extraSpace / totalGrow;
      for col := 0 to FColumnCount - 1 do
      begin
        if FColumns[col].GrowPriority > 0 then
          FColumns[col].PrefSize := FColumns[col].PrefSize + round(FColumns[col].GrowPriority * growFactor);
      end;
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
  totalPrefHeight, totalGrow: Integer;
  extraSpace, growFactor: Double;
  spanningHeight: Integer;
begin
  SetLength(FRows, FRowCount);

  if FRowCount = 1 then
  begin
    FRows[0].PrefSize := AAvailableHeight - (2 * AGap);
  end
  else
  begin
    // Calculate preferred height and grow weight for each row
    totalPrefHeight := 0;
    totalGrow := 0;
    for row := 0 to FRowCount - 1 do
    begin
      maxHeight := 0;
      FRows[row].GrowPriority := 0;
      for i := 0 to CellCount - 1 do
      begin
        cell := GetCell(i);
        // Only consider single-row widgets for initial calculation
        if (cell.Row = row) and (cell.SpanY = 1) then
        begin
          if cell.Widget.Height > maxHeight then
            maxHeight := cell.Widget.Height;
          if cell.Constraint.GrowY > FRows[row].GrowPriority then
            FRows[row].GrowPriority := cell.Constraint.GrowY;
        end;
      end;
      FRows[row].PrefSize := maxHeight;
      totalPrefHeight := totalPrefHeight + maxHeight;
      totalGrow := totalGrow + FRows[row].GrowPriority;
    end;

    // Handle multi-row spanning widgets
    // Only expand rows if the total spanned height is less than what single-span widgets need
    // Spanning widgets adapt to available space rather than forcing expansion
    // Note: Full MigLayout would use "eagerness" levels here for more sophisticated expansion
    for i := 0 to CellCount - 1 do
    begin
      cell := GetCell(i);
      if cell.SpanY > 1 then
      begin
        // Calculate total height of spanned rows (including gaps)
        spanningHeight := 0;
        for row := cell.Row to cell.Row + cell.SpanY - 1 do
          spanningHeight := spanningHeight + FRows[row].PrefSize;
        spanningHeight := spanningHeight + (cell.SpanY - 1) * AGap;

        // Only expand if there are no other constraints preventing it
        // In this simplified implementation, we don't automatically expand for spanning widgets
        // The widget will be sized to fit the available spanned space
        // To force expansion, use GrowY constraint
      end;
    end;

    // Distribute extra space
    extraSpace := AAvailableHeight - totalPrefHeight - (FRowCount + 1) * AGap;
    if (extraSpace > 0) and (totalGrow > 0) then
    begin
      growFactor := extraSpace / totalGrow;
      for row := 0 to FRowCount - 1 do
      begin
        if FRows[row].GrowPriority > 0 then
          FRows[row].PrefSize := FRows[row].PrefSize + round(FRows[row].GrowPriority * growFactor);
      end;
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
  i: Integer;
begin
  cellX := FColumns[ACell.Col].Position;
  cellY := FRows[ACell.Row].Position;

  cellW := 0;
  for i := ACell.Col to ACell.Col + ACell.SpanX - 1 do
    cellW := cellW + FColumns[i].PrefSize;
  cellW := cellW + (ACell.SpanX - 1) * FColumnGap;

  cellH := 0;
  for i := ACell.Row to ACell.Row + ACell.SpanY - 1 do
    cellH := cellH + FRows[i].PrefSize;
  cellH := cellH + (ACell.SpanY - 1) * FRowGap;

  widgetW := ACell.Widget.Width;
  widgetH := ACell.Widget.Height;

  // Handle horizontal alignment
  case ACell.Constraint.AlignX of
    axLeft: newX := cellX;
    axCenter: newX := cellX + (cellW - widgetW) div 2;
    axRight: newX := cellX + cellW - widgetW;
    axFill: newX := cellX;
  end;

  // Handle vertical alignment
  case ACell.Constraint.AlignY of
    ayTop: newY := cellY;
    ayCenter: newY := cellY + (cellH - widgetH) div 2;
    ayBottom: newY := cellY + cellH - widgetH;
    ayFill: newY := cellY;
  end;

  if ACell.Constraint.AlignX = axFill then widgetW := cellW;
  if ACell.Constraint.AlignY = ayFill then widgetH := cellH;

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
  i, col, row: Integer;
  cellRect: TfpgRect;
begin
  // 1. Build the grid cells
  grid := TfpgMigGrid.Create(FColumnCount);
  try
    row := 0;
    col := 0;

    // place widgets in grid cells
    iter := GetIterator(AContainer);
    while iter.HasNext do
    begin
      // Find next available cell
      while (row < grid.FRowCount) and (grid.FGrid[col, row] <> nil) do
      begin
        Inc(col);
        if col >= FColumnCount then
        begin
          col := 0;
          Inc(row);
        end;
      end;

      widget := iter.Next as TfpgWidget;
      constraint := GetConstraintOrDefault(widget) as TfpgMigConstraint;
      grid.AddWidget(widget, row, col, constraint.SpanX, constraint.SpanY, constraint);

      // Advance grid position
      inc(col, constraint.SpanX);
      if col >= FColumnCount then
      begin
        col := 0;
        inc(row);
      end;
    end;

    // 2. Calculate column widths and row heights
    grid.CalculateColumnWidths(AContainer.Width, FColumnGap);
    grid.CalculateRowHeights(AContainer.Height, FRowGap);

    // 3. Position widgets within cells
    for i := 0 to grid.CellCount - 1 do
    begin
      cellRect := grid.GetCellRect(grid.GetCell(i));
      widget := grid.GetCell(i).Widget;
      constraint := GetConstraintOrDefault(widget) as TfpgMigConstraint;

      // Apply alignment and growth
      PositionWidgetInCell(widget, cellRect, constraint);
    end;

  finally
    grid.Free;
  end;
end;

procedure TfpgMigLayoutManager.PositionWidgetInCell(AWidget: TfpgWidget;
  const ACellRect: TfpgRect; AConstraint: TfpgMigConstraint);
var
  widgetRect: TfpgRect;
  widgetWidth, widgetHeight: Integer;
begin
  widgetRect := ACellRect;

  // Calculate widget width based on constraint
  case AConstraint.AlignX of
    axFill:
      widgetWidth := ACellRect.Width;  // Fill entire cell width
    else
      // Use preferred width or current width
      if AWidget.Width > 0 then
        widgetWidth := AWidget.Width
      else
        widgetWidth := AWidget.Width;

      // Apply horizontal alignment
      case AConstraint.AlignX of
        axLeft:
          widgetRect.Left := ACellRect.Left;
        axCenter:
          widgetRect.Left := ACellRect.Left + (ACellRect.Width - widgetWidth) div 2;
        axRight:
          widgetRect.Left := ACellRect.Right - widgetWidth;
      end;

      widgetRect.Width := widgetWidth;
  end;

  // Calculate widget height based on constraint (similar to width)
  case AConstraint.AlignY of
    ayFill:
      widgetHeight := ACellRect.Height;
    else
      if AWidget.Height > 0 then
        widgetHeight := AWidget.Height
      else
        widgetHeight := AWidget.Height;

      case AConstraint.AlignY of
        ayTop:
          widgetRect.Top := ACellRect.Top;
        ayCenter:
          widgetRect.Top := ACellRect.Top + (ACellRect.Height - widgetHeight) div 2;
        ayBottom:
          widgetRect.Top := ACellRect.Bottom - widgetHeight;
      end;

      widgetRect.Height := widgetHeight;
  end;

  // Apply calculated bounds to widget
  AWidget.SetPosition(widgetrect.Left, widgetrect.Top, widgetrect.Width, widgetrect.Height);
//  AWidget.SetBoundsRect(widgetRect);
end;

function TfpgMigLayoutManager.DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize;
begin
  // To be implemented
  Result.SetSize(0, 0);
end;

end.
