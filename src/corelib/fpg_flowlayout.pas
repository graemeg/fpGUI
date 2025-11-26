{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2025 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      A layout manager that arranges components in a directional flow,
      much like lines of text in a paragraph.
}
unit fpg_flowlayout;

{$I fpg_defines.inc}

interface

uses
  Classes, SysUtils,
  fpg_base,
  fpg_layoutmanager, fpg_layouttypes;

type
  TfpgFlowLayoutAlignment = (
    flaLeft,
    flaCenter,
    flaRight//,
    //flaLeading,
    //flaTrailing
  );

  TfpgFlowLayoutVAlignment = (
    flvaTop,
    flvaCenter,
    flvaBottom
  );

  TfpgFlowLayoutManager = class(TfpgBaseLayoutManager)
  private
    FAlignment: TfpgFlowLayoutAlignment;
    FVAlignment: TfpgFlowLayoutVAlignment;
    FHGap: TfpgCoord;
    FVGap: TfpgCoord;
    procedure SetAlignment(AValue: TfpgFlowLayoutAlignment);
    procedure SetVAlignment(AValue: TfpgFlowLayoutVAlignment);
    procedure SetHGap(AValue: TfpgCoord);
    procedure SetVGap(AValue: TfpgCoord);
  protected
    procedure DoLayout(AContainer: TfpgWidgetBase); override;
    function DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize; override;
    function DoGetMinimumSize(AContainer: TfpgWidgetBase): TfpgSize; override;
    function CreateDefaultConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint; override;
  public
    constructor Create; override; overload;
    constructor Create(const hgap: integer; const vgap: integer); overload;
    property Alignment: TfpgFlowLayoutAlignment read FAlignment write SetAlignment default flaLeft;
    property VAlignment: TfpgFlowLayoutVAlignment read FVAlignment write SetVAlignment default flvaTop;
    property HGap: TfpgCoord read FHGap write SetHGap default 5;
    property VGap: TfpgCoord read FVGap write SetVGap default 5;
  end;

implementation

uses
  fpg_widget;

type
  TRowInfo = record
    Width: TfpgCoord;
    Height: TfpgCoord;
    ComponentCount: integer;
  end;

{ TfpgFlowLayoutManager }

constructor TfpgFlowLayoutManager.Create;
begin
  inherited Create;
  FAlignment := flaLeft;
  FVAlignment := flvaTop;
  FHGap := 5;
  FVGap := 5;
end;

constructor TfpgFlowLayoutManager.Create(const hgap: integer; const vgap: integer);
begin
  Create;
  FHGap := hgap;
  FVGap := vgap;
end;

procedure TfpgFlowLayoutManager.SetAlignment(AValue: TfpgFlowLayoutAlignment);
begin
  if FAlignment <> AValue then
  begin
    FAlignment := AValue;
  end;
end;

procedure TfpgFlowLayoutManager.SetVAlignment(AValue: TfpgFlowLayoutVAlignment);
begin
  if FVAlignment <> AValue then
  begin
    FVAlignment := AValue;
  end;
end;

procedure TfpgFlowLayoutManager.SetHGap(AValue: TfpgCoord);
begin
  if FHGap <> AValue then
  begin
    FHGap := AValue;
  end;
end;

procedure TfpgFlowLayoutManager.SetVGap(AValue: TfpgCoord);
begin
  if FVGap <> AValue then
  begin
    FVGap := AValue;
  end;
end;

procedure TfpgFlowLayoutManager.DoLayout(AContainer: TfpgWidgetBase);
var
  Iterator: ILayoutIterator;
  w: TfpgWidget;
  prefSize: TfpgSize;
  x, y: TfpgCoord;
  rowMaxHeight: TfpgCoord;
  ContainerWidth, ContainerHeight: TfpgCoord;
  Rows: array of TRowInfo;
  rowIdx: integer;
  i, j: integer;
  rowWidth: TfpgCoord;
  rowStartX: TfpgCoord;
  totalRowHeight: TfpgCoord;
  startY: TfpgCoord;
  bounds: TfpgRect;
begin
  if not (AContainer is TfpgWidget) then Exit;

  Iterator := GetIterator(AContainer);
  if not Assigned(Iterator) then Exit;

  bounds := (AContainer as TfpgWidget).GetClientRect;
  ContainerWidth := bounds.Width;
  ContainerHeight := bounds.Height;
  SetLength(Rows, 1);
  rowIdx := 0;
  Rows[rowIdx].Width := 0;
  Rows[rowIdx].Height := 0;
  Rows[rowIdx].ComponentCount := 0;

  // First pass: Calculate row dimensions
  while Iterator.HasNext do
  begin
    w := Iterator.Next as TfpgWidget;
    prefSize := w.PreferredSize;

    if (Rows[rowIdx].Width > 0) and (Rows[rowIdx].Width + FHGap + prefSize.W > ContainerWidth - FHGap * 2) then
    begin
      Inc(rowIdx);
      SetLength(Rows, rowIdx + 1);
      Rows[rowIdx].Width := 0;
      Rows[rowIdx].Height := 0;
      Rows[rowIdx].ComponentCount := 0;
    end;

    if Rows[rowIdx].Width > 0 then
      Rows[rowIdx].Width := Rows[rowIdx].Width + FHGap;
    Rows[rowIdx].Width := Rows[rowIdx].Width + prefSize.W;
    if prefSize.H > Rows[rowIdx].Height then
      Rows[rowIdx].Height := prefSize.H;
    Inc(Rows[rowIdx].ComponentCount);
  end;

  // Calculate total height for vertical alignment
  totalRowHeight := 0;
  for i := 0 to rowIdx do
    totalRowHeight := totalRowHeight + Rows[i].Height;
  if rowIdx > 0 then
    totalRowHeight := totalRowHeight + rowIdx * FVGap;

  // Determine starting Y position
  case FVAlignment of
    flvaTop: startY := FVGap;
    flvaCenter: startY := (ContainerHeight - totalRowHeight) div 2;
    flvaBottom: startY := ContainerHeight - totalRowHeight - FVGap;
  else
    startY := FVGap;
  end;
  if startY < FVGap then startY := FVGap;

  // Second pass: Position components
  Iterator := GetIterator(AContainer); // get new iterator for second pass
  y := startY;
  for i := 0 to rowIdx do
  begin
    rowWidth := Rows[i].Width;

    // Determine starting X for the row based on alignment
    case FAlignment of
      flaLeft: rowStartX := FHGap;
      flaCenter: rowStartX := (ContainerWidth - rowWidth) div 2;
      flaRight: rowStartX := ContainerWidth - rowWidth - FHGap;
      //flaLeading: rowStartX := 0; // Assuming LTR for now
      //flaTrailing: rowStartX := ContainerWidth - rowWidth; // Assuming LTR
    else
      rowStartX := FHGap;
    end;
    if rowStartX < FHGap then rowStartX := FHGap;

    x := rowStartX;
    rowMaxHeight := Rows[i].Height;

    for j := 1 to Rows[i].ComponentCount do
    begin
      w := Iterator.Next as TfpgWidget;
      prefSize := w.PreferredSize;
      (w as ILayoutTarget).MoveAndResize(x, y, prefSize.W, prefSize.H);
      x := x + prefSize.W + FHGap;
    end;

    y := y + rowMaxHeight + FVGap;
  end;
end;

function TfpgFlowLayoutManager.CreateDefaultConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint;
begin
  Result := TfpgLayoutConstraint.Create;
end;

function TfpgFlowLayoutManager.DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize;
var
  Iterator: ILayoutIterator;
  w: TfpgWidget;
  prefSize: TfpgSize;
  maxWidth: TfpgCoord;
  totalHeight: TfpgCoord;
  rowWidth: TfpgCoord;
  rowHeight: TfpgCoord;
  ContainerWidth: TfpgCoord;
  bounds: TfpgRect;
begin
  if not (AContainer is TfpgWidget) then
  begin
    Result.SetSize(0, 0);
    Exit;
  end;

  // Get container's actual width to calculate wrapping
  bounds := (AContainer as TfpgWidget).GetClientRect;
  ContainerWidth := bounds.Width;

  // If container width is not yet set (e.g., during initial construction),
  // use a reasonable default to allow multi-row layout
  if ContainerWidth <= 0 then
    ContainerWidth := 400;  // Default reasonable width for wrapping

  Iterator := GetIterator(AContainer);
  maxWidth := 0;
  totalHeight := 0;
  rowWidth := 0;
  rowHeight := 0;

  while Iterator.HasNext do
  begin
    w := Iterator.Next as TfpgWidget;
    prefSize := w.PreferredSize;

    // Check if adding this widget would exceed container width (need to wrap)
    if (rowWidth > 0) and (rowWidth + FHGap + prefSize.W > ContainerWidth - FHGap * 2) then
    begin
      // Finish current row
      if rowWidth > maxWidth then
        maxWidth := rowWidth;
      totalHeight := totalHeight + rowHeight + FVGap;

      // Start new row
      rowWidth := prefSize.W;
      rowHeight := prefSize.H;
    end
    else
    begin
      // Add to current row
      if rowWidth > 0 then
        rowWidth := rowWidth + FHGap;
      rowWidth := rowWidth + prefSize.W;
      if prefSize.H > rowHeight then
        rowHeight := prefSize.H;
    end;
  end;

  // Finish last row
  if rowWidth > maxWidth then
    maxWidth := rowWidth;
  totalHeight := totalHeight + rowHeight;

  Result.SetSize(maxWidth + FHGap * 2, totalHeight + FVGap * 2);
end;

function TfpgFlowLayoutManager.DoGetMinimumSize(AContainer: TfpgWidgetBase): TfpgSize;
var
  Iterator: ILayoutIterator;
  w: TfpgWidget;
  prefSize: TfpgSize;
  maxWidth: TfpgCoord;
  maxHeight: TfpgCoord;
begin
  // For a flow layout, the minimum size is the size needed to display
  // the largest single widget (since everything else can wrap)
  if not (AContainer is TfpgWidget) then
  begin
    Result.SetSize(0, 0);
    Exit;
  end;

  Iterator := GetIterator(AContainer);
  maxWidth := 0;
  maxHeight := 0;

  while Iterator.HasNext do
  begin
    w := Iterator.Next as TfpgWidget;
    prefSize := w.PreferredSize;
    if prefSize.W > maxWidth then
      maxWidth := prefSize.W;
    if prefSize.H > maxHeight then
      maxHeight := prefSize.H;
  end;

  // Add gaps/padding for the minimum size
  Result.SetSize(maxWidth + FHGap * 2, maxHeight + FVGap * 2);
end;

end.
