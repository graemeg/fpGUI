unit vertex.wgt.canvas;

{
  TVertexCanvasWidget — renders the HVIF icon from TVertexDocument and provides
  interactive path editing via drag of anchor nodes and Bezier handles.

  Step #5: interactive editing wired to TVertexDocument + undo stack.
  Step #6: node add (click on segment) and delete (Delete/Backspace key).

  Rendering:
    Document → BuildWriter → TMemoryStream → THvifIcon → GetImage(sz,sz)
    Rebuilt only when FIconDirty is set (document changed, resize, load).

  Control overlay:
    Shown for the currently selected shape (set via SelectedShapeIndex property).
    All paths belonging to that shape get anchor nodes + handle circles + arm lines.

  Drag model (snapshot-on-down, live-update, commit-on-up):
    MouseDown  — snapshot FDragPtBefore; record which node/handle was hit.
    MouseMove  — update path point directly (bypasses undo stack for live preview).
    MouseUp    — create TVertexCmdMoveNode / TVertexCmdMoveHandle with before/after
                 snapshots and call FDocument.UndoStack.Execute(cmd).

  Node add:
    Click on a bezier segment → de Casteljau split at hit parameter t.
    Creates TVertexCmdAddPoint which updates adjacent handles and inserts the node.

  Node delete:
    Delete or Backspace when a node is selected → TVertexCmdDeletePoint.
    Blocked when the path would have fewer than 2 nodes remaining.

  Coordinate mapping:
    Icon is rendered at FIconSZ × FIconSZ pixels, centred at (FIconOX, FIconOY).
    HVIF uses 0..64 units on each axis.
    Scale = FIconSZ / 64.0.
    ScreenX = FIconOX + Round(hvif_x * Scale).
    hvif_x  = (screen_x - FIconOX) / Scale.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  fpg_base, fpg_main, fpg_widget,
  fpg_hvif, fpg_hvif_writer,
  fpg_vertex_document;


type
  TDragTarget = (dtNone, dtAnchor, dtInHandle, dtOutHandle);
  TVertexCursorMoveEvent = procedure(Sender: TObject; AHvifX, AHvifY: Single) of object;

  { Active editing tool }
  TVertexToolMode = (
    tmSelect,      { click/drag to select and translate shapes }
    tmNode,        { drag path nodes and Bezier handles — default mode }
    tmPan,         { drag to pan the viewport }
    tmAddPoint,    { click on a segment to insert a node }
    tmDeletePoint, { click on a node to remove it }
    tmZoom         { left-click zoom in, Alt+click zoom out }
  );

  TVertexCanvasWidget = class(TfpgWidget)
  private
    { Data (not owned) }
    FDocument: TVertexDocument;

    { Rendered image cache (owned) }
    FIcon:      THvifIcon;
    FIconDirty: Boolean;

    { Icon geometry — updated in UpdateIconGeometry, used by coord helpers }
    FIconOX, FIconOY: Integer;
    FIconSZ:          Integer;
    FScale:           Single;

    { Selection }
    FSelectedShapeIdx: Integer;   { -1 = none }
    FSelectedNodeIdx:  Integer;   { -1 = none; index within FActivePath }
    FActivePath:       TVertexPath;  { nil = none; which path owns the selected node }

    { Drag state }
    FDragTarget:   TDragTarget;
    FDragOffX, FDragOffY: Integer;  { mouse offset from exact node screen position }
    FDragPtBefore: TVertexPoint;       { snapshot of the point at drag-start }

    { Tool mode }
    FToolMode: TVertexToolMode;

    { Pan offset (pixels) — applied on top of centred geometry when zoomed }
    FPanX, FPanY: Integer;

    { Pan-tool drag state }
    FPanDragActive: Boolean;
    FPanDragStartX, FPanDragStartY: Integer;
    FPanDragOX, FPanDragOY: Integer;

    { Select-mode drag state }
    FSelectDragActive: Boolean;
    FSelectDragStartX, FSelectDragStartY: Integer;   { screen px at drag start }
    FSelectDragShapeIdx: Integer;                    { shape being dragged, -1=none }
    FSelectDragShapeOX, FSelectDragShapeOY: Single;  { shape translation at drag start }

    { Zoom — negative means "fit to widget" }
    FZoom: Integer;        { -1 = fit; 50/100/200/400 = fixed % }

    { Grid }
    FShowGrid:  Boolean;
    FGridStep:  Integer;   { grid step in HVIF units; default 8 }
    FSnapToGrid: Boolean;

    { Events }
    FOnCursorMove:    TVertexCursorMoveEvent;
    FOnShapeSelected: TNotifyEvent;   { fires when user clicks a shape in the canvas }

    { Coordinate helpers }
    function  HvifToScreenX(AHvif: Single): Integer;
    function  HvifToScreenY(AHvif: Single): Integer;
    function  ScreenToHvifX(AScreen: Integer): Single;
    function  ScreenToHvifY(AScreen: Integer): Single;

    { Rendering helpers }
    procedure UpdateIconGeometry;
    procedure RebuildIcon;
    procedure DrawCheckerboard;
    procedure DrawHvifImage;
    procedure DrawGrid;
    procedure DrawEmptyHint;
    procedure DrawShapeBoundingBox;
    procedure DrawNodeOverlayForPath(APath: TVertexPath);
    procedure DrawControlOverlay;
    procedure DrawNodeCircle(AScreenX, AScreenY, ARadius: Integer;
                              AFill, ABorder: TfpgColor; ASelected: Boolean);

    { Hit-testing }
    function HitTestNodes(AX, AY: Integer; out APath: TVertexPath;
                          out ANodeIdx: Integer;
                          out ATarget: TDragTarget): Boolean;
    function HitTestSegments(AX, AY: Integer; out APath: TVertexPath;
                             out ASegmentIdx: Integer;
                             out AT: Single): Boolean;

    { Returns the index of the topmost shape whose path bounding-box contains
      the given screen coordinates, or -1 if none. }
    function HitTestShapeBBox(AX, AY: Integer): Integer;

    procedure SetSelectedShapeIndex(AValue: Integer);
    procedure SelectShapeByClick(AIndex: Integer);   { sets index + fires OnShapeSelected }
    procedure SetZoom(AValue: Integer);
    procedure SetShowGrid(AValue: Boolean);
    procedure SetGridStep(AValue: Integer);
    function  SnapCoord(AHvif: Single): Single;

  protected
    procedure HandlePaint; override;
    procedure HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure HandleMouseMove(x, y: integer; btnstate: word;
                              shiftstate: TShiftState); override;
    procedure HandleKeyPress(var keycode: word; var shiftstate: TShiftState;
                             var consumed: boolean); override;
    procedure HandleResize(awidth, aheight: TfpgCoord); override;
    procedure HandleMouseScroll(x, y: integer; shiftstate: TShiftState;
                                delta: smallint); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    { Assign the document to render and edit. Pass nil to clear. }
    procedure SetDocument(ADoc: TVertexDocument);

    { Call after an external change (undo/redo from main form menu). }
    procedure DocumentChanged;

    property SelectedShapeIndex: Integer
        read FSelectedShapeIdx write SetSelectedShapeIndex;

    { Set the path whose nodes are shown in path-edit mode.
      Pass nil to clear node editing state. }
    procedure SetEditPath(APath: TVertexPath);

    { Zoom level: -1 = fit to widget; 50 / 100 / 200 / 400 = fixed percentage.
      Changing this repaints the canvas. }
    property Zoom: Integer read FZoom write SetZoom;

    { Active tool mode — determines how mouse events are interpreted. }
    property ToolMode: TVertexToolMode read FToolMode write FToolMode;

    { Grid overlay — when ShowGrid is True, draws a grid at GridStep HVIF-unit intervals. }
    property ShowGrid:  Boolean read FShowGrid  write SetShowGrid;
    property GridStep:  Integer read FGridStep  write SetGridStep;
    property SnapToGrid: Boolean read FSnapToGrid write FSnapToGrid;

    { Fires on every mouse-move, passing cursor position in HVIF units (0–64). }
    property OnCursorMove: TVertexCursorMoveEvent
        read FOnCursorMove write FOnCursorMove;

    { Fires when the user clicks a shape in the canvas, changing the selection.
      The form should respond by syncing the tree and property panels. }
    property OnShapeSelected: TNotifyEvent
        read FOnShapeSelected write FOnShapeSelected;
  end;


implementation

const
  CHECKER_CELL  = 8;
  HIT_RADIUS    = 6;
  NODE_RADIUS   = 5;
  HANDLE_RADIUS = 3;
  SEG_SAMPLES   = 20;   { bezier segment hit-test sample count }
  MIN_PATH_NODES = 2;   { minimum nodes allowed; blocks delete below this }

  COL_CHECKER1:    TfpgColor = $FFCCCCCC;
  COL_CHECKER2:    TfpgColor = $FFAAAAAA;
  COL_BORDER:      TfpgColor = $FF999999;
  COL_BG:          TfpgColor = $FFE8E8E8;
  COL_ANCHOR_FILL: TfpgColor = $FF4488FF;
  COL_ANCHOR_SEL:  TfpgColor = $FFFF8800;
  COL_HANDLE_FILL: TfpgColor = $FFFF44FF;
  COL_HANDLE_ARM:  TfpgColor = $FF888888;


{ ── De Casteljau bezier split ────────────────────────────────────────────── }

{ Split the cubic bezier P0-P1-P2-P3 at parameter t.
  P0 = preceding anchor, P1 = its OutHandle,
  P2 = following InHandle, P3 = following anchor.
  All values in HVIF 64-unit space.

  Outputs:
    SplitX/Y  — new anchor on the curve at t
    APrevOutX/Y — updated OutHandle for the preceding node  (was P1)
    ANewInX/Y   — incoming handle of the new node
    ANewOutX/Y  — outgoing handle of the new node
    ANextInX/Y  — updated InHandle for the following node  (was P2) }
procedure SplitCubicBezier(
    P0x, P0y, P1x, P1y, P2x, P2y, P3x, P3y, t: Single;
    out SplitX, SplitY: Single;
    out APrevOutX, APrevOutY: Single;
    out ANewInX,  ANewInY:   Single;
    out ANewOutX, ANewOutY:  Single;
    out ANextInX, ANextInY:  Single);
var
  Q0x, Q0y, Q1x, Q1y, Q2x, Q2y: Single;
  R0x, R0y, R1x, R1y: Single;
begin
  { Level 1 }
  Q0x := P0x + t * (P1x - P0x);   Q0y := P0y + t * (P1y - P0y);
  Q1x := P1x + t * (P2x - P1x);   Q1y := P1y + t * (P2y - P1y);
  Q2x := P2x + t * (P3x - P2x);   Q2y := P2y + t * (P3y - P2y);
  { Level 2 }
  R0x := Q0x + t * (Q1x - Q0x);   R0y := Q0y + t * (Q1y - Q0y);
  R1x := Q1x + t * (Q2x - Q1x);   R1y := Q1y + t * (Q2y - Q1y);
  { Level 3 = split point }
  SplitX := R0x + t * (R1x - R0x);  SplitY := R0y + t * (R1y - R0y);
  { Updated handles }
  APrevOutX := Q0x;   APrevOutY := Q0y;
  ANewInX   := R0x;   ANewInY   := R0y;
  ANewOutX  := R1x;   ANewOutY  := R1y;
  ANextInX  := Q2x;   ANextInY  := Q2y;
end;


{ ── TVertexCanvasWidget ─────────────────────────────────────────────────────── }

constructor TVertexCanvasWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Focusable         := True;
  FDocument         := nil;
  FIcon             := nil;
  FIconDirty        := False;
  FSelectedShapeIdx := -1;
  FSelectedNodeIdx  := -1;
  FActivePath       := nil;
  FDragTarget       := dtNone;
  FScale            := 4.0;   { default: 256px / 64 units }
  FZoom             := -1;    { -1 = fit to widget }
  FShowGrid         := False;
  FGridStep         := 8;
  FSnapToGrid       := False;
  FToolMode         := tmNode;
  FSelectDragActive := False;
  FSelectDragShapeIdx := -1;
  FPanX             := 0;
  FPanY             := 0;
  FPanDragActive    := False;
end;

destructor TVertexCanvasWidget.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;

procedure TVertexCanvasWidget.SetDocument(ADoc: TVertexDocument);
begin
  if FDocument = ADoc then
    Exit;
  FDocument := ADoc;
  FSelectedShapeIdx := -1;
  FSelectedNodeIdx  := -1;
  FActivePath       := nil;
  FDragTarget       := dtNone;
  FreeAndNil(FIcon);
  FIconDirty := (FDocument <> nil);
  Repaint;
end;

procedure TVertexCanvasWidget.DocumentChanged;
begin
  FIconDirty := True;
  Repaint;
end;

procedure TVertexCanvasWidget.SetSelectedShapeIndex(AValue: Integer);
begin
  if FSelectedShapeIdx = AValue then
    Exit;
  FSelectedShapeIdx := AValue;
  FSelectedNodeIdx  := -1;
  FActivePath       := nil;
  FDragTarget       := dtNone;
  Repaint;
end;

procedure TVertexCanvasWidget.SelectShapeByClick(AIndex: Integer);
begin
  SetSelectedShapeIndex(AIndex);
  if Assigned(FOnShapeSelected) then
    FOnShapeSelected(Self);
end;

procedure TVertexCanvasWidget.SetEditPath(APath: TVertexPath);
begin
  FActivePath      := APath;
  FSelectedNodeIdx := -1;
  FDragTarget      := dtNone;
  Repaint;
end;

procedure TVertexCanvasWidget.SetZoom(AValue: Integer);
begin
  if FZoom = AValue then
    Exit;
  FZoom := AValue;
  if FZoom < 0 then
  begin
    FPanX := 0;
    FPanY := 0;
  end;
  UpdateIconGeometry;
  FIconDirty := True;
  Repaint;
end;

procedure TVertexCanvasWidget.SetShowGrid(AValue: Boolean);
begin
  if FShowGrid = AValue then Exit;
  FShowGrid := AValue;
  Repaint;
end;

procedure TVertexCanvasWidget.SetGridStep(AValue: Integer);
begin
  if AValue < 1 then AValue := 1;
  if FGridStep = AValue then Exit;
  FGridStep := AValue;
  if FShowGrid then
    Repaint;
end;

function TVertexCanvasWidget.SnapCoord(AHvif: Single): Single;
begin
  if FSnapToGrid and (FGridStep > 0) then
    Result := Round(AHvif / FGridStep) * FGridStep
  else
    Result := AHvif;
end;

{ ── Coordinate helpers ───────────────────────────────────────────────────── }

function TVertexCanvasWidget.HvifToScreenX(AHvif: Single): Integer;
begin
  Result := FIconOX + Round(AHvif * FScale);
end;

function TVertexCanvasWidget.HvifToScreenY(AHvif: Single): Integer;
begin
  Result := FIconOY + Round(AHvif * FScale);
end;

function TVertexCanvasWidget.ScreenToHvifX(AScreen: Integer): Single;
begin
  if FScale > 0 then
    Result := (AScreen - FIconOX) / FScale
  else
    Result := 0;
end;

function TVertexCanvasWidget.ScreenToHvifY(AScreen: Integer): Single;
begin
  if FScale > 0 then
    Result := (AScreen - FIconOY) / FScale
  else
    Result := 0;
end;

procedure TVertexCanvasWidget.UpdateIconGeometry;
var
  sz: Integer;
begin
  if FZoom < 0 then
  begin
    { Fit mode: scale to fill the widget with a 40px margin }
    sz := Min(Width - 40, Height - 40);
    if sz < 16 then sz := 16;
    sz := (sz div 4) * 4;
  end
  else
  begin
    { Fixed zoom: 100% = 256px (4 px per HVIF unit) }
    sz := (256 * FZoom) div 100;
  end;
  FIconSZ := sz;
  FIconOX := (Width  - FIconSZ) div 2 + FPanX;
  FIconOY := (Height - FIconSZ) div 2 + FPanY;
  FScale  := FIconSZ / 64.0;
end;


{ ── Rendering ────────────────────────────────────────────────────────────── }

procedure TVertexCanvasWidget.RebuildIcon;
var
  writer: THvifWriter;
  ms:     TMemoryStream;
begin
  FreeAndNil(FIcon);
  if FDocument = nil then
    Exit;
  writer := FDocument.BuildWriter;
  try
    ms := TMemoryStream.Create;
    try
      writer.SaveToStream(ms);
      ms.Position := 0;
      FIcon := THvifIcon.CreateFromStream(ms);
    finally
      ms.Free;
    end;
  finally
    writer.Free;
  end;
end;

procedure TVertexCanvasWidget.DrawCheckerboard;
var
  col, row: Integer;
begin
  col := 0;
  while col < FIconSZ do
  begin
    row := 0;
    while row < FIconSZ do
    begin
      if Odd((col div CHECKER_CELL) + (row div CHECKER_CELL)) then
        Canvas.SetColor(COL_CHECKER2)
      else
        Canvas.SetColor(COL_CHECKER1);
      Canvas.FillRectangle(FIconOX + col, FIconOY + row,
                           Min(CHECKER_CELL, FIconSZ - col),
                           Min(CHECKER_CELL, FIconSZ - row));
      Inc(row, CHECKER_CELL);
    end;
    Inc(col, CHECKER_CELL);
  end;
end;

procedure TVertexCanvasWidget.DrawGrid;
var
  step, sx, sy: Integer;
  hvifStep: Single;
begin
  if not FShowGrid or (FGridStep <= 0) or (FScale <= 0) then
    Exit;
  hvifStep := FGridStep;
  step     := Round(hvifStep * FScale);
  if step < 2 then
    Exit;  { grid too dense to be visible }

  Canvas.SetColor($C0C0C0C0);  { light grey, semi-transparent hint }

  { Vertical lines }
  sx := 0;
  while sx <= 64 do
  begin
    Canvas.DrawLine(FIconOX + Round(sx * FScale), FIconOY,
                    FIconOX + Round(sx * FScale), FIconOY + FIconSZ);
    sx := sx + FGridStep;
  end;

  { Horizontal lines }
  sy := 0;
  while sy <= 64 do
  begin
    Canvas.DrawLine(FIconOX, FIconOY + Round(sy * FScale),
                    FIconOX + FIconSZ, FIconOY + Round(sy * FScale));
    sy := sy + FGridStep;
  end;
end;

procedure TVertexCanvasWidget.DrawHvifImage;
var
  img: TfpgImage;
begin
  if FIcon = nil then
    Exit;
  img := FIcon.GetImage(FIconSZ, FIconSZ);
  if img <> nil then
    Canvas.DrawImage(FIconOX, FIconOY, img);
end;

procedure TVertexCanvasWidget.DrawEmptyHint;
begin
  Canvas.SetTextColor($FF888888);
  Canvas.DrawString(Width div 2 - 90, Height div 2 - 8,
      'File > Open to load an HVIF icon');
end;

procedure TVertexCanvasWidget.DrawNodeCircle(AScreenX, AScreenY, ARadius: Integer;
    AFill, ABorder: TfpgColor; ASelected: Boolean);
var
  bx, by, bd: Integer;
begin
  bx := AScreenX - ARadius;
  by := AScreenY - ARadius;
  bd := ARadius * 2;
  Canvas.SetColor(AFill);
  Canvas.FillArc(bx, by, bd, bd, 0, 360);
  if ASelected then
    Canvas.SetColor(COL_ANCHOR_SEL)
  else
    Canvas.SetColor(ABorder);
  Canvas.DrawArc(bx, by, bd, bd, 0, 360);
end;

procedure TVertexCanvasWidget.DrawShapeBoundingBox;
const
  COL_BBOX    = $FF0080FF;   { bright blue }
  DASH_ON     = 6;           { pixels drawn }
  DASH_OFF    = 4;           { pixels skipped }
  PAD         = 4;           { screen-pixel padding around the tight bbox }

  procedure DashedHLine(x1, x2, y: Integer);
  var x, phase: Integer;
  begin
    x := x1; phase := 0;
    while x <= x2 do
    begin
      if phase < DASH_ON then
        Canvas.DrawLine(x, y, x + 1, y);
      Inc(x);
      Inc(phase);
      if phase >= DASH_ON + DASH_OFF then phase := 0;
    end;
  end;

  procedure DashedVLine(x, y1, y2: Integer);
  var y, phase: Integer;
  begin
    y := y1; phase := 0;
    while y <= y2 do
    begin
      if phase < DASH_ON then
        Canvas.DrawLine(x, y, x, y + 1);
      Inc(y);
      Inc(phase);
      if phase >= DASH_ON + DASH_OFF then phase := 0;
    end;
  end;

var
  shape: TVertexShape;
  pi, ni: Integer;
  path: TVertexPath;
  pt: TVertexPoint;
  minHX, minHY, maxHX, maxHY: Single;
  first: Boolean;
  sx1, sy1, sx2, sy2: Integer;

  procedure ExpandBy(hx, hy: Single);
  begin
    if first then
    begin
      minHX := hx; maxHX := hx;
      minHY := hy; maxHY := hy;
      first := False;
    end
    else
    begin
      if hx < minHX then minHX := hx;
      if hx > maxHX then maxHX := hx;
      if hy < minHY then minHY := hy;
      if hy > maxHY then maxHY := hy;
    end;
  end;

begin
  if (FDocument = nil) or (FSelectedShapeIdx < 0) or
     (FSelectedShapeIdx >= FDocument.ShapeCount) then Exit;

  shape := FDocument.Shapes[FSelectedShapeIdx];
  first := True;

  { Collect bounding box over all anchor points and Bezier handles of all paths,
    then apply shape translation offset. }
  for pi := 0 to shape.PathCount - 1 do
  begin
    path := shape.Paths[pi];
    for ni := 0 to path.PointCount - 1 do
    begin
      pt := path.Points[ni];
      ExpandBy(pt.X, pt.Y);
      ExpandBy(pt.InX, pt.InY);
      ExpandBy(pt.OutX, pt.OutY);
    end;
  end;

  if first then Exit;  { no points found }

  { Apply shape translation (affine matrix support to follow) }
  if shape.HasTranslation then
  begin
    minHX := minHX + shape.TranslateX;  maxHX := maxHX + shape.TranslateX;
    minHY := minHY + shape.TranslateY;  maxHY := maxHY + shape.TranslateY;
  end;

  { Convert to screen coordinates and add padding }
  sx1 := HvifToScreenX(minHX) - PAD;
  sy1 := HvifToScreenY(minHY) - PAD;
  sx2 := HvifToScreenX(maxHX) + PAD;
  sy2 := HvifToScreenY(maxHY) + PAD;

  Canvas.SetColor(COL_BBOX);
  DashedHLine(sx1, sx2, sy1);   { top }
  DashedHLine(sx1, sx2, sy2);   { bottom }
  DashedVLine(sx1, sy1, sy2);   { left }
  DashedVLine(sx2, sy1, sy2);   { right }

  { Corner handles — small solid squares at each corner }
  Canvas.FillRectangle(sx1 - 3, sy1 - 3, 7, 7);
  Canvas.FillRectangle(sx2 - 3, sy1 - 3, 7, 7);
  Canvas.FillRectangle(sx1 - 3, sy2 - 3, 7, 7);
  Canvas.FillRectangle(sx2 - 3, sy2 - 3, 7, 7);

  { Edge midpoint handles — small solid squares at mid-points of each edge }
  Canvas.FillRectangle((sx1 + sx2) div 2 - 3, sy1 - 3, 7, 7);   { top mid }
  Canvas.FillRectangle((sx1 + sx2) div 2 - 3, sy2 - 3, 7, 7);   { bottom mid }
  Canvas.FillRectangle(sx1 - 3, (sy1 + sy2) div 2 - 3, 7, 7);   { left mid }
  Canvas.FillRectangle(sx2 - 3, (sy1 + sy2) div 2 - 3, 7, 7);   { right mid }
end;

procedure TVertexCanvasWidget.DrawNodeOverlayForPath(APath: TVertexPath);
var
  ni: Integer;
  pt: TVertexPoint;
  ax, ay, ihx, ihy, ohx, ohy: Integer;
  isSelNode: Boolean;
begin
  for ni := 0 to APath.PointCount - 1 do
  begin
    pt  := APath.Points[ni];
    ax  := HvifToScreenX(pt.X);
    ay  := HvifToScreenY(pt.Y);
    ihx := HvifToScreenX(pt.InX);
    ihy := HvifToScreenY(pt.InY);
    ohx := HvifToScreenX(pt.OutX);
    ohy := HvifToScreenY(pt.OutY);
    isSelNode := (APath = FActivePath) and (ni = FSelectedNodeIdx);
    Canvas.SetColor(COL_HANDLE_ARM);
    Canvas.DrawLine(ax, ay, ihx, ihy);
    Canvas.DrawLine(ax, ay, ohx, ohy);
    DrawNodeCircle(ihx, ihy, HANDLE_RADIUS, COL_HANDLE_FILL, $FF000000,
                   isSelNode and (FDragTarget = dtInHandle));
    DrawNodeCircle(ohx, ohy, HANDLE_RADIUS, COL_HANDLE_FILL, $FF000000,
                   isSelNode and (FDragTarget = dtOutHandle));
    DrawNodeCircle(ax, ay, NODE_RADIUS, COL_ANCHOR_FILL, $FF000000,
                   isSelNode);
  end;
end;

procedure TVertexCanvasWidget.DrawControlOverlay;
var
  shape: TVertexShape;
  pi: Integer;
begin
  if FDocument = nil then Exit;

  { Shape-transform mode: draw bounding box with handles, no node overlay. }
  if FToolMode = tmSelect then
  begin
    DrawShapeBoundingBox;
    Exit;
  end;

  { Path-edit mode: draw nodes for the active path only.
    FActivePath is set by SetEditPath when the user selects a path in the tree. }
  if FActivePath <> nil then
  begin
    DrawNodeOverlayForPath(FActivePath);
    Exit;
  end;

  { Fallback (Node tool active but no path explicitly selected in tree):
    draw nodes for all paths of the selected shape so the user can click one. }
  if (FSelectedShapeIdx < 0) or (FSelectedShapeIdx >= FDocument.ShapeCount) then
    Exit;
  shape := FDocument.Shapes[FSelectedShapeIdx];
  for pi := 0 to shape.PathCount - 1 do
    DrawNodeOverlayForPath(shape.Paths[pi]);
end;

procedure TVertexCanvasWidget.HandlePaint;
begin
  Canvas.BeginDraw;
  try
    Canvas.SetColor(COL_BG);
    Canvas.FillRectangle(0, 0, Width, Height);

    if FDocument = nil then
    begin
      DrawEmptyHint;
      Exit;
    end;

    UpdateIconGeometry;

    if FIconDirty then
    begin
      RebuildIcon;
      FIconDirty := False;
    end;

    DrawCheckerboard;
    DrawHvifImage;
    DrawGrid;
    Canvas.SetColor(COL_BORDER);
    Canvas.DrawRectangle(FIconOX - 1, FIconOY - 1, FIconSZ + 2, FIconSZ + 2);
    DrawControlOverlay;
  finally
    Canvas.EndDraw;
  end;
end;

procedure TVertexCanvasWidget.HandleResize(awidth, aheight: TfpgCoord);
begin
  inherited HandleResize(awidth, aheight);
  if FIcon <> nil then
    FIcon.ClearCache;
  FIconDirty := (FDocument <> nil);
  Repaint;
end;


{ ── Hit-testing ──────────────────────────────────────────────────────────── }

function TVertexCanvasWidget.HitTestNodes(AX, AY: Integer; out APath: TVertexPath;
    out ANodeIdx: Integer; out ATarget: TDragTarget): Boolean;
var
  shape: TVertexShape;
  pi: Integer;

  procedure TestOnePath(ATestPath: TVertexPath);
  var
    lni: Integer;
    lpt: TVertexPoint;
    lsx, lsy, ldx, ldy: Integer;
  begin
    for lni := 0 to ATestPath.PointCount - 1 do
    begin
      lpt := ATestPath.Points[lni];
      lsx := HvifToScreenX(lpt.X);  lsy := HvifToScreenY(lpt.Y);
      ldx := AX - lsx; ldy := AY - lsy;
      if (ldx*ldx + ldy*ldy) <= (HIT_RADIUS*HIT_RADIUS) then
      begin
        APath := ATestPath; ANodeIdx := lni; ATarget := dtAnchor;
        Result := True; Exit;
      end;
      lsx := HvifToScreenX(lpt.InX); lsy := HvifToScreenY(lpt.InY);
      ldx := AX - lsx; ldy := AY - lsy;
      if (ldx*ldx + ldy*ldy) <= (HIT_RADIUS*HIT_RADIUS) then
      begin
        APath := ATestPath; ANodeIdx := lni; ATarget := dtInHandle;
        Result := True; Exit;
      end;
      lsx := HvifToScreenX(lpt.OutX); lsy := HvifToScreenY(lpt.OutY);
      ldx := AX - lsx; ldy := AY - lsy;
      if (ldx*ldx + ldy*ldy) <= (HIT_RADIUS*HIT_RADIUS) then
      begin
        APath := ATestPath; ANodeIdx := lni; ATarget := dtOutHandle;
        Result := True; Exit;
      end;
    end;
  end;

begin
  Result := False;
  APath := nil; ANodeIdx := -1; ATarget := dtNone;
  if FDocument = nil then Exit;

  { When a specific path is active (selected in tree), only hit-test that path. }
  if FActivePath <> nil then
  begin
    TestOnePath(FActivePath);
    Exit;
  end;

  { Fallback: test all paths of the selected shape. }
  if (FSelectedShapeIdx < 0) or (FSelectedShapeIdx >= FDocument.ShapeCount) then
    Exit;
  shape := FDocument.Shapes[FSelectedShapeIdx];
  for pi := 0 to shape.PathCount - 1 do
  begin
    TestOnePath(shape.Paths[pi]);
    if Result then Exit;
  end;
end;

function TVertexCanvasWidget.HitTestSegments(AX, AY: Integer;
    out APath: TVertexPath; out ASegmentIdx: Integer; out AT: Single): Boolean;
var
  shape: TVertexShape;
  pi: Integer;
  bestDist2: Single;   { shared with TestOneSeg via closure }

  procedure TestOneSeg(ATestPath: TVertexPath);
  var
    lni, lsi, lsegCount: Integer;
    ln0, ln1: TVertexPoint;
    lt, lbx, lby, ldx, ldy, ldist2: Single;
    lsx0, lsy0, lsx1, lsy1, lsx2, lsy2, lsx3, lsy3: Single;
    lu: Single;
  begin
    if ATestPath.PointCount < 2 then Exit;
    if ATestPath.Closed then
      lsegCount := ATestPath.PointCount
    else
      lsegCount := ATestPath.PointCount - 1;
    for lni := 0 to lsegCount - 1 do
    begin
      ln0 := ATestPath.Points[lni];
      ln1 := ATestPath.Points[(lni + 1) mod ATestPath.PointCount];
      lsx0 := HvifToScreenX(ln0.X);    lsy0 := HvifToScreenY(ln0.Y);
      lsx1 := HvifToScreenX(ln0.OutX); lsy1 := HvifToScreenY(ln0.OutY);
      lsx2 := HvifToScreenX(ln1.InX);  lsy2 := HvifToScreenY(ln1.InY);
      lsx3 := HvifToScreenX(ln1.X);    lsy3 := HvifToScreenY(ln1.Y);
      for lsi := 1 to SEG_SAMPLES - 1 do
      begin
        lt := lsi / SEG_SAMPLES;
        lu := 1.0 - lt;
        lbx := lu*lu*lu*lsx0 + 3.0*lu*lu*lt*lsx1 + 3.0*lu*lt*lt*lsx2 + lt*lt*lt*lsx3;
        lby := lu*lu*lu*lsy0 + 3.0*lu*lu*lt*lsy1 + 3.0*lu*lt*lt*lsy2 + lt*lt*lt*lsy3;
        ldx := AX - lbx;  ldy := AY - lby;
        ldist2 := ldx*ldx + ldy*ldy;
        if ldist2 < bestDist2 then
        begin
          bestDist2   := ldist2;
          APath       := ATestPath;
          ASegmentIdx := lni;
          AT          := lt;
          Result      := True;
        end;
      end;
    end;
  end;

begin
  Result := False;
  APath := nil; ASegmentIdx := -1; AT := 0;
  if FDocument = nil then Exit;
  bestDist2 := Sqr(HIT_RADIUS);

  { When a specific path is active (selected in tree), only test that path. }
  if FActivePath <> nil then
  begin
    TestOneSeg(FActivePath);
    Exit;
  end;

  { Fallback: test all paths of the selected shape. }
  if (FSelectedShapeIdx < 0) or (FSelectedShapeIdx >= FDocument.ShapeCount) then
    Exit;
  shape := FDocument.Shapes[FSelectedShapeIdx];
  for pi := 0 to shape.PathCount - 1 do
    TestOneSeg(shape.Paths[pi]);
end;


{ ── Keyboard handling ────────────────────────────────────────────────────── }

procedure TVertexCanvasWidget.HandleKeyPress(var keycode: word;
    var shiftstate: TShiftState; var consumed: boolean);
var
  cmd: TVertexCmdDeletePoint;
begin
  if (keycode = keyDelete) or (keycode = keyBackSpace) then
  begin
    if (FDocument <> nil) and (FSelectedNodeIdx >= 0) and
       (FActivePath <> nil) and
       (FActivePath.PointCount > MIN_PATH_NODES) then
    begin
      cmd := TVertexCmdDeletePoint.Create(FActivePath, FSelectedNodeIdx);
      { Adjust selection before firing the command so the overlay is consistent }
      if FSelectedNodeIdx >= FActivePath.PointCount - 1 then
        FSelectedNodeIdx := FActivePath.PointCount - 2
      else
        FSelectedNodeIdx := FSelectedNodeIdx;
      FActivePath  := nil;
      FSelectedNodeIdx := -1;
      FDocument.UndoStack.Execute(cmd);
      { Execute fires OnChange → form HandleDocumentChange → DocumentChanged → Repaint }
      consumed := True;
    end;
  end;
  if not consumed then
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;


procedure TVertexCanvasWidget.HandleMouseScroll(x, y: integer;
    shiftstate: TShiftState; delta: smallint);
const
  kZoomLevels: array[0..4] of Integer = (-1, 50, 100, 200, 400);
var
  curIdx, newIdx: Integer;
  i: Integer;
begin
  inherited HandleMouseScroll(x, y, shiftstate, delta);
  { Ctrl+scroll = zoom; plain scroll = pan vertically }
  if ssCtrl in shiftstate then
  begin
    { Find current zoom level in the table }
    curIdx := 0;
    for i := 0 to High(kZoomLevels) do
      if kZoomLevels[i] = FZoom then begin curIdx := i; Break; end;
    if delta > 0 then
      newIdx := Min(curIdx + 1, High(kZoomLevels))
    else
      newIdx := Max(curIdx - 1, 0);
    if newIdx <> curIdx then
      SetZoom(kZoomLevels[newIdx]);
  end
  else
  begin
    { Plain scroll — pan the viewport }
    if delta > 0 then
      Inc(FPanY, 16)
    else
      Dec(FPanY, 16);
    Repaint;
  end;
end;

function TVertexCanvasWidget.HitTestShapeBBox(AX, AY: Integer): Integer;
{ Hit-test in screen space using the same bbox as DrawShapeBoundingBox.
  A fixed screen-pixel margin (PAD = 4) is used so the clickable area
  exactly matches the drawn box regardless of zoom level. }
const
  PAD = 4;  { screen pixels — must match DrawShapeBoundingBox PAD constant }
var
  i, j, k: Integer;
  sh:       TVertexShape;
  ph:       TVertexPath;
  pt:       TVertexPoint;
  minHX, minHY, maxHX, maxHY: Single;
  first:    Boolean;
  sx1, sy1, sx2, sy2: Integer;
begin
  Result := -1;
  if FDocument = nil then Exit;

  { Iterate top-to-bottom so the topmost (last-drawn) shape wins on overlap }
  for i := FDocument.ShapeCount - 1 downto 0 do
  begin
    sh := FDocument.Shapes[i];
    if not sh.Visible then Continue;

    { Compute bbox over all anchors + Bezier handles across all paths,
      matching DrawShapeBoundingBox exactly. }
    first := True;
    minHX := 0; minHY := 0; maxHX := 0; maxHY := 0;

    for j := 0 to sh.PathCount - 1 do
    begin
      ph := sh.Paths[j];
      for k := 0 to ph.PointCount - 1 do
      begin
        pt := ph.Points[k];
        if first then
        begin
          minHX := pt.X;   maxHX := pt.X;
          minHY := pt.Y;   maxHY := pt.Y;
          first := False;
        end;
        if pt.X   < minHX then minHX := pt.X;
        if pt.X   > maxHX then maxHX := pt.X;
        if pt.Y   < minHY then minHY := pt.Y;
        if pt.Y   > maxHY then maxHY := pt.Y;
        if pt.InX < minHX then minHX := pt.InX;
        if pt.InX > maxHX then maxHX := pt.InX;
        if pt.InY < minHY then minHY := pt.InY;
        if pt.InY > maxHY then maxHY := pt.InY;
        if pt.OutX < minHX then minHX := pt.OutX;
        if pt.OutX > maxHX then maxHX := pt.OutX;
        if pt.OutY < minHY then minHY := pt.OutY;
        if pt.OutY > maxHY then maxHY := pt.OutY;
      end;
    end;

    if first then Continue;  { shape has no points }

    { Apply shape translation }
    if sh.HasTranslation then
    begin
      minHX := minHX + sh.TranslateX;  maxHX := maxHX + sh.TranslateX;
      minHY := minHY + sh.TranslateY;  maxHY := maxHY + sh.TranslateY;
    end;

    { Convert to screen coords + same PAD as the drawn box }
    sx1 := HvifToScreenX(minHX) - PAD;
    sy1 := HvifToScreenY(minHY) - PAD;
    sx2 := HvifToScreenX(maxHX) + PAD;
    sy2 := HvifToScreenY(maxHY) + PAD;

    if (AX >= sx1) and (AX <= sx2) and (AY >= sy1) and (AY <= sy2) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

{ ── Mouse event handling ─────────────────────────────────────────────────── }

procedure TVertexCanvasWidget.HandleLMouseDown(x, y: integer;
    shiftstate: TShiftState);
var
  hitPath:   TVertexPath;
  hitNode:   Integer;
  hitTarget: TDragTarget;
  segPath:   TVertexPath;
  segIdx:    Integer;
  segT:      Single;
  n0, n1:    TVertexPoint;
  prevIdx, nextIdx, insertIdx: Integer;
  newPt:     TVertexPoint;
  prevAfter, nextAfter: TVertexPoint;
  splitX, splitY: Single;
  prevOutX, prevOutY: Single;
  newInX,   newInY:   Single;
  newOutX,  newOutY:  Single;
  nextInX,  nextInY:  Single;
  cmd: TVertexCmdAddPoint;
  cmdDel: TVertexCmdDeletePoint;
  pt: TVertexPoint;
  shapeIdx: Integer;
  sh: TVertexShape;
  curIdxZ, newIdxZ, iZ: Integer;
begin
  { Claim keyboard focus so Delete/Backspace reach HandleKeyPress }
  SetFocus;

  { ── Pan-tool mode ───────────────────────────────────────────────────────── }
  if FToolMode = tmPan then
  begin
    FPanDragActive  := True;
    FPanDragStartX  := x;
    FPanDragStartY  := y;
    FPanDragOX      := FPanX;
    FPanDragOY      := FPanY;
    Exit;
  end;

  { ── Select-tool mode: click to select / drag to translate shape ─────────── }
  if FToolMode = tmSelect then
  begin
    shapeIdx := HitTestShapeBBox(x, y);
    if shapeIdx >= 0 then
    begin
      SelectShapeByClick(shapeIdx);   { updates canvas + notifies form to sync tree }
      sh := FDocument.Shapes[shapeIdx];
      FSelectDragActive    := True;
      FSelectDragStartX    := x;
      FSelectDragStartY    := y;
      FSelectDragShapeIdx  := shapeIdx;
      FSelectDragShapeOX   := sh.TranslateX;
      FSelectDragShapeOY   := sh.TranslateY;
    end;
    Exit;
  end;

  { ── Zoom-tool mode ─────────────────────────────────────────────────────── }
  if FToolMode = tmZoom then
  begin
    { Zoom levels: -1(fit), 50, 100, 200, 400 }
    curIdxZ := 0;
    if      FZoom = 50  then curIdxZ := 1
    else if FZoom = 100 then curIdxZ := 2
    else if FZoom = 200 then curIdxZ := 3
    else if FZoom = 400 then curIdxZ := 4;
    if ssAlt in shiftstate then
      newIdxZ := Max(curIdxZ - 1, 0)
    else
      newIdxZ := Min(curIdxZ + 1, 4);
    if newIdxZ <> curIdxZ then
    begin
      case newIdxZ of
        0: SetZoom(-1);
        1: SetZoom(50);
        2: SetZoom(100);
        3: SetZoom(200);
        4: SetZoom(400);
      end;
    end;
    Exit;
  end;

  { ── Add-point mode ───────────────────────────────────────────────────────── }
  if FToolMode = tmAddPoint then
  begin
    if (FDocument <> nil) and HitTestSegments(x, y, segPath, segIdx, segT) then
    begin
      prevIdx := segIdx;
      nextIdx := (segIdx + 1) mod segPath.PointCount;
      if (segPath.Closed) and (segIdx = segPath.PointCount - 1) then
        insertIdx := segPath.PointCount
      else
        insertIdx := segIdx + 1;
      n0 := segPath.Points[prevIdx];
      n1 := segPath.Points[nextIdx];
      SplitCubicBezier(
          n0.X, n0.Y, n0.OutX, n0.OutY,
          n1.InX, n1.InY, n1.X, n1.Y,
          segT,
          splitX, splitY,
          prevOutX, prevOutY,
          newInX,  newInY,
          newOutX, newOutY,
          nextInX, nextInY);
      newPt        := Default(TVertexPoint);
      newPt.X      := splitX;   newPt.Y      := splitY;
      newPt.InX    := newInX;   newPt.InY    := newInY;
      newPt.OutX   := newOutX;  newPt.OutY   := newOutY;
      newPt.Smooth := False;
      prevAfter      := n0;
      prevAfter.OutX := prevOutX;  prevAfter.OutY := prevOutY;
      nextAfter      := n1;
      nextAfter.InX  := nextInX;   nextAfter.InY  := nextInY;
      cmd := TVertexCmdAddPoint.Create(segPath, insertIdx, prevIdx, nextIdx,
                 newPt, n0, prevAfter, n1, nextAfter);
      FDocument.UndoStack.Execute(cmd);
      FActivePath      := segPath;
      FSelectedNodeIdx := insertIdx;
      FDragTarget      := dtNone;
      Repaint;
    end;
    Exit;
  end;

  { ── Delete-point mode ───────────────────────────────────────────────────── }
  if FToolMode = tmDeletePoint then
  begin
    if (FDocument <> nil) and HitTestNodes(x, y, hitPath, hitNode, hitTarget) then
    begin
      if (hitTarget = dtAnchor) and (hitPath.PointCount > MIN_PATH_NODES) then
      begin
        cmdDel := TVertexCmdDeletePoint.Create(hitPath, hitNode);
        FActivePath      := nil;
        FSelectedNodeIdx := -1;
        FDocument.UndoStack.Execute(cmdDel);
        Repaint;
      end;
    end;
    Exit;
  end;

  { ── Node-edit mode (default) ─────────────────────────────────────────────── }
  { 1. Try to hit an existing node or handle }
  if HitTestNodes(x, y, hitPath, hitNode, hitTarget) then
  begin
    pt := hitPath.Points[hitNode];
    FActivePath      := hitPath;
    FSelectedNodeIdx := hitNode;
    FDragTarget      := hitTarget;
    FDragPtBefore    := pt;

    case hitTarget of
      dtAnchor:
      begin
        FDragOffX := x - HvifToScreenX(pt.X);
        FDragOffY := y - HvifToScreenY(pt.Y);
      end;
      dtInHandle:
      begin
        FDragOffX := x - HvifToScreenX(pt.InX);
        FDragOffY := y - HvifToScreenY(pt.InY);
      end;
      dtOutHandle:
      begin
        FDragOffX := x - HvifToScreenX(pt.OutX);
        FDragOffY := y - HvifToScreenY(pt.OutY);
      end;
    end;
    Repaint;
    Exit;
  end;

  { 2. Try to hit a bezier segment → insert a new node }
  if (FDocument <> nil) and HitTestSegments(x, y, segPath, segIdx, segT) then
  begin
    prevIdx := segIdx;
    nextIdx := (segIdx + 1) mod segPath.PointCount;

    { For closed path last segment: insert at the end (append) }
    if (segPath.Closed) and (segIdx = segPath.PointCount - 1) then
      insertIdx := segPath.PointCount
    else
      insertIdx := segIdx + 1;

    n0 := segPath.Points[prevIdx];
    n1 := segPath.Points[nextIdx];

    { De Casteljau split in HVIF coordinate space }
    SplitCubicBezier(
        n0.X, n0.Y, n0.OutX, n0.OutY,
        n1.InX, n1.InY, n1.X, n1.Y,
        segT,
        splitX, splitY,
        prevOutX, prevOutY,
        newInX,  newInY,
        newOutX, newOutY,
        nextInX, nextInY);

    { Build the new node }
    newPt        := Default(TVertexPoint);
    newPt.X      := splitX;   newPt.Y      := splitY;
    newPt.InX    := newInX;   newPt.InY    := newInY;
    newPt.OutX   := newOutX;  newPt.OutY   := newOutY;
    newPt.Smooth := False;

    { Build the updated snapshots for adjacent nodes }
    prevAfter      := n0;
    prevAfter.OutX := prevOutX;  prevAfter.OutY := prevOutY;

    nextAfter      := n1;
    nextAfter.InX  := nextInX;   nextAfter.InY  := nextInY;

    cmd := TVertexCmdAddPoint.Create(segPath, insertIdx, prevIdx, nextIdx,
               newPt, n0, prevAfter, n1, nextAfter);
    FDocument.UndoStack.Execute(cmd);

    { Select the newly inserted node }
    FActivePath      := segPath;
    FSelectedNodeIdx := insertIdx;
    FDragTarget      := dtNone;
    Repaint;
    Exit;
  end;

  { 3. Click in empty space — deselect node, keep shape selected }
  FSelectedNodeIdx := -1;
  FActivePath      := nil;
  FDragTarget      := dtNone;
  Repaint;
end;

procedure TVertexCanvasWidget.HandleLMouseUp(x, y: integer;
    shiftstate: TShiftState);
var
  ptAfter:  TVertexPoint;
  cmd:      TVertexCommand;
  sh:       TVertexShape;
  cmdTrans: TVertexCmdSetShapeTranslation;
  newX, newY: Single;
  newHas:   Boolean;
begin
  { ── Pan-tool mode: commit pan ────────────────────────────────────────── }
  if FToolMode = tmPan then
  begin
    FPanDragActive := False;
    Exit;
  end;

  { ── Select-tool mode: commit drag translation ─────────────────────────── }
  if FToolMode = tmSelect then
  begin
    if FSelectDragActive and (FSelectDragShapeIdx >= 0) then
    begin
      sh   := FDocument.Shapes[FSelectDragShapeIdx];
      newX := ScreenToHvifX(x) - ScreenToHvifX(FSelectDragStartX) + FSelectDragShapeOX;
      newY := ScreenToHvifY(y) - ScreenToHvifY(FSelectDragStartY) + FSelectDragShapeOY;
      newHas := (newX <> 0) or (newY <> 0);
      { Reset shape to original position so the command captures the right before-state }
      sh.TranslateX    := FSelectDragShapeOX;
      sh.TranslateY    := FSelectDragShapeOY;
      sh.HasTranslation := (FSelectDragShapeOX <> 0) or (FSelectDragShapeOY <> 0);
      if (newX <> FSelectDragShapeOX) or (newY <> FSelectDragShapeOY) then
      begin
        cmdTrans := TVertexCmdSetShapeTranslation.Create(sh, newHas, newX, newY);
        FDocument.UndoStack.Execute(cmdTrans);
      end else
      begin
        { No real movement — just repaint to clear live preview }
        FIconDirty := True;
        Repaint;
      end;
    end;
    FSelectDragActive   := False;
    FSelectDragShapeIdx := -1;
    Exit;
  end;

  { ── Node-edit mode ────────────────────────────────────────────────────── }
  if (FDragTarget = dtNone) or (FActivePath = nil) then
    Exit;

  ptAfter := FActivePath.Points[FSelectedNodeIdx];

  { Only record an undo command if the point actually moved. }
  if (ptAfter.X    <> FDragPtBefore.X)   or (ptAfter.Y    <> FDragPtBefore.Y)   or
     (ptAfter.InX  <> FDragPtBefore.InX) or (ptAfter.InY  <> FDragPtBefore.InY) or
     (ptAfter.OutX <> FDragPtBefore.OutX) or (ptAfter.OutY <> FDragPtBefore.OutY) then
  begin
    case FDragTarget of
      dtAnchor:
        cmd := TVertexCmdMoveNode.Create(
                   FActivePath, FSelectedNodeIdx, FDragPtBefore, ptAfter);
      dtInHandle:
        cmd := TVertexCmdMoveHandle.Create(
                   FActivePath, FSelectedNodeIdx, True, FDragPtBefore, ptAfter);
      dtOutHandle:
        cmd := TVertexCmdMoveHandle.Create(
                   FActivePath, FSelectedNodeIdx, False, FDragPtBefore, ptAfter);
    else
      cmd := nil;
    end;
    if cmd <> nil then
      FDocument.UndoStack.Execute(cmd);
  end;

  FDragTarget := dtNone;
end;

procedure TVertexCanvasWidget.HandleMouseMove(x, y: integer; btnstate: word;
    shiftstate: TShiftState);
var
  hvx, hvy: Single;
  newPt:    TVertexPoint;
  sh2:      TVertexShape;
  dx, dy:   Single;
begin
  { Always fire cursor-move so the status bar can show the HVIF coordinates }
  if Assigned(FOnCursorMove) then
    FOnCursorMove(Self, ScreenToHvifX(x), ScreenToHvifY(y));

  { ── Pan-tool mode: live pan ──────────────────────────────────────────── }
  if FToolMode = tmPan then
  begin
    if FPanDragActive then
    begin
      FPanX := FPanDragOX + (x - FPanDragStartX);
      FPanY := FPanDragOY + (y - FPanDragStartY);
      Repaint;
    end;
    Exit;
  end;

  { ── Select-tool mode: live-preview shape drag ────────────────────────── }
  if FToolMode = tmSelect then
  begin
    if FSelectDragActive and (FSelectDragShapeIdx >= 0) then
    begin
      sh2 := FDocument.Shapes[FSelectDragShapeIdx];
      dx  := ScreenToHvifX(x) - ScreenToHvifX(FSelectDragStartX);
      dy  := ScreenToHvifY(y) - ScreenToHvifY(FSelectDragStartY);
      sh2.HasTranslation := True;
      sh2.TranslateX     := FSelectDragShapeOX + dx;
      sh2.TranslateY     := FSelectDragShapeOY + dy;
      FIconDirty := True;
      Repaint;
    end;
    Exit;
  end;

  if (FDragTarget = dtNone) or (FActivePath = nil) then
    Exit;

  newPt := FDragPtBefore;

  case FDragTarget of
    dtAnchor:
    begin
      hvx := SnapCoord(ScreenToHvifX(x - FDragOffX));
      hvy := SnapCoord(ScreenToHvifY(y - FDragOffY));
      newPt.InX  := hvx + (FDragPtBefore.InX  - FDragPtBefore.X);
      newPt.InY  := hvy + (FDragPtBefore.InY  - FDragPtBefore.Y);
      newPt.OutX := hvx + (FDragPtBefore.OutX - FDragPtBefore.X);
      newPt.OutY := hvy + (FDragPtBefore.OutY - FDragPtBefore.Y);
      newPt.X    := hvx;
      newPt.Y    := hvy;
    end;
    dtInHandle:
    begin
      newPt.InX := SnapCoord(ScreenToHvifX(x - FDragOffX));
      newPt.InY := SnapCoord(ScreenToHvifY(y - FDragOffY));
    end;
    dtOutHandle:
    begin
      newPt.OutX := SnapCoord(ScreenToHvifX(x - FDragOffX));
      newPt.OutY := SnapCoord(ScreenToHvifY(y - FDragOffY));
    end;
  end;

  FActivePath.Points[FSelectedNodeIdx] := newPt;
  FIconDirty := True;
  Repaint;
end;

end.
