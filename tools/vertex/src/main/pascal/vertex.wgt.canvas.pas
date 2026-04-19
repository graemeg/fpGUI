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
  fpg_hvif_model, fpg_hvif, fpg_hvif_writer,
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
    tmZoom,        { left-click zoom in, Alt+click zoom out }
    tmRect,        { drag to create a new rectangular closed path }
    tmEllipse,     { drag to create a new elliptical closed path (4-point Bezier) }
    tmPen          { click to place nodes; click first node or Enter to commit }
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
    { -1=unset, 0-3=corner(rotate), 4-7=edge(scale), 8=inside(translate) }
    FSelectDragHandle:    Integer;
    FSelectDragHasM:      Boolean;    { shape.HasTransform at drag start }
    FSelectDragHasTr:     Boolean;    { shape.HasTranslation at drag start }
    FSelectDragMatrix:    array[0..5] of Single;   { effective matrix at drag start }
    FSelectDragCHX,
    FSelectDragCHY:       Single;     { bbox center in HVIF space (rotation pivot) }
    FSelectDragAngle0:    Single;     { initial angle for rotation drag }
    FSelectDragAnchorHX,
    FSelectDragAnchorHY:  Single;     { scale anchor in HVIF space }
    FSelectDragHVecX,
    FSelectDragHVecY:     Single;     { handle vector from anchor in HVIF space }
    FSelectDragHVecLen2:  Single;     { |HVec|^2 }
    FSelectDragScaleIsX:  Boolean;    { True = scale X axis, False = scale Y axis }
    FSelectDragAnchorRaw: Single;     { anchor coord in raw shape space }

    { Bounding-box handle cache — populated by DrawShapeBoundingBox,
      consumed by HitTestBBoxHandle and the drag initialisers }
    FBBoxValid:    Boolean;
    FBBoxHandleSX: array[0..7] of Integer;   { screen X of each handle }
    FBBoxHandleSY: array[0..7] of Integer;   { screen Y of each handle }
    FBBoxRawMinX,
    FBBoxRawMinY,
    FBBoxRawMaxX,
    FBBoxRawMaxY:  Single;   { raw (pre-transform) bbox bounds }

    { Gradient handle editing state (set when user selects a gradient style in tree) }
    FEditGradStyle:     TVertexStyle;
    FGradHandleSX:      array[0..1] of Integer;  { screen X of gradient handles }
    FGradHandleSY:      array[0..1] of Integer;  { screen Y of gradient handles }
    FGradHandleValid:   Boolean;
    FGradDragActive:    Boolean;
    FGradDragHandle:    Integer;  { 0 = origin, 1 = radius/direction }
    FGradDragOldMatrix: array[0..5] of Single;

    { Rectangle / ellipse drag-create state }
    FShapeDragActive:  Boolean;
    FShapeDragStartHX,
    FShapeDragStartHY: Single;   { HVIF start corner }
    FShapeDragCurHX,
    FShapeDragCurHY:   Single;   { HVIF current corner }

    { Pen tool state — FPenPath is owned by canvas until committed }
    FPenPath:    TVertexPath;
    FPenPreviewX,
    FPenPreviewY: Single;        { HVIF coords of the current mouse position }

    { Zoom — negative means "fit to widget" }
    FZoom: Integer;        { -1 = fit; 50/100/200/400 = fixed % }

    { Grid }
    FShowGrid:  Boolean;
    FGridStep:  Integer;   { grid step in HVIF units; default 8 }
    FSnapToGrid: Boolean;

    { Events }
    FOnCursorMove:    TVertexCursorMoveEvent;
    FOnShapeSelected: TNotifyEvent;   { fires when user clicks a shape in the canvas }
    FOnPathAdded:     TNotifyEvent;   { fires after a tool creates and commits a new path }

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

    { Rubber-band overlays for drag-create tools }
    procedure DrawShapeDragOverlay;
    procedure DrawPenOverlay;
    { Builds and commits the rect/ellipse path after the drag-create mouse-up }
    procedure CommitDragCreatePath;

    { Gradient handle rendering and hit-testing }
    procedure DrawGradientHandles;
    function  HitTestGradHandle(AX, AY: Integer): Integer;

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

    { Hit-test against the 8 handles of the selected shape's bounding box.
      Returns 0-3 for corner handles (rotate), 4-7 for edge midpoint handles
      (scale), 8 if inside the bbox (translate), -1 if no hit.
      FBBoxValid must be True before calling. }
    function HitTestBBoxHandle(AX, AY: Integer): Integer;

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

    { Set the gradient style whose transform handles are shown on the canvas.
      Pass nil to hide gradient handles. }
    procedure SetEditGradient(AStyle: TVertexStyle);

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

    { Fires after a Rect, Ellipse, or Pen tool creates and commits a new path.
      The form should repopulate the tree and select the new path. }
    property OnPathAdded: TNotifyEvent
        read FOnPathAdded write FOnPathAdded;
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


{ ── Affine matrix helpers ────────────────────────────────────────────────── }
{ AggPas trans_affine order: M = [sx, shy, shx, sy, tx, ty]
  Point transform: x' = sx*x + shx*y + tx  (M[0]*x + M[2]*y + M[4])
                   y' = shy*x + sy*y  + ty  (M[1]*x + M[3]*y + M[5]) }

procedure MatApply(const M: array of Single; X, Y: Single; out OX, OY: Single);
begin
  OX := M[0]*X + M[2]*Y + M[4];
  OY := M[1]*X + M[3]*Y + M[5];
end;

{ Compose: apply A first in shape space, then B in canvas space.
  Result maps p → B(A(p)). }
procedure MatCompose(const A, B: array of Single; out R: array of Single);
begin
  R[0] := B[0]*A[0] + B[2]*A[1];
  R[1] := B[1]*A[0] + B[3]*A[1];
  R[2] := B[0]*A[2] + B[2]*A[3];
  R[3] := B[1]*A[2] + B[3]*A[3];
  R[4] := B[0]*A[4] + B[2]*A[5] + B[4];
  R[5] := B[1]*A[4] + B[3]*A[5] + B[5];
end;

{ Rotation around (CX, CY) by AAngle radians, in canvas (HVIF) space. }
procedure MatRotateAround(AAngle, CX, CY: Single; out R: array of Single);
var
  cosA, sinA: Single;
begin
  cosA  := cos(AAngle);
  sinA  := sin(AAngle);
  R[0]  := cosA;
  R[1]  := sinA;
  R[2]  := -sinA;
  R[3]  := cosA;
  R[4]  := CX*(1.0 - cosA) + CY*sinA;
  R[5]  := CY*(1.0 - cosA) - CX*sinA;
end;

{ Build effective matrix from shape's transform fields. }
procedure GetShapeMatrix(AShape: TVertexShape; out M: array of Single);
begin
  if AShape.HasTransform then
    AShape.GetTransform(M)
  else
  begin
    M[0] := 1.0;  M[1] := 0.0;
    M[2] := 0.0;  M[3] := 1.0;
    if AShape.HasTranslation then
    begin
      M[4] := AShape.TranslateX;
      M[5] := AShape.TranslateY;
    end
    else
    begin
      M[4] := 0.0;  M[5] := 0.0;
    end;
  end;
end;

{ True if point (PX,PY) is inside the convex quad (X0,Y0)..(X3,Y3) in order. }
function PointInConvexQuad(PX, PY, X0, Y0, X1, Y1, X2, Y2, X3, Y3: Integer): Boolean;
  function Cross(ax, ay, bx, by, cx, cy: Integer): Integer;
  begin
    Result := (bx - ax)*(cy - ay) - (by - ay)*(cx - ax);
  end;
var
  d0, d1, d2, d3: Integer;
begin
  d0 := Cross(X0, Y0, X1, Y1, PX, PY);
  d1 := Cross(X1, Y1, X2, Y2, PX, PY);
  d2 := Cross(X2, Y2, X3, Y3, PX, PY);
  d3 := Cross(X3, Y3, X0, Y0, PX, PY);
  Result := ((d0 >= 0) and (d1 >= 0) and (d2 >= 0) and (d3 >= 0)) or
            ((d0 <= 0) and (d1 <= 0) and (d2 <= 0) and (d3 <= 0));
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
  FToolMode           := tmNode;
  FSelectDragActive   := False;
  FSelectDragShapeIdx := -1;
  FSelectDragHandle   := -1;
  FBBoxValid          := False;
  FEditGradStyle      := nil;
  FGradHandleValid    := False;
  FGradDragActive     := False;
  FGradDragHandle     := -1;
  FShapeDragActive    := False;
  FPenPath            := nil;
  FPanX               := 0;
  FPanY               := 0;
  FPanDragActive      := False;
end;

destructor TVertexCanvasWidget.Destroy;
begin
  FIcon.Free;
  FPenPath.Free;   { cancel any in-progress pen path }
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
  FEditGradStyle    := nil;
  FGradHandleValid  := False;
  FGradDragActive   := False;
  FShapeDragActive  := False;
  FreeAndNil(FPenPath);
  FreeAndNil(FIcon);
  FIconDirty := (FDocument <> nil);
  Repaint;
end;

procedure TVertexCanvasWidget.DocumentChanged;
begin
  FIconDirty       := True;
  FBBoxValid       := False;
  FGradHandleValid := False;
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
  FBBoxValid        := False;
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

procedure TVertexCanvasWidget.SetEditGradient(AStyle: TVertexStyle);
begin
  FEditGradStyle   := AStyle;
  FGradHandleValid := False;
  FGradDragActive  := False;
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
{ Draws a transform-aware bounding box around the selected shape.
  The 8 handle positions are cached in FBBoxHandleSX/SY for hit-testing.
  Handle index convention (also used in select-drag interactions):
    0=TL corner, 1=TR corner, 2=BR corner, 3=BL corner  (rotation handles)
    4=top mid, 5=right mid, 6=bottom mid, 7=left mid     (scale handles)
  Edges connect corners: TL-TR, TR-BR, BR-BL, BL-TL. }
const
  COL_BBOX    = $FF0080FF;
  DASH_ON     = 6;
  DASH_OFF    = 4;
  HANDLE_HALF = 3;   { half-size of the square handle in screen px }

  procedure DashedSegment(x1, y1, x2, y2: Integer);
  var
    dx, dy, len, t, tEnd: Single;
  begin
    dx  := x2 - x1;
    dy  := y2 - y1;
    len := Sqrt(dx*dx + dy*dy);
    if len < 0.5 then Exit;
    t := 0;
    while t < len do
    begin
      tEnd := Min(t + DASH_ON, len);
      Canvas.DrawLine(
        Round(x1 + t/len*dx),    Round(y1 + t/len*dy),
        Round(x1 + tEnd/len*dx), Round(y1 + tEnd/len*dy));
      t := t + DASH_ON + DASH_OFF;
    end;
  end;

var
  shape:  TVertexShape;
  pi, ni: Integer;
  path:   TVertexPath;
  pt:     TVertexPoint;
  minRX, minRY, maxRX, maxRY: Single;
  first:  Boolean;
  midRX, midRY: Single;
  M:      array[0..5] of Single;
  rawX:   array[0..7] of Single;
  rawY:   array[0..7] of Single;
  hx, hy: Single;
  i:      Integer;

  procedure ExpandRaw(rx, ry: Single);
  begin
    if first then
    begin
      minRX := rx;  maxRX := rx;
      minRY := ry;  maxRY := ry;
      first := False;
    end
    else
    begin
      if rx < minRX then minRX := rx;
      if rx > maxRX then maxRX := rx;
      if ry < minRY then minRY := ry;
      if ry > maxRY then maxRY := ry;
    end;
  end;

begin
  FBBoxValid := False;
  if (FDocument = nil) or (FSelectedShapeIdx < 0) or
     (FSelectedShapeIdx >= FDocument.ShapeCount) then Exit;

  shape := FDocument.Shapes[FSelectedShapeIdx];
  first := True;

  { Collect raw (pre-transform) bounding box over all anchors and handles }
  for pi := 0 to shape.PathCount - 1 do
  begin
    path := shape.Paths[pi];
    for ni := 0 to path.PointCount - 1 do
    begin
      pt := path.Points[ni];
      ExpandRaw(pt.X,    pt.Y);
      ExpandRaw(pt.InX,  pt.InY);
      ExpandRaw(pt.OutX, pt.OutY);
    end;
  end;
  if first then Exit;

  { Store raw bounds for drag computations }
  FBBoxRawMinX := minRX;  FBBoxRawMinY := minRY;
  FBBoxRawMaxX := maxRX;  FBBoxRawMaxY := maxRY;
  midRX := (minRX + maxRX) * 0.5;
  midRY := (minRY + maxRY) * 0.5;

  { Get the effective transform (HasTransform → full matrix; else identity+translate) }
  GetShapeMatrix(shape, M);

  { Raw positions of the 8 handles (in shape/raw space) }
  rawX[0] := minRX;  rawY[0] := minRY;   { TL corner }
  rawX[1] := maxRX;  rawY[1] := minRY;   { TR corner }
  rawX[2] := maxRX;  rawY[2] := maxRY;   { BR corner }
  rawX[3] := minRX;  rawY[3] := maxRY;   { BL corner }
  rawX[4] := midRX;  rawY[4] := minRY;   { top mid }
  rawX[5] := maxRX;  rawY[5] := midRY;   { right mid }
  rawX[6] := midRX;  rawY[6] := maxRY;   { bottom mid }
  rawX[7] := minRX;  rawY[7] := midRY;   { left mid }

  { Transform and cache screen positions }
  for i := 0 to 7 do
  begin
    MatApply(M, rawX[i], rawY[i], hx, hy);
    FBBoxHandleSX[i] := HvifToScreenX(hx);
    FBBoxHandleSY[i] := HvifToScreenY(hy);
  end;

  { Draw dashed quadrilateral (corners 0-1-2-3-0) }
  Canvas.SetColor(COL_BBOX);
  DashedSegment(FBBoxHandleSX[0], FBBoxHandleSY[0], FBBoxHandleSX[1], FBBoxHandleSY[1]);
  DashedSegment(FBBoxHandleSX[1], FBBoxHandleSY[1], FBBoxHandleSX[2], FBBoxHandleSY[2]);
  DashedSegment(FBBoxHandleSX[2], FBBoxHandleSY[2], FBBoxHandleSX[3], FBBoxHandleSY[3]);
  DashedSegment(FBBoxHandleSX[3], FBBoxHandleSY[3], FBBoxHandleSX[0], FBBoxHandleSY[0]);

  { Draw handles }
  for i := 0 to 7 do
    Canvas.FillRectangle(FBBoxHandleSX[i] - HANDLE_HALF, FBBoxHandleSY[i] - HANDLE_HALF,
                         HANDLE_HALF*2 + 1, HANDLE_HALF*2 + 1);

  FBBoxValid := True;
end;

procedure TVertexCanvasWidget.DrawNodeOverlayForPath(APath: TVertexPath);
const
  COL_PATH_CURVE = $FF0055CC;  { blue path outline }
var
  ni, segCount, si: Integer;
  pt, npt: TVertexPoint;
  ax, ay, ihx, ihy, ohx, ohy: Integer;
  isSelNode: Boolean;
  t, u: Single;
  p0x, p0y, p1x, p1y, p2x, p2y, p3x, p3y: Single;
  bx, by, prevBx, prevBy: Single;
begin
  { ── Pass 1: draw the actual Bezier curve segments ── }
  if APath.PointCount >= 2 then
  begin
    if APath.Closed then
      segCount := APath.PointCount
    else
      segCount := APath.PointCount - 1;

    Canvas.SetColor(COL_PATH_CURVE);
    for ni := 0 to segCount - 1 do
    begin
      pt  := APath.Points[ni];
      npt := APath.Points[(ni + 1) mod APath.PointCount];
      p0x := HvifToScreenX(pt.X);    p0y := HvifToScreenY(pt.Y);
      p1x := HvifToScreenX(pt.OutX); p1y := HvifToScreenY(pt.OutY);
      p2x := HvifToScreenX(npt.InX); p2y := HvifToScreenY(npt.InY);
      p3x := HvifToScreenX(npt.X);   p3y := HvifToScreenY(npt.Y);
      prevBx := p0x;  prevBy := p0y;
      for si := 1 to SEG_SAMPLES do
      begin
        t := si / SEG_SAMPLES;
        u := 1.0 - t;
        bx := u*u*u*p0x + 3*u*u*t*p1x + 3*u*t*t*p2x + t*t*t*p3x;
        by := u*u*u*p0y + 3*u*u*t*p1y + 3*u*t*t*p2y + t*t*t*p3y;
        Canvas.DrawLine(Round(prevBx), Round(prevBy), Round(bx), Round(by));
        prevBx := bx;  prevBy := by;
      end;
    end;
  end;

  { ── Pass 2: draw handle arms and node circles on top ── }
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

procedure TVertexCanvasWidget.CommitDragCreatePath;
{ Builds a rect or 4-point Bezier ellipse from the current drag bounds
  and commits it as a new TVertexPath via TVertexCmdAddPath. }
const
  KAPPA = 0.5523;  { Bezier circle approximation constant }
var
  path:     TVertexPath;
  pt:       TVertexPoint;
  x1, y1, x2, y2: Single;
  cx, cy, rx, ry, k: Single;
begin
  if FDocument = nil then Exit;
  x1 := Min(FShapeDragStartHX, FShapeDragCurHX);
  y1 := Min(FShapeDragStartHY, FShapeDragCurHY);
  x2 := Max(FShapeDragStartHX, FShapeDragCurHX);
  y2 := Max(FShapeDragStartHY, FShapeDragCurHY);
  { Ignore trivially small shapes }
  if (Abs(x2 - x1) < 0.5) or (Abs(y2 - y1) < 0.5) then Exit;

  path := TVertexPath.Create(FDocument.UniqueName('path'));
  path.Closed := True;
  FillChar(pt, SizeOf(pt), 0);

  if FToolMode = tmRect then
  begin
    pt.X := x1; pt.Y := y1; pt.InX := x1; pt.InY := y1; pt.OutX := x1; pt.OutY := y1; path.AddPoint(pt);
    pt.X := x2; pt.Y := y1; pt.InX := x2; pt.InY := y1; pt.OutX := x2; pt.OutY := y1; path.AddPoint(pt);
    pt.X := x2; pt.Y := y2; pt.InX := x2; pt.InY := y2; pt.OutX := x2; pt.OutY := y2; path.AddPoint(pt);
    pt.X := x1; pt.Y := y2; pt.InX := x1; pt.InY := y2; pt.OutX := x1; pt.OutY := y2; path.AddPoint(pt);
  end
  else  { tmEllipse }
  begin
    cx := (x1 + x2) * 0.5;  cy := (y1 + y2) * 0.5;
    rx := (x2 - x1) * 0.5;  ry := (y2 - y1) * 0.5;
    k  := KAPPA;
    { Top }
    pt.X := cx;      pt.Y := cy - ry;
    pt.InX  := cx - rx*k;  pt.InY  := cy - ry;
    pt.OutX := cx + rx*k;  pt.OutY := cy - ry;
    path.AddPoint(pt);
    { Right }
    pt.X := cx + rx;  pt.Y := cy;
    pt.InX  := cx + rx;  pt.InY  := cy - ry*k;
    pt.OutX := cx + rx;  pt.OutY := cy + ry*k;
    path.AddPoint(pt);
    { Bottom }
    pt.X := cx;      pt.Y := cy + ry;
    pt.InX  := cx + rx*k;  pt.InY  := cy + ry;
    pt.OutX := cx - rx*k;  pt.OutY := cy + ry;
    path.AddPoint(pt);
    { Left }
    pt.X := cx - rx;  pt.Y := cy;
    pt.InX  := cx - rx;  pt.InY  := cy + ry*k;
    pt.OutX := cx - rx;  pt.OutY := cy - ry*k;
    path.AddPoint(pt);
  end;

  FDocument.UndoStack.Execute(TVertexCmdAddPath.Create(FDocument, path));
  if Assigned(FOnPathAdded) then FOnPathAdded(Self);
end;

procedure TVertexCanvasWidget.DrawShapeDragOverlay;
{ Draws a dashed rectangle or ellipse from FShapeDragStart to FShapeDragCur. }
const
  COL_DRAG = $FF0080FF;
  DASH_ON  = 5;
  DASH_OFF = 4;

  procedure DashLine(x1, y1, x2, y2: Integer);
  var dx, dy, len, t, tEnd: Single;
  begin
    dx := x2 - x1;  dy := y2 - y1;
    len := Sqrt(dx*dx + dy*dy);
    if len < 0.5 then Exit;
    t := 0;
    while t < len do
    begin
      tEnd := Min(t + DASH_ON, len);
      Canvas.DrawLine(Round(x1 + t/len*dx),    Round(y1 + t/len*dy),
                      Round(x1 + tEnd/len*dx), Round(y1 + tEnd/len*dy));
      t := t + DASH_ON + DASH_OFF;
    end;
  end;

var
  x1, y1, x2, y2: Integer;
  cx, cy, rw, rh: Integer;
  nx, ny: Single;
  i: Integer;
  ang, dAng: Single;
  pts: array[0..35] of TPoint;  { polyline approx for ellipse }
begin
  if not FShapeDragActive then Exit;
  Canvas.SetColor(COL_DRAG);
  x1 := HvifToScreenX(Min(FShapeDragStartHX, FShapeDragCurHX));
  y1 := HvifToScreenY(Min(FShapeDragStartHY, FShapeDragCurHY));
  x2 := HvifToScreenX(Max(FShapeDragStartHX, FShapeDragCurHX));
  y2 := HvifToScreenY(Max(FShapeDragStartHY, FShapeDragCurHY));
  if FToolMode = tmRect then
  begin
    DashLine(x1, y1, x2, y1);
    DashLine(x2, y1, x2, y2);
    DashLine(x2, y2, x1, y2);
    DashLine(x1, y2, x1, y1);
  end
  else  { tmEllipse }
  begin
    cx := (x1 + x2) div 2;
    cy := (y1 + y2) div 2;
    rw := (x2 - x1) div 2;
    rh := (y2 - y1) div 2;
    if (rw < 1) or (rh < 1) then Exit;
    dAng := 2 * Pi / 36;
    for i := 0 to 35 do
    begin
      ang := i * dAng;
      pts[i].X := cx + Round(rw * Cos(ang));
      pts[i].Y := cy + Round(rh * Sin(ang));
    end;
    for i := 0 to 35 do
      Canvas.DrawLine(pts[i].X, pts[i].Y, pts[(i+1) mod 36].X, pts[(i+1) mod 36].Y);
  end;
end;

procedure TVertexCanvasWidget.DrawPenOverlay;
{ Draws committed pen nodes, connecting lines, and a rubber-band to the mouse. }
const
  COL_PEN_LINE = $FF4488FF;
  COL_PEN_NODE = $FF4488FF;
  COL_RUBBER   = $FFAAAAAA;
var
  i:       Integer;
  pt, npt: TVertexPoint;
  ax, ay, nx, ny: Integer;
begin
  if FPenPath = nil then Exit;

  Canvas.SetColor(COL_PEN_LINE);
  { Draw lines between placed nodes }
  for i := 0 to FPenPath.PointCount - 2 do
  begin
    pt  := FPenPath.Points[i];
    npt := FPenPath.Points[i + 1];
    Canvas.DrawLine(HvifToScreenX(pt.X),  HvifToScreenY(pt.Y),
                    HvifToScreenX(npt.X), HvifToScreenY(npt.Y));
  end;

  { Rubber-band from last node to current mouse position }
  if FPenPath.PointCount > 0 then
  begin
    pt := FPenPath.Points[FPenPath.PointCount - 1];
    Canvas.SetColor(COL_RUBBER);
    Canvas.DrawLine(HvifToScreenX(pt.X), HvifToScreenY(pt.Y),
                    HvifToScreenX(FPenPreviewX), HvifToScreenY(FPenPreviewY));
  end;

  { Draw anchor circles }
  for i := 0 to FPenPath.PointCount - 1 do
  begin
    pt := FPenPath.Points[i];
    ax := HvifToScreenX(pt.X);
    ay := HvifToScreenY(pt.Y);
    DrawNodeCircle(ax, ay, NODE_RADIUS, COL_PEN_NODE, $FF000000,
                   i = FPenPath.PointCount - 1);
  end;
end;

procedure TVertexCanvasWidget.DrawGradientHandles;
{ Render interactive handles for the currently edited gradient style.
  Handle 0 (orange) = gradient origin, at HVIF (M[4], M[5]).
  Handle 1 (green)  = gradient direction, at HVIF (M[4]+64*M[0], M[5]+64*M[1]).
  For circular gradients a radius circle is also drawn. }
const
  COL_GRAD_ORIG = $FFFF6600;   { orange }
  COL_GRAD_DIR  = $FF00CC66;   { green }
  COL_GRAD_LINE = $FFBBBBBB;
var
  M:             array[0..5] of Single;
  ox, oy, dx, dy: Single;
  osx, osy, dsx, dsy: Integer;
  r: Single;
  ri: Integer;
begin
  FGradHandleValid := False;
  if (FEditGradStyle = nil) or (not FEditGradStyle.IsGradient) then
    Exit;

  FEditGradStyle.GetGradTransform(M);
  ox := M[4];              oy := M[5];
  dx := M[4] + 64.0*M[0]; dy := M[5] + 64.0*M[1];

  osx := HvifToScreenX(ox);  osy := HvifToScreenY(oy);
  dsx := HvifToScreenX(dx);  dsy := HvifToScreenY(dy);

  FGradHandleSX[0] := osx;  FGradHandleSY[0] := osy;
  FGradHandleSX[1] := dsx;  FGradHandleSY[1] := dsy;
  FGradHandleValid := True;

  { Line from origin to direction handle }
  Canvas.SetColor(COL_GRAD_LINE);
  Canvas.DrawLine(osx, osy, dsx, dsy);

  { For circular gradient: draw radius circle around origin }
  if FEditGradStyle.GradientType = hgtCircular then
  begin
    r  := Sqrt(Sqr(Single(dsx - osx)) + Sqr(Single(dsy - osy)));
    ri := Round(r);
    if ri > 1 then
    begin
      Canvas.SetColor(COL_GRAD_LINE);
      Canvas.DrawArc(osx - ri, osy - ri, ri*2, ri*2, 0, 360);
    end;
  end;

  { Filled circles for both handles }
  DrawNodeCircle(osx, osy, HANDLE_RADIUS + 2, COL_GRAD_ORIG, $FF000000, False);
  DrawNodeCircle(dsx, dsy, HANDLE_RADIUS + 2, COL_GRAD_DIR,  $FF000000, False);
end;

function TVertexCanvasWidget.HitTestGradHandle(AX, AY: Integer): Integer;
const
  GRAD_HIT = 8;
begin
  Result := -1;
  if not FGradHandleValid then Exit;
  { Origin handle (0) takes priority }
  if (Abs(AX - FGradHandleSX[0]) <= GRAD_HIT) and
     (Abs(AY - FGradHandleSY[0]) <= GRAD_HIT) then
    Result := 0
  else if (Abs(AX - FGradHandleSX[1]) <= GRAD_HIT) and
          (Abs(AY - FGradHandleSY[1]) <= GRAD_HIT) then
    Result := 1;
end;

procedure TVertexCanvasWidget.DrawControlOverlay;
var
  shape: TVertexShape;
  pi: Integer;
begin
  if FDocument = nil then Exit;

  { Drag-create tools: rubber-band overlay only }
  if FToolMode in [tmRect, tmEllipse] then
  begin
    DrawShapeDragOverlay;
    Exit;
  end;

  { Pen tool: in-progress path + rubber-band segment }
  if FToolMode = tmPen then
  begin
    DrawPenOverlay;
    Exit;
  end;

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
    if FEditGradStyle <> nil then
      DrawGradientHandles;
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
  sh:  TVertexShape;
begin
  { Escape / Enter for pen tool }
  if FToolMode = tmPen then
  begin
    if keycode = keyEscape then
    begin
      FreeAndNil(FPenPath);
      Repaint;
      consumed := True;
      Exit;
    end;
    if keycode = keyReturn then
    begin
      if (FPenPath <> nil) and (FPenPath.PointCount >= 2) then
      begin
        FDocument.UndoStack.Execute(TVertexCmdAddPath.Create(FDocument, FPenPath));
        FPenPath := nil;
        if Assigned(FOnPathAdded) then FOnPathAdded(Self);
      end;
      consumed := True;
      Exit;
    end;
  end;

  { Escape cancels rect/ellipse drag }
  if (keycode = keyEscape) and FShapeDragActive then
  begin
    FShapeDragActive := False;
    Repaint;
    consumed := True;
    Exit;
  end;

  { Escape cancels an in-progress gradient drag }
  if (keycode = keyEscape) and FGradDragActive then
  begin
    if FEditGradStyle <> nil then
      FEditGradStyle.SetGradTransform(FGradDragOldMatrix);
    FGradDragActive  := False;
    FGradHandleValid := False;
    FIconDirty       := True;
    Repaint;
    consumed := True;
    Exit;
  end;

  { Escape cancels an in-progress bbox drag (rotation, scale, or matrix translate) }
  if keycode = keyEscape then
  begin
    if FSelectDragActive and (FSelectDragShapeIdx >= 0) and
       (FSelectDragHandle >= 0) and (FSelectDragHandle <= 7) then
    begin
      sh := FDocument.Shapes[FSelectDragShapeIdx];
      sh.HasTransform   := FSelectDragHasM;
      sh.SetTransform(FSelectDragMatrix);
      sh.HasTranslation := FSelectDragHasTr;
      sh.TranslateX     := FSelectDragShapeOX;
      sh.TranslateY     := FSelectDragShapeOY;
      FSelectDragActive   := False;
      FSelectDragShapeIdx := -1;
      FSelectDragHandle   := -1;
      FIconDirty := True;
      Repaint;
      consumed := True;
    end
    else if FSelectDragActive and (FSelectDragShapeIdx >= 0) and
            (FSelectDragHandle = 8) and FSelectDragHasM then
    begin
      sh := FDocument.Shapes[FSelectDragShapeIdx];
      sh.HasTransform   := FSelectDragHasM;
      sh.SetTransform(FSelectDragMatrix);
      FSelectDragActive   := False;
      FSelectDragShapeIdx := -1;
      FSelectDragHandle   := -1;
      FIconDirty := True;
      Repaint;
      consumed := True;
    end;
  end;

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
{ Hit-test using the transform-aware bounding box: transforms the 4 raw bbox
  corners through the effective matrix and tests for point-in-convex-quad. }
var
  i, j, k: Integer;
  sh:       TVertexShape;
  ph:       TVertexPath;
  pt:       TVertexPoint;
  minRX, minRY, maxRX, maxRY: Single;
  first:    Boolean;
  M:        array[0..5] of Single;
  hx, hy:   Single;
  cx:       array[0..3] of Integer;
  cy:       array[0..3] of Integer;

  procedure ExpandRaw(rx, ry: Single);
  begin
    if first then
    begin
      minRX := rx;  maxRX := rx;
      minRY := ry;  maxRY := ry;
      first := False;
    end
    else
    begin
      if rx < minRX then minRX := rx;
      if rx > maxRX then maxRX := rx;
      if ry < minRY then minRY := ry;
      if ry > maxRY then maxRY := ry;
    end;
  end;

begin
  Result := -1;
  if FDocument = nil then Exit;

  for i := FDocument.ShapeCount - 1 downto 0 do
  begin
    sh := FDocument.Shapes[i];
    if not sh.Visible then Continue;

    first := True;
    for j := 0 to sh.PathCount - 1 do
    begin
      ph := sh.Paths[j];
      for k := 0 to ph.PointCount - 1 do
      begin
        pt := ph.Points[k];
        ExpandRaw(pt.X,   pt.Y);
        ExpandRaw(pt.InX, pt.InY);
        ExpandRaw(pt.OutX,pt.OutY);
      end;
    end;
    if first then Continue;

    GetShapeMatrix(sh, M);

    MatApply(M, minRX, minRY, hx, hy);  cx[0] := HvifToScreenX(hx);  cy[0] := HvifToScreenY(hy);
    MatApply(M, maxRX, minRY, hx, hy);  cx[1] := HvifToScreenX(hx);  cy[1] := HvifToScreenY(hy);
    MatApply(M, maxRX, maxRY, hx, hy);  cx[2] := HvifToScreenX(hx);  cy[2] := HvifToScreenY(hy);
    MatApply(M, minRX, maxRY, hx, hy);  cx[3] := HvifToScreenX(hx);  cy[3] := HvifToScreenY(hy);

    if PointInConvexQuad(AX, AY,
        cx[0], cy[0], cx[1], cy[1], cx[2], cy[2], cx[3], cy[3]) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TVertexCanvasWidget.HitTestBBoxHandle(AX, AY: Integer): Integer;
const
  HANDLE_HIT = 8;  { screen pixels from centre of handle }
var
  i: Integer;
begin
  Result := -1;
  if not FBBoxValid then Exit;

  { Corner and edge handles take priority over the "inside" check }
  for i := 0 to 7 do
    if (Abs(AX - FBBoxHandleSX[i]) <= HANDLE_HIT) and
       (Abs(AY - FBBoxHandleSY[i]) <= HANDLE_HIT) then
    begin
      Result := i;
      Exit;
    end;

  { Inside the convex quad (corners 0-1-2-3) → translate }
  if PointInConvexQuad(AX, AY,
      FBBoxHandleSX[0], FBBoxHandleSY[0],
      FBBoxHandleSX[1], FBBoxHandleSY[1],
      FBBoxHandleSX[2], FBBoxHandleSY[2],
      FBBoxHandleSX[3], FBBoxHandleSY[3]) then
    Result := 8;
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
  handleIdx: Integer;
  hvx, hvy: Single;
begin
  { Claim keyboard focus so Delete/Backspace reach HandleKeyPress }
  SetFocus;

  { ── Gradient handle interaction (active when a gradient style is selected) ── }
  if (FEditGradStyle <> nil) and FGradHandleValid then
  begin
    handleIdx := HitTestGradHandle(x, y);
    if handleIdx >= 0 then
    begin
      FGradDragActive  := True;
      FGradDragHandle  := handleIdx;
      FEditGradStyle.GetGradTransform(FGradDragOldMatrix);
      Exit;
    end;
  end;

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

  { ── Select-tool mode ────────────────────────────────────────────────────── }
  if FToolMode = tmSelect then
  begin
    if FDocument = nil then Exit;

    { First: try to hit a handle on the already-selected shape }
    handleIdx := -1;
    if FBBoxValid and (FSelectedShapeIdx >= 0) then
      handleIdx := HitTestBBoxHandle(x, y);

    if handleIdx >= 0 then
    begin
      { Interact with a bbox handle of the current selection }
      sh := FDocument.Shapes[FSelectedShapeIdx];
      FSelectDragActive    := True;
      FSelectDragHandle    := handleIdx;
      FSelectDragStartX    := x;
      FSelectDragStartY    := y;
      FSelectDragShapeIdx  := FSelectedShapeIdx;
      FSelectDragShapeOX   := sh.TranslateX;
      FSelectDragShapeOY   := sh.TranslateY;
      FSelectDragHasM      := sh.HasTransform;
      FSelectDragHasTr     := sh.HasTranslation;
      GetShapeMatrix(sh, FSelectDragMatrix);

      { Precompute interaction parameters }
      FSelectDragCHX := (FBBoxRawMinX + FBBoxRawMaxX) * 0.5;
      FSelectDragCHY := (FBBoxRawMinY + FBBoxRawMaxY) * 0.5;
      { Transform centre to HVIF canvas space }
      MatApply(FSelectDragMatrix, FSelectDragCHX, FSelectDragCHY,
               FSelectDragCHX, FSelectDragCHY);

      case handleIdx of
        0..3:
          { Rotation: capture initial angle from centre to mouse in HVIF space }
          FSelectDragAngle0 := ArcTan2(ScreenToHvifY(y) - FSelectDragCHY,
                                       ScreenToHvifX(x) - FSelectDragCHX);

        4..7:
        begin
          { Scale: compute anchor point + h-vector in HVIF space }
          FSelectDragScaleIsX := handleIdx in [5, 7];
          case handleIdx of
            4: { top edge — anchor = bottom mid }
              begin
                FSelectDragAnchorRaw := FBBoxRawMaxY;
                MatApply(FSelectDragMatrix,
                         (FBBoxRawMinX+FBBoxRawMaxX)*0.5, FBBoxRawMaxY,
                         FSelectDragAnchorHX, FSelectDragAnchorHY);
                { h-vector: anchor to handle }
                MatApply(FSelectDragMatrix,
                         (FBBoxRawMinX+FBBoxRawMaxX)*0.5, FBBoxRawMinY,
                         FSelectDragHVecX, FSelectDragHVecY);
              end;
            5: { right edge — anchor = left mid }
              begin
                FSelectDragAnchorRaw := FBBoxRawMinX;
                MatApply(FSelectDragMatrix,
                         FBBoxRawMinX, (FBBoxRawMinY+FBBoxRawMaxY)*0.5,
                         FSelectDragAnchorHX, FSelectDragAnchorHY);
                MatApply(FSelectDragMatrix,
                         FBBoxRawMaxX, (FBBoxRawMinY+FBBoxRawMaxY)*0.5,
                         FSelectDragHVecX, FSelectDragHVecY);
              end;
            6: { bottom edge — anchor = top mid }
              begin
                FSelectDragAnchorRaw := FBBoxRawMinY;
                MatApply(FSelectDragMatrix,
                         (FBBoxRawMinX+FBBoxRawMaxX)*0.5, FBBoxRawMinY,
                         FSelectDragAnchorHX, FSelectDragAnchorHY);
                MatApply(FSelectDragMatrix,
                         (FBBoxRawMinX+FBBoxRawMaxX)*0.5, FBBoxRawMaxY,
                         FSelectDragHVecX, FSelectDragHVecY);
              end;
            7: { left edge — anchor = right mid }
              begin
                FSelectDragAnchorRaw := FBBoxRawMaxX;
                MatApply(FSelectDragMatrix,
                         FBBoxRawMaxX, (FBBoxRawMinY+FBBoxRawMaxY)*0.5,
                         FSelectDragAnchorHX, FSelectDragAnchorHY);
                MatApply(FSelectDragMatrix,
                         FBBoxRawMinX, (FBBoxRawMinY+FBBoxRawMaxY)*0.5,
                         FSelectDragHVecX, FSelectDragHVecY);
              end;
          end;
          { Convert h-vector from absolute to relative (subtract anchor) }
          FSelectDragHVecX   := FSelectDragHVecX - FSelectDragAnchorHX;
          FSelectDragHVecY   := FSelectDragHVecY - FSelectDragAnchorHY;
          FSelectDragHVecLen2 := FSelectDragHVecX*FSelectDragHVecX +
                                 FSelectDragHVecY*FSelectDragHVecY;
        end;
        { 8 = inside/translate — no extra setup needed }
      end;
    end
    else
    begin
      { No handle hit: try to select/re-select a shape }
      shapeIdx := HitTestShapeBBox(x, y);
      if shapeIdx >= 0 then
      begin
        SelectShapeByClick(shapeIdx);
        sh := FDocument.Shapes[shapeIdx];
        FSelectDragActive    := True;
        FSelectDragHandle    := 8;   { translate }
        FSelectDragStartX    := x;
        FSelectDragStartY    := y;
        FSelectDragShapeIdx  := shapeIdx;
        FSelectDragShapeOX   := sh.TranslateX;
        FSelectDragShapeOY   := sh.TranslateY;
        FSelectDragHasM      := sh.HasTransform;
        FSelectDragHasTr     := sh.HasTranslation;
        GetShapeMatrix(sh, FSelectDragMatrix);
      end
      else
        SetSelectedShapeIndex(-1);   { click on empty canvas: deselect }
    end;
    Exit;
  end;

  { ── Rect / Ellipse drag-create mode ────────────────────────────────────── }
  if FToolMode in [tmRect, tmEllipse] then
  begin
    if FDocument = nil then Exit;
    FShapeDragActive  := True;
    FShapeDragStartHX := SnapCoord(ScreenToHvifX(x));
    FShapeDragStartHY := SnapCoord(ScreenToHvifY(y));
    FShapeDragCurHX   := FShapeDragStartHX;
    FShapeDragCurHY   := FShapeDragStartHY;
    Exit;
  end;

  { ── Pen tool mode ───────────────────────────────────────────────────────── }
  if FToolMode = tmPen then
  begin
    if FDocument = nil then Exit;
    hvx := SnapCoord(ScreenToHvifX(x));
    hvy := SnapCoord(ScreenToHvifY(y));

    if FPenPath = nil then
    begin
      { Start a new path }
      FPenPath := TVertexPath.Create('pen');
      FPenPath.Closed := False;
    end
    else
    begin
      { Click near the first node → close and commit }
      pt := FPenPath.Points[0];
      if (FPenPath.PointCount >= 2) and
         (Sqr(HvifToScreenX(pt.X) - x) + Sqr(HvifToScreenY(pt.Y) - y) <= Sqr(HIT_RADIUS + 2)) then
      begin
        FPenPath.Closed := True;
        FDocument.UndoStack.Execute(TVertexCmdAddPath.Create(FDocument, FPenPath));
        FPenPath := nil;
        if Assigned(FOnPathAdded) then FOnPathAdded(Self);
        Exit;
      end;
    end;
    { Append new anchor (handles coincident with anchor = straight-line segment) }
    newPt        := Default(TVertexPoint);
    newPt.X      := hvx;  newPt.Y      := hvy;
    newPt.InX    := hvx;  newPt.InY    := hvy;
    newPt.OutX   := hvx;  newPt.OutY   := hvy;
    newPt.Smooth := False;
    FPenPath.AddPoint(newPt);
    FPenPreviewX := hvx;
    FPenPreviewY := hvy;
    Repaint;
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
  ptAfter:    TVertexPoint;
  cmd:        TVertexCommand;
  sh:         TVertexShape;
  cmdTrans:   TVertexCmdSetShapeTranslation;
  cmdMatrix:  TVertexCmdSetShapeTransform;
  cmdGrad:    TVertexCmdSetGradientTransform;
  newX, newY: Single;
  newHas:     Boolean;
  finalM:     array[0..5] of Single;
  newGradM:   array[0..5] of Single;
begin
  { ── Rect / Ellipse drag-create: commit path on mouse-up ─────────────── }
  if FToolMode in [tmRect, tmEllipse] then
  begin
    if FShapeDragActive then
    begin
      FShapeDragActive := False;
      CommitDragCreatePath;
    end;
    Exit;
  end;

  { ── Gradient handle drag: commit via undo command ─────────────────────── }
  if FGradDragActive then
  begin
    FGradDragActive := False;
    if FEditGradStyle <> nil then
    begin
      FEditGradStyle.GetGradTransform(newGradM);
      FEditGradStyle.SetGradTransform(FGradDragOldMatrix);  { restore before command }
      cmdGrad := TVertexCmdSetGradientTransform.Create(FEditGradStyle, newGradM);
      FDocument.UndoStack.Execute(cmdGrad);
    end;
    Exit;
  end;

  { ── Pan-tool mode: commit pan ────────────────────────────────────────── }
  if FToolMode = tmPan then
  begin
    FPanDragActive := False;
    Exit;
  end;

  { ── Select-tool mode: commit drag ────────────────────────────────────── }
  if FToolMode = tmSelect then
  begin
    if FSelectDragActive and (FSelectDragShapeIdx >= 0) then
    begin
      sh := FDocument.Shapes[FSelectDragShapeIdx];

      case FSelectDragHandle of
        0..7:
        begin
          { Rotation or scale — shape.HasTransform was set during live preview.
            Grab final matrix, reset to original state, then commit via command. }
          sh.GetTransform(finalM);
          sh.HasTransform   := FSelectDragHasM;
          sh.SetTransform(FSelectDragMatrix);
          sh.HasTranslation := FSelectDragHasTr;
          sh.TranslateX     := FSelectDragShapeOX;
          sh.TranslateY     := FSelectDragShapeOY;
          cmdMatrix := TVertexCmdSetShapeTransform.Create(sh, True, finalM);
          FDocument.UndoStack.Execute(cmdMatrix);
        end;

        8:
        begin
          if FSelectDragHasM then
          begin
            { Shape had a full affine transform: commit updated tx/ty in matrix }
            sh.GetTransform(finalM);
            sh.HasTransform   := FSelectDragHasM;
            sh.SetTransform(FSelectDragMatrix);
            sh.HasTranslation := FSelectDragHasTr;
            sh.TranslateX     := FSelectDragShapeOX;
            sh.TranslateY     := FSelectDragShapeOY;
            cmdMatrix := TVertexCmdSetShapeTransform.Create(sh, True, finalM);
            FDocument.UndoStack.Execute(cmdMatrix);
          end
          else
          begin
            { Simple translation }
            newX   := SnapCoord(ScreenToHvifX(x) - ScreenToHvifX(FSelectDragStartX) + FSelectDragShapeOX);
            newY   := SnapCoord(ScreenToHvifY(y) - ScreenToHvifY(FSelectDragStartY) + FSelectDragShapeOY);
            newHas := (newX <> 0) or (newY <> 0);
            sh.TranslateX     := FSelectDragShapeOX;
            sh.TranslateY     := FSelectDragShapeOY;
            sh.HasTranslation := (FSelectDragShapeOX <> 0) or (FSelectDragShapeOY <> 0);
            if (newX <> FSelectDragShapeOX) or (newY <> FSelectDragShapeOY) then
            begin
              cmdTrans := TVertexCmdSetShapeTranslation.Create(sh, newHas, newX, newY);
              FDocument.UndoStack.Execute(cmdTrans);
            end
            else
            begin
              FIconDirty := True;
              Repaint;
            end;
          end;
        end;

      else
        FIconDirty := True;
        Repaint;
      end;
    end;
    FSelectDragActive   := False;
    FSelectDragShapeIdx := -1;
    FSelectDragHandle   := -1;
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
  angle, delta, sf, dot: Single;
  R, S, newM: array[0..5] of Single;
  gradM:    array[0..5] of Single;
  gradCX, gradCY, gradR: Single;
begin
  { Always fire cursor-move so the status bar can show the HVIF coordinates }
  if Assigned(FOnCursorMove) then
    FOnCursorMove(Self, ScreenToHvifX(x), ScreenToHvifY(y));

  { ── Gradient handle drag (live preview) ─────────────────────────────── }
  if FGradDragActive and (FEditGradStyle <> nil) then
  begin
    hvx := ScreenToHvifX(x);
    hvy := ScreenToHvifY(y);
    Move(FGradDragOldMatrix[0], gradM[0], SizeOf(gradM));
    case FGradDragHandle of
      0: { origin: translate entire gradient }
      begin
        gradM[4] := hvx;
        gradM[5] := hvy;
      end;
      1: { radius/direction }
      begin
        gradCX := FGradDragOldMatrix[4];
        gradCY := FGradDragOldMatrix[5];
        if FEditGradStyle.GradientType = hgtCircular then
        begin
          gradR := Sqrt(Sqr(hvx - gradCX) + Sqr(hvy - gradCY)) / 64.0;
          if gradR < 0.001 then gradR := 0.001;
          gradM[0] := gradR;  gradM[1] := 0.0;
          gradM[2] := 0.0;    gradM[3] := gradR;
        end
        else
        begin
          gradM[0] := (hvx - gradCX) / 64.0;
          gradM[1] := (hvy - gradCY) / 64.0;
        end;
      end;
    end;
    FEditGradStyle.SetGradTransform(gradM);
    FIconDirty       := True;
    FGradHandleValid := False;
    Repaint;
    Exit;
  end;

  { ── Rect / Ellipse drag-create: live rubber-band ────────────────────── }
  if FToolMode in [tmRect, tmEllipse] then
  begin
    if FShapeDragActive then
    begin
      FShapeDragCurHX := SnapCoord(ScreenToHvifX(x));
      FShapeDragCurHY := SnapCoord(ScreenToHvifY(y));
      Repaint;
    end;
    Exit;
  end;

  { ── Pen tool: update rubber-band preview ─────────────────────────────── }
  if FToolMode = tmPen then
  begin
    FPenPreviewX := ScreenToHvifX(x);
    FPenPreviewY := ScreenToHvifY(y);
    if FPenPath <> nil then
      Repaint;
    Exit;
  end;

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
      case FSelectDragHandle of
        0..3:
        begin
          { Rotation around bbox centre }
          angle := ArcTan2(ScreenToHvifY(y) - FSelectDragCHY,
                           ScreenToHvifX(x) - FSelectDragCHX);
          delta := angle - FSelectDragAngle0;
          MatRotateAround(delta, FSelectDragCHX, FSelectDragCHY, R);
          MatCompose(FSelectDragMatrix, R, newM);
          sh2.HasTransform := True;
          sh2.SetTransform(newM);
        end;
        4..7:
        begin
          { Scale along one axis anchored at the opposite edge }
          dot := (ScreenToHvifX(x) - FSelectDragAnchorHX) * FSelectDragHVecX +
                 (ScreenToHvifY(y) - FSelectDragAnchorHY) * FSelectDragHVecY;
          if FSelectDragHVecLen2 > 0.0001 then
            sf := dot / FSelectDragHVecLen2
          else
            sf := 1.0;
          if sf < 0.01 then sf := 0.01;   { prevent degenerate matrix }
          if FSelectDragScaleIsX then
          begin
            S[0] := sf;   S[1] := 0.0;
            S[2] := 0.0;  S[3] := 1.0;
            S[4] := FSelectDragAnchorRaw*(1.0 - sf);
            S[5] := 0.0;
          end
          else
          begin
            S[0] := 1.0;  S[1] := 0.0;
            S[2] := 0.0;  S[3] := sf;
            S[4] := 0.0;
            S[5] := FSelectDragAnchorRaw*(1.0 - sf);
          end;
          MatCompose(S, FSelectDragMatrix, newM);
          sh2.HasTransform := True;
          sh2.SetTransform(newM);
        end;
        8:
        begin
          { Translation }
          dx := ScreenToHvifX(x) - ScreenToHvifX(FSelectDragStartX);
          dy := ScreenToHvifY(y) - ScreenToHvifY(FSelectDragStartY);
          if FSelectDragHasM then
          begin
            { Shape already has a full transform: update tx/ty in the matrix }
            newM := FSelectDragMatrix;
            newM[4] := FSelectDragMatrix[4] + dx;
            newM[5] := FSelectDragMatrix[5] + dy;
            sh2.HasTransform := True;
            sh2.SetTransform(newM);
          end
          else
          begin
            sh2.HasTranslation := True;
            sh2.TranslateX     := SnapCoord(FSelectDragShapeOX + dx);
            sh2.TranslateY     := SnapCoord(FSelectDragShapeOY + dy);
          end;
        end;
      end;
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
