unit iom.wgt.canvas;

{
  TIomCanvasWidget — renders the HVIF icon from TIomDocument and provides
  interactive path editing via drag of anchor nodes and Bezier handles.

  Step #5: interactive editing wired to TIomDocument + undo stack.
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
    MouseUp    — create TIomCmdMoveNode / TIomCmdMoveHandle with before/after
                 snapshots and call FDocument.UndoStack.Execute(cmd).

  Node add:
    Click on a bezier segment → de Casteljau split at hit parameter t.
    Creates TIomCmdAddPoint which updates adjacent handles and inserts the node.

  Node delete:
    Delete or Backspace when a node is selected → TIomCmdDeletePoint.
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
  fpg_iom_document;


type
  TDragTarget = (dtNone, dtAnchor, dtInHandle, dtOutHandle);

  TIomCanvasWidget = class(TfpgWidget)
  private
    { Data (not owned) }
    FDocument: TIomDocument;

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
    FActivePath:       TIomPath;  { nil = none; which path owns the selected node }

    { Drag state }
    FDragTarget:   TDragTarget;
    FDragOffX, FDragOffY: Integer;  { mouse offset from exact node screen position }
    FDragPtBefore: TIomPoint;       { snapshot of the point at drag-start }

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
    procedure DrawEmptyHint;
    procedure DrawControlOverlay;
    procedure DrawNodeCircle(AScreenX, AScreenY, ARadius: Integer;
                              AFill, ABorder: TfpgColor; ASelected: Boolean);

    { Hit-testing }
    function HitTestNodes(AX, AY: Integer; out APath: TIomPath;
                          out ANodeIdx: Integer;
                          out ATarget: TDragTarget): Boolean;
    function HitTestSegments(AX, AY: Integer; out APath: TIomPath;
                             out ASegmentIdx: Integer;
                             out AT: Single): Boolean;

    procedure SetSelectedShapeIndex(AValue: Integer);

  protected
    procedure HandlePaint; override;
    procedure HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure HandleMouseMove(x, y: integer; btnstate: word;
                              shiftstate: TShiftState); override;
    procedure HandleKeyPress(var keycode: word; var shiftstate: TShiftState;
                             var consumed: boolean); override;
    procedure HandleResize(awidth, aheight: TfpgCoord); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    { Assign the document to render and edit. Pass nil to clear. }
    procedure SetDocument(ADoc: TIomDocument);

    { Call after an external change (undo/redo from main form menu). }
    procedure DocumentChanged;

    property SelectedShapeIndex: Integer
        read FSelectedShapeIdx write SetSelectedShapeIndex;
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


{ ── TIomCanvasWidget ─────────────────────────────────────────────────────── }

constructor TIomCanvasWidget.Create(AOwner: TComponent);
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
end;

destructor TIomCanvasWidget.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;

procedure TIomCanvasWidget.SetDocument(ADoc: TIomDocument);
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

procedure TIomCanvasWidget.DocumentChanged;
begin
  FIconDirty := True;
  Repaint;
end;

procedure TIomCanvasWidget.SetSelectedShapeIndex(AValue: Integer);
begin
  if FSelectedShapeIdx = AValue then
    Exit;
  FSelectedShapeIdx := AValue;
  FSelectedNodeIdx  := -1;
  FActivePath       := nil;
  FDragTarget       := dtNone;
  Repaint;
end;

{ ── Coordinate helpers ───────────────────────────────────────────────────── }

function TIomCanvasWidget.HvifToScreenX(AHvif: Single): Integer;
begin
  Result := FIconOX + Round(AHvif * FScale);
end;

function TIomCanvasWidget.HvifToScreenY(AHvif: Single): Integer;
begin
  Result := FIconOY + Round(AHvif * FScale);
end;

function TIomCanvasWidget.ScreenToHvifX(AScreen: Integer): Single;
begin
  if FScale > 0 then
    Result := (AScreen - FIconOX) / FScale
  else
    Result := 0;
end;

function TIomCanvasWidget.ScreenToHvifY(AScreen: Integer): Single;
begin
  if FScale > 0 then
    Result := (AScreen - FIconOY) / FScale
  else
    Result := 0;
end;

procedure TIomCanvasWidget.UpdateIconGeometry;
var
  sz: Integer;
begin
  sz := Min(Width - 40, Height - 40);
  if sz < 16 then sz := 16;
  sz := (sz div 4) * 4;
  FIconSZ := sz;
  FIconOX := (Width  - FIconSZ) div 2;
  FIconOY := (Height - FIconSZ) div 2;
  FScale  := FIconSZ / 64.0;
end;


{ ── Rendering ────────────────────────────────────────────────────────────── }

procedure TIomCanvasWidget.RebuildIcon;
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

procedure TIomCanvasWidget.DrawCheckerboard;
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

procedure TIomCanvasWidget.DrawHvifImage;
var
  img: TfpgImage;
begin
  if FIcon = nil then
    Exit;
  img := FIcon.GetImage(FIconSZ, FIconSZ);
  if img <> nil then
    Canvas.DrawImage(FIconOX, FIconOY, img);
end;

procedure TIomCanvasWidget.DrawEmptyHint;
begin
  Canvas.SetTextColor($FF888888);
  Canvas.DrawString(Width div 2 - 90, Height div 2 - 8,
      'File > Open to load an HVIF icon');
end;

procedure TIomCanvasWidget.DrawNodeCircle(AScreenX, AScreenY, ARadius: Integer;
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

procedure TIomCanvasWidget.DrawControlOverlay;
var
  shape: TIomShape;
  pi, ni: Integer;
  path: TIomPath;
  pt: TIomPoint;
  ax, ay, ihx, ihy, ohx, ohy: Integer;
  isSelNode: Boolean;
begin
  if (FDocument = nil) or (FSelectedShapeIdx < 0) or
     (FSelectedShapeIdx >= FDocument.ShapeCount) then
    Exit;
  shape := FDocument.Shapes[FSelectedShapeIdx];
  for pi := 0 to shape.PathCount - 1 do
  begin
    path := shape.Paths[pi];
    for ni := 0 to path.PointCount - 1 do
    begin
      pt  := path.Points[ni];
      ax  := HvifToScreenX(pt.X);
      ay  := HvifToScreenY(pt.Y);
      ihx := HvifToScreenX(pt.InX);
      ihy := HvifToScreenY(pt.InY);
      ohx := HvifToScreenX(pt.OutX);
      ohy := HvifToScreenY(pt.OutY);
      isSelNode := (path = FActivePath) and (ni = FSelectedNodeIdx);
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
end;

procedure TIomCanvasWidget.HandlePaint;
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
    Canvas.SetColor(COL_BORDER);
    Canvas.DrawRectangle(FIconOX - 1, FIconOY - 1, FIconSZ + 2, FIconSZ + 2);
    DrawControlOverlay;
  finally
    Canvas.EndDraw;
  end;
end;

procedure TIomCanvasWidget.HandleResize(awidth, aheight: TfpgCoord);
begin
  inherited HandleResize(awidth, aheight);
  if FIcon <> nil then
    FIcon.ClearCache;
  FIconDirty := (FDocument <> nil);
  Repaint;
end;


{ ── Hit-testing ──────────────────────────────────────────────────────────── }

function TIomCanvasWidget.HitTestNodes(AX, AY: Integer; out APath: TIomPath;
    out ANodeIdx: Integer; out ATarget: TDragTarget): Boolean;
var
  shape: TIomShape;
  pi, ni: Integer;
  path: TIomPath;
  pt: TIomPoint;
  sx, sy, dx, dy: Integer;
begin
  Result := False;
  APath := nil; ANodeIdx := -1; ATarget := dtNone;
  if (FDocument = nil) or (FSelectedShapeIdx < 0) or
     (FSelectedShapeIdx >= FDocument.ShapeCount) then
    Exit;
  shape := FDocument.Shapes[FSelectedShapeIdx];
  for pi := 0 to shape.PathCount - 1 do
  begin
    path := shape.Paths[pi];
    for ni := 0 to path.PointCount - 1 do
    begin
      pt := path.Points[ni];
      { Test anchor first (highest priority) }
      sx := HvifToScreenX(pt.X);  sy := HvifToScreenY(pt.Y);
      dx := AX - sx; dy := AY - sy;
      if (dx*dx + dy*dy) <= (HIT_RADIUS*HIT_RADIUS) then
      begin
        APath := path; ANodeIdx := ni; ATarget := dtAnchor;
        Result := True; Exit;
      end;
      { In handle }
      sx := HvifToScreenX(pt.InX); sy := HvifToScreenY(pt.InY);
      dx := AX - sx; dy := AY - sy;
      if (dx*dx + dy*dy) <= (HIT_RADIUS*HIT_RADIUS) then
      begin
        APath := path; ANodeIdx := ni; ATarget := dtInHandle;
        Result := True; Exit;
      end;
      { Out handle }
      sx := HvifToScreenX(pt.OutX); sy := HvifToScreenY(pt.OutY);
      dx := AX - sx; dy := AY - sy;
      if (dx*dx + dy*dy) <= (HIT_RADIUS*HIT_RADIUS) then
      begin
        APath := path; ANodeIdx := ni; ATarget := dtOutHandle;
        Result := True; Exit;
      end;
    end;
  end;
end;

function TIomCanvasWidget.HitTestSegments(AX, AY: Integer;
    out APath: TIomPath; out ASegmentIdx: Integer; out AT: Single): Boolean;
var
  shape: TIomShape;
  pi, ni, si, segCount: Integer;
  path: TIomPath;
  n0, n1: TIomPoint;
  t, bx, by, dx, dy, dist2, bestDist2: Single;
  sx0, sy0, sx1, sy1, sx2, sy2, sx3, sy3: Single;
  u: Single;
begin
  Result := False;
  APath := nil; ASegmentIdx := -1; AT := 0;
  if (FDocument = nil) or (FSelectedShapeIdx < 0) or
     (FSelectedShapeIdx >= FDocument.ShapeCount) then
    Exit;

  bestDist2 := Sqr(HIT_RADIUS);

  shape := FDocument.Shapes[FSelectedShapeIdx];
  for pi := 0 to shape.PathCount - 1 do
  begin
    path := shape.Paths[pi];
    if path.PointCount < 2 then
      Continue;

    if path.Closed then
      segCount := path.PointCount       { last segment wraps back to node 0 }
    else
      segCount := path.PointCount - 1;

    for ni := 0 to segCount - 1 do
    begin
      n0 := path.Points[ni];
      n1 := path.Points[(ni + 1) mod path.PointCount];

      { Convert segment control points to screen space for distance testing }
      sx0 := HvifToScreenX(n0.X);    sy0 := HvifToScreenY(n0.Y);
      sx1 := HvifToScreenX(n0.OutX); sy1 := HvifToScreenY(n0.OutY);
      sx2 := HvifToScreenX(n1.InX);  sy2 := HvifToScreenY(n1.InY);
      sx3 := HvifToScreenX(n1.X);    sy3 := HvifToScreenY(n1.Y);

      for si := 1 to SEG_SAMPLES - 1 do
      begin
        t := si / SEG_SAMPLES;
        u := 1.0 - t;
        { Cubic bezier evaluation }
        bx := u*u*u*sx0 + 3.0*u*u*t*sx1 + 3.0*u*t*t*sx2 + t*t*t*sx3;
        by := u*u*u*sy0 + 3.0*u*u*t*sy1 + 3.0*u*t*t*sy2 + t*t*t*sy3;
        dx := AX - bx;  dy := AY - by;
        dist2 := dx*dx + dy*dy;
        if dist2 < bestDist2 then
        begin
          bestDist2  := dist2;
          APath      := path;
          ASegmentIdx := ni;
          AT         := t;
          Result     := True;
        end;
      end;
    end;
  end;
end;


{ ── Keyboard handling ────────────────────────────────────────────────────── }

procedure TIomCanvasWidget.HandleKeyPress(var keycode: word;
    var shiftstate: TShiftState; var consumed: boolean);
var
  cmd: TIomCmdDeletePoint;
begin
  if (keycode = keyDelete) or (keycode = keyBackSpace) then
  begin
    if (FDocument <> nil) and (FSelectedNodeIdx >= 0) and
       (FActivePath <> nil) and
       (FActivePath.PointCount > MIN_PATH_NODES) then
    begin
      cmd := TIomCmdDeletePoint.Create(FActivePath, FSelectedNodeIdx);
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


{ ── Mouse event handling ─────────────────────────────────────────────────── }

procedure TIomCanvasWidget.HandleLMouseDown(x, y: integer;
    shiftstate: TShiftState);
var
  hitPath:   TIomPath;
  hitNode:   Integer;
  hitTarget: TDragTarget;
  segPath:   TIomPath;
  segIdx:    Integer;
  segT:      Single;
  n0, n1:    TIomPoint;
  prevIdx, nextIdx, insertIdx: Integer;
  newPt:     TIomPoint;
  prevAfter, nextAfter: TIomPoint;
  splitX, splitY: Single;
  prevOutX, prevOutY: Single;
  newInX,   newInY:   Single;
  newOutX,  newOutY:  Single;
  nextInX,  nextInY:  Single;
  cmd: TIomCmdAddPoint;
  pt: TIomPoint;
begin
  { Claim keyboard focus so Delete/Backspace reach HandleKeyPress }
  SetFocus;

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
    newPt        := Default(TIomPoint);
    newPt.X      := splitX;   newPt.Y      := splitY;
    newPt.InX    := newInX;   newPt.InY    := newInY;
    newPt.OutX   := newOutX;  newPt.OutY   := newOutY;
    newPt.Smooth := False;

    { Build the updated snapshots for adjacent nodes }
    prevAfter      := n0;
    prevAfter.OutX := prevOutX;  prevAfter.OutY := prevOutY;

    nextAfter      := n1;
    nextAfter.InX  := nextInX;   nextAfter.InY  := nextInY;

    cmd := TIomCmdAddPoint.Create(segPath, insertIdx, prevIdx, nextIdx,
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

procedure TIomCanvasWidget.HandleLMouseUp(x, y: integer;
    shiftstate: TShiftState);
var
  ptAfter: TIomPoint;
  cmd: TIomCommand;
begin
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
        cmd := TIomCmdMoveNode.Create(
                   FActivePath, FSelectedNodeIdx, FDragPtBefore, ptAfter);
      dtInHandle:
        cmd := TIomCmdMoveHandle.Create(
                   FActivePath, FSelectedNodeIdx, True, FDragPtBefore, ptAfter);
      dtOutHandle:
        cmd := TIomCmdMoveHandle.Create(
                   FActivePath, FSelectedNodeIdx, False, FDragPtBefore, ptAfter);
    else
      cmd := nil;
    end;
    if cmd <> nil then
      FDocument.UndoStack.Execute(cmd);
  end;

  FDragTarget := dtNone;
end;

procedure TIomCanvasWidget.HandleMouseMove(x, y: integer; btnstate: word;
    shiftstate: TShiftState);
var
  hvx, hvy: Single;
  newPt: TIomPoint;
begin
  if (FDragTarget = dtNone) or (FActivePath = nil) then
    Exit;

  newPt := FDragPtBefore;

  case FDragTarget of
    dtAnchor:
    begin
      hvx := ScreenToHvifX(x - FDragOffX);
      hvy := ScreenToHvifY(y - FDragOffY);
      newPt.InX  := hvx + (FDragPtBefore.InX  - FDragPtBefore.X);
      newPt.InY  := hvy + (FDragPtBefore.InY  - FDragPtBefore.Y);
      newPt.OutX := hvx + (FDragPtBefore.OutX - FDragPtBefore.X);
      newPt.OutY := hvy + (FDragPtBefore.OutY - FDragPtBefore.Y);
      newPt.X    := hvx;
      newPt.Y    := hvy;
    end;
    dtInHandle:
    begin
      newPt.InX := ScreenToHvifX(x - FDragOffX);
      newPt.InY := ScreenToHvifY(y - FDragOffY);
    end;
    dtOutHandle:
    begin
      newPt.OutX := ScreenToHvifX(x - FDragOffX);
      newPt.OutY := ScreenToHvifY(y - FDragOffY);
    end;
  end;

  FActivePath.Points[FSelectedNodeIdx] := newPt;
  FIconDirty := True;
  Repaint;
end;

end.
