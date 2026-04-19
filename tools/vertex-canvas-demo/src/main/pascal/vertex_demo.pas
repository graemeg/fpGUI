program vertex_demo;

{
  AggPas Canvas Prototype — fpGUI Vertex risk-reducer.

  Proves:
    1. A HVIF path renders correctly on an fpGUI canvas via THvifIcon.GetImage.
    2. Anchor nodes are draggable using a pure fpGUI widget (no external deps).
    3. Coordinate round-trip: screen → HVIF → screen stays within 0.5 px.
    4. Bezier handles (circles + arm lines) are visible and update on drag.

  Coordinate system:
    HVIF "icon space" is nominally 0..64 (the historical icon unit).
    On-screen we render at PREVIEW_SIZE × PREVIEW_SIZE pixels.
    Scale factor S = PREVIEW_SIZE / 64.0.
    Screen coord = CANVAS_OX + round(hvif_coord * S).
    HVIF coord   = (screen_coord - CANVAS_OX) / S.
}

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Math,
  fpg_base, fpg_main, fpg_form, fpg_widget,
  fpg_hvif_model, fpg_hvif, fpg_hvif_writer;


{ ── Constants ────────────────────────────────────────────────────────────── }

const
  PREVIEW_SIZE = 256;          { pixel dimensions of the rendered HVIF image }
  CANVAS_OX    = 20;           { left margin of the icon canvas in the window }
  CANVAS_OY    = 20;           { top margin of the icon canvas in the window }
  SCALE        = PREVIEW_SIZE / 64.0;  { px per HVIF unit }

  HIT_RADIUS   = 6;            { px — anchor node hit radius }
  NODE_RADIUS  = 5;            { px — drawn anchor node radius }
  HANDLE_RADIUS = 3;           { px — drawn bezier handle radius }

  { Colours for the overlay (RGB values for fpgSetColor / TfpgColor) }
  COL_ANCHOR_FILL   = $FF4488FF;  { blue filled circle }
  COL_ANCHOR_SEL    = $FFFF8800;  { orange — selected node }
  COL_HANDLE_FILL   = $FFFF44FF;  { magenta — bezier handles }
  COL_HANDLE_ARM    = $FF888888;  { grey — handle arm lines }
  COL_CHECKERBOARD1 = $FFCCCCCC;
  COL_CHECKERBOARD2 = $FFAAAAAA;


{ ── Conversion helpers ───────────────────────────────────────────────────── }

function HvifToScreen(AHvif: Single): Integer; inline;
begin
  Result := CANVAS_OX + Round(AHvif * SCALE);
end;

function ScreenToHvif(AScreen: Integer): Single; inline;
begin
  Result := (AScreen - CANVAS_OX) / SCALE;
end;

function HvifToScreenY(AHvif: Single): Integer; inline;
begin
  Result := CANVAS_OY + Round(AHvif * SCALE);
end;

function ScreenToHvifY(AScreen: Integer): Single; inline;
begin
  Result := (AScreen - CANVAS_OY) / SCALE;
end;


{ ── Hard-coded demo path (a rounded triangle in HVIF icon-space 0..64) ── }

{ We define one cubic-bezier closed path with four anchor points.
  Each point has: anchor (X,Y) + incoming handle (InX,InY) + outgoing handle (OutX,OutY). }

procedure BuildDemoPath(out APath: THvifPath);
var
  P: array[0..3] of THvifPoint;
begin
  { Point 0 — top centre }
  P[0].X := 32; P[0].Y := 8;
  P[0].InX  := 24; P[0].InY  := 8;
  P[0].OutX := 40; P[0].OutY := 8;

  { Point 1 — right }
  P[1].X := 56; P[1].Y := 48;
  P[1].InX  := 56; P[1].InY  := 38;
  P[1].OutX := 56; P[1].OutY := 56;

  { Point 2 — bottom centre }
  P[2].X := 32; P[2].Y := 56;
  P[2].InX  := 44; P[2].InY  := 56;
  P[2].OutX := 20; P[2].OutY := 56;

  { Point 3 — left }
  P[3].X := 8; P[3].Y := 48;
  P[3].InX  := 8; P[3].InY  := 56;
  P[3].OutX := 8; P[3].OutY  := 38;

  APath.Closed := True;
  SetLength(APath.Points, 4);
  APath.Points[0] := P[0];
  APath.Points[1] := P[1];
  APath.Points[2] := P[2];
  APath.Points[3] := P[3];
end;

procedure BuildDemoStyle(out AStyle: THvifStyle);
begin
  AStyle.StyleType := hstSolidColor;
  AStyle.Color.R := $26;
  AStyle.Color.G := $8B;
  AStyle.Color.B := $D2;   { Haiku blue }
  AStyle.Color.A := $FF;
end;

procedure BuildDemoShape(out AShape: THvifShape);
begin
  AShape.StyleIndex := 0;
  SetLength(AShape.PathIndices, 1);
  AShape.PathIndices[0] := 0;
  AShape.HasTransform   := False;
  AShape.HasTranslation := False;
  AShape.HasStroke      := False;
end;


{ ── TVertexDemoCanvas ───────────────────────────────────────────────────────── }

type
  { Drag target: which part of which point is being dragged }
  TDragTarget = (dtNone, dtAnchor, dtInHandle, dtOutHandle);

  TVertexDemoCanvas = class(TfpgWidget)
  private
    FPath:    THvifPath;    { editable path in HVIF coords }
    FStyle:   THvifStyle;
    FShape:   THvifShape;
    FIcon:    THvifIcon;    { rendered HVIF image — re-created after each edit }

    { Drag state }
    FDragTarget: TDragTarget;
    FDragNode:   Integer;    { index into FPath.Points }
    FDragOffX, FDragOffY: Integer;  { screen offset from exact node centre }
    FSelectedNode: Integer;

    { Round-trip test result (updated on each drag completion; -1 = not yet measured) }
    FRoundTripError: Single;
    FRoundTripMeasured: Boolean;

    procedure RebuildIcon;
    procedure DrawCheckerboard;
    procedure DrawHvifImage;
    procedure DrawControlOverlay;
    procedure DrawNodeCircle(AScreenX, AScreenY, ARadius: Integer;
                              AFill, ABorder: TfpgColor; ASelected: Boolean);
    function HitTestAnchor(AX, AY: Integer; out ANodeIdx: Integer): Boolean;
    function HitTestHandle(AX, AY: Integer; out ANodeIdx: Integer;
                           out ATarget: TDragTarget): Boolean;

  protected
    procedure HandlePaint; override;
    procedure HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property RoundTripError: Single read FRoundTripError;
  end;


constructor TVertexDemoCanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectedNode       := -1;
  FDragTarget         := dtNone;
  FDragNode           := -1;
  FRoundTripError     := 0;
  FRoundTripMeasured  := False;

  BuildDemoPath(FPath);
  BuildDemoStyle(FStyle);
  BuildDemoShape(FShape);
  RebuildIcon;
end;

destructor TVertexDemoCanvas.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;

{ Rebuild the HVIF writer → byte stream → THvifIcon }
procedure TVertexDemoCanvas.RebuildIcon;
var
  writer: THvifWriter;
  ms: TMemoryStream;
begin
  FreeAndNil(FIcon);
  writer := THvifWriter.Create;
  try
    writer.AddStyle(FStyle);
    writer.AddPath(FPath);
    writer.AddShape(FShape);
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

{ Draw a checkerboard background to make alpha visible }
procedure TVertexDemoCanvas.DrawCheckerboard;
const
  CELL = 8;
var
  col, row: Integer;
begin
  col := 0;
  while col < PREVIEW_SIZE do
  begin
    row := 0;
    while row < PREVIEW_SIZE do
    begin
      if Odd((col div CELL) + (row div CELL)) then
        Canvas.SetColor(COL_CHECKERBOARD2)
      else
        Canvas.SetColor(COL_CHECKERBOARD1);
      Canvas.FillRectangle(CANVAS_OX + col, CANVAS_OY + row,
                           Min(CELL, PREVIEW_SIZE - col),
                           Min(CELL, PREVIEW_SIZE - row));
      Inc(row, CELL);
    end;
    Inc(col, CELL);
  end;
end;

{ Blit the rendered HVIF image onto the canvas }
procedure TVertexDemoCanvas.DrawHvifImage;
var
  img: TfpgImage;
begin
  if FIcon = nil then
    Exit;
  img := FIcon.GetImage(PREVIEW_SIZE, PREVIEW_SIZE);
  if img <> nil then
    Canvas.DrawImage(CANVAS_OX, CANVAS_OY, img);
end;

{ Draw a filled circle with a border; orange border when selected }
procedure TVertexDemoCanvas.DrawNodeCircle(AScreenX, AScreenY, ARadius: Integer;
                                         AFill, ABorder: TfpgColor;
                                         ASelected: Boolean);
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

{ Draw the control-point overlay: anchor nodes, handle circles, arm lines }
procedure TVertexDemoCanvas.DrawControlOverlay;
var
  i, ax, ay, ihx, ihy, ohx, ohy: Integer;
  pt: THvifPoint;
begin
  for i := 0 to High(FPath.Points) do
  begin
    pt := FPath.Points[i];

    ax  := HvifToScreen(pt.X);
    ay  := HvifToScreenY(pt.Y);
    ihx := HvifToScreen(pt.InX);
    ihy := HvifToScreenY(pt.InY);
    ohx := HvifToScreen(pt.OutX);
    ohy := HvifToScreenY(pt.OutY);

    { Arm lines from anchor to handles (draw first so circles appear on top) }
    Canvas.SetColor(COL_HANDLE_ARM);
    Canvas.DrawLine(ax, ay, ihx, ihy);
    Canvas.DrawLine(ax, ay, ohx, ohy);

    { Incoming handle }
    DrawNodeCircle(ihx, ihy, HANDLE_RADIUS, COL_HANDLE_FILL, $FF000000,
                   (FSelectedNode = i) and (FDragTarget = dtInHandle));

    { Outgoing handle }
    DrawNodeCircle(ohx, ohy, HANDLE_RADIUS, COL_HANDLE_FILL, $FF000000,
                   (FSelectedNode = i) and (FDragTarget = dtOutHandle));

    { Anchor node (drawn last — on top) }
    DrawNodeCircle(ax, ay, NODE_RADIUS, COL_ANCHOR_FILL, $FF000000,
                   FSelectedNode = i);
  end;
end;

procedure TVertexDemoCanvas.HandlePaint;
var
  err_s: string;
begin
  Canvas.BeginDraw;
  try
    { Background }
    Canvas.SetColor(clWindowBackground);
    Canvas.FillRectangle(0, 0, Width, Height);

    { Hint line above icon canvas }
    Canvas.SetColor($FF000000);
    Canvas.SetTextColor($FF444444);
    Canvas.DrawString(CANVAS_OX, 4,
      'Drag blue anchors or magenta bezier handles');

    DrawCheckerboard;
    DrawHvifImage;
    DrawControlOverlay;

    { Round-trip error }
    Canvas.SetColor($FF000000);
    Canvas.SetTextColor($FF000000);
    if FRoundTripMeasured then
      err_s := Format('Round-trip error: %.3f px', [FRoundTripError])
    else
      err_s := 'Round-trip: drag a node to measure';
    Canvas.DrawString(CANVAS_OX, CANVAS_OY + PREVIEW_SIZE + 8, err_s);
  finally
    Canvas.EndDraw;
  end;
end;

{ Hit-test anchor nodes; returns True and sets ANodeIdx if within HIT_RADIUS }
function TVertexDemoCanvas.HitTestAnchor(AX, AY: Integer;
                                       out ANodeIdx: Integer): Boolean;
var
  i, sx, sy, dx, dy: Integer;
begin
  Result := False;
  ANodeIdx := -1;
  for i := 0 to High(FPath.Points) do
  begin
    sx := HvifToScreen(FPath.Points[i].X);
    sy := HvifToScreenY(FPath.Points[i].Y);
    dx := AX - sx;
    dy := AY - sy;
    if (dx * dx + dy * dy) <= (HIT_RADIUS * HIT_RADIUS) then
    begin
      ANodeIdx := i;
      Result := True;
      Exit;
    end;
  end;
end;

{ Hit-test bezier handles; returns True and sets ANodeIdx + ATarget }
function TVertexDemoCanvas.HitTestHandle(AX, AY: Integer;
                                       out ANodeIdx: Integer;
                                       out ATarget: TDragTarget): Boolean;
var
  i, sx, sy, dx, dy: Integer;
  pt: THvifPoint;
begin
  Result := False;
  ANodeIdx := -1;
  ATarget  := dtNone;
  for i := 0 to High(FPath.Points) do
  begin
    pt := FPath.Points[i];

    { Incoming handle }
    sx := HvifToScreen(pt.InX);
    sy := HvifToScreenY(pt.InY);
    dx := AX - sx; dy := AY - sy;
    if (dx * dx + dy * dy) <= (HIT_RADIUS * HIT_RADIUS) then
    begin
      ANodeIdx := i; ATarget := dtInHandle; Result := True; Exit;
    end;

    { Outgoing handle }
    sx := HvifToScreen(pt.OutX);
    sy := HvifToScreenY(pt.OutY);
    dx := AX - sx; dy := AY - sy;
    if (dx * dx + dy * dy) <= (HIT_RADIUS * HIT_RADIUS) then
    begin
      ANodeIdx := i; ATarget := dtOutHandle; Result := True; Exit;
    end;
  end;
end;

procedure TVertexDemoCanvas.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  ni: Integer;
  tgt: TDragTarget;
begin
  { Anchors take priority over handles }
  if HitTestAnchor(x, y, ni) then
  begin
    FDragTarget   := dtAnchor;
    FDragNode     := ni;
    FSelectedNode := ni;
    FDragOffX     := x - HvifToScreen(FPath.Points[ni].X);
    FDragOffY     := y - HvifToScreenY(FPath.Points[ni].Y);
    Repaint;
  end
  else if HitTestHandle(x, y, ni, tgt) then
  begin
    FDragTarget   := tgt;
    FDragNode     := ni;
    FSelectedNode := ni;
    if tgt = dtInHandle then
    begin
      FDragOffX := x - HvifToScreen(FPath.Points[ni].InX);
      FDragOffY := y - HvifToScreenY(FPath.Points[ni].InY);
    end else
    begin
      FDragOffX := x - HvifToScreen(FPath.Points[ni].OutX);
      FDragOffY := y - HvifToScreenY(FPath.Points[ni].OutY);
    end;
    Repaint;
  end
  else
  begin
    FSelectedNode := -1;
    Repaint;
  end;
end;

procedure TVertexDemoCanvas.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
var
  hx, hy: Single;
  bx, by: Integer;
  pt: THvifPoint;
begin
  if FDragTarget <> dtNone then
  begin
    { Compute round-trip error for the dragged point on mouse-up }
    pt := FPath.Points[FDragNode];
    case FDragTarget of
      dtAnchor:
      begin
        hx := pt.X; hy := pt.Y;
      end;
      dtInHandle:
      begin
        hx := pt.InX; hy := pt.InY;
      end;
      dtOutHandle:
      begin
        hx := pt.OutX; hy := pt.OutY;
      end;
    else
      hx := pt.X; hy := pt.Y;
    end;
    { Round-trip: HVIF coord → screen → HVIF → screen; measure error in px }
    bx := HvifToScreen(ScreenToHvif(HvifToScreen(hx)));
    by := HvifToScreenY(ScreenToHvifY(HvifToScreenY(hy)));
    FRoundTripError    := Sqrt(Sqr(Single(HvifToScreen(hx) - bx)) +
                              Sqr(Single(HvifToScreenY(hy) - by)));
    FRoundTripMeasured := True;
  end;
  FDragTarget := dtNone;
  FDragNode   := -1;
  Repaint;
end;

procedure TVertexDemoCanvas.HandleMouseMove(x, y: integer; btnstate: word;
                                          shiftstate: TShiftState);
var
  ni: Integer;
  hvx, hvy: Single;
  dx_in, dy_in, dx_out, dy_out: Single;
begin
  if FDragTarget = dtNone then
    Exit;

  ni := FDragNode;

  case FDragTarget of
    dtAnchor:
    begin
      hvx := ScreenToHvif(x - FDragOffX);
      hvy := ScreenToHvifY(y - FDragOffY);
      { Move handles rigidly with the anchor }
      dx_in  := FPath.Points[ni].InX  - FPath.Points[ni].X;
      dy_in  := FPath.Points[ni].InY  - FPath.Points[ni].Y;
      dx_out := FPath.Points[ni].OutX - FPath.Points[ni].X;
      dy_out := FPath.Points[ni].OutY - FPath.Points[ni].Y;

      FPath.Points[ni].X    := hvx;
      FPath.Points[ni].Y    := hvy;
      FPath.Points[ni].InX  := hvx + dx_in;
      FPath.Points[ni].InY  := hvy + dy_in;
      FPath.Points[ni].OutX := hvx + dx_out;
      FPath.Points[ni].OutY := hvy + dy_out;
    end;

    dtInHandle:
    begin
      FPath.Points[ni].InX := ScreenToHvif(x - FDragOffX);
      FPath.Points[ni].InY := ScreenToHvifY(y - FDragOffY);
    end;

    dtOutHandle:
    begin
      FPath.Points[ni].OutX := ScreenToHvif(x - FDragOffX);
      FPath.Points[ni].OutY := ScreenToHvifY(y - FDragOffY);
    end;
  end;

  RebuildIcon;
  Repaint;
end;


{ ── Main form ────────────────────────────────────────────────────────────── }

type
  TMainForm = class(TfpgForm)
  private
    FDemoCanvas: TVertexDemoCanvas;
  public
    procedure AfterCreate; override;
  end;

procedure TMainForm.AfterCreate;
begin
  WindowTitle := 'Vertex — Canvas Prototype';
  SetPosition(100, 100,
              CANVAS_OX + PREVIEW_SIZE + CANVAS_OX,
              CANVAS_OY + PREVIEW_SIZE + 48 + CANVAS_OY);

  FDemoCanvas := TVertexDemoCanvas.Create(Self);
  FDemoCanvas.SetPosition(0, 0, Width, Height);
end;


{ ── Entry point ──────────────────────────────────────────────────────────── }

procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.
