unit vertex.wgt.gradienteditor;

{
  TVertexGradientEditor — modal dialog for editing a gradient style.

  Layout (top to bottom):
    "Gradient type:" label  +  type combo box (Linear/Radial/Diamond/Conic/XY/Square)
    TVertexGradRamp          — 200×24 colour bar with draggable stop handles
    "Colour:" label  +  colour swatch (click to pick)
    "Alpha:" label   +  trackbar (0-255)  +  spin edit
    "Offset:" label  +  read-only label (0.00–1.00)
    [Close] button

  All edits go through TVertexCmdSetGradientStop / TVertexCmdSetStyleType on the
  undo stack.  The editor holds weak (non-owning) references to the document and
  the style being edited.

  TVertexGradRamp behaviour:
    - Paints a horizontal gradient bar interpolating stop colours.
    - Small triangular handles are drawn below the bar at each stop's offset.
    - Left-click on the ramp area adds a new stop (interpolated colour).
    - Left-click on a handle selects it.
    - Mouse-drag moves the selected handle (committed on mouse-up).
    - Right-click or Del key on the selected handle removes it (≥ 3 stops needed).
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  fpg_base, fpg_main, fpg_widget, fpg_form,
  fpg_label, fpg_spinedit, fpg_trackbar, fpg_button, fpg_combobox,
  fpg_dialogs,
  fpg_hvif_model,
  fpg_vertex_document;


{ ── TVertexColorSwatch ───────────────────────────────────────────────────────── }
{ Small widget that paints a solid colour rectangle. Owns its HandlePaint so  }
{ repaints always show the current colour, unlike TfpgBevel which overwrites.  }

type
  TVertexColorSwatch = class(TfpgWidget)
  private
    FColor: TfpgColor;
  protected
    procedure HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Color: TfpgColor read FColor write FColor;
  end;


{ ── TVertexGradRamp ─────────────────────────────────────────────────────────── }
{ Custom widget that paints the gradient and provides handle interaction. }

type
  TVertexGradRamp = class(TfpgWidget)
  private
    FStyle:         TVertexStyle;       { weak ref; may be nil }
    FDocument:      TVertexDocument;    { weak ref; may be nil }
    FSelectedStop:  Integer;            { -1 = none }
    FDragging:      Boolean;
    FDragStartX:    Integer;
    FDragOrigOffset: Single;
    FOnStopSelected: TNotifyEvent;

    { Returns the X pixel position (0..Width-1) for a given stop offset. }
    function OffsetToX(AOffset: Single): Integer;
    { Returns the offset (0.0..1.0) for a given X pixel position. }
    function XToOffset(AX: Integer): Single;
    { Returns the stop index whose handle is within click range of AX, or -1. }
    function HitTestHandle(AX, AY: Integer): Integer;
    { Interpolate colour across current stops at normalised offset t. }
    function InterpolateColor(t: Single): THvifColor;
    { Paint the gradient bar (top part of widget). }
    procedure PaintGradientBar;
    { Paint the handle triangles (bottom part). }
    procedure PaintHandles;

  protected
    procedure HandlePaint; override;
    procedure HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure HandleRMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;

    property Style:         TVertexStyle    read FStyle        write FStyle;
    property Document:      TVertexDocument read FDocument     write FDocument;
    property SelectedStop:  Integer         read FSelectedStop write FSelectedStop;
    property OnStopSelected: TNotifyEvent   read FOnStopSelected write FOnStopSelected;
  end;


{ ── TVertexGradientEditor ───────────────────────────────────────────────────── }

type
  TVertexGradientEditor = class(TfpgForm)
  private
    FDocument:    TVertexDocument;    { weak ref }
    FStyle:       TVertexStyle;       { weak ref }
    FUpdating:    Boolean;
    FDragAlpha:   Boolean;
    FOrigAlpha:   Byte;

    { Type row }
    FLblType:     TfpgLabel;
    FTypeCombo:   TfpgComboBox;

    { Ramp }
    FGradRamp:    TVertexGradRamp;

    { Selected-stop controls }
    FLblColor:    TfpgLabel;
    FColorSwatch: TVertexColorSwatch; { clickable painted swatch }
    FSwatchColor: THvifColor;         { colour currently shown in the swatch }

    FLblAlpha:    TfpgLabel;
    FAlphaBar:    TfpgTrackBar;
    FAlphaSpin:   TfpgSpinEdit;

    FLblOffset:   TfpgLabel;
    FOffsetLabel: TfpgLabel;          { read-only display }

    FBtnClose:    TfpgButton;

    procedure SetupControls;
    procedure UpdateStopControls;

    { Event handlers }
    procedure TypeComboChanged(Sender: TObject);
    procedure SwatchClick(Sender: TObject);
    procedure AlphaBarChanged(Sender: TObject; APosition: integer);
    procedure AlphaBarMouseUp(Sender: TObject; AButton: TMouseButton;
                              AShift: TShiftState; const AMousePos: TPoint);
    procedure AlphaSpinChanged(Sender: TObject);
    procedure AlphaSpinExit(Sender: TObject);
    procedure StopSelected(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;

    procedure SetDocumentAndStyle(ADoc: TVertexDocument; AStyle: TVertexStyle);
    { Refresh after an undo/redo or external change. }
    procedure DocumentChanged;
  end;


implementation

{ ── Layout constants ─────────────────────────────────────────────────────────── }
const
  PAD        = 6;
  LBL_W      = 90;
  CTRL_H     = 24;
  RAMP_W     = 336;   { width of the gradient ramp widget }
  RAMP_H     = 36;    { total height: 24 bar + 12 handles }
  BAR_H      = 24;    { height of the colour bar portion }
  HDL_HALF   = 5;     { half-width of each handle triangle }
  RAMP_MARGIN = 8;    { left/right margin so edge handles are fully visible }
  SWATCH_W   = 44;
  SWATCH_H   = 28;
  COMBO_W    = 160;
  ASPIN_W    = 56;
  ALP_W      = 170;

  FORM_W    = PAD + RAMP_W + PAD;
  ROW0      = PAD;                              { type row }
  ROW1      = ROW0 + CTRL_H + PAD;             { ramp row }
  ROW2      = ROW1 + RAMP_H + PAD;             { colour swatch row }
  ROW3      = ROW2 + SWATCH_H + PAD;           { alpha row }
  ROW4      = ROW3 + CTRL_H + PAD;             { offset row }
  ROW5      = ROW4 + CTRL_H + PAD;             { close button row }
  FORM_H    = ROW5 + CTRL_H + PAD;


{ ── Helpers ──────────────────────────────────────────────────────────────────── }

function HvifColorToFpg(const C: THvifColor): TfpgColor;
begin
  Result := (TfpgColor($FF) shl 24) or (TfpgColor(C.R) shl 16) or
            (TfpgColor(C.G) shl 8)  or TfpgColor(C.B);
end;

procedure FpgColorToHvif(ACol: TfpgColor; out AHvif: THvifColor);
begin
  AHvif.R := Byte((ACol shr 16) and $FF);
  AHvif.G := Byte((ACol shr  8) and $FF);
  AHvif.B := Byte( ACol         and $FF);
  AHvif.A := 255;
end;

function LerpByte(a, b: Byte; t: Single): Byte;
begin
  Result := Byte(Round(a + (b - a) * t));
end;


{ ── TVertexColorSwatch ───────────────────────────────────────────────────────── }

constructor TVertexColorSwatch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor    := clBlack;
  Focusable := False;
end;

procedure TVertexColorSwatch.HandlePaint;
begin
  Canvas.BeginDraw;
  try
    Canvas.SetColor(FColor);
    Canvas.FillRectangle(0, 0, Width, Height);
    Canvas.SetColor($FF444444);
    Canvas.DrawRectangle(0, 0, Width, Height);
  finally
    Canvas.EndDraw;
  end;
end;


{ ── TVertexGradRamp ─────────────────────────────────────────────────────────── }

constructor TVertexGradRamp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyle        := nil;
  FDocument     := nil;
  FSelectedStop := -1;
  FDragging     := False;
  Focusable     := True;
end;

function TVertexGradRamp.OffsetToX(AOffset: Single): Integer;
var
  usableW: Integer;
begin
  usableW := Width - 2 * RAMP_MARGIN;
  if usableW < 1 then usableW := 1;
  Result := RAMP_MARGIN + Round(AOffset * (usableW - 1));
end;

function TVertexGradRamp.XToOffset(AX: Integer): Single;
var
  usableW: Integer;
begin
  usableW := Width - 2 * RAMP_MARGIN;
  if usableW <= 1 then
  begin
    Result := 0;
    Exit;
  end;
  Result := (AX - RAMP_MARGIN) / (usableW - 1);
  if Result < 0.0 then Result := 0.0;
  if Result > 1.0 then Result := 1.0;
end;

function TVertexGradRamp.HitTestHandle(AX, AY: Integer): Integer;
var
  i, hx: Integer;
begin
  Result := -1;
  if (FStyle = nil) or (AY < BAR_H - 2) then
    Exit;
  for i := 0 to FStyle.StopCount - 1 do
  begin
    hx := OffsetToX(FStyle.Stops[i].Offset);
    if Abs(AX - hx) <= HDL_HALF + 2 then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TVertexGradRamp.InterpolateColor(t: Single): THvifColor;
var
  i:           Integer;
  t0, t1, f:  Single;
  c0, c1:     THvifColor;
begin
  Result.R := 0; Result.G := 0; Result.B := 0; Result.A := 255;
  if (FStyle = nil) or (FStyle.StopCount = 0) then
    Exit;
  if FStyle.StopCount = 1 then
  begin
    Result := FStyle.Stops[0].Color;
    Exit;
  end;
  { Find the bracketing pair }
  for i := 0 to FStyle.StopCount - 2 do
  begin
    t0 := FStyle.Stops[i].Offset;
    t1 := FStyle.Stops[i + 1].Offset;
    if t <= t0 then
    begin
      Result := FStyle.Stops[i].Color;
      Exit;
    end;
    if t <= t1 then
    begin
      if t1 > t0 then
        f := (t - t0) / (t1 - t0)
      else
        f := 0;
      c0 := FStyle.Stops[i].Color;
      c1 := FStyle.Stops[i + 1].Color;
      Result.R := LerpByte(c0.R, c1.R, f);
      Result.G := LerpByte(c0.G, c1.G, f);
      Result.B := LerpByte(c0.B, c1.B, f);
      Result.A := LerpByte(c0.A, c1.A, f);
      Exit;
    end;
  end;
  { Past last stop }
  Result := FStyle.Stops[FStyle.StopCount - 1].Color;
end;

procedure TVertexGradRamp.PaintGradientBar;
var
  x:            Integer;
  col:          THvifColor;
  t:            Single;
  barX, barW:   Integer;
begin
  barX := RAMP_MARGIN;
  barW := Width - 2 * RAMP_MARGIN;
  if barW < 1 then barW := 1;

  { Fill side margins with the handle-area background }
  Canvas.SetColor($FFD0D0D0);
  Canvas.FillRectangle(0, 0, barX, BAR_H);
  Canvas.FillRectangle(barX + barW, 0, Width - (barX + barW), BAR_H);

  if (FStyle = nil) or (FStyle.StopCount = 0) then
  begin
    Canvas.SetColor($FF808080);
    Canvas.FillRectangle(barX, 0, barW, BAR_H);
    Exit;
  end;
  { Paint column by column within the usable bar area }
  for x := barX to barX + barW - 1 do
  begin
    if barW > 1 then
      t := (x - barX) / (barW - 1)
    else
      t := 0;
    col := InterpolateColor(t);
    Canvas.SetColor(HvifColorToFpg(col));
    Canvas.FillRectangle(x, 0, 1, BAR_H);
  end;
  { Border around bar only }
  Canvas.SetColor($FF444444);
  Canvas.DrawRectangle(barX, 0, barW, BAR_H);
end;

procedure TVertexGradRamp.PaintHandles;
var
  i, hx:  Integer;
  col:    TfpgColor;
begin
  { Background for handle area }
  Canvas.SetColor($FFD0D0D0);
  Canvas.FillRectangle(0, BAR_H, Width, Height - BAR_H);
  if FStyle = nil then
    Exit;
  for i := 0 to FStyle.StopCount - 1 do
  begin
    hx := OffsetToX(FStyle.Stops[i].Offset);
    if i = FSelectedStop then
      col := $FF0060FF   { highlight selected }
    else
      col := $FF333333;
    Canvas.SetColor(col);
    { Draw a simple downward-pointing triangle as handle }
    Canvas.DrawLine(hx, BAR_H + 1, hx - HDL_HALF, Height - 1);
    Canvas.DrawLine(hx, BAR_H + 1, hx + HDL_HALF, Height - 1);
    Canvas.DrawLine(hx - HDL_HALF, Height - 1, hx + HDL_HALF, Height - 1);
  end;
end;

procedure TVertexGradRamp.HandlePaint;
begin
  Canvas.BeginDraw;
  try
    PaintGradientBar;
    PaintHandles;
  finally
    Canvas.EndDraw;
  end;
end;

procedure TVertexGradRamp.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  hit:     Integer;
  newStop: TVertexGradientStop;
  cmd:     TVertexCmdSetGradientStop;
begin
  SetFocus;
  hit := HitTestHandle(x, y);
  if hit >= 0 then
  begin
    { Select this handle and start dragging }
    FSelectedStop    := hit;
    FDragging        := True;
    FDragStartX      := x;
    FDragOrigOffset  := FStyle.Stops[hit].Offset;
    Repaint;
    if Assigned(FOnStopSelected) then
      FOnStopSelected(Self);
  end
  else if (FStyle <> nil) and (FDocument <> nil) and (y < BAR_H) then
  begin
    { Add a new stop, inserted at the sorted position by offset }
    newStop.Offset := XToOffset(x);
    newStop.Color  := InterpolateColor(newStop.Offset);
    hit := 0;
    while (hit < FStyle.StopCount) and
          (FStyle.Stops[hit].Offset <= newStop.Offset) do
      Inc(hit);
    cmd := TVertexCmdSetGradientStop.Create(FStyle, gsaAdd, hit, newStop);
    FDocument.UndoStack.Execute(cmd);
    FSelectedStop := hit;
    Repaint;
    if Assigned(FOnStopSelected) then
      FOnStopSelected(Self);
  end;
end;

procedure TVertexGradRamp.HandleMouseMove(x, y: integer; btnstate: word;
    shiftstate: TShiftState);
var
  newOff: Single;
  stop:   TVertexGradientStop;
begin
  if not FDragging then
    Exit;
  if (FStyle = nil) or (FSelectedStop < 0) or
     (FSelectedStop >= FStyle.StopCount) then
    Exit;
  newOff := XToOffset(x);
  { Clamp to adjacent stops so the array stays sorted during drag }
  if FSelectedStop > 0 then
    if newOff < FStyle.Stops[FSelectedStop - 1].Offset then
      newOff := FStyle.Stops[FSelectedStop - 1].Offset;
  if FSelectedStop < FStyle.StopCount - 1 then
    if newOff > FStyle.Stops[FSelectedStop + 1].Offset then
      newOff := FStyle.Stops[FSelectedStop + 1].Offset;
  stop := FStyle.Stops[FSelectedStop];
  stop.Offset := newOff;
  FStyle.Stops[FSelectedStop] := stop;
  if FDocument <> nil then
    FDocument.NotifyChanged;
  Repaint;
end;

procedure TVertexGradRamp.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
var
  newOff:  Single;
  oldStop: TVertexGradientStop;
  newStop: TVertexGradientStop;
  cmd:     TVertexCmdSetGradientStop;
begin
  if not FDragging then
    Exit;
  FDragging := False;
  if (FStyle = nil) or (FDocument = nil) or
     (FSelectedStop < 0) or (FSelectedStop >= FStyle.StopCount) then
    Exit;
  newOff := XToOffset(x);
  { Apply same adjacent-stop clamp used in HandleMouseMove }
  if FSelectedStop > 0 then
    if newOff < FStyle.Stops[FSelectedStop - 1].Offset then
      newOff := FStyle.Stops[FSelectedStop - 1].Offset;
  if FSelectedStop < FStyle.StopCount - 1 then
    if newOff > FStyle.Stops[FSelectedStop + 1].Offset then
      newOff := FStyle.Stops[FSelectedStop + 1].Offset;
  { Only commit if actually moved }
  if Abs(newOff - FDragOrigOffset) < 0.0001 then
    Exit;
  { Build before/after snapshots }
  oldStop        := FStyle.Stops[FSelectedStop];
  newStop        := oldStop;
  oldStop.Offset := FDragOrigOffset;
  newStop.Offset := newOff;
  { The live mutation already happened in HandleMouseMove; restore original
    offset so Execute can reapply the change through the undo stack. }
  FStyle.Stops[FSelectedStop] := oldStop;
  cmd := TVertexCmdSetGradientStop.Create(FStyle, gsaUpdate,
           FSelectedStop, newStop);
  FDocument.UndoStack.Execute(cmd);
  Repaint;
  if Assigned(FOnStopSelected) then
    FOnStopSelected(Self);
end;

procedure TVertexGradRamp.HandleRMouseDown(x, y: integer; shiftstate: TShiftState);
var
  hit: Integer;
  cmd: TVertexCmdSetGradientStop;
  dummy: TVertexGradientStop;
begin
  if (FStyle = nil) or (FDocument = nil) then
    Exit;
  hit := HitTestHandle(x, y);
  if (hit < 0) or (FStyle.StopCount <= 2) then
    Exit;
  dummy := Default(TVertexGradientStop);
  cmd := TVertexCmdSetGradientStop.Create(FStyle, gsaRemove, hit, dummy);
  FDocument.UndoStack.Execute(cmd);
  if FSelectedStop >= FStyle.StopCount then
    FSelectedStop := FStyle.StopCount - 1;
  Repaint;
  if Assigned(FOnStopSelected) then
    FOnStopSelected(Self);
end;

procedure TVertexGradRamp.HandleKeyPress(var keycode: word;
    var shiftstate: TShiftState; var consumed: Boolean);
var
  cmd:   TVertexCmdSetGradientStop;
  dummy: TVertexGradientStop;
begin
  { Delete key removes selected handle (if ≥ 3 stops remain) }
  if (keycode = keyDelete) and (FStyle <> nil) and (FDocument <> nil) and
     (FSelectedStop >= 0) and (FStyle.StopCount > 2) then
  begin
    dummy := Default(TVertexGradientStop);
    cmd := TVertexCmdSetGradientStop.Create(FStyle, gsaRemove,
             FSelectedStop, dummy);
    FDocument.UndoStack.Execute(cmd);
    if FSelectedStop >= FStyle.StopCount then
      FSelectedStop := FStyle.StopCount - 1;
    Repaint;
    if Assigned(FOnStopSelected) then
      FOnStopSelected(Self);
    consumed := True;
  end;
end;


{ ── TVertexGradientEditor ───────────────────────────────────────────────────── }

constructor TVertexGradientEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument := nil;
  FStyle    := nil;
  FUpdating := False;
  FDragAlpha := False;
  WindowTitle := 'Gradient Editor';
  SetPosition(0, 0, FORM_W, FORM_H);
  Sizeable := False;
  SetupControls;
end;

procedure TVertexGradientEditor.SetupControls;
const
  LBL_X = PAD;
begin
  { -- Gradient type row -- }
  FLblType := TfpgLabel.Create(Self);
  FLblType.SetPosition(LBL_X, ROW0 + 4, LBL_W, CTRL_H);
  FLblType.Text := 'Gradient type:';

  FTypeCombo := TfpgComboBox.Create(Self);
  FTypeCombo.SetPosition(LBL_X + LBL_W + 2, ROW0, COMBO_W, CTRL_H);
  FTypeCombo.Items.Add('Linear');
  FTypeCombo.Items.Add('Radial');
  FTypeCombo.Items.Add('Diamond');
  FTypeCombo.Items.Add('Conic');
  FTypeCombo.Items.Add('XY');
  FTypeCombo.Items.Add('Square');
  FTypeCombo.FocusItem := 0;
  FTypeCombo.OnChange  := @TypeComboChanged;

  { -- Gradient ramp -- }
  FGradRamp := TVertexGradRamp.Create(Self);
  FGradRamp.SetPosition(LBL_X, ROW1, RAMP_W, RAMP_H);
  FGradRamp.OnStopSelected := @StopSelected;

  { -- Colour swatch row -- }
  FLblColor := TfpgLabel.Create(Self);
  FLblColor.SetPosition(LBL_X, ROW2 + 4, LBL_W, CTRL_H);
  FLblColor.Text := 'Colour:';

  FColorSwatch := TVertexColorSwatch.Create(Self);
  FColorSwatch.SetPosition(LBL_X + LBL_W + 2, ROW2, SWATCH_W, SWATCH_H);
  FColorSwatch.OnClick := @SwatchClick;
  FColorSwatch.MouseCursor := mcHand;

  { -- Alpha row -- }
  FLblAlpha := TfpgLabel.Create(Self);
  FLblAlpha.SetPosition(LBL_X, ROW3 + 4, LBL_W, CTRL_H);
  FLblAlpha.Text := 'Alpha:';

  FAlphaBar := TfpgTrackBar.Create(Self);
  FAlphaBar.SetPosition(LBL_X + LBL_W + 2, ROW3, ALP_W, CTRL_H);
  FAlphaBar.Min      := 0;
  FAlphaBar.Max      := 255;
  FAlphaBar.Position := 255;
  FAlphaBar.OnChange  := @AlphaBarChanged;
  FAlphaBar.OnMouseUp := @AlphaBarMouseUp;

  FAlphaSpin := TfpgSpinEdit.Create(Self);
  FAlphaSpin.SetPosition(LBL_X + LBL_W + 2 + ALP_W + 4, ROW3, ASPIN_W, CTRL_H);
  FAlphaSpin.MinValue := 0;
  FAlphaSpin.MaxValue := 255;
  FAlphaSpin.Value    := 255;
  FAlphaSpin.OnChange := @AlphaSpinChanged;
  FAlphaSpin.OnExit   := @AlphaSpinExit;

  { -- Offset row -- }
  FLblOffset := TfpgLabel.Create(Self);
  FLblOffset.SetPosition(LBL_X, ROW4 + 4, LBL_W, CTRL_H);
  FLblOffset.Text := 'Offset:';

  FOffsetLabel := TfpgLabel.Create(Self);
  FOffsetLabel.SetPosition(LBL_X + LBL_W + 2, ROW4 + 4, 80, CTRL_H);
  FOffsetLabel.Text := '—';

  { -- Close button -- }
  FBtnClose := TfpgButton.Create(Self);
  FBtnClose.SetPosition(FORM_W - 80 - PAD, ROW5, 80, CTRL_H);
  FBtnClose.Text    := 'Close';
  FBtnClose.OnClick := @BtnCloseClick;
end;

procedure TVertexGradientEditor.UpdateStopControls;
var
  idx: Integer;
  stop: TVertexGradientStop;
begin
  idx := FGradRamp.SelectedStop;
  if (FStyle = nil) or (idx < 0) or (idx >= FStyle.StopCount) then
  begin
    FLblColor.Enabled   := False;
    FColorSwatch.Enabled:= False;
    FLblAlpha.Enabled   := False;
    FAlphaBar.Enabled   := False;
    FAlphaSpin.Enabled  := False;
    FLblOffset.Enabled  := False;
    FOffsetLabel.Text   := '—';
    Exit;
  end;

  FLblColor.Enabled   := True;
  FColorSwatch.Enabled:= True;
  FLblAlpha.Enabled   := True;
  FAlphaBar.Enabled   := True;
  FAlphaSpin.Enabled  := True;
  FLblOffset.Enabled  := True;

  stop := FStyle.Stops[idx];
  FSwatchColor := stop.Color;
  FSwatchColor.A := 255;   { swatch shows RGB only }
  FColorSwatch.Color := HvifColorToFpg(FSwatchColor);
  FColorSwatch.Repaint;

  FUpdating := True;
  try
    FAlphaBar.Position := stop.Color.A;
    FAlphaSpin.Value   := stop.Color.A;
  finally
    FUpdating := False;
  end;
  FOrigAlpha     := stop.Color.A;
  FOffsetLabel.Text := Format('%.3f', [stop.Offset]);
end;

{ ── Event handlers ─────────────────────────────────────────────────────────── }

procedure TVertexGradientEditor.TypeComboChanged(Sender: TObject);
var
  newType: THvifGradientType;
begin
  if FUpdating or (FStyle = nil) or (FDocument = nil) then
    Exit;
  newType := THvifGradientType(FTypeCombo.FocusItem);
  if FStyle.GradientType = newType then
    Exit;
  { Direct mutation — no undo command for gradient type changes (minor) }
  FStyle.GradientType := newType;
  FDocument.NotifyChanged;
end;

procedure TVertexGradientEditor.SwatchClick(Sender: TObject);
var
  preset, picked: TfpgColor;
  idx:            Integer;
  oldStop, newStop: TVertexGradientStop;
  cmd:            TVertexCmdSetGradientStop;
begin
  if (FStyle = nil) or (FDocument = nil) then
    Exit;
  idx := FGradRamp.SelectedStop;
  if (idx < 0) or (idx >= FStyle.StopCount) then
    Exit;
  preset := HvifColorToFpg(FSwatchColor);
  picked := fpgSelectColorDialog(preset);
  if picked = preset then
    Exit;
  oldStop := FStyle.Stops[idx];
  newStop := oldStop;
  FpgColorToHvif(picked, newStop.Color);
  newStop.Color.A := oldStop.Color.A;  { preserve alpha }
  cmd := TVertexCmdSetGradientStop.Create(FStyle, gsaUpdate, idx, newStop);
  FDocument.UndoStack.Execute(cmd);
  FSwatchColor := newStop.Color;
  FSwatchColor.A := 255;
  FColorSwatch.Color := HvifColorToFpg(FSwatchColor);
  FColorSwatch.Repaint;
  FGradRamp.Repaint;
end;

procedure TVertexGradientEditor.AlphaBarChanged(Sender: TObject; APosition: integer);
var
  idx:  Integer;
  stop: TVertexGradientStop;
begin
  if FUpdating then
    Exit;
  FDragAlpha := True;
  FUpdating  := True;
  try
    FAlphaSpin.Value := APosition;
  finally
    FUpdating := False;
  end;
  idx := FGradRamp.SelectedStop;
  if (FStyle = nil) or (FDocument = nil) or
     (idx < 0) or (idx >= FStyle.StopCount) then
    Exit;
  stop        := FStyle.Stops[idx];
  stop.Color.A := Byte(APosition);
  FStyle.Stops[idx] := stop;
  FDocument.NotifyChanged;
end;

procedure TVertexGradientEditor.AlphaBarMouseUp(Sender: TObject;
    AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
var
  idx:             Integer;
  oldStop, newStop: TVertexGradientStop;
  cmd:             TVertexCmdSetGradientStop;
begin
  if not FDragAlpha then
    Exit;
  FDragAlpha := False;
  idx := FGradRamp.SelectedStop;
  if (FStyle = nil) or (FDocument = nil) or
     (idx < 0) or (idx >= FStyle.StopCount) then
    Exit;
  if FStyle.Stops[idx].Color.A = FOrigAlpha then
    Exit;
  oldStop        := FStyle.Stops[idx];
  newStop        := oldStop;
  oldStop.Color.A := FOrigAlpha;
  { Restore original so Execute reapplies through stack }
  FStyle.Stops[idx] := oldStop;
  cmd := TVertexCmdSetGradientStop.Create(FStyle, gsaUpdate, idx, newStop);
  FDocument.UndoStack.Execute(cmd);
  FOrigAlpha := newStop.Color.A;
end;

procedure TVertexGradientEditor.AlphaSpinChanged(Sender: TObject);
begin
  if FUpdating then
    Exit;
  FUpdating := True;
  try
    FAlphaBar.Position := FAlphaSpin.Value;
  finally
    FUpdating := False;
  end;
end;

procedure TVertexGradientEditor.AlphaSpinExit(Sender: TObject);
var
  idx:             Integer;
  oldStop, newStop: TVertexGradientStop;
  cmd:             TVertexCmdSetGradientStop;
begin
  idx := FGradRamp.SelectedStop;
  if (FStyle = nil) or (FDocument = nil) or
     (idx < 0) or (idx >= FStyle.StopCount) then
    Exit;
  if FStyle.Stops[idx].Color.A = FOrigAlpha then
    Exit;
  oldStop        := FStyle.Stops[idx];
  newStop        := oldStop;
  oldStop.Color.A := FOrigAlpha;
  FStyle.Stops[idx] := oldStop;
  cmd := TVertexCmdSetGradientStop.Create(FStyle, gsaUpdate, idx, newStop);
  FDocument.UndoStack.Execute(cmd);
  FOrigAlpha := newStop.Color.A;
end;

procedure TVertexGradientEditor.StopSelected(Sender: TObject);
begin
  UpdateStopControls;
end;

procedure TVertexGradientEditor.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

{ ── Public interface ─────────────────────────────────────────────────────────── }

procedure TVertexGradientEditor.SetDocumentAndStyle(ADoc: TVertexDocument;
    AStyle: TVertexStyle);
begin
  FDocument := ADoc;
  FStyle    := AStyle;
  FGradRamp.Document := ADoc;
  FGradRamp.Style    := AStyle;
  FGradRamp.SelectedStop := -1;

  FUpdating := True;
  try
    if (AStyle <> nil) then
      FTypeCombo.FocusItem := Ord(AStyle.GradientType)
    else
      FTypeCombo.FocusItem := 0;
  finally
    FUpdating := False;
  end;

  UpdateStopControls;
  FGradRamp.Repaint;
end;

procedure TVertexGradientEditor.DocumentChanged;
begin
  if FStyle = nil then
    Exit;
  FUpdating := True;
  try
    FTypeCombo.FocusItem := Ord(FStyle.GradientType);
  finally
    FUpdating := False;
  end;
  FGradRamp.Repaint;
  UpdateStopControls;
end;


end.
