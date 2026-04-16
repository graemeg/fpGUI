unit vertex.wgt.stylepanel;

{
  TVertexStylePanel — shows and edits the style of the currently selected shape.

  Step #7: solid-colour style editing with undo support (no Apply button).
  Step #10: gradient style type switching + gradient preview + editor popup.

  Controls:
    FTypeCombo — "Solid" / "Gradient" combo; fires TVertexCmdSetStyleType on change.

    Solid-colour controls (visible when StyleType = solid):
      FSwatch   — clickable colour rectangle; opens fpgSelectColorDialog on click.
      FHexEdit  — "#RRGGBB" text field; parsed and committed on focus-loss.
      FAlphaBar — horizontal trackbar; direct mutation for live preview, committed
                  on mouse-up (same model as canvas node drag).
      FAlphaSpin — numeric spin edit for alpha; committed on focus-loss.

    Gradient controls (visible when StyleType = gradient):
      FGradSwatch  — read-only painted preview of the gradient (click opens editor).
      FBtnEditGrad — "Edit gradient…" button.

  Commit model (identical to canvas drag):
    Every completed gesture creates one TVertexCmdSetStyleColour entry on the undo
    stack.  In-progress drags (alpha trackbar) mutate FStyle.Color directly and
    call FDocument.NotifyChanged so the canvas repaints live — no undo entry is
    added until the user releases the slider.

    FOrigColor is the "last committed" snapshot.  It advances each time a commit
    succeeds, so each distinct gesture gets its own undo step.

  Swatch behaviour:
    Click → colour dialog → on OK: RGB committed immediately; alpha unchanged.
    Cancel → no change.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_widget, fpg_panel,
  fpg_label, fpg_spinedit, fpg_edit, fpg_trackbar,
  fpg_combobox, fpg_button,
  fpg_dialogs,
  fpg_hvif_model,
  fpg_vertex_document,
  vertex.wgt.gradienteditor;


type
  { ── TVertexColorSwatch ─────────────────────────────────────────────────────── }
  { Clickable colour preview rectangle.  Alpha is shown separately by the
    trackbar; the swatch always fills with the solid RGB. }
  TVertexColorSwatch = class(TfpgWidget)
  private
    FColor:   THvifColor;
    FOnClick: TNotifyEvent;
    procedure SetColor(const AValue: THvifColor);
  protected
    procedure HandlePaint; override;
    procedure HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Color:   THvifColor   read FColor   write SetColor;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;


  { ── TVertexGradientSwatch ──────────────────────────────────────────────────── }
  { Clickable gradient preview bar.  Paints a horizontal colour interpolation of
    the style's gradient stops, or a grey bar when the style has no stops. }
  TVertexGradientSwatch = class(TfpgWidget)
  private
    FStyle:   TVertexStyle;    { weak ref; may be nil }
    FOnClick: TNotifyEvent;
    function InterpolateColor(t: Single): THvifColor;
  protected
    procedure HandlePaint; override;
    procedure HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Style:   TVertexStyle read FStyle   write FStyle;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;


  { ── TVertexStylePanel ──────────────────────────────────────────────────────── }
  TVertexStylePanel = class(TfpgBevel)
  private
    FDocument:    TVertexDocument;   { not owned }
    FStyle:       TVertexStyle;      { current style, not owned; nil = no selection }
    FOrigColor:   THvifColor;     { last-committed colour — undo baseline }
    FDragAlpha:   Boolean;        { True while the user is dragging FAlphaBar }
    FUpdating:    Boolean;        { guards recursive OnChange loops }

    FLblHeader:   TfpgLabel;
    FLblType:     TfpgLabel;      { "No shape selected" or "Solid colour" / "Gradient" }

    { Style type selector }
    FTypeCombo:   TfpgComboBox;

    { Solid-colour controls }
    FSwatch:      TVertexColorSwatch;
    FHexEdit:     TfpgEdit;
    FLblA:        TfpgLabel;
    FAlphaBar:    TfpgTrackBar;
    FAlphaSpin:   TfpgSpinEdit;

    { Gradient controls }
    FGradSwatch:  TVertexGradientSwatch;
    FBtnEditGrad: TfpgButton;

    { Gradient editor popup (owned by this panel) }
    FGradEditor:  TVertexGradientEditor;

    procedure SetupControls;
    procedure UpdateControls;
    procedure ShowSolidControls(AVisible: Boolean);
    procedure ShowGradientControls(AVisible: Boolean);

    { Commit helpers }
    procedure CommitColor(const ANew: THvifColor);

    { Control event handlers }
    procedure TypeComboChanged(Sender: TObject);
    procedure SwatchClick(Sender: TObject);
    procedure HexEditExit(Sender: TObject);
    procedure AlphaBarChanged(Sender: TObject; APosition: integer);
    procedure AlphaBarMouseUp(Sender: TObject; AButton: TMouseButton;
                              AShift: TShiftState; const AMousePos: TPoint);
    procedure AlphaSpinChanged(Sender: TObject);
    procedure AlphaSpinExit(Sender: TObject);
    procedure GradSwatchClick(Sender: TObject);
    procedure BtnEditGradClick(Sender: TObject);

    { Read the live RGBA values from the editing controls }
    function  CurrentColor: THvifColor;
    { Push a colour into all three editing controls atomically }
    procedure LoadColorToControls(const AColor: THvifColor);

  public
    constructor Create(AOwner: TComponent); override;

    { Connect to the document. Must be called before SetStyle. }
    procedure SetDocument(ADoc: TVertexDocument);

    { Show the given style. Pass nil to clear / show "no selection". }
    procedure SetStyle(AStyle: TVertexStyle);

    { Refresh controls from the current style after an undo/redo. }
    procedure DocumentChanged;
  end;


implementation


{ ── Layout constants ─────────────────────────────────────────────────────── }
const
  LBL_X     = 4;
  SPH       = 24;   { standard control height }

  SW_W      = 44;   { colour swatch width }
  SW_H      = 28;   { colour swatch height }
  HEX_X     = 52;   { hex edit left edge }
  HEX_W     = 162;  { hex edit width }

  ALP_LBL_W = 20;   { "A:" label width }
  ALP_X     = LBL_X + ALP_LBL_W + 2;
  ALP_W     = 124;  { trackbar width }
  ASPIN_X   = ALP_X + ALP_W + 4;
  ASPIN_W   = 54;   { alpha spin width }

  COMBO_W   = 210;  { type combo width }

  ROW0      = 4;
  ROW1      = 26;
  ROW2      = 48;   { type combo row }
  ROW3      = ROW2 + SPH + 6;             { colour row }
  ROW4      = ROW3 + SW_H + 6;            { alpha row }
  PANEL_H   = ROW4 + SPH  + 6;            { total panel height }


{ ── Helpers ──────────────────────────────────────────────────────────────────── }

procedure FpgColorToHvifRGB(ACol: TfpgColor; out R, G, B: Byte);
begin
  R := Byte((ACol shr 16) and $FF);
  G := Byte((ACol shr  8) and $FF);
  B := Byte( ACol         and $FF);
end;

function HvifColorToFpg(const C: THvifColor): TfpgColor;
begin
  Result := (TfpgColor($FF) shl 24) or (TfpgColor(C.R) shl 16) or
            (TfpgColor(C.G) shl 8)  or TfpgColor(C.B);
end;

function LerpByte(a, b: Byte; t: Single): Byte;
begin
  Result := Byte(Round(a + (b - a) * t));
end;


{ ── TVertexColorSwatch ─────────────────────────────────────────────────────── }

constructor TVertexColorSwatch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor      := Default(THvifColor);
  FColor.A    := 255;
  MouseCursor := mcHand;
end;

procedure TVertexColorSwatch.SetColor(const AValue: THvifColor);
begin
  FColor := AValue;
  Repaint;
end;

procedure TVertexColorSwatch.HandlePaint;
var
  col: TfpgColor;
begin
  Canvas.BeginDraw;
  try
    col := ($FF shl 24) or (LongWord(FColor.R) shl 16) or
           (LongWord(FColor.G) shl 8) or LongWord(FColor.B);
    Canvas.SetColor(col);
    Canvas.FillRectangle(0, 0, Width, Height);
    Canvas.SetColor($FF444444);
    Canvas.DrawRectangle(0, 0, Width, Height);
  finally
    Canvas.EndDraw;
  end;
end;

procedure TVertexColorSwatch.HandleLMouseDown(x, y: integer;
    shiftstate: TShiftState);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;


{ ── TVertexGradientSwatch ──────────────────────────────────────────────────── }

constructor TVertexGradientSwatch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyle      := nil;
  MouseCursor := mcHand;
end;

function TVertexGradientSwatch.InterpolateColor(t: Single): THvifColor;
var
  i:          Integer;
  t0, t1, f: Single;
  c0, c1:    THvifColor;
begin
  Result.R := 128; Result.G := 128; Result.B := 128; Result.A := 255;
  if (FStyle = nil) or (FStyle.StopCount = 0) then
    Exit;
  if FStyle.StopCount = 1 then
  begin
    Result := FStyle.Stops[0].Color;
    Exit;
  end;
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
  Result := FStyle.Stops[FStyle.StopCount - 1].Color;
end;

procedure TVertexGradientSwatch.HandlePaint;
var
  x:   Integer;
  col: THvifColor;
  t:   Single;
begin
  Canvas.BeginDraw;
  try
    if (FStyle = nil) or (FStyle.StopCount = 0) then
    begin
      Canvas.SetColor($FF808080);
      Canvas.FillRectangle(0, 0, Width, Height);
    end
    else
    begin
      for x := 0 to Width - 1 do
      begin
        if Width > 1 then
          t := x / (Width - 1)
        else
          t := 0;
        col := InterpolateColor(t);
        Canvas.SetColor(HvifColorToFpg(col));
        Canvas.FillRectangle(x, 0, 1, Height);
      end;
    end;
    Canvas.SetColor($FF444444);
    Canvas.DrawRectangle(0, 0, Width, Height);
  finally
    Canvas.EndDraw;
  end;
end;

procedure TVertexGradientSwatch.HandleLMouseDown(x, y: integer;
    shiftstate: TShiftState);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;


{ ── TVertexStylePanel ──────────────────────────────────────────────────────── }

constructor TVertexStylePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument    := nil;
  FStyle       := nil;
  FDragAlpha   := False;
  FUpdating    := False;
  FGradEditor  := nil;
  PreferredSize := fpgSize(220, PANEL_H);
  SetupControls;
  UpdateControls;
end;

procedure TVertexStylePanel.SetupControls;
begin
  FLblHeader := TfpgLabel.Create(Self);
  FLblHeader.SetPosition(LBL_X, ROW0, 210, 18);
  FLblHeader.Text     := 'Style Properties';
  FLblHeader.FontDesc := '#Label1:bold';

  FLblType := TfpgLabel.Create(Self);
  FLblType.SetPosition(LBL_X, ROW1, 210, 18);
  FLblType.Text := 'No shape selected';

  { Style type combo (Solid / Gradient) }
  FTypeCombo := TfpgComboBox.Create(Self);
  FTypeCombo.SetPosition(LBL_X, ROW2, COMBO_W, SPH);
  FTypeCombo.Items.Add('Solid colour');
  FTypeCombo.Items.Add('Gradient');
  FTypeCombo.FocusItem := 0;
  FTypeCombo.OnChange  := @TypeComboChanged;
  FTypeCombo.Enabled   := False;

  { Colour row: swatch + hex edit }
  FSwatch := TVertexColorSwatch.Create(Self);
  FSwatch.SetPosition(LBL_X, ROW3, SW_W, SW_H);
  FSwatch.OnClick := @SwatchClick;

  FHexEdit := TfpgEdit.Create(Self);
  FHexEdit.SetPosition(HEX_X, ROW3 + 2, HEX_W, SPH);
  FHexEdit.Text   := '';
  FHexEdit.OnExit := @HexEditExit;

  { Alpha row: "A:" label + trackbar + spin }
  FLblA := TfpgLabel.Create(Self);
  FLblA.SetPosition(LBL_X, ROW4 + 4, ALP_LBL_W, 18);
  FLblA.Text := 'A:';

  FAlphaBar := TfpgTrackBar.Create(Self);
  FAlphaBar.SetPosition(ALP_X, ROW4, ALP_W, SPH);
  FAlphaBar.Min      := 0;
  FAlphaBar.Max      := 255;
  FAlphaBar.Position := 255;
  FAlphaBar.OnChange  := @AlphaBarChanged;
  FAlphaBar.OnMouseUp := @AlphaBarMouseUp;

  FAlphaSpin := TfpgSpinEdit.Create(Self);
  FAlphaSpin.SetPosition(ASPIN_X, ROW4, ASPIN_W, SPH);
  FAlphaSpin.MinValue := 0;
  FAlphaSpin.MaxValue := 255;
  FAlphaSpin.Value    := 255;
  FAlphaSpin.OnChange := @AlphaSpinChanged;
  FAlphaSpin.OnExit   := @AlphaSpinExit;

  { Gradient preview swatch }
  FGradSwatch := TVertexGradientSwatch.Create(Self);
  FGradSwatch.SetPosition(LBL_X, ROW3, SW_W + HEX_W + 4, SW_H);
  FGradSwatch.OnClick := @GradSwatchClick;
  FGradSwatch.Visible := False;

  { "Edit gradient…" button }
  FBtnEditGrad := TfpgButton.Create(Self);
  FBtnEditGrad.SetPosition(LBL_X, ROW4, 120, SPH);
  FBtnEditGrad.Text    := 'Edit gradient…';
  FBtnEditGrad.OnClick := @BtnEditGradClick;
  FBtnEditGrad.Visible := False;
end;

procedure TVertexStylePanel.ShowSolidControls(AVisible: Boolean);
begin
  FSwatch.Visible   := AVisible;
  FHexEdit.Visible  := AVisible;
  FLblA.Visible     := AVisible;
  FAlphaBar.Visible := AVisible;
  FAlphaSpin.Visible:= AVisible;
end;

procedure TVertexStylePanel.ShowGradientControls(AVisible: Boolean);
begin
  FGradSwatch.Visible  := AVisible;
  FBtnEditGrad.Visible := AVisible;
end;

procedure TVertexStylePanel.UpdateControls;
var
  isGradient: Boolean;
begin
  if FStyle = nil then
  begin
    FLblType.Text := 'No shape selected';
    FTypeCombo.Enabled := False;
    ShowSolidControls(False);
    ShowGradientControls(False);
    FHexEdit.Text := '';
    Exit;
  end;

  FTypeCombo.Enabled := True;
  isGradient := FStyle.StyleType = hstGradient;

  FUpdating := True;
  try
    if isGradient then
      FTypeCombo.FocusItem := 1
    else
      FTypeCombo.FocusItem := 0;
  finally
    FUpdating := False;
  end;

  if isGradient then
  begin
    FLblType.Text := 'Gradient';
    ShowSolidControls(False);
    ShowGradientControls(True);
    FGradSwatch.Style := FStyle;
    FGradSwatch.Repaint;
  end
  else
  begin
    case FStyle.StyleType of
      hstSolidColor, hstSolidColorNoAlpha,
      hstSolidGray,  hstSolidGrayNoAlpha:
        FLblType.Text := 'Solid colour';
    else
      FLblType.Text := 'Unknown style';
    end;
    ShowSolidControls(True);
    ShowGradientControls(False);
    FSwatch.Enabled   := True;
    FHexEdit.Enabled  := True;
    FLblA.Enabled     := True;
    FAlphaBar.Enabled := True;
    FAlphaSpin.Enabled:= True;
    LoadColorToControls(FStyle.Color);
  end;
end;


{ ── Commit helper ────────────────────────────────────────────────────────── }

procedure TVertexStylePanel.CommitColor(const ANew: THvifColor);
var
  cmd: TVertexCmdSetStyleColour;
begin
  if (FDocument = nil) or (FStyle = nil) then
    Exit;
  if (ANew.R = FOrigColor.R) and (ANew.G = FOrigColor.G) and
     (ANew.B = FOrigColor.B) and (ANew.A = FOrigColor.A) then
    Exit;  { no change — skip }
  cmd := TVertexCmdSetStyleColour.Create(FStyle, FOrigColor, ANew);
  FDocument.UndoStack.Execute(cmd);
  FOrigColor := ANew;   { advance snapshot for the next gesture }
end;


{ ── Control event handlers ───────────────────────────────────────────────── }

procedure TVertexStylePanel.TypeComboChanged(Sender: TObject);
var
  wantGradient: Boolean;
  wantType:     THvifStyleType;
  cmd:          TVertexCmdSetStyleType;
begin
  if FUpdating or (FDocument = nil) or (FStyle = nil) then
    Exit;
  wantGradient := FTypeCombo.FocusItem = 1;
  if wantGradient then
    wantType := hstGradient
  else
    wantType := hstSolidColor;
  if FStyle.StyleType = wantType then
    Exit;
  cmd := TVertexCmdSetStyleType.Create(FStyle, wantType);
  FDocument.UndoStack.Execute(cmd);
  UpdateControls;
end;

procedure TVertexStylePanel.SwatchClick(Sender: TObject);
var
  preset, picked: TfpgColor;
  c: THvifColor;
begin
  c := FSwatch.Color;
  { Pass fully-opaque RGB — the colour dialog does not handle alpha }
  preset := ($FF shl 24) or (LongWord(c.R) shl 16) or
            (LongWord(c.G) shl 8) or LongWord(c.B);
  picked := fpgSelectColorDialog(preset);
  if picked = preset then
    Exit;  { cancelled or no change }
  FpgColorToHvifRGB(picked, c.R, c.G, c.B);
  { Update swatch + hex edit, keep current alpha }
  FUpdating := True;
  try
    FSwatch.Color := c;
    FHexEdit.Text := Format('#%2.2X%2.2X%2.2X', [c.R, c.G, c.B]);
  finally
    FUpdating := False;
  end;
  CommitColor(CurrentColor);
end;

procedure TVertexStylePanel.HexEditExit(Sender: TObject);
var
  s:       string;
  r, g, b: Integer;
  c:       THvifColor;
begin
  s := Trim(FHexEdit.Text);
  if (Length(s) > 0) and (s[1] = '#') then
    Delete(s, 1, 1);
  c := FSwatch.Color;
  if Length(s) = 6 then
  begin
    try
      r  := StrToInt('$' + Copy(s, 1, 2));
      g  := StrToInt('$' + Copy(s, 3, 2));
      b  := StrToInt('$' + Copy(s, 5, 2));
      c.R := Byte(r);
      c.G := Byte(g);
      c.B := Byte(b);
      FSwatch.Color := c;
      FHexEdit.Text := Format('#%2.2X%2.2X%2.2X', [c.R, c.G, c.B]);
    except
      FHexEdit.Text := Format('#%2.2X%2.2X%2.2X', [c.R, c.G, c.B]);
    end;
  end
  else
    FHexEdit.Text := Format('#%2.2X%2.2X%2.2X', [c.R, c.G, c.B]);
  CommitColor(CurrentColor);
end;

procedure TVertexStylePanel.AlphaBarChanged(Sender: TObject; APosition: integer);
var
  c: THvifColor;
begin
  if FUpdating then Exit;
  { Live preview: directly mutate the style and signal a repaint.
    No undo entry is created here — that happens in AlphaBarMouseUp. }
  FDragAlpha := True;
  FUpdating := True;
  try
    FAlphaSpin.Value := APosition;
  finally
    FUpdating := False;
  end;
  if (FDocument <> nil) and (FStyle <> nil) then
  begin
    c   := FStyle.Color;   { read-modify-write: can't assign through a property }
    c.A := Byte(APosition);
    FStyle.Color := c;
    FDocument.NotifyChanged;
  end;
end;

procedure TVertexStylePanel.AlphaBarMouseUp(Sender: TObject; AButton: TMouseButton;
    AShift: TShiftState; const AMousePos: TPoint);
begin
  if FDragAlpha then
  begin
    FDragAlpha := False;
    CommitColor(CurrentColor);
  end;
end;

procedure TVertexStylePanel.AlphaSpinChanged(Sender: TObject);
begin
  if FUpdating then Exit;
  FUpdating := True;
  try
    FAlphaBar.Position := FAlphaSpin.Value;
  finally
    FUpdating := False;
  end;
end;

procedure TVertexStylePanel.AlphaSpinExit(Sender: TObject);
begin
  CommitColor(CurrentColor);
end;

procedure TVertexStylePanel.GradSwatchClick(Sender: TObject);
begin
  BtnEditGradClick(Sender);
end;

procedure TVertexStylePanel.BtnEditGradClick(Sender: TObject);
begin
  if (FDocument = nil) or (FStyle = nil) then
    Exit;
  if FGradEditor = nil then
    FGradEditor := TVertexGradientEditor.Create(Self);
  FGradEditor.SetDocumentAndStyle(FDocument, FStyle);
  FGradEditor.Show;
end;


{ ── Colour control helpers ───────────────────────────────────────────────── }

procedure TVertexStylePanel.LoadColorToControls(const AColor: THvifColor);
begin
  FUpdating := True;
  try
    FSwatch.Color      := AColor;
    FHexEdit.Text      := Format('#%2.2X%2.2X%2.2X', [AColor.R, AColor.G, AColor.B]);
    FAlphaBar.Position := AColor.A;
    FAlphaSpin.Value   := AColor.A;
  finally
    FUpdating := False;
  end;
end;

function TVertexStylePanel.CurrentColor: THvifColor;
begin
  Result   := FSwatch.Color;
  Result.A := Byte(FAlphaSpin.Value);
end;


{ ── Public interface ─────────────────────────────────────────────────────── }

procedure TVertexStylePanel.SetDocument(ADoc: TVertexDocument);
begin
  FDocument := ADoc;
  FStyle    := nil;
  UpdateControls;
end;

procedure TVertexStylePanel.SetStyle(AStyle: TVertexStyle);
begin
  FStyle := AStyle;
  if AStyle <> nil then
    FOrigColor := AStyle.Color;
  UpdateControls;
end;

procedure TVertexStylePanel.DocumentChanged;
begin
  if FStyle <> nil then
  begin
    UpdateControls;
    if (FGradEditor <> nil) and FGradEditor.Visible then
      FGradEditor.DocumentChanged;
  end;
end;

end.
