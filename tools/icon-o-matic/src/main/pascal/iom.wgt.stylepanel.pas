unit iom.wgt.stylepanel;

{
  TIomStylePanel — shows and edits the style of the currently selected shape.

  Step #7: solid-colour style editing with undo support (no Apply button).

  Controls:
    FSwatch   — clickable colour rectangle; opens fpgSelectColorDialog on click.
    FHexEdit  — "#RRGGBB" text field; parsed and committed on focus-loss.
    FAlphaBar — horizontal trackbar; direct mutation for live preview, committed
                on mouse-up (same model as canvas node drag).
    FAlphaSpin — numeric spin edit for alpha; committed on focus-loss.

  Commit model (identical to canvas drag):
    Every completed gesture creates one TIomCmdSetStyleColour entry on the undo
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
  fpg_dialogs,
  fpg_hvif_model,
  fpg_iom_document;


type
  { ── TIomColorSwatch ─────────────────────────────────────────────────────── }
  { Clickable colour preview rectangle.  Alpha is shown separately by the
    trackbar; the swatch always fills with the solid RGB. }
  TIomColorSwatch = class(TfpgWidget)
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


  { ── TIomStylePanel ──────────────────────────────────────────────────────── }
  TIomStylePanel = class(TfpgBevel)
  private
    FDocument:    TIomDocument;   { not owned }
    FStyle:       TIomStyle;      { current style, not owned; nil = no selection }
    FOrigColor:   THvifColor;     { last-committed colour — undo baseline }
    FDragAlpha:   Boolean;        { True while the user is dragging FAlphaBar }
    FUpdating:    Boolean;        { guards recursive OnChange loops }

    FLblHeader:   TfpgLabel;
    FLblType:     TfpgLabel;
    FSwatch:      TIomColorSwatch;
    FHexEdit:     TfpgEdit;
    FLblA:        TfpgLabel;
    FAlphaBar:    TfpgTrackBar;
    FAlphaSpin:   TfpgSpinEdit;

    procedure SetupControls;
    procedure UpdateControls;
    procedure SetControlsEnabled(AEnabled: Boolean);

    { Commit helpers }
    procedure CommitColor(const ANew: THvifColor);

    { Control event handlers }
    procedure SwatchClick(Sender: TObject);
    procedure HexEditExit(Sender: TObject);
    procedure AlphaBarChanged(Sender: TObject; APosition: integer);
    procedure AlphaBarMouseUp(Sender: TObject; AButton: TMouseButton;
                              AShift: TShiftState; const AMousePos: TPoint);
    procedure AlphaSpinChanged(Sender: TObject);
    procedure AlphaSpinExit(Sender: TObject);

    { Read the live RGBA values from the editing controls }
    function  CurrentColor: THvifColor;
    { Push a colour into all three editing controls atomically }
    procedure LoadColorToControls(const AColor: THvifColor);

  public
    constructor Create(AOwner: TComponent); override;

    { Connect to the document. Must be called before SetStyle. }
    procedure SetDocument(ADoc: TIomDocument);

    { Show the given style. Pass nil to clear / show "no selection". }
    procedure SetStyle(AStyle: TIomStyle);

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

  ROW0      = 4;
  ROW1      = 26;
  ROW2      = 48;                       { colour row }
  ROW3      = ROW2 + SW_H + 6;          { alpha row }
  PANEL_H   = ROW3 + SPH  + 6;          { total panel height }


{ ── Helpers ──────────────────────────────────────────────────────────────── }

procedure FpgColorToHvifRGB(ACol: TfpgColor; out R, G, B: Byte);
begin
  R := Byte((ACol shr 16) and $FF);
  G := Byte((ACol shr  8) and $FF);
  B := Byte( ACol         and $FF);
end;


{ ── TIomColorSwatch ─────────────────────────────────────────────────────── }

constructor TIomColorSwatch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor      := Default(THvifColor);
  FColor.A    := 255;
  MouseCursor := mcHand;
end;

procedure TIomColorSwatch.SetColor(const AValue: THvifColor);
begin
  FColor := AValue;
  Repaint;
end;

procedure TIomColorSwatch.HandlePaint;
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

procedure TIomColorSwatch.HandleLMouseDown(x, y: integer;
    shiftstate: TShiftState);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;


{ ── TIomStylePanel ──────────────────────────────────────────────────────── }

constructor TIomStylePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument  := nil;
  FStyle     := nil;
  FDragAlpha := False;
  FUpdating  := False;
  PreferredSize := fpgSize(220, PANEL_H);
  SetupControls;
  UpdateControls;
end;

procedure TIomStylePanel.SetupControls;
begin
  FLblHeader := TfpgLabel.Create(Self);
  FLblHeader.SetPosition(LBL_X, ROW0, 210, 18);
  FLblHeader.Text     := 'Style Properties';
  FLblHeader.FontDesc := '#Label1:bold';

  FLblType := TfpgLabel.Create(Self);
  FLblType.SetPosition(LBL_X, ROW1, 210, 18);
  FLblType.Text := 'No shape selected';

  { Colour row: swatch + hex edit }
  FSwatch := TIomColorSwatch.Create(Self);
  FSwatch.SetPosition(LBL_X, ROW2, SW_W, SW_H);
  FSwatch.OnClick := @SwatchClick;

  FHexEdit := TfpgEdit.Create(Self);
  FHexEdit.SetPosition(HEX_X, ROW2 + 2, HEX_W, SPH);
  FHexEdit.Text   := '';
  FHexEdit.OnExit := @HexEditExit;

  { Alpha row: "A:" label + trackbar + spin }
  FLblA := TfpgLabel.Create(Self);
  FLblA.SetPosition(LBL_X, ROW3 + 4, ALP_LBL_W, 18);
  FLblA.Text := 'A:';

  FAlphaBar := TfpgTrackBar.Create(Self);
  FAlphaBar.SetPosition(ALP_X, ROW3, ALP_W, SPH);
  FAlphaBar.Min      := 0;
  FAlphaBar.Max      := 255;
  FAlphaBar.Position := 255;
  FAlphaBar.OnChange  := @AlphaBarChanged;
  FAlphaBar.OnMouseUp := @AlphaBarMouseUp;

  FAlphaSpin := TfpgSpinEdit.Create(Self);
  FAlphaSpin.SetPosition(ASPIN_X, ROW3, ASPIN_W, SPH);
  FAlphaSpin.MinValue := 0;
  FAlphaSpin.MaxValue := 255;
  FAlphaSpin.Value    := 255;
  FAlphaSpin.OnChange := @AlphaSpinChanged;
  FAlphaSpin.OnExit   := @AlphaSpinExit;
end;

procedure TIomStylePanel.SetControlsEnabled(AEnabled: Boolean);
begin
  FSwatch.Enabled   := AEnabled;
  FHexEdit.Enabled  := AEnabled;
  FLblA.Enabled     := AEnabled;
  FAlphaBar.Enabled := AEnabled;
  FAlphaSpin.Enabled:= AEnabled;
end;

procedure TIomStylePanel.LoadColorToControls(const AColor: THvifColor);
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

function TIomStylePanel.CurrentColor: THvifColor;
begin
  Result   := FSwatch.Color;
  Result.A := Byte(FAlphaSpin.Value);
end;

procedure TIomStylePanel.UpdateControls;
var
  editable: Boolean;
begin
  if FStyle = nil then
  begin
    FLblType.Text := 'No shape selected';
    SetControlsEnabled(False);
    FHexEdit.Text := '';
    Exit;
  end;

  editable := False;
  case FStyle.StyleType of
    hstSolidColor, hstSolidColorNoAlpha,
    hstSolidGray,  hstSolidGrayNoAlpha:
    begin
      FLblType.Text := 'Solid colour';
      editable      := True;
    end;
    hstGradient:
      FLblType.Text := 'Gradient (view only)';
  else
    FLblType.Text := 'Unknown style';
  end;

  SetControlsEnabled(editable);

  if editable then
    LoadColorToControls(FStyle.Color)
  else
    FHexEdit.Text := '';
end;


{ ── Commit helper ────────────────────────────────────────────────────────── }

procedure TIomStylePanel.CommitColor(const ANew: THvifColor);
var
  cmd: TIomCmdSetStyleColour;
begin
  if (FDocument = nil) or (FStyle = nil) then
    Exit;
  if (ANew.R = FOrigColor.R) and (ANew.G = FOrigColor.G) and
     (ANew.B = FOrigColor.B) and (ANew.A = FOrigColor.A) then
    Exit;  { no change — skip }
  cmd := TIomCmdSetStyleColour.Create(FStyle, FOrigColor, ANew);
  FDocument.UndoStack.Execute(cmd);
  FOrigColor := ANew;   { advance snapshot for the next gesture }
end;


{ ── Control event handlers ───────────────────────────────────────────────── }

procedure TIomStylePanel.SwatchClick(Sender: TObject);
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

procedure TIomStylePanel.HexEditExit(Sender: TObject);
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

procedure TIomStylePanel.AlphaBarChanged(Sender: TObject; APosition: integer);
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

procedure TIomStylePanel.AlphaBarMouseUp(Sender: TObject; AButton: TMouseButton;
    AShift: TShiftState; const AMousePos: TPoint);
begin
  if FDragAlpha then
  begin
    FDragAlpha := False;
    CommitColor(CurrentColor);
  end;
end;

procedure TIomStylePanel.AlphaSpinChanged(Sender: TObject);
begin
  if FUpdating then Exit;
  FUpdating := True;
  try
    FAlphaBar.Position := FAlphaSpin.Value;
  finally
    FUpdating := False;
  end;
end;

procedure TIomStylePanel.AlphaSpinExit(Sender: TObject);
begin
  CommitColor(CurrentColor);
end;


{ ── Public interface ─────────────────────────────────────────────────────── }

procedure TIomStylePanel.SetDocument(ADoc: TIomDocument);
begin
  FDocument := ADoc;
  FStyle    := nil;
  UpdateControls;
end;

procedure TIomStylePanel.SetStyle(AStyle: TIomStyle);
begin
  FStyle := AStyle;
  if AStyle <> nil then
    FOrigColor := AStyle.Color;
  UpdateControls;
end;

procedure TIomStylePanel.DocumentChanged;
begin
  if FStyle <> nil then
    UpdateControls;
end;

end.
