{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    Description:
      Hybrid canvas that uses AggPas for all 2D rendering (anti-aliased
      lines, alpha blending, gradients) and renders text directly into
      the AggPas buffer via cached FreeType bitmap glyphs (TGlyphCache).
      Buffer management (allocation, screen flushing) is handled via
      IBufferManager.

      Text is rendered into the same pixel buffer as 2D content, giving
      correct z-ordering automatically. No deferred text queues, no
      two-surface compositing, no flickering.

      No platform-specific code, no include files.
}

unit fpg_hybrid_canvas;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  agg_2D;

type

  { THybridCanvas — composes a clean Agg2D object for 2D rendering and
    IBufferManager for platform-specific pixel buffer operations.

    Inherits from TfpgCanvasBase to satisfy the fpGUI canvas contract.
    All 2D operations are forwarded to the internal Agg2D object.
    Text is rendered via TfpgFontResourceBase.DrawTextToBuffer, which is
    implemented by the platform's registered AggFontResourceClass (e.g.
    TfpgFreeTypeFontResource on Unix/macOS, TfpgGDIAggFontResource on
    Windows). The canvas itself has no font-engine dependency.

    No platform-specific code, no include files. }

  THybridCanvas = class(TfpgCanvasBase)
  private
    FAgg: agg_2D.Agg2D;
    FBufferManager: IBufferManager;
    FCurrentTextColor: TfpgColor;
    FWindowAttached: Boolean;
    FAttachedWindow: TfpgWindowBase;
    FBufData: Pointer;
    FBufStride: Integer;
    FBufWidth: Integer;
    FBufHeight: Integer;
    procedure EnsureWindowAttached;
  protected
    { Text rendering — into buffer via glyph cache }
    procedure DoDrawString(x, y: TfpgCoord; const txt: string); override;
    procedure DoSetFontRes(fntres: TfpgFontResourceBase); override;
    procedure DoSetTextColor(cl: TfpgColor); override;
    { 2D rendering — delegated to FAgg }
    procedure DoSetColor(cl: TfpgColor); override;
    procedure DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle); override;
    procedure DoFillRectangle(x, y, w, h: TfpgCoord); override;
    procedure DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord); override;
    procedure DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord); override;
    procedure DoDrawRectangle(x, y, w, h: TfpgCoord); override;
    procedure DoDrawLine(x1, y1, x2, y2: TfpgCoord); override;
    procedure DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer); override;
    procedure DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: double); override;
    procedure DoFillArc(x, y, w, h: TfpgCoord; a1, a2: double); override;
    procedure DoDrawPolygon(const Points: array of TPoint); override;
    function  GetPixel(X, Y: integer): TfpgColor; override;
    procedure SetPixel(X, Y: integer; const AValue: TfpgColor); override;
    { Clip rect — applied to FAgg }
    procedure DoSetClipRect(const ARect: TfpgRect); override;
    function  DoGetClipRect: TfpgRect; override;
    procedure DoAddClipRect(const ARect: TfpgRect); override;
    procedure DoClearClipRect; override;
    { Lifecycle — coordinate FAgg and IBufferManager }
    procedure DoBeginDraw(awidget: TfpgWidgetBase; CanvasTarget: TfpgCanvasBase); override;
    procedure DoPutBufferToScreen(x, y, w, h: TfpgCoord); override;
    procedure DoEndDraw; override;
    function  GetBufferAllocated: Boolean; override;
    procedure DoAllocateBuffer; override;
    procedure DoRestoreFromBuffer(const ARect: TfpgRect); override;
  public
    constructor Create(awidget: TfpgWidgetBase); override;
    destructor  Destroy; override;
    procedure   GradientFill(ARect: TfpgRect; AStart, AStop: TfpgColor; ADirection: TGradientDirection); override;
  end;


{ Factory function type for creating platform-specific buffer manager }
type
  TBufferManagerFactory = function: IBufferManager;

var
  { Set by platform-specific initialisation code (e.g. fpg_interface.pas) }
  CreateBufferManager: TBufferManagerFactory = nil;


implementation

uses
  agg_color,
  agg_basics,
  fpg_main;


{ Helper: convert TfpgColor to AggPas Color (rgba8).
  AggPas uses pixfmt_bgra32 which places B at byte 0, G at 1, R at 2, A at 3.
  This matches the native display format on X11, GDI, and Cocoa. }
function fpgColorToAgg(c: TfpgColor): agg_2D.Color;
var
  rgb: TfpgColor;
begin
  { Resolve named/system colours (e.g. clWindowBackground) to raw RGB first }
  rgb := fpgColorToRGB(c);
  { TfpgColor is $AARRGGBB. AggPas Color.r/g/b map to byte offsets via
    bgra_order, so we use natural R/G/B extraction. }
  Result.r := (rgb shr 16) and $FF;
  Result.g := (rgb shr 8) and $FF;
  Result.b := rgb and $FF;
  Result.a := (rgb shr 24) and $FF;
  if Result.a = 0 then
    Result.a := 255;  // 0 alpha means fully opaque in fpGUI
end;


{ THybridCanvas }

constructor THybridCanvas.Create(awidget: TfpgWidgetBase);
begin
  inherited Create(awidget);
  FAgg.Construct;
  FCurrentTextColor := 0;
  FWindowAttached := False;
  FAttachedWindow := nil;
  FBufData := nil;
  FBufStride := 0;
  FBufWidth := 0;
  FBufHeight := 0;
  if Assigned(CreateBufferManager) then
    FBufferManager := CreateBufferManager();
end;

destructor THybridCanvas.Destroy;
begin
  if Assigned(FBufferManager) then
  begin
    FBufferManager.FreeBuffer;
    FBufferManager := nil;
  end;
  FAgg.Destruct;
  inherited Destroy;
end;

procedure THybridCanvas.EnsureWindowAttached;
begin
  if not Assigned(FWidget) then
    Exit;
  if not Assigned(FWidget.Window) then
    Exit;
  { Always re-attach the buffer manager to the current window.
    Popup windows are destroyed and recreated between show/hide
    cycles, so the X11 handle changes even though the canvas
    persists. Object-pointer comparison is unreliable because FPC's
    memory allocator may reuse the same address for the new window,
    making a stale FAttachedWindow appear current while the buffer
    manager still holds a zeroed-out (invalid) X11 handle.
    AttachWindow is cheap (copies a few fields), so always calling
    it is safer than trying to detect staleness. }
  FAttachedWindow := FWidget.Window;
  if Assigned(FBufferManager) then
    FBufferManager.AttachWindow(FAttachedWindow);
  FWindowAttached := True;
end;


{ --- Text rendering (into buffer via glyph cache) --- }

procedure THybridCanvas.DoDrawString(x, y: TfpgCoord; const txt: string);
var
  cb: agg_2D.RectD;
begin
  if (Length(txt) < 1) or (FBufData = nil) or not Assigned(FFont) then
    Exit;
  { AY passed to DrawTextToBuffer is the baseline (top + ascent).
    The clip box from Agg2D is forwarded so text clips to the same region
    as 2D operations — required for selection-highlight redraws in TfpgEdit. }
  cb := FAgg.clipBox;
  FFont.DrawTextToBuffer(PByte(FBufData), FBufStride, FBufWidth, FBufHeight,
    x + FDeltaX, y + FDeltaY + FFont.GetAscent, txt, FCurrentTextColor,
    Trunc(cb.x1), Trunc(cb.y1), Trunc(cb.x2), Trunc(cb.y2));
end;

procedure THybridCanvas.DoSetFontRes(fntres: TfpgFontResourceBase);
begin
  { Font resource is stored in FFont by the base class SetFont call.
    Each AggCanvas font resource (TfpgFreeTypeFontResource,
    TfpgGDIAggFontResource) is self-contained — no extra canvas-side
    glyph-cache state to synchronise here. }
end;

procedure THybridCanvas.DoSetTextColor(cl: TfpgColor);
begin
  FCurrentTextColor := cl;
end;


{ --- 2D rendering (delegated to FAgg) --- }

procedure THybridCanvas.DoSetColor(cl: TfpgColor);
var
  c: agg_2D.Color;
begin
  c := fpgColorToAgg(cl);
  FAgg.lineColor(c);
  FAgg.fillColor(c);
end;

procedure THybridCanvas.DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle);
const
  StyleMap: array[TfpgLineStyle] of agg_2D.LineStyle = (
    stySolid,       // lsSolid
    styDash,        // lsDash
    styDot,         // lsDot
    styDashDot,     // lsDashDot
    styDashDotDot   // lsDashDotDot
  );
var
  w: integer;
begin
  { X11 treats lineWidth=0 as "1px hardware-accelerated line".
    AggPas treats 0 as "don't stroke at all", so clamp to 1. }
  w := awidth;
  if w < 1 then
    w := 1;
  FAgg.SetLineStyle(w, StyleMap[astyle]);
end;

procedure THybridCanvas.DoFillRectangle(x, y, w, h: TfpgCoord);
var
  fc: agg_2D.Color;
  rx, ry, rw, rh: TfpgCoord;
  cx1, cy1, cx2, cy2: TfpgCoord;
  cb: agg_2D.RectD;
  bgra: LongWord;
  py: TfpgCoord;
  rowPtr: PLongWord;
begin
  if (w < 1) or (h < 1) then
    Exit;

  fc := FAgg.fillColor;

  { Fast path: for fully opaque fills we write directly into the pixel buffer,
    bypassing AggPas's per-pixel alpha-blend rasteriser. Profiling showed that
    BGRA32_BLEND_SOLID_HSPAN consumed 84% of CPU time — most of which was
    opaque solid rectangle fills that need no blending at all. }
  if (fc.a = 255) and (FBufData <> nil) then
  begin
    { Build BGRA32 LongWord: on little-endian [B, G, R, A] in memory }
    bgra := LongWord(fc.b)
          or (LongWord(fc.g) shl 8)
          or (LongWord(fc.r) shl 16)
          or LongWord($FF000000);

    { Get clip rect from AggPas }
    cb := FAgg.clipBox;
    cx1 := Round(cb.x1);
    cy1 := Round(cb.y1);
    cx2 := Round(cb.x2);
    cy2 := Round(cb.y2);

    { Apply window delta and compute fill rect }
    rx := x + FDeltaX;
    ry := y + FDeltaY;
    rw := w;
    rh := h;

    { Clip to AggPas clip rect }
    if rx < cx1 then begin Dec(rw, cx1 - rx); rx := cx1; end;
    if ry < cy1 then begin Dec(rh, cy1 - ry); ry := cy1; end;
    if rx + rw > cx2 then rw := cx2 - rx;
    if ry + rh > cy2 then rh := cy2 - ry;

    { Clip to buffer bounds }
    if rx < 0 then begin Dec(rw, -rx); rx := 0; end;
    if ry < 0 then begin Dec(rh, -ry); ry := 0; end;
    if rx + rw > FBufWidth then rw := FBufWidth - rx;
    if ry + rh > FBufHeight then rh := FBufHeight - ry;

    if (rw < 1) or (rh < 1) then
      Exit;

    { Direct memory fill — no alpha blending needed }
    for py := ry to ry + rh - 1 do
    begin
      rowPtr := PLongWord(PByte(FBufData) + py * FBufStride + rx * 4);
      FillDWord(rowPtr^, rw, bgra);
    end;
  end
  else
  begin
    { Transparent or no buffer — fall through to AggPas for proper blending }
    FAgg.noLine;
    FAgg.rectangle(x + FDeltaX, y + FDeltaY,
                   x + FDeltaX + w - 1, y + FDeltaY + h - 1, True);
  end;
end;

procedure THybridCanvas.DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord);
var
  px, py: TfpgCoord;
  rx, ry, rw, rh: TfpgCoord;
  xorMask: LongWord;
  rgb: TfpgColor;
  rowPtr: PLongWord;
begin
  if (w < 1) or (h < 1) then
    Exit;
  if FBufData = nil then
    Exit;

  { Perform true bitwise XOR on pixel data, matching X11's GXxor behaviour.
    This ensures the caret is always visible regardless of background colour. }
  rgb := fpgColorToRGB(col);
  { Build BGRA XOR mask: TfpgColor is $AARRGGBB, buffer is BGRA byte order.
    On little-endian, a LongWord in memory is [B, G, R, A].
    Force alpha to $FF so XOR flips the colour channels but keeps pixels opaque. }
  xorMask := (rgb and $FF) shl 16          { R → byte 2 }
           or (rgb and $FF00)               { G → byte 1 }
           or ((rgb shr 16) and $FF)        { B → byte 0 }
           or $FF000000;                    { A = $FF }

  rx := x + FDeltaX;
  ry := y + FDeltaY;
  rw := w;
  rh := h;
  { Clip to buffer bounds }
  if rx < 0 then begin Inc(rw, rx); rx := 0; end;
  if ry < 0 then begin Inc(rh, ry); ry := 0; end;
  if rx + rw > FBufWidth then rw := FBufWidth - rx;
  if ry + rh > FBufHeight then rh := FBufHeight - ry;
  if (rw < 1) or (rh < 1) then
    Exit;

  for py := ry to ry + rh - 1 do
  begin
    rowPtr := PLongWord(PByte(FBufData) + py * FBufStride + rx * 4);
    for px := 0 to rw - 1 do
    begin
      rowPtr^ := rowPtr^ xor xorMask;
      Inc(rowPtr);
    end;
  end;
end;

procedure THybridCanvas.DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord);
var
  c: agg_2D.Color;
begin
  { Ensure both fill and line use the current color, matching
    TAgg2D.DoFillTriangle behaviour. Add 0.5 for pixel alignment. }
  c := FAgg.lineColor;
  FAgg.fillColor(c);
  FAgg.lineWidth(1);
  FAgg.triangle(x1 + FDeltaX + 0.5, y1 + FDeltaY + 0.5,
                x2 + FDeltaX + 0.5, y2 + FDeltaY + 0.5,
                x3 + FDeltaX + 0.5, y3 + FDeltaY + 0.5);
end;

procedure THybridCanvas.DoDrawRectangle(x, y, w, h: TfpgCoord);
begin
  FAgg.noFill;
  FAgg.rectangle(x + FDeltaX + 0.5, y + FDeltaY + 0.5,
                 x + FDeltaX + w - 1.5, y + FDeltaY + h - 1.5, False);
end;

procedure THybridCanvas.DoDrawLine(x1, y1, x2, y2: TfpgCoord);
begin
  FAgg.line(x1 + FDeltaX, y1 + FDeltaY, x2 + FDeltaX, y2 + FDeltaY, True);
end;

procedure THybridCanvas.DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer);
var
  aggImg: agg_2D.Image;
  stride: Integer;
  buffer: Pointer;
  tempBuf: PLongWord;
  srcBuf: PLongWord;
  maskPtr: PByte;
  imgW, imgH: Integer;
  row, col: Integer;
  mskLineLen: Integer;
  byteIdx, bitIdx: Integer;
  needFree: Boolean;
begin
  if not (img is TfpgImage) then
    Exit;
  if TfpgImage(img).ColorDepth <> 32 then
    Exit;

  imgW := TfpgImage(img).Width;
  imgH := TfpgImage(img).Height;
  needFree := False;

  if img.Masked and (img.MaskData <> nil) then
  begin
    { Image has a 1-bit mask. Create a temporary copy with alpha applied.
      Mask format: 1 bit per pixel, MSB first, rows padded to 32-bit.
      Bit = 1 means opaque, bit = 0 means transparent.
      BMP palette images may have alpha=0 for all pixels (the native X11
      canvas ignores alpha), so we must set alpha=0xFF for opaque pixels. }
    tempBuf := GetMem(imgW * imgH * 4);
    srcBuf := PLongWord(img.ImageData);
    Move(srcBuf^, tempBuf^, imgW * imgH * 4);

    mskLineLen := ((imgW + 31) div 32) * 4;  { bytes per mask row }
    maskPtr := PByte(img.MaskData);

    for row := 0 to imgH - 1 do
    begin
      for col := 0 to imgW - 1 do
      begin
        byteIdx := col div 8;
        bitIdx := 7 - (col mod 8);  { MSB first }
        if ((maskPtr + row * mskLineLen + byteIdx)^ shr bitIdx) and 1 = 0 then
          { Mask bit = 0: transparent — clear alpha }
          tempBuf[row * imgW + col] := tempBuf[row * imgW + col] and $00FFFFFF
        else
          { Mask bit = 1: opaque — ensure alpha is 0xFF }
          tempBuf[row * imgW + col] := tempBuf[row * imgW + col] or $FF000000;
      end;
    end;

    buffer := tempBuf;
    stride := imgW * 4;
    needFree := True;
  end
  else
  begin
    stride := Integer(PByte(TfpgImage(img).ScanLine[1]) - PByte(TfpgImage(img).ScanLine[0]));
    if stride < 0 then
      buffer := TfpgImage(img).ScanLine[imgH - 1]
    else
      buffer := TfpgImage(img).ScanLine[0];
  end;

  aggImg.Construct(int8u_ptr(buffer), imgW, imgH, stride);
  FAgg.transformImage(
    @aggImg,
    xi, yi, xi + w, yi + h,
    x + FDeltaX, y + FDeltaY, x + FDeltaX + w, y + FDeltaY + h);
  aggImg.Destruct;

  if needFree then
    FreeMem(tempBuf);
end;

procedure THybridCanvas.DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: double);
var
  cx, cy, rx, ry: double;
begin
  cx := x + FDeltaX + w / 2.0;
  cy := y + FDeltaY + h / 2.0;
  rx := w / 2.0;
  ry := h / 2.0;
  FAgg.noFill;
  FAgg.arc(cx, cy, rx, ry, a1, a1 + a2);
end;

procedure THybridCanvas.DoFillArc(x, y, w, h: TfpgCoord; a1, a2: double);
var
  cx, cy, rx, ry: double;
begin
  cx := x + FDeltaX + w / 2.0;
  cy := y + FDeltaY + h / 2.0;
  rx := w / 2.0;
  ry := h / 2.0;
  FAgg.noLine;
  FAgg.arc(cx, cy, rx, ry, a1, a1 + a2);
end;

procedure THybridCanvas.DoDrawPolygon(const Points: array of TPoint);
var
  i, j: Integer;
  poly: array of double;
  c: agg_2D.Color;
begin
  poly := nil;
  if Length(Points) < 2 then
    Exit;
  SetLength(poly, (Length(Points) * 2) + 1);
  j := 1;
  for i := Low(Points) to High(Points) do
  begin
    poly[j * 2 - 1] := Points[i].X + FDeltaX + 0.5;
    poly[j * 2] := Points[i].Y + FDeltaY + 0.5;
    Inc(j);
  end;
  { X11 uses XFillPolygon, GDI uses Windows.Polygon — both fill.
    Ensure fill and line color match so polygon renders filled. }
  c := FAgg.lineColor;
  FAgg.fillColor(c);
  FAgg.lineWidth(1);
  FAgg.polygon(@poly[1], Length(Points));
end;

function THybridCanvas.GetPixel(X, Y: integer): TfpgColor;
var
  px: PByte;
  drawX, drawY: Integer;
begin
  Result := 0;
  drawX := X + FDeltaX;
  drawY := Y + FDeltaY;
  if (FBufData = nil) or (drawX < 0) or (drawY < 0) or
     (drawX >= FBufWidth) or (drawY >= FBufHeight) then
    Exit;
  px := PByte(FBufData) + drawY * FBufStride + drawX * 4;
  { BGRA to TfpgColor ($AARRGGBB) }
  Result := (TfpgColor(px[2]) shl 16) or (TfpgColor(px[1]) shl 8) or TfpgColor(px[0]);
end;

procedure THybridCanvas.SetPixel(X, Y: integer; const AValue: TfpgColor);
var
  px: PByte;
  rgb: TfpgColor;
  drawX, drawY: Integer;
begin
  drawX := X + FDeltaX;
  drawY := Y + FDeltaY;
  if (FBufData = nil) or (drawX < 0) or (drawY < 0) or
     (drawX >= FBufWidth) or (drawY >= FBufHeight) then
    Exit;
  rgb := fpgColorToRGB(AValue);
  px := PByte(FBufData) + drawY * FBufStride + drawX * 4;
  { TfpgColor ($AARRGGBB) to BGRA }
  px[0] := rgb and $FF;           { B }
  px[1] := (rgb shr 8) and $FF;   { G }
  px[2] := (rgb shr 16) and $FF;  { R }
  px[3] := 255;                   { A = fully opaque }
end;

procedure THybridCanvas.GradientFill(ARect: TfpgRect; AStart, AStop: TfpgColor; ADirection: TGradientDirection);
var
  c1, c2: agg_2D.Color;
begin
  c1 := fpgColorToAgg(AStart);
  c2 := fpgColorToAgg(AStop);
  if ADirection = gdVertical then
    FAgg.fillLinearGradient(
      ARect.Left + FDeltaX, ARect.Top + FDeltaY,
      ARect.Left + FDeltaX, ARect.Bottom + FDeltaY, c1, c2)
  else
    FAgg.fillLinearGradient(
      ARect.Left + FDeltaX, ARect.Top + FDeltaY,
      ARect.Right + FDeltaX, ARect.Top + FDeltaY, c1, c2);
  FAgg.noLine;
  FAgg.rectangle(
    ARect.Left + FDeltaX, ARect.Top + FDeltaY,
    ARect.Right + FDeltaX, ARect.Bottom + FDeltaY, True);
end;


{ --- Clip rect --- }

procedure THybridCanvas.DoSetClipRect(const ARect: TfpgRect);
begin
  FAgg.clipBox(ARect.Left + FDeltaX, ARect.Top + FDeltaY,
               ARect.Left + FDeltaX + ARect.Width,
               ARect.Top + FDeltaY + ARect.Height);
end;

function THybridCanvas.DoGetClipRect: TfpgRect;
var
  cb: agg_2D.RectD;
begin
  cb := FAgg.clipBox;
  Result.SetRect(Round(cb.x1), Round(cb.y1),
                 Round(cb.x2 - cb.x1), Round(cb.y2 - cb.y1));
end;

procedure THybridCanvas.DoAddClipRect(const ARect: TfpgRect);
var
  NewRect: TfpgRect;
begin
  DoGetClipRect.IntersectRect(NewRect, ARect);
  DoSetClipRect(NewRect);
end;

procedure THybridCanvas.DoClearClipRect;
begin
  if Assigned(FWidget) then
    FAgg.clipBox(0, 0, FBufWidth, FBufHeight);
end;


{ --- Lifecycle --- }

procedure THybridCanvas.DoBeginDraw(awidget: TfpgWidgetBase; CanvasTarget: TfpgCanvasBase);
begin
  if CanvasTarget = Self then
  begin
    { Top-level canvas: we draw to our own buffer.
      Buffer is already attached via DoAllocateBuffer. }
    EnsureWindowAttached;
  end
  else if CanvasTarget is THybridCanvas then
  begin
    { Alien widget: attach our Agg2D to a sub-region of the parent's buffer.
      We calculate a byte offset so that AggPas position (0,0) maps to the
      widget's top-left pixel. This way we do NOT need FDeltaX/FDeltaY offsets
      in draw calls — AggPas draws in widget-local coordinates.
      This mirrors the original TAgg2D.AttachPartialImage approach. }
    if THybridCanvas(CanvasTarget).FBufData <> nil then
    begin
      FBufStride := THybridCanvas(CanvasTarget).FBufStride;
      FBufWidth := awidget.ActualWidth;
      FBufHeight := awidget.ActualHeight;
      { Calculate byte offset into parent's buffer so position (0,0) maps
        to the widget's top-left pixel. Both FAgg and FBufData must use
        this same offset pointer so the glyph cache renders into the
        correct sub-region. }
      FBufData := PByte(THybridCanvas(CanvasTarget).FBufData)
                  + FDeltaX * 4 + FDeltaY * FBufStride;
      FAgg.attach(int8u_ptr(FBufData), FBufWidth, FBufHeight, FBufStride);
      { Zero out deltas: partial-attach already positioned the buffer
        at the widget's origin, so draw calls use widget-local coords. }
      FDeltaX := 0;
      FDeltaY := 0;
    end;
  end;
end;

procedure THybridCanvas.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
begin
  if Assigned(FBufferManager) then
    FBufferManager.PutBufferToScreen(x, y, w, h);
end;

procedure THybridCanvas.DoEndDraw;
begin
  { Called during FreeResources (widget destruction).
    Detach from window and release resources. }
  FWindowAttached := False;
  FAttachedWindow := nil;
  if Assigned(FBufferManager) then
    FBufferManager.DetachWindow;
  FCanvasTarget := nil;
end;

function THybridCanvas.GetBufferAllocated: Boolean;
begin
  if (FCanvasTarget <> nil) and (FCanvasTarget <> Self) then
  begin
    { Alien widget: check the parent's buffer }
    Result := THybridCanvas(FCanvasTarget).GetBufferAllocated;
  end
  else
  begin
    Result := Assigned(FBufData);
    if Result and Assigned(FWidget) then
    begin
      { If the widget's window was destroyed (popup close/hide), the
        buffer cannot be blitted to screen.  Return False so that the
        Expose handler falls through to InvalidateRect and triggers a
        full repaint when the window is recreated on re-show. }
      if not Assigned(FWidget.Window) then
      begin
        Result := False;
        Exit;
      end;
      { Check if the window was resized }
      if (FBufWidth < FWidget.ActualWidth) or (FBufHeight < FWidget.ActualHeight) then
      begin
        if Assigned(FBufferManager) then
          FBufferManager.FreeBuffer;
        FBufData := nil;
        FBufWidth := 0;
        FBufHeight := 0;
        Result := False;
      end;
    end;
  end;
end;

procedure THybridCanvas.DoAllocateBuffer;
const
  ResizeThreshold = 50;
var
  AllocW, AllocH: Integer;
begin
  if not Assigned(FBufferManager) then
    Exit;
  if not Assigned(FWidget) then
    Exit;

  { Buffer allocation needs the display connection, so ensure
    the window is attached first (BeginDraw calls us before DoBeginDraw). }
  EnsureWindowAttached;

  AllocW := FWidget.ActualWidth + ResizeThreshold;
  AllocH := FWidget.ActualHeight + ResizeThreshold;

  FBufferManager.AllocateBuffer(AllocW, AllocH, FBufData, FBufStride);
  FBufWidth := AllocW;
  FBufHeight := AllocH;

  { Attach the Agg2D rendering engine to our buffer }
  FAgg.attach(int8u_ptr(FBufData), FBufWidth, FBufHeight, FBufStride);
end;

procedure THybridCanvas.DoRestoreFromBuffer(const ARect: TfpgRect);
begin
  { RestoreFromBuffer can be called from the Expose event handler, outside
    of a BeginDraw/EndDraw pair.  The buffer manager's cached window handle
    may be stale (popup windows are destroyed and recreated between show/hide
    cycles).  Re-attach to the current window so we blit to the right target,
    matching how the X11 canvas reads FWidget.Window.WinHandle directly. }
  EnsureWindowAttached;
  if Assigned(FBufferManager) then
    FBufferManager.RestoreFromBuffer(ARect);
end;


{ AggFontResourceClass is registered by each platform's fpg_interface.pas
  initialisation block, not here, so this unit remains free of any
  font-engine dependency. }

end.
