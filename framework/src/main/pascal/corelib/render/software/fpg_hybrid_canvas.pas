{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    Description:
      Hybrid canvas that uses AggPas for all 2D rendering (anti-aliased
      lines, alpha blending, gradients) and delegates text rendering to
      the native platform backend (Xft on X11, GDI on Windows) via
      ITextRenderer. Buffer management (allocation, screen flushing)
      is handled via IBufferManager.

      Text draw calls are deferred into a queue and replayed after the
      AggPas buffer is flushed to screen, ensuring correct Z-order
      (2D elements underneath, text on top).
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

  { Deferred text draw item — captures all state needed to replay a
    DrawString call after the AggPas buffer has been flushed. }
  TDeferredTextItem = record
    X, Y: TfpgCoord;
    Text: string;
    Color: TfpgColor;
    Font: TfpgFontResourceBase;
    DeltaX, DeltaY: TfpgCoord;
    ClipRect: TfpgRect;
    HasClipRect: Boolean;
  end;


  { THybridCanvas — composes a clean Agg2D object for 2D rendering
    with ITextRenderer for native text and IBufferManager for
    platform-specific pixel buffer operations.

    Inherits from TfpgCanvasBase to satisfy the fpGUI canvas contract.
    All 2D operations are forwarded to the internal Agg2D object.
    Text operations are deferred and replayed after buffer flush.

    No platform-specific code, no include files. }

  THybridCanvas = class(TfpgCanvasBase)
  private
    FAgg: agg_2D.Agg2D;
    FTextRenderer: ITextRenderer;
    FBufferManager: IBufferManager;
    FTextQueue: array of TDeferredTextItem;
    FTextQueueCount: Integer;
    FLastTextQueue: array of TDeferredTextItem;  // Snapshot for expose replay
    FLastTextQueueCount: Integer;
    FCurrentTextColor: TfpgColor;
    FWindowAttached: Boolean;
    FAttachedWindow: TfpgWindowBase;
    FBufData: Pointer;
    FBufStride: Integer;
    FBufWidth: Integer;
    FBufHeight: Integer;
    FWinDeltaX: TfpgCoord;  // Real window offsets for text rendering (preserved when FDeltaX is zeroed)
    FWinDeltaY: TfpgCoord;
    FParentCanvas: THybridCanvas;  // Parent canvas for alien widgets (text queue target)
    procedure EnsureWindowAttached;
    procedure FlushTextQueue;
    procedure ReplayTextQueue;
    procedure RenderTextItems(AQueue: array of TDeferredTextItem; ACount: Integer);
    procedure EnqueueText(AX, AY: TfpgCoord; const AText: string);
  protected
    { Text rendering — deferred to ITextRenderer }
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
    { Clip rect — applied to FAgg, captured per deferred text item }
    procedure DoSetClipRect(const ARect: TfpgRect); override;
    function  DoGetClipRect: TfpgRect; override;
    procedure DoAddClipRect(const ARect: TfpgRect); override;
    procedure DoClearClipRect; override;
    { Lifecycle — coordinate FAgg, IBufferManager, and ITextRenderer }
    procedure DoBeginDraw(awidget: TfpgWidgetBase; CanvasTarget: TfpgCanvasBase); override;
    procedure DoAfterPaint; override;
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


{ Factory function types for creating platform-specific implementations }
type
  TTextRendererFactory = function: ITextRenderer;
  TBufferManagerFactory = function: IBufferManager;

var
  { Set by platform-specific initialisation code (e.g. fpg_main.pas) }
  CreateTextRenderer: TTextRendererFactory = nil;
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
  FTextQueueCount := 0;
  FLastTextQueueCount := 0;
  FCurrentTextColor := 0;
  FWindowAttached := False;
  FAttachedWindow := nil;
  FBufData := nil;
  FBufStride := 0;
  FBufWidth := 0;
  FBufHeight := 0;
  FWinDeltaX := 0;
  FWinDeltaY := 0;
  FParentCanvas := nil;
  if Assigned(CreateTextRenderer) then
    FTextRenderer := CreateTextRenderer();
  if Assigned(CreateBufferManager) then
    FBufferManager := CreateBufferManager();
end;

destructor THybridCanvas.Destroy;
begin
  FTextRenderer := nil;
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
  if FWindowAttached and (FWidget.Window = FAttachedWindow) then
    Exit;
  { Attach (or re-attach if window changed) }
  FAttachedWindow := FWidget.Window;
  if Assigned(FTextRenderer) then
    FTextRenderer.AttachWindow(FAttachedWindow);
  if Assigned(FBufferManager) then
    FBufferManager.AttachWindow(FAttachedWindow);
  FWindowAttached := True;
end;

procedure THybridCanvas.EnqueueText(AX, AY: TfpgCoord; const AText: string);
var
  item: ^TDeferredTextItem;
  cr: TfpgRect;
begin
  if FTextQueueCount >= Length(FTextQueue) then
    SetLength(FTextQueue, FTextQueueCount + 64);
  item := @FTextQueue[FTextQueueCount];
  item^.X := AX;
  item^.Y := AY;
  item^.Text := AText;
  item^.Color := FCurrentTextColor;
  item^.Font := FFont;
  { Use real window offsets for text rendering (FDeltaX may be zeroed for alien widgets) }
  item^.DeltaX := FWinDeltaX;
  item^.DeltaY := FWinDeltaY;
  { Translate clip rect to window coordinates for text rendering }
  item^.HasClipRect := True;
  cr := DoGetClipRect;
  cr.Left := cr.Left + FWinDeltaX;
  cr.Top := cr.Top + FWinDeltaY;
  item^.ClipRect := cr;
  Inc(FTextQueueCount);
end;

procedure THybridCanvas.RenderTextItems(AQueue: array of TDeferredTextItem; ACount: Integer);
var
  i: Integer;
  lastColor: TfpgColor;
  lastFont: TfpgFontResourceBase;
  lastClip: TfpgRect;
  lastHasClip: Boolean;
begin
  if (ACount = 0) or not Assigned(FTextRenderer) then
    Exit;

  EnsureWindowAttached;
  if not FWindowAttached then
    Exit;

  lastColor := High(TfpgColor);  // sentinel
  lastFont := nil;
  lastHasClip := False;
  lastClip.SetRect(0, 0, 0, 0);

  for i := 0 to ACount - 1 do
  begin
    with AQueue[i] do
    begin
      { Only update renderer state when it changes }
      if Font <> lastFont then
      begin
        FTextRenderer.SetFont(Font);
        lastFont := Font;
      end;
      if Color <> lastColor then
      begin
        FTextRenderer.SetTextColor(Color);
        lastColor := Color;
      end;
      { Update clip rect if changed }
      if HasClipRect then
      begin
        if (not lastHasClip) or
           (ClipRect.Left <> lastClip.Left) or (ClipRect.Top <> lastClip.Top) or
           (ClipRect.Width <> lastClip.Width) or (ClipRect.Height <> lastClip.Height) then
        begin
          FTextRenderer.SetClipRect(ClipRect);
          lastClip := ClipRect;
          lastHasClip := True;
        end;
      end
      else if lastHasClip then
      begin
        FTextRenderer.ClearClipRect;
        lastHasClip := False;
      end;

      FTextRenderer.DrawText(X + DeltaX, Y + DeltaY, Text);
    end;
  end;

  { Clear clip state after flushing }
  if lastHasClip then
    FTextRenderer.ClearClipRect;
end;

procedure THybridCanvas.FlushTextQueue;
var
  i, base: Integer;
  target: THybridCanvas;
begin
  if FTextQueueCount = 0 then
    Exit;

  RenderTextItems(FTextQueue, FTextQueueCount);

  { Append to the top-level parent's snapshot for final blit replay.
    The parent's DoPutBufferToScreen blits the full buffer (overwriting
    all per-widget text), then replays this complete snapshot so all
    text reappears in the correct z-order. }
  if Assigned(FParentCanvas) then
    target := FParentCanvas
  else
    target := Self;

  base := target.FLastTextQueueCount;
  if Length(target.FLastTextQueue) < base + FTextQueueCount then
    SetLength(target.FLastTextQueue, base + FTextQueueCount + 64);
  for i := 0 to FTextQueueCount - 1 do
    target.FLastTextQueue[base + i] := FTextQueue[i];
  target.FLastTextQueueCount := base + FTextQueueCount;

  FTextQueueCount := 0;
end;

procedure THybridCanvas.ReplayTextQueue;
begin
  if FLastTextQueueCount > 0 then
    RenderTextItems(FLastTextQueue, FLastTextQueueCount);
end;


{ --- Text rendering (deferred to ITextRenderer) --- }

procedure THybridCanvas.DoDrawString(x, y: TfpgCoord; const txt: string);
begin
  if Length(txt) < 1 then
    Exit;
  EnqueueText(x, y, txt);
end;

procedure THybridCanvas.DoSetFontRes(fntres: TfpgFontResourceBase);
begin
  { Font is stored in TfpgCanvasBase.FFont by SetFont() before this
    is called. We don't need to configure AggPas font engine since
    text rendering is handled by ITextRenderer. }
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
begin
  FAgg.lineWidth(awidth);
  { TODO: map TfpgLineStyle to AggPas dash patterns if needed }
end;

procedure THybridCanvas.DoFillRectangle(x, y, w, h: TfpgCoord);
begin
  if (w < 1) or (h < 1) then
    Exit;
  { fillColor was already set by DoSetColor — just draw.
    Do NOT re-read FColor here: Clear() calls DoSetColor() directly
    without updating FColor, so FColor may be stale. }
  FAgg.noLine;
  FAgg.rectangle(x + FDeltaX, y + FDeltaY,
                 x + FDeltaX + w - 1, y + FDeltaY + h - 1, True);
end;

procedure THybridCanvas.DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord);
var
  c: agg_2D.Color;
begin
  { AggPas doesn't natively support XOR drawing.
    Fall back to a normal fill with the specified colour. }
  if (w < 1) or (h < 1) then
    Exit;
  c := fpgColorToAgg(col);
  FAgg.noLine;
  FAgg.fillColor(c);
  FAgg.rectangle(x + FDeltaX, y + FDeltaY,
                 x + FDeltaX + w - 1, y + FDeltaY + h - 1, True);
end;

procedure THybridCanvas.DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord);
begin
  FAgg.noLine;
  FAgg.triangle(x1 + FDeltaX, y1 + FDeltaY,
                x2 + FDeltaX, y2 + FDeltaY,
                x3 + FDeltaX, y3 + FDeltaY);
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
begin
  { TODO: implement image rendering via AggPas TransformImage }
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
begin
  { TODO: implement polygon rendering }
end;

function THybridCanvas.GetPixel(X, Y: integer): TfpgColor;
begin
  { TODO: read from buffer }
  Result := 0;
end;

procedure THybridCanvas.SetPixel(X, Y: integer; const AValue: TfpgColor);
begin
  { TODO: write to buffer }
end;

procedure THybridCanvas.GradientFill(ARect: TfpgRect; AStart, AStop: TfpgColor; ADirection: TGradientDirection);
begin
  { TODO: implement via AggPas gradient }
  inherited GradientFill(ARect, AStart, AStop, ADirection);
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
    FParentCanvas := nil;
    FWinDeltaX := FDeltaX;
    FWinDeltaY := FDeltaY;
    FLastTextQueueCount := 0;  // Reset snapshot for this paint cycle
  end
  else if CanvasTarget is THybridCanvas then
  begin
    { Alien widget: attach our Agg2D to a sub-region of the parent's buffer.
      We calculate a byte offset so that AggPas position (0,0) maps to the
      widget's top-left pixel. This way we do NOT need FDeltaX/FDeltaY offsets
      in draw calls — AggPas draws in widget-local coordinates.
      This mirrors the original TAgg2D.AttachPartialImage approach. }
    FParentCanvas := THybridCanvas(CanvasTarget);
    if THybridCanvas(CanvasTarget).FBufData <> nil then
    begin
      FBufData := THybridCanvas(CanvasTarget).FBufData;
      FBufStride := THybridCanvas(CanvasTarget).FBufStride;
      FBufWidth := awidget.ActualWidth;
      FBufHeight := awidget.ActualHeight;
      FAgg.attach(
        int8u_ptr(PByte(FBufData) + FDeltaX * 4 + FDeltaY * FBufStride),
        FBufWidth,
        FBufHeight,
        FBufStride);
      { Save real window offsets for text rendering (text goes direct to window) }
      FWinDeltaX := FDeltaX;
      FWinDeltaY := FDeltaY;
      { Zero out deltas: partial-attach already positioned the buffer
        at the widget's origin, so draw calls use widget-local coords. }
      FDeltaX := 0;
      FDeltaY := 0;
    end;
  end;
end;

procedure THybridCanvas.DoAfterPaint;
begin
  { Called after each widget's HandlePaint, before its children paint.
    Blit this widget's buffer region to the window and render its text
    immediately, so child widgets will correctly overlap parent content. }
  if Assigned(FParentCanvas) then
  begin
    { Alien widget: blit our region from the parent's buffer }
    if Assigned(FParentCanvas.FBufferManager) then
      FParentCanvas.FBufferManager.PutBufferToScreen(
        FWinDeltaX, FWinDeltaY, FWidget.ActualWidth, FWidget.ActualHeight);
  end
  else
  begin
    { Top-level widget: blit our full area }
    if Assigned(FBufferManager) then
      FBufferManager.PutBufferToScreen(0, 0, FWidget.ActualWidth, FWidget.ActualHeight);
  end;
  FlushTextQueue;
end;

procedure THybridCanvas.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
begin
  { Each widget already blitted its region and drew its text in DoAfterPaint
    with correct z-ordering (parent text under children). A full blit here
    would overwrite that text, so we skip it. The buffer and text snapshot
    remain valid for expose-event replay via DoRestoreFromBuffer. }
end;

procedure THybridCanvas.DoEndDraw;
begin
  { Called during FreeResources (widget destruction).
    Detach from window and release resources. }
  FTextQueueCount := 0;
  FWindowAttached := False;
  FAttachedWindow := nil;
  if Assigned(FTextRenderer) then
    FTextRenderer.DetachWindow;
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
var
  fullRect: TfpgRect;
begin
  if not Assigned(FBufferManager) then
    Exit;
  if FLastTextQueueCount > 0 then
  begin
    { Text was rendered directly to the window (not into the buffer).
      To avoid drawing text on top of stale text (which causes darkening
      due to sub-pixel anti-aliasing accumulation), blit the full window
      area to reset all pixels, then replay the text queue. }
    fullRect.SetRect(0, 0, FWidget.ActualWidth, FWidget.ActualHeight);
    FBufferManager.RestoreFromBuffer(fullRect);
    ReplayTextQueue;
  end
  else
    { No text to replay — just blit the exposed region }
    FBufferManager.RestoreFromBuffer(ARect);
end;


end.
