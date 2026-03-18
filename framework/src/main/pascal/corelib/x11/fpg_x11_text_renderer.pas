{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    Description:
      X11 platform implementations for the hybrid canvas architecture:
      - TX11TextRenderer: Xft-based text rendering
      - TX11BufferManager: XImage-based pixel buffer management
}

unit fpg_x11_text_renderer;

{$mode objfpc}{$H+}

interface

uses
  X,
  Xlib,
  XUtil,
  fpg_impl,
  fpg_base,
  fpg_xft_x11,
  fpg_x11;

type

  { TX11TextRenderer - Renders text via Xft onto an X11 window drawable }

  TX11TextRenderer = class(TInterfacedObject, ITextRenderer)
  private
    FDisplay: PXDisplay;
    FXftDraw: PXftDraw;
    FWinHandle: TfpgWinHandle;
    FXftColor: TXftColor;
    FCurFont: PXftFont;
    procedure ConvertColor(AColor: TfpgColor);
  public
    constructor Create;
    destructor Destroy; override;
    { ITextRenderer }
    procedure AttachWindow(AWindow: TfpgWindowBase);
    procedure DetachWindow;
    procedure SetFont(AFont: TfpgFontResourceBase);
    procedure SetTextColor(AColor: TfpgColor);
    procedure DrawText(AX, AY: TfpgCoord; const AText: string);
    procedure SetClipRect(const ARect: TfpgRect);
    procedure ClearClipRect;
  end;


  { TX11BufferManager - Manages an XImage pixel buffer and blits it
    to an X11 window via XPutImage }

  TX11BufferManager = class(TInterfacedObject, IBufferManager)
  private
    FDisplay: PXDisplay;
    FWinHandle: TfpgWinHandle;
    FXImage: TXImage;
    FXImageInitialised: Boolean;
    FBuffer: Pointer;
    FBufWidth: Integer;
    FBufHeight: Integer;
    FDisplayDepth: Integer;
    FDefaultScreen: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    { IBufferManager }
    procedure AttachWindow(AWindow: TfpgWindowBase);
    procedure DetachWindow;
    procedure AllocateBuffer(AWidth, AHeight: Integer;
      out AData: Pointer; out AStride: Integer);
    function  BufferAllocated: Boolean;
    procedure FreeBuffer;
    procedure PutBufferToScreen(x, y, w, h: TfpgCoord);
    procedure RestoreFromBuffer(const ARect: TfpgRect);
  end;


function CreateX11TextRenderer: ITextRenderer;
function CreateX11BufferManager: IBufferManager;


implementation

var
  { X error suppression for stale window handles.
    Popup menus can be destroyed between paint cycles, leaving stale
    handles in the text renderer and buffer manager. Rather than let
    the default X error handler call exit(), we temporarily suppress
    errors around X calls on potentially-stale drawables. }
  XErrorOccurred: Boolean = False;
  PreviousErrorHandler: TXErrorHandler = nil;

function SuppressedXErrorHandler(Display: PDisplay; Error: PXErrorEvent): longint; cdecl;
begin
  XErrorOccurred := True;
  Result := 0;  { Do not call exit() }
end;

procedure BeginSuppressXErrors;
begin
  XErrorOccurred := False;
  PreviousErrorHandler := XSetErrorHandler(@SuppressedXErrorHandler);
end;

procedure EndSuppressXErrors;
begin
  XSetErrorHandler(PreviousErrorHandler);
  PreviousErrorHandler := nil;
end;


{ TX11TextRenderer }

procedure TX11TextRenderer.ConvertColor(AColor: TfpgColor);
var
  c: TfpgColor;
begin
  { Convert fpGUI colour to Xft colour format.
    fpGUI stores colours as $AARRGGBB after fpgColorToRGB.
    We do the RGB extraction inline to avoid depending on fpg_main. }
  c := AColor;
  FXftColor.color.blue  := (c and $000000FF) shl 8;
  FXftColor.color.green := (c and $0000FF00);
  FXftColor.color.red   := (c and $00FF0000) shr 8;
  FXftColor.color.alpha := (c and $FF000000) shr 16;
  if FXftColor.color.alpha = 0 then
    FXftColor.color.alpha := FXftColor.color.alpha xor $FFFF;  // 0 means fully opaque
  FXftColor.pixel := 0;
end;

constructor TX11TextRenderer.Create;
begin
  inherited Create;
  FDisplay := nil;
  FXftDraw := nil;
  FWinHandle := 0;
  FCurFont := nil;
end;

destructor TX11TextRenderer.Destroy;
begin
  DetachWindow;
  inherited Destroy;
end;

procedure TX11TextRenderer.AttachWindow(AWindow: TfpgWindowBase);
var
  win: TfpgX11Window;
begin
  win := TfpgX11Window(AWindow);
  if win.WinHandle = FWinHandle then
    Exit;  // already attached to this window

  DetachWindow;

  FWinHandle := win.WinHandle;
  if FWinHandle <= 0 then
    Exit;

  { Get the display via public accessor functions }
  FDisplay := fpgX11Display;

  FXftDraw := XftDrawCreate(
    FDisplay,
    FWinHandle,
    XDefaultVisual(FDisplay, fpgX11Screen),
    XDefaultColormap(FDisplay, fpgX11Screen));
end;

procedure TX11TextRenderer.DetachWindow;
begin
  if Assigned(FXftDraw) then
  begin
    { The underlying window may have been destroyed already (e.g. popup menus).
      XftDrawDestroy internally calls XRenderFreePicture which will generate
      a RenderBadPicture error on a stale drawable. Suppress it. }
    BeginSuppressXErrors;
    try
      XftDrawDestroy(FXftDraw);
      if FDisplay <> nil then
        XSync(FDisplay, 0);
    finally
      EndSuppressXErrors;
    end;
    FXftDraw := nil;
  end;
  FWinHandle := 0;
end;

procedure TX11TextRenderer.SetFont(AFont: TfpgFontResourceBase);
begin
  if AFont is TfpgX11FontResource then
    FCurFont := TfpgX11FontResource(AFont).Handle
  else
    FCurFont := nil;
end;

procedure TX11TextRenderer.SetTextColor(AColor: TfpgColor);
begin
  ConvertColor(AColor);
end;

procedure TX11TextRenderer.DrawText(AX, AY: TfpgCoord; const AText: string);
begin
  if (Length(AText) < 1) or (FCurFont = nil) or (FXftDraw = nil) then
    Exit;
  XftDrawStringUTF8(FXftDraw, FXftColor, FCurFont,
    AX, AY + FCurFont^.ascent,
    PChar(AText), Length(AText));
end;

procedure TX11TextRenderer.SetClipRect(const ARect: TfpgRect);
var
  rgn: TRegion;
  xr: TXRectangle;
begin
  if FXftDraw = nil then
    Exit;
  xr.x := ARect.Left;
  xr.y := ARect.Top;
  xr.width := ARect.Width;
  xr.height := ARect.Height;
  rgn := XCreateRegion;
  XUnionRectWithRegion(@xr, rgn, rgn);
  XftDrawSetClip(FXftDraw, rgn);
  XDestroyRegion(rgn);
end;

procedure TX11TextRenderer.ClearClipRect;
begin
  if FXftDraw = nil then
    Exit;
  XftDrawSetClip(FXftDraw, nil);
end;


function CreateX11TextRenderer: ITextRenderer;
begin
  Result := TX11TextRenderer.Create;
end;


{ TX11BufferManager }

constructor TX11BufferManager.Create;
begin
  inherited Create;
  FDisplay := nil;
  FWinHandle := 0;
  FBuffer := nil;
  FBufWidth := 0;
  FBufHeight := 0;
  FXImageInitialised := False;
  FDisplayDepth := 24;  // safe default, updated on AttachWindow
end;

destructor TX11BufferManager.Destroy;
begin
  FreeBuffer;
  inherited Destroy;
end;

procedure TX11BufferManager.AttachWindow(AWindow: TfpgWindowBase);
var
  win: TfpgX11Window;
begin
  win := TfpgX11Window(AWindow);
  FWinHandle := win.WinHandle;
  FDisplay := fpgX11Display;
  FDisplayDepth := fpgX11DisplayDepth;
  FDefaultScreen := fpgX11Screen;
end;

procedure TX11BufferManager.DetachWindow;
begin
  FWinHandle := 0;
end;

procedure TX11BufferManager.AllocateBuffer(AWidth, AHeight: Integer;
  out AData: Pointer; out AStride: Integer);
var
  vis: PVisual;
begin
  { Free existing buffer if dimensions changed }
  if Assigned(FBuffer) and ((FBufWidth <> AWidth) or (FBufHeight <> AHeight)) then
    FreeBuffer;

  if not Assigned(FBuffer) then
  begin
    FBufWidth := AWidth;
    FBufHeight := AHeight;
    AStride := AWidth * 4;  // 32-bit RGBA, 4 bytes per pixel
    FBuffer := GetMem(AStride * AHeight);
    FillChar(FBuffer^, AStride * AHeight, 0);

    { Get colour masks from the display's default visual }
    vis := XDefaultVisual(FDisplay, FDefaultScreen);

    { Initialise the XImage structure pointing to our buffer }
    FillChar(FXImage, SizeOf(FXImage), 0);
    with FXImage do
    begin
      Width          := AWidth;
      Height         := AHeight;
      xoffset        := 0;
      format         := ZPixmap;
      Data           := FBuffer;
      byte_order     := LSBFirst;
      bitmap_unit    := 32;
      bitmap_bit_order := LSBFirst;
      bitmap_pad     := 32;
      depth          := FDisplayDepth;
      bytes_per_line := AStride;
      bits_per_pixel := 32;
      red_mask       := vis^.red_mask;
      green_mask     := vis^.green_mask;
      blue_mask      := vis^.blue_mask;
      obdata         := #0;
    end;
    XInitImage(@FXImage);
    FXImageInitialised := True;

    AData := FBuffer;
  end
  else
  begin
    { Buffer already allocated with matching dimensions }
    AData := FBuffer;
    AStride := FBufWidth * 4;
  end;
end;

function TX11BufferManager.BufferAllocated: Boolean;
begin
  Result := Assigned(FBuffer) and FXImageInitialised;
end;

procedure TX11BufferManager.FreeBuffer;
begin
  FXImageInitialised := False;
  if Assigned(FBuffer) then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;
  FBufWidth := 0;
  FBufHeight := 0;
end;

procedure TX11BufferManager.PutBufferToScreen(x, y, w, h: TfpgCoord);
var
  gc: TGC;
  GcValues: TXGcValues;
begin
  if (FWinHandle <= 0) or (FDisplay = nil) or not BufferAllocated then
    Exit;
  if (w < 1) or (h < 1) then
    Exit;

  { The window may have been destroyed (e.g. popup menus closing).
    Suppress X errors around the draw calls to prevent the default
    error handler from calling exit(). }
  BeginSuppressXErrors;
  try
    gc := XCreateGc(FDisplay, FWinHandle, 0, @GcValues);
    if not XErrorOccurred then
    begin
      XPutImage(FDisplay, FWinHandle, gc, @FXImage, x, y, x, y, w, h);
      XFreeGc(FDisplay, gc);
    end;
    XSync(FDisplay, 0);
  finally
    EndSuppressXErrors;
  end;

  if XErrorOccurred then
    FWinHandle := 0;  { Mark as invalid so we don't try again }
end;

procedure TX11BufferManager.RestoreFromBuffer(const ARect: TfpgRect);
begin
  if (ARect.Width < 1) or (ARect.Height < 1) then
    Exit;
  PutBufferToScreen(ARect.Left, ARect.Top, ARect.Width, ARect.Height);
end;


function CreateX11BufferManager: IBufferManager;
begin
  Result := TX11BufferManager.Create;
end;


end.
