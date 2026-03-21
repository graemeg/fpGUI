{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    Description:
      macOS/Cocoa platform implementation of IBufferManager for the hybrid
      canvas. Manages a BGRA pixel buffer and blits it to an NSView via
      Core Graphics CGImage.
}

unit fpg_cocoa_buffer_manager;

{$mode objfpc}{$H+}

interface

uses
  CocoaAll,
  CGImage,
  CGColorSpace,
  CGDataProvider,
  CGContext,
  fpg_impl,
  fpg_base,
  fpg_cocoa;

type

  { TCocoaBufferManager - Manages a BGRA pixel buffer and blits it
    to an NSView via Core Graphics CGImage drawing }

  TCocoaBufferManager = class(TInterfacedObject, IBufferManager)
  private
    FView: TfpgCocoaView;
    FBuffer: Pointer;
    FBufWidth: Integer;
    FBufHeight: Integer;
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


function CreateCocoaBufferManager: IBufferManager;


implementation

uses
  fpg_main;


{ TCocoaBufferManager }

constructor TCocoaBufferManager.Create;
begin
  inherited Create;
  FView := nil;
  FBuffer := nil;
  FBufWidth := 0;
  FBufHeight := 0;
end;

destructor TCocoaBufferManager.Destroy;
begin
  FreeBuffer;
  inherited Destroy;
end;

procedure TCocoaBufferManager.AttachWindow(AWindow: TfpgWindowBase);
var
  win: TfpgCocoaWindow;
begin
  win := TfpgCocoaWindow(AWindow);
  FView := win.View;
end;

procedure TCocoaBufferManager.DetachWindow;
begin
  FView := nil;
end;

procedure TCocoaBufferManager.AllocateBuffer(AWidth, AHeight: Integer;
  out AData: Pointer; out AStride: Integer);
begin
  { Free existing buffer if dimensions changed }
  if Assigned(FBuffer) and ((FBufWidth <> AWidth) or (FBufHeight <> AHeight)) then
    FreeBuffer;

  if not Assigned(FBuffer) then
  begin
    FBufWidth := AWidth;
    FBufHeight := AHeight;
    AStride := AWidth * 4;  { 32-bit BGRA, 4 bytes per pixel }
    FBuffer := GetMem(AStride * AHeight);
    FillChar(FBuffer^, AStride * AHeight, 0);
    AData := FBuffer;
  end
  else
  begin
    { Buffer already allocated with matching dimensions }
    AData := FBuffer;
    AStride := FBufWidth * 4;
  end;
end;

function TCocoaBufferManager.BufferAllocated: Boolean;
begin
  Result := Assigned(FBuffer);
end;

procedure TCocoaBufferManager.FreeBuffer;
begin
  if Assigned(FBuffer) then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;
  FBufWidth := 0;
  FBufHeight := 0;
end;

procedure TCocoaBufferManager.PutBufferToScreen(x, y, w, h: TfpgCoord);
var
  colorSpace: CGColorSpaceRef;
  provider: CGDataProviderRef;
  image: CGImageRef;
  stride: Integer;
  ctx: CGContextRef;
  destRect: CGRect;
  nsCtx: NSGraphicsContext;
begin
  if not Assigned(FView) or not BufferAllocated then
    Exit;
  if (w < 1) or (h < 1) then
    Exit;

  stride := FBufWidth * 4;

  { Create a CGImage from our BGRA pixel buffer.
    kCGBitmapByteOrder32Little + kCGImageAlphaNoneSkipFirst = BGRA byte order
    which matches our AggPas buffer layout exactly. }
  colorSpace := CGColorSpaceCreateDeviceRGB;
  provider := CGDataProviderCreateWithData(nil, FBuffer, stride * FBufHeight, nil);
  image := CGImageCreate(
    FBufWidth, FBufHeight,
    8,               { bits per component }
    32,              { bits per pixel }
    stride,          { bytes per row }
    colorSpace,
    kCGBitmapByteOrder32Little or kCGImageAlphaNoneSkipFirst,
    provider,
    nil,             { no decode array }
    False,           { no interpolation }
    kCGRenderingIntentDefault
  );

  if image <> nil then
  begin
    { Use lockFocus to draw outside of drawRect.
      This is the direct equivalent of X11's XPutImage or GDI's BitBlt -
      immediate buffer-to-screen transfer without going through the
      Cocoa display cycle. }
    FView.lockFocus;
    try
      nsCtx := NSGraphicsContext.currentContext;
      if nsCtx <> nil then
      begin
        ctx := nsCtx.CGContext;
        if ctx <> nil then
        begin
          { The view uses isFlipped=True (top-left origin) so coordinate
            system already matches our buffer layout. Draw only the dirty
            region by clipping and drawing the full image - CG will only
            rasterise the visible portion. }
          CGContextSaveGState(ctx);
          destRect := CGRectMake(0, 0, FBufWidth, FBufHeight);
          CGContextClipToRect(ctx, CGRectMake(x, y, w, h));
          CGContextDrawImage(ctx, destRect, image);
          CGContextRestoreGState(ctx);
        end;
      end;
    finally
      FView.unlockFocus;
    end;
  end;

  { Release Core Graphics objects }
  CGImageRelease(image);
  CGDataProviderRelease(provider);
  CGColorSpaceRelease(colorSpace);
end;

procedure TCocoaBufferManager.RestoreFromBuffer(const ARect: TfpgRect);
begin
  if (ARect.Width < 1) or (ARect.Height < 1) then
    Exit;
  PutBufferToScreen(ARect.Left, ARect.Top, ARect.Width, ARect.Height);
end;


function CreateCocoaBufferManager: IBufferManager;
begin
  Result := TCocoaBufferManager.Create;
end;


end.
