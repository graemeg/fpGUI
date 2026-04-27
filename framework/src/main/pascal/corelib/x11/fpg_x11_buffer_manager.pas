{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    Description:
      X11 platform implementation of IBufferManager for the hybrid canvas.
      Manages an XImage pixel buffer and blits it to an X11 window via
      XPutImage.
}

unit fpg_x11_buffer_manager;

{$mode objfpc}{$H+}

interface

uses
  X,
  Xlib,
  XUtil,
  fpg_impl,
  fpg_base,
  fpg_x11;

type

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


function CreateX11BufferManager: IBufferManager;


implementation

uses
  fpg_main;  // for fpgColorToRGB

var
  { X error suppression for stale window handles.
    Popup menus can be destroyed between paint cycles, leaving stale
    handles in the buffer manager. Rather than let the default X error
    handler call exit(), we temporarily suppress errors around X calls
    on potentially-stale drawables. }
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
    { Flush any stale errors from previously destroyed windows before
      we start our own operations, so they don't falsely trigger our
      error check below. }
    XSync(FDisplay, 0);
    XErrorOccurred := False;

    gc := XCreateGc(FDisplay, FWinHandle, 0, @GcValues);
    if not XErrorOccurred then
    begin
      XPutImage(FDisplay, FWinHandle, gc, @FXImage, x, y, x, y, w, h);
      XFreeGc(FDisplay, gc);
      XSync(FDisplay, 0);
    end;
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
