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
{$modeswitch objectivec1}

interface

uses
  CocoaAll,
  fpg_impl,
  fpg_base,
  fpg_cocoa;

type

  { TCocoaBufferManager - Manages a BGRA pixel buffer and blits it
    to an NSView via Core Graphics CGImage drawing }

  TCocoaBufferManager = class(TInterfacedObject, IBufferManager)
  private
    FView: TfpgCocoaView;
    FWindow: TfpgCocoaWindow;
    FBuffer: Pointer;
    FBufWidth: Integer;
    FBufHeight: Integer;
    FBufScale: Integer;
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
  FBufScale := 1;
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
  FWindow := win;
  FView := win.View;
end;

procedure TCocoaBufferManager.DetachWindow;
begin
  FView := nil;
  FWindow := nil;
end;

procedure TCocoaBufferManager.AllocateBuffer(AWidth, AHeight: Integer;
  out AData: Pointer; out AStride: Integer);
begin
  { The canvas sizes the buffer in device pixels using the window's
    backing scale; remember it so drawRect maps the buffer onto points. }
  if Assigned(FWindow) then
    FBufScale := FWindow.BackingScale
  else
    FBufScale := 1;

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
begin
  if not Assigned(FView) or not BufferAllocated then
    Exit;
  if (w < 1) or (h < 1) then
    Exit;

  { Pass the buffer pointer to the view so drawRect can access it.
    On modern macOS (10.14+) views are layer-backed by default, which
    means lockFocus does not provide a valid NSGraphicsContext. The
    correct pattern is to hand the data to the view and let Cocoa call
    drawRect: in the display cycle, where a valid CGContext exists. }
  FView.setImageBuffer(FBuffer, FBufWidth, FBufHeight, FBufScale);

  { Mark the view as needing display and force an immediate
    redraw so the update appears without waiting for the next
    run loop iteration — gives immediate feedback like X11's XPutImage. }
  FView.setNeedsDisplay_(True);
  FView.displayIfNeeded;
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
