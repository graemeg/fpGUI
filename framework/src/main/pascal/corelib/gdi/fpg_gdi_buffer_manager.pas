{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    Description:
      Windows/GDI platform implementation of IBufferManager for the hybrid
      canvas. Manages a DIB section pixel buffer and blits it to a window
      via BitBlt.
}

unit fpg_gdi_buffer_manager;

{$mode objfpc}{$H+}

interface

uses
  Windows,
  fpg_impl,
  fpg_base,
  fpg_gdi;

type

  { TGDIBufferManager - Manages a DIB section pixel buffer and blits it
    to a Windows window via BitBlt }

  TGDIBufferManager = class(TInterfacedObject, IBufferManager)
  private
    FWinHandle: TfpgWinHandle;
    FDIBSection: HBITMAP;
    FBuffer: Pointer;       { Points into DIB section memory - do not free separately }
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


function CreateGDIBufferManager: IBufferManager;


implementation

uses
  fpg_main;


{ TGDIBufferManager }

constructor TGDIBufferManager.Create;
begin
  inherited Create;
  FWinHandle := 0;
  FDIBSection := 0;
  FBuffer := nil;
  FBufWidth := 0;
  FBufHeight := 0;
end;

destructor TGDIBufferManager.Destroy;
begin
  FreeBuffer;
  inherited Destroy;
end;

procedure TGDIBufferManager.AttachWindow(AWindow: TfpgWindowBase);
var
  win: TfpgGDIWindow;
begin
  win := TfpgGDIWindow(AWindow);
  FWinHandle := win.WinHandle;
end;

procedure TGDIBufferManager.DetachWindow;
begin
  FWinHandle := 0;
end;

procedure TGDIBufferManager.AllocateBuffer(AWidth, AHeight: Integer;
  out AData: Pointer; out AStride: Integer);
var
  bi: BITMAPINFO;
  dc: HDC;
begin
  { Free existing buffer if dimensions changed }
  if (FDIBSection <> 0) and ((FBufWidth <> AWidth) or (FBufHeight <> AHeight)) then
    FreeBuffer;

  if FDIBSection = 0 then
  begin
    FBufWidth := AWidth;
    FBufHeight := AHeight;
    AStride := AWidth * 4;  { 32-bit BGRA, 4 bytes per pixel }

    { Set up BITMAPINFO for a top-down 32-bit DIB section.
      Negative biHeight gives us top-down scanline order matching
      AggPas buffer layout. BI_RGB with 32 bits gives BGRA byte
      order natively on Windows - no channel swapping needed. }
    FillChar(bi, SizeOf(bi), 0);
    with bi.bmiHeader do
    begin
      biSize := SizeOf(bi.bmiHeader);
      biWidth := AWidth;
      biHeight := -AHeight;       { Negative = top-down DIB }
      biPlanes := 1;
      biBitCount := 32;
      biCompression := BI_RGB;
    end;

    { CreateDIBSection gives us a HBITMAP whose pixel memory we can
      write to directly via FBuffer. AggPas renders into FBuffer,
      and BitBlt reads from the same memory - zero copy. }
    dc := Windows.GetDC(0);
    FDIBSection := CreateDIBSection(dc, bi, DIB_RGB_COLORS, FBuffer, 0, 0);
    Windows.ReleaseDC(0, dc);

    if FDIBSection = 0 then
    begin
      FBuffer := nil;
      FBufWidth := 0;
      FBufHeight := 0;
      AData := nil;
      AStride := 0;
      Exit;
    end;

    { Clear buffer to zero (transparent black) }
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

function TGDIBufferManager.BufferAllocated: Boolean;
begin
  Result := (FDIBSection <> 0) and Assigned(FBuffer);
end;

procedure TGDIBufferManager.FreeBuffer;
begin
  if FDIBSection <> 0 then
  begin
    DeleteObject(FDIBSection);
    FDIBSection := 0;
    FBuffer := nil;  { Memory owned by DIB section, already freed }
  end;
  FBufWidth := 0;
  FBufHeight := 0;
end;

procedure TGDIBufferManager.PutBufferToScreen(x, y, w, h: TfpgCoord);
var
  srcdc: HDC;
  destdc: HDC;
  oldBmp: HBITMAP;
begin
  if (FWinHandle = 0) or not BufferAllocated then
    Exit;
  if (w < 1) or (h < 1) then
    Exit;

  destdc := Windows.GetDC(FWinHandle);
  if destdc = 0 then
    Exit;
  try
    srcdc := CreateCompatibleDC(destdc);
    if srcdc = 0 then
      Exit;
    try
      oldBmp := SelectObject(srcdc, FDIBSection);
      BitBlt(destdc, x, y, w, h, srcdc, x, y, SRCCOPY);
      SelectObject(srcdc, oldBmp);
    finally
      DeleteDC(srcdc);
    end;
  finally
    Windows.ReleaseDC(FWinHandle, destdc);
  end;
end;

procedure TGDIBufferManager.RestoreFromBuffer(const ARect: TfpgRect);
begin
  if (ARect.Width < 1) or (ARect.Height < 1) then
    Exit;
  PutBufferToScreen(ARect.Left, ARect.Top, ARect.Width, ARect.Height);
end;


function CreateGDIBufferManager: IBufferManager;
begin
  Result := TGDIBufferManager.Create;
end;


end.
