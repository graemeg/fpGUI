{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2020 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit implements a canvas class that can directly draw on a shm
      segment used with Wayland. It's primary purpose was to aid drawing window
      decorations but it may not be used for that.
}
unit fpg_agg2d_canvas_wayland;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base,
  fpg_main, Agg2D, fpg_wayland_classes;

type

  { TAgg2dWaylandBufferCanvas }

  TAgg2dWaylandBufferCanvas = class(TAgg2D)
     FWindow: TfpgwWindow;
     FBuffer: TfpgwBuffer;
     //DoBeginDraw
     //GetPixel
     //SetPixel
     //DoXORFillRectangle
     procedure DoClearClipRect; override;
     function  DoGetClipRect: TfpgRect; override;
     function  GetBufferAllocated: Boolean; override;
     procedure DoAllocateBuffer; override;
     procedure DoBeginDraw(awidget: TfpgWidgetBase; CanvasTarget: TfpgCanvasBase); override;
     procedure DoPutBufferToScreen(x, y, w, h: TfpgCoord); override;
     procedure SetBuffer(ABuffer: TfpgwBuffer);
     constructor Create(AWindow: TfpgwWindow); reintroduce;
  end;

implementation
uses
  fpg_wayland, wayland_protocol;

{ TAgg2dWaylandBufferCanvas }

procedure TAgg2dWaylandBufferCanvas.DoClearClipRect;
begin
  {m_renBase.reset_clipping       (true );
  m_renBaseComp.reset_clipping   (true );
  m_renBasePre.reset_clipping    (true );
  m_renBaseCompPre.reset_clipping(true );
  ClipBox(0, 0, FWindow.Width, FWindow.Height);

  m_rasterizer.m_clipping := False;}
end;

function TAgg2dWaylandBufferCanvas.DoGetClipRect: TfpgRect;
begin
  Result:=fpgRect(0,0, 0, 0);
end;

function TAgg2dWaylandBufferCanvas.GetBufferAllocated: Boolean;
begin
  Result := (FImg<>nil);
  if not Result then
    Exit;

  FImg.AllocateImage(32, FWindow.GetWidth, FWindow.GetHeight);
end;

procedure TAgg2dWaylandBufferCanvas.DoAllocateBuffer;
begin
  //WriteLn('TAgg2dWaylandBufferCanvas.DoAllocateBuffer;');
  {FBuffer := FWindow.NextBuffer;
  FBuffer.Allocate(FWindow.Width, FWindow.Height, WL_SHM_FORMAT_ARGB8888);}
end;

procedure TAgg2dWaylandBufferCanvas.DoBeginDraw(awidget: TfpgWidgetBase;
  CanvasTarget: TfpgCanvasBase);
begin
  Attach(FBuffer.Data, pf32bit, FBuffer.Width, FBuffer.Height, FBuffer.Stride, 0);
end;

procedure TAgg2dWaylandBufferCanvas.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
begin
  FBuffer.SetPaintRect(x,y,w,h);
  //FWindow.Paint(FBuffer);
  //WriteLn(Format('Putting Buffer to Screen xy[%d:%d]  wh[%d:%d]', [x,y,w,h]));
end;

procedure TAgg2dWaylandBufferCanvas.SetBuffer(ABuffer: TfpgwBuffer);
begin
  FBuffer := ABuffer;
end;

constructor TAgg2dWaylandBufferCanvas.Create(AWindow: TfpgwWindow);
begin
  FWindow := AWindow;
  inherited Create(nil);
end;

end.

