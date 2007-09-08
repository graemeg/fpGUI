{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      The resizer widget used in the form designer.
}

unit vfdresizer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  gfx_widget;

type
  TwgResizer = class(TfpgWidget)
  protected
    wgdesigner: TObject;
    FBackgroundColor: TfpgColor;
    procedure HandlePaint; override;
    procedure HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
  public
    direction: integer;
    FDragging: boolean;
    FDragPosX,
    FDragPosY: TfpgCoord;
    constructor Create(ACompDesigner: TObject; adirection: integer); reintroduce;
    procedure   Show;
  end;

implementation

uses
  vfddesigner,
  vfdforms;

{ TwgResizer }

procedure TwgResizer.HandlePaint;
begin
  inherited HandlePaint;
  Canvas.BeginDraw;
  Canvas.Clear(FBackgroundColor);
  Canvas.EndDraw;
end;

procedure TwgResizer.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  FDragging := True;
  FDragPosX := x;
  FDragPosy := y;
end;

procedure TwgResizer.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  FDragging := False;
end;

procedure TwgResizer.HandleMouseMove(x, y: integer; btnstate: word;
  shiftstate: TShiftState);
var
  dx,
  dy: integer;
  gridc: integer;
  wgd: TWidgetDesigner;
begin
//  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  if not FDragging then
    Exit;
  dx := x - FDragPosX;
  dy := y - FDragPosY;

  wgd   := TWidgetDesigner(wgdesigner);
  gridc := GridResolution;

  dx := dx - dx mod gridc;
  dy := dy - dy mod gridc;

  case direction of
    1: wgd.Widget.MoveAndResizeBy(dx, dy, -dx, -dy);
    2: wgd.Widget.MoveAndResizeBy(0, dy, 0, -dy);
    3: wgd.Widget.MoveAndResizeBy(0, dy, dx, -dy);
    4: wgd.Widget.MoveAndResizeBy(0, 0, dx, 0);
    5: wgd.Widget.MoveAndResizeBy(0, 0, dx, dy);
    6: wgd.Widget.MoveAndResizeBy(0, 0, 0, dy);
    7: wgd.Widget.MoveAndResizeBy(dx, 0, -dx, dy);
    8: wgd.Widget.MoveAndResizeBy(dx, 0, -dx, 0);
  end;
  wgd.UpdateResizerPositions;
  wgd.FormDesigner.UpdatePropWin;
end;

constructor TwgResizer.Create(ACompDesigner: TObject; adirection: integer);
begin
  inherited Create(TWidgetDesigner(aCompDesigner).Widget.Parent);
  FBackgroundColor := $404040;
  wgdesigner := aCompDesigner;
  FDragging := False;
  Width     := 5;
  Height    := 5;
  direction := adirection;
  case direction of
    1: MouseCursor := mcSizeSENW;
    2: MouseCursor := mcSizeNS;
    3: MouseCursor := mcSizeSWNE;
    4: MouseCursor := mcSizeEW;
    5: MouseCursor := mcSizeNWSE;
    6: MouseCursor := mcSizeNS;
    7: MouseCursor := mcSizeNESW;
    8: MouseCursor := mcSizeEW;
  end;
end;

procedure TwgResizer.Show;
begin
  HandleShow;
end;

end.

