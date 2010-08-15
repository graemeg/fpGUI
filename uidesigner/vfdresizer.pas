{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
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
  fpg_base,
  fpg_widget;

type
  TwgResizer = class(TfpgWidget)
  protected
    wgdesigner: TObject;
    procedure   HandlePaint; override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
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
  vfdmain;

{ TwgResizer }

procedure TwgResizer.HandlePaint;
begin
  Canvas.Clear(FBackgroundColor);
end;

procedure TwgResizer.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  FDragging := True;
  FDragPosX := x;
  FDragPosy := y;
  {$IFDEF MSWINDOWS}
  CaptureMouse;
  {$ENDIF}
end;

procedure TwgResizer.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  FDragging := False;
  {$IFDEF MSWINDOWS}
  ReleaseMouse;
  {$ENDIF}
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

  if (not FDragging) or ((btnstate and MOUSE_LEFT) = 0) then
    Exit;
    
  dx := x - FDragPosX;
  dy := y - FDragPosY;

  wgd   := TWidgetDesigner(wgdesigner);
  gridc := maindsgn.GridResolution;

  dx := dx - (dx mod gridc);
  dy := dy - (dy mod gridc);

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
    1: MouseCursor := mcSizeSENW;   // top left
    2: MouseCursor := mcSizeNS;     // top
    3: MouseCursor := mcSizeSWNE;   // top right
    4: MouseCursor := mcSizeEW;     // right
    5: MouseCursor := mcSizeNWSE;   // bottom right
    6: MouseCursor := mcSizeNS;     // bottom
    7: MouseCursor := mcSizeNESW;   // bottom left
    8: MouseCursor := mcSizeEW;     // left
  end;
end;

procedure TwgResizer.Show;
begin
  HandleShow;
end;

end.

