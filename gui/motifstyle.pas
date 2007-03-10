{
    fpGUI  -  Free Pascal GUI Library

    Motif style implementation

    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit MotifStyle;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,SysUtils
  ,fpGUI
  ,fpGFX
  ;
  
  
type

  TMotifStyle = class(TBasicStyle)
  public
    // General
    procedure   DrawFocusRect(Canvas: TFCanvas; const ARect: TRect); override;
    // Buttons
    procedure   DrawButtonFace(Canvas: TFCanvas; const ARect: TRect; Flags: TButtonFlags); override;
    // Check boxes
    procedure   DrawCheckBox(Canvas: TFCanvas; const ARect, LabelRect: TRect; Flags: TCheckboxFlags); override;
  end;


implementation


{ MotifStyle }

procedure TMotifStyle.DrawButtonFace(Canvas: TFCanvas; const ARect: TRect;
  Flags: TButtonFlags);
var
  r: TRect;
begin
  r := ARect;

  if btnIsSelected in Flags then
  begin
    SetUIColor(Canvas, cl3DDkShadow);
    Canvas.DrawRect(r);
    Inc(r.Left);
    Inc(r.Top);
    Dec(r.Right);
    Dec(r.Bottom);
  end;

  if btnIsPressed in Flags then
  begin
    SetUIColor(Canvas, cl3DShadow);
    Canvas.DrawRect(r);
    Inc(r.Left);
    Inc(r.Top);
    Dec(r.Right);
    Dec(r.Bottom);
  end else
  begin
    if btnIsEmbedded in Flags then
      Draw3DFrame(Canvas, r, cl3DLight, cl3DHighlight, cl3DDkShadow, cl3DShadow)
    else
      Draw3DFrame(Canvas, r, cl3DHighlight, cl3DLight, cl3DDkShadow, cl3DShadow);
    Inc(r.Left, 2);
    Inc(r.Top, 2);
    Dec(r.Right, 2);
    Dec(r.Bottom, 2);
  end;

  SetUIColor(Canvas, cl3DFace);
  Canvas.FillRect(r);

  if btnHasFocus in Flags then
  begin
    r.Left    := ARect.Left + 4;
    r.Top     := ARect.Top + 4;
    r.Right   := ARect.Right - 4;
    r.Bottom  := ARect.Bottom - 4;
    DrawFocusRect(Canvas, r);
  end;
end;

procedure TMotifStyle.DrawFocusRect(Canvas: TFCanvas; const ARect: TRect);
begin
  SetUIColor(Canvas, clGray);
  Canvas.DrawRect(ARect);
end;

procedure TMotifStyle.DrawCheckBox(Canvas: TFCanvas; const ARect,
  LabelRect: TRect; Flags: TCheckboxFlags);
var
  r: TRect;
  xmid: integer;
  ymid: integer;
  
  procedure _DrawBottomHalf;
  begin
    // draw the bottom \ line
    Canvas.DrawLine(Point(r.Left+1, r.Top+ymid+1), Point(r.Left+xmid, r.Bottom));
    Canvas.DrawLine(Point(r.Left+2, r.Top+ymid+1), Point(r.Left+xmid, r.Bottom-1));
    Canvas.DrawLine(Point(r.Left+3, r.Top+ymid+1), Point(r.Left+xmid, r.Bottom-2));
    // draw the bottom / line
    Canvas.DrawLine(Point(r.Left+xmid, r.Bottom), Point(r.Right, r.Top+ymid-1));
    Canvas.DrawLine(Point(r.Left+xmid, r.Bottom-1), Point(r.Right-1, r.Top+ymid-1));
    Canvas.DrawLine(Point(r.Left+xmid, r.Bottom-2), Point(r.Right-2, r.Top+ymid-1));
  end;
  
  procedure _DrawTopHalf;
  begin
    // draw the top / line
    Canvas.DrawLine(Point(r.Left, r.Top+ymid), Point(r.Left+xmid+1, r.Top));
    Canvas.DrawLine(Point(r.Left+1, r.Top+ymid), Point(r.Left+xmid+1, r.Top+1));
    Canvas.DrawLine(Point(r.Left+2, r.Top+ymid), Point(r.Left+xmid+1, r.Top+2));
    // draw the top \ line
    Canvas.DrawLine(Point(r.Left+xmid+1, r.Top+2), Point(r.Right-1, r.Top+ymid));
    Canvas.DrawLine(Point(r.Left+xmid+1, r.Top+3), Point(r.Right-2, r.Top+ymid));
    Canvas.DrawLine(Point(r.Left+xmid+1, r.Top+4), Point(r.Right-3, r.Top+ymid));
  end;
  
begin
  SetUIColor(Canvas, cl3DFace);
  Canvas.FillRect(ARect);

  r.Left    := ARect.Left;
  r.Top     := ARect.Top + (ARect.Bottom - ARect.Top - 13) div 2;
  r.Right   := 13;
  r.Bottom  := r.Top + 13;
  xmid      := ((r.Right - r.Left) div 2);
  ymid      := ((r.Bottom - r.Top) div 2) + 1;

  if (cbIsChecked in Flags) then
  begin
    SetUIColor(Canvas, clWhite);
    _DrawBottomHalf;
    SetUIColor(Canvas, cl3DShadow);
    _DrawTopHalf;
  end
  else
  begin
    SetUIColor(Canvas, cl3DShadow);
    _DrawBottomHalf;
    SetUIColor(Canvas, clWhite);
    _DrawTopHalf;
  end;

  if cbHasFocus in Flags then
    with LabelRect do
      DrawFocusRect(Canvas, Rect(Left - 2, Top - 2, Right + 2, Bottom + 2));

end;


//initialization
//finalization
//  gStyleManager.RegisterClass('Motif', TMotifStyle);

end.

