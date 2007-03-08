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
begin
  inherited DrawCheckBox(Canvas, ARect, LabelRect, Flags);
end;


//initialization
//finalization
//  gStyleManager.RegisterClass('Motif', TMotifStyle);

end.

