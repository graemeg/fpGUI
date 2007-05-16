{
    fpGUI  -  Free Pascal GUI Library

    Windows style implementation

    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit WindowsStyle;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,SysUtils
  ,fpGUI
  ,fpgfx
  ;
  
  
type

  TWindowsStyle = class(TBasicStyle)
  end;
  
  // Win95 and Win98 look. ie: Buttons are different
  TWin9xStyle = class(TWindowsStyle)
  end;
  
  
  // Win2000 look. ie: Again the buttons are different (more flat)
  TWin2000Style = class(TWindowsStyle)
  public
    // Buttons
    procedure   DrawButtonFace(Canvas: TFCanvas; const ARect: TRect; Flags: TFButtonFlags); override;
    function    GetButtonBorders: TRect; override;
  end;


implementation


{ TWin2000Style }

procedure TWin2000Style.DrawButtonFace(Canvas: TFCanvas; const ARect: TRect;
  Flags: TFButtonFlags);
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
      Draw3DFrame(Canvas, r, cl3DLight, cl3DFace, cl3DDkShadow, cl3DShadow)
    else
      Draw3DFrame(Canvas, r, cl3DHighlight, cl3DFace, cl3DDkShadow, cl3DShadow);
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

function TWin2000Style.GetButtonBorders: TRect;
begin
  Result := Rect(4, 4, 4, 4);
end;

end.

