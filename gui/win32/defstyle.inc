{
    fpGUI  -  Free Pascal GUI Library
    
    Basic Style implementation for Win32
    
    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$DEFINE Has_DefaultStyle_GetGUIColor}

function TBasicStyle.GetGUIColor(Color: TColor): TGfxColor;
begin
  Color := Windows.GetSysColor(Color and $ffff);
  Result.Red := (Color and $ff) * 257;
  Result.Green := ((Color shr 8) and $ff) * 257;
  Result.Blue := ((Color shr 16) and $ff) * 257;
  Result.Alpha := 0;
end;


{$DEFINE Has_DefaultStyle_DrawFocusRect}

procedure TBasicStyle.DrawFocusRect(Canvas: TFCanvas; const ARect: TRect);
var
  Rect: Windows.TRect;
begin
  if Canvas.InheritsFrom(TGDICanvas) then
  begin
    Rect := RectToWinRect(Canvas.Transform(ARect));
    Windows.DrawFocusRect(TGDICanvas(Canvas).Handle, Rect)
  end
  else
  begin
    Canvas.SetColor(GetGUIColor(clGray));
    Canvas.DrawRect(ARect);
  end;
end;


