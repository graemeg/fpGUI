{
    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther
    Copyright (C) 2006 by Graeme Geldenhuys 
      member of the fpGUI development team.

    Default Style class implementation

    See the file COPYING.fpGUI, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$IFNDEF Has_DefaultStyle_GetGUIColor}

function TDefaultStyle.GetGUIColor(Color: TColor): TGfxColor;
begin
  case Color of
    // UI element colors
    clScrollBar:      Result := GetUIColor(clSilver);
    clMenu:           Result := GetUIColor(clSilver);
    clWindow:         Result := GetUIColor(clWhite);
    clMenuText:       Result := GetUIColor(clBlack);
    clWindowText:     Result := GetUIColor(clBlack);
    clAppWorkSpace:   Result := GetUIColor(clGray);
    clHighlight:      Result := GetUIColor(clNavy);
    clHighlightText:  Result := GetUIColor(clWhite);
    cl3DFace:         Result := GetUIColor(clSilver);
    cl3DShadow:       Result := GetUIColor(clGray);
    clGrayText:       Result := GetUIColor(clGray);
    clBtnText:        Result := GetUIColor(clBlack);
    cl3DHighlight:    Result := GetUIColor(clWhite);
    cl3DDkShadow:     Result := GetUIColor(clBlack);
    cl3DLight:        Result := rgbaDkWhite;
    clInfoText:       Result := GetUIColor(clBlack);
    clInfoBk:         Result := GetUIColor(clLightYellow);

    else              Result := GetUIColor(clWhite);
  end;
end;

{$ENDIF}


{$IFNDEF Has_DefaultStyle_DrawFocusRect}

procedure TDefaultStyle.DrawFocusRect(Canvas: TFCanvas; const ARect: TRect);
begin
  // !!!: Disabled for Linux as long a a certain nVidia X-Server has bugs with this...

  Canvas.SetColor(GetUIColor(cl3DDkShadow));
  Canvas.SetLineStyle(lsDot);
  Canvas.DrawRect(ARect);
  Canvas.SetLineStyle(lsSolid);

//  Canvas.SetColor(rgbaGray);
//  Canvas.DrawRect(ARect);
end;

{$ENDIF}


