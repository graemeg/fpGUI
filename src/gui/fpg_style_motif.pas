{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2011 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Implements the classic Motif / CDE look.
}
unit fpg_style_motif;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,fpg_base
  ,fpg_main
  ;

type

  TfpgMotifStyle = class(TfpgStyle)
  public
    constructor Create; override;
    { General }
    procedure   DrawControlFrame(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord); override;
    procedure   DrawFocusRect(ACanvas: TfpgCanvas; r: TfpgRect); override;
    { Buttons }
    procedure   DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; AFlags: TfpgButtonFlags); override;
    function    GetButtonBorders: TRect; override;
  end;

implementation

uses
  fpg_stylemanager
  ;

{ TfpgMotifStyle }

constructor TfpgMotifStyle.Create;
begin
  inherited Create;

  fpgSetNamedColor(clWindowBackground, $999999);
  fpgSetNamedColor(clButtonFace, $999999);
  fpgSetNamedColor(clBoxColor, $999999);
  fpgSetNamedColor(clListBox, $999999);

end;

procedure TfpgMotifStyle.DrawControlFrame(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord);
var
  r: TfpgRect;
begin
  r.SetRect(x, y, w, h);
  ACanvas.SetColor(clShadow2);
  ACanvas.DrawLine(r.Left, r.Bottom, r.Left, r.Top); // left (outer)
  ACanvas.DrawLine(r.Left, r.Top, r.Right, r.Top);   // top (outer)

  ACanvas.SetColor(clHilite1);
  ACanvas.DrawLine(r.Right, r.Top, r.Right, r.Bottom);   // right (outer)
  ACanvas.DrawLine(r.Right, r.Bottom, r.Left, r.Bottom); // bottom (outer)

  ACanvas.SetColor(clShadow2);
  ACanvas.DrawLine(r.Left+1, r.Bottom-1, r.Left+1, r.Top+1);   // left (inner)
  ACanvas.DrawLine(r.Left+1, r.Top+1, r.Right-1, r.Top+1);   // top (inner)

  ACanvas.SetColor(clHilite1);
  ACanvas.DrawLine(r.Right-1, r.Top+1, r.Right-1, r.Bottom-1);   // right (inner)
  ACanvas.DrawLine(r.Right-1, r.Bottom-1, r.Left+1, r.Bottom-1);   // bottom (inner)
end;

procedure TfpgMotifStyle.DrawFocusRect(ACanvas: TfpgCanvas; r: TfpgRect);
var
  oldColor: TfpgColor;
  oldLineWidth: integer;
  oldLineStyle: TfpgLineStyle;
begin
  oldColor      := ACanvas.Color;
  oldLineWidth  := ACanvas.GetLineWidth;
  oldLineStyle  := ACanvas.LineStyle;

  ACanvas.SetColor(clBlack);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.DrawRectangle(r);

  // restore previous settings
  ACanvas.SetColor(oldColor);
  ACanvas.SetLineStyle(oldLineWidth, oldLineStyle);
end;

procedure TfpgMotifStyle.DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; AFlags: TfpgButtonFlags);
var
  r: TfpgRect;
begin
  r.SetRect(x, y, w, h);

  if btfIsDefault in AFlags then
  begin
    ACanvas.SetColor(clBlack);
    ACanvas.SetLineStyle(1, lsSolid);
    ACanvas.DrawRectangle(r);
    InflateRect(r, -1, -1);
    Exclude(AFlags, btfIsDefault);
    fpgStyle.DrawButtonFace(ACanvas, r.Left, r.Top, r.Width, r.Height, AFlags);
    Exit; //==>
  end;

  { Clear the rectangle with a color }
  ACanvas.SetColor(clButtonFace);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.FillRectangle(x, y, w, h);

  if (btfFlat in AFlags) and not (btfIsPressed in AFlags) then
    Exit; // no need to go further

  // Left and Top (outer)
  if (btfIsPressed in AFlags) then
  begin
    if (btfIsEmbedded in AFlags) then
      ACanvas.SetColor(clHilite2)
    else
    begin
      if (btfFlat in AFlags) or (btfHover in AFlags) then
        ACanvas.SetColor(clShadow1)  { light shadow }
      else
        ACanvas.SetColor(clShadow2); { dark shadow }
    end;
  end
  else
    ACanvas.SetColor(clHilite1);

  ACanvas.DrawLine(r.Left, r.Bottom, r.Left, r.Top);  // left
  ACanvas.DrawLine(r.Left, r.Top, r.Right, r.Top);    // top

  // Left and Top (inner)
  if not ((btfFlat in AFlags) or (btfHover in AFlags)) then
  begin
    ACanvas.DrawLine(r.left+1, r.bottom-1, r.left+1, r.top+1);  // left
    ACanvas.DrawLine(r.left+1, r.top+1, r.right-1, r.top+1);    // top
  end;

  // Right and Bottom (outer)
  if (btfIsPressed in AFlags) then
  begin
    if (btfIsEmbedded in AFlags) then
      ACanvas.SetColor(clHilite1)
    else
    begin
      if (btfFlat in AFlags) or (btfHover in AFlags) then
        ACanvas.SetColor(clHilite2)  { light shadow }
      else
        ACanvas.SetColor(clShadow2); { dark shadow }
    end;
  end
  else
  begin
    if btfHover in AFlags then
      ACanvas.SetColor(clShadow1)  { light shadow }
    else
      ACanvas.SetColor(clShadow2); { dark shadow }
  end;

  ACanvas.DrawLine(r.Right, r.Top, r.Right, r.Bottom);   // right
  ACanvas.DrawLine(r.Right, r.Bottom, r.Left-1, r.Bottom);   // bottom

  if (btfFlat in AFlags) or (btfHover in AFlags) then
    exit; { "toolbar" style buttons need a nice thin/flat border }

  // Right and Bottom (inner)
  if btfIsPressed in AFlags then
  begin
    if (btfIsEmbedded in AFlags) then
      ACanvas.SetColor(clButtonFace)
    else
      ACanvas.SetColor(clHilite1);
  end
  else
    ACanvas.SetColor(clShadow2);

  ACanvas.DrawLine(r.Right-1, r.Top+1, r.Right-1, r.Bottom-1);   // right
  ACanvas.DrawLine(r.Right-1, r.Bottom-1, r.Left, r.Bottom-1);   // bottom
end;

function TfpgMotifStyle.GetButtonBorders: TRect;
begin
  Result := Rect(4, 4, 4, 4);
end;


initialization
  fpgStyleManager.RegisterClass('Motif', TfpgMotifStyle);

end.

