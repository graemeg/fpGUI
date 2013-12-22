{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2013 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Carbon fpGUI styles

    Author: Rochdi Abdelilah
}

unit fpg_style_carbon;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  fpg_main,
  fpg_base;

type

  TfpgCarbonStyle = class(TfpgStyle)
  public
    constructor Create; override;
    { General }
    procedure   DrawControlFrame(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord); override;
    function    GetControlFrameBorders: TRect; override;
    procedure   DrawBevel(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; ARaised: boolean = True); override;
    procedure   DrawDirectionArrow(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; direction: TArrowDirection); override;
    procedure   DrawString(ACanvas: TfpgCanvas; x, y: TfpgCoord; AText: string; AEnabled: boolean = True); override;
    procedure   DrawFocusRect(ACanvas: TfpgCanvas; r: TfpgRect); override;
    { Buttons }
    procedure   DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; AFlags: TfpgButtonFlags); override;
    function    GetButtonBorders: TRect; override;
    function    GetButtonShift: TPoint; override;
    function    HasButtonHoverEffect: boolean; override;
    procedure   DrawMenuBar(ACanvas: TfpgCanvas; r: TfpgRect; ABackgroundColor: TfpgColor); override;
    procedure   DrawMenuRow(ACanvas: TfpgCanvas; r: TfpgRect; AFlags: TfpgMenuItemFlags); override;
    procedure   DrawMenuItemSeparator(ACanvas: TfpgCanvas; r: TfpgRect); override;
  end;


implementation

uses
  fpg_stylemanager;

const
  CarbonBaseColors: array [0..15] of TfpgColor = (
    $FF333333, $FF191919, $FF616161,
    $FF202020, $FF474747, $FFC0C0C0,
    $FF6E6E6E, $FF3399FF, $FFEAEAEA,
    $FF2D2D2D, $FF494949, $FF24617A,
    $FF353535, $FF434343, $FF313131,
    $FF27546A);

{ TfpgCarbonStyle }

constructor TfpgCarbonStyle.Create;
begin
  inherited Create;
  fpgSetNamedColor(clWindowBackground, CarbonBaseColors[0]);
  fpgSetNamedColor(clBoxColor, CarbonBaseColors[1]);
  fpgSetNamedColor(clShadow1, CarbonBaseColors[2]);
  fpgSetNamedColor(clShadow2, CarbonBaseColors[1]);
  fpgSetNamedColor(clHilite1, CarbonBaseColors[3]);
  fpgSetNamedColor(clHilite2, CarbonBaseColors[4]);
  fpgSetNamedColor(clText1, CarbonBaseColors[5]);
  fpgSetNamedColor(clText4, CarbonBaseColors[6]);
  fpgSetNamedColor(clSelection, CarbonBaseColors[7]);
  fpgSetNamedColor(clSelectionText, CarbonBaseColors[8]);
  fpgSetNamedColor(clInactiveSel, CarbonBaseColors[7]);
  fpgSetNamedColor(clInactiveSelText, CarbonBaseColors[8]);
  fpgSetNamedColor(clScrollBar, CarbonBaseColors[9]);
  fpgSetNamedColor(clButtonFace, CarbonBaseColors[0]);
  fpgSetNamedColor(clListBox, CarbonBaseColors[1]);
  fpgSetNamedColor(clGridLines, CarbonBaseColors[2]);
  fpgSetNamedColor(clGridHeader, CarbonBaseColors[0]);
  fpgSetNamedColor(clWidgetFrame, CarbonBaseColors[2]);
  fpgSetNamedColor(clInactiveWgFrame, CarbonBaseColors[10]);
  fpgSetNamedColor(clUnset, CarbonBaseColors[11]);
  fpgSetNamedColor(clMenuText, CarbonBaseColors[5]);
  fpgSetNamedColor(clMenuDisabled, CarbonBaseColors[0]);
  fpgSetNamedColor(clHintWindow, CarbonBaseColors[0]);
  fpgSetNamedColor(clGridSelection, CarbonBaseColors[7]);
  fpgSetNamedColor(clGridSelectionText, CarbonBaseColors[8]);
  fpgSetNamedColor(clGridInactiveSel, CarbonBaseColors[7]);
  fpgSetNamedColor(clGridInactiveSelText, CarbonBaseColors[8]);
  fpgSetNamedColor(clSplitterGrabBar, CarbonBaseColors[7]);
end;

procedure TfpgCarbonStyle.DrawControlFrame(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord);
var
  r: TfpgRect;
begin
  r.SetRect(x, y, w, h);
  ACanvas.SetColor(clShadow1);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.DrawRectangle(r);
end;

function TfpgCarbonStyle.GetControlFrameBorders: TRect;
begin
  Result := Rect(1, 1, 1, 1);
end;

procedure TfpgCarbonStyle.DrawBevel(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord;
  ARaised: boolean);
begin
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.GradientFill(fpgRect(x,y,w,h), clUnset, CarbonBaseColors[15], gdVertical);
  ACanvas.SetColor(clHilite1);
  ACanvas.DrawRectangle(x, y, w, h);
end;

procedure TfpgCarbonStyle.DrawDirectionArrow(ACanvas: TfpgCanvas;
  x, y, w, h: TfpgCoord; direction: TArrowDirection);
begin
  ACanvas.SetColor(clBoxColor);
  inherited DrawDirectionArrow(ACanvas, x + 1, y + 1, w, h, direction);
end;

procedure TfpgCarbonStyle.DrawString(ACanvas: TfpgCanvas; x, y: TfpgCoord;
  AText: string; AEnabled: boolean);
begin
  if AText = '' then
    Exit;
  if not AEnabled then
    ACanvas.SetTextColor(clText4)
  else
    ACanvas.SetTextColor(clText1);
  ACanvas.DrawString(x, y, AText);
end;

procedure TfpgCarbonStyle.DrawButtonFace(ACanvas: TfpgCanvas;
  x, y, w, h: TfpgCoord; AFlags: TfpgButtonFlags);
var
  r: TfpgRect;
begin
  ACanvas.SetColor(clBoxColor);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.FillRectangle(x, y, w, h);

  r.SetRect(x + 1, y + 1, w - 2, h - 2);

  if (btfIsPressed in AFlags) then
    begin
      if (btfFlat in AFlags) or (btfHover in AFlags) then
        ACanvas.SetColor(clHilite2)
      else
      begin
        ACanvas.GradientFill(r, CarbonBaseColors[14], CarbonBaseColors[13], gdVertical);
        ACanvas.SetColor(clInactiveWgFrame);
      end;
    end
    else
    begin
      if btfHover in AFlags then
      begin
        ACanvas.GradientFill(r, clHilite2, CarbonBaseColors[12], gdVertical);
        ACanvas.SetColor(clShadow1);
      end
      else
      begin
        if not ((btfFlat in AFlags) and not (btfIsPressed in AFlags)) then
        begin
          ACanvas.GradientFill(r, CarbonBaseColors[13], CarbonBaseColors[14], gdVertical);
          ACanvas.SetColor(clInactiveWgFrame);
        end
        else if btfFlat in AFlags then
        begin
          ACanvas.SetColor(clButtonFace);
          ACanvas.FillRectangle(r);
        end;
      end;
    end;

  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.DrawRectangle(r);
  if btfIsDefault in AFlags then
  begin
    ACanvas.SetColor(clUnset);
    ACanvas.DrawLine(2, 1, w - 2, 1);
    ACanvas.DrawLine(2, h - 2, w - 2, h - 2);
  end;
end;

function TfpgCarbonStyle.GetButtonBorders: TRect;
begin
  Result := Rect(2, 2, 2, 2);
end;

function TfpgCarbonStyle.GetButtonShift: TPoint;
begin
  Result := Point(0, 0);
end;

function TfpgCarbonStyle.HasButtonHoverEffect: boolean;
begin
  Result := True;
end;

procedure TfpgCarbonStyle.DrawMenuRow(ACanvas: TfpgCanvas; r: TfpgRect;
  AFlags: TfpgMenuItemFlags);
begin
  inherited DrawMenuRow(ACanvas, r, AFlags);
  if (mifSelected in AFlags) and not (mifSeparator in AFlags) then
    ACanvas.GradientFill(r, clUnset, CarbonBaseColors[15], gdVertical);
end;


procedure TfpgCarbonStyle.DrawMenuBar(ACanvas: TfpgCanvas; r: TfpgRect;
  ABackgroundColor: TfpgColor);
begin
  ACanvas.Clear(clWindowBackground);
  ACanvas.SetColor(clShadow1);
  ACanvas.DrawLine(r.Left, r.Bottom-1, r.Right + 1, r.Bottom-1);
  ACanvas.SetColor(clBoxColor);
  ACanvas.DrawLine(r.Left, r.Bottom, r.Right + 1, r.Bottom);
end;

procedure TfpgCarbonStyle.DrawMenuItemSeparator(ACanvas: TfpgCanvas; r: TfpgRect);
begin
  ACanvas.SetColor(clBoxColor);
  ACanvas.DrawLine(r.Left + 1, r.Top + 2, r.Right, r.Top + 2);
end;

procedure TfpgCarbonStyle.DrawFocusRect(ACanvas: TfpgCanvas; r: TfpgRect);
begin
  ACanvas.SetColor(clUnset);
  ACanvas.SetLineStyle(1, lsSolid);
  //InflateRect(r, 1, 1);
  ACanvas.DrawRectangle(r);
end;

initialization
  fpgStyleManager.RegisterClass('Carbon', TfpgCarbonStyle);

end.
