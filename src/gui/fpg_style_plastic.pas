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
      Plastic fpGUI styles

    Author: Rochdi Abdelilah
}

{$define RegPlasticDark}
{$define RegPlasticDarkGray}
{$define RegPlasticMediumGray}
{$define RegPlasticLightGray}

{$IF not defined(RegPlasticDark) and
     not defined(RegPlasticDarkGray) and
     not defined(RegPlasticMediumGray) and
     not defined(RegPlasticLightGray))}
  {$define RegPlasticDark}
{$ifend}

unit fpg_style_plastic;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  fpg_main,
  fpg_base;

type
  TPlasticColors = array [0..22] of TfpgColor;
  PPlasticColors = ^TPlasticColors;

  TfpgPlasticStyle = class(TfpgStyle)
  protected
    FPlasticColors: PPlasticColors;
    procedure   LoadPlasticColors; virtual; abstract;
  public
    constructor Create; override;
    { General }
    procedure   DrawControlFrame(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord); override; overload;
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
    { Menus }
    procedure   DrawMenuItemSeparator(ACanvas: TfpgCanvas; r: TfpgRect); override;
  end;

  { TfpgPlasticDarkStyle }

  {$IFDEF RegPlasticDark}
  TfpgPlasticDarkStyle = class(TfpgPlasticStyle)
  protected
    procedure LoadPlasticColors; override;
  end;
  {$ENDIF}

  { TfpgPlasticDarkGrayStyle }

  {$IFDEF RegPlasticDarkGray}
  TfpgPlasticDarkGrayStyle = class(TfpgPlasticStyle)
  protected
    procedure LoadPlasticColors; override;
  end;
  {$ENDIF}

  { TfpgPlasticMediumGrayStyle }

  {$IFDEF RegPlasticMediumGray}
  TfpgPlasticMediumGrayStyle = class(TfpgPlasticStyle)
  protected
    procedure LoadPlasticColors; override;
  end;
  {$ENDIF}

  { TfpgPlasticLightGrayStyle }

  {$IFDEF RegPlasticLightGray}
  TfpgPlasticLightGrayStyle = class(TfpgPlasticStyle)
  protected
    procedure LoadPlasticColors; override;
  end;
  {$ENDIF}

implementation

uses
  fpg_stylemanager;

const
  {$IFDEF RegPlasticDark}
  PlasticDarkColors: TPlasticColors =
    ($FF343434, $FF222222, $FF141414, $FF454545,
    $FFDDDDDD, $FF4B5367, $FF464646, $FF101010,
    $FF4A669B, $FF373737, $FF303030, $FF1F1F1F,
    $FF878787, $FF696969, $FF646464, $FF4E4E4E,
    $FF262626, $FF1D1D1D, $FF3D3D3D, $FF272727,
    $FF282828, $FF292929, $FF2A2A2A);
  {$ENDIF}
  {$IFDEF RegPlasticDarkGray}
  PlasticDarkGrayColors: TPlasticColors =
    ($FF535353, $FF3a3a3a, $FF282828, $FF6A6A6A,
    $FFE5E5E5, $FF596678, $FF6A6A6A, $FF303030,
    $FF506FAC, $FF575757, $FF4D4D4D, $FF333333,
    $FFA0A0A0, $FF919191, $FF848484, $FF757575,
    $FF3F3F3F, $FF373737, $FF626262, $FF444444,
    $FF464646, $FF474747, $FF484848);
  {$ENDIF}
  {$IFDEF RegPlasticMediumGray}
  PlasticMediumGrayColors: TPlasticColors =
    ($FFB8B8B8, $FFFFFFFF, $FF707070, $FFCDCDCD,
    $FF373737, $FFB7CDF9, $FFC8C8C8, $FF686868,
    $FF74AAF3, $FFBBBBBB, $FFA9A9A9, $FF7F7F7F,
    $FFFAFAFA, $FFF7F7F7, $FFFEFEFE, $FFE7E7E7,
    $FF8D8D8D, $FF868686, $FFBDBDBD, $FF909090,
    $FF919191, $FF929292, $FF959595);
  {$ENDIF}
  {$IFDEF RegPlasticLightGray}
  PlasticLightGrayColors: TPlasticColors =
    ($FFD6D6D6, $FFFFFFFF, $FF737373, $FFEBEBEB,
    $FF373737, $FFB7CDF9, $FFE8E8E8, $FF7C7C7C,
    $FF9BCAFA, $FFD9D9D9, $FFC3C3C3, $FF999999,
    $FFFFFFFF, $FFF5F5F5, $FFFEFEFE, $FFE6E6E6,
    $FFA9A9A9, $FFA0A0A0, $FFD7D7D7, $FFACACAC,
    $FFAEAEAE, $FFB7B7B7, $FFBABABA);
  {$ENDIF}

{ TfpgPlasticLightGrayStyle }

{$IFDEF RegPlasticLightGray}
procedure TfpgPlasticLightGrayStyle.LoadPlasticColors;
begin
  FPlasticColors := @PlasticLightGrayColors;
end;
{$ENDIF}

{ TfpgPlasticMediumGrayStyle }

{$IFDEF RegPlasticMediumGray}
procedure TfpgPlasticMediumGrayStyle.LoadPlasticColors;
begin
  FPlasticColors := @PlasticMediumGrayColors;
end;
{$ENDIF}

{ TfpgPlasticDarkGrayStyle }

{$IFDEF RegPlasticDarkGray}
procedure TfpgPlasticDarkGrayStyle.LoadPlasticColors;
begin
  FPlasticColors := @PlasticDarkGrayColors;
end;
{$ENDIF}

{ TfpgPlasticDarkStyle }

{$IFDEF RegPlasticDark}
procedure TfpgPlasticDarkStyle.LoadPlasticColors;
begin
  FPlasticColors := @PlasticDarkColors;
end;
{$ENDIF}

{ TfpgPlasticStyle }

constructor TfpgPlasticStyle.Create;
begin
  inherited Create;
  LoadPlasticColors;
  fpgSetNamedColor(clWindowBackground, FPlasticColors^[0]);
  fpgSetNamedColor(clBoxColor, FPlasticColors^[1]);
  fpgSetNamedColor(clShadow1, FPlasticColors^[2]);
  fpgSetNamedColor(clShadow2, FPlasticColors^[3]);
  fpgSetNamedColor(clHilite1, FPlasticColors^[3]);
  fpgSetNamedColor(clHilite2, FPlasticColors^[3]);
  fpgSetNamedColor(clText1, FPlasticColors^[4]);
  fpgSetNamedColor(clText4, FPlasticColors^[2]);
  fpgSetNamedColor(clSelection, FPlasticColors^[5]);
  fpgSetNamedColor(clSelectionText, FPlasticColors^[4]);
  fpgSetNamedColor(clInactiveSel, FPlasticColors^[5]);
  fpgSetNamedColor(clInactiveSelText, FPlasticColors^[4]);
  fpgSetNamedColor(clScrollBar, FPlasticColors^[6]);
  fpgSetNamedColor(clButtonFace, FPlasticColors^[0]);
  fpgSetNamedColor(clListBox, FPlasticColors^[1]);
  fpgSetNamedColor(clGridLines, FPlasticColors^[7]);
  fpgSetNamedColor(clGridHeader, FPlasticColors^[0]);
  fpgSetNamedColor(clWidgetFrame, FPlasticColors^[3]);
  fpgSetNamedColor(clInactiveWgFrame, FPlasticColors^[2]);
  fpgSetNamedColor(clMenuText, FPlasticColors^[4]);
  fpgSetNamedColor(clHintWindow, FPlasticColors^[1]);
  fpgSetNamedColor(clGridSelection, FPlasticColors^[5]);
  fpgSetNamedColor(clGridSelectionText, FPlasticColors^[4]);
  fpgSetNamedColor(clGridInactiveSel, FPlasticColors^[5]);
  fpgSetNamedColor(clGridInactiveSelText, FPlasticColors^[4]);
  fpgSetNamedColor(clSplitterGrabBar, FPlasticColors^[8]);
end;

procedure TfpgPlasticStyle.DrawDirectionArrow(ACanvas: TfpgCanvas;
  x, y, w, h: TfpgCoord; direction: TArrowDirection);
begin
  ACanvas.SetColor(clText1);
  inherited DrawDirectionArrow(ACanvas, x + 2, y + 1, w - 2, h - 3, direction);
end;

procedure TfpgPlasticStyle.DrawBevel(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord;
  ARaised: boolean);
begin
  DrawButtonFace(ACanvas, x, y, w, h, [btfIsPressed]);
end;

procedure TfpgPlasticStyle.DrawString(ACanvas: TfpgCanvas; x, y: TfpgCoord;
  AText: string; AEnabled: boolean);
var
  lOldColor: TfpgColor;
begin
  if AText = '' then
    Exit;
  lOldColor := ACanvas.TextColor;
  if not AEnabled then
    ACanvas.SetTextColor(clText4)
  else
    ACanvas.SetTextColor(clText1);
  if lOldColor = clShadow1 then
    ACanvas.SetTextColor(clHilite2);
  ACanvas.DrawString(x, y, AText);
  if lOldColor <> clBlue then
    ACanvas.SetTextColor(lOldColor);
end;

procedure TfpgPlasticStyle.DrawControlFrame(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord);
begin
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.GradientFill(fpgRect(x, y, w, h), clWindowBackground, clScrollBar, gdVertical);
  ACanvas.Pixels[x, y + h - 1] := FPlasticColors^[9];
  ACanvas.Pixels[x + w - 1, y + h - 1] := FPlasticColors^[9];
  ACanvas.SetColor(clGridLines);
  ACanvas.DrawRectangle(fpgRect(x + 1, y + 1, w - 2, h - 2));
  ACanvas.Pixels[x + 1, y + 1] := FPlasticColors^[10];
  ACanvas.Pixels[x + w - 2, y + 1] := FPlasticColors^[10];
  ACanvas.Pixels[x + 1, y + h - 2] := FPlasticColors^[10];
  ACanvas.Pixels[x + w - 2, y + h - 2] := FPlasticColors^[10];
end;

procedure TfpgPlasticStyle.DrawFocusRect(ACanvas: TfpgCanvas; r: TfpgRect);
begin
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.SetColor(clSplitterGrabBar);
  ACanvas.DrawRectangle(r);
  ACanvas.Pixels[r.Left, r.Top] := FPlasticColors^[9];
  ACanvas.Pixels[r.Left + 1, r.Top + 1] := clSplitterGrabBar;
  ACanvas.Pixels[r.Width - 1, r.Top] := FPlasticColors^[9];
  ACanvas.Pixels[r.Width - 2, r.Top + 1] := clSplitterGrabBar;
  ACanvas.Pixels[r.Left, r.Height - 1] := FPlasticColors^[9];
  ACanvas.Pixels[r.Left + 1, r.Height - 2] := clSplitterGrabBar;
  ACanvas.Pixels[r.Width - 1, r.Height - 1] := FPlasticColors^[9];
  ACanvas.Pixels[r.Width - 2, r.Height - 2] := clSplitterGrabBar;
end;

procedure TfpgPlasticStyle.DrawButtonFace(ACanvas: TfpgCanvas;
  x, y, w, h: TfpgCoord; AFlags: TfpgButtonFlags);
var
  r: TfpgRect;
begin
  ACanvas.SetLineStyle(1, lsSolid);
  r.SetRect(x, y, w, h);
  DrawControlFrame(ACanvas, r);
  r.SetRect(x + 2, y + 3, w - 4, h - 5);

  if (btfIsPressed in AFlags) then
  begin
    ACanvas.GradientFill(r, FPlasticColors^[16], FPlasticColors^[17], gdVertical);
    ACanvas.SetColor(FPlasticColors^[11]);
  end
  else
  begin
    if btfHover in AFlags then
    begin
      ACanvas.GradientFill(r, FPlasticColors^[14], FPlasticColors^[15], gdVertical);
      ACanvas.SetColor(FPlasticColors^[12]);
    end
    else
    begin
      if not ((btfFlat in AFlags) and not (btfIsPressed in AFlags)) then
      begin
        ACanvas.GradientFill(r, FPlasticColors^[15], FPlasticColors^[18], gdVertical);
        ACanvas.SetColor(FPlasticColors^[13]);
      end
      else if btfFlat in AFlags then
      begin
        ACanvas.SetColor(clWindowBackground);
        ACanvas.FillRectangle(r);
      end;
    end;
  end;
  if not (btfFlat in AFlags) then
  begin
    if (btfIsDefault in AFlags) and not (btfIsPressed in AFlags) and
      not (btfHasFocus in AFlags) then
      ACanvas.SetColor(clSplitterGrabBar);
    ACanvas.DrawLine(x + 2, y + 2, x + w - 2, y + 2);
  end;
  ACanvas.Pixels[x + 2, y + 2] := FPlasticColors^[19];
  ACanvas.Pixels[x + w - 3, y + 2] := FPlasticColors^[20];
  ACanvas.Pixels[x + 2, y + h - 3] := FPlasticColors^[21];
  ACanvas.Pixels[x + w - 3, y + h - 3] := FPlasticColors^[22];
end;

function TfpgPlasticStyle.GetButtonBorders: TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function TfpgPlasticStyle.GetButtonShift: TPoint;
begin
  Result := Point(0, 0);
end;

function TfpgPlasticStyle.HasButtonHoverEffect: boolean;
begin
  Result := True;
end;

procedure TfpgPlasticStyle.DrawMenuBar(ACanvas: TfpgCanvas; r: TfpgRect;
  ABackgroundColor: TfpgColor);
begin
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.SetColor(clWindowBackground);
  ACanvas.FillRectangle(r);
  ACanvas.SetColor(clShadow2);
  ACanvas.DrawLine(r.Left, r.Top, r.Left + r.Right, r.Top);
  DrawMenuItemSeparator(ACanvas, fpgRect(r.Left - 1, r.Height - 4, r.Width, r.Height));
end;

procedure TfpgPlasticStyle.DrawMenuItemSeparator(ACanvas: TfpgCanvas; r: TfpgRect);
begin
  ACanvas.SetColor(clShadow1);
  ACanvas.DrawLine(r.Left + 1, r.Top + 2, r.Right, r.Top + 2);
  ACanvas.SetColor(clShadow2);
  ACanvas.DrawLine(r.Left + 1, r.Top + 3, r.Right, r.Top + 3);
end;


initialization
  {$IFDEF RegPlasticDark}
  fpgStyleManager.RegisterClass('Plastic Dark', TfpgPlasticDarkStyle);
  {$ENDIF}
  {$IFDEF RegPlasticDarkGray}
  fpgStyleManager.RegisterClass('Plastic Dark Gray', TfpgPlasticDarkGrayStyle);
  {$ENDIF}
  {$IFDEF RegPlasticMediumGray}
  fpgStyleManager.RegisterClass('Plastic Medium Gray', TfpgPlasticMediumGrayStyle);
  {$ENDIF}
  {$IFDEF RegPlasticLightGray}
  fpgStyleManager.RegisterClass('Plastic Light Gray', TfpgPlasticLightGrayStyle);
  {$ENDIF}
end.
