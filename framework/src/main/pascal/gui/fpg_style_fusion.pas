{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Fusion fpGUI styles — inspired by Qt's Fusion theme.
      A clean, modern, flat-ish appearance with subtle gradients
      and a blue accent colour. Available in Light and Dark variants.
}

unit fpg_style_fusion;

{$I fpg_defines.inc}

interface

uses
  Classes,
  fpg_main,
  fpg_base;

type
  { Fusion colour palette indices:
      0  Window background
      1  Input/box background
      2  Dark shadow
      3  Widget frame (border)
      4  Primary text
      5  Selection background (accent)
      6  Scrollbar track
      7  Grid lines
      8  Focus/accent (splitter grab bar)
      9  Button gradient top
      10 Button gradient bottom
      11 Button hover gradient top
      12 Button hover gradient bottom
      13 Button pressed gradient top
      14 Button pressed gradient bottom
      15 Disabled text
      16 Menu separator
      17 Button highlight line
      18 Button border }
  TFusionColors = array [0..18] of TfpgColor;
  PFusionColors = ^TFusionColors;

  TfpgFusionStyle = class(TfpgStyle)
  protected
    FColors: PFusionColors;
    procedure LoadColors; virtual; abstract;
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
    { Menus }
    procedure   DrawMenuBar(ACanvas: TfpgCanvas; r: TfpgRect; ABackgroundColor: TfpgColor); override;
    procedure   DrawMenuRow(ACanvas: TfpgCanvas; r: TfpgRect; AFlags: TfpgMenuItemFlags); override;
    procedure   DrawMenuItemSeparator(ACanvas: TfpgCanvas; r: TfpgRect); override;
  end;

  TfpgFusionLightStyle = class(TfpgFusionStyle)
  protected
    procedure LoadColors; override;
  end;

  TfpgFusionDarkStyle = class(TfpgFusionStyle)
  protected
    procedure LoadColors; override;
  end;


implementation

uses
  fpg_stylemanager;

const
  FusionLightColors: TFusionColors = (
    $FFEFF0F1,  {  0 Window background }
    $FFFCFCFC,  {  1 Input/box background }
    $FF76797C,  {  2 Dark shadow }
    $FFBCBEC0,  {  3 Widget frame }
    $FF232627,  {  4 Primary text }
    $FF3DAEE9,  {  5 Selection/accent }
    $FFD8D8D8,  {  6 Scrollbar track }
    $FFBCBCBC,  {  7 Grid lines }
    $FF3DAEE9,  {  8 Focus/accent }
    $FFF6F6F6,  {  9 Button gradient top }
    $FFE4E4E4,  { 10 Button gradient bottom }
    $FFEAF6FD,  { 11 Button hover top }
    $FFD0E8F8,  { 12 Button hover bottom }
    $FFCCE4F7,  { 13 Button pressed top }
    $FFB6D6F0,  { 14 Button pressed bottom }
    $FFA0A0A4,  { 15 Disabled text }
    $FFD5D5D5,  { 16 Menu separator }
    $FFFFFFFF,  { 17 Button highlight line }
    $FF999EA3   { 18 Button border }
  );

  FusionDarkColors: TFusionColors = (
    $FF31363B,  {  0 Window background }
    $FF232629,  {  1 Input/box background }
    $FF1E1E1E,  {  2 Dark shadow }
    $FF54575B,  {  3 Widget frame }
    $FFEFF0F1,  {  4 Primary text }
    $FF3DAEE9,  {  5 Selection/accent }
    $FF3E4349,  {  6 Scrollbar track }
    $FF4A4E52,  {  7 Grid lines }
    $FF3DAEE9,  {  8 Focus/accent }
    $FF444A50,  {  9 Button gradient top }
    $FF383E44,  { 10 Button gradient bottom }
    $FF4F5862,  { 11 Button hover top }
    $FF434B55,  { 12 Button hover bottom }
    $FF2A3035,  { 13 Button pressed top }
    $FF252B30,  { 14 Button pressed bottom }
    $FF72767B,  { 15 Disabled text }
    $FF4A4E52,  { 16 Menu separator }
    $FF505860,  { 17 Button highlight line }
    $FF5E6164   { 18 Button border }
  );


{ TfpgFusionLightStyle }

procedure TfpgFusionLightStyle.LoadColors;
begin
  FColors := @FusionLightColors;
end;

{ TfpgFusionDarkStyle }

procedure TfpgFusionDarkStyle.LoadColors;
begin
  FColors := @FusionDarkColors;
end;


{ TfpgFusionStyle }

constructor TfpgFusionStyle.Create;
begin
  inherited Create;
  LoadColors;
  fpgSetNamedColor(clWindowBackground, FColors^[0]);
  fpgSetNamedColor(clBoxColor, FColors^[1]);
  fpgSetNamedColor(clShadow1, FColors^[2]);
  fpgSetNamedColor(clShadow2, FColors^[3]);
  fpgSetNamedColor(clHilite1, FColors^[3]);
  fpgSetNamedColor(clHilite2, FColors^[3]);
  fpgSetNamedColor(clText1, FColors^[4]);
  fpgSetNamedColor(clText4, FColors^[15]);
  fpgSetNamedColor(clSelection, FColors^[5]);
  fpgSetNamedColor(clSelectionText, $FFFFFFFF);
  fpgSetNamedColor(clInactiveSel, FColors^[3]);
  fpgSetNamedColor(clInactiveSelText, FColors^[4]);
  fpgSetNamedColor(clScrollBar, FColors^[6]);
  fpgSetNamedColor(clButtonFace, FColors^[0]);
  fpgSetNamedColor(clListBox, FColors^[1]);
  fpgSetNamedColor(clGridLines, FColors^[7]);
  fpgSetNamedColor(clGridHeader, FColors^[0]);
  fpgSetNamedColor(clWidgetFrame, FColors^[3]);
  fpgSetNamedColor(clInactiveWgFrame, FColors^[2]);
  fpgSetNamedColor(clMenuText, FColors^[4]);
  fpgSetNamedColor(clMenuDisabled, FColors^[15]);
  fpgSetNamedColor(clHintWindow, FColors^[1]);
  fpgSetNamedColor(clGridSelection, FColors^[5]);
  fpgSetNamedColor(clGridSelectionText, $FFFFFFFF);
  fpgSetNamedColor(clGridInactiveSel, FColors^[3]);
  fpgSetNamedColor(clGridInactiveSelText, FColors^[4]);
  fpgSetNamedColor(clSplitterGrabBar, FColors^[8]);
end;

procedure TfpgFusionStyle.DrawControlFrame(ACanvas: TfpgCanvas;
  x, y, w, h: TfpgCoord);
var
  r: TfpgRect;
begin
  r.SetRect(x, y, w, h);
  ACanvas.SetColor(FColors^[3]);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.DrawRectangle(r);
end;

procedure TfpgFusionStyle.DrawBevel(ACanvas: TfpgCanvas;
  x, y, w, h: TfpgCoord; ARaised: boolean);
var
  r: TfpgRect;
begin
  r.SetRect(x, y, w, h);
  ACanvas.SetLineStyle(1, lsSolid);
  if ARaised then
    ACanvas.GradientFill(r, FColors^[9], FColors^[10], gdVertical)
  else
    ACanvas.GradientFill(r, FColors^[13], FColors^[14], gdVertical);
  ACanvas.SetColor(FColors^[18]);
  ACanvas.DrawRectangle(r);
end;

procedure TfpgFusionStyle.DrawDirectionArrow(ACanvas: TfpgCanvas;
  x, y, w, h: TfpgCoord; direction: TArrowDirection);
begin
  ACanvas.SetColor(clText1);
  inherited DrawDirectionArrow(ACanvas, x + 1, y, w, h, direction);
end;

procedure TfpgFusionStyle.DrawString(ACanvas: TfpgCanvas; x, y: TfpgCoord;
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

procedure TfpgFusionStyle.DrawFocusRect(ACanvas: TfpgCanvas; r: TfpgRect);
begin
  ACanvas.SetColor(FColors^[8]);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.DrawRectangle(r);
end;

procedure TfpgFusionStyle.DrawButtonFace(ACanvas: TfpgCanvas;
  x, y, w, h: TfpgCoord; AFlags: TfpgButtonFlags);
var
  r: TfpgRect;
begin
  ACanvas.SetLineStyle(1, lsSolid);

  { Button interior }
  r.SetRect(x + 1, y + 1, w - 2, h - 2);

  if (btfIsPressed in AFlags) then
  begin
    ACanvas.GradientFill(r, FColors^[13], FColors^[14], gdVertical);
  end
  else if (btfHover in AFlags) then
  begin
    ACanvas.GradientFill(r, FColors^[11], FColors^[12], gdVertical);
  end
  else if (btfFlat in AFlags) then
  begin
    ACanvas.SetColor(clWindowBackground);
    ACanvas.FillRectangle(r);
  end
  else
  begin
    ACanvas.GradientFill(r, FColors^[9], FColors^[10], gdVertical);
  end;

  { Top highlight line for raised appearance }
  if not (btfFlat in AFlags) and not (btfIsPressed in AFlags) then
  begin
    ACanvas.SetColor(FColors^[17]);
    ACanvas.DrawLine(x + 2, y + 1, x + w - 2, y + 1);
  end;

  { Border }
  if not (btfFlat in AFlags) then
  begin
    ACanvas.SetColor(FColors^[18]);
    ACanvas.DrawRectangle(x, y, w, h);
  end;

  { Default button accent — draw a coloured border }
  if (btfIsDefault in AFlags) and not (btfIsPressed in AFlags) then
  begin
    ACanvas.SetColor(FColors^[8]);
    ACanvas.DrawRectangle(x, y, w, h);
  end;

  { Focus indicator — inner accent border }
  if (btfHasFocus in AFlags) and not (btfIsPressed in AFlags) then
  begin
    ACanvas.SetColor(FColors^[8]);
    ACanvas.DrawRectangle(x + 1, y + 1, w - 2, h - 2);
  end;
end;

function TfpgFusionStyle.GetButtonBorders: TRect;
begin
  Result := Rect(2, 2, 2, 2);
end;

function TfpgFusionStyle.GetButtonShift: TPoint;
begin
  Result := Point(0, 0);
end;

function TfpgFusionStyle.HasButtonHoverEffect: boolean;
begin
  Result := True;
end;

procedure TfpgFusionStyle.DrawMenuBar(ACanvas: TfpgCanvas; r: TfpgRect;
  ABackgroundColor: TfpgColor);
begin
  ACanvas.Clear(clWindowBackground);
  ACanvas.SetColor(FColors^[3]);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.DrawLine(r.Left, r.Bottom, r.Right + 1, r.Bottom);
end;

procedure TfpgFusionStyle.DrawMenuRow(ACanvas: TfpgCanvas; r: TfpgRect;
  AFlags: TfpgMenuItemFlags);
begin
  inherited DrawMenuRow(ACanvas, r, AFlags);
  if (mifSelected in AFlags) and not (mifSeparator in AFlags) then
  begin
    ACanvas.SetColor(FColors^[5]);
    ACanvas.FillRectangle(r);
  end;
end;

procedure TfpgFusionStyle.DrawMenuItemSeparator(ACanvas: TfpgCanvas;
  r: TfpgRect);
begin
  ACanvas.SetColor(FColors^[16]);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.DrawLine(r.Left + 1, r.Top + 2, r.Right, r.Top + 2);
end;


initialization
  fpgStyleManager.RegisterClass('Fusion Light', TfpgFusionLightStyle);
  fpgStyleManager.RegisterClass('Fusion Dark', TfpgFusionDarkStyle);

end.
