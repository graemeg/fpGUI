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
  SysUtils,
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
      18 Button border
      19 Progress bar gradient top
      20 Progress bar gradient bottom
      21 Progress bar highlight line
      22 Progress bar border
      23 Progress bar track background
      24 Checkbox background
      25 Checkbox border
      26 Checkbox check mark colour
      27 Checkbox pressed background
      28 Inactive tab background
      29 Tab border }
  TFusionColors = array [0..29] of TfpgColor;
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
    { ProgressBar }
    procedure   DrawProgressBar(ACanvas: TfpgCanvas; AParams: TfpgStyleDrawProgressBar); override;
    { Checkbox }
    function    GetCheckBoxSize: integer; override;
    procedure   DrawCheckBox(ACanvas: TfpgCanvas; r: TfpgRect; AFlags: TfpgCheckBoxFlags); override;
    { RadioButton }
    function    GetRadioButtonSize: integer; override;
    procedure   DrawRadioButton(ACanvas: TfpgCanvas; r: TfpgRect; AFlags: TfpgCheckBoxFlags); override;
    { PageControl & Tabs }
    procedure   DrawPageControlBody(ACanvas: TfpgCanvas; r: TfpgRect); override;
    procedure   DrawPageControlTab(ACanvas: TfpgCanvas; AParams: TfpgStyleDrawTab); override;
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
  fpg_stylemanager,
  fpg_tab;

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
    $FF999EA3,  { 18 Button border }
    $FF4DB8F0,  { 19 Progress bar gradient top }
    $FF2D8BC9,  { 20 Progress bar gradient bottom }
    $FF6EC6F5,  { 21 Progress bar highlight line }
    $FF2980B9,  { 22 Progress bar border }
    $FFD4D4D4,  { 23 Progress bar track background }
    $FFFCFCFC,  { 24 Checkbox background }
    $FF999EA3,  { 25 Checkbox border }
    $FF232627,  { 26 Checkbox check mark colour }
    $FFD0E8F8,  { 27 Checkbox pressed background }
    $FFD8DADC,  { 28 Inactive tab background }
    $FFBCBEC0   { 29 Tab border }
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
    $FF5E6164,  { 18 Button border }
    $FF3DAEE9,  { 19 Progress bar gradient top }
    $FF2A8BC4,  { 20 Progress bar gradient bottom }
    $FF5BBEF0,  { 21 Progress bar highlight line }
    $FF1F7AAE,  { 22 Progress bar border }
    $FF3E4349,  { 23 Progress bar track background }
    $FF232629,  { 24 Checkbox background }
    $FF5E6164,  { 25 Checkbox border }
    $FFEFF0F1,  { 26 Checkbox check mark colour }
    $FF2A3035,  { 27 Checkbox pressed background }
    $FF272B30,  { 28 Inactive tab background }
    $FF54575B   { 29 Tab border }
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
  fpgSetNamedColor(clText2, FColors^[5]);   { accent text — for emphasis/category labels }
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
  else if fpgIsNamedColor(ACanvas.TextColor) then
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

procedure TfpgFusionStyle.DrawProgressBar(ACanvas: TfpgCanvas;
  AParams: TfpgStyleDrawProgressBar);
var
  r, fill: TfpgRect;
  diff: integer;
  aPos: integer;
  pos: integer;
  percent: integer;
  txt: string;
  x, y: TfpgCoord;
begin
  r := AParams.Rect;

  { Calculate position }
  diff    := AParams.Max - AParams.Min;
  aPos    := AParams.Position - AParams.Min;
  percent := round(((100 / diff) * aPos));
  pos     := round(percent * (r.Width - 2) / 100);

  { Track background }
  ACanvas.SetColor(FColors^[23]);
  ACanvas.FillRectangle(r);

  { Track border }
  ACanvas.SetColor(FColors^[3]);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.DrawRectangle(r);

  if AParams.Position > AParams.Min then
  begin
    { Fill bar area }
    fill.SetRect(r.Left + 1, r.Top + 1, pos, r.Height - 2);

    { Gradient fill using accent colours }
    ACanvas.GradientFill(fill, FColors^[19], FColors^[20], gdVertical);

    { Top highlight line for subtle raised appearance }
    ACanvas.SetColor(FColors^[21]);
    ACanvas.DrawLine(fill.Left, fill.Top, fill.Right, fill.Top);

    { Fill bar border }
    ACanvas.SetColor(FColors^[22]);
    ACanvas.DrawRectangle(fill);
  end;

  { Paint percentage text if required }
  if AParams.ShowCaption then
  begin
    txt := IntToStr(percent) + '%';
    x := r.Left + (r.Width - AParams.Font.GetTextWidth(txt)) div 2;
    y := r.Top + (r.Height - AParams.Font.GetHeight) div 2;
    ACanvas.SetFont(AParams.Font);
    { Use contrasting text — white over fill, normal text over track }
    ACanvas.SetTextColor(AParams.TextColor);
    ACanvas.DrawString(x, y, txt);
  end;
end;

function TfpgFusionStyle.GetCheckBoxSize: integer;
begin
  Result := 20; // 20x20 - it is always a rectangle
end;

procedure TfpgFusionStyle.DrawCheckBox(ACanvas: TfpgCanvas; r: TfpgRect;
  AFlags: TfpgCheckBoxFlags);
var
  cx, cy: TfpgCoord;
  p1x, p1y, p2x, p2y, p3x, p3y: TfpgCoord;
begin
  ACanvas.SetLineStyle(1, lsSolid);

  { Checkbox box background }
  if cbfPressed in AFlags then
    ACanvas.SetColor(FColors^[27])
  else
    ACanvas.SetColor(FColors^[24]);
  ACanvas.FillRectangle(r);

  { Border — use accent colour when focused, normal border otherwise }
  if cbfHasFocus in AFlags then
    ACanvas.SetColor(FColors^[8])
  else
    ACanvas.SetColor(FColors^[25]);
  ACanvas.DrawRectangle(r);

  { Draw the check mark }
  if cbfChecked in AFlags then
  begin
    if (cbfEnabled in AFlags) and not (cbfReadOnly in AFlags) then
      ACanvas.SetColor(FColors^[8])  { accent colour for check mark }
    else
      ACanvas.SetColor(FColors^[15]);  { disabled text colour }
    ACanvas.SetLineStyle(3, lsSolid);
(*
    { Draw a tick/check mark centred in the box }
    cx := r.Left + (r.Width div 2);
    cy := r.Top + (r.Height div 2);
    { Short leg of tick: bottom-left to centre-bottom }
    ACanvas.DrawLine(cx - 3, cy, cx - 1, cy + 3);
    { Long leg of tick: centre-bottom to top-right }
    ACanvas.DrawLine(cx - 1, cy + 3, cx + 4, cy - 2);
    ACanvas.SetLineStyle(1, lsSolid);
*)
    { Calculate all three anchor points }
    p1x := r.Left + Round(r.Width * 0.22);
    p1y := r.Top + Round(r.Height * 0.50);

    p2x := r.Left + Round(r.Width * 0.42);
    p2y := r.Top + Round(r.Height * 0.75);

    p3x := r.Left + Round(r.Width * 0.70);
    p3y := r.Top + Round(r.Height * 0.20);

    ACanvas.DrawLine(p1x, p1y, p2x, p2y);
    ACanvas.DrawLine(p2x, p2y, p3x, p3y);
    ACanvas.SetLineStyle(1, lsSolid);
  end;
end;

function TfpgFusionStyle.GetRadioButtonSize: integer;
begin
  Result := 14;
end;

procedure TfpgFusionStyle.DrawRadioButton(ACanvas: TfpgCanvas; r: TfpgRect;
  AFlags: TfpgCheckBoxFlags);
var
  cx, cy, radius: integer;
begin
  cx := r.Left + (r.Width div 2);
  cy := r.Top + (r.Height div 2);
  radius := (r.Width div 2) - 1;

  { Outer circle background }
  if cbfPressed in AFlags then
    ACanvas.SetColor(FColors^[27])
  else
    ACanvas.SetColor(FColors^[24]);
  ACanvas.FillArc(cx - radius, cy - radius, radius * 2, radius * 2, 0, 360);

  { Border circle — accent when focused, normal border otherwise }
  if cbfHasFocus in AFlags then
    ACanvas.SetColor(FColors^[8])
  else
    ACanvas.SetColor(FColors^[25]);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.DrawArc(cx - radius, cy - radius, radius * 2, radius * 2, 0, 360);

  { Inner dot when selected }
  if cbfChecked in AFlags then
  begin
    if (cbfEnabled in AFlags) and not (cbfReadOnly in AFlags) then
      ACanvas.SetColor(FColors^[8])  { accent colour }
    else
      ACanvas.SetColor(FColors^[15]);  { disabled colour }
    ACanvas.FillArc(cx - 3, cy - 3, 6, 6, 0, 360);
  end;
end;

procedure TfpgFusionStyle.DrawPageControlBody(ACanvas: TfpgCanvas; r: TfpgRect);
begin
  ACanvas.SetColor(clWindowBackground);
  ACanvas.FillRectangle(r);
  ACanvas.SetColor(FColors^[29]);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.DrawRectangle(r);
  { Subtle bottom shadow line — lighter than border, darker than background }
  ACanvas.SetColor(FColors^[6]);
  ACanvas.DrawLine(r.Left, r.Bottom, r.Right + 1, r.Bottom);
end;

procedure TfpgFusionStyle.DrawPageControlTab(ACanvas: TfpgCanvas;
  AParams: TfpgStyleDrawTab);
var
  r: TfpgRect;
  activeColor: TfpgColor;
begin
  r := AParams.TabRect;
  ACanvas.SetLineStyle(1, lsSolid);

  { Determine the active tab colour }
  if TfpgTabSheet(AParams.TabSheet).PageControl.ActiveTabColor = clDefault then
    activeColor := TfpgTabSheet(AParams.TabSheet).TabColor
  else
    activeColor := TfpgTabSheet(AParams.TabSheet).PageControl.ActiveTabColor;

  case AParams.TabPosition of
    tpTop:
      begin
        if AParams.IsSelected then
        begin
          { Active tab: light background, no bottom border }
          ACanvas.SetColor(activeColor);
          ACanvas.FillRectangle(r.Left + 1, r.Top + 1, r.Width - 2, r.Height - 1);
          ACanvas.SetColor(FColors^[29]);
          ACanvas.DrawLine(r.Left, r.Bottom - 1, r.Left, r.Top + 1);    // left
          ACanvas.DrawLine(r.Left + 1, r.Top, r.Right - 1, r.Top);      // top
          ACanvas.DrawLine(r.Right - 1, r.Top + 1, r.Right - 1, r.Bottom - 1); // right
        end
        else
        begin
          { Inactive tab: shaded background }
          ACanvas.SetColor(FColors^[28]);
          ACanvas.FillRectangle(r.Left + 1, r.Top + 1, r.Width - 2, r.Height - 2);
          ACanvas.SetColor(FColors^[29]);
          ACanvas.DrawLine(r.Left, r.Bottom - 1, r.Left, r.Top + 1);    // left
          ACanvas.DrawLine(r.Left + 1, r.Top, r.Right - 1, r.Top);      // top
          ACanvas.DrawLine(r.Right - 1, r.Top + 1, r.Right - 1, r.Bottom - 1); // right
          ACanvas.DrawLine(r.Left, r.Bottom - 1, r.Right, r.Bottom - 1);       // bottom
        end;
      end;

    tpBottom:
      begin
        if AParams.IsSelected then
        begin
          ACanvas.SetColor(activeColor);
          ACanvas.FillRectangle(r.Left + 1, r.Top, r.Width - 2, r.Height - 1);
          ACanvas.SetColor(FColors^[29]);
          ACanvas.DrawLine(r.Left, r.Top, r.Left, r.Bottom - 1);        // left
          ACanvas.DrawLine(r.Left + 1, r.Bottom - 1, r.Right - 1, r.Bottom - 1); // bottom
          ACanvas.DrawLine(r.Right - 1, r.Top, r.Right - 1, r.Bottom - 1);      // right
        end
        else
        begin
          ACanvas.SetColor(FColors^[28]);
          ACanvas.FillRectangle(r.Left + 1, r.Top + 1, r.Width - 2, r.Height - 2);
          ACanvas.SetColor(FColors^[29]);
          ACanvas.DrawLine(r.Left, r.Top, r.Left, r.Bottom - 1);        // left
          ACanvas.DrawLine(r.Left + 1, r.Bottom - 1, r.Right - 1, r.Bottom - 1); // bottom
          ACanvas.DrawLine(r.Right - 1, r.Top, r.Right - 1, r.Bottom - 1);      // right
          ACanvas.DrawLine(r.Left, r.Top, r.Right, r.Top);              // top
        end;
      end;

    tpLeft:
      begin
        if AParams.IsSelected then
        begin
          r.Width := r.Width - 1;
          r.Height := r.Height + 2;
          ACanvas.SetColor(activeColor);
          ACanvas.FillRectangle(r.Left + 1, r.Top + 1, r.Width - 1, r.Height - 2);
          ACanvas.SetColor(FColors^[29]);
          ACanvas.DrawLine(r.Left, r.Bottom - 1, r.Left, r.Top + 1);    // left
          ACanvas.DrawLine(r.Left + 1, r.Top, r.Right - 1, r.Top);      // top
          ACanvas.DrawLine(r.Left + 1, r.Bottom - 1, r.Right - 1, r.Bottom - 1); // bottom
        end
        else
        begin
          ACanvas.SetColor(FColors^[28]);
          ACanvas.FillRectangle(r.Left + 1, r.Top + 1, r.Width - 2, r.Height - 2);
          ACanvas.SetColor(FColors^[29]);
          ACanvas.DrawLine(r.Left, r.Bottom - 1, r.Left, r.Top + 1);    // left
          ACanvas.DrawLine(r.Left + 1, r.Top, r.Right - 1, r.Top);      // top
          ACanvas.DrawLine(r.Left + 1, r.Bottom - 1, r.Right - 1, r.Bottom - 1); // bottom
          ACanvas.DrawLine(r.Right - 1, r.Top, r.Right - 1, r.Bottom);  // right
        end;
      end;

    tpRight:
      begin
        if AParams.IsSelected then
        begin
          r.Height := r.Height + 2;
          ACanvas.SetColor(activeColor);
          ACanvas.FillRectangle(r.Left, r.Top + 1, r.Width - 1, r.Height - 2);
          ACanvas.SetColor(FColors^[29]);
          ACanvas.DrawLine(r.Left + 1, r.Top, r.Right - 1, r.Top);      // top
          ACanvas.DrawLine(r.Right - 1, r.Top + 1, r.Right - 1, r.Bottom - 1); // right
          ACanvas.DrawLine(r.Left + 1, r.Bottom - 1, r.Right - 1, r.Bottom - 1); // bottom
        end
        else
        begin
          ACanvas.SetColor(FColors^[28]);
          ACanvas.FillRectangle(r.Left + 1, r.Top + 1, r.Width - 2, r.Height - 2);
          ACanvas.SetColor(FColors^[29]);
          ACanvas.DrawLine(r.Left + 1, r.Top, r.Right - 1, r.Top);      // top
          ACanvas.DrawLine(r.Right - 1, r.Top + 1, r.Right - 1, r.Bottom - 1); // right
          ACanvas.DrawLine(r.Left + 1, r.Bottom - 1, r.Right - 1, r.Bottom - 1); // bottom
          ACanvas.DrawLine(r.Left, r.Top, r.Left, r.Bottom);            // left
        end;
      end;
  end;  { case }
end;


initialization
  fpgStyleManager.RegisterClass('Fusion Light', TfpgFusionLightStyle);
  fpgStyleManager.RegisterClass('Fusion Dark', TfpgFusionDarkStyle);

end.
