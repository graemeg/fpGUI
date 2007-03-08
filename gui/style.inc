{
    fpGUI  -  Free Pascal GUI Library
    
    Style class declarations
    
    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{%mainunit fpgui.pp}

{
  Default style implementation
}

{$IFDEF read_interface}

  { Possible styles }
  TItemFlags = set of (ifFocused, ifSelected, ifDeactivated);

  { Possible arrow directions }
  TArrowDirection = (arrowUp, arrowDown, arrowLeft, arrowRight);

  { Abstract Style declarations }
  TStyleAbs = class(TObject)
  protected
    UIColorCache: array[0..$18] of TGfxColor;
  public
    constructor Create; virtual;
    // Colors
    procedure   UpdateUIColorCache;
    function    GetGUIColor(Color: TColor): TGfxColor; virtual; abstract;
    function    GetUIColor(AColor: TColor): TGfxColor;
    procedure   SetUIColor(Canvas: TFCanvas; Color: TColor);
    // General
    procedure   DrawText(Canvas: TFCanvas; const APosition: TPoint; const AText: String; State: TWidgetState); virtual; abstract;
    procedure   DrawItemBefore(Canvas: TFCanvas; const ARect: TRect; Flags: TItemFlags); virtual; abstract;
    procedure   DrawItemAfter(Canvas: TFCanvas; const ARect: TRect; Flags: TItemFlags); virtual; abstract;
    procedure   Draw3DFrame(Canvas: TFCanvas; const ARect: TRect; Color1, Color2, Color3, Color4: TColor); virtual; abstract;
    procedure   DrawFocusRect(Canvas: TFCanvas; const ARect: TRect); virtual; abstract;
    procedure   DrawSunkenOuterBorder(Canvas: TFCanvas; const ARect: TRect); virtual; abstract;
    procedure   DrawRaisedOuterBorder(Canvas: TFCanvas; const ARect: TRect); virtual; abstract;
    // Windows
    procedure   DrawWindowBackground(Canvas: TFCanvas; const ARect: TRect); virtual; abstract;
    // Buttons
    procedure   DrawButtonFace(Canvas: TFCanvas; const ARect: TRect; Flags: TButtonFlags); virtual; abstract;
    function    GetButtonBorders: TRect; virtual; abstract;
    // Separators
    function    GetSeparatorSize: Integer; virtual; abstract;
    procedure   DrawSeparator(Canvas: TFCanvas; const ARect: TRect; AOrientation: TOrientation); virtual; abstract;
    // Group boxes
    procedure   DrawGroupBox(Canvas: TFCanvas; const ARect: TRect; const ALabel: String; WidgetState: TWidgetState); virtual; abstract;
    function    GetGroupBoxBorders(Canvas: TFCanvas; const ALabel: String; var LabelWidth: Integer): TRect; virtual; abstract;
    // Edit widgets
    procedure   DrawEditBox(Canvas: TFCanvas; const ARect: TRect); virtual; abstract;
    function    GetEditBoxBorders: TRect; virtual; abstract;
    // Check boxes
    procedure   DrawCheckBox(Canvas: TFCanvas; const ARect, LabelRect: TRect; Flags: TCheckboxFlags); virtual; abstract;
    procedure   GetCheckBoxLayout(const LabelSize: TSize; var TotalSize: TSize; var LabelPos: TPoint); virtual; abstract;
    // Radio buttons
    procedure   DrawRadioButton(Canvas: TFCanvas; const ARect, LabelRect: TRect; Flags: TCheckboxFlags); virtual; abstract;
    procedure   GetRadioButtonLayout(const LabelSize: TSize; var TotalSize: TSize; var LabelPos: TPoint); virtual; abstract;
    // Combo boxes
    function    GetComboBoxArrowSize: TSize; virtual; abstract;
    procedure   DrawComboBoxArrow(Canvas: TFCanvas; const ARect: TRect; IsPressed, IsEnabled: Boolean); virtual; abstract;
    function    GetComboBoxBtnSize: TSize; virtual; abstract;
    // Scroll bars
    function    GetScrollBarBorders(Orientation: TOrientation): TRect; virtual; abstract;
    function    GetScrollBarBtnSize(Orientation: TOrientation): TSize; virtual; abstract;
    function    GetScrollBarBtnMinSize: Integer; virtual; abstract;
    procedure   DrawScrollBarBorder(Canvas: TFCanvas; const ARect: TRect); virtual; abstract;
    procedure   DrawScrollBarButton(Canvas: TFCanvas; const ARect: TRect; Direction: TArrowDirection; IsPressed, IsEnabled: Boolean); virtual; abstract;
    // Scroll boxes
    function    GetScrollBoxBorders: TRect; virtual; abstract;
    procedure   DrawScrollBoxBorder(Canvas: TFCanvas; const ARect: TRect); virtual; abstract;
    // Menus
    function    GetMenuBorders(pCanvas: TFCanvas; const pText: string; var pTextWidth: Integer): TRect; virtual; abstract;
    // Panel/Frame
    function    GetPanelBorders: TRect; virtual; abstract;
    procedure   DrawPanel(Canvas: TFCanvas; const ARect: TRect; ABevelStyle: TBevelStyle); virtual; abstract;
  end;


  { TDefaultStyle }

  TBasicStyle = class(TStyleAbs)
  protected
    procedure   DrawDirectionArrows(ACanvas: TFCanvas; const ARect: TRect; ADirection: TArrowDirection);
    procedure   DrawCheckBoxCheck(ACanvas: TFCanvas; const ARect: TRect; AFlags: TCheckboxFlags);
  public
    // Colors
    function    GetGUIColor(Color: TColor): TGfxColor; override;
    // General
    procedure   DrawText(Canvas: TFCanvas; const APosition: TPoint; const AText: String; State: TWidgetState); override;
    procedure   DrawItemBefore(Canvas: TFCanvas; const ARect: TRect; Flags: TItemFlags); override;
    procedure   DrawItemAfter(Canvas: TFCanvas; const ARect: TRect; Flags: TItemFlags); override;
    procedure   Draw3DFrame(Canvas: TFCanvas; const ARect: TRect; Color1, Color2, Color3, Color4: TColor); override;
    procedure   DrawFocusRect(Canvas: TFCanvas; const ARect: TRect); override;
    procedure   DrawSunkenOuterBorder(Canvas: TFCanvas; const ARect: TRect); override;
    procedure   DrawRaisedOuterBorder(Canvas: TFCanvas; const ARect: TRect); override;
    // Windows
    procedure   DrawWindowBackground(Canvas: TFCanvas; const ARect: TRect); override;
    // Buttons
    procedure   DrawButtonFace(Canvas: TFCanvas; const ARect: TRect; Flags: TButtonFlags); override;
    function    GetButtonBorders: TRect; override;
    // Separators
    procedure   DrawSeparator(Canvas: TFCanvas; const ARect: TRect; AOrientation: TOrientation); override;
    function    GetSeparatorSize: Integer; override;
    // Group boxes
    procedure   DrawGroupBox(Canvas: TFCanvas; const ARect: TRect; const ALabel: String; WidgetState: TWidgetState); override;
    function    GetGroupBoxBorders(Canvas: TFCanvas; const ALabel: String; var LabelWidth: Integer): TRect; override;
    // Edit widgets
    procedure   DrawEditBox(Canvas: TFCanvas; const ARect: TRect); override;
    function    GetEditBoxBorders: TRect; override;
    // Check boxes
    procedure   DrawCheckBox(Canvas: TFCanvas; const ARect, LabelRect: TRect; Flags: TCheckboxFlags); override;
    procedure   GetCheckBoxLayout(const LabelSize: TSize; var TotalSize: TSize; var LabelPos: TPoint); override;
    // Radio buttons
    procedure   DrawRadioButton(Canvas: TFCanvas; const ARect, LabelRect: TRect; Flags: TCheckboxFlags); override;
    procedure   GetRadioButtonLayout(const LabelSize: TSize; var TotalSize: TSize; var LabelPos: TPoint); override;
    // Combo boxes
    procedure   DrawComboBoxArrow(Canvas: TFCanvas; const ARect: TRect; IsPressed, IsEnabled: Boolean); override;
    function    GetComboBoxArrowSize: TSize; override;
    function    GetComboBoxBtnSize: TSize; override;
    // Scroll bars
    function    GetScrollBarBorders(Orientation: TOrientation): TRect; override;
    function    GetScrollBarBtnSize(Orientation: TOrientation): TSize; override;
    function    GetScrollBarBtnMinSize: Integer; override;
    procedure   DrawScrollBarBorder(Canvas: TFCanvas; const ARect: TRect); override;
    procedure   DrawScrollBarButton(Canvas: TFCanvas; const ARect: TRect; Direction: TArrowDirection; IsPressed, IsEnabled: Boolean); override;
    // Scroll boxes
    function    GetScrollBoxBorders: TRect; override;
    procedure   DrawScrollBoxBorder(Canvas: TFCanvas; const ARect: TRect); override;
    // Menus
    function    GetMenuBorders(pCanvas: TFCanvas; const pText: string; var pTextWidth: Integer): TRect; override;
    // Panel/Frame
    function    GetPanelBorders: TRect; override;
    procedure   DrawPanel(Canvas: TFCanvas; const ARect: TRect; ABevelStyle: TBevelStyle); override;
  end;
  
  
{$ENDIF read_interface}



{$IFDEF read_implementation}


{ TStyleAbs }

constructor TStyleAbs.Create;
begin
  inherited Create;
  UpdateUIColorCache;
end;


procedure TStyleAbs.UpdateUIColorCache;
var
  i: TColor;
begin
  for i := 0 to $18 do
    UIColorCache[i] := GetGUIColor(TColor(i + clScrollBar));
end;


function TStyleAbs.GetUIColor(AColor: TColor): TGfxColor;
begin
  if (AColor >= clScrollBar) and (AColor <= clScrollBar + $18) then
    Result := UIColorCache[TColor(AColor - clScrollBar)]
  else
  begin
//    Result := colBlack;
    Result.Red    := (AColor and $ff) * 257;
    Result.Green  := ((AColor shr 8) and $ff) * 257;
    Result.Blue   := ((AColor shr 16) and $ff) * 257;
    Result.Alpha  := 0;
  end;
end;


procedure TStyleAbs.SetUIColor(Canvas: TFCanvas; Color: TColor);
var
  lGfxColor: TGfxColor;
begin
  if (Color >= clScrollBar) and (Color <= clScrollBar + $18) then
    Canvas.SetColor(UIColorCache[TColor(Color - clScrollBar)])
  else
  begin
    lGfxColor.Red     := (Color and $ff) * 257;
    lGfxColor.Green   := ((Color shr 8) and $ff) * 257;
    lGfxColor.Blue    := ((Color shr 16) and $ff) * 257;
    lGfxColor.Alpha   := 0;
    Canvas.SetColor(lGfxColor);
  end;
end;


{ TBasicStyle }

const
  rgbaDkWhite:    TGfxColor = (Red: $e000; Green: $e000; Blue: $e000; Alpha: 0);


{ This gets platform specific colors and focus rectangle styles }
{$IFDEF MSWINDOWS}
  {$I win32/defstyle.inc}
{$ENDIF}
{$IFDEF UNIX}
  {$I defimpl/defstyle.inc}
{$ENDIF}

procedure TBasicStyle.DrawDirectionArrows(ACanvas: TFCanvas;
  const ARect: TRect; ADirection: TArrowDirection);
var
  peekx, peeky: Cardinal;
  basex, basey: Cardinal;
  side, margin: Cardinal;
begin
  side := (ARect.Right div 4) + 1;
  margin := side;

  if ADirection in [arrowUp, arrowDown] then
  begin
    peekx := ARect.Left+(ARect.Right div 2);
    if ADirection = arrowDown then
    begin
      peeky := ARect.Top + ARect.Bottom - margin;
      basey := peeky-side;
    end
    else
    begin  // up
      peeky := ARect.Top+margin;
      basey := peeky+side;
    end;
    LAYOUTTRACE('Triangle Points (%d,%d)(%d,%d)(%d,%d)',
      [peekx, peeky, peekx+side, basey, peekx-side, basey]);

    ACanvas.FillTriangle(Point(peekx, peeky), Point(peekx+side, basey), Point(peekx-side, basey));
  end
  else // horizontal
  begin
    peeky := ARect.Top + (ARect.Bottom div 2);
    if ADirection = arrowRight then
    begin
      peekx := ARect.Left + ARect.Right - margin;
      basex := peekx - side;
    end
    else    // left
    begin
      peekx := ARect.Left + margin;
      basex := peekx + side;
    end;
    LAYOUTTRACE('Triangle Points (%d,%d)(%d,%d)(%d,%d)',
      [peekx, peeky, basex, peeky-side, basex, peeky+side]);
    ACanvas.FillTriangle(Point(peekx, peeky), Point(basex, peeky-side), Point(basex, peeky+side));
  end;
end;

procedure TBasicStyle.DrawCheckBoxCheck(ACanvas: TFCanvas;
  const ARect: TRect; AFlags: TCheckboxFlags);
begin
  if (cbIsEnabled in AFlags) then
    SetUIColor(ACanvas, clBtnText)
  else
    SetUIColor(ACanvas, cl3DShadow);

   {$Note As soon as canvas supports a Pen, we need to set the PenWidth}
  if (cbIsChecked in AFlags) then
  begin
    ACanvas.DrawLine(Point(ARect.Left+1, ARect.Top+1), Point(ARect.Right, ARect.Bottom));
    ACanvas.DrawLine(Point(ARect.Left+2, ARect.Top+1), Point(ARect.Right, ARect.Bottom-1));
    ACanvas.DrawLine(Point(ARect.Left+1, ARect.Top+2), Point(ARect.Right-1, ARect.Bottom));

    ACanvas.DrawLine(Point(ARect.Right-1, ARect.Top+1), Point(ARect.Left, ARect.Bottom));
    ACanvas.DrawLine(Point(ARect.Right-2, ARect.Top+1), Point(ARect.Left, ARect.Bottom-1));
    ACanvas.DrawLine(Point(ARect.Right-1, ARect.Top+2), Point(ARect.Left+1, ARect.Bottom));
  end;
end;

// helper functions


{ Draws a 3D frame, its thickness is 2 pixels. The 4 given colors are used in
  this way:
  Color1: Outer frame left & top
  Color2: Inner frame left & top
  Color3: Outer frame right & bottom
  Color4: Inner frame right & bottom
}

procedure TBasicStyle.Draw3DFrame(Canvas: TFCanvas; const ARect: TRect;
  Color1, Color2, Color3, Color4: TColor);
begin
  with ARect do
  begin
    SetUIColor(Canvas, Color1);
    {Canvas.DrawPolyLine([Point(Left, Bottom - 2), TopLeft,
      Point(Right - 1, Top)]);}
    Canvas.DrawLine(Point(Left, Bottom - 2), TopLeft);
    Canvas.DrawLine(TopLeft, Point(Right - 1, Top));
    SetUIColor(Canvas, Color2);
    {Canvas.DrawPolyLine([Point(Left + 1, Bottom - 3), Point(Left + 1, Top + 1),
      Point(Right - 2, Top + 1)]);}
    Canvas.DrawLine(Point(Left + 1, Bottom - 3), Point(Left + 1, Top + 1));
    Canvas.DrawLine(Point(Left + 1, Top + 1), Point(Right - 2, Top + 1));
    SetUIColor(Canvas, Color3);
    {Canvas.DrawPolyLine([Point(Left, Bottom - 1), Point(Right - 1, Bottom - 1),
      Point(Right - 1, Top - 1)]);}
    Canvas.DrawLine(Point(Left, Bottom - 1), Point(Right - 1, Bottom - 1));
    Canvas.DrawLine(Point(Right - 1, Bottom - 1), Point(Right - 1, Top - 1));
    SetUIColor(Canvas, Color4);
    {Canvas.DrawPolyLine([Point(Left + 1, Bottom - 2),
      Point(Right - 2, Bottom - 2), Point(Right - 2, Top)]);}
    Canvas.DrawLine(Point(Left + 1, Bottom - 2), Point(Right - 2, Bottom - 2));
    Canvas.DrawLine(Point(Right - 2, Bottom - 2), Point(Right - 2, Top));
  end;
end;

procedure TBasicStyle.DrawSunkenOuterBorder(Canvas: TFCanvas;
  const ARect: TRect);
begin
  with ARect do
  begin
    SetUIColor(Canvas, cl3DShadow);
    Canvas.DrawLine(Point(Left, Bottom - 2), TopLeft);
    Canvas.DrawLine(TopLeft, Point(Right, Top));
    SetUIColor(Canvas, cl3DHighlight);
    Canvas.DrawLine(Point(Left, Bottom - 1), Point(Right - 1, Bottom - 1));
    Canvas.DrawLine(Point(Right - 1, Bottom - 1), Point(Right - 1, Top - 1));
  end;
end;

procedure TBasicStyle.DrawRaisedOuterBorder(Canvas: TFCanvas;
  const ARect: TRect);
begin
  with ARect do
  begin
    SetUIColor(Canvas, cl3DHighlight);
    Canvas.DrawLine(Point(Left, Bottom - 2), TopLeft);
    Canvas.DrawLine(TopLeft, Point(Right, Top));
    SetUIColor(Canvas, cl3DShadow);
    Canvas.DrawLine(Point(Left, Bottom - 1), Point(Right - 1, Bottom - 1));
    Canvas.DrawLine(Point(Right - 1, Bottom - 1), Point(Right - 1, Top - 1));
  end;
end;

procedure TBasicStyle.DrawText(Canvas: TFCanvas; const APosition: TPoint;
  const AText: String; State: TWidgetState);
begin
  if not (wsEnabled in State) then
  begin
    SetUIColor(Canvas, cl3DHighlight);
    Canvas.TextOut(APosition + Point(1, 1), AText);
    SetUIColor(Canvas, cl3DShadow);
  end;
  Canvas.TextOut(APosition, AText);
end;

procedure TBasicStyle.DrawItemBefore(Canvas: TFCanvas; const ARect: TRect;
  Flags: TItemFlags);
begin
  if ifSelected in Flags then
  begin
    SetUIColor(Canvas, clHighlight);
    Canvas.FillRect(ARect);
    SetUIColor(Canvas, clHighlightText);
  end;
end;

procedure TBasicStyle.DrawItemAfter(Canvas: TFCanvas; const ARect: TRect;
  Flags: TItemFlags);
begin
  if ifFocused in Flags then
    DrawFocusRect(Canvas, ARect);
end;

procedure TBasicStyle.DrawWindowBackground(Canvas: TFCanvas;
  const ARect: TRect);
begin
//  SetUIColor(Canvas, cl3DFace);
  Canvas.FillRect(ARect);
end;

procedure TBasicStyle.DrawButtonFace(Canvas: TFCanvas; const ARect: TRect;
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

function TBasicStyle.GetButtonBorders: TRect;
begin
  Result := Rect(5, 5, 5, 5);
end;

function TBasicStyle.GetSeparatorSize: Integer;
begin
  Result := 2;
end;

procedure TBasicStyle.DrawSeparator(Canvas: TFCanvas; const ARect: TRect;
  AOrientation: TOrientation);
var
  r: TRect;
begin
  with ARect do
    if AOrientation = Horizontal then
      r := Rect(Left, Top + (Bottom - Top) div 2 - 1, Right,
        Top + (Bottom - Top) div 2 + 1)
    else
      r := Rect(Left + (Right - Left) div 2 - 1, Top,
        Left + (Right - Left) div 2 + 1, Bottom);
  DrawSunkenOuterBorder(Canvas, r);
end;

procedure TBasicStyle.DrawGroupBox(Canvas: TFCanvas; const ARect: TRect;
  const ALabel: String; WidgetState: TWidgetState);
var
  TitleWidth, TitleHeight, TopLine: Integer;
begin
  TitleWidth  := Canvas.TextWidth(ALabel);
  TitleHeight := Canvas.FontCellHeight;
  TopLine     := ARect.Top + TitleHeight div 3;

  SetUIColor(Canvas, cl3DHighlight);
  with ARect do
  begin
    Canvas.DrawLine(Point(Left + TitleWidth + 10, TopLine + 1), Point(Right - 1, TopLine + 1));     { top - right of text }
    Canvas.DrawLine(Point(Right - 1, TopLine + 1), Point(Right - 1, Bottom - 1));                   { right }
    Canvas.DrawLine(Point(Right - 1, Bottom - 1), Point(Left + 1, Bottom - 1));                     { bottom }
    Canvas.DrawLine(Point(Left + 1, Bottom - 1), Point(Left + 1, TopLine + 1));                     { left }
    Canvas.DrawLine(Point(Left + 1, TopLine + 1), Point(Left + 8, TopLine + 1));                    { top - left of text }
  end;

  SetUIColor(Canvas, cl3DShadow);
  with ARect do
  begin
    Canvas.DrawLine(Point(Left + TitleWidth + 10, TopLine), Point(Right - 2, TopLine));
    Canvas.DrawLine(Point(Right - 2, TopLine), Point(Right - 2, Bottom - 2));
    Canvas.DrawLine(Point(Right - 2, Bottom - 2), Point(Left, Bottom - 2));
    Canvas.DrawLine(Point(Left, Bottom - 2), Point(Left, TopLine));
    Canvas.DrawLine(Point(Left, TopLine), Point(Left + 8, TopLine));
  end;

  SetUIColor(Canvas, clWindowText);
  if ALabel <> '' then
    DrawText(Canvas, ARect.TopLeft + Point(9, 0), ALabel, WidgetState);
end;

function TBasicStyle.GetGroupBoxBorders(Canvas: TFCanvas;
  const ALabel: String; var LabelWidth: Integer): TRect;
begin
  Result := Rect(6, Canvas.FontCellHeight + 4, 6, 6);
  LabelWidth := Canvas.TextWidth(ALabel) + 6;
end;

procedure TBasicStyle.DrawEditBox(Canvas: TFCanvas; const ARect: TRect);
begin
  Draw3DFrame(Canvas, ARect, cl3DShadow, cl3DDkShadow, cl3DHighlight, cl3DFace);
  SetUIColor(Canvas, clWindow);
  with ARect do
    Canvas.FillRect(Rect(Left + 2, Top + 2, Right - 2, Bottom - 2));
end;

function TBasicStyle.GetEditBoxBorders: TRect;
begin
  Result := Rect(2, 2, 2, 2);
end;

procedure TBasicStyle.DrawCheckBox(Canvas: TFCanvas;
  const ARect, LabelRect: TRect; Flags: TCheckboxFlags);
var
  r: TRect;
begin
  SetUIColor(Canvas, cl3DFace);
  Canvas.FillRect(ARect);

  r.Left    := ARect.Left;
  r.Top     := ARect.Top + (ARect.Bottom - ARect.Top - 13) div 2;
  r.Right   := 13;
  r.Bottom  := r.Top + 13;
  DrawEditBox(Canvas, r);

  inc(r.Left,2);
  inc(r.Top,2);
  dec(r.Right,3);
  dec(r.Bottom,3);
  DrawCheckBoxCheck(Canvas, r, Flags);
  
  if cbHasFocus in Flags then
    with LabelRect do
      DrawFocusRect(Canvas, Rect(Left - 2, Top - 2, Right + 2, Bottom + 2));
end;

procedure TBasicStyle.GetCheckBoxLayout(const LabelSize: TSize;
  var TotalSize: TSize; var LabelPos: TPoint);
begin
  TotalSize := Size(LabelSize.cx + 21, Max(13, LabelSize.cy + 4));
  LabelPos := Point(19, (TotalSize.cy - LabelSize.cy) div 2);
end;


procedure TBasicStyle.DrawRadioButton(Canvas: TFCanvas;
  const ARect, LabelRect: TRect; Flags: TCheckboxFlags);
var
  Index, BtnY: Integer;
  r: TRect;
begin
{
  with ARect do
  begin
    BtnY := Top + (Bottom - Top - 12) div 2;
    if cbIsEnabled in Flags then
    begin
      Index := Ord(cbIsChecked in Flags);
      if cbIsPressed in Flags then
        Inc(Index, 2);
    end else
      Index := 2 + Ord(cbIsChecked in Flags) * 2;
    Canvas.MaskedCopyRect(ImageCanvas, MaskCanvas,
      Rect(Index * 12, 0, (Index + 1) * 12, 12),
      Point(0, 0), Point(Left, BtnY));
  end;
}
// cl3DLight, cl3DHighlight, cl3DDkShadow, cl3DShadow
  SetUIColor(Canvas, clLtGray);
  r.Left    := ARect.Left;
  r.Top     := ARect.Top + (ARect.Bottom - ARect.Top - 13) div 2;
  r.Right   := 13;
  r.Bottom  := r.Top + 13;
  Canvas.DrawCircle(r);
  
  SetUIColor(Canvas, cl3DShadow);
  InflateRect(r, -2, -2);
  Canvas.DrawCircle(r);

  SetUIColor(Canvas, cl3DDkShadow);
  InflateRect(r, 1, 1);
  Canvas.DrawCircle(r);
  
  if cbIsChecked in Flags then
  begin
    SetUIColor(Canvas, clNavy);
    InflateRect(r, -3, -3);
    Canvas.DrawCircle(r);
    InflateRect(r, -1, -1);
    Canvas.FillRect(r);
    InflateRect(r, 1, 1);
    SetUIColor(Canvas, clSkyBlue);
    InflateRect(r, -2, -2);
    OffsetRect(r, -1, -1);
    Canvas.FillRect(r);
  end;

  if cbHasFocus in Flags then
    with LabelRect do
      DrawFocusRect(Canvas, Rect(Left - 2, Top - 2, Right + 2, Bottom + 2));
end;

procedure TBasicStyle.GetRadioButtonLayout(const LabelSize: TSize;
  var TotalSize: TSize; var LabelPos: TPoint);
begin
  TotalSize := Size(LabelSize.cx + 20, Max(12, LabelSize.cy + 4));
  LabelPos := Point(18, (TotalSize.cy - LabelSize.cy) div 2);
end;

function TBasicStyle.GetComboBoxArrowSize: TSize;
begin
  Result.cx := 17;
  Result.cy := 17;
end;

function TBasicStyle.GetComboBoxBtnSize: TSize;
begin
  Result.cx := 18;
  Result.cy := 18;
end;

procedure TBasicStyle.DrawComboBoxArrow(Canvas: TFCanvas;
  const ARect: TRect; IsPressed, IsEnabled: Boolean);
var
  r: TRect;
begin
  r := ARect;
  if IsEnabled then
  begin
    Canvas.SetColor(GetGUIColor(clBtnText));
    if IsPressed then
    begin
      r.Top   := ARect.Top + 1;
      r.Left  := ARect.Left + 1;
    end;
  end
  else
    Canvas.SetColor(GetGUIColor(cl3DShadow));

  DrawDirectionArrows(Canvas, r, arrowDown);
end;

function TBasicStyle.GetScrollBarBorders(Orientation: TOrientation): TRect;
begin
  Result := Rect(1, 1, 1, 1);
end;

function TBasicStyle.GetScrollBarBtnSize(Orientation: TOrientation): TSize;
begin
  Result.cx := 16;
  Result.cy := 16;
end;

function TBasicStyle.GetScrollBarBtnMinSize: Integer;
begin
  Result := 30;
end;

procedure TBasicStyle.DrawScrollBarBorder(Canvas: TFCanvas;
  const ARect: TRect);
begin
  with ARect do
  begin
    SetUIColor(Canvas, cl3DShadow);
    Canvas.DrawLine(Point(Left, Bottom - 1), TopLeft);
    Canvas.DrawLine(TopLeft, Point(Right, Top));

    SetUIColor(Canvas, cl3DHighlight);
    Canvas.DrawLine(Point(Left + 1, Bottom - 1), Point(Right - 1, Bottom - 1));
    Canvas.DrawLine(Point(Right - 1, Bottom - 1), Point(Right - 1, Top));

    SetUIColor(Canvas, cl3DFace);
    Canvas.FillRect(Rect(Left, Bottom - 1, Left + 1, Bottom));
    Canvas.FillRect(Rect(Right - 1, Top, Right, Top + 1));
  end;
end;

procedure TBasicStyle.DrawScrollBarButton(Canvas: TFCanvas;
  const ARect: TRect; Direction: TArrowDirection;
  IsPressed, IsEnabled: Boolean);
var
  r: TRect;
begin
  r := ARect;
  if IsEnabled then
  begin
    SetUIColor(Canvas, clBtnText);
    if IsPressed then
    begin
      r.Top   := ARect.Top + 1;
      r.Left  := ARect.Left + 1;
    end;
  end
  else
    SetUIColor(Canvas, cl3DShadow);

  DrawDirectionArrows(Canvas, r, Direction);
end;

function TBasicStyle.GetScrollBoxBorders: TRect;
begin
  Result := Rect(2, 2, 2, 2);
end;

procedure TBasicStyle.DrawScrollBoxBorder(Canvas: TFCanvas; const ARect: TRect);
begin
  Draw3DFrame(Canvas, ARect, cl3DShadow, cl3DDkShadow,
    cl3DHighlight, cl3DLight);
end;

function TBasicStyle.GetMenuBorders(pCanvas: TFCanvas; const pText: string;
    var pTextWidth: Integer): TRect;
begin
  pTextWidth := pCanvas.TextWidth(pText) + 6;
//  Result := Rect(2, 2, 6, pCanvas.FontCellHeight+4);
  Result := Rect(6, 2, 2, 2);
//  Result := Rect(6, pCanvas.FontCellHeight + 4, 6, 6);
end;

function TBasicStyle.GetPanelBorders: TRect;
begin
  Result := Rect(5, 5, 5, 5);
end;

procedure TBasicStyle.DrawPanel(Canvas: TFCanvas; const ARect: TRect;
  ABevelStyle: TBevelStyle);
begin
  // bsPlain, bsLowered, bsRaised
  if ABevelStyle = bsPlain then
    DrawWindowBackground(Canvas, ARect)
  else if ABevelStyle = bsLowered then
    DrawSunkenOuterBorder(Canvas, ARect)
  else
    DrawRaisedOuterBorder(Canvas, ARect);
//    Draw3DFrame(Canvas, ARect, cl3DHighlight, cl3DFace, cl3DFace, cl3DShadow);
end;



{$ENDIF read_implementation}

