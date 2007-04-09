unit OpenSoftStyle;

{$mode objfpc}{$H+}

interface

uses
  Classes, fpgui, gfxbase;
  
type

  TGradientDirection  = (gdTopToBottom, gdBottomToTop, gdLeftToRight, gdRightToLeft);
  TCalcGradientEndX   = function(Y, H: Integer): Integer;
  

  TOpenSoftStyle = class(TDefaultStyle)
  private
    procedure   PaintGradient(pCanvas: TGfxCanvas; const R: TRect; StartColor, EndColor: TColor; Direction: TGradientDirection; GradLines: Integer = -1);
  public
    // Colors
    function    GetGUIColor(Color: TColor): TGfxColor; override;
    // Buttons (todo)
    procedure   DrawButtonFace(Canvas: TGfxCanvas; const ARect: TRect; Flags: TFButtonFlags); override;
    // GroupBox
    procedure   DrawGroupBox(Canvas: TGfxCanvas; const ARect: TRect; const ALabel: String; WidgetState: TWidgetState); override;
  end;
  

var
  FOpenSoftStyle: TOpenSoftStyle;
  
implementation


const
  // Some predefined colors:
  rgbaDkBlue:     TGfxColor = (Red: $0000; Green: $0000; Blue: $8000; Alpha: $0000);
  rgbaLtYellow:   TGfxColor = (Red: $ffff; Green: $ffff; Blue: $e100; Alpha: $0000);

  rgbaWindowText: TGfxColor = (Red: $0000; Green: $0000; Blue: $0000; Alpha: $0000);
  rgbaWindow:     TGfxColor	= (Red: $efef; Green: $efef; Blue: $efef; Alpha: $0000);
  rgbaDkGrey:     TGfxColor	= (Red: $8686; Green: $8686; Blue: $8686; Alpha: $0000);
  rgbaGbAALtGrey: TGfxColor	= (Red: $baba; Green: $baba; Blue: $baba; Alpha: $0000);
  rgbaGbAADkGrey: TGfxColor	= (Red: $7878; Green: $7878; Blue: $7878; Alpha: $0000);


{
procedure DrawGradient(Canvas: TCanvas; const R: TRect; StartColor, EndColor: TColor;
  Direction: TGradientDirection; GradLines: Integer = -1; CalcEndX: TCalcGradientEndX = nil);
procedure DrawGradientEx(Canvas: TCanvas; const R: TRect; StartColor: TColor;
  StartToMidHeight: Integer; MidColor, EndColor: TColor;
  Direction: TGradientDirection; CalcEndX: TCalcGradientEndX = nil);


procedure ToRGB(c: TColor; out rgb: TRGB);
var
  l: TColorRef;
begin
  c := ColorFromColormap(c);
  l := ColorToRGB(c);
  rgb.r := TRGBValue(l).r;
  rgb.g := TRGBValue(l).g;
  rgb.b := TRGBValue(l).b;
end;
}


{ TOpenSoftStyle }

procedure TOpenSoftStyle.PaintGradient(pCanvas: TGfxCanvas; const R: TRect; StartColor, EndColor: TColor; Direction: TGradientDirection; GradLines: Integer = -1);
var
  X, i, w, h, Count: Integer;
  EndCol, StartCol, AddCol, Tmp: TGfxColor;
begin
  w := R.Right - R.Left - 1;
  h := R.Bottom - R.Top - 1;
  if (w <= 0) or (h <= 0) then
    Exit; //==>

  StartCol  := GetGUIColor(StartColor);
  EndCol    := GetGUIColor(EndColor);

  case Direction of
    gdTopToBottom:
      Count       := h;
    gdLeftToRight:
      Count       := w;
    gdBottomToTop:
      begin
        Count     := h;
        Tmp       := EndCol;
        EndCol    := StartCol;
        StartCol  := Tmp;
      end;
    gdRightToLeft:
      begin
        Count     := w;
        Tmp       := EndCol;
        EndCol    := StartCol;
        StartCol  := Tmp;
      end;
  else
    Exit; //==>
  end;
  if GradLines < 0 then
    GradLines := Count;

  AddCol.r := (EndCol.r - StartCol.r) / GradLines;
  AddCol.g := (EndCol.g - StartCol.g) / GradLines;
  AddCol.b := (EndCol.b - StartCol.b) / GradLines;

//  Canvas.Pen.Style := psSolid;
  pCanvas.SaveState;
//  Canvas.Start;
  try
    StartColor := TColor(Round(StartCol.r), Round(StartCol.g), Round(StartCol.b));
//    Canvas.Pen.Color := StartColor;
    pCanvas.SetColor(GetGUIColor(StartColor));
    for i := 0 to Count - 1 do
    begin
      if Direction in [gdTopToBottom, gdBottomToTop] then
      begin
        Canvas.MoveTo(R.Left, R.Top + i);
        if Assigned(CalcEndX) then
          X := CalcEndX(i, Count)
        else
          X := 0;
        Canvas.LineTo(R.Right + X, R.Top + i);
      end
      else
      begin
        Canvas.MoveTo(R.Left + i, R.Top);
        Canvas.LineTo(R.Left + i, R.Bottom);
      end;
      StartCol.r := StartCol.r + AddCol.r;
      StartCol.g := StartCol.g + AddCol.g;
      StartCol.b := StartCol.b + AddCol.b;
      EndColor := RGB(Round(StartCol.r), Round(StartCol.g), Round(StartCol.b));
      if StartColor <> EndColor then
      begin
//        Canvas.Pen.Color := EndColor;
        pCanvas.SetColor(GetGUIColor(EndColor));
        StartColor := EndColor;
      end;
    end; // for
  finally
//    Canvas.Stop;
    pCanvas.RestoreState;
  end;
end;


function TOpenSoftStyle.GetGUIColor(Color: TColor): TGfxColor;
begin
  Result := inherited GetGUIColor(Color);
  case Color of
    // UI element colors
    clScrollBar:      Result := rgbaWindow;
    clMenu:           Result := rgbaWindow;
//    clWindow:         Result := GetUIColor(clWhite);
//    clMenuText:       Result := GetUIColor(clBlack);
//    clWindowText:     Result := GetUIColor(clBlack);
//    clAppWorkSpace:   Result := GetUIColor(clGray);
//    clHighlight:      Result := GetUIColor(clNavy);
//    clHighlightText:  Result := GetUIColor(clWhite);
    cl3DFace:         Result := rgbaWindow;
//    cl3DShadow:       Result := rgbaDkWhite;
//    clGrayText:       Result := GetUIColor(clGray);
//    clBtnText:        Result := GetUIColor(clBlack);
//    cl3DHighlight:    Result := GetUIColor(clWhite);
    cl3DDkShadow:     Result := GetUIColor(clMidnightBlue);
//    cl3DLight:        Result := GetUIColor(clDarkWhite);
//    clInfoText:       Result := GetUIColor(clBlack);
//    clInfoBk:         Result := GetUIColor(clLightYellow);
//
//    else              Result := GetUIColor(clWhite);
  end;

end;


procedure TOpenSoftStyle.DrawButtonFace(Canvas: TGfxCanvas; const ARect: TRect;
  Flags: TFButtonFlags);
begin
//  inherited DrawButtonFace(Canvas, ARect, Flags);
  PaintGradient(Canvas, ARect, Flags);
  Draw3DFrame(Canvas, ARect, cl3DHighlight, cl3DLight, cl3DDkShadow, cl3DShadow);
end;


procedure TOpenSoftStyle.DrawGroupBox(Canvas: TGfxCanvas; const ARect: TRect;
    const ALabel: String; WidgetState: TWidgetState);
var
  TitleWidth, TitleHeight, TopLine: Integer;
begin
  TitleWidth  := Canvas.TextWidth(ALabel);
  TitleHeight := Canvas.FontCellHeight;
  TopLine     := ARect.Top + TitleHeight div 3;

  Canvas.SetColor(rgbaDkGrey);
  // box outline
  with ARect do
  begin
    // top
    Canvas.DrawLine(Point(Left + 2, TopLine), Point(Left + 12, TopLine));
    Canvas.DrawLine(Point(Left + TitleWidth + 16, TopLine), Point(Right - 2, TopLine));
    // right
    Canvas.DrawLine(Point(Right-1, TopLine + 2), Point(Right-1, Bottom - 2));
    // bottom
    Canvas.DrawLine(Point(Right - 3, Bottom-1), Point(Left + 1, Bottom-1));
    // left
    Canvas.DrawLine(Point(Left, Bottom - 3), Point(Left, TopLine + 1));
  end;

  // Text caption
  SetUIColor(Canvas, clWindowText);
  DrawText(Canvas, ARect.TopLeft + Point(14, 0), ALabel, WidgetState);

  { Anti-Aliasing - Top/Left }
  Canvas.SetColor(rgbaGbAALtGrey);
  Canvas.DrawPoint(ARect.TopLeft + Point(0, TopLine+1));
  Canvas.DrawPoint(ARect.TopLeft + Point(1, TopLine));
  Canvas.SetColor(rgbaGbAADkGrey);
  Canvas.DrawPoint(ARect.TopLeft + Point(1, TopLine+1));
  { Anti-Aliasing - Top/Right }
  Canvas.SetColor(rgbaGbAALtGrey);
  Canvas.DrawPoint(ARect.TopLeft + Point(ARect.Right-1, TopLine+1));
  Canvas.DrawPoint(ARect.TopLeft + Point(ARect.Right-2, TopLine));
  Canvas.SetColor(rgbaGbAADkGrey);
  Canvas.DrawPoint(ARect.TopLeft + Point(ARect.Right-2, TopLine+1));
  { Anti-Aliasing - Bottom/Right }
  Canvas.SetColor(rgbaGbAALtGrey);
  Canvas.DrawPoint(ARect.TopLeft + Point(ARect.Right-1, ARect.Bottom-2));
  Canvas.DrawPoint(ARect.TopLeft + Point(ARect.Right-2, ARect.Bottom-1));
  Canvas.SetColor(rgbaGbAADkGrey);
  Canvas.DrawPoint(ARect.TopLeft + Point(ARect.Right-2, ARect.Bottom-2));
  { Anti-Aliasing - Bottom/Left }
  Canvas.SetColor(rgbaGbAALtGrey);
  Canvas.DrawPoint(ARect.TopLeft + Point(0, ARect.Bottom-2));
  Canvas.DrawPoint(ARect.TopLeft + Point(1, ARect.Bottom-1));
  Canvas.SetColor(rgbaGbAADkGrey);
  Canvas.DrawPoint(ARect.TopLeft + Point(1, ARect.Bottom-2));
end;


initialization
  FOpenSoftStyle := TOpenSoftStyle.Create(Application.Display);
  
finalization
  if Assigned(FOpenSoftStyle) then
    FOpenSoftStyle.Free;
  
end.

