{
    fpGUI  -  Free Pascal GUI Library

    OpenSoft look-and-feel style implementation

    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit OpenSoftStyle;

{$mode objfpc}{$H+}

interface
uses
  Classes
  ,fpGUI
  ,gfxBase
  ,fpGFX
  ;
  
type

  TGradientDirection  = (gdTopToBottom, gdBottomToTop, gdLeftToRight, gdRightToLeft);
  TCalcGradientEndX   = function(Y, H: Integer): Integer;
  

  TOpenSoftStyle = class(TBasicStyle)
  private
    procedure   PaintGradient(pCanvas: TFCanvas; const R: TRect; StartColor, EndColor: TColor; Direction: TGradientDirection; GradLines: Integer = -1);
  public
    // Colors
    function    GetGUIColor(Color: TColor): TGfxColor; override;
    // Buttons (todo)
//    procedure   DrawButtonFace(Canvas: TFCanvas; const ARect: TRect; Flags: TButtonFlags); override;
    // GroupBox
    procedure   DrawGroupBox(Canvas: TFCanvas; const ARect: TRect; const ALabel: String; WidgetState: TWidgetState); override;
  end;
  

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

procedure TOpenSoftStyle.PaintGradient(pCanvas: TFCanvas; const R: TRect;
    StartColor, EndColor: TColor; Direction: TGradientDirection;
    GradLines: Integer = -1);
var
  X: integer;
  i: integer;
  w: integer;
  h: integer;
  Count: integer;
  EndCol: TGfxColor;
  StartCol: TGfxColor;
  AddCol: TGfxColor;
  Tmp: TGfxColor;
begin
(*
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

  AddCol.Red    := (EndCol.Red - StartCol.Red) div GradLines;
  AddCol.Green  := (EndCol.Green - StartCol.Green) div GradLines;
  AddCol.Blue   := (EndCol.Blue - StartCol.Blue) div GradLines;

//  Canvas.Pen.Style := psSolid;
  pCanvas.SaveState;
//  Canvas.Start;
  try
//    StartColor := TColor(Round(StartCol.Red), Round(StartCol.Green), Round(StartCol.Blue));
//    Canvas.Pen.Color := StartColor;
    pCanvas.SetColor(GetGUIColor(StartColor));
    for i := 0 to Count - 1 do
    begin
      if Direction in [gdTopToBottom, gdBottomToTop] then
      begin
//        pCanvas.MoveTo(R.Left, R.Top + i);
//        if Assigned(CalcEndX) then
//          X := CalcEndX(i, Count)
//        else
          X := 0;
//        pCanvas.LineTo(R.Right + X, R.Top + i);
        pCanvas.DrawLine(Point(R.Left, R.Top + i), Point(R.Right + X, R.Top + i));
      end
      else
      begin
        pCanvas.DrawLine(Point(R.Left + i, R.Top), Point(R.Left + i, R.Bottom))
//        pCanvas.MoveTo(R.Left + i, R.Top);
//        pCanvas.LineTo(R.Left + i, R.Bottom);
      end;
      StartCol.Red    := StartCol.Red + AddCol.Red;
      StartCol.Green  := StartCol.Green + AddCol.Green;
      StartCol.Blue   := StartCol.Blue + AddCol.Blue;
      EndColor        := RGB(Round(StartCol.Red), Round(StartCol.Green), Round(StartCol.Blue));
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
*)
end;


function TOpenSoftStyle.GetGUIColor(Color: TColor): TGfxColor;
begin
  Result := inherited GetGUIColor(Color);
  case Color of
    // UI element colors
    clScrollBar:      Result := GetUIColor(clLightGrey); //rgbaWindow;
    clMenu:           Result := GetUIColor(clLightGrey); //rgbaWindow;
//    clWindow:         Result := GetUIColor(clWhite);
//    clMenuText:       Result := GetUIColor(clBlack);
//    clWindowText:     Result := GetUIColor(clBlack);
//    clAppWorkSpace:   Result := GetUIColor(clGray);
//    clHighlight:      Result := GetUIColor(clNavy);
//    clHighlightText:  Result := GetUIColor(clWhite);
    cl3DFace:         Result := GetUIColor(clLightGrey); //rgbaWindow;
//    cl3DShadow:       Result := rgbaDkWhite;
//    clGrayText:       Result := GetUIColor(clGray);
//    clBtnText:        Result := GetUIColor(clBlack);
//    cl3DHighlight:    Result := GetUIColor(clWhite);
    cl3DDkShadow:     Result := GetUIColor(clBlack);
//    cl3DLight:        Result := GetUIColor(clDarkWhite);
//    clInfoText:       Result := GetUIColor(clBlack);
//    clInfoBk:         Result := GetUIColor(clLightYellow);
//
//    else              Result := GetUIColor(clWhite);
  end;

end;

(*
procedure TOpenSoftStyle.DrawButtonFace(Canvas: TFCanvas; const ARect: TRect;
  Flags: TButtonFlags);
begin
//  inherited DrawButtonFace(Canvas, ARect, Flags);
//  PaintGradient(Canvas, ARect, Flags);
  Draw3DFrame(Canvas, ARect, cl3DHighlight, cl3DLight, cl3DDkShadow, cl3DShadow);
end;
*)

procedure TOpenSoftStyle.DrawGroupBox(Canvas: TFCanvas; const ARect: TRect;
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


//initialization
//finalization
//  gStyleManager.RegisterClass('OpenSoft', TOpenSoftStyle);

end.

