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
      A Gauge component that supports different display styles. eg: Needle,
      Dial, Pie etc.
}

unit fpg_gauge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math,
  fpg_base,
  fpg_main,
  fpg_widget;

type

  TGaugeKind = (gkText, gkHorizontalBar, gkVerticalBar, gkPie, gkNeedle, gkDial);

  TBorderStyle = (bsNone, bsSingle, bsDouble, bsRaisedPanel, bsSunkenPanel, 
                  bsRaised3D, bsSunken3D, bsEtched, bsEmmbossed);


  TfpgBaseGauge = class(TfpgWidget)
  private
    FFont: TfpgFont;
    FMin: Longint;
    FMax: Longint;
    FPosition: Longint;
    FShowText: Boolean;
    { TODO: Implement Border style }
    FBorderStyle: TBorderStyle;
    FColor: TfpgColor; // Background color
    { Currently little used colors, should be derived from style and possibly
    overriden by user TODO - How to deal with gradients? Starting color and compute ending,
    or give pair? }
    FFirstColor: TfpgColor; // Text and Needle color
    FSecondColor: TfpgColor; // Bar, Pie etc. main color
    { TODO: Currently unused. Implement Low Watermark and High Watermark }
//    FLWMColor: TfpgColor; // Low Watermark Color
//    FLWMValue: Longint;   //  Low Watermark Value
//    FHWMColor: TfpgColor; // High Watermark Color
//    FHWMValue: Longint;   // High Watermark Color
    procedure   SetGaugeKind(AValue: TGaugeKind);
    procedure   SetShowText(AValue: Boolean);
    procedure   SetBorderStyle(AValue: TBorderStyle);
    procedure   SetFirstColor(AValue: TfpgColor);
    procedure   SetSecondColor(AValue: TfpgColor);
    procedure   SetMin(AValue: Longint);
    procedure   SetMax(AValue: Longint);
    procedure   SetProgress(AValue: Longint);
    function    GetPercentage: Longint;
  protected
    FClientRect: TfpgRect;
    FKind: TGaugeKind;
    procedure   BackgroundDraw; virtual;
    procedure   TextDraw; virtual;
    procedure   BarDraw; virtual;
    procedure   PieDraw; virtual;
    procedure   NeedleDraw; virtual;
    procedure   DialDraw; virtual;
    procedure   HandlePaint; override;
    property    BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property    Color: TfpgColor read FColor write FColor default TfpgColor($c4c4c4);
    property    FirstColor: TfpgColor read FFirstColor write SetFirstColor default clBlack;
    property    Kind: TGaugeKind read FKind write SetGaugeKind default gkHorizontalBar;
    property    MaxValue: Longint read FMax write SetMax default 100;
    property    MinValue: Longint read FMin write SetMin default 0;
    property    Progress: Longint read FPosition write SetProgress;
    property    SecondColor: TfpgColor read FSecondColor write SetSecondColor default clWhite;
    property    ShowText: Boolean read FShowText write SetShowText default True;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AddProgress(AValue: Longint);
    property    Percentage: Longint read GetPercentage;
    property    Font: TfpgFont read FFont;
  end;


  TfpgGauge = class(TfpgBaseGauge)
  published
    property    Align;
    property    Anchors;
    property    BorderStyle;
    property    Color;
    property    Enabled;
    property    FirstColor;
    property    Hint;
    property    Kind;
    property    MaxValue;
    property    MinValue;
    property    ParentShowHint;
    property    Progress;
    property    SecondColor;
    property    ShowHint;
    property    ShowText;
    property    Visible;
    property    OnShowHint;
  end;

// A convenience function to quickly create a gauge from code
function CreateGauge (AOwner: TComponent; ALeft, ATop, AWidth,
    AHeight: TfpgCoord; AKind: TGaugeKind ): TfpgBaseGauge;


implementation

uses
  fpg_wuline;

{ This procedure draws a filled arc with a color gradient  -
  to be moved in CanvasBase? }
procedure FillArcGradient(canvas: TfpgCanvas; X,Y,W,H: TfpgCoord; a1,a2: double; Astart,Astop: TfpgColor);
var
  RGBStart: TRGBTriple;
  RGBStop: TRGBTriple;
  RDiff, GDiff, BDiff: Integer;
  count: Integer;
  i: Integer;
  newcolor: TRGBTriple;
begin
  if Astart = Astop then
  begin { No gradient, just solid color}
    canvas.SetColor(Astart);
    canvas.FillArc(X, Y, W, H, a1, a2);
    Exit; //==>
  end;

  RGBStart := fpgColorToRGBTriple(fpgColorToRGB(AStart));
  RGBStop  := fpgColorToRGBTriple(fpgColorToRGB(AStop));

  count := min(H,W);
  count := count div 2;
  count := count -2 ;

  RDiff := RGBStop.Red - RGBStart.Red;
  GDiff := RGBStop.Green - RGBStart.Green;
  BDiff := RGBStop.Blue - RGBStart.Blue;


  { X11 draws arcs at one pixel distance without leaving out pixels, so Line Width
   of 1 would be appropriate, but GDI doesn't, and therefore Line Width 2 is
   required to make both work}

  //canvas.SetLineStyle(1,lsSolid);
  canvas.SetLineStyle(2,lsSolid);
  for i := 0 to count do
  begin
    X := X + 1;
    Y := Y + 1;
    W := W - 2;
    H := H - 2;
    newcolor.Red    := RGBStart.Red + (i * RDiff) div count;
    newcolor.Green  := RGBStart.Green + (i * GDiff) div count;
    newcolor.Blue   := RGBStart.Blue + (i * BDiff) div count;
    canvas.SetColor(RGBTripleTofpgColor(newcolor));
    canvas.DrawArc(X, Y, W, H, a1, a2);
  end;
end;

function CreateGauge(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TfpgCoord;
    AKind: TGaugeKind): TfpgBaseGauge;
begin
  Result := TfpgBaseGauge.Create(AOwner);
  Result.Left   := ALeft;
  Result.Top    := ATop;
  Result.Width  := AWidth;
  Result.Height := AHeight;
  Result.Kind   := AKind;
end;

{ TfpgBaseGauge }

{ Drawing procedures - they're called from HandlePaint, which takes care of
  Canvas.BeginDraw and Canvas.EndDraw - Shouldn't be used otherwise. }
procedure TfpgBaseGauge.BackgroundDraw;
begin
  {common Background for all kinds }

  {Client area is Widget area, to start with}
  FClientRect.SetRect(0, 0, Width, Height);
  Canvas.ClearClipRect;
  Canvas.Clear(Color);
  { This must be adjusted according the selected style }
  Canvas.SetColor(TfpgColor($999999));
  Canvas.SetLineStyle(1, lsSolid);
  Canvas.DrawRectangle(FClientRect);
  { This must be completed and adjusted with border style }
  InflateRect(FClientRect, -1, -1);
  with FClientRect do
  begin
    { Kind specific Bacground }
    case FKind of
      { Currently Text doesn't require additional Bacground }
      { And so horizontal and vertical bar - Unless style requires it}
      gkHorizontalBar,
      gkVerticalBar:
          begin
            Canvas.SetLineStyle(1, lsSolid); // just in case background changed that
          end;
      gkPie:
          begin
            { Round frame for the Pie }
            Canvas.SetLineStyle(2, lsSolid);
            Canvas.SetColor(TfpgColor($98b2ed));
            Canvas.DrawArc(Left, Top, Width, Height, 0, 360);
          end;
      gkNeedle:
          begin
            { Half a filled circle background for needle }
            FillArcGradient(Canvas,Left, Top, Width, Height * 2 -1, 0, 180,TfpgColor($425d9b),TfpgColor($98b2ed));
            Canvas.SetLineStyle(2, lsSolid);
            //Canvas.SetColor(TfpgColor($3b4c71));
            Canvas.SetColor(TfpgColor($98b2ed));
            Canvas.DrawArc(Left, Top, Width, Height * 2 - 1, 0, 180);
            Canvas.SetLineStyle(1, lsSolid);
            Canvas.SetColor(TfpgColor($3b4c71));
            Canvas.DrawLine(Left, Bottom,Left + Width, Bottom);
          end;
      gkDial:
          begin
            { 270° pie shaped background for Dial }
            FillArcGradient (Canvas,Left, Top, Width, Height , 225, -270 ,TfpgColor($425d9b),TfpgColor($98b2ed));
            Canvas.SetLineStyle(2, lsSolid);
            //Canvas.SetColor(TfpgColor($3b4c71));
            Canvas.SetColor(TfpgColor($98b2ed));
            Canvas.DrawArc(Left,Top,Width,Height,225,-270);
          end;
    end;
  end;  { with }
end;

procedure TfpgBaseGauge.TextDraw;
var
  S: string;
  X, Y: Integer;
begin
  S := Format('%d%%', [Percentage]);
  with FClientRect do 
  begin
    X := (Width - FFont.TextWidth(S)) div 2;
    Y := (Height - FFont.Height) div 2;
    if Kind = gkDial then 
      Y := Y + (Y div 2);
  end;
{ If contrast is poor we might use a Xor function }
  Canvas.SetTextColor(FirstColor);
  Canvas.Font := FFont;
  Canvas.DrawString(x, y, S);
end;

procedure TfpgBaseGauge.BarDraw;
var
  BarLength: Longint;
  SavedRect: TfpgRect;
begin
  SavedRect := FClientRect; // save client rect for text !!
  with FClientRect do
  begin
    case FKind of
      gkHorizontalBar:
          begin
            BarLength := Longint(Trunc( (Width * Percentage) / 100.0 ) );
            if BarLength > 0 then
            begin
              if BarLength > Width then
                BarLength := Width;
              Width := BarLength;
              // left top
              Canvas.SetColor(TfpgColor($98b2ed));
              Canvas.DrawLine(Left, Bottom, Left, Top);  // left
              Canvas.DrawLine(Left, Top, Right, Top);    // top
              // right bottom
              Canvas.SetColor(TfpgColor($3b4c71));
              Canvas.DrawLine(Right, Top, Right, Bottom);   // right
              Canvas.DrawLine(Right, Bottom, Left, Bottom);   // bottom
              // inside gradient fill
              InflateRect(FClientRect, -1, -1);
              Canvas.GradientFill(FClientRect, TfpgColor($425d9b), TfpgColor($97b0e8), gdVertical);
            end;  { if }
          end;
      gkVerticalBar:
          begin
            BarLength := Longint(Trunc( (Height * Percentage) / 100.0 ) );
            if BarLength > 0 then
            begin
              if BarLength > Height then
                BarLength := Height;
              Top := Height - BarLength+1;
              Height := BarLength;
              // left top
              Canvas.SetColor(TfpgColor($98b2ed));
              Canvas.DrawLine(Left, Bottom, Left, Top);  // left
              Canvas.DrawLine(Left, Top, Right, Top);    // top
              // right bottom
              Canvas.SetColor(TfpgColor($3b4c71));
              Canvas.DrawLine(Right, Top, Right, Bottom);   // right
              Canvas.DrawLine(Right, Bottom, Left, Bottom);   // bottom
              // inside gradient fill
              InflateRect(FClientRect, -1, -1);
              Canvas.GradientFill(FClientRect, TfpgColor($425d9b), TfpgColor($97b0e8), gdHorizontal);
            end;
          end;  { if }
    end;  { case }
  end; { with }
  FClientRect := SavedRect;
end;

procedure TfpgBaseGauge.PieDraw;
var
  Angle: Double;
begin
  with FClientRect do
  begin
    Angle := Percentage;
    Angle := Angle * 3.6; // Percentage to degrees
    Canvas.SetColor(TfpgColor($425d9b));
    FillArcGradient (Canvas,Left, Top, Width, Height , 90, -Angle,TfpgColor($425d9b),TfpgColor($98b2ed));
  end;
end;

procedure TfpgBaseGauge.NeedleDraw;
var
  Center: TPoint;
  Radius: TPoint;
  Angle: Double;
begin
  with FClientRect do
  begin
    if Percentage > 0 then
    begin
      { Compute the center }
      Center := CenterPoint(Rect(Left,Top,Width,Height));
      { Make needle 4 pixel shorter than gauge radius to accomodate border }
      Radius.X := Center.X - 4;
      Radius.Y := (Bottom - 4);
      Canvas.SetLineStyle(2,lsSolid);
      Angle := (Pi * ((Percentage / 100.0))); // percentage to radiants
      Canvas.SetColor(TfpgColor($3b4c71));
      Canvas.SetLineStyle(2,lsSolid);
      //Canvas.DrawLine(Center.X, FClientRect.Bottom,
          //Integer(round(Center.X - (Radius.X * Cos(Angle)))),
          //Integer(round((FClientRect.Bottom) - (Radius.Y * Sin(Angle)))));
          
      { *** Experimental *** }
      WuLine(Canvas,
          Point(Center.X, FClientRect.Bottom),
          Point(Integer(round(Center.X - (Radius.X * Cos(Angle)))),
                Integer(round((FClientRect.Bottom) - (Radius.Y * Sin(Angle))))),
          Canvas.Color);
      WuLine(Canvas,
          Point(Center.X+1, FClientRect.Bottom),
          Point(Integer(round(Center.X+1 - (Radius.X * Cos(Angle)))),
                Integer(round((FClientRect.Bottom) - (Radius.Y * Sin(Angle))))),
          Canvas.Color);
    end;
  end;
end;

procedure TfpgBaseGauge.DialDraw;
var
  Center: TPoint;
  Radius: TPoint;
  Angle: Double;
  CenterDot: Integer;
begin
  with FClientRect do
  begin
    if Percentage >= 0 then
    begin
      { Compute the center }
      Center := CenterPoint(Rect(Left,Top,Width,Height));
      { Make needle 3 pixel shorter than gauge radius }
      Radius.X := Center.X -3;
      Radius.Y := Center.Y -3;
      {compute centre circle size}
      CenterDot := (Width + Height) div 40; // approx. scaled to 1/10 of widget size:
      if CenterDot < 2 then
        CenterDot := 2;
      { draw needle centre circle }
      Canvas.SetColor(TfpgColor($3b4c71));
      Canvas.FillArc(Center.X - CenterDot, Center.Y - CenterDot,CenterDot * 2, CenterDot * 2,0,360);
      { draw needle }
      Angle := (Pi * ((Percentage / (100 * 2 / 3)) + -0.25));
      Canvas.SetLineStyle(2,lsSolid);
      //Canvas.DrawLine(Center.X, Center.Y,
          //Integer(round(Center.X  - ( Radius.X * cos(Angle)))),
          //Integer(round((Center.Y) - (Radius.Y * Sin(Angle)))));

      { *** Experimental *** }
      WuLine(Canvas,
          Point(Center.X, Center.Y),
          Point(Integer(round(Center.X  - ( Radius.X * cos(Angle)))),
                Integer(round((Center.Y) - (Radius.Y * Sin(Angle))))),
          Canvas.Color);
      WuLine(Canvas,
          Point(Center.X+1, Center.Y),
          Point(Integer(round(Center.X+1  - ( Radius.X * cos(Angle)))),
                Integer(round((Center.Y) - (Radius.Y * Sin(Angle))))),
          Canvas.Color);
    end;  { if }
  end;  { with }
end;

procedure TfpgBaseGauge.HandlePaint;
begin
  inherited HandlePaint;
//  Canvas.BeginDraw(True);
  {Paint Background and adjust FClientRect according style and BorderStyle}
  BackgroundDraw;
  {Paint foreground according selected Kind}
  case FKind of
    gkHorizontalBar,
    gkVerticalBar:
        BarDraw;
    gkPie:
        PieDraw;
    gkNeedle:
        NeedleDraw;
    gkDial:
        DialDraw;
  end;
  {Add Text if required}
  if ShowText then
    TextDraw;
//  Canvas.EndDraw;
end;

procedure TfpgBaseGauge.SetGaugeKind(AValue: TGaugeKind);
begin
  if AValue <> FKind then
  begin
    FKind := AValue;
    RePaint;
  end;
end;

procedure TfpgBaseGauge.SetShowText(AValue: Boolean);
begin
  if AValue <> FShowText then
  begin
    FShowText := AValue;
    RePaint;
  end;
end;

procedure TfpgBaseGauge.SetBorderStyle(AValue: TBorderStyle);
begin
  if AValue <> FBorderStyle then
  begin
    FBorderStyle := AValue;
    { TODO: Implement Border style }
    // Graeme:  Wouldn't descending from TfpgBevel give you this functionality already?
    //          It could be a option.
    //RePaint;
  end;
end;

procedure TfpgBaseGauge.SetFirstColor(AValue: TfpgColor);
begin
  if AValue <> FFirstColor then
  begin
    FFirstColor := AValue;
    { TODO: allow user colors}
    //RePaint;
  end;
end;

procedure TfpgBaseGauge.SetSecondColor(AValue: TfpgColor);
begin
  if AValue <> FSecondColor then
  begin
    FSecondColor := AValue;
    { TODO: allow user colors}
    //RePaint;
  end;
end;

procedure TfpgBaseGauge.SetMin(AValue: Longint);
begin
  if AValue <> FMin then
  begin
    // correct input errors
    if AValue > FMax then
      if not (csLoading in ComponentState) then
        FMax := AValue + 1;
    if FPosition < AValue then
      FPosition := AValue;
    // then update
    FMin := AValue;
    RePaint;
  end;
end;

procedure TfpgBaseGauge.SetMax(AValue: Longint);
begin
  if AValue <> FMax then
  begin
    // correct input errors
    if AValue < FMin then
      if not (csLoading in ComponentState) then
        FMin := AValue - 1;
    if FPosition > AValue then
      FPosition := AValue;
    // then update
    FMax := AValue;
    RePaint;
  end;
end;

procedure TfpgBaseGauge.SetProgress(AValue: Longint);
var
  CurrPercentage: Longint;
  MustRepaint: Boolean;
begin
  CurrPercentage  := GetPercentage;
  MustRepaint     := False;
  
  if AValue < FMin then
    AValue := FMin
  else if AValue > FMax then
    AValue := FMax;
    
  if FPosition <> AValue then
  begin // Value has changed
    FPosition := AValue;
    if CurrPercentage <> Percentage then  // Visible value has changed
      MustRepaint := True;
    { TODO: Check against low and high watermarks }
    end;
  if MustRepaint then
    RePaint;
end;

function TfpgBaseGauge.GetPercentage: Longint;
Var
 V,T: Longint;
begin
  T := FMax - FMin;
  V := FPosition - FMin;
  if T = 0 then
    Result := 0
  else
    Result := Longint(Trunc( (V * 100.0) / T ));
end;

constructor TfpgBaseGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Focusable     := False;
  FWidth        := 100;
  FHeight       := 25;
  FKind         := gkHorizontalBar;
  FSecondColor  := clWhite;
  FFirstColor   := clBlack;
  FColor        := TfpgColor($c4c4c4); //clInactiveWgFrame;
  FMax          := 100;
  FMin          := 0;
  FPosition     := 0;
  FShowText     := True;
  FBorderStyle  := bsNone;
  FFont         := fpgStyle.DefaultFont;
end;

procedure TfpgBaseGauge.AddProgress(AValue: Longint);
begin
  Progress := FPosition + AValue;
end;

end.

