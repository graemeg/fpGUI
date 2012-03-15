{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:

      A Object Pascal implemenation of Wu Anti-Aliased Lines.
      For details about the algorithm have a look at:
        http://freespace.virgin.net/hugo.elias/graphics/x_wuline.htm
      Here is an implementation in C++
        http://www.codeproject.com/gdi/antialias.asp
}
unit fpg_wuline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base, fpg_main;


procedure WuLine(ACanvas: TfpgCanvas; Point1, Point2: TPoint; AColor: TfpgColor);
procedure DrawWuCircle(Canvas: TfpgCanvas; x, y, r: integer; AColor: TfpgColor);


implementation


type
  // to get access to Protected fields
  TCanvasHack = class(TfpgCanvas);
  

// Blend a pixel with the current colour
procedure AlphaBlendPixel(ACanvas: TfpgCanvas; X, Y: integer; R, G, B: word; ARatio: Double);
var
  LBack, LNew: TRGBTriple;
  LMinusRatio: Double;
begin
  if (X < 0) or (X > TCanvasHack(ACanvas).FWindow.Width - 1) or (Y < 0) or
     (Y > TCanvasHack(ACanvas).FWindow.Height - 1) then
    Exit; // clipping

  LMinusRatio := 1 - ARatio;
  LBack       := fpgColorToRGBTriple(ACanvas.Pixels[X, Y]);
  LNew.Blue   := round(B*ARatio + LBack.Blue*LMinusRatio);
  LNew.Green  := round(G*ARatio + LBack.Green*LMinusRatio);
  LNew.Red    := round(R*ARatio + LBack.Red*LMinusRatio);
  LNew.Alpha := 255;
  ACanvas.Pixels[X, Y] := RGBTripleTofpgColor(LNew);
end;

// Draw a anti-aliased line
procedure WuLine(ACanvas: TfpgCanvas; Point1, Point2: TPoint; AColor: TfpgColor);
var
  deltax, deltay, loop, start, finish: integer;
  dx, dy, dydx: single; // fractional parts
  LR, LG, LB: word;
  x1, x2, y1, y2: integer;
begin
  x1      := Point1.X;
  y1      := Point1.Y;
  x2      := Point2.X;
  y2      := Point2.Y;
  deltax  := abs(x2 - x1); // Calculate deltax and deltay for initialisation
  deltay  := abs(y2 - y1);
  
  if (deltax = 0) or (deltay = 0) then  // straight lines
  begin
    ACanvas.SetColor(AColor);
    ACanvas.DrawLine(x1, y1, x2, y2);
    Exit; //==>
  end;
  LR := fpgGetRed(AColor);
  LG := fpgGetGreen(AColor);
  LB := fpgGetBlue(AColor);
  if deltax > deltay then
  begin // horizontal or vertical
    if y2 > y1 then // determine rise and run
      dydx := -(deltay / deltax)
    else
      dydx := deltay / deltax;
      
    if x2 < x1 then
    begin
      start   := x2; // right to left
      finish  := x1;
      dy      := y2;
    end
    else
    begin
      start   := x1; // left to right
      finish  := x2;
      dy      := y1;
      dydx    := -dydx; // inverse slope
    end;
    
    for loop := start to finish do
    begin
      AlphaBlendPixel(ACanvas, loop, trunc(dy), LR, LG, LB, 1 - frac(dy));
      AlphaBlendPixel(ACanvas, loop, trunc(dy) + 1, LR, LG, LB, frac(dy));
      dy := dy + dydx; // next point
    end;
  end
  else
  begin
    if x2 > x1 then // determine rise and run
      dydx := -(deltax / deltay)
    else
      dydx := deltax / deltay;
      
    if y2 < y1 then
    begin
      start   := y2; // right to left
      finish  := y1;
      dx      := x2;
    end
    else
    begin
      start   := y1; // left to right
      finish  := y2;
      dx      := x1;
      dydx    := -dydx; // inverse slope
    end;
    
    for loop := start to finish do
    begin
      AlphaBlendPixel(ACanvas, trunc(dx), loop, LR, LG, LB, 1 - frac(dx));
      AlphaBlendPixel(ACanvas, trunc(dx) + 1, loop, LR, LG, LB, frac(dx));
      dx := dx + dydx; // next point
    end;
  end;
end;

// A very basic circle implementation. Not to pretty, but it works.
procedure DrawWuCircle(Canvas: TfpgCanvas; x, y, r: integer; AColor: TfpgColor);
var
	x1, y1, x2, y2: integer;
	dt: integer;
  theta: integer;
begin
  theta := 0;
  dt := 4;
  while theta < 360 do
  begin
		x1 := round( r*cos(theta*pi/180.0)+x);
		y1 := round(-r*sin(theta*pi/180.0)+y);

		x2 := round( r*cos((theta+dt)*pi/180.0)+x);
		y2 := round(-r*sin((theta+dt)*pi/180.0)+y);

		WuLine(Canvas, Point(x1, y1), Point(x2, y2), AColor);
    theta := theta + dt;
  end;
end;

end.

