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
      This unit adds extra drawing routines to fpGUI. It unit originally
      came from the Lazarus Component Library (LCL) and was ported to fpGUI.
}

unit fpg_extgraphics;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Math,
  fpg_main;

type
  TShapeDirection = (atUp, atDown, atLeft, atRight);
  TInitShapeProc  = procedure(var P: array of TPoint; const R: TRect; var NumPts: integer);


procedure Paint2HeadArrow(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintBarbadosTrident(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintBigI(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintBoldArrow(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintCanadianMaple(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintChevronArrow(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintFivePointStar(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintHexagon(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintNotchedArrow(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintOctogon(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintPentagon(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintPlus(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintQuadrangle(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintRightTriangle(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintSwastika(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintTriangle(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintTriangular(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0; RightLeftFactor: extended = 0.5);
procedure PaintValve(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintVArrow(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
procedure PaintCross(Canvas: TfpgCanvas; XLeft, YUp, XRight, YLow, CrossX1, CrossX2, CrossY1, CrossY2: integer);
procedure PaintHalfEllipse(Canvas: TfpgCanvas; const PaintRect: TRect; AHalfEllipseDirection: TShapeDirection);
procedure PaintFivePointLineStar(Canvas: TfpgCanvas; const PaintRect: TRect);
procedure PaintStarN(Canvas: TfpgCanvas; cx, cy, r, n, a: integer);


procedure InitPolygon(Canvas: TfpgCanvas; PaintRect: TRect; RadAngle: extended; InitShapeProc: TInitShapeProc);

procedure CalculatePentagonPoints(const PentagonRect: TRect; var P1, P2, P3, P4, P5: TPoint);
function LinesPointOfIntersection(const Line1a, Line1b, Line2a, line2b: TPoint): TPoint;


implementation

uses
  SysUtils;


function RoundToInt(const e: extended): integer;
begin
  Result := integer(Round(e));
end;

procedure CalculatePentagonPoints(const PentagonRect: TRect; var P1, P2, P3, P4, P5: TPoint);
var
  cx, cy, dy, dx: integer;
  r: Extended;
begin
  P1.y := PentagonRect.Top;
  P2.x := PentagonRect.Left;
  P3.y := PentagonRect.Bottom;
  P4.y := PentagonRect.Bottom;
  P5.x := PentagonRect.Right;
  P1.x := (PentagonRect.Right + PentagonRect.Left) div 2;
  dy   := RoundToInt((P1.x - P2.x) * tan(Pi / 10));
  r    := sqrt(dy * dy + (P1.x - P2.x) * (P1.x - P2.x));
  cx   := P1.x;
  cy   := P1.y + round(r);
  P2.y := cy - dy;
  P5.y := P2.y;
  dx   := RoundToInt(r * sin(Pi / 5));
  P3.x := cx - dx;
  P4.x := cx + dx;
end;

function LinesPointOfIntersection(const Line1a, Line1b, Line2a, line2b: TPoint): TPoint;
var
  k1, k2, b1, b2, x, x1, x2, x3, x4, y, y1, y2, y3, y4: Extended;
begin
  x1  := Line1a.x;      y1  := Line1a.y;
  x2  := Line1b.x;      y2  := Line1b.y;
  x3  := Line2a.x;      y3  := Line2a.y;
  x4  := Line2b.x;      y4  := Line2b.y;
  k1  := (y2 - y1) / (x2 - x1);
  k2  := (y4 - y3) / (x4 - x3);
  b1  := -k1 * x1 + y1;
  b2  := -k2 * x3 + y3;
  x   := (b1 - b2) / (k2 - k1);
  y   := (k2 * b1 - k1 * b2) / (k2 - k1);
  Result.x := RoundToInt(x);
  Result.y := RoundToInt(y);
end;

procedure PaintCross(Canvas: TfpgCanvas; XLeft, YUp, XRight, YLow, CrossX1, CrossX2, CrossY1, CrossY2: integer);
var
  P: array[0..12] of TPoint;
begin
  P[0].x  := XLeft;     P[0].y  := CrossY1;
  P[1].x  := CrossX1;   P[1].y  := P[0].y;
  P[2].x  := P[1].x;    P[2].y  := YUp;
  P[3].x  := CrossX2;   P[3].y  := P[2].y;
  P[4].x  := P[3].x;    P[4].y  := CrossY1;
  P[5].x  := XRight;    P[5].y  := P[4].y;
  P[6].x  := P[5].x;    P[6].y  := CrossY2;
  P[7].x  := CrossX2;   P[7].y  := P[6].y;
  P[8].x  := P[7].x;    P[8].y  := YLow;
  P[9].x  := CrossX1;   P[9].y  := P[8].y;
  P[10].x := P[9].x;    P[10].y := CrossY2;
  P[11].x := XLeft;     P[11].y := P[10].y;
  P[12].x := P[11].x;   P[12].y := CrossY1;
  Canvas.DrawPolygon(P);
end;

procedure PolycRotate(var Pts: array of TPoint; CountPts: integer; cntPoint: TPoint; fii: extended);
var
  i, dx, dy: integer;
  x, y: extended;
begin
  for i := 0 to CountPts - 1 do
  begin
    dx       := Pts[i].x - cntPoint.x;
    dy       := Pts[i].y - cntPoint.y;
    x        := dx * cos(fii) + dy * sin(fii);
    y        := dy * cos(fii) - dx * sin(fii);
    Pts[i].x := cntPoint.x + Round(x);
    Pts[i].y := cntPoint.y + Round(y);
  end;
end;

procedure PolycMinMax
  (var N: array of TPoint; const P: array of TPoint; CountPts: integer);
var
  i, Xmin, Xmax, Ymin, Ymax: integer;
begin
  Xmin := P[0].x;
  Xmax := P[0].x;
  Ymin := P[0].y;
  Ymax := P[0].y;
  for i := 0 to CountPts - 1 do
  begin
    if P[i].x < Xmin then
      Xmin := P[i].x;
    if P[i].x > Xmax then
      Xmax := P[i].x;
    if P[i].y < Ymin then
      Ymin := P[i].y;
    if P[i].y > Ymax then
      Ymax := P[i].y;
  end;
  N[0] := Point(Xmin, Ymin);
  N[1] := Point(Xmin, Ymax);
  N[2] := Point(Xmax, Ymax);
  N[3] := Point(Xmax, Ymin);
end;

procedure PolycNewPaintRect(var PR: TRect; cP: TPoint; wv, hv: integer);
begin
  with PR do
  begin
    Left   := cP.x - wv;
    Right  := cP.x + wv;
    Top    := cP.y - hv;
    Bottom := cP.y + hv;
  end;
end;

procedure PolycFixCenterpoint(var N: array of TPoint; cP: TPoint; var P: array of TPoint; CountPts: integer);
var
  i, nx, ny, dx, dy: integer;
begin
  nx := (N[0].x + N[2].x) div 2;
  ny := (N[0].y + N[2].y) div 2;
  dx := cP.x - nx;
  dy := cP.y - ny;
  for i := 0 to 3 do
  begin
    N[i].x := N[i].x + dx;
    N[i].y := N[i].y + dy;
  end;
  for i := 0 to CountPts - 1 do
  begin
    P[i].x := P[i].x + dx;
    P[i].y := P[i].y + dy;
  end;
end;

procedure PolycSetHalfWidthAndHeight(const PR: TRect; var hv, wv: integer; fii: extended);
var
  h, w: integer;
begin
  h  := PR.Bottom - PR.Top;
  w  := PR.Right - PR.Left;
  hv := Round(h * abs(cos(fii)) + w * abs(sin(fii))) div 2;
  wv := Round(h * abs(sin(fii)) + w * abs(cos(fii))) div 2;
end;

procedure PolycScale(var P: array of TPoint; CountPts: integer; const PaintRect: TRect; cntPoint: TPoint; N: array of TPoint);
var
  k, kx, ky: extended;
  i: integer;
begin
  kx := (PaintRect.Right - PaintRect.Left) / (N[2].x - N[0].x);
  ky := (PaintRect.Bottom - PaintRect.Top) / (N[2].y - N[0].y);
  k  := min(kx, ky);
  for i := 0 to CountPts - 1 do
  begin
    P[i].x := cntPoint.x + Round(k * (P[i].x - cntPoint.x));
    P[i].y := cntPoint.y + Round(k * (P[i].y - cntPoint.y));
  end;
end;

procedure PaintPolygon(Canvas: TfpgCanvas; PR: TRect; fii: extended; P: array of TPoint; CountPts: integer; cntPoint: TPoint);
var
  N: array[0..3] of TPoint;
begin
  PolycRotate(P, CountPts, cntPoint, fii);
  PolycMinMax(N, P, CountPts);
  PolycFixCenterpoint(N, cntPoint, P, CountPts);
  PolycScale(P, CountPts, PR, cntPoint, N);
  case CountPts of
    3:  Canvas.DrawPolygon([P[0], P[1], P[2]]);
    4:  Canvas.DrawPolygon([P[0], P[1], P[2], P[3]]);
    5:  Canvas.DrawPolygon([P[0], P[1], P[2], P[3], P[4]]);
    6:  Canvas.DrawPolygon([P[0], P[1], P[2], P[3], P[4], P[5]]);
    7:  Canvas.DrawPolygon([P[0], P[1], P[2], P[3], P[4], P[5], P[6]]);
    8:  Canvas.DrawPolygon([P[0], P[1], P[2], P[3], P[4], P[5], P[6], P[7]]);
    9:  Canvas.DrawPolygon([P[0], P[1], P[2], P[3], P[4], P[5], P[6], P[7], P[8]]);
    10: Canvas.DrawPolygon([P[0], P[1], P[2], P[3], P[4], P[5], P[6], P[7], P[8], P[9]]);
    11: Canvas.DrawPolygon([P[0], P[1], P[2], P[3], P[4], P[5], P[6], P[7], P[8], P[9],
        P[10]]);
    12: Canvas.DrawPolygon([P[0], P[1], P[2], P[3], P[4], P[5], P[6], P[7], P[8], P[9],
        P[10], P[11]]);
    13: Canvas.DrawPolygon([P[0], P[1], P[2], P[3], P[4], P[5], P[6], P[7], P[8], P[9],
        P[10], P[11], P[12]]);
    20: Canvas.DrawPolygon([P[0], P[1], P[2], P[3], P[4], P[5], P[6], P[7], P[8], P[9],
        P[10], P[11], P[12], P[13], P[14], P[15], P[16], P[17], P[18], P[19]]);
    33: Canvas.DrawPolygon([P[0], P[1], P[2], P[3], P[4], P[5], P[6], P[7], P[8], P[9],
        P[10], P[11], P[12], P[13], P[14], P[15], P[16], P[17], P[18], P[19],
        P[20], P[21], P[22], P[23], P[24], P[25], P[26], P[27], P[28], P[29],
        P[30], P[31], P[32]]);
    35: Canvas.DrawPolygon([P[0], P[1], P[2], P[3], P[4], P[5], P[6], P[7], P[8], P[9],
        P[10], P[11], P[12], P[13], P[14], P[15], P[16], P[17], P[18], P[19],
        P[20], P[21], P[22], P[23], P[24], P[25], P[26], P[27], P[28], P[29],
        P[30], P[31], P[32], P[33], P[34]]);
  end;
end;

procedure InitPolygon(Canvas: TfpgCanvas; PaintRect: TRect; RadAngle: extended; InitShapeProc: TInitShapeProc);
var
  PR, vPR: TRect;
  P: array[0..35] of TPoint;
  CountPts, hv, wv: integer;
  cntPoint: TPoint;
begin
  PR       := PaintRect;
  cntPoint := CenterPoint(PR);
  PolycSetHalfWidthAndHeight(PR, hv, wv, RadAngle);
  PolycNewPaintRect(vPR, cntPoint, wv, hv);
  InitShapeProc(P, vPR, CountPts);
  PaintPolygon(Canvas, PR, RadAngle, P, CountPts, cntPoint);
end;

procedure Init2HeadArrow(var P: array of TPoint; const R: TRect; var NumPts: integer);
var
  dx, dy: integer;
begin
  with R do
  begin
    dx     := (Right - Left) div 4;
    dy     := (Bottom - Top) div 4;
    P[0].x := Left;                 P[0].y := Top + (Bottom - Top) div 2;
    P[1].x := Left + dx;            P[1].y := Top;
    P[2].x := P[1].x;               P[2].y := Top + dy;
    P[3].x := Right - dx;           P[3].y := P[2].y;
    P[4].x := P[3].x;               P[4].y := Top;
    P[5].x := Right;                P[5].y := P[0].y;
    P[6].x := P[3].x;               P[6].y := Bottom;
    P[7].x := P[3].x;               P[7].y := Bottom - dy;
    P[8].x := P[1].x;               P[8].y := P[7].y;
    P[9].x := P[1].x;               P[9].y := Bottom;
  end;
  NumPts := 10;
end;

procedure InitBarbadosTrident(var P: array of TPoint; const R: TRect; var NumPts: integer);
var
  RmLpW, BmTpH: extended;
  cntPoint: TPoint;
begin
  cntPoint := CenterPoint(R);
  with R do
  begin
    RmLpW   := (Right - Left) / 140;
    BmTpH   := (Bottom - Top) / 160;
    P[0].x  := cntPoint.x - round(RmLpW * 10);
    P[0].y  := Bottom;
    P[34].x := cntPoint.x + round(RmLpW * 10);
    P[34].y := P[0].y;
    P[1].x  := P[0].x;
    P[1].y  := Bottom - round(BmTpH * 50);
    P[33].x := P[34].x;
    P[33].y := P[1].y;
    P[2].x  := cntPoint.x - round(RmLpW * 35);
    P[2].y  := P[1].y;
    P[32].x := cntPoint.x + round(RmLpW * 35);
    P[32].y := P[2].y;
    P[3].x  := cntPoint.x - round(RmLpW * 48);
    P[3].y  := Bottom - round(BmTpH * 98);
    P[31].x := cntPoint.x + round(RmLpW * 48);
    P[31].y := P[3].y;
    P[4].x  := left;
    P[4].y  := top;
    P[30].x := Right;
    P[30].y := P[4].y;
    P[5].x  := cntPoint.x - round(RmLpW * 42);
    P[5].y  := Top + round(BmTpH * 4);
    P[29].x := cntPoint.x + round(RmLpW * 42);
    P[29].y := P[5].y;
    P[6].x  := cntPoint.x - round(RmLpW * 40);
    P[6].y  := Top + round(BmTpH * 6);
    P[28].x := cntPoint.x + round(RmLpW * 40);
    P[28].y := P[6].y;
    P[7].x  := cntPoint.x - round(RmLpW * 39);
    P[7].y  := Top + round(BmTpH * 11);
    P[27].x := cntPoint.x + round(RmLpW * 39);
    P[27].y := P[7].y;
    P[8].x  := cntPoint.x - round(RmLpW * 45);
    P[8].y  := Top + round(BmTpH * 16);
    P[26].x := cntPoint.x + round(RmLpW * 45);
    P[26].y := P[8].y;
    P[9].x  := cntPoint.x - round(RmLpW * 45);
    P[9].y  := Top + round(BmTpH * 21);
    P[25].x := cntPoint.x + round(RmLpW * 45);
    P[25].y := P[9].y;
    P[10].x := cntPoint.x - round(RmLpW * 32);
    P[10].y := Top + round(BmTpH * 47);
    P[24].x := cntPoint.x + round(RmLpW * 32);
    P[24].y := P[10].y;
    P[11].x := cntPoint.x - round(RmLpW * 28);
    P[11].y := Top + round(BmTpH * 70);
    P[23].x := cntPoint.x + round(RmLpW * 28);
    P[23].y := P[11].y;
    P[12].x := cntPoint.x - round(RmLpW * 22);
    P[12].y := Top + round(BmTpH * 92);
    P[22].x := cntPoint.x + round(RmLpW * 22);
    P[22].y := P[12].y;
    P[13].x := P[0].x;
    P[13].y := P[12].y;
    P[21].x := P[34].x;
    P[21].y := P[13].y;
    P[14].x := P[0].x;
    P[14].y := Top + round(BmTpH * 30);
    P[20].x := P[34].x;
    P[20].y := P[14].y;
    P[15].x := cntPoint.x - round(RmLpW * 22);
    P[15].y := Top + round(BmTpH * 22);
    P[19].x := cntPoint.x + round(RmLpW * 22);
    P[19].y := P[15].y;
    P[16].x := cntPoint.x - round(RmLpW * 9);
    P[16].y := Top + round(BmTpH * 12);
    P[18].x := cntPoint.x + round(RmLpW * 9);
    P[18].y := P[16].y;
    P[17].x := cntPoint.x;
    P[17].y := Top;
  end;
  NumPts := 35;
end;

procedure InitBigI(var P: array of TPoint; const R: TRect; var NumPts: integer);
var
  dx, dy: integer;
begin
  with R do
  begin
    dx     := (Right - Left) div 4;
    dy     := (Bottom - Top) div 18;
    P[0].x := Left;                 P[0].y := Top;
    P[1].x := Right;                P[1].y := TOP;
    P[2].x := Right - dx;           P[2].y := Top + dy;
    P[3].x := P[2].x;               P[3].y := Bottom - dy;
    P[4].x := Right;                P[4].y := Bottom;
    P[5].x := Left;                 P[5].y := Bottom;
    P[6].x := Left + dx;            P[6].y := P[3].y;
    P[7].x := P[6].x;               P[7].y := P[2].y;
  end;
  NumPts := 8;
end;

procedure InitBoldArrow(var P: array of TPoint; const R: TRect; var NumPts: integer);
var
  dy: integer;
  cntPoint: TPoint;
begin
  cntPoint := CenterPoint(R);
  with R do
  begin
    dy     := (Bottom - Top) div 4;
    P[0].x := Left;                 P[0].y := Top + dy;
    P[1].x := cntPoint.x;           P[1].y := P[0].y;
    P[2].x := cntPoint.x;           P[2].y := Top;
    P[3].x := Right;                P[3].y := cntPoint.y;
    P[4].x := cntPoint.x;           P[4].y := Bottom;
    P[5].x := cntPoint.x;           P[5].y := Bottom - dy;
    P[6].x := Left;                 P[6].y := P[5].y;
  end;
  NumPts := 7;
end;

procedure InitCanadianMaple(var P: array of TPoint; const R: TRect; var NumPts: integer);
const
  leafheight = 54;
  leafwidth  = 50;
var
  xcenter, x2: integer;
  RmLpLW: extended;   // (Right - Left)/LeafWidth;
  BmTpLH: extended;   // (Bottom-Top)/ LeafHeight
begin
  with R do
  begin
    xcenter := Left + (Right - Left) div 2;
    p[0].y  := Top;                 p[0].x  := xcenter;
    RmLpLW  := (Right - Left) / LeafWidth;
    BmTpLH  := (Bottom - Top) / LeafHeight;
    x2      := RoundToInt(RmLpLW * 5);
    P[1].x  := xcenter - x2;        P[1].y  := RoundToInt(BmTpLH * 9 + Top);
    P[32].x := xcenter + x2;        P[32].y := P[1].y;
    x2      := RoundToInt(RmLpLW * 10);
    P[2].x  := xcenter - x2;        P[2].y  := RoundToInt(BmTpLH * 7 + Top);
    P[31].x := xcenter + x2;        P[31].y := P[2].y;
    x2      := RoundToInt(RmLpLW * 7);
    P[3].x  := xcenter - x2;        P[3].y  := RoundToInt(BmTpLH * 21 + Top);
    P[30].x := xcenter + x2;        P[30].y := P[3].y;
    x2      := RoundToInt(RmLpLW * 9);
    P[4].x  := xcenter - x2;        P[4].y  := P[3].y;
    P[29].x := xcenter + x2;        P[29].y := P[3].y;
    x2      := RoundtoInt(RmLpLW * 15);
    P[5].x  := xcenter - x2;        P[5].y  := RoundtoInt(BmTpLH * 15 + Top);
    P[28].x := xcenter + x2;        P[28].y := P[5].y;
    x2      := RoundtoInt(RmLpLW * 17);
    P[6].x  := xcenter - x2;        P[6].y  := RoundtoInt(BmTpLH * 19 + Top);
    P[27].x := xcenter + x2;        P[27].y := P[6].y;
    x2      := RoundtoInt(RmLpLW * 24);
    P[7].x  := xcenter - x2;        P[7].y  := RoundtoInt(BmTpLH * 17 + Top);
    P[26].x := xcenter + x2;        P[26].y := P[7].y;
    x2      := RoundtoInt(RmLpLW * 22);
    P[8].x  := xcenter - x2;        P[8].y  := RoundtoInt(BmTpLH * 26 + Top);
    P[25].x := xcenter + x2;        P[25].y := P[8].y;
    x2      := RoundtoInt(RmLpLW * 25);
    P[9].x  := xcenter - x2;        P[9].y  := RoundtoInt(BmTpLH * 28 + Top);
    P[24].x := xcenter + x2;        P[24].y := P[9].y;
    x2      := RoundtoInt(RmLpLW * 14);
    P[10].x := xcenter - x2;        P[10].y := RoundtoInt(BmTpLH * 38 + Top);
    P[23].x := xcenter + x2;        P[23].y := P[10].y;
    x2      := RoundtoInt(RmLpLW * 15);
    P[11].x := xcenter - x2;        P[11].y := RoundtoInt(BmTpLH * 43 + Top);
    P[22].x := xcenter + x2;        P[22].y := P[11].y;
    x2      := RoundtoInt(RmLpLW);
    P[12].x := xcenter - x2;        P[12].y := RoundtoInt(BmTpLH * 41 + Top);
    P[21].x := xcenter + x2;        P[21].y := P[12].y;
    x2      := RoundtoInt(RmLpLW / 2);
    P[13].x := xcenter - x2;        P[13].y := RoundtoInt(BmTpLH * 42 + Top);
    P[20].x := xcenter + x2;        P[20].y := P[13].y;
    P[14].x := P[13].x;             P[14].y := RoundtoInt(BmTpLH * 47 + Top);
    P[19].x := P[20].x;             P[19].y := P[14].y;
    x2      := RoundtoInt(RmLpLW);
    P[15].x := xcenter - x2;        P[15].y := P[14].y;
    P[18].x := xcenter + x2;        P[18].y := P[14].y;
    P[16].x := P[15].x;             P[16].y := bottom;
    P[17].x := P[18].x;             P[17].y := bottom;
  end;
  NumPts := 33;
end;

procedure InitChevronArrow(var P: array of TPoint; const R: TRect; var NumPts: integer);
var
  dx: integer;
begin
  with R do
  begin
    dx     := (Right - Left) div 3;
    P[0].x := Left;                 P[0].y := Top;
    P[1].x := Right - dx;           P[1].y := Top;
    P[2].x := Right;                P[2].y := (Top + Bottom) div 2;
    P[3].x := P[1].x;               P[3].y := Bottom;
    P[4].x := Left;                 P[4].y := Bottom;
    P[5].x := Left + dx;            P[5].y := P[2].y;
  end;
  NumPts := 6;
end;

procedure InitFivePointStar(var P: array of TPoint; const R: TRect; var NumPts: integer);
begin
  CalculatePentagonPoints(R, P[0], P[2], P[4], P[6], P[8]);
  P[1]   := LinesPointOfIntersection(P[0], P[4], P[2], P[8]);
  P[3]   := LinesPointOfIntersection(P[0], P[4], P[2], P[6]);
  P[5]   := LinesPointOfIntersection(P[8], P[4], P[2], P[6]);
  P[7]   := LinesPointOfIntersection(P[8], P[4], P[0], P[6]);
  P[9]   := LinesPointOfIntersection(P[8], P[2], P[0], P[6]);
  NumPts := 10;
end;

procedure InitHexagon(var P: array of TPoint; const R: TRect; var NumPts: integer);
var
  dx: integer;
begin
  with R do
  begin
    dx     := round(((Right - Left) / 2 * cos(DegToRad(15))) / 2);
    P[0].x := Left + dx;            P[0].y := Top;
    P[1].x := Left;                 P[1].y := (Top + Bottom) div 2;
    P[2].x := P[0].x;               P[2].y := Bottom;
    P[3].x := Right - dx;           P[3].y := Bottom;
    P[4].x := Right;                P[4].y := P[1].y;
    P[5].x := Right - dx;           P[5].y := Top;
  end;
  NumPts := 6;
end;

procedure InitNotchedArrow(var P: array of TPoint; const R: TRect; var NumPts: integer);
begin
  InitBoldArrow(P, R, NumPts);
  with R do
  begin
    P[7].x := Left + (Right - Left) div 4;
    P[7].y := P[3].y; // centerpoint y
  end;
  NumPts := 8;
end;

procedure InitOctogon(var P: array of TPoint; const R: TRect; var NumPts: integer);
var
  dx, dy: integer;
begin
  with R do
  begin
    dx     := Right - Left;
    dx     := round((dx - dx / (sqrt(2) + 1)) / 2);
    dy     := Bottom - Top;
    dy     := round((dy - dy / (sqrt(2) + 1)) / 2);
    P[0].x := Left + dx;            P[0].y := Top;
    P[1].x := Right - dx;           P[1].y := Top;
    P[2].x := Right;                P[2].y := Top + dy;
    P[3].x := Right;                P[3].y := Bottom - dy;
    P[4].x := P[1].x;               P[4].y := Bottom;
    P[5].x := P[0].x;               P[5].y := Bottom;
    P[6].x := Left;                 P[6].y := P[3].y;
    P[7].x := Left;                 P[7].y := P[2].y;
  end;
  NumPts := 8;
end;

procedure InitPentagon(var P: array of TPoint; const R: TRect; var NumPts: integer);
begin
  CalculatePentagonPoints(R, P[0], P[1], P[2], P[3], P[4]);
  NumPts := 5;
end;

procedure InitPlus(var P: array of TPoint; const R: TRect; var NumPts: integer);
var
  CrossX, Crossy: integer;
begin
  with R do
  begin
    CrossX  := (Right - Left) div 3;
    CrossY  := (Bottom - Top) div 3;
    P[0].x  := Left;                P[0].y  := Top + CrossY;
    P[1].x  := Left + CrossX;       P[1].y  := P[0].y;
    P[2].x  := P[1].x;              P[2].y  := Top;
    P[3].x  := Right - CrossX;      P[3].y  := P[2].y;
    P[4].x  := P[3].x;              P[4].y  := P[0].y;
    P[5].x  := Right;               P[5].y  := P[4].y;
    P[6].x  := P[5].x;              P[6].y  := Bottom - CrossY;
    P[7].x  := P[3].x;              P[7].y  := P[6].y;
    P[8].x  := P[7].x;              P[8].y  := Bottom;
    P[9].x  := P[1].x;              P[9].y  := P[8].y;
    P[10].x := P[9].x;              P[10].y := P[6].y;
    P[11].x := Left;                P[11].y := P[10].y;
    P[12].x := P[11].x;             P[12].y := P[0].y;
  end;
  NumPts := 13;
end;

procedure InitQuadrangle(var P: array of TPoint; const R: TRect; var NumPts: integer);
begin
  with R do
  begin
    P[0].x := Left;                 P[0].y := Top;
    P[1].x := Left;                 P[1].y := Bottom;
    P[2].x := Right;                P[2].y := Bottom;
    P[3].x := Right;                P[3].y := Top;
  end;
  NumPts := 4;
end;

procedure InitRightTriangle(var P: array of TPoint; const R: TRect; var NumPts: integer);
begin
  with R do
  begin
    P[0].x := Left;                 P[0].y := Top;
    P[1].x := Right;                P[1].y := Bottom;
    P[2].x := P[0].x;               P[2].y := Bottom;
  end;
  NumPts := 3;
end;

procedure InitSwastika(var P: array of TPoint; const R: TRect; var NumPts: integer);
var
  x1, x2, y1, y2: integer;
begin
  with r do
  begin
    x1      := (Right - Left) div 5;
    y1      := (Bottom - Top) div 5;
    x2      := (Right - Left) * 2 div 5;
    y2      := (Bottom - Top) * 2 div 5;
    P[0].x  := Left;                P[0].y  := Top;
    P[1].x  := Left + x1;           P[1].y  := Top;
    P[2].x  := P[1].x;              P[2].y  := Top + y2;
    P[3].x  := Left + x2;           P[3].y  := P[2].y;
    P[4].x  := P[3].x;              P[4].y  := Top;
    P[5].x  := Right;               P[5].y  := P[4].y;
    P[6].x  := P[5].x;              P[6].y  := Top + y1;
    P[7].x  := Right - x2;          P[7].y  := P[6].y;
    P[8].x  := P[7].x;              P[8].y  := p[2].y;
    P[9].x  := Right;               P[9].y  := P[8].y;
    P[10].x := P[9].x;              P[10].y := Bottom;
    P[11].x := Right - x1;          P[11].y := P[10].y;
    P[12].x := P[11].x;             P[12].y := Bottom - y2;
    P[13].x := P[7].x;              P[13].y := P[12].y;
    P[14].x := P[13].x;             P[14].y := Bottom;
    P[15].x := Left;                P[15].y := P[14].y;
    P[16].x := P[15].x;             P[16].y := Bottom - y1;
    P[17].x := Left + x2;           P[17].y := P[16].y;
    P[18].x := P[17].x;             P[18].y := Bottom - y2;
    P[19].x := Left;                P[19].y := P[18].y;
  end;
  NumPts := 20;
end;

procedure InitTriangle(var P: array of TPoint; const R: TRect; var NumPts: integer);
begin
  with R do
  begin
    P[0].x := Left;
    P[0].y := Top;
    P[1].x := Right;
    P[1].y := Top + (Bottom - Top) div 2;
    P[2].x := P[0].x;
    P[2].y := Bottom;
  end;
  NumPts := 3;
end;

procedure InitValve(var P: array of TPoint; const R: TRect; var NumPts: integer);
var
  cntPoint: TPoint;
begin
  cntPoint := CenterPoint(R);
  with R do
  begin
    P[0].x := Left;                 P[0].y := Top;
    P[1].x := cntPoint.x;           P[1].y := cntPoint.y;
    P[2].x := Right;                P[2].y := Top;
    P[3].x := Right;                P[3].y := Bottom;
    P[4].x := cntPoint.x;           P[4].y := cntPoint.y;
    P[5].x := Left;                 P[5].y := Bottom;
  end;
  NumPts := 6;
end;

procedure InitVArrow(var P: array of TPoint; const R: TRect; var NumPts: integer);
var
  cntPoint: TPoint;
begin
  cntPoint := CenterPoint(R);
  with R do
  begin
    P[0].x := Left;                 P[0].y := Top;
    P[1].x := Right;                P[1].y := cntPoint.y;
    P[2].x := Left;                 P[2].y := Bottom;
    P[3].x := cntPoint.x;           P[3].y := cntPoint.y;
  end;
  NumPts := 4;
end;

procedure Paint2HeadArrow(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @Init2HeadArrow);
end;

procedure PaintBarbadosTrident(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitBarbadosTrident);
end;

procedure PaintBigI(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitBigI);
end;

procedure PaintBoldArrow(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitBoldArrow);
end;

procedure PaintCanadianMaple(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitCanadianMaple);
end;

procedure PaintChevronArrow(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitChevronArrow);
end;

procedure PaintFivePointStar(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitFivePointStar);
end;

procedure PaintHexagon(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitHexagon);
end;

procedure PaintNotchedArrow(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitNotchedArrow);
end;

procedure PaintOctogon(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitOctogon);
end;

procedure PaintPentagon(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitPentagon);
end;

procedure PaintPlus(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitPlus);
end;

procedure PaintQuadrangle(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitQuadrangle);
end;

procedure PaintRightTriangle(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitRightTriangle);
end;

procedure PaintSwastika(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitSwastika);
end;

procedure PaintTriangle(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitTriangle);
end;

procedure PaintValve(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitValve);
end;

procedure PaintVArrow(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitVArrow);
end;

procedure PaintTriangular(Canvas: TfpgCanvas; const PaintRect: TRect; RadAngle: extended = 0.0; RightLeftFactor: extended = 0.5);
var
  PR, vPR: TRect;
  P: array[0..35] of TPoint;
  CountPts, hv, wv: integer;
  cntPoint: TPoint;
begin
  PR       := PaintRect;
  cntPoint := CenterPoint(PR);
  PolycSetHalfWidthAndHeight(PR, hv, wv, RadAngle);
  PolycNewPaintRect(vPR, cntPoint, wv, hv);

  with vPR do
  begin
    P[0].x := Left;
    P[0].y := Bottom;
    P[1].x := Left + round((Right - left) * RightLeftFactor);
    P[1].y := Top;
    P[2].x := Right;
    P[2].y := Bottom;
  end;
  CountPts := 3;

  PaintPolygon(Canvas, PR, RadAngle, P, CountPts, cntPoint);
end;

procedure PaintHalfEllipse(Canvas: TfpgCanvas; const PaintRect: TRect; AHalfEllipseDirection: TShapeDirection);
var
  Ex1, Ex2, Ey1, Ey2, Sx, Sy, Ex, Ey, i: integer;
begin
  case AHalfEllipseDirection of
    atUp:
      with PaintRect do
      begin
        Ex1 := Left;
        Ex2 := Right;
        Ex  := Left;
        Sx  := Right;
        i   := Bottom - Top;
        Ey1 := Top;
        Ey2 := Bottom + i;
        Sy  := Top + i;
        Ey  := Top + i;
      end;

    atDown:
      with PaintRect do
      begin
        Ex1 := Left;
        Ex2 := Right;
        Sx  := Left;
        Ex  := Right;
        i   := Bottom - Top;
        Ey1 := Top - i;
        Ey2 := Bottom;
        Sy  := Top;
        Ey  := Top;
      end;

    atRight:
      with PaintRect do
      begin
        Ey1 := Top;
        Ey2 := Bottom;
        Ey  := Top;
        Sy  := Bottom;
        i   := Right - Left;
        Ex1 := Left - i;
        Ex2 := Right;
        Sx  := Left;
        Ex  := Left;
      end;

    atLeft:
      with PaintRect do
      begin
        Ey1 := Top;
        Ey2 := Bottom;
        Sy  := Top;
        Ey  := Bottom;
        i   := Right - Left;
        Ex1 := Left;
        Ex2 := Right + i;
        Sx  := Left + i;
        Ex  := Left + i;
      end;
  end;

  { TODO : Implement Canvas.DrawPie() }
  raise Exception.Create('Canvas.DrawPie() is not implemented yet');
//   Canvas.DrawPie(Ex1,Ey1,Ex2,Ey2,Sx,Sy,Ex,Ey);
end;

procedure PaintFivePointLineStar(Canvas: TfpgCanvas; const PaintRect: TRect);
var
  P: array[0..4] of TPoint;
begin
  CalculatePentagonPoints(PaintRect, P[0], P[1], P[2], P[3], P[4]);
  Canvas.DrawLine(P[0].x, P[0].y, P[2].x, P[2].y);
  Canvas.DrawLine(P[0].x, P[0].y, P[3].x, P[3].y);
  Canvas.DrawLine(P[1].x, P[1].y, P[3].x, P[3].y);
  Canvas.DrawLine(P[1].x, P[1].y, P[4].x, P[4].y);
  Canvas.DrawLine(P[2].x, P[2].y, P[4].x, P[4].y);
end;

procedure PaintStarN(Canvas: TfpgCanvas; cx, cy, r, n, a: integer);
const
  MaxStarPoint = 36;
var
  r1, r0, alpha: double;
  P: array[0..MaxStarPoint * 2 - 1] of TPoint;
  i, cs: integer;
begin
  r1 := r / 2;
  for i := 0 to 2 * n do
  begin
    if (i mod 2) = 0 then
      r0 := r
    else
      r0 := r1;
    alpha := a + (0.5 + i / n) * Pi;
    cs := RoundToInt(r0 * cos(alpha));
    P[i].x := cx + cs;
    P[i].y := cy - Round(r0 * sin(alpha));
  end;
  for i := 2 * n to MaxStarPoint * 2 - 1 do
  begin
    P[i].x := P[2 * n - 1].x;
    P[i].y := P[2 * n - 1].y;
  end;
  Canvas.DrawPolygon(P);
end;

end.

