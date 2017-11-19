{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Canvas helper class for fpReport. Used by HTML and FPImage exporters.[B

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportcanvashelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpimage, fpreport, fpcanvas;

Type

  { TFPReportCanvasHelper }

  TFPReportCanvasHelper = Class(TObject)
  private
    FCanvas: TFPCustomCanvas;
    FDPI: Integer;
    procedure RenderShapeCircle(const lpt1: TFPReportPoint; const ALayout: TFPReportLayout);
    procedure RenderShapeEllipse(const lpt1: TFPReportPoint; const ALayout: TFPReportLayout);
    procedure RenderShapeLine(lpt1: TFPReportPoint; const AOrientation: TFPReportOrientation; const ALayout: TFPReportLayout);
    procedure RenderShapeRect(const lpt1: TFPReportPoint; const ALayout: TFPReportLayout);
    procedure RenderShapeTriangle(Alpt: TFPReportPoint; const AOrientation: TFPReportOrientation; const ALayout: TFPReportLayout);
  public
    constructor Create(aCanvas : TFPCustomCanvas; ADPI : Integer);
    procedure RenderImage(aRect: TFPReportRect; var AImage: TFPCustomImage); overload;
    Procedure RenderShape(const lpt1: TFPReportPoint; const AShape: TFPReportCustomShape);
    procedure RenderImage(const lpt1: TFPReportPoint; const AImage: TFPReportCustomImage);overload;
    procedure RenderCheckbox(const lPt: TFPReportPoint; const ACheckbox: TFPReportCustomCheckbox);
    function CoordToPoint(const APos: TFPReportPoint; const AHOffset: TFPReportUnits = 0; const AVOffset: TFPReportUnits = 0): TPoint;
    function CoordToRect(const APos: TFPReportPoint; const AWidth: TFPReportUnits = 0; const AHeight: TFPReportUnits = 0): TRect;
    function mmToPixels(const AValue: TFPReportUnits): Integer;
    class function ColorToRGBTriple(const AColor: UInt32): TFPColor;
    property Canvas : TFPCustomCanvas Read FCanvas Write FCanvas;
    property DPI : Integer Read FDPI Write FDPI;
  end;

const
  cInchToMM = 25.4;

implementation

uses math;

Type
  TReportImageFriend = class(TFPReportCustomImage);


function GetColorComponent(Var AColor: UInt32): Word;
begin
  Result:=AColor and $FF;
  Result:=Result or (Result shl 8);
  AColor:=AColor shr 8;
end;

function TFPReportCanvasHelper.mmToPixels(const AValue: TFPReportUnits): Integer;
begin
  Result := Round(AValue * (DPI / cInchToMM));
end;

function TFPReportCanvasHelper.CoordToRect(const APos: TFPReportPoint;
  const AWidth: TFPReportUnits = 0; const AHeight: TFPReportUnits = 0): TRect;

begin
  Result.Left:=mmToPixels(APos.Left);
  Result.Top:=mmToPixels(APos.Top);
  Result.Right:=mmToPixels(APos.Left+AWidth)-1;
  Result.Bottom:=mmToPixels(APos.Top+AHeight)-1;
end;

function TFPReportCanvasHelper.CoordToPoint(const APos: TFPReportPoint;
  const AHOffset: TFPReportUnits; const AVOffset: TFPReportUnits): TPoint;

begin
  Result.X:=mmToPixels(APos.Left+AHOffset);
  Result.Y:=mmToPixels(APos.Top+AVOffset);
end;

class function TFPReportCanvasHelper.ColorToRGBTriple(const AColor: UInt32): TFPColor;

Var
  C : UInt32;

begin
  C:=AColor;
  with Result do
    begin
    Blue  := GetColorComponent(C);
    Green := GetColorComponent(C);
    Red   := GetColorComponent(C);
    Alpha := GetColorComponent(C);
    end
end;

constructor TFPReportCanvasHelper.Create(aCanvas: TFPCustomCanvas; ADPI: Integer
  );
begin
  FCanvas:=ACanvas;
  FDPI:=ADPI;
end;

procedure TFPReportCanvasHelper.RenderImage(aRect: TFPReportRect;
  var AImage: TFPCustomImage);
Var
  lpt : TFPReportPoint;
  pt : TPoint;

begin
  lPt.Left := aRect.Left;
  lPt.Top := aRect.Top;
  PT:=CoordToPoint(Lpt,0,0);
//  Canvas.StretchDraw(pt.X,pT.Y,mmToPixels(arect.Width), mmToPixels(arect.Height),AImage);
  Canvas.Draw(pt.X,pT.Y,AImage);
end;

procedure TFPReportCanvasHelper.RenderShape(const lpt1: TFPReportPoint; const AShape: TFPReportCustomShape);

begin
  Canvas.Pen.FPColor:=ColorToRGBTriple(TFPReportShape(AShape).Color);
  Canvas.Pen.Style:=psSolid;
  Canvas.Pen.Width:=1;
  { exit if Shape will not be visible. }
  if (TFPReportShape(AShape).Color = clNone)
  or (TFPReportShape(AShape).Color = TFPReportShape(AShape).Frame.BackgroundColor) then
    exit;
  case TFPReportShape(AShape).ShapeType of
    stEllipse: RenderShapeEllipse(lpt1,AShape.RTLayout);
    stCircle: RenderShapeCircle(lpt1,AShape.RTLayout);
    stLine: RenderShapeLine(lpt1,TFPReportShape(AShape).Orientation, AShape.RTLayout);
    stSquare: RenderShapeRect(lpt1,AShape.RTLayout);
    stTriangle: RenderShapeTriangle(lpt1,TFPReportShape(AShape).Orientation, AShape.RTLayout);
  end;
end;

procedure TFPReportCanvasHelper.RenderCheckbox(const lPt: TFPReportPoint; const ACheckbox: TFPReportCustomCheckbox);
var
  pt : TPoint;
  lImage: TFPCustomImage;

begin
  Pt:=CoordToPoint(lpt,0,0);
  lImage:=ACheckBox.GetRTImage;
  Canvas.StretchDraw(pt.X,Pt.Y,mmToPixels(ACheckBox.RTLayout.Width), mmToPixels(ACheckBox.RTLayout.Height),limage);
end;

procedure TFPReportCanvasHelper.RenderImage(const lpt1: TFPReportPoint;
  const AImage: TFPReportCustomImage);
Var
  PT : TPoint;
  img: TReportImageFriend;

begin
  img := TReportImageFriend(AImage);  { for access to Protected methods }
  if not Assigned(img.Image) then
    Exit; { nothing further to do }
  PT:=CoordToPoint(Lpt1,0,0);
  if img.Stretched then
    Canvas.StretchDraw(pt.X,pT.Y,mmToPixels(AImage.RTLayout.Width), mmToPixels(AImage.RTLayout.Height),Img.Image)
  else
    Canvas.Draw(pt.X,pT.Y,Img.Image);
end;

procedure TFPReportCanvasHelper.RenderShapeCircle(const lpt1: TFPReportPoint;
  const ALayout: TFPReportLayout);

var
  lPt2: TFPReportPoint;  // original Report point
  R : TRect;
  LW : TFPReportUnits;

begin
  // Keep center of circle at center of rectangle
  lw := Min(ALayout.Width, ALayout.Height);
  lpt2.Left:=lPt1.Left+(ALayout.Width / 2)-lW/2;
  lpt2.Top:=lPt1.top+(ALayout.Height / 2)-lW/2;
  R:=CoordToRect(lpt2,LW,LW);
  Canvas.ellipse(R);
end;

procedure TFPReportCanvasHelper.RenderShapeEllipse(const lpt1: TFPReportPoint;
  const ALayout: TFPReportLayout);

Var
  R : TRect;
begin
  R:=CoordToRect(lpt1,ALayout.Width,ALayout.Height);
  Canvas.ellipse(R);
end;

procedure TFPReportCanvasHelper.RenderShapeLine(lpt1: TFPReportPoint;
  const AOrientation: TFPReportOrientation; const ALayout: TFPReportLayout);

var
  lPt2: TFPReportPoint;  // original Report point
  R1,R2 : TPoint;

begin
  case AOrientation of
  orNorth, orSouth:
     begin                                         //   |
     lPt1.Left := lPt1.Left + (ALayout.Width / 2); //   |
     lPt2.Left := lPt1.Left ;                      //   |
     lPt2.Top := LPT1.Top + ALayout.Height;        //   |
     end;
  orNorthEast, orSouthWest:
     begin                                         //    /
     lPt2.Left := lPt1.Left;                       //   /
     lPt1.Left := lPt1.Left + ALayout.Width;       //  /
     lPt2.Top := lPt1.Top + ALayout.Height;        // /
     end;
  orEast, orWest:
     begin                                         //
     lPt2.Left := lPt1.Left + ALayout.Width;       // ----
     lPt1.Top := lPt1.Top + (ALayout.Height / 2);  //
     lPt2.Top := lPt1.Top;                         //
     end;
  orSouthEast, orNorthWest:
     begin                                         // \
     lPt2.Left := lPt1.Left + ALayout.Width;       //  \
     lPt2.Top := lPt1.Top + ALayout.Height;        //   \
     end;                                          //    \
  end;
  R1:=CoordToPoint(lpt1);
  R2:=CoordToPoint(lpt2);
  Canvas.line(R1,R2);
end;

procedure TFPReportCanvasHelper.RenderShapeRect(const lpt1: TFPReportPoint;
  const ALayout: TFPReportLayout);

Var
  ldx, ldy, lw: TFPReportUnits;
  P : TFPReportPoint;
begin
  lw := Min(ALayout.Width, ALayout.Height);
  if ALayout.Width = ALayout.Height then
  begin
    ldx := 0;
    ldy := 0;
  end
  else if ALayout.Width > ALayout.Height then
  begin
    ldx := (ALayout.Width - ALayout.Height) / 2;
    ldy := 0;
  end
  else if ALayout.Width < ALayout.Height then
  begin
    ldx := 0;
    ldy := (ALayout.Height - ALayout.Width) / 2;
  end;
  P.Left := lPt1.Left + ldx;
  { PDF origin coordinate is Bottom-Left, and Report Layout is Top-Left }
  P.Top := lPt1.Top + ldy;
  Canvas.rectangle(CoordToRect(P,lw,Lw));
end;

procedure TFPReportCanvasHelper.RenderShapeTriangle(Alpt: TFPReportPoint;
  const AOrientation: TFPReportOrientation; const ALayout: TFPReportLayout);


  Procedure DrawLine(Const A,B : TFPReportPoint);

  begin
    Canvas.Line(CoordToPoint(A),CoordToPoint(B));
  end;

var
  lpt1,lPt2,lpt3: TFPReportPoint;  // original Report points for 3 corners of triangle.

begin
  case AOrientation of
  orNorth:
    begin
    lPt1.Left := ALPT.Left + (ALayout.Width / 2); //      1
    lPt1.Top  := ALPT.Top;                        //      /\
    lPt2.Left := ALPT.Left;                       //     /  \
    lPt2.Top  := ALPT.Top + ALayout.Height;       //    /____\
    lPt3.Left := ALPT.Left + ALayout.Width;       //  2       3
    lPt3.Top  := lPt2.Top;
    end;
  orNorthEast:
    begin
    lPt1.Left := ALPT.Left + (ALayout.Width );    //   +-------1
    lPt1.Top  := ALPT.Top;                        //   |       |
    lPt2.Left := ALPT.Left;                       //   2       |
    lPt2.Top  := ALPT.Top + ALayout.Height/2;     //   |       |
    lPt3.Left := ALPT.Left + ALayout.Width/2;     //   +---3---+
    lPt3.Top  := lPt1.Top + aLayout.height;
    end;
  orSouth:
    begin
    lPt1.Left := ALPT.Left;                        //  1 ------ 2
    lPt1.Top  := ALPT.Top;                         //    \    /
    lPt2.Left := ALPT.Left+ ALayout.Width;         //     \  /
    lPt2.Top  := ALPT.Top;                         //      \/
    lPt3.Left := ALPT.Left + (ALayout.Width / 2);  //      3
    lPt3.Top  := ALPT.Top+ALayout.Height;
    end;
  orSouthEast:
    begin
    lPt1.Left := ALPT.Left + (ALayout.Width/2);   //   +---1---+
    lPt1.Top  := ALPT.Top;                        //   |       |
    lPt2.Left := ALPT.Left;                       //   2       |
    lPt2.Top  := ALPT.Top + ALayout.Height/2;     //   |       |
    lPt3.Left := ALPT.Left + ALayout.Width;       //   +-------3
    lPt3.Top  := lPt1.Top + aLayout.height;
    end;
  orEast:
    begin
    lPt1.Left := ALPT.Left;                       //   1
    lPt1.Top  := Alpt.Top ;                       //   |\
    lPt2.Left := ALPT.Left + ALayout.Width;       //   | \ 2
    lPt2.Top  := ALPT.Top + (ALayout.Height / 2); //   | /
    lPt3.Left := ALPT.Left;                       //   |/
    lPt3.Top  := Alpt.Top + ALayout.Height;       //   3
    end;
  orNorthWest:
    begin
    lPt1.Left := ALPT.Left;                       //   1-------+
    lPt1.Top  := ALPT.Top;                        //   |       |
    lPt2.Left := ALPT.Left+ALayout.width;         //   |       2
    lPt2.Top  := ALPT.Top + ALayout.Height/2;     //   |       |
    lPt3.Left := ALPT.Left + ALayout.Width/2;     //   +---3---+
    lPt3.Top  := lPt1.Top + aLayout.height;
    end;
  orWest:
    begin
    lPt1.Left := ALPT.Left + ALayout.Width;      //       1
    lPt1.Top  := ALPT.Top;                       //      /|
    lPt2.Left := ALPT.Left;                      //   2 / |
    lPt2.Top  := ALPT.Top + ALayout.Height / 2;  //     \ |
    lPt3.Left := ALPT.Left + ALayout.Width;      //      \|
    lPt3.Top  := ALPT.Top+ ALayout.Height;       //       3
    end;
  orSouthWest:
    begin
    lPt1.Left := ALPT.Left+ ALayout.Height/2;     //   +---1---+
    lPt1.Top  := ALPT.Top;                        //   |       |
    lPt2.Left := ALPT.Left+ALayout.width;         //   |       2
    lPt2.Top  := ALPT.Top + ALayout.Height/2;     //   |       |
    lPt3.Left := ALPT.Left ;                      //   3-------+
    lPt3.Top  := lPt1.Top + aLayout.height;
    end;
  end;
  DrawLine(lpt1,lpt2);
  DrawLine(lpt2,lpt3);
  DrawLine(lpt3,lpt1);
end;

end.

