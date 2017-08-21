unit reportpolygon;

{$mode objfpc}{$H+}

{$DEFINE NATIVERENDERER}
{ $DEFINE EXPORTFPIMAGE}
{ $DEFINE EXPORTPDF}
{ $DEFINE EXPORTLCL}
{ $DEFINE EXPORTAGGPAS}
{ $DEFINE EXPORTFPGUI}

interface

uses
  Classes,  fpReport, fpReportStreamer;

 Type

   { TReportPolygon }

   TReportPolygon = class(TFPReportElement)
   private
     FColor: TFPReportColor;
     FCorners: Cardinal;
     FLineWidth: Cardinal;
     FRotateAngle: Double;
     procedure SetCLineWidth(AValue: Cardinal);
     procedure SetCorners(AValue: Cardinal);
   Protected
     Procedure   RecalcLayout; override;
     procedure DoWriteLocalProperties(AWriter: TFPReportStreamer; AOriginal: TFPReportElement = nil); override;
   public
     procedure ReadElement(AReader: TFPReportStreamer); override;
     Procedure Assign(Source: TPersistent); override;
   Published
     Property Corners : Cardinal Read FCorners Write SetCorners;
     Property LineWidth : Cardinal Read FLineWidth Write SetCLineWidth;
     Property Color : TFPReportColor Read FColor Write FColor;
     // In degrees
     Property RotateAngle : Double Read FRotateAngle Write FRotateAngle;
   end;

implementation

{$IF DEFINED(EXPORTFPGUI) or DEFINED(EXPORTAGGPAS)}
{$DEFINE NEEDAGGPAS}
{$ENDIF}

uses
{$IFDEF EXPORTPDF}
   fppdf,
   fpreportpdfexport,
{$ENDIF}
{$IFDEF EXPORTLCL}
   graphics,
   fpreportlclexport,
{$ENDIF}
{$IFDEF EXPORTFPIMAGE}
   fpreportfpimageexport,
{$ENDIF}
{$IFDEF EXPORTFPGUI}
  fpreport_export_fpgui,
{$ENDIF}
{$IFDEF NEEDAGGPAS}
  fpreportaggpasexport,
  agg_2D,
{$ENDIF}
   sysutils, fpimage, fpcanvas, FPImgCanv;

// Angle in degrees.

{$IFDEF EXPORTLCL}
Procedure LCLPaintPolygon(Canvas : TCanvas; ARect : Trect; ANumber : Integer; AStartAngle : Double; ALineWidth : Integer; AColor : TColor);

Var
  CX,CY,R,I : Integer;
  P : Array of TPoint;
  A,Step : Double;
  AWidth,AHeight : Integer;

begin
  AWidth:=ARect.Right-ARect.Left+1;
  AHeight:=ARect.Bottom-ARect.Top+1;
  Canvas.Pen.Color:=AColor;
  Canvas.Pen.style:=psSolid;
  Canvas.Pen.Width:=aLineWidth;
  if ANumber<3 then
    exit;
  CX:=ARect.Left+AWidth div 2;
  CY:=ARect.Top+AHeight div 2;
  if aWidth<aHeight then
    R:=AWidth div 2
  else
    R:=AHeight div 2;
  SetLength(P,ANumber);
  A:=AStartAngle;
  Step:=(2*Pi)/ANumber;
  For I:=0 to ANumber-1 do
    begin
    P[i].X:=CX+Round(R*Cos(a));
    P[i].Y:=CY-Round(R*Sin(a));
    A:=A+Step;
    end;
  For I:=0 to ANumber-2 do
    Canvas.Line(P[I],P[I+1]);
  Canvas.Line(P[ANumber-1],P[0]);
end;

Procedure RenderPolygonInLCL(AOffset : TFPReportPoint; E: TFPReportElement; RE : TFPReportExporter; ADPI : Integer);
var
  PR : TFPReportExportCanvas;
  PG : TReportPolygon;
  R :  Trect;
  rPt : TFPReportPoint;
  pt : TPoint;

begin
  PR:=RE as TFPReportExportCanvas;
  PG:=E as TReportPolygon;
  rpt.Left:=AOffset.Left+E.RTLayout.Left;
  rpt.Top:=AOffset.Top+E.RTLayout.top;
  Pt:=PR.CoordToPoint(rpt,0,0);
  R.TopLeft:=pt;
  R.Right:=R.Left+PR.HmmToPixels(E.RTLayout.Width);
  R.Bottom:= R.Top+PR.VmmToPixels(E.RTLayout.Height);
  PR.Canvas.Brush.Color:=e.Frame.BackgroundColor;
  PR.Canvas.Brush.Style:=bsSolid;
  PR.Canvas.FillRect(R);
  LCLPaintPolygon(PR.Canvas,R,PG.Corners,PG.RotateAngle,PG.LineWidth,PR.RGBToBGR(PG.Color));
end;
{$ENDIF}

{$IFDEF EXPORTPDF}
Procedure RenderPolygonInPDF(AOffset : TFPReportPoint; E: TFPReportElement; RE : TFPReportExporter; ADPI : Integer);

Var
  PR : TFPReportExportPDF;
  PG : TReportPolygon;
  C : TPDFCoord;
  I,ANumber : Integer;
  P : Array of TPDFCoord;
  R,A,Step : Double;
  APage: TPDFPage;

begin
  PR:=RE as TFPReportExportPDF;
  PG:=E as TReportPolygon;
  APage:=PR.CurrentPage;
  ANumber:=PG.Corners;
  if ANumber<3 then
    exit;
  C.X:=AOffset.Left+E.RTLayout.Left+E.RTLayout.Width / 2;
  C.Y:=AOffset.Top+E.RTLayout.Top+E.RTLayout.Height / 2;
  if E.RTLayout.Width<E.RTLayout.Height then
    R:=E.RTLayout.Width / 2
  else
    R:=E.RTLayout.Height / 2;
  SetLength(P,ANumber);
  A:=PG.RotateAngle;
  Step:=(2*Pi)/ANumber;
  For I:=0 to ANumber-1 do
    begin
    P[i].X:=C.X+R*Cos(a);
    P[i].Y:=C.Y-R*Sin(a);
    A:=A+Step;
    end;
  APage.SetColor(PG.Color,True);
//  For I:=0 to ANumber-2 do
//    APage.DrawLine(P[I],P[I+1],PG.LineWidth,True);
//  aPage.DrawLine(P[ANumber-1],P[0],PG.LineWidth,True);
  APage.DrawPolyGon(P,PG.LineWidth);
  APage.StrokePath;
end;
{$ENDIF}

function GetColorComponent(Var AColor: UInt32): Word;
begin
  Result:=AColor and $FF;
  Result:=Result or (Result shl 8);
  AColor:=AColor shr 8;
end;

function ColorToRGBTriple(const AColor: UInt32): TFPColor;

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


Procedure PaintPolygon(Canvas : TFPCustomCanvas; AOffset : TPoint; AWidth,AHeight : Integer; ANumber : Integer; AStartAngle : Double; ALineWidth : Integer; AColor : TFPColor);

Var
  CX,CY,R,I : Integer;
  P : Array of TPoint;
  A,Step : Double;

begin
  Canvas.Pen.FPColor:=AColor;
  Canvas.Pen.Width:=aLineWidth;
  Canvas.Pen.Style:=psSolid;
  if ANumber<3 then
    exit;
  CX:=AOffset.x+AWidth div 2;
  CY:=AOffset.y+AHeight div 2;
  if aWidth<aHeight then
    R:=AWidth div 2
  else
    R:=AHeight div 2;
  SetLength(P,ANumber);
  A:=AStartAngle;
  Step:=(2*Pi)/ANumber;
  For I:=0 to ANumber-1 do
    begin
    P[i].X:=CX+Round(R*Cos(a));
    P[i].Y:=CY-Round(R*Sin(a));
    A:=A+Step;
    end;
  For I:=0 to ANumber-2 do
    Canvas.Line(P[I],P[I+1]);
  Canvas.Line(P[ANumber-1],P[0]);
  SetLength(P,0);
end;

Procedure RenderPolygonToImage(aElement : TFPReportElement;AImage : TFPCustomImage);

Var
  C : TFPImageCanvas;
  P : TReportPolygon;

begin
  P:=AElement as TReportPolygon;
  C:=TFPImageCanvas.Create(AImage);
  try
    if (AElement.Frame.BackgroundColor<>fpreport.clNone) then
      begin
      C.Brush.FPColor:=ColorToRGBTriple(AElement.Frame.BackgroundColor);
      C.Brush.Style:=bsSolid;
      C.FillRect(0,0,AImage.Width-1,AImage.Height-1);
      end;
    PaintPolygon(C,Point(0,0),AImage.Width,AImage.Height,P.Corners,P.RotateAngle,P.linewidth,ColorToRGBTriple(P.Color));
  finally
    C.Free;
  end;
end;

{$IFDEF EXPORTFPIMAGE}
Procedure RenderPolygonInFPImage(AOffset : TFPReportPoint; E: TFPReportElement; RE : TFPReportExporter; ADPI : Integer);

Var
  PR : TFPReportExportFPImage;
  PG : TReportPolygon;
  R :  TPoint;
  AWidth,AHeight : Integer;

begin
  PR:=RE as TFPReportExportFPImage;
  PG:=E as TReportPolygon;
  R.X:=PR.mmToPixels(AOffset.Left+E.RTLayout.Left);
  R.Y:=PR.mmToPixels(AOffset.Top+E.RTLayout.Top);
  AWidth:=PR.mmToPixels(E.RTLayout.Width);
  AHeight:=PR.mmToPixels(E.RTLayout.Height);
  PaintPolygon(PR.Canvas,R,AWidth,AHeight, PG.Corners,PG.RotateAngle,PG.LineWidth,PR.ColorToRGBTriple(PG.Color));
end;
{$ENDIF}

{ TReportPolygon }

procedure TReportPolygon.SetCLineWidth(AValue: Cardinal);
begin
  If AValue<1 then
    AValue:=1;
  if FLineWidth=AValue then Exit;
  FLineWidth:=AValue;
end;

procedure TReportPolygon.SetCorners(AValue: Cardinal);
begin
  If AValue<3 then
    AValue:=3;
  if FCorners=AValue then Exit;
  FCorners:=AValue;
end;

procedure TReportPolygon.RecalcLayout;
begin
  // Do nothing
end;

procedure TReportPolygon.DoWriteLocalProperties(AWriter: TFPReportStreamer;
  AOriginal: TFPReportElement);

Var
  P : TReportPolygon;

begin
  inherited DoWriteLocalProperties(AWriter, AOriginal);
  if AOriginal is TReportPolygon then
    begin
    P:=AOriginal as TReportPolygon;
    AWriter.WriteIntegerDiff('Color', Color, P.Color);
    AWriter.WriteIntegerDiff('Corners',Corners,P.Color);
    AWriter.WriteIntegerDiff('LineWidth',LineWidth,P.LineWidth);
    AWriter.WriteFloatDiff('RotateAngle',RotateAngle,P.RotateAngle);
    end
  else
    begin
    AWriter.WriteInteger('Color', Color);
    AWriter.WriteInteger('Corners',Corners);
    AWriter.WriteInteger('LineWidth',LineWidth);
    AWriter.WriteFloat('RotateAngle',RotateAngle);
    end;
end;

procedure TReportPolygon.ReadElement(AReader: TFPReportStreamer);
begin
  inherited ReadElement(AReader);
  Color:= AReader.ReadInteger('Color', clBlack);
  Corners:=AReader.ReadInteger('Corners',3);
  LineWidth:=AReader.ReadInteger('LineWidth',1);
  RotateAngle:=AReader.ReadFloat('RotateAngle',0);
end;

procedure TReportPolygon.Assign(Source: TPersistent);

Var
  P : TReportPolygon;

begin
  if (Source is TReportPolygon) then
    begin
    P:=Source as TReportPolygon;
    Corners:=P.Corners;
    Color:=P.Color;
    LineWidth:=P.LineWidth;
    RotateAngle:=P.RotateAngle;
    end;
  inherited Assign(Source);
end;

{$IFDEF EXPORTFPGUI}
Procedure AggPasPaintPolygon(Ex: TFPReportExportAggPas; ARect : TFPReportRect; ANumber : Integer; AStartAngle : Double; ALineWidth : Integer; AColor : TRGBTriple);

Var
  C : TFPReportPoint;
  I : Integer;
  P : Array of TFPReportPoint;
  R,A,Step : Double;
  AWidth,AHeight : Double;

begin
  ex.Agg.noFill;
  ex.Agg.lineColor(aColor.Red, aColor.Green, aColor.Blue);
  ex.Agg.lineWidth(aLineWidth);
  if ANumber<3 then
    exit;
  AWidth:=ARect.Width;
  AHeight:=ARect.Height;
  C.Left:=ARect.Left+AWidth / 2;
  C.Top:=ARect.Top+AHeight / 2;
  if aWidth<aHeight then
    R:=AWidth / 2
  else
    R:=AHeight / 2;
  SetLength(P,ANumber);
  A:=AStartAngle;
  Step:=(2*Pi)/ANumber;
  For I:=0 to ANumber-1 do
    begin
    P[i].Left:=Ex.mmToPixels(C.Left+R*Cos(a));
    P[i].Top:=Ex.mmToPixels(C.Top-R*Sin(a));
    A:=A+Step;
    end;
  For I:=0 to ANumber-2 do
    ex.agg.Line(P[I].Left,P[i].Top,P[I+1].Left,P[I+1].Top);
  ex.agg.Line(P[ANumber-1].Left,P[ANumber-1].Top,P[0].Left,P[0].Top);
end;

Procedure RenderPolygonInAggPas(AOffset : TFPReportPoint; E: TFPReportElement; RE : TFPReportExporter; ADPI : Integer);
var
  PR : TFPReportExportAggPas;
  PG : TReportPolygon;
  rPt : TFPReportRect;

begin
  PR:=RE as TFPReportExportAggPas;
  PG:=E as TReportPolygon;
  rpt.Left:=AOffset.Left+E.RTLayout.Left;
  rpt.Top:=AOffset.Top+E.RTLayout.top;
  Rpt.Width:=E.RTLayout.Width;
  Rpt.Height:=E.RTLayout.Height;
  AggPasPaintPolygon(PR,rpt,PG.Corners,PG.RotateAngle,PG.LineWidth,PR.ColorToRGBTriple(PG.Color));
end;

{$ENDIF}

begin
  gElementFactory.RegisterClass('Polygon',TReportPolygon);
  // Fallback renderer
  gElementFactory.RegisterImageRenderer(TReportPolygon,@RenderPolygonToImage);
{$IFDEF NATIVERENDERER}

{$IFDEF EXPORTPDF}
  gElementFactory.RegisterElementRenderer(TReportPolygon,TFPReportExportPDF,@RenderPolygonInPDF);
{$ENDIF}
{$IFDEF EXPORTFPIMAGE}
  gElementFactory.RegisterElementRenderer(TReportPolygon,TFPReportExportfpImage,@RenderPolygonInFPImage);
{$ENDIF}
{$IFDEF EXPORTLCL}
  gElementFactory.RegisterElementRenderer(TReportPolygon,TFPReportExportCanvas,@RenderPolygonInLCL);
{$ENDIF}
{$IFDEF EXPORTFPGUI}
  gElementFactory.RegisterElementRenderer(TReportPolygon,TFPReportExportAggPas,@RenderPolygonInAggPas);
  gElementFactory.RegisterElementRenderer(TReportPolygon,TFPReportExportCanvas,@RenderPolygonInAggPas);
{$ENDIF}
{$ENDIF}

end.

