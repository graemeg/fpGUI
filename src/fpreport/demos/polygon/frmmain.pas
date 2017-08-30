unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Spin, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BColor: TColorButton;
    FEAngle: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PaintBox1: TPaintBox;
    SECorners: TSpinEdit;
    SEWidth: TSpinEdit;
    procedure BColorColorChanged(Sender: TObject);
    procedure FEAngleChange(Sender: TObject);
    procedure PaintBox1Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure SECornersChange(Sender: TObject);
    procedure SEWidthChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.PaintBox1Click(Sender: TObject);
begin

end;

procedure TForm1.FEAngleChange(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;

procedure TForm1.BColorColorChanged(Sender: TObject);
begin
  Paintbox1.Invalidate;
end;


Procedure PaintPolygon(Canvas : TCanvas; AWidth,AHeight : Integer; ANumber : Integer; AStartAngle : Double; ALineWidth : Integer; AColor : TColor);

Var
  CX,CY,R,I : Integer;
  P : Array of TPoint;
  A,Step : Double;

begin
  Canvas.Pen.Color:=AColor;
  Canvas.Pen.Width:=aLineWidth;
  if ANumber<3 then
    exit;
  CX:=AWidth div 2;
  CY:=AHeight div 2;
  if aWidth<aHeight then
    R:=AWidth div 2
  else
    R:=AHeight div 2;
  SetLength(P,ANumber-1);
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

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  With PaintBox1 do
    PaintPolygon(Canvas,Width-1,Height-1,SECorners.Value,FEANgle.Value*Pi/180, SEWidth.Value, BColor.ButtonColor);
end;

procedure TForm1.SECornersChange(Sender: TObject);
begin
  Paintbox1.INvalidate ;
end;

procedure TForm1.SEWidthChange(Sender: TObject);
begin
  Paintbox1.INvalidate;
end;

end.

