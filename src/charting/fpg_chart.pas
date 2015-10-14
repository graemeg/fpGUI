{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2013 - 2015 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      A new widget which can draw various small charts. It supports
      the following chart types: Pie, Bar, HBar, Line, xyLine and Scatter
      charts.
}

unit fpg_chart;

// Features:
// - set LineWidth.
// - set drawing colors.
// - set text color.
// - set background color.
// - NO ticks on x- or y-axis.
// - NO values or text along x- or y-axis.
// - NO real, only integer data.
// - x- and y values must fit in the chart.
// Uses Array[][] for the data
// Pie uses array like 1 * many,
//    with angle in degrees (between 0 and 360)
// Bar uses array like 1 * many
// Line uses array like 1 * many
// xy line array like (x,y) * many
//   but to create more lines use (x,y1,y2,y3) * many
// Scatter can only use (x,y) * many

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  fpg_base,
  fpg_main,
  fpg_widget;

type
  TfpgChart = class;  // forward declaration.

  TfpgChartType = ( ctPie, ctBar, ctHBar, ctLine,  // using 1 dimensional array
                    ctScatter,           // using xy coordinates array[2,many]
                    ctxyLine, ctArea );  // using (x and many y coordinates

  // This is a minimalistic unit so we use only 1 method to store data:
  TDataset = array of array of integer;


  TfpgAxis = class(TObject)
  private
    FParent: TfpgChart;
  protected
    property Parent: TfpgChart read FParent write FParent;
  public
    Low: integer;
    High: integer;
    Visible: Boolean;
    Color:   TfpgColor;
    constructor Create(AOwner: TComponent);
    procedure   Draw; virtual; abstract;
  end;


  TfpgXAxis = class(TfpgAxis)
  public
    procedure Draw; override;
  end;


  TfpgYAxis = class(TfpgAxis)
  public
    procedure Draw; override;
  end;


  TfpgChart = class(TfpgWidget)
  private
    FText:      TfpgString;
    Fxaxis:     TfpgXAxis;
    Fyaxis:     TfpgYAxis;
    FLineWidth: integer;
    FDataSet:   TDataSet;
    FChartType: TfpgChartType;
    FColors:    array of TfpgColor;
  protected
    procedure   HandlePaint; override;
    procedure   Draw;
    procedure   SetText(const AValue: TfpgString);
    procedure   DrawPie;
    procedure   DrawBar;
    procedure   DrawHBar;
    procedure   DrawLine;
    procedure   DrawxyLine;
    procedure   DrawArea;
    procedure   DrawScatter;
    function    Color(i: Integer): TfpgColor; inline;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Xaxis: TfpgXAxis read Fxaxis write Fxaxis;
    property    Yaxis: TfpgYAxis read Fyaxis write Fyaxis;
    property    Text: TfpgString read FText write SetText;
    property    DataSet: TDataSet read FDataSet write FDataSet;
    property    ChartType: TfpgChartType read FCharttype write FCharttype;
    property    Linewidth: integer read FLinewidth write FLineWidth;
    //procedure   AutoXYaxis;
    procedure   Clear;
    procedure   DrawDot(x, y, d: integer; Acolor: TfpgColor);
    procedure   Drawline(x1,y1,x2,y2: integer);
    procedure   DrawlinePolar(cx,cy,r,a: integer);
    procedure   DrawRectangle(x,y,w,h: integer);
    procedure   DrawTrapezoid( x1,y1,x2,y2: TfpgCoord);
    procedure   FillRectangle(x,y,w,h: integer);
    procedure   Update;
    //procedure   DataSet(adata: TDataSet); // of array
    //procedure   Drawxline(x1,y1,x2,y2: integer);
    //procedure   DrawAuto(c: TfpgCanvas; an: array of integer);
  end;
{ Note:
  TfpgWidget has already a lot of useful properties & functions
  if these are fit for purpose we do not define again, examples:
  TextColor:   TfpgColor;
  BackgroundColor: TfpgColor;
  property:    FontDesc    is provided by TfpgWidget
  procedure:   SetPosition is provided by TfpgWidget
}

  TfpgChartTypeAbs = class(TObject)
  end;


  // A class reference for the TfpgChartTypeAbs descendants
  TfpgChartTypeClass = class of TfpgChartTypeAbs;


  TfpgChartTypeClassMapping = class(TObject)
  private
    FMappingName: TfpgString;
    FChartTypeClass: TfpgChartTypeClass;
  public
    constructor Create(const AMappingName: TfpgString; AChartTypeClass: TfpgChartTypeClass);
    property    MappingName: string read FMappingName;
    property    ChartTypeClass: TfpgChartTypeClass read FChartTypeClass;
  end;


  TfpgChartFactory = class(TObject)
  private
    FList: TObjectList;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   RegisterClass(const AChartTypeName: TfpgString; AChartTypeClass: TfpgChartTypeClass);
    function    CreateInstance(const AChartTypeName: TfpgString): TfpgChartTypeAbs;
  end;


const
  DefaultChartColors: array[0..13] of TfpgColor= (
    clAqua, clBlue,  clDkGray, clFuchsia, clGreen,
    clLime, clMaroon, clNavy,  clOlive,   clPurple,
    clRed,  clSilver, clTeal,  clYellow
    );

// The ChartFactory is a singleton
function fpgChartFactory: TfpgChartFactory;


implementation


{$I chartsincos.inc}

var
  uChartFactory: TfpgChartFactory;
  
function fpgChartFactory: TfpgChartFactory;
begin
  if not Assigned(uChartFactory) then
    uChartFactory := TfpgChartFactory.Create;
  Result := uChartFactory; 
end;


{ TfpgAxis }

constructor TfpgAxis.Create(AOwner: TComponent);
begin
  inherited Create;
  if (AOwner <> nil) and (AOwner is TfpgWidget) then
    Parent := TfpgChart(AOwner);
  Low:=0;  // set some sane defaults
  High:=100;
  Visible:=True;
  Color:=clBlack;
end;

{ TfpgXAxis }

procedure TfpgXAxis.Draw;
var
  xend: integer;
begin
  if Visible then
  begin
    xend:=Parent.Width-1;
    Parent.Canvas.Color := Color;
    Parent.Drawline(0,0,xend,0);  // x1, y1, x2, y2
  end;
end;

{ TfpgYAxis }

procedure TfpgYAxis.Draw;
var
  yend: integer;
begin
  if Visible then
  begin
    yend:=Parent.Height-1;
    Parent.Canvas.Color := Color;
    Parent.Drawline(0, 0, 0, yend);  // x1, y1, x2, y2
  end;
end;

{ TfpgChart }

constructor TfpgChart.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited Create(Aowner);
  Text:='Chart';    // set some sane defaults
  FWidth:= 128;
  FHeight:= 96;
  FLineWidth:=1;
  FChartType:=ctLine;
  BackgroundColor:= clCream;
  TextColor:= clBlack;
  Xaxis:=TfpgXAxis.Create(self);
  Yaxis:=TfpgYAxis.Create(self);
  Canvas.TextColor:=TextColor;
  SetLength(FColors,length(DefaultChartColors));
  for i := 0 to length(DefaultChartColors)-1 do
    FColors[i]:=DefaultChartColors[i];
end;

destructor TfpgChart.Destroy;
begin
  Xaxis.Free;
  Yaxis.Free;
  inherited Destroy;
end;

procedure TfpgChart.HandlePaint;
begin
  Canvas.Clear(BackgroundColor);
  Draw;
end;

procedure TfpgChart.SetText(const AValue: TfpgString);
begin
  if FText <> AValue then
  begin
    FText := AValue;
  end;
end;

procedure TfpgChart.Draw;
begin
  Canvas.SetLineStyle(Linewidth,lsSolid);
  Xaxis.Draw;
  Yaxis.Draw;
  Canvas.Color := clGreen;
  if FDataSet=Nil then
    Canvas.DrawString(5, 5, 'No Data')
  else
  begin
    case FChartType of
      ctPie: DrawPie;
      ctBar: DrawBar;
      ctHBar: DrawHBar;
      ctLine: DrawLine;
      ctArea: DrawArea;
      ctxyLine: DrawxyLine;
      ctScatter: DrawScatter;
    end;
    //writeln('backgr color    : ',BackgroundColor);
    //writeln('canv. backgr.col: ',Canvas.BackgroundColor);
    //writeln('text color: ',TextColor);
    //writeln('canv.t col: ',Canvas.TextColor);
    Canvas.TextColor:=TextColor;
    Canvas.DrawString(5, 5, Text);
  end;
end;

// Filled pie like default google pie
procedure TfpgChart.DrawPie;
var
  n,i,box : integer;
  a1,a2: double;
begin
  if Width < Height then
    box:=width
  else
    box:=Height;
  n:=High(FDataset);
  a1:=0;
  for i:=0 to n do
  begin
    a2:=FDataSet[i,0];
    //write('iya1a2c: ',i,' ',FDataset[i,0],' ',a1:4:1,' ',a2:4:1);
    Canvas.Color := Color(i);
    Canvas.FillArc(0,0,box,box,a1,a2);
    a1:=a1+a2;
  end;
end;

procedure TfpgChart.DrawBar;
var
  i, n, bw, bp: integer;
begin
  n:=High(FDataset);
  // avoid to creat insane no of bars
  if n>(Width div 2) then
    exit; //==>
  bp:=Width div n;  // bar pitch
  bw:=bp-LineWidth; // bar width (keep some space between bars)
  for i:= 0 to  n-1 do
    FillRectangle(i*bp,0,bw,FDataSet[i,0]);  // x,y,w,h
end;

procedure TfpgChart.DrawHBar;
var
  i, n, bw, bp: integer;
begin
  n:=High(FDataset);
  bp:=Height div n;  // bar pitch
  bw:=bp-LineWidth; // bar width (keep some space between bars)
  for i:= 0 to  n-1 do
    FillRectangle(0,i*bp,FDataSet[i,0],bw);  // x,y,w,h
    //writeln('Hbar ',i,': ',0,' ',i*bp,' ',FDataSet[i,0],' ',bw);  // x,y,w,h
end;

procedure TfpgChart.DrawLine;
var
  i, n,bw,x1,y1,x2,y2: integer;
begin
  // the array is [0..many,0]
  // only use the first element to draw lines
  n:=High(FDataset);
  bw:=Width div n; // block width
  x1:=0;
  y1:=FdataSet[0,0];
  for i:= 1 to  n do
  begin
    y2:=FdataSet[i,0];
    x2:=i*bw;
    Drawline(x1,y1,x2,y2);
    x1:=x2;
    y1:=y2;
    //writeln(x1,' ',y1,' ',x2,' ',y2);
  end;
end;

procedure TfpgChart.DrawxyLine;
var
  i,j,n,m,x1,y1,x2,y2: integer;
begin
  // the array is [0..many,(x,y)]
  n:=High(FDataset);
  m:=High(FDataset[0]);
  for j:=1 to m do
  begin
    x1:=FDataSet[0,0];
    y1:=FDataSet[0,j];
    Canvas.Color := FColors[j];
    for i:= 1 to  n do
    begin
      x2:=FDataSet[i,0];
      y2:=FdataSet[i,j];
      Drawline(x1,y1,x2,y2);
      //writeln(x1,' ',y1,' ',x2,' ',y2);
      x1:=x2;
      y1:=y2;
    end;
  end;
end;

procedure TfpgChart.DrawArea;
var
  i,j,n,m,x1,y1,x2,y2: integer;
begin
  // the array is [0..many,(x,y)]
  n:=High(FDataset);
  m:=High(FDataset[0]);
  for j:=1 to m do
  begin
    x1:=FDataSet[0,0];
    y1:=FDataSet[0,j];
    Canvas.Color := FColors[j];
    for i:= 1 to  n do
    begin
      x2:=FDataSet[i,0];
      y2:=FdataSet[i,j];
      DrawTrapezoid(x1,y1,x2,y2);
      //writeln(x1,' ',y1,' ',x2,' ',y2);
      x1:=x2;
      y1:=y2;
    end;
  end;
end;

procedure TfpgChart.DrawScatter;
var
  i,n,x1,y1: integer;
begin
  // the array is [0..many,(x,y)]
  n:=High(FDataset);
  for i:= 0 to  n do
    begin
      x1:=FDataSet[i,0];
      y1:=FdataSet[i,1];
      DrawDot(x1,y1,Linewidth,clGreen);
    end;
end;

{.$INLINE ON}

function TfpgChart.Color(i: Integer): TfpgColor;
begin
  Result := FColors[i mod 14];
end;

procedure TfpgChart.DrawDot(x, y, d: integer; Acolor: TfpgColor);
const
  dot5: array[-2..+2,-2..+2] of boolean =(
    (False, True, True, True, False),
    (True,  True, True, true, True),
    (True,  True, True, true, True),
    (True,  True, True, true, True),
    (False, True, True, True, False));
var
  yd, i, j: integer;
begin
  yd := Height-y-1;
  if d<6 then
  case d of
    1: Canvas.Pixels[x,yd]:=Acolor;
    2: Begin
        Canvas.Pixels[x,yd]:=Acolor;
        Canvas.Pixels[x+1,yd]:=Acolor;
        Canvas.Pixels[x,yd+1]:=Acolor;
        Canvas.Pixels[x+1,yd+1]:=Acolor;
       end;
    3: begin
         for i:=-1 to 1 do
           for j:=-1 to 1 do
             Canvas.Pixels[x+i,yd+j]:=Acolor;
       end;
    4: begin
         Canvas.Pixels[x,yd+2]:=Acolor;
         Canvas.Pixels[x,yd-2]:=Acolor;
         Canvas.Pixels[x+2,yd]:=Acolor;
         Canvas.Pixels[x-2,yd]:=Acolor;
         for i:=-1 to 1 do
           for j:=-1 to 1 do
             Canvas.Pixels[x+i,yd+j]:=Acolor;
       end;
    5: for i:=-2 to +2 do
         for j:=-2 to +2 do
           if dot5[i,j] then Canvas.Pixels[x+i,yd+j]:=AColor;
  end;
end;

procedure TfpgChart.DrawRectangle(x,y,w,h: integer);
begin
  //writeln('h ',Height);
  Canvas.DrawRectangle(x,Height-h-y,w,h);
  //Canvas.DrawRectangle(x,y,w,h);
end;

procedure TfpgChart.DrawTrapezoid( x1,y1,x2,y2: TfpgCoord);
var
  ya, yb, base: TfpgCoord;
begin
  ya:=Height-y1-1;
  yb:=Height-y2-1;
  base:=Height-1;
  Canvas.FillTriangle(x1, base, x2, base, x1, ya);
  Canvas.FillTriangle(x2, base, x1, ya, x2, yb);
  //FillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord);
end;

procedure TfpgChart.FillRectangle(x,y,w,h: integer);
begin
  Canvas.FillRectangle(x,Height-h-y,w,h);
end;

procedure TfpgChart.Drawline(x1,y1,x2,y2: integer);
begin
  Canvas.Drawline(x1,Height-y1-1,x2,Height-y2-1);
end;
{.$INLINE OFF}

procedure TfpgChart.Update;
begin
  Repaint;
end;

procedure TfpgChart.Clear;
begin
  Canvas.Clear(clYellow);
end;

{
procedure TfpgChart.Drawxline(x1,y1,x2,y2: integer);
begin
  //writeln('--',x1,' ',Height-y1-1,' x2y2: ',x2,' ',Height-y2-1,'--');
  Canvas.Drawline(x1,Height-y1-1,x2,Height-y2-1);
end;
}

procedure TfpgChart.DrawLinePolar(cx,cy,r,a: integer);
var
  p,q,x2,y2: integer;
  s,c: double;
begin
  p:=a div 45;
  q:=a mod 360;
  //writeln('cxcyra: ',cx,' ',cy,' ',r,' ',a,' ',' p: ',p,' q: ',q);
  case p of
    0: begin s:=chartsincos[q,0]; c:=chartsincos[q,1] end;
    1: begin s:=chartsincos[90-q,1]; c:=chartsincos[90-q,0] end;
    2: begin s:=chartsincos[q-90,1]; c:=0-chartsincos[q-90,0] end;
    3: begin s:=chartsincos[180-q,0]; c:=0-chartsincos[180-q,1] end;
    4: begin s:=0-chartsincos[q-180,0]; c:=0-chartsincos[q-180,1] end;
    5: begin s:=0-chartsincos[270-q,1]; c:=0-chartsincos[270-q,0] end;
    6: begin s:=0-chartsincos[q-270,1]; c:=chartsincos[q-270,0] end;
    7: begin s:=0-chartsincos[360-q,0]; c:=chartsincos[360-q,1] end;
  end;
  x2:=trunc(r * c);
  y2:=trunc(r * s);
  //writeln('s c x2y2: ',s:4:2,' ',c:4:2,' ',cx+x2,' ',cy+y2);
  Canvas.Color := clBlack; //BackgroundColor;
  Drawline(cx,cy,cx+x2,cy+y2);
end;

{ TfpgChartTypeClassMapping }

constructor TfpgChartTypeClassMapping.Create(const AMappingName: TfpgString; AChartTypeClass: TfpgChartTypeClass);
begin
  inherited Create;
  FMappingName := AMappingName;
  FChartTypeClass := AChartTypeClass;
end;

{ TfpgChartFactory }

constructor TfpgChartFactory.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
end;

destructor TfpgChartFactory.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

// Register a TfpgChartTypeClass class for creation by the factory
procedure TfpgChartFactory.RegisterClass(const AChartTypeName: TfpgString; AChartTypeClass: TfpgChartTypeClass);
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    if UpperCase(TfpgChartTypeClassMapping(FList.Items[i]).MappingName) = UpperCase(AChartTypeName) then
      Assert(false, Format('ChartType class <%s> already registered.', [AChartTypeName]));
  end;
  FList.Add(TfpgChartTypeClassMapping.Create(AChartTypeName, AChartTypeClass));
end;

function TfpgChartFactory.CreateInstance(const AChartTypeName: TfpgString): TfpgChartTypeAbs;
var
  i: integer;
begin
  result := nil;
  for i := 0 to FList.Count-1 do
  begin
    if UpperCase(TfpgChartTypeClassMapping(FList.Items[i]).MappingName) = UpperCase(AChartTypeName) then
    begin
      result := TfpgChartTypeClassMapping(FList.Items[i]).ChartTypeClass.Create;
      Break; //==>
    end;
  end;
  Assert(result <> nil, Format('<%s> does not identify a registered chart type class.', [AChartTypeName]));
end;


initialization
  uChartFactory := nil;

finalization
  uChartFactory.Free;

end.
