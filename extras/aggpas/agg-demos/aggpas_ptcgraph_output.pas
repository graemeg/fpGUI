(*
  This is a minimalistic console application demo derived from the Agg2DConsole.dpr at

  https://github.com/graemeg/fpGUI/tree/develop/src/corelib/render/software/agg-demos

  It uses the Agg2D object in AggPas, which has a much friendlier API, to do all the drawing.

  The drawing buffer is then displayed in a window using ptcgraph and 16 bit RGB565 color format.

  Uses a GraphBitmapBuffer record type that includes the width and height of the image as well as a reserved parameter
  per the info in this post: http://lists.freepascal.org/pipermail/fpc-pascal/2017-June/051524.html

  [...]
  "The structure, used by putimage is as follows:
  3 longints (12 bytes):
  - image width
  - image height
  - reserved
  followed by width*height 16-bit words.
  [...]

  Agg2D object constructor requires a modification to use pixel formats other than rgba32 (e.g., 16 bit RGB565 color format) as follows:
[...]
  public
    constructor Construct(pixfmt:define_pixfmt);
    destructor  Destruct;
[...]
  { CONSTRUCT }
  constructor Agg2D.Construct(pixfmt:define_pixfmt);
  begin
   	m_rbuf.Construct;

  	pixfmt(m_pixFormat ,@m_rbuf);
  	pixfmt_custom_blend_rgba(m_pixFormatComp ,@m_rbuf ,@comp_op_adaptor_rgba ,rgba_order );
   	pixfmt(m_pixFormatPre ,@m_rbuf);
   	pixfmt_custom_blend_rgba(m_pixFormatCompPre ,@m_rbuf ,@comp_op_adaptor_rgba ,rgba_order);
[...]

ADDITIONAL NOTES:
- although rendering appears to work with pixel formats other than rgba32, custom blending may need further testing.
- there may other implementations of graph units and video modes requiring a different bitmap structure than the one
used below.
- example is tested with fpc 3.0.1 and Agg2D - Version 1.0 Release Milano 3 (AggPas 2.4 RM3)
*)

program aggpas_ptcgraph_output;

{$mode objfpc}{$H+}

uses
  ptcgraph,
  ptccrt,
	agg_pixfmt_rgb_packed, //for pixfmt_rgb565
  agg_2D;

const
  IMAGE_WIDTH = 800;
  IMAGE_HEIGHT = 600;
  RGB_WIDTH =2; //16bit RGB565 format
  LINE_COUNT = 30;

type

TGraphBitmapBuffer=packed record
  width,
  height,
  reserved:	longint;//per info in http://lists.freepascal.org/pipermail/fpc-pascal/2017-June/051524.html
  data: 	array[0..RGB_WIDTH*IMAGE_WIDTH*IMAGE_HEIGHT-1] of byte;
end;

var
  gd,gm : smallint;
  graph_buffer: TGraphBitmapBuffer;

procedure DrawStuff(agg: Agg2D_ptr);
var
  i: Integer;
  x, y, px, py, d: Double;
  c1, c2: Color;
begin
  // draw a full screen graph with grid
  agg^.clearAll(0, 0, 0);
  agg^.lineColor(0, 0, 0, 255);
  agg^.lineWidth(3);
  agg^.rectangle(0, 0, IMAGE_WIDTH, IMAGE_HEIGHT);
  agg^.lineWidth(1);
  agg^.lineColor(0, 155, 0, 255);
  agg^.rectangle(10, 10, 50, 50);
//  agg^.font(fontfile, 16);
  d := IMAGE_WIDTH / LINE_COUNT;
  agg^.lineColor(0, 0, 0, 100);
  agg^.lineWidth(1);
  for i := 1 to LINE_COUNT - 1 do
  begin
    x := i * d;
    agg^.line(x, 0, x, IMAGE_HEIGHT);
  end;
  for i := 1 to trunc(IMAGE_HEIGHT / d) do
  begin
    y := i * d;
    agg^.line(0, y, IMAGE_WIDTH, y);
  end;
  x := 0;
  y := IMAGE_HEIGHT / 2;
  px := x;
  py := y;
  agg^.lineColor(255, 0, 0, 200);
  agg^.fillColor(0, 0, 0, 200);
  agg^.lineWidth(1);
  for i := 0 to LINE_COUNT - 1 do
  begin
    x := x + d;
    y := y + Random(Round(IMAGE_HEIGHT / 3)) - IMAGE_HEIGHT / 6;
    if y < 0 then
      y := IMAGE_HEIGHT / 6;
    if y >= IMAGE_HEIGHT then
      y := IMAGE_HEIGHT - IMAGE_HEIGHT / 6;
    agg^.line(px, py, x, y);
//    agg^.text(x, y, char_ptr(IntToStr(i) + ' point'));
    px := x;
    py := y;
  end;

  // Star shape
  agg^.LineCap(CapRound);
  agg^.LineWidth(5);
  agg^.LineColor($32 ,$cd ,$32 );
  c1.Construct(0, 0 , 255, 200);
  c2.Construct(0, 0, 255, 50);
  agg^.FillLinearGradient(100, 100, 150, 150, c1, c2);
  agg^.Star(100 ,150 ,30 ,70 ,55 ,5 );

  // Draw Arc from 45 degrees to 270 degrees
  agg^.LineColor($4C, $6C, $9C);
  agg^.LineWidth(5 );
  agg^.Arc(300 ,320 ,80 ,50 ,Deg2Rad(45 ) ,Deg2Rad(270 ) );
end;

procedure DrawAndDisplay;
var
  agg: Agg2D_ptr;
begin
//agg draw
  New(agg, Construct(@pixfmt_rgb565));
  agg^.attach(@graph_buffer.data, IMAGE_WIDTH, IMAGE_HEIGHT, -(IMAGE_WIDTH * RGB_WIDTH));
  DrawStuff(agg);
  Dispose(agg, Destruct); // not necessary to keep it after rendering is finished

//display on ptc surface
	graph_buffer.width:=IMAGE_WIDTH;
	graph_buffer.height:=IMAGE_HEIGHT;
  ptcgraph.PutImage(0,0,graph_buffer,NormalPut);
  ptcgraph.Rectangle(10,10,100,100);
  ptcgraph.PieSlice(100,100,0,25,30);
  ptcgraph.OutTextXY(80,80,'It works!');
end;


begin
  gd:=d16Bit;
  gm:=m800x600;
  ptcgraph.Initgraph(gd,gm,'');
  Randomize;
  DrawAndDisplay;
  ReadKey;
  ptcgraph.Closegraph;
end.

