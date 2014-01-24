{
  This is a console application demo. It uses the Agg2D object,
  which has a much friendlier API, to do all the drawing. We then
  save the image buffer to a JPG, using the fcl-image package,
  which comes standard with the Free Pascal Compiler.

// Paths: ..\;..\svg;..\util;expat-wrap
}
program console_aggpas_2;

{$mode objfpc}{$H+}

uses
  sysutils,
  FPimage,
  FPWriteJPEG,
  agg_2D,
  agg_basics;

const
  ImageWidth = 800;
  ImageHeight = 480;
  RGBA_Width = 4;
  LineCount = 30;
  {$IFDEF Unix}
  FontFile = '../../arial.ttf';
  {$ENDIF}
  {$IFDEF Windows}
  FontFile = 'Arial';
  {$ENDIF}

type
  TPainter = class(TObject)
  public
    procedure HandlePlug;
    procedure DrawStuff(agg: Agg2D_ptr);
  end;


procedure TPainter.HandlePlug;
var
  agg: Agg2D_ptr;
  buf: array of int8;
  image: TFPMemoryImage;
  writer: TFPWriterJPEG;
  x, y: Integer;
  c: TFPColor;
  time, totalTime: TDateTime;
  function getBufItemAsWord(aDelta: byte): Word;
  var
    actualY: Integer;
  begin
    actualY := ImageHeight - y - 1;
    result :=
      Word(buf[x * RGBA_Width + actualY * ImageWidth * RGBA_Width + aDelta] shl 8)
      or Word(128);
  end;
begin
  totalTime := Now;
  time := Now;
  SetLength(buf, ImageWidth * ImageHeight * RGBA_Width);
  New(agg, Construct);
  agg^.attach(@(buf[0]), ImageWidth, ImageHeight, ImageWidth * RGBA_Width);
  DrawStuff(agg);
  Dispose(agg, Destruct); // not necessary to keep it after rendering is finished
  time := Now - time;
//  Logger.Emit('Draw: time spent: ' + TimeStampToString(time));
  time := Now;
  image := TFPMemoryImage.create(ImageWidth, ImageHeight);
  for x := 0 to ImageWidth - 1 do
    for y := 0 to ImageHeight - 1 do
    begin
      c.red := getBufItemAsWord(2);
      c.green := getBufItemAsWord(1);
      c.blue := getBufItemAsWord(0);
      c.alpha := getBufItemAsWord(3);
      image.Colors[x, y] := c;
    end;
  time := Now - time;
//  WriteLn('Image copy: time spent: ' + DateTimeToString(time));
  time := Now;
  writer := TFPWriterJPEG.Create;
  writer.CompressionQuality := $FF div 3; // bad quality plz
  writer.ProgressiveEncoding := True;
  image.SaveToFile('test.jpeg', writer);
  image.Free;
  writer.Free;
  time := Now - time;
//  WriteLn('Image encode: time spent: ' + DateTimeToString(time));
  totalTime := Now - totalTime;
//  WriteLn('Total time: ' + DateTimeToString(totalTime));
end;

procedure TPainter.DrawStuff(agg: Agg2D_ptr);
var
  i: Integer;
  x, y, px, py, d: Double;
begin
  agg^.clearAll(0, 0, 0);
  agg^.lineColor(0, 0, 0, 255);
  agg^.lineWidth(3);
  agg^.rectangle(0, 0, ImageWidth, ImageHeight);
  agg^.font(fontfile, 16);
  d := ImageWidth / LineCount;
  agg^.lineColor(0, 0, 0, 100);
  agg^.lineWidth(1);
  for i := 1 to LineCount - 1 do
  begin
    x := i * d;
    agg^.line(x, 0, x, ImageHeight);
  end;
  for i := 1 to trunc(ImageHeight / d) do
  begin
    y := i * d;
    agg^.line(0, y, ImageWidth, y);
  end;
  x := 0;
  y := ImageHeight / 2;
  px := x;
  py := y;
  agg^.lineColor(255, 0, 0, 200);
  agg^.fillColor(0, 0, 0, 200);
  agg^.lineWidth(3);
  for i := 0 to LineCount - 1 do
  begin
    x := x + d;
    y := y + Random(Round(ImageHeight / 3)) - ImageHeight / 6;
    if y < 0 then
      y := ImageHeight / 6;
    if y >= ImageHeight then
      y := ImageHeight - ImageHeight / 6;
    agg^.line(px, py, x, y);
    agg^.text(x, y, char_ptr(IntToStr(i) + ' point'{' шта?'}));
    px := x;
    py := y;
  end;
end;


var
  p: TPainter;
begin
  Randomize;
  p := TPainter.Create;
  p.HandlePlug;
  p.Free;
end.

