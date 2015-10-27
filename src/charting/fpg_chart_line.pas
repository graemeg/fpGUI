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
      This unit defines a class that draws a Line chart.
}
unit fpg_chart_line;

{$mode objfpc}{$H+}

{ TODO : Option to show dots or not. }
{ TODO : Option to use smooth curves instead of straight lines. }

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_widget,
  fpg_chart;

type

  TfpgLineChart = class(TfpgChartTypeAbs)
  public
    procedure DoDraw; override;
  end;

implementation

uses
  Agg2D,
  Math;



{ TfpgLineChart }

procedure TfpgLineChart.DoDraw;
var
  lChart: TfpgChart;
  i, n,bw,x1,y1,x2,y2: integer;
begin
  VG.ClearAll(255, 255, 255);
  lChart := TfpgChart(FWidget);
  // the array is [0..many,0]
  // only use the first element to draw lines
  n := High(lChart.Dataset);
  bw := lChart.Width div n; // block width
  x1 := 0;
  y1 := lChart.Height - lChart.Dataset[0,0];
  for i:= 1 to n do
  begin
    VG.NoFill;
    VG.LineColor(0, $80, 0);
    VG.LineWidth(1);
    y2 := lChart.Height - lChart.Dataset[i,0];
    x2 := i*bw;
    VG.Line(x1, y1, x2, y2, True);

    // draw dot
    VG.ResetPath;
    VG.LineWidth(0.75);
    VG.LineColor(0, 0, 255);
    VG.FillColor(0, 0, 255, 100);
    VG.Ellipse(x1, y1, 3, 3);

    x1 := x2;
    y1 := y2;
    //writeln(x1,' ',y1,' ',x2,' ',y2);
  end;
end;


initialization
  fpgChartFactory.RegisterClass(cChartLine, TfpgLineChart);

end.
