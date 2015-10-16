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
      This unit defines a class that draws a Bar chart.
}
unit fpg_chart_bar;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_widget,
  fpg_chart;

type

  TfpgBarChart = class(TfpgChartTypeAbs)
  public
    procedure DoDraw; override;
  end;

implementation

uses
  Agg2D,
  Math;


{ TfpgBarChart }

procedure TfpgBarChart.DoDraw;
var
  lChart: TfpgChart;
  i, n, bw, bp: integer;
  x1, y1, x2, y2: double;
begin
  lChart := TfpgChart(FWidget);
  n := High(lChart.Dataset)+1;  // number of bars needed
  // avoid to create insane no of bars
  if n > (lChart.Width div 2) then
    exit; //==>
  bp := lChart.Width div n;  // bar pitch
  bw := bp - Max(2, lChart.LineWidth); // bar width (keep some space between bars)

  VG.LineWidth(1);
  VG.LineColor(0, $80, 0);
  VG.FillColor(0, $80, 0, 100); // ligter version of the outline color - using alpha channel
  for i := 0 to n-1 do
  begin
    x1 := i * bp;
    y1 := lChart.Height - lChart.DataSet[i,0];
    x2 := x1 + bw;
    y2 := lChart.Height;
    VG.Rectangle(x1, y1, x2, y2, True);
  end;
end;


initialization
  fpgChartFactory.RegisterClass(cChartBar, TfpgBarChart);

end.
