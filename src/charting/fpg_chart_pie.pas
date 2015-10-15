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
      This unit defines a class that draws a Pie chart.
}
unit fpg_chart_pie;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_widget,
  fpg_chart;

type

  TfpgPieChart = class(TfpgChartTypeAbs)
  public
    procedure DoDraw; override;
  end;

implementation

uses
  Agg2D;

{ TfpgPieChart }

procedure TfpgPieChart.DoDraw;
var
  lChart: TfpgChart;
  n,i,box : integer;
  a1,a2: double;
begin
  lChart := TfpgChart(FWidget);
  if lChart.Width < lChart.Height then
    box := lChart.Width
  else
    box := lChart.Height;
  n := High(lChart.Dataset);
  a1 := 0;
  for i := 0 to n do
  begin
    a2 := lChart.DataSet[i, 0];
    //write('iya1a2c: ',i,' ',FDataset[i,0],' ',a1:4:1,' ',a2:4:1);
    lChart.Canvas.Color := lChart.Color(i);
    lChart.Canvas.FillArc(0, 0, box, box, a1, a2);
    a1 := a1 + a2;
  end;
end;

initialization
  fpgChartFactory.RegisterClass(cChartPie, TfpgPieChart);

end.
