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
  i,box : integer;
  a1,a2: double;
  c: TAggColor;
  r: double;
  startingAngle: double;
  endingAngle: double;
  arcSize: double;
  centerX, centerY: double;
begin
  VG.ClearAll(255, 255, 255);
  lChart := TfpgChart(FWidget);
  if lChart.Width < lChart.Height then
    box := lChart.Width
  else
    box := lChart.Height;
  r := (box / 2);
  a1 := 0;  { starting angle is 0 degrees - 3 o'clock position }
  centerX := lChart.Width / 2;
  centerY := lChart.Height / 2;

  for i := 0 to High(lChart.Dataset) do
  begin
    a2 := lChart.DataSet[i, 0];
    //write('iya1a2c: ',i,' ',FDataset[i,0],' ',a1:4:1,' ',a2:4:1);
    c := fpgColor2AggColor(lChart.Color(i));
    c.a := 100; { alpha channel - gives it a pastel colour feel }
    
    startingAngle := Deg2Rad(a1);
    arcSize := a2;
    endingAngle := startingAngle + Deg2Rad(arcSize);  { we are going clockwise }

    VG.ResetPath;
    VG.NoLine;
    VG.FillColor(c);
    VG.MoveTo(centerX, centerY);
    VG.ArcPath(centerX, centerY, r, r, startingAngle, endingAngle);
    VG.MoveTo(centerX, centerY);
    VG.ClosePolygon;
    VG.DrawPath;

    a1 := a1 + a2;
  end;
end;

initialization
  fpgChartFactory.RegisterClass(cChartPie, TfpgPieChart);

end.
