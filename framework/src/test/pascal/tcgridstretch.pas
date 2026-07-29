{
  Tests for per-column stretch weights on TfpgBaseGrid descendants.

  Stretch weights let columns share the grid's spare client width
  proportionally. A weight of 0 keeps a column at its set width.
}
unit tcgridstretch;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  fpg_base,
  fpg_main,
  fpg_basegrid,
  fpg_scrollbar,
  fpg_customgrid,
  fpg_grid;

type

  TTestGridStretch = class(TTestCase)
  private
    FGrid: TfpgStringGrid;
    procedure SetupColumns(ACount: integer; AWidth: integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestColumnStretchDefaultsToZero;
    procedure TestColumnMinWidthDefault;
    procedure TestNoStretchLeavesWidthsAlone;
    procedure TestSingleStretchColumnAbsorbsSlack;
    procedure TestFixedColumnsKeepTheirWidth;
    procedure TestWeightsDistributeProportionally;
    procedure TestEqualWeightsSplitEvenly;
    procedure TestRemainderGoesToLastStretchColumn;
    procedure TestShrinkRespectsMinWidth;
    procedure TestStretchGrowsFromBelowMinWidth;
    procedure TestHasStretchColumns;
  end;

procedure RegisterTests;

implementation

procedure RegisterTests;
begin
  RegisterTest(TTestGridStretch);
end;

{ TTestGridStretch }

procedure TTestGridStretch.SetUp;
begin
  inherited SetUp;
  FGrid := TfpgStringGrid.Create(nil);
  FGrid.ScrollBarStyle := ssNone;
end;

procedure TTestGridStretch.TearDown;
begin
  FreeAndNil(FGrid);
  inherited TearDown;
end;

procedure TTestGridStretch.SetupColumns(ACount: integer; AWidth: integer);
var
  i: integer;
begin
  for i := 1 to ACount do
    FGrid.AddColumn(Format('Col%d', [i]), AWidth);
end;

procedure TTestGridStretch.TestColumnStretchDefaultsToZero;
begin
  SetupColumns(2, 50);
  CheckEquals(0, FGrid.ColumnStretch[0], 'Column 0 stretch default');
  CheckEquals(0, FGrid.ColumnStretch[1], 'Column 1 stretch default');
end;

procedure TTestGridStretch.TestColumnMinWidthDefault;
begin
  SetupColumns(1, 50);
  CheckEquals(8, FGrid.ColumnMinWidth[0], 'Column MinWidth default');
end;

procedure TTestGridStretch.TestNoStretchLeavesWidthsAlone;
begin
  SetupColumns(2, 50);
  FGrid.DistributeStretchWidths(400);
  CheckEquals(50, FGrid.ColumnWidth[0], 'Column 0 unchanged');
  CheckEquals(50, FGrid.ColumnWidth[1], 'Column 1 unchanged');
end;

procedure TTestGridStretch.TestSingleStretchColumnAbsorbsSlack;
begin
  SetupColumns(2, 50);
  FGrid.ColumnStretch[1] := 1;
  FGrid.DistributeStretchWidths(300);
  CheckEquals(50, FGrid.ColumnWidth[0], 'Fixed column keeps width');
  CheckEquals(250, FGrid.ColumnWidth[1], 'Stretch column absorbs the rest');
end;

procedure TTestGridStretch.TestFixedColumnsKeepTheirWidth;
begin
  SetupColumns(3, 40);
  FGrid.ColumnWidth[0] := 60;
  FGrid.ColumnWidth[1] := 70;
  FGrid.ColumnStretch[2] := 1;
  FGrid.DistributeStretchWidths(300);
  CheckEquals(60, FGrid.ColumnWidth[0], 'Column 0 fixed');
  CheckEquals(70, FGrid.ColumnWidth[1], 'Column 1 fixed');
  CheckEquals(170, FGrid.ColumnWidth[2], 'Column 2 takes 300-60-70');
end;

procedure TTestGridStretch.TestWeightsDistributeProportionally;
begin
  SetupColumns(2, 10);
  FGrid.ColumnStretch[0] := 2;
  FGrid.ColumnStretch[1] := 1;
  FGrid.DistributeStretchWidths(300);
  { 300 shared 2:1 }
  CheckEquals(200, FGrid.ColumnWidth[0], 'Weight 2 gets two thirds');
  CheckEquals(100, FGrid.ColumnWidth[1], 'Weight 1 gets one third');
end;

procedure TTestGridStretch.TestEqualWeightsSplitEvenly;
begin
  SetupColumns(2, 10);
  FGrid.ColumnStretch[0] := 1;
  FGrid.ColumnStretch[1] := 1;
  FGrid.DistributeStretchWidths(400);
  CheckEquals(200, FGrid.ColumnWidth[0], 'Even split, column 0');
  CheckEquals(200, FGrid.ColumnWidth[1], 'Even split, column 1');
end;

procedure TTestGridStretch.TestRemainderGoesToLastStretchColumn;
begin
  SetupColumns(3, 10);
  FGrid.ColumnStretch[0] := 1;
  FGrid.ColumnStretch[1] := 1;
  FGrid.ColumnStretch[2] := 1;
  { 100 / 3 = 33 remainder 1 }
  FGrid.DistributeStretchWidths(100);
  CheckEquals(33, FGrid.ColumnWidth[0], 'Column 0 base share');
  CheckEquals(33, FGrid.ColumnWidth[1], 'Column 1 base share');
  CheckEquals(34, FGrid.ColumnWidth[2], 'Last stretch column takes remainder');
  CheckEquals(100, FGrid.ColumnWidth[0] + FGrid.ColumnWidth[1] + FGrid.ColumnWidth[2],
      'Widths sum exactly to available width');
end;

procedure TTestGridStretch.TestShrinkRespectsMinWidth;
begin
  SetupColumns(2, 10);
  FGrid.ColumnMinWidth[0] := 50;
  FGrid.ColumnMinWidth[1] := 50;
  FGrid.ColumnStretch[0] := 1;
  FGrid.ColumnStretch[1] := 1;
  FGrid.DistributeStretchWidths(80);
  CheckEquals(50, FGrid.ColumnWidth[0], 'Column 0 clamped at MinWidth');
  CheckEquals(50, FGrid.ColumnWidth[1], 'Column 1 clamped at MinWidth');
end;

procedure TTestGridStretch.TestStretchGrowsFromBelowMinWidth;
begin
  SetupColumns(1, 10);
  FGrid.ColumnMinWidth[0] := 20;
  FGrid.ColumnStretch[0] := 1;
  FGrid.DistributeStretchWidths(200);
  CheckEquals(200, FGrid.ColumnWidth[0], 'MinWidth is a floor, not a cap');
end;

procedure TTestGridStretch.TestHasStretchColumns;
begin
  SetupColumns(2, 50);
  CheckFalse(FGrid.HasStretchColumns, 'No weights set');
  FGrid.ColumnStretch[1] := 1;
  CheckTrue(FGrid.HasStretchColumns, 'Weight set on column 1');
  FGrid.ColumnStretch[1] := 0;
  CheckFalse(FGrid.HasStretchColumns, 'Weight cleared again');
end;

initialization
  RegisterTests;

end.
