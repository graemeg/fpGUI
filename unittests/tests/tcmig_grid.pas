unit tcmig_grid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_miglayout,
  fpg_mig_cc,
  fpg_mig_layoututil;

type

  { TTestMigGrid }

  TTestMigGrid = class(TTestCase)
  published
    // CompWrap helper method tests
    procedure TestCorrectMinMax_AllInOrder;
    procedure TestCorrectMinMax_MinTooLarge;
    procedure TestCorrectMinMax_PrefTooSmall;
    procedure TestCorrectMinMax_PrefTooLarge;
    procedure TestCorrectMinMax_AllNeedCorrection;

    procedure TestConstrainSize_PositiveValue;
    procedure TestConstrainSize_Zero;
    procedure TestConstrainSize_Negative;
    procedure TestConstrainSize_INF;
    procedure TestConstrainSize_AboveINF;

    procedure TestGetGapIx_TopLeft;
    procedure TestGetGapIx_TopRight;
    procedure TestGetGapIx_BottomLeft;
    procedure TestGetGapIx_BottomRight;

    procedure TestFilter_ValidSize;
    procedure TestFilter_NotSet_MinType;
    procedure TestFilter_NotSet_PrefType;
    procedure TestFilter_NotSet_MaxType;

    // Grid/Cell tests
    procedure TestSplit_TwoComponentsShareCell;
    procedure TestSplit_ThreeComponentsShareCell;
  end;

procedure RegisterTests;

implementation

procedure RegisterTests;
begin
  RegisterTest(TTestMigGrid);
end;

{ TTestMigGrid }

procedure TTestMigGrid.TestCorrectMinMax_AllInOrder;
var
  cw: TfpgMigCompWrap;
  sizes: array[0..2] of Integer;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    sizes[SIZE_MIN] := 50;
    sizes[SIZE_PREF] := 100;
    sizes[SIZE_MAX] := 200;

    cw.CorrectMinMax(sizes);

    CheckEquals(50, sizes[SIZE_MIN], 'Min should remain unchanged');
    CheckEquals(100, sizes[SIZE_PREF], 'Pref should remain unchanged');
    CheckEquals(200, sizes[SIZE_MAX], 'Max should remain unchanged');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestCorrectMinMax_MinTooLarge;
var
  cw: TfpgMigCompWrap;
  sizes: array[0..2] of Integer;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    sizes[SIZE_MIN] := 250;  // Too large
    sizes[SIZE_PREF] := 100;
    sizes[SIZE_MAX] := 200;

    cw.CorrectMinMax(sizes);

    CheckEquals(200, sizes[SIZE_MIN], 'Min should be clamped to Max');
    CheckEquals(200, sizes[SIZE_PREF], 'Pref should be clamped to Max');
    CheckEquals(200, sizes[SIZE_MAX], 'Max should remain unchanged');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestCorrectMinMax_PrefTooSmall;
var
  cw: TfpgMigCompWrap;
  sizes: array[0..2] of Integer;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    sizes[SIZE_MIN] := 100;
    sizes[SIZE_PREF] := 50;  // Too small
    sizes[SIZE_MAX] := 200;

    cw.CorrectMinMax(sizes);

    CheckEquals(100, sizes[SIZE_MIN], 'Min should remain unchanged');
    CheckEquals(100, sizes[SIZE_PREF], 'Pref should be raised to Min');
    CheckEquals(200, sizes[SIZE_MAX], 'Max should remain unchanged');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestCorrectMinMax_PrefTooLarge;
var
  cw: TfpgMigCompWrap;
  sizes: array[0..2] of Integer;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    sizes[SIZE_MIN] := 50;
    sizes[SIZE_PREF] := 250;  // Too large
    sizes[SIZE_MAX] := 200;

    cw.CorrectMinMax(sizes);

    CheckEquals(50, sizes[SIZE_MIN], 'Min should remain unchanged');
    CheckEquals(200, sizes[SIZE_PREF], 'Pref should be clamped to Max');
    CheckEquals(200, sizes[SIZE_MAX], 'Max should remain unchanged');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestCorrectMinMax_AllNeedCorrection;
var
  cw: TfpgMigCompWrap;
  sizes: array[0..2] of Integer;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    sizes[SIZE_MIN] := 300;  // Too large
    sizes[SIZE_PREF] := 50;  // Will be too small after min is corrected
    sizes[SIZE_MAX] := 200;

    cw.CorrectMinMax(sizes);

    CheckEquals(200, sizes[SIZE_MIN], 'Min should be clamped to Max');
    CheckEquals(200, sizes[SIZE_PREF], 'Pref should be clamped to corrected Min');
    CheckEquals(200, sizes[SIZE_MAX], 'Max should remain unchanged');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestConstrainSize_PositiveValue;
var
  cw: TfpgMigCompWrap;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    CheckEquals(100, cw.ConstrainSize(100), 'Positive value should pass through');
    CheckEquals(1, cw.ConstrainSize(1), 'Small positive value should pass through');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestConstrainSize_Zero;
var
  cw: TfpgMigCompWrap;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    CheckEquals(0, cw.ConstrainSize(0), 'Zero should return 0');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestConstrainSize_Negative;
var
  cw: TfpgMigCompWrap;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    CheckEquals(0, cw.ConstrainSize(-10), 'Negative value should return 0');
    CheckEquals(0, cw.ConstrainSize(-100), 'Large negative value should return 0');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestConstrainSize_INF;
var
  cw: TfpgMigCompWrap;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    CheckEquals(INF, cw.ConstrainSize(INF), 'INF should pass through');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestConstrainSize_AboveINF;
var
  cw: TfpgMigCompWrap;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    CheckEquals(INF, cw.ConstrainSize(INF + 1000), 'Value above INF should be clamped to INF');
    CheckEquals(INF, cw.ConstrainSize(High(Integer)), 'MaxInt should be clamped to INF');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestGetGapIx_TopLeft;
var
  cw: TfpgMigCompWrap;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    CheckEquals(0, cw.GetGapIx(False, True), 'Top (vertical, true) should be index 0');
    CheckEquals(1, cw.GetGapIx(True, True), 'Left (horizontal, true) should be index 1');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestGetGapIx_TopRight;
var
  cw: TfpgMigCompWrap;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    CheckEquals(0, cw.GetGapIx(False, True), 'Top should be index 0');
    CheckEquals(3, cw.GetGapIx(True, False), 'Right (horizontal, false) should be index 3');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestGetGapIx_BottomLeft;
var
  cw: TfpgMigCompWrap;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    CheckEquals(2, cw.GetGapIx(False, False), 'Bottom (vertical, false) should be index 2');
    CheckEquals(1, cw.GetGapIx(True, True), 'Left should be index 1');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestGetGapIx_BottomRight;
var
  cw: TfpgMigCompWrap;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    CheckEquals(2, cw.GetGapIx(False, False), 'Bottom should be index 2');
    CheckEquals(3, cw.GetGapIx(True, False), 'Right should be index 3');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestFilter_ValidSize;
var
  cw: TfpgMigCompWrap;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    CheckEquals(100, cw.Filter(SIZE_MIN, 100), 'Valid MIN size should pass through');
    CheckEquals(150, cw.Filter(SIZE_PREF, 150), 'Valid PREF size should pass through');
    CheckEquals(200, cw.Filter(SIZE_MAX, 200), 'Valid MAX size should pass through');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestFilter_NotSet_MinType;
var
  cw: TfpgMigCompWrap;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    CheckEquals(0, cw.Filter(SIZE_MIN, NOT_SET), 'NOT_SET for MIN should return 0');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestFilter_NotSet_PrefType;
var
  cw: TfpgMigCompWrap;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    CheckEquals(0, cw.Filter(SIZE_PREF, NOT_SET), 'NOT_SET for PREF should return 0');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestFilter_NotSet_MaxType;
var
  cw: TfpgMigCompWrap;
begin
  cw := TfpgMigCompWrap.Create(nil, nil, 0, False);
  try
    CheckEquals(INF, cw.Filter(SIZE_MAX, NOT_SET), 'NOT_SET for MAX should return INF');
  finally
    cw.Free;
  end;
end;

procedure TTestMigGrid.TestSplit_TwoComponentsShareCell;
var
  container: TfpgWidget;
  mig: TfpgMigLayoutManager;
  w1, w2: TfpgWidget;
begin
  // Setup: Create container with MigLayout and two widgets
  // w1 has Split(2), w2 should share the same cell
  // Test verifies Split by checking widget positions after layout
//  fpgApplication.Initialize;
  try
    container := TfpgWidget.Create(nil);
    try
      container.Name := 'container';
      container.Width := 400;
      container.Height := 300;

      mig := TfpgMigLayoutManager.Create;
      container.LayoutManager := mig;

      w1 := TfpgWidget.Create(container);
      w1.Width := 80;
      w1.Height := 24;
      mig.AddLayoutComponent(w1, TfpgMigCC.Create().Split(2));

      w2 := TfpgWidget.Create(container);
      w2.Width := 80;
      w2.Height := 24;
      mig.AddLayoutComponent(w2, TfpgMigCC.Create());

      // Force layout
      container.Realign;

      // With Split(2), both widgets should be on same row (same Y position)
      // Without Split, w2 would be below w1
      CheckEquals(w1.Top, w2.Top, 'Both widgets should have same Y position (same row) due to Split');
      CheckTrue(w2.Left > w1.Left, 'Second widget should be to the right of first widget');
    finally
      container.Free;
    end;
  finally
//    fpgApplication.Terminate;
  end;
end;

procedure TTestMigGrid.TestSplit_ThreeComponentsShareCell;
var
  container: TfpgWidget;
  mig: TfpgMigLayoutManager;
  w1, w2, w3: TfpgWidget;
begin
  // Setup: Create container with MigLayout and three widgets
  // w1 has Split(3), w2 and w3 should share the same cell
  fpgApplication.Initialize;
  try
    container := TfpgWidget.Create(nil);
    try
      container.Width := 400;
      container.Height := 300;

      mig := TfpgMigLayoutManager.Create;
      container.LayoutManager := mig;

      w1 := TfpgWidget.Create(container);
      w1.Width := 80;
      w1.Height := 24;
      mig.AddLayoutComponent(w1, TfpgMigCC.Create().Split(3));

      w2 := TfpgWidget.Create(container);
      w2.Width := 80;
      w2.Height := 24;
      mig.AddLayoutComponent(w2, TfpgMigCC.Create());

      w3 := TfpgWidget.Create(container);
      w3.Width := 80;
      w3.Height := 24;
      mig.AddLayoutComponent(w3, TfpgMigCC.Create());

      // Force layout
      container.Realign;

      // With Split(3), all three widgets should be on same row (same Y position)
      CheckEquals(w1.Top, w2.Top, 'Widgets 1 and 2 should have same Y position');
      CheckEquals(w1.Top, w3.Top, 'Widgets 1 and 3 should have same Y position');
      CheckTrue(w2.Left > w1.Left, 'Widget 2 should be to the right of widget 1');
      CheckTrue(w3.Left > w2.Left, 'Widget 3 should be to the right of widget 2');
    finally
      container.Free;
    end;
  finally
    fpgApplication.Terminate;
  end;
end;

initialization
  RegisterTests;
end.
