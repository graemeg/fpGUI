unit tcmiglayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_form,
  fpg_button,
  fpg_layoutmanager,
  fpg_layouttypes,
  fpg_miglayout,
  fpg_mig_cc;

type

  { TTestMigLayout }

  TTestMigLayout = class(TTestCase)
  published
    procedure TestCreateMigLayout;
    procedure TestSingleWidgetLayout;
    procedure TestTwoWidgetsLayout;
    procedure TestTwoColumnLayout;
    procedure TestColumnWidthCalculation;
    procedure TestCellAlignment;
    procedure TestColumnSpan;
    procedure TestRowSpan;
    procedure TestGrow;
  end;

procedure RegisterTests;

implementation

procedure RegisterTests;
begin
  RegisterTest(TTestMigLayout);
end;

{ TTestMigLayout }

procedure TTestMigLayout.TestCreateMigLayout;
var
  mig: TfpgMigLayoutManager;
begin
  mig := TfpgMigLayoutManager.Create;
  try
    CheckNotNull(mig, 'MigLayout should be created');
    CheckNotNull(mig.LC, 'LC should be created');
    CheckNotNull(mig.RowConstraints, 'Row constraints should be created');
    CheckNotNull(mig.ColumnConstraints, 'Column constraints should be created');
    CheckTrue(mig.LC.IsFlowX, 'Default flow should be horizontal');
    // Default wrap is INF (no wrapping) - consistent with Java MigLayout
    CheckTrue(mig.LC.GetWrapAfter > 1000000, 'Default should be no wrap (large value)');
  finally
    mig.Free;
  end;
end;

procedure TTestMigLayout.TestSingleWidgetLayout;
var
  container: TfpgWidget;
  lm: ILayoutManager;
  w1: TfpgWidget;
begin
  container := TfpgWidget.Create(nil);
  container.Name := 'container';
  container.SetPosition(0, 0, 200, 200);
  lm := TfpgMigLayoutManager.Create as ILayoutManager;
  container.LayoutManager := lm;

  w1 := TfpgWidget.Create(container);
  w1.Name := 'w1';
  w1.SetPosition(0, 0, 50, 20);

  lm.AddLayoutComponent(w1, TfpgMigCC.Create());

  container.Realign;

  CheckEquals(6, w1.Left, 'w1.Left');
  CheckEquals(6, w1.Top, 'w1.Top');

  container.Free;
end;

procedure TTestMigLayout.TestTwoWidgetsLayout;
var
  container: TfpgWidget;
  lm: TfpgMigLayoutManager;
  w1, w2: TfpgWidget;
begin
  container := TfpgWidget.Create(nil);
  container.Name := 'container';
  container.SetPosition(0, 0, 200, 200);
  lm := TfpgMigLayoutManager.Create;
  lm.LC.SetWrapAfter(1);  // Wrap after each component (vertical stacking)
  container.LayoutManager := lm;

  w1 := TfpgWidget.Create(container);
  w1.Name := 'w1';
  w1.SetPosition(0, 0, 50, 20);
  lm.AddLayoutComponent(w1, TfpgMigCC.Create());

  w2 := TfpgWidget.Create(container);
  w2.Name := 'w2';
  w2.SetPosition(0, 0, 50, 20);
  lm.AddLayoutComponent(w2, TfpgMigCC.Create());

  container.Realign;

  CheckEquals(6, w1.Left, 'w1.Left');
  CheckEquals(6, w1.Top, 'w1.Top');

  CheckEquals(6, w2.Left, 'w2.Left');
  CheckEquals(32, w2.Top, 'w2.Top'); // 6 (gap) + 20 (w1.height) + 6 (gap)

  container.Free;
end;

procedure TTestMigLayout.TestTwoColumnLayout;
var
  container: TfpgWidget;
  lm: TfpgMigLayoutManager;
  w1, w2: TfpgWidget;
begin
  container := TfpgWidget.Create(nil);
  container.Name := 'container';
  container.SetPosition(0, 0, 200, 200);
  lm := TfpgMigLayoutManager.Create;
  lm.LC.SetWrapAfter(2);  // Wrap after 2 components (2 columns)
  container.LayoutManager := lm;

  w1 := TfpgWidget.Create(container);
  w1.Name := 'w1';
  w1.SetPosition(0, 0, 50, 20);
  lm.AddLayoutComponent(w1, TfpgMigCC.Create());

  w2 := TfpgWidget.Create(container);
  w2.Name := 'w2';
  w2.SetPosition(0, 0, 50, 20);
  lm.AddLayoutComponent(w2, TfpgMigCC.Create());

  container.Realign;

  CheckEquals(6, w1.Left, 'w1.Left');
  CheckEquals(6, w1.Top, 'w1.Top');

  CheckEquals(62, w2.Left, 'w2.Left'); // 6 (gap) + 50 (w1.width) + 6 (gap)
  CheckEquals(6, w2.Top, 'w2.Top');

  container.Free;
end;

procedure TTestMigLayout.TestColumnWidthCalculation;
var
  container: TfpgWidget;
  lm: TfpgMigLayoutManager;
  w1, w2, w3: TfpgWidget;
begin
  container := TfpgWidget.Create(nil);
  container.Name := 'container';
  container.SetPosition(0, 0, 300, 200);
  lm := TfpgMigLayoutManager.Create;
  lm.LC.SetWrapAfter(2);  // Wrap after 2 components (2 columns)
  container.LayoutManager := lm;

  w1 := TfpgWidget.Create(container);
  w1.Name := 'w1';
  w1.SetPosition(0, 0, 50, 20);
  lm.AddLayoutComponent(w1, TfpgMigCC.Create());

  w2 := TfpgWidget.Create(container);
  w2.Name := 'w2';
  w2.SetPosition(0, 0, 50, 20);
  lm.AddLayoutComponent(w2, TfpgMigCC.Create());

  w3 := TfpgWidget.Create(container);
  w3.Name := 'w3';
  w3.SetPosition(0, 0, 100, 20);
  lm.AddLayoutComponent(w3, TfpgMigCC.Create());

  container.Realign;

  // w1 is in column 0, row 0
  CheckEquals(6, w1.Left, 'w1.Left');
  CheckEquals(6, w1.Top, 'w1.Top');

  // w2 is in column 1, row 0
  // The first column width should be 100 because of w3
  CheckEquals(112, w2.Left, 'w2.Left'); // 6 (gap) + 100 (w3.width) + 6 (gap)
  CheckEquals(6, w2.Top, 'w2.Top');

  // w3 is in column 0, row 1
  CheckEquals(6, w3.Left, 'w3.Left');
  CheckEquals(32, w3.Top, 'w3.Top'); // 6 (gap) + 20 (max row height) + 6 (gap)

  container.Free;
end;

procedure TTestMigLayout.TestCellAlignment;
var
  container: TfpgWidget;
  lm: TfpgMigLayoutManager;
  w1: TfpgWidget;
  c: TfpgMigCC;
begin
  container := TfpgWidget.Create(nil);
  container.Name := 'container';
  container.SetPosition(0, 0, 200, 200);
  lm := TfpgMigLayoutManager.Create;
  lm.LC.Fill;  // Make cells fill container (Java MigLayout v11 way)
  container.LayoutManager := lm;

  w1 := TfpgWidget.Create(container);
  w1.Name := 'w1';
  w1.SetPosition(0, 0, 50, 20);

  c := TfpgMigCC.Create;
  c.AlignX('right').AlignY('bottom');
  lm.AddLayoutComponent(w1, c);

  container.Realign;

  // The cell is the whole container, so widget should be at the bottom right

  CheckEquals(container.Right - 50 - 6, w1.Left, 'w1.Left');  // 50 width, 6 padding
  CheckEquals(container.Bottom - 20 - 6, w1.Top, 'w1.Top');   // 20 height, 6 padding

  container.Free;
end;

procedure TTestMigLayout.TestColumnSpan;
var
  container: TfpgWidget;
  lm: TfpgMigLayoutManager;
  w1, w2: TfpgWidget;
  c: TfpgMigCC;
begin
  container := TfpgWidget.Create(nil);
  container.Name := 'container';
  container.SetPosition(0, 0, 200, 200);
  lm := TfpgMigLayoutManager.Create;
  lm.LC.SetWrapAfter(2);  // Wrap after 2 components (2 columns)
  container.LayoutManager := lm;

  w1 := TfpgWidget.Create(container);
  w1.Name := 'w1';
  w1.SetPosition(0, 0, 100, 20);
  c := TfpgMigCC.Create;
  c.SpanX(2);
  lm.AddLayoutComponent(w1, c);

  w2 := TfpgWidget.Create(container);
  w2.Name := 'w2';
  w2.SetPosition(0, 0, 50, 20);
  lm.AddLayoutComponent(w2, TfpgMigCC.Create());

  container.Realign;

  CheckEquals(6, w1.Left, 'w1.Left');
  CheckEquals(6, w1.Top, 'w1.Top');

  CheckEquals(6, w2.Left, 'w2.Left');
  CheckEquals(32, w2.Top, 'w2.Top'); // 6 (gap) + 20 (w1.height) + 6 (gap)

  container.Free;
end;

procedure TTestMigLayout.TestRowSpan;
var
  container: TfpgWidget;
  lm: TfpgMigLayoutManager;
  w1, w2, w3: TfpgWidget;
  c: TfpgMigCC;
begin
  container := TfpgWidget.Create(nil);
  container.Name := 'container';
  container.SetPosition(0, 0, 200, 200);
  lm := TfpgMigLayoutManager.Create;
  lm.LC.SetWrapAfter(2);  // Wrap after 2 components (2 columns)
  container.LayoutManager := lm;

  w1 := TfpgWidget.Create(container);
  w1.Name := 'w1';
  w1.SetPosition(0, 0, 50, 50);
  c := TfpgMigCC.Create;
  c.SpanY(2);
  lm.AddLayoutComponent(w1, c);

  w2 := TfpgWidget.Create(container);
  w2.Name := 'w2';
  w2.SetPosition(0, 0, 50, 20);
  lm.AddLayoutComponent(w2, TfpgMigCC.Create());

  w3 := TfpgWidget.Create(container);
  w3.Name := 'w3';
  w3.SetPosition(0, 0, 50, 20);
  lm.AddLayoutComponent(w3, TfpgMigCC.Create());

  container.Realign;

  CheckEquals(6, w1.Left, 'w1.Left');
  CheckEquals(6, w1.Top, 'w1.Top');

  CheckEquals(62, w2.Left, 'w2.Left');
  CheckEquals(6, w2.Top, 'w2.Top');

  CheckEquals(62, w3.Left, 'w3.Left');
  CheckEquals(32, w3.Top, 'w3.Top');

  container.Free;
end;

procedure TTestMigLayout.TestGrow;
var
  container: TfpgWidget;
  lm: TfpgMigLayoutManager;
  w1, w2: TfpgWidget;
  c1, c2: TfpgMigCC;
begin
  container := TfpgWidget.Create(nil);
  container.Name := 'container';
  container.SetPosition(0, 0, 200, 100);
  lm := TfpgMigLayoutManager.Create;
  lm.LC.SetWrapAfter(2);  // Wrap after 2 components (2 columns)
  lm.LC.FillX;  // Make columns fill container width (Java MigLayout v11 way)
  container.LayoutManager := lm;

  w1 := TfpgWidget.Create(container);
  w1.Name := 'w1';
  w1.SetPosition(0, 0, 50, 20);
  c1 := TfpgMigCC.Create;
  lm.AddLayoutComponent(w1, c1);

  w2 := TfpgWidget.Create(container);
  w2.Name := 'w2';
  w2.SetPosition(0, 0, 50, 20);
  c2 := TfpgMigCC.Create;
  c2.GrowX.AlignX('fill');
  lm.AddLayoutComponent(w2, c2);

  container.Realign;

  CheckEquals(6, w1.Left, 'w1.Left');
  CheckEquals(50, w1.Width, 'w1.Width');

  CheckEquals(62, w2.Left, 'w2.Left');
  CheckEquals(132, w2.Width, 'w2.Width');

  container.Free;
end;

initialization
  RegisterTests;
end.
