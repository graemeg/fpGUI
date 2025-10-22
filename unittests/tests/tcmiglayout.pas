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
  fpg_migconstraint;

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
    CheckEquals(1, mig.ColumnCount, 'Default ColumnCount should be 1');
    CheckEquals(6, mig.RowGap, 'Default RowGap should be 6');
    CheckEquals(6, mig.ColumnGap, 'Default ColumnGap should be 6');
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

  lm.AddLayoutComponent(w1, TfpgMigConstraint.Create());

  container.Realign;

  CheckEquals(6, w1.Left, 'w1.Left');
  CheckEquals(6, w1.Top, 'w1.Top');

  container.Free;
end;

procedure TTestMigLayout.TestTwoWidgetsLayout;
var
  container: TfpgWidget;
  lm: ILayoutManager;
  w1, w2: TfpgWidget;
begin
  container := TfpgWidget.Create(nil);
  container.Name := 'container';
  container.SetPosition(0, 0, 200, 200);
  lm := TfpgMigLayoutManager.Create as ILayoutManager;
  container.LayoutManager := lm;

  w1 := TfpgWidget.Create(container);
  w1.Name := 'w1';
  w1.SetPosition(0, 0, 50, 20);
  lm.AddLayoutComponent(w1, TfpgMigConstraint.Create());

  w2 := TfpgWidget.Create(container);
  w2.Name := 'w2';
  w2.SetPosition(0, 0, 50, 20);
  lm.AddLayoutComponent(w2, TfpgMigConstraint.Create());

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
  lm.ColumnCount := 2;
  container.LayoutManager := lm;

  w1 := TfpgWidget.Create(container);
  w1.Name := 'w1';
  w1.SetPosition(0, 0, 50, 20);
  lm.AddLayoutComponent(w1, TfpgMigConstraint.Create());

  w2 := TfpgWidget.Create(container);
  w2.Name := 'w2';
  w2.SetPosition(0, 0, 50, 20);
  lm.AddLayoutComponent(w2, TfpgMigConstraint.Create());

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
  lm.ColumnCount := 2;
  container.LayoutManager := lm;

  w1 := TfpgWidget.Create(container);
  w1.Name := 'w1';
  w1.SetPosition(0, 0, 50, 20);
  lm.AddLayoutComponent(w1, TfpgMigConstraint.Create());

  w2 := TfpgWidget.Create(container);
  w2.Name := 'w2';
  w2.SetPosition(0, 0, 50, 20);
  lm.AddLayoutComponent(w2, TfpgMigConstraint.Create());

  w3 := TfpgWidget.Create(container);
  w3.Name := 'w3';
  w3.SetPosition(0, 0, 100, 20);
  lm.AddLayoutComponent(w3, TfpgMigConstraint.Create());

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
  c: TfpgMigConstraint;
begin
  container := TfpgWidget.Create(nil);
  container.Name := 'container';
  container.SetPosition(0, 0, 200, 200);
  lm := TfpgMigLayoutManager.Create;
  container.LayoutManager := lm;

  w1 := TfpgWidget.Create(container);
  w1.Name := 'w1';
  w1.SetPosition(0, 0, 50, 20);

  c := TfpgMigConstraint.Create;
  c.AlignX := axRight;
  c.AlignY := ayBottom;
  lm.AddLayoutComponent(w1, c);

  container.Realign;

  // The cell is the whole container, so widget should be at the bottom right
  CheckEquals(200 - 50 - 6, w1.Left, 'w1.Left');
  CheckEquals(200 - 20 - 6, w1.Top, 'w1.Top');

  container.Free;
end;

initialization
  RegisterTests;
end.
