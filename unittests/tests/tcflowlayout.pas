unit tcflowlayout;

{.$I fpg_defines.inc}
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
  fpg_widget,
  fpg_layouttypes,
  fpg_flowlayout;

type

  TTestFlowLayout = class(TTestCase)
  published
    procedure TestDefaults;
    procedure TestSimpleFlow;
    procedure TestWrappingFlow;
    procedure TestPreferredSizeWithWrapping;
    procedure TestDynamicResize;
  end;

procedure RegisterTests;

implementation

procedure RegisterTests;
begin
  RegisterTest(TTestFlowLayout);
end;

{ TTestFlowLayout }

procedure TTestFlowLayout.TestDefaults;
var
  lm: TfpgFlowLayoutManager;
begin
  lm := TfpgFlowLayoutManager.Create;
  try
    CheckEquals(5, lm.HGap, 'HGap default');
    CheckEquals(5, lm.VGap, 'VGap default');
  finally
    FreeAndNil(lm);
  end;
end;

procedure TTestFlowLayout.TestSimpleFlow;
var
  container: TfpgWidget;
  lm: ILayoutManager;
  w1, w2, w3: TfpgWidget;
begin
  container := TfpgWidget.Create(nil);
  container.Name := 'container';
  container.Width := 200;
  container.Height := 200;
  lm := TfpgFlowLayoutManager.Create as ILayoutManager;
  container.LayoutManager := lm;

  w1 := TfpgWidget.Create(container);
  w1.Name := 'w1';
  w1.Width := 50;
  w1.Height := 20;

  w2 := TfpgWidget.Create(container);
  w2.Name := 'w2';
  w2.Width := 50;
  w2.Height := 20;

  w3 := TfpgWidget.Create(container);
  w3.Name := 'w3';
  w3.Width := 50;
  w3.Height := 20;

  lm.AddLayoutComponent(w1, TfpgLayoutConstraint.Create());
  lm.AddLayoutComponent(w2, TfpgLayoutConstraint.Create());
  lm.AddLayoutComponent(w3, TfpgLayoutConstraint.Create());

  container.Realign;

  CheckEquals(5, w1.Left, 'w1.Left');
  CheckEquals(5, w1.Top, 'w1.Top');

  CheckEquals(60, w2.Left, 'w2.Left');
  CheckEquals(5, w2.Top, 'w2.Top');

  CheckEquals(115, w3.Left, 'w3.Left');
  CheckEquals(5, w3.Top, 'w3.Top');

  container.Free;
end;

procedure TTestFlowLayout.TestWrappingFlow;
var
  container: TfpgWidget;
  lm: ILayoutManager;
  w1, w2, w3: TfpgWidget;
begin
  container := TfpgWidget.Create(nil);
  container.Name := 'container';
  container.Width := 120;  // Narrow container
  container.Height := 200;
  lm := TfpgFlowLayoutManager.Create(0, 0) as ILayoutManager;
  container.LayoutManager := lm;

  w1 := TfpgWidget.Create(container);
  w1.Name := 'w1';
  w1.Width := 50;
  w1.Height := 20;

  w2 := TfpgWidget.Create(container);
  w2.Name := 'w2';
  w2.Width := 50;
  w2.Height := 20;

  w3 := TfpgWidget.Create(container);
  w3.Name := 'w3';
  w3.Width := 50;
  w3.Height := 20;

  lm.AddLayoutComponent(w1, TfpgLayoutConstraint.Create());
  lm.AddLayoutComponent(w2, TfpgLayoutConstraint.Create());
  lm.AddLayoutComponent(w3, TfpgLayoutConstraint.Create());

  container.Realign;

  // w1 and w2 should be on the first row
  CheckEquals(0, w1.Left, 'w1.Left');
  CheckEquals(0, w1.Top, 'w1.Top');
  CheckEquals(50, w2.Left, 'w2.Left');
  CheckEquals(0, w2.Top, 'w2.Top');

  // w3 should wrap to the second row
  CheckEquals(0, w3.Left, 'w3.Left after wrap');
  CheckEquals(20, w3.Top, 'w3.Top after wrap');

  container.Free;
end;

procedure TTestFlowLayout.TestPreferredSizeWithWrapping;
var
  container: TfpgWidget;
  lm: ILayoutManager;
  w1, w2, w3, w4, w5, w6, w7, w8, w9, w10: TfpgWidget;
  prefSize: TfpgSize;
begin
  // This test simulates the frm_simple scenario where a container (form)
  // has its size set explicitly, then a layout manager is assigned,
  // then widgets are added. The layout manager's GetPreferredSize should
  // not return a single-row width (which would be ~900 pixels), but should
  // consider wrapping based on the container's actual/preferred width.

  container := TfpgWidget.Create(nil);
  try
    container.Name := 'container';
    container.SetPosition(0, 0, 300, 250);  // Explicit size like SetPosition in frm_simple

    lm := TfpgFlowLayoutManager.Create as ILayoutManager;
    container.LayoutManager := lm;

    // Create 10 widgets with varying widths (like frm_simple does)
    w1 := TfpgWidget.Create(container);
    w1.Width := 65; w1.Height := 30;
    lm.AddLayoutComponent(w1, TfpgLayoutConstraint.Create());

    w2 := TfpgWidget.Create(container);
    w2.Width := 70; w2.Height := 30;
    lm.AddLayoutComponent(w2, TfpgLayoutConstraint.Create());

    w3 := TfpgWidget.Create(container);
    w3.Width := 75; w3.Height := 30;
    lm.AddLayoutComponent(w3, TfpgLayoutConstraint.Create());

    w4 := TfpgWidget.Create(container);
    w4.Width := 80; w4.Height := 30;
    lm.AddLayoutComponent(w4, TfpgLayoutConstraint.Create());

    w5 := TfpgWidget.Create(container);
    w5.Width := 85; w5.Height := 30;
    lm.AddLayoutComponent(w5, TfpgLayoutConstraint.Create());

    w6 := TfpgWidget.Create(container);
    w6.Width := 90; w6.Height := 30;
    lm.AddLayoutComponent(w6, TfpgLayoutConstraint.Create());

    w7 := TfpgWidget.Create(container);
    w7.Width := 95; w7.Height := 30;
    lm.AddLayoutComponent(w7, TfpgLayoutConstraint.Create());

    w8 := TfpgWidget.Create(container);
    w8.Width := 100; w8.Height := 30;
    lm.AddLayoutComponent(w8, TfpgLayoutConstraint.Create());

    w9 := TfpgWidget.Create(container);
    w9.Width := 105; w9.Height := 30;
    lm.AddLayoutComponent(w9, TfpgLayoutConstraint.Create());

    w10 := TfpgWidget.Create(container);
    w10.Width := 110; w10.Height := 30;
    lm.AddLayoutComponent(w10, TfpgLayoutConstraint.Create());

    // Get the preferred size - this should NOT be 900+ pixels wide
    prefSize := lm.GetPreferredSize(container);

    // The preferred width should be reasonable (close to container width of 300)
    // not the sum of all widget widths (~875)
    CheckTrue(prefSize.W < 500, Format('Preferred width should be < 500, got %d', [prefSize.W]));

    // The preferred height should account for multiple rows
    // With 300px width and varying widget widths, we should have 3-4 rows
    // Each row is 30px + 5px gap = 35px per row
    // So height should be around 105-140px (3-4 rows)
    CheckTrue(prefSize.H > 80, Format('Preferred height should be > 80 (multiple rows), got %d', [prefSize.H]));

  finally
    container.Free;
  end;
end;

procedure TTestFlowLayout.TestDynamicResize;
var
  container: TfpgWidget;
  lm: ILayoutManager;
  w1, w2, w3: TfpgWidget;
begin
  // Test that widgets rewrap when container is resized
  container := TfpgWidget.Create(nil);
  try
    container.Name := 'container';
    container.Width := 200;  // Wide enough for all 3 widgets in one row
    container.Height := 100;
    lm := TfpgFlowLayoutManager.Create(5, 5) as ILayoutManager;
    container.LayoutManager := lm;

    w1 := TfpgWidget.Create(container);
    w1.Name := 'w1';
    w1.Width := 50;
    w1.Height := 20;

    w2 := TfpgWidget.Create(container);
    w2.Name := 'w2';
    w2.Width := 50;
    w2.Height := 20;

    w3 := TfpgWidget.Create(container);
    w3.Name := 'w3';
    w3.Width := 50;
    w3.Height := 20;

    lm.AddLayoutComponent(w1, TfpgLayoutConstraint.Create());
    lm.AddLayoutComponent(w2, TfpgLayoutConstraint.Create());
    lm.AddLayoutComponent(w3, TfpgLayoutConstraint.Create());

    // Initial layout - all in one row
    container.Realign;
    CheckEquals(5, w1.Left, 'w1.Left initial');
    CheckEquals(5, w1.Top, 'w1.Top initial');
    CheckEquals(60, w2.Left, 'w2.Left initial');
    CheckEquals(5, w2.Top, 'w2.Top initial');
    CheckEquals(115, w3.Left, 'w3.Left initial');
    CheckEquals(5, w3.Top, 'w3.Top initial - all in one row');

    // Resize container to be narrower - should cause wrapping
    container.Width := 120;  // Only wide enough for 2 widgets
    container.Realign;

    // After resize, w1 and w2 should still be on first row
    CheckEquals(5, w1.Left, 'w1.Left after resize');
    CheckEquals(5, w1.Top, 'w1.Top after resize');
    CheckEquals(60, w2.Left, 'w2.Left after resize');
    CheckEquals(5, w2.Top, 'w2.Top after resize');

    // w3 should wrap to second row
    CheckEquals(5, w3.Left, 'w3.Left after resize - should wrap');
    CheckEquals(30, w3.Top, 'w3.Top after resize - should wrap');

    // Resize back to wider - should unwrap
    container.Width := 200;
    container.Realign;

    // All widgets should be back on one row
    CheckEquals(5, w1.Left, 'w1.Left after expand');
    CheckEquals(5, w1.Top, 'w1.Top after expand');
    CheckEquals(60, w2.Left, 'w2.Left after expand');
    CheckEquals(5, w2.Top, 'w2.Top after expand');
    CheckEquals(115, w3.Left, 'w3.Left after expand');
    CheckEquals(5, w3.Top, 'w3.Top after expand - should unwrap');

  finally
    container.Free;
  end;
end;

initialization
  RegisterTests;

end.
