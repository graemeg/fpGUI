unit tcmig_quickstart;

{
  MigLayout v11 Quick Start Guide Tests

  These tests replicate examples from the MigLayout Quick Start Guide
  to verify our implementation matches expected behavior.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpcunit, testregistry,
  fpg_base, fpg_main, fpg_widget, fpg_form, fpg_label,
  fpg_mig_lc, fpg_mig_cc, fpg_miglayout;

type
  { Test suite for Quick Start Guide examples }
  TTestMigQuickStart = class(TTestCase)
  private
    FForm: TfpgForm;
    FLayout: TfpgMigLayoutManager;
    FComp1, FComp2, FComp3, FComp4: TfpgLabel;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Test basic wrap - manual wrap on comp3 }
    procedure TestManualWrap_3Columns;

    { Test auto wrap - using "wrap 3" in layout constraints }
    procedure TestAutoWrap_3Columns;

    { Test cell spanning - comp2 spans 2 cells, comp4 spans whole row }
    procedure TestCellSpanning;
  end;

implementation

{ TTestMigQuickStart }

procedure TTestMigQuickStart.SetUp;
begin
  inherited SetUp;

  // Create form and components
  FForm := TfpgForm.Create(nil);
  FForm.Width := 400;
  FForm.Height := 300;

  FComp1 := TfpgLabel.Create(FForm);
  FComp1.Name := 'comp1';
  FComp1.Text := 'comp1';
  FComp1.Width := 80;
  FComp1.Height := 24;

  FComp2 := TfpgLabel.Create(FForm);
  FComp2.Name := 'comp2';
  FComp2.Text := 'comp2';
  FComp2.Width := 80;
  FComp2.Height := 24;

  FComp3 := TfpgLabel.Create(FForm);
  FComp3.Name := 'comp3';
  FComp3.Text := 'comp3';
  FComp3.Width := 80;
  FComp3.Height := 24;

  FComp4 := TfpgLabel.Create(FForm);
  FComp4.Name := 'comp4';
  FComp4.Text := 'comp4';
  FComp4.Width := 80;
  FComp4.Height := 24;
end;

procedure TTestMigQuickStart.TearDown;
begin
  FForm.Free;
  inherited TearDown;
end;

procedure TTestMigQuickStart.TestManualWrap_3Columns;
{
  Quick Start Guide Example 1:

    panel.add(comp1)
    panel.add(comp2)
    panel.add(comp3, "wrap")  // Wrap to next row
    panel.add(comp4)

    Expected grid:
    +-----------------------+
    | comp1 | comp2 | comp3 |
    +-----------------------+
    | comp4 |       |       |
    +-----------------------+
}
begin
  // Create layout manager (no auto-wrap)
  FLayout := TfpgMigLayoutManager.Create;
  FForm.LayoutManager := FLayout;

  // Add components with manual wrap on comp3
  FLayout.AddLayoutComponent(FComp1, TfpgMigCC.Create);
  FLayout.AddLayoutComponent(FComp2, TfpgMigCC.Create);
  FLayout.AddLayoutComponent(FComp3, TfpgMigCC.Create.Wrap);  // Wrap after this
  FLayout.AddLayoutComponent(FComp4, TfpgMigCC.Create);

  // Perform layout
  FForm.Realign;

  // Verify grid structure:
  // Row 1: comp1, comp2, comp3 (same Y position, different X)
  // Row 2: comp4 alone

  AssertEquals('comp1 and comp2 should be on same row',
    FComp1.Top, FComp2.Top);
  AssertEquals('comp1 and comp3 should be on same row',
    FComp1.Top, FComp3.Top);

  AssertTrue('comp1 should be left of comp2',
    FComp1.Left < FComp2.Left);
  AssertTrue('comp2 should be left of comp3',
    FComp2.Left < FComp3.Left);

  AssertTrue('comp4 should be on next row (below comp1)',
    FComp4.Top > FComp1.Top);
  AssertEquals('comp4 should align with comp1 horizontally',
    FComp1.Left, FComp4.Left);
end;

procedure TTestMigQuickStart.TestAutoWrap_3Columns;
{
  Quick Start Guide Example 2:

    MigLayout layout = new MigLayout("wrap 3");
    panel.add(comp1)
    panel.add(comp2)
    panel.add(comp3)  // Auto wrap after column 3
    panel.add(comp4)

    Expected grid (same as manual wrap):
    +-----------------------+
    | comp1 | comp2 | comp3 |
    +-----------------------+
    | comp4 |       |       |
    +-----------------------+
}
begin
  // Create layout manager with auto-wrap after 3 columns
  FLayout := TfpgMigLayoutManager.Create;
  FLayout.LC.WrapAfter(3);  // Auto wrap after 3 components
  FForm.LayoutManager := FLayout;

  // Add components - no manual wrap needed
  FLayout.AddLayoutComponent(FComp1, TfpgMigCC.Create);
  FLayout.AddLayoutComponent(FComp2, TfpgMigCC.Create);
  FLayout.AddLayoutComponent(FComp3, TfpgMigCC.Create);  // Will auto-wrap after this
  FLayout.AddLayoutComponent(FComp4, TfpgMigCC.Create);

  // Perform layout
  FForm.Realign;

  // Verify grid structure (same as TestManualWrap_3Columns)
  AssertEquals('comp1 and comp2 should be on same row',
    FComp1.Top, FComp2.Top);
  AssertEquals('comp1 and comp3 should be on same row',
    FComp1.Top, FComp3.Top);

  AssertTrue('comp1 should be left of comp2',
    FComp1.Left < FComp2.Left);
  AssertTrue('comp2 should be left of comp3',
    FComp2.Left < FComp3.Left);

  AssertTrue('comp4 should be on next row (below comp1)',
    FComp4.Top > FComp1.Top);
  AssertEquals('comp4 should align with comp1 horizontally',
    FComp1.Left, FComp4.Left);
end;

procedure TTestMigQuickStart.TestCellSpanning;
begin
  // Quick Start Guide: Merging and Splitting Cells
  // panel.add(comp1)
  // panel.add(comp2, "span 2")  // Component spans two cells
  // panel.add(comp3, "wrap")    // Wrap to next row
  // panel.add(comp4, "span")    // Span whole row
  //
  // Expected grid:
  // +-------------------------------+
  // | comp1 | comp2 (span 2) | comp3|
  // +-------------------------------+
  // |       comp4 (span row)        |
  // +-------------------------------+

  // Create layout manager
  FLayout := TfpgMigLayoutManager.Create;
  FForm.LayoutManager := FLayout;

  // Add components with spanning constraints
  FLayout.AddLayoutComponent(FComp1, TfpgMigCC.Create);
  FLayout.AddLayoutComponent(FComp2, TfpgMigCC.Create.SpanX(2).GrowX);  // Span 2 columns and grow
  FLayout.AddLayoutComponent(FComp3, TfpgMigCC.Create.Wrap);      // Wrap after this
  FLayout.AddLayoutComponent(FComp4, TfpgMigCC.Create.SpanX.GrowX);     // Span whole row and grow

  // Perform layout
  FForm.Realign;

  // Verify Row 1: comp1, comp2 (spanning 2), comp3
  AssertEquals('comp1 and comp2 should be on same row',
    FComp1.Top, FComp2.Top);
  AssertEquals('comp1 and comp3 should be on same row',
    FComp1.Top, FComp3.Top);

  AssertTrue('comp1 should be left of comp2',
    FComp1.Left < FComp2.Left);
  AssertTrue('comp2 should be left of comp3',
    FComp2.Left < FComp3.Left);

  // comp2 should span 2 cells, so it should be wider than comp1
  // NOTE: Use ActualWidth (set by layout manager), not Width (preferred size)
  AssertTrue('comp2 should be wider than comp1 (spans 2 cells)',
    FComp2.ActualWidth > FComp1.ActualWidth);

  // Verify Row 2: comp4 (spanning whole row)
  AssertTrue('comp4 should be on next row (below comp1)',
    FComp4.Top > FComp1.Top);
  AssertEquals('comp4 should align with comp1 horizontally',
    FComp1.Left, FComp4.Left);

  // comp4 should span multiple columns, so it should be wider than single-column components
  // NOTE: Use ActualWidth (set by layout manager), not Width (preferred size)
  AssertTrue('comp4 should be wider than comp1 (spans whole row)',
    FComp4.ActualWidth > FComp1.ActualWidth);
  AssertTrue('comp4 should be wider than comp2 (spans whole row)',
    FComp4.ActualWidth > FComp2.ActualWidth);
end;

initialization
  RegisterTest(TTestMigQuickStart);

end.
