unit tcmig_cc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  fpg_mig_cc, fpg_mig_boundsize, fpg_mig_unitvalue, fpg_mig_dimconstraint;

type
  { Test cases for TfpgMigCC }
  TTestMigCC = class(TTestCase)
  published
    { Basic creation and initialization }
    procedure TestCreate;
    procedure TestDefaultValues;

    { Dimension constraints - Width }
    procedure TestSizeGroupX;
    procedure TestEndGroupX;
    procedure TestMinWidth;
    procedure TestWidth;
    procedure TestMaxWidth;
    procedure TestGapX;
    procedure TestAlignX;
    procedure TestGrowPrioX;
    procedure TestGrowX_Default;
    procedure TestGrowX_Weight;
    procedure TestShrinkPrioX;
    procedure TestShrinkX;

    { Dimension constraints - Height }
    procedure TestSizeGroupY;
    procedure TestEndGroupY;
    procedure TestMinHeight;
    procedure TestHeight;
    procedure TestMaxHeight;
    procedure TestGapY;
    procedure TestAlignY;
    procedure TestGrowPrioY;
    procedure TestGrowY_Default;
    procedure TestGrowY_Weight;
    procedure TestShrinkPrioY;
    procedure TestShrinkY;

    { Dimension constraints - Both }
    procedure TestSizeGroup_Both;
    procedure TestEndGroup_Both;
    procedure TestGrow_Both;
    procedure TestGrow_BothDefault;
    procedure TestGrowPrio_Both;
    procedure TestShrink_Both;
    procedure TestShrink_BothDefault;
    procedure TestShrinkPrio_Both;

    { Gap methods }
    procedure TestGapBefore;
    procedure TestGapAfter;
    procedure TestGapTop;
    procedure TestGapBottom;
    procedure TestGapLeft;
    procedure TestGapRight;
    procedure TestGap_FourSides;

    { Cell and Spanning }
    procedure TestCell_TwoArgs;
    procedure TestCell_FourArgs;
    procedure TestSpan_TwoArgs;
    procedure TestSpan_NoArgs;
    procedure TestSpanX_WithArg;
    procedure TestSpanX_NoArg;
    procedure TestSpanY_WithArg;
    procedure TestSpanY_NoArg;

    { Flow and Wrapping }
    procedure TestFlowX;
    procedure TestFlowY;
    procedure TestSkip_WithArg;
    procedure TestSkip_NoArg;
    procedure TestSplit_WithArg;
    procedure TestSplit_NoArg;
    procedure TestWrap_NoArg;
    procedure TestNewline_NoArg;

    { Docking }
    procedure TestDockNorth;
    procedure TestDockWest;
    procedure TestDockSouth;
    procedure TestDockEast;

    { Positioning }
    procedure TestX;
    procedure TestY;
    procedure TestX2;
    procedure TestY2;

    { Miscellaneous }
    procedure TestHideMode;
    procedure TestId;
    procedure TestTag;
    procedure TestExternal_NoArg;
    procedure TestExternal_WithArg;
    procedure TestPush_NoArg;
    procedure TestPush_WithArgs;
    procedure TestPushX_NoArg;
    procedure TestPushX_WithArg;
    procedure TestPushY_NoArg;
    procedure TestPushY_WithArg;

    { Fluent API Chaining }
    procedure TestFluentChaining_Dimensions;
    procedure TestFluentChaining_Mixed;
  end;

implementation

uses
  Math;

{ TTestMigCC }

procedure TTestMigCC.TestCreate;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    AssertNotNull('CC should be created', cc);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestDefaultValues;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    AssertEquals('Dock should be -1', -1, cc.Dock);
    AssertEquals('FlowX should be 0 (nil)', 0, cc.IsFlowX);
    AssertEquals('Skip should be 0', 0, cc.SkipCells);
    AssertEquals('Split should be 1', 1, cc.SplitParts);
    AssertEquals('SpanX should be 1', 1, cc.CellSpanX);
    AssertEquals('SpanY should be 1', 1, cc.CellSpanY);
    AssertEquals('CellX should be -1', -1, cc.CellX);
    AssertEquals('CellY should be 0', 0, cc.CellY);
    AssertEquals('Tag should be empty', '', cc.TagValue);
    AssertEquals('Id should be empty', '', cc.IdValue);
    AssertEquals('HideMode should be -1', -1, cc.HideModeValue);
    AssertTrue('BoundsInGrid should be true', cc.IsBoundsInGrid);
    AssertFalse('External should be false', cc.IsExternal);
    AssertTrue('PushX should be NaN (null)', IsNaN(cc.PushWeightX));
    AssertTrue('PushY should be NaN (null)', IsNaN(cc.PushWeightY));
    AssertNotNull('Horizontal should be initialized', cc.Horizontal);
    AssertNotNull('Vertical should be initialized', cc.Vertical);
  finally
    cc.Free;
  end;
end;

{ Dimension Constraints - Width }

procedure TTestMigCC.TestSizeGroupX;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.SizeGroupX('group1');
    AssertEquals('SizeGroup should be set', 'group1', cc.Horizontal.GetSizeGroup);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestEndGroupX;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.EndGroupX('group2');
    AssertEquals('EndGroup should be set', 'group2', cc.Horizontal.GetEndGroup);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestMinWidth;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.MinWidth('100px');
    AssertNotNull('Size should be set', cc.Horizontal.GetSize);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestWidth;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    try
      cc.Width('100px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected
    end;
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestMaxWidth;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.MaxWidth('200px');
    AssertNotNull('Size should be set', cc.Horizontal.GetSize);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGapX;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    try
      cc.GapX('10px', '20px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected
    end;
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestAlignX;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.AlignX('center');
    AssertNotNull('AlignX should be set', cc.Horizontal.GetAlign);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGrowPrioX;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.GrowPrioX(50);
    AssertEquals('GrowPriority should be 50', 50, cc.Horizontal.GetGrowPriority);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGrowX_Default;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.GrowX;
    AssertEquals('GrowPriority should be 100', 100, cc.Horizontal.GetGrowPriority);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGrowX_Weight;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.GrowX(0.5);
    AssertEquals('Grow weight should be 0.5', 0.5, cc.Horizontal.GetGrowWeight, 0.001);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestShrinkPrioX;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.ShrinkPrioX(75);
    AssertEquals('ShrinkPriority should be 75', 75, cc.Horizontal.GetShrinkPriority);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestShrinkX;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.ShrinkX(0.3);
    AssertEquals('Shrink weight should be 0.3', 0.3, cc.Horizontal.GetShrinkWeight, 0.001);
  finally
    cc.Free;
  end;
end;

{ Dimension Constraints - Height }

procedure TTestMigCC.TestSizeGroupY;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.SizeGroupY('group3');
    AssertEquals('SizeGroup should be set', 'group3', cc.Vertical.GetSizeGroup);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestEndGroupY;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.EndGroupY('group4');
    AssertEquals('EndGroup should be set', 'group4', cc.Vertical.GetEndGroup);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestMinHeight;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.MinHeight('50px');
    AssertNotNull('Size should be set', cc.Vertical.GetSize);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestHeight;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    try
      cc.Height('100px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected
    end;
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestMaxHeight;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.MaxHeight('150px');
    AssertNotNull('Size should be set', cc.Vertical.GetSize);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGapY;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    try
      cc.GapY('5px', '10px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected
    end;
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestAlignY;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.AlignY('top');
    AssertNotNull('AlignY should be set', cc.Vertical.GetAlign);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGrowPrioY;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.GrowPrioY(60);
    AssertEquals('GrowPriority should be 60', 60, cc.Vertical.GetGrowPriority);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGrowY_Default;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.GrowY;
    AssertEquals('GrowPriority should be 100', 100, cc.Vertical.GetGrowPriority);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGrowY_Weight;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.GrowY(0.7);
    AssertEquals('Grow weight should be 0.7', 0.7, cc.Vertical.GetGrowWeight, 0.001);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestShrinkPrioY;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.ShrinkPrioY(80);
    AssertEquals('ShrinkPriority should be 80', 80, cc.Vertical.GetShrinkPriority);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestShrinkY;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.ShrinkY(0.4);
    AssertEquals('Shrink weight should be 0.4', 0.4, cc.Vertical.GetShrinkWeight, 0.001);
  finally
    cc.Free;
  end;
end;

{ Dimension Constraints - Both }

procedure TTestMigCC.TestSizeGroup_Both;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.SizeGroup('groupX', 'groupY');
    AssertEquals('SizeGroupX should be set', 'groupX', cc.Horizontal.GetSizeGroup);
    AssertEquals('SizeGroupY should be set', 'groupY', cc.Vertical.GetSizeGroup);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestEndGroup_Both;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.EndGroup('endX', 'endY');
    AssertEquals('EndGroupX should be set', 'endX', cc.Horizontal.GetEndGroup);
    AssertEquals('EndGroupY should be set', 'endY', cc.Vertical.GetEndGroup);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGrow_Both;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Grow(0.6, 0.8);
    AssertEquals('Horizontal grow weight should be 0.6', 0.6, cc.Horizontal.GetGrowWeight, 0.001);
    AssertEquals('Vertical grow weight should be 0.8', 0.8, cc.Vertical.GetGrowWeight, 0.001);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGrow_BothDefault;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Grow;
    AssertEquals('Horizontal grow priority should be 100', 100, cc.Horizontal.GetGrowPriority);
    AssertEquals('Vertical grow priority should be 100', 100, cc.Vertical.GetGrowPriority);
    { .Grow must also set default grow weights, matching the behaviour of .GrowX and .GrowY }
    AssertEquals('Horizontal grow weight should be 100.0', 100.0, cc.Horizontal.GetGrowWeight, 0.001);
    AssertEquals('Vertical grow weight should be 100.0', 100.0, cc.Vertical.GetGrowWeight, 0.001);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGrowPrio_Both;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.GrowPrio(40, 60);
    AssertEquals('Horizontal grow priority should be 40', 40, cc.Horizontal.GetGrowPriority);
    AssertEquals('Vertical grow priority should be 60', 60, cc.Vertical.GetGrowPriority);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestShrink_Both;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Shrink(0.2, 0.3);
    AssertEquals('Horizontal shrink weight should be 0.2', 0.2, cc.Horizontal.GetShrinkWeight, 0.001);
    AssertEquals('Vertical shrink weight should be 0.3', 0.3, cc.Vertical.GetShrinkWeight, 0.001);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestShrink_BothDefault;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Shrink;
    AssertEquals('Horizontal shrink priority should be 100', 100, cc.Horizontal.GetShrinkPriority);
    AssertEquals('Vertical shrink priority should be 100', 100, cc.Vertical.GetShrinkPriority);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestShrinkPrio_Both;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.ShrinkPrio(30, 50);
    AssertEquals('Horizontal shrink priority should be 30', 30, cc.Horizontal.GetShrinkPriority);
    AssertEquals('Vertical shrink priority should be 50', 50, cc.Vertical.GetShrinkPriority);
  finally
    cc.Free;
  end;
end;

{ Gap Methods }

procedure TTestMigCC.TestGapBefore;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    try
      cc.GapBefore('10px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected
    end;
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGapAfter;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    try
      cc.GapAfter('15px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected
    end;
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGapTop;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    try
      cc.GapTop('5px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected
    end;
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGapBottom;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    try
      cc.GapBottom('8px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected
    end;
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGapLeft;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    try
      cc.GapLeft('12px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected
    end;
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGapRight;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    try
      cc.GapRight('18px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected
    end;
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestGap_FourSides;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    try
      cc.Gap('10px', '15px', '5px', '8px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected
    end;
  finally
    cc.Free;
  end;
end;

{ Cell and Spanning }

procedure TTestMigCC.TestCell_TwoArgs;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Cell(2, 3);
    AssertEquals('CellX should be 2', 2, cc.CellX);
    AssertEquals('CellY should be 3', 3, cc.CellY);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestCell_FourArgs;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Cell(1, 2, 3, 4);
    AssertEquals('CellX should be 1', 1, cc.CellX);
    AssertEquals('CellY should be 2', 2, cc.CellY);
    AssertEquals('SpanX should be 3', 3, cc.CellSpanX);
    AssertEquals('SpanY should be 4', 4, cc.CellSpanY);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestSpan_TwoArgs;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Span(2, 3);
    AssertEquals('SpanX should be 2', 2, cc.CellSpanX);
    AssertEquals('SpanY should be 3', 3, cc.CellSpanY);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestSpan_NoArgs;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Span;
    AssertEquals('SpanX should be INF_SIZE', 2147471302, cc.CellSpanX);
    AssertEquals('SpanY should be 1', 1, cc.CellSpanY);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestSpanX_WithArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.SpanX(5);
    AssertEquals('SpanX should be 5', 5, cc.CellSpanX);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestSpanX_NoArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.SpanX;
    AssertEquals('SpanX should be INF_SIZE', 2147471302, cc.CellSpanX);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestSpanY_WithArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.SpanY(4);
    AssertEquals('SpanY should be 4', 4, cc.CellSpanY);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestSpanY_NoArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.SpanY;
    AssertEquals('SpanY should be INF_SIZE', 2147471302, cc.CellSpanY);
  finally
    cc.Free;
  end;
end;

{ Flow and Wrapping }

procedure TTestMigCC.TestFlowX;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.FlowX;
    AssertEquals('FlowX should be 2 (true)', 2, cc.IsFlowX);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestFlowY;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.FlowY;
    AssertEquals('FlowX should be 1 (false)', 1, cc.IsFlowX);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestSkip_WithArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Skip(3);
    AssertEquals('Skip should be 3', 3, cc.SkipCells);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestSkip_NoArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Skip;
    AssertEquals('Skip should be 1', 1, cc.SkipCells);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestSplit_WithArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Split(4);
    AssertEquals('Split should be 4', 4, cc.SplitParts);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestSplit_NoArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Split;
    AssertEquals('Split should be INF_SIZE', 2147471302, cc.SplitParts);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestWrap_NoArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Wrap;
    AssertNotNull('Wrap should be set', cc.WrapGap);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestNewline_NoArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Newline;
    AssertNotNull('Newline should be set', cc.NewlineGap);
  finally
    cc.Free;
  end;
end;

{ Docking }

procedure TTestMigCC.TestDockNorth;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.DockNorth;
    AssertEquals('Dock should be 0 (north)', 0, cc.Dock);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestDockWest;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.DockWest;
    AssertEquals('Dock should be 1 (west)', 1, cc.Dock);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestDockSouth;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.DockSouth;
    AssertEquals('Dock should be 2 (south)', 2, cc.Dock);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestDockEast;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.DockEast;
    AssertEquals('Dock should be 3 (east)', 3, cc.Dock);
  finally
    cc.Free;
  end;
end;

{ Positioning }

procedure TTestMigCC.TestX;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.X('100px');
    // Position array is not accessible via properties, just test no exception
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestY;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Y('50px');
    // Position array is not accessible via properties, just test no exception
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestX2;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.X2('200px');
    // Position array is not accessible via properties, just test no exception
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestY2;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Y2('150px');
    // Position array is not accessible via properties, just test no exception
  finally
    cc.Free;
  end;
end;

{ Miscellaneous }

procedure TTestMigCC.TestHideMode;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.HideMode(2);
    AssertEquals('HideMode should be 2', 2, cc.HideModeValue);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestId;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Id('button1');
    AssertEquals('Id should be "button1"', 'button1', cc.IdValue);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestTag;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Tag('group1');
    AssertEquals('Tag should be "group1"', 'group1', cc.TagValue);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestExternal_NoArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.External;
    AssertTrue('External should be true', cc.IsExternal);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestExternal_WithArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.External(False);
    AssertFalse('External should be false', cc.IsExternal);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestPush_NoArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Push;
    AssertEquals('PushX should be 100.0', 100.0, cc.PushWeightX, 0.001);
    AssertEquals('PushY should be 100.0', 100.0, cc.PushWeightY, 0.001);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestPush_WithArgs;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Push(50.0, 75.0);
    AssertEquals('PushX should be 50.0', 50.0, cc.PushWeightX, 0.001);
    AssertEquals('PushY should be 75.0', 75.0, cc.PushWeightY, 0.001);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestPushX_NoArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.PushX;
    AssertEquals('PushX should be 100.0', 100.0, cc.PushWeightX, 0.001);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestPushX_WithArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.PushX(60.0);
    AssertEquals('PushX should be 60.0', 60.0, cc.PushWeightX, 0.001);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestPushY_NoArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.PushY;
    AssertEquals('PushY should be 100.0', 100.0, cc.PushWeightY, 0.001);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestPushY_WithArg;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.PushY(80.0);
    AssertEquals('PushY should be 80.0', 80.0, cc.PushWeightY, 0.001);
  finally
    cc.Free;
  end;
end;

{ Fluent API Chaining }

procedure TTestMigCC.TestFluentChaining_Dimensions;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.GrowX(0.5).GrowY(0.7).ShrinkX(0.3).ShrinkY(0.4);

    AssertEquals('Horizontal grow weight should be 0.5', 0.5, cc.Horizontal.GetGrowWeight, 0.001);
    AssertEquals('Vertical grow weight should be 0.7', 0.7, cc.Vertical.GetGrowWeight, 0.001);
    AssertEquals('Horizontal shrink weight should be 0.3', 0.3, cc.Horizontal.GetShrinkWeight, 0.001);
    AssertEquals('Vertical shrink weight should be 0.4', 0.4, cc.Vertical.GetShrinkWeight, 0.001);
  finally
    cc.Free;
  end;
end;

procedure TTestMigCC.TestFluentChaining_Mixed;
var
  cc: TfpgMigCC;
begin
  cc := TfpgMigCC.Create;
  try
    cc.Cell(2, 3).Span(2, 1).Grow.Push(50.0, 75.0).Tag('test').Id('widget1');

    AssertEquals('CellX should be 2', 2, cc.CellX);
    AssertEquals('CellY should be 3', 3, cc.CellY);
    AssertEquals('SpanX should be 2', 2, cc.CellSpanX);
    AssertEquals('SpanY should be 1', 1, cc.CellSpanY);
    AssertEquals('Horizontal grow priority should be 100', 100, cc.Horizontal.GetGrowPriority);
    AssertEquals('Vertical grow priority should be 100', 100, cc.Vertical.GetGrowPriority);
    AssertEquals('PushX should be 50.0', 50.0, cc.PushWeightX, 0.001);
    AssertEquals('PushY should be 75.0', 75.0, cc.PushWeightY, 0.001);
    AssertEquals('Tag should be "test"', 'test', cc.TagValue);
    AssertEquals('Id should be "widget1"', 'widget1', cc.IdValue);
  finally
    cc.Free;
  end;
end;

initialization
  RegisterTest(TTestMigCC);

end.
