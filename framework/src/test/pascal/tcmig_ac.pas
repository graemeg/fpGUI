unit tcmig_ac;

{
  Unit tests for TfpgMigAC (Axis Constraints)
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  fpg_mig_ac, fpg_mig_dimconstraint, fpg_mig_boundsize, fpg_mig_unitvalue;

type
  { Local type alias for test readability }
  TfpgMigDimConstraintArray = fpg_mig_ac.TfpgMigDimConstraintArray;

type
  TTestMigAC = class(TTestCase)
  published
    { Basic creation and structure }
    procedure TestCreate;
    procedure TestGetCount;
    procedure TestCount;
    procedure TestIndex;

    { NoGrid property }
    procedure TestNoGrid_Current;
    procedure TestNoGrid_Specific;

    { Fill property }
    procedure TestFill_Current;
    procedure TestFill_Specific;

    { Size group }
    procedure TestSizeGroup_Empty;
    procedure TestSizeGroup_Named;
    procedure TestSizeGroup_Specific;

    { Gap methods }
    procedure TestGap_Default;
    procedure TestGap_MoveToNext;

    { Align methods }
    procedure TestAlign_Current;
    procedure TestAlign_Specific;

    { Grow priority }
    procedure TestGrowPrio_Current;
    procedure TestGrowPrio_Specific;

    { Grow weight }
    procedure TestGrow_Default;
    procedure TestGrow_Weight;
    procedure TestGrow_Specific;

    { Shrink priority }
    procedure TestShrinkPrio_Current;
    procedure TestShrinkPrio_Specific;

    { Shrink weight }
    procedure TestShrink_Default;
    procedure TestShrink_Weight;
    procedure TestShrink_Specific;

    { Fluent API chaining }
    procedure TestFluentChaining;

    { Get/Set constraints }
    procedure TestGetConstraints;
    procedure TestSetConstraints;
  end;

implementation

{ TTestMigAC }

procedure TTestMigAC.TestCreate;
var
  ac: TfpgMigAC;
begin
  ac := TfpgMigAC.Create;
  try
    AssertEquals('Should start with 1 constraint', 1, ac.GetCount);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestGetCount;
var
  ac: TfpgMigAC;
begin
  ac := TfpgMigAC.Create;
  try
    AssertEquals('Initial count should be 1', 1, ac.GetCount);

    ac.Index(2);
    AssertEquals('Count should be 3 after Index(2)', 3, ac.GetCount);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestCount;
var
  ac: TfpgMigAC;
begin
  ac := TfpgMigAC.Create;
  try
    ac.Count(5);
    AssertEquals('Count should be 6 after Count(5)', 6, ac.GetCount);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestIndex;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.Index(2).Fill;
    constraints := ac.GetConstraints;

    AssertEquals('Should have 3 constraints', 3, Length(constraints));
    AssertTrue('Index 2 should be filled', constraints[2].IsFill);
    AssertFalse('Index 0 should not be filled', constraints[0].IsFill);
    AssertFalse('Index 1 should not be filled', constraints[1].IsFill);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestNoGrid_Current;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.NoGrid;
    constraints := ac.GetConstraints;

    AssertTrue('Current (0) should have NoGrid', constraints[0].IsNoGrid);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestNoGrid_Specific;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.NoGrid(2);
    constraints := ac.GetConstraints;

    AssertEquals('Should have 3 constraints', 3, Length(constraints));
    AssertTrue('Index 2 should have NoGrid', constraints[2].IsNoGrid);
    AssertFalse('Index 0 should not have NoGrid', constraints[0].IsNoGrid);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestFill_Current;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.Fill;
    constraints := ac.GetConstraints;

    AssertTrue('Current (0) should be filled', constraints[0].IsFill);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestFill_Specific;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.Fill(1);
    constraints := ac.GetConstraints;

    AssertEquals('Should have 2 constraints', 2, Length(constraints));
    AssertTrue('Index 1 should be filled', constraints[1].IsFill);
    AssertFalse('Index 0 should not be filled', constraints[0].IsFill);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestSizeGroup_Empty;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.SizeGroup;
    constraints := ac.GetConstraints;

    AssertEquals('Size group should be empty string', '', constraints[0].GetSizeGroup);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestSizeGroup_Named;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.SizeGroup('group1');
    constraints := ac.GetConstraints;

    AssertEquals('Size group should be "group1"', 'group1', constraints[0].GetSizeGroup);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestSizeGroup_Specific;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.SizeGroup('mygroup', 2);
    constraints := ac.GetConstraints;

    AssertEquals('Index 2 size group should be "mygroup"', 'mygroup', constraints[2].GetSizeGroup);
    AssertEquals('Index 0 size group should be empty', '', constraints[0].GetSizeGroup);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestGap_Default;
var
  ac: TfpgMigAC;
begin
  ac := TfpgMigAC.Create;
  try
    ac.Gap;
    AssertEquals('Gap should create constraint at new index', 2, ac.GetCount);

    ac.Gap;
    AssertEquals('Second gap should create constraint at index 2', 3, ac.GetCount);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestGap_MoveToNext;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.Fill.Gap.Fill;  // Fill index 0, gap, then fill index 1
    constraints := ac.GetConstraints;

    AssertEquals('Should have 2 constraints', 2, Length(constraints));
    AssertTrue('Index 0 should be filled', constraints[0].IsFill);
    AssertTrue('Index 1 should be filled', constraints[1].IsFill);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestAlign_Current;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
  align: TfpgMigUnitValue;
begin
  ac := TfpgMigAC.Create;
  try
    ac.Align('center');
    constraints := ac.GetConstraints;

    align := constraints[0].GetAlign;
    AssertNotNull('Alignment should be set', align);
    AssertSame('Should be center alignment', UnitValueCenter, align);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestAlign_Specific;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.Align('center', 1);
    constraints := ac.GetConstraints;

    AssertNotNull('Index 1 alignment should be set', constraints[1].GetAlign);
    AssertNull('Index 0 alignment should be nil', constraints[0].GetAlign);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestGrowPrio_Current;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.GrowPrio(200);
    constraints := ac.GetConstraints;

    AssertEquals('Grow priority should be 200', 200, constraints[0].GetGrowPriority);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestGrowPrio_Specific;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.GrowPrio(150, 1);
    constraints := ac.GetConstraints;

    AssertEquals('Index 1 grow priority should be 150', 150, constraints[1].GetGrowPriority);
    AssertEquals('Index 0 should have default grow priority', 100, constraints[0].GetGrowPriority);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestGrow_Default;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.Grow;
    constraints := ac.GetConstraints;

    AssertTrue('Should have grow weight', constraints[0].HasGrowWeight);
    AssertEquals('Default grow weight should be 100', 100.0, constraints[0].GetGrowWeight, 0.01);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestGrow_Weight;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.Grow(50.5);
    constraints := ac.GetConstraints;

    AssertTrue('Should have grow weight', constraints[0].HasGrowWeight);
    AssertEquals('Grow weight should be 50.5', 50.5, constraints[0].GetGrowWeight, 0.01);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestGrow_Specific;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.Grow(75.0, 2);
    constraints := ac.GetConstraints;

    AssertTrue('Index 2 should have grow weight', constraints[2].HasGrowWeight);
    AssertEquals('Index 2 grow weight should be 75', 75.0, constraints[2].GetGrowWeight, 0.01);
    AssertFalse('Index 0 should not have grow weight', constraints[0].HasGrowWeight);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestShrinkPrio_Current;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.ShrinkPrio(50);
    constraints := ac.GetConstraints;

    AssertEquals('Shrink priority should be 50', 50, constraints[0].GetShrinkPriority);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestShrinkPrio_Specific;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.ShrinkPrio(75, 1);
    constraints := ac.GetConstraints;

    AssertEquals('Index 1 shrink priority should be 75', 75, constraints[1].GetShrinkPriority);
    AssertEquals('Index 0 should have default shrink priority', 100, constraints[0].GetShrinkPriority);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestShrink_Default;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.Shrink;
    constraints := ac.GetConstraints;

    AssertTrue('Should have shrink weight', constraints[0].HasShrinkWeight);
    AssertEquals('Shrink weight should be 100', 100.0, constraints[0].GetShrinkWeight, 0.01);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestShrink_Weight;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.Shrink(25.5);
    constraints := ac.GetConstraints;

    AssertTrue('Should have shrink weight', constraints[0].HasShrinkWeight);
    AssertEquals('Shrink weight should be 25.5', 25.5, constraints[0].GetShrinkWeight, 0.01);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestShrink_Specific;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.Shrink(80.0, 1);
    constraints := ac.GetConstraints;

    AssertTrue('Index 1 should have shrink weight', constraints[1].HasShrinkWeight);
    AssertEquals('Index 1 shrink weight should be 80', 80.0, constraints[1].GetShrinkWeight, 0.01);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestFluentChaining;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    // Chain multiple operations: fill index 0, gap, grow index 1, gap, nogrid index 2
    ac.Fill.Gap.Grow.Gap.NoGrid;
    constraints := ac.GetConstraints;

    AssertEquals('Should have 3 constraints', 3, Length(constraints));
    AssertTrue('Index 0 should be filled', constraints[0].IsFill);
    AssertTrue('Index 1 should have grow weight', constraints[1].HasGrowWeight);
    AssertTrue('Index 2 should have NoGrid', constraints[2].IsNoGrid);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestGetConstraints;
var
  ac: TfpgMigAC;
  constraints: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  try
    ac.Index(2);
    constraints := ac.GetConstraints;

    AssertEquals('Should return 3 constraints', 3, Length(constraints));
    AssertNotNull('All constraints should be non-nil', constraints[0]);
    AssertNotNull('All constraints should be non-nil', constraints[1]);
    AssertNotNull('All constraints should be non-nil', constraints[2]);
  finally
    ac.Free;
  end;
end;

procedure TTestMigAC.TestSetConstraints;
var
  ac: TfpgMigAC;
  newConstraints: TfpgMigDimConstraintArray;
  dc1, dc2: TfpgMigDimConstraint;
  result: TfpgMigDimConstraintArray;
begin
  ac := TfpgMigAC.Create;
  dc1 := TfpgMigDimConstraint.Create;
  dc2 := TfpgMigDimConstraint.Create;
  try
    dc1.SetFill(True);
    dc2.SetNoGrid(True);

    SetLength(newConstraints, 2);
    newConstraints[0] := dc1;
    newConstraints[1] := dc2;

    ac.SetConstraints(newConstraints);
    result := ac.GetConstraints;

    AssertEquals('Should have 2 constraints', 2, Length(result));
    AssertTrue('First constraint should be filled', result[0].IsFill);
    AssertTrue('Second constraint should have NoGrid', result[1].IsNoGrid);
  finally
    ac.Free;
    // Note: ac owns the constraints now, so don't free dc1, dc2
  end;
end;

initialization
  RegisterTest(TTestMigAC);

end.
