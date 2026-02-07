unit tcmig_dimconstraint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  fpg_mig_dimconstraint, fpg_mig_unitvalue, fpg_mig_boundsize;

type
  TTestMigDimConstraint = class(TTestCase)
  published
    procedure TestCreate;
    procedure TestGrowPriority;
    procedure TestGrowWeight;
    procedure TestShrinkPriority;
    procedure TestShrinkWeight;
    procedure TestAlign;
    procedure TestAlignOrDefault_Columns;
    procedure TestAlignOrDefault_RowsWithFill;
    procedure TestSize;
    procedure TestSizeWithLinked;
    procedure TestGapBefore;
    procedure TestGapAfter;
    procedure TestSizeGroup;
    procedure TestEndGroup;
    procedure TestFill;
    procedure TestNoGrid;
  end;

implementation

{ TTestMigDimConstraint }

procedure TTestMigDimConstraint.TestCreate;
var
  dc: TfpgMigDimConstraint;
begin
  dc := TfpgMigDimConstraint.Create;
  try
    AssertEquals('Default grow priority should be 100', 100, dc.GetGrowPriority);
    AssertFalse('Should not have grow weight by default', dc.HasGrowWeight);
    AssertEquals('Default shrink priority should be 100', 100, dc.GetShrinkPriority);
    AssertTrue('Should have shrink weight by default', dc.HasShrinkWeight);
    AssertEquals('Default shrink weight should be 100', 100.0, dc.GetShrinkWeight, 0.001);
    AssertFalse('Fill should be false by default', dc.IsFill);
    AssertFalse('NoGrid should be false by default', dc.IsNoGrid);
  finally
    dc.Free;
  end;
end;

procedure TTestMigDimConstraint.TestGrowPriority;
var
  dc: TfpgMigDimConstraint;
begin
  dc := TfpgMigDimConstraint.Create;
  try
    dc.SetGrowPriority(200);
    AssertEquals('Grow priority should be 200', 200, dc.GetGrowPriority);
  finally
    dc.Free;
  end;
end;

procedure TTestMigDimConstraint.TestGrowWeight;
var
  dc: TfpgMigDimConstraint;
begin
  dc := TfpgMigDimConstraint.Create;
  try
    AssertFalse('Should not have grow weight initially', dc.HasGrowWeight);

    dc.SetGrowWeight(1.5);
    AssertTrue('Should have grow weight after setting', dc.HasGrowWeight);
    AssertEquals('Grow weight should be 1.5', 1.5, dc.GetGrowWeight, 0.001);

    dc.ClearGrowWeight;
    AssertFalse('Should not have grow weight after clearing', dc.HasGrowWeight);
  finally
    dc.Free;
  end;
end;

procedure TTestMigDimConstraint.TestShrinkPriority;
var
  dc: TfpgMigDimConstraint;
begin
  dc := TfpgMigDimConstraint.Create;
  try
    dc.SetShrinkPriority(50);
    AssertEquals('Shrink priority should be 50', 50, dc.GetShrinkPriority);
  finally
    dc.Free;
  end;
end;

procedure TTestMigDimConstraint.TestShrinkWeight;
var
  dc: TfpgMigDimConstraint;
begin
  dc := TfpgMigDimConstraint.Create;
  try
    AssertTrue('Should have shrink weight initially', dc.HasShrinkWeight);
    AssertEquals('Default shrink weight', 100.0, dc.GetShrinkWeight, 0.001);

    dc.SetShrinkWeight(0.5);
    AssertEquals('Shrink weight should be 0.5', 0.5, dc.GetShrinkWeight, 0.001);

    dc.ClearShrinkWeight;
    AssertFalse('Should not have shrink weight after clearing', dc.HasShrinkWeight);
  finally
    dc.Free;
  end;
end;

procedure TTestMigDimConstraint.TestAlign;
var
  dc: TfpgMigDimConstraint;
  uv: TfpgMigUnitValue;
begin
  dc := TfpgMigDimConstraint.Create;
  uv := TfpgMigUnitValue.Create(10, utPixel, '10px');
  try
    AssertNull('Align should be nil by default', dc.GetAlign);

    dc.SetAlign(uv);
    AssertSame('Align should be set value', uv, dc.GetAlign);
  finally
    uv.Free;
    dc.Free;
  end;
end;

procedure TTestMigDimConstraint.TestAlignOrDefault_Columns;
var
  dc: TfpgMigDimConstraint;
  align: TfpgMigUnitValue;
begin
  dc := TfpgMigDimConstraint.Create;
  try
    align := dc.GetAlignOrDefault(True);  // True = columns
    AssertSame('Column default alignment should be LEADING',
      UnitValueLeading, align);
  finally
    dc.Free;
  end;
end;

procedure TTestMigDimConstraint.TestAlignOrDefault_RowsWithFill;
var
  dc: TfpgMigDimConstraint;
  align: TfpgMigUnitValue;
begin
  dc := TfpgMigDimConstraint.Create;
  try
    dc.SetFill(True);
    align := dc.GetAlignOrDefault(False);  // False = rows
    AssertSame('Row alignment with fill should be CENTER',
      UnitValueCenter, align);
  finally
    dc.Free;
  end;
end;

procedure TTestMigDimConstraint.TestSize;
var
  dc: TfpgMigDimConstraint;
  bs: TfpgMigBoundSize;
  uv: TfpgMigUnitValue;
begin
  dc := TfpgMigDimConstraint.Create;
  uv := TfpgMigUnitValue.Create(100, utPixel, '100px');
  bs := TfpgMigBoundSize.Create(uv, uv, nil); // bs clones uv
  uv.Free; // Original uv is no longer needed

  try
    AssertSame('Default size should be NULL_SIZE',
      BoundSizeNullSize, dc.GetSize);

    dc.SetSize(bs); // dc takes ownership of bs
    AssertSame('Size should be set value', bs, dc.GetSize);
  finally
    dc.Free;  // This will free bs and the cloned uv
  end;
end;

procedure TTestMigDimConstraint.TestSizeWithLinked;
var
  dc: TfpgMigDimConstraint;
  bs: TfpgMigBoundSize;
  uv: TfpgMigUnitValue;
  exceptionRaised: Boolean;
begin
  dc := TfpgMigDimConstraint.Create;
  uv := TfpgMigUnitValue.Create(0, utLinkW, 'button1.w');
  bs := TfpgMigBoundSize.Create(uv, nil, nil);
  try
    exceptionRaised := False;
    try
      dc.SetSize(bs);
    except
      on E: Exception do
        exceptionRaised := True;
    end;
    AssertTrue('Should raise exception for linked size', exceptionRaised);
    // Since SetSize raised exception, bs was not transferred to dc
    // so test still owns it
  finally
    bs.Free;  // Still owned by test since SetSize failed
    uv.Free;
    dc.Free;
  end;
end;

procedure TTestMigDimConstraint.TestGapBefore;
var
  dc: TfpgMigDimConstraint;
  gap: TfpgMigBoundSize;
  uv: TfpgMigUnitValue;
begin
  dc := TfpgMigDimConstraint.Create;
  uv := TfpgMigUnitValue.Create(10, utPixel, '10px');
  gap := TfpgMigBoundSize.Create(uv, uv, nil);
  uv.Free; // Original uv is no longer needed

  try
    AssertFalse('Should not have gap before initially', dc.HasGapBefore);
    AssertFalse('Should not be push gap initially', dc.IsGapBeforePush);

    dc.SetGapBefore(gap); // dc takes ownership of gap
    AssertTrue('Should have gap before after setting', dc.HasGapBefore);
    AssertSame('Gap before should be set value', gap, dc.GetGapBefore);
  finally
    dc.Free;  // This will free gap and the cloned uv
  end;
end;

procedure TTestMigDimConstraint.TestGapAfter;
var
  dc: TfpgMigDimConstraint;
  gap: TfpgMigBoundSize;
  uv: TfpgMigUnitValue;
begin
  dc := TfpgMigDimConstraint.Create;
  uv := TfpgMigUnitValue.Create(10, utPixel, '10px');
  gap := TfpgMigBoundSize.Create(uv, uv, nil, True);  // True = gap push
  uv.Free; // Original uv is no longer needed

  try
    AssertFalse('Should not have gap after initially', dc.HasGapAfter);

    dc.SetGapAfter(gap); // dc takes ownership of gap
    AssertTrue('Should have gap after after setting', dc.HasGapAfter);
    AssertTrue('Should be push gap', dc.IsGapAfterPush);
    AssertSame('Gap after should be set value', gap, dc.GetGapAfter);
  finally
    dc.Free;  // This will free gap and the cloned uv
  end;
end;

procedure TTestMigDimConstraint.TestSizeGroup;
var
  dc: TfpgMigDimConstraint;
begin
  dc := TfpgMigDimConstraint.Create;
  try
    AssertEquals('Size group should be empty by default', '', dc.GetSizeGroup);

    dc.SetSizeGroup('group1');
    AssertEquals('Size group should be "group1"', 'group1', dc.GetSizeGroup);
  finally
    dc.Free;
  end;
end;

procedure TTestMigDimConstraint.TestEndGroup;
var
  dc: TfpgMigDimConstraint;
begin
  dc := TfpgMigDimConstraint.Create;
  try
    AssertEquals('End group should be empty by default', '', dc.GetEndGroup);

    dc.SetEndGroup('endgroup1');
    AssertEquals('End group should be "endgroup1"', 'endgroup1', dc.GetEndGroup);
  finally
    dc.Free;
  end;
end;

procedure TTestMigDimConstraint.TestFill;
var
  dc: TfpgMigDimConstraint;
begin
  dc := TfpgMigDimConstraint.Create;
  try
    AssertFalse('Fill should be false by default', dc.IsFill);

    dc.SetFill(True);
    AssertTrue('Fill should be true after setting', dc.IsFill);
  finally
    dc.Free;
  end;
end;

procedure TTestMigDimConstraint.TestNoGrid;
var
  dc: TfpgMigDimConstraint;
begin
  dc := TfpgMigDimConstraint.Create;
  try
    AssertFalse('NoGrid should be false by default', dc.IsNoGrid);

    dc.SetNoGrid(True);
    AssertTrue('NoGrid should be true after setting', dc.IsNoGrid);
  finally
    dc.Free;
  end;
end;

initialization
  RegisterTest(TTestMigDimConstraint);

end.
