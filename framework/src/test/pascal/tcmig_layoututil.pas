unit tcmig_layoututil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  fpg_mig_layoututil, fpg_mig_resizeconstraint, fpg_mig_boundsize,
  fpg_mig_unitvalue;

type
  { Test TfpgMigLayoutUtil }
  TTestLayoutUtil = class(TTestCase)
  published
    procedure TestCalculateSerial_EqualPreferred;
    procedure TestCalculateSerial_GrowSingleComponent;
    procedure TestCalculateSerial_GrowMultipleWithPriority;
    procedure TestCalculateSerial_ShrinkSingleComponent;
    procedure TestCalculateSerial_ShrinkMultipleWithPriority;
    procedure TestCalculateSerial_ExactFit;
    procedure TestCalculateSerial_MinMaxConstraints;
    procedure TestCalculateSerial_NoResizeConstraints;
    procedure TestSum_PartialArray;
    procedure TestSum_FullArray;
    procedure TestSum_EmptyArray;
    procedure TestClamp_Float;
    procedure TestClamp_Integer;
    procedure TestRoundSizes;
    procedure TestGetSizeSafe_ValidSize;
    procedure TestGetSizeSafe_NotSet;
    procedure TestGetIndexSafe_ValidIndex;
    procedure TestGetIndexSafe_OutOfBounds;
    procedure TestGetIndexSafe_EmptyArray;
    procedure TestDerive_AllNewValues;
    procedure TestDerive_PartialNewValues;
    procedure TestDerive_NilBase;
    procedure TestObjectEquals_BothNil;
    procedure TestObjectEquals_OneNil;
    procedure TestObjectEquals_BothSame;
    procedure TestObjectEquals_Different;
  end;

implementation



{ TTestLayoutUtil }

procedure TTestLayoutUtil.TestCalculateSerial_EqualPreferred;
var
  sizes: TfpgMigSizeArrayArray;
  resConstr: TfpgMigResizeConstraintArray;
  defPush: TfpgMigFloatArray;
  result: TfpgMigIntegerArray;
begin
  // Three components, each wants 100, space is 300 - perfect fit
  SetLength(sizes, 3);
  sizes[0][SIZE_MIN] := 50;
  sizes[0][SIZE_PREF] := 100;
  sizes[0][SIZE_MAX] := 200;
  sizes[1][SIZE_MIN] := 50;
  sizes[1][SIZE_PREF] := 100;
  sizes[1][SIZE_MAX] := 200;
  sizes[2][SIZE_MIN] := 50;
  sizes[2][SIZE_PREF] := 100;
  sizes[2][SIZE_MAX] := 200;

  SetLength(resConstr, 0);  // No resize constraints
  SetLength(defPush, 0);

  result := TfpgMigLayoutUtil.CalculateSerial(sizes, resConstr, defPush, SIZE_PREF, 300);

  AssertEquals('Component 0 size', 100, result[0]);
  AssertEquals('Component 1 size', 100, result[1]);
  AssertEquals('Component 2 size', 100, result[2]);
end;

procedure TTestLayoutUtil.TestCalculateSerial_GrowSingleComponent;
var
  sizes: TfpgMigSizeArrayArray;
  resConstr: TfpgMigResizeConstraintArray;
  defPush: TfpgMigFloatArray;
  result: TfpgMigIntegerArray;
  rc: TfpgMigResizeConstraint;
begin
  // One component wants 100, space is 150, can grow
  SetLength(sizes, 1);
  sizes[0][SIZE_MIN] := 50;
  sizes[0][SIZE_PREF] := 100;
  sizes[0][SIZE_MAX] := 200;

  SetLength(resConstr, 1);
  rc := TfpgMigResizeConstraint.Create;
  rc.Grow := 100.0;
  rc.GrowPrio := 100;
  resConstr[0] := rc;

  SetLength(defPush, 0);

  result := TfpgMigLayoutUtil.CalculateSerial(sizes, resConstr, defPush, SIZE_PREF, 150);

  AssertEquals('Component grew to fill space', 150, result[0]);

  // Cleanup
  rc.Free;
end;

procedure TTestLayoutUtil.TestCalculateSerial_GrowMultipleWithPriority;
var
  sizes: TfpgMigSizeArrayArray;
  resConstr: TfpgMigResizeConstraintArray;
  defPush: TfpgMigFloatArray;
  result: TfpgMigIntegerArray;
  rc1, rc2: TfpgMigResizeConstraint;
  total: Integer;
begin
  // Two components, equal grow weight, need to share extra space
  SetLength(sizes, 2);
  sizes[0][SIZE_MIN] := 50;
  sizes[0][SIZE_PREF] := 100;
  sizes[0][SIZE_MAX] := 200;
  sizes[1][SIZE_MIN] := 50;
  sizes[1][SIZE_PREF] := 100;
  sizes[1][SIZE_MAX] := 200;

  SetLength(resConstr, 2);
  rc1 := TfpgMigResizeConstraint.Create;
  rc1.Grow := 50.0;  // Equal weight
  rc1.GrowPrio := 100;
  resConstr[0] := rc1;

  rc2 := TfpgMigResizeConstraint.Create;
  rc2.Grow := 50.0;  // Equal weight
  rc2.GrowPrio := 100;
  resConstr[1] := rc2;

  SetLength(defPush, 0);

  result := TfpgMigLayoutUtil.CalculateSerial(sizes, resConstr, defPush, SIZE_PREF, 250);

  // Each should get 25 pixels extra (total 250 = 200 + 50 extra)
  AssertEquals('Component 0 size', 125, result[0]);
  AssertEquals('Component 1 size', 125, result[1]);

  total := result[0] + result[1];
  AssertEquals('Total size matches bounds', 250, total);

  // Cleanup
  rc1.Free;
  rc2.Free;
end;

procedure TTestLayoutUtil.TestCalculateSerial_ShrinkSingleComponent;
var
  sizes: TfpgMigSizeArrayArray;
  resConstr: TfpgMigResizeConstraintArray;
  defPush: TfpgMigFloatArray;
  result: TfpgMigIntegerArray;
  rc: TfpgMigResizeConstraint;
begin
  // One component wants 100, space is 75, can shrink
  SetLength(sizes, 1);
  sizes[0][SIZE_MIN] := 50;
  sizes[0][SIZE_PREF] := 100;
  sizes[0][SIZE_MAX] := 200;

  SetLength(resConstr, 1);
  rc := TfpgMigResizeConstraint.Create;
  rc.Shrink := 100.0;
  rc.ShrinkPrio := 100;
  resConstr[0] := rc;

  SetLength(defPush, 0);

  result := TfpgMigLayoutUtil.CalculateSerial(sizes, resConstr, defPush, SIZE_PREF, 75);

  AssertEquals('Component shrank to available space', 75, result[0]);

  // Cleanup
  rc.Free;
end;

procedure TTestLayoutUtil.TestCalculateSerial_ShrinkMultipleWithPriority;
var
  sizes: TfpgMigSizeArrayArray;
  resConstr: TfpgMigResizeConstraintArray;
  defPush: TfpgMigFloatArray;
  result: TfpgMigIntegerArray;
  rc1, rc2: TfpgMigResizeConstraint;
  total: Integer;
begin
  // Two components, equal shrink weight, need to share space reduction
  SetLength(sizes, 2);
  sizes[0][SIZE_MIN] := 50;
  sizes[0][SIZE_PREF] := 100;
  sizes[0][SIZE_MAX] := 200;
  sizes[1][SIZE_MIN] := 50;
  sizes[1][SIZE_PREF] := 100;
  sizes[1][SIZE_MAX] := 200;

  SetLength(resConstr, 2);
  rc1 := TfpgMigResizeConstraint.Create;
  rc1.Shrink := 50.0;  // Equal weight
  rc1.ShrinkPrio := 100;
  resConstr[0] := rc1;

  rc2 := TfpgMigResizeConstraint.Create;
  rc2.Shrink := 50.0;  // Equal weight
  rc2.ShrinkPrio := 100;
  resConstr[1] := rc2;

  SetLength(defPush, 0);

  result := TfpgMigLayoutUtil.CalculateSerial(sizes, resConstr, defPush, SIZE_PREF, 150);

  // Each should shrink by 25 pixels (total 150 = 200 - 50)
  AssertEquals('Component 0 size', 75, result[0]);
  AssertEquals('Component 1 size', 75, result[1]);

  total := result[0] + result[1];
  AssertEquals('Total size matches bounds', 150, total);

  // Cleanup
  rc1.Free;
  rc2.Free;
end;

procedure TTestLayoutUtil.TestCalculateSerial_ExactFit;
var
  sizes: TfpgMigSizeArrayArray;
  resConstr: TfpgMigResizeConstraintArray;
  defPush: TfpgMigFloatArray;
  result: TfpgMigIntegerArray;
begin
  // Space exactly matches preferred sizes
  SetLength(sizes, 2);
  sizes[0][SIZE_MIN] := 50;
  sizes[0][SIZE_PREF] := 100;
  sizes[0][SIZE_MAX] := 200;
  sizes[1][SIZE_MIN] := 50;
  sizes[1][SIZE_PREF] := 100;
  sizes[1][SIZE_MAX] := 200;

  SetLength(resConstr, 0);
  SetLength(defPush, 0);

  result := TfpgMigLayoutUtil.CalculateSerial(sizes, resConstr, defPush, SIZE_PREF, 200);

  AssertEquals('Component 0 size', 100, result[0]);
  AssertEquals('Component 1 size', 100, result[1]);
end;

procedure TTestLayoutUtil.TestCalculateSerial_MinMaxConstraints;
var
  sizes: TfpgMigSizeArrayArray;
  resConstr: TfpgMigResizeConstraintArray;
  defPush: TfpgMigFloatArray;
  result: TfpgMigIntegerArray;
  rc: TfpgMigResizeConstraint;
begin
  // Component wants to grow but hits max constraint
  SetLength(sizes, 1);
  sizes[0][SIZE_MIN] := 50;
  sizes[0][SIZE_PREF] := 100;
  sizes[0][SIZE_MAX] := 150;  // Max is 150

  SetLength(resConstr, 1);
  rc := TfpgMigResizeConstraint.Create;
  rc.Grow := 100.0;
  rc.GrowPrio := 100;
  resConstr[0] := rc;

  SetLength(defPush, 0);

  result := TfpgMigLayoutUtil.CalculateSerial(sizes, resConstr, defPush, SIZE_PREF, 200);

  // Should be clamped to max of 150
  AssertEquals('Component clamped to max', 150, result[0]);

  // Cleanup
  rc.Free;
end;

procedure TTestLayoutUtil.TestCalculateSerial_NoResizeConstraints;
var
  sizes: TfpgMigSizeArrayArray;
  resConstr: TfpgMigResizeConstraintArray;
  defPush: TfpgMigFloatArray;
  result: TfpgMigIntegerArray;
begin
  // No resize constraints, should stay at preferred
  SetLength(sizes, 2);
  sizes[0][SIZE_MIN] := 50;
  sizes[0][SIZE_PREF] := 100;
  sizes[0][SIZE_MAX] := 200;
  sizes[1][SIZE_MIN] := 50;
  sizes[1][SIZE_PREF] := 100;
  sizes[1][SIZE_MAX] := 200;

  SetLength(resConstr, 0);  // Empty array
  SetLength(defPush, 0);

  result := TfpgMigLayoutUtil.CalculateSerial(sizes, resConstr, defPush, SIZE_PREF, 250);

  // Without resize constraints, components don't grow
  AssertEquals('Component 0 stays at preferred', 100, result[0]);
  AssertEquals('Component 1 stays at preferred', 100, result[1]);
end;

procedure TTestLayoutUtil.TestSum_PartialArray;
var
  arr: TfpgMigIntegerArray;
  result: Integer;
begin
  SetLength(arr, 5);
  arr[0] := 10;
  arr[1] := 20;
  arr[2] := 30;
  arr[3] := 40;
  arr[4] := 50;

  result := TfpgMigLayoutUtil.Sum(arr, 1, 3);  // Sum arr[1..3]
  AssertEquals('Partial sum', 90, result);  // 20 + 30 + 40
end;

procedure TTestLayoutUtil.TestSum_FullArray;
var
  arr: TfpgMigIntegerArray;
  result: Integer;
begin
  SetLength(arr, 3);
  arr[0] := 10;
  arr[1] := 20;
  arr[2] := 30;

  result := TfpgMigLayoutUtil.Sum(arr);
  AssertEquals('Full array sum', 60, result);
end;

procedure TTestLayoutUtil.TestSum_EmptyArray;
var
  arr: TfpgMigIntegerArray;
  result: Integer;
begin
  SetLength(arr, 0);
  result := TfpgMigLayoutUtil.Sum(arr);
  AssertEquals('Empty array sum', 0, result);
end;

procedure TTestLayoutUtil.TestClamp_Float;
var
  result: Single;
begin
  result := TfpgMigLayoutUtil.Clamp(50.0, 0.0, 100.0);
  AssertEquals('Within range', 50.0, result, 0.001);

  result := TfpgMigLayoutUtil.Clamp(-10.0, 0.0, 100.0);
  AssertEquals('Below min', 0.0, result, 0.001);

  result := TfpgMigLayoutUtil.Clamp(150.0, 0.0, 100.0);
  AssertEquals('Above max', 100.0, result, 0.001);
end;

procedure TTestLayoutUtil.TestClamp_Integer;
var
  result: Integer;
begin
  result := TfpgMigLayoutUtil.Clamp(50, 0, 100);
  AssertEquals('Within range', 50, result);

  result := TfpgMigLayoutUtil.Clamp(-10, 0, 100);
  AssertEquals('Below min', 0, result);

  result := TfpgMigLayoutUtil.Clamp(150, 0, 100);
  AssertEquals('Above max', 100, result);
end;

procedure TTestLayoutUtil.TestRoundSizes;
var
  floatArr: TfpgMigFloatArray;
  result: TfpgMigIntegerArray;
begin
  SetLength(floatArr, 4);
  floatArr[0] := 10.4;
  floatArr[1] := 20.5;
  floatArr[2] := 30.6;
  floatArr[3] := 40.2;

  result := TfpgMigLayoutUtil.RoundSizes(floatArr);

  AssertEquals('Count matches', 4, Length(result));
  AssertEquals('Round 10.4', 10, result[0]);
  AssertEquals('Round 20.5', 20, result[1]);  // FPC uses banker's rounding: 20.5 -> 20 (nearest even)
  AssertEquals('Round 30.6', 31, result[2]);
  AssertEquals('Round 40.2', 40, result[3]);
end;

procedure TTestLayoutUtil.TestGetSizeSafe_ValidSize;
var
  sizeArr: TfpgMigSizeArray;
  result: Integer;
begin
  sizeArr[SIZE_MIN] := 50;
  sizeArr[SIZE_PREF] := 100;
  sizeArr[SIZE_MAX] := 200;

  result := TfpgMigLayoutUtil.GetSizeSafe(sizeArr, SIZE_PREF);
  AssertEquals('Get preferred size', 100, result);
end;

procedure TTestLayoutUtil.TestGetSizeSafe_NotSet;
var
  sizeArr: TfpgMigSizeArray;
  result: Integer;
begin
  sizeArr[SIZE_MIN] := NOT_SET;
  sizeArr[SIZE_PREF] := NOT_SET;
  sizeArr[SIZE_MAX] := NOT_SET;

  result := TfpgMigLayoutUtil.GetSizeSafe(sizeArr, SIZE_PREF);
  AssertEquals('NOT_SET returns 0', 0, result);
end;



procedure TTestLayoutUtil.TestGetIndexSafe_ValidIndex;
var
  arr: TfpgMigResizeConstraintArray;
  rc1, rc2: TfpgMigResizeConstraint;
  result: TfpgMigResizeConstraint;
begin
  SetLength(arr, 2);
  rc1 := TfpgMigResizeConstraint.Create;
  rc2 := TfpgMigResizeConstraint.Create;
  arr[0] := rc1;
  arr[1] := rc2;

  result := TfpgMigLayoutUtil.GetIndexSafe(arr, 0);
  AssertTrue('Returns correct element', result = rc1);

  result := TfpgMigLayoutUtil.GetIndexSafe(arr, 1);
  AssertTrue('Returns correct element', result = rc2);

  // Cleanup
  rc1.Free;
  rc2.Free;
end;

procedure TTestLayoutUtil.TestGetIndexSafe_OutOfBounds;
var
  arr: TfpgMigResizeConstraintArray;
  rc: TfpgMigResizeConstraint;
  result: TfpgMigResizeConstraint;
begin
  SetLength(arr, 2);
  rc := TfpgMigResizeConstraint.Create;
  arr[0] := rc;
  arr[1] := rc;

  // Out of bounds returns last element
  result := TfpgMigLayoutUtil.GetIndexSafe(arr, 5);
  AssertTrue('Returns last element for out of bounds', result = rc);

  // Cleanup
  rc.Free;
end;

procedure TTestLayoutUtil.TestGetIndexSafe_EmptyArray;
var
  arr: TfpgMigResizeConstraintArray;
  result: TfpgMigResizeConstraint;
begin
  SetLength(arr, 0);
  result := TfpgMigLayoutUtil.GetIndexSafe(arr, 0);
  AssertTrue('Returns nil for empty array', result = nil);
end;

procedure TTestLayoutUtil.TestDerive_AllNewValues;
var
  base: TfpgMigBoundSize;
  newMin1, newPref1, newMax1: TfpgMigUnitValue;
  newMin, newPref, newMax: TfpgMigUnitValue;
  result: TfpgMigBoundSize;
begin
  newMin1 := TfpgMigUnitValue.Create(50);
  newPref1 := TfpgMigUnitValue.Create(100);
  newMax1 := TfpgMigUnitValue.Create(200);
  try
     base := TfpgMigBoundSize.Create(newMin1, newPref1, newMax1);
  finally
    newMin1.Free;
    newPref1.Free;
    newMax1.Free;
  end;

  newMin := TfpgMigUnitValue.Create(60);
  newPref := TfpgMigUnitValue.Create(110);
  newMax := TfpgMigUnitValue.Create(210);
  try
     result := TfpgMigLayoutUtil.Derive(base, newMin, newPref, newMax);
  finally
    newMin.Free;
    newPref.Free;
    newMax.Free;
  end;
  AssertEquals('New min value', 60, result.Min.Value);
  AssertEquals('New pref value', 110, result.Preferred.Value);
  AssertEquals('New max value', 210, result.Max.Value);

  // Cleanup - BoundSize clones UnitValues, so we must free originals
  base.Free;
  result.Free;
end;

procedure TTestLayoutUtil.TestDerive_PartialNewValues;
var
  base: TfpgMigBoundSize;
  newPref: TfpgMigUnitValue;
  result: TfpgMigBoundSize;
begin
  base := TfpgMigBoundSize.Create(
    TfpgMigUnitValue.Create(50),
    TfpgMigUnitValue.Create(100),
    TfpgMigUnitValue.Create(200)
  );

  newPref := TfpgMigUnitValue.Create(120);

  result := TfpgMigLayoutUtil.Derive(base, nil, newPref, nil);

  AssertEquals('Keep base min', 50, result.Min.Value);
  AssertEquals('Use new pref', 120, result.Preferred.Value);
  AssertEquals('Keep base max', 200, result.Max.Value);

  // Cleanup - BoundSize clones UnitValues, so we must free originals
  newPref.Free;
  base.Free;
  result.Free;
end;

procedure TTestLayoutUtil.TestDerive_NilBase;
var
  newMin, newPref, newMax: TfpgMigUnitValue;
  result: TfpgMigBoundSize;
begin
  newMin := TfpgMigUnitValue.Create(60);
  newPref := TfpgMigUnitValue.Create(110);
  newMax := TfpgMigUnitValue.Create(210);

  result := TfpgMigLayoutUtil.Derive(nil, newMin, newPref, newMax);

  AssertEquals('New min value', 60, result.Min.Value);
  AssertEquals('New pref value', 110, result.Preferred.Value);
  AssertEquals('New max value', 210, result.Max.Value);

  // Cleanup - BoundSize clones UnitValues, so we must free originals
  newMin.Free;
  newPref.Free;
  newMax.Free;
  result.Free;
end;

procedure TTestLayoutUtil.TestObjectEquals_BothNil;
begin
  AssertTrue('Both nil are equal', TfpgMigLayoutUtil.ObjectEquals(nil, nil));
end;

procedure TTestLayoutUtil.TestObjectEquals_OneNil;
var
  obj: TObject;
begin
  obj := TObject.Create;
  try
    AssertFalse('One nil not equal', TfpgMigLayoutUtil.ObjectEquals(obj, nil));
    AssertFalse('One nil not equal (reversed)', TfpgMigLayoutUtil.ObjectEquals(nil, obj));
  finally
    obj.Free;
  end;
end;

procedure TTestLayoutUtil.TestObjectEquals_BothSame;
var
  obj: TObject;
begin
  obj := TObject.Create;
  try
    AssertTrue('Same object equals', TfpgMigLayoutUtil.ObjectEquals(obj, obj));
  finally
    obj.Free;
  end;
end;

procedure TTestLayoutUtil.TestObjectEquals_Different;
var
  obj1, obj2: TObject;
begin
  obj1 := TObject.Create;
  obj2 := TObject.Create;
  try
    AssertFalse('Different objects not equal', TfpgMigLayoutUtil.ObjectEquals(obj1, obj2));
  finally
    obj1.Free;
    obj2.Free;
  end;
end;

initialization
  RegisterTest(TTestLayoutUtil);

end.
