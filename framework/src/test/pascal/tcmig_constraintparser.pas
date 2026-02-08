unit tcmig_constraintparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  fpg_mig_constraintparser, fpg_mig_unitvalue, fpg_mig_boundsize;

type
  TTestMigConstraintParser = class(TTestCase)
  published
    // ParseUnitValue tests
    procedure TestParseUnitValue_SimplePixels;
    procedure TestParseUnitValue_Percent;
    procedure TestParseUnitValue_Millimeters;
    procedure TestParseUnitValue_LogicalPixels;
    procedure TestParseUnitValue_MinSize;
    procedure TestParseUnitValue_PrefSize;
    procedure TestParseUnitValue_MaxSize;
    procedure TestParseUnitValue_AlignLeft;
    procedure TestParseUnitValue_AlignCenter;
    procedure TestParseUnitValue_AlignRight;
    procedure TestParseUnitValue_AlignTop;
    procedure TestParseUnitValue_AlignBottom;
    procedure TestParseUnitValue_WithAddition;
    procedure TestParseUnitValue_WithSubtraction;
    procedure TestParseUnitValue_MinOperation;
    procedure TestParseUnitValue_MaxOperation;
    procedure TestParseUnitValue_MidOperation;

    // ParseBoundSize tests
    procedure TestParseBoundSize_SimpleValue;
    procedure TestParseBoundSize_MinPrefMax;
    procedure TestParseBoundSize_PrefOnly;
    procedure TestParseBoundSize_MinPref;
    procedure TestParseBoundSize_PrefMax;
    procedure TestParseBoundSize_Null;
    procedure TestParseBoundSize_Push;

    // ParseInsets tests
    procedure TestParseInsets_SingleValue;
    procedure TestParseInsets_FourValues;
    procedure TestParseInsets_TwoValues;
    procedure TestParseInsets_Null;

    // Note: Helper functions (ToTrimmedTokens, GetNumTextParts, GetOper) are
    // private implementation details and are tested indirectly through the
    // main parsing functions above
  end;

implementation

{ TTestMigConstraintParser }

// ============================================================================
// ParseUnitValue tests
// ============================================================================

procedure TTestMigConstraintParser.TestParseUnitValue_SimplePixels;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValue('10px', True);
  try
    AssertNotNull('Result should not be nil', uv);
    AssertEquals('Value should be 10', 10.0, uv.Value, 0.001);
    AssertEquals('Unit should be pixel', Ord(utPixel), Ord(uv.UnitType));
  finally
    uv.Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseUnitValue_Percent;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValue('50%', True);
  try
    AssertNotNull('Result should not be nil', uv);
    AssertEquals('Value should be 50', 50.0, uv.Value, 0.001);
    AssertEquals('Unit should be percent', Ord(utPercent), Ord(uv.UnitType));
  finally
    uv.Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseUnitValue_Millimeters;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValue('5mm', True);
  try
    AssertNotNull('Result should not be nil', uv);
    AssertEquals('Value should be 5', 5.0, uv.Value, 0.001);
    AssertEquals('Unit should be mm', Ord(utMM), Ord(uv.UnitType));
  finally
    uv.Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseUnitValue_LogicalPixels;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValue('12lpx', True);
  try
    AssertNotNull('Result should not be nil', uv);
    AssertEquals('Value should be 12', 12.0, uv.Value, 0.001);
    AssertEquals('Unit should be lpx', Ord(utLPX), Ord(uv.UnitType));
  finally
    uv.Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseUnitValue_MinSize;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValue('min', True);
  try
    AssertNotNull('Result should not be nil', uv);
    AssertEquals('Unit should be minimum', Ord(utMinSize), Ord(uv.UnitType));
  finally
    uv.Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseUnitValue_PrefSize;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValue('pref', True);
  try
    AssertNotNull('Result should not be nil', uv);
    AssertEquals('Unit should be preferred', Ord(utPrefSize), Ord(uv.UnitType));
  finally
    uv.Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseUnitValue_MaxSize;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValue('max', True);
  try
    AssertNotNull('Result should not be nil', uv);
    AssertEquals('Unit should be maximum', Ord(utMaxSize), Ord(uv.UnitType));
  finally
    uv.Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseUnitValue_AlignLeft;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValueOrAlign('left', True);
  // Do NOT free uv - ParseUnitValueOrAlign returns singleton UnitValue constants
  AssertNotNull('Result should not be nil', uv);
  AssertEquals('Value should be 0', 0.0, uv.Value, 0.001);
  AssertEquals('Unit should be percent', Ord(utPercent), Ord(uv.UnitType));
end;

procedure TTestMigConstraintParser.TestParseUnitValue_AlignCenter;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValueOrAlign('center', True);
  // Do NOT free uv - ParseUnitValueOrAlign returns singleton UnitValue constants
  AssertNotNull('Result should not be nil', uv);
  AssertEquals('Value should be 50', 50.0, uv.Value, 0.001);
  AssertEquals('Unit should be percent', Ord(utPercent), Ord(uv.UnitType));
end;

procedure TTestMigConstraintParser.TestParseUnitValue_AlignRight;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValueOrAlign('right', True);
  // Do NOT free uv - ParseUnitValueOrAlign returns singleton UnitValue constants
  AssertNotNull('Result should not be nil', uv);
  AssertEquals('Value should be 100', 100.0, uv.Value, 0.001);
  AssertEquals('Unit should be percent', Ord(utPercent), Ord(uv.UnitType));
end;

procedure TTestMigConstraintParser.TestParseUnitValue_AlignTop;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValueOrAlign('top', False);
  // Do NOT free uv - ParseUnitValueOrAlign returns singleton UnitValue constants
  AssertNotNull('Result should not be nil', uv);
  AssertEquals('Value should be 0', 0.0, uv.Value, 0.001);
  AssertEquals('Unit should be percent', Ord(utPercent), Ord(uv.UnitType));
end;

procedure TTestMigConstraintParser.TestParseUnitValue_AlignBottom;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValueOrAlign('bottom', False);
  // Do NOT free uv - ParseUnitValueOrAlign returns singleton UnitValue constants
  AssertNotNull('Result should not be nil', uv);
  AssertEquals('Value should be 100', 100.0, uv.Value, 0.001);
  AssertEquals('Unit should be percent', Ord(utPercent), Ord(uv.UnitType));
end;

procedure TTestMigConstraintParser.TestParseUnitValue_WithAddition;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValue('10px+5px', True);
  try
    AssertNotNull('Result should not be nil', uv);
    AssertEquals('Operation should be add', Ord(opAdd), Ord(uv.Operation));
  finally
    uv.Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseUnitValue_WithSubtraction;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValue('100%-10px', True);
  try
    AssertNotNull('Result should not be nil', uv);
    AssertEquals('Operation should be subtract', Ord(opSub), Ord(uv.Operation));
  finally
    uv.Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseUnitValue_MinOperation;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValue('min(10px,20px)', True);
  try
    AssertNotNull('Result should not be nil', uv);
    AssertEquals('Operation should be min', Ord(opMin), Ord(uv.Operation));
  finally
    uv.Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseUnitValue_MaxOperation;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValue('max(10px,20px)', True);
  try
    AssertNotNull('Result should not be nil', uv);
    AssertEquals('Operation should be max', Ord(opMax), Ord(uv.Operation));
  finally
    uv.Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseUnitValue_MidOperation;
var
  uv: TfpgMigUnitValue;
begin
  uv := ParseUnitValue('mid(10px,20px)', True);
  try
    AssertNotNull('Result should not be nil', uv);
    AssertEquals('Operation should be mid', Ord(opMid), Ord(uv.Operation));
  finally
    uv.Free;
  end;
end;

// ============================================================================
// ParseBoundSize tests
// ============================================================================

procedure TTestMigConstraintParser.TestParseBoundSize_SimpleValue;
var
  bs: TfpgMigBoundSize;
begin
  bs := ParseBoundSize('100px', False, True);
  try
    AssertNotNull('Result should not be nil', bs);
    // For non-gap single value: only preferred is set (Java v11 behavior)
    AssertNull('Min should be nil for non-gap', bs.Min);
    AssertNotNull('Preferred should not be nil', bs.Preferred);
    AssertNull('Max should be nil for non-gap', bs.Max);
    AssertEquals('Preferred value should be 100', 100.0, bs.Preferred.Value, 0.001);
  finally
    bs.Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseBoundSize_MinPrefMax;
var
  bs: TfpgMigBoundSize;
begin
  bs := ParseBoundSize('10px:100px:200px', False, True);
  try
    AssertNotNull('Result should not be nil', bs);
    AssertNotNull('Min should not be nil', bs.Min);
    AssertNotNull('Preferred should not be nil', bs.Preferred);
    AssertNotNull('Max should not be nil', bs.Max);
    AssertEquals('Min value should be 10', 10.0, bs.Min.Value, 0.001);
    AssertEquals('Preferred value should be 100', 100.0, bs.Preferred.Value, 0.001);
    AssertEquals('Max value should be 200', 200.0, bs.Max.Value, 0.001);
  finally
    bs.Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseBoundSize_PrefOnly;
var
  bs: TfpgMigBoundSize;
begin
  // "::100px" means min=empty, pref=empty, max=100px (max-only constraint)
  bs := ParseBoundSize('::100px', False, True);
  try
    AssertNotNull('Result should not be nil', bs);
    AssertNull('Min should be nil', bs.Min);
    AssertNull('Preferred should be nil', bs.Preferred);
    AssertNotNull('Max should not be nil', bs.Max);
    AssertEquals('Max value should be 100', 100.0, bs.Max.Value, 0.001);
  finally
    bs.Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseBoundSize_MinPref;
var
  bs: TfpgMigBoundSize;
begin
  bs := ParseBoundSize('10px:100px:', False, True);
  try
    AssertNotNull('Result should not be nil', bs);
    AssertNotNull('Min should not be nil', bs.Min);
    AssertNotNull('Preferred should not be nil', bs.Preferred);
    AssertEquals('Min value should be 10', 10.0, bs.Min.Value, 0.001);
    AssertEquals('Preferred value should be 100', 100.0, bs.Preferred.Value, 0.001);
  finally
    bs.Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseBoundSize_PrefMax;
var
  bs: TfpgMigBoundSize;
begin
  bs := ParseBoundSize(':100px:200px', False, True);
  try
    AssertNotNull('Result should not be nil', bs);
    AssertNotNull('Preferred should not be nil', bs.Preferred);
    AssertNotNull('Max should not be nil', bs.Max);
    AssertEquals('Preferred value should be 100', 100.0, bs.Preferred.Value, 0.001);
    AssertEquals('Max value should be 200', 200.0, bs.Max.Value, 0.001);
  finally
    bs.Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseBoundSize_Null;
var
  bs: TfpgMigBoundSize;
begin
  bs := ParseBoundSize('null', False, True);
  AssertNull('null should return nil', bs);

  bs := ParseBoundSize('n', False, True);
  AssertNull('n should return nil', bs);
end;

procedure TTestMigConstraintParser.TestParseBoundSize_Push;
var
  bs: TfpgMigBoundSize;
begin
  bs := ParseBoundSize('10px:push', False, True);
  try
    AssertNotNull('Result should not be nil', bs);
    AssertTrue('Should have gap push', bs.GapPush);
  finally
    bs.Free;
  end;
end;

// ============================================================================
// ParseInsets tests
// ============================================================================

procedure TTestMigConstraintParser.TestParseInsets_SingleValue;
var
  insets: TfpgMigUnitValueArray;
  i: Integer;
begin
  insets := ParseInsets('10px', True);
  try
    AssertEquals('Should have 4 values', 4, Length(insets));
    AssertNotNull('Top should not be nil', insets[0]);
    AssertNotNull('Right should not be nil', insets[1]);
    AssertNotNull('Bottom should not be nil', insets[2]);
    AssertNotNull('Left should not be nil', insets[3]);
    AssertEquals('All values should be 10', 10.0, insets[0].Value, 0.001);
    AssertEquals('All values should be 10', 10.0, insets[1].Value, 0.001);
    AssertEquals('All values should be 10', 10.0, insets[2].Value, 0.001);
    AssertEquals('All values should be 10', 10.0, insets[3].Value, 0.001);
  finally
    for i := 0 to High(insets) do
      insets[i].Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseInsets_FourValues;
var
  insets: TfpgMigUnitValueArray;
  i: Integer;
begin
  insets := ParseInsets('10px 20px 30px 40px', True);
  try
    AssertEquals('Should have 4 values', 4, Length(insets));
    AssertNotNull('Top should not be nil', insets[0]);
    AssertNotNull('Right should not be nil', insets[1]);
    AssertNotNull('Bottom should not be nil', insets[2]);
    AssertNotNull('Left should not be nil', insets[3]);
    AssertEquals('Top should be 10', 10.0, insets[0].Value, 0.001);
    AssertEquals('Right should be 20', 20.0, insets[1].Value, 0.001);
    AssertEquals('Bottom should be 30', 30.0, insets[2].Value, 0.001);
    AssertEquals('Left should be 40', 40.0, insets[3].Value, 0.001);
  finally
    for i := 0 to High(insets) do
      insets[i].Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseInsets_TwoValues;
var
  insets: TfpgMigUnitValueArray;
  i: Integer;
begin
  insets := ParseInsets('10px 20px', True);
  try
    AssertEquals('Should have 4 values', 4, Length(insets));
    // Two values: uses last value for remaining (Java v11 behavior)
    // Order: top, right, bottom, left (0,1,2,3)
    AssertEquals('Top should be 10', 10.0, insets[0].Value, 0.001);
    AssertEquals('Right should be 20', 20.0, insets[1].Value, 0.001);
    AssertEquals('Bottom should be 20 (uses last)', 20.0, insets[2].Value, 0.001);
    AssertEquals('Left should be 20 (uses last)', 20.0, insets[3].Value, 0.001);
  finally
    for i := 0 to High(insets) do
      insets[i].Free;
  end;
end;

procedure TTestMigConstraintParser.TestParseInsets_Null;
var
  insets: TfpgMigUnitValueArray;
begin
  // In Java v11, "null" string in insets returns platform defaults, not empty array
  // parseUnitValue("null") returns nil, which triggers platform default fallback
  insets := ParseInsets('null', True);
  AssertEquals('null should return platform defaults (4 values)', 4, Length(insets));
  AssertNotNull('All insets should be set', insets[0]);
  AssertNotNull('All insets should be set', insets[1]);
  AssertNotNull('All insets should be set', insets[2]);
  AssertNotNull('All insets should be set', insets[3]);

  insets := ParseInsets('n', True);
  AssertEquals('n should return platform defaults (4 values)', 4, Length(insets));
end;

initialization
  RegisterTest(TTestMigConstraintParser);

end.
