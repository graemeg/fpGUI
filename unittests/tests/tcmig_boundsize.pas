unit tcmig_boundsize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  fpg_mig_unitvalue, fpg_mig_boundsize;

type
  TTestMigBoundSize = class(TTestCase)
  private
    FUV10px: TfpgMigUnitValue;
    FUV20px: TfpgMigUnitValue;
    FUV30px: TfpgMigUnitValue;
    FUVPercent: TfpgMigUnitValue;
    FUVLink: TfpgMigUnitValue;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateWithSingleValue;
    procedure TestCreateWithThreeValues;
    procedure TestCreateWithGapPush;
    procedure TestIsUnset_TrueWhenAllNil;
    procedure TestIsUnset_FalseWhenMinSet;
    procedure TestIsUnset_FalseWhenPrefSet;
    procedure TestIsUnset_FalseWhenMaxSet;
    procedure TestIsUnset_FalseWhenGapPushSet;
    procedure TestIsUnset_TrueForZeroPixelConstant;
    procedure TestIsLinked_FalseWhenNoLinks;
    procedure TestIsLinked_TrueWhenMinLinked;
    procedure TestIsLinked_TrueWhenPrefLinked;
    procedure TestIsLinked_TrueWhenMaxLinked;
    procedure TestIsAbsolute_TrueWhenAllAbsolute;
    procedure TestIsAbsolute_TrueWhenAllNil;
    procedure TestIsAbsolute_FalseWhenMinRelative;
    procedure TestIsAbsolute_FalseWhenPrefRelative;
    procedure TestIsAbsolute_FalseWhenMaxRelative;
    procedure TestConstantValues;
    procedure TestPropertyAccess;
  end;

implementation

{ TTestMigBoundSize }

procedure TTestMigBoundSize.SetUp;
begin
  inherited SetUp;
  FUV10px := TfpgMigUnitValue.Create(10, utPixel, '10px');
  FUV20px := TfpgMigUnitValue.Create(20, utPixel, '20px');
  FUV30px := TfpgMigUnitValue.Create(30, utPixel, '30px');
  FUVPercent := TfpgMigUnitValue.Create(50, utPercent, '50%');
  FUVLink := TfpgMigUnitValue.Create(0, 'btn1.width', True, opStatic, 'btn1.width');
end;

procedure TTestMigBoundSize.TearDown;
begin
  FUVLink.Free;
  FUVPercent.Free;
  FUV30px.Free;
  FUV20px.Free;
  FUV10px.Free;
  inherited TearDown;
end;

procedure TTestMigBoundSize.TestCreateWithSingleValue;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(FUV10px);
  try
    AssertTrue('Min should have same content as input', bs.Min.ContentEquals(FUV10px));
    AssertTrue('Pref should have same content as input', bs.Preferred.ContentEquals(FUV10px));
    AssertTrue('Max should have same content as input', bs.Max.ContentEquals(FUV10px));
    AssertFalse('GapPush should be false', bs.GapPush);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestCreateWithThreeValues;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(FUV10px, FUV20px, FUV30px);
  try
    AssertTrue('Min should be 10px', bs.Min.ContentEquals(FUV10px));
    AssertTrue('Pref should be 20px', bs.Preferred.ContentEquals(FUV20px));
    AssertTrue('Max should be 30px', bs.Max.ContentEquals(FUV30px));
    AssertFalse('GapPush should be false', bs.GapPush);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestCreateWithGapPush;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(FUV10px, FUV20px, FUV30px, True);
  try
    AssertTrue('Min should be 10px', bs.Min.ContentEquals(FUV10px));
    AssertTrue('Pref should be 20px', bs.Preferred.ContentEquals(FUV20px));
    AssertTrue('Max should be 30px', bs.Max.ContentEquals(FUV30px));
    AssertTrue('GapPush should be true', bs.GapPush);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestIsUnset_TrueWhenAllNil;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(nil, nil, nil);
  try
    AssertTrue('Should be unset when all values are nil', bs.IsUnset);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestIsUnset_FalseWhenMinSet;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(FUV10px, nil, nil);
  try
    AssertFalse('Should not be unset when min is set', bs.IsUnset);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestIsUnset_FalseWhenPrefSet;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(nil, FUV20px, nil);
  try
    AssertFalse('Should not be unset when pref is set', bs.IsUnset);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestIsUnset_FalseWhenMaxSet;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(nil, nil, FUV30px);
  try
    AssertFalse('Should not be unset when max is set', bs.IsUnset);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestIsUnset_FalseWhenGapPushSet;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(nil, nil, nil, True);
  try
    AssertFalse('Should not be unset when gapPush is true', bs.IsUnset);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestIsUnset_TrueForZeroPixelConstant;
var
  bs: TfpgMigBoundSize;
begin
  bs := BoundSizeZeroPixel;
  AssertTrue('ZERO_PIXEL constant should report as unset', bs.IsUnset);
end;

procedure TTestMigBoundSize.TestIsLinked_FalseWhenNoLinks;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(FUV10px, FUV20px, FUV30px);
  try
    AssertFalse('Should not be linked when no links present', bs.IsLinked);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestIsLinked_TrueWhenMinLinked;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(FUVLink, FUV20px, FUV30px);
  try
    AssertTrue('Should be linked when min contains link', bs.IsLinked);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestIsLinked_TrueWhenPrefLinked;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(FUV10px, FUVLink, FUV30px);
  try
    AssertTrue('Should be linked when pref contains link', bs.IsLinked);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestIsLinked_TrueWhenMaxLinked;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(FUV10px, FUV20px, FUVLink);
  try
    AssertTrue('Should be linked when max contains link', bs.IsLinked);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestIsAbsolute_TrueWhenAllAbsolute;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(FUV10px, FUV20px, FUV30px);
  try
    AssertTrue('Should be absolute when all values are absolute', bs.IsAbsolute);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestIsAbsolute_TrueWhenAllNil;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(nil, nil, nil);
  try
    AssertTrue('Should be absolute when all values are nil', bs.IsAbsolute);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestIsAbsolute_FalseWhenMinRelative;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(FUVPercent, FUV20px, FUV30px);
  try
    AssertFalse('Should not be absolute when min is relative', bs.IsAbsolute);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestIsAbsolute_FalseWhenPrefRelative;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(FUV10px, FUVPercent, FUV30px);
  try
    AssertFalse('Should not be absolute when pref is relative', bs.IsAbsolute);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestIsAbsolute_FalseWhenMaxRelative;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(FUV10px, FUV20px, FUVPercent);
  try
    AssertFalse('Should not be absolute when max is relative', bs.IsAbsolute);
  finally
    bs.Free;
  end;
end;

procedure TTestMigBoundSize.TestConstantValues;
var
  nullSize, zeroPixel: TfpgMigBoundSize;
begin
  nullSize := BoundSizeNullSize;
  AssertNotNull('NULL_SIZE should not be nil', nullSize);
  AssertTrue('NULL_SIZE should be unset', nullSize.IsUnset);
  AssertNull('NULL_SIZE min should be nil', nullSize.Min);
  AssertNull('NULL_SIZE pref should be nil', nullSize.Preferred);
  AssertNull('NULL_SIZE max should be nil', nullSize.Max);

  zeroPixel := BoundSizeZeroPixel;
  AssertNotNull('ZERO_PIXEL should not be nil', zeroPixel);
  AssertNotNull('ZERO_PIXEL should have min', zeroPixel.Min);
  AssertNotNull('ZERO_PIXEL should have pref', zeroPixel.Preferred);
  AssertNotNull('ZERO_PIXEL should have max', zeroPixel.Max);
  // Check values are equal (all zero pixels), not pointer equality
  AssertEquals('ZERO_PIXEL min should be 0', 0.0, zeroPixel.Min.Value);
  AssertEquals('ZERO_PIXEL pref should be 0', 0.0, zeroPixel.Preferred.Value);
  AssertEquals('ZERO_PIXEL max should be 0', 0.0, zeroPixel.Max.Value);
  AssertEquals('ZERO_PIXEL min should be pixel', Ord(utPixel), Ord(zeroPixel.Min.UnitType));
  AssertEquals('ZERO_PIXEL pref should be pixel', Ord(utPixel), Ord(zeroPixel.Preferred.UnitType));
  AssertEquals('ZERO_PIXEL max should be pixel', Ord(utPixel), Ord(zeroPixel.Max.UnitType));
end;

procedure TTestMigBoundSize.TestPropertyAccess;
var
  bs: TfpgMigBoundSize;
begin
  bs := TfpgMigBoundSize.Create(FUV10px, FUV20px, FUV30px, True);
  try
    AssertTrue('Min property should return correct value', bs.Min.ContentEquals(FUV10px));
    AssertTrue('Preferred property should return correct value', bs.Preferred.ContentEquals(FUV20px));
    AssertTrue('Max property should return correct value', bs.Max.ContentEquals(FUV30px));
    AssertTrue('GapPush property should return true', bs.GapPush);
  finally
    bs.Free;
  end;
end;

initialization
  RegisterTest(TTestMigBoundSize);

end.
