unit tcmig_unitvalue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  fpg_mig_unitvalue;

type
  TTestMigUnitValue = class(TTestCase)
  published
    procedure TestCreateSimplePixel;
    procedure TestCreateWithUnit;
    procedure TestParseUnitType_Pixels;
    procedure TestParseUnitType_Millimeters;
    procedure TestParseUnitType_Centimeters;
    procedure TestParseUnitType_Inches;
    procedure TestParseUnitType_Points;
    procedure TestParseUnitType_Percent;
    procedure TestParseUnitType_LogicalPixels;
    procedure TestParseUnitType_ScreenPercent;
    procedure TestParseUnitType_MinMaxPref;
    procedure TestParseUnitType_Button;
    procedure TestParseUnitType_Label;
    procedure TestParseUnitType_Link;
    procedure TestParseUnitType_Invalid;
    procedure TestIsAbsolute_TrueForPixels;
    procedure TestIsAbsolute_TrueForPhysicalUnits;
    procedure TestIsAbsolute_FalseForPercent;
    procedure TestIsAbsolute_FalseForMinMaxPref;
    procedure TestIsLinked_FalseForSimpleUnit;
    procedure TestIsLinked_TrueForLinkUnit;
    procedure TestConstantValues;
  end;

implementation

{ TTestMigUnitValue }

procedure TTestMigUnitValue.TestCreateSimplePixel;
var
  uv: TfpgMigUnitValue;
begin
  uv := TfpgMigUnitValue.Create(10.0);
  try
    AssertEquals('Value should be 10', 10.0, uv.Value, 0.001);
    AssertEquals('Unit should be pixel', Ord(utPixel), Ord(uv.UnitType));
    AssertEquals('Operation should be static', Ord(opStatic), Ord(uv.Operation));
    AssertTrue('Should be horizontal', uv.IsHorizontal);
  finally
    uv.Free;
  end;
end;

procedure TTestMigUnitValue.TestCreateWithUnit;
var
  uv: TfpgMigUnitValue;
begin
  uv := TfpgMigUnitValue.Create(5.0, utMM, '5mm');
  try
    AssertEquals('Value should be 5', 5.0, uv.Value, 0.001);
    AssertEquals('Unit should be millimeters', Ord(utMM), Ord(uv.UnitType));
  finally
    uv.Free;
  end;
end;

procedure TTestMigUnitValue.TestParseUnitType_Pixels;
var
  ut: TfpgMigUnitType;
begin
  ut := ParseUnitType('px');
  AssertEquals('px should parse as pixel', Ord(utPixel), Ord(ut));
end;

procedure TTestMigUnitValue.TestParseUnitType_Millimeters;
var
  ut: TfpgMigUnitType;
begin
  ut := ParseUnitType('mm');
  AssertEquals('mm should parse as millimeters', Ord(utMM), Ord(ut));
end;

procedure TTestMigUnitValue.TestParseUnitType_Centimeters;
var
  ut: TfpgMigUnitType;
begin
  ut := ParseUnitType('cm');
  AssertEquals('cm should parse as centimeters', Ord(utCM), Ord(ut));
end;

procedure TTestMigUnitValue.TestParseUnitType_Inches;
var
  ut: TfpgMigUnitType;
begin
  ut := ParseUnitType('in');
  AssertEquals('in should parse as inches', Ord(utInch), Ord(ut));
end;

procedure TTestMigUnitValue.TestParseUnitType_Points;
var
  ut: TfpgMigUnitType;
begin
  ut := ParseUnitType('pt');
  AssertEquals('pt should parse as points', Ord(utPT), Ord(ut));
end;

procedure TTestMigUnitValue.TestParseUnitType_Percent;
var
  ut: TfpgMigUnitType;
begin
  ut := ParseUnitType('%');
  AssertEquals('% should parse as percent', Ord(utPercent), Ord(ut));
end;

procedure TTestMigUnitValue.TestParseUnitType_LogicalPixels;
var
  ut: TfpgMigUnitType;
begin
  ut := ParseUnitType('lpx');
  AssertEquals('lpx should parse as logical horizontal pixels', Ord(utLPX), Ord(ut));

  ut := ParseUnitType('lpy');
  AssertEquals('lpy should parse as logical vertical pixels', Ord(utLPY), Ord(ut));
end;

procedure TTestMigUnitValue.TestParseUnitType_ScreenPercent;
var
  ut: TfpgMigUnitType;
begin
  ut := ParseUnitType('spx');
  AssertEquals('spx should parse as screen percent width', Ord(utSPX), Ord(ut));

  ut := ParseUnitType('spy');
  AssertEquals('spy should parse as screen percent height', Ord(utSPY), Ord(ut));
end;

procedure TTestMigUnitValue.TestParseUnitType_MinMaxPref;
var
  ut: TfpgMigUnitType;
begin
  ut := ParseUnitType('min');
  AssertEquals('min should parse as minimum', Ord(utMinSize), Ord(ut));

  ut := ParseUnitType('minimum');
  AssertEquals('minimum should parse as minimum', Ord(utMinSize), Ord(ut));

  ut := ParseUnitType('p');
  AssertEquals('p should parse as preferred', Ord(utPrefSize), Ord(ut));

  ut := ParseUnitType('pref');
  AssertEquals('pref should parse as preferred', Ord(utPrefSize), Ord(ut));

  ut := ParseUnitType('max');
  AssertEquals('max should parse as maximum', Ord(utMaxSize), Ord(ut));

  ut := ParseUnitType('maximum');
  AssertEquals('maximum should parse as maximum', Ord(utMaxSize), Ord(ut));
end;

procedure TTestMigUnitValue.TestParseUnitType_Button;
var
  ut: TfpgMigUnitType;
begin
  ut := ParseUnitType('button');
  AssertEquals('button should parse as button', Ord(utButton), Ord(ut));
end;

procedure TTestMigUnitValue.TestParseUnitType_Label;
var
  ut: TfpgMigUnitType;
begin
  ut := ParseUnitType('label');
  AssertEquals('label should parse as label align', Ord(utLabelAlign), Ord(ut));
end;

procedure TTestMigUnitValue.TestParseUnitType_Link;
var
  uv: TfpgMigUnitValue;
begin
  // Test link syntax: componentId.property
  uv := TfpgMigUnitValue.Create(0, 'btn1.width', True, opStatic, 'btn1.width');
  try
    AssertEquals('Should parse as link to width', Ord(utLinkW), Ord(uv.UnitType));
    AssertEquals('LinkTargetId should be btn1', 'btn1', uv.LinkTargetId);
  finally
    uv.Free;
  end;

  uv := TfpgMigUnitValue.Create(0, 'panel.height', False, opStatic, 'panel.height');
  try
    AssertEquals('Should parse as link to height', Ord(utLinkH), Ord(uv.UnitType));
    AssertEquals('LinkTargetId should be panel', 'panel', uv.LinkTargetId);
  finally
    uv.Free;
  end;

  uv := TfpgMigUnitValue.Create(0, 'other.x', True, opStatic, 'other.x');
  try
    AssertEquals('Should parse as link to x', Ord(utLinkX), Ord(uv.UnitType));
  finally
    uv.Free;
  end;

  uv := TfpgMigUnitValue.Create(0, 'other.y', False, opStatic, 'other.y');
  try
    AssertEquals('Should parse as link to y', Ord(utLinkY), Ord(uv.UnitType));
  finally
    uv.Free;
  end;
end;

procedure TTestMigUnitValue.TestParseUnitType_Invalid;
var
  exceptionRaised: Boolean;
begin
  exceptionRaised := False;
  try
    ParseUnitType('invalid');
  except
    on E: Exception do
      exceptionRaised := True;
  end;
  AssertTrue('Should raise exception for invalid unit string', exceptionRaised);
end;

procedure TTestMigUnitValue.TestIsAbsolute_TrueForPixels;
var
  uv: TfpgMigUnitValue;
begin
  uv := TfpgMigUnitValue.Create(10.0);
  try
    AssertTrue('Pixels should be absolute', uv.IsAbsolute);
  finally
    uv.Free;
  end;
end;

procedure TTestMigUnitValue.TestIsAbsolute_TrueForPhysicalUnits;
var
  uv: TfpgMigUnitValue;
begin
  // Test mm
  uv := TfpgMigUnitValue.Create(10.0, utMM, '10mm');
  try
    AssertTrue('Millimeters should be absolute', uv.IsAbsolute);
  finally
    uv.Free;
  end;

  // Test cm
  uv := TfpgMigUnitValue.Create(2.0, utCM, '2cm');
  try
    AssertTrue('Centimeters should be absolute', uv.IsAbsolute);
  finally
    uv.Free;
  end;

  // Test inches
  uv := TfpgMigUnitValue.Create(1.0, utInch, '1in');
  try
    AssertTrue('Inches should be absolute', uv.IsAbsolute);
  finally
    uv.Free;
  end;

  // Test points
  uv := TfpgMigUnitValue.Create(72.0, utPT, '72pt');
  try
    AssertTrue('Points should be absolute', uv.IsAbsolute);
  finally
    uv.Free;
  end;

  // Test logical pixels
  uv := TfpgMigUnitValue.Create(10.0, utLPX, '10lpx');
  try
    AssertTrue('Logical pixels should be absolute', uv.IsAbsolute);
  finally
    uv.Free;
  end;
end;

procedure TTestMigUnitValue.TestIsAbsolute_FalseForPercent;
var
  uv: TfpgMigUnitValue;
begin
  uv := TfpgMigUnitValue.Create(50.0, utPercent, '50%');
  try
    AssertFalse('Percent should not be absolute', uv.IsAbsolute);
  finally
    uv.Free;
  end;
end;

procedure TTestMigUnitValue.TestIsAbsolute_FalseForMinMaxPref;
var
  uv: TfpgMigUnitValue;
begin
  uv := TfpgMigUnitValue.Create(0, utMinSize, 'min');
  try
    AssertFalse('MinSize should not be absolute', uv.IsAbsolute);
  finally
    uv.Free;
  end;

  uv := TfpgMigUnitValue.Create(0, utPrefSize, 'pref');
  try
    AssertFalse('PrefSize should not be absolute', uv.IsAbsolute);
  finally
    uv.Free;
  end;

  uv := TfpgMigUnitValue.Create(0, utMaxSize, 'max');
  try
    AssertFalse('MaxSize should not be absolute', uv.IsAbsolute);
  finally
    uv.Free;
  end;
end;

procedure TTestMigUnitValue.TestIsLinked_FalseForSimpleUnit;
var
  uv: TfpgMigUnitValue;
begin
  uv := TfpgMigUnitValue.Create(10.0);
  try
    AssertFalse('Simple pixel unit should not be linked', uv.IsLinked);
  finally
    uv.Free;
  end;
end;

procedure TTestMigUnitValue.TestIsLinked_TrueForLinkUnit;
var
  uv: TfpgMigUnitValue;
begin
  uv := TfpgMigUnitValue.Create(0, 'button1.width', True, opStatic, 'button1.width');
  try
    AssertTrue('Link unit should be linked', uv.IsLinked);
    AssertEquals('Link target should be button1', 'button1', uv.LinkTargetId);
  finally
    uv.Free;
  end;
end;

procedure TTestMigUnitValue.TestConstantValues;
var
  zero, inf: TfpgMigUnitValue;
begin
  zero := UnitValueZero;
  AssertNotNull('UnitValueZero should not be nil', zero);
  AssertEquals('UnitValueZero value should be 0', 0.0, zero.Value, 0.001);
  AssertEquals('UnitValueZero unit should be pixel', Ord(utPixel), Ord(zero.UnitType));

  inf := UnitValueInf;
  AssertNotNull('UnitValueInf should not be nil', inf);
  AssertTrue('UnitValueInf value should be large', inf.Value > 1000000);
  AssertEquals('UnitValueInf unit should be pixel', Ord(utPixel), Ord(inf.UnitType));
end;

initialization
  RegisterTest(TTestMigUnitValue);

end.
