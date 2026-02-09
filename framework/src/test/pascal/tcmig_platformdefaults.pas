unit tcmig_platformdefaults;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  fpg_mig_unitvalue, fpg_mig_boundsize, fpg_mig_platformdefaults;

type
  TTestMigPlatformDefaults = class(TTestCase)
  published
    procedure TestGetCurrentPlatform;
    procedure TestSetPlatform_Windows;
    procedure TestSetPlatform_MacOSX;
    procedure TestSetPlatform_Gnome;
    procedure TestLogicalPixelBase_Default;
    procedure TestLogicalPixelBase_SetValid;
    procedure TestLogicalPixelBase_SetInvalid;
    procedure TestBaseDPI_Default;
    procedure TestBaseDPI_SetCustom;
    procedure TestScaleFactors_Default;
    procedure TestScaleFactors_SetCustom;
    procedure TestDefaultUnits;
    procedure TestRelatedGaps;
    procedure TestUnrelatedGaps;
    procedure TestGridCellGaps;
    procedure TestButtonWidth;
    procedure TestButtonPadding;
    procedure TestModCount;
  end;

implementation

{ TTestMigPlatformDefaults }

procedure TTestMigPlatformDefaults.TestGetCurrentPlatform;
var
  platform: Integer;
begin
  platform := TfpgMigPlatformDefaults.GetCurrentPlatform;

  {$IFDEF MSWINDOWS}
  AssertEquals('Platform should be Windows', PLATFORM_WINDOWS, platform);
  {$ELSE}
    {$IFDEF DARWIN}
    AssertEquals('Platform should be Mac OSX', PLATFORM_MAC_OSX, platform);
    {$ELSE}
    AssertEquals('Platform should be Gnome', PLATFORM_GNOME, platform);
    {$ENDIF}
  {$ENDIF}
end;

procedure TTestMigPlatformDefaults.TestSetPlatform_Windows;
var
  relX, relY: TfpgMigBoundSize;
begin
  TfpgMigPlatformDefaults.SetPlatform(PLATFORM_WINDOWS);

  relX := TfpgMigPlatformDefaults.GetRelatedGapX;
  relY := TfpgMigPlatformDefaults.GetRelatedGapY;

  AssertNotNull('Related gap X should be set for Windows', relX);
  AssertNotNull('Related gap Y should be set for Windows', relY);
  AssertNotNull('Related gap X min should be set', relX.Min);
  AssertNotNull('Related gap Y min should be set', relY.Min);

  // Windows uses 7lpx/7lpy for related gaps
  AssertEquals('Related gap X value should be 7', 7.0, relX.Min.Value, 0.001);
  AssertEquals('Related gap Y value should be 7', 7.0, relY.Min.Value, 0.001);
end;

procedure TTestMigPlatformDefaults.TestSetPlatform_MacOSX;
var
  unrelX, unrelY: TfpgMigBoundSize;
begin
  TfpgMigPlatformDefaults.SetPlatform(PLATFORM_MAC_OSX);

  unrelX := TfpgMigPlatformDefaults.GetUnrelatedGapX;
  unrelY := TfpgMigPlatformDefaults.GetUnrelatedGapY;

  AssertNotNull('Unrelated gap X should be set for Mac', unrelX);
  AssertNotNull('Unrelated gap Y should be set for Mac', unrelY);

  // Mac uses 12lpx/12lpy for unrelated gaps
  AssertEquals('Unrelated gap X value should be 12', 12.0, unrelX.Min.Value, 0.001);
  AssertEquals('Unrelated gap Y value should be 12', 12.0, unrelY.Min.Value, 0.001);
end;

procedure TTestMigPlatformDefaults.TestSetPlatform_Gnome;
var
  relX, cellX: TfpgMigBoundSize;
begin
  TfpgMigPlatformDefaults.SetPlatform(PLATFORM_GNOME);

  relX := TfpgMigPlatformDefaults.GetRelatedGapX;
  cellX := TfpgMigPlatformDefaults.GetDefaultHGap;

  AssertNotNull('Related gap X should be set for Gnome', relX);
  AssertNotNull('Grid cell gap X should be set for Gnome', cellX);

  // Gnome uses 6lpx for related and grid cell gaps
  AssertEquals('Related gap X value should be 6', 6.0, relX.Min.Value, 0.001);
  AssertEquals('Grid cell gap X value should be 6', 6.0, cellX.Min.Value, 0.001);
end;

procedure TTestMigPlatformDefaults.TestLogicalPixelBase_Default;
var
  base: Integer;
begin
  base := TfpgMigPlatformDefaults.GetLogicalPixelBase;
  AssertEquals('Default logical pixel base should be BASE_SCALE_FACTOR',
    BASE_SCALE_FACTOR, base);
end;

procedure TTestMigPlatformDefaults.TestLogicalPixelBase_SetValid;
begin
  TfpgMigPlatformDefaults.SetLogicalPixelBase(BASE_FONT_SIZE);
  AssertEquals('Logical pixel base should be BASE_FONT_SIZE',
    BASE_FONT_SIZE, TfpgMigPlatformDefaults.GetLogicalPixelBase);

  TfpgMigPlatformDefaults.SetLogicalPixelBase(BASE_REAL_PIXEL);
  AssertEquals('Logical pixel base should be BASE_REAL_PIXEL',
    BASE_REAL_PIXEL, TfpgMigPlatformDefaults.GetLogicalPixelBase);

  // Reset to default
  TfpgMigPlatformDefaults.SetLogicalPixelBase(BASE_SCALE_FACTOR);
end;

procedure TTestMigPlatformDefaults.TestLogicalPixelBase_SetInvalid;
var
  exceptionRaised: Boolean;
begin
  exceptionRaised := False;
  try
    TfpgMigPlatformDefaults.SetLogicalPixelBase(999);
  except
    on E: Exception do
      exceptionRaised := True;
  end;
  AssertTrue('Should raise exception for invalid base', exceptionRaised);
end;

procedure TTestMigPlatformDefaults.TestBaseDPI_Default;
var
  dpi: Integer;
begin
  dpi := TfpgMigPlatformDefaults.GetBaseDPI;
  AssertEquals('Default DPI should be 96', 96, dpi);
end;

procedure TTestMigPlatformDefaults.TestBaseDPI_SetCustom;
begin
  TfpgMigPlatformDefaults.SetBaseDPI(120);
  AssertEquals('DPI should be 120', 120, TfpgMigPlatformDefaults.GetBaseDPI);

  // Reset to default
  TfpgMigPlatformDefaults.SetBaseDPI(96);
end;

procedure TTestMigPlatformDefaults.TestScaleFactors_Default;
var
  horScale, verScale: Single;
begin
  horScale := TfpgMigPlatformDefaults.GetHorizontalScaleFactor;
  verScale := TfpgMigPlatformDefaults.GetVerticalScaleFactor;

  AssertEquals('Default horizontal scale should be 1.0', 1.0, horScale, 0.001);
  AssertEquals('Default vertical scale should be 1.0', 1.0, verScale, 0.001);
end;

procedure TTestMigPlatformDefaults.TestScaleFactors_SetCustom;
begin
  TfpgMigPlatformDefaults.SetHorizontalScaleFactor(1.5);
  TfpgMigPlatformDefaults.SetVerticalScaleFactor(2.0);

  AssertEquals('Horizontal scale should be 1.5', 1.5,
    TfpgMigPlatformDefaults.GetHorizontalScaleFactor, 0.001);
  AssertEquals('Vertical scale should be 2.0', 2.0,
    TfpgMigPlatformDefaults.GetVerticalScaleFactor, 0.001);

  // Reset to default
  TfpgMigPlatformDefaults.SetHorizontalScaleFactor(1.0);
  TfpgMigPlatformDefaults.SetVerticalScaleFactor(1.0);
end;

procedure TTestMigPlatformDefaults.TestDefaultUnits;
var
  horUnit, verUnit: Integer;
begin
  horUnit := TfpgMigPlatformDefaults.GetDefaultHorizontalUnit;
  verUnit := TfpgMigPlatformDefaults.GetDefaultVerticalUnit;

  AssertEquals('Default horizontal unit should be LPX', Ord(utLPX), horUnit);
  AssertEquals('Default vertical unit should be LPY', Ord(utLPY), verUnit);
end;

procedure TTestMigPlatformDefaults.TestRelatedGaps;
var
  uv: TfpgMigUnitValue;
  gapX, gapY: TfpgMigBoundSize;
begin
  uv := TfpgMigUnitValue.Create(5, utPixel, '5px');
  try
    TfpgMigPlatformDefaults.SetRelatedGap(uv, uv);

    gapX := TfpgMigPlatformDefaults.GetRelatedGapX;
    gapY := TfpgMigPlatformDefaults.GetRelatedGapY;

    AssertNotNull('Related gap X should not be nil', gapX);
    AssertNotNull('Related gap Y should not be nil', gapY);
    AssertTrue('Related gap X min should have same content', gapX.Min.ContentEquals(uv));
    AssertTrue('Related gap Y min should have same content', gapY.Min.ContentEquals(uv));
  finally
    uv.Free;
  end;

  // Reset to platform defaults
  TfpgMigPlatformDefaults.SetPlatform(TfpgMigPlatformDefaults.GetCurrentPlatform);
end;

procedure TTestMigPlatformDefaults.TestUnrelatedGaps;
var
  uv: TfpgMigUnitValue;
  gapX, gapY: TfpgMigBoundSize;
begin
  uv := TfpgMigUnitValue.Create(10, utPixel, '10px');
  try
    TfpgMigPlatformDefaults.SetUnrelatedGap(uv, uv);

    gapX := TfpgMigPlatformDefaults.GetUnrelatedGapX;
    gapY := TfpgMigPlatformDefaults.GetUnrelatedGapY;

    AssertNotNull('Unrelated gap X should not be nil', gapX);
    AssertNotNull('Unrelated gap Y should not be nil', gapY);
    AssertTrue('Unrelated gap X min should have same content', gapX.Min.ContentEquals(uv));
    AssertTrue('Unrelated gap Y min should have same content', gapY.Min.ContentEquals(uv));
  finally
    uv.Free;
  end;

  // Reset to platform defaults
  TfpgMigPlatformDefaults.SetPlatform(TfpgMigPlatformDefaults.GetCurrentPlatform);
end;

procedure TTestMigPlatformDefaults.TestGridCellGaps;
var
  uv: TfpgMigUnitValue;
  gapX, gapY: TfpgMigBoundSize;
begin
  uv := TfpgMigUnitValue.Create(8, utPixel, '8px');
  try
    TfpgMigPlatformDefaults.SetGridCellGap(uv, uv);

    gapX := TfpgMigPlatformDefaults.GetDefaultHGap;
    gapY := TfpgMigPlatformDefaults.GetDefaultVGap;

    AssertNotNull('Grid cell gap X should not be nil', gapX);
    AssertNotNull('Grid cell gap Y should not be nil', gapY);
    AssertTrue('Grid cell gap X min should have same content', gapX.Min.ContentEquals(uv));
    AssertTrue('Grid cell gap Y min should have same content', gapY.Min.ContentEquals(uv));
  finally
    uv.Free;
  end;

  // Reset to platform defaults
  TfpgMigPlatformDefaults.SetPlatform(TfpgMigPlatformDefaults.GetCurrentPlatform);
end;

procedure TTestMigPlatformDefaults.TestButtonWidth;
var
  uv: TfpgMigUnitValue;
  width: TfpgMigUnitValue;
begin
  uv := TfpgMigUnitValue.Create(100, utPixel, '100px');
  try
    TfpgMigPlatformDefaults.SetMinimumButtonWidth(uv);

    width := TfpgMigPlatformDefaults.GetMinimumButtonWidth;
    AssertTrue('Button width should have same content', width.ContentEquals(uv));
  finally
    uv.Free;
  end;

  // Reset to platform defaults
  TfpgMigPlatformDefaults.SetPlatform(TfpgMigPlatformDefaults.GetCurrentPlatform);
end;

procedure TTestMigPlatformDefaults.TestButtonPadding;
var
  uv: TfpgMigUnitValue;
  padding: TfpgMigUnitValue;
begin
  uv := TfpgMigUnitValue.Create(4, utPixel, '4px');
  try
    TfpgMigPlatformDefaults.SetMinimumButtonPadding(uv);

    padding := TfpgMigPlatformDefaults.GetMinimumButtonPadding;
    AssertTrue('Button padding should have same content', padding.ContentEquals(uv));
  finally
    uv.Free;
  end;

  // Reset to nil (default has no padding)
  TfpgMigPlatformDefaults.SetMinimumButtonPadding(nil);
end;

procedure TTestMigPlatformDefaults.TestModCount;
var
  count1, count2: Integer;
begin
  count1 := TfpgMigPlatformDefaults.GetModCount;

  // Changing a setting should increment mod count
  TfpgMigPlatformDefaults.SetBaseDPI(120);
  count2 := TfpgMigPlatformDefaults.GetModCount;

  AssertTrue('ModCount should increment after change', count2 > count1);

  // Reset
  TfpgMigPlatformDefaults.SetBaseDPI(96);
end;

initialization
  RegisterTest(TTestMigPlatformDefaults);

end.
