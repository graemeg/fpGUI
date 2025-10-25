unit fpg_mig_platformdefaults;

{
  MigLayout v11 PlatformDefaults for fpGUI

  Ported from: net.miginfocom.layout.PlatformDefaults.java (v11.4.2)

  Handles platform-specific default values for gaps, insets, button sizes, etc.
  This is a simplified implementation for Phase 1 focusing on core functionality.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_mig_unitvalue, fpg_mig_boundsize;

const
  { Platform constants }
  PLATFORM_WINDOWS = 0;
  PLATFORM_MAC_OSX = 1;
  PLATFORM_GNOME = 2;

  { Logical pixel base modes }
  BASE_FONT_SIZE = 100;      // Use font size as base for logical pixels
  BASE_SCALE_FACTOR = 101;   // Use screen DPI as base (default)
  BASE_REAL_PIXEL = 102;     // 1 logical pixel = 1 real pixel

type
  { TfpgMigPlatformDefaults - Singleton class for platform-specific defaults

    This is a simplified implementation for Phase 1. Full implementation
    will include visual padding, custom unit values, and more platform-specific
    settings as needed in later phases.
  }
  TfpgMigPlatformDefaults = class
  private
    class var FInstance: TfpgMigPlatformDefaults;
    class var FModCount: Integer;
    class var FCurrentPlatform: Integer;
    class var FLogicalPixelBase: Integer;
    class var FBaseDPI: Integer;
    class var FHorScale: Single;
    class var FVerScale: Single;
    class var FDefHUnit: Integer;  // Default horizontal unit (utLPX)
    class var FDefVUnit: Integer;  // Default vertical unit (utLPY)

    { Predefined UnitValues for common gaps }
    class var FLPX6, FLPX7, FLPX11, FLPX12, FLPX16, FLPX18, FLPX20: TfpgMigUnitValue;
    class var FLPY6, FLPY7, FLPY11, FLPY12, FLPY16, FLPY18, FLPY20: TfpgMigUnitValue;

    { Gap values }
    class var FRelatedX, FRelatedY: TfpgMigBoundSize;
    class var FUnrelatedX, FUnrelatedY: TfpgMigBoundSize;
    class var FDefHGap, FDefVGap: TfpgMigBoundSize;

    { Button defaults }
    class var FMinButtonWidth: TfpgMigUnitValue;
    class var FMinButtonPadding: TfpgMigUnitValue;

    class procedure Initialize;
    class procedure CreatePredefinedValues;
    class procedure SetPlatformDefaults(APlatform: Integer);
  public
    { Get singleton instance }
    class function Instance: TfpgMigPlatformDefaults;

    { Platform detection }
    class function GetCurrentPlatform: Integer;

    { Set current platform and apply its defaults }
    class procedure SetPlatform(APlatform: Integer);

    { Logical pixel base }
    class function GetLogicalPixelBase: Integer;
    class procedure SetLogicalPixelBase(ABase: Integer);

    { DPI settings }
    class function GetBaseDPI: Integer;
    class procedure SetBaseDPI(ADPI: Integer);

    { Scale factors }
    class function GetHorizontalScaleFactor: Single;
    class procedure SetHorizontalScaleFactor(AScale: Single);
    class function GetVerticalScaleFactor: Single;
    class procedure SetVerticalScaleFactor(AScale: Single);

    { Default units }
    class function GetDefaultHorizontalUnit: Integer;
    class procedure SetDefaultHorizontalUnit(AUnit: Integer);
    class function GetDefaultVerticalUnit: Integer;
    class procedure SetDefaultVerticalUnit(AUnit: Integer);

    { Gap values }
    class procedure SetRelatedGap(AX, AY: TfpgMigUnitValue);
    class procedure SetUnrelatedGap(AX, AY: TfpgMigUnitValue);
    class procedure SetGridCellGap(AX, AY: TfpgMigUnitValue);
    class function GetRelatedGapX: TfpgMigBoundSize;
    class function GetRelatedGapY: TfpgMigBoundSize;
    class function GetUnrelatedGapX: TfpgMigBoundSize;
    class function GetUnrelatedGapY: TfpgMigBoundSize;
    class function GetDefaultHGap: TfpgMigBoundSize;
    class function GetDefaultVGap: TfpgMigBoundSize;

    { Button defaults }
    class procedure SetMinimumButtonWidth(AWidth: TfpgMigUnitValue);
    class function GetMinimumButtonWidth: TfpgMigUnitValue;
    class procedure SetMinimumButtonPadding(APadding: TfpgMigUnitValue);
    class function GetMinimumButtonPadding: TfpgMigUnitValue;

    { Modification counter for cache invalidation }
    class function GetModCount: Integer;
  end;

implementation

uses
  fpg_base;  // For platform detection

{ TfpgMigPlatformDefaults }

class procedure TfpgMigPlatformDefaults.Initialize;
begin
  if FInstance <> nil then
    Exit;

  FInstance := TfpgMigPlatformDefaults.Create;
  FModCount := 0;
  FLogicalPixelBase := BASE_SCALE_FACTOR;
  FBaseDPI := 96;  // Default Windows DPI
  FHorScale := 1.0;
  FVerScale := 1.0;
  FDefHUnit := Ord(utLPX);
  FDefVUnit := Ord(utLPY);

  CreatePredefinedValues;

  // Set defaults for current platform
  SetPlatform(GetCurrentPlatform);
  FModCount := 0;  // Reset after initialization
end;

class procedure TfpgMigPlatformDefaults.CreatePredefinedValues;
begin
  { Create predefined LPX values }
  FLPX6 := TfpgMigUnitValue.Create(6, utLPX, '6lpx');
  FLPX7 := TfpgMigUnitValue.Create(7, utLPX, '7lpx');
  FLPX11 := TfpgMigUnitValue.Create(11, utLPX, '11lpx');
  FLPX12 := TfpgMigUnitValue.Create(12, utLPX, '12lpx');
  FLPX16 := TfpgMigUnitValue.Create(16, utLPX, '16lpx');
  FLPX18 := TfpgMigUnitValue.Create(18, utLPX, '18lpx');
  FLPX20 := TfpgMigUnitValue.Create(20, utLPX, '20lpx');

  { Create predefined LPY values }
  FLPY6 := TfpgMigUnitValue.Create(6, utLPY, '6lpy');
  FLPY7 := TfpgMigUnitValue.Create(7, utLPY, '7lpy');
  FLPY11 := TfpgMigUnitValue.Create(11, utLPY, '11lpy');
  FLPY12 := TfpgMigUnitValue.Create(12, utLPY, '12lpy');
  FLPY16 := TfpgMigUnitValue.Create(16, utLPY, '16lpy');
  FLPY18 := TfpgMigUnitValue.Create(18, utLPY, '18lpy');
  FLPY20 := TfpgMigUnitValue.Create(20, utLPY, '20lpy');
end;

class function TfpgMigPlatformDefaults.GetCurrentPlatform: Integer;
begin
  {$IFDEF MSWINDOWS}
  Result := PLATFORM_WINDOWS;
  {$ELSE}
    {$IFDEF DARWIN}
    Result := PLATFORM_MAC_OSX;
    {$ELSE}
    Result := PLATFORM_GNOME;  // Linux/Unix
    {$ENDIF}
  {$ENDIF}
end;

class procedure TfpgMigPlatformDefaults.SetPlatformDefaults(APlatform: Integer);
begin
  case APlatform of
    PLATFORM_WINDOWS:
    begin
      SetRelatedGap(FLPX7, FLPY7);
      SetUnrelatedGap(FLPX11, FLPY11);
      SetGridCellGap(FLPX7, FLPY7);
      SetMinimumButtonWidth(TfpgMigUnitValue.Create(75, utLPX, '75lpx'));
    end;

    PLATFORM_MAC_OSX:
    begin
      SetRelatedGap(FLPX7, FLPY7);
      SetUnrelatedGap(FLPX12, FLPY12);
      SetGridCellGap(FLPX7, FLPY7);
      SetMinimumButtonWidth(TfpgMigUnitValue.Create(70, utLPX, '70lpx'));
    end;

    PLATFORM_GNOME:
    begin
      SetRelatedGap(FLPX6, FLPY6);
      SetUnrelatedGap(FLPX12, FLPY12);
      SetGridCellGap(FLPX6, FLPY6);
      SetMinimumButtonWidth(TfpgMigUnitValue.Create(70, utLPX, '70lpx'));
    end;
  end;
end;

class function TfpgMigPlatformDefaults.Instance: TfpgMigPlatformDefaults;
begin
  if FInstance = nil then
    Initialize;
  Result := FInstance;
end;

class procedure TfpgMigPlatformDefaults.SetPlatform(APlatform: Integer);
begin
  if FInstance = nil then
    Initialize;

  FCurrentPlatform := APlatform;
  SetPlatformDefaults(APlatform);
  Inc(FModCount);
end;

class function TfpgMigPlatformDefaults.GetLogicalPixelBase: Integer;
begin
  if FInstance = nil then
    Initialize;
  Result := FLogicalPixelBase;
end;

class procedure TfpgMigPlatformDefaults.SetLogicalPixelBase(ABase: Integer);
begin
  if FInstance = nil then
    Initialize;

  if (ABase < BASE_FONT_SIZE) or (ABase > BASE_REAL_PIXEL) then
    raise Exception.CreateFmt('Invalid logical pixel base: %d', [ABase]);

  FLogicalPixelBase := ABase;
  Inc(FModCount);
end;

class function TfpgMigPlatformDefaults.GetBaseDPI: Integer;
begin
  if FInstance = nil then
    Initialize;
  Result := FBaseDPI;
end;

class procedure TfpgMigPlatformDefaults.SetBaseDPI(ADPI: Integer);
begin
  if FInstance = nil then
    Initialize;
  FBaseDPI := ADPI;
  Inc(FModCount);
end;

class function TfpgMigPlatformDefaults.GetHorizontalScaleFactor: Single;
begin
  if FInstance = nil then
    Initialize;
  Result := FHorScale;
end;

class procedure TfpgMigPlatformDefaults.SetHorizontalScaleFactor(AScale: Single);
begin
  if FInstance = nil then
    Initialize;
  FHorScale := AScale;
  Inc(FModCount);
end;

class function TfpgMigPlatformDefaults.GetVerticalScaleFactor: Single;
begin
  if FInstance = nil then
    Initialize;
  Result := FVerScale;
end;

class procedure TfpgMigPlatformDefaults.SetVerticalScaleFactor(AScale: Single);
begin
  if FInstance = nil then
    Initialize;
  FVerScale := AScale;
  Inc(FModCount);
end;

class function TfpgMigPlatformDefaults.GetDefaultHorizontalUnit: Integer;
begin
  if FInstance = nil then
    Initialize;
  Result := FDefHUnit;
end;

class procedure TfpgMigPlatformDefaults.SetDefaultHorizontalUnit(AUnit: Integer);
begin
  if FInstance = nil then
    Initialize;
  FDefHUnit := AUnit;
  Inc(FModCount);
end;

class function TfpgMigPlatformDefaults.GetDefaultVerticalUnit: Integer;
begin
  if FInstance = nil then
    Initialize;
  Result := FDefVUnit;
end;

class procedure TfpgMigPlatformDefaults.SetDefaultVerticalUnit(AUnit: Integer);
begin
  if FInstance = nil then
    Initialize;
  FDefVUnit := AUnit;
  Inc(FModCount);
end;

class procedure TfpgMigPlatformDefaults.SetRelatedGap(AX, AY: TfpgMigUnitValue);
begin
  if FInstance = nil then
    Initialize;

  if AX <> nil then
    FRelatedX := TfpgMigBoundSize.Create(AX, AX, nil);
  if AY <> nil then
    FRelatedY := TfpgMigBoundSize.Create(AY, AY, nil);

  Inc(FModCount);
end;

class procedure TfpgMigPlatformDefaults.SetUnrelatedGap(AX, AY: TfpgMigUnitValue);
begin
  if FInstance = nil then
    Initialize;

  if AX <> nil then
    FUnrelatedX := TfpgMigBoundSize.Create(AX, AX, nil);
  if AY <> nil then
    FUnrelatedY := TfpgMigBoundSize.Create(AY, AY, nil);

  Inc(FModCount);
end;

class procedure TfpgMigPlatformDefaults.SetGridCellGap(AX, AY: TfpgMigUnitValue);
begin
  if FInstance = nil then
    Initialize;

  if AX <> nil then
    FDefHGap := TfpgMigBoundSize.Create(AX, AX, nil);
  if AY <> nil then
    FDefVGap := TfpgMigBoundSize.Create(AY, AY, nil);

  Inc(FModCount);
end;

class function TfpgMigPlatformDefaults.GetRelatedGapX: TfpgMigBoundSize;
begin
  if FInstance = nil then
    Initialize;
  Result := FRelatedX;
end;

class function TfpgMigPlatformDefaults.GetRelatedGapY: TfpgMigBoundSize;
begin
  if FInstance = nil then
    Initialize;
  Result := FRelatedY;
end;

class function TfpgMigPlatformDefaults.GetUnrelatedGapX: TfpgMigBoundSize;
begin
  if FInstance = nil then
    Initialize;
  Result := FUnrelatedX;
end;

class function TfpgMigPlatformDefaults.GetUnrelatedGapY: TfpgMigBoundSize;
begin
  if FInstance = nil then
    Initialize;
  Result := FUnrelatedY;
end;

class function TfpgMigPlatformDefaults.GetDefaultHGap: TfpgMigBoundSize;
begin
  if FInstance = nil then
    Initialize;
  Result := FDefHGap;
end;

class function TfpgMigPlatformDefaults.GetDefaultVGap: TfpgMigBoundSize;
begin
  if FInstance = nil then
    Initialize;
  Result := FDefVGap;
end;

class procedure TfpgMigPlatformDefaults.SetMinimumButtonWidth(AWidth: TfpgMigUnitValue);
begin
  if FInstance = nil then
    Initialize;
  FMinButtonWidth := AWidth;
  Inc(FModCount);
end;

class function TfpgMigPlatformDefaults.GetMinimumButtonWidth: TfpgMigUnitValue;
begin
  if FInstance = nil then
    Initialize;
  Result := FMinButtonWidth;
end;

class procedure TfpgMigPlatformDefaults.SetMinimumButtonPadding(APadding: TfpgMigUnitValue);
begin
  if FInstance = nil then
    Initialize;
  FMinButtonPadding := APadding;
  Inc(FModCount);
end;

class function TfpgMigPlatformDefaults.GetMinimumButtonPadding: TfpgMigUnitValue;
begin
  if FInstance = nil then
    Initialize;
  Result := FMinButtonPadding;
end;

class function TfpgMigPlatformDefaults.GetModCount: Integer;
begin
  if FInstance = nil then
    Initialize;
  Result := FModCount;
end;

finalization
  // Free predefined values
  FreeAndNil(TfpgMigPlatformDefaults.FLPX6);
  FreeAndNil(TfpgMigPlatformDefaults.FLPX7);
  FreeAndNil(TfpgMigPlatformDefaults.FLPX11);
  FreeAndNil(TfpgMigPlatformDefaults.FLPX12);
  FreeAndNil(TfpgMigPlatformDefaults.FLPX16);
  FreeAndNil(TfpgMigPlatformDefaults.FLPX18);
  FreeAndNil(TfpgMigPlatformDefaults.FLPX20);
  FreeAndNil(TfpgMigPlatformDefaults.FLPY6);
  FreeAndNil(TfpgMigPlatformDefaults.FLPY7);
  FreeAndNil(TfpgMigPlatformDefaults.FLPY11);
  FreeAndNil(TfpgMigPlatformDefaults.FLPY12);
  FreeAndNil(TfpgMigPlatformDefaults.FLPY16);
  FreeAndNil(TfpgMigPlatformDefaults.FLPY18);
  FreeAndNil(TfpgMigPlatformDefaults.FLPY20);

  // Free gap values
  FreeAndNil(TfpgMigPlatformDefaults.FRelatedX);
  FreeAndNil(TfpgMigPlatformDefaults.FRelatedY);
  FreeAndNil(TfpgMigPlatformDefaults.FUnrelatedX);
  FreeAndNil(TfpgMigPlatformDefaults.FUnrelatedY);
  FreeAndNil(TfpgMigPlatformDefaults.FDefHGap);
  FreeAndNil(TfpgMigPlatformDefaults.FDefVGap);

  // Free button defaults (if they were created)
  FreeAndNil(TfpgMigPlatformDefaults.FMinButtonWidth);
  FreeAndNil(TfpgMigPlatformDefaults.FMinButtonPadding);

  FreeAndNil(TfpgMigPlatformDefaults.FInstance);

end.
