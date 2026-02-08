unit fpg_mig_platformdefaults;

{
  MigLayout v11 PlatformDefaults for fpGUI

  Ported from: net.miginfocom.layout.PlatformDefaults.java (v11.4.2)

  Handles platform-specific default values for gaps, insets, button sizes, etc.
  This is a simplified implementation for Phase 1 focusing on core functionality.
}

{$I fpg_defines.inc}

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

    class var FPanelInsets: array[0..3] of TfpgMigUnitValue;
    class var FDialogInsets: array[0..3] of TfpgMigUnitValue;

    { Predefined UnitValues for common gaps }
    class var FLPX6, FLPX7, FLPX11, FLPX12, FLPX16, FLPX18, FLPX20: TfpgMigUnitValue;
    class var FLPY6, FLPY7, FLPY11, FLPY12, FLPY16, FLPY18, FLPY20: TfpgMigUnitValue;
    class var FLPX70, FLPX75: TfpgMigUnitValue;  // Button widths

    { Gap values }
    class var FRelatedX, FRelatedY: TfpgMigBoundSize;
    class var FUnrelatedX, FUnrelatedY: TfpgMigBoundSize;
    class var FDefHGap, FDefVGap: TfpgMigBoundSize;

    { Button defaults }
    class var FMinButtonWidth: TfpgMigUnitValue;
    class var FMinButtonPadding: TfpgMigUnitValue;
    class var FButtonOrder: string;  // Platform-specific button order string

    { Row alignment }
    class var FDefaultRowAlignmentBaseline: Boolean;

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

    { Pixel conversion }
    class function GetPixelUnitFactor(AIsHor: Boolean): Single;

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

    { Inset values }
    class function GetPanelInsets(ASide: Integer): TfpgMigUnitValue;
    class procedure SetPanelInsets(ATop, ALeft, ABottom, ARight: TfpgMigUnitValue);
    class function GetDialogInsets(ASide: Integer): TfpgMigUnitValue;
    class procedure SetDialogInsets(ATop, ALeft, ABottom, ARight: TfpgMigUnitValue);

    { Button defaults }
    class procedure SetMinimumButtonWidth(AWidth: TfpgMigUnitValue);
    class function GetMinimumButtonWidth: TfpgMigUnitValue;
    class procedure SetMinimumButtonPadding(APadding: TfpgMigUnitValue);
    class function GetMinimumButtonPadding: TfpgMigUnitValue;

    { Button order for platform-specific button bar layouts }
    class function GetButtonOrder: string;
    class procedure SetButtonOrder(const AOrder: string);
    class function GetTagForChar(AChar: Char): string;

    { Row alignment }
    class function GetDefaultRowAlignmentBaseline: Boolean;
    class procedure SetDefaultRowAlignmentBaseline(AValue: Boolean);

    { Modification counter for cache invalidation }
    class function GetModCount: Integer;
  end;

implementation

uses
  fpg_base, fpg_main,  // For platform detection and screen DPI
  inifiles, fpg_utils, fpg_constants;


{ TfpgMigPlatformDefaults }

class procedure TfpgMigPlatformDefaults.Initialize;
var
  Ini: TINIFile;
  IniFileName: string;
  ScaleFactor: Single;
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
  FDefaultRowAlignmentBaseline := True;  // Default since v3.5

  // Read scale factor from fpgui.ini
  IniFileName := fpgGetToolkitConfigDir + FPG_CONFIG_FILE;
  if fpgFileExists(IniFileName) then
  begin
    Ini := TINIFile.Create(IniFileName);
    try
      // Read scale factor from [Display] section
      ScaleFactor := Ini.ReadFloat(FPG_DISPLAY_SECTION, 'ScaleFactor', 1.0);
      if (ScaleFactor > 0) and (abs(ScaleFactor - 1.0) > 1e-6) then
      begin
        FHorScale := ScaleFactor;
        FVerScale := ScaleFactor;
      end;
    finally
      Ini.Free;
    end;
  end;

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

  { Create predefined button widths }
  FLPX70 := TfpgMigUnitValue.Create(70, utLPX, '70lpx');
  FLPX75 := TfpgMigUnitValue.Create(75, utLPX, '75lpx');
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
      SetMinimumButtonWidth(FLPX75);
      SetButtonOrder('L_E+U+YNBXOCAH_I_R');  // Windows button order
      SetDialogInsets(FLPY11, FLPX11, FLPY11, FLPX11);
      SetPanelInsets(FLPY7, FLPX7, FLPY7, FLPX7);
    end;

    PLATFORM_MAC_OSX:
    begin
      SetRelatedGap(FLPX7, FLPY7);
      SetUnrelatedGap(FLPX12, FLPY12);
      SetGridCellGap(FLPX7, FLPY7);
      SetMinimumButtonWidth(FLPX70);
      SetButtonOrder('L_HE+U+NYBXCOA_I_R');  // Mac OS X button order
      SetDialogInsets(FLPY20, FLPX20, FLPY20, FLPX20);
      SetPanelInsets(FLPY16, FLPX16, FLPY16, FLPX16);
    end;

    PLATFORM_GNOME:
    begin
      SetRelatedGap(FLPX6, FLPY6);
      SetUnrelatedGap(FLPX12, FLPY12);
      SetGridCellGap(FLPX6, FLPY6);
      SetMinimumButtonWidth(FLPX70);
      SetButtonOrder('L_HE+UNYACBXO_I_R');  // GNOME button order
      SetDialogInsets(FLPY12, FLPX12, FLPY12, FLPX12);
      SetPanelInsets(FLPY6, FLPX6, FLPY6, FLPX6);
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

class function TfpgMigPlatformDefaults.GetPixelUnitFactor(AIsHor: Boolean): Single;
var
  s: Single;
  screenDPI: Integer;
begin
  if FInstance = nil then Initialize;

  case FLogicalPixelBase of
    BASE_FONT_SIZE:
      begin
        // TODO: Port font-based scaling from SwingComponentWrapper
        Result := 1.0;
      end;
    BASE_SCALE_FACTOR:
      begin
        if AIsHor then
          s := FHorScale
        else
          s := FVerScale;

        // Use explicit scale factor if set (and not 1.0)
        if abs(s - 1.0) > 1e-6 then
        begin
          Result := s;
          Exit;
        end;

        // Otherwise, calculate based on DPI.
        if (fpgApplication <> nil) then
        begin
          screenDPI := fpgApplication.Screen_dpi;
          if screenDPI > 0 then
            Result := screenDPI / GetBaseDPI
          else
            Result := 1.0; // Fallback if DPI is not available
        end
        else
          Result := 1.0; // Fallback if fpgApplication is not ready
      end;
  else // BASE_REAL_PIXEL
    Result := 1.0;
  end;
end;

class procedure TfpgMigPlatformDefaults.SetRelatedGap(AX, AY: TfpgMigUnitValue);
begin
  if FInstance = nil then
    Initialize;

  if AX <> nil then
  begin
    FreeAndNil(FRelatedX);
    FRelatedX := TfpgMigBoundSize.Create(AX, AX, nil);
  end;
  if AY <> nil then
  begin
    FreeAndNil(FRelatedY);
    FRelatedY := TfpgMigBoundSize.Create(AY, AY, nil);
  end;

  Inc(FModCount);
end;

class procedure TfpgMigPlatformDefaults.SetUnrelatedGap(AX, AY: TfpgMigUnitValue);
begin
  if FInstance = nil then
    Initialize;

  if AX <> nil then
  begin
    FreeAndNil(FUnrelatedX);
    FUnrelatedX := TfpgMigBoundSize.Create(AX, AX, nil);
  end;
  if AY <> nil then
  begin
    FreeAndNil(FUnrelatedY);
    FUnrelatedY := TfpgMigBoundSize.Create(AY, AY, nil);
  end;

  Inc(FModCount);
end;

class procedure TfpgMigPlatformDefaults.SetGridCellGap(AX, AY: TfpgMigUnitValue);
begin
  if FInstance = nil then
    Initialize;

  if AX <> nil then
  begin
    FreeAndNil(FDefHGap);
    FDefHGap := TfpgMigBoundSize.Create(AX, AX, nil);
  end;
  if AY <> nil then
  begin
    FreeAndNil(FDefVGap);
    FDefVGap := TfpgMigBoundSize.Create(AY, AY, nil);
  end;

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

class function TfpgMigPlatformDefaults.GetPanelInsets(ASide: Integer): TfpgMigUnitValue;
begin
  if FInstance = nil then Initialize;
  Result := FPanelInsets[ASide];
end;

class procedure TfpgMigPlatformDefaults.SetPanelInsets(ATop, ALeft, ABottom, ARight: TfpgMigUnitValue);
begin
  if FInstance = nil then Initialize;
  if ATop <> nil then FPanelInsets[0] := ATop;
  if ALeft <> nil then FPanelInsets[1] := ALeft;
  if ABottom <> nil then FPanelInsets[2] := ABottom;
  if ARight <> nil then FPanelInsets[3] := ARight;
  Inc(FModCount);
end;

class function TfpgMigPlatformDefaults.GetDialogInsets(ASide: Integer): TfpgMigUnitValue;
begin
  if FInstance = nil then Initialize;
  Result := FDialogInsets[ASide];
end;

class procedure TfpgMigPlatformDefaults.SetDialogInsets(ATop, ALeft, ABottom, ARight: TfpgMigUnitValue);
begin
  if FInstance = nil then Initialize;
  if ATop <> nil then FDialogInsets[0] := ATop;
  if ALeft <> nil then FDialogInsets[1] := ALeft;
  if ABottom <> nil then FDialogInsets[2] := ABottom;
  if ARight <> nil then FDialogInsets[3] := ARight;
  Inc(FModCount);
end;

class procedure TfpgMigPlatformDefaults.SetMinimumButtonWidth(AWidth: TfpgMigUnitValue);
begin
  if FInstance = nil then
    Initialize;
  // Note: We don't free the old value as we don't own it
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
  // Note: We don't free the old value as we don't own it
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

{ Row alignment }

class function TfpgMigPlatformDefaults.GetDefaultRowAlignmentBaseline: Boolean;
begin
  if FInstance = nil then
    Initialize;
  Result := FDefaultRowAlignmentBaseline;
end;

class procedure TfpgMigPlatformDefaults.SetDefaultRowAlignmentBaseline(AValue: Boolean);
begin
  if FInstance = nil then
    Initialize;
  FDefaultRowAlignmentBaseline := AValue;
  Inc(FModCount);
end;

{ Button order methods }

class function TfpgMigPlatformDefaults.GetButtonOrder: string;
begin
  if FInstance = nil then
    Initialize;
  Result := FButtonOrder;
end;

class procedure TfpgMigPlatformDefaults.SetButtonOrder(const AOrder: string);
begin
  if FInstance = nil then
    Initialize;
  FButtonOrder := AOrder;
  Inc(FModCount);
end;

class function TfpgMigPlatformDefaults.GetTagForChar(AChar: Char): string;
begin
  // Convert to lowercase for case-insensitive matching
  case LowerCase(AChar) of
    'o': Result := 'ok';
    'c': Result := 'cancel';
    'h': Result := 'help';
    'e': Result := 'help2';
    'y': Result := 'yes';
    'n': Result := 'no';
    'a': Result := 'apply';
    'x': Result := 'next';    // a.k.a forward
    'b': Result := 'back';    // a.k.a. previous
    'i': Result := 'finish';
    'l': Result := 'left';
    'r': Result := 'right';
    'u': Result := 'other';
  else
    Result := '';  // Unknown tag
  end;
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
  FreeAndNil(TfpgMigPlatformDefaults.FLPX70);
  FreeAndNil(TfpgMigPlatformDefaults.FLPX75);

  // Free gap values
  FreeAndNil(TfpgMigPlatformDefaults.FRelatedX);
  FreeAndNil(TfpgMigPlatformDefaults.FRelatedY);
  FreeAndNil(TfpgMigPlatformDefaults.FUnrelatedX);
  FreeAndNil(TfpgMigPlatformDefaults.FUnrelatedY);
  FreeAndNil(TfpgMigPlatformDefaults.FDefHGap);
  FreeAndNil(TfpgMigPlatformDefaults.FDefVGap);

  // Note: FMinButtonWidth and FMinButtonPadding are not freed here as they are
  // either references to predefined values (FLPX70, FLPX75) or externally owned

  FreeAndNil(TfpgMigPlatformDefaults.FInstance);

end.
