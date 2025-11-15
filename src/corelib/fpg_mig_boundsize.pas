unit fpg_mig_boundsize;

{
  MigLayout v11 BoundSize for fpGUI

  Ported from: net.miginfocom.layout.BoundSize.java (v11.4.2)

  Represents a size with minimum, preferred, and maximum bounds.
  Each bound is a TfpgMigUnitValue that can be null.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_mig_unitvalue;

type
  { TfpgMigBoundSize - A size that contains minimum, preferred and maximum size

    This class is immutable - once created, values cannot be changed.
    Null values mean that boundary is "not in use".
  }
  TfpgMigBoundSize = class
  private
    FMin: TfpgMigUnitValue;
    FPref: TfpgMigUnitValue;
    FMax: TfpgMigUnitValue;
    FGapPush: Boolean;
  public
    { Constructor using same value for min/preferred/max }
    constructor Create(AMinMaxPref: TfpgMigUnitValue); overload;

    { Constructor with separate min, preferred, and max values }
    constructor Create(AMin, APref, AMax: TfpgMigUnitValue); overload;

    { Constructor with gap push flag }
    constructor Create(AMin, APref, AMax: TfpgMigUnitValue; AGapPush: Boolean); overload;

    destructor Destroy; override;

    { Returns true if all bounds are nil (unset) }
    function IsUnset: Boolean;

    { Returns true if any bound contains links to other components }
    function IsLinked: Boolean;

    { Returns true if all bounds are absolute (not relative) }
    function IsAbsolute: Boolean;

    { Raises exception if this BoundSize contains links }
    procedure CheckNotLinked;

    { Getters for the bounds }
    property Min: TfpgMigUnitValue read FMin;
    property Preferred: TfpgMigUnitValue read FPref;
    property Max: TfpgMigUnitValue read FMax;
    property GapPush: Boolean read FGapPush;
  end;

  { Array type for arrays of BoundSizes }
  TfpgMigBoundSizeArray = array of TfpgMigBoundSize;

  { Predefined constant bound sizes }
  function BoundSizeNullSize: TfpgMigBoundSize;
  function BoundSizeZeroPixel: TfpgMigBoundSize;

implementation

const
  LAYOUT_INF = 2097051;  // LayoutUtil.INF from Java

var
  { Cached constant bound sizes }
  _BoundSizeNullSize: TfpgMigBoundSize = nil;
  _BoundSizeZeroPixel: TfpgMigBoundSize = nil;

{ TfpgMigBoundSize }

constructor TfpgMigBoundSize.Create(AMinMaxPref: TfpgMigUnitValue);
begin
  Create(AMinMaxPref, AMinMaxPref, AMinMaxPref, False);
end;

constructor TfpgMigBoundSize.Create(AMin, APref, AMax: TfpgMigUnitValue);
begin
  Create(AMin, APref, AMax, False);
end;

constructor TfpgMigBoundSize.Create(AMin, APref, AMax: TfpgMigUnitValue; AGapPush: Boolean);
begin
  inherited Create;

  // Clone UnitValues to take ownership, but don't clone global constants
  if (AMin = nil) or (AMin = UnitValueZero) or (AMin = UnitValueInf) or
     (AMin = UnitValueLeading) or (AMin = UnitValueCenter) or (AMin = UnitValueBaselineIdentity) then
    FMin := AMin
  else
    FMin := AMin.Clone;

  if (APref = nil) or (APref = UnitValueZero) or (APref = UnitValueInf) or
     (APref = UnitValueLeading) or (APref = UnitValueCenter) or (APref = UnitValueBaselineIdentity) then
    FPref := APref
  else
    FPref := APref.Clone;

  if (AMax = nil) or (AMax = UnitValueZero) or (AMax = UnitValueInf) or
     (AMax = UnitValueLeading) or (AMax = UnitValueCenter) or (AMax = UnitValueBaselineIdentity) then
    FMax := AMax
  else
    FMax := AMax.Clone;

  FGapPush := AGapPush;
end;

destructor TfpgMigBoundSize.Destroy;
begin
  // Free owned UnitValues, but do not free the global constants
  // Use try-except to handle finalization order issues where global constants
  // might already be freed when this destructor runs
  if (FMin <> nil) and (FMin <> UnitValueZero) and (FMin <> UnitValueInf) and
     (FMin <> UnitValueLeading) and (FMin <> UnitValueCenter) and (FMin <> UnitValueBaselineIdentity) then
  try
    FreeAndNil(FMin);
  except
    // Silently ignore errors during finalization
  end;

  if (FPref <> nil) and (FPref <> UnitValueZero) and (FPref <> UnitValueInf) and
     (FPref <> UnitValueLeading) and (FPref <> UnitValueCenter) and (FPref <> UnitValueBaselineIdentity) then
  try
    FreeAndNil(FPref);
  except
    // Silently ignore errors during finalization
  end;

  if (FMax <> nil) and (FMax <> UnitValueZero) and (FMax <> UnitValueInf) and
     (FMax <> UnitValueLeading) and (FMax <> UnitValueCenter) and (FMax <> UnitValueBaselineIdentity) then
  try
    FreeAndNil(FMax);
  except
    // Silently ignore errors during finalization
  end;

  inherited Destroy;
end;

function TfpgMigBoundSize.IsUnset: Boolean;
begin
  // Check if this is the ZERO_PIXEL constant
  if Self = _BoundSizeZeroPixel then
    Exit(True);

  // All values must be nil and gapPush must be false
  Result := (FPref = nil) and (FMin = nil) and (FMax = nil) and (not FGapPush);
end;

function TfpgMigBoundSize.IsLinked: Boolean;
begin
  Result := False;

  if (FMin <> nil) and FMin.IsLinked then
    Exit(True);

  if (FPref <> nil) and FPref.IsLinked then
    Exit(True);

  if (FMax <> nil) and FMax.IsLinked then
    Exit(True);
end;

function TfpgMigBoundSize.IsAbsolute: Boolean;
begin
  // All non-nil values must be absolute
  Result := ((FMin = nil) or FMin.IsAbsolute) and
            ((FPref = nil) or FPref.IsAbsolute) and
            ((FMax = nil) or FMax.IsAbsolute);
end;

procedure TfpgMigBoundSize.CheckNotLinked;
begin
  if IsLinked then
    raise Exception.Create('Size may not contain links');
end;

{ Predefined constant values }

function BoundSizeNullSize: TfpgMigBoundSize;
begin
  if _BoundSizeNullSize = nil then
    _BoundSizeNullSize := TfpgMigBoundSize.Create(nil, nil, nil);
  Result := _BoundSizeNullSize;
end;

function BoundSizeZeroPixel: TfpgMigBoundSize;
begin
  if _BoundSizeZeroPixel = nil then
    _BoundSizeZeroPixel := TfpgMigBoundSize.Create(UnitValueZero);
  Result := _BoundSizeZeroPixel;
end;

finalization
  FreeAndNil(_BoundSizeNullSize);
  FreeAndNil(_BoundSizeZeroPixel);

end.
