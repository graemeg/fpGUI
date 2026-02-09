unit fpg_mig_dimconstraint;

{
  MigLayout v11 DimConstraint for fpGUI

  Ported from: net.miginfocom.layout.DimConstraint.java (v11.4.2)

  Constraints for a single dimension (row or column).
  Handles size, alignment, gaps, grow/shrink priorities and weights.
}

{$I fpg_defines.inc}

interface

uses
  Classes, SysUtils, Math,
  fpg_base,
  fpg_mig_unitvalue, fpg_mig_boundsize, fpg_mig_resizeconstraint;

type
  { Dynamic array type for gap sizes [min, pref, max] }
  TfpgMigGapArray = array of Integer;

const
  { Default grow/shrink priorities }
  DEFAULT_PRIORITY = 100;

  { Default shrink weight }
  DEFAULT_SHRINK_WEIGHT = 100.0;

type
  { TfpgMigDimConstraint - Constraint for one dimension (row or column)

    Handles sizing, alignment, gaps, and resize behavior for a single dimension.
  }
  TfpgMigDimConstraint = class
  private
    { Resize properties }
    FGrowPriority: Integer;
    FGrowWeight: Single;      // Using Single instead of nullable Float
    FHasGrowWeight: Boolean;   // Track if grow weight is set
    FShrinkPriority: Integer;
    FShrinkWeight: Single;
    FHasShrinkWeight: Boolean;

    { Size and alignment }
    FSize: TfpgMigBoundSize;
    FAlign: TfpgMigUnitValue;

    { Gaps }
    FGapBefore: TfpgMigBoundSize;
    FGapAfter: TfpgMigBoundSize;

    { Grouping }
    FSizeGroup: string;
    FEndGroup: string;  // Only for components

    { Row/column specific }
    FFill: Boolean;
    FNoGrid: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function GetResize: TfpgMigResizeConstraint;

    { Grow properties }
    function GetGrowPriority: Integer;
    procedure SetGrowPriority(APriority: Integer);
    function GetGrowWeight: Single;
    function HasGrowWeight: Boolean;
    procedure SetGrowWeight(AWeight: Single);
    procedure ClearGrowWeight;

    { Shrink properties }
    function GetShrinkPriority: Integer;
    procedure SetShrinkPriority(APriority: Integer);
    function GetShrinkWeight: Single;
    function HasShrinkWeight: Boolean;
    procedure SetShrinkWeight(AWeight: Single);
    procedure ClearShrinkWeight;

    { Alignment }
    function GetAlign: TfpgMigUnitValue;
    procedure SetAlign(AAlign: TfpgMigUnitValue);
    function GetAlignOrDefault(AIsCols: Boolean): TfpgMigUnitValue;

    { Size }
    function GetSize: TfpgMigBoundSize;
    procedure SetSize(ASize: TfpgMigBoundSize);

    { Gaps }
    function GetGapBefore: TfpgMigBoundSize;
    procedure SetGapBefore(AGap: TfpgMigBoundSize);
    function HasGapBefore: Boolean;
    function IsGapBeforePush: Boolean;

    function GetGapAfter: TfpgMigBoundSize;
    procedure SetGapAfter(AGap: TfpgMigBoundSize);
    function HasGapAfter: Boolean;
    function IsGapAfterPush: Boolean;

    { Get component gap sizes for layout calculation
      @param AParent The parent container
      @param AComp The component this constraint is for
      @param AAdjGap Gap from adjacent component (may be nil)
      @param AAdjacentComp The adjacent component (may be nil)
      @param ATag Tag string from component constraints (may be '')
      @param ARefSize Reference size for pixel calculation
      @param AAdjacentSide Which side: 0=top, 1=left, 2=bottom, 3=right
      @param AIsLTR Left-to-right layout flag
      @returns [min,pref,max] array or empty if no gap }
    function GetComponentGaps(AParent: TfpgWidgetBase; AComp: TfpgWidgetBase;
                             AAdjGap: TfpgMigBoundSize; AAdjacentComp: TfpgWidgetBase;
                             const ATag: string; ARefSize, AAdjacentSide: Integer;
                             AIsLTR: Boolean): TfpgMigGapArray;

    { Grouping }
    function GetSizeGroup: string;
    procedure SetSizeGroup(const AGroup: string);
    function GetEndGroup: string;
    procedure SetEndGroup(const AGroup: string);

    { Row/column properties }
    function IsFill: Boolean;
    procedure SetFill(AFill: Boolean);
    function IsNoGrid: Boolean;
    procedure SetNoGrid(ANoGrid: Boolean);

    property Resize: TfpgMigResizeConstraint read GetResize;
  end;

implementation

uses
  fpg_mig_platformdefaults, fpg_mig_layoututil;

{ TfpgMigDimConstraint }

constructor TfpgMigDimConstraint.Create;
begin
  inherited Create;

  { Initialize resize properties }
  FGrowPriority := DEFAULT_PRIORITY;
  FGrowWeight := NaN;  // Not set by default (Java null equivalent)
  FHasGrowWeight := False;

  FShrinkPriority := DEFAULT_PRIORITY;
  FShrinkWeight := DEFAULT_SHRINK_WEIGHT;
  FHasShrinkWeight := True;

  { Initialize size - use NULL_SIZE constant }
  FSize := BoundSizeNullSize;
  FAlign := nil;

  { Initialize gaps }
  FGapBefore := nil;
  FGapAfter := nil;

  { Initialize grouping }
  FSizeGroup := '';
  FEndGroup := '';

  { Initialize row/column flags }
  FFill := False;
  FNoGrid := False;
end;

destructor TfpgMigDimConstraint.Destroy;
begin
  // Free BoundSize objects if they exist (but not constants)
  if (FSize <> nil) and (FSize <> BoundSizeNullSize) and (FSize <> BoundSizeZeroPixel) then
    FSize.Free;
  if (FGapBefore <> nil) and (FGapBefore <> BoundSizeNullSize) and (FGapBefore <> BoundSizeZeroPixel) then
    FGapBefore.Free;
  if (FGapAfter <> nil) and (FGapAfter <> BoundSizeNullSize) and (FGapAfter <> BoundSizeZeroPixel) then
    FGapAfter.Free;

  inherited Destroy;
end;

function TfpgMigDimConstraint.GetResize: TfpgMigResizeConstraint;
begin
  Result := TfpgMigResizeConstraint.Create(FShrinkPriority, FShrinkWeight, FGrowPriority, FGrowWeight);
end;

{ Grow properties }

function TfpgMigDimConstraint.GetGrowPriority: Integer;
begin
  Result := FGrowPriority;
end;

procedure TfpgMigDimConstraint.SetGrowPriority(APriority: Integer);
begin
  FGrowPriority := APriority;
end;

function TfpgMigDimConstraint.GetGrowWeight: Single;
begin
  Result := FGrowWeight;
end;

function TfpgMigDimConstraint.HasGrowWeight: Boolean;
begin
  Result := FHasGrowWeight;
end;

procedure TfpgMigDimConstraint.SetGrowWeight(AWeight: Single);
begin
  FGrowWeight := AWeight;
  FHasGrowWeight := True;
end;

procedure TfpgMigDimConstraint.ClearGrowWeight;
begin
  FGrowWeight := NaN;  // Clear means "not set" (Java null equivalent)
  FHasGrowWeight := False;
end;

{ Shrink properties }

function TfpgMigDimConstraint.GetShrinkPriority: Integer;
begin
  Result := FShrinkPriority;
end;

procedure TfpgMigDimConstraint.SetShrinkPriority(APriority: Integer);
begin
  FShrinkPriority := APriority;
end;

function TfpgMigDimConstraint.GetShrinkWeight: Single;
begin
  Result := FShrinkWeight;
end;

function TfpgMigDimConstraint.HasShrinkWeight: Boolean;
begin
  Result := FHasShrinkWeight;
end;

procedure TfpgMigDimConstraint.SetShrinkWeight(AWeight: Single);
begin
  FShrinkWeight := AWeight;
  FHasShrinkWeight := True;
end;

procedure TfpgMigDimConstraint.ClearShrinkWeight;
begin
  FShrinkWeight := DEFAULT_SHRINK_WEIGHT;
  FHasShrinkWeight := False;
end;

{ Alignment }

function TfpgMigDimConstraint.GetAlign: TfpgMigUnitValue;
begin
  Result := FAlign;
end;

procedure TfpgMigDimConstraint.SetAlign(AAlign: TfpgMigUnitValue);
begin
  FAlign := AAlign;
end;

function TfpgMigDimConstraint.GetAlignOrDefault(AIsCols: Boolean): TfpgMigUnitValue;
begin
  if FAlign <> nil then
  begin
    Result := FAlign;
    Exit;
  end;

  if AIsCols then
    Result := UnitValueLeading
  else
  begin
    { For rows: if fill or baseline mode is disabled, use CENTER, else BASELINE }
    if FFill or (not TfpgMigPlatformDefaults.GetDefaultRowAlignmentBaseline) then
      Result := UnitValueCenter
    else
      Result := UnitValueBaselineIdentity;
  end;
end;

{ Size }

function TfpgMigDimConstraint.GetSize: TfpgMigBoundSize;
begin
  Result := FSize;
end;

procedure TfpgMigDimConstraint.SetSize(ASize: TfpgMigBoundSize);
begin
  if ASize <> nil then
    ASize.CheckNotLinked;
  // Free old BoundSize before assigning new one (but not constants)
  if (FSize <> nil) and (FSize <> BoundSizeNullSize) and (FSize <> BoundSizeZeroPixel) then
    FSize.Free;
  FSize := ASize;
end;

{ Gaps }

function TfpgMigDimConstraint.GetGapBefore: TfpgMigBoundSize;
begin
  Result := FGapBefore;
end;

procedure TfpgMigDimConstraint.SetGapBefore(AGap: TfpgMigBoundSize);
begin
  // Free old BoundSize before assigning new one (but not constants)
  if (FGapBefore <> nil) and (FGapBefore <> BoundSizeNullSize) and (FGapBefore <> BoundSizeZeroPixel) then
    FGapBefore.Free;
  FGapBefore := AGap;
end;

function TfpgMigDimConstraint.HasGapBefore: Boolean;
begin
  Result := (FGapBefore <> nil) and (not FGapBefore.IsUnset);
end;

function TfpgMigDimConstraint.IsGapBeforePush: Boolean;
begin
  Result := (FGapBefore <> nil) and FGapBefore.GapPush;
end;

function TfpgMigDimConstraint.GetGapAfter: TfpgMigBoundSize;
begin
  Result := FGapAfter;
end;

procedure TfpgMigDimConstraint.SetGapAfter(AGap: TfpgMigBoundSize);
begin
  // Free old BoundSize before assigning new one (but not constants)
  if (FGapAfter <> nil) and (FGapAfter <> BoundSizeNullSize) and (FGapAfter <> BoundSizeZeroPixel) then
    FGapAfter.Free;
  FGapAfter := AGap;
end;

function TfpgMigDimConstraint.HasGapAfter: Boolean;
begin
  Result := (FGapAfter <> nil) and (not FGapAfter.IsUnset);
end;

function TfpgMigDimConstraint.IsGapAfterPush: Boolean;
begin
  Result := (FGapAfter <> nil) and FGapAfter.GapPush;
end;

function TfpgMigDimConstraint.GetComponentGaps(AParent: TfpgWidgetBase;
  AComp: TfpgWidgetBase; AAdjGap: TfpgMigBoundSize;
  AAdjacentComp: TfpgWidgetBase; const ATag: string; ARefSize,
  AAdjacentSide: Integer; AIsLTR: Boolean): TfpgMigGapArray;
var
  gap: TfpgMigBoundSize;
  hasGap: Boolean;
  uv: TfpgMigUnitValue;
begin
  // Determine which gap to use based on side (0=top, 1=left are "before", 2=bottom, 3=right are "after")
  if AAdjacentSide < 2 then
    gap := FGapBefore
  else
    gap := FGapAfter;

  hasGap := (gap <> nil) and gap.GapPush;

  // If no gap is set, use platform defaults
  // TODO: Implement full GetDefaultComponentGap with component type detection
  if ((gap = nil) or gap.IsUnset) and ((AAdjGap = nil) or AAdjGap.IsUnset) and (AComp <> nil) then
  begin
    // For now, use related gap as default (horizontal for left/right, vertical for top/bottom)
    if AAdjacentSide in [1, 3] then  // left=1, right=3
      gap := TfpgMigPlatformDefaults.GetRelatedGapX
    else  // top=0, bottom=2
      gap := TfpgMigPlatformDefaults.GetRelatedGapY;
  end;

  // If still no gap, return nil or zeros with push
  if gap = nil then
  begin
    if hasGap then
    begin
      SetLength(Result, 3);
      Result[0] := 0;
      Result[1] := 0;
      Result[2] := NOT_SET;  // NOT_SET constant should be imported
    end
    else
      SetLength(Result, 0);  // Return empty array (nil equivalent)
    Exit;
  end;

  // Convert BoundSize to [min, pref, max] array
  SetLength(Result, 3);

  // Get min value
  uv := gap.Min;
  if uv <> nil then
    Result[0] := Round(uv.Value)
  else
    Result[0] := NOT_SET;

  // Get preferred value
  uv := gap.Preferred;
  if uv <> nil then
    Result[1] := Round(uv.Value)
  else
    Result[1] := NOT_SET;

  // Get max value
  uv := gap.Max;
  if uv <> nil then
    Result[2] := Round(uv.Value)
  else
    Result[2] := NOT_SET;

  // TODO: Use full UnitValue.GetPixels(ARefSize, AParent, nil) instead of uv.Value
end;

{ Grouping }

function TfpgMigDimConstraint.GetSizeGroup: string;
begin
  Result := FSizeGroup;
end;

procedure TfpgMigDimConstraint.SetSizeGroup(const AGroup: string);
begin
  FSizeGroup := AGroup;
end;

function TfpgMigDimConstraint.GetEndGroup: string;
begin
  Result := FEndGroup;
end;

procedure TfpgMigDimConstraint.SetEndGroup(const AGroup: string);
begin
  FEndGroup := AGroup;
end;

{ Row/column properties }

function TfpgMigDimConstraint.IsFill: Boolean;
begin
  Result := FFill;
end;

procedure TfpgMigDimConstraint.SetFill(AFill: Boolean);
begin
  FFill := AFill;
end;

function TfpgMigDimConstraint.IsNoGrid: Boolean;
begin
  Result := FNoGrid;
end;

procedure TfpgMigDimConstraint.SetNoGrid(ANoGrid: Boolean);
begin
  FNoGrid := ANoGrid;
end;

end.
