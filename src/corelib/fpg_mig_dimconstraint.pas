unit fpg_mig_dimconstraint;

{
  MigLayout v11 DimConstraint for fpGUI

  Ported from: net.miginfocom.layout.DimConstraint.java (v11.4.2)

  Constraints for a single dimension (row or column).
  Handles size, alignment, gaps, grow/shrink priorities and weights.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_mig_unitvalue, fpg_mig_boundsize;

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
  end;

implementation

uses
  fpg_mig_platformdefaults;

{ TfpgMigDimConstraint }

constructor TfpgMigDimConstraint.Create;
begin
  inherited Create;

  { Initialize resize properties }
  FGrowPriority := DEFAULT_PRIORITY;
  FGrowWeight := 0.0;
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
  { Note: We don't free FSize, FAlign, FGapBefore, FGapAfter
    as we don't own them - they are either references or managed elsewhere }
  inherited Destroy;
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
  FGrowWeight := 0.0;
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
  FSize := ASize;
end;

{ Gaps }

function TfpgMigDimConstraint.GetGapBefore: TfpgMigBoundSize;
begin
  Result := FGapBefore;
end;

procedure TfpgMigDimConstraint.SetGapBefore(AGap: TfpgMigBoundSize);
begin
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
