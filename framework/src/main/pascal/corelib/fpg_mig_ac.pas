unit fpg_mig_ac;

{
  MigLayout v11 AC (Axis Constraints) for fpGUI

  Ported from: net.miginfocom.layout.AC.java (v11.4.2)

  Constraint that holds column or row constraints for the grid.
  This class is a holder and builder for a number of DimConstraints.

  Supports fluent API for building constraints:
    AC.Index(0).Fill.Gap.Size('100px').Gap.Fill
}

{$I fpg_defines.inc}

interface

uses
  Classes, SysUtils, Contnrs, fpg_mig_dimconstraint, fpg_mig_boundsize,
  fpg_mig_unitvalue;

type
  { Array of DimConstraints }
  TfpgMigDimConstraintArray = array of TfpgMigDimConstraint;

  { TfpgMigAC - Axis Constraint builder

    Holds constraints for all columns OR all rows.
    Uses fluent API pattern - methods return Self for chaining.
  }
  TfpgMigAC = class
  private
    FConstraints: TObjectList;  // List of TfpgMigDimConstraint
    FCurIndex: Integer;          // Current column/row being configured

    { Ensures constraint list has at least Size elements }
    procedure MakeSize(ASize: Integer);

    { Get constraint at index, creating if needed }
    function GetConstraint(AIndex: Integer): TfpgMigDimConstraint;
  public
    constructor Create;
    destructor Destroy; override;

    { Returns number of constraints (columns/rows) }
    function GetCount: Integer;

    { Sets total number of constraints to Size }
    function Count(ASize: Integer): TfpgMigAC;

    { Sets current index for subsequent operations }
    function Index(AIndex: Integer): TfpgMigAC;

    { Marks current row/column as non-grid (merged cell) }
    function NoGrid: TfpgMigAC; overload;
    function NoGrid(AIndex: Integer): TfpgMigAC; overload;

    { Sets fill property - components grow by default }
    function Fill: TfpgMigAC; overload;
    function Fill(AIndex: Integer): TfpgMigAC; overload;

    { Sets size constraints for current or specified row/column }
    function Size(const ASize: string): TfpgMigAC; overload;
    function Size(const ASize: string; AIndex: Integer): TfpgMigAC; overload;

    { Sets size group name }
    function SizeGroup: TfpgMigAC; overload;  // Empty group name
    function SizeGroup(const AGroup: string): TfpgMigAC; overload;
    function SizeGroup(const AGroup: string; AIndex: Integer): TfpgMigAC; overload;

    { Sets gap after current row/column and moves to next }
    function Gap: TfpgMigAC; overload;  // Default gap, move to next
    function Gap(const AGap: string): TfpgMigAC; overload;  // Specified gap, move to next
    function Gap(const AGap: string; AIndex: Integer): TfpgMigAC; overload;  // Set gap for index

    { Sets alignment for components in row/column }
    function Align(const ASide: string): TfpgMigAC; overload;
    function Align(const ASide: string; AIndex: Integer): TfpgMigAC; overload;

    { Sets grow priority }
    function GrowPrio(APriority: Integer): TfpgMigAC; overload;
    function GrowPrio(APriority: Integer; AIndex: Integer): TfpgMigAC; overload;

    { Sets grow weight }
    function Grow: TfpgMigAC; overload;  // Default weight 100
    function Grow(AWeight: Single): TfpgMigAC; overload;
    function Grow(AWeight: Single; AIndex: Integer): TfpgMigAC; overload;

    { Sets shrink priority }
    function ShrinkPrio(APriority: Integer): TfpgMigAC; overload;
    function ShrinkPrio(APriority: Integer; AIndex: Integer): TfpgMigAC; overload;

    { Sets shrink weight }
    function Shrink: TfpgMigAC; overload;  // Default weight 100
    function Shrink(AWeight: Single): TfpgMigAC; overload;
    function Shrink(AWeight: Single; AIndex: Integer): TfpgMigAC; overload;

    { Gets array of all DimConstraints (read-only) }
    function GetConstraints: TfpgMigDimConstraintArray;

    { Sets constraints from array }
    procedure SetConstraints(AConstraints: TfpgMigDimConstraintArray);
  end;

implementation

uses
  fpg_mig_constraintparser;

{ TfpgMigAC }

constructor TfpgMigAC.Create;
begin
  inherited Create;
  FConstraints := TObjectList.Create(True);  // Owns objects
  FCurIndex := 0;

  // Start with one default constraint
  FConstraints.Add(TfpgMigDimConstraint.Create);
end;

destructor TfpgMigAC.Destroy;
begin
  FConstraints.Free;
  inherited Destroy;
end;

procedure TfpgMigAC.MakeSize(ASize: Integer);
var
  i: Integer;
begin
  if FConstraints.Count <= ASize then
  begin
    FConstraints.Capacity := ASize + 1;
    for i := FConstraints.Count to ASize do
      FConstraints.Add(TfpgMigDimConstraint.Create);
  end;
end;

function TfpgMigAC.GetConstraint(AIndex: Integer): TfpgMigDimConstraint;
begin
  MakeSize(AIndex);
  Result := TfpgMigDimConstraint(FConstraints[AIndex]);
end;

function TfpgMigAC.GetCount: Integer;
begin
  Result := FConstraints.Count;
end;

function TfpgMigAC.Count(ASize: Integer): TfpgMigAC;
begin
  MakeSize(ASize);
  Result := Self;
end;

function TfpgMigAC.Index(AIndex: Integer): TfpgMigAC;
begin
  MakeSize(AIndex);
  FCurIndex := AIndex;
  Result := Self;
end;

function TfpgMigAC.NoGrid: TfpgMigAC;
begin
  Result := NoGrid(FCurIndex);
end;

function TfpgMigAC.NoGrid(AIndex: Integer): TfpgMigAC;
begin
  GetConstraint(AIndex).SetNoGrid(True);
  Result := Self;
end;

function TfpgMigAC.Fill: TfpgMigAC;
begin
  Result := Fill(FCurIndex);
end;

function TfpgMigAC.Fill(AIndex: Integer): TfpgMigAC;
begin
  GetConstraint(AIndex).SetFill(True);
  Result := Self;
end;

function TfpgMigAC.Size(const ASize: string): TfpgMigAC;
begin
  Result := Size(ASize, FCurIndex);
end;

function TfpgMigAC.Size(const ASize: string; AIndex: Integer): TfpgMigAC;
var
  bs: TfpgMigBoundSize;
begin
  bs := ParseBoundSize(ASize, False, True);
  GetConstraint(AIndex).SetSize(bs);
  Result := Self;
end;

function TfpgMigAC.SizeGroup: TfpgMigAC;
begin
  Result := SizeGroup('', FCurIndex);
end;

function TfpgMigAC.SizeGroup(const AGroup: string): TfpgMigAC;
begin
  Result := SizeGroup(AGroup, FCurIndex);
end;

function TfpgMigAC.SizeGroup(const AGroup: string; AIndex: Integer): TfpgMigAC;
begin
  GetConstraint(AIndex).SetSizeGroup(AGroup);
  Result := Self;
end;

function TfpgMigAC.Gap: TfpgMigAC;
begin
  Inc(FCurIndex);
  MakeSize(FCurIndex);
  Result := Self;
end;

function TfpgMigAC.Gap(const AGap: string): TfpgMigAC;
begin
  Result := Gap(AGap, FCurIndex);
  Inc(FCurIndex);
end;

function TfpgMigAC.Gap(const AGap: string; AIndex: Integer): TfpgMigAC;
var
  bs: TfpgMigBoundSize;
begin
  MakeSize(AIndex + 1);
  if AGap <> '' then
  begin
    bs := ParseBoundSize(AGap, True, True);
    GetConstraint(AIndex).SetGapAfter(bs);
  end;
  Result := Self;
end;

function TfpgMigAC.Align(const ASide: string): TfpgMigAC;
begin
  Result := Align(ASide, FCurIndex);
end;

function TfpgMigAC.Align(const ASide: string; AIndex: Integer): TfpgMigAC;
var
  al: TfpgMigUnitValue;
begin
  al := ParseAlignKeywords(ASide, True);
  if al = nil then
    al := ParseAlignKeywords(ASide, False);

  GetConstraint(AIndex).SetAlign(al);
  Result := Self;
end;

function TfpgMigAC.GrowPrio(APriority: Integer): TfpgMigAC;
begin
  Result := GrowPrio(APriority, FCurIndex);
end;

function TfpgMigAC.GrowPrio(APriority: Integer; AIndex: Integer): TfpgMigAC;
begin
  GetConstraint(AIndex).SetGrowPriority(APriority);
  Result := Self;
end;

function TfpgMigAC.Grow: TfpgMigAC;
begin
  Result := Grow(100.0, FCurIndex);
end;

function TfpgMigAC.Grow(AWeight: Single): TfpgMigAC;
begin
  Result := Grow(AWeight, FCurIndex);
end;

function TfpgMigAC.Grow(AWeight: Single; AIndex: Integer): TfpgMigAC;
begin
  GetConstraint(AIndex).SetGrowWeight(AWeight);
  Result := Self;
end;

function TfpgMigAC.ShrinkPrio(APriority: Integer): TfpgMigAC;
begin
  Result := ShrinkPrio(APriority, FCurIndex);
end;

function TfpgMigAC.ShrinkPrio(APriority: Integer; AIndex: Integer): TfpgMigAC;
begin
  GetConstraint(AIndex).SetShrinkPriority(APriority);
  Result := Self;
end;

function TfpgMigAC.Shrink: TfpgMigAC;
begin
  Result := Shrink(100.0, FCurIndex);
end;

function TfpgMigAC.Shrink(AWeight: Single): TfpgMigAC;
begin
  Result := Shrink(AWeight, FCurIndex);
end;

function TfpgMigAC.Shrink(AWeight: Single; AIndex: Integer): TfpgMigAC;
begin
  GetConstraint(AIndex).SetShrinkWeight(AWeight);
  Result := Self;
end;

function TfpgMigAC.GetConstraints: TfpgMigDimConstraintArray;
var
  i: Integer;
begin
  SetLength(Result, FConstraints.Count);
  for i := 0 to FConstraints.Count - 1 do
    Result[i] := TfpgMigDimConstraint(FConstraints[i]);
end;

procedure TfpgMigAC.SetConstraints(AConstraints: TfpgMigDimConstraintArray);
var
  i: Integer;
begin
  FConstraints.Clear;

  if (AConstraints = nil) or (Length(AConstraints) = 0) then
  begin
    // Reset to default
    FConstraints.Add(TfpgMigDimConstraint.Create);
  end
  else
  begin
    FConstraints.Capacity := Length(AConstraints);
    for i := 0 to High(AConstraints) do
      FConstraints.Add(AConstraints[i]);
  end;
end;

end.
