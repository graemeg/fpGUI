unit fpg_mig_constraintparser;

{
  MigLayout v11 ConstraintParser for fpGUI

  Ported from: net.miginfocom.layout.ConstraintParser.java (v11.4.2)

  Parses constraint strings into constraint objects.
  This is a minimal stub implementation for Phase 2, Step 2.2.
  Full parsing will be implemented in a later phase.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_mig_boundsize, fpg_mig_unitvalue;

{ Parse a BoundSize from a string like "100px" or "50:100:200" }
function ParseBoundSize(const AStr: string; AIsGap, AIsGapPushable: Boolean): TfpgMigBoundSize;

{ Parse alignment keywords like "left", "right", "top", "bottom", "center", etc. }
function ParseAlignKeywords(const AStr: string; AIsHorizontal: Boolean): TfpgMigUnitValue;

implementation

function ParseBoundSize(const AStr: string; AIsGap, AIsGapPushable: Boolean): TfpgMigBoundSize;
begin
  // TODO: Implement full parsing in later phase
  // For now, return a simple fixed size
  raise Exception.Create('ParseBoundSize not yet implemented - needed for AC constraint parsing');
end;

function ParseAlignKeywords(const AStr: string; AIsHorizontal: Boolean): TfpgMigUnitValue;
var
  s: string;
begin
  s := LowerCase(AStr);

  // Parse horizontal alignment keywords
  if AIsHorizontal then
  begin
    if (s = 'left') or (s = 'leading') then
      Exit(UnitValueLeading);
    if (s = 'right') or (s = 'trailing') then
      Exit(UnitValueLeading);  // TODO: Should be trailing
    if s = 'center' then
      Exit(UnitValueCenter);
  end
  else
  begin
    // Parse vertical alignment keywords
    if s = 'top' then
      Exit(UnitValueLeading);
    if s = 'bottom' then
      Exit(UnitValueLeading);  // TODO: Should be trailing
    if s = 'center' then
      Exit(UnitValueCenter);
    if (s = 'baseline') or (s = 'base') then
      Exit(UnitValueBaselineIdentity);
  end;

  // Return nil if not recognized
  Result := nil;
end;

end.
