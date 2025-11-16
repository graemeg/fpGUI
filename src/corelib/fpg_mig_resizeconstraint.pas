unit fpg_mig_resizeconstraint;

{$I fpg_defines.inc}

interface

uses
  Classes, SysUtils;

type
  TfpgMigResizeConstraint = class
  public
    Grow: Single;       // NaN means null
    GrowPrio: Integer;
    Shrink: Single;     // NaN means null
    ShrinkPrio: Integer;

    constructor Create; overload;
    constructor Create(AShrinkPrio: Integer; AShrinkWeight: Single;
                       AGrowPrio: Integer; AGrowWeight: Single); overload;
  end;

implementation

uses
  Math; // For IsNaN

{ TfpgMigResizeConstraint }

constructor TfpgMigResizeConstraint.Create;
begin
  inherited Create;
  Grow := NaN;
  GrowPrio := 100;
  Shrink := 100.0;
  ShrinkPrio := 100;
end;

constructor TfpgMigResizeConstraint.Create(AShrinkPrio: Integer;
  AShrinkWeight: Single; AGrowPrio: Integer; AGrowWeight: Single);
begin
  inherited Create;
  ShrinkPrio := AShrinkPrio;
  Shrink := AShrinkWeight;
  GrowPrio := AGrowPrio;
  Grow := AGrowWeight;
end;

end.
