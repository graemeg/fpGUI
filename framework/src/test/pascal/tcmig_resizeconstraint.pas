unit tcmig_resizeconstraint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Math,
  fpg_mig_resizeconstraint;

type
  TTestMigResizeConstraint = class(TTestCase)
  published
    procedure TestDefaultConstructor;
    procedure TestParameterizedConstructor;
  end;

implementation

{ TTestMigResizeConstraint }

procedure TTestMigResizeConstraint.TestDefaultConstructor;
var
  rc: TfpgMigResizeConstraint;
begin
  rc := TfpgMigResizeConstraint.Create;
  try
    AssertTrue('Default Grow should be NaN', IsNaN(rc.Grow));
    AssertEquals('Default GrowPrio should be 100', 100, rc.GrowPrio);
    AssertEquals('Default Shrink should be 100.0', 100.0, rc.Shrink, 0.001);
    AssertEquals('Default ShrinkPrio should be 100', 100, rc.ShrinkPrio);
  finally
    rc.Free;
  end;
end;

procedure TTestMigResizeConstraint.TestParameterizedConstructor;
var
  rc: TfpgMigResizeConstraint;
  nanFloat: Single;
begin
  nanFloat := NaN;
  rc := TfpgMigResizeConstraint.Create(50, 50.0, 150, nanFloat);
  try
    AssertTrue('Grow should be NaN', IsNaN(rc.Grow));
    AssertEquals('GrowPrio should be 150', 150, rc.GrowPrio);
    AssertEquals('Shrink should be 50.0', 50.0, rc.Shrink, 0.001);
    AssertEquals('ShrinkPrio should be 50', 50, rc.ShrinkPrio);
  finally
    rc.Free;
  end;
end;

initialization
  RegisterTest(TTestMigResizeConstraint);

end.
