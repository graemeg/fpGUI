unit tcmig_linkhandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  fpg_mig_linkhandler;

type
  TTestMigLinkHandler = class(TTestCase)
  private
    FLayout1: TObject;
    FLayout2: TObject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetAndGetBounds;
    procedure TestMultipleLayouts;
    procedure TestClearBounds;
    procedure TestClearTemporaryBounds;
    procedure TestExpandBounds;
    procedure TestGetValueTypes;
  end;

implementation

{ TTestMigLinkHandler }

procedure TTestMigLinkHandler.SetUp;
begin
  FLayout1 := TObject.Create;
  FLayout2 := TObject.Create;
  TfpgMigLinkHandler.ClearAll;
end;

procedure TTestMigLinkHandler.TearDown;
begin
  TfpgMigLinkHandler.ClearAll;
  FLayout1.Free;
  FLayout2.Free;
end;

procedure TTestMigLinkHandler.TestSetAndGetBounds;
var
  changed: Boolean;
  val: PInteger;
begin
  // Set bounds for a component
  changed := TfpgMigLinkHandler.SetBounds(FLayout1, 'comp1', 10, 20, 100, 50);
  AssertTrue('First SetBounds should return true', changed);

  // Get X value
  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_X);
  AssertNotNull('X value should not be nil', val);
  AssertEquals('X value should be 10', 10, val^);
  Dispose(val);

  // Get Y value
  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_Y);
  AssertNotNull('Y value should not be nil', val);
  AssertEquals('Y value should be 20', 20, val^);
  Dispose(val);

  // Get Width value
  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_WIDTH);
  AssertNotNull('Width value should not be nil', val);
  AssertEquals('Width value should be 100', 100, val^);
  Dispose(val);

  // Get Height value
  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_HEIGHT);
  AssertNotNull('Height value should not be nil', val);
  AssertEquals('Height value should be 50', 50, val^);
  Dispose(val);

  // Get X2 value (calculated)
  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_X2);
  AssertNotNull('X2 value should not be nil', val);
  AssertEquals('X2 value should be 110 (10+100)', 110, val^);
  Dispose(val);

  // Get Y2 value (calculated)
  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_Y2);
  AssertNotNull('Y2 value should not be nil', val);
  AssertEquals('Y2 value should be 70 (20+50)', 70, val^);
  Dispose(val);
end;

procedure TTestMigLinkHandler.TestMultipleLayouts;
var
  val: PInteger;
begin
  // Set bounds in different layouts
  TfpgMigLinkHandler.SetBounds(FLayout1, 'comp1', 10, 20, 100, 50);
  TfpgMigLinkHandler.SetBounds(FLayout2, 'comp1', 30, 40, 200, 80);

  // Verify layout1 values
  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_X);
  AssertEquals('Layout1 X should be 10', 10, val^);
  Dispose(val);

  // Verify layout2 values
  val := TfpgMigLinkHandler.GetValue(FLayout2, 'comp1', LINK_X);
  AssertEquals('Layout2 X should be 30', 30, val^);
  Dispose(val);

  val := TfpgMigLinkHandler.GetValue(FLayout2, 'comp1', LINK_WIDTH);
  AssertEquals('Layout2 Width should be 200', 200, val^);
  Dispose(val);
end;

procedure TTestMigLinkHandler.TestClearBounds;
var
  cleared: Boolean;
  val: PInteger;
begin
  // Set then clear
  TfpgMigLinkHandler.SetBounds(FLayout1, 'comp1', 10, 20, 100, 50);
  cleared := TfpgMigLinkHandler.ClearBounds(FLayout1, 'comp1');
  AssertTrue('ClearBounds should return true when clearing existing bounds', cleared);

  // Verify it's cleared
  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_X);
  AssertNull('Value should be nil after clearing', val);

  // Clear non-existent should return false
  cleared := TfpgMigLinkHandler.ClearBounds(FLayout1, 'nonexistent');
  AssertFalse('ClearBounds should return false for non-existent key', cleared);
end;

procedure TTestMigLinkHandler.TestClearTemporaryBounds;
var
  val: PInteger;
begin
  // Set permanent bounds
  TfpgMigLinkHandler.SetBounds(FLayout1, 'comp1', 10, 20, 100, 50, False, False);

  // Set temporary bounds
  TfpgMigLinkHandler.SetBounds(FLayout1, 'comp2', 30, 40, 200, 80, True, False);

  // Clear temporary
  TfpgMigLinkHandler.ClearTemporaryBounds(FLayout1);

  // Permanent should still exist
  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_X);
  AssertNotNull('Permanent bounds should still exist', val);
  AssertEquals('Permanent X should be 10', 10, val^);
  Dispose(val);

  // Temporary should be gone
  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp2', LINK_X);
  AssertNull('Temporary bounds should be cleared', val);
end;

procedure TTestMigLinkHandler.TestExpandBounds;
var
  changed: Boolean;
  val: PInteger;
begin
  // Set initial bounds
  TfpgMigLinkHandler.SetBounds(FLayout1, 'comp1', 10, 20, 100, 50);

  // Expand bounds (AIncCur = True)
  changed := TfpgMigLinkHandler.SetBounds(FLayout1, 'comp1', 5, 15, 110, 60, False, True);
  AssertTrue('Expanding bounds should return true', changed);

  // X should be minimum (5 < 10)
  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_X);
  AssertEquals('X should be expanded to 5', 5, val^);
  Dispose(val);

  // Width should expand to cover both ranges
  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_WIDTH);
  AssertNotNull('Width should not be nil', val);
  Dispose(val);
end;

procedure TTestMigLinkHandler.TestGetValueTypes;
var
  val: PInteger;
begin
  TfpgMigLinkHandler.SetBounds(FLayout1, 'comp1', 100, 200, 300, 400);

  // Test all value types
  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_X);
  AssertEquals('LINK_X', 100, val^);
  Dispose(val);

  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_Y);
  AssertEquals('LINK_Y', 200, val^);
  Dispose(val);

  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_WIDTH);
  AssertEquals('LINK_WIDTH', 300, val^);
  Dispose(val);

  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_HEIGHT);
  AssertEquals('LINK_HEIGHT', 400, val^);
  Dispose(val);

  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_X2);
  AssertEquals('LINK_X2', 400, val^);  // 100 + 300
  Dispose(val);

  val := TfpgMigLinkHandler.GetValue(FLayout1, 'comp1', LINK_Y2);
  AssertEquals('LINK_Y2', 600, val^);  // 200 + 400
  Dispose(val);
end;

initialization
  RegisterTest(TTestMigLinkHandler);

end.
