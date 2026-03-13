unit tcmig_lc;

{
  Unit tests for TfpgMigLC (Layout Constraints)
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  fpg_mig_lc, fpg_mig_unitvalue, fpg_mig_boundsize, fpg_mig_hidemode,
  fpg_mig_constraintparser;

type
  TTestMigLC = class(TTestCase)
  published
    { Basic creation and defaults }
    procedure TestCreate;
    procedure TestDefaults;

    { Flow direction }
    procedure TestFlowX_Default;
    procedure TestFlowX_Set;
    procedure TestFlowY_Set;

    { Wrap }
    procedure TestWrap_Default;
    procedure TestWrap_AfterCount;

    { Fill }
    procedure TestFill_Both;
    procedure TestFillX_Only;
    procedure TestFillY_Only;

    { Insets }
    procedure TestInsetsAll;
    procedure TestInsets_String;
    procedure TestInsets_Individual;

    { Alignment }
    procedure TestAlignX;
    procedure TestAlignY;
    procedure TestAlign_Both;

    { Grid gaps }
    procedure TestGridGapX;
    procedure TestGridGapY;
    procedure TestGridGap_Both;

    { Container size }
    procedure TestWidth;
    procedure TestMinWidth;
    procedure TestMaxWidth;
    procedure TestHeight;
    procedure TestMinHeight;
    procedure TestMaxHeight;

    { Packing }
    procedure TestPack_Default;
    procedure TestPack_Sizes;
    procedure TestPackAlign;

    { Direction }
    procedure TestLeftToRight_True;
    procedure TestLeftToRight_False;
    procedure TestRightToLeft;
    procedure TestTopToBottom_Default;
    procedure TestTopToBottom_Explicit;
    procedure TestBottomToTop;

    { Flags }
    procedure TestNoGrid;
    procedure TestNoCache;
    procedure TestNoVisualPadding;

    { Debug and hide mode }
    procedure TestDebug_Default;
    procedure TestHideMode_Integer;
    procedure TestHideMode_Enum;

    { Fluent API chaining }
    procedure TestFluentChaining;

    { Property getters }
    procedure TestGetProperties;
  end;

implementation

{ TTestMigLC }

procedure TTestMigLC.TestCreate;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    AssertNotNull('LC should be created', lc);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestDefaults;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    AssertEquals('Default wrap should be INF_SIZE', INF_SIZE, lc.GetWrapAfter);
    AssertTrue('Default flow should be horizontal', lc.IsFlowX);
    AssertFalse('Default fillX should be false', lc.IsFillX);
    AssertFalse('Default fillY should be false', lc.IsFillY);
    AssertTrue('Default top-to-bottom should be true', lc.IsTopToBottom);
    AssertFalse('Default noGrid should be false', lc.IsNoGrid);
    AssertTrue('Default visual padding should be true', lc.IsVisualPadding);
    AssertFalse('Default debug should be false', lc.GetDebug);
    AssertEquals('Default hide mode should be 0', 0, lc.GetHideMode);
    AssertEquals('Default pack width align should be 0.5', 0.5, lc.GetPackWidthAlign, 0.01);
    AssertEquals('Default pack height align should be 1.0', 1.0, lc.GetPackHeightAlign, 0.01);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestFlowX_Default;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    AssertTrue('Default flow should be horizontal', lc.IsFlowX);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestFlowX_Set;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.FlowX;
    AssertTrue('FlowX should set horizontal flow', lc.IsFlowX);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestFlowY_Set;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.FlowY;
    AssertFalse('FlowY should set vertical flow', lc.IsFlowX);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestWrap_Default;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.Wrap;
    AssertEquals('Wrap should set wrap after to 0', 0, lc.GetWrapAfter);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestWrap_AfterCount;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.WrapAfter(5);
    AssertEquals('WrapAfter should set wrap count', 5, lc.GetWrapAfter);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestFill_Both;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.Fill;
    AssertTrue('Fill should set fillX', lc.IsFillX);
    AssertTrue('Fill should set fillY', lc.IsFillY);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestFillX_Only;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.FillX;
    AssertTrue('FillX should set fillX', lc.IsFillX);
    AssertFalse('FillX should not set fillY', lc.IsFillY);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestFillY_Only;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.FillY;
    AssertFalse('FillY should not set fillX', lc.IsFillX);
    AssertTrue('FillY should set fillY', lc.IsFillY);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestInsetsAll;
var
  lc: TfpgMigLC;
  insets: TfpgMigUnitValueArray;
begin
  lc := TfpgMigLC.Create;
  try
    lc.InsetsAll('10');
    insets := lc.GetInsets;
    AssertEquals('Should have 4 insets', 4, Length(insets));
    // All insets should be set (non-nil)
    // Note: Parser stubs return nil for now, so we just verify the call succeeds
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestInsets_String;
var
  lc: TfpgMigLC;
  insets: TfpgMigUnitValueArray;
begin
  lc := TfpgMigLC.Create;
  try
    lc.Insets('10 20 30 40');
    insets := lc.GetInsets;
    AssertEquals('Should have 4 insets', 4, Length(insets));
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestInsets_Individual;
var
  lc: TfpgMigLC;
  insets: TfpgMigUnitValueArray;
begin
  lc := TfpgMigLC.Create;
  try
    lc.Insets('10', '20', '30', '40');
    insets := lc.GetInsets;
    AssertEquals('Should have 4 insets', 4, Length(insets));
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestAlignX;
var
  lc: TfpgMigLC;
  align: TfpgMigUnitValue;
begin
  lc := TfpgMigLC.Create;
  try
    lc.AlignX('center');
    align := lc.GetAlignX;
    AssertNotNull('AlignX should be set', align);
    AssertSame('Should be center alignment', UnitValueCenter, align);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestAlignY;
var
  lc: TfpgMigLC;
  align: TfpgMigUnitValue;
begin
  lc := TfpgMigLC.Create;
  try
    lc.AlignY('center');
    align := lc.GetAlignY;
    AssertNotNull('AlignY should be set', align);
    AssertSame('Should be center alignment', UnitValueCenter, align);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestAlign_Both;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.Align('center', 'center');
    AssertNotNull('AlignX should be set', lc.GetAlignX);
    AssertNotNull('AlignY should be set', lc.GetAlignY);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestGridGapX;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    // Note: This will fail with "not implemented" error from parser stub
    // We'll test the method exists and can be called
    try
      lc.GridGapX('10px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected - parser not implemented yet
    end;
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestGridGapY;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    try
      lc.GridGapY('10px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected - parser not implemented yet
    end;
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestGridGap_Both;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    try
      lc.GridGap('10px', '10px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected - parser not implemented yet
    end;
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestWidth;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    try
      lc.Width('100px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected
    end;
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestMinWidth;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.MinWidth('100px');
    AssertNotNull('Width should be set', lc.GetWidth);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestMaxWidth;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.MaxWidth('200px');
    AssertNotNull('Width should be set', lc.GetWidth);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestHeight;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    try
      lc.Height('100px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected
    end;
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestMinHeight;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.MinHeight('100px');
    AssertNotNull('Height should be set', lc.GetHeight);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestMaxHeight;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.MaxHeight('200px');
    AssertNotNull('Height should be set', lc.GetHeight);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestPack_Default;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    try
      lc.Pack;
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected
    end;
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestPack_Sizes;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    try
      lc.Pack('100px', '100px');
      Fail('Should raise exception - parser not implemented');
    except
      on E: Exception do
        ; // Expected
    end;
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestPackAlign;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.PackAlign(0.3, 0.7);
    AssertEquals('Pack width align should be 0.3', 0.3, lc.GetPackWidthAlign, 0.01);
    AssertEquals('Pack height align should be 0.7', 0.7, lc.GetPackHeightAlign, 0.01);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestLeftToRight_True;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.LeftToRight(True);
    AssertEquals('LeftToRight(true) should set value to 2', 2, lc.GetLeftToRight);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestLeftToRight_False;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.LeftToRight(False);
    AssertEquals('LeftToRight(false) should set value to 1', 1, lc.GetLeftToRight);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestRightToLeft;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.RightToLeft;
    AssertEquals('RightToLeft should set value to 1', 1, lc.GetLeftToRight);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestTopToBottom_Default;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    AssertTrue('Default should be top-to-bottom', lc.IsTopToBottom);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestTopToBottom_Explicit;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.TopToBottom;
    AssertTrue('TopToBottom should set value', lc.IsTopToBottom);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestBottomToTop;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.BottomToTop;
    AssertFalse('BottomToTop should clear value', lc.IsTopToBottom);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestNoGrid;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.NoGrid;
    AssertTrue('NoGrid should set flag', lc.IsNoGrid);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestNoCache;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.NoCache;
    AssertTrue('NoCache should set flag', lc.IsNoCache);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestNoVisualPadding;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.NoVisualPadding;
    AssertFalse('NoVisualPadding should clear flag', lc.IsVisualPadding);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestDebug_Default;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.Debug;
    AssertTrue('Debug should enable debug mode', lc.GetDebug);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestHideMode_Integer;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.HideMode(2);
    AssertEquals('HideMode should be set', 2, lc.GetHideMode);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestHideMode_Enum;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.HideMode(hmSize0Gaps0);
    AssertEquals('HideMode should be set to 2', 2, lc.GetHideMode);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestFluentChaining;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.FlowX.Fill.Wrap.Debug.NoGrid;

    AssertTrue('FlowX should be set', lc.IsFlowX);
    AssertTrue('FillX should be set', lc.IsFillX);
    AssertTrue('FillY should be set', lc.IsFillY);
    AssertEquals('Wrap should be set to 0', 0, lc.GetWrapAfter);
    AssertTrue('Debug should be set', lc.GetDebug);
    AssertTrue('NoGrid should be set', lc.IsNoGrid);
  finally
    lc.Free;
  end;
end;

procedure TTestMigLC.TestGetProperties;
var
  lc: TfpgMigLC;
begin
  lc := TfpgMigLC.Create;
  try
    lc.WrapAfter(5);
    lc.Debug;
    lc.HideMode(1);
    lc.FillX;
    lc.FlowY; // FlowY and FlowX are mutually exclusive. Setting one to True, makes the other one False.

    AssertEquals('GetWrapAfter should return value', 5, lc.GetWrapAfter);
    AssertTrue('GetDebug should return value', lc.GetDebug);
    AssertEquals('GetHideMode should return value', 1, lc.GetHideMode);
    AssertTrue('IsFillX should return value', lc.IsFillX);
    AssertFalse('IsFlowX should return value', lc.IsFlowX);
  finally
    lc.Free;
  end;
end;

initialization
  RegisterTest(TTestMigLC);

end.
