unit tcspinedit;

{$mode objfpc}{$H+}

{ Regression tests for TfpgSpinEdit / TfpgSpinEditFloat value handling when
  edit focus is lost or Enter is pressed (GitHub issue #149).

  Bugs in the original EditExit behaviour:
  - Empty text set the value to 0 even when 0 was outside MinValue..MaxValue.
  - Out-of-range text silently reverted to the previous value instead of
    clamping to the nearest bound (inconsistent with the Up/Down key
    behaviour, which does clamp).
  - A lone '-' in the edit raised EConvertError via StrToInt.
  - OnChange never fired from EditExit, even when the value changed.
}

interface

uses
  fpcunit, testregistry,
  fpg_spinedit, fpg_edit;

type
  TTestSpinEditExit = class(TTestCase)
  private
    FSpin: TfpgSpinEdit;
    FChangeCount: integer;
    procedure ChangeHandler(Sender: TObject);
    function InnerEdit: TfpgEditInteger;
    procedure TypeTextAndExit(const AText: string);
    procedure TypeTextAndPressEnter(const AText: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { In-range typed text is accepted as the new value. }
    procedure TestInRangeValueAccepted;
    { Empty text must clamp into the range, not blindly become 0. }
    procedure TestEmptyTextClampsIntoRange;
    { Empty text becomes 0 when 0 lies within the range. }
    procedure TestEmptyTextBecomesZeroWhenZeroInRange;
    { Text above MaxValue clamps to MaxValue instead of reverting. }
    procedure TestValueAboveMaxClampsToMax;
    { Text below MinValue clamps to MinValue instead of reverting. }
    procedure TestValueBelowMinClampsToMin;
    { Empty text in a negative-only range clamps to the nearest bound. }
    procedure TestEmptyTextNegativeOnlyRangeClampsToMax;
    { OnChange fires once when the value is clamped to a bound. }
    procedure TestOnChangeFiresWhenValueClamped;
    { OnChange fires when an in-range edit changes the value. }
    procedure TestOnChangeFiresWhenInRangeValueChanges;
    { OnChange must not fire when the value did not change. }
    procedure TestNoOnChangeWhenValueUnchanged;
    { A lone minus sign must not raise EConvertError. }
    procedure TestLoneMinusSignDoesNotRaise;
    { The Enter key path must clamp the same way as focus loss. }
    procedure TestEnterKeyClampsToMax;
  end;

  TTestSpinEditFloatExit = class(TTestCase)
  private
    FSpin: TfpgSpinEditFloat;
    FChangeCount: integer;
    procedure ChangeHandler(Sender: TObject);
    function InnerEdit: TfpgEditFloat;
    procedure TypeTextAndExit(const AText: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { In-range typed text is accepted as the new value. }
    procedure TestInRangeValueAccepted;
    { Empty text must clamp into the range, not blindly become 0. }
    procedure TestEmptyTextClampsIntoRange;
    { Text above MaxValue clamps to MaxValue instead of reverting. }
    procedure TestValueAboveMaxClampsToMax;
    { Text below MinValue clamps to MinValue instead of reverting. }
    procedure TestValueBelowMinClampsToMin;
    { OnChange fires once when the value is clamped to a bound. }
    procedure TestOnChangeFiresWhenValueClamped;
  end;

implementation

uses
  Classes, fpg_base, fpg_main;

type
  { grants test access to the protected EditExit/EditKeyPress handlers }
  TSpinEditAccess = class(TfpgSpinEdit);
  TSpinEditFloatAccess = class(TfpgSpinEditFloat);


{ TTestSpinEditExit }

procedure TTestSpinEditExit.SetUp;
begin
  if not fpgApplication.IsInitialized then
    fpgApplication.Initialize;
  FSpin := TfpgSpinEdit.Create(nil);
  FChangeCount := 0;
end;

procedure TTestSpinEditExit.TearDown;
begin
  FSpin.Free;
end;

procedure TTestSpinEditExit.ChangeHandler(Sender: TObject);
begin
  Inc(FChangeCount);
end;

function TTestSpinEditExit.InnerEdit: TfpgEditInteger;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to FSpin.ComponentCount-1 do
    if FSpin.Components[i] is TfpgEditInteger then
    begin
      Result := TfpgEditInteger(FSpin.Components[i]);
      Exit;
    end;
  Fail('TfpgSpinEdit has no internal TfpgEditInteger');
end;

procedure TTestSpinEditExit.TypeTextAndExit(const AText: string);
begin
  InnerEdit.Text := AText;
  TSpinEditAccess(FSpin).EditExit(nil);
end;

procedure TTestSpinEditExit.TypeTextAndPressEnter(const AText: string);
var
  keycode: word;
  shift: TShiftState;
  consumed: Boolean;
begin
  InnerEdit.Text := AText;
  keycode := keyReturn;
  shift := [];
  consumed := False;
  TSpinEditAccess(FSpin).EditKeyPress(nil, keycode, shift, consumed);
end;

procedure TTestSpinEditExit.TestInRangeValueAccepted;
begin
  TypeTextAndExit('42');
  AssertEquals('value', 42, FSpin.Value);
end;

procedure TTestSpinEditExit.TestEmptyTextClampsIntoRange;
begin
  FSpin.MinValue := 10;
  TypeTextAndExit('');
  AssertEquals('value', 10, FSpin.Value);
  AssertEquals('edit text', '10', InnerEdit.Text);
end;

procedure TTestSpinEditExit.TestEmptyTextBecomesZeroWhenZeroInRange;
begin
  FSpin.Value := 50;
  TypeTextAndExit('');
  AssertEquals('value', 0, FSpin.Value);
end;

procedure TTestSpinEditExit.TestValueAboveMaxClampsToMax;
begin
  FSpin.Value := 50;
  TypeTextAndExit('150');
  AssertEquals('value', 100, FSpin.Value);
  AssertEquals('edit text', '100', InnerEdit.Text);
end;

procedure TTestSpinEditExit.TestValueBelowMinClampsToMin;
begin
  FSpin.MinValue := 10;
  FSpin.Value := 50;
  TypeTextAndExit('5');
  AssertEquals('value', 10, FSpin.Value);
end;

procedure TTestSpinEditExit.TestEmptyTextNegativeOnlyRangeClampsToMax;
begin
  FSpin.MinValue := -20;
  FSpin.MaxValue := -5;
  TypeTextAndExit('');
  AssertEquals('value', -5, FSpin.Value);
end;

procedure TTestSpinEditExit.TestOnChangeFiresWhenValueClamped;
begin
  FSpin.Value := 50;
  FSpin.OnChange := @ChangeHandler;
  TypeTextAndExit('150');
  AssertEquals('change count', 1, FChangeCount);
end;

procedure TTestSpinEditExit.TestOnChangeFiresWhenInRangeValueChanges;
begin
  FSpin.Value := 50;
  FSpin.OnChange := @ChangeHandler;
  TypeTextAndExit('60');
  AssertEquals('change count', 1, FChangeCount);
end;

procedure TTestSpinEditExit.TestNoOnChangeWhenValueUnchanged;
begin
  FSpin.Value := 42;
  FSpin.OnChange := @ChangeHandler;
  TypeTextAndExit('42');
  AssertEquals('change count', 0, FChangeCount);
  AssertEquals('value', 42, FSpin.Value);
end;

procedure TTestSpinEditExit.TestLoneMinusSignDoesNotRaise;
begin
  FSpin.Value := 50;
  TypeTextAndExit('-');
  { a lone '-' parses as 0, which lies inside the default 0..100 range }
  AssertEquals('value', 0, FSpin.Value);
end;

procedure TTestSpinEditExit.TestEnterKeyClampsToMax;
begin
  FSpin.Value := 50;
  TypeTextAndPressEnter('150');
  AssertEquals('value', 100, FSpin.Value);
end;


{ TTestSpinEditFloatExit }

procedure TTestSpinEditFloatExit.SetUp;
begin
  if not fpgApplication.IsInitialized then
    fpgApplication.Initialize;
  FSpin := TfpgSpinEditFloat.Create(nil);
  FSpin.MaxValue := 100;
  FChangeCount := 0;
end;

procedure TTestSpinEditFloatExit.TearDown;
begin
  FSpin.Free;
end;

procedure TTestSpinEditFloatExit.ChangeHandler(Sender: TObject);
begin
  Inc(FChangeCount);
end;

function TTestSpinEditFloatExit.InnerEdit: TfpgEditFloat;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to FSpin.ComponentCount-1 do
    if FSpin.Components[i] is TfpgEditFloat then
    begin
      Result := TfpgEditFloat(FSpin.Components[i]);
      Exit;
    end;
  Fail('TfpgSpinEditFloat has no internal TfpgEditFloat');
end;

procedure TTestSpinEditFloatExit.TypeTextAndExit(const AText: string);
begin
  InnerEdit.Text := AText;
  TSpinEditFloatAccess(FSpin).EditExit(nil);
end;

procedure TTestSpinEditFloatExit.TestInRangeValueAccepted;
begin
  TypeTextAndExit('42');
  AssertEquals('value', 42.0, FSpin.Value, 0.001);
end;

procedure TTestSpinEditFloatExit.TestEmptyTextClampsIntoRange;
begin
  FSpin.MinValue := 10;
  TypeTextAndExit('');
  AssertEquals('value', 10.0, FSpin.Value, 0.001);
end;

procedure TTestSpinEditFloatExit.TestValueAboveMaxClampsToMax;
begin
  FSpin.Value := 50;
  TypeTextAndExit('150');
  AssertEquals('value', 100.0, FSpin.Value, 0.001);
end;

procedure TTestSpinEditFloatExit.TestValueBelowMinClampsToMin;
begin
  FSpin.MinValue := 10;
  FSpin.Value := 50;
  TypeTextAndExit('5');
  AssertEquals('value', 10.0, FSpin.Value, 0.001);
end;

procedure TTestSpinEditFloatExit.TestOnChangeFiresWhenValueClamped;
begin
  FSpin.Value := 50;
  FSpin.OnChange := @ChangeHandler;
  TypeTextAndExit('150');
  AssertEquals('change count', 1, FChangeCount);
end;


initialization
  RegisterTest(TTestSpinEditExit);
  RegisterTest(TTestSpinEditFloatExit);

end.
