{
  Tests for the &-prefixed accelerator (mnemonic) support.

  Covers the pure parsing helpers in fpg_stringutils, and the Alt-exclusivity
  rule used when dispatching accelerators, so that Ctrl+Alt+S or Shift+Alt+S
  never trigger an Alt+S accelerator.
}
unit tcaccelerator;

{$mode objfpc}{$h+}

interface

uses
  Classes,
  SysUtils,
  fpcunit, testutils, testregistry,
  fpg_base,
  fpg_widget,
  fpg_form,
  fpg_button,
  fpg_checkbox,
  fpg_panel,
  fpg_stringutils;

type

  TTestAccelParsing = class(TTestCase)
  published
    procedure TestNoAccelReturnsEmpty;
    procedure TestSimpleAccel;
    procedure TestAccelNotFirstChar;
    procedure TestEscapedAmpersandIsNotAnAccel;
    procedure TestEscapedAmpersandFollowedByRealAccel;
    procedure TestTrailingAmpersandIsIgnored;
    procedure TestAccelIsCaseInsensitiveOnMatch;
    procedure TestUTF8Accel;
    procedure TestStripNoAccel;
    procedure TestStripSimpleAccel;
    procedure TestStripEscapedAmpersand;
    procedure TestStripEscapedAndRealAccel;
    procedure TestStripTrailingAmpersand;
    procedure TestStripUTF8;
    procedure TestAccelPosNone;
    procedure TestAccelPosFirst;
    procedure TestAccelPosMidWord;
    procedure TestAccelPosAfterEscapedAmpersand;
    procedure TestAccelPosUTF8;
  end;

  TTestAccelShiftState = class(TTestCase)
  published
    procedure TestPlainAltMatches;
    procedure TestCtrlAltDoesNotMatch;
    procedure TestShiftAltDoesNotMatch;
    procedure TestCtrlShiftAltDoesNotMatch;
    procedure TestNoModifierDoesNotMatch;
    procedure TestCtrlAloneDoesNotMatch;
    procedure TestAltPlusUnrelatedModifierStillMatches;
  end;

  { End-to-end dispatch through TfpgWidget.DoKeyShortcut }
  TTestAccelDispatch = class(TTestCase)
  private
    FForm: TfpgForm;
    FButton: TfpgButton;
    FClickCount: integer;
    procedure   ButtonClicked(Sender: TObject);
    { drives the same entry point the real key handling uses }
    function    SendKey(const AKey: word; const AShiftState: TShiftState): boolean;
  protected
    procedure   SetUp; override;
    procedure   TearDown; override;
  published
    procedure TestAltAccelFiresButton;
    procedure TestAltAccelIsCaseInsensitive;
    procedure TestCtrlAltDoesNotFireButton;
    procedure TestShiftAltDoesNotFireButton;
    procedure TestNoModifierDoesNotFireButton;
    procedure TestWrongLetterDoesNotFire;
    procedure TestDisabledButtonDoesNotFire;
    procedure TestButtonWithoutAccelDoesNotFire;
    procedure TestNestedButtonInPanelFires;
    procedure TestCheckBoxAccelToggles;
  end;


procedure RegisterTests;


implementation

uses
  fpg_main;


procedure RegisterTests;
begin
  RegisterTest(TTestAccelParsing);
  RegisterTest(TTestAccelShiftState);
  RegisterTest(TTestAccelDispatch);
end;

type
  { DoKeyShortcut is protected, so reach it the usual friend-class way }
  TfpgWidgetFriend = class(TfpgWidget);


{ TTestAccelParsing }

procedure TTestAccelParsing.TestNoAccelReturnsEmpty;
begin
  CheckEquals('', fpgExtractAccelChar('Scan'));
end;

procedure TTestAccelParsing.TestSimpleAccel;
begin
  CheckEquals('S', fpgExtractAccelChar('&Scan'));
end;

procedure TTestAccelParsing.TestAccelNotFirstChar;
begin
  { the marked character is returned verbatim, case included }
  CheckEquals('c', fpgExtractAccelChar('S&can'));
end;

procedure TTestAccelParsing.TestEscapedAmpersandIsNotAnAccel;
begin
  { '&&' is a literal ampersand, so there is no accelerator here }
  CheckEquals('', fpgExtractAccelChar('Black && White'));
end;

procedure TTestAccelParsing.TestEscapedAmpersandFollowedByRealAccel;
begin
  CheckEquals('W', fpgExtractAccelChar('Black && &White'));
end;

procedure TTestAccelParsing.TestTrailingAmpersandIsIgnored;
begin
  { a lone trailing '&' has no character after it }
  CheckEquals('', fpgExtractAccelChar('Scan&'));
end;

procedure TTestAccelParsing.TestAccelIsCaseInsensitiveOnMatch;
begin
  { the helper returns the char as-is; matching is done case-insensitively
    by the caller, so a lowercase accel must be preserved verbatim here }
  CheckEquals('s', fpgExtractAccelChar('&scan'));
end;

procedure TTestAccelParsing.TestUTF8Accel;
begin
  { a multi-byte accelerator character must come back whole }
  CheckEquals('ü', fpgExtractAccelChar('&überprüfen'));
end;

procedure TTestAccelParsing.TestStripNoAccel;
begin
  CheckEquals('Scan', fpgStripAccelChars('Scan'));
end;

procedure TTestAccelParsing.TestStripSimpleAccel;
begin
  CheckEquals('Scan', fpgStripAccelChars('&Scan'));
end;

procedure TTestAccelParsing.TestStripEscapedAmpersand;
begin
  CheckEquals('Black & White', fpgStripAccelChars('Black && White'));
end;

procedure TTestAccelParsing.TestStripEscapedAndRealAccel;
begin
  CheckEquals('Black & White', fpgStripAccelChars('Black && &White'));
end;

procedure TTestAccelParsing.TestStripTrailingAmpersand;
begin
  CheckEquals('Scan', fpgStripAccelChars('Scan&'));
end;

procedure TTestAccelParsing.TestStripUTF8;
begin
  CheckEquals('überprüfen', fpgStripAccelChars('&überprüfen'));
end;

procedure TTestAccelParsing.TestAccelPosNone;
begin
  CheckEquals(0, fpgAccelCharPos('Scan'));
end;

procedure TTestAccelParsing.TestAccelPosFirst;
begin
  CheckEquals(1, fpgAccelCharPos('&Scan'));
end;

procedure TTestAccelParsing.TestAccelPosMidWord;
begin
  { 'S&can' strips to 'Scan', where 'c' is the 2nd character }
  CheckEquals(2, fpgAccelCharPos('S&can'));
end;

procedure TTestAccelParsing.TestAccelPosAfterEscapedAmpersand;
begin
  { 'Black && &White' strips to 'Black & White', where 'W' is the 9th char }
  CheckEquals(9, fpgAccelCharPos('Black && &White'));
end;

procedure TTestAccelParsing.TestAccelPosUTF8;
begin
  { position is in characters, not bytes - 'S&ürnamè' strips to 'Sürnamè' }
  CheckEquals(2, fpgAccelCharPos('S&ürnamè'));
end;


{ TTestAccelShiftState }

procedure TTestAccelShiftState.TestPlainAltMatches;
begin
  CheckTrue(fpgIsAccelShiftState([ssAlt]));
end;

procedure TTestAccelShiftState.TestCtrlAltDoesNotMatch;
begin
  { Ctrl+Alt+S must never fire an Alt+S accelerator }
  CheckFalse(fpgIsAccelShiftState([ssCtrl, ssAlt]));
end;

procedure TTestAccelShiftState.TestShiftAltDoesNotMatch;
begin
  CheckFalse(fpgIsAccelShiftState([ssShift, ssAlt]));
end;

procedure TTestAccelShiftState.TestCtrlShiftAltDoesNotMatch;
begin
  CheckFalse(fpgIsAccelShiftState([ssCtrl, ssShift, ssAlt]));
end;

procedure TTestAccelShiftState.TestNoModifierDoesNotMatch;
begin
  CheckFalse(fpgIsAccelShiftState([]));
end;

procedure TTestAccelShiftState.TestCtrlAloneDoesNotMatch;
begin
  CheckFalse(fpgIsAccelShiftState([ssCtrl]));
end;

procedure TTestAccelShiftState.TestAltPlusUnrelatedModifierStillMatches;
begin
  { only Shift/Alt/Ctrl take part in the test - a stray mouse-button state
    in the set must not stop a legitimate accelerator from firing }
  CheckTrue(fpgIsAccelShiftState([ssAlt, ssLeft]));
end;


{ TTestAccelDispatch }

procedure TTestAccelDispatch.SetUp;
begin
  if not fpgApplication.IsInitialized then
    fpgApplication.Initialize;
  FClickCount := 0;
  FForm := TfpgForm.Create(nil);
  FButton := TfpgButton.Create(FForm);
  FButton.Text := '&Scan';
  FButton.OnClick := @ButtonClicked;
end;

procedure TTestAccelDispatch.TearDown;
begin
  FForm.Free;   { owns FButton }
end;

procedure TTestAccelDispatch.ButtonClicked(Sender: TObject);
begin
  Inc(FClickCount);
end;

function TTestAccelDispatch.SendKey(const AKey: word; const AShiftState: TShiftState): boolean;
var
  consumed: boolean;
begin
  consumed := False;
  { AOrigin is nil so that no widget is excluded from the walk }
  TfpgWidgetFriend(FForm).DoKeyShortcut(nil, AKey, AShiftState, consumed);
  Result := consumed;
end;

procedure TTestAccelDispatch.TestAltAccelFiresButton;
begin
  CheckTrue(SendKey(keyS, [ssAlt]), 'Alt+S should be consumed');
  CheckEquals(1, FClickCount);
end;

procedure TTestAccelDispatch.TestAltAccelIsCaseInsensitive;
begin
  { '&scan' declares a lowercase accelerator, but Alt+S must still fire it }
  FButton.Text := '&scan';
  CheckTrue(SendKey(keyS, [ssAlt]));
  CheckEquals(1, FClickCount);
end;

procedure TTestAccelDispatch.TestCtrlAltDoesNotFireButton;
begin
  { the regression this guards: Ctrl+Alt+S must not trigger Alt+S }
  CheckFalse(SendKey(keyS, [ssCtrl, ssAlt]), 'Ctrl+Alt+S must not be consumed');
  CheckEquals(0, FClickCount);
end;

procedure TTestAccelDispatch.TestShiftAltDoesNotFireButton;
begin
  CheckFalse(SendKey(keyS, [ssShift, ssAlt]));
  CheckEquals(0, FClickCount);
end;

procedure TTestAccelDispatch.TestNoModifierDoesNotFireButton;
begin
  { a bare 'S' keypress must not activate the button }
  CheckFalse(SendKey(keyS, []));
  CheckEquals(0, FClickCount);
end;

procedure TTestAccelDispatch.TestWrongLetterDoesNotFire;
begin
  CheckFalse(SendKey(keyQ, [ssAlt]));
  CheckEquals(0, FClickCount);
end;

procedure TTestAccelDispatch.TestDisabledButtonDoesNotFire;
begin
  FButton.Enabled := False;
  CheckFalse(SendKey(keyS, [ssAlt]));
  CheckEquals(0, FClickCount);
end;

procedure TTestAccelDispatch.TestButtonWithoutAccelDoesNotFire;
begin
  FButton.Text := 'Scan';
  CheckFalse(SendKey(keyS, [ssAlt]));
  CheckEquals(0, FClickCount);
end;

procedure TTestAccelDispatch.TestNestedButtonInPanelFires;
var
  pnl: TfpgPanel;
  btn: TfpgButton;
begin
  { a button nested inside a container must still be reachable }
  FButton.Text := 'Plain';
  pnl := TfpgPanel.Create(FForm);
  btn := TfpgButton.Create(pnl);
  btn.Text := '&Deep';
  btn.OnClick := @ButtonClicked;
  CheckTrue(SendKey(keyD, [ssAlt]), 'nested Alt+D should be consumed');
  CheckEquals(1, FClickCount);
end;

procedure TTestAccelDispatch.TestCheckBoxAccelToggles;
var
  cb: TfpgCheckBox;
begin
  FButton.Text := 'Plain';
  cb := TfpgCheckBox.Create(FForm);
  cb.Text := '&Verbose';
  CheckFalse(cb.Checked, 'precondition: unchecked');
  CheckTrue(SendKey(keyV, [ssAlt]), 'Alt+V should be consumed');
  CheckTrue(cb.Checked, 'checkbox should have toggled on');
  CheckTrue(SendKey(keyV, [ssAlt]));
  CheckFalse(cb.Checked, 'checkbox should have toggled back off');
end;


initialization
  RegisterTests;

end.
