unit tctimer;

{$mode objfpc}{$h+}

interface

uses
  Classes,
  SysUtils,
  fpcunit, testutils, testregistry,
  fpg_base,
  fpg_main;

type

  TTestFPGTimer = class(TTestCase)
  private
    FSelfDestructTimer: TfpgTimer;
    FCallbackFired: Boolean;
    procedure SelfDestructCallback(Sender: TObject);
  published
    procedure TestResetAllTimers_WithNilEntry;
    procedure TestCheckTimers_TimerSelfDestruct;
  end;


procedure RegisterTests;


implementation


procedure RegisterTests;
begin
  RegisterTest(TTestFPGTimer);
end;


{ TTestFPGTimer }

procedure TTestFPGTimer.SelfDestructCallback(Sender: TObject);
begin
  FCallbackFired := True;
  // Timer destroys itself during its own callback
  FSelfDestructTimer.Free;
  FSelfDestructTimer := nil;
end;

procedure TTestFPGTimer.TestResetAllTimers_WithNilEntry;
var
  t: TfpgTimer;
begin
  // Create a timer, then destroy it. The destructor sets fpgTimers[i] := nil
  // rather than removing the entry. fpgResetAllTimers must handle nil entries
  // without crashing.
  t := TfpgTimer.Create(1000);
  t.Free;

  // This should not raise an access violation
  fpgResetAllTimers;
end;

procedure TTestFPGTimer.TestCheckTimers_TimerSelfDestruct;
begin
  // Create a timer whose callback frees the timer itself.
  // After the callback, fpgCheckTimers must not access the freed object.
  FCallbackFired := False;
  FSelfDestructTimer := TfpgTimer.Create(1);
  FSelfDestructTimer.OnTimer := @SelfDestructCallback;
  FSelfDestructTimer.Enabled := True;

  // Wait long enough for the timer to be past due
  Sleep(50);

  // This should not raise an access violation
  fpgCheckTimers;

  AssertTrue('Timer callback should have fired', FCallbackFired);
end;

initialization
  RegisterTests;

end.
