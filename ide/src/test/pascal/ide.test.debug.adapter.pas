{
    fpGUI IDE - Debug Adapter Tests

    Tests for TIDEDebugAdapter state machine transitions.
}
unit ide.test.debug.adapter;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  ide.debug.adapter;

type

  { TTestDebugAdapterState }

  TTestDebugAdapterState = class(TTestCase)
  published
    procedure TestInitialStateIsIdle;
    procedure TestStartSessionWithInvalidBinary;
    procedure TestEndSessionResetsState;
    procedure TestRunRequiresStartingOrPaused;
    procedure TestContinueRequiresPaused;
    procedure TestStepRequiresPaused;
  end;

implementation

{ TTestDebugAdapterState }

procedure TTestDebugAdapterState.TestInitialStateIsIdle;
var
  adapter: TIDEDebugAdapter;
begin
  adapter := TIDEDebugAdapter.Create;
  try
    AssertTrue('initial state should be idsIdle', adapter.State = idsIdle);
    AssertTrue('engine should be nil before session', adapter.Engine = nil);
  finally
    adapter.Free;
  end;
end;

procedure TTestDebugAdapterState.TestStartSessionWithInvalidBinary;
var
  adapter: TIDEDebugAdapter;
  ok: Boolean;
begin
  adapter := TIDEDebugAdapter.Create;
  try
    ok := adapter.StartSession('/nonexistent/binary');
    AssertFalse('should fail for nonexistent binary', ok);
    AssertTrue('state should remain idle', adapter.State = idsIdle);
    AssertTrue('engine should be nil after failed start', adapter.Engine = nil);
  finally
    adapter.Free;
  end;
end;

procedure TTestDebugAdapterState.TestEndSessionResetsState;
var
  adapter: TIDEDebugAdapter;
begin
  adapter := TIDEDebugAdapter.Create;
  try
    { Even without a session, EndSession should not crash }
    adapter.EndSession;
    AssertTrue('state should be idle after EndSession', adapter.State = idsIdle);
  finally
    adapter.Free;
  end;
end;

procedure TTestDebugAdapterState.TestRunRequiresStartingOrPaused;
var
  adapter: TIDEDebugAdapter;
begin
  adapter := TIDEDebugAdapter.Create;
  try
    { Run from idle should be a no-op (no crash) }
    adapter.Run;
    AssertTrue('state should remain idle', adapter.State = idsIdle);
  finally
    adapter.Free;
  end;
end;

procedure TTestDebugAdapterState.TestContinueRequiresPaused;
var
  adapter: TIDEDebugAdapter;
begin
  adapter := TIDEDebugAdapter.Create;
  try
    adapter.Continue;
    AssertTrue('state should remain idle', adapter.State = idsIdle);
  finally
    adapter.Free;
  end;
end;

procedure TTestDebugAdapterState.TestStepRequiresPaused;
var
  adapter: TIDEDebugAdapter;
begin
  adapter := TIDEDebugAdapter.Create;
  try
    adapter.StepInto;
    AssertTrue('state should remain idle after StepInto', adapter.State = idsIdle);
    adapter.StepOver;
    AssertTrue('state should remain idle after StepOver', adapter.State = idsIdle);
    adapter.StepLine;
    AssertTrue('state should remain idle after StepLine', adapter.State = idsIdle);
  finally
    adapter.Free;
  end;
end;


initialization
  RegisterTest(TTestDebugAdapterState);

end.
