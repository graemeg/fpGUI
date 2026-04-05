{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2026 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      IDE debug adapter — bridges the PDR debugger engine to the IDE's
      GUI thread. Owns the PDR component lifecycle (adapters + engine)
      and dispatches all commands to a persistent worker thread.

      All ptrace operations must happen on the same thread that forked
      the tracee, so the worker thread owns the entire ptrace
      relationship and collects stop info before posting back.
}

unit ide.debug.adapter;

{$mode objfpc}{$H+}
{ This unit requires FPC 3.3.x (HAS_OPDF_DEBUG). On FPC 3.2.x the IDE
  uses ide.debug.adapter.stub instead and this unit is never compiled. }
{$I ide.debug.config.inc}
{$IFNDEF HAS_OPDF_DEBUG}
  {. $FATAL ide.debug.adapter requires FPC 3.3.x or later (OPDF debug support)}
  interface
  implementation
{$ELSE}

interface

uses
  Classes, SysUtils,
  pdr_ports, pdr_engine,
  ide.debug.worker;

type
  TIDEDebugState = (
    idsIdle,        // No debug session
    idsStarting,    // Loading program, setting initial breakpoints
    idsRunning,     // Tracee is running (worker thread blocked in FpWaitPid)
    idsPaused,      // Tracee stopped at breakpoint/step/signal
    idsTerminated   // Tracee exited
  );

  TDebugStopEvent = procedure(Sender: TObject; AState: TIDEDebugState;
    const AFile: String; ALine: Integer) of object;

  TDebugOutputEvent = procedure(Sender: TObject;
    const AMessage: String) of object;

  { Fired on the main thread after a live SetBreakpointLive call completes.
    AHandle is the PDR handle (-1 means the engine could not install it).
    ATag is whatever integer the caller passed to SetBreakpointLive — use it
    to identify which breakpoint in the list to update. }
  TBreakpointSetEvent = procedure(Sender: TObject;
    AHandle: TBreakpointHandle; ATag: Integer) of object;

  TIDEDebugAdapter = class(TObject)
  private
    FEngine: TDebuggerEngine;
    FProcessController: IProcessController;
    FDebugInfoReader: IDebugInfoReader;
    FArchAdapter: IArchAdapter;
    FWorkerThread: TDebugWorkerThread;
    FState: TIDEDebugState;
    FOnStopped: TDebugStopEvent;
    FOnTerminated: TNotifyEvent;
    FOnOutput: TDebugOutputEvent;
    FOnBreakpointSet: TBreakpointSetEvent;
    FPendingBPTag: Integer;
    procedure HandleCommandDone;
    procedure HandleBPDone;
    procedure SendOutput(const AMsg: String);
  public
    constructor Create;
    destructor Destroy; override;

    { Session management }
    function  StartSession(const ABinaryPath: String): Boolean;
    procedure EndSession;

    { Execution control — dispatches to worker thread }
    procedure Run;
    procedure PrepareInitialBreakpoints(const ALocations: array of String);
    procedure Continue;
    procedure StepInto;
    procedure StepOver;
    procedure StepLine;
    procedure Pause;

    { Inspection — call only when State = idsPaused.
      TODO: These currently call through to the engine directly from
      the main thread. Since ptrace requires calls from the forking
      thread, these will need to be routed through the worker thread
      when we implement the Variables/Call Stack panels (steps 5.6-5.8). }
    function  GetLocalVariables: TVariableValueArray;
    function  GetCallStack(ALimit: Integer = 0): TStringArray;
    function  EvaluateExpression(const AExpr: String): TVariableValue;

    { Breakpoints — synchronous; safe only when no session is active }
    function  SetBreakpoint(const ALocation: String): TBreakpointHandle;
    function  RemoveBreakpoint(AHandle: TBreakpointHandle): Boolean;

    { Live breakpoint changes during an active session — routed through the
      worker thread so all ptrace calls happen on the ptrace owner thread.
      ATag is passed through to OnBreakpointSet so callers can identify which
      entry to update. Only valid when State = idsPaused. }
    procedure SetBreakpointLive(const ALocation: String; ATag: Integer);
    procedure RemoveBreakpointLive(AHandle: TBreakpointHandle; ATag: Integer);

    { State }
    property State: TIDEDebugState read FState;
    property Engine: TDebuggerEngine read FEngine;
    property OnStopped: TDebugStopEvent read FOnStopped write FOnStopped;
    property OnTerminated: TNotifyEvent read FOnTerminated write FOnTerminated;
    property OnOutput: TDebugOutputEvent read FOnOutput write FOnOutput;
    { Fired on the main thread after SetBreakpointLive or RemoveBreakpointLive
      completes. For Remove, AHandle will be 0. }
    property OnBreakpointSet: TBreakpointSetEvent
      read FOnBreakpointSet write FOnBreakpointSet;
  end;

implementation

uses
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  pdr_linux_ptrace, pdr_opdf_adapter, pdr_arch_adapters;


{ TIDEDebugAdapter }

constructor TIDEDebugAdapter.Create;
begin
  inherited Create;
  FState := idsIdle;
  FEngine := nil;
  FWorkerThread := nil;
end;

destructor TIDEDebugAdapter.Destroy;
begin
  EndSession;
  inherited Destroy;
end;

procedure TIDEDebugAdapter.SendOutput(const AMsg: String);
begin
  if Assigned(FOnOutput) then
    FOnOutput(Self, AMsg);
end;

function TIDEDebugAdapter.StartSession(const ABinaryPath: String): Boolean;
begin
  Result := False;

  { Clean up any previous session }
  if FState <> idsIdle then
    EndSession;

  try
    { Create platform-specific adapters — same wiring as PDR CLI }
    FProcessController := TLinuxPtraceAdapter.Create;
    FDebugInfoReader := TOPDFReaderAdapter.Create;
    {$IFDEF CPUX86_64}
    FArchAdapter := TArchX86_64Adapter.Create(FProcessController);
    {$ENDIF}
    {$IFDEF CPUI386}
    FArchAdapter := TArchX86Adapter.Create(FProcessController);
    {$ENDIF}

    { Create the debugger engine }
    FEngine := TDebuggerEngine.Create(FProcessController, FDebugInfoReader, FArchAdapter);

    { Load program and OPDF debug information }
    if not FEngine.LoadProgram(ABinaryPath) then
    begin
      SendOutput('Failed to load debug information from: ' + ABinaryPath);
      EndSession;
      Exit;
    end;

    { Create the persistent worker thread — it will own the ptrace
      relationship by being the thread that calls FEngine.Run (which
      forks the tracee). }
    FWorkerThread := TDebugWorkerThread.Create(FEngine, FProcessController,
      FDebugInfoReader, @HandleCommandDone);
    FWorkerThread.Start;

    FState := idsStarting;
    SendOutput('Debug session started: ' + ABinaryPath);
    Result := True;
  except
    on E: Exception do
    begin
      SendOutput('Error starting debug session: ' + E.Message);
      EndSession;
    end;
  end;
end;

procedure TIDEDebugAdapter.EndSession;
begin
  { Stop the worker thread — it will detach from the tracee on its
    own thread (the ptrace owner) before exiting. }
  if FWorkerThread <> nil then
  begin
    { If tracee is running, kill it to unblock FpWaitPid in the worker }
    {$IFDEF UNIX}
    if (FState = idsRunning) and (FEngine <> nil) and
       (FEngine.AttachedPID > 0) then
      FpKill(FEngine.AttachedPID, SIGKILL);
    {$ENDIF}
    FWorkerThread.SendCommand(dcQuit);
    FWorkerThread.WaitFor;
    FreeAndNil(FWorkerThread);
  end;

  { Clean up the engine.
    Do NOT call FEngine.Detach here — the tracee has already been
    killed (SIGKILL above), and Detach issues ptrace calls which
    must come from the worker thread. Just free the engine. }
  FreeAndNil(FEngine);

  { Release interface references }
  FProcessController := nil;
  FDebugInfoReader := nil;
  FArchAdapter := nil;
  FState := idsIdle;
end;

procedure TIDEDebugAdapter.HandleCommandDone;
var
  R: TDebugWorkerResult;
begin
  if FWorkerThread = nil then
    Exit;

  R := FWorkerThread.LastResult;

  if R.EngineState = dsTerminated then
  begin
    FState := idsTerminated;
    SendOutput('Process terminated.');
    if Assigned(FOnTerminated) then
      FOnTerminated(Self);
    Exit;
  end;

  if R.EngineState = dsPaused then
  begin
    FState := idsPaused;
    if Assigned(FOnStopped) then
      FOnStopped(Self, FState, R.StopFile, R.StopLine);
  end
  else
  begin
    SendOutput('Debugger in unexpected state after command.');
    FState := idsIdle;
  end;
end;

procedure TIDEDebugAdapter.HandleBPDone;
begin
  if Assigned(FOnBreakpointSet) and (FWorkerThread <> nil) then
    FOnBreakpointSet(Self, FWorkerThread.LastBPHandle, FPendingBPTag);
end;

procedure TIDEDebugAdapter.SetBreakpointLive(const ALocation: String; ATag: Integer);
begin
  if (FState = idsPaused) and (FWorkerThread <> nil) then
  begin
    FPendingBPTag := ATag;
    FWorkerThread.SendSetBreakpoint(ALocation, @HandleBPDone);
  end;
end;

procedure TIDEDebugAdapter.RemoveBreakpointLive(AHandle: TBreakpointHandle; ATag: Integer);
begin
  if (FState = idsPaused) and (FWorkerThread <> nil) then
  begin
    FPendingBPTag := ATag;
    FWorkerThread.SendRemoveBreakpoint(AHandle, @HandleBPDone);
  end;
end;

procedure TIDEDebugAdapter.Run;
begin
  if (FState in [idsStarting, idsPaused]) and (FWorkerThread <> nil) then
  begin
    FState := idsRunning;
    FWorkerThread.SendCommand(dcRun);
  end;
end;

procedure TIDEDebugAdapter.PrepareInitialBreakpoints(const ALocations: array of String);
begin
  if FWorkerThread <> nil then
    FWorkerThread.SetInitialBreakpoints(ALocations);
end;

procedure TIDEDebugAdapter.Continue;
begin
  if (FState = idsPaused) and (FWorkerThread <> nil) then
  begin
    FState := idsRunning;
    FWorkerThread.SendCommand(dcContinue);
  end;
end;

procedure TIDEDebugAdapter.StepInto;
begin
  if (FState = idsPaused) and (FWorkerThread <> nil) then
  begin
    FState := idsRunning;
    FWorkerThread.SendCommand(dcStepInto);
  end;
end;

procedure TIDEDebugAdapter.StepOver;
begin
  if (FState = idsPaused) and (FWorkerThread <> nil) then
  begin
    FState := idsRunning;
    FWorkerThread.SendCommand(dcStepOver);
  end;
end;

procedure TIDEDebugAdapter.StepLine;
begin
  if (FState = idsPaused) and (FWorkerThread <> nil) then
  begin
    FState := idsRunning;
    FWorkerThread.SendCommand(dcStepLine);
  end;
end;

procedure TIDEDebugAdapter.Pause;
begin
  {$IFDEF UNIX}
  if (FState = idsRunning) and (FEngine <> nil) and
     (FEngine.AttachedPID > 0) then
    FpKill(FEngine.AttachedPID, SIGSTOP);
  {$ENDIF}
end;

function TIDEDebugAdapter.GetLocalVariables: TVariableValueArray;
begin
  if (FState = idsPaused) and (FEngine <> nil) then
    Result := FEngine.GetLocalVariables
  else
    SetLength(Result, 0);
end;

function TIDEDebugAdapter.GetCallStack(ALimit: Integer): TStringArray;
begin
  if (FState = idsPaused) and (FEngine <> nil) then
    Result := FEngine.GetCallStack(ALimit)
  else
    SetLength(Result, 0);
end;

function TIDEDebugAdapter.EvaluateExpression(const AExpr: String): TVariableValue;
begin
  if (FState = idsPaused) and (FEngine <> nil) then
    Result := FEngine.EvaluateExpression(AExpr)
  else
  begin
    Result.Name := AExpr;
    Result.Value := '<not available>';
    Result.IsValid := False;
  end;
end;

function TIDEDebugAdapter.SetBreakpoint(const ALocation: String): TBreakpointHandle;
begin
  if FEngine <> nil then
    Result := FEngine.SetBreakpoint(ALocation)
  else
    Result := 0;
end;

function TIDEDebugAdapter.RemoveBreakpoint(AHandle: TBreakpointHandle): Boolean;
begin
  if FEngine <> nil then
    Result := FEngine.RemoveBreakpoint(AHandle)
  else
    Result := False;
end;

{$ENDIF}
end.
