{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2026 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Persistent worker thread for the PDR debugger engine.

      All ptrace calls must originate from the same thread that forked
      the tracee. This thread owns the entire ptrace relationship:
      it launches the program, issues all execution commands, and
      reads registers/memory. Results are posted to the main thread
      via Synchronize.

      The thread sleeps on an RTLEvent until the main thread posts
      a command via SendCommand. After executing, it posts back
      via Synchronize and waits for the next command.
}

unit ide.debug.worker;

{$mode objfpc}{$H+}
{ This unit requires FPC 3.3.x (HAS_OPDF_DEBUG). On FPC 3.2.x the IDE
  uses ide.debug.adapter.stub instead and this unit is never compiled. }
{$I ide.debug.config.inc}
{$IFNDEF HAS_OPDF_DEBUG}
  {. $FATAL ide.debug.worker requires FPC 3.3.x or later (OPDF debug support)}
  interface
  implementation
{$ELSE}

interface

uses
  Classes, SysUtils, pdr_ports, pdr_engine;

type
  TDebugCommand = (
    dcNone,
    dcRun,
    dcContinue,
    dcStepInto,
    dcStepOver,
    dcStepLine,
    dcSetBreakpoint,    // Install one breakpoint; result in LastBPHandle
    dcRemoveBreakpoint, // Remove one breakpoint by handle
    dcQuit              // Tells the thread to exit its loop
  );

  { Result data collected on the worker thread, read on the main thread }
  TDebugWorkerResult = record
    EngineState: TDebuggerState;
    StopFile: String;
    StopLine: Integer;
    CurrentAddress: QWord;
  end;

  TDebugWorkerThread = class(TThread)
  private
    FEngine: TDebuggerEngine;
    FProcessController: IProcessController;
    FDebugInfoReader: IDebugInfoReader;
    FCommand: TDebugCommand;
    FCommandEvent: PRTLEvent;
    FResult: TDebugWorkerResult;
    FOnCommandDone: TThreadMethod;
    FInitialBreakpoints: array of String;  { set before dcRun; installed on worker thread }
    { Breakpoint command fields — written by main thread before SendSetBreakpoint/
      SendRemoveBreakpoint; read by worker thread after RTLEventWaitFor }
    FBPLocation: String;
    FBPHandle: TBreakpointHandle;
    FOnBPDone: TThreadMethod;
    procedure CollectStopInfo;
  protected
    procedure Execute; override;
  public
    constructor Create(AEngine: TDebuggerEngine;
      AProcessController: IProcessController;
      ADebugInfoReader: IDebugInfoReader;
      AOnCommandDone: TThreadMethod);
    destructor Destroy; override;
    { Post a command from the main thread. Thread-safe. }
    procedure SendCommand(ACmd: TDebugCommand);
    { Set breakpoint locations to install at program launch. Call before SendCommand(dcRun). }
    procedure SetInitialBreakpoints(const ALocations: array of String);
    { Install a single breakpoint on the ptrace owner thread. AOnDone is called on the
      main thread when complete; read LastBPHandle for the resulting handle. }
    procedure SendSetBreakpoint(const ALocation: String; AOnDone: TThreadMethod);
    { Remove a single breakpoint by handle on the ptrace owner thread. AOnDone is called
      on the main thread when complete. }
    procedure SendRemoveBreakpoint(AHandle: TBreakpointHandle; AOnDone: TThreadMethod);
    { Result from the last run/pause command — read after OnCommandDone fires }
    property LastResult: TDebugWorkerResult read FResult;
    { Handle from the last dcSetBreakpoint command — read after OnBPDone fires }
    property LastBPHandle: TBreakpointHandle read FBPHandle;
  end;

implementation

{ TDebugWorkerThread }

constructor TDebugWorkerThread.Create(AEngine: TDebuggerEngine;
  AProcessController: IProcessController;
  ADebugInfoReader: IDebugInfoReader;
  AOnCommandDone: TThreadMethod);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FEngine := AEngine;
  FProcessController := AProcessController;
  FDebugInfoReader := ADebugInfoReader;
  FOnCommandDone := AOnCommandDone;
  FCommand := dcNone;
  FCommandEvent := RTLEventCreate;
end;

destructor TDebugWorkerThread.Destroy;
begin
  RTLEventDestroy(FCommandEvent);
  inherited Destroy;
end;

procedure TDebugWorkerThread.SendCommand(ACmd: TDebugCommand);
begin
  FCommand := ACmd;
  RTLEventSetEvent(FCommandEvent);
end;

procedure TDebugWorkerThread.SendSetBreakpoint(const ALocation: String;
  AOnDone: TThreadMethod);
begin
  FBPLocation := ALocation;
  FBPHandle   := 0;
  FOnBPDone   := AOnDone;
  FCommand    := dcSetBreakpoint;
  RTLEventSetEvent(FCommandEvent);
end;

procedure TDebugWorkerThread.SendRemoveBreakpoint(AHandle: TBreakpointHandle;
  AOnDone: TThreadMethod);
begin
  FBPHandle := AHandle;
  FOnBPDone := AOnDone;
  FCommand  := dcRemoveBreakpoint;
  RTLEventSetEvent(FCommandEvent);
end;

procedure TDebugWorkerThread.SetInitialBreakpoints(const ALocations: array of String);
var
  i: Integer;
begin
  SetLength(FInitialBreakpoints, Length(ALocations));
  for i := 0 to High(ALocations) do
    FInitialBreakpoints[i] := ALocations[i];
end;

procedure TDebugWorkerThread.CollectStopInfo;
var
  LineInfo: TLineInfo;
  Addr: QWord;
begin
  FResult.EngineState := FEngine.GetState;
  FResult.StopFile := '';
  FResult.StopLine := 0;
  FResult.CurrentAddress := 0;

  if FResult.EngineState = dsPaused then
  begin
    Addr := FProcessController.GetCurrentAddress;
    FResult.CurrentAddress := Addr;
    if FDebugInfoReader.FindLineByAddress(Addr, LineInfo) then
    begin
      FResult.StopFile := LineInfo.FileName;
      FResult.StopLine := Integer(LineInfo.LineNumber);
    end;
  end;
end;

procedure TDebugWorkerThread.Execute;
var
  Cmd: TDebugCommand;
  i: Integer;
begin
  while not Terminated do
  begin
    { Wait for a command from the main thread }
    RTLEventWaitFor(FCommandEvent);
    RTLEventResetEvent(FCommandEvent);

    Cmd := FCommand;
    FCommand := dcNone;

    if (Cmd = dcQuit) or Terminated then
    begin
      { Clean up ptrace relationship from this thread (the ptrace owner).
        Kill the tracee and detach so the engine destructor won't try
        to issue ptrace calls from the main thread. }
      if FEngine.State in [dsRunning, dsPaused] then
        FEngine.Detach;
      Break;
    end;

    if Cmd = dcNone then
      System.Continue;

    { Breakpoint commands post a separate callback and do not collect stop info }
    if Cmd = dcSetBreakpoint then
    begin
      FBPHandle := FEngine.SetBreakpoint(FBPLocation);
      if not Terminated then
        Synchronize(FOnBPDone);
      System.Continue;
    end;

    if Cmd = dcRemoveBreakpoint then
    begin
      FEngine.RemoveBreakpoint(FBPHandle);
      if not Terminated then
        Synchronize(FOnBPDone);
      System.Continue;
    end;

    { Execute the blocking PDR command on this thread }
    case Cmd of
      dcRun:
        begin
          FEngine.Run;
          { Process is now paused at entry point — install any pre-registered
            breakpoints while still on the ptrace owner thread, then
            auto-continue so the user sees the first real stop. }
          if Length(FInitialBreakpoints) > 0 then
          begin
            for i := 0 to High(FInitialBreakpoints) do
              FEngine.SetBreakpoint(FInitialBreakpoints[i]);
            SetLength(FInitialBreakpoints, 0);
            FEngine.Continue;  { blocks until next stop }
          end;
        end;
      dcContinue: FEngine.Continue;
      dcStepInto: FEngine.StepInto;
      dcStepOver: FEngine.StepOver;
      dcStepLine: FEngine.StepLine;
    end;

    { Collect stop info while still on this thread (ptrace owner) }
    CollectStopInfo;

    { Post result to the main thread }
    if not Terminated then
      Synchronize(FOnCommandDone);
  end;
end;

{$ENDIF}
end.
