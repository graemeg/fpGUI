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
    dcQuit         // Tells the thread to exit its loop
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
    { Result from the last command — read after OnCommandDone fires }
    property LastResult: TDebugWorkerResult read FResult;
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

    { Execute the blocking PDR command on this thread }
    case Cmd of
      dcRun:      FEngine.Run;
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

end.
