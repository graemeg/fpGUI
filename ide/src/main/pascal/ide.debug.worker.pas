{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2026 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Worker thread for executing blocking PDR debugger commands.
      Each instance runs a single command (Run, Continue, Step, etc.)
      and posts the result back to the main thread via Synchronize.
}

unit ide.debug.worker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pdr_engine;

type
  TDebugCommand = (
    dcRun,
    dcContinue,
    dcStepInto,
    dcStepOver,
    dcStepLine
  );

  TDebugWorkerThread = class(TThread)
  private
    FEngine: TDebuggerEngine;
    FCommand: TDebugCommand;
    FOnStopped: TThreadMethod;
  protected
    procedure Execute; override;
  public
    constructor Create(AEngine: TDebuggerEngine; ACommand: TDebugCommand;
      AOnStopped: TThreadMethod);
  end;

implementation

{ TDebugWorkerThread }

constructor TDebugWorkerThread.Create(AEngine: TDebuggerEngine;
  ACommand: TDebugCommand; AOnStopped: TThreadMethod);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FEngine := AEngine;
  FCommand := ACommand;
  FOnStopped := AOnStopped;
end;

procedure TDebugWorkerThread.Execute;
begin
  case FCommand of
    dcRun:      FEngine.Run;
    dcContinue: FEngine.Continue;
    dcStepInto: FEngine.StepInto;
    dcStepOver: FEngine.StepOver;
    dcStepLine: FEngine.StepLine;
  end;
  Synchronize(FOnStopped);
end;

end.
