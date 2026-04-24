{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2026 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      No-op stub for TIDEDebugAdapter, used when building with FPC 3.2.x.

      Exports the same types and class interface as ide.debug.adapter so
      that ide.form.main compiles without any conditional blocks inside it.
      All methods are empty; StartSession always returns False.

      When HAS_OPDF_DEBUG is defined (FPC >= 3.3.x), ide.debug.adapter is
      used instead and this unit is never compiled.
}
unit ide.debug.adapter.stub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TIDEDebugState = (
    idsIdle,        { No debug session }
    idsStarting,    { Loading program, setting initial breakpoints }
    idsRunning,     { Tracee is running }
    idsPaused,      { Tracee stopped at breakpoint/step/signal }
    idsTerminated   { Tracee exited }
  );

  TBreakpointHandle = type Integer;

  { Minimal value record — matches pdr_ports.TVariableValue }
  TVariableValue = record
    Name:     String;
    TypeName: String;
    Value:    String;
    Address:  QWord;
    IsValid:  Boolean;
  end;

  TVariableValueArray = array of TVariableValue;
  TStringArray        = array of String;

  { Exception info — matches pdr_ports.TExceptionInfo }
  TExceptionInfo = record
    IsValid:    Boolean;
    ClassName:  String;
    Message:    String;
    RaiseAddr:  QWord;
    SourceFile: String;
    SourceLine: Integer;
  end;

  TDebugStopEvent = procedure(Sender: TObject; AState: TIDEDebugState;
      const AFile: String; ALine: Integer) of object;

  TDebugOutputEvent = procedure(Sender: TObject;
      const AMessage: String) of object;

  TBreakpointSetEvent = procedure(Sender: TObject;
      AHandle: TBreakpointHandle; ATag: Integer) of object;

  { TIDEDebugAdapter — stub, all methods are no-ops }
  TIDEDebugAdapter = class(TObject)
  private
    FState:                    TIDEDebugState;
    FLastLocalVars:            TVariableValueArray;
    FLastLocalVarsWithParents: TVariableValueArray;
    FLastGlobalVars:           TVariableValueArray;
    FLastCallStack:            TStringArray;
    FLastWatchResults:         TVariableValueArray;
    FLastExceptionInfo:        TExceptionInfo;
    FLastEvalResult:           TVariableValue;
    FOnStopped:      TDebugStopEvent;
    FOnTerminated:   TNotifyEvent;
    FOnOutput:       TDebugOutputEvent;
    FOnBreakpointSet: TBreakpointSetEvent;
  public
    constructor Create;
    destructor  Destroy; override;

    function  StartSession(const ABinaryPath: String): Boolean;
    procedure EndSession;

    procedure Run;
    procedure PrepareInitialBreakpoints(const ALocations: array of String);
    procedure Continue;
    procedure StepInto;
    procedure StepOver;
    procedure StepLine;
    procedure Pause;

    function  GetLocalVariables: TVariableValueArray;
    function  GetLocalVariablesWithParents: TVariableValueArray;
    function  GetCallStack(ALimit: Integer = 0): TStringArray;
    function  EvaluateExpression(const AExpr: String): TVariableValue;

    function  SetBreakpoint(const ALocation: String): TBreakpointHandle;
    function  RemoveBreakpoint(AHandle: TBreakpointHandle): Boolean;
    procedure SetBreakpointLive(const ALocation: String; ATag: Integer);
    procedure RemoveBreakpointLive(AHandle: TBreakpointHandle; ATag: Integer);
    procedure EvaluateExpressionLive(const AExpr: String; AOnDone: TNotifyEvent);
    procedure SetVarCollectScope(AValue: Boolean);
    procedure SetVarCollectGlobals(AValue: Boolean);
    procedure SetWatches(const AExprs: TStringArray);

    property State:          TIDEDebugState  read FState;
    property LastLocalVars:            TVariableValueArray read FLastLocalVars;
    property LastLocalVarsWithParents: TVariableValueArray read FLastLocalVarsWithParents;
    property LastGlobalVars:           TVariableValueArray read FLastGlobalVars;
    property LastCallStack:            TStringArray        read FLastCallStack;
    property LastWatchResults:         TVariableValueArray read FLastWatchResults;
    property LastExceptionInfo:        TExceptionInfo      read FLastExceptionInfo;
    property LastEvalResult:           TVariableValue      read FLastEvalResult;
    property OnStopped:      TDebugStopEvent read FOnStopped      write FOnStopped;
    property OnTerminated:   TNotifyEvent    read FOnTerminated    write FOnTerminated;
    property OnOutput:       TDebugOutputEvent read FOnOutput      write FOnOutput;
    property OnBreakpointSet: TBreakpointSetEvent
        read FOnBreakpointSet write FOnBreakpointSet;
  end;

implementation

constructor TIDEDebugAdapter.Create;
begin
  inherited Create;
  FState := idsIdle;
end;

destructor TIDEDebugAdapter.Destroy;
begin
  inherited Destroy;
end;

function TIDEDebugAdapter.StartSession(const ABinaryPath: String): Boolean;
begin
  Result := False;  { OPDF debugging requires FPC 3.3.x }
end;

procedure TIDEDebugAdapter.EndSession;
begin
end;

procedure TIDEDebugAdapter.Run;
begin
end;

procedure TIDEDebugAdapter.PrepareInitialBreakpoints(const ALocations: array of String);
begin
end;

procedure TIDEDebugAdapter.Continue;
begin
end;

procedure TIDEDebugAdapter.StepInto;
begin
end;

procedure TIDEDebugAdapter.StepOver;
begin
end;

procedure TIDEDebugAdapter.StepLine;
begin
end;

procedure TIDEDebugAdapter.Pause;
begin
end;

function TIDEDebugAdapter.GetLocalVariables: TVariableValueArray;
begin
  SetLength(Result, 0);
end;

function TIDEDebugAdapter.GetLocalVariablesWithParents: TVariableValueArray;
begin
  SetLength(Result, 0);
end;

function TIDEDebugAdapter.GetCallStack(ALimit: Integer): TStringArray;
begin
  SetLength(Result, 0);
end;

function TIDEDebugAdapter.EvaluateExpression(const AExpr: String): TVariableValue;
begin
  Result.Name    := AExpr;
  Result.Value   := '<debug not available — requires FPC 3.3.x>';
  Result.IsValid := False;
end;

function TIDEDebugAdapter.SetBreakpoint(const ALocation: String): TBreakpointHandle;
begin
  Result := 0;
end;

function TIDEDebugAdapter.RemoveBreakpoint(AHandle: TBreakpointHandle): Boolean;
begin
  Result := False;
end;

procedure TIDEDebugAdapter.SetBreakpointLive(const ALocation: String; ATag: Integer);
begin
end;

procedure TIDEDebugAdapter.RemoveBreakpointLive(AHandle: TBreakpointHandle; ATag: Integer);
begin
end;

procedure TIDEDebugAdapter.EvaluateExpressionLive(const AExpr: String;
  AOnDone: TNotifyEvent);
begin
  { Stub: populate a placeholder result and fire the callback immediately }
  FLastEvalResult.Name    := AExpr;
  FLastEvalResult.Value   := '<debug not available — requires FPC 3.3.x>';
  FLastEvalResult.IsValid := False;
  if Assigned(AOnDone) then
    AOnDone(Self);
end;

procedure TIDEDebugAdapter.SetVarCollectScope(AValue: Boolean);
begin
end;

procedure TIDEDebugAdapter.SetVarCollectGlobals(AValue: Boolean);
begin
end;

procedure TIDEDebugAdapter.SetWatches(const AExprs: TStringArray);
begin
end;

end.
