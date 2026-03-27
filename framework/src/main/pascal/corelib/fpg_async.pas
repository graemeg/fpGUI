{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      High-level async task support for fpGUI, inspired by Java's
      SwingWorker pattern.

      TfpgAsyncTask provides a base class for background tasks that
      can publish progress to the main GUI thread safely. Subclass
      it, override Execute (runs on worker thread), and call
      PublishProgress() to deliver status updates to the GUI.

      fpgInvokeLater() schedules a procedure for execution on the
      main GUI thread, similar to Java's SwingUtilities.invokeLater()
      or Qt's QMetaObject::invokeMethod(Qt::QueuedConnection).
}

unit fpg_async;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type

  { TfpgAsyncTask - Base class for background tasks with progress }

  TfpgAsyncTask = class(TObject)
  private
    FThread: TThread;
    FProgressQueue: TStringList;
    FProgressLock: TCriticalSection;
    FErrorMsg: string;
    procedure DoFlushProgress;
    procedure DoComplete;
    procedure DoError;
    procedure InternalExecute;
    procedure HandleThreadTerminate(Sender: TObject);
  protected
    { Override to perform background work. Called on the worker thread.
      Call PublishProgress() from here to send updates to the GUI. }
    procedure Execute; virtual; abstract;

    { Thread-safe: push a progress line for delivery to OnProgress.
      Multiple calls between GUI thread flushes are batched. }
    procedure PublishProgress(const ALine: string);

    { Called on the main GUI thread for each progress line.
      Override to update your UI. }
    procedure OnProgress(const ALine: string); virtual;

    { Called on the main GUI thread when Execute completes normally. }
    procedure OnComplete; virtual;

    { Called on the main GUI thread if Execute raises an exception. }
    procedure OnError(const AMessage: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    { Start the task on a background thread. Non-blocking. }
    procedure Start;

    { Run synchronously on the current thread (for testing).
      Progress and completion callbacks are called directly. }
    procedure RunSynchronous;
  end;


{ Schedule a parameterless procedure for execution on the main GUI thread.
  Thread-safe, non-blocking. Wakes the event loop via WakeMainThread. }
procedure fpgInvokeLater(AProc: TProcedure);

{ Process all pending fpgInvokeLater items. Called from the event loop. }
procedure fpgProcessInvokeQueue;


implementation

uses
  fpg_base,
  fpg_main;


type

  { Internal thread class }
  TAsyncTaskThread = class(TThread)
  private
    FTask: TfpgAsyncTask;
  protected
    procedure Execute; override;
  public
    constructor Create(ATask: TfpgAsyncTask);
  end;


var
  { Thread-safe queue of pending invoke procedures }
  GInvokeQueue: TThreadList;


{ TAsyncTaskThread }

constructor TAsyncTaskThread.Create(ATask: TfpgAsyncTask);
begin
  inherited Create(True);
  FTask := ATask;
  FreeOnTerminate := True;
end;

procedure TAsyncTaskThread.Execute;
begin
  FTask.InternalExecute;
end;


{ fpgInvokeLater }

type
  TInvokeItem = class
    Proc: TProcedure;
  end;

procedure fpgInvokeLater(AProc: TProcedure);
var
  item: TInvokeItem;
begin
  item := TInvokeItem.Create;
  item.Proc := AProc;
  GInvokeQueue.Add(item);
  fpgApplication.WakeMainThread;
end;

{ Process all pending invoke items. Called from fpgDeliverMessages
  or application.ProcessMessages. }
procedure fpgProcessInvokeQueue;
var
  lst: TList;
  i: Integer;
  item: TInvokeItem;
  snapshot: TList;
begin
  snapshot := TList.Create;
  try
    lst := GInvokeQueue.LockList;
    try
      for i := 0 to lst.Count - 1 do
        snapshot.Add(lst[i]);
      lst.Clear;
    finally
      GInvokeQueue.UnlockList;
    end;
    for i := 0 to snapshot.Count - 1 do
    begin
      item := TInvokeItem(snapshot[i]);
      if Assigned(item.Proc) then
        item.Proc();
      item.Free;
    end;
  finally
    snapshot.Free;
  end;
end;


{ TfpgAsyncTask }

constructor TfpgAsyncTask.Create;
begin
  inherited Create;
  FThread := nil;
  FProgressQueue := TStringList.Create;
  FProgressLock := TCriticalSection.Create;
  FErrorMsg := '';
end;

destructor TfpgAsyncTask.Destroy;
begin
  FProgressLock.Free;
  FProgressQueue.Free;
  inherited Destroy;
end;

procedure TfpgAsyncTask.PublishProgress(const ALine: string);
begin
  FProgressLock.Acquire;
  try
    FProgressQueue.Add(ALine);
  finally
    FProgressLock.Release;
  end;
end;

procedure TfpgAsyncTask.DoFlushProgress;
var
  i: Integer;
  snapshot: TStringList;
begin
  snapshot := TStringList.Create;
  try
    FProgressLock.Acquire;
    try
      snapshot.Assign(FProgressQueue);
      FProgressQueue.Clear;
    finally
      FProgressLock.Release;
    end;
    for i := 0 to snapshot.Count - 1 do
      OnProgress(snapshot[i]);
  finally
    snapshot.Free;
  end;
end;

procedure TfpgAsyncTask.DoComplete;
begin
  DoFlushProgress;
  OnComplete;
end;

procedure TfpgAsyncTask.DoError;
begin
  DoFlushProgress;
  OnError(FErrorMsg);
end;

procedure TfpgAsyncTask.InternalExecute;
begin
  try
    Execute;
    FErrorMsg := '';
  except
    on E: Exception do
      FErrorMsg := E.Message;
  end;
end;

procedure TfpgAsyncTask.OnProgress(const ALine: string);
begin
  { Default: no-op. Override in subclasses. }
end;

procedure TfpgAsyncTask.OnComplete;
begin
  { Default: no-op. Override in subclasses. }
end;

procedure TfpgAsyncTask.OnError(const AMessage: string);
begin
  { Default: no-op. Override in subclasses. }
end;

procedure TfpgAsyncTask.HandleThreadTerminate(Sender: TObject);
begin
  if FErrorMsg <> '' then
    DoError
  else
    DoComplete;
end;

procedure TfpgAsyncTask.Start;
var
  thd: TAsyncTaskThread;
begin
  thd := TAsyncTaskThread.Create(Self);
  FThread := thd;
  { OnTerminate is called on the main thread via Synchronize by FPC's
    RTL after Execute completes. The wake channel ensures this happens
    promptly without waiting for user input. }
  thd.OnTerminate := @HandleThreadTerminate;
  thd.Start;
end;

procedure TfpgAsyncTask.RunSynchronous;
begin
  InternalExecute;
  if FErrorMsg <> '' then
    DoError
  else
    DoComplete;
end;


initialization
  GInvokeQueue := TThreadList.Create;

finalization
  GInvokeQueue.Free;

end.
