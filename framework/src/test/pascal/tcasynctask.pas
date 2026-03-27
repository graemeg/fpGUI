unit tcasynctask;

{$mode objfpc}{$h+}

interface

uses
  Classes,
  SysUtils,
  fpcunit, testutils, testregistry,
  fpg_base,
  fpg_main,
  fpg_async;

type

  { Test async task that sums numbers and publishes progress }
  TTestSumTask = class(TfpgAsyncTask)
  private
    FFrom: Integer;
    FTo: Integer;
    FResult: Integer;
  protected
    procedure Execute; override;
  public
    ProgressLines: TStringList;
    CompleteCalled: Boolean;
    ErrorMessage: string;
    procedure OnProgress(const ALine: string); override;
    procedure OnComplete; override;
    procedure OnError(const AMessage: string); override;
    constructor Create(AFrom, ATo: Integer);
    destructor Destroy; override;
    property SumResult: Integer read FResult;
  end;

  { Test async task that raises an exception }
  TTestFailingTask = class(TfpgAsyncTask)
  protected
    procedure Execute; override;
  public
    CompleteCalled: Boolean;
    ErrorMessage: string;
    procedure OnComplete; override;
    procedure OnError(const AMessage: string); override;
    constructor Create;
  end;

  { TTestAsyncTask }

  TTestAsyncTask = class(TTestCase)
  published
    { Synchronous path }
    procedure TestRunSynchronous_ExecutesAndCompletes;
    procedure TestRunSynchronous_PublishProgressDelivered;
    procedure TestRunSynchronous_ErrorCallsOnError;
    { Threaded path via Start }
    procedure TestStart_CompleteFiredOnMainThread;
    procedure TestStart_ProgressDeliveredViaThread;
    procedure TestStart_ErrorFiredOnMainThread;
  end;

  { TTestInvokeLater }

  TTestInvokeLater = class(TTestCase)
  published
    procedure TestInvokeLater_ExecutesOnMainThread;
  end;


procedure RegisterTests;


implementation


procedure RegisterTests;
begin
  RegisterTest(TTestAsyncTask);
  RegisterTest(TTestInvokeLater);
end;


{ TTestSumTask }

constructor TTestSumTask.Create(AFrom, ATo: Integer);
begin
  inherited Create;
  FFrom := AFrom;
  FTo := ATo;
  FResult := 0;
  ProgressLines := TStringList.Create;
  CompleteCalled := False;
  ErrorMessage := '';
end;

destructor TTestSumTask.Destroy;
begin
  ProgressLines.Free;
  inherited Destroy;
end;

procedure TTestSumTask.Execute;
var
  i: Integer;
begin
  FResult := 0;
  for i := FFrom to FTo do
  begin
    FResult := FResult + i;
    if (i mod 10) = 0 then
      PublishProgress('Sum at ' + IntToStr(i) + ' = ' + IntToStr(FResult));
  end;
end;

procedure TTestSumTask.OnProgress(const ALine: string);
begin
  ProgressLines.Add(ALine);
end;

procedure TTestSumTask.OnComplete;
begin
  CompleteCalled := True;
end;

procedure TTestSumTask.OnError(const AMessage: string);
begin
  ErrorMessage := AMessage;
end;


{ TTestFailingTask }

constructor TTestFailingTask.Create;
begin
  inherited Create;
  CompleteCalled := False;
  ErrorMessage := '';
end;

procedure TTestFailingTask.Execute;
begin
  raise Exception.Create('deliberate test failure');
end;

procedure TTestFailingTask.OnComplete;
begin
  CompleteCalled := True;
end;

procedure TTestFailingTask.OnError(const AMessage: string);
begin
  ErrorMessage := AMessage;
end;


{ TTestAsyncTask — synchronous path }

procedure TTestAsyncTask.TestRunSynchronous_ExecutesAndCompletes;
var
  task: TTestSumTask;
begin
  task := TTestSumTask.Create(1, 100);
  try
    task.RunSynchronous;
    AssertTrue('OnComplete should have been called', task.CompleteCalled);
    AssertEquals('Sum 1..100 = 5050', 5050, task.SumResult);
    AssertEquals('No errors', '', task.ErrorMessage);
  finally
    task.Free;
  end;
end;

procedure TTestAsyncTask.TestRunSynchronous_PublishProgressDelivered;
var
  task: TTestSumTask;
begin
  task := TTestSumTask.Create(1, 50);
  try
    task.RunSynchronous;
    AssertEquals('Should have 5 progress lines', 5, task.ProgressLines.Count);
    AssertTrue('First progress line should mention 10',
      Pos('10', task.ProgressLines[0]) > 0);
  finally
    task.Free;
  end;
end;

procedure TTestAsyncTask.TestRunSynchronous_ErrorCallsOnError;
var
  task: TTestFailingTask;
begin
  task := TTestFailingTask.Create;
  try
    task.RunSynchronous;
    AssertFalse('OnComplete should NOT be called on error', task.CompleteCalled);
    AssertTrue('OnError should have the exception message',
      Pos('deliberate test failure', task.ErrorMessage) > 0);
  finally
    task.Free;
  end;
end;

{ TTestAsyncTask — threaded path via Start }

procedure TTestAsyncTask.TestStart_CompleteFiredOnMainThread;
var
  task: TTestSumTask;
  waited: Integer;
begin
  task := TTestSumTask.Create(1, 100);
  try
    task.Start;
    { Poll CheckSynchronize to process the OnTerminate callback.
      The wake channel ensures CheckSynchronize fires promptly. }
    waited := 0;
    while (not task.CompleteCalled) and (waited < 2000) do
    begin
      CheckSynchronize(10);
      Sleep(10);
      Inc(waited, 10);
    end;
    AssertTrue('OnComplete should fire via Start path', task.CompleteCalled);
    AssertEquals('Sum 1..100 = 5050 via threaded path', 5050, task.SumResult);
  finally
    task.Free;
  end;
end;

procedure TTestAsyncTask.TestStart_ProgressDeliveredViaThread;
var
  task: TTestSumTask;
  waited: Integer;
begin
  task := TTestSumTask.Create(1, 50);
  try
    task.Start;
    waited := 0;
    while (not task.CompleteCalled) and (waited < 2000) do
    begin
      CheckSynchronize(10);
      Sleep(10);
      Inc(waited, 10);
    end;
    AssertTrue('Task should have completed', task.CompleteCalled);
    AssertEquals('Should have 5 progress lines via thread', 5, task.ProgressLines.Count);
  finally
    task.Free;
  end;
end;

procedure TTestAsyncTask.TestStart_ErrorFiredOnMainThread;
var
  task: TTestFailingTask;
  waited: Integer;
begin
  task := TTestFailingTask.Create;
  try
    task.Start;
    waited := 0;
    while (task.ErrorMessage = '') and (not task.CompleteCalled) and (waited < 2000) do
    begin
      CheckSynchronize(10);
      Sleep(10);
      Inc(waited, 10);
    end;
    AssertFalse('OnComplete should NOT fire on error via Start', task.CompleteCalled);
    AssertTrue('OnError should fire via Start path',
      Pos('deliberate test failure', task.ErrorMessage) > 0);
  finally
    task.Free;
  end;
end;


{ TTestInvokeLater }

var
  GInvokeResult: Integer;

procedure SetInvokeResult;
begin
  GInvokeResult := 42;
end;

procedure TTestInvokeLater.TestInvokeLater_ExecutesOnMainThread;
begin
  GInvokeResult := 0;
  fpgInvokeLater(@SetInvokeResult);
  { Since we're on the main thread, process pending messages to
    trigger the invoke }
  fpgApplication.ProcessMessages;
  AssertEquals('fpgInvokeLater should have executed the proc', 42, GInvokeResult);
end;


initialization
  RegisterTests;

end.
