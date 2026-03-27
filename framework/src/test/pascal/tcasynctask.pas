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

  { TTestAsyncTask }

  TTestAsyncTask = class(TTestCase)
  published
    procedure TestAsyncTask_ExecutesAndCompletes;
    procedure TestAsyncTask_PublishProgressDelivered;
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


{ TTestAsyncTask }

procedure TTestAsyncTask.TestAsyncTask_ExecutesAndCompletes;
var
  task: TTestSumTask;
begin
  task := TTestSumTask.Create(1, 100);
  try
    { Run synchronously for testing }
    task.RunSynchronous;
    AssertTrue('OnComplete should have been called', task.CompleteCalled);
    AssertEquals('Sum 1..100 = 5050', 5050, task.SumResult);
    AssertEquals('No errors', '', task.ErrorMessage);
  finally
    task.Free;
  end;
end;

procedure TTestAsyncTask.TestAsyncTask_PublishProgressDelivered;
var
  task: TTestSumTask;
begin
  task := TTestSumTask.Create(1, 50);
  try
    task.RunSynchronous;
    { Progress published at every multiple of 10: 10, 20, 30, 40, 50 }
    AssertEquals('Should have 5 progress lines', 5, task.ProgressLines.Count);
    AssertTrue('First progress line should mention 10',
      Pos('10', task.ProgressLines[0]) > 0);
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
