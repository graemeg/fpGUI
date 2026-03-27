unit tcmsgqueue;

{$mode objfpc}{$h+}

interface

uses
  Classes,
  SysUtils,
  fpcunit, testutils, testregistry,
  fpg_base,
  fpg_main,
  fpg_msgqueue_ring;

type

  TTestRingBuffer = class(TTestCase)
  published
    procedure TestCreate_HasCorrectCapacity;
    procedure TestEmpty_AfterCreate;
    procedure TestEnqueueDequeue_SingleItem;
    procedure TestEnqueueDequeue_MultipleItems_FIFO;
    procedure TestEnqueueDequeue_FillToCapacity;
    procedure TestEnqueue_WhenFull_ReturnsFalse;
    procedure TestDequeue_WhenEmpty_ReturnsFalse;
    procedure TestWrapAround;
    procedure TestCount_TracksCorrectly;
    procedure TestClear;
  end;

  TTestRingBufferThreadSafety = class(TTestCase)
  private
    FBuffer: TfpgMessageRingBuffer;
    FProducerErrors: Integer;
    FConsumedCount: Integer;
  published
    procedure TestConcurrentProducersSingleConsumer;
  end;


procedure RegisterTests;


implementation

uses
  SyncObjs;


procedure RegisterTests;
begin
  RegisterTest(TTestRingBuffer);
  RegisterTest(TTestRingBufferThreadSafety);
end;


{ TTestRingBuffer }

procedure TTestRingBuffer.TestCreate_HasCorrectCapacity;
var
  buf: TfpgMessageRingBuffer;
begin
  buf := TfpgMessageRingBuffer.Create(64);
  try
    { Ring buffer rounds up to next power of two }
    AssertTrue('Capacity must be >= 64', buf.Capacity >= 64);
  finally
    buf.Free;
  end;
end;

procedure TTestRingBuffer.TestEmpty_AfterCreate;
var
  buf: TfpgMessageRingBuffer;
begin
  buf := TfpgMessageRingBuffer.Create(16);
  try
    AssertEquals('New buffer should have count 0', 0, buf.Count);
    AssertTrue('New buffer should report empty', buf.IsEmpty);
  finally
    buf.Free;
  end;
end;

procedure TTestRingBuffer.TestEnqueueDequeue_SingleItem;
var
  buf: TfpgMessageRingBuffer;
  msg, out_msg: TfpgMessageRec;
  ok: Boolean;
begin
  buf := TfpgMessageRingBuffer.Create(16);
  try
    FillChar(msg, SizeOf(msg), 0);
    msg.MsgCode := FPGM_PAINT;
    msg.Sender := nil;
    msg.Dest := nil;

    ok := buf.Enqueue(msg);
    AssertTrue('Enqueue should succeed', ok);
    AssertEquals('Count should be 1 after enqueue', 1, buf.Count);

    ok := buf.Dequeue(out_msg);
    AssertTrue('Dequeue should succeed', ok);
    AssertEquals('Dequeued message code should match', FPGM_PAINT, out_msg.MsgCode);
    AssertEquals('Count should be 0 after dequeue', 0, buf.Count);
  finally
    buf.Free;
  end;
end;

procedure TTestRingBuffer.TestEnqueueDequeue_MultipleItems_FIFO;
var
  buf: TfpgMessageRingBuffer;
  msg, out_msg: TfpgMessageRec;
  i: Integer;
begin
  buf := TfpgMessageRingBuffer.Create(16);
  try
    { Enqueue 5 messages with different codes }
    for i := 1 to 5 do
    begin
      FillChar(msg, SizeOf(msg), 0);
      msg.MsgCode := i * 10;
      buf.Enqueue(msg);
    end;
    AssertEquals('Count should be 5', 5, buf.Count);

    { Dequeue and verify FIFO order }
    for i := 1 to 5 do
    begin
      buf.Dequeue(out_msg);
      AssertEquals('FIFO order must be preserved', i * 10, out_msg.MsgCode);
    end;
    AssertTrue('Buffer should be empty after draining', buf.IsEmpty);
  finally
    buf.Free;
  end;
end;

procedure TTestRingBuffer.TestEnqueueDequeue_FillToCapacity;
var
  buf: TfpgMessageRingBuffer;
  msg, out_msg: TfpgMessageRec;
  i: Integer;
  cap: Integer;
begin
  buf := TfpgMessageRingBuffer.Create(32);
  try
    cap := buf.Capacity;
    FillChar(msg, SizeOf(msg), 0);
    for i := 0 to cap - 1 do
    begin
      msg.MsgCode := i;
      AssertTrue('Enqueue should succeed up to capacity', buf.Enqueue(msg));
    end;
    AssertEquals('Count should equal capacity', cap, buf.Count);

    { Verify all items dequeue in order }
    for i := 0 to cap - 1 do
    begin
      AssertTrue('Dequeue should succeed', buf.Dequeue(out_msg));
      AssertEquals('FIFO order at capacity', i, out_msg.MsgCode);
    end;
  finally
    buf.Free;
  end;
end;

procedure TTestRingBuffer.TestEnqueue_WhenFull_ReturnsFalse;
var
  buf: TfpgMessageRingBuffer;
  msg: TfpgMessageRec;
  i: Integer;
  cap: Integer;
begin
  buf := TfpgMessageRingBuffer.Create(16);
  try
    cap := buf.Capacity;
    FillChar(msg, SizeOf(msg), 0);
    for i := 0 to cap - 1 do
      buf.Enqueue(msg);

    { Buffer is now full }
    AssertFalse('Enqueue on full buffer should return false', buf.Enqueue(msg));
  finally
    buf.Free;
  end;
end;

procedure TTestRingBuffer.TestDequeue_WhenEmpty_ReturnsFalse;
var
  buf: TfpgMessageRingBuffer;
  out_msg: TfpgMessageRec;
begin
  buf := TfpgMessageRingBuffer.Create(16);
  try
    AssertFalse('Dequeue on empty buffer should return false', buf.Dequeue(out_msg));
  finally
    buf.Free;
  end;
end;

procedure TTestRingBuffer.TestWrapAround;
var
  buf: TfpgMessageRingBuffer;
  msg, out_msg: TfpgMessageRec;
  i: Integer;
  cap: Integer;
begin
  buf := TfpgMessageRingBuffer.Create(8);
  try
    cap := buf.Capacity;
    FillChar(msg, SizeOf(msg), 0);

    { Fill to capacity, drain half, refill, verify order }
    for i := 0 to cap - 1 do
    begin
      msg.MsgCode := i;
      buf.Enqueue(msg);
    end;

    { Drain half }
    for i := 0 to (cap div 2) - 1 do
    begin
      buf.Dequeue(out_msg);
      AssertEquals('First half dequeue order', i, out_msg.MsgCode);
    end;

    { Enqueue more — these wrap around the ring }
    for i := 0 to (cap div 2) - 1 do
    begin
      msg.MsgCode := 1000 + i;
      AssertTrue('Enqueue after partial drain should succeed', buf.Enqueue(msg));
    end;

    { Dequeue remaining original items }
    for i := (cap div 2) to cap - 1 do
    begin
      buf.Dequeue(out_msg);
      AssertEquals('Remaining original items', i, out_msg.MsgCode);
    end;

    { Dequeue wrapped items }
    for i := 0 to (cap div 2) - 1 do
    begin
      buf.Dequeue(out_msg);
      AssertEquals('Wrapped items', 1000 + i, out_msg.MsgCode);
    end;

    AssertTrue('Buffer should be empty after full drain', buf.IsEmpty);
  finally
    buf.Free;
  end;
end;

procedure TTestRingBuffer.TestCount_TracksCorrectly;
var
  buf: TfpgMessageRingBuffer;
  msg, out_msg: TfpgMessageRec;
begin
  buf := TfpgMessageRingBuffer.Create(16);
  try
    FillChar(msg, SizeOf(msg), 0);
    AssertEquals(0, buf.Count);

    buf.Enqueue(msg);
    AssertEquals(1, buf.Count);

    buf.Enqueue(msg);
    AssertEquals(2, buf.Count);

    buf.Dequeue(out_msg);
    AssertEquals(1, buf.Count);

    buf.Dequeue(out_msg);
    AssertEquals(0, buf.Count);
  finally
    buf.Free;
  end;
end;

procedure TTestRingBuffer.TestClear;
var
  buf: TfpgMessageRingBuffer;
  msg: TfpgMessageRec;
  i: Integer;
begin
  buf := TfpgMessageRingBuffer.Create(16);
  try
    FillChar(msg, SizeOf(msg), 0);
    for i := 1 to 5 do
      buf.Enqueue(msg);

    AssertEquals(5, buf.Count);
    buf.Clear;
    AssertEquals('Count should be 0 after Clear', 0, buf.Count);
    AssertTrue('Buffer should be empty after Clear', buf.IsEmpty);
  finally
    buf.Free;
  end;
end;


{ TTestRingBufferThreadSafety }

type
  TProducerThread = class(TThread)
  private
    FBuffer: TfpgMessageRingBuffer;
    FStartCode: Integer;
    FCount: Integer;
    FErrors: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ABuf: TfpgMessageRingBuffer; AStartCode, ACount: Integer);
    property Errors: Integer read FErrors;
  end;

constructor TProducerThread.Create(ABuf: TfpgMessageRingBuffer; AStartCode, ACount: Integer);
begin
  inherited Create(True);
  FBuffer := ABuf;
  FStartCode := AStartCode;
  FCount := ACount;
  FErrors := 0;
  FreeOnTerminate := False;
end;

procedure TProducerThread.Execute;
var
  msg: TfpgMessageRec;
  i: Integer;
begin
  FillChar(msg, SizeOf(msg), 0);
  for i := 0 to FCount - 1 do
  begin
    msg.MsgCode := FStartCode + i;
    if not FBuffer.Enqueue(msg) then
      InterlockedIncrement(FErrors);
  end;
end;

procedure TTestRingBufferThreadSafety.TestConcurrentProducersSingleConsumer;
const
  NumProducers = 4;
  ItemsPerProducer = 500;
  TotalItems = NumProducers * ItemsPerProducer;
var
  producers: array[0..NumProducers - 1] of TProducerThread;
  out_msg: TfpgMessageRec;
  i: Integer;
  totalErrors: Integer;
begin
  { Large enough buffer to hold all items }
  FBuffer := TfpgMessageRingBuffer.Create(4096);
  try
    { Create and start producers }
    for i := 0 to NumProducers - 1 do
      producers[i] := TProducerThread.Create(FBuffer, i * ItemsPerProducer, ItemsPerProducer);

    for i := 0 to NumProducers - 1 do
      producers[i].Start;

    { Wait for all producers to finish }
    for i := 0 to NumProducers - 1 do
      producers[i].WaitFor;

    { Check errors }
    totalErrors := 0;
    for i := 0 to NumProducers - 1 do
    begin
      totalErrors := totalErrors + producers[i].Errors;
      producers[i].Free;
    end;

    AssertEquals('No enqueue errors should occur', 0, totalErrors);
    AssertEquals('All items should be in the buffer', TotalItems, FBuffer.Count);

    { Drain and count — we cannot guarantee ordering across producers,
      but count must be exact }
    FConsumedCount := 0;
    while FBuffer.Dequeue(out_msg) do
      Inc(FConsumedCount);

    AssertEquals('All items should be consumed', TotalItems, FConsumedCount);
    AssertTrue('Buffer should be empty', FBuffer.IsEmpty);
  finally
    FBuffer.Free;
  end;
end;


initialization
  RegisterTests;

end.
