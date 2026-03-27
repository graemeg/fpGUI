{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Thread-safe MPSC (Multiple Producer, Single Consumer) ring buffer
      for fpGUI message records.

      The ring buffer uses a power-of-two capacity with a spinlock for
      producer serialisation. The consumer (main GUI thread) is expected
      to be the sole reader and does not contend with producers when
      reading — the spinlock is only held during Enqueue.

      This replaces the original linked-list pool allocator. Benefits:
        - Cache-friendly contiguous memory layout
        - No per-message heap allocation
        - Bounded, predictable memory usage
        - Lock-free for the single-consumer Dequeue path
}

unit fpg_msgqueue_ring;

{$mode objfpc}{$H+}

interface

uses
  fpg_base;

type

  { TfpgMessageRingBuffer }

  TfpgMessageRingBuffer = class(TObject)
  private
    FSlots: array of TfpgMessageRec;
    FCapacity: Integer;    { always a power of two }
    FMask: Integer;        { FCapacity - 1, for fast modulo }
    FHead: Integer;        { next write position (producers) }
    FTail: Integer;        { next read position (consumer) }
    FSpinLock: Integer;    { 0 = unlocked, 1 = locked }
    procedure SpinAcquire; inline;
    procedure SpinRelease; inline;
    function GetCount: Integer;
  public
    constructor Create(AMinCapacity: Integer);
    destructor Destroy; override;

    { Thread-safe: may be called from any thread.
      Returns True if the message was enqueued, False if the buffer is full. }
    function Enqueue(constref AMsg: TfpgMessageRec): Boolean;

    { NOT thread-safe: must only be called from the main GUI thread.
      Returns True if a message was dequeued, False if the buffer is empty. }
    function Dequeue(out AMsg: TfpgMessageRec): Boolean;

    { NOT thread-safe: must only be called from the main GUI thread. }
    procedure Clear;

    function IsEmpty: Boolean;
    property Count: Integer read GetCount;
    property Capacity: Integer read FCapacity;
  end;


implementation


function NextPowerOfTwo(AValue: Integer): Integer;
begin
  Result := 1;
  while Result < AValue do
    Result := Result shl 1;
end;


{ TfpgMessageRingBuffer }

constructor TfpgMessageRingBuffer.Create(AMinCapacity: Integer);
begin
  inherited Create;
  if AMinCapacity < 16 then
    AMinCapacity := 16;
  FCapacity := NextPowerOfTwo(AMinCapacity);
  FMask := FCapacity - 1;
  SetLength(FSlots, FCapacity);
  FHead := 0;
  FTail := 0;
  FSpinLock := 0;
end;

destructor TfpgMessageRingBuffer.Destroy;
begin
  SetLength(FSlots, 0);
  inherited Destroy;
end;

procedure TfpgMessageRingBuffer.SpinAcquire;
begin
  while InterlockedCompareExchange(FSpinLock, 1, 0) <> 0 do
    { spin — held for only a few nanoseconds per enqueue };
end;

procedure TfpgMessageRingBuffer.SpinRelease;
begin
  WriteBarrier;
  FSpinLock := 0;
end;

function TfpgMessageRingBuffer.GetCount: Integer;
begin
  ReadBarrier;
  Result := FHead - FTail;
end;

function TfpgMessageRingBuffer.Enqueue(constref AMsg: TfpgMessageRec): Boolean;
var
  used, pos: Integer;
begin
  SpinAcquire;
  try
    used := FHead - FTail;
    if used >= FCapacity then
    begin
      Result := False;
      Exit;
    end;
    pos := FHead and FMask;
    FSlots[pos] := AMsg;
    WriteBarrier;
    Inc(FHead);
    Result := True;
  finally
    SpinRelease;
  end;
end;

function TfpgMessageRingBuffer.Dequeue(out AMsg: TfpgMessageRec): Boolean;
var
  pos: Integer;
begin
  ReadBarrier;
  if FTail = FHead then
  begin
    Result := False;
    Exit;
  end;
  pos := FTail and FMask;
  AMsg := FSlots[pos];
  ReadBarrier;
  Inc(FTail);
  Result := True;
end;

procedure TfpgMessageRingBuffer.Clear;
begin
  FTail := FHead;
end;

function TfpgMessageRingBuffer.IsEmpty: Boolean;
begin
  ReadBarrier;
  Result := (FTail = FHead);
end;


end.
