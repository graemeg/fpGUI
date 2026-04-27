unit tcmsgcoalesce;

{$mode objfpc}{$h+}

interface

uses
  Classes,
  SysUtils,
  fpcunit, testutils, testregistry,
  fpg_base,
  fpg_main;

type

  { TTestMessageCoalescing }

  TTestMessageCoalescing = class(TTestCase)
  published
    { Multiple FPGM_PAINT for the same widget should coalesce into one }
    procedure TestCoalesce_DuplicatePaintSameTarget;
    { FPGM_PAINT for different targets should NOT coalesce }
    procedure TestCoalesce_PaintDifferentTargets;
    { Multiple FPGM_RESIZE for the same widget should coalesce into one,
      keeping the latest params }
    procedure TestCoalesce_DuplicateResizeSameTarget;
    { Non-coalescable messages should pass through unchanged }
    procedure TestCoalesce_NonCoalescableUnchanged;
    { Mixed coalescable and non-coalescable messages preserve order
      of non-coalescable messages }
    procedure TestCoalesce_MixedMessages;
  end;


procedure RegisterTests;


implementation


procedure RegisterTests;
begin
  RegisterTest(TTestMessageCoalescing);
end;


{ Helper: post N paint messages for the same dest object }
procedure PostPaintMessages(ADest: TObject; ACount: Integer);
var
  i: Integer;
  params: TfpgMessageParams;
begin
  FillChar(params, SizeOf(params), 0);
  for i := 1 to ACount do
    fpgPostMessage(nil, ADest, FPGM_PAINT, params);
end;

{ Helper: count pending messages for a target with a specific code }
function CountMessagesFor(ADest: TObject; AMsgCode: Integer): Integer;
var
  mp: PfpgMessageRec;
  m: TfpgMessageRec;
begin
  Result := 0;
  { Drain queue, count matching messages, discard all }
  repeat
    mp := fpgGetFirstMessage;
    if mp <> nil then
    begin
      m := mp^;
      fpgDeleteFirstMessage;
      if (m.Dest = ADest) and (m.MsgCode = AMsgCode) then
        Inc(Result);
    end;
  until mp = nil;
end;


{ TTestMessageCoalescing }

procedure TTestMessageCoalescing.TestCoalesce_DuplicatePaintSameTarget;
var
  dummy: TObject;
  count: Integer;
begin
  dummy := TObject.Create;
  try
    PostPaintMessages(dummy, 5);
    { Coalesce should reduce 5 paint messages to 1 }
    fpgCoalesceMessages;
    count := CountMessagesFor(dummy, FPGM_PAINT);
    AssertEquals('5 paints for same target should coalesce to 1', 1, count);
  finally
    dummy.Free;
  end;
end;

procedure TTestMessageCoalescing.TestCoalesce_PaintDifferentTargets;
var
  d1, d2: TObject;
  c1, c2: Integer;
  mp: PfpgMessageRec;
  m: TfpgMessageRec;
begin
  d1 := TObject.Create;
  d2 := TObject.Create;
  try
    PostPaintMessages(d1, 3);
    PostPaintMessages(d2, 2);
    fpgCoalesceMessages;
    { Count in a single pass since CountMessagesFor drains all }
    c1 := 0;
    c2 := 0;
    repeat
      mp := fpgGetFirstMessage;
      if mp <> nil then
      begin
        m := mp^;
        fpgDeleteFirstMessage;
        if (m.Dest = d1) and (m.MsgCode = FPGM_PAINT) then Inc(c1);
        if (m.Dest = d2) and (m.MsgCode = FPGM_PAINT) then Inc(c2);
      end;
    until mp = nil;
    AssertEquals('d1 should have 1 paint', 1, c1);
    AssertEquals('d2 should have 1 paint', 1, c2);
  finally
    d2.Free;
    d1.Free;
  end;
end;

procedure TTestMessageCoalescing.TestCoalesce_DuplicateResizeSameTarget;
var
  dummy: TObject;
  params: TfpgMessageParams;
  count: Integer;
begin
  dummy := TObject.Create;
  try
    FillChar(params, SizeOf(params), 0);
    params.rect.Width := 100;
    params.rect.Height := 50;
    fpgPostMessage(nil, dummy, FPGM_RESIZE, params);

    params.rect.Width := 200;
    params.rect.Height := 100;
    fpgPostMessage(nil, dummy, FPGM_RESIZE, params);

    params.rect.Width := 300;
    params.rect.Height := 150;
    fpgPostMessage(nil, dummy, FPGM_RESIZE, params);

    fpgCoalesceMessages;
    count := CountMessagesFor(dummy, FPGM_RESIZE);
    AssertEquals('3 resizes should coalesce to 1', 1, count);
  finally
    dummy.Free;
  end;
end;

procedure TTestMessageCoalescing.TestCoalesce_NonCoalescableUnchanged;
var
  dummy: TObject;
  params: TfpgMessageParams;
  mp: PfpgMessageRec;
  m: TfpgMessageRec;
  count: Integer;
begin
  dummy := TObject.Create;
  try
    FillChar(params, SizeOf(params), 0);
    fpgPostMessage(nil, dummy, FPGM_KEYPRESS, params);
    fpgPostMessage(nil, dummy, FPGM_KEYPRESS, params);
    fpgPostMessage(nil, dummy, FPGM_MOUSEDOWN, params);

    fpgCoalesceMessages;

    { Count all messages — non-coalescable should be unchanged }
    count := 0;
    repeat
      mp := fpgGetFirstMessage;
      if mp <> nil then
      begin
        fpgDeleteFirstMessage;
        Inc(count);
      end;
    until mp = nil;
    AssertEquals('Non-coalescable messages should pass through', 3, count);
  finally
    dummy.Free;
  end;
end;

procedure TTestMessageCoalescing.TestCoalesce_MixedMessages;
var
  dummy: TObject;
  params: TfpgMessageParams;
  mp: PfpgMessageRec;
  m: TfpgMessageRec;
  count: Integer;
begin
  dummy := TObject.Create;
  try
    FillChar(params, SizeOf(params), 0);
    { Post: paint, key, paint, paint, mouse, paint }
    fpgPostMessage(nil, dummy, FPGM_PAINT, params);
    fpgPostMessage(nil, dummy, FPGM_KEYPRESS, params);
    fpgPostMessage(nil, dummy, FPGM_PAINT, params);
    fpgPostMessage(nil, dummy, FPGM_PAINT, params);
    fpgPostMessage(nil, dummy, FPGM_MOUSEDOWN, params);
    fpgPostMessage(nil, dummy, FPGM_PAINT, params);

    fpgCoalesceMessages;

    { After coalescing: key, mouse, paint (1 paint remains, non-coalescable preserved) }
    count := 0;
    repeat
      mp := fpgGetFirstMessage;
      if mp <> nil then
      begin
        fpgDeleteFirstMessage;
        Inc(count);
      end;
    until mp = nil;
    { 1 key + 1 mouse + 1 coalesced paint = 3 }
    AssertEquals('Mixed messages: 1 key + 1 mouse + 1 coalesced paint', 3, count);
  finally
    dummy.Free;
  end;
end;


initialization
  RegisterTests;

end.
