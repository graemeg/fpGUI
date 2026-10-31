unit tcsearchtable;

{$mode objfpc}{$H+}

{ Tests for TSearchTable, the decoder of the full text search (FTS) records
  in INF files (GitHub issue #205).

  Bugs in the original implementation:
  - Topic indices from the FTS data were never checked against the topic
    count, so corrupt data (eg: produced by wipfc's RLE encoder) wrote past
    the end of the Results array and corrupted the heap.
  - The RLE decoder and the topic list formats read past the end of the
    current record when a block was truncated.

  Each test builds a single word record in memory. The Results array is
  allocated with extra guard entries after the last topic, which must
  remain untouched. Bytes placed after the record would set topic flags
  if the decoder read beyond the end of the record. }

interface

uses
  Classes,
  SysUtils,
  fpcunit, testregistry,
  IPFFileFormatUnit,
  SearchTable;

const
  cGuardCount = 32;
  cGuardValue = $DEADBEEF;

type
  TTestSearchTable = class(TTestCase)
  private
    FBuffer: array of byte;
    FResults: PLongWord;
    FTopicCount: integer;
    procedure RunSearch(ACode: byte; const AData: array of byte;
        const ATrailing: array of byte; ATopicCount: integer);
    function TopicValue(AIndex: integer): longword;
    procedure CheckGuardsIntact;
    procedure CheckTopicsSet(const ATopics: array of integer);
  protected
    procedure TearDown; override;
  published
    { Correct data still decodes as before. }
    procedure TestNotUsedClearsAll;
    procedure TestUsedEverywhereSetsAll;
    procedure TestRLEDecodesRepeatAndLiteral;
    procedure TestTopicListDecodes;
    procedure TestTruncatedBitstringDecodes;
    procedure TestOffsetBitstringDecodes;

    { Topic indices beyond the topic count must be ignored. }
    procedure TestRLERepeatBeyondTopicCount;
    procedure TestRLELiteralBeyondTopicCount;
    procedure TestRLEEscapeRepeatBeyondTopicCount;
    procedure TestTopicListBeyondTopicCount;
    procedure TestTopicExclusionListBeyondTopicCount;
    procedure TestTruncatedBitstringBeyondTopicCount;
    procedure TestOffsetBitstringBeyondTopicCount;

    { Decoding must never read beyond the end of the record. }
    procedure TestRLELiteralStopsAtRecordEnd;
    procedure TestRLERepeatStopsAtRecordEnd;
    procedure TestRLEEscapeStopsAtRecordEnd;
    procedure TestTopicListIgnoresOddTrailingByte;
    procedure TestTopicExclusionListIgnoresOddTrailingByte;
  end;

implementation

{ TTestSearchTable }

procedure TTestSearchTable.TearDown;
begin
  if FResults <> nil then
    FreeMem(FResults);
  FResults := nil;
  inherited TearDown;
end;

{ Builds a record with 8 bit record length: [length][code][data], followed
  by ATrailing, which is not part of the record. Then searches for it. }
procedure TTestSearchTable.RunSearch(ACode: byte; const AData: array of byte;
    const ATrailing: array of byte; ATopicCount: integer);
var
  lTable: TSearchTable;
  i: integer;
begin
  SetLength(FBuffer, 2 + Length(AData) + Length(ATrailing));
  FBuffer[0] := 2 + Length(AData);
  FBuffer[1] := ACode;
  for i := 0 to High(AData) do
    FBuffer[2 + i] := AData[i];
  for i := 0 to High(ATrailing) do
    FBuffer[2 + Length(AData) + i] := ATrailing[i];

  FTopicCount := ATopicCount;
  FResults := GetMem((ATopicCount + cGuardCount) * SizeOf(longword));
  for i := 0 to ATopicCount + cGuardCount - 1 do
    FResults[i] := cGuardValue;

  lTable := TSearchTable.Create(@FBuffer[0], False, 1, ATopicCount);
  try
    lTable.Search(0, UInt32ArrayPointer(FResults));
  finally
    lTable.Free;
  end;
end;

function TTestSearchTable.TopicValue(AIndex: integer): longword;
begin
  Result := FResults[AIndex];
end;

procedure TTestSearchTable.CheckGuardsIntact;
var
  i: integer;
begin
  for i := FTopicCount to FTopicCount + cGuardCount - 1 do
    AssertEquals(Format('Guard entry %d (beyond topic count %d) was overwritten',
        [i, FTopicCount]), cGuardValue, FResults[i]);
end;

{ Checks that exactly ATopics are set, and all other topics are clear. }
procedure TTestSearchTable.CheckTopicsSet(const ATopics: array of integer);
var
  i, j: integer;
  lExpected: longword;
begin
  for i := 0 to FTopicCount - 1 do
  begin
    lExpected := 0;
    for j := 0 to High(ATopics) do
      if ATopics[j] = i then
        lExpected := 1;
    AssertEquals(Format('Topic %d', [i]), lExpected, TopicValue(i));
  end;
  CheckGuardsIntact;
end;

procedure TTestSearchTable.TestNotUsedClearsAll;
begin
  RunSearch(0, [], [], 10);
  CheckTopicsSet([]);
end;

procedure TTestSearchTable.TestUsedEverywhereSetsAll;
begin
  RunSearch(1, [], [], 10);
  CheckTopicsSet([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
end;

procedure TTestSearchTable.TestRLEDecodesRepeatAndLiteral;
begin
  // $00 $00: 1 repeat of byte $00 (topics 0-7 clear)
  // $81 $80 $01: 2 literal bytes (topic 8, topic 23)
  // $01 $C0: 2 repeats of byte $C0 (topics 24, 25, 32, 33)
  RunSearch(2, [1, $00, $00, $81, $80, $01, $01, $C0], [], 64);
  CheckTopicsSet([8, 23, 24, 25, 32, 33]);
end;

procedure TTestSearchTable.TestTopicListDecodes;
begin
  RunSearch(3, [2, 0, 7, 0], [], 10);
  CheckTopicsSet([2, 7]);
end;

procedure TTestSearchTable.TestTruncatedBitstringDecodes;
begin
  RunSearch(5, [$00, $41], [], 20);
  CheckTopicsSet([9, 15]);
end;

procedure TTestSearchTable.TestOffsetBitstringDecodes;
begin
  // starts at byte 1, ie: topic 8
  RunSearch(6, [1, 0, $81], [], 20);
  CheckTopicsSet([8, 15]);
end;

procedure TTestSearchTable.TestRLERepeatBeyondTopicCount;
begin
  // 2 repeats of $FF = topics 0-15, but there are only 10 topics
  RunSearch(2, [1, $01, $FF], [], 10);
  CheckTopicsSet([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
end;

procedure TTestSearchTable.TestRLELiteralBeyondTopicCount;
begin
  RunSearch(2, [1, $81, $00, $FF], [], 10);
  CheckTopicsSet([8, 9]);
end;

procedure TTestSearchTable.TestRLEEscapeRepeatBeyondTopicCount;
begin
  // $80 escape: 2 repeats of the 2 bytes $FF $FF = topics 0-31
  RunSearch(2, [1, $80, $01, $FF, $FF], [], 10);
  CheckTopicsSet([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
end;

procedure TTestSearchTable.TestTopicListBeyondTopicCount;
begin
  RunSearch(3, [2, 0, 13, 0], [], 10);
  CheckTopicsSet([2]);
end;

procedure TTestSearchTable.TestTopicExclusionListBeyondTopicCount;
begin
  RunSearch(4, [2, 0, 13, 0], [], 10);
  CheckTopicsSet([0, 1, 3, 4, 5, 6, 7, 8, 9]);
end;

procedure TTestSearchTable.TestTruncatedBitstringBeyondTopicCount;
begin
  RunSearch(5, [$FF, $FF], [], 10);
  CheckTopicsSet([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
end;

procedure TTestSearchTable.TestOffsetBitstringBeyondTopicCount;
begin
  RunSearch(6, [1, 0, $FF], [], 10);
  CheckTopicsSet([8, 9]);
end;

procedure TTestSearchTable.TestRLELiteralStopsAtRecordEnd;
begin
  // literal block announces 2 bytes, but the record holds only 1
  RunSearch(2, [1, $81, $00], [$FF], 64);
  CheckTopicsSet([]);
end;

procedure TTestSearchTable.TestRLERepeatStopsAtRecordEnd;
begin
  // repeat block without its data byte
  RunSearch(2, [1, $05], [$FF], 64);
  CheckTopicsSet([]);
end;

procedure TTestSearchTable.TestRLEEscapeStopsAtRecordEnd;
begin
  // $80 escape announces 2 data bytes, but the record holds only 1
  RunSearch(2, [1, $80, $01, $00], [$FF, $FF], 64);
  CheckTopicsSet([]);
end;

procedure TTestSearchTable.TestTopicListIgnoresOddTrailingByte;
begin
  // the last byte of the record is half a topic number
  RunSearch(3, [5, 0, $02], [$00], 64);
  CheckTopicsSet([5]);
end;

procedure TTestSearchTable.TestTopicExclusionListIgnoresOddTrailingByte;
var
  i: integer;
begin
  RunSearch(4, [5, 0, $02], [$00], 10);
  for i := 0 to FTopicCount - 1 do
    if i = 5 then
      AssertEquals('Topic 5', 0, TopicValue(i))
    else
      AssertEquals(Format('Topic %d', [i]), 1, TopicValue(i));
  CheckGuardsIntact;
end;

initialization
  RegisterTest(TTestSearchTable);

end.
