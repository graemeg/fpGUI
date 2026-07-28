unit tcpropertyread;

{$mode objfpc}{$h+}

interface

uses
  Classes,
  SysUtils,
  ctypes,
  fpcunit, testutils, testregistry;

type

  { The chunked property reader in fpg_x11 cannot be exercised without a live
    X server, but the arithmetic it depends on can. These tests pin down the
    two conversions that made the original code truncate:

      - XGetWindowProperty reports 'count' in units of the property format
        (8/16/32), not in bytes;
      - its read offset is counted in 32-bit words, not in bytes.

    Getting either wrong silently drops or duplicates data, which is exactly
    the failure being fixed, so the rules are worth locking down. }

  TTestPropertyArithmetic = class(TTestCase)
  published
    procedure TestFormatToBytes_Format8IsOneByte;
    procedure TestFormatToBytes_Format16IsTwoBytes;
    procedure TestFormatToBytes_Format32IsLongSized;
    procedure TestWordOffset_RoundsUpPartialWord;
    procedure TestWordOffset_ExactWordBoundary;
  end;


procedure RegisterTests;


implementation

{ Mirrors the format -> bytes conversion in ReadWholeProperty. }
function FormatToBytes(AFormat: Integer; ACount: QWord): QWord;
begin
  case AFormat of
    8:  Result := ACount;
    16: Result := ACount * 2;
    32: Result := ACount * SizeOf(culong);
  else
    Result := 0;
  end;
end;

{ Mirrors the byte -> 32-bit-word offset conversion in ReadWholeProperty. }
function BytesToWordOffset(ABytes: QWord): QWord;
begin
  Result := (ABytes + 3) div 4;
end;


procedure RegisterTests;
begin
  RegisterTest(TTestPropertyArithmetic);
end;


procedure TTestPropertyArithmetic.TestFormatToBytes_Format8IsOneByte;
begin
  AssertEquals('format 8: count is already a byte count',
      QWord(24000), FormatToBytes(8, 24000));
end;

procedure TTestPropertyArithmetic.TestFormatToBytes_Format16IsTwoBytes;
begin
  AssertEquals('format 16: each item is 2 bytes',
      QWord(200), FormatToBytes(16, 100));
end;

procedure TTestPropertyArithmetic.TestFormatToBytes_Format32IsLongSized;
begin
  { X11 hands 32-bit properties back as C longs, so this is 8 bytes per item
    on LP64 and 4 on 32-bit. Assert against SizeOf rather than a literal. }
  AssertEquals('format 32: each item is one C long',
      QWord(100 * SizeOf(culong)), FormatToBytes(32, 100));
end;

procedure TTestPropertyArithmetic.TestWordOffset_RoundsUpPartialWord;
begin
  { 4001 bytes occupies 1001 words; truncating to 1000 would re-read the
    trailing partial word and duplicate bytes. }
  AssertEquals('partial trailing word must round up',
      QWord(1001), BytesToWordOffset(4001));
end;

procedure TTestPropertyArithmetic.TestWordOffset_ExactWordBoundary;
begin
  AssertEquals('exact multiple must not round up',
      QWord(1000), BytesToWordOffset(4000));
end;


initialization
  RegisterTests;

end.
