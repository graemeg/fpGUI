{
    fpGUI IDE - Cursor History Tests

    Tests for the cursor history stack used by back/forward
    navigation (Alt+Left / Alt+Right).
}
unit ide.test.cursorhistory;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  ide.cursorhistory;

type

  { TTestCursorHistory }

  TTestCursorHistory = class(TTestCase)
  private
    FHistory: TCursorHistory;
    function MakeLoc(const AFile: string; ALine: Integer; ACol: Integer = 0): TCursorLocation;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Basic operations }
    procedure TestEmptyHistory;
    procedure TestRecordSingleLocation;
    procedure TestRecordTwoLocations;
    procedure TestGoBackReturnsCorrectLocation;
    procedure TestGoForwardAfterBack;
    procedure TestGoBackTwice;

    { Edge cases }
    procedure TestGoBackAtStart;
    procedure TestGoForwardAtEnd;
    procedure TestCanGoBackAfterGoBack;

    { Deduplication }
    procedure TestDuplicateSameFileLine;
    procedure TestDuplicateDifferentCol;
    procedure TestDifferentLine;
    procedure TestDifferentFile;

    { Forward truncation }
    procedure TestRecordAfterGoBackTruncates;
    procedure TestCanGoForwardFalseAfterTruncation;

    { Capacity }
    procedure TestCapacityOverflow;
    procedure TestCapacityOverflowPreservesNewest;

    { RecordBeforeJump }
    procedure TestRecordBeforeJumpAtTip;
    procedure TestRecordBeforeJumpWhileTraversing;
    procedure TestRecordBeforeJumpThenGoBack;
    procedure TestRecordLocationResetsTraversal;

    { IsTraversing }
    procedure TestIsTraversingInitiallyFalse;
    procedure TestIsTraversingAfterGoBack;
    procedure TestIsTraversingAfterGoForwardToTip;

    { Clear }
    procedure TestClear;
  end;


implementation

{ TTestCursorHistory }

function TTestCursorHistory.MakeLoc(const AFile: string; ALine: Integer;
  ACol: Integer): TCursorLocation;
begin
  Result.Filename := AFile;
  Result.Line := ALine;
  Result.Col := ACol;
end;

procedure TTestCursorHistory.SetUp;
begin
  FHistory := TCursorHistory.Create(50);
end;

procedure TTestCursorHistory.TearDown;
begin
  FHistory.Free;
end;

{ --- Basic operations --- }

procedure TTestCursorHistory.TestEmptyHistory;
begin
  CheckFalse(FHistory.CanGoBack, 'CanGoBack should be False for empty history');
  CheckFalse(FHistory.CanGoForward, 'CanGoForward should be False for empty history');
  CheckEquals(0, FHistory.Count, 'Count should be 0');
end;

procedure TTestCursorHistory.TestRecordSingleLocation;
begin
  FHistory.RecordLocation(MakeLoc('file1.pas', 10));
  CheckEquals(1, FHistory.Count, 'Count should be 1 after one record');
  CheckFalse(FHistory.CanGoBack, 'CanGoBack should be False with single entry');
  CheckFalse(FHistory.CanGoForward, 'CanGoForward should be False at tip');
end;

procedure TTestCursorHistory.TestRecordTwoLocations;
begin
  FHistory.RecordLocation(MakeLoc('file1.pas', 10));
  FHistory.RecordLocation(MakeLoc('file1.pas', 50));
  CheckEquals(2, FHistory.Count, 'Count should be 2');
  CheckTrue(FHistory.CanGoBack, 'CanGoBack should be True with two entries');
  CheckFalse(FHistory.CanGoForward, 'CanGoForward should be False at tip');
end;

procedure TTestCursorHistory.TestGoBackReturnsCorrectLocation;
var
  loc: TCursorLocation;
begin
  FHistory.RecordLocation(MakeLoc('file1.pas', 10));
  FHistory.RecordLocation(MakeLoc('file2.pas', 20));
  CheckTrue(FHistory.GoBack(loc), 'GoBack should succeed');
  CheckEquals('file1.pas', loc.Filename, 'GoBack should return first location');
  CheckEquals(10, loc.Line, 'GoBack should return line 10');
end;

procedure TTestCursorHistory.TestGoForwardAfterBack;
var
  loc: TCursorLocation;
begin
  FHistory.RecordLocation(MakeLoc('file1.pas', 10));
  FHistory.RecordLocation(MakeLoc('file2.pas', 20));
  FHistory.GoBack(loc);
  CheckTrue(FHistory.GoForward(loc), 'GoForward should succeed after GoBack');
  CheckEquals('file2.pas', loc.Filename, 'GoForward should return second location');
  CheckEquals(20, loc.Line, 'GoForward should return line 20');
end;

procedure TTestCursorHistory.TestGoBackTwice;
var
  loc: TCursorLocation;
begin
  FHistory.RecordLocation(MakeLoc('a.pas', 1));
  FHistory.RecordLocation(MakeLoc('b.pas', 2));
  FHistory.RecordLocation(MakeLoc('c.pas', 3));
  CheckTrue(FHistory.GoBack(loc), 'First GoBack should succeed');
  CheckEquals('b.pas', loc.Filename, 'First GoBack returns B');
  CheckTrue(FHistory.GoBack(loc), 'Second GoBack should succeed');
  CheckEquals('a.pas', loc.Filename, 'Second GoBack returns A');
end;

{ --- Edge cases --- }

procedure TTestCursorHistory.TestGoBackAtStart;
var
  loc: TCursorLocation;
begin
  FHistory.RecordLocation(MakeLoc('file1.pas', 10));
  CheckFalse(FHistory.GoBack(loc), 'GoBack should fail with single entry');
end;

procedure TTestCursorHistory.TestGoForwardAtEnd;
var
  loc: TCursorLocation;
begin
  FHistory.RecordLocation(MakeLoc('file1.pas', 10));
  FHistory.RecordLocation(MakeLoc('file2.pas', 20));
  CheckFalse(FHistory.GoForward(loc), 'GoForward should fail at tip');
end;

procedure TTestCursorHistory.TestCanGoBackAfterGoBack;
var
  loc: TCursorLocation;
begin
  FHistory.RecordLocation(MakeLoc('a.pas', 1));
  FHistory.RecordLocation(MakeLoc('b.pas', 2));
  FHistory.RecordLocation(MakeLoc('c.pas', 3));
  FHistory.GoBack(loc);  { now at B }
  CheckTrue(FHistory.CanGoBack, 'CanGoBack should be True at B (A still behind)');
  FHistory.GoBack(loc);  { now at A }
  CheckFalse(FHistory.CanGoBack, 'CanGoBack should be False at start');
end;

{ --- Deduplication --- }

procedure TTestCursorHistory.TestDuplicateSameFileLine;
begin
  FHistory.RecordLocation(MakeLoc('file1.pas', 5, 0));
  FHistory.RecordLocation(MakeLoc('file1.pas', 5, 0));
  CheckEquals(1, FHistory.Count, 'Duplicate same file+line should not add entry');
end;

procedure TTestCursorHistory.TestDuplicateDifferentCol;
begin
  FHistory.RecordLocation(MakeLoc('file1.pas', 5, 0));
  FHistory.RecordLocation(MakeLoc('file1.pas', 5, 15));
  CheckEquals(1, FHistory.Count, 'Same file+line with different col should deduplicate');
end;

procedure TTestCursorHistory.TestDifferentLine;
begin
  FHistory.RecordLocation(MakeLoc('file1.pas', 5));
  FHistory.RecordLocation(MakeLoc('file1.pas', 50));
  CheckEquals(2, FHistory.Count, 'Different line should record');
end;

procedure TTestCursorHistory.TestDifferentFile;
begin
  FHistory.RecordLocation(MakeLoc('file1.pas', 5));
  FHistory.RecordLocation(MakeLoc('file2.pas', 5));
  CheckEquals(2, FHistory.Count, 'Different file should record');
end;

{ --- Forward truncation --- }

procedure TTestCursorHistory.TestRecordAfterGoBackTruncates;
var
  loc: TCursorLocation;
begin
  FHistory.RecordLocation(MakeLoc('a.pas', 1));
  FHistory.RecordLocation(MakeLoc('b.pas', 2));
  FHistory.RecordLocation(MakeLoc('c.pas', 3));
  FHistory.GoBack(loc);  { now at B, C is forward }
  FHistory.RecordLocation(MakeLoc('d.pas', 4));  { should truncate C }
  CheckEquals(3, FHistory.Count, 'Count should be 3 (A, B, D) after truncation');
  CheckFalse(FHistory.CanGoForward, 'CanGoForward should be False after recording new location');
  CheckTrue(FHistory.GoBack(loc), 'GoBack should succeed');
  CheckEquals('b.pas', loc.Filename, 'GoBack from D should return B');
end;

procedure TTestCursorHistory.TestCanGoForwardFalseAfterTruncation;
var
  loc: TCursorLocation;
begin
  FHistory.RecordLocation(MakeLoc('a.pas', 1));
  FHistory.RecordLocation(MakeLoc('b.pas', 2));
  FHistory.GoBack(loc);
  FHistory.RecordLocation(MakeLoc('c.pas', 3));
  CheckFalse(FHistory.CanGoForward, 'CanGoForward should be False after truncation');
end;

{ --- Capacity --- }

procedure TTestCursorHistory.TestCapacityOverflow;
var
  h: TCursorHistory;
  i: Integer;
  loc: TCursorLocation;
begin
  h := TCursorHistory.Create(5);
  try
    for i := 1 to 7 do
      h.RecordLocation(MakeLoc('file.pas', i * 10));
    CheckEquals(5, h.Count, 'Count should not exceed capacity');
    { Oldest entries (line 10, 20) should be dropped. Newest is line 70. }
    CheckTrue(h.GoBack(loc), 'GoBack should succeed');
    CheckEquals(60, loc.Line, 'GoBack from line 70 should return line 60');
  finally
    h.Free;
  end;
end;

procedure TTestCursorHistory.TestCapacityOverflowPreservesNewest;
var
  h: TCursorHistory;
  i: Integer;
  loc: TCursorLocation;
  ok: Boolean;
begin
  h := TCursorHistory.Create(5);
  try
    for i := 1 to 7 do
      h.RecordLocation(MakeLoc('file.pas', i * 10));
    { Should be able to traverse back through all 5 entries: 70, 60, 50, 40, 30 }
    ok := True;
    for i := 1 to 4 do
      ok := ok and h.GoBack(loc);
    CheckTrue(ok, 'Should traverse back through 4 entries');
    CheckEquals(30, loc.Line, 'Oldest surviving entry should be line 30');
    CheckFalse(h.CanGoBack, 'Should not be able to go further back');
  finally
    h.Free;
  end;
end;

{ --- RecordBeforeJump --- }

procedure TTestCursorHistory.TestRecordBeforeJumpAtTip;
begin
  FHistory.RecordLocation(MakeLoc('a.pas', 1));
  FHistory.RecordLocation(MakeLoc('b.pas', 2));
  FHistory.RecordBeforeJump(MakeLoc('c.pas', 3));
  CheckEquals(3, FHistory.Count, 'RecordBeforeJump at tip should record (Count = 3)');
end;

procedure TTestCursorHistory.TestRecordBeforeJumpWhileTraversing;
var
  loc: TCursorLocation;
begin
  FHistory.RecordLocation(MakeLoc('a.pas', 1));
  FHistory.RecordLocation(MakeLoc('b.pas', 2));
  FHistory.RecordLocation(MakeLoc('c.pas', 3));
  FHistory.GoBack(loc);  { now traversing }
  FHistory.RecordBeforeJump(MakeLoc('x.pas', 99));
  CheckEquals(3, FHistory.Count, 'RecordBeforeJump while traversing should be no-op');
end;

procedure TTestCursorHistory.TestRecordBeforeJumpThenGoBack;
var
  loc: TCursorLocation;
begin
  FHistory.RecordLocation(MakeLoc('a.pas', 1));
  FHistory.RecordLocation(MakeLoc('b.pas', 2));
  FHistory.RecordBeforeJump(MakeLoc('c.pas', 3));
  CheckTrue(FHistory.GoBack(loc), 'GoBack from C should succeed');
  CheckEquals('b.pas', loc.Filename, 'GoBack from C should return B');
  CheckTrue(FHistory.GoBack(loc), 'GoBack from B should succeed');
  CheckEquals('a.pas', loc.Filename, 'GoBack from B should return A');
end;

procedure TTestCursorHistory.TestRecordLocationResetsTraversal;
var
  loc: TCursorLocation;
begin
  FHistory.RecordLocation(MakeLoc('a.pas', 1));
  FHistory.RecordLocation(MakeLoc('b.pas', 2));
  FHistory.GoBack(loc);  { traversing }
  CheckTrue(FHistory.IsTraversing, 'Should be traversing after GoBack');
  FHistory.RecordLocation(MakeLoc('d.pas', 4));
  CheckFalse(FHistory.IsTraversing, 'RecordLocation should reset traversal');
end;

{ --- IsTraversing --- }

procedure TTestCursorHistory.TestIsTraversingInitiallyFalse;
begin
  CheckFalse(FHistory.IsTraversing, 'IsTraversing should be False initially');
end;

procedure TTestCursorHistory.TestIsTraversingAfterGoBack;
var
  loc: TCursorLocation;
begin
  FHistory.RecordLocation(MakeLoc('a.pas', 1));
  FHistory.RecordLocation(MakeLoc('b.pas', 2));
  FHistory.GoBack(loc);
  CheckTrue(FHistory.IsTraversing, 'IsTraversing should be True after GoBack');
end;

procedure TTestCursorHistory.TestIsTraversingAfterGoForwardToTip;
var
  loc: TCursorLocation;
begin
  FHistory.RecordLocation(MakeLoc('a.pas', 1));
  FHistory.RecordLocation(MakeLoc('b.pas', 2));
  FHistory.GoBack(loc);
  FHistory.GoForward(loc);
  CheckFalse(FHistory.IsTraversing, 'IsTraversing should be False after returning to tip');
end;

{ --- Clear --- }

procedure TTestCursorHistory.TestClear;
begin
  FHistory.RecordLocation(MakeLoc('a.pas', 1));
  FHistory.RecordLocation(MakeLoc('b.pas', 2));
  FHistory.Clear;
  CheckEquals(0, FHistory.Count, 'Count should be 0 after Clear');
  CheckFalse(FHistory.CanGoBack, 'CanGoBack should be False after Clear');
  CheckFalse(FHistory.CanGoForward, 'CanGoForward should be False after Clear');
end;


initialization
  RegisterTest(TTestCursorHistory);

end.
