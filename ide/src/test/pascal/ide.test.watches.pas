(*
    fpGUI IDE — Watch List Tests

    Tests for TWatchList: expression management, deduplication,
    ordering, and session round-trip.
*)
unit ide.test.watches;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, SysUtils,
  ide.watches;

type
  TWatchListTests = class(TTestCase)
  private
    FList: TWatchList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyListHasCountZero;
    procedure TestAddWatchIncreasesCount;
    procedure TestAddWatchReturnsIndex;
    procedure TestAddDuplicateReturnsExistingIndex;
    procedure TestAddDuplicateCaseSensitive;
    procedure TestExpressionByIndex;
    procedure TestExpressionOutOfRangeReturnsEmpty;
    procedure TestRemoveWatchDecreasesCount;
    procedure TestRemoveWatchShiftsEntries;
    procedure TestRemoveWatchOutOfRangeIsHarmless;
    procedure TestIndexOfFindsExistingEntry;
    procedure TestIndexOfReturnsMinus1WhenMissing;
    procedure TestIndexOfIsCaseSensitive;
    procedure TestGetExpressionsMatchesList;
    procedure TestGetExpressionsEmptyList;
    procedure TestLoadExpressions;
    procedure TestLoadExpressionsDeduplicate;
    procedure TestLoadExpressionsSkipsEmpty;
    procedure TestClearEmptiesList;
    procedure TestAddAfterClear;
  end;

implementation

procedure TWatchListTests.SetUp;
begin
  FList := TWatchList.Create;
end;

procedure TWatchListTests.TearDown;
begin
  FList.Free;
end;

procedure TWatchListTests.TestEmptyListHasCountZero;
begin
  AssertEquals('Empty list count', 0, FList.Count);
end;

procedure TWatchListTests.TestAddWatchIncreasesCount;
begin
  FList.AddWatch('myVar');
  AssertEquals('Count after add', 1, FList.Count);
  FList.AddWatch('otherVar');
  AssertEquals('Count after second add', 2, FList.Count);
end;

procedure TWatchListTests.TestAddWatchReturnsIndex;
var
  Idx: Integer;
begin
  Idx := FList.AddWatch('x');
  AssertEquals('First add returns 0', 0, Idx);
  Idx := FList.AddWatch('y');
  AssertEquals('Second add returns 1', 1, Idx);
end;

procedure TWatchListTests.TestAddDuplicateReturnsExistingIndex;
var
  Idx1, Idx2: Integer;
begin
  Idx1 := FList.AddWatch('SomeExpr');
  Idx2 := FList.AddWatch('SomeExpr');
  AssertEquals('Duplicate add returns same index', Idx1, Idx2);
  AssertEquals('Count unchanged after duplicate', 1, FList.Count);
end;

procedure TWatchListTests.TestAddDuplicateCaseSensitive;
var
  Idx1, Idx2: Integer;
begin
  Idx1 := FList.AddWatch('myVar');
  Idx2 := FList.AddWatch('MyVar');
  AssertTrue('Different case adds separate entry', Idx1 <> Idx2);
  AssertEquals('Two entries after case-different adds', 2, FList.Count);
end;

procedure TWatchListTests.TestExpressionByIndex;
begin
  FList.AddWatch('alpha');
  FList.AddWatch('beta');
  AssertEquals('Expression at 0', 'alpha', FList.Expression[0]);
  AssertEquals('Expression at 1', 'beta', FList.Expression[1]);
end;

procedure TWatchListTests.TestExpressionOutOfRangeReturnsEmpty;
begin
  FList.AddWatch('x');
  AssertEquals('Negative index', '', FList.Expression[-1]);
  AssertEquals('Out-of-bounds index', '', FList.Expression[99]);
end;

procedure TWatchListTests.TestRemoveWatchDecreasesCount;
begin
  FList.AddWatch('a');
  FList.AddWatch('b');
  FList.RemoveWatch(0);
  AssertEquals('Count after remove', 1, FList.Count);
end;

procedure TWatchListTests.TestRemoveWatchShiftsEntries;
begin
  FList.AddWatch('first');
  FList.AddWatch('second');
  FList.AddWatch('third');
  FList.RemoveWatch(0);
  AssertEquals('After removing 0, index 0 becomes second', 'second', FList.Expression[0]);
  AssertEquals('After removing 0, index 1 becomes third', 'third', FList.Expression[1]);
end;

procedure TWatchListTests.TestRemoveWatchOutOfRangeIsHarmless;
begin
  FList.AddWatch('x');
  FList.RemoveWatch(-1);
  FList.RemoveWatch(99);
  AssertEquals('Count unchanged after invalid removes', 1, FList.Count);
end;

procedure TWatchListTests.TestIndexOfFindsExistingEntry;
begin
  FList.AddWatch('alpha');
  FList.AddWatch('beta');
  AssertEquals('IndexOf alpha', 0, FList.IndexOf('alpha'));
  AssertEquals('IndexOf beta', 1, FList.IndexOf('beta'));
end;

procedure TWatchListTests.TestIndexOfReturnsMinus1WhenMissing;
begin
  FList.AddWatch('alpha');
  AssertEquals('IndexOf missing', -1, FList.IndexOf('gamma'));
end;

procedure TWatchListTests.TestIndexOfIsCaseSensitive;
begin
  FList.AddWatch('myVar');
  AssertEquals('Exact case found', 0, FList.IndexOf('myVar'));
  AssertEquals('Different case not found', -1, FList.IndexOf('myvar'));
  AssertEquals('Upper case not found', -1, FList.IndexOf('MYVAR'));
end;

procedure TWatchListTests.TestGetExpressionsMatchesList;
var
  Exprs: TStringArray;
begin
  FList.AddWatch('a');
  FList.AddWatch('b.field');
  FList.AddWatch('arr[0]');
  Exprs := FList.GetExpressions;
  AssertEquals('Array length', 3, Length(Exprs));
  AssertEquals('Expr 0', 'a', Exprs[0]);
  AssertEquals('Expr 1', 'b.field', Exprs[1]);
  AssertEquals('Expr 2', 'arr[0]', Exprs[2]);
end;

procedure TWatchListTests.TestGetExpressionsEmptyList;
var
  Exprs: TStringArray;
begin
  Exprs := FList.GetExpressions;
  AssertEquals('Empty list returns empty array', 0, Length(Exprs));
end;

procedure TWatchListTests.TestLoadExpressions;
var
  Exprs: TStringArray;
begin
  SetLength(Exprs, 3);
  Exprs[0] := 'x';
  Exprs[1] := 'y';
  Exprs[2] := 'z';
  FList.LoadExpressions(Exprs);
  AssertEquals('Count after load', 3, FList.Count);
  AssertEquals('Expr 0', 'x', FList.Expression[0]);
  AssertEquals('Expr 1', 'y', FList.Expression[1]);
  AssertEquals('Expr 2', 'z', FList.Expression[2]);
end;

procedure TWatchListTests.TestLoadExpressionsDeduplicate;
var
  Exprs: TStringArray;
begin
  SetLength(Exprs, 3);
  Exprs[0] := 'x';
  Exprs[1] := 'x';
  Exprs[2] := 'y';
  FList.LoadExpressions(Exprs);
  AssertEquals('Deduplicates on load', 2, FList.Count);
end;

procedure TWatchListTests.TestLoadExpressionsSkipsEmpty;
var
  Exprs: TStringArray;
begin
  SetLength(Exprs, 3);
  Exprs[0] := 'x';
  Exprs[1] := '';
  Exprs[2] := 'y';
  FList.LoadExpressions(Exprs);
  AssertEquals('Empty string skipped on load', 2, FList.Count);
end;

procedure TWatchListTests.TestClearEmptiesList;
begin
  FList.AddWatch('a');
  FList.AddWatch('b');
  FList.Clear;
  AssertEquals('Count after clear', 0, FList.Count);
end;

procedure TWatchListTests.TestAddAfterClear;
var
  Idx: Integer;
begin
  FList.AddWatch('a');
  FList.Clear;
  Idx := FList.AddWatch('a');
  AssertEquals('Add after clear returns 0', 0, Idx);
  AssertEquals('Count is 1 after add after clear', 1, FList.Count);
end;


initialization
  RegisterTest(TWatchListTests);

end.
