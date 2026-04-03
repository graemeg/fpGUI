{
    fpGUI IDE - Breakpoint Tests

    Tests for TBreakpointList — toggle, query, persistence, and handle
    management.
}
unit ide.test.breakpoint;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  ide.breakpoint;

type

  { TTestBreakpointList }

  TTestBreakpointList = class(TTestCase)
  private
    FBP: TBreakpointList;
    FTempFile: String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Toggle }
    procedure TestToggleAddsBreakpoint;
    procedure TestToggleExistingRemovesIt;
    procedure TestToggleTwiceResultsInNoBreakpoint;
    procedure TestToggleTwiceCountIsZero;

    { HasBreakpoint / FindIndex }
    procedure TestHasBreakpointFalseOnEmpty;
    procedure TestHasBreakpointTrueAfterAdd;
    procedure TestHasBreakpointFalseAfterRemove;
    procedure TestFindIndexMinusOneWhenMissing;
    procedure TestFindIndexCorrectAfterAdd;

    { Count }
    procedure TestCountZeroOnEmpty;
    procedure TestCountOneAfterAdd;
    procedure TestCountZeroAfterRemove;
    procedure TestCountTwoAfterTwoAdds;

    { GetItem }
    procedure TestGetItemFileName;
    procedure TestGetItemLine;
    procedure TestGetItemEnabledByDefault;
    procedure TestGetItemHandleMinusOne;

    { Filename matching — basename only }
    procedure TestMatchByBasename;
    procedure TestNoMatchDifferentBasename;

    { Multiple breakpoints }
    procedure TestTwoBreakpointsSameFile;
    procedure TestTwoBreakpointsDifferentFiles;
    procedure TestRemoveFirstKeepsSecond;
    procedure TestRemoveSecondKeepsFirst;

    { Handle management }
    procedure TestSetHandle;
    procedure TestClearHandles;

    { Clear }
    procedure TestClearResetsCount;
    procedure TestClearAllowsReAdd;

    { Persistence }
    procedure TestSaveAndLoadRoundTrip;
    procedure TestLoadNonExistentFileIsNoError;
    procedure TestSaveEmptyList;
    procedure TestLoadIgnoresInvalidEntries;
  end;

implementation

{ TTestBreakpointList }

procedure TTestBreakpointList.SetUp;
begin
  FBP := TBreakpointList.Create;
  FTempFile := GetTempFileName + '.json';
end;

procedure TTestBreakpointList.TearDown;
begin
  FBP.Free;
  if FileExists(FTempFile) then
    DeleteFile(FTempFile);
end;

{ Toggle }

procedure TTestBreakpointList.TestToggleAddsBreakpoint;
begin
  FBP.Toggle('/src/foo.pas', 10);
  AssertEquals('count after add', 1, FBP.Count);
end;

procedure TTestBreakpointList.TestToggleExistingRemovesIt;
begin
  FBP.Toggle('/src/foo.pas', 10);
  FBP.Toggle('/src/foo.pas', 10);
  AssertEquals('count after remove', 0, FBP.Count);
end;

procedure TTestBreakpointList.TestToggleTwiceResultsInNoBreakpoint;
begin
  FBP.Toggle('/src/foo.pas', 10);
  FBP.Toggle('/src/foo.pas', 10);
  AssertFalse('no breakpoint after double-toggle',
    FBP.HasBreakpoint('/src/foo.pas', 10));
end;

procedure TTestBreakpointList.TestToggleTwiceCountIsZero;
begin
  FBP.Toggle('/src/foo.pas', 10);
  FBP.Toggle('/src/foo.pas', 10);
  AssertEquals('count zero after double-toggle', 0, FBP.Count);
end;

{ HasBreakpoint / FindIndex }

procedure TTestBreakpointList.TestHasBreakpointFalseOnEmpty;
begin
  AssertFalse('empty list', FBP.HasBreakpoint('/src/foo.pas', 1));
end;

procedure TTestBreakpointList.TestHasBreakpointTrueAfterAdd;
begin
  FBP.Toggle('/src/foo.pas', 42);
  AssertTrue('has breakpoint', FBP.HasBreakpoint('/src/foo.pas', 42));
end;

procedure TTestBreakpointList.TestHasBreakpointFalseAfterRemove;
begin
  FBP.Toggle('/src/foo.pas', 42);
  FBP.Toggle('/src/foo.pas', 42);
  AssertFalse('no breakpoint after remove', FBP.HasBreakpoint('/src/foo.pas', 42));
end;

procedure TTestBreakpointList.TestFindIndexMinusOneWhenMissing;
begin
  AssertEquals('-1 when missing', -1, FBP.FindIndex('/src/foo.pas', 1));
end;

procedure TTestBreakpointList.TestFindIndexCorrectAfterAdd;
begin
  FBP.Toggle('/src/foo.pas', 10);
  AssertEquals('index 0', 0, FBP.FindIndex('/src/foo.pas', 10));
end;

{ Count }

procedure TTestBreakpointList.TestCountZeroOnEmpty;
begin
  AssertEquals('zero on empty', 0, FBP.Count);
end;

procedure TTestBreakpointList.TestCountOneAfterAdd;
begin
  FBP.Toggle('/src/foo.pas', 10);
  AssertEquals('one after add', 1, FBP.Count);
end;

procedure TTestBreakpointList.TestCountZeroAfterRemove;
begin
  FBP.Toggle('/src/foo.pas', 10);
  FBP.Toggle('/src/foo.pas', 10);
  AssertEquals('zero after remove', 0, FBP.Count);
end;

procedure TTestBreakpointList.TestCountTwoAfterTwoAdds;
begin
  FBP.Toggle('/src/foo.pas', 10);
  FBP.Toggle('/src/foo.pas', 20);
  AssertEquals('two after two adds', 2, FBP.Count);
end;

{ GetItem }

procedure TTestBreakpointList.TestGetItemFileName;
begin
  FBP.Toggle('/src/foo.pas', 10);
  AssertEquals('filename', '/src/foo.pas', FBP.GetItem(0).FileName);
end;

procedure TTestBreakpointList.TestGetItemLine;
begin
  FBP.Toggle('/src/foo.pas', 10);
  AssertEquals('line', 10, FBP.GetItem(0).Line);
end;

procedure TTestBreakpointList.TestGetItemEnabledByDefault;
begin
  FBP.Toggle('/src/foo.pas', 10);
  AssertTrue('enabled by default', FBP.GetItem(0).Enabled);
end;

procedure TTestBreakpointList.TestGetItemHandleMinusOne;
begin
  FBP.Toggle('/src/foo.pas', 10);
  AssertEquals('handle -1 by default', -1, FBP.GetItem(0).Handle);
end;

{ Filename matching — basename only }

procedure TTestBreakpointList.TestMatchByBasename;
begin
  FBP.Toggle('/project/src/foo.pas', 10);
  AssertTrue('basename match from different path',
    FBP.HasBreakpoint('/other/path/foo.pas', 10));
end;

procedure TTestBreakpointList.TestNoMatchDifferentBasename;
begin
  FBP.Toggle('/src/foo.pas', 10);
  AssertFalse('no match for different basename',
    FBP.HasBreakpoint('/src/bar.pas', 10));
end;

{ Multiple breakpoints }

procedure TTestBreakpointList.TestTwoBreakpointsSameFile;
begin
  FBP.Toggle('/src/foo.pas', 10);
  FBP.Toggle('/src/foo.pas', 20);
  AssertTrue('line 10', FBP.HasBreakpoint('/src/foo.pas', 10));
  AssertTrue('line 20', FBP.HasBreakpoint('/src/foo.pas', 20));
end;

procedure TTestBreakpointList.TestTwoBreakpointsDifferentFiles;
begin
  FBP.Toggle('/src/foo.pas', 10);
  FBP.Toggle('/src/bar.pas', 10);
  AssertTrue('foo.pas', FBP.HasBreakpoint('/src/foo.pas', 10));
  AssertTrue('bar.pas', FBP.HasBreakpoint('/src/bar.pas', 10));
end;

procedure TTestBreakpointList.TestRemoveFirstKeepsSecond;
begin
  FBP.Toggle('/src/foo.pas', 10);
  FBP.Toggle('/src/foo.pas', 20);
  FBP.Toggle('/src/foo.pas', 10);  { remove first }
  AssertEquals('count 1', 1, FBP.Count);
  AssertTrue('line 20 still present', FBP.HasBreakpoint('/src/foo.pas', 20));
end;

procedure TTestBreakpointList.TestRemoveSecondKeepsFirst;
begin
  FBP.Toggle('/src/foo.pas', 10);
  FBP.Toggle('/src/foo.pas', 20);
  FBP.Toggle('/src/foo.pas', 20);  { remove second }
  AssertEquals('count 1', 1, FBP.Count);
  AssertTrue('line 10 still present', FBP.HasBreakpoint('/src/foo.pas', 10));
end;

{ Handle management }

procedure TTestBreakpointList.TestSetHandle;
begin
  FBP.Toggle('/src/foo.pas', 10);
  FBP.SetHandle(0, 42);
  AssertEquals('handle updated', 42, FBP.GetItem(0).Handle);
end;

procedure TTestBreakpointList.TestClearHandles;
begin
  FBP.Toggle('/src/foo.pas', 10);
  FBP.Toggle('/src/foo.pas', 20);
  FBP.SetHandle(0, 1);
  FBP.SetHandle(1, 2);
  FBP.ClearHandles;
  AssertEquals('handle 0 cleared', -1, FBP.GetItem(0).Handle);
  AssertEquals('handle 1 cleared', -1, FBP.GetItem(1).Handle);
end;

{ Clear }

procedure TTestBreakpointList.TestClearResetsCount;
begin
  FBP.Toggle('/src/foo.pas', 10);
  FBP.Toggle('/src/foo.pas', 20);
  FBP.Clear;
  AssertEquals('count zero after clear', 0, FBP.Count);
end;

procedure TTestBreakpointList.TestClearAllowsReAdd;
begin
  FBP.Toggle('/src/foo.pas', 10);
  FBP.Clear;
  FBP.Toggle('/src/foo.pas', 10);
  AssertEquals('count 1 after re-add', 1, FBP.Count);
end;

{ Persistence }

procedure TTestBreakpointList.TestSaveAndLoadRoundTrip;
var
  BPLoad: TBreakpointList;
begin
  FBP.Toggle('/src/foo.pas', 10);
  FBP.Toggle('/src/bar.pas', 42);
  FBP.SaveToFile(FTempFile);

  BPLoad := TBreakpointList.Create;
  try
    BPLoad.LoadFromFile(FTempFile);
    AssertEquals('count', 2, BPLoad.Count);
    AssertTrue('foo.pas:10', BPLoad.HasBreakpoint('/src/foo.pas', 10));
    AssertTrue('bar.pas:42', BPLoad.HasBreakpoint('/src/bar.pas', 42));
  finally
    BPLoad.Free;
  end;
end;

procedure TTestBreakpointList.TestLoadNonExistentFileIsNoError;
begin
  FBP.LoadFromFile('/nonexistent/path/breakpoints.json');
  AssertEquals('count zero', 0, FBP.Count);
end;

procedure TTestBreakpointList.TestSaveEmptyList;
var
  BPLoad: TBreakpointList;
begin
  FBP.SaveToFile(FTempFile);
  BPLoad := TBreakpointList.Create;
  try
    BPLoad.LoadFromFile(FTempFile);
    AssertEquals('count zero after loading empty', 0, BPLoad.Count);
  finally
    BPLoad.Free;
  end;
end;

procedure TTestBreakpointList.TestLoadIgnoresInvalidEntries;
var
  FS: TFileStream;
  S: String;
  BPLoad: TBreakpointList;
begin
  { Write JSON with one valid and one invalid (no file) entry }
  S := '[{"file":"/src/foo.pas","line":10,"enabled":true},{"line":5}]';
  FS := TFileStream.Create(FTempFile, fmCreate);
  try
    FS.Write(S[1], Length(S));
  finally
    FS.Free;
  end;

  BPLoad := TBreakpointList.Create;
  try
    BPLoad.LoadFromFile(FTempFile);
    AssertEquals('only valid entry loaded', 1, BPLoad.Count);
    AssertTrue('foo.pas:10 present', BPLoad.HasBreakpoint('/src/foo.pas', 10));
  finally
    BPLoad.Free;
  end;
end;


initialization
  RegisterTest(TTestBreakpointList);

end.
