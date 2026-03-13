{
    fpGUI IDE — Undo/Redo System Tests

    Tests for TUndoManager and the concrete TUndoAction subclasses.
    These tests verify the undo system in isolation from the text editor
    widget, operating directly on a TStringList as the text model.
}
unit ide.test.editor.undo;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  ide.editor.undo;

type

  { TTestUndoManager }

  TTestUndoManager = class(TTestCase)
  private
    FManager: TUndoManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitialState;
    procedure TestCanUndoAfterAction;
    procedure TestCanRedoAfterUndo;
    procedure TestRedoClearedAfterNewAction;
    procedure TestUndoRestoresState;
    procedure TestRedoReappliesState;
    procedure TestMultipleUndoRedo;
    procedure TestUndoOnEmptyStackDoesNothing;
    procedure TestRedoOnEmptyStackDoesNothing;
    procedure TestMaxUndoLevels;
    procedure TestClear;
  end;

  { TTestInsertTextAction }

  TTestInsertTextAction = class(TTestCase)
  private
    FLines: TStringList;
    FManager: TUndoManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInsertSingleChar;
    procedure TestInsertAtMiddleOfLine;
    procedure TestInsertAtEndOfLine;
    procedure TestInsertIntoEmptyLine;
    procedure TestUndoInsertRemovesText;
    procedure TestRedoInsertRestoresText;
    procedure TestInsertMultiByteUTF8;
  end;

  { TTestDeleteTextAction }

  TTestDeleteTextAction = class(TTestCase)
  private
    FLines: TStringList;
    FManager: TUndoManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDeleteSingleChar;
    procedure TestDeleteAtStartOfLine;
    procedure TestUndoDeleteRestoresChar;
    procedure TestRedoDeleteRemovesAgain;
    procedure TestDeleteSelection;
    procedure TestUndoDeleteSelectionRestoresText;
  end;

  { TTestSplitLineAction }

  TTestSplitLineAction = class(TTestCase)
  private
    FLines: TStringList;
    FManager: TUndoManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSplitAtMiddle;
    procedure TestSplitAtStart;
    procedure TestSplitAtEnd;
    procedure TestSplitWithAutoIndent;
    procedure TestUndoSplitRejoinsLine;
    procedure TestRedoSplitSplitsAgain;
  end;

  { TTestJoinLinesAction }

  TTestJoinLinesAction = class(TTestCase)
  private
    FLines: TStringList;
    FManager: TUndoManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestJoinWithPreviousLine;
    procedure TestJoinWithNextLine;
    procedure TestUndoJoinRestoresTwoLines;
    procedure TestRedoJoinMergesAgain;
  end;

  { TTestCompoundAction }

  TTestCompoundAction = class(TTestCase)
  private
    FLines: TStringList;
    FManager: TUndoManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCompoundUndoesAllAtOnce;
    procedure TestCompoundRedoesAllAtOnce;
    procedure TestEmptyCompoundIsNoOp;
  end;

  { TTestActionMerging }

  TTestActionMerging = class(TTestCase)
  private
    FLines: TStringList;
    FManager: TUndoManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConsecutiveInsertsAtSamePositionMerge;
    procedure TestInsertAfterPauseDoesNotMerge;
    procedure TestDeletesDoNotMergeWithInserts;
    procedure TestConsecutiveBackspacesAtSamePositionMerge;
  end;


implementation

{ =========================================================================
    TTestUndoManager
  ========================================================================= }

procedure TTestUndoManager.SetUp;
begin
  inherited SetUp;
  FManager := TUndoManager.Create;
end;

procedure TTestUndoManager.TearDown;
begin
  FManager.Free;
  inherited TearDown;
end;

procedure TTestUndoManager.TestInitialState;
begin
  AssertFalse('New manager should not be able to undo', FManager.CanUndo);
  AssertFalse('New manager should not be able to redo', FManager.CanRedo);
  AssertEquals('Undo count should be 0', 0, FManager.UndoCount);
  AssertEquals('Redo count should be 0', 0, FManager.RedoCount);
end;

procedure TTestUndoManager.TestCanUndoAfterAction;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('hello');
    FManager.ExecuteAction(TInsertTextAction.Create(Lines, 0, 5, 'X'));
    AssertTrue('Should be able to undo after action', FManager.CanUndo);
    AssertEquals('Undo count should be 1', 1, FManager.UndoCount);
  finally
    Lines.Free;
  end;
end;

procedure TTestUndoManager.TestCanRedoAfterUndo;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('hello');
    FManager.ExecuteAction(TInsertTextAction.Create(Lines, 0, 5, 'X'));
    FManager.Undo;
    AssertTrue('Should be able to redo after undo', FManager.CanRedo);
    AssertEquals('Redo count should be 1', 1, FManager.RedoCount);
  finally
    Lines.Free;
  end;
end;

procedure TTestUndoManager.TestRedoClearedAfterNewAction;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('hello');
    FManager.ExecuteAction(TInsertTextAction.Create(Lines, 0, 5, 'X'));
    FManager.Undo;
    AssertTrue('Should have redo available', FManager.CanRedo);
    { Now perform a new action — redo stack should be cleared }
    FManager.ExecuteAction(TInsertTextAction.Create(Lines, 0, 5, 'Y'));
    AssertFalse('Redo should be cleared after new action', FManager.CanRedo);
    AssertEquals('Redo count should be 0', 0, FManager.RedoCount);
  finally
    Lines.Free;
  end;
end;

procedure TTestUndoManager.TestUndoRestoresState;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('hello');
    FManager.ExecuteAction(TInsertTextAction.Create(Lines, 0, 5, 'X'));
    AssertEquals('After execute', 'helloX', Lines[0]);
    FManager.Undo;
    AssertEquals('After undo', 'hello', Lines[0]);
  finally
    Lines.Free;
  end;
end;

procedure TTestUndoManager.TestRedoReappliesState;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('hello');
    FManager.ExecuteAction(TInsertTextAction.Create(Lines, 0, 5, 'X'));
    FManager.Undo;
    FManager.Redo;
    AssertEquals('After redo', 'helloX', Lines[0]);
  finally
    Lines.Free;
  end;
end;

procedure TTestUndoManager.TestMultipleUndoRedo;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('abc');
    FManager.ExecuteAction(TInsertTextAction.Create(Lines, 0, 3, 'D'));
    FManager.BreakMerge;
    FManager.ExecuteAction(TInsertTextAction.Create(Lines, 0, 4, 'E'));
    AssertEquals('After two inserts', 'abcDE', Lines[0]);
    AssertEquals('Undo count should be 2', 2, FManager.UndoCount);

    FManager.Undo;
    AssertEquals('After first undo', 'abcD', Lines[0]);

    FManager.Undo;
    AssertEquals('After second undo', 'abc', Lines[0]);

    FManager.Redo;
    AssertEquals('After first redo', 'abcD', Lines[0]);

    FManager.Redo;
    AssertEquals('After second redo', 'abcDE', Lines[0]);
  finally
    Lines.Free;
  end;
end;

procedure TTestUndoManager.TestUndoOnEmptyStackDoesNothing;
begin
  { Should not raise an exception }
  FManager.Undo;
  AssertFalse('Still cannot undo', FManager.CanUndo);
end;

procedure TTestUndoManager.TestRedoOnEmptyStackDoesNothing;
begin
  { Should not raise an exception }
  FManager.Redo;
  AssertFalse('Still cannot redo', FManager.CanRedo);
end;

procedure TTestUndoManager.TestMaxUndoLevels;
var
  Lines: TStringList;
  i: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('');
    FManager.MaxUndoLevels := 5;
    for i := 1 to 10 do
    begin
      FManager.BreakMerge;
      FManager.ExecuteAction(TInsertTextAction.Create(Lines, 0, i - 1, Chr(Ord('A') + i - 1)));
    end;
    AssertEquals('Should be capped at max levels', 5, FManager.UndoCount);
    { The oldest 5 actions should have been discarded }
    for i := 1 to 5 do
      FManager.Undo;
    AssertFalse('Should not be able to undo beyond max', FManager.CanUndo);
  finally
    Lines.Free;
  end;
end;

procedure TTestUndoManager.TestClear;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('hello');
    FManager.ExecuteAction(TInsertTextAction.Create(Lines, 0, 5, 'X'));
    FManager.Undo;
    AssertTrue('Should have undo', FManager.CanUndo or FManager.CanRedo);
    FManager.Clear;
    AssertFalse('After clear, no undo', FManager.CanUndo);
    AssertFalse('After clear, no redo', FManager.CanRedo);
  finally
    Lines.Free;
  end;
end;


{ =========================================================================
    TTestInsertTextAction
  ========================================================================= }

procedure TTestInsertTextAction.SetUp;
begin
  inherited SetUp;
  FLines := TStringList.Create;
  FManager := TUndoManager.Create;
end;

procedure TTestInsertTextAction.TearDown;
begin
  FManager.Free;
  FLines.Free;
  inherited TearDown;
end;

procedure TTestInsertTextAction.TestInsertSingleChar;
begin
  FLines.Add('hello');
  FManager.ExecuteAction(TInsertTextAction.Create(FLines, 0, 5, 'X'));
  AssertEquals('helloX', FLines[0]);
end;

procedure TTestInsertTextAction.TestInsertAtMiddleOfLine;
begin
  FLines.Add('helo');
  FManager.ExecuteAction(TInsertTextAction.Create(FLines, 0, 2, 'l'));
  AssertEquals('hello', FLines[0]);
end;

procedure TTestInsertTextAction.TestInsertAtEndOfLine;
begin
  FLines.Add('abc');
  FManager.ExecuteAction(TInsertTextAction.Create(FLines, 0, 3, 'def'));
  AssertEquals('abcdef', FLines[0]);
end;

procedure TTestInsertTextAction.TestInsertIntoEmptyLine;
begin
  FLines.Add('');
  FManager.ExecuteAction(TInsertTextAction.Create(FLines, 0, 0, 'A'));
  AssertEquals('A', FLines[0]);
end;

procedure TTestInsertTextAction.TestUndoInsertRemovesText;
begin
  FLines.Add('hello');
  FManager.ExecuteAction(TInsertTextAction.Create(FLines, 0, 5, ' world'));
  AssertEquals('hello world', FLines[0]);
  FManager.Undo;
  AssertEquals('hello', FLines[0]);
end;

procedure TTestInsertTextAction.TestRedoInsertRestoresText;
begin
  FLines.Add('hello');
  FManager.ExecuteAction(TInsertTextAction.Create(FLines, 0, 5, ' world'));
  FManager.Undo;
  FManager.Redo;
  AssertEquals('hello world', FLines[0]);
end;

procedure TTestInsertTextAction.TestInsertMultiByteUTF8;
begin
  FLines.Add('cafe');
  { Insert a UTF-8 character (e-acute: 2 bytes in UTF-8) }
  FManager.ExecuteAction(TInsertTextAction.Create(FLines, 0, 3, #$C3#$A9));
  AssertEquals('caf' + #$C3#$A9 + 'e', FLines[0]);
  FManager.Undo;
  AssertEquals('cafe', FLines[0]);
end;


{ =========================================================================
    TTestDeleteTextAction
  ========================================================================= }

procedure TTestDeleteTextAction.SetUp;
begin
  inherited SetUp;
  FLines := TStringList.Create;
  FManager := TUndoManager.Create;
end;

procedure TTestDeleteTextAction.TearDown;
begin
  FManager.Free;
  FLines.Free;
  inherited TearDown;
end;

procedure TTestDeleteTextAction.TestDeleteSingleChar;
begin
  FLines.Add('hello');
  { Delete the character at position 4 (the 'o') }
  FManager.ExecuteAction(TDeleteTextAction.Create(FLines, 0, 4, 1));
  AssertEquals('hell', FLines[0]);
end;

procedure TTestDeleteTextAction.TestDeleteAtStartOfLine;
begin
  FLines.Add('hello');
  FManager.ExecuteAction(TDeleteTextAction.Create(FLines, 0, 0, 1));
  AssertEquals('ello', FLines[0]);
end;

procedure TTestDeleteTextAction.TestUndoDeleteRestoresChar;
begin
  FLines.Add('hello');
  FManager.ExecuteAction(TDeleteTextAction.Create(FLines, 0, 4, 1));
  AssertEquals('hell', FLines[0]);
  FManager.Undo;
  AssertEquals('hello', FLines[0]);
end;

procedure TTestDeleteTextAction.TestRedoDeleteRemovesAgain;
begin
  FLines.Add('hello');
  FManager.ExecuteAction(TDeleteTextAction.Create(FLines, 0, 4, 1));
  FManager.Undo;
  FManager.Redo;
  AssertEquals('hell', FLines[0]);
end;

procedure TTestDeleteTextAction.TestDeleteSelection;
begin
  FLines.Add('hello world');
  { Delete 5 chars starting at position 5 (' worl') }
  FManager.ExecuteAction(TDeleteTextAction.Create(FLines, 0, 5, 5));
  AssertEquals('hellod', FLines[0]);
end;

procedure TTestDeleteTextAction.TestUndoDeleteSelectionRestoresText;
begin
  FLines.Add('hello world');
  FManager.ExecuteAction(TDeleteTextAction.Create(FLines, 0, 5, 5));
  AssertEquals('hellod', FLines[0]);
  FManager.Undo;
  AssertEquals('hello world', FLines[0]);
end;


{ =========================================================================
    TTestSplitLineAction
  ========================================================================= }

procedure TTestSplitLineAction.SetUp;
begin
  inherited SetUp;
  FLines := TStringList.Create;
  FManager := TUndoManager.Create;
end;

procedure TTestSplitLineAction.TearDown;
begin
  FManager.Free;
  FLines.Free;
  inherited TearDown;
end;

procedure TTestSplitLineAction.TestSplitAtMiddle;
begin
  FLines.Add('hello world');
  FManager.ExecuteAction(TSplitLineAction.Create(FLines, 0, 5));
  AssertEquals('Line count after split', 2, FLines.Count);
  AssertEquals('First line', 'hello', FLines[0]);
  AssertEquals('Second line', ' world', FLines[1]);
end;

procedure TTestSplitLineAction.TestSplitAtStart;
begin
  FLines.Add('hello');
  FManager.ExecuteAction(TSplitLineAction.Create(FLines, 0, 0));
  AssertEquals('Line count', 2, FLines.Count);
  AssertEquals('First line should be empty', '', FLines[0]);
  AssertEquals('Second line', 'hello', FLines[1]);
end;

procedure TTestSplitLineAction.TestSplitAtEnd;
begin
  FLines.Add('hello');
  FManager.ExecuteAction(TSplitLineAction.Create(FLines, 0, 5));
  AssertEquals('Line count', 2, FLines.Count);
  AssertEquals('First line', 'hello', FLines[0]);
  AssertEquals('Second line should be empty', '', FLines[1]);
end;

procedure TTestSplitLineAction.TestSplitWithAutoIndent;
begin
  FLines.Add('  hello world');
  { Split at position 7 (after 'he'), with 2-space auto-indent }
  FManager.ExecuteAction(TSplitLineAction.Create(FLines, 0, 7, '  '));
  AssertEquals('Line count', 2, FLines.Count);
  AssertEquals('First line', '  hello', FLines[0]);
  AssertEquals('Second line with indent', '   world', FLines[1]);
end;

procedure TTestSplitLineAction.TestUndoSplitRejoinsLine;
begin
  FLines.Add('hello world');
  FManager.ExecuteAction(TSplitLineAction.Create(FLines, 0, 5));
  AssertEquals('After split', 2, FLines.Count);
  FManager.Undo;
  AssertEquals('After undo line count', 1, FLines.Count);
  AssertEquals('After undo content', 'hello world', FLines[0]);
end;

procedure TTestSplitLineAction.TestRedoSplitSplitsAgain;
begin
  FLines.Add('hello world');
  FManager.ExecuteAction(TSplitLineAction.Create(FLines, 0, 5));
  FManager.Undo;
  FManager.Redo;
  AssertEquals('After redo line count', 2, FLines.Count);
  AssertEquals('First line', 'hello', FLines[0]);
  AssertEquals('Second line', ' world', FLines[1]);
end;


{ =========================================================================
    TTestJoinLinesAction
  ========================================================================= }

procedure TTestJoinLinesAction.SetUp;
begin
  inherited SetUp;
  FLines := TStringList.Create;
  FManager := TUndoManager.Create;
end;

procedure TTestJoinLinesAction.TearDown;
begin
  FManager.Free;
  FLines.Free;
  inherited TearDown;
end;

procedure TTestJoinLinesAction.TestJoinWithPreviousLine;
begin
  FLines.Add('hello');
  FLines.Add(' world');
  { Join line 1 onto line 0 (backspace at start of line 1) }
  FManager.ExecuteAction(TJoinLinesAction.Create(FLines, 1));
  AssertEquals('Line count', 1, FLines.Count);
  AssertEquals('Joined content', 'hello world', FLines[0]);
end;

procedure TTestJoinLinesAction.TestJoinWithNextLine;
begin
  FLines.Add('hello');
  FLines.Add(' world');
  { Join line 1 onto line 0 (delete at end of line 0) — same operation, different trigger }
  FManager.ExecuteAction(TJoinLinesAction.Create(FLines, 1));
  AssertEquals('Line count', 1, FLines.Count);
  AssertEquals('Joined content', 'hello world', FLines[0]);
end;

procedure TTestJoinLinesAction.TestUndoJoinRestoresTwoLines;
begin
  FLines.Add('hello');
  FLines.Add(' world');
  FManager.ExecuteAction(TJoinLinesAction.Create(FLines, 1));
  AssertEquals('After join', 1, FLines.Count);
  FManager.Undo;
  AssertEquals('After undo line count', 2, FLines.Count);
  AssertEquals('Line 0', 'hello', FLines[0]);
  AssertEquals('Line 1', ' world', FLines[1]);
end;

procedure TTestJoinLinesAction.TestRedoJoinMergesAgain;
begin
  FLines.Add('hello');
  FLines.Add(' world');
  FManager.ExecuteAction(TJoinLinesAction.Create(FLines, 1));
  FManager.Undo;
  FManager.Redo;
  AssertEquals('After redo line count', 1, FLines.Count);
  AssertEquals('After redo content', 'hello world', FLines[0]);
end;


{ =========================================================================
    TTestCompoundAction
  ========================================================================= }

procedure TTestCompoundAction.SetUp;
begin
  inherited SetUp;
  FLines := TStringList.Create;
  FManager := TUndoManager.Create;
end;

procedure TTestCompoundAction.TearDown;
begin
  FManager.Free;
  FLines.Free;
  inherited TearDown;
end;

procedure TTestCompoundAction.TestCompoundUndoesAllAtOnce;
var
  Compound: TCompoundAction;
begin
  FLines.Add('hello world');
  { Simulate paste-replacing-selection: delete selection then insert }
  Compound := TCompoundAction.Create;
  Compound.Add(TDeleteTextAction.Create(FLines, 0, 6, 5)); { delete 'world' }
  Compound.Add(TInsertTextAction.Create(FLines, 0, 6, 'there'));  { insert 'there' }
  FManager.ExecuteAction(Compound);
  AssertEquals('After compound', 'hello there', FLines[0]);

  { Single undo should revert the entire compound }
  FManager.Undo;
  AssertEquals('After undo', 'hello world', FLines[0]);
  AssertEquals('Only one undo step consumed', 0, FManager.UndoCount);
end;

procedure TTestCompoundAction.TestCompoundRedoesAllAtOnce;
var
  Compound: TCompoundAction;
begin
  FLines.Add('hello world');
  Compound := TCompoundAction.Create;
  Compound.Add(TDeleteTextAction.Create(FLines, 0, 6, 5));
  Compound.Add(TInsertTextAction.Create(FLines, 0, 6, 'there'));
  FManager.ExecuteAction(Compound);
  FManager.Undo;
  FManager.Redo;
  AssertEquals('After redo', 'hello there', FLines[0]);
end;

procedure TTestCompoundAction.TestEmptyCompoundIsNoOp;
var
  Compound: TCompoundAction;
begin
  FLines.Add('hello');
  Compound := TCompoundAction.Create;
  FManager.ExecuteAction(Compound);
  AssertEquals('Text unchanged', 'hello', FLines[0]);
  { Empty compound should still be on the stack but undo is harmless }
  FManager.Undo;
  AssertEquals('Still unchanged', 'hello', FLines[0]);
end;


{ =========================================================================
    TTestActionMerging
  ========================================================================= }

procedure TTestActionMerging.SetUp;
begin
  inherited SetUp;
  FLines := TStringList.Create;
  FManager := TUndoManager.Create;
end;

procedure TTestActionMerging.TearDown;
begin
  FManager.Free;
  FLines.Free;
  inherited TearDown;
end;

procedure TTestActionMerging.TestConsecutiveInsertsAtSamePositionMerge;
begin
  FLines.Add('');
  { Type 'hello' one character at a time — should merge into one undo step }
  FManager.ExecuteAction(TInsertTextAction.Create(FLines, 0, 0, 'h'));
  FManager.ExecuteAction(TInsertTextAction.Create(FLines, 0, 1, 'e'));
  FManager.ExecuteAction(TInsertTextAction.Create(FLines, 0, 2, 'l'));
  FManager.ExecuteAction(TInsertTextAction.Create(FLines, 0, 3, 'l'));
  FManager.ExecuteAction(TInsertTextAction.Create(FLines, 0, 4, 'o'));
  AssertEquals('Text after typing', 'hello', FLines[0]);
  AssertEquals('Should merge into 1 undo step', 1, FManager.UndoCount);

  FManager.Undo;
  AssertEquals('Single undo reverts entire word', '', FLines[0]);
end;

procedure TTestActionMerging.TestInsertAfterPauseDoesNotMerge;
begin
  FLines.Add('');
  FManager.ExecuteAction(TInsertTextAction.Create(FLines, 0, 0, 'a'));
  { Force a merge break }
  FManager.BreakMerge;
  FManager.ExecuteAction(TInsertTextAction.Create(FLines, 0, 1, 'b'));
  AssertEquals('Text after typing', 'ab', FLines[0]);
  AssertEquals('Should be 2 separate undo steps', 2, FManager.UndoCount);
end;

procedure TTestActionMerging.TestDeletesDoNotMergeWithInserts;
begin
  FLines.Add('hello');
  FManager.ExecuteAction(TInsertTextAction.Create(FLines, 0, 5, 'X'));
  FManager.ExecuteAction(TDeleteTextAction.Create(FLines, 0, 5, 1));
  AssertEquals('Text unchanged after insert+delete', 'hello', FLines[0]);
  AssertEquals('Different action types should not merge', 2, FManager.UndoCount);
end;

procedure TTestActionMerging.TestConsecutiveBackspacesAtSamePositionMerge;
begin
  FLines.Add('hello');
  { Backspace from end: delete 'o', then 'l', then 'l' }
  FManager.ExecuteAction(TDeleteTextAction.Create(FLines, 0, 4, 1));  { delete 'o' }
  FManager.ExecuteAction(TDeleteTextAction.Create(FLines, 0, 3, 1));  { delete 'l' }
  FManager.ExecuteAction(TDeleteTextAction.Create(FLines, 0, 2, 1));  { delete 'l' }
  AssertEquals('Text after backspaces', 'he', FLines[0]);
  AssertEquals('Consecutive backspaces should merge', 1, FManager.UndoCount);

  FManager.Undo;
  AssertEquals('Single undo restores all', 'hello', FLines[0]);
end;


initialization
  RegisterTest(TTestUndoManager);
  RegisterTest(TTestInsertTextAction);
  RegisterTest(TTestDeleteTextAction);
  RegisterTest(TTestSplitLineAction);
  RegisterTest(TTestJoinLinesAction);
  RegisterTest(TTestCompoundAction);
  RegisterTest(TTestActionMerging);

end.
