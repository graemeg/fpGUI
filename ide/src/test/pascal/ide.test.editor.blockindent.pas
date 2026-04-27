{
    fpGUI IDE — Block Indent/Unindent Tests

    Tests for block indent and unindent operations using TTextBlockAction.
    These tests verify the indent logic in isolation from the text editor
    widget, operating directly on a TStringList as the text model.
}
unit ide.test.editor.blockindent;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  ide.editor.undo;

type

  { TTestBlockIndent }

  TTestBlockIndent = class(TTestCase)
  private
    FLines: TStringList;
    FManager: TUndoManager;
    procedure IndentLines(AStartLine, AEndLine, AIndentSize: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIndentSingleLine;
    procedure TestIndentMultipleLines;
    procedure TestIndentPreservesExistingIndent;
    procedure TestIndentEmptyLine;
    procedure TestIndentMixedContentWithEmptyLines;
    procedure TestIndentCustomSize;
    procedure TestIndentUndo;
    procedure TestIndentUndoRedo;
  end;


  { TTestBlockUnindent }

  TTestBlockUnindent = class(TTestCase)
  private
    FLines: TStringList;
    FManager: TUndoManager;
    procedure UnindentLines(AStartLine, AEndLine, AIndentSize: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUnindentSingleLine;
    procedure TestUnindentMultipleLines;
    procedure TestUnindentRemovesPartialIndent;
    procedure TestUnindentNoLeadingSpaces;
    procedure TestUnindentEmptyLine;
    procedure TestUnindentMixedIndentLevels;
    procedure TestUnindentWithTabs;
    procedure TestUnindentUndo;
    procedure TestUnindentUndoRedo;
  end;


  { TTestBlockIndentRoundTrip }

  TTestBlockIndentRoundTrip = class(TTestCase)
  private
    FLines: TStringList;
    FManager: TUndoManager;
    procedure IndentLines(AStartLine, AEndLine, AIndentSize: Integer);
    procedure UnindentLines(AStartLine, AEndLine, AIndentSize: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIndentThenUnindentRestoresOriginal;
    procedure TestMultipleIndentsThenUnindents;
    procedure TestIndentUndoIsSingleStep;
    procedure TestUnindentUndoIsSingleStep;
  end;


implementation

{ Helper: indent lines AStartLine..AEndLine by prepending AIndentSize spaces.
  Uses TTextBlockAction for undo support. }
procedure IndentLinesImpl(ALines: TStringList; AManager: TUndoManager;
  AStartLine, AEndLine, AIndentSize: Integer);
var
  Block: TTextBlockAction;
  I: Integer;
  Indent: string;
begin
  Block := TTextBlockAction.Create(ALines, AStartLine);
  Block.SaveBefore(AEndLine);
  Block.CaretBefore := Point(0, AStartLine);

  Indent := StringOfChar(' ', AIndentSize);
  for I := AStartLine to AEndLine do
    ALines[I] := Indent + ALines[I];

  Block.SaveAfter(AEndLine);
  Block.CaretAfter := Point(AIndentSize, AStartLine);
  AManager.ExecuteAction(Block);
end;

{ Helper: unindent lines AStartLine..AEndLine by removing up to AIndentSize
  leading spaces (or one tab). Uses TTextBlockAction for undo support. }
procedure UnindentLinesImpl(ALines: TStringList; AManager: TUndoManager;
  AStartLine, AEndLine, AIndentSize: Integer);
var
  Block: TTextBlockAction;
  I, J, Remove: Integer;
  Line: string;
begin
  Block := TTextBlockAction.Create(ALines, AStartLine);
  Block.SaveBefore(AEndLine);
  Block.CaretBefore := Point(0, AStartLine);

  for I := AStartLine to AEndLine do
  begin
    Line := ALines[I];
    if (Length(Line) > 0) and (Line[1] = #9) then
    begin
      { Remove one leading tab }
      Delete(Line, 1, 1);
      ALines[I] := Line;
    end
    else
    begin
      { Remove up to AIndentSize leading spaces }
      Remove := 0;
      for J := 1 to Length(Line) do
      begin
        if (Remove >= AIndentSize) then
          Break;
        if Line[J] = ' ' then
          Inc(Remove)
        else
          Break;
      end;
      if Remove > 0 then
      begin
        Delete(Line, 1, Remove);
        ALines[I] := Line;
      end;
    end;
  end;

  Block.SaveAfter(AEndLine);
  Block.CaretAfter := Point(0, AStartLine);
  AManager.ExecuteAction(Block);
end;


{ TTestBlockIndent }

procedure TTestBlockIndent.IndentLines(AStartLine, AEndLine, AIndentSize: Integer);
begin
  IndentLinesImpl(FLines, FManager, AStartLine, AEndLine, AIndentSize);
end;

procedure TTestBlockIndent.SetUp;
begin
  FLines := TStringList.Create;
  FManager := TUndoManager.Create;
end;

procedure TTestBlockIndent.TearDown;
begin
  FManager.Free;
  FLines.Free;
end;

procedure TTestBlockIndent.TestIndentSingleLine;
begin
  FLines.Add('hello');
  IndentLines(0, 0, 2);
  AssertEquals('Indented line', '  hello', FLines[0]);
end;

procedure TTestBlockIndent.TestIndentMultipleLines;
begin
  FLines.Add('line one');
  FLines.Add('line two');
  FLines.Add('line three');
  IndentLines(0, 2, 2);
  AssertEquals('Line 0', '  line one', FLines[0]);
  AssertEquals('Line 1', '  line two', FLines[1]);
  AssertEquals('Line 2', '  line three', FLines[2]);
end;

procedure TTestBlockIndent.TestIndentPreservesExistingIndent;
begin
  FLines.Add('  already indented');
  IndentLines(0, 0, 2);
  AssertEquals('Double indented', '    already indented', FLines[0]);
end;

procedure TTestBlockIndent.TestIndentEmptyLine;
begin
  FLines.Add('');
  IndentLines(0, 0, 2);
  AssertEquals('Indented empty line', '  ', FLines[0]);
end;

procedure TTestBlockIndent.TestIndentMixedContentWithEmptyLines;
begin
  FLines.Add('first');
  FLines.Add('');
  FLines.Add('third');
  IndentLines(0, 2, 2);
  AssertEquals('Line 0', '  first', FLines[0]);
  AssertEquals('Line 1 (empty)', '  ', FLines[1]);
  AssertEquals('Line 2', '  third', FLines[2]);
end;

procedure TTestBlockIndent.TestIndentCustomSize;
begin
  FLines.Add('hello');
  IndentLines(0, 0, 4);
  AssertEquals('4-space indent', '    hello', FLines[0]);
end;

procedure TTestBlockIndent.TestIndentUndo;
begin
  FLines.Add('hello');
  FLines.Add('world');
  IndentLines(0, 1, 2);
  AssertEquals('Before undo line 0', '  hello', FLines[0]);
  AssertEquals('Before undo line 1', '  world', FLines[1]);
  FManager.Undo;
  AssertEquals('After undo line 0', 'hello', FLines[0]);
  AssertEquals('After undo line 1', 'world', FLines[1]);
end;

procedure TTestBlockIndent.TestIndentUndoRedo;
begin
  FLines.Add('hello');
  IndentLines(0, 0, 2);
  FManager.Undo;
  AssertEquals('After undo', 'hello', FLines[0]);
  FManager.Redo;
  AssertEquals('After redo', '  hello', FLines[0]);
end;


{ TTestBlockUnindent }

procedure TTestBlockUnindent.UnindentLines(AStartLine, AEndLine, AIndentSize: Integer);
begin
  UnindentLinesImpl(FLines, FManager, AStartLine, AEndLine, AIndentSize);
end;

procedure TTestBlockUnindent.SetUp;
begin
  FLines := TStringList.Create;
  FManager := TUndoManager.Create;
end;

procedure TTestBlockUnindent.TearDown;
begin
  FManager.Free;
  FLines.Free;
end;

procedure TTestBlockUnindent.TestUnindentSingleLine;
begin
  FLines.Add('  hello');
  UnindentLines(0, 0, 2);
  AssertEquals('Unindented line', 'hello', FLines[0]);
end;

procedure TTestBlockUnindent.TestUnindentMultipleLines;
begin
  FLines.Add('  line one');
  FLines.Add('  line two');
  FLines.Add('  line three');
  UnindentLines(0, 2, 2);
  AssertEquals('Line 0', 'line one', FLines[0]);
  AssertEquals('Line 1', 'line two', FLines[1]);
  AssertEquals('Line 2', 'line three', FLines[2]);
end;

procedure TTestBlockUnindent.TestUnindentRemovesPartialIndent;
begin
  FLines.Add(' hello');  { only 1 space, indent size is 2 }
  UnindentLines(0, 0, 2);
  AssertEquals('Partial unindent', 'hello', FLines[0]);
end;

procedure TTestBlockUnindent.TestUnindentNoLeadingSpaces;
begin
  FLines.Add('hello');
  UnindentLines(0, 0, 2);
  AssertEquals('No change', 'hello', FLines[0]);
end;

procedure TTestBlockUnindent.TestUnindentEmptyLine;
begin
  FLines.Add('');
  UnindentLines(0, 0, 2);
  AssertEquals('Empty stays empty', '', FLines[0]);
end;

procedure TTestBlockUnindent.TestUnindentMixedIndentLevels;
begin
  FLines.Add('    four spaces');
  FLines.Add('  two spaces');
  FLines.Add(' one space');
  FLines.Add('no spaces');
  UnindentLines(0, 3, 2);
  AssertEquals('Line 0', '  four spaces', FLines[0]);
  AssertEquals('Line 1', 'two spaces', FLines[1]);
  AssertEquals('Line 2', 'one space', FLines[2]);  { only 1 space removed }
  AssertEquals('Line 3', 'no spaces', FLines[3]);
end;

procedure TTestBlockUnindent.TestUnindentWithTabs;
begin
  FLines.Add(#9 + 'tabbed line');
  UnindentLines(0, 0, 2);
  AssertEquals('Tab removed', 'tabbed line', FLines[0]);
end;

procedure TTestBlockUnindent.TestUnindentUndo;
begin
  FLines.Add('  hello');
  FLines.Add('  world');
  UnindentLines(0, 1, 2);
  AssertEquals('Before undo line 0', 'hello', FLines[0]);
  AssertEquals('Before undo line 1', 'world', FLines[1]);
  FManager.Undo;
  AssertEquals('After undo line 0', '  hello', FLines[0]);
  AssertEquals('After undo line 1', '  world', FLines[1]);
end;

procedure TTestBlockUnindent.TestUnindentUndoRedo;
begin
  FLines.Add('  hello');
  UnindentLines(0, 0, 2);
  FManager.Undo;
  AssertEquals('After undo', '  hello', FLines[0]);
  FManager.Redo;
  AssertEquals('After redo', 'hello', FLines[0]);
end;


{ TTestBlockIndentRoundTrip }

procedure TTestBlockIndentRoundTrip.IndentLines(AStartLine, AEndLine, AIndentSize: Integer);
begin
  IndentLinesImpl(FLines, FManager, AStartLine, AEndLine, AIndentSize);
end;

procedure TTestBlockIndentRoundTrip.UnindentLines(AStartLine, AEndLine, AIndentSize: Integer);
begin
  UnindentLinesImpl(FLines, FManager, AStartLine, AEndLine, AIndentSize);
end;

procedure TTestBlockIndentRoundTrip.SetUp;
begin
  FLines := TStringList.Create;
  FManager := TUndoManager.Create;
end;

procedure TTestBlockIndentRoundTrip.TearDown;
begin
  FManager.Free;
  FLines.Free;
end;

procedure TTestBlockIndentRoundTrip.TestIndentThenUnindentRestoresOriginal;
begin
  FLines.Add('hello');
  FLines.Add('world');
  IndentLines(0, 1, 2);
  UnindentLines(0, 1, 2);
  AssertEquals('Line 0 restored', 'hello', FLines[0]);
  AssertEquals('Line 1 restored', 'world', FLines[1]);
end;

procedure TTestBlockIndentRoundTrip.TestMultipleIndentsThenUnindents;
begin
  FLines.Add('code');
  IndentLines(0, 0, 2);
  IndentLines(0, 0, 2);
  AssertEquals('Double indented', '    code', FLines[0]);
  UnindentLines(0, 0, 2);
  AssertEquals('Single indented', '  code', FLines[0]);
  UnindentLines(0, 0, 2);
  AssertEquals('Restored', 'code', FLines[0]);
end;

procedure TTestBlockIndentRoundTrip.TestIndentUndoIsSingleStep;
begin
  FLines.Add('line one');
  FLines.Add('line two');
  FLines.Add('line three');
  IndentLines(0, 2, 2);
  AssertEquals('Undo count after indent', 1, FManager.UndoCount);
  FManager.Undo;
  AssertEquals('Line 0 after undo', 'line one', FLines[0]);
  AssertEquals('Line 1 after undo', 'line two', FLines[1]);
  AssertEquals('Line 2 after undo', 'line three', FLines[2]);
end;

procedure TTestBlockIndentRoundTrip.TestUnindentUndoIsSingleStep;
begin
  FLines.Add('  line one');
  FLines.Add('  line two');
  FLines.Add('  line three');
  UnindentLines(0, 2, 2);
  AssertEquals('Undo count after unindent', 1, FManager.UndoCount);
  FManager.Undo;
  AssertEquals('Line 0 after undo', '  line one', FLines[0]);
  AssertEquals('Line 1 after undo', '  line two', FLines[1]);
  AssertEquals('Line 2 after undo', '  line three', FLines[2]);
end;


initialization
  RegisterTest(TTestBlockIndent);
  RegisterTest(TTestBlockUnindent);
  RegisterTest(TTestBlockIndentRoundTrip);

end.
