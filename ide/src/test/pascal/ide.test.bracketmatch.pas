{
    fpGUI IDE - Bracket Matching Tests

    Tests for FindMatchingBracket and related functions in ide.bracketmatch.
    These tests verify bracket matching using the TPascalHighlighter token
    stream against known Pascal source inputs.
}
unit ide.test.bracketmatch;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  ide.highlighter,
  ide.bracketmatch;

type

  { TTestBracketMatch }

  TTestBracketMatch = class(TTestCase)
  private
    FHL: TPascalHighlighter;
    FLines: TStringList;
    procedure SetupSource(const ASource: string);
    procedure CheckMatch(ALine, ACol: Integer;
      AExpMatchLine, AExpMatchCol: Integer; const AContext: string);
    procedure CheckNoMatch(ALine, ACol: Integer; const AContext: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Simple bracket pairs }
    procedure TestMatchParenForward;
    procedure TestMatchParenBackward;
    procedure TestMatchSquareBracketForward;
    procedure TestMatchSquareBracketBackward;

    { Nesting }
    procedure TestNestedParens;
    procedure TestNestedMixed;

    { Multi-line }
    procedure TestMultiLineParens;

    { begin/end pairs }
    procedure TestMatchBeginEnd;
    procedure TestMatchEndBegin;
    procedure TestNestedBeginEnd;
    procedure TestBeginEndAcrossLines;

    { Block openers: case, try, record }
    procedure TestMatchCaseEnd;
    procedure TestMatchTryEnd;

    { Edge cases }
    procedure TestNoMatchAtNonBracket;
    procedure TestBracketInsideComment;
    procedure TestBracketInsideString;
    procedure TestUnmatchedBracket;
    procedure TestCaretAtEndOfLine;
    procedure TestEmptySource;

    { Classification }
    procedure TestClassifySymbols;
    procedure TestClassifyKeywords;
    procedure TestOppositeKind;
    procedure TestIsOpeningBracket;
  end;


implementation

{ TTestBracketMatch }

procedure TTestBracketMatch.SetupSource(const ASource: string);
begin
  FLines.Text := ASource;
  FHL.Tokenise(ASource);
end;

procedure TTestBracketMatch.CheckMatch(ALine, ACol: Integer;
  AExpMatchLine, AExpMatchCol: Integer; const AContext: string);
var
  r: TBracketMatchResult;
begin
  r := FindMatchingBracket(FHL, ALine, ACol, FLines);
  AssertTrue(AContext + ': should find match', r.Found);
  AssertEquals(AContext + ': match line', AExpMatchLine, r.MatchLine);
  AssertEquals(AContext + ': match col', AExpMatchCol, r.MatchCol);
end;

procedure TTestBracketMatch.CheckNoMatch(ALine, ACol: Integer;
  const AContext: string);
var
  r: TBracketMatchResult;
begin
  r := FindMatchingBracket(FHL, ALine, ACol, FLines);
  AssertFalse(AContext + ': should not find match', r.Found);
end;

procedure TTestBracketMatch.SetUp;
begin
  FHL := TPascalHighlighter.Create;
  FLines := TStringList.Create;
end;

procedure TTestBracketMatch.TearDown;
begin
  FreeAndNil(FLines);
  FreeAndNil(FHL);
end;

{ --- Simple bracket pairs --- }

procedure TTestBracketMatch.TestMatchParenForward;
begin
  // program t; begin writeln('x') end.
  // Column positions for the parens around 'x':
  // writeln('x')
  //        ^   ^
  SetupSource('program t;' + LineEnding + 'begin writeln(''x'') end.');
  // Line 1: "begin writeln('x') end."
  // 'writeln' starts at col 6, '(' at col 13, 'x' string, ')' at col 17
  CheckMatch(1, 13, 1, 17, 'paren forward');
end;

procedure TTestBracketMatch.TestMatchParenBackward;
begin
  SetupSource('program t;' + LineEnding + 'begin writeln(''x'') end.');
  CheckMatch(1, 17, 1, 13, 'paren backward');
end;

procedure TTestBracketMatch.TestMatchSquareBracketForward;
begin
  SetupSource('program t;' + LineEnding + 'begin a[0] end.');
  // Line 1: "begin a[0] end."
  // 'a' at col 6, '[' at col 7, '0' at col 8, ']' at col 9
  CheckMatch(1, 7, 1, 9, 'square forward');
end;

procedure TTestBracketMatch.TestMatchSquareBracketBackward;
begin
  SetupSource('program t;' + LineEnding + 'begin a[0] end.');
  CheckMatch(1, 9, 1, 7, 'square backward');
end;

{ --- Nesting --- }

procedure TTestBracketMatch.TestNestedParens;
begin
  // "program t; begin a((b)) end."
  // Line 1: "begin a((b)) end."
  //                 ^^  ^^
  SetupSource('program t;' + LineEnding + 'begin a((b)) end.');
  // outer ( should match outer )
  CheckMatch(1, 7, 1, 11, 'nested outer paren forward');
  // inner ( should match inner )
  CheckMatch(1, 8, 1, 10, 'nested inner paren forward');
end;

procedure TTestBracketMatch.TestNestedMixed;
begin
  // "program t; begin a([b]) end."
  SetupSource('program t;' + LineEnding + 'begin a([b]) end.');
  // ( at col 7 should match ) at col 11, skipping [ and ]
  CheckMatch(1, 7, 1, 11, 'nested mixed paren forward');
  // [ at col 8 should match ] at col 10
  CheckMatch(1, 8, 1, 10, 'nested mixed square forward');
end;

{ --- Multi-line --- }

procedure TTestBracketMatch.TestMultiLineParens;
begin
  SetupSource('program t;' + LineEnding +
              'begin a(' + LineEnding +
              '  b,' + LineEnding +
              '  c) end.');
  // ( at line 1 col 7, ) at line 3 col 3
  CheckMatch(1, 7, 3, 3, 'multi-line paren forward');
  CheckMatch(3, 3, 1, 7, 'multi-line paren backward');
end;

{ --- begin/end pairs --- }

procedure TTestBracketMatch.TestMatchBeginEnd;
begin
  SetupSource('program t;' + LineEnding +
              'begin' + LineEnding +
              '  writeln;' + LineEnding +
              'end.');
  // 'begin' at line 1 col 0, 'end' at line 3 col 0
  CheckMatch(1, 0, 3, 0, 'begin->end forward');
end;

procedure TTestBracketMatch.TestMatchEndBegin;
begin
  SetupSource('program t;' + LineEnding +
              'begin' + LineEnding +
              '  writeln;' + LineEnding +
              'end.');
  CheckMatch(3, 0, 1, 0, 'end->begin backward');
end;

procedure TTestBracketMatch.TestNestedBeginEnd;
begin
  SetupSource('program t;' + LineEnding +
              'begin' + LineEnding +
              '  begin' + LineEnding +
              '    writeln;' + LineEnding +
              '  end;' + LineEnding +
              'end.');
  // outer begin at line 1 col 0, outer end at line 5 col 0
  CheckMatch(1, 0, 5, 0, 'nested outer begin->end');
  // inner begin at line 2 col 2, inner end at line 4 col 2
  CheckMatch(2, 2, 4, 2, 'nested inner begin->end');
  // inner end at line 4 col 2 -> inner begin at line 2 col 2
  CheckMatch(4, 2, 2, 2, 'nested inner end->begin');
end;

procedure TTestBracketMatch.TestBeginEndAcrossLines;
begin
  SetupSource('program t;' + LineEnding +
              'begin' + LineEnding +
              '  if True then begin' + LineEnding +
              '    x := 1;' + LineEnding +
              '  end;' + LineEnding +
              'end.');
  // 'begin' at line 2 col 17 -> 'end' at line 4 col 2
  CheckMatch(2, 17, 4, 2, 'begin/end across lines');
end;

{ --- Block openers: case, try --- }

procedure TTestBracketMatch.TestMatchCaseEnd;
begin
  SetupSource('program t;' + LineEnding +
              'begin' + LineEnding +
              '  case x of' + LineEnding +
              '    1: writeln;' + LineEnding +
              '  end;' + LineEnding +
              'end.');
  // 'case' at line 2 col 2 -> 'end' at line 4 col 2
  CheckMatch(2, 2, 4, 2, 'case->end forward');
  CheckMatch(4, 2, 2, 2, 'end->case backward');
end;

procedure TTestBracketMatch.TestMatchTryEnd;
begin
  SetupSource('program t;' + LineEnding +
              'begin' + LineEnding +
              '  try' + LineEnding +
              '    writeln;' + LineEnding +
              '  except' + LineEnding +
              '  end;' + LineEnding +
              'end.');
  // 'try' at line 2 col 2 -> 'end' at line 5 col 2
  CheckMatch(2, 2, 5, 2, 'try->end forward');
end;

{ --- Edge cases --- }

procedure TTestBracketMatch.TestNoMatchAtNonBracket;
begin
  SetupSource('program t;' + LineEnding + 'begin writeln end.');
  // 'writeln' is an identifier, not a bracket
  CheckNoMatch(1, 6, 'identifier is not a bracket');
end;

procedure TTestBracketMatch.TestBracketInsideComment;
begin
  // The ( inside the comment should not be structural
  SetupSource('program t;' + LineEnding +
              'begin' + LineEnding +
              '  { ( }' + LineEnding +
              '  a(b);' + LineEnding +
              'end.');
  // ( at line 3 col 3 should match ) at line 3 col 5
  CheckMatch(3, 3, 3, 5, 'paren not confused by comment');
end;

procedure TTestBracketMatch.TestBracketInsideString;
begin
  SetupSource('program t;' + LineEnding +
              'begin' + LineEnding +
              '  a(''('');' + LineEnding +
              'end.');
  // Source line is:  a('(');
  // a at col 2, ( at col 3, string '(' at cols 4-6, ) at col 7, ; at col 8
  CheckMatch(2, 3, 2, 7, 'paren skips string content');
end;

procedure TTestBracketMatch.TestUnmatchedBracket;
begin
  SetupSource('program t;' + LineEnding + 'begin a( end.');
  // ( with no matching )
  CheckNoMatch(1, 7, 'unmatched open paren');
end;

procedure TTestBracketMatch.TestCaretAtEndOfLine;
begin
  SetupSource('program t;' + LineEnding + 'begin end.');
  // Column past all tokens
  CheckNoMatch(1, 50, 'caret past end of line');
end;

procedure TTestBracketMatch.TestEmptySource;
begin
  SetupSource('');
  CheckNoMatch(0, 0, 'empty source');
end;

{ --- Classification tests --- }

procedure TTestBracketMatch.TestClassifySymbols;
var
  tok: THighlightToken;
  lines: TStringList;
begin
  lines := TStringList.Create;
  try
    lines.Add('()[]');
    tok.Column := 0; tok.Length := 1; tok.Category := hcSymbol;
    AssertTrue('( is RoundOpen', ClassifyBracket(tok, lines, 0) = bkRoundOpen);
    tok.Column := 1;
    AssertTrue(') is RoundClose', ClassifyBracket(tok, lines, 0) = bkRoundClose);
    tok.Column := 2;
    AssertTrue('[ is SquareOpen', ClassifyBracket(tok, lines, 0) = bkSquareOpen);
    tok.Column := 3;
    AssertTrue('] is SquareClose', ClassifyBracket(tok, lines, 0) = bkSquareClose);
  finally
    lines.Free;
  end;
end;

procedure TTestBracketMatch.TestClassifyKeywords;
var
  tok: THighlightToken;
  lines: TStringList;
begin
  lines := TStringList.Create;
  try
    lines.Add('begin end case try record');
    tok.Category := hcKeyword1;

    tok.Column := 0; tok.Length := 5; // 'begin'
    AssertTrue('begin is bkBegin', ClassifyBracket(tok, lines, 0) = bkBegin);

    tok.Column := 6; tok.Length := 3; // 'end'
    AssertTrue('end is bkEnd', ClassifyBracket(tok, lines, 0) = bkEnd);

    tok.Column := 10; tok.Length := 4; // 'case'
    AssertTrue('case is bkBegin', ClassifyBracket(tok, lines, 0) = bkBegin);

    tok.Column := 15; tok.Length := 3; // 'try'
    AssertTrue('try is bkBegin', ClassifyBracket(tok, lines, 0) = bkBegin);

    tok.Column := 19; tok.Length := 6; // 'record'
    AssertTrue('record is bkBegin', ClassifyBracket(tok, lines, 0) = bkBegin);
  finally
    lines.Free;
  end;
end;

procedure TTestBracketMatch.TestOppositeKind;
begin
  AssertTrue('( opposite is )', OppositeKind(bkRoundOpen) = bkRoundClose);
  AssertTrue(') opposite is (', OppositeKind(bkRoundClose) = bkRoundOpen);
  AssertTrue('[ opposite is ]', OppositeKind(bkSquareOpen) = bkSquareClose);
  AssertTrue('] opposite is [', OppositeKind(bkSquareClose) = bkSquareOpen);
  AssertTrue('begin opposite is end', OppositeKind(bkBegin) = bkEnd);
  AssertTrue('end opposite is begin', OppositeKind(bkEnd) = bkBegin);
  AssertTrue('none opposite is none', OppositeKind(bkNone) = bkNone);
end;

procedure TTestBracketMatch.TestIsOpeningBracket;
begin
  AssertTrue('( is opening', IsOpeningBracket(bkRoundOpen));
  AssertFalse(') is not opening', IsOpeningBracket(bkRoundClose));
  AssertTrue('[ is opening', IsOpeningBracket(bkSquareOpen));
  AssertFalse('] is not opening', IsOpeningBracket(bkSquareClose));
  AssertTrue('begin is opening', IsOpeningBracket(bkBegin));
  AssertFalse('end is not opening', IsOpeningBracket(bkEnd));
  AssertFalse('none is not opening', IsOpeningBracket(bkNone));
end;

initialization
  RegisterTest(TTestBracketMatch);

end.
