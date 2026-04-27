{
    fpGUI IDE - Syntax Highlighter Tests

    Tests for TEditorHighlighter and TPascalHighlighter.
    These tests verify token positions, lengths, and categories
    against known Pascal source inputs.
}
unit ide.test.highlighter;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  ide.highlighter;

type

  { TTestPascalHighlighter }

  TTestPascalHighlighter = class(TTestCase)
  private
    FHL: TPascalHighlighter;
    procedure CheckToken(ALine, AIndex: Integer; AExpCol, AExpLen: Integer;
      AExpCat: THighlightCategory; const AContext: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Basic token positioning }
    procedure TestKeywordUnit;
    procedure TestKeywordAndIdentifier;
    procedure TestKeywordBeginEnd;

    { Numbers }
    procedure TestDecimalNumber;
    procedure TestHexNumber;

    { Strings }
    procedure TestSimpleString;

    { Comments }
    procedure TestLineComment;
    procedure TestBraceComment;
    procedure TestParenStarComment;

    { Directives }
    procedure TestBraceDirective;
    procedure TestModeDirective;

    { Multi-line comments }
    procedure TestMultiLineBraceComment;

    { Combined: unit header }
    procedure TestUnitHeader;

    { Line count }
    procedure TestLineCount;

    { Empty input }
    procedure TestEmptyInput;

    { Scanner error resilience }
    procedure TestIncompleteSource;

    { Trailing line comments }
    procedure TestTrailingLineComment;
    procedure TestLineCommentAfterCode;

    { Include directives }
    procedure TestIncludeDirectiveContinues;

    { Conditional compilation directives }
    procedure TestIfdefDirective;
    procedure TestIfdefSkippedContent;

    { Modifier/directive keywords (hcKeyword2) }
    procedure TestVisibilityModifiers;
    procedure TestMethodModifiers;
    procedure TestCallingConventions;
    procedure TestModifierMixedWithKeyword;
  end;


implementation

{ TTestPascalHighlighter }

procedure TTestPascalHighlighter.SetUp;
begin
  FHL := TPascalHighlighter.Create;
end;

procedure TTestPascalHighlighter.TearDown;
begin
  FHL.Free;
end;

procedure TTestPascalHighlighter.CheckToken(ALine, AIndex: Integer;
  AExpCol, AExpLen: Integer; AExpCat: THighlightCategory;
  const AContext: string);
var
  tokens: THighlightTokenArray;
  tok: THighlightToken;
  catNames: array[THighlightCategory] of string = (
    'hcWhitespace', 'hcKeyword1', 'hcKeyword2', 'hcKeyword3',
    'hcIdentifier', 'hcString1', 'hcString2', 'hcNumber',
    'hcComment1', 'hcComment2', 'hcDirective', 'hcSymbol',
    'hcOperator', 'hcFunction', 'hcLabel', 'hcMarkup', 'hcInvalid');
begin
  tokens := FHL.GetLineTokens(ALine);
  AssertTrue(Format('%s: line %d has no tokens', [AContext, ALine]),
    tokens <> nil);
  AssertTrue(Format('%s: line %d token index %d out of range (count=%d)',
    [AContext, ALine, AIndex, Length(tokens)]),
    AIndex < Length(tokens));
  tok := tokens[AIndex];
  AssertEquals(Format('%s: line %d token %d Column', [AContext, ALine, AIndex]),
    AExpCol, tok.Column);
  AssertEquals(Format('%s: line %d token %d Length', [AContext, ALine, AIndex]),
    AExpLen, tok.Length);
  AssertTrue(Format('%s: line %d token %d Category expected %s got %s',
    [AContext, ALine, AIndex, catNames[AExpCat], catNames[tok.Category]]),
    AExpCat = tok.Category);
end;

procedure TTestPascalHighlighter.TestKeywordUnit;
begin
  { "unit foo;" -> keyword "unit" at col 0, len 4 }
  FHL.Tokenise('unit foo;');
  CheckToken(0, 0, 0, 4, hcKeyword1, 'unit keyword');
  CheckToken(0, 1, 5, 3, hcIdentifier, 'unit name');
  CheckToken(0, 2, 8, 1, hcSymbol, 'semicolon');
end;

procedure TTestPascalHighlighter.TestKeywordAndIdentifier;
begin
  { "var x: Integer;" }
  FHL.Tokenise('var x: Integer;');
  CheckToken(0, 0, 0, 3, hcKeyword1, 'var keyword');
  CheckToken(0, 1, 4, 1, hcIdentifier, 'x identifier');
  CheckToken(0, 2, 5, 1, hcSymbol, 'colon');
  CheckToken(0, 3, 7, 7, hcIdentifier, 'Integer identifier');
  CheckToken(0, 4, 14, 1, hcSymbol, 'semicolon');
end;

procedure TTestPascalHighlighter.TestKeywordBeginEnd;
begin
  FHL.Tokenise('begin' + LineEnding + 'end.');
  CheckToken(0, 0, 0, 5, hcKeyword1, 'begin');
  CheckToken(1, 0, 0, 3, hcKeyword1, 'end');
  CheckToken(1, 1, 3, 1, hcSymbol, 'dot');
end;

procedure TTestPascalHighlighter.TestDecimalNumber;
begin
  FHL.Tokenise('x := 42;');
  CheckToken(0, 2, 5, 2, hcNumber, 'decimal 42');
end;

procedure TTestPascalHighlighter.TestHexNumber;
begin
  FHL.Tokenise('x := $FF;');
  CheckToken(0, 2, 5, 3, hcNumber, 'hex $FF');
end;

procedure TTestPascalHighlighter.TestSimpleString;
begin
  { 'hello' -> string token including quotes }
  FHL.Tokenise('s := ''hello'';');
  CheckToken(0, 2, 5, 7, hcString1, 'string literal');
end;

procedure TTestPascalHighlighter.TestLineComment;
begin
  { "// comment" -> comment starting at col 0, length 10 }
  FHL.Tokenise('// comment');
  CheckToken(0, 0, 0, 10, hcComment1, 'line comment');
end;

procedure TTestPascalHighlighter.TestBraceComment;
begin
  (* "{ comment }" -> comment at col 0, length 11 including braces *)
  FHL.Tokenise('{ comment }');
  CheckToken(0, 0, 0, 11, hcComment1, 'brace comment');
end;

procedure TTestPascalHighlighter.TestParenStarComment;
begin
  { "(* comment *)" -> comment at col 0, length 14 }
  { (* comment *) is 13 characters: ( * space c o m m e n t space * ) }
  FHL.Tokenise('(* comment *)');
  CheckToken(0, 0, 0, 13, hcComment1, 'paren-star comment');
end;

procedure TTestPascalHighlighter.TestBraceDirective;
begin
  (* "{$mode objfpc}" -> directive at col 0, length 14 *)
  FHL.Tokenise('{$mode objfpc}');
  CheckToken(0, 0, 0, 14, hcDirective, 'brace directive');
end;

procedure TTestPascalHighlighter.TestModeDirective;
begin
  (* Two directives on one line: "{$mode objfpc}{$H+}" *)
  FHL.Tokenise('{$mode objfpc}{$H+}');
  CheckToken(0, 0, 0, 14, hcDirective, 'first directive');
  CheckToken(0, 1, 14, 5, hcDirective, 'second directive');
end;

procedure TTestPascalHighlighter.TestMultiLineBraceComment;
var
  src: string;
begin
  { A brace comment spanning 3 lines }
  src := '{ line one' + LineEnding +
         '  line two' + LineEnding +
         '  line three }';
  FHL.Tokenise(src);
  { First line }
  AssertTrue('line 0 has tokens', FHL.GetLineTokenCount(0) > 0);
  AssertTrue('line 0 is comment',
    FHL.GetLineTokens(0)[0].Category = hcComment1);
  { Middle line }
  AssertTrue('line 1 has tokens', FHL.GetLineTokenCount(1) > 0);
  AssertTrue('line 1 is comment',
    FHL.GetLineTokens(1)[0].Category = hcComment1);
  { Last line }
  AssertTrue('line 2 has tokens', FHL.GetLineTokenCount(2) > 0);
  AssertTrue('line 2 is comment',
    FHL.GetLineTokens(2)[0].Category = hcComment1);
end;

procedure TTestPascalHighlighter.TestUnitHeader;
begin
  { "unit MyUnit;" on one line }
  FHL.Tokenise('unit MyUnit;');
  AssertEquals('token count', 3, FHL.GetLineTokenCount(0));
  CheckToken(0, 0, 0, 4, hcKeyword1, 'unit keyword');
  CheckToken(0, 1, 5, 6, hcIdentifier, 'MyUnit');
  CheckToken(0, 2, 11, 1, hcSymbol, 'semicolon');
end;

procedure TTestPascalHighlighter.TestLineCount;
begin
  FHL.Tokenise('a' + LineEnding + 'b' + LineEnding + 'c');
  AssertTrue('at least 3 lines', FHL.LineCount >= 3);
end;

procedure TTestPascalHighlighter.TestEmptyInput;
begin
  FHL.Tokenise('');
  AssertEquals('no lines', 0, FHL.LineCount);
end;

procedure TTestPascalHighlighter.TestIncompleteSource;
begin
  { Unclosed string — scanner should error but not crash.
    We should get at least partial tokens. }
  FHL.Tokenise('x := ''unclosed');
  { Should not raise, and should have some tokens for "x" and ":=" }
  AssertTrue('has some tokens', FHL.GetLineTokenCount(0) > 0);
end;

procedure TTestPascalHighlighter.TestTrailingLineComment;
begin
  { "x := 1; // a comment" — the // comment trails code }
  FHL.Tokenise('x := 1; // a comment');
  { Find the comment token — should cover "// a comment" = 12 chars }
  CheckToken(0, 4, 8, 12, hcComment1, 'trailing line comment');
end;

procedure TTestPascalHighlighter.TestLineCommentAfterCode;
var
  tokens: THighlightTokenArray;
  i: Integer;
  found: Boolean;
begin
  { "syncobjs, // TCriticalSection usage" }
  FHL.Tokenise('syncobjs, // TCriticalSection usage');
  tokens := FHL.GetLineTokens(0);
  AssertTrue('has tokens', tokens <> nil);
  { Find a comment token on this line }
  found := False;
  for i := 0 to Length(tokens) - 1 do
    if tokens[i].Category = hcComment1 then
    begin
      found := True;
      AssertEquals('comment col', 10, tokens[i].Column);
      AssertEquals('comment len', 25, tokens[i].Length);
      Break;
    end;
  AssertTrue('comment token found', found);
end;

procedure TTestPascalHighlighter.TestIncludeDirectiveContinues;
var
  src: string;
begin
  { Include directive should not stop tokenisation.
    Code after the include must still be highlighted. }
  src := '{$I somefile.inc}' + LineEnding +
         '// comment after include';
  FHL.Tokenise(src);
  { Line 0: the include directive }
  AssertTrue('include line has tokens', FHL.GetLineTokenCount(0) > 0);
  AssertTrue('include is directive',
    FHL.GetLineTokens(0)[0].Category = hcDirective);
  { Line 1: code after include must be tokenised }
  AssertTrue('line after include has tokens', FHL.GetLineTokenCount(1) > 0);
  AssertTrue('comment after include',
    FHL.GetLineTokens(1)[0].Category = hcComment1);
end;

procedure TTestPascalHighlighter.TestIfdefDirective;
var
  src: string;
begin
  { IFDEF with undefined symbol - the directive itself must still
     be classified as hcDirective, not hcComment1 }
  src := '{$IFDEF UNDEFINED_SYM}' + LineEnding +
         'x := 1;' + LineEnding +
         '{$ENDIF}';
  FHL.Tokenise(src);
  { Line 0: the IFDEF directive }
  AssertTrue('IFDEF line has tokens', FHL.GetLineTokenCount(0) > 0);
  AssertTrue('IFDEF is directive',
    FHL.GetLineTokens(0)[0].Category = hcDirective);
  { Line 2: the ENDIF directive }
  AssertTrue('ENDIF line has tokens', FHL.GetLineTokenCount(2) > 0);
  AssertTrue('ENDIF is directive',
    FHL.GetLineTokens(2)[0].Category = hcDirective);
end;

procedure TTestPascalHighlighter.TestIfdefSkippedContent;
var
  src: string;
  tokens: THighlightTokenArray;
begin
  (* Content between false IFDEF and ENDIF is skipped by the scanner.
     Verify we still get the directive tokens. *)
  src := '{$IFDEF NOSYMBOL}' + LineEnding +
         'writeln(''skipped'');' + LineEnding +
         '{$ENDIF}';
  FHL.Tokenise(src);
  { IFDEF on line 0 }
  tokens := FHL.GetLineTokens(0);
  AssertTrue('IFDEF token exists', tokens <> nil);
  AssertEquals('IFDEF col', 0, tokens[0].Column);
  AssertTrue('IFDEF category', tokens[0].Category = hcDirective);
  { ENDIF on line 2 }
  tokens := FHL.GetLineTokens(2);
  AssertTrue('ENDIF token exists', tokens <> nil);
  AssertTrue('ENDIF category', tokens[0].Category = hcDirective);
end;

procedure TTestPascalHighlighter.TestVisibilityModifiers;
begin
  { private, protected, public, published — visibility sections }
  FHL.Tokenise('private');
  CheckToken(0, 0, 0, 7, hcKeyword2, 'private modifier');

  FHL.Tokenise('protected');
  CheckToken(0, 0, 0, 9, hcKeyword2, 'protected modifier');

  FHL.Tokenise('public');
  CheckToken(0, 0, 0, 6, hcKeyword2, 'public modifier');

  FHL.Tokenise('published');
  CheckToken(0, 0, 0, 9, hcKeyword2, 'published modifier');

  FHL.Tokenise('strict');
  CheckToken(0, 0, 0, 6, hcKeyword2, 'strict modifier');
end;

procedure TTestPascalHighlighter.TestMethodModifiers;
begin
  { override, virtual, abstract, final, dynamic }
  FHL.Tokenise('override');
  CheckToken(0, 0, 0, 8, hcKeyword2, 'override modifier');

  FHL.Tokenise('virtual');
  CheckToken(0, 0, 0, 7, hcKeyword2, 'virtual modifier');

  FHL.Tokenise('abstract');
  CheckToken(0, 0, 0, 8, hcKeyword2, 'abstract modifier');

  FHL.Tokenise('final');
  CheckToken(0, 0, 0, 5, hcKeyword2, 'final modifier');

  FHL.Tokenise('dynamic');
  CheckToken(0, 0, 0, 7, hcKeyword2, 'dynamic modifier');

  FHL.Tokenise('reintroduce');
  CheckToken(0, 0, 0, 11, hcKeyword2, 'reintroduce modifier');

  FHL.Tokenise('overload');
  CheckToken(0, 0, 0, 8, hcKeyword2, 'overload modifier');
end;

procedure TTestPascalHighlighter.TestCallingConventions;
begin
  FHL.Tokenise('stdcall');
  CheckToken(0, 0, 0, 7, hcKeyword2, 'stdcall modifier');

  FHL.Tokenise('cdecl');
  CheckToken(0, 0, 0, 5, hcKeyword2, 'cdecl modifier');

  FHL.Tokenise('register');
  CheckToken(0, 0, 0, 8, hcKeyword2, 'register modifier');

  FHL.Tokenise('safecall');
  CheckToken(0, 0, 0, 8, hcKeyword2, 'safecall modifier');
end;

procedure TTestPascalHighlighter.TestModifierMixedWithKeyword;
var
  tokens: THighlightTokenArray;
  i: Integer;
  foundVirtual, foundOverride: Boolean;
begin
  { "procedure Foo; virtual; override;" — mix of keyword and modifiers.
    Token sequence (whitespace skipped): procedure Foo ; virtual ; override ;
    Indices:                              0         1   2 3       4 5        6 }
  FHL.Tokenise('procedure Foo; virtual; override;');
  tokens := FHL.GetLineTokens(0);
  AssertTrue('has tokens', Length(tokens) >= 7);
  AssertEquals('procedure is keyword1', Ord(hcKeyword1), Ord(tokens[0].Category));
  AssertEquals('virtual col',  15, tokens[3].Column);
  AssertEquals('virtual cat',  Ord(hcKeyword2), Ord(tokens[3].Category));
  AssertEquals('override col', 24, tokens[5].Column);
  AssertEquals('override cat', Ord(hcKeyword2), Ord(tokens[5].Category));
  { Also verify no modifier was misclassified as keyword1 }
  foundVirtual  := False;
  foundOverride := False;
  for i := 0 to High(tokens) do
  begin
    if tokens[i].Column = 15 then foundVirtual  := tokens[i].Category = hcKeyword2;
    if tokens[i].Column = 24 then foundOverride := tokens[i].Category = hcKeyword2;
  end;
  AssertTrue('virtual is hcKeyword2',  foundVirtual);
  AssertTrue('override is hcKeyword2', foundOverride);
end;

initialization
  RegisterTest(TTestPascalHighlighter);

end.
