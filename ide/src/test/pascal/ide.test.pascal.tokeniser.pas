{
    fpGUI IDE - Pascal Tokeniser Tests

    Copyright (C) 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Unit tests for TFpgPascalTokeniser. Tests are organised by token
      category: keywords, identifiers, numbers, strings, comments,
      directives, symbols/operators, and multi-line handling.
}
unit ide.test.pascal.tokeniser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  ide.pascal.tokeniser;

type

  { TTestPascalTokeniser }

  TTestPascalTokeniser = class(TTestCase)
  private
    FTok: TFpgPascalTokeniser;
    { Helper: collect all non-whitespace, non-line-ending tokens }
    procedure CollectTokens(const ASource: string;
      out AKinds: array of TFpgPasTokenKind;
      out ATexts: array of string;
      out ACount: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { --- Empty / EOF --- }
    procedure TestEmptySource;

    { --- Keywords --- }
    procedure TestSingleKeyword_Begin;
    procedure TestSingleKeyword_End;
    procedure TestSingleKeyword_Procedure;
    procedure TestSingleKeyword_Function;
    procedure TestSingleKeyword_Interface;
    procedure TestSingleKeyword_Generic;
    procedure TestSingleKeyword_Specialize;
    procedure TestSingleKeyword_Dispinterface;
    procedure TestKeyword_CaseInsensitive;
    procedure TestAllKeywordsRecognised;

    { --- Identifiers --- }
    procedure TestSimpleIdentifier;
    procedure TestIdentifier_WithUnderscore;
    procedure TestIdentifier_WithDigits;
    procedure TestIdentifier_NotKeyword;

    { --- Numbers --- }
    procedure TestNumber_Decimal;
    procedure TestNumber_Hex;
    procedure TestNumber_Binary;
    procedure TestNumber_Octal;
    procedure TestNumber_Float;
    procedure TestNumber_FloatExponent;

    { --- Strings --- }
    procedure TestString_Simple;
    procedure TestString_EmbeddedQuotes;
    procedure TestString_Empty;
    procedure TestString_CharHash;
    procedure TestString_CharCaret;
    procedure TestString_Concatenated;

    { --- Comments --- }
    procedure TestComment_LineComment;
    procedure TestComment_BraceComment;
    procedure TestComment_ParenStarComment;
    procedure TestComment_MultiLine_Brace;
    procedure TestComment_MultiLine_ParenStar;

    { --- Directives --- }
    procedure TestDirective_BraceStyle;
    procedure TestDirective_ParenStarStyle;

    { --- Symbols / Operators --- }
    procedure TestSymbol_SingleChars;
    procedure TestSymbol_Assign;
    procedure TestSymbol_NotEqual;
    procedure TestSymbol_LessEqual;
    procedure TestSymbol_GreaterEqual;
    procedure TestSymbol_DotDot;
    procedure TestSymbol_Power;
    procedure TestSymbol_AtAt;
    procedure TestSymbol_PlusAssign;

    { --- Line and Column Tracking --- }
    procedure TestLineColumn_FirstToken;
    procedure TestLineColumn_SecondLine;
    procedure TestLineColumn_MultipleTokensOnLine;

    { --- Incomplete / Error Recovery --- }
    procedure TestIncomplete_UnterminatedString;
    procedure TestIncomplete_UnterminatedBraceComment;
    procedure TestIncomplete_UnterminatedParenStarComment;
    procedure TestIncomplete_UnknownChar;

    { --- Mixed Source --- }
    procedure TestMixedSource_SimpleProgram;
    procedure TestMixedSource_UsesClause;

    { --- FpgPasIsKeyword --- }
    procedure TestIsKeyword_True;
    procedure TestIsKeyword_False;
    procedure TestIsKeyword_CaseInsensitive;

    { --- FpgPasIsModifier + fptkModifier token kind --- }
    procedure TestModifier_VisibilityTokens;
    procedure TestModifier_MethodModifierTokens;
    procedure TestModifier_CallingConventionTokens;
    procedure TestIsModifier_True;
    procedure TestIsModifier_False;
    procedure TestIsModifier_CaseInsensitive;
    procedure TestModifier_NotKeyword;
    procedure TestKeyword_NotModifier;
  end;


implementation

{ TTestPascalTokeniser }

procedure TTestPascalTokeniser.CollectTokens(const ASource: string;
  out AKinds: array of TFpgPasTokenKind;
  out ATexts: array of string;
  out ACount: Integer);
var
  tok: TFpgPasToken;
begin
  ACount := 0;
  FTok.SetSource(ASource);
  repeat
    tok := FTok.NextToken;
    if tok.Kind in [fptkWhitespace, fptkLineEnding] then
      Continue;
    if tok.Kind = fptkEOF then
      Break;
    if ACount <= High(AKinds) then
    begin
      AKinds[ACount] := tok.Kind;
      ATexts[ACount] := FTok.TokenText;
    end;
    Inc(ACount);
  until False;
end;

procedure TTestPascalTokeniser.SetUp;
begin
  FTok := TFpgPascalTokeniser.Create;
end;

procedure TTestPascalTokeniser.TearDown;
begin
  FTok.Free;
end;

{ --- Empty / EOF --- }

procedure TTestPascalTokeniser.TestEmptySource;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('');
  tok := FTok.NextToken;
  AssertEquals('Empty source should yield EOF', Ord(fptkEOF), Ord(tok.Kind));
end;

{ --- Keywords --- }

procedure TTestPascalTokeniser.TestSingleKeyword_Begin;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('begin');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkKeyword), Ord(tok.Kind));
  AssertEquals('Text', 'begin', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestSingleKeyword_End;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('end');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkKeyword), Ord(tok.Kind));
  AssertEquals('Text', 'end', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestSingleKeyword_Procedure;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('procedure');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkKeyword), Ord(tok.Kind));
end;

procedure TTestPascalTokeniser.TestSingleKeyword_Function;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('function');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkKeyword), Ord(tok.Kind));
end;

procedure TTestPascalTokeniser.TestSingleKeyword_Interface;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('interface');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkKeyword), Ord(tok.Kind));
end;

procedure TTestPascalTokeniser.TestSingleKeyword_Generic;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('generic');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkKeyword), Ord(tok.Kind));
end;

procedure TTestPascalTokeniser.TestSingleKeyword_Specialize;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('specialize');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkKeyword), Ord(tok.Kind));
end;

procedure TTestPascalTokeniser.TestSingleKeyword_Dispinterface;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('dispinterface');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkKeyword), Ord(tok.Kind));
end;

procedure TTestPascalTokeniser.TestKeyword_CaseInsensitive;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('BEGIN');
  tok := FTok.NextToken;
  AssertEquals('Uppercase', Ord(fptkKeyword), Ord(tok.Kind));
  AssertEquals('Preserves case', 'BEGIN', FTok.TokenText);

  FTok.SetSource('Begin');
  tok := FTok.NextToken;
  AssertEquals('Mixed case', Ord(fptkKeyword), Ord(tok.Kind));
  AssertEquals('Preserves case', 'Begin', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestAllKeywordsRecognised;
const
  Keywords: array[0..78] of string = (
    'absolute', 'and', 'array', 'as', 'asm', 'begin', 'bitpacked',
    'case', 'class', 'const', 'constref', 'constructor', 'contains',
    'destructor', 'dispinterface', 'div', 'do', 'downto',
    'else', 'end', 'except', 'exports', 'false', 'file',
    'finalization', 'finally', 'for', 'function',
    'generic', 'goto',
    'if', 'implementation', 'in', 'inherited', 'initialization',
    'inline', 'interface', 'is',
    'label', 'library',
    'mod', 'nil', 'not',
    'objccategory', 'objcclass', 'objcprotocol', 'object', 'of',
    'operator', 'or', 'otherwise',
    'package', 'packed', 'procedure', 'program', 'property',
    'raise', 'record', 'repeat', 'requires', 'resourcestring',
    'self', 'set', 'shl', 'shr', 'specialize',
    'then', 'threadvar', 'to', 'true', 'try', 'type',
    'unit', 'until', 'uses', 'var', 'while', 'with', 'xor'
  );
var
  i: Integer;
  tok: TFpgPasToken;
begin
  for i := Low(Keywords) to High(Keywords) do
  begin
    FTok.SetSource(Keywords[i]);
    tok := FTok.NextToken;
    AssertEquals('Keyword "' + Keywords[i] + '" should be fptkKeyword',
      Ord(fptkKeyword), Ord(tok.Kind));
  end;
end;

{ --- Identifiers --- }

procedure TTestPascalTokeniser.TestSimpleIdentifier;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('MyVar');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkIdentifier), Ord(tok.Kind));
  AssertEquals('Text', 'MyVar', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestIdentifier_WithUnderscore;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('_private_field');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkIdentifier), Ord(tok.Kind));
  AssertEquals('Text', '_private_field', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestIdentifier_WithDigits;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('Item2Count');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkIdentifier), Ord(tok.Kind));
  AssertEquals('Text', 'Item2Count', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestIdentifier_NotKeyword;
var
  tok: TFpgPasToken;
begin
  { 'beginning' starts with 'begin' but is not a keyword }
  FTok.SetSource('beginning');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkIdentifier), Ord(tok.Kind));
  AssertEquals('Text', 'beginning', FTok.TokenText);
end;

{ --- Numbers --- }

procedure TTestPascalTokeniser.TestNumber_Decimal;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('42');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkNumber), Ord(tok.Kind));
  AssertEquals('Text', '42', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestNumber_Hex;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('$FF');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkNumber), Ord(tok.Kind));
  AssertEquals('Text', '$FF', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestNumber_Binary;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('%10110');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkNumber), Ord(tok.Kind));
  AssertEquals('Text', '%10110', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestNumber_Octal;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('&77');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkNumber), Ord(tok.Kind));
  AssertEquals('Text', '&77', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestNumber_Float;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('3.14');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkNumber), Ord(tok.Kind));
  AssertEquals('Text', '3.14', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestNumber_FloatExponent;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('1.5e10');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkNumber), Ord(tok.Kind));
  AssertEquals('Text', '1.5e10', FTok.TokenText);
end;

{ --- Strings --- }

procedure TTestPascalTokeniser.TestString_Simple;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('''hello''');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkString), Ord(tok.Kind));
  AssertEquals('Text', '''hello''', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestString_EmbeddedQuotes;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('''it''''s''');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkString), Ord(tok.Kind));
  AssertEquals('Text', '''it''''s''', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestString_Empty;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('''''');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkString), Ord(tok.Kind));
  AssertEquals('Text', '''''', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestString_CharHash;
var
  tok: TFpgPasToken;
begin
  { #13#10 should be a single string token }
  FTok.SetSource('#13#10');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkString), Ord(tok.Kind));
  AssertEquals('Text', '#13#10', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestString_CharCaret;
var
  tok: TFpgPasToken;
begin
  { ^M = control character }
  FTok.SetSource('^M');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkString), Ord(tok.Kind));
  AssertEquals('Text', '^M', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestString_Concatenated;
var
  tok: TFpgPasToken;
begin
  { 'abc'#13#10'def' should be a single string token }
  FTok.SetSource('''abc''#13#10''def''');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkString), Ord(tok.Kind));
  AssertEquals('Text', '''abc''#13#10''def''', FTok.TokenText);
end;

{ --- Comments --- }

procedure TTestPascalTokeniser.TestComment_LineComment;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('// this is a comment');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkComment), Ord(tok.Kind));
  AssertEquals('Text', '// this is a comment', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestComment_BraceComment;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('{ a comment }');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkComment), Ord(tok.Kind));
  AssertEquals('Text', '{ a comment }', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestComment_ParenStarComment;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('(* a comment *)');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkComment), Ord(tok.Kind));
  AssertEquals('Text', '(* a comment *)', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestComment_MultiLine_Brace;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('{ line1' + LineEnding + '  line2 }');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkComment), Ord(tok.Kind));
  AssertTrue('Contains line ending', Pos(LineEnding, FTok.TokenText) > 0);
end;

procedure TTestPascalTokeniser.TestComment_MultiLine_ParenStar;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('(* line1' + LineEnding + '   line2 *)');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkComment), Ord(tok.Kind));
  AssertTrue('Contains line ending', Pos(LineEnding, FTok.TokenText) > 0);
end;

{ --- Directives --- }

procedure TTestPascalTokeniser.TestDirective_BraceStyle;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('{$mode objfpc}');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkDirective), Ord(tok.Kind));
  AssertEquals('Text', '{$mode objfpc}', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestDirective_ParenStarStyle;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('(*$IFDEF UNIX*)');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkDirective), Ord(tok.Kind));
  AssertEquals('Text', '(*$IFDEF UNIX*)', FTok.TokenText);
end;

{ --- Symbols / Operators --- }

procedure TTestPascalTokeniser.TestSymbol_SingleChars;
const
  Chars: array[0..17] of Char = (
    '(', ')', '*', '+', ',', '-', '.', '/', ':', ';',
    '<', '=', '>', '@', '[', ']', '^', '\'
  );
var
  i: Integer;
  tok: TFpgPasToken;
begin
  for i := Low(Chars) to High(Chars) do
  begin
    FTok.SetSource(Chars[i]);
    tok := FTok.NextToken;
    AssertEquals('Char "' + Chars[i] + '" kind',
      Ord(fptkSymbol), Ord(tok.Kind));
    AssertEquals('Char "' + Chars[i] + '" text',
      Chars[i], FTok.TokenText);
  end;
end;

procedure TTestPascalTokeniser.TestSymbol_Assign;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource(':=');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkSymbol), Ord(tok.Kind));
  AssertEquals('Text', ':=', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestSymbol_NotEqual;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('<>');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkSymbol), Ord(tok.Kind));
  AssertEquals('Text', '<>', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestSymbol_LessEqual;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('<=');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkSymbol), Ord(tok.Kind));
  AssertEquals('Text', '<=', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestSymbol_GreaterEqual;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('>=');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkSymbol), Ord(tok.Kind));
  AssertEquals('Text', '>=', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestSymbol_DotDot;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('..');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkSymbol), Ord(tok.Kind));
  AssertEquals('Text', '..', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestSymbol_Power;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('**');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkSymbol), Ord(tok.Kind));
  AssertEquals('Text', '**', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestSymbol_AtAt;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('@@');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkSymbol), Ord(tok.Kind));
  AssertEquals('Text', '@@', FTok.TokenText);
end;

procedure TTestPascalTokeniser.TestSymbol_PlusAssign;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('+=');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkSymbol), Ord(tok.Kind));
  AssertEquals('Text', '+=', FTok.TokenText);
end;

{ --- Line and Column Tracking --- }

procedure TTestPascalTokeniser.TestLineColumn_FirstToken;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('begin');
  tok := FTok.NextToken;
  AssertEquals('Line', 1, tok.Line);
  AssertEquals('Column', 1, tok.Column);
end;

procedure TTestPascalTokeniser.TestLineColumn_SecondLine;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('begin' + LineEnding + 'end');
  tok := FTok.NextToken;  { begin }
  AssertEquals('begin line', 1, tok.Line);
  tok := FTok.NextToken;  { line ending }
  tok := FTok.NextToken;  { end }
  AssertEquals('end line', 2, tok.Line);
  AssertEquals('end column', 1, tok.Column);
end;

procedure TTestPascalTokeniser.TestLineColumn_MultipleTokensOnLine;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('var x: Integer;');
  tok := FTok.NextToken;  { var }
  AssertEquals('var col', 1, tok.Column);
  tok := FTok.NextToken;  { whitespace }
  tok := FTok.NextToken;  { x }
  AssertEquals('x col', 5, tok.Column);
  tok := FTok.NextToken;  { : }
  AssertEquals(': col', 6, tok.Column);
end;

{ --- Incomplete / Error Recovery --- }

procedure TTestPascalTokeniser.TestIncomplete_UnterminatedString;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('''hello');
  tok := FTok.NextToken;
  { Should return a string token for what it has, not crash }
  AssertEquals('Kind', Ord(fptkString), Ord(tok.Kind));
end;

procedure TTestPascalTokeniser.TestIncomplete_UnterminatedBraceComment;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('{ unterminated');
  tok := FTok.NextToken;
  { Should return a comment token, not crash }
  AssertEquals('Kind', Ord(fptkComment), Ord(tok.Kind));
end;

procedure TTestPascalTokeniser.TestIncomplete_UnterminatedParenStarComment;
var
  tok: TFpgPasToken;
begin
  FTok.SetSource('(* unterminated');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkComment), Ord(tok.Kind));
end;

procedure TTestPascalTokeniser.TestIncomplete_UnknownChar;
var
  tok: TFpgPasToken;
begin
  { Tilde is not a Pascal token }
  FTok.SetSource('~');
  tok := FTok.NextToken;
  AssertEquals('Kind', Ord(fptkSymbol), Ord(tok.Kind));
  AssertEquals('Len', 1, tok.Len);
end;

{ --- Mixed Source --- }

procedure TTestPascalTokeniser.TestMixedSource_SimpleProgram;
var
  kinds: array[0..19] of TFpgPasTokenKind;
  texts: array[0..19] of string;
  count: Integer;
begin
  CollectTokens(
    'program Test;' + LineEnding +
    'begin' + LineEnding +
    'end.',
    kinds, texts, count);
  AssertTrue('At least 5 tokens', count >= 5);
  AssertEquals('program', Ord(fptkKeyword), Ord(kinds[0]));
  AssertEquals('Test', Ord(fptkIdentifier), Ord(kinds[1]));
  AssertEquals(';', Ord(fptkSymbol), Ord(kinds[2]));
  AssertEquals('begin', Ord(fptkKeyword), Ord(kinds[3]));
  AssertEquals('end', Ord(fptkKeyword), Ord(kinds[4]));
end;

procedure TTestPascalTokeniser.TestMixedSource_UsesClause;
var
  kinds: array[0..19] of TFpgPasTokenKind;
  texts: array[0..19] of string;
  count: Integer;
begin
  CollectTokens('uses SysUtils, Classes;', kinds, texts, count);
  AssertEquals('Count', 5, count);
  AssertEquals('uses', Ord(fptkKeyword), Ord(kinds[0]));
  AssertEquals('SysUtils', texts[1]);
  AssertEquals(',', Ord(fptkSymbol), Ord(kinds[2]));
  AssertEquals('Classes', texts[3]);
  AssertEquals(';', Ord(fptkSymbol), Ord(kinds[4]));
end;

{ --- FpgPasIsKeyword --- }

procedure TTestPascalTokeniser.TestIsKeyword_True;
begin
  AssertTrue('begin is keyword', FpgPasIsKeyword('begin'));
  AssertTrue('procedure is keyword', FpgPasIsKeyword('procedure'));
  AssertTrue('interface is keyword', FpgPasIsKeyword('interface'));
end;

procedure TTestPascalTokeniser.TestIsKeyword_False;
begin
  AssertFalse('MyVar is not keyword', FpgPasIsKeyword('MyVar'));
  AssertFalse('empty is not keyword', FpgPasIsKeyword(''));
  AssertFalse('beginning is not keyword', FpgPasIsKeyword('beginning'));
end;

procedure TTestPascalTokeniser.TestIsKeyword_CaseInsensitive;
begin
  AssertTrue('BEGIN', FpgPasIsKeyword('BEGIN'));
  AssertTrue('Begin', FpgPasIsKeyword('Begin'));
  AssertTrue('bEgIn', FpgPasIsKeyword('bEgIn'));
end;


{ --- FpgPasIsModifier + fptkModifier token kind --- }

procedure TTestPascalTokeniser.TestModifier_VisibilityTokens;
var
  kinds: array of TFpgPasTokenKind;
  texts: array of string;
begin
  CollectTokens('private', kinds, texts);
  AssertEquals('private count', 1, Length(kinds));
  AssertEquals('private kind', Ord(fptkModifier), Ord(kinds[0]));
  AssertEquals('private text', 'private', texts[0]);

  CollectTokens('protected', kinds, texts);
  AssertEquals('protected kind', Ord(fptkModifier), Ord(kinds[0]));

  CollectTokens('public', kinds, texts);
  AssertEquals('public kind', Ord(fptkModifier), Ord(kinds[0]));

  CollectTokens('published', kinds, texts);
  AssertEquals('published kind', Ord(fptkModifier), Ord(kinds[0]));

  CollectTokens('strict', kinds, texts);
  AssertEquals('strict kind', Ord(fptkModifier), Ord(kinds[0]));
end;

procedure TTestPascalTokeniser.TestModifier_MethodModifierTokens;
var
  kinds: array of TFpgPasTokenKind;
  texts: array of string;
begin
  CollectTokens('override', kinds, texts);
  AssertEquals('override kind', Ord(fptkModifier), Ord(kinds[0]));

  CollectTokens('virtual', kinds, texts);
  AssertEquals('virtual kind', Ord(fptkModifier), Ord(kinds[0]));

  CollectTokens('abstract', kinds, texts);
  AssertEquals('abstract kind', Ord(fptkModifier), Ord(kinds[0]));

  CollectTokens('final', kinds, texts);
  AssertEquals('final kind', Ord(fptkModifier), Ord(kinds[0]));

  CollectTokens('overload', kinds, texts);
  AssertEquals('overload kind', Ord(fptkModifier), Ord(kinds[0]));

  CollectTokens('reintroduce', kinds, texts);
  AssertEquals('reintroduce kind', Ord(fptkModifier), Ord(kinds[0]));
end;

procedure TTestPascalTokeniser.TestModifier_CallingConventionTokens;
var
  kinds: array of TFpgPasTokenKind;
  texts: array of string;
begin
  CollectTokens('stdcall', kinds, texts);
  AssertEquals('stdcall kind', Ord(fptkModifier), Ord(kinds[0]));

  CollectTokens('cdecl', kinds, texts);
  AssertEquals('cdecl kind', Ord(fptkModifier), Ord(kinds[0]));

  CollectTokens('register', kinds, texts);
  AssertEquals('register kind', Ord(fptkModifier), Ord(kinds[0]));

  CollectTokens('safecall', kinds, texts);
  AssertEquals('safecall kind', Ord(fptkModifier), Ord(kinds[0]));
end;

procedure TTestPascalTokeniser.TestIsModifier_True;
begin
  AssertTrue('private is modifier',    FpgPasIsModifier('private'));
  AssertTrue('override is modifier',   FpgPasIsModifier('override'));
  AssertTrue('stdcall is modifier',    FpgPasIsModifier('stdcall'));
  AssertTrue('virtual is modifier',    FpgPasIsModifier('virtual'));
  AssertTrue('deprecated is modifier', FpgPasIsModifier('deprecated'));
  AssertTrue('external is modifier',   FpgPasIsModifier('external'));
end;

procedure TTestPascalTokeniser.TestIsModifier_False;
begin
  AssertFalse('MyVar is not modifier', FpgPasIsModifier('MyVar'));
  AssertFalse('empty is not modifier', FpgPasIsModifier(''));
  AssertFalse('overrideX not modifier', FpgPasIsModifier('overrideX'));
end;

procedure TTestPascalTokeniser.TestIsModifier_CaseInsensitive;
begin
  AssertTrue('PRIVATE',   FpgPasIsModifier('PRIVATE'));
  AssertTrue('Private',   FpgPasIsModifier('Private'));
  AssertTrue('OVERRIDE',  FpgPasIsModifier('OVERRIDE'));
  AssertTrue('StdCall',   FpgPasIsModifier('StdCall'));
end;

procedure TTestPascalTokeniser.TestModifier_NotKeyword;
begin
  { Modifier words must not be classified as keywords }
  AssertFalse('private not keyword',  FpgPasIsKeyword('private'));
  AssertFalse('override not keyword', FpgPasIsKeyword('override'));
  AssertFalse('virtual not keyword',  FpgPasIsKeyword('virtual'));
  AssertFalse('stdcall not keyword',  FpgPasIsKeyword('stdcall'));
end;

procedure TTestPascalTokeniser.TestKeyword_NotModifier;
begin
  { Reserved words must not be classified as modifiers }
  AssertFalse('begin not modifier',     FpgPasIsModifier('begin'));
  AssertFalse('procedure not modifier', FpgPasIsModifier('procedure'));
  AssertFalse('class not modifier',     FpgPasIsModifier('class'));
  AssertFalse('interface not modifier', FpgPasIsModifier('interface'));
end;

initialization
  RegisterTest(TTestPascalTokeniser);

end.
