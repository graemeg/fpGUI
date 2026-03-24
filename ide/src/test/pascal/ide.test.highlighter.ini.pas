{
    fpGUI IDE - INI Highlighter Tests

    Tests for TINIHighlighter token positions and categories.
}
unit ide.test.highlighter.ini;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  ide.highlighter,
  ide.highlighter.ini;

type

  { TTestINIHighlighter }

  TTestINIHighlighter = class(TTestCase)
  private
    FHL: TINIHighlighter;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSectionHeader;
    procedure TestKeyValue;
    procedure TestCommentSemicolon;
    procedure TestCommentHash;
    procedure TestEmptyLines;
    procedure TestKeyOnly;
  end;


implementation

procedure TTestINIHighlighter.SetUp;
begin
  FHL := TINIHighlighter.Create;
end;

procedure TTestINIHighlighter.TearDown;
begin
  FHL.Free;
end;

procedure TTestINIHighlighter.TestSectionHeader;
var
  tokens: THighlightTokenArray;
begin
  FHL.Tokenise('[General]');
  tokens := FHL.GetLineTokens(0);
  CheckEquals(1, Length(tokens), 'Should have 1 token');
  CheckEquals(0, tokens[0].Column, 'Column');
  CheckEquals(9, tokens[0].Length, 'Length');
  CheckEquals(Ord(hcKeyword1), Ord(tokens[0].Category), 'Category should be hcKeyword1');
end;

procedure TTestINIHighlighter.TestKeyValue;
var
  tokens: THighlightTokenArray;
begin
  FHL.Tokenise('name=John');
  tokens := FHL.GetLineTokens(0);
  CheckEquals(3, Length(tokens), 'Should have 3 tokens (key, =, value)');
  CheckEquals(Ord(hcIdentifier), Ord(tokens[0].Category), 'Key should be hcIdentifier');
  CheckEquals(0, tokens[0].Column, 'Key column');
  CheckEquals(4, tokens[0].Length, 'Key length');
  CheckEquals(Ord(hcSymbol), Ord(tokens[1].Category), '= should be hcSymbol');
  CheckEquals(Ord(hcString1), Ord(tokens[2].Category), 'Value should be hcString1');
end;

procedure TTestINIHighlighter.TestCommentSemicolon;
var
  tokens: THighlightTokenArray;
begin
  FHL.Tokenise('; this is a comment');
  tokens := FHL.GetLineTokens(0);
  CheckEquals(1, Length(tokens), 'Should have 1 token');
  CheckEquals(Ord(hcComment1), Ord(tokens[0].Category), 'Should be hcComment1');
  CheckEquals(0, tokens[0].Column, 'Column');
  CheckEquals(19, tokens[0].Length, 'Length covers entire line');
end;

procedure TTestINIHighlighter.TestCommentHash;
var
  tokens: THighlightTokenArray;
begin
  FHL.Tokenise('# comment');
  tokens := FHL.GetLineTokens(0);
  CheckEquals(1, Length(tokens), 'Should have 1 token');
  CheckEquals(Ord(hcComment1), Ord(tokens[0].Category), 'Should be hcComment1');
end;

procedure TTestINIHighlighter.TestEmptyLines;
begin
  FHL.Tokenise('' + LineEnding + '' + LineEnding + '[Section]');
  CheckEquals(0, FHL.GetLineTokenCount(0), 'Line 0 should have no tokens');
  CheckEquals(0, FHL.GetLineTokenCount(1), 'Line 1 should have no tokens');
  CheckEquals(1, FHL.GetLineTokenCount(2), 'Line 2 should have 1 token');
end;

procedure TTestINIHighlighter.TestKeyOnly;
var
  tokens: THighlightTokenArray;
begin
  { A line with just a key and no = sign }
  FHL.Tokenise('orphankey');
  tokens := FHL.GetLineTokens(0);
  CheckEquals(1, Length(tokens), 'Should have 1 token');
  CheckEquals(Ord(hcIdentifier), Ord(tokens[0].Category), 'Should be hcIdentifier');
end;

initialization
  RegisterTest(TTestINIHighlighter);

end.
