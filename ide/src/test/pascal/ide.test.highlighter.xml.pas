{
    fpGUI IDE - XML Highlighter Tests

    Tests for TXMLHighlighter token positions and categories.
}
unit ide.test.highlighter.xml;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  ide.highlighter,
  ide.highlighter.xml;

type

  { TTestXMLHighlighter }

  TTestXMLHighlighter = class(TTestCase)
  private
    FHL: TXMLHighlighter;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSimpleTag;
    procedure TestClosingTag;
    procedure TestSelfClosingTag;
    procedure TestAttribute;
    procedure TestComment;
    procedure TestProcessingInstruction;
    procedure TestEntityReference;
    procedure TestMultiLineComment;
    procedure TestCDATA;
  end;


implementation

procedure TTestXMLHighlighter.SetUp;
begin
  FHL := TXMLHighlighter.Create;
end;

procedure TTestXMLHighlighter.TearDown;
begin
  FHL.Free;
end;

procedure TTestXMLHighlighter.TestSimpleTag;
var
  tokens: THighlightTokenArray;
begin
  FHL.Tokenise('<root>');
  tokens := FHL.GetLineTokens(0);
  CheckTrue(Length(tokens) >= 2, 'Should have at least 2 tokens (tag name, >)');
  CheckEquals(Ord(hcKeyword1), Ord(tokens[0].Category), 'Tag name should be hcKeyword1');
  CheckEquals(0, tokens[0].Column, 'Tag starts at column 0');
end;

procedure TTestXMLHighlighter.TestClosingTag;
var
  tokens: THighlightTokenArray;
begin
  FHL.Tokenise('</root>');
  tokens := FHL.GetLineTokens(0);
  CheckTrue(Length(tokens) >= 2, 'Should have at least 2 tokens');
  CheckEquals(Ord(hcKeyword1), Ord(tokens[0].Category), 'Closing tag should be hcKeyword1');
  CheckEquals(0, tokens[0].Column, 'Starts at column 0');
  { The token should include </root }
  CheckTrue(tokens[0].Length >= 6, 'Should include </root');
end;

procedure TTestXMLHighlighter.TestSelfClosingTag;
var
  tokens: THighlightTokenArray;
begin
  FHL.Tokenise('<br/>');
  tokens := FHL.GetLineTokens(0);
  CheckTrue(Length(tokens) >= 2, 'Should have at least 2 tokens');
  CheckEquals(Ord(hcKeyword1), Ord(tokens[0].Category), 'Tag name hcKeyword1');
  { Last token should be /> }
  CheckEquals(Ord(hcKeyword1), Ord(tokens[Length(tokens) - 1].Category),
    '/> should be hcKeyword1');
end;

procedure TTestXMLHighlighter.TestAttribute;
var
  tokens: THighlightTokenArray;
  i: Integer;
  hasIdent, hasString, hasSymbol: Boolean;
begin
  FHL.Tokenise('<div class="main">');
  tokens := FHL.GetLineTokens(0);
  hasIdent := False;
  hasString := False;
  hasSymbol := False;
  for i := 0 to Length(tokens) - 1 do
  begin
    if tokens[i].Category = hcIdentifier then
      hasIdent := True;
    if tokens[i].Category = hcString1 then
      hasString := True;
    if tokens[i].Category = hcSymbol then
      hasSymbol := True;
  end;
  CheckTrue(hasIdent, 'Should have attribute name (hcIdentifier)');
  CheckTrue(hasString, 'Should have attribute value (hcString1)');
  CheckTrue(hasSymbol, 'Should have = separator (hcSymbol)');
end;

procedure TTestXMLHighlighter.TestComment;
var
  tokens: THighlightTokenArray;
begin
  FHL.Tokenise('<!-- a comment -->');
  tokens := FHL.GetLineTokens(0);
  CheckEquals(1, Length(tokens), 'Should have 1 comment token');
  CheckEquals(Ord(hcComment1), Ord(tokens[0].Category), 'Should be hcComment1');
  CheckEquals(0, tokens[0].Column, 'Starts at column 0');
  CheckEquals(18, tokens[0].Length, 'Covers entire comment');
end;

procedure TTestXMLHighlighter.TestProcessingInstruction;
var
  tokens: THighlightTokenArray;
begin
  FHL.Tokenise('<?xml version="1.0"?>');
  tokens := FHL.GetLineTokens(0);
  CheckEquals(1, Length(tokens), 'Should have 1 PI token');
  CheckEquals(Ord(hcDirective), Ord(tokens[0].Category), 'Should be hcDirective');
end;

procedure TTestXMLHighlighter.TestEntityReference;
var
  tokens: THighlightTokenArray;
begin
  FHL.Tokenise('a &amp; b');
  tokens := FHL.GetLineTokens(0);
  CheckEquals(1, Length(tokens), 'Should have 1 entity token');
  CheckEquals(Ord(hcSymbol), Ord(tokens[0].Category), 'Entity should be hcSymbol');
  CheckEquals(2, tokens[0].Column, 'Entity starts at column 2');
  CheckEquals(5, tokens[0].Length, 'Entity length is 5 (&amp;)');
end;

procedure TTestXMLHighlighter.TestMultiLineComment;
var
  tokens: THighlightTokenArray;
begin
  FHL.Tokenise('<!-- start' + LineEnding + 'middle' + LineEnding + 'end -->');
  { Line 0: partial comment }
  tokens := FHL.GetLineTokens(0);
  CheckEquals(1, Length(tokens), 'Line 0 should have 1 token');
  CheckEquals(Ord(hcComment1), Ord(tokens[0].Category), 'Line 0 should be comment');
  { Line 1: continuation }
  tokens := FHL.GetLineTokens(1);
  CheckEquals(1, Length(tokens), 'Line 1 should have 1 token');
  CheckEquals(Ord(hcComment1), Ord(tokens[1 - 1].Category), 'Line 1 should be comment');
  { Line 2: closing }
  tokens := FHL.GetLineTokens(2);
  CheckEquals(1, Length(tokens), 'Line 2 should have 1 token');
  CheckEquals(Ord(hcComment1), Ord(tokens[0].Category), 'Line 2 should be comment');
end;

procedure TTestXMLHighlighter.TestCDATA;
var
  tokens: THighlightTokenArray;
begin
  FHL.Tokenise('<![CDATA[some data]]>');
  tokens := FHL.GetLineTokens(0);
  CheckEquals(1, Length(tokens), 'Should have 1 CDATA token');
  CheckEquals(Ord(hcDirective), Ord(tokens[0].Category), 'CDATA should be hcDirective');
end;

initialization
  RegisterTest(TTestXMLHighlighter);

end.
