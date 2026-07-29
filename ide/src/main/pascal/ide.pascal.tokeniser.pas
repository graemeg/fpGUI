{
    fpGUI IDE - Pascal Tokeniser

    Copyright (C) 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Lightweight Object Pascal tokeniser for IDE use. Operates on a
      string buffer and yields tokens one at a time via NextToken.
      No exceptions on malformed source -- unrecognised characters
      produce fptkSymbol tokens of length 1, and unterminated strings
      or comments consume to end-of-source.

      Does NOT evaluate compiler directives or IFDEF branches --
      everything is tokenised literally.
}
unit ide.pascal.tokeniser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFpgPasTokenKind = (
    fptkEOF,
    fptkWhitespace,
    fptkLineEnding,
    fptkIdentifier,
    fptkKeyword,
    fptkModifier,     // context-sensitive directives: private, override, stdcall, …
    fptkNumber,
    fptkString,
    fptkComment,
    fptkDirective,
    fptkSymbol
  );

  TFpgPasToken = record
    Kind: TFpgPasTokenKind;
    Line: Integer;       { 1-based line number }
    Column: Integer;     { 1-based column }
    Len: Integer;        { character length in source }
    TextStart: Integer;  { 1-based index into source string }
  end;

  { TFpgPascalTokeniser }

  TFpgPascalTokeniser = class(TObject)
  private
    FSource: string;
    FPos: Integer;
    FLine: Integer;
    FLineStart: Integer;  { FPos value at start of current line }
    FToken: TFpgPasToken;
    function Peek: Char; inline;
    function PeekAt(AOffset: Integer): Char; inline;
    procedure Advance; inline;
    procedure AdvanceLine;
    procedure ReadWhitespace;
    procedure ReadLineEnding;
    procedure ReadIdentifierOrKeyword;
    procedure ReadNumber;
    procedure ReadString;
    procedure ReadBraceCommentOrDirective;
    procedure ReadParenStarCommentOrDirective;
    procedure ReadLineComment;
    procedure ReadSymbol;
  public
    constructor Create;
    procedure SetSource(const ASource: string);
    function NextToken: TFpgPasToken;
    function TokenText: string;
    function TokenTextUpper: string;
    property Token: TFpgPasToken read FToken;
    property Source: string read FSource;
  end;

{ Returns True if AText is a Pascal keyword (case-insensitive). }
function FpgPasIsKeyword(const AText: string): Boolean;

{ Returns True if AText is a Pascal modifier/directive (case-insensitive).
  Modifiers are context-sensitive words that are not reserved but carry
  structural meaning: visibility (private, protected), method qualifiers
  (override, virtual, abstract), calling conventions (stdcall, cdecl), etc. }
function FpgPasIsModifier(const AText: string): Boolean;


implementation

const
  { Sorted keyword list for binary search }
  KeywordCount = 79;
  Keywords: array[0..KeywordCount - 1] of string = (
    'ABSOLUTE', 'AND', 'ARRAY', 'AS', 'ASM',
    'BEGIN', 'BITPACKED',
    'CASE', 'CLASS', 'CONST', 'CONSTREF', 'CONSTRUCTOR', 'CONTAINS',
    'DESTRUCTOR', 'DISPINTERFACE', 'DIV', 'DO', 'DOWNTO',
    'ELSE', 'END', 'EXCEPT', 'EXPORTS',
    'FALSE', 'FILE', 'FINALIZATION', 'FINALLY', 'FOR', 'FUNCTION',
    'GENERIC', 'GOTO',
    'IF', 'IMPLEMENTATION', 'IN', 'INHERITED', 'INITIALIZATION',
    'INLINE', 'INTERFACE', 'IS',
    'LABEL', 'LIBRARY',
    'MOD',
    'NIL', 'NOT',
    'OBJCCATEGORY', 'OBJCCLASS', 'OBJCPROTOCOL', 'OBJECT', 'OF',
    'OPERATOR', 'OR', 'OTHERWISE',
    'PACKAGE', 'PACKED', 'PROCEDURE', 'PROGRAM', 'PROPERTY',
    'RAISE', 'RECORD', 'REPEAT', 'REQUIRES', 'RESOURCESTRING',
    'SELF', 'SET', 'SHL', 'SHR', 'SPECIALIZE',
    'THEN', 'THREADVAR', 'TO', 'TRUE', 'TRY', 'TYPE',
    'UNIT', 'UNTIL', 'USES',
    'VAR',
    'WHILE', 'WITH',
    'XOR'
  );

function BinarySearchKeyword(const AText: string): Boolean;
var
  Lo, Hi, Mid, Cmp: Integer;
begin
  Result := False;
  Lo := 0;
  Hi := KeywordCount - 1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) shr 1;
    Cmp := CompareStr(AText, Keywords[Mid]);
    if Cmp < 0 then
      Hi := Mid - 1
    else if Cmp > 0 then
      Lo := Mid + 1
    else
      Exit(True);
  end;
end;

const
  { Sorted list of context-sensitive Pascal modifier/directive words.
    These are not reserved words but have structural meaning in class,
    object, interface, and procedure declarations. }
  ModifierCount = 41;
  Modifiers: array[0..ModifierCount - 1] of string = (
    'ABSTRACT', 'ASSEMBLER',
    'CDECL',
    'DEFAULT', 'DEPRECATED', 'DISPID', 'DYNAMIC',
    'EXPERIMENTAL', 'EXPORT', 'EXTERNAL',
    'FAR', 'FINAL', 'FORWARD',
    'IMPLEMENTS', 'INDEX', 'INTERRUPT', 'IOCHECK',
    'MESSAGE',
    'NEAR', 'NODEFAULT', 'NORETURN',
    'OVERLOAD', 'OVERRIDE',
    'PASCAL', 'PLATFORM', 'PRIVATE', 'PROTECTED', 'PUBLIC', 'PUBLISHED',
    'READ', 'READONLY', 'REGISTER', 'REINTRODUCE',
    'SAFECALL', 'STDCALL', 'STORED', 'STRICT',
    'UNIMPLEMENTED',
    'VARARGS', 'VIRTUAL',
    'WRITE'
  );

function BinarySearchModifier(const AText: string): Boolean;
var
  Lo, Hi, Mid, Cmp: Integer;
begin
  Result := False;
  Lo := 0;
  Hi := ModifierCount - 1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) shr 1;
    Cmp := CompareStr(AText, Modifiers[Mid]);
    if Cmp < 0 then
      Hi := Mid - 1
    else if Cmp > 0 then
      Lo := Mid + 1
    else
      Exit(True);
  end;
end;

function FpgPasIsKeyword(const AText: string): Boolean;
begin
  if AText = '' then
    Exit(False);
  Result := BinarySearchKeyword(UpCase(AText));
end;

function FpgPasIsModifier(const AText: string): Boolean;
begin
  if AText = '' then
    Exit(False);
  Result := BinarySearchModifier(UpCase(AText));
end;

{ TFpgPascalTokeniser }

constructor TFpgPascalTokeniser.Create;
begin
  inherited Create;
  FSource := '';
  FPos := 1;
  FLine := 1;
  FLineStart := 1;
end;

procedure TFpgPascalTokeniser.SetSource(const ASource: string);
begin
  FSource := ASource;
  FPos := 1;
  FLine := 1;
  FLineStart := 1;
  FToken.Kind := fptkEOF;
  FToken.Line := 1;
  FToken.Column := 1;
  FToken.Len := 0;
  FToken.TextStart := 1;
end;

function TFpgPascalTokeniser.Peek: Char;
begin
  if FPos <= Length(FSource) then
    Result := FSource[FPos]
  else
    Result := #0;
end;

function TFpgPascalTokeniser.PeekAt(AOffset: Integer): Char;
var
  p: Integer;
begin
  p := FPos + AOffset;
  if (p >= 1) and (p <= Length(FSource)) then
    Result := FSource[p]
  else
    Result := #0;
end;

procedure TFpgPascalTokeniser.Advance;
begin
  Inc(FPos);
end;

procedure TFpgPascalTokeniser.AdvanceLine;
begin
  Inc(FLine);
  FLineStart := FPos;
end;

procedure TFpgPascalTokeniser.ReadWhitespace;
begin
  FToken.Kind := fptkWhitespace;
  while (FPos <= Length(FSource)) and (FSource[FPos] in [' ', #9]) do
    Advance;
  FToken.Len := FPos - FToken.TextStart;
end;

procedure TFpgPascalTokeniser.ReadLineEnding;
begin
  FToken.Kind := fptkLineEnding;
  if (FSource[FPos] = #13) and (PeekAt(1) = #10) then
    Advance;  { consume CR of CRLF }
  Advance;    { consume LF (or lone CR) }
  FToken.Len := FPos - FToken.TextStart;
  AdvanceLine;
end;

procedure TFpgPascalTokeniser.ReadIdentifierOrKeyword;
var
  Upper: string;
begin
  while (FPos <= Length(FSource)) and
        (FSource[FPos] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
    Advance;
  FToken.Len := FPos - FToken.TextStart;
  Upper := UpCase(TokenText);
  if BinarySearchKeyword(Upper) then
    FToken.Kind := fptkKeyword
  else if BinarySearchModifier(Upper) then
    FToken.Kind := fptkModifier
  else
    FToken.Kind := fptkIdentifier;
end;

procedure TFpgPascalTokeniser.ReadNumber;
var
  c: Char;
begin
  FToken.Kind := fptkNumber;
  c := FSource[FPos];

  if c = '$' then
  begin
    { Hex: $[0-9A-Fa-f]+ }
    Advance;
    while (FPos <= Length(FSource)) and
          (FSource[FPos] in ['0'..'9', 'A'..'F', 'a'..'f']) do
      Advance;
    FToken.Len := FPos - FToken.TextStart;
    Exit;
  end;

  if c = '%' then
  begin
    { Binary: %[01]+ }
    Advance;
    while (FPos <= Length(FSource)) and (FSource[FPos] in ['0', '1']) do
      Advance;
    FToken.Len := FPos - FToken.TextStart;
    Exit;
  end;

  if c = '&' then
  begin
    { Octal: &[0-7]+ }
    Advance;
    while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'7']) do
      Advance;
    FToken.Len := FPos - FToken.TextStart;
    Exit;
  end;

  { Decimal integer or float }
  while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9']) do
    Advance;

  { Check for decimal point (but not '..') }
  if (FPos <= Length(FSource)) and (FSource[FPos] = '.') and
     (PeekAt(1) <> '.') then
  begin
    Advance;  { consume '.' }
    while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9']) do
      Advance;
  end;

  { Check for exponent }
  if (FPos <= Length(FSource)) and (FSource[FPos] in ['e', 'E']) then
  begin
    Advance;
    if (FPos <= Length(FSource)) and (FSource[FPos] in ['+', '-']) then
      Advance;
    while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9']) do
      Advance;
  end;

  FToken.Len := FPos - FToken.TextStart;
end;

procedure TFpgPascalTokeniser.ReadString;
var
  c: Char;
begin
  { Pascal string literals can be composed of:
    - 'quoted text' (with '' for embedded quotes)
    - #nn (decimal char code)
    - #$nn (hex char code)
    - ^A (control char)
    These can be concatenated without operators: 'abc'#13#10'def' }
  FToken.Kind := fptkString;

  repeat
    c := Peek;
    if c = '''' then
    begin
      Advance;  { opening quote }
      while FPos <= Length(FSource) do
      begin
        if FSource[FPos] = '''' then
        begin
          Advance;
          { Embedded quote? '' }
          if (FPos <= Length(FSource)) and (FSource[FPos] = '''') then
            Advance
          else
            Break;  { closing quote }
        end
        else if FSource[FPos] in [#10, #13] then
          Break  { unterminated string at line end }
        else
          Advance;
      end;
    end
    else if c = '#' then
    begin
      Advance;  { consume # }
      if (FPos <= Length(FSource)) and (FSource[FPos] = '$') then
      begin
        Advance;  { hex char code }
        while (FPos <= Length(FSource)) and
              (FSource[FPos] in ['0'..'9', 'A'..'F', 'a'..'f']) do
          Advance;
      end
      else
      begin
        while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9']) do
          Advance;
      end;
    end
    else if c = '^' then
    begin
      Advance;  { consume ^ }
      if (FPos <= Length(FSource)) and (FSource[FPos] in ['A'..'Z', 'a'..'z']) then
        Advance;
    end
    else
      Break;  { not a string continuation }
  until False;

  FToken.Len := FPos - FToken.TextStart;
end;

procedure TFpgPascalTokeniser.ReadBraceCommentOrDirective;
begin
  // Already at open-brace. Check next char for '$'.
  if PeekAt(1) = '$' then
    FToken.Kind := fptkDirective
  else
    FToken.Kind := fptkComment;

  Advance;  // consume open-brace
  while FPos <= Length(FSource) do
  begin
    if FSource[FPos] = '}' then
    begin
      Advance;
      Break;
    end
    else if FSource[FPos] = #13 then
    begin
      Advance;
      if (FPos <= Length(FSource)) and (FSource[FPos] = #10) then
        Advance;
      AdvanceLine;
    end
    else if FSource[FPos] = #10 then
    begin
      Advance;
      AdvanceLine;
    end
    else
      Advance;
  end;
  FToken.Len := FPos - FToken.TextStart;
end;

procedure TFpgPascalTokeniser.ReadParenStarCommentOrDirective;
begin
  { Already at '('. Next is '*'. Check char after '*' for '$'. }
  if PeekAt(2) = '$' then
    FToken.Kind := fptkDirective
  else
    FToken.Kind := fptkComment;

  Advance;  { consume '(' }
  Advance;  { consume '*' }
  while FPos <= Length(FSource) do
  begin
    if (FSource[FPos] = '*') and (PeekAt(1) = ')') then
    begin
      Advance;  { consume '*' }
      Advance;  { consume ')' }
      Break;
    end
    else if FSource[FPos] = #13 then
    begin
      Advance;
      if (FPos <= Length(FSource)) and (FSource[FPos] = #10) then
        Advance;
      AdvanceLine;
    end
    else if FSource[FPos] = #10 then
    begin
      Advance;
      AdvanceLine;
    end
    else
      Advance;
  end;
  FToken.Len := FPos - FToken.TextStart;
end;

procedure TFpgPascalTokeniser.ReadLineComment;
begin
  FToken.Kind := fptkComment;
  { Consume everything until end of line or end of source }
  while (FPos <= Length(FSource)) and
        not (FSource[FPos] in [#10, #13]) do
    Advance;
  FToken.Len := FPos - FToken.TextStart;
end;

procedure TFpgPascalTokeniser.ReadSymbol;
var
  c, c2: Char;
begin
  FToken.Kind := fptkSymbol;
  c := FSource[FPos];
  c2 := PeekAt(1);
  Advance;

  case c of
    ':': if c2 = '=' then Advance;           // :=
    '<': if c2 in ['>', '='] then Advance;   // <> or <=
    '>': if c2 in ['<', '='] then Advance;   // >< or >=
    '.': if c2 = '.' then Advance;           // ..
    '*': if c2 = '*' then Advance;           // **
    '@': if c2 = '@' then Advance;           // @@
    '+': if c2 = '=' then Advance;           // +=
    '-': if c2 = '=' then Advance;           // -=
    '/': if c2 = '=' then Advance;           // /= (// handled separately)
  end;

  // Special: *= (if * was not followed by *)
  if (c = '*') and (c2 <> '*') and (c2 = '=') then
    Advance;

  FToken.Len := FPos - FToken.TextStart;
end;

function TFpgPascalTokeniser.NextToken: TFpgPasToken;
var
  c, c2: Char;
begin
  if FPos > Length(FSource) then
  begin
    FToken.Kind := fptkEOF;
    FToken.Line := FLine;
    FToken.Column := FPos - FLineStart + 1;
    FToken.Len := 0;
    FToken.TextStart := FPos;
    Result := FToken;
    Exit;
  end;

  { Record token start position }
  FToken.TextStart := FPos;
  FToken.Line := FLine;
  FToken.Column := FPos - FLineStart + 1;

  c := FSource[FPos];

  { Whitespace (not line endings) }
  if c in [' ', #9] then
  begin
    ReadWhitespace;
    Result := FToken;
    Exit;
  end;

  { Line endings }
  if c in [#13, #10] then
  begin
    ReadLineEnding;
    Result := FToken;
    Exit;
  end;

  { Identifiers and keywords }
  if c in ['A'..'Z', 'a'..'z', '_'] then
  begin
    ReadIdentifierOrKeyword;
    Result := FToken;
    Exit;
  end;

  { Numbers: digits or $ (hex) or % (binary) or & (octal) }
  if c in ['0'..'9'] then
  begin
    ReadNumber;
    Result := FToken;
    Exit;
  end;
  if (c = '$') and (PeekAt(1) in ['0'..'9', 'A'..'F', 'a'..'f']) then
  begin
    ReadNumber;
    Result := FToken;
    Exit;
  end;
  if (c = '%') and (PeekAt(1) in ['0', '1']) then
  begin
    ReadNumber;
    Result := FToken;
    Exit;
  end;
  if (c = '&') and (PeekAt(1) in ['0'..'7']) then
  begin
    ReadNumber;
    Result := FToken;
    Exit;
  end;

  { Strings: ' or # or ^ }
  if c in ['''', '#'] then
  begin
    ReadString;
    Result := FToken;
    Exit;
  end;
  if (c = '^') and (PeekAt(1) in ['A'..'Z', 'a'..'z']) then
  begin
    ReadString;
    Result := FToken;
    Exit;
  end;

  { Comments and directives }
  if c = '{' then
  begin
    ReadBraceCommentOrDirective;
    Result := FToken;
    Exit;
  end;

  c2 := PeekAt(1);

  if (c = '(') and (c2 = '*') then
  begin
    ReadParenStarCommentOrDirective;
    Result := FToken;
    Exit;
  end;

  if (c = '/') and (c2 = '/') then
  begin
    ReadLineComment;
    Result := FToken;
    Exit;
  end;

  { Symbols and operators }
  ReadSymbol;
  Result := FToken;
end;

function TFpgPascalTokeniser.TokenText: string;
begin
  if (FToken.TextStart >= 1) and (FToken.Len > 0) and
     (FToken.TextStart + FToken.Len - 1 <= Length(FSource)) then
    Result := Copy(FSource, FToken.TextStart, FToken.Len)
  else
    Result := '';
end;

function TFpgPascalTokeniser.TokenTextUpper: string;
begin
  Result := UpCase(TokenText);
end;

end.
