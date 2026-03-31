{
    fpGUI IDE - Syntax Highlighter Engine

    Copyright (C) 2006 - 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Token-based syntax highlighting engine. Provides an abstract base class
      (TEditorHighlighter) for language-agnostic tokenisation, and a concrete
      Pascal implementation (TPascalHighlighter) backed by
      TFpgPascalTokeniser.

      The highlighter produces structured token data indexed by line number.
      Rendering is handled separately by an OnDrawLine callback that reads
      from the highlighter.
}
unit ide.highlighter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { Token categories for syntax highlighting.
    Numbered variants (jEdit-inspired) allow a single theme to serve
    many languages — each language highlighter maps its tokens to the
    appropriate slot.  Unused slots inherit from their base style. }
  THighlightCategory = (
    hcWhitespace,
    hcKeyword1,       // primary keywords (Pascal: begin, end, if, class …)
    hcKeyword2,       // secondary keywords (future: built-in types, etc.)
    hcKeyword3,       // tertiary keywords (future)
    hcIdentifier,
    hcString1,        // primary string literals
    hcString2,        // secondary strings (future: heredoc, template, etc.)
    hcNumber,
    hcComment1,       // primary comments
    hcComment2,       // secondary comments (future: doc-comments, etc.)
    hcDirective,      // compiler directives / preprocessor
    hcSymbol,         // punctuation, brackets
    hcOperator,       // distinguished operators (future)
    hcFunction,       // function / procedure names (future)
    hcLabel,          // labels (future)
    hcMarkup,         // markup / annotations (future)
    hcInvalid         // error / invalid tokens (future)
  );

  { A single highlighted token on a line }
  THighlightToken = record
    Column: Integer;              // 0-based column position within the line
    Length: Integer;              // character length of the token
    Category: THighlightCategory;
  end;

  THighlightTokenArray = array of THighlightToken;

  PHighlightLine = ^THighlightLine;
  THighlightLine = record
    Tokens: THighlightTokenArray;
    Count: Integer;
  end;


  { TEditorHighlighter - abstract base class for syntax highlighting }

  TEditorHighlighter = class(TObject)
  private
    FLines: TFPList;     // list of PHighlightLine, indexed by line number
    function GetLine(AIndex: Integer): PHighlightLine;
  protected
    procedure ClearLines;
    procedure AddToken(ALine, AColumn, ALength: Integer; ACategory: THighlightCategory);
    procedure EnsureLineCount(ACount: Integer);
    procedure DoTokenise(const AText: string); virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Tokenise(const AText: string);
    function LineCount: Integer;
    function GetLineTokens(ALine: Integer): THighlightTokenArray;
    function GetLineTokenCount(ALine: Integer): Integer;
  end;


  { TPascalHighlighter - Object Pascal syntax highlighting }

  TPascalHighlighter = class(TEditorHighlighter)
  protected
    procedure DoTokenise(const AText: string); override;
  end;


implementation

uses
  ide.pascal.tokeniser;


{ TEditorHighlighter }

constructor TEditorHighlighter.Create;
begin
  inherited Create;
  FLines := TFPList.Create;
end;

destructor TEditorHighlighter.Destroy;
begin
  ClearLines;
  FLines.Free;
  inherited Destroy;
end;

procedure TEditorHighlighter.ClearLines;
var
  i: Integer;
  p: PHighlightLine;
begin
  for i := 0 to FLines.Count - 1 do
  begin
    p := PHighlightLine(FLines[i]);
    p^.Tokens := nil;
    Dispose(p);
  end;
  FLines.Clear;
end;

function TEditorHighlighter.GetLine(AIndex: Integer): PHighlightLine;
begin
  if (AIndex >= 0) and (AIndex < FLines.Count) then
    Result := PHighlightLine(FLines[AIndex])
  else
    Result := nil;
end;

procedure TEditorHighlighter.EnsureLineCount(ACount: Integer);
var
  p: PHighlightLine;
begin
  while FLines.Count < ACount do
  begin
    New(p);
    p^.Tokens := nil;
    p^.Count := 0;
    FLines.Add(p);
  end;
end;

procedure TEditorHighlighter.AddToken(ALine, AColumn, ALength: Integer;
  ACategory: THighlightCategory);
var
  p: PHighlightLine;
begin
  if ALength <= 0 then
    Exit;
  EnsureLineCount(ALine + 1);
  p := PHighlightLine(FLines[ALine]);
  if p^.Count >= Length(p^.Tokens) then
  begin
    if Length(p^.Tokens) = 0 then
      SetLength(p^.Tokens, 16)
    else
      SetLength(p^.Tokens, Length(p^.Tokens) * 2);
  end;
  p^.Tokens[p^.Count].Column := AColumn;
  p^.Tokens[p^.Count].Length := ALength;
  p^.Tokens[p^.Count].Category := ACategory;
  Inc(p^.Count);
end;

procedure TEditorHighlighter.Tokenise(const AText: string);
begin
  ClearLines;
  if AText <> '' then
    DoTokenise(AText);
end;

function TEditorHighlighter.LineCount: Integer;
begin
  Result := FLines.Count;
end;

function TEditorHighlighter.GetLineTokens(ALine: Integer): THighlightTokenArray;
var
  p: PHighlightLine;
  i, j: Integer;
  tmp: THighlightToken;
begin
  p := GetLine(ALine);
  if (p <> nil) and (p^.Count > 0) then
  begin
    SetLength(Result, p^.Count);
    Move(p^.Tokens[0], Result[0], p^.Count * SizeOf(THighlightToken));
    { Sort by column — multi-line token splitting may append out of order }
    for i := 1 to Length(Result) - 1 do
    begin
      tmp := Result[i];
      j := i - 1;
      while (j >= 0) and (Result[j].Column > tmp.Column) do
      begin
        Result[j + 1] := Result[j];
        Dec(j);
      end;
      Result[j + 1] := tmp;
    end;
  end
  else
    Result := nil;
end;

function TEditorHighlighter.GetLineTokenCount(ALine: Integer): Integer;
var
  p: PHighlightLine;
begin
  p := GetLine(ALine);
  if p <> nil then
    Result := p^.Count
  else
    Result := 0;
end;


{ TPascalHighlighter }

procedure TPascalHighlighter.DoTokenise(const AText: string);
var
  Tokeniser: TFpgPascalTokeniser;
  Tok: TFpgPasToken;
  Line, Col: Integer;
  Category: THighlightCategory;
  TokText: string;
  i, SegStart, SegLen: Integer;
  SrcLineCount: Integer;
begin
  // Count source lines and pre-allocate
  SrcLineCount := 1;
  for i := 1 to Length(AText) do
  begin
    if AText[i] = #10 then
      Inc(SrcLineCount)
    else if (AText[i] = #13) then
    begin
      Inc(SrcLineCount);
      if (i < Length(AText)) and (AText[i + 1] = #10) then
        ; // CRLF counted as one line ending
    end;
  end;
  EnsureLineCount(SrcLineCount);

  Tokeniser := TFpgPascalTokeniser.Create;
  try
    Tokeniser.SetSource(AText);
    repeat
      Tok := Tokeniser.NextToken;
      if Tok.Kind = fptkEOF then
        Break;
      if Tok.Kind in [fptkWhitespace, fptkLineEnding] then
        Continue;

      // Convert 1-based line/col to 0-based
      Line := Tok.Line - 1;
      Col := Tok.Column - 1;

      // Map token kind to highlight category
      case Tok.Kind of
        fptkKeyword:    Category := hcKeyword1;
        fptkIdentifier: Category := hcIdentifier;
        fptkString:     Category := hcString1;
        fptkNumber:     Category := hcNumber;
        fptkComment:    Category := hcComment1;
        fptkDirective:  Category := hcDirective;
      else
        Category := hcSymbol;
      end;

      TokText := Tokeniser.TokenText;

      // Multi-line tokens (comments, directives) must be split across lines
      if (Tok.Kind in [fptkComment, fptkDirective]) and
         ((Pos(#10, TokText) > 0) or (Pos(#13, TokText) > 0)) then
      begin
        SegStart := 1;
        i := 1;
        while i <= Length(TokText) do
        begin
          if (TokText[i] = #13) or (TokText[i] = #10) then
          begin
            SegLen := i - SegStart;
            if SegLen > 0 then
              AddToken(Line, Col, SegLen, Category);
            // Skip line ending
            if (TokText[i] = #13) and (i < Length(TokText)) and
               (TokText[i + 1] = #10) then
              Inc(i);
            Inc(i);
            Inc(Line);
            Col := 0;
            SegStart := i;
          end
          else
            Inc(i);
        end;
        // Final segment
        SegLen := Length(TokText) - SegStart + 1;
        if SegLen > 0 then
          AddToken(Line, Col, SegLen, Category);
      end
      else
        AddToken(Line, Col, Tok.Len, Category);
    until False;
  finally
    Tokeniser.Free;
  end;
end;

end.
