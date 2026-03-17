{
    fpGUI IDE - Bracket Matching Engine

    Copyright (C) 2006 - 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Pure logic unit for finding matching brackets and begin/end pairs
      using the token stream from TEditorHighlighter. No GUI dependencies.
}
unit ide.bracketmatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ide.highlighter;

type
  TBracketKind = (
    bkNone,
    bkRoundOpen,    // (
    bkRoundClose,   // )
    bkSquareOpen,   // [
    bkSquareClose,  // ]
    bkCurlyOpen,    // {
    bkCurlyClose,   // }
    bkBegin,        // begin, case, try, record, class, object
    bkEnd           // end
  );

  TBracketMatchResult = record
    Found: Boolean;
    SourceLine: Integer;    // 0-based line of the bracket at caret
    SourceCol: Integer;     // 0-based column
    SourceLength: Integer;  // character length (1 for symbols, 3/5/etc for keywords)
    MatchLine: Integer;     // 0-based line of the matching bracket
    MatchCol: Integer;      // 0-based column
    MatchLength: Integer;   // character length of the match
  end;

{ Find the matching bracket for the token at the given caret position.
  ACaretLine and ACaretCol are 0-based. ASourceLines provides the actual
  text for looking up token content (since THighlightToken only stores
  column and length, not text). }
function FindMatchingBracket(AHighlighter: TEditorHighlighter;
  ACaretLine, ACaretCol: Integer;
  ASourceLines: TStrings): TBracketMatchResult;

{ Classify a token as a bracket kind by examining the source text. }
function ClassifyBracket(const AToken: THighlightToken;
  ASourceLines: TStrings; ALine: Integer): TBracketKind;

{ Return the closing counterpart for an opening bracket kind, or vice versa. }
function OppositeKind(AKind: TBracketKind): TBracketKind;

{ Return True if the bracket kind is an opening bracket. }
function IsOpeningBracket(AKind: TBracketKind): Boolean;

implementation

const
  { Maximum number of lines to search before giving up }
  cMaxSearchLines = 5000;

function GetTokenText(const AToken: THighlightToken;
  ASourceLines: TStrings; ALine: Integer): string;
begin
  Result := '';
  if (ALine < 0) or (ALine >= ASourceLines.Count) then
    Exit;
  Result := Copy(ASourceLines[ALine], AToken.Column + 1, AToken.Length);
end;

function ClassifyBracket(const AToken: THighlightToken;
  ASourceLines: TStrings; ALine: Integer): TBracketKind;
var
  ch: Char;
  s: string;
begin
  Result := bkNone;

  if AToken.Category = hcSymbol then
  begin
    if AToken.Length <> 1 then
      Exit;
    s := GetTokenText(AToken, ASourceLines, ALine);
    if Length(s) <> 1 then
      Exit;
    ch := s[1];
    case ch of
      '(': Result := bkRoundOpen;
      ')': Result := bkRoundClose;
      '[': Result := bkSquareOpen;
      ']': Result := bkSquareClose;
      '{': Result := bkCurlyOpen;
      '}': Result := bkCurlyClose;
    end;
  end
  else if AToken.Category = hcKeyword1 then
  begin
    s := LowerCase(GetTokenText(AToken, ASourceLines, ALine));
    if (s = 'begin') or (s = 'case') or (s = 'try') or
       (s = 'record') or (s = 'class') or (s = 'object') then
      Result := bkBegin
    else if s = 'end' then
      Result := bkEnd;
  end;
end;

function OppositeKind(AKind: TBracketKind): TBracketKind;
begin
  case AKind of
    bkRoundOpen:   Result := bkRoundClose;
    bkRoundClose:  Result := bkRoundOpen;
    bkSquareOpen:  Result := bkSquareClose;
    bkSquareClose: Result := bkSquareOpen;
    bkCurlyOpen:   Result := bkCurlyClose;
    bkCurlyClose:  Result := bkCurlyOpen;
    bkBegin:       Result := bkEnd;
    bkEnd:         Result := bkBegin;
  else
    Result := bkNone;
  end;
end;

function IsOpeningBracket(AKind: TBracketKind): Boolean;
begin
  Result := AKind in [bkRoundOpen, bkSquareOpen, bkCurlyOpen, bkBegin];
end;

function IsStructuralToken(ACat: THighlightCategory): Boolean;
begin
  { Tokens inside comments, strings, and directives are not structural }
  Result := not (ACat in [hcComment1, hcComment2, hcString1, hcString2, hcDirective]);
end;

{ Returns True if two bracket kinds form a matching pair (same bracket family). }
function IsSameFamily(A, B: TBracketKind): Boolean;
begin
  Result := (A <> bkNone) and (OppositeKind(A) = B);
end;

function FindTokenAtCaret(AHighlighter: TEditorHighlighter;
  ALine, ACol: Integer; out ATokenIndex: Integer): Boolean;
var
  tokens: THighlightTokenArray;
  i: Integer;
begin
  Result := False;
  ATokenIndex := -1;
  tokens := AHighlighter.GetLineTokens(ALine);
  if tokens = nil then
    Exit;
  for i := 0 to Length(tokens) - 1 do
  begin
    if (ACol >= tokens[i].Column) and (ACol < tokens[i].Column + tokens[i].Length) then
    begin
      ATokenIndex := i;
      Result := True;
      Exit;
    end;
  end;
end;

function FindMatchingBracket(AHighlighter: TEditorHighlighter;
  ACaretLine, ACaretCol: Integer;
  ASourceLines: TStrings): TBracketMatchResult;
var
  tokens: THighlightTokenArray;
  tokIdx: Integer;
  srcKind, curKind, targetKind: TBracketKind;
  depth: Integer;
  line, i: Integer;
  linesSearched: Integer;
  searchForward: Boolean;
begin
  Result.Found := False;
  Result.SourceLine := 0;
  Result.SourceCol := 0;
  Result.SourceLength := 0;
  Result.MatchLine := 0;
  Result.MatchCol := 0;
  Result.MatchLength := 0;

  if AHighlighter = nil then
    Exit;
  if ASourceLines = nil then
    Exit;
  if (ACaretLine < 0) or (ACaretLine >= AHighlighter.LineCount) then
    Exit;

  { Find the token at the caret position }
  if not FindTokenAtCaret(AHighlighter, ACaretLine, ACaretCol, tokIdx) then
    Exit;

  tokens := AHighlighter.GetLineTokens(ACaretLine);
  srcKind := ClassifyBracket(tokens[tokIdx], ASourceLines, ACaretLine);
  if srcKind = bkNone then
    Exit;

  targetKind := OppositeKind(srcKind);
  searchForward := IsOpeningBracket(srcKind);

  Result.SourceLine := ACaretLine;
  Result.SourceCol := tokens[tokIdx].Column;
  Result.SourceLength := tokens[tokIdx].Length;

  { Search for the matching bracket }
  depth := 1;
  linesSearched := 0;

  if searchForward then
  begin
    { Start from the token after the source on the same line }
    line := ACaretLine;
    i := tokIdx + 1;
    while (line < AHighlighter.LineCount) and (linesSearched < cMaxSearchLines) do
    begin
      tokens := AHighlighter.GetLineTokens(line);
      if tokens <> nil then
      begin
        while i < Length(tokens) do
        begin
          if IsStructuralToken(tokens[i].Category) then
          begin
            curKind := ClassifyBracket(tokens[i], ASourceLines, line);
            if curKind = srcKind then
              Inc(depth)
            else if curKind = targetKind then
            begin
              Dec(depth);
              if depth = 0 then
              begin
                Result.Found := True;
                Result.MatchLine := line;
                Result.MatchCol := tokens[i].Column;
                Result.MatchLength := tokens[i].Length;
                Exit;
              end;
            end;
          end;
          Inc(i);
        end;
      end;
      Inc(line);
      Inc(linesSearched);
      i := 0;
    end;
  end
  else
  begin
    { Search backward: start from the token before the source on the same line }
    line := ACaretLine;
    i := tokIdx - 1;
    while (line >= 0) and (linesSearched < cMaxSearchLines) do
    begin
      tokens := AHighlighter.GetLineTokens(line);
      if tokens <> nil then
      begin
        while i >= 0 do
        begin
          if IsStructuralToken(tokens[i].Category) then
          begin
            curKind := ClassifyBracket(tokens[i], ASourceLines, line);
            if curKind = srcKind then
              Inc(depth)
            else if curKind = targetKind then
            begin
              Dec(depth);
              if depth = 0 then
              begin
                Result.Found := True;
                Result.MatchLine := line;
                Result.MatchCol := tokens[i].Column;
                Result.MatchLength := tokens[i].Length;
                Exit;
              end;
            end;
          end;
          Dec(i);
        end;
      end;
      Dec(line);
      Inc(linesSearched);
      if (line >= 0) then
      begin
        tokens := AHighlighter.GetLineTokens(line);
        if tokens <> nil then
          i := Length(tokens) - 1
        else
          i := -1;
      end;
    end;
  end;
end;

end.
