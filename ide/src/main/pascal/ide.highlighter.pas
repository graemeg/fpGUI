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
      Pascal implementation (TPascalHighlighter) backed by FPC's fcl-passrc
      TPascalScanner.

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


  { TPascalHighlighter - Object Pascal syntax highlighting via fcl-passrc }

  TPascalHighlighter = class(TEditorHighlighter)
  private
    procedure FillMissingDirectives(ASourceLines: TStrings);
    procedure FillUntokenisedGaps(ASourceLines: TStrings);
    function HasTokenAt(ALine, ACol: Integer): Boolean;
  protected
    procedure DoTokenise(const AText: string); override;
  end;


implementation

uses
  PScanner, ide.pascal.tokeniser;

type

  (* THighlightResolver - a stream resolver that returns empty content for
    include files instead of raising errors. This allows the scanner to
    continue past {$I ...} directives when highlighting editor text. *)

  THighlightResolver = class(TStreamResolver)
  public
    function FindIncludeFile(const AName: string): TLineReader; override;
  end;

function THighlightResolver.FindIncludeFile(const AName: string): TLineReader;
begin
  { Return a reader for empty content so the scanner doesn't error }
  Result := TStringStreamLineReader.Create(AName, '');
end;


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
    { Sort by column — post-processing steps (FillMissingDirectives,
      FillUntokenisedGaps) append tokens out of order }
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

{ Compute visual length for operator/symbol tokens.
  The scanner does not set CurTokenString for these, so we derive
  the length from the TToken type. }
function SymbolTokenLength(AToken: TToken): Integer;
begin
  case AToken of
    tkBraceOpen, tkBraceClose, tkMul, tkPlus, tkComma, tkMinus,
    tkDot, tkDivision, tkColon, tkSemicolon, tkLessThan, tkEqual,
    tkGreaterThan, tkAt, tkSquaredBraceOpen, tkSquaredBraceClose,
    tkCaret, tkBackslash:
      Result := 1;
    tkDotDot, tkAssign, tkNotEqual, tkLessEqualThan, tkGreaterEqualThan,
    tkPower, tkSymmetricalDifference, tkAssignPlus, tkAssignMinus,
    tkAssignMul, tkAssignDivision, tkAtAt:
      Result := 2;
    {$IFDEF FPC_IS_MAIN}
    tkDotDotDot:
      Result := 3;
    {$ENDIF}
    else
      Result := 1;
  end;
end;

function TPascalHighlighter.HasTokenAt(ALine, ACol: Integer): Boolean;
var
  p: PHighlightLine;
  i: Integer;
begin
  Result := False;
  p := GetLine(ALine);
  if p = nil then
    Exit;
  for i := 0 to p^.Count - 1 do
    if (p^.Tokens[i].Column = ACol) then
      Exit(True);
end;

procedure TPascalHighlighter.FillMissingDirectives(ASourceLines: TStrings);
var
  LineIdx, Col, DirEnd: Integer;
  S: string;
begin
  (* Scan source lines for {$...} and ( *$...* ) directive patterns
    that the scanner skipped (e.g. false IFDEF branches).
    Add hcDirective tokens for any not already covered. *)
  for LineIdx := 0 to ASourceLines.Count - 1 do
  begin
    S := ASourceLines[LineIdx];
    Col := 1;
    while Col <= Length(S) do
    begin
      if (S[Col] = '{') and (Col < Length(S)) and (S[Col + 1] = '$') then
      begin
        { Find closing brace }
        DirEnd := Col + 1;
        while (DirEnd <= Length(S)) and (S[DirEnd] <> '}') do
          Inc(DirEnd);
        if DirEnd <= Length(S) then
        begin
          if not HasTokenAt(LineIdx, Col - 1) then
            AddToken(LineIdx, Col - 1, DirEnd - Col + 1, hcDirective);
          Col := DirEnd + 1;
        end
        else
          Inc(Col);
      end
      else if (S[Col] = '(') and (Col + 1 < Length(S)) and
              (S[Col + 1] = '*') and (S[Col + 2] = '$') then
      begin
        { Find closing *) }
        DirEnd := Col + 2;
        while (DirEnd < Length(S)) and
              not ((S[DirEnd] = '*') and (S[DirEnd + 1] = ')')) do
          Inc(DirEnd);
        if DirEnd < Length(S) then
        begin
          if not HasTokenAt(LineIdx, Col - 1) then
            AddToken(LineIdx, Col - 1, DirEnd - Col + 2, hcDirective);
          Col := DirEnd + 2;
        end
        else
          Inc(Col);
      end
      else
        Inc(Col);
    end;
  end;
end;

procedure TPascalHighlighter.FillUntokenisedGaps(ASourceLines: TStrings);
var
  LineIdx, Col, RunStart, TokenEnd: Integer;
  S: string;
  p: PHighlightLine;
  i: Integer;
  Covered: Boolean;
begin
  { After FillMissingDirectives, some lines may still have untokenised
    non-whitespace text — typically code inside false IFDEF branches
    that the scanner skipped entirely. Add hcIdentifier tokens for
    these regions so the renderer draws them as plain text. }
  for LineIdx := 0 to ASourceLines.Count - 1 do
  begin
    p := GetLine(LineIdx);
    if p = nil then
      Continue;
    S := ASourceLines[LineIdx];
    Col := 0;  { 0-based }
    while Col < Length(S) do
    begin
      { Skip whitespace }
      if S[Col + 1] <= ' ' then
      begin
        Inc(Col);
        Continue;
      end;
      { Check if this column is covered by an existing token }
      Covered := False;
      for i := 0 to p^.Count - 1 do
      begin
        if (Col >= p^.Tokens[i].Column) and
           (Col < p^.Tokens[i].Column + p^.Tokens[i].Length) then
        begin
          Covered := True;
          { Skip past this token }
          Col := p^.Tokens[i].Column + p^.Tokens[i].Length;
          Break;
        end;
      end;
      if Covered then
        Continue;
      { Found untokenised non-whitespace — find the extent of the run }
      RunStart := Col;
      Inc(Col);
      while (Col < Length(S)) and (S[Col + 1] > ' ') do
      begin
        { Stop if we hit an existing token }
        Covered := False;
        for i := 0 to p^.Count - 1 do
        begin
          if Col = p^.Tokens[i].Column then
          begin
            Covered := True;
            Break;
          end;
        end;
        if Covered then
          Break;
        Inc(Col);
      end;
      AddToken(LineIdx, RunStart, Col - RunStart, hcIdentifier);
    end;
  end;
end;

procedure TPascalHighlighter.DoTokenise(const AText: string);
var
  SourceLines: TStringList;
  Resolver: THighlightResolver;
  Scanner: TPascalScanner;
  Token: TToken;
  Line: Integer;
  Col: Integer;
  TokenStr: string;
  TokenLen: Integer;
  Category: THighlightCategory;
  SrcLine: string;
  OpenDelimLen: Integer;
  CloseDelimLen: Integer;
  i, SegStart, SegLen: Integer;
begin
  { Split source into lines for reference during tokenisation }
  SourceLines := TStringList.Create;
  try
    SourceLines.Text := AText;
    EnsureLineCount(SourceLines.Count);

    Resolver := THighlightResolver.Create;
    try
      Resolver.OwnsStreams := True;
      Resolver.AddStream('source.pas', TStringStream.Create(AText));

      Scanner := TPascalScanner.Create(Resolver);
      try
        Scanner.SkipWhiteSpace := True;
        Scanner.SkipComments := False;
        Scanner.OpenFile('source.pas');

        try
          repeat
            Token := Scanner.FetchToken;
            if Token = tkEOF then
              Break;
            if Token in [tkLineEnding, tkWhitespace, tkTab] then
              Continue;

            { CurTokenPos records the token's START position.
              CurRow/CurColumn point past the token after FetchToken returns.
              Both Row and Column are 1-based. }
            Line := Scanner.CurTokenPos.Row - 1;
            Col := Scanner.CurTokenPos.Column - 1;
            TokenStr := Scanner.CurTokenString;

            { -------------------------------------------------------
              Map TToken to THighlightCategory
              -------------------------------------------------------
              CurTokenString content varies by token type:
                // comment    -> content AFTER // (excludes //)
                lbrace comment rbrace   -> content between braces (excludes delimiters)
                paren-star comment star-paren -> content between delimiters
                'string'      -> full raw text including quotes
                identifiers   -> the identifier text
                numbers       -> full text including $ % & prefix
                operators     -> EMPTY (CurTokenString not set)
              ------------------------------------------------------- }
            case Token of
              tkComment:
              begin
                if (Length(TokenStr) > 0) and (TokenStr[1] = '$') then
                  Category := hcDirective
                else
                  Category := hcComment1;
              end;

              tkIdentifier:
                Category := hcIdentifier;

              tkString{$IFDEF FPC_IS_MAIN}, tkStringMultiLine{$ENDIF}:
                Category := hcString1;

              tkChar:
                { tkChar covers ^A..^Z syntax. The scanner can't distinguish
                  character literals from pointer types (^byte, ^TPoint),
                  so leave these unhighlighted to avoid false colouring. }
                Category := hcSymbol;

              tkNumber:
                Category := hcNumber;

              { All reserved words map to hcKeyword1 for now.
                A future enhancement could split these into keyword2/3
                for finer-grained colouring (e.g. types vs flow-control). }
              tkabsolute, tkand, tkarray, tkas, tkasm, tkbegin, tkbitpacked,
              tkcase, tkclass, tkconst, tkconstref, tkconstructor, 
              {$IFDEF FPC_IS_MAIN}tkcontains, tkPackage, tkrequires,{$ENDIF}
              tkdestructor, tkdispinterface, tkdiv, tkdo, tkdownto, tkelse,
              tkend, tkexcept, tkexports, tkfalse, tkfile, tkfinalization,
              tkfinally, tkfor, tkfunction, tkgeneric, tkgoto, tkif,
              tkimplementation, tkin, tkinherited, tkinitialization, tkinline,
              tkinterface, tkis, tklabel, tklibrary, tkmod, tknil, tknot,
              tkobjccategory, tkobjcclass, tkobjcprotocol, tkobject, tkof,
              tkoperator, tkor, tkotherwise, tkpacked, tkprocedure,
              tkprogram, tkproperty, tkraise, tkrecord, tkrepeat,
              tkResourceString, tkself, tkset, tkshl, tkshr, tkspecialize,
              tkthen, tkthreadvar, tkto, tktrue, tktry, tktype, tkunit,
              tkuntil, tkuses, tkvar, tkwhile, tkwith, tkxor:
                Category := hcKeyword1;

              else
                Category := hcSymbol;
            end;

            { -------------------------------------------------------
              Compute visual token length and handle multi-line tokens
              ------------------------------------------------------- }
            case Token of

              tkComment:
              begin
                { Determine comment style by looking at the source text
                  at the token's start position }
                OpenDelimLen := 0;
                CloseDelimLen := 0;
                if (Line >= 0) and (Line < SourceLines.Count) then
                begin
                  SrcLine := SourceLines[Line];
                  if (Col + 1 <= Length(SrcLine)) and (Col + 2 <= Length(SrcLine)) then
                  begin
                    if (SrcLine[Col + 1] = '/') and (SrcLine[Col + 2] = '/') then
                    begin
                      { // style: CurTokenString = content after // }
                      OpenDelimLen := 2;
                      CloseDelimLen := 0;
                    end
                    else if SrcLine[Col + 1] = '{' then
                    begin
                      { lbrace rbrace style }
                      OpenDelimLen := 1;
                      CloseDelimLen := 1;
                    end
                    else if (SrcLine[Col + 1] = '(') then
                    begin
                      { paren-star style }
                      OpenDelimLen := 2;
                      CloseDelimLen := 2;
                    end;
                  end
                  else if (Col + 1 <= Length(SrcLine)) and (SrcLine[Col + 1] = '{') then
                  begin
                    OpenDelimLen := 1;
                    CloseDelimLen := 1;
                  end;
                end;

                { Check for multi-line comment }
                if (Pos(#10, TokenStr) > 0) or (Pos(#13, TokenStr) > 0) then
                begin
                  { Split across lines }
                  SegStart := 1;
                  i := 1;
                  while i <= Length(TokenStr) do
                  begin
                    if (TokenStr[i] = #13) or (TokenStr[i] = #10) then
                    begin
                      SegLen := i - SegStart;
                      if SegStart = 1 then
                        { First line: includes opening delimiter }
                        AddToken(Line, Col, SegLen + OpenDelimLen, Category)
                      else if SegLen > 0 then
                        AddToken(Line, 0, SegLen, Category);
                      { Skip line ending }
                      if (TokenStr[i] = #13) and (i < Length(TokenStr)) and
                         (TokenStr[i + 1] = #10) then
                        Inc(i);
                      Inc(i);
                      Inc(Line);
                      SegStart := i;
                    end
                    else
                      Inc(i);
                  end;
                  { Final line: includes closing delimiter }
                  SegLen := Length(TokenStr) - SegStart + 1;
                  AddToken(Line, 0, SegLen + CloseDelimLen, Category);
                end
                else
                begin
                  { Single-line comment }
                  TokenLen := Length(TokenStr) + OpenDelimLen + CloseDelimLen;
                  AddToken(Line, Col, TokenLen, Category);
                end;
              end;

              {$IFDEF FPC_IS_MAIN}
              tkStringMultiLine:
              begin
                { Multi-line strings: CurTokenString includes delimiters.
                  Split across lines. }
                if (Pos(#10, TokenStr) > 0) or (Pos(#13, TokenStr) > 0) then
                begin
                  SegStart := 1;
                  i := 1;
                  while i <= Length(TokenStr) do
                  begin
                    if (TokenStr[i] = #13) or (TokenStr[i] = #10) then
                    begin
                      SegLen := i - SegStart;
                      if SegStart = 1 then
                        AddToken(Line, Col, SegLen, Category)
                      else if SegLen > 0 then
                        AddToken(Line, 0, SegLen, Category);
                      if (TokenStr[i] = #13) and (i < Length(TokenStr)) and
                         (TokenStr[i + 1] = #10) then
                        Inc(i);
                      Inc(i);
                      Inc(Line);
                      SegStart := i;
                    end
                    else
                      Inc(i);
                  end;
                  SegLen := Length(TokenStr) - SegStart + 1;
                  if SegLen > 0 then
                    AddToken(Line, 0, SegLen, Category);
                end
                else
                  AddToken(Line, Col, Length(TokenStr), Category);
              end;
              {$ENDIF}

              { Operators: CurTokenString is empty, length from token type }
              tkBraceOpen, tkBraceClose, tkMul, tkPlus, tkComma, tkMinus,
              tkDot, tkDivision, tkColon, tkSemicolon, tkLessThan, tkEqual,
              tkGreaterThan, tkAt, tkSquaredBraceOpen, tkSquaredBraceClose,
              tkCaret, tkBackslash,
              tkDotDot, tkAssign, tkNotEqual, tkLessEqualThan,
              tkGreaterEqualThan, tkPower, tkSymmetricalDifference,
              tkAssignPlus, tkAssignMinus, tkAssignMul, tkAssignDivision,
              tkAtAt
              {$IFDEF FPC_IS_MAIN}, tkDotDotDot{$ENDIF}:
                AddToken(Line, Col, SymbolTokenLength(Token), Category);

              else
                { Identifiers, keywords, strings, numbers, chars:
                  CurTokenString contains the full visual text }
                AddToken(Line, Col, Length(TokenStr), Category);
            end;

          until False;
        except
          on E: EScannerError do
            { Gracefully handle scanner errors on incomplete/invalid source.
              Tokens collected so far provide partial highlighting. }
            ;
        end;

      finally
        Scanner.Free;
      end;
    finally
      Resolver.Free;
    end;

    { Post-process: fill in directives the scanner skipped
      (e.g. false IFDEF/IFNDEF branches) }
    FillMissingDirectives(SourceLines);
    FillUntokenisedGaps(SourceLines);
  finally
    SourceLines.Free;
  end;
end;

end.
