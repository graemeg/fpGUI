{
    fpGUI IDE - Interface/Implementation Navigation

    Copyright (C) 2006 - 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Provides navigation between the interface and implementation sections
      of an Object Pascal unit. Uses the existing tokenised highlight data
      to reliably identify keywords (skipping those inside comments or
      strings) and method signatures.
}
unit ide.navigation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ide.highlighter;

type
  TUnitSection = (usUnknown, usInterface, usImplementation);

  TNavigationResult = record
    Found: Boolean;
    Line: Integer;     // 0-based line number
  end;

{ Determine which section the given 0-based line is in, using the
  tokenised highlight data. }
function GetSectionAtLine(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ALine: Integer): TUnitSection;

{ Toggle: navigate from interface to implementation or vice versa.
  ACursorLine is 0-based. }
function NavigateInterfaceImplementation(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ACursorLine: Integer): TNavigationResult;

{ Jump to the interface declaration (from implementation). }
function NavigateToInterface(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ACursorLine: Integer): TNavigationResult;

{ Jump to the implementation (from interface). }
function NavigateToImplementation(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ACursorLine: Integer): TNavigationResult;


implementation

{ Helper: extract text for a token from the source line }
function TokenText(ALines: TStrings; ALineIdx: Integer;
  const AToken: THighlightToken): string;
begin
  if (ALineIdx >= 0) and (ALineIdx < ALines.Count) then
    Result := Copy(ALines[ALineIdx], AToken.Column + 1, AToken.Length)
  else
    Result := '';
end;

{ Helper: check if a keyword token on a given line matches a specific word }
function IsKeywordToken(AHighlighter: TEditorHighlighter; ALines: TStrings;
  ALineIdx: Integer; const ATokenIdx: Integer; const AKeyword: string): Boolean;
var
  tokens: THighlightTokenArray;
begin
  Result := False;
  tokens := AHighlighter.GetLineTokens(ALineIdx);
  if (ATokenIdx < 0) or (ATokenIdx >= Length(tokens)) then
    Exit;
  if tokens[ATokenIdx].Category <> hcKeyword1 then
    Exit;
  Result := CompareText(
    Copy(ALines[ALineIdx], tokens[ATokenIdx].Column + 1, tokens[ATokenIdx].Length),
    AKeyword) = 0;
end;

{ Find the first hcKeyword1 token on a line and return its text (uppercased).
  Returns empty string if no keyword found. }
function FirstKeywordOnLine(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ALineIdx: Integer): string;
var
  tokens: THighlightTokenArray;
  i: Integer;
begin
  Result := '';
  tokens := AHighlighter.GetLineTokens(ALineIdx);
  for i := 0 to Length(tokens) - 1 do
  begin
    if tokens[i].Category = hcKeyword1 then
    begin
      Result := UpperCase(TokenText(ALines, ALineIdx, tokens[i]));
      Exit;
    end;
  end;
end;

{ Check if a line starts with a method keyword (procedure, function,
  constructor, destructor) by examining the first keyword token.
  Also handles 'class function' and 'class procedure'. }
function IsMethodLine(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ALineIdx: Integer): Boolean;
var
  tokens: THighlightTokenArray;
  i: Integer;
  kw: string;
begin
  Result := False;
  tokens := AHighlighter.GetLineTokens(ALineIdx);
  { Find first keyword token }
  for i := 0 to Length(tokens) - 1 do
  begin
    if tokens[i].Category = hcKeyword1 then
    begin
      kw := UpperCase(TokenText(ALines, ALineIdx, tokens[i]));
      if (kw = 'PROCEDURE') or (kw = 'FUNCTION') or
         (kw = 'CONSTRUCTOR') or (kw = 'DESTRUCTOR') then
        Exit(True);
      if kw = 'CLASS' then
      begin
        { Check for 'class procedure' or 'class function' }
        if (i + 1 < Length(tokens)) and (tokens[i + 1].Category = hcKeyword1) then
        begin
          kw := UpperCase(TokenText(ALines, ALineIdx, tokens[i + 1]));
          if (kw = 'PROCEDURE') or (kw = 'FUNCTION') then
            Exit(True);
        end;
      end;
      { First keyword was something else — not a method line }
      Exit(False);
    end;
  end;
end;

{ Extract the method name from a method declaration line by reading
  the identifier token(s) that follow the method keyword.
  Returns the full name lowercased, e.g. 'tmainform.formshow'.
  Handles dotted names (ClassName.MethodName) by concatenating
  identifier and symbol tokens. }
function ExtractMethodNameFromLine(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ALineIdx: Integer): string;
var
  tokens: THighlightTokenArray;
  i: Integer;
  kw: string;
  foundKeyword: Boolean;
begin
  Result := '';
  tokens := AHighlighter.GetLineTokens(ALineIdx);
  foundKeyword := False;

  for i := 0 to Length(tokens) - 1 do
  begin
    if not foundKeyword then
    begin
      if tokens[i].Category = hcKeyword1 then
      begin
        kw := UpperCase(TokenText(ALines, ALineIdx, tokens[i]));
        if (kw = 'PROCEDURE') or (kw = 'FUNCTION') or
           (kw = 'CONSTRUCTOR') or (kw = 'DESTRUCTOR') then
          foundKeyword := True
        else if kw = 'CLASS' then
          Continue  { skip 'class' prefix, next keyword should be procedure/function }
        else
          Exit;  { not a method line }
      end;
    end
    else
    begin
      { After the method keyword, collect identifier.identifier tokens }
      if tokens[i].Category = hcIdentifier then
        Result := Result + TokenText(ALines, ALineIdx, tokens[i])
      else if (tokens[i].Category = hcSymbol) and
              (TokenText(ALines, ALineIdx, tokens[i]) = '.') then
        Result := Result + '.'
      else
        Break;  { hit something else (parenthesis, colon, semicolon) }
    end;
  end;
  Result := LowerCase(Result);
end;

{ Find the 0-based line numbers of the unit-level 'interface' and
  'implementation' keywords using the token stream. }
function FindUnitSections(AHighlighter: TEditorHighlighter;
  ALines: TStrings;
  out AInterfaceLine, AImplementationLine: Integer): Boolean;
var
  i: Integer;
  kw: string;
begin
  Result := False;
  AInterfaceLine := -1;
  AImplementationLine := -1;

  for i := 0 to AHighlighter.LineCount - 1 do
  begin
    kw := FirstKeywordOnLine(AHighlighter, ALines, i);
    if (AInterfaceLine < 0) and (kw = 'INTERFACE') then
      AInterfaceLine := i
    else if (AImplementationLine < 0) and (kw = 'IMPLEMENTATION') then
    begin
      AImplementationLine := i;
      Break;
    end;
  end;
  Result := (AInterfaceLine >= 0) and (AImplementationLine >= 0);
end;

function GetSectionAtLine(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ALine: Integer): TUnitSection;
var
  intfLine, implLine: Integer;
begin
  Result := usUnknown;
  if not FindUnitSections(AHighlighter, ALines, intfLine, implLine) then
    Exit;
  if ALine <= intfLine then
    Result := usUnknown
  else if ALine < implLine then
    Result := usInterface
  else
    Result := usImplementation;
end;

{ Find the enclosing method header for a cursor position within the
  implementation section. Scans backwards from ACursorLine looking
  for the nearest method declaration line. }
function FindEnclosingMethodLine(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ACursorLine, AImplementationLine: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := ACursorLine downto AImplementationLine + 1 do
  begin
    if IsMethodLine(AHighlighter, ALines, i) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function NavigateToInterface(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ACursorLine: Integer): TNavigationResult;
var
  intfLine, implLine: Integer;
  methodLine: Integer;
  methodName, candidateName: string;
  i: Integer;
begin
  Result.Found := False;
  Result.Line := 0;

  if not FindUnitSections(AHighlighter, ALines, intfLine, implLine) then
    Exit;

  { If cursor is not in implementation section, nothing to do }
  if ACursorLine <= implLine then
    Exit;

  { Find the enclosing method }
  methodLine := FindEnclosingMethodLine(AHighlighter, ALines,
    ACursorLine, implLine);
  if methodLine < 0 then
  begin
    { No enclosing method — jump to the interface keyword }
    Result.Found := True;
    Result.Line := intfLine;
    Exit;
  end;

  methodName := ExtractMethodNameFromLine(AHighlighter, ALines, methodLine);
  if methodName = '' then
  begin
    Result.Found := True;
    Result.Line := intfLine;
    Exit;
  end;

  { Search the interface section for a matching declaration.
    For implementation methods like 'tmainform.formshow', we need to
    match against just the method name part 'formshow' within the
    class declaration, or against the full qualified name if it
    appears as a standalone declaration. }
  for i := intfLine + 1 to implLine - 1 do
  begin
    if IsMethodLine(AHighlighter, ALines, i) then
    begin
      candidateName := ExtractMethodNameFromLine(AHighlighter, ALines, i);
      if candidateName = methodName then
      begin
        Result.Found := True;
        Result.Line := i;
        Exit;
      end;
      { Check if the interface has just the short name (inside a class declaration)
        and the implementation has the qualified ClassName.MethodName form }
      if (Pos('.', methodName) > 0) and
         (candidateName = Copy(methodName, Pos('.', methodName) + 1, MaxInt)) then
      begin
        Result.Found := True;
        Result.Line := i;
        Exit;
      end;
    end;
  end;

  { Not found in interface — might be a standalone implementation routine.
    Jump to the interface keyword. }
  Result.Found := True;
  Result.Line := intfLine;
end;

function NavigateToImplementation(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ACursorLine: Integer): TNavigationResult;
var
  intfLine, implLine: Integer;
  methodName, candidateName: string;
  i: Integer;
begin
  Result.Found := False;
  Result.Line := 0;

  if not FindUnitSections(AHighlighter, ALines, intfLine, implLine) then
    Exit;

  { If cursor is not in the interface section, nothing to do }
  if (ACursorLine <= intfLine) or (ACursorLine >= implLine) then
    Exit;

  { Check if cursor is on a method declaration line }
  if not IsMethodLine(AHighlighter, ALines, ACursorLine) then
  begin
    { Not on a method line — jump to the implementation keyword }
    Result.Found := True;
    Result.Line := implLine;
    Exit;
  end;

  methodName := ExtractMethodNameFromLine(AHighlighter, ALines, ACursorLine);
  if methodName = '' then
  begin
    Result.Found := True;
    Result.Line := implLine;
    Exit;
  end;

  { Search the implementation section for a matching method.
    Implementation methods are typically qualified (ClassName.MethodName),
    so match the short name from the interface against the suffix. }
  for i := implLine + 1 to ALines.Count - 1 do
  begin
    if IsMethodLine(AHighlighter, ALines, i) then
    begin
      candidateName := ExtractMethodNameFromLine(AHighlighter, ALines, i);
      if candidateName = methodName then
      begin
        Result.Found := True;
        Result.Line := i;
        Exit;
      end;
      { Interface has short name, implementation has ClassName.MethodName }
      if (Pos('.', candidateName) > 0) and
         (methodName = Copy(candidateName, Pos('.', candidateName) + 1, MaxInt)) then
      begin
        Result.Found := True;
        Result.Line := i;
        Exit;
      end;
    end;
  end;

  { Not found — jump to implementation keyword }
  Result.Found := True;
  Result.Line := implLine;
end;

function NavigateInterfaceImplementation(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ACursorLine: Integer): TNavigationResult;
var
  section: TUnitSection;
begin
  section := GetSectionAtLine(AHighlighter, ALines, ACursorLine);
  case section of
    usInterface:
      Result := NavigateToImplementation(AHighlighter, ALines, ACursorLine);
    usImplementation:
      Result := NavigateToInterface(AHighlighter, ALines, ACursorLine);
    else
    begin
      Result.Found := False;
      Result.Line := 0;
    end;
  end;
end;

end.
