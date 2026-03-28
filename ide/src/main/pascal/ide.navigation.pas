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
    Filename: string;  // non-empty when target is in a different file
  end;

{ Determine which section the given 0-based line is in, using the
  tokenised highlight data. }
function GetSectionAtLine(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ALine: Integer): TUnitSection;

{ Toggle: navigate from interface to implementation or vice versa.
  ACursorLine is 0-based. ACurrentFilePath is passed through for
  include file searching. }
function NavigateInterfaceImplementation(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ACursorLine: Integer;
  const ACurrentFilePath: string = ''): TNavigationResult;

{ Jump to the interface declaration (from implementation).
  ACurrentFilePath is optional — when provided and no interface section
  is found (include file), the mainunit directive is used to locate
  the parent unit and navigate to the interface declaration there. }
function NavigateToInterface(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ACursorLine: Integer;
  const ACurrentFilePath: string = ''): TNavigationResult;

{ Jump to the implementation (from interface).
  ACurrentFilePath is optional — when provided, include files in the
  implementation section will be searched if the method is not found
  in the current buffer. }
function NavigateToImplementation(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ACursorLine: Integer;
  const ACurrentFilePath: string = ''): TNavigationResult;


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

// Extract the mainunit filename from a {%mainunit filename} directive.
// Scans the first 30 lines of the file. Returns empty if not found.
function ExtractMainUnitName(ALines: TStrings): string;
var
  i, p, endP, maxLine: Integer;
  line, upper: string;
begin
  Result := '';
  maxLine := ALines.Count - 1;
  if maxLine > 29 then
    maxLine := 29;
  for i := 0 to maxLine do
  begin
    line := ALines[i];
    upper := UpperCase(line);
    p := Pos('{%MAINUNIT ', upper);
    if p > 0 then
    begin
      p := p + 11; // skip '{%mainunit '
      endP := p;
      while (endP <= Length(line)) and (line[endP] <> '}') do
        Inc(endP);
      Result := Trim(Copy(line, p, endP - p));
      Exit;
    end;
  end;
end;

// Search a parent unit file for the interface declaration of a method.
// Returns True if found, with AResultLine set to the 0-based line number.
function FindMethodInParentUnit(const AUnitPath, AMethodName: string;
  out AResultLine: Integer): Boolean;
var
  UnitHL: TPascalHighlighter;
  UnitLines: TStringList;
  UnitSource: string;
  SFile: TFileStream;
  Size: Integer;
  intfLine, implLine: Integer;
  candidateName: string;
  i: Integer;
begin
  Result := False;
  AResultLine := 0;

  if not FileExists(AUnitPath) then
    Exit;

  SFile := TFileStream.Create(AUnitPath, fmOpenRead or fmShareDenyWrite);
  try
    Size := SFile.Size;
    SetLength(UnitSource, Size);
    if Size > 0 then
      SFile.Read(UnitSource[1], Size);
  finally
    SFile.Free;
  end;

  if UnitSource = '' then
    Exit;

  UnitHL := TPascalHighlighter.Create;
  UnitLines := TStringList.Create;
  try
    UnitLines.Text := UnitSource;
    UnitHL.Tokenise(UnitSource);

    if not FindUnitSections(UnitHL, UnitLines, intfLine, implLine) then
      Exit;

    for i := intfLine + 1 to implLine - 1 do
    begin
      if IsMethodLine(UnitHL, UnitLines, i) then
      begin
        candidateName := ExtractMethodNameFromLine(UnitHL, UnitLines, i);
        if candidateName = AMethodName then
        begin
          Result := True;
          AResultLine := i;
          Exit;
        end;
        // Implementation has ClassName.MethodName, interface has short name
        if (Pos('.', AMethodName) > 0) and
           (candidateName = Copy(AMethodName, Pos('.', AMethodName) + 1, MaxInt)) then
        begin
          Result := True;
          AResultLine := i;
          Exit;
        end;
      end;
    end;
  finally
    UnitLines.Free;
    UnitHL.Free;
  end;
end;

function NavigateToInterface(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ACursorLine: Integer;
  const ACurrentFilePath: string = ''): TNavigationResult;
var
  intfLine, implLine: Integer;
  methodLine: Integer;
  methodName, candidateName: string;
  mainUnitName, mainUnitPath, baseDir: string;
  resultLine: Integer;
  i: Integer;
begin
  Result.Found := False;
  Result.Line := 0;
  Result.Filename := '';

  if not FindUnitSections(AHighlighter, ALines, intfLine, implLine) then
  begin
    // No interface/implementation sections — likely an include file.
    // Look for {%mainunit} directive to find the parent unit.
    if ACurrentFilePath = '' then
      Exit;

    mainUnitName := ExtractMainUnitName(ALines);
    if mainUnitName = '' then
      Exit;

    // Find the method at the cursor position
    methodName := '';
    if IsMethodLine(AHighlighter, ALines, ACursorLine) then
      methodName := ExtractMethodNameFromLine(AHighlighter, ALines, ACursorLine)
    else
    begin
      // Search backwards for the enclosing method
      for i := ACursorLine downto 0 do
      begin
        if IsMethodLine(AHighlighter, ALines, i) then
        begin
          methodName := ExtractMethodNameFromLine(AHighlighter, ALines, i);
          Break;
        end;
      end;
    end;

    // Resolve parent unit path relative to current file's directory
    // and its parent directory (include files are often in subdirectories)
    baseDir := ExtractFilePath(ACurrentFilePath);
    mainUnitPath := baseDir + mainUnitName;
    if not FileExists(mainUnitPath) then
    begin
      // Try parent directory
      mainUnitPath := ExtractFilePath(ExcludeTrailingPathDelimiter(baseDir)) + mainUnitName;
      if not FileExists(mainUnitPath) then
        Exit;
    end;

    if (methodName <> '') and
       FindMethodInParentUnit(mainUnitPath, methodName, resultLine) then
    begin
      Result.Found := True;
      Result.Line := resultLine;
      Result.Filename := mainUnitPath;
    end
    else
    begin
      // Method not found — just open the parent unit at the top
      Result.Found := True;
      Result.Line := 0;
      Result.Filename := mainUnitPath;
    end;
    Exit;
  end;

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

// Extract an include filename from a line containing an $I or $INCLUDE
// directive. Returns empty string if not an include directive.
function ExtractIncludeFilename(const ALine: string): string;
var
  p, startPos, endPos: Integer;
  upper: string;
begin
  Result := '';
  upper := UpperCase(ALine);

  // Look for $I or $INCLUDE directives
  p := Pos('{$I ', upper);
  if p = 0 then
    p := Pos('{$INCLUDE ', upper);
  if p = 0 then
    Exit;

  // Find the start of the filename after the directive
  startPos := p + 3; // skip the directive prefix
  if Copy(upper, p, 10) = '{$INCLUDE ' then
    startPos := p + 10
  else
    startPos := p + 4; // skip '$I ' plus opening brace

  // Skip any leading whitespace
  while (startPos <= Length(ALine)) and (ALine[startPos] = ' ') do
    Inc(startPos);

  { Find the closing brace }
  endPos := startPos;
  while (endPos <= Length(ALine)) and (ALine[endPos] <> '}') do
    Inc(endPos);

  if endPos > startPos then
    Result := Trim(Copy(ALine, startPos, endPos - startPos));

  { Remove quotes if present }
  if (Length(Result) >= 2) and (Result[1] = '''') and (Result[Length(Result)] = '''') then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

{ Search an include file for a method implementation matching AMethodName.
  ABaseDir is the directory of the file containing the include directive.
  Returns True if found, with AResultLine set to the 0-based line number. }
function FindMethodInIncludeFile(const AIncludePath, AMethodName: string;
  out AResultLine: Integer): Boolean;
var
  IncHL: TPascalHighlighter;
  IncLines: TStringList;
  IncSource: string;
  SFile: TFileStream;
  Size: Integer;
  candidateName: string;
  i: Integer;
begin
  Result := False;
  AResultLine := 0;

  if not FileExists(AIncludePath) then
    Exit;

  SFile := TFileStream.Create(AIncludePath, fmOpenRead or fmShareDenyWrite);
  try
    Size := SFile.Size;
    SetLength(IncSource, Size);
    if Size > 0 then
      SFile.Read(IncSource[1], Size);
  finally
    SFile.Free;
  end;

  if IncSource = '' then
    Exit;

  IncHL := TPascalHighlighter.Create;
  IncLines := TStringList.Create;
  try
    IncLines.Text := IncSource;
    IncHL.Tokenise(IncSource);

    for i := 0 to IncLines.Count - 1 do
    begin
      if IsMethodLine(IncHL, IncLines, i) then
      begin
        candidateName := ExtractMethodNameFromLine(IncHL, IncLines, i);
        if candidateName = AMethodName then
        begin
          Result := True;
          AResultLine := i;
          Exit;
        end;
        { Interface short name vs implementation ClassName.MethodName }
        if (Pos('.', candidateName) > 0) and
           (AMethodName = Copy(candidateName, Pos('.', candidateName) + 1, MaxInt)) then
        begin
          Result := True;
          AResultLine := i;
          Exit;
        end;
      end;
    end;
  finally
    IncLines.Free;
    IncHL.Free;
  end;
end;

// Resolve an include filename relative to a base directory.
// First checks the base directory itself, then searches subdirectories
// (one level deep) since FPC resolves includes via search paths that
// commonly include platform-specific subdirectories.
function ResolveIncludePath(const ABaseDir, AIncName: string): string;
var
  sr: TSearchRec;
  candidate: string;
begin
  // Check same directory first
  candidate := ABaseDir + AIncName;
  if FileExists(candidate) then
  begin
    Result := candidate;
    Exit;
  end;

  // Search one level of subdirectories
  if FindFirst(ABaseDir + AllFilesMask, faDirectory, sr) = 0 then
  begin
    try
      repeat
        if ((sr.Attr and faDirectory) <> 0) and
           (sr.Name <> '.') and (sr.Name <> '..') then
        begin
          candidate := ABaseDir + sr.Name + PathDelim + AIncName;
          if FileExists(candidate) then
          begin
            Result := candidate;
            Exit;
          end;
        end;
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;

  Result := '';
end;

function NavigateToImplementation(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ACursorLine: Integer;
  const ACurrentFilePath: string = ''): TNavigationResult;
var
  intfLine, implLine: Integer;
  methodName, candidateName: string;
  incName, incPath, baseDir: string;
  incLine: Integer;
  i: Integer;
begin
  Result.Found := False;
  Result.Line := 0;
  Result.Filename := '';

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

  { Method not found in current file — search include files in the
    implementation section }
  if ACurrentFilePath <> '' then
  begin
    baseDir := ExtractFilePath(ACurrentFilePath);
    for i := implLine + 1 to ALines.Count - 1 do
    begin
      incName := ExtractIncludeFilename(ALines[i]);
      if incName <> '' then
      begin
        incPath := ResolveIncludePath(baseDir, incName);
        if (incPath <> '') and
           FindMethodInIncludeFile(incPath, methodName, incLine) then
        begin
          Result.Found := True;
          Result.Line := incLine;
          Result.Filename := incPath;
          Exit;
        end;
      end;
    end;
  end;

  { Not found — jump to implementation keyword }
  Result.Found := True;
  Result.Line := implLine;
end;

function NavigateInterfaceImplementation(AHighlighter: TEditorHighlighter;
  ALines: TStrings; ACursorLine: Integer;
  const ACurrentFilePath: string = ''): TNavigationResult;
var
  section: TUnitSection;
begin
  section := GetSectionAtLine(AHighlighter, ALines, ACursorLine);
  case section of
    usInterface:
      Result := NavigateToImplementation(AHighlighter, ALines, ACursorLine,
        ACurrentFilePath);
    usImplementation:
      Result := NavigateToInterface(AHighlighter, ALines, ACursorLine,
        ACurrentFilePath);
    else
      // usUnknown — could be an include file; try navigating to interface
      Result := NavigateToInterface(AHighlighter, ALines, ACursorLine,
        ACurrentFilePath);
  end;
end;

end.
