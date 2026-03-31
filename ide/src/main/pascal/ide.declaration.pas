{
    fpGUI IDE - Go to Declaration

    Copyright (C) 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Provides "Go to Declaration" functionality using lightweight
      token scanning. Ctrl+B on an identifier navigates to where
      it is declared, including cross-unit resolution via uses clauses.
}
unit ide.declaration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ide.highlighter;

type
  TDeclarationResult = record
    Found: Boolean;
    DeclFile: string;     // absolute path to declaration's source file
    DeclLine: Integer;    // 1-based line number
    DeclName: string;     // name of the resolved declaration
  end;

// ALine and ACol are both 0-based (matching editor CaretPos_V / CaretPos_H)
function GetIdentifierAtCursor(AHighlighter: TPascalHighlighter;
  ALines: TStrings; ALine, ACol: Integer): string;

// ALine and ACol are 0-based. Converted to 1-based internally.
function FindDeclaration(AHighlighter: TPascalHighlighter;
  ALines: TStrings; const AFilename: string;
  ALine, ACol: Integer;
  AUnitPaths, AIncludePaths: TStrings): TDeclarationResult;


implementation

uses
  ide.pascal.tokeniser;

type
  TDeclMatch = record
    Found: Boolean;
    Line: Integer;       // 1-based
    Name: string;
    InInterface: Boolean;
  end;


// Helper: extract text for a token from the source line
function HLTokenText(ALines: TStrings; ALineIdx: Integer;
  const AToken: THighlightToken): string;
begin
  if (ALineIdx >= 0) and (ALineIdx < ALines.Count) then
    Result := Copy(ALines[ALineIdx], AToken.Column + 1, AToken.Length)
  else
    Result := '';
end;

function GetIdentifierAtCursor(AHighlighter: TPascalHighlighter;
  ALines: TStrings; ALine, ACol: Integer): string;
var
  tokens: THighlightTokenArray;
  i: Integer;
  tok: THighlightToken;
begin
  Result := '';
  if (ALine < 0) or (ALine >= AHighlighter.LineCount) then
    Exit;
  tokens := AHighlighter.GetLineTokens(ALine);
  for i := 0 to Length(tokens) - 1 do
  begin
    tok := tokens[i];
    if (ACol >= tok.Column) and (ACol < tok.Column + tok.Length) then
    begin
      if tok.Category = hcIdentifier then
        Result := HLTokenText(ALines, ALine, tok);
      Exit;
    end;
  end;
end;


// Search unit paths for a source file. Returns absolute path or ''.
function SearchUnitFile(APaths: TStrings; const AUnitName: string): string;
var
  i: Integer;
  dir, candidate: string;
begin
  Result := '';
  if APaths = nil then
    Exit;
  for i := 0 to APaths.Count - 1 do
  begin
    dir := IncludeTrailingPathDelimiter(APaths[i]);
    candidate := dir + AUnitName + '.pas';
    if FileExists(candidate) then Exit(candidate);
    candidate := dir + LowerCase(AUnitName) + '.pas';
    if FileExists(candidate) then Exit(candidate);
    candidate := dir + AUnitName + '.pp';
    if FileExists(candidate) then Exit(candidate);
    candidate := dir + LowerCase(AUnitName) + '.pp';
    if FileExists(candidate) then Exit(candidate);
  end;
end;


// Scan Pascal source for a declaration of AIdent. If AInterfaceOnly is True,
// only the interface section of a unit is scanned. Returns the best match,
// preferring interface declarations over implementation ones.
function ScanSourceForDecl(const ASource, AIdent: string;
  AInterfaceOnly: Boolean): TDeclMatch;
var
  Tokeniser: TFpgPascalTokeniser;
  Tok: TFpgPasToken;
  TokUp: string;
  Section: Integer;  // 0=none, 1=type, 2=const, 3=var
  IsUnit, InInterface: Boolean;
  NestDepth: Integer;
  IdentUp: string;
  TmpName, TmpUp: string;
  TmpLine: Integer;

  procedure FetchTok;
  begin
    repeat
      Tok := Tokeniser.NextToken;
    until not (Tok.Kind in [fptkWhitespace, fptkLineEnding,
                            fptkComment, fptkDirective]);
    TokUp := Tokeniser.TokenTextUpper;
  end;

  function IsKW(const AW: string): Boolean; inline;
  begin
    Result := (Tok.Kind = fptkKeyword) and (TokUp = AW);
  end;

  function IsSy(const AC: string): Boolean; inline;
  begin
    Result := (Tok.Kind = fptkSymbol) and (Tokeniser.TokenText = AC);
  end;

  procedure TryRecord(ALine: Integer; const AName: string);
  begin
    // Prefer interface matches; don't overwrite one with implementation
    if Result.Found and Result.InInterface then
      Exit;
    Result.Found := True;
    Result.Line := ALine;
    Result.Name := AName;
    Result.InInterface := InInterface;
  end;

  procedure SkipToSemicolon;
  begin
    while (Tok.Kind <> fptkEOF) and not IsSy(';') do
      FetchTok;
  end;

  procedure ScanClassBody;
  // Scan inside a class/record/object body for matching declarations
  var
    FldName, FldUp: string;
    FldLine: Integer;
  begin
    NestDepth := 1;
    while (Tok.Kind <> fptkEOF) and (NestDepth > 0) do
    begin
      if IsKW('RECORD') then
        Inc(NestDepth)
      else if IsKW('END') then
      begin
        Dec(NestDepth);
        if NestDepth <= 0 then
          Break;
      end
      else if (Tok.Kind = fptkKeyword) and
         ((TokUp = 'PROCEDURE') or (TokUp = 'FUNCTION') or
          (TokUp = 'CONSTRUCTOR') or (TokUp = 'DESTRUCTOR')) then
      begin
        FetchTok;
        if (Tok.Kind = fptkIdentifier) and (TokUp = IdentUp) then
          TryRecord(Tok.Line, Tokeniser.TokenText);
        SkipToSemicolon;
        if IsSy(';') then FetchTok;
        Continue;
      end
      else if IsKW('PROPERTY') then
      begin
        FetchTok;
        if (Tok.Kind = fptkIdentifier) and (TokUp = IdentUp) then
          TryRecord(Tok.Line, Tokeniser.TokenText);
        SkipToSemicolon;
        if IsSy(';') then FetchTok;
        Continue;
      end
      else if (Tok.Kind = fptkIdentifier) then
      begin
        // Potential field: <Name> :
        FldName := Tokeniser.TokenText;
        FldUp := TokUp;
        FldLine := Tok.Line;
        FetchTok;
        if IsSy(':') and (FldUp = IdentUp) then
          TryRecord(FldLine, FldName);
        // Skip rest of this declaration
        while (Tok.Kind <> fptkEOF) and not IsSy(';') and not IsKW('END') do
          FetchTok;
        if IsSy(';') then FetchTok;
        Continue;
      end;
      FetchTok;
    end;
    NestDepth := 0;
  end;

begin
  Result.Found := False;
  Result.InInterface := False;
  IdentUp := UpperCase(AIdent);

  Tokeniser := TFpgPascalTokeniser.Create;
  try
    Tokeniser.SetSource(ASource);
    FetchTok;

    // Determine file type
    IsUnit := IsKW('UNIT');
    if IsUnit then
    begin
      InInterface := False;
      while Tok.Kind <> fptkEOF do
      begin
        if IsKW('INTERFACE') then
        begin
          InInterface := True;
          FetchTok;
          Break;
        end;
        FetchTok;
      end;
      if not InInterface then
        Exit;
    end
    else
      // Program/library: treat all declarations as "interface"
      InInterface := True;

    Section := 0;
    NestDepth := 0;

    while Tok.Kind <> fptkEOF do
    begin
      // Track interface/implementation boundary
      if IsUnit and IsKW('IMPLEMENTATION') then
      begin
        if AInterfaceOnly then
          Break;
        InInterface := False;
        Section := 0;
        FetchTok;
        Continue;
      end;

      // Section keywords
      if IsKW('TYPE') then begin Section := 1; FetchTok; Continue; end;
      if IsKW('CONST') then begin Section := 2; FetchTok; Continue; end;
      if IsKW('VAR') then begin Section := 3; FetchTok; Continue; end;
      if IsKW('BEGIN') then begin Section := 0; FetchTok; Continue; end;

      // Procedure/function declarations
      if (Tok.Kind = fptkKeyword) and
         ((TokUp = 'PROCEDURE') or (TokUp = 'FUNCTION') or
          (TokUp = 'CONSTRUCTOR') or (TokUp = 'DESTRUCTOR')) then
      begin
        Section := 0;
        FetchTok;
        if Tok.Kind = fptkIdentifier then
        begin
          TmpName := Tokeniser.TokenText;
          TmpUp := TokUp;
          TmpLine := Tok.Line;
          FetchTok;
          // Check for ClassName.MethodName
          if IsSy('.') then
          begin
            FetchTok;
            if (Tok.Kind = fptkIdentifier) or (Tok.Kind = fptkKeyword) then
            begin
              TmpName := Tokeniser.TokenText;
              TmpUp := TokUp;
              TmpLine := Tok.Line;
              FetchTok;
            end;
          end;
          // Check proc name match
          if TmpUp = IdentUp then
            TryRecord(TmpLine, TmpName);
          // Scan parameters
          if IsSy('(') then
          begin
            FetchTok;
            while (Tok.Kind <> fptkEOF) and not IsSy(')') do
            begin
              if (Tok.Kind = fptkIdentifier) and (TokUp = IdentUp) then
              begin
                TmpLine := Tok.Line;
                TmpName := Tokeniser.TokenText;
                FetchTok;
                if IsSy(':') then
                  TryRecord(TmpLine, TmpName);
                Continue;
              end;
              FetchTok;
            end;
          end;
        end;
        SkipToSemicolon;
        if IsSy(';') then FetchTok;
        Continue;
      end;

      // Type declarations: <Name> =
      if (Section = 1) and (NestDepth = 0) and (Tok.Kind = fptkIdentifier) then
      begin
        TmpName := Tokeniser.TokenText;
        TmpUp := TokUp;
        TmpLine := Tok.Line;
        FetchTok;
        if IsSy('=') then
        begin
          if TmpUp = IdentUp then
            TryRecord(TmpLine, TmpName);
          FetchTok;
          // Check for class/record/object body
          if IsKW('CLASS') or IsKW('RECORD') or IsKW('OBJECT') or
             IsKW('PACKED') or IsKW('BITPACKED') then
          begin
            // Handle packed record / bitpacked record
            if IsKW('PACKED') or IsKW('BITPACKED') then
              FetchTok;
            if IsKW('CLASS') or IsKW('RECORD') or IsKW('OBJECT') then
            begin
              FetchTok;
              // Forward declaration: class;
              if IsSy(';') then
              begin
                FetchTok;
                Continue;
              end;
              // Skip optional inheritance: class(TParent)
              if IsSy('(') then
              begin
                while (Tok.Kind <> fptkEOF) and not IsSy(')') do
                  FetchTok;
                if IsSy(')') then
                  FetchTok;
              end;
              ScanClassBody;
              // Skip past 'end'
              if IsKW('END') then FetchTok;
              SkipToSemicolon;
              if IsSy(';') then FetchTok;
              Continue;
            end;
          end;
          // Simple type — skip to semicolon
          SkipToSemicolon;
          if IsSy(';') then FetchTok;
          Continue;
        end;
        // Not followed by '=' — skip
        Continue;
      end;

      // Const declarations: <Name> = or <Name> :
      if (Section = 2) and (NestDepth = 0) and (Tok.Kind = fptkIdentifier) then
      begin
        TmpName := Tokeniser.TokenText;
        TmpUp := TokUp;
        TmpLine := Tok.Line;
        FetchTok;
        if (IsSy('=') or IsSy(':')) and (TmpUp = IdentUp) then
          TryRecord(TmpLine, TmpName);
        SkipToSemicolon;
        if IsSy(';') then FetchTok;
        Continue;
      end;

      // Var declarations: <Name> :
      if (Section = 3) and (NestDepth = 0) and (Tok.Kind = fptkIdentifier) then
      begin
        TmpName := Tokeniser.TokenText;
        TmpUp := TokUp;
        TmpLine := Tok.Line;
        FetchTok;
        if IsSy(':') and (TmpUp = IdentUp) then
          TryRecord(TmpLine, TmpName);
        SkipToSemicolon;
        if IsSy(';') then FetchTok;
        Continue;
      end;

      FetchTok;
    end;
  finally
    Tokeniser.Free;
  end;
end;


// Extract unit names from uses clauses in the source
procedure CollectUsedUnits(const ASource: string; ANames: TStrings);
var
  Tokeniser: TFpgPascalTokeniser;
  Tok: TFpgPasToken;
  UnitName: string;
begin
  Tokeniser := TFpgPascalTokeniser.Create;
  try
    Tokeniser.SetSource(ASource);
    repeat
      Tok := Tokeniser.NextToken;
      if Tok.Kind in [fptkWhitespace, fptkLineEnding, fptkComment, fptkDirective] then
        Continue;
      if Tok.Kind = fptkEOF then
        Break;

      if (Tok.Kind = fptkKeyword) and (Tokeniser.TokenTextUpper = 'USES') then
      begin
        // Parse uses clause entries
        repeat
          Tok := Tokeniser.NextToken;
          if Tok.Kind in [fptkWhitespace, fptkLineEnding, fptkComment, fptkDirective] then
            Continue;
          if Tok.Kind = fptkEOF then Break;

          // Unit name starts with identifier (or keyword for dotted names)
          if (Tok.Kind = fptkIdentifier) or (Tok.Kind = fptkKeyword) then
          begin
            UnitName := Tokeniser.TokenText;
            // Check for dotted name continuation
            repeat
              Tok := Tokeniser.NextToken;
              if Tok.Kind in [fptkWhitespace, fptkLineEnding, fptkComment, fptkDirective] then
                Continue;
              if (Tok.Kind = fptkSymbol) and (Tokeniser.TokenText = '.') then
              begin
                UnitName := UnitName + '.';
                repeat
                  Tok := Tokeniser.NextToken;
                until not (Tok.Kind in [fptkWhitespace, fptkLineEnding,
                                        fptkComment, fptkDirective]);
                if (Tok.Kind = fptkIdentifier) or (Tok.Kind = fptkKeyword) then
                  UnitName := UnitName + Tokeniser.TokenText
                else
                  Break;
              end
              else
                Break;
            until Tok.Kind = fptkEOF;
            ANames.Add(UnitName);
            // Skip optional 'in' filename
            if (Tok.Kind = fptkKeyword) and (Tokeniser.TokenTextUpper = 'IN') then
            begin
              repeat
                Tok := Tokeniser.NextToken;
              until not (Tok.Kind in [fptkWhitespace, fptkLineEnding,
                                      fptkComment, fptkDirective]);
              // Skip the filename string
              if Tok.Kind = fptkString then
              begin
                repeat
                  Tok := Tokeniser.NextToken;
                until not (Tok.Kind in [fptkWhitespace, fptkLineEnding,
                                        fptkComment, fptkDirective]);
              end;
            end;
          end;

          // End of uses clause
          if (Tok.Kind = fptkSymbol) and (Tokeniser.TokenText = ';') then
            Break;
        until Tok.Kind = fptkEOF;
      end;

      // Stop at implementation (only scan interface uses for cross-unit)
      if (Tok.Kind = fptkKeyword) and (Tokeniser.TokenTextUpper = 'IMPLEMENTATION') then
        Break;
    until Tok.Kind = fptkEOF;
  finally
    Tokeniser.Free;
  end;
end;


function FindDeclaration(AHighlighter: TPascalHighlighter;
  ALines: TStrings; const AFilename: string;
  ALine, ACol: Integer;
  AUnitPaths, AIncludePaths: TStrings): TDeclarationResult;
var
  ident, sourceText: string;
  match: TDeclMatch;
  usedUnits: TStringList;
  unitPath, unitSource: string;
  fs: TFileStream;
  i, sz: Integer;
begin
  Result.Found := False;
  Result.DeclFile := '';
  Result.DeclLine := 0;
  Result.DeclName := '';

  // Step 1: Extract identifier at cursor
  ident := GetIdentifierAtCursor(AHighlighter, ALines, ALine, ACol);
  if ident = '' then
    Exit;

  sourceText := ALines.Text;

  // Step 2: Scan current file for declaration
  match := ScanSourceForDecl(sourceText, ident, False);
  if match.Found then
  begin
    Result.Found := True;
    Result.DeclFile := AFilename;
    Result.DeclLine := match.Line;
    Result.DeclName := match.Name;
    Exit;
  end;

  // Step 3: Scan used units (interface sections only)
  usedUnits := TStringList.Create;
  try
    CollectUsedUnits(sourceText, usedUnits);
    for i := 0 to usedUnits.Count - 1 do
    begin
      unitPath := SearchUnitFile(AUnitPaths, usedUnits[i]);
      if unitPath = '' then
        Continue;
      // Read unit source
      try
        fs := TFileStream.Create(unitPath, fmOpenRead or fmShareDenyNone);
        try
          sz := fs.Size;
          SetLength(unitSource, sz);
          if sz > 0 then
            fs.Read(unitSource[1], sz);
        finally
          fs.Free;
        end;
      except
        Continue;
      end;
      // Scan interface only
      match := ScanSourceForDecl(unitSource, ident, True);
      if match.Found then
      begin
        Result.Found := True;
        Result.DeclFile := unitPath;
        Result.DeclLine := match.Line;
        Result.DeclName := match.Name;
        Exit;
      end;
    end;
  finally
    usedUnits.Free;
  end;
end;

end.
