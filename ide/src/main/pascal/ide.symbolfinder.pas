{
    fpGUI IDE - Symbol Finder

    Copyright (C) 2026 by Graeme Geldenhuys

    Symbol extraction from Pascal source files and fuzzy filtering
    for the Navigate to Symbol dialog (Ctrl+N).

    Pure logic unit — no GUI dependencies.
}
unit ide.symbolfinder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ide.filefinder;

type

  TSymbolKind = (
    skProcedure,
    skFunction,
    skConstructor,
    skDestructor,
    skType,
    skConst,
    skVar,
    skProperty
  );

  TSymbolEntry = record
    Name: string;           // symbol name (e.g. 'TMainForm', 'LoadProcs')
    DisplayName: string;    // qualified display (e.g. 'TMainForm.LoadProcs')
    Kind: TSymbolKind;
    FileName: string;       // just the filename portion
    RelativePath: string;   // relative dir from project root
    FullPath: string;       // absolute path on disk
    Line: Integer;          // 1-based line number
  end;
  TSymbolEntryArray = array of TSymbolEntry;

  TFilteredSymbolEntry = record
    Entry: TSymbolEntry;
    Score: Integer;
    MatchRanges: TMatchRangeArray;
  end;
  TFilteredSymbolArray = array of TFilteredSymbolEntry;

function SymbolKindToStr(AKind: TSymbolKind): string;

{ Scan Pascal source text for symbols in the interface section.
  AFileName is used to populate the FileName field of each entry. }
procedure ScanSourceForSymbols(const ASource, AFileName: string;
  var ASymbols: TSymbolEntryArray);

{ Scan a Pascal source file on disk for symbols. }
procedure ScanFileForSymbols(const AFilePath, AProjectDir: string;
  var ASymbols: TSymbolEntryArray);

{ Collect symbols from all files in AFiles. }
procedure CollectProjectSymbols(const AFiles: TFileEntryArray;
  var ASymbols: TSymbolEntryArray);

{ Filter symbols by fuzzy matching on Name field. }
function FilterSymbolsEx(const APattern: string;
  const ASymbols: TSymbolEntryArray): TFilteredSymbolArray;


implementation

uses
  ide.pascal.tokeniser;

function SymbolKindToStr(AKind: TSymbolKind): string;
begin
  case AKind of
    skProcedure:   Result := 'procedure';
    skFunction:    Result := 'function';
    skConstructor: Result := 'constructor';
    skDestructor:  Result := 'destructor';
    skType:        Result := 'type';
    skConst:       Result := 'const';
    skVar:         Result := 'var';
    skProperty:    Result := 'property';
  else
    Result := '';
  end;
end;

procedure AddSymbol(var ASymbols: TSymbolEntryArray;
  const AName, ADisplayName, AFileName, ARelPath, AFullPath: string;
  AKind: TSymbolKind; ALine: Integer);
var
  Idx: Integer;
begin
  Idx := Length(ASymbols);
  SetLength(ASymbols, Idx + 1);
  ASymbols[Idx].Name := AName;
  ASymbols[Idx].DisplayName := ADisplayName;
  ASymbols[Idx].Kind := AKind;
  ASymbols[Idx].FileName := AFileName;
  ASymbols[Idx].RelativePath := ARelPath;
  ASymbols[Idx].FullPath := AFullPath;
  ASymbols[Idx].Line := ALine;
end;

procedure ScanSourceForSymbols(const ASource, AFileName: string;
  var ASymbols: TSymbolEntryArray);
var
  Tokeniser: TFpgPascalTokeniser;
  Tok: TFpgPasToken;
  TokUpper: string;
  IsUnit: Boolean;
  InInterface: Boolean;
  Section: (secNone, secType, secConst, secVar, secOther);
  TypeNestDepth: Integer;
  CurrentClassName: string;
  SymName: string;
  Line: Integer;
  SymKind: TSymbolKind;

  procedure FetchToken;
  begin
    repeat
      Tok := Tokeniser.NextToken;
    until not (Tok.Kind in [fptkWhitespace, fptkLineEnding,
                            fptkComment, fptkDirective]);
    TokUpper := Tokeniser.TokenTextUpper;
  end;

  function IsKW(const AWord: string): Boolean; inline;
  begin
    Result := (Tok.Kind = fptkKeyword) and (TokUpper = AWord);
  end;

  function IsSym(const ACh: string): Boolean; inline;
  begin
    Result := (Tok.Kind = fptkSymbol) and (Tokeniser.TokenText = ACh);
  end;

begin
  if Length(ASource) = 0 then
    Exit;

  Tokeniser := TFpgPascalTokeniser.Create;
  try
    Tokeniser.SetSource(ASource);
    FetchToken;

    // Determine if this is a unit (has interface/implementation sections)
    IsUnit := IsKW('UNIT');
    if IsUnit then
    begin
      InInterface := False;
      while Tok.Kind <> fptkEOF do
      begin
        if IsKW('INTERFACE') then
        begin
          InInterface := True;
          FetchToken;
          Break;
        end;
        FetchToken;
      end;
      if not InInterface then
        Exit;
    end;
    // For program/library files, scan the whole top-level

    Section := secNone;
    TypeNestDepth := 0;
    CurrentClassName := '';

    while Tok.Kind <> fptkEOF do
    begin
      // Stop at implementation section for units
      if IsUnit and IsKW('IMPLEMENTATION') then
        Break;

      // Track section changes at top level (not inside a type body)
      if TypeNestDepth = 0 then
      begin
        if IsKW('TYPE') then
        begin
          Section := secType;
          FetchToken;
          Continue;
        end;
        if IsKW('CONST') then
        begin
          Section := secConst;
          FetchToken;
          Continue;
        end;
        if IsKW('VAR') then
        begin
          Section := secVar;
          FetchToken;
          Continue;
        end;
        if (TokUpper = 'PROCEDURE') or (TokUpper = 'FUNCTION') or
           (TokUpper = 'CONSTRUCTOR') or (TokUpper = 'DESTRUCTOR') then
          Section := secOther;
      end;

      // Handle procedures/functions
      if (Tok.Kind = fptkKeyword) and
         ((TokUpper = 'PROCEDURE') or (TokUpper = 'FUNCTION') or
          (TokUpper = 'CONSTRUCTOR') or (TokUpper = 'DESTRUCTOR')) then
      begin
        Line := Tok.Line;
        if TokUpper = 'PROCEDURE' then SymKind := skProcedure
        else if TokUpper = 'FUNCTION' then SymKind := skFunction
        else if TokUpper = 'CONSTRUCTOR' then SymKind := skConstructor
        else SymKind := skDestructor;

        FetchToken;
        if Tok.Kind = fptkIdentifier then
        begin
          SymName := Tokeniser.TokenText;
          if (TypeNestDepth > 0) and (CurrentClassName <> '') then
            AddSymbol(ASymbols, SymName, CurrentClassName + '.' + SymName,
              AFileName, '', '', SymKind, Line)
          else
            AddSymbol(ASymbols, SymName, SymName,
              AFileName, '', '', SymKind, Line);
        end;
        // Skip to semicolon
        while not IsSym(';') and (Tok.Kind <> fptkEOF) do
          FetchToken;
        FetchToken;
        Continue;
      end;

      // Handle type declarations
      if (Section = secType) and (TypeNestDepth = 0) and
         (Tok.Kind = fptkIdentifier) then
      begin
        SymName := Tokeniser.TokenText;
        Line := Tok.Line;
        FetchToken;
        if IsSym('=') then
        begin
          AddSymbol(ASymbols, SymName, SymName,
            AFileName, '', '', skType, Line);
          FetchToken;
          // Check if this starts a class/record/object body
          if IsKW('CLASS') or IsKW('RECORD') or IsKW('OBJECT') then
          begin
            CurrentClassName := SymName;
            TypeNestDepth := 1;
            FetchToken;
            // Check for forward declaration: class;
            if IsSym(';') then
            begin
              TypeNestDepth := 0;
              CurrentClassName := '';
              FetchToken;
            end;
            Continue;
          end
          else
          begin
            // Simple type (enum, range, alias) — skip to semicolon
            while not IsSym(';') and (Tok.Kind <> fptkEOF) do
              FetchToken;
          end;
        end;
        FetchToken;
        Continue;
      end;

      // Track nesting inside class/record/object bodies
      if TypeNestDepth > 0 then
      begin
        if IsKW('RECORD') or IsKW('CLASS') or IsKW('OBJECT') then
          Inc(TypeNestDepth)
        else if IsKW('END') then
        begin
          Dec(TypeNestDepth);
          if TypeNestDepth = 0 then
            CurrentClassName := '';
        end;
      end;

      // Handle const declarations
      if (Section = secConst) and (TypeNestDepth = 0) and
         (Tok.Kind = fptkIdentifier) then
      begin
        SymName := Tokeniser.TokenText;
        Line := Tok.Line;
        FetchToken;
        if IsSym('=') or IsSym(':') then
        begin
          AddSymbol(ASymbols, SymName, SymName,
            AFileName, '', '', skConst, Line);
          while not IsSym(';') and (Tok.Kind <> fptkEOF) do
            FetchToken;
        end;
        FetchToken;
        Continue;
      end;

      // Handle var declarations
      if (Section = secVar) and (TypeNestDepth = 0) and
         (Tok.Kind = fptkIdentifier) then
      begin
        SymName := Tokeniser.TokenText;
        Line := Tok.Line;
        FetchToken;
        if IsSym(':') then
        begin
          AddSymbol(ASymbols, SymName, SymName,
            AFileName, '', '', skVar, Line);
          while not IsSym(';') and (Tok.Kind <> fptkEOF) do
            FetchToken;
        end;
        FetchToken;
        Continue;
      end;

      FetchToken;
    end;
  finally
    Tokeniser.Free;
  end;
end;

procedure ScanFileForSymbols(const AFilePath, AProjectDir: string;
  var ASymbols: TSymbolEntryArray);
var
  SFile: TFileStream;
  SourceText: string;
  Size: Integer;
  FName, RelDir, ProjDir: string;
  i, StartIdx: Integer;
begin
  if not FileExists(AFilePath) then
    Exit;

  FName := ExtractFileName(AFilePath);
  ProjDir := IncludeTrailingPathDelimiter(AProjectDir);

  { Compute relative path }
  if (AProjectDir <> '') and (Pos(ProjDir, AFilePath) = 1) then
    RelDir := ExtractFilePath(Copy(AFilePath, Length(ProjDir) + 1, MaxInt))
  else
    RelDir := ExtractFilePath(AFilePath);

  SFile := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
  try
    Size := SFile.Size;
    SetLength(SourceText, Size);
    if Size > 0 then
      SFile.Read(SourceText[1], Size);
  finally
    SFile.Free;
  end;

  StartIdx := Length(ASymbols);
  try
    ScanSourceForSymbols(SourceText, FName, ASymbols);
  except
    { If scanning fails, skip this file gracefully }
    SetLength(ASymbols, StartIdx);
    Exit;
  end;

  { Fill in file path details for the newly added symbols }
  for i := StartIdx to High(ASymbols) do
  begin
    ASymbols[i].FileName := FName;
    ASymbols[i].RelativePath := RelDir;
    ASymbols[i].FullPath := AFilePath;
  end;
end;

procedure CollectProjectSymbols(const AFiles: TFileEntryArray;
  var ASymbols: TSymbolEntryArray);
var
  i: Integer;
  Ext: string;
begin
  for i := 0 to High(AFiles) do
  begin
    Ext := LowerCase(ExtractFileExt(AFiles[i].FileName));
    { Only scan Pascal source files, not includes }
    if (Ext = '.pas') or (Ext = '.pp') or (Ext = '.lpr') or (Ext = '.dpr') then
      ScanFileForSymbols(AFiles[i].FullPath,
        ExtractFilePath(AFiles[i].FullPath), ASymbols);
  end;
end;

procedure SortFilteredSymbols(var A: TFilteredSymbolArray; L, R: Integer);
var
  I, J: Integer;
  Pivot: TFilteredSymbolEntry;
  Tmp: TFilteredSymbolEntry;
begin
  if L >= R then
    Exit;
  I := L;
  J := R;
  Pivot := A[(L + R) div 2];
  repeat
    while (A[I].Score > Pivot.Score) or
          ((A[I].Score = Pivot.Score) and (A[I].Entry.Name < Pivot.Entry.Name)) do
      Inc(I);
    while (A[J].Score < Pivot.Score) or
          ((A[J].Score = Pivot.Score) and (A[J].Entry.Name > Pivot.Entry.Name)) do
      Dec(J);
    if I <= J then
    begin
      Tmp := A[I];
      A[I] := A[J];
      A[J] := Tmp;
      Inc(I);
      Dec(J);
    end;
  until I > J;
  if L < J then
    SortFilteredSymbols(A, L, J);
  if I < R then
    SortFilteredSymbols(A, I, R);
end;

function FilterSymbolsEx(const APattern: string;
  const ASymbols: TSymbolEntryArray): TFilteredSymbolArray;
var
  Count, i, s: Integer;
  Positions: TMatchRangeArray;
begin
  SetLength(Result, 0);
  if Length(ASymbols) = 0 then
    Exit;

  SetLength(Result, Length(ASymbols));
  Count := 0;

  for i := 0 to High(ASymbols) do
  begin
    if FuzzyMatchEx(APattern, ASymbols[i].Name, s, Positions) then
    begin
      Result[Count].Entry := ASymbols[i];
      Result[Count].Score := s;
      Result[Count].MatchRanges := Positions;
      Inc(Count);
    end;
  end;

  if Count = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  SetLength(Result, Count);
  SortFilteredSymbols(Result, 0, Count - 1);
end;

end.
