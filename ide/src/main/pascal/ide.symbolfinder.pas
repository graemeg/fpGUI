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
  pscanner;

type
  { Custom resolver that returns empty content for include files }
  TSymbolListResolver = class(TStreamResolver)
  public
    function FindIncludeFile(const AName: string): TLineReader; override;
  end;

function TSymbolListResolver.FindIncludeFile(const AName: string): TLineReader;
begin
  Result := TStringStreamLineReader.Create(AName, '');
end;

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
  Scanner: TPascalScanner;
  Resolver: TSymbolListResolver;
  Token: TToken;
  IsUnit: Boolean;
  InInterface: Boolean;
  Section: (secNone, secType, secConst, secVar, secOther);
  TypeNestDepth: Integer;
  CurrentClassName: string;
  SymName: string;
  Line: Integer;
begin
  if Length(ASource) = 0 then
    Exit;

  Resolver := TSymbolListResolver.Create;
  try
    Resolver.OwnsStreams := True;
    Resolver.AddStream(AFileName, TStringStream.Create(ASource));

    Scanner := TPascalScanner.Create(Resolver);
    try
      Scanner.SkipWhiteSpace := True;
      Scanner.SkipComments := True;
      Scanner.OpenFile(AFileName);

      Token := Scanner.FetchToken;

      { Determine if this is a unit (has interface/implementation sections) }
      IsUnit := (Token = tkunit);
      if IsUnit then
      begin
        { Skip to interface keyword }
        InInterface := False;
        while Token <> tkEOF do
        begin
          if Token = tkinterface then
          begin
            InInterface := True;
            Token := Scanner.FetchToken;
            Break;
          end;
          Token := Scanner.FetchToken;
        end;
        if not InInterface then
          Exit;
      end;
      { For program/library files, scan the whole top-level }

      Section := secNone;
      TypeNestDepth := 0;
      CurrentClassName := '';

      while Token <> tkEOF do
      begin
        { Stop at implementation section for units }
        if IsUnit and (Token = tkimplementation) then
          Break;

        { Track section changes at the top level (not inside a type body) }
        if TypeNestDepth = 0 then
        begin
          case Token of
            tktype:
              begin
                Section := secType;
                Token := Scanner.FetchToken;
                Continue;
              end;
            tkconst:
              begin
                Section := secConst;
                Token := Scanner.FetchToken;
                Continue;
              end;
            tkvar:
              begin
                Section := secVar;
                Token := Scanner.FetchToken;
                Continue;
              end;
            tkprocedure, tkfunction, tkconstructor, tkdestructor:
              begin
                Section := secOther;
                { fall through to handle below }
              end;
          end;
        end;

        { Handle procedures/functions }
        if Token in [tkprocedure, tkfunction, tkconstructor, tkdestructor] then
        begin
          Line := Scanner.CurTokenPos.Row;
          case Token of
            tkprocedure:   Section := secOther;
            tkfunction:    Section := secOther;
            tkconstructor: Section := secOther;
            tkdestructor:  Section := secOther;
          end;

          { Remember the kind }
          case Token of
            tkprocedure:
              begin
                Token := Scanner.FetchToken;
                if Token = tkIdentifier then
                begin
                  SymName := Scanner.CurTokenString;
                  if (TypeNestDepth > 0) and (CurrentClassName <> '') then
                    AddSymbol(ASymbols, SymName, CurrentClassName + '.' + SymName,
                      AFileName, '', '', skProcedure, Line)
                  else
                    AddSymbol(ASymbols, SymName, SymName,
                      AFileName, '', '', skProcedure, Line);
                end;
                { Skip to semicolon }
                while (Token <> tkSemicolon) and (Token <> tkEOF) do
                  Token := Scanner.FetchToken;
                Token := Scanner.FetchToken;
                Continue;
              end;
            tkfunction:
              begin
                Token := Scanner.FetchToken;
                if Token = tkIdentifier then
                begin
                  SymName := Scanner.CurTokenString;
                  if (TypeNestDepth > 0) and (CurrentClassName <> '') then
                    AddSymbol(ASymbols, SymName, CurrentClassName + '.' + SymName,
                      AFileName, '', '', skFunction, Line)
                  else
                    AddSymbol(ASymbols, SymName, SymName,
                      AFileName, '', '', skFunction, Line);
                end;
                while (Token <> tkSemicolon) and (Token <> tkEOF) do
                  Token := Scanner.FetchToken;
                Token := Scanner.FetchToken;
                Continue;
              end;
            tkconstructor:
              begin
                Token := Scanner.FetchToken;
                if Token = tkIdentifier then
                begin
                  SymName := Scanner.CurTokenString;
                  if (TypeNestDepth > 0) and (CurrentClassName <> '') then
                    AddSymbol(ASymbols, SymName, CurrentClassName + '.' + SymName,
                      AFileName, '', '', skConstructor, Line)
                  else
                    AddSymbol(ASymbols, SymName, SymName,
                      AFileName, '', '', skConstructor, Line);
                end;
                while (Token <> tkSemicolon) and (Token <> tkEOF) do
                  Token := Scanner.FetchToken;
                Token := Scanner.FetchToken;
                Continue;
              end;
            tkdestructor:
              begin
                Token := Scanner.FetchToken;
                if Token = tkIdentifier then
                begin
                  SymName := Scanner.CurTokenString;
                  if (TypeNestDepth > 0) and (CurrentClassName <> '') then
                    AddSymbol(ASymbols, SymName, CurrentClassName + '.' + SymName,
                      AFileName, '', '', skDestructor, Line)
                  else
                    AddSymbol(ASymbols, SymName, SymName,
                      AFileName, '', '', skDestructor, Line);
                end;
                while (Token <> tkSemicolon) and (Token <> tkEOF) do
                  Token := Scanner.FetchToken;
                Token := Scanner.FetchToken;
                Continue;
              end;
          end;
        end;

        { Handle type declarations }
        if (Section = secType) and (TypeNestDepth = 0) and (Token = tkIdentifier) then
        begin
          SymName := Scanner.CurTokenString;
          Line := Scanner.CurTokenPos.Row;
          Token := Scanner.FetchToken;
          if Token = tkEqual then
          begin
            AddSymbol(ASymbols, SymName, SymName,
              AFileName, '', '', skType, Line);
            Token := Scanner.FetchToken;
            { Check if this starts a class/record/object body }
            if Token in [tkclass, tkrecord, tkobject] then
            begin
              CurrentClassName := SymName;
              TypeNestDepth := 1;
              Token := Scanner.FetchToken;
              { Check for forward declaration: class; }
              if Token = tkSemicolon then
              begin
                TypeNestDepth := 0;
                CurrentClassName := '';
                Token := Scanner.FetchToken;
              end;
              { Otherwise Token already holds the first token inside the body }
              Continue;
            end
            else
            begin
              { Simple type (enum, range, alias) — skip to semicolon }
              while (Token <> tkSemicolon) and (Token <> tkEOF) do
                Token := Scanner.FetchToken;
            end;
          end;
          Token := Scanner.FetchToken;
          Continue;
        end;

        { Track nesting inside class/record/object bodies }
        if TypeNestDepth > 0 then
        begin
          if Token in [tkrecord, tkclass, tkobject] then
          begin
            { Nested type — check for 'end' matching }
            Inc(TypeNestDepth);
          end
          else if Token = tkend then
          begin
            Dec(TypeNestDepth);
            if TypeNestDepth = 0 then
              CurrentClassName := '';
          end;
        end;

        { Handle const declarations }
        if (Section = secConst) and (TypeNestDepth = 0) and (Token = tkIdentifier) then
        begin
          SymName := Scanner.CurTokenString;
          Line := Scanner.CurTokenPos.Row;
          Token := Scanner.FetchToken;
          if (Token = tkEqual) or (Token = tkColon) then
          begin
            AddSymbol(ASymbols, SymName, SymName,
              AFileName, '', '', skConst, Line);
            { Skip to semicolon }
            while (Token <> tkSemicolon) and (Token <> tkEOF) do
              Token := Scanner.FetchToken;
          end;
          Token := Scanner.FetchToken;
          Continue;
        end;

        { Handle var declarations }
        if (Section = secVar) and (TypeNestDepth = 0) and (Token = tkIdentifier) then
        begin
          SymName := Scanner.CurTokenString;
          Line := Scanner.CurTokenPos.Row;
          Token := Scanner.FetchToken;
          if Token = tkColon then
          begin
            AddSymbol(ASymbols, SymName, SymName,
              AFileName, '', '', skVar, Line);
            { Skip to semicolon }
            while (Token <> tkSemicolon) and (Token <> tkEOF) do
              Token := Scanner.FetchToken;
          end;
          Token := Scanner.FetchToken;
          Continue;
        end;

        Token := Scanner.FetchToken;
      end;
    finally
      Scanner.Free;
    end;
  finally
    Resolver.Free;
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
