{
    fpGUI IDE - File Finder

    Copyright (C) 2026 by Graeme Geldenhuys

    Fuzzy file matching and project file collection for the
    Navigate to File dialog (Ctrl+Shift+N).

    Pure logic unit — no GUI dependencies.
}
unit ide.filefinder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TFileEntry = record
    FullPath: string;       // absolute path on disk
    RelativePath: string;   // directory relative to project root (for display)
    FileName: string;       // filename only (for matching)
  end;
  TFileEntryArray = array of TFileEntry;

{ Fuzzy match APattern against AText. Returns True if matched, with
  AScore indicating match quality (higher = better). Matching tiers:
  1. Exact match (case-insensitive)
  2. Prefix match
  3. Substring match
  4. Word-start / CamelCase abbreviation
  5. Subsequence }
function FuzzyMatch(const APattern, AText: string; out AScore: Integer): Boolean;

type
  TMatchRange = record
    Start: Integer;   // 1-based character position in filename
    Length: Integer;   // number of characters
  end;
  TMatchRangeArray = array of TMatchRange;

  TFilteredFileEntry = record
    Entry: TFileEntry;
    Score: Integer;
    MatchRanges: TMatchRangeArray;  // which characters matched in FileName
  end;
  TFilteredFileArray = array of TFilteredFileEntry;

{ Fuzzy match with match position tracking. AMatchPositions contains
  the 1-based indices of matched characters in AText. }
function FuzzyMatchEx(const APattern, AText: string; out AScore: Integer;
  out AMatchPositions: TMatchRangeArray): Boolean;

{ Filter AFiles by APattern using FuzzyMatch on FileName field.
  Returns matches sorted by score descending, then alphabetically. }
function FilterFiles(const APattern: string; const AFiles: TFileEntryArray): TFileEntryArray;

{ Filter with match position info for highlight rendering. }
function FilterFilesEx(const APattern: string; const AFiles: TFileEntryArray): TFilteredFileArray;

{ Recursively collect source files from ADir into AFiles.
  AProjectDir is used to compute RelativePath.
  AExtensions lists allowed extensions (e.g. '.pas', '.inc').
  Pass nil for all files. }
procedure CollectSourceFiles(const ADir, AProjectDir: string;
  AExtensions: TStringList; var AFiles: TFileEntryArray);


implementation

uses
  fpg_utils;

const
  ScoreExact      = 1000;
  ScorePrefix     = 800;
  ScoreSubstring  = 400;
  ScoreWordStart  = 600;
  ScoreSubseq     = 200;
  { Bonus for shorter filenames (closer match) }
  LengthBonusMax  = 100;

function IsWordBoundary(const AText: string; AIndex: Integer): Boolean;
var
  c, prev: Char;
begin
  if AIndex <= 1 then
  begin
    Result := True;
    Exit;
  end;
  c := AText[AIndex];
  prev := AText[AIndex - 1];
  { Word starts after '.', '_', '-' or at CamelCase boundary }
  Result := (prev = '.') or (prev = '_') or (prev = '-')
    or ((prev >= 'a') and (prev <= 'z') and (c >= 'A') and (c <= 'Z'));
end;

function TryWordStartMatch(const APatternLower, ATextLower: string): Boolean;
var
  pi, ti: Integer;
begin
  Result := False;
  pi := 1;
  ti := 1;
  while (pi <= Length(APatternLower)) and (ti <= Length(ATextLower)) do
  begin
    if (APatternLower[pi] = ATextLower[ti]) and IsWordBoundary(ATextLower, ti) then
    begin
      Inc(pi);
      Inc(ti);
    end
    else
      Inc(ti);
  end;
  Result := pi > Length(APatternLower);
end;

function TryWordStartMatchEx(const APatternLower, ATextLower: string;
  out APositions: TMatchRangeArray): Boolean;
var
  pi, ti, Count: Integer;
begin
  Result := False;
  SetLength(APositions, Length(APatternLower));
  Count := 0;
  pi := 1;
  ti := 1;
  while (pi <= Length(APatternLower)) and (ti <= Length(ATextLower)) do
  begin
    if (APatternLower[pi] = ATextLower[ti]) and IsWordBoundary(ATextLower, ti) then
    begin
      APositions[Count].Start := ti;
      APositions[Count].Length := 1;
      Inc(Count);
      Inc(pi);
      Inc(ti);
    end
    else
      Inc(ti);
  end;
  Result := pi > Length(APatternLower);
  if Result then
    SetLength(APositions, Count)
  else
    SetLength(APositions, 0);
end;

function TrySubsequenceMatch(const APatternLower, ATextLower: string): Boolean;
var
  pi, ti: Integer;
begin
  pi := 1;
  ti := 1;
  while (pi <= Length(APatternLower)) and (ti <= Length(ATextLower)) do
  begin
    if APatternLower[pi] = ATextLower[ti] then
      Inc(pi);
    Inc(ti);
  end;
  Result := pi > Length(APatternLower);
end;

function TrySubsequenceMatchEx(const APatternLower, ATextLower: string;
  out APositions: TMatchRangeArray): Boolean;
var
  pi, ti, Count: Integer;
begin
  SetLength(APositions, Length(APatternLower));
  Count := 0;
  pi := 1;
  ti := 1;
  while (pi <= Length(APatternLower)) and (ti <= Length(ATextLower)) do
  begin
    if APatternLower[pi] = ATextLower[ti] then
    begin
      APositions[Count].Start := ti;
      APositions[Count].Length := 1;
      Inc(Count);
      Inc(pi);
    end;
    Inc(ti);
  end;
  Result := pi > Length(APatternLower);
  if Result then
    SetLength(APositions, Count)
  else
    SetLength(APositions, 0);
end;

function LengthBonus(const AText: string): Integer;
begin
  { Shorter filenames get a higher bonus, capped at LengthBonusMax }
  if Length(AText) <= 8 then
    Result := LengthBonusMax
  else if Length(AText) >= 50 then
    Result := 0
  else
    Result := LengthBonusMax - ((Length(AText) - 8) * LengthBonusMax div 42);
end;

function FuzzyMatch(const APattern, AText: string; out AScore: Integer): Boolean;
var
  PatLower, TxtLower: string;
  Pos: Integer;
begin
  AScore := 0;

  { Empty pattern matches everything }
  if Length(APattern) = 0 then
  begin
    Result := True;
    AScore := 1;
    Exit;
  end;

  PatLower := LowerCase(APattern);
  TxtLower := LowerCase(AText);

  { Tier 1: Exact match }
  if PatLower = TxtLower then
  begin
    AScore := ScoreExact + LengthBonus(AText);
    Result := True;
    Exit;
  end;

  { Tier 2: Prefix match }
  if (Length(PatLower) <= Length(TxtLower)) and
     (Copy(TxtLower, 1, Length(PatLower)) = PatLower) then
  begin
    AScore := ScorePrefix + LengthBonus(AText);
    Result := True;
    Exit;
  end;

  { Tier 3: Word-start / CamelCase abbreviation }
  if TryWordStartMatch(PatLower, TxtLower) then
  begin
    AScore := ScoreWordStart + LengthBonus(AText);
    Result := True;
    Exit;
  end;

  { Tier 4: Substring match }
  Pos := System.Pos(PatLower, TxtLower);
  if Pos > 0 then
  begin
    AScore := ScoreSubstring + LengthBonus(AText);
    Result := True;
    Exit;
  end;

  { Tier 5: Subsequence match }
  if TrySubsequenceMatch(PatLower, TxtLower) then
  begin
    AScore := ScoreSubseq + LengthBonus(AText);
    Result := True;
    Exit;
  end;

  Result := False;
end;

type
  TScoredEntry = record
    Entry: TFileEntry;
    Score: Integer;
  end;
  TScoredEntryArray = array of TScoredEntry;

procedure SortScoredEntries(var A: TScoredEntryArray; L, R: Integer);
var
  I, J: Integer;
  Pivot: TScoredEntry;
  Tmp: TScoredEntry;
begin
  if L >= R then
    Exit;
  I := L;
  J := R;
  Pivot := A[(L + R) div 2];
  repeat
    while (A[I].Score > Pivot.Score) or
          ((A[I].Score = Pivot.Score) and (A[I].Entry.FileName < Pivot.Entry.FileName)) do
      Inc(I);
    while (A[J].Score < Pivot.Score) or
          ((A[J].Score = Pivot.Score) and (A[J].Entry.FileName > Pivot.Entry.FileName)) do
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
    SortScoredEntries(A, L, J);
  if I < R then
    SortScoredEntries(A, I, R);
end;

function FilterFiles(const APattern: string; const AFiles: TFileEntryArray): TFileEntryArray;
var
  Scored: TScoredEntryArray;
  Count, i, s: Integer;
begin
  SetLength(Result, 0);
  if Length(AFiles) = 0 then
    Exit;

  SetLength(Scored, Length(AFiles));
  Count := 0;

  for i := 0 to High(AFiles) do
  begin
    if FuzzyMatch(APattern, AFiles[i].FileName, s) then
    begin
      Scored[Count].Entry := AFiles[i];
      Scored[Count].Score := s;
      Inc(Count);
    end;
  end;

  if Count = 0 then
    Exit;

  SetLength(Scored, Count);
  SortScoredEntries(Scored, 0, Count - 1);

  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := Scored[i].Entry;
end;

function FuzzyMatchEx(const APattern, AText: string; out AScore: Integer;
  out AMatchPositions: TMatchRangeArray): Boolean;
var
  PatLower, TxtLower: string;
  Pos: Integer;
begin
  AScore := 0;
  SetLength(AMatchPositions, 0);

  if Length(APattern) = 0 then
  begin
    Result := True;
    AScore := 1;
    Exit;
  end;

  PatLower := LowerCase(APattern);
  TxtLower := LowerCase(AText);

  { Tier 1: Exact match }
  if PatLower = TxtLower then
  begin
    AScore := ScoreExact + LengthBonus(AText);
    SetLength(AMatchPositions, 1);
    AMatchPositions[0].Start := 1;
    AMatchPositions[0].Length := Length(AText);
    Result := True;
    Exit;
  end;

  { Tier 2: Prefix match }
  if (Length(PatLower) <= Length(TxtLower)) and
     (Copy(TxtLower, 1, Length(PatLower)) = PatLower) then
  begin
    AScore := ScorePrefix + LengthBonus(AText);
    SetLength(AMatchPositions, 1);
    AMatchPositions[0].Start := 1;
    AMatchPositions[0].Length := Length(APattern);
    Result := True;
    Exit;
  end;

  { Tier 3: Word-start / CamelCase abbreviation }
  if TryWordStartMatchEx(PatLower, TxtLower, AMatchPositions) then
  begin
    AScore := ScoreWordStart + LengthBonus(AText);
    Result := True;
    Exit;
  end;

  { Tier 4: Substring match }
  Pos := System.Pos(PatLower, TxtLower);
  if Pos > 0 then
  begin
    AScore := ScoreSubstring + LengthBonus(AText);
    SetLength(AMatchPositions, 1);
    AMatchPositions[0].Start := Pos;
    AMatchPositions[0].Length := Length(APattern);
    Result := True;
    Exit;
  end;

  { Tier 5: Subsequence match }
  if TrySubsequenceMatchEx(PatLower, TxtLower, AMatchPositions) then
  begin
    AScore := ScoreSubseq + LengthBonus(AText);
    Result := True;
    Exit;
  end;

  Result := False;
end;

procedure SortFilteredEntries(var A: TFilteredFileArray; L, R: Integer);
var
  I, J: Integer;
  Pivot: TFilteredFileEntry;
  Tmp: TFilteredFileEntry;
begin
  if L >= R then
    Exit;
  I := L;
  J := R;
  Pivot := A[(L + R) div 2];
  repeat
    while (A[I].Score > Pivot.Score) or
          ((A[I].Score = Pivot.Score) and (A[I].Entry.FileName < Pivot.Entry.FileName)) do
      Inc(I);
    while (A[J].Score < Pivot.Score) or
          ((A[J].Score = Pivot.Score) and (A[J].Entry.FileName > Pivot.Entry.FileName)) do
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
    SortFilteredEntries(A, L, J);
  if I < R then
    SortFilteredEntries(A, I, R);
end;

function FilterFilesEx(const APattern: string; const AFiles: TFileEntryArray): TFilteredFileArray;
var
  Count, i, s: Integer;
  Positions: TMatchRangeArray;
begin
  SetLength(Result, 0);
  if Length(AFiles) = 0 then
    Exit;

  SetLength(Result, Length(AFiles));
  Count := 0;

  for i := 0 to High(AFiles) do
  begin
    if FuzzyMatchEx(APattern, AFiles[i].FileName, s, Positions) then
    begin
      Result[Count].Entry := AFiles[i];
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
  SortFilteredEntries(Result, 0, Count - 1);
end;

function IsExcludedDir(const AName: string): Boolean;
begin
  Result := (AName = '.') or (AName = '..') or
            (AName = '.ide') or (AName = 'target') or
            (AName = 'units') or (AName = '.git');
end;

function MatchesExtension(const AName: string; AExtensions: TStringList): Boolean;
var
  Ext: string;
  j: Integer;
begin
  if (AExtensions = nil) or (AExtensions.Count = 0) then
  begin
    Result := True;
    Exit;
  end;
  Ext := LowerCase(ExtractFileExt(AName));
  for j := 0 to AExtensions.Count - 1 do
  begin
    if Ext = AExtensions[j] then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure CollectSourceFiles(const ADir, AProjectDir: string;
  AExtensions: TStringList; var AFiles: TFileEntryArray);
var
  sr: TSearchRec;
  FullDir: string;
  RelDir: string;
  Idx: Integer;
begin
  FullDir := IncludeTrailingPathDelimiter(ADir);

  { Compute relative path from project root }
  if (AProjectDir <> '') and (Pos(AProjectDir, FullDir) = 1) then
    RelDir := Copy(FullDir, Length(AProjectDir) + 1, MaxInt)
  else
    RelDir := FullDir;

  if fpgFindFirst(FullDir + AllFilesMask, faAnyFile, sr) = 0 then
  begin
    try
      repeat
        if (sr.Attr and faDirectory) <> 0 then
        begin
          if not IsExcludedDir(sr.Name) then
            CollectSourceFiles(FullDir + sr.Name, AProjectDir, AExtensions, AFiles);
        end
        else if MatchesExtension(sr.Name, AExtensions) then
        begin
          Idx := Length(AFiles);
          SetLength(AFiles, Idx + 1);
          AFiles[Idx].FileName := sr.Name;
          AFiles[Idx].RelativePath := RelDir;
          AFiles[Idx].FullPath := FullDir + sr.Name;
        end;
      until fpgFindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;
end;

end.
