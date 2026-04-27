{
    fpGUI IDE - Find Usages

    Copyright (C) 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Scans project source files for all occurrences of an identifier
      using the lightweight Pascal tokeniser. Results are grouped by
      file for display in the Find Usages dialog.
}
unit ide.findusages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ide.filefinder;

type

  TUsageEntry = record
    FullPath: string;       // absolute path on disk
    RelativePath: string;   // relative dir from project root
    FileName: string;       // filename only
    Line: Integer;          // 1-based
    Column: Integer;        // 1-based
    LineText: string;       // trimmed source line for context
  end;
  TUsageEntryArray = array of TUsageEntry;

  TUsageGroup = record
    FullPath: string;       // absolute path of the file
    RelativePath: string;   // relative path for display
    FileName: string;       // filename only
    Entries: TUsageEntryArray;
  end;
  TUsageGroupArray = array of TUsageGroup;


{ Scan a single file for all identifier tokens matching AIdent
  (case-insensitive). AProjectDir is used to compute relative paths. }
procedure ScanFileForUsages(const AFilePath, AProjectDir, AIdent: string;
  var AResults: TUsageEntryArray);

{ Scan all files for usages of AIdent. Results grouped by file.
  Returns total number of usages found. }
function FindAllUsages(const AIdent: string;
  const AFiles: TFileEntryArray;
  var AGroups: TUsageGroupArray): Integer;


implementation

uses
  ide.pascal.tokeniser;


procedure ScanFileForUsages(const AFilePath, AProjectDir, AIdent: string;
  var AResults: TUsageEntryArray);
var
  Tokeniser: TFpgPascalTokeniser;
  Tok: TFpgPasToken;
  Source: string;
  Lines: TStringList;
  FS: TFileStream;
  IdentUpper: string;
  Idx: Integer;
  ProjDir: string;
  RelDir: string;
  FName: string;
begin
  SetLength(AResults, 0);
  if not FileExists(AFilePath) then
    Exit;

  IdentUpper := UpperCase(AIdent);
  FName := ExtractFileName(AFilePath);
  ProjDir := IncludeTrailingPathDelimiter(AProjectDir);

  { Compute relative path }
  if (AProjectDir <> '') and (Pos(ProjDir, AFilePath) = 1) then
    RelDir := ExtractFilePath(Copy(AFilePath, Length(ProjDir) + 1, MaxInt))
  else
    RelDir := ExtractFilePath(AFilePath);

  { Read file into string }
  FS := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Source, FS.Size);
    if FS.Size > 0 then
      FS.Read(Source[1], FS.Size);
  finally
    FS.Free;
  end;

  { Split into lines for context extraction }
  Lines := TStringList.Create;
  try
    Lines.Text := Source;

    Tokeniser := TFpgPascalTokeniser.Create;
    try
      Tokeniser.SetSource(Source);
      repeat
        Tok := Tokeniser.NextToken;
        if (Tok.Kind = fptkIdentifier) and
           (Tokeniser.TokenTextUpper = IdentUpper) then
        begin
          Idx := Length(AResults);
          SetLength(AResults, Idx + 1);
          AResults[Idx].FullPath := AFilePath;
          AResults[Idx].RelativePath := RelDir;
          AResults[Idx].FileName := FName;
          AResults[Idx].Line := Tok.Line;
          AResults[Idx].Column := Tok.Column;
          if (Tok.Line >= 1) and (Tok.Line <= Lines.Count) then
            AResults[Idx].LineText := Trim(Lines[Tok.Line - 1])
          else
            AResults[Idx].LineText := '';
        end;
      until Tok.Kind = fptkEOF;
    finally
      Tokeniser.Free;
    end;
  finally
    Lines.Free;
  end;
end;


function FindAllUsages(const AIdent: string;
  const AFiles: TFileEntryArray;
  var AGroups: TUsageGroupArray): Integer;
var
  i, GIdx: Integer;
  FileResults: TUsageEntryArray;
  ProjectDir: string;
begin
  Result := 0;
  SetLength(AGroups, 0);

  { Determine project directory from first file entry }
  if Length(AFiles) > 0 then
  begin
    { RelativePath is relative to project dir, so reconstruct it }
    if (AFiles[0].RelativePath <> '') and
       (Pos(AFiles[0].RelativePath, AFiles[0].FullPath) > 0) then
      ProjectDir := Copy(AFiles[0].FullPath, 1,
        Pos(AFiles[0].RelativePath, AFiles[0].FullPath) - 1)
    else
      ProjectDir := ExtractFilePath(AFiles[0].FullPath);
  end
  else
    ProjectDir := '';

  for i := 0 to High(AFiles) do
  begin
    SetLength(FileResults, 0);
    ScanFileForUsages(AFiles[i].FullPath, ProjectDir, AIdent, FileResults);
    if Length(FileResults) > 0 then
    begin
      GIdx := Length(AGroups);
      SetLength(AGroups, GIdx + 1);
      AGroups[GIdx].FullPath := AFiles[i].FullPath;
      AGroups[GIdx].RelativePath := AFiles[i].RelativePath;
      AGroups[GIdx].FileName := AFiles[i].FileName;
      AGroups[GIdx].Entries := FileResults;
      Result := Result + Length(FileResults);
    end;
  end;
end;

end.
