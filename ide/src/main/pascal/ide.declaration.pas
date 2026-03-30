{
    fpGUI IDE - Go to Declaration

    Copyright (C) 2006 - 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Provides "Go to Declaration" functionality using FPC's fcl-passrc
      package (TPasParser + TPasResolver) for scope-aware identifier
      resolution. Ctrl+B on an identifier navigates to where it is
      declared.
}
unit ide.declaration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pscanner, pparser, pastree, pasresolver, ide.highlighter;

type
  TDeclarationResult = record
    Found: Boolean;
    DeclFile: string;     { absolute path to declaration's source file }
    DeclLine: Integer;    { 1-based line number (as stored by TPasElement) }
    DeclName: string;     { name of the resolved declaration }
  end;

  { TDeclarationFileResolver }

  TDeclarationFileResolver = class(TStreamResolver)
  private
    FUnitPaths: TStrings;        { borrowed, not owned }
    FIncludePaths: TStrings;     { borrowed, not owned }
    function SearchPaths(APaths: TStrings; const AName: string): string;
  public
    function FindSourceFile(const AName: string): TLineReader; override;
    function FindIncludeFile(const AName: string): TLineReader; override;
    property UnitPaths: TStrings read FUnitPaths write FUnitPaths;
    property IncludePaths: TStrings read FIncludePaths write FIncludePaths;
  end;

  { TDeclarationEngine }

  TDeclarationEngine = class(TPasResolver)
  private
    FUnitPaths: TStrings;        { borrowed }
    FIncludePaths: TStrings;     { borrowed }
    FParsing: TStringList;       { tracks units currently being parsed -- circular use guard }
  public
    constructor Create;
    destructor Destroy; override;
    function FindUnit(const AName, InFilename: String;
      NameExpr, InFileExpr: TPasExpr): TPasModule; override;
    property UnitPaths: TStrings read FUnitPaths write FUnitPaths;
    property IncludePaths: TStrings read FIncludePaths write FIncludePaths;
  end;

{ ALine and ACol are both 0-based (matching editor CaretPos_V / CaretPos_H) }
function GetIdentifierAtCursor(AHighlighter: TPascalHighlighter;
  ALines: TStrings; ALine, ACol: Integer): string;


implementation

{ TDeclarationFileResolver }

function TDeclarationFileResolver.SearchPaths(APaths: TStrings;
  const AName: string): string;
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
    { Try original case first }
    candidate := dir + AName;
    if FileExists(candidate) then
      Exit(candidate);
    { Try lowercase for Linux case-sensitivity }
    candidate := dir + LowerCase(AName);
    if FileExists(candidate) then
      Exit(candidate);
  end;
end;

function TDeclarationFileResolver.FindSourceFile(const AName: string): TLineReader;
var
  filePath: string;
  fs: TFileStream;
  ss: TStringStream;
begin
  { Check registered streams first (the override buffer) }
  Result := inherited FindSourceFile(AName);
  if Result <> nil then
    Exit;
  { Search unit paths for .pas / .pp files }
  filePath := SearchPaths(FUnitPaths, AName + '.pas');
  if filePath = '' then
    filePath := SearchPaths(FUnitPaths, AName + '.pp');
  if filePath = '' then
    Exit(nil);
  { Read file into a stream, register it, and return a reader }
  ss := TStringStream.Create('');
  try
    fs := TFileStream.Create(filePath, fmOpenRead or fmShareDenyNone);
    try
      ss.CopyFrom(fs, 0);
    finally
      fs.Free;
    end;
  except
    ss.Free;
    Exit(nil);
  end;
  ss.Position := 0;
  AddStream(filePath, ss);
  Result := TStringStreamLineReader.Create(filePath, ss.DataString);
end;

function TDeclarationFileResolver.FindIncludeFile(const AName: string): TLineReader;
var
  filePath: string;
begin
  { Search include paths }
  filePath := SearchPaths(FIncludePaths, AName);
  if filePath <> '' then
    Result := TFileLineReader.Create(filePath)
  else
    { Graceful degradation -- return empty reader }
    Result := TStringStreamLineReader.Create(AName, '');
end;


{ TDeclarationEngine }

constructor TDeclarationEngine.Create;
begin
  inherited Create;
  FParsing := TStringList.Create;
  FParsing.Sorted := True;
end;

destructor TDeclarationEngine.Destroy;
begin
  FParsing.Free;
  inherited Destroy;
end;

function TDeclarationEngine.FindUnit(const AName, InFilename: String;
  NameExpr, InFileExpr: TPasExpr): TPasModule;
var
  resolver: TDeclarationFileResolver;
  scanner: TPascalScanner;
  parser: TPasParser;
  filePath: string;
  fs: TFileStream;
  ss: TStringStream;
begin
  Result := nil;

  { Circular use guard }
  if FParsing.IndexOf(UpperCase(AName)) >= 0 then
    Exit;

  { Search unit paths for source file }
  filePath := '';
  if FUnitPaths <> nil then
  begin
    resolver := TDeclarationFileResolver.Create;
    try
      filePath := resolver.SearchPaths(FUnitPaths, AName + '.pas');
      if filePath = '' then
        filePath := resolver.SearchPaths(FUnitPaths, AName + '.pp');
    finally
      resolver.Free;
    end;
  end;

  if filePath = '' then
    Exit;

  { Sub-parse the unit }
  FParsing.Add(UpperCase(AName));
  resolver := TDeclarationFileResolver.Create;
  try
    resolver.OwnsStreams := True;
    resolver.UnitPaths := FUnitPaths;
    resolver.IncludePaths := FIncludePaths;

    { Load file content into a stream for the resolver }
    ss := TStringStream.Create('');
    fs := TFileStream.Create(filePath, fmOpenRead or fmShareDenyNone);
    try
      ss.CopyFrom(fs, 0);
    finally
      fs.Free;
    end;
    ss.Position := 0;
    resolver.AddStream(filePath, ss);

    scanner := TPascalScanner.Create(resolver);
    try
      scanner.OpenFile(filePath);
      parser := TPasParser.Create(scanner, resolver, Self);
      try
        parser.ImplicitUses.Clear;
        try
          parser.ParseMain(Result);
        except
          { Prevent secondary parse failures from corrupting the main scope stack }
          Result := nil;
        end;
      finally
        parser.Free;
      end;
    finally
      scanner.Free;
    end;
  finally
    resolver.Free;
    FParsing.Delete(FParsing.IndexOf(UpperCase(AName)));
  end;
end;


{ Helper: extract text for a token from the source line }
function TokenText(ALines: TStrings; ALineIdx: Integer;
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
        Result := TokenText(ALines, ALine, tok);
      Exit;
    end;
  end;
end;

end.
