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
    FSubEngines: TFPList;        { sub-engines kept alive until root cleanup }
    FSubObjects: TFPList;        { parsers/scanners/resolvers kept alive until root cleanup }
    FOwnsHub: Boolean;           { True for root engine, False for sub-engines }
    FOwnsParsing: Boolean;       { True for root engine, False for sub-engines }
    function SubParseUnit(const AName, AFilename, ASource: string): TPasModule;
  public
    constructor Create;
    constructor CreateSub(AHub: TPasResolverHub; AParsing: TStringList;
      ASubEngines: TFPList; ASubObjects: TFPList);
    destructor Destroy; override;
    function FindUnit(const AName, InFilename: String;
      NameExpr, InFileExpr: TPasExpr): TPasModule; override;
    procedure UsedInterfacesFinished(Section: TPasSection); override;
    property UnitPaths: TStrings read FUnitPaths write FUnitPaths;
    property IncludePaths: TStrings read FIncludePaths write FIncludePaths;
  end;

{ ALine and ACol are both 0-based (matching editor CaretPos_V / CaretPos_H) }
function GetIdentifierAtCursor(AHighlighter: TPascalHighlighter;
  ALines: TStrings; ALine, ACol: Integer): string;

{ ALine and ACol are 0-based (matching editor CaretPos_V / CaretPos_H).
  Converted to 1-based internally for TPasElement.SourceLinenumber. }
function FindDeclaration(AHighlighter: TPascalHighlighter;
  ALines: TStrings; const AFilename: string;
  ALine, ACol: Integer;
  AUnitPaths, AIncludePaths: TStrings): TDeclarationResult;


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
  Hub := TPasResolverHub.Create(Self);
  FOwnsHub := True;
  FParsing := TStringList.Create;
  FParsing.Sorted := True;
  FOwnsParsing := True;
  FSubEngines := TFPList.Create;
  FSubObjects := TFPList.Create;
end;

constructor TDeclarationEngine.CreateSub(AHub: TPasResolverHub;
  AParsing: TStringList; ASubEngines: TFPList; ASubObjects: TFPList);
begin
  inherited Create;
  Hub := AHub;
  FOwnsHub := False;
  FParsing := AParsing;
  FOwnsParsing := False;
  FSubEngines := ASubEngines;  { shared with root }
  FSubObjects := ASubObjects;  { shared with root }
end;

destructor TDeclarationEngine.Destroy;
var
  h: TPasResolverHub;
  i: Integer;
begin
  if FOwnsParsing then
  begin
    { Free sub-engines first (they may reference shared state) }
    for i := 0 to FSubEngines.Count - 1 do
      TObject(FSubEngines[i]).Free;
    FSubEngines.Free;
    { Free parsers, scanners, resolvers kept alive for AST integrity }
    for i := 0 to FSubObjects.Count - 1 do
      TObject(FSubObjects[i]).Free;
    FSubObjects.Free;
    FParsing.Free;
  end;
  { Save and detach Hub before inherited Destroy calls Clear }
  h := Hub;
  inherited Destroy;
  if FOwnsHub then
    h.Free;
end;

function TDeclarationEngine.SubParseUnit(const AName, AFilename,
  ASource: string): TPasModule;
var
  subResolver: TDeclarationFileResolver;
  subScanner: TPascalScanner;
  subParser: TPasParser;
  subEngine: TDeclarationEngine;
begin
  Result := nil;
  FParsing.Add(UpperCase(AName));
  try
    subResolver := TDeclarationFileResolver.Create;
    subResolver.OwnsStreams := True;
    subResolver.UnitPaths := FUnitPaths;
    subResolver.IncludePaths := FIncludePaths;
    subResolver.AddStream(AFilename, TStringStream.Create(ASource));

    subScanner := TPascalScanner.Create(subResolver);
    subScanner.OpenFile(AFilename);

    { Each unit needs its own engine instance, sharing Hub and parsing guard.
      Sub-engines are kept alive until root engine cleanup (AST nodes are
      owned by the engine that created them). }
    subEngine := TDeclarationEngine.CreateSub(Hub, FParsing, FSubEngines, FSubObjects);
    subEngine.UnitPaths := FUnitPaths;
    subEngine.IncludePaths := FIncludePaths;
    subEngine.AddObjFPCBuiltInIdentifiers;
    subEngine.AddBaseType('Integer', btLongint);
    subEngine.AddBaseType('Cardinal', btLongWord);
    subEngine.AddBaseType('SizeInt', {$ifdef HasInt64}btInt64{$else}btIntDouble{$endif});
    FSubEngines.Add(subEngine);

    subParser := TPasParser.Create(subScanner, subResolver, subEngine);
    subParser.ImplicitUses.Clear;

    { Keep parser, scanner, and resolver alive until root engine cleanup.
      TPasParser.Destroy calls Engine.CurrentParser := nil which triggers
      TPasResolver.Clear, wiping all CustomData from the AST.  We need
      the scopes intact for the main engine's cross-unit resolution. }
    FSubObjects.Add(subParser);
    FSubObjects.Add(subScanner);
    FSubObjects.Add(subResolver);

    try
      subParser.NextToken;
      subParser.ParseUnit(Result);
    except
      on E: Exception do
        Result := nil;
    end;
  finally
    FParsing.Delete(FParsing.IndexOf(UpperCase(AName)));
  end;
end;

procedure TDeclarationEngine.UsedInterfacesFinished(Section: TPasSection);
begin
  { Sub-engines: do not parse recursively -- using queue-based approach.
    Root engine: let inherited handle continuation. }
  if not FOwnsHub then
    Exit;
  inherited UsedInterfacesFinished(Section);
end;

function TDeclarationEngine.FindUnit(const AName, InFilename: String;
  NameExpr, InFileExpr: TPasExpr): TPasModule;
var
  filePath: string;
  fs: TFileStream;
  ss: TStringStream;
  searchResolver: TDeclarationFileResolver;
begin
  Result := nil;

  { Circular use guard }
  if FParsing.IndexOf(UpperCase(AName)) >= 0 then
    Exit;

  { Search unit paths for source file }
  filePath := '';
  if FUnitPaths <> nil then
  begin
    searchResolver := TDeclarationFileResolver.Create;
    try
      filePath := searchResolver.SearchPaths(FUnitPaths, AName + '.pas');
      if filePath = '' then
        filePath := searchResolver.SearchPaths(FUnitPaths, AName + '.pp');
    finally
      searchResolver.Free;
    end;
  end;

  if filePath = '' then
    Exit;

  { Load file content }
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
    Exit;
  end;

  Result := SubParseUnit(AName, filePath, ss.DataString);
  ss.Free;
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


{ AST visitor types }

type
  TDeclSearchData = record
    TargetFile: string;      { filter: only match in this file }
    TargetLine: Integer;     { 1-based (matching TPasElement.SourceLinenumber) }
    TargetName: string;      { case-insensitive comparison }
    FoundDecl: TPasElement;  { output: the resolved declaration, or nil }
  end;
  PDeclSearchData = ^TDeclSearchData;

  { TDeclVisitor - helper for ForEachCall which requires an 'of object' callback }

  TDeclVisitor = class
  public
    procedure VisitElement(El: TPasElement; arg: Pointer);
  end;

procedure TDeclVisitor.VisitElement(El: TPasElement; arg: Pointer);
var
  Data: PDeclSearchData;
begin
  Data := PDeclSearchData(arg);
  { Already found -- skip }
  if Data^.FoundDecl <> nil then
    Exit;
  { Filter: only match in the target file }
  if El.SourceFilename <> Data^.TargetFile then
    Exit;
  { Must be an identifier expression }
  if not (El is TPrimitiveExpr) then
    Exit;
  if TPrimitiveExpr(El).Kind <> pekIdent then
    Exit;
  { Must be on the target line }
  if El.SourceLinenumber <> Data^.TargetLine then
    Exit;
  { Must match the target name (case-insensitive) }
  if CompareText(TPrimitiveExpr(El).Value, Data^.TargetName) <> 0 then
    Exit;
  { Must have a resolved reference with a declaration }
  if not (El.CustomData is TResolvedReference) then
    Exit;
  if TResolvedReference(El.CustomData).Declaration = nil then
    Exit;
  Data^.FoundDecl := TResolvedReference(El.CustomData).Declaration;
end;


function FindDeclaration(AHighlighter: TPascalHighlighter;
  ALines: TStrings; const AFilename: string;
  ALine, ACol: Integer;
  AUnitPaths, AIncludePaths: TStrings): TDeclarationResult;
var
  ident: string;
  resolver: TDeclarationFileResolver;
  scanner: TPascalScanner;
  parser: TPasParser;
  engine: TDeclarationEngine;
  module: TPasModule;
  sourceStream: TStringStream;
  searchData: TDeclSearchData;
  visitor: TDeclVisitor;
begin
  Result.Found := False;
  Result.DeclFile := '';
  Result.DeclLine := 0;
  Result.DeclName := '';

  { Step 1: Extract identifier at cursor }
  ident := GetIdentifierAtCursor(AHighlighter, ALines, ALine, ACol);
  if ident = '' then
    Exit;

  { Step 2: Create the parsing pipeline }
  module := nil;
  try
  resolver := TDeclarationFileResolver.Create;
  try
    resolver.OwnsStreams := True;
    resolver.UnitPaths := AUnitPaths;
    resolver.IncludePaths := AIncludePaths;

    { Register current source buffer }
    sourceStream := TStringStream.Create(ALines.Text);
    resolver.AddStream(AFilename, sourceStream);

    scanner := TPascalScanner.Create(resolver);
    try
      scanner.OpenFile(AFilename);

      engine := TDeclarationEngine.Create;
      try
        engine.UnitPaths := AUnitPaths;
        engine.IncludePaths := AIncludePaths;
        engine.AddObjFPCBuiltInIdentifiers;
        { Register common type aliases not in base types (normally in System unit) }
        engine.AddBaseType('Integer', btLongint);
        engine.AddBaseType('Cardinal', btLongWord);
        engine.AddBaseType('SizeInt', {$ifdef HasInt64}btInt64{$else}btIntDouble{$endif});

        parser := TPasParser.Create(scanner, resolver, engine);
        try
          parser.ImplicitUses.Clear;

          { Step 3: Parse }
          try
            parser.ParseMain(module);
          except
            on E: Exception do
              module := nil;
          end;

          if module = nil then
            Exit;

          { Step 4: Walk AST }
          searchData.TargetFile := AFilename;
          searchData.TargetLine := ALine + 1;  { convert 0-based to 1-based }
          searchData.TargetName := ident;
          searchData.FoundDecl := nil;

          visitor := TDeclVisitor.Create;
          try
            module.ForEachCall(@visitor.VisitElement, @searchData);
          finally
            visitor.Free;
          end;

          { Step 5: Extract result }
          if searchData.FoundDecl <> nil then
          begin
            Result.Found := True;
            Result.DeclFile := searchData.FoundDecl.SourceFilename;
            Result.DeclLine := searchData.FoundDecl.SourceLinenumber;
            Result.DeclName := searchData.FoundDecl.Name;
          end;

        finally
          parser.Free;
        end;
      finally
        engine.Free;
      end;
    finally
      scanner.Free;
    end;
  finally
    resolver.Free;
  end;
  except
    { Catch any cleanup exceptions }
  end;
end;

end.
