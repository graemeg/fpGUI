{
    fpGUI IDE - Highlight Renderer Tests

    Tests for BuildRenderSegments and related functions in
    ide.highlight.renderer. These tests verify that token-based rendering
    produces correct segment data (colours, positions, gap filling) without
    requiring a canvas or widget.
}
unit ide.test.highlight.renderer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  fpg_base,
  ide.highlighter,
  ide.editor.theme,
  ide.bracketmatch,
  ide.highlight.renderer;

type

  { TTestBuildRenderSegments }

  TTestBuildRenderSegments = class(TTestCase)
  private
    FHL: TPascalHighlighter;
    FTheme: TEditorTheme;
    FNoMatch: TBracketMatchResult;
    procedure SetupSource(const ASource: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyLine;
    procedure TestNilTokensDrawsPlain;
    procedure TestSingleKeyword;
    procedure TestGapBeforeToken;
    procedure TestMultipleTokens;
    procedure TestBracketMatchHighlight;
    procedure TestBracketMatchOnMatchLine;
    procedure TestNoBracketMatchWhenDisabled;
    procedure TestTokenStyleForegroundFallback;
    procedure TestTokenStyleBackgroundFallback;
  end;

  { TTestBuildPatchSegments }

  TTestBuildPatchSegments = class(TTestCase)
  private
    FTheme: TEditorTheme;
  protected
    procedure SetUp; override;
  published
    procedure TestAddedLine;
    procedure TestRemovedLine;
    procedure TestHunkHeader;
    procedure TestLeftFile;
    procedure TestRightFile;
    procedure TestStartOfFile;
    procedure TestPlainLine;
  end;

  { TTestHighlighterCache }

  TTestHighlighterCache = class(TTestCase)
  published
    procedure TestCreateDestroy;
    procedure TestInvalidateEditor;
  end;

implementation

{ TTestBuildRenderSegments }

procedure TTestBuildRenderSegments.SetupSource(const ASource: string);
begin
  FHL.Tokenise(ASource);
end;

procedure TTestBuildRenderSegments.SetUp;
begin
  FHL := TPascalHighlighter.Create;
  FTheme := DefaultTheme;
  FNoMatch.Found := False;
end;

procedure TTestBuildRenderSegments.TearDown;
begin
  FHL.Free;
end;

procedure TTestBuildRenderSegments.TestEmptyLine;
var
  segs: TRenderSegmentArray;
begin
  SetupSource('');
  segs := BuildRenderSegments(FHL, '', 0, FTheme, FNoMatch, False);
  AssertEquals('empty line should produce no segments', 0, Length(segs));
end;

procedure TTestBuildRenderSegments.TestNilTokensDrawsPlain;
var
  segs: TRenderSegmentArray;
begin
  { Line index beyond what was tokenised — GetLineTokens returns nil }
  SetupSource('program test;');
  segs := BuildRenderSegments(FHL, 'some text', 999, FTheme, FNoMatch, False);
  { Should return a single plain-text segment }
  AssertEquals('nil tokens should produce one plain segment', 1, Length(segs));
  AssertEquals('plain segment column', 0, segs[0].Column);
  AssertEquals('plain segment text', 'some text', segs[0].Text);
  AssertTrue('plain segment uses default foreground',
    segs[0].Foreground = FTheme.Chrome.Foreground);
  AssertTrue('plain segment uses default background',
    segs[0].Background = FTheme.Chrome.Background);
end;

procedure TTestBuildRenderSegments.TestSingleKeyword;
var
  segs: TRenderSegmentArray;
  i: Integer;
  foundKw: Boolean;
begin
  SetupSource('begin' + LineEnding + 'end.');
  segs := BuildRenderSegments(FHL, 'begin', 0, FTheme, FNoMatch, False);
  AssertTrue('should have at least one segment', Length(segs) > 0);
  { Find the keyword segment }
  foundKw := False;
  for i := 0 to High(segs) do
    if segs[i].Text = 'begin' then
    begin
      foundKw := True;
      AssertTrue('keyword should use keyword1 foreground',
        segs[i].Foreground = FTheme.TokenStyles[hcKeyword1].Foreground);
    end;
  AssertTrue('should contain a begin keyword segment', foundKw);
end;

procedure TTestBuildRenderSegments.TestGapBeforeToken;
var
  segs: TRenderSegmentArray;
begin
  { "  begin" — two spaces gap before keyword }
  SetupSource('  begin' + LineEnding + '  end.');
  segs := BuildRenderSegments(FHL, '  begin', 0, FTheme, FNoMatch, False);
  AssertTrue('should have segments', Length(segs) > 0);
  { First segment should be the gap (spaces) at column 0 }
  AssertEquals('first segment should start at column 0', 0, segs[0].Column);
  AssertEquals('gap text should be two spaces', '  ', segs[0].Text);
end;

procedure TTestBuildRenderSegments.TestMultipleTokens;
var
  segs: TRenderSegmentArray;
begin
  SetupSource('var x: integer;');
  segs := BuildRenderSegments(FHL, 'var x: integer;', 0, FTheme, FNoMatch, False);
  AssertTrue('should have multiple segments', Length(segs) > 2);
end;

procedure TTestBuildRenderSegments.TestBracketMatchHighlight;
var
  segs: TRenderSegmentArray;
  match: TBracketMatchResult;
  i: Integer;
  foundHighlight: Boolean;
begin
  SetupSource('(x)');
  { Simulate bracket match at source column 0, match column 2 }
  match.Found := True;
  match.SourceLine := 0;
  match.SourceCol := 0;
  match.SourceLength := 1;
  match.MatchLine := 0;
  match.MatchCol := 2;
  match.MatchLength := 1;
  segs := BuildRenderSegments(FHL, '(x)', 0, FTheme, match, True);
  { Find a segment at column 0 with bracket match background }
  foundHighlight := False;
  for i := 0 to High(segs) do
    if (segs[i].Column = 0) and (segs[i].Background = FTheme.Chrome.BracketMatch) then
      foundHighlight := True;
  AssertTrue('source bracket should have BracketMatch background', foundHighlight);
end;

procedure TTestBuildRenderSegments.TestBracketMatchOnMatchLine;
var
  segs: TRenderSegmentArray;
  match: TBracketMatchResult;
  i: Integer;
  foundHighlight: Boolean;
begin
  SetupSource('(x)');
  match.Found := True;
  match.SourceLine := 0;
  match.SourceCol := 0;
  match.SourceLength := 1;
  match.MatchLine := 0;
  match.MatchCol := 2;
  match.MatchLength := 1;
  segs := BuildRenderSegments(FHL, '(x)', 0, FTheme, match, True);
  { Find a segment at column 2 with bracket match background }
  foundHighlight := False;
  for i := 0 to High(segs) do
    if (segs[i].Column = 2) and (segs[i].Background = FTheme.Chrome.BracketMatch) then
      foundHighlight := True;
  AssertTrue('match bracket should have BracketMatch background', foundHighlight);
end;

procedure TTestBuildRenderSegments.TestNoBracketMatchWhenDisabled;
var
  segs: TRenderSegmentArray;
  match: TBracketMatchResult;
  i: Integer;
begin
  SetupSource('(x)');
  match.Found := True;
  match.SourceLine := 0;
  match.SourceCol := 0;
  match.SourceLength := 1;
  match.MatchLine := 0;
  match.MatchCol := 2;
  match.MatchLength := 1;
  { Pass AShowBracketMatch = False }
  segs := BuildRenderSegments(FHL, '(x)', 0, FTheme, match, False);
  for i := 0 to High(segs) do
    AssertFalse('no segment should have BracketMatch bg when disabled',
      segs[i].Background = FTheme.Chrome.BracketMatch);
end;

procedure TTestBuildRenderSegments.TestTokenStyleForegroundFallback;
var
  segs: TRenderSegmentArray;
  i: Integer;
begin
  { Whitespace category has clNone foreground — should fall back to Chrome.Foreground }
  FTheme.TokenStyles[hcWhitespace].Foreground := clNone;
  SetupSource('  begin' + LineEnding + '  end.');
  segs := BuildRenderSegments(FHL, '  begin', 0, FTheme, FNoMatch, False);
  { The gap/whitespace segment should use Chrome.Foreground }
  for i := 0 to High(segs) do
    AssertFalse('no segment should have clNone foreground',
      segs[i].Foreground = clNone);
end;

procedure TTestBuildRenderSegments.TestTokenStyleBackgroundFallback;
var
  segs: TRenderSegmentArray;
  i: Integer;
begin
  SetupSource('begin' + LineEnding + 'end.');
  segs := BuildRenderSegments(FHL, 'begin', 0, FTheme, FNoMatch, False);
  for i := 0 to High(segs) do
    AssertFalse('no segment should have clNone background',
      segs[i].Background = clNone);
end;

{ TTestBuildPatchSegments }

procedure TTestBuildPatchSegments.SetUp;
begin
  FTheme := DefaultTheme;
end;

procedure TTestBuildPatchSegments.TestAddedLine;
var
  segs: TRenderSegmentArray;
begin
  segs := BuildPatchRenderSegments('+added line', FTheme);
  AssertTrue('should produce segments', Length(segs) > 0);
  AssertTrue('added line should be green', segs[0].Foreground = clGreen);
end;

procedure TTestBuildPatchSegments.TestRemovedLine;
var
  segs: TRenderSegmentArray;
begin
  segs := BuildPatchRenderSegments('-removed line', FTheme);
  AssertTrue('should produce segments', Length(segs) > 0);
  AssertTrue('removed line should be red', segs[0].Foreground = clRed);
end;

procedure TTestBuildPatchSegments.TestHunkHeader;
var
  segs: TRenderSegmentArray;
begin
  segs := BuildPatchRenderSegments('@@ -1,5 +1,6 @@', FTheme);
  AssertTrue('should produce segments', Length(segs) > 0);
  AssertTrue('hunk header should be blue', segs[0].Foreground = clBlue);
end;

procedure TTestBuildPatchSegments.TestLeftFile;
var
  segs: TRenderSegmentArray;
begin
  segs := BuildPatchRenderSegments('--- a/file.pas', FTheme);
  AssertTrue('should produce segments', Length(segs) > 0);
  AssertTrue('left file should be magenta', segs[0].Foreground = clMagenta);
end;

procedure TTestBuildPatchSegments.TestRightFile;
var
  segs: TRenderSegmentArray;
begin
  segs := BuildPatchRenderSegments('+++ b/file.pas', FTheme);
  AssertTrue('should produce segments', Length(segs) > 0);
  AssertTrue('right file should be magenta', segs[0].Foreground = clMagenta);
end;

procedure TTestBuildPatchSegments.TestStartOfFile;
var
  segs: TRenderSegmentArray;
begin
  segs := BuildPatchRenderSegments('diff --git a/file.pas b/file.pas', FTheme);
  AssertTrue('should produce segments', Length(segs) > 0);
  AssertTrue('start-of-file background should be clSilver',
    segs[0].Background = clSilver);
end;

procedure TTestBuildPatchSegments.TestPlainLine;
var
  segs: TRenderSegmentArray;
begin
  segs := BuildPatchRenderSegments(' context line', FTheme);
  AssertTrue('should produce one segment', Length(segs) = 1);
  AssertTrue('plain line should use default foreground',
    segs[0].Foreground = FTheme.Chrome.Foreground);
end;

{ TTestHighlighterCache }

procedure TTestHighlighterCache.TestCreateDestroy;
var
  cache: THighlighterCache;
begin
  cache := THighlighterCache.Create;
  try
    AssertNotNull('Pascal highlighter should be created', cache.PascalHighlighter);
    AssertNotNull('INI highlighter should be created', cache.INIHighlighter);
    AssertNotNull('XML highlighter should be created', cache.XMLHighlighter);
  finally
    cache.Free;
  end;
end;

procedure TTestHighlighterCache.TestInvalidateEditor;
var
  cache: THighlighterCache;
begin
  cache := THighlighterCache.Create;
  try
    { Invalidate with nil should not crash }
    cache.InvalidateEditor(nil);
    { After invalidation, tracked editors should be nil }
    AssertNull('Pascal editor should be nil after invalidation',
      cache.PascalEditor);
    AssertNull('INI editor should be nil after invalidation',
      cache.INIEditor);
    AssertNull('XML editor should be nil after invalidation',
      cache.XMLEditor);
  finally
    cache.Free;
  end;
end;

initialization
  RegisterTest(TTestBuildRenderSegments);
  RegisterTest(TTestBuildPatchSegments);
  RegisterTest(TTestHighlighterCache);

end.
