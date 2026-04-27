{
    fpGUI IDE - File Finder Tests

    Tests for fuzzy file matching logic used by the
    Navigate to File dialog (Ctrl+Shift+N).
}
unit ide.test.filefinder;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  ide.filefinder;

type

  { TTestFuzzyMatch }

  TTestFuzzyMatch = class(TTestCase)
  published
    { Basic matching }
    procedure TestExactPrefixMatch;
    procedure TestSubstringMatch;
    procedure TestCamelCaseMatch;
    procedure TestCamelCaseMatchDotSeparated;
    procedure TestSubsequenceMatch;
    procedure TestNoMatch;
    procedure TestCaseInsensitive;
    procedure TestEmptyPatternMatchesAll;
    procedure TestExactMatchFullName;

    { Score ordering }
    procedure TestExactPrefixScoresHigherThanSubstring;
    procedure TestShorterMatchScoresHigher;
    procedure TestExactMatchScoresHighest;
  end;

  { TTestFuzzyMatchEx - match position tracking }

  TTestFuzzyMatchEx = class(TTestCase)
  published
    procedure TestExactMatchPositions;
    procedure TestPrefixMatchPositions;
    procedure TestSubstringMatchPositions;
    procedure TestWordStartMatchPositions;
    procedure TestSubsequenceMatchPositions;
    procedure TestNoMatchReturnsEmptyPositions;
  end;

  { TTestFilterFiles }

  TTestFilterFiles = class(TTestCase)
  private
    FFiles: TFileEntryArray;
    function MakeEntry(const AFileName, ARelPath: string): TFileEntry;
    procedure SetupTestFiles;
  protected
    procedure SetUp; override;
  published
    procedure TestFilterReturnsMatchesSortedByScore;
    procedure TestFilterEmptyPatternReturnsAll;
    procedure TestFilterNoMatchReturnsEmpty;
    procedure TestFilterCamelCaseOrdering;
  end;


implementation

{ TTestFuzzyMatch }

procedure TTestFuzzyMatch.TestExactPrefixMatch;
var
  Score: Integer;
begin
  CheckTrue(FuzzyMatch('ide', 'ide.form.main.pas', Score),
    'Should match prefix "ide"');
  CheckTrue(Score > 0, 'Score should be positive');
end;

procedure TTestFuzzyMatch.TestSubstringMatch;
var
  Score: Integer;
begin
  CheckTrue(FuzzyMatch('form', 'ide.form.main.pas', Score),
    'Should match substring "form"');
end;

procedure TTestFuzzyMatch.TestCamelCaseMatch;
var
  Score: Integer;
begin
  CheckTrue(FuzzyMatch('BT', 'BuilderThread.pas', Score),
    'Should match CamelCase "BT" in BuilderThread');
end;

procedure TTestFuzzyMatch.TestCamelCaseMatchDotSeparated;
var
  Score: Integer;
begin
  CheckTrue(FuzzyMatch('ifm', 'ide.form.main.pas', Score),
    'Should match word starts "ifm" in ide.form.main');
end;

procedure TTestFuzzyMatch.TestSubsequenceMatch;
var
  Score: Integer;
begin
  CheckTrue(FuzzyMatch('imn', 'ide.form.main.pas', Score),
    'Should match subsequence "imn" (i...m...n)');
end;

procedure TTestFuzzyMatch.TestNoMatch;
var
  Score: Integer;
begin
  CheckFalse(FuzzyMatch('xyz', 'ide.form.main.pas', Score),
    'Should not match "xyz"');
end;

procedure TTestFuzzyMatch.TestCaseInsensitive;
var
  Score: Integer;
begin
  CheckTrue(FuzzyMatch('IDE', 'ide.form.main.pas', Score),
    'Should match case-insensitively');
end;

procedure TTestFuzzyMatch.TestEmptyPatternMatchesAll;
var
  Score: Integer;
begin
  CheckTrue(FuzzyMatch('', 'ide.form.main.pas', Score),
    'Empty pattern should match everything');
end;

procedure TTestFuzzyMatch.TestExactMatchFullName;
var
  Score: Integer;
begin
  CheckTrue(FuzzyMatch('ide.form.main.pas', 'ide.form.main.pas', Score),
    'Exact full name should match');
end;

procedure TTestFuzzyMatch.TestExactPrefixScoresHigherThanSubstring;
var
  PrefixScore, SubstringScore: Integer;
begin
  FuzzyMatch('ide', 'ide.main.pas', PrefixScore);
  FuzzyMatch('ide', 'maxide.pas', SubstringScore);
  CheckTrue(PrefixScore > SubstringScore,
    Format('Prefix score (%d) should exceed substring score (%d)',
      [PrefixScore, SubstringScore]));
end;

procedure TTestFuzzyMatch.TestShorterMatchScoresHigher;
var
  ShortScore, LongScore: Integer;
begin
  FuzzyMatch('main', 'main.pas', ShortScore);
  FuzzyMatch('main', 'ide.form.main.pas', LongScore);
  CheckTrue(ShortScore > LongScore,
    Format('Shorter filename score (%d) should exceed longer (%d)',
      [ShortScore, LongScore]));
end;

procedure TTestFuzzyMatch.TestExactMatchScoresHighest;
var
  ExactScore, PrefixScore, SubstringScore: Integer;
begin
  FuzzyMatch('main.pas', 'main.pas', ExactScore);
  FuzzyMatch('main', 'main.pas', PrefixScore);
  FuzzyMatch('main', 'ide.form.main.pas', SubstringScore);
  CheckTrue(ExactScore > PrefixScore,
    'Exact match should score higher than prefix');
  CheckTrue(PrefixScore > SubstringScore,
    'Prefix should score higher than substring in longer name');
end;

{ TTestFilterFiles }

function TTestFilterFiles.MakeEntry(const AFileName, ARelPath: string): TFileEntry;
begin
  Result.FileName := AFileName;
  Result.RelativePath := ARelPath;
  Result.FullPath := '/project/' + ARelPath + AFileName;
end;

procedure TTestFilterFiles.SetupTestFiles;
begin
  SetLength(FFiles, 5);
  FFiles[0] := MakeEntry('ide.form.main.pas', 'src/main/pascal/');
  FFiles[1] := MakeEntry('ide.main.pas', 'src/main/pascal/');
  FFiles[2] := MakeEntry('ide.utils.pas', 'src/main/pascal/');
  FFiles[3] := MakeEntry('ide.filemonitor.pas', 'src/main/pascal/');
  FFiles[4] := MakeEntry('main.pas', 'src/main/pascal/');
end;

procedure TTestFilterFiles.SetUp;
begin
  SetupTestFiles;
end;

procedure TTestFilterFiles.TestFilterReturnsMatchesSortedByScore;
var
  Results: TFileEntryArray;
begin
  Results := FilterFiles('main', FFiles);
  CheckTrue(Length(Results) >= 2, 'Should match at least 2 files containing "main"');
  { "main.pas" should score highest (exact prefix on short name) }
  CheckEquals('main.pas', Results[0].FileName,
    'First result should be "main.pas" (best match)');
end;

procedure TTestFilterFiles.TestFilterEmptyPatternReturnsAll;
var
  Results: TFileEntryArray;
begin
  Results := FilterFiles('', FFiles);
  CheckEquals(Length(FFiles), Length(Results),
    'Empty pattern should return all files');
end;

procedure TTestFilterFiles.TestFilterNoMatchReturnsEmpty;
var
  Results: TFileEntryArray;
begin
  Results := FilterFiles('zzzzz', FFiles);
  CheckEquals(0, Length(Results), 'Non-matching pattern should return empty');
end;

procedure TTestFilterFiles.TestFilterCamelCaseOrdering;
var
  Results: TFileEntryArray;
begin
  Results := FilterFiles('ifm', FFiles);
  { "ide.form.main.pas" matches word starts i.f.m, "ide.filemonitor.pas" matches i.f.m too }
  CheckTrue(Length(Results) >= 1, 'Should match at least 1 file for "ifm"');
end;


{ TTestFuzzyMatchEx }

procedure TTestFuzzyMatchEx.TestExactMatchPositions;
var
  Score: Integer;
  Positions: TMatchRangeArray;
begin
  CheckTrue(FuzzyMatchEx('main.pas', 'main.pas', Score, Positions),
    'Exact match should succeed');
  CheckEquals(1, Length(Positions), 'Should have 1 range for exact match');
  CheckEquals(1, Positions[0].Start, 'Range should start at 1');
  CheckEquals(8, Positions[0].Length, 'Range should cover full filename');
end;

procedure TTestFuzzyMatchEx.TestPrefixMatchPositions;
var
  Score: Integer;
  Positions: TMatchRangeArray;
begin
  CheckTrue(FuzzyMatchEx('ide', 'ide.form.main.pas', Score, Positions),
    'Prefix match should succeed');
  CheckEquals(1, Length(Positions), 'Should have 1 range for prefix match');
  CheckEquals(1, Positions[0].Start, 'Range should start at 1');
  CheckEquals(3, Positions[0].Length, 'Range should cover pattern length');
end;

procedure TTestFuzzyMatchEx.TestSubstringMatchPositions;
var
  Score: Integer;
  Positions: TMatchRangeArray;
begin
  CheckTrue(FuzzyMatchEx('form', 'ide.form.main.pas', Score, Positions),
    'Substring match should succeed');
  CheckEquals(1, Length(Positions), 'Should have 1 range for substring match');
  CheckEquals(5, Positions[0].Start, 'Range should start at position of "form"');
  CheckEquals(4, Positions[0].Length, 'Range should cover pattern length');
end;

procedure TTestFuzzyMatchEx.TestWordStartMatchPositions;
var
  Score: Integer;
  Positions: TMatchRangeArray;
begin
  CheckTrue(FuzzyMatchEx('ifm', 'ide.form.main.pas', Score, Positions),
    'Word-start match should succeed');
  CheckTrue(Length(Positions) >= 3,
    Format('Should have at least 3 positions for "ifm", got %d', [Length(Positions)]));
  { Each position should be a single character at a word boundary }
  CheckEquals(1, Positions[0].Start, 'First match at "i" in "ide"');
  CheckEquals(1, Positions[0].Length, 'Single character match');
end;

procedure TTestFuzzyMatchEx.TestSubsequenceMatchPositions;
var
  Score: Integer;
  Positions: TMatchRangeArray;
begin
  CheckTrue(FuzzyMatchEx('imn', 'ide.form.main.pas', Score, Positions),
    'Subsequence match should succeed');
  CheckTrue(Length(Positions) >= 3,
    Format('Should have at least 3 positions for "imn", got %d', [Length(Positions)]));
  { Each matched character should be a single-char range }
  CheckEquals(1, Positions[0].Length, 'Each match should be 1 character');
  CheckEquals(1, Positions[1].Length, 'Each match should be 1 character');
  CheckEquals(1, Positions[2].Length, 'Each match should be 1 character');
end;

procedure TTestFuzzyMatchEx.TestNoMatchReturnsEmptyPositions;
var
  Score: Integer;
  Positions: TMatchRangeArray;
begin
  CheckFalse(FuzzyMatchEx('xyz', 'ide.form.main.pas', Score, Positions),
    'Should not match');
  CheckEquals(0, Length(Positions), 'No match should return empty positions');
end;


initialization
  RegisterTest(TTestFuzzyMatch);
  RegisterTest(TTestFuzzyMatchEx);
  RegisterTest(TTestFilterFiles);

end.
