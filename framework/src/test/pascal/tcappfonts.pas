{
  Tests for application-local font folders: discovery of the default
  folders (next to the executable, inside a macOS bundle), recursive font
  file enumeration, and font cache precedence of app-local fonts.
}
unit tcappfonts;

{$mode objfpc}{$H+}

{ The font cache uses FreeType, which is not linked by default on Windows. }
{$IF not defined(WINDOWS) or defined(FORCE_FREETYPE)}
  {$DEFINE TEST_FONTCACHE}
{$IFEND}

interface

uses
  Classes,
  SysUtils,
  fpcunit, testutils, testregistry,
  fpg_base,
  fpg_utils
  {$IFDEF TEST_FONTCACHE}
  , fpg_fontcache
  {$ENDIF}
  ;

type
  { Base class that manages a scratch directory for each test }
  TAppFontsTestCase = class(TTestCase)
  private
    FTempDir: TfpgString;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function  FixtureFont: TfpgString;
    function  MakeDir(const ARelPath: TfpgString): TfpgString;
    procedure CopyFixtureTo(const ADir, AFileName: TfpgString);
    procedure WriteJunkFile(const AFileName: TfpgString);
    property  TempDir: TfpgString read FTempDir;
  end;


  TTestAppFontDirectories = class(TAppFontsTestCase)
  published
    procedure TestFontsFolderNextToExecutableIsCandidate;
    procedure TestPlainExecutableHasSingleCandidate;
    {$IFDEF DARWIN}
    procedure TestBundleResourcesFontsFolderIsFirstCandidate;
    {$ENDIF}
    procedure TestFindFontFilesRecursesAndFiltersByExtension;
    procedure TestFindFontFilesIgnoresMissingDirectory;
  end;


  {$IFDEF TEST_FONTCACHE}
  TTestFontCacheAppDirectories = class(TAppFontsTestCase)
  private
    FCache: TFontCacheList;
    FExeFontsDir: TfpgString;
    function  FindVeraMono(ACache: TFontCacheList): integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddFontDirectoryMakesFamilyFindable;
    procedure TestFirstAddedDirectoryWins;
    procedure TestAddedDirectoryTakesPrecedenceOverExistingEntries;
    procedure TestCorruptFontFileIsSkipped;
    procedure TestMissingDirectoryIsIgnored;
    procedure TestBuildFontCacheScansFontsFolderNextToExecutableFirst;
  end;
  {$ENDIF}


procedure RegisterTests;

implementation

const
  VeraMonoFamily = 'Bitstream Vera Sans Mono';

procedure DeleteTree(const ADir: TfpgString);
var
  sr: TSearchRec;
  lDir: TfpgString;
begin
  lDir := fpgAppendPathDelim(ADir);
  if fpgFindFirst(lDir + AllFilesMask, faAnyFile or $00000080, sr) = 0 then
  begin
    repeat
      if (sr.Name = '.') or (sr.Name = '..') or (sr.Name = '') then
        Continue;
      if (sr.Attr and faDirectory) <> 0 then
        DeleteTree(lDir + sr.Name)
      else
        fpgDeleteFile(lDir + sr.Name);
    until fpgFindNext(sr) <> 0;
    FindClose(sr);
  end;
  fpgRemoveDir(lDir);
end;

{ TAppFontsTestCase }

procedure TAppFontsTestCase.SetUp;
begin
  inherited SetUp;
  FTempDir := fpgAppendPathDelim(GetTempDir(False))
      + 'fpgui_appfonts_' + IntToStr(Random(MaxInt)) + PathDelim;
  fpgForceDirectories(FTempDir);
end;

procedure TAppFontsTestCase.TearDown;
begin
  DeleteTree(FTempDir);
  inherited TearDown;
end;

function TAppFontsTestCase.FixtureFont: TfpgString;
begin
  { pasbuild copies src/test/resources/ next to the test runner }
  Result := fpgExtractFilePath(ParamStr(0)) + 'fontfixtures' + PathDelim + 'VeraMono.ttf';
  if not fpgFileExists(Result) then
    Fail('Font fixture not found: ' + Result);
end;

function TAppFontsTestCase.MakeDir(const ARelPath: TfpgString): TfpgString;
begin
  Result := fpgAppendPathDelim(FTempDir + ARelPath);
  fpgForceDirectories(Result);
end;

procedure TAppFontsTestCase.CopyFixtureTo(const ADir, AFileName: TfpgString);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(FixtureFont);
    ms.SaveToFile(fpgAppendPathDelim(ADir) + AFileName);
  finally
    ms.Free;
  end;
end;

procedure TAppFontsTestCase.WriteJunkFile(const AFileName: TfpgString);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add('This is not a font file');
    sl.SaveToFile(AFileName);
  finally
    sl.Free;
  end;
end;

{ TTestAppFontDirectories }

procedure TTestAppFontDirectories.TestFontsFolderNextToExecutableIsCandidate;
var
  sl: TStringList;
  lExeDir: TfpgString;
begin
  lExeDir := MakeDir('myapp');
  sl := TStringList.Create;
  try
    fpgGetAppFontDirectories(lExeDir + 'myapp', sl);
    AssertTrue('fonts folder next to executable expected',
        sl.IndexOf(lExeDir + 'fonts' + PathDelim) >= 0);
  finally
    sl.Free;
  end;
end;

procedure TTestAppFontDirectories.TestPlainExecutableHasSingleCandidate;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fpgGetAppFontDirectories(MakeDir('myapp') + 'myapp', sl);
    AssertEquals('candidate count', 1, sl.Count);
  finally
    sl.Free;
  end;
end;

{$IFDEF DARWIN}
procedure TTestAppFontDirectories.TestBundleResourcesFontsFolderIsFirstCandidate;
var
  sl: TStringList;
  lBundle: TfpgString;
begin
  lBundle := MakeDir('My App.app');
  sl := TStringList.Create;
  try
    fpgGetAppFontDirectories(lBundle + 'Contents/MacOS/myapp', sl);
    AssertEquals('candidate count', 2, sl.Count);
    AssertEquals('bundle Resources/fonts first',
        lBundle + 'Contents/Resources/fonts/', sl[0]);
    AssertEquals('fonts next to executable second',
        lBundle + 'Contents/MacOS/fonts/', sl[1]);
  finally
    sl.Free;
  end;
end;
{$ENDIF}

procedure TTestAppFontDirectories.TestFindFontFilesRecursesAndFiltersByExtension;
var
  sl: TStringList;
  lDir: TfpgString;
  lSub: TfpgString;
begin
  lDir := MakeDir('fonts');
  lSub := MakeDir('fonts' + PathDelim + 'sub');
  CopyFixtureTo(lDir, 'a.ttf');
  CopyFixtureTo(lSub, 'b.OTF');
  WriteJunkFile(lDir + 'notes.txt');
  sl := TStringList.Create;
  try
    fpgFindFontFiles(lDir, sl);
    sl.Sort;
    AssertEquals('font file count', 2, sl.Count);
    AssertEquals('top-level file', lDir + 'a.ttf', sl[0]);
    AssertEquals('nested file', lSub + 'b.OTF', sl[1]);
  finally
    sl.Free;
  end;
end;

procedure TTestAppFontDirectories.TestFindFontFilesIgnoresMissingDirectory;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fpgFindFontFiles(TempDir + 'does-not-exist', sl);
    AssertEquals('font file count', 0, sl.Count);
  finally
    sl.Free;
  end;
end;

{$IFDEF TEST_FONTCACHE}
{ TTestFontCacheAppDirectories }

function TTestFontCacheAppDirectories.FindVeraMono(ACache: TFontCacheList): integer;
var
  fnt: TFontCacheItem;
begin
  fnt := TFontCacheItem.Create('');
  try
    fnt.FamilyName := VeraMonoFamily;
    Result := ACache.Find(fnt);
  finally
    fnt.Free;
  end;
end;

procedure TTestFontCacheAppDirectories.SetUp;
begin
  inherited SetUp;
  FCache := TFontCacheList.Create;
  FExeFontsDir := '';
end;

procedure TTestFontCacheAppDirectories.TearDown;
begin
  FCache.Free;
  if FExeFontsDir <> '' then
    DeleteTree(FExeFontsDir);
  inherited TearDown;
end;

procedure TTestFontCacheAppDirectories.TestAddFontDirectoryMakesFamilyFindable;
var
  lDir: TfpgString;
  i: integer;
begin
  lDir := MakeDir('a');
  CopyFixtureTo(lDir, 'VeraMono.ttf');
  FCache.AddFontDirectory(lDir);
  i := FindVeraMono(FCache);
  AssertTrue('family should be found', i >= 0);
  AssertEquals('file name', lDir + 'VeraMono.ttf', FCache[i].FileName);
end;

procedure TTestFontCacheAppDirectories.TestFirstAddedDirectoryWins;
var
  lDirA: TfpgString;
  lDirB: TfpgString;
  i: integer;
begin
  lDirA := MakeDir('a');
  lDirB := MakeDir('b');
  CopyFixtureTo(lDirA, 'VeraMono.ttf');
  CopyFixtureTo(lDirB, 'VeraMono.ttf');
  FCache.AddFontDirectory(lDirA);
  FCache.AddFontDirectory(lDirB);
  AssertEquals('both files cached', 2, FCache.Count);
  i := FindVeraMono(FCache);
  AssertEquals('first added directory wins', lDirA + 'VeraMono.ttf', FCache[i].FileName);
end;

procedure TTestFontCacheAppDirectories.TestAddedDirectoryTakesPrecedenceOverExistingEntries;
var
  lSystemFont: TFontCacheItem;
  lDir: TfpgString;
  i: integer;
begin
  { Simulate a system font of the same family already in the cache }
  lSystemFont := TFontCacheItem.Create('/system/fonts/VeraMono.ttf');
  lSystemFont.FamilyName := VeraMonoFamily;
  FCache.Add(lSystemFont);

  lDir := MakeDir('a');
  CopyFixtureTo(lDir, 'VeraMono.ttf');
  FCache.AddFontDirectory(lDir);

  i := FindVeraMono(FCache);
  AssertEquals('app font wins over system font', lDir + 'VeraMono.ttf', FCache[i].FileName);
end;

procedure TTestFontCacheAppDirectories.TestCorruptFontFileIsSkipped;
var
  lDir: TfpgString;
begin
  lDir := MakeDir('a');
  WriteJunkFile(lDir + 'broken.ttf');
  CopyFixtureTo(lDir, 'VeraMono.ttf');
  FCache.AddFontDirectory(lDir);
  AssertEquals('only the valid font is cached', 1, FCache.Count);
  AssertTrue('valid font still found', FindVeraMono(FCache) >= 0);
end;

procedure TTestFontCacheAppDirectories.TestMissingDirectoryIsIgnored;
begin
  FCache.AddFontDirectory(TempDir + 'does-not-exist');
  AssertEquals('nothing cached', 0, FCache.Count);
end;

procedure TTestFontCacheAppDirectories.TestBuildFontCacheScansFontsFolderNextToExecutableFirst;
var
  i: integer;
begin
  FExeFontsDir := fpgExtractFilePath(ParamStr(0)) + 'fonts' + PathDelim;
  if fpgDirectoryExists(FExeFontsDir) then
  begin
    FExeFontsDir := '';  { not ours - don't delete it in TearDown }
    Fail('Unexpected fonts folder next to the test runner');
  end;
  fpgForceDirectories(FExeFontsDir);
  CopyFixtureTo(FExeFontsDir, 'VeraMono.ttf');

  FCache.BuildFontCache;

  i := FindVeraMono(FCache);
  AssertTrue('family should be found', i >= 0);
  AssertEquals('app-local font wins', FExeFontsDir + 'VeraMono.ttf', FCache[i].FileName);
end;
{$ENDIF}

procedure RegisterTests;
begin
  RegisterTest(TTestAppFontDirectories);
  {$IFDEF TEST_FONTCACHE}
  RegisterTest(TTestFontCacheAppDirectories);
  {$ENDIF}
end;

initialization
  Randomize;
  RegisterTests;

end.
