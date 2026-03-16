{
    fpGUI IDE - Editor Theme Tests

    Tests for TEditorTheme loading, saving, and built-in themes.
}
unit ide.test.editortheme;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  fpg_base,
  ide.highlighter,
  ide.editor.theme;

type

  { TTestEditorTheme }

  TTestEditorTheme = class(TTestCase)
  published
    { Built-in themes }
    procedure TestDefaultThemeHasName;
    procedure TestDarkThemeHasName;
    procedure TestSolarizedDarkThemeHasName;
    procedure TestSolarizedLightThemeHasName;

    { Theme structure }
    procedure TestDefaultThemeBackground;
    procedure TestDefaultThemeKeywordStyle;
    procedure TestDefaultThemeCommentStyle;
    procedure TestDarkThemeBackground;

    { Category name mapping }
    procedure TestCategoryToName;
    procedure TestNameToCategory;
    procedure TestNameToCategoryInvalid;

    { INI round-trip }
    procedure TestSaveAndLoadRoundTrip;

    { INI loading from built-in files }
    procedure TestLoadDefaultINI;
    procedure TestLoadDarkINI;
    procedure TestLoadSolarizedDarkINI;
    procedure TestLoadSolarizedLightINI;

    { Style flags }
    procedure TestBoldStyleFlag;
    procedure TestItalicStyleFlag;

    { Colour format }
    procedure TestTokenBackgroundDefault;
  end;


implementation

uses
  IniFiles;

const
  { pasbuild copies resources to ide/target/ alongside the TestRunner binary }
  cThemeDir = 'editor-themes/';


{ TTestEditorTheme }

procedure TTestEditorTheme.TestDefaultThemeHasName;
var
  t: TEditorTheme;
begin
  t := DefaultTheme;
  AssertEquals('Default', t.Name);
end;

procedure TTestEditorTheme.TestDarkThemeHasName;
var
  t: TEditorTheme;
begin
  t := DarkTheme;
  AssertEquals('Dark', t.Name);
end;

procedure TTestEditorTheme.TestSolarizedDarkThemeHasName;
var
  t: TEditorTheme;
begin
  t := SolarizedDarkTheme;
  AssertEquals('Solarized Dark', t.Name);
end;

procedure TTestEditorTheme.TestSolarizedLightThemeHasName;
var
  t: TEditorTheme;
begin
  t := SolarizedLightTheme;
  AssertEquals('Solarized Light', t.Name);
end;

procedure TTestEditorTheme.TestDefaultThemeBackground;
var
  t: TEditorTheme;
begin
  t := DefaultTheme;
  AssertEquals('white background', Integer(clWhite), Integer(t.Chrome.Background));
end;

procedure TTestEditorTheme.TestDefaultThemeKeywordStyle;
var
  t: TEditorTheme;
begin
  t := DefaultTheme;
  AssertTrue('keyword1 bold', tsfBold in t.TokenStyles[hcKeyword1].Style);
end;

procedure TTestEditorTheme.TestDefaultThemeCommentStyle;
var
  t: TEditorTheme;
begin
  t := DefaultTheme;
  AssertTrue('comment1 italic', tsfItalic in t.TokenStyles[hcComment1].Style);
end;

procedure TTestEditorTheme.TestDarkThemeBackground;
var
  t: TEditorTheme;
begin
  t := DarkTheme;
  { Dark theme background should NOT be white }
  AssertTrue('dark bg not white',
    t.Chrome.Background <> clWhite);
end;

procedure TTestEditorTheme.TestCategoryToName;
begin
  AssertEquals('Keyword1', CategoryToName(hcKeyword1));
  AssertEquals('Comment1', CategoryToName(hcComment1));
  AssertEquals('String1', CategoryToName(hcString1));
  AssertEquals('Number', CategoryToName(hcNumber));
  AssertEquals('Invalid', CategoryToName(hcInvalid));
end;

procedure TTestEditorTheme.TestNameToCategory;
var
  cat: THighlightCategory;
begin
  AssertTrue('find Keyword1', NameToCategory('Keyword1', cat));
  AssertTrue('is hcKeyword1', cat = hcKeyword1);
  AssertTrue('find Comment2', NameToCategory('Comment2', cat));
  AssertTrue('is hcComment2', cat = hcComment2);
end;

procedure TTestEditorTheme.TestNameToCategoryInvalid;
var
  cat: THighlightCategory;
begin
  AssertFalse('unknown name', NameToCategory('NonExistent', cat));
end;

procedure TTestEditorTheme.TestSaveAndLoadRoundTrip;
var
  orig, loaded: TEditorTheme;
  fn: string;
begin
  fn := GetTempFileName('/tmp/', 'theme_test_');
  fn := fn + '.ini';
  try
    orig := DarkTheme;
    SaveThemeToINI(orig, fn);
    loaded := LoadThemeFromINI(fn);
    AssertEquals('name', orig.Name, loaded.Name);
    AssertEquals('bg', Integer(orig.Chrome.Background), Integer(loaded.Chrome.Background));
    AssertEquals('fg', Integer(orig.Chrome.Foreground), Integer(loaded.Chrome.Foreground));
    AssertEquals('kw1 fg', Integer(orig.TokenStyles[hcKeyword1].Foreground),
      Integer(loaded.TokenStyles[hcKeyword1].Foreground));
    AssertTrue('kw1 bold', tsfBold in loaded.TokenStyles[hcKeyword1].Style);
    AssertTrue('comment italic', tsfItalic in loaded.TokenStyles[hcComment1].Style);
  finally
    if FileExists(fn) then
      DeleteFile(fn);
  end;
end;

procedure TTestEditorTheme.TestLoadDefaultINI;
var
  t: TEditorTheme;
  fn: string;
begin
  fn := cThemeDir + 'default.ini';
  if not FileExists(fn) then
  begin
    Fail('Theme file not found: ' + fn);
    Exit;
  end;
  t := LoadThemeFromINI(fn);
  AssertEquals('Default', t.Name);
  AssertEquals('bg white', Integer(clWhite), Integer(t.Chrome.Background));
end;

procedure TTestEditorTheme.TestLoadDarkINI;
var
  t: TEditorTheme;
  fn: string;
begin
  fn := cThemeDir + 'dark.ini';
  if not FileExists(fn) then
  begin
    Fail('Theme file not found: ' + fn);
    Exit;
  end;
  t := LoadThemeFromINI(fn);
  AssertEquals('Dark', t.Name);
end;

procedure TTestEditorTheme.TestLoadSolarizedDarkINI;
var
  t: TEditorTheme;
  fn: string;
begin
  fn := cThemeDir + 'solarized-dark.ini';
  if not FileExists(fn) then
  begin
    Fail('Theme file not found: ' + fn);
    Exit;
  end;
  t := LoadThemeFromINI(fn);
  AssertEquals('Solarized Dark', t.Name);
end;

procedure TTestEditorTheme.TestLoadSolarizedLightINI;
var
  t: TEditorTheme;
  fn: string;
begin
  fn := cThemeDir + 'solarized-light.ini';
  if not FileExists(fn) then
  begin
    Fail('Theme file not found: ' + fn);
    Exit;
  end;
  t := LoadThemeFromINI(fn);
  AssertEquals('Solarized Light', t.Name);
end;

procedure TTestEditorTheme.TestBoldStyleFlag;
var
  t: TEditorTheme;
begin
  t := DefaultTheme;
  AssertTrue('keyword1 has bold', tsfBold in t.TokenStyles[hcKeyword1].Style);
  AssertFalse('keyword1 not italic', tsfItalic in t.TokenStyles[hcKeyword1].Style);
end;

procedure TTestEditorTheme.TestItalicStyleFlag;
var
  t: TEditorTheme;
begin
  t := DefaultTheme;
  AssertTrue('comment1 has italic', tsfItalic in t.TokenStyles[hcComment1].Style);
  AssertFalse('comment1 not bold', tsfBold in t.TokenStyles[hcComment1].Style);
end;

procedure TTestEditorTheme.TestTokenBackgroundDefault;
var
  t: TEditorTheme;
begin
  { Token backgrounds should default to clNone (use editor background) }
  t := DefaultTheme;
  AssertEquals('identifier bg is clNone',
    Integer(clNone), Integer(t.TokenStyles[hcIdentifier].Background));
end;

initialization
  RegisterTest(TTestEditorTheme);

end.
