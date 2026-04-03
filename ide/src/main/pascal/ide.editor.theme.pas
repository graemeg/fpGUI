{
    fpGUI IDE - Editor Theme System

    Copyright (C) 2006 - 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines the TEditorTheme record — a pure data structure that maps
      token categories and editor chrome elements to colours and font
      styles.  Themes are loadable from INI files and four built-in
      themes are provided (Default, Dark, Solarized Dark, Solarized Light).

      The token style slots use numbered variants (jEdit-inspired) so that
      a single theme file works across many languages.  Each language
      highlighter decides which tokens map to which slots.
}
unit ide.editor.theme;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base,
  ide.highlighter;

type

  { Font style flags for a token }
  TTokenStyleFlag = (tsfBold, tsfItalic, tsfUnderline);
  TTokenStyleFlags = set of TTokenStyleFlag;

  { Style for a single token category }
  TTokenStyle = record
    Foreground: TfpgColor;
    Background: TfpgColor;    // clNone = use editor background
    Style: TTokenStyleFlags;
  end;

  { Editor chrome colours (non-token UI elements) }
  TEditorChrome = record
    Background: TfpgColor;        // editor text area background
    Foreground: TfpgColor;        // default text colour
    CurrentLine: TfpgColor;       // current line highlight (clNone = auto)
    ExecutionLine: TfpgColor;     // debugger execution position highlight
    Selection: TfpgColor;         // selection background
    SelectionText: TfpgColor;     // selection foreground
    GutterBackground: TfpgColor;  // gutter / line-number area background
    GutterText: TfpgColor;        // line-number text colour
    RightEdge: TfpgColor;         // right margin indicator
    BracketMatch: TfpgColor;      // bracket match highlight background
  end;

  { Complete editor theme }
  TEditorTheme = record
    Name: string;
    Description: string;
    Chrome: TEditorChrome;
    TokenStyles: array[THighlightCategory] of TTokenStyle;
  end;

{ Built-in themes }
function DefaultTheme: TEditorTheme;
function DarkTheme: TEditorTheme;
function SolarizedDarkTheme: TEditorTheme;
function SolarizedLightTheme: TEditorTheme;

{ Load / save }
function LoadThemeFromINI(const AFileName: string): TEditorTheme;
procedure SaveThemeToINI(const ATheme: TEditorTheme; const AFileName: string);

{ Utility: find all .ini theme files in a directory }
procedure FindThemeFiles(const ADirectory: string; AList: TStrings);

{ Category name <-> enum conversion }
function CategoryToName(ACat: THighlightCategory): string;
function NameToCategory(const AName: string; out ACat: THighlightCategory): Boolean;


implementation

uses
  IniFiles;

const
  { Section names in the INI file }
  cSectTheme  = 'Theme';
  cSectEditor = 'Editor';

  { Maps THighlightCategory to INI section names.
    Order must match the enum declaration. }
  cCategoryNames: array[THighlightCategory] of string = (
    'Whitespace',
    'Keyword1',
    'Keyword2',
    'Keyword3',
    'Identifier',
    'String1',
    'String2',
    'Number',
    'Comment1',
    'Comment2',
    'Directive',
    'Symbol',
    'Operator',
    'Function',
    'Label',
    'Markup',
    'Invalid'
  );


{ --------------------------------------------------------------------------
  Colour helpers
  -------------------------------------------------------------------------- }

{ Parse a hex colour string (RRGGBB or AARRGGBB) to TfpgColor.
  Returns ADefault if the string is empty or invalid. }
function HexToColor(const AHex: string; ADefault: TfpgColor): TfpgColor;
var
  s: string;
  v: Int64;
begin
  Result := ADefault;
  s := Trim(AHex);
  if s = '' then
    Exit;
  { Strip optional leading $ or # }
  if (s[1] = '$') or (s[1] = '#') then
    Delete(s, 1, 1);
  if not TryStrToInt64('$' + s, v) then
    Exit;
  if Length(s) <= 6 then
    v := v or Int64($FF000000);  // add full alpha if RRGGBB
  Result := TfpgColor(v);
end;

{ Convert TfpgColor to RRGGBB hex string (strips alpha). }
function ColorToHex(AColor: TfpgColor): string;
begin
  if AColor = clNone then
    Result := ''
  else
    Result := IntToHex(AColor and $00FFFFFF, 6);
end;

{ Parse style flags from a comma-separated string (e.g. 'bold,italic') }
function ParseStyleFlags(const AStr: string): TTokenStyleFlags;
var
  s: string;
begin
  Result := [];
  s := LowerCase(Trim(AStr));
  if Pos('bold', s) > 0 then
    Include(Result, tsfBold);
  if Pos('italic', s) > 0 then
    Include(Result, tsfItalic);
  if Pos('underline', s) > 0 then
    Include(Result, tsfUnderline);
end;

{ Convert style flags to a comma-separated string }
function StyleFlagsToStr(AFlags: TTokenStyleFlags): string;
begin
  Result := '';
  if tsfBold in AFlags then
    Result := 'bold';
  if tsfItalic in AFlags then
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + 'italic';
  end;
  if tsfUnderline in AFlags then
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + 'underline';
  end;
end;


{ --------------------------------------------------------------------------
  Token style helper — creates a TTokenStyle value
  -------------------------------------------------------------------------- }

function MakeTokenStyle(AFg: TfpgColor; AStyle: TTokenStyleFlags;
  ABg: TfpgColor = TfpgColor(clNone)): TTokenStyle;
begin
  Result.Foreground := AFg;
  Result.Background := ABg;
  Result.Style := AStyle;
end;


{ --------------------------------------------------------------------------
  Category name <-> enum
  -------------------------------------------------------------------------- }

function CategoryToName(ACat: THighlightCategory): string;
begin
  Result := cCategoryNames[ACat];
end;

function NameToCategory(const AName: string; out ACat: THighlightCategory): Boolean;
var
  cat: THighlightCategory;
  s: string;
begin
  Result := False;
  s := LowerCase(Trim(AName));
  for cat := Low(THighlightCategory) to High(THighlightCategory) do
  begin
    if LowerCase(cCategoryNames[cat]) = s then
    begin
      ACat := cat;
      Exit(True);
    end;
  end;
end;


{ --------------------------------------------------------------------------
  Built-in themes
  -------------------------------------------------------------------------- }

function InitBlankTheme: TEditorTheme;
var
  cat: THighlightCategory;
begin
  Result.Name := '';
  Result.Description := '';
  FillChar(Result.Chrome, SizeOf(Result.Chrome), 0);
  for cat := Low(THighlightCategory) to High(THighlightCategory) do
  begin
    Result.TokenStyles[cat].Foreground := clBlack;
    Result.TokenStyles[cat].Background := clNone;
    Result.TokenStyles[cat].Style := [];
  end;
end;

function DefaultTheme: TEditorTheme;
begin
  Result := InitBlankTheme;
  Result.Name := 'Default';
  Result.Description := 'Light theme with IntelliJ-inspired colours';

  { Editor chrome }
  with Result.Chrome do
  begin
    Background      := clWhite;
    Foreground      := clBlack;
    CurrentLine     := TfpgColor($ffFFFAE3);
    ExecutionLine   := TfpgColor($ffFFF3A3);  // pale yellow — debugger stop marker
    Selection       := TfpgColor($ff3399FF);
    SelectionText   := clWhite;
    GutterBackground := TfpgColor($ffF0F0F0);
    GutterText      := TfpgColor($ff999999);
    RightEdge       := TfpgColor($ffE0E0E0);
    BracketMatch    := TfpgColor($ffB4D7FF);  // light blue highlight
  end;

  { Token styles — IntelliJ IDEA-inspired }
  Result.TokenStyles[hcKeyword1]  := MakeTokenStyle(TfpgColor($ff000080), [tsfBold]);   // dark blue, bold
  Result.TokenStyles[hcKeyword2]  := MakeTokenStyle(TfpgColor($ff000080), [tsfBold]);
  Result.TokenStyles[hcKeyword3]  := MakeTokenStyle(TfpgColor($ff000080), []);
  Result.TokenStyles[hcIdentifier] := MakeTokenStyle(clBlack, []);
  Result.TokenStyles[hcString1]   := MakeTokenStyle(TfpgColor($ff008000), []);           // green
  Result.TokenStyles[hcString2]   := MakeTokenStyle(TfpgColor($ff008000), []);
  Result.TokenStyles[hcNumber]    := MakeTokenStyle(TfpgColor($ff0000FF), []);           // blue
  Result.TokenStyles[hcComment1]  := MakeTokenStyle(TfpgColor($ff808080), [tsfItalic]);  // grey, italic
  Result.TokenStyles[hcComment2]  := MakeTokenStyle(TfpgColor($ff629755), [tsfItalic]);  // green-grey (doc comments)
  Result.TokenStyles[hcDirective] := MakeTokenStyle(TfpgColor($ffBBB529), []);           // olive/yellow
  Result.TokenStyles[hcSymbol]    := MakeTokenStyle(clBlack, []);
  Result.TokenStyles[hcOperator]  := MakeTokenStyle(clBlack, []);
  Result.TokenStyles[hcFunction]  := MakeTokenStyle(TfpgColor($ff7A7A43), []);           // dark yellow
  Result.TokenStyles[hcLabel]     := MakeTokenStyle(TfpgColor($ff000080), []);
  Result.TokenStyles[hcMarkup]    := MakeTokenStyle(TfpgColor($ff008000), [tsfBold]);
  Result.TokenStyles[hcInvalid]   := MakeTokenStyle(clRed, []);
  Result.TokenStyles[hcWhitespace] := MakeTokenStyle(clBlack, []);
end;

function DarkTheme: TEditorTheme;
begin
  Result := InitBlankTheme;
  Result.Name := 'Dark';
  Result.Description := 'Dark theme with Darcula-inspired colours';

  { Editor chrome }
  with Result.Chrome do
  begin
    Background      := TfpgColor($ff2B2B2B);
    Foreground      := TfpgColor($ffA9B7C6);
    CurrentLine     := TfpgColor($ff323232);
    ExecutionLine   := TfpgColor($ff3D3D1A);  // dark olive yellow — debugger stop marker
    Selection       := TfpgColor($ff214283);
    SelectionText   := TfpgColor($ffA9B7C6);
    GutterBackground := TfpgColor($ff313335);
    GutterText      := TfpgColor($ff606366);
    RightEdge       := TfpgColor($ff4D4D4D);
    BracketMatch    := TfpgColor($ff3B514D);  // subtle teal highlight
  end;

  { Token styles — Darcula-inspired }
  Result.TokenStyles[hcWhitespace] := MakeTokenStyle(TfpgColor($ffA9B7C6), []);
  Result.TokenStyles[hcKeyword1]  := MakeTokenStyle(TfpgColor($ffCC7832), [tsfBold]);   // orange, bold
  Result.TokenStyles[hcKeyword2]  := MakeTokenStyle(TfpgColor($ffCC7832), [tsfBold]);
  Result.TokenStyles[hcKeyword3]  := MakeTokenStyle(TfpgColor($ffCC7832), []);
  Result.TokenStyles[hcIdentifier] := MakeTokenStyle(TfpgColor($ffA9B7C6), []);         // light grey-blue
  Result.TokenStyles[hcString1]   := MakeTokenStyle(TfpgColor($ff6A8759), []);           // muted green
  Result.TokenStyles[hcString2]   := MakeTokenStyle(TfpgColor($ff6A8759), []);
  Result.TokenStyles[hcNumber]    := MakeTokenStyle(TfpgColor($ff6897BB), []);           // steel blue
  Result.TokenStyles[hcComment1]  := MakeTokenStyle(TfpgColor($ff808080), [tsfItalic]);  // grey, italic
  Result.TokenStyles[hcComment2]  := MakeTokenStyle(TfpgColor($ff629755), [tsfItalic]);  // green (doc comments)
  Result.TokenStyles[hcDirective] := MakeTokenStyle(TfpgColor($ffBBB529), []);           // yellow
  Result.TokenStyles[hcSymbol]    := MakeTokenStyle(TfpgColor($ffA9B7C6), []);
  Result.TokenStyles[hcOperator]  := MakeTokenStyle(TfpgColor($ffA9B7C6), []);
  Result.TokenStyles[hcFunction]  := MakeTokenStyle(TfpgColor($ffFFC66D), []);           // light orange
  Result.TokenStyles[hcLabel]     := MakeTokenStyle(TfpgColor($ffCC7832), []);
  Result.TokenStyles[hcMarkup]    := MakeTokenStyle(TfpgColor($ffE8BF6A), [tsfBold]);
  Result.TokenStyles[hcInvalid]   := MakeTokenStyle(TfpgColor($ffBC3F3C), []);           // red
end;

function SolarizedDarkTheme: TEditorTheme;
const
  { Solarized palette }
  base03  = TfpgColor($ff002B36);
  base02  = TfpgColor($ff073642);
  base01  = TfpgColor($ff586E75);
  base00  = TfpgColor($ff657B83);
  base0   = TfpgColor($ff839496);
  base1   = TfpgColor($ff93A1A1);
  base2   = TfpgColor($ffEEE8D5);
  base3   = TfpgColor($ffFDF6E3);
  yellow  = TfpgColor($ffB58900);
  orange  = TfpgColor($ffCB4B16);
  red     = TfpgColor($ffDC322F);
  magenta = TfpgColor($ffD33682);
  violet  = TfpgColor($ff6C71C4);
  blue    = TfpgColor($ff268BD2);
  cyan    = TfpgColor($ff2AA198);
  green   = TfpgColor($ff859900);
begin
  Result := InitBlankTheme;
  Result.Name := 'Solarized Dark';
  Result.Description := 'Solarized colour scheme — dark background';

  { Editor chrome }
  with Result.Chrome do
  begin
    Background      := base03;
    Foreground      := base0;
    CurrentLine     := base02;
    ExecutionLine   := TfpgColor($ff3A3500);  // dark yellow tint — debugger stop marker
    Selection       := base01;
    SelectionText   := base2;
    GutterBackground := base02;
    GutterText      := base01;
    RightEdge       := base01;
    BracketMatch    := base01;
  end;

  { Token styles }
  Result.TokenStyles[hcWhitespace] := MakeTokenStyle(base0, []);
  Result.TokenStyles[hcKeyword1]  := MakeTokenStyle(green, [tsfBold]);
  Result.TokenStyles[hcKeyword2]  := MakeTokenStyle(green, []);
  Result.TokenStyles[hcKeyword3]  := MakeTokenStyle(yellow, []);
  Result.TokenStyles[hcIdentifier] := MakeTokenStyle(base0, []);
  Result.TokenStyles[hcString1]   := MakeTokenStyle(cyan, []);
  Result.TokenStyles[hcString2]   := MakeTokenStyle(cyan, []);
  Result.TokenStyles[hcNumber]    := MakeTokenStyle(magenta, []);
  Result.TokenStyles[hcComment1]  := MakeTokenStyle(base01, [tsfItalic]);
  Result.TokenStyles[hcComment2]  := MakeTokenStyle(base01, [tsfItalic]);
  Result.TokenStyles[hcDirective] := MakeTokenStyle(orange, []);
  Result.TokenStyles[hcSymbol]    := MakeTokenStyle(base0, []);
  Result.TokenStyles[hcOperator]  := MakeTokenStyle(base0, []);
  Result.TokenStyles[hcFunction]  := MakeTokenStyle(blue, []);
  Result.TokenStyles[hcLabel]     := MakeTokenStyle(orange, []);
  Result.TokenStyles[hcMarkup]    := MakeTokenStyle(blue, [tsfBold]);
  Result.TokenStyles[hcInvalid]   := MakeTokenStyle(red, []);
end;

function SolarizedLightTheme: TEditorTheme;
const
  { Solarized palette }
  base03  = TfpgColor($ff002B36);
  base02  = TfpgColor($ff073642);
  base01  = TfpgColor($ff586E75);
  base00  = TfpgColor($ff657B83);
  base0   = TfpgColor($ff839496);
  base1   = TfpgColor($ff93A1A1);
  base2   = TfpgColor($ffEEE8D5);
  base3   = TfpgColor($ffFDF6E3);
  yellow  = TfpgColor($ffB58900);
  orange  = TfpgColor($ffCB4B16);
  red     = TfpgColor($ffDC322F);
  magenta = TfpgColor($ffD33682);
  violet  = TfpgColor($ff6C71C4);
  blue    = TfpgColor($ff268BD2);
  cyan    = TfpgColor($ff2AA198);
  green   = TfpgColor($ff859900);
begin
  Result := InitBlankTheme;
  Result.Name := 'Solarized Light';
  Result.Description := 'Solarized colour scheme — light background';

  { Editor chrome }
  with Result.Chrome do
  begin
    Background      := base3;
    Foreground      := base0;
    CurrentLine     := base2;
    ExecutionLine   := TfpgColor($ffFFF3CC);  // warm yellow — debugger stop marker
    Selection       := base1;
    SelectionText   := base03;
    GutterBackground := base2;
    GutterText      := base1;
    RightEdge       := base1;
    BracketMatch    := base1;
  end;

  { Token styles }
  Result.TokenStyles[hcWhitespace] := MakeTokenStyle(base00, []);
  Result.TokenStyles[hcKeyword1]  := MakeTokenStyle(green, [tsfBold]);
  Result.TokenStyles[hcKeyword2]  := MakeTokenStyle(green, []);
  Result.TokenStyles[hcKeyword3]  := MakeTokenStyle(yellow, []);
  Result.TokenStyles[hcIdentifier] := MakeTokenStyle(base00, []);
  Result.TokenStyles[hcString1]   := MakeTokenStyle(cyan, []);
  Result.TokenStyles[hcString2]   := MakeTokenStyle(cyan, []);
  Result.TokenStyles[hcNumber]    := MakeTokenStyle(magenta, []);
  Result.TokenStyles[hcComment1]  := MakeTokenStyle(base1, [tsfItalic]);
  Result.TokenStyles[hcComment2]  := MakeTokenStyle(base1, [tsfItalic]);
  Result.TokenStyles[hcDirective] := MakeTokenStyle(orange, []);
  Result.TokenStyles[hcSymbol]    := MakeTokenStyle(base00, []);
  Result.TokenStyles[hcOperator]  := MakeTokenStyle(base00, []);
  Result.TokenStyles[hcFunction]  := MakeTokenStyle(blue, []);
  Result.TokenStyles[hcLabel]     := MakeTokenStyle(orange, []);
  Result.TokenStyles[hcMarkup]    := MakeTokenStyle(blue, [tsfBold]);
  Result.TokenStyles[hcInvalid]   := MakeTokenStyle(red, []);
end;


{ --------------------------------------------------------------------------
  INI loading / saving
  -------------------------------------------------------------------------- }

function LoadThemeFromINI(const AFileName: string): TEditorTheme;
var
  ini: TIniFile;
  cat: THighlightCategory;
  sect: string;
begin
  { Start from default so missing keys are sensible }
  Result := DefaultTheme;

  ini := TIniFile.Create(AFileName);
  try
    { [Theme] metadata }
    Result.Name := ini.ReadString(cSectTheme, 'Name', Result.Name);
    Result.Description := ini.ReadString(cSectTheme, 'Description', Result.Description);

    { [Editor] chrome }
    with Result.Chrome do
    begin
      Background      := HexToColor(ini.ReadString(cSectEditor, 'Background', ''), Background);
      Foreground      := HexToColor(ini.ReadString(cSectEditor, 'Foreground', ''), Foreground);
      CurrentLine     := HexToColor(ini.ReadString(cSectEditor, 'CurrentLine', ''), CurrentLine);
      ExecutionLine   := HexToColor(ini.ReadString(cSectEditor, 'ExecutionLine', ''), ExecutionLine);
      Selection       := HexToColor(ini.ReadString(cSectEditor, 'Selection', ''), Selection);
      SelectionText   := HexToColor(ini.ReadString(cSectEditor, 'SelectionText', ''), SelectionText);
      GutterBackground := HexToColor(ini.ReadString(cSectEditor, 'GutterBackground', ''), GutterBackground);
      GutterText      := HexToColor(ini.ReadString(cSectEditor, 'GutterText', ''), GutterText);
      RightEdge       := HexToColor(ini.ReadString(cSectEditor, 'RightEdge', ''), RightEdge);
      BracketMatch    := HexToColor(ini.ReadString(cSectEditor, 'BracketMatch', ''), BracketMatch);
    end;

    { Token style sections }
    for cat := Low(THighlightCategory) to High(THighlightCategory) do
    begin
      sect := cCategoryNames[cat];
      if not ini.SectionExists(sect) then
        Continue;
      Result.TokenStyles[cat].Foreground :=
        HexToColor(ini.ReadString(sect, 'Foreground', ''), Result.TokenStyles[cat].Foreground);
      Result.TokenStyles[cat].Background :=
        HexToColor(ini.ReadString(sect, 'Background', ''), Result.TokenStyles[cat].Background);
      Result.TokenStyles[cat].Style :=
        ParseStyleFlags(ini.ReadString(sect, 'Style', ''));
    end;
  finally
    ini.Free;
  end;
end;

procedure SaveThemeToINI(const ATheme: TEditorTheme; const AFileName: string);
var
  ini: TIniFile;
  cat: THighlightCategory;
  sect, sty: string;
begin
  ini := TIniFile.Create(AFileName);
  try
    { [Theme] metadata }
    ini.WriteString(cSectTheme, 'Name', ATheme.Name);
    ini.WriteString(cSectTheme, 'Description', ATheme.Description);

    { [Editor] chrome }
    with ATheme.Chrome do
    begin
      ini.WriteString(cSectEditor, 'Background', ColorToHex(Background));
      ini.WriteString(cSectEditor, 'Foreground', ColorToHex(Foreground));
      ini.WriteString(cSectEditor, 'CurrentLine', ColorToHex(CurrentLine));
      ini.WriteString(cSectEditor, 'ExecutionLine', ColorToHex(ExecutionLine));
      ini.WriteString(cSectEditor, 'Selection', ColorToHex(Selection));
      ini.WriteString(cSectEditor, 'SelectionText', ColorToHex(SelectionText));
      ini.WriteString(cSectEditor, 'GutterBackground', ColorToHex(GutterBackground));
      ini.WriteString(cSectEditor, 'GutterText', ColorToHex(GutterText));
      ini.WriteString(cSectEditor, 'RightEdge', ColorToHex(RightEdge));
      ini.WriteString(cSectEditor, 'BracketMatch', ColorToHex(BracketMatch));
    end;

    { Token style sections }
    for cat := Low(THighlightCategory) to High(THighlightCategory) do
    begin
      sect := cCategoryNames[cat];
      ini.WriteString(sect, 'Foreground', ColorToHex(ATheme.TokenStyles[cat].Foreground));
      if ATheme.TokenStyles[cat].Background <> clNone then
        ini.WriteString(sect, 'Background', ColorToHex(ATheme.TokenStyles[cat].Background));
      sty := StyleFlagsToStr(ATheme.TokenStyles[cat].Style);
      if sty <> '' then
        ini.WriteString(sect, 'Style', sty);
    end;
  finally
    ini.Free;
  end;
end;


{ --------------------------------------------------------------------------
  Theme file discovery
  -------------------------------------------------------------------------- }

procedure FindThemeFiles(const ADirectory: string; AList: TStrings);
var
  sr: TSearchRec;
  dir: string;
begin
  dir := IncludeTrailingPathDelimiter(ADirectory);
  if FindFirst(dir + '*.ini', faAnyFile, sr) = 0 then
  begin
    try
      repeat
        AList.Add(dir + sr.Name);
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;
end;


end.
