unit tcaggfreetype;

{$mode objfpc}{$H+}

{ Tests for the AggPas FreeType font engine and Agg2D text placement.

  See GitHub issue #203: symbol fonts (eg: Chess Marroquin) rendered nothing
  because a charmap could never be selected, and vertical centring relied on
  the 'H' glyph which symbol fonts do not have.

  The AggPas FreeType engine is only used on Unix-like platforms (Windows
  uses the Win32 TrueType engine), so these tests are Unix only. }

interface

{$IFDEF UNIX}
uses
  Classes, SysUtils,
  fpcunit, testregistry,
  agg_basics, agg_2D, agg_font_cache_manager, agg_font_freetype,
  agg_font_freetype_lib;

type
  TTestAggFreeType = class(TTestCase)
  private
    function FixtureFont(const AName: string): string;
  published
    procedure TestEncodingTagsMatchFreeType;
    procedure TestSelectUnicodeCharmap;
    procedure TestSelectAppleRomanCharmapOnSymbolFont;
    procedure TestSymbolFontRendersWithoutExplicitCharmap;
  end;

  TTestAgg2DTextAlign = class(TTestCase)
  private
    FBuf: array of Byte;
    FAgg: Agg2D_ptr;
    function FixtureFont(const AName: string): string;
    procedure InkRows(out ATop, ABottom: Integer);
    procedure DrawCentred(const AFont, AText: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLatinFontCentresVertically;
    procedure TestSymbolFontCentresVertically;
    procedure TestLatinBaselineDoesNotDependOnText;
  end;
{$ENDIF}

implementation

{$IFDEF UNIX}
const
  cSize = 120;

function FixturePath(const AName: string): string;
begin
  { pasbuild copies src/test/resources/ next to the test runner }
  Result := ExtractFilePath(ParamStr(0)) + 'fontfixtures' + PathDelim + AName;
end;

{ TTestAggFreeType }

function TTestAggFreeType.FixtureFont(const AName: string): string;
begin
  Result := FixturePath(AName);
  if not FileExists(Result) then
    Fail('Font fixture not found: ' + Result);
end;

procedure TTestAggFreeType.TestEncodingTagsMatchFreeType;
begin
  { FreeType's FT_ENC_TAG packs the four characters big-endian into a
    32-bit integer, eg: FT_ENC_TAG('a','r','m','n') = $61726D6E }
  AssertEquals('none', 0, LongWord(FT_ENCODING_NONE));
  AssertEquals('unic', $756E6963, LongWord(FT_ENCODING_UNICODE));
  AssertEquals('symb', $73796D62, LongWord(FT_ENCODING_MS_SYMBOL));
  AssertEquals('armn', $61726D6E, LongWord(FT_ENCODING_APPLE_ROMAN));
end;

procedure TTestAggFreeType.TestSelectUnicodeCharmap;
var
  eng: font_engine_freetype_int32;
begin
  eng.Construct;
  try
    AssertTrue('load font', eng.load_font(PChar(FixtureFont('VeraMono.ttf')), 0, glyph_ren_outline));
    AssertTrue('select unicode charmap, error=' + IntToStr(eng._last_error),
      eng.char_map_(FT_ENCODING_UNICODE));
  finally
    eng.Destruct;
  end;
end;

procedure TTestAggFreeType.TestSelectAppleRomanCharmapOnSymbolFont;
var
  eng: font_engine_freetype_int32;
begin
  eng.Construct;
  try
    AssertTrue('load font', eng.load_font(PChar(FixtureFont('MarrFont.ttf')), 0, glyph_ren_outline));
    AssertTrue('select apple roman charmap, error=' + IntToStr(eng._last_error),
      eng.char_map_(FT_ENCODING_APPLE_ROMAN));
    eng.height_(40);
    eng.prepare_glyph(Ord('q'));
    AssertTrue('white queen glyph found', eng.glyph_index <> 0);
  finally
    eng.Destruct;
  end;
end;

procedure TTestAggFreeType.TestSymbolFontRendersWithoutExplicitCharmap;
var
  eng: font_engine_freetype_int32;
begin
  eng.Construct;
  try
    AssertTrue('load font', eng.load_font(PChar(FixtureFont('MarrFont.ttf')), 0, glyph_ren_outline));
    eng.height_(40);
    eng.prepare_glyph(Ord('q'));
    AssertTrue('white queen glyph found', eng.glyph_index <> 0);
  finally
    eng.Destruct;
  end;
end;

{ TTestAgg2DTextAlign }

function TTestAgg2DTextAlign.FixtureFont(const AName: string): string;
begin
  Result := FixturePath(AName);
  if not FileExists(Result) then
    Fail('Font fixture not found: ' + Result);
end;

procedure TTestAgg2DTextAlign.SetUp;
begin
  SetLength(FBuf, cSize * cSize * 4);
  New(FAgg, Construct);
  FAgg^.attach(@FBuf[0], cSize, cSize, cSize * 4);
  FAgg^.clearAll(255, 255, 255);
  FAgg^.noLine;
  FAgg^.fillColor(0, 0, 0);
end;

procedure TTestAgg2DTextAlign.TearDown;
begin
  Dispose(FAgg, Destruct);
  FAgg := nil;
  SetLength(FBuf, 0);
end;

procedure TTestAgg2DTextAlign.DrawCentred(const AFont, AText: string);
begin
  FAgg^.font(PChar(FixtureFont(AFont)), 48);
  FAgg^.textAlignment(AlignCenter, AlignCenter);
  FAgg^.text(cSize / 2, cSize / 2, AText);
end;

{ Returns the first and last buffer rows containing dark (ink) pixels. }
procedure TTestAgg2DTextAlign.InkRows(out ATop, ABottom: Integer);
var
  x, y: Integer;
begin
  ATop := -1;
  ABottom := -1;
  for y := 0 to cSize - 1 do
    for x := 0 to cSize - 1 do
      if FBuf[(y * cSize + x) * 4 + 1] < 128 then
      begin
        if ATop < 0 then
          ATop := y;
        ABottom := y;
        Break;
      end;
  if ATop < 0 then
    Fail('Nothing was rendered');
end;

procedure TTestAgg2DTextAlign.TestLatinFontCentresVertically;
var
  t, b: Integer;
begin
  DrawCentred('VeraMono.ttf', 'H');
  InkRows(t, b);
  AssertTrue(Format('ink centre %d expected near %d (rows %d..%d)', [(t + b) div 2, cSize div 2, t, b]),
    Abs((t + b) div 2 - cSize div 2) <= 2);
end;

procedure TTestAgg2DTextAlign.TestSymbolFontCentresVertically;
var
  t, b: Integer;
begin
  DrawCentred('MarrFont.ttf', 'q');
  InkRows(t, b);
  AssertTrue(Format('ink centre %d expected near %d (rows %d..%d)', [(t + b) div 2, cSize div 2, t, b]),
    Abs((t + b) div 2 - cSize div 2) <= 2);
end;

procedure TTestAgg2DTextAlign.TestLatinBaselineDoesNotDependOnText;
var
  t1, b1, t2, b2: Integer;
begin
  { 'x' has no ascender. With cap-height based centring its bottom edge
    sits on the same baseline as 'H'; ink based centring would move it. }
  DrawCentred('VeraMono.ttf', 'H');
  InkRows(t1, b1);
  FAgg^.clearAll(255, 255, 255);
  DrawCentred('VeraMono.ttf', 'x');
  InkRows(t2, b2);
  { Agg2D's buffer may be y-up or y-down, so the baseline is whichever
    edge the two glyphs share. }
  AssertTrue(Format('shared baseline: H rows %d..%d, x rows %d..%d', [t1, b1, t2, b2]),
    (Abs(t1 - t2) <= 1) or (Abs(b1 - b2) <= 1));
end;

initialization
  RegisterTest('AggPas', TTestAggFreeType);
  RegisterTest('AggPas', TTestAgg2DTextAlign);
{$ENDIF}

end.
