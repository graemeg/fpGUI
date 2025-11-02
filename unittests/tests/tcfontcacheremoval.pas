unit tcfontcacheremoval;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit, testutils, testregistry,
  fpg_base,
  fpg_main,
  fpg_fontmanager;

type
  { Mock font resource for testing - no platform dependencies }
  TMockFontResource = class(TfpgFontResourceBase)
  private
    FWidth: integer;
    FHeight: integer;
  public
    constructor Create(const AFontDesc: string; AWidth, AHeight: integer); reintroduce;
    function GetAscent: integer; override;
    function GetDescent: integer; override;
    function GetHeight: integer; override;
    function GetTextWidth(const txt: string): integer; override;
    function GetCanvasRef: TObject; override;
    function HandleIsValid: boolean; override;
  end;

  { Test case for persistent font cache behavior }
  TTestFontCacheRemoval = class(TTestCase)
  private
    FFontManager: TfpgFontManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Test that fonts stay cached (persistent cache model) }
    procedure TestFontStaysInCacheWhenReleased;
    procedure TestMultipleFontsCached;
    procedure TestCacheSizeGrowsWithUniqueFonts;
    procedure TestCacheNormalizationReturnsIdenticalObject;
  end;

procedure RegisterTests;

implementation

{ TMockFontResource }

constructor TMockFontResource.Create(const AFontDesc: string; AWidth, AHeight: integer);
begin
  inherited Create(AFontDesc);
  FWidth := AWidth;
  FHeight := AHeight;
end;

function TMockFontResource.GetAscent: integer;
begin
  Result := FHeight * 80 div 100;  // 80% of height
end;

function TMockFontResource.GetDescent: integer;
begin
  Result := FHeight * 20 div 100;  // 20% of height
end;

function TMockFontResource.GetHeight: integer;
begin
  Result := FHeight;
end;

function TMockFontResource.GetTextWidth(const txt: string): integer;
begin
  Result := Length(txt) * FWidth;
end;

function TMockFontResource.GetCanvasRef: TObject;
begin
  Result := nil;
end;

function TMockFontResource.HandleIsValid: boolean;
begin
  Result := True;
end;

{ TTestFontCacheRemoval }

procedure TTestFontCacheRemoval.SetUp;
begin
  inherited SetUp;

  { Initialize fpGUI application once }
  if not fpgApplication.IsInitialized then
    fpgApplication.Initialize;

  FFontManager := TfpgFontManager.Create;
end;

procedure TTestFontCacheRemoval.TearDown;
begin
  FFontManager.Free;
  inherited TearDown;
end;

procedure TTestFontCacheRemoval.TestFontStaysInCacheWhenReleased;
var
  font: TfpgFontResourceBase;
  initialCacheSize, afterAddSize, afterReleaseSize: integer;
  fontDesc: string;
begin
  { Get initial cache size }
  initialCacheSize := FFontManager.GetCacheSize;

  { Use a specific font descriptor }
  fontDesc := 'Liberation Sans-14';

  { Get font - this adds it to cache }
  font := FFontManager.GetFont(fontDesc);
  CheckNotNull(font, 'Font should be created');

  { Cache should have grown }
  afterAddSize := FFontManager.GetCacheSize;
  CheckEquals(initialCacheSize + 1, afterAddSize,
    Format('Cache should grow by 1 (was %d, now %d)', [initialCacheSize, afterAddSize]));

  { Release the font reference }
  font := nil;

  { Font should STAY in cache (persistent cache model) }
  afterReleaseSize := FFontManager.GetCacheSize;
  CheckEquals(afterAddSize, afterReleaseSize,
    Format('Cache size should stay same after release - persistent cache (expected %d, got %d)',
           [afterAddSize, afterReleaseSize]));
end;

procedure TTestFontCacheRemoval.TestMultipleFontsCached;
var
  initialSize, afterAdd3, afterReleaseAll: integer;
begin
  { Get initial cache size }
  initialSize := FFontManager.GetCacheSize;

  { Add three different fonts }
  FFontManager.GetFont('Liberation Sans-12');
  FFontManager.GetFont('Liberation Sans-14');
  FFontManager.GetFont('Liberation Sans-16');

  { Cache should have 3 more fonts }
  afterAdd3 := FFontManager.GetCacheSize;
  CheckEquals(initialSize + 3, afterAdd3,
    Format('Cache should have 3 more fonts (expected %d, got %d)',
           [initialSize + 3, afterAdd3]));

  { Release all font references }
  // No need to set to nil if not declared

  { All fonts should STAY in cache (persistent cache model) }
  afterReleaseAll := FFontManager.GetCacheSize;
  CheckEquals(afterAdd3, afterReleaseAll,
    Format('Cache should keep all fonts after release (expected %d, got %d)',
           [afterAdd3, afterReleaseAll]));
end;

procedure TTestFontCacheRemoval.TestCacheSizeGrowsWithUniqueFonts;
var
  font1, font1Again: TfpgFontResourceBase;
  initialSize, afterFont1, afterFont2, afterFont1Again: integer;
begin
  { Get initial cache size }
  initialSize := FFontManager.GetCacheSize;

  { Add first font }
  font1 := FFontManager.GetFont('Liberation Mono-12');
  afterFont1 := FFontManager.GetCacheSize;
  CheckEquals(initialSize + 1, afterFont1,
    'Cache should grow by 1 for new font');

  { Add second different font }
  FFontManager.GetFont('Liberation Mono-14');
  afterFont2 := FFontManager.GetCacheSize;
  CheckEquals(initialSize + 2, afterFont2,
    'Cache should grow by 1 for another new font');

  { Request first font again - should NOT grow cache }
  font1Again := FFontManager.GetFont('Liberation Mono-12');
  afterFont1Again := FFontManager.GetCacheSize;
  CheckEquals(afterFont2, afterFont1Again,
    'Cache should not grow when requesting existing font');

  { Verify it's the same object (cache hit) }
  CheckTrue(font1 = font1Again,
    'Should return same cached font object');

  { Cleanup }
  font1 := nil;
  // font2 := nil; // Removed
  font1Again := nil;
end;

procedure TTestFontCacheRemoval.TestCacheNormalizationReturnsIdenticalObject;
var
  font1, font2, font3, font4: TfpgFontResourceBase;
  initialSize: integer;
begin
  { Get initial cache size }
  initialSize := FFontManager.GetCacheSize;

  { Request font with normal case }
  font1 := FFontManager.GetFont('Liberation Mono-12');
  CheckNotNull(font1, 'Font1 should be created');
  CheckEquals(initialSize + 1, FFontManager.GetCacheSize,
    'Cache should grow by 1 for new font');

  { Request same font with different case - should get same object }
  font2 := FFontManager.GetFont('Liberation MONO-12');
  CheckTrue(font1 = font2,
    'Different case should return same cached object');
  CheckEquals(initialSize + 1, FFontManager.GetCacheSize,
    'Cache should not grow - normalization should find existing font');

  { Request with lowercase - should still get same object }
  font3 := FFontManager.GetFont('liberation mono-12');
  CheckTrue(font1 = font3,
    'Lowercase should return same cached object');
  CheckEquals(initialSize + 1, FFontManager.GetCacheSize,
    'Cache should still not grow');

  { Request with bold attribute in different order }
  font4 := FFontManager.GetFont('Liberation Mono-12:bold');
  CheckTrue(font1 <> font4,
    'Different attributes should return different object');
  CheckEquals(initialSize + 2, FFontManager.GetCacheSize,
    'Different attributes should create new cache entry');

  { Cleanup }
  font1 := nil;
  font2 := nil;
  font3 := nil;
  font4 := nil;
end;

procedure RegisterTests;
begin
  RegisterTest(TTestFontCacheRemoval);
end;

initialization
  RegisterTests;

end.
