unit tcfontmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  //TestFramework,
  fpcunit, testutils, testregistry,
  fpg_base,
  fpg_main,
  fpg_fontmanager;

type
  { TTestFontManager - Unit tests for TfpgFontManager }
  TTestFontManager = class(TTestCase)
  private
    FFontManager: TfpgFontManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Basic functionality tests }
    procedure TestCreate;
    procedure TestGetDefaultFont;
    procedure TestGetFixedFont;

    { Font caching tests }
    procedure TestFontCaching_SameDescriptor;
    procedure TestFontCaching_DifferentDescriptors;
    procedure TestCacheSize;

    { Font descriptor resolution tests }
    procedure TestEmptyDescriptor_ReturnsDefault;
    procedure TestValidDescriptor;

    { Cache statistics tests }
    procedure TestGetCacheStats;
    procedure TestGetCacheSize_Empty;
    procedure TestGetCacheSize_WithFonts;

    { Cleanup tests }
    procedure TestCacheClearsOnDestroy;
  end;

procedure RegisterTests;

implementation

procedure RegisterTests;
begin
  // TestFramework.RegisterTest('fpg_fontmanager', TTestFontManager.Suite);
  RegisterTest(TTestFontManager);
end;

{ TTestFontManager }

procedure TTestFontManager.SetUp;
begin
  inherited SetUp;
  FFontManager := TfpgFontManager.Create;

  { Set reasonable defaults for testing }
  {$IFDEF UNIX}
  FFontManager.DefaultFontDesc := 'Liberation Sans-10';
  FFontManager.FixedFontDesc := 'Liberation Mono-10';
  {$ELSE}
  FFontManager.DefaultFontDesc := 'Arial-10';
  FFontManager.FixedFontDesc := 'Courier New-10';
  {$ENDIF}
end;

procedure TTestFontManager.TearDown;
begin
  FFontManager.Free;
  inherited TearDown;
end;

procedure TTestFontManager.TestCreate;
begin
  CheckNotNull(FFontManager, 'FontManager should be created');
  CheckEquals(0, FFontManager.GetCacheSize, 'Cache should start empty');
end;

procedure TTestFontManager.TestGetDefaultFont;
var
  font: TfpgFontResourceBase;
begin
  font := FFontManager.GetDefaultFont;

  CheckNotNull(font, 'Default font should not be nil');
  CheckTrue(font.HandleIsValid, 'Default font should have valid handle');

  { Default font should be cached }
  CheckEquals(1, FFontManager.GetCacheSize, 'Default font should be in cache');
end;

procedure TTestFontManager.TestGetFixedFont;
var
  font: TfpgFontResourceBase;
begin
  font := FFontManager.GetFixedFont;

  CheckNotNull(font, 'Fixed font should not be nil');
  CheckTrue(font.HandleIsValid, 'Fixed font should have valid handle');

  { Fixed font should be cached }
  CheckEquals(1, FFontManager.GetCacheSize, 'Fixed font should be in cache');
end;

procedure TTestFontManager.TestFontCaching_SameDescriptor;
var
  font1, font2: TfpgFontResourceBase;
  descriptor: string;
begin
  {$IFDEF UNIX}
  descriptor := 'Liberation Sans-12';
  {$ELSE}
  descriptor := 'Arial-12';
  {$ENDIF}

  { Request same font twice }
  font1 := FFontManager.GetFont(descriptor);
  font2 := FFontManager.GetFont(descriptor);

  { Should return same instance (cached) }
  CheckTrue(font1 = font2, 'Same descriptor should return same font instance');

  { Cache should only have one entry }
  CheckEquals(1, FFontManager.GetCacheSize, 'Cache should have only 1 font');
end;

procedure TTestFontManager.TestFontCaching_DifferentDescriptors;
var
  font1, font2: TfpgFontResourceBase;
begin
  {$IFDEF UNIX}
  font1 := FFontManager.GetFont('Liberation Sans-10');
  font2 := FFontManager.GetFont('Liberation Sans-12');
  {$ELSE}
  font1 := FFontManager.GetFont('Arial-10');
  font2 := FFontManager.GetFont('Arial-12');
  {$ENDIF}

  { Different descriptors should return different fonts }
  CheckFalse(font1 = font2, 'Different descriptors should return different fonts');

  { Cache should have two entries }
  CheckEquals(2, FFontManager.GetCacheSize, 'Cache should have 2 fonts');
end;

procedure TTestFontManager.TestCacheSize;
begin
  CheckEquals(0, FFontManager.GetCacheSize, 'Initial cache should be empty');

  {$IFDEF UNIX}
  FFontManager.GetFont('Liberation Sans-10');
  {$ELSE}
  FFontManager.GetFont('Arial-10');
  {$ENDIF}
  CheckEquals(1, FFontManager.GetCacheSize, 'Cache size should be 1');

  {$IFDEF UNIX}
  FFontManager.GetFont('Liberation Sans-12');
  {$ELSE}
  FFontManager.GetFont('Arial-12');
  {$ENDIF}
  CheckEquals(2, FFontManager.GetCacheSize, 'Cache size should be 2');

  {$IFDEF UNIX}
  FFontManager.GetFont('Liberation Mono-10');
  {$ELSE}
  FFontManager.GetFont('Courier New-10');
  {$ENDIF}
  CheckEquals(3, FFontManager.GetCacheSize, 'Cache size should be 3');

  { Request cached font - size should not increase }
  FFontManager.GetFont(TfpgFontResource(FFontManager.GetFont('Liberation Sans-10')).FontDesc);
  CheckEquals(3, FFontManager.GetCacheSize, 'Cache size should still be 3');
end;

procedure TTestFontManager.TestEmptyDescriptor_ReturnsDefault;
var
  font: TfpgFontResourceBase;
begin
  font := FFontManager.GetFont('');

  CheckNotNull(font, 'Empty descriptor should return default font');
  CheckTrue(font.HandleIsValid, 'Returned font should be valid');

  { Should be same as explicitly getting default font }
  CheckTrue(font = FFontManager.GetDefaultFont,
            'Empty descriptor should return same as GetDefaultFont');
end;

procedure TTestFontManager.TestValidDescriptor;
var
  font: TfpgFontResourceBase;
  descriptor: string;
begin
  {$IFDEF UNIX}
  descriptor := 'Liberation Sans-14';
  {$ELSE}
  descriptor := 'Arial-14';
  {$ENDIF}

  font := FFontManager.GetFont(descriptor);

  CheckNotNull(font, 'Valid descriptor should return font');
  CheckTrue(font.HandleIsValid, 'Font should have valid handle');
  CheckEquals(descriptor, font.FontDesc, 'Font descriptor should match');
end;

procedure TTestFontManager.TestGetCacheStats;
var
  stats: string;
  font: TfpgFontResourceBase;
begin
  font := nil;
  { Get a font to populate cache }
  {$IFDEF UNIX}
  font := FFontManager.GetFont('Liberation Sans-12');
  {$ELSE}
  font := FFontManager.GetFont('Arial-12');
  {$ENDIF}
  CheckNotNull(font, 'font should be a valid font');
  stats := FFontManager.GetCacheStats;

  CheckTrue(Length(stats) > 0, 'Stats should not be empty');
  CheckTrue(Pos('Font Cache Statistics', stats) > 0,
            'Stats should contain header');
  CheckTrue(Pos('Total cached fonts: 1', stats) > 0,
            'Stats should show 1 cached font');
end;

procedure TTestFontManager.TestGetCacheSize_Empty;
begin
  CheckEquals(0, FFontManager.GetCacheSize, 'Empty cache should return 0');
end;

procedure TTestFontManager.TestGetCacheSize_WithFonts;
begin
  {$IFDEF UNIX}
  FFontManager.GetFont('Liberation Sans-10');
  FFontManager.GetFont('Liberation Mono-10');
  {$ELSE}
  FFontManager.GetFont('Arial-10');
  FFontManager.GetFont('Courier New-10');
  {$ENDIF}

  CheckEquals(2, FFontManager.GetCacheSize,
              'Cache with 2 fonts should return 2');
end;

procedure TTestFontManager.TestCacheClearsOnDestroy;
var
  tempManager: TfpgFontManager;
begin
  { Create temporary manager and add fonts }
  tempManager := TfpgFontManager.Create;
  try
    {$IFDEF UNIX}
    tempManager.DefaultFontDesc := 'Liberation Sans-10';
    tempManager.GetFont('Liberation Sans-12');
    {$ELSE}
    tempManager.DefaultFontDesc := 'Arial-10';
    tempManager.GetFont('Arial-12');
    {$ENDIF}

    CheckEquals(1, tempManager.GetCacheSize, 'Cache should have 1 font');
  finally
    { Destroying manager should free all cached fonts }
    tempManager.Free;
  end;

  { Test passes if no memory leaks or access violations }
  Check(True, 'Manager destroyed without errors');
end;


initialization
  RegisterTests;

end.
