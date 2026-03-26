{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    Description:
      Platform-independent glyph cache for the hybrid canvas. Wraps
      the AggPas FreeType font engine and font_cache_manager to rasterise
      glyphs as gray8 bitmaps, then composites them directly into a BGRA
      pixel buffer via a tight alpha-blend loop — bypassing the full AGG
      scanline renderer pipeline for maximum performance.

      Font file resolution uses the existing fpg_fontcache infrastructure
      (TFontCacheList / gFontCache) which discovers TTF/OTF files across
      OS-specific search paths and caches family+style -> filepath mappings.
}

unit fpg_glyph_cache;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  fpg_base;

type

  { TGlyphCache — cached FreeType bitmap glyph renderer.

    Rasterises glyphs once via FreeType into gray8 alpha-coverage bitmaps,
    caches them via agg_font_cache_manager, and composites into a BGRA
    pixel buffer using a direct alpha-blend loop (no AGG renderer pipeline).

    Usage:
      1. Call SetFont() when the font changes (resolves descriptor to file path)
      2. Call DrawText() to render text into the buffer
      3. Call TextWidth() to measure text width }

  TGlyphCache = class(TObject)
  private
    FInitialised: Boolean;
    FCurrentFontDesc: string;
    FCurrentFontPath: string;
    FCurrentSize: double;
    FCurrentBold: Boolean;
    FCurrentItalic: Boolean;
    FAscent: Integer;
    FDescent: Integer;
    FLineHeight: Integer;
    FEnginePtr: Pointer;       { ^font_engine_freetype_int32 }
    FCacheManagerPtr: Pointer;  { ^font_cache_manager }
    procedure EnsureInitialised;
    procedure BlitGlyph(ABuf: PByte; AStride, ABufW, ABufH: Integer;
      AGlyphData: PByte; ADataSize: Cardinal;
      ADestX, ADestY: Integer;
      AR, AG, AB: Byte;
      AClipX1, AClipY1, AClipX2, AClipY2: Integer);
    function ResolveFontPath(const AFontDesc: string;
      out ASize: double; out ABold, AItalic: Boolean): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetFont(AFontRes: TfpgFontResourceBase);
    { DrawText renders at baseline Y — caller must add Ascent to convert
      from top-of-text to baseline. }
    procedure DrawText(ABuf: PByte; AStride, ABufW, ABufH: Integer;
      AX, AY: Integer; const AText: string; AColor: TfpgColor); overload;
    procedure DrawText(ABuf: PByte; AStride, ABufW, ABufH: Integer;
      AX, AY: Integer; const AText: string; AColor: TfpgColor;
      AClipX1, AClipY1, AClipX2, AClipY2: Integer); overload;
    function TextWidth(const AText: string): Integer;
    { Font metrics from the same FreeType instance that renders glyphs.
      Guaranteed consistent with rendered output. }
    property Ascent: Integer read FAscent;
    property Descent: Integer read FDescent;
    property LineHeight: Integer read FLineHeight;
  end;


implementation

uses
  fpg_main,
  fpg_fontcache,
  fpg_stringutils,
  agg_basics,
  agg_font_freetype,
  agg_font_freetype_lib,
  agg_font_engine,
  agg_font_cache_manager;


type
  PFontEngine = ^font_engine_freetype_int32;
  PCacheManager = ^font_cache_manager;


{ Helper: read a little-endian int32 from serialised scanline data }
function ReadInt32(var p: PByte): Int32;
begin
  Result := Int32(p[0]) or (Int32(p[1]) shl 8) or
            (Int32(p[2]) shl 16) or (Int32(p[3]) shl 24);
  Inc(p, 4);
end;


{ TGlyphCache }

constructor TGlyphCache.Create;
begin
  inherited Create;
  FInitialised := False;
  FCurrentFontDesc := '';
  FCurrentFontPath := '';
  FCurrentSize := 0;
  FCurrentBold := False;
  FAscent := 0;
  FDescent := 0;
  FCurrentItalic := False;
  FEnginePtr := nil;
  FCacheManagerPtr := nil;
end;

destructor TGlyphCache.Destroy;
begin
  if FInitialised then
  begin
    PCacheManager(FCacheManagerPtr)^.Destruct;
    PFontEngine(FEnginePtr)^.Destruct;
    FreeMem(FCacheManagerPtr);
    FreeMem(FEnginePtr);
  end;
  inherited Destroy;
end;

procedure TGlyphCache.EnsureInitialised;
begin
  if FInitialised then
    Exit;
  FEnginePtr := AllocMem(SizeOf(font_engine_freetype_int32));
  FCacheManagerPtr := AllocMem(SizeOf(font_cache_manager));
  PFontEngine(FEnginePtr)^.Construct;
  PCacheManager(FCacheManagerPtr)^.Construct(font_engine_ptr(FEnginePtr));
  FInitialised := True;
end;

function TGlyphCache.ResolveFontPath(const AFontDesc: string;
  out ASize: double; out ABold, AItalic: Boolean): string;
var
  fnt: TFontCacheItem;
  i: Integer;
  facename: string;
  cp: Integer;
  c: char;
  token: string;
  prop: string;

  function NextC: char;
  begin
    Inc(cp);
    if cp > Length(AFontDesc) then
      c := #0
    else
      c := AFontDesc[cp];
    Result := c;
  end;

  procedure NextToken;
  begin
    token := '';
    while (c <> #0) and (c in [' ', 'a'..'z', 'A'..'Z', '_', '0'..'9', '.']) do
    begin
      token := token + c;
      NextC;
    end;
  end;

begin
  Result := '';
  ASize := 10;
  ABold := False;
  AItalic := False;

  { Parse font descriptor: FamilyName-Size[:bold][:italic] }
  cp := 0;
  NextC;
  NextToken;
  facename := token;

  { Substitute common bitmap font names to outline equivalents }
  if CompareText(facename, 'Helv') = 0 then
    facename := FPG_DEFAULT_SANS
  else if CompareText(facename, 'Helvetica') = 0 then
    facename := FPG_DEFAULT_SANS
  else if CompareText(facename, 'Tms Rmn') = 0 then
    facename := 'Times New Roman'
  else if CompareText(facename, 'Times') = 0 then
    facename := 'Times New Roman'
  else if CompareText(facename, 'System Proportional') = 0 then
    facename := FPG_DEFAULT_SANS
  else if CompareText(facename, 'System Monospaced') = 0 then
    facename := FPG_DEFAULT_FIXED
  else if CompareText(facename, 'System VIO') = 0 then
    facename := FPG_DEFAULT_FIXED
  else if CompareText(facename, 'Courier') = 0 then
    facename := 'Courier New'
  else if CompareText(facename, 'Monospace') = 0 then
    facename := FPG_DEFAULT_FIXED;

  if c = '-' then
  begin
    NextC;
    NextToken;
    ASize := StrToIntDef(token, 10);
  end;

  while c = ':' do
  begin
    NextC;
    NextToken;
    prop := UpperCase(token);
    if prop = 'BOLD' then
      ABold := True
    else if prop = 'ITALIC' then
      AItalic := True;
    { Skip '=' value if present }
    if c = '=' then
    begin
      NextC;
      NextToken;
    end;
  end;

  { Look up in font cache }
  fnt := TFontCacheItem.Create('');
  try
    fnt.FamilyName := facename;
    if ABold then
      fnt.IsBold := True;
    if AItalic then
      fnt.IsItalic := True;
    i := gFontCache.Find(fnt);
    if i >= 0 then
      Result := gFontCache.Items[i].FileName
    else
    begin
      { Fallback to default sans }
      fnt.FamilyName := FPG_DEFAULT_SANS;
      fnt.StyleFlags := 0;
      i := gFontCache.Find(fnt);
      if i >= 0 then
        Result := gFontCache.Items[i].FileName;
    end;
  finally
    fnt.Free;
  end;
end;

procedure TGlyphCache.SetFont(AFontRes: TfpgFontResourceBase);
var
  desc: string;
  fontpath: string;
  sz: double;
  bold, italic: Boolean;
begin
  if not Assigned(AFontRes) then
    Exit;

  desc := AFontRes.FontDesc;
  if desc = FCurrentFontDesc then
    Exit;  { Same font, nothing to do }

  EnsureInitialised;

  fontpath := ResolveFontPath(desc, sz, bold, italic);
  if fontpath = '' then
    Exit;  { Font not found }

  if (fontpath <> FCurrentFontPath) or (sz <> FCurrentSize) or
     (bold <> FCurrentBold) or (italic <> FCurrentItalic) then
  begin
    PFontEngine(FEnginePtr)^.load_font(
      PChar(fontpath), 0, glyph_ren_agg_gray8);
    PFontEngine(FEnginePtr)^.hinting_(True);
    PFontEngine(FEnginePtr)^.flip_y_(True);
    PFontEngine(FEnginePtr)^.height_(
      sz * fpgApplication.Screen_dpi / 72);

    { Read metrics from face->size->metrics (populated by FT_Set_Pixel_Sizes).
      These are 26.6 fixed-point values scaled by units_per_EM, which matches
      how X11/Xft computes ascent and descent.

      The AGG wrapper's _ascender/_descender use a different scaling:
        face.ascender * pixel_height / face.height
      where face.height can differ from units_per_EM. This produces
      values that are too small, causing squashed line spacing. }
    FAscent  := (PFontEngine(FEnginePtr)^.m_cur_face^.size^.metrics.ascender + 63) shr 6;
    FDescent := (-PFontEngine(FEnginePtr)^.m_cur_face^.size^.metrics.descender + 63) shr 6;
    FLineHeight := (PFontEngine(FEnginePtr)^.m_cur_face^.size^.metrics.height + 63) shr 6;

    FCurrentFontPath := fontpath;
    FCurrentSize := sz;
    FCurrentBold := bold;
    FCurrentItalic := italic;
  end;

  FCurrentFontDesc := desc;
end;

procedure TGlyphCache.BlitGlyph(ABuf: PByte; AStride, ABufW, ABufH: Integer;
  AGlyphData: PByte; ADataSize: Cardinal;
  ADestX, ADestY: Integer;
  AR, AG, AB: Byte;
  AClipX1, AClipY1, AClipX2, AClipY2: Integer);
var
  p: PByte;
  minX, minY, maxX, maxY: Int32;
  slSize, slY, numSpans: Int32;
  spanX, spanLen: Int32;
  cover: Byte;
  row: PByte;
  px: PByte;
  srcAlpha, invAlpha: Cardinal;
  drawX, drawY: Integer;
  i, clipLeft, clipRight, clipStart: Integer;
begin
  if (AGlyphData = nil) or (ADataSize = 0) then
    Exit;

  p := AGlyphData;

  { Read bounding box header: min_x, min_y, max_x, max_y }
  minX := ReadInt32(p);
  minY := ReadInt32(p);
  maxX := ReadInt32(p);
  maxY := ReadInt32(p);

  { Iterate serialised scanlines }
  while PtrUInt(p) < PtrUInt(AGlyphData) + ADataSize do
  begin
    slSize := ReadInt32(p);     { scanline size in bytes (including this field) }
    slY := ReadInt32(p);        { Y coordinate of this scanline }
    numSpans := ReadInt32(p);   { number of spans }

    drawY := ADestY + slY;

    { Skip scanlines outside clip rect vertically }
    if (drawY < AClipY1) or (drawY >= AClipY2) then
    begin
      { Skip past remaining span data.
        slSize includes its own 4 bytes, and we already read Y and numSpans (8 bytes),
        so remaining = slSize - 12 }
      Inc(p, slSize - 12);
      Continue;
    end;

    row := ABuf + drawY * AStride;

    while numSpans > 0 do
    begin
      spanX := ReadInt32(p);
      spanLen := ReadInt32(p);

      drawX := ADestX + spanX;

      if spanLen < 0 then
      begin
        { Solid span: single coverage value, |spanLen| pixels }
        cover := p^;
        Inc(p, 1);
        spanLen := -spanLen;

        { Clip horizontally against clip rect }
        clipLeft := drawX;
        clipRight := drawX + spanLen;
        if clipLeft < AClipX1 then clipLeft := AClipX1;
        if clipRight > AClipX2 then clipRight := AClipX2;

        if clipLeft < clipRight then
        begin
          srcAlpha := Cardinal(cover);
          invAlpha := 255 - srcAlpha;
          for i := clipLeft to clipRight - 1 do
          begin
            px := row + i * 4;  { BGRA pixel }
            px[0] := Byte((Cardinal(AB) * srcAlpha + Cardinal(px[0]) * invAlpha + 127) div 255);
            px[1] := Byte((Cardinal(AG) * srcAlpha + Cardinal(px[1]) * invAlpha + 127) div 255);
            px[2] := Byte((Cardinal(AR) * srcAlpha + Cardinal(px[2]) * invAlpha + 127) div 255);
          end;
        end;
      end
      else
      begin
        { Variable span: one coverage byte per pixel }
        clipStart := 0;
        clipLeft := drawX;
        clipRight := drawX + spanLen;
        if clipLeft < AClipX1 then
        begin
          clipStart := AClipX1 - clipLeft;
          clipLeft := AClipX1;
        end;
        if clipRight > AClipX2 then
          clipRight := AClipX2;

        if clipLeft < clipRight then
        begin
          for i := clipStart to clipStart + (clipRight - clipLeft) - 1 do
          begin
            cover := (p + i)^;
            if cover > 0 then
            begin
              px := row + (drawX + i) * 4;  { BGRA pixel }
              srcAlpha := Cardinal(cover);
              invAlpha := 255 - srcAlpha;
              px[0] := Byte((Cardinal(AB) * srcAlpha + Cardinal(px[0]) * invAlpha + 127) div 255);
              px[1] := Byte((Cardinal(AG) * srcAlpha + Cardinal(px[1]) * invAlpha + 127) div 255);
              px[2] := Byte((Cardinal(AR) * srcAlpha + Cardinal(px[2]) * invAlpha + 127) div 255);
            end;
          end;
        end;
        Inc(p, spanLen);
      end;

      Dec(numSpans);
    end;
  end;
end;

procedure TGlyphCache.DrawText(ABuf: PByte; AStride, ABufW, ABufH: Integer;
  AX, AY: Integer; const AText: string; AColor: TfpgColor);
begin
  { Default: clip to full buffer bounds }
  DrawText(ABuf, AStride, ABufW, ABufH, AX, AY, AText, AColor,
    0, 0, ABufW, ABufH);
end;

procedure TGlyphCache.DrawText(ABuf: PByte; AStride, ABufW, ABufH: Integer;
  AX, AY: Integer; const AText: string; AColor: TfpgColor;
  AClipX1, AClipY1, AClipX2, AClipY2: Integer);
var
  engine: PFontEngine;
  cache: PCacheManager;
  glyph: glyph_cache_ptr;
  startX, startY: double;
  rgb: TfpgColor;
  r, g, b: Byte;
  str_: PChar;
  charLen: int;
  charId: int32u;
  first: Boolean;
begin
  if (AText = '') or (ABuf = nil) or not FInitialised then
    Exit;

  engine := PFontEngine(FEnginePtr);
  cache := PCacheManager(FCacheManagerPtr);

  { Clamp clip rect to buffer bounds }
  if AClipX1 < 0 then AClipX1 := 0;
  if AClipY1 < 0 then AClipY1 := 0;
  if AClipX2 > ABufW then AClipX2 := ABufW;
  if AClipY2 > ABufH then AClipY2 := ABufH;

  { Resolve named colours and extract RGB }
  rgb := fpgColorToRGB(AColor);
  r := (rgb shr 16) and $FF;
  g := (rgb shr 8) and $FF;
  b := rgb and $FF;

  { AY is the baseline Y — caller has already added Ascent. }
  startX := AX;
  startY := AY;

  { Iterate UTF-8 characters }
  str_ := PChar(AText);
  first := True;

  while str_^ <> #0 do
  begin
    charId := UTF8CharToUnicode(str_, charLen);
    Inc(str_, charLen);

    glyph := cache^.glyph(charId);
    if glyph <> nil then
    begin
      if not first then
        cache^.add_kerning(@startX, @startY)
      else
        first := False;

      { Only handle gray8 bitmap data (the fast path) }
      if glyph^.data_type = glyph_data_gray8 then
      begin
        cache^.init_embedded_adaptors(glyph, startX, startY);
        BlitGlyph(ABuf, AStride, ABufW, ABufH,
          glyph^.data, glyph^.data_size,
          Trunc(startX), Trunc(startY),
          r, g, b,
          AClipX1, AClipY1, AClipX2, AClipY2);
      end;

      startX := startX + glyph^.advance_x;
      startY := startY + glyph^.advance_y;
    end;
  end;
end;

function TGlyphCache.TextWidth(const AText: string): Integer;
var
  cache: PCacheManager;
  glyph: glyph_cache_ptr;
  x, y: double;
  str_: PChar;
  charLen: int;
  charId: int32u;
  first: Boolean;
begin
  Result := 0;
  if (AText = '') or not FInitialised then
    Exit;

  cache := PCacheManager(FCacheManagerPtr);
  x := 0;
  y := 0;
  first := True;
  str_ := PChar(AText);

  while str_^ <> #0 do
  begin
    charId := UTF8CharToUnicode(str_, charLen);
    Inc(str_, charLen);

    glyph := cache^.glyph(charId);
    if glyph <> nil then
    begin
      if not first then
        cache^.add_kerning(@x, @y)
      else
        first := False;
      x := x + glyph^.advance_x;
      y := y + glyph^.advance_y;
    end;
  end;

  Result := Trunc(x);
end;


end.
