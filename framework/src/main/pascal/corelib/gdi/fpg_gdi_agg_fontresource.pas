{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    Description:
      Windows/GDI-backed font resource for the hybrid canvas. Provides
      font metrics via GDI (TEXTMETRIC, font_cache_manager advance widths)
      and renders glyphs into the pixel buffer via the AggPas Win32 TrueType
      font engine (agg_font_win32_tt), which extracts glyph outlines through
      GetGlyphOutlineX and rasterises them to gray8 coverage bitmaps. No
      FreeType DLL dependency.

      Registered as AggFontResourceClass by the GDI fpg_interface.pas
      initialisation, replacing TfpgFreeTypeFontResource on Windows.

      Font metrics (ascent, descent, height) come from GDI TEXTMETRIC.
      Text width measurement iterates the font_cache_manager advance widths,
      matching the rendering path exactly. Both paths use the same HFONT
      created internally by font_engine_win32_tt_int32.
}

unit fpg_gdi_agg_fontresource;

{$mode objfpc}{$H+}

interface

uses
  Windows,
  SysUtils,
  fpg_base;

type

  { TfpgGDIAggFontResource — GDI-backed font resource for THybridCanvas.

    A dedicated memory DC (FDC) is created in the constructor and kept alive
    for the lifetime of this object. The AggPas Win32 TT engine uses FDC to
    select fonts and extract glyph outlines; GDI font metrics are read from
    the same DC after font creation, ensuring consistency.

    Thread safety: identical to TfpgFreeTypeFontResource — single-threaded
    use per instance, ownership by TfpgFontManager cache. }

  TfpgGDIAggFontResource = class(TfpgFontResourceBase)
  private
    FDC: HDC;              { Memory DC owned for the lifetime of this resource }
    FMetrics: Windows.TEXTMETRIC;
    FEnginePtr: Pointer;   { ^font_engine_win32_tt_int32 }
    FCachePtr: Pointer;    { ^font_cache_manager }
    FValid: Boolean;
    FFaceName: string;
    FPointSize: double;
    FBold: Boolean;
    FItalic: Boolean;
    procedure ParseFontDesc(const ADesc: string;
      out AFaceName: string; out ASize: double; out ABold, AItalic: Boolean);
    procedure BlitGlyph(ABuf: PByte; AStride, ABufW, ABufH: Integer;
      AGlyphData: PByte; ADataSize: Cardinal;
      ADestX, ADestY: Integer;
      AR, AG, AB: Byte;
      AClipX1, AClipY1, AClipX2, AClipY2: Integer);
  public
    constructor Create(const AFontDesc: string); override;
    destructor  Destroy; override;
    function    HandleIsValid: boolean; override;
    function    GetAscent: integer; override;
    function    GetDescent: integer; override;
    function    GetHeight: integer; override;
    function    GetTextWidth(const txt: string): integer; override;
    procedure   DrawTextToBuffer(ABuf: PByte; AStride, ABufW, ABufH,
                  AX, AY: Integer; const AText: string; AColor: TfpgColor;
                  AClipX1, AClipY1, AClipX2, AClipY2: Integer); override;
  end;


implementation

uses
  agg_basics,
  agg_font_engine,
  agg_font_cache_manager,
  agg_font_win32_tt,
  fpg_main,
  fpg_stringutils;


type
  PFontEngine   = ^font_engine_win32_tt_int32;
  PCacheManager = ^font_cache_manager;


{ Helper: read a little-endian int32 from serialised scanline data }
function ReadInt32(var p: PByte): Int32; inline;
begin
  Result := Int32(p[0]) or (Int32(p[1]) shl 8) or
            (Int32(p[2]) shl 16) or (Int32(p[3]) shl 24);
  Inc(p, 4);
end;


{ TfpgGDIAggFontResource }

procedure TfpgGDIAggFontResource.ParseFontDesc(const ADesc: string;
  out AFaceName: string; out ASize: double; out ABold, AItalic: Boolean);
var
  i, p: Integer;
  rest, tok: string;
begin
  AFaceName := '';
  ASize     := 10;
  ABold     := False;
  AItalic   := False;

  { Locate the '-' that immediately precedes the size digits by scanning
    backward: first '-' whose successor is a digit is the size separator.
    This correctly handles face names like "Liberation Sans" or "Times New Roman"
    where internal hyphens are followed by letters, not digits. }
  p := 0;
  for i := Length(ADesc) downto 1 do
    if (ADesc[i] = '-') and (i < Length(ADesc)) and
       (ADesc[i + 1] in ['0'..'9']) then
    begin
      p := i;
      Break;
    end;

  if p = 0 then
  begin
    AFaceName := Trim(ADesc);
    Exit;
  end;

  AFaceName := Trim(Copy(ADesc, 1, p - 1));
  rest      := Copy(ADesc, p + 1, MaxInt);   { "12:bold:italic" or "12" }

  { Extract size }
  i := Pos(':', rest);
  if i > 0 then
  begin
    ASize := StrToFloatDef(Copy(rest, 1, i - 1), 10);
    rest  := Copy(rest, i, MaxInt);           { ":bold:italic" }
  end
  else
  begin
    ASize := StrToFloatDef(rest, 10);
    rest  := '';
  end;

  { Parse attribute tokens }
  while (rest <> '') and (rest[1] = ':') do
  begin
    Delete(rest, 1, 1);
    i := Pos(':', rest);
    if i > 0 then
    begin
      tok  := UpperCase(Copy(rest, 1, i - 1));
      rest := Copy(rest, i, MaxInt);
    end
    else
    begin
      tok  := UpperCase(rest);
      rest := '';
    end;
    { Ignore key=value pairs (e.g. antialias=false) }
    i := Pos('=', tok);
    if i > 0 then tok := Copy(tok, 1, i - 1);
    if tok = 'BOLD'   then ABold   := True
    else if tok = 'ITALIC' then AItalic := True;
  end;
end;

constructor TfpgGDIAggFontResource.Create(const AFontDesc: string);
var
  dpi:    Integer;
  weight: Integer;
  engine: PFontEngine;
begin
  inherited Create(AFontDesc);
  FValid      := False;
  FDC         := 0;
  FEnginePtr  := nil;
  FCachePtr   := nil;

  ParseFontDesc(AFontDesc, FFaceName, FPointSize, FBold, FItalic);

  { Create a dedicated memory DC for font metric and glyph operations.
    CreateCompatibleDC(0) gives a screen-compatible DC; no window needed. }
  FDC := CreateCompatibleDC(0);
  if FDC = 0 then
    Exit;

  { Initialise the AggPas Win32 TT font engine }
  FEnginePtr := AllocMem(SizeOf(font_engine_win32_tt_int32));
  PFontEngine(FEnginePtr)^.Construct(FDC);

  { Set screen DPI so the engine converts point sizes to pixels:
      pixel_height = MulDiv(point_size, dpi, 72)
    This matches how TfpgGDIFontResource.OpenFontByDesc computes lfHeight. }
  dpi := GetDeviceCaps(FDC, LOGPIXELSY);
  PFontEngine(FEnginePtr)^.resolution_(dpi);

  { GetGlyphOutline returns outlines in Y-up (mathematical) coordinates.
    Flipping Y maps them to screen coordinates (Y-down) so glyphs render
    the right way up. }
  PFontEngine(FEnginePtr)^.flip_y_(True);

  weight := FW_NORMAL;
  if FBold then weight := FW_BOLD;

  { create_font_ creates an HFONT via CreateFont(-h,...), selects it into
    FDC, and caches the handle internally. The font is ready for glyph
    extraction and metric queries after this call. }
  if not PFontEngine(FEnginePtr)^.create_font_(
      PChar(FFaceName),
      glyph_ren_agg_gray8,
      FPointSize,   { point size — engine applies dpi/72 scaling }
      0.0,          { width: 0 = aspect-correct }
      weight,
      FItalic) then
    Exit;

  { Initialise the glyph cache manager (wraps the engine for lazy caching) }
  FCachePtr := AllocMem(SizeOf(font_cache_manager));
  PCacheManager(FCachePtr)^.Construct(font_engine_ptr(FEnginePtr));

  { The engine has selected its HFONT into FDC; read GDI metrics from it.
    These are the authoritative values used for ascent/descent/height. }
  GetTextMetrics(FDC, FMetrics);

  FValid := True;
end;

destructor TfpgGDIAggFontResource.Destroy;
begin
  if Assigned(FCachePtr) then
  begin
    PCacheManager(FCachePtr)^.Destruct;
    FreeMem(FCachePtr);
    FCachePtr := nil;
  end;
  if Assigned(FEnginePtr) then
  begin
    PFontEngine(FEnginePtr)^.Destruct;
    FreeMem(FEnginePtr);
    FEnginePtr := nil;
  end;
  if FDC <> 0 then
  begin
    DeleteDC(FDC);
    FDC := 0;
  end;
  inherited Destroy;
end;

function TfpgGDIAggFontResource.HandleIsValid: boolean;
begin
  Result := FValid;
end;

function TfpgGDIAggFontResource.GetAscent: integer;
begin
  Result := FMetrics.tmAscent;
end;

function TfpgGDIAggFontResource.GetDescent: integer;
begin
  Result := FMetrics.tmDescent;
end;

function TfpgGDIAggFontResource.GetHeight: integer;
begin
  Result := FMetrics.tmHeight;
end;

function TfpgGDIAggFontResource.GetTextWidth(const txt: string): integer;
var
  cache:   PCacheManager;
  glyph:   glyph_cache_ptr;
  x, y:    double;
  str_:    PChar;
  charLen: int;
  charId:  int32u;
  first:   Boolean;
begin
  Result := 0;
  if (txt = '') or not FValid then
    Exit;

  { Sum advance widths through the same cache manager used for rendering —
    measurement and rendering are always consistent. }
  cache := PCacheManager(FCachePtr);
  x     := 0;
  y     := 0;
  first := True;
  str_  := PChar(txt);

  while str_^ <> #0 do
  begin
    charId := UTF8CharToUnicode(str_, charLen);
    Inc(str_, charLen);
    glyph := cache^.glyph(charId);
    if glyph <> nil then
    begin
      if not first then cache^.add_kerning(@x, @y)
      else first := False;
      x := x + glyph^.advance_x;
    end;
  end;

  Result := Trunc(x);
end;

{ BlitGlyph — composite a single gray8 glyph bitmap into a BGRA pixel buffer.

  The glyph data is the serialised scanline format written by AggPas's
  scanline_storage_aa: a 16-byte bounding-box header followed by a sequence
  of scanline records. Each scanline record contains spans; spans are either
  solid (single coverage byte for N pixels) or variable (one coverage byte
  per pixel). This format is identical for both agg_font_freetype and
  agg_font_win32_tt when using glyph_ren_agg_gray8. }
procedure TfpgGDIAggFontResource.BlitGlyph(
  ABuf: PByte; AStride, ABufW, ABufH: Integer;
  AGlyphData: PByte; ADataSize: Cardinal;
  ADestX, ADestY: Integer;
  AR, AG, AB: Byte;
  AClipX1, AClipY1, AClipX2, AClipY2: Integer);
var
  p:                     PByte;
  minX, minY, maxX, maxY: Int32;
  slSize, slY, numSpans: Int32;
  spanX, spanLen:        Int32;
  cover:                 Byte;
  row:                   PByte;
  px:                    PByte;
  srcAlpha, invAlpha:    Cardinal;
  drawX, drawY:          Integer;
  i, clipLeft, clipRight, clipStart: Integer;
begin
  if (AGlyphData = nil) or (ADataSize = 0) then
    Exit;

  p := AGlyphData;

  { 16-byte bounding-box header (unused but must be skipped) }
  minX := ReadInt32(p);
  minY := ReadInt32(p);
  maxX := ReadInt32(p);
  maxY := ReadInt32(p);

  while PtrUInt(p) < PtrUInt(AGlyphData) + ADataSize do
  begin
    slSize   := ReadInt32(p);
    slY      := ReadInt32(p);
    numSpans := ReadInt32(p);

    drawY := ADestY + slY;

    if (drawY < AClipY1) or (drawY >= AClipY2) then
    begin
      { Skip remainder of this scanline: slSize covers all three header
        fields (12 bytes) plus span data, so remaining = slSize - 12. }
      Inc(p, slSize - 12);
      Continue;
    end;

    row := ABuf + drawY * AStride;

    while numSpans > 0 do
    begin
      spanX   := ReadInt32(p);
      spanLen := ReadInt32(p);
      drawX   := ADestX + spanX;

      if spanLen < 0 then
      begin
        { Solid span: single coverage byte, |spanLen| pixels wide }
        cover   := p^;
        Inc(p);
        spanLen := -spanLen;

        clipLeft  := drawX;
        clipRight := drawX + spanLen;
        if clipLeft  < AClipX1 then clipLeft  := AClipX1;
        if clipRight > AClipX2 then clipRight := AClipX2;

        if clipLeft < clipRight then
        begin
          srcAlpha := Cardinal(cover);
          invAlpha := 255 - srcAlpha;
          for i := clipLeft to clipRight - 1 do
          begin
            px    := row + i * 4;
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
        clipLeft  := drawX;
        clipRight := drawX + spanLen;
        if clipLeft < AClipX1 then
        begin
          clipStart := AClipX1 - clipLeft;
          clipLeft  := AClipX1;
        end;
        if clipRight > AClipX2 then clipRight := AClipX2;

        if clipLeft < clipRight then
          for i := clipStart to clipStart + (clipRight - clipLeft) - 1 do
          begin
            cover := (p + i)^;
            if cover > 0 then
            begin
              px       := row + (drawX + i) * 4;
              srcAlpha := Cardinal(cover);
              invAlpha := 255 - srcAlpha;
              px[0] := Byte((Cardinal(AB) * srcAlpha + Cardinal(px[0]) * invAlpha + 127) div 255);
              px[1] := Byte((Cardinal(AG) * srcAlpha + Cardinal(px[1]) * invAlpha + 127) div 255);
              px[2] := Byte((Cardinal(AR) * srcAlpha + Cardinal(px[2]) * invAlpha + 127) div 255);
            end;
          end;
        Inc(p, spanLen);
      end;

      Dec(numSpans);
    end;
  end;
end;

procedure TfpgGDIAggFontResource.DrawTextToBuffer(ABuf: PByte;
  AStride, ABufW, ABufH, AX, AY: Integer; const AText: string;
  AColor: TfpgColor; AClipX1, AClipY1, AClipX2, AClipY2: Integer);
var
  cache:         PCacheManager;
  glyph:         glyph_cache_ptr;
  startX, startY: double;
  rgb:           TfpgColor;
  r, g, b:       Byte;
  str_:          PChar;
  charLen:       int;
  charId:        int32u;
  first:         Boolean;
begin
  if (AText = '') or (ABuf = nil) or not FValid then
    Exit;

  cache := PCacheManager(FCachePtr);

  { Clamp clip rect to buffer bounds }
  if AClipX1 < 0     then AClipX1 := 0;
  if AClipY1 < 0     then AClipY1 := 0;
  if AClipX2 > ABufW then AClipX2 := ABufW;
  if AClipY2 > ABufH then AClipY2 := ABufH;

  rgb := fpgColorToRGB(AColor);
  r   := (rgb shr 16) and $FF;
  g   := (rgb shr  8) and $FF;
  b   :=  rgb         and $FF;

  startX := AX;
  startY := AY;
  str_   := PChar(AText);
  first  := True;

  while str_^ <> #0 do
  begin
    charId := UTF8CharToUnicode(str_, charLen);
    Inc(str_, charLen);

    glyph := cache^.glyph(charId);
    if glyph <> nil then
    begin
      if not first then cache^.add_kerning(@startX, @startY)
      else first := False;

      if glyph^.data_type = glyph_data_gray8 then
        BlitGlyph(ABuf, AStride, ABufW, ABufH,
          glyph^.data, glyph^.data_size,
          Trunc(startX), Trunc(startY),
          r, g, b,
          AClipX1, AClipY1, AClipX2, AClipY2);

      startX := startX + glyph^.advance_x;
      startY := startY + glyph^.advance_y;
    end;
  end;
end;


end.
