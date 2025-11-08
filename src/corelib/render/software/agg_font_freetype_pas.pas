//----------------------------------------------------------------------------
// Anti-Grain Geometry - Version 2.4 (Public License)
// Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)
//
// FreeType 1 Pascal Adapter
// Copyright (c) 2025 Graeme Geldenhuys
//
// Permission to copy, use, modify, sell and distribute this software
// is granted provided this copyright notice appears in all copies.
// This software is provided "as is" without express or implied
// warranty, and with no claim as to its suitability for any purpose.
//
//----------------------------------------------------------------------------
// This unit provides a FreeType 2 compatible API using the FreeType 1
// Pascal implementation. It acts as an adapter between the FT2 API used
// by aggpas and the FT1 (TT_*) API provided by the Pascal FreeType.
//----------------------------------------------------------------------------

unit agg_font_freetype_pas;

{$I agg_mode.inc}

interface

uses
  agg_basics,
  SysUtils,
  LazFreeType,
  TTTypes;

type
  FT_Encoding = array[0..3] of char;

const
  FT_CURVE_TAG_ON    = 1;
  FT_CURVE_TAG_CONIC = 0;
  FT_CURVE_TAG_CUBIC = 2;

  FT_FACE_FLAG_SCALABLE = 1 shl 0;
  FT_FACE_FLAG_FIXED_SIZES = 1 shl 1;
  FT_FACE_FLAG_FIXED_WIDTH = 1 shl 2;
  FT_FACE_FLAG_SFNT = 1 shl 3;
  FT_FACE_FLAG_HORIZONTAL = 1 shl 4;
  FT_FACE_FLAG_VERTICAL = 1 shl 5;
  FT_FACE_FLAG_KERNING = 1 shl 6;
  FT_FACE_FLAG_FAST_GLYPHS = 1 shl 7;
  FT_FACE_FLAG_MULTIPLE_MASTERS = 1 shl 8;
  FT_FACE_FLAG_GLYPH_NAMES = 1 shl 9;
  FT_FACE_FLAG_EXTERNAL_STREAM = 1 shl 10;

  FT_STYLE_FLAG_ITALIC = 1 shl 0;
  FT_STYLE_FLAG_BOLD = 1 shl 1;

  FT_LOAD_DEFAULT =          $0000;
  FT_LOAD_NO_SCALE =         $0001;
  FT_LOAD_NO_HINTING =       $0002;
  FT_LOAD_RENDER =           $0004;
  FT_LOAD_NO_BITMAP =        $0008;
  FT_LOAD_VERTICAL_LAYOUT =  $0010;
  FT_LOAD_FORCE_AUTOHINT =   $0020;
  FT_LOAD_CROP_BITMAP =      $0040;
  FT_LOAD_PEDANTIC =         $0080;
  FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH = $0200;
  FT_LOAD_NO_RECURSE =       $0400;
  FT_LOAD_IGNORE_TRANSFORM = $0800;
  FT_LOAD_MONOCHROME =       $1000;
  FT_LOAD_LINEAR_DESIGN =    $2000;

  ft_glyph_format_none      = $00000000;
  ft_glyph_format_composite = $636F6D70; //comp
  ft_glyph_format_bitmap    = $62697473; //bits
  ft_glyph_format_outline   = $6F75746C; //outl
  ft_glyph_format_plotter   = $706C6F74; //plot

  FT_ENCODING_NONE : FT_Encoding = (#0 ,#0 ,#0 ,#0 );
  FT_ENCODING_MS_SYMBOL : FT_Encoding = 'symb';
  FT_ENCODING_UNICODE : FT_Encoding = 'unic';
  FT_ENCODING_MS_SJIS : FT_Encoding = 'sjis';
  FT_ENCODING_MS_GB2312 : FT_Encoding = 'gb  ';
  FT_ENCODING_MS_BIG5 : FT_Encoding = 'big5';
  FT_ENCODING_MS_WANSUNG : FT_Encoding = 'wans';
  FT_ENCODING_MS_JOHAB : FT_Encoding = 'joha';
  FT_ENCODING_ADOBE_STANDARD : FT_Encoding = 'ADOB';
  FT_ENCODING_ADOBE_EXPERT : FT_Encoding = 'ADBE';
  FT_ENCODING_ADOBE_CUSTOM : FT_Encoding = 'ADBC';
  FT_ENCODING_ADOBE_LATIN_1 : FT_Encoding = 'lat1';
  FT_ENCODING_OLD_LATIN_2 : FT_Encoding = 'lat2';
  FT_ENCODING_APPLE_ROMAN : FT_Encoding = 'armn';

  ft_glyph_bbox_unscaled  = 0;
  ft_glyph_bbox_subpixels = 0;
  ft_glyph_bbox_gridfit   = 1;
  ft_glyph_bbox_truncate  = 2;
  ft_glyph_bbox_pixels    = 3;

  FT_KERNING_DEFAULT  = 0;
  FT_KERNING_UNFITTED = 1;
  FT_KERNING_UNSCALED = 2;

  // TrueType 'kern' table tag
  TTAG_kern = $6B65726E; // 'kern' in hex

type
  // Kerning pair for caching
  TKernPair = record
    left: Word;
    right: Word;
    value: SmallInt;
  end;
  PKernPair = ^TKernPair;

  TKernPairArray = array[0..32767] of TKernPair;
  PKernPairArray = ^TKernPairArray;

  // Kerning table cache
  TKernTable = record
    pairs: PKernPair;
    num_pairs: Integer;
    loaded: Boolean;
    sorted: Boolean;  // True if pairs are sorted for binary search
  end;

  // Character index cache entry
  TCharIndexCacheEntry = record
    charcode: Cardinal;
    glyph_index: Word;
    used: Boolean;
  end;

  // Character index cache (simple hash table)
  TCharIndexCache = record
    entries: array[0..255] of TCharIndexCacheEntry;  // 256 entries
    hits: Integer;
    misses: Integer;
  end;
  FT_Bool = boolean;
  FT_FWord = smallint;
  FT_UFWord = word;
  FT_Char = char;
  FT_Byte = byte;
  FT_String = char;
  FT_Short = smallint;
  FT_UShort = word;
  FT_Int = longint;
  FT_UInt = longword;
  FT_Int32   = longint;
  {$if defined(cpu64) and not(defined(win64) and defined(cpux86_64))}
  FT_Long = int64;
  FT_ULong = qword;
  FT_Pos = int64;
  {$ELSE}
  FT_Long = longint;
  FT_ULong = longword;
  FT_Pos = longint;
  {$ENDIF}
  FT_F2Dot14 = smallint;
  FT_F26Dot6 = longint;
  FT_Fixed = FT_Long;
  FT_Error = longint;
  FT_Pointer = pointer;

  FT_Byte_ptr  = ^FT_Byte;
  FT_Short_ptr = ^FT_Short;

  FT_Render_Mode = (
    FT_RENDER_MODE_NORMAL,
    FT_RENDER_MODE_LIGHT,
    FT_RENDER_MODE_MONO,
    FT_RENDER_MODE_LCD,
    FT_RENDER_MODE_LCD_V,
    FT_RENDER_MODE_MAX
  );

  FT_Library_ptr_ptr = ^FT_Library_ptr;
  FT_Library_ptr = ^FT_Library;
  FT_Library = record
    initialized: Boolean;
  end;

  FT_Subglyph_ptr = ^FT_Subglyph;
  FT_Subglyph = record
  end;

  FT_Bitmap_Size = record
    height,
    width: FT_Short;
  end;

  AFT_Bitmap_Size = array [0..1023] of FT_Bitmap_Size;
  FT_Bitmap_Size_ptr = ^AFT_Bitmap_Size;

  FT_Charmap_ptr = ^FT_Charmap;
  FT_Charmap_ptr_ptr = ^FT_Charmap_ptr;

  FT_Generic_Finalizer = procedure(AnObject: pointer);

  FT_Generic = record
    data: pointer;
    finalizer: FT_Generic_Finalizer;
  end;

  FT_BBox_ptr = ^FT_BBox;
  FT_BBox = record
    xMin,
    yMin,
    xMax,
    yMax: FT_Pos;
  end;

  FT_Vector_ptr = ^FT_Vector;
  FT_Vector = record
    x,
    y: FT_Pos;
  end;

  FT_Bitmap_ptr = ^FT_Bitmap;
  FT_Bitmap = record
    rows,
    width,
    pitch: FT_Int;
    buffer: pointer;
    num_grays: FT_Short;
    pixel_mode,
    palette_mode: char;
    palette: pointer;
  end;

  FT_Outline_ptr = ^FT_Outline;
  FT_Outline = record
    n_contours,
    n_points: FT_Short;
    points: array of FT_Vector;
    tags: array of Char;
    contours: array of FT_Short;
    flags: FT_Int;
  end;

  FT_Glyph_Metrics = record
    width,
    height,
    horiBearingX,
    horiBearingY,
    horiAdvance,
    vertBearingX,
    vertBearingY,
    vertAdvance: FT_Pos;
  end;

  FT_Face_ptr_ptr = ^FT_Face_ptr;
  FT_Face_ptr = ^FT_Face;

  FT_GlyphSlot_ptr = ^FT_GlyphSlot;
  FT_GlyphSlot = record
    alibrary: FT_Library_ptr;
    face: FT_Face_ptr;
    next: FT_GlyphSlot_ptr;
    flags: FT_UInt;
    generic: FT_Generic;
    metrics: FT_Glyph_Metrics;
    linearHoriAdvance,
    linearVertAdvance: FT_Fixed;
    advance: FT_Vector;
    format: longword;
    bitmap: FT_Bitmap;
    bitmap_left,
    bitmap_top: FT_Int;
    outline: FT_Outline;
    num_subglyphs: FT_UInt;
    subglyphs: FT_SubGlyph_ptr;
    control_data: pointer;
    control_len: longint;
    other: pointer;
  end;

  FT_Size_Metrics = record
    x_ppem,
    y_ppem: FT_UShort;
    x_scale,
    y_scale: FT_Fixed;
    ascender,
    descender,
    height,
    max_advance: FT_Pos;
  end;

  FT_Size_ptr = ^FT_Size;
  FT_Size = record
    face: FT_Face_ptr;
    generic: FT_Generic;
    metrics: FT_Size_Metrics;
  end;

  FT_Face = record
    num_faces,
    face_index,
    face_flags,
    style_flags,
    num_glyphs: FT_Long;
    family_name,
    style_name: PChar;
    num_fixed_sizes: FT_Int;
    available_sizes: FT_Bitmap_Size_ptr;
    num_charmaps: FT_Int;
    charmaps: FT_CharMap_ptr_ptr;
    generic: FT_Generic;
    bbox: FT_BBox;
    units_per_EM: FT_UShort;
    ascender,
    descender,
    height,
    max_advance_width,
    max_advance_height,
    underline_position,
    underline_thickness: FT_Short;
    glyph: FT_GlyphSlot_ptr;
    size: FT_Size_ptr;
    charmap: FT_CharMap_ptr;

    // Internal FT1 handles
    tt_face: TT_Face;
    tt_instance: TT_Instance;
    tt_glyph: TT_Glyph;

    // Kerning cache
    kern_table: TKernTable;

    // Character index cache
    char_index_cache: TCharIndexCache;
  end;

  FT_Charmap = record
    face: FT_Face_ptr;
    encoding: FT_Encoding;
    platform_id,
    encoding_id: FT_UShort;
  end;

{ GLOBAL PROCEDURES }
function FT_CURVE_TAG(flag: char): char;
function FT_IS_SCALABLE(face: FT_Face_ptr): boolean;
function FT_HAS_KERNING(face: FT_Face_ptr): boolean;

function FT_Init_FreeType(var alibrary: FT_Library_ptr): FT_Error;
function FT_Done_FreeType(alibrary: FT_Library_ptr): FT_Error;
function FT_Attach_File(face: FT_Face_ptr; filepathname: PChar): FT_Error;
function FT_New_Memory_Face(library_: FT_Library_ptr; file_base: FT_Byte_ptr;
  file_size, face_index: FT_Long; var aface: FT_Face_ptr): FT_Error;
function FT_New_Face(library_: FT_Library_ptr; filepathname: PChar;
  face_index: FT_Long; var aface: FT_Face_ptr): FT_Error;
function FT_Done_Face(face: FT_Face_ptr): FT_Error;
function FT_Select_Charmap(face: FT_Face_ptr; encoding: FT_Encoding): FT_Error;
function FT_Get_Char_Index(face: FT_Face_ptr; charcode: FT_ULong): FT_UInt;
function FT_Load_Glyph(face: FT_Face_ptr; glyph_index: FT_UInt;
  load_flags: FT_Int32): FT_Error;
function FT_Render_Glyph(slot: FT_GlyphSlot_ptr; render_mode: FT_Render_Mode): FT_Error;
function FT_Get_Kerning(face: FT_Face_ptr; left_glyph, right_glyph,
  kern_mode: FT_UInt; akerning: FT_Vector_ptr): FT_Error;
function FT_Set_Char_Size(face: FT_Face_ptr; char_width, char_height: FT_F26dot6;
  horz_res, vert_res: FT_UInt): FT_Error;
function FT_Set_Pixel_Sizes(face: FT_Face_ptr; pixel_width,
  pixel_height: FT_UInt): FT_Error;

implementation

var
  g_library_initialized: Boolean = False;

{ Helper function to swap bytes for big-endian TrueType format }
function SwapWord(w: Word): Word; inline;
begin
  Result := (w shr 8) or ((w and $FF) shl 8);
end;

function SwapSmallInt(w: SmallInt): SmallInt; inline;
begin
  Result := SmallInt(SwapWord(Word(w)));
end;

function SwapLongInt(l: LongInt): LongInt; inline;
begin
  Result := ((l shr 24) and $FF) or
            ((l shr 8) and $FF00) or
            ((l shl 8) and $FF0000) or
            ((l shl 24) and $FF000000);
end;

{ Compare function for kerning pair sorting }
function CompareKernPairs(const p1, p2: TKernPair): Integer;
var
  key1, key2: Cardinal;
begin
  // Create composite key: (left << 16) | right
  key1 := (Cardinal(p1.left) shl 16) or p1.right;
  key2 := (Cardinal(p2.left) shl 16) or p2.right;

  if key1 < key2 then
    Result := -1
  else if key1 > key2 then
    Result := 1
  else
    Result := 0;
end;

{ Quick sort for kerning pairs }
procedure QuickSortKernPairs(pairs: PKernPair; left, right: Integer);
var
  i, j: Integer;
  pivot, temp: TKernPair;
  pairArray: PKernPairArray;
begin
  if left >= right then
    Exit;

  pairArray := PKernPairArray(pairs);

  i := left;
  j := right;
  pivot := pairArray^[(left + right) div 2];

  repeat
    while CompareKernPairs(pairArray^[i], pivot) < 0 do
      Inc(i);
    while CompareKernPairs(pairArray^[j], pivot) > 0 do
      Dec(j);

    if i <= j then
    begin
      temp := pairArray^[i];
      pairArray^[i] := pairArray^[j];
      pairArray^[j] := temp;
      Inc(i);
      Dec(j);
    end;
  until i > j;

  if left < j then
    QuickSortKernPairs(pairs, left, j);
  if i < right then
    QuickSortKernPairs(pairs, i, right);
end;

{ Binary search for kerning pair - returns kerning value or 0 if not found }
function BinarySearchKern(pairs: PKernPair; num_pairs: Integer;
  left_glyph, right_glyph: Word): SmallInt;
var
  low, high, mid: Integer;
  search_key, mid_key: Cardinal;
  pairArray: PKernPairArray;
begin
  Result := 0;

  if (pairs = nil) or (num_pairs = 0) then
    Exit;

  pairArray := PKernPairArray(pairs);

  // Create composite search key
  search_key := (Cardinal(left_glyph) shl 16) or right_glyph;

  low := 0;
  high := num_pairs - 1;

  while low <= high do
  begin
    mid := (low + high) div 2;
    mid_key := (Cardinal(pairArray^[mid].left) shl 16) or pairArray^[mid].right;

    if mid_key = search_key then
    begin
      Result := pairArray^[mid].value;
      Exit;
    end
    else if mid_key < search_key then
      low := mid + 1
    else
      high := mid - 1;
  end;
end;

{ Initialize character index cache }
procedure InitCharIndexCache(var cache: TCharIndexCache);
var
  i: Integer;
begin
  for i := 0 to 255 do
  begin
    cache.entries[i].used := False;
    cache.entries[i].charcode := 0;
    cache.entries[i].glyph_index := 0;
  end;
  cache.hits := 0;
  cache.misses := 0;
end;

{ Hash function for character index cache }
function CharIndexCacheHash(charcode: Cardinal): Byte; inline;
begin
  // Simple hash: XOR fold the 32-bit charcode into 8 bits
  Result := Byte(charcode xor (charcode shr 8) xor (charcode shr 16) xor (charcode shr 24));
end;

{ Lookup character index in cache }
function LookupCharIndexCache(var cache: TCharIndexCache; charcode: Cardinal;
  out glyph_index: Word): Boolean;
var
  hash: Byte;
begin
  hash := CharIndexCacheHash(charcode);

  if cache.entries[hash].used and (cache.entries[hash].charcode = charcode) then
  begin
    glyph_index := cache.entries[hash].glyph_index;
    Inc(cache.hits);
    Result := True;
  end
  else
  begin
    Inc(cache.misses);
    Result := False;
  end;
end;

{ Store character index in cache }
procedure StoreCharIndexCache(var cache: TCharIndexCache; charcode: Cardinal;
  glyph_index: Word);
var
  hash: Byte;
begin
  hash := CharIndexCacheHash(charcode);
  cache.entries[hash].charcode := charcode;
  cache.entries[hash].glyph_index := glyph_index;
  cache.entries[hash].used := True;
end;

{ Load and parse kerning table from font }
procedure LoadKernTable(face: FT_Face_ptr);
var
  buffer: array of Byte;
  length: LongInt;
  err: TT_Error;
  p: ^Byte;
  version: Word;
  nTables: Word;
  i, j: Integer;
  subVersion: Word;
  subLength: Word;
  coverage: Word;
  format: Word;
  nPairs: Word;
  searchRange: Word;
  entrySelector: Word;
  rangeShift: Word;
  left, right: Word;
  value: SmallInt;
  pair_idx: Integer;
  p_temp: Pointer;
begin
  // Check if already loaded
  if face^.kern_table.loaded then
    Exit;

  // Initialize
  face^.kern_table.pairs := nil;
  face^.kern_table.num_pairs := 0;
  face^.kern_table.loaded := True;

  // Try to get kern table length
  length := 0;
  p_temp := nil;
  err := TT_Get_Font_Data(face^.tt_face, TTAG_kern, 0, p_temp, length);
  if (err <> 0) or (length = 0) then
    Exit; // No kern table

  // Allocate buffer and read kern table
  SetLength(buffer, length);
  err := TT_Get_Font_Data(face^.tt_face, TTAG_kern, 0, buffer[0], length);
  if err <> 0 then
    Exit;

  // Parse kern table header
  if length < 4 then
    Exit;

  p := @buffer[0];

  // Read version (big-endian)
  version := SwapWord(PWord(p)^);
  Inc(p, 2);

  // Read number of subtables
  nTables := SwapWord(PWord(p)^);
  Inc(p, 2);

  // For simplicity, we only support format 0 (the most common)
  // Count total pairs first
  face^.kern_table.num_pairs := 0;

  for i := 0 to nTables - 1 do
  begin
    if PtrUInt(p) - PtrUInt(@buffer[0]) + 6 > PtrUInt(length) then
      Break;

    subVersion := SwapWord(PWord(p)^);
    Inc(p, 2);
    subLength := SwapWord(PWord(p)^);
    Inc(p, 2);
    coverage := SwapWord(PWord(p)^);
    Inc(p, 2);

    format := coverage shr 8;

    // Only process format 0 (kerning pairs)
    if format = 0 then
    begin
      if PtrUInt(p) - PtrUInt(@buffer[0]) + 8 > PtrUInt(length) then
        Break;

      nPairs := SwapWord(PWord(p)^);
      Inc(face^.kern_table.num_pairs, nPairs);

      // Skip to next subtable
      Inc(p, subLength - 6);
    end
    else
      // Skip unknown format
      Inc(p, subLength - 6);
  end;

  if face^.kern_table.num_pairs = 0 then
    Exit;

  // Allocate kerning pairs array
  GetMem(face^.kern_table.pairs, face^.kern_table.num_pairs * SizeOf(TKernPair));

  // Parse again and extract pairs
  p := @buffer[4]; // Skip header
  pair_idx := 0;

  for i := 0 to nTables - 1 do
  begin
    if PtrUInt(p) - PtrUInt(@buffer[0]) + 6 > PtrUInt(length) then
      Break;

    subVersion := SwapWord(PWord(p)^);
    Inc(p, 2);
    subLength := SwapWord(PWord(p)^);
    Inc(p, 2);
    coverage := SwapWord(PWord(p)^);
    Inc(p, 2);

    format := coverage shr 8;

    if format = 0 then
    begin
      if PtrUInt(p) - PtrUInt(@buffer[0]) + 8 > PtrUInt(length) then
        Break;

      nPairs := SwapWord(PWord(p)^);
      Inc(p, 2);
      searchRange := SwapWord(PWord(p)^);
      Inc(p, 2);
      entrySelector := SwapWord(PWord(p)^);
      Inc(p, 2);
      rangeShift := SwapWord(PWord(p)^);
      Inc(p, 2);

      // Read kerning pairs
      for j := 0 to nPairs - 1 do
      begin
        if PtrUInt(p) - PtrUInt(@buffer[0]) + 6 > PtrUInt(length) then
          Break;
        if pair_idx >= face^.kern_table.num_pairs then
          Break;

        left := SwapWord(PWord(p)^);
        Inc(p, 2);
        right := SwapWord(PWord(p)^);
        Inc(p, 2);
        value := SwapSmallInt(PSmallInt(p)^);
        Inc(p, 2);

        PKernPairArray(face^.kern_table.pairs)^[pair_idx].left := left;
        PKernPairArray(face^.kern_table.pairs)^[pair_idx].right := right;
        PKernPairArray(face^.kern_table.pairs)^[pair_idx].value := value;
        Inc(pair_idx);
      end;
    end
    else
      Inc(p, subLength - 6);
  end;

  // Update kerning flag if we found pairs
  if face^.kern_table.num_pairs > 0 then
  begin
    face^.face_flags := face^.face_flags or FT_FACE_FLAG_KERNING;

    // Sort kerning pairs for binary search
    // Most fonts already have sorted pairs, but we ensure it
    QuickSortKernPairs(face^.kern_table.pairs, 0, face^.kern_table.num_pairs - 1);
    face^.kern_table.sorted := True;
  end;
end;

{ FT_CURVE_TAG }
function FT_CURVE_TAG(flag: char): char;
begin
  result := char(int8u(flag) and 3);
end;

{ FT_IS_SCALABLE }
function FT_IS_SCALABLE(face: FT_Face_ptr): boolean;
begin
  Result := (face^.face_flags and FT_FACE_FLAG_SCALABLE) <> 0;
end;

{ FT_HAS_KERNING }
function FT_HAS_KERNING(face: FT_Face_ptr): boolean;
begin
  Result := (face^.face_flags and FT_FACE_FLAG_KERNING) <> 0;
end;

{ FT_Init_FreeType }
function FT_Init_FreeType(var alibrary: FT_Library_ptr): FT_Error;
var
  err: TT_Error;
begin
  if not g_library_initialized then
  begin
    err := TT_Init_FreeType;
    if err = 0 then
    begin
      New(alibrary);
      alibrary^.initialized := True;
      g_library_initialized := True;
      Result := 0;
    end
    else
      Result := err;
  end
  else
  begin
    New(alibrary);
    alibrary^.initialized := True;
    Result := 0;
  end;
end;

{ FT_Done_FreeType }
function FT_Done_FreeType(alibrary: FT_Library_ptr): FT_Error;
begin
  if alibrary <> nil then
  begin
    alibrary^.initialized := False;
    Dispose(alibrary);
  end;

  if g_library_initialized then
  begin
    TT_Done_FreeType;
    g_library_initialized := False;
  end;

  Result := 0;
end;

{ FT_Attach_File }
function FT_Attach_File(face: FT_Face_ptr; filepathname: PChar): FT_Error;
begin
  // FreeType 1 doesn't have a direct equivalent for this
  // This is typically used for additional font files like .afm
  Result := 0;
end;

{ FT_New_Memory_Face }
function FT_New_Memory_Face(library_: FT_Library_ptr; file_base: FT_Byte_ptr;
  file_size, face_index: FT_Long; var aface: FT_Face_ptr): FT_Error;
begin
  // FreeType 1 Pascal doesn't support memory-based faces in the same way
  // This would need to be implemented by writing to a temp file first
  Result := -1; // Not implemented
end;

{ FT_New_Face }
function FT_New_Face(library_: FT_Library_ptr; filepathname: PChar;
  face_index: FT_Long; var aface: FT_Face_ptr): FT_Error;
var
  err: TT_Error;
  face_props: TT_Face_Properties;
  new_face: FT_Face_ptr;
begin
  New(new_face);
  FillChar(new_face^, SizeOf(FT_Face), 0);

  // Open the TrueType face
  if face_index = 0 then
    err := TT_Open_Face(StrPas(filepathname), new_face^.tt_face)
  else
    err := TT_Open_Collection(StrPas(filepathname), face_index, new_face^.tt_face);

  if err <> 0 then
  begin
    Dispose(new_face);
    Result := err;
    Exit;
  end;

  // Get face properties
  err := TT_Get_Face_Properties(new_face^.tt_face, face_props);
  if err <> 0 then
  begin
    TT_Close_Face(new_face^.tt_face);
    Dispose(new_face);
    Result := err;
    Exit;
  end;

  // Fill in FT2-style face structure
  new_face^.num_faces := 1;
  new_face^.face_index := face_index;
  new_face^.face_flags := FT_FACE_FLAG_SCALABLE or FT_FACE_FLAG_HORIZONTAL;
  new_face^.style_flags := 0;
  new_face^.num_glyphs := face_props.num_Glyphs;
  new_face^.units_per_EM := face_props.header.units_Per_EM;
  new_face^.ascender := face_props.horizontal.Ascender;
  new_face^.descender := face_props.horizontal.Descender;
  new_face^.height := face_props.header.units_Per_EM;
  new_face^.max_advance_width := face_props.horizontal.advance_Width_Max;

  // Set up bounding box
  new_face^.bbox.xMin := face_props.header.xMin;
  new_face^.bbox.yMin := face_props.header.yMin;
  new_face^.bbox.xMax := face_props.header.xMax;
  new_face^.bbox.yMax := face_props.header.yMax;

  // Allocate glyph slot
  New(new_face^.glyph);
  FillChar(new_face^.glyph^, SizeOf(FT_GlyphSlot), 0);
  new_face^.glyph^.face := new_face;

  // Create instance (will be configured later)
  err := TT_New_Instance(new_face^.tt_face, new_face^.tt_instance);
  if err <> 0 then
  begin
    Dispose(new_face^.glyph);
    TT_Close_Face(new_face^.tt_face);
    Dispose(new_face);
    Result := err;
    Exit;
  end;

  // Create glyph container
  err := TT_New_Glyph(new_face^.tt_face, new_face^.tt_glyph);
  if err <> 0 then
  begin
    TT_Done_Instance(new_face^.tt_instance);
    Dispose(new_face^.glyph);
    TT_Close_Face(new_face^.tt_face);
    Dispose(new_face);
    Result := err;
    Exit;
  end;

  // Load kerning table
  LoadKernTable(new_face);

  // Initialize character index cache
  InitCharIndexCache(new_face^.char_index_cache);

  aface := new_face;
  Result := 0;
end;

{ FT_Done_Face }
function FT_Done_Face(face: FT_Face_ptr): FT_Error;
begin
  if face = nil then
  begin
    Result := 0;
    Exit;
  end;

  // Clean up TT objects
  if face^.tt_glyph.z <> nil then
    TT_Done_Glyph(face^.tt_glyph);

  if face^.tt_instance.z <> nil then
    TT_Done_Instance(face^.tt_instance);

  if face^.tt_face.z <> nil then
    TT_Close_Face(face^.tt_face);

  // Free kerning table
  if face^.kern_table.pairs <> nil then
    FreeMem(face^.kern_table.pairs);

  // Free glyph slot
  if face^.glyph <> nil then
    Dispose(face^.glyph);

  Dispose(face);
  Result := 0;
end;

{ FT_Select_Charmap }
function FT_Select_Charmap(face: FT_Face_ptr; encoding: FT_Encoding): FT_Error;
begin
  // FreeType 1 uses different character mapping
  // For now, we'll assume Unicode is the default
  Result := 0;
end;

{ FT_Get_Char_Index }
function FT_Get_Char_Index(face: FT_Face_ptr; charcode: FT_ULong): FT_UInt;
var
  map: TT_CharMap;
  glyph_idx: Word;
begin
  // Try cache first
  if LookupCharIndexCache(face^.char_index_cache, charcode, glyph_idx) then
  begin
    Result := glyph_idx;
    Exit;
  end;

  // Cache miss - look up in font
  if TT_Get_CharMap(face^.tt_face, 0, map) = 0 then
  begin
    glyph_idx := TT_Char_Index(map, charcode);
    // Store in cache for next time
    StoreCharIndexCache(face^.char_index_cache, charcode, glyph_idx);
    Result := glyph_idx;
  end
  else
    Result := 0;
end;

{ FT_Load_Glyph }
function FT_Load_Glyph(face: FT_Face_ptr; glyph_index: FT_UInt;
  load_flags: FT_Int32): FT_Error;
var
  err: TT_Error;
  load_flag: Integer;
  outline: TT_Outline;
  metrics: TT_Glyph_Metrics;
  i: Integer;
begin
  // Map FT2 load flags to FT1 load flags
  load_flag := 0;
  if (load_flags and FT_LOAD_NO_SCALE) = 0 then
    load_flag := load_flag or TT_Load_Scale_Glyph;
  if (load_flags and FT_LOAD_NO_HINTING) = 0 then
    load_flag := load_flag or TT_Load_Hint_Glyph;

  // Load the glyph
  err := TT_Load_Glyph(face^.tt_instance, face^.tt_glyph, glyph_index, load_flag);
  if err <> 0 then
  begin
    Result := err;
    Exit;
  end;

  // Get glyph outline
  TT_Get_Glyph_Outline(face^.tt_glyph, outline);

  // Get glyph metrics
  TT_Get_Glyph_Metrics(face^.tt_glyph, metrics);

  // Copy outline to glyph slot
  face^.glyph^.outline.n_contours := outline.n_Contours;
  face^.glyph^.outline.n_points := outline.n_Points;

  // Allocate and copy points
  // TT_Outline.points is TT_Points = ^array of TT_Vector, so we can index directly
  SetLength(face^.glyph^.outline.points, outline.n_Points);
  for i := 0 to outline.n_Points - 1 do
    face^.glyph^.outline.points[i] := outline.points^[i];

  // Allocate and copy tags (flags)
  // TT_Outline.flags is TT_PTouchTable = ^array of byte
  SetLength(face^.glyph^.outline.tags, outline.n_Points);
  for i := 0 to outline.n_Points - 1 do
    face^.glyph^.outline.tags[i] := Char(outline.flags^[i]);

  // Allocate and copy contours (conEnds)
  // TT_Outline.conEnds is TT_PConStarts = ^array of word
  SetLength(face^.glyph^.outline.contours, outline.n_Contours);
  for i := 0 to outline.n_Contours - 1 do
    face^.glyph^.outline.contours[i] := outline.conEnds^[i];

  face^.glyph^.format := ft_glyph_format_outline;

  // Copy metrics
  face^.glyph^.metrics.width := metrics.bbox.xMax - metrics.bbox.xMin;
  face^.glyph^.metrics.height := metrics.bbox.yMax - metrics.bbox.yMin;
  face^.glyph^.metrics.horiBearingX := metrics.bearingX;
  face^.glyph^.metrics.horiBearingY := metrics.bearingY;
  face^.glyph^.metrics.horiAdvance := metrics.advance;
  face^.glyph^.advance.x := metrics.advance;
  face^.glyph^.advance.y := 0;

  Result := 0;
end;

{ FT_Render_Glyph }
function FT_Render_Glyph(slot: FT_GlyphSlot_ptr; render_mode: FT_Render_Mode): FT_Error;
var
  bitmap: TT_Raster_Map;
  metrics: TT_Big_Glyph_Metrics;
  err: TT_Error;
begin
  // Render based on mode
  if render_mode = FT_RENDER_MODE_MONO then
  begin
    err := TT_Get_Glyph_Bitmap(slot^.face^.tt_glyph, bitmap, 0, 0, nil);
    slot^.bitmap.num_grays := 2; // Monochrome
  end
  else
  begin
    err := TT_Get_Glyph_Pixmap(slot^.face^.tt_glyph, bitmap, 0, 0, nil);
    slot^.bitmap.num_grays := 5; // FT1 uses 5-level grayscale
  end;

  if err <> 0 then
  begin
    Result := err;
    Exit;
  end;

  // Copy bitmap data to slot
  slot^.bitmap.rows := bitmap.rows;
  slot^.bitmap.width := bitmap.width;
  slot^.bitmap.pitch := bitmap.cols;
  slot^.bitmap.buffer := bitmap.buffer;
  slot^.bitmap.pixel_mode := chr(5); // Grayscale mode
  slot^.format := ft_glyph_format_bitmap;

  // Get bitmap position
  TT_Get_Glyph_Big_Metrics(slot^.face^.tt_glyph, metrics);
  slot^.bitmap_left := metrics.bbox.xMin div 64;
  slot^.bitmap_top := metrics.bbox.yMax div 64;

  Result := 0;
end;

{ FT_Get_Kerning }
function FT_Get_Kerning(face: FT_Face_ptr; left_glyph, right_glyph,
  kern_mode: FT_UInt; akerning: FT_Vector_ptr): FT_Error;
var
  left16, right16: Word;
  kern_value: SmallInt;
begin
  // Initialize to no kerning
  akerning^.x := 0;
  akerning^.y := 0;

  // Check if kerning table is available
  if (face = nil) or (face^.kern_table.pairs = nil) or
     (face^.kern_table.num_pairs = 0) then
  begin
    Result := 0;
    Exit;
  end;

  // Convert to 16-bit glyph indices
  left16 := Word(left_glyph);
  right16 := Word(right_glyph);

  // Use binary search for fast lookup (O(log n) instead of O(n))
  kern_value := BinarySearchKern(face^.kern_table.pairs,
                                 face^.kern_table.num_pairs,
                                 left16, right16);

  // Convert kern value to 26.6 fixed point format
  // Kern values in the font are typically in font units
  akerning^.x := kern_value;
  akerning^.y := 0;

  Result := 0;
end;

{ FT_Set_Char_Size }
function FT_Set_Char_Size(face: FT_Face_ptr; char_width, char_height: FT_F26dot6;
  horz_res, vert_res: FT_UInt): FT_Error;
var
  err: TT_Error;
begin
  // Set instance resolutions
  err := TT_Set_Instance_Resolutions(face^.tt_instance, horz_res, vert_res);
  if err <> 0 then
  begin
    Result := err;
    Exit;
  end;

  // Set character size
  // FT1 uses points * 64, FT2 uses 26.6 fixed point which is the same
  err := TT_Set_Instance_CharSize(face^.tt_instance, char_height);

  Result := err;
end;

{ FT_Set_Pixel_Sizes }
function FT_Set_Pixel_Sizes(face: FT_Face_ptr; pixel_width,
  pixel_height: FT_UInt): FT_Error;
var
  err: TT_Error;
begin
  // Set pixel sizes
  err := TT_Set_Instance_PixelSizes(face^.tt_instance, pixel_width,
    pixel_height, pixel_height);
  Result := err;
end;

end.
