{
    Copyright (c) 2026 Graeme Geldenhuys

    This program is part of the fpGUI Toolkit project.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      HVIF (Haiku Vector Icon Format) reader and AggPas renderer for fpGUI.

    Canonical format references (Haiku r1beta5):
      src/libs/icon/flat_icon/FlatIconImporter.cpp  -- binary wire format parser
      src/libs/icon/flat_icon/FlatIconFormat.cpp     -- coord/float24 encoding
      src/libs/icon/flat_icon/PathCommandQueue.cpp   -- path command bit-packing
      src/libs/icon/IconRenderer.cpp                 -- gradient rendering model

    Key encoding facts (from Haiku source, not from secondary descriptions):
      - Magic: 4 bytes $6E $63 $69 $66 ('ficn' as LE uint32)
      - Coordinate: bit7 of first byte = extended flag
          0: coord = byte - 32.0          (range -32..95)
          1: coord = ((byte & $7F)<<8 | low) / 102.0 - 128.0   (range -128..192)
      - Float24: 1 sign bit, 6 exponent bits (bias 32), 17 mantissa bits
      - Gradient transform maps gradient parameter space to icon (64-unit) space.
        For radial gradients: radius in icon space = 64 * sqrt(sx^2 + shy^2)
        where [sx, shy, shx, sy, tx, ty] = the 6 decoded float24 matrix values.

    Rendering notes:
      - Uses standalone Agg2D object (no dependency on THybridCanvas).
      - Pixel format: Agg2D produces BGRA32; TfpgImage stores ARGB.
        On little-endian (x86/x64/ARM64) these are byte-identical.
      - Big-endian targets are NOT supported (requires a channel-swap pass).
      - Gradient rendering: full multi-stop support via a 256-entry pre-computed LUT.
        All intermediate stops are interpolated using their normalised Offset values.
        Diamond/Conic/XY gradient types fall back to the first stop's solid colour.
      - Thread safety: NOT thread-safe. Caller must use from the fpGUI main thread.
      - Image cache: linear scan, 8 entries max per icon instance.
}

unit fpg_hvif;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  fpg_hvif_model,
  fpg_base,
  fpg_main;


{ ==================== Cached image entry ==================== }

type
  { Cached rendered image entry }
  THvifCachedImage = record
    Width, Height: Integer;
    Image: TfpgImage;
  end;


{ ==================== Public API ==================== }

  { THvifIcon — owns the parsed HVIF data and the image cache.
    All returned TfpgImage pointers are owned by the THvifIcon instance.

    Usage:
      icon := THvifIcon.CreateFromFile('open.hvif');
      try
        canvas.DrawImage(icon.GetImage(24, 24), x, y);
      finally
        icon.Free;
      end;
  }
  THvifIcon = class
  private
    FStyles: array of THvifStyle;
    FPaths:  array of THvifPath;
    FShapes: array of THvifShape;
    FCachedImages: array of THvifCachedImage;

    { Low-level byte buffer reader state }
    FData: TBytes;
    FPos:  Integer;

    { Buffer primitives }
    function  ReadByte: Byte;
    function  ReadWord: Word;   { LE }
    function  ReadCoord: Single;
    function  ReadFloat24: Single;

    { Section parsers }
    procedure ParseFrom(const AData: TBytes);
    procedure ParseStyles(ACount: Byte);
    procedure ParsePaths(ACount: Byte);
    procedure ParseShapes(ACount: Byte);

    { Gradient stop colour reader (shared between solid and gradient paths) }
    procedure ReadStopColor(AAlpha, AGray: Boolean; out AColor: THvifColor);

    { Image cache }
    function FindCachedImage(AW, AH: Integer): TfpgImage;

    { Rendering }
    procedure RenderIntoImage(AImg: TfpgImage);

    { Property getters for Styles[], Paths[], Shapes[] }
    function GetStyle(AIndex: Integer): THvifStyle;
    function GetPath(AIndex: Integer): THvifPath;
    function GetShape(AIndex: Integer): THvifShape;

  public
    destructor Destroy; override;

    { Factory class functions — caller must Free the returned instance. }
    class function CreateFromStream(AStream: TStream): THvifIcon;
    class function CreateFromFile(const AFileName: string): THvifIcon;
    class function CreateFromResource(AInstance: THandle;
                                      const AName: string): THvifIcon;

    { Returns a cached TfpgImage rendered at (AWidth × AHeight) pixels.
      The returned image is owned by this THvifIcon — do NOT free it.
      Raises EHvifError if the icon has no renderable shapes. }
    function GetImage(AWidth, AHeight: Integer): TfpgImage;

    { Frees all cached rendered images so they will be re-rendered on next
      GetImage call. Call this after a DPI change. }
    procedure ClearCache;

    { Diagnostic: number of parsed styles/paths/shapes }
    function StyleCount: Integer;
    function PathCount: Integer;
    function ShapeCount: Integer;

    { Raw data accessors for the document model (e.g. TIomDocument.FromHvifArrays).
      Indices are 0-based. Results are deep copies of the internal records. }
    property Styles[AIndex: Integer]: THvifStyle read GetStyle;
    property Paths[AIndex: Integer]:  THvifPath  read GetPath;
    property Shapes[AIndex: Integer]: THvifShape read GetShape;
  end;


implementation

uses
  agg_basics,                   { int8u_ptr = PByte }
  agg_array,                    { array_base }
  agg_color,
  agg_rendering_buffer,
  agg_pixfmt,
  agg_pixfmt_rgba,
  agg_renderer_base,
  agg_scanline_u,
  agg_scanline_bin,
  agg_span_allocator,
  agg_rasterizer_compound_aa,
  agg_renderer_scanline,
  agg_trans_affine,
  agg_path_storage,
  agg_conv_curve,
  agg_conv_stroke,
  agg_math_stroke,
  agg_conv_transform,
  agg_span_gradient,
  agg_span_interpolator_linear;

{$ifdef MSWINDOWS}
const
  RT_RCDATA = PChar(10);
{$endif}


{ ===================================================================
  Byte buffer helpers
  =================================================================== }

function THvifIcon.ReadByte: Byte;
begin
  if FPos >= Length(FData) then
    raise EHvifParseError.CreateFmt(
      'HVIF: unexpected end of data at position %d', [FPos]);
  Result := FData[FPos];
  Inc(FPos);
end;

function THvifIcon.ReadWord: Word;
var
  lo, hi: Byte;
begin
  lo := ReadByte;
  hi := ReadByte;
  Result := lo or (Word(hi) shl 8);
end;

{ Decode one HVIF variable-length coordinate (1 or 2 bytes).
  Source: FlatIconFormat.cpp : read_coord() }
function THvifIcon.ReadCoord: Single;
var
  v, lo: Byte;
  coordValue: Word;
begin
  v := ReadByte;
  if (v and 128) <> 0 then
  begin
    { Extended 2-byte encoding: high bit of first byte set }
    lo := ReadByte;
    v := v and 127;
    coordValue := (Word(v) shl 8) or lo;
    Result := coordValue / 102.0 - 128.0;
  end
  else
    Result := v - 32.0;
end;

{ Decode one HVIF 24-bit custom float.
  Source: FlatIconFormat.cpp : read_float_24()
  Layout: 1 sign bit | 6 exponent bits (bias 32) | 17 mantissa bits }
function THvifIcon.ReadFloat24: Single;
var
  b0, b1, b2: Byte;
  shortVal: LongWord;
  sign: LongWord;
  exponent: Integer;
  mantissa: LongWord;
  U: packed record
       case Integer of
         0: (I: LongWord);
         1: (F: Single);
     end;
begin
  b0 := ReadByte;
  b1 := ReadByte;
  b2 := ReadByte;
  shortVal := (LongWord(b0) shl 16) or (LongWord(b1) shl 8) or LongWord(b2);
  if shortVal = 0 then
    Result := 0.0
  else
  begin
    sign     := (shortVal shr 23) and 1;
    exponent := Integer((shortVal and $7E0000) shr 17) - 32;
    mantissa := (shortVal and $01FFFF) shl 6;
    U.I := (sign shl 31) or (LongWord(exponent + 127) shl 23) or mantissa;
    Result := U.F;
  end;
end;


{ ===================================================================
  Stop colour reader (shared logic for solid and gradient stops)
  AAlpha: true  → alpha byte is present
  AGray:  true  → only one channel stored (replicated to R/G/B)
  Source: FlatIconImporter.cpp : _ReadColorStyle()
  =================================================================== }

procedure THvifIcon.ReadStopColor(AAlpha, AGray: Boolean;
  out AColor: THvifColor);
begin
  if AGray then
  begin
    AColor.R := ReadByte;
    AColor.G := AColor.R;
    AColor.B := AColor.R;
    if AAlpha then
      AColor.A := ReadByte
    else
      AColor.A := 255;
  end
  else
  begin
    AColor.R := ReadByte;
    AColor.G := ReadByte;
    AColor.B := ReadByte;
    if AAlpha then
      AColor.A := ReadByte
    else
      AColor.A := 255;
  end;
end;


{ ===================================================================
  Section parsers
  =================================================================== }

{ Parse the styles section.
  Source: FlatIconImporter.cpp : _ParseStyles() and _ReadGradientStyle() }
procedure THvifIcon.ParseStyles(ACount: Byte);
const
  GRADIENT_FLAG_TRANSFORM  = 1 shl 1;
  GRADIENT_FLAG_NO_ALPHA   = 1 shl 2;
  GRADIENT_FLAG_GRAYS      = 1 shl 4;
var
  i, s: Integer;
  styleType: Byte;
  gradType, gradFlags: Byte;
  stopCount: Byte;
  stopOffset: Byte;
  hasAlpha, isGray: Boolean;
  tagLen: Word;
begin
  SetLength(FStyles, ACount);
  for i := 0 to ACount - 1 do
  begin
    FillChar(FStyles[i], SizeOf(THvifStyle), 0);
    styleType := ReadByte;
    case styleType of
      1: { STYLE_TYPE_SOLID_COLOR: RGBA }
        begin
          FStyles[i].StyleType := hstSolidColor;
          ReadStopColor(True, False, FStyles[i].Color);
        end;

      3: { STYLE_TYPE_SOLID_COLOR_NO_ALPHA: RGB }
        begin
          FStyles[i].StyleType := hstSolidColorNoAlpha;
          ReadStopColor(False, False, FStyles[i].Color);
        end;

      4: { STYLE_TYPE_SOLID_GRAY: K + A }
        begin
          FStyles[i].StyleType := hstSolidGray;
          ReadStopColor(True, True, FStyles[i].Color);
        end;

      5: { STYLE_TYPE_SOLID_GRAY_NO_ALPHA: K only }
        begin
          FStyles[i].StyleType := hstSolidGrayNoAlpha;
          ReadStopColor(False, True, FStyles[i].Color);
        end;

      2: { STYLE_TYPE_GRADIENT }
        begin
          FStyles[i].StyleType := hstGradient;
          gradType  := ReadByte;
          gradFlags := ReadByte;
          stopCount := ReadByte;

          FStyles[i].GradientType := THvifGradientType(gradType);
          FStyles[i].HasGradTransform := (gradFlags and GRADIENT_FLAG_TRANSFORM) <> 0;

          if FStyles[i].HasGradTransform then
          begin
            FStyles[i].GradTransform[0] := ReadFloat24;
            FStyles[i].GradTransform[1] := ReadFloat24;
            FStyles[i].GradTransform[2] := ReadFloat24;
            FStyles[i].GradTransform[3] := ReadFloat24;
            FStyles[i].GradTransform[4] := ReadFloat24;
            FStyles[i].GradTransform[5] := ReadFloat24;
          end
          else
          begin
            { Identity: maps gradient coords 1:1 to icon space }
            FStyles[i].GradTransform[0] := 1.0;
            FStyles[i].GradTransform[1] := 0.0;
            FStyles[i].GradTransform[2] := 0.0;
            FStyles[i].GradTransform[3] := 1.0;
            FStyles[i].GradTransform[4] := 0.0;
            FStyles[i].GradTransform[5] := 0.0;
          end;

          hasAlpha := (gradFlags and GRADIENT_FLAG_NO_ALPHA) = 0;
          isGray   := (gradFlags and GRADIENT_FLAG_GRAYS) <> 0;

          SetLength(FStyles[i].Stops, stopCount);
          for s := 0 to stopCount - 1 do
          begin
            stopOffset := ReadByte;
            FStyles[i].Stops[s].Offset := stopOffset / 255.0;
            ReadStopColor(hasAlpha, isGray, FStyles[i].Stops[s].Color);
          end;
        end;

      else
      begin
        { Unknown style type: read past the tag-length-prefixed block }
        tagLen := ReadWord;
        Inc(FPos, tagLen);
        { Leave style[i] as hstSolidColor with zero color (transparent) }
      end;
    end;
  end;
end;

{ Parse the paths section.
  Source: FlatIconImporter.cpp : _ParsePaths(), read_path_no_curves(),
          read_path_curves(), PathCommandQueue.cpp : Read() }
procedure THvifIcon.ParsePaths(ACount: Byte);
const
  PATH_FLAG_CLOSED       = 1 shl 1;
  PATH_FLAG_USES_COMMANDS = 1 shl 2;
  PATH_FLAG_NO_CURVES     = 1 shl 3;

  PATH_COMMAND_H_LINE = 0;
  PATH_COMMAND_V_LINE = 1;
  PATH_COMMAND_LINE   = 2;
  PATH_COMMAND_CURVE  = 3;
var
  i, p: Integer;
  pathFlags, pointCount: Byte;
  cmdBufSize: Integer;
  cmdBuf: array of Byte;
  cmdByte: Byte;
  cmdPos: Integer;
  cmdBufIdx: Integer;
  cmd: Byte;
  pt: THvifPoint;
  lastX, lastY: Single;
begin
  SetLength(FPaths, ACount);
  for i := 0 to ACount - 1 do
  begin
    pathFlags  := ReadByte;
    pointCount := ReadByte;

    FPaths[i].Closed := (pathFlags and PATH_FLAG_CLOSED) <> 0;
    SetLength(FPaths[i].Points, pointCount);

    if pointCount = 0 then
      Continue;

    if (pathFlags and PATH_FLAG_NO_CURVES) <> 0 then
    begin
      { Line-only path: each point is (x, y), no control points }
      for p := 0 to pointCount - 1 do
      begin
        pt.X := ReadCoord;
        pt.Y := ReadCoord;
        pt.InX := pt.X;  pt.InY := pt.Y;
        pt.OutX := pt.X; pt.OutY := pt.Y;
        FPaths[i].Points[p] := pt;
      end;
    end
    else if (pathFlags and PATH_FLAG_USES_COMMANDS) <> 0 then
    begin
      { Command-encoded path:
        - command buffer comes FIRST: ceil(pointCount/4) bytes, 4 cmds packed per byte
        - then coordinates follow in the main stream (interleaved after unpacking) }
      cmdBufSize := (pointCount + 3) div 4;
      SetLength(cmdBuf, cmdBufSize);
      for p := 0 to cmdBufSize - 1 do
        cmdBuf[p] := ReadByte;

      cmdByte   := 0;
      cmdPos    := 0;
      cmdBufIdx := 0;
      lastX := 0.0;
      lastY := 0.0;

      for p := 0 to pointCount - 1 do
      begin
        { Unpack 2-bit command from cmdBuf }
        if cmdPos = 0 then
        begin
          cmdByte := cmdBuf[cmdBufIdx];
          Inc(cmdBufIdx);
        end;
        cmd := (cmdByte shr cmdPos) and $03;
        Inc(cmdPos, 2);
        if cmdPos = 8 then
          cmdPos := 0;

        case cmd of
          PATH_COMMAND_H_LINE:
            begin
              pt.X := ReadCoord;
              pt.Y := lastY;
              pt.InX := pt.X;  pt.InY := pt.Y;
              pt.OutX := pt.X; pt.OutY := pt.Y;
            end;
          PATH_COMMAND_V_LINE:
            begin
              pt.X := lastX;
              pt.Y := ReadCoord;
              pt.InX := pt.X;  pt.InY := pt.Y;
              pt.OutX := pt.X; pt.OutY := pt.Y;
            end;
          PATH_COMMAND_LINE:
            begin
              pt.X := ReadCoord;
              pt.Y := ReadCoord;
              pt.InX := pt.X;  pt.InY := pt.Y;
              pt.OutX := pt.X; pt.OutY := pt.Y;
            end;
          PATH_COMMAND_CURVE:
            begin
              pt.X    := ReadCoord;
              pt.Y    := ReadCoord;
              pt.InX  := ReadCoord;
              pt.InY  := ReadCoord;
              pt.OutX := ReadCoord;
              pt.OutY := ReadCoord;
            end;
          else
            begin
              pt.X := 0; pt.Y := 0;
              pt.InX := 0; pt.InY := 0;
              pt.OutX := 0; pt.OutY := 0;
            end;
        end;

        lastX := pt.X;
        lastY := pt.Y;
        FPaths[i].Points[p] := pt;
      end;
    end
    else
    begin
      { Cubic-curves path: each point is (point, pointIn, pointOut) }
      for p := 0 to pointCount - 1 do
      begin
        pt.X    := ReadCoord;
        pt.Y    := ReadCoord;
        pt.InX  := ReadCoord;
        pt.InY  := ReadCoord;
        pt.OutX := ReadCoord;
        pt.OutY := ReadCoord;
        FPaths[i].Points[p] := pt;
      end;
    end;
  end;
end;

{ Parse the shapes section.
  Source: FlatIconImporter.cpp : _ParseShapes() and _ReadPathSourceShape() }
procedure THvifIcon.ParseShapes(ACount: Byte);
const
  SHAPE_TYPE_PATH_SOURCE = 10;

  SHAPE_FLAG_TRANSFORM     = 1 shl 1;
  SHAPE_FLAG_HINTING       = 1 shl 2;
  SHAPE_FLAG_LOD_SCALE     = 1 shl 3;
  SHAPE_FLAG_HAS_TRANSFORMERS = 1 shl 4;
  SHAPE_FLAG_TRANSLATION   = 1 shl 5;

  TRANSFORMER_TYPE_AFFINE      = 20;
  TRANSFORMER_TYPE_CONTOUR     = 21;
  TRANSFORMER_TYPE_PERSPECTIVE = 22;
  TRANSFORMER_TYPE_STROKE      = 23;
var
  i, p: Integer;
  shapeType, shapeFlags: Byte;
  sIdx, nPaths, pIdx: Byte;
  transformerCount, ttype: Byte;
  tagLen: Word;
begin
  SetLength(FShapes, ACount);
  for i := 0 to ACount - 1 do
  begin
    FillChar(FShapes[i], SizeOf(THvifShape), 0);
    shapeType := ReadByte;

    if shapeType <> SHAPE_TYPE_PATH_SOURCE then
    begin
      { Unknown shape type: skip via tag-length }
      tagLen := ReadWord;
      Inc(FPos, tagLen);
      Continue;
    end;

    sIdx   := ReadByte;
    nPaths := ReadByte;

    FShapes[i].StyleIndex := sIdx;
    SetLength(FShapes[i].PathIndices, nPaths);
    for p := 0 to nPaths - 1 do
    begin
      pIdx := ReadByte;
      FShapes[i].PathIndices[p] := pIdx;
    end;

    shapeFlags := ReadByte;

    { Shape hinting flag is stored but not used in rendering }

    if (shapeFlags and SHAPE_FLAG_TRANSFORM) <> 0 then
    begin
      { Full 6-float24 affine transform }
      FShapes[i].Transform[0] := ReadFloat24;
      FShapes[i].Transform[1] := ReadFloat24;
      FShapes[i].Transform[2] := ReadFloat24;
      FShapes[i].Transform[3] := ReadFloat24;
      FShapes[i].Transform[4] := ReadFloat24;
      FShapes[i].Transform[5] := ReadFloat24;
      FShapes[i].HasTransform := True;
    end
    else if (shapeFlags and SHAPE_FLAG_TRANSLATION) <> 0 then
    begin
      { Translation-only: 2 variable-length coords }
      FShapes[i].TranslateX := ReadCoord;
      FShapes[i].TranslateY := ReadCoord;
      FShapes[i].HasTranslation := True;
    end;

    if (shapeFlags and SHAPE_FLAG_LOD_SCALE) <> 0 then
    begin
      { min/max visibility scale: encoded as byte = Round(scale * 63.75) }
      FShapes[i].MinVisScale := ReadByte / 63.75;
      FShapes[i].MaxVisScale := ReadByte / 63.75;
      FShapes[i].HasLODScale := True;
    end;

    if (shapeFlags and SHAPE_FLAG_HAS_TRANSFORMERS) <> 0 then
    begin
      { Transformer list — we skip them but must parse to advance the stream }
      transformerCount := ReadByte;
      for p := 0 to transformerCount - 1 do
      begin
        ttype := ReadByte;
        case ttype of
          TRANSFORMER_TYPE_AFFINE:
            begin
              { 6 × raw float32 (not float24!) }
              Inc(FPos, 6 * SizeOf(Single));
            end;
          TRANSFORMER_TYPE_CONTOUR:
            begin
              { width (byte), lineJoin (byte), miterLimit (byte) }
              Inc(FPos, 3);
            end;
          TRANSFORMER_TYPE_PERSPECTIVE:
            begin
              { 9 × float24 }
              Inc(FPos, 9 * 3);
            end;
          TRANSFORMER_TYPE_STROKE:
            begin
              { 3 raw uint8 bytes — see Haiku FlatIconImporter.cpp line 501-520:
                  width      : uint8, float = width - 128.0
                  lineOptions: uint8, join = low nibble, cap = high nibble
                  miterLimit : uint8, float = raw value }
              FShapes[i].StrokeWidth      := ReadByte - 128.0;
              tagLen                       := ReadByte;  { reuse tagLen as lineOpts }
              FShapes[i].StrokeLineJoin   := tagLen and $0F;
              FShapes[i].StrokeLineCap    := (tagLen shr 4) and $0F;
              FShapes[i].StrokeMiterLimit := ReadByte;
              FShapes[i].HasStroke        := True;
            end;
          else
          begin
            { Unknown transformer: skip tag-length block }
            tagLen := ReadWord;
            Inc(FPos, tagLen);
          end;
        end;
      end;
    end;
  end;
end;


{ ===================================================================
  Top-level parse dispatcher
  =================================================================== }

procedure THvifIcon.ParseFrom(const AData: TBytes);
var
  magic: array[0..3] of Byte;
  nStyles, nPaths, nShapes: Byte;
begin
  FData := AData;
  FPos  := 0;

  { Verify magic }
  magic[0] := ReadByte;
  magic[1] := ReadByte;
  magic[2] := ReadByte;
  magic[3] := ReadByte;
  if (magic[0] <> HVIF_MAGIC[0]) or (magic[1] <> HVIF_MAGIC[1]) or
     (magic[2] <> HVIF_MAGIC[2]) or (magic[3] <> HVIF_MAGIC[3]) then
    raise EHvifFormatError.Create('HVIF: invalid magic — not an HVIF file');

  nStyles := ReadByte;
  ParseStyles(nStyles);

  nPaths := ReadByte;
  ParsePaths(nPaths);

  nShapes := ReadByte;
  ParseShapes(nShapes);
end;


{ ===================================================================
  Factory class functions
  =================================================================== }

class function THvifIcon.CreateFromStream(AStream: TStream): THvifIcon;
var
  data: TBytes;
  sz: Int64;
begin
  sz := AStream.Size - AStream.Position;
  if (sz <= 0) or (sz > 256 * 1024) then
    raise EHvifFormatError.CreateFmt(
      'HVIF: stream size %d is out of valid range', [sz]);
  SetLength(data, sz);
  AStream.ReadBuffer(data[0], sz);
  Result := THvifIcon.Create;
  try
    Result.ParseFrom(data);
  except
    Result.Free;
    raise;
  end;
end;

class function THvifIcon.CreateFromFile(const AFileName: string): THvifIcon;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CreateFromStream(fs);
  finally
    fs.Free;
  end;
end;

class function THvifIcon.CreateFromResource(AInstance: THandle;
  const AName: string): THvifIcon;
var
  rs: TResourceStream;
begin
  rs := TResourceStream.Create(AInstance, AName, RT_RCDATA);
  try
    Result := CreateFromStream(rs);
  finally
    rs.Free;
  end;
end;


{ ===================================================================
  Destructor
  =================================================================== }

destructor THvifIcon.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FCachedImages) do
    FCachedImages[i].Image.Free;
  inherited Destroy;
end;


procedure THvifIcon.ClearCache;
var
  i: Integer;
begin
  for i := 0 to High(FCachedImages) do
    FCachedImages[i].Image.Free;
  SetLength(FCachedImages, 0);
end;

{ ===================================================================
  Image cache
  =================================================================== }

function THvifIcon.FindCachedImage(AW, AH: Integer): TfpgImage;
var
  i: Integer;
begin
  for i := 0 to High(FCachedImages) do
    if (FCachedImages[i].Width = AW) and (FCachedImages[i].Height = AH) then
    begin
      Result := FCachedImages[i].Image;
      Exit;
    end;
  Result := nil;
end;


{ ===================================================================
  Compound renderer support types
  =================================================================== }

type
  { 256-entry pre-computed colour LUT for multi-stop gradients.
    Inherits array_base so it can be passed directly to span_gradient as the
    color_function parameter.  All 256 aggclr entries are computed once in
    Build() by linearly interpolating between the HVIF gradient stops using
    their normalised Offset values. }
  THvifColorTable = object(array_base)
    FTable: array[0..255] of aggclr;

    { Must be called once before first use to initialise the FPC VMT pointer.
      Builds the 256-entry LUT from AStops. Safe to call with 0 or 1 stops. }
    constructor Build(const AStops: array of THvifGradientStop);

    function size  : unsigned; virtual;
    function entry : unsigned; virtual;
    function array_operator(i: unsigned): pointer; virtual;
  end;

  { Per-gradient entry.  All pointer fields (Interp.m_trans, SpanGen pointers)
    refer to sibling fields within the SAME record.
    Stability: styleEntries dynamic array is SetLength'd once before any entry
    is initialised, and never resized afterwards. }
  THvifGradEntry = record
    Matrix:     trans_affine;             { screen-pixels -> gradient-local 64-unit }
    ColorTable: THvifColorTable;          { 256-entry multi-stop colour LUT }
    Alloc:      span_allocator;           { internal span buffer (heap; call Destruct) }
    Interp:     span_interpolator_linear; { stores @Matrix -- no separate Destruct }
    SpanGen:    span_gradient;            { stores @Interp, @ColorTable, GradFunc ptr }
    { One instance per gradient function type; only the matching one is used. }
    GFLinear:  gradient_x;
    GFCircle:  gradient_circle;
    GFDiamond: gradient_diamond;
    GFConic:   gradient_conic;
    GFXY:      gradient_xy;
    GFSqrtXY:  gradient_sqrt_xy;
  end;

  THvifStyleEntry = record
    IsSolid:    Boolean;
    SolidColor: aggclr;
    Grad:       THvifGradEntry;   { valid only when IsSolid = False }
  end;

  { Implements style_handler for the compound rasterizer.
    FData points to styleEntries[0] in the dynamic array.
    NOTE: Must call Init before use — FPC object VMT is only set by a constructor. }
  THvifStyleHandler = object(style_handler)
    FData:  ^THvifStyleEntry;
    FCount: Integer;
    constructor Init;  { no-op — call to force FPC VMT initialisation }
    function  is_solid     (style : unsigned) : boolean; virtual;
    function  color        (style : unsigned) : aggclr_ptr; virtual;
    procedure generate_span(span  : aggclr_ptr;
                            x, y  : int;
                            len, style : unsigned); virtual;
  end;

{ ===================================================================
  THvifColorTable
  =================================================================== }

constructor THvifColorTable.Build(const AStops: array of THvifGradientStop);
var
  nStops, i, lo, hi: Integer;
  t, frac, r, g, b, a: Double;
  ofs0, ofs1: Double;
begin
  nStops := Length(AStops);
  if nStops = 0 then
  begin
    { No stops — fill with opaque black }
    for i := 0 to 255 do
      FTable[i].ConstrInt(0, 0, 0, 255);
    Exit;
  end;
  if nStops = 1 then
  begin
    { Single stop — fill with that colour }
    for i := 0 to 255 do
      FTable[i].ConstrInt(
        AStops[0].Color.R, AStops[0].Color.G,
        AStops[0].Color.B, AStops[0].Color.A);
    Exit;
  end;
  { Multiple stops: for each of the 256 table entries find the surrounding
    stops and lerp. }
  for i := 0 to 255 do
  begin
    t := i / 255.0;
    { Find the segment [lo, lo+1] that brackets t }
    lo := 0;
    hi := nStops - 1;
    while (hi - lo > 1) do
    begin
      { Binary search not needed for typical stop counts (≤16), linear is fine }
      if t >= AStops[lo + 1].Offset then
        Inc(lo)
      else
        Break;
    end;
    hi := lo + 1;
    if hi >= nStops then hi := nStops - 1;
    ofs0 := AStops[lo].Offset;
    ofs1 := AStops[hi].Offset;
    if (ofs1 <= ofs0) or (lo = hi) then
      frac := 0.0
    else
      frac := (t - ofs0) / (ofs1 - ofs0);
    if frac < 0.0 then frac := 0.0;
    if frac > 1.0 then frac := 1.0;
    r := AStops[lo].Color.R + frac * (AStops[hi].Color.R - AStops[lo].Color.R);
    g := AStops[lo].Color.G + frac * (AStops[hi].Color.G - AStops[lo].Color.G);
    b := AStops[lo].Color.B + frac * (AStops[hi].Color.B - AStops[lo].Color.B);
    a := AStops[lo].Color.A + frac * (AStops[hi].Color.A - AStops[lo].Color.A);
    FTable[i].ConstrInt(
      Round(r), Round(g), Round(b), Round(a));
  end;
end;

function THvifColorTable.size: unsigned;
begin
  Result := 256;
end;

function THvifColorTable.entry: unsigned;
begin
  Result := SizeOf(aggclr);
end;

function THvifColorTable.array_operator(i: unsigned): pointer;
begin
  if i >= 256 then i := 255;
  Result := @FTable[i];
end;


{ ===================================================================
  THvifStyleHandler
  =================================================================== }

constructor THvifStyleHandler.Init;
begin
  { No-op — called only to force FPC VMT pointer initialisation for this
    stack-allocated object.  Without a constructor call, FPC leaves the VMT
    pointer uninitialised, causing a crash at address 0 on the first virtual
    dispatch inside render_scanlines_compound. }
end;

function THvifStyleHandler.is_solid(style: unsigned): boolean;
begin
  if style >= unsigned(FCount) then
    Result := True
  else
    Result := FData[style].IsSolid;
end;

function THvifStyleHandler.color(style: unsigned): aggclr_ptr;
begin
  { SolidColor is already premultiplied at setup time (see RenderToImage).
    Calling premultiply here again would compound the multiplication on every
    scan-line span, decaying RGB to zero within a few pixels. }
  if style >= unsigned(FCount) then
    Result := @FData[0].SolidColor
  else
    Result := @FData[style].SolidColor;
end;

procedure THvifStyleHandler.generate_span(span: aggclr_ptr; x, y: int;
  len, style: unsigned);
var
  src, p: aggclr_ptr;
  i: unsigned;
begin
  FData[style].Grad.Alloc.allocate(len);
  src := FData[style].Grad.SpanGen.generate(x, y, len);
  Move(src^, span^, len * SizeOf(aggclr));
  { Premultiply each gradient span pixel — same requirement as
    the solid colour path above. }
  p := span;
  for i := 0 to len - 1 do
  begin
    p^.premultiply;
    Inc(p);
  end;
end;


{ ===================================================================
  Path emission helper
  =================================================================== }

{ Emit HVIF path commands into path_storage.
  Point 0: move_to. Subsequent points: line_to or curve4 depending on
  whether adjacent control points are collapsed to the anchor. }
procedure EmitHvifPath(const APath: THvifPath; var APS: path_storage);
var
  n, i: Integer;
  prev, pt: THvifPoint;
  isLine: Boolean;
begin
  n := Length(APath.Points);
  if n = 0 then
    Exit;

  APS.move_to(APath.Points[0].X, APath.Points[0].Y);

  for i := 1 to n - 1 do
  begin
    prev := APath.Points[i - 1];
    pt   := APath.Points[i];

    isLine := (Abs(prev.OutX - prev.X) < 1e-6) and
              (Abs(prev.OutY - prev.Y) < 1e-6) and
              (Abs(pt.InX   - pt.X)   < 1e-6) and
              (Abs(pt.InY   - pt.Y)   < 1e-6);

    if isLine then
      APS.line_to(pt.X, pt.Y)
    else
      APS.curve4(prev.OutX, prev.OutY,
                 pt.InX,    pt.InY,
                 pt.X,      pt.Y);
  end;

  if APath.Closed and (n >= 2) then
  begin
    { Emit the closing segment from the last point back to the first.
      Without this, close_polygon draws a straight line, which flattens
      one edge of shapes like circles defined with 4 bezier points. }
    prev := APath.Points[n - 1];
    pt   := APath.Points[0];

    isLine := (Abs(prev.OutX - prev.X) < 1e-6) and
              (Abs(prev.OutY - prev.Y) < 1e-6) and
              (Abs(pt.InX   - pt.X)   < 1e-6) and
              (Abs(pt.InY   - pt.Y)   < 1e-6);

    if not isLine then
      APS.curve4(prev.OutX, prev.OutY,
                 pt.InX,    pt.InY,
                 pt.X,      pt.Y);
    APS.close_polygon;
  end
  else if APath.Closed then
    APS.close_polygon;
end;


{ ===================================================================
  Rendering
  =================================================================== }

{ Render all shapes into AImg in a single pass using AGG's compound rasterizer.
  Each pixel is visited once; overlapping shapes are blended by style in one
  scanline sweep instead of being re-painted per shape.

  Coordinate conventions:
    Path points and HVIF GradTransform are in 64-unit icon space.
    Shape-to-screen matrix: ShapeAffine first (in icon space), then Scale
      to screen pixels.  Scale must come last so that translations in the
      shape affine remain in 64-unit space before being scaled down.
    Gradient matrix: GradTransform[sx,shy,shx,sy,tx,ty] maps gradient-local
      64-unit space to icon space.  Multiplied by scale and inverted so the
      span interpolator maps screen pixels back to gradient-local coords.
    d1/d2 for span_gradient are in gradient-local 64-unit space:
      linear  d1=-64, d2=64  (gradient x-axis spans [-64, 64])
      others  d1=0,   d2=64  (radial distance spans [0, 64])  }
procedure THvifIcon.RenderIntoImage(AImg: TfpgImage);
var
  W, H: Integer;
  buf:  array of Byte;
  scale: Double;

  { AGG rendering pipeline }
  rbuf:     rendering_buffer;
  pixf:     pixel_formats;
  renBase:  renderer_base;
  ras:      rasterizer_compound_aa_int;
  slAA:     scanline_u8;
  slBin:    scanline_bin;
  mixAlloc: span_allocator;

  { Path pipeline -- shared across all shapes }
  ps:     path_storage;
  curve:  conv_curve;
  ct:     conv_transform;
  stroke:    conv_stroke;
  ct_stroke: conv_transform;

  { Per-style data }
  styleEntries: array of THvifStyleEntry;
  sh:           THvifStyleHandler;

  { Style setup locals }
  si:    Integer;
  styl:  THvifStyle;
  entry: ^THvifStyleEntry;

  { Shape processing locals }
  i, pidx: Integer;
  shape:   THvifShape;
  shapeMatrix, ta: trans_affine;
  pathIdx: Byte;

  { De-premultiply pass }
  pxPtr: PLongWord;
  pxAlpha: LongWord;

begin
  W     := AImg.Width;
  H     := AImg.Height;
  scale := Min(W, H) / 64.0;

  { BGRA32 pixel buffer, fully transparent }
  SetLength(buf, W * H * 4);
  FillChar(buf[0], W * H * 4, 0);

  { ---- AGG pipeline ----
    Use premultiplied pixel format (pixfmt_bgra32_pre) so that the compound
    renderer blends antialiased edges correctly.  Haiku's IconRenderer uses
    fBaseRendererPre for the same reason.  The buffer is de-premultiplied
    before copying to TfpgImage (which the hybrid canvas treats as straight
    alpha). }
  rbuf.Construct(@buf[0], W, H, W * 4);
  pixfmt_bgra32_pre(pixf, @rbuf);
  renBase.Construct(@pixf);

  ras.Construct;
  slAA.Construct;
  slBin.Construct;
  mixAlloc.Construct;
  ras.clip_box(0, 0, W, H);

  { ---- Per-style setup ---- }
  { SetLength once: ensures intra-record pointer fields stay stable }
  SetLength(styleEntries, Length(FStyles));

  for si := 0 to High(FStyles) do
  begin
    styl  := FStyles[si];
    entry := @styleEntries[si];

    case styl.StyleType of
      hstSolidColor, hstSolidColorNoAlpha,
      hstSolidGray,  hstSolidGrayNoAlpha:
        begin
          entry^.IsSolid := True;
          entry^.SolidColor.ConstrInt(
            styl.Color.R, styl.Color.G,
            styl.Color.B, styl.Color.A);
          { Premultiply once here; color() returns this pointer directly
            so premultiplying there would compound on every span call. }
          entry^.SolidColor.premultiply;
        end;

      hstGradient:
        begin
          if Length(styl.Stops) = 0 then
          begin
            entry^.IsSolid := True;
            entry^.SolidColor.ConstrInt(0, 0, 0, 255);
          end
          else
          begin
            entry^.IsSolid := False;

            { Forward matrix: gradient-local 64-unit to screen pixels }
            entry^.Grad.Matrix.Construct(
              scale * styl.GradTransform[0],
              scale * styl.GradTransform[1],
              scale * styl.GradTransform[2],
              scale * styl.GradTransform[3],
              scale * styl.GradTransform[4],
              scale * styl.GradTransform[5]);
            { Invert: screen-pixels to gradient-local }
            entry^.Grad.Matrix.invert;

            { Colour LUT: pre-compute 256 entries interpolating across all stops }
            entry^.Grad.ColorTable.Build(styl.Stops);

            { Span allocator + interpolator (Interp stores @Matrix) }
            entry^.Grad.Alloc.Construct;
            entry^.Grad.Interp.Construct(@entry^.Grad.Matrix);

            { Gradient function + span generator }
            case styl.GradientType of
              hgtLinear:
                begin
                  entry^.Grad.GFLinear.Construct;
                  entry^.Grad.SpanGen.Construct(
                    @entry^.Grad.Alloc,    @entry^.Grad.Interp,
                    @entry^.Grad.GFLinear, @entry^.Grad.ColorTable,
                    -64.0, 64.0);
                end;
              hgtCircular:
                begin
                  entry^.Grad.GFCircle.Construct;
                  entry^.Grad.SpanGen.Construct(
                    @entry^.Grad.Alloc,    @entry^.Grad.Interp,
                    @entry^.Grad.GFCircle, @entry^.Grad.ColorTable,
                    0.0, 64.0);
                end;
              hgtDiamond:
                begin
                  entry^.Grad.GFDiamond.Construct;
                  entry^.Grad.SpanGen.Construct(
                    @entry^.Grad.Alloc,     @entry^.Grad.Interp,
                    @entry^.Grad.GFDiamond, @entry^.Grad.ColorTable,
                    0.0, 64.0);
                end;
              hgtConic:
                begin
                  entry^.Grad.GFConic.Construct;
                  entry^.Grad.SpanGen.Construct(
                    @entry^.Grad.Alloc,   @entry^.Grad.Interp,
                    @entry^.Grad.GFConic, @entry^.Grad.ColorTable,
                    0.0, 64.0);
                end;
              hgtXY:
                begin
                  entry^.Grad.GFXY.Construct;
                  entry^.Grad.SpanGen.Construct(
                    @entry^.Grad.Alloc, @entry^.Grad.Interp,
                    @entry^.Grad.GFXY,  @entry^.Grad.ColorTable,
                    0.0, 64.0);
                end;
              hgtSqrtXY:
                begin
                  entry^.Grad.GFSqrtXY.Construct;
                  entry^.Grad.SpanGen.Construct(
                    @entry^.Grad.Alloc,     @entry^.Grad.Interp,
                    @entry^.Grad.GFSqrtXY,  @entry^.Grad.ColorTable,
                    0.0, 64.0);
                end;
            end; { case GradientType }
          end;
        end; { hstGradient }

      else
        begin
          entry^.IsSolid := True;
          entry^.SolidColor.ConstrInt(0, 0, 0, 255);
        end;
    end; { case StyleType }
  end;

  { ---- Wire up style handler ----
    Init must be called first to set the FPC object VMT pointer; without it
    virtual dispatch in render_scanlines_compound crashes at address 0. }
  sh.Init;
  if Length(styleEntries) > 0 then
    sh.FData := @styleEntries[0]
  else
    sh.FData := nil;
  sh.FCount := Length(styleEntries);

  { ---- Path pipeline ---- }
  ps.Construct;
  curve.Construct(@ps);
  stroke.Construct(@curve);

  try
    { ---- Add all shapes to the compound rasterizer ---- }
    for i := 0 to High(FShapes) do
    begin
      shape := FShapes[i];

      { LOD visibility: skip shapes whose scale range excludes the current size.
        Matches Haiku PathSourceShape::Visible(float scale). }
      if shape.HasLODScale then
        if not ((scale >= shape.MinVisScale) and
                ((scale <= shape.MaxVisScale) or (shape.MaxVisScale >= 4.0))) then
          Continue;

      { Shape-to-screen: ShapeAffine first (in 64-unit icon space),
        then scale to screen pixels.  Order matters — applying scale
        before the shape transform would scale translations into
        icon-space units, pushing geometry off the target bitmap. }
      shapeMatrix.Construct;

      if shape.HasTransform then
      begin
        ta.Construct(
          shape.Transform[0], shape.Transform[1],
          shape.Transform[2], shape.Transform[3],
          shape.Transform[4], shape.Transform[5]);
        shapeMatrix.multiply(@ta);
      end
      else if shape.HasTranslation then
      begin
        ta.Construct(1.0, 0.0, 0.0, 1.0,
                     shape.TranslateX, shape.TranslateY);
        shapeMatrix.multiply(@ta);
      end;

      shapeMatrix.scale(scale, scale);

      for pidx := 0 to High(shape.PathIndices) do
      begin
        pathIdx := shape.PathIndices[pidx];
        if pathIdx >= Length(FPaths) then
          Continue;

        ps.remove_all;
        EmitHvifPath(FPaths[pathIdx], ps);

        ras.styles(shape.StyleIndex, -1);

        if shape.HasStroke then
        begin
          { Route through conv_stroke to expand the centre-line into a filled outline.
          StrokeWidth is in 64-unit icon space; shapeMatrix already applies 'scale'
          when transforming to screen pixels, so do NOT multiply by scale here —
          doing so would apply the scaling twice, making strokes sub-pixel at small
          render sizes (e.g. 0.25 px at 16×16). }
          stroke.width_(shape.StrokeWidth);
          stroke.line_cap_(shape.StrokeLineCap);
          stroke.line_join_(shape.StrokeLineJoin);
          stroke.miter_limit_(shape.StrokeMiterLimit);
          stroke.approximation_scale_(scale);
          ct_stroke.Construct(@stroke, @shapeMatrix);
          ras.add_path(@ct_stroke);
        end
        else
        begin
          ct.Construct(@curve, @shapeMatrix);
          ras.add_path(@ct);
        end;
      end;

      { ---- Per-shape compound render pass ----
        Commit each shape individually to avoid sub-pixel seams at style
        boundaries where overlapping shapes (e.g. round stroke caps) would
        split coverage in a single compound pass.  Haiku's IconRenderer
        does multi-pass for transparent styles; we extend this to all
        shapes for correct edge blending. }
      render_scanlines_compound(@ras, @slAA, @slBin, @renBase, @mixAlloc, @sh);
      ras.reset;
    end;

  finally
    stroke.Destruct;
    curve.Destruct;
    ps.Destruct;

    { Free span allocators (Alloc.Destruct is safe on NIL spans) }
    for si := 0 to High(styleEntries) do
      if not styleEntries[si].IsSolid then
        styleEntries[si].Grad.Alloc.Destruct;

    ras.Destruct;
    slAA.Destruct;
    slBin.Destruct;
    mixAlloc.Destruct;
    rbuf.Destruct;
  end;

  { De-premultiply the buffer: the compound renderer produced premultiplied
    BGRA pixels, but TfpgImage / the hybrid canvas expects straight alpha.
    For each pixel with A > 0, recover straight RGB = premultiplied RGB * 255 / A. }
  pxPtr := @buf[0];
  for si := 0 to W * H - 1 do
  begin
    pxAlpha := (pxPtr^ shr 24) and $FF;  { alpha byte in BGRA32 }
    if (pxAlpha > 0) and (pxAlpha < 255) then
    begin
      pxPtr^ := (LongWord(((pxPtr^        and $FF) * 255 + pxAlpha shr 1) div pxAlpha)) or
                 (LongWord((((pxPtr^ shr 8) and $FF) * 255 + pxAlpha shr 1) div pxAlpha) shl 8) or
                 (LongWord((((pxPtr^ shr 16) and $FF) * 255 + pxAlpha shr 1) div pxAlpha) shl 16) or
                 (LongWord(pxAlpha) shl 24);
    end;
    Inc(pxPtr);
  end;

  { Copy BGRA32 buffer to TfpgImage (byte-identical on little-endian) }
  Move(buf[0], AImg.ImageData^, W * H * 4);
  AImg.UpdateImage;

end;


{ ===================================================================
  Public API
  =================================================================== }

function THvifIcon.GetImage(AWidth, AHeight: Integer): TfpgImage;
var
  img: TfpgImage;
  entry: THvifCachedImage;
begin
  if (AWidth <= 0) or (AHeight <= 0) then
    raise EHvifError.CreateFmt(
      'THvifIcon.GetImage: invalid size %d×%d', [AWidth, AHeight]);

  Result := FindCachedImage(AWidth, AHeight);
  if Assigned(Result) then
    Exit;

  img := TfpgImage.Create;
  img.AllocateImage(32, AWidth, AHeight);

  RenderIntoImage(img);

  entry.Width  := AWidth;
  entry.Height := AHeight;
  entry.Image  := img;
  SetLength(FCachedImages, Length(FCachedImages) + 1);
  FCachedImages[High(FCachedImages)] := entry;

  Result := img;
end;

function THvifIcon.StyleCount: Integer;
begin
  Result := Length(FStyles);
end;

function THvifIcon.PathCount: Integer;
begin
  Result := Length(FPaths);
end;

function THvifIcon.ShapeCount: Integer;
begin
  Result := Length(FShapes);
end;

function THvifIcon.GetStyle(AIndex: Integer): THvifStyle;
begin
  if (AIndex < 0) or (AIndex >= Length(FStyles)) then
    raise EHvifError.CreateFmt('THvifIcon.GetStyle: index %d out of range [0..%d]',
      [AIndex, Length(FStyles) - 1]);
  Result := FStyles[AIndex];
end;

function THvifIcon.GetPath(AIndex: Integer): THvifPath;
begin
  if (AIndex < 0) or (AIndex >= Length(FPaths)) then
    raise EHvifError.CreateFmt('THvifIcon.GetPath: index %d out of range [0..%d]',
      [AIndex, Length(FPaths) - 1]);
  Result := FPaths[AIndex];
end;

function THvifIcon.GetShape(AIndex: Integer): THvifShape;
begin
  if (AIndex < 0) or (AIndex >= Length(FShapes)) then
    raise EHvifError.CreateFmt('THvifIcon.GetShape: index %d out of range [0..%d]',
      [AIndex, Length(FShapes) - 1]);
  Result := FShapes[AIndex];
end;


end.
