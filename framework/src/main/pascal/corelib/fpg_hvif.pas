{
    This unit is part of the fpGUI Toolkit project.

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
      - Gradient rendering: 2-stop approximation only (first and last stop).
        Multi-stop intermediate colours are not rendered in v1.
        Diamond/Conic/XY gradient types fall back to the first stop's solid colour.
      - Thread safety: NOT thread-safe. Caller must use from the fpGUI main thread.
      - Image cache: linear scan, 8 entries max per icon instance.
}

unit fpg_hvif;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  fpg_base,
  fpg_main,
  agg_2D,
  agg_color;


{ ==================== Exception types ==================== }

type
  EHvifError      = class(Exception);
  EHvifFormatError = class(EHvifError);
  EHvifParseError  = class(EHvifFormatError);


{ ==================== Public constants ==================== }

const
  { HVIF magic bytes as they appear in the file stream (LE uint32 = 'ficn') }
  HVIF_MAGIC: array[0..3] of Byte = ($6E, $63, $69, $66);


{ ==================== Internal data model ==================== }

type
  { Gradient types (from Haiku GradientTransformable.h) }
  THvifGradientType = (
    hgtLinear   = 0,
    hgtCircular = 1,
    hgtDiamond  = 2,
    hgtConic    = 3,
    hgtXY       = 4,
    hgtSqrtXY   = 5
  );

  { Style (color/fill) types -- from Haiku FlatIconFormat.h }
  THvifStyleType = (
    hstSolidColor        = 1,  { 4 bytes: R G B A }
    hstGradient          = 2,
    hstSolidColorNoAlpha = 3,  { 3 bytes: R G B    (alpha=255) }
    hstSolidGray         = 4,  { 2 bytes: K A }
    hstSolidGrayNoAlpha  = 5   { 1 byte:  K         (alpha=255) }
  );

  THvifColor = record
    R, G, B, A: Byte;
  end;

  THvifGradientStop = record
    Offset: Single;   { normalised 0.0..1.0 }
    Color: THvifColor;
  end;

  THvifStyle = record
    StyleType:    THvifStyleType;
    { For solid styles: }
    Color: THvifColor;
    { For gradient styles: }
    GradientType: THvifGradientType;
    { Gradient transform: maps gradient parameter space -> icon (64-unit) space.
      Stored as [sx, shy, shx, sy, tx, ty] in agg trans_affine order. }
    GradTransform: array[0..5] of Single;
    HasGradTransform: Boolean;
    Stops: array of THvifGradientStop;
  end;

  { Unified path point: stores main point + incoming/outgoing control points.
    For line points: InX=X, InY=Y, OutX=X, OutY=Y. }
  THvifPoint = record
    X, Y: Single;
    InX, InY: Single;
    OutX, OutY: Single;
  end;

  THvifPath = record
    Closed: Boolean;
    Points: array of THvifPoint;
  end;

  THvifShape = record
    StyleIndex:  Byte;
    PathIndices: array of Byte;
    { Affine shape transform (6 float24 values, agg order) }
    Transform:    array[0..5] of Single;
    HasTransform: Boolean;
    { Translation-only (2 coords, applied when HasTransform=False) }
    TranslateX, TranslateY: Single;
    HasTranslation: Boolean;
  end;

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
    procedure ApplyStyleFill(var AAgg: Agg2D; AStyleIdx: Byte; AScale: Double);
    procedure RenderPath(var AAgg: Agg2D; const APath: THvifPath);

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

    { Diagnostic: number of parsed styles/paths/shapes }
    function StyleCount: Integer;
    function PathCount: Integer;
    function ShapeCount: Integer;
  end;


implementation

uses
  agg_basics;   { int8u_ptr = PByte }


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
      { min/max visibility scale: 2 bytes, ignored in rendering }
      ReadByte;
      ReadByte;
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
              { width (byte), lineOptions (byte), miterLimit (byte) }
              Inc(FPos, 3);
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
  Rendering
  =================================================================== }

{ Apply fill style to the Agg2D canvas for the given style index.
  Gradient coordinates are derived from the gradient transform matrix.

  Gradient transform convention (from Haiku IconRenderer.cpp):
    The gradient transform maps gradient PARAMETER space → icon (64-unit) space.
    For linear gradient (start=-64, end=64):
      left end  = transform(-64, 0) = (tx - sx*64, ty - shy*64)
      right end = transform( 64, 0) = (tx + sx*64, ty + shy*64)
    For radial gradient (start=0, end=64):
      center = transform(0, 0) = (tx, ty)
      radius = 64 * sqrt(sx^2 + shy^2)

  v1 limitation: only the first and last stops are used for gradient colour.
  Diamond/Conic/XY/SqrtXY fall back to the first stop's solid colour.
}
procedure THvifIcon.ApplyStyleFill(var AAgg: Agg2D; AStyleIdx: Byte;
  AScale: Double);
var
  style: THvifStyle;
  c1, c2: Color;
  sx, shy, tx, ty, r: Double;

  function HvifToAggColor(const C: THvifColor): Color;
  begin
    Result.Construct(C.R, C.G, C.B, C.A);
  end;

begin
  AAgg.noLine;

  if AStyleIdx >= Length(FStyles) then
  begin
    { Out-of-range style index: render transparent }
    AAgg.fillColor(0, 0, 0, 0);
    Exit;
  end;

  style := FStyles[AStyleIdx];

  case style.StyleType of
    hstSolidColor, hstSolidColorNoAlpha:
      AAgg.fillColor(style.Color.R, style.Color.G, style.Color.B, style.Color.A);

    hstSolidGray, hstSolidGrayNoAlpha:
      AAgg.fillColor(style.Color.R, style.Color.G, style.Color.B, style.Color.A);

    hstGradient:
      begin
        if Length(style.Stops) = 0 then
        begin
          AAgg.fillColor(0, 0, 0, 255);
          Exit;
        end;

        c1 := HvifToAggColor(style.Stops[0].Color);
        c2 := HvifToAggColor(style.Stops[High(style.Stops)].Color);

        { Extract key matrix values: m[sx, shy, shx, sy, tx, ty] }
        sx  := style.GradTransform[0];
        shy := style.GradTransform[1];
        tx  := style.GradTransform[4];
        ty  := style.GradTransform[5];

        case style.GradientType of
          hgtLinear:
            { Endpoints in icon (64-unit) space:
              start = transform(-64, 0), end = transform(64, 0) }
            AAgg.fillLinearGradient(
              tx - sx * 64, ty - shy * 64,
              tx + sx * 64, ty + shy * 64,
              c1, c2);

          hgtCircular:
            begin
              r := 64.0 * Sqrt(sx * sx + shy * shy);
              AAgg.fillRadialGradient(tx, ty, r, c1, c2);
            end;

          hgtDiamond:
            begin
              r := 64.0 * Sqrt(sx * sx + shy * shy);
              AAgg.fillDiamondGradient(tx, ty, r, c1, c2);
            end;

          hgtConic:
            begin
              r := 64.0 * Sqrt(sx * sx + shy * shy);
              AAgg.fillConicGradient(tx, ty, r, c1, c2);
            end;

          hgtXY, hgtSqrtXY:
            begin
              r := 64.0 * Sqrt(sx * sx + shy * shy);
              AAgg.fillXYGradient(tx, ty, r, c1, c2);
            end;
        end;
      end;

    else
      AAgg.fillColor(0, 0, 0, 255);
  end;
end;

{ Emit AggPas path commands for one HVIF path.
  Rendering rule (from Haiku VectorPath / PathSourceShape):
    Point 0 → moveTo
    Point i (i>0):
      if prev.OutX=prev.X and prev.OutY=prev.Y and pt.InX=pt.X and pt.InY=pt.Y
        → lineTo(pt.X, pt.Y)
      else
        → cubicCurveTo(prev.OutX, prev.OutY, pt.InX, pt.InY, pt.X, pt.Y)
    If path.Closed → closePolygon
}
procedure THvifIcon.RenderPath(var AAgg: Agg2D; const APath: THvifPath);
var
  n, i: Integer;
  prev, pt: THvifPoint;
  isLine: Boolean;
begin
  n := Length(APath.Points);
  if n = 0 then
    Exit;

  AAgg.resetPath;
  AAgg.moveTo(APath.Points[0].X, APath.Points[0].Y);

  for i := 1 to n - 1 do
  begin
    prev := APath.Points[i - 1];
    pt   := APath.Points[i];

    isLine := (Abs(prev.OutX - prev.X) < 1e-6) and
              (Abs(prev.OutY - prev.Y) < 1e-6) and
              (Abs(pt.InX - pt.X) < 1e-6) and
              (Abs(pt.InY - pt.Y) < 1e-6);

    if isLine then
      AAgg.lineTo(pt.X, pt.Y)
    else
      AAgg.cubicCurveTo(
        prev.OutX, prev.OutY,
        pt.InX,    pt.InY,
        pt.X,      pt.Y);
  end;

  if APath.Closed then
    AAgg.closePolygon;
end;

{ Render all shapes into AImg using a standalone Agg2D object.
  The icon's 64×64 native coordinate space is scaled uniformly to fit AImg. }
procedure THvifIcon.RenderIntoImage(AImg: TfpgImage);
var
  buf: array of Byte;
  W, H: Integer;
  agg: Agg2D;
  scale: Double;
  tr: Transformations_;
  trp: Transformations_ptr;
  i, pidx: Integer;
  shape: THvifShape;
  pathIdx: Byte;
begin
  W := AImg.Width;
  H := AImg.Height;

  { Allocate BGRA32 pixel buffer, cleared to fully transparent }
  SetLength(buf, W * H * 4);
  FillChar(buf[0], W * H * 4, 0);

  agg.Construct;
  try
    agg.attach(@buf[0], W, H, W * 4);
    { Default fill is white after attach — override to transparent }
    agg.fillColor(0, 0, 0, 0);
    agg.noLine;

    { Uniform scale: map 64×64 native canvas to output rectangle }
    scale := Min(W, H) / 64.0;

    for i := 0 to High(FShapes) do
    begin
      shape := FShapes[i];

      { Build canvas transform for this shape }
      agg.resetTransformations;
      agg.scale(scale, scale);

      trp := @tr;
      if shape.HasTransform then
      begin
        tr.affineMatrix[0] := shape.Transform[0];
        tr.affineMatrix[1] := shape.Transform[1];
        tr.affineMatrix[2] := shape.Transform[2];
        tr.affineMatrix[3] := shape.Transform[3];
        tr.affineMatrix[4] := shape.Transform[4];
        tr.affineMatrix[5] := shape.Transform[5];
        agg.affine(trp);
      end
      else if shape.HasTranslation then
      begin
        tr.affineMatrix[0] := 1.0;
        tr.affineMatrix[1] := 0.0;
        tr.affineMatrix[2] := 0.0;
        tr.affineMatrix[3] := 1.0;
        tr.affineMatrix[4] := shape.TranslateX;
        tr.affineMatrix[5] := shape.TranslateY;
        agg.affine(trp);
      end;

      { Apply fill style }
      ApplyStyleFill(agg, shape.StyleIndex, scale);

      { Render each path referenced by this shape }
      for pidx := 0 to High(shape.PathIndices) do
      begin
        pathIdx := shape.PathIndices[pidx];
        if pathIdx >= Length(FPaths) then
          Continue;
        RenderPath(agg, FPaths[pathIdx]);
        agg.drawPath(FillOnly);
      end;
    end;

  finally
    agg.Destruct;
  end;

  { Copy BGRA32 buffer to TfpgImage.
    On little-endian: BGRA32 bytes = ARGB uint32 = byte-compatible. }
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


end.
