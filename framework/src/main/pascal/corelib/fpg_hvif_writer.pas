{
    Copyright (c) 2026 Graeme Geldenhuys

    This program is part of the fpGUI Toolkit project.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      HVIF (Haiku Vector Icon Format) binary serialiser for fpGUI.
      Inverse of the reader in fpg_hvif.pas — serialises THvifStyle,
      THvifPath and THvifShape data structures to a valid HVIF byte stream.

    Canonical format references (Haiku r1beta5):
      src/apps/icon-o-matic/import_export/flat_icon/FlatIconExporter.cpp
      src/libs/icon/flat_icon/FlatIconFormat.cpp  (write_coord, write_float_24)

    Encoding notes:
      - WriteCoord: 1-byte when value is an integer in [-32, 95]; else 2 bytes
          1-byte: byte = round(coord) + 32
          2-byte: uint16 = round((coord+128)*102) | $8000, written high byte first
      - WriteFloat24: 24-bit custom float — 1 sign, 6 exp (bias 32), 17 mantissa
          If exponent outside [-32, 31]: write $000000 (clamp to zero)
      - Path encoding: NO_CURVES when all control points equal their main point;
          CURVES (6 coords/point) otherwise. COMMANDS format not used in v1.
      - Gradient stops always written with full RGBA (no gray/no-alpha optimisation
          in v1; semantically identical to GRADIENT_FLAG_NO_ALPHA variants).

    Limitations (v1):
      - No COMMANDS path format (writer always uses NO_CURVES or CURVES).
      - Gradient stops always written with alpha and full RGB.
      - LOD-scale emitted when min > 0 or max < 4.0 (non-default bounds only).
      - Big-endian targets are NOT supported.
}

unit fpg_hvif_writer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_hvif_model;


{ =========================================================
  Standalone encoding helpers (also exposed for testing)
  ========================================================= }

{ Encodes a single coordinate to 1 or 2 bytes.
  1 byte  when value is an integer in [-32..95].
  2 bytes otherwise (bit 15 of the uint16 is set as extension flag). }
function HvifEncodeCoord(AValue: Single): TBytes;

{ Encodes a single-precision float to exactly 3 bytes using
  the HVIF 24-bit custom float format. }
function HvifEncodeFloat24(AValue: Single): TBytes;


{ =========================================================
  THvifWriter — builds an HVIF byte stream
  ========================================================= }

type
  THvifWriter = class
  private
    FStyles: array of THvifStyle;
    FPaths:  array of THvifPath;
    FShapes: array of THvifShape;

    { Write buffer (valid only during a SaveToBytes/SaveToStream call) }
    FStream: TMemoryStream;

    { Low-level write primitives }
    procedure WByte(AValue: Byte);
    procedure WCoord(AValue: Single);
    procedure WFloat24(AValue: Single);

    { Section writers }
    procedure WriteStyles;
    procedure WritePaths;
    procedure WriteShapes;

    { Per-element writers }
    procedure WriteStyle(const ASt: THvifStyle);
    procedure WriteGradient(const ASt: THvifStyle);
    procedure WritePath(const APth: THvifPath);
    procedure WriteShape(const ASh: THvifShape);

  public
    { Append a style/path/shape to the writer's internal lists.
      Records are deep-copied (dynamic-array sub-fields included). }
    procedure AddStyle(const AStyle: THvifStyle);
    procedure AddPath(const APath: THvifPath);
    procedure AddShape(const AShape: THvifShape);

    { Serialise the accumulated data to a TBytes buffer. }
    function  SaveToBytes: TBytes;

    { Serialise and write directly into AStream. }
    procedure SaveToStream(AStream: TStream);

    { Serialise and write to a file on disk. }
    procedure SaveToFile(const AFileName: string);
  end;


implementation

uses Math;


{ =========================================================
  Standalone encoding helpers
  ========================================================= }

function HvifEncodeCoord(AValue: Single): TBytes;
var
  v: Word;
begin
  { Clamp to the overall supported range }
  if AValue < -128.0 then AValue := -128.0;
  if AValue >  192.0 then AValue :=  192.0;

  { 1-byte encoding when value is an integer in [-32, 95].
    Integer test: Trunc(v*100) = Trunc(v)*100 (matches Haiku write_coord) }
  if (Trunc(AValue * 100.0) = Trunc(AValue) * 100) and
     (AValue >= -32.0) and (AValue <= 95.0) then
  begin
    SetLength(Result, 1);
    Result[0] := Byte(Round(AValue) + 32);
  end
  else
  begin
    { 2-byte encoding: uint16 = round((coord+128)*102) with bit 15 set }
    v := Word(Round((AValue + 128.0) * 102.0)) or $8000;
    SetLength(Result, 2);
    Result[0] := (v shr 8) and $FF;   { high byte written first }
    Result[1] := v and $FF;
  end;
end;


function HvifEncodeFloat24(AValue: Single): TBytes;
var
  U: packed record
       case Integer of
         0: (I: LongWord);
         1: (F: Single);
     end;
  sign, exponent: Integer;
  mantissa, shortVal: LongWord;
begin
  SetLength(Result, 3);

  if AValue = 0.0 then
  begin
    Result[0] := 0; Result[1] := 0; Result[2] := 0;
    Exit;
  end;

  U.F := AValue;
  sign     := Integer((U.I shr 31) and 1);
  exponent := Integer((U.I shr 23) and $FF) - 127;
  mantissa := U.I and $007FFFFF;

  { Values whose exponent is outside [-32, 31] cannot be represented;
    emit zero as the Haiku exporter does in this case. }
  if (exponent >= 32) or (exponent < -32) then
  begin
    Result[0] := 0; Result[1] := 0; Result[2] := 0;
    Exit;
  end;

  { Pack into 24 bits: 1 sign | 6 exp (bias 32) | 17 mantissa }
  shortVal := LongWord(sign shl 23)
            or LongWord((exponent + 32) shl 17)
            or LongWord(mantissa shr 6);

  Result[0] := (shortVal shr 16) and $FF;
  Result[1] := (shortVal shr  8) and $FF;
  Result[2] :=  shortVal         and $FF;
end;


{ =========================================================
  THvifWriter — private primitives
  ========================================================= }

procedure THvifWriter.WByte(AValue: Byte);
begin
  FStream.WriteBuffer(AValue, 1);
end;

procedure THvifWriter.WCoord(AValue: Single);
var
  b: TBytes;
begin
  b := HvifEncodeCoord(AValue);
  FStream.WriteBuffer(b[0], Length(b));
end;

procedure THvifWriter.WFloat24(AValue: Single);
var
  b: TBytes;
begin
  b := HvifEncodeFloat24(AValue);
  FStream.WriteBuffer(b[0], 3);
end;


{ =========================================================
  THvifWriter — section writers
  ========================================================= }

procedure THvifWriter.WriteGradient(const ASt: THvifStyle);
const
  GRADIENT_FLAG_TRANSFORM = 1 shl 1;
var
  flags: Byte;
  stopOffset: Byte;
  i: Integer;
begin
  WByte(Byte(Ord(ASt.GradientType)));

  flags := 0;
  if ASt.HasGradTransform then
    flags := flags or GRADIENT_FLAG_TRANSFORM;
  WByte(flags);

  WByte(Byte(Length(ASt.Stops)));

  if ASt.HasGradTransform then
    for i := 0 to 5 do
      WFloat24(ASt.GradTransform[i]);

  { Write stops with full RGBA — no gray/no-alpha optimisation in v1 }
  for i := 0 to High(ASt.Stops) do
  begin
    stopOffset := Byte(Round(ASt.Stops[i].Offset * 255.0));
    WByte(stopOffset);
    WByte(ASt.Stops[i].Color.R);
    WByte(ASt.Stops[i].Color.G);
    WByte(ASt.Stops[i].Color.B);
    WByte(ASt.Stops[i].Color.A);
  end;
end;


procedure THvifWriter.WriteStyle(const ASt: THvifStyle);
begin
  case ASt.StyleType of
    hstSolidColor:
      begin
        WByte(1);  { STYLE_TYPE_SOLID_COLOR }
        WByte(ASt.Color.R);
        WByte(ASt.Color.G);
        WByte(ASt.Color.B);
        WByte(ASt.Color.A);
      end;

    hstSolidColorNoAlpha:
      begin
        WByte(3);  { STYLE_TYPE_SOLID_COLOR_NO_ALPHA }
        WByte(ASt.Color.R);
        WByte(ASt.Color.G);
        WByte(ASt.Color.B);
      end;

    hstSolidGray:
      begin
        WByte(4);  { STYLE_TYPE_SOLID_GRAY }
        WByte(ASt.Color.R);  { K }
        WByte(ASt.Color.A);
      end;

    hstSolidGrayNoAlpha:
      begin
        WByte(5);  { STYLE_TYPE_SOLID_GRAY_NO_ALPHA }
        WByte(ASt.Color.R);  { K }
      end;

    hstGradient:
      begin
        WByte(2);  { STYLE_TYPE_GRADIENT }
        WriteGradient(ASt);
      end;

    else
      { Unknown style type — emit as transparent solid colour }
      begin
        WByte(1);
        WByte(0); WByte(0); WByte(0); WByte(0);
      end;
  end;
end;


procedure THvifWriter.WriteStyles;
var
  i: Integer;
begin
  WByte(Byte(Length(FStyles)));
  for i := 0 to High(FStyles) do
    WriteStyle(FStyles[i]);
end;


procedure THvifWriter.WritePath(const APth: THvifPath);
var
  flags: Byte;
  isNoCurves: Boolean;
  i: Integer;
  pt: THvifPoint;
begin
  { Determine encoding: NO_CURVES when all control points equal their main point }
  isNoCurves := True;
  for i := 0 to High(APth.Points) do
  begin
    pt := APth.Points[i];
    if (pt.InX <> pt.X) or (pt.InY <> pt.Y) or
       (pt.OutX <> pt.X) or (pt.OutY <> pt.Y) then
    begin
      isNoCurves := False;
      Break;
    end;
  end;

  flags := 0;
  if APth.Closed    then flags := flags or (1 shl 1);  { PATH_FLAG_CLOSED }
  if isNoCurves     then flags := flags or (1 shl 3);  { PATH_FLAG_NO_CURVES }

  WByte(flags);
  WByte(Byte(Length(APth.Points)));

  if isNoCurves then
  begin
    { 2 coords per point }
    for i := 0 to High(APth.Points) do
    begin
      WCoord(APth.Points[i].X);
      WCoord(APth.Points[i].Y);
    end;
  end
  else
  begin
    { 6 coords per point: main + incoming control + outgoing control }
    for i := 0 to High(APth.Points) do
    begin
      WCoord(APth.Points[i].X);
      WCoord(APth.Points[i].Y);
      WCoord(APth.Points[i].InX);
      WCoord(APth.Points[i].InY);
      WCoord(APth.Points[i].OutX);
      WCoord(APth.Points[i].OutY);
    end;
  end;
end;


procedure THvifWriter.WritePaths;
var
  i: Integer;
begin
  WByte(Byte(Length(FPaths)));
  for i := 0 to High(FPaths) do
    WritePath(FPaths[i]);
end;


procedure THvifWriter.WriteShape(const ASh: THvifShape);
const
  SHAPE_TYPE_PATH_SOURCE      = 10;
  SHAPE_FLAG_TRANSFORM        = 1 shl 1;
  SHAPE_FLAG_LOD_SCALE        = 1 shl 3;
  SHAPE_FLAG_HAS_TRANSFORMERS = 1 shl 4;
  SHAPE_FLAG_TRANSLATION      = 1 shl 5;
  TRANSFORMER_TYPE_STROKE     = 23;
  LOD_SCALE_FACTOR            = 63.75;   { byte = Round(scale * factor) }
var
  flags: Byte;
  i: Integer;
  emitLOD: Boolean;
begin
  WByte(SHAPE_TYPE_PATH_SOURCE);
  WByte(ASh.StyleIndex);
  WByte(Byte(Length(ASh.PathIndices)));
  for i := 0 to High(ASh.PathIndices) do
    WByte(ASh.PathIndices[i]);

  { Emit LOD only when the bounds are non-default (min>0 or max<4). }
  emitLOD := ASh.HasLODScale and ((ASh.MinVisScale > 0.0) or (ASh.MaxVisScale < 4.0));

  flags := 0;
  if ASh.HasTransform then
    flags := flags or SHAPE_FLAG_TRANSFORM
  else if ASh.HasTranslation then
    flags := flags or SHAPE_FLAG_TRANSLATION;
  if emitLOD then
    flags := flags or SHAPE_FLAG_LOD_SCALE;
  if ASh.HasStroke then
    flags := flags or SHAPE_FLAG_HAS_TRANSFORMERS;
  WByte(flags);

  if ASh.HasTransform then
    for i := 0 to 5 do
      WFloat24(ASh.Transform[i])
  else if ASh.HasTranslation then
  begin
    WCoord(ASh.TranslateX);
    WCoord(ASh.TranslateY);
  end;

  { LOD section comes after transform, before transformers (per HVIF spec) }
  if emitLOD then
  begin
    WByte(Byte(Round(ASh.MinVisScale * LOD_SCALE_FACTOR)));
    WByte(Byte(Round(ASh.MaxVisScale * LOD_SCALE_FACTOR)));
  end;

  if ASh.HasStroke then
  begin
    WByte(1);                       { transformer count }
    WByte(TRANSFORMER_TYPE_STROKE); { transformer type  }
    { 3 raw uint8 bytes — matches Haiku FlatIconImporter.cpp:
        width = Round(float + 128), lineOptions (join | cap<<4), miterLimit }
    WByte(Byte(Round(ASh.StrokeWidth + 128.0)));
    WByte((ASh.StrokeLineCap shl 4) or ASh.StrokeLineJoin);
    WByte(Byte(Round(ASh.StrokeMiterLimit)));
  end;
end;


procedure THvifWriter.WriteShapes;
var
  i: Integer;
begin
  WByte(Byte(Length(FShapes)));
  for i := 0 to High(FShapes) do
    WriteShape(FShapes[i]);
end;


{ =========================================================
  THvifWriter — public API
  ========================================================= }

procedure THvifWriter.AddStyle(const AStyle: THvifStyle);
begin
  SetLength(FStyles, Length(FStyles) + 1);
  FStyles[High(FStyles)] := AStyle;
end;

procedure THvifWriter.AddPath(const APath: THvifPath);
begin
  SetLength(FPaths, Length(FPaths) + 1);
  FPaths[High(FPaths)] := APath;
end;

procedure THvifWriter.AddShape(const AShape: THvifShape);
begin
  SetLength(FShapes, Length(FShapes) + 1);
  FShapes[High(FShapes)] := AShape;
end;

function THvifWriter.SaveToBytes: TBytes;
begin
  FStream := TMemoryStream.Create;
  try
    FStream.WriteBuffer(HVIF_MAGIC[0], 4);
    WriteStyles;
    WritePaths;
    WriteShapes;
    SetLength(Result, FStream.Size);
    if FStream.Size > 0 then
      Move(FStream.Memory^, Result[0], FStream.Size);
  finally
    FreeAndNil(FStream);
  end;
end;

procedure THvifWriter.SaveToStream(AStream: TStream);
var
  b: TBytes;
begin
  b := SaveToBytes;
  if Length(b) > 0 then
    AStream.WriteBuffer(b[0], Length(b));
end;

procedure THvifWriter.SaveToFile(const AFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;


end.
