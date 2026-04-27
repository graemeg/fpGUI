{
    This unit is part of the fpGUI Toolkit project.

    Description:
      Unit tests for fpg_hvif_writer.pas — the HVIF binary serialiser.
      Tests cover:
        - Standalone HvifEncodeCoord / HvifEncodeFloat24 helpers
        - Per-section serialisation (styles, paths, shapes)
        - Round-trip: writer output parsed successfully by THvifIcon
}

unit tchvif_writer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  fpcunit, testregistry,
  fpg_hvif,
  fpg_hvif_writer;

type
  TTestHvifWriter = class(TTestCase)
  private
    FWriter: THvifWriter;

    { Build a solid-colour THvifStyle record }
    function MakeSolidStyle(R, G, B, A: Byte): THvifStyle;
    { Build a THvifStyle with hstSolidColorNoAlpha }
    function MakeSolidStyleNoAlpha(R, G, B: Byte): THvifStyle;
    { Build a THvifStyle with hstSolidGray }
    function MakeGrayStyle(K, A: Byte): THvifStyle;
    { Build a THvifStyle with hstSolidGrayNoAlpha }
    function MakeGrayStyleNoAlpha(K: Byte): THvifStyle;
    { Build a 2-stop linear gradient THvifStyle }
    function MakeLinearGradientStyle(R1, G1, B1, A1, R2, G2, B2, A2: Byte): THvifStyle;
    { Build a closed path from flat (x,y) pairs — all line points }
    function MakeLinePath(const AXY: array of Single; AClosed: Boolean): THvifPath;
    { Build a path with a single bezier segment }
    function MakeCurvePath: THvifPath;
    { Build a shape referencing style 0 and path 0 }
    function MakeBasicShape: THvifShape;

    { Parse bytes produced by FWriter.SaveToBytes using THvifIcon }
    function ParseWriterOutput: THvifIcon;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    { ---- Standalone encode helpers ---- }
    procedure TestEncodeCoordZero;
    procedure TestEncodeCoordNegativeEdge;
    procedure TestEncodeCoordPositiveEdge;
    procedure TestEncodeCoordOutOfRange;
    procedure TestEncodeCoordFractional;
    procedure TestEncodeFloat24Zero;
    procedure TestEncodeFloat24One;
    procedure TestEncodeFloat24NegOne;
    procedure TestEncodeFloat24Half;

    { ---- Writer output structure ---- }
    procedure TestEmptyIconMagic;
    procedure TestEmptyIconLength;
    procedure TestEmptyIconCounts;

    { ---- Style serialisation ---- }
    procedure TestWriteSolidColor;
    procedure TestWriteSolidColorNoAlpha;
    procedure TestWriteSolidGray;
    procedure TestWriteSolidGrayNoAlpha;
    procedure TestWriteGradientStyle;

    { ---- Path serialisation ---- }
    procedure TestWritePathNoCurvesFlagSet;
    procedure TestWritePathCurvesFlagAbsent;
    procedure TestWritePathClosedFlag;

    { ---- Shape serialisation ---- }
    procedure TestWriteShapeNoFlags;
    procedure TestWriteShapeTranslationFlag;
    procedure TestWriteShapeTransformFlag;

    { ---- Round-trip ---- }
    procedure TestRoundTripParseable;
    procedure TestRoundTripCounts;
  end;

procedure RegisterTests;

implementation

{ ===================================================================
  Helpers
  =================================================================== }

function TTestHvifWriter.MakeSolidStyle(R, G, B, A: Byte): THvifStyle;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.StyleType := hstSolidColor;
  Result.Color.R := R;
  Result.Color.G := G;
  Result.Color.B := B;
  Result.Color.A := A;
end;

function TTestHvifWriter.MakeSolidStyleNoAlpha(R, G, B: Byte): THvifStyle;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.StyleType := hstSolidColorNoAlpha;
  Result.Color.R := R;
  Result.Color.G := G;
  Result.Color.B := B;
  Result.Color.A := 255;
end;

function TTestHvifWriter.MakeGrayStyle(K, A: Byte): THvifStyle;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.StyleType := hstSolidGray;
  Result.Color.R := K;
  Result.Color.G := K;
  Result.Color.B := K;
  Result.Color.A := A;
end;

function TTestHvifWriter.MakeGrayStyleNoAlpha(K: Byte): THvifStyle;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.StyleType := hstSolidGrayNoAlpha;
  Result.Color.R := K;
  Result.Color.G := K;
  Result.Color.B := K;
  Result.Color.A := 255;
end;

function TTestHvifWriter.MakeLinearGradientStyle(
  R1, G1, B1, A1, R2, G2, B2, A2: Byte): THvifStyle;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.StyleType    := hstGradient;
  Result.GradientType := hgtLinear;
  Result.HasGradTransform := False;
  SetLength(Result.Stops, 2);
  Result.Stops[0].Offset   := 0.0;
  Result.Stops[0].Color.R  := R1;
  Result.Stops[0].Color.G  := G1;
  Result.Stops[0].Color.B  := B1;
  Result.Stops[0].Color.A  := A1;
  Result.Stops[1].Offset   := 1.0;
  Result.Stops[1].Color.R  := R2;
  Result.Stops[1].Color.G  := G2;
  Result.Stops[1].Color.B  := B2;
  Result.Stops[1].Color.A  := A2;
end;

function TTestHvifWriter.MakeLinePath(const AXY: array of Single;
  AClosed: Boolean): THvifPath;
var
  i, nPts: Integer;
  pt: THvifPoint;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Closed := AClosed;
  nPts := Length(AXY) div 2;
  SetLength(Result.Points, nPts);
  for i := 0 to nPts - 1 do
  begin
    pt.X   := AXY[i * 2];
    pt.Y   := AXY[i * 2 + 1];
    pt.InX := pt.X; pt.InY  := pt.Y;
    pt.OutX:= pt.X; pt.OutY := pt.Y;
    Result.Points[i] := pt;
  end;
end;

function TTestHvifWriter.MakeCurvePath: THvifPath;
var
  pt: THvifPoint;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Closed := False;
  SetLength(Result.Points, 2);

  pt.X := 0.0;  pt.Y := 0.0;
  pt.InX := 0.0; pt.InY := 0.0;
  pt.OutX := 10.0; pt.OutY := 5.0;   { control point differs → curve }
  Result.Points[0] := pt;

  pt.X := 32.0; pt.Y := 0.0;
  pt.InX := 22.0; pt.InY := -5.0;   { incoming control differs → curve }
  pt.OutX := pt.X; pt.OutY := pt.Y;
  Result.Points[1] := pt;
end;

function TTestHvifWriter.MakeBasicShape: THvifShape;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.StyleIndex     := 0;
  Result.HasTransform   := False;
  Result.HasTranslation := False;
  SetLength(Result.PathIndices, 1);
  Result.PathIndices[0] := 0;
end;

function TTestHvifWriter.ParseWriterOutput: THvifIcon;
var
  bytes: TBytes;
  ms: TMemoryStream;
begin
  bytes := FWriter.SaveToBytes;
  ms := TMemoryStream.Create;
  try
    if Length(bytes) > 0 then
      ms.WriteBuffer(bytes[0], Length(bytes));
    ms.Position := 0;
    Result := THvifIcon.CreateFromStream(ms);
  finally
    ms.Free;
  end;
end;

procedure TTestHvifWriter.SetUp;
begin
  FWriter := THvifWriter.Create;
end;

procedure TTestHvifWriter.TearDown;
begin
  FreeAndNil(FWriter);
end;


{ ===================================================================
  Standalone encode helper tests
  =================================================================== }

procedure TTestHvifWriter.TestEncodeCoordZero;
var
  b: TBytes;
begin
  b := HvifEncodeCoord(0.0);
  CheckEquals(1, Length(b), 'Length should be 1');
  CheckEquals(32, Integer(b[0]), '0.0 encodes as 32');
end;

procedure TTestHvifWriter.TestEncodeCoordNegativeEdge;
var
  b: TBytes;
begin
  { -32.0 is the minimum 1-byte value }
  b := HvifEncodeCoord(-32.0);
  CheckEquals(1, Length(b), 'Length should be 1');
  CheckEquals(0, Integer(b[0]), '-32.0 encodes as 0');
end;

procedure TTestHvifWriter.TestEncodeCoordPositiveEdge;
var
  b: TBytes;
begin
  { 95.0 is the maximum 1-byte value }
  b := HvifEncodeCoord(95.0);
  CheckEquals(1, Length(b), 'Length should be 1');
  CheckEquals(127, Integer(b[0]), '95.0 encodes as 127');
end;

procedure TTestHvifWriter.TestEncodeCoordOutOfRange;
var
  b: TBytes;
  decoded: Single;
begin
  { 100.0 is an integer but exceeds 95 — must use 2 bytes }
  b := HvifEncodeCoord(100.0);
  CheckEquals(2, Length(b), 'Length should be 2');
  CheckTrue((b[0] and $80) <> 0, 'High bit set in first byte');
  { Verify round-trip via reader formula }
  decoded := ((Integer(b[0] and $7F) shl 8) or Integer(b[1])) / 102.0 - 128.0;
  CheckTrue(Abs(decoded - 100.0) < 0.01, 'Round-trip 100.0');
end;

procedure TTestHvifWriter.TestEncodeCoordFractional;
var
  b: TBytes;
  decoded: Single;
begin
  { 0.5 is in [-32, 95] but is NOT an integer — must use 2 bytes }
  b := HvifEncodeCoord(0.5);
  CheckEquals(2, Length(b), 'Length should be 2');
  CheckTrue((b[0] and $80) <> 0, 'High bit set in first byte');
  decoded := ((Integer(b[0] and $7F) shl 8) or Integer(b[1])) / 102.0 - 128.0;
  CheckTrue(Abs(decoded - 0.5) < 0.01, 'Round-trip 0.5');
end;

procedure TTestHvifWriter.TestEncodeFloat24Zero;
var
  b: TBytes;
begin
  b := HvifEncodeFloat24(0.0);
  CheckEquals(3, Length(b), 'Length must be 3');
  CheckEquals($00, Integer(b[0]), 'b[0]');
  CheckEquals($00, Integer(b[1]), 'b[1]');
  CheckEquals($00, Integer(b[2]), 'b[2]');
end;

procedure TTestHvifWriter.TestEncodeFloat24One;
var
  b: TBytes;
begin
  { 1.0: sign=0, exp=0 → (0+32)<<17 = $400000, mantissa=0 → $40 $00 $00 }
  b := HvifEncodeFloat24(1.0);
  CheckEquals(3, Length(b), 'Length must be 3');
  CheckEquals($40, Integer(b[0]), 'b[0]');
  CheckEquals($00, Integer(b[1]), 'b[1]');
  CheckEquals($00, Integer(b[2]), 'b[2]');
end;

procedure TTestHvifWriter.TestEncodeFloat24NegOne;
var
  b: TBytes;
begin
  { -1.0: sign=1, exp=0 → $C00000 → $C0 $00 $00 }
  b := HvifEncodeFloat24(-1.0);
  CheckEquals(3, Length(b), 'Length must be 3');
  CheckEquals($C0, Integer(b[0]), 'b[0]');
  CheckEquals($00, Integer(b[1]), 'b[1]');
  CheckEquals($00, Integer(b[2]), 'b[2]');
end;

procedure TTestHvifWriter.TestEncodeFloat24Half;
var
  b: TBytes;
begin
  { 0.5: sign=0, exp=-1 → (-1+32)<<17 = 31<<17 = $3E0000 → $3E $00 $00 }
  b := HvifEncodeFloat24(0.5);
  CheckEquals(3, Length(b), 'Length must be 3');
  CheckEquals($3E, Integer(b[0]), 'b[0]');
  CheckEquals($00, Integer(b[1]), 'b[1]');
  CheckEquals($00, Integer(b[2]), 'b[2]');
end;


{ ===================================================================
  Writer output structure tests
  =================================================================== }

procedure TTestHvifWriter.TestEmptyIconMagic;
var
  b: TBytes;
begin
  b := FWriter.SaveToBytes;
  CheckTrue(Length(b) >= 7, 'At least 7 bytes');
  CheckEquals($6E, Integer(b[0]), 'magic[0]');
  CheckEquals($63, Integer(b[1]), 'magic[1]');
  CheckEquals($69, Integer(b[2]), 'magic[2]');
  CheckEquals($66, Integer(b[3]), 'magic[3]');
end;

procedure TTestHvifWriter.TestEmptyIconLength;
var
  b: TBytes;
begin
  { Empty icon = 4 (magic) + 1 (style count) + 1 (path count) + 1 (shape count) = 7 bytes }
  b := FWriter.SaveToBytes;
  CheckEquals(7, Length(b), 'Empty icon must be 7 bytes');
end;

procedure TTestHvifWriter.TestEmptyIconCounts;
var
  b: TBytes;
begin
  b := FWriter.SaveToBytes;
  CheckEquals(0, Integer(b[4]), 'Style count byte');
  CheckEquals(0, Integer(b[5]), 'Path count byte');
  CheckEquals(0, Integer(b[6]), 'Shape count byte');
end;


{ ===================================================================
  Style serialisation tests
  =================================================================== }

procedure TTestHvifWriter.TestWriteSolidColor;
var
  b: TBytes;
begin
  FWriter.AddStyle(MakeSolidStyle(255, 128, 0, 200));
  b := FWriter.SaveToBytes;
  { magic(4) + styleCount(1) + type(1) + RGBA(4) + pathCount(1) + shapeCount(1) = 12 }
  CheckEquals(12, Length(b), 'Total length');
  CheckEquals(1,   Integer(b[4]), 'Style count byte');
  CheckEquals(1,   Integer(b[5]), 'Style type = SOLID_COLOR');
  CheckEquals(255, Integer(b[6]), 'R');
  CheckEquals(128, Integer(b[7]), 'G');
  CheckEquals(0,   Integer(b[8]), 'B');
  CheckEquals(200, Integer(b[9]), 'A');
end;

procedure TTestHvifWriter.TestWriteSolidColorNoAlpha;
var
  b: TBytes;
begin
  FWriter.AddStyle(MakeSolidStyleNoAlpha(10, 20, 30));
  b := FWriter.SaveToBytes;
  { magic(4) + styleCount(1) + type(1) + RGB(3) + pathCount(1) + shapeCount(1) = 11 }
  CheckEquals(11, Length(b), 'Total length');
  CheckEquals(3,  Integer(b[5]), 'Style type = SOLID_COLOR_NO_ALPHA');
  CheckEquals(10, Integer(b[6]), 'R');
  CheckEquals(20, Integer(b[7]), 'G');
  CheckEquals(30, Integer(b[8]), 'B');
end;

procedure TTestHvifWriter.TestWriteSolidGray;
var
  b: TBytes;
begin
  FWriter.AddStyle(MakeGrayStyle(128, 200));
  b := FWriter.SaveToBytes;
  { magic(4) + styleCount(1) + type(1) + K(1) + A(1) + pathCount(1) + shapeCount(1) = 10 }
  CheckEquals(10,  Length(b), 'Total length');
  CheckEquals(4,   Integer(b[5]), 'Style type = SOLID_GRAY');
  CheckEquals(128, Integer(b[6]), 'K');
  CheckEquals(200, Integer(b[7]), 'A');
end;

procedure TTestHvifWriter.TestWriteSolidGrayNoAlpha;
var
  b: TBytes;
begin
  FWriter.AddStyle(MakeGrayStyleNoAlpha(64));
  b := FWriter.SaveToBytes;
  { magic(4) + styleCount(1) + type(1) + K(1) + pathCount(1) + shapeCount(1) = 9 }
  CheckEquals(9,  Length(b), 'Total length');
  CheckEquals(5,  Integer(b[5]), 'Style type = SOLID_GRAY_NO_ALPHA');
  CheckEquals(64, Integer(b[6]), 'K');
end;

procedure TTestHvifWriter.TestWriteGradientStyle;
var
  icon: THvifIcon;
begin
  { Add a gradient style and verify the output parses without error.
    Gradient: type=LINEAR, no transform, 2 RGBA stops. }
  FWriter.AddStyle(MakeLinearGradientStyle(255, 0, 0, 255, 0, 0, 255, 255));
  icon := ParseWriterOutput;
  try
    CheckEquals(1, icon.StyleCount, 'Parsed style count');
    CheckEquals(0, icon.PathCount,  'Parsed path count');
  finally
    icon.Free;
  end;
end;


{ ===================================================================
  Path serialisation tests
  =================================================================== }

procedure TTestHvifWriter.TestWritePathNoCurvesFlagSet;
var
  b: TBytes;
  pathFlagsOffset: Integer;
begin
  { One line-only path: flags byte must have PATH_FLAG_NO_CURVES (bit 3) set.
    Byte layout: magic(4) + styleCount(1=0) + pathCount(1=1) + pathFlags(1) }
  FWriter.AddPath(MakeLinePath([0.0, 0.0, 32.0, 0.0, 32.0, 32.0], True));
  b := FWriter.SaveToBytes;
  pathFlagsOffset := 6;   { [4]=0 styles, [5]=1 path, [6]=path flags }
  CheckEquals(1, Integer(b[5]), 'Path count byte');
  CheckTrue((Integer(b[pathFlagsOffset]) and (1 shl 3)) <> 0,
    'PATH_FLAG_NO_CURVES (bit 3) must be set for line-only path');
end;

procedure TTestHvifWriter.TestWritePathCurvesFlagAbsent;
var
  b: TBytes;
  pathFlagsOffset: Integer;
begin
  { A path with bezier control points must NOT have NO_CURVES or USES_COMMANDS set }
  FWriter.AddPath(MakeCurvePath);
  b := FWriter.SaveToBytes;
  pathFlagsOffset := 6;
  CheckEquals(1, Integer(b[5]), 'Path count byte');
  CheckTrue((Integer(b[pathFlagsOffset]) and (1 shl 3)) = 0,
    'PATH_FLAG_NO_CURVES (bit 3) must be absent for curve path');
  CheckTrue((Integer(b[pathFlagsOffset]) and (1 shl 2)) = 0,
    'PATH_FLAG_USES_COMMANDS (bit 2) must be absent');
end;

procedure TTestHvifWriter.TestWritePathClosedFlag;
var
  b: TBytes;
  pathFlagsOffset: Integer;
begin
  { Closed path must have PATH_FLAG_CLOSED (bit 1) set }
  FWriter.AddPath(MakeLinePath([0.0, 0.0, 32.0, 32.0], True));
  b := FWriter.SaveToBytes;
  pathFlagsOffset := 6;
  CheckTrue((Integer(b[pathFlagsOffset]) and (1 shl 1)) <> 0,
    'PATH_FLAG_CLOSED (bit 1) must be set');
end;


{ ===================================================================
  Shape serialisation tests
  =================================================================== }

procedure TTestHvifWriter.TestWriteShapeNoFlags;
var
  icon: THvifIcon;
begin
  FWriter.AddStyle(MakeSolidStyle(255, 0, 0, 255));
  FWriter.AddPath(MakeLinePath([0.0, 0.0, 32.0, 32.0], False));
  FWriter.AddShape(MakeBasicShape);
  icon := ParseWriterOutput;
  try
    CheckEquals(1, icon.StyleCount, 'Style count');
    CheckEquals(1, icon.PathCount,  'Path count');
    CheckEquals(1, icon.ShapeCount, 'Shape count');
  finally
    icon.Free;
  end;
end;

procedure TTestHvifWriter.TestWriteShapeTranslationFlag;
var
  sh: THvifShape;
  icon: THvifIcon;
begin
  FWriter.AddStyle(MakeSolidStyle(255, 0, 0, 255));
  FWriter.AddPath(MakeLinePath([0.0, 0.0, 10.0, 10.0], False));
  sh := MakeBasicShape;
  sh.HasTranslation := True;
  sh.TranslateX     := 8.0;
  sh.TranslateY     := -4.0;
  FWriter.AddShape(sh);
  icon := ParseWriterOutput;
  try
    CheckEquals(1, icon.ShapeCount, 'Shape count');
  finally
    icon.Free;
  end;
end;

procedure TTestHvifWriter.TestWriteShapeTransformFlag;
var
  sh: THvifShape;
  icon: THvifIcon;
begin
  FWriter.AddStyle(MakeSolidStyle(0, 255, 0, 255));
  FWriter.AddPath(MakeLinePath([0.0, 0.0, 32.0, 32.0], False));
  sh := MakeBasicShape;
  sh.HasTransform   := True;
  { Identity matrix in agg order: [sx, shy, shx, sy, tx, ty] = [1,0,0,1,0,0] }
  sh.Transform[0] := 1.0;
  sh.Transform[1] := 0.0;
  sh.Transform[2] := 0.0;
  sh.Transform[3] := 1.0;
  sh.Transform[4] := 0.0;
  sh.Transform[5] := 0.0;
  FWriter.AddShape(sh);
  icon := ParseWriterOutput;
  try
    CheckEquals(1, icon.ShapeCount, 'Shape count');
  finally
    icon.Free;
  end;
end;


{ ===================================================================
  Round-trip tests
  =================================================================== }

procedure TTestHvifWriter.TestRoundTripParseable;
var
  icon: THvifIcon;
begin
  FWriter.AddStyle(MakeSolidStyle(200, 100, 50, 255));
  FWriter.AddPath(MakeLinePath([0.0, 0.0, 32.0, 0.0, 32.0, 32.0], True));
  FWriter.AddShape(MakeBasicShape);
  icon := ParseWriterOutput;
  try
    CheckNotNull(icon, 'Icon was created');
  finally
    icon.Free;
  end;
end;

procedure TTestHvifWriter.TestRoundTripCounts;
var
  icon: THvifIcon;
begin
  FWriter.AddStyle(MakeSolidStyle(255, 255, 0, 255));
  FWriter.AddStyle(MakeSolidStyleNoAlpha(0, 128, 255));
  FWriter.AddPath(MakeLinePath([0.0, 0.0, 32.0, 32.0], False));
  FWriter.AddPath(MakeCurvePath);
  FWriter.AddShape(MakeBasicShape);
  icon := ParseWriterOutput;
  try
    CheckEquals(2, icon.StyleCount, 'Style count preserved');
    CheckEquals(2, icon.PathCount,  'Path count preserved');
    CheckEquals(1, icon.ShapeCount, 'Shape count preserved');
  finally
    icon.Free;
  end;
end;


{ ===================================================================
  Registration
  =================================================================== }

procedure RegisterTests;
begin
  RegisterTest(TTestHvifWriter);
end;

initialization
  RegisterTests;

end.
