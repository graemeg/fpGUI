{
    svg2hvif_converter.pas — SVG to HVIF conversion logic.

    Copyright (c) 2026 Graeme Geldenhuys

    This program is part of the fpGUI Toolkit project.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Converts a subset of SVG to the Haiku Vector Icon Format (HVIF).

    Supported SVG features (v3):
      - <path> elements with 'd' attribute
      - <rect>, <circle>, <ellipse>, <polygon>, <polyline>, <line> elements
      - <g> group elements with transform propagation
      - Path commands: M m L l H h V v C c S s Q q T t A a Z z
      - Arc (A a) commands converted to cubic bezier segments
      - transform attribute: translate, scale, rotate, skewX, skewY, matrix
      - fill attribute: #RRGGBB, #RGB, rgb(R,G,B), named colours
      - fill-opacity and opacity attributes
      - style="" inline CSS (fill, fill-opacity, opacity)
      - display:none / visibility:hidden filtering
      - viewBox attribute on <svg> for coordinate scaling
      - Linear and radial gradients via <defs>/<linearGradient>/<radialGradient>
      - url(#id) fill references resolved to gradient styles
      - xlink:href gradient inheritance for stop colours
      - Style deduplication (identical styles share one HVIF style entry)

    Limitations:
      - stroke not converted
      - Gradient objectBoundingBox units use viewBox as bounding box approximation
      - HVIF renderer uses only first and last gradient stop colours

    Coordinate transform:
      SVG viewBox is fitted uniformly into the HVIF 64 x 64 unit space.
      Scale = 64 / max(viewBoxWidth, viewBoxHeight).
      Group and shape transforms are composed using full affine matrix math.
}

unit svg2hvif_converter;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math, Classes,
  fpg_hvif_model, fpg_hvif_writer;

type
  EsvgConvertError = class(Exception);

  TfpgSvgToHvif = class
  public
    { Convert ASvgFile to HVIF and write the result to AHvifFile.
      Raises EsvgConvertError on unrecoverable parse failure. }
    class procedure Convert(const ASvgFile, AHvifFile: string);
  end;


implementation

uses
  XMLRead, DOM;


{ =========================================================
  Locale-independent float conversion
  ========================================================= }

var
  GSvgFmt: TFormatSettings;


function SvgStrToFloat(const s: string): Double;
begin
  Result := StrToFloat(Trim(s), GSvgFmt);
end;

function SvgStrToFloatDef(const s: string; ADefault: Double): Double;
begin
  Result := StrToFloatDef(Trim(s), ADefault, GSvgFmt);
end;


{ =========================================================
  SVG colour record, gradient types
  ========================================================= }

type
  TSvgColor = record
    R, G, B: Byte;
    A: Byte;        { 0=transparent, 255=opaque }
    IsNone: Boolean;
  end;

  TDoubleArray = array of Double;

  TSvgGradientStop = record
    Offset: Double;
    Color:  TSvgColor;
  end;

  TSvgGradientKind = (sgkLinear, sgkRadial);

  TSvgGradientDef = class
    Kind:          TSvgGradientKind;
    GradientUnits: string;
    { Linear gradient endpoints (in gradient coordinate space) }
    X1, Y1, X2, Y2: Double;
    { Radial gradient geometry }
    CX, CY, R:    Double;
    { Gradient stops }
    Stops:         array of TSvgGradientStop;
    { xlink:href target id for stop inheritance (without leading '#') }
    HrefId:        string;
  end;

  TPathList = array of THvifPath;


{ =========================================================
  SVG colour parser
  ========================================================= }

function MakeColor(R, G, B: Byte; A: Byte = 255): TSvgColor;
begin
  Result.R := R; Result.G := G; Result.B := B; Result.A := A;
  Result.IsNone := False;
end;

function NoneColor: TSvgColor;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.IsNone := True;
end;

function HexNibble(c: Char): Byte;
begin
  case c of
    '0'..'9': Result := Ord(c) - Ord('0');
    'A'..'F': Result := Ord(c) - Ord('A') + 10;
    'a'..'f': Result := Ord(c) - Ord('a') + 10;
    else      Result := 0;
  end;
end;

function HexByte(const s: string; i: Integer): Byte;
begin
  Result := HexNibble(s[i]) shl 4 or HexNibble(s[i+1]);
end;

procedure ParseRgbArgs(const inner: string;
  out R, G, B: Byte; out A: Byte; hasAlpha: Boolean);
var
  parts: TStringList;
  i: Integer;
  vals: array[0..3] of Double;
begin
  R := 0; G := 0; B := 0; A := 255;
  parts := TStringList.Create;
  try
    parts.Delimiter := ',';
    parts.StrictDelimiter := True;
    parts.DelimitedText := inner;
    for i := 0 to Min(parts.Count - 1, 3) do
      vals[i] := SvgStrToFloatDef(parts[i], 0);
    R := Byte(Round(EnsureRange(vals[0], 0, 255)));
    G := Byte(Round(EnsureRange(vals[1], 0, 255)));
    B := Byte(Round(EnsureRange(vals[2], 0, 255)));
    if hasAlpha and (parts.Count > 3) then
      A := Byte(Round(EnsureRange(vals[3] * 255, 0, 255)));
  finally
    parts.Free;
  end;
end;

function ParseSvgColor(const s: string): TSvgColor;
var
  t, inner: string;
  R, G, B, A: Byte;
begin
  t := Trim(s);
  Result := NoneColor;

  if (t = '') or SameText(t, 'none') then Exit;

  if (Length(t) >= 1) and (t[1] = '#') then
  begin
    if Length(t) = 7 then
      Result := MakeColor(HexByte(t, 2), HexByte(t, 4), HexByte(t, 6))
    else if Length(t) = 4 then
      Result := MakeColor(
        HexNibble(t[2]) * 17,
        HexNibble(t[3]) * 17,
        HexNibble(t[4]) * 17);
    Exit;
  end;

  if SameText(Copy(t, 1, 5), 'rgba(') and (t[Length(t)] = ')') then
  begin
    inner := Copy(t, 6, Length(t) - 6);
    ParseRgbArgs(inner, R, G, B, A, True);
    Result := MakeColor(R, G, B, A);
    Exit;
  end;
  if SameText(Copy(t, 1, 4), 'rgb(') and (t[Length(t)] = ')') then
  begin
    inner := Copy(t, 5, Length(t) - 5);
    ParseRgbArgs(inner, R, G, B, A, False);
    Result := MakeColor(R, G, B);
    Exit;
  end;

  { url() references are handled separately via IsUrlRef }
  if SameText(Copy(t, 1, 4), 'url(') then Exit;

  if SameText(t, 'black')   then begin Result := MakeColor(0,   0,   0);   Exit; end;
  if SameText(t, 'white')   then begin Result := MakeColor(255, 255, 255); Exit; end;
  if SameText(t, 'red')     then begin Result := MakeColor(255, 0,   0);   Exit; end;
  if SameText(t, 'lime')    then begin Result := MakeColor(0,   255, 0);   Exit; end;
  if SameText(t, 'green')   then begin Result := MakeColor(0,   128, 0);   Exit; end;
  if SameText(t, 'blue')    then begin Result := MakeColor(0,   0,   255); Exit; end;
  if SameText(t, 'yellow')  then begin Result := MakeColor(255, 255, 0);   Exit; end;
  if SameText(t, 'cyan')    then begin Result := MakeColor(0,   255, 255); Exit; end;
  if SameText(t, 'magenta') then begin Result := MakeColor(255, 0,   255); Exit; end;
  if SameText(t, 'orange')  then begin Result := MakeColor(255, 165, 0);   Exit; end;
  if SameText(t, 'purple')  then begin Result := MakeColor(128, 0,   128); Exit; end;
  if SameText(t, 'pink')    then begin Result := MakeColor(255, 192, 203); Exit; end;
  if SameText(t, 'brown')   then begin Result := MakeColor(165, 42,  42);  Exit; end;
  if SameText(t, 'gray')    then begin Result := MakeColor(128, 128, 128); Exit; end;
  if SameText(t, 'grey')    then begin Result := MakeColor(128, 128, 128); Exit; end;
  if SameText(t, 'silver')  then begin Result := MakeColor(192, 192, 192); Exit; end;
  if SameText(t, 'maroon')  then begin Result := MakeColor(128, 0,   0);   Exit; end;
  if SameText(t, 'navy')    then begin Result := MakeColor(0,   0,   128); Exit; end;
  if SameText(t, 'teal')    then begin Result := MakeColor(0,   128, 128); Exit; end;
  if SameText(t, 'olive')   then begin Result := MakeColor(128, 128, 0);   Exit; end;

  { Unknown colour — default to opaque black }
  Result := MakeColor(0, 0, 0);
end;

{ Returns True if s is a url(#id) reference and sets AId to the bare id. }
function IsUrlRef(const s: string; out AId: string): Boolean;
var
  t: string;
begin
  t := Trim(s);
  Result := SameText(Copy(t, 1, 4), 'url(') and (t[Length(t)] = ')');
  if Result then
  begin
    AId := Trim(Copy(t, 5, Length(t) - 5));
    if (AId <> '') and (AId[1] = '#') then
      AId := Copy(AId, 2, MaxInt);
  end
  else
    AId := '';
end;


{ =========================================================
  Inline CSS style parser
  ========================================================= }

{ Parse 'fill: red; fill-opacity: 0.5' into a TStringList with name=value
  pairs (keys lowercased). Caller must free the returned list. }
function ParseInlineStyle(const s: string): TStringList;
var
  parts: TStringList;
  i, colonPos: Integer;
  key, val: string;
begin
  Result := TStringList.Create;
  parts := TStringList.Create;
  try
    parts.Delimiter := ';';
    parts.StrictDelimiter := True;
    parts.DelimitedText := s;
    for i := 0 to parts.Count - 1 do
    begin
      colonPos := Pos(':', parts[i]);
      if colonPos > 0 then
      begin
        key := LowerCase(Trim(Copy(parts[i], 1, colonPos - 1)));
        val := Trim(Copy(parts[i], colonPos + 1, MaxInt));
        if key <> '' then
          Result.Values[key] := val;
      end;
    end;
  finally
    parts.Free;
  end;
end;


{ =========================================================
  SVG affine matrix type and operations
  ========================================================= }

type
  { 2D affine transform.
    x' = A*x + C*y + TX
    y' = B*x + D*y + TY }
  TSvgMatrix = record
    A, B, C, D: Double;
    TX, TY: Double;
  end;

function SvgIdentityMatrix: TSvgMatrix;
begin
  Result.A  := 1.0; Result.B  := 0.0;
  Result.C  := 0.0; Result.D  := 1.0;
  Result.TX := 0.0; Result.TY := 0.0;
end;

{ Compose P after Q: result(p) = P(Q(p)).
  Used to accumulate transforms left-to-right as in SVG transform attribute. }
function SvgMatrixMul(const P, Q: TSvgMatrix): TSvgMatrix;
begin
  Result.A  := P.A * Q.A  + P.C * Q.B;
  Result.B  := P.B * Q.A  + P.D * Q.B;
  Result.C  := P.A * Q.C  + P.C * Q.D;
  Result.D  := P.B * Q.C  + P.D * Q.D;
  Result.TX := P.A * Q.TX + P.C * Q.TY + P.TX;
  Result.TY := P.B * Q.TX + P.D * Q.TY + P.TY;
end;

{ Build the root viewBox matrix: uniform scale + offset so SVG maps to 0..64. }
function MakeViewBoxMatrix(vbX, vbY, vbW, vbH: Double): TSvgMatrix;
var
  s: Double;
begin
  s := 64.0 / Max(vbW, vbH);
  Result.A  := s;   Result.B  := 0.0;
  Result.C  := 0.0; Result.D  := s;
  Result.TX := -vbX * s;
  Result.TY := -vbY * s;
end;

{ Apply matrix to x coordinate. }
function MX(const M: TSvgMatrix; x, y: Double): Single;
begin
  Result := Single(M.A * x + M.C * y + M.TX);
end;

{ Apply matrix to y coordinate. }
function MY(const M: TSvgMatrix; x, y: Double): Single;
begin
  Result := Single(M.B * x + M.D * y + M.TY);
end;


{ =========================================================
  SVG transform attribute parser
  ========================================================= }

{ Parse a single number from a string starting at position APos.
  Advances APos past the number and any following whitespace/commas. }
function ParseTransformNum(const s: string; var APos: Integer): Double;
var
  start: Integer;
  numStr: string;
begin
  { skip leading ws/comma }
  while (APos <= Length(s)) and (s[APos] in [' ', #9, ',']) do
    Inc(APos);
  start := APos;
  if (APos <= Length(s)) and (s[APos] in ['-', '+']) then
    Inc(APos);
  while (APos <= Length(s)) and (s[APos] in ['0'..'9']) do
    Inc(APos);
  if (APos <= Length(s)) and (s[APos] = '.') then
  begin
    Inc(APos);
    while (APos <= Length(s)) and (s[APos] in ['0'..'9']) do
      Inc(APos);
  end;
  if (APos <= Length(s)) and (s[APos] in ['E', 'e']) then
  begin
    Inc(APos);
    if (APos <= Length(s)) and (s[APos] in ['-', '+']) then
      Inc(APos);
    while (APos <= Length(s)) and (s[APos] in ['0'..'9']) do
      Inc(APos);
  end;
  numStr := Copy(s, start, APos - start);
  Result := SvgStrToFloatDef(numStr, 0.0);
  { skip trailing ws/comma }
  while (APos <= Length(s)) and (s[APos] in [' ', #9, ',']) do
    Inc(APos);
end;

{ Parse the SVG transform attribute string and return the composed matrix.
  Multiple functions (e.g. "translate(10,20) rotate(45)") are composed
  left-to-right (each new transform post-multiplied onto the result). }
function ParseSvgTransform(const s: string): TSvgMatrix;
var
  i, nameStart, nameEnd, parenEnd: Integer;
  funcName: string;
  args: array[0..5] of Double;
  nArgs, j: Integer;
  M, T: TSvgMatrix;
  tx, ty, sx, sy, ang, cosA, sinA, cx, cy: Double;
begin
  Result := SvgIdentityMatrix;
  i := 1;
  while i <= Length(s) do
  begin
    { skip whitespace }
    while (i <= Length(s)) and (s[i] in [' ', #9, #10, #13, ',']) do
      Inc(i);
    if i > Length(s) then Break;

    { read function name }
    nameStart := i;
    while (i <= Length(s)) and (s[i] in ['A'..'Z', 'a'..'z']) do
      Inc(i);
    nameEnd := i - 1;
    if nameEnd < nameStart then begin Inc(i); Continue; end;
    funcName := LowerCase(Copy(s, nameStart, nameEnd - nameStart + 1));

    { find opening paren }
    while (i <= Length(s)) and (s[i] <> '(') do Inc(i);
    if i > Length(s) then Break;
    Inc(i); { skip '(' }

    { find closing paren }
    parenEnd := i;
    while (parenEnd <= Length(s)) and (s[parenEnd] <> ')') do
      Inc(parenEnd);

    { parse up to 6 numeric arguments }
    nArgs := 0;
    for j := 0 to 5 do args[j] := 0.0;
    while (i < parenEnd) and (nArgs < 6) do
    begin
      args[nArgs] := ParseTransformNum(s, i);
      Inc(nArgs);
    end;
    i := parenEnd + 1; { skip ')' }

    { Build the individual transform matrix T }
    T := SvgIdentityMatrix;

    if funcName = 'translate' then
    begin
      tx := args[0];
      if nArgs >= 2 then ty := args[1] else ty := 0.0;
      T.TX := tx; T.TY := ty;
    end
    else if funcName = 'scale' then
    begin
      sx := args[0];
      if nArgs >= 2 then sy := args[1] else sy := sx;
      T.A := sx; T.D := sy;
    end
    else if funcName = 'rotate' then
    begin
      ang  := args[0] * (Pi / 180.0);
      cosA := Cos(ang); sinA := Sin(ang);
      if nArgs >= 3 then
      begin
        cx := args[1]; cy := args[2];
        { rotate around (cx,cy): translate to origin, rotate, translate back }
        T.A := cosA;  T.B := sinA;
        T.C := -sinA; T.D := cosA;
        T.TX := cx - cosA * cx + sinA * cy;
        T.TY := cy - sinA * cx - cosA * cy;
      end
      else
      begin
        T.A := cosA;  T.B := sinA;
        T.C := -sinA; T.D := cosA;
      end;
    end
    else if funcName = 'skewx' then
    begin
      T.C := Tan(args[0] * (Pi / 180.0));
    end
    else if funcName = 'skewy' then
    begin
      T.B := Tan(args[0] * (Pi / 180.0));
    end
    else if funcName = 'matrix' then
    begin
      { SVG matrix(a,b,c,d,e,f): x' = a*x + c*y + e, y' = b*x + d*y + f }
      T.A := args[0]; T.B := args[1];
      T.C := args[2]; T.D := args[3];
      T.TX := args[4]; T.TY := args[5];
    end;

    { Post-multiply: Result = Result ∘ T  (T applied first, then Result) }
    M := SvgMatrixMul(Result, T);
    Result := M;
  end;
end;


{ =========================================================
  viewBox parser
  ========================================================= }

function ParseViewBox(const s: string;
  out vbX, vbY, vbW, vbH: Double): Boolean;
var
  tokens: TStringList;
  cleaned: string;
  i: Integer;
begin
  Result := False;
  vbX := 0; vbY := 0; vbW := 64; vbH := 64;

  cleaned := Trim(s);
  if cleaned = '' then Exit;

  for i := 1 to Length(cleaned) do
    if cleaned[i] = ',' then cleaned[i] := ' ';

  tokens := TStringList.Create;
  try
    tokens.Delimiter := ' ';
    tokens.StrictDelimiter := False;
    tokens.DelimitedText := cleaned;

    for i := tokens.Count - 1 downto 0 do
      if Trim(tokens[i]) = '' then tokens.Delete(i);

    if tokens.Count < 4 then Exit;

    vbX := SvgStrToFloatDef(tokens[0], 0);
    vbY := SvgStrToFloatDef(tokens[1], 0);
    vbW := SvgStrToFloatDef(tokens[2], 64);
    vbH := SvgStrToFloatDef(tokens[3], 64);
    Result := (vbW > 0) and (vbH > 0);
  finally
    tokens.Free;
  end;
end;


{ =========================================================
  SVG path 'd' attribute parser
  ========================================================= }

type
  TSvgPathParser = class
  private
    FStr: string;
    FPos: Integer;
    FMatrix: TSvgMatrix;

    FCurX, FCurY: Double;
    FStartX, FStartY: Double;
    FLastCPX, FLastCPY: Double;
    FLastQCPX, FLastQCPY: Double;
    FLastCmd: Char;

    FCurPoints: array of THvifPoint;
    FResult: TPathList;

    procedure SkipWS;
    function  PeekChar: Char;
    function  IsNumericStart: Boolean;
    function  ReadNumber: Double;

    function  TX(x, y: Double): Single;
    function  TY(x, y: Double): Single;

    procedure FinishSubPath(AClosed: Boolean);
    procedure EnsureStartPoint;

    procedure AddLineTo(x, y: Double);
    procedure AddCubicTo(c1x, c1y, c2x, c2y, x, y: Double);

    procedure DoMoveTo(rel: Boolean);
    procedure DoLineTo(rel: Boolean);
    procedure DoHLine(rel: Boolean);
    procedure DoVLine(rel: Boolean);
    procedure DoCubic(rel: Boolean);
    procedure DoSmoothCubic(rel: Boolean);
    procedure DoQuad(rel: Boolean);
    procedure DoSmoothQuad(rel: Boolean);
    procedure DoClose;
    procedure DoArc(rel: Boolean);

  public
    constructor Create(const AStr: string; const AMatrix: TSvgMatrix);
    function  Parse: TPathList;
  end;


constructor TSvgPathParser.Create(const AStr: string; const AMatrix: TSvgMatrix);
begin
  FStr    := AStr;
  FPos    := 1;
  FMatrix := AMatrix;
  FCurX   := 0; FCurY   := 0;
  FStartX := 0; FStartY := 0;
  FLastCPX  := 0; FLastCPY  := 0;
  FLastQCPX := 0; FLastQCPY := 0;
  FLastCmd  := #0;
  SetLength(FCurPoints, 0);
  SetLength(FResult, 0);
end;


procedure TSvgPathParser.SkipWS;
begin
  while (FPos <= Length(FStr)) and
        (FStr[FPos] in [' ', #9, #10, #13, ',']) do
    Inc(FPos);
end;

function TSvgPathParser.PeekChar: Char;
begin
  if FPos <= Length(FStr) then
    Result := FStr[FPos]
  else
    Result := #0;
end;

function TSvgPathParser.IsNumericStart: Boolean;
var
  c: Char;
begin
  c := PeekChar;
  Result := c in ['0'..'9', '.', '-', '+'];
end;

function TSvgPathParser.ReadNumber: Double;
var
  start: Integer;
  numStr: string;
  hasDecimal: Boolean;
begin
  SkipWS;
  start := FPos;
  hasDecimal := False;

  if (FPos <= Length(FStr)) and (FStr[FPos] in ['-', '+']) then
    Inc(FPos);

  while (FPos <= Length(FStr)) and (FStr[FPos] in ['0'..'9']) do
    Inc(FPos);

  if (FPos <= Length(FStr)) and (FStr[FPos] = '.') and not hasDecimal then
  begin
    hasDecimal := True;
    Inc(FPos);
    while (FPos <= Length(FStr)) and (FStr[FPos] in ['0'..'9']) do
      Inc(FPos);
  end;

  if (FPos <= Length(FStr)) and (FStr[FPos] in ['E', 'e']) then
  begin
    Inc(FPos);
    if (FPos <= Length(FStr)) and (FStr[FPos] in ['-', '+']) then
      Inc(FPos);
    while (FPos <= Length(FStr)) and (FStr[FPos] in ['0'..'9']) do
      Inc(FPos);
  end;

  numStr := Copy(FStr, start, FPos - start);
  if numStr = '' then
    raise EsvgConvertError.CreateFmt(
      'SVG path: expected number at position %d', [FPos]);

  Result := SvgStrToFloat(numStr);
end;

function TSvgPathParser.TX(x, y: Double): Single;
begin
  Result := MX(FMatrix, x, y);
end;

function TSvgPathParser.TY(x, y: Double): Single;
begin
  Result := MY(FMatrix, x, y);
end;

procedure TSvgPathParser.FinishSubPath(AClosed: Boolean);
var
  path: THvifPath;
  i: Integer;
begin
  if Length(FCurPoints) >= 2 then
  begin
    path.Closed := AClosed;
    SetLength(path.Points, Length(FCurPoints));
    for i := 0 to High(FCurPoints) do
      path.Points[i] := FCurPoints[i];
    SetLength(FResult, Length(FResult) + 1);
    FResult[High(FResult)] := path;
  end;
  SetLength(FCurPoints, 0);
end;

procedure TSvgPathParser.EnsureStartPoint;
var
  pt: THvifPoint;
begin
  if Length(FCurPoints) = 0 then
  begin
    SetLength(FCurPoints, 1);
    pt.X   := TX(FCurX, FCurY); pt.Y   := TY(FCurX, FCurY);
    pt.InX := pt.X;             pt.InY := pt.Y;
    pt.OutX:= pt.X;             pt.OutY:= pt.Y;
    FCurPoints[0] := pt;
  end;
end;

procedure TSvgPathParser.AddLineTo(x, y: Double);
var
  pt: THvifPoint;
begin
  EnsureStartPoint;
  SetLength(FCurPoints, Length(FCurPoints) + 1);
  pt.X   := TX(x, y); pt.Y   := TY(x, y);
  pt.InX := pt.X;     pt.InY := pt.Y;
  pt.OutX:= pt.X;     pt.OutY:= pt.Y;
  FCurPoints[High(FCurPoints)] := pt;
  FCurX := x; FCurY := y;
end;

procedure TSvgPathParser.AddCubicTo(c1x, c1y, c2x, c2y, x, y: Double);
var
  pt: THvifPoint;
begin
  EnsureStartPoint;
  FCurPoints[High(FCurPoints)].OutX := TX(c1x, c1y);
  FCurPoints[High(FCurPoints)].OutY := TY(c1x, c1y);
  SetLength(FCurPoints, Length(FCurPoints) + 1);
  pt.X   := TX(x,   y);   pt.Y   := TY(x,   y);
  pt.InX := TX(c2x, c2y); pt.InY := TY(c2x, c2y);
  pt.OutX:= pt.X;          pt.OutY:= pt.Y;
  FCurPoints[High(FCurPoints)] := pt;
  FCurX := x; FCurY := y;
  FLastCPX := c2x; FLastCPY := c2y;
end;


procedure TSvgPathParser.DoMoveTo(rel: Boolean);
var
  x, y: Double;
begin
  FinishSubPath(False);
  x := ReadNumber; SkipWS; y := ReadNumber;
  if rel then begin x := FCurX + x; y := FCurY + y; end;
  FCurX := x; FCurY := y;
  FStartX := x; FStartY := y;
  FLastCPX := x; FLastCPY := y;
  FLastQCPX:= x; FLastQCPY:= y;
  FLastCmd := 'M';
  SkipWS;
  while IsNumericStart do
  begin
    x := ReadNumber; SkipWS; y := ReadNumber;
    if rel then begin x := FCurX + x; y := FCurY + y; end;
    AddLineTo(x, y);
    SkipWS;
  end;
end;

procedure TSvgPathParser.DoLineTo(rel: Boolean);
var
  x, y: Double;
begin
  repeat
    x := ReadNumber; SkipWS; y := ReadNumber;
    if rel then begin x := FCurX + x; y := FCurY + y; end;
    AddLineTo(x, y);
    SkipWS;
  until not IsNumericStart;
  FLastCmd := 'L';
end;

procedure TSvgPathParser.DoHLine(rel: Boolean);
var
  x: Double;
begin
  repeat
    x := ReadNumber;
    if rel then x := FCurX + x;
    AddLineTo(x, FCurY);
    SkipWS;
  until not IsNumericStart;
  FLastCmd := 'H';
end;

procedure TSvgPathParser.DoVLine(rel: Boolean);
var
  y: Double;
begin
  repeat
    y := ReadNumber;
    if rel then y := FCurY + y;
    AddLineTo(FCurX, y);
    SkipWS;
  until not IsNumericStart;
  FLastCmd := 'V';
end;

procedure TSvgPathParser.DoCubic(rel: Boolean);
var
  c1x, c1y, c2x, c2y, x, y, rx, ry: Double;
begin
  repeat
    rx := FCurX; ry := FCurY;
    c1x := ReadNumber; SkipWS; c1y := ReadNumber; SkipWS;
    c2x := ReadNumber; SkipWS; c2y := ReadNumber; SkipWS;
    x   := ReadNumber; SkipWS; y   := ReadNumber;
    if rel then
    begin
      c1x := rx + c1x; c1y := ry + c1y;
      c2x := rx + c2x; c2y := ry + c2y;
      x   := rx + x;   y   := ry + y;
    end;
    AddCubicTo(c1x, c1y, c2x, c2y, x, y);
    SkipWS;
  until not IsNumericStart;
  FLastCmd := 'C';
end;

procedure TSvgPathParser.DoSmoothCubic(rel: Boolean);
var
  c1x, c1y, c2x, c2y, x, y, rx, ry: Double;
begin
  repeat
    rx := FCurX; ry := FCurY;
    if FLastCmd in ['C', 'S'] then
    begin
      c1x := 2*rx - FLastCPX;
      c1y := 2*ry - FLastCPY;
    end
    else
    begin
      c1x := rx;
      c1y := ry;
    end;
    c2x := ReadNumber; SkipWS; c2y := ReadNumber; SkipWS;
    x   := ReadNumber; SkipWS; y   := ReadNumber;
    if rel then
    begin
      c2x := rx + c2x; c2y := ry + c2y;
      x   := rx + x;   y   := ry + y;
    end;
    AddCubicTo(c1x, c1y, c2x, c2y, x, y);
    FLastCmd := 'S';
    SkipWS;
  until not IsNumericStart;
end;

procedure TSvgPathParser.DoQuad(rel: Boolean);
var
  qx1, qy1, qx, qy, rx, ry: Double;
  c1x, c1y, c2x, c2y: Double;
begin
  repeat
    rx := FCurX; ry := FCurY;
    qx1 := ReadNumber; SkipWS; qy1 := ReadNumber; SkipWS;
    qx  := ReadNumber; SkipWS; qy  := ReadNumber;
    if rel then
    begin
      qx1 := rx + qx1; qy1 := ry + qy1;
      qx  := rx + qx;  qy  := ry + qy;
    end;
    c1x := rx  + 2.0/3.0 * (qx1 - rx);
    c1y := ry  + 2.0/3.0 * (qy1 - ry);
    c2x := qx  + 2.0/3.0 * (qx1 - qx);
    c2y := qy  + 2.0/3.0 * (qy1 - qy);
    AddCubicTo(c1x, c1y, c2x, c2y, qx, qy);
    FLastQCPX := qx1; FLastQCPY := qy1;
    FLastCmd := 'Q';
    SkipWS;
  until not IsNumericStart;
end;

procedure TSvgPathParser.DoSmoothQuad(rel: Boolean);
var
  qx1, qy1, qx, qy, rx, ry: Double;
  c1x, c1y, c2x, c2y: Double;
begin
  repeat
    rx := FCurX; ry := FCurY;
    if FLastCmd in ['Q', 'T'] then
    begin
      qx1 := 2*rx - FLastQCPX;
      qy1 := 2*ry - FLastQCPY;
    end
    else
    begin
      qx1 := rx;
      qy1 := ry;
    end;
    qx := ReadNumber; SkipWS; qy := ReadNumber;
    if rel then begin qx := rx + qx; qy := ry + qy; end;
    c1x := rx  + 2.0/3.0 * (qx1 - rx);
    c1y := ry  + 2.0/3.0 * (qy1 - ry);
    c2x := qx  + 2.0/3.0 * (qx1 - qx);
    c2y := qy  + 2.0/3.0 * (qy1 - qy);
    AddCubicTo(c1x, c1y, c2x, c2y, qx, qy);
    FLastQCPX := qx1; FLastQCPY := qy1;
    FLastCmd := 'T';
    SkipWS;
  until not IsNumericStart;
end;

procedure TSvgPathParser.DoClose;
begin
  FinishSubPath(True);
  FCurX := FStartX; FCurY := FStartY;
  FLastCmd := 'Z';
end;

{ Convert SVG arc to one or more cubic bezier segments.
  Implements SVG Appendix F.6.5 endpoint-to-centre parameterisation. }
procedure TSvgPathParser.DoArc(rel: Boolean);
var
  arx, ary, phi, x2, y2: Double;
  fA, fS: Integer;
  x1, y1: Double;
  dx, dy: Double;
  cphi, sphi: Double;
  x1p, y1p: Double;
  lam, sqLam: Double;
  num, den, sq: Double;
  sign: Double;
  cxp, cyp: Double;
  cx, cy: Double;
  ux, uy, vx, vy: Double;
  dotUV, magU, magV, angSign: Double;
  theta1, dtheta: Double;
  nSegs, k: Integer;
  segDth, alpha: Double;
  th1, th2: Double;
  p1ux, p1uy, p2ux, p2uy: Double;
  c1ux, c1uy, c2ux, c2uy: Double;
  exC1x, eyC1y, exC2x, eyC2y, exP2x, eyP2y: Double;
  halfFmt: TFormatSettings;
begin
  halfFmt := GSvgFmt; { unused but keep compiler happy }
  while IsNumericStart do
  begin
    arx  := Abs(ReadNumber); SkipWS;
    ary  := Abs(ReadNumber); SkipWS;
    phi  := ReadNumber * (Pi / 180.0); SkipWS;
    { large-arc-flag and sweep-flag are single digits (0 or 1) }
    fA   := Round(ReadNumber); SkipWS;
    fS   := Round(ReadNumber); SkipWS;
    x2   := ReadNumber; SkipWS;
    y2   := ReadNumber; SkipWS;

    if rel then
    begin
      x2 := FCurX + x2;
      y2 := FCurY + y2;
    end;

    x1 := FCurX; y1 := FCurY;

    { Degenerate: start = end → skip }
    if (Abs(x1 - x2) < 1e-10) and (Abs(y1 - y2) < 1e-10) then Continue;
    { Degenerate: zero radii → straight line }
    if (arx < 1e-10) or (ary < 1e-10) then
    begin
      AddLineTo(x2, y2);
      Continue;
    end;

    cphi := Cos(phi); sphi := Sin(phi);
    dx := (x1 - x2) / 2.0; dy := (y1 - y2) / 2.0;
    x1p :=  cphi * dx + sphi * dy;
    y1p := -sphi * dx + cphi * dy;

    { Fix radii if they are too small }
    lam := Sqr(x1p / arx) + Sqr(y1p / ary);
    if lam > 1.0 then
    begin
      sqLam := Sqrt(lam);
      arx := sqLam * arx;
      ary := sqLam * ary;
    end;

    { Compute (cx', cy') }
    num := Sqr(arx) * Sqr(ary) - Sqr(arx) * Sqr(y1p) - Sqr(ary) * Sqr(x1p);
    den := Sqr(arx) * Sqr(y1p) + Sqr(ary) * Sqr(x1p);
    if den < 1e-20 then
      sq := 0.0
    else
      sq := Sqrt(Max(0.0, num / den));
    if fA = fS then sign := -1.0 else sign := 1.0;
    cxp :=  sign * sq * (arx * y1p / ary);
    cyp :=  sign * sq * (-ary * x1p / arx);

    { Compute centre in original coordinate space }
    cx := cphi * cxp - sphi * cyp + (x1 + x2) / 2.0;
    cy := sphi * cxp + cphi * cyp + (y1 + y2) / 2.0;

    { Compute theta1 and dtheta }
    ux := (x1p - cxp) / arx; uy := (y1p - cyp) / ary;
    vx := (-x1p - cxp) / arx; vy := (-y1p - cyp) / ary;

    magU := Sqrt(ux * ux + uy * uy);
    if magU < 1e-20 then magU := 1e-20;
    dotUV := EnsureRange(ux / magU, -1.0, 1.0);
    angSign := uy; { sign of (1*uy - 0*ux) }
    if angSign < 0 then theta1 := -ArcCos(dotUV)
    else theta1 := ArcCos(dotUV);

    magU := Sqrt(ux * ux + uy * uy);
    magV := Sqrt(vx * vx + vy * vy);
    if (magU < 1e-20) or (magV < 1e-20) then
      dtheta := 0.0
    else
    begin
      dotUV := EnsureRange((ux * vx + uy * vy) / (magU * magV), -1.0, 1.0);
      angSign := ux * vy - uy * vx;
      if angSign < 0 then dtheta := -ArcCos(dotUV)
      else dtheta := ArcCos(dotUV);
    end;

    { Adjust dtheta for sweep direction }
    if (fS = 0) and (dtheta > 0) then dtheta := dtheta - 2.0 * Pi;
    if (fS = 1) and (dtheta < 0) then dtheta := dtheta + 2.0 * Pi;

    { Generate cubic bezier segments (at most one per 90 degrees) }
    nSegs := Max(1, Ceil(Abs(dtheta) / (Pi / 2.0)));
    segDth := dtheta / nSegs;
    alpha  := 4.0 / 3.0 * Tan(segDth / 4.0);

    for k := 0 to nSegs - 1 do
    begin
      th1 := theta1 + k * segDth;
      th2 := theta1 + (k + 1) * segDth;

      { Unit circle anchor and control points for this segment }
      p1ux := Cos(th1); p1uy := Sin(th1);
      p2ux := Cos(th2); p2uy := Sin(th2);

      c1ux := p1ux - alpha * p1uy;
      c1uy := p1uy + alpha * p1ux;
      c2ux := p2ux + alpha * p2uy;
      c2uy := p2uy - alpha * p2ux;

      { Map unit circle → ellipse in SVG coordinate space:
        Ex(ux,uy) = cx + cphi*arx*ux - sphi*ary*uy
        Ey(ux,uy) = cy + sphi*arx*ux + cphi*ary*uy }
      exC1x := cx + cphi * arx * c1ux - sphi * ary * c1uy;
      eyC1y := cy + sphi * arx * c1ux + cphi * ary * c1uy;
      exC2x := cx + cphi * arx * c2ux - sphi * ary * c2uy;
      eyC2y := cy + sphi * arx * c2ux + cphi * ary * c2uy;
      exP2x := cx + cphi * arx * p2ux - sphi * ary * p2uy;
      eyP2y := cy + sphi * arx * p2ux + cphi * ary * p2uy;

      AddCubicTo(exC1x, eyC1y, exC2x, eyC2y, exP2x, eyP2y);
    end;

    FLastCmd := 'A';
  end;
end;


function TSvgPathParser.Parse: TPathList;
var
  cmd: Char;
begin
  FPos := 1;
  while FPos <= Length(FStr) do
  begin
    SkipWS;
    if FPos > Length(FStr) then Break;
    cmd := FStr[FPos];
    if cmd in ['M', 'm', 'L', 'l', 'H', 'h', 'V', 'v',
               'C', 'c', 'S', 's', 'Q', 'q', 'T', 't',
               'A', 'a', 'Z', 'z'] then
    begin
      Inc(FPos);
      SkipWS;
      case cmd of
        'M': begin FLastCmd := 'M'; DoMoveTo(False); end;
        'm': begin FLastCmd := 'M'; DoMoveTo(True);  end;
        'L': DoLineTo(False);
        'l': DoLineTo(True);
        'H': DoHLine(False);
        'h': DoHLine(True);
        'V': DoVLine(False);
        'v': DoVLine(True);
        'C': DoCubic(False);
        'c': DoCubic(True);
        'S': DoSmoothCubic(False);
        's': DoSmoothCubic(True);
        'Q': DoQuad(False);
        'q': DoQuad(True);
        'T': DoSmoothQuad(False);
        't': DoSmoothQuad(True);
        'A': DoArc(False);
        'a': DoArc(True);
        'Z', 'z': DoClose;
      end;
    end
    else
      Inc(FPos);
  end;
  FinishSubPath(False);
  Result := FResult;
end;


{ =========================================================
  Shape element path builders (all accept TSvgMatrix)
  ========================================================= }

const
  { Bezier approximation constant for a quarter-circle arc }
  KAPPA = 0.5523;

{ Parse whitespace/comma-separated coordinate pairs from a SVG points attribute
  (used by <polygon> and <polyline>). Returns a flat [x0,y0,x1,y1,...] array. }
function ParseSvgPoints(const s: string): TDoubleArray;
var
  cleaned, numStr: string;
  i, start, count: Integer;
begin
  SetLength(Result, 0);
  cleaned := s;
  for i := 1 to Length(cleaned) do
    if cleaned[i] = ',' then cleaned[i] := ' ';

  count := 0;
  i := 1;
  while i <= Length(cleaned) do
  begin
    while (i <= Length(cleaned)) and (cleaned[i] in [' ', #9, #10, #13]) do
      Inc(i);
    if i > Length(cleaned) then Break;
    start := i;
    if (i <= Length(cleaned)) and (cleaned[i] in ['-', '+']) then Inc(i);
    while (i <= Length(cleaned)) and (cleaned[i] in ['0'..'9', '.', 'E', 'e']) do
      Inc(i);
    numStr := Copy(cleaned, start, i - start);
    if numStr <> '' then
    begin
      SetLength(Result, count + 1);
      Result[count] := SvgStrToFloatDef(numStr, 0);
      Inc(count);
    end;
  end;
end;

{ Build a simple straight-segment path from a flat coordinate array. }
function BuildPolygonPath(const pts: TDoubleArray; AClosed: Boolean;
  const AMatrix: TSvgMatrix): THvifPath;
var
  n, i: Integer;
  hx, hy: Single;
begin
  n := Length(pts) div 2;
  Result.Closed := AClosed;
  if n < 2 then
  begin
    SetLength(Result.Points, 0);
    Exit;
  end;
  SetLength(Result.Points, n);
  for i := 0 to n - 1 do
  begin
    hx := MX(AMatrix, pts[i*2], pts[i*2+1]);
    hy := MY(AMatrix, pts[i*2], pts[i*2+1]);
    Result.Points[i].X    := hx;
    Result.Points[i].Y    := hy;
    Result.Points[i].InX  := hx;
    Result.Points[i].InY  := hy;
    Result.Points[i].OutX := hx;
    Result.Points[i].OutY := hy;
  end;
end;

{ Build a closed ellipse/circle path using four cubic Bezier quarter-arcs.
  The path goes clockwise: N -> E -> S -> W. }
function BuildEllipsePath(cx, cy, rx, ry: Double;
  const AMatrix: TSvgMatrix): THvifPath;
var
  hcx, hcy, hrx, hry, k: Single;
  { For ellipses, scale only by the matrix scale component (not translation).
    Since ellipses are centred shapes, we approximate by extracting scale
    from the matrix diagonal. For non-uniform or rotated matrices this is an
    approximation, but SVG icons rarely use rotated ellipses with group transforms. }
  scaleX, scaleY: Double;
begin
  { Apply centre through full matrix }
  hcx := MX(AMatrix, cx, cy);
  hcy := MY(AMatrix, cx, cy);
  { Scale radii by the matrix scale components }
  scaleX := Sqrt(AMatrix.A * AMatrix.A + AMatrix.B * AMatrix.B);
  scaleY := Sqrt(AMatrix.C * AMatrix.C + AMatrix.D * AMatrix.D);
  hrx := Single(rx * scaleX);
  hry := Single(ry * scaleY);
  k   := KAPPA;

  Result.Closed := True;
  SetLength(Result.Points, 4);

  { N: top of ellipse at (cx, cy-ry) }
  Result.Points[0].X    := hcx;
  Result.Points[0].Y    := hcy - hry;
  Result.Points[0].InX  := hcx - k * hrx;
  Result.Points[0].InY  := hcy - hry;
  Result.Points[0].OutX := hcx + k * hrx;
  Result.Points[0].OutY := hcy - hry;

  { E: right of ellipse at (cx+rx, cy) }
  Result.Points[1].X    := hcx + hrx;
  Result.Points[1].Y    := hcy;
  Result.Points[1].InX  := hcx + hrx;
  Result.Points[1].InY  := hcy - k * hry;
  Result.Points[1].OutX := hcx + hrx;
  Result.Points[1].OutY := hcy + k * hry;

  { S: bottom of ellipse at (cx, cy+ry) }
  Result.Points[2].X    := hcx;
  Result.Points[2].Y    := hcy + hry;
  Result.Points[2].InX  := hcx + k * hrx;
  Result.Points[2].InY  := hcy + hry;
  Result.Points[2].OutX := hcx - k * hrx;
  Result.Points[2].OutY := hcy + hry;

  { W: left of ellipse at (cx-rx, cy) }
  Result.Points[3].X    := hcx - hrx;
  Result.Points[3].Y    := hcy;
  Result.Points[3].InX  := hcx - hrx;
  Result.Points[3].InY  := hcy + k * hry;
  Result.Points[3].OutX := hcx - hrx;
  Result.Points[3].OutY := hcy - k * hry;
end;

{ Build a closed rectangle path with optional rounded corners.
  rx=ry=0 produces a simple 4-point rect; otherwise 8 points with arcs. }
function BuildRectPath(x, y, w, h, rx, ry: Double;
  const AMatrix: TSvgMatrix): THvifPath;
var
  lx1, ly1, lx2, ly2: Single;
  lrx, lry, lk: Single;
  scaleX, scaleY: Double;
begin
  lx1 := MX(AMatrix, x,     y);
  ly1 := MY(AMatrix, x,     y);
  lx2 := MX(AMatrix, x + w, y + h);
  ly2 := MY(AMatrix, x + w, y + h);
  scaleX := Sqrt(AMatrix.A * AMatrix.A + AMatrix.B * AMatrix.B);
  scaleY := Sqrt(AMatrix.C * AMatrix.C + AMatrix.D * AMatrix.D);
  lrx := Single(rx * scaleX);
  lry := Single(ry * scaleY);
  lk  := KAPPA;

  Result.Closed := True;

  if (lrx < 0.001) or (lry < 0.001) then
  begin
    { Simple rectangle: TL, TR, BR, BL (all straight) }
    SetLength(Result.Points, 4);
    Result.Points[0].X := lx1; Result.Points[0].Y := ly1;
    Result.Points[0].InX := lx1; Result.Points[0].InY := ly1;
    Result.Points[0].OutX := lx1; Result.Points[0].OutY := ly1;

    Result.Points[1].X := lx2; Result.Points[1].Y := ly1;
    Result.Points[1].InX := lx2; Result.Points[1].InY := ly1;
    Result.Points[1].OutX := lx2; Result.Points[1].OutY := ly1;

    Result.Points[2].X := lx2; Result.Points[2].Y := ly2;
    Result.Points[2].InX := lx2; Result.Points[2].InY := ly2;
    Result.Points[2].OutX := lx2; Result.Points[2].OutY := ly2;

    Result.Points[3].X := lx1; Result.Points[3].Y := ly2;
    Result.Points[3].InX := lx1; Result.Points[3].InY := ly2;
    Result.Points[3].OutX := lx1; Result.Points[3].OutY := ly2;
  end
  else
  begin
    { Rounded rectangle: 8 points (clockwise from TL arc end) }
    SetLength(Result.Points, 8);

    { P0: TL arc end }
    Result.Points[0].X    := lx1 + lrx;
    Result.Points[0].Y    := ly1;
    Result.Points[0].InX  := lx1 + lrx - lk * lrx;
    Result.Points[0].InY  := ly1;
    Result.Points[0].OutX := lx1 + lrx;
    Result.Points[0].OutY := ly1;

    { P1: TR arc start }
    Result.Points[1].X    := lx2 - lrx;
    Result.Points[1].Y    := ly1;
    Result.Points[1].InX  := lx2 - lrx;
    Result.Points[1].InY  := ly1;
    Result.Points[1].OutX := lx2 - lrx + lk * lrx;
    Result.Points[1].OutY := ly1;

    { P2: TR arc end }
    Result.Points[2].X    := lx2;
    Result.Points[2].Y    := ly1 + lry;
    Result.Points[2].InX  := lx2;
    Result.Points[2].InY  := ly1 + lry - lk * lry;
    Result.Points[2].OutX := lx2;
    Result.Points[2].OutY := ly1 + lry;

    { P3: BR arc start }
    Result.Points[3].X    := lx2;
    Result.Points[3].Y    := ly2 - lry;
    Result.Points[3].InX  := lx2;
    Result.Points[3].InY  := ly2 - lry;
    Result.Points[3].OutX := lx2;
    Result.Points[3].OutY := ly2 - lry + lk * lry;

    { P4: BR arc end }
    Result.Points[4].X    := lx2 - lrx;
    Result.Points[4].Y    := ly2;
    Result.Points[4].InX  := lx2 - lrx + lk * lrx;
    Result.Points[4].InY  := ly2;
    Result.Points[4].OutX := lx2 - lrx;
    Result.Points[4].OutY := ly2;

    { P5: BL arc start }
    Result.Points[5].X    := lx1 + lrx;
    Result.Points[5].Y    := ly2;
    Result.Points[5].InX  := lx1 + lrx;
    Result.Points[5].InY  := ly2;
    Result.Points[5].OutX := lx1 + lrx - lk * lrx;
    Result.Points[5].OutY := ly2;

    { P6: BL arc end }
    Result.Points[6].X    := lx1;
    Result.Points[6].Y    := ly2 - lry;
    Result.Points[6].InX  := lx1;
    Result.Points[6].InY  := ly2 - lry + lk * lry;
    Result.Points[6].OutX := lx1;
    Result.Points[6].OutY := ly2 - lry;

    { P7: TL arc start }
    Result.Points[7].X    := lx1;
    Result.Points[7].Y    := ly1 + lry;
    Result.Points[7].InX  := lx1;
    Result.Points[7].InY  := ly1 + lry;
    Result.Points[7].OutX := lx1;
    Result.Points[7].OutY := ly1 + lry - lk * lry;
  end;
end;


{ =========================================================
  Gradient definition parsing
  ========================================================= }

{ Parse gradient stop colour taking into account inline style overrides. }
function ParseStopColor(stopElem: TDOMElement): TSvgGradientStop;
var
  colorStr, opacStr, offsetStr: string;
  styleProps: TStringList;
  opac: Double;
begin
  colorStr  := stopElem.GetAttribute('stop-color');
  opacStr   := stopElem.GetAttribute('stop-opacity');
  offsetStr := Trim(stopElem.GetAttribute('offset'));

  { Inline style overrides presentation attributes }
  styleProps := ParseInlineStyle(stopElem.GetAttribute('style'));
  try
    if styleProps.Values['stop-color'] <> '' then
      colorStr := styleProps.Values['stop-color'];
    if styleProps.Values['stop-opacity'] <> '' then
      opacStr := styleProps.Values['stop-opacity'];
  finally
    styleProps.Free;
  end;

  if colorStr = '' then colorStr := 'black';
  Result.Color := ParseSvgColor(colorStr);

  opac := SvgStrToFloatDef(opacStr, 1.0);
  Result.Color.A := Byte(Round(Result.Color.A * EnsureRange(opac, 0.0, 1.0)));

  { Parse offset: may be a percentage }
  if (offsetStr <> '') and (offsetStr[Length(offsetStr)] = '%') then
    Result.Offset := SvgStrToFloatDef(
      Copy(offsetStr, 1, Length(offsetStr) - 1), 0) / 100.0
  else
    Result.Offset := SvgStrToFloatDef(offsetStr, 0.0);
  Result.Offset := EnsureRange(Result.Offset, 0.0, 1.0);
end;

{ Walk the DOM and collect all <linearGradient> and <radialGradient> elements
  into ADefs (keyed by id, Objects are TSvgGradientDef instances).
  Caller is responsible for freeing the TSvgGradientDef objects. }
procedure CollectGradientDefs(node: TDOMNode; ADefs: TStringList);
var
  elem: TDOMElement;
  def: TSvgGradientDef;
  child: TDOMNode;
  name, idStr, hrefVal: string;
  stopCount: Integer;
begin
  if node.NodeType <> ELEMENT_NODE then Exit;

  elem := TDOMElement(node);
  name := LowerCase(elem.NodeName);

  if SameText(name, 'lineargradient') or SameText(name, 'radialgradient') then
  begin
    idStr := elem.GetAttribute('id');
    if idStr = '' then Exit;

    def := TSvgGradientDef.Create;

    if SameText(name, 'lineargradient') then
    begin
      def.Kind := sgkLinear;
      def.X1   := SvgStrToFloatDef(elem.GetAttribute('x1'), 0.0);
      def.Y1   := SvgStrToFloatDef(elem.GetAttribute('y1'), 0.0);
      def.X2   := SvgStrToFloatDef(elem.GetAttribute('x2'), 1.0);
      def.Y2   := SvgStrToFloatDef(elem.GetAttribute('y2'), 0.0);
    end
    else
    begin
      def.Kind := sgkRadial;
      def.CX   := SvgStrToFloatDef(elem.GetAttribute('cx'), 0.5);
      def.CY   := SvgStrToFloatDef(elem.GetAttribute('cy'), 0.5);
      def.R    := SvgStrToFloatDef(elem.GetAttribute('r'),  0.5);
    end;

    def.GradientUnits := elem.GetAttribute('gradientUnits');
    if def.GradientUnits = '' then
      def.GradientUnits := 'objectBoundingBox';

    { xlink:href for stop inheritance }
    hrefVal := elem.GetAttribute('href');
    if hrefVal = '' then
      hrefVal := elem.GetAttribute('xlink:href');
    if (hrefVal <> '') and (hrefVal[1] = '#') then
      hrefVal := Copy(hrefVal, 2, MaxInt);
    def.HrefId := hrefVal;

    { Collect <stop> children }
    stopCount := 0;
    child := elem.FirstChild;
    while Assigned(child) do
    begin
      if (child.NodeType = ELEMENT_NODE) and
         SameText(child.NodeName, 'stop') then
      begin
        SetLength(def.Stops, stopCount + 1);
        def.Stops[stopCount] := ParseStopColor(TDOMElement(child));
        Inc(stopCount);
      end;
      child := child.NextSibling;
    end;

    ADefs.AddObject(idStr, def);
    Exit; { Gradient children are <stop> elements, not further gradients }
  end;

  { Recurse into all other element types }
  child := elem.FirstChild;
  while Assigned(child) do
  begin
    CollectGradientDefs(child, ADefs);
    child := child.NextSibling;
  end;
end;

{ Build a THvifStyle for a gradient definition.
  AViewBox{W,H}: viewBox dimensions used as bounding-box approximation for
                 objectBoundingBox gradient units.
  ADefs: the full gradient dictionary for resolving xlink:href stop inheritance. }
function BuildGradientStyle(def: TSvgGradientDef; ADefs: TStringList;
  const ARootMatrix: TSvgMatrix; AvbW, AvbH: Double): THvifStyle;
var
  stops: array of TSvgGradientStop;
  refIdx: Integer;
  refDef: TSvgGradientDef;
  hx1, hy1, hx2, hy2: Double;
  hcx, hcy, hr: Double;
  tx, ty, sx, shy: Double;
  i: Integer;
  scale: Double;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.StyleType      := hstGradient;
  Result.HasGradTransform := True;

  { Resolve gradient stops: use own stops, or inherit from href target }
  if Length(def.Stops) > 0 then
    stops := def.Stops
  else if def.HrefId <> '' then
  begin
    refIdx := ADefs.IndexOf(def.HrefId);
    if refIdx >= 0 then
    begin
      refDef := TSvgGradientDef(ADefs.Objects[refIdx]);
      stops  := refDef.Stops;
    end
    else
      stops := def.Stops;
  end
  else
    stops := def.Stops;

  SetLength(Result.Stops, Length(stops));
  for i := 0 to High(stops) do
  begin
    Result.Stops[i].Offset    := Single(stops[i].Offset);
    Result.Stops[i].Color.R   := stops[i].Color.R;
    Result.Stops[i].Color.G   := stops[i].Color.G;
    Result.Stops[i].Color.B   := stops[i].Color.B;
    Result.Stops[i].Color.A   := stops[i].Color.A;
  end;

  { Extract uniform scale factor from root matrix for gradient coords }
  scale := Sqrt(ARootMatrix.A * ARootMatrix.A + ARootMatrix.B * ARootMatrix.B);

  case def.Kind of
    sgkLinear:
    begin
      Result.GradientType := hgtLinear;

      if SameText(def.GradientUnits, 'userSpaceOnUse') then
      begin
        hx1 := MX(ARootMatrix, def.X1, def.Y1);
        hy1 := MY(ARootMatrix, def.X1, def.Y1);
        hx2 := MX(ARootMatrix, def.X2, def.Y2);
        hy2 := MY(ARootMatrix, def.X2, def.Y2);
      end
      else
      begin
        { objectBoundingBox: fractions 0..1 relative to bounding box.
          Approximate bounding box with the full viewBox. }
        hx1 := def.X1 * AvbW * scale;
        hy1 := def.Y1 * AvbH * scale;
        hx2 := def.X2 * AvbW * scale;
        hy2 := def.Y2 * AvbH * scale;
      end;

      { Map to HVIF gradient transform.
        Renderer decodes: p1 = (tx - sx*64, ty - shy*64)
                          p2 = (tx + sx*64, ty + shy*64) }
      tx  := (hx1 + hx2) / 2.0;
      ty  := (hy1 + hy2) / 2.0;
      sx  := (hx2 - hx1) / 128.0;
      shy := (hy2 - hy1) / 128.0;

      Result.GradTransform[0] := Single(sx);
      Result.GradTransform[1] := Single(shy);
      Result.GradTransform[2] := 0.0;
      Result.GradTransform[3] := 0.0;
      Result.GradTransform[4] := Single(tx);
      Result.GradTransform[5] := Single(ty);
    end;

    sgkRadial:
    begin
      Result.GradientType := hgtCircular;

      if SameText(def.GradientUnits, 'userSpaceOnUse') then
      begin
        hcx := MX(ARootMatrix, def.CX, def.CY);
        hcy := MY(ARootMatrix, def.CX, def.CY);
        hr  := def.R * scale;
      end
      else
      begin
        hcx := def.CX * AvbW * scale;
        hcy := def.CY * AvbH * scale;
        hr  := def.R * Min(AvbW, AvbH) * scale;
      end;

      { Renderer decodes: r = 64 * sqrt(sx^2 + shy^2)
        For a circular gradient: sx = r/64, shy = 0 }
      Result.GradTransform[0] := Single(hr / 64.0);
      Result.GradTransform[1] := 0.0;
      Result.GradTransform[2] := 0.0;
      Result.GradTransform[3] := 0.0;
      Result.GradTransform[4] := Single(hcx);
      Result.GradTransform[5] := Single(hcy);
    end;
  end;
end;


{ =========================================================
  Helper: element attribute access
  ========================================================= }

function GetAttr(elem: TDOMElement; const name: string): string;
begin
  Result := elem.GetAttribute(name);
end;

{ Returns True if the element should be excluded from rendering.
  Checks display:none and visibility:hidden in both the style attribute
  and direct presentation attributes. }
function IsElementVisible(elem: TDOMElement): Boolean;
var
  styleProps: TStringList;
  dispStr, visStr: string;
begin
  Result := True;

  dispStr := LowerCase(Trim(GetAttr(elem, 'display')));
  visStr  := LowerCase(Trim(GetAttr(elem, 'visibility')));

  styleProps := ParseInlineStyle(GetAttr(elem, 'style'));
  try
    if styleProps.IndexOfName('display') >= 0 then
      dispStr := LowerCase(Trim(styleProps.Values['display']));
    if styleProps.IndexOfName('visibility') >= 0 then
      visStr := LowerCase(Trim(styleProps.Values['visibility']));
  finally
    styleProps.Free;
  end;

  if dispStr = 'none' then Result := False;
  if visStr  = 'hidden' then Result := False;
end;


{ =========================================================
  TfpgSvgToHvif.Convert — main entry point
  ========================================================= }

class procedure TfpgSvgToHvif.Convert(const ASvgFile, AHvifFile: string);
var
  doc: TXMLDocument;
  svgRoot: TDOMElement;
  gradDefs: TStringList;
  vbStr: string;
  vbX, vbY, vbW, vbH: Double;
  rootMatrix: TSvgMatrix;
  writer: THvifWriter;
  nextPathIdx: Integer;
  addedStyles: array of THvifStyle;
  addedStyleCount: Integer;
  i: Integer;

  { ---- Style deduplication ---- }
  function StylesEqual(const A, B: THvifStyle): Boolean;
  var
    k, n: Integer;
  begin
    Result := False;
    if A.StyleType <> B.StyleType then Exit;
    if A.StyleType = hstSolidColor then
    begin
      Result := (A.Color.R = B.Color.R) and
                (A.Color.G = B.Color.G) and
                (A.Color.B = B.Color.B) and
                (A.Color.A = B.Color.A);
      Exit;
    end;
    if A.StyleType = hstGradient then
    begin
      if A.GradientType <> B.GradientType then Exit;
      if A.HasGradTransform <> B.HasGradTransform then Exit;
      for k := 0 to 5 do
        if A.GradTransform[k] <> B.GradTransform[k] then Exit;
      n := Length(A.Stops);
      if n <> Length(B.Stops) then Exit;
      for k := 0 to n - 1 do
      begin
        if A.Stops[k].Color.R <> B.Stops[k].Color.R then Exit;
        if A.Stops[k].Color.G <> B.Stops[k].Color.G then Exit;
        if A.Stops[k].Color.B <> B.Stops[k].Color.B then Exit;
        if A.Stops[k].Color.A <> B.Stops[k].Color.A then Exit;
        if A.Stops[k].Offset  <> B.Stops[k].Offset  then Exit;
      end;
      Result := True;
      Exit;
    end;
    { Other style types: compare colour only }
    Result := (A.Color.R = B.Color.R) and
              (A.Color.G = B.Color.G) and
              (A.Color.B = B.Color.B) and
              (A.Color.A = B.Color.A);
  end;

  function FindOrAddStyle(const s: THvifStyle): Integer;
  var
    k: Integer;
  begin
    for k := 0 to addedStyleCount - 1 do
      if StylesEqual(addedStyles[k], s) then
      begin
        Result := k;
        Exit;
      end;
    { Not found — append }
    if addedStyleCount >= Length(addedStyles) then
      SetLength(addedStyles, addedStyleCount + 32);
    addedStyles[addedStyleCount] := s;
    Inc(addedStyleCount);
    writer.AddStyle(s);
    Result := addedStyleCount - 1;
  end;

  { ---- Recursive element processor ---- }
  procedure ProcessElement(elem: TDOMElement; const groupMatrix: TSvgMatrix); forward;

  procedure ProcessElement(elem: TDOMElement; const groupMatrix: TSvgMatrix);
  var
    elemName: string;
    localMatrix, childMatrix: TSvgMatrix;
    transformStr, fillStr, opacStr, styleStr, gradId, elemName2: string;
    svgColor: TSvgColor;
    opacity: Double;
    hvifStyle: THvifStyle;
    hvifShape: THvifShape;
    paths: TPathList;
    path: THvifPath;
    parser: TSvgPathParser;
    pts: TDoubleArray;
    rx, ry, cx, cy, r, x1, y1, x2, y2: Double;
    attrW, attrH, attrX, attrY: Double;
    child: TDOMNode;
    styleIdx, j: Integer;
    def: TSvgGradientDef;
    defIdx: Integer;
    styleProps: TStringList;
  begin
    if not IsElementVisible(elem) then Exit;

    elemName := LowerCase(elem.NodeName);

    { Skip <defs> subtrees entirely }
    if SameText(elemName, 'defs') then Exit;

    { Apply this element's transform onto the group matrix }
    transformStr := GetAttr(elem, 'transform');
    if transformStr <> '' then
      localMatrix := SvgMatrixMul(groupMatrix, ParseSvgTransform(transformStr))
    else
      localMatrix := groupMatrix;

    { Recurse into <g> and <svg> containers }
    if SameText(elemName, 'g') or SameText(elemName, 'svg') then
    begin
      child := elem.FirstChild;
      while Assigned(child) do
      begin
        if child.NodeType = ELEMENT_NODE then
          ProcessElement(TDOMElement(child), localMatrix);
        child := child.NextSibling;
      end;
      Exit;
    end;

    { Only handle the seven drawable shape elements }
    if not (SameText(elemName, 'path')     or SameText(elemName, 'rect')     or
            SameText(elemName, 'circle')   or SameText(elemName, 'ellipse')  or
            SameText(elemName, 'polygon')  or SameText(elemName, 'polyline') or
            SameText(elemName, 'line')) then
      Exit;

    { ---- Resolve fill colour with inline style override ---- }
    fillStr := GetAttr(elem, 'fill');
    opacStr := '';

    styleStr := GetAttr(elem, 'style');
    if styleStr <> '' then
    begin
      styleProps := ParseInlineStyle(styleStr);
      try
        if styleProps.IndexOfName('fill') >= 0 then
          fillStr := styleProps.Values['fill'];
        if styleProps.IndexOfName('fill-opacity') >= 0 then
          opacStr := styleProps.Values['fill-opacity'];
        if styleProps.IndexOfName('opacity') >= 0 then
        begin
          if opacStr = '' then
            opacStr := styleProps.Values['opacity']
          else
          begin
            opacity := SvgStrToFloatDef(opacStr, 1.0) *
                       SvgStrToFloatDef(styleProps.Values['opacity'], 1.0);
            Str(opacity:0:6, opacStr);
          end;
        end;
      finally
        styleProps.Free;
      end;
    end;

    if fillStr = '' then fillStr := 'black';

    if opacStr = '' then opacStr := GetAttr(elem, 'fill-opacity');
    if opacStr = '' then opacStr := GetAttr(elem, 'opacity');

    { ---- Build HVIF style record ---- }
    FillChar(hvifStyle, SizeOf(hvifStyle), 0);

    if IsUrlRef(fillStr, gradId) then
    begin
      defIdx := gradDefs.IndexOf(gradId);
      if defIdx < 0 then Exit;
      def := TSvgGradientDef(gradDefs.Objects[defIdx]);
      hvifStyle := BuildGradientStyle(def, gradDefs, rootMatrix, vbW, vbH);
      if opacStr <> '' then
      begin
        opacity := EnsureRange(SvgStrToFloatDef(opacStr, 1.0), 0.0, 1.0);
        for j := 0 to High(hvifStyle.Stops) do
          hvifStyle.Stops[j].Color.A :=
            Byte(Round(hvifStyle.Stops[j].Color.A * opacity));
      end;
    end
    else
    begin
      svgColor := ParseSvgColor(fillStr);
      if svgColor.IsNone then Exit;

      if opacStr <> '' then
      begin
        opacity := EnsureRange(SvgStrToFloatDef(opacStr, 1.0), 0.0, 1.0);
        svgColor.A := Byte(Round(svgColor.A * opacity));
      end;

      hvifStyle.StyleType := hstSolidColor;
      hvifStyle.Color.R   := svgColor.R;
      hvifStyle.Color.G   := svgColor.G;
      hvifStyle.Color.B   := svgColor.B;
      hvifStyle.Color.A   := svgColor.A;
    end;

    styleIdx := FindOrAddStyle(hvifStyle);

    { ---- Generate path(s) for the element ---- }
    SetLength(paths, 0);
    elemName2 := elemName; { suppress warning about using loop var in nested }

    if SameText(elemName2, 'path') then
    begin
      parser := TSvgPathParser.Create(GetAttr(elem, 'd'), localMatrix);
      try
        paths := parser.Parse;
      finally
        parser.Free;
      end;
    end
    else if SameText(elemName2, 'rect') then
    begin
      attrX := SvgStrToFloatDef(GetAttr(elem, 'x'), 0);
      attrY := SvgStrToFloatDef(GetAttr(elem, 'y'), 0);
      attrW := SvgStrToFloatDef(GetAttr(elem, 'width'),  0);
      attrH := SvgStrToFloatDef(GetAttr(elem, 'height'), 0);
      rx    := SvgStrToFloatDef(GetAttr(elem, 'rx'), 0);
      ry    := SvgStrToFloatDef(GetAttr(elem, 'ry'), 0);
      if (rx = 0) and (ry > 0) then rx := ry;
      if (ry = 0) and (rx > 0) then ry := rx;
      if (attrW > 0) and (attrH > 0) then
      begin
        SetLength(paths, 1);
        paths[0] := BuildRectPath(attrX, attrY, attrW, attrH, rx, ry, localMatrix);
      end;
    end
    else if SameText(elemName2, 'circle') then
    begin
      cx := SvgStrToFloatDef(GetAttr(elem, 'cx'), 0);
      cy := SvgStrToFloatDef(GetAttr(elem, 'cy'), 0);
      r  := SvgStrToFloatDef(GetAttr(elem, 'r'),  0);
      if r > 0 then
      begin
        SetLength(paths, 1);
        paths[0] := BuildEllipsePath(cx, cy, r, r, localMatrix);
      end;
    end
    else if SameText(elemName2, 'ellipse') then
    begin
      cx := SvgStrToFloatDef(GetAttr(elem, 'cx'), 0);
      cy := SvgStrToFloatDef(GetAttr(elem, 'cy'), 0);
      rx := SvgStrToFloatDef(GetAttr(elem, 'rx'), 0);
      ry := SvgStrToFloatDef(GetAttr(elem, 'ry'), 0);
      if (rx > 0) and (ry > 0) then
      begin
        SetLength(paths, 1);
        paths[0] := BuildEllipsePath(cx, cy, rx, ry, localMatrix);
      end;
    end
    else if SameText(elemName2, 'polygon') then
    begin
      pts := ParseSvgPoints(GetAttr(elem, 'points'));
      if Length(pts) >= 4 then
      begin
        SetLength(paths, 1);
        paths[0] := BuildPolygonPath(pts, True, localMatrix);
      end;
    end
    else if SameText(elemName2, 'polyline') then
    begin
      pts := ParseSvgPoints(GetAttr(elem, 'points'));
      if Length(pts) >= 4 then
      begin
        SetLength(paths, 1);
        paths[0] := BuildPolygonPath(pts, False, localMatrix);
      end;
    end
    else if SameText(elemName2, 'line') then
    begin
      x1 := SvgStrToFloatDef(GetAttr(elem, 'x1'), 0);
      y1 := SvgStrToFloatDef(GetAttr(elem, 'y1'), 0);
      x2 := SvgStrToFloatDef(GetAttr(elem, 'x2'), 0);
      y2 := SvgStrToFloatDef(GetAttr(elem, 'y2'), 0);
      SetLength(pts, 4);
      pts[0] := x1; pts[1] := y1;
      pts[2] := x2; pts[3] := y2;
      SetLength(paths, 1);
      paths[0] := BuildPolygonPath(pts, False, localMatrix);
    end;

    if Length(paths) = 0 then Exit;

    { Filter out degenerate paths (fewer than 2 points) }
    j := 0;
    while j <= High(paths) do
    begin
      if Length(paths[j].Points) < 2 then
      begin
        path := paths[j];
        Move(paths[j+1], paths[j],
          (Length(paths) - j - 1) * SizeOf(THvifPath));
        SetLength(paths, Length(paths) - 1);
      end
      else
        Inc(j);
    end;

    if Length(paths) = 0 then Exit;

    { Add all sub-paths to the writer }
    for j := 0 to High(paths) do
      writer.AddPath(paths[j]);

    { Build shape referencing this style and all its paths }
    FillChar(hvifShape, SizeOf(hvifShape), 0);
    hvifShape.StyleIndex     := Byte(styleIdx);
    hvifShape.HasTransform   := False;
    hvifShape.HasTranslation := False;
    SetLength(hvifShape.PathIndices, Length(paths));
    for j := 0 to High(paths) do
      hvifShape.PathIndices[j] := Byte(nextPathIdx + j);
    writer.AddShape(hvifShape);

    Inc(nextPathIdx, Length(paths));
  end;

begin
  ReadXMLFile(doc, ASvgFile);
  try
    { ---- Determine coordinate transform from viewBox ---- }
    svgRoot := doc.DocumentElement as TDOMElement;
    vbStr := GetAttr(svgRoot, 'viewBox');
    if not ParseViewBox(vbStr, vbX, vbY, vbW, vbH) then
    begin
      vbX := 0; vbY := 0;
      vbW := SvgStrToFloatDef(GetAttr(svgRoot, 'width'),  64);
      vbH := SvgStrToFloatDef(GetAttr(svgRoot, 'height'), 64);
    end;
    rootMatrix := MakeViewBoxMatrix(vbX, vbY, vbW, vbH);

    { ---- First pass: collect gradient definitions from <defs> ---- }
    gradDefs := TStringList.Create;
    try
      gradDefs.CaseSensitive := True;
      CollectGradientDefs(doc.DocumentElement, gradDefs);

      writer := THvifWriter.Create;
      try
        nextPathIdx    := 0;
        addedStyleCount := 0;
        SetLength(addedStyles, 32);

        { ---- Second pass: traverse all elements recursively ---- }
        ProcessElement(svgRoot, rootMatrix);

        writer.SaveToFile(AHvifFile);
      finally
        writer.Free;
      end;

    finally
      for i := 0 to gradDefs.Count - 1 do
        gradDefs.Objects[i].Free;
      gradDefs.Free;
    end;
  finally
    doc.Free;
  end;
end;


initialization
  GSvgFmt := DefaultFormatSettings;
  GSvgFmt.DecimalSeparator  := '.';
  GSvgFmt.ThousandSeparator := ',';

end.
