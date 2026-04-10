{
    svg2hvif_converter.pas — SVG to HVIF conversion logic.

    Converts a subset of SVG to the Haiku Vector Icon Format (HVIF).

    Supported SVG features (v2):
      - <path> elements with 'd' attribute
      - <rect>, <circle>, <ellipse>, <polygon>, <polyline>, <line> elements
      - Path commands: M m L l H h V v C c S s Q q T t Z z
      - Arc commands (A a) are silently skipped
      - fill attribute: #RRGGBB, #RGB, rgb(R,G,B), named colours
      - fill-opacity and opacity attributes
      - style="" inline CSS (fill, fill-opacity, opacity)
      - viewBox attribute on <svg> for coordinate scaling
      - Linear and radial gradients via <defs>/<linearGradient>/<radialGradient>
      - url(#id) fill references resolved to gradient styles
      - xlink:href gradient inheritance for stop colours

    Limitations:
      - <g> element transforms not applied (children processed individually)
      - stroke not converted
      - Arc-to-bezier conversion not implemented (arcs silently dropped)
      - Gradient objectBoundingBox units use viewBox as bounding box approximation
      - HVIF renderer uses only first and last gradient stop colours

    Coordinate transform:
      SVG viewBox is fitted uniformly into the HVIF 64 x 64 unit space.
      Scale = 64 / max(viewBoxWidth, viewBoxHeight).
}

unit svg2hvif_converter;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math, Classes,
  fpg_hvif, fpg_hvif_writer;

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
    FScale: Double;
    FOffX, FOffY: Double;

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

    function  TX(x: Double): Single;
    function  TY(y: Double): Single;

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
    procedure SkipArc(rel: Boolean);

  public
    constructor Create(const AStr: string; AScale, AOffX, AOffY: Double);
    function  Parse: TPathList;
  end;


constructor TSvgPathParser.Create(const AStr: string; AScale, AOffX, AOffY: Double);
begin
  FStr    := AStr;
  FPos    := 1;
  FScale  := AScale;
  FOffX   := AOffX;
  FOffY   := AOffY;
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

function TSvgPathParser.TX(x: Double): Single;
begin
  Result := Single((x - FOffX) * FScale);
end;

function TSvgPathParser.TY(y: Double): Single;
begin
  Result := Single((y - FOffY) * FScale);
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
    pt.X   := TX(FCurX); pt.Y   := TY(FCurY);
    pt.InX := pt.X;      pt.InY := pt.Y;
    pt.OutX:= pt.X;      pt.OutY:= pt.Y;
    FCurPoints[0] := pt;
  end;
end;

procedure TSvgPathParser.AddLineTo(x, y: Double);
var
  pt: THvifPoint;
begin
  EnsureStartPoint;
  SetLength(FCurPoints, Length(FCurPoints) + 1);
  pt.X   := TX(x); pt.Y   := TY(y);
  pt.InX := pt.X;  pt.InY := pt.Y;
  pt.OutX:= pt.X;  pt.OutY:= pt.Y;
  FCurPoints[High(FCurPoints)] := pt;
  FCurX := x; FCurY := y;
end;

procedure TSvgPathParser.AddCubicTo(c1x, c1y, c2x, c2y, x, y: Double);
var
  pt: THvifPoint;
begin
  EnsureStartPoint;
  FCurPoints[High(FCurPoints)].OutX := TX(c1x);
  FCurPoints[High(FCurPoints)].OutY := TY(c1y);
  SetLength(FCurPoints, Length(FCurPoints) + 1);
  pt.X   := TX(x);   pt.Y   := TY(y);
  pt.InX := TX(c2x); pt.InY := TY(c2y);
  pt.OutX:= pt.X;    pt.OutY:= pt.Y;
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

procedure TSvgPathParser.SkipArc(rel: Boolean);
begin
  while IsNumericStart do
  begin
    ReadNumber; SkipWS;  { rx }
    ReadNumber; SkipWS;  { ry }
    ReadNumber; SkipWS;  { x-rotation }
    ReadNumber; SkipWS;  { large-arc-flag }
    ReadNumber; SkipWS;  { sweep-flag }
    ReadNumber; SkipWS;  { x }
    ReadNumber; SkipWS;  { y }
  end;
  FLastCmd := 'A';
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
        'A': SkipArc(False);
        'a': SkipArc(True);
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
  Shape element path builders
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

{ Build a simple straight-segment path from a flat coordinate array.
  AScale, AOffX, AOffY apply the SVG->HVIF transform. }
function BuildPolygonPath(const pts: TDoubleArray; AClosed: Boolean;
  AScale, AOffX, AOffY: Double): THvifPath;
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
    hx := Single((pts[i*2]     - AOffX) * AScale);
    hy := Single((pts[i*2 + 1] - AOffY) * AScale);
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
  AScale, AOffX, AOffY: Double): THvifPath;
var
  hcx, hcy, hrx, hry, k: Single;
begin
  hcx := Single((cx - AOffX) * AScale);
  hcy := Single((cy - AOffY) * AScale);
  hrx := Single(rx * AScale);
  hry := Single(ry * AScale);
  k   := KAPPA;

  Result.Closed := True;
  SetLength(Result.Points, 4);

  { N: top of ellipse at (cx, cy-ry) }
  Result.Points[0].X    := hcx;
  Result.Points[0].Y    := hcy - hry;
  Result.Points[0].InX  := hcx - k * hrx;   { from W->N closing arc }
  Result.Points[0].InY  := hcy - hry;
  Result.Points[0].OutX := hcx + k * hrx;   { to N->E arc }
  Result.Points[0].OutY := hcy - hry;

  { E: right of ellipse at (cx+rx, cy) }
  Result.Points[1].X    := hcx + hrx;
  Result.Points[1].Y    := hcy;
  Result.Points[1].InX  := hcx + hrx;       { from N->E arc }
  Result.Points[1].InY  := hcy - k * hry;
  Result.Points[1].OutX := hcx + hrx;       { to E->S arc }
  Result.Points[1].OutY := hcy + k * hry;

  { S: bottom of ellipse at (cx, cy+ry) }
  Result.Points[2].X    := hcx;
  Result.Points[2].Y    := hcy + hry;
  Result.Points[2].InX  := hcx + k * hrx;   { from E->S arc }
  Result.Points[2].InY  := hcy + hry;
  Result.Points[2].OutX := hcx - k * hrx;   { to S->W arc }
  Result.Points[2].OutY := hcy + hry;

  { W: left of ellipse at (cx-rx, cy) }
  Result.Points[3].X    := hcx - hrx;
  Result.Points[3].Y    := hcy;
  Result.Points[3].InX  := hcx - hrx;       { from S->W arc }
  Result.Points[3].InY  := hcy + k * hry;
  Result.Points[3].OutX := hcx - hrx;       { to W->N closing arc }
  Result.Points[3].OutY := hcy - k * hry;
end;

{ Build a closed rectangle path with optional rounded corners.
  rx=ry=0 produces a simple 4-point rect; otherwise 8 points with arcs. }
function BuildRectPath(x, y, w, h, rx, ry: Double;
  AScale, AOffX, AOffY: Double): THvifPath;
var
  lx1, ly1, lx2, ly2: Single;
  lrx, lry, lk: Single;
begin
  lx1 := Single((x     - AOffX) * AScale);
  ly1 := Single((y     - AOffY) * AScale);
  lx2 := Single((x + w - AOffX) * AScale);
  ly2 := Single((y + h - AOffY) * AScale);
  lrx := Single(rx * AScale);
  lry := Single(ry * AScale);
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
    { Rounded rectangle: 8 points (clockwise from TL arc end).
      Arc direction: each corner arc curves inward from edge to edge.
      P0: TL arc end = (x+rx, y)
      P1: TR arc start = (x+w-rx, y)
      P2: TR arc end = (x+w, y+ry)
      P3: BR arc start = (x+w, y+h-ry)
      P4: BR arc end = (x+w-rx, y+h)
      P5: BL arc start = (x+rx, y+h)
      P6: BL arc end = (x, y+h-ry)
      P7: TL arc start = (x, y+ry) }
    SetLength(Result.Points, 8);

    { P0: TL arc end }
    Result.Points[0].X    := lx1 + lrx;
    Result.Points[0].Y    := ly1;
    Result.Points[0].InX  := lx1 + lrx - lk * lrx;  { c2 of TL arc }
    Result.Points[0].InY  := ly1;
    Result.Points[0].OutX := lx1 + lrx;              { straight to P1 }
    Result.Points[0].OutY := ly1;

    { P1: TR arc start }
    Result.Points[1].X    := lx2 - lrx;
    Result.Points[1].Y    := ly1;
    Result.Points[1].InX  := lx2 - lrx;              { straight from P0 }
    Result.Points[1].InY  := ly1;
    Result.Points[1].OutX := lx2 - lrx + lk * lrx;  { c1 of TR arc }
    Result.Points[1].OutY := ly1;

    { P2: TR arc end }
    Result.Points[2].X    := lx2;
    Result.Points[2].Y    := ly1 + lry;
    Result.Points[2].InX  := lx2;                    { c2 of TR arc }
    Result.Points[2].InY  := ly1 + lry - lk * lry;
    Result.Points[2].OutX := lx2;                    { straight to P3 }
    Result.Points[2].OutY := ly1 + lry;

    { P3: BR arc start }
    Result.Points[3].X    := lx2;
    Result.Points[3].Y    := ly2 - lry;
    Result.Points[3].InX  := lx2;                    { straight from P2 }
    Result.Points[3].InY  := ly2 - lry;
    Result.Points[3].OutX := lx2;                    { c1 of BR arc }
    Result.Points[3].OutY := ly2 - lry + lk * lry;

    { P4: BR arc end }
    Result.Points[4].X    := lx2 - lrx;
    Result.Points[4].Y    := ly2;
    Result.Points[4].InX  := lx2 - lrx + lk * lrx;  { c2 of BR arc }
    Result.Points[4].InY  := ly2;
    Result.Points[4].OutX := lx2 - lrx;              { straight to P5 }
    Result.Points[4].OutY := ly2;

    { P5: BL arc start }
    Result.Points[5].X    := lx1 + lrx;
    Result.Points[5].Y    := ly2;
    Result.Points[5].InX  := lx1 + lrx;              { straight from P4 }
    Result.Points[5].InY  := ly2;
    Result.Points[5].OutX := lx1 + lrx - lk * lrx;  { c1 of BL arc }
    Result.Points[5].OutY := ly2;

    { P6: BL arc end }
    Result.Points[6].X    := lx1;
    Result.Points[6].Y    := ly2 - lry;
    Result.Points[6].InX  := lx1;                    { c2 of BL arc }
    Result.Points[6].InY  := ly2 - lry + lk * lry;
    Result.Points[6].OutX := lx1;                    { straight to P7 }
    Result.Points[6].OutY := ly2 - lry;

    { P7: TL arc start }
    Result.Points[7].X    := lx1;
    Result.Points[7].Y    := ly1 + lry;
    Result.Points[7].InX  := lx1;                    { straight from P6 }
    Result.Points[7].InY  := ly1 + lry;
    Result.Points[7].OutX := lx1;                    { c1 of TL arc }
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
  AScale/AOffX/AOffY: the SVG->HVIF coordinate transform.
  AvbW/AvbH: viewBox dimensions used as bounding-box approximation for
             objectBoundingBox gradient units.
  ADefs: the full gradient dictionary for resolving xlink:href stop inheritance. }
function BuildGradientStyle(def: TSvgGradientDef; ADefs: TStringList;
  AScale, AOffX, AOffY, AvbW, AvbH: Double): THvifStyle;
var
  stops: array of TSvgGradientStop;
  refIdx: Integer;
  refDef: TSvgGradientDef;
  hx1, hy1, hx2, hy2: Double;
  hcx, hcy, hr: Double;
  tx, ty, sx, shy: Double;
  i: Integer;
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

  case def.Kind of
    sgkLinear:
    begin
      Result.GradientType := hgtLinear;

      { Convert SVG gradient endpoints to HVIF 64-unit space }
      if SameText(def.GradientUnits, 'userSpaceOnUse') then
      begin
        hx1 := (def.X1 - AOffX) * AScale;
        hy1 := (def.Y1 - AOffY) * AScale;
        hx2 := (def.X2 - AOffX) * AScale;
        hy2 := (def.Y2 - AOffY) * AScale;
      end
      else
      begin
        { objectBoundingBox: fractions 0..1 relative to bounding box.
          Approximate bounding box with the full viewBox. }
        hx1 := def.X1 * AvbW * AScale;
        hy1 := def.Y1 * AvbH * AScale;
        hx2 := def.X2 * AvbW * AScale;
        hy2 := def.Y2 * AvbH * AScale;
      end;

      { Map to HVIF gradient transform.
        Renderer decodes: p1 = (tx - sx*64, ty - shy*64)
                          p2 = (tx + sx*64, ty + shy*64)
        Therefore: tx  = (hx1+hx2)/2, ty  = (hy1+hy2)/2
                   sx  = (hx2-hx1)/128
                   shy = (hy2-hy1)/128 }
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
        hcx := (def.CX - AOffX) * AScale;
        hcy := (def.CY - AOffY) * AScale;
        hr  := def.R * AScale;
      end
      else
      begin
        hcx := def.CX * AvbW * AScale;
        hcy := def.CY * AvbH * AScale;
        hr  := def.R * Min(AvbW, AvbH) * AScale;
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
  SVG element traversal helpers
  ========================================================= }

function GetAttr(elem: TDOMElement; const name: string): string;
begin
  Result := elem.GetAttribute(name);
end;

{ Collect all visible shape elements (<path>, <rect>, <circle>, <ellipse>,
  <polygon>, <polyline>, <line>) via a depth-first DOM walk.
  <defs> subtrees are skipped (they contain only definitions, not shapes). }
procedure CollectShapeElements(node: TDOMNode; AList: TList);
var
  child: TDOMNode;
  name: string;
begin
  if node.NodeType <> ELEMENT_NODE then Exit;

  name := LowerCase(node.NodeName);

  { Skip <defs> — their contents are definitions, not rendered shapes }
  if SameText(name, 'defs') then Exit;

  if SameText(name, 'path')     or SameText(name, 'rect')     or
     SameText(name, 'circle')   or SameText(name, 'ellipse')  or
     SameText(name, 'polygon')  or SameText(name, 'polyline') or
     SameText(name, 'line') then
    AList.Add(node);

  child := node.FirstChild;
  while Assigned(child) do
  begin
    CollectShapeElements(child, AList);
    child := child.NextSibling;
  end;
end;


{ =========================================================
  TfpgSvgToHvif.Convert — main entry point
  ========================================================= }

class procedure TfpgSvgToHvif.Convert(const ASvgFile, AHvifFile: string);
var
  doc: TXMLDocument;
  svgRoot: TDOMElement;
  elemList: TList;
  elem: TDOMElement;
  gradDefs: TStringList;
  vbStr, fillStr, opacStr, styleStr, gradId, elemName: string;
  vbX, vbY, vbW, vbH, scale: Double;
  svgColor: TSvgColor;
  opacity: Double;
  writer: THvifWriter;
  parser: TSvgPathParser;
  paths: TPathList;
  path: THvifPath;
  hvifStyle: THvifStyle;
  hvifShape: THvifShape;
  nextStyleIdx, nextPathIdx: Integer;
  i, j: Integer;
  styleProps: TStringList;
  def: TSvgGradientDef;
  defIdx: Integer;
  pts: TDoubleArray;
  rx, ry, cx, cy, r, x1, y1, x2, y2: Double;
  attrW, attrH, attrX, attrY: Double;
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
    scale := 64.0 / Max(vbW, vbH);

    { ---- First pass: collect gradient definitions from <defs> ---- }
    gradDefs := TStringList.Create;
    try
      gradDefs.CaseSensitive := True;
      CollectGradientDefs(doc.DocumentElement, gradDefs);

      writer := THvifWriter.Create;
      try
        nextStyleIdx := 0;
        nextPathIdx  := 0;

        { ---- Second pass: collect and process all shape elements ---- }
        elemList := TList.Create;
        try
          CollectShapeElements(doc.DocumentElement, elemList);

          for i := 0 to elemList.Count - 1 do
          begin
            elem     := TDOMElement(elemList[i]);
            elemName := LowerCase(elem.NodeName);

            { ---- Resolve fill colour with inline style override ---- }
            fillStr := GetAttr(elem, 'fill');
            opacStr := '';

            styleStr := GetAttr(elem, 'style');
            if styleStr <> '' then
            begin
              styleProps := ParseInlineStyle(styleStr);
              try
                { Inline style takes precedence over presentation attributes }
                if styleProps.IndexOfName('fill') >= 0 then
                  fillStr := styleProps.Values['fill'];
                if styleProps.IndexOfName('fill-opacity') >= 0 then
                  opacStr := styleProps.Values['fill-opacity'];
                if styleProps.IndexOfName('opacity') >= 0 then
                begin
                  { When both are present, multiply; store in opacStr for now }
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

            { ---- Apply fill-opacity and opacity from direct attributes
                   (only if not already set by inline style) ---- }
            if opacStr = '' then
              opacStr := GetAttr(elem, 'fill-opacity');
            if opacStr = '' then
              opacStr := GetAttr(elem, 'opacity');

            { ---- Build HVIF style record ---- }
            FillChar(hvifStyle, SizeOf(hvifStyle), 0);

            if IsUrlRef(fillStr, gradId) then
            begin
              { Gradient fill: look up definition }
              defIdx := gradDefs.IndexOf(gradId);
              if defIdx < 0 then Continue; { Unknown gradient — skip shape }
              def := TSvgGradientDef(gradDefs.Objects[defIdx]);
              hvifStyle := BuildGradientStyle(def, gradDefs,
                             scale, vbX, vbY, vbW, vbH);
              { Apply overall opacity to gradient stops }
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
              { Solid colour fill }
              svgColor := ParseSvgColor(fillStr);
              if svgColor.IsNone then Continue;

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

            writer.AddStyle(hvifStyle);

            { ---- Generate path(s) for the element ---- }
            SetLength(paths, 0);

            if SameText(elemName, 'path') then
            begin
              parser := TSvgPathParser.Create(
                GetAttr(elem, 'd'), scale, vbX, vbY);
              try
                paths := parser.Parse;
              finally
                parser.Free;
              end;
            end
            else if SameText(elemName, 'rect') then
            begin
              attrX := SvgStrToFloatDef(GetAttr(elem, 'x'), 0);
              attrY := SvgStrToFloatDef(GetAttr(elem, 'y'), 0);
              attrW := SvgStrToFloatDef(GetAttr(elem, 'width'),  0);
              attrH := SvgStrToFloatDef(GetAttr(elem, 'height'), 0);
              rx    := SvgStrToFloatDef(GetAttr(elem, 'rx'), 0);
              ry    := SvgStrToFloatDef(GetAttr(elem, 'ry'), 0);
              { SVG spec: if only one of rx/ry is given, the other equals it }
              if (rx = 0) and (ry > 0) then rx := ry;
              if (ry = 0) and (rx > 0) then ry := rx;
              if (attrW > 0) and (attrH > 0) then
              begin
                SetLength(paths, 1);
                paths[0] := BuildRectPath(
                  attrX, attrY, attrW, attrH, rx, ry, scale, vbX, vbY);
              end;
            end
            else if SameText(elemName, 'circle') then
            begin
              cx := SvgStrToFloatDef(GetAttr(elem, 'cx'), 0);
              cy := SvgStrToFloatDef(GetAttr(elem, 'cy'), 0);
              r  := SvgStrToFloatDef(GetAttr(elem, 'r'),  0);
              if r > 0 then
              begin
                SetLength(paths, 1);
                paths[0] := BuildEllipsePath(cx, cy, r, r, scale, vbX, vbY);
              end;
            end
            else if SameText(elemName, 'ellipse') then
            begin
              cx := SvgStrToFloatDef(GetAttr(elem, 'cx'), 0);
              cy := SvgStrToFloatDef(GetAttr(elem, 'cy'), 0);
              rx := SvgStrToFloatDef(GetAttr(elem, 'rx'), 0);
              ry := SvgStrToFloatDef(GetAttr(elem, 'ry'), 0);
              if (rx > 0) and (ry > 0) then
              begin
                SetLength(paths, 1);
                paths[0] := BuildEllipsePath(cx, cy, rx, ry, scale, vbX, vbY);
              end;
            end
            else if SameText(elemName, 'polygon') then
            begin
              pts := ParseSvgPoints(GetAttr(elem, 'points'));
              if Length(pts) >= 4 then
              begin
                SetLength(paths, 1);
                paths[0] := BuildPolygonPath(pts, True, scale, vbX, vbY);
              end;
            end
            else if SameText(elemName, 'polyline') then
            begin
              pts := ParseSvgPoints(GetAttr(elem, 'points'));
              if Length(pts) >= 4 then
              begin
                SetLength(paths, 1);
                paths[0] := BuildPolygonPath(pts, False, scale, vbX, vbY);
              end;
            end
            else if SameText(elemName, 'line') then
            begin
              x1 := SvgStrToFloatDef(GetAttr(elem, 'x1'), 0);
              y1 := SvgStrToFloatDef(GetAttr(elem, 'y1'), 0);
              x2 := SvgStrToFloatDef(GetAttr(elem, 'x2'), 0);
              y2 := SvgStrToFloatDef(GetAttr(elem, 'y2'), 0);
              { Build as a two-point open polygon }
              SetLength(pts, 4);
              pts[0] := x1; pts[1] := y1;
              pts[2] := x2; pts[3] := y2;
              SetLength(paths, 1);
              paths[0] := BuildPolygonPath(pts, False, scale, vbX, vbY);
            end;

            { Skip if no valid geometry produced }
            if Length(paths) = 0 then
            begin
              Inc(nextStyleIdx);
              Continue;
            end;

            { Filter out degenerate paths (fewer than 2 points) }
            j := 0;
            while j <= High(paths) do
            begin
              if Length(paths[j].Points) < 2 then
              begin
                { Remove this path by shifting remaining down }
                path := paths[j];
                Move(paths[j+1], paths[j],
                  (Length(paths) - j - 1) * SizeOf(THvifPath));
                SetLength(paths, Length(paths) - 1);
              end
              else
                Inc(j);
            end;

            if Length(paths) = 0 then
            begin
              Inc(nextStyleIdx);
              Continue;
            end;

            { Add all sub-paths to the writer }
            for j := 0 to High(paths) do
              writer.AddPath(paths[j]);

            { Build shape referencing this style and all its paths }
            FillChar(hvifShape, SizeOf(hvifShape), 0);
            hvifShape.StyleIndex     := nextStyleIdx;
            hvifShape.HasTransform   := False;
            hvifShape.HasTranslation := False;
            SetLength(hvifShape.PathIndices, Length(paths));
            for j := 0 to High(paths) do
              hvifShape.PathIndices[j] := nextPathIdx + j;
            writer.AddShape(hvifShape);

            Inc(nextStyleIdx);
            Inc(nextPathIdx, Length(paths));
          end;

        finally
          elemList.Free;
        end;

        writer.SaveToFile(AHvifFile);
      finally
        writer.Free;
      end;

    finally
      { Free the TSvgGradientDef objects owned by gradDefs }
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
