{
    svg2hvif_converter.pas — SVG to HVIF conversion logic.

    Converts a subset of SVG to the Haiku Vector Icon Format (HVIF).

    Supported SVG features (v1):
      - <path> elements with 'd' attribute
      - Path commands: M m L l H h V v C c S s Q q T t Z z
      - Arc commands (A a) are silently skipped
      - fill attribute: #RRGGBB, #RGB, rgb(R,G,B), named colours
      - fill-opacity and opacity attributes
      - viewBox attribute on <svg> for coordinate scaling

    Limitations (v1):
      - <g> element transforms not applied (children are processed with
        their own individual attributes only)
      - Gradient fill via url() references not supported
      - stroke not converted
      - style="" attribute not parsed
      - No arc-to-bezier conversion (arcs are dropped)

    Coordinate transform:
      SVG viewBox is fitted uniformly into the HVIF 64×64 unit space.
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
  SVG colour record and parser
  ========================================================= }

type
  TSvgColor = record
    R, G, B: Byte;
    A: Byte;        { 0=transparent, 255=opaque }
    IsNone: Boolean;
  end;


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

{ Parse comma/space-separated list of numbers from "R, G, B" inside rgb() }
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

  { Hex colour }
  if (Length(t) >= 1) and (t[1] = '#') then
  begin
    if Length(t) = 7 then   { #RRGGBB }
      Result := MakeColor(HexByte(t, 2), HexByte(t, 4), HexByte(t, 6))
    else if Length(t) = 4 then  { #RGB → expand }
      Result := MakeColor(
        HexNibble(t[2]) * 17,
        HexNibble(t[3]) * 17,
        HexNibble(t[4]) * 17);
    Exit;
  end;

  { rgb() or rgba() }
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

  { url() — gradient reference not supported }
  if SameText(Copy(t, 1, 4), 'url(') then Exit;   { IsNone stays True }

  { Named colours (CSS Level 1 + a few extras) }
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

  { Unknown — default to opaque black so shapes at least appear }
  Result := MakeColor(0, 0, 0);
end;


{ =========================================================
  viewBox parser
  ========================================================= }

function ParseViewBox(const s: string;
  out vbX, vbY, vbW, vbH: Double): Boolean;
var
  tokens: TStringList;
  t, cleaned: string;
  i: Integer;
begin
  Result := False;
  vbX := 0; vbY := 0; vbW := 64; vbH := 64;

  cleaned := Trim(s);
  if cleaned = '' then Exit;

  { Replace commas with spaces, then split on whitespace }
  for i := 1 to Length(cleaned) do
    if cleaned[i] = ',' then cleaned[i] := ' ';

  tokens := TStringList.Create;
  try
    tokens.Delimiter := ' ';
    tokens.StrictDelimiter := False;
    tokens.DelimitedText := cleaned;

    { Filter out empty strings that arise from multiple spaces }
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
  TPathList = array of THvifPath;

  TSvgPathParser = class
  private
    FStr: string;
    FPos: Integer;
    FScale: Double;
    FOffX, FOffY: Double;  { viewBox origin in SVG user space }

    { Current pen position in SVG user space }
    FCurX, FCurY: Double;
    { Sub-path start (for Z command) }
    FStartX, FStartY: Double;
    { Last cubic/smooth control point in SVG user space (for S command) }
    FLastCPX, FLastCPY: Double;
    { Last quadratic control point in SVG user space (for T command) }
    FLastQCPX, FLastQCPY: Double;
    { Last command letter }
    FLastCmd: Char;

    { Current sub-path accumulator }
    FCurPoints: array of THvifPoint;

    { Completed paths }
    FResult: TPathList;

    procedure SkipWS;
    function  PeekChar: Char;
    function  IsNumericStart: Boolean;
    function  ReadNumber: Double;

    { Coordinate transform: SVG user space → HVIF 64-unit space }
    function  TX(x: Double): Single;
    function  TY(y: Double): Single;

    { Sub-path management }
    procedure FinishSubPath(AClosed: Boolean);
    procedure EnsureStartPoint;

    { Segment adders (coordinates in SVG user space) }
    procedure AddLineTo(x, y: Double);
    procedure AddCubicTo(c1x, c1y, c2x, c2y, x, y: Double);

    { Command processors }
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
    { AScale and (AOffX, AOffY) map SVG user space to HVIF 64-unit space:
        hvifX = (svgX - AOffX) * AScale
        hvifY = (svgY - AOffY) * AScale }
    constructor Create(const AStr: string; AScale, AOffX, AOffY: Double);
    function  Parse: TPathList;
  end;


constructor TSvgPathParser.Create(const AStr: string; AScale, AOffX, AOffY: Double);
begin
  FStr   := AStr;
  FPos   := 1;
  FScale := AScale;
  FOffX  := AOffX;
  FOffY  := AOffY;
  FCurX  := 0; FCurY  := 0;
  FStartX:= 0; FStartY:= 0;
  FLastCPX := 0; FLastCPY := 0;
  FLastQCPX:= 0; FLastQCPY:= 0;
  FLastCmd := #0;
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
  { Also consider 'E'/'e' part of a number already started, but not as a start }
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
  { Previous point's Out stays equal to itself (line — no change needed) }
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
  { Update the outgoing control of the previous point }
  FCurPoints[High(FCurPoints)].OutX := TX(c1x);
  FCurPoints[High(FCurPoints)].OutY := TY(c1y);
  { Add the new endpoint }
  SetLength(FCurPoints, Length(FCurPoints) + 1);
  pt.X   := TX(x);   pt.Y   := TY(y);
  pt.InX := TX(c2x); pt.InY := TY(c2y);
  pt.OutX:= pt.X;    pt.OutY:= pt.Y;   { out = anchor until next curve updates it }
  FCurPoints[High(FCurPoints)] := pt;
  FCurX := x; FCurY := y;
  FLastCPX := c2x; FLastCPY := c2y;
end;


{ ---- Command handlers ---- }

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
  { Implicit lineto for subsequent coordinate pairs }
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
    { Reflect last control point if previous command was C or S }
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
    { Convert quadratic to cubic in SVG user space }
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
    { Reflect last quadratic control point if previous was Q or T }
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
  { Each arc segment: rx ry x-rotation large-arc-flag sweep-flag x y }
  while IsNumericStart do
  begin
    ReadNumber; SkipWS;  { rx }
    ReadNumber; SkipWS;  { ry }
    ReadNumber; SkipWS;  { x-rotation }
    ReadNumber; SkipWS;  { large-arc-flag }
    ReadNumber; SkipWS;  { sweep-flag }
    ReadNumber; SkipWS;  { x }
    ReadNumber; SkipWS;  { y }
    { Update pen position so subsequent commands stay correct }
    { We can't easily fake the arc endpoint without computing it,
      so the shape will be incomplete — acceptable v1 limitation. }
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
      Inc(FPos);   { consume command letter }
      SkipWS;
      case cmd of
        'M': begin FLastCmd := 'M'; DoMoveTo(False); end;
        'm': begin FLastCmd := 'M'; DoMoveTo(True);  end;
        'L': begin DoLineTo(False); end;
        'l': begin DoLineTo(True);  end;
        'H': begin DoHLine(False);  end;
        'h': begin DoHLine(True);   end;
        'V': begin DoVLine(False);  end;
        'v': begin DoVLine(True);   end;
        'C': begin DoCubic(False);  end;
        'c': begin DoCubic(True);   end;
        'S': begin DoSmoothCubic(False); end;
        's': begin DoSmoothCubic(True);  end;
        'Q': begin DoQuad(False);   end;
        'q': begin DoQuad(True);    end;
        'T': begin DoSmoothQuad(False); end;
        't': begin DoSmoothQuad(True);  end;
        'A': begin SkipArc(False);  end;
        'a': begin SkipArc(True);   end;
        'Z', 'z': DoClose;
      end;
    end
    else
      Inc(FPos);   { skip unrecognised character }
  end;
  FinishSubPath(False);
  Result := FResult;
end;


{ =========================================================
  SVG element traversal helper
  ========================================================= }

function GetAttr(elem: TDOMElement; const name: string): string;
begin
  Result := elem.GetAttribute(name);
end;

{ Walk the DOM tree depth-first, collecting <path> elements }
procedure CollectPaths(node: TDOMNode; list: TList);
var
  child: TDOMNode;
begin
  if node.NodeType = ELEMENT_NODE then
  begin
    if SameText(node.NodeName, 'path') then
      list.Add(node);
    child := node.FirstChild;
    while Assigned(child) do
    begin
      CollectPaths(child, list);
      child := child.NextSibling;
    end;
  end;
end;


{ =========================================================
  TfpgSvgToHvif.Convert — main entry point
  ========================================================= }

class procedure TfpgSvgToHvif.Convert(const ASvgFile, AHvifFile: string);
var
  doc: TXMLDocument;
  svgRoot: TDOMElement;
  pathList: TList;
  pathElem: TDOMElement;
  vbStr, dStr, fillStr, opacStr: string;
  vbX, vbY, vbW, vbH, scale: Double;
  svgColor: TSvgColor;
  opacity: Double;
  writer: THvifWriter;
  parser: TSvgPathParser;
  paths: TPathList;
  style: THvifStyle;
  shape: THvifShape;
  nextStyleIdx, nextPathIdx: Integer;
  i, j: Integer;
begin
  ReadXMLFile(doc, ASvgFile);
  try
    { ---- Determine coordinate transform from viewBox ---- }
    svgRoot := doc.DocumentElement as TDOMElement;
    vbStr := GetAttr(svgRoot, 'viewBox');
    if not ParseViewBox(vbStr, vbX, vbY, vbW, vbH) then
    begin
      { Fall back to width/height attributes if present }
      vbX := 0; vbY := 0;
      vbW := SvgStrToFloatDef(GetAttr(svgRoot, 'width'),  64);
      vbH := SvgStrToFloatDef(GetAttr(svgRoot, 'height'), 64);
    end;
    { Uniform scale to fit the longest axis into 64 HVIF units }
    scale := 64.0 / Max(vbW, vbH);

    writer := THvifWriter.Create;
    try
      nextStyleIdx := 0;
      nextPathIdx  := 0;

      { ---- Collect all <path> elements ---- }
      pathList := TList.Create;
      try
        CollectPaths(doc.DocumentElement, pathList);

        for i := 0 to pathList.Count - 1 do
        begin
          pathElem := TDOMElement(pathList[i]);

          dStr := GetAttr(pathElem, 'd');
          if dStr = '' then Continue;

          { Resolve fill colour }
          fillStr := GetAttr(pathElem, 'fill');
          if fillStr = '' then fillStr := 'black';   { SVG default }
          svgColor := ParseSvgColor(fillStr);
          if svgColor.IsNone then Continue;    { invisible — skip }

          { fill-opacity attribute }
          opacStr := GetAttr(pathElem, 'fill-opacity');
          if opacStr <> '' then
          begin
            opacity := SvgStrToFloatDef(opacStr, 1.0);
            svgColor.A := Byte(Round(svgColor.A * EnsureRange(opacity, 0, 1)));
          end;

          { opacity attribute (multiplies on top of fill-opacity) }
          opacStr := GetAttr(pathElem, 'opacity');
          if opacStr <> '' then
          begin
            opacity := SvgStrToFloatDef(opacStr, 1.0);
            svgColor.A := Byte(Round(svgColor.A * EnsureRange(opacity, 0, 1)));
          end;

          { Build the style record }
          FillChar(style, SizeOf(style), 0);
          style.StyleType := hstSolidColor;
          style.Color.R := svgColor.R;
          style.Color.G := svgColor.G;
          style.Color.B := svgColor.B;
          style.Color.A := svgColor.A;
          writer.AddStyle(style);

          { Parse the 'd' attribute }
          parser := TSvgPathParser.Create(dStr, scale, vbX, vbY);
          try
            paths := parser.Parse;
          finally
            parser.Free;
          end;

          if Length(paths) = 0 then
          begin
            { No valid paths — still consumed a style slot, skip shape }
            Inc(nextStyleIdx);
            Continue;
          end;

          { Add all sub-paths to the writer }
          for j := 0 to High(paths) do
            writer.AddPath(paths[j]);

          { Build a shape that references this style and all its sub-paths }
          FillChar(shape, SizeOf(shape), 0);
          shape.StyleIndex      := nextStyleIdx;
          shape.HasTransform    := False;
          shape.HasTranslation  := False;
          SetLength(shape.PathIndices, Length(paths));
          for j := 0 to High(paths) do
            shape.PathIndices[j] := nextPathIdx + j;
          writer.AddShape(shape);

          Inc(nextStyleIdx);
          Inc(nextPathIdx, Length(paths));
        end;

      finally
        pathList.Free;
      end;

      writer.SaveToFile(AHvifFile);
    finally
      writer.Free;
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
