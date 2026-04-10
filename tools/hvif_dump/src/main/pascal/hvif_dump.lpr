{
    hvif_dump — HVIF binary file inspector.

    This program is part of the fpGUI Toolkit project.

    Reads an HVIF file and prints a human-readable summary of its
    structure: magic, style section, path section, and shape section.
    Useful for debugging fpg_hvif.pas (reader), fpg_hvif_writer.pas,
    and the svg2hvif converter.

    Usage:
      hvif_dump <file.hvif>
      hvif_dump <file.hvif> --verbose

    Exit codes:
      0  Success
      1  File not found or parse error
}

program hvif_dump;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, Classes, Math,
  fpg_hvif;


{ =========================================================
  Low-level byte reader (mirrors THvifIcon internals so the
  dump works independently even if the reader has a bug)
  ========================================================= }

type
  TByteReader = class
  private
    FData: TBytes;
    FPos:  Integer;
  public
    constructor Create(const AData: TBytes);
    function  EOF: Boolean;
    function  Pos: Integer;
    function  ReadByte: Byte;
    function  ReadWord: Word;
    function  ReadCoord: Single;
    function  ReadFloat24: Single;
    procedure Skip(n: Integer);
  end;

constructor TByteReader.Create(const AData: TBytes);
begin
  FData := AData;
  FPos  := 0;
end;

function TByteReader.EOF: Boolean;
begin
  Result := FPos >= Length(FData);
end;

function TByteReader.Pos: Integer;
begin
  Result := FPos;
end;

function TByteReader.ReadByte: Byte;
begin
  if FPos >= Length(FData) then
    raise Exception.CreateFmt('Unexpected end of data at position %d', [FPos]);
  Result := FData[FPos];
  Inc(FPos);
end;

function TByteReader.ReadWord: Word;
var
  lo, hi: Byte;
begin
  lo := ReadByte;
  hi := ReadByte;
  Result := lo or (Word(hi) shl 8);
end;

function TByteReader.ReadCoord: Single;
var
  v, lo: Byte;
  coordValue: Word;
begin
  v := ReadByte;
  if (v and 128) <> 0 then
  begin
    lo := ReadByte;
    v  := v and 127;
    coordValue := (Word(v) shl 8) or lo;
    Result := coordValue / 102.0 - 128.0;
  end
  else
    Result := v - 32.0;
end;

function TByteReader.ReadFloat24: Single;
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
  b0 := ReadByte; b1 := ReadByte; b2 := ReadByte;
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

procedure TByteReader.Skip(n: Integer);
begin
  Inc(FPos, n);
end;


{ =========================================================
  Dump routines
  ========================================================= }

const
  STYLE_NAMES: array[1..5] of string = (
    'SOLID_COLOR', 'GRADIENT', 'SOLID_COLOR_NO_ALPHA',
    'SOLID_GRAY', 'SOLID_GRAY_NO_ALPHA');

  GRADIENT_NAMES: array[0..5] of string = (
    'LINEAR', 'CIRCULAR', 'DIAMOND', 'CONIC', 'XY', 'SQRT_XY');

  PATH_CMD_NAMES: array[0..3] of string = (
    'H_LINE', 'V_LINE', 'LINE', 'CURVE');

var
  GVerbose: Boolean = False;


procedure DumpStyles(r: TByteReader; count: Byte);
const
  GRADIENT_FLAG_TRANSFORM = 1 shl 1;
  GRADIENT_FLAG_NO_ALPHA  = 1 shl 2;
  GRADIENT_FLAG_GRAYS     = 1 shl 4;
var
  i, s: Integer;
  styleType, gradType, gradFlags, stopCount, stopOffset: Byte;
  hasAlpha, isGray: Boolean;
  tagLen: Word;
  r_, g_, b_, a_: Byte;
  f: Single;
begin
  WriteLn('--- Styles (', count, ') ---');
  for i := 0 to count - 1 do
  begin
    Write('  [', i, '] @', r.Pos, ' ');
    styleType := r.ReadByte;
    case styleType of
      1: begin
           WriteLn('SOLID_COLOR');
           r_ := r.ReadByte; g_ := r.ReadByte;
           b_ := r.ReadByte; a_ := r.ReadByte;
           WriteLn('       R=', r_, ' G=', g_, ' B=', b_, ' A=', a_,
                   '  (#', IntToHex(r_,2), IntToHex(g_,2), IntToHex(b_,2), ')');
         end;
      3: begin
           WriteLn('SOLID_COLOR_NO_ALPHA');
           r_ := r.ReadByte; g_ := r.ReadByte; b_ := r.ReadByte;
           WriteLn('       R=', r_, ' G=', g_, ' B=', b_,
                   '  (#', IntToHex(r_,2), IntToHex(g_,2), IntToHex(b_,2), ')');
         end;
      4: begin
           WriteLn('SOLID_GRAY');
           r_ := r.ReadByte; a_ := r.ReadByte;
           WriteLn('       K=', r_, ' A=', a_);
         end;
      5: begin
           WriteLn('SOLID_GRAY_NO_ALPHA');
           r_ := r.ReadByte;
           WriteLn('       K=', r_);
         end;
      2: begin
           WriteLn('GRADIENT');
           gradType  := r.ReadByte;
           gradFlags := r.ReadByte;
           stopCount := r.ReadByte;
           if gradType <= 5 then
             WriteLn('       type=', GRADIENT_NAMES[gradType])
           else
             WriteLn('       type=', gradType, ' (unknown)');
           Write('       flags=', IntToHex(gradFlags, 2));
           if (gradFlags and GRADIENT_FLAG_TRANSFORM) <> 0 then Write(' TRANSFORM');
           if (gradFlags and GRADIENT_FLAG_NO_ALPHA)  <> 0 then Write(' NO_ALPHA');
           if (gradFlags and GRADIENT_FLAG_GRAYS)     <> 0 then Write(' GRAYS');
           WriteLn;
           WriteLn('       stops=', stopCount);
           if (gradFlags and GRADIENT_FLAG_TRANSFORM) <> 0 then
           begin
             Write('       matrix=[');
             for s := 0 to 5 do
             begin
               f := r.ReadFloat24;
               if s > 0 then Write(', ');
               Write(Format('%.4f', [f]));
             end;
             WriteLn(']');
           end;
           hasAlpha := (gradFlags and GRADIENT_FLAG_NO_ALPHA) = 0;
           isGray   := (gradFlags and GRADIENT_FLAG_GRAYS) <> 0;
           for s := 0 to stopCount - 1 do
           begin
             stopOffset := r.ReadByte;
             Write('       stop[', s, '] offset=', Format('%.3f', [stopOffset/255.0]));
             if isGray then
             begin
               r_ := r.ReadByte;
               if hasAlpha then begin a_ := r.ReadByte; WriteLn(' K=', r_, ' A=', a_); end
               else WriteLn(' K=', r_);
             end
             else
             begin
               r_ := r.ReadByte; g_ := r.ReadByte; b_ := r.ReadByte;
               if hasAlpha then begin a_ := r.ReadByte; WriteLn(' RGBA=', r_,',',g_,',',b_,',',a_); end
               else WriteLn(' RGB=', r_,',',g_,',',b_);
             end;
           end;
         end;
      else begin
        Write('UNKNOWN (type=', styleType, ')');
        tagLen := r.ReadWord;
        WriteLn(' — skipping ', tagLen, ' bytes');
        r.Skip(tagLen);
      end;
    end;
  end;
end;


procedure DumpPaths(r: TByteReader; count: Byte);
const
  PATH_FLAG_CLOSED        = 1 shl 1;
  PATH_FLAG_USES_COMMANDS = 1 shl 2;
  PATH_FLAG_NO_CURVES     = 1 shl 3;
  PATH_COMMAND_H_LINE = 0;
  PATH_COMMAND_V_LINE = 1;
  PATH_COMMAND_LINE   = 2;
  PATH_COMMAND_CURVE  = 3;
var
  i, p: Integer;
  pathFlags, pointCount: Byte;
  cmdBufSize, cmdPos, cmdBufIdx: Integer;
  cmdBuf: array of Byte;
  cmdByte, cmd: Byte;
  x, y, ix, iy, ox, oy: Single;
  lx, ly: Single;
begin
  WriteLn('--- Paths (', count, ') ---');
  for i := 0 to count - 1 do
  begin
    pathFlags  := r.ReadByte;
    pointCount := r.ReadByte;
    Write('  [', i, '] @', r.Pos - 2, ' points=', pointCount);
    Write(' flags=', IntToHex(pathFlags, 2));
    if (pathFlags and PATH_FLAG_CLOSED) <> 0        then Write(' CLOSED');
    if (pathFlags and PATH_FLAG_NO_CURVES) <> 0     then Write(' NO_CURVES');
    if (pathFlags and PATH_FLAG_USES_COMMANDS) <> 0 then Write(' COMMANDS');
    WriteLn;
    if pointCount = 0 then Continue;

    if (pathFlags and PATH_FLAG_NO_CURVES) <> 0 then
    begin
      if GVerbose then
        for p := 0 to pointCount - 1 do
        begin
          x := r.ReadCoord; y := r.ReadCoord;
          WriteLn('       pt[', p, ']  (', Format('%.2f', [x]), ', ', Format('%.2f', [y]), ')');
        end
      else
      begin
        for p := 0 to pointCount - 1 do
          begin r.ReadCoord; r.ReadCoord; end;
      end;
    end
    else if (pathFlags and PATH_FLAG_USES_COMMANDS) <> 0 then
    begin
      cmdBufSize := (pointCount + 3) div 4;
      SetLength(cmdBuf, cmdBufSize);
      for p := 0 to cmdBufSize - 1 do
        cmdBuf[p] := r.ReadByte;
      cmdByte := 0; cmdPos := 0; cmdBufIdx := 0;
      lx := 0; ly := 0;
      for p := 0 to pointCount - 1 do
      begin
        if cmdPos = 0 then
        begin
          cmdByte := cmdBuf[cmdBufIdx]; Inc(cmdBufIdx);
        end;
        cmd := (cmdByte shr cmdPos) and $03;
        Inc(cmdPos, 2);
        if cmdPos = 8 then cmdPos := 0;
        if GVerbose then
          Write('       pt[', p, '] ', PATH_CMD_NAMES[cmd]);
        case cmd of
          PATH_COMMAND_H_LINE: begin x := r.ReadCoord; y := ly; end;
          PATH_COMMAND_V_LINE: begin x := lx;          y := r.ReadCoord; end;
          PATH_COMMAND_LINE:   begin x := r.ReadCoord; y := r.ReadCoord; end;
          PATH_COMMAND_CURVE:
            begin
              x := r.ReadCoord; y := r.ReadCoord;
              ix:= r.ReadCoord; iy:= r.ReadCoord;
              ox:= r.ReadCoord; oy:= r.ReadCoord;
              if GVerbose then
                Write(' in=(', Format('%.2f',[ix]),',', Format('%.2f',[iy]),
                      ') out=(', Format('%.2f',[ox]),',', Format('%.2f',[oy]), ')');
            end;
          else begin x := 0; y := 0; end;
        end;
        if GVerbose then
          WriteLn('  (', Format('%.2f',[x]),', ', Format('%.2f',[y]),')');
        lx := x; ly := y;
      end;
    end
    else
    begin
      if GVerbose then
        for p := 0 to pointCount - 1 do
        begin
          x  := r.ReadCoord; y  := r.ReadCoord;
          ix := r.ReadCoord; iy := r.ReadCoord;
          ox := r.ReadCoord; oy := r.ReadCoord;
          WriteLn('       pt[', p, ']  pos=(', Format('%.2f',[x]),',', Format('%.2f',[y]),
                  ')  in=(', Format('%.2f',[ix]),',', Format('%.2f',[iy]),
                  ')  out=(', Format('%.2f',[ox]),',', Format('%.2f',[oy]),')');
        end
      else
        for p := 0 to pointCount - 1 do
        begin
          r.ReadCoord; r.ReadCoord;
          r.ReadCoord; r.ReadCoord;
          r.ReadCoord; r.ReadCoord;
        end;
    end;
  end;
end;


procedure DumpShapes(r: TByteReader; count: Byte);
const
  SHAPE_TYPE_PATH_SOURCE   = 10;
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
  i, p, t: Integer;
  shapeType, shapeFlags, sIdx, nPaths, pIdx: Byte;
  tagLen: Word;
  transformerCount, ttype: Byte;
  f: Single;
  tx, ty: Single;
begin
  WriteLn('--- Shapes (', count, ') ---');
  for i := 0 to count - 1 do
  begin
    Write('  [', i, '] @', r.Pos, ' ');
    shapeType := r.ReadByte;
    if shapeType <> SHAPE_TYPE_PATH_SOURCE then
    begin
      tagLen := r.ReadWord;
      WriteLn('type=', shapeType, ' (unknown) — skipping ', tagLen, ' bytes');
      r.Skip(tagLen);
      Continue;
    end;

    sIdx   := r.ReadByte;
    nPaths := r.ReadByte;
    Write('PATH_SOURCE  style=', sIdx, '  paths=[');
    for p := 0 to nPaths - 1 do
    begin
      pIdx := r.ReadByte;
      if p > 0 then Write(',');
      Write(pIdx);
    end;
    shapeFlags := r.ReadByte;
    Write(']  flags=', IntToHex(shapeFlags, 2));
    if (shapeFlags and SHAPE_FLAG_HINTING)    <> 0 then Write(' HINTING');
    if (shapeFlags and SHAPE_FLAG_LOD_SCALE)  <> 0 then Write(' LOD');
    WriteLn;

    if (shapeFlags and SHAPE_FLAG_TRANSFORM) <> 0 then
    begin
      Write('       transform=[');
      for t := 0 to 5 do
      begin
        f := r.ReadFloat24;
        if t > 0 then Write(', ');
        Write(Format('%.4f', [f]));
      end;
      WriteLn(']');
    end
    else if (shapeFlags and SHAPE_FLAG_TRANSLATION) <> 0 then
    begin
      tx := r.ReadCoord; ty := r.ReadCoord;
      WriteLn('       translate=(', Format('%.2f',[tx]),', ', Format('%.2f',[ty]),')');
    end;

    if (shapeFlags and SHAPE_FLAG_LOD_SCALE) <> 0 then
    begin
      Write('       lod-min=', r.ReadByte/63.75:5:2,
            '  lod-max=', r.ReadByte/63.75:5:2);
      WriteLn;
    end;

    if (shapeFlags and SHAPE_FLAG_HAS_TRANSFORMERS) <> 0 then
    begin
      transformerCount := r.ReadByte;
      WriteLn('       transformers=', transformerCount);
      for t := 0 to transformerCount - 1 do
      begin
        ttype := r.ReadByte;
        case ttype of
          TRANSFORMER_TYPE_AFFINE:
            begin
              Write('         [', t, '] AFFINE [');
              for p := 0 to 5 do
              begin
                f := r.ReadFloat24;
                if p > 0 then Write(', ');
                Write(Format('%.4f', [f]));
              end;
              WriteLn(']');
            end;
          TRANSFORMER_TYPE_CONTOUR:
            begin
              p := r.ReadByte; { width }
              WriteLn('         [', t, '] CONTOUR  width=', Integer(p)-128,
                      '  join=', r.ReadByte,
                      '  miter=', r.ReadByte);
            end;
          TRANSFORMER_TYPE_PERSPECTIVE:
            begin
              Write('         [', t, '] PERSPECTIVE [');
              for p := 0 to 8 do
              begin
                f := r.ReadFloat24;
                if p > 0 then Write(', ');
                Write(Format('%.4f', [f]));
              end;
              WriteLn(']');
            end;
          TRANSFORMER_TYPE_STROKE:
            begin
              p := r.ReadByte; { width }
              WriteLn('         [', t, '] STROKE  width=', Integer(p)-128,
                      '  options=', IntToHex(r.ReadByte, 2),
                      '  miter=', r.ReadByte);
            end;
          else
          begin
            tagLen := r.ReadWord;
            WriteLn('         [', t, '] type=', ttype, ' (unknown) — skipping ', tagLen, ' bytes');
            r.Skip(tagLen);
          end;
        end;
      end;
    end;
  end;
end;


{ =========================================================
  Entry point
  ========================================================= }

var
  fileName: string;
  data: TBytes;
  sz: Int64;
  fs: TFileStream;
  r: TByteReader;
  magic: array[0..3] of Byte;
  nStyles, nPaths, nShapes: Byte;
  argIdx: Integer;

begin
  GVerbose := False;
  fileName  := '';

  if ParamCount < 1 then
  begin
    WriteLn('Usage: hvif_dump <file.hvif> [--verbose]');
    WriteLn;
    WriteLn('Prints the structure of a Haiku Vector Icon Format (HVIF) file.');
    WriteLn('Use --verbose to also print individual path point coordinates.');
    ExitCode := 1;
    Exit;
  end;

  for argIdx := 1 to ParamCount do
    if ParamStr(argIdx) = '--verbose' then GVerbose := True
    else if fileName = '' then fileName := ParamStr(argIdx);

  if not FileExists(fileName) then
  begin
    WriteLn('Error: file not found: ', fileName);
    ExitCode := 1;
    Exit;
  end;

  try
    fs := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
    try
      sz := fs.Size;
      SetLength(data, sz);
      fs.ReadBuffer(data[0], sz);
    finally
      fs.Free;
    end;

    WriteLn('File:  ', fileName);
    WriteLn('Size:  ', sz, ' bytes');
    WriteLn;

    r := TByteReader.Create(data);
    try
      { Magic }
      magic[0] := r.ReadByte; magic[1] := r.ReadByte;
      magic[2] := r.ReadByte; magic[3] := r.ReadByte;
      Write('Magic: ');
      if (magic[0] = $6E) and (magic[1] = $63) and
         (magic[2] = $69) and (magic[3] = $66) then
        WriteLn('6E 63 69 66  (valid HVIF "ficn")')
      else
      begin
        WriteLn(IntToHex(magic[0],2),' ',IntToHex(magic[1],2),' ',
                IntToHex(magic[2],2),' ',IntToHex(magic[3],2),
                '  *** NOT VALID HVIF ***');
        ExitCode := 1;
        Exit;
      end;
      WriteLn;

      nStyles := r.ReadByte;
      DumpStyles(r, nStyles);
      WriteLn;

      nPaths := r.ReadByte;
      DumpPaths(r, nPaths);
      WriteLn;

      nShapes := r.ReadByte;
      DumpShapes(r, nShapes);
      WriteLn;

      if not r.EOF then
        WriteLn('WARNING: ', Length(data) - r.Pos, ' trailing bytes not consumed')
      else
        WriteLn('OK — all bytes consumed');

    finally
      r.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
