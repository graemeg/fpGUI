{
    fpGUI  -  Free Pascal Graphical User Interface

    GelImage  -  X11 image conversion routines

    Copyright (C) 2000 - 2006 See the file AUTHORS, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit GELImage;

{$IFDEF Debug}
  {$ASSERTIONS On}
{$ENDIF}

{$mode objfpc}{$H+}

interface

uses
  Classes,
  GfxBase;


procedure ConvertImage(
  ASourceRect: TRect; ASourceFormat: TGfxPixelFormat; ASourcePal: TGfxPalette;
  ASourceData: Pointer; ASourceStride: LongWord;
  ADestX, ADestY: Integer; ADestFormat: TGfxPixelFormat;
  ADestData: Pointer; ADestStride: LongWord);

procedure FlipMonoImageBits(
  ASourceRect: TRect; ASourceData: Pointer; ASourceStride: LongWord;
  ADestX, ADestY: Integer; ADestData: Pointer; ADestStride: LongWord);


implementation

type
  TConvertParams = record
    RedShiftR, RedShiftL,
      GreenShiftR, GreenShiftL,
      BlueShiftR, BlueShiftL: Byte;
    RedMask, GreenMask, BlueMask: LongWord;
    RedMult, GreenMult, BlueMult: Word;
    Palette: array[0..255] of LongWord;
  end;

  TConvertToInternalProc = procedure(Params: TConvertParams; Data: Pointer;
    StartX, EndX: Integer; Dest: Pointer);

  TConvertFromInternalProc = procedure(Params: TConvertParams; Data: Pointer;
    Dest: Pointer; Width: Integer);


procedure ConvertMonoToInternal(Params: TConvertParams; Data: Pointer;
  StartX, EndX: Integer; Dest: Pointer);
var
  Mask: Byte;
begin
  Inc(Data, StartX shr 3);
  Mask := 1 shl (7 - StartX and 7);
  while StartX < EndX do
  begin
    PLongWord(Dest)^ := Params.Palette[Ord((PByte(Data)^ and Mask) <> 0)];
    if Mask = 1 then
    begin
      Inc(Data);
      Mask := 128;
    end else
      Mask := Mask shr 1;
    Inc(StartX);
    Inc(Dest, 4);
  end;
end;

procedure ConvertPal4ToInternal(Params: TConvertParams; Data: Pointer;
  StartX, EndX: Integer; Dest: Pointer);
var
  b: Byte;
begin
  // !!!: Just works for even StartX and EndX values
  ASSERT((StartX and 1) = 0);
  ASSERT((EndX and 1) = 0);
  Inc(Data, StartX shr 1);
  while StartX < EndX do
  begin
    b := PByte(Data)^;
    PLongWord(Dest)[0] := Params.Palette[b shr 4];
    PLongWord(Dest)[1] := Params.Palette[b and 15];
    Inc(StartX, 2);
    Inc(Data);
    Inc(Dest, 8);
   end;
end;

procedure ConvertPal8ToInternal(Params: TConvertParams; Data: Pointer;
  StartX, EndX: Integer; Dest: Pointer);
begin
  Inc(Data, StartX);
  while StartX < EndX do
  begin
    PLongWord(Dest)^ := Params.Palette[PByte(Data)^];
    Inc(StartX);
    Inc(Data);
    Inc(Dest, 4);
  end;
end;

procedure ConvertRGB24ToInternal(Params: TConvertParams; Data: Pointer;
  StartX, EndX: Integer; Dest: Pointer);
var
  PixelIn: LongWord;
begin
  Inc(Data, StartX * 3);
  while StartX < EndX do
  begin
    PixelIn := 0;
    Move(Data^, PixelIn, 3);
    PLongWord(Dest)^ :=
      (((PixelIn shr Params.RedShiftR) and $ff) shl Params.RedShiftL) or
      (((PixelIn shr Params.GreenShiftR) and $ff) shl Params.GreenShiftL) or
      (((PixelIn shr Params.BlueShiftR) and $ff) shl Params.BlueShiftL);
    Inc(StartX);
    Inc(Data, 3);
    Inc(Dest, 4);
  end;
end;

procedure ConvertRGB32ToInternal(Params: TConvertParams; Data: Pointer;
  StartX, EndX: Integer; Dest: Pointer);
var
  PixelIn: LongWord;
begin
  Inc(Data, StartX * 4);
  while StartX < EndX do
  begin
    PixelIn := PLongWord(Data)^;
    PLongWord(Dest)^ :=
      (((PixelIn shr Params.RedShiftR) and $ff) shl Params.RedShiftL) or
      (((PixelIn shr Params.GreenShiftR) and $ff) shl Params.GreenShiftL) or
      (((PixelIn shr Params.BlueShiftR) and $ff) shl Params.BlueShiftL);
    Inc(StartX);
    Inc(Data, 4);
    Inc(Dest, 4);
  end;
end;


procedure ConvertInternalToRGB16(Params: TConvertParams; Data: Pointer;
  Dest: Pointer; Width: Integer);
var
  PixelIn: LongWord;
begin
  repeat
    PixelIn := PLongWord(Data)^;
    PWord(Dest)^ :=
      (((PixelIn and $0000ff) shr Params.RedShiftR) shl Params.RedShiftL) or
      (((PixelIn and $00ff00) shr Params.GreenShiftR) shl Params.GreenShiftL) or
      (((PixelIn and $ff0000) shr Params.BlueShiftR) shl Params.BlueShiftL);

    Inc(Data, 4);
    Inc(Dest, 2);
    Dec(Width);
  until Width = 0;
end;

procedure ConvertInternalToRGB24(Params: TConvertParams; Data: Pointer;
  Dest: Pointer; Width: Integer);
var
  PixelIn, PixelOut: LongWord;
begin
  repeat
    PixelIn := PLongWord(Data)^;
    PixelOut :=
      (((PixelIn and $0000ff) shr Params.RedShiftR) shl Params.RedShiftL) or
      (((PixelIn and $00ff00) shr Params.GreenShiftR) shl Params.GreenShiftL) or
      (((PixelIn and $ff0000) shr Params.BlueShiftR) shl Params.BlueShiftL);
    PWord(Dest)^ := Word(PixelOut);
    PByte(Dest)[2] := PixelOut shr 16;

    Inc(Data, 4);
    Inc(Dest, 3);
    Dec(Width);
  until Width = 0;
end;

procedure ConvertInternalToRGB32(Params: TConvertParams; Data: Pointer;
  Dest: Pointer; Width: Integer);
var
  PixelIn: LongWord;
begin
  repeat
    PixelIn := PLongWord(Data)^;
    PLongWord(Dest)^ :=
      (((PixelIn and $0000ff) shr Params.RedShiftR) shl Params.RedShiftL) or
      (((PixelIn and $00ff00) shr Params.GreenShiftR) shl Params.GreenShiftL) or
      (((PixelIn and $ff0000) shr Params.BlueShiftR) shl Params.BlueShiftL);

    Inc(Data, 4);
    Inc(Dest, 4);
    Dec(Width);
  until Width = 0;
end;


function GetBitShiftAndCount(Mask: LongWord; var Shift: Byte): Integer;
begin
  Shift := 0;
  while (Mask and 1) = 0 do
  begin
    Mask := Mask shr 1;
    Inc(Shift);
  end;
  Result := 0;
  while Mask > 0 do
  begin
    Mask := Mask shr 1;
    Inc(Result);
  end;
end;

procedure SetupShifts(PixelFormat: TGfxPixelFormat; var Params: TConvertParams);
begin
  Params.RedShiftR := 8 -
    GetBitShiftAndCount(PixelFormat.RedMask, Params.RedShiftL);
  Params.GreenShiftR := 16 -
    GetBitShiftAndCount(PixelFormat.GreenMask, Params.GreenShiftL);
  Params.BlueShiftR := 24 -
    GetBitShiftAndCount(PixelFormat.BlueMask, Params.BlueShiftL);
end;

procedure ConvertImage(
  ASourceRect: TRect; ASourceFormat: TGfxPixelFormat; ASourcePal: TGfxPalette;
  ASourceData: Pointer; ASourceStride: LongWord;
  ADestX, ADestY: Integer; ADestFormat: TGfxPixelFormat;
  ADestData: Pointer; ADestStride: LongWord);

  // returns the highest processed index
  function ConvertPalette(MaxIndex: Integer;
    var Params: TConvertParams): Integer;
  var
    i: Integer;
  begin
    Assert(MaxIndex = MaxIndex);  // removes compiler warning
    if Assigned(ASourcePal) then
    begin
      Result := ASourcePal.EntryCount - 1;
      if Result > 255 then
        Result := 255;
      for i := 0 to Result do
        with ASourcePal.Entries[i] do
          Params.Palette[i] :=
            (Red div 257) or
    	    ((Green div 257) shl 8) or
    	    ((Blue div 257) shl 16);
    end else
      Result := -1;
  end;

var
  ParamsS2I, ParamsI2D: TConvertParams;  // Source to internal, internal to dest
  ConvertToInternal: TConvertToInternalProc;
  ConvertFromInternal: TConvertFromInternalProc;
  Scanline: Pointer;
  i, max, w, y: Integer;
begin
  Assert(ADestX = ADestX);  // removes compiler warning
  Assert(ADestY = AdestY);
  Scanline := nil;
  ParamsI2D.BlueShiftL := 0;
  ParamsS2I.BlueShiftL := 0;
  
  case ASourceFormat.FormatType of
    ftMono:
      begin
        ConvertToInternal := @ConvertMonoToInternal;
	max := ConvertPalette(1, ParamsS2I);
	if max < 1 then
	begin
	  ParamsS2I.Palette[1] := $ffffff;
	  if max < 0 then
	    ParamsS2I.Palette[0] := 0;
	end;
      end;
     ftPal4, ftPal4A:
       begin
         ConvertToInternal := @ConvertPal4ToInternal;
         max := ConvertPalette(15, ParamsS2I);
         for i := max + 1 to 15 do
           ParamsS2I.Palette[i] := 0;
       end;
    ftPal8, ftPal8A:
      begin
        ConvertToInternal := @ConvertPal8ToInternal;
	max := ConvertPalette(255, ParamsS2I);
	for i := max + 1 to 255 do
	  ParamsS2I.Palette[i] := i or (i shl 8) or (i shl 16);
      end;
     ftRGB24:
       begin
         ConvertToInternal := @ConvertRGB24ToInternal;
         ParamsS2I.RedShiftR := 8 -
           GetBitShiftAndCount(ASourceFormat.RedMask, ParamsS2I.RedShiftL);
         ParamsS2I.GreenShiftR := 16 -
           GetBitShiftAndCount(ASourceFormat.GreenMask, ParamsS2I.GreenShiftL);
         ParamsS2I.BlueShiftR := 24 -
           GetBitShiftAndCount(ASourceFormat.BlueMask, ParamsS2I.BlueShiftL);
       end;
    ftRGB32:
      begin
        ConvertToInternal := @ConvertRGB32ToInternal;
	ParamsS2I.RedShiftR := 8 -
	  GetBitShiftAndCount(ASourceFormat.RedMask, ParamsS2I.RedShiftL);
	ParamsS2I.GreenShiftR := 16 -
	  GetBitShiftAndCount(ASourceFormat.GreenMask, ParamsS2I.GreenShiftL);
	ParamsS2I.BlueShiftR := 24 -
	  GetBitShiftAndCount(ASourceFormat.BlueMask, ParamsS2I.BlueShiftL);
      end;
    else
      raise EGfxUnsupportedPixelFormat.Create(ASourceFormat);
  end;

  case ADestFormat.FormatType of
    ftPal8, ftPal8A:
      begin
//        ConvertFromInternal := @ConvertInternalTo8Pal;
//	SetupShifts(ADestFormat, ParamsI2D);
      end;
    ftRGB16:
      begin
        ConvertFromInternal := @ConvertInternalToRGB16;
	SetupShifts(ADestFormat, ParamsI2D);
      end;
    ftRGB24:
      begin
        ConvertFromInternal := @ConvertInternalToRGB24;
	SetupShifts(ADestFormat, ParamsI2D);
      end;
    ftRGB32:
      begin
        ConvertFromInternal := @ConvertInternalToRGB32;
	SetupShifts(ADestFormat, ParamsI2D);
      end;
    else
      raise EGfxUnsupportedPixelFormat.Create(ADestFormat);
  end;

  w := ASourceRect.Right - ASourceRect.Left;
  GetMem(Scanline, w * SizeOf(TGfxPixel));
  for y := ASourceRect.Top to ASourceRect.Bottom - 1 do
  begin
    ConvertToInternal(ParamsS2I, ASourceData,
      ASourceRect.Left, ASourceRect.Right, Scanline);
    Inc(ASourceData, ASourceStride);
    ConvertFromInternal(ParamsI2D, Scanline, ADestData, w);
    Inc(ADestData, ADestStride);
  end;
  FreeMem(Scanline);
end;


procedure FlipMonoImageBits(
  ASourceRect: TRect; ASourceData: Pointer; ASourceStride: LongWord;
  ADestX, ADestY: Integer; ADestData: Pointer; ADestStride: LongWord);
const
  BitFlipTable: array[Byte] of Byte = (
    $00, $80, $40, $C0, $20, $A0, $60, $E0, $10, $90, $50, $D0, $30, $B0, $70, $F0,
    $08, $88, $48, $C8, $28, $A8, $68, $E8, $18, $98, $58, $D8, $38, $B8, $78, $F8,
    $04, $84, $44, $C4, $24, $A4, $64, $E4, $14, $94, $54, $D4, $34, $B4, $74, $F4,
    $0C, $8C, $4C, $CC, $2C, $AC, $6C, $EC, $1C, $9C, $5C, $DC, $3C, $BC, $7C, $FC,
    $02, $82, $42, $C2, $22, $A2, $62, $E2, $12, $92, $52, $D2, $32, $B2, $72, $F2,
    $0A, $8A, $4A, $CA, $2A, $AA, $6A, $EA, $1A, $9A, $5A, $DA, $3A, $BA, $7A, $FA,
    $06, $86, $46, $C6, $26, $A6, $66, $E6, $16, $96, $56, $D6, $36, $B6, $76, $F6,
    $0E, $8E, $4E, $CE, $2E, $AE, $6E, $EE, $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE,
    $01, $81, $41, $C1, $21, $A1, $61, $E1, $11, $91, $51, $D1, $31, $B1, $71, $F1,
    $09, $89, $49, $C9, $29, $A9, $69, $E9, $19, $99, $59, $D9, $39, $B9, $79, $F9,
    $05, $85, $45, $C5, $25, $A5, $65, $E5, $15, $95, $55, $D5, $35, $B5, $75, $F5,
    $0D, $8D, $4D, $CD, $2D, $AD, $6D, $ED, $1D, $9D, $5D, $DD, $3D, $BD, $7D, $FD,
    $03, $83, $43, $C3, $23, $A3, $63, $E3, $13, $93, $53, $D3, $33, $B3, $73, $F3,
    $0B, $8B, $4B, $CB, $2B, $AB, $6B, $EB, $1B, $9B, $5B, $DB, $3B, $BB, $7B, $FB,
    $07, $87, $47, $C7, $27, $A7, $67, $E7, $17, $97, $57, $D7, $37, $B7, $77, $F7,
    $0F, $8F, $4F, $CF, $2F, $AF, $6F, $EF, $1F, $9F, $5F, $DF, $3F, $BF, $7F, $FF);
var
  StartByte, EndByte, x, y: Integer;
begin
  Assert(ADestX = ADestX);  // removes compiler warning
  Assert(ADestY = AdestY);

  StartByte := ASourceRect.Left shr 3;
  EndByte := (ASourceRect.Right + 7) shr 3;
  Inc(ASourceData, StartByte);
  Dec(EndByte, StartByte);
  for y := ASourceRect.Top to ASourceRect.Bottom - 1 do
  begin
    for x := 0 to EndByte do
      PByte(ADestData)[x] := BitFlipTable[PByte(ASourceData)[x]];
    Inc(ASourceData, ASourceStride);
    Inc(ADestData, ADestStride);
  end;
end;

end.


