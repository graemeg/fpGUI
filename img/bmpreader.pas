{
    fpGUI  -  Free Pascal GUI Library
    
    Image reader for BMP files
    
    Copyright (C) 2000 - 2006 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit BMPReader;

{$IFDEF Debug}
{$ASSERTIONS On}
{$ENDIF}

interface

uses
  Classes
  ,GFXBase
  ,ImageIO
  ;

type
  DWORD = LongWord;
  LONG = LongInt;


  TBitmapFileHeader = packed record
    bfType: WORD;
    bfSize: DWORD;
    bfReserved1: WORD;
    bfReserved2: WORD;
    bfOffBits: DWORD;
  end;


  TBitmapInfoHeader = packed record
    biSize: DWORD;
    biWidth: LONG;
    biHeight: LONG;
    biPlanes: WORD;
    biBitCount: WORD;
    biCompression: DWORD;
    biSizeImage: DWORD;
    biXPelsPerMeter: LONG;
    biYPelsPerMeter: LONG;
    biClrUsed: DWORD;
    biClrImportant: DWORD;
  end;


  PRGBQuad = ^TRGBQuad;
  TRGBQuad = packed record
    rgbBlue, rgbGreen, rgbRed, rgbReserved: BYTE;
  end;


  TBMPReader = class(TImageReader)
  protected
    FFileHeader: TBitmapFileHeader;
    FInfoHeader: TBitmapInfoHeader;
    FBMPPalette: PRGBQuad;
    FFileStride: LongWord;
    HeaderBytesRead, PalBytesRead: Integer;
    ScanlinesLeft: Integer;
    ThisSegmentHeight: Integer;
    ScanlinesLeftInSegment: Integer;
    ScanlineBytesDone: LongWord;
    CurScanline: Pointer;
    procedure   DoProcessHeaderData(AStream: TStream); override;
    function    DoGetImageSegmentStartY(ASegmentHeight: Integer): Integer; override;
    procedure   InitImageReading; override;
    procedure   InitSegmentReading;
    procedure   DoProcessImageData(AStream: TStream); override;
  public
    destructor  Destroy; override;
    property    FileHeader: TBitmapFileHeader read FFileHeader;
    property    InfoHeader: TBitmapInfoHeader read FInfoHeader;
    property    BMPPalette: PRGBQuad read FBMPPalette;
    property    FileStride: LongWord read FFileStride;
  end;


implementation


destructor TBMPReader.Destroy;
begin
  if Assigned(Palette) then
    FreeMem(FPalette);
  if Assigned(BMPPalette) then
    FreeMem(FBMPPalette);
  inherited Destroy;
end;

procedure TBMPReader.DoProcessHeaderData(AStream: TStream);
var
  DataOffset: LongWord;
  HaveRead, BytesToSkip, i: Integer;
  IsFirstRead: Boolean;
  SkipBuffer: array[0..1023] of Byte;
begin
  if HeaderBytesRead < SizeOf(FileHeader) then
  begin
    HaveRead := AStream.Read(PChar(@FileHeader)[HeaderBytesRead],
      SizeOf(FileHeader) - HeaderBytesRead);
    if HaveRead = 0 then
      raise EImgOutOfData.Create;
    Inc(HeaderBytesRead, HaveRead);
    IsFirstRead := False;
  end
  else
    IsFirstRead := True;

  if HeaderBytesRead < SizeOf(FileHeader) + SizeOf(InfoHeader) then
  begin
    HaveRead := AStream.Read(
      PChar(@InfoHeader)[HeaderBytesRead - SizeOf(FileHeader)],
      SizeOf(FileHeader) + SizeOf(InfoHeader) - HeaderBytesRead);
    if HaveRead = 0 then
      if IsFirstRead then
        raise EImgOutOfData.Create
      else
        exit;
    IsFirstRead := False;
    Inc(HeaderBytesRead, HaveRead);
  end;

  if HeaderBytesRead = SizeOf(FileHeader) + SizeOf(InfoHeader) then
  begin
    case InfoHeader.biBitCount of
      1: FPaletteSize := 2;
      4: FPaletteSize := 16;
      8: FPaletteSize := 256;
    end;
    if PaletteSize > 0 then
    begin
      GetMem(FBMPPalette, PaletteSize * SizeOf(TRGBQuad));
      GetMem(FPalette, PaletteSize * SizeOf(TGfxColor));
    end;
  end;

  if HeaderBytesRead >= SizeOf(FileHeader) + SizeOf(InfoHeader) then
  begin
    DataOffset := FileHeader.bfOffBits;
    if HeaderBytesRead < DataOffset then
    begin
      BytesToSkip := DataOffset - HeaderBytesRead;
      if BytesToSkip > SizeOf(SkipBuffer) then
        BytesToSkip := SizeOf(SkipBuffer);
      HaveRead := AStream.Read(SkipBuffer, BytesToSkip);
      if HaveRead = 0 then
	      if IsFirstRead then
	        raise EImgOutOfData.Create
	      else
	        exit; //==>
      IsFirstRead := False;
      Inc(HeaderBytesRead, HaveRead);
      if PalBytesRead < PaletteSize * SizeOf(TGfxPixel) then
      begin
        Move(SkipBuffer, PByte(FBMPPalette)[PalBytesRead], HaveRead);
        Inc(PalBytesRead, HaveRead);
      end;
    end;  { if }

    if HeaderBytesRead = DataOffset then
    begin
      FWidth := InfoHeader.biWidth;
      FHeight := InfoHeader.biHeight;

      if PaletteSize > 0 then
        for i := 0 to PaletteSize - 1 do
      	begin
      	  Palette[i].Red := BMPPalette[i].rgbRed * 257;
      	  Palette[i].Green := BMPPalette[i].rgbGreen * 257;
      	  Palette[i].Blue := BMPPalette[i].rgbBlue * 257;
      	  Palette[i].Alpha := 0;
      	end;

      case InfoHeader.biBitCount of
      	1:
          begin
      	    FFileStride := ((Width + 31) shr 3) and not 3;
      	    FPixelFormat.FormatType := ftMono;
      	  end;
      	4:
          begin
      	    FFileStride := ((Width + 7) shr 1) and not 3;
      	    FPixelFormat.FormatType := ftPal4;
      	  end;
      	8:
          begin
      	    FFileStride := (Width + 3) and not 3;
      	    FPixelFormat.FormatType := ftPal8;
      	  end;
        24:
          begin
            FFileStride := (Width * 3 + 3) and not 3;
      	    FPixelFormat := PixelFormatBGR24;
          end;
        else
          raise EImgUnsupportedPixelFormat.Create;
      end;  { case }
    end;  { if }

    HeaderFinished;
  end;  { if }
end;

function TBMPReader.DoGetImageSegmentStartY(ASegmentHeight: Integer): Integer;
begin
  Result := ScanlinesLeft - ASegmentHeight;
  if Result < 0 then
    Result := 0;
end;

procedure TBMPReader.InitImageReading;
begin
  ScanlinesLeft := Height;
  InitSegmentReading;
end;

procedure TBMPReader.InitSegmentReading;
begin
  ThisSegmentHeight := ScanlinesLeft;
  if ThisSegmentHeight > SegmentHeight then
    ThisSegmentHeight := SegmentHeight;
  ScanlinesLeftInSegment := ThisSegmentHeight;
  ScanlineBytesDone := 0;
  CurScanline := SegmentData + (ThisSegmentHeight - 1) * SegmentStride;
end;

procedure TBMPReader.DoProcessImageData(AStream: TStream);

  procedure ScanlineDone;
  begin
    Dec(ScanlinesLeftInSegment);
    Dec(ScanlinesLeft);

    if ScanlinesLeftInSegment = 0 then
    begin
      SegmentFinished(ScanlinesLeft, ThisSegmentHeight);
      if ScanlinesLeft = 0 then
        ImageFinished
      else
        InitSegmentReading;
    end
    else
      Dec(CurScanline, SegmentStride);
  end;

var
  ReadMayFail: Boolean;
  ToRead, HaveRead: Integer;
begin
  if ScanlineBytesDone > 0 then
  begin
    ToRead := SegmentStride;
    if ToRead > FileStride then
      ToRead := FileStride;
    Dec(ToRead, ScanlineBytesDone);
    HaveRead := AStream.Read(PChar(CurScanline)[ScanlineBytesDone], ToRead);
    if HaveRead = 0 then
      raise EImgOutOfData.Create;
    if HaveRead = ToRead then
    begin
      ScanlineBytesDone := 0;
      ScanlineDone
    end
    else
    begin
      Inc(ScanlineBytesDone, HaveRead);
      exit;
    end;
    ReadMayFail := True;
  end
  else
    ReadMayFail := False;

  while ScanlinesLeft > 0 do
  begin
    ToRead := SegmentStride;
    if ToRead > FileStride then
      ToRead := FileStride;

    HaveRead := AStream.Read(CurScanline^, ToRead);

    if HaveRead = 0 then
      if ReadMayFail then
        exit  //==>
      else
        raise EImgOutOfData.Create;

    if HaveRead < ToRead then
    begin
      ScanlineBytesDone := HaveRead;
      break;
    end;

    // Handle the ordinary case: a full scanline has been read
    if ToRead < FileStride then
      AStream.Position := AStream.Position + FileStride - ToRead;
    ReadMayFail := True;
    ScanlineDone;
  end;  { while }
end;

end.
