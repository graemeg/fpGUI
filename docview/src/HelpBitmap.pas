{
  fpGUI  -  Free Pascal GUI Toolkit

  Copyright (C) 2006 - 2013 See the file AUTHORS.txt, included in this
  distribution, for details of the copyright.

  See the file COPYING.modifiedLGPL, included in this distribution,
  for details about redistributing fpGUI.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  Description:
    Encapsulates a bitmap as stored in a IPF file. Once created from
    file data they can be used as a normal bitmap.
}

unit HelpBitmap;

{$mode objfpc}{$H+}

// Debug purposes only
{.$define LZW_DEBUG}

interface

uses
  Classes, SysUtils, fpg_main,
  IPFFileFormatUnit;

type
  EHelpBitmapException = class( Exception );


  // Lead part of BITMAPARRAYFILEHEADER
  INFBITMAPARRAYHEADER = packed record
    usType: uint16;    // 'BA', 16706
    cbSize: uint32;     // Size of the BITMAPARRAYFILEHEADER structure in bytes.
    offNext: uint32;    // Offset of the next BITMAPARRAYFILEHEADER structure from the start of the file.
    cxDisplay: uint16; // Device width, in pels.
    cyDisplay: uint16; // Device height, in pels.
  end;


  INFBITMAPHEADER = packed record
    // BITMAP FILE HEADER
    usType: uint16; // = 'bM'
    cbSize: uint32;
    xHotspot: uint16;
    yHotspot: uint16;
    offBits: uint32; // =size(hdr)+size(palette)
    // BITMAP INFO HEADER
    cbFIx: uint32; // =size(info_hdr) (usually = 12?)
    cx: uint16; // width size
    cy: uint16; // height size
    cPlanes: uint16; // planes, =1   (always seems to be one)
    cBitCount: uint16;    // bits per pixel
                          // followed by RGB triples if <= 8bpp
  end;

  TRGB = packed record
    Blue: uint8;
    Green: uint8;
    Red: uint8;
  end;

  TRGBA = packed record
    Blue: uint8;
    Green: uint8;
    Red: uint8;
    Reserved: uint8;
  end;


  THelpBitmap = class( TfpgImage )
  protected
    _Header: INFBITMAPHEADER;
    _PaletteColorCount: longint;
    _pPalette: ^TRGB;
    _BitsSize: longint;
    FileHandle: TFileStream;
    _UncompressedBlockSize: longint;
    function GetPaletteSize: longint;
    procedure BitmapError(Msg: string);
    procedure ReadBitmapData( Blocks: TList; TotalSize: longint );
  public
    constructor CreateFromHelpFile(var AFileHandle: TFileStream; Offset: longint);
    destructor Destroy; override;
  end;


implementation

uses
  nvUtilities,
  Math,
  LZWDecompress,
  fpg_imgfmt_bmp;

const
  BFT_bMAP =$4d62; // 'bM'
  BFT_BITMAP_ARRAY = $4142; // 'BA'

type
  INFBITMAPDATAHEADER = packed record
    ulTotalSize: uint32;
    usUncompressedBlockSize: uint16; // bytes per block, after decompression
  end;

  TBitmapBlock = class(TObject)
  public
    _Size: uint16;
    _CompressionType: uint8;
    _Data: PBYTE;
    constructor Create;
    destructor Destroy; override;
  end;

constructor TBitmapBlock.Create;
begin
  _Data := nil;
end;

destructor TBitmapBlock.Destroy;
begin
  FreeMem( _Data );
  inherited Destroy;
end;

constructor THelpBitmap.CreateFromHelpFile(var AFileHandle: TFileStream; Offset: longint);
var
  WordsPerLine: longint;
  LineSize: longint;
  DataHeader: INFBITMAPDATAHEADER;
  BytesRead: longint;

  Block: TBitmapBlock;
  Blocks: TList;
  BlockIndex: longint;
  ImageType: uint16;
  BitmapArrayHeader: INFBITMAPARRAYHEADER;
  bytes: integer;
begin
  inherited Create;
  FileHandle := AFileHandle;

  FileHandle.Seek(Offset, soBeginning);
  bytes := FileHandle.Read(ImageType, sizeof(ImageType));
  if bytes <> SizeOf(ImageType) then
    raise EHelpBitmapException.Create( 'Failed to read ImageType.' );

  if ImageType = BFT_BITMAP_ARRAY then
  begin
    // skip array header and read first bitmap only
    FileHandle.Seek(Sizeof( BitmapArrayHeader ) - sizeof( ImageType ),  soCurrent);
  end
  else
  begin
    // skip back over imagetype bytes to read header.
    FileHandle.Seek(- sizeof( ImageType ),  soCurrent);
  end;

  // Read bitmap header
  bytes := FileHandle.Read(_Header, SizeOf(_Header));
  if bytes <> SizeOf(_Header) then
    raise EHelpBitmapException.Create( 'Failed to read Header.' );

  // Check it's got a valid type
  if _Header.usType <> BFT_bMAP then
    raise EHelpBitmapException.Create( 'Invalid bitmap header' );

// Graeme: we don't need to do this any more. It was only for Sybil
//  _Header.usType := $4d42; // sibyl only accepts 'BM' not 'bM'

  // We can only parse bitmaps with 1 colour plane
  // (I can't be bothered and have never seen bitmaps
  //  with more than 1 color plane)
  if _Header.cPlanes <> 1 then
    exit;

  _PaletteColorCount := 0;
  if _Header.cBitCount < 24 then
    _PaletteColorCount := 1 shl _Header.cBitCount;

  // OS/2 always rounds bitmap rows up to a word:
  WordsPerLine := ( _Header.cBitCount * _Header.cx + 31 ) div 32;
  LineSize := WordsPerLine * 4;

  // Total size of the bitmap pixel data
  _BitsSize := LineSize * _Header.cy;

  // Correct header offset - it is wrong in the header (why?)
  _Header.OffBits := sizeof( _Header ) + GetPaletteSize;

  // Load palette
  if _Header.cBitCount <= 8 then
  begin
    _pPalette := GetMem( GetPaletteSize );
    bytes := FileHandle.Read(_pPalette^, GetPaletteSize);
    if bytes <> GetPaletteSize then
      raise EHelpBitmapException.Create( 'Failed to read Palette.' );
  end;

  // Read data header
//  FillChar( DataHeader, sizeof( DataHeader ), 0 );
  bytes := FileHandle.Read(DataHeader, SizeOf(DataHeader));
  if bytes <> SizeOf(DataHeader) then
    raise EHelpBitmapException.Create( 'Failed to read DataHeader.' );
  _UncompressedBlockSize := DataHeader.usUncompressedBlockSize;

  // For counting total size, we have already read some bytes:
  // the uncompressedblocksize field
  BytesRead := sizeof( DataHeader.usUncompressedBlockSize );
  Blocks := TList.Create;

  while BytesRead < DataHeader.ulTotalSize do
  begin
    Block := TBitmapBlock.Create;

    // Read the block size
    FileHandle.Read(Block._Size, SizeOf(Block._Size));
    inc( BytesRead, sizeof( Block._Size ) );

    // Read the compression type
    FileHandle.Read(Block._CompressionType, SizeOf(Block._CompressionType));
    inc( BytesRead, sizeof( Block._CompressionType ) );

    // since size in the file includes this compression type field, subtract it
    dec( Block._Size, sizeof( Block._CompressionType ) );

    // Now read the block
    Block._Data := GetMem( Block._Size );
    FileHandle.Read(Block._Data^, Block._Size);

    inc( BytesRead, Block._Size  );
    Blocks.Add( Block );
  end;

  ReadBitmapData( Blocks, sizeof( _Header ) + GetPaletteSize + _BitsSize );

  for BlockIndex := 0 to Blocks.Count - 1 do
  begin
    Block := TBitmapBlock(Blocks[ BlockIndex ]);
    Block.Free;
  end;

  Blocks.Free;
end;

function THelpBitmap.GetPaletteSize: longint;
begin
  Result := sizeof( TRGB ) * _PaletteColorCount;
end;

procedure THelpBitmap.BitmapError(Msg: string);
begin
  //Msg:=Msg+' at position '+IntToStr(s.Position);
  //if fStartPos>0 then
  //  Msg:=Msg+'(BitmapPosition='+IntToStr(fStartPos)+')';
  raise EHelpBitmapException.Create(Msg);
end;

destructor THelpBitmap.Destroy;
begin
  FreeMem( _pPalette );
  inherited Destroy;
end;

procedure THelpBitmap.ReadBitmapData( Blocks: TList; TotalSize: longint );
var
  BytesWritten: longint;
  BytesWrittenFromBlock: longword;
  BytesRemainingInBlock: longword;
  BytesRemainingInBitmap: longword;
  FillerBytesRequired: longint;
  lastOutByte: byte;
  BitmapOutputPointer: PByte;
  Block: TBitmapBlock;
  BlockIndex: longint;
  BitmapData: PBYTE;
  ptr: PByte;
  i: integer;
  img: TfpgImage;
begin
  BitmapOutputPointer := nil;
  BitmapData := nil;
  ptr := nil;

  // Allocate memory to store the bitmap
  BitmapData := GetMem( TotalSize );

  // Copy header to bitmap
  MemCopy( _Header, BitmapData^, sizeof( _Header ) );

  // Copy palette into bitmap
  ptr := BitmapData + sizeof( _Header );
  MemCopy( _pPalette^, ptr^, GetPaletteSize );

  BytesWritten := 0;

  // Point to start writing to bitmap bits.
  BitmapOutputPointer := BitmapData + sizeof( _Header ) + GetPaletteSize;

  for BlockIndex := 0 to Blocks.Count - 1 do
  begin
    Block := TBitmapBlock(Blocks[ BlockIndex ]);

    case Block._CompressionType of
      0,1: // uncompressed (I'm not sure about 1)
      begin
        MemCopy( Block._Data^, BitmapOutputPointer^, Block._Size );
        BytesWrittenFromBlock := Block._Size;
        inc( BytesWritten, BytesWrittenFromBlock );
      end;

      2: // LZW compression
      begin
        LZWDecompressBlock( Block._Data,
                            Block._Size,
                            BitmapOutputPointer,
                            BytesWrittenFromBlock,
                            lastOutByte );

        inc( BytesWritten, BytesWrittenFromBlock );

        // If the uncompressed data stopped short then
        // copy the final code (byte) into remaining bytes
        if ( BytesWrittenFromBlock < _UncompressedBlockSize )
          and ( BytesWritten < _BitsSize ) then
        begin
          BytesRemainingInBlock := _UncompressedBlockSize - BytesWrittenFromBlock;
          BytesRemainingInBitmap := _BitsSize - BytesWritten;

          FillerBytesRequired := Min( BytesRemainingInBlock,
                                      BytesRemainingInBitmap );

          FillMem( BitmapOutputPointer + BytesWrittenFromBlock,
                   FillerBytesRequired,
                   LastOutByte );
          inc( BytesWritten, FillerBytesRequired );
          inc( BytesWrittenFromBlock, FillerBytesRequired );
        end;
      end;
      else
        raise EHelpBitmapException.Create( 'Unrecognised bitmap block type' );
    end; // case

    assert( BytesWrittenFromBlock <= _UncompressedBlockSize );
    assert( BytesWritten <= _BitsSize );

    if ( BitmapOutputPointer + BytesWrittenFromBlock
         > BitmapData + TotalSize ) then
      assert( false );

{ NOTE: This doesn't seem right. It moves the pointer so later the moving of data to
  ImageData will be wrong! }
//    inc( BitmapOutputPointer, BytesWrittenFromBlock ); TPersistentObjectState
  end;

  i := TotalSize + SizeOf(_Header) + GetPaletteSize;
  img := CreateImage_BMP(BitmapData, i);

  AllocateImage(32, _Header.cx, _Header.cy);

  {$IFDEF LZW_DEBUG}
  writeln('Width = ', Width);
  writeln('Height = ', Height);
  writeln('ImageDataSize = ', ImageDataSize);
  writeln('------------- START -------------');
  for i := 1 to ImageDataSize do
  begin
    write(HexStr(BitmapOutputPointer[i-1],2)+' ');
    if (i mod 16 = 0) then
      writeln('')
    else if (i mod 4 = 0) then
      write (' | ');
  end;
  Writeln('');
  writeln('------------- END -------------');
  {$ENDIF}

//  Move(BitmapOutputPointer^, ImageData^, ImageDataSize);
  Move(img.ImageData^, self.ImageData^, img.ImageDataSize);
  UpdateImage;
  img.Free;

  FreeMem( BitmapData, TotalSize );
end;


end.
