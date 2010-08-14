unit HelpBitmap;

interface

// Encapsulates a bitmap as stored in a IPF file.
// Once created from file data they can be used as a normal bitmap.

uses
  Classes, SysUtils, fpg_main, ctypes,
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
    procedure DecompressLZW(var Buffer: Pointer; const Count: integer; var NewBuffer: PByte; var NewCount: integer);
    procedure ReadBitmapData( Blocks: TList; TotalSize: longint );
  public
    constructor CreateFromHelpFile(var AFileHandle: TFileStream; Offset: longint);
    destructor Destroy; override;
  end;


var
  LZWDecompressBlock: function( pInput: PBYTE;
                               pOutput: PBYTE;
                               bytesIn: uint32;
                               Var bytesOut: uint32;
                               Var FinalCode: byte ): Boolean;
//  APIENTRY;
//  'newview' index 1;

implementation

uses
  nvUtilities, Math, fpg_imgfmt_bmp;

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
    destructor Destroy; override;
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
  p: pointer;
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

  _Header.usType := $4d42; // sibyl only accepts 'BM' not 'bM'

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
  _Header.OffBits := sizeof( _Header ) + GetPaletteSize;           // TODO: Graeme, double check this!

  // Load palette
  if _Header.cBitCount <= 8 then
  begin
    _pPalette := GetMem( GetPaletteSize );
    bytes := FileHandle.Read(_pPalette, GetPaletteSize);
    if bytes <> GetPaletteSize then
      raise EHelpBitmapException.Create( 'Failed to read Palette.' );
  end;

  // Read data header
  FillChar( DataHeader, sizeof( DataHeader ), 0 );
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
    FileHandle.Read(Block._Data, Block._Size);

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

procedure THelpBitmap.DecompressLZW(var Buffer: Pointer; const Count: Integer; var NewBuffer: PByte; var NewCount: integer);
type
  TLZWString = packed record
    Count: integer;
    Data: PByte;
  end;
  PLZWString = ^TLZWString;
const
  ClearCode = 256; // clear table, start with 9bit codes
  EoiCode = 257; // end of input
var
//  NewBuffer: PByte;
//  NewCount: PtrInt;
  NewCapacity: PtrInt;
  SrcPos: PtrInt;
  SrcPosBit: integer;
  CurBitLength: integer;
  Code: Word;
  Table: PLZWString;
  TableCapacity: integer;
  TableCount: integer;
  OldCode: Word;

  function GetNextCode: Word;
  var
    v: Integer;
  begin
    Result:=0;
    // CurBitLength can be 9 to 12
    //writeln('GetNextCode CurBitLength=',CurBitLength,' SrcPos=',SrcPos,' SrcPosBit=',SrcPosBit,' ',hexstr(PByte(Buffer)[SrcPos],2),' ',hexstr(PByte(Buffer)[SrcPos+1],2),' ',hexstr(PByte(Buffer)[SrcPos+2],2));
    // read two or three bytes
    if CurBitLength+SrcPosBit>16 then begin
      // read from three bytes
      if SrcPos+3>Count then BitmapError('LZW stream overrun');
      v:=PByte(Buffer)[SrcPos];
      inc(SrcPos);
      v:=(v shl 8)+PByte(Buffer)[SrcPos];
      inc(SrcPos);
      v:=(v shl 8)+PByte(Buffer)[SrcPos];
      v:=v shr (24-CurBitLength-SrcPosBit);
    end else begin
      // read from two bytes
      if SrcPos+2>Count then BitmapError('LZW stream overrun');
      v:=PByte(Buffer)[SrcPos];
      inc(SrcPos);
      v:=(v shl 8)+PByte(Buffer)[SrcPos];
      if CurBitLength+SrcPosBit=16 then
        inc(SrcPos);
      v:=v shr (16-CurBitLength-SrcPosBit);
    end;
    Result:=v and ((1 shl CurBitLength)-1);
    SrcPosBit:=(SrcPosBit+CurBitLength) and 7;
    //writeln('GetNextCode END SrcPos=',SrcPos,' SrcPosBit=',SrcPosBit,' Result=',Result,' Result=',hexstr(Result,4));
  end;

  procedure ClearTable;
  var
    i: Integer;
  begin
    for i:=0 to TableCount-1 do
      ReAllocMem(Table[i].Data,0);
    TableCount:=0;
  end;

  procedure InitializeTable;
  begin
    CurBitLength:=9;
    ClearTable;
  end;

  function IsInTable(Code: word): boolean;
  begin
    Result:=Code<258+TableCount;
  end;

  procedure WriteStringFromCode(Code: integer; AddFirstChar: boolean = false);
  var
    s: TLZWString;
    b: byte;
  begin
    //WriteLn('WriteStringFromCode Code=',Code,' AddFirstChar=',AddFirstChar,' x=',(NewCount div 4) mod IDF.ImageWidth,' y=',(NewCount div 4) div IDF.ImageWidth,' PixelByte=',NewCount mod 4);
    if Code<256 then begin
      // write byte
      b:=Code;
      s.Data:=@b;
      s.Count:=1;
    end else if Code>=258 then begin
      // write string
      if Code-258>=TableCount then
        BitmapError('LZW code out of bounds');
      s:=Table[Code-258];
    end else
      BitmapError('LZW code out of bounds');
    if NewCount+s.Count+1>NewCapacity then begin
      NewCapacity:=NewCapacity*2+8;
      ReAllocMem(NewBuffer,NewCapacity);
    end;
    System.Move(s.Data^,NewBuffer[NewCount],s.Count);
    //for i:=0 to s.Count-1 do write(HexStr(NewBuffer[NewCount+i],2)); // debug
    inc(NewCount,s.Count);
    if AddFirstChar then begin
      NewBuffer[NewCount]:=s.Data^;
      //write(HexStr(NewBuffer[NewCount],2)); // debug
      inc(NewCount);
    end;
    //writeln(',WriteStringFromCode'); // debug
  end;

  procedure AddStringToTable(Code, AddFirstCharFromCode: integer);
  // add string from code plus first character of string from code as new string
  var
    b1, b2: byte;
    s1, s2: TLZWString;
    p: PByte;
  begin
    //WriteLn('AddStringToTable Code=',Code,' FCFCode=',AddFirstCharFromCode,' TableCount=',TableCount,' TableCapacity=',TableCapacity);
    // grow table
    if TableCount>=TableCapacity then begin
      TableCapacity:=TableCapacity*2+128;
      ReAllocMem(Table,TableCapacity*SizeOf(TLZWString));
    end;
    // find string 1
    if Code<256 then begin
      // string is byte
      b1:=Code;
      s1.Data:=@b1;
      s1.Count:=1;
    end else if Code>=258 then begin
      // normal string
      if Code-258>=TableCount then
        BitmapError('LZW code out of bounds');
      s1:=Table[Code-258];
    end else
      BitmapError('LZW code out of bounds');
    // find string 2
    if AddFirstCharFromCode<256 then begin
      // string is byte
      b2:=AddFirstCharFromCode;
      s2.Data:=@b2;
      s2.Count:=1;
    end else begin
      // normal string
      if AddFirstCharFromCode-258>=TableCount then
        BitmapError('LZW code out of bounds');
      s2:=Table[AddFirstCharFromCode-258];
    end;
    // set new table entry
    Table[TableCount].Count:=s1.Count+1;
    p:=nil;
    GetMem(p,s1.Count+1);
    Table[TableCount].Data:=p;
    System.Move(s1.Data^,p^,s1.Count);
    // add first character from string 2
    p[s1.Count]:=s2.Data^;
    // increase TableCount
    inc(TableCount);
    case TableCount+259 of
      512,1024,2048: inc(CurBitLength);
      4096: BitmapError('LZW too many codes');
    end;
  end;

begin
  if Count=0 then exit;
  //WriteLn('TFPReaderTiff.DecompressLZW START Count=',Count);
  //for SrcPos:=0 to 19 do
  //  write(HexStr(PByte(Buffer)[SrcPos],2));
  //writeln();

  NewBuffer:=nil;
  NewCount:=0;
  NewCapacity:=Count*2;
  ReAllocMem(NewBuffer,NewCapacity);

  SrcPos:=0;
  SrcPosBit:=0;
  CurBitLength:=9;
  Table:=nil;
  TableCount:=0;
  TableCapacity:=0;
  try
    repeat
      Code:=GetNextCode;
      //WriteLn('TFPReaderTiff.DecompressLZW Code=',Code);
      if Code=EoiCode then break;
      if Code=ClearCode then begin
        InitializeTable;
        Code:=GetNextCode;
        //WriteLn('TFPReaderTiff.DecompressLZW after clear Code=',Code);
        if Code=EoiCode then break;
        if Code=ClearCode then
          BitmapError('LZW code out of bounds');
        WriteStringFromCode(Code);
        OldCode:=Code;
      end else begin
        if Code<TableCount+258 then begin
          WriteStringFromCode(Code);
          AddStringToTable(OldCode,Code);
          OldCode:=Code;
        end else if Code=TableCount+258 then begin
          WriteStringFromCode(OldCode,true);
          AddStringToTable(OldCode,OldCode);
          OldCode:=Code;
        end else
          BitmapError('LZW code out of bounds');
      end;
    until false;
  finally
    ClearTable;
    ReAllocMem(Table,0);
  end;

  ReAllocMem(NewBuffer,NewCount);
//  FreeMem(Buffer);
//  Buffer:=NewBuffer;
//  Count:=NewCount;
end;


procedure THelpBitmap.ReadBitmapData( Blocks: TList;
                                      TotalSize: longint );
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
begin
  // Allocate memory to store the bitmap
  Bitmapdata := GetMem( TotalSize );

  // Copy header to bitmap
  MemCopy( _Header, BitmapData, sizeof( _Header ) );

  // Copy palette into bitmap
  ptr := BitmapData + sizeof( _Header );
  MemCopy( _pPalette, ptr, GetPaletteSize );

  BytesWritten := 0;

  // Point to start writing to bitmap bits.
  BitmapOutputPointer := BitmapData + sizeof( _Header ) + GetPaletteSize;

  for BlockIndex := 0 to Blocks.Count - 1 do
  begin
    Block := TBitmapBlock(Blocks[ BlockIndex ]);

    case Block._CompressionType of
      0,1: // uncompressed (I'm not sure about 1)
      begin
        MemCopy( Block._Data, BitmapOutputPointer, Block._Size );
        BytesWrittenFromBlock := Block._Size;
        inc( BytesWritten, BytesWrittenFromBlock );
      end;

      2: // LZW compression
      begin
        // decompress block
        if not Assigned( LZWDecompressBlock )then
          raise EHelpBitmapException.Create( 'Cannot decode bitmap - DLL not found' );

//        DecompressLZW(Block._Data, Block._Size);
        //LZWDecompressBlock( Block._Data,
        //                    BitmapOutputPointer,
        //                    Block._Size,
        //                    BytesWrittenFromBlock,
        //                    lastOutByte );

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

    inc( BitmapOutputPointer, BytesWrittenFromBlock );
  end;


  AllocateImage(32, _Header.cx, _Header.cy);
  if TotalSize <> ImageDataSize then
    writeln('Warning: INF Bitmap size and allocated bitmap size are different. ', TotalSize, ' vs ', ImageDataSize);
  Move(BitmapData^, ImageData^, TotalSize);
  UpdateImage;

  FreeMem( BitmapData, TotalSize );
end;


end.
