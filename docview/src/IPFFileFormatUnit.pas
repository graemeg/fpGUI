Unit IPFFileFormatUnit;

{$mode objfpc}{$H+}

interface

// Definition of IPF file header and other structures

uses
  SysUtils;

type
  uint32  = longword;       // 4 bytes
  uint16  = word;           // 2 bytes
  uint8   = byte;           // 1 byte
  pUInt16 = ^uint16;
  pUInt32 = ^uint32;
  pUInt8  = ^uint8;
  Unsigned_31 = 0 .. (1 shl 31) - 1;  // 31-bit type
  Unsigned_4  = 0 .. (1 shl 4) - 1;   // 4-bit type
  Unsigned_1  = 0 .. (1 shl 1) - 1;   // 1-bit type


  PCharArray  = packed array[ 0..0 ] of PCHar;
  UInt32Array = packed array[ 0..0 ] of UInt32;
  UInt16Array = packed array[ 0..0 ] of UInt16;
  UInt8Array  = packed array[ 0..0 ] of UInt8;

  PCharArrayPointer   = ^PCharArray;
  UInt32ArrayPointer  = ^UInt32Array;
  UInt16ArrayPointer  = ^UInt16Array;
  UInt8ArrayPointer   = ^UInt8Array;

  TBooleanArray = array[ 0..0 ] of boolean;
  BooleanArrayPointer = ^TBooleanArray;


  EHelpFileException = class( Exception )
  end;


  EWindowsHelpFormatException = class( Exception )
  end;


  TProgressCallback = procedure(n, outof: integer; AMessage: string) of object;


const
  ErrorCorruptHelpFile = 'Corrupt help file, or something similar';

const
  INF_HEADER_ID = 'HSP';

Type
  THelpFileHeader = packed record
    ID: array[0..2] of ansichar; // ID magic word "HSP"
    flags: uint8;         // probably a flag word...
                          // [0x01] bit 0: set if INF style file
                          // [0x10] bit 4: set if HLP style file
                          // patching this byte allows reading HLP files
                          // using the VIEW command, while help files
                          // seem to work with INF settings here as well.
    hdrsize: uint16;      // total size of header
    version_hi: uint8;
    version_lo: uint8;
    ntoc: uint16;         // number of entries in the tocarray
    tocstart: uint32;     // file offset of the start of the toc
    toclen: uint32;       // number of bytes in file occupied by the toc
    tocoffsetsstart: uint32;     // file offset of the start of array of toc offsets
    nres: uint16;         // number of panels with ressource numbers
    resstart: uint32;     // 32 bit file offset of ressource number table
    nname: uint16;        // number of panels with textual name
    namestart: uint32;    // 32 bit file offset to panel name table
    nindex: uint16;       // number of index entries
    indexstart: uint32;   // 32 bit file offset to index table
    indexlen: uint32;     // size of index table
    icmdCount: uint16;    // number of icmd index items
    icmdOffset: uint32;   // file offset to icmd index items
    icmdSize: uint32;     // size of icmd index table
    searchstart: uint32;  // 31 bit file offset of full text search table
                          // Note: top bit indicates 16 or 8 bit search record!
    searchlen: uint32;    // size of full text search table
    nslots: uint16;       // number of "slots"
    slotsstart: uint32;   // file offset of the slots array
    dictlen: uint32;      // number of bytes occupied by the "dictionary"
    ndict: uint16;        // number of entries in the dictionary
    dictstart: uint32;    // file offset of the start of the dictionary
    imgstart: uint32;     // file offset of image data
    maxCVTIndex: byte;    // highest index inside panel's local dictionary,
                          // always seems to be 245
    nlsstart: uint32;     // 32 bit file offset of NLS table
    nlslen: uint32;       // size of NLS table
    extstart: uint32;     // 32 bit file offset of extended data block
    reserved: array[ 0..11 ] of byte; // for future use. set to zero.
    title: array[ 0..47 ] of ansichar;    // ASCII title of database
  end;
  TPHelpFileHeader = ^THelpFileHeader;

  TExtendedHelpFileHeader = packed record
    NumFontEntry: uint16;             // FONT TABLE:   Number entries
    FontTableOffset: uint32;          // FONT TABLE:   Offset in file
    NumDataBase: uint16;              // DATA BASE:    Number of files
    DataBaseOffset: uint32;           // DATA BASE:    Offset in file
    DataBaseSize: uint32;             // DATA BASE:    Size in bytes
    EntryInGNameTable: uint16;        // GLOBAL NAMES: Number entries
    HelpPanelGNameTblOffset: uint32;  // GLOBAL NAMES: Offset in file
    StringsOffset: uint32;            // STRINGS : Offset in file
    StringsSize: uint16;              // STRINGS : Total bytes of all strings
    ChildPagesOffset: uint32;         // CHILDPAGES : Offset in file
    ChildPagesSize: uint32;           // CHILDPAGES : Total bytes of all strings
    NumGIndexEntry: uint32;           // Total number of Global Index items
    CtrlOffset: uint32;               // CTRL BUTTONS : offset in file
    CtrlSize: uint32;                 // CTRL BUTTONS : size in bytes
    Reserved: array[0..3] of uint32;  // For future use. Set to zero
  end;
  TPExtendedHelpFileHeader = ^TExtendedHelpFileHeader;

Type
  TTOCEntryStart = packed record
    length: uint8; // length of the entry including this byte (but not including extended data)
    flags: uint8; // flag byte, description folows (MSB first)
                  // bit8 haschildren;  // following nodes are a higher level
                  // bit7 hidden;       // this entry doesn't appear in VIEW.EXE's
                                        // presentation of the toc
                  // bit6 extended;     // extended entry format
                  // bit5 stuff;        // ??
                  // int4 level;        // nesting level
    numSlots: uint8; // number of "slots" occupied by the text for
                                // this toc entry
  end;
  pTTOCEntryStart = ^TTOCEntryStart;

  TExtendedTOCEntry = packed record
    w1: uint8;
               // bit 3: Window controls are specified
               // bit 2: Viewport
               // bit 1: Size is specified.
               // bit 0: Position is specified.
    w2: uint8;
               // bit 3:
               // bit 2: Group is specified.
               // bit 1
               // bit 0: Clear (all windows before display)
  end;
  pExtendedTOCEntry = ^TExtendedTOCEntry;

  TTOCEntryOffsetArray =  packed array[ 0..0 ] of uint32;
  pTTOCEntryOffsetArray = ^TTOCEntryOffsetArray;

const
  TOCEntryExtended      = $20; { extended entry format }
  TOCEntryHidden        = $40; { this entry doesn't appear in VIEW.EXE's presentation of the toc }
  TOCEntryHasChildren   = $80; { following nodes are a higher level }
  TOCEntryLevelMask     = $0f;

type
  THelpXYPair = packed record
    Flags: uint8;
    X: uint16;
    Y: uint16;
  end;
  pHelpXYPair = ^THelpXYPair;

  TSlotHeader = packed record
    stuff: uint8;             // always 0??
    localdictpos: uint32;     // file offset of the local dictionary
    nlocaldict: uint8;        // number of entries in the local dict
    ntext: uint16;            // number of bytes in the text
  end;
  pSlotHeader = ^TSlotHeader;

  THelpFontSpec = packed record
    FaceName: array[ 0..32 ] of ansichar;
    Height: uint16;
    Width: uint16;
    Codepage: uint16;
  end;
  pTHelpFontSpec = ^THelpFontSpec;

  TNlsHeader = packed record
    NlsSize: uint16;
    NlsType: uint8;
    NlsFormat: uint8;
  end;

  TNlsCountryDef = packed record
    { if the following is true then...
      NlsHeader.size = 10
      NlsHeader.type = NLSRecType.CONTROL
      NlsHeader.format = 0
    }
    Value: uint16;      // =256
    Code: uint16;       // country code
    Page: uint16;       // code page
    Reserved: uint16;
  end;

  // Single-byte character set
  TSbcsNlsGrammerDef = packed record
    { if the following is true then...
      NlsHeader.size = 36
      NlsHeader.type = NLSRecType.WORD || NLSRecType.GRAPHIC
      NlsHeader.format = 0
    }
    bits: array[0..31] of uint8;  // high-order bits first
  end;

  TPanelControls = packed record
    ControlCount: uint16;   // number of ControlDef records
    GroupCount: uint16;     // number of GroupDef records
    GroupIndex: uint16;     // for cover page
    Reserved: uint16;
  end;

  TControlDef = packed record
    CtrlType: uint16;       // type of control
    ResourceID: uint16;     // resource id (panel) it belongs to
    { variable length data follows, contains button text }
    // DictString: array of char;
  end;

  TControlGroupDef = packed record
    Count: uint16;          // number of indexes into ControlDef records
    { variable length data follows }
    // index[count] of type uint16
  end;

// List of IPF escape codes. 

const
  // Basic byte codes
  IPF_END_PARA = $fa;
  IPF_CENTER = $fb;
  IPF_INVERT_SPACING = $fc;
  IPF_LINEBREAK = $fd;
  IPF_SPACE = $fe;
  IPF_ESC = $ff; // followed by one of the ecXXX codes below

  // FF XX
  ecSetLeftMargin = $02;
  ecHighlight1 = $04; // hp1,2,3,5,6,7
  ecLinkStart = $05;
  ecFootnoteLinkStart = $07;
  ecLinkEnd = $08;
  ecStartCharGraphics = $0b;
  ecEndCharGraphics = $0c;
  ecHighlight2 = $0d; // hp4,8,9
  ecImage = $0e;
  ecLinkedImage = $0f;
  ecProgramLink = $10;
  ecSetLeftMarginNewLine = $11;
  ecSetLeftMarginFit = $12;
  ecForegroundColor = $13;
  ecBackgroundColor = $14;
  ecFontChange = $19;
  ecStartLines = $1a;
  ecEndLines = $1b;
  ecSetLeftMarginHere = $1c;
  ecStartLinkByResourceID = $1d;
  ecExternalLink = $1f;

  // Subescape codes of
  HPART_DEFINE = 0;
  HPART_PT_HDREF = 1;
  HPART_PT_FNREF = 2;
  HPART_PT_SPREF = 3;
  HPART_HDREF = 4;
  HPART_FNREF = 5;
  HPART_SPREF = 6;
  HPART_LAUNCH = 7;
  HPART_PT_LAUNCH = 8;
  HPART_INFORM = 9;
  HPART_PT_INFORM = 10;
  // ?? 11 ??
  HPART_EXTERN_PT_HDREF = 12;
  HPART_EXTERN_PT_SPREF = 13;
  HPART_EXTERN_HDREF = 14;
  HPART_EXTERN_SPREF = 15;
  HPART_GLOBAL_HDREF = 16;
  HPART_GLOBAL_PT_HDREF = 17;

// -----------------------------------------------------------
// Operations on Int32 arrays, used for searching
// These could be optimised heavily if needed.
procedure AllocUInt32Array( Var pArray: UInt32ArrayPointer;
                            Size: longint );
procedure FreeUInt32Array( Var pArray: UInt32ArrayPointer;
                           Size: longint );

procedure FillUInt32Array( pArray: UInt32ArrayPointer;
                           Size: longint;
                           Value: UInt32 );

procedure AddUInt32Array( pSource: UInt32ArrayPointer;
                          pDest: UInt32ArrayPointer;
                          Size: longint );

// Dest = Dest + source * Multiplier
procedure AddMultConstUInt32Array( pSource: UInt32ArrayPointer;
                                   Multiplier: longint;
                                   pDest: UInt32ArrayPointer;
                                   Size: longint );

procedure AndUInt32Array( pSource: UInt32ArrayPointer;
                          pDest: UInt32ArrayPointer;
                          Size: longint );

// If both source and dest > 0 then
//   add source to dest
procedure AndAddUInt32Array( pSource: UInt32ArrayPointer;
                             pDest: UInt32ArrayPointer;
                             Size: longint );

// if Source > 0 then dest is set to 0
procedure AndNotUInt32Array( pSource: UInt32ArrayPointer;
                             pDest: UInt32ArrayPointer;
                             Size: longint );

// dest = dest or source;
// if source > 0  then set dest to  > 0
procedure OrUInt32Array( pSource: UInt32ArrayPointer;
                         pDest: UInt32ArrayPointer;
                         Size: longint );

// if source = 0 then dest set to >0
procedure NotOrUInt32Array( pSource: UInt32ArrayPointer;
                            pDest: UInt32ArrayPointer;
                            Size: longint );

procedure CopyUInt32Array( pSource: UInt32ArrayPointer;
                           pDest: UInt32ArrayPointer;
                           Size: longint );

procedure ClearUInt32Array( pArray: UInt32ArrayPointer;
                            Size: longint );
procedure SetUInt32Array( pArray: UInt32ArrayPointer;
                          Size: longint );

// returns the result of ORing every array element.
// Can be useful for debugging e.g. seeing at a glance
// if any element is non-zero
function OrAllUInt32Array( pArray: UInt32ArrayPointer;
                           Size: longint ): longint;


Implementation


// Operations on int32 arrays
// -----------------------------------------------------------

procedure AllocUInt32Array( Var pArray: UInt32ArrayPointer;
                            Size: longint );
begin
  GetMem( pArray,
          Size
          * sizeof( UInt32 ) );
end;

procedure FreeUInt32Array( Var pArray: UInt32ArrayPointer;
                           Size: longint );
begin
  FreeMem( pArray,
           Size
           * sizeof( UInt32 ) );
end;

// This is a nice fast implementation of filling an
// array of dwords (Int32/longword)
procedure FillUInt32Array( pArray: UInt32ArrayPointer;
                           Size: longint;
                           Value: UInt32 );
var
  i: integer;
begin
  assert( Size > 0 );
  if Size < 1 then
    Exit;
  for i := 0 to Size-1 do
  begin
    pArray^[i] := Value;
  end;
end;

procedure ClearUInt32Array( pArray: UInt32ArrayPointer;
                            Size: longint );
begin
  FillUInt32Array( pArray, Size, 0 );
end;

procedure SetUInt32Array( pArray: UInt32ArrayPointer;
                          Size: longint );
begin
  FillUInt32Array( pArray, Size, $ffffffff );
end;

procedure AddUInt32Array( pSource: UInt32ArrayPointer;
                          pDest: UInt32ArrayPointer;
                          Size: longint );
var
  i: longint;
begin
  for i := 0 to Size - 1 do
    inc( pDest^[ i ], pSource^[ i ] );
end;

procedure AddMultConstUInt32Array( pSource: UInt32ArrayPointer;
                                   Multiplier: longint;
                                   pDest: UInt32ArrayPointer;
                                   Size: longint );
var
  i: longint;
begin
  for i := 0 to Size - 1 do
    inc( pDest^[ i ], pSource^[ i ] * Multiplier );
end;

procedure OrUInt32Array( pSource: UInt32ArrayPointer;
                         pDest: UInt32ArrayPointer;
                         Size: longint );
var
  i: longint;
begin
  for i := 0 to Size - 1 do
    pDest^[ i ] := pDest^[ i ] or pSource^[ i ];
end;

procedure CopyUInt32Array( pSource: UInt32ArrayPointer;
                           pDest: UInt32ArrayPointer;
                           Size: longint );
begin
  Move(pSource^, PDest^, Size * SizeOf(LongInt));
end;

procedure NotOrUInt32Array( pSource: UInt32ArrayPointer;
                            pDest: UInt32ArrayPointer;
                            Size: longint );
var
  i: longint;
begin
  for i := 0 to Size - 1 do
    if pSource^[ i ] = 0 then
      pDest^[ i ] := 1;
end;

procedure AndUInt32Array( pSource: UInt32ArrayPointer;
                          pDest: UInt32ArrayPointer;
                          Size: longint );
var
  i: longint;
begin
  for i := 0 to Size - 1 do
    pDest^[ i ] := pDest^[ i ] and pSource^[ i ];
end;

procedure AndAddUInt32Array( pSource: UInt32ArrayPointer;
                             pDest: UInt32ArrayPointer;
                             Size: longint );
var
  i: longint;
begin
  for i := 0 to Size - 1 do
    if     ( pSource^[ i ] > 0 )
       and ( pDest^[ i ] > 0 ) then
      inc( pDest^[ i ], pSource^[ i ] )
    else
      pDest^[ i ] := 0;
end;

procedure AndNotUInt32Array( pSource: UInt32ArrayPointer;
                             pDest: UInt32ArrayPointer;
                             Size: longint );
var
  i: longint;
begin
  for i := 0 to Size - 1 do
    if pSource^[ i ] > 0 then
      pDest^[ i ] := 0;
end;

function OrAllUInt32Array( pArray: UInt32ArrayPointer;
                           Size: longint ): longint;
var
  i: longint;
begin
  Result := 0;
  for i := 0 to Size - 1 do
    Result := Result or pArray^[ i ];
end;


end.
