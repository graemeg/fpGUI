Unit HelpFileHeader;

{$mode objfpc}{$H+}

// NewView - a new OS/2 Help Viewer
// Copyright 2001 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

// Definition of IPF file header and other structures

uses
  DataTypes;

Type
  THelpFileHeader = record
    ID: int16;           // ID magic word (5348h = "HS")
    unknown1: int8;      // unknown purpose, could be third letter of ID
    flags: int8;         // probably a flag word...
                         //  bit 0: set if INF style file
                         //  bit 4: set if HLP style file
                         // patching this byte allows reading HLP files
                         // using the VIEW command, while help files 
                         // seem to work with INF settings here as well.
    hdrsize: int16;      // total size of header
    unknown2: int16;     // unknown purpose

    ntoc: int16;         // number of entries in the tocarray
    tocstart: int32;     // file offset of the start of the toc
    toclen: int32;       // number of bytes in file occupied by the toc
    tocoffsetsstart: int32;     // file offset of the start of array of toc offsets
    nres: int16;         // number of panels with ressource numbers
    resstart: int32;     // 32 bit file offset of ressource number table
    nname: int16;        // number of panels with textual name
    namestart: int32;    // 32 bit file offset to panel name table
    nindex: int16;       // number of index entries
    indexstart: int32;   // 32 bit file offset to index table
    indexlen: int32;     // size of index table
    unknown3: array[ 0..9 ] of int8; // unknown purpose
    searchstart: int32;  // 32 bit file offset of full text search table
    searchlen: int32;    // size of full text search table
    nslots: int16;       // number of "slots"
    slotsstart: int32;   // file offset of the slots array
    dictlen: int32;      // number of bytes occupied by the "dictionary"
    ndict: int16;        // number of entries in the dictionary
    dictstart: int32;    // file offset of the start of the dictionary
    imgstart: int32;     // file offset of image data
    unknown4: int8;     // unknown purpose
    nlsstart: int32;     // 32 bit file offset of NLS table
    nlslen: int32;       // size of NLS table
    extstart: int32;     // 32 bit file offset of extended data block
    reserved: array[ 0..2 ] of int32; // for future use. set to zero.
    title: array[ 0..47 ] of char;    // ASCII title of database
  end;
             
Type
  TTOCEntryStart = record
    length: int8; // length of the entry including this byte
    flags: int8; // flag byte, description folows (MSB first)
             // bit1 haschildren;  // following nodes are a higher level
             // bit1 hidden;       // this entry doesn't appear in VIEW.EXE's
                                   // presentation of the toc
             // bit1 extended;     // extended entry format
             // bit1 stuff;        // ??
             // int4 level;        // nesting level
    numSlots: int8; // number of "slots" occupied by the text for
                                // this toc entry
  end;
  pTTOCEntryStart = ^TTOCEntryStart;

  TExtendedTOCEntry = record
    w1: int8;
    w2: int8;
  end;
  pExtendedTOCEntry = ^TExtendedTOCEntry;

  TTOCEntryOffsetArray =  array[ 0..0 ] of int32;
  pTTOCEntryOffsetArray = ^ TTOCEntryOffsetArray;

Const
  TOCEntryExtended = 32;
  TOCEntryHidden = 64;
  TOCEntryHasChildren = 128;

type
  THelpXYPair = record
    Flags: int8;
    X: int16;
    Y: int16;
  end;
  pHelpXYPair = ^ THelpXYPair;

  TSlotHeader = record
    stuff: int8; // always 0??
    localdictpos: int32; // file offset of the local dictionary
    nlocaldict: int8; // number of entries in the local dict
    ntext: int16; // number of bytes in the text
  end;
  pSlotHeader = ^TSlotHeader;

Implementation

Initialization
End.
