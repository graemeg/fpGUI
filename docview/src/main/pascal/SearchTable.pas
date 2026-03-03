{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright 2003 Aaron Lawrence (aaronl at consultant dot com)
    Copyright (C) 2006 - 2011 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Code to read and use the IPF search tables. Note: The RLE
      decompression was arrived at by trial and error. It seems
      to be correct but it's difficult to test.
}

Unit SearchTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, IPFFileFormatUnit;

type
  TSearchTable = class(TObject)
  protected
    _Data: pointer;
    _Entries: TList; // pointers to panel flag records
    _RecordLengthIs16Bit: boolean;
    _DictionaryCount: longint;
    _TopicCount: longint;

    procedure ReadEntries;

    Procedure Check1ByteOfFlags( b: byte;
                                 StartingIndex: longint;
                                 Results: UInt32ArrayPointer );

    procedure DoRLESearch( p: pbyte;
                           pDataEnd: pointer;
                           Results: UInt32ArrayPointer );

  public
    constructor Create( Data: pointer;
                        RecordLengthIs16Bit: boolean;
                        DictionaryCount: longint;
                        TopicCount: longint );
    destructor Destroy; override;

    // Sets Results to 1 for occurrences of DictIndex
    procedure Search( DictIndex: uint16;
                      Results: UInt32ArrayPointer );

  end;


implementation


constructor TSearchTable.Create( Data: pointer;
                                 RecordLengthIs16Bit: boolean;
                                 DictionaryCount: longint;
                                 TopicCount: longint );
begin
  _Data := Data;
  _RecordLengthIs16Bit := RecordLengthIs16Bit;
  _Entries := TList.Create;
  _DictionaryCount := DictionaryCount;
  _TopicCount := TopicCount;
  ReadEntries;
end;

destructor TSearchTable.Destroy;
begin
  _Entries.Destroy;
end;

procedure TSearchTable.ReadEntries;
var
  pWordRecord: pointer;
  RecordLen: uint16;
  WordIndex: uint16;
begin
  pWordRecord:= _Data;

  for WordIndex:= 0 to _DictionaryCount - 1 do
  begin
    _Entries.Add( pWordRecord );

    if _RecordLengthIs16Bit then
      RecordLen:= pUInt16( pWordRecord )^
    else // 8 bit
      RecordLen:= pUInt8( pWordRecord )^;
    inc( pWordRecord, RecordLen );
  end;
end;


// Search table decompression

// Looks through a single byte of 8 flags, given by b,
// and updates topic entries within results for any flags
// that are set.
Procedure TSearchTable.Check1ByteOfFlags( b: byte;
                                          StartingIndex: longint;
                                          Results: UInt32ArrayPointer );
var
  TopicIndex: longint;
begin
  TopicIndex:= StartingIndex;
  while b > 0 do
  begin
    if b and $80 > 0 then
      Results^[ TopicIndex ] := 1;
    inc( TopicIndex );
    b:= b shl 1;
  end;
end;

// Decompress RLE compressed data starting at p,
// running til pDataEnd. Update topic entries in Results.
procedure TSearchTable.DoRLESearch( p: pbyte;
                                    pDataEnd: pointer;
                                    Results: UInt32ArrayPointer );
var
  TopicIndex: integer;

  N: integer;
  thebyte: byte;
  byte1, byte2: byte;
begin
  assert( pbyte( p )^ = 1, 'Unexpected RLE type' );
  inc( p ); // skip header, always 1?

  TopicIndex:= 0;

  while p < pDataEnd do
  begin
    thebyte:= p^;
    inc( p );

    if thebyte = $80 then
    begin
      // escape
      thebyte := p^;
      inc( p );

      if thebyte = 0 then
      begin
        // 16 bit repeat of zeroes??
        N := pUInt16( p )^ + 1;
        inc( p, 2 );
        inc( TopicIndex, N );
      end
      else
      begin
        // n+1 repeats of next 2 bytes???
        N := thebyte + 1;
        byte1 := p^;
        inc( p );
        byte2 := p^;
        inc( p );
        while N > 0 do
        begin
          Check1ByteOfFlags( byte1,
                             TopicIndex,
                             Results );
          inc( TopicIndex, 8 );
          Check1ByteOfFlags( byte2,
                             TopicIndex,
                             Results );
          inc( TopicIndex, 8 );
          dec( N );
        end;
      end;
    end
    else
    begin
      N:= thebyte and $7f + 1;

      if thebyte and $80 > 0 then
      begin
        // literal data
        while N > 0 do
        begin
          Check1ByteOfFlags( p^,
                             TopicIndex,
                             Results );
          inc( TopicIndex, 8 );
          inc( p );
          dec( N );
        end;
      end
      else
      begin
        // repeat of next byte
        thebyte := p^;
        inc( p );
        while N > 0 do
        begin
          Check1ByteOfFlags( thebyte,
                             TopicIndex,
                             Results );
          inc( TopicIndex, 8 );
          dec( N );
        end;
      end;
    end;
  end;
end;

// This function finds uses of the given word (DictIndex)
// using the search table. Results[ topic ] is set to
// non-zero for topics which contain the word.
procedure TSearchTable.Search( DictIndex: uint16;
                               Results: UInt32ArrayPointer );
var
  TopicIndex: integer;
  pWordRecord: pointer;
  RecordLen: uint16;
  CompressionCode: uint8;
  pData: pointer;
  pDataEnd: pointer;
  Flags: uint8;
begin
  pWordRecord:= _Entries[ DictIndex ];

  // Check search table format
  if _RecordLengthIs16Bit then
  begin
    RecordLen:= pUInt16( pWordRecord )^;
    CompressionCode:= pUInt8( pWordRecord + 2 )^;
    pData:= pWordRecord + 3;
  end
  else // 8 bit
  begin
    RecordLen:= pUInt8( pWordRecord )^;
    CompressionCode:= pUInt8( pWordRecord + 1 )^;
    pData:= pWordRecord + 2;
  end;

  // Decompress the search table for this word
  pDataEnd:= pWordRecord + RecordLen;
  case CompressionCode of
    0: // word not used anywhere.
      ClearUInt32Array( Results, _TopicCount );

    1: // used in all panels
      FillUInt32Array( Results, _TopicCount, 1 );

    2: // RLE
    begin
      ClearUInt32Array( Results, _TopicCount );
      DoRLESearch( pData,
                   pDataEnd,
                   Results );
    end;

    3: // list of topics containing word
    begin
      ClearUInt32Array( Results, _TopicCount );
      while pData < pDataEnd do
      begin
        TopicIndex:= pUInt16( pData )^;
        Results^[ TopicIndex ] := 1;
        inc( pData, 2 );
      end;
    end;

    4: // list of topics NOT containing word
    begin
      FillUInt32Array( Results, _TopicCount, 1 );

      while pData < pDataEnd do
      begin
        TopicIndex:= pUInt16( pData )^;
        Results^[ TopicIndex ] := 0;
        inc( pData, 2 );
      end;
    end;

    5, // compressed by truncating bit stream at last byte containing a set bit.
    6: // same as above but starting at non-zero byte (first word contains start topic)
    begin
      ClearUInt32Array( Results, _TopicCount );
      if CompressionCode = 5 then
      begin
        TopicIndex:= 0
      end
      else
      begin
        TopicIndex:= pUInt16( pData )^ * 8;
        inc( pData, 2  );
      end;

      while pData < pDataEnd do
      begin
        Flags:= pUInt8( pData )^;
        Check1ByteOfFlags( Flags,
                           TopicIndex,
                           Results );
        inc( TopicIndex, 8 );
        inc( pData );
      end;
    end;

    else  { unmatched case items }
    begin
//      writeln('Unknown search method: Found no topic text match');
      // unknown method
      ClearUInt32Array( Results, _TopicCount );
    end;
  end;
end;


end.
