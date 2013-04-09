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
    LZW decompression code for uncompressing IPF bitmaps.
}

unit LZWDecompress;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, types;

procedure LZWDecompressBlock( pbInput: pByte;
                              number_bytes: LongWord;
                              pbOutput: PBYTE;
                              Var bytesOut: LongWord;
                              Var FinalCode: byte );

Implementation

(*
/********************************************************************
 *                                                                  *
 *  LZW decompression                                               *
 *                                                                  *
 *******************************************************************/

/*
 *  This is based on code (W) by Peter Fitzsimmons, pfitz@ican.net.
 *  His liner notes in the original:
 *      has its roots in a June 1990
 *      DDJ article "LZW REVISITED", by Shawn M. Regan
 *      --=>revision history<=--
 *      1 lzw.c 21-Aug-96,2:24:36,`PLF' ;
 *      2 lzw.c 24-Aug-96,2:27:24,`PLF' wip
 *
 *  The code has been modified to take the input not from an
 *  open file, but from any memory region. For this, a double
 *  pointer is used, which must be passed to LZWDecompressBlock.
 *  I've also added a few comments for clarity.
 *
 * Ported to Sibyl Pascal by Aaron Lawrence
 * Variables renamed etc to make things clearer.
 */
*)
// -- Stuff for LZW decompression -- */
const INIT_BITS = 9;
const MAX_BITS = 12;     //PLF Tue  95-10-03 02:16:56*/
const HASHING_SHIFT = MAX_BITS - 8;

{if MAX_BITS == 15
const TABLE_SIZE 36768
#elif MAX_BITS == 14
const TABLE_SIZE 18041
#elif MAX_BITS == 13
const TABLE_SIZE 9029
#else}
// For max_bits = 12:
const TABLE_SIZE = 5021;

const CLEAR_TABLE = 256;
const TERMINATOR = 257;
const FIRST_CODE = 258;

function MaxValNBits( N: word ): word;
begin
  Result:= ( 1 shl n ) - 1;
end;

var
  prefix_code: array[ 0..TABLE_SIZE ] of longword;
  append_character: array[ 0..TABLE_SIZE ] of Byte;
  decode_stack: array[ 0..10000 ] of byte;
  bitsPerCode: longint;
  maxDictionaryCode: longint;

(*
 * decode_string:
 *
 *)
function decode_string( buffer: PByte; code: longword ): PByte;
var
  i: longint;
begin
  i:= 0;

  while Code > 255 do
  begin
    buffer^:= append_character[ Code ];
    inc( Buffer );
    code:= prefix_code[ code ];

    inc( i );
    if i > High( decode_stack ) then
      assert( false, 'Out of space decompressing bitmap!' );
  end;

  buffer^ := code;
  Result:= buffer;
end;

(*
 * input_code:
 *      this function reads in bytes from the input
 *      stream.
 *)

var
  bytes_out: longword = 0;
  input_bit_count: longword = 0;
  input_bit_buffer: longword = 0;

// I think this simply reads the next bitsPerCode bits of the input data
// returning the resulting code.
function input_code( var pbInput: PBYTE; bytes_to_read: longword ): longword;
var
  return_value: longword;
begin
  while input_bit_count <= 24 do
  begin
    if bytes_out <= bytes_to_read then
    begin
      input_bit_buffer:= input_bit_buffer
                         or
                         ( ( longword( pbInput^ ) shl (24 - input_bit_count) ) );
      inc( pbInput );
    end
    else
      input_bit_buffer:= input_bit_buffer
                         or
                         ( longword( 0 ) shl ( 24 - input_bit_count ) );
    inc( bytes_out );
    inc( input_bit_count, 8 );
  end;

  return_value:= input_bit_buffer shr (32 - bitsPerCode);
  input_bit_buffer:= input_bit_buffer shl bitsPerCode;
  dec( input_bit_count, bitsPerCode );

  if bytes_out > bytes_to_read then
  begin
    // flush static vars and quit */
    bytes_out:= 0;
    input_bit_count:= 0;
    input_bit_buffer:= 0;
    Result:= TERMINATOR;
  end
  else
    Result:= return_value;
end;

// LZWDecompressBlock:
//      this takes one of the INF bitmap blocks
//      and decompresses it using LZW algorithms.

procedure LZWDecompressBlock( pbInput: pByte;
                              number_bytes: LongWord;
                              pbOutput: PBYTE;
                              Var bytesOut: LongWord;
                              Var FinalCode: byte );
var
  nextAvailableCode: LongWord;
  currentCode: LongWord;
  lastCode: LongWord;
  character: longword;
  clear_flag: boolean;
  theString: pByte;
begin
  clear_flag:= true;

  nextAvailableCode:= FIRST_CODE;
  bitsPerCode:= INIT_BITS;
  maxDictionaryCode:= MaxValNBits( bitsPerCode );

  bytesOut:= 0;
  input_bit_count:= 0;
  input_bit_buffer:= 0;

  // read the first code from input
  currentCode:= input_code( pbInput, number_bytes );
  while currentCode <> TERMINATOR do
  begin
    if clear_flag then
    begin
      clear_flag:= false;
      lastCode:= currentCode;
      character:= currentCode;

      pbOutput^:= currentCode;
      inc( pbOutput );
      FinalCode:= currentCode;
      inc( BytesOut );
    end
    else if currentCode = CLEAR_TABLE then
    begin
      clear_flag:= true;
      nextAvailableCode:= FIRST_CODE;
      bitsPerCode:= INIT_BITS;
      maxDictionaryCode:= MaxValNBits( bitsPerCode );
    end
    else
    begin
      if currentCode >= nextAvailableCode then
      begin
         decode_stack[ 0 ]:= character;
         theString:= decode_string( Addr( decode_stack[ 1 ] ),
                                    lastCode );
      end
      else
        theString:= decode_string( Addr( decode_stack[ 0 ] ),
                                   currentCode );

      character:= longword( theString^ );
      while theString >= Addr( decode_stack[ 0 ] ) do
      begin
        FinalCode:= theString^;

        pbOutput^:= theString^;
        inc( pbOutput );
        dec( TheString );

        inc( BytesOut );
      end;

      if nextAvailableCode <= maxDictionaryCode then
      begin
        prefix_code[ nextAvailableCode ]:= lastCode;
        append_character[ nextAvailableCode ]:= character;

        inc( nextAvailableCode );

        if ( nextAvailableCode = maxDictionaryCode ) and ( bitsPerCode < MAX_BITS ) then
        begin
          // expand dictionary
          inc( bitsPerCode );
          maxDictionaryCode:= MaxValNBits( bitsPerCode );
        end;
      end;

      lastCode:= currentCode;
    end;

    // Read next code from input
    currentCode:= input_code( pbInput, number_bytes );
  end;
end;


End.
