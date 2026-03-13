// ============================================================================
// D5-implementation of "US Secure Hash Algorithm 1 (SHA1)" (RFC3174)
// Copyright (c) 2001, Juergen Haible.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.
// ============================================================================

{------------------------------------------------------------------------------
Update by F. Piette for ICS (http://www.overbyte.be)
Jan 10, 2004 Defined uint32_t as Cardinal instead of LongInt
------------------------------------------------------------------------------}


unit Sha1;

{$mode objfpc}{$H+}
{$R-}
{$Q-}

interface

uses SysUtils, Classes;

const
  shaSuccess      = 0;
  shaNull         = 1;
  shaInputTooLong = 2;
  shaStateError   = 3;
  SHA1HashSize    = 20;

type
  uint32_t      = Cardinal; // unsigned 32 bit integer
  uint8_t       = Byte;     // unsigned 8 bit integer (i.e., unsigned char)
  int_least16_t = LongInt;  // integer of >= 16 bits

  SHA1Digest = array[0..SHA1HashSize-1] of Char;
  SHA1DigestString = AnsiString;  // string containing 20 chars

  // This structure will hold context information for the SHA-1
  // hashing operation
  SHA1Context = record
    Intermediate_Hash: array[0..SHA1HashSize div 4-1] of uint32_t; // Message Digest
    Length_Low : uint32_t;                  // Message length in bits
    Length_High: uint32_t;                  // Message length in bits
    Message_Block_Index: int_least16_t;     // Index into message block array
    Message_Block: array[0..63] of uint8_t; // 512-bit message blocks
    Computed: Integer;                      // Is the digest computed?
    Corrupted: Integer;                     // Is the message digest corrupted?
  end;

function SHA1Reset ( var context       : SHA1Context ): Integer;
function SHA1Input ( var context       : SHA1Context;
                     message_array     : PChar;
                     length            : Cardinal ): Integer;
function SHA1Result( var context       : SHA1Context;
                     var Message_Digest: SHA1Digest ): Integer;

function SHA1ofStr   ( const s: String            ): SHA1DigestString;
function SHA1ofBuf   ( const buf; buflen: Integer ): SHA1DigestString;
function SHA1ofStream( const strm: TStream        ): SHA1DigestString;

function SHA1toHex( const digest: SHA1DigestString ): String;

procedure HMAC_SHA1( const Data; DataLen: Integer;
                     const Key;  KeyLen : Integer;
                     out   Digest       : SHA1Digest );
function  HMAC_SHA1_EX( const Data: String;
                     const Key : String ): String; //overload;

implementation

// Define the SHA1 circular left shift macro
function SHA1CircularShift( const bits, word: uint32_t ): uint32_t;
begin
   Result := (((word) shl (bits)) or ((word) shr (32-(bits))));
end;

// This function will process the next 512 bits of the message
// stored in the Message_Block array.
procedure SHA1ProcessMessageBlock( var context: SHA1Context );
const K: array[0..3] of uint32_t = (  //* Constants defined in SHA-1   */
                                      $5A827999,
                                      $6ED9EBA1,
                                      $8F1BBCDC,
                                      $CA62C1D6
                                   );
var
    t: Integer;                  //* Loop counter                */
    temp: uint32_t;              //* Temporary word value        */
    W: array[0..79] of uint32_t; //* Word sequence               */
    A, B, C, D, E: uint32_t;     //* Word buffers                */
begin

    // Initialize the first 16 words in the array W
    for t := 0 to 15 do begin
        W[t] := context.Message_Block[t * 4    ] shl 24
             or context.Message_Block[t * 4 + 1] shl 16
             or context.Message_Block[t * 4 + 2] shl 8
             or context.Message_Block[t * 4 + 3];
    end;

    for t := 16 to 79 do begin
       W[t] := SHA1CircularShift(1,W[t-3] xor W[t-8] xor W[t-14] xor W[t-16]);
    end;

    A := context.Intermediate_Hash[0];
    B := context.Intermediate_Hash[1];
    C := context.Intermediate_Hash[2];
    D := context.Intermediate_Hash[3];
    E := context.Intermediate_Hash[4];

    for t := 0 to 19 do begin
        temp :=  SHA1CircularShift(5,A) +
                 ((B and C) or ((not B) and D)) + E + W[t] + K[0];
        E := D;
        D := C;
        C := SHA1CircularShift(30,B);
        B := A;
        A := temp;
    end;

    for t := 20 to 39 do begin
        temp := SHA1CircularShift(5,A) + (B xor C xor D) + E + W[t] + K[1];
        E := D;
        D := C;
        C := SHA1CircularShift(30,B);
        B := A;
        A := temp;
    end;

    for t := 40 to 59 do begin
        temp := SHA1CircularShift(5,A) +
                ((B and C) or (B and D) or (C and D)) + E + W[t] + K[2];
        E := D;
        D := C;
        C := SHA1CircularShift(30,B);
        B := A;
        A := temp;
    end;

    for t := 60 to 79 do begin
        temp := SHA1CircularShift(5,A) + (B xor C xor D) + E + W[t] + K[3];
        E := D;
        D := C;
        C := SHA1CircularShift(30,B);
        B := A;
        A := temp;
    end;

    inc( context.Intermediate_Hash[0], A );
    inc( context.Intermediate_Hash[1], B );
    inc( context.Intermediate_Hash[2], C );
    inc( context.Intermediate_Hash[3], D );
    inc( context.Intermediate_Hash[4], E );

    context.Message_Block_Index := 0;
end;

// According to the standard, the message must be padded to an even
// 512 bits.  The first padding bit must be a '1'.  The last 64
// bits represent the length of the original message.  All bits in
// between should be 0.  This function will pad the message
// according to those rules by filling the Message_Block array
// accordingly.  It will also call the ProcessMessageBlock function
// provided appropriately.  When it returns, it can be assumed that
// the message digest has been computed.
procedure SHA1PadMessage( var context: SHA1Context );
begin
   (*
    *  Check to see if the current message block is too small to hold
    *  the initial padding bits and length.  If so, we will pad the
    *  block, process it, and then continue padding into a second
    *  block.
    *)
    if (context.Message_Block_Index > 55) then begin
        context.Message_Block[context.Message_Block_Index] := $80;
        inc( context.Message_Block_Index );
        while (context.Message_Block_Index < 64) do begin
            context.Message_Block[context.Message_Block_Index] := 0;
            inc( context.Message_Block_Index );
        end;

        SHA1ProcessMessageBlock( context );

        while (context.Message_Block_Index < 56) do begin
            context.Message_Block[context.Message_Block_Index] := 0;
            inc( context.Message_Block_Index );
        end;
    end else begin
        context.Message_Block[context.Message_Block_Index] := $80;
        inc( context.Message_Block_Index );
        while (context.Message_Block_Index < 56) do begin
            context.Message_Block[context.Message_Block_Index] := 0;
            inc( context.Message_Block_Index );
        end;
    end;

    // Store the message length as the last 8 octets
    context.Message_Block[56] := context.Length_High shr 24;
    context.Message_Block[57] := context.Length_High shr 16;
    context.Message_Block[58] := context.Length_High shr 8;
    context.Message_Block[59] := context.Length_High;
    context.Message_Block[60] := context.Length_Low shr 24;
    context.Message_Block[61] := context.Length_Low shr 16;
    context.Message_Block[62] := context.Length_Low shr 8;
    context.Message_Block[63] := context.Length_Low;

    SHA1ProcessMessageBlock(context);
end;

// This function will initialize the SHA1Context in preparation
// for computing a new SHA1 message digest.
function SHA1Reset( var context: SHA1Context ): Integer;
begin
   // if (context=0) then begin Result:=shaNull; exit end;

   context.Length_Low           := 0;
   context.Length_High          := 0;
   context.Message_Block_Index  := 0;

   context.Intermediate_Hash[0] := $67452301;
   context.Intermediate_Hash[1] := $EFCDAB89;
   context.Intermediate_Hash[2] := $98BADCFE;
   context.Intermediate_Hash[3] := $10325476;
   context.Intermediate_Hash[4] := $C3D2E1F0;

   context.Computed   := 0;
   context.Corrupted  := 0;

   Result := shaSuccess;
end;

// This function will return the 160-bit message digest into the
// Message_Digest array  provided by the caller.
function SHA1Result( var context: SHA1Context;
                     var Message_Digest: SHA1Digest ): Integer;
var  i: Integer;
begin
   // if (!context || !Message_Digest) then begin Result:=shaNull; exit end;

   if (context.Corrupted<>0) then begin Result:=context.Corrupted; exit end;

   if (context.Computed=0) then begin
      SHA1PadMessage( context );
      for i:=0 to 63 do begin
          //* message may be sensitive, clear it out */
          context.Message_Block[i] := 0;
      end;
      context.Length_Low  := 0;    //* and clear length */
      context.Length_High := 0;
      context.Computed := 1;
   end;

   for i := 0 to SHA1HashSize-1 do begin
        Message_Digest[i] := chr( context.Intermediate_Hash[i shr 2]
                             shr ( 8 * ( 3 - ( uint32_t(i) and $03 ) ) ) );
   end;

   Result := shaSuccess;
end;

// This function accepts an array of octets as the next portion
// of the message.
function SHA1Input( var context: SHA1Context;
                    message_array: PChar;
                    length: Cardinal ): Integer;
begin
    if (length=0) then begin Result:=shaSuccess; exit end;
    // if (!context || !message_array) then begin Result:=shaNull; exit end;
    if (message_array=nil) then begin Result:=shaNull; exit end;

    if (context.Computed<>0) then begin
        context.Corrupted := shaStateError;
        Result := shaStateError;
        exit;
    end;

    if (context.Corrupted<>0) then begin
         Result := context.Corrupted;
         exit;
    end;

    while (length>0) and (context.Corrupted=0) do begin
       context.Message_Block[context.Message_Block_Index] := (ord(message_array^) and $FF);
       inc( context.Message_Block_Index );

       inc( context.Length_Low, 8 );
       if (context.Length_Low = 0) then begin
           inc( context.Length_High );
           if (context.Length_High = 0) then begin
               // Message is too long
               context.Corrupted := 1;
           end;
       end;

       if (context.Message_Block_Index = 64) then begin
           SHA1ProcessMessageBlock(context);
       end;

       inc( message_array );
       dec( length );
    end;

    Result := shaSuccess;
end;

// ----------------------------------------------------------------------------

// returns SHA1 digest of given string
function SHA1ofStr( const s: String ): SHA1DigestString;
var  context: SHA1Context;
     digest : SHA1Digest;
begin
   SHA1Reset ( context);
   SHA1Input ( context, PChar( @s[1] ), length(s) );
   SHA1Result( context, digest );
   SetLength( Result, sizeof(digest) );
   Move( digest, Result[1], sizeof(digest) );
end;


// returns SHA1 digest of given buffer
function SHA1ofBuf( const buf; buflen: Integer ): SHA1DigestString;
var  context: SHA1Context;
     digest : SHA1Digest;
begin
   SHA1Reset ( context);
   SHA1Input ( context, PChar( buf ), buflen );
   SHA1Result( context, digest );
   SetLength( Result, sizeof(digest) );
   Move( digest, Result[1], sizeof(digest) );
end;


// returns SHA1 digest of given stream
function SHA1ofStream( const strm: TStream ): SHA1DigestString;
var  context: SHA1Context;
     digest : SHA1Digest;
     buf: array[0..4095] of char;
     buflen: Integer;
begin
   SHA1Reset ( context);
   strm.Position := 0;
   repeat
      buflen := strm.Read( buf[0], 4096 );
      if buflen>0 then SHA1Input ( context, buf, buflen );
   until buflen<4096;
   SHA1Result( context, digest );
   SetLength( Result, sizeof(digest) );
   Move( digest, Result[1], sizeof(digest) );
end;


// converts SHA1 digest into a hex-string

function SHA1toHex( const digest: SHA1DigestString ): String;
var  i: Integer;
begin
   Result := '';
   for i:=1 to length(digest) do Result := Result + inttohex( ord( digest[i] ), 2 );
   Result := LowerCase( Result );
end;

// ----------------------------------------------------------------------------

// Keyed SHA1 (HMAC-SHA1), RFC 2104


procedure HMAC_SHA1(const Data; DataLen: Integer;
                    const Key;  KeyLen : Integer;
                    out   Digest       : SHA1Digest );
var  k_ipad, k_opad: array[0..64] of Byte;
     Context: SHA1Context;
     i      : Integer;
begin
   // clear pads
   FillChar( k_ipad, sizeof(k_ipad), 0 );
   FillChar( k_opad, sizeof(k_ipad), 0 );

   if KeyLen > 64 then begin
    // if key is longer than 64 bytes reset it to key=SHA1(key)
    SHA1Reset ( Context);
    SHA1Input ( Context, PChar(@Key), KeyLen );
    SHA1Result( Context, Digest );
    // store key in pads
    Move( Digest, k_ipad, SHA1HashSize );
    Move( Digest, k_opad, SHA1HashSize );
   end else begin
    // store key in pads
    Move( Key, k_ipad, KeyLen );
    Move( Key, k_opad, KeyLen );
   end;

   // XOR key with ipad and opad values
   for i:=0 to 63 do begin
   k_ipad[i] := k_ipad[i] xor $36;
   k_opad[i] := k_opad[i] xor $5c;
   end;

   // perform inner SHA1
   SHA1Reset ( Context );
   SHA1Input ( Context, PChar(@k_ipad[0]), 64 );
   SHA1Input ( Context, PChar(@Data), DataLen );
   SHA1Result( Context, Digest );

   // perform outer SHA1
   SHA1Reset ( Context );
   SHA1Input ( Context, PChar(@k_opad[0]), 64 );
   SHA1Input ( Context, Digest, SHA1HashSize );
   SHA1Result( Context, Digest );
end;

function HMAC_SHA1_EX( const Data: String;
                       const Key : String ): String;
var  Digest: SHA1Digest;
begin
   HMAC_SHA1( Data[1], length(Data), Key[1], length(Key), Digest );
   SetLength( Result, SHA1HashSize );
   Move( digest[0], Result[1], SHA1HashSize );
end;

// ----------------------------------------------------------------------------

{
SHA1 test suit:
procedure TForm1.Button1Click(Sender: TObject);
const TEST1   = 'abc';
      TEST2a  = 'abcdbcdecdefdefgefghfghighijhi';
      TEST2b  = 'jkijkljklmklmnlmnomnopnopq';
      TEST2   = TEST2a + TEST2b;
      TEST3   = 'a';
      TEST4a  = '01234567012345670123456701234567';
      TEST4b  = '01234567012345670123456701234567';
      TEST4   = TEST4a + TEST4b;
      testarray: array[0..3] of String = ( TEST1, TEST2, TEST3, TEST4 );
      repeatcount: array[0..3] of Integer = ( 1, 1, 1000000, 10 );
      resultarray: array [0..3] of String = (
             'A9 99 3E 36 47 06 81 6A BA 3E 25 71 78 50 C2 6C 9C D0 D8 9D',
             '84 98 3E 44 1C 3B D2 6E BA AE 4A A1 F9 51 29 E5 E5 46 70 F1',
             '34 AA 97 3C D4 C4 DA A4 F6 1E EB 2B DB AD 27 31 65 34 01 6F',
             'DE A3 56 A2 CD DD 90 C7 A7 EC ED C5 EB B5 63 93 4F 46 04 52' );
var   sha: SHA1Context;
      i, j, err: Integer;
      Message_Digest: SHA1Digest;
      s: String;
begin
    for j := 0 to 3 do begin
        ListBox1.Items.Add( Format( 'Test %d: %d, "%s"',
                            [ j+1, repeatcount[j], testarray[j] ] ) );

        err := SHA1Reset(sha);
        if (err<>0) then begin
            ListBox1.Items.Add( Format( 'SHA1Reset Error %d.', [err] ) );
            break;    //* out of for j loop */
        end;

        for i := 0 to repeatcount[j]-1 do begin
            err := SHA1Input( sha, @testarray[j][1], length(testarray[j]) );
            if (err<>0) then begin
               ListBox1.Items.Add( Format('SHA1Input Error %d.', [err] ) );
               break;    //* out of for i loop */
            end;
        end;

        err := SHA1Result(sha, Message_Digest);
        if (err<>0) then begin
            ListBox1.Items.Add( Format(
            'SHA1Result Error %d, could not compute message digest.', [err] ) );
        end else begin
          s := '';
          for i := 0 to 19 do begin
          s := s + Format('%02X ', [ ord(Message_Digest[i]) ] );
        end;
    ListBox1.Items.Add( 'Result: ' + s );
    end;

    ListBox1.Items.Add( 'Wanted: ' + Format('%s', [resultarray[j]] ) );
    end;
end;

HMAC-SHA1 test suite of RFC 2202:
procedure TForm1.Button3Click(Sender: TObject);
end;
}

end.

