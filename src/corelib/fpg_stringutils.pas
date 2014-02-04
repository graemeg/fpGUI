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
      Some handly UTF8 functions copied from the Lazarus LCL. Comes from the
      LCLProc unit. Surely we can move this into FPC?
}

unit fpg_stringutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base;

type
  TCharToUTF8Table = array[char] of PChar;


function  UTF8CharacterLength(p: PChar): integer;
function  UTF8CharToUnicode(p: PChar; out CharLen: longint): Cardinal;
function  UTF8CharStart(UTF8Str: PChar; Len, Index: PtrInt): PChar;
function  UTF8Copy(const s: string; StartCharIndex, CharCount: PtrInt): string;
function  UTF8CStringToUTF8String(SourceStart: PChar; SourceLen: PtrInt): string;
function  UTF8Length(const s: string): PtrInt;
function  UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;
function  UTF8Pos(const SearchForText, SearchInText: string): PtrInt;
procedure UTF8Delete(var S: string; Index, Size: PtrInt);
procedure UTF8Insert(const SubStr: string; var AText: string; Index: PtrInt);
function  UTF8CharAtByte(const s: string; const BytePos: PtrInt; out aChar: string): PtrInt;


// short form (alias or convenience) functions for the UTF8 ones above
function  Copy8(const s: string; StartCharIndex, CharCount: PtrInt): string;
function  Length8(const s: string): PtrInt;
function  Pos8(const SearchForText, SearchInText: string): PtrInt;
procedure Delete8(var S: string; Index, Size: PtrInt);
procedure Insert8(const Source: string; var S: string; Index: PtrInt);

function  fpgCharAt(const s: TfpgString; Index: PtrInt): TfpgChar;
function  fpgAppendPathDelim(const Path: TfpgString): TfpgString;
function  fpgRemovePathDelim(const Path: TfpgString): TfpgString;
function  fpgTrimR(const AString, ATrim: TfpgString; ACaseSensitive: boolean = false): TfpgString;


// Encoding conversions
function CP437ToUTF8(const s: string): TfpgString;  // DOS central europe
function CP850ToUTF8(const s: string): TfpgString;  // DOS western europe
function CP866ToUTF8(const s: string): TfpgString;  // DOS and Windows console's cyrillic
function CP1250ToUTF8(const s: string): TfpgString; // central europe
function IBMGraphToUTF8(const s: string): TfpgString;  // IBM PC / DOS  http://www.unicode.org/Public/MAPPINGS/VENDORS/MISC/IBMGRAPH.TXT
function IPFToUTF8(const s: string): TfpgString; // minor replacements to improve DocView output
function SingleByteToUTF8(const s: string; const Table: TCharToUTF8Table): TfpgString;

function ConvertTextToUTF8(const AEncoding: TfpgTextEncoding; const AText: AnsiString): TfpgString;


implementation


{ If it's a multibyte character, the first byte specifies the amount of bytes
  used. All subsequent bytes will start with 10xxxxxx. It is assumed that p
  points to the beginning of a single or multibyte character. }
function UTF8CharacterLength(p: PChar): integer;
begin
  if p <> nil then
  begin
    if ord(p^) < %11000000 then  //  00000000-01111111
    begin
      // regular single byte character (#0 is a character, this is pascal ;)
      Result := 1;
    end
    else if ((ord(p^) and %11100000) = %11000000) then  //  11000010-11011111
    begin
      // could be 2 byte character
      if (ord(p[1]) and %11000000) = %10000000 then
        Result := 2
      else
        Result := 1;
    end
    else if ((ord(p^) and %11110000) = %11100000) then  //  11100000-11101111
    begin
      // could be 3 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
          and ((ord(p[2]) and %11000000) = %10000000) then
        Result := 3
      else
        Result := 1;
    end
    else if ((ord(p^) and %11111000) = %11110000) then  //  11110000-11110100
    begin
      // could be 4 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
          and ((ord(p[2]) and %11000000) = %10000000)
          and ((ord(p[3]) and %11000000) = %10000000) then
        Result := 4
      else
        Result := 1;
    end
    else
      Result := 1;
  end
  else
    Result := 0;
end;

function UTF8CharToUnicode(p: PChar; out CharLen: longint): Cardinal;
begin
  if p=nil then begin
    Result:=0;
    CharLen:=0;
    exit;
  end;
  if ord(p^) < %11000000 then begin
    // regular single byte character (#0 is a normal char, this is pascal ;)
  end
  else if ((ord(p^) and %11100000) = %11000000) then begin
    // could be double byte character
    if (ord(p[1]) and %11000000) = %10000000 then begin
      Result:=((ord(p^) and %00011111) shl 6)
              or (ord(p[1]) and %00111111);
      CharLen:=2;
      exit;
    end;
  end
  else if ((ord(p^) and %11110000) = %11100000) then begin
    // could be triple byte character
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000) then begin
      Result:=((ord(p^) and %00011111) shl 12)
              or ((ord(p[1]) and %00111111) shl 6)
              or (ord(p[2]) and %00111111);
      CharLen:=3;
      exit;
    end;
  end
  else if ((ord(p^) and %11111000) = %11110000) then begin
    // could be 4 byte character
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000)
    and ((ord(p[3]) and %11000000) = %10000000) then begin
      Result:=((ord(p^) and %00001111) shl 18)
              or ((ord(p[1]) and %00111111) shl 12)
              or ((ord(p[2]) and %00111111) shl 6)
              or (ord(p[3]) and %00111111);
      CharLen:=4;
      exit;
    end;
  end
  else begin
    // invalid character
  end;
  Result:=ord(p^);
  CharLen:=1;
end;


{ Returns the character starting position as PChar in the UTF8Str string. }
function UTF8CharStart(UTF8Str: PChar; Len, Index: PtrInt): PChar;
var
  CharLen: LongInt;
begin
  Result := UTF8Str;
  if Result <> nil then
  begin
    while (Index > 0) and (Len > 0) do
    begin
      CharLen := UTF8CharacterLength(Result);
      dec(Len, CharLen);
      dec(Index);
      inc(Result, CharLen);
    end;
    if (Index > 0) or (Len < 0) then
      Result := nil;
  end;
end;

// returns substring
function UTF8Copy(const s: string; StartCharIndex, CharCount: PtrInt): string;
var
  StartBytePos: PChar;
  EndBytePos: PChar;
  MaxBytes: PtrInt;
begin
  Result := '';
  // Some sanity checks
  if (Length(s) = 0) then
    Exit; //==>
  if CharCount = 0 then
    Exit; //==>
    
  StartBytePos := UTF8CharStart(PChar(s),length(s),StartCharIndex-1);
  if StartBytePos = nil then
    Result := ''
  else
  begin
    MaxBytes := PtrInt(PChar(s)+length(s)-StartBytePos);
    EndBytePos := UTF8CharStart(StartBytePos,MaxBytes,CharCount);
    if EndBytePos = nil then
      Result := copy(s,StartBytePos-PChar(s)+1,MaxBytes)
    else
      Result := copy(s,StartBytePos-PChar(s)+1,EndBytePos-StartBytePos);
  end;
end;

function UTF8CStringToUTF8String(SourceStart: PChar; SourceLen: PtrInt): string;
var
  Source: PChar;
  Dest: PChar;
  SourceEnd: PChar;
  SourceCopied: PChar;

  // Copies from SourceStart till Source to Dest and updates Dest
  procedure CopyPart; inline;
  var
    CopyLength: SizeInt;
  begin
    CopyLength := Source - SourceCopied;
    if CopyLength=0 then exit;
    move(SourceCopied^ , Dest^, CopyLength);
    SourceCopied:=Source;
    inc(Dest, CopyLength);
  end;

begin
  SetLength(Result, SourceLen);
  if SourceLen=0 then
    Exit; //==>
  SourceCopied:=SourceStart;
  Source:=SourceStart;
  Dest:=PChar(Result);
  SourceEnd := Source + SourceLen;
  while Source<SourceEnd do
  begin
    if (Source^='\') then
    begin
      CopyPart;
      inc(Source);
      if Source^ in ['t', 'n', '"', '\'] then
      begin
        case Source^ of
         't' : Dest^ := #9;
         '"' : Dest^ := '"';
         '\' : Dest^ := '\';
         'n' :
         // fpc 2.1.1 stores string constants as array of char so maybe this
         // will work for without ifdef (once available in 2.0.x too):
         // move(lineending, dest^, sizeof(LineEnding));
{$IFDEF WINDOWS}
               begin
                 move(lineending[1], dest^, length(LineEnding));
                 inc(dest^, length(LineEnding)-1);
               end;
{$ELSE}
               Dest^ := LineEnding;
{$ENDIF}
        end;
        inc(Source);
        inc(Dest);
      end;
      SourceCopied := Source;
    end
    else
      Inc(Source, UTF8CharacterLength(Source));
  end;
  CopyPart;
  SetLength(Result, Dest - PChar(Result));
end;

function UTF8Length(const s: string): PtrInt;
begin
  if s = '' then
    Result := 0
  else
    Result := UTF8Length(PChar(s),length(s));
end;

function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;
var
  CharLen: Integer;
begin
  Result := 0;
  while (ByteCount > 0) do
  begin
    inc(Result);
    CharLen := UTF8CharacterLength(p);
    inc(p, CharLen);
    dec(ByteCount, CharLen);
  end;
end;

// returns the character index, where the SearchForText starts in SearchInText
function UTF8Pos(const SearchForText, SearchInText: string): PtrInt;
var
 p: SizeInt;
begin
  p := System.Pos(SearchForText, SearchInText);
  if p > 0 then
    Result := UTF8Length(PChar(SearchInText), p-1) + 1
  else
    Result := 0;
end;

procedure UTF8Delete(var S: string; Index, Size: PtrInt);
var
  ls: PtrInt;
  b: string;
  e: string;
begin
  ls := UTF8Length(S);
  if (Index > ls) or (Index <= 0) or (Size <= 0) then
    Exit; //==>
  b := UTF8Copy(S, 1, Index-1); // beginning string
  e := UTF8Copy(S, Index+Size, UTF8Length(S)-(Index+Size-1)); // ending string
  S := b + e;
end;

procedure UTF8Insert(const SubStr: string; var AText: string; Index: PtrInt);
var
  b: string;
  e: string;
begin
  if Length(SubStr) = 0 then
    Exit; //==>
  b := UTF8Copy(AText, 1, Index-1); // beginning string
  e := UTF8Copy(AText, Index, UTF8Length(AText)-Index+1); // ending string
  AText := b + SubStr + e;
end;

function UTF8CharAtByte(const s: string; const BytePos: PtrInt; out aChar: string): PtrInt;
var
  CharLen: Integer;
begin
  if BytePos > 0 then
  begin
    {$R-}  { Disable range-check because we do it manually above }
    CharLen := UTF8CharacterLength(@s[BytePos]);
    {$R+}
    aChar   := Copy(s, BytePos, CharLen);
    Result  := BytePos + CharLen;
  end else
  begin
    aChar   := '';
    Result  := 1;
  end;
end;

function Copy8(const s: string; StartCharIndex, CharCount: PtrInt): string;
begin
  Result := UTF8Copy(s, StartCharIndex, CharCount);
end;

function Length8(const s: string): PtrInt;
begin
  Result := UTF8Length(s);
end;

function Pos8(const SearchForText, SearchInText: string): PtrInt;
begin
  Result := UTF8Pos(SearchForText, SearchInText);
end;

procedure Delete8(var S: string; Index, Size: PtrInt);
begin
  UTF8Delete(S, Index, Size);
end;

procedure Insert8(const Source: string; var S: string; Index: PtrInt);
begin
  UTF8Insert(Source, S, Index);
end;

function fpgCharAt(const s: TfpgString; Index: PtrInt): TfpgChar;
begin
  Result := UTF8Copy(s, Index, 1);
end;

function fpgAppendPathDelim(const Path: TfpgString): TfpgString;
begin
  if (Path <> '') and (Path[Length(Path)] <> PathDelim) then
    Result := Path + PathDelim
  else
    Result := Path;
end;

function fpgRemovePathDelim(const Path: TfpgString): TfpgString;
begin
  if (Path <> '') and (Path[Length(Path)] = PathDelim) then
    Result := LeftStr(Path, Length(Path)-1)
  else
    Result := Path;
end;

function fpgTrimR(const AString, ATrim: TfpgString; ACaseSensitive: boolean): TfpgString;
var
  li: integer;
begin
  if ACaseSensitive then
    li := UTF8Pos(ATrim, AString)
  else
    li := UTF8Pos(UpperCase(ATrim), UpperCase(AString));

  if li <> 0 then
    Result := UTF8Copy(AString, 1, li - 1)
  else
    Result := AString;
end;

const
  ArrayCP437ToUTF8 : TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #$E2#$80#$A2,       // #7  bullet
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    '>',                // #16   right arrow head
    '<',                // #17   left arrow head
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #$E2#$86#$91,       // #24  up arrow
    #$E2#$86#$93,       // #25  down arrow
    #$E2#$86#$92,       // #26  right arrow
    #$E2#$86#$90,       // #27  left arrow
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #$E2#$8C#$82,       // #127   House
    #195#135,           // #128
    #195#188,           // #129
    #195#169,           // #130
    #195#162,           // #131
    #195#164,           // #132
    #195#160,           // #133
    #195#165,           // #134
    #195#167,           // #135
    #195#170,           // #136
    #195#171,           // #137
    #195#168,           // #138
    #195#175,           // #139
    #195#174,           // #140
    #195#172,           // #141
    #195#132,           // #142
    #195#133,           // #143
    #195#137,           // #144
    #195#166,           // #145
    #195#134,           // #146
    #195#180,           // #147
    #195#182,           // #148
    #195#178,           // #149
    #195#187,           // #150
    #195#185,           // #151
    #195#191,           // #152
    #195#150,           // #153
    #195#156,           // #154
    #194#162,           // #155
    #194#163,           // #156
    #194#165,           // #157
    #226#130#167,       // #158
    #198#146,           // #159
    #195#161,           // #160
    #195#173,           // #161
    #195#179,           // #162
    #195#186,           // #163
    #195#177,           // #164
    #195#145,           // #165
    #194#170,           // #166
    #194#186,           // #167
    #194#191,           // #168
    #226#140#144,       // #169
    #194#172,           // #170
    #194#189,           // #171
    #194#188,           // #172
    #194#161,           // #173
    #194#171,           // #174
    #194#187,           // #175
    #226#150#145,       // #176
    #226#150#146,       // #177
    #226#150#147,       // #178
    #226#148#130,       // #179
    #226#148#164,       // #180
    #226#149#161,       // #181
    #226#149#162,       // #182
    #226#149#150,       // #183
    #226#149#149,       // #184
    #226#149#163,       // #185
    #226#149#145,       // #186
    #226#149#151,       // #187
    #226#149#157,       // #188
    #226#149#156,       // #189
    #226#149#155,       // #190
    #226#148#144,       // #191
    #226#148#148,       // #192
    #226#148#180,       // #193
    #226#148#172,       // #194
    #226#148#156,       // #195
    #226#148#128,       // #196
    #226#148#188,       // #197
    #226#149#158,       // #198
    #226#149#159,       // #199
    #226#149#154,       // #200
    #226#149#148,       // #201
    #226#149#169,       // #202
    #226#149#166,       // #203
    #226#149#160,       // #204
    #226#149#144,       // #205
    #226#149#172,       // #206
    #226#149#167,       // #207
    #226#149#168,       // #208
    #226#149#164,       // #209
    #226#149#165,       // #210
    #226#149#153,       // #211
    #226#149#152,       // #212
    #226#149#146,       // #213
    #226#149#147,       // #214
    #226#149#171,       // #215
    #226#149#170,       // #216
    #226#148#152,       // #217
    #226#148#140,       // #218
    #226#150#136,       // #219
    #226#150#132,       // #220
    #226#150#140,       // #221
    #226#150#144,       // #222
    #226#150#128,       // #223
    #206#177,           // #224
    #195#159,           // #225
    #206#147,           // #226
    #207#128,           // #227
    #206#163,           // #228
    #207#131,           // #229
    #194#181,           // #230
    #207#132,           // #231
    #206#166,           // #232
    #206#152,           // #233
    #206#169,           // #234
    #206#180,           // #235
    #226#136#158,       // #236
    #207#134,           // #237
    #206#181,           // #238
    #226#136#169,       // #239
    #226#137#161,       // #240
    #194#177,           // #241
    #226#137#165,       // #242
    #226#137#164,       // #243
    #226#140#160,       // #244
    #226#140#161,       // #245
    #195#183,           // #246
    #226#137#136,       // #247
    #194#176,           // #248
    #226#136#153,       // #249
    #194#183,           // #250
    #226#136#154,       // #251
    #226#129#191,       // #252
    #194#178,           // #253
    #226#150#160,       // #254
    #194#160            // #255
  );

  ArrayCP850ToUTF8 : TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #$E2#$80#$A2,       // #7  bullet
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #195#135,           // #128
    #195#188,           // #129
    #195#169,           // #130
    #195#162,           // #131
    #195#164,           // #132
    #195#160,           // #133
    #195#165,           // #134
    #195#167,           // #135
    #195#170,           // #136
    #195#171,           // #137
    #195#168,           // #138
    #195#175,           // #139
    #195#174,           // #140
    #195#172,           // #141
    #195#132,           // #142
    #195#133,           // #143
    #195#137,           // #144
    #195#166,           // #145
    #195#134,           // #146
    #195#180,           // #147
    #195#182,           // #148
    #195#178,           // #149
    #195#187,           // #150
    #195#185,           // #151
    #195#191,           // #152
    #195#150,           // #153
    #195#156,           // #154
    #195#184,           // #155
    #194#163,           // #156
    #195#152,           // #157
    #195#151,           // #158
    #198#146,           // #159
    #195#161,           // #160
    #195#173,           // #161
    #195#179,           // #162
    #195#186,           // #163
    #195#177,           // #164
    #195#145,           // #165
    #194#170,           // #166
    #194#186,           // #167
    #194#191,           // #168
    #194#174,           // #169
    #194#172,           // #170
    #194#189,           // #171
    #194#188,           // #172
    #194#161,           // #173
    #194#171,           // #174
    #194#187,           // #175
    #226#150#145,       // #176
    #226#150#146,       // #177
    #226#150#147,       // #178
    #226#148#130,       // #179
    #226#148#164,       // #180
    #195#129,           // #181
    #195#130,           // #182
    #195#128,           // #183
    #194#169,           // #184
    #226#149#163,       // #185
    #226#149#145,       // #186
    #226#149#151,       // #187
    #226#149#157,       // #188
    #194#162,           // #189
    #194#165,           // #190
    #226#148#144,       // #191
    #226#148#148,       // #192
    #226#148#180,       // #193
    #226#148#172,       // #194
    #226#148#156,       // #195
    #226#148#128,       // #196
    #226#148#188,       // #197
    #195#163,           // #198
    #195#131,           // #199
    #226#149#154,       // #200
    #226#149#148,       // #201
    #226#149#169,       // #202
    #226#149#166,       // #203
    #226#149#160,       // #204
    #226#149#144,       // #205
    #226#149#172,       // #206
    #194#164,           // #207
    #195#176,           // #208
    #195#144,           // #209
    #195#138,           // #210
    #195#139,           // #211
    #195#136,           // #212
    #196#177,           // #213
    #195#141,           // #214
    #195#142,           // #215
    #195#143,           // #216
    #226#148#152,       // #217
    #226#148#140,       // #218
    #226#150#136,       // #219
    #226#150#132,       // #220
    #194#166,           // #221
    #195#140,           // #222
    #226#150#128,       // #223
    #195#147,           // #224
    #195#159,           // #225
    #195#148,           // #226
    #195#146,           // #227
    #195#181,           // #228
    #195#149,           // #229
    #194#181,           // #230
    #195#190,           // #231
    #195#158,           // #232
    #195#154,           // #233
    #195#155,           // #234
    #195#153,           // #235
    #195#189,           // #236
    #195#157,           // #237
    #194#175,           // #238
    #194#180,           // #239
    #194#173,           // #240
    #194#177,           // #241
    #226#128#151,       // #242
    #194#190,           // #243
    #194#182,           // #244
    #194#167,           // #245
    #195#183,           // #246
    #194#184,           // #247
    #194#176,           // #248
    #194#168,           // #249
    #194#183,           // #250
    #194#185,           // #251
    #194#179,           // #252
    #194#178,           // #253
    #226#150#160,       // #254
    #194#160            // #255
  );


  ArrayCP866ToUTF8 : TCharToUTF8Table = (
    #0,                 //#0
    #1,                 //#1
    #2,                 //#2
    #3,                 //#3
    #4,                 //#4
    #5,                 //#5
    #6,                 //#6
    #7,                 //#7
    #8,                 //#8
    #9,                 //#9
    #10,                //#10
    #11,                //#11
    #12,                //#12
    #13,                //#13
    #14,                //#14
    #15,                //#15
    #16,                //#16
    #17,                //#17
    #18,                //#18
    #19,                //#19
    #20,                //#20
    #21,                //#21
    #22,                //#22
    #23,                //#23
    #24,                //#24
    #25,                //#25
    #26,                //#26
    #27,                //#27
    #28,                //#28
    #29,                //#29
    #30,                //#30
    #31,                //#31
    #32,                //#32
    #33,                //#33
    #34,                //#34
    #35,                //#35
    #36,                //#36
    #37,                //#37
    #38,                //#38
    #39,                //#39
    #40,                //#40
    #41,                //#41
    #42,                //#42
    #43,                //#43
    #44,                //#44
    #45,                //#45
    #46,                //#46
    #47,                //#47
    #48,                //#48
    #49,                //#49
    #50,                //#50
    #51,                //#51
    #52,                //#52
    #53,                //#53
    #54,                //#54
    #55,                //#55
    #56,                //#56
    #57,                //#57
    #58,                //#58
    #59,                //#59
    #60,                //#60
    #61,                //#61
    #62,                //#62
    #63,                //#63
    #64,                //#64
    #65,                //#65
    #66,                //#66
    #67,                //#67
    #68,                //#68
    #69,                //#69
    #70,                //#70
    #71,                //#71
    #72,                //#72
    #73,                //#73
    #74,                //#74
    #75,                //#75
    #76,                //#76
    #77,                //#77
    #78,                //#78
    #79,                //#79
    #80,                //#80
    #81,                //#81
    #82,                //#82
    #83,                //#83
    #84,                //#84
    #85,                //#85
    #86,                //#86
    #87,                //#87
    #88,                //#88
    #89,                //#89
    #90,                //#90
    #91,                //#91
    #92,                //#92
    #93,                //#93
    #94,                //#94
    #95,                //#95
    #96,                //#96
    #97,                //#97
    #98,                //#98
    #99,                //#99
    #100,               //#100
    #101,               //#101
    #102,               //#102
    #103,               //#103
    #104,               //#104
    #105,               //#105
    #106,               //#106
    #107,               //#107
    #108,               //#108
    #109,               //#109
    #110,               //#110
    #111,               //#111
    #112,               //#112
    #113,               //#113
    #114,               //#114
    #115,               //#115
    #116,               //#116
    #117,               //#117
    #118,               //#118
    #119,               //#119
    #120,               //#120
    #121,               //#121
    #122,               //#122
    #123,               //#123
    #124,               //#124
    #125,               //#125
    #126,               //#126
    #127,               //#127
    #208#144,           //#128
    #208#145,           //#129
    #208#146,           //#130
    #208#147,           //#131
    #208#148,           //#132
    #208#149,           //#133
    #208#150,           //#134
    #208#151,           //#135
    #208#152,           //#136
    #208#153,           //#137
    #208#154,           //#138
    #208#155,           //#139
    #208#156,           //#140
    #208#157,           //#141
    #208#158,           //#142
    #208#159,           //#143
    #208#160,           //#144
    #208#161,           //#145
    #208#162,           //#146
    #208#163,           //#147
    #208#164,           //#148
    #208#165,           //#149
    #208#166,           //#150
    #208#167,           //#151
    #208#168,           //#152
    #208#169,           //#153
    #208#170,           //#154
    #208#171,           //#155
    #208#172,           //#156
    #208#173,           //#157
    #208#174,           //#158
    #208#175,           //#159
    #208#176,           //#160
    #208#177,           //#161
    #208#178,           //#162
    #208#179,           //#163
    #208#180,           //#164
    #208#181,           //#165
    #208#182,           //#166
    #208#183,           //#167
    #208#184,           //#168
    #208#185,           //#169
    #208#186,           //#170
    #208#187,           //#171
    #208#188,           //#172
    #208#189,           //#173
    #208#190,           //#174
    #208#191,           //#175
    #226#150#145,       //#176
    #226#150#146,       //#177
    #226#150#147,       //#178
    #226#148#130,       //#179
    #226#148#164,       //#180
    #226#149#161,       //#181
    #226#149#162,       //#182
    #226#149#150,       //#183
    #226#149#149,       //#184
    #226#149#163,       //#185
    #226#149#145,       //#186
    #226#149#151,       //#187
    #226#149#157,       //#188
    #226#149#156,       //#189
    #226#149#155,       //#190
    #226#148#144,       //#191
    #226#148#148,       //#192
    #226#148#180,       //#193
    #226#148#172,       //#194
    #226#148#156,       //#195
    #226#148#128,       //#196
    #226#148#188,       //#197
    #226#149#158,       //#198
    #226#149#159,       //#199
    #226#149#154,       //#200
    #226#149#148,       //#201
    #226#149#169,       //#202
    #226#149#166,       //#203
    #226#149#160,       //#204
    #226#149#144,       //#205
    #226#149#172,       //#206
    #226#149#167,       //#207
    #226#149#168,       //#208
    #226#149#164,       //#209
    #226#149#165,       //#210
    #226#149#153,       //#211
    #226#149#152,       //#212
    #226#149#146,       //#213
    #226#149#147,       //#214
    #226#149#171,       //#215
    #226#149#170,       //#216
    #226#148#152,       //#217
    #226#148#140,       //#218
    #226#150#136,       //#219
    #226#150#132,       //#220
    #226#150#140,       //#221
    #226#150#144,       //#222
    #226#150#128,       //#223
    #209#128,           //#224
    #209#129,           //#225
    #209#130,           //#226
    #209#131,           //#227
    #209#132,           //#228
    #209#133,           //#229
    #209#134,           //#230
    #209#135,           //#231
    #209#136,           //#232
    #209#137,           //#233
    #209#138,           //#234
    #209#139,           //#235
    #209#140,           //#236
    #209#141,           //#237
    #209#142,           //#238
    #209#143,           //#239
    #208#129,           //#240
    #209#145,           //#241
    #208#132,           //#242
    #209#148,           //#243
    #208#135,           //#244
    #209#151,           //#245
    #208#142,           //#246
    #209#158,           //#247
    #194#176,           //#248
    #226#136#153,       //#249
    #194#183,           //#250
    #226#136#154,       //#251
    #226#132#150,       //#252
    #194#164,           //#253
    #226#150#160,       //#254
    #194#160            //#255
  );


  ArrayCP1250ToUTF8: TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #226#130#172,       // #128
    '',                 // #129
    #226#128#154,       // #130
    '',                 // #131
    #226#128#158,       // #132
    #226#128#166,       // #133
    #226#128#160,       // #134
    #226#128#161,       // #135
    '',                 // #136
    #226#128#176,       // #137
    #197#160,           // #138
    #226#128#185,       // #139
    #197#154,           // #140
    #197#164,           // #141
    #197#189,           // #142
    #197#185,           // #143
    '',                 // #144
    #226#128#152,       // #145
    #226#128#153,       // #146
    #226#128#156,       // #147
    #226#128#157,       // #148
    #226#128#162,       // #149
    #226#128#147,       // #150
    #226#128#148,       // #151
    '',                 // #152
    #226#132#162,       // #153
    #197#161,           // #154
    #226#128#186,       // #155
    #197#155,           // #156
    #197#165,           // #157
    #197#190,           // #158
    #197#186,           // #159
    #194#160,           // #160
    #203#135,           // #161
    #203#152,           // #162
    #197#129,           // #163
    #194#164,           // #164
    #196#132,           // #165
    #194#166,           // #166
    #194#167,           // #167
    #194#168,           // #168
    #194#169,           // #169
    #197#158,           // #170
    #194#171,           // #171
    #194#172,           // #172
    #194#173,           // #173
    #194#174,           // #174
    #197#187,           // #175
    #194#176,           // #176
    #194#177,           // #177
    #203#155,           // #178
    #197#130,           // #179
    #194#180,           // #180
    #194#181,           // #181
    #194#182,           // #182
    #194#183,           // #183
    #194#184,           // #184
    #196#133,           // #185
    #197#159,           // #186
    #194#187,           // #187
    #196#189,           // #188
    #203#157,           // #189
    #196#190,           // #190
    #197#188,           // #191
    #197#148,           // #192
    #195#129,           // #193
    #195#130,           // #194
    #196#130,           // #195
    #195#132,           // #196
    #196#185,           // #197
    #196#134,           // #198
    #195#135,           // #199
    #196#140,           // #200
    #195#137,           // #201
    #196#152,           // #202
    #195#139,           // #203
    #196#154,           // #204
    #195#141,           // #205
    #195#142,           // #206
    #196#142,           // #207
    #196#144,           // #208
    #197#131,           // #209
    #197#135,           // #210
    #195#147,           // #211
    #195#148,           // #212
    #197#144,           // #213
    #195#150,           // #214
    #195#151,           // #215
    #197#152,           // #216
    #197#174,           // #217
    #195#154,           // #218
    #197#176,           // #219
    #195#156,           // #220
    #195#157,           // #221
    #197#162,           // #222
    #195#159,           // #223
    #197#149,           // #224
    #195#161,           // #225
    #195#162,           // #226
    #196#131,           // #227
    #195#164,           // #228
    #196#186,           // #229
    #196#135,           // #230
    #195#167,           // #231
    #196#141,           // #232
    #195#169,           // #233
    #196#153,           // #234
    #195#171,           // #235
    #196#155,           // #236
    #195#173,           // #237
    #195#174,           // #238
    #196#143,           // #239
    #196#145,           // #240
    #197#132,           // #241
    #197#136,           // #242
    #195#179,           // #243
    #195#180,           // #244
    #197#145,           // #245
    #195#182,           // #246
    #195#183,           // #247
    #197#153,           // #248
    #197#175,           // #249
    #195#186,           // #250
    #197#177,           // #251
    #195#188,           // #252
    #195#189,           // #253
    #197#163,           // #254
    #203#153            // #255
  );


  ArrayIBMGraphToUTF8: TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #$E2#$80#$A2,       // #7    bullet
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #$E2#$96#$B8,       // #16   U+25B8 Right arrow head
    #$E2#$96#$82,       // #17   U+25C2 Left arrow head
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #$E2#$86#$91,       // #24  up arrow
    #$E2#$86#$93,       // #25  down arrow
    #$E2#$86#$92,       // #26  right arrow
    #$E2#$86#$90,       // #27  left arrow
    #28,                // #28
    #29,                // #29  left arrow
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #$E2#$8C#$82,       // #127   House
    #195#135,           // #128
    #195#188,           // #129
    #195#169,           // #130
    #195#162,           // #131
    #195#164,           // #132
    #195#160,           // #133
    #195#165,           // #134
    #195#167,           // #135
    #195#170,           // #136
    #195#171,           // #137
    #195#168,           // #138
    #195#175,           // #139
    #195#174,           // #140
    #195#172,           // #141
    #195#132,           // #142
    #195#133,           // #143
    #195#137,           // #144
    #195#166,           // #145
    #195#134,           // #146
    #195#180,           // #147
    #195#182,           // #148
    #195#178,           // #149
    #195#187,           // #150
    #195#185,           // #151
    #195#191,           // #152
    #195#150,           // #153
    #195#156,           // #154
    #194#162,           // #155
    #194#163,           // #156
    #194#165,           // #157
    #226#130#167,       // #158
    #198#146,           // #159
    #195#161,           // #160
    #195#173,           // #161
    #195#179,           // #162
    #195#186,           // #163
    #195#177,           // #164
    #195#145,           // #165
    #194#170,           // #166
    #194#186,           // #167
    #194#191,           // #168
    #226#140#144,       // #169
    #194#172,           // #170
    #194#189,           // #171
    #194#188,           // #172
    #194#161,           // #173
    #194#171,           // #174
    #194#187,           // #175
    #226#150#145,       // #176
    #226#150#146,       // #177
    #226#150#147,       // #178
    #226#148#130,       // #179
    #226#148#164,       // #180
    #226#149#161,       // #181
    #226#149#162,       // #182
    #226#149#150,       // #183
    #226#149#149,       // #184
    #226#149#163,       // #185
    #226#149#145,       // #186
    #226#149#151,       // #187
    #226#149#157,       // #188
    #226#149#156,       // #189
    #226#149#155,       // #190
    #226#148#144,       // #191
    #226#148#148,       // #192
    #226#148#180,       // #193
    #226#148#172,       // #194
    #226#148#156,       // #195
    #226#148#128,       // #196
    #226#148#188,       // #197
    #226#149#158,       // #198
    #226#149#159,       // #199
    #226#149#154,       // #200
    #226#149#148,       // #201
    #226#149#169,       // #202
    #226#149#166,       // #203
    #226#149#160,       // #204
    #226#149#144,       // #205
    #226#149#172,       // #206
    #226#149#167,       // #207
    #226#149#168,       // #208
    #226#149#164,       // #209
    #226#149#165,       // #210
    #226#149#153,       // #211
    #226#149#152,       // #212
    #226#149#146,       // #213
    #226#149#147,       // #214
    #226#149#171,       // #215
    #226#149#170,       // #216
    #226#148#152,       // #217
    #226#148#140,       // #218
    #226#150#136,       // #219
    #226#150#132,       // #220
    #226#150#140,       // #221
    #226#150#144,       // #222
    #226#150#128,       // #223
    #206#177,           // #224
    #195#159,           // #225
    #206#147,           // #226
    #207#128,           // #227
    #206#163,           // #228
    #207#131,           // #229
    #194#181,           // #230
    #207#132,           // #231
    #206#166,           // #232
    #206#152,           // #233
    #206#169,           // #234
    #206#180,           // #235
    #226#136#158,       // #236
    #207#134,           // #237
    #206#181,           // #238
    #226#136#169,       // #239
    #226#137#161,       // #240
    #194#177,           // #241
    #226#137#165,       // #242
    #226#137#164,       // #243
    #226#140#160,       // #244
    #226#140#161,       // #245
    #195#183,           // #246
    #226#137#136,       // #247
    #194#176,           // #248
    #226#136#153,       // #249
    #194#183,           // #250
    #226#136#154,       // #251
    #226#129#191,       // #252
    #194#178,           // #253
    #226#150#160,       // #254
    #194#160            // #255
  );

  ArrayIPFToUTF8 : TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #$E2#$80#$A2,       // #7  bullet   (hard-coded in IPF Compiler)
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #$E2#$97#$84,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    '>',                // #16
    '<',                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24  up arrow
    #25,                // #25  down arrow
    #26,                // #26  right arrow
    #27,                // #27  left arrow
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #$E2#$8C#$82,       // #127   house
    #128,           // #195#135
    #129,           // #195#188
    #130,           // #195#169
    #131,           // #195#162
    #132,           // #195#164
    #133,           // #195#160
    #134,           // #195#165
    #135,           // #195#167
    #136,           // #195#170
    #137,           // #195#171
    #138,           // #195#168
    #139,           // #195#175
    #140,           // #195#174
    #141,           // #195#172
    #142,           // #195#132
    #143,           // #195#133
    #144,           // #195#137
    #145,           // #195#166
    #146,           // #195#134
    #147,           // #195#180
    #148,           // #195#182
    #149,           // #195#178
    #150,           // #195#187
    #151,           // #195#185
    #152,           // #195#191
    #153,           // #195#150
    #154,           // #195#156
    #155,           // #194#162
    #156,           // #194#163
    #157,           // #194#165
    #158,       // #226#130#167
    #159,           // #198#146
    #160,           // #195#161
    #161,           // #195#173
    #162,           // #195#179
    #163,           // #195#186
    #164,           // #195#177
    #165,           // #195#145
    #166,           // #194#170
    #167,           // #194#186
    #168,           // #194#191
    #169,       // #226#140#144
    #170,           // #194#172
    #171,           // #194#189
    #172,           // #194#188
    #173,           // #194#161
    #174,           // #194#171
    #175,           // #194#187
    #176,       // #226#150#145
    #177,       // #226#150#146
    #178,       // #226#150#147
    #179,       // #226#148#130
    #180,       // #226#148#164
    #181,       // #226#149#161
    #182,       // #226#149#162
    #183,       // #226#149#150
    #184,       // #226#149#149
    #185,       // #226#149#163
    #186,       // #226#149#145
    #187,       // #226#149#151
    #188,       // #226#149#157
    #189,       // #226#149#156
    #190,       // #226#149#155
    #191,       // #226#148#144
    #192,       // #226#148#148
    #193,       // #226#148#180
    #$E2#$94#$AC,       // #226#148#172
    #195,       // #226#148#156
    #$E2#$94#$80,       // #196
    #197,       // #226#148#188
    #198,       // #226#149#158
    #199,       // #226#149#159
    #200,       // #226#149#154
    #201,       // #226#149#148
    #202,       // #226#149#169
    #203,       // #226#149#166
    #204,       // #226#149#160
    #205,       // #226#149#144
    #206,       // #226#149#172
    #207,       // #226#149#167
    #208,       // #226#149#168
    #209,       // #226#149#164
    #210,       // #226#149#165
    #211,       // #226#149#153
    #212,       // #226#149#152
    #213,       // #226#149#146
    #214,       // #226#149#147
    #215,       // #226#149#171
    #216,       // #226#149#170
    #217,       // #226#148#152
    #218,       // #226#148#140
    #219,       // #226#150#136
    #220,       // #226#150#132
    #221,       // #226#150#140
    #222,       // #226#150#144
    #223,       // #226#150#128
    #224,           // #206#177
    #225,           // #195#159
    #226,           // #206#147
    #227,           // #207#128
    #228,           // #206#163
    #229,           // #207#131
    #230,           // #194#181
    #231,           // #207#132
    #232,           // #206#166
    #233,           // #206#152
    #234,           // #206#169
    #235,           // #206#180
    #236,       // #226#136#158
    #237,           // #207#134
    #238,           // #206#181
    #239,       // #226#136#169
    #240,       // #226#137#161
    #241,           // #194#177
    #242,       // #226#137#165
    #243,       // #226#137#164
    #244,       // #226#140#160
    #245,       // #226#140#161
    #246,           // #195#183
    #247,       // #226#137#136
    #248,           // #194#176
    #249,       // #226#136#153
    #250,           // #194#183
    #251,       // #226#136#154
    #252,       // #226#129#191
    #253,           // #194#178
    #254,       // #226#150#160
    #255            // #194#160
  );


function CP437ToUTF8(const s: string): TfpgString;
begin
  Result := SingleByteToUTF8(s, ArrayCP437ToUTF8);
end;

function CP850ToUTF8(const s: string): TfpgString;
begin
  Result := SingleByteToUTF8(s, ArrayCP850ToUTF8);
end;

function CP866ToUTF8(const s: string): TfpgString;
begin
  Result := SingleByteToUTF8(s, ArrayCP866ToUTF8);
end;

function CP1250ToUTF8(const s: string): TfpgString;
begin
  Result := SingleByteToUTF8(s, ArrayCP1250ToUTF8);
end;

function IBMGraphToUTF8(const s: string): TfpgString;
begin
  Result := SingleByteToUTF8(s, ArrayIBMGraphToUTF8);
end;

function IPFToUTF8(const s: string): TfpgString;
  // Seaches <AValue> and replaces <ADel> with <AIns>. Case sensitive.
  function tiStrTran(AValue, ADel, AIns : string): string;
  var
    i : integer;
    sToChange : string;
  begin
    result := '';
    sToChange := AValue;
    i := UTF8Pos(ADel, sToChange);
    while i <> 0 do
    begin
      result := result + UTF8Copy(sToChange, 1, i-1) + AIns;
      UTF8Delete(sToChange, 1, i+UTF8length(ADel)-1);
      i := UTF8Pos(ADel, sToChange);
    end;
    result := result + sToChange;
  end;

begin
  // IBM Graph CodePage 437 (kind-of) to Unicode mapping.
  // Below is what we had before this function
  Result := SingleByteToUTF8(s, ArrayIPFToUTF8);
end;

function SingleByteToUTF8(const s: string; const Table: TCharToUTF8Table): TfpgString;
var
  len: Integer;
  i: Integer;
  Src: PChar;
  Dest: PChar;
  p: PChar;
  c: Char;
begin
  if s='' then
  begin
    Result := s;
    exit;
  end;
  len := length(s);
  SetLength(Result, len*4); // UTF-8 is at most 4 bytes
  Src := PChar(s);
  Dest := PChar(Result);
  for i := 1 to len do
  begin
    c := Src^;
    inc(Src);
    //if ord(c) < 128 then
    //begin
    //  Dest^ := c;
    //  inc(Dest);
    //end
    //else
    begin
      p := Table[c];
      if p <> nil then
      begin
        while p^ <> #0 do
        begin
          Dest^ := p^;
          inc(p);
          inc(Dest);
        end;
      end;
    end;
  end;
  SetLength(Result, PtrUInt(Dest)-PtrUInt(Result));
end;

function ConvertTextToUTF8(const AEncoding: TfpgTextEncoding; const AText: AnsiString): TfpgString;
begin
  case AEncoding of
    encUTF8:      Result := IPFToUTF8(AText);
    encCP437:     Result := CP437ToUTF8(AText);
    encCP850:     Result := CP850ToUTF8(AText);
    encCP866:     Result := CP866ToUTF8(AText);
    encIBMGraph:  Result := IBMGraphToUTF8(AText);
  else
    Result := IPFToUTF8(AText);
  end;
end;


end.

