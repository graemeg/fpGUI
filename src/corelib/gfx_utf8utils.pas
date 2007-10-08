{
  Some handly UTF8 function copied from the Lazarus LCL. Comes from the LCLProc
  unit.Surely we can move this into FPC?
}
unit gfx_UTF8utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


function  UTF8Pos(const SearchForText, SearchInText: string): integer;
function  UTF8Copy(const s: string; StartCharIndex, CharCount: integer): string;
function  UTF8Length(const s: string): integer;
function  UTF8Length(p: PChar; ByteCount: integer): integer;
function  UTF8CharStart(UTF8Str: PChar; Len, Index: integer): PChar;
function  UTF8CharacterLength(p: PChar): integer;
procedure UTF8Insert(const Source: string; var S: string; Index: integer);
procedure UTF8Delete(var S: string; Index, Size: integer);


implementation

// returns the character index, where the SearchForText starts in SearchInText
function UTF8Pos(const SearchForText, SearchInText: string): integer;
var
 p: LongInt;
begin
  p := System.Pos(SearchForText, SearchInText);
  if p > 0 then
    Result := UTF8Length(PChar(SearchInText), p-1) + 1
  else
    Result := 0;
end;

// returns substring
function UTF8Copy(const s: string; StartCharIndex, CharCount: integer): string;
var
 StartBytePos: PChar;
 EndBytePos: PChar;
 MaxBytes: PtrInt;
begin
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

function UTF8Length(const s: string): integer;
begin
  Result := UTF8Length(PChar(s),length(s));
end;

function UTF8Length(p: PChar; ByteCount: integer): integer;
var
  CharLen: LongInt;
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

function UTF8CharStart(UTF8Str: PChar; Len, Index: integer): PChar;
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

function UTF8CharacterLength(p: PChar): integer;
begin
  if p <> nil then
  begin
    if ord(p^) < %11000000 then
    begin
      // regular single byte character (#0 is a character, this is pascal ;)
      Result:=1;
    end
    else if ((ord(p^) and %11100000) = %11000000) then
    begin
      // could be 2 byte character
      if (ord(p[1]) and %11000000) = %10000000 then
        Result := 2
      else
        Result := 1;
    end
    else if ((ord(p^) and %11110000) = %11100000) then
    begin
      // could be 3 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
          and ((ord(p[2]) and %11000000) = %10000000) then
        Result:=3
      else
        Result:=1;
    end
    else if ((ord(p^) and %11111000) = %11110000) then
    begin
      // could be 4 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
          and ((ord(p[2]) and %11000000) = %10000000)
          and ((ord(p[3]) and %11000000) = %10000000) then
        Result:=4
      else
        Result:=1;
    end
    else
      Result:=1;
  end
  else
    Result:=0;
end;

procedure UTF8Insert(const Source: string; var S: string; Index: integer);
var
  b: string;
  e: string;
begin
  if UTF8Length(Source) = 0 then
    Exit; //==>
  b := UTF8Copy(S, 1, Index-1); // beginning string
  e := UTF8Copy(S, Index, UTF8Length(S)-Index+1); // ending string
  S := b + Source + e;
end;

procedure UTF8Delete(var S: string; Index, Size: integer);
var
  ls: integer;
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

end.

