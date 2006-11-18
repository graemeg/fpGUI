{ schar16.pas: Function for handling 16 bit unicode in normal 8 bit ansi strings
  File maintainer: nvitya@freemail.hu

History:
}

unit schar16;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils;

const
  u8escchar  : char = '^';
  u8escclose : char = ';';

type
  String16 = string;
  String8  = string;

  Char16 = packed record
             char1 : char;
             char2 : char;
           end;

function Str8to16(s : String8) : string16; // = u8
function Str16to8(s : String16) : string8;  // = u16u8

function u8(s : String8) : string16;       // decodes escaped text
function u8u16(s : String8) : string16;    // same as u8
function u8noesc(s : String8) : string16;  // inserts #0-s, doesn't use escapes

function u16u8(s : string16) : string8;    // escapes only 256..65535
function u16u8safe(s : string16) : string8; // escapes 0.31 and 255..65535
function u16u8trunc(s : string16) : string8; // truncates hi byte, doesn't use escapes ('^' -> '^'), try handle code pages
// this function could cause information loss

function Length16(s : String16) : integer;
procedure SetLength16(var s : String16; len : integer);

function Pos16(const s : string16; const searched : string16) : integer;
function UpCase16(const s : string16) : string16;
function Upper16(const s : string16) : string16;

function Copy16(s : String16; ind,len : integer) : String16;
procedure Insert16(s : String16; var dest : string16; ind : integer);
procedure Delete16(var s : String16; ind, count : integer);

procedure AddChar16(var s : String16; ch16 : Char16); overload;
procedure AddChar16(var s : String16; w : word); overload;

implementation

uses UnitKeys;

function u8(s : String8) : string16;
var
  n : integer;
  ccode : word;
  c : char;
begin
  // 'asdf^123^452^354;78;
  result := '';
  n := 1;
  while (n <= length(s)) do
  begin
    if s[n] = u8escchar then
    begin
      inc(n);
      if (n <= length(s)) and (s[n] = u8escchar) then
      begin
        result := result + u8escchar + #0;
        inc(n);
        continue;
      end;

      ccode := 0;
      while (n <= length(s)) do
      begin
        c := s[n];
        if (c >= '0') and (c <= '9') then ccode := ccode * 10 + (ord(c)-ord('0'))
        else
            break;
        inc(n);
      end;
      result := result + chr(lo(ccode)) + chr(hi(ccode));

      if (n <= length(s)) and (s[n] = u8escclose) then inc(n);
    end
    else
    begin
      result := result + s[n] + #0;
      inc(n);
    end;
  end;
end;

function u8u16(s : String8) : string16;    // same as u8
begin
  result := u8(s);
end;

function u16u8safe(s : string16) : string8;
var
  ccode : word;
  n : integer;
  uni : boolean;
begin
  result := '';
  uni := false;
  n := 1;
  while n < length(s) do
  begin
    ccode := ord(s[n]) + (ord(s[n+1]) shl 8);
    if (ccode < 32) or (ccode > 254) then
    begin
       result := result + u8escchar + IntToStr(ccode);
       uni := true;
    end
    else
    begin
      if ccode = ord(u8escchar) then result := result + u8escchar + u8escchar
      else
      begin
        if uni and (ccode >= ord('0')) and (ccode <= ord('9')) then result := result + u8escclose;
        result := result + chr(ccode);
      end;
      uni := false;
    end;
    inc(n,2);
  end;
end;

function u16u8(s : string16) : string8;
var
  ccode : word;
  n : integer;
  uni : boolean;
begin
  result := '';
  uni := false;
  n := 1;
  while n < length(s) do
  begin
    ccode := ord(s[n]) + (ord(s[n+1]) shl 8);
    if (ccode > 255) then
    begin
       result := result + u8escchar + IntToStr(ccode);
       uni := true;
    end
    else
    begin
      if ccode = ord(u8escchar) then result := result + u8escchar + u8escchar
      else
      begin
        if uni and (ccode >= ord('0')) and (ccode <= ord('9')) then result := result + u8escclose;
        result := result + chr(ccode);
      end;
      uni := false;
    end;
    inc(n,2);
  end;
end;

function u16u8trunc(s : string16) : string8;
var
  n : integer;
  i : integer;
  len : integer;
  ct,c : char;
begin
  SetLength(Result,length16(s));
  i := 1;
  len := length16(s);
  for n := 1 to len do
  begin
    ct := s[i+1];
    c  := s[i];
    // some hungarian translation:
    if ct = #1 then
    begin
      c := TranslateChar(ct,c);
    end;
    Result[n] := c;
    inc(i,2);
  end;
end;

function u8noesc(s: String8): string16;
var
  n : integer;
  i : integer;
  len : integer;
begin
  SetLength16(Result,length(s));
  i := 1;
  len := length(s);
  for n := 1 to len do
  begin
    Result[i] :=  s[n];
    inc(i);
    Result[i] := #0;
    inc(i);
  end;
end;

function Str8to16(s : String8) : string16;
begin
  result := u8(s);
end;

function Str16to8(s : String16) : string8;
begin
  result := u16u8(s);
end;

{
function Str8to16(s : String8) : string16;
var
  n : integer;
  i : integer;
  len : integer;
begin
  SetLength16(Result,length(s));
  i := 1;
  len := length(s);
  for n := 1 to len do
  begin
    Result[i] :=  s[n];
    inc(i);
    Result[i] := #0;
    inc(i);
  end;
end;

function Str16to8(s : String8) : string8;
var
  n : integer;
  i : integer;
  len : integer;
  ct,c : char;
begin
  SetLength(Result,length16(s));
  i := 1;
  len := length16(s);
  for n := 1 to len do
  begin
    ct := s[i+1];
    c  := s[i];
    // some hungarian translation:
    if ct = #1 then
    begin
      c := TranslateChar(ct,c);
    end;
    Result[n] := c;
    inc(i,2);
  end;
end;
}

procedure SetLength16(var s : String16; len : integer);
begin
  if len >= 0 then SetLength(s,len shl 1);
end;

function Length16(s : String16) : integer;
begin
  Result := Length(s) shr 1;
end;

function Pos16(const s : string16; const searched : string16) : integer;
var
  n: integer;
begin
  result := 0;
  if length16(s) < 1 then Exit;
  for n := 1 to Length16(searched)-Length16(s) do
  begin
    if CompareMem(@s[1], @searched[n*2-1], Length16(s)) then
    begin
      result := n;
      Exit;
    end;
  end;
end;

function UpCase16(const s : string16) : string16;
begin
  if length(s) < 2 then Exit;
  if s[2] = #0 then Result := UpCase(s[1])+#0
  else
  begin
    result := chr(ord(s[1]) and $FE) + s[2];
  end;
end;

function Upper16(const s : string16) : string16;
var
  n : integer;
begin
  result := '';
  for n := 1 to length16(s) do
  begin
    result := result + UpCase16(s[n*2-1]+s[n*2]);
  end;
end;

function Copy16(s : String16; ind,len : integer) : String16;
begin
  result := copy(s,1 + ((ind-1) shl 1), len shl 1);
end;

procedure Insert16(s : String16; var dest : string16; ind : integer);
begin
  Insert(s,dest,1 + ((ind-1) shl 1));
end;

procedure Delete16(var s : String16; ind, count : integer);
begin
  Delete(s,1 + ((ind-1) shl 1), count shl 1);
end;

procedure AddChar16(var s : String16; ch16 : Char16);
begin
  s := s + ch16.char1 + ch16.char2;
end;

procedure AddChar16(var s : String16; w : word);
begin
  s := s + chr(lo(w)) + chr(hi(w));
end;

end.

