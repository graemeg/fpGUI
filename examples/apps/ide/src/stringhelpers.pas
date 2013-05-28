{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2013 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      ---
}

unit stringhelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function psubstr(const start,stop: pchar): string;
function cstringtostringvar(var inp: pchar): string;
function trystrtooct(const inp: string; out value: longword): boolean;
function strtooct(const inp: string): longword;
function trystrtobin(const inp: string; out value: longword): boolean;
function strtobin(const inp: string): longword;
function trystrtodec(const inp: string; out value: longword): boolean;
function strtodec(const inp: string): longword;
function trystrtohex(const inp: string; out value: longword): boolean;
function strtohex(const inp: string): longword;
function trystrtooct64(const inp: string; out value: qword): boolean;

function trystrtointvalue(const inp: string; out value: longword): boolean; overload;
function trystrtointvalue64(const inp: string; out value: qword): boolean; overload;


implementation


procedure formaterror(const value: string);
begin
 raise exception.Create('Invalid number '''+value+'''.');
end;

function strtooct(const inp: string): longword;
   //wandelt 1..0-string in longword)
begin
 if not trystrtooct(inp,result) then begin
  formaterror(inp);
 end;
end;

function strtooct64(const inp: string): qword;
   //wandelt 1..0-string in longword)
begin
 if not trystrtooct64(inp,result) then begin
  formaterror(inp);
 end;
end;

function strtodec1(const inp: string; out value: longword): boolean;
begin
 result:= trystrtoint(inp,integer(value));
end;

function strtodec164(const inp: string; out value: qword): boolean;
begin
 result:= trystrtoint64(inp,int64(value));
end;

function strtohex1(const inp: string; out value: longword): boolean;
begin
 result:= trystrtoint('$'+inp,integer(value));
end;

function strtohex164(const inp: string; out value: qword): boolean;
begin
 result:= trystrtoint64('$'+inp,int64(value));
end;


function strtooct1(const inp: string; out value: longword): boolean;
var
 int1: integer;
 ca1: longword;
 ch1: char;
begin
 result:= false;
 if inp <> '' then begin
  value:= 0;
  ca1:= 0;
  for int1:= length(inp) downto 1 do begin
   ch1:= inp[int1];
   if (ch1 < '0') or (ch1 > '7') then begin
    exit;
   end;
   value:= value + longword(((ord(ch1) - ord('0'))) shl ca1);
   inc(ca1,3);
  end;
  result:= true;
 end;
end;

function strtobin1(const inp: string; out value: longword): boolean;
   //wandelt 1..0-string in longword)
var
 int1: integer;
 lwo1: longword;
begin
 result:= false;
 if inp <> '' then begin
  value:= 0;
  lwo1:= 1;
  for int1:= length(inp) downto 1 do begin
   if inp[int1] = '1' then begin
    value:= value + lwo1;
   end
   else begin
    if inp[int1] <> '0' then begin
     exit;
    end;
   end;
   lwo1:= lwo1 shl 1;
  end;
  result:= true;
 end;
end;

function strtobin164(const inp: string; out value: qword): boolean;
   //wandelt 1..0-string in longword)
var
 int1: integer;
 lwo1: qword;
begin
 result:= false;
 if inp <> '' then begin
  value:= 0;
  lwo1:= 1;
  for int1:= length(inp) downto 1 do begin
   if inp[int1] = '1' then begin
    value:= value + lwo1;
   end
   else begin
    if inp[int1] <> '0' then begin
     exit;
    end;
   end;
   lwo1:= lwo1 shl 1;
  end;
  result:= true;
 end;
end;



function psubstr(const start,stop: pchar): string;
var
 int1: integer;
begin
 if (start = nil) or (stop = nil) then begin
  result:= '';
 end
 else begin
  int1:= stop-start;
  setlength(result,int1);
  move(start^,result[1],int1);
 end;
end;

function cstringtostringvar(var inp: pchar): string;

const
 quotechar = '"';
 escapechar = '\';

var
 po1,po2: pchar;
 int1,int2: integer;
 ch1: char;

begin
 result:= '';
 if inp <> nil then begin
  po1:= inp;
  while true do begin
   while (po1^ = ' ') do begin //first quote
    inc(po1);
   end;
   if (po1^ <> quotechar) then begin
    break;  //end or no start quote
   end;
   inc(po1);
   po2:= po1;  //text
   while true do begin
    while (po1^ <> quotechar) and (po1^ <> escapechar) do begin
     if (po1^ = #0) then begin
      result:= '';
      inp:= nil;
      exit; //error: no end quote
     end;
     inc(po1);
    end;
    int1:= po1-po2; //text length
    int2:= length(result)+1;
    setlength(result,length(result) + int1);
    move(po2^,result[int2],int1); //add text
    if po1^ = escapechar then begin
     inc(po1);
     case po1^ of
      'a': ch1:= #$07;
      'b': ch1:= #$08;
      'f': ch1:= #$0c;
      'n': ch1:= #$0a;
      'r': ch1:= #$0d;
      't': ch1:= #$09;
      'v': ch1:= #$0b;
      '\': ch1:= '\';
      '''': ch1:= '''';
      '"': ch1:= '"';
      '?': ch1:= '?';
      '0'..'7': begin
       po2:= po1;
       for int1:= 0 to 2 do begin
        if (po1^ < '0') or (po1^ > '7') then begin
         break;
        end;
        inc(po1);
       end;
       ch1:= char(strtooct(psubstr(po2,po1)));
       dec(po1);
      end;
      'x','X': begin
       inc(po1);
       po2:= po1;
       while (po1^ >= '0') and (po1^ <= '9') or
             (po1^ >= 'a') and (po1^ <= 'f') or
             (po1^ >= 'A') and (po1^ <= 'F') do begin
        inc(po1);
       end;
       ch1:= char(strtohex(psubstr(po2,po1)));
       dec(po1);
      end;
      else begin
       ch1:= ' ';
      end;
     end;
     result:= result + ch1;
     inc(po1);
    end
    else begin
     inc(po1);
     break;
    end;
    po2:= po1; //past quote
   end;
  end;
  inp:= po1;
 end;
end;

function strtooct164(const inp: string; out value: qword): boolean;
var
 int1: integer;
 ca1: longword;
 ch1: char;
begin
 result:= false;
 if inp <> '' then begin
  value:= 0;
  ca1:= 0;
  for int1:= length(inp) downto 1 do begin
   ch1:= inp[int1];
   if (ch1 < '0') or (ch1 > '7') then begin
    exit;
   end;
   value:= value + qword(((ord(ch1) - ord('0'))) shl ca1);
   inc(ca1,3);
  end;
  result:= true;
 end;
end;

function trystrtooct(const inp: string; out value: longword): boolean;
begin
 result:= strtooct1(inp,value);
 if not result then begin
  result:= trystrtointvalue(inp,value);
 end;
end;

function trystrtobin(const inp: string; out value: longword): boolean;
begin
 result:= strtobin1(inp,value);
 if not result then begin
  result:= trystrtointvalue(inp,value);
 end;
end;

function strtobin(const inp: string): longword;
   //wandelt 0..1-string in longword)
begin
 if not trystrtobin(inp,result) then begin
  formaterror(inp);
 end;
end;

function trystrtodec(const inp: string; out value: longword): boolean;
begin
 result:= strtodec1(inp,value);
 if not result then begin
  result:= trystrtointvalue(inp,value);
 end;
end;

function strtodec(const inp: string): longword;
   //wandelt 0..9-string in longword)
begin
 if not trystrtodec(inp,result) then begin
  formaterror(inp);
 end;
end;

function trystrtohex(const inp: string; out value: longword): boolean;
begin
 result:= strtohex1(inp,value);
 if not result then begin
  result:= trystrtointvalue(inp,value);
 end;
end;

function strtohex(const inp: string): longword;
begin
 if not trystrtohex(inp,result) then begin
  formaterror(inp);
 end;
end;

function trystrtooct64(const inp: string; out value: qword): boolean;
begin
 result:= strtooct164(inp,value);
 if not result then begin
  result:= trystrtointvalue64(inp,value);
 end;
end;

function trystrtointvalue(const inp: string; out value: longword): boolean;
var
 lint1: int64;
begin
 result:= false;
 if length(inp) > 0 then begin
  case inp[1] of
  '%': result:= strtobin1(copy(inp,2,length(inp)-1),value);
  '&': result:= strtooct1(copy(inp,2,length(inp)-1),value);
  '#': result:= strtodec1(copy(inp,2,length(inp)-1),value);
  '$': result:= strtohex1(copy(inp,2,length(inp)-1),value);
   else begin
    if (length(inp) > 2) and
           ((inp[2] = 'x') or (inp[2] = 'X')) and (inp[1] = '0') then begin
     result:= strtohex1(copy(inp,3,length(inp)-2),value);
    end
    else begin
     result:= trystrtoint64(inp,lint1);
     if result then begin
      value:= lint1;
     end;
    end;
   end;
  end;
 end;
end;

function trystrtointvalue64(const inp: string; out value: qword): boolean; overload;
var
 lint1: int64;
begin
 result:= false;
 if length(inp) > 0 then begin
  case inp[1] of
  '%': result:= strtobin164(copy(inp,2,length(inp)-1),value);
  '&': result:= strtooct164(copy(inp,2,length(inp)-1),value);
  '#': result:= strtodec164(copy(inp,2,length(inp)-1),value);
  '$': result:= strtohex164(copy(inp,2,length(inp)-1),value);
   else begin
    if (length(inp) > 2) and
           ((inp[2] = 'x') or (inp[2] = 'X')) and (inp[1] = '0') then begin
     result:= strtohex164(copy(inp,3,length(inp)-2),value);
    end
    else begin
     result:= trystrtoint64(inp,lint1);
     if result then begin
      value:= lint1;
     end;
    end;
   end;
  end;
 end;
end;

end.

