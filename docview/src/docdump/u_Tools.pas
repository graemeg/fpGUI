{
  This unit will grow to include all handy functions that can be used in
  different Lazarus projects.

  There may be no links to other non-standard units!
}
unit u_Tools;

{$mode objfpc}{$H+}

interface

  { Missing iif() known from Visual Basic - return a string }
  function  iif(fCon: Boolean; sTrue, sFalse: String): String;
  { Missing iif() known from Visual Basic - return an Integer }
  function  iif(fCon: Boolean; iTrue, iFalse: Integer): Integer;
  { Missing iif() known from Visual Basic - return an Extended }
  function  iif(fCon: Boolean; iTrue, iFalse: Extended): Extended;


implementation
uses
  SysUtils;


function iif(fCon: Boolean; sTrue, sFalse: String): String;
begin
  if fCon then
    Result := sTrue
  else
    Result := sFalse;
end;

function iif(fCon: Boolean; iTrue, iFalse: Integer): Integer;
begin
  if fCon then
    Result := iTrue
  else
    Result := iFalse;
end;

function iif(fCon: Boolean; iTrue, iFalse: Extended): Extended;
begin
  if fCon then
    Result := iTrue
  else
    Result := iFalse;
end;


end.

