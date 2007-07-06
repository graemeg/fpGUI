unit uhelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 


function InflateRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean;

implementation

function InflateRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean;
begin
  if Assigned(@Rect) then
  begin
    with Rect do
    begin
      dec(Left, dx);
      dec(Top, dy);
      inc(Right, dx);
      inc(Bottom, dy);
    end;
    Result := True;
  end
  else
    Result := False;
end;


end.

