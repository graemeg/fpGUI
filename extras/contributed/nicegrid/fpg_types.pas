unit fpg_types;
{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, fpg_base;

function EqualfpgRect(const r1,r2 : TfpgRect) : Boolean;
function PtInfpgRect(const ARect : TfpgRect; const p : TPoint) : Boolean;
function IntersectfpgRect(out ARect : TfpgRect; const R1,R2 : TfpgRect) : Boolean;
function UnionfpgRect(out ARect : TfpgRect; const R1,R2 : TfpgRect) : Boolean;
function IsfpgRectEmpty(const ARect : TfpgRect) : Boolean;
function OffsetfpgRect(var ARect : TfpgRect;DX : Integer;DY : Integer) : Boolean;
function InflatefpgRect(var ARect: TfpgRect; dx: Integer; dy: Integer): Boolean;
function CopyfpgRect(out Ds: TfpgRect;const Sc : TfpgRect): Boolean;

implementation


function EqualfpgRect(const r1,r2 : TfpgRect) : Boolean;
begin
  Result:= (r1.Left=r2.Left) and (r1.Right=r2.Right) and (r1.Width=r2.Width) and (r1.Height=r2.Height);
end;

function PtInfpgRect(const ARect : TfpgRect;const p : TPoint) : Boolean;
begin
  Result:=(p.y >= ARect.Top) and
            (p.y  <= ARect.Bottom) and
            (p.x  >= ARect.Left) and
            (p.x  <= ARect.Right);
end;

function IsfpgRectEmpty(const ARect : TfpgRect) : Boolean;
begin
  Result:=(ARect.Width <= 0) or (ARect.Height <= 0);
end;

function IntersectfpgRect(out ARect : TfpgRect;const R1,R2 : TfpgRect) : Boolean;
begin
  ARect:=R1;
  with R2 do
  begin
    if Left > R1.Left then
      ARect.Left:=Left;
    if Top > R1.Top then
      ARect.Top:=Top;
    if Right < R1.Right then
      ARect.Width:= ARect.Left + Right; 
    if Bottom < R1.Bottom then
      ARect.Height:= ARect.Top + Bottom; 
    end;
  if IsfpgRectEmpty(ARect) then
  begin
    FillChar(ARect,SizeOf(ARect),0);
    Result:=false;
  end
  else
    Result:=true;
end;

function UnionfpgRect(out ARect : TfpgRect;const R1,R2 : TfpgRect) : Boolean;
begin
  ARect:=R1;
  with R2 do
  begin
    if Left < R1.Left then
      ARect.Left:=Left;
    if Top < R1.Top then
      ARect.Top:=Top;
    if Right > R1.Right then
      ARect.Width:=  ARect.Left + Right; 
    if Bottom>R1.Bottom then
      ARect.Height:= ARect.Top + Bottom; 
    end;
  if IsfpgRectEmpty(ARect) then
  begin
    FillChar(ARect,SizeOf(ARect),0);
    Result:=false;
  end
  else
    Result:=true;
end;



function OffsetfpgRect(var ARect : TfpgRect;DX : Integer;DY : Integer) : Boolean;
begin
  if assigned(@ARect) then
    begin
    with ARect do
    begin
      inc(Left,dx);
      inc(Top,dy);
    end;
    Result:=true;
    end
  else
    Result:=false;
end;

function InflatefpgRect(var ARect: TfpgRect; dx: Integer; dy: Integer): Boolean;
begin
  if Assigned(@ARect) then
  begin
    with ARect do
    begin
      dec(Left, dx);
      dec(Top, dy);
      inc(Width, dx*2);
      inc(Height, dy*2);
    end;
    Result := True;
  end
  else
    Result := False;
end;
  
function CopyfpgRect(out Ds: TfpgRect;const Sc : TfpgRect): Boolean;
begin
  Ds:=Sc;
  if IsfpgRectEmpty(Ds) then
  begin	  
    FillChar(Ds,SizeOf(Ds),0);
    Result:=false;
  end
  else
    Result:=true;
end;  


end. 
