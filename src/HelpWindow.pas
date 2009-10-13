Unit HelpWindow;

{$mode objfpc}{$H+}

// NewView - a new OS/2 Help Viewer
// Copyright 2001 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

// What a misnomer. This really just contains a few definitions
// and functions relevant to helpwindow dimensions.

uses
  IPFFileFormatUnit;

const
  ptCharacters = 0;
  ptPercentage = 1;
  ptPixels = 2;
  ptPoints = 3;
  ptDynamic = 4;

  XPosRight = 577;
  YPosTop = 577;
  XYPosCenter = 578;

type
  THelpWindowRect = class
    Left: longint;
    Bottom: longint;
    Width: longint;
    Height: longint;
    constructor Create;
  end;

procedure SetFootnoteRect( Var Rect: THelpWindowRect );

procedure ReadHelpSize( const XY: THelpXYPair;
                        Var Rect: THelpWindowRect );
procedure ReadHelpPosition( const XY: THelpXYPair;
                            Var Rect: THelpWindowRect );


Implementation

constructor THelpWindowRect.Create;
begin
  Left:= -1;
  Bottom:= -1;
  Width:= -1;
  Height:= -1;
end;
                 
function GetPos( const PositionType: uint8;
                 const Value: longint ): longint;
begin
  case PositionType of
    ptCharacters:
      Result:= Value;
    ptPercentage:
      Result:= Value;
    ptPixels:
      Result:= Value * 5;
    ptPoints:
      Result:= Value;
    ptDynamic:
      case Value of
        1: Result:= 0; // left
        2: Result:= XPosRight; // right
        4: Result:= 0; // bottom
        8: Result:= YPosTop; // top
        16: Result:= 50; //XYPosCenter; // center.
      end;
  end;
end;

procedure ReadHelpPosition( const XY: THelpXYPair;
                            Var Rect: THelpWindowRect );
var
  XPositionType: uint8;
  YPositionType: uint8;
begin
  // read origin
  XPositionType:= XY.Flags div 16;
  YPositionType:= XY.Flags and 15;

  if XY.X <> -1 then
    Rect.Left:= GetPos( XPositionType, XY.X );
  if XY.Y <> -1 then
    Rect.Bottom:= GetPos( YPositionType, XY.Y );
end;

procedure ReadHelpSize( const XY: THelpXYPair;
                        Var Rect: THelpWindowRect );
begin
  if XY.X <> -1 then
    Rect.Width:= XY.X;
  if XY.Y <> -1 then
    Rect.Height:= XY.Y;
end;

procedure SetFootnoteRect( Var Rect: THelpWindowRect );
begin
  with Rect do
  begin
    Left:= 10;
    Width:= 80;
    Bottom:= 10;
    Height:= 40;
  end;
end;

Initialization
End.
