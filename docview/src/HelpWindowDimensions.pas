unit HelpWindowDimensions;

{$mode objfpc}{$H+}

 // NewView - a new OS/2 Help Viewer
 // Copyright 2003 Aaron Lawrence (aaronl at consultant dot com)
 // This software is released under the Gnu Public License - see readme.txt

interface

uses
  IPFFileFormatUnit;

const
  ptCharacters = 0;
  ptPercentage = 1;
  ptPixels     = 2;
  ptPoints     = 3;
  ptDynamic    = 4;

  XPosRight    = 577; // some random values as markers
  YPosTop      = 577;
  XYPosCenter  = 578;

type
  THelpWindowRect = class(TObject)
  public
    Left:   longint; // xposright means, right aligned
    Bottom: longint; // xpostop means top aligned
                     // both: xyposcenter means centered
    Width:  longint;
    Height: longint;
    constructor Create;
    procedure   Assign(Rect: THelpWindowRect);
  end;

var
  FootnoteRect: THelpWindowRect;

procedure ReadHelpSize(const XY: THelpXYPair; var Rect: THelpWindowRect);
procedure ReadHelpPosition(const XY: THelpXYPair; var Rect: THelpWindowRect);


implementation

constructor THelpWindowRect.Create;
begin
  Left   := -1;
  Bottom := -1;
  Width  := -1;
  Height := -1;
end;

procedure THelpWindowRect.Assign(Rect: THelpWindowRect);
begin
  Left   := Rect.Left;
  Bottom := Rect.Bottom;
  Width  := Rect.Width;
  Height := Rect.Height;
end;

function GetPos(const PositionType: uint8; const Value: longint): longint;
begin
  case PositionType of
    ptCharacters:
      Result := Value;
    ptPercentage:
      Result := Value;
    ptPixels:
      Result := Value * 5;
    ptPoints:
      Result := Value;
    ptDynamic:
      case Value of
        1:
          Result := 0;           // left
        2:
          Result := XPosRight;   // right
        4:
          Result := YPosTop;     // top
        8:
          Result := 0;           // bottom
        16:
          Result := XYPosCenter; // center.
      end;
  end;
end;

procedure ReadHelpPosition(const XY: THelpXYPair; var Rect: THelpWindowRect);
var
  XPositionType: uint8;
  YPositionType: uint8;
begin
  // read origin
  XPositionType := XY.Flags div 16;
  YPositionType := XY.Flags and 15;

  if XY.X <> $ffff then
    Rect.Left   := GetPos(XPositionType, XY.X);
  if XY.Y <> $ffff then
    Rect.Bottom := GetPos(YPositionType, XY.Y);
end;

procedure ReadHelpSize(const XY: THelpXYPair; var Rect: THelpWindowRect);
begin
  if XY.X <> $ffff then
    Rect.Width  := XY.X;
  if XY.Y <> $ffff then
    Rect.Height := XY.Y;
end;

initialization
  FootnoteRect := THelpWindowRect.Create;
  with FootnoteRect do
  begin
    Left   := 10;
    Width  := 80;
    Bottom := 10;
    Height := 40;
  end;

finalization
  FootnoteRect.Free;

end.

