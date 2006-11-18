{
    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2006 by Graeme Geldenhuys 
      member of the fpGFX development team.

    Xft interface functions

    See the file COPYING.fpGFX, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit unitxft;

{$mode objfpc}{$H+}

{$linklib Xft}

interface
uses
  Classes
  ,SysUtils
  ,X
  ,XLib
  ,Xutil
  ;
  
  
type
  TPicture = longword;

  TXftDraw = record
               dummy : Pointer;
             end;
  PXftDraw = ^TXftDraw;

  TXftFont = record
               ascent   : integer;
               descent  : integer;
               height   : integer;
               max_advance_width : integer;
               ptr1     : Pointer;  // charset
               ptr2     : Pointer;  // pattern
             end;
  PXftFont = ^TXftFont;

  TXRenderColor = record
                    red   : word;
                    green : word;
                    blue  : word;
                    alpha : word;
                  end;

  TXftColor = record
                pixel : longword;
                color : TXRenderColor;
              end;
              
  TXGlyphInfo = packed record
                  width   : word;
                  height  : word;
                  x       : smallint;
                  y       : smallint;
                  xOff    : smallint;
                  yOff    : smallint;
                end;

  TFcPattern = record
    dummy : integer;
  end;

  PFcPattern = ^TFcPattern;
  PPFcPattern = ^PFcPattern;

  TFcFontSet = packed record
    nfont : integer;
    sfont : integer;
    fonts : PPFcPattern;
  end;
  PFcFontSet = ^TFcFontSet;

const
  FC_FAMILY  : PChar = 'family';
  FC_SIZE    : PChar = 'size';
  FC_SCALABLE : PChar = 'scalable';

  FcTypeVoid         = 0;
  FcTypeInteger      = 1;
  FcTypeDouble       = 2;
  FcTypeString       = 3;
  FcTypeBool         = 4;
  FcTypeMatrix       = 5;
  FcTypeCharSet      = 6;
  FcTypeFTFace       = 7;
  FcTypeLangSet      = 8;

function XftDrawCreate(display : PXDisplay; win : TXID; vis : PVisual; colorm : longint) : PXftDraw; cdecl;
procedure XftDrawChange(xftd : PXftDraw; win : TXID); cdecl;
procedure XftDrawDestroy(draw : PXftDraw); cdecl;

function XftDrawPicture(draw : PXftDraw) : TPicture; cdecl;

function XftFontOpenName(display : PXDisplay; scr : integer; par3 : PChar) : PXftFont; cdecl;
procedure XftFontClose(display : PXDisplay; fnt : PXftFont); cdecl;

procedure XftDrawString8(draw : PXftDraw; var col : TXftColor; fnt : PXftFont; x,y : integer; txt : PChar; len : integer); cdecl;
procedure XftDrawString16(draw : PXftDraw; var col : TXftColor; fnt : PXftFont; x,y : integer; txt : PChar; len : integer); cdecl;

procedure XftTextExtents16(display : PXDisplay; fnt : PXftFont; txt : PChar; len : integer; var extents : TXGlyphInfo); cdecl;
procedure XftTextExtents8(display : PXDisplay; fnt : PXftFont; txt : PChar; len : integer; var extents : TXGlyphInfo); cdecl;

//function XftGlyphExists(display : PXDisplay; fnt : PXftFont; ch : integer) : longbool; cdecl;

//procedure XftDrawSetClipRectangles(draw : PXftDraw; xorigin, yorigin : integer; rect : PXRectangle; rnum : integer); cdecl;

procedure XftDrawSetClip(draw : PXftDraw; rg : TRegion); cdecl;

function XftListFonts(display : PXDisplay; screen : integer; params : array of const) : PFcFontSet; cdecl;
function XftNameUnparse(pat : PFcPattern; dest : PChar; destlen : integer) : boolean; cdecl;
procedure FcFontSetDestroy(fsp : PFcFontSet); cdecl;

implementation

function XftDrawCreate(display : PXDisplay; win : TXID; vis : PVisual; colorm : longint) : PXftDraw; cdecl; external;
procedure XftDrawChange(xftd : PXftDraw; win : TXID); cdecl; external;
procedure XftDrawDestroy(draw : PXftDraw); cdecl; external;

function XftDrawPicture(draw : PXftDraw) : TPicture; cdecl; external;

function XftFontOpenName(display : PXDisplay; scr : integer; par3 : PChar) : PXftFont; cdecl; external;
procedure XftFontClose(display : PXDisplay; fnt : PXftFont); cdecl; external;

procedure XftDrawString8(draw : PXftDraw; var col : TXftColor; fnt : PXftFont; x,y : integer; txt : PChar; len : integer); cdecl; external;
procedure XftDrawString16(draw : PXftDraw; var col : TXftColor; fnt : PXftFont; x,y : integer; txt : PChar; len : integer); cdecl; external;

procedure XftTextExtents16(display : PXDisplay; fnt : PXftFont; txt : PChar; len : integer; var extents : TXGlyphInfo); cdecl; external;
procedure XftTextExtents8(display : PXDisplay; fnt : PXftFont; txt : PChar; len : integer; var extents : TXGlyphInfo); cdecl; external;

//function XftGlyphExists(display : PXDisplay; fnt : PXftFont; ch : integer) : longbool; cdecl; external;

//procedure XftDrawSetClipRectangles(draw : PXftDraw; xorigin, yorigin : integer; rect : PXRectangle; rnum : integer); cdecl; external;

procedure XftDrawSetClip(draw : PXftDraw; rg : TRegion); cdecl; external;

function XftListFonts(display : PXDisplay; screen : integer; params : array of const) : PFcFontSet; cdecl; external;
function XftNameUnparse(pat : PFcPattern; dest : PChar; destlen : integer) : boolean; cdecl; external;
procedure FcFontSetDestroy(fsp : PFcFontSet); cdecl; external;

end.

