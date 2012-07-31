{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Xft interface functions
}

unit fpg_xft_x11;

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

const
  {$IF Defined(DARWIN)}
    libXft = 'libXft.dylib';
    {$LINKLIB libXft}
    fclib = 'libfontconfig.dylib';
    {$LINKLIB libfontconfig}
  {$ELSE}
    libXft = 'libXft.so';
    fclib = 'fontconfig';
  {$IFEND}

  
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
                pixel : ptrint;
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
//  FC_FAMILY  : PChar = 'family';
//  FC_SIZE    : PChar = 'size';
//  FC_SCALABLE : PChar = 'scalable';

  FC_FAMILY =          'family';	//* String */
  FC_STYLE =           'style';		//* String */
  FC_SLANT =           'slant';		//* Int */
  FC_WEIGHT =	       'weight';	//* Int */
  FC_SIZE =	           'size';      //* Double */
  FC_ASPECT =	       'aspect';	//* Double */
  FC_PIXEL_SIZE =      'pixelsize';     //* Double */
  FC_SPACING =	       'spacing';	//* Int */
  FC_FOUNDRY =	       'foundry';	//* String */
  FC_ANTIALIAS =       'antialias';	//* Bool (depends) */
  FC_HINTING =	       'hinting';	//* Bool (true) */
  FC_VERTICAL_LAYOUT = 'verticallayout';//* Bool (false) */
  FC_AUTOHINT =	       'autohint';	//* Bool (false) */
  FC_GLOBAL_ADVANCE =  'globaladvance';	//* Bool (true) */
  FC_FILE =	       'file';		//* String */
  FC_INDEX =	       'index';		//* Int */
  FC_FT_FACE =	       'ftface';	//* FT_Face */
  FC_RASTERIZER =      'rasterizer';	//* String */
  FC_OUTLINE =	       'outline';	//* Bool */
  FC_SCALABLE =	       'scalable';	//* Bool */
  FC_SCALE =	       'scale';		//* double */
  FC_DPI =             'dpi';		//* double */
  FC_RGBA =            'rgba';		//* Int */
  FC_MINSPACE =	       'minspace';	//* Bool use minimum line spacing */
  FC_SOURCE =	       'source';	//* String (X11, freetype) */
  FC_CHARSET =	       'charset';	//* CharSet */
  FC_LANG =            'lang';		//* String RFC 3066 langs */
  FC_FONTVERSION =     'fontversion';	//* Int from 'head' table */

  FC_MATRIX =          'matrix';
  FC_CHAR_WIDTH =      'charwidth';

  FC_WEIGHT_BOLD = 200;
  FC_SLANT_ITALIC = 100;
  FC_PROPORTIONAL = 0;
  FC_MONO = 100;


  FcTypeVoid         = 0;
  FcTypeInteger      = 1;
  FcTypeDouble       = 2;
  FcTypeString       = 3;
  FcTypeBool         = 4;
  FcTypeMatrix       = 5;
  FcTypeCharSet      = 6;
  FcTypeFTFace       = 7;
  FcTypeLangSet      = 8;



function  XftDrawCreate(display : PXDisplay; win : TXID; vis : PVisual; colorm : longint) : PXftDraw; cdecl; external libXft;
procedure XftDrawChange(xftd : PXftDraw; win : TXID); cdecl; external libXft;
procedure XftDrawDestroy(draw : PXftDraw); cdecl; external libXft;
function  XftDrawPicture(draw : PXftDraw) : TPicture; cdecl; external libXft;
function  XftFontOpenName(display : PXDisplay; scr : integer; par3 : PChar) : PXftFont; cdecl; external libXft;
procedure XftFontClose(display : PXDisplay; fnt : PXftFont); cdecl; external libXft;
procedure XftDrawStringUtf8(draw : PXftDraw; var col : TXftColor; fnt : PXftFont; x,y : integer; txt : PChar; len : integer); cdecl; external libXft;
procedure XftDrawString8(draw : PXftDraw; var col : TXftColor; fnt : PXftFont; x,y : integer; txt : PChar; len : integer); cdecl; external libXft;
procedure XftDrawString16(draw : PXftDraw; var col : TXftColor; fnt : PXftFont; x,y : integer; txt : PChar; len : integer); cdecl; external libXft;
procedure XftTextExtentsUtf8(display : PXDisplay; fnt : PXftFont; txt : PChar; len : integer; var extents : TXGlyphInfo); cdecl; external libXft;
procedure XftTextExtents8(display : PXDisplay; fnt : PXftFont; txt : PChar; len : integer; var extents : TXGlyphInfo); cdecl; external libXft;
procedure XftTextExtents16(display : PXDisplay; fnt : PXftFont; txt : PChar; len : integer; var extents : TXGlyphInfo); cdecl; external libXft;
//function XftGlyphExists(display : PXDisplay; fnt : PXftFont; ch : integer) : longbool; cdecl; external libXft;
//procedure XftDrawSetClipRectangles(draw : PXftDraw; xorigin, yorigin : integer; rect : PXRectangle; rnum : integer); cdecl; external libXft;
procedure XftDrawSetClip(draw : PXftDraw; rg : TRegion); cdecl; external libXft;
function  XftListFonts(display : PXDisplay; screen : integer; params : array of const) : PFcFontSet; cdecl; external libXft;
function  XftNameUnparse(pat : PFcPattern; dest : PChar; destlen : integer) : boolean; cdecl; external libXft;
procedure FcFontSetDestroy(fsp : PFcFontSet); cdecl; external fclib;


//function FcFontList(config: PFcConfig; p:PFcPattern; os:PFcObjectSet): PFcFontSet;cdecl; external fclib name 'FcFontList';


implementation

end.

