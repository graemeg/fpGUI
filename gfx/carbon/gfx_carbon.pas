{
    fpGUI  -  Free Pascal Graphical User Interface

    GFX_Carbon  -  Carbon (Mac OS X) target implementation

    Copyright (C) 2006 - 2007 See the file AUTHORS, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit gfx_carbon;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  { Pascal RTL Units }
  SysUtils, Classes,
  { Carbon headers }
  MacOSXAll,
  { fpGfx units }
  GfxBase; 

type

  ECarbonError = class(EGfxError);

  { TCarbonFont }

  TCarbonFont = class(TFCustomFont)
  public
    class function GetDefaultFontName(const AFontClass: TGfxFontClass): String; override;
    constructor Create(const Descriptor: String);
    destructor  Destroy; override;
  end;


  { TCarbonCanvas }

  TCarbonCanvas = class(TFCustomCanvas)
  protected
    function    DoExcludeClipRect(const ARect: TRect): Boolean; override;
    function    DoIntersectClipRect(const ARect: TRect): Boolean; override;
    function    DoUnionClipRect(const ARect: TRect): Boolean; override;
    function    DoGetClipRect: TRect; override;
    procedure   DoDrawArc(const ARect: TRect; StartAngle, EndAngle: Single); override;
    procedure   DoDrawCircle(const ARect: TRect); override;
    procedure   DoDrawLine(const AFrom, ATo: TPoint); override;
    procedure   DoDrawPoint(const APoint: TPoint); override;
    procedure   DoFillRect(const ARect: TRect); override;
    procedure   DoTextOut(const APosition: TPoint; const AText: String); override;
    procedure   DoCopyRect(ASource: TFCustomCanvas; const ASourceRect: TRect; const ADestPos: TPoint); override;
    procedure   DoMaskedCopyRect(ASource, AMask: TFCustomCanvas; const ASourceRect: TRect; const AMaskPos, ADestPos: TPoint); override;
    procedure   DoDrawImageRect(AImage: TFCustomBitmap; ASourceRect: TRect; const ADestPos: TPoint); override;
  public
    constructor Create(AHandle: HDC);
    destructor  Destroy; override;
    function    MapColor(const AColor: TGfxColor): TGfxPixel; override;
    function    FontCellHeight: Integer; override;
    function    TextExtent(const AText: String): TSize; override;
    procedure   SaveState; override;
    procedure   RestoreState; override;
    procedure   EmptyClipRect; override;
    procedure   SetColor_(AColor: TGfxPixel); override;
    procedure   SetFont(AFont: TFCustomFont); override;
    procedure   SetLineStyle(ALineStyle: TGfxLineStyle); override;
    property    Handle: HDC read FHandle;
  end;


  TCarbonWindowCanvas = class(TxxxCanvas)
  private
    FWnd: HWND;
  public
    constructor Create(AWnd: HWND);
    destructor Destroy; override;
  end;


  TCarbonBitmapCanvas = class(TxxxCanvas)
  public
    constructor Create(ABitmap: HBITMAP; AWidth, AHeight: Integer);
    destructor Destroy; override;
  end;

  { TCarbonBitmap }

  TCarbonBitmap = class(TFCustomBitmap)
  private
    IsLocked: Boolean;
  public
    constructor Create(AWidth, AHeight: Integer; APixelFormat: TGfxPixelFormat); override;
    destructor Destroy; override;
    procedure Lock(var AData: Pointer; var AStride: LongWord); override;
    procedure Unlock; override;
  end;

  { TCarbonScreen }

  TCarbonScreen = class(TFCustomScreen)
  public
    constructor Create; override;
  end;


  { TCarbonApplication }

  TCarbonApplication = class(TFCustomApplication)
  private
    DoBreakRun: Boolean;
  public
    { default methods }
    constructor Create; override;
    destructor  Destroy; override;
    procedure   AddWindow(AWindow: TFCustomWindow); override;
    procedure   Initialize(ADisplayName: String = ''); override;
    procedure   Run; override;
    procedure   Quit; override;
  end;

  { TCarbonWindow }

  TCarbonWindow = class(TFCustomWindow)
  protected
    function    GetTitle: String; override;
    procedure   SetTitle(const ATitle: String); override;
    procedure   DoSetCursor; override;
  public
    constructor Create(AParent: TFCustomWindow; AWindowOptions: TGfxWindowOptions); override;
    destructor  Destroy; override;
    procedure   DefaultHandler(var Message); override;
    procedure   SetPosition(const APosition: TPoint); override;
    procedure   SetSize(const ASize: TSize); override;
    procedure   SetMinMaxSize(const AMinSize, AMaxSize: TSize); override;
    procedure   SetClientSize(const ASize: TSize); override;
    procedure   SetMinMaxClientSize(const AMinSize, AMaxSize: TSize); override;
    procedure   Show; override;
    procedure   Invalidate(const ARect: TRect); override;
    procedure   PaintInvalidRegion; override;
    procedure   CaptureMouse; override;
    procedure   ReleaseMouse; override;
  end;


implementation

end.
