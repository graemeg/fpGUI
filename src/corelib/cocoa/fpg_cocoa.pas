{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2019 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit implements MacOS Cocoa support for fpGUI.
}

unit fpg_cocoa;

{$I fpg_defines.inc}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  CocoaAll,
  fpg_base,
  fpg_impl;
  
type

  TfpgCocoaFontResource = class(TfpgFontResourceBase)
  public
    constructor Create(const afontdesc: string); override;
    function    GetAscent: integer; override;
    function    GetDescent: integer; override;
    function    GetHeight: integer; override;
    function    GetTextWidth(const txt: string): integer; override;
    function    HandleIsValid: boolean; override;
  end;


  TfpgCocoaImage = class(TfpgImageBase)
  protected
    procedure   DoFreeImage; override;
    procedure   DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: Pointer); override;
    procedure   DoInitImageMask(awidth, aheight: integer; aimgdata: Pointer); override;
  end;
  
  // graemeg: Since we are going to use AggCanvas, this is probably not needed at all.
  TfpgCocoaCanvas = class(TfpgCanvasBase)
  protected
    procedure   DoSetFontRes(fntres: TfpgFontResourceBase); override;
    procedure   DoSetTextColor(cl: TfpgColor); override;
    procedure   DoSetColor(cl: TfpgColor); override;
    procedure   DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle); override;
    procedure   DoFillRectangle(x, y, w, h: TfpgCoord); override;
    procedure   DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord); override;
    procedure   DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord); override;
    procedure   DoDrawRectangle(x, y, w, h: TfpgCoord); override;
    procedure   DoDrawLine(x1, y1, x2, y2: TfpgCoord); override;
    procedure   DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer); override;
    procedure   DoDrawString(x, y: TfpgCoord; const txt: string); override;
    procedure   DoSetClipRect(const ARect: TfpgRect); override;
    function    DoGetClipRect: TfpgRect; override;
    procedure   DoAddClipRect(const ARect: TfpgRect); override;
    procedure   DoClearClipRect; override;
    procedure   DoBeginDraw(awidget: TfpgWidgetBase; CanvasTarget: TfpgCanvasBase); override;
    procedure   DoPutBufferToScreen(x, y, w, h: TfpgCoord); override;
    procedure   DoEndDraw; override;
    function    GetPixel(X, Y: integer): TfpgColor; override;
    procedure   SetPixel(X, Y: integer; const AValue: TfpgColor); override;
    procedure   DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: Extended); override;
    procedure   DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended); override;
    procedure   DoDrawPolygon(const Points: array of TPoint); override;
    function    GetBufferAllocated: Boolean; override;
    procedure   DoAllocateBuffer; override;
  end;
  
  
  TfpgCocoaWindow = class(TfpgWindowBase)
  private
    FWinHandle: TfpgWinHandle;
  protected
    FModalForWin: TfpgCocoaWindow;
    function    HandleIsValid: boolean; override;
    procedure   DoUpdateWindowPosition; override;
    procedure   DoAllocateWindowHandle(AParent: TfpgWidgetBase); override;
    procedure   DoReleaseWindowHandle; override;
    procedure   DoRemoveWindowLookup; override;
    procedure   DoSetWindowAttributes(const AOldAtributes, ANewAttributes: TWindowAttributes; const AForceAll: Boolean); override;
    procedure   DoSetWindowVisible(const AValue: Boolean); override;
    procedure   DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord); override;
    function    DoWindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint; override;
    procedure   DoSetWindowTitle(const ATitle: string); override;
    procedure   DoSetMouseCursor; override;
    procedure   DoDNDEnabled(const AValue: boolean); override;
    property    WinHandle: TfpgWinHandle read FWinHandle;
  end;
  
  
  TfpgCocoaApplication = class(TfpgApplicationBase)
  protected
    function    DoGetFontFaceList: TStringList; override;
    procedure   DoWaitWindowMessage(atimeoutms: integer); override;
    function    MessagesPending: boolean; override;
    procedure   DoFlush; override;
  public
    function    GetScreenWidth: TfpgCoord; override;
    function    GetScreenHeight: TfpgCoord; override;
    function    GetScreenPixelColor(APos: TPoint): TfpgColor; override;
    function    Screen_dpi_x: integer; override;
    function    Screen_dpi_y: integer; override;
    function    Screen_dpi: integer; override;
  end;
  
  
  TfpgCocoaClipboard = class(TfpgClipboardBase)
  protected
    function    DoGetText: TfpgString; override;
    procedure   DoSetText(const AValue: TfpgString); override;
    procedure   InitClipboard; override;
  end;
  
  
  TfpgCocoaFileList = class(TfpgFileListBase)
  end;
  
  
  TfpgCocoaMimeData = class(TfpgMimeDataBase)
  end;
  
  
  TfpgCocoaDrag = class(TfpgDragBase)
  public
    function    Execute(const ADropActions: TfpgDropActions; const ADefaultAction: TfpgDropAction=daCopy): TfpgDropAction; override;
  end;
  
  
  TfpgCocoaDrop = class(TfpgDropBase)
  protected
    function    GetDropAction: TfpgDropAction; override;
    procedure   SetDropAction(AValue: TfpgDropAction); override;
    function    GetWindowForDrop: TfpgWindowBase; override;
  end;
  
  
  TfpgCocoaTimer = class(TfpgBaseTimer)
  end;
  

  TfpgCocoaSystemTrayHandler = class(TfpgSystemTrayHandlerBase)
  public
    procedure   Show; override;
    procedure   Hide; override;
    function    IsSystemTrayAvailable: boolean; override;
    function    SupportsMessages: boolean; override;
  end;
  
  
  
implementation

uses
  baseunix,
  unix,
  fpg_main,
  fpg_widget,
  fpg_popupwindow,
  fpg_window,       // used for window attributes changed callback
  fpg_stringutils,  // used for GetTextWidth
  fpg_utils,
  fpg_form,         // for modal event support
  fpg_cmdlineparams,
  fpg_constants; 

{ TfpgCocoaFontResource }

constructor TfpgCocoaFontResource.Create(const afontdesc: string);
begin
end;

function    TfpgCocoaFontResource.GetAscent: integer;
begin
end;

function    TfpgCocoaFontResource.GetDescent: integer;
begin
end;

function    TfpgCocoaFontResource.GetHeight: integer;
begin
end;

function    TfpgCocoaFontResource.GetTextWidth(const txt: string): integer;
begin
end;

function    TfpgCocoaFontResource.HandleIsValid: boolean;
begin
end;

{ TfpgCocoaImage }

procedure   TfpgCocoaImage.DoFreeImage;
begin
end;

procedure   TfpgCocoaImage.DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: Pointer);
begin
end;

procedure   TfpgCocoaImage.DoInitImageMask(awidth, aheight: integer; aimgdata: Pointer);
begin
end;

{ TfpgCocoaWindow }
  
function    TfpgCocoaWindow.HandleIsValid: boolean;
begin
end;

procedure   TfpgCocoaWindow.DoUpdateWindowPosition;
begin
end;

procedure   TfpgCocoaWindow.DoAllocateWindowHandle(AParent: TfpgWidgetBase);
begin
end;

procedure   TfpgCocoaWindow.DoReleaseWindowHandle;
begin
end;

procedure   TfpgCocoaWindow.DoRemoveWindowLookup;
begin
end;

procedure   TfpgCocoaWindow.DoSetWindowAttributes(const AOldAtributes, ANewAttributes: TWindowAttributes; const AForceAll: Boolean);
begin
end;

procedure   TfpgCocoaWindow.DoSetWindowVisible(const AValue: Boolean);
begin
end;

procedure   TfpgCocoaWindow.DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord);
begin
end;

function    TfpgCocoaWindow.DoWindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint;
begin
end;

procedure   TfpgCocoaWindow.DoSetWindowTitle(const ATitle: string);
begin
end;

procedure   TfpgCocoaWindow.DoSetMouseCursor;
begin
end;

procedure   TfpgCocoaWindow.DoDNDEnabled(const AValue: boolean);
begin
end;

{ TfpgCocoaApplication }

function    TfpgCocoaApplication.DoGetFontFaceList: TStringList;
begin
end;

procedure   TfpgCocoaApplication.DoWaitWindowMessage(atimeoutms: integer);
begin
end;

function    TfpgCocoaApplication.MessagesPending: boolean;
begin
end;

procedure   TfpgCocoaApplication.DoFlush;
begin
end;

function    TfpgCocoaApplication.GetScreenWidth: TfpgCoord;
begin
end;

function    TfpgCocoaApplication.GetScreenHeight: TfpgCoord;
begin
end;

function    TfpgCocoaApplication.GetScreenPixelColor(APos: TPoint): TfpgColor;
begin
end;

function    TfpgCocoaApplication.Screen_dpi_x: integer;
begin
end;

function    TfpgCocoaApplication.Screen_dpi_y: integer;
begin
end;

function    TfpgCocoaApplication.Screen_dpi: integer;
begin
end;

{ TfpgCocoaClipboard }

function    TfpgCocoaClipboard.DoGetText: TfpgString;
begin
end;

procedure   TfpgCocoaClipboard.DoSetText(const AValue: TfpgString);
begin
end;

procedure   TfpgCocoaClipboard.InitClipboard;
begin
end;

{ TfpgCocoaDrag }

function    TfpgCocoaDrag.Execute(const ADropActions: TfpgDropActions; const ADefaultAction: TfpgDropAction=daCopy): TfpgDropAction;
begin
end;

{ TfpgCocoaDrop }

function    TfpgCocoaDrop.GetDropAction: TfpgDropAction;
begin
end;

procedure   TfpgCocoaDrop.SetDropAction(AValue: TfpgDropAction);
begin
end;

function    TfpgCocoaDrop.GetWindowForDrop: TfpgWindowBase;
begin
end;

{ TfpgCocoaCanvas }

procedure   TfpgCocoaCanvas.DoSetFontRes(fntres: TfpgFontResourceBase);
begin
end;

procedure   TfpgCocoaCanvas.DoSetTextColor(cl: TfpgColor);
begin
end;

procedure   TfpgCocoaCanvas.DoSetColor(cl: TfpgColor);
begin
end;

procedure   TfpgCocoaCanvas.DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle);
begin
end;

procedure   TfpgCocoaCanvas.DoFillRectangle(x, y, w, h: TfpgCoord);
begin
end;

procedure   TfpgCocoaCanvas.DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord);
begin
end;

procedure   TfpgCocoaCanvas.DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord);
begin
end;

procedure   TfpgCocoaCanvas.DoDrawRectangle(x, y, w, h: TfpgCoord);
begin
end;

procedure   TfpgCocoaCanvas.DoDrawLine(x1, y1, x2, y2: TfpgCoord);
begin
end;

procedure   TfpgCocoaCanvas.DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer);
begin
end;

procedure   TfpgCocoaCanvas.DoDrawString(x, y: TfpgCoord; const txt: string);
begin
end;

procedure   TfpgCocoaCanvas.DoSetClipRect(const ARect: TfpgRect);
begin
end;

function    TfpgCocoaCanvas.DoGetClipRect: TfpgRect;
begin
end;

procedure   TfpgCocoaCanvas.DoAddClipRect(const ARect: TfpgRect);
begin
end;

procedure   TfpgCocoaCanvas.DoClearClipRect;
begin
end;

procedure   TfpgCocoaCanvas.DoBeginDraw(awidget: TfpgWidgetBase; CanvasTarget: TfpgCanvasBase);
begin
end;

procedure   TfpgCocoaCanvas.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
begin
end;

procedure   TfpgCocoaCanvas.DoEndDraw;
begin
end;

function    TfpgCocoaCanvas.GetPixel(X, Y: integer): TfpgColor;
begin
end;

procedure   TfpgCocoaCanvas.SetPixel(X, Y: integer; const AValue: TfpgColor);
begin
end;

procedure   TfpgCocoaCanvas.DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
begin
end;

procedure   TfpgCocoaCanvas.DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
begin
end;

procedure   TfpgCocoaCanvas.DoDrawPolygon(const Points: array of TPoint);
begin
end;

function    TfpgCocoaCanvas.GetBufferAllocated: Boolean;
begin
end;

procedure   TfpgCocoaCanvas.DoAllocateBuffer;
begin
end;

{ TfpgCocoaSystemTrayHandler }

procedure   TfpgCocoaSystemTrayHandler.Show;
begin
end;

procedure   TfpgCocoaSystemTrayHandler.Hide;
begin
end;

function    TfpgCocoaSystemTrayHandler.IsSystemTrayAvailable: boolean;
begin
end;

function    TfpgCocoaSystemTrayHandler.SupportsMessages: boolean;
begin
end;

end.
