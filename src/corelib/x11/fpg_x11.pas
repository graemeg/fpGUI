{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2011 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit implements X11 / Xlib support for fpGUI.
}

unit fpg_x11;

{$mode objfpc}{$H+}

{.$Define DEBUG}      // general debugging - mostly OS messages though
{.$Define DNDDEBUG}   // drag-n-drop specific debugging

{ TODO : Compiz effects: Menu popup with correct window hint. Same for Combo dropdown window. }
{ TODO : Under Compiz restoring a window position moves the window down/right the width and height
         of the window borders. This as something to do with win_gravity = StaticGravity setting. }

interface

uses
  Classes,
  SysUtils,
  contnrs,
  X,
  Xlib,
  XUtil,
  ctypes,
  fpg_xft_x11,
  fpg_netlayer_x11,
  fpg_base,
  fpg_impl;

const
  IconBitmapWidth = 16;
  IconBitmapHeight = 16;

  IconBitmapBits: packed array[1..32] of Byte = (
     $00, $00, $78, $07, $08, $09, $38, $09, $08, $07, $08, $01,
     $08, $01, $00, $00, $00, $00, $98, $74, $a4, $24, $84, $24,
     $b4, $24, $a4, $24, $18, $73, $00, $00);

type

  TfpgGContext  = Xlib.TGc;
  PInt = ^integer;

  TAtomArray = array[0..0] of TAtom;
  PAtomArray = ^TAtomArray;

  TWindowArray = array[0..0] of TWindow;
  PWindowArray = ^TWindowArray;

  {$HINTS OFF}
  TXIC = record
    dummy: Pointer;
  end;
  PXIC = ^TXIC;


  TXIM = record
    dummy: Pointer;
  end;
  PXIM = ^TXIM;
  {$HINTS ON}


  TXdbeSwapInfo = record
    Window: TfpgWinHandle;
    SwapAction: PChar;
  end;
  PXdbeSwapInfo = ^TXdbeSwapInfo;

  // MWM support
  TMWMHints = record
    flags: culong;
    functions: culong;
    decorations: culong;
    input_mode: longint;
    status: culong;
  end;


const
// Motif window hints
  MWM_HINTS_FUNCTIONS     = 1 shl 0;
  MWM_HINTS_DECORATIONS   = 1 shl 1;
  MWM_HINTS_INPUT_MODE    = 1 shl 2;
  MWM_HINTS_STATUS        = 1 shl 3;
// bit definitions for MwmHints.functions
  MWM_FUNC_ALL            = 1 shl 0;
  MWM_FUNC_RESIZE         = 1 shl 1;
  MWM_FUNC_MOVE           = 1 shl 2;
  MWM_FUNC_MINIMIZE       = 1 shl 3;
  MWM_FUNC_MAXIMIZE       = 1 shl 4;
  MWM_FUNC_CLOSE          = 1 shl 5;
// bit definitions for MwmHints.decorations
  MWM_DECOR_ALL           = 1 shl 0;
  MWM_DECOR_BORDER        = 1 shl 1;
  MWM_DECOR_RESIZEH       = 1 shl 2;
  MWM_DECOR_TITLE         = 1 shl 3;
  MWM_DECOR_MENU          = 1 shl 4;
  MWM_DECOR_MINIMIZE      = 1 shl 5;
  MWM_DECOR_MAXIMIZE      = 1 shl 6;
// bit definitions for MwmHints.inputMode
  MWM_INPUT_MODELESS                  = 0;
  MWM_INPUT_PRIMARY_APPLICATION_MODAL = 1;
  MWM_INPUT_SYSTEM_MODAL              = 2;
  MWM_INPUT_FULL_APPLICATION_MODAL    = 3;
  PROP_MWM_HINTS_ELEMENTS             = 5;
// System Tray message opcodes
  SYSTEM_TRAY_REQUEST_DOCK   = 0;
  SYSTEM_TRAY_BEGIN_MESSAGE  = 1;
  SYSTEM_TRAY_CANCEL_MESSAGE = 2;

type
  TXWindowStateFlag = (xwsfMapped);
  TXWindowStateFlags = set of TXWindowStateFlag;

  // Returns True if it 'ate' the event
  TX11EventFilter = function(const AEvent: TXEvent): Boolean of object;

  // forward declaration
  TfpgX11Window = class;
  TfpgX11Drag = class;


  TfpgX11FontResource = class(TfpgFontResourceBase)
  private
    FFontData: PXftFont;
    function    DoGetTextWidthClassic(const txt: string): integer;
    function    DoGetTextWidthWorkaround(const txt: string): integer;
  protected
    property    Handle: PXftFont read FFontData;
  public
    constructor Create(const afontdesc: string);
    destructor  Destroy; override;
    function    HandleIsValid: boolean;
    function    GetAscent: integer; override;
    function    GetDescent: integer; override;
    function    GetHeight: integer; override;
    function    GetTextWidth(const txt: string): integer; override;
  end;


  TfpgX11Image = class(TfpgImageBase)
  private
    FXimg: TXImage;
    FXimgmask: TXImage;
  protected
    function    XImage: PXImage;
    function    XImageMask: PXImage;
    procedure   DoFreeImage; override;
    procedure   DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: Pointer); override;
    procedure   DoInitImageMask(awidth, aheight: integer; aimgdata: Pointer); override;
  public
    constructor Create;
  end;


  TfpgX11Canvas = class(TfpgCanvasBase)
  private
    FDrawing: boolean;
    FDrawWindow: TfpgX11Window;
    FBufferPixmap: TfpgDCHandle;
    FDrawHandle: TfpgDCHandle;
    Fgc: TfpgGContext;
    FCurFontRes: TfpgX11FontResource;
    FClipRect: TfpgRect;
    FClipRectSet: boolean;
    FXftDraw: PXftDraw;
    FColorTextXft: TXftColor;
    FClipRegion: TRegion;
    FPixHeight,
    FPixWidth: Integer;
    FBufferFreeTimer: TObject;
    procedure   BufferFreeTimer(Sender: TObject);
    procedure   TryFreePixmap;
  protected
    procedure   DoSetFontRes(fntres: TfpgFontResourceBase); override;
    procedure   DoSetTextColor(cl: TfpgColor); override;
    procedure   DoSetColor(cl: TfpgColor); override;
    procedure   DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle); override;
    procedure   DoGetWinRect(out r: TfpgRect); override;
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
    procedure   DoBeginDraw(awin: TfpgWindowBase; buffered: boolean); override;
    procedure   DoPutBufferToScreen(x, y, w, h: TfpgCoord); override;
    procedure   DoEndDraw; override;
    function    GetPixel(X, Y: integer): TfpgColor; override;
    procedure   SetPixel(X, Y: integer; const AValue: TfpgColor); override;
    procedure   DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: Extended); override;
    procedure   DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended); override;
    procedure   DoDrawPolygon(Points: PPoint; NumPts: Integer; Winding: boolean=False); override;
    property    DCHandle: TfpgDCHandle read FDrawHandle;
  public
    constructor Create(awin: TfpgWindowBase); override;
    destructor  Destroy; override;
  end;


  TfpgX11Window = class(TfpgWindowBase)
  private
    QueueEnabledDrops: boolean;
  protected
    FWinFlags: TXWindowStateFlags;
    FWinHandle: TfpgWinHandle;
    FBackupWinHandle: TfpgWinHandle;  // Used by DestroyNotify & UnmapNotify events
    FModalForWin: TfpgX11Window;
    procedure   DoAllocateWindowHandle(AParent: TfpgWindowBase); override;
    procedure   DoReleaseWindowHandle; override;
    procedure   DoRemoveWindowLookup; override;
    procedure   DoSetWindowVisible(const AValue: Boolean); override;
    function    HandleIsValid: boolean; override;
    procedure   DoSetWindowTitle(const ATitle: string); override;
    procedure   DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord); override;
    function    DoWindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint; override;
    procedure   DoUpdateWindowPosition; override;
    procedure   DoSetMouseCursor; override;
    procedure   DoDNDEnabled(const AValue: boolean); override;
    procedure   DoAcceptDrops(const AValue: boolean); override;
    property    WinHandle: TfpgWinHandle read FWinHandle;
    function    GetWindowState: TfpgWindowState; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   ActivateWindow; override;
    procedure   CaptureMouse; override;
    procedure   ReleaseMouse; override;
    procedure   SetFullscreen(AValue: Boolean); override;
    procedure   BringToFront; override;
  end;


  TfpgX11Application = class(TfpgApplicationBase)
  private
    FComposeBuffer: TfpgString;
    FComposeStatus: TStatus;
    FEventFilter: TX11EventFilter;
    { XDND type list received from Source window }
    FDNDTypeList: TObjectList;
    { all XDND atoms }
    XdndAware: TAtom;
    XdndTypeList: TAtom;
    XdndSelection: TAtom;
    { XDND client messages }
    XdndEnter: TAtom;
    XdndPosition: TAtom;
    XdndStatus: TAtom;
    XdndLeave: TAtom;
    XdndDrop: TAtom;
    XdndFinished: TAtom;
    { XDND actions }
    XdndActionCopy: TAtom;
    XdndActionMove: TAtom;
    XdndActionLink: TAtom;
    XdndActionAsk: TAtom;
    XdndActionPrivate: TAtom;
    { XDND variables }
    FActionType: TAtom;
    FSrcWinHandle: TfpgWinHandle;
    FDNDVersion: integer;
    FDNDDataType: TAtom;
    FSrcTimeStamp: clong;
    FLastDropTarget: TfpgWinHandle;
    FDropPos: TPoint;
    FDrag: TfpgX11Drag;
    { X11 window grouping }
    FLeaderWindow: TfpgWinHandle;
    FClientLeaderAtom: TAtom;
    procedure   SetDrag(const AValue: TfpgX11Drag);
    function    ConvertShiftState(AState: Cardinal): TShiftState;
    function    KeySymToKeycode(KeySym: TKeySym): Word;
    function    StartComposing(const Event: TXEvent): TKeySym;
    function    GetDropActionFromAtom(const AAtom: TAtom): TfpgDropAction;
    function    GetAtomFromDropAction(const AAction: TfpgDropAction): TAtom;
    procedure   XdndInit;
    procedure   ResetDNDVariables;
    procedure   HandleDNDenter(ATopLevelWindow: TfpgX11Window; const ASource: TWindow; const ev: TXEvent);
    procedure   HandleDNDleave(ATopLevelWindow: TfpgX11Window; const ASource: TWindow);
    procedure   HandleDNDposition(ATopLevelWindow: TfpgX11Window; const ASource: TWindow; const x_root: integer; const y_root: integer; const AAction: TAtom; const ATimestamp: x.TTime);
    procedure   HandleDNDdrop(ATopLevelWindow: TfpgX11Window; const ASource: TWindow; const ATimestamp: x.TTime);
    procedure   HandleDNDSelection(const ev: TXEvent);
    property    Drag: TfpgX11Drag read FDrag write SetDrag;
  protected
    FDisplay: PXDisplay;
    DisplayDepth: integer;
    DefaultBackground: TfpgColor;
    DefaultForeground: TfpgColor;
    DefaultScreen: integer;
    DefaultVisual: PVisual;
    DefaultColorMap: TColorMap;
    FRootWindow: TfpgWinHandle;
    xia_clipboard: TAtom;
    xia_motif_wm_hints: TAtom;
    xia_wm_protocols: TAtom;
    xia_wm_delete_window: TAtom;
    xia_wm_state: TAtom;
    xia_targets: TAtom;
    netlayer: TNETWindowLayer;
    InputMethod: PXIM;
    InputContext: PXIC;
    FLastKeySym: TKeySym;   // Used for KeyRelease event
    function    DoGetFontFaceList: TStringList; override;
    procedure   DoWaitWindowMessage(atimeoutms: integer); override;
    function    MessagesPending: boolean; override;
  public
    constructor Create(const AParams: string); override;
    destructor  Destroy; override;
    procedure   DoFlush;
    function    GetScreenWidth: TfpgCoord; override;
    function    GetScreenHeight: TfpgCoord; override;
    function    Screen_dpi_x: integer; override;
    function    Screen_dpi_y: integer; override;
    function    Screen_dpi: integer; override;
    property    Display: PXDisplay read FDisplay;
    property    RootWindow: TfpgWinHandle read FRootWindow; platform;
    property    EventFilter: TX11EventFilter read FEventFilter write FEventFilter; platform;
  end;


  TfpgX11Clipboard = class(TfpgClipboardBase)
  private
    FWaitingForSelection: Boolean;
  protected
    FClipboardText: TfpgString;
    function    DoGetText: TfpgString; override;
    procedure   DoSetText(const AValue: TfpgString); override;
    procedure   InitClipboard; override;
  end;


  TfpgX11FileList = class(TfpgFileListBase)
  protected
    function    InitializeEntry(sr: TSearchRec): TFileEntry; override;
    procedure   PopulateSpecialDirs(const aDirectory: TfpgString); override;
  public
    constructor Create; override;
    function    EncodeModeString(FileMode: longword): TFileModeString;
  end;


  TfpgX11MimeData = class(TfpgMimeDataBase)
  end;


  TfpgX11Drag = class(TfpgDragBase)
  private
    FLastTarget: TfpgWinHandle;
    FUseVersion: integer;
    FTargetIsDNDAware: Boolean;
    FStatusPending: Boolean;
    FDropAccepted: Boolean;
    FProposedAction: TAtom;
    FAcceptedAction: TAtom;
    FMimeTypesArray: array of TAtom;
    xia_plain_text: TAtom;
    procedure   SetTypeListProperty;
    procedure   InitializeMimeTypesToAtoms;
    procedure   Dragging(ev: TXEvent);
    function    IsDNDAware(win: TWindow): boolean;
    procedure   SendDNDLeave(ATarget: TWindow);
    procedure   SendDNDEnter(ATarget: TWindow);
    procedure   SendDNDPosition(ATarget: TWindow; x_root: cint; y_root: cint; AAction: TAtom; ATime: X.TTime);
    procedure   SendDNDDrop;
    procedure   HandleDNDStatus(ATarget: TWindow; AAccept: integer; ARect: TfpgRect; AAction: TAtom);
    procedure   HandleSelectionRequest(ev: TXEvent);
  protected
    FSource: TfpgX11Window;
    function    GetSource: TfpgX11Window; virtual;
  public
    destructor  Destroy; override;
    function    Execute(const ADropActions: TfpgDropActions; const ADefaultAction: TfpgDropAction = daCopy): TfpgDropAction; override;
  end;


  TfpgX11Timer = class(TfpgBaseTimer)
  end;


  TfpgX11SystemTrayHandler = class(TfpgComponent)
  private
    FTrayIconParent: TWindow;
    FTrayWidget: TfpgWindowBase;
    function    GetTrayIconParent: TWindow;
    function    GetSysTrayWindow: TWindow;
    function    Send_Message(dest: TWindow; msg: longword; data1, data2, data3: longword): boolean;
    property    TrayIconParent: TWindow read GetTrayIconParent;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   Show;
    procedure   Hide;
    function    IsSystemTrayAvailable: boolean;
    function    SupportsMessages: boolean;
  end;


function fpgColorToX(col: TfpgColor): longword;


implementation

uses
  baseunix,
  {$IFDEF LINUX}
  users,  { For Linux user and group name support. FPC only supports this in Linux. }
  {$ENDIF}
  fpg_main,
  fpg_widget,
  fpg_popupwindow,
  fpg_stringutils,  // used for GetTextWidth
  fpg_utils,
  fpg_form,         // for modal event support
  fpg_cmdlineparams,
  cursorfont,
  xatom,            // used for XA_WM_NAME
  keysym,
  math;

type
  TDNDSrcType = class(TObject)
  public
    Name: TfpgString;
    ID: integer;
  end;

var
  xapplication: TfpgApplication;
  uDragSource: TfpgWidget;  { points to the Source widget of the DND when drop is inside the same app }

const
  FPG_XDND_VERSION: culong = 4; // our supported XDND version

 // some externals

// defines:
procedure XRenderSetPictureClipRectangles(disp: PXDisplay; pic: TPicture; xorigin, yorigin: integer; rect: PXRectangle; num: integer); cdecl; external;

// redefines:
function XmbLookupString(p1: PXIC; ev: PXKeyPressedEvent; str: PChar; len: longword; ks: PKeySym; stat: PStatus): longint; cdecl; external;
function Xutf8LookupString(p1: PXIC; ev: PXKeyPressedEvent; str: PChar; len: longword; ks: PKeySym; stat: PStatus): longint; cdecl; external;

// Double buffer functions
function XdbeQueryExtension(ADisplay: PXDisplay; AMajor, AMinor: PInt): PStatus; cdecl; external;
function XdbeAllocateBackBufferName(ADisplay: PXDisplay; AWindow: TfpgWinHandle; ASwapAction: PChar): TfpgWinHandle; cdecl; external;
function XdbeSwapBuffers(ADisplay: PXDisplay; ASwapInfo: PXdbeSwapInfo; AScreenNums: integer): PStatus; cdecl; external;
function XdbeDeallocateBackBufferName(ADisplay: PXDisplay; ABuffer: TfpgWinHandle): PStatus; cdecl; external;

function XOpenIM(para1: PDisplay; para2: PXrmHashBucketRec; para3: Pchar; para4: Pchar): PXIM; cdecl; external;
function XCreateIC(para1: PXIM; para2: array of const): PXIC; cdecl; external;

const
  AltGrMask = 1 shl 13;  // missing from X unit

function ConvertTo565Pixel(rgb: longword): word;
begin
  Result := (rgb and $F8) shr 3;
  Result := Result or ((rgb and $FC00) shr 5);
  Result := Result or ((rgb and $F80000) shr 8);
end;

function fpgColorToX(col: TfpgColor): longword;
var
  xc: TXColor;
  c: TfpgColor;
begin
  c := fpgColorToRGB(col);

  if xapplication.DisplayDepth >= 24 then
    Result   := c and $FFFFFF       { No Alpha channel information }
  else if xapplication.DisplayDepth = 16 then
    Result   := ConvertTo565Pixel(c)
  else
  begin
    c        := col;
    xc.blue  := (c and $000000FF) shl 8;
    xc.green := (c and $0000FF00);
    xc.red   := (c and $00FF0000) shr 8;

    // THIS CALL IS TOO SLOW !!!!!:
    XAllocColor(xapplication.display, xapplication.DefaultColorMap, @xc);
    Result := xc.pixel;
  end;
end;

procedure SetXftColor(col: TfpgColor; var colxft: TXftColor);
var
  c: TfpgColor;
begin
  c := fpgColorToRGB(col);

  colxft.color.blue  := (c and $000000FF) shl 8;
  colxft.color.green := (c and $0000FF00);
  colxft.color.red   := (c and $00FF0000) shr 8;

  colxft.color.alpha := (c and $7F000000) shr 15;
  colxft.color.alpha := colxft.color.alpha xor $FFFF;  // invert: 0 means not translucent

  colxft.pixel := 0;
end;

type
  PWindowLookupRec = ^WindowLookupRec;

  // single direction linked list
  WindowLookupRec = record
    w: TfpgX11Window;
    Next: PWindowLookupRec;
  end;

var
  FirstWindowLookupRec: PWindowLookupRec;
  LastWindowLookupRec: PWindowLookupRec;

procedure AddWindowLookup(w: TfpgX11Window);
var
  p: PWindowLookupRec;
begin
  if w = nil then
    Exit;

  New(p);
  p^.w    := w;
  p^.Next := nil;
  if FirstWindowLookupRec = nil then
    FirstWindowLookupRec := p
  else
    LastWindowLookupRec^.Next := p;
  LastWindowLookupRec := p;
end;

procedure RemoveWindowLookup(w: TfpgX11Window);
var
  prevp: PWindowLookupRec;
  p: PWindowLookupRec;
  px: PWindowLookupRec;
begin
  p     := FirstWindowLookupRec;
  prevp := nil;

  while p <> nil do
    if p^.w = w then
    begin
      if prevp = nil then
        FirstWindowLookupRec := p^.Next
      else
        prevp^.Next          := p^.Next;
      if LastWindowLookupRec = p then
        LastWindowLookupRec  := prevp;
      px := p;
      p := p^.Next;
      Dispose(px);
    end
    else
    begin
      prevp := p;
      p     := p^.Next;
    end;
end;

function FindWindowByHandle(wh: TfpgWinHandle): TfpgX11Window;
var
  p: PWindowLookupRec;
begin
  p := FirstWindowLookupRec;
  while p <> nil do
  begin
    if p^.w.WinHandle = wh then
    begin
      Result := p^.w;
      Exit;
    end;
    p := p^.Next;
  end;
  {$IFDEF DEBUG}
  writeln('fpGUI/X11: FindWindowByHandle failed to find <', IntToHex(wh, 9), '>');
  {$ENDIF}
  Result := nil;
end;

function FindWindowByBackupHandle(wh: TfpgWinHandle): TfpgX11Window;
var
  p: PWindowLookupRec;
begin
  p := FirstWindowLookupRec;
  while p <> nil do
  begin
    if p^.w.FBackupWinHandle = wh then
    begin
      Result := p^.w;
      Exit;
    end;
    p := p^.Next;
  end;
  {$IFDEF DEBUG}
  writeln('fpGUI/X11: FindWindowByBackupHandle failed to find <', IntToHex(wh, 9), '>');
  {$ENDIF}
  Result := nil;
end;

function GetXEventName(Event: longint): string;
const
  EventNames: array[2..34] of string = (
    'KeyPress', 'KeyRelease', 'ButtonPress', 'ButtonRelease', 'MotionNotify',
    'EnterNotify', 'LeaveNotify', 'FocusIn', 'FocusOut', 'KeymapNotify',
    'Expose', 'GraphicsExpose', 'NoExpose', 'VisibilityNotify', 'CreateNotify',
    'DestroyNotify', 'UnmapNotify', 'MapNotify', 'MapRequest', 'ReparentNotify',
    'ConfigureNotify', 'ConfigureRequest', 'GravityNotify', 'ResizeRequest',
    'CirculateNotify', 'CirculateRequest', 'PropertyNotify', 'SelectionClear',
    'SelectionRequest', 'SelectionNotify', 'ColormapNotify', 'ClientMessage',
    'MappingNotify');
begin
  if (Event >= Low(EventNames)) and (Event <= High(EventNames)) then
    Result := EventNames[Event]
  else
    Result := '#' + IntToStr(Event);
end;

// clipboard event
procedure ProcessSelection(var ev: TXEvent);
var
  s: string;
  actualformat: TAtom;
  actualtype: cint;
  count, remaining: culong;
  data: PChar;
begin
  if ev.xselection._property > 0 then
  begin
    XGetWindowProperty(xapplication.Display, ev.xselection.requestor,
        ev.xselection._property, 0, 16000,
        TBool(false), // delete
        0, // type
        @actualformat, @actualtype, @count, @remaining,
        @data);
    s := data;

    fpgClipboard.FClipboardText := s;
    XFree(data);
  end
  else
  begin
    fpgClipboard.FClipboardText := '';
  end;

  fpgClipboard.FWaitingForSelection := false;
end;

// clipboard event
procedure ProcessSelectionRequest(var ev: TXEvent);
var
  e: TXSelectionEvent;
  a: TAtom;
begin
  e._type       := SelectionNotify;
  e.requestor   := ev.xselectionrequest.requestor;
  e.selection   := ev.xselectionrequest.selection;
  e.target      := ev.xselectionrequest.target;
  e.time        := ev.xselectionrequest.time;
  e._property   := ev.xselectionrequest._property;

  if e.target = xapplication.xia_targets then
  begin
    a := XA_STRING;
    XChangeProperty(xapplication.Display, e.requestor, e._property, XA_ATOM,
        32, PropModeReplace, PByte(@a), Sizeof(TAtom)); // I think last parameter is right?
  end
  else
  begin
    XChangeProperty(xapplication.Display, e.requestor, e._property, e.target,
        8, PropModeReplace, PByte(@fpgClipboard.FClipboardText[1]), Length(fpgClipboard.FClipboardText));
  end;

  XSendEvent(xapplication.Display, e.requestor, false, 0, @e );
end;

function IsTopLevel(AWin: TWindow): Boolean;
var
  actualtype: TAtom = None;
  actualformat: cint;
  count, remaining: culong;
  data: pchar = nil;
begin
  XGetWindowProperty(xapplication.Display, AWin, xapplication.xia_wm_state, 0, 0,
      TBool(False), AnyPropertyType, @actualtype, @actualformat, @count,
      @remaining, @data);
  if data <> nil then
    XFree(data);
  Result := actualtype <> None;
end;

{ find toplevel window that contains mouse co-ordinates x, y (co-ordinates are
  from root window) }
function FindWindow(ARoot: TWindow; const x, y: cint): TWindow;
var
  wattr: TXWindowAttributes;
  r, p: TWindow;
  children: PWindowArray = nil;
  numchildren: cuint = 0;
  i: integer;
begin
  XGetWindowAttributes(xapplication.Display, ARoot, @wattr);
  if (wattr.map_state <> IsUnmapped) and
      ((x >= wattr.x) and (x < (wattr.x + wattr.width))) and
      ((y >= wattr.y) and (y < (wattr.y + wattr.height))) then
  begin
    // mapped and inside, is it a top-level?
    if (IsTopLevel(ARoot)) then
    begin
      Result := ARoot;
      exit;
    end;
    if XQueryTree(xapplication.Display, ARoot, @r, @p, @children, @numchildren) <> 0 then
    begin
      if (numchildren > 0) then
      begin
        r := None;
        { upon return from XQueryTree, children are listed in the current
          stacking order, from bottom-most (first) to top-most (last) }
        for i := numchildren-1 downto 0 do
        begin
          r := FindWindow(children^[i], x - wattr.x, y - wattr.y);
          if r <> None then
            break;
        end;

        XFree(children);
        if r <> None then
        begin
          Result := r;
          exit;
        end;
        Result := ARoot;   // a fallback Result - we should never get here though
      end;
    end;
  end
  else
    Result := None;
end;

procedure SetWindowGroup(AWindow: TfpgWinHandle);
var
  ClassHint: PXClassHint;
begin
  ClassHint := XAllocClassHint;
  ClassHint^.res_name := PChar(fpgGetExecutableName);
  ClassHint^.res_class := PChar(ApplicationName);
  XSetClassHint(xapplication.display, AWindow, ClassHint);
  XFree(ClassHint);
end;

// File utils
function ExtractTargetSymLinkPath(ALink: string): string;
begin
  Result := fpReadLink(ALink);
end;

function FileIsSymlink(const AFilename: string): boolean;
begin
  Result := (FpReadLink(AFilename) <> '');
end;


{ TfpgX11Application }

procedure TfpgX11Application.SetDrag(const AValue: TfpgX11Drag);
begin
  if Assigned(FDrag) then
    FDrag.Free;
  FDrag := AValue;
end;

function TfpgX11Application.ConvertShiftState(AState: Cardinal): TShiftState;
begin
  Result := [];
  if (AState and Button1Mask) <> 0 then
    Include(Result, ssLeft);
  if (AState and Button2Mask) <> 0 then
    Include(Result, ssMiddle);
  if (AState and Button3Mask) <> 0 then
    Include(Result, ssRight);
  if (AState and ShiftMask) <> 0 then
    Include(Result, ssShift);
  if (AState and LockMask) <> 0 then
    Include(Result, ssCaps);
  if (AState and ControlMask) <> 0 then
    Include(Result, ssCtrl);
  if (AState and Mod1Mask) <> 0 then
    Include(Result, ssAlt);
  { Numlock is often permanently enabled after booting, so this always gives
    use the incorrect ShiftState. So we simply stop checking ssNum as part
    of the ShiftState. }
//  if (AState and Mod2Mask) <> 0 then
//    Include(Result, ssNum);
  { NOTE: Mod3Mask is normally unused for some reason }
  if (AState and Mod4Mask) <> 0 then   { aka "Windows key" }
    Include(Result, ssSuper);
  if (AState and Mod5Mask) <> 0 then
    Include(Result, ssScroll);
  if (AState and AltGrMask) <> 0 then
    Include(Result, ssAltGr);
end;

function TfpgX11Application.KeySymToKeycode(KeySym: TKeySym): Word;
const
  Table_20aX: array[$20a0..$20ac] of Word = (keyEcuSign, keyColonSign,
    keyCruzeiroSign, keyFFrancSign, keyLiraSign, keyMillSign, keyNairaSign,
    keyPesetaSign, keyRupeeSign, keyWonSign, keyNewSheqelSign, keyDongSign,
    keyEuroSign);
  Table_feXX: array[$fe50..$fe60] of Word = (keyDeadGrave, keyDeadAcute,
    keyDeadCircumflex, keyDeadTilde, keyDeadMacron,keyDeadBreve,
    keyDeadAbovedot, keyDeadDiaeresis, keyDeadRing, keyDeadDoubleacute,
    keyDeadCaron, keyDeadCedilla, keyDeadOgonek, keyDeadIota,
    keyDeadVoicedSound, keyDeadSemivoicedSound, keyDeadBelowdot);
  Table_ff5X: array[$ff50..$ff58] of Word = (keyHome, keyLeft, keyUp, keyRight,
    keyDown, keyPrior, keyNext, keyEnd, keyBegin);
  Table_ff6X: array[$ff60..$ff6b] of Word = (keySelect, keyPrintScreen,
    keyExecute, keyInsert, keyNIL, keyUndo, keyRedo, keyMenu, keyFind,
    keyCancel, keyHelp, keyBreak);
  Table_ff9X: array[$ff91..$ff9f] of Word = (keyPF1, keyPF2, keyPF3, keyPF4,
    keyP7, keyP4, keyP8, keyP6, keyP2, keyP9, keyP3, keyP1, keyP5, keyP0,
    keyPDecimal);
  Table_ffeX: array[$ffe1..$ffee] of Word = (keyShiftL, keyShiftR, keyCtrlL,
    keyCtrlR, keyCapsLock, keyShiftLock, keyMetaL, keyMetaR, keyAltL, keyAltR,
    keySuperL, keySuperR, keyHyperL, keyHyperR);
begin
  case KeySym of
    0..Ord('a')-1, Ord('z')+1..$bf, $f7:
      Result := KeySym;
    Ord('a')..Ord('z'), $c0..$f6, $f8..$ff:
      Result := KeySym - 32;  // ignore case: convert lowercase a-z to A-Z keysyms;
    $20a0..$20ac: Result := Table_20aX[KeySym];
    $fe20: Result := keyTab;
    $fe50..$fe60: Result := Table_feXX[KeySym];
    XK_BackSpace:   Result := keyBackspace;
    XK_Tab:         Result := keyTab;
    XK_Linefeed:    Result := keyLinefeed;
    $ff0b: Result := keyClear;
    $ff0d: Result := keyReturn;
    $ff13: Result := keyPause;
    $ff14: Result := keyScrollLock;
    $ff15: Result := keySysRq;
    $ff1b: Result := keyEscape;
    $ff50..$ff58: Result := Table_ff5X[KeySym];
    $ff60..$ff6b: Result := Table_ff6X[KeySym];
    $ff7e: Result := keyModeSwitch;
    $ff7f: Result := keyNumLock;
    $ff80: Result := keyPSpace;
    $ff89: Result := keyPTab;
    $ff8d: Result := keyPEnter;
    $ff91..$ff9f: Result := Table_ff9X[KeySym];
    $ffaa: Result := keyPAsterisk;
    $ffab: Result := keyPPlus;
    $ffac: Result := keyPSeparator;
    $ffad: Result := keyPMinus;
    $ffae: Result := keyPDecimal;
    $ffaf: Result := keyPSlash;
    $ffb0..$ffb9: Result := keyP0 + KeySym - $ffb0;
    $ffbd: Result := keyPEqual;
    $ffbe..$ffe0: Result := keyF1 + KeySym - $ffbe;
    $ffe1..$ffee: Result := Table_ffeX[KeySym];
    $ffff: Result := keyDelete;
  else
    Result := keyNIL;
  end;

{$IFDEF Debug}
  if Result = keyNIL then
    WriteLn('fpGFX/X11: Unknown KeySym: $', IntToHex(KeySym, 4));
{$ENDIF}
end;

function TfpgX11Application.StartComposing(const Event: TXEvent): TKeySym;
var
  l: integer;
begin
  SetLength(FComposeBuffer, 20); // buffer set to some default size
  // Xutf8LookupString returns the size of FComposeBuffer in bytes.
  l := Xutf8LookupString(InputContext, @Event.xkey, @FComposeBuffer[1],
        Length(FComposeBuffer), @Result, @FComposeStatus);
  SetLength(FComposeBuffer, l);
  // if overflow occured, then previous SetLength() would have fixed the buffer
  // size, so run Xutf8LookupString again to read correct value.
  if FComposeStatus = XBufferOverflow then
    Xutf8LookupString(InputContext, @Event.xkey, @FComposeBuffer[1],
        Length(FComposeBuffer), @Result, @FComposeStatus);
end;

function TfpgX11Application.GetDropActionFromAtom(const AAtom: TAtom): TfpgDropAction;
begin
  if AAtom = XdndActionCopy then
    Result := daCopy
  else if AAtom = XdndActionMove then
    Result := daMove
  else if AAtom = XdndActionLink then
    Result := daLink
  else
    Result := daCopy; { the safe fallback option }
end;

function TfpgX11Application.GetAtomFromDropAction(const AAction: TfpgDropAction): TAtom;
begin
  case AAction of
    daCopy:  Result := XdndActionCopy;
    daMove:  Result := XdndActionMove;
    daLink:  Result := XdndActionLink;
    else
      Result := XdndActionCopy;  { the safe fallback option }
  end;
end;

procedure TfpgX11Application.XdndInit;
begin
  XdndAware         := XInternAtom(FDisplay, 'XdndAware',         False);
  XdndTypeList      := XInternAtom(FDisplay, 'XdndTypeList',      False);
  XdndSelection     := XInternAtom(FDisplay, 'XdndSelection',     False);

  // client messages
  XdndEnter         := XInternAtom(FDisplay, 'XdndEnter',         False);
  XdndPosition      := XInternAtom(FDisplay, 'XdndPosition',      False);
  XdndStatus        := XInternAtom(FDisplay, 'XdndStatus',        False);
  XdndLeave         := XInternAtom(FDisplay, 'XdndLeave',         False);
  XdndDrop          := XInternAtom(FDisplay, 'XdndDrop',          False);
  XdndFinished      := XInternAtom(FDisplay, 'XdndFinished',      False);

  // actions
  XdndActionCopy    := XInternAtom(FDisplay, 'XdndActionCopy',    False);
  XdndActionMove    := XInternAtom(FDisplay, 'XdndActionMove',    False);
  XdndActionLink    := XInternAtom(FDisplay, 'XdndActionLink',    False);
  XdndActionAsk     := XInternAtom(FDisplay, 'XdndActionAsk',     False);
  XdndActionPrivate := XInternAtom(FDisplay, 'XdndActionPrivate', False);
end;

procedure TfpgX11Application.ResetDNDVariables;
var
  msgp: TfpgMessageParams;
begin
  if FLastDropTarget <> 0 then
  begin
    fillchar(msgp, sizeof(msgp), 0);
    fpgPostMessage(nil, FindWindowByHandle(FLastDropTarget), FPGM_DROPEXIT, msgp);
  end;
  FDNDTypeList.Clear;
  FActionType     := 0;
  FSrcWinHandle   := 0;
  FLastDropTarget := 0;
  FDropPos.X := 0;
  FDropPos.Y := 0;
end;

procedure TfpgX11Application.HandleDNDenter(ATopLevelWindow: TfpgX11Window;
    const ASource: TWindow; const ev: TXEvent);
var
  actualtype: TAtom;
  actualformat: cint;
  count, remaining: culong;
  xdndtypes: PAtomArray;
  i: integer;
  s: TfpgString;
  itm: TDNDSrcType;
begin
  {$IFDEF DNDDEBUG}
  writeln('TfpgX11Application.HandleDNDenter');
  {$ENDIF}
  ResetDNDVariables;
  FSrcWinHandle := ASource;
  FDNDVersion := min(FPG_XDND_VERSION, (ev.xclient.data.l[1] and $FF000000) shr 24);
  {$IFDEF DNDDEBUG}
  writeln(Format('  ver(%d) check-XdndTypeList(%s) data=%xh,%d,%d,%d,%d',
      [ FDNDVersion,
        BoolToStr(fpgIsBitSet(ev.xclient.data.l[1], 0), True),
        ev.xclient.data.l[0],
        ev.xclient.data.l[1],
        ev.xclient.data.l[2],
        ev.xclient.data.l[3],
        ev.xclient.data.l[4]  ]));
  writeln(Format('  * We will be using XDND v%d protocol *', [FDNDVersion]));
  if fpgIsBitSet(ev.xclient.data.l[1], 0) then
    writeln('  ** We need to fetch XdndTypeList (>3 types)');
  {$ENDIF}

  // read typelist
  if fpgIsBitSet(ev.xclient.data.l[1], 0) then
  begin
    // now fetch the data
    XGetWindowProperty(Display, FSrcWinHandle,
        XdndTypeList, 0, 16000,
        TBool(False),
        AnyPropertyType,
        @actualtype, @actualformat, @count, @remaining,
        @xdndtypes);

    {$IFDEF DNDDEBUG}
    s := XGetAtomName(Display, actualtype);
    writeln('Actual fetch -----------------------');
    writeln(Format('  ActualType: %s (%d)', [s, ActualType]));
    writeln('  Actualformat = ', ActualFormat);
    writeln('  count = ', count);
    writeln('  remaining = ', remaining);
    {$ENDIF}

    if (actualtype <> XA_ATOM) or (actualformat <> 32) then
      count := 0;
  end
  else
  begin
    count := 3;
    xdndtypes := @ev.xclient.data.l[2];
  end;

  for i := 0 to count-1 do
  begin
    if xdndtypes^[i] <> 0 then
    begin
      s := XGetAtomName(FDisplay, xdndtypes^[i]);
      {$IFDEF DNDDEBUG}
      writeln(Format('  Format #%d = %s (%d)', [i+1, s, xdndtypes^[i]]));
      {$ENDIF}
      // store each supported data type for later use
      itm := TDNDSrcType.Create;
      itm.Name := s;
      itm.ID := xdndtypes^[i];
      FDNDTypeList.Add(itm);
    end;
  end;
  if count > 3 then
    XFree(xdndtypes);
end;

procedure TfpgX11Application.HandleDNDleave(ATopLevelWindow: TfpgX11Window;
    const ASource: TWindow);
var
  wg: TfpgWidget;
begin
  {$IFDEF DNDDEBUG}
  writeln('TfpgX11Application.HandleDNDleave');
  {$ENDIF}
  if FLastDropTarget <> 0 then { 0 would be first time in, so there is no last window }
  begin
    wg := FindWindowByHandle(FLastDropTarget) as TfpgWidget;
    if wg.AcceptDrops then
    begin
      if Assigned(wg.OnDragLeave) then
        wg.OnDragLeave(wg);
    end;
  end;
  ResetDNDVariables;
end;

procedure TfpgX11Application.HandleDNDposition(ATopLevelWindow: TfpgX11Window; const ASource: TWindow;
    const x_root: integer; const y_root: integer; const AAction: TAtom; const ATimestamp: x.TTime);
var
  Msg: TXEvent;
  dx, dy, dx2, dy2: cint;
  child: TWindow;
  prevchild: TWindow;

  ret_child: TfpgWinHandle;

  i: integer;
  lTargetWinHandle: TWindow;
  w: TfpgX11Window;
  wg: TfpgWidget;
  wg2: TfpgWidget;
  swg: TfpgWidget;
  msgp: TfpgMessageParams;
  lDropAction: TfpgDropAction;
  lAccept: Boolean;
  lMimeChoice: TfpgString;
  lMimeList: TStringList;
begin
  {$IFDEF DNDDEBUG}
  writeln('TfpgX11Application.HandleDNDposition  (toplevel window = ', ATopLevelWindow.Name, ')');
  {$ENDIF}
  lAccept := False;
  FSrcWinHandle := ASource;
  FActionType   := AAction;
  FSrcTimeStamp := ATimeStamp;
  lTargetWinHandle := ATopLevelWindow.WinHandle;
  lMimeChoice := 'text/plain';

  {$IFDEF DNDDEBUG}
  s := XGetAtomName(FDisplay, AAction);
  writeln(Format('  requested action: %s (%d)', [s, AAction]));
  writeln('  x_root: ', x_root, '  y_root: ', y_root);
  {$ENDIF}

  // Find window under cursor, and position of cursor inside that window
  XTranslateCoordinates(FDisplay, XDefaultRootWindow(FDisplay), ATopLevelWindow.WinHandle,
      x_root, y_root, @dx, @dy, @ret_child);
  child := ret_child;
  prevchild := ATopLevelWindow.WinHandle;
  while ret_child <> 0 do   // If we have chidren, iterate until we reach the top most child
  begin
    child :=  ret_child;
    dx2 := dx;
    dy2 := dy;
    XTranslateCoordinates(FDisplay, prevchild, child,
        dx2, dy2, @dx, @dy, @ret_child);
    prevchild := child;
  end;

  if child <> 0 then
    w := FindWindowByHandle(child)
  else
    w := ATopLevelWindow;

  {$IFDEF DNDDEBUG}
  writeln(Format('x:%d  y:%d  child:%d (%x)', [dx, dy, w.WinHandle, w.WinHandle]));
  {$ENDIF}

  if Assigned(w) then
  begin
    lTargetWinHandle := w.WinHandle;
    {$IFDEF DNDDEBUG}
    writeln('dragging over window: ', w.Name);
    {$ENDIF}
    if w is TfpgWidget then      // TODO: We could use Interfaces here eg: IDragDropEnabled
    begin
      wg := TfpgWidget(w);
      if FLastDropTarget <> lTargetWinHandle then
      begin
        if FLastDropTarget <> 0 then { 0 would be first time in, so there is no last window }
        begin
          wg2 := FindWindowByHandle(FLastDropTarget) as TfpgWidget;
          if wg2.AcceptDrops then
          begin
            if Assigned(wg2.OnDragLeave) then
              wg2.OnDragLeave(wg2);
          end;
          fillchar(msgp, sizeof(msgp), 0);
          { Notify the widget so it can reset its looks if needed }
          fpgPostMessage(nil, wg2, FPGM_DROPEXIT, msgp);
        end;
      end;
      FLastDropTarget := lTargetWinHandle;
      if wg.AcceptDrops then
      begin
        if Assigned(wg.OnDragEnter) then
        begin
          lDropAction := GetDropActionFromAtom(AAction);
          lMimeList := TStringList.Create;
          for i := 0 to FDNDTypeList.Count-1 do
            lMimeList.Add(TDNDSrcType(FDNDTypeList[i]).Name);

          { Use the first mime-type as the default option presented. The mime-list
            should always be from most specific to least specific. }
          if lMimeList.Count > 0 then
            lMimeChoice := lMimeList[0]
          else
            {$NOTE We need to replace this message with a resouce string }
            raise Exception.Create('fpGUI/X11: no mime types available for DND operation');

          { TODO: We need to populate the Source parameter. }
          if Assigned(Drag) then
            swg := TfpgDrag(Drag).Source as TfpgWidget
          else
            swg := nil;
          wg.OnDragEnter(wg, swg, lMimeList, lMimeChoice, lDropAction, lAccept);
          lMimeList.Free;
          FActionType := GetAtomFromDropAction(lDropAction);
        end;

        { Notify widget of drag status, so it can update its look }
        if lAccept then
        begin
          FDropPos.X := dx;
          FDropPos.Y := dy;
          fillchar(msgp, sizeof(msgp), 0);
          msgp.mouse.x := dx;
          msgp.mouse.y := dy;
          fpgPostMessage(nil, w, FPGM_DROPENTER, msgp);
        end;
      end;
    end;
  end;

  FDNDDataType := None;
  for i := 0 to FDNDTypeList.Count-1 do
  begin
    { This list must be from most specific to least specific }
    if TDNDSrcType(FDNDTypeList[i]).Name = lMimeChoice then
    begin
      FDNDDataType := TDNDSrcType(FDNDTypeList[i]).ID;
      break;
    end;
  end;

  // send message to confirm drop will be accepted in specified rectangle
  FillChar(Msg, SizeOf(Msg), 0);
  Msg.xany._type      := ClientMessage;
  Msg.xany.display    := FDisplay;
  Msg.xclient.window  := FSrcWinHandle; // source winhandle msg is going to
  Msg.xclient.message_type  := XdndStatus;
  Msg.xclient.format        := 32;

  Msg.xclient.data.l[0]   := ATopLevelWindow.WinHandle;  // always top-level window
  if lAccept then
  begin
    Msg.xclient.data.l[1] := 1;
    Msg.xclient.data.l[4] := FActionType;
  end
  else
  begin
    Msg.xclient.data.l[1] := 0;
    Msg.xclient.data.l[4] := None;
  end;
  Msg.xclient.data.l[2]   := 0;       // x & y co-ordinates
  Msg.xclient.data.l[3]   := 0;       // w & h co-ordinates

  XSendEvent(FDisplay, FSrcWinHandle, False, NoEventMask, @Msg);
end;

procedure TfpgX11Application.HandleDNDdrop(ATopLevelWindow: TfpgX11Window;
    const ASource: TWindow; const ATimestamp: x.TTime);
var
  Msg: TXEvent;
begin
  {$IFDEF DNDDEBUG}
  writeln('TfpgX11Application.HandleDNDdrop');
  {$ENDIF}
  { TODO: Must XConvertSelection always be called? }
  if FDNDDataType <> None then
    XConvertSelection(FDisplay, XdndSelection, FDNDDataType, XdndSelection, ATopLevelWindow.FWinHandle, ATimestamp);

  { send message to signal drop is finished }
  FillChar(Msg, SizeOf(Msg), 0);
  Msg.xany._type            := ClientMessage;
  Msg.xany.display          := FDisplay;
  Msg.xclient.window        := FSrcWinHandle; // source winhandle msg is going to
  Msg.xclient.message_type  := XdndFinished;
  Msg.xclient.format        := 32;

  Msg.xclient.data.l[0] := ATopLevelWindow.FWinHandle;  // winhandle of target window
  if FDNDDataType = None then
    Msg.xclient.data.l[1] := 0 // drop NOT accepted
  else
    Msg.xclient.data.l[1] := 1; // drop accepted - target can remove the data
  Msg.xclient.data.l[2] := FActionType; // this should be the action we accepted

  XSendEvent(FDisplay, FSrcWinHandle, False, NoEventMask, @Msg);
end;

procedure TfpgX11Application.HandleDNDSelection(const ev: TXEvent);
var
  actualtype: TAtom;
  actualformat: cint;
  count, remaining, dummy: culong;
  s: variant;
  data: PChar;
  wg: TfpgWidget;
  swg: TfpgWidget; { source widget that started drag - if drag and drop occur in the same application }
begin
  {$IFDEF DNDDEBUG}
  writeln('TfpgX11Application.HandleDNDselection');
  {$ENDIF}
  if FLastDropTarget <> 0 then { 0 would be first time in, so there is no last window }
  begin
    wg := FindWindowByHandle(FLastDropTarget) as TfpgWidget;
    if not wg.AcceptDrops then
      Exit;
  end;

  { do not get data yet, just see how much there is }
  XGetWindowProperty(FDisplay, ev.xselection.requestor,
      ev.xselection._property, 0, 0,
      TBool(false),
      AnyPropertyType,
      @actualtype, @actualformat, @count, @remaining,
      @data);

  { we handle the DND selection here }
    {$IFDEF DNDDEBUG}
    s := XGetAtomName(FDisplay, actualtype);
    writeln(Format('  ActualType: %s (%d)', [s, ActualType]));
    writeln('  Actualformat = ', ActualFormat);
    writeln('  count = ', count);
    writeln('  remaining = ', remaining);
    writeln('-----------------');
    {$ENDIF}

  if remaining > 0 then   { we have data - now fetch it! }
  begin
    XGetWindowProperty(FDisplay, ev.xselection.requestor,
        ev.xselection._property, 0, remaining,
        TBool(false),
        AnyPropertyType,
        @actualtype, @actualformat, @count, @dummy,
        @data);
    s := data;
  end;

  if FLastDropTarget <> 0 then { 0 would be first time in, so there is no last window }
  begin
    wg := FindWindowByHandle(FLastDropTarget) as TfpgWidget;
    if wg.AcceptDrops then
    begin
      if Assigned(wg.OnDragDrop) then
      begin
        if Assigned(uDragSource) then
          swg := uDragSource as TfpgWidget
        else
          swg := nil;
        wg.OnDragDrop(wg, swg, FDropPos.X, FDropPos.Y, s);
        uDragSource := nil; { reset variable for the next DND action }
      end;
    end;
  end;
  {$IFDEF DNDDEBUG}
  writeln(' s = ', s);
  {$ENDIF}
  ResetDNDVariables;
end;

function TfpgX11Application.DoGetFontFaceList: TStringList;
var
  pfs: PFcFontSet;
  ppat: PPFcPattern;
  n: integer;
  s: string;
  pc: PChar;
begin
  // this now even returns non-scaleable fonts which is what we sometimes want.
  pfs := XftListFonts(Display, DefaultScreen, [0, FC_FAMILY, 0]);

  if pfs = nil then
    Exit; //==>

  Result := TStringList.Create;

  GetMem(pc, 128);
  n := 0;
  ppat := pfs^.fonts;
  while n < pfs^.nfont do
  begin
    XftNameUnparse(ppat^, pc, 127);  //XftNameUnparse does not free the name string!
    s := pc;
    Result.Add(s);
    inc(PChar(ppat), sizeof(pointer));
    inc(n);
  end;
  FreeMem(pc);
  FcFontSetDestroy(pfs);

  Result.Sort;
end;

constructor TfpgX11Application.Create(const AParams: string);
var
  s: string;
begin
  inherited Create(AParams);
  FIsInitialized    := False;

  if gCommandLineParams.IsParam('display') then
  begin
    s := gCommandLineParams.GetParam('display');
    FDisplay := XOpenDisplay(PChar(s));
  end
  else
    FDisplay := XOpenDisplay('');

  if FDisplay = nil then
    raise Exception.Create('fpGUI-X11: Could not open the display. Is your X11 server running?');

  Terminated := False;
  DefaultScreen     := XDefaultScreen(Display);
  FRootWindow       := XRootWindow(FDisplay, DefaultScreen);
  DefaultBackground := XBlackPixel(FDisplay, DefaultScreen);
  DefaultForeground := XWhitePixel(FDisplay, DefaultScreen);

  DefaultVisual := XDefaultVisual(FDisplay, DefaultScreen);
  DisplayDepth  := XDefaultDepth(FDisplay, DefaultScreen);

  //Writeln('display depth: ',DisplayDepth);
  DefaultColorMap := XDefaultColorMap(FDisplay, DefaultScreen);

  // Initialize atoms
  xia_clipboard         := XInternAtom(FDisplay, 'CLIPBOARD', TBool(False));
  xia_targets           := XInternAtom(FDisplay, 'TARGETS', TBool(False));
  xia_motif_wm_hints    := XInternAtom(FDisplay, '_MOTIF_WM_HINTS', TBool(False));
  xia_wm_protocols      := XInternAtom(FDisplay, 'WM_PROTOCOLS', TBool(False));
  xia_wm_delete_window  := XInternAtom(FDisplay, 'WM_DELETE_WINDOW', TBool(False));
  xia_wm_state          := XInternAtom(FDisplay, 'WM_STATE', TBool(False));

  { initializa the XDND atoms }
  FDNDTypeList := TObjectList.Create;
  XdndInit;

  netlayer := TNETWindowLayer.Create(FDisplay);

  // for correct keyboard handling
  InputMethod := XOpenIM(FDisplay, nil, nil, nil);
  if InputMethod = nil then
    Exit;

  InputContext := XCreateIC(InputMethod, [XNInputStyle, XIMPreeditNothing or XIMStatusNothing, nil]);
  if InputContext = nil then
    Exit;
  FIsInitialized := True;
  xapplication := TfpgApplication(self);
end;

destructor TfpgX11Application.Destroy;
begin
  netlayer.Free;
  XCloseDisplay(FDisplay);
  FDNDTypeList.Free;
  inherited Destroy;
end;

function TfpgX11Application.MessagesPending: boolean;
begin
  Result := (XPending(display) > 0);
  fpgCheckTimers;
end;

function GetParentWindow(wh: TfpgWinHandle; var pw, rw: TfpgWinHandle): boolean;
var
  rootw: TfpgWinHandle;
  parentw: TfpgWinHandle;
  childs: ^TfpgWinHandle;
  cnum: cuint;
begin
  childs := nil;
  if XQueryTree(xapplication.display, wh, @rootw, @parentw, @childs, @cnum) <> 0 then
  begin
    pw     := parentw;
    rw     := rootw;
    Result := True;
  end
  else
    Result := False;
  if childs <> nil then
    XFree(childs);
end;

function GetDecorationWindow(wh: TfpgWinHandle): TfpgWinHandle;
var
  lpw: TfpgWinHandle;
  pw: TfpgWinHandle;
  rw: TfpgWinHandle;
  bok: boolean;
begin
  pw := wh;
  repeat
    lpw := pw;
    bok := GetParentWindow(lpw, pw, rw);
  until (not bok) or (pw = rw);
  if bok then
    Result := lpw
  else
    Result := 0;
end;

function X11keycodeToScanCode(akeycode: word): word;
begin
  case akeycode and $ff of
    $09..$5B: Result := akeycode - 8;
    $6C: Result := $11C; // numpad enter
    $6D: Result := $11D; // right ctrl
    $70: Result := $135; // numpad /
    $62: Result := $148; // up arrow
    $64: Result := $14B;
    $66: Result := $14D;
    $68: Result := $150; // down arrow
    $6A: Result := $152;
    $61: Result := $147;
    $63: Result := $149;
    $6B: Result := $153;
    $67: Result := $14F;
    $69: Result := $151;
    $71: Result := $138;
    else
      Result := akeycode;
  end;
end;

procedure TfpgX11Application.DoWaitWindowMessage(atimeoutms: integer);
var
  ev: TXEvent;
  NewEvent: TXevent;
  i: integer;
  r: integer;
  blockmsg: boolean;
  w: TfpgX11Window;
  ew: TfpgX11Window;
  kwg: TfpgWidget;
  wh: TfpgWinHandle;
  wa: TXWindowAttributes;
  mcode: integer;
  msgp: TfpgMessageParams;
  rfds: baseunix.TFDSet;
  xfd: integer;
  KeySym: TKeySym;
  Popup: TfpgWidget;
  needToWait: boolean;

  // debug purposes only
  procedure PrintKeyEvent(const event: TXEvent);
  var
    keysym: TKeySym;
    icstatus: TStatus;
    l: integer;
    s: string;
  begin
    case event._type of
      X.KeyPress:
          begin
            write('*** KeyPress ');
          end;
      X.KeyRelease:
          begin
            write('*** KeyRelease ');
          end;
    else
        begin
          writeln('not a key event ');
        end;
    end;
    SetLength(s, 20);
    l := Xutf8LookupString(InputContext, @event.xkey, @s[1], Length(s), @keysym, @icstatus);
    SetLength(s, l);
    if icstatus = XBufferOverflow then
      Xutf8LookupString(InputContext, @event.xkey, @s[1], Length(s), @keysym, @icstatus);
    writeln('result of xlookupstring [' + s + ']');
    writeln(Format('*** keysym [%s] ', [XKeysymToString(keysym)]));
  end;

  // Debug info only
  procedure ReportLostWindow(const event: TXEvent);
  begin
    {$IFDEF DEBUG}
    writeln('fpGUI/X11: ', GetXEventName(event._type), ' can''t find <',
        IntToHex(event.xany.window, 9), '>');
    {$ENDIF}
  end;

begin
  xfd := XConnectionNumber(display);
  DoFlush;
(*
  repeat

    if (atimeoutms >= 0) and (XPending(display) < 1) then   { there are no X messages }
    begin
      if Assigned(FOnIdle) then
        OnIdle(self);
      // Some event is waiting for the given timeout.
      // This Select handles only the first 256 file descriptors.
      // Poll would be better but FPC has no official poll interface (if I'm right)
      fpFD_ZERO(rfds);
      fpFD_SET(xfd, rfds);
      r := fpSelect(xfd + 1, @rfds, nil, nil, {atimeoutms} 50);

      if r < 1 then
        Exit; // no event received.
    end;

   XNextEvent(display, @ev);
  until (not XFilterEvent(@ev, X.None));
*)

  needToWait := True;
  if XPending(display) > 0 then // We have a X message to process
  begin
    XNextEvent(display, @ev);
    needToWait := False;
  end;

  if needToWait then  // No X messages to process (we are idle). So do a timeout wait
  begin
    if Assigned(FOnIdle) then
      OnIdle(self);
    fpFD_ZERO(rfds);
    fpFD_SET(xfd, rfds);
    r := fpSelect(xfd + 1, @rfds, nil, nil, {atimeoutms} 50);
    if r <> 0 then  // We got a X event or the timeout happened
      XNextEvent(display, @ev)
    else
      Exit; // nothing further to do here!
  end;


  // if the event filter returns true then it ate the message
  if Assigned(FEventFilter) and FEventFilter(ev) then
    exit; // no more processing required for that event


  blockmsg := False;
  fillchar(msgp, sizeof(msgp), 0);


  // According to a comment in X.h, the valid event types start with 2!
  if ev._type < 2 then
    exit;


  Popup := PopupListFirst;


  {$IFDEF DEBUG}
  w := FindWindowByHandle(ev.xany.window);
  if not Assigned(w) then
    WriteLn('Event ',GetXEventName(ev._type),'(', ev._type,') window: ', IntToHex(ev.xany.window,7))
  else
    WriteLn('Event ',GetXEventName(ev._type),'(', ev._type,') window: ', IntToHex(ev.xany.window,7), ' name:', w.Name);
//  PrintKeyEvent(ev);  { debug purposes only }
  {$ENDIF}

  case ev._type of
    X.KeyPress,
    X.KeyRelease:
        begin
          if ev._type = X.KeyPress then
          begin
            KeySym := StartComposing(ev);
            { TODO -oGG : Move some code into the case statement }
            case FComposeStatus of
              XLookupNone:
                  begin
//                    writeln('KeyPress - XLookupNone');
                    Exit;
                  end;
              XLookupChars:
                  begin
//                    writeln('KeyPress - XLookupChars');
                  // do nothing
                  end;
              XLookupKeySymVal:
                  begin
//                    writeln('KeyPress - XLookupKeySymVal');
                  end;
              XLookupBoth:
                  begin
//                    writeln('KeyPress - XLookupBoth');
                  end;
            end;
            FLastKeySym := KeySym   // save it for KeyRelease event
          end
          else
            KeySym := FLastKeySym;  // restore saved KeySym

          msgp.keyboard.keycode     := KeySymToKeycode(KeySym);
          msgp.keyboard.shiftstate  := ConvertShiftState(ev.xkey.state);

          // By default X11 sends keyboard event to window under mouse cursor.
          // We need to get the corrected "focused" widget instead.
          kwg := FindKeyboardFocus;
          if kwg <> nil then
            w := kwg
          else
          begin
            {$IFDEF DEBUG}
            writeln('ERR: We couldn''t find keyboard focused window. Using event window instead!');
            {$ENDIF}
            w := FindWindowByHandle(ev.xkey.window);
            if not Assigned(w) then
              ReportLostWindow(ev);
          end;

          if ev._type = X.KeyPress then
          begin
            fpgPostMessage(nil, w, FPGM_KEYPRESS, msgp);

            if (ev.xkey.state and (ControlMask or Mod1Mask)) = 0 then
            begin
              for i := 1 to UTF8Length(FComposeBuffer) do
              begin
                msgp.keyboard.keychar := UTF8Copy(FComposeBuffer, i, 1);
                fpgPostMessage(nil, w, FPGM_KEYCHAR, msgp);
              end;
            end;
          end { if }
          else if ev._type = X.KeyRelease then
            fpgPostMessage(nil, w, FPGM_KEYRELEASE, msgp);
        end;

    X.ButtonPress,
    X.ButtonRelease:
        begin
          msgp.mouse.x          := ev.xbutton.x;
          msgp.mouse.y          := ev.xbutton.y;
          msgp.mouse.Buttons    := ev.xbutton.button;
          msgp.mouse.shiftstate := ConvertShiftState(ev.xbutton.state);


          w := FindWindowByHandle(ev.xbutton.window);
          if not Assigned(w) then
            ReportLostWindow(ev);

          { This closes popup windows when you click the mouse elsewhere }
          if ev._type = X.ButtonPress then
          begin
            if (Popup <> nil) then
            begin
              ew := w;
              while (w <> nil) and (w.Parent <> nil) do
                w := TfpgX11Window(w.Parent);

              if (w <> nil) and (PopupListFind(w.WinHandle) = nil) and
                 (not PopupDontCloseWidget(TfpgWidget(ew))) then
              begin
                ClosePopups;
              end;
            end;
          end
          else
          begin
            if Assigned(Drag) then    // button released
            begin
              Drag.SendDNDDrop;
              { TODO: Start timeout in case XdndFinished is not received, so we can free Drag }
//              writeln('Freeing Drag Object');
              if not Drag.FDropAccepted then
              begin
                FreeAndNil(FDrag);
              end;
            end;
          end;

          w := FindWindowByHandle(ev.xbutton.window); // restore w
          if xapplication.TopModalForm <> nil then
          begin
            ew := TfpgX11Window(WidgetParentForm(TfpgWidget(w)));
            if (ew <> nil) and (xapplication.TopModalForm <> ew) and (waUnblockableMessages in ew.WindowAttributes = False) then
              blockmsg := true;
          end;

          // Is message blocked by a modal form?
          if not blockmsg then
          begin
            if (ev.xbutton.button >= 4) and (ev.xbutton.button <= 7) then  // mouse wheel
            begin
              // generate scroll events:
              if ev._type = X.ButtonPress then
              begin
                if ev.xbutton.button = Button4 then
                  i := -1
                else
                  i := 1;

        	      // Check for other mouse wheel messages in the queue
                while XCheckTypedWindowEvent(display, ev.xbutton.window, X.ButtonPress, @NewEvent) do
                begin
      	          if NewEvent.xbutton.Button = 4 then
      	            Dec(i)
                  else if NewEvent.xbutton.Button = 5 then
      	            Inc(i)
                  else
            	    begin
            	      XPutBackEvent(display, @NewEvent);
                    break;
            	    end;
                end;

                msgp.mouse.delta := i;
                fpgPostMessage(nil, w, FPGM_SCROLL, msgp);
              end;
            end
            else
            begin
              if ev._type = X.ButtonRelease then
              begin
                {$IFDEF DEBUG}
                writeln('****  PostMessage MouseUp ', w.ClassName, ' - ', w.Name);
                {$ENDIF}
                mcode := FPGM_MOUSEUP;
              end
              else
              begin
                {$IFDEF DEBUG}
                writeln('**** PostMessage MouseDown ', w.ClassName, ' - ', w.Name);
                {$ENDIF}
                mcode := FPGM_MOUSEDOWN;
              end;
              fpgPostMessage(nil, w, mcode, msgp);
            end;  { if/else }
          end;  { if not blocking }
        end;

    X.Expose:
        begin
          repeat
            //
          until not XCheckTypedWindowEvent(display, ev.xexpose.window, X.Expose, @ev);
          if ev.xexpose.count = 0 then
          begin
            fpgPostMessage(nil, FindWindowByHandle(ev.xexpose.window), FPGM_PAINT);
          end;
        end;

    X.GraphicsExpose:
        begin
          repeat
            //
          until not XCheckTypedWindowEvent(display, ev.xexpose.window, X.GraphicsExpose, @ev);
          if ev.xgraphicsexpose.count = 0 then
          begin
            fpgPostMessage(nil, FindWindowByHandle(ev.xgraphicsexpose.drawable), FPGM_PAINT);
          end;
        end;

    X.MotionNotify:
        begin
          repeat
            //
          until not XCheckTypedWindowEvent(display, ev.xmotion.window, X.MotionNotify, @ev);
          w := FindWindowByHandle(ev.xmotion.window);
          if not Assigned(w) then
            ReportLostWindow(ev)
          else
          begin
            if Assigned(Drag) then
            begin
              if ((ev.xmotion.state and $FF00) shr 8) = MOUSE_LEFT then
                Drag.Dragging(ev);
            end;
            if xapplication.TopModalForm <> nil then
            begin
              ew := TfpgX11Window(WidgetParentForm(TfpgWidget(w)));
              if (ew <> nil) and (xapplication.TopModalForm <> ew) and (waUnblockableMessages in ew.WindowAttributes = False) then
                blockmsg := true;
            end;
            if not blockmsg then
            begin
              msgp.mouse.x          := ev.xmotion.x;
              msgp.mouse.y          := ev.xmotion.y;
              msgp.mouse.Buttons    := (ev.xmotion.state and $FF00) shr 8;
              msgp.mouse.shiftstate := ConvertShiftState(ev.xmotion.state);
              fpgPostMessage(nil, w, FPGM_MOUSEMOVE, msgp);
            end;
          end;
        end;

    { one use is for message blockings for modal windows, or XDND etc. }
    X.ClientMessage:
        begin
          w := FindWindowByHandle(ev.xclient.window);
          if not Assigned(w) then
            ReportLostWindow(ev);

          // WM_PROTOCOLS message
          if Assigned(w) and (ev.xclient.message_type = xia_wm_protocols) then
          begin
            if (ev.xclient.data.l[0] = netlayer.NetAtom[naWM_PING]) then
            begin
              // always respond to pings or the wm will kill us
              netlayer.WindowReplyToPING(w.FWinHandle, @ev.xclient);
            end
            else if ev.xclient.data.l[0] = xia_wm_delete_window then
            begin
              if xapplication.TopModalForm <> nil then
              begin
                // This is ugly!!!!!!!!!!!!!!!
                ew := TfpgX11Window(WidgetParentForm(TfpgWidget(w)));
                if (ew <> nil) and (TopModalForm <> ew) and (waUnblockableMessages in ew.WindowAttributes = False) then
                  blockmsg := true;
              end;

              if not blockmsg then
                fpgPostMessage(nil, FindWindowByHandle(ev.xclient.window), FPGM_CLOSE);
             end;
          end
          { XDND protocol - XdndEnter }
          else if Assigned(w) and (ev.xclient.message_type = XdndEnter) then
          begin
            {$IFDEF DNDDEBUG}
            writeln('ClientMessage.XdndEnter event received');
            {$ENDIF}
            HandleDNDenter(w, ev.xclient.data.l[0], ev);
          end
          { XDND protocol - XdndPosition }
          else if Assigned(w) and (ev.xclient.message_type = XdndPosition) then
          begin
            {$IFDEF DNDDEBUG}
            writeln('ClientMessage.XdndPosition event received');
            {$ENDIF}
            HandleDNDposition(w,                            // top level window
                ev.xclient.data.l[0],                       // Source window
                (ev.xclient.data.l[2] and $FFFF0000) shr 16, // x_root
                ev.xclient.data.l[2] and $0000FFFF,          // y_root
                ev.xclient.data.l[4],                       // action
                ev.xclient.data.l[3]);                      // timestamp
          end
          { XDND protocol - XdndStatus }
          else if Assigned(w) and (ev.xclient.message_type = XdndStatus) then
          begin
            {$IFDEF DNDDEBUG}
            writeln('ClientMessage.XdndStatus event received');
            {$ENDIF}
            if Assigned(Drag) then
            begin
              Drag.HandleDNDStatus(
                  ev.xclient.data.l[0],
                  ev.xclient.data.l[1] and 1,
                  fpgRect(
                    (ev.xclient.data.l[2] shr 16) and $FFFF,
                    ev.xclient.data.l[2] and $FFFF,
                    (ev.xclient.data.l[3] shr 16) and $FFFF,
                    ev.xclient.data.l[3] and $FFFF),
                  ev.xclient.data.l[4]);
            end;
          end
          { XDND protocol - XdndLeave }
          else if Assigned(w) and (ev.xclient.message_type = XdndLeave) then
          begin
            {$IFDEF DNDDEBUG}
            writeln('ClientMessage.XdndLeave event received');
            {$ENDIF}
            HandleDNDleave(w, ev.xclient.data.l[0]);
          end
          { XDND protocol - XdndDrop }
          else if Assigned(w) and (ev.xclient.message_type = XdndDrop) then
          begin
            {$IFDEF DNDDEBUG}
            writeln('ClientMessage.XdndDrop event received');
            writeln('    ClassName = ', w.ClassName);
            writeln('    Name = ', w.Name);
            {$ENDIF}
            HandleDNDdrop(w, ev.xclient.data.l[0], ev.xclient.data.l[2]);
          end
          { XDND protocol - XdndFinished }
          else if Assigned(w) and (ev.xclient.message_type = XdndFinished) then
          begin
            {$IFDEF DNDDEBUG}
            writeln('ClientMessage.XdndFinished event received');
            {$ENDIF}
            if Assigned(Drag) then
            begin
              {$IFDEF DNDDEBUG}
              writeln('Freeing Drag Object');
              {$ENDIF}
              FreeAndNil(FDrag);
            end;
          end;
        end;

    X.ConfigureNotify:
        begin
          repeat
            //
          until not XCheckTypedWindowEvent(display, ev.xconfigure.window, ConfigureNotify, @ev);

          msgp.rect.Left   := ev.xconfigure.x;
          msgp.rect.Top    := ev.xconfigure.y;
          msgp.rect.Width  := ev.xconfigure.Width;
          msgp.rect.Height := ev.xconfigure.Height;

          w := FindWindowByBackupHandle(ev.xconfigure.window);
          if not Assigned(w) then
            ReportLostWindow(ev);

          if w <> nil then
          begin
            if w.FWindowType <> wtChild then
            begin
              wh := GetDecorationWindow(ev.xconfigure.window);
              if wh > 0 then
              begin
                XGetWindowAttributes(display, wh, @wa);
                msgp.rect.Left := wa.x;
                msgp.rect.Top  := wa.y;
              end;
            end;

            if (w.FWidth <> msgp.rect.Width) or (w.FHeight <> msgp.rect.Height) then
              fpgPostMessage(nil, w, FPGM_RESIZE, msgp);

            if (w.FLeft <> msgp.rect.Left) or (w.FTop <> msgp.rect.Top) then
              fpgPostMessage(nil, w, FPGM_MOVE, msgp);
          end;
        end;

    X.SelectionNotify:
        begin
          { Handle XDND data }
          if ev.xselection._property = XdndSelection then
          begin
            {$IFDEF DNDDEBUG}
            writeln('XdndSelection message received');
            {$ENDIF}
            HandleDNDSelection(ev);
          end
          else  { Handle X Selections - clipboard data }
            ProcessSelection(ev);
        end;

    X.SelectionRequest:
        begin
          if ev.xselectionrequest.selection = XdndSelection then
          begin
            {$IFDEF DNDDEBUG}
            writeln('found a XdndSelection request');
            {$ENDIF}
            if Assigned(Drag) then
              Drag.HandleSelectionRequest(ev);
          end
          else
          begin
            {$IFDEF DEBUG}
            writeln('found a clipboard selection request');
            {$ENDIF}
            ProcessSelectionRequest(ev);
          end;
        end;

    X.SelectionClear:
        begin
          { TODO : Not sure if I am handling this correctly? }
          if ev.xselectionclear.selection = xia_clipboard then
          begin
            fpgClipboard.FClipboardText := '';
            Exit;
          end;
        end;

    X.FocusIn:
        fpgPostMessage(nil, FindWindowByHandle(ev.xfocus.window), FPGM_ACTIVATE);

    X.FocusOut:
        fpgPostMessage(nil, FindWindowByHandle(ev.xfocus.window), FPGM_DEACTIVATE);

    X.EnterNotify:
        fpgPostMessage(nil, FindWindowByHandle(ev.xcrossing.window), FPGM_MOUSEENTER);

    X.LeaveNotify:
        fpgPostMessage(nil, FindWindowByHandle(ev.xcrossing.window), FPGM_MOUSEEXIT);

    X.MapNotify:
        begin
          w := FindWindowByHandle(ev.xmap.window);
          if w <> nil then
            Include(w.FWinFlags, xwsfMapped);

          { X11 is too efficient, so new windows don't need a OnResize when mapped,
            but because Windows GDI does so, we want the same events under X11.
            Lets fake one. }
          //if w <> nil then
          //begin
          //  msgp.rect.Left   := w.Left;
          //  msgp.rect.Top    := w.Top;
          //  msgp.rect.Width  := w.Width;
          //  msgp.rect.Height := w.Height;
          //  fpgPostMessage(nil, w, FPGM_RESIZE, msgp);
          //end;
        end;

    X.UnmapNotify:
        begin
          // special case which uses a different find window method
          w := FindWindowByBackupHandle(ev.xunmap.window);
          if not Assigned(w) then
            ReportLostWindow(ev)
          else
            Exclude(w.FWinFlags, xwsfMapped);
        end;

    X.DestroyNotify:
        begin
          // special case which uses a different find window method
          w := FindWindowByBackupHandle(ev.xdestroywindow.window);
          if not Assigned(w) then
            ReportLostWindow(ev)
          else
            RemoveWindowLookup(TfpgX11Window(w));
        end;

    X.NoExpose:
        begin
          // writeln('got a GraphicsExpose or NoExpose event');
          { If this application calls XCopyArea or XCopyPlane
            and the graphics_exposures member of the GC is
            True and the source is a window, these events may
            be generated; handle GraphicsExpose like Expose }
        end;

    X.ReparentNotify:
        begin
          // We are not interrested in this event yet
        end;

    else
      WriteLn('fpGUI/X11: Unhandled X11 event received: ', GetXEventName(ev._type));
  end;
end;

procedure TfpgX11Application.DoFlush;
begin
  XFlush(FDisplay);
end;

function TfpgX11Application.GetScreenWidth: TfpgCoord;
var
  wa: TXWindowAttributes;
begin
  XGetWindowAttributes(FDisplay, FRootWindow, @wa);
  Result := wa.Width;
end;

function TfpgX11Application.GetScreenHeight: TfpgCoord;
var
  wa: TXWindowAttributes;
begin
  XGetWindowAttributes(FDisplay, FRootWindow, @wa);
  Result := wa.Height;
end;

function TfpgX11Application.Screen_dpi_x: integer;
var
  mm: integer;
begin
  // 25.4 is millimeters per inch
  mm := 0;
  mm := DisplayWidthMM(Display, DefaultScreen);
  if mm > 0 then
    Result := Round((GetScreenWidth * 25.4) / mm)
  else
    Result := 96; // seems to be a well known default. :-(
end;

function TfpgX11Application.Screen_dpi_y: integer;
var
  mm: integer;
begin
  // 25.4 is millimeters per inch
  mm := 0;
  mm := DisplayHeightMM(Display, DefaultScreen);
  if mm > 0 then
    Result := Round((GetScreenHeight * 25.4) / mm)
  else
    Result := Screen_dpi_x; // same as width
end;

function TfpgX11Application.Screen_dpi: integer;
begin
  Result := Screen_dpi_y;
  {$IFDEF DEBUG}
  writeln('Display width in mm: ', DisplayWidthMM(Display, DefaultScreen));
  writeln('Display height in mm: ', DisplayHeightMM(Display, DefaultScreen));
  writeln('Display dpi: ', Result);
  {$ENDIF}
end;

{ TfpgX11Window }

procedure TfpgX11Window.DoAllocateWindowHandle(AParent: TfpgWindowBase);
var
  pwh: TfpgWinHandle;
  wh: TfpgWinHandle;
  lmwh: TfpgWinHandle;
  attr: TXSetWindowAttributes;
  mask: longword;
  hints: TXSizeHints;
  IconPixmap: TPixmap;
  WMHints: PXWMHints;
  prop: TAtom;
  mwmhints: TMWMHints;
begin
  if HandleIsValid then
    Exit; //==>

  if AParent <> nil then
    pwh := TfpgX11Window(AParent).WinHandle
  else
    pwh := xapplication.RootWindow;

  FillChar(attr, sizeof(attr), 0);
  mask := 0;
  if (FWindowType in [wtPopup]) or (waX11SkipWMHints in FWindowAttributes) then
  begin
    attr.Override_Redirect := 1;
    mask := CWOverrideRedirect;
  end;

  AdjustWindowStyle;

  if (not (waX11SkipWMHints in FWindowAttributes)) and (FWindowType = wtWindow) then
  begin
    if xapplication.FLeaderWindow = 0 then
    begin
      xapplication.FLeaderWindow := XCreateSimpleWindow(xapplication.Display,
          XDefaultRootWindow(xapplication.Display), 0, 0, 1, 1, 0, 0, 0);
      SetWindowGroup(xapplication.FLeaderWindow);
      xapplication.FClientLeaderAtom := XInternAtom(xapplication.Display, 'WM_CLIENT_LEADER', TBool(False));
    end;
  end;

  wh := XCreateWindow(xapplication.Display, pwh,
    FLeft, FTop, FWidth, FHeight, 0,
    CopyFromParent,
    InputOutput,
    xapplication.DefaultVisual,
    mask, @attr);

  if wh = 0 then
    raise Exception.Create('fpGUI/X11: Failed to create window ' + ClassName);

  FWinHandle := wh;
  FBackupWinHandle := wh;

  if AParent = nil then // is a toplevel window
  begin
    { setup a window icon }
    IconPixMap := XCreateBitmapFromData(fpgApplication.Display, FWinHandle,
      @IconBitmapBits, IconBitmapWidth, IconBitmapHeight);

    WMHints := XAllocWMHints;
    WMHints^.icon_pixmap := IconPixmap;
    WMHints^.flags := IconPixmapHint;

    { setup window grouping posibilities }
    if (not (waX11SkipWMHints in FWindowAttributes)) and (FWindowType = wtWindow) then
    begin
      WMHints^.flags := WMHints^.flags or WindowGroupHint;
      WMHints^.window_group := xapplication.FLeaderWindow;
    end;


    XSetWMProperties(fpgApplication.Display, FWinHandle, nil, nil, nil, 0, nil, WMHints, nil);

    if (not (waX11SkipWMHints in FWindowAttributes)) and (FWindowType = wtWindow) then
    begin
      { set class group hint per top-level window }
      SetWindowGroup(FWinHandle);
      XChangeProperty(xapplication.display, FWinHandle, xapplication.FClientLeaderAtom, 33, 32,
        PropModeReplace, @xapplication.FLeaderWindow, 1);
    end;

    { so newish window manager can close unresponsive programs }
    fpgApplication.netlayer.WindowSetPID(FWinHandle, GetProcessID);
    fpgApplication.netlayer.WindowSetSupportPING(FWinHandle);

    XFree(WMHints);

    { we need to set the XdndAware property }
    if QueueEnabledDrops then
    begin
      DoDNDEnabled(True);
    end;
  end;

  FillChar(hints, sizeof(hints), 0);
  hints.flags := 0;

  if not (waAutoPos in FWindowAttributes) then
    hints.flags := hints.flags or PPosition;

  if waScreenCenterPos in FWindowAttributes then
  begin
    hints.flags := hints.flags or PPosition;
    FLeft := (xapplication.ScreenWidth - FWidth) div 2;
    FTop  := (xapplication.ScreenHeight - FHeight) div 2;
    DoMoveWindow(FLeft, FTop);
  end
  else if waOneThirdDownPos in FWindowAttributes then
  begin
    hints.flags := hints.flags or PPosition;
    FLeft := (xapplication.ScreenWidth - FWidth) div 2;
    FTop  := (xapplication.ScreenHeight - FHeight) div 3;
    DoMoveWindow(FLeft, FTop);
  end;

  if (FWindowType <> wtChild) and (waSizeable in FWindowAttributes) then
  begin
    hints.flags      := hints.flags or PMinSize;
    hints.min_width  := FMinWidth;
    hints.min_height := FMinHeight;
  end
  else
  begin
    hints.flags      := hints.flags or PMinSize or PMaxSize;
    hints.min_width  := FWidth;
    hints.min_height := FHeight;
    hints.max_width  := FWidth;
    hints.max_height := FHeight;
  end;

  XSetWMNormalHints(xapplication.display, FWinHandle, @hints);

  if FWindowType <> wtChild then
    // send close event instead of quiting the whole application...
    fpgApplication.netlayer.WindowAddProtocol(FWinHandle, xapplication.xia_wm_delete_window);

  // for modal windows, this is necessary
  if FWindowType = wtModalForm then
  begin
    if Parent = nil then
    begin
      lmwh := 0;
      if fpgApplication.PrevModalForm <> nil then
        lmwh := TfpgX11Window(fpgApplication.PrevModalForm).WinHandle
{ 2011-03-24: Graeme Geldenhuys
  I commented code this code because it caused more problems that it solved
  when multiple modal dialogs or prompts are shown in succession.
  This code was originally introduced in commit 2ffdd747. I'm looking for an
  alternative solution to the original problem. }
//      else if FocusRootWidget <> nil then
//        lmwh := TfpgX11Window(FocusRootWidget).WinHandle
      else if fpgApplication.MainForm <> nil then
        lmwh := TfpgX11Window(fpgApplication.MainForm).WinHandle;
      if lmwh <> 0 then
      begin
        XSetTransientForHint(xapplication.display, FWinHandle, lmwh);
        fpgApplication.netlayer.WindowSetModal(FWinHandle, True);
      end;
    end;
  end;

  if (FWindowType = wtPopup) and (waStayOnTop in FWindowAttributes) then
    // we have a Splash screen
    fpgApplication.netlayer.WindowSetType(FWinHandle, [nwtSplash]);

  // process Borderless forms
  if ((FWindowType = wtWindow) or (FWindowType = wtModalForm)) and (waBorderless in FWindowAttributes) and not (waX11SkipWMHints in FWindowAttributes) then
  begin
    prop := X.None;
    prop := XInternAtom(xapplication.display, '_MOTIF_WM_INFO', TBool(False));
    if prop = X.None then
    begin
//      writeln('Window Manager does not support MWM hints.  Bypassing window manager control for borderless window.');
      // Set Override Redirect here!
      mwmhints.flags := 0;
    end
    else
    begin
      mwmhints.flags := MWM_HINTS_DECORATIONS;
      mwmhints.decorations := 0;

      if xapplication.xia_motif_wm_hints <> X.None then
      begin

        prop := xapplication.xia_motif_wm_hints;
        XChangeProperty(xapplication.display, FWinHandle, prop, prop, 32, PropModeReplace, @mwmhints, PROP_MWM_HINTS_ELEMENTS);
      end;
    end;
  end;

  { TODO : We could optimise this for non-focusable widgets }
  XSelectInput(xapplication.Display, wh, KeyPressMask or KeyReleaseMask or
      ButtonPressMask or ButtonReleaseMask or
      EnterWindowMask or LeaveWindowMask or
      ButtonMotionMask or PointerMotionMask or
      ExposureMask or FocusChangeMask or
      StructureNotifyMask);

  SetWindowParameters;

  AddWindowLookup(self);
end;

procedure TfpgX11Window.DoReleaseWindowHandle;
//var
//  lCallTrace: IInterface;
begin
//  lCallTrace := PrintCallTrace(Classname, 'DoReleaseWindowHandle: ' + Name);
  if HandleIsValid then
  begin
//    PrintCallTraceDbgLn('XDestroyWindow');
    XDestroyWindow(xapplication.Display, FWinHandle);
  end
  else
  begin
//    PrintCallTraceDbgLn(' RemoveWindowLookup');
    RemoveWindowLookup(self);
  end;

  FWinHandle := 0;
end;

procedure TfpgX11Window.DoRemoveWindowLookup;
begin
//  PrintCallTraceDbgLn('RemoveWindowLookup ' + Name + ' [' + Classname + ']');
  RemoveWindowLookup(self);
end;

procedure TfpgX11Window.DoSetWindowVisible(const AValue: Boolean);
begin
  if AValue then
  begin
    if not HandleIsValid then
      AllocateWindowHandle;
    XMapWindow(xapplication.Display, FWinHandle);
    Include(FWinFlags, xwsfMapped);
    // Fullscreen can only be set on visible (already mapped) windows.
    if waFullScreen in FWindowAttributes then
      fpgApplication.netlayer.WindowSetFullscreen(FWinHandle, True);
  end
  else
  begin
    if HandleIsValid and (xwsfMapped in FWinFlags) then
      XUnmapWindow(xapplication.Display, FWinHandle);
  end;
end;

function TfpgX11Window.HandleIsValid: boolean;
begin
  Result := (FWinHandle > 0);
end;

procedure TfpgX11Window.DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord);
begin
  if HandleIsValid then
    XMoveWindow(xapplication.display, FWinHandle, x, y);
end;

function TfpgX11Window.DoWindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint;
var
  dx: integer;
  dy: integer;
  cw: TfpgWinHandle;
begin
  if not TfpgX11Window(ASource).HandleIsValid then
    Exit; //==>

  XTranslateCoordinates(xapplication.display, TfpgX11Window(ASource).WinHandle,
      XDefaultRootWindow(xapplication.display), AScreenPos.X, AScreenPos.Y, @dx, @dy, @cw);

  Result.X := dx;
  Result.Y := dy;
end;

procedure TfpgX11Window.DoUpdateWindowPosition;
var
  w: longword;
  h: longword;
begin
  if HasHandle then
  begin
    if FWidth > 1 then
      w := FWidth
    else
      w := 1;
    if FHeight > 1 then
      h := FHeight
    else
      h := 1;

    XMoveResizeWindow(xapplication.display, FWinHandle, FLeft, FTop, w, h);
  end;
end;

procedure TfpgX11Window.DoSetMouseCursor;
var
  xc: TCursor;
  shape: integer;
begin
  if not HasHandle then
  begin
    FMouseCursorIsDirty := True;
    Exit; //==>
  end;

  case FMouseCursor of
    mcDefault:    shape := XC_left_ptr;
    mcArrow:      shape := XC_arrow;
    mcSizeEW:     shape := XC_sb_h_double_arrow;
    mcSizeNS:     shape := XC_sb_v_double_arrow;
    mcIBeam:      shape := XC_xterm;
    mcSizeNWSE:   shape := XC_bottom_right_corner;
    mcSizeNESW:   shape := XC_bottom_left_corner;
    mcSizeSWNE:   shape := XC_top_right_corner;
    mcSizeSENW:   shape := XC_top_left_corner;
    mcMove:       shape := XC_fleur;
    mcCross:      shape := XC_crosshair;
    mcHourGlass:  shape := XC_watch;
    mcHand:       shape := XC_hand2;
    mcDrag:       shape := XC_target;
    mcNoDrop:     shape := XC_pirate;
  else
    shape := XC_left_ptr; //XC_arrow;
  end;

  xc := XCreateFontCursor(xapplication.Display, shape);
  XDefineCursor(xapplication.Display, FWinHandle, xc);
  XFreeCursor(xapplication.Display, xc);

  FMouseCursorIsDirty := False;
end;

procedure TfpgX11Window.DoDNDEnabled(const AValue: boolean);
begin
  // notify XDND protocol that we can handle DND
  if AValue then
  begin
    if HasHandle then
      XChangeProperty(xapplication.Display, WinHandle, xapplication.XdndAware, XA_ATOM, 32, PropModeReplace, @FPG_XDND_VERSION, 1)
    else
      QueueEnabledDrops := True; // we need to do this once we have a winhandle
  end
  else
    XDeleteProperty(xapplication.Display, WinHandle, xapplication.XdndAware);
end;

procedure TfpgX11Window.DoAcceptDrops(const AValue: boolean);
begin
  { TODO : Remove EnableDrops, then recurse from here to parent top level from, and set XDNDAware property for form. }
end;

function TfpgX11Window.GetWindowState: TfpgWindowState;
type
  TWMStateType = (wms_none, wms_normal, wms_withdrawn, wms_iconic);
  wmstate = record
    state: longword;
    icon: PtrUInt;
  end;
  pwmstate = ^wmstate;
var
  actualtype: TAtom = None;
  actualformat: cint;
  count, remaining: culong;
  data: pwmstate = nil;
  lWindowStates: TNetWindowStates;
  maxh, maxv: boolean;
begin
  Result := inherited GetWindowState;

  if XGetWindowProperty(xapplication.Display, FWinHandle, xapplication.xia_wm_state, 0, 1,
      TBool(False), xapplication.xia_wm_state, @actualtype, @actualformat, @count,
      @remaining, @data) = Success then
  begin
    if (actualformat = 32) and (count = 1) then
    begin
      case TWMStateType(data^.State) of
        wms_none:
            begin
              // do nothing
            end;
        wms_normal:
            begin
              Result := wsNormal;
              maxh := false;
              maxv := false;
              xapplication.netlayer.WindowGetState(FWinHandle, lWindowStates);
              if nwsFullScreen in lWindowStates then
                Result := wsMaximized; // not really true, but ok for now
              if nwsMaxVert in lWindowStates then
                maxv := True;
              if nwsMaxHorz in lWindowStates then
                maxh := True;
              if (Result = wsNormal) and maxv and maxh then
                Result := wsMaximized;
            end;
        wms_withdrawn:
            begin
              // do nothing
            end;
        wms_iconic:
            begin
              Result := wsMinimized;
            end;
      end; { case }
    end; { if }
  end;  { if }

  if data <> nil then
    XFree(data);
end;

procedure TfpgX11Window.DoSetWindowTitle(const ATitle: string);
var
  tp: TXTextProperty;
begin
  if FWinHandle <= 0 then
    Exit;
  fpgApplication.netlayer.WindowSetName(FWinHandle, PChar(ATitle));

  // Required for titles to work in IceWM. The above netlayer doesn't do the trick.
  tp.value    := PCUChar(ATitle);
  tp.encoding := XA_WM_NAME;
  tp.format   := 8;
  tp.nitems   := UTF8Length(ATitle);

  XSetWMName(xapplication.Display, FWinHandle, @tp);
  XStoreName(xapplication.Display, FWinHandle, PChar(ATitle));
  XSetIconName(xapplication.Display, FWinHandle, PChar(ATitle));
  XSetWMIconName(xapplication.Display, FWinHandle, @tp);
end;

constructor TfpgX11Window.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWinHandle := 0;
  FBackupWinHandle := 0;
  QueueEnabledDrops := False;
end;

procedure TfpgX11Window.ActivateWindow;
begin
  XSetInputFocus(xapplication.Display, FWinHandle, RevertToParent, CurrentTime);
end;

procedure TfpgX11Window.CaptureMouse;
begin
  XGrabPointer(xapplication.Display, FWinHandle,
      TBool(False),
      ButtonPressMask or ButtonReleaseMask or ButtonMotionMask or PointerMotionMask
        or EnterWindowMask or LeaveWindowMask,
      GrabModeAsync,
      GrabModeAsync,
      None,
      0,
      CurrentTime
      );
end;

procedure TfpgX11Window.ReleaseMouse;
begin
  XUngrabPointer(xapplication.display, CurrentTime);
end;

procedure TfpgX11Window.SetFullscreen(AValue: Boolean);
begin
  inherited SetFullscreen(AValue);
  fpgApplication.netlayer.WindowSetFullscreen(FWinHandle, AValue);
end;

procedure TfpgX11Window.BringToFront;
begin
  if HasHandle then
    XRaiseWindow(xapplication.display, FWinHandle);
end;

{ TfpgX11FontResource }

function TfpgX11FontResource.DoGetTextWidthClassic(const txt: string): integer;
var
  extents: TXGlyphInfo;
begin
  XftTextExtentsUTF8(xapplication.display, FFontData, PChar(txt), Length(txt), extents);
  Result := extents.xOff;
end;

function TfpgX11FontResource.DoGetTextWidthWorkaround(const txt: string): integer;
var
  extents: TXGlyphInfo;
  ch: string;
  dpos: integer;
begin
  Result := 0;
  dpos   := 1;
  while dpos <= Length(txt) do
  begin
    dpos := UTF8CharAtByte(txt, dpos, ch);
    XftTextExtentsUTF8(xapplication.display, FFontData, PChar(ch), Length(ch), extents);
    Inc(Result, extents.xOff);
  end;
end;

constructor TfpgX11FontResource.Create(const afontdesc: string);
begin
  FFontData := XftFontOpenName(xapplication.display, xapplication.DefaultScreen, PChar(afontdesc));
end;

destructor TfpgX11FontResource.Destroy;
begin
  if HandleIsValid then
    XftFontClose(xapplication.Display, FFontData);
  inherited;
end;

function TfpgX11FontResource.HandleIsValid: boolean;
begin
  Result := (FFontData <> nil);
end;

function TfpgX11FontResource.GetAscent: integer;
begin
  Result := FFontData^.ascent;
end;

function TfpgX11FontResource.GetDescent: integer;
begin
  Result := FFontData^.descent;
end;

function TfpgX11FontResource.GetHeight: integer;
begin
  Result := FFontData^.Height;
end;

function TfpgX11FontResource.GetTextWidth(const txt: string): integer;
begin
  if length(txt) < 1 then
  begin
    Result := 0;
    Exit;
  end;
  // Xft uses smallint to return text extent information, so we have to
  // check if the text width is small enough to fit into smallint range
  if DoGetTextWidthClassic('W') * Length(txt) < High(smallint) then
    Result := DoGetTextWidthClassic(txt)
  else
    Result := DoGetTextWidthWorkaround(txt);
end;

{ TfpgX11Canvas }

constructor TfpgX11Canvas.Create(awin: TfpgWindowBase);
begin
  inherited Create(awin);
  FDrawing    := False;
  FDrawWindow := nil;

  FBufferPixmap := 0;
  FDrawHandle   := 0;
  Fgc           := nil;
  FXftDraw      := nil;
  FClipRegion   := nil;
end;

destructor TfpgX11Canvas.Destroy;
begin
  if FDrawing then
    DoEndDraw;
  FreeAndNil(FBufferFreeTimer);
  TryFreePixmap;
  inherited Destroy;
end;

procedure TfpgX11Canvas.DoBeginDraw(awin: TfpgWindowBase; buffered: boolean);
var
  x: integer;
  y: integer;
  rw: TXID;
  d: TXID;
  w: longword;
  h: longword;
  bw: longword;
  pmw: longword;
  pmh: longword;
  GcValues: TXGcValues;
begin
  if Assigned(TfpgX11Window(awin)) then
  begin
    // This occurs every now and again with TfpgMemo and InvertCaret painting!
    // Investigate this.
    if not TfpgX11Window(awin).HasHandle then
      raise Exception.Create('Window doesn''t have a Handle');
  end;

  XGetGeometry(xapplication.display, TfpgX11Window(awin).FWinHandle, @rw, @x, @y, @w, @h, @bw, @d);

  if FDrawing and buffered and (FBufferPixmap > 0) then
    if FBufferPixmap > 0 then
    begin
      // check if the dimensions are ok
      XGetGeometry(xapplication.display, FBufferPixmap, @rw, @x, @y, @pmw, @pmh, @bw, @d);
      if (pmw <> w) or (pmh <> h) then
        DoEndDraw;
    end;

  if not FDrawing then
  begin
    FDrawWindow := TfpgX11Window(awin);

    if buffered then
    begin
      if (FBufferPixmap = 0)
      or (FastDoubleBuffer = False)
      or (FastDoubleBuffer and (w > FPixWidth) or (h > FPixHeight))
      or ((FastDoubleBuffer = False) and ((w <> FPixWidth) or (h <> FPixHeight)))
      then
      begin
        if FastDoubleBuffer and ((w > FPixWidth) or (h > FPixHeight)) then
        begin
          FPixHeight := h + 30;
          FPixWidth  := w + 30;
        end
        else begin
          FPixHeight := h;
          FPixWidth  := w;
        end;
        TryFreePixmap;
        FBufferPixmap := XCreatePixmap(xapplication.display, FDrawWindow.FWinHandle, FPixWidth, FPixHeight, xapplication.DisplayDepth);
      end;
      if FastDoubleBuffer then
      begin
        // Rapid paint events reuse the double buffer which resets a delay
        // After the delay the double buffer is freed, letting the OS use video
        // memory if needed.
        // Things like scrolling and resizing are fast

        // Reset the timers next trigger
        if FBufferFreeTimer = nil then
        begin
          FBufferFreeTimer := TfpgTimer.Create(500);
          TfpgTimer(FBufferFreeTimer).OnTimer := @BufferFreeTimer;
        end
        else
          TfpgTimer(FBufferFreeTimer).Enabled := False;
        TfpgTimer(FBufferFreeTimer).Enabled := True;
      end;
      FDrawHandle   := FBufferPixmap;
    end
    else
    begin
      TryFreePixmap;
      FDrawHandle   := FDrawWindow.FWinHandle;
    end;

    Fgc := XCreateGc(xapplication.display, FDrawHandle, 0, @GcValues);
    // CapNotLast is so we get the same behavior as Windows. See documentation for more details.
    XSetLineAttributes(xapplication.display, Fgc, 0, LineSolid, CapNotLast, JoinMiter);

    FXftDraw := XftDrawCreate(xapplication.display, FDrawHandle,
      XDefaultVisual(xapplication.display, xapplication.DefaultScreen),
      XDefaultColormap(xapplication.display, xapplication.DefaultScreen));

    FClipRegion := XCreateRegion;
  end;

  FDrawing := True;
end;

procedure TfpgX11Canvas.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
var
  cgc: TfpgGContext;
  GcValues: TXGcValues;
begin
  if FBufferPixmap > 0 then
  begin
    cgc := XCreateGc(xapplication.display, FBufferPixmap, 0, @GcValues);
    XCopyArea(xapplication.Display, FBufferPixmap, FDrawWindow.FWinHandle, cgc, x, y, w, h, x, y);
    XFreeGc(xapplication.display, cgc);
  end;
end;

procedure TfpgX11Canvas.DoEndDraw;
begin
  if FDrawing then
  begin
    XDestroyRegion(FClipRegion);
    XftDrawDestroy(FXftDraw);
    XFreeGc(xapplication.display, Fgc);

    if FastDoubleBuffer = False then
      TryFreePixmap;

    FDrawing    := False;
    FDrawWindow := nil;
  end;
end;

function TfpgX11Canvas.GetPixel(X, Y: integer): TfpgColor;
var
  Image: PXImage;
  Pixel: Cardinal;
  x_Color: TXColor;
begin
  Result := 0;

  Image := XGetImage(xapplication.display, FDrawHandle, X, Y, 1, 1, $FFFFFFFF, ZPixmap);
  if Image = nil then
    raise Exception.Create('fpGFX/X11: Invalid XImage');

  try
    Pixel := XGetPixel(Image, 0, 0);
    x_Color.pixel := Pixel;

    XQueryColor(xapplication.display, xapplication.DefaultColorMap, @x_Color);
    Result := TfpgColor(((x_Color.red and $00FF) shl 16) or
                       ((x_Color.green and $00FF) shl 8) or
                        (x_Color.blue and $00FF));
  finally
    XDestroyImage(Image);
  end;
end;

procedure TfpgX11Canvas.SetPixel(X, Y: integer; const AValue: TfpgColor);
var
  oldColor: TfpgColor;
begin
  oldColor := Color;
  SetColor(AValue);
  XDrawPoint(xapplication.display, FDrawHandle, Fgc, X, Y);
  SetColor(oldColor);
end;

procedure TfpgX11Canvas.DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
begin
  XDrawArc(xapplication.display, FDrawHandle, Fgc, x, y, w-1, h-1,
      Trunc(64 * a1), Trunc(64 * a2));
end;

procedure TfpgX11Canvas.DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
begin
  XFillArc(xapplication.display, FDrawHandle, Fgc, x, y, w, h,
      Trunc(64 * a1), Trunc(64 * a2));
end;

procedure TfpgX11Canvas.DoDrawPolygon(Points: fpg_base.PPoint; NumPts: Integer; Winding: boolean);
var
  PointArray: PXPoint;
  i: integer;
begin
  { convert TPoint to TXPoint }
  GetMem(PointArray, SizeOf(TXPoint)*(NumPts+1)); // +1 for return line
  for i := 0 to NumPts-1 do
  begin
    PointArray[i].x := Points[i].x;
    PointArray[i].y := Points[i].y;
  end;
  XFillPolygon(xapplication.display, FDrawHandle, Fgc, PointArray, NumPts, CoordModeOrigin, X.Complex);
  if PointArray <> nil then
    FreeMem(PointArray);
end;

procedure TfpgX11Canvas.BufferFreeTimer(Sender: TObject);
begin
  {$IFDEF DEBUG}
  WriteLn('fpGFX/X11: Freeing Buffer w=', FPixWidth, ' h=', FPixHeight);
  {$ENDIF}
  TryFreePixmap;
  FreeAndNil(FBufferFreeTimer);
end;

procedure TfpgX11Canvas.TryFreePixmap;
begin
  if FBufferPixmap > 0 then
    XFreePixmap(xapplication.Display, FBufferPixmap);
  FBufferPixmap := 0;
end;

procedure TfpgX11Canvas.DoSetFontRes(fntres: TfpgFontResourceBase);
begin
  if fntres = nil then
    Exit; //==>
  FCurFontRes := TfpgX11FontResource(fntres);
end;

procedure TfpgX11Canvas.DoSetTextColor(cl: TfpgColor);
begin
  { We use fpgColorToX() because we don't want Alpha channel information for X11 text }
  SetXftColor(fpgColorToX(cl), FColorTextXft);
end;

procedure TfpgX11Canvas.DoSetColor(cl: TfpgColor);
begin
  XSetForeGround(xapplication.display, Fgc, fpgColorToX(cl));
end;

procedure TfpgX11Canvas.DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle);
const
  cDot: array[0..1] of Char = #1#1;
  cDash: array[0..1] of Char = #4#2;
  cDashDot: array[0..3] of Char = #4#1#1#1;
  cDashDotDot: array[0..5] of Char = #4#1#1#1#1#1;
var
  aCapStyle: Longint;
begin
  aCapStyle := CapNotLast;
  // Is this still needed??  I don't think so
  //if (awidth > 1) and (astyle = lsSolid) then
  //  aCapStyle := CapButt;
  if awidth = 1 then
    awidth := 0;  // switch to hardware algorithm

  case AStyle of
    lsDot:
        begin
          XSetLineAttributes(xapplication.display, Fgc, awidth,
            LineOnOffDash, aCapStyle, JoinMiter);
          XSetDashes(xapplication.display, Fgc, 0, cDot, 2);
        end;
    lsDash:
        begin
          XSetLineAttributes(xapplication.display, Fgc, awidth,
            LineOnOffDash, aCapStyle, JoinMiter);
          XSetDashes(xapplication.display, Fgc, 0, cDash, 2);
        end;
    lsDashDot:
        begin
          XSetLineAttributes(xapplication.display, Fgc, awidth,
            LineOnOffDash, aCapStyle, JoinMiter);
          XSetDashes(xapplication.display, Fgc, 0, cDashDot, 4);
        end;
    lsDashDotDot:
        begin
          XSetLineAttributes(xapplication.display, Fgc, awidth,
            LineOnOffDash, aCapStyle, JoinMiter);
          XSetDashes(xapplication.display, Fgc, 0, cDashDotDot, 6);
        end;
    else  // which includes lsSolid
      XSetLineAttributes(xapplication.display, Fgc, awidth,
        LineSolid, aCapStyle, JoinMiter);
  end;  { case }
end;

procedure TfpgX11Canvas.DoDrawString(x, y: TfpgCoord; const txt: string);
begin
  if Length(txt) < 1 then
    Exit; //==>

  XftDrawStringUTF8(FXftDraw, FColorTextXft, FCurFontRes.Handle, x,
    y + FCurFontRes.GetAscent, PChar(txt), Length(txt));
end;

procedure TfpgX11Canvas.DoGetWinRect(out r: TfpgRect);
var
  rw: TfpgWinHandle;
  x: integer;
  y: integer;
  bw: longword;
  d: longword;
begin
  r.Left    := 0;
  r.Top     := 0;
  XGetGeometry(xapplication.display, FDrawWindow.FWinHandle, @rw, @x, @y,
      @(r.width), @(r.height), @bw, @d);
end;

procedure TfpgX11Canvas.DoFillRectangle(x, y, w, h: TfpgCoord);
begin
  XFillRectangle(xapplication.display, FDrawHandle, Fgc, x, y, w, h);
end;

procedure TfpgX11Canvas.DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord);
begin
  XSetForeGround(xapplication.display, Fgc, fpgColorToX(fpgColorToRGB(col)));
  XSetFunction(xapplication.display, Fgc, GXxor);
  XFillRectangle(xapplication.display, FDrawHandle, Fgc, x, y, w, h);
  XSetForeGround(xapplication.display, Fgc, 0);
  XSetFunction(xapplication.display, Fgc, GXcopy);
end;

procedure TfpgX11Canvas.DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord);
var
  pts: array[1..3] of TXPoint;
begin
  pts[1].x := x1;   pts[1].y := y1;
  pts[2].x := x2;   pts[2].y := y2;
  pts[3].x := x3;   pts[3].y := y3;

  XFillPolygon(xapplication.display, FDrawHandle, Fgc, @pts, 3, CoordModeOrigin, X.Complex);
end;

procedure TfpgX11Canvas.DoDrawRectangle(x, y, w, h: TfpgCoord);
begin
//  writeln(Format('DoDrawRectangle  x=%d y=%d w=%d h=%d', [x, y, w, h]));
  // Same behavior as Windows. See documentation for reason.
  if (w = 1) and (h = 1) then // a dot
    DoDrawLine(x, y, x+w, y+w)
  else
    XDrawRectangle(xapplication.display, FDrawHandle, Fgc, x, y, w-1, h-1);
end;

procedure TfpgX11Canvas.DoDrawLine(x1, y1, x2, y2: TfpgCoord);
begin
  // Same behavior as Windows. See documentation for reason.
  XDrawLine(xapplication.display, FDrawHandle, Fgc, x1, y1, x2, y2);
end;

procedure TfpgX11Canvas.DoSetClipRect(const ARect: TfpgRect);
var
  r: TXRectangle;
  rg: TRegion;
begin
  r.x      := ARect.Left;
  r.y      := ARect.Top;
  r.Width  := ARect.Width;
  r.Height := ARect.Height;

  rg := XCreateRegion;

  XUnionRectWithRegion(@r, rg, FClipRegion);
  XSetRegion(xapplication.display, Fgc, FClipRegion);
  XftDrawSetClip(FXftDraw, FClipRegion);

  FClipRect    := ARect;
  FClipRectSet := True;
  XDestroyRegion(rg);
end;

function TfpgX11Canvas.DoGetClipRect: TfpgRect;
begin
  Result := FClipRect;
end;

procedure TfpgX11Canvas.DoAddClipRect(const ARect: TfpgRect);
var
  r: TXRectangle;
  rg: TRegion;
begin
  r.x      := ARect.Left;
  r.y      := ARect.Top;
  r.Width  := ARect.Width;
  r.Height := ARect.Height;

  rg := XCreateRegion;
  XUnionRectWithRegion(@r, rg, rg);
  XIntersectRegion(FClipRegion, rg, FClipRegion);
  XSetRegion(xapplication.display, Fgc, FClipRegion);

  FClipRect    := ARect;    // Double check this, it might be wrong!!
  FClipRectSet := True;

  XftDrawSetClip(FXftDraw, FClipRegion);
  XDestroyRegion(rg);
end;

procedure TfpgX11Canvas.DoClearClipRect;
var
  r: TfpgRect;
begin
  DoGetWinRect(r);
  DoSetClipRect(r);
  FClipRectSet := False;
end;

procedure TfpgX11Canvas.DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer);
var
  msk: TPixmap;
  gc2: Tgc;
  drawgc: Tgc;
  GcValues: TXGcValues;
begin
  if img = nil then
    Exit; //==>

  if img.Masked then
  begin
    // rendering the mask
    msk := XCreatePixmap(xapplication.display, XDefaultRootWindow(xapplication.display), w, h, 1);
    GcValues.foreground := 1;
    GcValues.background := 0;

    // clear mask
    gc2 := XCreateGc(xapplication.display, msk, GCForeground or GCBackground, @GcValues);
    XSetForeground(xapplication.display, gc2, 0);
    XFillRectangle(xapplication.display, msk, gc2, 0, 0, w, h);

    XSetForeground(xapplication.display, gc2, 1);
    XPutImage(xapplication.display, msk, gc2, TfpgX11Image(img).XImageMask, xi, yi, 0, 0, w, h);

    drawgc := XCreateGc(xapplication.display, FDrawHandle, 0, @GcValues);
    XSetClipMask(xapplication.display, drawgc, msk);
    XSetClipOrigin(xapplication.display, drawgc, x, y);

    XPutImage(xapplication.display, FDrawHandle, drawgc, TfpgX11Image(img).XImage, xi, yi, x, y, w, h);
    XFreePixmap(xapplication.display, msk);
    XFreeGc(xapplication.display, drawgc);
    XFreeGc(xapplication.display, gc2);
  end
  else
    XPutImage(xapplication.display, FDrawHandle, Fgc, TfpgImage(img).XImage, xi, yi, x, y, w, h);
end;

{ TfpgX11Image }

constructor TfpgX11Image.Create;
begin
  inherited Create;
end;

procedure TfpgX11Image.DoFreeImage;
begin
  // does nothing on X11
end;

procedure TfpgX11Image.DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: Pointer);
begin
  FMasked := False;

  with FXimg do
  begin
    Width          := awidth;
    Height         := aheight;
    xoffset        := 0;
    obdata         := #0;
    byte_order     := LSBFirst;
    bitmap_bit_order := LSBFirst; //MSBFirst;
    bitmap_pad     := 32;
    bytes_per_line := 0;

    if acolordepth = 1 then
    begin
      format         := XYBitmap;
      bitmap_unit    := 8;
      depth          := 1;
      bits_per_pixel := 1;
      red_mask       := 1;
      green_mask     := 0;
      blue_mask      := 0;
    end
    else
    begin
      format      := ZPixmap;
      bitmap_unit := 32;

      // only truecolor 24/32 displays supported now, otherwise color conversion required!
      // this must be match for the display !!!
      depth          := fpgApplication.DisplayDepth;
      bits_per_pixel := 32;

      // Shouldn't we rather get this from XDefaultVisualOfScreen(). PVisual?
      red_mask   := $000000FF;
      green_mask := $0000FF00;
      blue_mask  := $00FF0000;
    end;

    Data := aimgdata;
  end;

  XInitImage(@FXimg);
end;

procedure TfpgX11Image.DoInitImageMask(awidth, aheight: integer; aimgdata: Pointer);
begin
  FMasked := True;

  with FXimgMask do
  begin
    Width          := awidth;
    Height         := aheight;
    xoffset        := 0;
    format         := XYBitmap;
    byte_order     := LSBFirst;
    bitmap_unit    := 8;
    bitmap_bit_order := MSBFirst;
    bitmap_pad     := 32;
    depth          := 1;
    bytes_per_line := 0;
    bits_per_pixel := 1;

    red_mask   := 1;
    green_mask := 0;
    blue_mask  := 0;

    obdata := #0;

    Data := aimgdata;
  end;

  XInitImage(@FXimgMask);
end;

function TfpgX11Image.XImage: PXImage;
begin
  Result := @FXimg;
end;

function TfpgX11Image.XImageMask: PXImage;
begin
  Result := @FXimgMask;
end;

{ TfpgX11Clipboard }

function TfpgX11Clipboard.DoGetText: TfpgString;
begin
  XConvertSelection(xapplication.Display, xapplication.xia_clipboard,
      XA_STRING, xapplication.xia_clipboard, FClipboardWndHandle, 0);

  FWaitingForSelection := True;
  fpgDeliverMessages; // delivering the remaining messages

  repeat
    fpgWaitWindowMessage;
    fpgDeliverMessages;
  until not FWaitingForSelection;

  Result := FClipboardText;
end;

procedure TfpgX11Clipboard.DoSetText(const AValue: TfpgString);
begin
  FClipboardText := AValue;
  XSetSelectionOwner(xapplication.Display, xapplication.xia_clipboard,
      FClipboardWndHandle, CurrentTime);
end;

procedure TfpgX11Clipboard.InitClipboard;
begin
  FWaitingForSelection := False;
  FClipboardWndHandle := XCreateSimpleWindow(xapplication.Display,
      xapplication.RootWindow, 10, 10, 10, 10, 0, 0, 0);
end;

{ TfpgX11FileList }

function TfpgX11FileList.EncodeModeString(FileMode: longword): TFileModeString;
const
  modestring: string[9] = 'xwrxwrxwr';  // must be in reverse order
var
  b: integer;
  n: integer;
begin
  // rights
  //rwx rwx rwx
  b := 1;
  n := 1;
  Result := '';
  while n <= 9 do
  begin
    if (FileMode and b) = 0 then
      Result := '-' + Result
    else
      Result := modestring[n] + Result;
    inc(n);
    b := b shl 1;
  end;
end;

constructor TfpgX11FileList.Create;
begin
  inherited Create;
  FHasFileMode := true;
end;

function TfpgX11FileList.InitializeEntry(sr: TSearchRec): TFileEntry;
var
  info: Tstat;
  fullname: TfpgString;
begin
  Result := inherited InitializeEntry(sr);
  if Assigned(Result) then
  begin
    fullname            := DirectoryName + Result.Name;
    Result.LinkTarget   := ExtractTargetSymLinkPath(fullname);
    Result.IsLink       := (Result.LinkTarget<>'');
    Result.IsExecutable := ((sr.Mode and $40) <> 0);
    Result.mode         := EncodeModeString(sr.Mode);
    Fpstat(PChar(fullname), info);
    // Especially if files are transfered on removable media the host system
    // might not have those user or group ids. So name lookups will fail. This
    // simply returns the ID's in such cases.
    {$IFDEF LINUX}
    try
      Result.Owner := GetUserName(TUID(info.st_uid));
    except
      Result.Owner := IntToStr(info.st_uid);
    end;
    try
      Result.Group := GetGroupName(TGID(info.st_gid));
    except
      Result.Group := IntToStr(info.st_gid);
    end;
    {$ELSE}
      // Other *nix systems dont' seem to have a users.pp unit in FPC.
      Result.Owner := IntToStr(info.st_uid);
      Result.Group := IntToStr(info.st_gid);
    {$ENDIF}
  end;
end;

procedure TfpgX11FileList.PopulateSpecialDirs(const aDirectory: TfpgString);
var
  ds: string;
begin
  FSpecialDirs.Clear;
  FSpecialDirs.Add(DirectorySeparator); // add root

  ds := aDirectory;
  if Copy(ds, 1, 1) <> DirectorySeparator then
    ds := DirectorySeparator + ds;

  inherited PopulateSpecialDirs(ds);
end;


{ TfpgX11Drag }

procedure TfpgX11Drag.SetTypeListProperty;
begin
  XChangeProperty(xapplication.Display, FSource.WinHandle,
      xapplication.XdndTypeList, XA_ATOM, 32,
      PropModeReplace, @FMimeTypesArray[0], Length(FMimeTypesArray));
end;

procedure TfpgX11Drag.InitializeMimeTypesToAtoms;
var
  sl: TStringList;
  i: integer;
  a: TAtom;
  s: PChar;
begin
  { free old array }
  SetLength(FMimeTypesArray, 0);
  { set size of new array. Extra element for the terminating x.None value }
  SetLength(FMimeTypesArray, FMimedata.Count+1);

  sl := FMimeData.Formats as TStringList;
  try
    for i := 0 to FMimeData.Count-1 do
    begin
      s := PChar(sl[i]);
      a := XInternAtom(xapplication.Display, s, TBool(False));
      FMimeTypesArray[i] := a;
    end;
    FMimeTypesArray[i+1] := x.None; // termination value
  finally
    sl.Free;
  end;
end;

procedure TfpgX11Drag.Dragging(ev: TXEvent);
var
  lTarget: TWindow;
begin
  lTarget := FindWindow(ev.xmotion.root, ev.xmotion.x_root, ev.xmotion.y_root);
  if FLastTarget <> lTarget then
  begin
    if FLastTarget <> 0 then { meaning we had a target before }
      SendDNDLeave(FLastTarget);

    FLastTarget       := lTarget;
    FTargetIsDNDAware := IsDNDAware(lTarget);
    {$IFDEF DNDDEBUG}
    writeln('IsDNDAware = ', BoolToStr(FTargetIsDNDAware, True));
    {$ENDIF}
    FStatusPending    := False;
    FDropAccepted     := False;
    FAcceptedAction   := X.None;

    if FTargetIsDNDAware then
      SendDNDEnter(FLastTarget);
  end;

  if FTargetIsDNDAware and not FStatusPending then
  begin
    SendDNDPosition(FLastTarget, ev.xmotion.x_root, ev.xmotion.y_root,
        FProposedAction, ev.xmotion.time);
    // this is to avoid sending XdndPosition messages over and over
    // if the target is not responding
    FStatusPending := True;
  end;
end;

function TfpgX11Drag.IsDNDAware(win: TWindow): boolean;
var
  actualtype: TAtom;
  actualformat: cint;
  count, remaining: culong;
  data: PChar;
  lversion: culong;
begin
  if (win = None) then
  begin
    Result := False;
    exit;
  end;
  XGetWindowProperty(xapplication.Display, win, xapplication.XdndAware, 0, $8000000,
      TBool(False), XA_ATOM, @actualtype, @actualformat, @count, @remaining, @data);

  if count = 0 then
  begin
    if data <> nil then
      XFree(data);
    Result := False;
    exit;
  end;

  lversion := Integer(data[0]);
  FUseVersion := min(Integer(FPG_XDND_VERSION), Integer(lversion));
  Result := True;

  {$IFDEF DNDDEBUG}
  writeln(Format('IsDNDAware theirs:%d  ours:%d  using:%d', [lversion, FPG_XDND_VERSION, FUseVersion]));
  {$ENDIF}
end;

procedure TfpgX11Drag.SendDNDLeave(ATarget: TWindow);
var
  xev: TXEvent;
begin
  xev.xany._type        := X.ClientMessage;
  xev.xany.display      := xapplication.Display;
  xev.xclient.window    := ATarget;
  xev.xclient.message_type := xapplication.XdndLeave;
  xev.xclient.format    := 32;

  xev.xclient.data.l[0] := FSource.WinHandle;
  xev.xclient.data.l[1] := 0;

  xev.xclient.data.l[2] := 0;
  xev.xclient.data.l[3] := 0;
  xev.xclient.data.l[4] := 0;

  XSendEvent(xapplication.Display, ATarget, False, NoEventMask, @xev);
end;

procedure TfpgX11Drag.SendDNDEnter(ATarget: TWindow);
var
  xev: TXEvent;
  i, n: integer;
begin
  xev.xany._type       := X.ClientMessage;
  xev.xany.display     := xapplication.Display;
  xev.xclient.window   := ATarget;
  xev.xclient.message_type := xapplication.XdndEnter;
  xev.xclient.format   := 32;

  xev.xclient.data.l[0] := FSource.WinHandle;

  n := FMimeData.Count;

  if n > 3 then
    i := 1
  else
    i := 0;
  xev.xclient.data.l[1] := i or (FUseVersion shl 24);

  if n <= 3 then
  begin
    for i := 0 to 2 do
    begin
      if i < n then
        xev.xclient.data.l[2+i] := FMimeTypesArray[i]
      else
        xev.xclient.data.l[2+i] := x.None;
    end;
  end
  else
  begin
    { available types are in the XdndTypeList property instead }
    xev.xclient.data.l[2] := x.None;
    xev.xclient.data.l[3] := x.None;
    xev.xclient.data.l[4] := x.None;
  end;

  XSendEvent(xapplication.Display, ATarget, False, NoEventMask, @xev);
end;

procedure TfpgX11Drag.SendDNDPosition(ATarget: TWindow; x_root: cint;
  y_root: cint; AAction: TAtom; ATime: X.TTime);
var
  xev: TXEvent;
begin
  xev.xany._type       := X.ClientMessage;
  xev.xany.display     := xapplication.Display;
  xev.xclient.window   := ATarget;
  xev.xclient.message_type := xapplication.XdndPosition;
  xev.xclient.format   := 32;

  xev.xclient.data.l[0] := FSource.WinHandle;
  xev.xclient.data.l[1] := 0;

  xev.xclient.data.l[2] := (x_root shl 16) or y_root;    // root coordinates
  xev.xclient.data.l[3] := ATime;   // timestamp for retrieving data
  xev.xclient.data.l[4] := AAction; // requested action

  XSendEvent(xapplication.Display, ATarget, False, NoEventMask, @xev);
end;

procedure TfpgX11Drag.SendDNDDrop;
var
  xev: TXEvent;
begin
  if FDropAccepted then
  begin
    FillChar(xev, SizeOf(TXEvent), 0);

    xev.xany._type      := X.ClientMessage;
    xev.xany.display    := xapplication.Display;
    xev.xclient.window  := FLastTarget;
    xev.xclient.message_type := xapplication.XdndDrop;
    xev.xclient.format  := 32;

    xev.xclient.data.l[0] := FSource.WinHandle;    // from
    xev.xclient.data.l[1] := 0;                // reserved
    xev.xclient.data.l[2] := CurrentTime;       // timestamp
    xev.xclient.data.l[3] := 0;
    xev.xclient.data.l[4] := 0;

    XSendEvent(xapplication.Display, FLastTarget, False, NoEventMask, @xev);

    uDragSource := FSource as TfpgWidget;
  end
  else
  begin
    SendDNDLeave(FLastTarget);
    uDragSource := nil;
  end;
  FSource.MouseCursor := mcDefault;
end;

procedure TfpgX11Drag.HandleDNDStatus(ATarget: TWindow; AAccept: integer;
  ARect: TfpgRect; AAction: TAtom);
begin
  if ATarget = FLastTarget then
  begin
    FStatusPending := False;
    if AAccept = 1 then
    begin
      FDropAccepted := True;
      FAcceptedAction := AAction;
      FSource.MouseCursor := mcDrag;
    end
    else
    begin
      FDropAccepted := False;
      FAcceptedAction := X.None; { AAction should equal None, but lets just make sure }
      FSource.MouseCursor := mcNoDrop;
    end;
  end;
  { TODO: If we waited to long, we have a timeout }
end;

procedure TfpgX11Drag.HandleSelectionRequest(ev: TXEvent);
var
  e: TXSelectionEvent;
  s: string;
  v: variant;
begin
  e._type       := SelectionNotify;
  e.requestor   := ev.xselectionrequest.requestor;
  e.selection   := ev.xselectionrequest.selection;
  e.target      := ev.xselectionrequest.target;
  e.time        := ev.xselectionrequest.time;
  e._property   := ev.xselectionrequest._property;

  s := XGetAtomName(xapplication.Display, e.target);
  if FMimeData.HasFormat(s) then
  begin
    if s = 'text/plain' then
      XChangeProperty(xapplication.Display, e.requestor, e._property, e.target,
        8, PropModeReplace, PByte(@FMimeData.Text[1]), Length(FMimeData.Text))
    else if s = 'text/html' then
    begin
      XChangeProperty(xapplication.Display, e.requestor, e._property, e.target,
        8, PropModeReplace, PByte(@FMimeData.HTML[1]), Length(FMimeData.HTML))
    end
    else
    begin
      { transfering as raw bytes of data }
      v := FMimeData.GetData(s);
      s := v;
      XChangeProperty(xapplication.Display, e.requestor, e._property, e.target,
        8, PropModeReplace, PByte(@s[1]), Length(s));
    end;

  end;

  XSendEvent(xapplication.Display, e.requestor, false, NoEventMask, @e );
end;

function TfpgX11Drag.GetSource: TfpgX11Window;
begin
  Result := FSource;
end;

destructor TfpgX11Drag.Destroy;
begin
  {$IFDEF DNDDEBUG}
  writeln('TfpgX11Drag.Destroy ');
  {$ENDIF}
  FSource.MouseCursor := mcDefault;
  XDeleteProperty(xapplication.Display, FSource.WinHandle, xapplication.XdndAware);
  XDeleteProperty(xapplication.Display, FSource.WinHandle, xapplication.XdndTypeList);
  SetLength(FMimeTypesArray, 0);
  inherited Destroy;
end;

function TfpgX11Drag.Execute(const ADropActions: TfpgDropActions; const ADefaultAction: TfpgDropAction): TfpgDropAction;
var
  win: TWindow;
begin
  if FDragging then
    Result := daIgnore
  else
  begin
    FDragging := True;
    xia_plain_text := XInternAtom(xapplication.Display, 'text/plain', TBool(False));
    FProposedAction := xapplication.GetAtomFromDropAction(ADefaultAction);
    xapplication.Drag := self;

    XSetSelectionOwner(xapplication.Display, xapplication.XdndSelection, FSource.WinHandle, CurrentTime);
    win := XGetSelectionOwner(xapplication.Display, xapplication.XdndSelection);
    if win <> FSource.WinHandle then
      raise Exception.Create('fpGUI/X11: Application failed to aquire selection owner status');

    InitializeMimeTypesToAtoms;
    if FMimeData.Count > 3 then
      SetTypeListProperty;
  end;
end;

{ TfpgX11SystemTrayHandler }

function TfpgX11SystemTrayHandler.GetTrayIconParent: TWindow;
begin
  if FTrayIconParent = None then
    FTrayIconParent := GetSysTrayWindow;
  Result := FTrayIconParent;
end;

function TfpgX11SystemTrayHandler.GetSysTrayWindow: TWindow;
var
  buf: array[0..32] of char;
  selection_atom: TAtom;
begin
  XGrabServer(xapplication.Display);

  buf := PChar('_NET_SYSTEM_TRAY_S' + IntToStr(xapplication.DefaultScreen));
  selection_atom := XInternAtom(xapplication.Display, buf, false);
  Result := XGetSelectionOwner(xapplication.Display, selection_atom);

  XUngrabServer(xapplication.Display);
end;

function TfpgX11SystemTrayHandler.Send_Message(dest: TWindow; msg: longword; data1, data2, data3: longword): boolean;
var
  ev: TXEvent;
begin
  FillChar(ev, SizeOf(TXEvent), 0);

  ev.xclient._type := ClientMessage;
  ev.xclient.window := dest;      { sender (tray icon window) }
  ev.xclient.message_type := XInternAtom(xapplication.Display, '_NET_SYSTEM_TRAY_OPCODE', False );
  ev.xclient.format := 32;

  ev.xclient.data.l[0] := CurrentTime;
  ev.xclient.data.l[1] := msg;    { message opcode }
  ev.xclient.data.l[2] := data1;
  ev.xclient.data.l[3] := data2;
  ev.xclient.data.l[4] := data3;

  Result := XSendEvent(xapplication.Display, TrayIconParent, False, NoEventMask, @ev) <> 0;
  XSync(xapplication.Display, False);
end;

procedure TfpgX11SystemTrayHandler.Show;
begin
  Send_Message(TrayIconParent, SYSTEM_TRAY_REQUEST_DOCK, TfpgX11Window(Owner).WinHandle, 0, 0);
end;

procedure TfpgX11SystemTrayHandler.Hide;
begin
  TfpgX11Window(FTrayWidget).DoSetWindowVisible(False);
end;

constructor TfpgX11SystemTrayHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTrayWidget := AOwner as TfpgWindowBase;
  FTrayIconParent := None;
end;

function TfpgX11SystemTrayHandler.IsSystemTrayAvailable: boolean;
begin
  Result := GetSysTrayWindow <> None;
end;

function TfpgX11SystemTrayHandler.SupportsMessages: boolean;
begin
  Result := True;
end;


initialization
  xapplication := nil;

end.

