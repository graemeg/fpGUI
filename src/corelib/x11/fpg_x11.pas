{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2009 See the file AUTHORS.txt, included in this
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

{.$Define DEBUG}

{ TODO : Compiz effects: Menu popup with correct window hint. Same for Combo dropdown window. }

interface

uses
  Classes,
  SysUtils,
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


  TXIC = record
    dummy: Pointer;
  end;
  PXIC = ^TXIC;


  TXIM = record
    dummy: Pointer;
  end;
  PXIM = ^TXIM;


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
// bit definitions for MwmHints.functions */
  MWM_FUNC_ALL            = 1 shl 0;
  MWM_FUNC_RESIZE         = 1 shl 1;
  MWM_FUNC_MOVE           = 1 shl 2;
  MWM_FUNC_MINIMIZE       = 1 shl 3;
  MWM_FUNC_MAXIMIZE       = 1 shl 4;
  MWM_FUNC_CLOSE          = 1 shl 5;
// bit definitions for MwmHints.decorations */
  MWM_DECOR_ALL           = 1 shl 0;
  MWM_DECOR_BORDER        = 1 shl 1;
  MWM_DECOR_RESIZEH       = 1 shl 2;
  MWM_DECOR_TITLE         = 1 shl 3;
  MWM_DECOR_MENU          = 1 shl 4;
  MWM_DECOR_MINIMIZE      = 1 shl 5;
  MWM_DECOR_MAXIMIZE      = 1 shl 6;
// bit definitions for MwmHints.inputMode */
  MWM_INPUT_MODELESS                  = 0;
  MWM_INPUT_PRIMARY_APPLICATION_MODAL = 1;
  MWM_INPUT_SYSTEM_MODAL              = 2;
  MWM_INPUT_FULL_APPLICATION_MODAL    = 3;
  PROP_MWM_HINTS_ELEMENTS             = 5;

type

  TXWindowStateFlag = (xwsfMapped);
  TXWindowStateFlags = set of TXWindowStateFlag;
  
  // Returns True if it 'ate' the event
  TX11EventFilter = function(const AEvent: TXEvent): Boolean of object;

  // forward declaration
  TfpgWindowImpl = class;
  

  TfpgFontResourceImpl = class(TfpgFontResourceBase)
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


  TfpgImageImpl = class(TfpgImageBase)
  private
    FXimg: TXImage;
    FXimgmask: TXImage;
    function    XImage: PXImage;
    function    XImageMask: PXImage;
  protected
    procedure   DoFreeImage; override;
    procedure   DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: Pointer); override;
    procedure   DoInitImageMask(awidth, aheight: integer; aimgdata: Pointer); override;
  public
    constructor Create;
  end;


  TfpgCanvasImpl = class(TfpgCanvasBase)
  private
    FDrawing: boolean;
    FDrawWindow: TfpgWindowImpl;
    FBufferPixmap: TfpgDCHandle;
    FDrawHandle: TfpgDCHandle;
    Fgc: TfpgGContext;
    FCurFontRes: TfpgFontResourceImpl;
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
    constructor Create; override;
    destructor  Destroy; override;
  end;


  TfpgWindowImpl = class(TfpgWindowBase)
  protected
    FWinFlags: TXWindowStateFlags;
    FWinHandle: TfpgWinHandle;
    FBackupWinHandle: TfpgWinHandle;  // Used by DestroyNotify & UnmapNotify events
    FModalForWin: TfpgWindowImpl;
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
    property    WinHandle: TfpgWinHandle read FWinHandle;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   ActivateWindow; override;
    procedure   CaptureMouse; override;
    procedure   ReleaseMouse; override;
    procedure   SetFullscreen(AValue: Boolean); override;
  end;


  TfpgApplicationImpl = class(TfpgApplicationBase)
  private
    FComposeBuffer: String[32];
    FComposeStatus: TStatus;
    FEventFilter: TX11EventFilter;
    function    ConvertShiftState(AState: Cardinal): TShiftState;
    function    KeySymToKeycode(KeySym: TKeySym): Word;
    function    StartComposing(const Event: TXEvent): TKeySym;
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
    netlayer: TNETWindowLayer;
    xia_targets: TAtom;
    InputMethod: PXIM;
    InputContext: PXIC;
    FLastKeySym: TKeySym;   // Used for KeyRelease event
    function    DoGetFontFaceList: TStringList; override;
  public
    constructor Create(const AParams: string); override;
    destructor  Destroy; override;
    function    DoMessagesPending: boolean;
    procedure   DoWaitWindowMessage(atimeoutms: integer);
    procedure   DoFlush;
    function    GetScreenWidth: TfpgCoord; override;
    function    GetScreenHeight: TfpgCoord; override;
    function    Screen_dpi_x: integer; override;
    function    Screen_dpi_y: integer; override;
    function    Screen_dpi: integer; override;
    property    Display: PXDisplay read FDisplay;
    property    RootWindow: TfpgWinHandle read FRootWindow;
    property    EventFilter: TX11EventFilter read FEventFilter write FEventFilter;
  end;


  TfpgClipboardImpl = class(TfpgClipboardBase)
  private
    FWaitingForSelection: Boolean;
  protected
    FClipboardText: TfpgString;
    function    DoGetText: TfpgString; override;
    procedure   DoSetText(const AValue: TfpgString); override;
    procedure   InitClipboard; override;
  end;
  
  
  TfpgFileListImpl = class(TfpgFileListBase)
    function    EncodeModeString(FileMode: longword): TFileModeString;
    constructor Create; override;
    function    InitializeEntry(sr: TSearchRec): TFileEntry; override;
    procedure   PopulateSpecialDirs(const aDirectory: TfpgString); override;
  end;


function fpgColorToX(col: TfpgColor): longword;


implementation

uses
  baseunix,
  users,  { for *nix user and group name support }
  fpg_main,
  fpg_widget,
  fpg_popupwindow,
  fpg_stringutils,  // used for GetTextWidth
  fpg_form,         // for modal event support
  cursorfont,
  xatom;            // used for XA_WM_NAME

var
  xapplication: TfpgApplication;


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
    Result   := c
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
    w: TfpgWindowImpl;
    Next: PWindowLookupRec;
  end;

var
  FirstWindowLookupRec: PWindowLookupRec;
  LastWindowLookupRec: PWindowLookupRec;

procedure AddWindowLookup(w: TfpgWindowImpl);
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

procedure RemoveWindowLookup(w: TfpgWindowImpl);
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

function FindWindowByHandle(wh: TfpgWinHandle): TfpgWindowImpl;
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
  writeln('GFX/X11: FindWindowByHandle failed to find <', IntToHex(wh, 9), '>');
  {$ENDIF}
  Result := nil;
end;

function FindWindowByBackupHandle(wh: TfpgWinHandle): TfpgWindowImpl;
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
  writeln('GFX/X11: FindWindowByBackupHandle failed to find <', IntToHex(wh, 9), '>');
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
  actual: TAtom;
  format: integer;
  count, remaining: longword;
  data: PChar;
begin
  if ev.xselection._property > 0 then
  begin
    XGetWindowProperty(xapplication.Display, ev.xselection.requestor,
        ev.xselection._property, 0, 16000,
        TBool(false), // delete
        0, // type
        @actual, @format, @count, @remaining,
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
  e.selection   := xapplication.xia_clipboard;
  e.target      := ev.xselectionrequest.target;
  e.time        := ev.xselectionrequest.time;
  e._property   := ev.xselectionrequest._property;

  if e.target = xapplication.xia_targets then
  begin
    a := XA_STRING;
    XChangeProperty(xapplication.Display, e.requestor, e._property,
		      XA_ATOM, sizeof(TAtom)*8, 0, PByte(@a), sizeof(TAtom));
  end
  else
  begin
    XChangeProperty(xapplication.Display, e.requestor, e._property, e.target,
        8, 0, PByte(@fpgClipboard.FClipboardText[1]), Length(fpgClipboard.FClipboardText));
  end;

  XSendEvent(xapplication.Display, e.requestor, false, 0, @e );
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


{ TfpgApplicationImpl }

function TfpgApplicationImpl.ConvertShiftState(AState: Cardinal): TShiftState;
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
  if (AState and Mod2Mask) <> 0 then
    Include(Result, ssNum);
  if (AState and Mod4Mask) <> 0 then
    Include(Result, ssSuper);
  if (AState and Mod5Mask) <> 0 then
    Include(Result, ssScroll);
  if (AState and (1 shl 13)) <> 0 then
    Include(Result, ssAltGr);
end;

function TfpgApplicationImpl.KeySymToKeycode(KeySym: TKeySym): Word;
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
      Result := KeySym - 32;
    $20a0..$20ac: Result := Table_20aX[KeySym];
    $fe20: Result := keyTab;
    $fe50..$fe60: Result := Table_feXX[KeySym];
    $ff08: Result := keyBackspace;
    $ff09: Result := keyTab;
    $ff0a: Result := keyLinefeed;
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

function TfpgApplicationImpl.StartComposing(const Event: TXEvent): TKeySym;
var
  l: integer;
begin
  // Xutf8LookupString returns the size of FComposeBuffer in bytes.
  l := Xutf8LookupString(InputContext, @Event.xkey, @FComposeBuffer[1],
        SizeOf(FComposeBuffer) - 1, @Result, @FComposeStatus);
  SetLength(FComposeBuffer, l);
end;

function TfpgApplicationImpl.DoGetFontFaceList: TStringList;
var
  pfs: PFcFontSet;
  ppat: PPFcPattern;
  n: integer;
  s: string;
  pc: PChar;
begin
  pfs := XftListFonts(Display, DefaultScreen, [FC_SCALABLE, FcTypeBool, 1, 0, FC_FAMILY, 0]);

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

constructor TfpgApplicationImpl.Create(const AParams: string);
begin
  inherited Create(AParams);
  FIsInitialized    := False;
  FDisplay          := XOpenDisplay(PChar(aparams));

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
  xia_clipboard         := XInternAtom(FDisplay, 'CLIPBOARD', longbool(0));
  xia_targets           := XInternAtom(FDisplay, 'TARGETS', longbool(0));
  xia_motif_wm_hints    := XInternAtom(FDisplay, '_MOTIF_WM_HINTS', longbool(0));
  xia_wm_protocols      := XInternAtom(FDisplay, 'WM_PROTOCOLS', longbool(0));
  xia_wm_delete_window  := XInternAtom(FDisplay, 'WM_DELETE_WINDOW', longbool(0));

  netlayer := TNETWindowLayer.Create(FDisplay);

  // for correct keyboard handling
  InputMethod := XOpenIM(FDisplay, nil, nil, nil);
  if InputMethod = nil then
    Exit;

  InputContext := XCreateIC(InputMethod, [XNInputStyle, XIMPreeditNothing or XIMStatusNothing, 0]);
  if InputContext = nil then
    Exit;

  FIsInitialized := True;
  xapplication := TfpgApplication(self);
end;

destructor TfpgApplicationImpl.Destroy;
begin
  netlayer.Free;
  XCloseDisplay(FDisplay);
  
  inherited Destroy;
end;

function TfpgApplicationImpl.DoMessagesPending: boolean;
begin
  Result := (XPending(display) > 0);
end;

function GetParentWindow(wh: TfpgWinHandle; var pw, rw: TfpgWinHandle): boolean;
var
  rootw: TfpgWinHandle;
  parentw: TfpgWinHandle;
  childs: ^TfpgWinHandle;
  cnum: longword;
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

procedure TfpgApplicationImpl.DoWaitWindowMessage(atimeoutms: integer);
var
  ev: TXEvent;
  NewEvent: TXevent;
  i: integer;
  r: integer;
  blockmsg: boolean;
  w: TfpgWindowImpl;
  ew: TfpgWindowImpl;
  kwg: TfpgWidget;
  wh: TfpgWinHandle;
  wa: TXWindowAttributes;
  mcode: integer;
  msgp: TfpgMessageParams;
  rfds: baseunix.TFDSet;
  xfd: integer;
  KeySym: TKeySym;
  Popup: TfpgWidget;
  status: TStatus;
  needToWait: boolean;

  // debug purposes only
  procedure PrintKeyEvent(const event: TXEvent);
  var
    keysym: TKeySym;
    compose_status: TXComposeStatus;
    length: integer;
    s: string[10];
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
    length := Xutf8LookupString(InputContext, @event.xkey, @s[1], 9, @keysym, @compose_status);
    SetLength(s, length);
    if((length > 0) and (length <=9)) then
      writeln('result of xlookupstring [' + s + ']');
    writeln(Format('*** keysym [%s] ', [XKeysymToString(keysym)]));
  end;

  // Debug info only
  procedure ReportLostWindow(const event: TXEvent);
  begin
    {$IFDEF DEBUG}
    writeln('fpGFX/X11: ', GetXEventName(event._type), ' can''t find <',
        IntToHex(event.xany.window, 9), '>');
    {$ENDIF}
  end;
  
begin
  xfd := XConnectionNumber(display);
  XFlush(display);
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
                w := TfpgWindowImpl(w.Parent);

              if (w <> nil) and (PopupListFind(w.WinHandle) = nil) and
                 (not PopupDontCloseWidget(TfpgWidget(ew))) then
              begin
                ClosePopups;
              end;
            end;
          end;

          w := FindWindowByHandle(ev.xbutton.window); // restore w
          if xapplication.TopModalForm <> nil then
          begin
            ew := TfpgWindowImpl(WidgetParentForm(TfpgWidget(w)));
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

    X.MotionNotify:
        begin
          repeat
            //
          until not XCheckTypedWindowEvent(display, ev.xmotion.window, X.MotionNotify, @ev);
          w := FindWindowByHandle(ev.xmotion.window);
          if not Assigned(w) then
            ReportLostWindow(ev);
          if xapplication.TopModalForm <> nil then
          begin
            ew := TfpgWindowImpl(WidgetParentForm(TfpgWidget(w)));
            if (ew <> nil) and (xapplication.TopModalForm <> ew) and (waUnblockableMessages in ew.WindowAttributes = False) then
              blockmsg := true;
          end;
          if not blockmsg then
          begin
            msgp.mouse.x          := ev.xmotion.x;
            msgp.mouse.y          := ev.xmotion.y;
            msgp.mouse.Buttons    := (ev.xmotion.state and $FF00) shr 8;
            msgp.mouse.shiftstate := ConvertShiftState(ev.xmotion.state);
            fpgPostMessage(nil, FindWindowByHandle(ev.xmotion.window), FPGM_MOUSEMOVE, msgp);
          end;
        end;

    // message blockings for modal windows
    X.ClientMessage:
        begin
          w := FindWindowByBackupHandle(ev.xclient.window);
          if not Assigned(w) then
            ReportLostWindow(ev);

          // WM_PROTOCOLS message
          if Assigned(w) and (ev.xclient.message_type = xia_wm_protocols) then
          begin
            //WriteLn(XGetAtomName(FDisplay, TAtom(ev.xclient.data.l[0])));
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
                ew := TfpgWindowImpl(WidgetParentForm(TfpgWidget(w)));
                if (ew <> nil) and (xapplication.TopModalForm <> ew) and (waUnblockableMessages in ew.WindowAttributes = False) then
                  blockmsg := true;
              end;
          
              if not blockmsg then
                fpgPostMessage(nil, FindWindowByHandle(ev.xclient.window), FPGM_CLOSE);
             end;
          end; // WM_PROTOCOLS
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
          ProcessSelection(ev);
        end;

    X.SelectionRequest:
        begin
          ProcessSelectionRequest(ev);
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
            RemoveWindowLookup(TfpgWindowImpl(w));
        end;

    X.GraphicsExpose,
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
      WriteLn('fpGFX/X11: Unhandled X11 event received: ', GetXEventName(ev._type));
  end;
end;

procedure TfpgApplicationImpl.DoFlush;
begin
  XFlush(FDisplay);
end;

function TfpgApplicationImpl.GetScreenWidth: TfpgCoord;
var
  wa: TXWindowAttributes;
begin
  XGetWindowAttributes(FDisplay, RootWindow, @wa);
  Result := wa.Width;
end;

function TfpgApplicationImpl.GetScreenHeight: TfpgCoord;
var
  wa: TXWindowAttributes;
begin
  XGetWindowAttributes(FDisplay, RootWindow, @wa);
  Result := wa.Height;
end;

function TfpgApplicationImpl.Screen_dpi_x: integer;
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

function TfpgApplicationImpl.Screen_dpi_y: integer;
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

function TfpgApplicationImpl.Screen_dpi: integer;
begin
  Result := Screen_dpi_y;
  {$IFDEF DEBUG}
  writeln('Display width in mm: ', DisplayWidthMM(Display, DefaultScreen));
  writeln('Display height in mm: ', DisplayHeightMM(Display, DefaultScreen));
  writeln('Display dpi: ', Result);
  {$ENDIF}
end;

{ TfpgWindowImpl }

procedure TfpgWindowImpl.DoAllocateWindowHandle(AParent: TfpgWindowBase);
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
    pwh := TfpgWindowImpl(AParent).WinHandle
  else
    pwh := xapplication.RootWindow;

  FillChar(attr, sizeof(attr), 0);
  mask := 0;
  if (FWindowType in [wtPopup]) or (waX11SkipWMHints in FWindowAttributes) then
  begin
    attr.Override_Redirect := TBool(True);
    mask := CWOverrideRedirect;
  end;

  AdjustWindowStyle;
  wh := XCreateWindow(xapplication.Display, pwh,
    FLeft, FTop, FWidth, FHeight, 0,
    CopyFromParent,
    InputOutput,
    xapplication.DefaultVisual,
    mask, @attr);

  FWinHandle := wh;
  FBackupWinHandle := wh;
  
  // so newish window manager can close unresponsive programs
  if AParent = nil then // is a toplevel window
  begin
    IconPixMap := XCreateBitmapFromData(fpgApplication.Display, FWinHandle,
      @IconBitmapBits, IconBitmapWidth, IconBitmapHeight);

    WMHints := XAllocWMHints;
    WMHints^.icon_pixmap := IconPixmap;
    WMHints^.flags := IconPixmapHint;

//    XSetWMProperties(fpgApplication.Display, FWinHandle, nil, nil, nil, 0, nil, nil, nil);
    XSetWMProperties(fpgApplication.Display, FWinHandle, nil, nil, nil, 0, nil, WMHints, nil);
    fpgApplication.netlayer.WindowSetPID(FWinHandle, GetProcessID);
    fpgApplication.netlayer.WindowSetSupportPING(FWinHandle);

    XFree(WMHints);
  end;

  hints.flags := 0;

  if not (waAutoPos in FWindowAttributes) then
    hints.flags := hints.flags or PPosition;

  if waScreenCenterPos in FWindowAttributes then
  begin
    hints.flags := hints.flags or PPosition;

    FLeft := (xapplication.ScreenWidth - FWidth) div 2;
    FTop  := (xapplication.ScreenHeight - FHeight) div 2;
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
        lmwh := TfpgWindowImpl(fpgApplication.PrevModalForm).WinHandle
      else if fpgApplication.MainForm <> nil then
        lmwh := TfpgWindowImpl(fpgApplication.MainForm).WinHandle;
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
    prop := XInternAtom(xapplication.display, '_MOTIF_WM_INFO', longbool(0));
    if prop = X.None then
    begin
      writeln('Window Manager does not support MWM hints.  Bypassing window manager control for borderless window.');
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

procedure TfpgWindowImpl.DoReleaseWindowHandle;
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

procedure TfpgWindowImpl.DoRemoveWindowLookup;
begin
//  PrintCallTraceDbgLn('RemoveWindowLookup ' + Name + ' [' + Classname + ']');
  RemoveWindowLookup(self);
end;

procedure TfpgWindowImpl.DoSetWindowVisible(const AValue: Boolean);
begin
  if AValue then
  begin
    if not HandleIsValid then
      AllocateWindowHandle;
    XMapWindow(xapplication.Display, FWinHandle);
    Include(FWinFlags, xwsfMapped);
    // Fullscreen can only be set visible (mapped) windows.
    if waFullScreen in FWindowAttributes then
      fpgApplication.netlayer.WindowSetFullscreen(FWinHandle, True);
  end
  else
  begin
    if HandleIsValid and (xwsfMapped in FWinFlags) then
      XUnmapWindow(xapplication.Display, FWinHandle);
  end;
end;

function TfpgWindowImpl.HandleIsValid: boolean;
begin
  Result := (FWinHandle > 0);
end;

procedure TfpgWindowImpl.DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord);
begin
  if HandleIsValid then
    XMoveWindow(xapplication.display, FWinHandle, x, y);
end;

function TfpgWindowImpl.DoWindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint;
var
  dx: integer;
  dy: integer;
  cw: TfpgWinHandle;
begin
  if not TfpgWindowImpl(ASource).HandleIsValid then
    Exit; //==>
    
  XTranslateCoordinates(xapplication.display, TfpgWindowImpl(ASource).WinHandle,
      XDefaultRootWindow(xapplication.display), AScreenPos.X, AScreenPos.Y, @dx, @dy, @cw);

  Result.X := dx;
  Result.Y := dy;
end;

procedure TfpgWindowImpl.DoUpdateWindowPosition;
var
  w: longword;
  h: longword;
begin
  if FWidth > 1 then
    w := FWidth
  else
    w := 1;
  if FHeight > 1 then
    h := FHeight
  else
    h := 1;

  if FWinHandle > 0 then
    XMoveResizeWindow(xapplication.display, FWinHandle, FLeft, FTop, w, h);
end;

procedure TfpgWindowImpl.DoSetMouseCursor;
var
  xc: TCursor;
  shape: integer;
begin
  if not HasHandle then
    Exit; //==>
    
  case FMouseCursor of
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
  else
    shape := XC_left_ptr; //XC_arrow;
  end;

  xc := XCreateFontCursor(xapplication.Display, shape);
  XDefineCursor(xapplication.Display, FWinHandle, xc);
  XFreeCursor(xapplication.Display, xc);
end;

procedure TfpgWindowImpl.DoSetWindowTitle(const ATitle: string);
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

constructor TfpgWindowImpl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWinHandle := 0;
  FBackupWinHandle := 0;
end;

procedure TfpgWindowImpl.ActivateWindow;
begin
  XSetInputFocus(xapplication.Display, FWinHandle, RevertToParent, CurrentTime);
end;

procedure TfpgWindowImpl.CaptureMouse;
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

procedure TfpgWindowImpl.ReleaseMouse;
begin
  XUngrabPointer(xapplication.display, CurrentTime);
end;

procedure TfpgWindowImpl.SetFullscreen(AValue: Boolean);
begin
  inherited SetFullscreen(AValue);
  fpgApplication.netlayer.WindowSetFullscreen(FWinHandle, AValue);
end;

{ TfpgFontResourceImpl }

function TfpgFontResourceImpl.DoGetTextWidthClassic(const txt: string): integer;
var
  extents: TXGlyphInfo;
begin
  XftTextExtentsUTF8(xapplication.display, FFontData, PChar(txt), Length(txt), extents);
  Result := extents.xOff;
end;

function TfpgFontResourceImpl.DoGetTextWidthWorkaround(const txt: string): integer;
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

constructor TfpgFontResourceImpl.Create(const afontdesc: string);
begin
  FFontData := XftFontOpenName(xapplication.display, xapplication.DefaultScreen, PChar(afontdesc));
end;

destructor TfpgFontResourceImpl.Destroy;
begin
  if HandleIsValid then
    XftFontClose(xapplication.Display, FFontData);
  inherited;
end;

function TfpgFontResourceImpl.HandleIsValid: boolean;
begin
  Result := (FFontData <> nil);
end;

function TfpgFontResourceImpl.GetAscent: integer;
begin
  Result := FFontData^.ascent;
end;

function TfpgFontResourceImpl.GetDescent: integer;
begin
  Result := FFontData^.descent;
end;

function TfpgFontResourceImpl.GetHeight: integer;
begin
  // Do NOT use FFontData^.height as it isn't as accurate
  Result := GetAscent + GetDescent;
end;

function TfpgFontResourceImpl.GetTextWidth(const txt: string): integer;
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

{ TfpgCanvasImpl }

constructor TfpgCanvasImpl.Create;
begin
  inherited;
  FDrawing    := False;
  FDrawWindow := nil;

  FBufferPixmap := 0;
  FDrawHandle   := 0;
  Fgc           := nil;
  FXftDraw      := nil;
  FClipRegion   := nil;
end;

destructor TfpgCanvasImpl.Destroy;
begin
  if FDrawing then
    DoEndDraw;
  FreeAndNil(FBufferFreeTimer);
  TryFreePixmap;
  inherited Destroy;
end;

procedure TfpgCanvasImpl.DoBeginDraw(awin: TfpgWindowBase; buffered: boolean);
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
  if Assigned(TfpgWindowImpl(awin)) then
  begin
    // This occurs every now and again with TfpgMemo and InvertCaret painting!
    // Investigate this.
    if not TfpgWindowImpl(awin).HasHandle then
      raise Exception.Create('Window doesn''t have a Handle');
  end;
  
  XGetGeometry(xapplication.display, TfpgWindowImpl(awin).FWinHandle, @rw, @x, @y, @w, @h, @bw, @d);

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
    FDrawWindow := TfpgWindowImpl(awin);

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
        // Rapid paint events reuse the double buffer which resests a delay
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

procedure TfpgCanvasImpl.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
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

procedure TfpgCanvasImpl.DoEndDraw;
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

function TfpgCanvasImpl.GetPixel(X, Y: integer): TfpgColor;
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

procedure TfpgCanvasImpl.SetPixel(X, Y: integer; const AValue: TfpgColor);
var
  oldColor: TfpgColor;
begin
  oldColor := Color;
  SetColor(AValue);
  XDrawPoint(xapplication.display, FDrawHandle, Fgc, X, Y);
  SetColor(oldColor);
end;

procedure TfpgCanvasImpl.DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
begin
  XDrawArc(xapplication.display, FDrawHandle, Fgc, x, y, w-1, h-1,
      Trunc(64 * a1), Trunc(64 * a2));
end;

procedure TfpgCanvasImpl.DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
begin
  XFillArc(xapplication.display, FDrawHandle, Fgc, x, y, w, h,
      Trunc(64 * a1), Trunc(64 * a2));
end;

procedure TfpgCanvasImpl.DoDrawPolygon(Points: fpg_base.PPoint; NumPts: Integer; Winding: boolean);
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

procedure TfpgCanvasImpl.BufferFreeTimer(Sender: TObject);
begin
  {$IFDEF DEBUG}
  WriteLn('fpGFX/X11: Freeing Buffer w=', FPixWidth, ' h=', FPixHeight);
  {$ENDIF}
  TryFreePixmap;
  FreeAndNil(FBufferFreeTimer);
end;

procedure TfpgCanvasImpl.TryFreePixmap;
begin
  if FBufferPixmap > 0 then
    XFreePixmap(xapplication.Display, FBufferPixmap);
  FBufferPixmap := 0;
end;

procedure TfpgCanvasImpl.DoSetFontRes(fntres: TfpgFontResourceBase);
begin
  if fntres = nil then
    Exit; //==>
  FCurFontRes := TfpgFontResourceImpl(fntres);
end;

procedure TfpgCanvasImpl.DoSetTextColor(cl: TfpgColor);
begin
  SetXftColor(cl, FColorTextXft);
end;

procedure TfpgCanvasImpl.DoSetColor(cl: TfpgColor);
begin
  XSetForeGround(xapplication.display, Fgc, fpgColorToX(cl));
end;

procedure TfpgCanvasImpl.DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle);
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

procedure TfpgCanvasImpl.DoDrawString(x, y: TfpgCoord; const txt: string);
begin
  if Length(txt) < 1 then
    Exit; //==>

  XftDrawStringUTF8(FXftDraw, FColorTextXft, FCurFontRes.Handle, x,
    y + FCurFontRes.GetAscent, PChar(txt), Length(txt));
end;

procedure TfpgCanvasImpl.DoGetWinRect(out r: TfpgRect);
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

procedure TfpgCanvasImpl.DoFillRectangle(x, y, w, h: TfpgCoord);
begin
  XFillRectangle(xapplication.display, FDrawHandle, Fgc, x, y, w, h);
end;

procedure TfpgCanvasImpl.DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord);
begin
  XSetForeGround(xapplication.display, Fgc, fpgColorToX(fpgColorToRGB(col)));
  XSetFunction(xapplication.display, Fgc, GXxor);
  XFillRectangle(xapplication.display, FDrawHandle, Fgc, x, y, w, h);
  XSetForeGround(xapplication.display, Fgc, 0);
  XSetFunction(xapplication.display, Fgc, GXcopy);
end;

procedure TfpgCanvasImpl.DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord);
var
  pts: array[1..3] of TXPoint;
begin
  pts[1].x := x1;   pts[1].y := y1;
  pts[2].x := x2;   pts[2].y := y2;
  pts[3].x := x3;   pts[3].y := y3;

  XFillPolygon(xapplication.display, FDrawHandle, Fgc, @pts, 3, CoordModeOrigin, X.Complex);
end;

procedure TfpgCanvasImpl.DoDrawRectangle(x, y, w, h: TfpgCoord);
begin
//  writeln(Format('DoDrawRectangle  x=%d y=%d w=%d h=%d', [x, y, w, h]));
  // Same behavior as Windows. See documentation for reason.
  XDrawRectangle(xapplication.display, FDrawHandle, Fgc, x, y, w-1, h-1);
end;

procedure TfpgCanvasImpl.DoDrawLine(x1, y1, x2, y2: TfpgCoord);
begin
  // Same behavior as Windows. See documentation for reason.
  XDrawLine(xapplication.display, FDrawHandle, Fgc, x1, y1, x2, y2);
end;

procedure TfpgCanvasImpl.DoSetClipRect(const ARect: TfpgRect);
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

function TfpgCanvasImpl.DoGetClipRect: TfpgRect;
begin
  Result := FClipRect;
end;

procedure TfpgCanvasImpl.DoAddClipRect(const ARect: TfpgRect);
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

procedure TfpgCanvasImpl.DoClearClipRect;
var
  r: TfpgRect;
begin
  DoGetWinRect(r);
  DoSetClipRect(r);
  FClipRectSet := False;
end;

procedure TfpgCanvasImpl.DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer);
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
    XPutImage(xapplication.display, msk, gc2, TfpgImageImpl(img).XImageMask, xi, yi, 0, 0, w, h);

    drawgc := XCreateGc(xapplication.display, FDrawHandle, 0, @GcValues);
    XSetClipMask(xapplication.display, drawgc, msk);
    XSetClipOrigin(xapplication.display, drawgc, x, y);

    XPutImage(xapplication.display, FDrawHandle, drawgc, TfpgImage(img).XImage, xi, yi, x, y, w, h);
    XFreePixmap(xapplication.display, msk);
    XFreeGc(xapplication.display, drawgc);
    XFreeGc(xapplication.display, gc2);
  end
  else
    XPutImage(xapplication.display, FDrawHandle, Fgc, TfpgImage(img).XImage, xi, yi, x, y, w, h);
end;

{ TfpgImageImpl }

constructor TfpgImageImpl.Create;
begin
  inherited Create;
end;

procedure TfpgImageImpl.DoFreeImage;
begin
  // does nothing on X11
end;

procedure TfpgImageImpl.DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: Pointer);
begin
  FMasked := False;

  with FXimg do
  begin
    Width          := awidth;
    Height         := aheight;
    xoffset        := 0;
    obdata         := #0;
    byte_order     := LSBFirst;
    bitmap_bit_order := MSBFirst;
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
      depth          := xapplication.DisplayDepth; //  acolordepth;
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

procedure TfpgImageImpl.DoInitImageMask(awidth, aheight: integer; aimgdata: Pointer);
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

function TfpgImageImpl.XImage: PXImage;
begin
  Result := @FXimg;
end;

function TfpgImageImpl.XImageMask: PXImage;
begin
  Result := @FXimgMask;
end;

{ TfpgClipboardImpl }

function TfpgClipboardImpl.DoGetText: TfpgString;
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

procedure TfpgClipboardImpl.DoSetText(const AValue: TfpgString);
begin
  FClipboardText := AValue;
  XSetSelectionOwner(xapplication.Display, xapplication.xia_clipboard,
      FClipboardWndHandle, 0);
end;

procedure TfpgClipboardImpl.InitClipboard;
begin
  FWaitingForSelection := False;
  FClipboardWndHandle := XCreateSimpleWindow(xapplication.Display,
      xapplication.RootWindow, 10, 10, 10, 10, 0, 0, 0);
end;

{ TfpgFileListImpl }

function TfpgFileListImpl.EncodeModeString(FileMode: longword): TFileModeString;
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

constructor TfpgFileListImpl.Create;
begin
  inherited Create;
  FHasFileMode := true;
end;

function TfpgFileListImpl.InitializeEntry(sr: TSearchRec): TFileEntry;
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
    {Result.GroupID     := info.st_gid;
    Result.OwnerID      := info.st_uid;}
    Result.Owner        := GetUserName(TUID(info.st_uid));
    Result.Group        := GetGroupName(TGID(info.st_uid));
  end;
end;

procedure TfpgFileListImpl.PopulateSpecialDirs(const aDirectory: TfpgString);
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


initialization
  xapplication := nil;

end.

