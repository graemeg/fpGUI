unit gfx_x11;

{$mode objfpc}{$H+}

{.$Define DEBUG}

interface

uses
  Classes,
  SysUtils,
  X,
  Xlib,
  XUtil,
  x11_xft,
//  x11_keyconv,
  gfxbase;

type
  TfpgWinHandle = TXID;
  TfpgGContext  = Xlib.TGc;

type
  PInt = ^integer;

  TXIC = record
    dummy: Pointer;
  end;
  PXIC = ^TXIC;

  TXIM = record
    dummy: Pointer;
  end;
  PXIM = ^TXIM;

  PXdbeSwapInfo = ^TXdbeSwapInfo;

  TXdbeSwapInfo = record
    Window: TfpgWinHandle;
    SwapAction: PChar;
  end;

type

  TXWindowStateFlag = (xwsfMapped);
  
  TXWindowStateFlags = set of TXWindowStateFlag;
  
  TfpgWindowImpl = class;
  

  TfpgFontResourceImpl = class(TfpgFontResourceBase)
  private
    FFontData: PXftFont;
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
    FBufferPixmap: TPixmap;
    FDrawHandle: TXID;
    Fgc: TfpgGContext;
    FCurFontRes: TfpgFontResourceImpl;
    FClipRect: TRect;
    FClipRectSet: boolean;
    FXftDraw: PXftDraw;
    FXftDrawBuffer: PXftDraw;
    FColorTextXft: TXftColor;
    FClipRegion: TRegion;
  protected
    procedure   DoSetFontRes(fntres: TfpgFontResourceBase); override;
    procedure   DoSetTextColor(cl: TfpgColor); override;
    procedure   DoSetColor(cl: TfpgColor); override;
    procedure   DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle); override;
    procedure   DoGetWinRect(out r: TRect); override;
    procedure   DoFillRectangle(x, y, w, h: integer); override;
    procedure   DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord); override;
    procedure   DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord); override;
    procedure   DoDrawRectangle(x, y, w, h: integer); override;
    procedure   DoDrawLine(x1, y1, x2, y2: integer); override;
    procedure   DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer); override;
    procedure   DoDrawString(x, y: TfpgCoord; const txt: string); override;
    procedure   DoSetClipRect(const ARect: TRect); override;
    function    DoGetClipRect: TRect; override;
    procedure   DoAddClipRect(const ARect: TRect); override;
    procedure   DoClearClipRect; override;
    procedure   DoBeginDraw(awin: TfpgWindowBase; buffered: boolean); override;
    procedure   DoPutBufferToScreen(x, y, w, h: TfpgCoord); override;
    procedure   DoEndDraw; override;
    function    GetPixel(X, Y: integer): TfpgColor; override;
    procedure   SetPixel(X, Y: integer; const AValue: TfpgColor); override;
    procedure   DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: Extended); override;
    procedure   DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
  end;


  { TfpgWindowImpl }

  TfpgWindowImpl = class(TfpgWindowBase)
  protected
    FWinFlags : TXWindowStateFlags;
    FWinHandle: TfpgWinHandle;
    FModalForWin: TfpgWindowImpl;
    procedure   DoAllocateWindowHandle(AParent: TfpgWindowBase); override;
    procedure   DoReleaseWindowHandle; override;
    procedure   DoSetWindowVisible(const AValue: Boolean); override;
    function    HandleIsValid: boolean; override;
    procedure   DoSetWindowTitle(const ATitle: string); override;
    procedure   DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord); override;
    function    DoWindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint; override;
    procedure   DoUpdateWindowPosition(aleft, atop, awidth, aheight: TfpgCoord); override;
    property    WinHandle: TfpgWinHandle read FWinHandle;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   CaptureMouse; override;
    procedure   ReleaseMouse; override;
  end;


  TfpgApplicationImpl = class(TfpgApplicationBase)
  private
    FComposeBuffer: String[32];
    FComposeStatus: TXComposeStatus;
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
    RootWindow: TfpgWinHandle;
    xia_clipboard: TAtom;
    xia_motif_wm_hints: TAtom;
    xia_wm_protocols: TAtom;
    xia_wm_delete_window: TAtom;
    xia_wm_state: TAtom;
    xia_wm_state_modal: TAtom;
    xia_targets: TAtom;
    InputMethod: PXIM;
    InputContext: PXIC;
    LastClickWindow: TfpgWinHandle;   // double click generation
    LastWinClickTime: longword;
  public
    constructor Create(const aparams: string); override;
    destructor  Destroy; override;
    function    DoMessagesPending: boolean;
    procedure   DoWaitWindowMessage(atimeoutms: integer);
    procedure   DoFlush;
    function    GetScreenWidth: TfpgCoord;
    function    GetScreenHeight: TfpgCoord;
    property    Display: PXDisplay read FDisplay;
  end;


implementation

uses
  baseunix,
  fpgfx,
  gfx_widget,  {$Note This dependency to gfx_widget must be removed.}
  gui_form, // remove this!!!!!
  xatom,
  gfx_utf8utils,
  _netlayer;

var
  xapplication: TfpgApplication;

const
  // map X11 event types to custom event types
  MSG_SCROLL      = 65;
  MSG_RESIZE      = 66;
  MSG_POPUPCLOSE  = 67;
  MSG_MOVE        = 68;
  MSG_DOUBLECLICK = 69;


 // some externals

// defines:
procedure XRenderSetPictureClipRectangles(disp: PXDisplay; pic: TPicture; xorigin, yorigin: integer; rect: PXRectangle; num: integer); cdecl; external;

// redefines:
function XmbLookupString(p1: PXIC; ev: PXKeyPressedEvent; str: PChar; len: longword; ks: PKeySym; stat: PStatus): longint; cdecl; external;

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
begin
  SetLength(FComposeBuffer,
    XLookupString(@Event, @FComposeBuffer[1],
      SizeOf(FComposeBuffer) - 1, @Result, @FComposeStatus));
end;

constructor TfpgApplicationImpl.Create(const aparams: string);
var
  wa: TXWindowAttributes;
begin
  FIsInitialized    := False;
  FDisplay          := XOpenDisplay(PChar(aparams));
  
  if FDisplay = nil then
    Exit; //==>

  DefaultScreen     := XDefaultScreen(Display);
  RootWindow        := XRootWindow(FDisplay, DefaultScreen);
  DefaultBackground := XBlackPixel(FDisplay, DefaultScreen);
  DefaultForeground := XWhitePixel(FDisplay, DefaultScreen);

  DefaultVisual := XDefaultVisual(FDisplay, DefaultScreen);
  DisplayDepth  := XDefaultDepth(FDisplay, DefaultScreen);

  //Writeln('display depth: ',DisplayDepth);
  DefaultColorMap := XDefaultColorMap(FDisplay, DefaultScreen);

  // Initialize atoms
  xia_clipboard        := XInternAtom(FDisplay, 'CLIPBOARD', longbool(0));
  xia_targets          := XInternAtom(FDisplay, 'TARGETS', longbool(0));
  xia_motif_wm_hints   := XInternAtom(FDisplay, '_MOTIF_WM_HINTS', longbool(0));
  xia_wm_protocols     := XInternAtom(FDisplay, 'WM_PROTOCOLS', longbool(0));
  xia_wm_delete_window := XInternAtom(FDisplay, 'WM_DELETE_WINDOW', longbool(0));
  xia_wm_state       := XInternAtom(FDisplay, '_NET_WM_STATE', longbool(0));
  xia_wm_state_modal := XInternAtom(FDisplay, '_NET_WM_STATE_MODAL', longbool(0));

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
  n: integer;
  i: integer;
  r: integer;
  i2: integer;
  ks: integer;
  uc: word;
  a: array[1..16] of char;
  ss: integer;
  sr: integer;
  p: PChar;
  blockmsg: boolean;
  b: boolean;
  w: TfpgWindowImpl;
  ew: TfpgWindowImpl;
  kwg: TfpgWidget;
  wh: TfpgWinHandle;
  wa: TXWindowAttributes;
  px: integer;
  py: integer;
  mcode: integer;
  msgp: TfpgMessageParams;
  rfds: TFDSet;
  xfd: integer;
  KeySym: TKeySym;

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
    length := XLookupString(@event, @s[1], 9, @keysym, @compose_status);
    SetLength(s, length);
    if((length > 0) and (length <=9)) then
      writeln('result of xlookupstring [' + s + ']');
    writeln(Format('*** keysym [%s] ', [XKeysymToString(keysym)]));
  end;
  
begin
  xfd := XConnectionNumber(display);

  repeat
    if (atimeoutms >= 0) and (XPending(display) <= 0) then
    begin
      // Some event is waiting for the given timeout.
      // This Select handles only the first 256 file descriptors.
      // Poll would be better but FPC has no official poll interface (if I'm right)
      fpFD_ZERO(rfds);
      fpFD_SET(xfd, rfds);
      r := fpSelect(xfd + 1, @rfds, nil, nil, atimeoutms);

      if r <= 0 then
        Exit; // no event received.
    end;
    XNextEvent(display, @ev);
  until (not XFilterEvent(@ev, 0));

  blockmsg := False;
  fillchar(msgp, sizeof(msgp), 0);


  // According to a comment in X.h, the valid event types start with 2!
  if ev._type < 2 then
    exit;

// WriteLn('Event ',GetXEventName(ev._type),': ', ev._type,' window: ', ev.xany.window);
//  PrintKeyEvent(ev);  { debug purposes only }

  case ev._type of
    X.KeyPress,
    X.KeyRelease:
        begin
          KeySym := StartComposing(ev);
          msgp.keyboard.keycode     := KeySymToKeycode(KeySym);
          msgp.keyboard.shiftstate  := ConvertShiftState(ev.xkey.state);

          kwg := FindKeyboardFocus;
          if kwg <> nil then
            w := kwg
          else
            w := FindWindowByHandle(ev.xkey.window);

          //Writeln('XKey event(',ev._type,'):',
            //IntToHex(ev.xkey.keycode,4),' (',ev.xkey.keycode,'), shift=',IntToHex(ev.xkey.state,4));

          if ev._type = X.KeyPress then
          begin
            fpgPostMessage(nil, w, FPGM_KEYPRESS, msgp);

            //Writeln('scancode: ',IntToHex(X11keycodeToScanCode(ev.xkey.keycode),4)
            //  ,' (',X11keycodeToScanCode(ev.xkey.keycode),')');

            // Revision 203 used scancodes and XmbLookupString compared to XLookupString.
            // Maybe in the future we can switch to XmbLookupString again.
            if (ev.xkey.state and (ControlMask or Mod1Mask)) = 0 then
            begin
              for i := 1 to Length(FComposeBuffer) do
              begin
                msgp.keyboard.keychar := FComposeBuffer[i];
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
          if xapplication.TopModalForm <> nil then
          begin
            // This is ugly!!!!!!!!!!!!!!!
            ew := TfpgWindowImpl(WidgetParentForm(TfpgWidget(w)));
            if (ew <> nil) and (xapplication.TopModalForm <> ew) then
              blockmsg := true;
          end;
      
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
                while XCheckTypedWindowEvent(display, ev.xany.window, X.ButtonPress, @NewEvent) do
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
                mcode := FPGM_MOUSEUP
              else
                mcode := FPGM_MOUSEDOWN;
              fpgPostMessage(nil, w, mcode, msgp);
            end;  { if/else }
          end;  { if not blocking }
        end;

    X.Expose:
        begin
          repeat
            //
          until not XCheckTypedWindowEvent(display, ev.xany.window, X.Expose, @ev);
          if ev.xexpose.count = 0 then
          begin
            fpgPostMessage(nil, FindWindowByHandle(ev.xany.window), FPGM_PAINT);
          end;
        end;

    X.MotionNotify:
        begin
          repeat
            //
          until not XCheckTypedWindowEvent(display, ev.xbutton.window, X.MotionNotify, @ev);

          w := FindWindowByHandle(ev.xany.window);
          if xapplication.TopModalForm <> nil then
          begin
            // This is ugly!!!!!!!!!!!!!!!
            ew := TfpgWindowImpl(WidgetParentForm(TfpgWidget(w)));
            if (ew <> nil) and (xapplication.TopModalForm <> ew) then
              blockmsg := true;
          end;

          if not blockmsg then
          begin
            msgp.mouse.x          := ev.xmotion.x;
            msgp.mouse.y          := ev.xmotion.y;
            msgp.mouse.Buttons    := (ev.xmotion.state and $FF00) shr 8;
            msgp.mouse.shiftstate := ConvertShiftState(ev.xmotion.state);
            fpgPostMessage(nil, FindWindowByHandle(ev.xbutton.window), FPGM_MOUSEMOVE, msgp);
          end;
        end;

    // message blockings for modal windows
    X.ClientMessage:
        begin
          w := FindWindowByHandle(ev.xany.window);
          if xapplication.TopModalForm <> nil then
          begin
            // This is ugly!!!!!!!!!!!!!!!
            ew := TfpgWindowImpl(WidgetParentForm(TfpgWidget(w)));
            if (ew <> nil) and (xapplication.TopModalForm <> ew) then
              blockmsg := true;
          end;
          
          if not blockmsg then
            fpgPostMessage(nil, FindWindowByHandle(ev.xany.window), FPGM_CLOSE);
        end;


    X.ConfigureNotify:
        begin
          repeat
            //
          until not XCheckTypedWindowEvent(display, ev.xany.window, ConfigureNotify, @ev);

          msgp.rect.Left   := ev.xconfigure.x;
          msgp.rect.Top    := ev.xconfigure.y;
          msgp.rect.Width  := ev.xconfigure.Width;
          msgp.rect.Height := ev.xconfigure.Height;

          w := FindWindowByHandle(ev.xconfigure.window);
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

{
    X.SelectionNotify:
        begin
          ProcessSelection(ev);
        end;

    X.SelectionRequest:
        begin
          ProcessSelectionRequest(ev);
        end;
}

    X.FocusIn:
        fpgPostMessage(nil, FindWindowByHandle(ev.xany.window), FPGM_ACTIVATE);

    X.FocusOut:
        fpgPostMessage(nil, FindWindowByHandle(ev.xany.window), FPGM_DEACTIVATE);

    X.EnterNotify:
        fpgPostMessage(nil, FindWindowByHandle(ev.xany.window), FPGM_MOUSEENTER);
        
    X.LeaveNotify:
        fpgPostMessage(nil, FindWindowByHandle(ev.xany.window), FPGM_MOUSEEXIT);

    X.MapNotify:
        begin
          w := FindWindowByHandle(ev.xmap.window);
          if w <> nil then begin
            Include(w.FWinFlags, xwsfMapped);
          end;
        end;
    X.UnmapNotify:
        begin
          w := FindWindowByHandle(ev.xunmap.window);
          if w <> nil then begin
            Exclude(w.FWinFlags, xwsfMapped);
          end;
        end;

    { We handle this event manually as well. }
    X.DestroyNotify:
        begin
          //Writeln('DestroyNotify');
          //fpgPostMessage(nil, FindWindowByHandle(ev.xany.window), FPGM_CLOSE);
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
          // We are not interrested in this event
        end;
    else
      {$Note This needs attention. We still have two events slipping by.}
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

{ TfpgWindowImpl }

procedure TfpgWindowImpl.DoAllocateWindowHandle(AParent: TfpgWindowBase);
var
  pwh: TfpgWinHandle;
  wh: TfpgWinHandle;
  attr: TXSetWindowAttributes;
  mask: longword;
  bcolor: longword;
  hints: TXSizeHints;
begin
  if FWinHandle > 0 then
    Exit; //==>

  if aparent <> nil then
    pwh := TfpgWindowImpl(AParent).WinHandle
  else
    pwh := xapplication.RootWindow;

  FillChar(attr, sizeof(attr), 0);
  mask := 0;
  if FWindowType in [wtPopup] then
  begin
    attr.Override_Redirect := longbool(1);
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
    XSetWMProtocols(xapplication.Display, FWinHandle, @(xapplication.xia_wm_delete_window),
      1);// send close event instead of quitting the whole application...

  // for modal windows, this is necessary
  if (FWindowType = wtModalForm) and (AParent <> nil) then
    XSetTransientForHint(xapplication.display, FWinHandle, TfpgWindowImpl(AParent).WinHandle);

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
begin
  if FWinHandle <= 0 then
    Exit;

  RemoveWindowLookup(self);
  XDestroyWindow(xapplication.Display, FWinHandle);

  FWinHandle := 0;
end;

procedure TfpgWindowImpl.DoSetWindowVisible(const AValue: Boolean);
begin
  if AValue then begin
    if not HandleIsValid then AllocateWindowHandle;
    XMapWindow(xapplication.Display, FWinHandle);
    Include(FWinFlags, xwsfMapped);
  end
  else begin
    if HandleIsValid  and (xwsfMapped in FWinFlags) then
      XUnmapWindow(xapplication.Display, FWinHandle);
  end;
end;

function TfpgWindowImpl.HandleIsValid: boolean;
begin
  Result := (FWinHandle > 0);
end;

procedure TfpgWindowImpl.DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord);
begin
  if FWinHandle > 0 then
    XMoveWindow(xapplication.display, FWinHandle, x, y);
end;

function TfpgWindowImpl.DoWindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint;
var
  dx: integer;
  dy: integer;
  cw : TfpgWinHandle;
begin
//  if not HandleIsValid then
//    Exit; //==>
    
  XTranslateCoordinates(xapplication.display, TfpgWindowImpl(ASource).WinHandle,
      XDefaultRootWindow(xapplication.display), AScreenPos.X, AScreenPos.Y, @dx, @dy, @cw);

  Result.X := dx;
  Result.Y := dy;
end;

procedure TfpgWindowImpl.DoUpdateWindowPosition(aleft, atop, awidth, aheight: TfpgCoord);
var
  w: longword;
  h: longword;
begin
  if awidth > 1 then
    w := awidth
  else
    w := 1;
  if aheight > 1 then
    h := aheight
  else
    h := 1;

  if FWinHandle > 0 then
    XMoveResizeWindow(xapplication.display, FWinHandle, aleft, atop, w, h);
end;

procedure TfpgWindowImpl.DoSetWindowTitle(const atitle: string);
var
  //s: string;
  //p: PByte;
  netlayer: TNETWindowLayer;
begin
  if FWinHandle <= 0 then
    Exit;

  //s := atitle;

  //if length(s) > 0 then
    //p := @s[1]
  //else
    //p := nil;

  netlayer := TNETWindowLayer.Create(xapplication.display);
  try
    netlayer.WindowSetName(FWinHandle, PChar(ATitle));
  finally
    netlayer.Free;
  end;
  
//  XChangeProperty(xapplication.display, FWinHandle, 39, 31, 8, 0, p, length(s));
//  XChangeProperty(xapplication.display, FWinHandle, 37, 31, 8, 0, p, length(s));
end;

constructor TfpgWindowImpl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWinHandle := 0;
end;

procedure TfpgWindowImpl.CaptureMouse;
begin
  XGrabPointer(xapplication.Display, FWinHandle,
      True,
      ButtonPressMask or ButtonReleaseMask or ButtonMotionMask or PointerMotionMask,
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

{ TfpgFontResourceImpl }

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
var
  extents: TXGlyphInfo;
begin
  if length(txt) < 1 then
  begin
    Result := 0;
    Exit;
  end;
  XftTextExtentsUTF8(xapplication.display, FFontData, PChar(txt), Length(txt), extents);
  Result := extents.xOff;
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
      FBufferPixmap := XCreatePixmap(xapplication.display, FDrawWindow.FWinHandle, w, h, xapplication.DisplayDepth);
      FDrawHandle   := FBufferPixmap;
    end
    else
    begin
      FBufferPixmap := 0;
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

    if FBufferPixmap > 0 then
      XFreePixmap(xapplication.Display, FBufferPixmap);
    FBufferPixmap := 0;

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
begin
  case AStyle of
    lsDot:
        begin
          XSetLineAttributes(xapplication.display, Fgc, 0,
            LineOnOffDash, CapNotLast, JoinMiter);
          XSetDashes(xapplication.display, Fgc, 0, cDot, 2);
        end;
    lsDash:
        begin
          XSetLineAttributes(xapplication.display, Fgc, 0,
            LineOnOffDash, CapNotLast, JoinMiter);
          XSetDashes(xapplication.display, Fgc, 0, cDash, 2);
        end;
    lsDashDot:
        begin
          XSetLineAttributes(xapplication.display, Fgc, 0,
            LineOnOffDash, CapNotLast, JoinMiter);
          XSetDashes(xapplication.display, Fgc, 0, cDashDot, 4);
        end;
    lsDashDotDot:
        begin
          XSetLineAttributes(xapplication.display, Fgc, 0,
            LineOnOffDash, CapNotLast, JoinMiter);
          XSetDashes(xapplication.display, Fgc, 0, cDashDotDot, 6);
        end;
    else  // which includes lsSolid
      XSetLineAttributes(xapplication.display, Fgc, 0,
        LineSolid, CapNotLast, JoinMiter);
  end;  { case }
end;

procedure TfpgCanvasImpl.DoDrawString(x, y: TfpgCoord; const txt: string);
begin
  if Length(txt) < 1 then
    Exit; //==>

  XftDrawStringUTF8(FXftDraw, FColorTextXft, FCurFontRes.Handle, x,
    y + FCurFontRes.GetAscent, PChar(txt), Length(txt));
end;

procedure TfpgCanvasImpl.DoGetWinRect(out r: TRect);
var
  rw: TfpgWinHandle;
  x: integer;
  y: integer;
  bw: longword;
  d: longword;
  w: Cardinal;
  h: Cardinal;
begin
  XGetGeometry(xapplication.display, FDrawWindow.FWinHandle, @rw, @x, @y,
      @w, @h, @bw, @d);
  r.Left    := 0;
  r.Top     := 0;
  r.Right   := w;
  r.Bottom  := h;
end;

procedure TfpgCanvasImpl.DoFillRectangle(x, y, w, h: integer);
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
  pts[1].x := x1;
  pts[1].y := y1;
  pts[2].x := x2;
  pts[2].y := y2;
  pts[3].x := x3;
  pts[3].y := y3;

  XFillPolygon(xapplication.display, FDrawHandle, Fgc, @pts, 3, 0, 0);
end;

procedure TfpgCanvasImpl.DoDrawRectangle(x, y, w, h: integer);
begin
//  writeln(Format('DoDrawRectangle  x=%d y=%d w=%d h=%d', [x, y, w, h]));
  // Same behavior as Windows. See documentation for reason.
  XDrawRectangle(xapplication.display, FDrawHandle, Fgc, x, y, w-1, h-1);
end;

procedure TfpgCanvasImpl.DoDrawLine(x1, y1, x2, y2: integer);
begin
  // Same behavior as Windows. See documentation for reason.
  XDrawLine(xapplication.display, FDrawHandle, Fgc, x1, y1, x2, y2);
end;

procedure TfpgCanvasImpl.DoSetClipRect(const ARect: TRect);
var
  r: TXRectangle;
  rg: TRegion;
begin
  r.x      := ARect.Left;
  r.y      := ARect.Top;
  r.Width  := ARect.Right - ARect.Left + 1;
  r.Height := ARect.Bottom - ARect.Top + 1;

  rg := XCreateRegion;
  
  XUnionRectWithRegion(@r, rg, FClipRegion);
  XSetRegion(xapplication.display, Fgc, FClipRegion);
  XftDrawSetClip(FXftDraw, FClipRegion);

  FClipRect    := ARect;
  FClipRectSet := True;
  XDestroyRegion(rg);
end;

function TfpgCanvasImpl.DoGetClipRect: TRect;
begin
  Result := FClipRect;
end;

procedure TfpgCanvasImpl.DoAddClipRect(const ARect: TRect);
var
  r: TXRectangle;
  rg: TRegion;
begin
  r.x      := ARect.Left;
  r.y      := ARect.Top;
  r.Width  := ARect.Right - ARect.Left + 1;
  r.Height := ARect.Bottom - ARect.Top + 1;

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
  r: TRect;
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

initialization
  xapplication := nil;

end.

