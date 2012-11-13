{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2012 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This defines the CoreLib backend interface to the Windows GDI API.

      Win32 API Reference - http://msdn.microsoft.com/en-us/library/ff468919(VS.85).aspx
      Windows CE 3.0 API Reference - http://msdn.microsoft.com/en-us/library/ms925466.aspx
      FPC WinCE information - http://wiki.freepascal.org/WinCE_port

    TODO:
      * Refactor out the many $IFDEF CPU64 lines in this unit.
}

unit fpg_gdi;

{$mode objfpc}{$H+}

{.$Define DEBUG}
{.$Define DND_DEBUG}
{.$Define DEBUGKEYS}

interface

uses
  Classes,
  SysUtils,
  Windows,
  ActiveX,
  fpg_base,
  fpg_impl
  {$IFDEF DEBUG}
  ,dbugintf
  {$ENDIF DEBUG}
  ,fpg_OLEDragDrop
  ;

{ Constants missing on windows unit }
const
  VER_PLATFORM_WIN32_CE = 3;
  CLEARTYPE_QUALITY     = 5;

var
  { Unicode selection variables }
  UnicodeEnabledOS: Boolean;
  WinVersion: TOSVersionInfo;
  { Font smoothing type selection variable }
  FontSmoothingType: Cardinal;

type
  // forward declaration
  TfpgGDIWindow = class;
  TGDIDragManager = class;
  TfpgGDIDrag = class;


  TfpgGDIFontResource = class(TfpgFontResourceBase)
  private
    FFontData: HFONT;
    FMetrics: Windows.TEXTMETRIC;
  protected
    function    OpenFontByDesc(const desc: string): HFONT;
    property    Handle: HFONT read FFontData;
  public
    constructor Create(const afontdesc: string);
    destructor  Destroy; override;
    function    HandleIsValid: boolean;
    function    GetAscent: integer; override;
    function    GetDescent: integer; override;
    function    GetHeight: integer; override;
    function    GetTextWidth(const txt: string): integer; override;
  end;


  TfpgGDIImage = class(TfpgImageBase)
  private
    FIsTwoColor: boolean;
    FBMPHandle: HBITMAP;
    FMaskHandle: HBITMAP;
  protected
    property    BMPHandle: HBITMAP read FBMPHandle;
    property    MaskHandle: HBITMAP read FMaskHandle;
    procedure   DoFreeImage; override;
    procedure   DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: Pointer); override;
    procedure   DoInitImageMask(awidth, aheight: integer; aimgdata: Pointer); override;
  public
    constructor Create;
  end;


  TfpgGDICanvas = class(TfpgCanvasBase)
  private
    FDrawing: boolean;
    FBufferBitmap: HBitmap;
    FDrawWindow: TfpgGDIWindow;
    Fgc: TfpgDCHandle;
    FBufgc: TfpgDCHandle;
    FWinGC: TfpgDCHandle;
    FBackgroundColor: TfpgColor;
    FCurFontRes: TfpgGDIFontResource;
    FClipRect: TfpgRect;
    FClipRectSet: Boolean;
    FWindowsColor: longword;
    FBrush: HBRUSH;
    FPen: HPEN;
    FClipRegion: HRGN;
    FIntLineStyle: integer;
    FBufWidth: Integer;
    FBufHeight: Integer;
    procedure   TryFreeBackBuffer;
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
    procedure   DoDrawPolygon(Points: PPoint; NumPts: Integer; Winding: boolean = False); override;
    property    DCHandle: TfpgDCHandle read Fgc;
  public
    constructor Create(awin: TfpgWindowBase); override;
    destructor  Destroy; override;
  end;


  TfpgGDIWindow = class(TfpgWindowBase)
  private
    FDropManager: TfpgOLEDropTarget;
    FDropPos: TPoint;
    FUserMimeSelection: TfpgString;
    FUserAcceptDrag: Boolean;
    function    GetDropManager: TfpgOLEDropTarget;
    procedure   HandleDNDLeave(Sender: TObject);
    procedure   HandleDNDEnter(Sender: TObject; DataObj: IDataObject; KeyState: Longint; PT: TPoint; var Effect: DWORD);
    procedure   HandleDNDPosition(Sender: TObject; KeyState: Longint; PT: TPoint; var Effect: TfpgOLEDragDropEffect);
    procedure   HandleDNDDrop(Sender: TObject; DataObj: IDataObject; KeyState: Longint; PT: TPoint; Effect: TfpgOLEDragDropEffect);
  private
    FMouseInWindow: boolean;
    FNonFullscreenRect: TfpgRect;
    FNonFullscreenStyle: longword;
    FFullscreenIsSet: boolean;
    FSkipResizeMessage: boolean;
    QueueAcceptDrops: boolean;
    function    DoMouseEnterLeaveCheck(AWindow: TfpgGDIWindow; uMsg, wParam, lParam: Cardinal): Boolean;
    procedure   WindowSetFullscreen(aFullScreen, aUpdate: boolean);
    property    DropManager: TfpgOLEDropTarget read GetDropManager;
  protected
    FWinHandle: TfpgWinHandle;
    FModalForWin: TfpgGDIWindow;
    FWinStyle: longword;
    FWinStyleEx: longword;
    FParentWinHandle: TfpgWinHandle;
    procedure   DoAllocateWindowHandle(AParent: TfpgWindowBase); override;
    procedure   DoReleaseWindowHandle; override;
    procedure   DoRemoveWindowLookup; override;
    procedure   DoSetWindowVisible(const AValue: Boolean); override;
    function    HandleIsValid: boolean; override;
    procedure   DoUpdateWindowPosition; override;
    procedure   DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord); override;
    function    DoWindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint; override;
    //procedure MoveToScreenCenter; override;
    procedure   DoSetWindowTitle(const ATitle: string); override;
    procedure   DoSetMouseCursor; override;
    procedure   DoDNDEnabled(const AValue: boolean); override;
    procedure   DoAcceptDrops(const AValue: boolean); override;
    procedure   DoDragStartDetected; override;
    function    GetWindowState: TfpgWindowState; override;
    property    WinHandle: TfpgWinHandle read FWinHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   ActivateWindow; override;
    procedure   CaptureMouse; override;
    procedure   ReleaseMouse; override;
    procedure   SetFullscreen(AValue: Boolean); override;
    procedure   BringToFront; override;
  end;


  TfpgGDIApplication = class(TfpgApplicationBase)
  private
    FDrag: TfpgGDIDrag;
    procedure   DoWakeMainThread(Sender: TObject);
    procedure   SetDrag(const AValue: TfpgGDIDrag);
    property    Drag: TfpgGDIDrag read FDrag write SetDrag;
  protected
    FDisplay: HDC;
    WindowClass: TWndClass;
    WidgetClass: TWndClass;
    hcr_default: HCURSOR;
    hcr_dir_ew: HCURSOR;
    hcr_dir_ns: HCURSOR;
    hcr_edit: HCURSOR;
    hcr_dir_nwse: HCURSOR;
    hcr_dir_nesw: HCURSOR;
//    hcr_dir_senw: HCURSOR;
//    hcr_dir_swne: HCURSOR;
    hcr_move: HCURSOR;
    hcr_crosshair: HCURSOR;
    hcr_wait: HCURSOR;
    hcr_hand: HCURSOR;
    FFocusedWindow: THANDLE;
    { FHiddenWindow serves as parent for modal forms,
      ensuring they don't have taskbar button. It is created
      on-demand and should be accessed via GetHiddenWindow. }
    FHiddenWindow: HWND;
    { To avoid problems, window classes should be accessible
      from RegisterClass call till the program is terminated. }
    HiddenWndClass: TWndClass;
    ActivationHook: HHOOK;
    function    GetHiddenWindow: HWND;
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
    property    Display: HDC read FDisplay;
  end;


  TfpgGDIClipboard = class(TfpgClipboardBase)
  protected
    FClipboardText: TfpgString;
    function    DoGetText: TfpgString; override;
    procedure   DoSetText(const AValue: TfpgString); override;
    procedure   InitClipboard; override;
  end;


  TfpgGDIFileList = class(TfpgFileListBase)
    function    EncodeAttributesString(attrs: longword): TFileModeString;
    constructor Create; override;
    function    InitializeEntry(sr: TSearchRec): TFileEntry; override;
    procedure   PopulateSpecialDirs(const aDirectory: TfpgString); override;
  end;


  TfpgGDIMimeDataBase = class(TfpgMimeDataBase)
  end;


  { Used mainly for sending drags - being the source of the drag }
  TfpgGDIDrag = class(TfpgDragBase)
  private
    function    StringToHandle(const AString: TfpgString): HGLOBAL;
  protected
    FSource: TfpgGDIWindow;
    function    GetSource: TfpgGDIWindow; virtual;
  public
    destructor  Destroy; override;
    function    Execute(const ADropActions: TfpgDropActions; const ADefaultAction: TfpgDropAction=daCopy): TfpgDropAction; override;
  end;


  { Used mainly for receiving drags - being the target of the drag }
  TGDIDragManager = class(TInterfacedObject, IDropTarget)
  private
    FDropTarget: TfpgWindowBase;  { actually a TfpgWidget }
    FRegistered: boolean;
    { IDropTarget }
    function    DragEnter(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;StdCall;
    function    DragOver(grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;StdCall;
    function    DragLeave: HResult;StdCall;
    function    Drop(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD):HResult;StdCall;
  public
    constructor Create(ADropTarget: TfpgWindowBase); reintroduce;
    destructor  Destroy; override;
    procedure   RegisterDragDrop;
    procedure   RevokeDragDrop;
    property    DropTarget: TfpgWindowBase read FDropTarget; { actually a TfpgWidget }
  end;


  TfpgGDITimer = class(TfpgBaseTimer)
  private
    FHandle: THandle;
  protected
    procedure   SetEnabled(const AValue: boolean); override;
  public
    constructor Create(AInterval: integer); override;
  end;


  TfpgGDISystemTrayIcon = class(TfpgComponent)
  public
    constructor Create(AOwner: TComponent); override;
    procedure   Show;
    procedure   Hide;
    function    IsSystemTrayAvailable: boolean;
    function    SupportsMessages: boolean;
  end;


implementation

uses
  fpg_main,
  fpg_widget,
  fpg_popupwindow,
  fpg_stringutils,
  fpg_form,
  math;


var
  wapplication: TfpgApplication;
  uDragSource: TfpgWidget;  { points to the Source widget of the DND when drop is inside the same app }
  MouseFocusedWH: HWND;
  OldMousePos: TPoint;  // used to detect fake MouseMove events
  NeedToUnitialize: Boolean;

// some required keyboard functions
{$INCLUDE fpg_keys_gdi.inc}

{$IFDEF wince}
// A few tweaks to get fpGUI working on the Symbol MC1000 WinCE 4.2
// *** Need to fix the hack in procedure TfpgWindowImpl.DoAllocateWindowHandle

const
  CS_OWNDC = 0;
  WS_OVERLAPPEDWINDOW = WS_VISIBLE;
  WS_POPUPWINDOW = 0;
  WS_EX_APPWINDOW = 0;


// From Lazarus wince\winext.pas:
function GET_X_LPARAM(lp : Windows.LParam) : longint;
begin
  result:=smallint(LOWORD(lp));
end;

function GET_Y_LPARAM(lp : Windows.LParam) : longint;
begin
  result:=smallint(HIWORD(lp));
end;

// *** copied from Lazarus
function MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;
begin
  if nDenominator = 0 then
    Result := -1
  else
//  Result := MathRound( int64(nNumber) * int64(nNumerator) / nDenominator);
    Result := Round( int64(nNumber) * int64(nNumerator) / nDenominator);
end;
{$ENDIF}

function fpgColorToWin(col: TfpgColor): longword;
var
  c: dword;
begin
  c      := fpgColorToRGB(col);
  //swapping bytes (Red and Blue colors)
  Result := ((c and $FF0000) shr 16) or (c and $00FF00) or ((c and $0000FF) shl 16);
end;

function WinColorTofpgColor(col: longword): TfpgColor;
var
  t: TRGBTriple;
begin
  { Windown Color is BBGGRR format }
  t.Blue := (col and $FF0000) shr 16;
  t.Green := (col and $00FF00) shr 8;
  t.Red := (col and $0000FF);
  t.Alpha := $FF;

  Result := RGBTripleTofpgColor(t);
end;

function GetMyWidgetFromHandle(wh: TfpgWinHandle): TfpgWidget;
var
  wg: TfpgWidget;
begin
  {$IFDEF CPU64}
  wg := TfpgWidget(Windows.GetWindowLongPtr(wh, GWL_USERDATA));
  if (wh <> 0) and (MainInstance = GetWindowLongPtr(wh, GWL_HINSTANCE))
    and (wg is TfpgWidget)
  {$ELSE}
  wg := TfpgWidget(Windows.GetWindowLong(wh, GWL_USERDATA));
  if (wh <> 0) and (MainInstance = longword(GetWindowLong(wh, GWL_HINSTANCE)))
    and (wg is TfpgWidget)
  {$ENDIF}
  then
    Result := wg
  else
    Result := nil;
end;

{ Use CenterPoint to get the Center-Point of any rectangle. It is primarily
  for use with, and in, other routines such as Quadrant, and RadialPoint. }
function CenterPoint(Rect: TRect): TPoint;
var
  Tmp:  Longint;
begin
  with Rect do
  begin
    if Right < Left then
    begin
      Tmp   := Right;
      Right := Left;
      Left  := Tmp;
    end;

    if Bottom < Top then
    begin
      Tmp    := Bottom;
      Bottom := Top;
      Top    := Tmp;
    end;

    Result.X := Left + (Right - Left) div 2;
    Result.Y := Top + (Bottom - Top) div 2;
  end;
end;

{ Use LineEndPoint to get the End-Point of a line of any given Length at
  any given angle with any given Start-Point. It is primarily for use in
  other routines such as RadialPoint. The angle is in 1/16th of a degree.
  For example, a full circle equals 5760 (16*360).  Zero degrees is at the
  3'o clock position. }
function LineEndPoint(StartPoint: TPoint; Angle, Length: Extended): TPoint;
begin
  if Angle > 360*16 then
    Angle := Frac(Angle / 360*16) * 360*16;

  if Angle < 0 then
    Angle := 360*16 - abs(Angle);

  Result.Y := StartPoint.Y - Round(Length*Sin(DegToRad(Angle/16)));
  Result.X := StartPoint.X + Round(Length*Cos(DegToRad(Angle/16)));
end;

{ Use EllipseRadialLength to get the Radial-Length of non-rotated ellipse at
  any given Eccentric( aka Radial ) Angle. It is primarily for use in other
  routines such as RadialPoint. The Eccentric angle is in 1/16th of a degree.
  For example, a full circle equals 5760 (16*360).  Zero degrees is at the
  3'o clock position. }
function EllipseRadialLength(Rect: TRect; EccentricAngle: Extended): Longint;
var
  a, b, R: Extended;
begin
  a := (Rect.Right - Rect.Left) div 2;
  b := (Rect.Bottom - Rect.Top) div 2;
  R := Sqr(a)*Sqr(b);
  R := Sqrt(R / ((Sqr(b)*Sqr(Cos(DegToRad(EccentricAngle/16))))
        + (Sqr(a)*Sqr(Sin(DegToRad(EccentricAngle/16))))));
  Result := integer(Trunc(R));
end;

{ Use RadialPoint to get the Radial-Point at any given Eccentric( aka Radial )
  angle on any non-rotated ellipse. It is primarily for use in Angles2Coords.
  The EccentricAngle is in 1/16th of a degree. For example, a full circle
  equals 5760 (16*360).  Zero degrees is at the 3'o clock position. }
function RadialPoint(EccentricAngle: Extended; Rect: TRect): TPoint;
var
  R: Longint;
Begin
  R := EllipseRadialLength(Rect, EccentricAngle);
  Result := LineEndPoint(CenterPoint(Rect), EccentricAngle, R);
end;

{ Use Angles2Coords to convert an Eccentric(aka Radial) Angle and an
  Angle-Length, such as are used in X-Windows and GTK, into the coords,
  for Start and End Radial-Points, such as are used in the Windows API Arc
  Pie and Chord routines. The angles are 1/16th of a degree. For example, a
  full circle equals 5760 (16*360). Positive values of Angle and AngleLength
  mean counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position. }
procedure Angles2Coords(X, Y, Width, Height: Integer; Angle1, Angle2: Extended;
    var SX, SY, EX, EY: Integer);
var
  aRect: TRect;
  SP, EP: TPoint;
begin
  aRect := Classes.Rect(X, Y, X+Width, Y+Height);
  SP := RadialPoint(Angle1, aRect);
  if Angle2 + Angle1 > 360*16 then
    Angle2 := (Angle2 + Angle1) - 360*16
  else
    Angle2 := Angle2 + Angle1;
  EP := RadialPoint(Angle2, aRect);
  SX := SP.X;
  SY := SP.Y;
  EX := EP.X;
  EY := EP.Y;
end;

// returns true when the operating system is windows 2000 or newer
function IsWin2kOrLater: Boolean;
begin
  {$IFDEF WinCE}
  Result := false;
  {$ELSE}
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5);
  {$ENDIF}
end;

// returns true when the operating system is windows XP or newer
function IsWinXPOrLater: Boolean;
begin
  {$IFDEF WinCE}
  Result := false;
  {$ELSE}
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and
     (((Win32MajorVersion = 5) and (Win32MinorVersion >= 1)) or
      ((Win32MajorVersion >= 6) and (Win32MinorVersion >= 0)));
  {$ENDIF}
end;

{  **********   Some helper conversion functions   ************* }

function WinkeystateToShiftstate(keystate: cardinal): TShiftState;
begin
  Result := [];
  if GetKeyState(vk_menu) < 0 then
    Include(result, ssAlt);
  if GetKeyState(vk_shift) < 0 then
    Include(result, ssShift);
  if GetKeyState(vk_control) < 0 then
    Include(result, ssCtrl);
end;

function TranslateToFPGDropActions(const pdwEffects: DWORD): TfpgDropActions;
begin
  Result := [daIgnore];
  if (pdwEffects and DROPEFFECT_LINK) <> 0 then
    Result := Result + [daLink];
  if (pdwEffects and DROPEFFECT_COPY) <> 0  then
    Result := Result + [daCopy];
  if (pdwEffects and DROPEFFECT_MOVE) <> 0  then
    Result := Result + [daMove];
end;

function TranslateToFPGDropAction(const pdwEffects: DWORD): TfpgDropAction;
begin
  if (pdwEffects and DROPEFFECT_LINK) <> 0 then
    Result := daLink
  else if (pdwEffects and DROPEFFECT_COPY) <> 0 then
    Result := daCopy
  else if (pdwEffects and DROPEFFECT_MOVE) <> 0 then
    Result := daMove
  else
    Result := daIgnore;
end;

function TranslateToWinDragEffects(const AActions: TfpgDropActions): DWORD;
begin
  Result := DROPEFFECT_NONE;
  if daLink in AActions then
    Result := Result or DROPEFFECT_LINK;
  if daCopy in AActions then
    Result := Result or DROPEFFECT_COPY;
  if daMove in AActions then
    Result := Result or DROPEFFECT_MOVE;
end;

function TranslateToWinDragEffect(const AAction: TfpgDropAction): DWORD;
begin
  if AAction = daIgnore then
    Result := DROPEFFECT_NONE
  else if daLink = AAction then
    Result := DROPEFFECT_LINK
  else if daCopy = AAction then
    Result := DROPEFFECT_COPY
  else if daMove = AAction then
    Result := DROPEFFECT_MOVE
  else
    Result := DROPEFFECT_NONE; { fallback, but should never be reached }
end;


{$IFDEF wince}
procedure WinCESetDibBits(BMP: HBITMAP; awidth, aheight: Integer; aimgdata: Pointer; var bi: TBitmapInfo);
var
  hdcSrc, hdcDest: HDC;
  hbmSrc: HBITMAP;
  bm: BITMAP;
begin
  hdcDest:= CreateCompatibleDC(0);
  SelectObject(hdcDest, BMP);
  if bi.bmiHeader.biBitCount = 1 then
  begin
    SetDIBitsToDevice(hdcDest, 0, 0, awidth, aheight, 0, 0, 0, aheight, aimgdata, bi, DIB_RGB_COLORS);
  end
  else
  begin
    hdcSrc:= CreateCompatibleDC(0);
    hbmSrc:= CreateBitmap(awidth, aheight, 1, bi.bmiHeader.biBitCount, aimgdata);
    SelectObject(hdcSrc, hbmSrc);
    BitBlt(hdcDest, 0, 0, awidth, aheight, hdcSrc, 0, 0, SRCCOPY);
    DeleteDC(hdcSrc);
    DeleteObject(hbmSrc);
  end;
  DeleteDC(hdcDest);
end;
{$ENDIF}

procedure GetWindowBorderDimensions(const w: TfpgWindowBase; var dx, dy: integer);
var
  bx: integer;  // left/right border width
  by: integer;  // top/bottom border height
  bt: integer;  // title bar
begin
  bx := 0;
  by := 0;
  bt := 0;

  if w.WindowType in [wtWindow, wtModalForm] then
  begin
    if w is TfpgForm then
    begin
      if TfpgForm(w).Sizeable then
      begin
        bx := GetSystemMetrics(SM_CXSIZEFRAME);
        by := GetSystemMetrics(SM_CYSIZEFRAME);
      end
      else
      begin
        bx := GetSystemMetrics(SM_CXFIXEDFRAME);
        by := GetSystemMetrics(SM_CYFIXEDFRAME);
      end;
    end;
    bt := GetSystemMetrics(SM_CYCAPTION);
  end;
  dx := (2 * bx);
  dy := (2 * by) + bt;
end;

function fpgCBTProc(nCode: longint; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  if nCode < 0 then
  begin
    Result := CallNextHookEx(wapplication.ActivationHook, nCode, wParam, lParam);
    Exit; //==>
  end;

  if (nCode = HCBT_ACTIVATE) then
  begin
    // write('Hooked HCBT_ACTIVATE at '+IntToStr(wParam)+': ');
    if (wapplication.TopModalForm <> nil) and
       (wParam <> TfpgGDIWindow(wapplication.TopModalForm).FWinHandle) then
    begin
      // writeln('stopped');
      SetActiveWindow(TfpgGDIWindow(wapplication.TopModalForm).FWinHandle);
      Result := 1;
    end else
    begin
      // writeln('passed');
      Result := 0;
    end;
  end else
    Result := CallNextHookEx(wapplication.ActivationHook, nCode, wParam, lParam);
end;

function fpgWindowProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  w: TfpgGDIWindow;
  pw: TfpgGDIWindow;
  kwg: TfpgWidget;
  mw: TfpgGDIWindow;
  kcode: integer;
  i: integer;
  sstate: integer;
  h: THANDLE;
  p: PChar;
  pt: TPOINT;
  r: TRECT;
  blockmsg: boolean;
  msgp: TfpgMessageParams;
  mcode: integer;
  wmsg: TMsg;
  PaintStruct: TPaintStruct;
  TmpW: widestring;

  //------------
  procedure SetMinMaxInfo(var MinMaxInfo: TMINMAXINFO);

    procedure SetWin32SizePoint(AWidth, AHeight: integer; var pt: TPoint);
    var
      IntfWidth: integer;
      IntfHeight: integer;
      dx: integer;
      dy: integer;
    begin
      // 0 means no constraint
//      if (AWidth=0) and (AHeight=0) then exit;
      dx := 0;
      dy := 0;
      IntfWidth   := AWidth;
      IntfHeight  := AHeight;
      
      GetWindowBorderDimensions(w, dx, dy);
      Inc(IntfWidth, dx);
      Inc(IntfHeight, dy);

      if AWidth > 0 then
        pt.X := IntfWidth;
      if AHeight > 0 then
        pt.Y := IntfHeight;
    end;
  begin
    if (w = nil) {or not (w is TfpgForm)} then
      Exit; //==>
    SetWin32SizePoint(w.MinWidth, w.MinHeight, MinMaxInfo.ptMinTrackSize);
//    SetWin32SizePoint(MaxWidth, MaxHeight, MinMaxInfo.ptMaxSize);
//    SetWin32SizePoint(MaxWidth, MaxHeight, MinMaxInfo.ptMaxTrackSize);
  end;

begin
  if uMsg = WM_CREATE then
  begin
    w := TfpgGDIWindow(PCreateStruct(lParam)^.lpCreateParams);
    w.FWinHandle := hwnd; // this is very important, because number of messages sent
    // before the createwindow returns the window handle
    {$IFDEF CPU64}
    Windows.SetWindowLongPtr(hwnd, GWL_USERDATA, long_ptr(w));
    {$ELSE}
    Windows.SetWindowLong(hwnd, GWL_USERDATA, longword(w));
    {$ENDIF}
  end
  else if (uMsg = WM_RENDERALLFORMATS) or (uMsg = WM_RENDERFORMAT) then
  begin
//    writeln('cliboard rendering...');
    if uMsg = WM_RENDERALLFORMATS then
    begin
//      writeln('ALL');
      CloseClipboard;
      OpenClipboard(0);
    end;
    // Windows seems unhappy unless I do these two steps. Documentation
    // seems to vary on whether opening the clipboard is necessary or
    // is in fact wrong:
    // fall through...
    h := GlobalAlloc(GHND, Length(fpgClipboard.FClipboardText)+1);
    if (h <> 0) then
    begin
      p := GlobalLock(h);
      Move(fpgClipboard.FClipboardText[1], p^, Length(fpgClipboard.FClipboardText));
      inc(p, Length(fpgClipboard.FClipboardText));
      p^ := #0;
      GlobalUnlock(h);
      SetClipboardData(CF_TEXT, h);
    end;

    // Windows also seems unhappy if I don't do this. Documentation very
    // unclear on what is correct:
    if uMsg = WM_RENDERALLFORMATS then
      CloseClipboard;

    Result := 1;
    Exit; //==>
  end;

  {$IFDEF CPU64}
  w      := TfpgGDIWindow(Windows.GetWindowLongPtr(hwnd, GWL_USERDATA));
  {$ELSE}
  w      := TfpgGDIWindow(Windows.GetWindowLong(hwnd, GWL_USERDATA));
  {$ENDIF}
  Result := 0;

  if not (w is TfpgGDIWindow) then
  begin
    {$IFDEF DEBUG} SendDebug('fpGFX/GDI: Unable to detect Window - using DefWindowProc'); {$ENDIF}
    Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
    Exit; //==>
  end;

  blockmsg := False;
  fillchar(msgp, sizeof(msgp), 0);

  case uMsg of
    WM_CHAR,
    WM_KEYUP,
    WM_SYSKEYUP,
    WM_KEYDOWN,
    WM_SYSKEYDOWN:
        begin
          {$IFDEF DEBUGKEYS} SendDebug(w.ClassName + ': wm_char, wm_keyup, wm_keydown'); {$ENDIF}
          kwg := FindKeyboardFocus;
          if kwg <> nil then
            w := kwg;

          msgp.keyboard.shiftstate := WinkeystateToShiftstate(lparam);
//          msgp.keyboard.shiftstate := GetKeyboardShiftState;
          msgp.keyboard.keycode := VirtKeyToKeycode(wParam);

          if (uMsg = WM_KEYDOWN) or (uMsg = WM_SYSKEYDOWN) then
          begin
            fpgSendMessage(nil, w, FPGM_KEYPRESS, msgp);

            // generating WM_CHAR
            fillchar(wmsg, sizeof(wmsg), 0);

            wmsg.hwnd    := hwnd;
            wmsg.message := uMsg;
            wmsg.wParam  := wParam;
            wmsg.lParam  := lParam;

            Windows.TranslateMessage(@wmsg);
            // TranslateMessage sends WM_CHAR ocassionally
            // but NOBODY KNOWS WHEN!

            if (wParam = $2e {VK_DELETE}) then
            begin
              msgp.keyboard.keychar := #127;
              msgp.keyboard.keycode := 0;
              fpgSendMessage(nil, w, FPGM_KEYCHAR, msgp);
            end;

          end
          else if (uMsg = WM_KEYUP) or (uMsg = WM_SYSKEYUP) then
            fpgSendMessage(nil, w, FPGM_KEYRELEASE, msgp)
          else if uMsg = WM_CHAR then
          begin
            tmpW := WideChar(wParam);
            msgp.keyboard.keychar := UTF8Encode(tmpW);
            fpgSendMessage(nil, w, FPGM_KEYCHAR, msgp);
          end;
          
          // Allow Alt+F4 and other system key combinations
          if (uMsg = WM_SYSKEYUP) or (uMsg = WM_SYSKEYDOWN) then
            Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
        end;

    WM_SETCURSOR:
        begin
//          {$IFDEF DEBUG} write(w.ClassName + ': '); {$ENDIF}
          //Writeln('Hittest: ',IntToHex((lParam and $FFFF),4));
          if (lParam and $FFFF) <= 1 then
            w.DoSetMouseCursor
          else
            Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
        end;

    WM_LBUTTONDBLCLK,
    WM_MOUSEMOVE,
    WM_LBUTTONDOWN,
    WM_LBUTTONUP,
    WM_MBUTTONDOWN,
    WM_MBUTTONUP,
    WM_RBUTTONDOWN,
    WM_RBUTTONUP:
        begin
          {$IFDEF DEBUG}
          if uMsg <> WM_MOUSEMOVE then
            writeln('fpGFX/GDI: Found a mouse button event');
          {$ENDIF}
//          msgp.mouse.x := smallint(lParam and $FFFF);
//          msgp.mouse.y := smallint((lParam and $FFFF0000) shr 16);
          msgp.mouse.x := GET_X_LPARAM(lParam);
          msgp.mouse.y := GET_Y_LPARAM(lParam);

          if uMsg = WM_MOUSEMOVE then
          begin
            {$IFDEF DEBUG}
            SendDebugFmt('old x=%d  y=%d', [OldMousePos.x, OldMousePos.y]);
            SendDebugFmt('new x=%d  y=%d', [msgp.mouse.x, msgp.mouse.y]);
            {$ENDIF}
            // Check for fake MouseMove messages - Windows sucks!
            if (OldMousePos.x = msgp.mouse.x) and
               (OldMousePos.y = msgp.mouse.y) then
            begin
              {$IFDEF DEBUG}
              SendDebug('We received fake MouseMove messages');
              {$ENDIF}
              Exit; //==>
            end
            else
            begin
              OldMousePos.x := msgp.mouse.x;
              OldMousePos.y := msgp.mouse.y;
            end;
          end;
          { This closes popup windows when you click the mouse elsewhere }
          if uMsg = WM_LBUTTONDOWN then
          begin
            if (PopupListFirst <> nil) then
            begin
              pt.x  := msgp.mouse.x;
              pt.y  := msgp.mouse.y;
              ClientToScreen(w.WinHandle, pt);
              h     := WindowFromPoint(pt);
              mw    := GetMyWidgetFromHandle(h);
              pw    := mw;
              while (pw <> nil) and (pw.Parent <> nil) do
                pw := TfpgGDIWindow(pw.Parent);

              if ((pw = nil) or (PopupListFind(pw.WinHandle) = nil)) and
                 (not PopupDontCloseWidget(TfpgWidget(mw))) and
                 (uMsg = WM_LBUTTONDOWN) then
              begin
                ClosePopups;
              end;
            end;  { if }
          end;

          if (wapplication.TopModalForm <> nil) then
          begin
            mw := nil;
            mw := TfpgGDIWindow(WidgetParentForm(TfpgWidget(w)));
            if (mw <> nil) and (wapplication.TopModalForm <> mw) then
              blockmsg := True;
          end;

          // Is message blocked by a modal form?
          if not blockmsg then
          begin
            case uMsg of
              WM_MOUSEMOVE:
                  begin
                    mcode := FPGM_MOUSEMOVE;
                  end;

              WM_LBUTTONDBLCLK,
              WM_LBUTTONDOWN,
              WM_MBUTTONDOWN,
              WM_RBUTTONDOWN:
                  begin
                    {$IFDEF DEBUG}
                    SendDebug('fpGUI/GDI: ' + w.ClassName + ': MouseButtonDown event');
                    {$ENDIF}
                    // This is temporary and we should try and move it to
                    // the UI Designer code instead.
                    if (uMsg = WM_LBUTTONDOWN) and (w is TfpgWidget) then
                    begin
                      if TfpgWidget(w).FormDesigner <> nil then
                        w.CaptureMouse;
                    end;
                    mcode := FPGM_MOUSEDOWN;
                  end;
                  
              WM_LBUTTONUP,
              WM_MBUTTONUP,
              WM_RBUTTONUP:
                  begin
                    {$IFDEF DEBUG}
                    SendDebug('fpGFX/GDI: '+ w.ClassName + ': MouseButtonUp event');
                    {$ENDIF}
                    // This is temporary and we should try and move it to
                    // the UI Designer code instead.
                    if (uMsg = WM_LBUTTONUP) and (w is TfpgWidget) then
                    begin
                      if TfpgWidget(w).FormDesigner <> nil then
                        w.ReleaseMouse;
                    end;
                    mcode := FPGM_MOUSEUP;
                  end;
              else
                  mcode := 0;
            end;

            case uMsg of
              WM_MOUSEMOVE:
              begin
                i := 0;
                if (wParam and MK_LBUTTON) <> 0 then
                  i := i or MOUSE_LEFT;
                if (wParam and MK_RBUTTON) <> 0 then
                  i := i or MOUSE_RIGHT;
                if (wParam and MK_MBUTTON) <> 0 then
                  i := i or MOUSE_MIDDLE;
                msgp.mouse.Buttons := i;
              end;

              WM_LBUTTONDBLCLK,
              WM_LBUTTONDOWN,
              WM_LBUTTONUP:
                  msgp.mouse.Buttons := MOUSE_LEFT;
                
              WM_MBUTTONDOWN,
              WM_MBUTTONUP:
                  msgp.mouse.Buttons := MOUSE_MIDDLE;

              WM_RBUTTONDOWN,
              WM_RBUTTONUP:
                  msgp.mouse.Buttons := MOUSE_RIGHT;
            end;

            msgp.mouse.shiftstate := GetKeyboardShiftState;

            if uMsg = WM_MOUSEMOVE then
              w.DoMouseEnterLeaveCheck(w, uMsg, wParam, lParam);

            if mcode <> 0 then
              fpgSendMessage(nil, w, mcode, msgp);
          end;  { if blockmsg }
        end;

    WM_GETMINMAXINFO:
        begin
          SetMinMaxInfo(PMINMAXINFO(LParam)^);
        end;

    WM_SIZE:
        begin
          if w.FSkipResizeMessage then
            Exit;
            
          // note that WM_SIZING allows some control on sizeing
          //writeln('WM_SIZE: wp=',IntToHex(wparam,8), ' lp=',IntToHex(lparam,8));
          msgp.rect.Width  := smallint(lParam and $FFFF);
          msgp.rect.Height := smallint((lParam and $FFFF0000) shr 16);

          {$IFDEF DEBUG}
            SendDebugFmt('%s: WM_SIZE  w=%d  h=%d', [w.ClassName, msgp.rect.width, msgp.rect.Height]);
          {$ENDIF}
          // skip minimize...
          if lparam <> 0 then
            fpgSendMessage(nil, w, FPGM_RESIZE, msgp);
        end;

    WM_MOVE:
        begin
          {$IFDEF DEBUG}
          SendDebug(w.ClassName + ': WM_MOVE');
          {$ENDIF}
          // window decoration correction ...
          {$IFDEF CPU64}
          if (GetWindowLongPtr(w.WinHandle, GWL_STYLE) and WS_CHILD) = 0 then
          {$ELSE}
          if (GetWindowLong(w.WinHandle, GWL_STYLE) and WS_CHILD) = 0 then
          {$ENDIF}
          begin
            GetWindowRect(w.WinHandle, r);
            msgp.rect.Left := r.Left;
            msgp.rect.top  := r.Top;
          end
          else
          begin
            msgp.rect.Left := smallint(lParam and $FFFF);
            msgp.rect.Top  := smallint((lParam and $FFFF0000) shr 16);
          end;

          fpgSendMessage(nil, w, FPGM_MOVE, msgp);
        end;

    WM_MOUSEWHEEL:
        begin
          {$IFDEF DEBUG}
            SendDebugFmt('%s: WM_MOUSEWHEEL: wp=%s  lp=%s', [w.ClassName, IntToHex(wparam,8), IntToHex(lparam,8)]);
          {$ENDIF}
          pt.x := GET_X_LPARAM(lParam);
          pt.y := GET_Y_LPARAM(lParam);
          mw   := nil;
          h    := WindowFromPoint(pt);
          if h > 0 then  // get window mouse is hovering over
          begin
            {$IFDEF CPU64}
            mw := TfpgGDIWindow(Windows.GetWindowLongPtr(h, GWL_USERDATA));
            {$ELSE}
            mw := TfpgGDIWindow(Windows.GetWindowLong(h, GWL_USERDATA));
            {$ENDIF}
          end;

          if (mw is TfpgGDIWindow) then
          begin
            msgp.mouse.x := pt.x;
            msgp.mouse.y := pt.y;
            msgp.mouse.delta := SmallInt(HiWord(wParam)) div -120;

            i := 0;
            if (wParam and MK_LBUTTON) <> 0 then
              i := i or MOUSE_LEFT;
            if (wParam and MK_RBUTTON) <> 0 then
              i := i or MOUSE_RIGHT;
            if (wParam and MK_MBUTTON) <> 0 then
              i := i or MOUSE_MIDDLE;
            msgp.mouse.Buttons := i;
            msgp.mouse.shiftstate := GetKeyboardShiftState;

            fpgSendMessage(nil, mw, FPGM_SCROLL, msgp)
          end;
        end;
(*
    WM_ACTIVATE:  // We currently use WM_NCACTIVATE instead!
        begin
          {$IFDEF DEBUG}
            SendDebug(w.ClassName + ': WM_ACTIVATE');
          {$ENDIF}
          if (Lo(wParam) = WA_INACTIVE) then
            fpgSendMessage(nil, w, FPGM_DEACTIVATE)
          else
            fpgSendMessage(nil, w, FPGM_ACTIVATE);
        end;
*)
    WM_TIMER:
        begin
//          writeln('WM_TIMER');  // used for event wait timeout
          Result := 0;
        end;

    WM_TIMECHANGE:
        begin
          {$IFDEF DEBUG}
          SendDebug(w.ClassName + ': WM_TIMECHANGE');
          {$ENDIF}
          writeln('fpGUI/GDI: ' + w.ClassName + ': WM_TIMECHANGE');
          fpgResetAllTimers;
        end;

    WM_NCACTIVATE:
        begin
          {$IFDEF DEBUG}
            SendDebugFmt('%s: WM_NCACTIVATE wparam=%d', [w.ClassName, wParam]);
          {$ENDIF}
          if (wParam = 0) then
            fpgSendMessage(nil, w, FPGM_DEACTIVATE)
          else
            fpgSendMessage(nil, w, FPGM_ACTIVATE);

          if (PopupListFirst <> nil) and (PopupListFirst.Visible) then
          begin
            {$IFDEF DEBUG}
            SendDebug(' Blockmsg = True (part 1) : ' + PopupListFirst.ClassName);
            {$ENDIF}
            // This is ugly but needed for now to get TfpgCombobox to work
            if (PopupListFirst.ClassName <> 'TDropDownWindow') then
//            if not (PopupListFirst is TfpgPopupWindow) then
              blockmsg := True;
          end;
          //end else
          //if (wapplication.TopModalForm <> nil) then
          //begin
            //if (wParam = 0) and (wapplication.TopModalForm = w) then
            //begin
              //{$IFDEF DEBUG}
              //writeln(' Blockmsg = True (part 2)');
              //{$ENDIF}
              //blockmsg := True;
            //end
            //else if (wParam <> 0) and (wapplication.TopModalForm <> w) then
            //begin
              //{$IFDEF DEBUG}
              //writeln(' Blockmsg = True (part 3)');
              //{$ENDIF}
              //blockmsg := True;
            //end;
          //end;

          if not blockmsg then
            Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
        end;

    WM_CLOSE:
        begin
          {$IFDEF DEBUG}
            SendDebug(w.ClassName + ': WM_Close');
          {$ENDIF}
          fpgSendMessage(nil, w, FPGM_CLOSE, msgp);
        end;

    WM_PAINT:
        begin
          {$IFDEF DEBUG}
            SendDebug(w.ClassName + ': WM_PAINT');
          {$ENDIF}
          Windows.BeginPaint(w.WinHandle, @PaintStruct);
          fpgSendMessage(nil, w, FPGM_PAINT, msgp);
          Windows.EndPaint(w.WinHandle, @PaintStruct);
        end;

    else
      Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
  end;
end;

{ TfpgGDIApplication }

// helper function for DoGetFontFaceList
{$IFDEF wince}
function MyFontEnumerator(var LogFont: ENUMLOGFONT; var TextMetric: NEWTEXTMETRIC;
    FontType: Integer; data: LPARAM): Integer; CDecl;
{$ELSE}
function MyFontEnumerator(var LogFont: ENUMLOGFONTEX; var TextMetric: NEWTEXTMETRICEX;
    FontType: Integer; data: LPARAM): Integer; stdcall;
{$ENDIF}
var
  sl: TStringList;
  s: string;
begin
  sl  := TStringList(data);
  s   := LogFont.elfLogFont.lfFaceName;
  if ((sl.Count = 0) or (sl.Strings[sl.Count-1] <> s)) then
    sl.Add(s);
  Result := 1;
end;

function TfpgGDIApplication.DoGetFontFaceList: TStringList;
var
  LFont: TLogFont;
begin
  Result := TStringList.Create;
  FillChar(LFont, sizeof(LFont), 0);
  LFont.lfCharset := DEFAULT_CHARSET;
  {$IFDEF wince}
  EnumFontFamiliesW(Display, @LFont, @MyFontEnumerator, LParam(result));
  {$ELSE}
  EnumFontFamiliesEx(Display, @LFont, @MyFontEnumerator, LParam(result), 0);
  {$ENDIF}
  Result.Sort;
end;

procedure TfpgGDIApplication.DoWakeMainThread(Sender: TObject);
begin
  // WakeMainThread is called during TThread.Synchronize.
  Windows.PostMessage(TfpgGDIWindow(MainForm).WinHandle, WM_NULL, 0, 0);
end;

procedure TfpgGDIApplication.SetDrag(const AValue: TfpgGDIDrag);
begin
  if Assigned(FDrag) then
    FDrag.Free;
  FDrag := AValue;
end;

function TfpgGDIApplication.GetHiddenWindow: HWND;
begin
  if (FHiddenWindow = 0) then
  begin
    with HiddenWndClass do
    begin
      style         := 0;
      lpfnWndProc   := WndProc(@DefWindowProc);
      hInstance     := MainInstance;
      hIcon         := 0;
      hCursor       := 0;
      hbrBackground := 0;
      lpszClassName := 'FPGHIDDEN';
    end;
    Windows.RegisterClass(@HiddenWndClass);

    FHiddenWindow := CreateWindow('FPGHIDDEN', '',
      DWORD(WS_POPUP), 0, 0, 0, 0, TfpgGDIWindow(MainForm).FWinHandle, 0, MainInstance, nil);
  end;
  Result := FHiddenWindow;
end;

constructor TfpgGDIApplication.Create(const AParams: string);
begin
  inherited Create(AParams);
  FIsInitialized  := False;
  FDisplay        := Windows.GetDC(0);
  Terminated := False;

  with WindowClass do
  begin
    style         := CS_HREDRAW or CS_VREDRAW or CS_OWNDC or CS_DBLCLKS;
    lpfnWndProc   := WndProc(@fpgWindowProc);
    hInstance     := MainInstance;
    // hIcon         := LoadIcon(0, IDI_APPLICATION);
    hIcon         := LoadIcon(hInstance, 'MAINICON');
    hCursor       := LoadCursor(0, IDC_ARROW);
    hbrBackground := 0; //COLOR_WINDOW;
    lpszClassName := 'FPGWIN';
  end;
  Windows.RegisterClass(@WindowClass);

  with WidgetClass do
  begin
    style         := CS_OWNDC or CS_DBLCLKS;
    lpfnWndProc   := WndProc(@fpgWindowProc);
    hInstance     := MainInstance;
    hIcon         := 0;
    hCursor       := 0;
    hbrBackground := 0; //COLOR_BACKGROUND;
    lpszClassName := 'FPGWIDGET';
  end;
  Windows.RegisterClass(@WidgetClass);

  hcr_default   := LoadCursor(0, IDC_ARROW);
  hcr_dir_ew    := LoadCursor(0, IDC_SIZEWE);
  hcr_dir_ns    := LoadCursor(0, IDC_SIZENS);
  hcr_edit      := LoadCursor(0, IDC_IBEAM);
  hcr_dir_nwse  := LoadCursor(0, IDC_SIZENWSE);
  hcr_dir_nesw  := LoadCursor(0, IDC_SIZENESW);
//  hcr_dir_senw  := LoadCursor(0, IDC_SIZENWSE);
//  hcr_dir_swne  := LoadCursor(0, IDC_SIZENESW);
  hcr_move      := LoadCursor(0, IDC_SIZEALL);
  hcr_crosshair := LoadCursor(0, IDC_CROSS);
  hcr_wait      := LoadCursor(0, IDC_WAIT);
  hcr_hand      := LoadCursor(0, IDC_HAND);

  FHiddenWindow := 0;

  ActivationHook := SetWindowsHookEx(WH_CBT, HOOKPROC(@fpgCBTProc), 0, GetCurrentThreadId);

  FIsInitialized := True;
  wapplication   := TfpgApplication(self);
  WakeMainThread := @DoWakeMainThread;
end;

destructor TfpgGDIApplication.Destroy;
begin
  WakeMainThread := nil;
  if Assigned(FDrag) then
    FDrag.Free;
  UnhookWindowsHookEx(ActivationHook);
  inherited Destroy;
end;

function TfpgGDIApplication.MessagesPending: boolean;
var
  Msg: TMsg;
begin
  Result := Windows.PeekMessageW(@Msg, 0, 0, 0, PM_NOREMOVE);
end;

procedure TfpgGDIApplication.DoWaitWindowMessage(atimeoutms: integer);
var
  Msg: TMsg;
  mp: boolean;
begin
  if (atimeoutms >= 0) and (not MessagesPending) then
  begin
    if Assigned(FOnIdle) then
      OnIdle(self);
  end;

  {$IFDEF WinCE}
  // No GetVersion
  Windows.GetMessageW(@Msg, 0, 0, 0);   //NT
  {$ELSE}
  if (GetVersion() < $80000000) then
    Windows.GetMessageW(@Msg, 0, 0, 0)   //NT
  else
    Windows.GetMessage(@Msg, 0, 0, 0);   //Win98
  {$ENDIF}

  Windows.DispatchMessage(@msg);
end;

procedure TfpgGDIApplication.DoFlush;
begin
  {$IFNDEF wince}
  GdiFlush;
  {$ENDIF}
end;

function TfpgGDIApplication.GetScreenWidth: TfpgCoord;
var
  r: TRECT;
begin
  GetWindowRect(GetDesktopWindow, r);
  Result := r.Right - r.Left;
  // Result := Windows.GetSystemMetrics(SM_CXSCREEN);
end;

function TfpgGDIApplication.GetScreenHeight: TfpgCoord;
var
  r: TRECT;
begin
  GetWindowRect(GetDesktopWindow, r);
  Result := r.Bottom - r.Top;
  // Result := Windows.GetSystemMetrics(SM_CYSCREEN);
end;

function TfpgGDIApplication.Screen_dpi_x: integer;
begin
  Result := GetDeviceCaps(wapplication.display, LOGPIXELSX)
end;

function TfpgGDIApplication.Screen_dpi_y: integer;
begin
  Result := GetDeviceCaps(wapplication.display, LOGPIXELSY)
end;

function TfpgGDIApplication.Screen_dpi: integer;
begin
  Result := Screen_dpi_y;
end;

{ TfpgGDIWindow }
var
  // this are required for Windows MouseEnter & MouseExit detection.
  uLastWindowHndl: TfpgWinHandle;

procedure TfpgGDIWindow.HandleDNDLeave(Sender: TObject);
var
  wg: TfpgWidget;
begin
  {$IFDEF DND_DEBUG}
  writeln('TfpgGDIWindow.HandleDNDLeave ');
  {$ENDIF}
  FUserMimeSelection := '';
  wg := self as TfpgWidget;
  if wg.AcceptDrops then  { if we get here, this should always be true anyway }
  begin
    if Assigned(wg.OnDragLeave) then
      wg.OnDragLeave(nil);
  end;
end;

procedure TfpgGDIWindow.HandleDNDEnter(Sender: TObject; DataObj: IDataObject;
    KeyState: Longint; PT: TPoint; var Effect: DWORD);
var
  wg: TfpgWidget;
  swg: TfpgWidget;
  lMimeList: TStringList;
  lMimeChoice: TfpgString;
  lAccept: Boolean;
  lDropAction: TfpgDropAction;
  EnumIntf: IEnumFORMATETC;
  msgp: TfpgMessageParams;
begin
  {$IFDEF DND_DEBUG}
  writeln('TfpgGDIWindow.HandleDNDEnter ');
  {$ENDIF}
  wg := self as TfpgWidget;
  if wg.AcceptDrops then
  begin
    lAccept := False;

    { enumerate the available formats and return them as a StringList }
    lMimeList := EnumDataToStringList(DataObj);
    try
      if lMimeList.Count > 0 then
        lMimeChoice := lMimeList[0]
      else
        {$NOTE We need to replace this message with a resouce string }
        raise Exception.Create('fpGUI/GDI: no mime types available for DND operation');

      lDropAction := TranslateToFPGDropAction(Effect);
      if Assigned(wg.OnDragEnter) then
      begin
        if Assigned(uDragSource) then
          swg := uDragSource as TfpgWidget
        else
          swg := nil;
        wg.OnDragEnter(self, swg, lMimeList, lMimeChoice, lDropAction, lAccept);
      end;
    finally
      lMimeList.Free;
    end;
    if not lAccept then
      Effect := DROPEFFECT_NONE
    else
    begin
      Effect := TranslateToWinDragEffect(lDropAction);
      FUserMimeSelection := lMimeChoice;
      FUserAcceptDrag := True;
    end;

    { Notify widget of drag status, so it can update its look }
    if lAccept then
    begin
      FDropPos.x := PT.x;
      FDropPos.y := PT.y;
      fillchar(msgp, sizeof(msgp), 0);
      msgp.mouse.x := PT.x;
      msgp.mouse.y := PT.y;
      fpgPostMessage(nil, wg, FPGM_DROPENTER, msgp);
    end;
  end;
end;

procedure TfpgGDIWindow.HandleDNDPosition(Sender: TObject; KeyState: Longint; PT: TPoint; var Effect: TfpgOLEDragDropEffect);
var
  msgp: TfpgMessageParams;
  wg: TfpgWidget;
begin
  wg := self as TfpgWidget;
  { Notify widget of drag status, so it can update its look. We do the pos
    check because OLE framework calls DragOver repeatedly even if the mouse
    doesn't move, but simply because the mouse is over the widget. We don't
    want that, for performance reasons. }
  if FDropPos <> PT then
  begin
    {$IFDEF DND_DEBUG}
    writeln('TfpgGDIWindow.HandleDNDPosition ');
    {$ENDIF}
    FDropPos.x := PT.x;
    FDropPos.y := PT.y;
    fillchar(msgp, sizeof(msgp), 0);
    msgp.mouse.x := PT.x;
    msgp.mouse.y := PT.y;
    fpgPostMessage(nil, wg, FPGM_DROPENTER, msgp);
  end;
end;

procedure TfpgGDIWindow.HandleDNDDrop(Sender: TObject; DataObj: IDataObject;
    KeyState: Longint; PT: TPoint; Effect: TfpgOLEDragDropEffect);
var
  FE: FORMATETC;
  stgmed: STGMEDIUM;
  data: pchar;
  wg: TfpgWidget;
  swg: TfpgWidget; { source widget }
  CF: DWORD;
  lIsTranslated: Boolean;
begin
  if not FUserAcceptDrag then
    exit;

  {$IFDEF DND_DEBUG}
  Writeln('TfpgGDIWindow.HandleDNDDrop');
  {$ENDIF}

  wg := self as TfpgWidget;
  { construct a FORMATETC object }
  CF := WindowsClipboardLookup(FUserMimeSelection, lIsTranslated);
  FE := GetFormatEtc(CF);

  if DataObj.QueryGetData(FE) = S_OK then
  begin
    if DataObj.GetData(FE, stgmed) = S_OK then
    begin
      { Yippie! the data is there, so go get it! }
      data := GlobalLock(stgmed.HGLOBAL);
      if Assigned(wg.OnDragDrop) then
      begin
        if Assigned(uDragSource) then
          swg := uDragSource as TfpgWidget
        else
          swg := nil;
        wg.OnDragDrop(wg, swg, pt.x, pt.y, data);
        uDragSource := nil;
      end;
      GlobalUnlock(stgmed.HGLOBAL);
      { release the data using the COM API }
      ReleaseStgMedium(stgmed);
    end;
  end;
end;

function TfpgGDIWindow.GetDropManager: TfpgOLEDropTarget;
begin
  if not Assigned(FDropManager) then
  begin
    FDropManager := TfpgOLEDropTarget.Create(self);
    FDropManager.OnDragLeave := @HandleDNDLeave;
    FDropManager.OnDragEnter := @HandleDNDEnter;
    FDropManager.OnDragOver  := @HandleDNDPosition;
    FDropManager.OnDragDrop  := @HandleDNDDrop;
  end;
  Result := FDropManager;
end;

function TfpgGDIWindow.DoMouseEnterLeaveCheck(AWindow: TfpgGDIWindow; uMsg, wParam, lParam: Cardinal): Boolean;
var
  pt, spt: Windows.POINT;
  msgp: TfpgMessageParams;
  CursorInDifferentWindow: boolean;
  CurrentWindowHndl: TfpgWinHandle;
  MouseCaptureWHndl: TfpgWinHandle;
  LastWindow: TfpgGDIWindow;
  CurrentWindow: TfpgGDIWindow;
begin
  // vvzh: this method currently cannot receive mouse events when mouse pointer
  // is outside of the application window. We could try to play with
  // TrackMouseEvent to catch such events and then
  //  - send FPGM_MOUSEEXIT/FPGM_MOUSEENTER
  //  - set uLastWindowHndl to 0
  // An example:
  // var tme: TTrackMouseEvent;
  // tme.cbSize := SizeOf(tme);
  // tme.hwndTrack := m_hWnd;
  // tme.dwFlags := TME_LEAVE or TME_HOVER;
  // tme.dwHoverTime := 1;
  // TrackMouseEvent(tme);

  pt.x := GET_X_LPARAM(lParam);
  pt.y := GET_Y_LPARAM(lParam);
  spt := pt;
  // only WM_MOUSEWHEEL uses screen coordinates!!!
  if uMsg = WM_MOUSEWHEEL then
    Windows.ScreenToClient(FWinHandle, @pt)
  else
    Windows.ClientToScreen(FWinHandle, @spt);

  CurrentWindowHndl := WindowFromPoint(spt);
  CursorInDifferentWindow := (CurrentWindowHndl <> uLastWindowHndl);
  
  if CursorInDifferentWindow then
  begin
    FillChar(msgp, sizeof(msgp), 0);
    msgp.mouse.x := pt.x;
    msgp.mouse.y := pt.y;
    LastWindow := GetMyWidgetFromHandle(uLastWindowHndl);
    // check if last window still exits. eg: Dialog window could be closed.
    if LastWindow <> nil then
      fpgSendMessage(nil, LastWindow, FPGM_MOUSEEXIT, msgp);

    // if some window captured mouse input, we should not send mouse events to other windows
    MouseCaptureWHndl := GetCapture;
    if (MouseCaptureWHndl = 0) or (MouseCaptureWHndl = CurrentWindowHndl) then
    begin
      CurrentWindow := GetMyWidgetFromHandle(CurrentWindowHndl);
      if (CurrentWindow <> nil) then
        fpgSendMessage(nil, CurrentWindow, FPGM_MOUSEENTER, msgp);
    end;
  end;
  
  uLastWindowHndl := CurrentWindowHndl;
end;

procedure TfpgGDIWindow.WindowSetFullscreen(aFullScreen, aUpdate: boolean);
begin
  if aFullScreen = FFullscreenIsSet then
    Exit; //==>
  if aFullScreen then
  begin
    // backup current bounds and style
    FNonFullscreenStyle := FWinStyle;
    FNonFullscreenRect.SetRect(Left, Top, Width, Height);
    // vvzh: the following lines are the workaround for bug. When calling
    // WindowSetFullscreen from TfpgGDIWindow.DoAllocateWindowHandle,
    // Left and Top are equal to -2147483648. As the result, if
    // we set FullScreen := True at the form creation time and then
    // call SetFullScreen(False) the form disappears, because it is moved
    // to (-2147483648; -2147483648).
    if FNonFullscreenRect.Left < 0 then
      FNonFullscreenRect.Left := 0;
    if FNonFullscreenRect.Top < 0 then
      FNonFullscreenRect.Top := 0;
    
    Left      := 0;
    Top       := 0;
    Width     := wapplication.GetScreenWidth;
    Height    := wapplication.GetScreenHeight;
    
    if aUpdate then
      UpdateWindowPosition;

    FWinStyle := WS_POPUP or WS_SYSMENU;
    FWinStyle := FWinStyle and not(WS_CAPTION or WS_THICKFRAME);
    
    if aUpdate then
    begin
      {$IFDEF CPU64}
      SetWindowLongPtr(FWinHandle, GWL_STYLE, FWinStyle);
      {$ELSE}
      SetWindowLong(FWinHandle, GWL_STYLE, FWinStyle);
      {$ENDIF}
      { According to MSDN, call SetWindowPos to apply changes made by SetWindowLong. }
      SetWindowPos(FWinHandle,HWND_TOP,0,0,Width,Height,SWP_FRAMECHANGED or SWP_SHOWWINDOW or SWP_NOACTIVATE);
    end;
  end
  else
  begin
    FWinStyle := FNonFullscreenStyle;
    if aUpdate then
    begin
      {$IFDEF CPU64}
      SetWindowLongPtr(FWinHandle, GWL_STYLE, FWinStyle);
      {$ELSE}
      SetWindowLong(FWinHandle, GWL_STYLE, FWinStyle);
      {$ENDIF}
      SetWindowPos(FWinHandle,0,0,0,0,0,SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_FRAMECHANGED);
      ShowWindow(FWinHandle, SW_SHOW);
    end;

    Left   := FNonFullscreenRect.Left;
    Top    := FNonFullscreenRect.Top;
    Width  := FNonFullscreenRect.Width;
    Height := FNonFullscreenRect.Height;
    
    if aUpdate then
      UpdateWindowPosition;
  end;
  FFullscreenIsSet := aFullScreen;
end;

procedure TfpgGDIWindow.DoAllocateWindowHandle(AParent: TfpgWindowBase);
var
{$IFDEF wince}
  wcname: widestring;
  wname: widestring;
{$ELSE}
  wcname: string;
  wname: string;
{$ENDIF}
  mid: dword;
  rwidth: integer;
  rheight: integer;
  r: TRect;
begin
  if FWinHandle > 0 then
    Exit; //==>
    
  FSkipResizeMessage := True;

  FWinStyle   := WS_OVERLAPPEDWINDOW;
  FWinStyleEx := WS_EX_APPWINDOW;
  mid         := 0;
  wcname      := 'FPGWIN';

  if AParent <> nil then
    FParentWinHandle := TfpgGDIWindow(AParent).WinHandle
  else
    FParentWinHandle := 0;

  if WindowType = wtChild then
  begin
    FWinStyle   := WS_CHILD;
    FWinStyleEx := 0;
    mid         := 1;
    wcname      := 'FPGWIDGET';
  end
  else if WindowType in [wtPopup] then
  begin
    // This prevents the popup window from stealing the focus. eg: ComboBox dropdown
    FParentWinHandle := GetDesktopWindow;
    FWinStyle   := WS_CHILD;
    FWinStyleEx := WS_EX_TOPMOST or WS_EX_TOOLWINDOW;
  end
  else if WindowType = wtModalForm then
  begin
    if FocusRootWidget <> nil then
      FParentWinHandle := FocusRootWidget.WinHandle
    else
      // set parent window to special hidden window. It helps to hide window taskbar button.
      FParentWinHandle := wapplication.GetHiddenWindow;

    // for modal windows, this is necessary
    FWinStyle   := WS_OVERLAPPEDWINDOW or WS_POPUPWINDOW;
    FWinStyle   := FWinStyle and not (WS_MINIMIZEBOX);
    FWinStyleEx := 0;
  end;

  if ((WindowType = wtWindow) or (WindowType = wtModalForm)) and (waBorderLess in FWindowAttributes) then
    FWinStyle := FWinStyle and WS_POPUP;  // this is different to wtPopop (toolwindow, hint window) because it can steal focus like a normal form

  AdjustWindowStyle;

  if waAutoPos in FWindowAttributes then
  begin
    FLeft := TfpgCoord(CW_USEDEFAULT);
    FTop  := TfpgCoord(CW_USEDEFAULT);
  end;

  if (WindowType <> wtChild) and not (waSizeable in FWindowAttributes) then
    FWinStyle := FWinStyle and not (WS_SIZEBOX or WS_MAXIMIZEBOX);

  FWinStyle := FWinStyle or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  
  if waFullScreen in FWindowAttributes then
    WindowSetFullscreen(True, False);

  wname   := '';
  rwidth  := FWidth;
  rheight := FHeight;

  // Because a child has no borders or title bar the
  // client area size gets adjusted.
  if (FWinStyle and WS_CHILD) = 0 then
  begin
    r.Left   := FLeft;
    r.Top    := FTop;
    r.Right  := FLeft + FWidth;
    r.Bottom := FTop + FHeight;
    {$IFDEF wince}
    AdjustWindowRectEx(@r, FWinStyle, False, FWinStyleEx);
    {$ELSE}
    AdjustWindowRectEx(r, FWinStyle, False, FWinStyleEx);
    {$ENDIF}
    rwidth   := r.Right - r.Left;
    rheight  := r.Bottom - r.Top;
  end;

  {$IFDEF wince}
  // required for some WinCE devices
  FWinStyleEx := FWinStyleEx or WS_VISIBLE; // or WS_BORDER;
  FWinStyle := FWinStyleEx;

  FWinHandle := Windows.CreateWindowExW(
    FWinStyleEx,     // extended window style
    PWideChar(wcname),   // registered class name
    PWideChar(wname),    // window name
    FWinStyle,       // window style
    FLeft,           // horizontal position of window
    FTop,            // vertical position of window
    rwidth,          // window width
    rheight,         // window height
    FParentWinHandle, // handle to parent or owner window
    mid,             // menu handle or child identifier
    MainInstance,    // handle to application instance
    Self             // window-creation data
    );
  {$ELSE}
  FWinHandle := Windows.CreateWindowEx(
    FWinStyleEx,     // extended window style
    PChar(wcname),   // registered class name
    PChar(wname),    // window name
    FWinStyle,       // window style
    FLeft,           // horizontal position of window
    FTop,            // vertical position of window
    rwidth,          // window width
    rheight,         // window height
    FParentWinHandle, // handle to parent or owner window
    mid,             // menu handle or child identifier
    MainInstance,    // handle to application instance
    Self             // window-creation data
    );
  {$ENDIF}

  if waScreenCenterPos in FWindowAttributes then
  begin
    FLeft := (wapplication.ScreenWidth - FWidth) div 2;
    FTop  := (wapplication.ScreenHeight - FHeight) div 2;
    DoMoveWindow(FLeft, FTop);
  end
  else if waOneThirdDownPos in FWindowAttributes then
  begin
    FLeft := (wapplication.ScreenWidth - FWidth) div 2;
    FTop  := (wapplication.ScreenHeight - FHeight) div 3;
    DoMoveWindow(FLeft, FTop);
  end;

  if waStayOnTop in FWindowAttributes then
    SetWindowPos(FWinHandle, HWND_TOPMOST, 0, 0, 0, 0,
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);

  // the forms require some adjustments before the Window appears
  SetWindowParameters;
  FSkipResizeMessage := False;

  if QueueAcceptDrops then
  begin
    DoAcceptDrops(True);
  end;
end;

procedure TfpgGDIWindow.DoReleaseWindowHandle;
begin
  if FWinHandle <= 0 then
    Exit;
  Windows.DestroyWindow(FWinHandle);
  FWinHandle := 0;
end;

procedure TfpgGDIWindow.DoRemoveWindowLookup;
begin
  // Nothing to do here
end;

procedure TfpgGDIWindow.DoSetWindowVisible(const AValue: Boolean);
var
  r: TRect;
begin
  if AValue then
  begin
    FSkipResizeMessage := True;
    BringWindowToTop(FWinHandle);

    if FWindowType in [wtPopup] then
      Windows.ShowWindow(FWinHandle, SW_SHOWNOACTIVATE)
    else
      Windows.ShowWindow(FWinHandle, SW_SHOWNORMAL);

    if (waAutoPos in FWindowAttributes) or
      (waScreenCenterPos in FWindowAttributes) or
      (waOneThirdDownPos in FWindowAttributes) then
    begin
      GetWindowRect(FWinHandle, r);
      FLeft := r.Left;
      FTop  := r.Top;
    end;
    Windows.UpdateWindow(FWinHandle);
    FSkipResizeMessage := False;
  end
  else
    Windows.ShowWindow(FWinHandle, SW_HIDE);
end;

procedure TfpgGDIWindow.DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord);
begin
  if HandleIsValid then
    Windows.SetWindowPos(
      WinHandle, HWND_TOP,
      x, y, 0, 0,
      SWP_NOZORDER or SWP_NOSIZE);// or SWP_NOREDRAW);
end;

function TfpgGDIWindow.DoWindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint;
begin
  if not TfpgGDIWindow(ASource).HandleIsValid then
    Exit; //==>

  Result.X := AScreenPos.X;
  Result.Y := AScreenPos.Y;
  ClientToScreen(TfpgGDIWindow(ASource).WinHandle, Result);
end;

{
procedure TfpgGDIWindow.MoveToScreenCenter;
var
  r : TRECT;
begin
  GetWindowRect(WinHandle, r);
  FLeft := (wapplication.ScreenWidth-(r.Right - r.Left)) div 2;
  FTop := (wapplication.ScreenHeight-(r.Bottom - r.Top)) div 2;
  MoveWindow(FLeft,FTop);
end;
}

procedure TfpgGDIWindow.DoSetWindowTitle(const atitle: string);
begin
  {$ifdef wince}
  Windows.SetWindowText(WinHandle, PWideChar(Utf8Decode(ATitle)));
  {$else}
  if UnicodeEnabledOS then
    Windows.SetWindowTextW(WinHandle, PWideChar(Utf8Decode(ATitle)))
  else
    Windows.SetWindowText(WinHandle, PChar(Utf8ToAnsi(ATitle)));
  {$endif}
end;

procedure TfpgGDIWindow.DoSetMouseCursor;
var
  hc: HCURSOR;
begin
  if not HasHandle then
    Exit; //==>

  case FMouseCursor of
    mcSizeEW:     hc := wapplication.hcr_dir_ew;
    mcSizeNS:     hc := wapplication.hcr_dir_ns;
    mcIBeam:      hc := wapplication.hcr_edit;
    mcSizeNWSE,
    mcSizeSENW:   hc := wapplication.hcr_dir_nwse;
    mcSizeNESW,
    mcSizeSWNE:   hc := wapplication.hcr_dir_nesw;
//    mcSizeSWNE:   hc := wapplication.hcr_dir_swne;
//    mcSizeSENW:   hc := wapplication.hcr_dir_senw;
    mcMove:       hc := wapplication.hcr_move;
    mcCross:      hc := wapplication.hcr_crosshair;
    mcHourGlass:  hc := wapplication.hcr_wait;
    mcHand:       hc := wapplication.hcr_hand;
  else
    hc := wapplication.hcr_default;
  end;

  SetCursor(hc);
end;

procedure TfpgGDIWindow.DoDNDEnabled(const AValue: boolean);
begin
  { GDI has nothing to do here }
end;

procedure TfpgGDIWindow.DoAcceptDrops(const AValue: boolean);
begin
  if AValue then
  begin
    if HasHandle then
        DropManager.RegisterDragDrop
    else
      QueueAcceptDrops := True; // we need to do this once we have a winhandle
  end
  else
  begin
    if HasHandle then
      DropManager.RevokeDragDrop;
    QueueAcceptDrops := False;
  end;
end;

procedure TfpgGDIWindow.DoDragStartDetected;
begin
  inherited DoDragStartDetected;
  { In windows OLE dragging is a blocking function, so it never returns until
    OnStartDragDetected is complete. So we need to set FDragActive to False
    here. }
  FDragActive := False;
  if Assigned(wapplication.FDrag) then
    FreeAndNil(wapplication.FDrag);
end;

function TfpgGDIWindow.GetWindowState: TfpgWindowState;
const
  flagsoffs = 0 * sizeof(integer);
var
  placement: TWindowPlacement;
begin
  Result := inherited GetWindowState;
  {$IFDEF CPU64}
  case GetWindowLongPtr(FWinHandle, flagsoffs) of
  {$ELSE}
  case GetWindowLong(FWinHandle, flagsoffs) of
  {$ENDIF}
    1:
      begin
        Result := wsMaximized; { TODO: this could later become wsFullScreen or something }
      end;

    2:
      begin
        { Do Nothing. This is actually just vertical fullscreen }
      end;

    else
    begin
      placement.length:= sizeof(placement);
      if GetWindowPlacement(FWinHandle, placement) then
      begin
        case placement.ShowCmd of
          SW_SHOWMAXIMIZED: result:= wsMaximized;
          SW_SHOWMINIMIZED: result:= wsMinimized;
        end;
      end;
    end; { case..else }
  end; { case }
end;

constructor TfpgGDIWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWinHandle := 0;
  FDropManager := nil;
  FDropPos.x := 0;
  FDropPos.y := 0;
  FFullscreenIsSet := false;
  FUserMimeSelection := '';
  FUserAcceptDrag := False;
end;

destructor TfpgGDIWindow.Destroy;
begin
  if Assigned(FDropManager) then
    FDropManager.Free;
  inherited Destroy;
end;

procedure TfpgGDIWindow.ActivateWindow;
begin
    Windows.SetWindowPos(
      WinHandle, HWND_NOTOPMOST,
      FLeft, FTop, FWidth, FHeight,
      SWP_NOZORDER or SWP_NOSIZE);
end;

procedure TfpgGDIWindow.CaptureMouse;
begin
  Windows.SetCapture(FWinHandle);
end;

procedure TfpgGDIWindow.ReleaseMouse;
begin
  Windows.ReleaseCapture;
//  if PopupListFirst <> nil then
//    Windows.SetCapture(PopupListFirst^.);
//  if GfxFirstPopup <> nil then SetCapture(GfxFirstPopup^.wg.WinHandle);
end;

procedure TfpgGDIWindow.SetFullscreen(AValue: Boolean);
begin
  inherited SetFullscreen(AValue);
  WindowSetFullscreen(AValue, True);
end;

procedure TfpgGDIWindow.BringToFront;
begin
  if HasHandle then
    Windows.SetWindowPos(
      WinHandle, HWND_TOP,
      FLeft, FTop, FWidth, FHeight,
      SWP_NOACTIVATE or SWP_NOSIZE);
end;

function TfpgGDIWindow.HandleIsValid: boolean;
begin
  Result := FWinHandle > 0;
end;

procedure TfpgGDIWindow.DoUpdateWindowPosition;
var
  bx, by: integer;
begin
  if HasHandle then
  begin
    FSkipResizeMessage := True;
    GetWindowBorderDimensions(Self, bx, by);
    Windows.SetWindowPos(
      WinHandle, HWND_TOP,
      FLeft, FTop, FWidth + bx, FHeight + by,
      SWP_NOZORDER);// or SWP_NOREDRAW);
    Windows.InvalidateRect(WinHandle, nil, True);
    FSkipResizeMessage := False;
  end;
end;

{ TfpgGDICanvas }

constructor TfpgGDICanvas.Create(awin: TfpgWindowBase);
begin
  inherited Create(awin);
  FDrawing      := False;
  FDrawWindow   := nil;
  FBufferBitmap := 0;
end;

destructor TfpgGDICanvas.Destroy;
begin
  if FDrawing then
    DoEndDraw;
  TryFreeBackBuffer;
  inherited;
end;

procedure TfpgGDICanvas.DoBeginDraw(awin: TfpgWindowBase; buffered: boolean);
var
  ARect: TfpgRect;
  bmsize: Windows.TSIZE;
begin
  if FDrawing and buffered and (FBufferBitmap > 0) then
  begin
    // check if the dimensions are ok
    {$IFNDEF wince}
    GetBitmapDimensionEx(FBufferBitmap, bmsize);
    {$ENDIF}
    FDrawWindow := TfpgGDIWindow(awin);
    DoGetWinRect(ARect);
    if (bmsize.cx <> (ARect.Right-ARect.Left+1)) or
       (bmsize.cy <> (ARect.Bottom-ARect.Top+1)) then
      DoEndDraw;
  end;

  if not FDrawing then
  begin
    FDrawWindow := TfpgGDIWindow(awin);
    FWinGC      := Windows.GetDC(FDrawWindow.FWinHandle);

    if buffered then
    begin
      DoGetWinRect(ARect);
      if (FastDoubleBuffer = False) or (FBufferBitmap = 0)
        or (FBufWidth <> ARect.Width) or (FBufHeight <> ARect.Height) then
      begin
        TryFreeBackBuffer;
//        DoGetWinRect(ARect);
        FBufferBitmap := Windows.CreateCompatibleBitmap(FWinGC, ARect.Width, ARect.Height);
        FBufgc        := CreateCompatibleDC(FWinGC);
        Fgc           := FBufgc;
      end;
      SelectObject(FBufgc, FBufferBitmap);
    end
    else
    begin
      FBufferBitmap := 0;
      Fgc           := FWinGC;
    end;

    SetTextAlign(Fgc, TA_TOP);
    SetBkMode(Fgc, TRANSPARENT);

    FBrush      := CreateSolidBrush(0);
    FPen        := CreatePen(PS_SOLID, 0, 0); // defaults to black
    FClipRegion := CreateRectRgn(0, 0, 1, 1);

    FColor           := fpgColorToWin(clText1);
    FLineStyle       := lsSolid;
    FLineWidth       := 1;
    FBackgroundColor := fpgColorToWin(clBoxColor);
  end;

  FDrawing := True;
end;

procedure TfpgGDICanvas.DoEndDraw;
begin
  if FDrawing then
  begin
    DeleteObject(FBrush);
    DeleteObject(FPen);
    DeleteObject(FClipRegion);

    TryFreeBackBuffer;
      
    Windows.ReleaseDC(FDrawWindow.FWinHandle, FWingc);

    FDrawing    := False;
    FDrawWindow := nil;
  end;
end;

function TfpgGDICanvas.GetPixel(X, Y: integer): TfpgColor;
var
  c: longword;
begin
  c := Windows.GetPixel(FWinGC, X, Y);
  if c = CLR_INVALID then
    Writeln('fpGFX/GDI: TfpgGDICanvas.GetPixel returned an invalid color');
  Result := WinColorTofpgColor(c);
end;

procedure TfpgGDICanvas.SetPixel(X, Y: integer; const AValue: TfpgColor);
begin
  Windows.SetPixel(Fgc, X, Y, fpgColorToWin(AValue));
end;

procedure TfpgGDICanvas.DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
var
  SX, SY, EX, EY: Longint;
begin
  {Stupid GDI can't tell the difference between 0 and 360 degrees!!}
  if a2 = 0 then
    Exit; //==>
  {Stupid GDI must be told in which direction to draw}
  {$IFNDEF wince}
  if a2 < 0 then
    Windows.SetArcDirection(FGc, AD_CLOCKWISE)
  else
    Windows.SetArcDirection(FGc, AD_COUNTERCLOCKWISE);
  {$ENDIF}
  Angles2Coords(x, y, w, h, a1*16, a2*16, SX, SY, EX, EY);
  {$IFNDEF wince}
  Windows.Arc(Fgc, x, y, x+w, y+h, SX, SY, EX, EY);
  {$ENDIF}
end;

procedure TfpgGDICanvas.DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
var
  SX, SY, EX, EY: Longint;
begin
  {Stupid GDI can't tell the difference between 0 and 360 degrees!!}
  if a2 = 0 then
    Exit; //==>
  {Stupid GDI must be told in which direction to draw}
  {$IFNDEF wince}
  if a2 < 0 then
    Windows.SetArcDirection(FGc, AD_CLOCKWISE)
  else
    Windows.SetArcDirection(FGc, AD_COUNTERCLOCKWISE);
  {$ENDIF}
  Angles2Coords(x, y, w, h, a1*16, a2*16, SX, SY, EX, EY);
  {$IFNDEF wince}
  Windows.Pie(Fgc, x, y, x+w, y+h, SX, SY, EX, EY);
  {$ENDIF}
end;

procedure TfpgGDICanvas.DoDrawPolygon(Points: PPoint; NumPts: Integer; Winding: boolean);
//var
//  pts: array of TPoint;
begin
  Windows.Polygon(Fgc, Points, NumPts);
end;

procedure TfpgGDICanvas.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
begin
  if FBufferBitmap > 0 then
    BitBlt(FWinGC, x, y, w, h, Fgc, x, y, SRCCOPY);
end;

procedure TfpgGDICanvas.DoAddClipRect(const ARect: TfpgRect);
var
  rg: HRGN;
begin
  rg           := CreateRectRgn(ARect.Left, ARect.Top, ARect.Left+ARect.Width, ARect.Top+ARect.Height);
  FClipRect    := ARect;
  FClipRectSet := True;
  CombineRgn(FClipRegion, rg, FClipRegion, RGN_AND);
  SelectClipRgn(Fgc, FClipRegion);
  DeleteObject(rg);
end;

procedure TfpgGDICanvas.DoClearClipRect;
begin
  SelectClipRgn(Fgc, 0);
  FClipRectSet := False;
end;

procedure TfpgGDICanvas.DoDrawLine(x1, y1, x2, y2: TfpgCoord);
begin
  Windows.MoveToEx(Fgc, x1, y1, nil);
  Windows.LineTo(Fgc, x2, y2);
end;

procedure TfpgGDICanvas.DoDrawRectangle(x, y, w, h: TfpgCoord);
var
  wr: Windows.TRect;
  r: TfpgRect;

{$IFDEF WinCE}
// *** copied from Lazarus
function FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH) : integer;
begin
//roozbeh....works for now!
  Result := Integer(DrawFocusRect(DC,Arect));
end;
{$ENDIF}

begin
  if FLineStyle = lsSolid then
  begin
    wr.Left   := x;
    wr.Top    := y;
    wr.Right  := x + w;
    wr.Bottom := y + h;
    {$IFDEF WinCE}
    FrameRect(Fgc, wr, FBrush);
    {$ELSE}
    Windows.FrameRect(Fgc, wr, FBrush); // this handles 1x1 rectangles
    {$ENDIF}
  end
  else
  begin
    r.SetRect(x, y, w, h);
    DoDrawLine(r.Left, r.Top, r.Right, r.Top);
    DoDrawLine(r.Right, r.Top, r.Right, r.Bottom);
    DoDrawLine(r.Right, r.Bottom, r.Left, r.Bottom);
    DoDrawLine(r.Left, r.Bottom, r.Left, r.Top);
  end;
end;

procedure TfpgGDICanvas.DoDrawString(x, y: TfpgCoord; const txt: string);
var
  WideText: widestring;
begin
  if UTF8Length(txt) < 1 then
    Exit; //==>

  WideText := Utf8Decode(txt);
  {$ifdef wince}
  Windows.ExtTextOut(Fgc, x, y, ETO_CLIPPED, nil, PWideChar(WideText), Length(WideText), nil);
  {$else}
  Windows.TextOutW(Fgc, x, y, PWideChar(WideText), Length(WideText));
  {$endif}
end;

procedure TfpgGDICanvas.DoFillRectangle(x, y, w, h: TfpgCoord);
var
  wr: Windows.TRect;
begin
  wr.Left   := x;
  wr.Top    := y;
  wr.Right  := x + w;
  wr.Bottom := y + h;
  Windows.FillRect(Fgc, wr, FBrush);
end;

procedure TfpgGDICanvas.DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord);
var
  pts: array[1..3] of Windows.TPoint;
begin
  pts[1].X := x1;
  pts[1].Y := y1;
  pts[2].X := x2;
  pts[2].Y := y2;
  pts[3].X := x3;
  pts[3].Y := y3;
  Windows.Polygon(Fgc, pts, 3);
end;

function TfpgGDICanvas.DoGetClipRect: TfpgRect;
begin
  Result := FClipRect;
end;

procedure TfpgGDICanvas.DoGetWinRect(out r: TfpgRect);
var
  wr: TRect;
begin
  GetClientRect(FDrawWindow.FWinHandle, wr);
  r.Top     := wr.Top;
  r.Left    := wr.Left;
  r.Width   := wr.Right - wr.Left + 1;
  r.Height  := wr.Bottom - wr.Top + 1;
end;

procedure TfpgGDICanvas.DoSetClipRect(const ARect: TfpgRect);
begin
  FClipRectSet := True;
  FClipRect    := ARect;
  DeleteObject(FClipRegion);
  FClipRegion  := CreateRectRgn(ARect.Left, ARect.Top, ARect.Left+ARect.Width, ARect.Top+ARect.Height);
  SelectClipRgn(Fgc, FClipRegion);
end;

procedure TfpgGDICanvas.DoSetColor(cl: TfpgColor);
begin
  DeleteObject(FBrush);
  FWindowsColor := fpgColorToWin(cl);
  FBrush := CreateSolidBrush(FWindowsColor);
  DoSetLineStyle(FLineWidth, FLineStyle);
  SelectObject(Fgc, FBrush);
end;

procedure TfpgGDICanvas.DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle);
const
  cDot: array[1..2] of DWORD = (1, 1);
  cDash: array[1..4] of DWORD = (4, 2, 4, 2);
var
  lw: integer;
  logBrush: TLogBrush;
begin
  FLineWidth := awidth;
  logBrush.lbStyle := BS_SOLID;
  logBrush.lbColor := FWindowsColor;
  logBrush.lbHatch := 0;
  DeleteObject(FPen);
  case AStyle of
    lsDot:
      begin
        {$IFNDEF wince}
        FPen := ExtCreatePen(PS_GEOMETRIC or PS_ENDCAP_FLAT or PS_USERSTYLE, FLineWidth, logBrush, Length(cDot), @cDot);
        {$ENDIF}
      end;
    lsDash:
      begin
        {$IFNDEF wince}
        FPen := ExtCreatePen(PS_GEOMETRIC or PS_ENDCAP_FLAT or PS_USERSTYLE, FLineWidth, logBrush, Length(cDash), @cDash);
        {$ENDIF}
      end;
    lsSolid:
      begin
        FPen := CreatePen(PS_SOLID, FLineWidth, FWindowsColor);
      end;
    else
      begin
        FPen := CreatePen(PS_SOLID, FLineWidth, FWindowsColor);
      end;
  end;
  SelectObject(Fgc, FPen);
end;

procedure TfpgGDICanvas.DoSetTextColor(cl: TfpgColor);
begin
  Windows.SetTextColor(Fgc, fpgColorToWin(cl));
end;

procedure TfpgGDICanvas.TryFreeBackBuffer;
begin
  if FBufferBitmap > 0 then
    DeleteObject(FBufferBitmap);
  FBufferBitmap := 0;
  
  if FBufgc > 0 then
    DeleteDC(FBufgc);
  FBufgc := 0;
end;

procedure TfpgGDICanvas.DoSetFontRes(fntres: TfpgFontResourceBase);
begin
  if fntres = nil then
    Exit; //==>
  FCurFontRes := TfpgGDIFontResource(fntres);
  Windows.SelectObject(Fgc, FCurFontRes.Handle);
end;

procedure TfpgGDICanvas.DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer);
const
  DSTCOPY     = $00AA0029;
  ROP_DSPDxax = $00E20746;
var
  tmpdc: HDC;
  rop: longword;
begin
  if img = nil then
    Exit; //==>

  tmpdc := CreateCompatibleDC(wapplication.display);
  SelectObject(tmpdc, TfpgGDIImage(img).BMPHandle);

  if TfpgGDIImage(img).FIsTwoColor then
    rop := PATCOPY
  else
    rop := SRCCOPY;

  if TfpgGDIImage(img).MaskHandle > 0 then
    MaskBlt(Fgc, x, y, w, h, tmpdc, xi, yi, TfpgGDIImage(img).MaskHandle, xi, yi, MakeRop4(rop, DSTCOPY))
  else
    BitBlt(Fgc, x, y, w, h, tmpdc, xi, yi, rop);

  DeleteDC(tmpdc);
end;

procedure TfpgGDICanvas.DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord);
var
  hb: HBRUSH;
  nullpen: HPEN;
begin
  hb      := CreateSolidBrush(fpgColorToWin(fpgColorToRGB(col)));
  nullpen := CreatePen(PS_NULL, 0, 0);

  SetROP2(Fgc, R2_XORPEN);
  SelectObject(Fgc, hb);
  SelectObject(Fgc, nullpen);

  Windows.Rectangle(Fgc, x, y, x + w + 1, y + h + 1);

  SetROP2(Fgc, R2_COPYPEN);
  DeleteObject(hb);
  SelectObject(Fgc, FPen);
end;

{ TfpgGDIFontResource }

constructor TfpgGDIFontResource.Create(const afontdesc: string);
begin
  FFontData := OpenFontByDesc(afontdesc);

  if HandleIsValid then
  begin
    SelectObject(wapplication.display, FFontData);
    GetTextMetrics(wapplication.display, FMetrics);
  end;
end;

destructor TfpgGDIFontResource.Destroy;
begin
  if HandleIsValid then
    Windows.DeleteObject(FFontData);
  inherited;
end;

function TfpgGDIFontResource.OpenFontByDesc(const desc: string): HFONT;
var
  lf: Windows.LOGFONT;
  facename: string;
  cp: integer;
  c: char;
  token: string;
  prop, propval: string;

  function NextC: char;
  begin
    Inc(cp);
    if cp > length(desc) then
      c := #0
    else
      c := desc[cp];
    Result := c;
  end;

  procedure NextToken;
  begin
    token := '';
    while (c <> #0) and (c in [' ', 'a'..'z', 'A'..'Z', '_', '0'..'9']) do
    begin
      token := token + c;
      NextC;
    end;
  end;

begin
  FillChar(lf, sizeof(lf), 0);

  with lf do
  begin
    lfWidth          := 0; { have font mapper choose }
    lfEscapement     := 0; { only straight fonts }
    lfOrientation    := 0; { no rotation }
    lfWeight         := FW_NORMAL;
    lfItalic         := 0;
    lfUnderline      := 0;
    lfStrikeOut      := 0;
    lfCharSet        := DEFAULT_CHARSET; //0; //Byte(Font.Charset);
    lfQuality        := Byte(FontSmoothingType);
    { Everything else as default }
    lfOutPrecision   := OUT_DEFAULT_PRECIS;
    lfClipPrecision  := CLIP_DEFAULT_PRECIS;
    lfPitchAndFamily := DEFAULT_PITCH;
  end;

  cp := 0;
  NextC;
  NextToken;

  facename := token + #0;
  move(facename[1], lf.lfFaceName[0], length(facename));

  if c = '-' then
  begin
    NextC;
    NextToken;
    lf.lfHeight := -MulDiv(StrToIntDef(token, 0), GetDeviceCaps(wapplication.display, LOGPIXELSY), 72);
  end;

  while c = ':' do
  begin
    NextC;
    NextToken;

    prop    := UpperCase(token);
    propval := '';

    if c = '=' then
    begin
      NextC;
      NextToken;
      propval := UpperCase(token);
    end;

    if prop = 'BOLD' then
      lf.lfWeight := FW_BOLD
    else if prop = 'ITALIC' then
      lf.lfItalic := 1
    else if prop = 'ANTIALIAS' then
      if propval = 'FALSE' then
        lf.lfQuality := NONANTIALIASED_QUALITY else
      if propval = 'DEFAULT' then
        lf.lfQuality := DEFAULT_QUALITY;
  end;

  {$IFDEF wince}
  Result := CreateFontIndirectW(@lf);
  {$ELSE}
  Result := CreateFontIndirectA(@lf);
  {$ENDIF}
end;

function TfpgGDIFontResource.HandleIsValid: boolean;
begin
  Result := FFontData <> 0;
end;

function TfpgGDIFontResource.GetAscent: integer;
begin
  Result := FMetrics.tmAscent;
end;

function TfpgGDIFontResource.GetDescent: integer;
begin
  Result := FMetrics.tmDescent;
end;

function TfpgGDIFontResource.GetHeight: integer;
begin
  Result := FMetrics.tmHeight;
end;

function TfpgGDIFontResource.GetTextWidth(const txt: string): integer;
var
  ts: Windows.SIZE;
  WideText: widestring;
begin
  if length(txt) < 1 then
  begin
    Result := 0;
    Exit;
  end;
  SelectObject(wapplication.display, FFontData);

  WideText := Utf8Decode(txt);
  {$ifdef wince}
  Windows.GetTextExtentPoint32(wapplication.display, PWideChar(WideText), Length(WideText), ts);
  {$else}
  Windows.GetTextExtentPoint32W(wapplication.display, PWideChar(WideText), Length(WideText), ts);
  {$endif}

  Result := ts.cx;
end;

{ TfpgGDIImage }

constructor TfpgGDIImage.Create;
begin
  FBMPHandle  := 0;
  FMaskHandle := 0;
  FIsTwoColor := False;
end;

procedure TfpgGDIImage.DoFreeImage;
begin
  if FBMPHandle > 0 then
    DeleteObject(FBMPHandle);
  FBMPHandle := 0;
  if FMaskHandle > 0 then
    DeleteObject(FMaskHandle);
  FMaskHandle := 0;
end;

procedure TfpgGDIImage.DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: Pointer);
var
  bi: TBitmapInfo;
begin
  if FBMPHandle > 0 then
    DeleteObject(FBMPHandle);

  FBMPHandle := CreateCompatibleBitmap(wapplication.display, awidth, aheight);

  FillChar(bi, sizeof(bi), 0);

  with bi.bmiHeader do
  begin
    biSize   := sizeof(bi);
    biWidth  := awidth;
    biHeight := -aheight;
    biPlanes := 1;
    if acolordepth = 1 then
      bibitcount := 1
    else
      bibitcount := 32;
    biCompression := BI_RGB;
    biSizeImage := 0;
    biXPelsPerMeter := 96;
    biYPelsPerMeter := 96;
    biClrUsed       := 0;
    biClrImportant  := 0;
  end;

  {$IFNDEF wince}
  SetDIBits(wapplication.display, FBMPHandle, 0, aheight, aimgdata, bi, DIB_RGB_COLORS);
  {$else}
  WinCESetDibBits(FBMPHandle, awidth, aheight, aimgdata, bi);
  {$ENDIF}

  FIsTwoColor := (acolordepth = 1);
end;

type
  TMyMonoBitmap = packed record
    bmiHeader: TBitmapInfoHeader;
    bmColors: array[1..2] of longword;
  end;

procedure TfpgGDIImage.DoInitImageMask(awidth, aheight: integer; aimgdata: Pointer);
var
  bi: TMyMonoBitmap;
  pbi: PBitmapInfo;
begin
  if FMaskHandle > 0 then
    DeleteObject(FMaskHandle);
  FMaskHandle := CreateBitmap(awidth, aheight, 1, 1, nil);
  FillChar(bi, sizeof(bi), 0);

  with bi.bmiHeader do
  begin
    biSize          := sizeof(bi.bmiHeader);
    biWidth         := awidth;
    biHeight        := -aheight;
    biPlanes        := 1;
    bibitcount      := 1;
    biCompression   := BI_RGB;
    biSizeImage     := 0;
    biXPelsPerMeter := 96;
    biYPelsPerMeter := 96;
    biClrUsed       := 2;
    biClrImportant  := 0;
  end;
  bi.bmColors[1] := $000000;
  bi.bmColors[2] := $FFFFFF;

  pbi := @bi;
  {$IFNDEF wince}
  SetDIBits(wapplication.display, FMaskHandle, 0, aheight, aimgdata, pbi^, DIB_RGB_COLORS);
  {$ELSE}
  WinCESetDibBits(FMaskHandle, awidth, aheight, aimgdata, pbi^);
  {$ENDIF}
end;

{ TfpgGDIClipboard }

function TfpgGDIClipboard.DoGetText: TfpgString;
var
  h: THANDLE;
  p: PChar;
begin
  Result := '';
  if not Windows.OpenClipboard(0) then
    Exit;

  h := GetClipboardData(CF_TEXT);
  if h <> 0 then
  begin
    p := Windows.GlobalLock(h);
    FClipboardText := '';
    while p^ <> #0 do
    begin
      FClipboardText := FClipboardText + p^;
      inc(p);
    end;
    GlobalUnlock(h);
    FClipboardText := AnsiToUtf8(FClipboardText);
  end;
  CloseClipboard;
  Result := FClipboardText;
end;

procedure TfpgGDIClipboard.DoSetText(const AValue: TfpgString);
var
  mem: THandle;
  po2: PWideChar;
  str: PWideChar;
begin
  FClipboardText := AValue;
  if OpenClipboard(0) then
  begin
    str := PWideChar(Utf8Decode(AValue));
    if EmptyClipboard then
    begin
      // Allocate a global memory object for the text.
      mem:= globalalloc(GMEM_MOVEABLE or GMEM_DDESHARE, (length(AValue)+1)*2);
      if mem <> 0 then
      begin
        po2:= globallock(mem);
        if po2 <> nil then
        begin
          move(str^, po2^, (length(AValue)+1)*2);
          globalunlock(mem);
          if SetClipboardData(CF_UNICODETEXT,longword(mem)) <> 0 then
          begin
            //writeln('Successfully copied to clipboard');
          end;
        end
        else
        begin
          globalfree(mem);
        end;
      end;
    end;
    CloseClipboard;
  end;
end;

procedure TfpgGDIClipboard.InitClipboard;
begin
  // nothing to do here
end;


{ TfpgGDIFileList }

function TfpgGDIFileList.EncodeAttributesString(attrs: longword
  ): TFileModeString;
begin
  Result := '';
  //if (attrs and FILE_ATTRIBUTE_ARCHIVE) <> 0    then s := s + 'a' else s := s + ' ';
  if (attrs and FILE_ATTRIBUTE_HIDDEN) <> 0     then Result := Result + 'h';
  if (attrs and FILE_ATTRIBUTE_READONLY) <> 0   then Result := Result + 'r';
  if (attrs and FILE_ATTRIBUTE_SYSTEM) <> 0     then Result := Result + 's';
  if (attrs and FILE_ATTRIBUTE_TEMPORARY) <> 0  then Result := Result + 't';
  if (attrs and FILE_ATTRIBUTE_COMPRESSED) <> 0 then Result := Result + 'c';
end;

constructor TfpgGDIFileList.Create;
begin
  inherited Create;
  FHasFileMode := false;
end;

function TfpgGDIFileList.InitializeEntry(sr: TSearchRec): TFileEntry;
begin
  Result := inherited InitializeEntry(sr);
  if Assigned(Result) then
  begin
    // using sr.Attr here is incorrect and needs to be improved!
    Result.Attributes   := EncodeAttributesString(sr.Attr);
    Result.IsExecutable := (LowerCase(Result.Extension) = '.exe');
  end;
end;

procedure TfpgGDIFileList.PopulateSpecialDirs(const aDirectory: TfpgString);
const
  MAX_DRIVES = 25;
var
  n: integer;
  drvs: string;
begin
  FSpecialDirs.Clear;
  
  // making drive list
  if Copy(aDirectory, 2, 1) = ':' then
  begin
    n := 0;
    while n <= MAX_DRIVES do
    begin
      drvs := chr(n+ord('A'))+':\';
      {$IFNDEF wince}
      if Windows.GetDriveType(PChar(drvs)) <> 1 then
      begin
        FSpecialDirs.Add(drvs);
      end;
      {$ENDIF}
      inc(n);
    end;
  end;

  inherited PopulateSpecialDirs(aDirectory);
end;

{ TfpgGDIDrag }

function TfpgGDIDrag.StringToHandle(const AString: TfpgString): HGLOBAL;
var
  dest: HGLOBAL;
  l: integer;
  p: PChar;
begin
  p := PChar(AString);
  l := Length(AString)+1;
  { allocate and lock a global memory buffer. Make it fixed
    data so we don't have to use GlobalLock }
  dest := GlobalAlloc(GMEM_FIXED, l);
  { Copy the string into the buffer }
  Move(p^, PChar(dest)^, l);
  Result := dest;
end;

function TfpgGDIDrag.GetSource: TfpgGDIWindow;
begin
  Result := FSource;
end;

destructor TfpgGDIDrag.Destroy;
begin
  {$IFDEF DND_DEBUG}
  writeln('TfpgGDIDrag.Destroy ');
  {$ENDIF}
  inherited Destroy;
end;

function TfpgGDIDrag.Execute(const ADropActions: TfpgDropActions; const ADefaultAction: TfpgDropAction): TfpgDropAction;
var
  dwEffect: DWORD;
  dwResult: HRESULT;
  i: Integer;
  F: PFormatEtc;
  S: string;
  M: PStgMedium;
  itm: TfpgMimeDataItem;
  lEffects: DWORD;
  FDataObject: TfpgOLEDataObject;
  FDropSource: TfpgOLEDropSource;
  lIsTranslated: boolean;
begin
  if FDragging then
  begin
    {$IFDEF DND_DEBUG}
    writeln('TfpgGDIDrag.Execute (already dragging)');
    {$ENDIF}
    Result := daIgnore;
  end
  else
  begin
    {$IFDEF DND_DEBUG}
    writeln('TfpgGDIDrag.Execute (new drag)');
    {$ENDIF}
    FDragging := True;
    wapplication.Drag := self;
    lEffects := TranslateToWinDragEffects(ADropActions);
    FDataObject := TfpgOLEDataObject.Create;

    for i := 0 to FMimeData.Count-1 do
    begin
      F := nil;
      M := nil;
      lIsTranslated := False;
      {$Note OLE DND: We are only handling strings at the moment, this needs to be extended to other types too }
      itm := FMimeData[i];
      {$IFDEF DND_DEBUG}
      writeln('  Processing mime-type: ', itm.Format);
      {$ENDIF}

      { description of data we are sending }
      New(F);
      F^.cfFormat := WindowsClipboardLookup(itm.format, lIsTranslated);
      F^.ptd := nil;
      F^.dwAspect := DVASPECT_CONTENT;
      F^.lindex := -1;
      F^.tymed := TYMED_HGLOBAL;
      FDataObject.FormatEtcList.Add(F);

      { storage for data we are sending }
      s := itm.data;
      New(M);
      M^.tymed := TYMED_HGLOBAL;
      M^.hGlobal := StringToHandle(s);
      FDataObject.StgMediumList.Add(M);

      { Original mime type was translated to a known Windows CF_ formats, add
        mimetype string as-is as well }
      if lIsTranslated then
      begin
        New(F);
        F^.cfFormat := RegisterClipboardFormat(PChar(itm.format));
        F^.ptd := nil;
        F^.dwAspect := DVASPECT_CONTENT;
        F^.lindex := -1;
        F^.tymed := TYMED_HGLOBAL;
        FDataObject.FormatEtcList.Add(F);

        { storage for data we are sending }
        s := itm.data;
        New(M);
        M^.tymed := TYMED_HGLOBAL;
        M^.hGlobal := StringToHandle(s);
        FDataObject.StgMediumList.Add(M);
      end;
    end;

    uDragSource := TfpgDrag(self).Source as TfpgWidget;
    { Now let OLE take over from here }
    FDropSource := TfpgOLEDropSource.Create;
    dwResult := ActiveX.DoDragDrop( FDataObject as IDataObject,
                                    FDropSource as IDropSource,
                                    lEffects,
                                    @dwEffect);
    Result := TranslateToFPGDropAction(dwEffect);

    if dwResult = DRAGDROP_S_DROP then
    begin
      { which action did the user select, and act accordingly }
      if dwEffect = DROPEFFECT_COPY then
      begin
        // nothing to do here
      end;
      if dwEffect = DROPEFFECT_MOVE then
      begin
        // Sowehow we need to remove the data from source
      end;
    end;

//    (FDropSource as IUnknown)._Release;
//    (FDataObject as IUnknown)._Release;
  end;
end;

{ TGDIDragManager }

function TGDIDragManager.DragEnter(const dataObj: IDataObject;
  grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; StdCall;
begin

end;

function TGDIDragManager.DragOver(grfKeyState: DWORD; pt: TPoint;
  var dwEffect: DWORD): HResult; StdCall;
begin

end;

function TGDIDragManager.DragLeave: HResult; StdCall;
begin

end;

function TGDIDragManager.Drop(const dataObj: IDataObject; grfKeyState: DWORD;
  pt: TPoint; var dwEffect: DWORD): HResult; StdCall;
begin

end;

constructor TGDIDragManager.Create(ADropTarget: TfpgWindowBase);
begin
  inherited Create;
  FDropTarget := ADropTarget;
  FRegistered := False;
end;

destructor TGDIDragManager.Destroy;
begin
  if FRegistered then
    RevokeDragDrop;
  inherited Destroy;
end;

procedure TGDIDragManager.RegisterDragDrop;
begin
  Activex.RegisterDragDrop(TfpgWidget(FDropTarget).WinHandle, self as IDropTarget)
end;

procedure TGDIDragManager.RevokeDragDrop;
begin
  ActiveX.RevokeDragDrop(TfpgWidget(FDropTarget).WinHandle);
end;

procedure TimerCallBackProc(window_hwnd : hwnd; msg : DWORD; idEvent: UINT; dwTime: DWORD); stdcall;
begin
  { idEvent contains the handle to the timer that got triggered }
  fpgCheckTimers;
end;

{ TfpgGDITimer }

procedure TfpgGDITimer.SetEnabled(const AValue: boolean);
begin
  inherited SetEnabled(AValue);
  if FEnabled then
  begin
//    FHandle := Windows.SetTimer(0, 0, Interval, nil);
    FHandle := Windows.SetTimer(0, 0, Interval, @TimerCallBackProc);
  end
  else
  begin
    if FHandle <> 0 then
    begin
      Windows.KillTimer(FHandle, 0);
      FHandle := 0;
    end;
  end;
end;

constructor TfpgGDITimer.Create(AInterval: integer);
begin
  inherited Create(AInterval);
  FHandle := 0;
end;


{ TfpgGDISystemTrayIcon }

constructor TfpgGDISystemTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TfpgGDISystemTrayIcon.Show;
begin
  //
end;

procedure TfpgGDISystemTrayIcon.Hide;
begin
  //
end;

function TfpgGDISystemTrayIcon.IsSystemTrayAvailable: boolean;
begin
  Result := False;
end;

function TfpgGDISystemTrayIcon.SupportsMessages: boolean;
begin
   Result := True;
end;



initialization
  wapplication   := nil;
  MouseFocusedWH := 0;
  NeedToUnitialize := Succeeded(OleInitialize(nil));

{$IFDEF WinCE}
  UnicodeEnabledOS := True;
  FontSmoothingType := DEFAULT_QUALITY;
{$ELSE}
  WinVersion.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(WinVersion);
  UnicodeEnabledOS := (WinVersion.dwPlatformID = VER_PLATFORM_WIN32_NT) or
    (WinVersion.dwPlatformID = VER_PLATFORM_WIN32_CE);
    
  if SystemParametersInfo(SPI_GETFONTSMOOTHINGTYPE, 0, @FontSmoothingType, 0)
    and (FontSmoothingType = FE_FONTSMOOTHINGCLEARTYPE) then
      FontSmoothingType := CLEARTYPE_QUALITY
    else
      FontSmoothingType := ANTIALIASED_QUALITY;
{$ENDIF}

finalization
  if NeedToUnitialize then
    OleUninitialize;

end.

