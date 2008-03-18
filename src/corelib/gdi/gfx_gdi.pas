{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2008 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This defines the CoreLib backend interface to the Windows GDI API.
}

unit gfx_gdi;

{$mode objfpc}{$H+}

{.$Define Debug}

interface

uses
  Windows,
  Classes,
  SysUtils,
  gfxbase,
  gfx_impl;

{ Constants missing on windows unit }
const
  WM_MOUSEWHEEL         = $020a;    // we could remove this since FPC 2.0.4
  VER_PLATFORM_WIN32_CE = 3;

{ Unicode selection variables }
var
  UnicodeEnabledOS: Boolean;
  WinVersion: TOSVersionInfo;

type
  TfpgGContext = HDC;

  // forward declaration
  TfpgWindowImpl = class;


  TfpgFontResourceImpl = class(TfpgFontResourceBase)
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


  TfpgFontImpl = class(TfpgFontBase)
  end;


  TfpgImageImpl = class(TfpgImageBase)
  private
    FBMPHandle: HBITMAP;
    FMaskHandle: HBITMAP;
    FIsTwoColor: boolean;
    property    BMPHandle: HBITMAP read FBMPHandle;
    property    MaskHandle: HBITMAP read FMaskHandle;
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
    FBufferBitmap: HBitmap;
    FDrawWindow: TfpgWindowImpl;
    Fgc: TfpgGContext;
    fBufgc: TfpgGContext;
    FWinGC: TfpgGContext;
    FBackgroundColor: TfpgColor;
    FCurFontRes: TfpgFontResourceImpl;
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
  public
    constructor Create; override;
    destructor  Destroy; override;
  end;


  TfpgWindowImpl = class(TfpgWindowBase)
  private
    FMouseInWindow: boolean;
    function    DoMouseEnterLeaveCheck(AWindow: TfpgWindowImpl; uMsg, wParam, lParam: Cardinal): Boolean;
  protected
    FWinHandle: TfpgWinHandle;
    FModalForWin: TfpgWindowImpl;
    FWinStyle: longword;
    FWinStyleEx: longword;
    FParentWinHandle: TfpgWinHandle;
    procedure   DoAllocateWindowHandle(AParent: TfpgWindowBase); override;
    procedure   DoReleaseWindowHandle; override;
    procedure   DoRemoveWindowLookup; override;
    procedure   DoSetWindowVisible(const AValue: Boolean); override;
    function    HandleIsValid: boolean; override;
    procedure   DoUpdateWindowPosition(aleft, atop, awidth, aheight: TfpgCoord); override;
    procedure   DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord); override;
    function    DoWindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint; override;
    //procedure MoveToScreenCenter; override;
    procedure   DoSetWindowTitle(const ATitle: string); override;
    procedure   DoSetMouseCursor; override;
    property    WinHandle: TfpgWinHandle read FWinHandle;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   CaptureMouse; override;
    procedure   ReleaseMouse; override;
  end;


  TfpgApplicationImpl = class(TfpgApplicationBase)
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
    LastClickWindow: TfpgWinHandle; // double click generation
    LastWinClickTime: longword;
    function    DoGetFontFaceList: TStringList; override;
  public
    constructor Create(const aparams: string); override;
    function    DoMessagesPending: boolean;
    procedure   DoWaitWindowMessage(atimeoutms: integer);
    procedure   DoFlush;
    function    GetScreenWidth: TfpgCoord; override;
    function    GetScreenHeight: TfpgCoord; override;
    function    Screen_dpi_x: integer; override;
    function    Screen_dpi_y: integer; override;
    function    Screen_dpi: integer; override;
    property    Display: HDC read FDisplay;
  end;


  TfpgClipboardImpl = class(TfpgClipboardBase)
  protected
    FClipboardText: string;
    function    DoGetText: string; override;
    procedure   DoSetText(const AValue: string); override;
    procedure   InitClipboard; override;
  end;


  TfpgFileListImpl = class(TfpgFileListBase)
    procedure   PopulateSpecialDirs(const aDirectory: TfpgString); override;
  end;


implementation

uses
  {$Note Remove the dependency on gfx_widget and gfx_form units.}
  fpgfx,
  gfx_widget,
  gui_form, // remove this!!!!!
  gfx_UTF8Utils,
  math,
  gfx_popupwindow;

var
  wapplication: TfpgApplication;
  MouseFocusedWH: HWND;

// some required keyboard functions
{$INCLUDE gdikeys.inc}

function fpgColorToWin(col: TfpgColor): longword;
var
  c: dword;
begin
  c      := fpgColorToRGB(col);
  //swapping bytes (Red and Blue colors)
  Result := ((c and $FF0000) shr 16) or ((c and $0000FF) shl 16) or (c and $00FF00);
end;

function WinColorTofpgColor(col: longword): TfpgColor;
begin
  //swapping bytes
  Result := fpgColorToWin(col);
end;

function GetMyWidgetFromHandle(wh: TfpgWinHandle): TfpgWidget;
begin
  if (wh <> 0) and (MainInstance = longword(GetWindowLong(wh, GWL_HINSTANCE))) then
    Result := TfpgWidget(Windows.GetWindowLong(wh, GWL_USERDATA))
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
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5);
end;

// returns true when the operating system is windows XP or newer
function IsWinXPOrLater: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and
     (((Win32MajorVersion = 5) and (Win32MinorVersion >= 1)) or
      ((Win32MajorVersion >= 6) and (Win32MinorVersion >= 0)));
end;

function WinkeystateToShiftstate(keystate: cardinal): TShiftState;
begin
 result:= [];
  if GetKeyState(vk_menu) < 0 then begin
    Include(result, ssAlt);
  end;
  if GetKeyState(vk_shift) < 0 then begin
    Include(result, ssShift);
  end;
  if GetKeyState(vk_control) < 0 then begin
    Include(result, ssCtrl);
  end;
end;

function fpgWindowProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  w: TfpgWindowImpl;
  pw: TfpgWindowImpl;
  kwg: TfpgWidget;
  mw: TfpgWindowImpl;
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
  
  //------------
  procedure SetMinMaxInfo(var MinMaxInfo: TMINMAXINFO);

    procedure GetWindowBorderDimentions(const w: TfpgWindowBase; var dx, dy: integer);
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
      
      GetWindowBorderDimentions(w, dx, dy);
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
    w := TfpgWindowImpl(PCreateStruct(lParam)^.lpCreateParams);
    w.FWinHandle := hwnd; // this is very important, because number of messages sent
    // before the createwindow returns the window handle
    Windows.SetWindowLong(hwnd, GWL_USERDATA, longword(w));
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

  w      := TfpgWindowImpl(Windows.GetWindowLong(hwnd, GWL_USERDATA));
  Result := 0;

  if not Assigned(w) then
  begin
    {$IFDEF DEBUG} writeln('fpGFX/GDI: Unable to detect Window - using DefWindowProc'); {$ENDIF}
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
          {$IFDEF DEBUG} write(w.ClassName + ': '); {$ENDIF}
          {$IFDEF DEBUG} writeln('wm_char, wm_keyup, wm_keydown'); {$ENDIF}
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

        // lets generate the FPGM_KEYCHAR for some special keys
        // based on this table of Windows virtual keys
//        case wParam of
//          $70..$7B,  // F1..F12
//          $21..$24,  // home, end, pageup, pagedn
//          $2D..$2E,  // insert, delete
//          $25..$28:  // arrows
//          begin
//            msgp.keyboard.keycode := kcode or $FF00; // scan code + $FF00
//            fpgSendMessage(nil, w, FPGM_KEYCHAR, msgp);
//          end;
//        end;

          end
          else if (uMsg = WM_KEYUP) or (uMsg = WM_SYSKEYUP) then
            fpgSendMessage(nil, w, FPGM_KEYRELEASE, msgp)
          else if uMsg = WM_CHAR then
          begin
            msgp.keyboard.keychar := UTF8Encode(WideChar(wParam));
            fpgSendMessage(nil, w, FPGM_KEYCHAR, msgp);
          end;
        end;

    WM_SETCURSOR:
        begin
//          {$IFDEF DEBUG} write(w.ClassName + ': '); {$ENDIF}
          //Writeln('Hittest: ',IntToHex((lParam and $FFFF),4));
          if (lParam and $FFFF) <= 1 then
            w.DoSetMouseCursor
          else
            Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
        end;
(*
    WM_LBUTTONDBLCLK:
        begin
          {$IFDEF DEBUG}
          writeln('fpGFX/GDI:', w.ClassName + ': MouseButton DoubleClick event');
          {$ENDIF}
        end;
*)

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
//          {$IFDEF DEBUG} write(w.ClassName + ': '); {$ENDIF}
//          {$IFDEF DEBUG} writeln('Mouse Move or Button Click'); {$ENDIF}
          msgp.mouse.x := smallint(lParam and $FFFF);
          msgp.mouse.y := smallint((lParam and $FFFF0000) shr 16);

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
                pw := TfpgWindowImpl(pw.Parent);

              if ((pw = nil) or (PopupListFind(pw.WinHandle) = nil)) and
                 (not PopupDontCloseWidget(TfpgWidget(mw))) and
                 (uMsg = WM_LBUTTONDOWN) then
              begin
                ClosePopups;
//                fpgSendMessage(nil, mw, FPGM_POPUPCLOSE);
              end;
            end;  { if }
          end;

          if (wapplication.TopModalForm <> nil) then
          begin
            mw := nil;
            mw := TfpgWindowImpl(WidgetParentForm(TfpgWidget(w)));
            if (mw <> nil) and (wapplication.TopModalForm <> mw) then
              blockmsg := True;
          end;

          // Is message blocked by a modal form?
          if not blockmsg then
          begin
            case uMsg of
              WM_MOUSEMOVE:
                  mcode := FPGM_MOUSEMOVE;

              WM_LBUTTONDBLCLK,
              WM_LBUTTONDOWN,
              WM_MBUTTONDOWN,
              WM_RBUTTONDOWN:
                  begin
                    {$IFDEF DEBUG}
                    writeln('fpGFX/GDI:', w.ClassName + ': MouseButtonDown event');
                    {$ENDIF}
                    mcode := FPGM_MOUSEDOWN;
//                    if PopupListFirst = nil then
//                      SetCapture(w.WinHandle);
                  end;
                  
              WM_LBUTTONUP,
              WM_MBUTTONUP,
              WM_RBUTTONUP:
                  begin
                    {$IFDEF DEBUG}
                    writeln('fpGFX/GDI:', w.ClassName + ': MouseButtonUp event');
                    {$ENDIF}
                    mcode := FPGM_MOUSEUP;
//                    if PopupListFirst = nil then
//                      ReleaseCapture;
                  end;
              //WM_LBUTTONDBLCLK:
                  //mcode := FPGM_DOUBLECLICK;
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

            if uMsg = WM_MouseMove then
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
          // note that WM_SIZING allows some control on sizeing
          //writeln('WM_SIZE: wp=',IntToHex(wparam,8), ' lp=',IntToHex(lparam,8));
          msgp.rect.Width  := smallint(lParam and $FFFF);
          msgp.rect.Height := smallint((lParam and $FFFF0000) shr 16);

          {$IFDEF DEBUG}
            write(w.ClassName + ': ');
            writeln('WM_SIZE: width=',msgp.rect.width, ' height=',msgp.rect.height);
          {$ENDIF}
          // skip minimize...
          if lparam <> 0 then
            fpgSendMessage(nil, w, FPGM_RESIZE, msgp);
        end;

    WM_MOVE:
        begin
          {$IFDEF DEBUG}
          write(w.ClassName + ': ');
          writeln('WM_MOVE');
          {$ENDIF}
          // window decoration correction ...
          if (GetWindowLong(w.WinHandle, GWL_STYLE) and WS_CHILD) = 0 then
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
            write(w.ClassName + ': ');
            writeln('WM_MOUSEWHEEL: wp=',IntToHex(wparam,8), ' lp=',IntToHex(lparam,8));
          {$ENDIF}
          pt.x := LoWord(lparam);
          pt.y := HiWord(lparam);
          mw   := nil;
          h    := WindowFromPoint(pt);
          if h > 0 then  // get window mouse is hovering over
            mw := TfpgWindowImpl(Windows.GetWindowLong(h, GWL_USERDATA));

          if mw <> nil then
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

    WM_ACTIVATE:
        begin
          {$IFDEF DEBUG}
            write(w.ClassName + ': ');
            writeln('WM_ACTIVATE');
          {$ENDIF}
          if ((wParam and $FFFF) = WA_INACTIVE) then
            fpgSendMessage(nil, w, FPGM_DEACTIVATE)
          else
            fpgSendMessage(nil, w, FPGM_ACTIVATE);
        end;

    WM_TIMER:
        begin
//          writeln('WM_TIMER');  // used for event wait timeout
          Result := 0;
        end;

    WM_NCACTIVATE:
        begin
          {$IFDEF DEBUG}
            write(w.ClassName + ': WM_NCACTIVATE');
          {$ENDIF}
          if (PopupListFirst <> nil) and (PopupListFirst.Visible) then
          begin
            {$IFDEF DEBUG}
            writeln(' Blockmsg = True (part 1) : ' + PopupListFirst.ClassName);
            {$ENDIF}
            // This is ugly but needed for now to get TfpgCombobox to work
            if (PopupListFirst.ClassName <> 'TDropDownWindow') then
//            if not (PopupListFirst is TfpgPopupWindow) then
              blockmsg := True;
          end else
          if (wapplication.TopModalForm <> nil) then
          begin
            if (wParam = 0) and (wapplication.TopModalForm = w) then
            begin
              {$IFDEF DEBUG}
              writeln(' Blockmsg = True (part 2)');
              {$ENDIF}
              blockmsg := True;
            end
            else if (wParam <> 0) and (wapplication.TopModalForm <> w) then
            begin
              {$IFDEF DEBUG}
              writeln(' Blockmsg = True (part 3)');
              {$ENDIF}
              blockmsg := True;
            end;
          end;

          if not blockmsg then
            Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
        end;

    WM_CLOSE:
        begin
          {$IFDEF DEBUG}
            write(w.ClassName + ': ');
            writeln('WM_Close');
          {$ENDIF}
          fpgSendMessage(nil, w, FPGM_CLOSE, msgp);
        end;

    WM_PAINT:
        begin
          {$IFDEF DEBUG}
            write(w.ClassName + ': ');
            writeln('WM_PAINT');
          {$ENDIF}
          Windows.BeginPaint(w.WinHandle, @PaintStruct);
          fpgSendMessage(nil, w, FPGM_PAINT, msgp);
          Windows.EndPaint(w.WinHandle, @PaintStruct);
        end;

    else
      Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
  end;
end;

{ TfpgApplicationImpl }

// helper function for DoGetFontFaceList
function MyFontEnumerator(var LogFont: ENUMLOGFONTEX; var TextMetric: NEWTEXTMETRICEX;
    FontType: Integer; data: LPARAM): Integer; stdcall;
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

function TfpgApplicationImpl.DoGetFontFaceList: TStringList;
var
  LFont: TLogFont;
begin
  Result := TStringList.Create;
  FillChar(LFont, sizeof(LFont), 0);
  LFont.lfCharset := DEFAULT_CHARSET;
  EnumFontFamiliesEx(Display, @LFont, @MyFontEnumerator, LongInt(result), 0);
  Result.Sort;
end;

constructor TfpgApplicationImpl.Create(const aparams: string);
begin
  FIsInitialized  := False;
  FDisplay        := Windows.GetDC(0);
  Terminated := False;

  with WindowClass do
  begin
    style         := CS_HREDRAW or CS_VREDRAW or CS_OWNDC or CS_DBLCLKS;
    lpfnWndProc   := WndProc(@fpgWindowProc);
    hInstance     := MainInstance;
    hIcon         := LoadIcon(0, IDI_APPLICATION);
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

  FIsInitialized := True;
  wapplication   := TfpgApplication(self);
end;

function TfpgApplicationImpl.DoMessagesPending: boolean;
var
  Msg: TMsg;
begin
  Result := Windows.PeekMessageW(@Msg, 0, 0, 0, PM_NOREMOVE);
end;

procedure TfpgApplicationImpl.DoWaitWindowMessage(atimeoutms: integer);
var
  Msg: TMsg;
  timerid: longword;
  ltimerWnd: HWND;
  mp: boolean;
begin
  timerid  := 0;
  if Assigned(wapplication.MainForm) then
    ltimerWnd := TfpgWindowImpl(wapplication.MainForm).WinHandle
  else
    ltimerWnd := 0;

  if (atimeoutms >= 0) and (not DoMessagesPending) then
  begin
    if atimeoutms > 0 then
      timerid := Windows.SetTimer(ltimerWnd, 1, atimeoutms, nil)
    else
      Exit;  // handling waiting timeout
  end;

  {$Note Incorporate Felipe's code from previous fpGUI in here. It handles WinCE and Windows just fine. }
  if (GetVersion() < $80000000) then
    Windows.GetMessageW(@Msg, 0, 0, 0)   //NT
  else
    Windows.GetMessage(@Msg, 0, 0, 0);   //Win98

  Windows.DispatchMessage(@msg);

  if timerid <> 0 then
    Windows.KillTimer(ltimerWnd, 1);  // same IDEvent as used in SetTimer
end;

procedure TfpgApplicationImpl.DoFlush;
begin
  GdiFlush;
end;

function TfpgApplicationImpl.GetScreenWidth: TfpgCoord;
var
  r: TRECT;
begin
  GetWindowRect(GetDesktopWindow, r);
  Result := r.Right - r.Left;
end;

function TfpgApplicationImpl.GetScreenHeight: TfpgCoord;
var
  r: TRECT;
begin
  GetWindowRect(GetDesktopWindow, r);
  Result := r.Bottom - r.Top;
end;

function TfpgApplicationImpl.Screen_dpi_x: integer;
begin
  Result := 96;
end;

function TfpgApplicationImpl.Screen_dpi_y: integer;
begin
  Result := 96;
end;

function TfpgApplicationImpl.Screen_dpi: integer;
begin
  Result := 96;
end;

{ TfpgWindowImpl }
var
  // these are required for Windows MouseEnter & MouseExit detection.
  uLastWindowHndl: TfpgWinHandle;
  uLastWindow: TfpgWindowImpl;
  
function TfpgWindowImpl.DoMouseEnterLeaveCheck(AWindow: TfpgWindowImpl; uMsg, wParam, lParam: Cardinal): Boolean;

  //----------------------
  function CursorInDifferentWindow: Boolean;
  var
    pt: Windows.POINT;
  begin
    pt.x := LoWord(lParam);
    pt.y := HiWord(lParam);

    // only WM_MOUSEWHEEL uses screen coordinates!!!
    if uMsg <> WM_MOUSEWHEEL then
      Windows.ClientToScreen(FWinHandle, @pt);

    Result := WindowFromPoint(pt) <> uLastWindowHndl;
  end;

var
  pt: Windows.POINT;
  msgp: TfpgMessageParams;
  b: boolean;
begin
  b := CursorInDifferentWindow;
  FillChar(msgp, sizeof(msgp), 0);
  pt.x := LoWord(lParam);
  pt.y := HiWord(lParam);
  if (not b) and (not FMouseInWindow) then
  begin
    if not CursorInDifferentWindow then
    begin
      FMouseInWindow := True;
//      writeln('GFX: MouseEnter detected');
      fpgSendMessage(nil, AWindow, FPGM_MOUSEENTER, msgp);
    end;
    Result := uMsg <> WM_MOUSEMOVE;
  end
  else
  begin
//    writeln('... in else part... (', pt.x, ',', pt.y, ')');
    if uMsg = WM_MOUSEWHEEL then
      Windows.ScreenToClient(FWinHandle, @pt);
    // we should change the Width and Height to ClientWidth, ClientHeight
    if (pt.x <= 0) or (pt.y <= 0) or (pt.x >= Width) or
        (pt.y >= Height) or CursorInDifferentWindow then
      FMouseInWindow := False;

    if not FMouseInWindow then
    begin
      msgp.mouse.x := LoWord(lParam);
      msgp.mouse.y := HiWord(lParam);
//      writeln('GFX: MouseExit detected');
      if uLastWindow <> nil then
      begin
        // check if last window still exits. eg: Dialog window could be closed.
        if GetMyWidgetFromHandle(uLastWindowHndl) <> nil then
          fpgSendMessage(nil, uLastWindow, FPGM_MOUSEEXIT, msgp);
      end;
      Result := False;
    end
    else
      Result := True;
  end;
  uLastWindowHndl := FWinHandle;
  uLastWindow := AWindow;
end;

procedure TfpgWindowImpl.DoAllocateWindowHandle(AParent: TfpgWindowBase);
var
  wcname: string;
  wname: string;
  mid: dword;
  rwidth: integer;
  rheight: integer;
  r: TRect;
begin
  if FWinHandle > 0 then
    Exit; //==>

  FWinStyle   := WS_OVERLAPPEDWINDOW;
  FWinStyleEx := WS_EX_APPWINDOW;
  mid         := 0;
  wcname      := 'FPGWIN';

  if aparent <> nil then
    FParentWinHandle := TfpgWindowImpl(AParent).WinHandle
  else
    FParentWinHandle := 0;

  if FWindowType = wtChild then
  begin
    FWinStyle   := WS_CHILD;
    FWinStyleEx := 0;
    mid         := 1;
    wcname      := 'FPGWIDGET';
  end
  else if FWindowType in [wtPopup] then
  begin
    // This prevents the popup window from stealing the focus. eg: ComboBox dropdown
    FParentWinHandle := GetDesktopWindow;
    FWinStyle   := WS_CHILD;
    FWinStyleEx := WS_EX_TOPMOST or WS_EX_TOOLWINDOW;
  end;

  if FWindowType = wtModalForm then
  begin
    // for modal windows, this is necessary
    FWinStyle   := WS_OVERLAPPEDWINDOW or WS_POPUPWINDOW;
    FWinStyle   := FWinStyle and not (WS_MINIMIZEBOX);
    FWinStyleEx := 0;
  end;

  AdjustWindowStyle;

  if waAutoPos in FWindowAttributes then
  begin
    FLeft := TfpgCoord(CW_USEDEFAULT);
    FTop  := TfpgCoord(CW_USEDEFAULT);
  end;

  if (FWindowType <> wtChild) and not (waSizeable in FWindowAttributes) then
    FWinStyle := FWinStyle and not (WS_SIZEBOX or WS_MAXIMIZEBOX or WS_MINIMIZEBOX);

  FWinStyle := FWinStyle or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;

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
    AdjustWindowRectEx(r, FWinStyle, False, FWinStyleEx);
    rwidth   := r.Right - r.Left;
    rheight  := r.Bottom - r.Top;
  end;

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

  if waScreenCenterPos in FWindowAttributes then
  begin
    FLeft := (wapplication.ScreenWidth - FWidth) div 2;
    FTop  := (wapplication.ScreenHeight - FHeight) div 2;
    DoMoveWindow(FLeft, FTop);
  end;

  if waStayOnTop in FWindowAttributes then
    SetWindowPos(FWinHandle, HWND_TOPMOST, 0, 0, 0, 0,
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);

  // the forms require some adjustments before the Window appears
  SetWindowParameters;
end;

procedure TfpgWindowImpl.DoReleaseWindowHandle;
begin
  if FWinHandle <= 0 then
    Exit;
  Windows.DestroyWindow(FWinHandle);
  FWinHandle := 0;
end;

procedure TfpgWindowImpl.DoRemoveWindowLookup;
begin
  // Nothing to do here
end;

procedure TfpgWindowImpl.DoSetWindowVisible(const AValue: Boolean);
var
  r: TRect;
begin
  if AValue then
  begin
    BringWindowToTop(FWinHandle);

    if FWindowType in [wtPopup] then
      Windows.ShowWindow(FWinHandle, SW_SHOWNOACTIVATE)
    else
      Windows.ShowWindow(FWinHandle, SW_SHOWNORMAL);

    if (waAutoPos in FWindowAttributes) or
      (waScreenCenterPos in FWindowAttributes) then
    begin
      GetWindowRect(FWinHandle, r);
      FLeft := r.Left;
      FTop  := r.Top;
    end;
    Windows.UpdateWindow(FWinHandle);
  end
  else
    Windows.ShowWindow(FWinHandle, SW_HIDE);
end;

procedure TfpgWindowImpl.DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord);
begin
  if HandleIsValid then
    Windows.SetWindowPos(
      WinHandle, HWND_TOP,
      x, y, 0, 0,
      SWP_NOZORDER or SWP_NOSIZE);// or SWP_NOREDRAW);
end;

function TfpgWindowImpl.DoWindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint;
begin
  if not TfpgWindowImpl(ASource).HandleIsValid then
    Exit; //==>

  Result.X := AScreenPos.X;
  Result.Y := AScreenPos.Y;
  ClientToScreen(TfpgWindowImpl(ASource).WinHandle, Result);
end;

{
procedure TfpgWindowImpl.MoveToScreenCenter;
var
  r : TRECT;
begin
  GetWindowRect(WinHandle, r);
  FLeft := (wapplication.ScreenWidth-(r.Right - r.Left)) div 2;
  FTop := (wapplication.ScreenHeight-(r.Bottom - r.Top)) div 2;
  MoveWindow(FLeft,FTop);
end;
}

procedure TfpgWindowImpl.DoSetWindowTitle(const atitle: string);
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

procedure TfpgWindowImpl.DoSetMouseCursor;
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

constructor TfpgWindowImpl.Create(aowner: TComponent);
begin
  inherited;
  FWinHandle := 0;
end;

procedure TfpgWindowImpl.CaptureMouse;
begin
  Windows.SetCapture(FWinHandle);
end;

procedure TfpgWindowImpl.ReleaseMouse;
begin
  Windows.ReleaseCapture;
//  if PopupListFirst <> nil then
//    Windows.SetCapture(PopupListFirst^.);
//  if GfxFirstPopup <> nil then SetCapture(GfxFirstPopup^.wg.WinHandle);
end;

function TfpgWindowImpl.HandleIsValid: boolean;
begin
  Result := FWinHandle > 0;
end;

procedure TfpgWindowImpl.DoUpdateWindowPosition(aleft, atop, awidth, aheight: TfpgCoord);
begin
  Windows.SetWindowPos(
    WinHandle, HWND_TOP,
    aleft, atop, awidth, aheight,
    SWP_NOZORDER);// or SWP_NOREDRAW);
end;

{ TfpgCanvasImpl }

constructor TfpgCanvasImpl.Create;
begin
  inherited;
  FDrawing      := False;
  FDrawWindow   := nil;
  FBufferBitmap := 0;
end;

destructor TfpgCanvasImpl.Destroy;
begin
  if FDrawing then
    DoEndDraw;
  TryFreeBackBuffer;
  inherited;
end;

procedure TfpgCanvasImpl.DoBeginDraw(awin: TfpgWindowBase; buffered: boolean);
var
  ARect: TfpgRect;
  bmsize: Windows.TSIZE;
begin
  if FDrawing and buffered and (FBufferBitmap > 0) then
  begin
    // check if the dimensions are ok
    GetBitmapDimensionEx(FBufferBitmap, bmsize);
    FDrawWindow := TfpgWindowImpl(awin);
    DoGetWinRect(ARect);
    if (bmsize.cx <> (ARect.Right-ARect.Left+1)) or
       (bmsize.cy <> (ARect.Bottom-ARect.Top+1)) then
      DoEndDraw;
  end;

  if not FDrawing then
  begin
    FDrawWindow := TfpgWindowImpl(awin);
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

procedure TfpgCanvasImpl.DoEndDraw;
begin
  if FDrawing then
  begin
    DeleteObject(FBrush);
    DeleteObject(FPen);
    DeleteObject(FClipRegion);

    if FastDoubleBuffer = False then
      TryFreeBackBuffer;
      
    Windows.ReleaseDC(FDrawWindow.FWinHandle, FWingc);

    FDrawing    := False;
    FDrawWindow := nil;
  end;
end;

function TfpgCanvasImpl.GetPixel(X, Y: integer): TfpgColor;
var
  c: longword;
begin
  c := Windows.GetPixel(Fgc, X, Y);
  if c = CLR_INVALID then
    Writeln('fpGFX/GDI: TfpgCanvasImpl.GetPixel returned an invalid color');
  Result := WinColorTofpgColor(c);
end;

procedure TfpgCanvasImpl.SetPixel(X, Y: integer; const AValue: TfpgColor);
begin
  Windows.SetPixel(Fgc, X, Y, fpgColorToWin(AValue));
end;

procedure TfpgCanvasImpl.DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
var
  SX, SY, EX, EY: Longint;
begin
  {Stupid GDI can't tell the difference between 0 and 360°!!}
  if a2 = 0 then
    Exit; //==>
  {Stupid GDI must be told in which direction to draw}
  if a2 < 0 then
    Windows.SetArcDirection(FGc, AD_CLOCKWISE)
  else
    Windows.SetArcDirection(FGc, AD_COUNTERCLOCKWISE);
  Angles2Coords(x, y, w, h, a1*16, a2*16, SX, SY, EX, EY);
  {$IFNDEF wince}
  Windows.Arc(Fgc, x, y, x+w, y+h, SX, SY, EX, EY);
  {$ENDIF}
end;

procedure TfpgCanvasImpl.DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
var
  SX, SY, EX, EY: Longint;
begin
  {Stupid GDI can't tell the difference between 0 and 360°!!}
  if a2 = 0 then
    Exit; //==>
  {Stupid GDI must be told in which direction to draw}
  if a2 < 0 then
    Windows.SetArcDirection(FGc, AD_CLOCKWISE)
  else
    Windows.SetArcDirection(FGc, AD_COUNTERCLOCKWISE);
  Angles2Coords(x, y, w, h, a1*16, a2*16, SX, SY, EX, EY);
  {$IFNDEF wince}
  Windows.Pie(Fgc, x, y, x+w, y+h, SX, SY, EX, EY);
  {$ENDIF}
end;

procedure TfpgCanvasImpl.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
begin
  if FBufferBitmap > 0 then
    BitBlt(FWinGC, x, y, w, h, Fgc, x, y, SRCCOPY);
end;

procedure TfpgCanvasImpl.DoAddClipRect(const ARect: TfpgRect);
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

procedure TfpgCanvasImpl.DoClearClipRect;
begin
  SelectClipRgn(Fgc, 0);
  FClipRectSet := False;
end;

procedure TfpgCanvasImpl.DoDrawLine(x1, y1, x2, y2: TfpgCoord);
begin
  Windows.MoveToEx(Fgc, x1, y1, nil);
  Windows.LineTo(Fgc, x2, y2);
end;

procedure TfpgCanvasImpl.DoDrawRectangle(x, y, w, h: TfpgCoord);
var
  wr: Windows.TRect;
  r: TfpgRect;
begin
  if FLineStyle = lsSolid then
  begin
    wr.Left   := x;
    wr.Top    := y;
    wr.Right  := x + w;
    wr.Bottom := y + h;
    Windows.FrameRect(Fgc, wr, FBrush)  // this handles 1x1 rectangles
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

procedure TfpgCanvasImpl.DoDrawString(x, y: TfpgCoord; const txt: string);
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

procedure TfpgCanvasImpl.DoFillRectangle(x, y, w, h: TfpgCoord);
var
  wr: Windows.TRect;
begin
  wr.Left   := x;
  wr.Top    := y;
  wr.Right  := x + w;
  wr.Bottom := y + h;
  Windows.FillRect(Fgc, wr, FBrush);
end;

procedure TfpgCanvasImpl.DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord);
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

function TfpgCanvasImpl.DoGetClipRect: TfpgRect;
begin
  Result := FClipRect;
end;

procedure TfpgCanvasImpl.DoGetWinRect(out r: TfpgRect);
var
  wr: TRect;
begin
  GetClientRect(FDrawWindow.FWinHandle, wr);
  r.Top     := wr.Top;
  r.Left    := wr.Left;
  r.Width   := wr.Right - wr.Left + 1;
  r.Height  := wr.Bottom - wr.Top + 1;
end;

procedure TfpgCanvasImpl.DoSetClipRect(const ARect: TfpgRect);
begin
  FClipRectSet := True;
  FClipRect    := ARect;
  DeleteObject(FClipRegion);
  FClipRegion  := CreateRectRgn(ARect.Left, ARect.Top, ARect.Left+ARect.Width, ARect.Top+ARect.Height);
  SelectClipRgn(Fgc, FClipRegion);
end;

procedure TfpgCanvasImpl.DoSetColor(cl: TfpgColor);
begin
  DeleteObject(FBrush);
  DeleteObject(FPen);

  FWindowsColor := fpgColorToWin(cl);

  FBrush := CreateSolidBrush(FWindowsColor);
  FPen   := CreatePen(FintLineStyle, FLineWidth, FWindowsColor);
  SelectObject(Fgc, FBrush);
  SelectObject(Fgc, FPen);
end;

procedure TfpgCanvasImpl.DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle);
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
        FPen := ExtCreatePen(PS_GEOMETRIC or PS_ENDCAP_FLAT or PS_USERSTYLE, FLineWidth, logBrush, Length(cDot), @cDot);
      end;
    lsDash:
      begin
        FPen := ExtCreatePen(PS_GEOMETRIC or PS_ENDCAP_FLAT or PS_USERSTYLE, FLineWidth, logBrush, Length(cDash), @cDash);
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

procedure TfpgCanvasImpl.DoSetTextColor(cl: TfpgColor);
begin
  Windows.SetTextColor(Fgc, fpgColorToWin(cl));
end;

procedure TfpgCanvasImpl.TryFreeBackBuffer;
begin
  if FBufferBitmap > 0 then
    DeleteObject(FBufferBitmap);
  FBufferBitmap := 0;
  
  if FBufgc > 0 then
    DeleteDC(FBufgc);
  FBufgc := 0;
end;

procedure TfpgCanvasImpl.DoSetFontRes(fntres: TfpgFontResourceBase);
begin
  if fntres = nil then
    Exit; //==>
  FCurFontRes := TfpgFontResourceImpl(fntres);
  Windows.SelectObject(Fgc, FCurFontRes.Handle);
end;

procedure TfpgCanvasImpl.DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer);
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
  SelectObject(tmpdc, TfpgImageImpl(img).BMPHandle);

  if TfpgImageImpl(img).FIsTwoColor then
    rop := PATCOPY
  else
    rop := SRCCOPY;

  if TfpgImageImpl(img).MaskHandle > 0 then
    MaskBlt(Fgc, x, y, w, h, tmpdc, xi, yi, TfpgImageImpl(img).MaskHandle, xi, yi, MakeRop4(rop, DSTCOPY))
  else
    BitBlt(Fgc, x, y, w, h, tmpdc, xi, yi, rop);

  DeleteDC(tmpdc);
end;

procedure TfpgCanvasImpl.DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord);
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

{ TfpgFontResourceImpl }

constructor TfpgFontResourceImpl.Create(const afontdesc: string);
begin
  FFontData := OpenFontByDesc(afontdesc);

  if HandleIsValid then
  begin
    SelectObject(wapplication.display, FFontData);
    GetTextMetrics(wapplication.display, FMetrics);
  end;
end;

destructor TfpgFontResourceImpl.Destroy;
begin
  if HandleIsValid then
    Windows.DeleteObject(FFontData);
  inherited;
end;

function TfpgFontResourceImpl.OpenFontByDesc(const desc: string): HFONT;
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
    lfQuality        := ANTIALIASED_QUALITY;
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
        lf.lfQuality := DEFAULT_QUALITY;
  end;

  Result := CreateFontIndirectA(@lf);
end;

function TfpgFontResourceImpl.HandleIsValid: boolean;
begin
  Result := FFontData <> 0;
end;

function TfpgFontResourceImpl.GetAscent: integer;
begin
  Result := FMetrics.tmAscent;
end;

function TfpgFontResourceImpl.GetDescent: integer;
begin
  Result := FMetrics.tmDescent;
end;

function TfpgFontResourceImpl.GetHeight: integer;
begin
  Result := FMetrics.tmHeight;
end;

function TfpgFontResourceImpl.GetTextWidth(const txt: string): integer;
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

{ TfpgImageImpl }

constructor TfpgImageImpl.Create;
begin
  FBMPHandle  := 0;
  FMaskHandle := 0;
  FIsTwoColor := False;
end;

procedure TfpgImageImpl.DoFreeImage;
begin
  if FBMPHandle > 0 then
    DeleteObject(FBMPHandle);
  FBMPHandle := 0;
  if FMaskHandle > 0 then
    DeleteObject(FMaskHandle);
  FMaskHandle := 0;
end;

procedure TfpgImageImpl.DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: Pointer);
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

  SetDIBits(wapplication.display, FBMPHandle, 0, aheight, aimgdata, bi, DIB_RGB_COLORS);

  FIsTwoColor := (acolordepth = 1);
end;

type
  TMyMonoBitmap = packed record
    bmiHeader: TBitmapInfoHeader;
    bmColors: array[1..2] of longword;
  end;

procedure TfpgImageImpl.DoInitImageMask(awidth, aheight: integer; aimgdata: Pointer);
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
  SetDIBits(wapplication.display, FMaskHandle, 0, aheight, aimgdata, pbi^, DIB_RGB_COLORS);
end;

{ TfpgClipboardImpl }

function TfpgClipboardImpl.DoGetText: string;
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
  end;
  CloseClipboard;
  Result := FClipboardText;
end;

procedure TfpgClipboardImpl.DoSetText(const AValue: string);
begin
  FClipboardText := AValue;
  if OpenClipboard(FClipboardWndHandle) then
  begin
    EmptyClipboard;
    SetClipboardData(CF_TEXT, 0);
    CloseClipboard;
  end;
end;

procedure TfpgClipboardImpl.InitClipboard;
begin
  FClipboardWndHandle := Windows.CreateWindowEx(
      0,			  // extended window style
      'FPGUI',  // registered class name
      nil,			// window name
      0,			  // window style
      0,			  // horizontal position of window
      0,			  // vertical position of window
      10,			  // window width
      10,       // window height
      0,        // handle to parent or owner window
      0,        // menu handle or child identifier
      MainInstance, // handle to application instance
      nil       // window-creation data
      );
end;

{ TfpgFileListImpl }

procedure TfpgFileListImpl.PopulateSpecialDirs(const aDirectory: TfpgString);
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
      if Windows.GetDriveType(PChar(drvs)) <> 1 then
      begin
        FSpecialDirs.Add(drvs);
      end;
      inc(n);
    end;
  end;

  inherited PopulateSpecialDirs(aDirectory);
end;

initialization
  wapplication   := nil;
  MouseFocusedWH := 0;

{$IFDEF WinCE}
  UnicodeEnabledOS := True;
{$ELSE}
  WinVersion.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(WinVersion);
  UnicodeEnabledOS := (WinVersion.dwPlatformID = VER_PLATFORM_WIN32_NT) or
    (WinVersion.dwPlatformID = VER_PLATFORM_WIN32_CE);
{$ENDIF}

end.

