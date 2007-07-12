{
    fpGUI  -  Free Pascal GUI Library

    GFX_GDI  -  Windows GDI specific code

    Copyright (C) 2000 - 2006 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit gfx_gdi;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Windows,
  SysUtils, Classes,
  GfxBase;


resourcestring
  SGDICanvasInvalidFontClass =
      'Tried to set font of class "%s" into GDI context. '
      + 'Only TGDIFont is allowed.';

{ Constants missing on windows unit }
const
  WM_MOUSEWHEEL = $020a;

  VER_PLATFORM_WIN32_CE = 3;

{ Unicode selection variables }
var
  UnicodeEnabledOS: Boolean;

  WinVersion: TOSVersionInfo;

type

  EGDIError = class(EGfxError);

  { TGDIFont }

  TGDIFont = class(TFCustomFont)
  public
    class function GetDefaultFontName(const AFontClass: TGfxFontClass): String; override;
    constructor Create(const Descriptor: String);
    destructor  Destroy; override;
  end;


  { TGDICanvas }

  PGDICanvasState = ^TGDICanvasState;
  TGDICanvasState = record
    Prev: PGDICanvasState;
    Matrix: TGfxMatrix;
    Color, PenColor, FontColor: TGfxPixel;
    PenLineStyle: TGfxLineStyle;
    Font: TFCustomFont;
    CurFontHandle: HFONT;
  end;

  TGDICanvas = class(TFCustomCanvas)
  private
    FColor, FBrushColor, FPenColor, FFontColor: TGfxPixel;
    FLineStyle, FPenLineStyle: TGfxLineStyle;
    FBrush, FOldBrush: HBRUSH;
    FPen, FOldPen: HPEN;
    FFont: TFCustomFont;
    FFontHandle, FDefaultFontHandle, FCurFontHandle: HFONT;
    FFontMetrics: TTextMetric;
    FStateStackpointer: PGDICanvasState;
    procedure   Resized(NewWidth, NewHeight: Integer);
  protected
    function    DoExcludeClipRect(const ARect: TRect): Boolean; override;
    function    DoIntersectClipRect(const ARect: TRect): Boolean; override;
    function    DoUnionClipRect(const ARect: TRect): Boolean; override;
    function    DoGetClipRect: TRect; override;
    procedure   NeedBrush;
    procedure   NeedPen;
    procedure   NeedFont(ANeedFontColor: Boolean);
    procedure   NeedFontColor;
    procedure   DoDrawArc(const ARect: TRect; StartAngle, EndAngle: Single); override;
    procedure   DoDrawCircle(const ARect: TRect); override;
    procedure   DoDrawLine(const AFrom, ATo: TPoint); override;
    procedure   DoDrawPoint(const APoint: TPoint); override;
    procedure   DoFillRect(const ARect: TRect); override;
    procedure   DoFillTriangle(const P1, P2, P3: TPoint); override;
    procedure   DoTextOut(const APosition: TPoint; const AText: String); override;
    procedure   DoCopyRect(ASource: TFCustomCanvas; const ASourceRect: TRect; const ADestPos: TPoint); override;
    procedure   DoMaskedCopyRect(ASource, AMask: TFCustomCanvas; const ASourceRect: TRect; const AMaskPos, ADestPos: TPoint); override;
    procedure   DoDrawImageRect(AImage: TFCustomBitmap; ASourceRect: TRect; const ADestPos: TPoint); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   SetHandle(AHandle: PtrUInt); override;
    function    MapColor(const AColor: TGfxColor): TGfxPixel; override;
    function    FontCellHeight: Integer; override;
    function    TextExtent(const AText: String): TSize; override;
    procedure   SaveState; override;
    procedure   RestoreState; override;
    procedure   EmptyClipRect; override;
    procedure   DoSetColor(AColor: TGfxPixel); override;
    procedure   SetFont(AFont: TFCustomFont); override;
    procedure   SetLineStyle(ALineStyle: TGfxLineStyle); override;
  end;


  TGDIWindowCanvas = class(TGDICanvas)
  private
    FWnd: HWND;
  public
    constructor Create(AWnd: HWND);
    destructor Destroy; override;
  end;


  TGDIBitmapCanvas = class(TGDICanvas)
  private
    FBitmap, FOldBitmap: HBITMAP;
  public
    constructor Create(ABitmap: HBITMAP; AWidth, AHeight: Integer);
    destructor Destroy; override;
    property Bitmap: HBITMAP read FBitmap;
  end;

  { TGDIBitmap }

  TGDIBitmap = class(TFCustomBitmap)
  private
    IsLocked: Boolean;
  public
    constructor Create(AWidth, AHeight: Integer; APixelFormat: TGfxPixelFormat); override;
    destructor Destroy; override;
    procedure Lock(out AData: Pointer; out AStride: LongWord); override;
    procedure Unlock; override;
  end;

  { TGDIScreen }

  TGDIScreen = class(TFCustomScreen)
  protected
    procedure   SetMousePos(const NewPos: TPoint); override;
    function    GetMousePos: TPoint; override;
  public
    constructor Create; override;
    function    CreateBitmapCanvas(AWidth, AHeight: Integer): TFCustomCanvas; override;
    function    CreateMonoBitmapCanvas(AWidth, AHeight: Integer): TFCustomCanvas; override;
  end;


  { TGDIApplication }

  TGDIApplication = class(TFCustomApplication)
  public
    { default methods }
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Initialize(ADisplayName: String = ''); override;
    procedure   Run; override;
    procedure   Quit; override;
  end;

  { TGDIWindow }

  TGDIWindow = class(TFCustomWindow)
  private
    FPaintStruct: TPaintStruct;
  protected
    FHandle: PtrUInt;
    WindowClass: TWndClass;
    WindowClassW: TWndClassW;
    FWindowStyle, FWindowStyleEx: LongWord;
    FMouseInWindow, FHasMouseCapture, FHasFocus: Boolean;

    { Internal resource allocation methods }
    procedure   DoSetCursor; override;
    procedure   DoSetWindowOptions; override;
    function    GetHandle: PtrUInt; override;
    procedure   CreateWindow; override;
    { Internal methods specific to the win backend }
    procedure   UpdateWindowButtons;
    function    DoMouseEnterLeaveCheck(uMsg, wParam, lParam: Cardinal): Boolean;
    procedure   EvInternalPaint;
    { Event processing methods }
    procedure   EvCreate; override;
    procedure   EvFocusIn; override;
    procedure   EvFocusOut; override;
    procedure   EvHide; override;
    procedure   EvKeyPressed(AKey: Word); override;
    procedure   EvKeyReleased(AKey: Word); override;
    procedure   EvKeyChar(AKeyChar: Char); override;
    procedure   EvMouseEnter(const AMousePos: TPoint); override;
    procedure   EvMouseLeave; override;
    procedure   EvMousePressed(AButton: TMouseButton; const AMousePos: TPoint); override;
    procedure   EvMouseReleased(AButton: TMouseButton; const AMousePos: TPoint); override;
    procedure   EvMouseMove(const AMousePos: TPoint); override;
    procedure   EvMouseWheel(AWheelDelta: Single; const AMousePos: TPoint); override;
    procedure   EvPaint; override;
    procedure   EvMove; override;
    procedure   EvResize; override;
    procedure   EvShow; override;
  public
    { Constructors / Destructors }
    constructor Create(AParent: TFCustomWindow; AWindowOptions: TFWindowOptions); override;
    destructor  Destroy; override;
    { Widget controling methods }
    function    GetTitle: String; override;
    procedure   SetTitle(const ATitle: String); override;
    procedure   SetPosition(const APosition: TPoint); override;
    procedure   SetSize(const ASize: TSize); override;
    procedure   SetMinMaxSize(const AMinSize, AMaxSize: TSize); override;
    procedure   SetClientSize(const ASize: TSize); override;
    procedure   SetMinMaxClientSize(const AMinSize, AMaxSize: TSize); override;
    procedure   Show; override;
    procedure   Invalidate; override;
    procedure   CaptureMouse; override;
    procedure   ReleaseMouse; override;
  end;


function RectToWinRect(const ARect: TRect): Windows.Rect;
function WinRectToRect(const ARect: Windows.Rect): TRect;

function VirtKeyToKeycode(VirtKey: Byte): Word;
function GetKeyboardShiftState: TShiftState;


implementation

uses
  math, fpgfx;

{
  Use CenterPoint to get the Center-Point of any rectangle. It is primarily
  for use with, and in, other routines such as Quadrant, and RadialPoint.
}
function CenterPoint(Rect : TRect) : TPoint;
var
  Tmp :  Longint;
begin
  With Rect do begin

    If Right < Left then begin
      Tmp   := Right;
      Right := Left;
      Left  := Tmp;
    end;

    If Bottom < Top then begin
      Tmp    := Bottom;
      Bottom := Top;
      Top    := Tmp;
    end;

    Result.X := Left + (Right - Left) div 2;
    Result.Y := Top + (Bottom - Top) div 2;
  end;
end;


{
  Use LineEndPoint to get the End-Point of a line of any given Length at
  any given angle with any given Start-Point. It is primarily for use in
  other routines such as RadialPoint. The angle is in 1/16th of a degree.
  For example, a full circle equals 5760 (16*360).  Zero degrees is at the
  3'o clock position.
}
Function LineEndPoint(StartPoint : TPoint; Angle, Length : Extended) :
TPoint;
begin
  if Angle > 360*16 then
    Angle := Frac(Angle / 360*16) * 360*16;

  if Angle < 0 then
    Angle := 360*16 - abs(Angle);

  Result.Y := StartPoint.Y - Round(Length*Sin(DegToRad(Angle/16)));
  Result.X := StartPoint.X + Round(Length*Cos(DegToRad(Angle/16)));
end;


{
  Use EllipseRadialLength to get the Radial-Length of non-rotated ellipse at
  any given Eccentric( aka Radial ) Angle. It is primarily for use in other
  routines such as RadialPoint. The Eccentric angle is in 1/16th of a degree.
  For example, a full circle equals 5760 (16*360).  Zero degrees is at the
  3'o clock position.
}
function EllipseRadialLength(Rect: TRect; EccentricAngle: Extended): Longint;
var
  a, b, R : Extended;
begin
  a := (Rect.Right - Rect.Left) div 2;
  b := (Rect.Bottom - Rect.Top) div 2;
  R := Sqr(a)*Sqr(b);
  R := Sqrt(R / ((Sqr(b)*Sqr(Cos(DegToRad(EccentricAngle/16))))
        + (Sqr(a)*Sqr(Sin(DegToRad(EccentricAngle/16))))));
  Result := integer(Trunc(R));
end;


{
  Use RadialPoint to get the Radial-Point at any given Eccentric( aka Radial )
  angle on any non-rotated ellipse. It is primarily for use in Angles2Coords.
  The EccentricAngle is in 1/16th of a degree. For example, a full circle
  equals 5760 (16*360).  Zero degrees is at the 3'o clock position.
}
function RadialPoint(EccentricAngle: Extended; Rect: TRect): TPoint;
var
  R: Longint;
Begin
  R := EllipseRadialLength(Rect, EccentricAngle);
  Result := LineEndPoint(CenterPoint(Rect), EccentricAngle, R);
end;


{
  Use Angles2Coords to convert an Eccentric(aka Radial) Angle and an
  Angle-Length, such as are used in X-Windows and GTK, into the coords,
  for Start and End Radial-Points, such as are used in the Windows API Arc
  Pie and Chord routines. The angles are 1/16th of a degree. For example, a
  full circle equals 5760 (16*360). Positive values of Angle and AngleLength
  mean counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position.
}
procedure Angles2Coords(X, Y, Width, Height: Integer; Angle1, Angle2: Extended;
    var SX, SY, EX, EY: Integer);
var
  aRect: TRect;
  SP, EP: TPoint;
begin
  aRect := Rect(X, Y, X + Width, Y + Height);
  SP := RadialPoint(Angle1, aRect);
  If Angle2 + Angle1 > 360*16 then
    Angle2 := (Angle2 + Angle1) - 360*16
  else
    Angle2 := Angle2 + Angle1;
  EP := RadialPoint(Angle2, aRect);
  SX := SP.X;
  SY := SP.Y;
  EX := EP.X;
  EY := EP.Y;
end;


{ TGDIFont }

class function TGDIFont.GetDefaultFontName(const AFontClass: TGfxFontClass): String;
const
  FontNames: array[TGfxFontClass] of String = (
    'Times New Roman', 'Arial', 'Courier New', 'Wingdings');
begin
  Result := FontNames[AFontClass];
end;

constructor TGDIFont.Create(const Descriptor: String);
type
  TXLFDFields = (lfdFoundry, lfdFamily, lfdWeight, lfdSlant, lfdSetWidth,
    lfdAddStyle, lfdPixelSize, lfdPointSize, lfdResolutionX, lfdResolutionY,
    lfdSpacing, lfdAverageWidth, lfdCharsetRegistry, lfdCharsetEncoding);
var
  Fields: array[TXLFDFields] of String;
  FontInfo: LOGFONT;
  FieldIndex: TXLFDFields;
  s: String;
  i: Integer;
  dc: HDC;
begin
  inherited Create;

  // Split the font descriptor string
  s := Descriptor;
  for FieldIndex := Low(TXLFDFields) to High(TXLFDFields) do
  begin
    Fields[FieldIndex] := Copy(s, 2, Length(s));
    i := Pos('-', Fields[FieldIndex]);
    if i = 0 then
      i := Length(s);
    Fields[FieldIndex] := Copy(Fields[FieldIndex], 1, i - 1);
    s := Copy(s, i + 1, Length(s));
  end;

  FillChar(FontInfo, SizeOf(FontInfo), 0);

  if (Length(Fields[lfdPixelSize]) > 0) and (Fields[lfdPixelSize] <> '*') then
    FontInfo.lfHeight := -StrToInt(Fields[lfdPixelSize])
  else if (Length(Fields[lfdPointSize]) > 0) and
    (Fields[lfdPointSize] <> '*') then
  begin
    dc := Windows.GetDC(0);
    FontInfo.lfHeight := ((StrToInt(Fields[lfdPointSize]) *
      Windows.GetDeviceCaps(dc, LOGPIXELSY)) + (5 * 72)) div 720;
    Windows.ReleaseDC(0, dc);
  end;

  if (Length(Fields[lfdAverageWidth]) > 0) and
    (Fields[lfdAverageWidth] <> '*') then
    FontInfo.lfWidth := StrToInt(Fields[lfdAverageWidth]);

  if CompareText(Fields[lfdWeight], 'medium') = 0 then
    FontInfo.lfWeight := FW_MEDIUM
  else if CompareText(Fields[lfdWeight], 'bold') = 0 then
    FontInfo.lfWeight := FW_BOLD;

  if (CompareText(Fields[lfdSlant], 'i') = 0) or
    (CompareText(Fields[lfdSlant], 'o') = 0) then
    FontInfo.lfItalic := 1;

  if (CompareText(Fields[lfdSpacing], 'm') = 0) or
    (CompareText(Fields[lfdSpacing], 'c') = 0) then
    FontInfo.lfPitchAndFamily := FIXED_PITCH
  else if CompareText(Fields[lfdSpacing], 'p') = 0 then
    FontInfo.lfPitchAndFamily := VARIABLE_PITCH;

  if Fields[lfdFamily] <> '*' then
    FontInfo.lfFaceName := Fields[lfdFamily];

  FHandle := Windows.CreateFontIndirect(@FontInfo);
end;


destructor TGDIFont.Destroy;
begin
  Windows.DeleteObject(Handle);
  inherited Destroy;
end;


{ TGDICanvas }

constructor TGDICanvas.Create;
begin
  inherited Create;
  FDefaultFontHandle := Windows.GetStockObject(DEFAULT_GUI_FONT);
  FCurFontHandle := FDefaultFontHandle;
end;

destructor TGDICanvas.Destroy;
begin
  if FBrush <> 0 then
  begin
    Windows.SelectObject(Handle, FOldBrush);
    Windows.DeleteObject(FBrush);
  end;
  if FPen <> 0 then
  begin
    Windows.SelectObject(Handle, FOldPen);
    Windows.DeleteObject(FPen);
  end;
  inherited Destroy;
end;

procedure TGDICanvas.SetHandle(AHandle: PtrUInt);
begin
  FHandle := AHandle;

  { It's possible to set the Handle to zero
    In this case we effectively disallowing new painting
    until a new handle is set }
  if AHandle = 0 then Exit;

  Windows.SelectObject(Handle, FDefaultFontHandle);
  Windows.GetTextMetrics(Handle, @FFontMetrics);
  Windows.SetBkMode(Handle, TRANSPARENT);
end;

procedure TGDICanvas.SaveState;
var
  SavedState: PGDICanvasState;
  NewRegion: HRGN;
begin
  New(SavedState);
  SavedState^.Prev            := FStateStackpointer;
  SavedState^.Matrix          := Matrix;
  SavedState^.Color           := FColor;
  SavedState^.PenColor        := FPenColor;
  SavedState^.PenLineStyle    := FPenLineStyle;
  SavedState^.FontColor       := FFontColor;
  SavedState^.Font            := FFont;
  SavedState^.CurFontHandle   := FCurFontHandle;
  FStateStackpointer          := SavedState;
  { !!!: This is very dangerous! Some of the FCurXXX variables are not saved in
    SavedState, which might result in graphics errors under certain
    circumstances. Better try to remove SaveDC/RestoreDC completely. }
  Windows.SaveDC(Handle);
end;


procedure TGDICanvas.RestoreState;
var
  SavedState: PGDICanvasState;
begin
  Windows.RestoreDC(Handle, -1);

  SavedState          := FStateStackpointer;
  FStateStackpointer  := SavedState^.Prev;
  Matrix              := SavedState^.Matrix;
  FColor              := SavedState^.Color;
  FPenColor           := SavedState^.PenColor;
  FPenLineStyle       := SavedState^.PenLineStyle;
  FFontColor          := SavedState^.FontColor;
  FCurFontHandle      := SavedState^.CurFontHandle;
  SetFont(SavedState^.Font);
  Dispose(SavedState);
end;


procedure TGDICanvas.EmptyClipRect;
begin
  Windows.IntersectClipRect(Handle, 0, 0, 0, 0);
end;


function TGDICanvas.DoExcludeClipRect(const ARect: TRect): Boolean;
begin
  with ARect do
    Result :=
      Windows.ExcludeClipRect(Handle, Left, Top, Right, Bottom) <> NULLREGION;
end;


function TGDICanvas.DoIntersectClipRect(const ARect: TRect): Boolean;
begin
  with ARect do
    Result :=
      Windows.IntersectClipRect(Handle, Left, Top, Right, Bottom) <> NULLREGION
end;


function TGDICanvas.DoUnionClipRect(const ARect: TRect): Boolean;
var
  Region: HRGN;
begin
  with ARect do
    Region := Windows.CreateRectRgn(Left, Top, Right, Bottom);
  {$IFNDEF WinCE}
  Result := Windows.ExtSelectClipRgn(Handle, Region, RGN_OR) <> NULLREGION;
  {$ENDIF}
  Windows.DeleteObject(Region);
end;


function TGDICanvas.DoGetClipRect: TRect;
var
  Rect: Windows.Rect;
begin
  Windows.GetClipBox(Handle, @Rect);
  Result := TRect(Rect);
end;


function TGDICanvas.MapColor(const AColor: TGfxColor): TGfxPixel;
begin
{  Result := Windows.GetNearestColor(Handle, RGB(AColor.Red div 257,
    AColor.Green div 257, AColor.Blue div 257));}
  Result := RGB(AColor.Red, AColor.Green, AColor.Blue);
end;


procedure TGDICanvas.DoSetColor(AColor: TGfxPixel);
begin
  FColor := AColor;
end;


procedure TGDICanvas.SetFont(AFont: TFCustomFont);
begin
  if AFont = FFont then
    exit;

  FFont := AFont;

  if not Assigned(AFont) then
    FFontHandle := FDefaultFontHandle
  else
  begin
    if not AFont.InheritsFrom(TGDIFont) then
      raise EGfxError.CreateFmt(SGDICanvasInvalidFontClass, [AFont.ClassName]);
    FFontHandle := TGDIFont(AFont).Handle;
  end;
end;


procedure TGDICanvas.SetLineStyle(ALineStyle: TGfxLineStyle);
begin
  FLineStyle := ALineStyle;
end;


procedure TGDICanvas.DoDrawArc(const ARect: TRect; StartAngle, EndAngle: Single);
var
  SX, SY, EX, EY : Longint;
begin
  {$Warning DoDrawArc needs testing. }
  Angles2Coords(ARect.Left, ARect.Top, ARect.Right - ARect.Left,
      ARect.Bottom - ARect.Top, StartAngle, EndAngle, SX, SY, EX, EY);
  {$ifndef wince}
  Windows.Arc(Handle, ARect.Left, ARect.Top, ARect.Right,
      ARect.Bottom, SX, SY, EX, EY)
  {$endif}
end;


procedure TGDICanvas.DoDrawCircle(const ARect: TRect);
begin
  {$Warning DoDrawCircle needs testing. }
  NeedPen;
  Windows.Ellipse(Handle, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;


procedure TGDICanvas.DoDrawLine(const AFrom, ATo: TPoint);
begin
  NeedPen;
  Windows.MoveToEx(Handle, AFrom.x, AFrom.y, nil);
  Windows.LineTo(Handle, ATo.x, ATo.y);
end;


procedure TGDICanvas.DoDrawPoint(const APoint: TPoint);
begin
  {$Warning This is not implemented yet. }
  // Use DrawLine() as windows doesn't have a DrawPoint.  It does have a
  // SetPixel() method, but not sure how to use it yet.
  DoDrawLine(APoint, APoint);
//  DoFillRect(Rect(APoint.X, APoint.Y, APoint.X, APoint.Y));
end;


procedure TGDICanvas.DoFillRect(const ARect: TRect);
var
  r: Windows.Rect;
begin
  NeedBrush;
  r := RectToWinRect(ARect);
  Windows.FillRect(Handle, r, FBrush);
end;

procedure TGDICanvas.DoFillTriangle(const P1, P2, P3: TPoint);
var
  pts : array[1..3] of windows.TPoint;
  pt: TPoint;
begin
  pt := Transform(P1);
  pts[1].X := pt.X;   pts[1].Y := pt.Y;
  pt := Transform(P2);
  pts[2].X := pt.X;   pts[2].Y := pt.Y;
  pt := Transform(P3);
  pts[3].X := pt.X;   pts[3].Y := pt.Y;

  NeedBrush;
  {$ifdef wince}
  Windows.Polygon(Handle, @pts[1], 3);
  {$else}
  Windows.Polygon(Handle, pts[1], 3);
  {$endif}
end;


function TGDICanvas.FontCellHeight: Integer;
begin
  NeedFont(False);
  Result := FFontMetrics.tmHeight;
end;


function TGDICanvas.TextExtent(const AText: String): TSize;
var
  WideText: WideString;
  ASize: Windows.SIZE;
begin
  NeedFont(False);

  WideText := Utf8Decode(AText);
  {$ifdef wince}
    Windows.GetTextExtentPoint32(Handle, PWideChar(WideText), Length(WideText), @Result);
  {$else}
    Windows.GetTextExtentPoint32W(Handle, PWideChar(WideText), Length(WideText), ASize);
    Result.cx := ASize.cx;
    Result.cy := ASize.cy;
  {$endif}
end;


procedure TGDICanvas.DoTextOut(const APosition: TPoint; const AText: String);
var
  WideText: WideString;
begin
  NeedFont(True);

  WideText := Utf8Decode(AText);
  {$ifdef wince}
    Windows.ExtTextOut(Handle, APosition.x, APosition.y, ETO_CLIPPED, nil, PWideChar(WideText), Length(WideText), nil)
  {$else}
    Windows.TextOutW(Handle, APosition.x, APosition.y, PWideChar(WideText), Length(WideText))
  {$endif}
end;


procedure TGDICanvas.DoCopyRect(ASource: TFCustomCanvas; const ASourceRect: TRect;
  const ADestPos: TPoint);
begin
  if not ASource.InheritsFrom(TGDICanvas) then
    raise EGDIError.CreateFmt(SIncompatibleCanvasForBlitting,
      [ASource.ClassName, Self.ClassName]);

  Windows.BitBlt(
    Handle, ADestPos.x, ADestPos.y, ASourceRect.Right - ASourceRect.Left,
    ASourceRect.Bottom - ASourceRect.Top,
    TGDICanvas(ASource).Handle, ASourceRect.Left, ASourceRect.Top,
    SRCCOPY);
end;


procedure TGDICanvas.DoMaskedCopyRect(ASource, AMask: TFCustomCanvas;
  const ASourceRect: TRect; const AMaskPos, ADestPos: TPoint);
var
  w, h: Integer;
  SourceBitmap, AndObjectBitmap, AndMemBitmap, SaveBitmap,
    OldSourceBitmap, OldAndObjectBitmap, OldAndMemBitmap,
    OldSaveBitmap: HBITMAP;
  SourceDC, MemDC, ObjectDC, SaveDC: HDC;
begin
  if not ASource.InheritsFrom(TGDICanvas) then
    raise EGDIError.CreateFmt(SIncompatibleCanvasForBlitting,
      [ASource.ClassName, Self.ClassName]);

  if not AMask.InheritsFrom(TGDICanvas) then
    raise EGDIError.CreateFmt(SIncompatibleCanvasForBlitting,
      [AMask.ClassName, Self.ClassName]);

  w := ASourceRect.Right - ASourceRect.Left;
  h := ASourceRect.Bottom - ASourceRect.Top;

  // See http://support.microsoft.com/support/kb/articles/Q79/2/12.ASP

  SourceDC := Windows.CreateCompatibleDC(Handle);
  ObjectDC := Windows.CreateCompatibleDC(Handle);
  MemDC := Windows.CreateCompatibleDC(Handle);
  SourceBitmap := Windows.CreateCompatibleBitmap(Handle, w, h);
  AndObjectBitmap := Windows.CreateCompatibleBitmap(ObjectDC, w, h);
  AndMemBitmap := Windows.CreateCompatibleBitmap(Handle, w, h);
  OldSourceBitmap := Windows.SelectObject(SourceDC, SourceBitmap);
  OldAndObjectBitmap := Windows.SelectObject(ObjectDC, AndObjectBitmap);
  OldAndMemBitmap := Windows.SelectObject(MemDC, AndMemBitmap);

  Windows.BitBlt(SourceDC, 0, 0, w, h,
    TGDICanvas(ASource).Handle, ASourceRect.Left, ASourceRect.Top, SRCCOPY);
  Windows.BitBlt(MemDC, 0, 0, w, h, Handle, ADestPos.x, ADestPos.y, SRCCOPY);

  // !!!: Find a ROP for replacing the following 2 Blits with a single one:
  Windows.BitBlt(ObjectDC, 0, 0, w, h,
    TGDICanvas(AMask).Handle, AMaskPos.x, AMaskPos.y, NOTSRCCOPY);
  Windows.BitBlt(MemDC, 0, 0, w, h, ObjectDC, 0, 0, SRCAND);

  Windows.BitBlt(SourceDC, 0, 0, w, h,
    TGDICanvas(AMask).Handle, AMaskPos.x, AMaskPos.y, SRCAND);
  Windows.BitBlt(MemDC, 0, 0, w, h, SourceDC, 0, 0, SRCPAINT);
  // Copy the result to the screen
  Windows.BitBlt(Handle, ADestPos.x, ADestPos.y, w, h, MemDC, 0, 0, SRCCOPY);

  // Clean up
  Windows.DeleteObject(Windows.SelectObject(ObjectDC, OldAndObjectBitmap));
  Windows.DeleteObject(Windows.SelectObject(MemDC, OldAndMemBitmap));
  Windows.DeleteObject(Windows.SelectObject(SourceDC, OldSourceBitmap));
  Windows.DeleteDC(MemDC);
  Windows.DeleteDC(ObjectDC);
  Windows.DeleteDC(SourceDC);
end;


procedure TGDICanvas.DoDrawImageRect(AImage: TFCustomBitmap; ASourceRect: TRect;
  const ADestPos: TPoint);
var
  MemDC: HDC;
  OldBitmap: HBITMAP;
  GDIPal: array of RGBQUAD;
  i: Integer;
begin
  ASSERT(AImage.InheritsFrom(TGDIBitmap));
  {$IFDEF Debug}
  ASSERT(not TGDIBitmap(AImage).IsLocked);
  {$ENDIF}

  MemDC := Windows.CreateCompatibleDC(Handle);
  OldBitmap := Windows.SelectObject(MemDC, AImage.Handle);

  // Set the color palette, if present
  if Assigned(AImage.Palette) then
  begin
    SetLength(GDIPal,AImage.Palette.EntryCount * SizeOf(RGBQUAD));
    for i := 0 to AImage.Palette.EntryCount - 1 do
      with AImage.Palette.Entries[i] do
      begin
        GDIPal[i].rgbRed := Red;
        GDIPal[i].rgbGreen := Green;
        GDIPal[i].rgbBlue := Blue;
        GDIPal[i].rgbReserved := 0;
      end;
    Windows.SetDIBColorTable(MemDC, 0, AImage.Palette.EntryCount, GDIPal[0]);
  end;

  with ASourceRect do
    Windows.BitBlt(Handle, ADestPos.x, ADestPos.y, Right - Left, Bottom - Top,
      MemDC, Left, Top, SRCCOPY);

  Windows.SelectObject(MemDC, OldBitmap);
  Windows.DeleteDC(MemDC);
end;


procedure TGDICanvas.NeedBrush;
begin
  if (FBrush = 0) or (FBrushColor <> FColor) then
  begin
    if FBrush <> 0 then
    begin
      Windows.SelectObject(Handle, FOldBrush);
      Windows.DeleteObject(FBrush);
    end;
    FBrushColor := FColor;
    FBrush := Windows.CreateSolidBrush(FBrushColor);
    FOldBrush := Windows.SelectObject(Handle, FBrush);
  end;
end;


procedure TGDICanvas.NeedPen;
begin
  if (FPen = 0) or (FPenColor <> FColor) or (FPenLineStyle <> FLineStyle) then
  begin
    if FPen <> 0 then
    begin
      Windows.SelectObject(Handle, FOldPen);
      Windows.DeleteObject(FPen);
    end;
    FPenColor := FColor;
    FPenLineStyle := FLineStyle;
    case FPenLineStyle of
      lsSolid:
        FPen := Windows.CreatePen(PS_SOLID, 0, FPenColor);
    end;
    FOldPen := Windows.SelectObject(Handle, FPen);
  end;
end;


procedure TGDICanvas.NeedFont(ANeedFontColor: Boolean);
begin
  if FCurFontHandle <> FFontHandle then
  begin
    Windows.SelectObject(Handle, FFontHandle);
    { TODO : Store the font metrics in TGDIFont }
    Windows.GetTextMetrics(Handle, @FFontMetrics);
    FCurFontHandle := FFontHandle;
  end;
  if ANeedFontColor then
    NeedFontColor;
end;


procedure TGDICanvas.NeedFontColor;
begin
  if FFontColor <> FColor then
  begin
    FFontColor := FColor;
    Windows.SetTextColor(Handle, FFontColor);
  end;
end;


procedure TGDICanvas.Resized(NewWidth, NewHeight: Integer);
begin
  FWidth := NewWidth;
  FHeight := NewHeight;
end;


{ TGDIWindowCanvas }

constructor TGDIWindowCanvas.Create(AWnd: HWND);
begin
  FWnd := AWnd;
  inherited Create();
  SetHandle(Windows.GetDC(FWnd));
end;


destructor TGDIWindowCanvas.Destroy;
begin
  inherited Destroy;
  if Handle <> 0 then
    Windows.ReleaseDC(FWnd, Handle);
end;


{ TGDIBitmapCanvas }

constructor TGDIBitmapCanvas.Create(ABitmap: HBITMAP; AWidth, AHeight: Integer);
begin
  ASSERT(ABitmap <> 0);
  FBitmap := ABitmap;
  inherited Create();
  SetHandle(Windows.CreateCompatibleDC(0));
  FWidth := AWidth;
  FHeight := AHeight;
  FOldBitmap := Windows.SelectObject(Handle, Bitmap);
end;


destructor TGDIBitmapCanvas.Destroy;
begin
  Windows.SelectObject(Handle, FOldBitmap);
  Windows.DeleteObject(Bitmap);
  Windows.DeleteDC(Handle);
  inherited Destroy;
end;


{ TGDIBitmap }

constructor TGDIBitmap.Create(AWidth, AHeight: Integer; APixelFormat: TGfxPixelFormat);
var
  BitmapInfo: PBitmapInfo;
  Color: PRGBQUAD;
  TempDC: HDC;
begin
  inherited Create(AWidth, AHeight, APixelFormat);

  case APixelFormat.FormatType of
    ftMono:
      begin
        FStride := (AWidth + 7) shr 3;
        GetMem(BitmapInfo, SizeOf(TBitmapInfoHeader) + 2 * SizeOf(RGBQUAD));
        BitmapInfo^.bmiHeader.biClrUsed := 2;
        Color               := @BitmapInfo^.bmiColors[0];
        Color^.rgbRed       := 0;
        Color^.rgbGreen     := 0;
        Color^.rgbBlue      := 0;
        Color^.rgbReserved  := 0;
        Inc(Color);
        Color^.rgbRed       := 255;
        Color^.rgbGreen     := 255;
        Color^.rgbBlue      := 255;
        Color^.rgbReserved  := 0;
      end;
    ftPal4, ftPal4A:
      begin
        FStride := (AWidth + 1) shr 1;
        GetMem(BitmapInfo, SizeOf(TBitmapInfoHeader) + 16 * SizeOf(RGBQUAD));
        BitmapInfo^.bmiHeader.biClrUsed := 0;
      end;
    ftPal8, ftPal8A:
      begin
        FStride := AWidth;
        GetMem(BitmapInfo, SizeOf(TBitmapInfoHeader) + 256 * SizeOf(RGBQUAD));
        BitmapInfo^.bmiHeader.biClrUsed := 0;
      end;
    else
    begin
      FStride := AWidth * (FormatTypeBPPTable[APixelFormat.FormatType] shr 3);
      GetMem(BitmapInfo, SizeOf(TBitmapInfoHeader));
      BitmapInfo^.bmiHeader.biClrUsed := 0;
    end;
  end;
  // The stride is always a multiple of 4
  FStride := (FStride + 3) and not 3;

  with BitmapInfo^.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := AWidth;
    biHeight := AHeight;
    biPlanes := 1;
    biBitCount := FormatTypeBPPTable[APixelFormat.FormatType];
    biCompression := 0;
    biSizeImage := 4 * AHeight * AWidth;
    biXPelsPerMeter := 0;
    biYPelsPerMeter := 0;
    biClrImportant := 0;
  end;

  FData := nil;

  TempDC := GetDC(0);
  FHandle := Windows.CreateDIBSection(TempDC, BitmapInfo^, DIB_RGB_COLORS, FData, 0, 0);
  ReleaseDC(0, TempDC);
  
  FreeMem(BitmapInfo);
end;


destructor TGDIBitmap.Destroy;
begin
  if Handle <> 0 then
    Windows.DeleteObject(Handle);
  inherited Destroy;
end;


procedure TGDIBitmap.Lock(out AData: Pointer; out AStride: LongWord);
begin
  ASSERT(not IsLocked);
  IsLocked := True;
  AData := Data;
  AStride := Stride;
  {$ifndef wince}
  Windows.GdiFlush;
  {$endif}
end;

procedure TGDIBitmap.Unlock;
begin
  ASSERT(IsLocked);
  IsLocked := False;
end;


{ TGDIScreen }

procedure TGDIScreen.SetMousePos(const NewPos: TPoint);
begin
  Windows.SetCursorPos(NewPos.x, NewPos.y);
end;

function TGDIScreen.GetMousePos: TPoint;
var
  Pos: Windows.TPoint;
begin
  Windows.GetCursorPos(Pos);
  
  Result.x := Pos.x;
  Result.y := Pos.y;
end;

constructor TGDIScreen.Create;
begin
  inherited Create;
  
end;

function TGDIScreen.CreateBitmapCanvas(AWidth, AHeight: Integer
  ): TFCustomCanvas;
var
  TempDC: HDC;
begin
  TempDC := Windows.GetDC(0);
  Result := TGDIBitmapCanvas.Create(
    Windows.CreateCompatibleBitmap(TempDC, AWidth, AHeight), AWidth, AHeight);
  Windows.ReleaseDC(0, TempDC);
end;

function TGDIScreen.CreateMonoBitmapCanvas(AWidth, AHeight: Integer): TFCustomCanvas;
var
  TempDC: HDC;
begin
  TempDC := Windows.CreateCompatibleDC(0);
  Result := TGDIBitmapCanvas.Create(
    Windows.CreateCompatibleBitmap(TempDC, AWidth, AHeight), AWidth, AHeight);
  Windows.DeleteDC(TempDC);
end;

{ TGDIApplication }

constructor TGDIApplication.Create;
begin
  inherited Create;
  
end;


{ It´s not the job of the Application object to clean up undestroyed forms
  This can generate crashes }
destructor TGDIApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TGDIApplication.Initialize(ADisplayName: String);
begin

end;

procedure TGDIApplication.Run;
var
  Msg: TMsg;
begin
  inherited Run;

  while Windows.GetMessage(@Msg, 0, 0, 0) and
   (not (QuitWhenLastWindowCloses and (Forms.Count = 0))) and
   (DoBreakRun = False) do
  begin
    Windows.TranslateMessage(@msg);
    Windows.DispatchMessage(@msg);
  end;
  
  DoBreakRun := False;
end;


procedure TGDIApplication.Quit;
begin
  DoBreakRun := True;
end;


{ TGDIWindow }

function fpGFXWindowProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall;
var
  Window: TGDIWindow;
  PaintStruct: TPaintStruct;
  r: TRect;
begin
  Result := 0;

  if uMsg = WM_CREATE then
  begin
    Window := TGDIWindow(PCreateStruct(lParam)^.lpCreateParams);
    Window.FHandle := hwnd;
    Windows.SetWindowLong(hwnd, GWL_USERDATA, LongWord(Window));
  end else
    Window := TGDIWindow(Windows.GetWindowLong(hwnd, GWL_USERDATA));

  if Assigned(Window) then
  begin
    case uMsg of
     { Messages }
     WM_CREATE:
     begin
       Window.EvCreate();
     end;
     WM_DESTROY:
     begin
       if Window.Handle <> 0 then Window.Free;
     end;
     WM_GetMinMaxInfo:
     begin
       if Window.FMinSize.cx > 0 then
        PMinMaxInfo(lParam)^.ptMinTrackSize.x := Window.FMinSize.cx;
       if Window.FMinSize.cy > 0 then
        PMinMaxInfo(lParam)^.ptMinTrackSize.y := Window.FMinSize.cy;
       if Window.FMaxSize.cx > 0 then
        PMinMaxInfo(lParam)^.ptMaxTrackSize.x := Window.FMaxSize.cx;
       if Window.FMaxSize.cy > 0 then
        PMinMaxInfo(lParam)^.ptMaxTrackSize.y := Window.FMaxSize.cy;
     end;
     WM_Activate:
     begin
       if wParam = WA_INACTIVE then Window.EvFocusOut()
       else Window.EvFocusIn();
     end;
     WM_Paint:
     begin
       Windows.BeginPaint(Window.Handle, @PaintStruct);
       Window.FPaintStruct := PaintStruct;
       Window.EvInternalPaint();
       Windows.EndPaint(Window.Handle, @PaintStruct);
     end;
     WM_ShowWindow:
     begin
       if wParam <> 0 then
       begin
         Window.EvFocusIn();

         Window.EvShow();

         GFApplication.AddWindow(Window);
       end
       else
       begin
         Window.EvHide();

         GFApplication.RemoveWindow(Window);
       end;
     end;
     WM_Move:
     begin
       if (LoWord(lParam) <> Window.Left) or (HiWord(lParam) <> Window.Top) then
       begin
         Window.FLeft := LoWord(lParam);
         Window.FTop := HiWord(lParam);

         Window.EvMove();
       end;
     end;
     WM_Size:
     begin
       if (LoWord(lParam) <> Window.ClientWidth) or (HiWord(lParam) <> Window.ClientHeight) then
       begin
         Windows.GetWindowRect(Window.Handle, @r);
         Window.FWidth := r.Right - r.Left;
         Window.FHeight := r.Bottom - r.Top;
         Windows.GetClientRect(Window.Handle, @r);
         Window.FClientWidth := LoWord(lParam);
         Window.FClientHeight := HiWord(lParam);

         Window.EvResize();
       end;
     end;
     { Input messages }
     WM_LButtonDown:
     begin
       if Window.FMouseInWindow and not Window.FHasFocus then
        Windows.SetActiveWindow(Window.Handle);
        
       if Window.DoMouseEnterLeaveCheck(uMsg, wParam, lParam) then
       begin
         Window.EvMousePressed(mbLeft,
          Point(LoWord(lparam), HiWord(lParam)));
       end;
     end;
     WM_LButtonUp:
     begin
       if Window.DoMouseEnterLeaveCheck(uMsg, wParam, lParam) then
       begin
         Window.EvMouseReleased(mbLeft,
          Point(LoWord(lparam), HiWord(lParam)));
       end;
     end;
     WM_RButtonDown:
     begin
       if Window.FMouseInWindow and not Window.FHasFocus then
        Windows.SetActiveWindow(Window.Handle);
        
       if Window.DoMouseEnterLeaveCheck(uMsg, wParam, lParam) then
       begin
         Window.EvMousePressed(mbRight,
          Point(LoWord(lparam), HiWord(lParam)));
       end;
     end;
     WM_RButtonUp:
     begin
       if Window.DoMouseEnterLeaveCheck(uMsg, wParam, lParam) then
       begin
         Window.EvMouseReleased(mbRight,
          Point(LoWord(lparam), HiWord(lParam)));
       end;
     end;
     WM_MButtonDown:
     begin
       if Window.FMouseInWindow and not Window.FHasFocus then
        Windows.SetActiveWindow(Window.Handle);

       if Window.DoMouseEnterLeaveCheck(uMsg, wParam, lParam) then
       begin
         Window.EvMousePressed(mbMiddle,
          Point(LoWord(lparam), HiWord(lParam)));
       end;
     end;
     WM_MButtonUp:
     begin
       if Window.DoMouseEnterLeaveCheck(uMsg, wParam, lParam) then
       begin
         Window.EvMouseReleased(mbMiddle,
          Point(LoWord(lparam), HiWord(lParam)));
       end;
     end;
     WM_MouseMove:
     begin
       if Window.DoMouseEnterLeaveCheck(uMsg, wParam, lParam) then
       begin
         Window.EvMouseMove(Point(LoWord(lParam), HiWord(lParam)));
       end;
     end;
     WM_MouseWheel:
     begin
       if Window.DoMouseEnterLeaveCheck(uMsg, wParam, lParam) then
       begin
//    Windows.ScreenToClient(Handle, @pt);
         Window.EvMouseWheel(
          SmallInt(HiWord(wParam)) / -120.0,
          Point(LoWord(lparam), HiWord(lparam))
          );
       end;
     end;
     WM_KeyDown, WM_SysKeyDown:
     begin
       Window.EvKeyPressed(VirtKeyToKeycode(wParam));

       if (wParam = $2e {VK_DELETE}) then Window.EvKeyChar(#127);
     end;
     WM_KeyUp, WM_SysKeyUp:
     begin
       Window.EvKeyReleased(VirtKeyToKeycode(wParam));
     end;
     WM_Char, WM_SysChar:
     begin
       Window.EvKeyChar(Chr(wParam));
     end;
    else
      if UnicodeEnabledOS then Result := Windows.DefWindowProcW(hwnd, uMsg, wParam, lParam)
      else Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
    end;
  end
  else
    if UnicodeEnabledOS then Result := Windows.DefWindowProcW(hwnd, uMsg, wParam, lParam)
    else Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
end;


constructor TGDIWindow.Create(AParent: TFCustomWindow; AWindowOptions: TFWindowOptions);
begin
  inherited Create(AParent, AWindowOptions);

  CreateWindow;

  { Creates the Canvas }

  FCanvas := TGDICanvas.Create();
end;


destructor TGDIWindow.Destroy;
var
  OldHandle: HWND;
begin
  if Assigned(OnClose) then
    OnClose(Self);

  Canvas.Free;

  if Handle <> 0 then
  begin
    OldHandle := Handle;
    FHandle := 0;
    Windows.DestroyWindow(OldHandle);
  end;

  GFApplication.RemoveWindow(Self);

  // Are we the last window for our owning application?
  if (GFApplication.QuitWhenLastWindowCloses and (GFApplication.Forms.Count = 0)) then
    Windows.PostQuitMessage(0);

  inherited Destroy;
end;


procedure TGDIWindow.SetPosition(const APosition: TPoint);
begin
  Windows.SetWindowPos(Handle, 0, APosition.x, APosition.y, 0, 0,
    SWP_NOSIZE or SWP_NOZORDER);
end;


procedure TGDIWindow.SetSize(const ASize: TSize);
begin
  if (ASize.cx <> Width) or (ASize.cy <> Height) then
    Windows.SetWindowPos(Handle, 0, 0, 0, ASize.cx, ASize.cy,
      SWP_NOMOVE or SWP_NOZORDER);
end;


procedure TGDIWindow.SetMinMaxSize(const AMinSize, AMaxSize: TSize);
begin
  FMinSize := AMinSize;
  FMaxSize := AMaxSize;
  UpdateWindowButtons;
end;


procedure TGDIWindow.SetClientSize(const ASize: TSize);
var
  r: Windows.Rect;
begin
  if (ASize.cx <> ClientWidth) or (ASize.cx <> ClientHeight) then
  begin
    r.Left := 0;
    r.Top := 0;
    r.Right := ASize.cx;
    r.Bottom := ASize.cy;
    Windows.AdjustWindowRectEx(@r, FWindowStyle, False, FWindowStyleEx);
    SetSize(Size(WinRectToRect(r)));
  end;
end;


procedure TGDIWindow.SetMinMaxClientSize(const AMinSize, AMaxSize: TSize);
var
  Rect: Windows.Rect;
begin
  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := AMinSize.cx;
  Rect.Bottom := AMinSize.cy;
  Windows.AdjustWindowRectEx(@Rect, FWindowStyle, False, FWindowStyleEx);
  if AMinSize.cx > 0 then
    FMinSize.cx := Rect.Right - Rect.Left
  else
    FMinSize.cx := 0;
  if AMinSize.cy > 0 then
    FMinSize.cy := Rect.Bottom - Rect.Top
  else
    FMinSize.cy := 0;

  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := AMaxSize.cx;
  Rect.Bottom := AMaxSize.cy;
  Windows.AdjustWindowRectEx(@Rect, FWindowStyle, False, FWindowStyleEx);
  if AMaxSize.cx > 0 then
    FMaxSize.cx := Rect.Right - Rect.Left
  else
    FMaxSize.cx := 0;
  if AMaxSize.cy > 0 then
    FMaxSize.cy := Rect.Bottom - Rect.Top
  else
    FMaxSize.cy := 0;

  UpdateWindowButtons;
end;


procedure TGDIWindow.Show;
var
  i: integer;
begin
  Windows.ShowWindow(Handle, SW_SHOWNORMAL);
  Windows.UpdateWindow(Handle);
  for i := 0 to Pred(ChildWindows.Count) do
    TGDIWindow(ChildWindows.Items[i]).Show;
end;


procedure TGDIWindow.Invalidate;
begin
  Windows.InvalidateRect(Handle, nil, True);
end;


procedure TGDIWindow.CaptureMouse;
begin
  if FHasMouseCapture then
    exit;

  FHasMouseCapture := True;

  if not FMouseInWindow then
  begin
    FMouseInWindow := True;
    Windows.SetCapture(Handle);
  end;
end;


procedure TGDIWindow.ReleaseMouse;
begin
  if FHasMouseCapture then
  begin
    FHasMouseCapture := False;
    if not FMouseInWindow then
    begin
      Windows.ReleaseCapture;
    end;
  end;
end;

procedure TGDIWindow.EvCreate;
begin
  if Assigned(OnCreate) then OnCreate(Self);
end;

procedure TGDIWindow.EvFocusIn;
begin
  FHasFocus := True;
  if Assigned(OnFocusIn) then OnFocusIn(Self);
end;

procedure TGDIWindow.EvFocusOut;
begin
  FHasFocus := False;
  if Assigned(OnFocusOut) then OnFocusOut(Self);
end;

procedure TGDIWindow.EvHide;
begin
  if Assigned(OnHide) then OnHide(Self);
end;

procedure TGDIWindow.EvKeyPressed(AKey: Word);
var
  vEvent: TFEvent;
begin
  if Assigned(OnKeyPressed) then OnKeyPressed(Self, AKey, GetKeyboardShiftState)
  else if Assigned(Parent) then
  begin
    vEvent.EventType := etKeyPressed;
    vEvent.Key := AKey;
    Parent.ProcessEvent(vEvent);
  end;
end;

procedure TGDIWindow.EvKeyReleased(AKey: Word);
var
  vEvent: TFEvent;
begin
  if Assigned(OnKeyReleased) then OnKeyReleased(Self, AKey, GetKeyboardShiftState)
  else if Assigned(Parent) then
  begin
    vEvent.EventType := etKeyReleased;
    vEvent.Key := AKey;
    Parent.ProcessEvent(vEvent);
  end;
end;

procedure TGDIWindow.EvKeyChar(AKeyChar: Char);
var
  vEvent: TFEvent;
begin
  if Assigned(OnKeyChar) then OnKeyChar(Self, AKeyChar)
  else if Assigned(Parent) then
  begin
    vEvent.EventType := etKeyChar;
    vEvent.KeyChar := AKeyChar;
    Parent.ProcessEvent(vEvent);
  end;
end;

procedure TGDIWindow.EvMouseEnter(const AMousePos: TPoint);
var
  vEvent: TFEvent;
begin
  if Assigned(OnMouseEnter) then
   OnMouseEnter(Self, GetKeyboardShiftState, AMousePos)
  else if Assigned(Parent) then
  begin
    vEvent.EventType := etMouseEnter;
    vEvent.MousePos := AMousePos;
    Parent.ProcessEvent(vEvent);
  end;
end;

procedure TGDIWindow.EvMouseLeave;
var
  vEvent: TFEvent;
begin
  if Assigned(OnMouseLeave) then OnMouseLeave(Self)
  else if Assigned(Parent) then
  begin
    vEvent.EventType := etMouseLeave;
    Parent.ProcessEvent(vEvent);
  end;
end;

procedure TGDIWindow.EvMousePressed(AButton: TMouseButton;
 const AMousePos: TPoint);
var
  vEvent: TFEvent;
begin
  if Assigned(OnMousePressed) then
   OnMousePressed(Self, AButton, GetKeyboardShiftState, AMousePos)
  else if Assigned(Parent) then
  begin
    vEvent.EventType := etMousePressed;
    vEvent.MousePos := AMousePos;
    vEvent.MouseButton := AButton;
    Parent.ProcessEvent(vEvent);
  end;
end;

procedure TGDIWindow.EvMouseReleased(AButton: TMouseButton;
  const AMousePos: TPoint);
var
  vEvent: TFEvent;
begin
  if Assigned(OnMouseReleased) then
   OnMouseReleased(Self, AButton, GetKeyboardShiftState, AMousePos)
  else if Assigned(Parent) then
  begin
    vEvent.EventType := etMouseReleased;
    vEvent.MousePos := AMousePos;
    vEvent.MouseButton := AButton;
    Parent.ProcessEvent(vEvent);
  end;
end;

procedure TGDIWindow.EvMouseMove(const AMousePos: TPoint);
var
  vEvent: TFEvent;
begin
  if Assigned(OnMouseMove) then
   OnMouseMove(Self, GetKeyboardShiftState, AMousePos)
  else if Assigned(Parent) then
  begin
    vEvent.EventType := etMouseMove;
    vEvent.MousePos := AMousePos;
    Parent.ProcessEvent(vEvent);
  end;
end;

procedure TGDIWindow.EvMouseWheel(AWheelDelta: Single; const AMousePos: TPoint);
var
  vEvent: TFEvent;
begin
  if Assigned(OnMouseWheel) then
    OnMouseWheel(Self, GetKeyboardShiftState, AWheelDelta, AMousePos)
  else if Assigned(Parent) then
  begin
    vEvent.EventType := etMouseMove;
    vEvent.WheelDelta := AWheelDelta;
    vEvent.MousePos := AMousePos;
    Parent.ProcessEvent(vEvent);
  end;
end;

{ Because the painting code is executed on the middle of the processing
  of the event, we have an internal paint method, which allows descending
  objects to override the EvPaint function like they would do with other
  event functions.

  This frees the OnPaint event for use by the users of the components }
procedure TGDIWindow.EvInternalPaint;
var
  rect: TRect;
  OldBitmap, NewBitmap: HBITMAP;
  hdcMem: HDC;
begin
  rect := FPaintStruct.rcPaint;

  { It is necessary to create a bitmap and select it to implement
    double buffering. If we just create a DC and don't select a bitmap,
    there is no memory where to store the painting }

  hdcMem := CreateCompatibleDC(FPaintStruct.hdc);

  NewBitmap := Windows.CreateCompatibleBitmap(FPaintStruct.hdc, Width, Height);

  OldBitmap := HBITMAP(SelectObject(hdcMem, NewBitmap));

  FCanvas.SetHandle(hdcMem);

  { Execution of user paint code }

  EvPaint;

  { Flushes the result to the screen }

  Windows.BitBlt(
   FPaintStruct.hdc,       // handle to destination DC
   rect.Left,              // x-coord of destination upper-left corner
   rect.Top,               // y-coord of destination upper-left corner
   rect.Left + rect.Right, // width of destination rectangle
   rect.Top + rect.Bottom, // height of destination rectangle
   FCanvas.Handle,         // handle to source DC
   rect.Left,              // x-coordinate of source upper-left corner
   rect.Top,               // y-coordinate of source upper-left corner
   SRCCOPY                 // raster operation code
  );

  { Clean up }

  SelectObject(hdcMem, OldBitmap);
  DeleteDC(hdcMem);
  DeleteObject(NewBitmap);

  { Resets the canvas handle }

  FCanvas.SetHandle(0);
end;

procedure TGDIWindow.EvPaint;
begin
  if Assigned(OnPaint) then OnPaint(Self);
end;

procedure TGDIWindow.EvMove;
begin
  if Assigned(OnMove) then OnMove(Self);
end;

procedure TGDIWindow.EvResize;
begin
  if Assigned(OnResize) then OnResize(Self);
end;

procedure TGDIWindow.EvShow;
begin
  if Assigned(OnShow) then OnShow(Self);
end;

function TGDIWindow.GetTitle: String;
var
  TextLen: Integer;
  AnsiBuffer: string;
  WideBuffer: WideString;
begin
  if UnicodeEnabledOS then
  begin
    TextLen := Windows.GetWindowTextLengthW(Handle);
    SetLength(WideBuffer, TextLen);
    TextLen := Windows.GetWindowTextW(Handle, @WideBuffer[1], TextLen + 1);
    SetLength(WideBuffer, TextLen);
    Result := Utf8Encode(WideBuffer);
  end
  else
  begin
    TextLen := Windows.GetWindowTextLength(Handle);
    SetLength(AnsiBuffer, TextLen);
    TextLen := Windows.GetWindowText(Handle, @AnsiBuffer[1], TextLen + 1);
    SetLength(AnsiBuffer, TextLen);
    Result := AnsiToUtf8(AnsiBuffer);
  end;
end;

procedure TGDIWindow.SetTitle(const ATitle: String);
begin
  {$ifdef wince}
    Windows.SetWindowText(Handle, PWideChar(Utf8Decode(ATitle)))
  {$else}
  if UnicodeEnabledOS then
    Windows.SetWindowTextW(Handle, PWideChar(Utf8Decode(ATitle)))
  else Windows.SetWindowText(Handle, PChar(Utf8ToAnsi(ATitle)));
  {$endif}
end;


procedure TGDIWindow.DoSetCursor;
const
  CursorTable: array[TFCursor] of Integer = (
    32512,	// crDefault
    0,		  // crNone
    32512,	// crArrow
    32515,	// crCross
    32513,	// crIBeam
    32646,	// crSize
    32645,	// crSizeNS
    32644,	// crSizeWE
    32516,	// crUpArrow
    32514,	// crHourGlass
    32648,	// crNoDrop
    32651);	// crHelp
var
  ID: Integer;
begin
  if FMouseInWindow then
  begin
    ID := CursorTable[Cursor];
    if ID <> 0 then
      Windows.SetCursor(Windows.LoadCursor(0, MAKEINTRESOURCE(ID)))
    else
      Windows.SetCursor(0);
  end;
end;

procedure TGDIWindow.DoSetWindowOptions;
begin
  // implement me
end;

function TGDIWindow.GetHandle: PtrUInt;
begin
//  if FHandle = 0 then CreateWindow;

  Result := FHandle;
end;

procedure TGDIWindow.CreateWindow;
var
  ParentHandle: HWND;
begin
  { Initialize a window class, if necessary }
  if woWindow in WindowOptions then
  begin
    if UnicodeEnabledOS then
    begin
      if not Assigned(WindowClassW.lpfnWndProc) then
      begin
        WindowClassW.style := CS_HREDRAW or CS_VREDRAW;
        WindowClassW.lpfnWndProc := WndProc(@fpGFXWindowProc);
        WindowClassW.hInstance := MainInstance;
        WindowClassW.hIcon := LoadIcon(0, IDI_APPLICATION);
        WindowClassW.hCursor := LoadCursor(0, IDC_ARROW);
        WindowClassW.hbrBackground := 0;
        WindowClassW.lpszClassName := 'fpGFX';
      end;
      Windows.RegisterClassW(@WindowClassW);
    end
    else
    begin
      if not Assigned(WindowClass.lpfnWndProc) then
      begin
        WindowClass.style := CS_HREDRAW or CS_VREDRAW;
        WindowClass.lpfnWndProc := WndProc(@fpGFXWindowProc);
        WindowClass.hInstance := MainInstance;
        WindowClass.hIcon := LoadIcon(0, IDI_APPLICATION);
        WindowClass.hCursor := LoadCursor(0, IDC_ARROW);
        WindowClass.hbrBackground := 0;
        WindowClass.lpszClassName := 'fpGFX';
      end;
      Windows.RegisterClass(@WindowClass);
    end;
  end;

  if Assigned(FParent) then
    ParentHandle := FParent.Handle
  else
    ParentHandle := 0;

  if not (woWindow in FWindowOptions) then FWindowStyle := WS_CHILD
  else if (woBorderless in FWindowOptions) and (woPopUp in FWindowOptions) then FWindowStyle := WS_POPUP
  else if woPopUp in FWindowOptions then FWindowStyle := WS_POPUPWINDOW
  else if woToolWindow in FWindowOptions then FWindowStyle := WS_OVERLAPPEDWINDOW
  else if woChildWindow in FWindowOptions then FWindowStyle := WS_CHILDWINDOW
  else if woBorderless in FWindowOptions then FWindowStyle := WS_OVERLAPPED
  else FWindowStyle := WS_OVERLAPPEDWINDOW;

  if not (woWindow in FWindowOptions) then FWindowStyleEx := 0
  else if woPopUp in FWindowOptions then FWindowStyleEx := WS_EX_TOOLWINDOW
  else if woToolWindow in FWindowOptions then FWindowStyleEx := WS_EX_TOOLWINDOW
  else FWindowStyleEx := WS_EX_APPWINDOW;

  if UnicodeEnabledOS then
   FHandle := Windows.CreateWindowExW(
     FWindowStyleEx,			// extended window style
     'fpGFX',				// registered class name
     'fpGFX Window',			// window name
     FWindowStyle,			// window style
     CW_USEDEFAULT,			// horizontal position of window
     CW_USEDEFAULT,			// vertical position of window
     CW_USEDEFAULT,			// window width
     CW_USEDEFAULT,			// window height
     ParentHandle,			// handle to parent or owner window
     0,					// menu handle or child identifier
     MainInstance,			// handle to application instance
     Self)				// window-creation data
  else
   FHandle := Windows.CreateWindowEx(
     FWindowStyleEx,			// extended window style
     'fpGFX',				// registered class name
     'fpGFX Window',			// window name
     FWindowStyle,			// window style
     CW_USEDEFAULT,			// horizontal position of window
     CW_USEDEFAULT,			// vertical position of window
     CW_USEDEFAULT,			// window width
     CW_USEDEFAULT,			// window height
     ParentHandle,			// handle to parent or owner window
     0,					// menu handle or child identifier
     MainInstance,			// handle to application instance
     Self);				// window-creation data
end;


procedure TGDIWindow.UpdateWindowButtons;
var
  CanMaximize: Boolean;
begin
  if woWindow in FWindowOptions then
  begin
    CanMaximize := (FMaxSize.cx = 0) or (FMaxSize.cy = 0) or
      (FMaxSize.cx > FMinSize.cx) or (FMaxSize.cy > FMinSize.cy);

    if CanMaximize and ((FWindowStyle and WS_MAXIMIZEBOX) = 0) then
      FWindowStyle := FWindowStyle or WS_MAXIMIZEBOX
    else if (not CanMaximize) and
      ((FWindowStyle and WS_MAXIMIZEBOX) <> 0) then
      FWindowStyle := FWindowStyle and not WS_MAXIMIZEBOX;

    Windows.SetWindowLong(Handle, GWL_STYLE, FWindowStyle or
      (Windows.GetWindowLong(Handle, GWL_STYLE) and
      (WS_MAXIMIZE or WS_MINIMIZE or WS_VISIBLE)));	// preserver these bits!
  end;
end;

function TGDIWindow.DoMouseEnterLeaveCheck(uMsg, wParam, lParam: Cardinal): Boolean;

  function CursorInDifferentWindow: Boolean;
  var
    pt: Windows.POINT;
  begin
    pt.x := LoWord(lParam);
    pt.y := HiWord(lParam);

    // only WM_MOUSEWHEEL uses screen coordinates!!!
    if uMsg <> WM_MOUSEWHEEL then
      Windows.ClientToScreen(Handle, @pt);

    Result := WindowFromPoint(pt) <> Handle;
  end;

var
  pt: Windows.POINT;
begin
  if not FMouseInWindow then
  begin
    FMouseInWindow := True;
    DoSetCursor;
    Windows.SetCapture(Handle);
    EvMouseEnter(Point(LoWord(lparam), HiWord(lparam)));
    Result := uMsg <> WM_MOUSEMOVE;
  end
  else
  begin
    pt.x := LoWord(lParam);
    pt.y := HiWord(lParam);
    if uMsg = WM_MOUSEWHEEL then
      Windows.ScreenToClient(Handle, @pt);
    if (pt.x < 0) or (pt.y < 0) or (pt.x >= ClientWidth) or
      (pt.y >= ClientHeight) or CursorInDifferentWindow then
      FMouseInWindow := False;

    if (not FHasMouseCapture) and (not FMouseInWindow) then
    begin
      Windows.ReleaseCapture;
      EvMouseLeave;
      Result := False;
    end
    else
      Result := True;
  end;
end;

{ Helpers }

function RectToWinRect(const ARect: TRect): Windows.Rect;
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Right := ARect.Right;
  Result.Bottom := ARect.Bottom;
end;

function WinRectToRect(const ARect: Windows.Rect): TRect;
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Right := ARect.Right;
  Result.Bottom := ARect.Bottom;
end;


{$INCLUDE gdikeys.inc}

initialization

{$IFDEF WinCE}

  UnicodeEnabledOS := True;

{$ELSE}

  WinVersion.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(WinVersion);

  UnicodeEnabledOS := (WinVersion.dwPlatformID = VER_PLATFORM_WIN32_NT)
   or (WinVersion.dwPlatformID = VER_PLATFORM_WIN32_CE);
{$ENDIF}

end.

