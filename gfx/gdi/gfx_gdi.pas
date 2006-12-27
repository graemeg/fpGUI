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
unit GFX_GDI;

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
    procedure Lock(var AData: Pointer; var AStride: LongWord); override;
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
  private
    DoBreakRun: Boolean;
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
  protected
    WindowClass: TWndClass;
    WindowClassW: TWndClassW;
    FWindowStyle, FWindowStyleEx: LongWord;
    FMouseInWindow, FHasMouseCapture, FHasFocus: Boolean;
    function    GetTitle: String; override;
    procedure   SetTitle(const ATitle: String); override;
    procedure   DoSetCursor; override;
    procedure   UpdateWindowButtons;
    function    DoMouseEnterLeaveCheck(uMsg, wParam, lParam: Cardinal): Boolean;
  public
    constructor Create(AParent: TFCustomWindow; AWindowOptions: TFWindowOptions); override;
    destructor  Destroy; override;
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
    procedure   ProcessEvent(AEvent: TFEvent); override;
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

constructor TGDICanvas.Create(AHandle: HDC);
begin
  inherited Create;
  FHandle := AHandle;
  ASSERT(Handle <> 0);
  FDefaultFontHandle := Windows.GetStockObject(DEFAULT_GUI_FONT);
  FCurFontHandle := FDefaultFontHandle;
  Windows.SelectObject(Handle, FDefaultFontHandle);
  Windows.GetTextMetrics(Handle, @FFontMetrics);
  Windows.SetBkMode(Handle, TRANSPARENT);
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
  Result := Windows.ExtSelectClipRgn(Handle, Region, RGN_OR) <> NULLREGION;
  Windows.DeleteObject(Region);
end;


function TGDICanvas.DoGetClipRect: TRect;
var
  Rect: Windows.Rect;
begin
  Windows.GetClipBox(Handle, Rect);
  Result := TRect(Rect);
end;


function TGDICanvas.MapColor(const AColor: TGfxColor): TGfxPixel;
begin
{  Result := Windows.GetNearestColor(Handle, RGB(AColor.Red div 257,
    AColor.Green div 257, AColor.Blue div 257));}
  Result := RGB(AColor.Red div 257, AColor.Green div 257, AColor.Blue div 257);
end;


procedure TGDICanvas.SetColor_(AColor: TGfxPixel);
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
  Windows.Arc(Handle, ARect.Left, ARect.Top, ARect.Right,
      ARect.Bottom, SX, SY, EX, EY)
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
  Windows.Polygon(Handle, pts, 3);
end;


function TGDICanvas.FontCellHeight: Integer;
begin
  NeedFont(False);
  Result := FFontMetrics.tmHeight;
end;


function TGDICanvas.TextExtent(const AText: String): TSize;
var
  WideText: WideString;
begin
  NeedFont(False);

  WideText := Utf8Decode(AText);
  Windows.GetTextExtentPoint32W(Handle, PWideChar(WideText), Length(WideText), @Result)
end;


procedure TGDICanvas.DoTextOut(const APosition: TPoint; const AText: String);
var
  WideText: WideString;
begin
  NeedFont(True);

  WideText := Utf8Decode(AText);
  Windows.TextOutW(Handle, APosition.x, APosition.y, PWideChar(WideText), Length(WideText))
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
  GDIPal: array of PRGBQUAD;
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
    GetMem(GDIPal, AImage.Palette.EntryCount * SizeOf(RGBQUAD));
    for i := 0 to AImage.Palette.EntryCount - 1 do
      with AImage.Palette.Entries[i] do
      begin
        GDIPal[i].rgbRed := Red div 257;
        GDIPal[i].rgbGreen := Green div 257;
        GDIPal[i].rgbBlue := Blue div 257;
        GDIPal[i].rgbReserved := 0;
      end;
    Windows.SetDIBColorTable(MemDC, 0, AImage.Palette.EntryCount, GDIPal[0]^);
    FreeMem(GDIPal);
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
  inherited Create(Windows.GetDC(FWnd));
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
  inherited Create(Windows.CreateCompatibleDC(0));
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


procedure TGDIBitmap.Lock(var AData: Pointer; var AStride: LongWord);
begin
  ASSERT(not IsLocked);
  IsLocked := True;
  AData := Data;
  AStride := Stride;
  Windows.GdiFlush;
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

function TGDIScreen.CreateMonoBitmapCanvas(AWidth, AHeight: Integer
  ): TFCustomCanvas;
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


destructor TGDIApplication.Destroy;
var
  i: Integer;
begin
  for i := 0 to Forms.Count - 1 do
    TGDIWindow(Forms[i]).Free;

  inherited Destroy;
end;

procedure TGDIApplication.Initialize(ADisplayName: String);
begin

end;

procedure TGDIApplication.Run;
var
  Msg: TMsg;
begin
  DoBreakRun := False;

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
  Event: TFEvent;
  PaintStruct: TPaintStruct;
  r: TRect;
  OldCanvas: TFCustomCanvas;
begin
  Event := TFEvent.Create;

  if uMsg = WM_CREATE then
  begin
    Window := TGDIWindow(PCreateStruct(lParam)^.lpCreateParams);
    Window.FHandle := hwnd;
    Windows.SetWindowLong(hwnd, GWL_USERDATA, LongWord(Window));
  end else
    Window := TGDIWindow(Windows.GetWindowLong(hwnd, GWL_USERDATA));

  if Assigned(Window) then
  begin
    Event.msg     := uMsg;
    Event.wParam  := wParam;
    Event.lParam  := lParam;
    Event.Result  := 0;

    case uMsg of
     { Messages }
     WM_CREATE:
     begin
       Event.EventType := etCreate;
       Window.ProcessEvent(Event);
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
       if wParam = WA_INACTIVE then
       begin
         Event.EventType := etFocusOut;
         Window.ProcessEvent(Event);
       end
       else
       begin
         Event.EventType := etFocusIn;
         Window.ProcessEvent(Event);
       end;
     end;
     WM_Paint:
     begin
       Event.EventType := etPaint;
       Window.ProcessEvent(Event);
     end;
     WM_ShowWindow:
     begin
       if wParam <> 0 then
       begin
         Event.EventType := etFocusIn;
         Window.ProcessEvent(Event);

         Event.EventType := etShow;
         Window.ProcessEvent(Event);
       end
       else
       begin
          Event.EventType := etHide;
          Window.ProcessEvent(Event);
       end;
     end;
     WM_Move:
     begin
       if (LoWord(lParam) <> Window.Left) or (HiWord(lParam) <> Window.Top) then
       begin
         Window.FLeft := LoWord(lParam);
         Window.FTop := HiWord(lParam);

         Event.EventType := etMove;
         Window.ProcessEvent(Event);
       end;
     end;
     WM_Size:
     begin
       if (LoWord(lParam) <> Window.ClientWidth) or (HiWord(lParam) <> Window.ClientHeight) then
       begin
         Windows.GetWindowRect(Window.Handle, r);
         Window.FWidth := r.Right - r.Left;
         Window.FHeight := r.Bottom - r.Top;
         Windows.GetClientRect(Window.Handle, r);
         Window.FClientWidth := LoWord(lParam);
         Window.FClientHeight := HiWord(lParam);
         TGDICanvas(Window.Canvas).Resized(Window.FWidth, Window.FHeight);

         Event.EventType := etResize;
         Window.ProcessEvent(Event);
       end;
     end;
     { Input messages }
     WM_LButtonDown:
     begin
       if Window.FMouseInWindow and not Window.FHasFocus then
        Windows.SetActiveWindow(Window.Handle);
        
       if Window.DoMouseEnterLeaveCheck(uMsg, wParam, lParam) then
       begin
         Event.EventType := etMousePressed;
         Event.MouseButton := mbLeft;
         Window.ProcessEvent(Event);
       end;
     end;
     WM_LButtonUp:
     begin
       if Window.DoMouseEnterLeaveCheck(uMsg, wParam, lParam) then
       begin
         Event.EventType := etMouseReleased;
         Event.MouseButton := mbLeft;
         Window.ProcessEvent(Event);
       end;
     end;
     WM_RButtonDown:
     begin
       if Window.FMouseInWindow and not Window.FHasFocus then
        Windows.SetActiveWindow(Window.Handle);
        
       if Window.DoMouseEnterLeaveCheck(uMsg, wParam, lParam) then
       begin
         Event.EventType := etMousePressed;
         Event.MouseButton := mbRight;
         Window.ProcessEvent(Event);
       end;
     end;
     WM_RButtonUp:
     begin
       if Window.DoMouseEnterLeaveCheck(uMsg, wParam, lParam) then
       begin
         Event.EventType := etMouseReleased;
         Event.MouseButton := mbRight;
         Window.ProcessEvent(Event);
       end;
     end;
     WM_MButtonDown:
     begin
       if Window.FMouseInWindow and not Window.FHasFocus then
        Windows.SetActiveWindow(Window.Handle);

       if Window.DoMouseEnterLeaveCheck(uMsg, wParam, lParam) then
       begin
         Event.EventType := etMousePressed;
         Event.MouseButton := mbMiddle;
         Window.ProcessEvent(Event);
       end;
     end;
     WM_MButtonUp:
     begin
       if Window.DoMouseEnterLeaveCheck(uMsg, wParam, lParam) then
       begin
         Event.EventType := etMouseReleased;
         Event.MouseButton := mbMiddle;
         Window.ProcessEvent(Event);
       end;
     end;
     WM_MouseMove:
     begin
       if Window.DoMouseEnterLeaveCheck(uMsg, wParam, lParam) then
       begin
         Event.EventType := etMouseMove;
         Window.ProcessEvent(Event);
       end;
     end;
     WM_MouseWheel:
     begin
       if Window.DoMouseEnterLeaveCheck(uMsg, wParam, lParam) then
       begin
         Event.EventType := etMouseWheel;
         Window.ProcessEvent(Event);
       end;
     end;
     WM_KeyDown, WM_SysKeyDown:
     begin
       Event.EventType := etKeyPressed;
       Window.ProcessEvent(Event);
     end;
     WM_KeyUp, WM_SysKeyUp:
     begin
       Event.EventType := etKeyReleased;
       Window.ProcessEvent(Event);
     end;
     WM_Char, WM_SysChar:
     begin
       Event.EventType := etKeyChar;
       Window.ProcessEvent(Event);
     end;
    else
      if UnicodeEnabledOS then Result := Windows.DefWindowProcW(hwnd, uMsg, wParam, lParam)
      else Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
    end;
    
    if Result = 0 then Result := Event.Result;
  end
  else
    if UnicodeEnabledOS then Result := Windows.DefWindowProcW(hwnd, uMsg, wParam, lParam)
    else Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);

  Event.Free;
end;


constructor TGDIWindow.Create(AParent: TFCustomWindow; AWindowOptions: TFWindowOptions);
var
  ParentHandle: HWND;
begin
  inherited Create(AParent, AWindowOptions);

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
  
  if Assigned(AParent) then
    ParentHandle := AParent.Handle
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

  FCanvas := TGDIWindowCanvas.Create(Handle);
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

  GFApplication.Forms.Remove(Self);

  // Are we the last window for our owning application?
  if GFApplication.Forms.Count = 0 then
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
    Windows.AdjustWindowRectEx(r, FWindowStyle, False, FWindowStyleEx);
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
  Windows.AdjustWindowRectEx(Rect, FWindowStyle, False, FWindowStyleEx);
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
  Windows.AdjustWindowRectEx(Rect, FWindowStyle, False, FWindowStyleEx);
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


procedure TGDIWindow.Invalidate(const ARect: TRect);
var
  Rect: Windows.Rect;
begin
  Rect.Left     := ARect.Left;
  Rect.Top      := ARect.Top;
  Rect.Right    := ARect.Right;
  Rect.Bottom   := ARect.Bottom;
  Windows.InvalidateRect(Handle, Rect, False);
end;


procedure TGDIWindow.PaintInvalidRegion;
begin
  Windows.UpdateWindow(Handle);
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

procedure TGDIWindow.ProcessEvent(AEvent: TFEvent);
var
  pt: Windows.POINT;
  PaintStruct: TPaintStruct;
  r: Windows.RECT;
  OldCanvas: TFCustomCanvas;
begin
  case AEvent.EventType of
   etCreate:
     begin
       if Assigned(OnCreate) then OnCreate(Self)
       else if Assigned(Parent) then Parent.ProcessEvent(AEvent);
     end;
   etCanClose:
     begin
     end;
   etClose:
     begin
     end;
   etFocusIn:
     begin
       FHasFocus := True;
       if Assigned(OnFocusIn) then OnFocusIn(Self);
     end;
   etFocusOut:
     begin
       FHasFocus := False;
       if Assigned(OnFocusOut) then OnFocusOut(Self);
     end;
   etHide:
     begin
       if Assigned(OnHide) then OnHide(Self);
     end;
   etKeyPressed:
     begin
       if Assigned(OnKeyPressed) then OnKeyPressed(Self, VirtKeyToKeycode(AEvent.wParam), GetKeyboardShiftState)
       else if Assigned(Parent) then Parent.ProcessEvent(AEvent);

       if (AEvent.wParam = $2e {VK_DELETE}) then
       begin
         if Assigned(OnKeyChar) then OnKeyChar(Self, #127)
         else if Assigned(Parent) then Parent.ProcessEvent(AEvent);
       end;
     end;
   etKeyReleased:
     begin
       if Assigned(OnKeyReleased) then OnKeyReleased(Self, VirtKeyToKeycode(AEvent.wParam), GetKeyboardShiftState)
       else if Assigned(Parent) then Parent.ProcessEvent(AEvent);
     end;
   etKeyChar:
     begin
       if Assigned(OnKeyChar) then OnKeyChar(Self, Chr(AEvent.wParam))
       else if Assigned(Parent) then Parent.ProcessEvent(AEvent);
     end;
   etMouseEnter:
     begin
       if Assigned(OnMouseEnter) then
        OnMouseEnter(Self, GetKeyboardShiftState, Point(LoWord(AEvent.lParam), HiWord(AEvent.lParam)))
       else if Assigned(Parent) then Parent.ProcessEvent(AEvent);
     end;
   etMouseLeave:
     begin
       if Assigned(OnMouseLeave) then OnMouseLeave(Self)
       else if Assigned(Parent) then Parent.ProcessEvent(AEvent);
     end;
   etMousePressed:
     begin
       if Assigned(OnMousePressed) then
        OnMousePressed(Self, AEvent.MouseButton, GetKeyboardShiftState, Point(LoWord(AEvent.lparam), HiWord(AEvent.lParam)))
       else if Assigned(Parent) then Parent.ProcessEvent(AEvent);
     end;
   etMouseReleased:
     begin
       if Assigned(OnMouseReleased) then
        OnMouseReleased(Self, AEvent.MouseButton, GetKeyboardShiftState, Point(LoWord(AEvent.lparam), HiWord(AEvent.lParam)))
       else if Assigned(Parent) then Parent.ProcessEvent(AEvent);
     end;
   etMouseMove:
     begin
       if Assigned(OnMouseMove) then
        OnMouseMove(Self, GetKeyboardShiftState, Point(LoWord(AEvent.lparam), HiWord(AEvent.lParam)))
       else if Assigned(Parent) then Parent.ProcessEvent(AEvent);
     end;
   etMouseWheel:
     begin
       if Assigned(OnMouseWheel) then
       begin
         pt.x := LoWord(AEvent.lparam);
         pt.y := HiWord(AEvent.lparam);
         Windows.ScreenToClient(Handle, pt);
         OnMouseWheel(Self, GetKeyboardShiftState, SmallInt(HiWord(AEvent.wParam)) / -120.0,
          Point(pt.x, pt.y));
       end
       else if Assigned(Parent) then Parent.ProcessEvent(AEvent);
     end;
   etPaint:
     begin
       Windows.BeginPaint(Handle, @PaintStruct);
       if Assigned(OnPaint) then
       begin
         r.Left    := PaintStruct.rcPaint.Left;
         r.Top     := PaintStruct.rcPaint.Top;
         r.Right   := PaintStruct.rcPaint.Right;
         r.Bottom  := PaintStruct.rcPaint.Bottom;

         OldCanvas := Canvas;
         FCanvas := TGDICanvas.Create(PaintStruct.hdc);
         OnPaint(Self, r);
         Canvas.Free;
         FCanvas := OldCanvas;
       end;
       Windows.EndPaint(Handle, @PaintStruct);
     end;
   etMove:
     begin
       if Assigned(OnMove) then OnMove(Self);
     end;
   etResize:
     begin
       if Assigned(OnResize) then OnResize(Self);
     end;
   etShow:
     begin
       if Assigned(OnShow) then OnShow(Self);
     end;
  end;
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
  if UnicodeEnabledOS then
    Windows.SetWindowTextW(Handle, PWideChar(Utf8Decode(ATitle)))
  else Windows.SetWindowText(Handle, PChar(Utf8ToAnsi(ATitle)));
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
      Windows.ClientToScreen(Handle, pt);

    Result := WindowFromPoint(pt) <> Handle;
  end;

var
  pt: Windows.POINT;
  Event: TFEvent;
begin
  if not FMouseInWindow then
  begin
    FMouseInWindow := True;
    DoSetCursor;
    Windows.SetCapture(Handle);
    Event := TFEvent.Create;
    try
      Event.lParam := lParam;
      Event.EventType := etMouseEnter;
      ProcessEvent(Event);
    finally
      Event.Free;
    end;
    Result := uMsg <> WM_MOUSEMOVE;
  end else
  begin
    pt.x := LoWord(lParam);
    pt.y := HiWord(lParam);
    if uMsg = WM_MOUSEWHEEL then
      Windows.ScreenToClient(Handle, pt);
    if (pt.x < 0) or (pt.y < 0) or (pt.x >= ClientWidth) or
      (pt.y >= ClientHeight) or CursorInDifferentWindow then
      FMouseInWindow := False;

    if (not FHasMouseCapture) and (not FMouseInWindow) then
    begin
      Windows.ReleaseCapture;
      Event := TFEvent.Create;
      try
        Event.EventType := etMouseLeave;
        ProcessEvent(Event);
      finally
        Event.Free;
      end;
      Result := False;
    end else
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

  WinVersion.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(WinVersion);

  UnicodeEnabledOS := (WinVersion.dwPlatformID = VER_PLATFORM_WIN32_NT)
   or (WinVersion.dwPlatformID = VER_PLATFORM_WIN32_CE);

end.

