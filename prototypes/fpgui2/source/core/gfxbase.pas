unit gfxbase;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TfpgCoord = integer;     // we might use floating point coordinates in the future...
  TfpgColor = longword;    // Always in RRGGBB (Red, Green, Blue) format!!

type
  TWindowType = (wtChild, wtWindow, wtModalForm, wtPopup);

  TWindowAttribute = (waSizeable, waAutoPos, waScreenCenterPos);
  TWindowAttributes = set of TWindowAttribute;

  TMouseCursor = (mcDefault, mcArrow, mcCross, mcIBeam, mcSizeEW, mcSizeNS,
      mcSizeNWSE, mcSizeNESW, mcMove, mcHourGlass);

const
  MOUSE_LEFT       = 1;
  MOUSE_RIGHT      = 2;
  MOUSE_MIDDLE     = 4;


  ss_Shift         = $0001;
  ss_Control       = $0004;
  ss_Alt           = $0008;
  ss_CapsLock      = $0002;
  ss_NumLock       = $0010;
  ss_ScrollLock    = $0080;

  // Platform independent messages used by fpGUI (TfpgWidget)
  FPGM_PAINT       = 1;
  FPGM_ACTIVATE    = 2;
  FPGM_DEACTIVATE  = 3;
  FPGM_KEYPRESS    = 4;
  FPGM_KEYRELEASE  = 5;
  FPGM_KEYCHAR     = 6;
  FPGM_MOUSEDOWN   = 7;
  FPGM_MOUSEUP     = 8;
  FPGM_MOUSEMOVE   = 9;
  FPGM_DOUBLECLICK = 10;
  FPGM_MOUSEENTER  = 11;
  FPGM_MOUSEEXIT   = 12;
  FPGM_CLOSE       = 13;
  FPGM_SCROLL      = 14;
  FPGM_RESIZE      = 15;
  FPGM_MOVE        = 16;
  FPGM_POPUPCLOSE  = 17;
  FPGM_KILLME      = 9999;


  // The special keys, based on the well-known keyboard scan codes
  KEY_LEFT         = $FF4B;
  KEY_RIGHT        = $FF4D;
  KEY_DOWN         = $FF50;
  KEY_UP           = $FF48;
  KEY_END          = $FF4F;
  KEY_HOME         = $FF47;
  KEY_PGUP         = $FF49;
  KEY_PGDN         = $FF51;
  KEY_INSERT       = $FF52;
  KEY_DELETE       = $FF53;
  KEY_F1           = $FF3B;
  KEY_F2           = KEY_F1 + 1;
  KEY_F3           = KEY_F1 + 2;
  KEY_F4           = KEY_F1 + 3;
  KEY_F5           = KEY_F1 + 4;
  KEY_F6           = KEY_F1 + 5;
  KEY_F7           = KEY_F1 + 6;
  KEY_F8           = KEY_F1 + 7;
  KEY_F9           = KEY_F1 + 8;
  KEY_F10          = KEY_F1 + 9;
  KEY_F11          = $FF57;
  KEY_F12          = $FF58;
  // some general keys
  KEY_TAB          = $0009;
  KEY_ENTER        = $000D;
  KEY_SPACE        = $0020;
  KEY_ESC          = $001B;
  KEY_BACKSPACE    = $0008;


  // scan codes for KeyPress/KeyRelease
  KEYSC_ENTER      = $1C;
  KEYSC_SPACE      = $39;


  FPG_DEFAULT_FONT_DESC = 'Arial-10';
  UserNamedColorStart   = 128;

  {$I predefinedcolors.inc}

type
  TfpgRect = object  // not class for static allocations
    Top: TfpgCoord;
    Left: TfpgCoord;
    Width: TfpgCoord;
    Height: TfpgCoord;
    procedure SetRect(aleft, atop, awidth, aheight: TfpgCoord);
    function  Bottom: TfpgCoord;
    function  Right: TfpgCoord;
    procedure SetBottom(Value: TfpgCoord);
    procedure SetRight(Value: TfpgCoord);
  end;


  TfpgMsgParmMouse = record
    x: TfpgCoord;
    y: TfpgCoord;
    Buttons: word;
    shiftstate: word;
    delta: word;
  end;


  TfpgMsgParmKeyboard = record
    keycode: word;
    shiftstate: word;
  end;
  

  TfpgMessageParams = record
    case integer of
      0: (mouse: TfpgMsgParmMouse);
      1: (keyboard: TfpgMsgParmKeyboard);
      2: (rect: TfpgRect);
  end;
  

  TfpgMessageRec = record
    MsgCode: integer;
    Sender: TObject;
    Dest: TObject;
    Params: TfpgMessageParams;
  end;
  PfpgMessageRec = ^TfpgMessageRec;


  TfpgLineStyle = (lsSolid, lsDash, lsDot);


  TfpgImageBase = class(TObject)
  protected
    FWidth: integer;
    FHeight: integer;
    FColorDepth: integer;
    FMasked: boolean;
    FImageData: pointer;
    FImageDataSize: integer;
    FMaskData: pointer;
    FMaskDataSize: integer;
  public
    property    ImageData: pointer read FImageData;
    property    ImageDataSize: integer read FImageDataSize;
    property    MaskData: pointer read FMaskData;
    property    MaskDataSize: integer read FMaskDataSize;
    property    Width: integer read FWidth;
    property    Height: integer read FHeight;
    property    ColorDepth: integer read FColorDepth;
    property    Masked: boolean read FMasked;
  end;


  TfpgFontResourceBase = class(TObject)
  public
    function    GetAscent: integer; virtual; abstract;
    function    GetDescent: integer; virtual; abstract;
    function    GetHeight: integer; virtual; abstract;
    function    GetTextWidth(const txt: string): integer; virtual; abstract;
  end;


  TfpgFontBase = class(TObject)
  protected
    FFontDesc: string;
    FFontRes: TfpgFontResourceBase;
  public
    function    TextWidth(const txt: string): integer;
    function    Ascent: integer;
    function    Descent: integer;
    function    Height: integer;
    property    FontDesc: string read FFontDesc;
    property    FontRes: TfpgFontResourceBase read FFontRes;
    property    Handle: TfpgFontResourceBase read FFontRes;
  end;


  // forward declaration
  TfpgWindowBase = class;


  TfpgCanvasBase = class(TObject)
  protected
    FBufferedDraw: boolean;
    FBeginDrawCount: integer;
    FWindow: TfpgWindowBase;
    FColor: TfpgColor;
    FTextColor: TfpgColor;
    FLineWidth: integer;
    FLineStyle: TfpgLineStyle;
    FFont: TfpgFontBase;
    FPersistentResources: boolean;
    procedure   DoSetFontRes(fntres: TfpgFontResourceBase); virtual; abstract;
    procedure   DoSetTextColor(cl: TfpgColor); virtual; abstract;
    procedure   DoSetColor(cl: TfpgColor); virtual; abstract;
    procedure   DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle); virtual; abstract;
    procedure   DoGetWinRect(var r: TfpgRect); virtual; abstract;
    procedure   DoFillRectangle(x, y, w, h: TfpgCoord); virtual; abstract;
    procedure   DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord); virtual; abstract;
    procedure   DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord); virtual; abstract;
    procedure   DoDrawRectangle(x, y, w, h: TfpgCoord); virtual; abstract;
    procedure   DoDrawLine(x1, y1, x2, y2: TfpgCoord); virtual; abstract;
    procedure   DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer); virtual; abstract;
    procedure   DoDrawString(x, y: TfpgCoord; const txt: string); virtual; abstract;
    procedure   DoSetClipRect(const rect: TfpgRect); virtual; abstract;
    function    DoGetClipRect: TfpgRect; virtual; abstract;
    procedure   DoAddClipRect(const rect: TfpgRect); virtual; abstract;
    procedure   DoClearClipRect; virtual; abstract;
    procedure   DoBeginDraw(awin: TfpgWindowBase; buffered: boolean); virtual; abstract;
    procedure   DoPutBufferToScreen(x, y, w, h: TfpgCoord); virtual; abstract;
    procedure   DoEndDraw; virtual; abstract;
  public
    procedure   DrawRectangle(x, y, w, h: TfpgCoord); overload;
    procedure   DrawRectangle(r: TfpgRect); overload;
    procedure   DrawLine(x1, y1, x2, y2: TfpgCoord);
    procedure   DrawImage(x, y: TfpgCoord; img: TfpgImageBase);
    procedure   DrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer);
    procedure   DrawString(x, y: TfpgCoord; const txt: string);
    procedure   FillRectangle(x, y, w, h: TfpgCoord); overload;
    procedure   FillRectangle(r: TfpgRect); overload;
    procedure   FillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord);
    procedure   XORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord); overload;
    procedure   XORFillRectangle(col: TfpgColor; r: TfpgRect); overload;
    procedure   SetClipRect(const rect: TfpgRect);
    function    GetClipRect: TfpgRect;
    procedure   AddClipRect(const rect: TfpgRect);
    procedure   ClearClipRect;
    procedure   Clear(AColor: TfpgColor);
    procedure   GetWinRect(var r: TfpgRect);
    procedure   SetColor(AColor: TfpgColor);
    procedure   SetTextColor(AColor: TfpgColor);
    procedure   SetLineStyle(AWidth: integer; AStyle: TfpgLineStyle);
    procedure   SetFont(AFont: TfpgFontBase);
    procedure   BeginDraw; overload;
    procedure   BeginDraw(ABuffered: boolean); overload;
    procedure   EndDraw(x, y, w, h: TfpgCoord); overload;
    procedure   EndDraw; overload;
    procedure   FreeResources;
    property    Color: TfpgColor read FColor;
    property    TextColor: TfpgColor read FTextColor;
    property    Font: TfpgFontBase read FFont write SetFont;
  end;


  TfpgWindowBase = class(TComponent)
  protected
    FWindowType: TWindowType;
    FWindowAttributes: TWindowAttributes;
    FTop: TfpgCoord;
    FLeft: TfpgCoord;
    FWidth: TfpgCoord;
    FHeight: TfpgCoord;
    FMinWidth: TfpgCoord;
    FMinHeight: TfpgCoord;
    FCanvas: TfpgCanvasBase;
    FParentWindow: TfpgWindowBase;
    function    HandleIsValid: boolean; virtual; abstract;
    procedure   DoUpdateWindowPosition(aleft, atop, awidth, aheight: TfpgCoord); virtual; abstract;
    procedure   DoAllocateWindowHandle(AParent: TfpgWindowBase); virtual; abstract;
    procedure   DoReleaseWindowHandle; virtual; abstract;
    procedure   SetParentWindow(const AValue: TfpgWindowBase);
    function    GetParentWindow: TfpgWindowBase;
    function    GetCanvas: TfpgCanvasBase; virtual;
    procedure   AllocateWindowHandle;
    procedure   ReleaseWindowHandle;
  public
    // make some setup before the window shows
    procedure   AdjustWindowStyle; virtual;    // forms modify the window creation parameters
    procedure   SetWindowParameters; virtual;  // invoked after the window is created
    // general properties and functions
    function    Right: TfpgCoord;
    function    Bottom: TfpgCoord;
    procedure   UpdateWindowPosition;
    property    HasHandle: boolean read HandleIsValid;
    property    WindowType: TWindowType read FWindowType write FWindowType;
    property    WindowAttributes: TWindowAttributes read FWindowAttributes write FWindowAttributes;
    property    Left: TfpgCoord read FLeft write FLeft;
    property    Top: TfpgCoord read FTop write FTop;
    property    Width: TfpgCoord read FWidth write FWidth;
    property    Height: TfpgCoord read FHeight write FHeight;
    property    MinWidth: TfpgCoord read FMinWidth write FMinWidth;
    property    MinHeight: TfpgCoord read FMinHeight write FMinHeight;
    property    Canvas: TfpgCanvasBase read GetCanvas;
    property    ParentWindow: TfpgWindowBase read GetParentWindow write SetParentWindow;
  end;


  TfpgApplicationBase = class(TObject)
  protected
    FIsInitialized: Boolean;
  public
    constructor Create(const AParams: string); virtual; abstract;
    property    IsInitialized: boolean read FIsInitialized;
  end;


implementation

uses
  fpgfx;  // needed for fpgApplication

{ TfpgRect }

procedure TfpgRect.SetRect(aleft, atop, awidth, aheight: TfpgCoord);
begin
  Left   := aleft;
  Top    := atop;
  Width  := awidth;
  Height := aheight;
end;

function TfpgRect.Bottom: TfpgCoord;
begin
  Result := Top + Height - 1;
end;

function TfpgRect.Right: TfpgCoord;
begin
  Result := Left + Width - 1;
end;

procedure TfpgRect.SetBottom(Value: TfpgCoord);
begin
  Height := Value - Top + 1;
end;

procedure TfpgRect.SetRight(Value: TfpgCoord);
begin
  Width := Value - Left + 1;
end;

{ TfpgWindowBase }

procedure TfpgWindowBase.SetParentWindow(const AValue: TfpgWindowBase);
begin
  FParentWindow := AValue;
end;

function TfpgWindowBase.GetParentWindow: TfpgWindowBase;
begin
  result := FParentWindow;
end;

function TfpgWindowBase.GetCanvas: TfpgCanvasBase;
begin
  Result := FCanvas;
end;

procedure TfpgWindowBase.AllocateWindowHandle;
begin
  DoAllocateWindowHandle(FParentWindow);
end;

procedure TfpgWindowBase.ReleaseWindowHandle;
begin
  if HasHandle then
  begin
    Canvas.FreeResources;
    DoReleaseWindowHandle;
  end;
end;

procedure TfpgWindowBase.AdjustWindowStyle;
begin
  // does nothing here
end;

procedure TfpgWindowBase.SetWindowParameters;
begin
  // does nothing
end;

function TfpgWindowBase.Right: TfpgCoord;
begin
  Result := FLeft + FWidth - 1;
end;

function TfpgWindowBase.Bottom: TfpgCoord;
begin
  Result := FTop + FHeight - 1;
end;

procedure TfpgWindowBase.UpdateWindowPosition;
begin
  if HasHandle then
    DoUpdateWindowPosition(FLeft, FTop, FWidth, FHeight);
end;

{ TfpgCanvasBase }

procedure TfpgCanvasBase.DrawRectangle(x, y, w, h: TfpgCoord);
begin
  DoDrawRectangle(x, y, w, h);
end;

procedure TfpgCanvasBase.DrawRectangle(r: TfpgRect);
begin
  DoDrawRectangle(r.Left, r.Top, r.Width, r.Height);
end;

procedure TfpgCanvasBase.DrawLine(x1, y1, x2, y2: TfpgCoord);
begin
  DoDrawLine(x1, y1, x2, y2);
end;

procedure TfpgCanvasBase.DrawImage(x, y: TfpgCoord; img: TfpgImageBase);
begin
  if img = nil then
    Exit; //==>
  DrawImagePart(x, y, img, 0, 0, img.Width, img.Height);
end;

procedure TfpgCanvasBase.DrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi,
  yi, w, h: integer);
begin
  DoDrawImagePart(x, y, img, xi, yi, w, h);
end;

procedure TfpgCanvasBase.DrawString(x, y: TfpgCoord; const txt: string);
begin
  DoDrawString(x, y, txt);
end;

procedure TfpgCanvasBase.FillRectangle(x, y, w, h: TfpgCoord);
begin
  DoFillRectangle(x, y, w, h);
end;

procedure TfpgCanvasBase.FillRectangle(r: TfpgRect);
begin
  DoFillRectangle(r.Left, r.Top, r.Width, r.Height);
end;

procedure TfpgCanvasBase.FillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord);
begin
  DoFillTriangle(x1, y1, x2, y2, x3, y3);
end;

procedure TfpgCanvasBase.XORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord);
begin
  DoXORFillRectangle(col, x, y, w, h);
end;

procedure TfpgCanvasBase.XORFillRectangle(col: TfpgColor; r: TfpgRect);
begin
  DoXORFillRectangle(col, r.Left, r.Top, r.Width, r.Height);
end;

procedure TfpgCanvasBase.SetClipRect(const rect: TfpgRect);
begin
  DoSetClipRect(rect);
end;

function TfpgCanvasBase.GetClipRect: TfpgRect;
begin
  Result := DoGetClipRect;
end;

procedure TfpgCanvasBase.AddClipRect(const rect: TfpgRect);
begin
  DoAddClipRect(rect);
end;

procedure TfpgCanvasBase.ClearClipRect;
begin
  DoClearClipRect;
end;

procedure TfpgCanvasBase.Clear(AColor: TfpgColor);
var
  lCol:     TfpgColor;
  lWinRect: TfpgRect;
begin
  lCol := FColor;
  DoSetColor(AColor);
  DoGetWinRect(lWinRect);
  DoFillRectangle(0, 0, lWinRect.Width, lWinRect.Height);
  DoSetColor(lCol);
end;

procedure TfpgCanvasBase.GetWinRect(var r: TfpgRect);
begin
  DoGetWinRect(r);
end;

procedure TfpgCanvasBase.SetColor(AColor: TfpgColor);
begin
  FColor := AColor;
  DoSetColor(FColor);
end;

procedure TfpgCanvasBase.SetTextColor(AColor: TfpgColor);
begin
  FTextColor := AColor;
  DoSetTextColor(FTextColor);
end;

procedure TfpgCanvasBase.SetLineStyle(AWidth: integer; AStyle: TfpgLineStyle);
begin
  FLineWidth := AWidth;
  FLineStyle := AStyle;
  DoSetLineStyle(FLineWidth, FLineStyle);
end;

procedure TfpgCanvasBase.SetFont(AFont: TfpgFontBase);
begin
  FFont := AFont;
  DoSetFontRes(AFont.FFontRes);
end;

procedure TfpgCanvasBase.BeginDraw;
begin
  BeginDraw(FBufferedDraw);
end;

procedure TfpgCanvasBase.BeginDraw(ABuffered: boolean);
begin
  if FBeginDrawCount < 1 then
  begin
    DoBeginDraw(FWindow, ABuffered);

    SetColor(clText1);
    SetTextColor(clText1);
    SetFont(fpgApplication.DefaultFont);
    SetLineStyle(0, lsSolid);

    FBeginDrawCount := 0;
  end;
  Inc(FBeginDrawCount);
end;

procedure TfpgCanvasBase.EndDraw(x, y, w, h: TfpgCoord);
begin
  if FBeginDrawCount > 0 then
  begin
    Dec(FBeginDrawCount);
    if FBeginDrawCount = 0 then
    begin
      DoPutBufferToScreen(x, y, w, h);

      if not FPersistentResources then
        DoEndDraw;
    end;
  end;  { if }
end;

procedure TfpgCanvasBase.EndDraw;
begin
  EndDraw(0, 0, FWindow.Width, FWindow.Height);
end;

procedure TfpgCanvasBase.FreeResources;
begin
  DoEndDraw;
  FBeginDrawCount := 0;
end;

{ TfpgFontBase }

function TfpgFontBase.TextWidth(const txt: string): integer;
begin
  Result := FFontRes.GetTextWidth(txt);
end;

function TfpgFontBase.Ascent: integer;
begin
  Result := FFontRes.GetAscent;
end;

function TfpgFontBase.Descent: integer;
begin
  Result := FFontRes.GetDescent;
end;

function TfpgFontBase.Height: integer;
begin
  Result := FFontRes.GetHeight;
end;

end.

