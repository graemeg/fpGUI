{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      The Big Bang starts here!  The starting unit for fpGUI.
}

unit fpg_base;

{$mode objfpc}{$H+}

// To enable the AggPas powered Canvas
{.$define AGGCanvas}

// For debug use only
{.$define GDEBUG}

interface

uses
  Classes,
  SysUtils,
  fpg_impl,
  syncobjs, // TCriticalSection usage
  variants, contnrs;

type
  TfpgCoord       = integer;     // we might use floating point coordinates in the future...
  TfpgColor       = type longword;    // Always in AARRGGBB (Alpha, Red, Green, Blue) format!!
  TfpgString      = type AnsiString;
  TfpgChar        = type String[4];

  PPoint = ^TPoint;

  TRGBTriple = packed record
    Red: byte;
    Green: byte;
    Blue: byte;
    Alpha: byte;
  end;

  // Same declaration as in FPImage unit, but we don't use FPImage yet, so declare it here
  TFPColor = record
    Red: byte;
    Green: byte;
    Blue: byte;
    Alpha: byte;
  end deprecated;

  TWindowType = (wtChild, wtWindow, wtModalForm, wtPopup);

  TWindowAttribute = (waSizeable, waAutoPos, waScreenCenterPos, waStayOnTop,
      waFullScreen, waBorderless, waUnblockableMessages, waX11SkipWMHints,
      waOneThirdDownPos);
  TWindowAttributes = set of TWindowAttribute;

  TfpgWindowState = (wsNormal, wsMinimized, wsMaximized);

  TMouseCursor = (mcDefault, mcArrow, mcCross, mcIBeam, mcSizeEW, mcSizeNS,
      mcSizeNWSE, mcSizeNESW, mcSizeSWNE, mcSizeSENW, mcMove, mcHourGlass,
      mcHand, mcDrag, mcNoDrop);

  TGradientDirection = (gdVertical,     // Fill vertical
                        gdHorizontal);  // Fill Horizontal

  TClipboardKeyType = (ckNone, ckCopy, ckPaste, ckCut);

  // If you have to convert this to an Integer, mrNone = 0 etc.
  TfpgModalResult = (mrNone, mrOK, mrCancel, mrYes, mrNo, mrAbort, mrRetry,
      mrIgnore, mrAll, mrNoToAll, mrYesToAll, mrHelp);

  TfpgDropAction = (daIgnore, daCopy, daMove, daLink, daAsk);
  TfpgDropActions = set of TfpgDropAction;

  TfpgEditBorderStyle = (ebsNone, ebsDefault, ebsSingle);

  // in case we wanted to trap any fpGUI specific exceptions
  EfpGUIException = class(Exception);

  // For providing user feedback. No need to display backtrace information
  EfpGUIUserFeedbackException = class(EfpGUIException);



const
  MOUSE_LEFT       = 1;
  MOUSE_RIGHT      = 3;
  MOUSE_MIDDLE     = 2;

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
  FPGM_HINTTIMER   = 18;
  FPGM_FREEME      = 19;
  FPGM_DROPENTER   = 20;
  FPGM_DROPEXIT    = 21;
  FPGM_USER        = 50000;
  FPGM_KILLME      = MaxInt;

  // The special keys, based on the well-known keyboard scan codes
  {$I keys.inc}


var
  {$IFDEF MSWINDOWS}
  FPG_DEFAULT_FONT_DESC: string = 'Arial-8:antialias=true';
  FPG_DEFAULT_SANS: string = 'Arial';
  {$ENDIF}
  {$IFDEF UNIX}
  FPG_DEFAULT_FONT_DESC: string = 'Liberation Sans-10:antialias=true';
  FPG_DEFAULT_SANS: string = 'Liberation Sans';
  {$ENDIF}

const
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


  TfpgPoint = object  // not class for static allocations
    X: integer;
    Y: integer;
    procedure SetPoint(AX, AY: integer);
    function  ManhattanLength: integer;      { See URL for explanation http://en.wikipedia.org/wiki/Taxicab_geometry }
    function  ManhattanLength(const PointB: TfpgPoint): integer;
  end;


  TfpgSize = object  // not class for static allocations
    W: integer;
    H: integer;
    procedure SetSize(AWidth, AHeight: integer);
  end;


  TfpgMsgParmMouse = record
    x: TfpgCoord;
    y: TfpgCoord;
    Buttons: word;
    shiftstate: TShiftState;
    delta: Integer;
    timestamp: TDateTime;  // for future use
  end;


  TfpgMsgParmKeyboard = record
    keycode: word;
    keychar: TfpgChar;
    shiftstate: TShiftState;
  end;


  TfpgMsgParmUser = record
    Param1: Integer;
    Param2: Integer;
    Param3: Integer;
  end;


  TfpgMessageParams = record
    case integer of
      0: (mouse: TfpgMsgParmMouse);
      1: (keyboard: TfpgMsgParmKeyboard);
      2: (rect: TfpgRect);
      3: (user: TfpgMsgParmUser);
  end;


  TfpgMessageRec = record
    MsgCode: integer;
    Sender: TObject;
    Dest: TObject;
    Params: TfpgMessageParams;
    Stop: Boolean;
  end;
  PfpgMessageRec = ^TfpgMessageRec;


  TfpgLineStyle = (lsSolid, lsDash, lsDot, lsDashDot, lsDashDotDot);


  // forward declaration
  TfpgWindowBase = class;
  TfpgCanvasBase = class;


  TfpgImageBase = class(TObject)
  private
    function    GetColor(x, y: TfpgCoord): TfpgColor;
    procedure   SetColor(x, y: TfpgCoord; const AValue: TfpgColor);
  protected
    FWidth: integer;
    FHeight: integer;
    FColorDepth: integer;
    FMasked: boolean;
    FImageData: pointer;
    FImageDataSize: integer;
    FMaskData: pointer;
    FMaskDataSize: integer;
    FMaskPoint: TPoint;
    procedure   DoFreeImage; virtual; abstract;
    procedure   DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: Pointer); virtual; abstract;
    procedure   DoInitImageMask(awidth, aheight: integer; aimgdata: Pointer); virtual; abstract;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Invert(IncludeMask: Boolean = False);
    procedure   FreeImage;
    procedure   AllocateImage(acolordepth, awidth, aheight: integer);
    procedure   AllocateMask;
    procedure   CreateMaskFromSample(x, y: TfpgCoord);
    { Must always be called AFTER you populated the ImageData array. Then only does it allocate OS resources. }
    procedure   UpdateImage;
    { Internal representation of color data is always ARGB }
    property    ImageData: pointer read FImageData;
    property    ImageDataSize: integer read FImageDataSize;
    property    MaskData: pointer read FMaskData;
    property    MaskDataSize: integer read FMaskDataSize;
    property    Width: integer read FWidth;
    property    Height: integer read FHeight;
    property    ColorDepth: integer read FColorDepth;
    property    Masked: boolean read FMasked;
    property    MaskPoint: TPoint read FMaskPoint;
    property    Colors[x, y: TfpgCoord]: TfpgColor read GetColor write SetColor;
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
    function    GetIsFixedWidth: boolean; virtual;
  public
    function    TextWidth(const txt: TfpgString): integer;
    function    Ascent: integer;
    function    Descent: integer;
    function    Height: integer;
    property    FontDesc: string read FFontDesc;
    property    FontRes: TfpgFontResourceBase read FFontRes;
    property    Handle: TfpgFontResourceBase read FFontRes;
    property    IsFixedWidth: boolean read GetIsFixedWidth;
  end;


  TfpgCustomInterpolation = class(TObject)
  private
    FCanvas: TfpgCanvasBase;
    FImage: TfpgImageBase;
  protected
    procedure   Initialize(AImage: TfpgImageBase; ACanvas: TfpgCanvasBase); virtual;
    procedure   Execute(x, y, w, h: integer); virtual; abstract;
  public
    property    Canvas: TfpgCanvasBase read FCanvas;
    property    Image: TfpgImageBase read FImage;
  end;


  TfpgBaseInterpolation = class(TfpgCustomInterpolation)
  private
    xfactor: double;
    yfactor: double;
    xsupport: double;
    ysupport: double;
    tempimage: TfpgImageBase;
    procedure   Horizontal(width: integer);
    procedure   Vertical(dx, dy, width, height: integer);
  protected
    procedure   Execute(x, y, w, h: integer); override;
    function    Filter(x : double): double; virtual; abstract;
    function    MaxSupport: double; virtual; abstract;
  public
    destructor  Destroy; override;
  end;


  TfpgMitchelInterpolation = class(TfpgBaseInterpolation)
  protected
    function    Filter(x: double): double; override;
    function    MaxSupport: double; override;
  end;


  TfpgCanvasBase = class(TObject)
  private
    FFastDoubleBuffer: Boolean;
    FInterpolation: TfpgCustomInterpolation;
    procedure SetInterpolation(const AValue: TfpgCustomInterpolation);
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
    procedure   DoGetWinRect(out r: TfpgRect); virtual; abstract;
    procedure   DoFillRectangle(x, y, w, h: TfpgCoord); virtual; abstract;
    procedure   DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord); virtual; abstract;
    procedure   DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord); virtual; abstract;
    procedure   DoDrawRectangle(x, y, w, h: TfpgCoord); virtual; abstract;
    procedure   DoDrawLine(x1, y1, x2, y2: TfpgCoord); virtual; abstract;
    procedure   DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer); virtual; abstract;
    procedure   DoDrawString(x, y: TfpgCoord; const txt: string); virtual; abstract;
    procedure   DoSetClipRect(const ARect: TfpgRect); virtual; abstract;
    function    DoGetClipRect: TfpgRect; virtual; abstract;
    procedure   DoAddClipRect(const ARect: TfpgRect); virtual; abstract;
    procedure   DoClearClipRect; virtual; abstract;
    procedure   DoBeginDraw(awin: TfpgWindowBase; buffered: boolean); virtual; abstract;
    procedure   DoPutBufferToScreen(x, y, w, h: TfpgCoord); virtual; abstract;
    procedure   DoEndDraw; virtual; abstract;
    function    GetPixel(X, Y: integer): TfpgColor; virtual; abstract;
    procedure   SetPixel(X, Y: integer; const AValue: TfpgColor); virtual; abstract;
    procedure   DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: Extended); virtual; abstract;
    procedure   DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended); virtual; abstract;
    procedure   DoDrawPolygon(Points: PPoint; NumPts: Integer; Winding: boolean = False); virtual; abstract;
  public
    constructor Create(awin: TfpgWindowBase); virtual;
    destructor  Destroy; override;
    procedure   DrawRectangle(x, y, w, h: TfpgCoord); overload;
    procedure   DrawRectangle(r: TfpgRect); overload;
    procedure   DrawLine(x1, y1, x2, y2: TfpgCoord);
    procedure   DrawLineClipped(var x1, y1, x2, y2: TfpgCoord; const AClipRect: TfpgRect);
    procedure   ClipLine(var x1, y1, x2, y2: TfpgCoord; const AClipRect: TfpgRect; out FallsOutsideRegion: Boolean);
    procedure   DrawImage(x, y: TfpgCoord; img: TfpgImageBase);
    procedure   DrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer);
    procedure   DrawArc(x, y, w, h: TfpgCoord; a1, a2: double);
    procedure   DrawPolygon(const Points: array of TPoint; Winding: Boolean; StartIndex: Integer = 0; NumPts: Integer = -1);
    procedure   DrawPolygon(Points: PPoint; NumPts: Integer; Winding: boolean = False); virtual;
    procedure   DrawPolygon(const Points: array of TPoint);
    procedure   StretchDraw (x, y, w, h: TfpgCoord; ASource: TfpgImageBase);
    procedure   CopyRect(ADest_x, ADest_y: TfpgCoord; ASrcCanvas: TfpgCanvasBase; var ASrcRect: TfpgRect);
    // x,y is the top/left corner of where the text output will start.
    procedure   DrawString(x, y: TfpgCoord; const txt: string);
    procedure   FillRectangle(x, y, w, h: TfpgCoord); overload;
    procedure   FillRectangle(r: TfpgRect); overload;
    procedure   FillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord);
    procedure   FillArc(x, y, w, h: TfpgCoord; a1, a2: double);
    procedure   GradientFill(ARect: TfpgRect; AStart, AStop: TfpgColor; ADirection: TGradientDirection);
    procedure   XORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord); overload;
    procedure   XORFillRectangle(col: TfpgColor; r: TfpgRect); overload;
    procedure   SetClipRect(const ARect: TfpgRect);
    function    GetClipRect: TfpgRect;
    function    GetLineWidth: integer;
    procedure   AddClipRect(const ARect: TfpgRect);
    procedure   ClearClipRect;
    procedure   Clear(AColor: TfpgColor);
    procedure   GetWinRect(out r: TfpgRect);
    procedure   SetColor(AColor: TfpgColor);
    procedure   SetTextColor(AColor: TfpgColor);
    procedure   SetLineStyle(AWidth: integer; AStyle: TfpgLineStyle);
    procedure   SetFont(AFont: TfpgFontBase);
    procedure   BeginDraw; overload;
    procedure   BeginDraw(ABuffered: boolean); overload;
    procedure   EndDraw(x, y, w, h: TfpgCoord); overload;
    procedure   EndDraw(ARect: TfpgRect); overload;
    procedure   EndDraw; overload;
    procedure   FreeResources;
    property    Color: TfpgColor read FColor write SetColor;
    property    TextColor: TfpgColor read FTextColor write SetTextColor;
    property    Font: TfpgFontBase read FFont write SetFont;
    property    Pixels[X, Y: integer]: TfpgColor read GetPixel write SetPixel;
    property    InterpolationFilter: TfpgCustomInterpolation read FInterpolation write SetInterpolation;
    property    FastDoubleBuffer: Boolean read FFastDoubleBuffer write FFastDoubleBuffer;
    property    LineStyle: TfpgLineStyle read FLineStyle;
  end;

  TfpgCanvasBaseClass = class of TfpgCanvasBase;


  TfpgComponent = class(TComponent)
  private
    FTagPointer: Pointer;
    FHelpContext: THelpContext;
    FHelpKeyword: TfpgString;
    FHelpType: THelpType;
  protected
    procedure   SetHelpContext(const AValue: THelpContext); virtual;
    procedure   SetHelpKeyword(const AValue: TfpgString); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property    TagPointer: Pointer read FTagPointer write FTagPointer;
  published
    property    HelpContext: THelpContext read FHelpContext write SetHelpContext default 0;
    property    HelpKeyword: TfpgString read FHelpKeyword write SetHelpKeyword;
    property    HelpType: THelpType read FHelpType write FHelpType default htKeyword;
  end;


  TfpgWindowBase = class(TfpgComponent)
  private
    FParent: TfpgWindowBase;
    procedure   SetMouseCursor(const AValue: TMouseCursor);
    function    ConstraintWidth(NewWidth: TfpgCoord): TfpgCoord;
    function    ConstraintHeight(NewHeight: TfpgCoord): TfpgCoord;
  protected
    FMouseCursor: TMouseCursor;
    FWindowType: TWindowType;
    FWindowAttributes: TWindowAttributes;
    FTop: TfpgCoord;
    FLeft: TfpgCoord;
    FWidth: TfpgCoord;
    FHeight: TfpgCoord;
    FPrevTop: TfpgCoord;
    FPrevLeft: TfpgCoord;
    FPrevWidth: TfpgCoord;
    FPrevHeight: TfpgCoord;
    FMinWidth: TfpgCoord;
    FMinHeight: TfpgCoord;
    FMaxHeight: TfpgCoord;
    FMaxWidth: TfpgCoord;
    FCanvas: TfpgCanvasBase;
    FSizeIsDirty: Boolean;
    FPosIsDirty: Boolean;
    FMouseCursorIsDirty: Boolean;
    FOnDragStartDetected: TNotifyEvent;
    FDragActive: boolean;
    FWindowState: TfpgWindowState;
    function    HandleIsValid: boolean; virtual; abstract;
    procedure   DoUpdateWindowPosition; virtual; abstract;
    procedure   DoAllocateWindowHandle(AParent: TfpgWindowBase); virtual; abstract;
    procedure   DoReleaseWindowHandle; virtual; abstract;
    procedure   DoRemoveWindowLookup; virtual; abstract;
    procedure   DoSetWindowVisible(const AValue: Boolean); virtual; abstract;
    procedure   DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord); virtual; abstract;
    function    DoWindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint; virtual; abstract;
    procedure   DoSetWindowTitle(const ATitle: string); virtual; abstract;
    procedure   DoSetMouseCursor; virtual; abstract;
    procedure   DoDNDEnabled(const AValue: boolean); virtual; abstract;
    procedure   DoAcceptDrops(const AValue: boolean); virtual; abstract;
    function    GetWindowState: TfpgWindowState; virtual;
    procedure   SetWindowState(const AValue: TfpgWindowState); virtual;
    procedure   DoDragStartDetected; virtual;
    procedure   SetParent(const AValue: TfpgWindowBase); virtual;
    function    GetParent: TfpgWindowBase; virtual;
    function    GetCanvas: TfpgCanvasBase; virtual;
    procedure   AllocateWindowHandle;
    procedure   ReleaseWindowHandle;
    procedure   SetWindowTitle(const ATitle: string); virtual;
    procedure   SetTop(const AValue: TfpgCoord);
    procedure   SetLeft(const AValue: TfpgCoord);
    procedure   SetHeight(const AValue: TfpgCoord);
    procedure   SetWidth(const AValue: TfpgCoord);
    procedure   HandleMove(x, y: TfpgCoord); virtual;
    procedure   HandleResize(AWidth, AHeight: TfpgCoord); virtual;
    property    OnDragStartDetected: TNotifyEvent read FOnDragStartDetected write FOnDragStartDetected;
    property    WindowState: TfpgWindowState read GetWindowState {write SetWindowState} default wsNormal;
  public
    // The standard constructor.
    constructor Create(AOwner: TComponent); override;
    procedure   AfterConstruction; override;
    // Make some setup before the window shows. Forms modify the window creation parameters.
    procedure   AdjustWindowStyle; virtual;
    // Make some setup before the window shows. Invoked after the window is created.
    procedure   SetWindowParameters; virtual;
    // general properties and functions
    function    Right: TfpgCoord;
    function    Bottom: TfpgCoord;
    procedure   UpdateWindowPosition;
    procedure   MoveWindow(const x: TfpgCoord; const y: TfpgCoord);
    function    WindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint;
    function    HasParent: Boolean; override;
    function    GetClientRect: TfpgRect; virtual;
    function    GetBoundsRect: TfpgRect; virtual;
    procedure   ActivateWindow; virtual; abstract;
    procedure   CaptureMouse; virtual; abstract;
    procedure   ReleaseMouse; virtual; abstract;
    procedure   BringToFront; virtual; abstract;
    procedure   SetFullscreen(AValue: Boolean); virtual;
    property    HasHandle: boolean read HandleIsValid;
    property    WindowType: TWindowType read FWindowType write FWindowType;
    property    WindowAttributes: TWindowAttributes read FWindowAttributes write FWindowAttributes;
    property    Left: TfpgCoord read FLeft write SetLeft;
    property    Top: TfpgCoord read FTop write SetTop;
    property    Width: TfpgCoord read FWidth write SetWidth;
    property    Height: TfpgCoord read FHeight write SetHeight;
    property    MinWidth: TfpgCoord read FMinWidth write FMinWidth;
    property    MinHeight: TfpgCoord read FMinHeight write FMinHeight;
    property    MaxWidth: TfpgCoord read FMaxWidth write FMaxWidth default 0;
    property    MaxHeight: TfpgCoord read FMaxHeight write FMaxHeight default 0;
    property    Canvas: TfpgCanvasBase read GetCanvas;
    property    Parent: TfpgWindowBase read GetParent write SetParent;
    property    MouseCursor: TMouseCursor read FMouseCursor write SetMouseCursor;
  end;


  TfpgApplicationBase = class(TfpgComponent)
  private
    FMainForm: TfpgWindowBase;
    FTerminated: boolean;
    FCritSect: TCriticalSection;
    FHelpKey: word;
    FHelpFile: TfpgString;
    function    GetForm(Index: Integer): TfpgWindowBase;
    function    GetFormCount: integer;
    function    GetTopModalForm: TfpgWindowBase;
    function    GetHelpFile: TfpgString;
  protected
    FOnIdle: TNotifyEvent;
    FIsInitialized: Boolean;
    FModalFormStack: TList;
    function    DoGetFontFaceList: TStringList; virtual; abstract;
    procedure   DoWaitWindowMessage(atimeoutms: integer); virtual; abstract;
    function    MessagesPending: boolean; virtual; abstract;
    function    GetHelpViewer: TfpgString; virtual;
  public
    constructor Create(const AParams: string); virtual; reintroduce;
    destructor  Destroy; override;
    function    GetFontFaceList: TStringList;
    procedure   PushModalForm(AForm: TfpgWindowBase);
    procedure   PopModalForm;
    function    PrevModalForm: TfpgWindowBase;
    function    RemoveWindowFromModalStack(AForm: TfpgWindowBase): Integer;
    procedure   CreateForm(InstanceClass: TComponentClass; out Reference);
    function    GetScreenWidth: TfpgCoord; virtual; abstract;
    function    GetScreenHeight: TfpgCoord; virtual; abstract;
    function    Screen_dpi_x: integer; virtual; abstract;
    function    Screen_dpi_y: integer; virtual; abstract;
    function    Screen_dpi: integer; virtual; abstract;
    procedure   Terminate;
    procedure   Lock;
    procedure   Unlock;
    procedure   InvokeHelp;
    function    ContextHelp(const AHelpContext: THelpContext): Boolean;
    function    KeywordHelp(const AHelpKeyword: string): Boolean;
    property    FormCount: integer read GetFormCount;
    property    Forms[Index: Integer]: TfpgWindowBase read GetForm;
    property    HelpContext;
    property    HelpFile: TfpgString read GetHelpFile write FHelpFile;
    property    HelpKey: word read FHelpKey write FHelpKey default keyF1;
    property    IsInitialized: boolean read FIsInitialized;
    property    TopModalForm: TfpgWindowBase read GetTopModalForm;
    property    MainForm: TfpgWindowBase read FMainForm write FMainForm;
    property    Terminated: boolean read FTerminated write FTerminated;
    property    OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
  end;


  TfpgClipboardBase = class(TObject)
  protected
    FClipboardWndHandle: TfpgWinHandle;
    function    DoGetText: TfpgString; virtual; abstract;
    procedure   DoSetText(const AValue: TfpgString); virtual; abstract;
    procedure   InitClipboard; virtual; abstract;
  public
    constructor Create; virtual;
    property    Text: TfpgString read DoGetText write DoSetText;
  end;


  TFileEntryType = (etFile, etDir);
  TFileListSortOrder = (soNone, soFileName, soCSFileName, soFileExt, soSize, soTime);
  TFileModeString = string[9];


  // A simple data object
  TFileEntry = class(TObject)
  private
    FEntryType: TFileEntryType;
    FExtension: string;
    FName: string;
    FModTime: TDateTime;
    FSize: int64;
    FIsLink: boolean;
    FLinkTarget: string;
    FIsExecutable: boolean;
    FModeString: TFileModeString;
    FOwner: TfpgString;
    FGroup: TfpgString;
    FAttrString: TFileModeString;
  public
    constructor Create;
    property    Name: string read FName write FName;
    property    Extension: string read FExtension write FExtension;
    property    Size: int64 read FSize write FSize;
    property    EntryType: TFileEntryType read FEntryType write FEntryType;
    property    IsLink: boolean read FIsLink write FIsLink;
    property    LinkTarget: string read FLinkTarget write FLinkTarget;
    property    IsExecutable: boolean read FIsExecutable write FIsExecutable;
    property    ModTime: TDateTime read FModTime write FModTime;
    property    Mode: TFileModeString read FModeString write FModeString;
    property    Owner: TfpgString read FOwner write FOwner;
    property    Group: TfpgString read FGroup write FGroup;
    property    Attributes: TFileModeString read FAttrString write FAttrString;
  end;


  TfpgFileListBase = class(TObject)
  private
    FEntries: TList;
    FDirectoryName: TfpgString;
    FFileMask: TfpgString;
    FShowHidden: boolean;
    FCurrentSpecialDir: integer;
    procedure   AddEntry(sr: TSearchRec);
    function    GetEntry(i: integer): TFileEntry;
    function    HasAttrib(fileAttrib, testAttrib: Integer): Boolean;
  protected
    FSpecialDirs: TStringList;
    FHasFileMode: boolean;
    function    InitializeEntry(sr: TSearchRec): TFileEntry; virtual;
    procedure   PopulateSpecialDirs(const aDirectory: TfpgString); virtual;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    function    Count: integer;
    function    CurrentSpecialDir: integer;
    function    ReadDirectory(const aDirectory: TfpgString = ''): boolean;
    procedure   Clear;
    procedure   Sort(AOrder: TFileListSortOrder);
    property    DirectoryName: TfpgString read FDirectoryName;
    property    Entry[i: integer]: TFileEntry read GetEntry;
    property    FileMask: TfpgString read FFileMask write FFileMask;
    property    HasFileMode: boolean read FHasFileMode;
    property    ShowHidden: boolean read FShowHidden write FShowHidden;
    property    SpecialDirs: TStringList read FSpecialDirs;
  end;


  TfpgMimeDataItem = class(TObject)
  public
    format: TfpgString;   { mime string type }
    data: Variant;
    constructor Create(const AFormat: TfpgString; const AData: variant); reintroduce;
  end;


  TfpgMimeDataBase = class(TObject)
  private
    { TODO: This is wrong, we must have one Data Storage object }
    FDataList: TObjectList;
    FUrlList: TList;
    function    GetItem(AIndex: Integer): TfpgMimeDataItem;
    function    Geturls: TList;
    procedure   Seturls(const AValue: TList);
    function    GetText: TfpgString;
    procedure   SetText(const AValue: TfpgString);
    function    GetHTML: TfpgString;
    procedure   SetHTML(const AValue: TfpgString);
    function    GetCount: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Clear;
    function    HasFormat(const AMimeType: TfpgString): boolean;
    function    Formats: TStrings;
    function    GetData(const AMimeType: TfpgString): Variant;
    procedure   SetData(const AMimeType: TfpgString; const AData: Variant);
    property    Items[AIndex: Integer]: TfpgMimeDataItem read GetItem; default;
    property    urls: TList read Geturls write Seturls;
    property    Text: TfpgString read GetText write SetText;
    property    HTML: TfpgString read GetHTML write SetHTML;
    property    Count: integer read GetCount;
  end;


  TfpgDragBase = class(TObject)
  protected
    FDragging: Boolean;
    FMimeData: TfpgMimeDataBase;
  public
    constructor Create;
    destructor  Destroy; override;
    function    Execute(const ADropActions: TfpgDropActions; const ADefaultAction: TfpgDropAction = daCopy): TfpgDropAction; virtual; abstract;
  end;
  
  
  { TfpgBaseTimer }

  TfpgBaseTimer = class(TObject)
  private
    FNextAlarm: TDateTime;
    FInterval: integer;
    FOnTimer: TNotifyEvent;
    procedure   SetInterval(const AValue: integer);
  protected
    FEnabled: boolean;
    procedure   SetEnabled(const AValue: boolean); virtual;
  public
    constructor Create(AInterval: integer); virtual;
    destructor  Destroy; override;
    procedure   CheckAlarm(ACurrentTime: TDateTime);
    procedure   Reset;
    procedure   Pause(ASeconds: integer);
    property    Enabled: boolean read FEnabled write SetEnabled;
    property    NextAlarm: TDateTime read FNextAlarm;
    { Interval is in milliseconds. }
    property    Interval: integer read FInterval write SetInterval;
    property    OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;




{ ********  Helper functions  ******** }
{ Keyboard }
function  KeycodeToText(AKey: Word; AShiftState: TShiftState): string;
function  CheckClipboardKey(AKey: Word;  AShiftstate: TShiftState): TClipboardKeyType;

{ Color }
function  fpgColorToRGBTriple(const AColor: TfpgColor): TRGBTriple;
function  fpgColorToFPColor(const AColor: TfpgColor): TFPColor; deprecated;
function  RGBTripleTofpgColor(const AColor: TRGBTriple): TfpgColor;
function  FPColorTofpgColor(const AColor: TFPColor): TfpgColor; deprecated;
function  fpgGetRed(const AColor: TfpgColor): byte;
function  fpgGetGreen(const AColor: TfpgColor): byte;
function  fpgGetBlue(const AColor: TfpgColor): byte;
function  fpgGetAlpha(const AColor: TfpgColor): byte;
function  fpgGetAvgColor(const AColor1, AColor2: TfpgColor): TfpgColor;
function  fpgColor(const ARed, AGreen, ABlue: byte): TfpgColor;
function  fpgColor(const ARed, AGreen, ABlue, AAlpha: byte): TfpgColor;
function  fpgDarker(const AColor: TfpgColor; APercent: Byte = 50): TfpgColor;
function  fpgLighter(const AColor: TfpgColor; APercent: Byte = 50): TfpgColor;


{ Points }
function  PtInRect(const ARect: TfpgRect; const APoint: TPoint): Boolean;
procedure SortRect(var ARect: TRect);
procedure SortRect(var ARect: TfpgRect);
procedure SortRect(var left, top, right, bottom: integer);



implementation

uses
  fpg_main,  // needed for fpgApplication & fpgNamedColor
  fpg_utils, // needed for fpgFileList
  fpg_constants,
  fpg_form,  // needed for fpgApplication.CreateForms()
  typinfo,
  process,
  {$IFDEF GDEBUG}
  dbugintf,
  {$ENDIF}
  dateutils;


const
  NoDefault = $80000000;
  tkPropsWithDefault = [tkInteger, tkChar, tkSet, tkEnumeration];


function KeycodeToText(AKey: Word; AShiftState: TShiftState): string;

  function GetASCIIText: String;
  var
    c: Char;
  begin
    result := '';
    c := Chr(AKey and $ff);
    case c of
      #13:  Result := Result + rsKeyEnter;
      #127: Result := Result + rsKeyDel;
      else
        Result := Result + c;
    end;
  end;

var
  s: String;
begin
  SetLength(Result, 0);

  { The order of these three are imprortant - don't change them }
  if ssCtrl in AShiftState then
    Result := Result + rsKeyCtrl;
  if ssAlt in AShiftState then
    Result := Result + rsKeyAlt;
  if ssShift in AShiftState then
    Result := Result + rsKeyShift;
  if ssMeta in AShiftState then
    Result := Result + rskeyMeta;

  if (AKey > Ord(' ')) and (AKey < 255) then
  begin
    Result := Result + GetASCIIText;
    Exit; //==>
  end;

  case AKey of
    keyNul:           s := 'Null';
    keyBackSpace:     s := rsKeyBksp;
    keyTab:           s := rsKeyTab;
    keyLinefeed:      s := 'Linefeed';
    keyReturn:        s := rsKeyEnter;
    keyEscape:        s := rsKeyEsc;
    Ord(' '):         s := rsKeySpace;
    keyDelete:        s := rsKeyDel;
    keyVoid:          s := 'Void';
    keyBreak:         s := 'Break';
    keyScrollForw:    s := 'ScrollForw';
    keyScrollBack:    s := 'ScrollBack';
    keyBoot:          s := 'Boot';
    keyCompose:       s := 'Compose';
    keySAK:           s := 'SAK';
    keyUndo:          s := 'Undo';
    keyRedo:          s := 'Redo';
    keyMenu:          s := 'Menu';
    keyCancel:        s := 'Cancel';
    keyPrintScreen:   s := 'PrtScr';
    keyExecute:       s := 'Exec';
    keyFind:          s := 'Find';
    keyBegin:         s := 'Begin';
    keyClear:         s := 'Clear';
    keyInsert:        s := rsKeyIns;
    keySelect:        s := 'Select';
    keyMacro:         s := 'Macro';
    keyHelp:          s := 'Help';
    keyDo:            s := 'Do';
    keyPause:         s := 'Pause';
    keySysRq:         s := 'SysRq';
    keyModeSwitch:    s := 'ModeSw';
    keyUp:            s := rsKeyUp;
    keyDown:          s := rsKeyDown;
    keyLeft:          s := rsKeyLeft;
    keyRight:         s := rsKeyRight;
    keyPrior:         s := rsKeyPgUp;
    keyNext:          s := rsKeyPgDn;
    keyHome:          s := rsKeyHome;
    keyEnd:           s := rsKeyEnd;
    keyF0..keyF64:    s := 'F' + IntToStr(AKey - keyF0);
    keyP0..keyP9:     s := 'KP' + Chr(AKey - keyP0 + Ord('0'));
    keyPA..keyPF:     s := 'KP' + Chr(AKey - keyPA + Ord('A'));
    keyPPlus, keyPMinus, keyPSlash, keyPStar, keyPEqual, keyPSeparator,
      keyPDecimal, keyPParenLeft, keyPParenRight, keyPSpace, keyPEnter,
      keyPTab:        s := 'KP' + GetASCIIText;
    keyPPlusMinus:    s := 'KPPlusMinus';
    keyPBegin:        s := 'KPBegin';
    keyPF1..keyPF9:   s := 'KPF' + IntToStr(AKey - keyPF1);
    keyShiftL:        s := 'ShiftL';
    keyShiftR:        s := 'ShiftR';
    keyCtrlL:         s := 'CtrlL';
    keyCtrlR:         s := 'CtrlR';
    keyAltL:          s := 'AltL';
    keyAltR:          s := 'AltR';
    keyMetaL:         s := 'MetaL';
    keyMetaR:         s := 'MetaR';
    keySuperL:        s := 'SuperL';
    keySuperR:        s := 'SuperR';
    keyHyperL:        s := 'HyperL';
    keyHyperR:        s := 'HyperR';
    keyAltGr:         s := 'AltGr';
    keyCaps:          s := 'Caps';
    keyNum:           s := 'Num';
    keyScroll:        s := 'Scroll';
    keyShiftLock:     s := 'ShiftLock';
    keyCtrlLock:      s := 'CtrlLock';
    keyAltLock:       s := 'AltLock';
    keyMetaLock:      s := 'MetaLock';
    keySuperLock:     s := 'SuperLock';
    keyHyperLock:     s := 'HyperLock';
    keyAltGrLock:     s := 'AltGrLock';
    keyCapsLock:      s := 'CapsLock';
    keyNumLock:       s := 'NumLock';
    keyScrollLock:    s := 'ScrollLock';
    keyDeadRing:      s := 'DeadRing';
    keyDeadCaron:     s := 'DeadCaron';
    keyDeadOgonek:    s := 'DeadOgonek';
    keyDeadIota:      s := 'DeadIota';
    keyDeadDoubleAcute:     s := 'DeadDoubleAcute';
    keyDeadBreve:           s := 'DeadBreve';
    keyDeadAboveDot:        s := 'DeadAboveDot';
    keyDeadBelowDot:        s := 'DeadBelowDot';
    keyDeadVoicedSound:     s := 'DeadVoicedSound';
    keyDeadSemiVoicedSound: s := 'DeadSemiVoicedSound';
    keyDeadAcute:           s := 'DeadAcute';
    keyDeadCedilla:         s := 'DeadCedilla';
    keyDeadCircumflex:      s := 'DeadCircumflex';
    keyDeadDiaeresis:       s := 'DeadDiaeresis';
    keyDeadGrave:           s := 'DeadGrave';
    keyDeadTilde:           s := 'DeadTilde';
    keyDeadMacron:          s := 'DeadMacron';

    keyEcuSign:       s := 'Ecu';
    keyColonSign:     s := 'Colon';
    keyCruzeiroSign:  s := 'Cruzeiro';
    keyFFrancSign:    s := 'FFranc';
    keyLiraSign:      s := 'Lira';
    keyMillSign:      s := 'Mill';
    keyNairaSign:     s := 'Naira';
    keyPesetaSign:    s := 'Peseta';
    keyRupeeSign:     s := 'Rupee';
    keyWonSign:       s := 'Won';
    keyNewSheqelSign: s := 'NewShequel';
    keyDongSign:      s := 'Dong';
    keyEuroSign:      s := 'Euro';
  else
    s := '#' + IntToHex(AKey, 4);
  end;
  Result := Result + s;
end;

function CheckClipboardKey(AKey: Word; AShiftstate: TShiftState): TClipboardKeyType;
var
  c: string;
begin
//  writeln('CheckClipboardKey');
  Result := ckNone;

  if AKey = keyInsert then
  begin
    if (AShiftstate = [ssCtrl]) then
      Result := ckCopy
    else if (AShiftstate = [ssShift]) then
      Result := ckPaste;
  end
  else if (AKey = keyDelete) and (AShiftstate = [ssShift]) then
    Result := ckCut
  else if (AShiftstate = [ssCtrl]) then
  begin
    c := KeycodeToText(AKey, []);   // case is not important
//    Writeln('Key: ', c);
    if c = 'C' then
      Result := ckCopy
    else if c = 'V' then
      Result := ckPaste
    else if c = 'X' then
      Result := ckCut;
  end  { if/else }
end;

function fpgColorToRGBTriple(const AColor: TfpgColor): TRGBTriple;
begin
  with Result do
  begin
    Red   := fpgGetRed(AColor);
    Green := fpgGetGreen(AColor);
    Blue  := fpgGetBlue(AColor);
    Alpha := fpgGetAlpha(AColor);
  end
end;

function fpgColorToFPColor(const AColor: TfpgColor): TFPColor; deprecated;
begin
  with Result do
  begin
    Red   := fpgGetRed(AColor);
    Green := fpgGetGreen(AColor);
    Blue  := fpgGetBlue(AColor);
    Alpha := fpgGetAlpha(AColor);
  end
end;

function RGBTripleTofpgColor(const AColor: TRGBTriple): TfpgColor;
begin
  Result := AColor.Blue or (AColor.Green shl 8) or (AColor.Red shl 16) or (AColor.Alpha shl 24);
end;

function FPColorTofpgColor(const AColor: TFPColor): TfpgColor; deprecated;
begin
  Result := AColor.Blue or (AColor.Green shl 8) or (AColor.Red shl 16) or (AColor.Alpha shl 24);
end;

function fpgGetRed(const AColor: TfpgColor): byte;
var
  c: TfpgColor;
begin
  c := fpgColorToRGB(AColor);
  // AARRGGBB format
  Result := (c shr 16) and $FF;
end;

function fpgGetGreen(const AColor: TfpgColor): byte;
var
  c: TfpgColor;
begin
  c := fpgColorToRGB(AColor);
  // AARRGGBB format
  Result := (c shr 8) and $FF;
end;

function fpgGetBlue(const AColor: TfpgColor): byte;
var
  c: TfpgColor;
begin
  c := fpgColorToRGB(AColor);
  // AARRGGBB format
  Result := c and $FF;
end;

function fpgGetAlpha(const AColor: TfpgColor): byte;
var
  c: TfpgColor;
begin
  c := fpgColorToRGB(AColor);
  // AARRGGBB format
  Result := (c shr 24) and $FF;
end;

function fpgGetAvgColor(const AColor1, AColor2: TfpgColor): TfpgColor;
var
  c1, c2: TRGBTriple;
  avg: TRGBTriple;
begin
  c1 := fpgColorToRGBTriple(AColor1);
  c2 := fpgColorToRGBTriple(AColor2);
  avg.Red   := c1.Red + (c2.Red - c1.Red) div 2;
  avg.Green := c1.Green + (c2.Green - c1.Green) div 2;
  avg.Blue  := c1.Blue + (c2.Blue - c1.Blue) div 2;
  avg.Alpha := c1.Alpha + (c2.Alpha - c1.Alpha) div 2;
  Result := RGBTripleTofpgColor(avg);
end;

function fpgColor(const ARed, AGreen, ABlue: byte): TfpgColor;
begin
  { color is always fully opaque }
  Result := ABlue or (AGreen shl 8) or (ARed shl 16) or ($FF shl 24);
end;

function fpgColor(const ARed, AGreen, ABlue, AAlpha: byte): TfpgColor;
begin
  Result := ABlue or (AGreen shl 8) or (ARed shl 16) or (AAlpha shl 24);
end;

function fpgDarker(const AColor: TfpgColor; APercent: Byte): TfpgColor;
var
  lColor: TRGBTriple;
begin
  lColor.Red := fpgGetRed(AColor);
  lColor.Green := fpgGetGreen(AColor);
  lColor.Blue := fpgGetBlue(AColor);
  lColor.Red := Round(lColor.Red*APercent/100);
  lColor.Green := Round(lColor.Green*APercent/100);
  lColor.Blue := Round(lColor.Blue*APercent/100);
  Result := RGBTripleTofpgColor(lColor);
end;

function fpgLighter(const AColor: TfpgColor; APercent: Byte): TfpgColor;
var
  lColor: TRGBTriple;
begin
  lColor.Red := fpgGetRed(AColor);
  lColor.Green := fpgGetGreen(AColor);
  lColor.Blue := fpgGetBlue(AColor);
  lColor.Red := Round((lColor.Red*APercent/100) + (255 - APercent/100*255));
  lColor.Green := Round((lColor.Green*APercent/100) + (255 - APercent/100*255));
  lColor.Blue := Round((lColor.Blue*APercent/100) + (255 - APercent/100*255));
  Result := RGBTripleTofpgColor(lColor);
end;

function PtInRect(const ARect: TfpgRect; const APoint: TPoint): Boolean;
begin
  Result := (APoint.x >= ARect.Left) and
            (APoint.y >= ARect.Top) and
            (APoint.x <= ARect.Right) and
            (APoint.y <= ARect.Bottom);
end;

procedure SortRect(var ARect: TRect);
begin
  with ARect do
    SortRect(left, top, right, bottom);
end;

procedure SortRect(var ARect: TfpgRect);
var
  r: TfpgCoord;
  b: TfpgCoord;
begin
  r := ARect.Right;
  b := ARect.Bottom;
  SortRect(ARect.Left, ARect.Top, r, b);
  ARect.SetRight(r);
  ARect.SetBottom(b);
end;

procedure SortRect(var left, top, right, bottom: integer);
var
  r: integer;
begin
  if left > right then
  begin
    r       := left;
    left    := right;
    right   := r;
  end;
  if top > bottom then
  begin
    r       := top;
    top     := bottom;
    bottom  := r;
  end;
end;

// This function uses RTTI to automatically set the default values of properties.
// That means we don't have to do it in the constructor anymore! :-)
procedure SetDefaults(Obj: TObject);
var
  PropInfos: PPropList;
  Count, Loop: Integer;
begin
  PropInfos := nil;
  { Find out how many properties we'll be considering }
  Count := GetPropList(Obj.ClassInfo, tkPropsWithDefault, nil);
  { Allocate memory to hold their RTTI data }
  GetMem(PropInfos, Count * SizeOf(PPropInfo));
  try
    { Get hold of the property list in our new buffer }
    GetPropList(Obj.ClassInfo, tkPropsWithDefault, PropInfos);
    { Loop through all the selected properties }
    for Loop := 0 to Count - 1 do
    begin
      with PropInfos^[Loop]^ do
      begin
        { If there is supposed to be a default value... }
        if Default <> NoDefault then
          { ...then jolly well set it }
          SetOrdProp(Obj, PropInfos^[Loop], Default)
      end;
    end;
  finally
    FreeMem(PropInfos, Count * SizeOf(PPropInfo));
  end;
end;

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


{ TfpgPoint }

procedure TfpgPoint.SetPoint(AX, AY: integer);
begin
  X := AX;
  Y := AY;
end;

function TfpgPoint.ManhattanLength: integer;
begin
  Result := Abs(X) + Abs(Y);
end;

function TfpgPoint.ManhattanLength(const PointB: TfpgPoint): integer;
begin
  Result := Abs(PointB.X-X) + Abs(PointB.Y-Y);
end;


{ TfpgSize }

procedure TfpgSize.SetSize(AWidth, AHeight: integer);
begin
  W := AWidth;
  H := AHeight;
end;


{ TfpgWindowBase }

procedure TfpgWindowBase.SetMouseCursor(const AValue: TMouseCursor);
begin
  if FMouseCursor = AValue then
    Exit; //==>
  FMouseCursor := AValue;
  DoSetMouseCursor;
end;

function TfpgWindowBase.ConstraintWidth(NewWidth: TfpgCoord): TfpgCoord;
begin
  Result := NewWidth;
  if (MaxWidth >= MinWidth) and (Result > MaxWidth) and (MaxWidth > 0) then
    Result := MaxWidth;
  if Result < MinWidth then
    Result := MinWidth;
end;

function TfpgWindowBase.ConstraintHeight(NewHeight: TfpgCoord): TfpgCoord;
begin
  Result := NewHeight;
  if (MaxHeight >= MinHeight) and (Result > MaxHeight) and (MaxHeight > 0) then
    Result := MaxHeight;
  if Result < MinHeight then
    Result := MinHeight;
end;

function TfpgWindowBase.GetWindowState: TfpgWindowState;
begin
  Result := FWindowState;
end;

procedure TfpgWindowBase.SetWindowState(const AValue: TfpgWindowState);
begin
  // do nothing
end;

procedure TfpgWindowBase.DoDragStartDetected;
begin
  if Assigned(FOnDragStartDetected) then
    FOnDragStartDetected(self);
end;

procedure TfpgWindowBase.SetParent(const AValue: TfpgWindowBase);
begin
  FParent := AValue;
end;

function TfpgWindowBase.GetParent: TfpgWindowBase;
begin
  result := FParent;
end;

function TfpgWindowBase.GetCanvas: TfpgCanvasBase;
begin
  Result := FCanvas;
end;

procedure TfpgWindowBase.AllocateWindowHandle;
begin
  DoAllocateWindowHandle(FParent);
  if FMouseCursorIsDirty then
    DoSetMouseCursor;
end;

procedure TfpgWindowBase.ReleaseWindowHandle;
begin
  if HasHandle then
  begin
    Canvas.FreeResources;
    DoReleaseWindowHandle;
  end;
  DoRemoveWindowLookup;
end;

procedure TfpgWindowBase.SetWindowTitle(const ATitle: string);
begin
  DoSetWindowTitle(ATitle);
end;

procedure TfpgWindowBase.SetTop(const AValue: TfpgCoord);
begin
  HandleMove(Left, AValue);
end;

procedure TfpgWindowBase.SetLeft(const AValue: TfpgCoord);
begin
  HandleMove(AValue, Top);
end;

procedure TfpgWindowBase.SetHeight(const AValue: TfpgCoord);
begin
  HandleResize(Width, AValue);
end;

procedure TfpgWindowBase.SetWidth(const AValue: TfpgCoord);
begin
  HandleResize(AValue, Height);
end;

procedure TfpgWindowBase.HandleMove(x, y: TfpgCoord);
begin
  if FTop <> y then
  begin
    if not (csLoading in ComponentState) then
      FPrevTop := FTop
    else
      FPrevTop := y;
    FTop := y;
    FPosIsDirty := FPosIsDirty or (FTop <> FPrevTop);
  end;

  if FLeft <> x then
  begin
    if not (csLoading in ComponentState) then
      FPrevLeft := FLeft
    else
      FPrevLeft := x;
    FLeft := x;
    FPosIsDirty := FPosIsDirty or (FLeft <> FPrevLeft);
  end;
end;

procedure TfpgWindowBase.HandleResize(AWidth, AHeight: TfpgCoord);
begin
  if FWidth <> AWidth then
  begin
    if not (csLoading in ComponentState) then
      FPrevWidth := FWidth
    else
      FPrevWidth := AWidth;
    FWidth := ConstraintWidth(AWidth);
    FSizeIsDirty := FSizeIsDirty or (FWidth <> FPrevWidth);
  end;

  if FHeight <> AHeight then
  begin
    if not (csLoading in ComponentState) then
      FPrevHeight := FHeight
    else
      FPrevHeight := AHeight;
    FHeight := ConstraintHeight(AHeight);
    FSizeIsDirty := FSizeIsDirty or (FHeight <> FPrevHeight);
  end;
end;

constructor TfpgWindowBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMouseCursor := mcDefault;
  FMouseCursorIsDirty := False;
  FPosIsDirty := True;
  FSizeIsDirty := True;
  FMaxWidth := 0;
  FMaxHeight := 0;
  FDragActive := False;
  FWindowState := wsNormal;
end;

procedure TfpgWindowBase.AfterConstruction;
begin
  inherited AfterConstruction;
  { There is a neater way by using RTTI to set default property values all
    automatically. No need to duplicate the efforts and manually set the
    property default values in the constructor. This code is now the same for
    each TfpgWindowBase descendant (which includes GUI widgets) }
//  SetDefaults(self);
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
  DoUpdateWindowPosition;
end;

procedure TfpgWindowBase.MoveWindow(const x: TfpgCoord; const y: TfpgCoord);
begin
  Left  := x;
  Top   := y;
  DoMoveWindow(x, y);
end;

function TfpgWindowBase.WindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint;
begin
  Result := DoWindowToScreen(ASource, AScreenPos);
end;

function TfpgWindowBase.HasParent: Boolean;
begin
  Result := FParent <> nil;
end;

function TfpgWindowBase.GetClientRect: TfpgRect;
begin
  Result.SetRect(0, 0, Width, Height);
end;

function TfpgWindowBase.GetBoundsRect: TfpgRect;
begin
  Result.SetRect(Left, Top, Width+1, Height+1);
end;

procedure TfpgWindowBase.SetFullscreen(AValue: Boolean);
begin
  if AValue then
    Include(FWindowAttributes, waFullScreen)
  else
    Exclude(FWindowAttributes, waFullScreen);
  // now decendants must override this and implement the actualy fullscreen part
end;

{ TfpgCanvasBase }

procedure TfpgCanvasBase.SetInterpolation(const AValue: TfpgCustomInterpolation);
begin
  FInterpolation.Free;
  FInterpolation := AValue;
end;

constructor TfpgCanvasBase.Create(awin: TfpgWindowBase);
begin
  FBufferedDraw := True;
  FFastDoubleBuffer := True;
  FWindow := awin;
end;

destructor TfpgCanvasBase.Destroy;
begin
  FInterpolation.Free;
  inherited Destroy;
end;

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

procedure TfpgCanvasBase.DrawLineClipped(var x1, y1, x2, y2: TfpgCoord;
  const AClipRect: TfpgRect);
var
  OutOfRegion: boolean;
begin
  ClipLine(X1, Y1, X2, Y2, AClipRect, OutOfRegion);
  if not OutOfRegion then
    DrawLine(X1, Y1, X2, Y2);                { Draw the new line!            }
end;

{ DrawLineClipped - This procedure clips a line to the AClipRect boundaries and
 then calls the DrawLine procedure with the clipped coordinates.  If the line
 lies completely outside of the clip boundary, then the Line routine is not
 called.  This procedure uses the well known Cohen-Sutherland line clipping
 algorithm to clip each coordinate.

 Use this if you did not what to change the Canvas.ClipRegion for some reason.
 For a detailed explanation see:
   http://www.nondot.org/~sabre/graphpro/line6.html                           }
procedure TfpgCanvasBase.ClipLine(var x1, y1, x2, y2: TfpgCoord;
  const AClipRect: TfpgRect; out FallsOutsideRegion: Boolean);
CONST
  CodeBottom = 1; CodeTop    = 2;             { BitFields for output codes }
  CodeLeft   = 4; CodeRight  = 8;

  FUNCTION CompOutCode(X, Y : INTEGER) : integer;  { Nested function }
  VAR Code : integer;
  BEGIN
    Code := 0;
    IF      Y > AClipRect.Bottom THEN Code := CodeBottom
    ELSE IF Y < AClipRect.Top THEN Code := CodeTop;
    IF      X > AClipRect.Right THEN Code := Code+CodeRight
    ELSE IF X < AClipRect.Left THEN Code := Code+CodeLeft;
    Result := Code;
  END;

VAR
  OutCode0,         { The code of the first endpoint  }
  OutCode1,         { The code of the second endpoint }
  OutCodeOut : integer;
  X, Y : INTEGER;
BEGIN
  FallsOutsideRegion := False;
  OutCode0 := CompOutCode(X1, Y1);            { Compute the original codes   }
  OutCode1 := CompOutCode(X2, Y2);

  WHILE (OutCode0 <> 0) OR (OutCode1 <> 0) DO { While not Trivially Accepted }
  BEGIN
    IF (OutCode0 AND OutCode1) <> 0 THEN      { Trivial Reject }
    begin
      FallsOutsideRegion := True;
      Exit;   //==>
    end
    ELSE
    BEGIN        { Failed both tests, so calculate the line segment to clip }
      IF OutCode0 > 0 THEN
        OutCodeOut := OutCode0    { Clip the first point }
      ELSE
        OutCodeOut := OutCode1;   { Clip the last point  }

      IF (OutCodeOut AND CodeBottom) = CodeBottom THEN
      BEGIN               { Clip the line to the bottom of the viewport     }
        Y := AClipRect.Bottom;
        X := X1+LONGINT(X2-X1)*LONGINT(Y-Y1) DIV (Y2 - Y1);
      END
      ELSE IF (OutCodeOut AND CodeTop) = CodeTop THEN
      BEGIN               { Clip the line to the top of the viewport        }
        Y := AClipRect.Top;
        X := X1+LONGINT(X2-X1)*LONGINT(Y-Y1) DIV (Y2 - Y1);
      END
      ELSE IF (OutCodeOut AND CodeRight) = CodeRight THEN
      BEGIN               { Clip the line to the right edge of the viewport }
        X := AClipRect.Right;
        Y := Y1+LONGINT(Y2-Y1)*LONGINT(X-X1) DIV (X2-X1);
      END
      ELSE IF (OutCodeOut AND CodeLeft) = CodeLeft THEN
      BEGIN               { Clip the line to the left edge of the viewport  }
        X := AClipRect.Left;
        Y := Y1+LONGINT(Y2-Y1)*LONGINT(X-X1) DIV (X2-X1);
      END;

      IF (OutCodeOut = OutCode0) THEN       { Modify the first coordinate   }
      BEGIN
        X1 := X; Y1 := Y;                   { Update temporary variables    }
        OutCode0 := CompOutCode(X1, Y1);    { Recalculate the OutCode       }
      END
      ELSE                                  { Modify the second coordinate  }
      BEGIN
        X2 := X; Y2 := Y;                   { Update temporary variables    }
        OutCode1 := CompOutCode(X2, Y2);    { Recalculate the OutCode       }
      END;
    END;
  END;  { while }
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

procedure TfpgCanvasBase.DrawArc(x, y, w, h: TfpgCoord; a1, a2: double);
begin
  DoDrawArc(x, y, w, h, a1, a2);
end;

{ Use Polygon to draw a closed, many-sided shape on the canvas, using the value
  of Canvas.Color. The shape is always filled.
  The Points parameter is an array of points that give the vertices of the
  polygon.
  Winding determines how the polygon is filled. When Winding is True, Polygon
  fills the shape using the Winding fill algorithm. When Winding is False,
  Polygon uses the even-odd (alternative) fill algorithm.
  StartIndex gives the index of the first point in the array to use. All points
  before this are ignored.
  NumPts indicates the number of points to use, starting at StartIndex.
  If NumPts is -1 (the default), Polygon uses all points from StartIndex to the
  end of the array.
  The first point is always connected to the last point.
  To draw a polygon on the canvas, without filling it, use the Polyline method,
  specifying the first point a second time at the end. }
procedure TfpgCanvasBase.DrawPolygon(const Points: array of TPoint;
  Winding: Boolean; StartIndex: Integer; NumPts: Integer);
var
  NPoints: integer;
begin
  if NumPts<0 then
    NPoints:=High(Points)-StartIndex+1
  else
    NPoints:=NumPts;
  if NPoints<=0 then exit;
  DrawPolygon(@Points[StartIndex],NPoints,Winding);
end;

procedure TfpgCanvasBase.DrawPolygon(Points: PPoint; NumPts: Integer;
  Winding: boolean);
begin
  if NumPts<=0 then exit;
  DoDrawPolygon(Points,NumPts,Winding);
end;

procedure TfpgCanvasBase.DrawPolygon(const Points: array of TPoint);
begin
  DrawPolygon(Points, True, Low(Points), High(Points) - Low(Points) + 1);
end;

procedure TfpgCanvasBase.StretchDraw(x, y, w, h: TfpgCoord; ASource: TfpgImageBase);
var
  FreeInterpolation: boolean;
  IP: TfpgCustomInterpolation;
begin
  FreeInterpolation := not Assigned(FInterpolation);
  if FreeInterpolation then
    IP := TfpgMitchelInterpolation.Create
  else
    IP := FInterpolation;
  try
    IP.Initialize(ASource, self);
    IP.Execute(x, y, w, h);
  finally
    if FreeInterpolation then
      IP.Free;
  end;
end;

procedure TfpgCanvasBase.CopyRect(ADest_x, ADest_y: TfpgCoord; ASrcCanvas: TfpgCanvasBase;
  var ASrcRect: TfpgRect);
var
  x, sx, y, sy: TfpgCoord;
begin
  SortRect(ASrcRect);
  // X position of source
  for sx := ASrcRect.Left to ASrcRect.Right do
  begin
    x := ADest_x + (sx - ASrcRect.Left);  // calc dest x
    // Y position of source
    for sy := ASrcRect.Top to ASrcRect.Bottom do
    begin
      y := ADest_y + (sy - ASrcRect.Top); // calc dest y
      Pixels[x, y] := ASrcCanvas.Pixels[sx, sy];
    end;
  end;
end;

procedure TfpgCanvasBase.DrawString(x, y: TfpgCoord; const txt: string);
var
  underline: integer;
begin
  DoDrawString(x, y, txt);

  { What was not handled: underline }
  if Pos('UNDERLINE', UpperCase(Font.FontDesc)) > 0 then
  begin
    underline := (Font.Descent div 2) + 1;
    if underline = 0 then
      underline := 1;
    if underline >= Font.Descent then
      underline := Font.Descent - 1;

    DoSetLineStyle(1, lsSolid);
    DoSetColor(TextColor);
    DoDrawLine(x, y+Font.Height-underline, x+Font.TextWidth(txt), y+Font.Height-underline);
  end;
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

procedure TfpgCanvasBase.FillArc(x, y, w, h: TfpgCoord; a1, a2: double);
begin
  DoFillArc(x, y, w, h, a1, a2);
end;

procedure TfpgCanvasBase.GradientFill(ARect: TfpgRect; AStart, AStop: TfpgColor;
  ADirection: TGradientDirection);
var
  RGBStart: TRGBTriple;
  RGBStop: TRGBTriple;
  RDiff, GDiff, BDiff: Integer;
  count: Integer;
  i: Integer;
  newcolor: TRGBTriple;
begin
  RGBStart := fpgColorToRGBTriple(AStart);
  RGBStop  := fpgColorToRGBTriple(AStop);

  if ADirection = gdVertical then
    count := ARect.Bottom - ARect.Top
  else
    count := ARect.Right - ARect.Left;

  RDiff := RGBStop.Red - RGBStart.Red;
  GDiff := RGBStop.Green - RGBStart.Green;
  BDiff := RGBStop.Blue - RGBStart.Blue;

//  Changing;
  for i := 0 to count do
  begin
    newcolor.Red    := RGBStart.Red + (i * RDiff) div count;
    newcolor.Green  := RGBStart.Green + (i * GDiff) div count;
    newcolor.Blue   := RGBStart.Blue + (i * BDiff) div count;
    SetColor(RGBTripleTofpgColor(newcolor));

    // We have to overshoot by 1 pixel as DrawLine paints 1 pixel short (by design)
    if ADirection = gdHorizontal then
      DrawLine(ARect.Left+i, ARect.Top, ARect.Left+i, ARect.Bottom+1)
    else
      DrawLine(ARect.Left, ARect.Top+i, ARect.Right+1, ARect.Top+i);
  end;
//  Changed;
end;

procedure TfpgCanvasBase.XORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord);
begin
  DoXORFillRectangle(col, x, y, w, h);
end;

procedure TfpgCanvasBase.XORFillRectangle(col: TfpgColor; r: TfpgRect);
begin
  DoXORFillRectangle(col, r.Left, r.Top, r.Width, r.Height);
end;

procedure TfpgCanvasBase.SetClipRect(const ARect: TfpgRect);
begin
  DoSetClipRect(ARect);
end;

function TfpgCanvasBase.GetClipRect: TfpgRect;
begin
  Result := DoGetClipRect;
end;

function TfpgCanvasBase.GetLineWidth: integer;
begin
  Result := FLineWidth;
end;

procedure TfpgCanvasBase.AddClipRect(const ARect: TfpgRect);
begin
  DoAddClipRect(ARect);
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

procedure TfpgCanvasBase.GetWinRect(out r: TfpgRect);
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
  if AFont = nil then
    exit;
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

procedure TfpgCanvasBase.EndDraw(ARect: TfpgRect);
begin
  EndDraw(ARect.Left, ARect.Top, ARect.Width, ARect.Height);
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

function TfpgFontBase.GetIsFixedWidth: boolean;
begin
  // very crude but handy as a fallback option
  if (Pos('mono', Lowercase(FFontDesc)) > 0) or
     (Pos('courier', Lowercase(FFontDesc)) > 0) or
     (Pos('fixed', Lowercase(FFontDesc)) > 0) then
    Result := True
  else
    Result := False;
end;

function TfpgFontBase.TextWidth(const txt: TfpgString): integer;
begin
  if Length(txt) = 0 then
    Result := 0
  else
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

{ TfpgCustomInterpolation }

procedure TfpgCustomInterpolation.Initialize(AImage: TfpgImageBase;
  ACanvas: TfpgCanvasBase);
begin
  FImage  := AImage;
  FCanvas := ACanvas;
end;

{ TfpgBaseInterpolation }

type
  TfpgInterpolationContribution = record
    weight: double;
    place: integer;
  end;

function ColorRound(c: double): word;
begin
  if c > $FFFF then
    result := $FFFF
  else if c < 0.0 then
    result := 0
  else
    result := round(c);
end;


procedure TfpgBaseInterpolation.Horizontal(width: integer);
var
  x, y, r: integer;
  start, stop, maxcontribs: integer;
  center, re, gr, bl, density: double;
  contributions: array[0..10] of TfpgInterpolationContribution;
  dif, w, gamma, a: double;
  c: TfpgColor;
  rgb: TRGBTriple;
begin
  for x := 0 to Width - 1 do
  begin
    center := x * xfactor;
    start  := round(center - xsupport);
    if start < 0 then
      start := 0;
    stop := round(center + xsupport);
    if stop >= image.Width then
      stop := image.Width - 1;
    density     := 0.0;
    maxcontribs := -1;
    for r := start to stop do
    begin
      dif := r - center;
      w   := Filter(dif);
      if w > 0.0 then
      begin
        Inc(maxcontribs);
        with contributions[maxcontribs] do
        begin
          weight  := w;
          density := density + w;
          place   := r;
        end;
      end;
    end;
    if (density <> 0.0) and (density <> 1.0) then
    begin
      density := 1.0 / density;
      for r := 0 to maxcontribs do
        contributions[r].weight := contributions[r].weight * density;
    end;
    for y := 0 to image.Height - 1 do
    begin
      gamma := 0.0;
      re    := 0.0;
      gr    := 0.0;
      bl    := 0.0;
      for r := 0 to maxcontribs do
        with contributions[r] do
        begin
          c   := image.colors[place, y];
          rgb := fpgColorToRGBTriple(c);
          a     := weight; // * rgb.Alpha / $FFFF;
          re    := re + a * rgb.Red;
          gr    := gr + a * rgb.Green;
          bl    := bl + a * rgb.Blue;
          gamma := gamma + a;
        end;  { with }
      with rgb do
      begin
        red   := ColorRound(re);
        green := ColorRound(gr);
        blue  := ColorRound(bl);
//        alpha := ColorRound(gamma * $FFFF);
      end;
      tempimage.colors[x, y] := RGBTripleTofpgColor(rgb);
    end;
  end;
end;

procedure TfpgBaseInterpolation.Vertical(dx, dy, width, height: integer);
var
  x, y, r: integer;
  start, stop, maxcontribs: integer;
  center, re, gr, bl, density: double;
  contributions: array[0..10] of TfpgInterpolationContribution;
  dif, w, gamma, a: double;
  c: TfpgColor;
  rgb: TRGBTriple;
begin
  for y := 0 to Height - 1 do
  begin
    center := y * yfactor;
    start  := round(center - ysupport);
    if start < 0 then
      start := 0;
    stop := round(center + ysupport);
    if stop >= tempimage.Height then
      stop := tempimage.Height - 1;
    density     := 0.0;
    maxcontribs := -1;
    for r := start to stop do
    begin
      dif := r - center;
      w   := Filter(dif);
      if w > 0.0 then
      begin
        Inc(maxcontribs);
        with contributions[maxcontribs] do
        begin
          weight  := w;
          density := density + w;
          place   := r;
        end;
      end;
    end;
    if (density <> 0.0) and (density <> 1.0) then
    begin
      density := 1.0 / density;
      for r := 0 to maxcontribs do
        contributions[r].weight := contributions[r].weight * density;
    end;
    for x := 0 to Width - 1 do
    begin
      gamma := 0.0;
      re    := 0.0;
      gr    := 0.0;
      bl    := 0.0;
      for r := 0 to maxcontribs do
        with contributions[r] do
        begin
          c := tempimage.colors[x, place];
          rgb := fpgColorToRGBTriple(c);
          a     := weight;// * rgb.alpha / $FFFF;
          re    := re + a * rgb.red;
          gr    := gr + a * rgb.green;
          bl    := bl + a * rgb.blue;
          gamma := gamma + a;
        end;  { width }
      with rgb do
      begin
        red   := ColorRound(re);
        green := ColorRound(gr);
        blue  := ColorRound(bl);
//        alpha := ColorRound(gamma * $FFFF);
      end;
      Canvas.Pixels[x + dx, y + dy] := RGBTripleTofpgColor(rgb);
    end;
  end;
end;

procedure TfpgBaseInterpolation.Execute(x, y, w, h: integer);
begin
  tempimage := TfpgImageBase.Create;
  tempimage.AllocateImage(image.ColorDepth, w, image.Height);

  xfactor   := image.Width / w;
  yfactor   := image.Height / h;
  if xfactor > 1.0 then
    xsupport := MaxSupport
  else
    xsupport := xfactor * MaxSupport;
  if yfactor > 1.0 then
    ysupport := MaxSupport
  else
    ysupport := yfactor * MaxSupport;
  Horizontal(w);
  Vertical(x, y, w, h);
end;

destructor TfpgBaseInterpolation.Destroy;
begin
  tempimage.Free;
  inherited Destroy;
end;

{ TfpgMitchelInterpolation }

function TfpgMitchelInterpolation.Filter(x: double): double;
const
  B  = (1.0/3.0);
  C  = (1.0/3.0);
  P0 = ((  6.0- 2.0*B       )/6.0);
  P2 = ((-18.0+12.0*B+ 6.0*C)/6.0);
  P3 = (( 12.0- 9.0*B- 6.0*C)/6.0);
  Q0 = ((       8.0*B+24.0*C)/6.0);
  Q1 = ((     -12.0*B-48.0*C)/6.0);
  Q2 = ((       6.0*B+30.0*C)/6.0);
  Q3 = ((     - 1.0*B- 6.0*C)/6.0);
begin
  if (x < -2.0) then
    result := 0.0
  else if (x < -1.0) then
    result := Q0-x*(Q1-x*(Q2-x*Q3))
  else if (x < 0.0) then
    result := P0+x*x*(P2-x*P3)
  else if (x < 1.0) then
    result := P0+x*x*(P2+x*P3)
  else if (x < 2.0) then
    result := Q0+x*(Q1+x*(Q2+x*Q3))
  else
  result := 0.0;
end;

function TfpgMitchelInterpolation.MaxSupport: double;
begin
  result := 2.0;
end;


{ TfpgImageBase }

function TfpgImageBase.GetColor(x, y: TfpgCoord): TfpgColor;
var
  p: Plongword;
begin
  p := FImageData;
  Inc(p, (FWidth * y) + x);
  Result := TfpgColor(p^);
//  write(IntToHex(Result, 6) + ' ');
end;

procedure TfpgImageBase.SetColor(x, y: TfpgCoord; const AValue: TfpgColor);
var
  p: Plongword;
begin
  p := FImageData;
  Inc(p, (FWidth * y) + x);
  p^ := AValue;
//  write(IntToHex(AValue, 6) + ' ');
end;

constructor TfpgImageBase.Create;
begin
  FWidth      := 0;
  FHeight     := 0;
  FColorDepth := 0;

  FImageData     := nil;
  FImageDataSize := 0;
  FMaskData      := nil;
  FMaskDataSize  := 0;
  FMasked        := False;
  FMaskPoint     := Point(0, 0);
end;

destructor TfpgImageBase.Destroy;
begin
  FreeImage;
  inherited Destroy;
end;

procedure TfpgImageBase.Invert(IncludeMask: Boolean);
var
  p: ^byte;
  n: integer;
begin
  if FImageData = nil then
    Exit; //==>

  p := FImageData;
  for n := 1 to FImageDataSize do
  begin
    p^ := p^ xor $FF;
    Inc(p);
  end;

  if IncludeMask then
  begin
    if FMaskData <> nil then
    begin
      p := FMaskData;
      for n := 1 to FMaskDataSize do
      begin
        p^ := p^ xor $FF;
        Inc(p);
      end;
    end;
  end;
end;

procedure TfpgImageBase.FreeImage;
begin
  if FImageData <> nil then
    FreeMem(FImageData, FImageDataSize);
  FImageData     := nil;
  FImageDataSize := 0;
  if FMaskData <> nil then
    FreeMem(FMaskData, FMaskDataSize);
  FMaskData     := nil;
  FMaskDataSize := 0;
  FMasked       := False;
  FWidth        := 0;
  FHeight       := 0;
//  DoFreeImage;
end;

procedure TfpgImageBase.AllocateImage(acolordepth, awidth, aheight: integer);
var
  dww: integer;
begin
  FreeImage;
  FWidth      := awidth;
  FHeight     := aheight;
  FColorDepth := acolordepth;

  // Real bitmap
  if FColorDepth = 1 then
    dww := (awidth + 31) div 32
  else
    dww := FWidth;

  FImageDataSize := dww * FHeight * 4;
  FImageData := nil;
  GetMem(FImageData, FImageDataSize);
  if FImageData = nil then
    raise Exception.Create('Failed to allocate ' + IntToStr(FImageDataSize) + 'bytes of memory for FImageData');
end;

procedure TfpgImageBase.AllocateMask;
var
  dww: integer;
begin
  if (FWidth < 1) or (FHeight < 1) then
    Exit; //==>

  FMasked := True;
  if FMaskData <> nil then
    FreeMem(FMaskData);

  dww           := (FWidth + 31) div 32;
  FMaskDataSize := dww * FHeight * 4;
  GetMem(FMaskData, FMaskDataSize);
end;

procedure TfpgImageBase.CreateMaskFromSample(x, y: TfpgCoord);
var
  p: ^longword;
  pmsk: ^byte;
  c, n: longword;
  linecnt: integer;
  pixelcnt: integer;
  bit: byte;
  msklinelen: integer;
  row, col: integer;
begin
  if FColorDepth = 1 then
    Exit; //==>

  if (FImageData = nil) then
    Exit; //==>

{$ifdef AGGCanvas}
  p := FImageData;
  if x < 0 then
    Inc(p, FWidth - 1)
  else
    Inc(p, x);
  if y < 0 then
    Inc(p, FWidth * (FHeight - 1))
  else
    Inc(p, FWidth * y);

  c := p^;  // the sample

  for row := 0 to FHeight-1 do
  begin
    for col := 0 to FWidth-1 do
    begin
      n := PLongWord(FImageData)[row * FWidth + col];
      if n = c then
        { set Alpha value 100% transparent }
        PLongWord(FImageData)[row * FWidth + col] := n and $00FFFFFF;
    end;
  end;

{$else}

  AllocateMask;
  FMaskPoint := Point(x, y);

  p := FImageData;
  if x < 0 then
    Inc(p, FWidth - 1)
  else
    Inc(p, x);
  if y < 0 then
    Inc(p, FWidth * (FHeight - 1))
  else
    Inc(p, FWidth * y);

  c := p^;  // the sample

  msklinelen := FWidth div 32;
  if (FWidth and $1F) > 0 then
    Inc(msklinelen);

  msklinelen := msklinelen shl 2;

  p       := FImageData;
  linecnt := 0;

  repeat
    pixelcnt := 0;
    bit      := $80;
    pmsk     := FMaskData;
    Inc(pmsk, linecnt * msklinelen);

    repeat
      if bit = $80 then
        pmsk^ := 0;

      if p^ <> c then
        pmsk^ := pmsk^ or bit;

      Inc(p);
      Inc(pixelcnt);

      if bit = 1 then
      begin
        bit := $80;
        Inc(pmsk);
      end
      else
        bit := bit shr 1;
    until pixelcnt >= FWidth;

    Inc(linecnt);
  until linecnt >= FHeight;
{$endif}
end;

procedure TfpgImageBase.UpdateImage;
begin
  if FImageData <> nil then
    DoInitImage(FColorDepth, FWidth, FHeight, FImageData);

  if FMaskData <> nil then
    DoInitImageMask(FWidth, FHeight, FMaskData);
end;

{ TfpgApplicationBase }

function TfpgApplicationBase.GetTopModalForm: TfpgWindowBase;
begin
  Result := nil;
  if (FModalFormStack <> nil) and (FModalFormStack.Count > 0) then
    Result := TFpgWindowBase(FModalFormStack.Items[FModalFormStack.Count-1]);
end;

function TfpgApplicationBase.GetHelpFile: TfpgString;
begin
  Result := FHelpFile;
  //if Result = '' then
  //begin
    { TODO : Should we extend this to try the <applicationname>.inf as a help file? }
  //end;
end;

function TfpgApplicationBase.GetHelpViewer: TfpgString;
var
  ext: TfpgString;
begin
  // Default location is in same directory as current running application
  // This location might change in the future.
  ext := fpgExtractFileExt(ParamStr(0));
  Result := fpgExtractFilePath(ParamStr(0)) + FPG_HELPVIEWER + ext;
end;

constructor TfpgApplicationBase.Create(const AParams: string);
begin
  inherited Create(nil);
  FModalFormStack := TList.Create;
  FCritSect := TCriticalSection.Create;
  FHelpKey := keyF1;
  FHelpType := htContext;
end;

destructor TfpgApplicationBase.Destroy;
begin
  FCritSect.Free;
  inherited Destroy;
end;

function TfpgApplicationBase.GetFormCount: integer;
begin
  Result := ComponentCount;
end;

function TfpgApplicationBase.GetForm(Index: Integer): TfpgWindowBase;
begin
  Result := TfpgWindowBase(Components[Index]);
end;

function TfpgApplicationBase.GetFontFaceList: TStringList;
begin
  Result := DoGetFontFaceList;
end;

procedure TfpgApplicationBase.PushModalForm(AForm: TfpgWindowBase);
var
  StackIndex: Integer;
begin
  if FModalFormStack = nil then
    Exit;
  StackIndex := FModalFormStack.IndexOf(AForm);
  if StackIndex = -1 then
    FModalFormStack.Add(AForm)
  //else move to top of stack?
end;

procedure TfpgApplicationBase.PopModalForm;
begin
  if FModalFormStack = nil then
    Exit;
  if FModalFormStack.Count > 0 then
    FModalFormStack.Delete(FModalFormStack.Count-1);
end;

function TfpgApplicationBase.PrevModalForm: TfpgWindowBase;
begin
  Result := nil;
  if FModalFormStack = nil then
    Exit;
  if FModalFormStack.Count < 2 then
    Exit;

  Result := TfpgWindowBase(FModalFormStack.Items[FModalFormStack.Count-2]);
end;

function TfpgApplicationBase.RemoveWindowFromModalStack (AForm: TfpgWindowBase): Integer;
begin
  Result := FModalFormStack.Remove(AForm);
end;

procedure TfpgApplicationBase.CreateForm(InstanceClass: TComponentClass; out Reference);
var
  Instance: TComponent;
  ok: boolean;
  AForm: TfpgForm;
begin
  // Allocate the instance, without calling the constructor
  Instance := TComponent(InstanceClass.NewInstance);
  // set the Reference before the constructor is called, so that
  // events and constructors can refer to it
  TComponent(Reference) := Instance;

  ok:=false;
  try
    Instance.Create(Self);
    ok:=true;
  finally
    if not ok then
    begin
      TComponent(Reference) := nil;
    end;
  end;

  if (Instance is TfpgForm) then
  begin
    AForm := TfpgForm(Instance);
    if FMainForm = nil then
      FMainForm := AForm;
  end;
end;

procedure TfpgApplicationBase.Terminate;
var
  i: integer;
begin
  // make sure all forms are closed before main form
  for i := FormCount - 1 downto 0 do
    if Forms[i] <> MainForm then
      fpgSendMessage(Self, Forms[i], FPGM_CLOSE); // SendMessage waits for it to complete. Post doesn't.
  Terminated := True;
end;

procedure TfpgApplicationBase.Lock;
begin
  FCritSect.Enter;
end;

procedure TfpgApplicationBase.Unlock;
begin
  FCritSect.Leave;
end;

procedure TfpgApplicationBase.InvokeHelp;
begin
  { TODO -oGraeme -cHelp System : We should probably try ActiveForm and ActiveWidget help first. }
  if HelpType = htKeyword then
    KeywordHelp(HelpKeyword)
  else
    ContextHelp(HelpContext);
end;

function TfpgApplicationBase.ContextHelp(const AHelpContext: THelpContext): Boolean;
var
  p: TProcess;
begin
  Result := False;
  if not fpgFileExists(GetHelpViewer) then
    raise EfpGUIUserFeedbackException.Create(rsfailedtofindhelpviewer);
  p := TProcess.Create(nil);
  try
    if fpgFileExists(HelpFile) then
    begin
      if AHelpContext = 0 then
        p.CommandLine := GetHelpViewer + ' ' + HelpFile
      else
        p.CommandLine := GetHelpViewer + ' ' + HelpFile + ' -n ' + IntToStr(AHelpContext);
        {$ifdef GDEBUG}
        senddebug(p.CommandLine);
        {$endif}
    end
    else
      p.CommandLine := GetHelpViewer;
    Result := True;
    p.Execute;
  finally
    p.Free;
  end;
end;

function TfpgApplicationBase.KeywordHelp(const AHelpKeyword: string): Boolean;
var
  p: TProcess;
begin
  Result := False;
  if not fpgFileExists(GetHelpViewer) then
    raise EfpGUIUserFeedbackException.Create(rsfailedtofindhelpviewer);
  p := TProcess.Create(nil);
  try
    if fpgFileExists(HelpFile) then
    begin
      p.CommandLine := GetHelpViewer + ' ' + HelpFile + ' -s ' + AHelpKeyword;
      {$ifdef GDEBUG}
      senddebug(p.CommandLine);
      {$endif}
    end
    else
      p.CommandLine := GetHelpViewer;
    Result := True;
    p.Execute;
  finally
    p.Free;
  end;
end;

{ TfpgClipboardBase }

constructor TfpgClipboardBase.Create;
begin
  inherited Create;
  InitClipboard;
end;

// Helper functions for TFileEntry and TfpgFileListBase

function StringMatches(const astr, apat: string): boolean;
var
  pati, si: longint;
begin
  result := True;
  pati := 1;
  si := 1;
  while result and (si <= length(astr)) and (pati <= length(apat)) do
  begin
    if (apat[pati] = '?') or (apat[pati] = astr[si]) then
    begin
      inc(si);
      inc(pati);
    end
    else if (apat[pati] = '*') then
    begin
      while (pati <= length(apat)) and (apat[pati] in ['?','*']) do
        inc(pati);
      if pati > length(apat) then
      begin
        si := length(astr)+1;
        Break;   // * at the end
      end;

      while (si <= length(astr)) and (astr[si] <> apat[pati]) do
        inc(si);
      if si > length(astr) then
        result := False;
    end
    else
    begin
      result := False;
    end;
  end;

  result := result and (si > length(astr));
end;

// multiple patterns separated with ;
function FileNameMatches(const astr, apats: string): boolean;
var
  cpat: string;
  p: integer;
  s: string;
  astrupper: string;
begin
  astrupper := UpperCase(astr);
  result := False;
  s := apats;
  repeat
    cpat := '';
    p := pos(';',s);
    if p > 0 then
    begin
      cpat := copy(s, 1, p-1);
      delete(s, 1, p);
    end
    else
    begin
      cpat := s;
      s := '';
    end;  { if/else }
    cpat := UpperCase(trim(cpat));
    if cpat <> '' then
      result := StringMatches(astrupper, cpat);
  until result or (cpat = '');
end;

{ TFileEntry }

constructor TFileEntry.Create;
begin
  {FAttributes := 0;
  FMode := 0;}
  FAttrString := '';
  FModeString := '';
  FSize := 0;
  FIsLink := False;
  FIsExecutable := false;
  FEntryType := etFile;
end;

{ TfpgFileListBase }

procedure TfpgFileListBase.AddEntry(sr: TSearchRec);
var
  e: TFileEntry;
begin
  e := InitializeEntry(sr);
  if Assigned(e) then
    FEntries.Add(e);
end;

function TfpgFileListBase.HasAttrib(fileAttrib, testAttrib: Integer): Boolean;
begin
  { HasAttrib() tests whether or not a file (with attributes fileAttrib) has the
  testAttrib attribute bit set. }
  Result := (fileAttrib and testAttrib) <> 0;
end;

function TfpgFileListBase.GetEntry(i: integer): TFileEntry;
begin
  if (i < 0) or (i > FEntries.Count-1) then
    Result := nil
  else
    Result := TFileEntry(FEntries[i]);
end;

function TfpgFileListBase.InitializeEntry(sr: TSearchRec): TFileEntry;
var
  e: TFileEntry;
begin
  e := TFileEntry.Create;
  e.Name        := fpgFromOSEncoding(sr.Name);
  e.Extension   := fpgExtractFileExt(e.Name);
  e.Size        := sr.Size;
  // e.Attributes  := sr.Attr; // this is incorrect and needs to improve!
  e.ModTime     := FileDateToDateTime(sr.Time);

  if HasAttrib(sr.Attr, faDirectory) then
    e.EntryType := etDir
  else
    e.EntryType := etFile;

  if (e.Name = '.') or
     ((e.Name = '..') and (FDirectoryName = '/')) or
     (not FShowHidden and (Copy(e.Name, 1, 1) = '.') and (Copy(e.Name, 2, 1) <> '.')) or
//       (not FShowHidden and HasAttrib(sr.Attr, faHidden)) or
     ((e.EntryType = etFile) and not FileNameMatches(e.Name, FFileMask)) then
  begin
    // do not add this entry
    e.Free;
    Result := nil;
  end else
    Result := e;
end;

procedure TfpgFileListBase.PopulateSpecialDirs(const aDirectory: TfpgString);
{Sets up FSpecialDirs list}
var
  i, n, sp: integer;
begin
  // FSpecialDirs under Windows will be all available drive letters.
  // FSpecialDirs under Linux is the root (/)

  // find insert position in FSpecialDirs where we can insert all parts
  // of aDirectory.
  i := 0;
  // We have to use UpperCase() because under Windows aDirectory's drive
  // letter could be lower case, but Win API returns initial drive letters
  // in upper case, so the second test could never be false causing
  // Index out of bounds error further down.
  while (i < FSpecialDirs.Count)
    and (UpperCase(FSpecialDirs.Strings[i][1]) < UpperCase(aDirectory[1])) do
      Inc(i);

  sp := Pos(DirectorySeparator, aDirectory) + 1;
  n := sp;
  while n < Length(aDirectory) do
  begin
    if aDirectory[n] = DirectorySeparator then
    begin
      Inc(i);
      FSpecialDirs.Insert(i, Copy(aDirectory, 1, n-1));
    end;
    Inc(n);
  end;

  if (n > sp) then
  begin
    Inc(i);
    FSpecialDirs.Insert(i, ExcludeTrailingPathDelimiter(aDirectory))
  end;

  FCurrentSpecialDir := i;
end;

constructor TfpgFileListBase.Create;
begin
  FEntries := TList.Create;
  FFileMask := '*';
  FDirectoryName := '';
  FSpecialDirs := TStringList.Create;
end;

destructor TfpgFileListBase.Destroy;
begin
  Clear;
  FSpecialDirs.Free;
  FEntries.Free;
  inherited Destroy;
end;

function TfpgFileListBase.Count: integer;
begin
  Result := FEntries.Count;
end;

function TfpgFileListBase.CurrentSpecialDir: integer;
begin
  Result := FCurrentSpecialDir;
end;

function TfpgFileListBase.ReadDirectory(const aDirectory: TfpgString = ''): boolean;
var
  SearchRec: TSearchRec;
  dir: TfpgString; //  to prevent FDirectoryName from having incorrect value
begin
  Result:=False;
  // default parameter value is current directory
  if aDirectory <> '' then
    dir := fpgExpandFileName(aDirectory)
  else
    dir := fpgGetCurrentDir;

  // vvzh: now we have to use SetCurrentDir in order to make ExpandFileName work
  if not fpgSetCurrentDir(dir) then
    Exit; //==>

  // Add PathDelim to end if it doesn't yet exist
  FDirectoryName := IncludeTrailingPathDelimiter(dir);
  PopulateSpecialDirs(FDirectoryName);

  Clear;
  try
    // The extra 'or' includes Normal attribute files under Windows. faAnyFile doesn't return those.
    // Reported to FPC as bug 9440 in Mantis.
    if fpgFindFirst(FDirectoryName + AllFilesMask, faAnyFile or $00000080, SearchRec) = 0 then
    begin
      AddEntry(SearchRec);
      while fpgFindNext(SearchRec) = 0 do
      begin
        AddEntry(SearchRec);
      end;
    end;
    Result:=True;
  finally
    SysUtils.FindClose(SearchRec);
  end;
end;

procedure TfpgFileListBase.Clear;
var
  n: integer;
begin
  for n := 0 to FEntries.Count-1 do
    TFileEntry(FEntries[n]).Free;
  FEntries.Clear;
end;

procedure TfpgFileListBase.Sort(AOrder: TFileListSortOrder);
var
  newl: TList;
  n: integer;
  i: integer;
  e: TFileEntry;

  function IsBefore(newitem, item: TFileEntry): boolean;
  begin
    //if newitem.etype = etDir then writeln('dir: ',newitem.name,' (',item.name,')');
    if (newitem.EntryType = etDir) and (item.EntryType <> etDir) then
    begin
      result := true;
    end
    else if (newitem.EntryType <> etDir) and (item.EntryType = etDir) then
    begin
      result := false;
    end
    else if (newitem.EntryType = etDir) and (newitem.Name = '..') then
    begin
      result := true;
    end
    else if (item.EntryType = etDir) and (item.Name = '..') then
    begin
      result := false;
    end
    else
      case AOrder of
        soFileName   : result := UpperCase(newitem.Name) < UpperCase(item.Name);
        soCSFileName : result := newitem.Name < item.Name;
        soFileExt    : result := UpperCase(newitem.Extension+' '+newitem.Name) < UpperCase(item.Extension+' '+item.Name);
        soSize       : result := newitem.size < item.size;
        soTime       : result := newitem.modtime < item.modtime;
      else
        result := False;
      end;
  end;

begin
  newl := TList.Create;
  for n := 0 to FEntries.Count-1 do
  begin
    e := TFileEntry(FEntries[n]);
    i := 0;
    while (i < newl.Count) and not IsBefore(e,TFileEntry(newl[i])) do inc(i);
    newl.Insert(i,e);
  end;
  FEntries.Free;
  FEntries := newl;
end;

{ TfpgComponent }

procedure TfpgComponent.SetHelpContext(const AValue: THelpContext);
begin
  if not (csLoading in ComponentState) then
    FHelpType := htContext;
  if FHelpContext = AValue then
    Exit; //==>
  FHelpContext := AValue;
end;

procedure TfpgComponent.SetHelpKeyword(const AValue: TfpgString);
begin
  if not (csLoading in ComponentState) then
    FHelpType := htKeyword;
  if FHelpKeyword = AValue then
    Exit; //==>
  FHelpKeyword := AValue;
end;

constructor TfpgComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHelpType     := htKeyword;
  FHelpContext  := 0;
  FHelpKeyword  := '';
  FTagPointer   := nil;
end;

{ TfpgMimeDataItem }

constructor TfpgMimeDataItem.Create(const AFormat: TfpgString; const AData: variant);
begin
  inherited Create;
  format := AFormat;
  data := AData;
end;


{ TfpgMimeDataBase }

function TfpgMimeDataBase.Geturls: TList;
begin
  { TODO: We should only return data related to MIME type:  text/uri-list }
  Result := nil;
end;

function TfpgMimeDataBase.GetItem(AIndex: Integer): TfpgMimeDataItem;
begin
  Result := TfpgMimeDataItem(FDataList[AIndex]);
end;

procedure TfpgMimeDataBase.Seturls(const AValue: TList);
begin
  if AValue = nil then
    raise Exception.Create('Source URI list must not be nil');

  if Assigned(FUrlList) then
    FUrlList.Free;

  { We take ownership of AValue. Can we do this? }
  FUrlList := AValue;
//  FFormats.Clear;
//  Formats.Add('text/uri-list');
end;

function TfpgMimeDataBase.GetText: TfpgString;
var
  i: integer;
  s: string;
begin
  { TODO: if no text/plain, but we have HTML, we must strip all tags and return that }
  for i := 0 to Count-1 do
  begin
    if Items[i].format = 'text/plain' then
    begin
      s := Items[i].data;
      Result := s;
      break;
    end;
  end;
end;

procedure TfpgMimeDataBase.SetText(const AValue: TfpgString);
var
  i: integer;
  r: TfpgMimeDataItem;
begin
  { remove existing 'text/plain' first }
  for i := Count-1 downto 0 do
  begin
    r := Items[i];
    if r.format = 'text/plain' then
    begin
      FDataList.Remove(r);
      break;
    end;
  end;
  { now add new structure }
  r := TfpgMimeDataItem.Create('text/plain', AValue);
  FDataList.Add(r);
end;

function TfpgMimeDataBase.GetHTML: TfpgString;
var
  i: integer;
  s: string;
begin
  { TODO: if data was HTML, we must strip all tags - regex will make this easy }
  for i := 0 to Count-1 do
  begin
    if Items[i].format = 'text/html' then
    begin
      s := Items[i].data;
      Result := s;
      break;
    end;
  end;
end;

procedure TfpgMimeDataBase.SetHTML(const AValue: TfpgString);
var
  i: integer;
  r: TfpgMimeDataItem;
begin
  { remove existing 'text/html' first }
  for i := Count-1 downto 0 do
  begin
    r := Items[i];
    if r.format = 'text/html' then
    begin
      FDataList.Remove(r);
      break;
    end;
  end;
  { now add new structure }
  r := TfpgMimeDataItem.Create('text/html', AValue);
  FDataList.Add(r);
end;

function TfpgMimeDataBase.GetCount: integer;
begin
  Result := FDataList.Count;
end;

constructor TfpgMimeDataBase.Create;
begin
  inherited Create;
  FDataList := TObjectList.Create;
end;

destructor TfpgMimeDataBase.Destroy;
begin
  FDataList.Free;
  inherited Destroy;
end;

procedure TfpgMimeDataBase.Clear;
begin
  FUrlList.Clear;
  FDataList.Clear;
end;

function TfpgMimeDataBase.HasFormat(const AMimeType: TfpgString): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Count-1 do
  begin
    Result := Items[i].format = AMimeType;
    if Result then
      break;
  end;
end;

function TfpgMimeDataBase.Formats: TStrings;
var
  i: integer;
  r: TfpgMimeDataItem;
  s: string;
begin
  if Count = 0 then
    Result := nil
  else
  begin
    Result := TStringList.Create;
    for i := 0 to Count-1 do
    begin
      s := Items[i].format;
      Result.Add(s);
    end;
  end;
end;

function TfpgMimeDataBase.GetData(const AMimeType: TfpgString): Variant;
var
  i: integer;
begin
  for i := 0 to Count-1 do
  begin
    if Items[i].format = AMimeType then
    begin
      Result := Items[i].data;
      break;
    end;
  end;
end;

procedure TfpgMimeDataBase.SetData(const AMimeType: TfpgString; const AData: Variant);
var
  i: integer;
  r: TfpgMimeDataItem;
begin
  { remove existing mime type first }
  for i := Count-1 downto 0 do
  begin
    r := Items[i];
    if r.format = AMimeType then
    begin
      FDataList.Remove(r);
      break;
    end;
  end;
  { now add new structure }
  r := TfpgMimeDataItem.Create(AMimeType, AData);
  FDataList.Add(r);
end;


{ TfpgDragBase }

constructor TfpgDragBase.Create;
begin
  inherited Create;
  FDragging := False;
end;

destructor TfpgDragBase.Destroy;
begin
  FMimeData.Free;
  inherited Destroy;
end;


{ TfpgBaseTimer }

procedure TfpgBaseTimer.SetInterval(const AValue: integer);
begin
  FInterval := AValue;
  FNextAlarm := Now + (FInterval * ONE_MILISEC);
end;

procedure TfpgBaseTimer.SetEnabled(const AValue: boolean);
begin
  if AValue and (FInterval <= 0) then
     Exit;
  if (not FEnabled) and AValue then
    FNextAlarm := now + (interval * ONE_MILISEC);
  FEnabled := AValue;
end;

constructor TfpgBaseTimer.Create(AInterval: integer);
begin
  inherited Create;
  FInterval := AInterval;
  FEnabled  := False;
  OnTimer   := nil;
end;

destructor TfpgBaseTimer.Destroy;
begin
  Enabled := False;
  inherited Destroy;
end;

procedure TfpgBaseTimer.CheckAlarm(ACurrentTime: TDateTime);
begin
  if not FEnabled then
    Exit; //==>

  if FNextAlarm <= ACurrentTime then
  begin
    // set the next alarm point
    if Interval > 0 then
      while FNextAlarm <= ACurrentTime do
        FNextAlarm += (Interval * ONE_MILISEC);

    if Assigned(FOnTimer) then
      FOnTimer(self);
  end;
end;

procedure TfpgBaseTimer.Reset;
begin
  Enabled := False;
  Enabled := True;
end;

procedure TfpgBaseTimer.Pause(ASeconds: integer);
begin
  if Enabled then
  begin
    FNextAlarm := IncSecond(Now, ASeconds);
  end;
end;



end.

