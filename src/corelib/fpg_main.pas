{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2025 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      The main unit that ties everything together from CoreLib.
}

unit fpg_main;

{$I fpg_defines.inc}

{ TODO : Implement font size adjustments for each platform. eg: linux=10pt & windows=8pt }

interface

uses
  Classes,
  SysUtils,
  fpg_constants,
  fpg_base,
  fpg_interface,
  fpg_impl,
  fpg_fontmanager;

type
  TOrientation = (orVertical, orHorizontal);

  TAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient);
  TLayout = (tlTop, tlCenter, tlBottom);
  TBoxLayout = (tbLeftBox, tbRightBox);

  TAnchor  = (anLeft, anRight, anTop, anBottom);
  TAnchors = set of TAnchor;

  TfpgButtonFlags = set of (btfIsEmbedded, btfIsDefault, btfIsPressed,
    btfIsSelected, btfHasFocus, btfHasParentColor, btfFlat, btfHover, btfDisabled);

  TfpgMenuItemFlags = set of (mifSelected, mifHasFocus, mifSeparator,
    mifEnabled, mifChecked, mifSubMenu, mifHeader);

  TfpgTextFlags = set of (txtLeft, txtHCenter, txtRight, txtTop, txtVCenter,
    txtBottom, txtWrap, txtDisabled, txtAutoSize);

  TMouseButton = (mbLeft, mbRight, mbMiddle);

  TArrowDirection = (adUp, adDown, adLeft, adRight);

const
  AllAnchors = [anLeft, anRight, anTop, anBottom];
  TextFlagsDflt = [txtLeft, txtTop];


type
  { *******************************************
      Internal event properties: Event Types
    *******************************************}
  TIntKeyPressEvent = procedure(Sender: TObject; var keycode: word; var shiftstate: word;
                            var consumed: boolean) of object;
  TIntMouseEvent = procedure(Sender: TObject; x, y: TfpgCoord; var button: word;
                          var shiftstate: word) of object;


  { *******************************************
      Public event properties: Event Types
    *******************************************}
  { Keyboard }
  TfpgKeyCharEvent = procedure(Sender: TObject; AChar: TfpgChar; var Consumed: boolean) of object;
  TKeyPressEvent = procedure(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean) of object;
  { Mouse }
  TMouseButtonEvent = procedure(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint) of object;
  TMouseButtonMultiClickEvent = procedure(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint; const AClickCount: Integer) of object;
  TMouseMoveEvent = procedure(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint) of object;
  TMouseWheelEvent = procedure(Sender: TObject; AShift: TShiftState; AWheelDelta: Single; const AMousePos: TPoint) of object;
  { Painting }
  TPaintEvent = procedure(Sender: TObject) of object;
  { Exceptions }
  TExceptionEvent = procedure(Sender: TObject; E: Exception) of object;


type
  TSizeParams = record
    min_width: TfpgCoord;
    max_width: TfpgCoord;
    min_height: TfpgCoord;
    max_height: TfpgCoord;
  end;


  TfpgStyleDrawTab = record
    TabSheet: TObject;
    TabPosition: TfpgTabPosition;
    TabRect: TfpgRect;
    IsSelected: boolean;
  end;


  TfpgFontResource = class(TfpgFontResourceImpl)
  public
    constructor Create(const afontdesc: string);
  end;


  // forward declaration
  TfpgCanvas = class;
  TfpgTimer = class;
  TfpgDrag = class;


  TfpgNativeWindow = class(TfpgWindowImpl)
  public
    constructor Create(AOwner: TComponent); override;
    property    WinHandle;  // surface this property from TfpgXXXImpl class in it's native format
  end;


  TfpgImage = class(TfpgImageImpl)
  private
    function    GetScanLine(Row: Integer): Pointer;
  public
    function    CreateDisabledImage: TfpgImage;
    function    ImageFromSource: TfpgImage;
    function    ImageFromRect(var ARect: TRect): TfpgImage; overload;
    function    ImageFromRect(var ARect: TfpgRect): TfpgImage; overload;
    property    ScanLine[Row: Integer]: Pointer read GetScanLine;
  end;


  TfpgImages = class
  private
    FImages: TStringList;
  public
    constructor Create;
    destructor  Destroy; override;
    function    AddImage(const imgid: string; img: TfpgImage): boolean;
    function    DeleteImage(const imgid: string; freeimg: boolean): boolean;
    function    GetImage(const imgid: string): TfpgImage;
    function    AddBMP(const imgid: string; bmpdata: pointer; bmpsize: integer): TfpgImage;
    function    AddMaskedBMP(const imgid: string; bmpdata: pointer; bmpsize: integer; mcx, mcy: integer): TfpgImage;
    procedure   ListImages(var sl: TStringList);
  end;


  TfpgCanvas = class(TfpgCanvasImpl)
  private
    function    AddLineBreaks(const s: TfpgString; aMaxLineWidth: integer): string;
  public
    constructor Create(awidget: TfpgWidgetBase); override;
    destructor  Destroy; override;

    // As soon as TfpgStyle has moved out of CoreLib, these must go!
    procedure   DrawButtonFace(x, y, w, h: TfpgCoord; AFlags: TfpgButtonFlags); overload;
    procedure   DrawButtonFace(r: TfpgRect; AFlags: TfpgButtonFlags); overload;
    procedure   DrawBevel(x, y, w, h: TfpgCoord; ARaised: Boolean = True); overload;
    procedure   DrawBevel(r: TfpgRect; ARaised: Boolean = True); overload;
    procedure   DrawDirectionArrow(x, y, w, h: TfpgCoord; direction: TArrowDirection); overload;
    procedure   DrawDirectionArrow(r: TfpgRect; direction: TArrowDirection); overload;
    procedure   DrawFocusRect(r: TfpgRect);
    function    DrawText(x, y, w, h: TfpgCoord; const AText: TfpgString; AFlags: TfpgTextFlags = TextFlagsDflt; ALineSpace: integer = 2): integer; overload;
    function    DrawText(x, y: TfpgCoord; const AText: TfpgString; AFlags: TfpgTextFlags = TextFlagsDflt; ALineSpace: integer = 2): integer; overload;
    function    DrawText(r: TfpgRect; const AText: TfpgString; AFlags: TfpgTextFlags = TextFlagsDflt; ALineSpace: integer = 2): integer; overload;
  end;


  { This is very basic for now, just to remind us of theming support. Later we
    will rework this to use a Style Manager like the previous fpGUI.
    Also support Bitmap based styles for easier theme implementations. }

  TfpgStyle = class(TObject)
  private
    { Font definition fields }
    FDefaultFontDef: TfpgFontDefinition;
    FFixedFontDef: TfpgFontDefinition;
    FMenuFontDef: TfpgFontDefinition;
    FMenuHeaderFontDef: TfpgFontDefinition;
    FMenuAccelFontDef: TfpgFontDefinition;
    FMenuDisabledFontDef: TfpgFontDefinition;
    FTabFontDef: TfpgFontDefinition;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    { Font definitions }
    property    DefaultFontDef: TfpgFontDefinition read FDefaultFontDef;
    property    FixedFontDef: TfpgFontDefinition read FFixedFontDef;
    property    MenuFontDef: TfpgFontDefinition read FMenuFontDef;
    property    MenuHeaderFontDef: TfpgFontDefinition read FMenuHeaderFontDef;
    property    MenuAccelFontDef: TfpgFontDefinition read FMenuAccelFontDef;
    property    MenuDisabledFontDef: TfpgFontDefinition read FMenuDisabledFontDef;
    property    TabFontDef: TfpgFontDefinition read FTabFontDef;
    { Helper methods to get font resources from font manager }
    function    GetDefaultFont: TfpgFontResourceBase;
    function    GetFixedFont: TfpgFontResourceBase;
    function    GetMenuFont: TfpgFontResourceBase;
    function    GetMenuHeaderFont: TfpgFontResourceBase;
    function    GetMenuAccelFont: TfpgFontResourceBase;
    function    GetMenuDisabledFont: TfpgFontResourceBase;
    function    GetTabFont: TfpgFontResourceBase;
    { General }
    procedure   DrawControlFrame(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord); virtual; overload;
    procedure   DrawControlFrame(ACanvas: TfpgCanvas; r: TfpgRect); overload;
    function    GetControlFrameBorders: TRect; virtual;
    procedure   DrawBevel(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; ARaised: Boolean = True); virtual;
    function    GetBevelWidth: TfpgCoord; virtual;
    procedure   DrawDirectionArrow(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; direction: TArrowDirection); virtual;
    procedure   DrawString(ACanvas: TfpgCanvas; x, y: TfpgCoord; AText: string; AEnabled: boolean = True); virtual;
    procedure   DrawFocusRect(ACanvas: TfpgCanvas; r: TfpgRect); virtual;
    { Buttons }
    procedure   DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; AFlags: TfpgButtonFlags); virtual; overload;
    procedure   DrawButtonFace(ACanvas: TfpgCanvas; r: TfpgRect; AFlags: TfpgButtonFlags); overload;
    function    GetButtonBorders: TRect; virtual;
    function    GetButtonShift: TPoint; virtual;
    function    HasButtonHoverEffect: boolean; virtual;
    { Menus }
    procedure   DrawMenuBar(ACanvas: TfpgCanvas; r: TfpgRect; ABackgroundColor: TfpgColor); virtual;
    procedure   DrawMenuRow(ACanvas: TfpgCanvas; r: TfpgRect; AFlags: TfpgMenuItemFlags); virtual;
    procedure   DrawMenuItem(ACanvas: TfpgCanvas; r: TfpgRect; AFlags: TfpgMenuItemFlags; AText: TfpgString); virtual;
    procedure   DrawMenuItemSeparator(ACanvas: TfpgCanvas; r: TfpgRect); virtual;
    procedure   DrawMenuItemImage(ACanvas: TfpgCanvas; x, y: TfpgCoord; r: TfpgRect; AFlags: TfpgMenuItemFlags); virtual;
    function    GetSeparatorSize: integer; virtual;
    { Editbox }
    procedure   DrawEditBox(ACanvas: TfpgCanvas; const r: TfpgRect; const IsEnabled: Boolean; const IsReadOnly: Boolean; const ABackgroundColor: TfpgColor); virtual;
    procedure   DrawPlaceholderText(ACanvas: TfpgCanvas; const r: TfpgRect; constref AText: TfpgString); virtual;
    { Combobox }
    procedure   DrawStaticComboBox(ACanvas: TfpgCanvas; r: TfpgRect; const IsEnabled: Boolean; const IsFocused: Boolean; const IsReadOnly: Boolean; const ABackgroundColor: TfpgColor; const AInternalBtnRect: TfpgRect; const ABtnPressed: Boolean); virtual;
    procedure   DrawInternalComboBoxButton(ACanvas: TfpgCanvas; r: TfpgRect; const IsEnabled: Boolean; const IsPressed: Boolean); virtual;
    { Checkbox }
    function    GetCheckBoxSize: integer; virtual;
    procedure   DrawCheckbox(ACanvas: TfpgCanvas; x, y: TfpgCoord; ix, iy: TfpgCoord); virtual;
    { PageControl & Tabs }
    function    GetTabBorders: TRect; virtual;
    function    GetDefaultTabHeight: TfpgCoord; virtual;
    procedure   DrawTabBackground(ACanvas: TfpgCanvas; ABGColor: TfpgColor); virtual;
    procedure   DrawPageControlTab(ACanvas: TfpgCanvas; AParams: TfpgStyleDrawTab); virtual;
    { Listbox }
    procedure   DrawListBox(ACanvas: TfpgCanvas; const r: TfpgRect; const IsEnabled: Boolean; const IsReadOnly: Boolean; const ABackgroundColor: TfpgColor); virtual;
    procedure   DrawListBoxItem(ACanvas: TfpgCanvas; r: TfpgRect; const IsFocusedItem: Boolean; const HasFocus: Boolean); virtual;
  end;


  TMsgHookItem = class
    Dest: TObject;
    Listener: TObject;
    MsgCode: integer;
  end;


  TfpgApplication = class(TfpgApplicationImpl)
  private
    FHintPause: Integer;
    FShowHint: boolean;
    FOnException: TExceptionEvent;
    FStopOnException: Boolean;
    FHintWindow: TfpgWidgetBase;
    FHintTimer: TfpgTimer;
    FHintWidget: TfpgWidgetBase;
    FHintPos: TPoint;
    FOnKeyPress: TKeyPressEvent;
    FStartDragDistance: integer;
    procedure   SetHintPause(const AValue: Integer);
    procedure   SetupLocalizationStrings;
    procedure   InternalMsgFreeMe(var msg: TfpgMessageRec); message FPGM_FREEME;
    procedure   InternalMsgHintTimer(var msg: TfpgMessageRec); message FPGM_HINTTIMER;
    procedure   CreateHintWindow;
    procedure   HintTimerFired(Sender: TObject);
    procedure   SetShowHint(const AValue: boolean);
    procedure   SetStartDragDistance(const AValue: integer);
  protected
    FDisplayParams: string;
    FScreenWidth: integer;
    FScreenHeight: integer;
    FFontManager: TfpgFontManager;  // centralized font management
    FMessageHookList: TFPList;
    procedure   InternalInit;
    procedure   RunMessageLoop;
    procedure   WaitWindowMessage(atimeoutms: integer);
  public
    constructor Create(const AParams: string = ''); override;
    destructor  Destroy; override;
    procedure   ActivateHint(APos: TPoint; AHint: TfpgString);
    procedure   RecreateHintWindow;
    procedure   Flush;
    procedure   HandleException(Sender: TObject);
    procedure   HideHint;
    procedure   Initialize;
    procedure   ProcessMessages;
    procedure   Run;
    procedure   SetMessageHook(AWidget: TObject; const AMsgCode: integer; AListener: TObject);
    procedure   ShowException(E: Exception);
    procedure   ShowBacktrace(sender: TObject; E: Exception);
    procedure   UnsetMessageHook(AWidget: TObject; const AMsgCode: integer; AListener: TObject);
    property    HintPause: Integer read FHintPause write SetHintPause;
    property    HintWindow: TfpgWidgetBase read FHintWindow;
    property    ScreenWidth: integer read FScreenWidth;
    property    ScreenHeight: integer read FScreenHeight;
    property    FontManager: TfpgFontManager read FFontManager;
    property    ShowHint: boolean read FShowHint write SetShowHint default True;
    property    StartDragDistance: integer read FStartDragDistance write SetStartDragDistance default 5;
    property    StopOnException: Boolean read FStopOnException write FStopOnException;
    property    OnException: TExceptionEvent read FOnException write FOnException;
    property    OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
  end;


  TfpgTimer = class(TfpgTimerImpl)
  public
    { AInterval is in milliseconds. }
    constructor Create(AInterval: integer); override;
    destructor  Destroy; override;
  end;


  { Caret or text cursor, inverts painting over text and has blinking support. }
  TfpgCaret = class(TObject)
  private
    FEnabled: boolean;
    FVisible: boolean;
    FInterval: integer;
    FCanvas: TfpgCanvas;
    FTop: TfpgCoord;
    FLeft: TfpgCoord;
    FWidth: TfpgCoord;
    FHeight: TfpgCoord;
    FTimer: TfpgTimer;
    procedure   OnTimerTime(Sender: TObject);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   SetCaret(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord);
    procedure   UnSetCaret(ACanvas: TfpgCanvas);
    procedure   InvertCaret;
    procedure   ResetTimeout;
    function    IsVisible(acanvas: TfpgCanvas): Boolean;
    property    Width: integer read FWidth;
    property    Height: integer read FHeight;
  end;


  TfpgClipboard = class(TfpgClipboardImpl)
  end;


  TfpgFileList = class(TfpgFileListImpl)
  end;


  TfpgMimeData = class(TfpgMimeDataImpl)
  end;


  TfpgDragPaintEvent = procedure(ASender: TfpgDrag; ACanvas: TfpgCanvas) of object;


  TfpgDrag = class(TfpgDragImpl)
  private
    FOnPaintPreview: TfpgDragPaintEvent;
    FPreviewSize: TfpgSize;
    FTarget: TfpgWinHandle;
    procedure   SetMimeData(const AValue: TfpgMimeDataBase);
    procedure   MsgMouseMove(var msg: TfpgMessageRec); message FPGM_MOUSEMOVE;
  protected
    FPreviewWin: TfpgWidgetBase; // TfpgDNDWindow
    procedure   DoOnPaintPreview(ACanvas: TfpgCanvas);
  public
    constructor Create(ASource: TfpgWidgetBase);
    destructor  Destroy; override;
    function    Execute(const ADropActions: TfpgDropActions = [daCopy]; const ADefaultAction: TfpgDropAction = daCopy): TfpgDropAction; override;
    property    Source: TfpgWidgetBase read GetSource;
    property    Target: TfpgWinHandle read FTarget write FTarget;
    property    MimeData: TfpgMimeDataBase read FMimeData write SetMimeData;
    property    PreviewSize: TfpgSize read FPreviewSize write FPreviewSize;
    property    OnPaintPreview: TfpgDragPaintEvent read FOnPaintPreview write FOnPaintPreview;
  end;


  TfpgDrop = class(TfpgDropImpl)
  public
    function AcceptMimeType(const ACompatibleFormat: array of TfpgString): Boolean;
  end;


  TfpgDropCommonEvent = procedure(Drop: TfpgDrop) of object;
  TfpgDropDropEvent = procedure(Drop: TfpgDrop; AData: Variant) of object;
  TfpgDropMoveEvent = procedure(Drop: TfpgDrop; X, Y: TfpgCoord) of object;


  TfpgDropHandler = class(TObject)
  protected
    procedure   Enter(ADrop: TfpgDrop); virtual; abstract;
    procedure   Leave(ADrop: TfpgDrop); virtual; abstract;
    procedure   Move(ADrop: TfpgDrop; AX, AY: Integer); virtual; abstract;
    procedure   Drop(ADrop: TfpgDrop; AData: Variant); virtual; abstract;
  end;


  TfpgDropEventHandler = class(TfpgDropHandler)
  private
    FOnDrop:  TfpgDropDropEvent;
    FOnEnter: TfpgDropCommonEvent;
    FOnLeave: TfpgDropCommonEvent;
    FOnMove:  TfpgDropMoveEvent;
  protected
    procedure   Enter(ADrop: TfpgDrop); override;
    procedure   Leave(ADrop: TfpgDrop); override;
    procedure   Move(ADrop: TfpgDrop; AX, AY: Integer); override;
    procedure   Drop(ADrop: TfpgDrop; AData: Variant); override;
  public
    constructor Create(AOnEnter, AOnLeave: TfpgDropCommonEvent; AOnDrop:TfpgDropDropEvent; AOnMove: TfpgDropMoveEvent);
    property    OnEnter: TfpgDropCommonEvent read FOnEnter write FOnEnter;
    property    OnLeave: TfpgDropCommonEvent read FOnLeave write FOnLeave;
    property    OnDrop: TfpgDropDropEvent read FOnDrop write FOnDrop;
    property    OnMove: TfpgDropMoveEvent read FOnMove write FOnMove;
  end;


var
  fpgStyle:  TfpgStyle;   { TODO -ograemeg : move this into fpgApplication }
  fpgCaret:  TfpgCaret;   { TODO -ograemeg : move this into fpgApplication }
  fpgImages: TfpgImages;  { TODO -ograemeg : move this into fpgApplication }

  DefaultCanvasClass: TfpgCanvasBaseClass = nil;

// Application & Clipboard singletons
function  fpgApplication: TfpgApplication;
function  fpgClipboard: TfpgClipboard;

// Message Queue  (easy access function)
procedure fpgWaitWindowMessage;
procedure fpgPostMessage(Sender, Dest: TObject; MsgCode: integer; var aparams: TfpgMessageParams); overload;
procedure fpgPostMessage(Sender, Dest: TObject; MsgCode: integer); overload;
procedure fpgSendMessage(Sender, Dest: TObject; MsgCode: integer; var aparams: TfpgMessageParams); overload;
procedure fpgSendMessage(Sender, Dest: TObject; MsgCode: integer); overload;
function  fpgPeekMessage(Dest: TObject; MsgCode: integer; Msg: PfpgMessageRec = nil): Boolean;
procedure fpgDeliverMessage(var msg: TfpgMessageRec);
procedure fpgDeliverMessages;
function  fpgGetFirstMessage: PfpgMessageRec;
procedure fpgDeleteFirstMessage;

{ if MsgCode is -1 then all messages for the object will be deleted. otherwise
  only messages matching MsgCode will be removed }
procedure fpgDeleteMessagesForTarget(Dest: TObject; MsgCode: integer = -1);

// Color & Font routines
function  fpgColorToRGB(col: TfpgColor): TfpgColor;
function  fpgGetNamedColor(col: TfpgColor): TfpgColor;
procedure fpgSetNamedColor(colorid, rgbvalue: longword);
function  fpgIsNamedColor(col: TfpgColor): boolean;
function  fpgGetNamedFontDesc(afontid: string): string;
procedure fpgSetNamedFont(afontid, afontdesc: string);
function  fpgGetNamedFontList: TStringlist;

// Timers rountines
procedure fpgInitTimers;
function  fpgCheckTimers: Boolean;
procedure fpgResetAllTimers;
function  fpgClosestTimer(ctime: TDateTime; amaxtime: integer): integer;
function  fpgGetTickCount: DWord;
procedure fpgPause(MilliSeconds: Cardinal);

// Rectangle, Point & Size routines
function  CopyRect(out Dest: TfpgRect; const Src: TfpgRect): Boolean; deprecated 'Use TfpgRect.CopyRect() instead.';
function  InflateRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean;
function  InflateRect(var Rect: TfpgRect; dx: Integer; dy: Integer): Boolean; deprecated 'Use TfpgRect.InflateRect() instead.';
function  IntersectRect(out ARect: TfpgRect; const r1, r2: TfpgRect): Boolean; deprecated 'Use TfpgRect.IntersectRect() instead.';
function  IsRectEmpty(const ARect: TfpgRect): Boolean; deprecated 'Use TfpgRect.IsRectEmpty() instead.';
function  OffsetRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean;
function  OffsetRect(var Rect: TfpgRect; dx: Integer; dy: Integer): Boolean; deprecated 'Use TfpgRect.OffsetRect() instead.';
function  PtInRect(const ARect: TfpgRect; const APoint: TPoint): Boolean; deprecated 'Use TfpgRect.PointInRect() instead.';
function  UnionRect(out ARect: TfpgRect; const R1, R2: TfpgRect): Boolean; deprecated 'Use TfpgRect.UnionRect() instead.';
function  CenterPoint(const Rect: TRect): TPoint;
function  CenterPoint(const Rect: TfpgRect): TPoint; deprecated 'Use TfpgRect.CenterPoint() instead.';
function  fpgRect(ALeft, ATop, AWidth, AHeight: integer): TfpgRect; deprecated 'Use TfpgRect.SetRect() instead.';
function  fpgRectToRect(const ARect: TfpgRect): TRect;
function  fpgPoint(const AX, AY: integer): TfpgPoint;
function  fpgSize(const AWidth, AHeight: integer): TfpgSize;

// Debug rountines
procedure PrintRect(const Rect: TRect);
procedure PrintRect(const Rect: TfpgRect);
procedure PrintCoord(const x, y: TfpgCoord);
procedure PrintSize(const ASize: TfpgSize);
procedure PrintCoord(const pt: TPoint);
function  PrintCallTrace(const AClassName, AMethodName: string): IInterface;
procedure PrintCallTraceDbgLn(const AMessage: string);
procedure DumpStack;
procedure DumpStack(var AList: TStrings);

{ These methods are safe to use even on Windows gui applications. }
procedure DebugWrite(const s1: TfpgString);
procedure DebugLn(const s1: TfpgString);
procedure DebugLn(const s1, s2: TfpgString);
procedure DebugLn(const s1, s2, s3: TfpgString);
procedure DebugLn(const s1, s2, s3, s4: TfpgString);
procedure DebugLn(const s1, s2, s3, s4, s5: TfpgString);
procedure DebugLnFmt(const Msg: string; const Args: array of const);
function  DebugMethodEnter(const s1: TfpgString): IInterface;
procedure DebugSeparator;

// operator overloading of some useful structures
operator = (const a, b: TRect): boolean;
operator = (const a, b: TfpgRect): boolean;
operator = (const ASize1, ASize2: TfpgSize) b: Boolean;
operator = (const APoint1, APoint2: TPoint) b: Boolean;
operator + (const APoint1, APoint2: TPoint) p: TPoint;
operator + (const APoint1, APoint2: TfpgPoint) p: TfpgPoint;
operator + (const APoint: TPoint; ASize: TfpgSize) p: TPoint;
operator + (const APoint: TfpgPoint; ASize: TfpgSize) p: TfpgPoint;
operator + (const ASize: TfpgSize; APoint: TPoint) s: TfpgSize;
operator + (const ASize: TfpgSize; APoint: TfpgPoint) s: TfpgSize;
operator + (const ASize1, ASize2: TfpgSize) s: TfpgSize;
operator + (const APoint: TPoint; i: Integer) p: TPoint;
operator + (const APoint: TfpgPoint; i: Integer) p: TfpgPoint;
operator + (const ASize: TfpgSize; i: Integer) s: TfpgSize;
operator - (const APoint1, APoint2: TPoint) p: TPoint;
operator - (const APoint1, APoint2: TfpgPoint) p: TfpgPoint;
operator - (const APoint: TPoint; i: Integer) p: TPoint;
operator - (const APoint: TfpgPoint; i: Integer) p: TfpgPoint;
operator - (const ASize: TfpgSize; const APoint: TPoint) s: TfpgSize;
operator - (const ASize: TfpgSize; const APoint: TfpgPoint) s: TfpgSize;
operator - (const ASize: TfpgSize; i: Integer) s: TfpgSize;
operator = (const AColor1, AColor2: TRGBTriple) b: Boolean;


implementation

uses
  strutils,
  math,
{$ifdef AGGCanvas}
  Agg2D,
{$endif}
{$IFDEF GDEBUG}
  fpg_dbugintf,
{$ENDIF}
  fpg_imgfmt_bmp,
  fpg_stdimages,
  fpg_translations,
  fpg_widget,
  fpg_dialogs,
  fpg_hint,
  fpg_extgraphics,
  fpg_utils,
  fpg_cmdlineparams,
  fpg_imgutils,
  fpg_dnd_window,
  fpg_stylemanager,
  fpg_style_win2k,   // TODO: This needs to be removed!
  fpg_style_motif,   // TODO: This needs to be removed!
  fpg_style_carbon,
  fpg_style_plastic,
  fpg_tab;

var
  fpgTimers: TList;
  fpgNamedColors: array[0..255] of TfpgColor;
  fpgNamedFonts: TList;
  uApplication: TfpgApplication;
  uClipboard: TfpgClipboard;
  uMsgQueueList: TList;
  uDebugText: ^Text;
  uDebugTextAllocated: Boolean;
  uDebugIndent: integer;

type

  TDebugMethodHelper = class(TInterfacedObject)
  private
    FMethod: string;
  public
    constructor Create(const AMethodName: string);
    destructor Destroy; override;
  end;


  TNamedFontItem = class(TObject)
  public
    FontID: string;
    FontDesc: string;
    constructor Create(AFontID, AFontDesc: string);
  end;


  TWidgetFriend = class(TfpgWidget);

{ TfpgDrop }

function TfpgDrop.AcceptMimeType(const ACompatibleFormat: array of TfpgString
  ): Boolean;
var
  MimeType: TfpgMimeDataItem;
begin
  Result := False;
  for MimeType in Mimetypes do
  begin
    if MimeType.format in ACompatibleFormat then
    begin
      Result := True;
      MimeChoice := MimeType.format;
    end;
  end;
end;

{ TfpgDropEventHandler }

procedure TfpgDropEventHandler.Enter(ADrop: TfpgDrop);
begin
  if Assigned(FOnEnter) then
    FOnEnter(ADrop);
end;

procedure TfpgDropEventHandler.Leave(ADrop: TfpgDrop);
begin
  if Assigned(FOnLeave) then
    FOnLeave(ADrop);
end;

procedure TfpgDropEventHandler.Move(ADrop: TfpgDrop; AX, AY: Integer);
begin
  if Assigned(FOnMove) then
    FOnMove(ADrop, AX, AY);
end;

procedure TfpgDropEventHandler.Drop(ADrop: TfpgDrop; AData: Variant);
begin
  if Assigned(FOnDrop) then
    FOnDrop(ADrop, AData);
end;

constructor TfpgDropEventHandler.Create(AOnEnter, AOnLeave: TfpgDropCommonEvent; AOnDrop: TfpgDropDropEvent; AOnMove: TfpgDropMoveEvent);
begin
  FOnEnter := AOnEnter;
  FOnLeave := AOnLeave;
  FOnMove  := AOnMove;
  FOnDrop  := AOnDrop;
end;


{ TDebugMethodHelper }

constructor TDebugMethodHelper.Create(const AMethodName: string);
begin
  inherited Create;
  FMethod := AMethodName;
  DebugLn('>> ' + FMethod);
  uDebugIndent := uDebugIndent + 2;
end;

destructor TDebugMethodHelper.Destroy;
begin
  uDebugIndent := uDebugIndent - 2;
  DebugLn('<< ' + FMethod);
  inherited Destroy;
end;

  // so we can get access to the Protected section

constructor TNamedFontItem.Create(AFontID, AFontDesc: string);
begin
  FontID   := AFontID;
  FontDesc := AFontDesc;
end;

{$include fpg_msgqueue.inc}

// Timer support

procedure fpgInitTimers;
begin
  if fpgTimers = nil then
    fpgTimers := TList.Create;
end;

function fpgCheckTimers: Boolean;
var
  i: integer;
  ctime: TDateTime;
begin
  if fpgTimers = nil then
    Exit;
  ctime := now;
  i := fpgTimers.Count;
  Result := i > 0;
  while i > 0 do
  begin
    dec(i);
    if fpgTimers[i] = nil then
      fpgTimers.Delete(i)
    else
      TfpgTimer(fpgTimers[i]).CheckAlarm(ctime);
  end;
end;

procedure fpgResetAllTimers;
var
  i: integer;
begin
  if fpgTimers = nil then
    Exit;
  for i := 0 to fpgTimers.Count-1 do
    TfpgTimer(fpgTimers[i]).Reset;
end;

function fpgClosestTimer(ctime: TDateTime; amaxtime: integer): integer;
var
  i: integer;
  t: TfpgTimer;
  dt: TDateTime;
  tb: Boolean;
begin
  if fpgTimers = nil then
    Exit;
  // returns -1 if no timers are pending
  dt := ctime + amaxtime * ONE_MILLISEC;
  tb := False;

  for i := 0 to fpgTimers.Count-1 do
  begin
    t := TfpgTimer(fpgTimers[i]);
    if (t <> nil) and t.Enabled and (t.NextAlarm < dt) then
    begin
      dt := t.NextAlarm;
      tb := True;
    end;
  end;

  if tb then
  begin
    Result := trunc(0.5 + (dt - ctime) / ONE_MILLISEC);
    if Result < 0 then
      Result := 0;
  end
  else
    Result := -1;
end;

function fpgGetTickCount: DWord;
begin
  Result := DWord(Trunc(Now * MSecsPerDay));
end;

{ blocking function for the caller, but still processes framework messages }
procedure fpgPause(MilliSeconds: Cardinal);
var
  lStart: TDateTime;
begin
   lStart := Now * MSecsPerDay;
   repeat
     fpgApplication.ProcessMessages;
   until ((Now*MSecsPerDay)-lStart) > MilliSeconds;
end;

function CopyRect(out Dest: TfpgRect; const Src: TfpgRect): Boolean;
begin
  Dest := Src;
  if Dest.IsRectEmpty then
  begin
    FillChar(Dest, SizeOf(Dest), 0);
    Result := false;
  end
  else
    Result := true;
end;

function InflateRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean;
begin
  if Assigned(@Rect) then
  begin
    with Rect do
    begin
      dec(Left, dx);
      dec(Top, dy);
      inc(Right, dx);
      inc(Bottom, dy);
    end;
    Result := True;
  end
  else
    Result := False;
end;

function InflateRect(var Rect: TfpgRect; dx: Integer; dy: Integer): Boolean;
begin
  if Assigned(@Rect) then
  begin
    dec(Rect.Left, dx);
    dec(Rect.Top, dy);
    inc(Rect.Width, 2*dx);
    inc(Rect.Height, 2*dy);
    Result := True;
  end
  else
    Result := False;
end;

function IntersectRect(out ARect: TfpgRect; const r1, r2: TfpgRect): Boolean;
var
  TmpRect: TfpgRect; // use tmp to avoid changing r1 if ARect and r1 are the same var
begin
  TmpRect := r1;
  TmpRect.Left:=Max(R1.Left, R2.Left);
  TmpRect.Top:=Max(R1.Top, R2.Top);
  TmpRect.SetBottom(Min(R1.Bottom, R2.Bottom));
  TmpRect.SetRight(Min(R1.Right, R2.Right));

  if TmpRect.IsRectEmpty then
  begin
    FillChar(ARect, SizeOf(ARect), 0);
    Result := false;
  end
  else
  begin
    ARect := TmpRect;
    Result := true;
  end;
end;

function IsRectEmpty(const ARect: TfpgRect): Boolean;
begin
  Result := (ARect.Width <= 0) or (ARect.Height <= 0);
end;

function OffsetRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean;
begin
  if Assigned(@Rect) then
  begin
    with Rect do
    begin
      inc(Left, dx);
      inc(Top, dy);
      inc(Right, dx);
      inc(Bottom, dy);
    end;
    Result := True;
  end
  else
    Result := False;
end;

function OffsetRect(var Rect: TfpgRect; dx: Integer; dy: Integer): Boolean;
begin
  if Assigned(@Rect) then
  begin
    with Rect do
    begin
      inc(Left, dx);
      inc(Top, dy);
    end;
    Result := True;
  end
  else
    Result := False;
end;

function PtInRect(const ARect: TfpgRect; const APoint: TPoint): Boolean;
begin
  Result := (APoint.x >= ARect.Left) and
            (APoint.y >= ARect.Top) and
            (APoint.x <= ARect.Right) and
            (APoint.y <= ARect.Bottom);
end;

function UnionRect(out ARect: TfpgRect; const R1, R2: TfpgRect): Boolean;
var
  TmpRect: TfpgRect;
begin
  TmpRect := R1;

  TmpRect.Left:=Min(R1.Left,R2.Left);
  TmpRect.Top :=Min(R1.Top, R2.Top);
  TmpRect.SetBottom(Max(R1.Bottom, R2.Bottom));
  TmpRect.SetRight (Max(R1.Right, R2.Right));

  if TmpRect.IsRectEmpty then
  begin
    FillChar(ARect, SizeOf(ARect), 0);
    Result := false;
  end
  else
  begin
    Result := true;
    ARect := TmpRect;
  end;
end;

function CenterPoint(const Rect: TRect): TPoint;
begin
  Result.X := (Rect.Left + Rect.Right) div 2;
  Result.Y := (Rect.Top + Rect.Bottom) div 2;
end;

function CenterPoint(const Rect: TfpgRect): TPoint;
begin
  Result.X := (Rect.Left + Rect.Right) div 2;
  Result.Y := (Rect.Top + Rect.Bottom) div 2;
end;

function fpgRect(ALeft, ATop, AWidth, AHeight: integer): TfpgRect;
begin
  Result.SetRect(ALeft, ATop, AWidth, AHeight);
end;

function fpgRectToRect(const ARect: TfpgRect): TRect;
begin
  Result.Left   := ARect.Left;
  Result.Top    := ARect.Top;
  Result.Right  := ARect.Right;
  Result.Bottom := ARect.Bottom;
end;

function fpgPoint(const AX, AY: integer): TfpgPoint;
begin
  Result.SetPoint(AX, AY);
end;

function fpgSize(const AWidth, AHeight: integer): TfpgSize;
begin
  Result.SetSize(AWidth, AHeight);
end;

procedure InitializeDebugOutput;
var
  DebugFileName: string;

  function GetDebugFileName: string;
  var
    EnvVarName: string;
    cmd: ICmdLineParams;
  begin
    Result := '';
    // first try to find the log file name in the command line parameters
    if Supports(fpgApplication, ICmdLineParams, cmd) then
    begin
      if cmd.HasOption('debuglog') then
        Result := cmd.GetOptionValue('debuglog');
    end;

    if Result = '' then
    begin
      // if not found yet, then try to find in the environment variable
      EnvVarName  := ApplicationName + '_debuglog';
      Result      := GetEnvironmentVariable(EnvVarName);
    end;
    if (Result <> '') then
      Result := fpgExpandFileName(Result);
  end;

begin
  uDebugIndent := 0;
  uDebugText := nil;
  DebugFileName := GetDebugFileName;
  if (DebugFileName <> '') and
     (fpgDirectoryExists(fpgExtractFileDir(DebugFileName))) then
  begin
    new(uDebugText);
    try
      Assign(uDebugText^, DebugFileName);
      if fpgFileExists(DebugFileName) then
        Append(uDebugText^)
      else
        Rewrite(uDebugText^);
    except
      Freemem(uDebugText);
      uDebugText := nil;
      // Add extra line ending: a dialog will be shown in Windows gui application
      writeln(StdOut, 'Cannot open file: ', DebugFileName+LineEnding);
    end;
  end;
  if uDebugText = nil then
  begin
    if TextRec(Output).Mode = fmClosed then
      uDebugText := nil
    else
      uDebugText := @Output;
    uDebugTextAllocated := False;
  end else
    uDebugTextAllocated := True;
end;

procedure CloseDebugOutput;
begin
  if uDebugTextAllocated then
  begin
    Close(uDebugText^);
    Dispose(uDebugText);
    uDebugTextAllocated := False;
  end;
  uDebugText := nil;
end;

procedure FinalizeDebugOutput;
begin
  CloseDebugOutput;
end;

procedure PrintRect(const Rect: TRect);
begin
  DebugLn(Format('Rect left=%d top=%d right=%d bottom=%d', [Rect.Left, Rect.Top, Rect.Right, Rect.Bottom]));
end;

procedure PrintRect(const Rect: TfpgRect);
begin
  DebugLn(Rect.ToString);
end;

procedure PrintCoord(const x, y: TfpgCoord);
begin
  DebugLn(Format('x=%d, y=%d', [x, y]));
end;

var
  iCallTrace: integer;

type
  TPrintCallTrace = class(TInterfacedObject)
  private
    FClassName: string;
    FMethodName: string;
    spacing: string;
  public
    constructor Create(const AClassName, AMethodName: string);
    destructor Destroy; override;
  end;

{ TPrintCallTrace }

constructor TPrintCallTrace.Create(const AClassname, AMethodName: string);
var
  i: integer;
begin
  inherited Create;
  spacing := '';
  inc(iCallTrace);
  for i := 0 to iCallTrace do
    spacing += '  ';
  FClassName := AClassName;
  FMethodName := AMethodName;
  {$IFDEF GDEBUG}
  SendDebug(Format('%s>> %s.%s', [spacing, FClassName, FMethodName]));
  {$ENDIF}
end;

destructor TPrintCallTrace.Destroy;
begin
  {$IFDEF GDEBUG}
  SendDebug(Format('%s<< %s.%s', [spacing, FClassName, FMethodName]));
  {$ENDIF}
  dec(iCallTrace);
  inherited Destroy;
end;

procedure PrintSize(const ASize: TfpgSize);
begin
  DebugLn(Format('w=%d  h=%d', [ASize.W, ASize.H]));
end;

procedure PrintCoord(const pt: TPoint);
begin
  PrintCoord(pt.X, pt.Y);
end;

function PrintCallTrace(const AClassName, AMethodName: string): IInterface;
begin
  Result := TPrintCallTrace.Create(AClassName, AMethodName);
end;

procedure PrintCallTraceDbgLn(const AMessage: string);
var
  i: integer;
  s: string;
begin
  s := '';
  for i := 0 to iCallTrace+1 do
    s += '  ';
  writeln(s + AMessage);
end;

procedure DumpStack;
var
  lMessage: String;
  i: longint;
begin
  Writeln(stdout, ' Stack trace:');
  Writeln(stdout, 'An unhandled exception occurred at $', HexStr(Ptrint(ExceptAddr), sizeof(PtrInt)*2),' :');
  if ExceptObject is Exception then
  begin
    lMessage := Exception(ExceptObject).ClassName + ' : ' + Exception(ExceptObject).Message;
    Writeln(stdout, lMessage);
  end
  else
    Writeln(stdout, 'Exception object ', ExceptObject.ClassName, ' is not of class Exception.');
  Writeln(stdout, BackTraceStrFunc(ExceptAddr));
  if (ExceptFrameCount > 0) then
  begin
    for i := 0 to ExceptFrameCount-1 do
      Writeln(stdout, BackTraceStrFunc(ExceptFrames[i]));
  end;
  Writeln(stdout, '');
end;

procedure DumpStack(var AList: TStrings);
var
  lMessage: String;
  i: longint;
begin
  AList.Add(' Stack trace:');
  AList.Add('An unhandled exception occurred at $' + HexStr(PtrInt(ExceptAddr), sizeof(PtrInt)*2) + ' :');
  if ExceptObject is Exception then
  begin
    lMessage := Exception(ExceptObject).ClassName + ' : ' + Exception(ExceptObject).Message;
    AList.Add(lMessage);
  end
  else
    AList.Add('Exception object ' + ExceptObject.ClassName + ' is not of class Exception.');
  AList.Add(BackTraceStrFunc(ExceptAddr));
  if (ExceptFrameCount > 0) then
  begin
    for i := 0 to ExceptFrameCount-1 do
      AList.Add(BackTraceStrFunc(ExceptFrames[i]));
  end;
  AList.Add('');
end;

procedure DebugWrite(const s1: TfpgString);
var
  s: string;
begin
  if not Assigned(uDebugText) then
    Exit; //==>
  s := DupeString(' ', uDebugIndent);
  write(uDebugText^, s + fpgConvertLineEndings(s1));
end;

procedure DebugLn(const s1: TfpgString);
var
  s: string;
begin
  if not Assigned(uDebugText) then
    Exit; //==>
  s := DupeString(' ', uDebugIndent);
  writeln(uDebugText^, s + fpgConvertLineEndings(s1));
end;

procedure DebugLn(const s1, s2: TfpgString);
begin
  DebugLn(s1 + ' ' + s2);
end;

procedure DebugLn(const s1, s2, s3: TfpgString);
begin
  DebugLn(s1 + ' ' + s2  + ' ' + s3);
end;

procedure DebugLn(const s1, s2, s3, s4: TfpgString);
begin
  DebugLn(s1 + ' ' + s2 + ' ' + s3 + ' ' + s4);
end;

procedure DebugLn(const s1, s2, s3, s4, s5: TfpgString);
begin
  DebugLn(s1 + ' ' + s2 + ' ' + s3 + ' ' + s4 + ' ' + s5);
end;

procedure DebugLnFmt(const Msg: string; const Args: array of const);
begin
  DebugLn(Format(Msg,Args));
end;

function DebugMethodEnter(const s1: TfpgString): IInterface;
begin
  Result := TDebugMethodHelper.Create(s1);
end;

procedure DebugSeparator;
begin
  DebugLn('>--------------------------<');
end;

operator = (const a, b: TRect): boolean;
begin
  Result := (a.Top = b.Top) and
            (a.Left = b.Left) and
            (a.Bottom = b.Bottom) and
            (a.Right = b.Right);
end;

operator = (const a, b: TfpgRect): boolean;
begin
  Result := (a.Left = b.Left) and
            (a.Top = b.Top) and
            (a.Width = b.Width) and
            (a.Height = b.Height);
end;

operator = (const ASize1, ASize2: TfpgSize) b: Boolean;
begin
  b := (ASize1.w = ASize2.w) and (ASize1.h = ASize2.h);
end;

operator = (const APoint1, APoint2: TPoint) b: Boolean;
begin
  b := (APoint1.X = APoint2.X) and (APoint1.Y = APoint2.Y);
end;

operator + (const APoint1, APoint2: TPoint) p: TPoint;
begin
  p.x := APoint1.x + APoint2.x;
  p.y := APoint1.y + APoint2.y;
end;

operator + (const APoint1, APoint2: TfpgPoint) p: TfpgPoint;
begin
  p.x := APoint1.x + APoint2.x;
  p.y := APoint1.y + APoint2.y;
end;

operator + (const APoint: TPoint; ASize: TfpgSize) p: TPoint;
begin
  p.x := APoint.x + ASize.w;
  p.y := APoint.y + ASize.h;
end;

operator + (const APoint: TfpgPoint; ASize: TfpgSize) p: TfpgPoint;
begin
  p.x := APoint.x + ASize.w;
  p.y := APoint.y + ASize.h;
end;

operator + (const ASize: TfpgSize; APoint: TPoint) s: TfpgSize;
begin
  s.w := ASize.w + APoint.x;
  s.h := ASize.h + APoint.y;
end;

operator + (const ASize: TfpgSize; APoint: TfpgPoint) s: TfpgSize;
begin
  s.w := ASize.w + APoint.x;
  s.h := ASize.h + APoint.y;
end;

operator + (const ASize1, ASize2: TfpgSize) s: TfpgSize;
begin
  s.w := ASize1.w + ASize2.w;
  s.h := ASize1.h + ASize2.h;
end;

operator + (const APoint: TPoint; i: Integer) p: TPoint;
begin
  p.x := APoint.x + i;
  p.y := APoint.y + i;
end;

operator + (const APoint: TfpgPoint; i: Integer) p: TfpgPoint;
begin
  p.x := APoint.x + i;
  p.y := APoint.y + i;
end;

operator + (const ASize: TfpgSize; i: Integer) s: TfpgSize;
begin
  s.w := ASize.w + i;
  s.h := ASize.h + i;
end;

operator - (const APoint1, APoint2: TPoint) p: TPoint;
begin
  p.x := APoint1.x - APoint2.x;
  p.y := APoint1.y - APoint2.y;
end;

operator - (const APoint1, APoint2: TfpgPoint) p: TfpgPoint;
begin
  p.x := APoint1.x - APoint2.x;
  p.y := APoint1.y - APoint2.y;
end;

operator - (const APoint: TPoint; i: Integer) p: TPoint;
begin
  p.x := APoint.x - i;
  p.y := APoint.y - i;
end;

operator - (const APoint: TfpgPoint; i: Integer) p: TfpgPoint;
begin
  p.x := APoint.x - i;
  p.y := APoint.y - i;
end;

operator - (const ASize: TfpgSize; const APoint: TPoint) s: TfpgSize;
begin
  s.w := ASize.w - APoint.x;
  s.h := ASize.h - APoint.y;
end;

operator - (const ASize: TfpgSize; const APoint: TfpgPoint) s: TfpgSize;
begin
  s.w := ASize.w - APoint.x;
  s.h := ASize.h - APoint.y;
end;

operator - (const ASize: TfpgSize; i: Integer) s: TfpgSize;
begin
  s.w := ASize.w - i;
  s.h := ASize.h - i;
end;

operator = (const AColor1, AColor2: TRGBTriple) b: Boolean;
begin
  b := (AColor1.Red = AColor2.Red)
        and (AColor1.Green = AColor2.Green)
        and (AColor1.Blue = AColor2.Blue)
        and (AColor1.Alpha = AColor2.Alpha);
end;

{ TfpgTimer }

constructor TfpgTimer.Create(AInterval: integer);
begin
  inherited Create(AInterval);
  fpgTimers.Add(self);
end;

destructor TfpgTimer.Destroy;
var
  i: integer;
begin
  i := fpgTimers.IndexOf(self);
  if i > -1 then
    fpgTimers[i] := nil; // we free the item in fpgCheckTimers
  inherited Destroy;
end;


function fpgApplication: TfpgApplication;
begin
  if not Assigned(uApplication) then
    uApplication := TfpgApplication.Create;
  result := uApplication;
end;


function fpgClipboard: TfpgClipboard;
begin
  if not Assigned(uClipboard) then
    uClipboard := TfpgClipboard.Create;
  Result := uClipboard;
end;


function fpgColorToRGB(col: TfpgColor): TfpgColor;
begin
  if (((col shr 24) and $FF) = $80) and ((col and $FFFFFF) <= $FF) then
    Result := fpgNamedColors[col and $FF]
  else
    Result := col;
end;


function fpgGetNamedColor(col: TfpgColor): TfpgColor;
begin
  if fpgIsNamedColor(col) then
    Result := col  // nothing to do, it is already a named color
  else
    Result := fpgNamedColors[col and $FF];
end;

procedure fpgSetNamedColor(colorid, rgbvalue: longword);
var
  i: longword;
begin
  if (colorid and cl_BaseNamedColor) = 0 then
    Exit;
  i := colorid and $FF;
  fpgNamedColors[i] := rgbvalue;
end;

function fpgIsNamedColor(col: TfpgColor): boolean;
begin
  if (((col shr 24) and $FF) = $80) and ((col and $FFFFFF) <= $FF) then
    Result := True
  else
    Result := False;
end;

function fpgGetNamedFontDesc(afontid: string): string;
var
  n: integer;
begin
  for n := 0 to fpgNamedFonts.Count - 1 do
    if (lowercase(TNamedFontItem(fpgNamedFonts[n]).FontID) = lowercase(afontid)) then
    begin // found
      Result := TNamedFontItem(fpgNamedFonts[n]).FontDesc;
      Exit; //==>
    end;

  {$IFDEF GDEBUG}
  SendDebug('GetNamedFontDesc error: "' + afontid + '" is missing. Default is used.');
  {$ENDIF}
  Result := FPG_DEFAULT_FONT_DESC;
end;

procedure fpgSetNamedFont(afontid, afontdesc: string);
var
  n: integer;
begin
  n := 0;
  while (n < fpgNamedFonts.Count) and (lowercase(TNamedFontItem(fpgNamedFonts[n]).FontID) <> lowercase(afontid)) do
    Inc(n);

  if n < fpgNamedFonts.Count then
    TNamedFontItem(fpgNamedFonts[n]).FontDesc := afontdesc// already defined
  else
    fpgNamedFonts.Add(TNamedFontItem.Create(afontid, afontdesc));
end;

function fpgGetNamedFontList: TStringlist;
var
  n: integer;
  oFont: TNamedFontItem;
begin
  if fpgNamedFonts.Count > 0 then
    Result := TStringList.Create
  else
    Exit; //==>

  for n := 0 to fpgNamedFonts.Count-1 do
  begin
    oFont := TNamedFontItem(fpgNamedFonts[n]);
    Result.Add(Format('#%s=%s', [oFont.FontID, oFont.FontDesc]));
  end;
end;

procedure fpgWaitWindowMessage;
begin
  fpgApplication.WaitWindowMessage(500);
end;

constructor TfpgApplication.Create(const AParams: string);
begin
  fpgInitMsgQueue;

  FFontManager    := TfpgFontManager.Create;
  FDisplayParams  := AParams;
  FScreenWidth    := -1;
  FScreenHeight   := -1;
  FMessageHookList := TFPList.Create;
  FStopOnException := False;
  FHintWindow     := nil;   // popup window with Hint text
  FHintPause      := DEFAULT_HINT_PAUSE;
  FHintWidget     := nil;   // widget the mouse is over and whos hint text we need.
  FShowHint       := True;
  FStartDragDistance := 5; // pixels

  try
    inherited Create(AParams);
    if IsInitialized then
    begin
      FScreenWidth  := GetScreenWidth;
      FScreenHeight := GetScreenHeight;
    end;
  except
    on E: Exception do
      SysUtils.ShowException(ExceptObject, ExceptAddr);
  end;
end;

destructor TfpgApplication.Destroy;
var
  i: integer;
begin
  if Assigned(FHintWindow) then
  begin
    HideHint;
    FreeAndNil(FHintWindow);
  end;
  if Assigned(FHintTimer) then
  begin
    FHintTimer.Enabled := False;
    FHintTimer.OnTimer := nil;
    FHintTimer.Free;
  end;

  DestroyComponents;  // while message queue is still active

  for i := 0 to (fpgNamedFonts.Count - 1) do
    TNamedFontItem(fpgNamedFonts.Items[i]).Free;
  fpgNamedFonts.Free;

  fpgImages.Free;
  fpgStyleManager.FreeStyleInstance;
  fpgStyle := nil;
  fpgCaret.Free;

  for i := fpgTimers.Count-1 downto 0 do
    if fpgTimers[i] <> nil then
      TfpgTimer(fpgTimers[i]).Free;
  fpgTimers.Free;

  // Free font manager (will free all cached fonts)
  FFontManager.Free;

  FreeAndNil(FModalFormStack);

  for i := 0 to FMessageHookList.Count-1 do
    TMsgHookItem(FMessageHookList[i]).Free;
  FreeAndNil(FMessageHookList);

  for i := uMsgQueueList.Count-1 downto 0 do
  begin
    TMessageListElement(uMsgQueueList[i]).Free;
    uMsgQueueList.Delete(i);
  end;
  uMsgQueueList.Free;

  inherited Destroy;
end;

procedure TfpgApplication.ActivateHint(APos: TPoint; AHint: TfpgString);
var
  wnd: TfpgHintWindow;
  w: Integer;
  h: Integer;
begin
  wnd := TfpgHintWindow(FHintWindow);
  if Assigned(wnd) and wnd.Visible then
    Exit; //==>  Nothing to do

  wnd.Text := AHint;
  w := wnd.Font.GetTextWidth(AHint) + (wnd.Border * 2) + (wnd.Margin * 2);
  h := wnd.Font.GetHeight + (wnd.Border * 2) + (wnd.Margin * 2);
  { prevents hint from going off the right screen edge }
  if (APos.X + w) > ScreenWidth then
  begin
    APos.X := ScreenWidth - w;
    // just a few more sanity checks
    if APos.X < 0 then
      APos.X := 0;
    if w > ScreenWidth then
      w := ScreenWidth;
  end;
  wnd.SetPosition(APos.X, APos.Y, w, h);
  wnd.UpdatePosition;
  wnd.Show;
end;

procedure TfpgApplication.RecreateHintWindow;
begin
  if Assigned(FHintWindow) then
  begin
    HideHint;
    FreeAndNil(FHintWindow);
  end;
  CreateHintWindow;
end;

procedure TfpgApplication.Initialize;
begin
  InitializeDebugOutput;
  if IsInitialized then
    InternalInit
  else
    raise Exception.Create('Failed to initialize the Application object!');
end;

procedure TfpgApplication.Run;
begin
  repeat
    try
      RunMessageLoop;
    except
      HandleException(Self);
    end;
  until Terminated;
end;

procedure TfpgApplication.SetupLocalizationStrings;
begin
  // setup internal FPC arrays with localized values
  DefaultFormatSettings.ShortDayNames[1] := rsShortSun;
  DefaultFormatSettings.ShortDayNames[2] := rsShortMon;
  DefaultFormatSettings.ShortDayNames[3] := rsShortTue;
  DefaultFormatSettings.ShortDayNames[4] := rsShortWed;
  DefaultFormatSettings.ShortDayNames[5] := rsShortThu;
  DefaultFormatSettings.ShortDayNames[6] := rsShortFri;
  DefaultFormatSettings.ShortDayNames[7] := rsShortSat;

  DefaultFormatSettings.LongDayNames[1] := rsLongSun;
  DefaultFormatSettings.LongDayNames[2] := rsLongMon;
  DefaultFormatSettings.LongDayNames[3] := rsLongTue;
  DefaultFormatSettings.LongDayNames[4] := rsLongWed;
  DefaultFormatSettings.LongDayNames[5] := rsLongThu;
  DefaultFormatSettings.LongDayNames[6] := rsLongFri;
  DefaultFormatSettings.LongDayNames[7] := rsLongSat;

  DefaultFormatSettings.ShortMonthNames[1] := rsShortJan;
  DefaultFormatSettings.ShortMonthNames[2] := rsShortFeb;
  DefaultFormatSettings.ShortMonthNames[3] := rsShortMar;
  DefaultFormatSettings.ShortMonthNames[4] := rsShortApr;
  DefaultFormatSettings.ShortMonthNames[5] := rsShortMay;
  DefaultFormatSettings.ShortMonthNames[6] := rsShortJun;
  DefaultFormatSettings.ShortMonthNames[7] := rsShortJul;
  DefaultFormatSettings.ShortMonthNames[8] := rsShortAug;
  DefaultFormatSettings.ShortMonthNames[9] := rsShortSep;
  DefaultFormatSettings.ShortMonthNames[10] := rsShortOct;
  DefaultFormatSettings.ShortMonthNames[11] := rsShortNov;
  DefaultFormatSettings.ShortMonthNames[12] := rsShortDec;

  DefaultFormatSettings.LongMonthNames[1] := rsLongJan;
  DefaultFormatSettings.LongMonthNames[2] := rsLongFeb;
  DefaultFormatSettings.LongMonthNames[3] := rsLongMar;
  DefaultFormatSettings.LongMonthNames[4] := rsLongApr;
  DefaultFormatSettings.LongMonthNames[5] := rsLongMay;
  DefaultFormatSettings.LongMonthNames[6] := rsLongJun;
  DefaultFormatSettings.LongMonthNames[7] := rsLongJul;
  DefaultFormatSettings.LongMonthNames[8] := rsLongAug;
  DefaultFormatSettings.LongMonthNames[9] := rsLongSep;
  DefaultFormatSettings.LongMonthNames[10] := rsLongOct;
  DefaultFormatSettings.LongMonthNames[11] := rsLongNov;
  DefaultFormatSettings.LongMonthNames[12] := rsLongDec;

  SetLength(TrueBoolStrs,1);
  SetLength(FalseBoolStrs,1);
  TrueBoolStrs[0]   := rsTrue;
  FalseBoolStrs[0]  := rsFalse;

  // Dialog box button captions
  cMsgDlgBtnText[mbOK]        := rsOK;
  cMsgDlgBtnText[mbCancel]    := rsCancel;
  cMsgDlgBtnText[mbYes]       := rsYes;
  cMsgDlgBtnText[mbNo]        := rsNo;
  cMsgDlgBtnText[mbAbort]     := rsAbort;
  cMsgDlgBtnText[mbRetry]     := rsRetry;
  cMsgDlgBtnText[mbIgnore]    := rsIgnore;
  cMsgDlgBtnText[mbAll]       := rsAll;
  cMsgDlgBtnText[mbNoToAll]   := rsNoToAll;
  cMsgDlgBtnText[mbYesToAll]  := rsYesToAll;
  cMsgDlgBtnText[mbHelp]      := rsHelp;
  cMsgDlgBtnText[mbClose]     := rsClose;
end;

procedure TfpgApplication.SetHintPause(const AValue: Integer);
begin
  FHintPause := AValue;
  FHintTimer.Interval := FHintPause;
end;

procedure TfpgApplication.InternalMsgFreeMe(var msg: TfpgMessageRec);
begin
  if Assigned(msg.Sender) then
  begin
    if csDestroying in TComponent(msg.Sender).ComponentState then
      Exit;
    RemoveComponent(TfpgWindowBase(msg.Sender));
    TfpgWindowBase(msg.Sender).Free;
  end;
end;

procedure TfpgApplication.InternalMsgHintTimer(var msg: TfpgMessageRec);
begin
//  writeln('InternalMsgHintTimer msg');
  if (msg.Params.user.Param1 < 2) then
  begin
    { MouseEnter occured }
    FHintTimer.Enabled := Boolean(msg.Params.user.Param1);
    FHintWidget := TfpgWidget(msg.Sender);
  end
  else
  begin
    { Handle mouse move information }
    FHintPos.X := msg.Params.user.Param2;
    FHintPos.Y := msg.Params.user.Param3;
    FHintWidget := TfpgWidget(msg.Sender);
    if FHintTimer.Enabled then
      FHintTimer.Reset    // keep reseting to prevent hint from showing
    else
      HideHint;
  end;
end;

procedure TfpgApplication.CreateHintWindow;
begin
  if not Assigned(FHintWindow) then
  begin
    FHintWindow := HintWindowClass.Create(nil);
    TfpgHintWindow(FHintWindow).Visible := False;
  end;
end;

procedure TfpgApplication.HintTimerFired(Sender: TObject);
var
  w: TfpgWidget;
  lHint: TfpgString;
begin
  w := nil;
  w := TfpgWidget(FHintWidget);
  lHint := '';
  try
    if Assigned(w) then
    begin
//writeln('fpgApplication.HintTimerFired w = ', w.ClassName, ' - ', w.Name);
      TWidgetFriend(w).DoShowHint(lHint);
      ActivateHint(w.WidgetToScreen(w, FHintPos), lHint);
    end;
  except
    // silence it!
    { TODO : FHintWidget probably went out of scope just as timer fired. Try
      and detect such cases better! }
  end;

  FHintTimer.Enabled := False;
end;

procedure TfpgApplication.SetShowHint(const AValue: boolean);
begin
//writeln('>> SetShowHint to :', AValue);
  FShowHint := AValue;
end;

procedure TfpgApplication.SetStartDragDistance(const AValue: integer);
begin
  if AValue < 0 then
    FStartDragDistance := 0
  else
    FStartDragDistance := AValue;
end;

procedure TfpgApplication.InternalInit;
var
  cmd: ICmdLineParams;
begin
  fpgInitTimers;
  fpgNamedFonts := TList.Create;

  { If the end-user passed in a style, try and create an instance of it }
  if Supports(self, ICmdLineParams, cmd) then
    if cmd.HasOption('style') then
      fpgStyleManager.SetStyle(cmd.GetOptionValue('style'));
  fpgStyle := fpgStyleManager.Style;

  fpgCaret      := TfpgCaret.Create;
  fpgImages     := TfpgImages.Create;

  fpgCreateStandardImages;

  // This will process Application and fpGUI Toolkit translation (*.po) files
  TranslateResourceStrings(ApplicationName, ExtractFilePath(ParamStr(0)), '');
  SetupLocalizationStrings;
  CreateHintWindow;

  FHintTimer := TfpgTimer.Create(HintPause);
  FHintTimer.OnTimer := @HintTimerFired;
end;

procedure TfpgApplication.Flush;
begin
  DoFlush;
end;

procedure TfpgApplication.ProcessMessages;
begin
  Flush;
  while MessagesPending do
  begin
    WaitWindowMessage(250);
    Flush;
  end;
end;

procedure TfpgApplication.SetMessageHook(AWidget: TObject; const AMsgCode: integer; AListener: TObject);
var
  oItem: TMsgHookItem;
begin
  oItem := TMsgHookItem.Create;
  oItem.Dest := AWidget;
  oItem.Listener := AListener;
  oItem.MsgCode := AMsgCode;
  FMessageHookList.Add(oItem);
end;

procedure TfpgApplication.UnsetMessageHook(AWidget: TObject;
  const AMsgCode: integer; AListener: TObject);
var
  oItem: TMsgHookItem;
  i: integer;
begin
  for i := 0 to FMessageHookList.Count-1 do
  begin
    oItem := TMsgHookItem(FMessageHookList.Items[i]);
    if (oItem.Dest = AWidget) and (oItem.Listener = AListener) and (oItem.MsgCode = AMsgCode) then
    begin
      FMessageHookList.Delete(i);
      oItem.Free;
      Exit;
    end;
  end;
end;

procedure TfpgApplication.HandleException(Sender: TObject);
begin
  if not (ExceptObject is Exception) then
    SysUtils.ShowException(ExceptObject, ExceptAddr)
  else
  begin
    if not (ExceptObject is EAbort) then  // EAborts are silent. They show no message.
    begin
      if Assigned(FOnException) then
        FOnException(Sender, Exception(ExceptObject))
      else
      begin
//        SysUtils.ShowException(ExceptObject, ExceptAddr);
        ShowException(Exception(ExceptObject));
//        DumpStack;
      end;
    end;
  end;  { if/else }

  // Note: We should not terminate when we receive EAbort exceptions.
  if (not (ExceptObject is EAbort)) and StopOnException then
    Terminated := True;
end;

procedure TfpgApplication.HideHint;
begin
  {$IFDEF GDEBUG}
  SendDebug('HideHint');
  {$ENDIF}
  FHintTimer.Enabled := False;
  if Assigned(FHintWindow) and TfpgHintWindow(FHintWindow).Visible then
    TfpgHintWindow(FHintWindow).Hide;
end;

procedure TfpgApplication.ShowException(E: Exception);
begin
  TfpgMessageDialog.Critical(rsErrUnexpected, E.Message);
end;

procedure TfpgApplication.ShowBacktrace(sender: TObject; E: Exception);
var
  m: string;
  i: Integer;
  frames: PPointer;
begin
  m:='Backtrace:'#10;
  m+='   * '+BackTraceStrFunc(ExceptAddr)+#10;
  frames:=ExceptFrames;
  for i:=0 to ExceptFrameCount-1 do m+='   * '+BackTraceStrFunc(frames[i])+#10;
  fpgClipboard.text:=m;
  TfpgMessageDialog.Critical('Exception '+E.ClassName+': '+E.message, m);
end;

procedure TfpgApplication.WaitWindowMessage(atimeoutms: integer);
begin
  if IsMultiThread then
    CheckSynchronize;  // execute the to-be synchronized method

  DoWaitWindowMessage(fpgClosestTimer(now, atimeoutms));
  fpgDeliverMessages;
  fpgCheckTimers;
end;

procedure TfpgApplication.RunMessageLoop;
begin
  WaitWindowMessage(2000);
end;

{ TfpgFontResource }

constructor TfpgFontResource.Create(const afontdesc: string);
begin
  inherited Create(afontdesc);
end;

{ TfpgCanvas }

// Warning! This function is not supposed to handle existing line breaks,
// it is only supposed to insert new ones when appropriate. Also, this function
// simply inserts line breaks, it doesn't split text lines etc...
function TfpgCanvas.AddLineBreaks(const s: TfpgString; aMaxLineWidth: integer): string;
var
  i, n, ls: integer;
  sub: string;
  lw, tw: integer;
begin
  Result := '';
  ls := Length(s);
  lw := 0;
  i  := 1;
  while i <= ls do
  begin
    if (s[i] in txtWordDelims) then       // read the delimeter only
    begin
      sub := s[i];
      Inc(i);
    end else                              // read the whole word
    begin
      n := PosSetEx(txtWordDelims, s, i);
      if n > 0 then
      begin
        sub := Copy(s, i, n-i);
        i := n;
      end else
      begin
        sub := Copy(s, i, MaxInt);
        i := ls+1;
      end;
    end;
    tw := Font.GetTextWidth(sub);            // wrap if needed
    if (lw + tw > aMaxLineWidth) and (lw > 0) then
    begin
      lw := tw;
      Result := TrimRight(Result) + sLineBreak;
    end else
      Inc(lw, tw);
    Result += sub;
  end;
end;

constructor TfpgCanvas.Create(awidget: TfpgWidgetBase);
begin
  inherited Create(awidget);

  FBeginDrawCount := 0;
end;

destructor TfpgCanvas.Destroy;
begin
  if fpgCaret.FCanvas = self then
    fpgCaret.UnSetCaret(self);
  inherited Destroy;
end;

procedure TfpgCanvas.DrawButtonFace(x, y, w, h: TfpgCoord; AFlags: TfpgButtonFlags);
begin
  fpgStyle.DrawButtonFace(self, x, y, w, h, AFlags);
end;

procedure TfpgCanvas.DrawButtonFace(r: TfpgRect; AFlags: TfpgButtonFlags);
begin
  DrawButtonFace(r.Left, r.Top, r.Width, r.Height, AFlags);
end;

procedure TfpgCanvas.DrawBevel(x, y, w, h: TfpgCoord; ARaised: Boolean);
begin
  fpgStyle.DrawBevel(self, x, y, w, h, ARaised);
end;

procedure TfpgCanvas.DrawBevel(r: TfpgRect; ARaised: Boolean);
begin
  DrawBevel(r.Left, r.Top, r.Width, r.Height, ARaised);
end;

procedure TfpgCanvas.DrawDirectionArrow(x, y, w, h: TfpgCoord; direction: TArrowDirection);
begin
  fpgStyle.DrawDirectionArrow(self, x, y, w, h, direction);
end;

procedure TfpgCanvas.DrawDirectionArrow(r: TfpgRect; direction: TArrowDirection);
begin
  DrawDirectionArrow(r.Left, r.Top, r.Width, r.Height, direction);
end;

procedure TfpgCanvas.DrawFocusRect(r: TfpgRect);
begin
  fpgStyle.DrawFocusRect(self, r);
end;

function TfpgCanvas.DrawText(x, y, w, h: TfpgCoord; const AText: TfpgString;
    AFlags: TfpgTextFlags; ALineSpace: integer): integer;
var
  wtxt, htxt, i, nw, nx, ny, l: integer;
  buf: TfpgString;
  wraplst: TStringList;
  lEnabled: Boolean;
begin
  lEnabled := not (txtDisabled in AFlags);

  // calculate longest word width to autosize properly
  wtxt := 0;
  if ((txtAutoSize in AFlags) or (w = 0)) then
  begin
    i := 1;
    buf := ExtractSubstr(AText, i, txtWordDelims);
    while buf <> '' do
    begin
      wtxt := Max(wtxt, Font.GetTextWidth(buf));
      buf := ExtractSubstr(AText, i, txtWordDelims);
    end;
  end;
  nw := Max(wtxt, w);

  wraplst := TStringList.Create;
  wraplst.Text := AText;

  if (txtWrap in AFlags) then
  begin
    for i := 0 to wraplst.Count-1 do
      wraplst[i] := AddLineBreaks(wraplst[i], nw);
    // force line breaks
    wraplst.Text := wraplst.Text;
  end;

  htxt := (Font.GetHeight() * wraplst.Count) + (ALineSpace * Pred(wraplst.Count));

  // Now paint the actual text
  for i := 0 to wraplst.Count-1 do
  begin
    l :=  (Font.GetHeight() + ALineSpace) * i;
    wtxt := Font.GetTextWidth(wraplst[i]);

    // horizontal alignment
    if (txtRight in AFlags) then
      nx := x + w - wtxt
    else
    if (txtHCenter in AFlags) then
      nx := x + (w - wtxt) div 2
    else // txtLeft is default
      nx := x;

    // vertical alignment
    if (txtBottom in AFlags) then
      ny := y + l + h - htxt
    else if (txtVCenter in AFlags) then
      ny := y + l + ((h - htxt) div 2)
    else // txtTop is default
      ny := y + l;

    fpgStyle.DrawString(self, nx, ny, wraplst[i], lEnabled);
  end;

  wraplst.Free;
  Result := htxt;
end;

function TfpgCanvas.DrawText(x, y: TfpgCoord; const AText: TfpgString;
    AFlags: TfpgTextFlags; ALineSpace: integer): integer;
begin
  Result := DrawText(x, y, 0, 0, AText, AFlags, ALineSpace);
end;

function TfpgCanvas.DrawText(r: TfpgRect; const AText: TfpgString; AFlags: TfpgTextFlags;
    ALineSpace: integer): integer;
begin
  Result := DrawText(r.Left, r.Top, r.Width, r.Height, AText, AFlags, ALineSpace);
end;

{ TfpgNativeWindow }

constructor TfpgNativeWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); // initialize the platform internals

  FModalForWin := nil;

  if not (FWindowType in [wtModalForm, wtPopup]) then
  begin
    if (AOwner <> nil) and (AOwner is TfpgNativeWindow) then
      FWindowType   := wtChild
    else
      FWindowType   := wtWindow;
  end;
end;

{ TfpgStyle }

{ Lazy getters for backward compatibility - create font objects on demand }

function TfpgStyle.GetDefaultFont: TfpgFontResourceBase;
begin
  Result := fpgApplication.FontManager.GetFont(FDefaultFontDef.FontDesc);
end;

function TfpgStyle.GetFixedFont: TfpgFontResourceBase;
begin
  Result := fpgApplication.FontManager.GetFont(FFixedFontDef.FontDesc);
end;

function TfpgStyle.GetMenuFont: TfpgFontResourceBase;
begin
  Result := fpgApplication.FontManager.GetFont(FMenuFontDef.FontDesc);
end;

function TfpgStyle.GetMenuHeaderFont: TfpgFontResourceBase;
begin
  Result := fpgApplication.FontManager.GetFont(FMenuHeaderFontDef.FontDesc);
end;

function TfpgStyle.GetMenuAccelFont: TfpgFontResourceBase;
begin
  Result := fpgApplication.FontManager.GetFont(FMenuAccelFontDef.FontDesc);
end;

function TfpgStyle.GetMenuDisabledFont: TfpgFontResourceBase;
begin
  Result := fpgApplication.FontManager.GetFont(FMenuDisabledFontDef.FontDesc);
end;

function TfpgStyle.GetTabFont: TfpgFontResourceBase;
begin
  Result := fpgApplication.FontManager.GetFont(FTabFontDef.FontDesc);
end;

constructor TfpgStyle.Create;
begin
  // Create font definitions with default descriptors
  FDefaultFontDef := TfpgFontDefinition.Create(FPG_DEFAULT_FONT_DESC);
  FFixedFontDef := TfpgFontDefinition.Create(FPG_DEFAULT_FIXED_FONT_DESC);
  FMenuFontDef := TfpgFontDefinition.Create(FPG_DEFAULT_FONT_DESC);
  FMenuHeaderFontDef := TfpgFontDefinition.Create(FPG_DEFAULT_FONT_DESC + ':bold');
  FMenuAccelFontDef := TfpgFontDefinition.Create(FPG_DEFAULT_FONT_DESC + ':underline');
  FMenuDisabledFontDef := TfpgFontDefinition.Create(FPG_DEFAULT_FONT_DESC);
  FTabFontDef := TfpgFontDefinition.Create(FPG_DEFAULT_FONT_DESC);

  // Setup font aliases
  fpgSetNamedFont('Label1', FPG_DEFAULT_FONT_DESC);
  fpgSetNamedFont('Label2', FPG_DEFAULT_FONT_DESC + ':bold');
  fpgSetNamedFont('Edit1', FPG_DEFAULT_FONT_DESC);
  fpgSetNamedFont('Edit2', FPG_DEFAULT_FIXED_FONT_DESC);
  fpgSetNamedFont('List', FPG_DEFAULT_FONT_DESC);
  fpgSetNamedFont('Grid', FPG_DEFAULT_SANS + '-9');
  fpgSetNamedFont('GridHeader', FPG_DEFAULT_SANS + '-9:bold');
  fpgSetNamedFont('Menu', FPG_DEFAULT_FONT_DESC);
  fpgSetNamedFont('MenuHeader', FPG_DEFAULT_FONT_DESC+':bold');
  fpgSetNamedFont('MenuAccel', FPG_DEFAULT_FONT_DESC + ':underline');
  fpgSetNamedFont('MenuDisabled', FPG_DEFAULT_FONT_DESC);

  {$Note Refactor this so under Windows it can detect the system colors instead.
    Also under Linux (KDE and Gnome) we should be able to detect the system colors.}
  fpgSetNamedColor(clWindowBackground, $FFD5D2CD);
  fpgSetNamedColor(clBoxColor, $FFFFFFFF);
  fpgSetNamedColor(clShadow1, $FF848284);       // medium
  fpgSetNamedColor(clShadow2, $FF424142);       // dark
  fpgSetNamedColor(clHilite1, $FFE0E0E0);       // light
  fpgSetNamedColor(clHilite2, $FFFFFFFF);       // white
  fpgSetNamedColor(clText1, $FF000000);
  fpgSetNamedColor(clText2, $FF000040);
  fpgSetNamedColor(clText3, $FF800000);
  fpgSetNamedColor(clText4, $FF404000);
  fpgSetNamedColor(clSelection, $FF08246A);
  fpgSetNamedColor(clSelectionText, $FFFFFFFF);
  fpgSetNamedColor(clInactiveSel, $FF99A6BF);  // win 2000 buttonface = $D4D0C8
  fpgSetNamedColor(clInactiveSelText, $FF000000);
  fpgSetNamedColor(clScrollBar, $FFE8E4DB);
  fpgSetNamedColor(clButtonFace, $FFD5D2CD);
  fpgSetNamedColor(clListBox, $FFFFFFFF);
  fpgSetNamedColor(clGridLines, $FFA0A0A0);
  fpgSetNamedColor(clGridHeader, $FFD5D2CD);
  fpgSetNamedColor(clWidgetFrame, $FF000000);
  fpgSetNamedColor(clInactiveWgFrame, $FFA0A0A0);
  fpgSetNamedColor(clTextCursor, $FF000000);
  fpgSetNamedColor(clChoiceListBox, $FFE8E8E8);
  fpgSetNamedColor(clUnset, $FF99A6BF);                   // dull (gray) blue
  fpgSetNamedColor(clMenuText, $FF000000);
  fpgSetNamedColor(clMenuDisabled, $FF909090);
  fpgSetNamedColor(clHintWindow, $FFFFFFBF);
  fpgSetNamedColor(clGridSelection, $FF08246A);           // same as clSelection
  fpgSetNamedColor(clGridSelectionText, $FFFFFFFF);       // same as clSelectionText
  fpgSetNamedColor(clGridInactiveSel, $FF99A6BF);         // same as clInactiveSel
  fpgSetNamedColor(clGridInactiveSelText, $FF000000);     // same as clInactiveSelText
  fpgSetNamedColor(clSplitterGrabBar, $FF839EFE);         // pale blue
  fpgSetNamedColor(clHyperLink, clBlue);
  fpgSetNamedColor(clPlaceholderText, $FF848284);         // Same as clShadow1
end;

destructor TfpgStyle.Destroy;
begin
  // Free font definitions
  FDefaultFontDef.Free;
  FFixedFontDef.Free;
  FMenuFontDef.Free;
  FMenuHeaderFontDef.Free;
  FMenuAccelFontDef.Free;
  FMenuDisabledFontDef.Free;
  FTabFontDef.Free;

  inherited Destroy;
end;

procedure TfpgStyle.DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; AFlags: TfpgButtonFlags);
var
  r: TfpgRect;
begin
  r.SetRect(x, y, w, h);

  if btfIsDefault in AFlags then
  begin
    ACanvas.SetColor(clBlack);
    ACanvas.SetLineStyle(1, lsSolid);
    ACanvas.DrawRectangle(r);
    r.InflateRect(-1, -1);
    Exclude(AFlags, btfIsDefault);
    fpgStyle.DrawButtonFace(ACanvas, r.Left, r.Top, r.Width, r.Height, AFlags);
    Exit; //==>
  end;

  { Clear the rectangle with a color }
  ACanvas.SetColor(clButtonFace);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.FillRectangle(x, y, w, h);

  if (btfFlat in AFlags) and not (btfIsPressed in AFlags) then
    Exit; // no need to go further

  // Left and Top (outer)
  if (btfIsPressed in AFlags) then
  begin
    if (btfIsEmbedded in AFlags) then
      ACanvas.SetColor(clShadow1)
    else
    begin
      if (btfFlat in AFlags) or (btfHover in AFlags) then
        ACanvas.SetColor(clShadow1)  { light shadow }
      else
        ACanvas.SetColor(clShadow1); { light shadow }
    end;
  end
  else
    ACanvas.SetColor(clHilite2); { white }

  ACanvas.DrawLine(r.Left, r.Bottom, r.Left, r.Top);  // left
  ACanvas.DrawLine(r.Left, r.Top, r.Right, r.Top);    // top

  // Right and Bottom (outer)
  if (btfIsPressed in AFlags) then
  begin
    if (btfIsEmbedded in AFlags) then
      ACanvas.SetColor(clShadow1)
    else
    begin
      if (btfFlat in AFlags) or (btfHover in AFlags) then
        ACanvas.SetColor(clHilite2)  { white }
      else
        ACanvas.SetColor(clHilite2); { white }
    end;
  end
  else
  begin
    if btfHover in AFlags then
      ACanvas.SetColor(clShadow1)  { light shadow }
    else
      ACanvas.SetColor(clShadow2); { dark shadow }
  end;

  ACanvas.DrawLine(r.Right, r.Top, r.Right, r.Bottom);   // right
  ACanvas.DrawLine(r.Right, r.Bottom, r.Left-1, r.Bottom);   // bottom

  if (btfFlat in AFlags) or (btfHover in AFlags) then
    exit; { "toolbar" style buttons need a nice thin/flat border }

  // Left and Top (inner)
  if btfIsPressed in AFlags then
  begin
    if not (btfIsEmbedded in AFlags) then
    begin
      ACanvas.SetColor(clShadow2);  { dark shadow }
      ACanvas.DrawLine(r.Left+1, r.Bottom-1, r.Left+1, r.Top+1);  // left
      ACanvas.DrawLine(r.Left+1, r.Top+1, r.Right-1, r.Top+1);    // top
    end;
  end;

  // Right and Bottom (inner)
  if btfIsPressed in AFlags then
  begin
    if (btfIsEmbedded in AFlags) then
      ACanvas.SetColor(clButtonFace)
    else
      ACanvas.SetColor(clButtonFace);
  end
  else
    ACanvas.SetColor(clShadow1);

  ACanvas.DrawLine(r.Right-1, r.Top+1, r.Right-1, r.Bottom-1);   // right
  ACanvas.DrawLine(r.Right-1, r.Bottom-1, r.Left, r.Bottom-1);   // bottom
end;

procedure TfpgStyle.DrawButtonFace(ACanvas: TfpgCanvas; r: TfpgRect; AFlags: TfpgButtonFlags);
begin
  DrawButtonFace(ACanvas, r.Left, r.Top, r.Width, r.Height, AFlags);
end;

procedure TfpgStyle.DrawControlFrame(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord);
var
  r: TfpgRect;
begin
  r.SetRect(x, y, w, h);
  ACanvas.SetColor(clShadow1);
  ACanvas.DrawLine(r.Left, r.Bottom, r.Left, r.Top); // left (outer)
  ACanvas.DrawLine(r.Left, r.Top, r.Right, r.Top);   // top (outer)

  ACanvas.SetColor(clHilite2);
  ACanvas.DrawLine(r.Right, r.Top, r.Right, r.Bottom);   // right (outer)
  ACanvas.DrawLine(r.Right, r.Bottom, r.Left, r.Bottom); // bottom (outer)

  ACanvas.SetColor(clShadow2);
  ACanvas.DrawLine(r.Left+1, r.Bottom-1, r.Left+1, r.Top+1);   // left (inner)
  ACanvas.DrawLine(r.Left+1, r.Top+1, r.Right-1, r.Top+1);   // top (inner)

  ACanvas.SetColor(clHilite1);
  ACanvas.DrawLine(r.Right-1, r.Top+1, r.Right-1, r.Bottom-1);   // right (inner)
  ACanvas.DrawLine(r.Right-1, r.Bottom-1, r.Left+1, r.Bottom-1);   // bottom (inner)
end;

procedure TfpgStyle.DrawControlFrame(ACanvas: TfpgCanvas; r: TfpgRect);
begin
  DrawControlFrame(ACanvas, r.Left, r.Top, r.Width, r.Height);
end;

procedure TfpgStyle.DrawBevel(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; ARaised: Boolean);
var
  r: TfpgRect;
begin
  r.SetRect(x, y, w, h);
  ACanvas.SetColor(clWindowBackground);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.FillRectangle(x, y, w, h);

  if ARaised then
    ACanvas.SetColor(clHilite2)
  else
    ACanvas.SetColor(clShadow1);

  { top }
  ACanvas.DrawLine(r.Right-1, r.Top, r.Left, r.Top);

  { left }
  ACanvas.DrawLine(r.Left, r.Top, r.Left, r.Bottom);

  if ARaised then
    ACanvas.SetColor(clShadow1)
  else
    ACanvas.SetColor(clHilite2);

  { right, then bottom }
  ACanvas.DrawLine(r.Right, r.Top, r.Right, r.Bottom);
  ACanvas.DrawLine(r.Right, r.Bottom, r.Left-1, r.Bottom);
end;

function TfpgStyle.GetBevelWidth: TfpgCoord;
begin
  Result := 1;
end;

procedure TfpgStyle.DrawDirectionArrow(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; direction: TArrowDirection);
var
{
  peekx: integer;
  peeky: integer;
  basex: integer;
  basey: integer;
  side: integer;
  margin: integer;
}
  rad: Extended;
  r: TRect;
  r2: TfpgRect;
begin
{
  side   := (w div 4) + 1;
  margin := side + 1;

  if direction in [adUp, adDown] then  // vertical
  begin
    peekx := x + (w div 2);
    if direction = adDown then  // down
    begin
      peeky := y + h - margin;
      basey := peeky - side;
    end
    else
    begin                  // up
      peeky := y + margin;
      basey := peeky + side;
    end;
    ACanvas.FillTriangle(peekx, peeky, peekx + side, basey, peekx - side, basey);
  end
  else // horizontal
  begin
    peeky := y + (h div 2);
    if direction = adRight then  // right
    begin
      peekx := x + w - margin;
      basex := peekx - side;
    end
    else                   // left
    begin
      peekx := x + margin;
      basex := peekx + side;
    end;
    ACanvas.FillTriangle(peekx, peeky, basex, peeky - side, basex, peeky + side);
  end;
}

  r2.SetRect(x, y, w, h);
  r := fpgRectToRect(r2);

  if direction = adRight then
    rad := DegToRad(0)
  else if direction = adUp then
    rad := DegToRad(90)
  else if direction = adLeft then
    rad := DegToRad(180)
  else
    rad := DegToRad(270);

  PaintTriangle(ACanvas, r, rad);
end;

procedure TfpgStyle.DrawString(ACanvas: TfpgCanvas; x, y: TfpgCoord;
  AText: string; AEnabled: boolean);
begin
  if AText = '' then
    Exit; //==>
  if not AEnabled then
  begin
    ACanvas.SetTextColor(clHilite2);
    ACanvas.DrawString(x+1, y+1, AText);
    ACanvas.SetTextColor(clShadow1);
  end;
  ACanvas.DrawString(x, y, AText);
end;

procedure TfpgStyle.DrawFocusRect(ACanvas: TfpgCanvas; r: TfpgRect);
var
  oldColor: TfpgColor;
  oldLineWidth: integer;
  oldLineStyle: TfpgLineStyle;
begin
  oldColor      := ACanvas.Color;
  oldLineWidth  := ACanvas.GetLineWidth;
  oldLineStyle  := ACanvas.LineStyle;

  ACanvas.SetColor(clText1);
  ACanvas.SetLineStyle(1, lsDot);
  ACanvas.DrawRectangle(r);

  // restore previous settings
  ACanvas.SetColor(oldColor);
  ACanvas.SetLineStyle(oldLineWidth, oldLineStyle);
end;

procedure TfpgStyle.DrawMenuBar(ACanvas: TfpgCanvas; r: TfpgRect; ABackgroundColor: TfpgColor);
begin
  ACanvas.Clear(ABackgroundColor);

  // inner bottom line
  ACanvas.SetColor(clShadow1);
  ACanvas.DrawLine(r.Left, r.Bottom-1, r.Right+1, r.Bottom-1);   // bottom
  // outer bottom line
  ACanvas.SetColor(clWhite);
  ACanvas.DrawLine(r.Left, r.Bottom, r.Right+1, r.Bottom);   // bottom
end;

procedure TfpgStyle.DrawMenuRow(ACanvas: TfpgCanvas; r: TfpgRect; AFlags: TfpgMenuItemFlags);
begin
  ACanvas.FillRectangle(r);
end;

procedure TfpgStyle.DrawMenuItem(ACanvas: TfpgCanvas; r: TfpgRect;
  AFlags: TfpgMenuItemFlags; AText: TfpgString);
begin
  //
end;

procedure TfpgStyle.DrawMenuItemSeparator(ACanvas: TfpgCanvas; r: TfpgRect);
begin
  ACanvas.SetColor(clShadow1);
  ACanvas.DrawLine(r.Left+1, r.Top+2, r.Right, r.Top+2);
  ACanvas.SetColor(clHilite2);
  ACanvas.DrawLine(r.Left+1, r.Top+3, r.Right, r.Top+3);
end;

procedure TfpgStyle.DrawMenuItemImage(ACanvas: TfpgCanvas; x, y: TfpgCoord; r: TfpgRect; AFlags: TfpgMenuItemFlags);
var
  img: TfpgImage;
  lx: TfpgCoord;
  ly: TfpgCoord;
begin
  if mifChecked in AFlags then
  begin
    img := fpgImages.GetImage('stdimg.check');    // Do NOT localize
    if mifSelected in AFlags then
      img.Invert;  // invert modifies the original image, so we must restore it later
    ACanvas.DrawImage(x, y, img);
    if mifSelected in AFlags then
      img.Invert;  // restore image to original state
  end;
  if mifSubMenu in AFlags then
  begin
    img := fpgImages.GetImage('sys.sb.right');    // Do NOT localize
    lx := (r.height div 2) - 3;
    lx := r.right-lx-2;
    ly := y + ((r.Height-img.Height) div 2);
    if mifSelected in AFlags then
      img.Invert;  // invert modifies the original image, so we must restore it later
    ACanvas.DrawImage(lx, ly, img);
    if mifSelected in AFlags then
      img.Invert;  // restore image to original state
  end;
end;

function TfpgStyle.GetButtonBorders: TRect;
begin
  Result := Rect(3, 3, 3, 3);
end;

function TfpgStyle.GetButtonShift: TPoint;
begin
  Result := Point(1, 1);
end;

function TfpgStyle.HasButtonHoverEffect: boolean;
begin
  Result := False;
end;

function TfpgStyle.GetControlFrameBorders: TRect;
begin
  Result := Rect(2, 2, 2, 2);
end;

function TfpgStyle.GetSeparatorSize: integer;
begin
  Result := 2;
end;

procedure TfpgStyle.DrawEditBox(ACanvas: TfpgCanvas; const r: TfpgRect; const IsEnabled: Boolean; const IsReadOnly: Boolean; const ABackgroundColor: TfpgColor);
begin
  if IsEnabled and not IsReadOnly then
    ACanvas.SetColor(ABackgroundColor)
  else
    ACanvas.SetColor(clWindowBackground);
  ACanvas.FillRectangle(r);
end;

procedure TfpgStyle.DrawPlaceholderText(ACanvas: TfpgCanvas; const r: TfpgRect; constref AText: TfpgString);
begin
  ACanvas.SetTextColor(clPlaceholderText);
  ACanvas.DrawText(r, AText, [txtLeft, txtVCenter]);
end;

procedure TfpgStyle.DrawStaticComboBox(ACanvas: TfpgCanvas; r: TfpgRect;
    const IsEnabled: Boolean; const IsFocused: Boolean; const IsReadOnly: Boolean;
    const ABackgroundColor: TfpgColor; const AInternalBtnRect: TfpgRect;
    const ABtnPressed: Boolean);
var
  lr: TfpgRect;
begin
  lr := r;
  if IsEnabled then
  begin
    if IsReadOnly then
      ACanvas.SetColor(clWindowBackground)
    else
      ACanvas.SetColor(ABackgroundColor);
  end
  else
    ACanvas.SetColor(clWindowBackground);

  ACanvas.FillRectangle(r);

  if IsFocused then
  begin
    ACanvas.SetColor(clSelection);
    lr.InflateRect(-1, -1);
    ACanvas.FillRectangle(lr);
  end;

  // paint the fake dropdown button
  DrawInternalComboBoxButton(ACanvas, AInternalBtnRect, IsEnabled, ABtnPressed);
end;

procedure TfpgStyle.DrawInternalComboBoxButton(ACanvas: TfpgCanvas;
    r: TfpgRect; const IsEnabled: Boolean; const IsPressed: Boolean);
var
  ar: TfpgRect;
  btnflags: TfpgButtonFlags;
begin
  btnflags := [];
  ar := r;

  { The bounding rectangle for the arrow }
  ar.Width := 8;
  ar.Height := 6;
  ar.Left := r.Left + ((r.Width-ar.Width) div 2);
  ar.Top := r.Top + ((r.Height-ar.Height) div 2);

  if IsPressed then
  begin
    Include(btnflags, btfIsPressed);
    ar.OffsetRect(1, 1);
  end;
  // paint button face
  DrawButtonFace(ACanvas, r.Left, r.Top, r.Width, r.Height, btnflags);
  if IsEnabled then
    ACanvas.SetColor(clText1)
  else
    ACanvas.SetColor(clShadow1);

  // paint arrow
  DrawDirectionArrow(ACanvas, ar.Left, ar.Top, ar.Width, ar.Height, adDown);
end;

function TfpgStyle.GetCheckBoxSize: integer;
begin
  Result := 13; // 13x13 - it is always a rectangle
end;

procedure TfpgStyle.DrawCheckbox(ACanvas: TfpgCanvas; x, y: TfpgCoord; ix, iy: TfpgCoord);
var
  img: TfpgImage;
  size: integer;
begin
  img := fpgImages.GetImage('sys.checkboxes');    // Do NOT localize - return value is a reference only
  size := GetCheckBoxSize;
  ACanvas.DrawImagePart(x, y, img, ix, iy, size, size);
end;

function TfpgStyle.GetTabBorders: TRect;
begin
  Result := Rect(2, 2, 2, 2);
end;

function TfpgStyle.GetDefaultTabHeight: TfpgCoord;
begin
  Result := 21;
end;

procedure TfpgStyle.DrawTabBackground(ACanvas: TfpgCanvas; ABGColor: TfpgColor);
begin
  ACanvas.Clear(ABGColor);
end;

procedure TfpgStyle.DrawPageControlTab(ACanvas: TfpgCanvas; AParams: TfpgStyleDrawTab);
var
  r: TfpgRect;

  procedure ApplyCorrectTabColorToCanvas;
  begin
    if TfpgTabSheet(AParams.TabSheet).PageControl.ActiveTabColor = clDefault then
      ACanvas.SetColor(TfpgTabSheet(AParams.TabSheet).TabColor)
    else
      ACanvas.SetColor(TfpgTabSheet(AParams.TabSheet).PageControl.ActiveTabColor);
  end;

begin
  r := AParams.TabRect;

  if AParams.IsSelected then
    ApplyCorrectTabColorToCanvas
  else
    ACanvas.SetColor(TfpgTabSheet(AParams.TabSheet).TabColor);

  case AParams.TabPosition of
    tpTop:
      begin
        with ACanvas do
        begin
          FillRectangle(r.Left+1, r.Top+1, r.Width-3, r.Height-2);     // fill tab background
          SetColor(clHilite2);
          DrawLine(r.Left, r.Bottom-2 , r.Left, r.Top+2);        // left edge
          DrawLine(r.Left, r.Top+2 , r.Left+2, r.Top);           // left rounder edge
          DrawLine(r.Left+2,  r.Top, r.Right-1, r.Top);          // top edge
          SetColor(clShadow1);
          DrawLine(r.Right-1, r.Top+1, r.Right-1, r.Bottom-1);   // right inner edge
          SetColor(clShadow2);
          DrawLine(r.Right-1, r.Top+1, r.Right, r.Top+2);        // right rounded edge (1px)
          DrawLine(r.Right, r.Top+2, r.Right, r.Bottom-1);       // right outer edge
        end;
      end;

    tpBottom:
      begin
        with ACanvas do
        begin
          FillRectangle(r.Left, r.Top, r.Width-1, r.Height-2);   // fill tab background
          SetColor(clHilite2);
          DrawLine(r.Left, r.Top, r.Left, r.Bottom-1);           // left edge
          SetColor(clShadow2);
          DrawLine(r.Left+2,  r.Bottom, r.Right-1, r.Bottom);    // bottom outer edge
          SetColor(clShadow1);
          DrawLine(r.Right-1, r.Bottom-1, r.Right-1, r.Top-1);   // right inner edge
          DrawLine(r.Left+1,  r.Bottom-1, r.Right-1, r.Bottom-1);// bottom inner edge
          SetColor(clShadow2);
          DrawLine(r.Right-1, r.Bottom-1, r.Right, r.Bottom-2);  // right rounded edge (1px)
          DrawLine(r.Right, r.Bottom-2, r.Right, r.Top-1);       // right outer edge
          if AParams.IsSelected then
          begin
            ApplyCorrectTabColorToCanvas;
            DrawLine(r.Left+1, r.Top-1, r.Right-1, r.Top-1);
          end;
        end;
      end;

    tpLeft:
      begin
        if AParams.IsSelected then
        begin
          r.Width  := r.Width - 1;
          r.Height := r.Height + 2;
        end;

        with ACanvas do
        begin
          FillRectangle(r.Left+1, r.Top+1, r.Width-2, r.Height-3);
          SetColor(clHilite2);
          DrawLine(r.Left, r.Bottom-2, r.Left, r.Top+2);
          DrawLine(r.Left, r.Top+2, r.Left+2, r.Top);
          DrawLine(r.Left+2, r.Top, r.Right-1, r.Top);
          SetColor(clShadow1);
          DrawLine(r.Left+2, r.Bottom-1, r.Right-1, r.Bottom-1);
          SetColor(clShadow2);
          DrawLine(r.Left+1, r.Bottom-1, r.Left+3, r.Bottom);
          DrawLine(r.Left+2, r.Bottom, r.Right, r.Bottom);
        end;
      end;

    tpRight:
      begin
        if AParams.IsSelected then
          r.Height := r.Height + 2;

        with ACanvas do
        begin
          FillRectangle(r.Left+1, r.Top+1, r.Width-2, r.Height-3);
          SetColor(clHilite2);
          DrawLine(r.Left+1, r.Top, r.Right-2, r.Top);
          SetColor(clShadow1);
          DrawLine(r.Right-2,r.Top,r.Right-1,r.Top+1);
          DrawLine(r.Left+2, r.Bottom-1, r.Right-2, r.Bottom-1);
          DrawLine(r.Right-3, r.Bottom-1, r.Right-1, r.Bottom-3);
          DrawLine(r.Right-1, r.Bottom-3, r.Right-1, r.Top);
          SetColor(clShadow2);
          DrawLine(r.Left+2,r.Bottom,r.Right-3, r.Bottom);
          DrawLine(r.Right-3, r.Bottom, r.Right, r.Bottom-3);
          DrawLine(r.Right, r.Top+2, r.Right, r.Bottom-2);
        end;
      end;
  end;  { case }
end;

procedure TfpgStyle.DrawListBox(ACanvas: TfpgCanvas; const r: TfpgRect; const IsEnabled: Boolean;
  const IsReadOnly: Boolean; const ABackgroundColor: TfpgColor);
begin
  if IsEnabled and not IsReadOnly then
    ACanvas.SetColor(ABackgroundColor)
  else
    ACanvas.SetColor(clWindowBackground);
  ACanvas.FillRectangle(r);
end;

procedure TfpgStyle.DrawListBoxItem(ACanvas: TfpgCanvas; r: TfpgRect; const IsFocusedItem: Boolean;
  const HasFocus: Boolean);
begin
  if IsFocusedItem then
  begin
    if HasFocus then
    begin
      ACanvas.SetColor(clSelection);
      ACanvas.SetTextColor(clSelectionText);
    end
    else
    begin
      ACanvas.SetColor(clInactiveSel);
      ACanvas.SetTextColor(clInactiveSelText);
    end;
    ACanvas.FillRectangle(r);
  end;
end;


{ TfpgCaret }

procedure TfpgCaret.OnTimerTime(Sender: TObject);
begin
  if FEnabled then
    InvertCaret;
end;

constructor TfpgCaret.Create;
begin
  FEnabled       := False;
  FInterval      := 500;  // blinking interval
  FCanvas        := nil;
  FTop           := 0;
  FLeft          := 0;
  FWidth         := 2;
  FHeight        := 8;
  FTimer         := TfpgTimer.Create(FInterval);
  FTimer.OnTimer := @OnTimerTime;
end;

destructor TfpgCaret.Destroy;
begin
  FCanvas := nil;
  FTimer.Free;
  inherited Destroy;
end;

procedure TfpgCaret.SetCaret(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord);
begin
  FEnabled := True;
  FVisible := False;
  FCanvas  := ACanvas;
  FLeft    := x;
  FTop     := y;
  FWidth   := w;
  FHeight  := h;
  InvertCaret;

  FTimer.Enabled  := False;
  FTimer.Interval := FInterval;
  FTimer.Enabled  := True;
end;

procedure TfpgCaret.UnSetCaret(ACanvas: TfpgCanvas);
begin
  if (FCanvas = ACanvas) or (ACanvas = nil) then
  begin
    FTimer.Enabled := False;
    FEnabled := False;
    FCanvas  := nil;
  end;
end;

procedure TfpgCaret.InvertCaret;
begin
  if FCanvas = nil then
    Exit; //==>

  // we could not be sure about the buffer contents!
  try
    FCanvas.BeginDraw;
    try
      // this works well on narrow characters like 'i' or 'l' in non-mono fonts
      FCanvas.XORFillRectangle($FFFFFF, FLeft, FTop, FWidth, FHeight);
      FVisible := not FVisible;
    finally
      FCanvas.EndDraw(FLeft, FTop, FWidth, FHeight);
    end;
  except
    {$Note This occurs every now and again with TfpgMemo and CaretInvert painting! }
    // Investigate this.
    {$IFDEF GDEBUG}
    SendDebug('TfpgCaret.InvertCaret cause an exception');
    {$ENDIF}
  end;
end;

procedure TfpgCaret.ResetTimeout;
begin
  if FVisible and FTimer.Enabled then
  begin
    FTimer.Reset;
  end;
end;

function TfpgCaret.IsVisible(acanvas: TfpgCanvas): Boolean;
begin
  Result := FVisible and (FCanvas = acanvas);
end;

{ TfpgImages }

constructor TfpgImages.Create;
begin
  FImages := TStringList.Create;
end;

destructor TfpgImages.Destroy;
var
  i: integer;
  img: TfpgImage;
begin
  for i := FImages.Count-1 downto 0 do
  begin
    img := TfpgImage(FImages.Objects[i]);
    FImages.Delete(i);
    img.Free;
  end;
  FImages.Free;
  inherited Destroy;
end;

function TfpgImages.AddImage(const imgid: string; img: TfpgImage): boolean;
var
  i: integer;
begin
  i := FImages.IndexOf(LowerCase(imgid));
  if i >= 0 then
  begin
    FImages.Strings[i] := LowerCase(imgid);
    FImages.Objects[i] := img;
    Result := False;
  end
  else
  begin
    FImages.AddObject(LowerCase(imgid), img);
    Result := True;
  end;
end;

function TfpgImages.DeleteImage(const imgid: string; freeimg: boolean): boolean;
var
  i: integer;
begin
  i := FImages.IndexOf(LowerCase(imgid));
  if i >= 0 then
  begin
    if freeimg then
      TfpgImage(FImages.Objects[i]).Free;
    FImages.Delete(i);
    Result := True;
  end
  else
    Result := False;
end;

function TfpgImages.GetImage(const imgid: string): TfpgImage;
var
  i: integer;
begin
  i := FImages.IndexOf(LowerCase(imgid));
  if i >= 0 then
    Result := TfpgImage(FImages.Objects[i])
  else
    Result := nil;
end;

function TfpgImages.AddBMP(const imgid: string; bmpdata: pointer; bmpsize: integer): TfpgImage;
begin
  Result := CreateImage_BMP(bmpdata, bmpsize);
  if Result <> nil then
    AddImage(imgid, Result);
end;

function TfpgImages.AddMaskedBMP(const imgid: string; bmpdata: pointer; bmpsize: integer;
  mcx, mcy: integer): TfpgImage;
begin
  Result := AddBMP(imgid, bmpdata, bmpsize);
  if Result <> nil then
  begin
    Result.CreateMaskFromSample(mcx, mcy);
    Result.UpdateImage;
  end;
end;

procedure TfpgImages.ListImages(var sl: TStringList);
begin
  if sl <> nil then
    sl.Assign(FImages);
end;


{ TfpgImage }

function TfpgImage.GetScanLine(Row: Integer): Pointer;
var
  pdest: Plongword;
begin
  if (Height = 0) or (Width = 0) then
  begin
    Result := nil;
    Exit;
  end;

  pdest := ImageData; // This is so that pointer math uses correct increment size
  Result := pdest + (Row * Width);
end;

function TfpgImage.CreateDisabledImage: TfpgImage;
begin
  Result := ImageFromSource;
  fpgApplyGreyFilter(Result);
end;

function TfpgImage.ImageFromSource: TfpgImage;
var
  x, y: TfpgCoord;
begin
  Result := TfpgImage.Create;
  Result.AllocateImage(ColorDepth, Width, Height);
  for x := 0 to Width-1 do
  begin
    for y := 0 to Height-1 do
    begin
      Result.Colors[x, y] := Colors[x, y];
    end;
  end;
  if Masked then
    Result.CreateMaskFromSample(MaskPoint.X, MaskPoint.Y);
  Result.UpdateImage;
end;

function TfpgImage.ImageFromRect(var ARect: TRect): TfpgImage;
var
  x, y: TfpgCoord;
  ix, iy: TfpgCoord;
begin
  SortRect(ARect);
  Result := TfpgImage.Create;
  Result.AllocateImage(ColorDepth, ARect.Right-ARect.Left, ARect.Bottom-ARect.Top);
  iy := -1;
  for y := ARect.Top to ARect.Bottom-1 do
  begin
    Inc(iy);
    ix := -1;
    for x := ARect.Left to ARect.Right-1 do
    begin
      Inc(ix);
      Result.Colors[ix, iy] := Colors[x, y];
    end;
  end;
  Result.UpdateImage;
end;

function TfpgImage.ImageFromRect(var ARect: TfpgRect): TfpgImage;
var
  x, y: TfpgCoord;
  ix, iy: TfpgCoord;
begin
  SortRect(ARect);
  Result := TfpgImage.Create;
  Result.AllocateImage(ColorDepth, ARect.Width, ARect.Height);
  iy := -1;
  for y := ARect.Top to ARect.Bottom do
  begin
    Inc(iy);
    ix := -1;
    for x := ARect.Left to ARect.Right do
    begin
      Inc(ix);
      Result.Colors[ix, iy] := Colors[x, y];
    end;
  end;
  Result.UpdateImage;
end;


{ TfpgDrag }

procedure TfpgDrag.SetMimeData(const AValue: TfpgMimeDataBase);
begin
  if Assigned(FMimeData) then
    FMimeData.Free;
  FMimeData := AValue;
end;

procedure TfpgDrag.MsgMouseMove(var msg: TfpgMessageRec);
var
  FOffset: TfpgPoint;
begin
  if TfpgDNDWindow(FPreviewWin).Visible then
  begin
    FOffset := TWidgetFriend(Source).FDragStartPos;

    FPreviewWin.MoveWidget(msg.Params.mouse.x-FOffset.X, msg.Params.mouse.y-FOffset.Y);
  end;
end;


procedure TfpgDrag.DoOnPaintPreview(ACanvas: TfpgCanvas);
begin
  if Assigned(FOnPaintPreview) then
    FOnPaintPreview(Self, ACanvas);
end;


constructor TfpgDrag.Create(ASource: TfpgWidgetBase);
begin
  inherited Create(ASource);
  FSource := ASource;
  FPreviewWin := TfpgDNDWindow.Create(nil, Self);
end;

destructor TfpgDrag.Destroy;
begin
  FPreviewWin.Free;
  inherited Destroy;
end;

function TfpgDrag.Execute(const ADropActions: TfpgDropActions;
  const ADefaultAction: TfpgDropAction): TfpgDropAction;
begin
  {$NOTE These exception messages need to become resource strings }
  if not Assigned(FMimeData) then
    raise Exception.Create(ClassName + ': No mimedata was set before starting the drag');
  if not Assigned(FSource) then
    raise Exception.Create(ClassName + ': No Source window was specified before starting the drag');
  if ADropActions = [] then
    raise Exception.Create(ClassName + ': No Drop Action was specified');
  if Assigned(FOnPaintPreview) or TfpgDNDWindow(FPreviewWin).HasWidgetChildren then
    TfpgDNDWindow(FPreviewWin).Show(FPreviewSize);
  Result := inherited Execute(ADropActions, ADefaultAction);
end;



initialization
  uApplication    := nil;
  uClipboard      := nil;
  uMsgQueueList   := nil;
  fpgTimers       := nil;
  fpgCaret        := nil;
  fpgImages       := nil;
  iCallTrace      := -1;

{$ifdef AGGCanvas}
  DefaultCanvasClass := TAgg2D;
{$else}
  DefaultCanvasClass := TfpgCanvas;
{$endif}
  {$IF FPC_FULLVERSION >= 30000}
  // This switches RTL, FCL and String data type to UTF-8. Many of fpg_utils functions will not be needed any more.
  DefaultSystemCodePage := CP_UTF8;
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
  {$IFEND}

finalization
  uClipboard.Free;
  uApplication.Free;
  FinalizeDebugOutput;

end.
