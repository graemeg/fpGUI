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
      The main unit trying everything together in corelib.
}

unit fpg_main;

{$mode objfpc}{$H+}

{.$Define DEBUG}

{ TODO : Implement font size adjustments for each platform. eg: linux=10pt & windows=8pt }

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_interface;

type
  TOrientation = (orVertical, orHorizontal);

  TAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient);
  TLayout = (tlTop, tlCenter, tlBottom);
  TBoxLayout = (tbLeftBox, tbRightBox);

  TAnchor  = (anLeft, anRight, anTop, anBottom);
  TAnchors = set of TAnchor;

  TFButtonFlags = set of (btfIsEmbedded, btfIsDefault, btfIsPressed,
    btfIsSelected, btfHasFocus, btfHasParentColor, btfFlat, btfHover);
    
  TFTextFlags = set of (txtLeft, txtHCenter, txtRight, txtTop, txtVCenter, txtBottom, txtWrap, txtDisabled,
    txtAutoSize);

  TMouseButton = (mbLeft, mbRight, mbMiddle);

  TArrowDirection = (adUp, adDown, adLeft, adRight);

const
  AllAnchors = [anLeft, anRight, anTop, anBottom];
  TextFlagsDflt = [txtLeft, txtTop];
  
  // Used for the internal message queue
  cMessageQueueSize = 1024;

  // version and name constants
  {$I VERSION_FILE.inc}  // this includes the auto generated:  fpGUI_Version = xxx
  fpGUIName    = 'fpGUI Toolkit';

const
  txtWordDelims: set of char = [' ', #9, #13, #10];


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
  TKeyEvent = procedure(Sender: TObject; AKey: Word; AShift: TShiftState) of object;
  TKeyCharEvent = procedure(Sender: TObject; AKeyChar: Char) of object;
  TKeyPressEvent = procedure(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean) of object;
  { Mouse }
  TMouseButtonEvent = procedure(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint) of object;
  TMouseMoveEvent = procedure(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint) of object;
  TMouseWheelEvent = procedure(Sender: TObject; AShift: TShiftState; AWheelDelta: Single; const AMousePos: TPoint) of object;
  { Painting }
  TPaintEvent = procedure(Sender: TObject{; const ARect: TfpgRect}) of object;
  { Exceptions }
  TExceptionEvent = procedure(Sender: TObject; E: Exception) of object;


type
  TSizeParams = record
    min_width: TfpgCoord;
    max_width: TfpgCoord;
    min_height: TfpgCoord;
    max_height: TfpgCoord;
  end;


  TfpgFontResource = class(TfpgFontResourceImpl)
  protected
    FFontDesc: string;
    FRefCount: integer;
  public
    constructor Create(const afontdesc: string);
    function    IncRefCount: integer;
    function    DecRefCount: integer;
    property    FontDesc: string read FFontDesc;
  end;


  TfpgFont = class(TfpgFontBase)
  public
    constructor Create(afontres: TfpgFontResource; const afontdesc: string);
    destructor  Destroy; override;
  end;


  // forward declaration
  TfpgCanvas = class;
  TfpgTimer = class;


  TfpgWindow = class(TfpgWindowImpl)
  protected
    procedure   SetParent(const AValue: TfpgWindow); reintroduce;
    function    GetParent: TfpgWindow; reintroduce;
    function    GetCanvas: TfpgCanvas; reintroduce;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Parent: TfpgWindow read GetParent write SetParent;
    property    Canvas: TfpgCanvas read GetCanvas;
    property    WinHandle;  // surface this property from TfpgXXXImpl class in it's native format
  end;


  TfpgImage = class(TfpgImageImpl)
  public
    function    ImageFromRect(var ARect: TRect): TfpgImage; overload;
    function    ImageFromRect(var ARect: TfpgRect): TfpgImage; overload;
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
    constructor Create(awin: TfpgWindow); reintroduce;
    destructor  Destroy; override;

    // As soon as TfpgStyle has moved out of CoreLib, these must go!
    procedure   DrawButtonFace(x, y, w, h: TfpgCoord; AFlags: TFButtonFlags);
    procedure   DrawButtonFace(r: TfpgRect; AFlags: TFButtonFlags);
    procedure   DrawControlFrame(x, y, w, h: TfpgCoord);
    procedure   DrawControlFrame(r: TfpgRect);
    procedure   DrawDirectionArrow(x, y, w, h: TfpgCoord; direction: TArrowDirection);
    procedure   DrawDirectionArrow(r: TfpgRect; direction: TArrowDirection);
    procedure   DrawFocusRect(r: TfpgRect);
    function    DrawText(x, y, w, h: TfpgCoord; const AText: TfpgString; AFlags: TFTextFlags = TextFlagsDflt; ALineSpace: integer = 2): integer;
    function    DrawText(x, y: TfpgCoord; const AText: TfpgString; AFlags: TFTextFlags = TextFlagsDflt; ALineSpace: integer = 2): integer;
    function    DrawText(r: TfpgRect; const AText: TfpgString; AFlags: TFTextFlags = TextFlagsDflt; ALineSpace: integer = 2): integer;
  end;


  { This is very basic for now, just to remind us of theming support. Later we
    will rework this to use a Style Manager like the previous fpGUI. Styles must
    also move out of fpGFX. Also support Bitmap based styles for easier theme
    implementations. }
  TfpgStyle = class
  public
    DefaultFont: TfpgFont;
    FixedFont: TfpgFont;
    MenuFont: TfpgFont;
    MenuAccelFont: TfpgFont;
    MenuDisabledFont: TfpgFont;
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; AFlags: TFButtonFlags); virtual;
    procedure   DrawControlFrame(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord); virtual;
    procedure   DrawDirectionArrow(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; direction: TArrowDirection); virtual;
    procedure   DrawString(ACanvas: TfpgCanvas; x, y: TfpgCoord; AText: string; AEnabled: boolean = True); virtual;
    procedure   DrawFocusRect(ACanvas: TfpgCanvas; r: TfpgRect); virtual;
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
    FHintWindow: TfpgWindow;
    FHintTimer: TfpgTimer;
    FHintWidget: TfpgWindow;
    FHintPos: TPoint;
    procedure   SetHintPause(const AValue: Integer);
    procedure   SetupLocalizationStrings;
    procedure   InternalMsgFreeMe(var msg: TfpgMessageRec); message FPGM_FREEME;
    procedure   InternalMsgHintTimer(var msg: TfpgMessageRec); message FPGM_HINTTIMER;
    procedure   CreateHintWindow;
    procedure   HintTimerFired(Sender: TObject);
    procedure   SetShowHint(const AValue: boolean);
  protected
    FDisplayParams: string;
    FScreenWidth: integer;
    FScreenHeight: integer;
    FDefaultFont: TfpgFont;
    FFontResList: TList;
    FMessageHookList: TFPList;
    procedure   FreeFontRes(afontres: TfpgFontResource);
    procedure   InternalInit;
    procedure   RunMessageLoop;
    procedure   WaitWindowMessage(atimeoutms: integer);
  public
    constructor Create(const AParams: string = ''); override;
    destructor  Destroy; override;
    function    GetFont(const afontdesc: string): TfpgFont;
    procedure   ActivateHint(APos: TPoint; AHint: TfpgString);
    procedure   Flush;
    procedure   HandleException(Sender: TObject);
    procedure   HideHint;
    procedure   Initialize;
    procedure   ProcessMessages;
    procedure   Run;
    procedure   SetMessageHook(AWidget: TObject; const AMsgCode: integer; AListener: TObject);
    procedure   ShowException(E: Exception);
    procedure   UnsetMessageHook(AWidget: TObject; const AMsgCode: integer; AListener: TObject);
    property    DefaultFont: TfpgFont read FDefaultFont;
    property    HintPause: Integer read FHintPause write SetHintPause;
    property    HintWindow: TfpgWindow read FHintWindow;
    property    ScreenWidth: integer read FScreenWidth;
    property    ScreenHeight: integer read FScreenHeight;
    property    ShowHint: boolean read FShowHint write SetShowHint default True;
    property    StopOnException: Boolean read FStopOnException write FStopOnException;
    property    OnException: TExceptionEvent read FOnException write FOnException;
  end;


  TfpgTimer = class(TObject)
  private
    FEnabled: boolean;
    FNextAlarm: TDateTime;
    FInterval: integer;
    FOnTimer: TNotifyEvent;
    procedure   SetEnabled(const AValue: boolean);
    procedure   SetInterval(const AValue: integer);
  public
    { AInterval is in milliseconds. }
    constructor Create(ainterval: integer);
    destructor  Destroy; override;
    procedure   CheckAlarm(ctime: TDateTime);
    procedure   Reset;
    property    Enabled: boolean read FEnabled write SetEnabled;
    property    NextAlarm: TDateTime read FNextAlarm;
    { Interval is in milliseconds. }
    property    Interval: integer read FInterval write SetInterval;
    property    OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
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
    procedure   SetCaret(acanvas: TfpgCanvas; x, y, w, h: TfpgCoord);
    procedure   UnSetCaret(acanvas: TfpgCanvas);
    procedure   InvertCaret;
    property    Width: integer read FWidth;
    property    Height: integer read FHeight;
  end;
  
  
  TfpgClipboard = class(TfpgClipboardImpl)
  end;

  
  TfpgFileList = class(TfpgFileListImpl)
  end;


var
  fpgStyle:  TfpgStyle;   { TODO -ograemeg : move this into fpgApplication }
  fpgCaret:  TfpgCaret;   { TODO -ograemeg : move this into fpgApplication }
  fpgImages: TfpgImages;  { TODO -ograemeg : move this into fpgApplication }

// Application & Clipboard singletons
function  fpgApplication: TfpgApplication;
function  fpgClipboard: TfpgClipboard;

// Fonts (easy access function)
function  fpgGetFont(const afontdesc: string): TfpgFont;

// Message Queue  (easy access function)
procedure fpgWaitWindowMessage;
procedure fpgPostMessage(Sender, Dest: TObject; MsgCode: integer; var aparams: TfpgMessageParams); overload;
procedure fpgPostMessage(Sender, Dest: TObject; MsgCode: integer); overload;
procedure fpgSendMessage(Sender, Dest: TObject; MsgCode: integer; var aparams: TfpgMessageParams); overload;
procedure fpgSendMessage(Sender, Dest: TObject; MsgCode: integer); overload;
procedure fpgDeliverMessage(var msg: TfpgMessageRec);
procedure fpgDeliverMessages;
function  fpgGetFirstMessage: PfpgMessageRec;
procedure fpgDeleteFirstMessage;

// Color & Font routines
function  fpgColorToRGB(col: TfpgColor): TfpgColor;
function  fpgGetNamedColor(col: TfpgColor): TfpgColor;
procedure fpgSetNamedColor(colorid, rgbvalue: longword);
function  fpgGetNamedFontDesc(afontid: string): string;
procedure fpgSetNamedFont(afontid, afontdesc: string);
function  fpgGetNamedFontList: TStringlist;

// Timers rountines
procedure fpgInitTimers;
procedure fpgCheckTimers;
function  fpgClosestTimer(ctime: TDateTime; amaxtime: integer): integer;
function  fpgGetTickCount: DWord;

// Rectangle routines
function  InflateRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean;
function  InflateRect(var Rect: TfpgRect; dx: Integer; dy: Integer): Boolean;
function  OffsetRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean;
function  OffsetRect(var Rect: TfpgRect; dx: Integer; dy: Integer): Boolean;
function  CenterPoint(const Rect: TRect): TPoint;
function  CenterPoint(const Rect: TfpgRect): TPoint;
function  fpgRect(ALeft, ATop, AWidth, AHeight: integer): TfpgRect;
function  fpgRectToRect(const ARect: TfpgRect): TRect;

// Debug rountines
procedure PrintRect(const Rect: TRect);
procedure PrintRect(const Rect: TfpgRect);
procedure PrintCoord(const x, y: TfpgCoord);
function  PrintCallTrace(const AClassName, AMethodName: string): IInterface;
procedure PrintCallTraceDbgLn(const AMessage: string);
procedure DumpStack;

{ These methods are safe to use even on Windows gui applications. }
procedure DebugLn(const s1: TfpgString);
procedure DebugLn(const s1, s2: TfpgString);
procedure DebugLn(const s1, s2, s3: TfpgString);
procedure DebugLn(const s1, s2, s3, s4: TfpgString);

// operator overloading of some useful structures
operator = (a: TRect; b: TRect): boolean;

implementation

uses
  strutils,
  math,
  fpg_imgfmt_bmp,
  fpg_stdimages,
  fpg_translations,
  fpg_constants,
  fpg_widget,
  fpg_dialogs,
  fpg_hint,
  fpg_extgraphics,
  fpg_utils,
  fpg_cmdlineparams;

var
  fpgTimers: TList;
  fpgNamedColors: array[0..255] of TfpgColor;
  fpgNamedFonts: TList;
  uApplication: TfpgApplication;
  uClipboard: TfpgClipboard;
  uMsgQueueList: TList;
  uDebugText: ^Text;
  uDebugTextAllocated: Boolean;

type
  TNamedFontItem = class
  public
    FontID: string;
    FontDesc: string;
    constructor Create(AFontID, AFontDesc: string);
  end;

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

procedure fpgCheckTimers;
var
  i: integer;
  ctime: TDateTime;
begin
  ctime := now;
  i := 0;
  while i < fpgTimers.Count do
  begin
    if fpgTimers[i] = nil then
      fpgTimers.Delete(i)
    else
    begin
      TfpgTimer(fpgTimers[i]).CheckAlarm(ctime);
      Inc(i);
    end;
  end;
end;

function fpgClosestTimer(ctime: TDateTime; amaxtime: integer): integer;
var
  i: integer;
  t: TfpgTimer;
  dt: TDateTime;
  tb: Boolean;
begin
  // returns -1 if no timers are pending
  dt := ctime + amaxtime * ONE_MILISEC;
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
    Result := trunc(0.5 + (dt - ctime) / ONE_MILISEC);
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
    OffsetRect := True;
  end
  else
    OffsetRect := False;
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
    OffsetRect := True;
  end
  else
    OffsetRect := False;
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

procedure InitializeDebugOutput;
var
  DebugFileName: string;

  function GetDebugFileName: string;
  var
    EnvVarName: string;
  begin
    Result := '';
    // first try to find the log file name in the command line parameters
    if gCommandLineParams.IsParam('debuglog') then
      Result := gCommandLineParams.GetParam('debuglog')
    else
    begin
      // if not found yet, then try to find in the environment variable
      EnvVarName  := ApplicationName + '_debuglog';
      Result      := GetEnvironmentVariable(EnvVarName);
    end;
    if (Result <> '') then
      Result := fpgExpandFileName(Result);
  end;

begin
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
  writeln('Rect left=', Rect.Left, ' top=', Rect.Top, ' right=', Rect.Right,
      ' bottom=', Rect.Bottom);
end;

procedure PrintRect(const Rect: TfpgRect);
begin
  writeln('Rect left=', Rect.Left, ' top=', Rect.Top, ' right=', Rect.Right,
      ' bottom=', Rect.Bottom, ' width=', Rect.Width, ' height=', Rect.Height);
end;

procedure PrintCoord(const x, y: TfpgCoord);
begin
  writeln('x=', x, '  y=', y);
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
  {$IFDEF DEBUG}
  Writeln(Format('%s>> %s.%s', [spacing, FClassName, FMethodName]));
  {$ENDIF}
end;

destructor TPrintCallTrace.Destroy;
begin
  {$IFDEF DEBUG}
  Writeln(Format('%s<< %s.%s', [spacing, FClassName, FMethodName]));
  {$ENDIF}
  dec(iCallTrace);
  inherited Destroy;
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
  writeln(' Stack trace:');
//  Dump_Stack(StdOut, get_frame);

  Writeln(stdout,'An unhandled exception occurred at $',HexStr(Ptrint(ExceptAddr),sizeof(PtrInt)*2),' :');
  if ExceptObject is exception then
   begin
     lMessage := Exception(ExceptObject).ClassName+' : '+Exception(ExceptObject).Message;
     Writeln(stdout,lMessage);
   end
  else
   Writeln(stdout,'Exception object ',ExceptObject.ClassName,' is not of class Exception.');
  Writeln(stdout,BackTraceStrFunc(ExceptAddr));
  if (ExceptFrameCount>0) then
    begin
      for i := 0 to ExceptFrameCount-1 do
        Writeln(stdout,BackTraceStrFunc(ExceptFrames[i]));
    end;
  Writeln(stdout,'');
end;

procedure DebugLn(const s1: TfpgString);
begin
  if not Assigned(uDebugText) then
    Exit; //==>
  writeln(uDebugText^, fpgConvertLineEndings(s1));
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

operator = (a: TRect; b: TRect): boolean;
begin
  if    (a.Top    = b.Top)
    and (a.Left   = b.Left)
    and (a.Bottom = b.Bottom)
    and (a.Right  = b.Right) then
    Result := True
  else
    Result := False;
end;

{ TfpgTimer }

procedure TfpgTimer.SetEnabled(const AValue: boolean);
begin
  if (not FEnabled) and AValue then
    FNextAlarm := now + (interval * ONE_MILISEC);
  FEnabled := AValue;
end;

procedure TfpgTimer.SetInterval(const AValue: integer);
begin
  FInterval := AValue;
  FNextAlarm := now + (FInterval * ONE_MILISEC);
end;

constructor TfpgTimer.Create(ainterval: integer);
begin
  FInterval := ainterval;
  OnTimer   := nil;
  FEnabled  := False;
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

procedure TfpgTimer.CheckAlarm(ctime: TDateTime);
begin
  if not FEnabled then
    Exit; //==>

  if FNextAlarm <= ctime then
  begin
    // set the next alarm point
    if interval > 0 then
      while FNextAlarm <= ctime do
        FNextAlarm += (interval * ONE_MILISEC);

    if Assigned(FOnTimer) then
      FOnTimer(self);
  end;
end;

procedure TfpgTimer.Reset;
begin
  Enabled := False;
  Enabled := True;
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
  if (col and cl_BaseNamedColor) <> 0 then
    Result := fpgNamedColors[col and $FF] or (col and $7F000000)// named color keeping alpha
  else
    Result := col;
end;

function fpgGetNamedColor(col: TfpgColor): TfpgColor;
begin
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

  {.$IFDEF DEBUG}
  Writeln('GetNamedFontDesc error: "' + afontid + '" is missing. Default is used.');
  {.$ENDIF}
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

function fpgGetFont(const afontdesc: string): TfpgFont;
begin
  Result := fpgApplication.GetFont(afontdesc);
end;

constructor TfpgApplication.Create(const AParams: string);
begin
  FFontResList    := TList.Create;
  FDisplayParams  := AParams;
  FScreenWidth    := -1;
  FScreenHeight   := -1;
  FMessageHookList := TFPList.Create;
  FStopOnException := False;
  FHintWindow     := nil;   // popup window with Hint text
  FHintPause      := DEFAULT_HINT_PAUSE;
  FHintWidget     := nil;   // widget the mouse is over and whos hint text we need.
  FShowHint       := True;

  try
    inherited Create(AParams);

    if IsInitialized then
    begin
      FScreenWidth  := GetScreenWidth;
      FScreenHeight := GetScreenHeight;
    end;

    FDefaultFont := GetFont(FPG_DEFAULT_FONT_DESC);
  except
    on E: Exception do
      Sysutils.ShowException(ExceptObject, ExceptAddr);
  end;
end;

destructor TfpgApplication.Destroy;
var
  i: integer;
begin
  if Assigned(FHintWindow) then
  begin
    HideHint;
    FHintWindow.Free;
  end;
  FHintTimer.Enabled := False;
  FHintTimer.OnTimer := nil;
  FHintTimer.Free;

  DestroyComponents;  // while message queue is still active

  for i := 0 to (fpgNamedFonts.Count - 1) do
    TNamedFontItem(fpgNamedFonts.Items[i]).Free;
  fpgNamedFonts.Free;

  fpgImages.Free;
  fpgStyle.Free;
  fpgCaret.Free;
  
  for i := fpgTimers.Count-1 downto 0 do
    if fpgTimers[i] <> nil then
      TfpgTimer(fpgTimers[i]).Free;
  fpgTimers.Free;

  FDefaultFont.Free;

  for i := FFontResList.Count-1 downto 0 do
  begin
    TfpgFontResource(FFontResList[i]).Free;
    FFontResList.Delete(i);
  end;
  FFontResList.Free;

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

function TfpgApplication.GetFont(const afontdesc: string): TfpgFont;
var
  fr: TfpgFontResource;
  n: integer;
  fdesc: string;
begin
  fdesc := afontdesc;

  if copy(fdesc, 1, 1) = '#' then   // A # (hash) denotes a named font
    fdesc := fpgGetNamedFontDesc(copy(afontdesc, 2, length(afontdesc)));

  Result := nil;

  for n := 0 to FFontResList.Count - 1 do
    if TfpgFontResource(FFontResList[n]).FontDesc = fdesc then
    begin
      fr     := TfpgFontResource(FFontResList[n]);
      Inc(fr.FRefCount);
      Result := TfpgFont.Create(fr, afontdesc);
      Exit; //==>
    end;

  fr := TfpgFontResource.Create(fdesc);

  if fr.HandleIsValid then
  begin
    FFontResList.Add(fr);
    Result := TfpgFont.Create(fr, afontdesc);
  end
  else
  begin
    fr.Free;
    {$IFDEF DEBUG}
    writeln('fpGFX: Error opening font.');
    {$ENDIF}
  end;
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
  w := wnd.Font.TextWidth(AHint) + (wnd.Border * 2) + (wnd.Margin * 2);
  h := wnd.Font.Height + (wnd.Border * 2) + (wnd.Margin * 2);
  { prevents hint from going off the right screen edge }
  if (APos.X + w) > ScreenWidth then
    APos.X:= ScreenWidth - w;
  wnd.SetPosition(APos.X, APos.Y, w, h);
  wnd.Show;
end;

procedure TfpgApplication.Initialize;
begin
  { TODO : Remember to process parameters!! }
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
  ShortDayNames[1] := rsShortSun;
  ShortDayNames[2] := rsShortMon;
  ShortDayNames[3] := rsShortTue;
  ShortDayNames[4] := rsShortWed;
  ShortDayNames[5] := rsShortThu;
  ShortDayNames[6] := rsShortFri;
  ShortDayNames[7] := rsShortSat;
  
  LongDayNames[1] := rsLongSun;
  LongDayNames[2] := rsLongMon;
  LongDayNames[3] := rsLongTue;
  LongDayNames[4] := rsLongWed;
  LongDayNames[5] := rsLongThu;
  LongDayNames[6] := rsLongFri;
  LongDayNames[7] := rsLongSat;

  ShortMonthNames[1] := rsShortJan;
  ShortMonthNames[2] := rsShortFeb;
  ShortMonthNames[3] := rsShortMar;
  ShortMonthNames[4] := rsShortApr;
  ShortMonthNames[5] := rsShortMay;
  ShortMonthNames[6] := rsShortJun;
  ShortMonthNames[7] := rsShortJul;
  ShortMonthNames[8] := rsShortAug;
  ShortMonthNames[9] := rsShortSep;
  ShortMonthNames[10] := rsShortOct;
  ShortMonthNames[11] := rsShortNov;
  ShortMonthNames[12] := rsShortDec;

  LongMonthNames[1] := rsLongJan;
  LongMonthNames[2] := rsLongFeb;
  LongMonthNames[3] := rsLongMar;
  LongMonthNames[4] := rsLongApr;
  LongMonthNames[5] := rsLongMay;
  LongMonthNames[6] := rsLongJun;
  LongMonthNames[7] := rsLongJul;
  LongMonthNames[8] := rsLongAug;
  LongMonthNames[9] := rsLongSep;
  LongMonthNames[10] := rsLongOct;
  LongMonthNames[11] := rsLongNov;
  LongMonthNames[12] := rsLongDec;

  SetLength(TrueBoolStrs,1);
  SetLength(FalseBoolStrs,1);
  TrueBoolStrs[0]   := rsTrue;
  FalseBoolStrs[0]  := rsFalse;
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
    FHintWidget := TfpgWindow(msg.Sender);
  end
  else
  begin
    { Handle mouse move information }
    FHintPos.X := msg.Params.user.Param2;
    FHintPos.Y := msg.Params.user.Param3;
    FHintWidget := TfpgWindow(msg.Sender);
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
begin
  w := nil;
//  writeln('HintTimerFired...');
  w := TfpgWidget(FHintWidget);
  try
    if Assigned(w) then
      ActivateHint(w.WindowToScreen(w, FHintPos), w.Hint);
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

procedure TfpgApplication.FreeFontRes(afontres: TfpgFontResource);
var
  n: integer;
begin
  for n := FFontResList.Count-1 downto 0 do
    if FFontResList[n] = Pointer(afontres) then
    begin
      TfpgFontResource(FFontResList[n]).Free;
      FFontResList.Delete(n);
      Exit; //==>
    end;
end;

procedure TfpgApplication.InternalInit;
begin
  fpgInitTimers;
  fpgNamedFonts := TList.Create;
  fpgStyle      := TfpgStyle.Create;
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
  while DoMessagesPending do
  begin
    WaitWindowMessage(0);
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
  {$IFDEF DEBUG}
  writeln('HideHint');
  {$ENDIF}
  FHintTimer.Enabled := False;
  if Assigned(FHintWindow) and TfpgHintWindow(FHintWindow).Visible then
    TfpgHintWindow(FHintWindow).Hide;
end;

procedure TfpgApplication.ShowException(E: Exception);
begin
  TfpgMessageDialog.Critical('An unexpected error occurred.', E.Message);
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
  WaitWindowMessage(1000);
end;

{ TfpgFont }

constructor TfpgFont.Create(afontres: TfpgFontResource; const afontdesc: string);
begin
  FFontRes  := afontres;
  FFontDesc := afontdesc;
  afontres.IncRefCount;
end;

destructor TfpgFont.Destroy;
begin
  if TfpgFontResource(FFontRes).DecRefCount <= 0 then
    fpgApplication.FreeFontRes(TfpgFontResource(FFontRes));
  inherited Destroy;
end;

{ TfpgFontResource }

constructor TfpgFontResource.Create(const afontdesc: string);
begin
  inherited;
  FFontDesc := afontdesc;
  FRefCount := 0;
end;

function TfpgFontResource.DecRefCount: integer;
begin
  Dec(FRefCount);
  Result := FRefCount;
end;

function TfpgFontResource.IncRefCount: integer;
begin
  Inc(FRefCount);
  Result := FRefCount;
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
    tw := Font.TextWidth(sub);            // wrap if needed
    if (lw + tw > aMaxLineWidth) and (lw > 0) then
    begin
      lw := tw;
      Result := TrimRight(Result) + sLineBreak;
    end else
      Inc(lw, tw);
    Result += sub;
  end;
end;

constructor TfpgCanvas.Create(awin: TfpgWindow);
begin
  inherited Create;

  FBeginDrawCount := 0;
  FWindow         := awin;

  // options
  FBufferedDraw        := True; // transparent widgets must turn this off
  FPersistentResources := False;
end;

destructor TfpgCanvas.Destroy;
begin
  if fpgCaret.FCanvas = self then
    fpgCaret.UnSetCaret(self);
  inherited Destroy;
end;

procedure TfpgCanvas.DrawButtonFace(x, y, w, h: TfpgCoord; AFlags: TFButtonFlags);
begin
  fpgStyle.DrawButtonFace(self, x, y, w, h, AFlags);
end;

procedure TfpgCanvas.DrawButtonFace(r: TfpgRect; AFlags: TFButtonFlags);
begin
  DrawButtonFace(r.Left, r.Top, r.Width, r.Height, AFlags);
end;

procedure TfpgCanvas.DrawControlFrame(x, y, w, h: TfpgCoord);
begin
  fpgStyle.DrawControlFrame(self, x, y, w, h);
end;

procedure TfpgCanvas.DrawControlFrame(r: TfpgRect);
begin
  DrawControlFrame(r.Left, r.Top, r.Width, r.Height);
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
                AFlags: TFTextFlags; ALineSpace: integer): integer;
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
      wtxt := Max(wtxt, Font.TextWidth(buf));
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

  htxt := (Font.Height * wraplst.Count) + (ALineSpace * Pred(wraplst.Count));
  
  // Now paint the actual text
  for i := 0 to wraplst.Count-1 do
  begin
    l :=  (Font.Height + ALineSpace) * i;
    wtxt := Font.TextWidth(wraplst[i]);

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
    AFlags: TFTextFlags; ALineSpace: integer): integer;
begin
  Result := DrawText(x, y, 0, 0, AText, AFlags, ALineSpace);
end;

function TfpgCanvas.DrawText(r: TfpgRect; const AText: TfpgString; AFlags: TFTextFlags;
    ALineSpace: integer): integer;
begin
  Result := DrawText(r.Left, r.Top, r.Width, r.Height, AText, AFlags, ALineSpace);
end;

{ TfpgWindow }

constructor TfpgWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); // initialize the platform internals

  FTop    := 0;
  FLeft   := 0;
  FWidth  := 16;
  FHeight := 16;

  FMinWidth  := 2;
  FMinHeight := 2;

  FModalForWin := nil;

  if (AOwner <> nil) and (AOwner is TfpgWindow) then
    FWindowType   := wtChild
  else
    FWindowType   := wtWindow;

  FCanvas := TfpgCanvas.Create(self);
end;

destructor TfpgWindow.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TfpgWindow.SetParent(const AValue: TfpgWindow);
begin
  inherited SetParent(AValue);
end;

function TfpgWindow.GetParent: TfpgWindow;
begin
  result := TfpgWindow(inherited GetParent);
end;

function TfpgWindow.GetCanvas: TfpgCanvas;
begin
  Result := TfpgCanvas(inherited GetCanvas);
end;


{ TfpgStyle }

constructor TfpgStyle.Create;
begin
  // Setup font aliases
  fpgSetNamedFont('Label1', FPG_DEFAULT_FONT_DESC);
  fpgSetNamedFont('Label2', FPG_DEFAULT_FONT_DESC + ':bold');
  fpgSetNamedFont('Edit1', FPG_DEFAULT_FONT_DESC);
  fpgSetNamedFont('Edit2', 'Courier New-10');
  fpgSetNamedFont('List', FPG_DEFAULT_FONT_DESC);
  fpgSetNamedFont('Grid', 'Arial-9');
  fpgSetNamedFont('GridHeader', 'Arial-9:bold');
  fpgSetNamedFont('Menu', FPG_DEFAULT_FONT_DESC);
  fpgSetNamedFont('MenuAccel', FPG_DEFAULT_FONT_DESC + ':underline');
  fpgSetNamedFont('MenuDisabled', FPG_DEFAULT_FONT_DESC);

  {$Note Refactor this so under Windows it can detect the system colors instead.
    Also under Linux (KDE and Gnome) we should be able to detect the system colors.}
  fpgSetNamedColor(clWindowBackground, $D5D2CD);
  fpgSetNamedColor(clBoxColor, $FFFFFF);
  fpgSetNamedColor(clShadow1, $848284);       // medium
  fpgSetNamedColor(clShadow2, $424142);       // dark
  fpgSetNamedColor(clHilite1, $E0E0E0);       // light
  fpgSetNamedColor(clHilite2, $FFFFFF);       // white
  fpgSetNamedColor(clText1, $000000);
  fpgSetNamedColor(clText2, $000040);
  fpgSetNamedColor(clText3, $800000);
  fpgSetNamedColor(clText4, $404000);
  fpgSetNamedColor(clSelection, $08246A);
  fpgSetNamedColor(clSelectionText, $FFFFFF);
  fpgSetNamedColor(clInactiveSel, $D0D0FF);
  fpgSetNamedColor(clInactiveSelText, $000000);
  fpgSetNamedColor(clScrollBar, $E8E4DB);
  fpgSetNamedColor(clButtonFace, $D5D2CD);
  fpgSetNamedColor(clListBox, $FFFFFF);
  fpgSetNamedColor(clGridLines, $A0A0A0);
  fpgSetNamedColor(clGridHeader, $D5D2CD);
  fpgSetNamedColor(clWidgetFrame, $000000);
  fpgSetNamedColor(clInactiveWgFrame, $A0A0A0);
  fpgSetNamedColor(clTextCursor, $000000);
  fpgSetNamedColor(clChoiceListBox, $E8E8E8);
  fpgSetNamedColor(clUnset, $D0D0FF);
  fpgSetNamedColor(clMenuText, $000000);
  fpgSetNamedColor(clMenuDisabled, $909090);
  fpgSetNamedColor(clHintWindow, $FFFFBF);


  // Global Font Objects
  DefaultFont      := fpgGetFont(fpgGetNamedFontDesc('Label1'));
  FixedFont        := fpgGetFont(fpgGetNamedFontDesc('Edit2'));
  MenuFont         := fpgGetFont(fpgGetNamedFontDesc('Menu'));
  MenuAccelFont    := fpgGetFont(fpgGetNamedFontDesc('MenuAccel'));
  MenuDisabledFont := fpgGetFont(fpgGetNamedFontDesc('MenuDisabled'));
end;

destructor TfpgStyle.Destroy;
begin
  DefaultFont.Free;
  FixedFont.Free;
  MenuFont.Free;
  MenuAccelFont.Free;
  MenuDisabledFont.Free;
  inherited Destroy;
end;

procedure TfpgStyle.DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; AFlags: TFButtonFlags);
var
  r: TfpgRect;
begin
  r.SetRect(x, y, w, h);

  if btfIsDefault in AFlags then
  begin
    ACanvas.SetColor(clBlack);
    ACanvas.SetLineStyle(1, lsSolid);
    ACanvas.DrawRectangle(r);
    InflateRect(r, -1, -1);
    Exclude(AFlags, btfIsDefault);
    fpgStyle.DrawButtonFace(ACanvas, r.Left, r.Top, r.Width, r.Height, AFlags);
    Exit; //==>
  end;

  ACanvas.SetColor(clButtonFace);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.FillRectangle(x, y, w, h);

  if (btfFlat in AFlags) and not (btfIsPressed in AFlags) then
    Exit; // no need to go further

  // Left and Top (outer)
  if (btfIsPressed in AFlags) then
  begin
    if (btfIsEmbedded in AFlags) then
      ACanvas.SetColor(clHilite2)
    else
      ACanvas.SetColor(clShadow2);
  end
  else
    ACanvas.SetColor(clHilite2);

  ACanvas.DrawLine(r.Left, r.Bottom, r.Left, r.Top);  // left
  ACanvas.DrawLine(r.Left, r.Top, r.Right, r.Top);    // top

  // Left and Top (inner)
  //if btnIsPressed in AFlags then
  //begin
    //ACanvas.SetColor(clShadow1);
    //ACanvas.DrawLine(x + 1, y + h - 3, x + 1, y + 1);  // left
    //ACanvas.DrawLine(x + 1, y + 1, x + w - 2, y + 1);  // top
  //end;

  // Right and Bottom (outer)
  if (btfIsPressed in AFlags) then
  begin
    if (btfIsEmbedded in AFlags) then
      ACanvas.SetColor(clHilite1)
    else
      ACanvas.SetColor(clShadow2);
  end
  else
    ACanvas.SetColor(clShadow2);
    
  ACanvas.DrawLine(r.Right, r.Top, r.Right, r.Bottom);   // right
  ACanvas.DrawLine(r.Right, r.Bottom, r.Left-1, r.Bottom);   // bottom

  // Right and Bottom (inner)
  if btfIsPressed in AFlags then
  begin
    if (btfIsEmbedded in AFlags) then
      ACanvas.SetColor(clButtonFace)
    else
      ACanvas.SetColor(clHilite1);
  end
  else
    ACanvas.SetColor(clShadow1);

  ACanvas.DrawLine(r.Right-1, r.Top+1, r.Right-1, r.Bottom-1);   // right
  ACanvas.DrawLine(r.Right-1, r.Bottom-1, r.Left, r.Bottom-1);   // bottom
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
  oldLineWidth  := ACanvas.LineWidth;
  oldLineStyle  := ACanvas.LineStyle;

  ACanvas.SetColor(clText1);
  ACanvas.SetLineStyle(1, lsDot);
  ACanvas.DrawRectangle(r);

  // restore previous settings
  ACanvas.SetColor(oldColor);
  ACanvas.SetLineStyle(oldLineWidth, oldLineStyle);
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
  FTimer.Enabled := True;
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
    FCanvas.BeginDraw(False);
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
    {$IFDEF DEBUG}
    writeln('TfpgCaret.InvertCaret cause an exception');
    {$ENDIF}
  end;
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

function TfpgImage.ImageFromRect(var ARect: TRect): TfpgImage;
var
  x, y: TfpgCoord;
  ix, iy: TfpgCoord;
begin
  SortRect(ARect);

  Result := TfpgImage.Create;
  Result.AllocateImage(ColorDepth, ARect.Right-ARect.Left, ARect.Bottom-ARect.Top);
  Result.UpdateImage;

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
end;

function TfpgImage.ImageFromRect(var ARect: TfpgRect): TfpgImage;
var
  x, y: TfpgCoord;
  ix, iy: TfpgCoord;
begin
  SortRect(ARect);

  Result := TfpgImage.Create;
  Result.AllocateImage(ColorDepth, ARect.Width, ARect.Height);
  Result.UpdateImage;
  
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
end;

initialization
  uApplication    := nil;
  uClipboard      := nil;
  uMsgQueueList   := nil;
  fpgTimers       := nil;
  fpgCaret        := nil;
  fpgImages       := nil;
  iCallTrace      := -1;
  InitializeDebugOutput;
  fpgInitMsgQueue;

finalization;
  uClipboard.Free;
  uApplication.Free;
  FinalizeDebugOutput;

end.

