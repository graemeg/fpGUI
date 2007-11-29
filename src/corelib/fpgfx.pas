unit fpgfx;

{$mode objfpc}{$H+}
{.$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,
  gfxbase
  // This is the only place we have such IFDEF!!! Is this ok, or must be
  // implement it like we have done for the previous version of fpGFX?
  {$IFDEF MSWINDOWS}
  ,gfx_gdi
  {$ENDIF}
  {$IFDEF UNIX}
  ,gfx_x11
  {$ENDIF}
  ;

type
  TOrientation = (orVertical, orHorizontal);

  TAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient);

  TAnchor  = (anLeft, anRight, anTop, anBottom);
  TAnchors = set of TAnchor;

  TFButtonFlags = set of (btnIsEmbedded, btnIsDefault, btnIsPressed,
    btnIsSelected, btnHasFocus, btnHasParentColor);
    
  TMouseButton = (mbLeft, mbRight, mbMiddle);


const
  AllAnchors = [anLeft, anRight, anTop, anBottom];

  // Used for the internal message queue
  cMessageQueueSize = 512;
  
  // version and name constants
  fpGUIVersion = '0.5.1';
  fpGUIName    = 'fpGUI Library';


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
    function    ImageFromRect(var ARect: TRect): TfpgImage;
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
  public
    constructor Create(awin: TfpgWindow); reintroduce;
    destructor  Destroy; override;
    
    // As soon as TfpgStyle has moved out of CoreLib, these must go!
    procedure   DrawButtonFace(x, y, w, h: TfpgCoord; AFlags: TFButtonFlags);
    procedure   DrawButtonFace(r: TfpgRect; AFlags: TFButtonFlags);
    procedure   DrawControlFrame(x, y, w, h: TfpgCoord);
    procedure   DrawControlFrame(r: TfpgRect);
    procedure   DrawDirectionArrow(x, y, w, h: TfpgCoord; direction: integer);
    procedure   DrawDirectionArrow(r: TfpgRect; direction: integer);
    procedure   DrawFocusRect(r: TfpgRect);
  end;


  { This is very basic for now, just to remind us of theming support. Later we
    will rework this to use a Style Manager like the previous fpGUI. Styles must
    also move out of fpGFX. Also support Bitmap based styles for easier theme
    implementations. }
  TfpgStyle = class
  public
    DefaultFont: TfpgFont;
    MenuFont: TfpgFont;
    MenuAccelFont: TfpgFont;
    MenuDisabledFont: TfpgFont;
  public
    constructor Create; virtual;
    procedure   DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; AFlags: TFButtonFlags); virtual;
    procedure   DrawControlFrame(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord); virtual;
    procedure   DrawDirectionArrow(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; direction: integer); virtual;
    procedure   DrawString(ACanvas: TfpgCanvas; x, y: TfpgCoord; AText: string; AEnabled: boolean = True); virtual;
    procedure   DrawFocusRect(ACanvas: TfpgCanvas; r: TfpgRect); virtual;
  end;


  TfpgApplication = class(TfpgApplicationImpl)
  protected
    FDisplayParams: string;
    FScreenWidth: integer;
    FScreenHeight: integer;
    FDefaultFont: TfpgFont;
    FFontResList: TList;
    procedure   FreeFontRes(afontres: TfpgFontResource);
    procedure   InternalInit;
    procedure   RunMessageLoop;
    procedure   WaitWindowMessage(atimeoutms: integer);
  public
    constructor Create(const aparams: string = ''); override;
    destructor  Destroy; override;
    function    GetFont(const afontdesc: string): TfpgFont;
    procedure   Initialize;
    procedure   Run;
    procedure   Flush;
    procedure   ProcessMessages;
    property    ScreenWidth: integer read FScreenWidth;
    property    ScreenHeight: integer read FScreenHeight;
    property    DefaultFont: TfpgFont read FDefaultFont;
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
    constructor Create(ainterval: integer);
    destructor  Destroy; override;
    procedure   CheckAlarm(ctime: TDateTime);
    property    Enabled: boolean read FEnabled write SetEnabled;
    property    NextAlarm: TDateTime read FNextAlarm;
    property    Interval: integer read FInterval write SetInterval;
    property    OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;


  { Caret or text cursor, inverts painting over text and has blinking
    support. }
  TfpgCaret = class
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

var
  fpgStyle:  TfpgStyle;   // move this into fpgApplication
  fpgCaret:  TfpgCaret;   // move this into fpgApplication
  fpgImages: TfpgImages;  // move this into fpgApplication

// Application singleton
function  fpgApplication: TfpgApplication;

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

// Color routines
function  fpgColorToRGB(col: TfpgColor): TfpgColor;
function  fpgGetNamedColor(col: TfpgColor): TfpgColor;
procedure fpgSetNamedColor(colorid, rgbvalue: longword);
function  fpgGetNamedFontDesc(afontid: string): string;
procedure fpgSetNamedFont(afontid, afontdesc: string);

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

// Debug rountines
procedure PrintRect(var Rect: TRect);
procedure PrintRect(var Rect: TfpgRect);
procedure DumpStack;


implementation

uses
  gfx_imgfmt_bmp,
  gfx_stdimages;

var
  fpgTimers:      TList;
  fpgNamedColors: array[0..255] of TfpgColor;
  fpgNamedFonts:  TList;
  uApplication:   TfpgApplication;

const
  ONE_MILISEC = 1 / (24 * 60 * 60 * 1000);

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

{$include gfx_msgqueue.inc}

// Timer support

procedure fpgInitTimers;
begin
  if fpgTimers = nil then
    fpgTimers := TList.Create;
end;

procedure fpgCheckTimers;
var
  n: integer;
  ctime: TDateTime;
begin
  ctime := now;
  for n := 1 to fpgTimers.Count do
    TfpgTimer(fpgTimers[n - 1]).CheckAlarm(ctime);
end;

function fpgClosestTimer(ctime: TDateTime; amaxtime: integer): integer;
var
  n: integer;
  t: TfpgTimer;
  dt: TDateTime;
  tb: Boolean;
begin
  // returns -1 if no timers are pending
  dt := ctime + amaxtime * ONE_MILISEC;
  tb := False;

  for n := 1 to fpgTimers.Count do
  begin
    t := TfpgTimer(fpgTimers[n - 1]);
    if t.Enabled and (t.NextAlarm < dt) then
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
  Result := DWord(Trunc(Now * 24 * 60 * 60 * 1000));
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

procedure PrintRect(var Rect: TRect);
begin
  writeln('Rect left=', Rect.Left, ' top=', Rect.Top, ' right=', Rect.Right,
      ' bottom=', Rect.Bottom);
end;

procedure PrintRect(var Rect: TfpgRect);
begin
  writeln('Rect left=', Rect.Left, ' top=', Rect.Top, ' right=', Rect.Right,
      ' bottom=', Rect.Bottom, ' width=', Rect.Width, ' height=', Rect.Height);
end;

procedure DumpStack;
begin
  writeln(' Stack trace:');
  Dump_Stack(StdOut, get_frame);
end;

{ TfpgTimer }

procedure TfpgTimer.SetEnabled(const AValue: boolean);
begin
  if not FEnabled and AValue then
    FNextAlarm := now + interval * ONE_MILISEC;
  FEnabled := AValue;
end;

procedure TfpgTimer.SetInterval(const AValue: integer);
begin
  FInterval := AValue;
  FNextAlarm := now + FInterval * ONE_MILISEC;
end;

constructor TfpgTimer.Create(ainterval: integer);
begin
  FInterval := ainterval;
  OnTimer  := nil;
  FEnabled := False;
  fpgTimers.Add(self);
end;

destructor TfpgTimer.Destroy;
var
  i: integer;
begin
  i := fpgTimers.IndexOf(self);
  if i >= 0 then
    fpgTimers.Delete(i);
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
        FNextAlarm := FNextAlarm + interval * ONE_MILISEC;

    if Assigned(FOnTimer) then
      FOnTimer(self);
  end;
end;

function  fpgApplication: TfpgApplication;
begin
  if not Assigned(uApplication) then
    uApplication := TfpgApplication.Create;
  result := uApplication;
end;

function fpgColorToRGB(col: TfpgColor): TfpgColor;
begin
  if (col and $80000000) <> 0 then
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
  if (colorid and $80000000) = 0 then
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

  Writeln('GetNamedFontDesc error: "' + afontid + '" is missing. Default is used.');
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

procedure fpgWaitWindowMessage;
begin
  fpgApplication.WaitWindowMessage(0);
end;

function fpgGetFont(const afontdesc: string): TfpgFont;
begin
  Result := fpgApplication.GetFont(afontdesc);
end;

constructor TfpgApplication.Create(const aparams: string);
begin
  FFontResList    := TList.Create;
  FDisplayParams  := aparams;
  FScreenWidth    := -1;
  FScreenHeight   := -1;
  FModalFormStack := TList.Create;

  inherited Create(aparams);
  
  if IsInitialized then
  begin
    FScreenWidth  := GetScreenWidth;
    FScreenHeight := GetScreenHeight;
  end;

  FDefaultFont := GetFont(FPG_DEFAULT_FONT_DESC);
end;

destructor TfpgApplication.Destroy;
var
  i: integer;
begin
  for i := 0 to (fpgNamedFonts.Count - 1) do
    TNamedFontItem(fpgNamedFonts.Items[i]).Free;
  fpgNamedFonts.Free;
  
  fpgImages.Free;

  FFontResList.Free;
  FDefaultFont.Free;
  FreeAndNil(FModalFormStack);
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
    writeln('error opening font.');
  end;
end;

procedure TfpgApplication.Initialize;
begin
  {$Note Remember to process parameters!! }
  if IsInitialized then
    InternalInit
  else
    raise Exception.Create('Failed in initialize the Application object!');
end;

procedure TfpgApplication.Run;
begin
  RunMessageLoop;
end;

procedure TfpgApplication.FreeFontRes(afontres: TfpgFontResource);
var
  n: integer;
begin
  for n := 0 to FFontResList.Count - 1 do
    if FFontResList[n] = Pointer(afontres) then
    begin
      TfpgFontResource(FFontResList[n]).Free;
      FFontResList.Delete(n);
      Exit;
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

procedure TfpgApplication.WaitWindowMessage(atimeoutms: integer);
begin
  DoWaitWindowMessage(fpgClosestTimer(now, atimeoutms));
  fpgDeliverMessages;
  fpgCheckTimers;
end;

procedure TfpgApplication.RunMessageLoop;
begin
  repeat
    WaitWindowMessage(1000);
  until Terminated;
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

  inherited;
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

procedure TfpgCanvas.DrawDirectionArrow(x, y, w, h: TfpgCoord; direction: integer);
begin
  fpgStyle.DrawDirectionArrow(self, x, y, w, h, direction);
end;

procedure TfpgCanvas.DrawDirectionArrow(r: TfpgRect; direction: integer);
begin
  DrawDirectionArrow(r.Left, r.Top, r.Width, r.Height, direction);
end;

procedure TfpgCanvas.DrawFocusRect(r: TfpgRect);
begin
  fpgStyle.DrawFocusRect(self, r);
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
  fpgSetNamedColor(clWindowBackground, $D4D0C8);
  fpgSetNamedColor(clBoxColor, $FFFFFF);
  fpgSetNamedColor(clShadow1, $808080);
  fpgSetNamedColor(clShadow2, $404040);
  fpgSetNamedColor(clHilite1, $E0E0E0);
  fpgSetNamedColor(clHilite2, $FFFFFF);
  fpgSetNamedColor(clText1, $000000);
  fpgSetNamedColor(clText2, $000040);
  fpgSetNamedColor(clText3, $800000);
  fpgSetNamedColor(clText4, $404000);
  fpgSetNamedColor(clSelection, $0A246A);
  fpgSetNamedColor(clSelectionText, $FFFFFF);
  fpgSetNamedColor(clInactiveSel, $D0D0FF);
  fpgSetNamedColor(clInactiveSelText, $000000);
  fpgSetNamedColor(clScrollBar, $E8E4DB);
  fpgSetNamedColor(clButtonFace, $D4D0C8);
  fpgSetNamedColor(clListBox, $FFFFFF);
  fpgSetNamedColor(clGridLines, $A0A0A0);
  fpgSetNamedColor(clGridHeader, $D4D0C8);
  fpgSetNamedColor(clWidgetFrame, $000000);
  fpgSetNamedColor(clInactiveWgFrame, $A0A0A0);
  fpgSetNamedColor(clTextCursor, $000000);
  fpgSetNamedColor(clChoiceListBox, $E8E8E8);
  fpgSetNamedColor(clUnset, $D0D0FF);
  fpgSetNamedColor(clMenuText, $000000);
  fpgSetNamedColor(clMenuDisabled, $909090);

  // Global Font Objects
  DefaultFont      := fpgGetFont(fpgGetNamedFontDesc('Label1'));
  MenuFont         := fpgGetFont(fpgGetNamedFontDesc('Menu'));
  MenuAccelFont    := fpgGetFont(fpgGetNamedFontDesc('MenuAccel'));
  MenuDisabledFont := fpgGetFont(fpgGetNamedFontDesc('MenuDisabled'));
end;

procedure TfpgStyle.DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; AFlags: TFButtonFlags);
var
  r: TfpgRect;
begin
  r.SetRect(x, y, w, h);
  if btnIsDefault in AFlags then
  begin
    ACanvas.SetColor(clBlack);
    ACanvas.SetLineStyle(1, lsSolid);
    ACanvas.DrawRectangle(r);
    InflateRect(r, -1, -1);
    Exclude(AFlags, btnIsDefault);
    fpgStyle.DrawButtonFace(ACanvas, r.Left, r.Top, r.Width, r.Height, AFlags);
    Exit; //==>
  end;

  ACanvas.SetColor(clButtonFace);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.FillRectangle(x, y, w, h);

  // Left and Top (outer)
  if (btnIsPressed in AFlags) then
  begin
    if (btnIsEmbedded in AFlags) then
      ACanvas.SetColor(clHilite1)
    else
      ACanvas.SetColor(clShadow2);
  end
  else
    ACanvas.SetColor(clHilite1);
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
  if (btnIsPressed in AFlags) then
  begin
    if (btnIsEmbedded in AFlags) then
      ACanvas.SetColor(clHilite1)
    else
      ACanvas.SetColor(clShadow2);
  end
  else
    ACanvas.SetColor(clShadow2);
  ACanvas.DrawLine(r.Right, r.Top, r.Right, r.Bottom);   // right
  ACanvas.DrawLine(r.Right, r.Bottom, r.Left-1, r.Bottom);   // bottom

  // Right and Bottom (inner)
  if btnIsPressed in AFlags then
  begin
    if (btnIsEmbedded in AFlags) then
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

procedure TfpgStyle.DrawDirectionArrow(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; direction: integer);
var
  peekx: integer;
  peeky: integer;
  basex: integer;
  basey: integer;
  side: integer;
  margin: integer;
begin
//  ACanvas.SetColor(clText1);
  side   := (w div 4) + 1;
  margin := side + 1;

  if direction < 2 then  // vertical
  begin
    peekx := x + (w div 2);
    if direction = 1 then  // down
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
    if direction = 3 then  // right
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
end;

procedure TfpgStyle.DrawString(ACanvas: TfpgCanvas; x, y: TfpgCoord;
  AText: string; AEnabled: boolean);
begin
  if not AEnabled then
  begin
    ACanvas.SetTextColor(clHilite1);
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
  FTimer.Free;
  inherited Destroy;
end;

procedure TfpgCaret.SetCaret(acanvas: TfpgCanvas; x, y, w, h: TfpgCoord);
begin
  FEnabled := True;
  FVisible := False;
  FCanvas  := acanvas;
  FLeft    := x;
  FTop     := y;
  FWidth   := w;
  FHeight  := h;
  InvertCaret;

  FTimer.Enabled  := False;
  FTimer.Interval := FInterval;
  FTimer.Enabled  := True;
end;

procedure TfpgCaret.UnSetCaret(acanvas: TfpgCanvas);
begin
  if (FCanvas = acanvas) or (acanvas = nil) then
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
  n: integer;
begin
  for n := 0 to FImages.Count - 1 do
    FImages.Objects[n].Free;
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

initialization
  uApplication    := nil;
  fpgTimers       := nil;
  fpgCaret        := nil;
  fpgImages       := nil;
  fpgInitMsgQueue;

finalization;
  fpgCaret.Free;
  uApplication.free;

end.

