unit fpg_wayland_classes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, Contnrs, AVL_Tree, fgl,
  wayland_client_core,
  wayland_protocol,
  wayland_util,
  wayland_cursor,
  xdg_shell_protocol
  ,xdg_decoration_unstable_v1_protocol
  ;

type
  TfpgwDisplay = class;
  TfpgwWindow = class;
  TfpgwCursor = class;
  TfpgwShellSurfaceCommon = class;
  TfpgwShellSurfaceClass = class of TfpgwShellSurfaceCommon;

  { TfpgwCallbackHelper }

  TfpgwCallbackHelper = class(IWlCallbackListener)
  private
    FCallback: TWlCallback;
    FNotify: TNotifyEvent;
    procedure wl_callback_done(AWlCallback: TWlCallback; ACallbackData: DWord);
  public
    property Notify: TNotifyEvent read FNotify;
    property Callback: TWlCallback read FCallback;
    constructor Create(ADisplay: TfpgwDisplay; ANotify: TNotifyEvent);
  end;

  TfpgwMouseEnterEvent = procedure(Sender: TObject; AX, AY: Integer) of object;
  TfpgwMouseLeaveEvent = procedure(Sender: TObject) of object;
  TfpgwMouseMotionEvent = procedure(Sender: TObject; ATime: LongWord; AX, AY: Integer) of object;
  TfpgwMouseAxisEvent = procedure(Sender: TObject; ATime: LongWord; AAxis: LongWord; AValue: LongInt) of object;
  TfpgwMouseButtonEvent = procedure(Sender: TObject; ATime: LongWord; AButton: LongWord; AState: LongInt) of object;

  TfpgwKeyboardKeymap = procedure(Sender: TObject; AFormat: LongWord; AFileDesc: LongInt; ASize: LongInt) of object;
  TfpgwKeyboardEnter = procedure(Sender: TObject; AKeys: Pwl_array)of object;
  TfpgwKeyboardLeave = procedure(Sender: TObject) of object;
  TfpgwKeyboardKey = procedure(Sender: TObject; ATime, AKey, AState: LongWord) of object;
  TfpgwKeyboardModifiers = procedure(Sender: TObject; AModsDepressed, AModsLatched, AmodsLocked, AGroup: LongWord) of object;
  TfpgwKeyboardRepeatInfo = procedure(Sender: TObject; ARate, ADelay: LongInt) of object;

  { TfpgwRegistryEntry }

  TfpgwRegistryEntry = class
  private
    FInterface: String;
    FName: DWord;
    FVersion: DWord;
  public
    constructor Create(AName: DWord; AInterface: String; AVersion: DWord);
  published
    property Name: DWord read FName write FName;
    property Interface_: String read FInterface write FInterface;
    property Version: DWord read FVersion write FVersion;
  end;

  TfpgwRegistryList = specialize TFPGObjectList<TfpgwRegistryEntry>;

  { TfpgwDisplay }

  TfpgwDisplay = class(IWlRegistryListener,
                       IWlShmListener,
                       IWlSeatListener,
                       IWlPointerListener,
                       IWlKeyboardListener,
                       IXdgWmBaseListener)
  private
    FSurfaceClass: TfpgwShellSurfaceClass;
    FCapabilities: LongWord;
    FDisplay: TWlDisplay;
    Ffd: PtrInt;
    FQueue: Pwl_event_queue;
    FRegistry: TWlRegistry;
    FCompositor: TWlCompositor;
    FSubcompositor: TWlSubcompositor;
    FFormats: LongWord;
    FSeat: TWlSeat;
    FShell: TWlShell;
    FShm: TWlShm;
    FXDGShell: TXdgWmBase;
    FMouse: TWlPointer;
    FKeyboard: TWlKeyboard;
    FDecorationManager: TZxdgDecorationManagerV1;
    FRegList: TfpgwRegistryList;
    function GetConnected: Boolean;
    // interface implementations
    // registry
    procedure wl_registry_global(AWlRegistry: TWlRegistry; AName: DWord; AInterface: String; AVersion: DWord);
    procedure wl_registry_global_remove(AWlRegistry: TWlRegistry; AName: DWord);
    // shm
    procedure wl_shm_format(AWlShm: TWlShm; AFormat: DWord);
    // seat
    procedure wl_seat_capabilities(AWlSeat: TWlSeat; ACapabilities: DWord);
    procedure wl_seat_name(AWlSeat: TWlSeat; AName: String);
    // pointer
    procedure wl_pointer_enter(AWlPointer: TWlPointer; ASerial: DWord; ASurface: TWlSurface; ASurfaceX: Longint{24.8}; ASurfaceY: Longint{24.8});
    procedure wl_pointer_leave(AWlPointer: TWlPointer; ASerial: DWord; ASurface: TWlSurface);
    procedure wl_pointer_motion(AWlPointer: TWlPointer; ATime: DWord; ASurfaceX: Longint{24.8}; ASurfaceY: Longint{24.8});
    procedure wl_pointer_button(AWlPointer: TWlPointer; ASerial: DWord; ATime: DWord; AButton: DWord; AState: DWord);
    procedure wl_pointer_axis(AWlPointer: TWlPointer; ATime: DWord; AAxis: DWord; AValue: Longint{24.8});
    procedure wl_pointer_frame(AWlPointer: TWlPointer);
    procedure wl_pointer_axis_source(AWlPointer: TWlPointer; AAxisSource: DWord);
    procedure wl_pointer_axis_stop(AWlPointer: TWlPointer; ATime: DWord; AAxis: DWord);
    procedure wl_pointer_axis_discrete(AWlPointer: TWlPointer; AAxis: DWord; ADiscrete: LongInt);
    // keyboard
    procedure wl_keyboard_keymap(AWlKeyboard: TWlKeyboard; AFormat: DWord; AFd: LongInt{fd}; ASize: DWord);
    procedure wl_keyboard_enter(AWlKeyboard: TWlKeyboard; ASerial: DWord; ASurface: TWlSurface; AKeys: Pwl_array);
    procedure wl_keyboard_leave(AWlKeyboard: TWlKeyboard; ASerial: DWord; ASurface: TWlSurface);
    procedure wl_keyboard_key(AWlKeyboard: TWlKeyboard; ASerial: DWord; ATime: DWord; AKey: DWord; AState: DWord);
    procedure wl_keyboard_modifiers(AWlKeyboard: TWlKeyboard; ASerial: DWord; AModsDepressed: DWord; AModsLatched: DWord; AModsLocked: DWord; AGroup: DWord);
    procedure wl_keyboard_repeat_info(AWlKeyboard: TWlKeyboard; ARate: LongInt; ADelay: LongInt);
    //xdg-shell
    procedure xdg_wm_base_ping(AXdgWmBase: TXdgWmBase; ASerial: DWord);
  private
    FCursor: TfpgwCursor;
    FEventSerial: LongWord;
    FOnKeyboardEnter: TfpgwKeyboardEnter;
    FOnKeyboardKey: TfpgwKeyboardKey;
    FOnKeyboardKeymap: TfpgwKeyboardKeymap;
    FOnKeyboardLeave: TfpgwKeyboardLeave;
    FOnKeyboardModifiers: TfpgwKeyboardModifiers;
    FOnKeyBoardRepeatInfo: TfpgwKeyboardRepeatInfo;
    FOnMouseAxis: TfpgwMouseAxisEvent;
    FOnMouseButton: TfpgwMouseButtonEvent;
    FOnMouseEnter: TfpgwMouseEnterEvent;
    FOnMouseLeave: TfpgwMouseLeaveEvent;
    FOnMouseMotion: TfpgwMouseMotionEvent;
    FActiveMouseWin: TfpgwWindow;
    FActiveKeyboardWin: TfpgwWindow;
    FOwner: TObject;
    FSupportsServerSideDecorations: Boolean;
    FUserDataList: TAVLTree;
    FPopupStack: TfpList;
    FSerial: DWord;
  protected
    function NextSerial: DWord;
  public
    // wayland objects
    property Display: TWlDisplay read FDisplay;
    property Registry: TWlRegistry read FRegistry;
    property Compositor: TWlCompositor read FCompositor;
    property SubCompositor: TWlSubcompositor read FSubcompositor;
    property Shell: TWlShell read FShell;
    property Shm: TWlShm read FShm;
    property Seat: TWlSeat read FSeat;
    property Formats: LongWord read FFormats;
    property Mouse: TWlPointer read FMouse;
    property Keyboard: TWlKeyboard read FKeyboard;
    property fd: PtrInt read Ffd; // display file descriptor
    property Queue: Pwl_event_queue read FQueue;
    property Capabilities: LongWord read FCapabilities;  // keyboard, pointer, touch
    property Cursor: TfpgwCursor read FCursor;

    property OnMouseEnter: TfpgwMouseEnterEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TfpgwMouseLeaveEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseButton: TfpgwMouseButtonEvent read FOnMouseButton write FOnMouseButton;
    property OnMouseMotion: TfpgwMouseMotionEvent read FOnMouseMotion write FOnMouseMotion;
    property OnMouseAxis: TfpgwMouseAxisEvent read FOnMouseAxis write FOnMouseAxis;
    property OnKeyboardKeymap: TfpgwKeyboardKeymap read FOnKeyboardKeymap write FOnKeyboardKeymap;
    property OnKeyboardEnter: TfpgwKeyboardEnter read FOnKeyboardEnter write FOnKeyboardEnter;
    property OnKeyboardLeave: TfpgwKeyboardLeave read FOnKeyboardLeave write FOnKeyboardLeave;
    property OnKeyboardKey: TfpgwKeyboardKey read FOnKeyboardKey write FOnKeyboardKey;
    property OnKeyboardModifiers: TfpgwKeyboardModifiers read FOnKeyboardModifiers write FOnKeyboardModifiers;
    property OnKeyBoardRepeatInfo: TfpgwKeyboardRepeatInfo read FOnKeyBoardRepeatInfo write FOnKeyBoardRepeatInfo;


    property Connected: Boolean read GetConnected;
    property EventSerial: LongWord read FEventSerial; // the last serial sent from the server

    class function TryCreate(AOwner: TObject; AName: String = ''): TfpgwDisplay;
    constructor Create(AOwner: TObject; AName: String = '');
    procedure   AfterCreate; // call this after the events are set to complete create
    destructor  Destroy; override;


    procedure Flush;
    procedure Roundtrip;
    procedure AddUserData(ALookup: Pointer; AData: TObject);
    function  GetUserData(ALookup: Pointer): Pointer;
    procedure RemoveUserData(Alookup: Pointer);
    function  HasEvent(ATimeout: Integer=0; AWillRead: Boolean=False): Boolean;
    procedure WaitEvent(ATimeOut: Integer);
    procedure SetCursor(ACursors: array of String);
    property Owner: TObject read FOwner;
    property ActiveMouseWin: TfpgwWindow read FActiveMouseWin;
    property SupportsServerSideDecorations: Boolean read FSupportsServerSideDecorations;
  end;

  { TfpgwSharedPool }

  TfpgwSharedPool = class
  private
    FDisplay: TfpgwDisplay;
    FPool: TWlShmPool;
    FData: Pointer;
    FFd: LongWord;
    FAllocated: LongWord;
    procedure GrowPool(ANewSize: LongWord);
  public
    constructor Create(ADisplay: TfpgwDisplay);
    destructor  Destroy; override;
    function GetBuffer(AWidth, AHeight: Integer; AFormat: DWord; out AData: Pointer): TWlBuffer;
  end;

  { TfpgwBuffer }

  TfpgwBuffer = class(IWlBufferListener)
  private
    FDisplay: TfpgwDisplay;
    FBuffer: TWlBuffer;
    FData: Pointer; {shm}
    FBusy: Boolean;
    FHeight: Integer;
    FNext: TfpgwBuffer;
    FWidth: Integer;
    FRect: TRect;
    FPool: TfpgwSharedPool;
    procedure FreeBuffer;
    function GetAllocated(AWidth, AHeight: Integer): Boolean;
    function GetStride: Integer;
    procedure wl_buffer_release(AWlBuffer: TWlBuffer);
  public
    constructor Create(ADisplay: TfpgwDisplay);
    destructor Destroy; override;
    procedure SetPaintRect(AX, AY, AWidth, Aheight: Integer);
    procedure Allocate(AWidth, AHeight: Integer; AFormat: LongWord{=WL_SHM_FORMAT_RGBA8888});
    property Allocated[AWidth, AHeight: Integer]: Boolean read GetAllocated;
    property Busy: Boolean read FBusy write FBusy;
    property Data: Pointer read FData;
    property Buffer: TWlBuffer read FBuffer;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Stride: Integer read GetStride;
    property Next: TfpgwBuffer read FNext write FNext;
    property PaintArea: TRect read FRect;
  end;

  { TfpgwShellSurfaceCommon }

  TfpgwShellSurfaceCommon = class(IWlSurfaceListener)
  private
    FDisplay: TfpgwDisplay;
    FWin: TfpgwWindow;
    FSurface: TWlSurface;
    FSubSurface: TWlSubsurface;
    procedure wl_surface_enter(AWlSurface: TWlSurface; AOutput: TWlOutput);
    procedure wl_surface_leave(AWlSurface: TWlSurface; AOutput: TWlOutput);
  protected
    FOutput: TWlOutput;
  public
    constructor Create(ADisplay: TfpgwDisplay; AWin: TfpgwWindow); virtual;
    destructor  Destroy; override;
    procedure Commit;
    procedure SetOpaqueRegion(ARegion: TRect);
    procedure SetTitle(AValue: String); virtual; abstract;
    procedure SetFullscreen(AValue: Boolean); virtual; abstract;
    procedure SetMaximized(AValue: Boolean); virtual; abstract;
    function IsMaximized: Boolean; virtual; abstract;
    procedure SetMinimized; virtual; abstract;
    procedure Move(Serial: LongWord); virtual; abstract;
    procedure Resize(ASerial: DWord; AEdges: DWord); virtual; abstract;


    // roles. only one is valid
    procedure SetToplevel; virtual; abstract;
    procedure SetPopup(AParent: TfpgwWindow; AX, AY: Integer); virtual; abstract;
    procedure SetSubSurface(AParent: TfpgwShellSurfaceCommon); virtual;
    property Surface: TWlSurface read FSurface;
    property SubSurface: TWlSubsurface read FSubSurface;
  end;

  { TfpgwWLShellSurface }

  TfpgwWLShellSurface = class(TfpgwShellSurfaceCommon, IWlShellSurfaceListener)
  private
    FShellSurface: TWlShellSurface;
    procedure wl_shell_surface_ping(AWlShellSurface: TWlShellSurface; ASerial: DWord);
    procedure wl_shell_surface_configure(AWlShellSurface: TWlShellSurface; AEdges: DWord; AWidth: LongInt; AHeight: LongInt);
    procedure wl_shell_surface_popup_done(AWlShellSurface: TWlShellSurface);
  public
    constructor Create(ADisplay: TfpgwDisplay; AWin: TfpgwWindow); override;
    destructor  Destroy; override;
    procedure SetToplevel; override;
    procedure SetPopup(AParent: TfpgwWindow; AX, AY: Integer); override;
    procedure SetTitle(AValue: String);  override;
    procedure SetFullscreen(AValue: Boolean); override;
    procedure SetMaximized(AValue: Boolean); override;
    procedure SetMinimized; override;
    procedure Move(Serial: LongWord); override;
    procedure Resize(ASerial: DWord; AEdges: DWord); override;
    property ShellSurface: TWlShellSurface read FShellSurface;

  end;

  { TfpgwXDGShellSurface }

  TfpgwXDGShellSurface = class(TfpgwShellSurfaceCommon,
                               IXdgSurfaceListener,
                               IXdgToplevelListener,
                               IXdgPopupListener)
  private
    FXdgSurface: TXdgSurface;
    procedure xdg_surface_configure(AXdgSurface: TXdgSurface; ASerial: DWord);
  private
    FToplevel: TXdgToplevel;
    FState: DWord;
    procedure xdg_toplevel_configure(AXdgToplevel: TXdgToplevel; AWidth: LongInt; AHeight: LongInt; AStates: Pwl_array);
    procedure xdg_toplevel_close(AXdgToplevel: TXdgToplevel);
  private
    FPopup: TXdgPopup;
    procedure xdg_popup_configure(AXdgPopup: TXdgPopup; AX: LongInt; AY: LongInt; AWidth: LongInt; AHeight: LongInt);
    procedure xdg_popup_popup_done(AXdgPopup: TXdgPopup);
  public
    constructor Create(ADisplay: TfpgwDisplay; AWin: TfpgwWindow); override;
    destructor  Destroy; override;
    procedure SetTitle(AValue: String); override;
    function  IsMaximized: Boolean; override;
    procedure SetMaximized(AValue: Boolean); override;
    procedure SetMinimized; override;
    procedure SetPopup(AParent: TfpgwWindow; AX, AY: Integer); override;
    procedure SetToplevel; override;
    procedure Move(Serial: LongWord); override;
    procedure Resize(ASerial: DWord; AEdges: DWord); override;
    property Toplevel: TXdgToplevel read FToplevel;
    property Popup: TXdgPopup read FPopup;
    property Surface: TXdgSurface read FXdgSurface;
  end;

  TfpgwShellConfigureEvent = procedure(Sender: TObject; AEdges: LongWord; AWidth, AHeight: LongInt) of object;

  TfpgwWindowDecorator = class;

  { TfpgwWindow }

  TfpgwWindow = class(IWlCallbackListener)
  private
    FDisplay: TfpgwDisplay;
    FBuffers: Array[0..1] of TfpgwBuffer;
    FReadyBuffer: TfpgwBuffer;
    FServerReadyToPaint: Boolean;
    FEntered: Boolean; // only useful if decoration assigned.
    //interfaces
    // frame draw
    procedure wl_callback_done(AWlCallback: TWlCallback; ACallbackData: DWord); // redraw
  private
    FClientHeight: Integer;
    FClientWidth: Integer;
    FOnClose: TNotifyEvent;
    FOnConfigure: TfpgwShellConfigureEvent;
    FOnPaint: TNotifyEvent;
    FOwner: TObject;
    FSurfaceShell: TfpgwShellSurfaceCommon;
    //FTopLevel: TfpgwWindow;
    FWindowState: DWord;
    FClientArea: TRect; // if toplevel then the decorations might resize the window.
    FDecorations: TfpgwWindowDecorator;
  public
    constructor Create(AOwner: TObject; ADisplay: TfpgwDisplay; AParent:TfpgwWindow; ALeft, ATop, AWidth, AHeight: Integer; APopupFor: TfpgwWindow);
    destructor  Destroy; override;
    procedure Redraw;
    procedure Paint(Buffer: TfpgwBuffer);
    function  NextBuffer: TfpgwBuffer;
    function  IsMaximized: Boolean;
    function  IsMinimized: Boolean;
    procedure SetClientSize(AWidth: Integer; AHeight: Integer);
    property  Display: TfpgwDisplay read FDisplay;
    property  SurfaceShell: TfpgwShellSurfaceCommon read FSurfaceShell;
    property  ClientWidth: Integer read FClientWidth;
    property  ClientHeight: Integer read FClientHeight;
    function  GetHeight: Integer;
    function  GetWidth: Integer;
    property  OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property  OnConfigure: TfpgwShellConfigureEvent read FOnConfigure write FOnConfigure;
    property  OnClose: TNotifyEvent read FOnClose write FOnClose;
    property  Owner: TObject read FOwner;
    property  WindowState: DWord read FWindowState;
  end;

  { TfpgwWindowDecorator }

  TfpgwWindowDecorator = class//(TfpgwWindow)
  private
    FBorderBottom: Integer;
    FBorderLeft: Integer;
    FBorderRight: Integer;
    FBorderTop: Integer;
    FHost: TfpgwWindow;
    //FChildHeight: Integer;
    //FChildWidth: Integer;
    FMousePos: TPoint;
    procedure SetHost(AValue: TfpgwWindow);
    function MouseInDecoratorArea(AX, AY: Integer): Boolean;
  protected
    function  MouseEnter(AX, AY: Integer): Boolean;
    function  MouseLeave: Boolean;
    function  MouseMove(AX, AY: Integer): Boolean;
    function  MouseButton(ASerial: Longword; ATime: Longword; AButton: LongWord; AState: LongWord): Boolean;
  public
    constructor Create(AOwner: TObject; ADisplay: TfpgwDisplay; L, R, T, B: Integer
      );
    property Host: TfpgwWindow read FHost write SetHost;
    property BorderLeft: Integer read FBorderLeft write FBorderLeft;
    property BorderRight: Integer read FBorderRight write FBorderRight;
    property BorderTop: Integer read FBorderTop write FBorderTop;
    property BorderBottom: Integer read FBorderBottom write FBorderBottom;
    //property ChildWidth: Integer read FChildWidth write FChildWidth;
    //property ChildHeight: Integer read FChildHeight write FChildHeight;
  end;

  { TfpgwCursor }

  TfpgwCursor = class
  private
    FSurface: TWlSurface;
    FTheme: Pwl_cursor_theme;
    FDisplay: TfpgwDisplay;
    FCursors: TFPHashList;
    FCurrentCursor: Pwl_cursor;
    FCurrentIndex: Integer;
    procedure CheckSurface;
  public
    constructor Create(ADisplay: TfpgwDisplay; AThemeName: String; ADesiredSize: Integer);
    destructor  Destroy; override;
    procedure SetCursor(ANames: array of String);
    property    Surface: TWlSurface read FSurface;
  end;

const
  // from input-event-codes.h in the kernel
  BTN_LEFT  = $110;
  BTN_RIGHT = $111;
  BTN_MIDDLE = $112;
  BTN_SIDE = $113;
  BTN_EXTRA = $114;
  BTN_FORWARD = $115;
  BTN_BACK = $116;
  BTN_TASK = $117;

implementation
uses
  wayland_client, wayland_shared_buffer, BaseUnix, syscall;

type
  TWaylandAvlNode = class(TAVLTreeNode)
    UserData: Pointer;
  end;

{ TfpgwRegistryEntry }

constructor TfpgwRegistryEntry.Create(AName: DWord; AInterface: String;
  AVersion: DWord);
begin
  FName:=AName;
  FInterface:=AInterface;
  FVersion:=AVersion;
end;

{ TfpgwWindowDecorator }

procedure TfpgwWindowDecorator.SetHost(AValue: TfpgwWindow);
begin
  if FHost=AValue then Exit;
  FHost:=AValue;

  if not Assigned(AValue) then
    Exit;

  FHost.FDecorations := Self;

  //FHost.FTopLevel := Self;

  {AValue.SurfaceShell.SubSurface.SetPosition(BorderLeft, BorderTop);
  AValue.SurfaceShell.SubSurface.SetDesync;}
end;

function TfpgwWindowDecorator.MouseInDecoratorArea(AX, AY: Integer): Boolean;
begin
  Result := (AX < BorderLeft) or (AY < BorderTop)
    or (AY > FHost.GetHeight - BorderBottom)
    or (AX > FHost.GetWidth - BorderRight)
end;

function TfpgwWindowDecorator.MouseEnter(AX, AY: Integer): Boolean;
begin
  Result := MouseInDecoratorArea(AX, AY);
  FMousePos := Point(AX, AY);
 // FDisplay.Cursor.SetCursor('arrow');
end;

function TfpgwWindowDecorator.MouseLeave: Boolean;
begin
  Result := False;//MouseInDecoratorArea(AX, AY);
end;

function TfpgwWindowDecorator.MouseMove(AX, AY: Integer): Boolean;
begin
  Result := MouseInDecoratorArea(AX, AY);
  FMousePos := Point(AX, AY);
  if FMousePos.Y > BorderTop then
  begin
    if FMousePos.Y >= FHost.GetHeight - BorderBottom then
    begin
      FHost.FDisplay.SetCursor(['sb_down_arrow', 'bottom_side']);
    end
    else
    if FMousePos.X <= BorderLeft then
    begin
      FHost.FDisplay.SetCursor(['sb_left_arrow', 'left_side']);
    end
    else
    if FMousePos.X > FHost.GetWidth - BorderRight then
    begin
      FHost.FDisplay.SetCursor(['sb_right_arrow', 'right_side']);
    end;
  end
  else
  begin
    if FMousePos.Y < BorderBottom then
      FHost.FDisplay.SetCursor(['sb_up_arrow', 'top_side'])
    else
      FHost.FDisplay.SetCursor(['left_ptr']);
  end;
end;

function TfpgwWindowDecorator.MouseButton(ASerial: Longword; ATime: Longword;
  AButton: LongWord; AState: LongWord): Boolean;
begin
  Result := MouseInDecoratorArea(FMousePos.X, FMousePos.Y) and (AState = WL_POINTER_BUTTON_STATE_PRESSED);
  if Result then
    FHost.SurfaceShell.Move(ASerial)
end;

constructor TfpgwWindowDecorator.Create(AOwner: TObject;
  ADisplay: TfpgwDisplay; L, R, T, B: Integer);
begin
  BorderLeft:=L;
  BorderRight:=R;
  BorderTop:=T;
  BorderBottom:=B;
  //inherited Create(AOwner, ADisplay, nil, 0,0, AChildWidth+L+R, AChildHeight+T+B, nil);
end;

{ TfpgwSharedPool }

const
  MREMAP_MAYMOVE = 1;
  MREMAP_FIXED = 2;

procedure TfpgwSharedPool.GrowPool(ANewSize: LongWord);
begin
  if not Assigned(FPool) then
  begin
    FPool := TWlShmPool(TWlShmBase(FDisplay.Shm).CreatePool(ANewSize));
    FData := FPool.Data(0);
  end
  else
  begin
    FPool.Reallocate(ANewSize);
    FData := FPool.Data(0);
    //FpFtruncate(FFd, ANewSize);
    //Fdata := Pointer(Do_SysCall(syscall_nr_mremap, TSysParam(FData), TSysParam(FAllocated), TSysParam(ANewSize), TsysParam(MREMAP_MAYMOVE)));
    //FPool;
    //wl_shm_pool_resize(FPool, ANewSize);
  end;
  FAllocated:=ANewSize;
end;

constructor TfpgwSharedPool.Create(ADisplay: TfpgwDisplay);
begin
  FDisplay := ADisplay;
end;

destructor TfpgwSharedPool.Destroy;
begin
  if Assigned(FPool) then
  begin
    FPool.Free;
  end;
  inherited Destroy;
end;

function TfpgwSharedPool.GetBuffer(AWidth, AHeight: Integer; AFormat: DWord;
  out AData: Pointer): TWlBuffer;
var
  lNeededBytes: Integer;
begin
  lNeededBytes:=AWidth*AHeight*4;

  if FAllocated < lNeededBytes then
     GrowPool(lNeededBytes+((AWidth+50)*50*4));

  Result := FPool.CreateBuffer(0, AWidth, AHeight, AWidth*4, AFormat);
  AData:=FData;
end;


{ TfpgwWLShellSurface }

procedure TfpgwWLShellSurface.wl_shell_surface_ping(
  AWlShellSurface: TWlShellSurface; ASerial: DWord);
begin
  AWlShellSurface.Pong(ASerial);
  //writeln('ping');
end;

procedure TfpgwWLShellSurface.wl_shell_surface_configure(
  AWlShellSurface: TWlShellSurface; AEdges: DWord; AWidth: LongInt;
  AHeight: LongInt);
begin
  if Assigned(FWin.FOnConfigure) then
    FWin.FOnConfigure(FWin, AEdges, AWidth, AHeight);
  //WriteLn('Configure ', FWin.ClassNAme);

end;

procedure TfpgwWLShellSurface.wl_shell_surface_popup_done(
  AWlShellSurface: TWlShellSurface);
begin

end;

constructor TfpgwWLShellSurface.Create(ADisplay: TfpgwDisplay; AWin: TfpgwWindow);
begin
  inherited Create(ADisplay, AWin);
end;

destructor TfpgwWLShellSurface.Destroy;
begin
  // subsurface windows don't have a shell surface
  if Assigned(FShellSurface) then
    FShellSurface.Free;
  inherited Destroy;
end;

procedure TfpgwWLShellSurface.SetToplevel;
begin
  FShellSurface:= FDisplay.Shell.GetShellSurface(FSurface);
  FShellSurface.AddListener(Self);
  FShellSurface.SetToplevel;
end;

procedure TfpgwWLShellSurface.SetPopup(AParent: TfpgwWindow; AX, AY: Integer);
begin
  //WriteLn('Setting Popup');
  FShellSurface:= FDisplay.Shell.GetShellSurface(FSurface);
  FShellSurface.AddListener(Self);
  FShellSurface.SetPopup(FDisplay.Seat, FDisplay.NextSerial, AParent.SurfaceShell.Surface, AX, AY, 0);
end;

procedure TfpgwWLShellSurface.SetTitle(AValue: String);
begin
  FShellSurface.SetTitle(AValue);
end;

procedure TfpgwWLShellSurface.SetFullscreen(AValue: Boolean);
begin

  if AValue then
  FShellSurface.SetFullscreen(WL_SHELL_SURFACE_FULLSCREEN_METHOD_DEFAULT, 30, FOutput)
  else
    //wl_shell_surface_set_maximized();
end;

procedure TfpgwWLShellSurface.SetMaximized(AValue: Boolean);
begin
  if AValue then
    FShellSurface.SetMaximized(FOutput);
end;

procedure TfpgwWLShellSurface.SetMinimized;
begin
  // not supported
end;

procedure TfpgwWLShellSurface.Move(Serial: LongWord);
begin
  if Assigned(FShellSurface) then
    FShellSurface.Move(FDisplay.Seat, Serial);
end;

procedure TfpgwWLShellSurface.Resize(ASerial: DWord; AEdges: DWord);
begin
  if Assigned(FShellSurface) then
    FShellSurface.Resize(FDisplay.Seat, ASerial, AEdges);
end;

{ TfpgwXDGShellSurface }

procedure TfpgwXDGShellSurface.xdg_surface_configure(AXdgSurface: TXdgSurface;
  ASerial: DWord);
begin
  //zxdg_surface_v6_set_window_geometry(FXdgSurface, 0,0,FWin.Width,FWin.Height);
  AXdgSurface.AckConfigure(ASerial);
end;

procedure TfpgwXDGShellSurface.xdg_toplevel_configure(
  AXdgToplevel: TXdgToplevel; AWidth: LongInt; AHeight: LongInt;
  AStates: Pwl_array);
var
  lIndex: Integer = 0;
  lValue: DWord;
  lState: DWord = 0;
begin
  Writeln(format('xdg_toplevel_configure %d:%d  States size: %d', [AWidth, AHeight, AStates^.size]));
  while AStates^.GetAsDWord(lIndex, lValue, True) do
  begin
    Writeln(' state: ', lValue);
    lState := lState or (1 shl lValue);
  end;
  FState := lState;
  if Assigned(FWin.OnConfigure) then
    FWin.OnConfigure(Self, 0, AWidth, AHeight);


end;

procedure TfpgwXDGShellSurface.xdg_toplevel_close(AXdgToplevel: TXdgToplevel);
begin
  if Assigned(FWin.OnClose) then
    FWin.OnClose(FWin);
  WriteLn('Close Toplevel');
end;

procedure TfpgwXDGShellSurface.xdg_popup_configure(AXdgPopup: TXdgPopup;
  AX: LongInt; AY: LongInt; AWidth: LongInt; AHeight: LongInt);
begin
  writeln('popup configure');
end;

procedure TfpgwXDGShellSurface.xdg_popup_popup_done(AXdgPopup: TXdgPopup);
begin
  WriteLn('Popup done');
end;

constructor TfpgwXDGShellSurface.Create(ADisplay: TfpgwDisplay;
  AWin: TfpgwWindow);
begin
  inherited Create(ADisplay, AWin);
  FXdgSurface:= FDisplay.FXDGShell.GetXdgSurface(FSurface);
  FXdgSurface.AddListener(Self);
end;

destructor TfpgwXDGShellSurface.Destroy;
begin
  if Assigned(FToplevel) then
    FToplevel.Free;
  if Assigned(FPopup) then
    FPopup.Free;
  FDisplay.Roundtrip;
  FXdgSurface.Free;
  inherited Destroy;
end;

procedure TfpgwXDGShellSurface.SetTitle(AValue: String);
begin
  if Assigned(FToplevel) then
    FToplevel.SetTitle(AValue);
end;

function TfpgwXDGShellSurface.IsMaximized: Boolean;
begin
  Result := FState and (1 shl XDG_TOPLEVEL_STATE_MAXIMIZED) <> 0;
end;

procedure TfpgwXDGShellSurface.SetMaximized(AValue: Boolean);
begin
  if not Assigned(FToplevel) then
    Exit;
  if AValue then
    FToplevel.SetMaximized
  else
    FToplevel.UnsetMaximized;
end;

procedure TfpgwXDGShellSurface.SetMinimized;
begin
  FToplevel.SetMinimized;
end;

procedure TfpgwXDGShellSurface.SetPopup(AParent: TfpgwWindow; AX, AY: Integer);
var
  lPositioner: TXdgPositioner;
  lDecor: TfpgwWindowDecorator absolute AParent;
begin
  lPositioner :=  FDisplay.FXDGShell.CreatePositioner;
  with lPositioner do
  begin
    SetAnchorRect(AX, AY,1,1);
    SetSize(FWin.GetWidth,FWin.GetHeight);
    SetAnchor(XDG_POSITIONER_ANCHOR_TOP or XDG_POSITIONER_ANCHOR_LEFT);
    if Assigned(AParent.FDecorations) then
      SetOffset(AParent.FDecorations.BorderLeft, AParent.FDecorations.BorderTop);
    SetGravity(XDG_POSITIONER_GRAVITY_BOTTOM_RIGHT);
  end;
  FPopup:= FXdgSurface.GetPopup(TfpgwXDGShellSurface(AParent.SurfaceShell).FXdgSurface, lPositioner);
  FPopup.AddListener(Self);
  FSurface.Commit;
  FDisplay.Display.Dispatch;
end;

procedure TfpgwXDGShellSurface.SetToplevel;
begin
  FToplevel :=  FXdgSurface.GetToplevel;
  FToplevel.AddListener(Self);
  FSurface.Commit;
  FDisplay.Display.Dispatch;
end;

procedure TfpgwXDGShellSurface.Move(Serial: LongWord);
begin
  if Assigned(FToplevel) then
    FToplevel.Move(FDisplay.Seat, Serial);
end;

procedure TfpgwXDGShellSurface.Resize(ASerial: DWord; AEdges: DWord);
begin
   if Assigned(FToplevel) then
    FToplevel.Resize(FDisplay.Seat, ASerial, AEdges);
end;

{ TfpgwShellSurfaceCommon }

procedure TfpgwShellSurfaceCommon.wl_surface_enter(AWlSurface: TWlSurface;
  AOutput: TWlOutput);
begin
  WriteLn('surface enter');
  FOutput:=AOutput;
end;

procedure TfpgwShellSurfaceCommon.wl_surface_leave(AWlSurface: TWlSurface;
  AOutput: TWlOutput);
begin
  WriteLn('surface leave');

end;

constructor TfpgwShellSurfaceCommon.Create(ADisplay: TfpgwDisplay;
  AWin: TfpgwWindow);
begin
  FDisplay := ADisplay;
  FWin := AWin;
  FSurface:= FDisplay.Compositor.CreateSurface;
  FSurface.UserData:=FWin;

  FDisplay.AddUserData(FSurface, FWin);

  //WriteLn('Created Win: 0x', HexStr(Pointer(FSurface)));

  FSurface.AddListener(Self);
end;

destructor TfpgwShellSurfaceCommon.Destroy;
begin
  //WriteLn('Removing Surface 0x: ', HexStr(pointer(self)));
  FDisplay.RemoveUserData(FSurface);
  if Assigned(FSubSurface) then
    FSubSurface.Free;

  FSurface.Free;
  inherited Destroy;
end;

procedure TfpgwShellSurfaceCommon.SetOpaqueRegion(ARegion: TRect);
var
  lRegion: TWlRegion;
begin
  lRegion := FDisplay.Compositor.CreateRegion;
  lRegion.Add(ARegion.Left, ARegion.Top, ARegion.Width, ARegion.Height);
  FSurface.SetOpaqueRegion(lRegion);
  Commit;
  lRegion.Free;
end;

procedure TfpgwShellSurfaceCommon.Commit;
begin
  FSurface.Commit;
end;

procedure TfpgwShellSurfaceCommon.SetSubSurface(AParent: TfpgwShellSurfaceCommon);
begin
  FSubSurface := FDisplay.SubCompositor.GetSubsurface(Surface, AParent.Surface);
end;

{ TfpgwCursor }

procedure TfpgwCursor.CheckSurface;
begin
  if Assigned(FSurface) then
    Exit;

  FSurface := FDisplay.Compositor.CreateSurface;
end;

constructor TfpgwCursor.Create(ADisplay: TfpgwDisplay; AThemeName: String;
  ADesiredSize: Integer);
begin
  FDisplay := ADisplay;
  FTheme := wl_cursor_theme_load(PChar(AThemeName), ADesiredSize, FDisplay.Shm.Proxy);
  FCursors := TFPHashList.Create;
end;

destructor TfpgwCursor.Destroy;
begin
  FSurface.Free;
  FCursors.Free;
  inherited Destroy;
end;

procedure TfpgwCursor.SetCursor(ANames: array of String);
var
  lItem: Pointer;
  lCursor: Pwl_cursor;
  lBuffer: TWlBuffer;
  S: String;
begin
  CheckSurface;
  for S in ANames do
  begin
    lItem := FCursors.Find(S);

    if Assigned(lItem) then
    lCursor := lItem
    else
    begin
      lCursor :=  wl_cursor_theme_get_cursor(FTheme, PChar(S));
      if Assigned(lCursor) then
      begin
        FCursors.Add(S, lCursor);
        Break;
      end;
    end;
  end;


  FCurrentCursor:=lCursor;
  FCurrentIndex:=0;

  if FCurrentCursor = nil then
    Exit;

  FDisplay.Mouse.SetCursor(FDisplay.NextSerial, FSurface, FCurrentCursor^.images[FCurrentIndex]^.hotspot_x,FCurrentCursor^.images[FCurrentIndex]^.hotspot_y);

  lBuffer :=  TWlBuffer.Create(wl_cursor_image_get_buffer(FCurrentCursor^.images[FCurrentIndex]));

  FSurface.Attach(lBuffer, 0,0);
  FSurface.Commit;
end;

{ TfpgwBuffer }

procedure TfpgwBuffer.wl_buffer_release(AWlBuffer: TWlBuffer);
begin
  FBusy:=False;
  FNext := nil;
end;

constructor TfpgwBuffer.Create(ADisplay: TfpgwDisplay);
begin
  FDisplay := ADisplay;
  FPool := TfpgwSharedPool.Create(FDisplay);
end;

procedure TfpgwBuffer.FreeBuffer;
begin
  if Assigned(FBuffer) then
  begin
    FreeAndNil(FBuffer);
  end;
end;

function TfpgwBuffer.GetAllocated(AWidth, AHeight: Integer): Boolean;
begin
  Result := FBuffer <> nil;
  if not Result then
    Exit;

  Result := (AWidth = FWidth) and (AHeight = FHeight);
end;

function TfpgwBuffer.GetStride: Integer;
begin
  Result := FWidth *4;
end;

destructor TfpgwBuffer.Destroy;
begin
  FPool.Free;
end;

procedure TfpgwBuffer.SetPaintRect(AX, AY, AWidth, Aheight: Integer);
begin
  FRect.Left:=AX;
  Frect.Top:=AY;
  FRect.Width:=AWidth;
  FRect.Height:=Aheight;
end;

procedure TfpgwBuffer.Allocate(AWidth, AHeight: Integer; AFormat: LongWord);
begin
  if (AWidth = FWidth) and (AHeight = FHeight) then
    Exit;
  FreeBuffer;

  FWidth:=AWidth;
  FHeight:=AHeight;
  // the pool gets some extra data so resizes are fast. It will grow as needed
  FBuffer := FPool.GetBuffer(FWidth, FHeight, AFormat, FData);
  FBuffer.AddListener(Self);

  // unneeded
  FillDWord(FData^, FWidth * FHeight, $FEFEFEFE);
end;

{ TfpgwWindow }

procedure TfpgwWindow.wl_callback_done(AWlCallback: TWlCallback;
  ACallbackData: DWord);
var
  buffer: TfpgwBuffer;
  lCallback: TWlCallback;
begin
  if Assigned(AWlCallback) then
    AWlCallback.Free;

  FServerReadyToPaint:=True;
  //WriteLn('server ready for paint');
  if FReadyBuffer <> nil then
  begin
    Paint(FReadyBuffer);

  end
  //else if Assigned(FOnPaint) then
  //  FOnPaint(Self)
  else
  begin
    exit;
    FServerReadyToPaint := False;
    // default paint...not very useful
    buffer := NextBuffer;
    buffer.FBusy:=True;
   // FillDWord(buffer.Data^, Width*20, $00FF0000);

    SurfaceShell.Surface.Attach(buffer.FBuffer, 0, 0);
    SurfaceShell.Surface.Damage(20, 20, GetWidth - 40, getHeight - 40);

    lCallback := SurfaceShell.Surface.Frame();
    lCallback.AddListener(Self);
    SurfaceShell.Surface.Commit;
  end;
end;

function TfpgwWindow.NextBuffer: TfpgwBuffer;
begin
  Result := nil;
  if not FBuffers[0].Busy then
    Result := FBuffers[0]
  else if not FBuffers[1].Busy then
    Result := FBuffers[1]
  else
    Exit;

  if not Result.Allocated[GetWidth, GetHeight] then
  begin
    //WriteLn(Format('Allocate buffer: %d:%d', [GetWidth,GetHeight]));
    Result.Allocate(GetWidth, GetHeight, WL_SHM_FORMAT_ARGB8888);
  end;

  // useful for debugging
  //FillDWord(Result.Data^, Width*Height, $0000ff00);
end;

function TfpgwWindow.IsMaximized: Boolean;
begin

end;

function TfpgwWindow.IsMinimized: Boolean;
begin
  Result := False;
  //Result := Result := WindowState and 1 shl ZXDG_TOPLEVEL_V6_STATE_MAXIMIZED ;
end;

procedure TfpgwWindow.SetClientSize(AWidth: Integer; AHeight: Integer);
begin
  FClientWidth:=AWidth;
  FClientHeight:=AHeight;
end;

function TfpgwWindow.GetHeight: Integer;
begin
  Result := FClientHeight;
  if Assigned(FDecorations) then
    Result += FDecorations.BorderTop + FDecorations.BorderBottom;
end;

function TfpgwWindow.GetWidth: Integer;
begin
  Result := FClientWidth;
  if Assigned(FDecorations) then
    Result += FDecorations.BorderLeft + FDecorations.BorderRight;
end;

constructor TfpgwWindow.Create(AOwner: TObject; ADisplay: TfpgwDisplay;
  AParent: TfpgwWindow; ALeft, ATop, AWidth, AHeight: Integer;
  APopupFor: TfpgwWindow);
var
  lParentSurface: TfpgwShellSurfaceCommon = nil;
begin
  //FTopLevel := Self;
  FOwner := AOwner;
  FDisplay := ADisplay;
  FClientWidth:=AWidth;
  FClientHeight:=AHeight;

  if Assigned(AParent) then
    lParentSurface := AParent.SurfaceShell;

  if Assigned(lParentSurface) then
  begin
    // child surfaces are not xdg or ivi
    FSurfaceShell := TfpgwWLShellSurface.Create(FDisplay, Self);
    SurfaceShell.SetSubSurface(lParentSurface);
  end
  else
  begin
    // create the shell as the prefered parentless class
    FSurfaceShell := FDisplay.FSurfaceClass.Create(FDisplay, Self);

    if not Assigned(APopupFor) then
      SurfaceShell.SetToplevel
    else
      SurfaceShell.SetPopup(APopupFor, ALeft, ATop);
  end;

  FBuffers[0] := TfpgwBuffer.Create(FDisplay);
  FBuffers[1] := TfpgwBuffer.Create(FDisplay);

  SurfaceShell.Surface.Damage(0 ,0, AWidth, AHeight);
  FServerReadyToPaint:=True;
  ADisplay.Roundtrip;
end;

destructor TfpgwWindow.Destroy;
begin
  FDisplay.RemoveUserData(FSurfaceShell.Surface);
  FSurfaceShell.Free;

  FBuffers[0].Free;
  FBuffers[1].Free;
end;

procedure TfpgwWindow.Redraw;
begin
  wl_callback_done(nil, 0);
  FOnPaint(Self);
end;

procedure TfpgwWindow.Paint(Buffer: TfpgwBuffer);
     procedure DrawPaintBorder;
     var
       i, j: Integer;
       c: LongWord;
     begin
       c := Random($00ffffff);
       with Buffer.PaintArea do
       for i :=  Left to Right-1 do begin
         for j := Top to Bottom-1 do
           if (i = Left) or ( i = Right-1) or (j = Top) or (j = Bottom-1) then
         PDword(Buffer.Data)[(j)*Buffer.Width+i] := c;
       end;
     end;

var
  lCallback: TWlCallback;
begin
  if Buffer = nil then
    Exit;
  if FServerReadyToPaint then
  begin
    //WriteLn(Format('Actually painting: %d, %d, %d, %d', [Buffer.PaintArea.Left,Buffer.PaintArea.Top, Buffer.PaintArea.Width, Buffer.PaintArea.Height]));
    //DrawPaintBorder;
    with buffer.PaintArea do
    begin

      SurfaceShell.Surface.Damage(Left, Top, Width, Height);
      //wl_surface_damage_buffer(FSurface, Left, Top, Width, Height);
    end;

    SurfaceShell.Surface.Attach(Buffer.FBuffer, 0, 0);
    buffer.FBusy:=True;
    FReadyBuffer := buffer.Next;
    buffer.Next := nil;

    lCallback := SurfaceShell.Surface.Frame();
    lCallback.AddListener(Self);
    SurfaceShell.Surface.Commit;
    FServerReadyToPaint := False;
    FDisplay.Display.DispatchPending;
  end
  else
  begin
    if (FReadyBuffer <> nil) and (FReadyBuffer <> Buffer) then
      FReadyBuffer.Next := Buffer
    else
      FReadyBuffer := buffer;
    Buffer.Busy:=True;
  end;
end;

{ TfpgwCallbackHelper }

procedure TfpgwCallbackHelper.wl_callback_done(AWlCallback: TWlCallback;
  ACallbackData: DWord);
begin
  FCallback:=AWlCallback;
  if Assigned(FNotify) then
    FNotify(Self);
end;

constructor TfpgwCallbackHelper.Create(ADisplay: TfpgwDisplay; ANotify: TNotifyEvent);
begin
  FNotify:=ANotify;
end;

{ TfpgwDisplay }

function TfpgwDisplay.GetConnected: Boolean;
begin
  Result := FDisplay <> nil;
end;

procedure TfpgwDisplay.wl_registry_global(AWlRegistry: TWlRegistry;
  AName: DWord; AInterface: String; AVersion: DWord);
begin
  WriteLn(AInterface, ' v ', AVersion);

  FRegList.Add(TfpgwRegistryEntry.Create(AName, AInterface, AVersion));

  case String(AInterface) of
    'wl_compositor': FCompositor:= TWlCompositor.Create(AWlRegistry.Bind(AName, @wl_compositor_interface, 1));
    'wl_subcompositor': FSubcompositor := TWlSubcompositor.Create(AWlRegistry.Bind(AName, @wl_subcompositor_interface, 1));
    'wl_shell'     :
      begin
        FShell:= TWlShell.Create(AWlRegistry.Bind(AName, @wl_shell_interface, 1));
        if not Assigned(FSurfaceClass) then
          FSurfaceClass:=TfpgwWLShellSurface;
      end;
    'wl_shm'       :
      begin
        FShm:= TWlShm.Create(AWlRegistry.Bind(AName, @wl_shm_interface, 1));
        FShm.AddListener(Self);
        FCursor := TfpgwCursor.Create(Self, '', 32 {shrug});
      end;
    'wl_seat':
      begin
        FSeat := TWlSeat.Create(AWlRegistry.Bind(AName, @wl_seat_interface, 1));
        FSeat.AddListener(Self);
        FMouse := FSeat.GetPointer;
        if Assigned(FMouse) then
          FMouse.AddListener(Self);

        FKeyboard := FSeat.GetKeyboard;
        if Assigned(FKeyboard) then
          FKeyboard.AddListener(Self);
      end;
    'xdg_wm_base':
      begin
        FXDGShell := TXdgWmBase.Create(AWlRegistry.Bind(AName, @xdg_wm_base_interface, 1));
        FXDGShell.AddListener(Self);
        // we prefer xdg surfaces. perhaps ivi in the future...
        FSurfaceClass:=TfpgwXDGShellSurface;
      end;
    'zxdg_decoration_manager_v1':
      begin
        FDecorationManager:= TZxdgDecorationManagerV1.Create(AWlRegistry.Bind(AName, @zxdg_decoration_manager_v1_interface, 1));
        FSupportsServerSideDecorations := True;
      end
    else
      ;//WriteLn(&interface, ' v ', version);
  end;
end;

procedure TfpgwDisplay.wl_registry_global_remove(AWlRegistry: TWlRegistry;
  AName: DWord);
begin

end;

procedure TfpgwDisplay.wl_shm_format(AWlShm: TWlShm; AFormat: DWord);
begin
  // supported pixel formats WL_SHM_FORMAT_xxxxx;
  FFormats:=FFormats or (1 shl AFormat);
end;

procedure TfpgwDisplay.wl_seat_capabilities(AWlSeat: TWlSeat;
  ACapabilities: DWord);
begin
  //WL_SEAT_CAPABILITY_KEYBOARD;
  //WL_SEAT_CAPABILITY_POINTER;
  //WL_SEAT_CAPABILITY_TOUCH;
  FCapabilities:=ACapabilities;
end;

procedure TfpgwDisplay.wl_seat_name(AWlSeat: TWlSeat; AName: String);
begin

end;

procedure TfpgwDisplay.wl_pointer_enter(AWlPointer: TWlPointer; ASerial: DWord;
  ASurface: TWlSurface; ASurfaceX: Longint; ASurfaceY: Longint);
var
  lWin: TfpgwWindow;
  lDecor: TfpgwWindowDecorator absolute lWin;
begin
  FEventSerial:=ASerial;
  lWin := TfpgwWindow(GetUserData(ASurface));

  if not Assigned(lWin) then
    lWin := TfpgwWindow(ASurface.UserData);
  if not Assigned(lWin) then
    Raise Exception.CreateFmt('pointer enter for unknown surface 0x%s', [HexStr(Pointer(ASurface))]);
  FActiveMouseWin := lWin;

  if Assigned(lWin.FDecorations)
  and not (lWin.FDecorations.MouseEnter(ASurfaceX shr 8, ASurfaceY shr 8))
  then
  begin
    if Assigned(FOnMouseEnter) then
      FOnMouseEnter(lWin.Owner, ASurfaceX shr 8, ASurfaceY shr 8);
    lWin.FEntered:= True;
  end;
end;

procedure TfpgwDisplay.wl_pointer_leave(AWlPointer: TWlPointer; ASerial: DWord;
  ASurface: TWlSurface);
var
  lWin: TfpgwWindow;
  lDecor: TfpgwWindowDecorator absolute lWin;
begin
  FEventSerial:=ASerial;
  lWin := TfpgwWindow(GetUserData(ASurface));

  if not Assigned(lWin) then
  begin
    FActiveMouseWin := nil;
    Exit;
  end;

  if Assigned(lWin.FDecorations) then
    lWin.FDecorations.MouseLeave;

  if Assigned(FOnMouseLeave) and Assigned(lWin) then
  begin
    lWin.FEntered:=False;
    FOnMouseLeave(lWin.Owner);
  end;

  FActiveMouseWin := nil;
end;

procedure TfpgwDisplay.wl_pointer_motion(AWlPointer: TWlPointer; ATime: DWord;
  ASurfaceX: Longint; ASurfaceY: Longint);
var
  lHandled: Boolean = False;
begin
  if Assigned(FActiveMouseWin) and Assigned(FActiveMouseWin.FDecorations) then
  begin
    lHandled:=FActiveMouseWin.FDecorations.MouseMove(ASurfaceX shr 8, ASurfaceY shr 8);
  end;

  if {not lHandled and} Assigned(FOnMouseMotion) then
  begin
    if not FActiveMouseWin.FEntered then
    begin
      FActiveMouseWin.FEntered := True;
      if Assigned(FOnMouseEnter) then
        FOnMouseEnter(FActiveMouseWin.Owner, ASurfaceX shr 8, ASurfaceY shr 8);
    end;


    FOnMouseMotion(FActiveMouseWin.Owner, ATime, ASurfaceX shr 8, ASurfaceY shr 8);
  end;
end;

procedure TfpgwDisplay.wl_pointer_button(AWlPointer: TWlPointer;
  ASerial: DWord; ATime: DWord; AButton: DWord; AState: DWord);
var
  lHandled: Boolean = False;
begin
  FEventSerial:=ASerial;
  {if Assigned(FActiveMouseWin) and Assigned(FActiveMouseWin.FDecorations) then
  begin
    lHandled:=FActiveMouseWin.FDecorations.MouseButton(ASerial, ATime, AButton, AState);
  end;}
  if {not lHandled and} Assigned(FOnMouseButton) then
    FOnMouseButton(FActiveMouseWin.Owner, ATime, AButton, AState);
end;

procedure TfpgwDisplay.wl_pointer_axis(AWlPointer: TWlPointer; ATime: DWord;
  AAxis: DWord; AValue: Longint);
begin
   if Assigned(FOnMouseAxis) then
    FOnMouseAxis(FActiveMouseWin.Owner, ATime, AAxis, AValue);
end;

procedure TfpgwDisplay.wl_pointer_frame(AWlPointer: TWlPointer);
begin

end;

procedure TfpgwDisplay.wl_pointer_axis_source(AWlPointer: TWlPointer;
  AAxisSource: DWord);
begin

end;

procedure TfpgwDisplay.wl_pointer_axis_stop(AWlPointer: TWlPointer;
  ATime: DWord; AAxis: DWord);
begin

end;

procedure TfpgwDisplay.wl_pointer_axis_discrete(AWlPointer: TWlPointer;
  AAxis: DWord; ADiscrete: LongInt);
begin

end;

procedure TfpgwDisplay.wl_keyboard_keymap(AWlKeyboard: TWlKeyboard;
  AFormat: DWord; AFd: LongInt; ASize: DWord);
begin
  Writeln('keymap');
  if Assigned(FOnKeyboardKeymap) then
    FOnKeyboardKeymap(Owner,AFormat,AFd,ASize);
end;

procedure TfpgwDisplay.wl_keyboard_enter(AWlKeyboard: TWlKeyboard;
  ASerial: DWord; ASurface: TWlSurface; AKeys: Pwl_array);
begin
  FEventSerial:=ASerial;
  FActiveKeyboardWin := TfpgwWindow(GetUserData(ASurface));
  if Assigned(FOnKeyboardEnter) and Assigned(FActiveKeyboardWin) then
    FOnKeyboardEnter(FActiveKeyboardWin.Owner,AKeys);
end;

procedure TfpgwDisplay.wl_keyboard_leave(AWlKeyboard: TWlKeyboard;
  ASerial: DWord; ASurface: TWlSurface);
var
  lWin: TfpgwWindow;
begin
  FEventSerial:=ASerial;
  lWin := TfpgwWindow(GetUserData(ASurface));
  if Assigned(FOnKeyboardLeave) and Assigned(lWin) then
    FOnKeyboardLeave(lWin.Owner);
  FActiveKeyboardWin := nil;
end;

procedure TfpgwDisplay.wl_keyboard_key(AWlKeyboard: TWlKeyboard;
  ASerial: DWord; ATime: DWord; AKey: DWord; AState: DWord);
begin
  FEventSerial:=ASerial;
  if Assigned(FOnKeyboardKey) then
    FOnKeyboardKey(FActiveKeyboardWin.Owner,ATime,AKey,AState);
end;

procedure TfpgwDisplay.wl_keyboard_modifiers(AWlKeyboard: TWlKeyboard;
  ASerial: DWord; AModsDepressed: DWord; AModsLatched: DWord;
  AModsLocked: DWord; AGroup: DWord);
begin
  FEventSerial:=ASerial;
  Writeln('modifiers');
  if Assigned(FOnKeyboardModifiers) then
    FOnKeyboardModifiers(Owner,AModsDepressed,AModsLatched, AModsLocked, AGroup);
end;

procedure TfpgwDisplay.wl_keyboard_repeat_info(AWlKeyboard: TWlKeyboard;
  ARate: LongInt; ADelay: LongInt);
begin
  if Assigned(FOnKeyBoardRepeatInfo) then
    FOnKeyBoardRepeatInfo(Owner,ARate,ADelay);
end;

procedure TfpgwDisplay.xdg_wm_base_ping(AXdgWmBase: TXdgWmBase; ASerial: DWord);
begin
  FEventSerial:=ASerial;
  FXDGShell.Pong(ASerial);
end;

function TfpgwDisplay.NextSerial: DWord;
begin
  Result := FSerial;
  Inc(FSerial);
end;

class function TfpgwDisplay.TryCreate(AOwner: TObject; AName: String): TfpgwDisplay;
begin
  Result := TfpgwDisplay.Create(AOwner, AName);
  if not Result.Connected then
  begin
    Result.Free;
    Result := nil;
  end;
end;

constructor TfpgwDisplay.Create(AOwner: TObject; AName: String);
begin
  FOwner := AOwner;
  FRegList := TfpgwRegistryList.Create(True);

  FDisplay := TWlDisplay(TWlDisplay.Connect(AName));
  if not Connected then
    Exit; // ==>

  FRegistry:= FDisplay.GetRegistry;
  FRegistry.AddListener(Self);

  FUserDataList := TAVLTree.Create;
  FUserDataList.NodeClass:=TWaylandAvlNode;

end;

destructor TfpgwDisplay.Destroy;
begin

  inherited Destroy;
  FRegList.Free;
  //wl_display_flush(FDisplay);
  //wl_pointer_release(FMouse);
 // wl_keyboard_release(FKeyboard);

 { if Assigned(FShell) then wl_shell_destroy(FShell);
  if Assigned(FXDGShell) then zxdg_shell_v6_destroy(FXDGShell);
  if Assigned(FSeat) then wl_seat_release(FSeat);
  if Assigned(FShm) then wl_shm_destroy(FShm);
  if Assigned(FCompositor) then wl_compositor_destroy(FCompositor);
  if Assigned(FSubcompositor) then wl_subcompositor_destroy(FSubcompositor);
  if Assigned(FRegistry) then wl_registry_destroy(FRegistry);
  if Assigned(FQueue) then wl_event_queue_destroy(FQueue);}

  if Connected then
  begin
    FDisplay.Flush;
    if Assigned(FDisplay) then
      FreeAndNil(FDisplay);
    FUserDataList.Free;
  end;
end;

procedure TfpgwDisplay.AfterCreate;
begin
  FDisplay.Roundtrip;
  FDisplay.Roundtrip;

  Ffd := FDisplay.GetFd;
  FQueue:=FDisplay.CreateQueue;
end;

procedure TfpgwDisplay.Flush;
begin
  FDisplay.Flush;
end;

procedure TfpgwDisplay.Roundtrip;
begin
  FDisplay.Roundtrip;
end;

procedure TfpgwDisplay.AddUserData(ALookup: Pointer; AData: TObject);
var
  lNode: TWaylandAvlNode;
begin
  lNode := TWaylandAvlNode(FUserDataList.NewNode);
  lnode.Data:=ALookup;
  lNode.UserData:=AData;

  FUserDataList.Add(TAVLTreeNode(lNode));
end;

function TfpgwDisplay.GetUserData(ALookup: Pointer): Pointer;
var
  lNode: TWaylandAvlNode;
begin
  Result := nil;
  lNode := TWaylandAvlNode(FUserDataList.Find(ALookup));
  if Assigned(lNode) then
    Result := lNode.UserData;
end;

procedure TfpgwDisplay.RemoveUserData(Alookup: Pointer);
begin
  FUserDataList.Remove(Alookup);
end;

function TfpgwDisplay.HasEvent(ATimeout: Integer = 0; AWillRead: Boolean = False): Boolean;
var
  r: cint;
  poll: pollfd;
begin
  while FDisplay.PrepareRead <> 0 do
  begin
    FDisplay.DispatchPending;
  end;

  try
    poll.fd:=Ffd;
    poll.events:=POLLIN;
    poll.revents:=POLLIN;

    FDisplay.Flush;
        r := FpPoll(@poll, 1, ATimeOut);
    Result := r > 0;
  finally
    if not AWillRead then
      FDisplay.CancelRead;
  end;

end;

procedure TfpgwDisplay.WaitEvent(ATimeOut: Integer);
begin

  if HasEvent(ATimeOut, True) then
  begin
    //WriteLn('Data to be read');
    FDisplay.ReadEvents;
    FDisplay.DispatchPending;
  end
  else
    FDisplay.CancelRead;
end;

procedure TfpgwDisplay.SetCursor(ACursors: array of String);
begin
  FCursor.SetCursor(ACursors);
end;



end.

