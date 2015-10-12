{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2015 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      The base widget, which all GUI widgets inherit from.
}

unit fpg_widget;

{$mode objfpc}{$H+}

{.$Define GDEBUG}
{.$Define CStackDebug}

interface

uses
  Classes,
  SysUtils,
  fpg_main,
  fpg_base;

type
  TFocusSearchDirection = (fsdFirst, fsdLast, fsdNext, fsdPrev);

  THintEvent = procedure(Sender: TObject; var AHint: TfpgString) of object;

  TfpgDragEnterEvent = procedure(Sender, Source: TObject; AMimeList: TStringList; var AMimeChoice: TfpgString; var ADropAction: TfpgDropAction; var Accept: Boolean) of object;
  TfpgDragDropEvent = procedure(Sender, Source: TObject; X, Y: integer; AData: variant) of object;


  TfpgWidget = class(TfpgWidgetBase)
  private
    FAcceptDrops: boolean;
    FAlignRect: TfpgRect;
    FInvalidated: Boolean;
    FOnClick: TNotifyEvent;
    FOnDoubleClick: TMouseButtonEvent;
    FOnDragDrop: TfpgDragDropEvent;
    FOnDragEnter: TfpgDragEnterEvent;
    FOnDragLeave: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnKeyChar: TfpgKeyCharEvent;
    FOnMouseDown: TMouseButtonEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseExit: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseButtonEvent;
    FOnMouseScroll: TMouseWheelEvent;
    FOnMouseHorizScroll: TMouseWheelEvent;
    FOnPaint: TPaintEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnResize: TNotifyEvent;
    FOnScreen: boolean;
    FOnShowHint: THintEvent;
    alist: TList;
    procedure   SetActiveWidget(const AValue: TfpgWidget);
    function    IsShowHintStored: boolean;
    procedure   SetFormDesigner(const AValue: TObject);
    procedure   SetAlign(const AValue: TAlign);
  protected
    function    GetWindow: TfpgNativeWindow; reintroduce;
    procedure   MsgPaint(var msg: TfpgMessageRec); message FPGM_PAINT;
    procedure   MsgResize(var msg: TfpgMessageRec); message FPGM_RESIZE;
    procedure   MsgMove(var msg: TfpgMessageRec); message FPGM_MOVE;
    procedure   MsgKeyChar(var msg: TfpgMessageRec); message FPGM_KEYCHAR;
    procedure   MsgKeyPress(var msg: TfpgMessageRec); message FPGM_KEYPRESS;
    procedure   MsgKeyRelease(var msg: TfpgMessageRec); message FPGM_KEYRELEASE;
    procedure   MsgMouseDown(var msg: TfpgMessageRec); message FPGM_MOUSEDOWN;
    procedure   MsgMouseUp(var msg: TfpgMessageRec); message FPGM_MOUSEUP;
    procedure   MsgMouseMove(var msg: TfpgMessageRec); message FPGM_MOUSEMOVE;
    procedure   MsgDoubleClick(var msg: TfpgMessageRec); message FPGM_DOUBLECLICK;
    procedure   MsgMouseEnter(var msg: TfpgMessageRec); message FPGM_MOUSEENTER;
    procedure   MsgMouseExit(var msg: TfpgMessageRec); message FPGM_MOUSEEXIT;
    procedure   MsgMouseScroll(var msg: TfpgMessageRec); message FPGM_SCROLL;
    procedure   MsgMouseHorizScroll(var msg: TfpgMessageRec); message FPGM_HSCROLL;
    procedure   MsgDropEnter(var msg: TfpgMessageRec); message FPGM_DROPENTER;
    procedure   MsgDropExit(var msg: TfpgMessageRec); message FPGM_DROPEXIT;
  protected
    FDragStartPos: TfpgPoint;
    FFormDesigner: TObject;
    FVisible: boolean;
    FEnabled: boolean;
    FFocusable: boolean;
    FFocused: boolean;
    FTabOrder: integer;
    FAnchors: TAnchors;
    FActiveWidget: TfpgWidget;
    FAlign: TAlign;
    FHint: TfpgString;
    FInvalidRect: TfpgRect;
    FShowHint: boolean;
    FParentShowHint: boolean;
    FBackgroundColor: TfpgColor;
    FTextColor: TfpgColor;
    FIsContainer: Boolean;
    FOnClickPending: Boolean;
    FIgnoreDblClicks: Boolean;
    procedure   DoAllocateWindowHandle; override;
    procedure   AllocateWindowHandle; virtual;
    procedure   SetAcceptDrops(const AValue: boolean); virtual;
    function    GetOnShowHint: THintEvent; virtual;
    procedure   SetOnShowHint(const AValue: THintEvent); virtual;
    procedure   SetBackgroundColor(const AValue: TfpgColor); virtual;
    procedure   SetTextColor(const AValue: TfpgColor); virtual;
    function    GetParent: TfpgWidget; reintroduce;
    procedure   SetParent(const AValue: TfpgWidget); reintroduce;
    procedure   SetEnabled(const AValue: boolean); virtual;
    procedure   SetVisible(const AValue: boolean); virtual;
    procedure   SetShowHint(const AValue: boolean); virtual;
    procedure   SetParentShowHint(const AValue: boolean); virtual;
    function    GetHint: TfpgString; virtual;
    procedure   SetHint(const AValue: TfpgString); virtual;
    function    GetCanvas: TfpgCanvas; reintroduce;
    function    CreateCanvas: TfpgCanvasBase; virtual;
    procedure   DoUpdatePosition; override;
    procedure   DoAlignment;
    procedure   DoResize;
    procedure   DoShowHint(var AHint: TfpgString);
    procedure   DoKeyShortcut(const AOrigin: TfpgWidget; const keycode: word; const shiftstate: TShiftState; var consumed: boolean; const IsChildOfOrigin: boolean = False); virtual;
    procedure   HandlePaint; virtual;
    procedure   HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: boolean); virtual;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); virtual;
    procedure   HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); virtual;
    procedure   HandleSetFocus; virtual;
    procedure   HandleKillFocus; virtual;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); virtual;
    procedure   HandleRMouseDown(x, y: integer; shiftstate: TShiftState); virtual;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); virtual;
    procedure   HandleRMouseUp(x, y: integer; shiftstate: TShiftState); virtual;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); virtual;
    procedure   HandleDoubleClick(x, y: integer; button: word; shiftstate: TShiftState); virtual;
    procedure   HandleMouseEnter; virtual;
    procedure   HandleMouseExit; virtual;
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); virtual;
    function    Invalidated: Boolean;
    function    IsHidden: Boolean; virtual;
    procedure   HandleMouseHorizScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); virtual;
    function    FindFocusWidget(startwg: TfpgWidget; direction: TFocusSearchDirection): TfpgWidget;
    procedure   HandleAlignments(const dwidth, dheight: TfpgCoord); virtual;
    procedure   HandleShow; virtual;
    procedure   InternalHandleShow; virtual;
    procedure   HandleHide; virtual;
    procedure   MoveAndResize(ALeft, ATop, AWidth, AHeight: TfpgCoord);
    procedure   RePaint; virtual;
    { property events }
    property    OnClick: TNotifyEvent read FOnClick write FOnClick;
    property    OnDoubleClick: TMouseButtonEvent read FOnDoubleClick write FOnDoubleClick;
    property    OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property    OnExit: TNotifyEvent read FOnExit write FOnExit;
    property    OnKeyChar: TfpgKeyCharEvent read FOnKeyChar write FOnKeyChar;
    property    OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property    OnMouseDown: TMouseButtonEvent read FOnMouseDown write FOnMouseDown;
    property    OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property    OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
    property    OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property    OnMouseUp: TMouseButtonEvent read FOnMouseUp write FOnMouseUp;
    property    OnMouseScroll: TMouseWheelEvent read FOnMouseScroll write FOnMouseScroll;
    property    OnMouseHorizScroll: TMouseWheelEvent read FOnMouseHorizScroll write FOnMouseHorizScroll;
    property    OnPaint: TPaintEvent read FOnPaint write FOnPaint;
    property    OnResize: TNotifyEvent read FOnResize write FOnResize;
    property    OnShowHint: THintEvent read GetOnShowHint write SetOnShowHint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterConstruction; override;
    function    InDesigner: boolean;
    function    IsLoading: boolean;
    procedure   InvokeHelp; virtual;
    procedure   Realign;
    procedure   SetFocus;
    procedure   KillFocus;
    procedure   MoveAndResizeBy(const dx, dy, dw, dh: TfpgCoord);
    procedure   SetPosition(aleft, atop, awidth, aheight: TfpgCoord); virtual;
    procedure   Invalidate; // double check this works as developers expect????
    procedure   InvalidateRect(ARect: TfpgRect);
    property    Window: TfpgNativeWindow read GetWindow;
    property    FormDesigner: TObject read FFormDesigner write SetFormDesigner;
    property    Parent: TfpgWidget read GetParent write SetParent;
    property    AcceptDrops: boolean read FAcceptDrops write SetAcceptDrops default False;
    property    ActiveWidget: TfpgWidget read FActiveWidget write SetActiveWidget;
    property    IsContainer: Boolean read FIsContainer;
    property    Canvas: TfpgCanvas read GetCanvas;
    property    Visible: boolean read FVisible write SetVisible default True;
    property    Enabled: boolean read FEnabled write SetEnabled default True;
    property    TabOrder: integer read FTabOrder write FTabOrder;
    { Is the widget allowed to receive keyboard focus. }
    property    Focusable: boolean read FFocusable write FFocusable default False;
    property    Focused: boolean read FFocused write FFocused default False;
    property    Anchors: TAnchors read FAnchors write FAnchors default [anLeft, anTop];
    property    Align: TAlign read FAlign write SetAlign default alNone;
    property    Hint: TfpgString read GetHint write SetHint;
    property    IgnoreDblClicks: Boolean read FIgnoreDblClicks write FIgnoreDblClicks default False;
    property    ShowHint: boolean read FShowHint write SetShowHint stored IsShowHintStored;
    property    ParentShowHint: boolean read FParentShowHint write SetParentShowHint default True;
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor default clWindowBackground;
    property    TextColor: TfpgColor read FTextColor write SetTextColor default clText1;
    property    OnDragEnter: TfpgDragEnterEvent read FOnDragEnter write FOnDragEnter;
    property    OnDragLeave: TNotifyEvent read FOnDragLeave write FOnDragLeave;
    property    OnDragDrop: TfpgDragDropEvent read FOnDragDrop write FOnDragDrop;
  end;


var
  FocusRootWidget: TfpgWidget;


function FindKeyboardFocus: TfpgWidget;

implementation

uses
  fpg_constants,
  fpg_menu,
  fpg_form,   { for OnKeyPress handling }
  fpg_window; { for Finding the Toplevel Window }


var
  uLastClickWidget: TfpgWidget;
  uLastClickPoint: TPoint;
  uLastClickTime: DWord;
  uMouseDownSourceWidget: TfpgWidget; { widget Left MButton was pressed on }


type
  TfpgFormFriend = class(TfpgBaseForm)
  end;

  TfpgWindowHack = class(TfpgWindow)
  end;

function FindKeyboardFocus: TfpgWidget;
begin
  Result := nil;

  if FocusRootWidget <> nil then
  begin
    Result := FocusRootWidget;
    while (Result <> nil) and (Result.ActiveWidget <> nil) do
      Result := Result.ActiveWidget;
  end;
end;

function CompareInts(i1, i2: integer): integer;
begin
  if i1 < i2 then
    Result := -1
  else if i1 > i2 then
    Result := 1
  else
    Result := 0;
end;

function AlignCompare(p1, p2: Pointer): integer;
var
  w1: TfpgWidget;
  w2: TfpgWidget;
begin
  w1 := TfpgWidget(p1);
  w2 := TfpgWidget(p2);
  case w1.Align of
    alTop:    Result := CompareInts(w1.Top, w2.Top);
    alBottom: Result := CompareInts(w2.Top, w1.Top);
    alLeft:   Result := CompareInts(w1.Left, w2.Left);
    alRight:  Result := CompareInts(w2.Left, w1.Left);
    else
      Result         := 0;
  end;
end;


// finds the window that is a widget. not a NativeWindow
function FindTopLevelWindow(AWidget: TfpgWidgetBase): TfpgWindow;
var
  w: TfpgWidgetBase;
begin
  Result := nil;
  if AWidget = nil then
    exit; // ==>
  w := AWidget;
  while Assigned(w) do
  begin
    if w is TfpgWindow then
      Exit(TfpgWindow(w)); // ==>
    w := w.Parent;
  end;
end;

{ TfpgWidget }

procedure TfpgWidget.SetEnabled(const AValue: boolean);
var
  i: integer;
begin
  if FEnabled = AValue then
    Exit; //==>
  FEnabled := AValue;
  for i := 0 to ComponentCount - 1 do
  begin
    if Components[i] is TfpgWidget then
      TfpgWidget(Components[i]).Enabled := FEnabled;
  end;
  RePaint;
end;

procedure TfpgWidget.SetActiveWidget(const AValue: TfpgWidget);
begin
  if FActiveWidget = AValue then
    Exit; //==>
  if InDesigner then
    Exit; //==>
  
  try
    if FActiveWidget <> nil then
      FActiveWidget.HandleKillFocus;
  except
    { This is just a failsafe, in case FActiveWidget was not correctly set
      in the destructor of TfpgWidget }
    FActiveWidget := nil;
  end;
  FActiveWidget := AValue;
  if FActiveWidget <> nil then
    FActiveWidget.HandleSetFocus;
end;

function TfpgWidget.GetWindow: TfpgNativeWindow;
begin
  Result := TfpgNativeWindow(inherited GetWindow);
end;

procedure TfpgWidget.SetAcceptDrops(const AValue: boolean);
begin
  if FAcceptDrops = AValue then
    exit;
  FAcceptDrops := AValue;
  DoAcceptDrops(AValue);
end;

function TfpgWidget.GetHint: TfpgString;
begin
  Result := FHint;
end;

function TfpgWidget.IsShowHintStored: boolean;
begin
  Result := not ParentShowHint;
end;

procedure TfpgWidget.SetFormDesigner(const AValue: TObject);
var
  i: integer;
begin
  FFormDesigner := AValue;
  for i := 0 to ComponentCount-1 do
  begin
    if (Components[i] is TfpgWidget) and (TfpgWidget(Components[i]).Parent = self) then
      TfpgWidget(Components[i]).FormDesigner := AValue;
  end;
end;

procedure TfpgWidget.SetAlign(const AValue: TAlign);
begin
  if FAlign = AValue then
    Exit;
  FAlign := AValue;
  if Parent <> nil then
    Parent.Realign;
end;

procedure TfpgWidget.DoAllocateWindowHandle;
begin
  if HasOwnWindow then
  begin
    FWindow := TfpgNativeWindow.Create(Self);
    Window.WindowType:=wtChild;
    UpdatePosition;
  end
  else
    FWindow := nil; // GetWindow asks the parent for it's window.
end;

procedure TfpgWidget.AllocateWindowHandle;
begin
  DoAllocateWindowHandle;
  Window.AllocateWindowHandle;

  if Window.Owner = Self then
  begin
    Window.SetWindowVisible(Visible);
    //Window;
  end;
  //TfpgWindowHack(Window).DoAcceptDrops(True);
end;

procedure TfpgWidget.SetVisible(const AValue: boolean);
begin
  if FVisible = AValue then
    Exit; //==>
  FVisible := AValue;
  if FOnScreen then
    if FVisible then
    begin
//      writeln('DEBUG:  TfpgWidget.SetVisible - handleshow');
      HandleShow;
    end
    else
    begin
//      writeln('DEBUG:  TfpgWidget.SetVisible - handlehide');
      HandleHide;
      FOnScreen := True;
    end;
end;

procedure TfpgWidget.SetShowHint(const AValue: boolean);
begin
  if FShowHint <> AValue then
    FShowHint := AValue;
  if FShowHint then
    FParentShowHint := False;
end;

procedure TfpgWidget.SetParentShowHint(const AValue: boolean);
begin
  if FParentShowHint <> AValue then
    FParentShowHint := AValue;
  if FParentShowHint then
    FShowHint := False;
end;

procedure TfpgWidget.SetHint(const AValue: TfpgString);
begin
  FHint := AValue;
end;

function TfpgWidget.GetCanvas: TfpgCanvas;
begin
  Result := TfpgCanvas(inherited GetCanvas);
end;

function TfpgWidget.CreateCanvas: TfpgCanvasBase;
begin
  Result := DefaultCanvasClass.Create(self);
end;

procedure TfpgWidget.DoUpdatePosition;
var
  dw: integer;
  dh: integer;
  ParentLeft, ParentTop: TfpgCoord;
  PaintRect: TfpgRect;
{$IFDEF CStackDebug}
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TfpgWidget.DoUpdatePosition - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  dw      := FWidth - FPrevWidth;
  dh      := FHeight - FPrevHeight;

  if IsContainer and (wdfSize in FDirtyFlags) then
  begin
    {$IFDEF CStackDebug}
    DebugLn(Format('  Alignment deltas  w: %d  h: %d', [dw, dh]));
    {$ENDIF}
    HandleAlignments(dw, dh);
  end;

  if HasOwnWindow and Assigned(Window) then
  begin
    ParentLeft := 0;
    ParentTop := 0;
    if Assigned(Parent) and not Parent.HasOwnWindow then
    begin
      Parent.WidgetToWindow(ParentLeft, ParentTop);
    end;
    //WriteLn(ClassName,' resizing ', Left,':',Top,':',Width,':', Height);
    Window.UpdateWindowPosition(Left+ParentLeft, Top+ParentTop, Width, Height);
  end
  else if Parent <> nil then
  begin
    Invalidate;

    if (dw < 0) or (dh < 0) then
      Parent.InvalidateRect(fpgRect(FLeft, FTop, FPrevWidth, FPrevHeight));
    if wdfPosition in FDirtyFlags then
    begin
      PaintRect := fpgRect(FLeft, FTop, FWidth, FHeight);
      UnionRect(PaintRect, fpgRect(FPrevLeft, FPrevTop, FWidth, FHeight), PaintRect);
      //PaintRect.Width:=PaintRect.Width+(FPrevLeft-FLeft);
      //PaintRect.Height:=PaintRect.Height+(FPrevTop-FTop);

      PaintRect.Width:=PaintRect.Width+5;
      PaintRect.Height:=PaintRect.Height+5;
      Parent.InvalidateRect(PaintRect);

    end;
  end;

  if (dw <> 0) or (dh <> 0) then
    DoResize;

  // We have now handled the difference between old and new values, so reset
  // them here not to affect the next iteration.
  FPrevWidth  := FWidth;
  FPrevHeight := FHeight;
  Exclude(FDirtyFlags, wdfSize);
  Exclude(FDirtyFlags, wdfPosition);
end;

procedure TfpgWidget.SetBackgroundColor(const AValue: TfpgColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    RePaint;
  end;
end;

procedure TfpgWidget.SetTextColor(const AValue: TfpgColor);
begin
  if FTextColor <> AValue then
  begin
    FTextColor := AValue;
    Repaint;
  end;
end;

function TfpgWidget.InDesigner: boolean;
begin
  Result := (FFormDesigner <> nil)
end;

function TfpgWidget.IsLoading: boolean;
begin
  Result := csLoading in ComponentState;
end;

procedure TfpgWidget.InvokeHelp;
begin
  case HelpType of
    htKeyword:
      if HelpKeyword <> '' then
      begin
        fpgApplication.KeywordHelp(HelpKeyword);
        Exit; //==>
      end;
    htContext:
      if HelpContext <> 0 then
      begin
        fpgApplication.ContextHelp(HelpContext);
        Exit; //==>
      end;
  end;
  if Parent <> nil then
    Parent.InvokeHelp
  else
    fpgApplication.InvokeHelp;
end;

procedure TfpgWidget.Realign;
begin
  HandleAlignments(0, 0);
  RePaint;
end;

function TfpgWidget.GetParent: TfpgWidget;
begin
  Result := TfpgWidget(inherited GetParent);
end;

procedure TfpgWidget.SetParent(const AValue: TfpgWidget);
begin
  inherited SetParent(AValue);
  if not HasOwnWindow then
  begin
    if AValue <> nil then
      FWindow := AValue.Window
    else
      FWindow := nil;
  end;
end;

constructor TfpgWidget.Create(AOwner: TComponent);
begin
  Loading;

  //HasOwnWindow:=True;

  FIsContainer    := False;
  FOnScreen       := False;
  FVisible        := True;
  FActiveWidget   := nil;
  FEnabled        := True;
  FFocusable      := False;
  FFocused        := False;
  FTabOrder       := 0;
  FAnchors        := [anLeft, anTop];
  FAlign          := alNone;
  FHint           := '';
  FShowHint       := False;
  FParentShowHint := True;
  FBackgroundColor := clWindowBackground;
  FTextColor      := clText1;
  FAcceptDrops    := False;
  FOnClickPending := False;
  FIgnoreDblClicks := False;

  inherited Create(AOwner);

  if (AOwner <> nil) and (AOwner is TfpgWidget)
  and not (InheritsFrom(TfpgWindow) and (TfpgWindowHack(Self).FWindowType in [wtModalForm, wtPopup, wtWindow])) then
  begin
    Parent := TfpgWidget(AOwner);
    FTabOrder := AOwner.ComponentCount;
  end
  else
    Parent := nil;

  if Parent <> nil then
  begin
    FShowHint   := Parent.ShowHint;
  end;
  FCanvas := CreateCanvas;
end;

destructor TfpgWidget.Destroy;
begin
  {$IFDEF GDEBUG}
  writeln('TfpgWidget.Destroy [', Classname, '.', Name, ']');
  {$ENDIF}
  FCanvas.Free;
  HandleHide;
  if Parent <> nil then
  begin
    if Parent.ActiveWidget = self then
      Parent.ActiveWidget := nil;
    if not HasOwnWindow and Visible then
      Parent.Invalidate;
  end;
  if Assigned(Window) then
    Window.NotifyWidgetDestroying(Self);

  inherited Destroy;
end;

procedure TfpgWidget.AfterConstruction;
begin
  inherited AfterConstruction;
  // This is for components that are created at runtime, after it's
  // parent has already been shown.
  if (Parent <> nil) and (Parent.WindowAllocated) then
  begin
    HandleShow;
  end;

  Loaded;  // remove csLoading from ComponentState
end;

procedure TfpgWidget.MsgKeyChar(var msg: TfpgMessageRec);
var
  lChar: TfpgChar;
  ss: TShiftState;
  consumed: boolean;
  wg: TfpgWidget;
begin
  lChar := msg.params.keyboard.keychar;
  ss  := msg.params.keyboard.shiftstate;

  consumed := False;
  HandleKeyChar(lChar, ss, consumed);

  if not consumed then
  begin
    wg := Parent;
    while (not consumed) and (wg <> nil) do
    begin
      wg.HandleKeyChar(lChar, ss, consumed);
      wg := wg.Parent;
    end;
  end;
end;

procedure TfpgWidget.MsgKeyPress(var msg: TfpgMessageRec);
var
  key: word;
  ss: TShiftState;
  consumed: boolean;
  wg: TfpgWidget;
  wlast: TfpgWidget;
begin
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
    if msg.Stop then
      Exit;
  end;

  key := msg.params.keyboard.keycode;
  ss  := msg.params.keyboard.shiftstate;
  consumed := False;

  { can we handle it ourselves? }
  HandleKeyPress(key, ss, consumed);

  { process our children }
  if not consumed then
    DoKeyShortcut(self, key, ss, consumed, True);

  if not consumed then
  begin
    { Work its way to the top level form. The recursive calling of
      HandleKeyPress() also gives tab-to-change-focus a chance to work. }
    wg := Parent;
    wlast := wg;
    while (not consumed) and (wg <> nil) do
    begin
      wg.HandleKeyPress(key, ss, consumed);
      wlast := wg;
      wg := wg.Parent;
    end;
    wg := wlast;
  end;


  if not consumed then
  begin
    { Now let the top level form do keyboard shortcut processing. }
    if Assigned(wg) then
      wg.DoKeyShortcut(self, key, ss, consumed);

    { Forms aren't focusable, so Form.HandleKeyPress() will not suffice. Give
      the Form a chance to fire its OnKeyPress event. }
    if not consumed then
    begin
      if (wg is TfpgForm) and Assigned(TfpgForm(wg).OnKeyPress) then
        wg.OnKeyPress(self, key, ss, consumed);
    end;

    { now try the Application MainForm - if not the same as top-level form }
    if not consumed then
    begin
      { only do this if the top-level form is not Modal }
      if (wg is TfpgForm) and (TfpgForm(wg).Window.WindowType <> wtModalForm) then
        if wg <> fpgApplication.MainForm then
          TfpgFormFriend(fpgApplication.MainForm).DoKeyShortcut(self, key, ss, consumed);
    end;
  end;

  { now finaly, lets give fpgApplication a chance }
  if (not consumed) and Assigned(fpgApplication.OnKeyPress) then
    fpgApplication.OnKeyPress(self, key, ss, consumed);
end;

procedure TfpgWidget.MsgKeyRelease(var msg: TfpgMessageRec);
var
  key: word;
  ss: TShiftState;
  consumed: boolean;
  wg: TfpgWidget;
begin
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
    if msg.Stop then
      Exit;
  end;

  key := msg.params.keyboard.keycode;
  ss  := msg.params.keyboard.shiftstate;
  consumed := False;

  HandleKeyRelease(key, ss, consumed);
  if not consumed then
  begin
    wg := Parent;
    while (not consumed) and (wg <> nil) do
    begin
      wg.HandleKeyRelease(key, ss, consumed);
      wg := wg.Parent;
    end;
  end;
end;

procedure TfpgWidget.MsgMouseDown(var msg: TfpgMessageRec);
var
  mb: TMouseButton;
begin
  if InDesigner then
  begin
    // dispatching message to designer
    FFormDesigner.Dispatch(msg);
    if msg.Stop then
      Exit;
  end;

  if not FEnabled then
    exit;   // Do we want this here?

  case msg.Params.mouse.Buttons of
    MOUSE_LEFT:
      begin
        uMouseDownSourceWidget := self;
        FDragStartPos.SetPoint(msg.Params.mouse.x, msg.Params.mouse.y);
        mb := mbLeft;
        HandleLMouseDown(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.shiftstate);
      end;

    MOUSE_RIGHT:
      begin
        mb := mbRight;
        HandleRMouseDown(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.shiftstate);
      end;

    MOUSE_MIDDLE:
      begin
        mb := mbMiddle;
      end;
  end;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(self, mb, msg.Params.mouse.shiftstate,
        Point(msg.Params.mouse.x, msg.Params.mouse.y));
end;

procedure TfpgWidget.MsgMouseUp(var msg: TfpgMessageRec);
var
  mb: TMouseButton;
  IsDblClick: boolean;
begin
  //writeln('>> TfpgWidget.MsgMouseUp - ', Classname, '.', Name);
  FDragActive := False;
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
    if msg.Stop then
      Exit;
  end;

  if not FEnabled then
    exit;   // Do we want this here?
    
  IsDblClick := False;

  case msg.Params.mouse.Buttons of
    MOUSE_LEFT:
      begin
        FOnClickPending := True;
        mb := mbLeft;
        if (uLastClickWidget = self) and (not FIgnoreDblClicks) then
          IsDblClick := ((fpgGetTickCount - uLastClickTime) <= DOUBLECLICK_MS)
            and (Abs(uLastClickPoint.x - msg.Params.mouse.x) <= DOUBLECLICK_DISTANCE)
            and (Abs(uLastClickPoint.y - msg.Params.mouse.y) <= DOUBLECLICK_DISTANCE)
          // we detected a double click
        else
          uLastClickWidget := self;

        uLastClickPoint := Point(msg.Params.mouse.x, msg.Params.mouse.y);
        uLastClickTime := fpgGetTickCount;
        if IsDblClick then
        begin
          FOnClickPending := False; { When Double Click occurs we don't want single click }
          HandleDoubleClick(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.Buttons, msg.Params.mouse.shiftstate);
          if Assigned(FOnDoubleClick) then
            FOnDoubleClick(self, mb, msg.Params.mouse.shiftstate,
                Point(msg.Params.mouse.x, msg.Params.mouse.y));
        end;

        // The mouse up must still be handled even if we had a double click event.
        HandleLMouseUp(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.shiftstate);

        uMouseDownSourceWidget := nil;
      end;

    MOUSE_RIGHT:
      begin
        mb := mbRight;
        HandleRMouseUp(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.shiftstate);
      end;

    MOUSE_MIDDLE:
      begin
        mb := mbMiddle;
      end;
  end;
  if Assigned(FOnMouseUp) then // and not IsDblClick then
    FOnMouseUp(self, mb, msg.Params.mouse.shiftstate,
        Point(msg.Params.mouse.x, msg.Params.mouse.y));
  //writeln('<< TfpgWidget.MsgMouseUp - ', Classname, '.', Name);
end;

procedure TfpgWidget.MsgMouseMove(var msg: TfpgMessageRec);
begin
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
    if msg.Stop then
      Exit;
  end;

  if ((msg.Params.mouse.Buttons and MOUSE_LEFT) = MOUSE_LEFT) and (self = uMouseDownSourceWidget) then
  begin
    if not FDragActive and (FDragStartPos.ManhattanLength(fpgPoint(msg.Params.mouse.x, msg.Params.mouse.y)) > fpgApplication.StartDragDistance) then
    begin
      FDragActive := True;
      // In Windows dragging is a blocking function, so FDragActive is false after this call
      DoDragStartDetected;
    end;
  end;

  HandleMouseMove(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.Buttons, msg.Params.mouse.shiftstate);
  if Assigned(OnMouseMove) then
    OnMouseMove(self, msg.Params.mouse.shiftstate,
        Point(msg.Params.mouse.x, msg.Params.mouse.y));
end;

procedure TfpgWidget.MsgDoubleClick(var msg: TfpgMessageRec);
begin
(*
  // If we don't generate a mouse down, we get a rapid click
  // delay under Windows.
  HandleLMouseDown(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.shiftstate);

  HandleDoubleClick(msg.Params.mouse.x, msg.Params.mouse.y,
      msg.Params.mouse.Buttons, msg.Params.mouse.shiftstate);
  if Assigned(FOnDoubleClick) then
    FOnDoubleClick(self, mbLeft, msg.Params.mouse.shiftstate,
        Point(msg.Params.mouse.x, msg.Params.mouse.y));
*)
end;

procedure TfpgWidget.MsgMouseEnter(var msg: TfpgMessageRec);
{$IFDEF CStackDebug}
var
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TfpgWidget.MsgMouseEnter - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
    if msg.Stop then
      Exit;
  end;

  HandleMouseEnter;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure TfpgWidget.MsgMouseExit(var msg: TfpgMessageRec);
{$IFDEF CStackDebug}
var
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TfpgWidget.MsgMouseExit - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
    if msg.Stop then
      Exit;
  end;

  HandleMouseExit;
  if Assigned(FOnMouseExit) then
    FOnMouseExit(Self);
end;

procedure TfpgWidget.MsgMouseScroll(var msg: TfpgMessageRec);
begin
  HandleMouseScroll(msg.Params.mouse.x, msg.Params.mouse.y,
      msg.Params.mouse.shiftstate, msg.Params.mouse.delta);
end;

procedure TfpgWidget.MsgMouseHorizScroll(var msg: TfpgMessageRec);
begin
  HandleMouseHorizScroll(msg.Params.mouse.x, msg.Params.mouse.y,
      msg.Params.mouse.shiftstate, msg.Params.mouse.delta);
end;

procedure TfpgWidget.MsgDropEnter(var msg: TfpgMessageRec);
begin
  // do nothing
end;

procedure TfpgWidget.MsgDropExit(var msg: TfpgMessageRec);
begin
  // do nothing
end;

function TfpgWidget.GetOnShowHint: THintEvent;
begin
  Result := FOnShowHint;
end;

procedure TfpgWidget.SetOnShowHint(const AValue: THintEvent);
begin
  FOnShowHint := AValue;
end;

procedure TfpgWidget.HandleShow;
var
  n: integer;
  c: TComponent;
begin
  InternalHandleShow;

  for n := 0 to ComponentCount - 1 do
  begin
    c := Components[n];
    if (c is TfpgWidget) and (TfpgWidget(c).Parent = self) then
    begin
      if not (c is TfpgPopupMenu) then  // these should not be created yet
      begin
        TfpgWidget(c).HandleShow;
      end;
    end;
  end;
end;

procedure TfpgWidget.InternalHandleShow;
begin
  FOnScreen := True;
  AllocateWindowHandle;
end;

procedure TfpgWidget.HandleHide;
var
  n: integer;
  c: TComponent;
begin
  for n := 0 to ComponentCount - 1 do
  begin
    c := Components[n];
    if (c is TfpgWidget) and (TfpgWidget(c).Parent = self) then
      TfpgWidget(c).HandleHide;
  end;
  FOnScreen := False;

  if HasOwnWindow then
  begin
    FWindow.Free;
    FWindow := nil;
  end;
end;

procedure TfpgWidget.RePaint;
var
  P: TfpgMessageParams;
begin
  if WindowAllocated  and Window.HasHandle then
  begin
    if not HasOwnWindow and Assigned(Parent) then
      Parent.InvalidateRect(GetBoundsRect)
    else if not Invalidated then
    begin
      FInvalidated := True;
      P.rect := FInvalidRect;
      fpgPostMessage(self, self, FPGM_PAINT, P);
    end;
  end;
end;

procedure TfpgWidget.SetFocus;
begin
  HandleSetFocus;
end;

procedure TfpgWidget.KillFocus;
begin
  HandleKillFocus;
end;

procedure TfpgWidget.HandlePaint;
begin
  Canvas.ClearClipRect;
  if FBackgroundColor <> clNone then
    Canvas.Clear(FBackgroundColor);
end;

procedure TfpgWidget.HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: boolean);
begin
  if FFocusable and Assigned(OnKeyChar) then
    OnKeyChar(self, AText, consumed);
end;

procedure TfpgWidget.HandleKeyPress(var keycode: word; var shiftstate: TShiftState;
    var consumed: boolean);
var
  wg: TfpgWidget;
  dir: integer;
begin
  if Assigned(OnKeyPress) and FFocusable then
    OnKeyPress(self, keycode, shiftstate, consumed);

  if consumed then
    Exit; //==>

  dir := 0;

  if not consumed and (keycode = fpgApplication.HelpKey) and (shiftstate=[]) then
  begin
    if fpgApplication.HelpFile <> '' then
    begin
      InvokeHelp;
      consumed := True;
    end;
  end;

  case keycode of
    keyTab:
        if (ssShift in shiftstate) then
          dir := -1
        else
          dir := 1;
{
    keyReturn,
    keyDown,
    keyRight:
        dir := 1;

    keyUp,
    keyLeft:
        dir := -1;
}
      keyMenu:
        begin
          // ssExtra1 is a signal that keyMenu was used.
          HandleRMouseDown(Width div 2, Height div 2, [ssExtra1]);
          consumed := True;
        end;
  end;

  {$Note Optimize this code. Constantly setting ActiveWidget causes RePaint to be called!}
  if dir = 1 then
  begin
    // forward
    wg           := FindFocusWidget(ActiveWidget, fsdNext);
    ActiveWidget := wg;
    if wg <> nil then
      consumed := True
    else
    begin
      if Parent = nil then
      begin
        wg           := FindFocusWidget(ActiveWidget, fsdFirst);
        ActiveWidget := wg;
        consumed     := True;
      end;
    end;
  end
  else if dir = -1 then
  begin
    // backward
    wg           := FindFocusWidget(ActiveWidget, fsdPrev);
    ActiveWidget := wg;
    if wg <> nil then
    begin
      consumed := True;
      // we must find the last one!
      while wg <> nil do
      begin
        wg.ActiveWidget := wg.FindFocusWidget(ActiveWidget, fsdLast);
        wg := wg.ActiveWidget;
      end;
    end
    else if Parent = nil then
    begin
      wg           := FindFocusWidget(ActiveWidget, fsdLast);
      ActiveWidget := wg;
      consumed     := True;
    end;
  end;
end;

procedure TfpgWidget.HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  // descendants will implement this.
end;

procedure TfpgWidget.HandleSetFocus;
var
  awg: TfpgWidget;
begin
  if not FFocused and FFocusable then
  begin
    FFocused := True;
    RePaint;
    // focusing a child
    if ActiveWidget <> nil then
      ActiveWidget.SetFocus
    else
    begin
      // try to find it for the first time.
      awg := FindFocusWidget(nil, fsdFirst);
      if awg <> nil then
        ActiveWidget := awg;
    end;
  end;

  if Parent <> nil then
  begin
    Parent.ActiveWidget := self;
    Parent.SetFocus;
  end;
  
  if Assigned(OnEnter) then
    OnEnter(self);
end;

procedure TfpgWidget.HandleKillFocus;
begin
  FFocused := False;
  RePaint;

  if ActiveWidget <> nil then
    ActiveWidget.KillFocus;
    
  if Assigned(OnExit) then
    OnExit(self);
end;

procedure TfpgWidget.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  pw: TfpgWidget;
  w: TfpgWidget;
begin
  if FShowHint then
    fpgApplication.HideHint;

  // setting the focus through all parents
  pw := Parent;
  w  := self;
  while pw <> nil do
  begin
    if w.Visible and w.Enabled and w.Focusable then
      pw.ActiveWidget := w;
    w := pw;
    pw := pw.Parent;
  end;
end;

procedure TfpgWidget.HandleRMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  if FShowHint then
    fpgApplication.HideHint;
  // keyMenu was pressed
  if shiftstate = [ssExtra1] then
    HandleRMouseUp(x, y, []);
end;

procedure TfpgWidget.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
var
  r: TfpgRect;
begin
  r.SetRect(0, 0, Width, Height);
  if PtInRect(r, Point(x, y)) and FOnClickPending and (self = uMouseDownSourceWidget) then
  begin
    if Assigned(FOnClick) then
    FOnClick(self);
  end;
end;

procedure TfpgWidget.HandleRMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  // do nothing yet
end;

procedure TfpgWidget.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  msgp: TfpgMessageParams;
begin
  fillchar(msgp, sizeof(msgp), 0);
  msgp.user.Param1 := 2;
  msgp.user.Param2 := x+10;
  msgp.user.Param3 := y+2;

  { Only send message if really needed. }
  if Assigned(Parent) then
  begin
    if fpgApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.ShowHint)) and (FHint <> '') then
      fpgPostMessage(Self, fpgApplication, FPGM_HINTTIMER, msgp);
  end
  else
  begin
    if fpgApplication.ShowHint and FShowHint and (FHint <> '') then
      fpgPostMessage(Self, fpgApplication, FPGM_HINTTIMER, msgp);
  end;
end;

procedure TfpgWidget.HandleDoubleClick(x, y: integer; button: word; shiftstate: TShiftState);
begin
  // do nothing yet
end;

procedure TfpgWidget.HandleMouseEnter;
var
  msgp: TfpgMessageParams;
  b: boolean;
{$IFDEF CStackDebug}
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TfpgWidget.HandleMouseEnter - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  fillchar(msgp, sizeof(msgp), 0);

  if Assigned(Parent) then
    b := Enabled and fpgApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.ShowHint)) and (FHint <> '')
  else
    b := Enabled and fpgApplication.ShowHint and FShowHint and (FHint <> '');

  msgp.user.Param1 := Ord(b);
  fpgPostMessage(Self, fpgApplication, FPGM_HINTTIMER, msgp);
end;

procedure TfpgWidget.HandleMouseExit;
begin
  {$IFDEF CStackDebug}
  writeln('TfpgWidget.HandleMouseExit: ' + ClassName);
  {$ENDIF}
  if FShowHint then
    fpgApplication.HideHint;
end;

procedure TfpgWidget.HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint);
begin
  if Assigned(FOnMouseScroll) then
    FOnMouseScroll(self, shiftstate, delta, Point(x, y));
end;

function TfpgWidget.Invalidated: Boolean;
begin
  Result := FInvalidated;
  // if the parent is invalidated we will be repainted anyway
  if not Result and not HasOwnWindow and Assigned(Parent) then
    Result := Parent.Invalidated;
end;

function TfpgWidget.IsHidden: Boolean;
begin
  // this currently is only True for TabSheets that are not the active page.
  // Perhaps in the future this could be smart enough to know if the widget is
  // completely covered by another widget and return True here.
  Result := False;
  if Assigned(Parent) then
  begin
    if (not Visible)
    or (Left >= Parent.Width-1)
    or (Left + Width <= 0)
    or (Top >= Parent.Height-1)
    or (Top + Height <= 0) then
      Result := True;
  end;

  if not Result and not HasOwnWindow and Assigned(Parent) then
    Result := Parent.IsHidden;
end;

procedure TfpgWidget.HandleMouseHorizScroll(x, y: integer; shiftstate: TShiftState; delta: smallint);
begin
  if Assigned(FOnMouseHorizScroll) then
    FOnMouseHorizScroll(self, shiftstate, delta, Point(x, y));
end;

function TfpgWidget.FindFocusWidget(startwg: TfpgWidget; direction: TFocusSearchDirection): TfpgWidget;
var
  w: TfpgWidget;
  n: integer;
  FoundIt: boolean;
  lasttaborder: integer;
begin
  Result  := nil;
  FoundIt := False;
  if direction in [fsdLast, fsdPrev] then
    lasttaborder := Low(integer)
  else
    lasttaborder := High(integer);

  for n := 0 to ComponentCount - 1 do
  begin
    if Components[n] is TfpgWidget then
    begin
      w := TfpgWidget(Components[n]);
      if w.Enabled and w.Visible and w.Focusable then
      begin
        case direction of
          fsdFirst:
            if w.TabOrder < lasttaborder then
            begin
              Result       := w;
              lasttaborder := w.TabOrder;
            end;

          fsdLast:
            if lasttaborder <= w.TabOrder then
            begin
              Result       := w;
              lasttaborder := w.TabOrder;
            end;

          fsdNext:
            if startwg = w then
              FoundIt := True
            else if w.TabOrder < lasttaborder then
            begin
              if (startwg = nil) or
                (w.TabOrder > startwg.TabOrder) or
                (FoundIt and (w.TabOrder = startwg.TabOrder)) then
              begin
                Result       := w;
                lasttaborder := w.TabOrder;
              end;
            end;
          fsdPrev:
            if startwg = w then
              FoundIt := True
            else if w.TabOrder >= lasttaborder then
              if (startwg = nil) or
                (w.TabOrder < startwg.TabOrder) or
                (not FoundIt and (w.TabOrder = startwg.TabOrder)) then
              begin
                Result       := w;
                lasttaborder := w.TabOrder;
              end;

        end; { case }
      end; { if w.Enabled... }
    end;
  end; { if }
end;

procedure TfpgWidget.MsgPaint(var msg: TfpgMessageRec);
var
  i: Integer;
  w: TfpgWidget;
  HasInvalidRegion: Boolean;
  Params: TfpgMessageParams;
  {$IFDEF GDEBUG}
  Tmp: TfpgRect;
  {$ENDIF}
begin
  if IsHidden then
    Exit;

  if not (WindowAllocated and (Window.HasHandle)) then
  begin
    // The window will generate a paint message later when it exists
    FInvalidated:=False;
    Exit;//
  end;


  if (Width < 1) or (Height < 1) then
    Exit;

  // combine existing invalid rect with message rect if sent
  if FInvalidRect.IsUnassigned then
    FInvalidRect := msg.Params.rect
  else
    if not msg.Params.rect.IsUnassigned then
      UnionRect(FInvalidRect, FInvalidRect, msg.Params.rect);

  HasInvalidRegion:=not FInvalidRect.IsUnassigned;

  Canvas.BeginDraw;

  HandlePaint;
  if Assigned(FOnPaint) then
    FOnPaint(Self);

  {$IFDEF GDEBUG}
  Tmp := FInvalidRect; // for debugging
  {$ENDIF}

  if HasOwnWindow then
  begin
    //WriteLn('WINDOW PAINT ==============>>>>>>>>>>>>>');
    //Write('Main ClipRect: '); PrintRect(FInvalidRect);
    if HasInvalidRegion and ((FInvalidRect.Width <= 0)  or (FInvalidRect.Height <= 0 )) then
    begin
      Canvas.EndDraw;
      FInvalidRect.Clear;
      FInvalidated:=False;
      {$IFDEF GDEBUG}
      Writeln('Invalid Rect Detected!');
      Write('MSG: '); PrintRect(msg.Params.rect);
      Write('INV: '); PrintRect(Tmp);
      {$ENDIF}
      Exit;
    end;
  end;



  for i := 0 to ComponentCount-1 do
  begin
    w := TfpgWidget(Components[i]);
    if w.InheritsFrom(TfpgWidget) then
    begin
      if not w.HasOwnWindow and w.Visible and assigned(w.parent) then
      begin
        if not HasInvalidRegion or IntersectRect(Params.rect, FInvalidRect, w.GetBoundsRect) then
        begin
          if HasInvalidRegion then
            w.ParentToWidget(Params.rect.Left, Params.rect.Top)
          else
            Params.rect.Clear;
          fpgSendMessage(Self, w, FPGM_PAINT, Params);
        end;
      end;
    end;

  end;
  if HasInvalidRegion then
    Canvas.EndDraw(FInvalidRect)
  else
    Canvas.EndDraw;

  FInvalidRect.Clear;
  FInvalidated:=False;
end;

procedure TfpgWidget.MsgResize(var msg: TfpgMessageRec);
var
  dw: integer;
  dh: integer;
  _w, _h: integer;
{$IFDEF CStackDebug}
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TfpgWidget.MsgResize - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  _w := FWidth;
  _h := FHeight;
  { Width and Height might not be what came through in the msg because of
    size constraints, so we calculate the delta diffs after HandleResize }
  HandleResize(msg.Params.rect.Width, msg.Params.rect.Height);
  //dw      := msg.Params.rect.Width - FWidth;
  //dh      := msg.Params.rect.Height - FHeight;
  dw := FWidth - _w;
  dh := FHeight - _h;
  HandleAlignments(dw, dh);
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
  end;
  DoResize;
end;

procedure TfpgWidget.MsgMove(var msg: TfpgMessageRec);
{$IFDEF CStackDebug}
var
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TfpgWidget.MsgMove - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  HandleMove(msg.Params.rect.Left, msg.Params.rect.Top);
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
  end;
end;

procedure TfpgWidget.HandleAlignments(const dwidth, dheight: TfpgCoord);
var
  n: integer;
  wg: TfpgWidget;
  dx: integer;
  dy: integer;
  dw: integer;
  dh: integer;
  w: TfpgWidget;
{$IFDEF CStackDebug}
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TfpgWidget.HandleAlignments - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  if (csLoading in ComponentState) then
  begin
    {$IFDEF CStackDebug}
    DebugLn('HandleAlignments ('+Name+'): csLoading detected, so we exit early');
    {$ENDIF}
    Exit;  //==>
  end;

  {$IFDEF CStackDebug}
  DebugLn(Format('dwidth=%d  dheight=%d  Classname=''%s''', [dwidth, dheight, ClassName]));
  {$ENDIF}

  FAlignRect := GetClientRect;
  alist := TList.Create;
  try
    for n := 0 to ComponentCount - 1 do
    begin
      if Components[n] is TfpgWidget then
      begin
        w := TfpgWidget(Components[n]);
        if (w.Align <> alNone) and (w.Visible) then
          alist.Add(w);
      end;
    end;

    DoAlignment;
    //DoAlign(alTop);
    //DoAlign(alBottom);
    //DoAlign(alLeft);
    //DoAlign(alRight);
    //DoAlign(alClient);
  finally
    alist.Free;
  end;

  // Finally handle anchors (where Align = alNone)
  for n := 0 to ComponentCount - 1 do
    if (Components[n] is TfpgWidget) then
    begin
      wg := TfpgWidget(Components[n]);
      if (wg.FAlign = alNone) and ([anLeft, anTop] <> wg.Anchors) then
      begin
        // we must alter the window
        dx := 0;
        dy := 0;
        dw := 0;
        dh := 0;

        if (anRight in wg.Anchors) then
          if (anLeft in wg.Anchors) then
            dw := dwidth
          else
            dx := dwidth
        else if not (anLeft in wg.Anchors) then
          dx := (dwidth div 2);

        if (anBottom in wg.Anchors) then
          if (anTop in wg.Anchors) then
            dh := dheight
          else
            dy := dheight
        else if not (anTop in wg.Anchors) then
          dy := (dheight div 2);

        wg.MoveAndResizeBy(dx, dy, dw, dh);
        //Write(wg.ClassName + ': ');
        //PrintRect(wg.WidgetBoundsInWindow);
      end;
    end;  { if }
end;

procedure TfpgWidget.MoveAndResize(ALeft, ATop, AWidth, AHeight: TfpgCoord);
{$IFDEF CStackDebug}
var
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TfpgWidget.MoveAndResize');
  DebugLn(Format('Class:%s  t:%d  l:%d  w:%d  h:%d', [Classname, ATop, ALeft, AWidth, aHeight]));
  {$ENDIF}
  if not (csLoading in ComponentState) then
  begin
    if (ALeft <> FLeft) or (ATop <> FTop) then
      HandleMove(ALeft, ATop);
    if (AWidth <> FWidth) or (AHeight <> FHeight) then
      HandleResize(AWidth, AHeight);
  end
  else
  begin
    // When the widget is created, it's position will be applied
    Left   := ALeft;
    Top    := ATop;
    Width  := AWidth;
    Height := AHeight;
  end;
  UpdatePosition;
end;

procedure TfpgWidget.MoveAndResizeBy(const dx, dy, dw, dh: TfpgCoord);
begin
  if (dx <> 0) or (dy <> 0) or
    (dw <> 0) or (dh <> 0) then
    MoveAndResize(FLeft + dx, FTop + dy, FWidth + dw, FHeight + dh);
end;

procedure TfpgWidget.DoAlignment;
var
  w: TfpgWidget;
  n: integer;
begin
  // and process this list in order
  for n := 0 to alist.Count - 1 do
  begin
    w := TfpgWidget(alist[n]);
    case w.Align of
      alTop:
        begin
          w.MoveAndResize(FAlignRect.Left, FAlignRect.Top, FAlignRect.Width, w.Height);
          Inc(FAlignRect.top, w.Height);
          Dec(FAlignRect.Height, w.Height);
        end;

      alBottom:
        begin
          w.MoveAndResize(FAlignRect.Left, FAlignRect.Top + FAlignRect.Height - w.Height, FAlignRect.Width, w.Height);
          Dec(FAlignRect.Height, w.Height);
        end;

      alLeft:
        begin
          w.MoveAndResize(FAlignRect.Left, FAlignRect.Top, w.Width, FAlignRect.Height);
          Inc(FAlignRect.Left, w.Width);
          Dec(FAlignRect.Width, w.Width);
        end;

      alRight:
        begin
          w.MoveAndResize(FAlignRect.Left + FAlignRect.Width - w.Width, FAlignRect.Top, w.Width, FAlignRect.Height);
          Dec(FAlignRect.Width, w.Width);
        end;

      alClient:
        w.MoveAndResize(FAlignRect.Left, FAlignRect.Top, FAlignRect.Width, FAlignRect.Height);
    end; { case }
  end;
end;

procedure TfpgWidget.DoResize;
begin
  if Assigned(FOnResize) then
    FOnResize(Self);
end;

procedure TfpgWidget.DoShowHint(var AHint: TfpgString);
begin
  AHint := Hint;
  if Assigned(FOnShowHint) then
  begin
    FOnShowHint(self, AHint);
  end;
end;

procedure TfpgWidget.DoKeyShortcut(const AOrigin: TfpgWidget;
  const keycode: word; const shiftstate: TShiftState; var consumed: boolean; const IsChildOfOrigin: boolean = False);
var
  c: TfpgComponent;
  wg: TfpgWidget;
  i: integer;
begin
  //writeln(Classname, ' - ', Name, '.DoKeyShortcut() - ' + KeycodeToText(keycode, shiftstate));
  { process children of self }
  for i := 0 to ComponentCount-1 do
  begin
    c := TfpgComponent(Components[i]);
    if not (c is TfpgWidget) then
    begin
      //writeln('** skipped ', Classname, ' - ', Name);
      continue;
    end
    else
      wg := TfpgWidget(c);
    if (wg <> nil) and (wg <> self) and (wg <> AOrigin) then
    begin
      { ignore the MenuBar now, because it will be processed later by the top-level Form }
      if IsChildOfOrigin and (wg is TfpgMenuBar) then
      begin
        continue;
      end
      else
        wg.DoKeyShortcut(AOrigin, keycode, shiftstate, consumed);
      if consumed then
        Exit;
    end;
  end;
end;

procedure TfpgWidget.SetPosition(aleft, atop, awidth, aheight: TfpgCoord);
{$IFDEF CStackDebug}
var
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TfpgWidget.SetPosition - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  if (FLeft <> ALeft) or (FTop <> ATop) or (FWidth <> AWidth) or (FHeight <> AHeight) then
    MoveAndResize(aleft, atop, awidth, aheight);
end;

procedure TfpgWidget.Invalidate;
begin
  RePaint;
end;

procedure TfpgWidget.InvalidateRect(ARect: TfpgRect);
begin
  if not HasOwnWindow then
  begin
    // If we don't have our own window then call invalidate on the parent
    WidgetToParent(ARect.Left, ARect.Top);
    if Assigned(Parent) then
      Parent.InvalidateRect(ARect);
    Exit;
  end;

  if FInvalidRect.IsUnassigned then
    FInvalidRect := ARect
  else
    UnionRect(FInvalidRect, FInvalidRect, ARect);

  Invalidate;
end;


initialization
  FocusRootWidget := nil;
  uLastClickWidget := nil;
  uLastClickTime := 0;

end.

