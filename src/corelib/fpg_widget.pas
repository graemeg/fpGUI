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
      The base widget, which all GUI widgets inherit from.
}

unit fpg_widget;

{$mode objfpc}{$H+}

{.$Define DEBUG}

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


  TfpgWidget = class(TfpgWindow)
  private
    FAcceptDrops: boolean;
    FAlignRect: TfpgRect;
    FOnClick: TNotifyEvent;
    FOnDoubleClick: TMouseButtonEvent;
    FOnDragDrop: TfpgDragDropEvent;
    FOnDragEnter: TfpgDragEnterEvent;
    FOnDragLeave: TNotifyEvent;
    FOnDragStartDetected: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnMouseDown: TMouseButtonEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseExit: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseButtonEvent;
    FOnPaint: TPaintEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnResize: TNotifyEvent;
    FOnScreen: boolean;
    FOnShowHint: THintEvent;
    FDragStartPos: TfpgPoint;
    FDragActive: boolean;
    procedure   SetActiveWidget(const AValue: TfpgWidget);
    function    IsShowHintStored: boolean;
    procedure   SetFormDesigner(const AValue: TObject);
  protected
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
    procedure   MsgDropEnter(var msg: TfpgMessageRec); message FPGM_DROPENTER;
    procedure   MsgDropExit(var msg: TfpgMessageRec); message FPGM_DROPEXIT;
  protected
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
    FShowHint: boolean;
    FParentShowHint: boolean;
    FBackgroundColor: TfpgColor;
    FTextColor: TfpgColor;
    FIsContainer: Boolean;
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
    procedure   DoUpdateWindowPosition; override;
    procedure   DoAlign(AAlign: TAlign);
    procedure   DoResize;
    procedure   DoShowHint(var AHint: TfpgString);
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
    function    FindFocusWidget(startwg: TfpgWidget; direction: TFocusSearchDirection): TfpgWidget;
    procedure   HandleAlignments(const dwidth, dheight: TfpgCoord); virtual;
    procedure   HandleShow; virtual;
    procedure   InternalHandleShow; virtual;
    procedure   HandleHide; virtual;
    procedure   MoveAndResize(ALeft, ATop, AWidth, AHeight: TfpgCoord);
    procedure   RePaint;
    { property events }
    property    OnClick: TNotifyEvent read FOnClick write FOnClick;
    property    OnDoubleClick: TMouseButtonEvent read FOnDoubleClick write FOnDoubleClick;
    property    OnDragStartDetected: TNotifyEvent read FOnDragStartDetected write FOnDragStartDetected;
    property    OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property    OnExit: TNotifyEvent read FOnExit write FOnExit;
    property    OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property    OnMouseDown: TMouseButtonEvent read FOnMouseDown write FOnMouseDown;
    property    OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property    OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
    property    OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property    OnMouseUp: TMouseButtonEvent read FOnMouseUp write FOnMouseUp;
    property    OnPaint: TPaintEvent read FOnPaint write FOnPaint;
    property    OnResize: TNotifyEvent read FOnResize write FOnResize;
    property    OnShowHint: THintEvent read GetOnShowHint write SetOnShowHint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterConstruction; override;
    function    InDesigner: boolean;
    procedure   InvokeHelp; virtual;
    procedure   Realign;
    procedure   SetFocus;
    procedure   KillFocus;
    procedure   MoveAndResizeBy(const dx, dy, dw, dh: TfpgCoord);
    procedure   SetPosition(aleft, atop, awidth, aheight: TfpgCoord); virtual;
    procedure   Invalidate; // double check this works as developers expect????
    property    FormDesigner: TObject read FFormDesigner write SetFormDesigner;
    property    Parent: TfpgWidget read GetParent write SetParent;
    property    AcceptDrops: boolean read FAcceptDrops write SetAcceptDrops;
    property    ActiveWidget: TfpgWidget read FActiveWidget write SetActiveWidget;
    property    IsContainer: Boolean read FIsContainer;
    property    Visible: boolean read FVisible write SetVisible default True;
    property    Enabled: boolean read FEnabled write SetEnabled default True;
    property    TabOrder: integer read FTabOrder write FTabOrder;
    { Is the widget allowed to receive keyboard focus. }
    property    Focusable: boolean read FFocusable write FFocusable default False;
    property    Focused: boolean read FFocused write FFocused default False;
    property    Anchors: TAnchors read FAnchors write FAnchors;
    property    Align: TAlign read FAlign write FAlign;
    property    Hint: TfpgString read GetHint write SetHint;
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
  fpg_form;  { for OnKeyPress handling }


var
  uLastClickWidget: TfpgWidget;
  uLastClickPoint: TPoint;
  uLastClickTime: DWord;
  

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
  
  if FActiveWidget <> nil then
    FActiveWidget.HandleKillFocus;
  FActiveWidget := AValue;
  if FActiveWidget <> nil then
    FActiveWidget.HandleSetFocus;
end;

procedure TfpgWidget.SetAcceptDrops(const AValue: boolean);
begin
  if FAcceptDrops = AValue then
    exit;
  FAcceptDrops := AValue;
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

procedure TfpgWidget.DoUpdateWindowPosition;
var
  dw: integer;
  dh: integer;
begin
//  writeln('DoUpdateWindowPosition - ', Classname);
  dw      := FWidth - FPrevWidth;
  dh      := FHeight - FPrevHeight;

  if IsContainer and FSizeIsDirty then
  begin
//    writeln('DoUpdateWindowPosition ', Classname, ' - w:', dw, ' h:', dh);
    HandleAlignments(dw, dh);
  end;

  inherited DoUpdateWindowPosition;
  if (dw <> 0) or (dh <> 0) then
    DoResize;

  // We have now handled the difference between old and new values, so reset
  // them here not to affect the next iteration.
  FPrevWidth  := FWidth;
  FPrevHeight := FHeight;
  FSizeIsDirty:= False;
  FPosIsDirty := False;
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
end;

constructor TfpgWidget.Create(AOwner: TComponent);
begin
  {$if defined(VER2_0) or defined(VER2_2_0)}
  Include(ComponentState, csLoading);
  {$else}
  Loading;
  {$endif}

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
  FDragActive     := False;

  inherited Create(AOwner);

  if (AOwner <> nil) and (AOwner is TfpgWidget) then
  begin
    Parent := TfpgWidget(AOwner);
    FTabOrder := AOwner.ComponentCount;
  end
  else
    Parent := nil;

  if Parent <> nil then
  begin
    FWindowType := wtChild;
    FShowHint   := Parent.ShowHint;
  end;
end;

destructor TfpgWidget.Destroy;
begin
  {$IFDEF DEBUG}
  writeln('TfpgWidget.Destroy [', Classname, '.', Name, ']');
  {$ENDIF}
  HandleHide;
  inherited Destroy;
end;

procedure TfpgWidget.AfterConstruction;
begin
  inherited AfterConstruction;
  // This is for components that are created at runtime, after it's
  // parent has already been shown.
  if (Parent <> nil) and (Parent.HasHandle) then
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

  HandleKeyPress(key, ss, consumed);
  if not consumed then
  begin
    { work it's way to one before top level form - forms are not focusable remember }
    wg := Parent;
    wlast := wg;
    while (not consumed) and (wg <> nil) do
    begin
      wg.HandleKeyPress(key, ss, consumed);
      wlast := wg;
      wg := wg.Parent;
    end;
  end;
  { we should now be at the top level form }
  if (not consumed) and (wlast <> nil) then
  begin
    if (wlast is TfpgForm) and Assigned(wlast.OnKeyPress) then
      wlast.OnKeyPress(self, key, ss, consumed);
  end;
  { now finaly, lets give fpgApplication a chance }
  if (not consumed) and Assigned(fpgApplication.OnKeyPress) then
  begin
    fpgApplication.OnKeyPress(self, key, ss, consumed);
  end;
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
//  writeln('TfpgWidget.MsgMouseUp');
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
        mb := mbLeft;
        if uLastClickWidget = self then
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

          HandleDoubleClick(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.Buttons, msg.Params.mouse.shiftstate);
          if Assigned(FOnDoubleClick) then
            FOnDoubleClick(self, mb, msg.Params.mouse.shiftstate,
                Point(msg.Params.mouse.x, msg.Params.mouse.y));
        end;

        // The mouse up must still be handled even if we had a double click event.
        HandleLMouseUp(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.shiftstate);
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
end;

procedure TfpgWidget.MsgMouseMove(var msg: TfpgMessageRec);
begin
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
    if msg.Stop then
      Exit;
  end;

  if (msg.Params.mouse.Buttons and MOUSE_LEFT) = MOUSE_LEFT then
  begin
    if not FDragActive and (FDragStartPos.ManhattanLength(fpgPoint(msg.Params.mouse.x, msg.Params.mouse.y)) > fpgApplication.StartDragDistance) then
    begin
      FDragActive := True;
      if Assigned(OnDragStartDetected) then
        OnDragStartDetected(self);
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
begin
  {$IFDEF DEBUG}
  writeln('MsgMouseEnter');
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
begin
  {$IFDEF DEBUG}
  writeln('MsgMouseExit');
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
  FOnScreen := True;
  AllocateWindowHandle;
  DoSetWindowVisible(FVisible);

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
  DoSetWindowVisible(FVisible);
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

  if HasHandle then
    ReleaseWindowHandle;
end;

procedure TfpgWidget.RePaint;
begin
  if HasHandle then
    fpgSendMessage(self, self, FPGM_PAINT);
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
  // descendants will implement this.
end;

procedure TfpgWidget.HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: boolean);
begin
  // descendants will implement this.
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
  // do nothing yet
end;

procedure TfpgWidget.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  if Assigned(FOnClick) then
    FOnClick(self);
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
begin
  {$IFDEF DEBUG}
  writeln('TfpgWidget.HandleMouseEnter: ' + ClassName);
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
  {$IFDEF DEBUG}
  writeln('TfpgWidget.HandleMouseExit: ' + ClassName);
  {$ENDIF}
  if FShowHint then
    fpgApplication.HideHint;
end;

procedure TfpgWidget.HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint);
begin
  // do nothing yet
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
end;

procedure TfpgWidget.MsgPaint(var msg: TfpgMessageRec);
begin
//  writeln('TfpgWidget.MsgPaint - ', Classname);
  Canvas.BeginDraw;
  HandlePaint;
  if Assigned(FOnPaint) then
    FOnPaint(Self);
  Canvas.EndDraw;
end;

procedure TfpgWidget.MsgResize(var msg: TfpgMessageRec);
var
  dw: integer;
  dh: integer;
begin
  dw      := msg.Params.rect.Width - FWidth;
  dh      := msg.Params.rect.Height - FHeight;
  HandleResize(msg.Params.rect.Width, msg.Params.rect.Height);
  HandleAlignments(dw, dh);
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
  end;
  DoResize;
end;

procedure TfpgWidget.MsgMove(var msg: TfpgMessageRec);
begin
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
begin
  if (csLoading in ComponentState) then
    Exit;  //==>
//  writeln('HandleAlignments - ', Classname);
  FAlignRect := GetClientRect;

  DoAlign(alTop);
  DoAlign(alBottom);
  DoAlign(alLeft);
  DoAlign(alRight);
  DoAlign(alClient);

  // handle anchors finally for alNone
  for n := 0 to ComponentCount - 1 do
    if (Components[n] is TfpgWidget) then
    begin
      wg := TfpgWidget(Components[n]);
      if (wg.FAlign = alNone) and
          ((anBottom in wg.Anchors) or (anRight in wg.Anchors)) then
      begin
        // we must alter the window
        dx := 0;
        dy := 0;
        dw := 0;
        dh := 0;

        if (anLeft in wg.Anchors) and (anRight in wg.Anchors) then
          dw := dwidth
        else if anRight in wg.Anchors then
          dx := dwidth;

        if (anTop in wg.Anchors) and (anBottom in wg.Anchors) then
          dh := dheight
        else if anBottom in wg.Anchors then
          dy := dheight;

        wg.MoveAndResizeBy(dx, dy, dw, dh);
      end;
    end;  { if }
end;

procedure TfpgWidget.MoveAndResize(ALeft, ATop, AWidth, AHeight: TfpgCoord);
begin
//  writeln('MoveAndResize: ', Classname, ' t:', ATop, ' l:', ALeft, ' w:', AWidth, ' h:', aHeight);
  if HasHandle then
  begin
    if (ALeft <> FLeft) or (ATop <> FTop) then
      HandleMove(ALeft, ATop);
    if (AWidth <> FWidth) or (AHeight <> FHeight) then
      HandleResize(AWidth, AHeight);
    UpdateWindowPosition;
  end
  else
  begin
    // When the widget is created, it's position will be applied
    Left   := ALeft;
    Top    := ATop;
    Width  := AWidth;
    Height := AHeight;
  end;
end;

procedure TfpgWidget.MoveAndResizeBy(const dx, dy, dw, dh: TfpgCoord);
begin
  if (dx <> 0) or (dy <> 0) or
    (dw <> 0) or (dh <> 0) then
    MoveAndResize(FLeft + dx, FTop + dy, FWidth + dw, FHeight + dh);
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

procedure TfpgWidget.DoAlign(AAlign: TAlign);
var
  alist: TList;
  w: TfpgWidget;
  n: integer;
begin
  alist := TList.Create;
  for n := 0 to ComponentCount - 1 do
    if Components[n] is TfpgWidget then
    begin
      w := TfpgWidget(Components[n]);
      if (w.Align = AAlign) and (w.Visible) then
        alist.Add(w);
    end;

  alist.Sort(@AlignCompare);

  // and process this list in order
  for n := 0 to alist.Count - 1 do
  begin
    w := TfpgWidget(alist[n]);
    case AAlign of
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

  alist.Free;
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

procedure TfpgWidget.SetPosition(aleft, atop, awidth, aheight: TfpgCoord);
begin
  MoveAndResize(aleft, atop, awidth, aheight);
end;

procedure TfpgWidget.Invalidate;
begin
  RePaint;
end;


initialization
  FocusRootWidget := nil;
  uLastClickWidget := nil;
  uLastClickTime := 0;

end.

