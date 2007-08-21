unit gfx_widget;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpgfx,
  gfxbase;

type
  TFocusSearchDirection = (fsdFirst, fsdLast, fsdNext, fsdPrev);

  { TfpgWidget }

  TfpgWidget = class(TfpgWindow)
  private
    FAlignRect: TfpgRect;
    FOnDoubleClick: TMouseButtonEvent;
    FOnMouseDown: TMouseButtonEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseExit: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseButtonEvent;
    FOnPaint: TPaintEvent;
    FOnScreen: boolean;
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
    procedure   SetActiveWidget(const AValue: TfpgWidget);
  protected
    FVisible: boolean;
    FEnabled: boolean;
    FFocusable: boolean;
    FFocused: boolean;
    FTabOrder: integer;
    FAnchors: TAnchors;
    FActiveWidget: TfpgWidget;
    FAlign: TAlign;
    function    GetParent: TfpgWidget; reintroduce;
    procedure   SetParent(const AValue: TfpgWidget); reintroduce;
    procedure   SetEnabled(const AValue: boolean); virtual;
    procedure   SetVisible(const AValue: boolean); virtual;
    procedure   DoAlign(aalign: TAlign);
    procedure   HandlePaint; virtual;
    procedure   HandleResize(awidth, aheight: TfpgCoord); virtual;
    procedure   HandleMove(x, y: TfpgCoord); virtual;
    procedure   HandleKeyChar(var AText: string; var shiftstate: TShiftState; var consumed: boolean); virtual;
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
    procedure   HandleAlignments(dwidth, dheight: TfpgCoord); virtual;
    procedure   HandleShow; virtual;
    procedure   HandleHide; virtual;
    procedure   MoveAndResize(aleft, atop, awidth, aheight: TfpgCoord);
    procedure   MoveAndResizeBy(dx, dy, dw, dh: TfpgCoord);
    procedure   RePaint;
    { property events }
    property    OnPaint: TPaintEvent read FOnPaint write FOnPaint;
    property    OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
    property    OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property    OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property    OnMouseDown: TMouseButtonEvent read FOnMouseDown write FOnMouseDown;
    property    OnMouseUp: TMouseButtonEvent read FOnMouseUp write FOnMouseUp;
    property    OnDoubleClick: TMouseButtonEvent read FOnDoubleClick write FOnDoubleClick;
    //property    OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   SetFocus;
    procedure   KillFocus;
    procedure   SetPosition(aleft, atop, awidth, aheight: TfpgCoord);
    property    Parent: TfpgWidget read GetParent write SetParent;
    property    ActiveWidget: TfpgWidget read FActiveWidget write SetActiveWidget;
    property    Visible: boolean read FVisible write SetVisible;
    property    Enabled: boolean read FEnabled write SetEnabled;
    property    TabOrder: integer read FTabOrder write FTabOrder;
    property    Focusable: boolean read FFocusable write FFocusable;
    property    Focused: boolean read FFocused write FFocused;
    property    Anchors: TAnchors read FAnchors write FAnchors;
    property    Align: TAlign read FAlign write FAlign;
  end;


var
  FocusRootWidget: TfpgWidget;


function FindKeyboardFocus: TfpgWidget;

implementation


{ Double click support }
const
  DOUBLECLICK_MS = 320; // the max time between left-clicks for doubleclick
var
  uLastClickWidget: TfpgWidget;
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
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  RePaint;
end;

procedure TfpgWidget.SetActiveWidget(const AValue: TfpgWidget);
begin
  if FActiveWidget = AValue then
    Exit; //==>
  if FActiveWidget <> nil then
    FActiveWidget.HandleKillFocus;
  FActiveWidget := AValue;
  if FActiveWidget <> nil then
    FActiveWidget.HandleSetFocus;
end;

procedure TfpgWidget.SetVisible(const AValue: boolean);
begin
  if FVisible = AValue then
    Exit; //==>
  FVisible := AValue;
  if FOnScreen then
    if FVisible then
      HandleShow
    else
    begin
      HandleHide;
      FOnScreen := True;
    end;
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
  FOnScreen := False;
  FVisible  := True;
  FActiveWidget := nil;
  FEnabled      := True;
  FFocusable := False;
  FFocused   := False;
  FTabOrder  := 0;
  FAnchors := [anLeft, anTop];
  FAlign := alNone;
//  OnKeyPress := nil;

  if (AOwner <> nil) and (AOwner is TfpgWidget) then
    Parent := TfpgWidget(AOwner)
  else
    Parent := nil;

  if Parent <> nil then
    FWindowType := wtChild;

  inherited;
end;

destructor TfpgWidget.Destroy;
begin
  HandleHide;
  inherited;
end;

procedure TfpgWidget.MsgKeyChar(var msg: TfpgMessageRec);
var
  lText: string;
  ss: TShiftState;
  consumed: boolean;
  wg: TfpgWidget;
begin
  lText := msg.params.keyboard.keychar;
  ss  := msg.params.keyboard.shiftstate;

  consumed := False;
  HandleKeyChar(lText, ss, consumed);

  if not consumed then
  begin
    wg := Parent;
    while (not consumed) and (wg <> nil) do
    begin
      wg.HandleKeyChar(lText, ss, consumed);
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
begin
  key := msg.params.keyboard.keycode;
  ss  := msg.params.keyboard.shiftstate;
  consumed := False;

  HandleKeyPress(key, ss, consumed);
  if not consumed then
  begin
    wg := Parent;
    while (not consumed) and (wg <> nil) do
    begin
      wg.HandleKeyPress(key, ss, consumed);
      wg := wg.Parent;
    end;
  end;
end;

procedure TfpgWidget.MsgKeyRelease(var msg: TfpgMessageRec);
var
  key: word;
  ss: TShiftState;
  consumed: boolean;
  wg: TfpgWidget;
begin
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
  if not FEnabled then
    exit;   // Do we want this here?

  case msg.Params.mouse.Buttons of
    MOUSE_LEFT:
      begin
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
  t: DWord;
begin
  if not FEnabled then
    exit;   // Do we want this here?
    
  IsDblClick := False;

  case msg.Params.mouse.Buttons of
    MOUSE_LEFT:
      begin
        mb := mbLeft;
        t := fpgGetTickCount - uLastClickTime;
//        writeln('diff: ', t, '  DoubleClick_MS:', DOUBLECLICK_MS);
        if uLastClickWidget = self then
          IsDblClick := (t) <= DOUBLECLICK_MS   // we detected a double click
        else
          uLastClickWidget := self;
        uLastClickTime := fpgGetTickCount;
//        Writeln('IsDblClick: ', IsDblClick);
        if IsDblClick then
        begin
          HandleDoubleClick(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.Buttons, msg.Params.mouse.shiftstate);
          if Assigned(FOnDoubleClick) then
            FOnDoubleClick(self, mb, msg.Params.mouse.shiftstate,
                Point(msg.Params.mouse.x, msg.Params.mouse.y));
        end
        else
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
  if Assigned(FOnMouseUp) and not IsDblClick then
    FOnMouseUp(self, mb, msg.Params.mouse.shiftstate,
        Point(msg.Params.mouse.x, msg.Params.mouse.y));
end;

procedure TfpgWidget.MsgMouseMove(var msg: TfpgMessageRec);
begin
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
  HandleMouseEnter;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure TfpgWidget.MsgMouseExit(var msg: TfpgMessageRec);
begin
  HandleMouseExit;
  if Assigned(FOnMouseExit) then
    FOnMouseExit(Self);
end;

procedure TfpgWidget.MsgMouseScroll(var msg: TfpgMessageRec);
begin
  HandleMouseScroll(msg.Params.mouse.x, msg.Params.mouse.y,
      msg.Params.mouse.shiftstate, msg.Params.mouse.delta);
end;

procedure TfpgWidget.HandleShow;
var
  n: integer;
  c: TComponent;
begin
  FOnScreen := True;
  if FVisible then
  begin
    AllocateWindowHandle;
    DoSetWindowVisible(True);
    
    for n := 0 to ComponentCount - 1 do
    begin
      c := Components[n];
      if (c is TfpgWidget) and (TfpgWidget(c).Parent = self) then
        TfpgWidget(c).HandleShow;
    end;
  end;
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

  ReleaseWindowHandle;
end;

procedure TfpgWidget.RePaint;
begin
  if HasHandle then
    HandlePaint;
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

procedure TfpgWidget.HandleKeyChar(var AText: string; var shiftstate: TShiftState; var consumed: boolean);
begin
  // descendants will implement this.
end;

procedure TfpgWidget.HandleKeyPress(var keycode: word; var shiftstate: TShiftState;
  var consumed: boolean);
var
  wg: TfpgWidget;
  dir: integer;
begin
  //if Assigned(OnKeyPress) then
    //OnKeyPress(self, keycode, shiftstate, consumed);

  if consumed then
    Exit; //==>

  dir := 0;

  case keycode of
    keyTab:
        if (ssShift in shiftstate) then
          dir := -1
        else
          dir := 1;

    keyReturn,
    keyDown,
    keyRight:
        dir := 1;

    keyUp,
    keyLeft:
        dir := -1;
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
  if not FFocused then
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
end;

procedure TfpgWidget.HandleKillFocus;
begin
  FFocused := False;
  RePaint;

  if ActiveWidget <> nil then
    ActiveWidget.KillFocus;
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
  // do nothing yet
end;

procedure TfpgWidget.HandleRMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  // do nothing yet
end;

procedure TfpgWidget.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
begin
  // do nothing yet
end;

procedure TfpgWidget.HandleDoubleClick(x, y: integer; button: word; shiftstate: TShiftState);
begin
  // do nothing yet
end;

procedure TfpgWidget.HandleMouseEnter;
begin
  // do nothing yet
end;

procedure TfpgWidget.HandleMouseExit;
begin
  // do nothing yet
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
    lasttaborder := -999999
  else
    lasttaborder := 999999;

  for n := 0 to ComponentCount - 1 do
    if Components[n] is TfpgWidget then
    begin
      w := TfpgWidget(Components[n]);

      if w.Visible and w.Enabled and w.Focusable then
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
              if (startwg = nil) or
                (w.TabOrder > startwg.TabOrder) or
                (FoundIt and (w.TabOrder = startwg.TabOrder)) then
              begin
                Result       := w;
                lasttaborder := w.TabOrder;
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

        end;
    end;
end;

procedure TfpgWidget.MsgPaint(var msg: TfpgMessageRec);
begin
  HandlePaint;
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure TfpgWidget.MsgResize(var msg: TfpgMessageRec);
begin
  HandleResize(msg.Params.rect.Width, msg.Params.rect.Height);
end;

procedure TfpgWidget.HandleResize(awidth, aheight: TfpgCoord);
var
  dw: integer;
  dh: integer;
begin
  dw := awidth - FWidth;
  dh := aheight - FHeight;
  FWidth  := awidth;
  FHeight := aheight;
  HandleAlignments(dw, dh);
end;

procedure TfpgWidget.MsgMove(var msg: TfpgMessageRec);
begin
  HandleMove(msg.Params.rect.left, msg.Params.rect.top);
end;

procedure TfpgWidget.HandleMove(x, y: TfpgCoord);
begin
  FLeft := x;
  FTop  := y;
end;

procedure TfpgWidget.HandleAlignments(dwidth, dheight: TfpgCoord);
var
  n: integer;
  wg: TfpgWidget;
  dx: integer;
  dy: integer;
  dw: integer;
  dh: integer;
begin
  FAlignRect.Top    := 0;
  FAlignRect.Left   := 0;
  FAlignRect.Width  := Width;
  FAlignRect.Height := Height;

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
        (anBottom in wg.Anchors) or (anRight in wg.Anchors) then
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
    end;

  RePaint;
end;

procedure TfpgWidget.MoveAndResize(aleft, atop, awidth, aheight: TfpgCoord);
begin
  if (aleft <> FLeft) or (atop <> FTop) then
    HandleMove(aleft, atop);
  if (awidth <> FWidth) or (aheight <> FHeight) then
    HandleResize(awidth, aheight);

  UpdateWindowPosition;
end;

procedure TfpgWidget.MoveAndResizeBy(dx, dy, dw, dh: TfpgCoord);
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
    alTop: Result := CompareInts(w1.Top, w2.Top);
    alBottom: Result := CompareInts(w2.Top, w1.Top);
    alLeft: Result   := CompareInts(w1.Left, w2.Left);
    alRight: Result  := CompareInts(w2.Left, w1.Left);
    else
      Result         := 0;
  end;
end;

procedure TfpgWidget.DoAlign(aalign: TAlign);
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
      if w.Align = aalign then
        alist.Add(w);
    end;

  alist.Sort(@AlignCompare);

  // and process this list in order
  for n := 0 to alist.Count - 1 do
  begin
    w := TfpgWidget(alist[n]);
    case aalign of
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

procedure TfpgWidget.SetPosition(aleft, atop, awidth, aheight: TfpgCoord);
begin
  MoveAndResize(aleft, atop, awidth, aheight);
end;


initialization
  FocusRootWidget := nil;
  uLastClickWidget := nil;
  uLastClickTime := 0;

end.

