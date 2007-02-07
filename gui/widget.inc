{
    fpGUI  -  Free Pascal GUI Library
    
    Basic events and Widget class declarations
    
    Copyright (C) 2000 - 2006 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{%mainunit fpgui.pp}

{$IFDEF read_interface}

{ Basic events and widget declarations }


// -------------------------------------------------------------------
//   Basic event objects
// -------------------------------------------------------------------

  {$M+}
  { Basic/Abstract event object which has RTTI enabled. This object does some
    kind of reference counting as well. }
  TEventObj = class
  private
    RefCount: LongInt;
    FSender: TObject;
  public
    constructor Create(ASender: TObject);
    procedure   AddRef;
    procedure   Release;
    function    SendToChild(AChild: TWidget): Boolean; virtual;
  published
    property    Sender: TObject read FSender write FSender;
  end;
  {$M-}

  TEventClass = class of TEventObj;

  // Miscellaneous events

  TLayoutingEventObj = class(TEventObj);

  TCalcSizesEventObj = class(TLayoutingEventObj);

  TResizedEventObj = class(TLayoutingEventObj)
  private
    FIsForced: Boolean;
  public
    constructor Create(ASender: TObject; AIsForced: Boolean);
  published
    property IsForced: Boolean read FIsForced write FIsForced;
  end;


  TDestroyEventObj          = class(TEventObj);
  TEnabledChangeEventObj    = class(TEventObj);
  TVisibilityChangeEventObj = class(TEventObj);


  TPaintingEventObj = class(TEventObj)
  private
    FCanvas: TFCanvas;
  public
    constructor Create(ASender: TObject; ACanvas: TFCanvas);
  published
    property    Canvas: TFCanvas read FCanvas write FCanvas;
  end;


  TPreparePaintEventObj = class(TPaintingEventObj)
  public
    // To prevent invisible children to be processed:
    function SendToChild(AChild: TWidget): Boolean; override;
  end;


  TPaintEventObj = class(TPaintingEventObj)
  public
    // To adapt the clipping region and add a translation:
    function SendToChild(AChild: TWidget): Boolean; override;
  end;


  // Mouse events

  TMouseEventObj = class(TEventObj)
  private
    FShift: TShiftState;
    FPosition: TPoint;
  public
    constructor Create(ASender: TObject; AShift: TShiftState; APosition: TPoint);
    function SendToChild(AChild: TWidget): Boolean; override;
    property Position: TPoint read FPosition write FPosition;
  published
    property Shift: TShiftState read FShift write FShift;
  end;


  TMouseButtonEventObj = class(TMouseEventObj)
  private
    FButton: TMouseButton;
  public
    constructor Create(ASender: TObject; AButton: TMouseButton;
      AShift: TShiftState; APosition: TPoint);
  published
    property Button: TMouseButton read FButton write FButton;
  end;


  TMousePressedEventObj = class(TMouseButtonEventObj);
  TMouseReleasedEventObj = class(TMouseButtonEventObj);


  TMouseMoveEventObj = class(TMouseEventObj)
  public
    function SendToChild(AChild: TWidget): Boolean; override;
  end;


  TMouseEnterEventObj = class(TMouseEventObj)
  public
    NewCursor: TFCursor;
  end;


  TMouseLeaveEventObj = class(TEventObj)
  public
    function SendToChild(AChild: TWidget): Boolean; override;
  end;


  TMouseLeaveCheckEventObj = class(TMouseEventObj)
  public
    function SendToChild(AChild: TWidget): Boolean; override;
    NewCursor: TFCursor;
  end;


  TMouseWheelEventObj = class(TMouseEventObj)
  private
    FWheelDelta: Single;
  public
    constructor Create(ASender: TObject; AShift: TShiftState;
      AWheelDelta: Single; APosition: TPoint);
  published
    property WheelDelta: Single read FWheelDelta write FWheelDelta;
  end;


// -------------------------------------------------------------------
//   TWidget
// -------------------------------------------------------------------

  TWidgetStyle = set of (wsCaptureMouse, wsClickable, wsOpaque);

  TContainerWidget = class;


  TWidget = class(TComponent)
  private
    FColor: TColor;
    FParent: TWidget;
    FOnClick: TNotifyEvent;
    // Property access
    procedure SetColor(const AValue: TColor);
    procedure SetParent(AParent: TWidget);
    function  GetBoundsRect: TRect;
    function  GetLeft: Integer;
    function  GetTop: Integer;
    function  GetWidth: Integer;
    function  GetHeight: Integer;
    procedure SetEnabled(AEnabled: Boolean);
    procedure SetStyle(const AValue: TStyle);
    procedure SetVisible(AVisible: Boolean);
    // Event handling
    function  EvCalcSizes(Event: TCalcSizesEventObj): Boolean;
    function  EvEnabledChange(Event: TEnabledChangeEventObj): Boolean;
    function  EvVisibilityChange(Event: TVisibilityChangeEventObj): Boolean;
    function  EvMousePressed(Event: TMousePressedEventObj): Boolean;
    function  EvMouseReleased(Event: TMouseReleasedEventObj): Boolean;
    function  EvMouseEnter(Event: TMouseEnterEventObj): Boolean;
    function  EvMouseLeave(Event: TMouseLeaveEventObj): Boolean;
    function  EvMouseLeaveCheck(Event: TMouseLeaveCheckEventObj): Boolean;
  protected
    FCursor: TFCursor;
    FText: string;
    FStyle: TStyle;
    FCanExpandHeight: Boolean;
    FCanExpandWidth: Boolean;
    FEnabled: Boolean;
    FVisible: Boolean;
    FOrigin: TPoint;
    FBoundsSize: TSize;
    FClientRect: TRect;
    FMinSize: TSize;
    FMaxSize: TSize;
    FDefSize: TSize;
    WidgetStyle: TWidgetStyle;
    WidgetState: TWidgetState;
    procedure Loaded; override;
    procedure Click; dynamic;
    procedure Paint(Canvas: TFCanvas); virtual;
    procedure SetParentComponent(AParent: TComponent); override;

    // Layouting
    procedure CalcSizes; virtual; abstract;
    procedure Resized; virtual;

    // Events
    function  ProcessEvent(Event: TEventObj): Boolean; virtual;
    function  DistributeEvent(Event: TEventObj): Boolean; virtual;
    procedure EvFocusChanged; dynamic;		// Widget got or lost focus
    procedure EvKeyPressed(Key: Word; Shift: TShiftState); dynamic;
    procedure EvKeyReleased(Key: Word; Shift: TShiftState); dynamic;
    procedure EvKeyChar(KeyChar: Char); dynamic;
    procedure EvTextChanged; dynamic;
    function  DoMouseEnter(AShift: TShiftState; AMousePos: TPoint): Boolean;

    // Properties
    function  GetStyle: TStyle;
    procedure SetCanExpandWidth(allow: Boolean);
    procedure SetCanExpandHeight(allow: Boolean);
    procedure SetText(const AText: String); virtual;
    property  CanExpandWidth: Boolean read FCanExpandWidth write SetCanExpandWidth default False;
    property  CanExpandHeight: Boolean read FCanExpandHeight write SetCanExpandHeight default False;
    property  Cursor: TFCursor read FCursor write FCursor default crDefault;
    property  Text: String read FText write SetText;
    property  Color: TColor read FColor write SetColor default cl3DFace;
    property  OnClick: TNotifyEvent read FOnClick write FOnClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    SendEvent(Event: TEventObj): Boolean;
    function    FindForm: TCustomForm;
    procedure   SetEmbeddedParent(AParent: TWidget);
    procedure   SetBounds(APosition: TPoint; ASize: TSize);
    procedure   SetBounds(x, y, w, h: Integer); overload;
    function    WidgetToClient(const APoint: TPoint): TPoint; virtual;
    function    ClientToWidget(const APoint: TPoint): TPoint; virtual;
    function    ClientToScreen(const APoint: TPoint): TPoint; virtual;
    procedure   Show; dynamic;
    procedure   Hide; dynamic;
    procedure   Redraw;
    procedure   Redraw(const ARect: TRect);
    procedure   Scroll(const ARect: TRect; DeltaX, DeltaY: Integer);
    procedure   SetFocus;
    procedure   Update;
    property    Parent: TWidget read FParent write SetParent;
    property    Origin: TPoint read FOrigin;
    property    BoundsSize: TSize read FBoundsSize;
    property    BoundsRect: TRect read GetBoundsRect;
    property    Left: Integer read GetLeft;
    property    Top: Integer read GetTop;
    property    Width: Integer read GetWidth;
    property    Height: Integer read GetHeight;
    property    MinSize: TSize read FMinSize;
    property    MaxSize: TSize read FMaxSize;
    property    DefSize: TSize read FDefSize;
    property    ClientRect: TRect read FClientRect;
    property    Style: TStyle read GetStyle write SetStyle;
    property    Enabled: Boolean read FEnabled write SetEnabled default True;
    property    Visible: Boolean read FVisible write SetVisible default True;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}

// -------------------------------------------------------------------
//   TEventObj
// -------------------------------------------------------------------

constructor TEventObj.Create(ASender: TObject);
begin
  Sender := ASender;
  RefCount := 1;
end;


procedure TEventObj.AddRef;
begin
  Inc(RefCount);
end;


procedure TEventObj.Release;
begin
  ASSERT(RefCount > 0);
  Dec(RefCount);
  if RefCount = 0 then
    Self.Free;
end;


function TEventObj.SendToChild(AChild: TWidget): Boolean;
begin
  Inc(RefCount);
  Result := AChild.SendEvent(Self);
end;


// -------------------------------------------------------------------
//   Miscellaneous events
// -------------------------------------------------------------------

constructor TResizedEventObj.Create(ASender: TObject; AIsForced: Boolean);
begin
  inherited Create(ASender);
  FIsForced := AIsForced;
end;


function TPreparePaintEventObj.SendToChild(AChild: TWidget): Boolean;
begin
  if wsIsVisible in AChild.WidgetState then
    Result := inherited SendToChild(AChild)
  else
    Result := False;
end;


constructor TPaintingEventObj.Create(ASender: TObject; ACanvas: TFCanvas);
begin
  inherited Create(ASender);
  FCanvas := ACanvas;
end;


function TPaintEventObj.SendToChild(AChild: TWidget): Boolean;
var
  ClientPos: TPoint;
begin
  if wsIsVisible in AChild.WidgetState then
  begin
    Canvas.SaveState;
    try
      if Canvas.IntersectClipRect(AChild.BoundsRect) then
      begin
        ClientPos := AChild.ClientToWidget(AChild.Origin);
        Canvas.AppendTranslation(ClientPos);
        Result := inherited SendToChild(AChild);
      end else
        Result := False;
    finally
      Canvas.RestoreState;
    end;
  end else
    Result := False;
end;


// -------------------------------------------------------------------
//   Mouse events
// -------------------------------------------------------------------

constructor TMouseEventObj.Create(ASender: TObject; AShift: TShiftState;
  APosition: TPoint);
begin
  inherited Create(ASender);
  Shift     := AShift;
  Position  := APosition;
end;


function TMouseEventObj.SendToChild(AChild: TWidget): Boolean;
var
  OldPos, ClientPos: TPoint;
begin
  if (AChild.WidgetState * [wsEnabled, wsIsVisible] = [wsEnabled, wsIsVisible])
    and ((AChild = AChild.FindForm.MouseCaptureWidget) or
    PtInRect(AChild.BoundsRect, Position)) then
  begin
    // Store the old values, as they might get modified during AChild.SendEvent!
    OldPos    := Position;
    ClientPos := AChild.WidgetToClient(Position);
    Position  := ClientPos - AChild.Origin;
    Result    := inherited SendToChild(AChild);
    Position  := OldPos;
  end
  else
    Result := False;
end;


constructor TMouseButtonEventObj.Create(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; APosition: TPoint);
begin
  inherited Create(ASender, AShift, APosition);
  Button := AButton;
end;


function TMouseMoveEventObj.SendToChild(AChild: TWidget): Boolean;
begin
  if AChild.WidgetState * [wsEnabled, wsIsVisible] = [wsEnabled, wsIsVisible]
    then
  begin
    if PtInRect(AChild.BoundsRect, Position) and not
      (wsMouseInside in AChild.WidgetState) then
      AChild.DoMouseEnter(Shift, Position - AChild.Origin);

    Result := inherited SendToChild(AChild);
  end else
    Result := False;
end;


function TMouseLeaveEventObj.SendToChild(AChild: TWidget): Boolean;
begin
  if wsMouseInside in AChild.WidgetState then
    Result := inherited SendToChild(AChild)
  else
    Result := False;
end;


function TMouseLeaveCheckEventObj.SendToChild(AChild: TWidget): Boolean;
begin
  if ((FPosition.X < AChild.Left) or (FPosition.Y < AChild.Top) or
    (FPosition.X >= AChild.Left + AChild.Width) or
    (FPosition.Y >= AChild.Top + AChild.Height)) and
      (wsMouseInside in AChild.WidgetState) then
    AChild.SendEvent(TMouseLeaveEventObj.Create(Self));

  Result := inherited SendToChild(AChild);
end;


constructor TMouseWheelEventObj.Create(ASender: TObject; AShift: TShiftState;
  AWheelDelta: Single; APosition: TPoint);
begin
  inherited Create(ASender, AShift, APosition);
  WheelDelta := AWheelDelta;
end;


// ===================================================================
//   TWidget
// ===================================================================

constructor TWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := [];
  WidgetState := [wsEnabled];
  FCanExpandWidth   := False;
  FCanExpandHeight  := False;
  FEnabled          := True;
  FVisible          := True;
  FColor            := cl3DFace;
end;

destructor TWidget.Destroy;
begin
  inherited Destroy;
end;

function TWidget.SendEvent(Event: TEventObj): Boolean;
{$IFDEF TraceEvents}
var
  i: Integer;
{$ENDIF}
begin
{$IFDEF TraceEvents}
  for i := 1 to EventNestingLevel do
    Write('  ');
  WriteLn(Event.ClassName, ' event for ', Name, ':', ClassName);
  Inc(EventNestingLevel);
{$ENDIF}

  Result := ProcessEvent(Event);

{$IFDEF TraceEvents}
  Dec(EventNestingLevel);
  for i := 1 to EventNestingLevel do
    Write('  ');
  if Result then
    WriteLn(Event.ClassName, ' event has been blocked.');
{$ENDIF}
  Event.Release;
end;

function TWidget.FindForm: TCustomForm;
var
  Widget: TWidget;
begin
  Widget := Self;
  while not (Widget is TCustomForm) do
  begin
    Widget := Widget.Parent;
    if not Assigned(Widget) then
    begin
      Result := nil;
      exit;
    end;
  end;
  Result := TCustomForm(Widget);
end;

procedure TWidget.SetEmbeddedParent(AParent: TWidget);
begin
  FParent := AParent;
end;

procedure TWidget.SetBounds(APosition: TPoint; ASize: TSize);
begin
  LAYOUTTRACE('TWidget.SetBounds for %s:%s. Old size: %dx%d, new size: %dx%d',
    [Name, ClassName, BoundsSize.cx, BoundsSize.cy, ASize.cx, ASize.cy]);
  FOrigin := APosition;
  if ASize <> BoundsSize then
  begin
    FBoundsSize := ASize;
    FClientRect := Rect(0, 0, BoundsSize.cx, BoundsSize.cy);
    SendEvent(TResizedEventObj.Create(Self, wsSizeIsForced in WidgetState));
  end;
end;

procedure TWidget.SetBounds(x, y, w, h: Integer);
begin
  SetBounds(Point(x, y), Size(w, h));
end;

function TWidget.WidgetToClient(const APoint: TPoint): TPoint;
begin
  // as default, the outer rectangle is identical to the client rectangle
  Result := APoint;
end;

function TWidget.ClientToWidget(const APoint: TPoint): TPoint;
begin
  // as default, the outer rectangle is identical to the client rectangle
  Result := APoint;
end;

function TWidget.ClientToScreen(const APoint: TPoint): TPoint;
begin
  Result := APoint + Origin;
  Result := ClientToWidget(Result);
  if Assigned(Parent) then
    Result := Parent.ClientToScreen(Result);
end;

{procedure TWidget.Update;
begin
  LAYOUTTRACE('TWidget.Update for %s:%s', [Name, ClassName]);
  if wsIsUpdating in WidgetState then
    exit;

  Include(WidgetState, wsIsUpdating);
  SendEvent(TUpdateEventObj.Create(Self));
  Exclude(WidgetState, wsIsUpdating);
end;

procedure TWidget.RecalcLayout;
var
  OldW, OldH: Integer;
  x, y: Integer;
  Widget: TWidget;
begin
  if (csLoading in ComponentState) or (not Visible) then
    exit;

  LAYOUTTRACE('TWidget.RecalcLayout for %s:%s', [Name, ClassName]);

  OldW := Width;
  OldH := Height;

  MinW := 0;
  MinH := 0;
  DefW := 0;
  DefH := 0;
  MaxW := InfiniteSize;
  MaxH := InfiniteSize;

  EvRecalcLayout;

  if MinW = 0 then MinW := 1;
  if MinH = 0 then MinH := 1;
  if DefW < MinW then DefW := MinW;
  if DefH < MinH then DefH := MinH;

  if (not FCanExpandWidth) or (MaxW < DefW) then
    MaxW := DefW;
  if (not FCanExpandHeight) or (MaxH < DefH) then
    MaxH := DefH;

  if (DefW < OldW) or (DefH < OldH) or ((not (wsOpaque in WidgetStyle)) and
    ((DefW > OldW) or (DefH > OldH))) then
  begin
    x := 0;
    y := 0;
    Widget := Self;
    while not (Widget is TCustomForm) do
    begin
      Inc(x, Widget.Left);
      Inc(y, Widget.Top);
      Widget := Widget.parent;
    end;
    TCustomForm(Widget).Wnd.Invalidate(Rect(x, y,
      x + Max(Width, OldW), y + Max(Height, OldH)));
  end;

  if Assigned(Parent) and not (wsIsUpdating in Parent.WidgetState) then
    Parent.RecalcLayout;
end;}

procedure TWidget.Show;
begin
  if not Visible then
  begin
    LAYOUTTRACE('TWidget.Show for %s:%s', [Name, ClassName]);
    FVisible := True;
    if Assigned(Parent) and (wsIsVisible in Parent.WidgetState) then
    begin
      SendEvent(TVisibilityChangeEventObj.Create(Self));
      Parent.Update;
    end;
  end;
end;

procedure TWidget.Hide;
begin
  if Visible then
  begin
    LAYOUTTRACE('TWidget.Hide for %s:%s', [Name, ClassName]);
    FVisible := False;
    if wsIsVisible in WidgetState then
    begin
      SendEvent(TVisibilityChangeEventObj.Create(Self));
      if Assigned(Parent) then
        Parent.Update;
    end;
  end;
end;

procedure TWidget.Redraw;
begin
  Redraw(Rect(0, 0, BoundsSize.cx, BoundsSize.cy));
  {$IFDEF DEBUG}
  writeln(ClassName + '.Redraw');
  {$ENDIF}
end;

procedure TWidget.Redraw(const ARect: TRect);
var
  x, y: Integer;
  Form: TCustomForm;
  WidgetPos, FormPos: TPoint;
begin
  if not (wsIsVisible in WidgetState) then
    exit;

  Form := FindForm;
  WidgetPos := ClientToScreen(Point(0, 0));
  FormPos := Form.ClientToScreen(Point(0, 0));

  x := ARect.Left + WidgetPos.x - FormPos.x;
  y := ARect.Top + WidgetPos.y - FormPos.y;
  Form.Wnd.Invalidate(
    Rect(x, y, x + ARect.Right - ARect.Left, y + ARect.Bottom - ARect.Top));
end;

procedure TWidget.Scroll(const ARect: TRect; DeltaX, DeltaY: Integer);
var
  r, ClipRect: TRect;
  Widget: TWidget;
  Form: TCustomForm;
  Canvas: TFCanvas;
begin
  if not (wsIsVisible in WidgetState) then
    exit;

  Form := FindForm;

  { !!!: Better do real DirtyList correction, which might improve performance
    a lot in some situations }
  Form.Wnd.PaintInvalidRegion;

  r.Left  := ARect.Left;
  r.Top   := ARect.Top;
  Widget  := Self;
  Canvas  := TFCanvas(Form.Wnd.Canvas);
  Canvas.SaveState;
  try
    while Widget <> Form do
    begin
      ClipRect.TopLeft := Form.WidgetCoords(Widget);
      ClipRect.BottomRight := ClipRect.TopLeft + Widget.BoundsSize;
      Canvas.IntersectClipRect(ClipRect);
      r.TopLeft := r.TopLeft + Widget.Origin +
        Widget.ClientToWidget(Point(0, 0));
      Widget := Widget.Parent;
    end;

    r.Right   := r.Left + ARect.Right - ARect.Left;
    r.Bottom  := r.Top + ARect.Bottom - ARect.Top;

    ClipRect  := Canvas.GetClipRect;

    { Perform a quick clipping against the ClipRect - this might reduce
      the number of pixels which get copied }
    if r.Left < ClipRect.Left then
      r.Left := ClipRect.Left;
    if r.Left + DeltaX < ClipRect.Left then
      r.Left := ClipRect.Left - DeltaX;
    if r.Top < ClipRect.Top then
      r.Top := ClipRect.Top;
    if r.Top + DeltaY < ClipRect.Top then
      r.Top := ClipRect.Top - DeltaY;
    if r.Right > ClipRect.Right then
      r.Right := ClipRect.Right;
    if r.Right + DeltaX > ClipRect.Right then
      r.Right := ClipRect.Right - DeltaX;
    if r.Bottom > ClipRect.Bottom then
      r.Bottom := ClipRect.Bottom;
    if r.Bottom + DeltaY > ClipRect.Bottom then
      r.Bottom := ClipRect.Bottom - DeltaY;

    Canvas.CopyRect(Canvas, r, r.TopLeft + Point(DeltaX, DeltaY));
  finally
    Canvas.RestoreState;
  end;

  // Redraw the areas which has been scrolled in
  with Form.WidgetCoords(Self) do
  begin
    Dec(ClipRect.Left, x);
    Dec(ClipRect.Top, y);
    Dec(ClipRect.Right, x);
    Dec(ClipRect.Bottom, y);
  end;

  if DeltaX <> 0 then
  begin
    r := ARect;
    if DeltaX < 0 then		// Scrolling to the left size
    begin
      r.Left := r.Right + DeltaX;
      if r.Right > ClipRect.Right then
      begin
        Dec(r.Left, r.Right - ClipRect.Right);
	      r.Right := ClipRect.Right;
      end;
    end else			// Scrolling to the right size
    begin
      r.Right := r.Left + DeltaX;
      if r.Left < ClipRect.Left then
      begin
        Inc(r.Right, ClipRect.Left - r.Left);
	      r.Left := ClipRect.Left;
      end;
    end;
    Redraw(r);
  end;

  if DeltaY <> 0 then
  begin
    r := ARect;
    if DeltaY < 0 then		// Scrolling upwards
    begin
      r.Top := r.Bottom + DeltaY;
      if r.Bottom > ClipRect.Bottom then
      begin
        Dec(r.Top, r.Bottom - ClipRect.Bottom);
	      r.Bottom := ClipRect.Bottom;
      end;
    end else			// Scrolling downwards
    begin
      r.Bottom := r.Top + DeltaY;
      if r.Top < ClipRect.Top then
      begin
        Inc(r.Bottom, ClipRect.Top - r.Top);
	      r.Top := ClipRect.Top;
      end;
    end;
    Redraw(r);
  end;
end;

procedure TWidget.SetFocus;
begin
  FindForm.FocusedWidget := Self;
end;

procedure TWidget.Update;
var
  PropagateUpdate: Boolean;
  OldMinSize, OldMaxSize, OldDefSize: TSize;
begin
  if not (wsIsVisible in WidgetState) then
    exit;

  if wsSizeIsForced in WidgetState then
    PropagateUpdate := True
  else
  begin
    OldMinSize := MinSize;
    OldMaxSize := MaxSize;
    OldDefSize := DefSize;
    SendEvent(TCalcSizesEventObj.Create(Self));
    PropagateUpdate := (OldMinSize <> MinSize) or (OldMaxSize <> MaxSize) or
      (OldDefSize <> DefSize);
  end;

  if PropagateUpdate and Assigned(Parent) then
    Parent.Update
  else
    SendEvent(TResizedEventObj.Create(Self, wsSizeIsForced in WidgetState));
end;

procedure TWidget.Loaded;
begin
  inherited Loaded;
{!!!:  if not (wsEnabled in WidgetState) then
    UpdateEnabledState;}
end;

procedure TWidget.Click;
begin
  Redraw;
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TWidget.Paint(Canvas: TFCanvas);
begin
  Canvas.SetColor(Style.GetUIColor(FColor));
end;

procedure TWidget.SetParentComponent(AParent: TComponent);
begin
  if AParent is TContainerWidget then
    SetParent(TContainerWidget(AParent));
end;

procedure TWidget.SetEnabled(AEnabled: Boolean);
begin
  if AEnabled <> Enabled then
  begin
    FEnabled := AEnabled;
    SendEvent(TEnabledChangeEventObj.Create(Self));
  end;
end;

procedure TWidget.SetStyle(const AValue: TStyle);
begin
  FStyle := AValue;
end;

procedure TWidget.SetVisible(AVisible: Boolean);
begin
  if AVisible then
    Show
  else
    Hide;
end;

procedure TWidget.Resized;
begin
  // Do nothing by default
end;

procedure TWidget.EvFocusChanged;
begin
  Redraw;
end;

procedure TWidget.EvKeyPressed(Key: Word; Shift: TShiftState);

  function SetFocusIfPossible(Widget: TWidget): Boolean;
  begin
    Result := (wsClickable in Widget.WidgetStyle) and
      (wsEnabled in Widget.WidgetState);
    if Result then
      Widget.SetFocus;
  end;

  function FocusFirst(Widget: TWidget): Boolean;
  var
    i: Integer;
    Child: TWidget;
  begin
    Result := False;
    if not Widget.InheritsFrom(TContainerWidget) then
      exit;

    for i := 0 to TContainerWidget(Widget).ChildCount - 1 do
    begin
      Child := TContainerWidget(Widget).Children[i];
      if SetFocusIfPossible(Child) or FocusFirst(Child) then
      begin
        Result := True;
	      exit;
      end;
    end;
  end;

  function FocusLast(Widget: TWidget): Boolean;
  var
    i: Integer;
    Child: TWidget;
  begin
    Result := False;
    if not Widget.InheritsFrom(TContainerWidget) then
      exit;

    for i := TContainerWidget(Widget).ChildCount - 1 downto 0 do
    begin
      Child := TContainerWidget(Widget).Children[i];
      if SetFocusIfPossible(Child) or FocusLast(Child) then
      begin
        Result := True;
	      exit;
      end;
    end;
  end;

var
  mshift: TShiftState;
  i, j: Integer;
  CurWidget: TWidget;
  CurParent: TContainerWidget;
begin
  // Handle focus movement keys
  mshift := Shift * [ssShift, ssAlt, ssCtrl, ssMeta, ssSuper, ssHyper, ssAltGr];
  if ((mshift = []) and ((Key = keyLeft) or (Key = keyUp))) or
    ((mshift = [ssShift]) and (Key = keyTab)) then
  begin
    if Assigned(Parent) then
    begin
      CurWidget := Self;
      CurParent := TContainerWidget(Parent);
      while Assigned(CurParent) and CurParent.InheritsFrom(TContainerWidget) do
      begin
        for i := CurParent.ChildCount - 1 downto 1 do
        begin
      	  if CurParent.Children[i] = CurWidget then
      	  begin
            j :=  i - 1;
            //Dec(i);
      	    while j >= 0 do
      	    begin
      	      if SetFocusIfPossible(CurParent.Children[j]) or
        	        FocusLast(CurParent.Children[j]) then
            		exit;
    	        Dec(j);
	          end;
	          break;
	        end;  { if }
        end;  { for }
	      CurParent := TContainerWidget(CurParent.Parent);
      end;  { while }
    end
    else
    begin
      if FocusLast(Self) then
        exit;
    end;
  end
  else
  begin
    if (mshift = []) and
      ((Key = keyRight) or (Key = keyDown) or (Key = keyTab)) then
    begin
      if Assigned(Parent) then
      begin
        CurWidget := Self;
        CurParent := TContainerWidget(Parent);
        while Assigned(CurParent) and CurParent.InheritsFrom(TContainerWidget) do
        begin
          for i := 0 to CurParent.ChildCount - 2 do
          begin
	          if CurParent.Children[i] = CurWidget then
	          begin
              j := i;
	            Inc(j);
	            while j < CurParent.ChildCount do
	            begin
	              if SetFocusIfPossible(CurParent.Children[j]) or
	                  FocusFirst(CurParent.Children[j]) then
		              exit;
	              Inc(j);
	            end;
	            break;
	          end;  { if }
          end;  { for }
	        CurParent := TContainerWidget(CurParent.Parent);
        end;  { while }
      end
      else
      begin
        if FocusFirst(Self) then
        exit;
      end;
    end
    else if (Key = Ord(' ')) and (wsClickable in WidgetStyle) then
    begin
      Click;
      exit;
    end;
  end;
  
  if Assigned(Parent) then
    Parent.EvKeyPressed(Key, Shift);
end;

procedure TWidget.EvKeyReleased(Key: Word; Shift: TShiftState);
begin
  if Assigned(Parent) then
    Parent.EvKeyReleased(Key, Shift);
end;

procedure TWidget.EvKeyChar(KeyChar: Char);
begin
  if Assigned(Parent) then
    Parent.EvKeyChar(KeyChar);
end;

procedure TWidget.EvTextChanged;
begin
  Update;
  Redraw;
end;

function TWidget.DoMouseEnter(AShift: TShiftState; AMousePos: TPoint): Boolean;
var
  Event: TMouseEnterEventObj;
begin
  Event := TMouseEnterEventObj.Create(Self, AShift, AMousePos);
  Event.AddRef;
  Result := SendEvent(Event);
  if Event.NewCursor <> crDefault then
    FindForm.Wnd.Cursor := Event.NewCursor;
  Event.Free;
end;

function TWidget.GetStyle: TStyle;
var
  Widget: TWidget;
begin
  if Assigned(FStyle) then
    Result := FStyle
  else
  begin
    ASSERT(Assigned(Parent));
    // Don't use recursive calls here, they are not necessary
    Widget := Parent;
    while not Assigned(Widget.FStyle) do
    begin
      Widget := Widget.Parent;
      ASSERT(Assigned(Widget));
    end;
    Result := Widget.FStyle;
  end;
end;

function TWidget.ProcessEvent(Event: TEventObj): Boolean;
var
  Canvas: TFCanvas;
  Matrix: TGfxMatrix;
  ClientPos: TPoint;
  PreparationEvent: TPreparePaintEventObj;
begin
  // Handle events which must be processed before the children get them
  if Event.InheritsFrom(TResizedEventObj) then
  begin
    if TResizedEventObj(Event).IsForced then
      Include(WidgetState, wsSizeIsForced)
    else
      Exclude(WidgetState, wsSizeIsForced);
    Resized;
    Result := DistributeEvent(Event);
  end
  else if Event.InheritsFrom(TEnabledChangeEventObj) then
    Result := EvEnabledChange(TEnabledChangeEventObj(Event))
  else if Event.InheritsFrom(TVisibilityChangeEventObj) then
    Result := EvVisibilityChange(TVisibilityChangeEventObj(Event)) or DistributeEvent(Event)
  else if Event.InheritsFrom(TMouseEnterEventObj) then
    Result := EvMouseEnter(TMouseEnterEventObj(Event)) or DistributeEvent(Event)
  else if Event.InheritsFrom(TPreparePaintEventObj) then
  begin
    Canvas := TPreparePaintEventObj(Event).Canvas;
    if wsOpaque in WidgetStyle then
    begin
      Canvas.ExcludeClipRect(BoundsRect);
      Result := False;
    end
    else
    begin
      Matrix        := Canvas.Matrix;
      ClientPos     := ClientToWidget(Origin);
      Canvas.AppendTranslation(ClientPos);
      Result        := DistributeEvent(Event);
      Canvas.Matrix := Matrix;
    end;
  end
  else if Event.InheritsFrom(TPaintEventObj) then
  begin
    Canvas := TPaintEventObj(Event).Canvas;
    Canvas.SaveState;
    PreparationEvent := TPreparePaintEventObj.Create(Self, Canvas);
    DistributeEvent(PreparationEvent);
    PreparationEvent.Release;
    Paint(Canvas);
    Canvas.RestoreState;
    Result := DistributeEvent(Event);
  end
  else
  begin
    // First distribute the event to all children, then try to handle them here
    Result := DistributeEvent(Event);
    if not Result then
    begin
      if Event.InheritsFrom(TCalcSizesEventObj) then
        EvCalcSizes(TCalcSizesEventObj(Event))
{      if Event.InheritsFrom(TUpdateEventObj) then
        Result := EvUpdate(TUpdateEventObj(Event))}
      else if Event.InheritsFrom(TMousePressedEventObj) then
	      Result := EvMousePressed(TMousePressedEventObj(Event))
      else if Event.InheritsFrom(TMouseReleasedEventObj) then
	      Result := EvMouseReleased(TMouseReleasedEventObj(Event))
      else if Event.InheritsFrom(TMouseLeaveEventObj) then
	      Result := EvMouseLeave(TMouseLeaveEventObj(Event))
      else if Event.InheritsFrom(TMouseLeaveCheckEventObj) then
	      Result := EvMouseLeaveCheck(TMouseLeaveCheckEventObj(Event));
    end;
  end;
end;

function TWidget.DistributeEvent(Event: TEventObj): Boolean;
begin
  // Do nothing here, as TWidget itself doesn't have children
  Result := False;
end;

procedure TWidget.SetParent(AParent: TWidget);
begin
  // !!!: reparenting when changing the form is not possible
  if AParent <> FParent then
  begin
    // Remove the widget from the old parent, if it had a parent...
    if Assigned(FParent) and FParent.InheritsFrom(TContainerWidget) then
      TContainerWidget(FParent).RemoveChild(Self);

    FParent := AParent;

    // ...and add it to the new parent, if existent.
    if Assigned(Parent) and Parent.InheritsFrom(TContainerWidget) then
      TContainerWidget(Parent).InsertChild(Self);
  end;
end;

procedure TWidget.SetColor(const AValue: TColor);
begin
  if FColor=AValue then exit;
  FColor:=AValue;
end;

function TWidget.GetBoundsRect: TRect;
begin
  Result.TopLeft := Origin;
  Result.BottomRight := Origin + BoundsSize;
end;

function TWidget.GetLeft: Integer;
begin
  Result := Origin.x;
end;

function TWidget.GetTop: Integer;
begin
  Result := Origin.y;
end;

function TWidget.GetWidth: Integer;
begin
  Result := BoundsSize.cx;
end;

function TWidget.GetHeight: Integer;
begin
  Result := BoundsSize.cy;
end;

procedure TWidget.SetCanExpandWidth(allow: Boolean);
begin
  if FCanExpandWidth <> allow then
  begin
    FCanExpandWidth := allow;
    Update;
  end;
end;

procedure TWidget.SetCanExpandHeight(allow: Boolean);
begin
  if FCanExpandHeight <> allow then
  begin
    FCanExpandHeight := allow;
    Update;
  end;
end;

procedure TWidget.SetText(const AText: String);
begin
  if AText <> Text then
  begin
    FText := AText;
    EvTextChanged;
  end;
end;

function TWidget.EvCalcSizes(Event: TCalcSizesEventObj): Boolean;
begin
  LAYOUTTRACE('TWidget.EvCalcSizes for %s:%s', [Name, ClassName]);

  FMinSize := Size(0, 0);
  FMaxSize := Size(InfiniteSize, InfiniteSize);
  FDefSize := Size(0, 0);

  CalcSizes;

  if MinSize.cx = 0 then FMinSize.cx := 1;
  if MinSize.cy = 0 then FMinSize.cy := 1;
  if DefSize.cx < MinSize.cx then FDefSize.cx := MinSize.cx;
  if DefSize.cy < MinSize.cy then FDefSize.cy := MinSize.cy;

  if (not FCanExpandWidth) or (MaxSize.cx < DefSize.cx) then
    FMaxSize.cx := DefSize.cx;
  if (not FCanExpandHeight) or (MaxSize.cy < DefSize.cy) then
    FMaxSize.cy := DefSize.cy;

  Result := False;
end;

function TWidget.EvEnabledChange(Event: TEnabledChangeEventObj): Boolean;
var
  NewState: Boolean;
begin
  if Assigned(Parent) then
    NewState := Enabled and (wsEnabled in Parent.WidgetState)
  else
    NewState := Enabled;

  if NewState and not (wsEnabled in WidgetState) then
  begin
    Include(WidgetState, wsEnabled);
    Redraw;
    Result := DistributeEvent(Event);
  end else if (not NewState) and (wsEnabled in WidgetState) then
  begin
    Exclude(WidgetState, wsEnabled);
    if wsHasFocus in WidgetState then
      FindForm.FocusedWidget := nil;
    Redraw;
    Result := DistributeEvent(Event);
  end else
    Result := False;
end;

function TWidget.EvVisibilityChange(Event: TVisibilityChangeEventObj): Boolean;
begin
  if Visible then
  begin
    if (not Assigned(Parent)) or (wsIsVisible in Parent.WidgetState) then
      Include(WidgetState, wsIsVisible);
  end else
    Exclude(WidgetState, wsIsVisible);

  Result := False;
end;

function TWidget.EvMousePressed(Event: TMousePressedEventObj): Boolean;
begin
  if (wsClickable in WidgetStyle) and (wsEnabled in WidgetState) and
    (Event.Button = mbLeft) then
  begin
    if wsCaptureMouse in WidgetStyle then
      FindForm.MouseCaptureWidget := Self;
    Include(WidgetState, wsClicked);
    Include(WidgetState, wsHasFocus);
    SetFocus;
    Redraw;
    Result := True;
  end
  else
    Result := False;
end;

function TWidget.EvMouseReleased(Event: TMouseReleasedEventObj): Boolean;
begin
  if (wsClickable in WidgetStyle) and (wsEnabled in WidgetState) and
    (Event.Button = mbLeft) then
  begin
    if wsClicked in WidgetState then
    begin
      Exclude(WidgetState, wsClicked);
      with Event do
        if (Position.x >= 0) and (Position.y >= 0)
            and (Position.x < BoundsSize.cx)
            and (Position.y < BoundsSize.cy) then
          Click
        else
          Redraw;
    end;
    if wsCaptureMouse in WidgetStyle then
      FindForm.MouseCaptureWidget := nil;
    Result := True;
  end else
    Result := False;
end;

function TWidget.EvMouseEnter(Event: TMouseEnterEventObj): Boolean;
begin
  Include(WidgetState, wsMouseInside);
  if Cursor <> crDefault then
    Event.NewCursor := Cursor;
  if (wsClickable in WidgetStyle) and (wsClicked in WidgetState) and
    (wsEnabled in WidgetState) then
    Redraw;
  Result := False;
  {$IFDEF DEBUG}
  WriteLn('Mouse entered ', Name, ':', ClassName, '. New cursor: ', Ord(Event.NewCursor));
  {$ENDIF}
end;

function TWidget.EvMouseLeave(Event: TMouseLeaveEventObj): Boolean;
begin
  Exclude(WidgetState, wsMouseInside);
  if (wsClickable in WidgetStyle) and (wsClicked in WidgetState) and
    (wsEnabled in WidgetState) then
    Redraw;
  Result := False;
  {$IFDEF DEBUG}
  WriteLn('Mouse left ', Name, ':', ClassName);
  {$ENDIF}
end;

function TWidget.EvMouseLeaveCheck(Event: TMouseLeaveCheckEventObj): Boolean;
begin
  if ((Event.Position.x < 0) or (Event.Position.y < 0) or
    (Event.Position.x >= BoundsSize.cx) or
    (Event.Position.y >= BoundsSize.cy)) then
  begin
    if wsMouseInside in WidgetState then
      SendEvent(TMouseLeaveEventObj.Create(Self))
  end else if (FindForm.MouseCaptureWidget = Self) and
    not (wsMouseInside in WidgetState) then
    SendEvent(TMouseEnterEventObj.Create(Self, Event.Shift, Event.Position));

  if (wsMouseInside in WidgetState) and (Event.NewCursor = crDefault) and
    (Cursor <> crDefault) then
  begin
    Event.NewCursor := Cursor;
  end;
  Result := False;
end;

{$ENDIF read_implementation}

