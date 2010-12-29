{
    fpGUI  -  Free Pascal GUI Library
    
    Form class declarations
    
    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{%mainunit fpgui.pas}

{$IFDEF read_interface}

  { TFCustomForm }

  TFCustomForm = class(TFBinWidget)
  private
    FFocusedWidget: TFWidget;
    FMouseCaptureWidget: TFWidget;
    FLastSetCursor: TFCursor;
    FWindowOptions: TFWindowOptions;
    FWnd: TFCustomWindow;
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    // Property access
    function    GetWnd: TFCustomWindow;
    procedure   SetFocusedWidget(AWidget: TFWidget);
    procedure   SetMouseCaptureWidget(AWidget: TFWidget);
    procedure   SetWindowOptions(const AValue: TFWindowOptions);
    // fpGFX event handling
    procedure   WndClose(Sender: TObject);
    procedure   WndFocusIn(Sender: TObject);
    procedure   WndFocusOut(Sender: TObject);
    procedure   WndHide(Sender: TObject);
    procedure   WndKeyPressed(Sender: TObject; AKey: Word; AShift: TShiftState);
    procedure   WndKeyReleased(Sender: TObject; AKey: Word; AShift: TShiftState);
    procedure   WndKeyChar(Sender: TObject; AKeyChar: Char);
    procedure   WndMouseEnter(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
    procedure   WndMouseLeave(Sender: TObject);
    procedure   WndMouseMoved(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
    procedure   WndMousePressed(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   WndMouseReleased(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   WndMouseWheel(Sender: TObject; AShift: TShiftState; AWheelDelta: Single; const AMousePos: TPoint);
    procedure   WndPaint(Sender: TObject; const ARect: TRect);
    procedure   WndMove(Sender: TObject);
    procedure   WndResize(Sender: TObject);
    procedure   WndShow(Sender: TObject);
  protected
    FBorderWidth: Integer;
    FIsActive: Boolean;
    FResizedByUser: Boolean;
    FPositionSpecified: Boolean;
    procedure   Loaded; override;
    procedure   Paint(Canvas: TFCanvas); override;
    procedure   Resized; override;
    function    WidgetCoords(AWidget: TFWidget): TPoint;
    function    ProcessEvent(Event: TEventObj): Boolean; override;
    procedure   CalcSizes; override;
    procedure   EvTextChanged; override;
    procedure   CreateWnd;
    property    CanExpandWidth default True;
    property    CanExpandHeight default True;
    property    Cursor default crArrow;
    property    BorderWidth: Integer read FBorderWidth write FBorderWidth;
    property    OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property    OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property    OnActivate: TNotifyEvent read FOnActivate write FOnActivate;        // Get focus
    property    OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;  // Loose focus
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Show; override;
    procedure   ShowModal;
    procedure   Close; virtual;
    procedure   SetPosition(APosition: TPoint);
    property    FocusedWidget: TFWidget read FFocusedWidget write SetFocusedWidget;
    property    IsActive: Boolean read FIsActive;
    property    MouseCaptureWidget: TFWidget read FMouseCaptureWidget write SetMouseCaptureWidget;
    property    WindowOptions: TFWindowOptions read FWindowOptions write SetWindowOptions;
    property    Wnd: TFCustomWindow read GetWnd;
  end;


  TFForm = class(TFCustomForm)
  published
    property BorderWidth;
    property Color;
    property Enabled;
    property Text;
    property WindowOptions;
    property OnCreate;
    property OnDestroy;
    property OnActivate;
    property OnDeactivate;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}

// ===================================================================
//   TFCustomForm
// ===================================================================

constructor TFCustomForm.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);
  FStyle := gStyleManager.DefaultStyle;

  FCanExpandWidth   := True;
  FCanExpandHeight  := True;
  FCursor           := crArrow;
  FWindowOptions    := [woWindow];
end;


destructor TFCustomForm.Destroy;
begin
  if Assigned(OnDestroy) then
    OnDestroy(Self);
  if Assigned(FWnd) then
    FWnd.Free;

//  GFApplication.Forms.Remove(Self);

  inherited Destroy;
end;


procedure TFCustomForm.Show;
begin
  LAYOUTTRACE('TFCustomForm.Show for %s:%s', [Name, ClassName]);

  FVisible := True;
//  GFApplication.AddWindow(Wnd);
  Wnd.Show;
end;

procedure TFCustomForm.ShowModal;
begin
  Include(FWindowOptions, woModal);
  Show;
end;

procedure TFCustomForm.Close;
begin
  LAYOUTTRACE('TFCustomForm.Close for %s:%s', [Name, ClassName]);

  FVisible := False;
  
  FWnd.Free;
  FWnd := nil;

  if FStyle <> gStyleManager.DefaultStyle then
    FStyle.Free;
end;


procedure TFCustomForm.SetPosition(APosition: TPoint);
begin
  if Assigned(FWnd) then
    Wnd.SetPosition(APosition)
  else
  begin
    FOrigin := APosition;
    FPositionSpecified := True;
  end;
end;


function TFCustomForm.WidgetCoords(AWidget: TFWidget): TPoint;
begin
  Result := Point(0, 0);
  while AWidget <> Self do
  begin
    Result := Result + AWidget.Parent.ClientToWidget(AWidget.Origin);
    AWidget := AWidget.Parent;
  end;
end;


procedure TFCustomForm.Loaded;
begin
  inherited Loaded;
  if Assigned(OnCreate) then
    OnCreate(Self);
end;


procedure TFCustomForm.Paint(Canvas: TFCanvas);
begin
  inherited Paint(Canvas);
  Style.DrawWindowBackground(Canvas, Rect(0, 0, Width, Height));
end;


procedure TFCustomForm.CalcSizes;
begin
  if Assigned(Child) then
  begin
    FMinSize := Child.MinSize + 2 * BorderWidth;
    FDefSize := Child.DefSize + 2 * BorderWidth;
    FMaxSize.cx := Min(InfiniteSize, Child.MaxSize.cx + 2 * BorderWidth);
    FMaxSize.cy := Min(InfiniteSize, Child.MaxSize.cy + 2 * BorderWidth);
  end;
end;


function TFCustomForm.ProcessEvent(Event: TEventObj): Boolean;
begin
  if Event is TDestroyEventObj then
    FWnd := nil;

  Result := inherited ProcessEvent(Event);
end;


procedure TFCustomForm.EvTextChanged;
begin
  if Assigned(FWnd) then
    Wnd.Title := Text;
end;


procedure TFCustomForm.CreateWnd;
var
  ParentWnd: TFCustomWindow;
begin
  if Parent is TFCustomForm then
    ParentWnd := TFCustomForm(Parent).Wnd
  else
    ParentWnd := nil;

  FWnd :=  TFWindow.Create(ParentWnd, WindowOptions);

  if FPositionSpecified then
    Wnd.SetPosition(Origin);

  Wnd.OnClose           := @WndClose;
  Wnd.OnFocusIn         := @WndFocusIn;
  Wnd.OnFocusOut        := @WndFocusOut;
  Wnd.OnHide            := @WndHide;
  Wnd.OnKeyPressed      := @WndKeyPressed;
  Wnd.OnKeyReleased     := @WndKeyReleased;
  Wnd.OnKeyChar         := @WndKeyChar;
  Wnd.OnMouseEnter      := @WndMouseEnter;
  Wnd.OnMouseLeave      := @WndMouseLeave;
  Wnd.OnMouseMove       := @WndMouseMoved;
  Wnd.OnMousePressed    := @WndMousePressed;
  Wnd.OnMouseReleased   := @WndMouseReleased;
  Wnd.OnMouseWheel      := @WndMouseWheel;
  Wnd.OnPaint           := @WndPaint;
  Wnd.OnMove            := @WndMove;
  Wnd.OnResize          := @WndResize;
  Wnd.OnShow            := @WndShow;

  if Length(Text) = 0 then
    Wnd.Title := GFApplication.Title
  else
    Wnd.Title := Text;
end;


{
procedure TFCustomForm.ApplyNewLayout;
var
  OrigW, OrigH: Integer;
begin
  Wnd.SetMinMaxClientSize(MinW, MinH, MaxW, MaxH);

  OrigW := ClientRect.Right;
  OrigH := ClientRect.Bottom;

  if (ClientRect.Right < MinW) or (ClientRect.Bottom < MinW) or
    (ClientRect.Right > MaxW) or (ClientRect.Bottom > MaxH) then
  begin
    if ClientRect.Right < MinW then
      FClientRect.Right := MinW;
    if ClientRect.Bottom < MinH then
      FClientRect.Bottom := MinH;
    if ClientRect.Right > MaxW then
      FClientRect.Right := MaxW;
    if ClientRect.Bottom > MaxH then
      FClientRect.Bottom := MaxH;
//    Wnd.SetClientSize(ClientRect.Right, ClientRect.Bottom);
  end;

  if not FResizedByUser then
  begin
    FClientRect.Right := DefW;
    FClientRect.Bottom := DefH;
  end;

  if (ClientRect.Right <> OrigW) or (ClientRect.Bottom <> OrigH) then
  begin
    LAYOUTTRACE('TFCustomForm.EvRecalcLayout for %s:%s: Setting size to %dx%d',
      [Name, ClassName, ClientRect.Right, ClientRect.Bottom]);
    Wnd.SetClientSize(ClientRect.Right, ClientRect.Bottom);
  end;
end;}


procedure TFCustomForm.Resized;
begin
  FClientRect.Right := Wnd.ClientWidth;
  FClientRect.Bottom := Wnd.ClientHeight;
  if Assigned(Child) then
    Child.SetBounds(Point(BorderWidth, BorderWidth),
      TSize(ClientRect.BottomRight) - 2 * BorderWidth);
end;


procedure TFCustomForm.SetFocusedWidget(AWidget: TFWidget);
begin
  if AWidget <> FocusedWidget then
  begin
    if Assigned(FocusedWidget) then
    begin
      Exclude(FFocusedWidget.WidgetState, wsHasFocus);
      FocusedWidget.EvFocusChanged;
    end;
    FFocusedWidget := AWidget;
    if Assigned(FocusedWidget) then
    begin
      Include(FFocusedWidget.WidgetState, wsHasFocus);
      FocusedWidget.EvFocusChanged;
    end;
  end;
end;

function TFCustomForm.GetWnd: TFCustomWindow;
begin
  if not Assigned(FWnd) then
  begin
    CreateWnd;
    // !!!: Doesn't handle a set initial size yet
    SendEvent(TCalcSizesEventObj.Create(Self));
    Wnd.SetMinMaxClientSize(MinSize, MaxSize);
    Wnd.SetClientSize(DefSize);
  end;
  Result := FWnd;
end;


procedure TFCustomForm.SetMouseCaptureWidget(AWidget: TFWidget);
begin
  if AWidget <> FMouseCaptureWidget then
  begin
    FMouseCaptureWidget := AWidget;
    if Assigned(AWidget) then
      Wnd.CaptureMouse
    else
      Wnd.ReleaseMouse;
  end;
end;

procedure TFCustomForm.SetWindowOptions(const AValue: TFWindowOptions);
begin
  if FWindowOptions=AValue then exit;
  FWindowOptions:=AValue;
  if Assigned(FWnd) then Wnd.WindowOptions := AValue;
end;


// GfxWindow message handlers

procedure TFCustomForm.WndClose(Sender: TObject);
begin
  SendEvent(TDestroyEventObj.Create(Self));
  FMouseCaptureWidget := nil;
end;

procedure TFCustomForm.WndFocusIn(Sender: TObject);
begin
  FIsActive := True;
  if Assigned(FocusedWidget) then
    FocusedWidget.EvFocusChanged;
  if Assigned(OnActivate) then
    OnActivate(Self);
end;

procedure TFCustomForm.WndFocusOut(Sender: TObject);
begin
  FIsActive := False;
  if Assigned(FocusedWidget) then
    FocusedWidget.EvFocusChanged;
  if Assigned(OnDeactivate) then
    OnDeactivate(Self);
end;

procedure TFCustomForm.WndHide(Sender: TObject);
begin
  LAYOUTTRACE('TFCustomForm.WndHide for %s:%s', [Name, ClassName]);
  if wsIsVisible in WidgetState then
  begin
    Exclude(WidgetState, wsIsVisible);
    SendEvent(TVisibilityChangeEventObj.Create(Self));
    Update;
  end;
end;

procedure TFCustomForm.WndKeyPressed(Sender: TObject;
  AKey: Word; AShift: TShiftState);
begin
  if Assigned(FocusedWidget) then
    FocusedWidget.EvKeyPressed(AKey, AShift)
  else
    EvKeyPressed(AKey, AShift);
end;

procedure TFCustomForm.WndKeyReleased(Sender: TObject;
  AKey: Word; AShift: TShiftState);
begin
  if Assigned(FocusedWidget) then
    FocusedWidget.EvKeyReleased(AKey, AShift)
  else
    EvKeyReleased(AKey, AShift);
end;

procedure TFCustomForm.WndKeyChar(Sender: TObject; AKeyChar: Char);
begin
  if Assigned(FocusedWidget) then
    FocusedWidget.EvKeyChar(AKeyChar)
  else
    EvKeyChar(AKeyChar);
end;

procedure TFCustomForm.WndMouseEnter(Sender: TObject;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  if wsEnabled in WidgetState then
    DoMouseEnter(AShift, AMousePos);
end;


procedure TFCustomForm.WndMouseLeave(Sender: TObject);
begin
  if wsEnabled in WidgetState then
    SendEvent(TMouseLeaveEventObj.Create(Self));
end;


procedure TFCustomForm.WndMouseMoved(Sender: TObject;
  AShift: TShiftState; const AMousePos: TPoint);
{var
  dx, dy: Integer;
  IsInside: Boolean;
begin
  if Assigned(MouseCaptureWidget) then
  begin
    WidgetCoords(MouseCaptureWidget, dx, dy);

    // Emulate MouseEnter/MouseLeave events
    IsInside := (x >= dx) and (y >= dy) and
      (x < dx + MouseCaptureWidget.Width) and (y < dy + MouseCaptureWidget.Height);
    if IsInside and not (wsMouseInside in MouseCaptureWidget.WidgetState) then
      MouseCaptureWidget.EvMouseEnter(Shift, x - dy, y - dy)
    else if (not IsInside) and (wsMouseInside in MouseCaptureWidget.WidgetState) then
      MouseCaptureWidget.EvMouseLeave;

    MouseCaptureWidget.SendEvent(
     TMouseMovedEventObj.Create(Self, Shift, x - dx, y - dy));
  end else}

  procedure SendMouseEvents(Widget: TFWidget; APos: TPoint);
  var
    LeaveCheckEvent: TMouseLeaveCheckEventObj;
  begin
    LeaveCheckEvent := TMouseLeaveCheckEventObj.Create(Self, AShift, APos);
    LeaveCheckEvent.AddRef;
    Widget.SendEvent(LeaveCheckEvent);
    Widget.SendEvent(TMouseMoveEventObj.Create(Self, AShift, APos));
    if (LeaveCheckEvent.NewCursor <> crDefault) and
      (LeaveCheckEvent.NewCursor <> Wnd.Cursor) then
      Wnd.Cursor := LeaveCheckEvent.NewCursor;
    LeaveCheckEvent.Free;
  end;

begin
  if wsEnabled in WidgetState then
  begin
    if Assigned(MouseCaptureWidget) then
    begin
      SendMouseEvents(MouseCaptureWidget,
        AMousePos - WidgetCoords(MouseCaptureWidget));
      if not Assigned(MouseCaptureWidget) then
	      SendMouseEvents(Self, AMousePos);
    end else
      SendMouseEvents(Self, AMousePos);
  end;  { if }
end;


procedure TFCustomForm.WndMousePressed(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  if wsEnabled in WidgetState then
  begin
    if Assigned(MouseCaptureWidget) then
    begin
      MouseCaptureWidget.SendEvent(
          TMousePressedEventObj.Create(Self, AButton, AShift,
	        AMousePos - WidgetCoords(MouseCaptureWidget)));
      if not Assigned(MouseCaptureWidget) then
	      SendEvent(TMouseMoveEventObj.Create(Self, AShift, AMousePos));
    end
    else
      SendEvent(TMousePressedEventObj.Create(Self, AButton, AShift, AMousePos));
  end;  { if }
end;


procedure TFCustomForm.WndMouseReleased(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  if wsEnabled in WidgetState then
    if Assigned(MouseCaptureWidget) then
    begin
      MouseCaptureWidget.SendEvent(
        TMouseReleasedEventObj.Create(Self, AButton, AShift,
	  AMousePos - WidgetCoords(MouseCaptureWidget)));
      if not Assigned(MouseCaptureWidget) then
	SendEvent(TMouseMoveEventObj.Create(Self, AShift, AMousePos));
    end else
      SendEvent(TMouseReleasedEventObj.Create(Self,
        AButton, AShift, AMousePos));
end;


procedure TFCustomForm.WndMouseWheel(Sender: TObject; AShift: TShiftState;
  AWheelDelta: Single; const AMousePos: TPoint);
begin
  if wsEnabled in WidgetState then
    SendEvent(TMouseWheelEventObj.Create(Self, AShift, AWheelDelta, AMousePos));
end;


procedure TFCustomForm.WndPaint(Sender: TObject; const ARect: TRect);
begin
  LAYOUTTRACE('TFCustomForm.WndPaint for %s:%s (%d/%d-%d/%d)',
    [Name, ClassName, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom]);
  if wsIsVisible in WidgetState then
    SendEvent(TPaintEventObj.Create(Self, TFCanvas(Wnd.Canvas)));
end;


procedure TFCustomForm.WndMove(Sender: TObject);
begin
  FOrigin := Point(Wnd.Left, Wnd.Top);
end;


procedure TFCustomForm.WndResize(Sender: TObject);
begin
  LAYOUTTRACE('TFCustomForm.WndResize for %s:%s: New size is %dx%d. Visible? %d',
    [Name, ClassName, Wnd.Width, Wnd.Height, Ord(wsIsVisible in WidgetState)]);
  if Visible or (wsIsVisible in WidgetState) then
  begin
    FResizedByUser := (Wnd.Width <> DefSize.cx) or (Wnd.Height <> DefSize.cy);
    SetBounds(Origin, gfxBase.Size(Wnd.Width, Wnd.Height));
  end;
end;


procedure TFCustomForm.WndShow(Sender: TObject);
begin
  LAYOUTTRACE('TFCustomForm.WndShow for %s:%s', [Name, ClassName]);
  if not (wsIsVisible in WidgetState) then
  begin
    Include(WidgetState, wsIsVisible);
    SendEvent(TVisibilityChangeEventObj.Create(Self));
  end;
end;


{$ENDIF read_implementation}

