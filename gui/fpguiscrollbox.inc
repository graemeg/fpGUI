{
    fpGUI  -  Free Pascal GUI Library
    
    ScrollBox class declarations
    
    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{%mainunit fpgui.pas}

{ Scrolling support implementation }

{$IFDEF read_interface}

  TScrollingSupport = class
  private
    Parent: TFWidget;
    FBorders: TRect;
    FClientRect: TRect;
    FVirtualSize: TSize;
    FHorzScrollBar, FVertScrollBar: TFScrollBar;
    FOnClientRectChange: TNotifyEvent;
    function    EvMouseWheel(Event: TMouseWheelEventObj): Boolean;
    procedure   EvKeyPressed(Key: Word; Shift: TShiftState);
  protected
  public
    constructor Create(AParent: TFWidget);
    destructor  Destroy; override;
    function    ProcessEvent(Event: TEventObj): Boolean;
    function    DistributeEvent(Event: TEventObj): Boolean;
    function    SendToChild(AChild: TFWidget; Event: TEventObj): Boolean;
    procedure   CalcSizes;
    procedure   Resized;
    function    CalcClientSize(AHorzBarVisible, AVertBarVisible: Boolean): TSize;
    procedure   SetVirtualSize(const ASize: TSize);
    function    ScrollPos: TPoint;
    procedure   DefHorzScrollHandler(Sender: TObject; var APosition: Integer);
    procedure   DefVertScrollHandler(Sender: TObject; var APosition: Integer);
    property    Borders: TRect read FBorders;
    property    ClientRect: TRect read FClientRect;
    property    HorzScrollBar: TFScrollBar read FHorzScrollBar;
    property    VertScrollBar: TFScrollBar read FVertScrollBar;
    property    OnClientRectChange: TNotifyEvent read FOnClientRectChange write FOnClientRectChange;
  end;


  TFCustomScrollBox = class(TFWidget)
  protected
    ScrollingSupport: TScrollingSupport;
    procedure   Paint(Canvas: TFCanvas); override;
    function    ProcessEvent(Event: TEventObj): Boolean; override;
    function    DistributeEvent(Event: TEventObj): Boolean; override;
    procedure   CalcSizes; override;
    procedure   Resized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;


  TFScrollBox = class(TFCustomScrollBox)
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}

// ===================================================================
//   TScrollingSupport
// ===================================================================

constructor TScrollingSupport.Create(AParent: TFWidget);
begin
  Parent := AParent;

  FHorzScrollBar := TFScrollBar.Create(Parent);
  HorzScrollBar.Name := '_Scrolling_HorzBar';
  HorzScrollBar.Embedded := True;
  HorzScrollBar.SetEmbeddedParent(Parent);

  FVertScrollBar := TFScrollBar.Create(Parent);
  VertScrollBar.Name := '_Scrolling_VertBar';
  VertScrollBar.Orientation := Vertical;
  VertScrollBar.Embedded := True;
  VertScrollBar.SetEmbeddedParent(Parent);
end;

destructor TScrollingSupport.Destroy;
begin
  inherited Destroy;
end;

function TScrollingSupport.ProcessEvent(Event: TEventObj): Boolean;
var
  HorzScrollBarHeight, VertScrollBarWidth: Integer;
  Canvas: TFCanvas;
begin
  if Event.InheritsFrom(TPaintEventObj) then
  begin
    if HorzScrollBar.Visible then
      HorzScrollBarHeight := HorzScrollBar.MinSize.cy
    else
      HorzScrollBarHeight := 0;

    if VertScrollBar.Visible then
      VertScrollBarWidth := VertScrollBar.MinSize.cx
    else
      VertScrollBarWidth := 0;

    Canvas := TPaintEventObj(Event).Canvas;
    Parent.Style.DrawScrollBoxBorder(Canvas,
      Rect(0, 0, Parent.Width, Parent.Height));
    Parent.Style.DrawWindowBackground(Canvas, Rect(VertScrollBar.Left,
      HorzScrollBar.Top, VertScrollBar.Left + VertScrollBarWidth,
      HorzScrollBar.Top + HorzScrollBarHeight));
    Result := False;
  end else if Event.InheritsFrom(TMouseWheelEventObj) then
    Result := EvMouseWheel(TMouseWheelEventObj(Event))
  else if Event.InheritsFrom(TMouseWheelEventObj) then
    Result := EvMouseWheel(TMouseWheelEventObj(Event))
  else
    Result := False;
end;

function TScrollingSupport.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := Event.SendToChild(HorzScrollBar) or
    Event.SendToChild(VertScrollBar);
end;

function TScrollingSupport.SendToChild(AChild: TFWidget;
  Event: TEventObj): Boolean;
var
  Canvas: TFCanvas;
  OldMatrix: TGfxMatrix;
begin
  if Event.InheritsFrom(TPreparePaintEventObj) then
  begin
    Canvas := TPaintEventObj(Event).Canvas;
    OldMatrix := Canvas.Matrix;
    Canvas.AppendTranslation(Point(ClientRect.Left - HorzScrollBar.Position,
      ClientRect.Top - VertScrollBar.Position));
    Result := Event.SendToChild(AChild);
    Canvas.Matrix := OldMatrix;
  end else if Event.InheritsFrom(TPaintEventObj) then
  begin
    Canvas := TPaintEventObj(Event).Canvas;
    Canvas.SaveState;
    try
      Canvas.AppendTranslation(Point(-HorzScrollBar.Position, -VertScrollBar.Position));
      if Canvas.IntersectClipRect(ClientRect) {and Canvas.IntersectClipRect(
        Rect(AChild.Left + ClientRect.Left, AChild.Top + ClientRect.Top,
          AChild.Left + AChild.Width + ClientRect.Left,
	  AChild.Top + AChild.Height + ClientRect.Top))} then
      begin
        {Canvas.AppendTranslation(AChild.Left + ClientRect.Left,
	  AChild.Top + ClientRect.Top);
        Inc(Event.RefCount);
        Result := AChild.SendEvent(Event);}
        Canvas.AppendTranslation(ClientRect.TopLeft);
	      Result := Event.SendToChild(AChild);
      end else
        Result := False;
    finally
      Canvas.RestoreState;
    end;
  end else
    Result := Event.SendToChild(AChild);
end;

procedure TScrollingSupport.CalcSizes;
begin
  FBorders := Parent.Style.GetScrollBoxBorders;
  with Parent, Borders do
  begin
    FMinSize := HorzScrollBar.MinSize + VertScrollBar.MinSize +
      TopLeft + BottomRight;
    FDefSize := HorzScrollBar.DefSize + VertScrollBar.DefSize +
      TopLeft + BottomRight;
  end;
end;

procedure TScrollingSupport.Resized;
var
  HorzScrollBarHeight, VertScrollBarWidth: Integer;

  procedure CalcScrollBarSizes;
  begin
    if HorzScrollBar.Visible then
      HorzScrollBarHeight := HorzScrollBar.MinSize.cy
    else
      HorzScrollBarHeight := 0;

    if VertScrollBar.Visible then
      VertScrollBarWidth := VertScrollBar.MinSize.cx
    else
      VertScrollBarWidth := 0;
  end;

var
  Canvas: TFCanvas;
  HorzBarVisible, VertBarVisible,
    LastHorzBarVisible, LastVertBarVisible: Boolean;
begin
  HorzBarVisible      := HorzScrollBar.Visible;
  VertBarVisible      := VertScrollBar.Visible;
  LastHorzBarVisible  := not HorzBarVisible;

  if FVirtualSize <> gfxbase.Size(0, 0) then
    with Size(ClientRect) do
    begin
      HorzScrollBar.PageSize  := cx;
      VertScrollBar.PageSize := cy;
    end;

  FBorders := Parent.Style.GetScrollBoxBorders;
  with FBorders do
  begin
    while (HorzBarVisible <> LastHorzBarVisible) or
      (VertBarVisible <> LastVertBarVisible) do
    begin
      LastHorzBarVisible := HorzBarVisible;
      LastVertBarVisible := VertBarVisible;
      CalcScrollBarSizes;
      HorzScrollBar.SetBounds(
	      Point(Left, Parent.Height - HorzScrollBar.MinSize.cy - Bottom),
        Size(Parent.Width - VertScrollBarWidth - Left - Right, HorzScrollBar.MinSize.cy));
      VertScrollBar.SetBounds(
        Point(Parent.Width - VertScrollBar.MinSize.cx - Right, Top),
	      Size(VertScrollBar.MinSize.cx, Parent.Height - HorzScrollBarHeight - Top - Bottom));

      ClientRect.Left   := Left;
      ClientRect.Top    := Top;
      ClientRect.Right  := Parent.Width - Right - VertScrollBarWidth;
      ClientRect.Bottom := Parent.Height - Bottom - HorzScrollBarHeight;
      if Assigned(OnClientRectChange) then
        OnClientRectChange(Self);

      HorzBarVisible := HorzScrollBar.Visible;
      VertBarVisible := VertScrollBar.Visible;
    end;
  end;
end;

function TScrollingSupport.CalcClientSize(AHorzBarVisible,
  AVertBarVisible: Boolean): TSize;
begin
  FBorders  := Parent.Style.GetScrollBoxBorders;
  Result    := Parent.BoundsSize - Borders.TopLeft - Borders.BottomRight;
  if AVertBarVisible then
    Dec(Result.cx, VertScrollBar.MinSize.cx);
  if AHorzBarVisible then
    Dec(Result.cy, HorzScrollBar.MinSize.cy);
end;

procedure TScrollingSupport.SetVirtualSize(const ASize: TSize);
begin
  FVirtualSize        := ASize;
  HorzScrollBar.Max   := FVirtualSize.cx;
  VertScrollBar.Max   := FVirtualSize.cy;
end;

function TScrollingSupport.ScrollPos: TPoint;
begin
  Result.x := HorzScrollBar.Position;
  Result.y := VertScrollBar.Position;
end;

procedure TScrollingSupport.DefHorzScrollHandler(Sender: TObject; var APosition: Integer);
var
  Delta: Integer;
  r: TRect;
begin
  Delta := HorzScrollBar.Position - APosition;
  r := ClientRect;
  if Delta < 0 then	// Scrolling to the right side
    Dec(r.Left, Delta)
  else			// Scrolling to the left side
    Dec(r.Right, Delta);
  Parent.Scroll(r, Delta, 0);
end;

procedure TScrollingSupport.DefVertScrollHandler(Sender: TObject;
  var APosition: Integer);
var
  Delta: Integer;
  r: TRect;
begin
  Delta := VertScrollBar.Position - APosition;
  r := ClientRect;
  if Delta < 0 then	// Scrolling downwards
    Dec(r.Top, Delta)
  else			// Scrolling upwards
    Dec(r.Bottom, Delta);
  Parent.Scroll(r, 0, Delta);
end;

function TScrollingSupport.EvMouseWheel(Event: TMouseWheelEventObj): Boolean;
var
  mshift: TShiftState;
begin
  if Parent.DistributeEvent(Event) then
    Exit; //==>

  mshift := Event.Shift * [ssShift, ssAlt, ssCtrl, ssMeta,
    ssSuper, ssHyper, ssAltGr];

  if not VertScrollBar.Visible then
    Include(mshift, ssShift);

  if mshift = [] then
    VertScrollBar.Position := VertScrollBar.Position +
      Round(Event.WheelDelta * VertScrollBar.SmallChange)
  else if mshift = [ssShift] then
    HorzScrollBar.Position := HorzScrollBar.Position +
      Round(Event.WheelDelta * VertScrollBar.SmallChange);
  Result := True;
end;

procedure TScrollingSupport.EvKeyPressed(Key: Word; Shift: TShiftState);
var
  mshift: TShiftState;
begin
{
  mshift := Shift * [ssShift, ssAlt, ssCtrl, ssMeta, ssSuper, ssHyper, ssAltGr];
  if mshift = [] then
    case Key of
      keyLeft:
        HorzScrollBar.ButtonUpClick(nil);
      keyRight:
        HorzScrollBar.ButtonDownClick(nil);
      keyUp:
        VertScrollBar.ButtonUpClick(nil);
      keyDown:
        VertScrollBar.ButtonDownClick(nil);
      keyPageUp:
        VertScrollBar.PageUp;
      keyPageDown:
        VertScrollBar.PageDown;
      keyHome:
        VertScrollBar.Position := 0;
      keyEnd:
        VertScrollBar.Position := VertScrollBar.Max - VertScrollBar.PageSize;
    end
  else if mshift = [ssShift] then
    case Key of
      keyPageUp:
        HorzScrollBar.PageUp;
      keyPageDown:
        HorzScrollBar.PageDown;
      keyHome:
        HorzScrollBar.Position := 0;
      keyEnd:
        HorzScrollBar.Position := HorzScrollBar.Max - HorzScrollBar.PageSize;
    end
  else
    inherited EvKeyPressed(Key, Shift);
}
end;



// ===================================================================
//   TFCustomScrollBox
// ===================================================================

constructor TFCustomScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle       := WidgetStyle + [wsClickable, wsOpaque];
  FCanExpandWidth   := True;
  FCanExpandHeight  := True;
  ScrollingSupport  := TScrollingSupport.Create(Self);
end;

destructor TFCustomScrollBox.Destroy;
begin
  ScrollingSupport.Free;
  inherited Destroy;
end;


// Protected methods

procedure TFCustomScrollBox.Paint(Canvas: TFCanvas);
begin
  Assert(Canvas = Canvas);
{
  Style.DrawWindowBackground(Canvas, Rect(HorzScrollBar.Left,
    VertScrollBar.Top, HorzScrollBar.Left + HorzScrollBar.Width,
    VertScrollBar.Top + VertScrollBar.Height));}
end;

function TFCustomScrollBox.ProcessEvent(Event: TEventObj): Boolean;
begin
  Result := ScrollingSupport.ProcessEvent(Event) or
    inherited ProcessEvent(Event);
end;

function TFCustomScrollBox.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := ScrollingSupport.DistributeEvent(Event) or
    inherited DistributeEvent(Event);
end;

procedure TFCustomScrollBox.CalcSizes;
begin
  ScrollingSupport.CalcSizes;
end;

procedure TFCustomScrollBox.Resized;
begin
  ScrollingSupport.Resized;
end;


{$ENDIF read_implementation}

