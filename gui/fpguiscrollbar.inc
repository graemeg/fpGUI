{
    fpGUI  -  Free Pascal GUI Library
    
    ScrollBar class declarations
    
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

  // !!!: Add argument which indicates the type of scrolling
  TScrollEvent = procedure(Sender: TObject; var APosition: Integer) of object;


  TFCustomScrollBar = class(TFWidget)
  private
    FOrientation: TOrientation;
    FMin: Integer;
    FMax: Integer;
    FPageSize: Integer;
    FPosition: Integer;
    FSmallChange: Integer;
    FLargeChange: Integer;
    FOnChange: TNotifyEvent;
    FOnScroll: TScrollEvent;
    ButtonUp: TFGenericButton;
    ButtonDown: TFGenericButton;
    Slider: TFWidget;
    Embedded: Boolean;		// for internal embedded usage!

    // Event handling
    procedure   ButtonUpClick(Sender: TObject);
    procedure   ButtonDownClick(Sender: TObject);
    // Property access
    procedure   SetOrientation(AOrientation: TOrientation);
    procedure   SetMin(AMin: Integer);
    procedure   SetMax(AMax: Integer);
    procedure   SetPageSize(APageSize: Integer);
    procedure   SetPosition(APosition: Integer);
    // Helpers
    function    GeTFButtonSize: Integer;
    function    ClipPosition(APosition: Integer): Integer;
    procedure   UpdateBar;
  protected
    function    DistributeEvent(Event: TEventObj): Boolean; override;
    procedure   Paint(Canvas: TFCanvas); override;
    procedure   CalcSizes; override;
    procedure   Resized; override;
    property    Orientation: TOrientation read FOrientation write SetOrientation;
    property    Min: Integer read FMin write SetMin default 0;
    property    Max: Integer read FMax write SetMax default 100;
    property    PageSize: Integer read FPageSize write SetPageSize;
    property    Position: Integer read FPosition write SetPosition default 0;
    property    SmallChange: Integer read FSmallChange write FSmallChange default 1;
    property    LargeChange: Integer read FLargeChange write FLargeChange default 0;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
    property    OnScroll: TScrollEvent read FOnScroll write FOnScroll;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   LineUp;
    procedure   LineDown;
    procedure   PageUp;
    procedure   PageDown;
  end;
  

  TFScrollBar = class(TFCustomScrollBar)
  published
    property Enabled;
    property Orientation;
    property Min;
    property Max;
    property PageSize;
    property Position;
    property SmallChange;
    property LargeChange;
    property OnChange;
    property OnScroll;
  end;


{$ENDIF read_interface}



{$IFDEF read_implementation}



// ===================================================================
//   TFCustomScrollBar and helper classes
// ===================================================================

// -------------------------------------------------------------------
//   TFScrollBarButton
// -------------------------------------------------------------------

type
  { Private button type only used for scrollbars. }
  TFScrollBarButton = class(TFGenericButton)
  protected
    procedure Paint(Canvas: TFCanvas); override;
    procedure CalcSizes; override;
  public
    Direction: TArrowDirection;
  end;


procedure TFScrollBarButton.Paint(Canvas: TFCanvas);
begin
  inherited Paint(Canvas);
  Style.DrawScrollBarButton(Canvas, Rect(0, 0, BoundsSize.cx, BoundsSize.cy),
     Direction, (wsClicked in WidgetState) and (wsMouseInside in WidgetState),
    wsEnabled in WidgetState);
end;


procedure TFScrollBarButton.CalcSizes;
begin
  ASSERT(Owner is TFCustomScrollBar);
  FMinSize := Style.GetScrollBarBtnSize(TFCustomScrollBar(Owner).Orientation);
end;


// -------------------------------------------------------------------
//   TFScrollBarSlider
// -------------------------------------------------------------------

type
  TFScrollBarSlider = class(TFWidget)
  private
    function    EvMousePressed(Event: TMousePressedEventObj): Boolean;
    function    EvMouseReleased(Event: TMouseReleasedEventObj): Boolean;
    function    EvMouseMove(Event: TMouseMoveEventObj): Boolean;
  protected
    ButtonPos: Integer;
    ButtonSize: Integer;
    IsDraggingButton: Boolean;
    PrevAreaPressed: Boolean;
    NextAreaPressed: Boolean;
    DragStartMousePos: Integer;
    DragStarTFButtonPos: Integer;
//    ButtonMoveSavedPosition: Integer;
    procedure   Paint(Canvas: TFCanvas); override;
    function    ProcessEvent(Event: TEventObj): Boolean; override;
    procedure   CalcSizes; override;
    function    CalcPosition: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   UpdateBar;
  end;


constructor TFScrollBarSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
end;

procedure TFScrollBarSlider.UpdateBar;
var
  Size: Integer;
begin
  ASSERT(Owner is TFCustomScrollBar);

  if Visible then
  begin
    with TFCustomScrollBar(Owner) do
    begin
      if (Min = Max) or (Max - Min = PageSize - 1) then
	      ButtonPos := 0
      else
      begin
      	if Orientation = Horizontal then
      	  Size := Self.BoundsSize.cx
      	else
      	  Size := Self.BoundsSize.cy;

      	if PageSize = 0 then
      	  ButtonPos := (Position - Min) * (Size - ButtonSize) div (Max - Min)
        else
          ButtonPos := (Position - Min) *
	            (Size - ButtonSize) div (Max - Min - PageSize + 1);
      end;
      ButtonSize := GeTFButtonSize;
    end;
    Redraw;
  end;
end;

procedure TFScrollBarSlider.Paint(Canvas: TFCanvas);
var
  Size: Integer;
  r: TRect;
  StartPos, EndPos: PInteger;
  Color2: TColor;
begin
  ASSERT(Owner is TFCustomScrollBar);
  inherited Paint(Canvas);

  if TFCustomScrollBar(Owner).Orientation = Horizontal then
  begin
    r.Top := 0;
    r.Bottom := Height;
    StartPos := @r.Left;
    EndPos := @r.Right;
    Size := Width;
  end else
  begin
    r.Left := 0;
    r.Right := Width;
    StartPos := @r.Top;
    EndPos := @r.Bottom;
    Size := Height;
  end;

  if ButtonPos > 0 then
  begin
    if PrevAreaPressed then
      Color2 := cl3DDkShadow
    else
      Color2 := cl3DLight;
    Canvas.SetColor(GetAvgColor(Style.GetUIColor(clScrollBar),
        Style.GetUIColor(Color2)));
    StartPos^ := 0;
    EndPos^ := ButtonPos;
    Canvas.FillRect(r);
  end;
  if ButtonPos + ButtonSize < Size then
  begin
    if NextAreaPressed then
      Color2 := cl3DDkShadow
    else
      Color2 := cl3DLight;
    Canvas.SetColor(GetAvgColor(Style.GetUIColor(clScrollBar),
        Style.GetUIColor(Color2)));
    StartPos^ := ButtonPos + ButtonSize;
    EndPos^ := Size;
    Canvas.FillRect(r);
  end;

  StartPos^ := ButtonPos;
  EndPos^ := ButtonPos + ButtonSize;
  Style.DrawButtonFace(Canvas, r, [btnIsEmbedded]);
end;

function TFScrollBarSlider.ProcessEvent(Event: TEventObj): Boolean;
begin
  Result := False;
  if Event.InheritsFrom(TMousePressedEventObj) then
    Result := EvMousePressed(TMousePressedEventObj(Event))
  else if Event.InheritsFrom(TMouseReleasedEventObj) then
    Result := EvMouseReleased(TMouseReleasedEventObj(Event))
  else if Event.InheritsFrom(TMouseMoveEventObj) then
    Result := EvMouseMove(TMouseMoveEventObj(Event));

  if not Result then
    Result := inherited ProcessEvent(Event);

  if Event.InheritsFrom(TVisibilityChangeEventObj) and Visible then
    UpdateBar;
end;

function TFScrollBarSlider.EvMousePressed(Event: TMousePressedEventObj): Boolean;
var
  Pos: Integer;
begin
  Result := inherited ProcessEvent(Event);	// For mouse grabbing support

  if Event.Button <> mbLeft then
    exit;

  if TFCustomScrollBar(Owner).Orientation = Horizontal then
    Pos := Event.Position.x
  else
    Pos := Event.Position.y;

  if Pos < ButtonPos then
  begin
    PrevAreaPressed := True;
    TFCustomScrollBar(Owner).PageUp
  end
  else if Pos > ButtonPos + ButtonSize then
  begin
    NextAreaPressed := True;
    TFCustomScrollBar(Owner).PageDown
  end
  else
  begin
    IsDraggingButton := True;
    DragStartMousePos := Pos;
    DragStarTFButtonPos := ButtonPos;
  end;

  Result := True;
end;

function TFScrollBarSlider.EvMouseReleased(Event: TMouseReleasedEventObj): Boolean;
var
  NewPosition: Integer;
begin
  Result := inherited ProcessEvent(Event);	// For mouse grabbing support

  if Event.Button <> mbLeft then
    exit;

  if IsDraggingButton then
  begin
    IsDraggingButton := False;
    NewPosition := CalcPosition;
    if NewPosition <> TFCustomScrollBar(Owner).Position then
    begin
      if Assigned(TFCustomScrollBar(Owner).OnScroll) then
        TFCustomScrollBar(Owner).OnScroll(Owner, NewPosition);
      TFCustomScrollBar(Owner).FPosition := NewPosition;
    end;
    if (NewPosition <> DragStartMousePos) and
        Assigned(TFCustomScrollBar(Owner).OnChange) then
      TFCustomScrollBar(Owner).OnChange(Self);
    UpdateBar;
  end
  else if PrevAreaPressed then
  begin
    PrevAreaPressed := False;
    Redraw;
  end
  else if NextAreaPressed then
  begin
    NextAreaPressed := False;
    Redraw;
  end;
  Result := True;
end;

function TFScrollBarSlider.EvMouseMove(Event: TMouseMoveEventObj): Boolean;
var
  Pos, Size, VirtualPos: Integer;
begin
  if IsDraggingButton then
  begin
    if wsMouseInside in WidgetState then
    begin
      if TFCustomScrollBar(Owner).Orientation = Horizontal then
      begin
        Pos := Event.Position.x;
        Size := Width;
      end else
      begin
        Pos := Event.Position.y;
        Size := Height;
      end;
    end
    else
    begin
      Pos := DragStartMousePos;
      if TFCustomScrollBar(Owner).Orientation = Horizontal then
        Size := Width
      else
        Size := Height;
    end;  { if/else }

    ButtonPos := ClipMinMax(DragStarTFButtonPos + Pos - DragStartMousePos,
      0, Size - ButtonSize);
    VirtualPos := CalcPosition;

    if VirtualPos <> TFCustomScrollBar(Owner).Position then
    begin
      if Assigned(TFCustomScrollBar(Owner).OnScroll) then
        TFCustomScrollBar(Owner).OnScroll(Owner, VirtualPos);
      TFCustomScrollBar(Owner).FPosition := VirtualPos;
    end;

    Redraw;
    Result := True
  end else
    Result := False;
end;

procedure TFScrollBarSlider.CalcSizes;
begin
  if TFCustomScrollBar(Owner).Orientation = Horizontal then
    FDefSize.cx := Style.GetScrollBarBtnSize(Horizontal).cy * 5
  else
    FDefSize.cy := Style.GetScrollBarBtnSize(Vertical).cx * 5;
end;

function TFScrollBarSlider.CalcPosition: Integer;
var
  Size: Integer;
  lOwner: TFCustomScrollBar;
begin
  Assert(Owner is TFCustomScrollBar);
  lOwner := TFCustomScrollBar(Owner);

  if lOwner.Orientation = Horizontal then
    Size := Width
  else
    Size := Height;

  if Size = ButtonSize then
    lOwner.Position := 0
  else
  begin
    if lOwner.PageSize = 0 then
	      Result := ButtonPos * (lOwner.Max - lOwner.Min + 1)
    else
	      Result := ButtonPos * (lOwner.Max - lOwner.Min - lOwner.PageSize + 2);
    Result := Result div (Size - ButtonSize);
    Result := Result + lOwner.Min;
  end;
  Result := lOwner.ClipPosition(Result);
end;


// -------------------------------------------------------------------
//   TFCustomScrollBar
// -------------------------------------------------------------------

constructor TFCustomScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Include(WidgetStyle, wsOpaque);
  Embedded := False;

  FMax          := 100;
  FSmallChange  := 1;

  ButtonUp := TFScrollBarButton.Create(Self);
  ButtonUp.Name               := '#ScrollBarButtonUp';
  TFScrollBarButton(ButtonUp).Direction := arrowLeft;
  ButtonUp.Embedded           := True;
  ButtonUp.CanExpandWidth     := False;
  ButtonUp.CanExpandHeight    := False;
  ButtonUp.OnClick            := @ButtonUpClick;
  ButtonUp.SetEmbeddedParent(Self);

  Slider := TFScrollBarSlider.Create(Self);
  Slider.Name                 := '#ScrollBarSlider';
  Slider.SetEmbeddedParent(Self);

  ButtonDown := TFScrollBarButton.Create(Self);
  ButtonDown.Name             := '#ScrollBarButtonDown';
  TFScrollBarButton(ButtonDown).Direction := arrowRight;
  ButtonDown.Embedded         := True;
  ButtonDown.CanExpandWidth   := False;
  ButtonDown.CanExpandHeight  := False;
  ButtonDown.OnClick          := @ButtonDownClick;
  ButtonDown.SetEmbeddedParent(Self);
end;

procedure TFCustomScrollBar.LineUp;
begin
  Position := Position - SmallChange;
end;

procedure TFCustomScrollBar.LineDown;
begin
  Position := Position + SmallChange;
end;

procedure TFCustomScrollBar.PageUp;
var
  Diff: Integer;
begin
  if LargeChange = 0 then
  begin
    Diff := (Max - Min + 6) div 10;
    if Diff = 0 then
      Inc(Diff);
    Position := Position - Diff;
  end else
    Position := Position - LargeChange;
end;


procedure TFCustomScrollBar.PageDown;
var
  Diff: Integer;
begin
  if LargeChange = 0 then
  begin
    Diff := (Max - Min + 6) div 10;
    if Diff = 0 then
      Inc(Diff);
    Position := Position + Diff;
  end else
    Position := Position + LargeChange;
end;


function TFCustomScrollBar.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := Event.SendToChild(Slider) or Event.SendToChild(ButtonUp) or
    Event.SendToChild(ButtonDown);
end;


procedure TFCustomScrollBar.Paint(Canvas: TFCanvas);
begin
  if not Embedded then
    Style.DrawScrollBarBorder(Canvas, Rect(0, 0, Width, Height));
end;


procedure TFCustomScrollBar.CalcSizes;
begin
  if Orientation = Horizontal then
  begin
    FMinSize.cx := ButtonUp.DefSize.cx + Slider.MinSize.cx + ButtonDown.DefSize.cx;
    FMinSize.cy := ButtonUp.DefSize.cy;
    FDefSize.cx := ButtonUp.DefSize.cx + Slider.DefSize.cx + ButtonDown.DefSize.cx;
    FDefSize.cy := ButtonUp.DefSize.cy;
    FMaxSize.cx := InfiniteSize;
    FMaxSize.cy := ButtonUp.DefSize.cy;
  end else
  begin
    FMinSize.cx := ButtonUp.DefSize.cx;
    FMinSize.cy := ButtonUp.DefSize.cy + Slider.MinSize.cy + ButtonDown.DefSize.cy;
    FDefSize.cx := ButtonUp.DefSize.cx;
    FDefSize.cy := ButtonUp.DefSize.cy + Slider.DefSize.cy + ButtonDown.DefSize.cy;
    FMaxSize.cx := ButtonUp.DefSize.cx;
    FMaxSize.cy := InfiniteSize;
  end;

  if not Embedded then
    with Style.GetScrollBarBorders(Orientation) do
    begin
      Inc(FMinSize.cx, Left + Right);
      Inc(FMinSize.cy, Top + Bottom);
      FDefSize.cx := Math.Min(DefSize.cx + Left + Right, InfiniteSize);
      FDefSize.cy := Math.Min(DefSize.cy + Top + Bottom, InfiniteSize);
      FMaxSize.cx := Math.Min(MaxSize.cx + Left + Right, InfiniteSize);
      FMaxSize.cy := Math.Min(MaxSize.cy + Top + Bottom, InfiniteSize);
    end;
end;


procedure TFCustomScrollBar.Resized;
var
  r: TRect;
begin
  if not Embedded then
    with Style.GetScrollBarBorders(Orientation) do
    begin
      r.Left := Left;
      r.Top := Top;
      r.Right := Width - Right;
      r.Bottom := Height - Bottom;
    end
  else
  begin
    r.Left := 0;
    r.Top := 0;
    r.Right := Width;
    r.Bottom := Height;
  end;

  with r do
    if Orientation = Horizontal then
    begin
      ButtonUp.SetBounds(TopLeft, Size(ButtonUp.DefSize.cx, Bottom - Top));
      ButtonDown.SetBounds(Point(Right - ButtonDown.DefSize.cx, Top),
          Size(ButtonDown.DefSize.cx, Bottom - Top));
      Slider.SetBounds(Point(Left + ButtonUp.DefSize.cx, Top),
          Size(Right - Left - ButtonUp.DefSize.cx - ButtonDown.DefSize.cx,
	          Bottom - Top));
    end
    else
    begin
      ButtonUp.SetBounds(TopLeft, Size(Right - Left, ButtonDown.DefSize.cy));
      ButtonDown.SetBounds(Point(Left, Bottom - ButtonDown.DefSize.cy),
          Size(Right - Left, ButtonDown.DefSize.cy));
      Slider.SetBounds(Point(Left, Top + ButtonUp.DefSize.cy),
          Size(Right - Left,
            Bottom - Top - ButtonUp.DefSize.cy - ButtonDown.DefSize.cy));
    end;

  UpdateBar;
end;


procedure TFCustomScrollBar.ButtonUpClick(Sender: TObject);
begin
  LineUp;
end;


procedure TFCustomScrollBar.ButtonDownClick(Sender: TObject);
begin
  LineDown;
end;


procedure TFCustomScrollBar.SetOrientation(AOrientation: TOrientation);
begin
  if AOrientation <> Orientation then
  begin
    FOrientation := AOrientation;
    if Orientation = Horizontal then
    begin
      TFScrollBarButton(ButtonUp).Direction := arrowLeft;
      TFScrollBarButton(ButtonDown).Direction := arrowRight;
    end else
    begin
      TFScrollBarButton(ButtonUp).Direction := arrowUp;
      TFScrollBarButton(ButtonDown).Direction := arrowDown;
    end;
  end;
end;


procedure TFCustomScrollBar.SetMin(AMin: Integer);
begin
  if AMin <> FMin then
  begin
    FMin := AMin;
    Position := Position;	// Do range clipping
    UpdateBar;
  end;
end;


procedure TFCustomScrollBar.SetMax(AMax: Integer);
begin
  if AMax <> FMax then
  begin
    FMax := AMax;
    Position := Position;	// Do range clipping
    UpdateBar;
  end;
end;


procedure TFCustomScrollBar.SetPageSize(APageSize: Integer);
begin
  if FPageSize <> APageSize then
  begin
    FPageSize := APageSize;
    Position := Position;	// Do range clipping
    UpdateBar;
  end;
end;


procedure TFCustomScrollBar.SetPosition(APosition: Integer);
begin
  APosition := ClipPosition(APosition);

  if (APosition <> Position) and Assigned(OnScroll) then
    OnScroll(Self, APosition);

  if APosition <> Position then
  begin
    FPosition := APosition;
    UpdateBar;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;


function TFCustomScrollBar.GeTFButtonSize: Integer;
var
  Size: Integer;
begin
  if PageSize = 0 then
    if Orientation = Horizontal then
      Result := Height
    else
      Result := Width
  else
  begin
    if Orientation = Horizontal then
      Size := Slider.Width
    else
      Size := Slider.Height;
    Result := Math.Max(Style.GetScrollBarBtnMinSize,
        PageSize * Size div Math.Max(1, Max - Min + 1));
    if Result > Size then
      Result := Size;
  end;
end;


function TFCustomScrollBar.ClipPosition(APosition: Integer): Integer;
begin
  if APosition > (Max - PageSize) then
  begin
    if PageSize = 0 then
      Result := Max
    else
      Result := Max - PageSize + 1;
  end
  else
    Result := APosition;
  if Result < Min then
    Result := Min;
end;


procedure TFCustomScrollBar.UpdateBar;
begin
  if Embedded then
    Visible := (Max > Min) and ((PageSize = 0) or (PageSize <= Max - Min));
  TFScrollBarSlider(Slider).UpdateBar;
end;


{$ENDIF read_implementation}

