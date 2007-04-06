{%mainunit fpgui.pas}

{ Listbox widget implementation }

{$IFDEF read_interface}

  TCustomListBox = class(TWidget)
  private
    FHotTrack: Boolean;
    FItems: TStrings;
    FItemIndex: Integer;
    function    EvMousePressed(Event: TMousePressedEventObj): Boolean;
    function    EvMouseReleased(Event: TMouseReleasedEventObj): Boolean;
    function    EvMouseMoved(Event: TMouseMoveEventObj): Boolean;
    function    ProcessMouseEvent(Event: TMouseEventObj): Boolean;
  protected
    ScrollingSupport: TScrollingSupport;
    FMaxItemWidth: Integer;
    ItemHeight: Integer;
    procedure   Paint(Canvas: TFCanvas); override;
    function    ProcessEvent(Event: TEventObj): Boolean; override;
    function    DistributeEvent(Event: TEventObj): Boolean; override;
//    procedure EvKeyPressed(Key: Word; Shift: TShiftState); override;
    procedure   CalcSizes; override;
    procedure   Resized; override;
    procedure   RecalcWidth;
    procedure   UpdateScrollBars;
    procedure   RedrawItem(AIndex: Integer);
    property    CanExpandWidth default True;
    property    CanExpandHeight default True;
    property    HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property    ItemIndex: Integer read FItemIndex write FItemIndex default -1;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Items: TStrings read FItems write FItems;
  end;


  TListBox = class(TCustomListBox)
  published
    // TWidget properties
    property    OnClick;
    property    Enabled;
    // TCustomListBox properties
    property    HotTrack;
    property    Items;
    property    ItemIndex;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}

// ===================================================================
//   TListBoxStrings
// ===================================================================

type

  TListBoxStrings = class(TStringList)
  protected
    ListBox: TCustomListBox;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(AListBox: TCustomListBox);
    function Add(const s: String): Integer; override;
  end;


constructor TListBoxStrings.Create(AListBox: TCustomListBox);
begin
  inherited Create;
  ListBox := AListBox;
end;

function TListBoxStrings.Add(const s: String): Integer;
var
  ItemWidth: Integer;
begin
  Result := inherited Add(s);
  if Assigned(ListBox.FindForm) and Assigned(ListBox.FindForm.Wnd) then
  begin
    ItemWidth := ListBox.FindForm.Wnd.Canvas.TextWidth(s) + 4;
    if ItemWidth > ListBox.FMaxItemWidth then
      ListBox.FMaxItemWidth := ItemWidth;
    ListBox.UpdateScrollBars;
  end;
end;

procedure TListBoxStrings.SetUpdateState(Updating: Boolean);
begin
  if not Updating then
    ListBox.RecalcWidth;
end;


// ===================================================================
//   TCustomListBox
// ===================================================================

constructor TCustomListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  FCanExpandWidth := True;
  FCanExpandHeight := True;

  ScrollingSupport := TScrollingSupport.Create(Self);
  ScrollingSupport.HorzScrollBar.OnScroll :=
    @ScrollingSupport.DefHorzScrollHandler;
  ScrollingSupport.VertScrollBar.OnScroll :=
    @ScrollingSupport.DefVertScrollHandler;
  Items := TListBoxStrings.Create(Self);
  FItemIndex := -1;
  UpdateScrollBars;
end;

destructor TCustomListBox.Destroy;
begin
  Items.Free;
  ScrollingSupport.Free;
  inherited Destroy;
end;


// protected methods

procedure TCustomListBox.Paint(Canvas: TFCanvas);
var
  i, StartIndex, EndIndex: Integer;
  ItemRect: TRect;
  ItemFlags: TItemFlags;
begin
  inherited Paint(Canvas);

  if not Canvas.IntersectClipRect(ScrollingSupport.ClientRect) then
    exit; //==>

  Style.SetUIColor(Canvas, clWindow);
//  Style.DrawWindowBackground(Canvas, ScrollingSupport.ClientRect);
  Canvas.FillRect(ScrollingSupport.ClientRect);
  Style.SetUIColor(Canvas, clWindowText);

  with ScrollingSupport.VertScrollBar do
  begin
    StartIndex := Position div ItemHeight;
    EndIndex := (Position + PageSize) div ItemHeight;
  end;

  Canvas.AppendTranslation(ScrollingSupport.ClientRect.TopLeft -
    ScrollingSupport.ScrollPos);

  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex >= Items.Count then
    EndIndex := Items.Count - 1;

  for i := StartIndex to EndIndex do
  begin
    Canvas.SaveState;

    ItemRect.Left := ScrollingSupport.HorzScrollBar.Position;
    ItemRect.Top := i * ItemHeight;
    ItemRect.Right := ScrollingSupport.ClientRect.Right -
      ScrollingSupport.ClientRect.Left +
      ScrollingSupport.HorzScrollBar.Position;
    ItemRect.Bottom := (i + 1) * ItemHeight;

    Canvas.IntersectClipRect(ItemRect);

    ItemFlags := [];
    if (wsHasFocus in WidgetState) and ((i = ItemIndex) or
      ((ItemIndex = -1) and (i = 0))) then
      Include(ItemFlags, ifFocused);
    if i = ItemIndex then
      Include(ItemFlags, ifSelected);

    Style.DrawItemBefore(Canvas, ItemRect, ItemFlags);
//    Canvas.TextOut(Point(2, i * ItemHeight), Items[i]);
    Style.DrawText(Canvas, Point(2, i * ItemHeight), Items[i], WidgetState);
    Style.DrawItemAfter(Canvas, ItemRect, ItemFlags);

    Canvas.RestoreState;
  end;
end;

function TCustomListBox.ProcessEvent(Event: TEventObj): Boolean;
begin
  if Event.InheritsFrom(TMousePressedEventObj) then
    Result := ScrollingSupport.ProcessEvent(Event) or
      EvMousePressed(TMousePressedEventObj(Event)) or
      inherited ProcessEvent(Event)
  else if Event.InheritsFrom(TMouseReleasedEventObj) then
    Result := ScrollingSupport.ProcessEvent(Event) or
      EvMouseReleased(TMouseReleasedEventObj(Event)) or
      inherited ProcessEvent(Event)
  else if Event.InheritsFrom(TMouseMoveEventObj) then
    Result := ScrollingSupport.ProcessEvent(Event) or
      EvMouseMoved(TMouseMoveEventObj(Event)) or
      inherited ProcessEvent(Event)
  else
    Result := ScrollingSupport.ProcessEvent(Event) or
      inherited ProcessEvent(Event);
end;

function TCustomListBox.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := ScrollingSupport.DistributeEvent(Event) or
    inherited DistributeEvent(Event);
end;

procedure TCustomListBox.CalcSizes;
begin
  ScrollingSupport.CalcSizes;
  ItemHeight := FindForm.Wnd.Canvas.FontCellHeight;
  ScrollingSupport.VertScrollBar.SmallChange := ItemHeight;
  RecalcWidth;
end;

procedure TCustomListBox.Resized;
begin
  ScrollingSupport.Resized;
  UpdateScrollBars;
end;

procedure TCustomListBox.RecalcWidth;
var
  i, ItemWidth: Integer;
begin
  if (not Assigned(FindForm)) or (not Assigned(FindForm.Wnd)) then
    exit; //==>

  FMaxItemWidth := 0;
  for i := 0 to Items.Count - 1 do
  begin
    ItemWidth := FindForm.Wnd.Canvas.TextWidth(Items[i]) + 4;
    if ItemWidth > FMaxItemWidth then
      FMaxItemWidth := ItemWidth;
  end;
  UpdateScrollBars;
end;

procedure TCustomListBox.UpdateScrollBars;
begin
  ScrollingSupport.SetVirtualSize(
      Size(FMaxItemWidth, Items.Count * ItemHeight - 1));
end;

procedure TCustomListBox.RedrawItem(AIndex: Integer);
var
  ItemRect: TRect;
begin
  if AIndex < 0 then
    exit; //==>
  ItemRect := ScrollingSupport.ClientRect;
  Inc(ItemRect.Top, AIndex * ItemHeight -
    ScrollingSupport.VertScrollBar.Position);
  if (ItemRect.Top > ScrollingSupport.ClientRect.Bottom) or
    (ItemRect.Top + ItemHeight <= ScrollingSupport.ClientRect.Top) then
    exit;
  ItemRect.Bottom := Min(ItemRect.Top + ItemHeight,
    ScrollingSupport.ClientRect.Bottom);
  Redraw(ItemRect);
end;


// private methods

function TCustomListBox.EvMousePressed(Event: TMousePressedEventObj): Boolean;
begin
  if HotTrack then
    Result := False
  else if Event.Button = mbLeft then
    Result := ProcessMouseEvent(Event)
  else
    Result := False;
end;

function TCustomListBox.EvMouseReleased(Event: TMouseReleasedEventObj): Boolean;
begin
  if HotTrack and (Event.Button = mbLeft) then
    Result := ProcessMouseEvent(Event)
  else
    Result := False;
end;

function TCustomListBox.EvMouseMoved(Event: TMouseMoveEventObj): Boolean;
begin
  if HotTrack then
    Result := ProcessMouseEvent(Event)
  else
    Result := False;
end;

function TCustomListBox.ProcessMouseEvent(Event: TMouseEventObj): Boolean;
var
  Index: Integer;
begin
  if not PtInRect(ScrollingSupport.ClientRect, Event.Position) then
  begin
    Result := False;
    exit;
  end;

  Index := (Event.Position.y - ScrollingSupport.ClientRect.Top +
    ScrollingSupport.VertScrollBar.Position) div ItemHeight;
  if (Index >= 0) and (Index < Items.Count) and ((Index <> ItemIndex) or
    (HotTrack and Event.InheritsFrom(TMouseReleasedEventObj))) then
  begin
    RedrawItem(ItemIndex);
    FItemIndex := Index;
    RedrawItem(ItemIndex);
    if (not Event.InheritsFrom(TMouseMoveEventObj)) and Assigned(OnClick) then
      OnClick(Self);
  end;

{ !!!: Re-include this for correct focus handling. But at the moment a focus
  change results in a complete widget redraw, which is not very brilliant. }
//  inherited ProcessEvent(Event);

  Result := True;
end;


{$ENDIF read_implementation}

