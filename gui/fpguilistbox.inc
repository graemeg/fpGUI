{
    fpGUI  -  Free Pascal GUI Library

    ListBox class declarations

    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{%mainunit fpgui.pas}

{ Listbox widget implementation }

{$IFDEF read_interface}

  { TFCustomListBox }

  TFCustomListBox = class(TFWidget)
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
    procedure   EvKeyPressed(Key: Word; Shift: TShiftState); override;
    procedure   CalcSizes; override;
    procedure   Resized; override;
    procedure   RecalcWidth;
    procedure   UpdateScrollBars;
    function    RedrawItem(AIndex: Integer): TRect;
    property    CanExpandWidth default True;
    property    CanExpandHeight default True;
    property    HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property    ItemIndex: Integer read FItemIndex write FItemIndex default -1;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Items: TStrings read FItems write FItems;
  end;


  TFListBox = class(TFCustomListBox)
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
//   TFListBoxStrings
// ===================================================================

type

  TFListBoxStrings = class(TStringList)
  protected
    ListBox: TFCustomListBox;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(AListBox: TFCustomListBox);
    function Add(const s: String): Integer; override;
  end;


constructor TFListBoxStrings.Create(AListBox: TFCustomListBox);
begin
  inherited Create;
  ListBox := AListBox;
end;

function TFListBoxStrings.Add(const s: String): Integer;
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

procedure TFListBoxStrings.SetUpdateState(Updating: Boolean);
begin
  if not Updating then
    ListBox.RecalcWidth;
end;


// ===================================================================
//   TCustomListBox
// ===================================================================

constructor TFCustomListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  FCanExpandWidth := True;
  FCanExpandHeight := True;

  ScrollingSupport := TScrollingSupport.Create(Self);
  ScrollingSupport.HorzScrollBar.OnScroll := @ScrollingSupport.DefHorzScrollHandler;
  ScrollingSupport.VertScrollBar.OnScroll := @ScrollingSupport.DefVertScrollHandler;
  Items := TFListBoxStrings.Create(Self);
  FItemIndex := -1;
  UpdateScrollBars;
end;

destructor TFCustomListBox.Destroy;
begin
  Items.Free;
  ScrollingSupport.Free;
  inherited Destroy;
end;

procedure TFCustomListBox.Paint(Canvas: TFCanvas);
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

function TFCustomListBox.ProcessEvent(Event: TEventObj): Boolean;
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

function TFCustomListBox.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := ScrollingSupport.DistributeEvent(Event) or
    inherited DistributeEvent(Event);
end;

procedure TFCustomListBox.EvKeyPressed(Key: Word; Shift: TShiftState);
var
  mshift: TShiftState;
  HorzScrollBar: TFScrollBar;
  VertScrollBar: TFScrollBar;
  r: TRect;
begin
//  writeln('Before FItemIndex=' + IntToStr(FItemIndex));
  HorzScrollBar := ScrollingSupport.HorzScrollBar;
  VertScrollBar := ScrollingSupport.VertScrollBar;
  
  mshift := Shift * [ssShift, ssAlt, ssCtrl, ssMeta, ssSuper, ssHyper, ssAltGr];
  if mshift = [] then
    case Key of
//      keyLeft:
//        HorzScrollBar.ButtonUpClick(nil);
//      keyRight:
//        HorzScrollBar.ButtonDownClick(nil);
      keyUp:
          begin
//            writeln('keyup');
            if FItemIndex > 0 then
            begin
              RedrawItem(ItemIndex);
              Dec(FItemIndex);
              r := RedrawItem(ItemIndex);
              // we should only call this when focus rect is out of view
              if not PtInRect(ScrollingSupport.ClientRect, Point(r.Left, r.Top)) then
              begin
                VertScrollBar.ButtonUpClick(nil);
              end;
            end;
          end;
      keyDown:
          begin
//            writeln('keydown');
            if FItemIndex < (Items.Count - 1) then
            begin
              RedrawItem(ItemIndex);
              Inc(FItemIndex);
              r := RedrawItem(ItemIndex);
              // we should only call this when focus rect is out of view
              if not PtInRect(ScrollingSupport.ClientRect, Point(r.Left, r.Bottom)) then
              begin
                VertScrollBar.ButtonDownClick(nil);
              end;
            end;
          end;
      keyPageUp:
        VertScrollBar.PageUp;
      keyPageDown:
        VertScrollBar.PageDown;
      keyHome:
          begin
            RedrawItem(ItemIndex);
            FItemIndex := 0;
            RedrawItem(ItemIndex);
            VertScrollBar.Position := 0;
          end;
      keyEnd:
          begin
            RedrawItem(ItemIndex);
            FItemIndex := (Items.Count - 1);
            RedrawItem(ItemIndex);
            VertScrollBar.Position := VertScrollBar.Max - VertScrollBar.PageSize;
          end;
      keyReturn:
          begin
            if Assigned(OnClick) then
              OnClick(Self);
          end;
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

//  writeln('After FItemIndex=' + IntToStr(FItemIndex));
end;

procedure TFCustomListBox.CalcSizes;
begin
  ScrollingSupport.CalcSizes;
  ItemHeight := FindForm.Wnd.Canvas.FontCellHeight;
  ScrollingSupport.VertScrollBar.SmallChange := ItemHeight;
  RecalcWidth;
end;

procedure TFCustomListBox.Resized;
begin
  ScrollingSupport.Resized;
  UpdateScrollBars;
end;

procedure TFCustomListBox.RecalcWidth;
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

procedure TFCustomListBox.UpdateScrollBars;
begin
  ScrollingSupport.SetVirtualSize(
      Size(FMaxItemWidth, Items.Count * ItemHeight - 1));
end;

function TFCustomListBox.RedrawItem(AIndex: Integer): TRect;
var
  ItemRect: TRect;
begin
  if AIndex < 0 then
    Exit; //==>
  ItemRect := ScrollingSupport.ClientRect;
  Inc(ItemRect.Top, AIndex * ItemHeight - ScrollingSupport.VertScrollBar.Position);
  if (ItemRect.Top > ScrollingSupport.ClientRect.Bottom) or
    (ItemRect.Top + ItemHeight <= ScrollingSupport.ClientRect.Top) then
    Exit; //==>
  ItemRect.Bottom := Min(ItemRect.Top + ItemHeight, ScrollingSupport.ClientRect.Bottom);
  Redraw(ItemRect);
  Result := ItemRect;
end;


// private methods

function TFCustomListBox.EvMousePressed(Event: TMousePressedEventObj): Boolean;
begin
  if HotTrack then
    Result := False
  else if Event.Button = mbLeft then
    Result := ProcessMouseEvent(Event)
  else
    Result := False;
end;

function TFCustomListBox.EvMouseReleased(Event: TMouseReleasedEventObj): Boolean;
begin
  if HotTrack and (Event.Button = mbLeft) then
    Result := ProcessMouseEvent(Event)
  else
    Result := False;
end;

function TFCustomListBox.EvMouseMoved(Event: TMouseMoveEventObj): Boolean;
begin
  if HotTrack then
    Result := ProcessMouseEvent(Event)
  else
    Result := False;
end;

function TFCustomListBox.ProcessMouseEvent(Event: TMouseEventObj): Boolean;
var
  Index: Integer;
begin
  if not PtInRect(ScrollingSupport.ClientRect, Event.Position) then
  begin
    Result := False;
    Exit; //==>
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

