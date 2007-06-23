{
    fpGUI  -  Free Pascal GUI Library

    Memo class declarations

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


  { TFCustomMemo }

  TFCustomMemo = class(TFWidget)
  private
    FLines: TStrings;
    function    EvMousePressed(Event: TMousePressedEventObj): Boolean;
    function    EvMouseReleased(Event: TMouseReleasedEventObj): Boolean;
    function    EvMouseMoved(Event: TMouseMoveEventObj): Boolean;
    function    ProcessMouseEvent(Event: TMouseEventObj): Boolean;
  protected
    FScrollingSupport: TScrollingSupport;
    FMaxItemWidth: Integer;
    FItemHeight: Integer;
    procedure   SetLines(const AValue: TStrings);
    procedure   Paint(Canvas: TFCanvas); override;
    function    ProcessEvent(Event: TEventObj): Boolean; override;
    function    DistributeEvent(Event: TEventObj): Boolean; override;
    procedure   CalcSizes; override;
    procedure   UpdateScrollBars;
    procedure   RecalcWidth;
    procedure   Resized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Lines: TStrings read FLines write SetLines;
  end;


  TFMemo = class(TFCustomMemo)
  published
//    property Alignment;
//    property BorderStyle;
//    property Color;
    property Enabled;
    property Lines;
//    property MaxLength;
  end;
{$ENDIF read_interface}



{$IFDEF read_implementation}

type

  TFMemoStrings = class(TStringList)
  protected
    Memo: TFCustomMemo;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(AMemo: TFCustomMemo);
    function Add(const s: String): Integer; override;
  end;


constructor TFMemoStrings.Create(AMemo: TFCustomMemo);
begin
  inherited Create;
  Memo := AMemo;
end;

function TFMemoStrings.Add(const s: String): Integer;
var
  ItemWidth: Integer;
begin
  Result := inherited Add(s);
  if Assigned(Memo.FindForm) and Assigned(Memo.FindForm.Wnd) then
  begin
    ItemWidth := Memo.FindForm.Wnd.Canvas.TextWidth(s) + 4;
    if ItemWidth > Memo.FMaxItemWidth then
      Memo.FMaxItemWidth := ItemWidth;
    Memo.UpdateScrollBars;
  end;
end;

procedure TFMemoStrings.SetUpdateState(Updating: Boolean);
begin
  if not Updating then
    Memo.RecalcWidth;
end;


{ TFCustomMemo }

function TFCustomMemo.EvMousePressed(Event: TMousePressedEventObj): Boolean;
begin

end;

function TFCustomMemo.EvMouseReleased(Event: TMouseReleasedEventObj): Boolean;
begin

end;

function TFCustomMemo.EvMouseMoved(Event: TMouseMoveEventObj): Boolean;
begin

end;

function TFCustomMemo.ProcessMouseEvent(Event: TMouseEventObj): Boolean;
var
  Index: Integer;
begin
  if not PtInRect(FScrollingSupport.ClientRect, Event.Position) then
  begin
    Result := False;
    exit;
  end;
{ // Graeme: TODO

  Index := (Event.Position.y - FScrollingSupport.ClientRect.Top +
    FScrollingSupport.VerTFScrollBar.Position) div FItemHeight;
  if (Index >= 0) and (Index < FLines.Count) and ((Index <> FItemIndex) or
    (HotTrack and Event.InheritsFrom(TMouseReleasedEventObj))) then
  begin
    RedrawItem(ItemIndex);
    FItemIndex := Index;
    RedrawItem(ItemIndex);
    if (not Event.InheritsFrom(TMouseMoveEventObj)) and Assigned(OnClick) then
      OnClick(Self);
  end;
}
  Result := True;
end;

procedure TFCustomMemo.SetLines(const AValue: TStrings);
begin
  FLines.Assign(AValue);
end;

procedure TFCustomMemo.Paint(Canvas: TFCanvas);
var
  i, StartIndex, EndIndex: Integer;
  ItemRect: TRect;
  ItemFlags: TItemFlags;
begin
  inherited Paint(Canvas);

  if not Canvas.IntersectClipRect(FScrollingSupport.ClientRect) then
    Exit; //==>

  Style.SetUIColor(Canvas, clWindow);
  Canvas.FillRect(FScrollingSupport.ClientRect);
  Style.SetUIColor(Canvas, clWindowText);

  with FScrollingSupport.VertScrollBar do
  begin
    StartIndex  := Position div FItemHeight;
    EndIndex    := (Position + PageSize) div FItemHeight;
  end;

  Canvas.AppendTranslation(FScrollingSupport.ClientRect.TopLeft - FScrollingSupport.ScrollPos);

  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex >= FLines.Count then
    EndIndex := FLines.Count - 1;

  for i := StartIndex to EndIndex do
  begin
    Canvas.SaveState;

    ItemRect.Left := FScrollingSupport.HorzScrollBar.Position;
    ItemRect.Top := i * FItemHeight;
    ItemRect.Right := FScrollingSupport.ClientRect.Right
        - FScrollingSupport.ClientRect.Left
        + FScrollingSupport.HorzScrollBar.Position;
    ItemRect.Bottom := (i + 1) * FItemHeight;

    Canvas.IntersectClipRect(ItemRect);

    ItemFlags := [];
//    if (wsHasFocus in WidgetState) and ((i = ItemIndex) or
//      ((ItemIndex = -1) and (i = 0))) then
//      Include(ItemFlags, ifFocused);
//    if i = ItemIndex then
//      Include(ItemFlags, ifSelected);

//    Style.DrawItemBefore(Canvas, ItemRect, ItemFlags);
    Style.DrawText(Canvas, Point(2, i * FItemHeight), FLines[i], WidgetState);
//    Style.DrawItemAfter(Canvas, ItemRect, ItemFlags);

    Canvas.RestoreState;
  end;
end;

function TFCustomMemo.ProcessEvent(Event: TEventObj): Boolean;
begin
  if Event.InheritsFrom(TMousePressedEventObj) then
    Result := FScrollingSupport.ProcessEvent(Event) or
      EvMousePressed(TMousePressedEventObj(Event)) or
      inherited ProcessEvent(Event)
  else if Event.InheritsFrom(TMouseReleasedEventObj) then
    Result := FScrollingSupport.ProcessEvent(Event) or
      EvMouseReleased(TMouseReleasedEventObj(Event)) or
      inherited ProcessEvent(Event)
  else if Event.InheritsFrom(TMouseMoveEventObj) then
    Result := FScrollingSupport.ProcessEvent(Event) or
      EvMouseMoved(TMouseMoveEventObj(Event)) or
      inherited ProcessEvent(Event)
  else
    Result := FScrollingSupport.ProcessEvent(Event) or
      inherited ProcessEvent(Event);
end;

function TFCustomMemo.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := FScrollingSupport.DistributeEvent(Event) or
    inherited DistributeEvent(Event);
end;

procedure TFCustomMemo.CalcSizes;
begin
  FScrollingSupport.CalcSizes;
  FItemHeight := FindForm.Wnd.Canvas.FontCellHeight;
  FScrollingSupport.VertScrollBar.SmallChange := FItemHeight;
  RecalcWidth;
end;

procedure TFCustomMemo.UpdateScrollBars;
begin
  FScrollingSupport.SetVirtualSize(Size(FMaxItemWidth, FLines.Count * FItemHeight - 1));
end;

procedure TFCustomMemo.RecalcWidth;
var
  i, ItemWidth: Integer;
begin
  if (not Assigned(FindForm)) or (not Assigned(FindForm.Wnd)) then
    Exit; //==>

  FMaxItemWidth := 0;
  for i := 0 to FLines.Count - 1 do
  begin
    ItemWidth := FindForm.Wnd.Canvas.TextWidth(FLines[i]) + 4;
    if ItemWidth > FMaxItemWidth then
      FMaxItemWidth := ItemWidth;
  end;
  UpdateScrollBars;
end;

procedure TFCustomMemo.Resized;
begin
  FScrollingSupport.Resized;
  UpdateScrollBars;
end;

constructor TFCustomMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  FCanExpandWidth := True;
  FCanExpandHeight := True;

  FScrollingSupport := TScrollingSupport.Create(Self);
  FScrollingSupport.HorzScrollBar.OnScroll := @FScrollingSupport.DefHorzScrollHandler;
  FScrollingSupport.VertScrollBar.OnScroll := @FScrollingSupport.DefVertScrollHandler;

  FLines := TFMemoStrings.Create(self);
//  SetBounds(10, 10, 180, 90);
  UpdateScrollBars;
end;

destructor TFCustomMemo.Destroy;
begin
  FLines.Free;
  FScrollingSupport.Free;
  inherited Destroy;
end;


{$ENDIF read_implementation}   


