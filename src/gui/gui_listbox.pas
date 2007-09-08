unit gui_listbox;

{$mode objfpc}{$H+}

{
  TODO:
    * Refactor these to have a better hierarchy
    * Only surface properties as published in TfpgListBox
    * Implement .BeginUpdate and .EndUpdate methods so we know when to refresh
      the items list.
}

interface

uses
  Classes,
  SysUtils,
  gfx_widget,
  gui_scrollbar,
  gfxbase,
  fpgfx;
  
type

  // My thinking was that we could use this class as the base class for anything
  // that contains a list and needs to be presented like a normal listBox.
  // Not sure if it is actually going to work.
  TfpgBaseListBox = class(TfpgWidget)
  private
    FHotTrack: boolean;
    FOnChange: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    FPopupFrame: boolean;
    function    GetFontDesc: string;
    procedure   SetFocusItem(const AValue: integer);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetPopupFrame(const AValue: boolean);
    procedure   UpdateScrollbarCoords;
  protected
    FFont: TfpgFont;
    FScrollBar: TfpgScrollBar;
    FFocusItem: integer;
    FMouseDragging: boolean;
    FFirstItem: integer;
    FMargin: integer;
    FBackgroundColor: TfpgColor;  // This should move to TfpgWidget
    procedure   SetFirstItem(item: integer);
    procedure   UpdateScrollBar;
    procedure   FollowFocus;
    function    ListHeight: TfpgCoord;
    function    ScrollBarWidth: TfpgCoord;
    function    PageLength: integer;
    procedure   ScrollBarMove(Sender: TObject; position : integer);
    procedure   DrawItem(num: integer; rect: TfpgRect; flags: integer); virtual;
    procedure   DoChange;
    procedure   DoSelect;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed : boolean); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure   HandleShow; override;
    procedure   HandlePaint; override;
    property    PopupFrame: boolean read FPopupFrame write SetPopupFrame;
    property    HotTrack: boolean read FHotTrack write FHotTrack;
    property    FocusItem: integer read FFocusItem write SetFocusItem;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Update;
    function    ItemCount: integer; virtual;
    function    RowHeight: integer; virtual;
    property    Font: TfpgFont read FFont;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
    property    OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;


  // Listbox containg strings - the normal listbox as we know it. Used by
  // component developers.
  TfpgTextListBox = class(TfpgBaseListBox)
  protected
    FItems: TStringList;
    FInternalItems: TStrings;
    procedure   DrawItem(num: integer; rect: TfpgRect; flags: integer); override;
    property    Items: TStringList read FItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    ItemCount: integer; override;
    function    Text: string;
  end;
  

  // The standard strings listbox we will actually use in a GUI.
  TfpgListBox = class(TfpgTextListBox)
  published
    property    FocusItem;
    property    FontDesc;
    property    HotTrack;
    property    Items;
    property    PopupFrame;
  end;


implementation

type
  // custom stringlist that will notify listbox of item changes
  TfpgListBoxStrings = class(TStringList)
  protected
    ListBox: TfpgTextListBox;
    procedure   SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(AListBox: TfpgTextListBox);
    function    Add(const s: String): Integer; override;
  end;

{ TfpgListBoxStrings }

procedure TfpgListBoxStrings.SetUpdateState(Updating: Boolean);
begin
  inherited SetUpdateState(Updating);
  // do nothing extra for now
end;

constructor TfpgListBoxStrings.Create(AListBox: TfpgTextListBox);
begin
  inherited Create;
  ListBox := AListBox;
end;

function TfpgListBoxStrings.Add(const s: String): Integer;
var
  ItemWidth: Integer;
begin
  Result := inherited Add(s);
  if Assigned(ListBox) and (ListBox.HasHandle) then
  begin
    ItemWidth := ListBox.Font.TextWidth(s) + 4;
//    if ItemWidth > ListBox.FMaxItemWidth then
//      ListBox.FMaxItemWidth := ItemWidth;
    ListBox.UpdateScrollBar;
  end;
end;


{ TfpgBaseListBox }

function TfpgBaseListBox.GetFontDesc: string;
begin
  result := FFont.FontDesc;
end;

procedure TfpgBaseListBox.SetFocusItem(const AValue: integer);
begin
  if FFocusItem = AValue then
    Exit; //==>
  FFocusItem := AValue;
  FollowFocus;
  UpdateScrollbar;
  RePaint;
end;

procedure TfpgBaseListBox.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  RePaint;
end;

procedure TfpgBaseListBox.SetPopupFrame(const AValue: boolean);
begin
  if FPopupFrame = AValue then
    Exit; //==>
  FPopupFrame := AValue;
  RePaint;
end;

procedure TfpgBaseListBox.UpdateScrollbarCoords;
var
  HWidth: integer;
  VHeight: integer;
begin
  VHeight := Height - 4;
  HWidth  := Width - 4;

  if FScrollBar.Visible then
    Dec(HWidth, FScrollBar.Width);

  FScrollBar.Top     := 2;
  FScrollBar.Left    := Width - FScrollBar.Width - 2;
  FScrollBar.Height  := VHeight;
  FScrollBar.UpdateWindowPosition;
end;

procedure TfpgBaseListBox.SetFirstItem(item: integer);
begin
  FFirstItem := item;
  UpdateScrollBar;
end;

procedure TfpgBaseListBox.UpdateScrollBar;
var
  pn : integer;
begin
  pn := PageLength;
  FScrollBar.Visible := PageLength < ItemCount;

  if FScrollBar.Visible then
  begin
    FScrollBar.Min := 1;
    if ItemCount <> 0 then
      FScrollBar.SliderSize := pn / ItemCount
    else
      FScrollBar.SliderSize := 1;
    FScrollBar.Max := ItemCount-pn+1;
    FScrollBar.Position := FFirstItem;
    FScrollBar.RepaintSlider;
  end;
end;

procedure TfpgBaseListBox.FollowFocus;
var
  n : integer;
  h : TfpgCoord;
begin
  if FFocusItem < FFirstItem then
  begin
    FFirstItem := FFocusItem;
    UpdateScrollBar;
  end
  else
  begin
    h := 0;
    for n := FFocusItem downto FFirstItem do
    begin
      h := h + RowHeight;
      if h > ListHeight then
      begin
        FFirstItem := n+1;
        UpdateScrollBar;
        break;
      end;
    end;
  end;
end;

function TfpgBaseListBox.ListHeight: TfpgCoord;
begin
  result := height - (2*FMargin);
end;

function TfpgBaseListBox.ScrollBarWidth: TfpgCoord;
begin
  if FScrollBar.Visible then
    result := FScrollBar.Width
  else
    result := 0;
end;

function TfpgBaseListBox.PageLength: integer;
begin
  result := Trunc(ListHeight / RowHeight);
end;

procedure TfpgBaseListBox.ScrollBarMove(Sender: TObject; position: integer);
begin
  FFirstItem := position;
  Repaint;
end;

procedure TfpgBaseListBox.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TfpgBaseListBox.DoSelect;
begin
  if Assigned(FOnSelect) then
    FOnSelect(self);
end;

procedure TfpgBaseListBox.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  consumed := true;

  case keycode of
    keyUp:
           begin
             if FFocusItem > 1 then
             begin
               dec(FFocusItem);
               FollowFocus;
               RePaint;
               DoChange;
             end;
           end;
           
    keyDown:
           begin
             if FFocusItem < ItemCount then
             begin
               inc(FFocusItem);
               FollowFocus;
               RePaint;
               DoChange;
             end;
           end;

    keyPageUp:
           begin
             dec(FFocusItem,PageLength);
             if FFocusItem < 1 then FFocusItem := 1;
             FollowFocus;
             RePaint;
             DoChange;
           end;

    keyPageDown:
           begin
             inc(FFocusItem,PageLength);
             if FFocusItem > ItemCount then FFocusItem := ItemCount;
             FollowFocus;
             RePaint;
             DoChange;
           end;

    keyHome:
           begin
             FFocusItem := 1;
             FollowFocus;
             RePaint;
             DoChange;
           end;

    keyEnd:
           begin
             FFocusItem := ItemCount;
             FollowFocus;
             RePaint;
             DoChange;
           end;

    keyReturn:
           begin
             DoSelect;
             consumed := false; // to allow the forms to detect it
           end;
  else
    begin
      consumed := false;
    end;
  end;
  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfpgBaseListBox.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  if ItemCount < 1 then
    Exit; //==>

  FFocusItem := FFirstItem + Trunc((y - FMargin) / RowHeight);
  if FFocusItem > ItemCount then
    FFocusItem := ItemCount;

  FollowFocus;
  FMouseDragging := true;
  Repaint;
  DoChange;
end;

procedure TfpgBaseListBox.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  if ItemCount < 1 then
    Exit; //==>

  FMouseDragging := False;

  FFocusItem := FFirstItem + Trunc((y - FMargin) / RowHeight);
  if FFocusItem > ItemCount then
    FFocusItem := ItemCount;

  FollowFocus;
  Repaint;
  DoChange;
  DoSelect;
end;

procedure TfpgBaseListBox.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  oldf: integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  if ItemCount < 1 then
    Exit; //==>

  if ((not FMouseDragging) or (btnstate and 1 = 0)) and (not HotTrack) then
    Exit; //==>

  oldf := FFocusItem;

  FFocusItem := FFirstItem + Trunc((y - FMargin) / RowHeight);
  if FFocusItem > ItemCount then
    FFocusItem := ItemCount;

  if oldf <> FFocusItem then
  begin
    FollowFocus;
    Repaint;
  end;
end;

procedure TfpgBaseListBox.HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint);
var
  pfi: integer;
begin
  pfi := FFirstItem;
  if delta > 0 then   // scroll down
    FFirstItem := FFirstItem + abs(delta)
  else                // scroll up
    FFirstItem := FFirstItem - abs(delta);

  if FFirstItem + PageLength > ItemCount then
    FFirstItem := ItemCount - PageLength + 1;
  if FFirstItem < 1 then
    FFirstItem := 1;
  if pfi <> FFirstItem then
  begin
    UpdateScrollBar;
    Repaint;
  end;
end;

procedure TfpgBaseListBox.HandleShow;
begin
  inherited HandleShow;
//  if (csDesigning in ComponentState) then
//    Exit;
  if (csLoading in ComponentState) then
    Exit;
  UpdateScrollBarCoords;
  UpdateScrollBar;
end;

procedure TfpgBaseListBox.HandlePaint;
var
  n: integer;
  r: TfpgRect;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;
  Canvas.ClearClipRect;
  
  r.SetRect(0, 0, Width, Height);

  if popupframe then
  begin
    Canvas.SetLineStyle(1, lsSolid);
    Canvas.SetColor(clWidgetFrame);
    Canvas.DrawRectangle(r);
    InflateRect(r, -1, -1);
  end
  else
  begin
    Canvas.DrawControlFrame(r);
    InflateRect(r, -2, -2);
  end;

  Canvas.SetClipRect(r);
  Canvas.SetColor(FBackgroundColor);
  Canvas.FillRectangle(r);
  Canvas.SetFont(FFont);

  r.SetRect(0, 0, Width-ScrollBarWidth, Height);
  InflateRect(r, -FMargin, -FMargin);
//  r.SetRect(FMargin, FMargin, Width-ScrollBarWidth-(FMargin*2), Height - (FMargin*2));
  Canvas.SetClipRect(r);

  r.Height := RowHeight;

  for n := FFirstItem to ItemCount do
  begin
    if n = FFocusItem then
    begin
      if FFocused then
      begin
        Canvas.SetColor(clSelection);
        Canvas.SetTextColor(clSelectionText);
      end
      else
      begin
        Canvas.SetColor(clInactiveSel);
        Canvas.SetTextColor(clInactiveSelText);
      end;
    end
    else
    begin
      Canvas.SetColor(FBackgroundColor);
      Canvas.SetTextColor(clText1);
    end;  { if/else }
    Canvas.FillRectangle(r);

    // This is just a test.
    // BlueCurve theme  :)
    if (n = FFocusItem) and FFocused then
    begin
      // outer dark border
      Canvas.SetColor(TfpgColor($3b4c71));
      Canvas.SetLineStyle(1, lsSolid);
      Canvas.DrawRectangle(r);
      InflateRect(r, -1, -1);
      // left top
      Canvas.SetColor(TfpgColor($98b2ed));
      Canvas.DrawLine(r.Left, r.Bottom, r.Left, r.Top);  // left
      Canvas.DrawLine(r.Left, r.Top, r.Right, r.Top);    // top
      // right bottom
      Canvas.SetColor(TfpgColor($4468b8));
      Canvas.DrawLine(r.Right, r.Top, r.Right, r.Bottom);   // right
      Canvas.DrawLine(r.Right, r.Bottom, r.Left-1, r.Bottom);   // bottom
      // inside gradient fill
      InflateRect(r, -1, -1);
      Canvas.GradientFill(r, TfpgColor($435e9a), TfpgColor($5476c4), gdVertical);
      // reset rectangle
      InflateRect(r, 2, 2);
    end;

    DrawItem(n, r, 0);
    inc(r.Top, RowHeight);

    if r.Top >= Height then
      Break;
  end;  { for }

  // clearing after the last row
  if r.Top <= Height then
  begin
    Canvas.SetColor(FBackgroundColor);
    r.SetBottom(Height - FMargin);
    Canvas.FillRectangle(r);
  end;

  Canvas.EndDraw;
end;

constructor TfpgBaseListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := fpgGetFont('#List');
  FBackgroundColor    := clListBox;
  
  FScrollBar          := TfpgScrollBar.Create(self);
  FScrollBar.OnScroll := @ScrollBarMove;
  FScrollBar.Visible  := False;

  FFocusable      := True;
  FFocusItem      := 1;
  FFirstItem      := 1;
  FWidth          := 80;
  FHeight         := 80;
  FMargin         := 2;
  FMouseDragging  := False;
  FPopupFrame     := False;
  FHotTrack       := False;

  FOnChange := nil;
  FOnSelect := nil;
end;

destructor TfpgBaseListBox.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TfpgBaseListBox.Update;
begin
  FFirstItem := 1;
  FFocusItem := 1;
  UpdateScrollBar;
  Repaint;
end;

function TfpgBaseListBox.ItemCount: integer;
begin
  // This must be overridden in descendant classes!
  result := 17;
end;

function TfpgBaseListBox.RowHeight: integer;
begin
  result := FFont.Height+2;
end;

procedure TfpgBaseListBox.DrawItem(num: integer; rect: TfpgRect; flags: integer);
var
  s: string;
begin
  // This must be overridden in descendant classes!
  s := 'Item' + IntToStr(num);
  Canvas.DrawString(rect.left+2, rect.top+1, s);
end;

{ TfpgTextListBox }

procedure TfpgTextListBox.DrawItem(num: integer; rect: TfpgRect; flags: integer);
begin
  fpgStyle.DrawString(Canvas, rect.left+2, rect.top+1, FItems.Strings[num-1], Enabled);
end;

constructor TfpgTextListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TfpgListBoxStrings.Create(self);
  FFocusItem := 0;
end;

destructor TfpgTextListBox.Destroy;
begin
  FItems.Free;
  FInternalItems.Free;
  inherited Destroy;
end;

function TfpgTextListBox.ItemCount: integer;
begin
  result := FItems.Count;
end;

function TfpgTextListBox.Text: string;
begin
  if (FocusItem > 0) and (FocusItem <= FItems.Count) then
    result := FItems.Strings[FocusItem-1]
  else
    result := '';
end;

end.

