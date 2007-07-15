unit gui_listbox;

{$mode objfpc}{$H+}

{$Note Graeme: This is still work-in-progress! Not ready for use yet.}

interface

uses
  Classes, SysUtils, gfx_widget, gui_scrollbar, gfxbase, fpgfx;
  
type

  { TfpgBaseListBox }

  TfpgBaseListBox = class(TfpgWidget)
  private
    FHotTrack: boolean;
    FOnChange: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    FPopupFrame: boolean;
    function    GetFontName: string;
    procedure   SetFocusItem(const AValue: integer);
    procedure   SetFontName(const AValue: string);
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
    procedure   DoChange;
    procedure   DoSelect;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: word); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: word); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: word); override;
    procedure   HandleMouseScroll(x, y: integer; shiftstate: word; delta: smallint); override;
    procedure   HandleShow; override;
    // ToDo
    // * handle mouse move
    // * handle window scrolling
    procedure   HandleResize(dwidth, dheight: integer); override;
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Update;
    function    ItemCount: integer; virtual;
    function    RowHeight: integer; virtual;
    procedure   DrawItem(num: integer; rect: TfpgRect; flags: integer); virtual;
    property    PopupFrame: boolean read FPopupFrame write SetPopupFrame;
    property    Font: TfpgFont read FFont;
    property    HotTrack: boolean read FHotTrack write FHotTrack;
    property    FocusItem: integer read FFocusItem write SetFocusItem;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
    property    OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  published
    property    FontName: string read GetFontName write SetFontName;
  end;


implementation

type
  TfpgScrollbarFriend = class(TfpgScrollbar)
  end;

{ TfpgBaseListBox }

function TfpgBaseListBox.GetFontName: string;
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

procedure TfpgBaseListBox.SetFontName(const AValue: string);
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

  if FScrollBar.Visible then Dec(HWidth, FScrollBar.Width);

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
    if FScrollBar.WinHandle > 0 then
      TfpgScrollbarFriend(FScrollBar).RePaint;
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
  var shiftstate: word; var consumed: boolean);
begin
  consumed := true;
  case keycode of
    KEY_UP:
           begin // up
             if FFocusItem > 1 then
             begin
               dec(FFocusItem);
               FollowFocus;
               RePaint;
               DoChange;
             end;
           end;
    KEY_DOWN:
           begin // down
             if FFocusItem < ItemCount then
             begin
               inc(FFocusItem);
               FollowFocus;
               RePaint;
               DoChange;
             end;
           end;
    KEY_PGUP:
           begin // pgup
             dec(FFocusItem,PageLength);
             if FFocusItem < 1 then FFocusItem := 1;
             FollowFocus;
             RePaint;
             DoChange;
           end;
    KEY_PGDN:
           begin // pgdown
             inc(FFocusItem,PageLength);
             if FFocusItem > ItemCount then FFocusItem := ItemCount;
             FollowFocus;
             RePaint;
             DoChange;
           end;
    KEY_HOME:
           begin // home
             FFocusItem := 1;
             FollowFocus;
             RePaint;
             DoChange;
           end;
    KEY_END:
           begin // end
             FFocusItem := ItemCount;
             FollowFocus;
             RePaint;
             DoChange;
           end;
    KEY_ENTER:
           begin // enter
             DoSelect;
             consumed := false; // to allow the forms to detect it
           end;
  else
    begin
      consumed := false;
      inherited HandleKeyPress(keycode, shiftstate, consumed);
    end;
  end;
end;

procedure TfpgBaseListBox.HandleLMouseDown(x, y: integer; shiftstate: word);
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

procedure TfpgBaseListBox.HandleLMouseUp(x, y: integer; shiftstate: word);
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

procedure TfpgBaseListBox.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: word);
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

procedure TfpgBaseListBox.HandleMouseScroll(x, y: integer; shiftstate: word; delta: smallint);
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
  UpdateScrollBarCoords;
end;

procedure TfpgBaseListBox.HandleResize(dwidth, dheight: integer);
begin
  inherited HandleResize(dwidth, dheight);
end;

procedure TfpgBaseListBox.HandlePaint;
var
  n: integer;
  r: TfpgRect;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;
  
  Canvas.ClearClipRect;

  if popupframe then
  begin
    Canvas.SetColor(clWidgetFrame);
    Canvas.DrawRectangle(0, 0, Width, Height);
    r.SetRect(1, 1, Width - 2, Height - 2);
  end
  else
  begin
    Canvas.DrawControlFrame(0, 0, Width, Height);
    r.SetRect(2, 2, Width-4, Height-4);
  end;

  Canvas.SetClipRect(r);
  Canvas.SetColor(FBackgroundColor);
  Canvas.FillRectangle(r);
  Canvas.SetFont(FFont);

  r.SetRect(FMargin, FMargin, Width-ScrollBarWidth-FMargin-2, Height-(2*FMargin));
  canvas.SetClipRect(r);

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
    end;
    Canvas.FillRectangle(r);

    DrawItem(n,r,0);
    r.Top := r.Top + r.Height;

    if r.Top >= self.Height then
      break;
  end;

  // clearing after the last row
  if r.Top <= Height then
  begin
    canvas.SetColor(FBackgroundColor);
    r.SetBottom(Height - fmargin);
    Canvas.FillRectangle(r);
  end;

  Canvas.EndDraw;
end;

constructor TfpgBaseListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFont := fpgGetFont('#List');
  FBackgroundColor := clListBox;
  FScrollBar := TfpgScrollBar.Create(self);
  FScrollBar.OnScroll := @ScrollBarMove;
  FFocusable := true;
  FFocusItem := 1;
  FFirstItem := 1;
  FWidth := 80;
  FHeight := 80;
  FMargin := 2;
  FMouseDragging := false;
  FPopupFrame := false;

  FHotTrack := false;

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
  {$Note This is wrong!!}
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
  s := 'Item' + IntToStr(num);
  Canvas.DrawString(rect.left+4, rect.top+1,s);
end;

end.

