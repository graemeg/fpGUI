{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2008 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines various ListBox controls. A basic text (string) listbox
      control has been implemented.
}

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
    FOnScroll: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    FPopupFrame: boolean;
    FAutoHeight: boolean;
    function    GetFontDesc: string;
    procedure   SetFocusItem(const AValue: integer);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetPopupFrame(const AValue: boolean);
    procedure   UpdateScrollbarCoords;
    procedure   SetAutoHeight(const AValue: boolean);
  protected
    FFont: TfpgFont;
    FScrollBar: TfpgScrollBar;
    FFocusItem: integer;
    FMouseDragging: boolean;
    FFirstItem: integer;
    FMargin: integer;
    procedure   UpdateScrollBar;
    procedure   FollowFocus;
    function    ListHeight: TfpgCoord;
    function    ScrollBarWidth: TfpgCoord;
    function    PageLength: integer;
    procedure   ScrollBarMove(Sender: TObject; position: integer);
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
    property    AutoHeight: boolean read FAutoHeight write SetAutoHeight default False;
    property    FocusItem: integer read FFocusItem write SetFocusItem;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    HotTrack: boolean read FHotTrack write FHotTrack;
    property    PopupFrame: boolean read FPopupFrame write SetPopupFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Update;
    function    ItemCount: integer; virtual;
    function    RowHeight: integer; virtual;
    procedure   SetFirstItem(item: integer);
    property    Font: TfpgFont read FFont;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
    property    OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property    OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property    OnKeyPress; // to allow to detect return or tab key has been pressed
  end;


  // Listbox containg strings - the normal listbox as we know it. Used by
  // component developers.
  TfpgTextListBox = class(TfpgBaseListBox)
  private
  protected
    FItems: TStringList;
    procedure   DrawItem(num: integer; rect: TfpgRect; flags: integer); override;
    procedure   HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: boolean); override;
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
    property    AutoHeight;
    property    BackgroundColor default clListBox;
    property    FocusItem;
    property    FontDesc;
    property    HotTrack;
    property    Items;
    property    PopupFrame;
    property    TabOrder;
    property    TextColor;
  end;


function CreateListBox(AOwner: TComponent; x, y, w, h: TfpgCoord): TfpgListBox;


implementation


type
  // custom stringlist that will notify listbox of item changes
  TfpgListBoxStrings = class(TStringList)
  protected
    ListBox: TfpgTextListBox;
    procedure   SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(AListBox: TfpgTextListBox);
    destructor  Destroy; override;
    function    Add(const s: String): Integer; override;
    procedure   Delete(Index: Integer); override;
    procedure   Clear; override;
  end;


function CreateListBox(AOwner: TComponent; x, y, w, h: TfpgCoord): TfpgListBox;
begin
  Result       := TfpgListBox.Create(AOwner);
  Result.Left  := x;
  Result.Top   := y;
  Result.Width := w;
  if h > 0 then
    Result.Height := h;
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

destructor TfpgListBoxStrings.Destroy;
begin
  ListBox := nil;
  inherited Destroy;
end;

function TfpgListBoxStrings.Add(const s: String): Integer;
begin
  Result := inherited Add(s);
  if Assigned(ListBox) and (ListBox.HasHandle) then
    ListBox.UpdateScrollBar;
end;

procedure TfpgListBoxStrings.Delete(Index: Integer);
begin
  inherited Delete(Index);
  if Assigned(ListBox) and (ListBox.HasHandle) then
    ListBox.UpdateScrollBar;
end;

procedure TfpgListBoxStrings.Clear;
begin
  inherited Clear;
  ListBox.FocusItem := 0;
end;


{ TfpgBaseListBox }

function TfpgBaseListBox.GetFontDesc: string;
begin
  result := FFont.FontDesc;
end;

procedure TfpgBaseListBox.SetFocusItem(const AValue: integer);
var
  old: integer;
begin
  if FFocusItem = AValue then
    Exit; //==>

  old := FFocusItem;
  // do some sanity checks
  if AValue < 0 then  // zero is a valid focusitem (no selection)
    FFocusItem := 1
  else if AValue > ItemCount then
    FFocusItem := ItemCount
  else
    FFocusItem := AValue;
    
  if FFocusItem = old then
    Exit; //==>
    
  if FFocusItem <= 1 then
    FFirstItem := 1;

  FollowFocus;
  UpdateScrollbar;
  RePaint;
  DoChange;
end;

procedure TfpgBaseListBox.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  if FAutoHeight then
    Height:= ((Height - 6) div RowHeight) * RowHeight + 6;
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

procedure TfpgBaseListBox.SetAutoHeight(const AValue: boolean);
begin
  if FAutoHeight= AValue then
    Exit; //==>
  FAutoHeight := AValue;
  Height := (PageLength * RowHeight) + (2 * FMargin);
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
  if Assigned(FOnScroll) then
    FOnScroll(self);
end;

procedure TfpgBaseListBox.DoChange;
begin
  {$IFDEF DEBUG}
  writeln(Name + '.OnChange assigned');
  {$ENDIF}
  if Assigned(OnChange) then
    FOnChange(self);
end;

procedure TfpgBaseListBox.DoSelect;
begin
  if Assigned(OnSelect) then
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
            FocusItem := FFocusItem - 1;
        end;
           
    keyDown:
        begin
          if FFocusItem < ItemCount then
            FocusItem := FFocusItem + 1;
        end;

    keyPageUp:
        begin
          FocusItem := FFocusItem - PageLength;
        end;

    keyPageDown:
        begin
          FocusItem := FFocusItem + PageLength;
        end;

    keyHome:
        begin
          FocusItem := 1;
        end;

    keyEnd:
        begin
          FocusItem := ItemCount;
        end;

    keyReturn:
        begin
          if FocusItem > 0 then
            DoSelect;
          consumed := false; // to allow the forms to detect it
        end;
  else
    consumed := false;
  end;
  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfpgBaseListBox.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  if ItemCount < 1 then
    Exit; //==>
    
  FocusItem := FFirstItem + Trunc((y - FMargin) / RowHeight);
  FMouseDragging := True;
end;

procedure TfpgBaseListBox.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
var
  r: TfpgRect;
begin
  r.SetRect(Left, Top, Width, Height);
  inherited HandleLMouseUp(x, y, shiftstate);
  if ItemCount < 1 then
    Exit; //==>
    
  FMouseDragging := False;
  DoSelect;
end;

procedure TfpgBaseListBox.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  NewFocus: Integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  if ItemCount < 1 then
    Exit; //==>

  if ((not FMouseDragging) or (btnstate and 1 = 0)) and (not HotTrack) then
    Exit; //==>

  NewFocus := FFirstItem + Trunc((y - FMargin) / RowHeight);
  if NewFocus < 1 then
    NewFocus := 1;

  FocusItem := NewFocus;
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
      Canvas.SetTextColor(FTextColor);
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
  FTextColor          := Parent.TextColor;

  FFocusable      := True;
  FFocusItem      := 0;
  FFirstItem      := 1;
  FWidth          := 80;
  FHeight         := 80;
  FMargin         := 2;
  FMouseDragging  := False;
  FPopupFrame     := False;
  FHotTrack       := False;
  FAutoHeight     := False;

  FScrollBar          := TfpgScrollBar.Create(self);
  FScrollBar.OnScroll := @ScrollBarMove;

  FOnChange := nil;
  FOnSelect := nil;
  FOnScroll := nil;
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
  if num < 1 then
    Exit;
  fpgStyle.DrawString(Canvas, rect.left+2, rect.top+1, FItems.Strings[num-1], Enabled);
end;

procedure TfpgTextListBox.HandleKeyChar(var AText: TfpgChar;
  var shiftstate: TShiftState; var consumed: boolean);
var
  i: integer;
begin
  // if user press a key then it will search the stringlist for a word
  // beginning with such as letter
  if (Ord(AText[1]) > 31) and (Ord(AText[1]) < 127) and (FFocusItem > 0) or (Length(AText) > 1 ) then
    for i := FFocusItem to FItems.Count do
    begin
      if SameText(LeftStr(FItems.Strings[i-1], Length(AText)), AText) then
      begin
        FocusItem := i;
        Consumed := True;
        break;
      end;
    end;  { for }
  inherited HandleKeyChar(AText, shiftstate, consumed);
end;

constructor TfpgTextListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TfpgListBoxStrings.Create(self);
  FFocusItem := 0;
end;

destructor TfpgTextListBox.Destroy;
begin
  TfpgListBoxStrings(FItems).Free;
  inherited Destroy;
end;

function TfpgTextListBox.ItemCount: integer;
begin
  result := FItems.Count;
end;

function TfpgTextListBox.Text: string;
begin
  if (ItemCount > 0) and (FocusItem <> 0) then
    result := FItems[FocusItem-1]
  else
    result := '';
end;

end.

