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
    * Color Listbox: User Defined color palette support.
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
  
  
  // simple data class containing color information
  TColorItem = class(TObject)
  public
    constructor Create(const AColorName: string; const AColorValue: TfpgColor);
    ColorName: string;
    ColorValue: TfpgColor;
  end;


  TfpgColorPalette = (cpStandardColors, cpSystemColors, cpWebColors, cpUserDefined);

  
  TfpgBaseColorListBox = class(TfpgBaseListBox)
  private
    FColorBoxWidth: TfpgCoord;
    FColorBoxHeight: TfpgCoord;
    FColorPalette: TfpgColorPalette;
    FShowColorNames: Boolean;
    function    GetColor: TfpgColor;
    procedure   SetColor(const AValue: TfpgColor);
    procedure   SetColorPalette(const AValue: TfpgColorPalette);
    procedure   SetShowColorNames (const AValue: Boolean );
    procedure   SetupColorPalette;
    procedure   FreeAndClearColors;
  protected
    FItems: TList;
    procedure   DrawItem(num: integer; rect: TfpgRect; flags: integer); override;
//    procedure   HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: boolean); override;
    property    Items: TList read FItems;
    property    Color: TfpgColor read GetColor write SetColor;
    property    ColorPalette: TfpgColorPalette read FColorPalette write SetColorPalette;
    property    ShowColorNames: Boolean read FShowColorNames write SetShowColorNames default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    ItemCount: integer; override;

  end;
  
  
  TfpgColorListBox = class(TfpgBaseColorListBox)
  published
    property    AutoHeight;
    property    BackgroundColor default clListBox;
    property    Color;
    property    ColorPalette;
    property    FocusItem;
    property    FontDesc;
    property    HotTrack;
    property    Items;
    property    PopupFrame;
    property    ShowColorNames;
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

    keyReturn, keyPEnter:
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
    // Bluecurve theme  :)
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

{ TColorItem }

constructor TColorItem.Create (const AColorName: string; const AColorValue: TfpgColor);
begin
  inherited Create;
  ColorName := AColorName;
  ColorValue := AColorValue;
end;

{ TfpgBaseColorListBox }

procedure TfpgBaseColorListBox.SetColorPalette (const AValue: TfpgColorPalette );
begin
  if FColorPalette = AValue then
    Exit;
  FColorPalette := AValue;
  SetupColorPalette;
  RePaint;
end;

procedure TfpgBaseColorListBox.SetShowColorNames (const AValue: Boolean );
begin
  if FShowColorNames = AValue then
    Exit;
  FShowColorNames := AValue;
  Repaint;
end;

function TfpgBaseColorListBox.GetColor: TfpgColor;
begin
  Result := TColorItem(FItems.Items[FocusItem-1]).ColorValue;
end;

procedure TfpgBaseColorListBox.SetColor(const AValue: TfpgColor);
var
  i: integer;
begin
  if GetColor = AValue then
    Exit;
  for i := 0 to FItems.Count-1 do
  begin
    if TColorItem(FItems.Items[i]).ColorValue = AValue then
    begin
      FocusItem := i+1;
      Exit;
    end;
  end;
end;

procedure TfpgBaseColorListBox.SetupColorPalette;
begin
  FreeAndClearColors;

  case  FColorPalette of
    cpStandardColors:
        begin
          FItems.Add(TColorItem.Create('clAqua', clAqua));
          FItems.Add(TColorItem.Create('clBlack', clBlack));
          FItems.Add(TColorItem.Create('clBlue', clBlue));
          FItems.Add(TColorItem.Create('clCream', clCream));
          FItems.Add(TColorItem.Create('clDkGray', clDkGray));
          FItems.Add(TColorItem.Create('clFuchsia', clFuchsia));
          FItems.Add(TColorItem.Create('clGray', clGray));
          FItems.Add(TColorItem.Create('clGreen', clGreen));
          FItems.Add(TColorItem.Create('clLime', clLime));
          FItems.Add(TColorItem.Create('clLtGray', clLtGray));
          FItems.Add(TColorItem.Create('clMaroon', clMaroon));
          FItems.Add(TColorItem.Create('clNavy', clNavy));
          FItems.Add(TColorItem.Create('clOlive', clOlive));
          FItems.Add(TColorItem.Create('clPurple', clPurple));
          FItems.Add(TColorItem.Create('clRed', clRed));
          FItems.Add(TColorItem.Create('clSilver', clSilver));
          FItems.Add(TColorItem.Create('clTeal', clTeal));
          FItems.Add(TColorItem.Create('clWhite', clWhite));
          FItems.Add(TColorItem.Create('clYellow', clYellow));
          FItems.Add(TColorItem.Create('clMoneyGreen', clMoneyGreen));
          FItems.Add(TColorItem.Create('clSkyBlue', clSkyBlue));
          FItems.Add(TColorItem.Create('clMedGray', clMedGray));
        end;
    cpSystemColors:
        begin
          FItems.Add(TColorItem.Create('clWindowBackground', clWindowBackground));
          FItems.Add(TColorItem.Create('clBoxColor', clBoxColor));
          FItems.Add(TColorItem.Create('clButtonFace', clButtonFace));
          FItems.Add(TColorItem.Create('clShadow1', clShadow1));
          FItems.Add(TColorItem.Create('clShadow2', clShadow2));
          FItems.Add(TColorItem.Create('clHilite1', clHilite1));
          FItems.Add(TColorItem.Create('clHilite2', clHilite2));
          FItems.Add(TColorItem.Create('clText1', clText1));
          FItems.Add(TColorItem.Create('clText2', clText2));
          FItems.Add(TColorItem.Create('clText3', clText3));
          FItems.Add(TColorItem.Create('clText4', clText4));
          FItems.Add(TColorItem.Create('clSelection', clSelection));
          FItems.Add(TColorItem.Create('clSelectionText', clSelectionText));
          FItems.Add(TColorItem.Create('clInactiveSel', clInactiveSel));
          FItems.Add(TColorItem.Create('clInactiveSelText', clInactiveSelText));
          FItems.Add(TColorItem.Create('clScrollBar', clScrollBar));
          FItems.Add(TColorItem.Create('clListBox', clListBox));
          FItems.Add(TColorItem.Create('clGridLines', clGridLines));
          FItems.Add(TColorItem.Create('clGridHeader', clGridHeader));
          FItems.Add(TColorItem.Create('clWidgetFrame', clWidgetFrame));
          FItems.Add(TColorItem.Create('clInactiveWgFrame', clInactiveWgFrame));
          FItems.Add(TColorItem.Create('clTextCursor', clTextCursor));
          FItems.Add(TColorItem.Create('clChoiceListBox', clChoiceListBox));
          FItems.Add(TColorItem.Create('clUnset', clUnset));
          FItems.Add(TColorItem.Create('clMenuText', clMenuText));
          FItems.Add(TColorItem.Create('clMenuDisabled', clMenuDisabled));
        end;
    cpWebColors:
        begin
          { TODO : Need to add the web colors }
          FItems.Add(TColorItem.Create('clAliceBlue', clAliceBlue));
          FItems.Add(TColorItem.Create('clAntiqueWhite', clAntiqueWhite));
          FItems.Add(TColorItem.Create('clAqua', clAqua));
          FItems.Add(TColorItem.Create('clAquamarine', clAquamarine));
          FItems.Add(TColorItem.Create('clAzure', clAzure));
          FItems.Add(TColorItem.Create('clBeige', clBeige));
          FItems.Add(TColorItem.Create('clBisque', clBisque));
          FItems.Add(TColorItem.Create('clBlack', clBlack));
          FItems.Add(TColorItem.Create('clBlanchedAlmond', clBlanchedAlmond));
          FItems.Add(TColorItem.Create('clBlue', clBlue));
          FItems.Add(TColorItem.Create('clBlueViolet', clBlueViolet));
          FItems.Add(TColorItem.Create('clBrown', clBrown));
          FItems.Add(TColorItem.Create('clBurlyWood', clBurlyWood));
          FItems.Add(TColorItem.Create('clCadetBlue', clCadetBlue));
          FItems.Add(TColorItem.Create('clChartreuse', clChartreuse));
          FItems.Add(TColorItem.Create('clChocolate', clChocolate));
          FItems.Add(TColorItem.Create('clCoral', clCoral));
          FItems.Add(TColorItem.Create('clCornflowerBlue', clCornflowerBlue));
          FItems.Add(TColorItem.Create('clCornsilk', clCornsilk));
          FItems.Add(TColorItem.Create('clCrimson', clCrimson));
          FItems.Add(TColorItem.Create('clCyan', clCyan));
          FItems.Add(TColorItem.Create('clDarkBlue', clDarkBlue));
          FItems.Add(TColorItem.Create('clDarkCyan', clDarkCyan));
          FItems.Add(TColorItem.Create('clDarkGoldenrod', clDarkGoldenrod));
          FItems.Add(TColorItem.Create('clDarkGray', clDarkGray));
          FItems.Add(TColorItem.Create('clDarkGreen', clDarkGreen));
          FItems.Add(TColorItem.Create('clDarkKhaki', clDarkKhaki));
          FItems.Add(TColorItem.Create('clDarkMagenta', clDarkMagenta));
          FItems.Add(TColorItem.Create('clDarkOliveGreen', clDarkOliveGreen));
          FItems.Add(TColorItem.Create('clDarkOrange', clDarkOrange));
          FItems.Add(TColorItem.Create('clDarkOrchid', clDarkOrchid));
          FItems.Add(TColorItem.Create('clDarkRed', clDarkRed));
          FItems.Add(TColorItem.Create('clDarkSalmon', clDarkSalmon));
          FItems.Add(TColorItem.Create('clDarkSeaGreen', clDarkSeaGreen));
          FItems.Add(TColorItem.Create('clDarkSlateBlue', clDarkSlateBlue));
          FItems.Add(TColorItem.Create('clDarkSlateGray', clDarkSlateGray));
          FItems.Add(TColorItem.Create('clDarkTurquoise', clDarkTurquoise));
          FItems.Add(TColorItem.Create('clDarkViolet', clDarkViolet));
          FItems.Add(TColorItem.Create('clDeepPink', clDeepPink));
          FItems.Add(TColorItem.Create('clDeepSkyBlue', clDeepSkyBlue));
          FItems.Add(TColorItem.Create('clDimGray',clDimGray ));
          FItems.Add(TColorItem.Create('clDodgerBlue', clDodgerBlue));
          FItems.Add(TColorItem.Create('clFireBrick', clFireBrick));
          FItems.Add(TColorItem.Create('clFloralWhite', clFloralWhite));
          FItems.Add(TColorItem.Create('clForestGreen', clForestGreen));
          FItems.Add(TColorItem.Create('clFuchsia', clFuchsia));
          FItems.Add(TColorItem.Create('clGainsboro', clGainsboro));
          FItems.Add(TColorItem.Create('clGhostWhite', clGhostWhite));
          FItems.Add(TColorItem.Create('clGold', clGold));
          FItems.Add(TColorItem.Create('clGoldenrod', clGoldenrod));
          FItems.Add(TColorItem.Create('clGray', clGray));
          FItems.Add(TColorItem.Create('clGreen', clGreen));
          FItems.Add(TColorItem.Create('clGreenYellow', clGreenYellow));
          FItems.Add(TColorItem.Create('clHoneydew', clHoneydew));
          FItems.Add(TColorItem.Create('clHotPink', clHotPink));
          FItems.Add(TColorItem.Create('clIndianRed', clIndianRed));
          FItems.Add(TColorItem.Create('clIndigo', clIndigo));
          FItems.Add(TColorItem.Create('clIvory', clIvory));
          FItems.Add(TColorItem.Create('clKhaki', clKhaki));
          FItems.Add(TColorItem.Create('clLavender', clLavender));
          FItems.Add(TColorItem.Create('clLavenderBlush', clLavenderBlush));
          FItems.Add(TColorItem.Create('clLawnGreen', clLawnGreen));
          FItems.Add(TColorItem.Create('clLemonChiffon', clLemonChiffon));
          FItems.Add(TColorItem.Create('clLightBlue', clLightBlue));
          FItems.Add(TColorItem.Create('clLightCoral', clLightCoral));
          FItems.Add(TColorItem.Create('clLightCyan', clLightCyan));
          FItems.Add(TColorItem.Create('clLightGoldenrodYellow', clLightGoldenrodYellow));
          FItems.Add(TColorItem.Create('clLightGreen', clLightGreen));
          FItems.Add(TColorItem.Create('clLightGray', clLightGray));
          FItems.Add(TColorItem.Create('clLightPink', clLightPink));
          FItems.Add(TColorItem.Create('clLightSalmon', clLightSalmon));
          FItems.Add(TColorItem.Create('clLightSeaGreen', clLightSeaGreen));
          FItems.Add(TColorItem.Create('clLightSkyBlue', clLightSkyBlue));
          FItems.Add(TColorItem.Create('clLightSlateGray', clLightSlateGray));
          FItems.Add(TColorItem.Create('clLightSteelBlue', clLightSteelBlue));
          FItems.Add(TColorItem.Create('clLightYellow', clLightYellow));
          FItems.Add(TColorItem.Create('clLime', clLime));
          FItems.Add(TColorItem.Create('clLimeGreen', clLimeGreen));
          FItems.Add(TColorItem.Create('clLinen', clLinen));
          FItems.Add(TColorItem.Create('clMagenta', clMagenta));
          FItems.Add(TColorItem.Create('clMaroon', clMaroon));
          FItems.Add(TColorItem.Create('clMediumAquamarine', clMediumAquamarine));
          FItems.Add(TColorItem.Create('clMediumBlue', clMediumBlue));
          FItems.Add(TColorItem.Create('clMediumOrchid', clMediumOrchid));
          FItems.Add(TColorItem.Create('clMediumPurple', clMediumPurple));
          FItems.Add(TColorItem.Create('clMediumSeaGreen', clMediumSeaGreen));
          FItems.Add(TColorItem.Create('clMediumSlateBlue', clMediumSlateBlue));
          FItems.Add(TColorItem.Create('clMediumSpringGreen', clMediumSpringGreen));
          FItems.Add(TColorItem.Create('clMediumTurquoise', clMediumTurquoise));
          FItems.Add(TColorItem.Create('clMediumVioletRed', clMediumVioletRed));
          FItems.Add(TColorItem.Create('clMidnightBlue', clMidnightBlue));
          FItems.Add(TColorItem.Create('clMintCream', clMintCream));
          FItems.Add(TColorItem.Create('clMistyRose', clMistyRose));
          FItems.Add(TColorItem.Create('clMoccasin', clMoccasin));
          FItems.Add(TColorItem.Create('clNavajoWhite', clNavajoWhite));
          FItems.Add(TColorItem.Create('clNavy', clNavy));
          FItems.Add(TColorItem.Create('clOldLace', clOldLace));
          FItems.Add(TColorItem.Create('clOlive', clOlive));
          FItems.Add(TColorItem.Create('clOliveDrab', clOliveDrab));
          FItems.Add(TColorItem.Create('clOrange', clOrange));
          FItems.Add(TColorItem.Create('clOrangeRed', clOrangeRed));
          FItems.Add(TColorItem.Create('clOrchid', clOrchid));
          FItems.Add(TColorItem.Create('clPaleGoldenrod', clPaleGoldenrod));
          FItems.Add(TColorItem.Create('clPaleGreen', clPaleGreen));
          FItems.Add(TColorItem.Create('clPaleTurquoise', clPaleTurquoise));
          FItems.Add(TColorItem.Create('clPaleVioletRed', clPaleVioletRed));
          FItems.Add(TColorItem.Create('clPaleBlue',clPaleBlue ));
          FItems.Add(TColorItem.Create('clPapayaWhip', clPapayaWhip));
          FItems.Add(TColorItem.Create('clPeachPuff',clPeachPuff ));
          FItems.Add(TColorItem.Create('clPeru', clPeru));
          FItems.Add(TColorItem.Create('clPink', clPink));
          FItems.Add(TColorItem.Create('clPlum', clPlum));
          FItems.Add(TColorItem.Create('clPowderBlue', clPowderBlue));
          FItems.Add(TColorItem.Create('clPurple', clPurple));
          FItems.Add(TColorItem.Create('clRed', clRed));
          FItems.Add(TColorItem.Create('clRosyBrown', clRosyBrown));
          FItems.Add(TColorItem.Create('clRoyalBlue', clRoyalBlue));
          FItems.Add(TColorItem.Create('clSaddleBrown', clSaddleBrown));
          FItems.Add(TColorItem.Create('clSalmon', clSalmon));
          FItems.Add(TColorItem.Create('clSandyBrown', clSandyBrown));
          FItems.Add(TColorItem.Create('clSeaGreen', clSeaGreen));
          FItems.Add(TColorItem.Create('clSeashell', clSeashell));
          FItems.Add(TColorItem.Create('clSienna', clSienna));
          FItems.Add(TColorItem.Create('clSilver', clSilver));
          FItems.Add(TColorItem.Create('clSkyBlue2', clSkyBlue2));
          FItems.Add(TColorItem.Create('clSlateBlue', clSlateBlue));
          FItems.Add(TColorItem.Create('clSlateGray', clSlateGray));
          FItems.Add(TColorItem.Create('clSnow', clSnow));
          FItems.Add(TColorItem.Create('clSpringGreen', clSpringGreen));
          FItems.Add(TColorItem.Create('clSteelBlue', clSteelBlue));
          FItems.Add(TColorItem.Create('clTan', clTan));
          FItems.Add(TColorItem.Create('clTeal', clTeal));
          FItems.Add(TColorItem.Create('clThistle', clThistle));
          FItems.Add(TColorItem.Create('clTomato', clTomato));
          FItems.Add(TColorItem.Create('clTurquoise', clTurquoise));
          FItems.Add(TColorItem.Create('clViolet', clViolet));
          FItems.Add(TColorItem.Create('clWheat', clWheat));
          FItems.Add(TColorItem.Create('clWhite', clWhite));
          FItems.Add(TColorItem.Create('clWhiteSmoke', clWhiteSmoke));
          FItems.Add(TColorItem.Create('clYellow', clYellow));
          FItems.Add(TColorItem.Create('clYellowGreen', clYellowGreen));
        end;
  end;
  FocusItem := 1;
  FollowFocus;
  UpdateScrollbar;
end;

procedure TfpgBaseColorListBox.FreeAndClearColors;
var
  i: integer;
begin
  for i := 0 to FItems.Count-1 do
    TColorItem(FItems.Items[i]).Free;
  FItems.Clear;
end;

procedure TfpgBaseColorListBox.DrawItem (num: integer; rect: TfpgRect; flags: integer );
var
  itm: TColorItem;
begin
  if num < 1 then
    Exit;
  itm := TColorItem(FItems.Items[num-1]);
  // color box
  Canvas.SetColor(itm.ColorValue);
  Canvas.FillRectangle(rect.Left + 2, rect.Top + 4, FColorBoxWidth, FColorboxHeight);
  Canvas.SetColor(clBlack);
  Canvas.DrawRectangle(rect.Left + 2, rect.Top + 4, FColorBoxWidth, FColorboxHeight);
  // color text
  if FShowColorNames then
    fpgStyle.DrawString(Canvas, FColorboxWidth + 8 + rect.left, rect.top+1, itm.ColorName, Enabled);
end;

constructor TfpgBaseColorListBox.Create (AOwner: TComponent );
begin
  inherited Create (AOwner );
  FColorBoxWidth := 35;
  FColorBoxHeight := 10;
  FShowColorNames := True;

  FItems := TList.Create;
  // default Delphi colors
  FColorPalette := cpStandardColors;
  SetupColorPalette;
end;

destructor TfpgBaseColorListBox.Destroy;
begin
  FreeAndClearColors;
  FItems.Free;
  inherited Destroy;
end;

function TfpgBaseColorListBox.ItemCount: integer;
begin
  result := FItems.Count;
end;

end.

