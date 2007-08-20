unit gui_menu;

{$mode objfpc}{$H+}

{
  Still under construction!!!!!
}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_widget,
  gui_form,
  gfx_popupwindow,
  gfx_UTF8utils;
  
type
  TfpgHotKeyDef = string;
  
  // forward declarations
  TfpgPopupMenu = class;
  TfpgMenuBar = class;
  
  
  TfpgMenuItem = class(TComponent)
  private
    FEnabled: boolean;
    FHotKeyDef: TfpgHotKeyDef;
    FOnClick: TNotifyEvent;
    FSeparator: boolean;
    FSubMenu: TfpgPopupMenu;
    FText: string;
    FVisible: boolean;
    procedure   SetEnabled(const AValue: boolean);
    procedure   SetHotKeyDef(const AValue: TfpgHotKeyDef);
    procedure   SetSeparator(const AValue: boolean);
    procedure   SetText(const AValue: string);
    procedure   SetVisible(const AValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure   Click;
    function    Selectable: boolean;
    function    GetAccelChar: string;
    procedure   DrawText(ACanvas: TfpgCanvas; x, y: TfpgCoord);
    property    Text: string read FText write SetText;
    property    HotKeyDef: TfpgHotKeyDef read FHotKeyDef write SetHotKeyDef;
    property    Separator: boolean read FSeparator write SetSeparator;
    property    Visible: boolean read FVisible write SetVisible;
    property    Enabled: boolean read FEnabled write SetEnabled;
    property    SubMenu: TfpgPopupMenu read FSubMenu write FSubMenu;
    property    OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;
  
  
  TfpgPopupMenu = class(TfpgPopupWindow)
  private
    FBackgroundColor: TfpgColor;
    FBeforeShow: TNotifyEvent;
    FMargin: TfpgCoord;
    FTextMargin: TfpgCoord;
    procedure   SetBackgroundColor(const AValue: TfpgColor);
    procedure   DoSelect;
    procedure   CloseSubmenus;
    function    GetItemPosY(index: integer): integer;
    function    CalcMouseRow(y: integer): integer;
    function    VisibleCount: integer;
    function    VisibleItem(ind: integer): TfpgMenuItem;
    function    MenuFocused: boolean;
  protected
    FMenuFont: TfpgFont;
    FMenuAccelFont: TfpgFont;
    FMenuDisabledFont: TfpgFont;
    FSymbolWidth: integer;
    FItems: TList;
    FFocusItem: integer;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandlePaint; override;
    procedure   HandleShow; override;
    procedure   DrawItem(mi: TfpgMenuItem; rect: TfpgRect); virtual;
    procedure   DrawRow(line: integer; focus: boolean); virtual;
    function    ItemHeight(mi: TfpgMenuItem): integer; virtual;
    procedure   PrepareToShow;
  public
    OpenerPopup: TfpgPopupMenu;
    OpenerMenuBar: TfpgMenuBar;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    AddMenuItem(const menuname: string; const hotkeydef: string; HandlerProc: TNotifyEvent): TfpgMenuItem;
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor;
    property    BeforeShow: TNotifyEvent read FBeforeShow write FBeforeShow;
  end;
  
  
  TfpgMenuBar = class(TfpgWidget)
  private
    FBackgroundColor: TfpgColor;
    FBeforeShow: TNotifyEvent;
    FLightColor: TfpgColor;
    FDarkColor: TfpgColor;
    procedure   SetBackgroundColor(const AValue: TfpgColor);
  protected
    FItems: TList;  // stores visible items only
    FFocusItem: integer;
    procedure   PrepareToShow;  //
    function    VisibleCount: integer;  //
    function    VisibleItem(ind: integer): TfpgMenuItem; //
    procedure   HandleShow; override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandlePaint; override;  //
  public
    constructor Create(AOwner: TComponent); override; //
    destructor  Destroy; override;  //
    function    ItemWidth(mi: TfpgMenuItem): integer;  //
    procedure   DrawColumn(col: integer; focus: boolean); //
    function    CalcMouseCol(x: integer): integer;  //
    function    GetItemPosX(index: integer): integer; //
    procedure   DoSelect; //
    procedure   CloseSubmenus;  //
    function    MenuFocused: boolean; //
    function    SearchItemByAccel(s: string): integer;  //
    procedure   DeActivateMenu; //
    procedure   ActivateMenu; //
    function    AddMenuItem(const AMenuTitle: string; OnClickProc: TNotifyEvent): TfpgMenuItem; //
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor;
    property    BeforeShow: TNotifyEvent read FBeforeShow write FBeforeShow;
  end;


implementation

var
  uFocusedPopupMenu: TfpgPopupMenu;


{ TfpgMenuItem }

procedure TfpgMenuItem.SetText(const AValue: string);
begin
  if FText=AValue then exit;
  FText:=AValue;
end;

procedure TfpgMenuItem.SetVisible(const AValue: boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
end;

procedure TfpgMenuItem.SetHotKeyDef(const AValue: TfpgHotKeyDef);
begin
  if FHotKeyDef=AValue then exit;
  FHotKeyDef:=AValue;
end;

procedure TfpgMenuItem.SetEnabled(const AValue: boolean);
begin
  if FEnabled=AValue then exit;
  FEnabled:=AValue;
end;

procedure TfpgMenuItem.SetSeparator(const AValue: boolean);
begin
  if FSeparator=AValue then exit;
  FSeparator:=AValue;
end;

constructor TfpgMenuItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text := '';
  HotKeyDef := '';
  FSeparator := False;
  FVisible := True;
  FEnabled := True;
  FSubMenu := nil;
  FOnClick := nil;
end;

procedure TfpgMenuItem.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(self);
end;

function TfpgMenuItem.Selectable: boolean;
begin
  Result := Enabled and Visible and (not Separator);
end;

function TfpgMenuItem.GetAccelChar: string;
var
  p: integer;
begin
  p := UTF8Pos('&', Text);
  if p > 0 then
  begin
    Result := UTF8Copy(Text, p+1, 1);
  end
  else
    Result := '';
end;

procedure TfpgMenuItem.DrawText(ACanvas: TfpgCanvas; x, y: TfpgCoord);
var
  s: string;
  p: integer;
  achar: string;
begin
//  writeln('DrawText  x:', x, '  y:', y);
  if not Enabled then
    ACanvas.SetFont(fpgStyle.MenuDisabledFont)
  else
    ACanvas.SetFont(fpgStyle.MenuFont);

  achar := '&';
  s := Text;

  repeat
    p := UTF8Pos(achar, s);
    if p > 0 then
    begin
      // first part of text before the & sign
      ACanvas.DrawString(x, y, UTF8Copy(s, 1, p-1));
      inc(x, fpgStyle.MenuFont.TextWidth(UTF8Copy(s, 1, p-1)));
      if UTF8Copy(s, p+1, 1) = achar then
      begin
        // Do we need to paint a actual & sign (create via && in item text)
        ACanvas.DrawString(x, y, achar);
        inc(x, fpgStyle.MenuFont.TextWidth(achar));
      end
      else
      begin
        // Draw the HotKey text
        if Enabled then
          ACanvas.SetFont(fpgStyle.MenuAccelFont);
        ACanvas.DrawString(x, y, UTF8Copy(s, p+1, 1));
        inc(x, ACanvas.Font.TextWidth(UTF8Copy(s, p+1, 1)));
        if Enabled then
          ACanvas.SetFont(fpgStyle.MenuFont);
      end;
      s := UTF8Copy(s, p+2, UTF8Length(s));
    end;  { if }
  until p < 1;

  // Draw the remaining text after the & sign
  if UTF8Length(s) > 0 then
    ACanvas.DrawString(x, y, s);
end;

{ TfpgMenuBar }

procedure TfpgMenuBar.SetBackgroundColor(const AValue: TfpgColor);
begin
  if FBackgroundColor=AValue then exit;
  FBackgroundColor:=AValue;
end;

procedure TfpgMenuBar.PrepareToShow;
var
  n: integer;
  mi: TfpgMenuItem;
begin
//  writeln(Classname, ' PrepareToShow');
  if Assigned(FBeforeShow) then
    FBeforeShow(self);

  FItems.Count := 0;
  // Collecting visible items
  for n := 0 to ComponentCount-1 do
  begin
    if Components[n] is TfpgMenuItem then
    begin
      mi := TfpgMenuItem(Components[n]);
      if mi.Visible then
        FItems.Add(mi);
    end;
  end;
end;

function TfpgMenuBar.VisibleCount: integer;
begin
  Result := FItems.Count;
end;

function TfpgMenuBar.VisibleItem(ind: integer): TfpgMenuItem;
begin
  if (ind < 1) or (ind > FItems.Count) then
    Result := nil
  else
    Result := TfpgMenuItem(FItems.Items[ind-1]);
end;

procedure TfpgMenuBar.HandleShow;
begin
  PrepareToShow;
  inherited HandleShow;
end;

procedure TfpgMenuBar.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  newf: integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  if not MenuFocused then
    Exit; //==>

  newf := CalcMouseCol(x);
  if not VisibleItem(newf).Selectable then
    Exit; //==>

  if newf = FFocusItem then
    Exit; //==>

  DrawColumn(FFocusItem, False);
  FFocusItem := newf;
  DrawColumn(FFocusItem, True);
end;

procedure TfpgMenuBar.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  newf: integer;
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  if not Focused then
    ActivateMenu;
  //else
  //begin
    //CloseSubmenus;
    //DeActivateMenu;
    //Exit; //==>
  //end;

  newf := CalcMouseCol(x);

  if not VisibleItem(newf).Selectable then
    Exit; //==>

  if newf <> FFocusItem then
  begin
    DrawColumn(FFocusItem, False);
    FFocusItem := newf;
    DrawColumn(FFocusItem, True);
  end;

  DoSelect;
end;

procedure TfpgMenuBar.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfpgMenuBar.HandlePaint;
var
  n: integer;
  r: TfpgRect;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;
  r.SetRect(0, 0, Width, Height);
  Canvas.GradientFill(r, FLightColor, FDarkColor, gdVertical);
//  Canvas.Clear(FBackgroundColor);
  for n := 1 to VisibleCount do
    DrawColumn(n, n = FFocusItem);
  Canvas.EndDraw;
end;

constructor TfpgMenuBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TList.Create;
  FBeforeShow := nil;
  FFocusItem := 1;
  FFocusable := False;
  FBackgroundColor := clWindowBackground;
  
  FLightColor := TfpgColor($f0ece3);  // color at top of menu bar
  FDarkColor  := TfpgColor($beb8a4);  // color at bottom of menu bar
end;

destructor TfpgMenuBar.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TfpgMenuBar.ItemWidth(mi: TfpgMenuItem): integer;
begin
  Result := fpgStyle.MenuFont.TextWidth(mi.Text) + 2*6;
end;

procedure TfpgMenuBar.DrawColumn(col: integer; focus: boolean);
var
  n: integer;
  r: TfpgRect;
  mi: TfpgMenuItem;
  r2: TfpgRect;
begin
  Canvas.BeginDraw;

  r.SetRect(2, 1, 1, fpgStyle.MenuFont.Height+2);

  for n := 1 to VisibleCount do
  begin
    mi := VisibleItem(n);
    r.width := ItemWidth(mi);
    if col = n then
    begin
      if focus and Focused then
      begin
        if MenuFocused then
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
        if mi.Enabled then
        begin
          Canvas.SetColor(BackgroundColor);
          Canvas.SetTextColor(clMenuText);
        end
        else
        begin
          Canvas.SetColor(BackgroundColor);
          Canvas.SetTextColor(clMenuDisabled);
        end;
      end;  { if/else }
      Canvas.FillRectangle(r);
      mi.DrawText(Canvas, r.left+4, r.top+1);
      Canvas.EndDraw;
      Exit; //==>
    end;  { if col=n }
    inc(r.Left, ItemWidth(mi));
  end;  { for }
end;

function TfpgMenuBar.CalcMouseCol(x: integer): integer;
var
  w: integer;
  n: integer;
begin
  Result := 1;
  w := 0;
  n := 1;
  while (w <= x) and (n <= VisibleCount) do
  begin
    Result := n;
    inc(w, ItemWidth(VisibleItem(n)));
    inc(n);
  end;
end;

function TfpgMenuBar.GetItemPosX(index: integer): integer;
var
  n: integer;
begin
  Result := 0;
  if index < 1 then
    Exit; //==>
  n := 1;
  while (n <= VisibleCount) and (n < index) do
  begin
    Inc(result, ItemWidth(VisibleItem(n)));
    inc(n);
  end;
end;

procedure TfpgMenuBar.DoSelect;
var
  mi: TfpgMenuItem;
begin
  mi := VisibleItem(FFocusItem);
  CloseSubMenus;  // deactivates menubar!

  if mi.SubMenu <> nil then
  begin
    ActivateMenu;
    // showing the submenu
    mi.SubMenu.ShowAt(self, GetItemPosX(FFocusItem)+2, fpgStyle.MenuFont.Height+4);
    mi.SubMenu.OpenerPopup := nil;
    mi.SubMenu.OpenerMenuBar := self;
    mi.SubMenu.DontCloseWidget := self;

    uFocusedPopupMenu := mi.SubMenu;
    RePaint;
  end
  else
  begin
    VisibleItem(FFocusItem).Click;
    DeActivateMenu;
  end;
end;

procedure TfpgMenuBar.CloseSubmenus;
var
  n: integer;
begin
  // Close all previous popups
  for n := 1 to VisibleCount do
  with VisibleItem(n) do
  begin
    if (SubMenu <> nil) and (SubMenu.HasHandle) then
      SubMenu.Close;
  end;
end;

function TfpgMenuBar.MenuFocused: boolean;
var
  n: integer;
  mi: TfpgMenuItem;
begin
  Result := True;
  for n := 1 to VisibleCount do
  begin
    mi := VisibleItem(n);
    if (mi.SubMenu <> nil) and (mi.SubMenu.HasHandle) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TfpgMenuBar.SearchItemByAccel(s: string): integer;
var
  n: integer;
begin
  Result := -1;
  for n := 1 to VisibleCount do
  begin
    with VisibleItem(n) do
    begin
      {$Note Should UpperCase take note of UTF-8? }
      if Enabled and (UpperCase(s) = UpperCase(GetAccelChar)) then
      begin
        Result := n;
        Exit; //==>
      end;
    end;
  end;
end;

procedure TfpgMenuBar.DeActivateMenu;
begin
  Parent.ActiveWidget := nil;
end;

procedure TfpgMenuBar.ActivateMenu;
begin
  Parent.ActiveWidget := self;
end;

function TfpgMenuBar.AddMenuItem(const AMenuTitle: string; OnClickProc: TNotifyEvent): TfpgMenuItem;
begin
  Result := TfpgMenuItem.Create(self);
  Result.Text       := AMenuTitle;
  Result.HotKeyDef  := '';
  Result.OnClick    := OnClickProc;
  Result.Separator  := False;
end;

{ TfpgPopupMenu }

procedure TfpgPopupMenu.SetBackgroundColor(const AValue: TfpgColor);
begin
  if FBackgroundColor=AValue then exit;
  FBackgroundColor:=AValue;
end;

procedure TfpgPopupMenu.DoSelect;
var
  mi: TfpgMenuItem;
  op: TfpgPopupMenu;
begin
  mi := VisibleItem(FFocusItem);
  if mi.SubMenu <> nil then
  begin
    CloseSubMenus;
    // showing the submenu
    mi.SubMenu.ShowAt(self, Width, GetItemPosY(FFocusItem));
    mi.SubMenu.OpenerPopup := self;
    mi.SubMenu.OpenerMenuBar := OpenerMenuBar;
    uFocusedPopupMenu := mi.SubMenu;
    RePaint;
  end
  else
  begin
    // Close this popup
    Close;
    op := OpenerPopup;
    while op <> nil do
    begin
      if op.HasHandle then
        op.Close;
      op := op.OpenerPopup;
    end;
    VisibleItem(FFocusItem).Click;
  end;  { if/else }

  if OpenerMenuBar <> nil then
    OpenerMenuBar.DeActivateMenu;
end;

procedure TfpgPopupMenu.CloseSubmenus;
var
  n: integer;
begin
  // Close all previous popups
  for n := 1 to VisibleCount do
  with VisibleItem(n) do
  begin
    if (SubMenu <> nil) and (SubMenu.HasHandle) then
      SubMenu.Close;
  end;
end;

function TfpgPopupMenu.GetItemPosY(index: integer): integer;
var
  n: integer;
begin
  Result := 2;
  if index < 1 then
    Exit; //==>
  n := 1;
  while (n <= VisibleCount) and (n < index) do
  begin
    Inc(Result, ItemHeight(VisibleItem(n)));
    inc(n);
  end;
end;

procedure TfpgPopupMenu.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  newf: integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  if not MenuFocused then
    Exit; //==>

  newf := CalcMouseRow(y);

  if not VisibleItem(newf).Selectable then
    Exit; //==>

  if newf = FFocusItem then
    Exit; //==>

  DrawRow(FFocusItem,false);
  FFocusItem := newf;
  DrawRow(FFocusItem,true);
end;

procedure TfpgPopupMenu.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  newf: integer;
  mi: TfpgMenuItem;
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  newf := CalcMouseRow(y);

  if not VisibleItem(newf).Selectable then
    Exit; //==>

  if newf <> FFocusItem then
  begin
    DrawRow(FFocusItem, False);
    FFocusItem := newf;
    DrawRow(FFocusItem, True);
  end;

  mi := VisibleItem(FFocusItem);
  if (mi <> nil) and (not MenuFocused) and (mi.SubMenu <> nil) and mi.SubMenu.HasHandle then
    mi.SubMenu.Close
  else
    DoSelect;
end;

procedure TfpgPopupMenu.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfpgPopupMenu.HandlePaint;
var
  n: integer;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;
  Canvas.Clear(FBackgroundColor);
  Canvas.SetColor(clWidgetFrame);
  Canvas.DrawRectangle(0, 0, Width, Height);
  for n := 1 to VisibleCount do
  begin
    DrawRow(n, n = FFocusItem);
  end;
  Canvas.EndDraw;
end;

procedure TfpgPopupMenu.HandleShow;
begin
  PrepareToShow;
  inherited HandleShow;
end;

function TfpgPopupMenu.VisibleCount: integer;
begin
  Result := FItems.Count;
end;

function TfpgPopupMenu.VisibleItem(ind: integer): TfpgMenuItem;
begin
  if (ind < 1) or (ind > FItems.Count) then
    Result := nil
  else
    Result := TfpgMenuItem(FItems.Items[ind-1]);
end;

procedure TfpgPopupMenu.DrawItem(mi: TfpgMenuItem; rect: TfpgRect);
var
  s: string;
  x: integer;
begin
  if mi.Separator then
  begin
    Canvas.SetColor(clMenuText);
    Canvas.DrawLine(rect.Left, rect.Top+2, rect.Right+1, rect.Top+2);
  end
  else
  begin
    x := rect.Left + FSymbolWidth + FTextMargin;

    mi.DrawText(Canvas,x,rect.top);

    if mi.HotKeyDef <> '' then
    begin
      s := mi.HotKeyDef;
      Canvas.DrawString(rect.Right-FMenuFont.TextWidth(s)-FTextMargin, rect.Top, s);
    end;

    if mi.SubMenu <> nil then
    begin
      canvas.SetColor(canvas.TextColor);
      x := (rect.height div 2) - 3;
      canvas.FillTriangle(rect.right-x-2, rect.top+2,
                          rect.right-2, rect.top+2+x,
                          rect.right-x-2, rect.top+2+2*x);
    end;
  end;
end;

procedure TfpgPopupMenu.DrawRow(line: integer; focus: boolean);
var
  n: integer;
  r: TfpgRect;
  mi: TfpgMenuItem;
begin
  Canvas.BeginDraw;
  r.SetRect(FMargin, FMargin, FWidth-(2*FMargin), FHeight-(2*FMargin));

  for n := 1 to VisibleCount do
  begin
    mi := VisibleItem(n);

    r.height := ItemHeight(mi);

    if line = n then
    begin
      if focus and (not mi.Separator) then
      begin
        if MenuFocused then
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
        if mi.Enabled then
        begin
          Canvas.SetColor(BackgroundColor);
          Canvas.SetTextColor(clMenuText);
        end
        else
        begin
          Canvas.SetColor(BackgroundColor);
          Canvas.SetTextColor(clMenuDisabled);
        end;
      end;
      Canvas.FillRectangle(r);
      DrawItem(mi, r);
      Canvas.EndDraw;
      Exit; //==>
    end;

    inc(r.Top, ItemHeight(mi) );
  end;
end;

function TfpgPopupMenu.ItemHeight(mi: TfpgMenuItem): integer;
begin
  if mi.Separator then
    Result := 5
  else
    Result := FMenuFont.Height + 2;
end;

function TfpgPopupMenu.MenuFocused: boolean;
begin
  Result := (uFocusedPopupMenu = self);
end;

// Collecting visible items and measuring sizes
procedure TfpgPopupMenu.PrepareToShow;
var
  n: integer;
  h: integer;
  tw: integer;
  hkw: integer;
  x: integer;
  mi: TfpgMenuItem;
begin
  if Assigned(FBeforeShow) then
    FBeforeShow(self);

  // Collecting visible items
  FItems.Count := 0;

  for n := 0 to ComponentCount-1 do
  begin
    if Components[n] is TfpgMenuItem then
    begin
      mi := TfpgMenuItem(Components[n]);
      if mi.Visible then
        FItems.Add(mi);
    end;
  end;

  // Measuring sizes
  h             := 0;
  tw            := 0;
  hkw           := 0;
  FSymbolWidth  := 0;
  for n := 1 to VisibleCount do
  begin
    mi  := VisibleItem(n);
    x   := ItemHeight(mi);
    inc(h, x);
    x := FMenuFont.TextWidth(mi.Text);
    if tw < x then
      tw := x;

    if mi.SubMenu <> nil then
      x := FMenuFont.Height
    else
      x := FMenuFont.TextWidth(mi.HotKeyDef);
    if hkw < x then
      hkw := x;
  end;

  if hkw > 0 then
    hkw := hkw + 5;

  FHeight := FMargin*2 + h;
  FWidth  := (FMargin+FTextMargin)*2 + FSymbolWidth + tw + hkw;

  uFocusedPopupMenu := self;
end;

function TfpgPopupMenu.CalcMouseRow(y: integer): integer;
var
  h: integer;
  n: integer;
begin
  Result := 1;
  h := 2;
  n := 1;
  while (h <= y) and (n <= VisibleCount) do
  begin
    Result := n;
    inc(h, ItemHeight(VisibleItem(n)));
    inc(n);
  end;
end;

constructor TfpgPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMargin     := 2;
  FTextMargin := 3;
  FItems      := TList.Create;
  FBackgroundColor := clWindowBackground;
  // fonts
  FMenuFont         := fpgStyle.MenuFont;
  FMenuAccelFont    := fpgStyle.MenuAccelFont;
  FMenuDisabledFont := fpgStyle.MenuDisabledFont;
  FSymbolWidth      := FMenuFont.Height+2;

  FBeforeShow   := nil;
  FFocusItem    := 1;
  OpenerPopup   := nil;
  OpenerMenubar := nil;
end;

destructor TfpgPopupMenu.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TfpgPopupMenu.AddMenuItem(const menuname: string;
  const hotkeydef: string; HandlerProc: TNotifyEvent): TfpgMenuItem;
begin
  result := TfpgMenuItem.Create(self);
  if menuname <> '-' then
  begin
    result.Text := menuname;
    result.hotkeydef := hotkeydef;
    result.OnClick := HandlerProc;
  end
  else
  begin
    result.Separator := true;
  end;
end;

end.

