{
    fpGUI  -  Free Pascal GUI Library
    
    Menu class declarations
    
    Copyright (C) 2000 - 2006 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{%mainunit fpgui.pp}

{
  All menu and menu item implementations
}

{$IFDEF read_interface}

  TPopupMenu = class;
  TMenuBar = class;

  { TMenuItem }

  TMenuItem = class(TCustomPanel)
  private
    FHotKeyDef: string;
    FSeparator: boolean;
    FSubMenu: TPopupMenu;
    function    GetSubMenu: TPopupMenu;
    procedure   InternalShowPopupMenu;
  protected
    procedure   Paint(Canvas: TFCanvas); override;
    function    ProcessEvent(Event: TEventObj): Boolean; override;
    procedure   Click; override;
  public
    constructor Create(const pText: string; pOwner: TComponent); overload;
    destructor  Destroy; override;
    property    SubMenu: TPopupMenu read GetSubMenu;
  published
    property    Separator: boolean read FSeparator write FSeparator;
    property    HotKeyDef: string read FHotKeyDef write FHotKeyDef;
    property    Text;
    property    Visible;
    property    Enabled;
  end;
  
  
  { TPopupMenu }

  TPopupMenu = class(TPopupWindow)
  private
    FMenu: TMenuBar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    AddMenu(const pTitle: string): TMenuItem;
    function    AddMenu(const pTitle: string; const pHotKeyDef: string; pHandlerProc: TNotifyEvent): TMenuItem;
  end;


  { TMenuBar }

  TMenuBar = class(TCustomBoxLayout)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    function    AddMenu(const pTitle: string): TMenuItem;
    function    AddMenu(const pTitle: string; const pHotKeyDef: string; pHandlerProc: TNotifyEvent): TMenuItem;
  published
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}

{ TMenuItem }

function TMenuItem.GetSubMenu: TPopupMenu;
begin
  if not Assigned(FSubMenu) then
    FSubMenu := TPopupMenu.Create(self);
  Result := FSubMenu;
end;

procedure TMenuItem.InternalShowPopupMenu;
begin
  if Assigned(FSubMenu) and FSubMenu.Visible then
  begin
    FSubMenu.Close;
    Exit; //==>
  end;

  if not Assigned(FSubMenu) then
  begin
    FSubMenu := TPopupMenu.Create(Self);
  end;

  FSubMenu.SetPosition(ClientToScreen(Point(0, Height)));
  FSubMenu.Show;
  FSubMenu.Wnd.SetMinMaxClientSize(MaxSize, MaxSize);
end;

procedure TMenuItem.Paint(Canvas: TFCanvas);
begin
  if (wsClicked in WidgetState) or (wsMouseInside in WidgetState) then
    FBevelStyle := bsLowered
  else
    FBevelStyle := bsPlain;

  inherited Paint(Canvas);
end;

function TMenuItem.ProcessEvent(Event: TEventObj): Boolean;
begin
  {$IFDEF DEBUG}
  if Event.InheritsFrom(TMouseEnterEventObj) then
    writeln(Format('MouseEnter for %s:%s', [Text, Classname]))
  else if Event.InheritsFrom(TMouseLeaveEventObj) then
    writeln(Format('MouseLeave for %s:%s', [Text, Classname]));
  {$ENDIF}
  
  if Event.InheritsFrom(TMouseEnterEventObj) then
  begin
    Include(WidgetState, wsMouseInside);
    Redraw;
    result := True;
  end
  else if Event.InheritsFrom(TMouseLeaveEventObj) then
  begin
    Exclude(WidgetState, wsMouseInside);
//    if Assigned(FSubMenu) and (FSubMenu.Visible) then
//    begin
//      writeln('1111111111111');
//     FSubMenu.Close;
//    end;
    Redraw;
    result := True;
  end
  else
    result := inherited ProcessEvent(Event);
end;

procedure TMenuItem.Click;
begin
  if (wsMouseInside in WidgetState) and Assigned(FSubMenu) then
  begin
    InternalShowPopupMenu;
  end
  else
    inherited Click;
end;

constructor TMenuItem.Create(const pText: string; pOwner: TComponent);
begin
  inherited Create(pText, pOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  FBevelStyle := bsPlain;
end;

destructor TMenuItem.Destroy;
begin
  if Assigned(FSubMenu) then
    FSubMenu.Free;
  inherited Destroy;
end;

{ TPopupMenu }

constructor TPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  BorderWidth := 1;
  Color       := clBlack;
  Name        := '#MenuPopup';

  FMenu := TMenuBar.Create(self);
  FMenu.Name        := '#VBoxMenu';
  FMenu.Orientation := Vertical;
  FMenu.Spacing     := 0;
  InsertChild(FMenu);
end;

destructor TPopupMenu.Destroy;
begin
  FMenu.Free;
  inherited Destroy;
end;

function TPopupMenu.AddMenu(const pTitle: string): TMenuItem;
begin
  Result := FMenu.AddMenu(pTitle);
end;

function TPopupMenu.AddMenu(const pTitle: string; const pHotKeyDef: string;
  pHandlerProc: TNotifyEvent): TMenuItem;
begin
  Result := FMenu.AddMenu(pTitle, photKeyDef, pHandlerProc);
end;

{ TMenuBar }

constructor TMenuBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  FCanExpandHeight := False;
  Spacing := 0;
end;

function TMenuBar.AddMenu(const pTitle: string): TMenuItem;
begin
  Result := TMenuItem.Create(pTitle, self);
  InsertChild(Result);
end;


function TMenuBar.AddMenu(const pTitle: string; const pHotKeyDef: string;
  pHandlerProc: TNotifyEvent): TMenuItem;
begin
  Result := AddMenu(pTitle);
  if pTitle <> '-' then
  begin
    Result.Text       := pTitle;
    Result.HotKeyDef  := pHotKeyDef;
    Result.OnClick    := pHandlerProc;
  end
  else
    Result.Separator  := True;
end;


{$ENDIF read_implementation}

