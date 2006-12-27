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

  { TMenuItem }

  TMenuItem = class(TCustomPanel)
  private
    FHotKeyDef: string;
    FSeparator: boolean;
  protected
    function    ProcessEvent(pEvent: TEventObj): Boolean; override;
  public
    constructor Create(const pText: string; pOwner: TComponent); overload;
  published
    property    Separator: boolean read FSeparator write FSeparator;
    property    HotKeyDef: string read FHotKeyDef write FHotKeyDef;
    property    Text;
    property    Visible;
    property    Enabled;
  end;

  
  { TMenuBar }

  TMenuBar = class(TBinWidget)
  private
  protected
    procedure   Paint(Canvas: TFCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    AddMenu(const pTitle: string): TMenuItem;
    function    AddMenu(const pTitle: string; const pHotKeyDef: string; pHandlerProc: TNotifyEvent): TMenuItem;
  published
//    FLayout: TBoxLayout;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}

{ TMenuItem }

function TMenuItem.ProcessEvent(pEvent: TEventObj): Boolean;
begin
  if pEvent.InheritsFrom(TMouseMoveEventObj) then
    Writeln('MenuItem ' + Text
        + ' X=' + IntToStr(TMouseMoveEventObj(pEvent).Position.X)
        + ',Y=' + IntToStr(TMouseMoveEventObj(pEvent).Position.Y));

  Result := inherited ProcessEvent(pEvent);
end;

constructor TMenuItem.Create(const pText: string; pOwner: TComponent);
begin
  inherited Create(pText, pOwner);
  FBevelStyle := bsPlain;
end;


procedure TMenuBar.Paint(Canvas: TFCanvas);
{var
  i: integer;   }
begin
  inherited Paint(Canvas);
  Style.DrawWindowBackground(Canvas, ClientRect);
//  FLayout.Paint(Canvas);
{

  for i := 1 to VisibleCount do
  begin
    DrawColumn(Canvas, i, i = FFocusItem);
  end;

  Style.DrawSeparator(
      Canvas,
      Rect(0, ClientRect.Bottom - 4, ClientRect.Right, ClientRect.Bottom),
      Horizontal);
 }
end;

constructor TMenuBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  FCanExpandWidth := True;
{
  FLayout := TBoxLayout.Create(nil);
  FLayout.Spacing := 0;
  FLayout.BorderSpacing := 6;
  FLayout.HorzAlign := horzLeft;
  
  InsertChild(FLayout);
}
end;

destructor TMenuBar.Destroy;
begin
//  RemoveChild(FLayout);
//  FLayout.Free;
  inherited Destroy;
end;


function TMenuBar.AddMenu(const pTitle: string): TMenuItem;
begin
  Result := TMenuItem.Create(pTitle, self);
//  FLayout.InsertChild(Result);
end;


function TMenuBar.AddMenu(const pTitle: string; const pHotKeyDef: string;
  pHandlerProc: TNotifyEvent): TMenuItem;
begin
  Result := TMenuItem.Create(self);
  if pTitle <> '-' then
  begin
    Result.Text       := pTitle;
    Result.HotKeyDef  := pHotKeyDef;
    Result.OnClick    := pHandlerProc;
  end
  else
  begin
    Result.Separator  := True;
  end;
//  FLayout.InsertChild(Result);
end;


{$ENDIF read_implementation}

