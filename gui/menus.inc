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
    procedure   Paint(Canvas: TFCanvas); override;
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

procedure TMenuItem.Paint(Canvas: TFCanvas);
begin
  if (wsClicked in WidgetState) and (wsMouseInside in WidgetState) then
    FBevelStyle := bsLowered
  else
    FBevelStyle := bsPlain;

  inherited Paint(Canvas);
end;

constructor TMenuItem.Create(const pText: string; pOwner: TComponent);
begin
  inherited Create(pText, pOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  FBevelStyle := bsPlain;
end;

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

