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
  gui_popupwindow;
  
type
  THotKeyDef = string;
  
  TPopupMenu = class;
  
  TMenuItem = class(TComponent)
  private
    FEnabled: boolean;
    FHotKeyDef: THotKeyDef;
    FOnClick: TNotifyEvent;
    FSeparator: boolean;
    FSubMenu: TPopupMenu;
    FText: string;
    FVisible: boolean;
    procedure   SetEnabled(const AValue: boolean);
    procedure   SetHotKeyDef(const AValue: THotKeyDef);
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
    property    HotKeyDef: THotKeyDef read FHotKeyDef write SetHotKeyDef;
    property    Separator: boolean read FSeparator write SetSeparator;
    property    Visible: boolean read FVisible write SetVisible;
    property    Enabled: boolean read FEnabled write SetEnabled;
    property    SubMenu: TPopupMenu read FSubMenu;
    property    OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;
  
  
  TPopupMenu = class(TfpgForm)   // this should actually descend from a popup window class
  end;
  
  
  TMenuBar = class(TfpgWidget)
  end;

implementation

{ TMenuItem }

procedure TMenuItem.SetText(const AValue: string);
begin
  if FText=AValue then exit;
  FText:=AValue;
end;

procedure TMenuItem.SetVisible(const AValue: boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
end;

procedure TMenuItem.SetHotKeyDef(const AValue: THotKeyDef);
begin
  if FHotKeyDef=AValue then exit;
  FHotKeyDef:=AValue;
end;

procedure TMenuItem.SetEnabled(const AValue: boolean);
begin
  if FEnabled=AValue then exit;
  FEnabled:=AValue;
end;

procedure TMenuItem.SetSeparator(const AValue: boolean);
begin
  if FSeparator=AValue then exit;
  FSeparator:=AValue;
end;

constructor TMenuItem.Create(AOwner: TComponent);
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

procedure TMenuItem.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(self);
end;

function TMenuItem.Selectable: boolean;
begin
  Result := Enabled and Visible and (not Separator);
end;

function TMenuItem.GetAccelChar: string;
begin

end;

procedure TMenuItem.DrawText(ACanvas: TfpgCanvas; x, y: TfpgCoord);
begin

end;

end.

