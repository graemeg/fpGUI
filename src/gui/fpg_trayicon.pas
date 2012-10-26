{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2012 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit implements a class that provides an icon for an application
      in the system tray.
}
unit fpg_trayicon;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_menu,
  fpg_interface;

type

  TfpgMessageIconType = (mitNoIcon, mitInformation, mitWarning, mitCritical);


  TfpgSystemTrayIcon = class(TfpgWidget)
  private
    FPopupMenu: TfpgPopupMenu;
    FImageName: TfpgString;
    FImage: TfpgImage;
    FOnMessageClicked: TNotifyEvent;
    procedure   SetImageName(AValue: TfpgString);
  protected
    FSysTrayHandler: TfpgSystemTrayHandler;
    procedure   SetVisible(const AValue: boolean); override;
    procedure   HandlePaint; override;
    procedure   HandleRMouseUp(x, y: integer; shiftstate: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    function    IsSystemTrayAvailable: boolean;
    function    SupportsMessages: boolean;
    procedure   Show;
    procedure   Hide;
    procedure   ShowMessage(const ATitle: TfpgString; const AMessage: TfpgString; const AMessageIcon: TfpgMessageIconType; const AMillisecondsTimeoutHint: Word = 10000);
  published
    property    BackgroundColor;
    property    Hint;
    property    ImageName: TfpgString read FImageName write SetImageName;
    property    PopupMenu: TfpgPopupMenu read FPopupMenu write FPopupMenu;
    property    ShowHint;
    property    OnClick;
    property    OnMessageClicked: TNotifyEvent read FOnMessageClicked write FOnMessageClicked;
    property    OnPaint;
  end;


implementation

{ TfpgSystemTrayIcon }

procedure TfpgSystemTrayIcon.SetImageName(AValue: TfpgString);
begin
  if FImageName = AValue then
    Exit;
  FImageName := AValue;
  Repaint;
end;

procedure TfpgSystemTrayIcon.SetVisible(const AValue: boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  if FVisible then
    Show
  else
    Hide;
end;

procedure TfpgSystemTrayIcon.HandlePaint;
var
  img: TfpgImage;
begin
  inherited HandlePaint;
  Canvas.Clear(FBackgroundColor);
  if FImageName <> '' then
  begin
    { request a reference to already loaded image }
    img := fpgImages.GetImage(FImageName);
    if Assigned(img) then
    begin
      FCanvas.DrawImage((Width-img.Width) div 2, (Height-img.Height) div 2, img);
    end;
  end;
end;

procedure TfpgSystemTrayIcon.HandleRMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleRMouseUp(x, y, shiftstate);
  if Assigned(FPopupMenu) then
  begin
    FPopupMenu.ShowAt(self, x, y, True);
  end;
end;

constructor TfpgSystemTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth := 20;
  FHeight := 20;
  FVisible := False;

  FHint := '';
  FImage := nil;
  FImageName := '';
  FPopupMenu := nil;

  FSysTrayHandler := TfpgSystemTrayHandler.Create(self);
end;

function TfpgSystemTrayIcon.IsSystemTrayAvailable: boolean;
begin
  Result := FSysTrayHandler.IsSystemTrayAvailable;
end;

function TfpgSystemTrayIcon.SupportsMessages: boolean;
begin
  { TODO : As far as I know Mac OS X doesn't support this, though they do now
    have this Notifications functionility since 10.8 }
  Result := FSysTrayHandler.SupportsMessages;
end;

procedure TfpgSystemTrayIcon.Show;
begin
  FSysTrayHandler.Show;
  FVisible := True;
end;

procedure TfpgSystemTrayIcon.Hide;
begin
  { TODO : TfpgSystemTrayIcon.Hide not implemented yet! }
//  FVisible := False;
//  FSysTrayHandler.Hide;
end;

procedure TfpgSystemTrayIcon.ShowMessage(const ATitle: TfpgString;
  const AMessage: TfpgString; const AMessageIcon: TfpgMessageIconType;
  const AMillisecondsTimeoutHint: Word);
begin
  { TODO : TfpgSystemTrayIcon.ShowMessage not implemented yet! }
end;

end.

