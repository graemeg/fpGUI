unit fpg_vlc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libvlc, vlc, fpg_main;

Type

  { TFpgVLCPlayer }

  TFpgVLCPlayer = Class(TVLCMediaPlayer)
  private
    FParentWindow: TfpgWindow;
    procedure SetParentWindowControl(AValue: TfpgWindow);
  Protected
    Procedure SetParentWindow; override;
    Procedure SetParentWindowSize(AWidth,AHeight : Cardinal); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  Published
    Property ParentWindow : TfpgWindow Read FParentWindow Write SetParentWindowControl;
  end;

implementation

{ TFpgVLCPlayer }

procedure TFpgVLCPlayer.SetParentWindowControl(AValue: TfpgWindow);
begin
  if FParentWindow=AValue then Exit;
  If Assigned(FParentWindow) then
    FParentWindow.RemoveFreeNotification(Self);
  FParentWindow:=AValue;
  If Assigned(FParentWindow) then
    FParentWindow.FreeNotification(Self);
end;

procedure TFpgVLCPlayer.SetParentWindow;

begin
  if Assigned(ParentWindow) then
    begin
    {$IFDEF UNIX}
    libvlc_media_player_set_xwindow(Instance, ParentWindow.WinHandle);
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    libvlc_media_player_set_hwnd(Instance, Pointer(ParentWindow.WinHandle));
    {$ENDIF}
    end
  else if HaveInstance then
    begin
    {$IFDEF UNIX}
    libvlc_media_player_set_xwindow(Instance, 0);
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    libvlc_media_player_set_hwnd(Instance, Nil);
    {$ENDIF}
    end
end;

procedure TFpgVLCPlayer.SetParentWindowSize(AWidth, AHeight: Cardinal);
begin
  If Assigned(ParentWindow) then
    begin
    ParentWindow.Width:=AWidth;
    ParentWindow.Height:=AHeight;
    end;
end;

procedure TFpgVLCPlayer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  Inherited;
  if (Operation=opRemove) and (AComponent=FParentWindow) then
    FParentWindow:=Nil;
end;

end.

