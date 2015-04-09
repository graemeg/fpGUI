{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2013 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a basic toplevel window type that forms, popup windows, hint
      windows etc will descend from.

      Do not confuse it with TfpgNativeWindow which is a
      basic type that widgets are associated with.
}

unit fpg_window;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_main,
  fpg_base,
  fpg_widget,
  fpg_interface;

type

  { TfpgWindow }
  // Base class for all toplevel windows
  TfpgWindow = class(TfpgWidget)
  private
    FDNDEnabled: boolean;
    FWindowTitle: string;
    function GetWindowState: TfpgWindowState;
    procedure SetWindowState(AValue: TfpgWindowState);
  protected
    FWindowType: TWindowType;
    procedure   AdjustWindowStyle; virtual;
    procedure   SetWindowParameters; virtual;
    procedure   SetWindowTitle(const ATitle: string); virtual;
    procedure   DoAllocateWindowHandle; override;
    procedure   AllocateWindowHandle; override;
    procedure   DoDNDEnabled(const AValue: boolean); virtual;
    property    WindowType: TWindowType read FWindowType write FWindowType;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   ActivateWindow;
    property    WindowTitle: string read FWindowTitle write SetWindowTitle;
    property    WindowState: TfpgWindowState read GetWindowState write SetWindowState;


  end;

implementation

type

  TfpgWindowHack = class(TfpgWindowBase)
  end;

{ TfpgWindow }

function TfpgWindow.GetWindowState: TfpgWindowState;
begin
  Result := Window.WindowState;
end;

procedure TfpgWindow.SetWindowState(AValue: TfpgWindowState);
begin
  Window.WindowState := AValue;
end;

procedure TfpgWindow.AdjustWindowStyle;
begin
  // do nothing
end;

procedure TfpgWindow.SetWindowParameters;
begin
  // do nothing
end;

procedure TfpgWindow.SetWindowTitle(const ATitle: string);
begin
  FWindowTitle := ATitle;
  if WindowAllocated then
    Window.WindowTitle := ATitle;
end;

procedure TfpgWindow.DoAllocateWindowHandle;
begin
  inherited DoAllocateWindowHandle;
  Window.WindowType:=FWindowType;
end;

procedure TfpgWindow.AllocateWindowHandle;
begin
  inherited AllocateWindowHandle;
  TfpgWindowHack(Window).DoDNDEnabled(FDNDEnabled);
end;

procedure TfpgWindow.DoDNDEnabled(const AValue: boolean);
begin
  FDNDEnabled:=AValue;
  if WindowAllocated then
    TfpgWindowHack(Window).DoDNDEnabled(AValue);
end;

constructor TfpgWindow.Create(AOwner: TComponent);
begin
  WindowType:=wtWindow;
  inherited Create(AOwner);
  HasOwnWindow:=True;
end;

procedure TfpgWindow.ActivateWindow;
begin
  if WindowAllocated then
    Window.ActivateWindow;
end;

end.

