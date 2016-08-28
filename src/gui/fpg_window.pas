{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2013 by Andrew Haines.
    Copyright (c) 2014 - 2016 by Graeme Geldenhuys.

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

  // Base class for all toplevel windows
  TfpgWindow = class(TfpgWidget)
  private
    FOpacity: Single;
    FWindowTitle: string;
    function GetWindowState: TfpgWindowState;
    procedure SetWindowState(AValue: TfpgWindowState);
  protected
    FWindowType: TWindowType;
    FOnWindowAttributesChange: TfpgWindowAttributeChanged;
    procedure   AdjustWindowStyle; virtual;
    procedure   SetWindowParameters; virtual;
    procedure   SetWindowTitle(const ATitle: string); virtual;
    procedure   SetWindowOpacity(AValue: Single);
    procedure   DoAllocateWindowHandle; override;
    property    WindowType: TWindowType read FWindowType write FWindowType;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   ActivateWindow;
    property    OnWindowAttributesChange: TfpgWindowAttributeChanged read FOnWindowAttributesChange write FOnWindowAttributesChange;
    property    WindowTitle: string read FWindowTitle write SetWindowTitle;
    property    WindowState: TfpgWindowState read GetWindowState write SetWindowState;
    property    WindowOpacity: Single read FOpacity write SetWindowOpacity;
  end;

implementation


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

procedure TfpgWindow.SetWindowOpacity(AValue: Single);
begin
  FOpacity:=AValue;
  if WindowAllocated then
    Window.WindowOpacity:=AValue;
end;

procedure TfpgWindow.DoAllocateWindowHandle;
begin
  inherited DoAllocateWindowHandle;
  Window.WindowType:=FWindowType;
  Window.WindowOpacity:=FOpacity;
end;

constructor TfpgWindow.Create(AOwner: TComponent);
begin
  WindowType:=wtWindow;
  inherited Create(AOwner);
  HasOwnWindow:=True;
  FOpacity:=1.0;
end;

procedure TfpgWindow.ActivateWindow;
begin
  if WindowAllocated then
    Window.ActivateWindow;
end;

end.

