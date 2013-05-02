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

  TfpgWindow = class(TfpgWidget)
  private
    FWindowTitle: string;
    function GetWindowState: TfpgWindowState;
    procedure SetWindowState(AValue: TfpgWindowState);
  protected
    procedure   AdjustWindowStyle; virtual;
    procedure   SetWindowParameters; virtual;
    procedure   SetWindowTitle(const ATitle: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property    WindowTitle: string read FWindowTitle write SetWindowTitle;
    property    WindowState: TfpgWindowState read GetWindowState write SetWindowState;


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

constructor TfpgWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HasOwnWindow:=True;
end;

end.

