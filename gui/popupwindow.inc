{
    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther
    Copyright (C) 2006 by Graeme Geldenhuys
      member of the fpGUI development team.

    PopupWindow class declaration

    See the file COPYING.fpGUI, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{%mainunit fpgui.pp}

{$IFDEF read_interface}

  TPopupWindow = class(TCustomForm)
  public
    constructor Create(AOwner: TComponent); override;
    procedure   Show; override;
    procedure   Close; override;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}

constructor TPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParent := nil;
  FWindowOptions := [woBorderless, woPopup];
end;


procedure TPopupWindow.Show;
begin
  inherited Show;
  LAYOUTTRACE('TPopupWindow.Show for %s:%s', [Name, ClassName]);
  Wnd.CaptureMouse;
end;


procedure TPopupWindow.Close;
begin
  Wnd.ReleaseMouse;
  inherited Close;
end;

{$ENDIF read_implementation}

