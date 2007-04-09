{
    fpGUI  -  Free Pascal GUI Library
    
    PopupWindow class declarations
    
    Copyright (C) 2000 - 2006 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{%mainunit fpgui.pas}

{$IFDEF read_interface}

  TFPopupWindow = class(TFCustomForm)
  public
    constructor Create(AOwner: TComponent); override;
    procedure   Show; override;
    procedure   Close; override;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}

constructor TFPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParent := nil;
  FWindowOptions := FWindowOptions + [woBorderless, woPopup];
end;


procedure TFPopupWindow.Show;
begin
  inherited Show;
  LAYOUTTRACE('TFPopupWindow.Show for %s:%s', [Name, ClassName]);
  Wnd.CaptureMouse;
end;


procedure TFPopupWindow.Close;
begin
  Wnd.ReleaseMouse;
  inherited Close;
end;

{$ENDIF read_implementation}

