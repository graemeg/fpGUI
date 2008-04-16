unit fpgui_intf;

{$mode objfpc}{$H+}

interface

uses
  gui_menu, basic_intf;
  
type

  IPopupCommandMenu = interface(IInterface)
  ['{812C1940-A8BD-4BB4-AE8D-37A912D44A6D}']
    function GetMenu: TfpgPopupMenu;
    procedure SetMenu(const AValue: TfpgPopupMenu);
    property Menu: TfpgPopupMenu read GetMenu write SetMenu;
  end;

implementation

end.

