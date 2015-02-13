unit gui_mig_componentwrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  TYPE_UNSET        = -1;
  TYPE_UNKNOWN      = 0;
  TYPE_CONTAINER    = 1;
  TYPE_LABEL        = 2;
  TYPE_TEXT_FIELD   = 3;
  TYPE_TEXT_AREA    = 4;
  TYPE_BUTTON       = 5;
  TYPE_LIST         = 6;
  TYPE_TABLE        = 7;
  TYPE_SCROLL_PANE  = 8;
  TYPE_IMAGE        = 9;
  TYPE_PANEL        = 10;
  TYPE_COMBO_BOX    = 11;
  TYPE_SLIDER       = 12;
  TYPE_SPINNER      = 13;
  TYPE_PROGRESS_BAR = 14;
  TYPE_TREE         = 15;
  TYPE_CHECK_BOX    = 16;
  TYPE_SCROLL_BAR   = 17;
  TYPE_SEPERATOR    = 18;

type


  IContainerWrapper = interface; // forward

  IComponentWrapper = interface
    function  GetComponent: TObject;
    function  GetX: Integer;
    function  GetY: Integer;
    function  GetWidth: Integer;
    function  GetHeight: Integer;
    function  GetScreenLocationX: Integer;
    function  GetScreenLocationY: Integer;
    function  GetMinimumWidth: Integer;
    function  GetMinimumHeight: Integer;
    function  GetPreferredWidth: Integer;
    function  GetPreferredHeight: Integer;
    function  GetMaximumWidth: Integer;
    function  GetMaximumHeight: Integer;
    procedure SetBounds(x,y,width,height: Integer);
    function  IsVisible: Boolean;
    function  GetBaseline(width, height: Integer): Integer;
    function  HasBaseline: Boolean;
    function  GetParent: IContainerWrapper;
    function  GetPixelUnitFactor(IsHorizontal: Boolean): Single;
    function  GetHorizontalScreenDPI: Integer;
    function  GetVerticalScreenDPI: Integer;
    function  GetScreenWidth: Integer;
    function  GetScreenHeight: Integer;
    function  GetLinkID: String;
    function  GetLayoutHashCode: Integer;
    function  GetVisualPadding: Integer;
    procedure PaintDebugOutline;
    function  GetComponentType(DisregardScrollPane: Boolean): Integer;
  end;

  IContainerWrapper = interface(IComponentWrapper)
    function  GetComponents: TList; // list of IComponentWrapper
    function  GetComponentCount: Integer;
    function  GetLayout: TObject;
    function  IsLeftToRight: Boolean;
    procedure PaintDebugCell(x,y,width,height: Integer);
  end;



implementation

end.

