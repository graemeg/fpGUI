{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2025 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Interface contract for layout managers
}
unit fpg_layouttypes;

{$I fpg_defines.inc}

interface

uses
  Classes, SysUtils, fpg_base;

type
  TfpgLayoutConstraint = class(TInterfacedObject)
  end;

  ILayoutManager = interface
    ['{D462F3B7-51ED-411D-AA8E-9F08A1DE2072}']
    // Core layout methods
    procedure LayoutContainer(AContainer: TfpgWidgetBase);
    function GetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize;
    function GetMinimumSize(AContainer: TfpgWidgetBase): TfpgSize;

    // Constraint management
    procedure AddLayoutComponent(AWidget: TfpgWidgetBase; AConstraint: TfpgLayoutConstraint);
    procedure RemoveLayoutComponent(AWidget: TfpgWidgetBase);

    // Invalidation
    procedure InvalidateLayout(AContainer: TfpgWidgetBase);

    // debugging
    procedure PaintDebug(AWidget: TfpgWidgetBase; ACanvas: TfpgCanvasBase);
  end;

  ILayoutIterator = interface
    ['{B4D41C22-E8B9-4B4A-80E8-53436157051E}']
    function HasNext: Boolean;
    function Next: TfpgWidgetBase;
  end;

implementation

end.
