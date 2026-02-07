unit fpg_mig_gapprovider;

{
  MigLayout for fpGUI - In-Cell Gap Provider Interface

  Port of InCellGapProvider.java from MigLayout v11.4.2

  License (BSD):
  ==============

  Copyright (c) 2004, Mikael Grev, MiG InfoCom AB. (miglayout (at) miginfocom (dot) com)
  Pascal port Copyright (c) 2025, Graeme Geldenhuys
  All rights reserved.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base,
  fpg_mig_boundsize;

const
  { Adjacent side constants - matching SwingConstants }
  ADJACENT_TOP = 1;
  ADJACENT_LEFT = 2;
  ADJACENT_BOTTOM = 3;
  ADJACENT_RIGHT = 4;

type
  { An interface to implement if you want to decide the gaps between two types
    of components within the same cell.

    Example:
      if (adjacentComp = nil) or (adjacentSide = ADJACENT_LEFT) or (adjacentSide = ADJACENT_TOP) then
        Exit(nil);

      isHor := (adjacentSide = ADJACENT_LEFT) or (adjacentSide = ADJACENT_RIGHT);

      if (adjacentComp is TfpgLabel) and (comp is TfpgEdit) then
        if isHor then
          Result := UnrelatedX
        else
          Result := UnrelatedY;

      Result := if isHor then RelatedX else RelatedY;
  }
  IfpgMigInCellGapProvider = interface
    ['{9F4E6B20-8C4F-4A29-9D5F-7E3B2A1C0D8E}']

    { Returns the default gap between two components that are in the same cell.
      @param AComp The component that the gap is for. Never nil.
      @param AAdjacentComp The adjacent component if any. May be nil.
      @param AAdjacentSide What side the AAdjacentComp is on. Use ADJACENT_TOP,
             ADJACENT_LEFT, ADJACENT_BOTTOM, or ADJACENT_RIGHT constants.
      @param ATag The tag string that the component might be tagged with in the
             component constraints. May be empty.
      @param AIsLTR If it is left-to-right.
      @returns The default gap between two components or nil if there should be no gap. }
    function GetDefaultGap(AComp, AAdjacentComp: TfpgWidgetBase; AAdjacentSide: Integer;
                          const ATag: string; AIsLTR: Boolean): TfpgMigBoundSize;
  end;

  { Abstract base class for in-cell gap providers - use this if you prefer
    inheritance over interface implementation }
  TfpgMigInCellGapProvider = class(TInterfacedObject, IfpgMigInCellGapProvider)
  public
    function GetDefaultGap(AComp, AAdjacentComp: TfpgWidgetBase; AAdjacentSide: Integer;
                          const ATag: string; AIsLTR: Boolean): TfpgMigBoundSize; virtual; abstract;
  end;

implementation

end.
