unit fpg_mig_layoutcallback;

{
  MigLayout for fpGUI - Layout Callback Interface

  Port of LayoutCallback.java from MigLayout v11.4.2

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
  fpg_mig_unitvalue,
  fpg_mig_boundsize;

type
  { A class to extend if you want to provide more control over where a component
    is placed or the size of it.

    Note: Returned arrays from this interface will never be altered. This means
    that caching of arrays in these methods is OK. }
  IfpgMigLayoutCallback = interface
    ['{8E3F5A10-7B3E-4F29-9C4F-8D2A1B3C4E5F}']

    { Returns a position similar to the "pos" in the component constraint.
      @param AComp The widget component. Should not be altered.
      @returns The [x, y, x2, y2] array. If nil is returned, nothing is done
               and this is the default. }
    function GetPosition(AComp: TfpgWidgetBase): TfpgMigUnitValueArray;

    { Returns a size similar to the "width" and "height" in the component constraint.
      @param AComp The widget component. Should not be altered.
      @returns The [width, height] array as BoundSize. If nil is returned,
               nothing is done and this is the default. }
    function GetSize(AComp: TfpgWidgetBase): TfpgMigBoundSizeArray;

    { A last minute change of the bounds. The bounds for the layout cycle have
      been set and you can correct them according to any set of rules you like.
      @param AComp The widget component to potentially correct bounds for. }
    procedure CorrectBounds(AComp: TfpgWidgetBase);
  end;

  { Abstract base class for layout callbacks - use this if you prefer
    inheritance over interface implementation }
  TfpgMigLayoutCallback = class(TInterfacedObject, IfpgMigLayoutCallback)
  public
    function GetPosition(AComp: TfpgWidgetBase): TfpgMigUnitValueArray; virtual;
    function GetSize(AComp: TfpgWidgetBase): TfpgMigBoundSizeArray; virtual;
    procedure CorrectBounds(AComp: TfpgWidgetBase); virtual;
  end;

implementation

{ TfpgMigLayoutCallback }

function TfpgMigLayoutCallback.GetPosition(AComp: TfpgWidgetBase): TfpgMigUnitValueArray;
begin
  Result := nil;  // Default: no custom positioning
end;

function TfpgMigLayoutCallback.GetSize(AComp: TfpgWidgetBase): TfpgMigBoundSizeArray;
begin
  Result := nil;  // Default: no custom sizing
end;

procedure TfpgMigLayoutCallback.CorrectBounds(AComp: TfpgWidgetBase);
begin
  // Default: no correction
end;

end.
