unit gui_mig_incellgapprovider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gui_mig_boundsize, gui_mig_componentwrapper;

type
  IInCellGapProvider = interface
    function GetDefaultGap(comp: TComponentWrapper; adjacentcomponent: TComponentWrapper; adjacentside: Integer; tag: string; IsLTR: Boolean): TBoundSize;
  end;

implementation

end.

