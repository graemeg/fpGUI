unit gui_mig_platformdefaults;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gui_mig_unitvalue, gui_mig_incellgapprovider;

type
  TPlatformDefaults = class
  private
    const
      DEF_H_UNIT = TUnitValue.LPX;
      DEF_V_UNIT = TUnitValue.LPY;
    class var
      GAP_PROVIDER: IInCellGapProvider;
      MOD_COUNT: Integer;

  end;

implementation

//const
  //cDEF_H_UNIT = gui_mi


end.

