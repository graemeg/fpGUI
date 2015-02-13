unit gui_mig_unitconverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gui_mig_componentwrapper;

type
  TUnitConverter = class
  public
    const
      UNABLE = -87654312;
    function ConvertToPixels(AValue: Single; AUnit: String; AIsHorizontal: Boolean;
       ARefValue: Single; AParent: IContainerWrapper; AComponent: IComponentWrapper): Integer; virtual; abstract;
  end;

implementation

end.

