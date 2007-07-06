unit gfxbaseinterfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes;
  
type
  // forward declarations
  IGFXWindow = interface;
  
  
  IGFXWindow = interface(IInterface)
    ['{45964C7E-A7E9-4915-9EA1-3F26C5C55A2F}']
    procedure DoAllocateWindowHandle(AParent: IGFXWindow);
    procedure DoSetWindowTitle(const ATitle: string);
  end;
  

implementation

end.

