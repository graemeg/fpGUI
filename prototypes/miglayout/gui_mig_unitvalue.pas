unit gui_mig_unitvalue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
type

  TUnitValue = class(TObject)
  private
    FPixels: integer;
  public
    property    Pixels: integer read FPixels;
  end;
  
  TUnitValueArray = array[0..3] of TUnitValue;

implementation

end.

