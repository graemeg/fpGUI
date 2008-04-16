{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit gg_mvp; 

interface

uses
  basic_intf, basic_impl, view_impl, fpgui_intf, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('gg_mvp', @Register); 
end.
