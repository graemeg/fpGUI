{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit fpGUIHelpIntegration; 

interface

uses
  pkghelpfpGUI, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('pkghelpfpGUI', @pkghelpfpGUI.Register); 
end; 

initialization
  RegisterPackage('fpGUIHelpIntegration', @Register); 
end.
