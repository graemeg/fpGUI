{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit fpgui_ide;

interface

uses
  fpGUILazIDEIntf, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('fpGUILazIDEIntf',@fpGUILazIDEIntf.Register);
end;

initialization
  RegisterPackage('fpgui_ide',@Register);
end.
