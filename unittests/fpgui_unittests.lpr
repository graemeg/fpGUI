program fpgui_unittests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpg_base, 
  fpg_main, 
  guitestrunner, 
  testdependencies;

begin
  RunRegisteredTests;
end.
