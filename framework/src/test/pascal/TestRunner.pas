{
  Console test runner>

  Usage:  ./fpgui_unittests_console -a --format=plain

}
program fpgui_unittests_console;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  consoletestrunner,
  testdependencies,
  fpg_main;

var
  Application: TTestRunner;

begin
  fpgApplication.Initialize; // some tests need it

  Application := TTestRunner.Create(nil);
  try
    Application.Initialize;
    Application.Title := 'fpGUI Unit Tests (Console)';
    Application.Run;
  finally
    Application.Free;
  end;
end.
