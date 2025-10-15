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
  testdependencies;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  try
    Application.Initialize;
    Application.Title := 'fpGUI Unit Tests (Console)';
    Application.Run;
  finally
    Application.Free;
  end;
end.
