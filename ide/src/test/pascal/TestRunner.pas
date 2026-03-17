{
    fpGUI IDE — Test Runner

    Console-based FPCUnit test runner for the IDE module.

    Usage:  ./TestRunner -a --format=plain
}
program TestRunner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  consoletestrunner,
  ide.test.editor.undo,
  ide.test.editor.blockindent,
  ide.test.highlighter,
  ide.test.editortheme,
  ide.test.bracketmatch;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  try
    Application.Initialize;
    Application.Title := 'fpGUI IDE Unit Tests';
    Application.Run;
  finally
    Application.Free;
  end;
end.
