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
  ide.test.bracketmatch,
  ide.test.navigation,
  ide.test.highlighter.ini,
  ide.test.highlighter.xml,
  ide.test.highlight.renderer,
  ide.test.build.dispatch,
  ide.test.projecttree,
  ide.test.editor.tabs,
  ide.test.profiles,
  ide.test.cursorhistory,
  ide.test.filefinder,
  ide.test.symbolfinder,
  ide.test.declaration,
  ide.test.pascal.tokeniser,
  ide.test.runner,
  (* ide.test.debug.adapter, *)
  ide.test.breakpoint,
  ide.test.variables,
  ide.test.callstack,
  ide.test.watches;

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
