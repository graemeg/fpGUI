{
  Console test runner for DocView.

  Usage:  ./TestRunner --all --format=plain
}
program TestRunner;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  consoletestrunner,
  tcsearchtable;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  try
    Application.Initialize;
    Application.Title := 'DocView Unit Tests (Console)';
    Application.Run;
  finally
    Application.Free;
  end;
end.
