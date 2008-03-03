program fpcunit_miglayout;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, constraintparser_tests, gui_mig_constraintparser;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
