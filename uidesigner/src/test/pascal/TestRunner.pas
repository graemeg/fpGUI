program TestRunner;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry, consoletestrunner;

type
  { Sample test case - replace with your actual tests }
  TSampleTest = class(TTestCase)
  published
    procedure TestExample;
  end;

procedure TSampleTest.TestExample;
begin
  AssertEquals('Sample test', 2, 1 + 1);
end;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  try
    RegisterTest(TSampleTest);
    Application.Initialize;
    Application.Run;
  finally
    Application.Free;
  end;
end.
