unit ${UNITNAME};

{$mode objfpc}{$H+}

interface

uses
  TestFramework;

type
  TMyTestCase = class(TTestCase)
  published
    procedure FirstTest;
  end;


procedure RegisterTests;


implementation

//uses
//  SomeUnitToTest;


procedure RegisterTests;
begin
  TestFramework.RegisterTest(TMyTestCase.Suite);
end;

{ TMyTestCase }

procedure TMyTestCase.FirstTest;
begin
  Check(2, 1+1, 'Failed on 1');
end;


end.
