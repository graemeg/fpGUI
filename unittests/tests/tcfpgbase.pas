unit tcfpgbase;

{$mode objfpc}{$h+}

interface

uses
  Classes,
  SysUtils,
  TestFramework,
  fpg_base;

type

  TTestFPGRect = class(TTestCase)
  private
    procedure CheckEqualsRect(const expected, actual: TfpgRect; const ErrorMsg: string= '');
  published
    procedure TestSetRect;
    procedure TestBottom;
    procedure TestRight;
    procedure TestUnionRect_Old_vs_New;
  end;


procedure RegisterTests;


implementation

uses
  fpg_main
  ;


procedure RegisterTests;
begin
  TestFramework.RegisterTest('fpg_base', TTestFPGRect.Suite);
end;


{ TTestFPGRect }

procedure TTestFPGRect.CheckEqualsRect(const expected: TfpgRect;
               const actual: TfpgRect; const ErrorMsg: string = '');
begin
  CheckEquals(Expected.Left, Actual.Left, ErrorMsg + ': Left');
  CheckEquals(Expected.Top, Actual.Top, ErrorMsg + ': Top');
  CheckEquals(Expected.Width, Actual.Width, ErrorMsg + ': Width');
  CheckEquals(Expected.Height, Actual.Height, ErrorMsg + ': Height');
end;

procedure TTestFPGRect.TestSetRect;
var
  r1: TfpgRect;
begin
  r1.SetRect(10, 20, 30, 40);
  CheckEquals(10, r1.Left, 'Failed on 1');
  CheckEquals(20, r1.Top, 'Failed on 2');
  CheckEquals(30, r1.Width, 'Failed on 3');
  CheckEquals(40, r1.Height, 'Failed on 4');
end;

{ See the TfpgRect API documentation for more details, if you don't understand. }
procedure TTestFPGRect.TestBottom;
var
  r1: TfpgRect;
begin
  r1.SetRect(10, 20, 30, 40);
  CheckEquals(59, r1.Bottom, 'Failed on 1');
  r1.SetRect(0, 0, 1, 1);
  CheckEquals(0, r1.Bottom, 'Failed on 2');
  r1.SetRect(0, 0, 2, 2);
  CheckEquals(1, r1.Bottom, 'Failed on 3');
end;

{ See the TfpgRect API documentation for more details, if you don't understand. }
procedure TTestFPGRect.TestRight;
var
  r1: TfpgRect;
begin
  r1.SetRect(10, 20, 30, 40);
  CheckEquals(39, r1.Right, 'Failed on 1');
  r1.SetRect(0, 0, 1, 1);
  CheckEquals(0, r1.Right, 'Failed on 2');
  r1.SetRect(0, 0, 2, 2);
  CheckEquals(1, r1.Right, 'Failed on 3');
end;

procedure TTestFPGRect.TestUnionRect_Old_vs_New;
var
  r1, r2: TfpgRect;
  lResult: TfpgRect;
  lRef: TfpgRect;
begin
  r1.SetRect(10, 10, 50, 50);
  r2.SetRect(20, 20, 50, 50);
  UnionRect(lResult, r1, r2);
  lRef.SetRect(10, 10, 60, 60);
  CheckEqualsRect(lRef, lResult, 'Failed on 1');

  r1.UnionRect(lResult, r2);
  CheckEqualsRect(lRef, lResult, 'Failed on 2');
end;

end.
