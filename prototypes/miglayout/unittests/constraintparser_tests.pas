unit constraintparser_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, gui_mig_constraintparser,
  gui_mig_lc;

type

  TTestConstraintParser= class(TTestCase)
  private
    cp: TConstraintParser;
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure Test_LeftToRight_short;
    procedure Test_LeftToRight_long;
    procedure Test_TopToBottom_short;
    procedure Test_TopToBottom_long;
  end;

implementation

procedure TTestConstraintParser.Test_LeftToRight_short;
var
  lc: TLC;
begin
  lc := cp.ParseLayoutConstraint('ltr');
  CheckNotNull(lc, 'Failed on 1');
  CheckTrue(lc.LeftToRight_prop, 'Failed on 2');
  FreeAndNil(lc);
  
  lc := cp.ParseLayoutConstraint(' ltr ');
  CheckNotNull(lc, 'Failed on 3');
  CheckTrue(lc.LeftToRight_prop, 'Failed on 4');
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint('rtl');
  CheckNotNull(lc, 'Failed on 5');
  CheckFalse(lc.LeftToRight_prop, 'Failed on 6');
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint(' rtl ');
  CheckNotNull(lc, 'Failed on 7');
  CheckFalse(lc.LeftToRight_prop, 'Failed on 8');
  FreeAndNil(lc);
end;

procedure TTestConstraintParser.Test_LeftToRight_long;
var
  lc: TLC;
begin
  lc := cp.ParseLayoutConstraint('lefttoright');
  CheckNotNull(lc, 'Failed on 1');
  CheckTrue(lc.LeftToRight_prop, 'Failed on 2');
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint(' lefttoright ');
  CheckNotNull(lc, 'Failed on 3');
  CheckTrue(lc.LeftToRight_prop, 'Failed on 4');
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint('righttoleft');
  CheckNotNull(lc, 'Failed on 5');
  CheckFalse(lc.LeftToRight_prop, 'Failed on 6');
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint(' righttoleft ');
  CheckNotNull(lc, 'Failed on 7');
  CheckFalse(lc.LeftToRight_prop, 'Failed on 8');
  FreeAndNil(lc);
end;

procedure TTestConstraintParser.Test_TopToBottom_short;
var
  lc: TLC;
begin
  lc := cp.ParseLayoutConstraint('ttb');
  CheckNotNull(lc, 'Failed on 1');
  CheckTrue(lc.TopToBottom_prop, 'Failed on 2');
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint(' ttb ');
  CheckNotNull(lc, 'Failed on 3');
  CheckTrue(lc.TopToBottom_prop, 'Failed on 4');
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint('btt');
  CheckNotNull(lc, 'Failed on 5');
  CheckFalse(lc.TopToBottom_prop, 'Failed on 6');
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint(' btt ');
  CheckNotNull(lc, 'Failed on 7');
  CheckFalse(lc.TopToBottom_prop, 'Failed on 8');
  FreeAndNil(lc);
end;

procedure TTestConstraintParser.Test_TopToBottom_long;
var
  lc: TLC;
begin
  lc := cp.ParseLayoutConstraint('toptobottom');
  CheckNotNull(lc, 'Failed on 1');
  CheckTrue(lc.TopToBottom_prop, 'Failed on 2');
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint(' toptobottom ');
  CheckNotNull(lc, 'Failed on 3');
  CheckTrue(lc.TopToBottom_prop, 'Failed on 4');
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint('bottomtotop');
  CheckNotNull(lc, 'Failed on 5');
  CheckFalse(lc.TopToBottom_prop, 'Failed on 6');
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint(' bottomtotop ');
  CheckNotNull(lc, 'Failed on 7');
  CheckFalse(lc.TopToBottom_prop, 'Failed on 8');
  FreeAndNil(lc);
end;

procedure TTestConstraintParser.SetUp; 
begin
  cp := TConstraintParser.Create;
end;

procedure TTestConstraintParser.TearDown; 
begin
  cp.Free;
end;


initialization
  RegisterTest(TTestConstraintParser.Suite);

end.

