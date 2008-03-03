unit constraintparser_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, gui_mig_constraintparser,
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
  AssertNotNull('Failed on 1', lc);
  AssertTrue('Failed on 2', lc.LeftToRight);
  FreeAndNil(lc);
  
  lc := cp.ParseLayoutConstraint(' ltr ');
  AssertNotNull('Failed on 3', lc);
  AssertTrue('Failed on 4', lc.LeftToRight);
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint('rtl');
  AssertNotNull('Failed on 5', lc);
  AssertFalse('Failed on 6', lc.LeftToRight);
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint(' rtl ');
  AssertNotNull('Failed on 7', lc);
  AssertFalse('Failed on 8', lc.LeftToRight);
  FreeAndNil(lc);
end;

procedure TTestConstraintParser.Test_LeftToRight_long;
var
  lc: TLC;
begin
  lc := cp.ParseLayoutConstraint('lefttoright');
  AssertNotNull('Failed on 1', lc);
  AssertTrue('Failed on 2', lc.LeftToRight);
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint(' lefttoright ');
  AssertNotNull('Failed on 3', lc);
  AssertTrue('Failed on 4', lc.LeftToRight);
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint('righttoleft');
  AssertNotNull('Failed on 5', lc);
  AssertFalse('Failed on 6', lc.LeftToRight);
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint(' righttoleft ');
  AssertNotNull('Failed on 7', lc);
  AssertFalse('Failed on 8', lc.LeftToRight);
  FreeAndNil(lc);
end;

procedure TTestConstraintParser.Test_TopToBottom_short;
var
  lc: TLC;
begin
  lc := cp.ParseLayoutConstraint('ttb');
  AssertNotNull('Failed on 1', lc);
  AssertTrue('Failed on 2', lc.TopToBottom);
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint(' ttb ');
  AssertNotNull('Failed on 3', lc);
  AssertTrue('Failed on 4', lc.TopToBottom);
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint('btt');
  AssertNotNull('Failed on 5', lc);
  AssertFalse('Failed on 6', lc.TopToBottom);
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint(' btt ');
  AssertNotNull('Failed on 7', lc);
  AssertFalse('Failed on 8', lc.TopToBottom);
  FreeAndNil(lc);
end;

procedure TTestConstraintParser.Test_TopToBottom_long;
var
  lc: TLC;
begin
  lc := cp.ParseLayoutConstraint('toptobottom');
  AssertNotNull('Failed on 1', lc);
  AssertTrue('Failed on 2', lc.TopToBottom);
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint(' toptobottom ');
  AssertNotNull('Failed on 3', lc);
  AssertTrue('Failed on 4', lc.TopToBottom);
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint('bottomtotop');
  AssertNotNull('Failed on 5', lc);
  AssertFalse('Failed on 6', lc.TopToBottom);
  FreeAndNil(lc);

  lc := cp.ParseLayoutConstraint(' bottomtotop ');
  AssertNotNull('Failed on 7', lc);
  AssertFalse('Failed on 8', lc.TopToBottom);
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

  RegisterTest(TTestConstraintParser); 
end.

