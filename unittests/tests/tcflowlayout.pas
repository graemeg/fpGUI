unit tcflowlayout;

{.$I fpg_defines.inc}
{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_layouttypes,
  fpg_flowlayout;

type

  TTestFlowLayout = class(TTestCase)
  published
    procedure TestDefaults;
    procedure TestSimpleFlow;
    procedure TestWrappingFlow;
  end;

procedure RegisterTests;

implementation

procedure RegisterTests;
begin
  RegisterTest(TTestFlowLayout);
end;

{ TTestFlowLayout }

procedure TTestFlowLayout.TestDefaults;
var
  lm: TfpgFlowLayoutManager;
begin
  lm := TfpgFlowLayoutManager.Create;
  try
    CheckEquals(5, lm.HGap, 'HGap default');
    CheckEquals(5, lm.VGap, 'VGap default');
  finally
    FreeAndNil(lm);
  end;
end;

procedure TTestFlowLayout.TestSimpleFlow;
var
  container: TfpgWidget;
  lm: ILayoutManager;
  w1, w2, w3: TfpgWidget;
begin
  container := TfpgWidget.Create(nil);
  container.Name := 'container';
  container.SetPosition(0, 0, 200, 200);
  lm := TfpgFlowLayoutManager.Create as ILayoutManager;
  container.LayoutManager := lm;

  w1 := TfpgWidget.Create(container);
  w1.Name := 'w1';
  w1.SetPosition(0, 0, 50, 20);

  w2 := TfpgWidget.Create(container);
  w2.Name := 'w2';
  w2.SetPosition(0, 0, 50, 20);

  w3 := TfpgWidget.Create(container);
  w3.Name := 'w3';
  w3.SetPosition(0, 0, 50, 20);

  lm.AddLayoutComponent(w1, TfpgLayoutConstraint.Create());
  lm.AddLayoutComponent(w2, TfpgLayoutConstraint.Create());
  lm.AddLayoutComponent(w3, TfpgLayoutConstraint.Create());

  container.Realign;

  CheckEquals(5, w1.Left, 'w1.Left');
  CheckEquals(5, w1.Top, 'w1.Top');

  CheckEquals(60, w2.Left, 'w2.Left');
  CheckEquals(5, w2.Top, 'w2.Top');

  CheckEquals(115, w3.Left, 'w3.Left');
  CheckEquals(5, w3.Top, 'w3.Top');

  container.Free;
end;

procedure TTestFlowLayout.TestWrappingFlow;
var
  container: TfpgWidget;
  lm: ILayoutManager;
  w1, w2, w3: TfpgWidget;
begin
  container := TfpgWidget.Create(nil);
  container.Name := 'container';
  container.SetPosition(0, 0, 120, 200); // Narrow container
  lm := TfpgFlowLayoutManager.Create(0, 0) as ILayoutManager;
  container.LayoutManager := lm;

  w1 := TfpgWidget.Create(container);
  w1.Name := 'w1';
  w1.SetPosition(0, 0, 50, 20);

  w2 := TfpgWidget.Create(container);
  w2.Name := 'w2';
  w2.SetPosition(0, 0, 50, 20);

  w3 := TfpgWidget.Create(container);
  w3.Name := 'w3';
  w3.SetPosition(0, 0, 50, 20);

  lm.AddLayoutComponent(w1, TfpgLayoutConstraint.Create());
  lm.AddLayoutComponent(w2, TfpgLayoutConstraint.Create());
  lm.AddLayoutComponent(w3, TfpgLayoutConstraint.Create());

  container.Realign;

  // w1 and w2 should be on the first row
  CheckEquals(0, w1.Left, 'w1.Left');
  CheckEquals(0, w1.Top, 'w1.Top');
  CheckEquals(50, w2.Left, 'w2.Left');
  CheckEquals(0, w2.Top, 'w2.Top');

  // w3 should wrap to the second row
  CheckEquals(0, w3.Left, 'w3.Left after wrap');
  CheckEquals(20, w3.Top, 'w3.Top after wrap');

  container.Free;
end;

initialization
  RegisterTests;

end.
