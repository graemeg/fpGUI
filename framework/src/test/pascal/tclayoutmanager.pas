unit tclayoutmanager;

{$mode objfpc}{$h+}

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
  fpg_layoutmanager;

type
  TDummyLayoutManager = class(TfpgBaseLayoutManager)
  protected
    procedure DoLayout(AContainer: TfpgWidgetBase); override;
    function DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize; override;
  end;

  TTestLayoutManager = class(TTestCase)
  published
    procedure TestAssignLayoutManager;
  end;

procedure RegisterTests;

implementation

procedure RegisterTests;
begin
  RegisterTest(TTestLayoutManager);
end;

{ TDummyLayoutManager }

procedure TDummyLayoutManager.DoLayout(AContainer: TfpgWidgetBase);
begin
  // Do nothing
end;

function TDummyLayoutManager.DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize;
begin
  Result.SetSize(100, 100);
end;

{ TTestLayoutManager }

procedure TTestLayoutManager.TestAssignLayoutManager;
var
  widget: TfpgWidget;
  lm: ILayoutManager;
begin
  widget := TfpgWidget.Create(nil);
  try
    lm := TDummyLayoutManager.Create as ILayoutManager;
    widget.LayoutManager := lm;
    Check(Assigned(widget.LayoutManager), 'LayoutManager should be assigned');
    Check(widget.LayoutManager = lm, 'Assigned LayoutManager should be the same instance');
  finally
    widget.Free;
  end;
end;

initialization
  RegisterTests;

end.
