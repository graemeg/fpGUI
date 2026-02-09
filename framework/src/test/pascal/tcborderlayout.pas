unit tcborderlayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  fpg_base, fpg_main, fpg_widget, fpg_panel, fpg_button,
  fpg_layouttypes, fpg_borderlayout;

type

  TTestBorderLayout = class(TTestCase)
  published
    procedure TestLayout;
    procedure TestGaps;
    procedure TestPreferredSize;
    procedure TestMinimumSize;
  end;

procedure RegisterTests;

implementation

uses
  fpg_layoutmanager;

procedure RegisterTests;
begin
  RegisterTest(TTestBorderLayout);
end;

{ TTestBorderLayout }

procedure TTestBorderLayout.TestLayout;
var
  container: TfpgWidget;
  lm: TfpgBorderLayoutManager;
  wNorth, wSouth, wEast, wWest, wCenter: TfpgWidget;
  c: TfpgBorderLayoutConstraint;
begin
  container := TfpgWidget.Create(nil);
  container.Name := 'container';
  container.Width := 200;
  container.Height := 200;
  lm := TfpgBorderLayoutManager.Create;
  container.LayoutManager := lm;

  wNorth := TfpgWidget.Create(container);
  wNorth.Name := 'north';
  wNorth.PreferredSize := fpgSize(200, 20);
  c := TfpgBorderLayoutConstraint.Create;
  c.Region := blrNorth;
  lm.AddLayoutComponent(wNorth, c);

  wSouth := TfpgWidget.Create(container);
  wSouth.Name := 'south';
  wSouth.PreferredSize := fpgSize(200, 20);
  c := TfpgBorderLayoutConstraint.Create;
  c.Region := blrSouth;
  lm.AddLayoutComponent(wSouth, c);

  wEast := TfpgWidget.Create(container);
  wEast.Name := 'east';
  wEast.PreferredSize := fpgSize(20, 160);
  c := TfpgBorderLayoutConstraint.Create;
  c.Region := blrEast;
  lm.AddLayoutComponent(wEast, c);

  wWest := TfpgWidget.Create(container);
  wWest.Name := 'west';
  wWest.PreferredSize := fpgSize(20, 160);
  c := TfpgBorderLayoutConstraint.Create;
  c.Region := blrWest;
  lm.AddLayoutComponent(wWest, c);

  wCenter := TfpgWidget.Create(container);
  wCenter.Name := 'center';
  c := TfpgBorderLayoutConstraint.Create;
  c.Region := blrCenter;
  lm.AddLayoutComponent(wCenter, c);

  container.Realign;

  CheckEquals(0, wNorth.Top, 'wNorth.Top');
  CheckEquals(0, wNorth.Left, 'wNorth.Left');
  CheckEquals(200, wNorth.ActualWidth, 'wNorth.ActualWidth');
  CheckEquals(20, wNorth.ActualHeight, 'wNorth.ActualHeight');

  CheckEquals(180, wSouth.Top, 'wSouth.Top');
  CheckEquals(0, wSouth.Left, 'wSouth.Left');
  CheckEquals(200, wSouth.ActualWidth, 'wSouth.ActualWidth');
  CheckEquals(20, wSouth.ActualHeight, 'wSouth.ActualHeight');

  CheckEquals(20, wWest.Top, 'wWest.Top');
  CheckEquals(0, wWest.Left, 'wWest.Left');
  CheckEquals(20, wWest.ActualWidth, 'wWest.ActualWidth');
  CheckEquals(160, wWest.ActualHeight, 'wWest.ActualHeight');

  CheckEquals(20, wEast.Top, 'wEast.Top');
  CheckEquals(180, wEast.Left, 'wEast.Left');
  CheckEquals(20, wEast.ActualWidth, 'wEast.ActualWidth');
  CheckEquals(160, wEast.ActualHeight, 'wEast.ActualHeight');

  CheckEquals(20, wCenter.Top, 'wCenter.Top');
  CheckEquals(20, wCenter.Left, 'wCenter.Left');
  CheckEquals(160, wCenter.ActualWidth, 'wCenter.ActualWidth');
  CheckEquals(160, wCenter.ActualHeight, 'wCenter.ActualHeight');

  container.Free;
end;

procedure TTestBorderLayout.TestGaps;
var
  container: TfpgPanel;
  lm: ILayoutManager;
  wNorth, wSouth, wWest: TfpgButton;
  c: TfpgBorderLayoutConstraint;
begin
  container := TfpgPanel.Create(nil);
  container.Name := 'container';
  container.Width := 200;
  container.Height := 200;
  lm := TfpgBorderLayoutManager.Create(5, 10);
  container.LayoutManager := lm;

  wNorth := TfpgButton.Create(container);
  wNorth.Name := 'north';
  wNorth.PreferredSize := fpgSize(200, 20);
  c := TfpgBorderLayoutConstraint.Create;
  c.Region := blrNorth;
  lm.AddLayoutComponent(wNorth, c);

  wSouth := TfpgButton.Create(container);
  wSouth.Name := 'south';
  wSouth.PreferredSize := fpgSize(200, 20);
  c := TfpgBorderLayoutConstraint.Create;
  c.Region := blrSouth;
  lm.AddLayoutComponent(wSouth, c);

  wWest := TfpgButton.Create(container);
  wWest.Name := 'west';
  wWest.PreferredSize := fpgSize(20, 160);
  c := TfpgBorderLayoutConstraint.Create;
  c.Region := blrWest;
  lm.AddLayoutComponent(wWest, c);

  container.Realign;

  CheckEquals(12, wNorth.Top, 'wNorth.Top with gap');
  CheckEquals(42, wWest.Top, 'wWest.Top with gap');
  CheckEquals(168, wSouth.Top, 'wSouth.Top with gap');
  CheckEquals(7, wWest.Left, 'wWest.Left with gap');

  container.Free;
end;

procedure TTestBorderLayout.TestPreferredSize;
var
  container: TfpgWidget;
  lm: ILayoutManager;
  wNorth, wCenter: TfpgWidget;
  c: TfpgBorderLayoutConstraint;
  pref: TfpgSize;
begin
  container := TfpgWidget.Create(nil);
  lm := TfpgBorderLayoutManager.Create;
  container.LayoutManager := lm;

  wNorth := TfpgWidget.Create(container);
  wNorth.PreferredSize := fpgSize(100, 20);
  c := TfpgBorderLayoutConstraint.Create;
  c.Region := blrNorth;
  lm.AddLayoutComponent(wNorth, c);

  wCenter := TfpgWidget.Create(container);
  wCenter.PreferredSize := fpgSize(50, 50);
  c := TfpgBorderLayoutConstraint.Create;
  c.Region := blrCenter;
  lm.AddLayoutComponent(wCenter, c);

  pref := lm.GetPreferredSize(container);

  CheckEquals(100, pref.W, 'PreferredWidth');
  CheckEquals(70, pref.H, 'PreferredHeight');

  container.Free;
end;

procedure TTestBorderLayout.TestMinimumSize;
var
  container: TfpgWidget;
  lm: ILayoutManager;
  wNorth, wCenter: TfpgWidget;
  c: TfpgBorderLayoutConstraint;
  min: TfpgSize;
begin
  container := TfpgWidget.Create(nil);
  lm := TfpgBorderLayoutManager.Create;
  container.LayoutManager := lm;

  wNorth := TfpgWidget.Create(container);
  wNorth.MinWidth := 80;
  wNorth.MinHeight := 15;
  c := TfpgBorderLayoutConstraint.Create;
  c.Region := blrNorth;
  lm.AddLayoutComponent(wNorth, c);

  wCenter := TfpgWidget.Create(container);
  wCenter.MinWidth := 40;
  wCenter.MinHeight := 40;
  c := TfpgBorderLayoutConstraint.Create;
  c.Region := blrCenter;
  lm.AddLayoutComponent(wCenter, c);

  min := lm.GetMinimumSize(container);

  CheckEquals(80, min.W, 'MinimumWidth');
  CheckEquals(55, min.H, 'MinimumHeight');

  container.Free;
end;

initialization
  RegisterTests;

end.
