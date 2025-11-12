unit tcfpgbase;

{$mode objfpc}{$h+}

interface

uses
  Classes,
  SysUtils,
  //TestFramework,
  fpcunit, testutils, testregistry,
  fpg_base,
  fpg_widget;

type

  TTestFPGRect = class(TTestCase)
  private
    procedure CheckEqualsRect(const expected, actual: TfpgRect; const ErrorMsg: string= '');
  published
    procedure TestSetRect;
    procedure TestClear;
    procedure TestIsUnassigned;
    procedure TestIsRectEmpty;
    procedure TestBottom;
    procedure TestRight;
    procedure TestSetBottom;
    procedure TestUnionRect_Old_vs_New;
    procedure TestUnionRect;
    procedure TestInclusiveBoundaries;
    procedure TestPixelCoverage;
  end;

  TTestFPGWidget = class(TTestCase)
  published
    procedure TestWidgetInclusiveBoundaries;
    procedure TestWidgetPixelCoverage;
    procedure TestWidgetAlignment;
  end;


procedure RegisterTests;


implementation

uses
  fpg_main
  ;


procedure RegisterTests;
begin
  // TestFramework.RegisterTest('fpg_base', TTestFPGRect.Suite);
  RegisterTest(TTestFPGRect);
  RegisterTest(TTestFPGWidget);
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

procedure TTestFPGRect.TestClear;
var
  r1: TfpgRect;
begin
  r1.SetRect(10, 20, 30, 40);
  r1.Clear;
  CheckEquals(0, r1.Left, 'Failed on 1');
  CheckEquals(0, r1.Top, 'Failed on 2');
  CheckEquals(0, r1.Width, 'Failed on 3');
  CheckEquals(0, r1.Height, 'Failed on 4');
end;

procedure TTestFPGRect.TestIsUnassigned;
var
  r1: TfpgRect;
begin
  r1.SetRect(10, 20, 30, 40);
  r1.Clear;
  CheckEquals(0, r1.Left, 'Failed on 1');
  CheckEquals(0, r1.Top, 'Failed on 2');
  CheckEquals(0, r1.Width, 'Failed on 3');
  CheckEquals(0, r1.Height, 'Failed on 4');
  CheckEquals(true, r1.IsUnassigned, 'Failed on 5');
  r1.Clear;
  r1.Left := 1;
  CheckEquals(false, r1.IsUnassigned, 'Failed on 6');
  r1.Clear;
  r1.Top := 1;
  CheckEquals(false, r1.IsUnassigned, 'Failed on 7');
  r1.Clear;
  r1.Width := 1;
  CheckEquals(false, r1.IsUnassigned, 'Failed on 8');
  r1.Clear;
  r1.Height := 1;
  CheckEquals(false, r1.IsUnassigned, 'Failed on 9');
end;

procedure TTestFPGRect.TestIsRectEmpty;
var
  r1: TfpgRect;
begin
  r1.Clear;
  CheckEquals(true, r1.IsRectEmpty, 'Failed on 1');
  r1.Clear;
  r1.Left := 1;
  CheckEquals(true, r1.IsRectEmpty, 'Failed on 2');
  r1.Clear;
  r1.Top := 1;
  CheckEquals(true, r1.IsRectEmpty, 'Failed on 3');
  r1.Clear;
  r1.Width := 1;
  // a rect with no height is not a rectangle
  CheckEquals(true, r1.IsRectEmpty, 'Failed on 4');
  r1.Clear;
  r1.Height := 1;
  // a rect with no width is not a rectangle
  CheckEquals(true, r1.IsRectEmpty, 'Failed on 5');
  r1.Clear;
  r1.Width := 1;
  r1.Height := 1;
  CheckEquals(false, r1.IsRectEmpty, 'Failed on 6');
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

procedure TTestFPGRect.TestSetBottom;
var
  r1: TfpgRect;
begin
  r1.SetRect(0, 0, 0, 0);
  r1.SetBottom(5);
  CheckEquals(5, r1.Bottom, 'Failed on 1');
  CheckEquals(6, r1.Height, 'Failed on 2');
end;

procedure TTestFPGRect.TestUnionRect_Old_vs_New;
var
  r1, r2: TfpgRect;
  lResult: TfpgRect;
  lRef: TfpgRect;
begin
  r1.SetRect(10, 10, 50, 50);
  r2.SetRect(20, 20, 50, 50);
  r1.UnionRect(lResult, r2);
  lRef.SetRect(10, 10, 60, 60);
  CheckEqualsRect(lRef, lResult, 'Failed on 1');

  r1.UnionRect(lResult, r2);
  CheckEqualsRect(lRef, lResult, 'Failed on 2');
end;

procedure TTestFPGRect.TestUnionRect;
var
  r1, r2: TfpgRect;
  lRef: TfpgRect;
begin
  r1.SetRect(10, 10, 50, 50);
  r2.SetRect(20, 20, 50, 50);
  lRef.SetRect(10, 10, 60, 60);
  r1.UnionRect(r1, r2);
  CheckEqualsRect(lRef, r1, 'Failed on 1');
end;

procedure TTestFPGRect.TestInclusiveBoundaries;
var
  r: TfpgRect;
begin
  // fpGUI uses inclusive boundaries: Right = Left + Width - 1, Bottom = Top + Height - 1
  // This means a rect at (0,0) with size (200,200) occupies pixels [0,199] x [0,199]

  r.SetRect(0, 0, 200, 200);
  CheckEquals(199, r.Right, 'Right should be Left + Width - 1');
  CheckEquals(199, r.Bottom, 'Bottom should be Top + Height - 1');

  r.SetRect(10, 20, 50, 30);
  CheckEquals(59, r.Right, 'Right = 10 + 50 - 1 = 59');
  CheckEquals(49, r.Bottom, 'Bottom = 20 + 30 - 1 = 49');

  // Edge case: 1x1 rectangle
  r.SetRect(5, 5, 1, 1);
  CheckEquals(5, r.Right, 'A 1x1 rect at (5,5) has Right=5');
  CheckEquals(5, r.Bottom, 'A 1x1 rect at (5,5) has Bottom=5');
end;

procedure TTestFPGRect.TestPixelCoverage;
var
  r: TfpgRect;
  pixelCount: Integer;
begin
  // A rect should cover exactly Width * Height pixels

  r.SetRect(10, 20, 50, 30);
  // Horizontal coverage: pixels [10..59] = 50 pixels
  pixelCount := r.Right - r.Left + 1;
  CheckEquals(50, pixelCount, 'Horizontal pixel count should equal Width');

  // Vertical coverage: pixels [20..49] = 30 pixels
  pixelCount := r.Bottom - r.Top + 1;
  CheckEquals(30, pixelCount, 'Vertical pixel count should equal Height');

  // Total coverage
  pixelCount := (r.Right - r.Left + 1) * (r.Bottom - r.Top + 1);
  CheckEquals(1500, pixelCount, 'Total pixels = Width * Height = 50 * 30 = 1500');
end;

{ TTestFPGWidget }

procedure TTestFPGWidget.TestWidgetInclusiveBoundaries;
var
  w: TfpgWidget;
begin
  fpgApplication.Initialize;

  w := TfpgWidget.Create(nil);
  try
    // Widget boundaries should follow the same inclusive rules as TfpgRect
    w.Width := 200;
    w.Height := 200;
    CheckEquals(199, w.Right, 'Widget.Right should be Left + Width - 1');
    CheckEquals(199, w.Bottom, 'Widget.Bottom should be Top + Height - 1');

    w.Left := 10;
    w.Top := 20;
    w.Width := 50;
    w.Height := 30;
    CheckEquals(59, w.Right, 'Widget.Right = 10 + 50 - 1 = 59');
    CheckEquals(49, w.Bottom, 'Widget.Bottom = 20 + 30 - 1 = 49');
  finally
    w.Free;
  end;
end;

procedure TTestFPGWidget.TestWidgetPixelCoverage;
var
  w: TfpgWidget;
  pixelCount: Integer;
begin
  fpgApplication.Initialize;

  w := TfpgWidget.Create(nil);
  try
    w.Width := 50;
    w.Height := 30;

    // Widget should occupy exactly Width * Height pixels with inclusive boundaries
    pixelCount := (w.Right - w.Left + 1) * (w.Bottom - w.Top + 1);
    CheckEquals(1500, pixelCount, 'Widget should occupy Width * Height pixels');
  finally
    w.Free;
  end;
end;

procedure TTestFPGWidget.TestWidgetAlignment;
var
  container, widget: TfpgWidget;
begin
  fpgApplication.Initialize;

  container := TfpgWidget.Create(nil);
  try
    container.Width := 200;
    container.Height := 200;

    widget := TfpgWidget.Create(container);
    try
      // Test right-bottom alignment with inclusive boundaries
      // Container: [0, 199] x [0, 199]
      // 6px insets: usable area [6, 193] x [6, 193]
      // Widget 50x20 right-aligned: should end at pixel 193
      //   widget.Right = 193
      //   widget.Left = 193 - 50 + 1 = 144

      widget.Left := 144;
      widget.Top := 174;
      widget.Width := 50;
      widget.Height := 20;

      CheckEquals(193, widget.Right, 'Widget right edge at container.Right - 6');
      CheckEquals(193, widget.Bottom, 'Widget bottom edge at container.Bottom - 6');
      CheckEquals(144, widget.Left, 'Widget.Left = (container.Right - 6) - Width + 1');
      CheckEquals(174, widget.Top, 'Widget.Top = (container.Bottom - 6) - Height + 1');
    finally
      widget.Free;
    end;
  finally
    container.Free;
  end;
end;


initialization
  RegisterTests;

end.
