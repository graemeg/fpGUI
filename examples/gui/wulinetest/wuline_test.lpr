program wuline_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, gfxbase, fpgfx, gui_form, gui_button, math,
  gfx_imgfmt_bmp, gfx_wuline;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    btnGo: TfpgButton;
    procedure btnGoClicked(Sender: TObject);
    procedure DrawSpokes(phase: double);
    procedure DrawSpiral;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

procedure TMainForm.btnGoClicked(Sender: TObject);
begin
  Canvas.BeginDraw;

//  DrawWuCircle(Canvas, 200, 200, 50, clBlue);
//  DrawSpiral;

  DrawSpokes(0);

//  WuLine(Canvas, Point(10, 35), Point(280, 180), clBlack);
//  Canvas.SetColor(clBlack);
//  Canvas.DrawLine(10, 45, 280, 190);

  Canvas.EndDraw;
end;

// Generates spokes. Animates them using phase.
procedure TMainForm.DrawSpokes(phase: double);
var
  x1, y1, x2, y2: integer;
  theta: double;
  img: TfpgImage;
begin
  Canvas.BeginDraw;

  theta := phase;
  while theta < (360+phase) do
  begin
		x1 := trunc(100.0*cos(theta*3.14/180.0)+355.0);
		y1 := trunc(-100.0*sin(theta*3.14/180.0)+155.0);

		x2 := trunc(20.0*cos(theta*3.14/180.0)+355.0);
		y2 := trunc(-20.0*sin(theta*3.14/180.0)+155.0);

		WuLine(Canvas, Point(x1, y1), Point(x2, y2), clBlack);
		Canvas.DrawLine(x2-240, y2, x1-240, y1);

    theta := theta + 10;
  end;
  Canvas.EndDraw;
end;

procedure TMainForm.DrawSpiral;
var
  Theta: double;
  X1, Y1, X2, Y2: integer;
begin
  X1 := 300;
  Y1 := 100;
  Theta := 0;
  Canvas.BeginDraw;
  while Theta < 15 * 3.1415926535 do
  begin
    X2 := trunc(X1 + Cos(Theta) * Theta);
    Y2 := trunc(Y1 + Sin(Theta) * Theta);
    WuLine(Canvas, Point(X1, Y1), Point(X2, Y2), clBlack);
    Theta := Theta + 0.2;
    X1 := X2;
    Y1 := Y2;
  end;
  Canvas.EndDraw;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'fpGUI Wu Anti-Aliased Line test';
  WindowPosition := wpScreenCenter;
  Width := 600;
  Height := 400;
  
  btnGo := TfpgButton.Create(self);
  with btnGo do
  begin
    SetPosition(4, 4, 90, 24);
    Text := 'GO';
    OnClick := @btnGoClicked;
  end;
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;


begin
  MainProc;
end.

