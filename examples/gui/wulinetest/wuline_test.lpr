program wuline_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, gfxbase, fpgfx, gui_form, gui_button, math,
  gfx_imgfmt_bmp, gfx_wuline, gui_bevel, gui_radiobutton;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    btnQuit: TfpgButton;
    pnlName1: TfpgBevel;
    rbSpokes: TfpgRadioButton;
    rbLines: TfpgRadioButton;
    rbSpiral: TfpgRadioButton;
    {@VFD_HEAD_END: MainForm}
    procedure   btnQuitClicked(Sender: TObject);
    procedure   DrawSpokes(phase: double);
    procedure   DrawSpiral;
    procedure   DrawLines;
    procedure   RadioButtonChanged(Sender: TObject);
  protected
    procedure   HandlePaint; override;
  public
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

{ TMainForm }

procedure TMainForm.RadioButtonChanged(Sender: TObject);
begin
  RePaint;
end;

procedure TMainForm.HandlePaint;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;

  if rbSpokes.Checked then
    DrawSpokes(0)
  else if rbLines.Checked then
    DrawLines
  else if rbSpiral.Checked then
    DrawSpiral;

  Canvas.EndDraw;
end;

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
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

procedure TMainForm.DrawLines;
begin
  WuLine(Canvas, Point(10, 35), Point(280, 180), clBlack);
  Canvas.SetColor(clBlack);
  Canvas.DrawLine(10, 45, 280, 190);
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(278, 186, 600, 400);
  WindowTitle := 'fpGUI Wu Anti-Aliased Line test';
  WindowPosition := wpScreenCenter;
  Sizeable := False;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(500, 368, 90, 24);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnQuitClicked;
  end;

  pnlName1 := TfpgBevel.Create(self);
  with pnlName1 do
  begin
    Name := 'pnlName1';
    SetPosition(436, 8, 156, 76);
  end;

  rbSpokes := TfpgRadioButton.Create(pnlName1);
  with rbSpokes do
  begin
    Name := 'rbSpokes';
    SetPosition(12, 8, 120, 19);
    Text := 'Spokes';
    FontDesc := '#Label1';
    GroupIndex := 1;
    Checked := True;
    OnChange := @RadioButtonChanged;
  end;

  rbLines := TfpgRadioButton.Create(pnlName1);
  with rbLines do
  begin
    Name := 'rbLines';
    SetPosition(12, 28, 120, 19);
    Text := 'Lines';
    FontDesc := '#Label1';
    GroupIndex := 1;
    OnChange := @RadioButtonChanged;
  end;

  rbSpiral := TfpgRadioButton.Create(pnlName1);
  with rbSpiral do
  begin
    Name := 'rbSpiral';
    SetPosition(12, 48, 120, 19);
    Text := 'Spiral';
    FontDesc := '#Label1';
    GroupIndex := 1;
    OnChange := @RadioButtonChanged;
  end;

  {@VFD_BODY_END: MainForm}
end;


{@VFD_NEWFORM_IMPL}



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

