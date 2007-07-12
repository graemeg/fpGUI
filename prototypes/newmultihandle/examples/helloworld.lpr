program HelloWorld;

{$mode objfpc}{$h+}

uses
  Classes,
  fpgui,
  fpGFX,
  gfxBase;

type
  TMainForm = class(TFWidget)
  private
    btnHello: TFButton;
    procedure FormPaint(Sender: TObject);
  public
    constructor Create(AParent: TFCustomWindow); override;
    procedure AfterConstruction; override;
  end;


{ TMainForm }

procedure TMainForm.FormPaint(Sender: TObject);
begin
  Canvas.SetColor(colBlack);
  Canvas.DrawLine(0,  0, 100, 100);
  Canvas.DrawLine(5,  0,  95, 100);
  Canvas.DrawLine(10, 0,  90, 100);
  
  Canvas.TextOut(Point(5, 150), 'This should be at (5, 150).');

  Canvas.SetColor(colBlue);
  Canvas.FillRect(Rect(5, 100, 55, 150));
  Canvas.SetColor(colGreen);
  Canvas.FillRect(Rect(60, 100, 110, 150));
  Canvas.SetColor(colRed);
  Canvas.FillRect(Rect(115, 100, 165, 150));

  Canvas.SetColor(colRed);
  Canvas.DrawRect(Rect(100, 5, 150, 55));
  Canvas.SetColor(colGreen);
  Canvas.DrawRect(Rect(105, 10, 155, 60));
  Canvas.SetColor(colBlue);
  Canvas.DrawRect(Rect(110, 15, 160, 65));
  
  Canvas.SetColor(colRed);
  Canvas.DrawCircle(Rect(10, 175, 50, 215));
  Canvas.SetColor(colGreen);
  Canvas.DrawCircle(Rect(65, 175, 105, 215));
  Canvas.SetColor(colBlue);
  Canvas.DrawCircle(Rect(120, 175, 160, 215));
end;

constructor TMainForm.Create(AParent: TFCustomWindow);
begin
  inherited;
  self.OnPaint := @FormPaint;
end;

procedure TMainForm.AfterConstruction;
var
  lSize: TSize;
  lPoint: TPoint;
begin

  inherited AfterConstruction;
//  BorderWidth := 8;
//  Text        := 'fpGUI Application';

  { create our button }
  btnHello := TFButton.Create('Hello World!', self);
  lSize.cx := 100;
  lSize.cy := 25;
  btnHello.SetClientSize(lSize);
  lPoint.x := 25;
  lPoint.y := 25;
  btnHello.SetPosition(lPoint);
  btnHello.Show;

  { set a min and max size }
  lSize.cx := 200;
  lSize.cy := 300;
  SetClientSize(lSize);
  SetMinMaxClientSize(lSize, lSize);
end;


var
  MainForm: TMainForm;
begin
  GFApplication.Initialize;
  MainForm := TMainForm.Create(nil);
  MainForm.Show;
  GFApplication.Run;
end.

