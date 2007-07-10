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
//    BoxLayout: TFBoxLayout;
    btnHello: TFButton;
  public
    procedure AfterConstruction; override;
  end;


{ TMainForm }

procedure TMainForm.AfterConstruction;
var
  lSize: TSize;
  lPoint: TPoint;
begin
  inherited AfterConstruction;
  Name        := 'MainForm';
//  BorderWidth := 8;
//  Text        := 'fpGUI Application';

  { every fpGUI app needs a layout manager }
{  BoxLayout := TFBoxLayout.Create(self);
  BoxLayout.Spacing       := 8;
  BoxLayout.VertAlign     := vertFill;
  InsertChild(BoxLayout);}

  { create our button }
  btnHello := TFButton.Create('Hello World!', self);
{  btnHello.CanExpandWidth   := True;
  btnHello.CanExpandHeight  := True;
  BoxLayout.InsertChild(btnHello); }
  lSize.cx := 50;
  lSize.cy := 100;
  btnHello.SetMinMaxClientSize(lSize, lSize);
  lPoint.x := 25;
  lPoint.y := 25;
  btnHello.SetPosition(lPoint);

  { set a min and max size }
  lSize.cx := 200;
  lSize.cy := 200;
  SetMinMaxClientSize(lSize, lSize);
end;


var
  MainForm: TMainForm;
begin
  GFApplication.Initialize;
  MainForm := TMainForm.Create(nil);
  try
//    GFApplication.AddWindow(MainForm);
    MainForm.Show;
    GFApplication.Run;
  finally
//    MainForm.Free;
  end;
end.

