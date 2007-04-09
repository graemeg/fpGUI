program HelloWorld;

{$mode objfpc}{$h+}

uses
  fpGUI
  ,fpGFX    { GFApplication }
  ,gfxBase
  ;

type
  TMainForm = class(TForm)
  private
    BoxLayout: TFBoxLayout;
    btnHello: TFButton;
  public
    procedure AfterConstruction; override;
  end;


{ TMainForm }

procedure TMainForm.AfterConstruction;
var
  lSize: TSize;
begin
  inherited AfterConstruction;
  Name        := 'MainForm';
  BorderWidth := 8;
  Text        := 'fpGUI Application';

  { every fpGUI app needs a layout manager }
  BoxLayout := TFBoxLayout.Create(self);
  BoxLayout.Spacing       := 8;
  BoxLayout.VertAlign     := vertFill;
  InsertChild(BoxLayout);

  { create our button }
  btnHello := TFButton.Create('Hello World!', self);
  btnHello.CanExpandWidth   := True;
  btnHello.CanExpandHeight  := True;
  BoxLayout.InsertChild(btnHello);

  { set a min and max size }
  lSize.cx := 150;
  lSize.cy := 100;
  Wnd.SetMinMaxClientSize(lSize, lSize);
end;


var
  MainForm: TMainForm;
begin
  GFApplication.Initialize;
  MainForm := TMainForm.Create(GFApplication);
  try
    MainForm.Show;
    GFApplication.Run;
  finally
    MainForm.Free;
  end;
end.
