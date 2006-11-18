program utfdemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  gfxbase,
  fpgui, fpGUI_laz;

type

  { TMainForm }

  TMainForm = class(TForm)
  private
    FLayout: TBoxLayout;
    procedure MainFormActivate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    TextLabel: TLabel;
  end;


{ TMainForm }

procedure TMainForm.MainFormActivate(Sender: TObject);
var
  max: TSize;
begin
  max.cx := 320;
  max.cy := 200;
  Wnd.SetMinMaxClientSize(MinSize, max);
end;


constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name            := 'frmMain';
  BorderWidth     := 8;
//  WindowType      := wtWindow;
  Text            := 'UTF Demo';
  OnActivate      := @MainFormActivate;

  FLayout         := TBoxLayout.Create(self);
  FLayout.Parent  := self;
  InsertChild(FLayout);

  TextLabel       := TLabel.Create(self);
  TextLabel.Text  := '&Gráficas Magnificacion! Teste';
  FLayout.InsertChild(TextLabel);
end;


var
  MainForm: TMainForm;
begin
  MainForm := TMainForm.Create(Application);
  try
    MainForm.Show;
    Application.Run;
  finally
    MainForm.Free;
  end;
end.


