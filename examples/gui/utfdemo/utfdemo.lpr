program utfdemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  ,GFXBase
  ,fpGFX
  ,fpGUI
  ;

type

  { TMainForm }

  TMainForm = class(TFForm)
  private
    FLayout: TFBoxLayout;
    procedure   MainFormActivate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    TextLabel: TFLabel;
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
  Text            := 'UTF Demo';
  OnActivate      := @MainFormActivate;

  FLayout         := TFBoxLayout.Create(self);
  FLayout.Parent  := self;
  InsertChild(FLayout);

  TextLabel       := TFLabel.Create(self);
  TextLabel.Text  := '&Gráficas Magnificacion! Teste';
  FLayout.InsertChild(TextLabel);
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


