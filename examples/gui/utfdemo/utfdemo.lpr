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
    Edit: TFEdit;
  end;


{ TMainForm }

procedure TMainForm.MainFormActivate(Sender: TObject);
var
  max: TSize;
begin
  max.cx := 250;
  max.cy := 80;
  Wnd.SetMinMaxClientSize(max, max);
end;


constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name            := 'frmMain';
  BorderWidth     := 8;
  Text            := 'UTF Demo';
  OnActivate      := @MainFormActivate;

  FLayout             := TFBoxLayout.Create(self);
  FLayout.Orientation := Vertical;
  FLayout.Parent      := self;
  InsertChild(FLayout);

  TextLabel       := TFLabel.Create(self);
//  TextLabel.Text  := 'Gráficas Magnificacion! Teste';
  TextLabel.Text  := 'Test Russian text -> Òåñò';
  FLayout.InsertChild(TextLabel);
  
  Edit                := TFEdit.Create(self);
  Edit.CanExpandWidth := True;
//  Edit.Text           := 'Gráficas Magnificacion! Teste';
  Edit.Text           := 'Test Russian text -> Òåñò';
  FLayout.InsertChild(Edit);
end;


var
  MainForm: TMainForm;
begin
  GFApplication.Initialize;
  MainForm := TMainForm.Create(nil);
  try
    MainForm.Show;
    GFApplication.Run;
  finally
    MainForm.Free;
  end;
end.


