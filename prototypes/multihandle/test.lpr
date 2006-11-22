{
  Proof of concept test app for multi-handle GUI widgets.
  Graeme Geldenhuys
}

program test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  ,gui2Base
  ,gfxbase
  ,fpgfx
  ;
  
  
type

  { TMainWindow }

  TMainWindow = class(TForm)
    procedure btnCancelClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    btnClose: TButton;
    btnCancel: TButton;
  public
    constructor Create; override;
    destructor  Destroy; override;
  end;

{ TMainWindow }

procedure TMainWindow.btnCancelClick(Sender: TObject);
begin
  Writeln('You click Cancel');
end;

procedure TMainWindow.btnCloseClick(Sender: TObject);
begin
  Writeln('You click Close');
  GFApplication.Quit;
end;

constructor TMainWindow.Create;
begin
  inherited Create;
  Title := 'fpGUI multi-handle example';
  SetClientSize(Size(320, 200));

  btnClose := TButton.Create(self, Point(20, 150));
  btnClose.Caption := 'Close';
  btnClose.OnClick := @btnCloseClick;

  btnCancel := TButton.Create(self, Point(150, 150));
  btnCancel.Caption := 'Cancel';
  btnCancel.OnClick := @btnCancelClick;
end;

destructor TMainWindow.Destroy;
begin
  btnClose.Free;
  btnCancel.Free;
  inherited Destroy;
end;


var
  MainWindow: TMainWindow;
begin
  GFApplication.Initialize;
  MainWindow := TMainWindow.Create;
  GFApplication.AddWindow(MainWindow);
  MainWindow.Show;
  GFApplication.Run;
end.

