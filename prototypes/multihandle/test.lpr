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

  TMainWindow = class(TFForm)
    procedure btnCancelClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnPopupClick(Sender: TObject);
  private
    btnClose: TFButton;
    btnCancel: TFButton;
    btnPopup: TFButton;
    lblWelcome: TFLabel;
    edEdit: TFEdit;
  public
    constructor Create; override;
    destructor  Destroy; override;
  end;
  
  
  TMyPopup = class(TFPopupWindow)
  public
    constructor Create; override;
  end;
  
const
  clBlue: TGfxColor           = (Red: $0000; Green: $0000; Blue: $FF00; Alpha: 0);
  clLightSteelBlue: TGfxColor = (Red: $B000; Green: $C400; Blue: $DE00; Alpha: 0);

{ TMyPopup }

constructor TMyPopup.Create;
begin
  inherited Create;
  Title := 'My Popup';
  SetClientSize(Size(180, 320));
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

procedure TMainWindow.btnPopupClick(Sender: TObject);
var
  frm: TMyPopup;
begin
  frm := TMyPopup.Create;

  GFApplication.AddWindow(frm);
//  frm.SetPosition(Point(0, btnPopup.Height));
  frm.Show;
end;

constructor TMainWindow.Create;
begin
  inherited Create;
  Title := 'fpGUI multi-handle example';
  SetClientSize(Size(320, 200));
  Color := clLightSteelBlue;

  btnClose := TFButton.Create(self, Point(20, 150));
  btnClose.Caption := 'Close';
  btnClose.OnClick := @btnCloseClick;

  btnCancel := TFButton.Create(self, Point(150, 150));
  btnCancel.Caption := 'Cancel';
  btnCancel.OnClick := @btnCancelClick;
  
  btnPopup := TFButton.Create(self, Point(80, 80));
  btnPopup.Caption := 'Popup';
  btnPopup.OnClick := @btnPopupClick;

  lblWelcome := TFLabel.Create(self, Point(10, 10));
  lblWelcome.Caption := 'So what do you think?';
  
  edEdit := TFEdit.Create(self, Point(65, 110));
  edEdit.Text := 'Multi-Handle widgets';
end;

destructor TMainWindow.Destroy;
begin
  btnClose.Free;
  btnCancel.Free;
  btnPopup.Free;
  lblWelcome.Free;
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
  MainWindow.Free;
end.

