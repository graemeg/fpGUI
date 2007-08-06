program gridtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpgfx,
  gui_form,
  gui_grid,
  gui_button,
  gui_checkbox;


type

  TMainForm = class(TfpgForm)
  private
    btnQuit: TfpgButton;
    grdMain: TfpgBaseGrid;
    chkShowHeader: TfpgCheckBox;
    chkShowGrid: TfpgCheckBox;
    procedure   chkShowHeaderChange(Sender: TObject);
    procedure   chkShowGridChange(Sender: TObject);
    procedure   btnQuitClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

procedure TMainForm.chkShowHeaderChange(Sender: TObject);
begin
  grdMain.ShowHeader := chkShowHeader.Checked;
end;

procedure TMainForm.chkShowGridChange(Sender: TObject);
begin
  grdMain.ShowGrid := chkShowGrid.Checked;
end;

procedure TMainForm.btnQuitClick(Sender: TObject);
begin
  Close;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Grid control test';
  SetPosition(100, 100, 566, 350);

  btnQuit := CreateButton(self, 476, 320, 80, 'Quit', @btnQuitClick);
  btnQuit.ImageName := 'stdimg.Quit';
  btnQuit.ShowImage := True;
  btnQuit.Anchors := [anRight, anBottom];
  
  grdMain := TfpgBaseGrid.Create(self);
  grdMain.Top      := 10;
  grdMain.Left     := 10;
  grdMain.Width    := Width - 20;
  grdMain.Height   := 300;
  grdMain.Anchors  := [anLeft, anTop, anRight, anBottom];

  chkShowHeader := CreateCheckBox(self, 10, 320, 'Show Header');
  chkShowHeader.Checked   := True;
  chkShowHeader.OnChange  := @chkShowHeaderChange;

  chkShowGrid := CreateCheckBox(self, chkShowHeader.Right+10, 320, 'Show Grid');
  chkShowGrid.Checked     := True;
  chkShowGrid.OnChange    := @chkShowGridChange;
end;
  
  
procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
end;

begin
  MainProc;
end.

