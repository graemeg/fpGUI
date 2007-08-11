program gridtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  fpgfx,
  gui_form,
  gui_customgrid,
  gui_button,
  gui_checkbox;


type

  TMainForm = class(TfpgForm)
  private
    btnQuit: TfpgButton;
    grdMain: TfpgGrid;
    chkShowHeader: TfpgCheckBox;
    chkShowGrid: TfpgCheckBox;
    chkRowSelect: TfpgCheckBox;
    chkDisabled: TfpgCheckBox;
    procedure   chkDisabledChange(Sender: TObject);
    procedure   chkRowSelectChange(Sender: TObject);
    procedure   chkShowHeaderChange(Sender: TObject);
    procedure   chkShowGridChange(Sender: TObject);
    procedure   btnQuitClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

procedure TMainForm.chkDisabledChange(Sender: TObject);
begin
  grdMain.Enabled := not chkDisabled.Checked;
end;

procedure TMainForm.chkRowSelectChange(Sender: TObject);
begin
  grdMain.RowSelect := chkRowSelect.Checked;
end;

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
var
  c: integer;
begin
  inherited Create(AOwner);
  WindowTitle := 'Grid control test';
  SetPosition(100, 100, 566, 350);

  btnQuit := CreateButton(self, 476, 320, 80, 'Quit', @btnQuitClick);
  btnQuit.ImageName := 'stdimg.Quit';
  btnQuit.ShowImage := True;
  btnQuit.Anchors := [anRight, anBottom];
  
  grdMain := TfpgGrid.Create(self);
  grdMain.Top      := 10;
  grdMain.Left     := 10;
  grdMain.Width    := Width - 20;
  grdMain.Height   := 300;
  grdMain.Anchors  := [anLeft, anTop, anRight, anBottom];
  grdMain.RowCount := 25;
  for c := 1 to grdMain.ColumnCount do
    grdMain.Columns[c-1].Title := 'Title ' + IntToStr(c);

  chkShowHeader := CreateCheckBox(self, 10, 320, 'Show Header');
  chkShowHeader.Checked   := True;
  chkShowHeader.OnChange  := @chkShowHeaderChange;
  chkShowHeader.Anchors   := [anLeft, anBottom];

  chkShowGrid := CreateCheckBox(self, chkShowHeader.Right+10, 320, 'Show Grid');
  chkShowGrid.Checked     := True;
  chkShowGrid.OnChange    := @chkShowGridChange;
  chkShowGrid.Anchors     := [anLeft, anBottom];

  chkRowSelect := CreateCheckBox(self, chkShowGrid.Right+10, 320, 'Row Select');
  chkRowSelect.Checked     := False;
  chkRowSelect.OnChange    := @chkRowSelectChange;
  chkRowSelect.Anchors     := [anLeft, anBottom];
  
  chkDisabled := CreateCheckBox(self, chkRowSelect.Right+10, 320, 'Disabled');
  chkDisabled.Checked     := False;
  chkDisabled.OnChange    := @chkDisabledChange;
  chkDisabled.Anchors     := [anLeft, anBottom];
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

