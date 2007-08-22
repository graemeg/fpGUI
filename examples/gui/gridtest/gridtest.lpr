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
  gui_grid,
  gui_button,
  gui_checkbox,
  gui_tab;


type

  TMainForm = class(TfpgForm)
  private
    btnQuit: TfpgButton;
    pagecontrol: TfpgPageControl;
    tsTab1: TfpgTabSheet;
    tsTab2: TfpgTabSheet;
    grdMain: TfpgGrid;
    stringgrid: TfpgStringGrid;
    chkShowHeader: TfpgCheckBox;
    chkShowGrid: TfpgCheckBox;
    chkRowSelect: TfpgCheckBox;
    chkDisabled: TfpgCheckBox;
    procedure   chkDisabledChange(Sender: TObject);
    procedure   chkRowSelectChange(Sender: TObject);
    procedure   chkShowHeaderChange(Sender: TObject);
    procedure   chkShowGridChange(Sender: TObject);
    procedure   btnQuitClick(Sender: TObject);
  protected
    procedure   HandleShow; override;
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

procedure TMainForm.HandleShow;
begin
  inherited HandleShow;
end;

constructor TMainForm.Create(AOwner: TComponent);
var
  c: integer;
  r: integer;
begin
  inherited Create(AOwner);
  WindowTitle := 'Grid control test';
  SetPosition(100, 100, 566, 350);

  btnQuit := CreateButton(self, 476, 320, 80, 'Quit', @btnQuitClick);
  btnQuit.ImageName := 'stdimg.Quit';
  btnQuit.ShowImage := True;
  btnQuit.Anchors := [anRight, anBottom];
  
  pagecontrol := TfpgPageControl.Create(self);
  pagecontrol.SetPosition(10, 10, Width-20, 300);
  pagecontrol.Anchors := [anLeft, anTop, anRight, anBottom];

  tsTab1 := TfpgTabSheet.Create(pagecontrol);
  tsTab1.Text := 'Base Grid';
  grdMain := TfpgGrid.Create(tsTab1);
  grdMain.SetPosition(10, 10, Width-50, 250);
//  grdMain.Anchors  := [anLeft, anTop, anRight, anBottom];
  grdMain.RowCount := 25;
  for c := 1 to grdMain.ColumnCount do
    grdMain.Columns[c-1].Title := 'Title ' + IntToStr(c);


  tsTab2 := pagecontrol.AppendTabSheet('String Grid');
  stringgrid := TfpgStringGrid.Create(tsTab2);
  stringgrid.SetPosition(10, 10, Width-50, 250);
  stringgrid.ColumnCount := 2;
  stringgrid.RowCount := 5;
  stringgrid.Cells[2, 1] := 'hello';
  stringgrid.Cells[5, 2] := 'hello';
  stringgrid.ColumnTitle[1] := 'Column 1';
//  stringgrid.Columns[1].Title := 'Column 1';
//  stringgrid.Columns[2].Title := 'Col2';
  //for r := 1 to stringgrid.RowCount do
    //for c := 1 to stringgrid.ColumnCount do
      //stringgrid.Cells[r, c] := IntToStr(r) + ',' + IntToStr(c);
//  stringgrid.Anchors  := [anLeft, anTop, anRight, anBottom];

  pagecontrol.ActivePageIndex := 0;

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

