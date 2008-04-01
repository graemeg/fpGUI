program gridtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gui_form,
  gui_basegrid,
  gui_grid,
  gui_button,
  gui_checkbox,
  gui_tab;


type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    btnQuit: TfpgButton;
    pagecontrol: TfpgPageControl;
    tsTab1: TfpgTabSheet;
    tsTab2: TfpgTabSheet;
    grdMain: TfpgStringGrid;
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
    procedure   stringgridDrawCell(Sender: TObject; const ARow, ACol: Longword;
        const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
  protected
    procedure   HandleShow; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

procedure TMainForm.chkDisabledChange(Sender: TObject);
begin
//  grdMain.Enabled := not chkDisabled.Checked;
  stringgrid.Enabled := not chkDisabled.Checked;
end;

procedure TMainForm.chkRowSelectChange(Sender: TObject);
begin
//  grdMain.RowSelect := chkRowSelect.Checked;
  stringgrid.RowSelect := chkRowSelect.Checked;
end;

procedure TMainForm.chkShowHeaderChange(Sender: TObject);
begin
//  grdMain.ShowHeader := chkShowHeader.Checked;
  stringgrid.ShowHeader := chkShowHeader.Checked;
end;

procedure TMainForm.chkShowGridChange(Sender: TObject);
begin
//  grdMain.ShowGrid := chkShowGrid.Checked;
  stringgrid.ShowGrid := chkShowGrid.Checked;
end;

procedure TMainForm.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.stringgridDrawCell(Sender: TObject; const ARow,
  ACol: Longword; const ARect: TfpgRect; const AFlags: TfpgGridDrawState;
  var ADefaultDrawing: boolean);
begin
  if (ACol = 1) and (ARow = 3) then
  begin
    ADefaultDrawing := False;
    StringGrid.Canvas.SetColor(clGreen);
    fpgStyle.DrawDirectionArrow(StringGrid.Canvas, ARect.Left, ARect.Top,
        ARect.Height, ARect.Height, 3);
    StringGrid.Canvas.SetTextColor(clTeal);
    StringGrid.Canvas.DrawString(ARect.Height + ARect.Left + 2, ARect.Top, StringGrid.Cells[ACol, ARow]);
  end;
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
//  grdMain := TfpgStringGrid.Create(tsTab1);
//  grdMain.SetPosition(10, 10, Width-50, 250);
//  grdMain.Anchors  := [anLeft, anTop, anRight, anBottom];
//  grdMain.RowCount := 25;
//  for c := 1 to grdMain.ColumnCount do
//    grdMain.Columns[c-1].Title := 'Title ' + IntToStr(c);

  tsTab2 := pagecontrol.AppendTabSheet('String Grid');
  stringgrid := TfpgStringGrid.Create(tsTab2);
  stringgrid.SetPosition(10, 10, Width-50, 250);
  // change row and column count to something different than the default
  stringgrid.ColumnCount  := 2;
  stringgrid.RowCount     := 3;
  // add and change a column
  stringgrid.AddColumn('Extra (col3)', 100);
  stringgrid.ColumnWidth[3] := 150;
  // changes header text in different ways
  stringgrid.ColumnTitle[1] := 'Column 1';
  stringgrid.Columns[2].Title := 'Col 2';
  // add some text
  stringgrid.Cells[1, 1] := '(r1,c1)';
  stringgrid.Cells[1, 3] := 'Custom';
  stringgrid.Cells[2, 3] := 'Hello';
  stringgrid.Cells[3, 1] := '(r1,c3)';
  
  // alignment test
  stringgrid.columns[1].Alignment := taLeftJustify;
  stringgrid.Cells[1, 2] := 'left';
  stringgrid.columns[2].Alignment := taCenter;
  stringgrid.Cells[2, 2] := 'center';
  stringgrid.columns[3].Alignment := taRightJustify;
  stringgrid.Cells[3, 2] := 'right';
  
  // override default colors
  stringgrid.BackgroundColor:= clKhaki;
  stringgrid.ColumnBackgroundColor[2] := clLightGray;
  stringgrid.TextColor:= clBlue;
  stringgrid.ColumnTextColor[1] := clRed;
  
  // Add custom painting
  stringgrid.OnDrawCell := @stringgridDrawCell;

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
  frm.Free;
end;

begin
  MainProc;
end.

