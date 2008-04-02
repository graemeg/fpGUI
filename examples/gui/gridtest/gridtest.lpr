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
  gui_tab,
  gui_edit;


type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    btnQuit: TfpgButton;
    stringgrid: TfpgStringGrid;
    chkShowHeader: TfpgCheckBox;
    chkShowGrid: TfpgCheckBox;
    chkRowSelect: TfpgCheckBox;
    chkDisabled: TfpgCheckBox;
    edtTopRow: TfpgEditInteger;
    btnTopRow: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    procedure   chkDisabledChange(Sender: TObject);
    procedure   chkRowSelectChange(Sender: TObject);
    procedure   chkShowHeaderChange(Sender: TObject);
    procedure   chkShowGridChange(Sender: TObject);
    procedure   btnQuitClick(Sender: TObject);
    procedure   stringgridDrawCell(Sender: TObject; const ARow, ACol: Longword;
        const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
    procedure   btnTopRowClicked(Sender: TObject);
  public
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

{ TMainForm }

procedure TMainForm.chkDisabledChange(Sender: TObject);
begin
  stringgrid.Enabled := not chkDisabled.Checked;
end;

procedure TMainForm.chkRowSelectChange(Sender: TObject);
begin
  stringgrid.RowSelect := chkRowSelect.Checked;
end;

procedure TMainForm.chkShowHeaderChange(Sender: TObject);
begin
  stringgrid.ShowHeader := chkShowHeader.Checked;
end;

procedure TMainForm.chkShowGridChange(Sender: TObject);
begin
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

procedure TMainForm.btnTopRowClicked(Sender: TObject);
begin
  if edtTopRow.Value < 1 then
    Exit;
  stringgrid.TopRow := edtTopRow.Value;
end;

procedure TMainForm.AfterCreate;
var
  r: Longword;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(351, 214, 566, 350);
  WindowTitle := 'Grid control test';
  WindowPosition := wpScreenCenter;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(476, 320, 80, 25);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    ImageName := 'stdimg.Quit';
    OnClick := @btnQuitClick;
  end;

  stringgrid := TfpgStringGrid.Create(self);
  with stringgrid do
  begin
    Name := 'stringgrid';
    SetPosition(10, 10, 426, 250);
    Anchors := [anLeft,anRight,anTop,anBottom];
    AddColumn('Column 1', 100, taLeftJustify);
    AddColumn('Col 2', 50, taCenter);
    AddColumn('New', 150, taRightJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    RowCount := 17;
    TabOrder := 1;
    // Alignment test
    Cells[1, 2] := 'left';
    Cells[2, 2] := 'center';
    Cells[3, 2] := 'right';
    // override default colors
    BackgroundColor:= clKhaki;
    ColumnBackgroundColor[2] := clLightGray;
    TextColor:= clBlue;
    ColumnTextColor[1] := clRed;
    // add some text
    Cells[1, 1] := '(r1,c1)';
    Cells[1, 3] := 'Custom';
    Cells[2, 3] := 'Hello';
    Cells[3, 1] := '(r1,c3)';
    // Add custom painting
    OnDrawCell := @stringgridDrawCell;
  end;

  chkShowHeader := TfpgCheckBox.Create(self);
  with chkShowHeader do
  begin
    Name := 'chkShowHeader';
    SetPosition(10, 320, 100, 24);
    Anchors := [anLeft,anBottom];
    Checked := True;
    FontDesc := '#Label1';
    TabOrder := 2;
    Text := 'Show Header';
    OnChange  := @chkShowHeaderChange;
  end;

  chkShowGrid := TfpgCheckBox.Create(self);
  with chkShowGrid do
  begin
    Name := 'chkShowGrid';
    SetPosition(110, 320, 100, 24);
    Anchors := [anLeft,anBottom];
    Checked := True;
    FontDesc := '#Label1';
    TabOrder := 3;
    Text := 'Show Grid';
    OnChange    := @chkShowGridChange;
  end;

  chkRowSelect := TfpgCheckBox.Create(self);
  with chkRowSelect do
  begin
    Name := 'chkRowSelect';
    SetPosition(210, 320, 100, 24);
    Anchors := [anLeft,anBottom];
    FontDesc := '#Label1';
    TabOrder := 4;
    Text := 'Row Select';
    OnChange    := @chkRowSelectChange;
  end;

  chkDisabled := TfpgCheckBox.Create(self);
  with chkDisabled do
  begin
    Name := 'chkDisabled';
    SetPosition(310, 320, 100, 24);
    Anchors := [anLeft,anBottom];
    FontDesc := '#Label1';
    TabOrder := 5;
    Text := 'Disabled';
    OnChange    := @chkDisabledChange;
  end;

  edtTopRow := TfpgEditInteger.Create(self);
  with edtTopRow do
  begin
    Name := 'edtTopRow';
    SetPosition(12, 280, 56, 22);
    Anchors := [anLeft,anBottom];
  end;

  btnTopRow := TfpgButton.Create(self);
  with btnTopRow do
  begin
    Name := 'btnTopRow';
    SetPosition(76, 280, 91, 24);
    Anchors := [anLeft,anBottom];
    Text := 'Set TopRow';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 7;
    OnClick := @btnTopRowClicked;
  end;

  {@VFD_BODY_END: MainForm}

  for r := 1 to stringgrid.RowCount do
    stringgrid.Cells[3, r] := IntToStr(r);

end;
  

{@VFD_NEWFORM_IMPL}

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

