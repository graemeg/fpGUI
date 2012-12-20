program gridtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_basegrid,
  fpg_grid,
  fpg_button,
  fpg_checkbox,
  fpg_tab,
  fpg_edit;


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
    chkHideFocus: TfpgCheckBox;
    edtTopRow: TfpgEditInteger;
    btnTopRow: TfpgButton;
    btnAddFive: TfpgButton;
    btnAddOne: TfpgButton;
    btnFiveOnly: TfpgButton;
    btnDelRow: TfpgButton;
    chkSmoothScroll: TfpgCheckBox;
    chkAlterColor: TfpgCheckBox;
    {@VFD_HEAD_END: MainForm}
    procedure   StringGridDoubleClicked(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   btnAddFiveClicked(Sender: TObject);
    procedure   btnAddOneClicked(Sender: TObject);
    procedure   btnFiveOnlyClicked(Sender: TObject);
    procedure   btnDelRowClicked(Sender: TObject);
    procedure   chkDisabledChange(Sender: TObject);
    procedure   chkRowSelectChange(Sender: TObject);
    procedure   chkShowHeaderChange(Sender: TObject);
    procedure   chkShowGridChange(Sender: TObject);
    procedure   chkHideFocusChange(Sender: TObject);
    procedure   chkSmoothScrollChange(Sender: TObject);
    procedure   chkAlterColorChange(Sender: TObject);
    procedure   btnQuitClick(Sender: TObject);
    procedure   stringgridDrawCell(Sender: TObject; const ARow, ACol: Integer;
        const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
    procedure   btnTopRowClicked(Sender: TObject);
  public
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

{ TMainForm }

procedure TMainForm.StringGridDoubleClicked(Sender: TObject;
  AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
var
  lCol, lRow: integer;
begin
  StringGrid.MouseToCell(AMousePos.X, AMousePos.Y, lCol, lRow);
  StringGrid.Cells[lCol, lRow] := Format('(c%d,r%d)', [lCol, lRow]);
end;

procedure TMainForm.btnAddFiveClicked(Sender: TObject);
begin
  StringGrid.RowCount := StringGrid.RowCount + 5;
end;

procedure TMainForm.btnAddOneClicked(Sender: TObject);
begin
  StringGrid.RowCount := StringGrid.RowCount + 1;
end;

procedure TMainForm.btnFiveOnlyClicked(Sender: TObject);
begin
  StringGrid.RowCount := 5;
end;

procedure TMainForm.btnDelRowClicked(Sender: TObject);
begin
  stringgrid.DeleteRow(stringgrid.FocusRow);
end;

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

procedure TMainForm.chkHideFocusChange(Sender: TObject);
begin
  if chkHideFocus.Checked then
    stringgrid.Options := stringgrid.Options + [go_HideFocusRect]
  else
    stringgrid.Options := stringgrid.Options - [go_HideFocusRect];
  stringgrid.Invalidate;
end;

procedure TMainForm.chkSmoothScrollChange(Sender: TObject);
begin
  if chkSmoothScroll.Checked then
    stringgrid.Options := stringgrid.Options + [go_SmoothScroll]
  else
    stringgrid.Options := stringgrid.Options - [go_SmoothScroll];
  stringgrid.Update;
end;

procedure TMainForm.chkAlterColorChange(Sender: TObject);
begin
  if chkAlterColor.Checked then
    stringgrid.Options := stringgrid.Options + [go_AlternativeColor]
  else
    stringgrid.Options := stringgrid.Options - [go_AlternativeColor];
  stringgrid.Invalidate;
end;

procedure TMainForm.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.stringgridDrawCell(Sender: TObject; const ARow,
  ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState;
  var ADefaultDrawing: boolean);
begin
  if (ACol = 1) and (ARow = 3) then
  begin
    ADefaultDrawing := False;
    StringGrid.Canvas.SetColor(clGreen);
    fpgStyle.DrawDirectionArrow(StringGrid.Canvas, ARect.Left, ARect.Top,
        ARect.Height, ARect.Height, adRight);
    StringGrid.Canvas.SetTextColor(clTeal);
    StringGrid.Canvas.DrawString(ARect.Height + ARect.Left + 2, ARect.Top, StringGrid.Cells[ACol, ARow]);
  end;
  // two rows with different background color
  if (ARow = 7) or (ARow = 8) then
  begin
    if ((gdSelected in AFlags) and (gdFocused in AFlags)) or
       (gdSelected in AFlags) then
      Exit; // we want select cel to be painted as normal
    // If we got here, we must do some painting. The background first.
    StringGrid.Canvas.Color := clOrange;
    StringGrid.Canvas.FillRectangle(ARect);
    // NOTE: We want the grid to take care of the drawing of the text, which
    //  handles text layout and alignment, so we MUST NOT set the
    //  ADefaultDrawing to False. If we do, we need to handle text painting
    //  ourselves.
  end;
end;

procedure TMainForm.btnTopRowClicked(Sender: TObject);
begin
  if edtTopRow.Value < 0 then
    Exit;
  stringgrid.TopRow := edtTopRow.Value;
end;

procedure TMainForm.AfterCreate;
var
  r: Longword;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(351, 214, 515, 350);
  WindowTitle := 'Grid control test';
  Hint := '';
  WindowPosition := wpScreenCenter;
  MinWidth := 200;
  MinHeight := 200;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(425, 320, 80, 25);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.Quit';
    TabOrder := 1;
    OnClick := @btnQuitClick;
  end;

  stringgrid := TfpgStringGrid.Create(self);
  with stringgrid do
  begin
    Name := 'stringgrid';
    SetPosition(10, 10, 375, 250);
    Anchors := [anLeft,anRight,anTop,anBottom];
    AddColumn('Column 1', 100, taLeftJustify);
    AddColumn('Col 2', 50, taCenter);
    AddColumn('Numbers', 150, taRightJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 17;
    RowSelect := False;
    TabOrder := 1;
    AddColumn('Column 0', 65);
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
    Cells[0, 0] := '[c0, r0]';
    Cells[1, 1] := '[c1, r1]';
    Cells[1, 3] := 'Custom';
    Cells[2, 3] := 'Hello';
    Cells[3, 1] := '[c3, r1]';
    // Add custom painting
    OnDrawCell := @StringGridDrawCell;
    OnDoubleClick := @StringGridDoubleClicked;
  end;

  chkShowHeader := TfpgCheckBox.Create(self);
  with chkShowHeader do
  begin
    Name := 'chkShowHeader';
    SetPosition(394, 12, 116, 24);
    Anchors := [anRight,anTop];
    Checked := True;
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 2;
    Text := 'Show Header';
    OnChange := @chkShowHeaderChange;
  end;

  chkShowGrid := TfpgCheckBox.Create(self);
  with chkShowGrid do
  begin
    Name := 'chkShowGrid';
    SetPosition(394, 36, 120, 24);
    Anchors := [anRight,anTop];
    Checked := True;
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 3;
    Text := 'Show Grid';
    OnChange := @chkShowGridChange;
  end;

  chkRowSelect := TfpgCheckBox.Create(self);
  with chkRowSelect do
  begin
    Name := 'chkRowSelect';
    SetPosition(394, 60, 116, 24);
    Anchors := [anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 4;
    Text := 'Row Select';
    OnChange := @chkRowSelectChange;
  end;

  chkDisabled := TfpgCheckBox.Create(self);
  with chkDisabled do
  begin
    Name := 'chkDisabled';
    SetPosition(394, 84, 116, 24);
    Anchors := [anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 5;
    Text := 'Disabled';
    OnChange := @chkDisabledChange;
  end;

  chkHideFocus := TfpgCheckBox.Create(self);
  with chkHideFocus do
  begin
    Name := 'chkHideFocus';
    SetPosition(394, 108, 120, 24);
    Anchors := [anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 6;
    Text := 'Hide Focus';
    OnChange := @chkHideFocusChange;
  end;

  edtTopRow := TfpgEditInteger.Create(self);
  with edtTopRow do
  begin
    Name := 'edtTopRow';
    SetPosition(12, 280, 56, 24);
    Anchors := [anLeft,anBottom];
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 8;
    Value := 0;
  end;

  btnTopRow := TfpgButton.Create(self);
  with btnTopRow do
  begin
    Name := 'btnTopRow';
    SetPosition(72, 280, 91, 23);
    Anchors := [anLeft,anBottom];
    Text := 'Set TopRow';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 7;
    OnClick := @btnTopRowClicked;
  end;

  btnAddFive := TfpgButton.Create(self);
  with btnAddFive do
  begin
    Name := 'btnAddFive';
    SetPosition(168, 280, 80, 23);
    Anchors := [anLeft,anBottom];
    Text := 'Add 5 lines';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 8;
    OnClick := @btnAddFiveClicked;
  end;

  btnAddOne := TfpgButton.Create(self);
  with btnAddOne do
  begin
    Name := 'btnAddOne';
    SetPosition(252, 280, 80, 23);
    Anchors := [anLeft,anBottom];
    Text := 'Add 1 line';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 9;
    OnClick := @btnAddOneClicked;
  end;

  btnFiveOnly := TfpgButton.Create(self);
  with btnFiveOnly do
  begin
    Name := 'btnFiveOnly';
    SetPosition(336, 280, 80, 23);
    Anchors := [anLeft,anBottom];
    Text := '5 lines only';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 10;
    OnClick := @btnFiveOnlyClicked;
  end;

  btnDelRow := TfpgButton.Create(self);
  with btnDelRow do
  begin
    Name := 'btnDelRow';
    SetPosition(168, 308, 80, 23);
    Anchors := [anLeft,anBottom];
    Text := 'Delete Row';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 12;
    OnClick := @btnDelRowClicked;
  end;

  chkSmoothScroll := TfpgCheckBox.Create(self);
  with chkSmoothScroll do
  begin
    Name := 'chkSmoothScroll';
    SetPosition(394, 132, 120, 24);
    Anchors := [anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 14;
    Text := 'Smooth Scroll';
    OnChange := @chkSmoothScrollChange;
  end;

  chkAlterColor := TfpgCheckBox.Create(self);
  with chkAlterColor do
  begin
    Name := 'chkAlterColor';
    SetPosition(394, 156, 120, 24);
    Anchors := [anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 15;
    Text := 'Alternate Color';
    OnChange := @chkAlterColorChange;
  end;

  {@VFD_BODY_END: MainForm}

  for r := 0 to stringgrid.RowCount-1 do
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

