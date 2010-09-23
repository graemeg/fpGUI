unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_basegrid,
  fpg_grid, fpg_button, fpg_edit, fpg_label;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    Grid1: TfpgStringGrid;
    btnQuit: TfpgButton;
    Label1: TfpgLabel;
    {@VFD_HEAD_END: MainForm}
    FCellEdit: TfpgEdit;
    FFocusRect: TfpgRect;
    FLastGrid: TfpgStringGrid; // reference only
    procedure SetupCellEdit(AGrid: TfpgStringGrid);
    procedure CellEditExit(Sender: TObject);
    procedure CellEditKeypressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure GridKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure GridDrawCell(Sender: TObject; const ARow, ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
    procedure ButtonQuitClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TMainForm.GridKeyPressed(Sender: TObject; var KeyCode: word;
  var ShiftState: TShiftState; var Consumed: boolean);
begin
  if (KeyCode = keyInsert) and (ssCtrl in ShiftState) then
  begin
    TfpgStringGrid(Sender).RowCount := TfpgStringGrid(Sender).RowCount + 1;
    Consumed := True;
    Exit;
  end;

  if (KeyCode = keyF2) or (KeyCode = keyReturn) then
  begin
    // we need to edit the cell contents
    SetupCellEdit(TfpgStringGrid(Sender));
    Consumed := True;
  end;
end;

procedure TMainForm.GridDrawCell(Sender: TObject; const ARow, ACol: Integer;
  const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
begin
  if (gdSelected in AFlags) then
  begin
    FFocusRect := ARect;
  end;
end;

procedure TMainForm.ButtonQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.SetupCellEdit(AGrid: TfpgStringGrid);
var
  pt: TPoint;
begin
  if Assigned(FCellEdit) then
    FCellEdit.Free;

  FLastGrid := AGrid;
  FCellEdit := TfpgEdit.Create(FLastGrid.Parent);
  pt.X := FLastGrid.Left + FFocusRect.Left;
  pt.Y := FLastGrid.Top + FFocusRect.Top;
  with FCellEdit do
  begin
    Name := 'FCellEdit';
    SetPosition(pt.X, pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    Text := AGrid.Cells[AGrid.FocusCol, AGrid.FocusRow];
    OnKeyPress := @CellEditKeypressed;
    OnExit  := @CellEditExit;
    SetFocus;
  end;
end;

procedure TMainForm.CellEditExit(Sender: TObject);
begin
  FCellEdit.Visible := False;
end;

procedure TMainForm.CellEditKeypressed(Sender: TObject; var KeyCode: word;
  var ShiftState: TShiftState; var Consumed: boolean);
begin
  if KeyCode = keyReturn then
  begin
    FLastGrid.Cells[FLastGrid.FocusCol, FLastGrid.FocusRow] := FCellEdit.Text;
    FCellEdit.Visible := False;
    FLastGrid.SetFocus;
  end;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(356, 172, 431, 361);
  WindowTitle := 'Editable StringGrid Demo';
  Hint := '';

  Grid1 := TfpgStringGrid.Create(self);
  with Grid1 do
  begin
    Name := 'Grid1';
    SetPosition(16, 76, 400, 239);
    Anchors := [anLeft,anRight,anTop,anBottom];
    AddColumn('#1', 100, taLeftJustify);
    AddColumn('#2', 100, taLeftJustify);
    AddColumn('#3', 100, taLeftJustify);
    AddColumn('#4', 75, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 5;
    RowSelect := False;
    TabOrder := 1;
    OnKeyPress  := @GridKeyPressed;
    OnDrawCell  := @GridDrawCell;
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(336, 329, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    Down := False;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
    OnClick  := @ButtonQuitClicked;
  end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(16, 8, 400, 56);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    Text := 'You can use Ctrl+Ins to add new grid lines. F2 or Enter will put you in edit mode. While in edit mode, changing focus or pressing Enter will take you back to non-edit mode.';
    WrapText := True;
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
end;


end.
