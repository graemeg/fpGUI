unit vertex.wgt.shapepanel;

{
  TVertexShapePanel — properties panel for the currently selected shape.

  Sections:
    Name         — editable; TVertexCmdRename on commit.
    Style        — combobox showing all document styles; change fires TVertexCmdSetShapeStyle.
    Paths        — list of assigned path names + Add (popup menu of unassigned paths) / Remove.
    Translation  — X offset / Y offset spin edits (HVIF units ×100 to allow decimals).
    LOD          — Min / Max spin edits (0 = unbounded for Max).
    Visible      — checkbox.
    Transformer  — type combobox (None/Stroke); Stroke sub-controls shown when active.

  All edits produce undo entries via the appropriate TVertexCmd* commands.
  SetShape(nil) clears and disables all controls.
  DocumentChanged refreshes controls after undo/redo.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  fpg_base, fpg_main, fpg_widget, fpg_panel,
  fpg_label, fpg_edit, fpg_checkbox, fpg_button,
  fpg_spinedit, fpg_combobox, fpg_listbox,
  fpg_menu, fpg_dialogs,
  fpg_hvif_model,
  fpg_vertex_document;


type
  TVertexShapePanel = class(TfpgBevel)
  private
    FDocument: TVertexDocument;   { not owned }
    FShape:    TVertexShape;      { current shape, not owned; nil = no selection }
    FUpdating: Boolean;

    { Name row }
    FLblHeader:    TfpgLabel;
    FLblName:      TfpgLabel;
    FNameEdit:     TfpgEdit;

    { Style row }
    FLblStyle:     TfpgLabel;
    FStyleCombo:   TfpgComboBox;

    { Paths section }
    FLblPaths:     TfpgLabel;
    FPathList:     TfpgListBox;
    FBtnPathAdd:   TfpgButton;
    FBtnPathDel:   TfpgButton;

    { Translation }
    FLblXOff:      TfpgLabel;
    FXOffSpin:     TfpgSpinEditFloat;
    FLblYOff:      TfpgLabel;
    FYOffSpin:     TfpgSpinEditFloat;

    { LOD }
    FLblLODMin:    TfpgLabel;
    FLODMinSpin:   TfpgSpinEditFloat;
    FLblLODMax:    TfpgLabel;
    FLODMaxSpin:   TfpgSpinEditFloat;

    { Visible }
    FVisibleChk:   TfpgCheckBox;

    { Transformer }
    FLblTrans:     TfpgLabel;
    FTransCombo:   TfpgComboBox;
    FLblWidth:     TfpgLabel;
    FWidthSpin:    TfpgSpinEditFloat;
    FLblCaps:      TfpgLabel;
    FCapsCombo:    TfpgComboBox;
    FLblJoins:     TfpgLabel;
    FJoinsCombo:   TfpgComboBox;
    FLblMiter:     TfpgLabel;
    FMiterSpin:    TfpgSpinEditFloat;

    procedure SetupControls;
    procedure UpdateControls;
    procedure UpdateStyleCombo;
    procedure UpdatePathList;
    procedure UpdateTransformerControls;
    procedure SetControlsEnabled(AEnabled: Boolean);

    procedure NameEditExit(Sender: TObject);
    procedure StyleComboChanged(Sender: TObject);
    procedure PathAddClick(Sender: TObject);
    procedure PathAddMenuClick(Sender: TObject);
    procedure PathDelClick(Sender: TObject);
    procedure XOffSpinExit(Sender: TObject);
    procedure YOffSpinExit(Sender: TObject);
    procedure LODMinSpinExit(Sender: TObject);
    procedure LODMaxSpinExit(Sender: TObject);
    procedure VisibleChkChanged(Sender: TObject);
    procedure TransComboChanged(Sender: TObject);
    procedure WidthSpinExit(Sender: TObject);
    procedure CapsComboChanged(Sender: TObject);
    procedure JoinsComboChanged(Sender: TObject);
    procedure MiterSpinExit(Sender: TObject);

    procedure CommitTransformer;

  public
    constructor Create(AOwner: TComponent); override;

    procedure SetDocument(ADoc: TVertexDocument);
    procedure SetShape(AShape: TVertexShape);
    procedure DocumentChanged;
  end;


implementation


const
  LBL_X   = 4;
  CTL_X   = 70;
  CTL_W   = 144;
  ROW_H   = 24;
  LBL_H   = 18;

  R0  = 4;
  R1  = R0 + 20;          { name }
  R2  = R1 + ROW_H;       { style }
  R3  = R2 + ROW_H;       { paths label }
  R4  = R3 + LBL_H;       { path list }
  R5  = R4 + 52;          { path add/del buttons }
  R6  = R5 + ROW_H + 4;   { x offset }
  R7  = R6 + ROW_H;       { y offset }
  R8  = R7 + ROW_H;       { lod min }
  R9  = R8 + ROW_H;       { lod max }
  R10 = R9 + ROW_H;       { visible }
  R11 = R10 + ROW_H + 4;  { transformer label }
  R12 = R11 + LBL_H;      { trans type combo }
  R13 = R12 + ROW_H;      { width }
  R14 = R13 + ROW_H;      { caps }
  R15 = R14 + ROW_H;      { joins }
  R16 = R15 + ROW_H;      { miter }

  PANEL_H = R16 + ROW_H + 6;


constructor TVertexShapePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument := nil;
  FShape    := nil;
  FUpdating := False;
  PreferredSize := fpgSize(220, PANEL_H);
  SetupControls;
  UpdateControls;
end;

procedure TVertexShapePanel.SetupControls;
begin
  FLblHeader := TfpgLabel.Create(Self);
  FLblHeader.SetPosition(LBL_X, R0, 210, LBL_H);
  FLblHeader.Text     := 'Shape Properties';
  FLblHeader.FontDesc := '#Label1:bold';

  { Name }
  FLblName := TfpgLabel.Create(Self);
  FLblName.SetPosition(LBL_X, R1 + 4, CTL_X - 6, LBL_H);
  FLblName.Text := 'Name:';

  FNameEdit := TfpgEdit.Create(Self);
  FNameEdit.SetPosition(CTL_X, R1, CTL_W, ROW_H);
  FNameEdit.OnExit := @NameEditExit;

  { Style }
  FLblStyle := TfpgLabel.Create(Self);
  FLblStyle.SetPosition(LBL_X, R2 + 4, CTL_X - 6, LBL_H);
  FLblStyle.Text := 'Style:';

  FStyleCombo := TfpgComboBox.Create(Self);
  FStyleCombo.SetPosition(CTL_X, R2, CTL_W, ROW_H);
  FStyleCombo.OnChange := @StyleComboChanged;

  { Paths }
  FLblPaths := TfpgLabel.Create(Self);
  FLblPaths.SetPosition(LBL_X, R3, 210, LBL_H);
  FLblPaths.Text := 'Paths:';

  FPathList := TfpgListBox.Create(Self);
  FPathList.SetPosition(LBL_X, R4, 210, 50);

  FBtnPathAdd := TfpgButton.Create(Self);
  FBtnPathAdd.SetPosition(LBL_X, R5, 100, ROW_H);
  FBtnPathAdd.Text    := 'Add path';
  FBtnPathAdd.OnClick := @PathAddClick;

  FBtnPathDel := TfpgButton.Create(Self);
  FBtnPathDel.SetPosition(LBL_X + 106, R5, 100, ROW_H);
  FBtnPathDel.Text    := 'Remove';
  FBtnPathDel.OnClick := @PathDelClick;

  { Translation }
  FLblXOff := TfpgLabel.Create(Self);
  FLblXOff.SetPosition(LBL_X, R6 + 4, CTL_X - 6, LBL_H);
  FLblXOff.Text := 'X offset:';

  FXOffSpin := TfpgSpinEditFloat.Create(Self);
  FXOffSpin.SetPosition(CTL_X, R6, CTL_W, ROW_H);
  FXOffSpin.MinValue := -128;
  FXOffSpin.MaxValue := 128;
  FXOffSpin.Decimals := 2;
  FXOffSpin.OnExit   := @XOffSpinExit;

  FLblYOff := TfpgLabel.Create(Self);
  FLblYOff.SetPosition(LBL_X, R7 + 4, CTL_X - 6, LBL_H);
  FLblYOff.Text := 'Y offset:';

  FYOffSpin := TfpgSpinEditFloat.Create(Self);
  FYOffSpin.SetPosition(CTL_X, R7, CTL_W, ROW_H);
  FYOffSpin.MinValue := -128;
  FYOffSpin.MaxValue := 128;
  FYOffSpin.Decimals := 2;
  FYOffSpin.OnExit   := @YOffSpinExit;

  { LOD }
  FLblLODMin := TfpgLabel.Create(Self);
  FLblLODMin.SetPosition(LBL_X, R8 + 4, CTL_X - 6, LBL_H);
  FLblLODMin.Text := 'LOD min:';

  FLODMinSpin := TfpgSpinEditFloat.Create(Self);
  FLODMinSpin.SetPosition(CTL_X, R8, CTL_W, ROW_H);
  FLODMinSpin.MinValue := 0;
  FLODMinSpin.MaxValue := 9999;
  FLODMinSpin.Decimals := 0;
  FLODMinSpin.OnExit   := @LODMinSpinExit;

  FLblLODMax := TfpgLabel.Create(Self);
  FLblLODMax.SetPosition(LBL_X, R9 + 4, CTL_X - 6, LBL_H);
  FLblLODMax.Text := 'LOD max:';

  FLODMaxSpin := TfpgSpinEditFloat.Create(Self);
  FLODMaxSpin.SetPosition(CTL_X, R9, CTL_W, ROW_H);
  FLODMaxSpin.MinValue := 0;
  FLODMaxSpin.MaxValue := 9999;
  FLODMaxSpin.Decimals := 0;
  FLODMaxSpin.OnExit   := @LODMaxSpinExit;

  { Visible }
  FVisibleChk := TfpgCheckBox.Create(Self);
  FVisibleChk.SetPosition(LBL_X, R10, 210, ROW_H);
  FVisibleChk.Text     := 'Visible';
  FVisibleChk.OnChange := @VisibleChkChanged;

  { Transformer }
  FLblTrans := TfpgLabel.Create(Self);
  FLblTrans.SetPosition(LBL_X, R11, 210, LBL_H);
  FLblTrans.Text     := 'Transformer:';
  FLblTrans.FontDesc := '#Label1:bold';

  FTransCombo := TfpgComboBox.Create(Self);
  FTransCombo.SetPosition(LBL_X, R12, 210, ROW_H);
  FTransCombo.Items.Add('None');
  FTransCombo.Items.Add('Stroke');
  FTransCombo.FocusItem := 0;
  FTransCombo.OnChange  := @TransComboChanged;

  FLblWidth := TfpgLabel.Create(Self);
  FLblWidth.SetPosition(LBL_X, R13 + 4, CTL_X - 6, LBL_H);
  FLblWidth.Text := 'Width:';

  FWidthSpin := TfpgSpinEditFloat.Create(Self);
  FWidthSpin.SetPosition(CTL_X, R13, CTL_W, ROW_H);
  FWidthSpin.MinValue := 0.1;
  FWidthSpin.MaxValue := 100;
  FWidthSpin.Decimals := 2;
  FWidthSpin.OnExit   := @WidthSpinExit;

  FLblCaps := TfpgLabel.Create(Self);
  FLblCaps.SetPosition(LBL_X, R14 + 4, CTL_X - 6, LBL_H);
  FLblCaps.Text := 'Caps:';

  FCapsCombo := TfpgComboBox.Create(Self);
  FCapsCombo.SetPosition(CTL_X, R14, CTL_W, ROW_H);
  FCapsCombo.Items.Add('Butt');
  FCapsCombo.Items.Add('Square');
  FCapsCombo.Items.Add('Round');
  FCapsCombo.FocusItem := 0;
  FCapsCombo.OnChange  := @CapsComboChanged;

  FLblJoins := TfpgLabel.Create(Self);
  FLblJoins.SetPosition(LBL_X, R15 + 4, CTL_X - 6, LBL_H);
  FLblJoins.Text := 'Joins:';

  FJoinsCombo := TfpgComboBox.Create(Self);
  FJoinsCombo.SetPosition(CTL_X, R15, CTL_W, ROW_H);
  FJoinsCombo.Items.Add('Miter');
  FJoinsCombo.Items.Add('Round');
  FJoinsCombo.Items.Add('Bevel');
  FJoinsCombo.FocusItem := 0;
  FJoinsCombo.OnChange  := @JoinsComboChanged;

  FLblMiter := TfpgLabel.Create(Self);
  FLblMiter.SetPosition(LBL_X, R16 + 4, CTL_X - 6, LBL_H);
  FLblMiter.Text := 'Miter lim:';

  FMiterSpin := TfpgSpinEditFloat.Create(Self);
  FMiterSpin.SetPosition(CTL_X, R16, CTL_W, ROW_H);
  FMiterSpin.MinValue := 1;
  FMiterSpin.MaxValue := 100;
  FMiterSpin.Decimals := 2;
  FMiterSpin.OnExit   := @MiterSpinExit;
end;

procedure TVertexShapePanel.SetControlsEnabled(AEnabled: Boolean);
begin
  FNameEdit.Enabled   := AEnabled;
  FStyleCombo.Enabled := AEnabled;
  FPathList.Enabled   := AEnabled;
  FBtnPathAdd.Enabled := AEnabled;
  FBtnPathDel.Enabled := AEnabled;
  FXOffSpin.Enabled   := AEnabled;
  FYOffSpin.Enabled   := AEnabled;
  FLODMinSpin.Enabled := AEnabled;
  FLODMaxSpin.Enabled := AEnabled;
  FVisibleChk.Enabled := AEnabled;
  FTransCombo.Enabled := AEnabled;
  FWidthSpin.Enabled  := AEnabled;
  FCapsCombo.Enabled  := AEnabled;
  FJoinsCombo.Enabled := AEnabled;
  FMiterSpin.Enabled  := AEnabled;
end;

procedure TVertexShapePanel.UpdateStyleCombo;
var
  i, cur: Integer;
begin
  FStyleCombo.Items.Clear;
  if FDocument = nil then Exit;
  cur := -1;
  for i := 0 to FDocument.StyleCount - 1 do
  begin
    FStyleCombo.Items.Add(FDocument.Styles[i].Name);
    if (FShape <> nil) and (FDocument.Styles[i] = FShape.Style) then
      cur := i;
  end;
  if cur >= 0 then
    FStyleCombo.FocusItem := cur;
end;

procedure TVertexShapePanel.UpdatePathList;
var
  i: Integer;
begin
  FPathList.Items.Clear;
  if FShape = nil then Exit;
  for i := 0 to FShape.PathCount - 1 do
    FPathList.Items.Add(FShape.Paths[i].Name);
end;

procedure TVertexShapePanel.UpdateTransformerControls;
var
  isStroke: Boolean;
  t: TVertexTransformer;
  joinIdx: Integer;
begin
  isStroke := (FShape <> nil) and (FShape.Transformer.TransType = ittStroke);
  FLblWidth.Visible   := isStroke;
  FWidthSpin.Visible  := isStroke;
  FLblCaps.Visible    := isStroke;
  FCapsCombo.Visible  := isStroke;
  FLblJoins.Visible   := isStroke;
  FJoinsCombo.Visible := isStroke;
  FLblMiter.Visible   := isStroke;
  FMiterSpin.Visible  := isStroke;

  if not isStroke then Exit;

  t := FShape.Transformer;
  FWidthSpin.Value := t.Width;
  FCapsCombo.FocusItem := t.LineCap;
  case t.LineJoin of
    2:   joinIdx := 1;
    3:   joinIdx := 2;
  else   joinIdx := 0;
  end;
  FJoinsCombo.FocusItem := joinIdx;
  FMiterSpin.Value := t.MiterLimit;
end;

procedure TVertexShapePanel.UpdateControls;
var
  lod: TVertexLOD;
begin
  FUpdating := True;
  try
    if FShape = nil then
    begin
      FNameEdit.Text        := '';
      FStyleCombo.Items.Clear;
      FPathList.Items.Clear;
      FXOffSpin.Value       := 0;
      FYOffSpin.Value       := 0;
      FLODMinSpin.Value     := 0;
      FLODMaxSpin.Value     := 0;
      FVisibleChk.Checked   := True;
      FTransCombo.FocusItem := 0;
      SetControlsEnabled(False);
      UpdateTransformerControls;
      Exit;
    end;

    FNameEdit.Text := FShape.Name;
    UpdateStyleCombo;
    UpdatePathList;

    FXOffSpin.Value := FShape.TranslateX;
    FYOffSpin.Value := FShape.TranslateY;

    lod := FShape.LOD;
    FLODMinSpin.Value := lod.MinSize;
    if lod.MaxSize >= MaxSingle / 2 then
      FLODMaxSpin.Value := 0
    else
      FLODMaxSpin.Value := lod.MaxSize;

    FVisibleChk.Checked := FShape.Visible;

    case FShape.Transformer.TransType of
      ittStroke: FTransCombo.FocusItem := 1;
    else
      FTransCombo.FocusItem := 0;
    end;

    SetControlsEnabled(True);
    UpdateTransformerControls;
  finally
    FUpdating := False;
  end;
end;


{ ── Event handlers ───────────────────────────────────────────────────────── }

procedure TVertexShapePanel.NameEditExit(Sender: TObject);
var
  newName: string;
  cmd:     TVertexCmdRename;
begin
  if FUpdating or (FShape = nil) or (FDocument = nil) then Exit;
  newName := Trim(FNameEdit.Text);
  if (newName = '') or (newName = FShape.Name) then
  begin
    FNameEdit.Text := FShape.Name;
    Exit;
  end;
  if FDocument.NameExists(newName) then
  begin
    FNameEdit.Text := FShape.Name;
    Exit;
  end;
  cmd := TVertexCmdRename.Create(FShape.NamePtr, FShape.Name, newName);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexShapePanel.StyleComboChanged(Sender: TObject);
var
  idx: Integer;
  cmd: TVertexCmdSetShapeStyle;
begin
  if FUpdating or (FShape = nil) or (FDocument = nil) then Exit;
  idx := FStyleCombo.FocusItem;
  if (idx < 0) or (idx >= FDocument.StyleCount) then Exit;
  if FDocument.Styles[idx] = FShape.Style then Exit;
  cmd := TVertexCmdSetShapeStyle.Create(FShape, FDocument.Styles[idx]);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexShapePanel.PathAddClick(Sender: TObject);
var
  i:      Integer;
  menu:   TfpgPopupMenu;
  item:   TfpgMenuItem;
  nAdded: Integer;
begin
  if (FShape = nil) or (FDocument = nil) then Exit;
  if FDocument.PathCount = 0 then Exit;

  nAdded := 0;
  menu   := TfpgPopupMenu.Create(Self);
  for i := 0 to FDocument.PathCount - 1 do
  begin
    if FShape.IndexOfPath(FDocument.Paths[i]) < 0 then
    begin
      item     := menu.AddMenuItem(FDocument.Paths[i].Name, '', @PathAddMenuClick);
      item.Tag := i;
      Inc(nAdded);
    end;
  end;
  if nAdded = 0 then Exit;
  menu.ShowAt(FBtnPathAdd, 0, FBtnPathAdd.Height);
end;

procedure TVertexShapePanel.PathAddMenuClick(Sender: TObject);
var
  item:   TfpgMenuItem;
  idx:    Integer;
  newPth: TVertexPath;
  cmd:    TVertexCmdSetShapePathRef;
begin
  if (FShape = nil) or (FDocument = nil) then Exit;
  item := TfpgMenuItem(Sender);
  idx  := item.Tag;
  if (idx < 0) or (idx >= FDocument.PathCount) then Exit;
  newPth := FDocument.Paths[idx];
  if FShape.IndexOfPath(newPth) >= 0 then Exit;
  cmd := TVertexCmdSetShapePathRef.Create(FShape, newPth, True);
  FDocument.UndoStack.Execute(cmd);
  UpdatePathList;
end;

procedure TVertexShapePanel.PathDelClick(Sender: TObject);
var
  selIdx: Integer;
  path:   TVertexPath;
  cmd:    TVertexCmdSetShapePathRef;
begin
  if (FShape = nil) or (FDocument = nil) then Exit;
  selIdx := FPathList.FocusItem;
  if (selIdx < 0) or (selIdx >= FShape.PathCount) then Exit;
  if FShape.PathCount <= 1 then
  begin
    ShowMessage('A shape must have at least one path.', 'Vertex');
    Exit;
  end;
  path := FShape.Paths[selIdx];
  cmd  := TVertexCmdSetShapePathRef.Create(FShape, path, False);
  FDocument.UndoStack.Execute(cmd);
  UpdatePathList;
end;

procedure TVertexShapePanel.XOffSpinExit(Sender: TObject);
var
  newX: Single;
  cmd:  TVertexCmdSetShapeTranslation;
begin
  if FUpdating or (FShape = nil) or (FDocument = nil) then Exit;
  newX := FXOffSpin.Value;
  if SameValue(newX, FShape.TranslateX, 0.005) then Exit;
  cmd := TVertexCmdSetShapeTranslation.Create(FShape,
      (Abs(newX) > 0.005) or (Abs(FShape.TranslateY) > 0.005), newX, FShape.TranslateY);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexShapePanel.YOffSpinExit(Sender: TObject);
var
  newY: Single;
  cmd:  TVertexCmdSetShapeTranslation;
begin
  if FUpdating or (FShape = nil) or (FDocument = nil) then Exit;
  newY := FYOffSpin.Value;
  if SameValue(newY, FShape.TranslateY, 0.005) then Exit;
  cmd := TVertexCmdSetShapeTranslation.Create(FShape,
      (Abs(FShape.TranslateX) > 0.005) or (Abs(newY) > 0.005), FShape.TranslateX, newY);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexShapePanel.LODMinSpinExit(Sender: TObject);
var
  newMin: Single;
  newLOD: TVertexLOD;
  cmd:    TVertexCmdSetShapeLOD;
begin
  if FUpdating or (FShape = nil) or (FDocument = nil) then Exit;
  newMin := FLODMinSpin.Value;
  if SameValue(newMin, FShape.LOD.MinSize, 0.05) then Exit;
  newLOD := FShape.LOD;
  newLOD.MinSize := newMin;
  cmd := TVertexCmdSetShapeLOD.Create(FShape, newLOD);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexShapePanel.LODMaxSpinExit(Sender: TObject);
var
  newMax: Single;
  newLOD: TVertexLOD;
  cmd:    TVertexCmdSetShapeLOD;
begin
  if FUpdating or (FShape = nil) or (FDocument = nil) then Exit;
  if FLODMaxSpin.Value = 0 then
    newMax := MaxSingle
  else
    newMax := FLODMaxSpin.Value;
  if SameValue(newMax, FShape.LOD.MaxSize, 0.05) then Exit;
  newLOD := FShape.LOD;
  newLOD.MaxSize := newMax;
  cmd := TVertexCmdSetShapeLOD.Create(FShape, newLOD);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexShapePanel.VisibleChkChanged(Sender: TObject);
var
  cmd: TVertexCmdSetShapeVisible;
begin
  if FUpdating or (FShape = nil) or (FDocument = nil) then Exit;
  if FVisibleChk.Checked = FShape.Visible then Exit;
  cmd := TVertexCmdSetShapeVisible.Create(FShape, FVisibleChk.Checked);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexShapePanel.TransComboChanged(Sender: TObject);
begin
  if FUpdating or (FShape = nil) then Exit;
  CommitTransformer;
  UpdateTransformerControls;
end;

procedure TVertexShapePanel.CommitTransformer;
var
  newTrans: TVertexTransformer;
  cmd:      TVertexCmdSetTransformer;
  joinMap:  array[0..2] of Byte = (0, 2, 3);
begin
  if (FShape = nil) or (FDocument = nil) then Exit;
  newTrans := Default(TVertexTransformer);
  if FTransCombo.FocusItem = 1 then
  begin
    newTrans.TransType  := ittStroke;
    newTrans.Width      := FWidthSpin.Value;
    newTrans.LineCap    := FCapsCombo.FocusItem;
    newTrans.LineJoin   := joinMap[FJoinsCombo.FocusItem];
    newTrans.MiterLimit := FMiterSpin.Value;
  end;
  { Only commit if something changed }
  if (newTrans.TransType  = FShape.Transformer.TransType) and
     (newTrans.Width      = FShape.Transformer.Width)     and
     (newTrans.LineCap    = FShape.Transformer.LineCap)   and
     (newTrans.LineJoin   = FShape.Transformer.LineJoin)  and
     (newTrans.MiterLimit = FShape.Transformer.MiterLimit) then
    Exit;
  cmd := TVertexCmdSetTransformer.Create(FShape, newTrans);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexShapePanel.WidthSpinExit(Sender: TObject);
begin
  if FUpdating or (FShape = nil) then Exit;
  CommitTransformer;
end;

procedure TVertexShapePanel.CapsComboChanged(Sender: TObject);
begin
  if FUpdating or (FShape = nil) then Exit;
  CommitTransformer;
end;

procedure TVertexShapePanel.JoinsComboChanged(Sender: TObject);
begin
  if FUpdating or (FShape = nil) then Exit;
  CommitTransformer;
end;

procedure TVertexShapePanel.MiterSpinExit(Sender: TObject);
begin
  if FUpdating or (FShape = nil) then Exit;
  CommitTransformer;
end;


{ ── Public interface ─────────────────────────────────────────────────────── }

procedure TVertexShapePanel.SetDocument(ADoc: TVertexDocument);
begin
  FDocument := ADoc;
  FShape    := nil;
  UpdateControls;
end;

procedure TVertexShapePanel.SetShape(AShape: TVertexShape);
begin
  FShape := AShape;
  UpdateControls;
end;

procedure TVertexShapePanel.DocumentChanged;
begin
  if FShape <> nil then
    UpdateControls;
end;


end.
