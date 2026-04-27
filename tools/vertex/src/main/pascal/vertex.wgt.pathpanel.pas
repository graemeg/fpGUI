unit vertex.wgt.pathpanel;

{
  TVertexPathPanel — properties panel for the currently selected path.

  Controls:
    FNameEdit    — editable path name; committed on focus-loss via TVertexCmdRename.
    FClosedChk   — checkbox for Path.Closed; committed on click via TVertexCmdSetPathClosed.
    FBtnReverse  — reverses point order via TVertexCmdReversePath.
    FBtnRotFwd   — rotates indices forward by one via TVertexCmdRotatePathIndices(+1).
    FBtnRotBwd   — rotates indices backward by one via TVertexCmdRotatePathIndices(-1).

  SetPath(nil) clears and disables all controls.
  DocumentChanged refreshes controls from the current path after an undo/redo.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_widget, fpg_panel,
  fpg_label, fpg_edit, fpg_checkbox, fpg_button,
  fpg_vertex_document;


type
  TVertexPathPanel = class(TfpgBevel)
  private
    FDocument: TVertexDocument;   { not owned }
    FPath:     TVertexPath;       { current path, not owned; nil = no selection }
    FUpdating: Boolean;

    FLblHeader:  TfpgLabel;
    FLblName:    TfpgLabel;
    FNameEdit:   TfpgEdit;
    FClosedChk:  TfpgCheckBox;
    FBtnReverse: TfpgButton;
    FBtnRotFwd:  TfpgButton;
    FBtnRotBwd:  TfpgButton;

    procedure SetupControls;
    procedure UpdateControls;
    procedure SetControlsEnabled(AEnabled: Boolean);

    procedure NameEditExit(Sender: TObject);
    procedure ClosedChkChanged(Sender: TObject);
    procedure BtnReverseClick(Sender: TObject);
    procedure BtnRotFwdClick(Sender: TObject);
    procedure BtnRotBwdClick(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;

    procedure SetDocument(ADoc: TVertexDocument);
    procedure SetPath(APath: TVertexPath);
    procedure DocumentChanged;
  end;


implementation


const
  LBL_X    = 4;
  CTL_X    = 60;
  CTL_W    = 152;
  ROW_H    = 24;
  ROW0     = 4;
  ROW1     = ROW0 + 20;
  ROW2     = ROW1 + ROW_H;
  ROW3     = ROW2 + ROW_H;
  ROW4     = ROW3 + ROW_H + 4;
  PANEL_H  = ROW4 + ROW_H + 6;


constructor TVertexPathPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument := nil;
  FPath     := nil;
  FUpdating := False;
  PreferredSize := fpgSize(220, PANEL_H);
  SetupControls;
  UpdateControls;
end;

procedure TVertexPathPanel.SetupControls;
begin
  FLblHeader := TfpgLabel.Create(Self);
  FLblHeader.SetPosition(LBL_X, ROW0, 210, 18);
  FLblHeader.Text     := 'Path Properties';
  FLblHeader.FontDesc := '#Label1:bold';

  FLblName := TfpgLabel.Create(Self);
  FLblName.SetPosition(LBL_X, ROW1 + 4, CTL_X - 6, 18);
  FLblName.Text := 'Name:';

  FNameEdit := TfpgEdit.Create(Self);
  FNameEdit.SetPosition(CTL_X, ROW1, CTL_W, ROW_H);
  FNameEdit.OnExit := @NameEditExit;

  FClosedChk := TfpgCheckBox.Create(Self);
  FClosedChk.SetPosition(LBL_X, ROW2, 210, ROW_H);
  FClosedChk.Text     := 'Closed';
  FClosedChk.OnChange := @ClosedChkChanged;

  FBtnReverse := TfpgButton.Create(Self);
  FBtnReverse.SetPosition(LBL_X, ROW3, 210, ROW_H);
  FBtnReverse.Text    := 'Reverse';
  FBtnReverse.OnClick := @BtnReverseClick;

  FBtnRotFwd := TfpgButton.Create(Self);
  FBtnRotFwd.SetPosition(LBL_X, ROW4, 100, ROW_H);
  FBtnRotFwd.Text    := 'Rotate >>';
  FBtnRotFwd.OnClick := @BtnRotFwdClick;

  FBtnRotBwd := TfpgButton.Create(Self);
  FBtnRotBwd.SetPosition(LBL_X + 106, ROW4, 100, ROW_H);
  FBtnRotBwd.Text    := '<< Rotate';
  FBtnRotBwd.OnClick := @BtnRotBwdClick;
end;

procedure TVertexPathPanel.SetControlsEnabled(AEnabled: Boolean);
begin
  FNameEdit.Enabled   := AEnabled;
  FClosedChk.Enabled  := AEnabled;
  FBtnReverse.Enabled := AEnabled;
  FBtnRotFwd.Enabled  := AEnabled;
  FBtnRotBwd.Enabled  := AEnabled;
end;

procedure TVertexPathPanel.UpdateControls;
begin
  if FPath = nil then
  begin
    FUpdating := True;
    try
      FNameEdit.Text     := '';
      FClosedChk.Checked := False;
    finally
      FUpdating := False;
    end;
    SetControlsEnabled(False);
    Exit;
  end;

  FUpdating := True;
  try
    FNameEdit.Text     := FPath.Name;
    FClosedChk.Checked := FPath.Closed;
  finally
    FUpdating := False;
  end;
  SetControlsEnabled(True);
end;


{ ── Event handlers ───────────────────────────────────────────────────────── }

procedure TVertexPathPanel.NameEditExit(Sender: TObject);
var
  newName: string;
  cmd:     TVertexCmdRename;
begin
  if FUpdating or (FPath = nil) or (FDocument = nil) then Exit;
  newName := Trim(FNameEdit.Text);
  if (newName = '') or (newName = FPath.Name) then
  begin
    FNameEdit.Text := FPath.Name;   { revert empty }
    Exit;
  end;
  if FDocument.NameExists(newName) then
  begin
    FNameEdit.Text := FPath.Name;
    Exit;
  end;
  cmd := TVertexCmdRename.Create(FPath.NamePtr, FPath.Name, newName);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexPathPanel.ClosedChkChanged(Sender: TObject);
var
  cmd: TVertexCmdSetPathClosed;
begin
  if FUpdating or (FPath = nil) or (FDocument = nil) then Exit;
  if FClosedChk.Checked = FPath.Closed then Exit;
  cmd := TVertexCmdSetPathClosed.Create(FPath, FClosedChk.Checked);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexPathPanel.BtnReverseClick(Sender: TObject);
var
  cmd: TVertexCmdReversePath;
begin
  if (FPath = nil) or (FDocument = nil) then Exit;
  cmd := TVertexCmdReversePath.Create(FPath);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexPathPanel.BtnRotFwdClick(Sender: TObject);
var
  cmd: TVertexCmdRotatePathIndices;
begin
  if (FPath = nil) or (FDocument = nil) then Exit;
  cmd := TVertexCmdRotatePathIndices.Create(FPath, 1);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexPathPanel.BtnRotBwdClick(Sender: TObject);
var
  cmd: TVertexCmdRotatePathIndices;
begin
  if (FPath = nil) or (FDocument = nil) then Exit;
  cmd := TVertexCmdRotatePathIndices.Create(FPath, -1);
  FDocument.UndoStack.Execute(cmd);
end;


{ ── Public interface ─────────────────────────────────────────────────────── }

procedure TVertexPathPanel.SetDocument(ADoc: TVertexDocument);
begin
  FDocument := ADoc;
  FPath     := nil;
  UpdateControls;
end;

procedure TVertexPathPanel.SetPath(APath: TVertexPath);
begin
  FPath := APath;
  UpdateControls;
end;

procedure TVertexPathPanel.DocumentChanged;
begin
  if FPath <> nil then
    UpdateControls;
end;


end.
