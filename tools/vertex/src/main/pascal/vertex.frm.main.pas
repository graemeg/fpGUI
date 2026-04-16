unit vertex.frm.main;

{
  TVertexMainForm — the top-level editor window.

  Step #5: wire TVertexDocument + interactive editing + undo/redo.
  Step #7: style editing panel with RGBA spin edits + Apply (undo-aware).
  Step #9: File > Save (.hvif) and File > Export as .inc (Pascal const array).

  Layout (MiG docking):
    ┌──────────────────────────────────────────────────┐
    │  File  Edit  Help               [menu bar]       │  DockNorth
    ├─────────┬────────────────────────┬───────────────┤
    │ Tools   │                        │  Objects      │
    │ (64px)  │  Canvas (grows)        │  (220px)      │
    │ DockWest│  GrowX + GrowY         │  (right panel │
    │         │                        │   DockEast)   │
    │         │                        ├───────────────┤
    │         │                        │  Style Panel  │
    │         │                        │  (220px wide) │
    └─────────┴────────────────────────┴───────────────┘
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_constants,
  fpg_menu, fpg_panel, fpg_label, fpg_tree, fpg_button,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc,
  fpg_dialogs,
  fpg_hvif_model,
  fpg_vertex_document,
  vertex.wgt.canvas,
  vertex.wgt.stylepanel,
  vertex.wgt.pathpanel,
  vertex.wgt.shapepanel,
  vertex.wgt.previewbar;


{ Show a Yes / No / Cancel message dialog. Returns mrYes, mrNo, or mrCancel. }
function YesNoCancelDialog(const AMsg, ATitle: string): TfpgModalResult;


type
  TVertexMainForm = class(TfpgForm)
  private
    { Widgets }
    FMenuBar:     TfpgMenuBar;
    FMnuFile:     TfpgPopupMenu;
    FMnuEdit:     TfpgPopupMenu;
    FMnuHelp:     TfpgPopupMenu;
    FToolBox:       TfpgBevel;
    FToolLabel:     TfpgLabel;
    FVertexCanvas:     TVertexCanvasWidget;
    FRightPanel:    TfpgBevel;
    FPreviewBar:    TVertexPreviewBar;
    FObjectTree:    TfpgTreeView;
    FShapeBar:      TfpgBevel;
    FBtnShapeAdd:   TfpgButton;
    FBtnShapeDel:   TfpgButton;
    FBtnShapeUp:    TfpgButton;
    FBtnShapeDown:  TfpgButton;
    FStylePanel:    TVertexStylePanel;
    FPathPanel:     TVertexPathPanel;
    FShapePanel:    TVertexShapePanel;
    FStatusBar:     TfpgLabel;        { bottom status bar }

    { Data }
    FDocument:    TVertexDocument;    { owned }
    FCurrentFile: string;             { '' when unsaved/untitled }
    FHvifByteCount: Integer;          { cached HVIF serialised size, updated on document change }
    FStatusCursorX, FStatusCursorY: Single;  { last known cursor position in HVIF units }
    FShapesNode:  TfpgTreeNode;    { weak ref into the tree; nil when tree is empty }
    FPathsNode:   TfpgTreeNode;    { weak ref into the paths subtree }

    { Setup helpers }
    procedure SetupMenus;
    procedure SetupLayout;

    { Object tree helpers }
    procedure PopulateObjectTree;
    procedure ClearObjectTree;

    { Document change handler — owns FDocument.OnChange; dispatches to canvas + style panel }
    procedure HandleDocumentChange(Sender: TVertexDocument; ACmd: TVertexCommand);

    { Menu handlers }
    procedure miFileNewClick(Sender: TObject);
    procedure miFileOpenClick(Sender: TObject);
    procedure miFileSaveClick(Sender: TObject);
    procedure miFileSaveAsClick(Sender: TObject);
    procedure miFileExportIncClick(Sender: TObject);
    procedure miFileExitClick(Sender: TObject);
    procedure miEditUndoClick(Sender: TObject);
    procedure miEditRedoClick(Sender: TObject);
    procedure miHelpAboutClick(Sender: TObject);

    { Save / export helpers }
    procedure DoSaveToFile(const AFileName: string);
    procedure ExportAsInc(const AFileName: string);
    procedure UpdateTitle;

    { Returns True if it's safe to discard the current document (saved or user OK'd).
      Prompts the user to save if the document is dirty. }
    function PromptSaveIfDirty: Boolean;

    { Replace the current document with a fresh blank one. }
    procedure NewDocument;

    { Close-query handler — blocks close if user cancels on dirty document. }
    procedure FormCloseQuery(Sender: TObject; var ACanClose: Boolean);

    { Canvas cursor-move callback — updates the status bar. }
    procedure CanvasCursorMove(Sender: TObject; AHvifX, AHvifY: Single);

    { Refresh the status bar text from the current document/selection state. }
    procedure UpdateStatusBar;

    { Tree event handler }
    procedure ObjectTreeChanged(Sender: TObject);

    { Shape management handlers }
    procedure ShapeAdd(Sender: TObject);
    procedure ShapeDelete(Sender: TObject);
    procedure ShapeMoveUp(Sender: TObject);
    procedure ShapeMoveDown(Sender: TObject);

    { Select the given shape index in the object tree (after repopulation). }
    procedure SelectShapeInTree(AShapeIdx: Integer);

  public
    procedure AfterCreate; override;
    destructor Destroy; override;
  end;


implementation


{ ── YesNoCancelDialog ────────────────────────────────────────────────────────── }

type
  TYNCDialog = class(TfpgForm)
  private
    FResult:  TfpgModalResult;
    FLabel:   TfpgLabel;
    FBtnYes:  TfpgButton;
    FBtnNo:   TfpgButton;
    FBtnCancel: TfpgButton;
    procedure BtnYesClick(Sender: TObject);
    procedure BtnNoClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
  public
    procedure AfterCreate; override;
    procedure SetMessage(const AMsg: string);
    property  Result: TfpgModalResult read FResult;
  end;

procedure TYNCDialog.AfterCreate;
begin
  inherited AfterCreate;
  WindowTitle  := 'Vertex';
  SetPosition(0, 0, 340, 120);
  WindowPosition := wpScreenCenter;
  Sizeable := False;

  FLabel := TfpgLabel.Create(Self);
  FLabel.SetPosition(12, 12, 316, 52);
  FLabel.WrapText := True;

  FBtnYes := TfpgButton.Create(Self);
  FBtnYes.SetPosition(58, 82, 68, 26);
  FBtnYes.Text    := 'Yes';
  FBtnYes.OnClick := @BtnYesClick;

  FBtnNo := TfpgButton.Create(Self);
  FBtnNo.SetPosition(134, 82, 68, 26);
  FBtnNo.Text    := 'No';
  FBtnNo.OnClick := @BtnNoClick;

  FBtnCancel := TfpgButton.Create(Self);
  FBtnCancel.SetPosition(210, 82, 68, 26);
  FBtnCancel.Text    := 'Cancel';
  FBtnCancel.OnClick := @BtnCancelClick;
end;

procedure TYNCDialog.SetMessage(const AMsg: string);
begin
  FLabel.Text := AMsg;
end;

procedure TYNCDialog.BtnYesClick(Sender: TObject);
begin
  FResult := mrYes;
  Close;
end;

procedure TYNCDialog.BtnNoClick(Sender: TObject);
begin
  FResult := mrNo;
  Close;
end;

procedure TYNCDialog.BtnCancelClick(Sender: TObject);
begin
  FResult := mrCancel;
  Close;
end;

function YesNoCancelDialog(const AMsg, ATitle: string): TfpgModalResult;
var
  dlg: TYNCDialog;
begin
  dlg := TYNCDialog.Create(nil);
  try
    dlg.WindowTitle := ATitle;
    dlg.SetMessage(AMsg);
    dlg.ShowModal;
    Result := dlg.Result;
  finally
    dlg.Free;
  end;
end;


{ ── TVertexMainForm ─────────────────────────────────────────────────────────── }

destructor TVertexMainForm.Destroy;
begin
  FDocument.OnChange := nil;    { clear before widgets are torn down }
  FPreviewBar.SetDocument(nil);
  FStylePanel.SetDocument(nil);
  FPathPanel.SetDocument(nil);
  FShapePanel.SetDocument(nil);
  FVertexCanvas.SetDocument(nil);
  FDocument.Free;
  inherited Destroy;
end;

procedure TVertexMainForm.AfterCreate;
begin
  inherited AfterCreate;
  FDocument   := TVertexDocument.Create;
  WindowTitle := 'Vertex';
  SetPosition(80, 80, 960, 640);
  SetupMenus;
  SetupLayout;
  FDocument.OnChange := @HandleDocumentChange;
  FVertexCanvas.SetDocument(FDocument);
  FStylePanel.SetDocument(FDocument);
  FPathPanel.SetDocument(FDocument);
  FShapePanel.SetDocument(FDocument);
  FPreviewBar.SetDocument(FDocument);
  OnCloseQuery := @FormCloseQuery;
  FVertexCanvas.OnCursorMove := @CanvasCursorMove;
end;

procedure TVertexMainForm.SetupMenus;
begin
  FMnuFile := TfpgPopupMenu.Create(Self);
  with FMnuFile do
  begin
    Name := 'mnuFile';
    AddMenuItem('New',            rsKeyCtrl + 'N',              @miFileNewClick);
    AddMenuItem('Open...',        rsKeyCtrl + 'O',              @miFileOpenClick);
    AddSeparator;
    AddMenuItem('Save',           rsKeyCtrl + 'S',              @miFileSaveClick);
    AddMenuItem('Save As...',     rsKeyCtrl + rsKeyShift + 'S', @miFileSaveAsClick);
    AddSeparator;
    AddMenuItem('Export as .inc...', '', @miFileExportIncClick);
    AddSeparator;
    AddMenuItem('Exit', rsKeyCtrl + 'Q', @miFileExitClick);
  end;

  FMnuEdit := TfpgPopupMenu.Create(Self);
  with FMnuEdit do
  begin
    Name := 'mnuEdit';
    AddMenuItem('Undo', rsKeyCtrl + 'Z', @miEditUndoClick);
    AddMenuItem('Redo', rsKeyCtrl + rsKeyShift + 'Z', @miEditRedoClick);
  end;

  FMnuHelp := TfpgPopupMenu.Create(Self);
  with FMnuHelp do
  begin
    Name := 'mnuHelp';
    AddMenuItem('About...', '', @miHelpAboutClick);
  end;

  FMenuBar := TfpgMenuBar.Create(Self);
  FMenuBar.Name := 'menuBar';
  FMenuBar.AddMenuItem('&File', nil).SubMenu := FMnuFile;
  FMenuBar.AddMenuItem('&Edit', nil).SubMenu := FMnuEdit;
  FMenuBar.AddMenuItem('&Help', nil).SubMenu := FMnuHelp;
end;

procedure TVertexMainForm.SetupLayout;
var
  mig:     TfpgMigLayoutManager;
  rmig:    TfpgMigLayoutManager;
begin
  mig := TfpgMigLayoutManager.Create;
  mig.LC.Fill;
  LayoutManager := mig;

  { Menu bar — spans full width at top }
  mig.AddLayoutComponent(FMenuBar, TfpgMigCC.Create().DockNorth);

  { Left toolbox placeholder — fixed 64px wide }
  FToolBox := TfpgBevel.Create(Self);
  FToolBox.Name  := 'toolBox';
  FToolBox.Style := bsLowered;
  FToolBox.PreferredSize := fpgSize(64, 200);

  FToolLabel := TfpgLabel.Create(FToolBox);
  FToolLabel.Name    := 'lblTools';
  FToolLabel.Text    := 'Tools';
  FToolLabel.SetPosition(4, 4, 56, 20);
  FToolLabel.FontDesc := '#Label1';

  mig.AddLayoutComponent(FToolBox, TfpgMigCC.Create().DockWest);

  { Canvas — centre, takes all remaining space }
  FVertexCanvas := TVertexCanvasWidget.Create(Self);
  FVertexCanvas.Name := 'vertexCanvas';
  FVertexCanvas.PreferredSize := fpgSize(512, 512);
  mig.AddLayoutComponent(FVertexCanvas, TfpgMigCC.Create().GrowX().GrowY());

  { Right panel — contains object tree (top, grows) + style panel (bottom, fixed) }
  FRightPanel := TfpgBevel.Create(Self);
  FRightPanel.Name  := 'rightPanel';
  FRightPanel.Style := bsFlat;
  FRightPanel.PreferredSize := fpgSize(226, 400);

  rmig := TfpgMigLayoutManager.Create;
  rmig.LC.Fill.WrapAfter(1);
  FRightPanel.LayoutManager := rmig;

  FPreviewBar := TVertexPreviewBar.Create(FRightPanel);
  FPreviewBar.Name := 'previewBar';
  rmig.AddLayoutComponent(FPreviewBar, TfpgMigCC.Create().GrowX());

  FObjectTree := TfpgTreeView.Create(FRightPanel);
  FObjectTree.Name     := 'objectTree';
  FObjectTree.PreferredSize := fpgSize(220, 200);
  FObjectTree.OnChange := @ObjectTreeChanged;
  rmig.AddLayoutComponent(FObjectTree, TfpgMigCC.Create().GrowX().GrowY().PushY());

  { Shape management button bar: [+] [-] [Up] [Down] }
  FShapeBar := TfpgBevel.Create(FRightPanel);
  FShapeBar.Name  := 'shapeBar';
  FShapeBar.Style := bsFlat;
  FShapeBar.PreferredSize := fpgSize(220, 26);

  FBtnShapeAdd := TfpgButton.Create(FShapeBar);
  FBtnShapeAdd.Text    := '+';
  FBtnShapeAdd.SetPosition(2, 2, 46, 22);
  FBtnShapeAdd.OnClick := @ShapeAdd;

  FBtnShapeDel := TfpgButton.Create(FShapeBar);
  FBtnShapeDel.Text    := '-';
  FBtnShapeDel.SetPosition(50, 2, 46, 22);
  FBtnShapeDel.OnClick := @ShapeDelete;

  FBtnShapeUp := TfpgButton.Create(FShapeBar);
  FBtnShapeUp.Text    := 'Up';
  FBtnShapeUp.SetPosition(98, 2, 54, 22);
  FBtnShapeUp.OnClick := @ShapeMoveUp;

  FBtnShapeDown := TfpgButton.Create(FShapeBar);
  FBtnShapeDown.Text    := 'Down';
  FBtnShapeDown.SetPosition(154, 2, 54, 22);
  FBtnShapeDown.OnClick := @ShapeMoveDown;

  rmig.AddLayoutComponent(FShapeBar, TfpgMigCC.Create().GrowX());

  FStylePanel := TVertexStylePanel.Create(FRightPanel);
  FStylePanel.Name := 'stylePanel';
  rmig.AddLayoutComponent(FStylePanel, TfpgMigCC.Create().GrowX());

  FPathPanel := TVertexPathPanel.Create(FRightPanel);
  FPathPanel.Name := 'pathPanel';
  rmig.AddLayoutComponent(FPathPanel, TfpgMigCC.Create().GrowX());

  FShapePanel := TVertexShapePanel.Create(FRightPanel);
  FShapePanel.Name := 'shapePanel';
  rmig.AddLayoutComponent(FShapePanel, TfpgMigCC.Create().GrowX());

  mig.AddLayoutComponent(FRightPanel, TfpgMigCC.Create().DockEast);

  { Status bar — fixed height at the bottom }
  FStatusBar := TfpgLabel.Create(Self);
  FStatusBar.Name := 'statusBar';
  FStatusBar.PreferredSize := fpgSize(600, 20);
  FStatusBar.Text := 'Ready';
  FStatusBar.FontDesc := '#Label1';
  mig.AddLayoutComponent(FStatusBar, TfpgMigCC.Create().DockSouth.GrowX());
end;


{ ── Document change handler ──────────────────────────────────────────────── }

procedure TVertexMainForm.HandleDocumentChange(Sender: TVertexDocument;
    ACmd: TVertexCommand);
begin
  FVertexCanvas.DocumentChanged;
  FStylePanel.DocumentChanged;
  FPathPanel.DocumentChanged;
  FShapePanel.DocumentChanged;
  FPreviewBar.DocumentChanged;
  UpdateTitle;
  UpdateStatusBar;
end;


{ ── Object tree ──────────────────────────────────────────────────────────── }

procedure TVertexMainForm.ClearObjectTree;
begin
  FShapesNode := nil;
  FPathsNode  := nil;
  FObjectTree.BeginUpdate;
  FObjectTree.RootNode.Clear;
  FObjectTree.EndUpdate;
end;

procedure TVertexMainForm.PopulateObjectTree;
var
  nStyles, nPaths, nShapes: TfpgTreeNode;
  n: TfpgTreeNode;
  i: Integer;
  st: TVertexStyle;
  ph: TVertexPath;
  sh: TVertexShape;
  s: string;
begin
  if FDocument = nil then
    Exit;

  FObjectTree.BeginUpdate;
  try
    FObjectTree.RootNode.Clear;
    FShapesNode := nil;
    FPathsNode  := nil;

    { ── Styles ── }
    nStyles := FObjectTree.RootNode.AppendText(
        Format('Styles (%d)', [FDocument.StyleCount]));
    nStyles.Expand;

    for i := 0 to FDocument.StyleCount - 1 do
    begin
      st := FDocument.Styles[i];
      case st.StyleType of
        hstSolidColor, hstSolidColorNoAlpha:
          s := Format('Style %d  solid #%2.2X%2.2X%2.2X A=%d',
               [i, st.Color.R, st.Color.G, st.Color.B, st.Color.A]);
        hstSolidGray, hstSolidGrayNoAlpha:
          s := Format('Style %d  gray K=%d', [i, st.Color.R]);
        hstGradient:
          s := Format('Style %d  gradient', [i]);
      else
        s := Format('Style %d', [i]);
      end;
      nStyles.AppendText(s);
    end;

    { ── Paths ── }
    nPaths := FObjectTree.RootNode.AppendText(
        Format('Paths (%d)', [FDocument.PathCount]));
    nPaths.Expand;
    FPathsNode := nPaths;

    for i := 0 to FDocument.PathCount - 1 do
    begin
      ph := FDocument.Paths[i];
      if ph.Closed then
        s := Format('%s  %d pts  closed', [ph.Name, ph.PointCount])
      else
        s := Format('%s  %d pts  open',   [ph.Name, ph.PointCount]);
      n := nPaths.AppendText(s);
      n.Data := Pointer(PtrUInt(i));
    end;

    { ── Shapes ── }
    nShapes := FObjectTree.RootNode.AppendText(
        Format('Shapes (%d)', [FDocument.ShapeCount]));
    nShapes.Expand;
    FShapesNode := nShapes;   { remember for selection tracking }

    for i := 0 to FDocument.ShapeCount - 1 do
    begin
      sh := FDocument.Shapes[i];
      s := Format('Shape %d  style=%d  paths=%d',
           [i, FDocument.IndexOfStyle(sh.Style), sh.PathCount]);
      n := nShapes.AppendText(s);
      n.Data := Pointer(PtrUInt(i));   { store shape index for tree → canvas selection }

      if sh.Transformer.TransType = ittStroke then
        n.AppendText(Format('stroke  w=%.2f', [sh.Transformer.Width]));
      if sh.HasTransform then
        n.AppendText('has transform');
    end;

  finally
    FObjectTree.EndUpdate;
  end;
end;


{ ── Shape management ─────────────────────────────────────────────────────── }

procedure TVertexMainForm.SelectShapeInTree(AShapeIdx: Integer);
var
  n: TfpgTreeNode;
begin
  if (FShapesNode = nil) or (AShapeIdx < 0) then
    Exit;
  n := FShapesNode.FirstSubNode;
  while n <> nil do
  begin
    if Integer(PtrUInt(n.Data)) = AShapeIdx then
    begin
      FObjectTree.Selection := n;
      Exit;
    end;
    n := n.Next;
  end;
end;

procedure TVertexMainForm.ShapeAdd(Sender: TObject);
var
  style:  TVertexStyle;
  path:   TVertexPath;
  shape:  TVertexShape;
  pt:     TVertexPoint;
  col:    THvifColor;
  cmd:    TVertexCmdNewShape;
  newIdx: Integer;
begin
  { New solid-colour style: opaque blue }
  style           := TVertexStyle.Create(FDocument.UniqueName('style'));
  style.StyleType := hstSolidColor;
  col.R := $44; col.G := $88; col.B := $FF; col.A := $FF;
  style.Color := col;

  { New diamond path centred in the 64×64 HVIF coordinate space }
  path        := TVertexPath.Create(FDocument.UniqueName('path'));
  path.Closed := True;
  FillChar(pt, SizeOf(pt), 0);

  pt.X := 32; pt.Y := 24; pt.InX := 32; pt.InY := 24; pt.OutX := 32; pt.OutY := 24;
  path.AddPoint(pt);
  pt.X := 40; pt.Y := 32; pt.InX := 40; pt.InY := 32; pt.OutX := 40; pt.OutY := 32;
  path.AddPoint(pt);
  pt.X := 32; pt.Y := 40; pt.InX := 32; pt.InY := 40; pt.OutX := 32; pt.OutY := 40;
  path.AddPoint(pt);
  pt.X := 24; pt.Y := 32; pt.InX := 24; pt.InY := 32; pt.OutX := 24; pt.OutY := 32;
  path.AddPoint(pt);

  { New shape referencing style and path }
  shape         := TVertexShape.Create(FDocument.UniqueName('shape'));
  shape.Style   := style;
  shape.Visible := True;
  shape.AddPathRef(path);

  cmd := TVertexCmdNewShape.Create(FDocument, style, path, shape);
  FDocument.UndoStack.Execute(cmd);

  newIdx := FDocument.ShapeCount - 1;
  PopulateObjectTree;
  SelectShapeInTree(newIdx);
end;

procedure TVertexMainForm.ShapeDelete(Sender: TObject);
var
  node:   TfpgTreeNode;
  idx:    Integer;
  shape:  TVertexShape;
  cmd:    TVertexCmdDeleteShape;
  newSel: Integer;
begin
  node := FObjectTree.Selection;
  if (node = nil) or (FShapesNode = nil) or (node.Parent <> FShapesNode) then
    Exit;
  idx := Integer(PtrUInt(node.Data));
  if (idx < 0) or (idx >= FDocument.ShapeCount) then
    Exit;

  shape  := FDocument.Shapes[idx];
  newSel := idx - 1;
  if newSel < 0 then
    newSel := 0;
  if FDocument.ShapeCount <= 1 then
    newSel := -1;

  cmd := TVertexCmdDeleteShape.Create(FDocument, shape);
  FDocument.UndoStack.Execute(cmd);

  FVertexCanvas.SelectedShapeIndex := -1;
  FStylePanel.SetStyle(nil);
  FPathPanel.SetPath(nil);
  FShapePanel.SetShape(nil);
  PopulateObjectTree;
  if newSel >= 0 then
    SelectShapeInTree(newSel);
end;

procedure TVertexMainForm.ShapeMoveUp(Sender: TObject);
var
  node: TfpgTreeNode;
  idx:  Integer;
  cmd:  TVertexCmdMoveShape;
begin
  node := FObjectTree.Selection;
  if (node = nil) or (FShapesNode = nil) or (node.Parent <> FShapesNode) then
    Exit;
  idx := Integer(PtrUInt(node.Data));
  if idx <= 0 then
    Exit;

  cmd := TVertexCmdMoveShape.Create(FDocument, idx, idx - 1);
  FDocument.UndoStack.Execute(cmd);

  PopulateObjectTree;
  SelectShapeInTree(idx - 1);
end;

procedure TVertexMainForm.ShapeMoveDown(Sender: TObject);
var
  node: TfpgTreeNode;
  idx:  Integer;
  cmd:  TVertexCmdMoveShape;
begin
  node := FObjectTree.Selection;
  if (node = nil) or (FShapesNode = nil) or (node.Parent <> FShapesNode) then
    Exit;
  idx := Integer(PtrUInt(node.Data));
  if idx >= FDocument.ShapeCount - 1 then
    Exit;

  cmd := TVertexCmdMoveShape.Create(FDocument, idx, idx + 1);
  FDocument.UndoStack.Execute(cmd);

  PopulateObjectTree;
  SelectShapeInTree(idx + 1);
end;


{ ── Tree event handler ───────────────────────────────────────────────────── }

procedure TVertexMainForm.ObjectTreeChanged(Sender: TObject);
var
  node: TfpgTreeNode;
  idx:  Integer;
begin
  node := FObjectTree.Selection;
  if node = nil then
    Exit;

  { Path selected }
  if (FPathsNode <> nil) and (node.Parent = FPathsNode) then
  begin
    idx := Integer(PtrUInt(node.Data));
    FPathPanel.SetPath(FDocument.Paths[idx]);
    FVertexCanvas.SelectedShapeIndex := -1;
    FStylePanel.SetStyle(nil);
    FShapePanel.SetShape(nil);
    UpdateStatusBar;
    Exit;
  end;

  { Shape selected }
  if (FShapesNode <> nil) and (node.Parent = FShapesNode) then
  begin
    idx := Integer(PtrUInt(node.Data));
    FVertexCanvas.SelectedShapeIndex := idx;
    FStylePanel.SetStyle(FDocument.Shapes[idx].Style);
    FPathPanel.SetPath(nil);
    FShapePanel.SetShape(FDocument.Shapes[idx]);
    UpdateStatusBar;
    Exit;
  end;

  { Any other node — clear all selections }
  FVertexCanvas.SelectedShapeIndex := -1;
  FStylePanel.SetStyle(nil);
  FPathPanel.SetPath(nil);
  FShapePanel.SetShape(nil);
  UpdateStatusBar;
end;


{ ── Document helpers ─────────────────────────────────────────────────────── }

function TVertexMainForm.PromptSaveIfDirty: Boolean;
var
  res: TfpgModalResult;
begin
  Result := True;
  if not FDocument.Dirty then
    Exit;
  res := YesNoCancelDialog('The document has unsaved changes.' + LineEnding +
      'Do you want to save before continuing?', 'Vertex');
  case res of
    mrYes:
      begin
        { Inline save — mirror miFileSaveClick logic }
        if FCurrentFile <> '' then
          DoSaveToFile(FCurrentFile)
        else
        begin
          FCurrentFile := SelectFileDialog(sfdSave,
              'HVIF files (*.hvif)|*.hvif|All files (*)|*', '');
          if FCurrentFile = '' then
          begin
            Result := False;   { user cancelled the save dialog }
            Exit;
          end;
          if ExtractFileExt(FCurrentFile) = '' then
            FCurrentFile := FCurrentFile + '.hvif';
          DoSaveToFile(FCurrentFile);
        end;
      end;
    mrNo:
      { Discard changes — proceed without saving }
      ;
    mrCancel:
      Result := False;
  end;
end;

procedure TVertexMainForm.NewDocument;
var
  newDoc: TVertexDocument;
begin
  newDoc := TVertexDocument.Create;
  { Disconnect all widgets from the old document }
  FDocument.OnChange := nil;
  FPreviewBar.SetDocument(nil);
  FStylePanel.SetDocument(nil);
  FPathPanel.SetDocument(nil);
  FShapePanel.SetDocument(nil);
  FVertexCanvas.SetDocument(nil);
  FDocument.Free;

  FDocument    := newDoc;
  FCurrentFile := '';

  { Connect all widgets to the new document }
  FDocument.OnChange := @HandleDocumentChange;
  FVertexCanvas.SetDocument(FDocument);
  FStylePanel.SetDocument(FDocument);
  FPathPanel.SetDocument(FDocument);
  FShapePanel.SetDocument(FDocument);
  FPreviewBar.SetDocument(FDocument);

  FVertexCanvas.OnCursorMove := @CanvasCursorMove;
  FVertexCanvas.SelectedShapeIndex := -1;
  ClearObjectTree;
  UpdateTitle;
  UpdateStatusBar;
end;

procedure TVertexMainForm.FormCloseQuery(Sender: TObject; var ACanClose: Boolean);
begin
  ACanClose := PromptSaveIfDirty;
end;


{ ── Menu handlers ────────────────────────────────────────────────────────── }

procedure TVertexMainForm.miFileNewClick(Sender: TObject);
begin
  if not PromptSaveIfDirty then
    Exit;
  NewDocument;
end;

procedure TVertexMainForm.miFileOpenClick(Sender: TObject);
var
  fn: string;
begin
  fn := SelectFileDialog(sfdOpen, 'HVIF files (*.hvif)|*.hvif|All files (*)|*', '');
  if fn = '' then
    Exit;

  try
    FDocument.LoadFromFile(fn);
  except
    on E: Exception do
    begin
      ShowMessage('Open failed: ' + E.Message, 'Vertex');
      Exit;
    end;
  end;

  { Notify canvas and preview bar of the load; clear style panel selection }
  FCurrentFile := fn;
  FVertexCanvas.DocumentChanged;
  FPreviewBar.DocumentChanged;
  FVertexCanvas.SelectedShapeIndex := -1;
  FStylePanel.SetStyle(nil);
  FPathPanel.SetPath(nil);
  FShapePanel.SetShape(nil);
  PopulateObjectTree;
  UpdateTitle;
end;

procedure TVertexMainForm.miFileSaveClick(Sender: TObject);
var
  fn: string;
begin
  if FCurrentFile <> '' then
  begin
    DoSaveToFile(FCurrentFile);
    Exit;
  end;
  { No current file — fall through to Save As }
  fn := SelectFileDialog(sfdSave, 'HVIF files (*.hvif)|*.hvif|All files (*)|*', '');
  if fn = '' then
    Exit;
  if ExtractFileExt(fn) = '' then
    fn := fn + '.hvif';
  DoSaveToFile(fn);
  FCurrentFile := fn;
  UpdateTitle;
end;

procedure TVertexMainForm.miFileSaveAsClick(Sender: TObject);
var
  fn:     string;
  initDir: string;
begin
  if FCurrentFile <> '' then
    initDir := ExtractFilePath(FCurrentFile)
  else
    initDir := '';
  fn := SelectFileDialog(sfdSave, 'HVIF files (*.hvif)|*.hvif|All files (*)|*', initDir);
  if fn = '' then
    Exit;
  if ExtractFileExt(fn) = '' then
    fn := fn + '.hvif';
  DoSaveToFile(fn);
  FCurrentFile := fn;
  UpdateTitle;
end;

procedure TVertexMainForm.miFileExportIncClick(Sender: TObject);
var
  fn:      string;
  initDir: string;
begin
  if FCurrentFile <> '' then
    initDir := ExtractFilePath(FCurrentFile)
  else
    initDir := '';
  fn := SelectFileDialog(sfdSave,
      'Pascal include files (*.inc)|*.inc|All files (*)|*', initDir);
  if fn = '' then
    Exit;
  if ExtractFileExt(fn) = '' then
    fn := fn + '.inc';
  ExportAsInc(fn);
end;

procedure TVertexMainForm.miFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TVertexMainForm.miEditUndoClick(Sender: TObject);
begin
  if FDocument.UndoStack.CanUndo then
  begin
    FDocument.UndoStack.Undo;
    { Tree structure may have changed (e.g. undo of AddShape) — repopulate. }
    FVertexCanvas.SelectedShapeIndex := -1;
    FStylePanel.SetStyle(nil);
  FPathPanel.SetPath(nil);
  FShapePanel.SetShape(nil);
    PopulateObjectTree;
  end;
end;

procedure TVertexMainForm.miEditRedoClick(Sender: TObject);
begin
  if FDocument.UndoStack.CanRedo then
  begin
    FDocument.UndoStack.Redo;
    FVertexCanvas.SelectedShapeIndex := -1;
    FStylePanel.SetStyle(nil);
  FPathPanel.SetPath(nil);
  FShapePanel.SetShape(nil);
    PopulateObjectTree;
  end;
end;

procedure TVertexMainForm.DoSaveToFile(const AFileName: string);
begin
  try
    FDocument.SaveToFile(AFileName);   { sets Dirty=False internally }
    UpdateTitle;
  except
    on E: Exception do
      ShowMessage('Save failed: ' + E.Message, 'Vertex');
  end;
end;

procedure TVertexMainForm.ExportAsInc(const AFileName: string);
const
  kBytesPerRow = 16;
var
  ms:       TMemoryStream;
  buf:      PByte;
  total, i: Integer;
  constName: string;
  row:      string;
  sl:       TStringList;
begin
  { Derive a valid Pascal identifier from the filename }
  constName := ExtractFileName(ChangeFileExt(AFileName, ''));
  if (constName = '') or not (constName[1] in ['A'..'Z', 'a'..'z', '_']) then
    constName := 'kIcon'
  else
    constName := 'k' + UpperCase(constName[1]) + Copy(constName, 2, MaxInt);

  ms := TMemoryStream.Create;
  sl := TStringList.Create;
  try
    FDocument.SaveToStream(ms);
    total := ms.Size;
    buf   := PByte(ms.Memory);

    sl.Add('{ Generated by Vertex — HVIF icon data }');
    sl.Add('const');
    sl.Add(Format('  %s: array[0..%d] of Byte = (', [constName, total - 1]));

    i := 0;
    while i < total do
    begin
      row := '    ';
      while i < total do
      begin
        row := row + Format('$%2.2X', [buf[i]]);
        Inc(i);
        if i >= total then
          Break;           { last byte — no comma }
        row := row + ', ';
        if (i mod kBytesPerRow) = 0 then
          Break;           { end of this row }
      end;
      sl.Add(row);
    end;

    sl.Add('  );');

    try
      sl.SaveToFile(AFileName);
    except
      on E: Exception do
      begin
        ShowMessage('Export failed: ' + E.Message, 'Vertex');
        Exit;
      end;
    end;
  finally
    ms.Free;
    sl.Free;
  end;
end;

procedure TVertexMainForm.UpdateStatusBar;
var
  ms:      TMemoryStream;
  selName: string;
  nodeInfo: string;
  node:    TfpgTreeNode;
  idx:     Integer;
begin
  if FDocument = nil then
  begin
    FStatusBar.Text := 'Ready';
    Exit;
  end;

  { Refresh cached HVIF byte count }
  ms := TMemoryStream.Create;
  try
    FDocument.SaveToStream(ms);
    FHvifByteCount := ms.Size;
  finally
    ms.Free;
  end;

  { Selected item name and node info }
  selName  := '';
  nodeInfo := '';
  node := FObjectTree.Selection;
  if node <> nil then
  begin
    if (FPathsNode <> nil) and (node.Parent = FPathsNode) then
    begin
      idx := Integer(PtrUInt(node.Data));
      if (idx >= 0) and (idx < FDocument.PathCount) then
      begin
        selName  := FDocument.Paths[idx].Name;
        nodeInfo := Format('nodes: %d', [FDocument.Paths[idx].PointCount]);
      end;
    end
    else if (FShapesNode <> nil) and (node.Parent = FShapesNode) then
    begin
      idx := Integer(PtrUInt(node.Data));
      if (idx >= 0) and (idx < FDocument.ShapeCount) then
        selName := FDocument.Shapes[idx].Name;
    end;
  end;

  if selName <> '' then
    FStatusBar.Text := Format('X: %.1f  Y: %.1f  |  %s  |  %s  |  %d bytes HVIF',
        [FStatusCursorX, FStatusCursorY, selName, nodeInfo, FHvifByteCount])
  else
    FStatusBar.Text := Format('X: %.1f  Y: %.1f  |  %d bytes HVIF',
        [FStatusCursorX, FStatusCursorY, FHvifByteCount]);
end;

procedure TVertexMainForm.CanvasCursorMove(Sender: TObject; AHvifX, AHvifY: Single);
var
  selName:  string;
  nodeInfo: string;
  node:     TfpgTreeNode;
  idx:      Integer;
begin
  FStatusCursorX := AHvifX;
  FStatusCursorY := AHvifY;

  selName  := '';
  nodeInfo := '';
  node := FObjectTree.Selection;
  if node <> nil then
  begin
    if (FPathsNode <> nil) and (node.Parent = FPathsNode) then
    begin
      idx := Integer(PtrUInt(node.Data));
      if (idx >= 0) and (idx < FDocument.PathCount) then
      begin
        selName  := FDocument.Paths[idx].Name;
        nodeInfo := Format('nodes: %d', [FDocument.Paths[idx].PointCount]);
      end;
    end
    else if (FShapesNode <> nil) and (node.Parent = FShapesNode) then
    begin
      idx := Integer(PtrUInt(node.Data));
      if (idx >= 0) and (idx < FDocument.ShapeCount) then
        selName := FDocument.Shapes[idx].Name;
    end;
  end;

  if selName <> '' then
    FStatusBar.Text := Format('X: %.1f  Y: %.1f  |  %s  |  %s  |  %d bytes HVIF',
        [AHvifX, AHvifY, selName, nodeInfo, FHvifByteCount])
  else
    FStatusBar.Text := Format('X: %.1f  Y: %.1f  |  %d bytes HVIF',
        [AHvifX, AHvifY, FHvifByteCount]);
end;

procedure TVertexMainForm.UpdateTitle;
var
  base: string;
begin
  if FCurrentFile <> '' then
    base := 'Vertex — ' + ExtractFileName(FCurrentFile)
  else
    base := 'Vertex';
  if FDocument.Dirty then
    WindowTitle := base + ' *'
  else
    WindowTitle := base;
end;

procedure TVertexMainForm.miHelpAboutClick(Sender: TObject);
begin
  ShowMessage(
      'Vertex' + LineEnding +
      'HVIF icon editor for the fpGUI toolkit.' + LineEnding + LineEnding +
      'Step 9: Save (.hvif) and Export as .inc',
      'About Vertex');
end;

end.
