unit iom.frm.main;

{
  TIomMainForm — the top-level editor window.

  Step #5: wire TIomDocument + interactive editing + undo/redo.
  Step #7: style editing panel with RGBA spin edits + Apply (undo-aware).

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
  fpg_menu, fpg_panel, fpg_label, fpg_tree,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc,
  fpg_dialogs,
  fpg_hvif_model,
  fpg_iom_document,
  iom.wgt.canvas,
  iom.wgt.stylepanel;


type
  TIomMainForm = class(TfpgForm)
  private
    { Widgets }
    FMenuBar:     TfpgMenuBar;
    FMnuFile:     TfpgPopupMenu;
    FMnuEdit:     TfpgPopupMenu;
    FMnuHelp:     TfpgPopupMenu;
    FToolBox:     TfpgBevel;
    FToolLabel:   TfpgLabel;
    FIomCanvas:   TIomCanvasWidget;
    FRightPanel:  TfpgBevel;
    FObjectTree:  TfpgTreeView;
    FStylePanel:  TIomStylePanel;

    { Data }
    FDocument:   TIomDocument;    { owned }
    FShapesNode: TfpgTreeNode;    { weak ref into the tree; nil when tree is empty }

    { Setup helpers }
    procedure SetupMenus;
    procedure SetupLayout;

    { Object tree helpers }
    procedure PopulateObjectTree;
    procedure ClearObjectTree;

    { Document change handler — owns FDocument.OnChange; dispatches to canvas + style panel }
    procedure HandleDocumentChange(Sender: TIomDocument; ACmd: TIomCommand);

    { Menu handlers }
    procedure miFileOpenClick(Sender: TObject);
    procedure miFileExitClick(Sender: TObject);
    procedure miEditUndoClick(Sender: TObject);
    procedure miEditRedoClick(Sender: TObject);
    procedure miHelpAboutClick(Sender: TObject);

    { Tree event handler }
    procedure ObjectTreeChanged(Sender: TObject);

  public
    procedure AfterCreate; override;
    destructor Destroy; override;
  end;


implementation


{ ── TIomMainForm ─────────────────────────────────────────────────────────── }

destructor TIomMainForm.Destroy;
begin
  FDocument.OnChange := nil;    { clear before widgets are torn down }
  FStylePanel.SetDocument(nil);
  FIomCanvas.SetDocument(nil);
  FDocument.Free;
  inherited Destroy;
end;

procedure TIomMainForm.AfterCreate;
begin
  inherited AfterCreate;
  FDocument   := TIomDocument.Create;
  WindowTitle := 'Icon-O-Matic';
  SetPosition(80, 80, 960, 640);
  SetupMenus;
  SetupLayout;
  FDocument.OnChange := @HandleDocumentChange;
  FIomCanvas.SetDocument(FDocument);
  FStylePanel.SetDocument(FDocument);
end;

procedure TIomMainForm.SetupMenus;
begin
  FMnuFile := TfpgPopupMenu.Create(Self);
  with FMnuFile do
  begin
    Name := 'mnuFile';
    AddMenuItem('Open...', rsKeyCtrl + 'O', @miFileOpenClick);
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

procedure TIomMainForm.SetupLayout;
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
  FIomCanvas := TIomCanvasWidget.Create(Self);
  FIomCanvas.Name := 'iomCanvas';
  FIomCanvas.PreferredSize := fpgSize(512, 512);
  mig.AddLayoutComponent(FIomCanvas, TfpgMigCC.Create().GrowX().GrowY());

  { Right panel — contains object tree (top, grows) + style panel (bottom, fixed) }
  FRightPanel := TfpgBevel.Create(Self);
  FRightPanel.Name  := 'rightPanel';
  FRightPanel.Style := bsFlat;
  FRightPanel.PreferredSize := fpgSize(226, 400);

  rmig := TfpgMigLayoutManager.Create;
  rmig.LC.Fill.WrapAfter(1);
  FRightPanel.LayoutManager := rmig;

  FObjectTree := TfpgTreeView.Create(FRightPanel);
  FObjectTree.Name     := 'objectTree';
  FObjectTree.PreferredSize := fpgSize(220, 200);
  FObjectTree.OnChange := @ObjectTreeChanged;
  rmig.AddLayoutComponent(FObjectTree, TfpgMigCC.Create().GrowX().GrowY());

  FStylePanel := TIomStylePanel.Create(FRightPanel);
  FStylePanel.Name := 'stylePanel';
  rmig.AddLayoutComponent(FStylePanel, TfpgMigCC.Create().GrowX());

  mig.AddLayoutComponent(FRightPanel, TfpgMigCC.Create().DockEast);
end;


{ ── Document change handler ──────────────────────────────────────────────── }

procedure TIomMainForm.HandleDocumentChange(Sender: TIomDocument;
    ACmd: TIomCommand);
begin
  FIomCanvas.DocumentChanged;
  FStylePanel.DocumentChanged;
end;


{ ── Object tree ──────────────────────────────────────────────────────────── }

procedure TIomMainForm.ClearObjectTree;
begin
  FShapesNode := nil;
  FObjectTree.BeginUpdate;
  FObjectTree.RootNode.Clear;
  FObjectTree.EndUpdate;
end;

procedure TIomMainForm.PopulateObjectTree;
var
  nStyles, nPaths, nShapes: TfpgTreeNode;
  n: TfpgTreeNode;
  i: Integer;
  st: TIomStyle;
  ph: TIomPath;
  sh: TIomShape;
  s: string;
begin
  if FDocument = nil then
    Exit;

  FObjectTree.BeginUpdate;
  try
    FObjectTree.RootNode.Clear;
    FShapesNode := nil;

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

    for i := 0 to FDocument.PathCount - 1 do
    begin
      ph := FDocument.Paths[i];
      if ph.Closed then
        s := Format('Path %d  %d pts  closed', [i, ph.PointCount])
      else
        s := Format('Path %d  %d pts  open',   [i, ph.PointCount]);
      nPaths.AppendText(s);
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


{ ── Tree event handler ───────────────────────────────────────────────────── }

procedure TIomMainForm.ObjectTreeChanged(Sender: TObject);
var
  node: TfpgTreeNode;
  idx: Integer;
begin
  node := FObjectTree.Selection;
  if (node = nil) or (FShapesNode = nil) or (node.Parent <> FShapesNode) then
    Exit;
  idx := Integer(PtrUInt(node.Data));
  FIomCanvas.SelectedShapeIndex := idx;
  FStylePanel.SetStyle(FDocument.Shapes[idx].Style);
end;


{ ── Menu handlers ────────────────────────────────────────────────────────── }

procedure TIomMainForm.miFileOpenClick(Sender: TObject);
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
      ShowMessage('Open failed: ' + E.Message, 'Icon-O-Matic');
      Exit;
    end;
  end;

  { Notify canvas of the load; clear style panel selection }
  FIomCanvas.DocumentChanged;
  FIomCanvas.SelectedShapeIndex := -1;
  FStylePanel.SetStyle(nil);
  PopulateObjectTree;
  WindowTitle := 'Icon-O-Matic — ' + ExtractFileName(fn);
end;

procedure TIomMainForm.miFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TIomMainForm.miEditUndoClick(Sender: TObject);
begin
  if FDocument.UndoStack.CanUndo then
    FDocument.UndoStack.Undo;
  { UndoStack.Undo fires OnChange → canvas HandleDocumentChange → Repaint }
end;

procedure TIomMainForm.miEditRedoClick(Sender: TObject);
begin
  if FDocument.UndoStack.CanRedo then
    FDocument.UndoStack.Redo;
end;

procedure TIomMainForm.miHelpAboutClick(Sender: TObject);
begin
  ShowMessage(
      'fpGUI Icon-O-Matic' + LineEnding +
      'HVIF icon editor for the fpGUI toolkit.' + LineEnding + LineEnding +
      'Step 7: Style editing with undo/redo',
      'About Icon-O-Matic');
end;

end.
