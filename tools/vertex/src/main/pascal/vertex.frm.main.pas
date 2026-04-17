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
  FPImage, FPWritePNG,
  fpg_base, fpg_main, fpg_form, fpg_constants,
  fpg_menu, fpg_panel, fpg_label, fpg_tree, fpg_button,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc,
  fpg_dialogs, fpg_iniutils, fpg_mru,
  fpg_hvif_model, fpg_hvif,
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
    FBtnToolSelect: TfpgButton;   { Select / move shapes tool }
    FBtnToolNode:   TfpgButton;   { Node edit tool (default) }
    FBtnToolPan:    TfpgButton;   { Pan viewport tool }
    FBtnToolAddPt:  TfpgButton;   { Add point tool }
    FBtnToolDelPt:  TfpgButton;   { Delete point tool }
    FBtnToolZoom:   TfpgButton;   { Zoom tool }
    FVertexCanvas:     TVertexCanvasWidget;
    FRightPanel:    TfpgBevel;
    FPreviewBar:    TVertexPreviewBar;
    FObjectTree:    TfpgTreeView;
    FShapeBar:      TfpgBevel;
    FBtnShapeAdd:   TfpgButton;
    FBtnShapeDel:   TfpgButton;
    FBtnShapeUp:    TfpgButton;
    FBtnShapeDown:  TfpgButton;
    FPathBar:       TfpgBevel;       { path management button bar }
    FBtnPathAdd:    TfpgButton;
    FBtnPathDel:    TfpgButton;
    FStylePanel:    TVertexStylePanel;
    FPathPanel:     TVertexPathPanel;
    FShapePanel:    TVertexShapePanel;
    FStatusBar:     TfpgLabel;        { bottom status bar }
    FZoomBar:       TfpgBevel;        { zoom toolbar strip above canvas }
    FZoomLabel:     TfpgLabel;        { shows current zoom % }
    FBtnZoom50:     TfpgButton;
    FBtnZoom100:    TfpgButton;
    FBtnZoom200:    TfpgButton;
    FBtnZoom400:    TfpgButton;
    FBtnZoomFit:    TfpgButton;
    FMnuPath:       TfpgPopupMenu;    { right-click on a path node }
    FMnuShape:      TfpgPopupMenu;    { right-click on a shape node }
    FBtnGrid:       TfpgButton;       { toggle grid overlay }
    FBtnSnap:       TfpgButton;       { toggle snap-to-grid }
    FStyleBar:      TfpgBevel;        { style management button bar }
    FBtnStyleAdd:   TfpgButton;
    FBtnStyleDel:   TfpgButton;

    { Data }
    FDocument:    TVertexDocument;    { owned }
    FCurrentFile: string;             { '' when unsaved/untitled }
    FLastOpenDir: string;             { last directory used in the Open dialog }
    FRecentFiles: TfpgMRU;            { MRU list — persisted to gINI }
    FMnuOpenRecent: TfpgPopupMenu;    { submenu populated by TfpgMRU }
    FHvifByteCount: Integer;          { cached HVIF serialised size, updated on document change }
    FStatusCursorX, FStatusCursorY: Single;  { last known cursor position in HVIF units }
    FStylesNode:  TfpgTreeNode;    { weak ref into the styles subtree }
    FShapesNode:  TfpgTreeNode;    { weak ref into the tree; nil when tree is empty }
    FPathsNode:   TfpgTreeNode;    { weak ref into the paths subtree }

    { Setup helpers }
    procedure SetupMenus;
    procedure SetupLayout;
    procedure SetupContextMenus;

    { Object tree helpers }
    procedure PopulateObjectTree;
    procedure ClearObjectTree;

    { Document change handler — owns FDocument.OnChange; dispatches to canvas + style panel }
    procedure HandleDocumentChange(Sender: TVertexDocument; ACmd: TVertexCommand);

    { Menu handlers }
    procedure miFileNewClick(Sender: TObject);
    procedure miFileOpenClick(Sender: TObject);
    procedure miRecentFileClick(Sender: TObject; const FileName: string);
    procedure miFileSaveClick(Sender: TObject);
    procedure miFileSaveAsClick(Sender: TObject);
    procedure miFileExportIncClick(Sender: TObject);
    procedure miFileExportPngClick(Sender: TObject);
    procedure miFileExitClick(Sender: TObject);
    procedure miEditUndoClick(Sender: TObject);
    procedure miEditRedoClick(Sender: TObject);
    procedure miHelpAboutClick(Sender: TObject);

    { Save / export helpers }
    procedure DoOpenFile(const AFileName: string);
    procedure DoSaveToFile(const AFileName: string);
    procedure ExportAsInc(const AFileName: string);
    procedure ExportAsPng(const ABasePath: string);
    procedure UpdateTitle;

    { Returns True if it's safe to discard the current document (saved or user OK'd).
      Prompts the user to save if the document is dirty. }
    function PromptSaveIfDirty: Boolean;

    { Replace the current document with a fresh blank one. }
    procedure NewDocument;

    { Close-query handler — blocks close if user cancels on dirty document. }
    procedure FormCloseQuery(Sender: TObject; var ACanClose: Boolean);

    { Set canvas tool mode and sync toolbar button bold state. }
    procedure ApplyToolMode(AMode: TVertexToolMode);

    { Tool button handlers }
    procedure ToolBtnClick(Sender: TObject);

    { Zoom button handlers }
    procedure ZoomBtnClick(Sender: TObject);
    procedure GridBtnClick(Sender: TObject);
    procedure SnapBtnClick(Sender: TObject);

    { Path context menu handlers }
    procedure PathMenuAddFreehand(Sender: TObject);
    procedure PathMenuAddRect(Sender: TObject);
    procedure PathMenuAddCircle(Sender: TObject);
    procedure PathMenuDuplicate(Sender: TObject);
    procedure PathMenuReverse(Sender: TObject);
    procedure PathMenuCleanUp(Sender: TObject);
    procedure PathMenuRotateFwd(Sender: TObject);
    procedure PathMenuRotateBwd(Sender: TObject);
    procedure PathMenuRemove(Sender: TObject);

    { Shape context menu handlers }
    procedure ShapeMenuAddEmpty(Sender: TObject);
    procedure ShapeMenuDuplicate(Sender: TObject);
    procedure ShapeMenuResetTransform(Sender: TObject);
    procedure ShapeMenuFreezeTransform(Sender: TObject);
    procedure ShapeMenuSetTransformer(Sender: TObject);
    procedure ShapeMenuRemove(Sender: TObject);

    { Shape panel path double-click — jump to that path in the tree for editing. }
    procedure ShapePanelPathDblClick(Sender: TObject);

    { Canvas cursor-move callback — updates the status bar. }
    procedure CanvasCursorMove(Sender: TObject; AHvifX, AHvifY: Single);

    { Canvas shape-selected callback — syncs tree and panels when user
      clicks a different shape directly on the canvas. }
    procedure CanvasShapeSelected(Sender: TObject);

    { Refresh the status bar text from the current document/selection state. }
    procedure UpdateStatusBar;

    { Tree event handler }
    procedure ObjectTreeChanged(Sender: TObject);

    { Style management handlers }
    procedure StyleAdd(Sender: TObject);
    procedure StyleDelete(Sender: TObject);

    { Shape management handlers }
    procedure ShapeAdd(Sender: TObject);
    procedure ShapeDelete(Sender: TObject);
    procedure ShapeMoveUp(Sender: TObject);
    procedure ShapeMoveDown(Sender: TObject);

    { Select the given style/shape/path index in the object tree. }
    procedure SelectStyleInTree(AStyleIdx: Integer);
    procedure SelectShapeInTree(AShapeIdx: Integer);
    { Select the given path index in the object tree (after repopulation). }
    procedure SelectPathInTree(APathIdx: Integer);

  public
    procedure AfterCreate; override;
    destructor Destroy; override;
    procedure HandleKeyPress(var keycode: word; var shiftstate: TShiftState;
                             var consumed: boolean); override;
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
  SetupContextMenus;
  FDocument.OnChange := @HandleDocumentChange;
  FVertexCanvas.SetDocument(FDocument);
  FStylePanel.SetDocument(FDocument);
  FPathPanel.SetDocument(FDocument);
  FShapePanel.SetDocument(FDocument);
  FPreviewBar.SetDocument(FDocument);
  OnCloseQuery := @FormCloseQuery;
  FVertexCanvas.OnCursorMove    := @CanvasCursorMove;
  FVertexCanvas.OnShapeSelected := @CanvasShapeSelected;
  FShapePanel.OnPathDblClick    := @ShapePanelPathDblClick;
end;

procedure TVertexMainForm.SetupMenus;
var
  miRecent: TfpgMenuItem;
begin
  FMnuOpenRecent := TfpgPopupMenu.Create(Self);
  FMnuOpenRecent.Name := 'mnuOpenRecent';

  FRecentFiles := TfpgMRU.Create(Self);
  FRecentFiles.ParentMenuItem := FMnuOpenRecent;
  FRecentFiles.MaxItems       := 15;
  FRecentFiles.ShowFullPath   := True;
  FRecentFiles.OnClick        := @miRecentFileClick;
  FRecentFiles.LoadMRU;

  FMnuFile := TfpgPopupMenu.Create(Self);
  with FMnuFile do
  begin
    Name := 'mnuFile';
    AddMenuItem('New',            rsKeyCtrl + 'N',              @miFileNewClick);
    AddMenuItem('Open...',        rsKeyCtrl + 'O',              @miFileOpenClick);
    miRecent         := AddMenuItem('Open Recent', '', nil);
    miRecent.SubMenu := FMnuOpenRecent;
    AddSeparator;
    AddMenuItem('Save',           rsKeyCtrl + 'S',              @miFileSaveClick);
    AddMenuItem('Save As...',     rsKeyCtrl + rsKeyShift + 'S', @miFileSaveAsClick);
    AddSeparator;
    AddMenuItem('Export as .inc...', '', @miFileExportIncClick);
    AddMenuItem('Export as PNG...', '',  @miFileExportPngClick);
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

  { Zoom toolbar — sits below menu, spans full width.
    The preview bar (98px tall) is embedded at the left; all zoom/grid/snap
    buttons are positioned to its right, vertically centred (BY = 38). }
  FZoomBar := TfpgBevel.Create(Self);
  FZoomBar.Name  := 'zoomBar';
  FZoomBar.Style := bsFlat;
  FZoomBar.PreferredSize := fpgSize(800, 100);

  { Preview bar — left side of the zoom strip }
  FPreviewBar := TVertexPreviewBar.Create(FZoomBar);
  FPreviewBar.Name := 'previewBar';
  FPreviewBar.SetPosition(2, 2, 220, 96);

  { Zoom buttons — right of preview bar; BX = 230, BY = 38 (centred in 100px) }
  FBtnZoomFit := TfpgButton.Create(FZoomBar);
  FBtnZoomFit.Text    := 'Fit';
  FBtnZoomFit.Tag     := -1;
  FBtnZoomFit.SetPosition(230, 38, 36, 22);
  FBtnZoomFit.OnClick := @ZoomBtnClick;

  FBtnZoom50 := TfpgButton.Create(FZoomBar);
  FBtnZoom50.Text    := '50%';
  FBtnZoom50.Tag     := 50;
  FBtnZoom50.SetPosition(268, 38, 44, 22);
  FBtnZoom50.OnClick := @ZoomBtnClick;

  FBtnZoom100 := TfpgButton.Create(FZoomBar);
  FBtnZoom100.Text    := '100%';
  FBtnZoom100.Tag     := 100;
  FBtnZoom100.SetPosition(314, 38, 50, 22);
  FBtnZoom100.OnClick := @ZoomBtnClick;

  FBtnZoom200 := TfpgButton.Create(FZoomBar);
  FBtnZoom200.Text    := '200%';
  FBtnZoom200.Tag     := 200;
  FBtnZoom200.SetPosition(366, 38, 50, 22);
  FBtnZoom200.OnClick := @ZoomBtnClick;

  FBtnZoom400 := TfpgButton.Create(FZoomBar);
  FBtnZoom400.Text    := '400%';
  FBtnZoom400.Tag     := 400;
  FBtnZoom400.SetPosition(418, 38, 50, 22);
  FBtnZoom400.OnClick := @ZoomBtnClick;

  FZoomLabel := TfpgLabel.Create(FZoomBar);
  FZoomLabel.Text := 'Fit';
  FZoomLabel.SetPosition(476, 41, 60, 18);

  FBtnGrid := TfpgButton.Create(FZoomBar);
  FBtnGrid.Text    := 'Grid';
  FBtnGrid.Tag     := 0;   { 0=off, 1=on }
  FBtnGrid.SetPosition(544, 38, 46, 22);
  FBtnGrid.OnClick := @GridBtnClick;

  FBtnSnap := TfpgButton.Create(FZoomBar);
  FBtnSnap.Text    := 'Snap';
  FBtnSnap.Tag     := 0;   { 0=off, 1=on }
  FBtnSnap.SetPosition(592, 38, 50, 22);
  FBtnSnap.OnClick := @SnapBtnClick;

  mig.AddLayoutComponent(FZoomBar, TfpgMigCC.Create().DockNorth.GrowX());

  { Left toolbox placeholder — fixed 64px wide }
  FToolBox := TfpgBevel.Create(Self);
  FToolBox.Name  := 'toolBox';
  FToolBox.Style := bsLowered;
  FToolBox.PreferredSize := fpgSize(64, 200);

  FToolLabel := TfpgLabel.Create(FToolBox);
  FToolLabel.Name    := 'lblTools';
  FToolLabel.Text    := 'Tools';
  FToolLabel.SetPosition(4, 4, 56, 18);
  FToolLabel.FontDesc := '#Label1';

  FBtnToolSelect := TfpgButton.Create(FToolBox);
  FBtnToolSelect.Text    := 'Sel';
  FBtnToolSelect.Tag     := 0;   { tmSelect }
  FBtnToolSelect.SetPosition(2, 24, 60, 26);
  FBtnToolSelect.OnClick := @ToolBtnClick;
  FBtnToolSelect.FontDesc := '#Label1';

  FBtnToolNode := TfpgButton.Create(FToolBox);
  FBtnToolNode.Text    := 'Node';
  FBtnToolNode.Tag     := 1;   { tmNode }
  FBtnToolNode.SetPosition(2, 52, 60, 26);
  FBtnToolNode.OnClick := @ToolBtnClick;
  FBtnToolNode.FontDesc := '#Label1';

  FBtnToolPan := TfpgButton.Create(FToolBox);
  FBtnToolPan.Text    := 'Pan';
  FBtnToolPan.Tag     := 2;   { tmPan }
  FBtnToolPan.SetPosition(2, 80, 60, 26);
  FBtnToolPan.OnClick := @ToolBtnClick;
  FBtnToolPan.FontDesc := '#Label1';

  FBtnToolAddPt := TfpgButton.Create(FToolBox);
  FBtnToolAddPt.Text    := '+Pt';
  FBtnToolAddPt.Tag     := 3;   { tmAddPoint }
  FBtnToolAddPt.SetPosition(2, 108, 60, 26);
  FBtnToolAddPt.OnClick := @ToolBtnClick;
  FBtnToolAddPt.FontDesc := '#Label1';

  FBtnToolDelPt := TfpgButton.Create(FToolBox);
  FBtnToolDelPt.Text    := '-Pt';
  FBtnToolDelPt.Tag     := 4;   { tmDeletePoint }
  FBtnToolDelPt.SetPosition(2, 136, 60, 26);
  FBtnToolDelPt.OnClick := @ToolBtnClick;
  FBtnToolDelPt.FontDesc := '#Label1';

  FBtnToolZoom := TfpgButton.Create(FToolBox);
  FBtnToolZoom.Text    := 'Zoom';
  FBtnToolZoom.Tag     := 5;   { tmZoom }
  FBtnToolZoom.SetPosition(2, 164, 60, 26);
  FBtnToolZoom.OnClick := @ToolBtnClick;
  FBtnToolZoom.FontDesc := '#Label1';

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
  FRightPanel.PreferredSize := fpgSize(240, 400);

  rmig := TfpgMigLayoutManager.Create;
  rmig.LC.Fill.WrapAfter(1);
  FRightPanel.LayoutManager := rmig;

  FObjectTree := TfpgTreeView.Create(FRightPanel);
  FObjectTree.Name     := 'objectTree';
  FObjectTree.PreferredSize := fpgSize(220, 200);
  FObjectTree.OnChange := @ObjectTreeChanged;
  rmig.AddLayoutComponent(FObjectTree, TfpgMigCC.Create().GrowX().GrowY().PushY());

  { Style management button bar: [+ Style] [-] }
  FStyleBar := TfpgBevel.Create(FRightPanel);
  FStyleBar.Name  := 'styleBar';
  FStyleBar.Style := bsFlat;
  FStyleBar.PreferredSize := fpgSize(220, 26);

  FBtnStyleAdd := TfpgButton.Create(FStyleBar);
  FBtnStyleAdd.Text    := '+ Style';
  FBtnStyleAdd.SetPosition(2, 2, 66, 22);
  FBtnStyleAdd.OnClick := @StyleAdd;

  FBtnStyleDel := TfpgButton.Create(FStyleBar);
  FBtnStyleDel.Text    := '- Style';
  FBtnStyleDel.SetPosition(70, 2, 66, 22);
  FBtnStyleDel.OnClick := @StyleDelete;

  rmig.AddLayoutComponent(FStyleBar, TfpgMigCC.Create().GrowX());

  { Path management button bar: [+ Path] [-] }
  FPathBar := TfpgBevel.Create(FRightPanel);
  FPathBar.Name  := 'pathBar';
  FPathBar.Style := bsFlat;
  FPathBar.PreferredSize := fpgSize(220, 26);

  FBtnPathAdd := TfpgButton.Create(FPathBar);
  FBtnPathAdd.Text    := '+ Path';
  FBtnPathAdd.SetPosition(2, 2, 66, 22);
  FBtnPathAdd.OnClick := @PathMenuAddFreehand;

  FBtnPathDel := TfpgButton.Create(FPathBar);
  FBtnPathDel.Text    := '- Path';
  FBtnPathDel.SetPosition(70, 2, 66, 22);
  FBtnPathDel.OnClick := @PathMenuRemove;

  rmig.AddLayoutComponent(FPathBar, TfpgMigCC.Create().GrowX());

  { Shape management button bar: [+ Shape] [-] [Up] [Down] }
  FShapeBar := TfpgBevel.Create(FRightPanel);
  FShapeBar.Name  := 'shapeBar';
  FShapeBar.Style := bsFlat;
  FShapeBar.PreferredSize := fpgSize(220, 26);

  FBtnShapeAdd := TfpgButton.Create(FShapeBar);
  FBtnShapeAdd.Text    := '+ Shape';
  FBtnShapeAdd.SetPosition(2, 2, 62, 22);
  FBtnShapeAdd.OnClick := @ShapeAdd;

  FBtnShapeDel := TfpgButton.Create(FShapeBar);
  FBtnShapeDel.Text    := '-';
  FBtnShapeDel.SetPosition(66, 2, 32, 22);
  FBtnShapeDel.OnClick := @ShapeDelete;

  FBtnShapeUp := TfpgButton.Create(FShapeBar);
  FBtnShapeUp.Text    := 'Up';
  FBtnShapeUp.SetPosition(100, 2, 50, 22);
  FBtnShapeUp.OnClick := @ShapeMoveUp;

  FBtnShapeDown := TfpgButton.Create(FShapeBar);
  FBtnShapeDown.Text    := 'Down';
  FBtnShapeDown.SetPosition(152, 2, 58, 22);
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
    FStylesNode := nil;
    FShapesNode := nil;
    FPathsNode  := nil;

    { ── Styles ── }
    nStyles := FObjectTree.RootNode.AppendText(
        Format('Styles (%d)', [FDocument.StyleCount]));
    nStyles.Expand;
    FStylesNode := nStyles;

    for i := 0 to FDocument.StyleCount - 1 do
    begin
      st := FDocument.Styles[i];
      case st.StyleType of
        hstSolidColor, hstSolidColorNoAlpha:
          s := Format('%s  #%2.2X%2.2X%2.2X  A=%d',
               [st.Name, st.Color.R, st.Color.G, st.Color.B, st.Color.A]);
        hstSolidGray, hstSolidGrayNoAlpha:
          s := Format('%s  gray K=%d', [st.Name, st.Color.R]);
        hstGradient:
          s := Format('%s  gradient (%d stops)', [st.Name, st.StopCount]);
      else
        s := st.Name;
      end;
      n := nStyles.AppendText(s);
      n.Data := Pointer(PtrUInt(i));
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
      if sh.Style <> nil then
        s := Format('%s  [%s]  %d path(s)', [sh.Name, sh.Style.Name, sh.PathCount])
      else
        s := Format('%s  (no style)  %d path(s)', [sh.Name, sh.PathCount]);
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


{ ── Style management ─────────────────────────────────────────────────────── }

procedure TVertexMainForm.SelectStyleInTree(AStyleIdx: Integer);
var
  n: TfpgTreeNode;
begin
  if (FStylesNode = nil) or (AStyleIdx < 0) then
    Exit;
  n := FStylesNode.FirstSubNode;
  while n <> nil do
  begin
    if Integer(PtrUInt(n.Data)) = AStyleIdx then
    begin
      FObjectTree.Selection := n;
      Exit;
    end;
    n := n.Next;
  end;
end;

procedure TVertexMainForm.StyleAdd(Sender: TObject);
var
  style: TVertexStyle;
  col:   THvifColor;
  cmd:   TVertexCmdAddStyle;
  newIdx: Integer;
begin
  { New solid white opaque style }
  style           := TVertexStyle.Create(FDocument.UniqueName('style'));
  style.StyleType := hstSolidColor;
  col.R := 255; col.G := 255; col.B := 255; col.A := 255;
  style.Color := col;
  cmd := TVertexCmdAddStyle.Create(FDocument, style);
  FDocument.UndoStack.Execute(cmd);
  newIdx := FDocument.StyleCount - 1;
  PopulateObjectTree;
  SelectStyleInTree(newIdx);
  ObjectTreeChanged(nil);
end;

procedure TVertexMainForm.StyleDelete(Sender: TObject);
var
  node:   TfpgTreeNode;
  idx:    Integer;
  style:  TVertexStyle;
  i:      Integer;
  users:  string;
  cmd:    TVertexCmdDeleteStyle;
begin
  node := FObjectTree.Selection;
  if (node = nil) or (FStylesNode = nil) or (node.Parent <> FStylesNode) then
    Exit;
  idx := Integer(PtrUInt(node.Data));
  if (idx < 0) or (idx >= FDocument.StyleCount) then
    Exit;
  style := FDocument.Styles[idx];
  { Refuse if any shape still references this style — collect all names }
  users := '';
  for i := 0 to FDocument.ShapeCount - 1 do
    if FDocument.Shapes[i].Style = style then
      users := users + '  ' + FDocument.Shapes[i].Name + LineEnding;
  if users <> '' then
  begin
    ShowMessage('Cannot delete style "' + style.Name +
        '": it is used by:' + LineEnding + users +
        'Remove the style from all those shapes first.', 'Vertex');
    Exit;
  end;
  cmd := TVertexCmdDeleteStyle.Create(FDocument, style);
  FDocument.UndoStack.Execute(cmd);
  FStylePanel.SetStyle(nil);
  FObjectTree.PopupMenu := nil;
  PopulateObjectTree;
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

procedure TVertexMainForm.SelectPathInTree(APathIdx: Integer);
var
  n: TfpgTreeNode;
begin
  if (FPathsNode = nil) or (APathIdx < 0) then
    Exit;
  n := FPathsNode.FirstSubNode;
  while n <> nil do
  begin
    if Integer(PtrUInt(n.Data)) = APathIdx then
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

  { Style selected → show style properties; clear canvas selection }
  if (FStylesNode <> nil) and (node.Parent = FStylesNode) then
  begin
    idx := Integer(PtrUInt(node.Data));
    FStylePanel.SetStyle(FDocument.Styles[idx]);
    FPathPanel.SetPath(nil);
    FShapePanel.SetShape(nil);
    FVertexCanvas.SelectedShapeIndex := -1;
    FVertexCanvas.SetEditPath(nil);
    FObjectTree.PopupMenu := nil;
    ApplyToolMode(tmSelect);
    UpdateStatusBar;
    Exit;
  end;

  { Path selected → path-edit mode: show anchor points and curve handles }
  if (FPathsNode <> nil) and (node.Parent = FPathsNode) then
  begin
    idx := Integer(PtrUInt(node.Data));
    FPathPanel.SetPath(FDocument.Paths[idx]);
    FVertexCanvas.SelectedShapeIndex := -1;
    FVertexCanvas.SetEditPath(FDocument.Paths[idx]);
    FStylePanel.SetStyle(nil);
    FShapePanel.SetShape(nil);
    FObjectTree.PopupMenu := FMnuPath;
    ApplyToolMode(tmNode);
    UpdateStatusBar;
    Exit;
  end;

  { Shape selected → shape-transform mode: show bounding box / translate handles }
  if (FShapesNode <> nil) and (node.Parent = FShapesNode) then
  begin
    idx := Integer(PtrUInt(node.Data));
    FVertexCanvas.SelectedShapeIndex := idx;
    FVertexCanvas.SetEditPath(nil);
    FStylePanel.SetStyle(FDocument.Shapes[idx].Style);
    FPathPanel.SetPath(nil);
    FShapePanel.SetShape(FDocument.Shapes[idx]);
    FObjectTree.PopupMenu := FMnuShape;
    ApplyToolMode(tmSelect);
    UpdateStatusBar;
    Exit;
  end;

  { Any other node (header rows, style nodes) — clear all }
  FVertexCanvas.SelectedShapeIndex := -1;
  FVertexCanvas.SetEditPath(nil);
  FStylePanel.SetStyle(nil);
  FPathPanel.SetPath(nil);
  FShapePanel.SetShape(nil);
  FObjectTree.PopupMenu := nil;
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

procedure TVertexMainForm.DoOpenFile(const AFileName: string);
begin
  try
    FDocument.LoadFromFile(AFileName);
  except
    on E: Exception do
    begin
      ShowMessage('Open failed: ' + E.Message, 'Vertex');
      Exit;
    end;
  end;

  FCurrentFile := AFileName;
  FLastOpenDir := ExtractFilePath(AFileName);
  FRecentFiles.AddItem(AFileName);

  FVertexCanvas.DocumentChanged;
  FPreviewBar.DocumentChanged;
  FVertexCanvas.SelectedShapeIndex := -1;
  FStylePanel.SetStyle(nil);
  FPathPanel.SetPath(nil);
  FShapePanel.SetShape(nil);
  PopulateObjectTree;
  UpdateTitle;
  UpdateStatusBar;
end;

procedure TVertexMainForm.miFileOpenClick(Sender: TObject);
var
  fn: string;
begin
  fn := SelectFileDialog(sfdOpen, 'HVIF files (*.hvif)|*.hvif|All files (*)|*', FLastOpenDir);
  if fn = '' then
    Exit;
  DoOpenFile(fn);
end;

procedure TVertexMainForm.miRecentFileClick(Sender: TObject; const FileName: string);
begin
  if not FileExists(FileName) then
  begin
    ShowMessage('File not found:' + LineEnding + FileName, 'Vertex');
    FRecentFiles.RemoveItem(FileName);
    Exit;
  end;
  if not PromptSaveIfDirty then
    Exit;
  DoOpenFile(FileName);
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

procedure TVertexMainForm.ApplyToolMode(AMode: TVertexToolMode);
begin
  FVertexCanvas.ToolMode := AMode;
  { Sync toolbar button bold state }
  FBtnToolSelect.FontDesc := '#Label1';
  FBtnToolNode.FontDesc   := '#Label1';
  FBtnToolPan.FontDesc    := '#Label1';
  FBtnToolAddPt.FontDesc  := '#Label1';
  FBtnToolDelPt.FontDesc  := '#Label1';
  FBtnToolZoom.FontDesc   := '#Label1';
  case AMode of
    tmSelect:      FBtnToolSelect.FontDesc := '#Label1:bold';
    tmNode:        FBtnToolNode.FontDesc   := '#Label1:bold';
    tmPan:         FBtnToolPan.FontDesc    := '#Label1:bold';
    tmAddPoint:    FBtnToolAddPt.FontDesc  := '#Label1:bold';
    tmDeletePoint: FBtnToolDelPt.FontDesc  := '#Label1:bold';
    tmZoom:        FBtnToolZoom.FontDesc   := '#Label1:bold';
  end;
end;

procedure TVertexMainForm.ToolBtnClick(Sender: TObject);
var
  mode: TVertexToolMode;
begin
  case TfpgButton(Sender).Tag of
    0: mode := tmSelect;
    1: mode := tmNode;
    2: mode := tmPan;
    3: mode := tmAddPoint;
    4: mode := tmDeletePoint;
    5: mode := tmZoom;
  else
    mode := tmNode;
  end;
  ApplyToolMode(mode);
end;

procedure TVertexMainForm.SetupContextMenus;
begin
  FMnuPath := TfpgPopupMenu.Create(Self);
  with FMnuPath do
  begin
    AddMenuItem('Add freehand path',  '', @PathMenuAddFreehand);
    AddMenuItem('Add rect path',      '', @PathMenuAddRect);
    AddMenuItem('Add circle path',    '', @PathMenuAddCircle);
    AddSeparator;
    AddMenuItem('Duplicate',          '', @PathMenuDuplicate);
    AddSeparator;
    AddMenuItem('Reverse',            '', @PathMenuReverse);
    AddMenuItem('Clean up',           '', @PathMenuCleanUp);
    AddMenuItem('Rotate indices >>',  '', @PathMenuRotateFwd);
    AddMenuItem('<< Rotate indices',  '', @PathMenuRotateBwd);
    AddSeparator;
    AddMenuItem('Remove',             '', @PathMenuRemove);
  end;

  FMnuShape := TfpgPopupMenu.Create(Self);
  with FMnuShape do
  begin
    AddMenuItem('Add empty shape',    '', @ShapeMenuAddEmpty);
    AddSeparator;
    AddMenuItem('Duplicate',          '', @ShapeMenuDuplicate);
    AddSeparator;
    AddMenuItem('Reset transformation', '', @ShapeMenuResetTransform);
    AddMenuItem('Freeze transformation', '', @ShapeMenuFreezeTransform);
    AddSeparator;
    with AddMenuItem('Set transformer: Stroke',      '', @ShapeMenuSetTransformer) do Tag := 1;
    with AddMenuItem('Set transformer: Contour',     '', @ShapeMenuSetTransformer) do Tag := 2;
    with AddMenuItem('Set transformer: Perspective', '', @ShapeMenuSetTransformer) do Tag := 3;
    with AddMenuItem('Remove transformer',           '', @ShapeMenuSetTransformer) do Tag := 0;
    AddSeparator;
    AddMenuItem('Remove',             '', @ShapeMenuRemove);
  end;
end;

{ ── Path context menu handlers ────────────────────────────────────────────── }

procedure TVertexMainForm.PathMenuAddFreehand(Sender: TObject);
var
  path: TVertexPath;
  pt:   TVertexPoint;
  cmd:  TVertexCmdAddPath;
  idx:  Integer;
begin
  if FDocument = nil then Exit;
  path        := TVertexPath.Create(FDocument.UniqueName('path'));
  path.Closed := False;
  FillChar(pt, SizeOf(pt), 0);
  pt.X := 20; pt.Y := 32; pt.InX := 20; pt.InY := 32; pt.OutX := 20; pt.OutY := 32;
  path.AddPoint(pt);
  pt.X := 44; pt.Y := 32; pt.InX := 44; pt.InY := 32; pt.OutX := 44; pt.OutY := 32;
  path.AddPoint(pt);
  cmd := TVertexCmdAddPath.Create(FDocument, path);
  FDocument.UndoStack.Execute(cmd);
  idx := FDocument.PathCount - 1;
  PopulateObjectTree;
  SelectPathInTree(idx);
end;

procedure TVertexMainForm.PathMenuAddRect(Sender: TObject);
var
  path: TVertexPath;
  pt:   TVertexPoint;
  cmd:  TVertexCmdAddPath;
  idx:  Integer;
begin
  if FDocument = nil then Exit;
  path        := TVertexPath.Create(FDocument.UniqueName('path'));
  path.Closed := True;
  FillChar(pt, SizeOf(pt), 0);
  { Four corners of a rect centred at (32,32), 20×20 }
  pt.X := 22; pt.Y := 22; pt.InX := 22; pt.InY := 22; pt.OutX := 22; pt.OutY := 22;
  path.AddPoint(pt);
  pt.X := 42; pt.Y := 22; pt.InX := 42; pt.InY := 22; pt.OutX := 42; pt.OutY := 22;
  path.AddPoint(pt);
  pt.X := 42; pt.Y := 42; pt.InX := 42; pt.InY := 42; pt.OutX := 42; pt.OutY := 42;
  path.AddPoint(pt);
  pt.X := 22; pt.Y := 42; pt.InX := 22; pt.InY := 42; pt.OutX := 22; pt.OutY := 42;
  path.AddPoint(pt);
  cmd := TVertexCmdAddPath.Create(FDocument, path);
  FDocument.UndoStack.Execute(cmd);
  idx := FDocument.PathCount - 1;
  PopulateObjectTree;
  SelectPathInTree(idx);
end;

procedure TVertexMainForm.PathMenuAddCircle(Sender: TObject);
const
  { Bezier circle approximation constant ≈ 0.5523 × radius }
  K = 7.07;   { 0.5523 × 12.8 ≈ 7.07 for radius ~12.8 }
var
  path: TVertexPath;
  pt:   TVertexPoint;
  cmd:  TVertexCmdAddPath;
  idx:  Integer;
  cx, cy, r: Single;
begin
  if FDocument = nil then Exit;
  cx := 32; cy := 32; r := 14;
  path        := TVertexPath.Create(FDocument.UniqueName('path'));
  path.Closed := True;
  FillChar(pt, SizeOf(pt), 0);
  { Top }
  pt.X := cx;    pt.Y := cy - r; pt.InX := cx - K; pt.InY := cy - r; pt.OutX := cx + K; pt.OutY := cy - r; path.AddPoint(pt);
  { Right }
  pt.X := cx + r; pt.Y := cy;    pt.InX := cx + r; pt.InY := cy - K; pt.OutX := cx + r; pt.OutY := cy + K; path.AddPoint(pt);
  { Bottom }
  pt.X := cx;    pt.Y := cy + r; pt.InX := cx + K; pt.InY := cy + r; pt.OutX := cx - K; pt.OutY := cy + r; path.AddPoint(pt);
  { Left }
  pt.X := cx - r; pt.Y := cy;    pt.InX := cx - r; pt.InY := cy + K; pt.OutX := cx - r; pt.OutY := cy - K; path.AddPoint(pt);
  cmd := TVertexCmdAddPath.Create(FDocument, path);
  FDocument.UndoStack.Execute(cmd);
  idx := FDocument.PathCount - 1;
  PopulateObjectTree;
  SelectPathInTree(idx);
end;

procedure TVertexMainForm.PathMenuDuplicate(Sender: TObject);
var
  node:    TfpgTreeNode;
  src:     TVertexPath;
  dup:     TVertexPath;
  cmd:     TVertexCmdAddPath;
  i, idx:  Integer;
  pt:      TVertexPoint;
begin
  node := FObjectTree.Selection;
  if (node = nil) or (FPathsNode = nil) or (node.Parent <> FPathsNode) then Exit;
  idx := Integer(PtrUInt(node.Data));
  if (idx < 0) or (idx >= FDocument.PathCount) then Exit;
  src := FDocument.Paths[idx];
  dup := TVertexPath.Create(FDocument.UniqueName(src.Name));
  dup.Closed := src.Closed;
  for i := 0 to src.PointCount - 1 do
  begin
    pt := src.Points[i];
    dup.AddPoint(pt);
  end;
  cmd := TVertexCmdAddPath.Create(FDocument, dup);
  FDocument.UndoStack.Execute(cmd);
  PopulateObjectTree;
end;

procedure TVertexMainForm.PathMenuReverse(Sender: TObject);
var
  node: TfpgTreeNode;
  idx:  Integer;
  cmd:  TVertexCmdReversePath;
begin
  node := FObjectTree.Selection;
  if (node = nil) or (FPathsNode = nil) or (node.Parent <> FPathsNode) then Exit;
  idx := Integer(PtrUInt(node.Data));
  if (idx < 0) or (idx >= FDocument.PathCount) then Exit;
  cmd := TVertexCmdReversePath.Create(FDocument.Paths[idx]);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexMainForm.PathMenuCleanUp(Sender: TObject);
var
  node:    TfpgTreeNode;
  idx, i:  Integer;
  path:    TVertexPath;
  pt, npt: TVertexPoint;
  toRemove: array of Integer;
  count:   Integer;
  dx, dy:  Single;
  cmd2:    TVertexCmdDeletePoint;
const
  kMinDist = 0.5;
begin
  node := FObjectTree.Selection;
  if (node = nil) or (FPathsNode = nil) or (node.Parent <> FPathsNode) then Exit;
  idx := Integer(PtrUInt(node.Data));
  if (idx < 0) or (idx >= FDocument.PathCount) then Exit;
  path := FDocument.Paths[idx];
  { Collect indices of near-duplicate points (dist < 0.5 HVIF units from successor) }
  count := 0;
  SetLength(toRemove, path.PointCount);
  i := 0;
  while i < path.PointCount - 1 do
  begin
    pt  := path.Points[i];
    npt := path.Points[i + 1];
    dx  := npt.X - pt.X;
    dy  := npt.Y - pt.Y;
    if Sqrt(dx * dx + dy * dy) < kMinDist then
    begin
      toRemove[count] := i + 1;
      Inc(count);
    end;
    Inc(i);
  end;
  { Remove in reverse order so indices stay valid }
  i := count - 1;
  while i >= 0 do
  begin
    if path.PointCount > 2 then
    begin
      cmd2 := TVertexCmdDeletePoint.Create(path, toRemove[i]);
      FDocument.UndoStack.Execute(cmd2);
    end;
    Dec(i);
  end;
  if count > 0 then
    PopulateObjectTree;
end;

procedure TVertexMainForm.PathMenuRotateFwd(Sender: TObject);
var
  node: TfpgTreeNode;
  idx:  Integer;
  cmd:  TVertexCmdRotatePathIndices;
begin
  node := FObjectTree.Selection;
  if (node = nil) or (FPathsNode = nil) or (node.Parent <> FPathsNode) then Exit;
  idx := Integer(PtrUInt(node.Data));
  if (idx < 0) or (idx >= FDocument.PathCount) then Exit;
  cmd := TVertexCmdRotatePathIndices.Create(FDocument.Paths[idx], 1);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexMainForm.PathMenuRotateBwd(Sender: TObject);
var
  node: TfpgTreeNode;
  idx:  Integer;
  cmd:  TVertexCmdRotatePathIndices;
begin
  node := FObjectTree.Selection;
  if (node = nil) or (FPathsNode = nil) or (node.Parent <> FPathsNode) then Exit;
  idx := Integer(PtrUInt(node.Data));
  if (idx < 0) or (idx >= FDocument.PathCount) then Exit;
  cmd := TVertexCmdRotatePathIndices.Create(FDocument.Paths[idx], -1);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexMainForm.PathMenuRemove(Sender: TObject);
var
  node: TfpgTreeNode;
  idx:  Integer;
  path: TVertexPath;
  cmd:  TVertexCmdDeletePath;
begin
  node := FObjectTree.Selection;
  if (node = nil) or (FPathsNode = nil) or (node.Parent <> FPathsNode) then Exit;
  idx := Integer(PtrUInt(node.Data));
  if (idx < 0) or (idx >= FDocument.PathCount) then Exit;
  path := FDocument.Paths[idx];
  { Don't delete if any shape still references this path }
  cmd := TVertexCmdDeletePath.Create(FDocument, path);
  try
    FDocument.UndoStack.Execute(cmd);
  except
    on E: Exception do
    begin
      ShowMessage('Cannot remove path: ' + E.Message, 'Vertex');
      Exit;
    end;
  end;
  FPathPanel.SetPath(nil);
  FObjectTree.PopupMenu := nil;
  PopulateObjectTree;
end;

{ ── Shape context menu handlers ───────────────────────────────────────────── }

procedure TVertexMainForm.ShapeMenuAddEmpty(Sender: TObject);
begin
  { Reuse the existing ShapeAdd logic }
  ShapeAdd(Sender);
end;

procedure TVertexMainForm.ShapeMenuDuplicate(Sender: TObject);
var
  node:  TfpgTreeNode;
  idx:   Integer;
  src:   TVertexShape;
  dup:   TVertexShape;
  cmd:   TVertexCmdAddShape;
  i:     Integer;
begin
  node := FObjectTree.Selection;
  if (node = nil) or (FShapesNode = nil) or (node.Parent <> FShapesNode) then Exit;
  idx := Integer(PtrUInt(node.Data));
  if (idx < 0) or (idx >= FDocument.ShapeCount) then Exit;
  src := FDocument.Shapes[idx];
  dup := TVertexShape.Create(FDocument.UniqueName(src.Name));
  dup.Style   := src.Style;
  dup.Visible := src.Visible;
  for i := 0 to src.PathCount - 1 do
    dup.AddPathRef(src.Paths[i]);
  dup.Transformer := src.Transformer;
  dup.HasTranslation := src.HasTranslation;
  dup.TranslateX := src.TranslateX;
  dup.TranslateY := src.TranslateY;
  cmd := TVertexCmdAddShape.Create(FDocument, dup);
  FDocument.UndoStack.Execute(cmd);
  PopulateObjectTree;
  SelectShapeInTree(FDocument.ShapeCount - 1);
end;

procedure TVertexMainForm.ShapeMenuResetTransform(Sender: TObject);
var
  node:  TfpgTreeNode;
  idx:   Integer;
  shape: TVertexShape;
  cmd:   TVertexCmdSetShapeTranslation;
begin
  node := FObjectTree.Selection;
  if (node = nil) or (FShapesNode = nil) or (node.Parent <> FShapesNode) then Exit;
  idx := Integer(PtrUInt(node.Data));
  if (idx < 0) or (idx >= FDocument.ShapeCount) then Exit;
  shape := FDocument.Shapes[idx];
  cmd := TVertexCmdSetShapeTranslation.Create(shape, False, 0, 0);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexMainForm.ShapeMenuFreezeTransform(Sender: TObject);
var
  node:  TfpgTreeNode;
  idx:   Integer;
  shape: TVertexShape;
  cmd:   TVertexCmdFreezeTransform;
begin
  node := FObjectTree.Selection;
  if (node = nil) or (FShapesNode = nil) or (node.Parent <> FShapesNode) then Exit;
  idx := Integer(PtrUInt(node.Data));
  if (idx < 0) or (idx >= FDocument.ShapeCount) then Exit;
  shape := FDocument.Shapes[idx];
  if not shape.HasTranslation then Exit;  { nothing to freeze }
  cmd := TVertexCmdFreezeTransform.Create(shape);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexMainForm.ShapeMenuSetTransformer(Sender: TObject);
var
  node:     TfpgTreeNode;
  idx:      Integer;
  shape:    TVertexShape;
  newTrans: TVertexTransformer;
  cmd:      TVertexCmdSetTransformer;
  tt:       Integer;
begin
  node := FObjectTree.Selection;
  if (node = nil) or (FShapesNode = nil) or (node.Parent <> FShapesNode) then Exit;
  idx := Integer(PtrUInt(node.Data));
  if (idx < 0) or (idx >= FDocument.ShapeCount) then Exit;
  shape := FDocument.Shapes[idx];
  tt    := TfpgMenuItem(Sender).Tag;  { 0=None, 1=Stroke, 2=Contour, 3=Perspective }
  newTrans := Default(TVertexTransformer);
  case tt of
    1: begin
         newTrans.TransType  := ittStroke;
         newTrans.Width      := 1.0;
         newTrans.MiterLimit := 4.0;
       end;
    2: begin
         newTrans.TransType := ittContour;
         newTrans.Width     := 1.0;
       end;
    3: newTrans.TransType := ittPerspective;
  { 0: ittNone — already the default }
  end;
  if newTrans.TransType = shape.Transformer.TransType then Exit;
  cmd := TVertexCmdSetTransformer.Create(shape, newTrans);
  FDocument.UndoStack.Execute(cmd);
end;

procedure TVertexMainForm.ShapeMenuRemove(Sender: TObject);
begin
  { Reuse the existing ShapeDelete logic }
  ShapeDelete(Sender);
end;

procedure TVertexMainForm.ZoomBtnClick(Sender: TObject);
var
  z: Integer;
begin
  z := TfpgButton(Sender).Tag;
  FVertexCanvas.Zoom := z;
  if z < 0 then
    FZoomLabel.Text := 'Fit'
  else
    FZoomLabel.Text := Format('%d%%', [z]);
end;

procedure TVertexMainForm.GridBtnClick(Sender: TObject);
begin
  FBtnGrid.Tag := 1 - FBtnGrid.Tag;   { toggle }
  FVertexCanvas.ShowGrid := FBtnGrid.Tag = 1;
  if FBtnGrid.Tag = 1 then
    FBtnGrid.Text := 'Grid ON'
  else
    FBtnGrid.Text := 'Grid';
end;

procedure TVertexMainForm.SnapBtnClick(Sender: TObject);
begin
  FBtnSnap.Tag := 1 - FBtnSnap.Tag;   { toggle }
  FVertexCanvas.SnapToGrid := FBtnSnap.Tag = 1;
  if FBtnSnap.Tag = 1 then
    FBtnSnap.Text := 'Snap ON'
  else
    FBtnSnap.Text := 'Snap';
end;

procedure TVertexMainForm.miFileExportPngClick(Sender: TObject);
var
  fn:      string;
  initDir: string;
begin
  if FCurrentFile <> '' then
    initDir := ExtractFilePath(FCurrentFile)
  else
    initDir := '';
  fn := SelectFileDialog(sfdSave,
      'PNG files (*.png)|*.png|All files (*)|*', initDir);
  if fn = '' then
    Exit;
  { Strip extension — we'll append _<size>.png ourselves }
  fn := ChangeFileExt(fn, '');
  ExportAsPng(fn);
end;

procedure TVertexMainForm.ExportAsPng(const ABasePath: string);
const
  kSizes: array[0..3] of Integer = (16, 32, 48, 64);
var
  ms:       TMemoryStream;
  icon:     THvifIcon;
  img:      TfpgImage;
  fpImg:    TFPMemoryImage;
  writer:   TFPWriterPNG;
  sz, x, y: Integer;
  fn:       string;
  c:        TfpgColor;
  fpc:      TFPColor;
  exported: Integer;
begin
  ms := TMemoryStream.Create;
  try
    FDocument.SaveToStream(ms);
    ms.Position := 0;
    icon := THvifIcon.CreateFromStream(ms);
  finally
    ms.Free;
  end;

  exported := 0;
  try
    for sz in kSizes do
    begin
      img   := icon.GetImage(sz, sz);   { owned by icon — do NOT free }
      fpImg := TFPMemoryImage.Create(sz, sz);
      try
        for y := 0 to sz - 1 do
          for x := 0 to sz - 1 do
          begin
            c := img.Colors[x, y];
            { TfpgColor layout: bits 31-24 alpha, 23-16 red, 15-8 green, 7-0 blue }
            fpc.Alpha := ((c shr 24) and $FF) * $0101;
            fpc.Red   := ((c shr 16) and $FF) * $0101;
            fpc.Green := ((c shr 8)  and $FF) * $0101;
            fpc.Blue  := (c          and $FF) * $0101;
            fpImg.Colors[x, y] := fpc;
          end;

        fn     := Format('%s_%d.png', [ABasePath, sz]);
        writer := TFPWriterPNG.Create;
        try
          writer.UseAlpha := True;
          fpImg.SaveToFile(fn, writer);
          Inc(exported);
        finally
          writer.Free;
        end;
      finally
        fpImg.Free;
      end;
    end;
  finally
    icon.Free;
  end;

  ShowMessage(Format('Exported %d PNG file(s) from %s.',
      [exported, ExtractFileName(ABasePath)]), 'Vertex');
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

procedure TVertexMainForm.ShapePanelPathDblClick(Sender: TObject);
var
  path: TVertexPath;
  idx:  Integer;
begin
  path := FShapePanel.SelectedPath;
  if path = nil then Exit;
  idx := FDocument.IndexOfPath(path);
  if idx < 0 then Exit;
  SelectPathInTree(idx);
  ObjectTreeChanged(nil);
end;

procedure TVertexMainForm.CanvasShapeSelected(Sender: TObject);
var
  idx: Integer;
begin
  idx := FVertexCanvas.SelectedShapeIndex;
  if (idx < 0) or (idx >= FDocument.ShapeCount) then Exit;
  { Programmatically setting Selection does not fire OnChange, so we
    update the tree node first then refresh all panels explicitly. }
  SelectShapeInTree(idx);
  ObjectTreeChanged(nil);
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

procedure TVertexMainForm.HandleKeyPress(var keycode: word;
    var shiftstate: TShiftState; var consumed: boolean);
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);
  if consumed then Exit;
  { Tool shortcuts — only when no modifier held }
  if shiftstate = [] then
    case keycode of
      Ord('s'), Ord('S'):
        begin
          ToolBtnClick(FBtnToolSelect);
          consumed := True;
        end;
      Ord('n'), Ord('N'):
        begin
          ToolBtnClick(FBtnToolNode);
          consumed := True;
        end;
      Ord('p'), Ord('P'):
        begin
          ToolBtnClick(FBtnToolPan);
          consumed := True;
        end;
    end;
end;

procedure TVertexMainForm.miHelpAboutClick(Sender: TObject);
begin
  ShowMessage(
      'Vertex — HVIF Icon Editor' + LineEnding +
      'Built with the fpGUI toolkit.' + LineEnding + LineEnding +
      'Tools: Sel / Node / Pan (toolbox)' + LineEnding +
      'Zoom:  Ctrl+scroll or zoom toolbar' + LineEnding +
      'Pan:   scroll or Pan tool' + LineEnding +
      'Grid:  Grid/Snap buttons in toolbar',
      'About Vertex');
end;

end.
