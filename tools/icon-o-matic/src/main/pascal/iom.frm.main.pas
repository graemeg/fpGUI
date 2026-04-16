unit iom.frm.main;

{
  TIomMainForm — the top-level editor window.

  Step #4: viewer shell — load HVIF, render to canvas, populate object tree.
  Editing comes in later steps.

  Layout (MiG docking):
    ┌──────────────────────────────────────────────────┐
    │  File  Help                         [menu bar]   │  DockNorth
    ├─────────┬────────────────────────┬───────────────┤
    │ Tools   │                        │  Objects      │
    │ (60px)  │  Canvas (grows)        │  (220px)      │
    │ DockWest│  GrowX + GrowY         │  DockEast     │
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
  fpg_hvif_model, fpg_hvif,
  iom.wgt.canvas;


type
  TIomMainForm = class(TfpgForm)
  private
    { Widgets }
    FMenuBar:    TfpgMenuBar;
    FMnuFile:    TfpgPopupMenu;
    FMnuHelp:    TfpgPopupMenu;
    FToolBox:    TfpgBevel;
    FToolLabel:  TfpgLabel;
    FIomCanvas:     TIomCanvasWidget;
    FObjectTree: TfpgTreeView;

    { Data }
    FIcon: THvifIcon;     { owned; set after a successful open }

    { Setup helpers }
    procedure SetupMenus;
    procedure SetupLayout;

    { Object tree helpers }
    procedure PopulateObjectTree;
    procedure ClearObjectTree;

    { Menu handlers }
    procedure miFileOpenClick(Sender: TObject);
    procedure miFileExitClick(Sender: TObject);
    procedure miHelpAboutClick(Sender: TObject);

  public
    procedure AfterCreate; override;
    destructor Destroy; override;
  end;


implementation


{ ── TIomMainForm ─────────────────────────────────────────────────────────── }

destructor TIomMainForm.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;

procedure TIomMainForm.AfterCreate;
begin
  inherited AfterCreate;
  WindowTitle := 'Icon-O-Matic';
  SetPosition(80, 80, 960, 640);
  SetupMenus;
  SetupLayout;
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

  FMnuHelp := TfpgPopupMenu.Create(Self);
  with FMnuHelp do
  begin
    Name := 'mnuHelp';
    AddMenuItem('About...', '', @miHelpAboutClick);
  end;

  FMenuBar := TfpgMenuBar.Create(Self);
  FMenuBar.Name := 'menuBar';
  FMenuBar.AddMenuItem('&File', nil).SubMenu := FMnuFile;
  FMenuBar.AddMenuItem('&Help', nil).SubMenu := FMnuHelp;
end;

procedure TIomMainForm.SetupLayout;
var
  mig: TfpgMigLayoutManager;
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

  { Object tree — right panel, fixed 220px wide }
  FObjectTree := TfpgTreeView.Create(Self);
  FObjectTree.Name := 'objectTree';
  FObjectTree.PreferredSize := fpgSize(220, 200);
  mig.AddLayoutComponent(FObjectTree, TfpgMigCC.Create().DockEast);
end;


{ ── Object tree ──────────────────────────────────────────────────────────── }

procedure TIomMainForm.ClearObjectTree;
begin
  FObjectTree.BeginUpdate;
  FObjectTree.RootNode.Clear;
  FObjectTree.EndUpdate;
end;

procedure TIomMainForm.PopulateObjectTree;
var
  nStyles, nPaths, nShapes: TfpgTreeNode;
  n: TfpgTreeNode;
  i: Integer;
  st: THvifStyle;
  ph: THvifPath;
  sh: THvifShape;
  s: string;
begin
  if FIcon = nil then
    Exit;

  FObjectTree.BeginUpdate;
  try
    FObjectTree.RootNode.Clear;

    { ── Styles ── }
    nStyles := FObjectTree.RootNode.AppendText(
        Format('Styles (%d)', [FIcon.StyleCount]));
    nStyles.Expand;

    for i := 0 to FIcon.StyleCount - 1 do
    begin
      st := FIcon.Styles[i];
      case st.StyleType of
        hstSolidColor, hstSolidColorNoAlpha:
          s := Format('Style %d  solid #%2.2X%2.2X%2.2X A=%d',
               [i, st.Color.R, st.Color.G, st.Color.B, st.Color.A]);
        hstSolidGray, hstSolidGrayNoAlpha:
          s := Format('Style %d  gray K=%d', [i, st.Color.R]);
        hstGradient:
          s := Format('Style %d  gradient (%d stops)',
               [i, Length(st.Stops)]);
      else
        s := Format('Style %d', [i]);
      end;
      nStyles.AppendText(s);
    end;

    { ── Paths ── }
    nPaths := FObjectTree.RootNode.AppendText(
        Format('Paths (%d)', [FIcon.PathCount]));
    nPaths.Expand;

    for i := 0 to FIcon.PathCount - 1 do
    begin
      ph := FIcon.Paths[i];
      if ph.Closed then
        s := Format('Path %d  %d pts  closed', [i, Length(ph.Points)])
      else
        s := Format('Path %d  %d pts  open',   [i, Length(ph.Points)]);
      nPaths.AppendText(s);
    end;

    { ── Shapes ── }
    nShapes := FObjectTree.RootNode.AppendText(
        Format('Shapes (%d)', [FIcon.ShapeCount]));
    nShapes.Expand;

    for i := 0 to FIcon.ShapeCount - 1 do
    begin
      sh := FIcon.Shapes[i];
      s := Format('Shape %d  style=%d  paths=%d',
           [i, sh.StyleIndex, Length(sh.PathIndices)]);
      n := nShapes.AppendText(s);

      if sh.HasStroke then
        n.AppendText(Format('stroke  w=%.2f', [sh.StrokeWidth]));
      if sh.HasTransform then
        n.AppendText('has transform');
    end;

  finally
    FObjectTree.EndUpdate;
  end;
end;


{ ── Menu handlers ────────────────────────────────────────────────────────── }

procedure TIomMainForm.miFileOpenClick(Sender: TObject);
var
  fn:      string;
  newIcon: THvifIcon;
begin
  fn := SelectFileDialog(sfdOpen, 'HVIF files (*.hvif)|*.hvif|All files (*)|*', '');
  if fn = '' then
    Exit;

  try
    newIcon := THvifIcon.CreateFromFile(fn);
  except
    on E: Exception do
    begin
      ShowMessage('Open failed: ' + E.Message, 'Icon-O-Matic');
      Exit;
    end;
  end;

  FIcon.Free;
  FIcon := newIcon;

  FIomCanvas.LoadFromFile(fn);
  PopulateObjectTree;
  WindowTitle := 'Icon-O-Matic — ' + ExtractFileName(fn);
end;

procedure TIomMainForm.miFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TIomMainForm.miHelpAboutClick(Sender: TObject);
begin
  ShowMessage(
      'fpGUI Icon-O-Matic' + LineEnding +
      'HVIF icon editor for the fpGUI toolkit.' + LineEnding + LineEnding +
      'Step 4: Viewer shell',
      'About Icon-O-Matic');
end;

end.
