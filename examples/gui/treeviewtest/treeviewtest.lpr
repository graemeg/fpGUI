program treeviewtest;

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
  fpg_tree,
  fpg_checkbox,
  fpg_button,
  fpg_imagelist,
  fpg_label,
  fpg_panel,
  fpg_dialogs,
  fpg_combobox;

type

  TMainForm = class(TfpgForm)
    procedure TestButtonClicked(Sender: TObject);
  private
    {@VFD_HEAD_BEGIN: MainForm}
    tree: TfpgTreeView;
    cbShowImages: TfpgCheckBox;
    cbIndentNode: TfpgCheckBox;
    btnClear: TfpgButton;
    Label1: TfpgLabel;
    Label2: TfpgLabel;
    Label3: TfpgLabel;
    Label4: TfpgLabel;
    Label5: TfpgLabel;
    Bevel1: TfpgBevel;
    lblSource: TfpgLabel;
    lblDestination: TfpgLabel;
    btnSource: TfpgButton;
    btnDest: TfpgButton;
    btnMoveTo: TfpgButton;
    cbMoveToTypes: TfpgComboBox;
    Button1: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    FImagelist: TfpgImageList;
    FSrcNode: TfpgTreeNode;
    FDestnode: TfpgTreeNode;
    procedure   cbShowImagesChange(Sender: TObject);
    procedure   cbIndentNodeChange(Sender: TObject);
    procedure   btnClearClicked(Sender: TObject);
    procedure   TreeNodeChanged(Sender: TObject);
    procedure   PopulateTree;
    procedure   btnSourceClicked(Sender: TObject);
    procedure   btnDestinationClicked(Sender: TObject);
    procedure   btnMoveToClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{ TMainForm }

procedure TMainForm.cbIndentNodeChange(Sender: TObject);
begin
  tree.IndentNodeWithNoImage := cbIndentNode.Checked;
end;

procedure TMainForm.btnClearClicked(Sender: TObject);
var
  n: TfpgTreeNode;
begin
  if tree.Selection <> nil then
  begin
    n := tree.Selection;
    tree.Selection.Clear;
    tree.Invalidate;
    tree.Selection := n;
  end;
end;

procedure TMainForm.TreeNodeChanged(Sender: TObject);
const
  cParent = 'Parent = %s';
  cPrev = 'Prev = %s';
  cNext = 'Next = %s';
  cFirstSubNode = 'FirstSubnode = %s';
  cLastSubNode = 'LastSubnode = %s';
var
  n: TfpgTreeNode;

  procedure PrintNodeInfo(ALabel: TfpgLabel; AFormat: string; ANode: TfpgTreeNode);
  begin
    if ANode = nil then
      ALabel.Text := Format(AFormat, ['nil'])
    else
      ALabel.Text := Format(AFormat, [ANode.Text]);
  end;

begin
  n := Tree.Selection;
  PrintNodeInfo(Label1, cParent, n.Parent);
  PrintNodeInfo(Label2, cPrev, n.Prev);
  PrintNodeInfo(Label3, cNext, n.Next);
  PrintNodeInfo(Label4, cFirstSubNode, n.FirstSubNode);
  PrintNodeInfo(Label5, cLastSubNode, n.LastSubNode);
end;

procedure TMainForm.PopulateTree;
var
  n: TfpgTreeNode;
  i: integer;
  n2: TfpgTreeNode;
  s: TfpgString;
begin
  n := tree.RootNode.AppendText('Node 1');

  n.ImageIndex := 0;
  n.AppendText('Node 1.1').ImageIndex := 1;
  n.AppendText('Node 1.2').ImageIndex := 1;
  n := tree.RootNode.AppendText('Node 2');
  n.ImageIndex := 0;
  n.AppendText('Node 2.1').ImageIndex := 1;
  n := n.AppendText('Node 2.2 The quick brownfox jumps over the...');
  n.ImageIndex := 1;
  for i := 1 to 3 do
  begin
    s := Format('Node 2.2.%d', [i]);
    if i = 2 then
      n2 := n.AppendText(s)
    else
      n.AppendText(s);
  end;
  n.Parent.AppendText('Node 2.3');
  tree.RootNode.FirstSubNode.Next.Collapse;
  tree.RootNode.AppendText('Node 3').ImageIndex := 0;
  tree.Selection := n;
//  n := tree.RootNode.FindSubNode('Node 2.2.2', True);
  if Assigned(n2) then
  begin
    n2.AppendText('Child 1').AppendText('Child 2');
    n2.Collapsed := False;
  end;

  TreeNodeChanged(nil);
end;

procedure TMainForm.btnSourceClicked(Sender: TObject);
begin
  FSrcNode := tree.Selection;
  if Assigned(FSrcNode) then
    lblSource.Text := FSrcNode.Text
  else
    lblSource.Text := '--';
end;

procedure TMainForm.btnDestinationClicked(Sender: TObject);
begin
  FDestNode := tree.Selection;
  if Assigned(FDestNode) then
    lblDestination.Text := FDestNode.Text
  else
    lblDestination.Text := '--';
end;

procedure TMainForm.btnMoveToClicked(Sender: TObject);
var
  i: integer;
begin
  if FSrcNode = FDestNode then
  begin
    ShowMessage('Source and Destination may not be the same');
    exit;
  end;
  i := cbMoveToTypes.FocusItem;
  FSrcNode.MoveTo(FDestnode, TfpgNodeAttachMode(i)); // This cast is a hack! Do not do this in real-world apps!!
  tree.FullExpand;
  tree.Invalidate;
  // reset values
  FSrcNode := nil;
  FDestnode := nil;
  lblSource.Text := '--';
  lblDestination.Text := '--';

  TreeNodeChanged(nil);
end;

procedure TMainForm.TestButtonClicked(Sender: TObject);
var
  n: TfpgTreeNode;
begin
  n := Tree.Selection;
  n.Parent.Remove(n);
  n.Free;
  Tree.Invalidate;
end;

procedure TMainForm.cbShowImagesChange(Sender: TObject);
begin
  tree.ShowImages := cbShowImages.Checked;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // create a image list
  FImagelist := TfpgImageList.Create;
  FImagelist.AddItemFromFile(SetDirSeparators('../../../images/folder_16.bmp'), 0);
  FImagelist.AddItemFromFile(SetDirSeparators('../../../images/menu_preferences_16.bmp'), 1);
  FImagelist.Item[1].Image.CreateMaskFromSample(0, 0);
  FImagelist.Item[1].Image.UpdateImage;
end;

destructor TMainForm.Destroy;
begin
  tree.ImageList := nil;
  FImagelist.Free;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  inherited AfterCreate;
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(294, 174, 709, 250);
  WindowTitle := 'Treeview Test';
  Hint := '';
  WindowPosition := wpScreenCenter;

  tree := TfpgTreeView.Create(self);
  with tree do
  begin
    Name := 'tree';
    SetPosition(8, 8, 284, 184);
    Anchors := [anLeft,anTop,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    ScrollWheelDelta := 30;
    ShowImages := True;
    TabOrder := 1;
    ImageList := FImagelist;
    OnChange  := @TreeNodeChanged;
  end;

  cbShowImages := TfpgCheckBox.Create(self);
  with cbShowImages do
  begin
    Name := 'cbShowImages';
    SetPosition(8, 200, 109, 20);
    Anchors := [anLeft,anBottom];
    Checked := True;
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 1;
    Text := 'Show images';
    OnChange := @cbShowImagesChange;
  end;

  cbIndentNode := TfpgCheckBox.Create(self);
  with cbIndentNode do
  begin
    Name := 'cbIndentNode';
    SetPosition(120, 200, 179, 20);
    Anchors := [anLeft,anBottom];
    Checked := True;
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 2;
    Text := 'Indent node with no image';
    OnChange := @cbIndentNodeChange;
  end;

  btnClear := TfpgButton.Create(self);
  with btnClear do
  begin
    Name := 'btnClear';
    SetPosition(8, 222, 144, 23);
    Anchors := [anLeft,anBottom];
    Text := 'Clear Selected Node';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnClearClicked;
  end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(312, 12, 292, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(312, 32, 288, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  Label3 := TfpgLabel.Create(self);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(312, 52, 272, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  Label4 := TfpgLabel.Create(self);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(312, 72, 300, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  Label5 := TfpgLabel.Create(self);
  with Label5 do
  begin
    Name := 'Label5';
    SetPosition(312, 92, 320, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  Bevel1 := TfpgBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(312, 116, 388, 12);
    Anchors := [anLeft,anRight,anTop];
    BorderStyle := bsDouble;
    Hint := '';
    Style := bsLowered;
    Shape := bsTopLine;
  end;

  lblSource := TfpgLabel.Create(self);
  with lblSource do
  begin
    Name := 'lblSource';
    SetPosition(312, 160, 140, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  lblDestination := TfpgLabel.Create(self);
  with lblDestination do
  begin
    Name := 'lblDestination';
    SetPosition(464, 160, 128, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  btnSource := TfpgButton.Create(self);
  with btnSource do
  begin
    Name := 'btnSource';
    SetPosition(312, 132, 80, 24);
    Text := 'Source';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 13;
    OnClick := @btnSourceClicked;
  end;

  btnDest := TfpgButton.Create(self);
  with btnDest do
  begin
    Name := 'btnDest';
    SetPosition(464, 132, 80, 24);
    Text := 'Destination';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 14;
    OnClick := @btnDestinationClicked;
  end;

  btnMoveTo := TfpgButton.Create(self);
  with btnMoveTo do
  begin
    Name := 'btnMoveTo';
    SetPosition(608, 132, 80, 24);
    Text := 'MoveTo';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 15;
    OnClick := @btnMoveToClicked;
  end;

  cbMoveToTypes := TfpgComboBox.Create(self);
  with cbMoveToTypes do
  begin
    Name := 'cbMoveToTypes';
    SetPosition(608, 164, 96, 22);
    FontDesc := '#List';
    Hint := '';
    Items.Add('naAdd');
    Items.Add('naAddFirst');
    Items.Add('naAddChild');
    Items.Add('naAddChildFirst');
    Items.Add('naInsert');
    TabOrder := 16;
    FocusItem := 0;
  end;

  Button1 := TfpgButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(472, 208, 80, 24);
    Text := 'Button';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 17;
    OnClick  := @TestButtonClicked;
  end;

  {@VFD_BODY_END: MainForm}

  PopulateTree;
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.


