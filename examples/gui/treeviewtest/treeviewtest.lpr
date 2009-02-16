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
  fpg_imagelist;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    tree: TfpgTreeView;
    cbShowImages: TfpgCheckBox;
    cbIndentNode: TfpgCheckBox;
    btnClear: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    FImagelist: TfpgImageList;
    procedure   cbShowImagesChange(Sender: TObject);
    procedure   cbIndentNodeChange(Sender: TObject);
    procedure   btnClearClicked(Sender: TObject);
    procedure   PopulateTree;
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
  SetPosition(294, 174, 300, 250);
  WindowTitle := 'Treeview Test';
  WindowPosition := wpScreenCenter;

  tree := TfpgTreeView.Create(self);
  with tree do
  begin
    Name := 'tree';
    SetPosition(8, 8, 284, 184);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Label1';
    ScrollWheelDelta := 30;
    ShowImages := True;
    ImageList := FImagelist;
  end;

  cbShowImages := TfpgCheckBox.Create(self);
  with cbShowImages do
  begin
    Name := 'cbShowImages';
    SetPosition(8, 200, 109, 20);
    Anchors := [anLeft,anBottom];
    Checked := True;
    FontDesc := '#Label1';
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
    TabOrder := 2;
    Text := 'Indent node with no image';
    OnChange := @cbIndentNodeChange;
  end;

  btnClear := TfpgButton.Create(self);
  with btnClear do
  begin
    Name := 'btnClear';
    SetPosition(8, 222, 144, 23);
    Text := 'Clear Selected Node';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnClearClicked;
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


