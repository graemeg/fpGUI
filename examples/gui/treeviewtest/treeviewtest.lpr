program treeviewtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  fpgfx,
  gui_form,
  gui_tree,
  gui_checkbox,
  gfxbase,
  gfx_imagelist;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    tree: TfpgTreeView;
    imagelist: TfpgImageList;
    cbShowImages: TfpgCheckBox;
    cbIndentNode: TfpgCheckBox;
    procedure   cbShowImagesChange(Sender: TObject);
    procedure   cbIndentNodeChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

{ TMainForm }

procedure TMainForm.cbIndentNodeChange(Sender: TObject);
begin
  tree.IndentNodeWithNoImage := cbIndentNode.Checked;
end;

procedure TMainForm.cbShowImagesChange(Sender: TObject);
begin
  tree.ShowImages := cbShowImages.Checked;
end;

constructor TMainForm.Create(AOwner: TComponent);
var
  n: TfpgTreeNode;
  n2: TfpgTreeNode;
  i: integer;
  s: string;
begin
  inherited Create(AOwner);
  WindowTitle := 'Treeview Test';
  WindowPosition := wpUser;
  SetPosition(100, 100, 300, 230);

  // create a image list
  imagelist := TfpgImageList.Create;
  imagelist.AddItemFromFile(SetDirSeparators('../../../images/folder_16.bmp'), 0);
  imagelist.AddItemFromFile(SetDirSeparators('../../../images/menu_preferences_16.bmp'), 1);

  // create a treeview
  tree := TfpgTreeView.Create(self);
  tree.SetPosition(8, 8, Width-16, 200-16);
  tree.Anchors := [anTop, anLeft, anRight, anBottom];
//  tree.ShowColumns := True;
//  tree.TreeLineStyle := lsSolid;
  tree.ScrollWheelDelta := 30;
  tree.ImageList := imagelist;
  tree.ShowImages := True;

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
  
  // create a checkbox
  cbShowImages := CreateCheckBox(self, 8, 204, 'Show images');
  cbShowImages.Checked := True;
  cbShowImages.OnChange := @cbShowImagesChange;
  cbShowImages.Anchors := [anLeft, anBottom];
  
  // create a checkbox
  cbIndentNode := CreateCheckBox(self, 120, 204, 'Indent node with no image');
  cbIndentNode.Checked := True;
  cbIndentNode.OnChange := @cbIndentNodeChange;
  cbIndentNode.Anchors := [anLeft, anBottom];
end;

destructor TMainForm.Destroy;
begin
  tree.ImageList := nil;
  imagelist.Free;
  inherited Destroy;
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


