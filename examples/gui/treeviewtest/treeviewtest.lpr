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
  gfxbase;

type
  TMainForm = class(TfpgForm)
  private
    tree: TfpgTreeView;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

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
  SetPosition(100, 100, 300, 200);
  
  tree := TfpgTreeView.Create(self);
  tree.SetPosition(8, 8, Width-16, Height-16);
  tree.Anchors := [anTop, anLeft, anRight, anBottom];
//  tree.ShowColumns := True;
//  tree.TreeLineStyle := lsSolid;
  tree.ScrollWheelDelta := 30;

  n := tree.RootNode.AppendText('Node 1');
  n.AppendText('Node 1.1');
  n.AppendText('Node 1.2');
  n := tree.RootNode.AppendText('Node 2');
  n.AppendText('Node 2.1');
  n := n.AppendText('Node 2.2 The quick brownfox jumps over the...');
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
  tree.RootNode.AppendText('Node 3');
  tree.Selection := n;
//  n := tree.RootNode.FindSubNode('Node 2.2.2', True);
  if Assigned(n2) then
  begin
    n2.AppendText('Child 1').AppendText('Child 2');
    n2.Collapsed := False;
  end;
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
  frm.Free;
end;

begin
  MainProc;
end.


