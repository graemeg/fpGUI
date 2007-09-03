program treeviewtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpgfx,
  gui_form,
  gui_tree;

type
  TMainForm = class(TfpgForm)
  private
    TV: TfpgTreeView;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
var
  n: TfpgTreeNode;
begin
  inherited Create(AOwner);
  WindowTitle := 'Treeview Test';
  WindowPosition := wpUser;
  SetPosition(100, 100, 300, 200);
  
  TV := TfpgTreeView.Create(self);
  TV.SetPosition(8, 8, 250, 180);
  TV.Align := alClient;
  TV.ShowColumns := True;
  n := TV.RootNode.AppendText('Node 1');
  n.AppendText('Node 1.1');
  n.AppendText('Node 1.2');
  n := TV.RootNode.AppendText('Node 2');
  n.AppendText('Node 2.1');
  n := n.AppendText('Node 2.2');
  n.AppendText('Node 2.2.1');
  TV.RootNode.FirstSubNode.Next.Collapse;
  TV.RootNode.AppendText('Node 3');
  TV.Selection := n;
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
end;

begin
  MainProc;
end.


