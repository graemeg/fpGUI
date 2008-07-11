unit treeview_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, gui_tree;

type

  TTestTreeview= class(TTestCase)
  private
    FTree: TfpgTreeview;
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure TestCount;
    procedure TestNext;
    procedure TestPrev;
  end; 

implementation

procedure TTestTreeview.SetUp;
begin
  FTree := TfpgTreeview.Create(nil);
end; 

procedure TTestTreeview.TearDown; 
begin
  FTree.Free;
end;

procedure TTestTreeview.TestCount;
var
  n: TfpgTreeNode;
begin
  AssertEquals('Failed on 1', 0, FTree.RootNode.Count);
  FTree.RootNode.AppendText('n1');
  AssertEquals('Failed on 2', 1, FTree.RootNode.Count);
  n := FTree.RootNode.AppendText('n2');
  AssertEquals('Failed on 3', 2, FTree.RootNode.Count);
  FTree.RootNode.Remove(n);
  AssertEquals('Failed on 4', 1, FTree.RootNode.Count);
end;

procedure TTestTreeview.TestNext;
var
  n: TfpgTreeNode;
begin
  AssertTrue('Failed on 1', FTree.RootNode.Next = nil);
  n := FTree.RootNode.AppendText('n1');
  AssertTrue('Failed on 2', FTree.RootNode.Next = n);
  AssertTrue('Failed on 3', n.Next = nil);
end;

procedure TTestTreeview.TestPrev;
begin
  raise Exception.Create('Implement this');
end;

initialization

  RegisterTest(TTestTreeview); 
end.

