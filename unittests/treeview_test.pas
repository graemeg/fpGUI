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
    procedure TestCountRecursive;
    procedure TestNext;
    procedure TestPrev;
    procedure TestAppendText;
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

procedure TTestTreeview.TestCountRecursive;
var
  n: TfpgTreeNode;
begin
  AssertEquals('Failed on 1', 0, FTree.RootNode.CountRecursive);
  FTree.RootNode.AppendText('n1');
  AssertEquals('Failed on 2', 1, FTree.RootNode.CountRecursive);
  n := FTree.RootNode.AppendText('n2');
  AssertEquals('Failed on 3', 2, FTree.RootNode.CountRecursive);
  n.AppendText('n2.1');
  AssertEquals('Failed on 4', 3, FTree.RootNode.CountRecursive);
  n.AppendText('n2.2');
  AssertEquals('Failed on 5', 4, FTree.RootNode.CountRecursive);
//  FTree.RootNode.Remove(n);
//  AssertEquals('Failed on 6', 1, FTree.RootNode.Count);
end;

procedure TTestTreeview.TestNext;
var
  n1, n2: TfpgTreeNode;
begin
  AssertTrue('Failed on 1', FTree.RootNode.Next = nil);
  n1 := FTree.RootNode.AppendText('n1');
  n2 := FTree.RootNode.AppendText('n2');
  AssertTrue('Failed on 2', n1.Next = n2);
  AssertTrue('Failed on 3', n2.Next = nil);
  n1.AppendText('n1.1');
  AssertTrue('Failed on 4', n1.Next = n2);
end;

procedure TTestTreeview.TestPrev;
var
  n1, n2: TfpgTreeNode;
begin
  AssertTrue('Failed on 1', FTree.RootNode.Prev = nil);
  n1 := FTree.RootNode.AppendText('n1');
  n2 := FTree.RootNode.AppendText('n2');
  AssertTrue('Failed on 2', n2.Prev = n1);
  AssertTrue('Failed on 3', n1.Prev = nil);
  n1.AppendText('n1.1');
  AssertTrue('Failed on 4', n2.Prev = n1);
end;

procedure TTestTreeview.TestAppendText;
var
  n1, n2: TfpgTreeNode;
begin
  n1 := FTree.RootNode.AppendText('n1');
  AssertTrue('Failed on 1', FTree.RootNode.FirstSubNode = n1);
  AssertEquals('Failed on 2', 1, FTree.RootNode.Count);
  n2 := FTree.RootNode.AppendText('n2');
  AssertEquals('Failed on 3', 2, FTree.RootNode.Count);
end;

initialization

  RegisterTest(TTestTreeview); 
end.

