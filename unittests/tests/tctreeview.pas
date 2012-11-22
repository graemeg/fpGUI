unit tcTreeview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpg_tree;

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
    procedure TestFindSubNode_Text;
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
  n1, n2: TfpgTreeNode;
begin
  { root
      |--n1
      |  |--n1.1
      |--n2
         |--n2.1
         |--n2.2
  }
  AssertEquals('Failed on 1', 0, FTree.RootNode.CountRecursive);
  n1 := FTree.RootNode.AppendText('n1');
  AssertEquals('Failed on 2', 1, FTree.RootNode.CountRecursive);
  n2 := FTree.RootNode.AppendText('n2');
  AssertEquals('Failed on 3', 2, FTree.RootNode.CountRecursive);
  n2.AppendText('n2.1');
  AssertEquals('Failed on 4', 3, FTree.RootNode.CountRecursive);
  n2.AppendText('n2.2');
  AssertEquals('Failed on 5', 4, FTree.RootNode.CountRecursive);
  n1.AppendText('n1.1');
  AssertEquals('Failed on 6', 5, FTree.RootNode.CountRecursive);
  FTree.RootNode.Remove(n2); // removing 3 nodes
  AssertEquals('Failed on 6', 2, FTree.RootNode.CountRecursive);
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

procedure TTestTreeview.TestFindSubNode_Text;
var
  n1, n2, n3: TfpgTreeNode;
  r: TfpgTreeNode;
begin
  { root
      |--n1
      |  |--n1.1
      |
      |--n2
      |  |--n2.1
      |  |  |--n2.1.1
      |  |
      |  |--n2.2
      |
      |--n3
  }
  AssertTrue('Failed on 1', FTree.RootNode.FindSubNode('n1', False) = nil);

  n1 := FTree.RootNode.AppendText('n1');
  AssertTrue('Failed on 2', FTree.RootNode.FindSubNode('n1', False) = n1);
  AssertTrue('Failed on 3', FTree.RootNode.FindSubNode('n1', True) = n1);

  n2 := FTree.RootNode.AppendText('n2');
  AssertTrue('Failed on 4', FTree.RootNode.FindSubNode('n2', False) = n2);
  AssertTrue('Failed on 5', FTree.RootNode.FindSubNode('n2', True) = n2);

  r := n2.AppendText('n2.1');
  AssertTrue('Failed on 6', FTree.RootNode.FindSubNode('n2.1', False) = nil);
  AssertTrue('Failed on 7', FTree.RootNode.FindSubNode('n2.1', True) = r);

  r := n2.AppendText('n2.2');
  AssertTrue('Failed on 8', FTree.RootNode.FindSubNode('n2.2', False) = nil);
  AssertTrue('Failed on 9', FTree.RootNode.FindSubNode('n2.2', True) = r);
  
  n3 := FTree.RootNode.AppendText('n3');
  AssertTrue('Failed on 10', FTree.RootNode.FindSubNode('n3', False) = n3);
  AssertTrue('Failed on 11', FTree.RootNode.FindSubNode('n3', True) = n3);

  r := n1.AppendText('n1.1');
  AssertTrue('Failed on 12', FTree.RootNode.FindSubNode('n1.1', False) = nil);
  AssertTrue('Failed on 13', FTree.RootNode.FindSubNode('n1.1', True) = r);

  r := n2.FindSubNode('n2.1', True).AppendText('n2.1.1');
  AssertTrue('Failed on 14', FTree.RootNode.FindSubNode('n2.1.1', False) = nil);
  AssertTrue('Failed on 15', FTree.RootNode.FindSubNode('n2.1.1', True) = r);
end;


initialization
  RegisterTest(TTestTreeview);
  
end.

