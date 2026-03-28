{
    fpGUI IDE - Build Dispatch Tests

    Tests for pure functions in ide.build.dispatch: module detection from
    tree nodes and build request construction.
}
unit ide.test.build.dispatch;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  fpg_tree,
  ide.build.dispatch,
  ide.project.pasbuild;

type

  { TTestDetectModuleFromTreeNode }

  TTestDetectModuleFromTreeNode = class(TTestCase)
  published
    procedure TestNilNode;
    procedure TestNodeWithNilData;
    procedure TestNodeWithCategoryTag;
    procedure TestNodeWithModuleInfo;
  end;

implementation

{ TTestDetectModuleFromTreeNode }

procedure TTestDetectModuleFromTreeNode.TestNilNode;
begin
  AssertEquals('nil node should return empty', '', DetectModuleFromTreeNode(nil));
end;

procedure TTestDetectModuleFromTreeNode.TestNodeWithNilData;
var
  n: TfpgTreeNode;
begin
  n := TfpgTreeNode.Create;
  try
    AssertEquals('node with nil data should return empty', '',
      DetectModuleFromTreeNode(n));
  finally
    n.Free;
  end;
end;

procedure TTestDetectModuleFromTreeNode.TestNodeWithCategoryTag;
var
  n: TfpgTreeNode;
begin
  n := TfpgTreeNode.Create;
  try
    n.Data := Pointer(PtrInt(2));  { category tag }
    AssertEquals('category tag node should return empty', '',
      DetectModuleFromTreeNode(n));
  finally
    n.Free;
  end;
end;

procedure TTestDetectModuleFromTreeNode.TestNodeWithModuleInfo;
var
  n: TfpgTreeNode;
  info: TAggregatorModuleInfo;
begin
  n := TfpgTreeNode.Create;
  info := TAggregatorModuleInfo.Create;
  try
    info.Name := 'fpgui-framework';
    n.Data := info;
    AssertEquals('module info node should return module name',
      'fpgui-framework', DetectModuleFromTreeNode(n));
  finally
    info.Free;
    n.Free;
  end;
end;

initialization
  RegisterTest(TTestDetectModuleFromTreeNode);

end.
