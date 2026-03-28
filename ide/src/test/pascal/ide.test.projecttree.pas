{
    fpGUI IDE - Project Tree Tests

    Tests for pure functions in ide.projecttree: node file path resolution
    from tree structure without requiring filesystem or widgets.
}
unit ide.test.projecttree;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  fpg_tree,
  ide.projecttree,
  ide.project.pasbuild;

type

  { TTestResolveNodeFilePath }

  TTestResolveNodeFilePath = class(TTestCase)
  published
    procedure TestNilNode;
    procedure TestNonLeafNode;
    procedure TestLeafWithNoCategoryAncestor;
    procedure TestLeafUnderSourcesCategory;
    procedure TestLeafUnderTestsCategory;
    procedure TestLeafUnderResourcesCategory;
    procedure TestLeafUnderTestResourcesCategory;
    procedure TestLeafWithSubdirectory;
    procedure TestAggregatorModuleLeaf;
    procedure TestAggregatorModuleTestsCategory;
  end;

implementation

procedure TTestResolveNodeFilePath.TestNilNode;
begin
  AssertEquals('nil node should return empty', '',
    ResolveNodeFilePath(nil, '/proj/', 'src/main/pascal', False));
end;

procedure TTestResolveNodeFilePath.TestNonLeafNode;
var
  root, cat: TfpgTreeNode;
begin
  root := TfpgTreeNode.Create;
  try
    cat := root.AppendText('Sources');
    cat.Data := Pointer(1);
    { cat has a child, so cat is not a leaf }
    cat.AppendText('child.pas');
    AssertEquals('non-leaf node should return empty', '',
      ResolveNodeFilePath(cat, '/proj/', 'src/main/pascal', False));
  finally
    root.Free;
  end;
end;

procedure TTestResolveNodeFilePath.TestLeafWithNoCategoryAncestor;
var
  root, leaf: TfpgTreeNode;
begin
  root := TfpgTreeNode.Create;
  try
    { root has no Data, so no category ancestor found }
    leaf := root.AppendText('file.pas');
    AssertEquals('leaf with no category ancestor should return empty', '',
      ResolveNodeFilePath(leaf, '/proj/', 'src/main/pascal', False));
  finally
    root.Free;
  end;
end;

procedure TTestResolveNodeFilePath.TestLeafUnderSourcesCategory;
var
  root, cat, leaf: TfpgTreeNode;
begin
  root := TfpgTreeNode.Create;
  try
    cat := root.AppendText('Sources');
    cat.Data := Pointer(1);
    leaf := cat.AppendText('myunit.pas');
    AssertEquals('should resolve to source directory + filename',
      SetDirSeparators('/proj/src/main/pascal/myunit.pas'),
      ResolveNodeFilePath(leaf, '/proj/', 'src/main/pascal', False));
  finally
    root.Free;
  end;
end;

procedure TTestResolveNodeFilePath.TestLeafUnderTestsCategory;
var
  root, cat, leaf: TfpgTreeNode;
begin
  root := TfpgTreeNode.Create;
  try
    cat := root.AppendText('Tests');
    cat.Data := Pointer(2);
    leaf := cat.AppendText('testunit.pas');
    AssertEquals('should resolve to test directory + filename',
      SetDirSeparators('/proj/src/test/pascal/testunit.pas'),
      ResolveNodeFilePath(leaf, '/proj/', 'src/main/pascal', False));
  finally
    root.Free;
  end;
end;

procedure TTestResolveNodeFilePath.TestLeafUnderResourcesCategory;
var
  root, cat, leaf: TfpgTreeNode;
begin
  root := TfpgTreeNode.Create;
  try
    cat := root.AppendText('Resources');
    cat.Data := Pointer(3);
    leaf := cat.AppendText('config.ini');
    AssertEquals('should resolve to resources directory + filename',
      SetDirSeparators('/proj/src/main/resources/config.ini'),
      ResolveNodeFilePath(leaf, '/proj/', 'src/main/pascal', False));
  finally
    root.Free;
  end;
end;

procedure TTestResolveNodeFilePath.TestLeafUnderTestResourcesCategory;
var
  root, cat, leaf: TfpgTreeNode;
begin
  root := TfpgTreeNode.Create;
  try
    cat := root.AppendText('Test Resources');
    cat.Data := Pointer(4);
    leaf := cat.AppendText('testdata.txt');
    AssertEquals('should resolve to test resources directory + filename',
      SetDirSeparators('/proj/src/test/resources/testdata.txt'),
      ResolveNodeFilePath(leaf, '/proj/', 'src/main/pascal', False));
  finally
    root.Free;
  end;
end;

procedure TTestResolveNodeFilePath.TestLeafWithSubdirectory;
var
  root, cat, subdir, leaf: TfpgTreeNode;
begin
  root := TfpgTreeNode.Create;
  try
    cat := root.AppendText('Sources');
    cat.Data := Pointer(1);
    { Intermediate directory node — no Data }
    subdir := cat.AppendText('gui');
    leaf := subdir.AppendText('fpg_button.pas');
    AssertEquals('should include subdirectory in path',
      SetDirSeparators('/proj/src/main/pascal/gui/fpg_button.pas'),
      ResolveNodeFilePath(leaf, '/proj/', 'src/main/pascal', False));
  finally
    root.Free;
  end;
end;

procedure TTestResolveNodeFilePath.TestAggregatorModuleLeaf;
var
  root, modNode, cat, leaf: TfpgTreeNode;
  info: TAggregatorModuleInfo;
begin
  root := TfpgTreeNode.Create;
  info := TAggregatorModuleInfo.Create;
  try
    info.Name := 'fpgui-framework';
    info.ProjectDir := '/proj/framework/';
    info.SourceDirectory := 'src/main/pascal';

    modNode := root.AppendText('fpgui-framework');
    modNode.Data := info;

    cat := modNode.AppendText('Sources');
    cat.Data := Pointer(1);

    leaf := cat.AppendText('fpg_main.pas');
    AssertEquals('aggregator leaf should use module directory',
      SetDirSeparators('/proj/framework/src/main/pascal/fpg_main.pas'),
      ResolveNodeFilePath(leaf, '/proj/', 'src/main/pascal', True));
  finally
    info.Free;
    root.Free;
  end;
end;

procedure TTestResolveNodeFilePath.TestAggregatorModuleTestsCategory;
var
  root, modNode, cat, leaf: TfpgTreeNode;
  info: TAggregatorModuleInfo;
begin
  root := TfpgTreeNode.Create;
  info := TAggregatorModuleInfo.Create;
  try
    info.Name := 'fpgui-ide';
    info.ProjectDir := '/proj/ide/';
    info.SourceDirectory := 'src/main/pascal';

    modNode := root.AppendText('fpgui-ide');
    modNode.Data := info;

    cat := modNode.AppendText('Tests');
    cat.Data := Pointer(2);

    leaf := cat.AppendText('TestRunner.pas');
    AssertEquals('aggregator test leaf should use module test directory',
      SetDirSeparators('/proj/ide/src/test/pascal/TestRunner.pas'),
      ResolveNodeFilePath(leaf, '/proj/', 'src/main/pascal', True));
  finally
    info.Free;
    root.Free;
  end;
end;

initialization
  RegisterTest(TTestResolveNodeFilePath);

end.
