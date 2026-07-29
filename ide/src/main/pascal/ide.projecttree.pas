{
    fpGUI IDE - Project Tree

    Copyright (C) 2006 - 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Extracted from ide.form.main.pas — separates project tree logic
      from the main form.

      ResolveNodeFilePath is a pure function: given a leaf tree node,
      it walks up the tree to find the category ancestor (Sources/Tests/
      Resources) and optionally the module ancestor, then assembles the
      full file path.

      AddDirectoryToTree and AddModuleSubtree build tree nodes from the
      filesystem — used by the form's PopulatePasBuildTree.
}
unit ide.projecttree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_tree, fpg_utils,
  ide.project.pasbuild;

{ Pure function: given a leaf node in a PasBuild project tree, resolve
  the full file path by walking up to find the category ancestor and
  optional module ancestor.

  AProjectDir: project root directory (with trailing separator)
  ASourceDir: source directory relative to project (e.g. 'src/main/pascal')
  AIsAggregator: whether the project is an aggregator (look for module info nodes)

  Returns '' if the node is nil, not a leaf, or has no valid category ancestor. }
function ResolveNodeFilePath(
  ANode: TfpgTreeNode;
  const AProjectDir, ASourceDir: TfpgString;
  AIsAggregator: Boolean
): TfpgString;

{ Inverse of ResolveNodeFilePath: given a full file path, find the leaf node
  that represents it. Walks the whole tree under ARoot and resolves each leaf
  with ResolveNodeFilePath, so the two directions can never disagree about how
  a path is assembled.

  Comparison is case-insensitive on Windows and case-sensitive elsewhere.

  Returns nil when no node matches — the file may simply not be part of the
  project tree (a dependency, or a file opened from outside the project). }
function FindNodeForFilePath(
  ARoot: TfpgTreeNode;
  const AFilePath, AProjectDir, ASourceDir: TfpgString;
  AIsAggregator: Boolean
): TfpgTreeNode;

{ Recursively adds directory contents to a tree node.
  AExtensions: list of file extensions to include (e.g. '.pas', '.inc').
  Pass nil or empty list to include all files. }
procedure AddDirectoryToTree(
  AParent: TfpgTreeNode;
  const ADir: TfpgString;
  const AExtensions: TStringList
);

{ Adds Sources/Tests/Resources/Dependencies subtree under a parent node. }
procedure AddModuleSubtree(
  AParent: TfpgTreeNode;
  const AModuleDir, ASourceDir: TfpgString;
  ADeps: TStringList;
  AExpandSources: Boolean
);

{ Recursively adds module info nodes for aggregator projects. }
procedure AddModuleInfoToTree(
  AParent: TfpgTreeNode;
  AInfo: TAggregatorModuleInfo
);


implementation

uses
  fpg_main;

{ -------------------------------------------------------------------------- }
{ ResolveNodeFilePath                                                        }
{ -------------------------------------------------------------------------- }

function ResolveNodeFilePath(
  ANode: TfpgTreeNode;
  const AProjectDir, ASourceDir: TfpgString;
  AIsAggregator: Boolean
): TfpgString;
var
  Cat: TfpgTreeNode;
  ModParent: TfpgTreeNode;
  ModInfo: TAggregatorModuleInfo;
  DirPath: TfpgString;
  RelPath: TfpgString;
begin
  Result := '';

  if ANode = nil then
    Exit;

  { Must be a leaf node with a parent }
  if (ANode.Parent = nil) or (ANode.Count > 0) then
    Exit;

  { Build relative sub-path from intermediate directory nodes
    (nodes with nil Data between the leaf and the category node) }
  RelPath := '';
  Cat := ANode.Parent;
  while (Cat <> nil) and (Cat.Data = nil) do
  begin
    RelPath := Cat.Text + PathDelim + RelPath;
    Cat := Cat.Parent;
  end;

  { Cat must be a category node with Data tag 1-4 }
  if (Cat = nil) or (Cat.Data = nil) then
    Exit;
  if (PtrInt(Cat.Data) < 1) or (PtrInt(Cat.Data) > 4) then
    Exit;

  { For aggregator projects, walk further up to find the module node }
  ModInfo := nil;
  if AIsAggregator then
  begin
    ModParent := Cat.Parent;
    while (ModParent <> nil) and (ModParent.Data = nil) do
      ModParent := ModParent.Parent;
    if (ModParent <> nil) and (ModParent.Data <> nil) and
       (PtrInt(ModParent.Data) > 4) then
      ModInfo := TAggregatorModuleInfo(ModParent.Data);
  end;

  if Assigned(ModInfo) then
  begin
    { Aggregator module — use module's directory and source dir }
    case PtrInt(Cat.Data) of
      1: DirPath := SetDirSeparators(ModInfo.SourceDirectory + '/');
      2: DirPath := SetDirSeparators('src/test/pascal/');
      3: DirPath := SetDirSeparators('src/main/resources/');
      4: DirPath := SetDirSeparators('src/test/resources/');
    end;
    Result := ModInfo.ProjectDir + DirPath + RelPath + ANode.Text;
  end
  else
  begin
    { Single module — use project directory }
    case PtrInt(Cat.Data) of
      1: DirPath := SetDirSeparators(ASourceDir + '/');
      2: DirPath := SetDirSeparators('src/test/pascal/');
      3: DirPath := SetDirSeparators('src/main/resources/');
      4: DirPath := SetDirSeparators('src/test/resources/');
    else
      DirPath := '';
    end;
    if DirPath = '' then
      Exit;
    Result := AProjectDir + DirPath + RelPath + ANode.Text;
  end;
end;

{ -------------------------------------------------------------------------- }
{ FindNodeForFilePath                                                        }
{ -------------------------------------------------------------------------- }

function FindNodeForFilePath(
  ARoot: TfpgTreeNode;
  const AFilePath, AProjectDir, ASourceDir: TfpgString;
  AIsAggregator: Boolean
): TfpgTreeNode;
var
  Target: TfpgString;

  function SamePath(const A, B: TfpgString): Boolean;
  begin
    {$IFDEF MSWINDOWS}
    Result := SameText(A, B);
    {$ELSE}
    Result := A = B;
    {$ENDIF}
  end;

  function Search(ANode: TfpgTreeNode): TfpgTreeNode;
  var
    Child: TfpgTreeNode;
  begin
    Result := nil;
    if ANode = nil then
      Exit;

    Child := ANode.FirstSubNode;
    while Child <> nil do
    begin
      if Child.Count > 0 then
        Result := Search(Child)   { directory or category node — descend }
      else if SamePath(ResolveNodeFilePath(Child, AProjectDir, ASourceDir,
                         AIsAggregator), Target) then
        Result := Child;

      if Result <> nil then
        Exit; //==>

      Child := Child.Next;
    end;
  end;

begin
  Result := nil;
  if (ARoot = nil) or (AFilePath = '') then
    Exit;

  Target := SetDirSeparators(AFilePath);
  Result := Search(ARoot);
end;

{ -------------------------------------------------------------------------- }
{ AddDirectoryToTree                                                         }
{ -------------------------------------------------------------------------- }

procedure AddDirectoryToTree(
  AParent: TfpgTreeNode;
  const ADir: TfpgString;
  const AExtensions: TStringList
);
var
  sr: TSearchRec;
  Files: TStringList;
  Dirs: TStringList;
  SubNode: TfpgTreeNode;
  Ext: TfpgString;
  i: integer;
  MatchAll: Boolean;

  function IsExcludedDir(const AName: TfpgString): Boolean;
  begin
    Result := (AName = '.') or (AName = '..') or
              (AName = '.ide') or (AName = 'target') or
              (AName = 'units');
  end;

  function MatchesExtension(const AName: TfpgString): Boolean;
  var
    j: integer;
  begin
    if MatchAll then
    begin
      Result := True;
      Exit;
    end;
    Ext := LowerCase(fpgExtractFileExt(AName));
    for j := 0 to AExtensions.Count - 1 do
    begin
      if Ext = AExtensions[j] then
      begin
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;

begin
  MatchAll := (AExtensions = nil) or (AExtensions.Count = 0);
  Files := TStringList.Create;
  Dirs := TStringList.Create;
  try
    Files.Sorted := True;
    Dirs.Sorted := True;

    { Single scan: collect directories and matching files }
    if fpgFindFirst(ADir + AllFilesMask, faAnyFile, sr) = 0 then
    begin
      repeat
        if (sr.Attr and faDirectory) <> 0 then
        begin
          if not IsExcludedDir(sr.Name) then
            Dirs.Add(sr.Name);
        end
        else if MatchesExtension(sr.Name) then
          Files.Add(sr.Name);
      until fpgFindNext(sr) <> 0;
      FindClose(sr);
    end;

    { Add subdirectories first, then recurse }
    for i := 0 to Dirs.Count - 1 do
    begin
      SubNode := AParent.AppendText(Dirs[i]);
      SubNode.TextColor := clText1;
      AddDirectoryToTree(SubNode,
        IncludeTrailingPathDelimiter(ADir + Dirs[i]), AExtensions);
    end;

    { Add files }
    for i := 0 to Files.Count - 1 do
      AParent.AppendText(Files[i]).TextColor := clText1;
  finally
    Dirs.Free;
    Files.Free;
  end;
end;

{ -------------------------------------------------------------------------- }
{ AddModuleSubtree                                                           }
{ -------------------------------------------------------------------------- }

procedure AddModuleSubtree(
  AParent: TfpgTreeNode;
  const AModuleDir, ASourceDir: TfpgString;
  ADeps: TStringList;
  AExpandSources: Boolean
);
var
  DirNode: TfpgTreeNode;
  DepNode: TfpgTreeNode;
  SourceExts: TStringList;
  SrcDir: TfpgString;
  i: integer;
begin
  SourceExts := TStringList.Create;
  try
    SourceExts.Add('.pas');
    SourceExts.Add('.pp');
    SourceExts.Add('.lpr');
    SourceExts.Add('.dpr');
    SourceExts.Add('.inc');

    { Sources }
    SrcDir := AModuleDir + SetDirSeparators(ASourceDir + '/');
    if fpgDirectoryExists(SrcDir) then
    begin
      DirNode := AParent.AppendText('Sources');
      DirNode.TextColor := clText2;
      DirNode.Data := Pointer(1);
      AddDirectoryToTree(DirNode, SrcDir, SourceExts);
      if AExpandSources then
        DirNode.Expand;
    end;

    { Tests }
    SrcDir := AModuleDir + SetDirSeparators('src/test/pascal/');
    if fpgDirectoryExists(SrcDir) then
    begin
      DirNode := AParent.AppendText('Tests');
      DirNode.TextColor := clText2;
      DirNode.Data := Pointer(2);
      AddDirectoryToTree(DirNode, SrcDir, SourceExts);
    end;
  finally
    SourceExts.Free;
  end;

  { Resources }
  SrcDir := AModuleDir + SetDirSeparators('src/main/resources/');
  if fpgDirectoryExists(SrcDir) then
  begin
    DirNode := AParent.AppendText('Resources');
    DirNode.TextColor := clText2;
    DirNode.Data := Pointer(3);
    AddDirectoryToTree(DirNode, SrcDir, nil);
  end;

  { Test Resources }
  SrcDir := AModuleDir + SetDirSeparators('src/test/resources/');
  if fpgDirectoryExists(SrcDir) then
  begin
    DirNode := AParent.AppendText('Test Resources');
    DirNode.TextColor := clText2;
    DirNode.Data := Pointer(4);
    AddDirectoryToTree(DirNode, SrcDir, nil);
  end;

  { Dependencies }
  if Assigned(ADeps) and (ADeps.Count > 0) then
  begin
    DepNode := AParent.AppendText('Dependencies');
    DepNode.TextColor := clText2;
    for i := 0 to ADeps.Count - 1 do
      DepNode.AppendText(ADeps[i]).TextColor := clText1;
  end;
end;

{ -------------------------------------------------------------------------- }
{ AddModuleInfoToTree                                                        }
{ -------------------------------------------------------------------------- }

procedure AddModuleInfoToTree(
  AParent: TfpgTreeNode;
  AInfo: TAggregatorModuleInfo
);
var
  ModNode: TfpgTreeNode;
  ModLabel: TfpgString;
  i: integer;
begin
  if AInfo.Version <> '' then
    ModLabel := AInfo.Name + ' (' + AInfo.Version + ')'
  else
    ModLabel := AInfo.Name;

  ModNode := AParent.AppendText(ModLabel);
  ModNode.TextColor := clText2;
  ModNode.Data := Pointer(AInfo);

  if AInfo.IsAggregator then
  begin
    { Nested aggregator — show its sub-modules recursively }
    for i := 0 to AInfo.SubModules.Count - 1 do
      AddModuleInfoToTree(ModNode, TAggregatorModuleInfo(AInfo.SubModules[i]));
  end
  else
  begin
    { Leaf module — show Sources/Tests/Resources/Dependencies }
    AddModuleSubtree(ModNode, AInfo.ProjectDir, AInfo.SourceDirectory,
      AInfo.DeclaredDeps, False);
  end;
end;

end.
