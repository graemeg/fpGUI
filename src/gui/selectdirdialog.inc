{%mainunit fpg_dialogs.pas}

{$IFDEF read_interface}


  { TfpgSelectDirDialog }

  TfpgSelectDirDialog = class(TfpgBaseDialog)
  private
    tv: TfpgTreeView;
    FRootDir: TfpgString;
    FShowHidden: Boolean;
    function    GetAbsolutePath(Node: TfpgTreeNode): TfpgString;
    procedure   InitializeTreeview;
    procedure   SetRootDir(const AValue: TfpgString);
    procedure   AddDirectories(Node: TfpgTreeNode; Dir: TfpgString);
    procedure   NodeExpanded(Sender: TObject; ANode: TfpgTreeNode);
    {$IFDEF MSWINDOWS}
    procedure   AddWindowsDriveLetters;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
    { return the selected directory }
    function    SelectedDir: TfpgString;
    { Directory the treeview starts from }
    property    RootDirectory: TfpgString read FRootDir write SetRootDir;
  end;


{$ENDIF read_interface}



{$IFDEF read_implementation}

function TfpgSelectDirDialog.GetAbsolutePath(Node: TfpgTreeNode): TfpgString;
begin
  Result := '';
  while Node <> nil do
  begin
    if Node.Text = PathDelim then
      Result := Node.Text + Result
    else
      Result := Node.Text + PathDelim + Result;
    Node := Node.Parent;
  end;
end;

procedure TfpgSelectDirDialog.InitializeTreeview;
begin
  { I'm not sure what we should set these to. Maybe another Config option? }
  {$IFDEF UNIX}
  RootDirectory := '/';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  RootDirectory := 'C:\';
  {$ENDIF}
  tv.RootNode.Expand;
end;

procedure TfpgSelectDirDialog.SetRootDir(const AValue: TfpgString);
var
  RootNode: TfpgTreeNode;
  lNode: TfpgTreeNode;
begin
  { Clear the list }
  tv.RootNode.Clear;
  FRootDir := AValue;

  {$IFDEF MSWINDOWS}
  { Add Windows drive letters }
  AddWindowsDriveLetters;
  {$ENDIF}

  { Remove the path delimeter unless this is root. }
  if FRootDir = '' then
    FRootDir := PathDelim;
  if (FRootDir <> PathDelim) and (FRootDir[length(FRootDir)] = PathDelim) then
    FRootDir := copy(FRootDir, 1, length(FRootDir) - 1);
  { Find or Create the root node and add it to the Tree View. }
  RootNode := tv.RootNode.FindSubNode(FRootDir + PathDelim, False);
//  RootNode := TV.Items.FindTopLvlNode(FRootDir + PathDelim);
  if RootNode = nil then
//    RootNode := TV.Items.Add(nil, FRootDir);
    RootNode := tv.RootNode.AppendText(FRootDir);

  { Add the Subdirectories to Root nodes }
//  lNode := TV.Items.GetFirstNode;
  lNode := RootNode;
  while lNode <> nil do
  begin
    AddDirectories(lNode, lNode.Text);
    lNode := lNode.Next;
//    lNode := lNode.GetNextSibling;
  end;

  { Set the original root node as the selected node. }
  tv.Selection := RootNode;
end;

{ Adds Subdirectories to a passed node if they exist }
procedure TfpgSelectDirDialog.AddDirectories(Node: TfpgTreeNode; Dir: TfpgString);
var
  FileInfo: TSearchRec;
  NewNode: TfpgTreeNode;
  i: integer;
  FCurrentDir: TfpgString;
  //used to sort the directories.
  SortList: TStringList;
begin
  if Dir <> '' then
  begin
    FCurrentDir := Dir;
    FCurrentDir := fpgAppendPathDelim(FCurrentDir);
    i           := length(FCurrentDir);
    FCurrentDir := FCurrentDir + AllFilesMask;
    try
      if fpgFindFirst(FCurrentDir, faAnyFile or $00000080, FileInfo) = 0 then
      begin
        try
          SortList        := TStringList.Create;
          SortList.Sorted := True;
          repeat
            // check if special file
            if (FileInfo.Name = '.') or (FileInfo.Name = '..') or (FileInfo.Name = '') then
              Continue;
            { If hidden files or directories must be filtered, we test for
              dot files, considered hidden under unix type OS's. }
            //if not FShowHidden then
              //if (FileInfo.Name[1] in ['.']) then
                //Continue;

            { if this is a directory then add it to the tree. }
            if ((faDirectory and FileInfo.Attr) > 0) then
            begin
              { If this is a hidden file and we have not been requested to show
                hidden files then do not add it to the list. }
              //if ((faHidden and FileInfo.Attr) > 0) and not FShowHidden then
                //continue;

              SortList.Add(FileInfo.Name);
            end;
          until fpgFindNext(FileInfo) <> 0;
          for i := 0 to SortList.Count - 1 do
          begin
            NewNode := Node.AppendText(SortList[i]);
//            NewNode := TV.Items.AddChild(Node, SortList[i]);
            // if subdirectories then indicate so.
{ Todo: Fix this by adding HasChildren to Treeview }
            NewNode.HasChildren := fpgHasSubDirs(fpgAppendPathDelim(Dir) + NewNode.Text, FShowHidden);
          end;
        finally
          SortList.Free;
        end;
      end;  { if FindFirst... }
    finally
      SysUtils.FindClose(FileInfo);
    end;
  end;  { if Dir... }
  //if Node.Level = 0 then
    //Node.Text := Dir;
end;

procedure TfpgSelectDirDialog.NodeExpanded(Sender: TObject; ANode: TfpgTreeNode);
begin
  if ANode.Count = 0 then
    AddDirectories(ANode, GetAbsolutePath(ANode));
end;

{$IFDEF MSWINDOWS}
procedure TfpgSelectDirDialog.AddWindowsDriveLetters;
const
  MAX_DRIVES = 25;
var
  n: integer;
  drvs: string;
begin
  // making drive list, skipping drives A: and B:
  n := 2;
  while n <= MAX_DRIVES do
  begin
    drvs := chr(n + Ord('A')) + ':\';
    if Windows.GetDriveType(PChar(drvs)) <> 1 then
      TV.RootNode.AppendText(drvs);
    Inc(n);
  end;
end;
{$ENDIF}

constructor TfpgSelectDirDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowHidden := False;
end;

procedure TfpgSelectDirDialog.AfterCreate;
begin
  inherited AfterCreate;
  Name := 'fpgSelectDirDialog';
  SetPosition(20, 20, 300, 370);
  WindowTitle := 'Select a Directory';    { TODO : Localize this!! }
  WindowPosition := wpScreenCenter;

  tv := TfpgTreeView.Create(self);
  with tv do
  begin
    Name := 'tv';
    SetPosition(FSpacing, FSpacing, 288, 322);
    OnExpand :=@NodeExpanded;
  end;

  // reposition buttons
  btnCancel.Left  := Width-FDefaultButtonWidth-FSpacing;
  btnCancel.Top   := Height - FSpacing - btnCancel.Height;
  btnOK.Left      := btnCancel.Left-FDefaultButtonWidth-FSpacing;
  btnOK.Top       := btnCancel.Top;

  // now reset tab order
  tv.TabOrder         := 1;
  btnOK.TabOrder      := 2;
  btnCancel.TabOrder  := 3;

  InitializeTreeview;
end;

function TfpgSelectDirDialog.SelectedDir: TfpgString;
begin
  Result := '';
  if tv.Selection <> nil then
    Result := GetAbsolutePath(tv.Selection);
end;

{$ENDIF read_implementation}


