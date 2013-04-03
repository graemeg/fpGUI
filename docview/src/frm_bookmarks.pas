unit frm_bookmarks;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_listbox,
  fpg_button,
  HelpBookmark;

type
  TBookmarkCallback = procedure(Bookmark: TBookmark) of object;

  TBookmarksForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: BookmarksForm}
    lbBookmarks: TfpgListBox;
    btnRename: TfpgButton;
    btnDelete: TfpgButton;
    btnGoTo: TfpgButton;
    btnHelp: TfpgButton;
    btnClose: TfpgButton;
    {@VFD_HEAD_END: BookmarksForm}
    FBookmarkList: TList;
    FOnBookmarksChanged: TNotifyEvent;
    FOnGotoBookmark: TBookmarkCallback;
    procedure lbBookmarksKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure FormShow(Sender: TObject);
    procedure lbBookmarksDoubleClicked(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure btnRenameClicked(Sender: TObject);
    procedure btnDeleteClicked(Sender: TObject);
    procedure btnGotoClicked(Sender: TObject);
    procedure btnHelpClicked(Sender: TObject);
    procedure btnCloseClicked(Sender: TObject);
    function SelectedObject(ListBox: TfpgListBox): TObject;
    procedure UpdateControls;
    function GetSelectedBookmark: TBookmark;
    procedure GotoSelectedBookmark;
  public
    procedure AfterCreate; override;
    procedure RefreshList;
  published
    property BookmarkList: TList read FBookmarkList write FBookmarkList;
    property OnBookmarksChanged: TNotifyEvent read FOnBookmarksChanged write FOnBookmarksChanged;
    property OnGotoBookmark: TBookmarkCallback read FOnGotoBookmark write FOnGotoBookmark;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  fpg_dialogs;

{@VFD_NEWFORM_IMPL}

procedure TBookmarksForm.lbBookmarksKeyPressed(Sender: TObject;
  var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  if (KeyCode = keyEnter) or (KeyCode = keyPEnter) then
  begin
    GotoSelectedBookmark;
    Close;
  end;
end;

procedure TBookmarksForm.FormShow(Sender: TObject);
begin
  RefreshList;
  lbBookmarks.SetFocus;
end;

procedure TBookmarksForm.lbBookmarksDoubleClicked(Sender: TObject;
  AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  GotoSelectedBookmark;
  Close;
end;

procedure TBookmarksForm.btnRenameClicked(Sender: TObject);
var
  Bookmark: TBookmark;
  lName: TfpgString;
begin
  Bookmark := GetSelectedBookmark;
  if Bookmark = nil then
    exit;

  lName := Bookmark.Name;
  if fpgInputQuery( 'Rename Bookmark', 'Enter the new name of the bookmark', lName ) then
  begin
    Bookmark.Name := lName;
    if Assigned(OnBookmarksChanged) then
      OnBookmarksChanged(self);
    // redisplay name in list
    lbBookmarks.Items[lbBookmarks.FocusItem] := Bookmark.Name;
    lbBookmarks.Invalidate;
  end;
end;

procedure TBookmarksForm.btnDeleteClicked(Sender: TObject);
var
  Bookmark: TBookmark;
  BookmarkIndex: integer;
begin
  Bookmark := GetSelectedBookmark;
  if Bookmark = nil then
    exit;

  if TfpgMessageDialog.Question('Delete Bookmark',
      Format('Delete the bookmark named "%s"?', [Bookmark.Name])) = mbYes then
  begin
    BookmarkIndex := BookmarkList.IndexOf( Bookmark );
    lbBookmarks.Items.Delete( BookmarkIndex );
    BookmarkList.Delete( BookmarkIndex );

    if BookmarkIndex > BookmarkList.Count - 1 then
      BookmarkIndex := BookmarkList.Count - 1;

    lbBookmarks.FocusItem := BookmarkIndex;

    Bookmark.Free;
    if Assigned(OnBookmarksChanged) then
      OnBookmarksChanged(self);
    lbBookmarks.Invalidate;

    UpdateControls;
  end;
end;

procedure TBookmarksForm.btnGotoClicked(Sender: TObject);
begin
  GotoSelectedBookmark;
end;

procedure TBookmarksForm.btnHelpClicked(Sender: TObject);
begin
  InvokeHelp;
end;

procedure TBookmarksForm.btnCloseClicked(Sender: TObject);
begin
  Close;
end;

function TBookmarksForm.SelectedObject(ListBox: TfpgListBox): TObject;
begin
  if (ListBox.FocusItem >= 0) and (ListBox.FocusItem < ListBox.Items.Count) then
    Result := ListBox.Items.Objects[ListBox.FocusItem]
  else
    Result := nil;
end;

procedure TBookmarksForm.UpdateControls;
var
  Selected: Boolean;
begin
  Selected := GetSelectedBookmark <> nil;
  btnRename.Enabled := Selected;
  btnDelete.Enabled := Selected;
  btnGoto.Enabled := Selected;
  if not btnGoto.Enabled then
    btnGoto.Default := false;
end;

function TBookmarksForm.GetSelectedBookmark: TBookmark;
begin
  if SelectedObject(lbBookmarks) = nil then
    result := nil
  else
    result := SelectedObject(lbBookmarks) as TBookmark;
end;

procedure TBookmarksForm.GotoSelectedBookmark;
begin
  if Assigned(FOnGotoBookmark) then
    if GetSelectedBookmark <> nil then
      FOnGotoBookmark(GetSelectedBookmark);
end;

procedure TBookmarksForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: BookmarksForm}
  Name := 'BookmarksForm';
  SetPosition(553, 246, 393, 247);
  WindowTitle := 'Bookmarks';
  Hint := '';
  HelpType := htContext;
  HelpContext := 8;
  OnShow := @FormShow;

  lbBookmarks := TfpgListBox.Create(self);
  with lbBookmarks do
  begin
    Name := 'lbBookmarks';
    SetPosition(8, 12, 272, 227);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#List';
    Hint := '';
    TabOrder := 1;
    OnDoubleClick := @lbBookmarksDoubleClicked;
    OnKeyPress := @lbBookmarksKeyPressed;
  end;

  btnRename := TfpgButton.Create(self);
  with btnRename do
  begin
    Name := 'btnRename';
    SetPosition(288, 16, 96, 23);
    Anchors := [anRight,anTop];
    Text := 'Rename...';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
    OnClick := @btnRenameClicked;
  end;

  btnDelete := TfpgButton.Create(self);
  with btnDelete do
  begin
    Name := 'btnDelete';
    SetPosition(288, 44, 96, 23);
    Anchors := [anRight,anTop];
    Text := 'Delete';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnDeleteClicked;
  end;

  btnGoTo := TfpgButton.Create(self);
  with btnGoTo do
  begin
    Name := 'btnGoTo';
    SetPosition(288, 72, 96, 23);
    Anchors := [anRight,anTop];
    Text := 'Goto';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
    Default := True;
    OnClick := @btnGotoClicked;
  end;

  btnHelp := TfpgButton.Create(self);
  with btnHelp do
  begin
    Name := 'btnHelp';
    SetPosition(288, 100, 96, 23);
    Anchors := [anRight,anTop];
    Text := 'Help';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 5;
    OnClick := @btnHelpClicked;
  end;

  btnClose := TfpgButton.Create(self);
  with btnClose do
  begin
    Name := 'btnClose';
    SetPosition(288, 217, 96, 23);
    Anchors := [anRight,anBottom];
    Text := 'Close';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 6;
    OnClick := @btnCloseClicked;
  end;

  {@VFD_BODY_END: BookmarksForm}
  {%endregion}
end;

procedure TBookmarksForm.RefreshList;
var
  i: integer;
  Bookmark: TBookmark;
Begin
  lbBookmarks.Items.BeginUpdate;

  lbBookmarks.Items.Clear;

  if not Assigned(BookmarkList) then
    exit;

  for i := 0 to BookmarkList.Count - 1 do
  begin
    Bookmark := TBookmark(BookmarkList[i]);
    lbBookmarks.Items.AddObject(Bookmark.Name, Bookmark);
  end;

  if lbBookmarks.Items.Count > 0 then
    lbBookmarks.FocusItem := 0;

  lbBookmarks.Items.EndUpdate;
  UpdateControls;
End;

end.
