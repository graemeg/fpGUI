{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit contains the dialog to manage bookmarks from the
      File Open/Save dialog.
}

{%mainunit fpg_dialogs.pas}

{$IFDEF read_interface}

  TConfigureBookmarksForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: ConfigureBookmarksForm}
    grdBookmarks: TfpgStringGrid;
    btnChangeTitle: TfpgButton;
    btnDelete: TfpgButton;
    btnClose: TfpgButton;
    btnMoveUp: TfpgButton;
    btnMoveDown: TfpgButton;
    {@VFD_HEAD_END: ConfigureBookmarksForm}
    FIni: TfpgIniFile;
    procedure   SetupCaptions;
    procedure   PopulateGrid;
    procedure   UpdateINIFile;
    procedure   btnChangeTitleClicked(Sender: TObject);
    procedure   btnDeleteClicked(Sender: TObject);
  public
    constructor Create(var AIniFile: TfpgIniFile); reintroduce;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;


{$ENDIF read_interface}

{$IFDEF read_implementation}

procedure TConfigureBookmarksForm.SetupCaptions;
begin
  WindowTitle := rsConfigureBookmarks;
  btnClose.Text := rsClose;
  btnMoveUp.Text := rsMoveUp;
  btnMoveDown.Text := rsMoveDown;
  btnChangeTitle.Text := rsChangeTitle;
  btnDelete.Text := rsDelete;
  grdBookmarks.ColumnTitle[0] := rsName;
  grdBookmarks.ColumnTitle[1] := rsDirectory;
end;

procedure TConfigureBookmarksForm.PopulateGrid;
var
  i: integer;
  lst: TStringList;
begin
  lst := TStringList.Create;
  FIni.ReadSection(FPG_BOOKMARK_SECTION, lst);
  grdBookmarks.RowCount := lst.Count;
  grdBookmarks.BeginUpdate;
  for i := 0 to lst.Count-1 do
  begin
    grdBookmarks.Cells[0, i] := lst[i];
    grdBookmarks.Cells[1, i] := FIni.ReadString(FPG_BOOKMARK_SECTION, lst[i], '');
  end;
  grdBookmarks.EndUpdate;
  lst.Free;
end;

procedure TConfigureBookmarksForm.UpdateINIFile;
var
  i: integer;
begin
  FIni.EraseSection(FPG_BOOKMARK_SECTION);
  for i := 0 to grdBookmarks.RowCount-1 do
  begin
    FIni.WriteString(FPG_BOOKMARK_SECTION, grdBookmarks.Cells[0, i], grdBookmarks.Cells[1, i]);
  end;
end;

procedure TConfigureBookmarksForm.btnChangeTitleClicked(Sender: TObject);
var
  s: TfpgString;
begin
  if (grdBookmarks.RowCount = 0) or (grdBookmarks.FocusRow = -1) then
    Exit;
  s := grdBookmarks.Cells[0, grdBookmarks.FocusRow];
  if fpgInputQuery('Bookmark', 'Enter new bookmark name', s) then
  begin
    s := StringReplace(s, '=', '-', [rfReplaceAll]); // don't allow '=' sign in name (ini file requirement)  
    grdBookmarks.Cells[0, grdBookmarks.FocusRow] := s;
  end;
end;

procedure TConfigureBookmarksForm.btnDeleteClicked(Sender: TObject);
begin
  if (grdBookmarks.RowCount = 0) or (grdBookmarks.FocusRow = -1) then
    Exit;
  grdBookmarks.DeleteRow(grdBookmarks.FocusRow);
end;

constructor TConfigureBookmarksForm.Create(var AIniFile: TfpgIniFile);
begin
  inherited Create(nil);
  FIni := AIniFile;
end;

destructor TConfigureBookmarksForm.Destroy;
begin
  UpdateINIFile;
  inherited Destroy;
end;

procedure TConfigureBookmarksForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: ConfigureBookmarksForm}
  Name := 'ConfigureBookmarksForm';
  SetPosition(331, 184, 596, 237);
  WindowTitle := 'Configure Bookmarks';
  Hint := '';
  ShowHint := True;
  WindowPosition := wpOneThirdDown;

  grdBookmarks := TfpgStringGrid.Create(self);
  with grdBookmarks do
  begin
    Name := 'grdBookmarks';
    SetPosition(8, 8, 473, 218);
    Anchors := [anLeft,anRight,anTop,anBottom];
    AddColumn('Title', 150, taLeftJustify);
    AddColumn('Directory', 300, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := True;
    TabOrder := 1;
    Options := [go_SmoothScroll, go_AlternativeColor]
  end;

  btnChangeTitle := TfpgButton.Create(self);
  with btnChangeTitle do
  begin
    Name := 'btnChangeTitle';
    SetPosition(489, 8, 100, 24);
    Anchors := [anRight,anTop];
    Text := 'btnChangeTitle';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
    OnClick := @btnChangeTitleClicked;
  end;

  btnDelete := TfpgButton.Create(self);
  with btnDelete do
  begin
    Name := 'btnDelete';
    SetPosition(489, 36, 100, 24);
    Anchors := [anRight,anTop];
    Text := 'btnDelete';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnDeleteClicked;
  end;

  btnMoveUp := TfpgButton.Create(self);
  with btnMoveUp do
  begin
    Name := 'btnMoveUp';
    SetPosition(489, 80, 100, 24);
    Anchors := [anRight,anTop];
    Text := 'btnMoveUp';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'sys.sb.up';
    TabOrder := 4;
  end;

  btnMoveDown := TfpgButton.Create(self);
  with btnMoveDown do
  begin
    Name := 'btnMoveDown';
    SetPosition(489, 108, 100, 24);
    Anchors := [anRight,anTop];
    Text := 'btnMoveDown';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'sys.sb.down';
    TabOrder := 5;
  end;

  btnClose := TfpgButton.Create(self);
  with btnClose do
  begin
    Name := 'btnClose';
    SetPosition(489, 204, 100, 24);
    Anchors := [anRight,anBottom];
    Text := 'btnClose';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.close';
    ModalResult := mrOK;
    TabOrder := 6;
  end;

  {@VFD_BODY_END: ConfigureBookmarksForm}
  {%endregion}

  SetupCaptions;
  PopulateGrid;
end;
{$ENDIF read_implementation}

