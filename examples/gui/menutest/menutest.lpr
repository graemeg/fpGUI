program menutest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_menu,
  fpg_edit,
  fpg_panel,
  fpg_button,
  fpg_dialogs,
  fpg_memo;

type
  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    FMenuBar: TfpgMenuBar;
    StatusBar: TfpgPanel;
    Toolbar: TfpgBevel;
    btnQuit: TfpgButton;
    btnSave: TfpgButton;
    btnOpen: TfpgButton;
    btnAbout: TfpgButton;
    edit1: TfpgEdit;
    Memo1: TfpgMemo;
    FFileSubMenu: TfpgPopupMenu;
    FEditSubMenu: TfpgPopupMenu;
    FEditSelectSubMenu: TfpgPopupMenu;
    FViewSubMenu: TfpgPopupMenu;
    FHelpSubMenu: TfpgPopupMenu;
    {@VFD_HEAD_END: MainForm}
    procedure   miExitClicked(Sender: TObject);
    procedure   miMenuItemSelected(Sender: TObject);
    procedure   miMenuItemChecked(Sender: TObject);
    procedure   miToolBarChecked(Sender: TObject);
    procedure   miStatusBarChecked(Sender: TObject);
    procedure   btnAboutClicked(Sender: TObject);
    procedure   Log(const AText: TfpgString);
  public
    procedure   AfterCreate; override;
  end;


{ TMainForm }

procedure TMainForm.miExitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.miMenuItemSelected(Sender: TObject);
begin
  if Sender is TfpgMenuItem then
    Log('Menu clicked: ' + TfpgMenuItem(Sender).Text);
end;

procedure TMainForm.miMenuItemChecked(Sender: TObject);
begin
  TfpgMenuItem(Sender).Checked := not TfpgMenuItem(Sender).Checked;
  Log('Check Menu item toggled');
end;

procedure TMainForm.miToolBarChecked(Sender: TObject);
begin
  TfpgMenuItem(Sender).Checked := not TfpgMenuItem(Sender).Checked;
  ToolBar.Visible := not ToolBar.Visible;
  Log('Check Menu for Toolbar toggled');
end;

procedure TMainForm.miStatusBarChecked(Sender: TObject);
begin
  TfpgMenuItem(Sender).Checked := not TfpgMenuItem(Sender).Checked;
  StatusBar.Visible := not StatusBar.Visible;
  Log('Check Menu for Statusbar toggled');
end;

procedure TMainForm.btnAboutClicked(Sender: TObject);
begin
  TfpgMessageDialog.AboutFPGui;
end;

procedure TMainForm.Log(const AText: TfpgString);
begin
  Memo1.Lines.Add(AText);
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(316, 169, 400, 200);
  WindowTitle := 'Menu Test';
  Hint := '';
  WindowPosition := wpOneThirdDown;

  FMenuBar := TfpgMenuBar.Create(self);
  with FMenuBar do
  begin
    Name := 'FMenuBar';
    SetPosition(0, 0, 400, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  StatusBar := TfpgPanel.Create(self);
  with StatusBar do
  begin
    Name := 'StatusBar';
    SetPosition(0, 176, 400, 24);
    Anchors := [anLeft,anRight,anBottom];
    Alignment := taLeftJustify;
    FontDesc := '#Label1';
    Hint := '';
    Style := bsLowered;
    Text := 'This is the status bar...';
  end;

  Toolbar := TfpgBevel.Create(self);
  with Toolbar do
  begin
    Name := 'Toolbar';
    SetPosition(0, 24, 400, 29);
    Anchors := [anLeft,anRight,anTop];
    Hint := '';
    Shape := bsBottomLine;
  end;

  btnQuit := TfpgButton.Create(Toolbar);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(4, 2, 24, 24);
    Text := '';
    Embedded := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := -1;
    ImageName := 'stdimg.quit';
    ImageSpacing := 0;
    TabOrder := 1;
    OnClick := @miExitClicked;
  end;

  btnSave := TfpgButton.Create(Toolbar);
  with btnSave do
  begin
    Name := 'btnSave';
    SetPosition(56, 2, 24, 24);
    Text := '';
    Embedded := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := -1;
    ImageName := 'stdimg.save';
    ImageSpacing := 0;
    TabOrder := 2;
  end;

  btnOpen := TfpgButton.Create(Toolbar);
  with btnOpen do
  begin
    Name := 'btnOpen';
    SetPosition(32, 2, 24, 24);
    Text := '';
    Embedded := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := -1;
    ImageName := 'stdimg.open';
    ImageSpacing := 0;
    TabOrder := 3;
  end;

  btnAbout := TfpgButton.Create(Toolbar);
  with btnAbout do
  begin
    Name := 'btnAbout';
    SetPosition(84, 2, 24, 24);
    Text := '';
    Embedded := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := -1;
    ImageName := 'stdimg.help';
    ImageSpacing := 0;
    TabOrder := 4;
    OnClick := @btnAboutClicked;
  end;

  edit1 := TfpgEdit.Create(self);
  with edit1 do
  begin
    Name := 'edit1';
    SetPosition(8, 62, 100, 24);
    ExtraHint := '';
    Hint := '';
    TabOrder := 6;
    Text := '';
    FontDesc := '#Edit1';
  end;

  Memo1 := TfpgMemo.Create(self);
  with Memo1 do
  begin
    Name := 'Memo1';
    SetPosition(124, 60, 268, 108);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    Lines.Add('<= Text Edit has a popup menu too.');
    FontDesc := '#Edit1';
    TabOrder := 10;
  end;

  FFileSubMenu := TfpgPopupMenu.Create(self);
  with FFileSubMenu do
  begin
    Name := 'FFileSubMenu';
    SetPosition(264, 60, 120, 20);
    AddMenuItem('&Open', 'Ctrl-O', @miMenuItemSelected);
    AddMenuItem('&Save', 'Ctrl-S', @miMenuItemSelected);
    AddMenuItem('S&ave As', 'Ctrl+Shift+S', @miMenuItemSelected);
    AddMenuItem('-', '', nil);
    AddMenuItem('Save && Reload', '', @miMenuItemSelected);
    AddMenuItem('-', '', nil);
    AddMenuItem('&Quit', 'Ctrl-Q', @miExitClicked);
  end;

  FEditSubMenu := TfpgPopupMenu.Create(self);
  with FEditSubMenu do
  begin
    Name := 'FEditSubMenu';
    SetPosition(264, 80, 120, 20);
    AddMenuItem('&Cut', 'Ctrl-X', @miMenuItemSelected);
    AddMenuItem('C&opy', 'Ctrl-C', @miMenuItemSelected);
    AddMenuItem('&Paste', 'Ctrl-V', @miMenuItemSelected);
    AddMenuItem('-', '', nil);
    AddMenuItem('&Spell check', 'F4', @miMenuItemSelected).Enabled := False;
  end;

  FEditSelectSubMenu := TfpgPopupMenu.Create(self);
  with FEditSelectSubMenu do
  begin
    Name := 'FEditSelectSubMenu';
    SetPosition(264, 100, 120, 20);
    AddMenuItem('Select All', '', @miMenuItemSelected);
    AddMenuItem('Select Word', '', @miMenuItemSelected);
    AddMenuItem('Select Line', '', @miMenuItemSelected);
    FEditSubMenu.AddMenuItem('Selec&t', '', nil).SubMenu := FEditSelectSubMenu;
  end;

  FViewSubMenu := TfpgPopupMenu.Create(self);
  with FViewSubMenu do
  begin
    Name := 'FViewSubMenu';
    SetPosition(264, 120, 120, 20);
    AddMenuItem('Full Screen', '', @miMenuItemChecked);
    AddMenuItem('Tool Bar', '', @miToolBarChecked).Checked := True;
    AddMenuItem('Status Bar', '', @miStatusBarChecked).Checked := True;
    AddMenuItem('Line Numbers', '', @miMenuItemChecked);
  end;

  FHelpSubMenu := TfpgPopupMenu.Create(self);
  with FHelpSubMenu do
  begin
    Name := 'FHelpSubMenu';
    SetPosition(264, 140, 120, 20);
    AddMenuItem('&About', 'F12', @btnAboutClicked);
    AddMenuItem('Test Russian text -> Òåñò', '', @miMenuItemSelected);
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}

  // Attach sub menus to main menu bar
  FMenuBar.AddMenuItem('&File', nil).SubMenu := FFileSubMenu;
  FMenuBar.AddMenuItem('&Edit', nil).SubMenu := FEditSubMenu;
  FMenuBar.AddMenuItem('&View', nil).SubMenu := FViewSubMenu;
  FMenuBar.AddMenuItem('&Windows', nil);
  FMenuBar.AddMenuItem('&Disabled', nil).Enabled := False;
  FMenuBar.AddMenuItem('&Help', nil).SubMenu := FHelpSubMenu;
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
  frm.Free;
end;

begin
  MainProc;
end.


