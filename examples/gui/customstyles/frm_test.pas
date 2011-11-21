unit frm_test;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_widget,
  fpg_form, fpg_edit, fpg_label, fpg_button, fpg_menu,
  fpg_memo;

type

  TTestForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: TestForm}
    btnName1: TfpgButton;
    lblName1: TfpgLabel;
    lblName2: TfpgLabel;
    btnName2: TfpgButton;
    lblName3: TfpgLabel;
    btnName3: TfpgButton;
    lblName4: TfpgLabel;
    btnName4: TfpgButton;
    edtName1: TfpgEdit;
    btnClose: TfpgButton;
    MainMenu: TfpgMenuBar;
    pmFile: TfpgPopupMenu;
    pmEdit: TfpgPopupMenu;
    pmHelp: TfpgPopupMenu;
    pmSubMenu1: TfpgPopupMenu;
    memStyles: TfpgMemo;
    Label1: TfpgLabel;
    {@VFD_HEAD_END: TestForm}
    procedure CloseClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  fpg_stylemanager;

{@VFD_NEWFORM_IMPL}

procedure TTestForm.CloseClicked(Sender: TObject);
begin
  Close;
end;

procedure TTestForm.AfterCreate;
var
  miSubMenu: TfpgMenuItem;
begin
  {@VFD_BODY_BEGIN: TestForm}
  Name := 'TestForm';
  SetPosition(335, 206, 484, 250);
  WindowTitle := 'Testing Custom Styles';
  Hint := '';
  WindowPosition := wpScreenCenter;

  btnName1 := TfpgButton.Create(self);
  with btnName1 do
  begin
    Name := 'btnName1';
    SetPosition(24, 56, 80, 24);
    Text := 'Button1';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(20, 32, 116, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Standard Button';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(164, 32, 124, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Embedded Button';
  end;

  btnName2 := TfpgButton.Create(self);
  with btnName2 do
  begin
    Name := 'btnName2';
    SetPosition(176, 56, 80, 24);
    Text := 'Button2';
    Embedded := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(20, 100, 100, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Default Button';
  end;

  btnName3 := TfpgButton.Create(self);
  with btnName3 do
  begin
    Name := 'btnName3';
    SetPosition(24, 124, 80, 24);
    Text := 'Button3';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    Default := True;
  end;

  lblName4 := TfpgLabel.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(164, 100, 116, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Flat Button';
  end;

  btnName4 := TfpgButton.Create(self);
  with btnName4 do
  begin
    Name := 'btnName4';
    SetPosition(176, 124, 80, 24);
    Text := 'Button4';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
  end;

  edtName1 := TfpgEdit.Create(self);
  with edtName1 do
  begin
    Name := 'edtName1';
    SetPosition(24, 168, 164, 27);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 5;
    Text := '';
  end;

  btnClose := TfpgButton.Create(self);
  with btnClose do
  begin
    Name := 'btnClose';
    SetPosition(396, 216, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Close';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.close';
    TabOrder := 6;
    OnClick := @CloseClicked;
  end;

  MainMenu := TfpgMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(8, 4, 120, 24);
    Align := alTop;
  end;

  pmFile := TfpgPopupMenu.Create(self);
  with pmFile do
  begin
    Name := 'pmFile';
    SetPosition(204, 148, 120, 24);
    AddMenuItem('&Open', 'Ctrl+O', nil);
    AddMenuItem('&Save', 'Ctrl+S', nil);
    AddMenuItem('S&ave As', 'Ctrl+A', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('Save && Reload', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('&Quit', 'Ctrl+Q', nil);
  end;

  pmEdit := TfpgPopupMenu.Create(self);
  with pmEdit do
  begin
    Name := 'pmEdit';
    SetPosition(204, 172, 120, 24);
    AddMenuItem('Cut', '', nil);
    AddMenuItem('Copy', '', nil);
    AddMenuItem('Paste', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('Some selected item', '', nil).Checked := True;
    miSubMenu := AddMenuItem('My sub-menu', '', nil);
  end;

  pmHelp := TfpgPopupMenu.Create(self);
  with pmHelp do
  begin
    Name := 'pmHelp';
    SetPosition(204, 196, 120, 24);
    AddMenuItem('About...', '', nil);
  end;

  pmSubMenu1 := TfpgPopupMenu.Create(self);
  with pmSubMenu1 do
  begin
    Name := 'pmSubMenu1';
    SetPosition(204, 220, 120, 24);
    AddMenuItem('Item 1', '', nil);
    AddMenuItem('Item 2', '', nil);
    AddMenuItem('Item 3', '', nil).Enabled := False;
  end;

  memStyles := TfpgMemo.Create(self);
  with memStyles do
  begin
    Name := 'memStyles';
    SetPosition(304, 48, 168, 146);
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 16;
  end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(304, 32, 172, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Registered Styles:';
  end;

  {@VFD_BODY_END: TestForm}

  // hook up menus to mainmenu bar
  MainMenu.AddMenuItem('File', nil).SubMenu := pmFile;
  MainMenu.AddMenuItem('Edit', nil).SubMenu := pmEdit;
  MainMenu.AddMenuItem('Help', nil).SubMenu := pmHelp;

  miSubMenu.SubMenu := pmSubMenu1;

  fpgStyleManager.AssignStyleTypes(memStyles.Lines);
end;



end.
