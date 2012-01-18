unit fra_test;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_checkbox, fpg_button,
  fpg_menu, fpg_memo, fpg_panel;

type

  { Note the tags for the UI Designer. This allows use to visually design
    our frame. }
  TMyFrame = class(TfpgFrame)
  private
    {@VFD_HEAD_BEGIN: MyFrame}
    fraCheckBox1: TfpgCheckBox;
    fraMenu1: TfpgMenuBar;
    Button1: TfpgButton;
    Memo1: TfpgMemo;
    {@VFD_HEAD_END: MyFrame}
    framnuFile: TfpgPopupMenu;
    framnuHelp: TfpgPopupMenu;
    procedure miHelpAboutClicked(Sender: TObject);
  public
    destructor Destroy; override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  fpg_dialogs;

{@VFD_NEWFORM_IMPL}

procedure TMyFrame.miHelpAboutClicked(Sender: TObject);
begin
  TfpgMessageDialog.AboutFPGui('');
end;

destructor TMyFrame.Destroy;
begin
  framnuFile.Free;
  framnuHelp.Free;
  inherited Destroy;
end;

procedure TMyFrame.AfterCreate;
var
  miFile: TfpgMenuItem;
  miHelp: TfpgMenuItem;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MyFrame}
  Name := 'MyFrame';
  SetPosition(380, 237, 200, 203);
  WindowTitle := 'MyFrame';
  Hint := '';

  fraCheckBox1 := TfpgCheckBox.Create(self);
  with fraCheckBox1 do
  begin
    Name := 'fraCheckBox1';
    SetPosition(8, 40, 120, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 0;
    Text := 'CheckBox';
  end;

  fraMenu1 := TfpgMenuBar.Create(self);
  with fraMenu1 do
  begin
    Name := 'fraMenu1';
    SetPosition(0, 0, 200, 24);
    Anchors := [anLeft,anRight,anTop];
    miFile := AddMenuItem('File', nil);
    AddMenuItem('Edit', nil).Enabled := False;
    miHelp := AddMenuItem('Help', nil);
  end;

  Button1 := TfpgButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(104, 164, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Button';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
  end;

  Memo1 := TfpgMemo.Create(self);
  with Memo1 do
  begin
    Name := 'Memo1';
    SetPosition(12, 60, 172, 88);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    Lines.Add('');
    FontDesc := '#Edit1';
    TabOrder := 3;
  end;

  {@VFD_BODY_END: MyFrame}
  {%endregion}

  { There still seems to be a minor issue with Popup Menus used in a frame. So
    for now the work around is to manually maintain the life of the Popup
    Menus - so Owner is set to nil. }
  framnuFile := TfpgPopupMenu.Create(nil);
  with framnuFile do
  begin
    Name := 'framnuFile';
    SetPosition(44, 64, 120, 20);
    AddMenuItem('Open...', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('Save', '', nil);
  end;
  miFile.SubMenu := framnuFile;

  framnuHelp := TfpgPopupMenu.Create(nil);
  with framnuHelp do
  begin
    Name := 'framnuHelp';
    SetPosition(44, 64, 120, 20);
    AddMenuItem('About fpGUI...', '', @miHelpAboutClicked);
  end;
  miHelp.SubMenu := framnuHelp;
end;


end.
