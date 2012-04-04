unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_memo, fpg_menu,
  fpg_button, fpg_editbtn;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    MainMenu: TfpgMenuBar;
    FilenameEdit1: TfpgFileNameEdit;
    memImages: TfpgMemo;
    Button1: TfpgButton;
    pmFile: TfpgPopupMenu;
    {@VFD_HEAD_END: MainForm}
    procedure miFileQuit(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TMainForm.miFileQuit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(357, 227, 484, 378);
  WindowTitle := 'MainForm';
  Hint := '';
  AcceptDrops := True;

  MainMenu := TfpgMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 484, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  FilenameEdit1 := TfpgFileNameEdit.Create(self);
  with FilenameEdit1 do
  begin
    Name := 'FilenameEdit1';
    SetPosition(4, 44, 384, 24);
    ExtraHint := '';
    FileName := '';
    Filter := '';
    InitialDir := '';
    TabOrder := 3;
  end;

  memImages := TfpgMemo.Create(self);
  with memImages do
  begin
    Name := 'memImages';
    SetPosition(4, 88, 476, 286);
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 5;
  end;

  Button1 := TfpgButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(396, 44, 80, 24);
    Text := 'Button';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
  end;

  pmFile := TfpgPopupMenu.Create(self);
  with pmFile do
  begin
    Name := 'pmFile';
    SetPosition(236, 128, 120, 20);
    AddMenuItem('Add File...', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('Quit', 'Ctrl+Q', @miFileQuit);
  end;

  {@VFD_BODY_END: MainForm}

  MainMenu.AddMenuItem('File', nil).SubMenu := pmFile;
  {%endregion}
end;


end.
