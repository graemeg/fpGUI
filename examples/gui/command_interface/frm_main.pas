{
  This demonstrates the usage of ICommand and ICommandHolder. They work
  similar to Delphi's TAction classes
}
unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_button,
  fpg_memo,
  fpg_menu;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    btnAdd: TfpgButton;
    memName1: TfpgMemo;
    btnQuit: TfpgButton;
    MainMenu: TfpgMenuBar;
    mnuFile: TfpgPopupMenu;
    {@VFD_HEAD_END: MainForm}
    miAdd: TfpgMenuItem;
    procedure CommandHandler(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  fpg_command_intf,
  commands;

{@VFD_NEWFORM_IMPL}

{ A single event handler that handles all Command based events. }
procedure TMainForm.CommandHandler(Sender: TObject);
var
  cmd: ICommand;
  holder: ICommandHolder;
begin
  if Supports(Sender, ICommandHolder, holder) then
  begin
    cmd := holder.GetCommand;
    cmd.Execute;
  end;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(293, 236, 284, 254);
  WindowTitle := 'Command Interface Test';
  WindowPosition := wpOneThirdDown;

  btnAdd := TfpgButton.Create(self);
  with btnAdd do
  begin
    Name := 'btnAdd';
    SetPosition(204, 36, 75, 24);
    Text := 'Add';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @CommandHandler;
  end;

  memName1 := TfpgMemo.Create(self);
  with memName1 do
  begin
    Name := 'memName1';
    SetPosition(8, 36, 188, 208);
    FontDesc := '#Edit1';
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(204, 220, 75, 24);
    Text := 'Quit';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @CommandHandler;
  end;

  MainMenu := TfpgMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 284, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  mnuFile := TfpgPopupMenu.Create(self);
  with mnuFile do
  begin
    Name := 'mnuFile';
    SetPosition(44, 72, 120, 20);
    miAdd := AddMenuItem('Add Item', '', @CommandHandler);
    AddMenuItem('Quit', '', @CommandHandler);
  end;

  {@VFD_BODY_END: MainForm}
  
  MainMenu.AddMenuItem('File', nil).SubMenu := mnuFile;

  // instantiate the Command classes
  btnAdd.SetCommand(TAddCommand.Create(memName1));
  btnQuit.SetCommand(TExitCommand.Create);
  miAdd.SetCommand(btnAdd.GetCommand);  // reuse exist command from btnAdd instance
  // The menu item File|Quit shares the command of btnQuit
  mnuFile.MenuItemByName('Quit').SetCommand(btnQuit.GetCommand);
end;


end.
