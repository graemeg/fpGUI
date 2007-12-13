{
  This demonstrates the usage of ICommand and ICommandHolder. They work
  similar to Delphi's TAction classes
}
unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, gfxbase, fpgfx, gui_edit, 
  gfx_widget, gui_form, gui_label, gui_button,
  gui_listbox, gui_memo, gui_combobox, gui_grid, 
  gui_dialogs, gui_checkbox, gui_tree, gui_trackbar, 
  gui_progressbar, gui_radiobutton, gui_tab, gui_menu,
  gui_bevel;

type

  TMainForm = class(TfpgForm)
  private
    procedure CommandHandler(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    btnAdd: TfpgButton;
    memName1: TfpgMemo;
    btnQuit: TfpgButton;
    MainMenu: TfpgMenuBar;
    mnuFile: TfpgPopupMenu;
    {@VFD_HEAD_END: MainForm}
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  gfx_command_intf,
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
  SetPosition(293, 236, 416, 273);
  WindowTitle := 'Command Interface Test';
  WindowPosition := wpScreenCenter;

  btnAdd := TfpgButton.Create(self);
  with btnAdd do
  begin
    Name := 'btnAdd';
    SetPosition(332, 36, 75, 24);
    Text := 'Add';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @CommandHandler;
  end;

  memName1 := TfpgMemo.Create(self);
  with memName1 do
  begin
    Name := 'memName1';
    SetPosition(8, 36, 316, 228);
    Lines.Add('');
    FontDesc := '#Edit1';
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(332, 240, 75, 24);
    Text := 'Quit';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @CommandHandler;
  end;

  MainMenu := TfpgMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 416, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  mnuFile := TfpgPopupMenu.Create(self);
  with mnuFile do
  begin
    Name := 'mnuFile';
    SetPosition(44, 72, 120, 20);
    AddMenuItem('Quit', '', @CommandHandler);
  end;

  {@VFD_BODY_END: MainForm}
  
  MainMenu.AddMenuItem('File', nil).SubMenu := mnuFile;

  // instantiate the Command classes
  btnAdd.SetCommand(TAddCommand.Create(memName1));
  btnQuit.SetCommand(TExitCommand.Create);
  // The menu item File|Quit shares the command of btnQuit
  mnuFile.MenuItemByName('Quit').SetCommand(btnQuit.GetCommand);
end;


end.
