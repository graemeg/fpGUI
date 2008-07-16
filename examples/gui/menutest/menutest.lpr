program menutest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpgfx,
  gui_form,
  gui_menu,
  gui_edit;

type
  TMainForm = class(TfpgForm)
  private
    FMenuBar: TfpgMenuBar;
    FFileSubMenu: TfpgPopupMenu;
    FEditSubMenu: TfpgPopupMenu;
    FEditSelectSubMenu: TfpgPopupMenu;
    FHelpSubMenu: TfpgPopupMenu;
    edit1: TfpgEdit;
    procedure   miExitClicked(Sender: TObject);
    procedure   miMenuItemSelected(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;


{ TMainForm }

procedure TMainForm.miExitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.miMenuItemSelected(Sender: TObject);
begin
  if Sender is TfpgMenuItem then
    writeln('Menu clicked: ', TfpgMenuItem(Sender).Text);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Menu Test';
  WindowPosition := wpUser;
  SetPosition(100, 100, 400, 200);
  
  // Create top level sub-menus
  FFileSubMenu := TfpgPopupMenu.Create(self);
  FFileSubMenu.AddMenuItem('&Open', 'Ctrl-O', @miMenuItemSelected);
  FFileSubMenu.AddMenuItem('&Save', 'Ctrl-S', @miMenuItemSelected);
  FFileSubMenu.AddMenuItem('S&ave As', 'Ctrl+Shift+S', @miMenuItemSelected);
  FFileSubMenu.AddMenuItem('-', '', nil);
  FFileSubMenu.AddMenuItem('Save && Reload', '', @miMenuItemSelected);
  FFileSubMenu.AddMenuItem('-', '', nil);
  FFileSubMenu.AddMenuItem('&Quit', 'Ctrl-Q', @miExitClicked);

  FEditSubMenu := TfpgPopupMenu.Create(self);
  FEditSubMenu.AddMenuItem('&Cut', 'Ctrl-X', @miMenuItemSelected);
  FEditSubMenu.AddMenuItem('C&opy', 'Ctrl-C', @miMenuItemSelected);
  FEditSubMenu.AddMenuItem('&Paste', 'Ctrl-V', @miMenuItemSelected);
  FEditSubMenu.AddMenuItem('-', '', nil);
  FEditSubMenu.AddMenuItem('&Spell check', 'F4', @miMenuItemSelected).Enabled := False;
  FEditSelectSubMenu := TfpgPopupMenu.Create(self);
  FEditSubMenu.AddMenuItem('Selec&t', '', nil).SubMenu := FEditSelectSubMenu;
    FEditSelectSubMenu.AddMenuItem('Select All', '', @miMenuItemSelected);
    FEditSelectSubMenu.AddMenuItem('Select Word', '', @miMenuItemSelected);
    FEditSelectSubMenu.AddMenuItem('Select Line', '', @miMenuItemSelected);

  FHelpSubMenu := TfpgPopupMenu.Create(self);
  FHelpSubMenu.AddMenuItem('&About', 'F12', @miMenuItemSelected);
  FHelpSubMenu.AddMenuItem('Test Russian text -> Òåñò', '', @miMenuItemSelected);

  // Create main menu bar
  FMenuBar := CreateMenuBar(self);
  FMenuBar.AddMenuItem('&File', nil).SubMenu := FFileSubMenu;
  FMenuBar.AddMenuItem('&Edit', nil).SubMenu := FEditSubMenu;
  FMenuBar.AddMenuItem('&Windows', nil);
  FMenuBar.AddMenuItem('&Disabled', nil).Enabled := False;
  FMenuBar.AddMenuItem('&Help', nil).SubMenu := FHelpSubMenu;

  edit1 := TfpgEdit.Create(self);
  edit1.SetPosition(10, 70, 100, 24);
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


