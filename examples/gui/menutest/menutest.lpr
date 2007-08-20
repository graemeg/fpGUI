program menutest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpgfx,
  gui_form,
  gui_menu;

type
  TMainForm = class(TfpgForm)
  private
    FMenuBar: TfpgMenuBar;
    FFileSubMenu: TfpgPopupMenu;
    FEditSubMenu: TfpgPopupMenu;
    FEditSelectSubMenu: TfpgPopupMenu;
    FHelpSubMenu: TfpgPopupMenu;
    procedure   miExitClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

procedure TMainForm.miExitClicked(Sender: TObject);
begin
  Close;
end;

constructor TMainForm.Create(AOwner: TComponent);
var
  mi: TfpgMenuItem;
begin
  inherited Create(AOwner);
  WindowTitle := 'Menu Test';
  WindowPosition := wpUser;
  SetPosition(100, 100, 400, 200);
  
  // Create top level sub-menus
  FFileSubMenu := TfpgPopupMenu.Create(self);
  FFileSubMenu.AddMenuItem('&Open', 'Ctrl-O', nil);
  FFileSubMenu.AddMenuItem('&Save', 'Ctrl-S', nil);
  FFileSubMenu.AddMenuItem('S&ave As', 'Ctrl+Shift+S', nil);
  FFileSubMenu.AddMenuItem('-', '', nil);
  FFileSubMenu.AddMenuItem('Save && Reload', '', nil);
  FFileSubMenu.AddMenuItem('-', '', nil);
  FFileSubMenu.AddMenuItem('&Quit', 'Ctrl-Q', @miExitClicked);

  FEditSubMenu := TfpgPopupMenu.Create(self);
  FEditSubMenu.AddMenuItem('&Cut', 'Ctrl-X', nil);
  FEditSubMenu.AddMenuItem('C&opy', 'Ctrl-C', nil);
  FEditSubMenu.AddMenuItem('&Paste', 'Ctrl-V', nil);
  FEditSubMenu.AddMenuItem('-', '', nil);
  FEditSubMenu.AddMenuItem('&Spell check', 'F4', nil).Enabled := False;
  FEditSelectSubMenu := TfpgPopupMenu.Create(self);
  FEditSubMenu.AddMenuItem('Selec&t', '', nil).SubMenu := FEditSelectSubMenu;
    FEditSelectSubMenu.AddMenuItem('Select All', '', nil);
    FEditSelectSubMenu.AddMenuItem('Select Word', '', nil);
    FEditSelectSubMenu.AddMenuItem('Select Line', '', nil);

  FHelpSubMenu := TfpgPopupMenu.Create(self);
  FHelpSubMenu.AddMenuItem('&About', 'F12', nil);
  FHelpSubMenu.AddMenuItem('Test Russian text -> Òåñò', '', nil);

  // Create main menu bar
  FMenuBar := TfpgMenuBar.Create(self);
  FMenuBar.SetPosition(0, 0, Width, FMenuBar.Height);
  FMenuBar.AddMenuItem('&File', nil).SubMenu := FFileSubMenu;
  FMenuBar.AddMenuItem('&Edit', nil).SubMenu := FEditSubMenu;
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
end;

begin
  MainProc;
end.


