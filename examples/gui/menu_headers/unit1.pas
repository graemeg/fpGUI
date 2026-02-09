unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_main, fpg_form, fpg_menu;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    MainMenu: TfpgMenuBar;
    {@VFD_HEAD_END: MainForm}
    procedure CreateMenuItems;
    procedure ExitClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

implementation

procedure TMainForm.CreateMenuItems;
var
  item: TfpgMenuItem;
begin
  // File
  item := MainMenu.AddMenuItem('&File', nil);
  item.SubMenu := TfpgPopupMenu.Create(item);

  item.SubMenu.AddMenuItem('Foo', '', nil);
  item.SubMenu.AddMenuItem('Bar', '', nil);
  item.SubMenu.AddMenuItem('-', '', nil);

  item.SubMenu.AddMenuItem('E&xit', '', @ExitClicked);

  // test
  item := MainMenu.AddMenuItem('Test Headers', nil);
  item.SubMenu := TfpgPopupMenu.Create(item);

  item.SubMenu.AddHeader('Header1');
  item.SubMenu.AddMenuItem('Item1', '', nil);
  item.SubMenu.AddMenuItem('Item2', '', nil);

  item.SubMenu.AddHeader('Header2');
  item.SubMenu.AddMenuItem('Item3', '', nil);
  item.SubMenu.AddMenuItem('Item4', '', nil);



end;

procedure TMainForm.ExitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(646, 270, 300, 250);
  WindowTitle := 'Form1';
  Hint := '';
  IconName := 'stdimg.windowicon';

  MainMenu := TfpgMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(60, 20, 120, 32);
    Align:= alTop;
  end;

  {@VFD_BODY_END: MainForm}
  CreateMenuItems;
end;

end.

