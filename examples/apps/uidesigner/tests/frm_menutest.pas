unit frm_menutest;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, gfxbase, fpgfx, gui_edit, 
  gfx_widget, gui_form, gui_label, gui_button,
  gui_listbox, gui_memo, gui_combobox, gui_grid, 
  gui_dialogs, gui_checkbox, gui_tree, gui_trackbar, 
  gui_progressbar, gui_radiobutton, gui_tab, gui_menu;

type

  TfrmMain = class(TfpgForm)
  private
    procedure miExitClicked(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: frmMain}
    MainMenu: TfpgMenuBar;
    miFile: TfpgPopupMenu;
    btnName1: TfpgButton;
    {@VFD_HEAD_END: frmMain}

    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TfrmMain.miExitClicked(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmMain}
  Name := 'frmMain';
  SetPosition(278, 186, 399, 142);
  WindowTitle := 'frmMain';
  WindowPosition := wpScreenCenter;

  MainMenu := TfpgMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 400, 23);
    Anchors := [anLeft,anRight,anTop];
  end;

  miFile := TfpgPopupMenu.Create(self);
  with miFile do
  begin
    Name := 'miFile';
    SetPosition(200, 48, 152, 24);
    AddMenuItem('&New...', 'Ctrl-N', nil);
    AddMenuItem('&Open...', 'Ctrl-O', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('E&xit', 'Alt+F4', @miExitClicked);
  end;

  btnName1 := TfpgButton.Create(self);
  with btnName1 do
  begin
    Name := 'btnName1';
    SetPosition(8, 112, 75, 24);
    Text := 'Button';
    FontDesc := '#Label1';
    ImageName := 'stdimg.quit';
    ModalResult := 0;
    ShowImage := True;
    OnClick := @miExitClicked;
  end;

  {@VFD_BODY_END: frmMain}
  
  MainMenu.AddMenuItem('&File', nil).SubMenu := miFile;
end;


end.
