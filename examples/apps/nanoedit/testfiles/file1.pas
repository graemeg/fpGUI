unit file1;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, gfxbase, fpgfx, gui_edit, 
  gfx_widget, gui_form, gui_label, gui_button,
  gui_listbox, gui_memo, gui_combobox, gui_grid, 
  gui_dialogs, gui_checkbox, gui_tree, gui_trackbar, 
  gui_progressbar, gui_radiobutton, gui_tab, gui_menu;

type

  TMainForm = class(TfpgForm)
  private
    procedure miOpenClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miQuitClick(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: MainFrom}
    menu: TfpgMenuBar;
    mnuFile: TfpgPopupMenu;
    memEditor: TfpgMemo;
    {@VFD_HEAD_END: MainFrom}

    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TMainForm.miOpenClick(Sender: TObject);
var
  dlg: TfpgFileDialog;
begin
  dlg := TfpgFileDialog.Create(nil);
  try
    if dlg.RunOpenFile then
    begin
      memEditor.Lines.LoadFromFile(dlg.FileName);
    end;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.miSaveClick(Sender: TObject);
var
  dlg: TfpgFileDialog;
begin
  dlg := TfpgFileDialog.Create(nil);
  try
    if dlg.RunSaveFile then
    begin
      memEditor.Lines.SaveToFile(dlg.FileName);
    end;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.miQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainFrom}
  SetPosition(327, 283, 500, 348);
  WindowPosition := wpScreenCenter;
  WindowTitle := 'fpGUI nanoedit';

  menu := TfpgMenuBar.Create(self);
  with menu do
  begin
    SetPosition(0, 0, 500, 24);
    Anchors := [anTop, anLeft, anRight];
  end;

  mnuFile := TfpgPopupMenu.Create(self);
  with mnuFile do
  begin
    SetPosition(320, 4, 120, 20);
    AddMenuItem('Open...', '', @miOpenClick);
    AddMenuItem('Save...', '', @miSaveClick);
    AddMenuItem('Quit', '', @miQuitClick);
  end;

  memEditor := TfpgMemo.Create(self);
  with memEditor do
  begin
    SetPosition(0, 24, 500, 324);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Edit1';
  end;

  {@VFD_BODY_END: MainFrom}
  
  menu.AddMenuItem('&File', nil).SubMenu := mnuFile;
end;


end.
