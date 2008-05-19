unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, gfxbase, fpgfx, gui_edit, 
  gfx_widget, gui_form, gui_label, gui_button,
  gui_listbox, gui_memo, gui_combobox, gui_basegrid, gui_grid, 
  gui_dialogs, gui_checkbox, gui_tree, gui_trackbar, 
  gui_progressbar, gui_radiobutton, gui_tab, gui_menu,
  gui_panel, gui_popupcalendar, gui_gauge;

type

  TMainForm = class(TfpgForm)
  private
    procedure cbHotTrackChanged(Sender: TObject);
    procedure btnAdd1Clicked(Sender: TObject);
    procedure btnFocusClicked(Sender: TObject);
    procedure btnClearClicked(Sender: TObject);
    procedure btnAdd10Clicked(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    lstName1: TfpgListBox;
    btnAdd10: TfpgButton;
    btnClear: TfpgButton;
    btnFocus: TfpgButton;
    btnAdd1: TfpgButton;
    memName1: TfpgMemo;
    cbHotTrack: TfpgCheckBox;
    {@VFD_HEAD_END: MainForm}
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TMainForm.cbHotTrackChanged(Sender: TObject);
begin
  lstName1.HotTrack := cbHotTrack.Checked;
end;

procedure TMainForm.btnAdd1Clicked(Sender: TObject);
begin
  lstName1.Items.Add(Format('Item %2d', [lstName1.ItemCount]));
end;

procedure TMainForm.btnFocusClicked(Sender: TObject);
begin
  if lstName1.ItemCount > 1 then
    lstName1.FocusItem := 2;
end;

procedure TMainForm.btnClearClicked(Sender: TObject);
begin
  lstName1.Items.Clear;
end;

procedure TMainForm.btnAdd10Clicked(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to 10 do
    lstName1.Items.Add(Format('Item %2d', [lstName1.ItemCount]));
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(345, 220, 300, 270);
  WindowTitle := 'ListBox test';
  WindowPosition := wpScreenCenter;

  lstName1 := TfpgListBox.Create(self);
  with lstName1 do
  begin
    Name := 'lstName1';
    SetPosition(12, 12, 128, 168);
    FontDesc := '#List';
  end;

  btnAdd10 := TfpgButton.Create(self);
  with btnAdd10 do
  begin
    Name := 'btnAdd10';
    SetPosition(204, 28, 92, 23);
    Text := 'Add 10 items';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 1;
    OnClick := @btnAdd10Clicked;
  end;

  btnClear := TfpgButton.Create(self);
  with btnClear do
  begin
    Name := 'btnClear';
    SetPosition(204, 56, 92, 23);
    Text := 'Clear Items';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 2;
    OnClick := @btnClearClicked;
  end;

  btnFocus := TfpgButton.Create(self);
  with btnFocus do
  begin
    Name := 'btnFocus';
    SetPosition(204, 84, 92, 23);
    Text := 'FocusItem = 2';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnFocusClicked;
  end;

  btnAdd1 := TfpgButton.Create(self);
  with btnAdd1 do
  begin
    Name := 'btnAdd1';
    SetPosition(204, 112, 92, 23);
    Text := 'Add 1 item';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 4;
    OnClick := @btnAdd1Clicked;
  end;

  memName1 := TfpgMemo.Create(self);
  with memName1 do
  begin
    Name := 'memName1';
    SetPosition(12, 188, 280, 77);
    FontDesc := '#Edit1';
    TabOrder := 5;
  end;

  cbHotTrack := TfpgCheckBox.Create(self);
  with cbHotTrack do
  begin
    Name := 'cbHotTrack';
    SetPosition(204, 140, 120, 19);
    FontDesc := '#Label1';
    TabOrder := 6;
    Text := 'Track Focus';
    OnChange := @cbHotTrackChanged;
  end;

  {@VFD_BODY_END: MainForm}
end;


end.
