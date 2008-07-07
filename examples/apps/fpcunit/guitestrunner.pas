unit GuiTestRunner;

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

  TGUITestRunnerForm = class(TfpgForm)
  public
    {@VFD_HEAD_BEGIN: GUITestRunnerForm}
    pbName1: TfpgProgressBar;
    cbName1: TfpgComboBox;
    btnRun: TfpgButton;
    lblName1: TfpgLabel;
    memName1: TfpgMemo;
    btnClear: TfpgButton;
    btnQuit: TfpgButton;
    lblName2: TfpgLabel;
    lblRuns: TfpgLabel;
    lblName4: TfpgLabel;
    lblErrors: TfpgLabel;
    lblName6: TfpgLabel;
    lblFailures: TfpgLabel;
    {@VFD_HEAD_END: GUITestRunnerForm}
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TGUITestRunnerForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: GUITestRunnerForm}
  Name := 'GUITestRunnerForm';
  SetPosition(304, 190, 359, 310);
  WindowTitle := 'GUI Test Runner';

  pbName1 := TfpgProgressBar.Create(self);
  with pbName1 do
  begin
    Name := 'pbName1';
    SetPosition(8, 64, 340, 22);
    Anchors := [anLeft,anRight,anTop];
  end;

  cbName1 := TfpgComboBox.Create(self);
  with cbName1 do
  begin
    Name := 'cbName1';
    SetPosition(16, 24, 244, 22);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#List';
    TabOrder := 1;
  end;

  btnRun := TfpgButton.Create(self);
  with btnRun do
  begin
    Name := 'btnRun';
    SetPosition(268, 24, 80, 24);
    Anchors := [anRight,anTop];
    Text := 'Run';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 2;
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(4, 4, 236, 16);
    FontDesc := '#Label2';
    Text := 'TestCase';
  end;

  memName1 := TfpgMemo.Create(self);
  with memName1 do
  begin
    Name := 'memName1';
    SetPosition(8, 120, 340, 156);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Edit1';
    TabOrder := 4;
  end;

  btnClear := TfpgButton.Create(self);
  with btnClear do
  begin
    Name := 'btnClear';
    SetPosition(8, 280, 80, 24);
    Anchors := [anLeft,anBottom];
    Text := 'Clear';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 5;
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(268, 280, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 6;
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(8, 96, 40, 16);
    FontDesc := '#Label2';
    Text := 'Runs:';
  end;

  lblRuns := TfpgLabel.Create(self);
  with lblRuns do
  begin
    Name := 'lblRuns';
    SetPosition(52, 96, 56, 16);
    FontDesc := '#Label1';
    Text := '---';
  end;

  lblName4 := TfpgLabel.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(124, 96, 52, 16);
    FontDesc := '#Label2';
    Text := 'Errors:';
  end;

  lblErrors := TfpgLabel.Create(self);
  with lblErrors do
  begin
    Name := 'lblErrors';
    SetPosition(172, 96, 36, 16);
    FontDesc := '#Label1';
    Text := '---';
  end;

  lblName6 := TfpgLabel.Create(self);
  with lblName6 do
  begin
    Name := 'lblName6';
    SetPosition(220, 96, 60, 16);
    FontDesc := '#Label2';
    Text := 'Failures:';
  end;

  lblFailures := TfpgLabel.Create(self);
  with lblFailures do
  begin
    Name := 'lblFailures';
    SetPosition(284, 96, 48, 16);
    FontDesc := '#Label1';
    Text := '---';
  end;

  {@VFD_BODY_END: GUITestRunnerForm}
end;


end.
