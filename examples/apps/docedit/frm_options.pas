unit frm_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_edit,
  fpg_widget, fpg_form, fpg_label, fpg_button,
  fpg_listbox, fpg_memo, fpg_combobox, fpg_grid,
  fpg_dialogs, fpg_checkbox, fpg_tree, fpg_trackbar,
  fpg_progressbar, fpg_radiobutton, fpg_tab, fpg_menu,
  fpg_panel;

type

  TfrmOptions = class(TfpgForm)
  public
    {@VFD_HEAD_BEGIN: frmOptions}
    lblName1: TfpgLabel;
    cbConfirmDeletes: TfpgCheckBox;
    cbBackups: TfpgCheckBox;
    cbSkipNodes: TfpgCheckBox;
    edtDftExt: TfpgEdit;
    edtBakExt: TfpgEdit;
    edtMRU: TfpgEdit;
    lblName2: TfpgLabel;
    lblName3: TfpgLabel;
    lblName4: TfpgLabel;
    edtMakeSkel: TfpgEdit;
    btnMakeSkel: TfpgButton;
    edtFPDoc: TfpgEdit;
    btnFPDoc: TfpgButton;
    cbShowHints: TfpgCheckBox;
    pnlName1: TfpgBevel;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    lblName5: TfpgLabel;
    lblName6: TfpgLabel;
    lblName7: TfpgLabel;
    {@VFD_HEAD_END: frmOptions}

    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TfrmOptions.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmOptions}
  SetPosition(394, 264, 397, 357);
  WindowTitle := 'Options';
  WindowPosition := wpScreenCenter;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    SetPosition(8, 8, 51, 16);
    Text := 'General';
    FontDesc := '#Label2';
    AutoSize := True;
  end;

  cbConfirmDeletes := TfpgCheckBox.Create(self);
  with cbConfirmDeletes do
  begin
    SetPosition(24, 28, 120, 20);
    Text := 'Confirm deletes';
    FontDesc := '#Label1';
  end;

  cbBackups := TfpgCheckBox.Create(self);
  with cbBackups do
  begin
    SetPosition(24, 52, 120, 20);
    Text := 'Create backups';
    FontDesc := '#Label1';
  end;

  cbSkipNodes := TfpgCheckBox.Create(self);
  with cbSkipNodes do
  begin
    SetPosition(24, 76, 216, 20);
    Text := 'Skip empty nodes when saving';
    FontDesc := '#Label1';
  end;

  edtDftExt := TfpgEdit.Create(self);
  with edtDftExt do
  begin
    SetPosition(164, 104, 120, 22);
    Text := '.xml';
    FontDesc := '#Edit1';
  end;

  edtBakExt := TfpgEdit.Create(self);
  with edtBakExt do
  begin
    SetPosition(164, 128, 120, 22);
    Text := '.~xml';
    FontDesc := '#Edit1';
  end;

  edtMRU := TfpgEdit.Create(self);
  with edtMRU do
  begin
    SetPosition(164, 152, 120, 22);
    Text := '10';
    FontDesc := '#Edit1';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    SetPosition(8, 252, 50, 16);
    Text := 'Desktop';
    FontDesc := '#Label2';
    AutoSize := True;
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    SetPosition(24, 108, 104, 16);
    Text := 'Default extension:';
    FontDesc := '#Label1';
  end;

  lblName4 := TfpgLabel.Create(self);
  with lblName4 do
  begin
    SetPosition(24, 132, 108, 16);
    Text := 'Backup extension:';
    FontDesc := '#Label1';
  end;

  edtMakeSkel := TfpgEdit.Create(self);
  with edtMakeSkel do
  begin
    SetPosition(164, 184, 196, 22);
    Anchors := [anLeft,anRight,anTop];
    Text := '';
    FontDesc := '#Edit1';
  end;

  btnMakeSkel := TfpgButton.Create(self);
  with btnMakeSkel do
  begin
    SetPosition(364, 184, 23, 20);
    Anchors := [anRight,anTop];
    Text := '...';
    FontDesc := '#Label1';
    ImageName := '';
  end;

  edtFPDoc := TfpgEdit.Create(self);
  with edtFPDoc do
  begin
    SetPosition(164, 208, 196, 22);
    Anchors := [anLeft,anRight,anTop];
    Text := '';
    FontDesc := '#Edit1';
  end;

  btnFPDoc := TfpgButton.Create(self);
  with btnFPDoc do
  begin
    SetPosition(364, 208, 23, 20);
    Anchors := [anRight,anTop];
    Text := '...';
    FontDesc := '#Label1';
    ImageName := '';
  end;

  cbShowHints := TfpgCheckBox.Create(self);
  with cbShowHints do
  begin
    SetPosition(24, 272, 120, 20);
    Text := 'Show hints';
    FontDesc := '#Label1';
  end;

  pnlName1 := TfpgBevel.Create(self);
  with pnlName1 do
  begin
    SetPosition(4, 313, 389, 11);
    Anchors := [anLeft,anRight,anBottom];
    Shape := bsTopLine;
    Style := bsLowered;
    Focusable := False;
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    SetPosition(234, 325, 75, 24);
    Anchors := [anRight,anBottom];
    Text := 'OK';
    FontDesc := '#Label1';
    ImageName := 'stdimg.ok';
    ModalResult := mrOK;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    SetPosition(314, 325, 75, 24);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    ImageName := 'stdimg.cancel';
    ModalResult := mrCancel;
  end;

  lblName5 := TfpgLabel.Create(self);
  with lblName5 do
  begin
    SetPosition(24, 156, 132, 16);
    Text := 'Max. recent used files:';
    FontDesc := '#Label1';
  end;

  lblName6 := TfpgLabel.Create(self);
  with lblName6 do
  begin
    SetPosition(24, 188, 127, 16);
    Text := 'makeskel executable:';
    FontDesc := '#Label1';
  end;

  lblName7 := TfpgLabel.Create(self);
  with lblName7 do
  begin
    SetPosition(24, 212, 102, 16);
    Text := 'fpdoc executable:';
    FontDesc := '#Label1';
  end;

  {@VFD_BODY_END: frmOptions}
end;


end.
