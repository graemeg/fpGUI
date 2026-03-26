{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2026 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Find in File search dialog.
}

unit ide.form.find;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_label,
  fpg_edit,
  fpg_button,
  fpg_checkbox,
  fpg_textedit,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc,
  fpg_mig_unitvalue, fpg_mig_platformdefaults;

type

  TFindForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: FindForm}
    Label1: TfpgLabel;
    edtFindText: TfpgEdit;
    chkReplace: TfpgCheckBox;
    edtReplaceText: TfpgEdit;
    Label2: TfpgLabel;
    chkCaseSensitive: TfpgCheckBox;
    chkWholeWord: TfpgCheckBox;
    chkGlobalScope: TfpgCheckBox;
    chkSearchBackwards: TfpgCheckBox;
    btnCancel: TfpgButton;
    btnFind: TfpgButton;
    btnHelp: TfpgButton;
    {@VFD_HEAD_END: FindForm}
    procedure chkReplaceChanged(Sender: TObject);
    procedure edtFindTextKeyPressed(Sender: TObject; var KeyCode: Word; var ShiftState: TShiftState; var Consumed: Boolean);
  public
    procedure AfterCreate; override;
  end;

procedure DisplayFindForm(var AFindText: TfpgString; var AOptions: TfpgFindOptions; var ABackward: Boolean);

{@VFD_NEWFORM_DECL}

implementation

procedure DisplayFindForm(var AFindText: TfpgString; var AOptions: TfpgFindOptions; var ABackward: Boolean);
var
  frm: TFindForm;
begin
  frm := TFindForm.Create(nil);
  try
    frm.chkCaseSensitive.Checked    := foMatchCase in AOptions;
    frm.chkWholeWord.Checked        := foWholeWords in AOptions;
    frm.chkGlobalScope.Checked      := foEntireScope in AOptions;
    frm.chkSearchBackwards.Checked  := ABackward;

    if frm.ShowModal = mrCancel then
      AFindText := ''
    else
    begin
      AFindText := frm.edtFindText.Text;
      AOptions := [];
      if frm.chkCaseSensitive.Checked then
        include(AOptions, foMatchCase);
      if frm.chkWholeWord.Checked then
        include(AOptions, foWholeWords);
      if frm.chkGlobalScope.Checked then
        include(AOptions, foEntireScope);
      ABackward := frm.chkSearchBackwards.Checked;
    end;
  finally
    frm.Free;
  end;
end;

{@VFD_NEWFORM_IMPL}

procedure TFindForm.chkReplaceChanged(Sender: TObject);
begin
  edtReplaceText.Enabled := chkReplace.Checked;
end;

procedure TFindForm.edtFindTextKeyPressed(Sender: TObject; var KeyCode: Word; var ShiftState: TShiftState; var Consumed: Boolean);
begin
  if KeyCode = keyEnter then
  begin
    Consumed := True;
    btnFind.Click;
  end
  else if KeyCode = keyEscape then
  begin
    Consumed := True;
    Close;
  end;
end;

procedure TFindForm.AfterCreate;
var
  mig: TfpgMigLayoutManager;
  lMinBtnWidth: TfpgMigUnitValue;
  lBtnWidth, lBtnHeight: Integer;
begin
  {%region 'Auto-generated GUI code' -fold}
  Name := 'FindForm';
  Left := 458;
  Top := 214;
  PreferredSize := fpgSize(300, 275);
  MinHeight := 275;
  MinWidth := 200;
  WindowTitle := 'Find';
  WindowPosition := wpMainFormCenter;

  mig := TfpgMigLayoutManager.Create;
  mig.LC.WrapAfter(1); //.Debug();
  LayoutManager := mig;

  // Get platform-specific minimum button width with DPI scaling
  lMinBtnWidth := TfpgMigPlatformDefaults.GetMinimumButtonWidth;
  lBtnWidth := Round(lMinBtnWidth.GetPixels(0, Self, nil));
  // Button height uses natural DPI-aware calculation
  lBtnHeight := Font.GetHeight + 8;  // Same as TfpgButton's default

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    PreferredSize := fpgSize(250, 20);
    MinWidth := 185;
    Text := 'Text to find:';
  end;

  edtFindText := TfpgEdit.Create(self);
  with edtFindText do
  begin
    Name := 'edtFindText';
    PreferredSize := fpgSize(292, 24);
    TabOrder := 2;
    OnKeyPress := @edtFindTextKeyPressed;
  end;

  chkReplace := TfpgCheckBox.Create(self);
  with chkReplace do
  begin
    Name := 'chkReplace';
    PreferredSize := fpgSize(188, 20);
    TabOrder := 3;
    Text := 'Replace with';
    OnChange := @chkReplaceChanged;
  end;

  edtReplaceText := TfpgEdit.Create(self);
  with edtReplaceText do
  begin
    Name := 'edtReplaceText';
    PreferredSize := fpgSize(292, 24);
    Enabled := False;
    TabOrder := 4;
  end;

  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    PreferredSize := fpgSize(160, 16);
    FontDesc := '#Label2';
    Text := 'Options';
  end;

  chkCaseSensitive := TfpgCheckBox.Create(self);
  with chkCaseSensitive do
  begin
    Name := 'chkCaseSensitive';
    PreferredSize := fpgSize(160, 20);
    TabOrder := 6;
    Text := 'Case Sensitive';
  end;

  chkWholeWord := TfpgCheckBox.Create(self);
  with chkWholeWord do
  begin
    Name := 'chkWholeWord';
    PreferredSize := fpgSize(160, 20);
    TabOrder := 7;
    Text := 'Whole Words Only';
  end;

  chkGlobalScope := TfpgCheckBox.Create(self);
  with chkGlobalScope do
  begin
    Name := 'chkGlobalScope';
    PreferredSize := fpgSize(160, 20);
    TabOrder := 8;
    Text := 'Global Scope';
  end;

  chkSearchBackwards := TfpgCheckBox.Create(self);
  with chkSearchBackwards do
  begin
    Name := 'chkSearchBackwards';
    PreferredSize := fpgSize(160, 20);
    TabOrder := 9;
    Text := 'Search backwards';
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    PreferredSize := fpgSize(lBtnWidth, lBtnHeight);
    Text := 'Cancel';
    ModalResult := mrCancel;
    TabOrder := 10;
  end;

  btnFind := TfpgButton.Create(self);
  with btnFind do
  begin
    Name := 'btnFind';
    PreferredSize := fpgSize(lBtnWidth, lBtnHeight);
    Text := 'Find';
    ModalResult := mrOK;
    TabOrder := 11;
  end;

  btnHelp := TfpgButton.Create(self);
  with btnHelp do
  begin
    Name := 'btnHelp';
    PreferredSize := fpgSize(lBtnHeight, lBtnHeight); // square shape
    Text := '?';
    TabOrder := 12;
  end;

  mig.AddLayoutComponent(Label1, TfpgMigCC.Create());
  mig.AddLayoutComponent(edtFindText, TfpgMigCC.Create().GrowX);
  mig.AddLayoutComponent(chkReplace, TfpgMigCC.Create());
  mig.AddLayoutComponent(edtReplaceText, TfpgMigCC.Create().GrowX);

  mig.AddLayoutComponent(Label2, TfpgMigCC.Create());
  mig.AddLayoutComponent(chkCaseSensitive, TfpgMigCC.Create().GrowX);
  mig.AddLayoutComponent(chkWholeWord, TfpgMigCC.Create().GrowX);
  mig.AddLayoutComponent(chkGlobalScope, TfpgMigCC.Create().GrowX);
  mig.AddLayoutComponent(chkSearchBackwards, TfpgMigCC.Create().GrowX);

  mig.AddLayoutComponent(btnHelp, TfpgMigCC.Create().SpanX.Split(3).Tag('help'));
  mig.AddLayoutComponent(btnFind, TfpgMigCC.Create().Tag('ok'));
  mig.AddLayoutComponent(btnCancel, TfpgMigCC.Create().Tag('cancel'));
  {%endregion}
end;


end.
