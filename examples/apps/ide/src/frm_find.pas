{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2013 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      ---
}

unit frm_find;

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
  fpg_textedit;

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
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: FindForm}
  Name := 'FindForm';
  SetPosition(458, 214, 300, 250);
  WindowTitle := 'Find';
  Hint := '';

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(4, 4, 284, 20);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Text to find:';
  end;

  edtFindText := TfpgEdit.Create(self);
  with edtFindText do
  begin
    Name := 'edtFindText';
    SetPosition(4, 22, 292, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 2;
    Text := '';
    OnKeyPress := @edtFindTextKeyPressed;
  end;

  chkReplace := TfpgCheckBox.Create(self);
  with chkReplace do
  begin
    Name := 'chkReplace';
    SetPosition(4, 50, 188, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 3;
    Text := 'Replace with';
    OnChange := @chkReplaceChanged;
  end;

  edtReplaceText := TfpgEdit.Create(self);
  with edtReplaceText do
  begin
    Name := 'edtReplaceText';
    SetPosition(4, 70, 292, 24);
    Anchors := [anLeft,anRight,anTop];
    Enabled := False;
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 4;
    Text := '';
  end;

  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(4, 100, 160, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Options';
  end;

  chkCaseSensitive := TfpgCheckBox.Create(self);
  with chkCaseSensitive do
  begin
    Name := 'chkCaseSensitive';
    SetPosition(16, 120, 160, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 6;
    Text := 'Case Sensitive';
  end;

  chkWholeWord := TfpgCheckBox.Create(self);
  with chkWholeWord do
  begin
    Name := 'chkWholeWord';
    SetPosition(16, 140, 160, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 7;
    Text := 'Whole Words Only';
  end;

  chkGlobalScope := TfpgCheckBox.Create(self);
  with chkGlobalScope do
  begin
    Name := 'chkGlobalScope';
    SetPosition(16, 160, 160, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 8;
    Text := 'Global Scope';
  end;

  chkSearchBackwards := TfpgCheckBox.Create(self);
  with chkSearchBackwards do
  begin
    Name := 'chkSearchBackwards';
    SetPosition(16, 180, 160, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 9;
    Text := 'Search backwards';
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(216, 220, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrCancel;
    TabOrder := 10;
  end;

  btnFind := TfpgButton.Create(self);
  with btnFind do
  begin
    Name := 'btnFind';
    SetPosition(132, 220, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Find';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrOK;
    TabOrder := 11;
  end;

  btnHelp := TfpgButton.Create(self);
  with btnHelp do
  begin
    Name := 'btnHelp';
    SetPosition(4, 220, 24, 24);
    Anchors := [anLeft,anBottom];
    Text := '?';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 12;
  end;

  {@VFD_BODY_END: FindForm}
  {%endregion}
end;


end.
