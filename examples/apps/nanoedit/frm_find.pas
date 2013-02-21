unit frm_find;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_label,
  fpg_edit, fpg_button, fpg_checkbox, fpg_panel, fpg_radiobutton,
  fpg_textedit;

type

  TFindForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: FindForm}
    Label1: TfpgLabel;
    FindEdit: TfpgEdit;
    btnFind: TfpgButton;
    btnCancel: TfpgButton;
    GroupBox1: TfpgGroupBox;
    chkWholeWord: TfpgCheckBox;
    chkCaseSensitive: TfpgCheckBox;
    rbForward: TfpgRadioButton;
    rbBackward: TfpgRadioButton;
    {@VFD_HEAD_END: FindForm}
    procedure btnFindClicked(Sender: TObject);
    function GetTextToFind: TfpgString;
    function GetIsForward: boolean;
    function GetFindOptions: TfpgFindOptions;
  public
    procedure AfterCreate; override;
    function Execute: boolean;
    property TextToFind: TfpgString read GetTextToFind;
    property IsForward: boolean read GetIsForward;
    property FindOptions: TfpgFindOptions read GetFindOptions;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TFindForm.btnFindClicked(Sender: TObject);
begin
  ModalResult := mrOK;
end;

function TFindForm.GetTextToFind: TfpgString;
begin
  Result := FindEdit.Text;
end;

function TFindForm.GetIsForward: boolean;
begin
  Result := rbForward.Checked;
end;

function TFindForm.GetFindOptions: TfpgFindOptions;
begin
  Result := [foEntireScope];
  if chkWholeWord.Checked then
    Result := Result + [foWholeWords];
  if chkCaseSensitive.Checked then
    Result := Result + [foMatchCase];
end;

procedure TFindForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: FindForm}
  Name := 'FindForm';
  SetPosition(292, 173, 429, 110);
  WindowTitle := 'Find';
  Hint := '';

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(4, 4, 280, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Text to find:';
  end;

  FindEdit := TfpgEdit.Create(self);
  with FindEdit do
  begin
    Name := 'FindEdit';
    SetPosition(4, 20, 332, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 2;
    Text := '';
  end;

  btnFind := TfpgButton.Create(self);
  with btnFind do
  begin
    Name := 'btnFind';
    SetPosition(345, 8, 80, 24);
    Anchors := [anRight,anTop];
    Text := 'Find';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnFindClicked;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(345, 36, 80, 24);
    Anchors := [anRight,anTop];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrCancel;
    TabOrder := 4;
  end;

  GroupBox1 := TfpgGroupBox.Create(self);
  with GroupBox1 do
  begin
    Name := 'GroupBox1';
    SetPosition(160, 56, 176, 44);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Direction';
  end;

  chkWholeWord := TfpgCheckBox.Create(self);
  with chkWholeWord do
  begin
    Name := 'chkWholeWord';
    SetPosition(4, 52, 148, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 6;
    Text := 'Whole words only';
  end;

  chkCaseSensitive := TfpgCheckBox.Create(self);
  with chkCaseSensitive do
  begin
    Name := 'chkCaseSensitive';
    SetPosition(4, 72, 148, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 7;
    Text := 'Case sensitive';
  end;

  rbForward := TfpgRadioButton.Create(GroupBox1);
  with rbForward do
  begin
    Name := 'rbForward';
    SetPosition(8, 20, 76, 20);
    Checked := True;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 1;
    Text := 'Forword';
  end;

  rbBackward := TfpgRadioButton.Create(GroupBox1);
  with rbBackward do
  begin
    Name := 'rbBackward';
    SetPosition(88, 20, 84, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 2;
    Text := 'Backward';
  end;

  {@VFD_BODY_END: FindForm}
  {%endregion}
end;

function TFindForm.Execute: boolean;
begin
  Result := ShowModal <> mrCancel;
end;


end.
