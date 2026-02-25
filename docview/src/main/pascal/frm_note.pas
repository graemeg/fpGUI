unit frm_note;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_memo,
  fpg_button;

type

  TNoteForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: NoteForm}
    Memo1: TfpgMemo;
    btnOK: TfpgButton;
    btnHelp: TfpgButton;
    btnCancel: TfpgButton;
    btnDelete: TfpgButton;
    {@VFD_HEAD_END: NoteForm}
    FCanDelete: Boolean;
    procedure   FormShow(Sender: TObject);
    function    GetText: TfpgString;
    procedure   SetText(const AValue: TfpgString);
    procedure   SetCanDelete(const AValue: boolean);
    procedure   btnHelpClicked(Sender: TObject);
  public
    procedure   AfterCreate; override;
    property    Text: TfpgString read GetText write SetText;
    property    CanDelete: boolean read FCanDelete write SetCanDelete;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TNoteForm.FormShow(Sender: TObject);
begin
  Memo1.SetFocus;
end;

function TNoteForm.GetText: TfpgString;
begin
  Result := Memo1.Text;
end;

procedure TNoteForm.SetText(const AValue: TfpgString);
begin
  Memo1.Text := AValue;
end;

procedure TNoteForm.SetCanDelete(const AValue: boolean);
begin
  FCanDelete := AValue;
  btnDelete.Enabled := FCanDelete;
end;

procedure TNoteForm.btnHelpClicked(Sender: TObject);
begin
  InvokeHelp;
end;

procedure TNoteForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: NoteForm}
  Name := 'NoteForm';
  SetPosition(349, 186, 420, 191);
  WindowTitle := 'Notes';
  Hint := '';
  OnShow := @FormShow;
  HelpType := htContext;
  HelpContext := 7;

  Memo1 := TfpgMemo.Create(self);
  with Memo1 do
  begin
    Name := 'Memo1';
    SetPosition(4, 4, 412, 149);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 1;
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    Name := 'btnOK';
    SetPosition(252, 161, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'OK';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrOK;
    TabOrder := 2;
  end;

  btnHelp := TfpgButton.Create(self);
  with btnHelp do
  begin
    Name := 'btnHelp';
    SetPosition(4, 161, 80, 24);
    Anchors := [anLeft,anBottom];
    Text := 'Help';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnHelpClicked;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(336, 161, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrCancel;
    TabOrder := 4;
  end;

  btnDelete := TfpgButton.Create(self);
  with btnDelete do
  begin
    Name := 'btnDelete';
    SetPosition(92, 161, 80, 24);
    Anchors := [anLeft,anBottom];
    Text := 'Delete';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrAbort;
    TabOrder := 5;
  end;

  {@VFD_BODY_END: NoteForm}
  {%endregion}
end;


end.
