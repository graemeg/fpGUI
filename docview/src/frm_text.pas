{
  A simple form with a memo to display text. Handle for debuging etc.
}
unit frm_text;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_button, fpg_memo;

type

  TTextForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: TextForm}
    btnClose: TfpgButton;
    Memo1: TfpgMemo;
    {@VFD_HEAD_END: TextForm}
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

procedure ShowText(const ATitle: TfpgString; const AText: TfpgString);


implementation


procedure ShowText(const ATitle: TfpgString; const AText: TfpgString);
var
  frm: TTextForm;
begin
  frm := TTextForm.Create(nil);
  try
    if ATitle = '' then
      frm.WindowTitle := 'Text Form'
    else
      frm.WindowTitle := ATitle;
    frm.Memo1.Lines.Text := AText;
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

{@VFD_NEWFORM_IMPL}

procedure TTextForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: TextForm}
  Name := 'TextForm';
  SetPosition(405, 197, 496, 297);
  WindowTitle := 'Text Form';
  Hint := '';

  btnClose := TfpgButton.Create(self);
  with btnClose do
  begin
    Name := 'btnClose';
    SetPosition(412, 268, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Close';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    ModalResult := mrOK;
  end;

  Memo1 := TfpgMemo.Create(self);
  with Memo1 do
  begin
    Name := 'Memo1';
    SetPosition(0, 0, 496, 260);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    FontDesc := '#Edit1';
    TabOrder := 2;
  end;

  {@VFD_BODY_END: TextForm}
  {%endregion}
end;


end.
