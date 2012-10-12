{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit contains the Input Query dialogs.
}

{%mainunit fpg_dialogs.pas}

{$IFDEF read_interface}

type

  TfpgQueryDialog = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: fpgQueryDialog}
    lblText: TfpgLabel;
    edtText: TfpgEdit;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    {@VFD_HEAD_END: fpgQueryDialog}
    procedure SetupCaptions;
    procedure edtTextKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
  protected
    procedure HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
  public
    procedure AfterCreate; override;
  end;


{$ENDIF read_interface}

{$IFDEF read_implementation}

function fpgInputQuery(const ACaption, APrompt: TfpgString; var Value: TfpgString): Boolean;
var
  dlg: TfpgQueryDialog;
begin
  dlg := TfpgQueryDialog.Create(nil);
  try
    dlg.WindowTitle := ACaption;
    dlg.lblText.Text := APrompt;
    dlg.edtText.Text := Value;
    Result := dlg.ShowModal = mrOK;
    if Result then
      Value := dlg.edtText.Text;
  finally
    dlg.Free;
  end;
end;

{ TfpgQueryDialog }

procedure TfpgQueryDialog.SetupCaptions;
begin
  btnOK.Text := rsOK;
  btnCancel.Text := rsCancel;
end;

procedure TfpgQueryDialog.edtTextKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  if KeyCode = keyEnter then
   btnOK.Click;
end;

procedure TfpgQueryDialog.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if KeyCode = keyEscape then
  begin
    consumed := True;
    ModalResult := mrCancel;
  end;
end;

procedure TfpgQueryDialog.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: fpgQueryDialog}
  Name := 'fpgQueryDialog';
  SetPosition(300, 150, 340, 97);
  WindowTitle := 'QueryDialog';
  Hint := '';
  WindowPosition := wpOneThirdDown;

  lblText := TfpgLabel.Create(self);
  with lblText do
  begin
    Name := 'lblText';
    SetPosition(8, 8, 324, 16);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    Text := 'lblText';
  end;

  edtText := TfpgEdit.Create(self);
  with edtText do
  begin
    Name := 'edtText';
    SetPosition(8, 26, 324, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    Hint := '';
    TabOrder := 2;
    Text := '';
    FontDesc := '#Edit1';
    OnKeyPress := @edtTextKeyPressed;
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    Name := 'btnOK';
    SetPosition(144, 64, 92, 24);
    Anchors := [anRight,anBottom];
    Text := 'OK';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrOK;
    TabOrder := 3;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(240, 64, 92, 24);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrCancel;
    TabOrder := 4;
  end;

  {@VFD_BODY_END: fpgQueryDialog}
  {%endregion}

  SetupCaptions;
end;

{$ENDIF read_implementation}

