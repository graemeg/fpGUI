{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2014 See the file AUTHORS.txt, included in this
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

  TfpgIntegerDialog = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: fpgIntegerDialog}
    lblText: TfpgLabel;
    edtInteger: TfpgEditInteger;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    {@VFD_HEAD_END: fpgIntegerDialog}
    procedure SetupCaptions;
    procedure edtIntegerKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
  protected
    procedure HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
  public
    procedure AfterCreate; override;
  end;


{$ENDIF read_interface}

{$IFDEF read_implementation}

function fpgIntegerQuery(const ACaption, APrompt: TfpgString; var Value: Integer; const MaxValue: Integer; const MinValue: Integer): Boolean;
var
  dlg: TfpgIntegerDialog;
begin
  dlg := TfpgIntegerDialog.Create(nil);
  try
    dlg.WindowTitle := ACaption;
    dlg.lblText.Text := APrompt;
    dlg.edtInteger.MaxValue:= MaxValue;
    dlg.edtinteger.MinValue:= MinValue;
    dlg.edtInteger.Value := Value;
    Result := dlg.ShowModal = mrOK;
    if Result then
      Value := dlg.edtInteger.Value;
  finally
    dlg.Free;
  end;
end;

{ TfpgIntegerDialog }

procedure TfpgIntegerDialog.SetupCaptions;
begin
  btnOK.Text := rsOK;
  btnCancel.Text := rsCancel;
end;

procedure TfpgIntegerDialog.edtIntegerKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  if KeyCode = keyEnter then
   btnOK.Click;
end;

procedure TfpgIntegerDialog.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if KeyCode = keyEscape then
  begin
    consumed := True;
    ModalResult := mrCancel;
  end;
end;

procedure TfpgIntegerDialog.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: fpgIntegerDialog}
  Name := 'fpgIntegerDialog';
  SetPosition(100, 150, 208, 97);
  WindowTitle := 'IntegerDialog';
  Hint := '';
  WindowPosition := wpOneThirdDown;

  lblText := TfpgLabel.Create(self);
  with lblText do
  begin
    Name := 'lblText';
    SetPosition(8, 8, 208, 16);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    Text := 'lblText';
  end;

  edtInteger := TfpgEditInteger.Create(self);
  with edtInteger do
  begin
    Name := 'edtInteger';
    SetPosition(8, 26, 100, 24);
    Anchors := [anLeft,anRight,anTop];
    Hint := '';
    TabOrder := 2;
    Text := '';
    FontDesc := '#Edit1';
    Value := 0;
    OnKeyPress := @edtIntegerKeyPressed;
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    Name := 'btnOK';
    SetPosition(8, 64, 92, 24);
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
    SetPosition(108, 64, 92, 24);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrCancel;
    TabOrder := 4;
  end;

  {@VFD_BODY_END: fpgIntegerDialog}
  {%endregion}

  SetupCaptions;
end;

{$ENDIF read_implementation}

