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
      This unit contains the Character Map form used by edit components
      to insert special characters, when the user might not know the
      actual character code.
}


{%mainunit fpg_dialogs.pas}

{$IFDEF read_interface}

  TCharMapForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: CharMapForm}
    grdCharacters: TfpgStringGrid;
    btnClose: TfpgButton;
    lblCharInfo: TfpgLabel;
    edText: TfpgEdit;
    lblText: TfpgLabel;
    pnlChar: TfpgPanel;
    {@VFD_HEAD_END: CharMapForm}
    procedure   FormShow(Sender: TObject);
    procedure   grdCharactersFocusChange(Sender: TObject; ARow, ACol: integer);
    procedure   grdCharactersDrawCell(Sender: TObject; const ARow, ACol: integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
    procedure   grdCharactersCanSelectCell(Sender: TObject; const ARow, ACol: integer; var ACanSelect: boolean);
    procedure   grdCharactersDoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   grdCharactersKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure   FillCharMap;
    procedure   Button1Clicked(Sender: TObject);
    function    GetNewText: TfpgString;
    procedure   SetupCaptions;
  public
    procedure   AfterCreate; override;
    property    NewText: TfpgString read GetNewText;
  end;


{$ENDIF read_interface}



{$IFDEF read_implementation}


function fpgShowCharMap: TfpgString;
var
  frm: TCharMapForm;
begin
  Result := '';
  frm := TCharMapForm.Create(nil);
  try
    frm.ShowModal;
    Result := frm.NewText;
  finally
    frm.Free;
  end;
end;

{ TCharMapForm }

procedure TCharMapForm.FormShow(Sender: TObject);
begin
  FillCharMap;
end;

procedure TCharMapForm.grdCharactersFocusChange(Sender: TObject; ARow, ACol: integer);
var
  i: integer;
  tmp, tmp2: TfpgString;
begin
  if (ARow > 0) and (ACol > 0) then
  begin
    tmp  := grdCharacters.Cells[ACol, ARow];
    tmp2 := '';
    // generate UTF-8 byte representation
    for i := 1 to Length(tmp) do
      tmp2 := tmp2 + '$' + IntToHex(Ord(tmp[i]), 2);
    lblCharInfo.Text := 'U+' + inttohex(Ord(tmp[1]), 4) + ', UTF-8 = ' + tmp2;
    pnlChar.Text := tmp;
  end
  else
  begin
    lblCharInfo.Text := '-';
    pnlChar.Text := '';
  end;
end;

procedure TCharMapForm.grdCharactersDrawCell(Sender: TObject;
  const ARow, ACol: integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState;
  var ADefaultDrawing: boolean);
begin
  if (ARow = 0) or (ACol = 0) then
  begin
    ADefaultDrawing := False;
    grdCharacters.Canvas.Color := clWindowBackground;
    grdCharacters.Canvas.FillRectangle(ARect);
    //grdCharacters.Canvas.DrawButtonFace(ARect, []);
    grdCharacters.Canvas.TextColor := clText1;  //clGray;
    grdCharacters.Canvas.DrawText(ARect, grdCharacters.Cells[ACol, ARow],
      [txtHCenter, txtVCenter]);
  end
  else
    ADefaultDrawing := True;
end;

procedure TCharMapForm.grdCharactersCanSelectCell(Sender: TObject;
  const ARow, ACol: integer; var ACanSelect: boolean);
begin
  if (ACol = 0) or (ARow = 0) then
    ACanSelect := False
  else
    ACanSelect := True;
end;

procedure TCharMapForm.grdCharactersDoubleClick(Sender: TObject;
  AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  edText.Text := edText.Text + grdCharacters.Cells[grdCharacters.FocusCol, grdCharacters.FocusRow];
end;

procedure TCharMapForm.grdCharactersKeyPressed(Sender: TObject; var KeyCode: word;
  var ShiftState: TShiftState; var Consumed: boolean);
begin
  if KeyCode = keyEnter then
  begin
    edText.Text := edText.Text + grdCharacters.Cells[grdCharacters.FocusCol, grdCharacters.FocusRow];
  end;
end;

procedure TCharMapForm.FillCharMap;
var
  i: byte;
  j: byte;
  c: byte;
begin
  grdCharacters.BeginUpdate;
  try
    grdCharacters.ColumnCount := 17;
    grdCharacters.RowCount    := 17;
    grdCharacters.ShowHeader  := False;
    for i := 0 to 15 do
    begin
      for j := 0 to 15 do
      begin
        grdCharacters.ColumnWidth[j] := 20;
        c := i shl 4 or j;
        if (c > 0) and (c < 128) then
          grdCharacters.Cells[j + 1, i + 1] := chr(c)
        else
          grdCharacters.Cells[j + 1, i + 1] :=
            chr($C0 or (i div $4)) + chr($80 or c mod $40);
      end;
      grdCharacters.Cells[0, i + 1] := Format('%.2x +', [i]);
      grdCharacters.Cells[i + 1, 0] := Format('%.2x', [i]);
    end;
    grdCharacters.ColumnWidth[0]  := 30;
    grdCharacters.ColumnWidth[16] := 20;
    grdCharacters.Cells[0, 0]     := '00';
  finally
    grdCharacters.FocusCol := 1;
    grdCharacters.FocusRow := 1;
    grdCharacters.EndUpdate;
  end;
end;

procedure TCharMapForm.Button1Clicked(Sender: TObject);
begin
  Close;
end;

function TCharMapForm.GetNewText: TfpgString;
begin
  Result := edText.Text;
end;

procedure TCharMapForm.SetupCaptions;
begin
  WindowTitle   := rsCharacterMap;
  btnClose.Text := rsClose;
  lblText.Text  := rsTextToInsert;
end;

procedure TCharMapForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: CharMapForm}
  Name := 'CharMapForm';
  SetPosition(316, 186, 377, 390);
  WindowTitle := 'Character Map';
  Hint := '';
  WindowPosition := wpOneThirdDown;
  OnShow  := @FormShow;

  grdCharacters := TfpgStringGrid.Create(self);
  with grdCharacters do
  begin
    Name := 'grdCharacters';
    SetPosition(4, 4, 368, 296);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := False;
    TabOrder := 0;
    OnFocusChange := @grdCharactersFocusChange;
    OnDrawCell := @grdCharactersDrawCell;
    OnCanSelectCell := @grdCharactersCanSelectCell;
    OnDoubleClick  := @grdCharactersDoubleClick;
    OnKeyPress  := @grdCharactersKeyPressed;
  end;

  btnClose := TfpgButton.Create(self);
  with btnClose do
  begin
    Name := 'btnClose';
    SetPosition(292, 360, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'btnClose';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    OnClick   := @Button1Clicked;
  end;

  lblCharInfo := TfpgLabel.Create(self);
  with lblCharInfo do
  begin
    Name := 'lblCharInfo';
    SetPosition(4, 304, 268, 16);
    Anchors := [anLeft,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    Text := 'lblCharInfo';
  end;

  edText := TfpgEdit.Create(self);
  with edText do
  begin
    Name := 'edText';
    SetPosition(108, 326, 156, 24);
    Anchors := [anLeft,anBottom];
    Hint := '';
    TabOrder := 3;
    Text := '';
    FontDesc := '#Edit1';
  end;

  lblText := TfpgLabel.Create(self);
  with lblText do
  begin
    Name := 'lblText';
    SetPosition(4, 330, 100, 16);
    Anchors := [anLeft,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    Text := 'lblTextToInsert';
  end;

  pnlChar := TfpgPanel.Create(self);
  with pnlChar do
  begin
    Name := 'pnlChar';
    SetPosition(292, 304, 60, 48);
    Anchors := [anRight,anBottom];
    FontDesc := 'Arial-16:antialias=true';
    Hint := '';
    Style := bsLowered;
    Text := '';
  end;

  {@VFD_BODY_END: CharMapForm}
  {%endregion}

  SetupCaptions;
end;


{$ENDIF read_implementation}

