{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2009 See the file AUTHORS.txt, included in this
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
    StringGrid1: TfpgStringGrid;
    Button1: TfpgButton;
    lblCharInfo: TfpgLabel;
    edText: TfpgEdit;
    lblText: TfpgLabel;
    pnlChar: TfpgPanel;
    {@VFD_HEAD_END: CharMapForm}
    procedure FormShow(Sender: TObject);
    procedure StringGrid1FocusChange(Sender: TObject; ARow, ACol: integer);
    procedure StringGrid1DrawCell(Sender: TObject; const ARow, ACol: integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
    procedure StringGrid1CanSelectCell(Sender: TObject; const ARow, ACol: integer; var ACanSelect: boolean);
    procedure StringGrid1DoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure FillCharMap;
    procedure Button1Clicked(Sender: TObject);
    function GetNewText: TfpgString;
  public
    procedure AfterCreate; override;
    property  NewText: TfpgString read GetNewText;
  end;

function fpgShowCharMap: TfpgString;

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

procedure TCharMapForm.StringGrid1FocusChange(Sender: TObject; ARow, ACol: integer);
var
  i: integer;
  tmp, tmp2: TfpgString;
begin
  if (ARow > 0) and (ACol > 0) then
  begin
    tmp  := StringGrid1.Cells[ACol, ARow];
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

procedure TCharMapForm.StringGrid1DrawCell(Sender: TObject;
  const ARow, ACol: integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState;
  var ADefaultDrawing: boolean);
begin
  if (ARow = 0) or (ACol = 0) then
  begin
    ADefaultDrawing := False;
    StringGrid1.Canvas.Color := clWindowBackground;
    StringGrid1.Canvas.FillRectangle(ARect);
    //StringGrid1.Canvas.DrawButtonFace(ARect, []);
    StringGrid1.Canvas.TextColor := clText1;  //clGray;
    StringGrid1.Canvas.DrawText(ARect, StringGrid1.Cells[ACol, ARow],
      [txtHCenter, txtVCenter]);
  end
  else
    ADefaultDrawing := True;
end;

procedure TCharMapForm.StringGrid1CanSelectCell(Sender: TObject;
  const ARow, ACol: integer; var ACanSelect: boolean);
begin
  if (ACol = 0) or (ARow = 0) then
    ACanSelect := False
  else
    ACanSelect := True;
end;

procedure TCharMapForm.StringGrid1DoubleClick(Sender: TObject;
  AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  edText.Text := edText.Text + StringGrid1.Cells[StringGrid1.FocusCol, StringGrid1.FocusRow];
end;

procedure TCharMapForm.FillCharMap;
var
  i: byte;
  j: byte;
  c: byte;
begin
  StringGrid1.BeginUpdate;
  try
    StringGrid1.ColumnCount := 17;
    StringGrid1.RowCount    := 17;
    StringGrid1.ShowHeader  := False;
    for i := 0 to 15 do
    begin
      for j := 0 to 15 do
      begin
        StringGrid1.ColumnWidth[j] := 20;
        c := i shl 4 or j;
        if (c > 0) and (c < 128) then
          StringGrid1.Cells[j + 1, i + 1] := chr(c)
        else
          StringGrid1.Cells[j + 1, i + 1] :=
            chr($C0 or (i div $4)) + chr($80 or c mod $40);
      end;
      StringGrid1.Cells[0, i + 1] := Format('%.2x +', [i]);
      StringGrid1.Cells[i + 1, 0] := Format('%.2x', [i]);
    end;
    StringGrid1.ColumnWidth[0]  := 30;
    StringGrid1.ColumnWidth[16] := 20;
    StringGrid1.Cells[0, 0]     := '00';
  finally
    StringGrid1.FocusCol := 1;
    StringGrid1.FocusRow := 1;
    StringGrid1.EndUpdate;
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

procedure TCharMapForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: CharMapForm}
  Name := 'CharMapForm';
  SetPosition(316, 186, 377, 390);
  WindowTitle := 'Character Map';
  WindowPosition := wpAuto;
  OnShow  := @FormShow;

  StringGrid1 := TfpgStringGrid.Create(self);
  with StringGrid1 do
  begin
    Name := 'StringGrid1';
    SetPosition(4, 4, 368, 296);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    RowCount := 0;
    RowSelect := False;
    TabOrder := 0;
    OnFocusChange := @StringGrid1FocusChange;
    OnDrawCell := @StringGrid1DrawCell;
    OnCanSelectCell := @StringGrid1CanSelectCell;
    OnDoubleClick  := @StringGrid1DoubleClick;
  end;

  Button1 := TfpgButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(292, 360, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Close';
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
    Text := 'Label';
  end;

  edText := TfpgEdit.Create(self);
  with edText do
  begin
    Name := 'edText';
    SetPosition(108, 326, 156, 24);
    Anchors := [anLeft,anBottom];
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
    Text := 'Text to Insert:';
  end;

  pnlChar := TfpgPanel.Create(self);
  with pnlChar do
  begin
    Name := 'pnlChar';
    SetPosition(292, 304, 60, 48);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := 'Arial-16:antialias=true';
    Style := bsLowered;
    Text := '';
  end;

  {@VFD_BODY_END: CharMapForm}
  {%endregion}
end;


{$ENDIF read_implementation}

