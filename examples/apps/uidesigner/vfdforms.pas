{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      The main uiDesigner forms.
}

unit vfdforms;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  gfx_widget,
  gui_form,
  gui_label,
  gui_edit,
  gui_button,
  gui_listbox,
  gui_memo,
  gui_combobox,
  gui_checkbox;

type

  TVFDDialog = class(TfpgForm)
  protected
    procedure HandleKeyPress(var keycode: word; var shiftstate: TShiftState;  var consumed: boolean); override;
  end;


  TInsertCustomForm = class(TVFDDialog)
  public
    l1,
    l2: TfpgLabel;
    edClass: TfpgEdit;
    edName: TfpgEdit;
    btnOK,
    btnCancel: TfpgButton;
    procedure AfterCreate; override;
    procedure OnButtonClick(Sender: TObject);
  end;


  TNewFormForm = class(TVFDDialog)
  public
    l1: TfpgLabel;
    edName: TfpgEdit;
    btnOK,
    btnCancel: TfpgButton;
    procedure AfterCreate; override;
    procedure OnButtonClick(Sender: TObject);
  end;


  TEditPositionForm = class(TVFDDialog)
  public
    lbPos: TfpgLabel;
    edPos: TfpgEdit;
    btnOK,
    btnCancel: TfpgButton;
    procedure AfterCreate; override;
    procedure OnButtonClick(Sender: TObject);
  end;
  

  TWidgetOrderForm = class(TVFDDialog)
  public
    l1: TfpgLabel;
    list: TfpgListBox;
    btnUP,
    btnDOWN,
    btnOK,
    btnCancel: TfpgButton;
    procedure AfterCreate; override;
    procedure OnButtonClick(Sender: TObject);
  end;
  

  TPaletteForm = class(TfpgForm)
  public
    clab: TfpgLabel;
    clist: TfpgListBox;
    procedure AfterCreate; override;
  end;


  TfrmLoadSave = class(TfpgForm)
  public
    {@VFD_HEAD_BEGIN: frmLoadSave}
    lb1: TfpgLabel;
    edFileName: TfpgEdit;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    {@VFD_HEAD_END: frmLoadSave}
    procedure AfterCreate; override;
  end;


  TfrmVFDSetup = class(TfpgForm)
  public
    {@VFD_HEAD_BEGIN: frmVFDSetup}
    lb1: TfpgLabel;
    chlGrid: TfpgComboBox;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    {@VFD_HEAD_END: frmVFDSetup}
    procedure AfterCreate; override;
  end;


  TMainForm = class(TfpgForm)
  public
    l1: TfpgLabel;
    l2: TfpgLabel;
    edFormFile: TfpgEdit;
    btnSave: TfpgButton;
    btnLoad: TfpgButton;
    btnNewForm: TfpgButton;
    chlGrid: TfpgComboBox;
    procedure AfterCreate; override;
  end;


  TPropertyForm = class(TfpgForm)
  protected
    procedure HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
  public
    l1, l2, l3, l4, l5, l6, l7, l8: TfpgLabel;
    lbClass: TfpgLabel;
    edName: TfpgEdit;
    lbText: TfpgLabel;
    edText: TfpgEdit;
    btnEdit: TfpgButton;
    lbTop, lbLeft, lbWidth, lbHeight: TfpgLabel;
    btnTop, btnLeft, btnWidth, btnHeight: TfpgButton;
    cbAL, cbAT, cbAR, cbAB: TfpgCheckBox;
    edOther: TfpgMemo;
    procedure AfterCreate; override;
  end;


var
  PaletteForm: TPaletteForm;
 //MainForm : TMainForm;
 //PropertyForm : TPropertyForm;

function GridResolution: integer;

implementation

uses
  vfdmain,
  fpgfx;

function GridResolution: integer;
begin
  Result := maindsgn.GridResolution;
end;

{ TPaletteForm }

procedure TPaletteForm.AfterCreate;
begin
  inherited AfterCreate;
  WindowPosition := wpUser;
  WindowTitle := 'Palette';
  SetPosition(10, 450, 110, 220);

  clab := CreateLabel(self, 3, 3, 'Widget Palette:');

  clist         := TfpgListBox.Create(self);
  clist.Left    := 1;
  clist.Width   := Width - 2;
  clist.Top     := 22;
  clist.Height  := Height - clist.top - 2;
  clist.Anchors := [anLeft, anRight, anTop, anBottom];

  clist.Items.Add('-');
{
  clist.Items.Add('Label');
  clist.Items.Add('Edit');
  clist.Items.Add('Button');
  clist.Items.Add('CheckBox');
  clist.Items.Add('ComboBox');
  clist.Items.Add('Memo');
  clist.Items.Add('ListBox');
  clist.Items.Add('[OTHER]');
}
  clist.OnChange := @(maindsgn.OnPaletteChange);
end;

{ TPropertyForm }

procedure TPropertyForm.AfterCreate;
var
  x, x2, w, y, gap: integer;
begin
  inherited AfterCreate;
  WindowPosition := wpUser;
  WindowTitle := 'Properties';
  SetPosition(10, 80, 250, 320);

  x   := 3;
  x2  := x + 90;
  gap := 22;
  w := Width - x2 - 3;
  y := 3;

  l1      := CreateLabel(self, x, y, 'Class name:');
  lbClass := CreateLabel(self, x2, y, 'CLASS');
  lbClass.Width := w;
  lbClass.FontDesc := '#Label2';

  Inc(y, gap);

  l2           := CreateLabel(self, x, y, 'Name:');
  edName       := CreateEdit(self, x2, y, w, 0);
  edName.Text  := 'NAME';
  edName.Anchors := [anLeft, anRight, anTop];

  Inc(y, gap);
  lbText         := CreateLabel(self, x, y, 'Text/Items:');
  edText         := CreateEdit(self, x2, y, w, 0);
  edText.Text    := 'Text';
  edText.Anchors := [anLeft, anRight, anTop];

  btnEdit := CreateButton(self, x2, y, 100, 'Edit...', nil);

  Inc(y, 2 * gap);
  l3           := CreateLabel(self, x, y, 'Left:');
  lbLeft       := CreateLabel(self, x2, y, 'Left');
  lbLeft.Width := 50;
  btnLeft      := CreateButton(self, x2 + 50, y - 2, 30, '...', @maindsgn.OnPropPosEdit);
  btnLeft.Height := 20;
  Inc(y, gap);
  l4           := CreateLabel(self, x, y, 'Top:');
  lbTop        := CreateLabel(self, x2, y, 'Top');
  lbTop.Width  := 50;
  btnTop       := CreateButton(self, x2 + 50, y - 2, 30, '...', @maindsgn.OnPropPosEdit);
  btnTop.Height := 20;
  Inc(y, gap);
  l5           := CreateLabel(self, x, y, 'Width:');
  lbWidth      := CreateLabel(self, x2, y, 'w');
  lbWidth.Width := 50;
  btnWidth     := CreateButton(self, x2 + 50, y - 2, 30, '...', @maindsgn.OnPropPosEdit);
  btnWidth.Height := 20;
  Inc(y, gap);
  l6           := CreateLabel(self, x, y, 'Height:');
  lbHeight     := CreateLabel(self, x2, y, 'h');
  lbHeight.Width := 50;
  btnHeight    := CreateButton(self, x2 + 50, y - 2, 30, '...', @maindsgn.OnPropPosEdit);
  btnHeight.Height := 20;

  Inc(y, gap);
  l6 := CreateLabel(self, x, y, 'Anchors:');

  cbAL := CreateCheckBox(self, x2, y, 'L');
  cbAT := CreateCheckBox(self, x2 + 36, y, 'T');
  cbAR := CreateCheckBox(self, x2 + 2 * 36, y, 'R');
  cbAB := CreateCheckBox(self, x2 + 3 * 36, y, 'B');

  Inc(y, gap);
  l7 := CreateLabel(self, x, y, 'Other settings:');

  edOther          := TfpgMemo.Create(self);
  edOther.SetPosition(x, y + gap, self.Width - 2 * x, self.Height - x - y - gap);
  edOther.Anchors  := AllAnchors;
  edOther.FontDesc := '#Edit2';

  //  lbHeight := CreateLabel(self, x2,y, 'h');
  //  lbHeight.Width := w;

  edText.OnChange := @maindsgn.OnPropTextChange;
  edName.OnChange := @maindsgn.OnPropNameChange;

  cbAL.OnChange := @maindsgn.OnAnchorChange;
  cbAT.OnChange := @maindsgn.OnAnchorChange;
  cbAR.OnChange := @maindsgn.OnAnchorChange;
  cbAB.OnChange := @maindsgn.OnAnchorChange;

  edOther.OnChange  := @maindsgn.OnOtherChange;
  btnEdit.OnClick   := @maindsgn.OnEditWidget;
end;

procedure TPropertyForm.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if (keycode = keyEnter) or (keycode = keyF11) then
  begin
    if maindsgn.selectedform <> nil then
      maindsgn.selectedform.Form.SetFocus;
//      GfxActivateWindow(maindsgn.selectedform.Form.WinHandle);
    consumed := True;
  end
  else
    inherited;
end;

{ TMainForm }

procedure TMainForm.AfterCreate;
begin
  inherited AfterCreate;
  WindowPosition := wpUser;
  WindowTitle := 'fpGUI Designer';
  SetPosition(0, 0, 550, 50);

  l1 := CreateLabel(self, 5, 5, 'File name:');

  edFormFile       := CreateEdit(self, 5, 5 + 20, 250, 0);
//  edFormFile.Text := './aanewform.pas';

  btnSave := CreateButton(self, 270, 12, 50, 'Save', nil);
  btnLoad := CreateButton(self, 330, 12, 50, 'Load', nil);

  l1      := CreateLabel(self, 400, 5, 'Grid:');
  chlGrid := CreateComboBox(self, 400, 5 + 20, 50, nil);
  chlGrid.Items.Add('1');
  chlGrid.Items.Add('4');
  chlGrid.Items.Add('8');
  chlGrid.FocusItem := 2;

  btnNewForm := CreateButton(self, 460, 12, 80, 'New Form', nil);

  btnSave.OnClick     := @maindsgn.OnSaveFile;
  btnLoad.OnClick     := @maindsgn.OnLoadFile;
  btnNewForm.OnClick  := @ maindsgn.OnNewForm;
end;

{ TInsertCustomForm }

procedure TInsertCustomForm.AfterCreate;
begin
  inherited;
  WindowPosition := wpScreenCenter;
  WindowTitle := 'Insert Custom Widget';
  SetPosition(0, 0, 300, 100);

  l1        := CreateLabel(self, 8, 4, 'Class name:');
  edClass   := CreateEdit(self, 8, 24, 150, 0);
  edClass.Text := 'Tfpg';
  l2        := CreateLabel(self, 8, 48, 'Name:');
  edName    := CreateEdit(self, 8, 68, 150, 0);
  btnOK     := CreateButton(self, 180, 20, 100, 'OK', @OnButtonClick);
  btnCancel := CreateButton(self, 180, 52, 100, 'Cancel', @OnButtonClick);
end;

procedure TInsertCustomForm.OnButtonClick(Sender: TObject);
begin
  if Sender = btnOK then
    ModalResult := 1
  else
    ModalResult := 2;
end;

{ TNewFormForm }

procedure TNewFormForm.AfterCreate;
begin
  inherited AfterCreate;
  WindowPosition := wpScreenCenter;
  SetPosition(0, 0, 286, 66);
  WindowTitle := 'New Form';

  l1           := CreateLabel(self, 8, 8, 'Form name:');
  edName       := CreateEdit(self, 8, 28, 180, 0);
  edName.Text  := 'frm';
  btnOK        := CreateButton(self, 196, 8, 80, 'OK', @OnButtonClick);
  btnCancel    := CreateButton(self, 196, 36, 80, 'Cancel', @OnButtonClick);
end;

procedure TNewFormForm.OnButtonClick(Sender: TObject);
begin
  if Sender = btnOK then
    ModalResult := 1
  else
    ModalResult := 2;
end;

{ TEditPositionForm }

procedure TEditPositionForm.AfterCreate;
begin
  inherited AfterCreate;
  WindowPosition := wpScreenCenter;
  Width := 186;
  Height := 66;
  WindowTitle := 'Position';

  lbPos           := CreateLabel(self, 8, 8, 'Pos:      ');
  edPos           := CreateEdit(self, 8, 28, 80, 0);
  btnOK           := CreateButton(self, 96, 8, 80, 'OK', @OnButtonClick);
  btnCancel       := CreateButton(self, 96, 36, 80, 'Cancel', @OnButtonClick);
  btnOK.ImageName := 'stdimg.ok';
  btnOK.ShowImage := True;
  btnCancel.ImageName := 'stdimg.cancel';
  btnCancel.ShowImage := True;
end;

procedure TEditPositionForm.OnButtonClick(Sender: TObject);
begin
  if Sender = btnOK then
    ModalResult := 1
  else
    ModalResult := 2;
end;

{ TWidgetOrderForm }

procedure TWidgetOrderForm.AfterCreate;
begin
  inherited AfterCreate;
  WindowPosition := wpScreenCenter;
  SetPosition(0, 0, 322, 258);
  WindowTitle := 'Widget order';

  l1 := CreateLabel(self, 4, 4, 'Form widget order:');

  list := TfpgListBox.Create(self);
  list.SetPosition(4, 24, 220, 228);

  btnOK           := CreateButton(self, 232, 24, 80, 'OK', @OnButtonClick);
  btnOK.ImageName := 'stdimg.ok';
  btnOK.ShowImage := True;
  btnCancel       := CreateButton(self, 232, 52, 80, 'Cancel', @OnButtonClick);
  btnCancel.ImageName := 'stdimg.cancel';
  btnCancel.ShowImage := True;

  btnUP   := CreateButton(self, 232, 108, 80, 'UP', @OnButtonClick);
  btnDOWN := CreateButton(self, 232, 136, 80, 'DOWN', @OnButtonClick);
end;

procedure TWidgetOrderForm.OnButtonClick(Sender: TObject);
var
  i,
  n,
  myilev: integer;

  function IdentLevel(astr: string): integer;
  var
    s: string;
    f: integer;
  begin
    Result := 0;
    s      := astr;
    f      := 1;
    while (f <= length(s)) and (s[f] = ' ') do
    begin
      Inc(Result);
      Inc(f);
    end;
  end;

begin
  if Sender = btnOK then
    ModalResult := 1
  else if Sender = btnCancel then
    ModalResult := 2
  else
  begin
    // up / down
    i := list.FocusItem;
    if i < 1 then
      Exit;

    myilev := IdentLevel(list.Items[i - 1]);

    if Sender = btnUP then
    begin
      if (i > 1) and (IdentLevel(list.Items[i - 2]) = myilev) then
      begin
        list.Items.Move(i - 1, i - 2);

        n := i;
        while (n < list.Items.Count) and (IdentLevel(list.Items[n]) > myilev) do
        begin
          list.Items.Move(n, n - 1);
          Inc(n);
        end;

        list.FocusItem := i - 1;
      end;
    end
    else if Sender = btnDOWN then
      if (i < list.Items.Count) then
      begin
        //list.Items.Move(i-1,i);

        n := i;
        while (n < list.Items.Count) and (IdentLevel(list.Items[n]) > myilev) do
          Inc(n)//list.Items.Move(n,n-1);
        ;

        if (i = n) and (i < list.Items.Count - 1) and (IdentLevel(list.Items[i + 1]) > myilev) then
          Exit;

        if (n > list.Items.Count - 1) then
          Exit;

        while (n >= i) do
        begin
          list.Items.Move(n, n - 1);
          Dec(n);
        end;

        list.FocusItem := i + 1;
      end;
  end;
end;

{ TVFDDialogBase }

procedure TVFDDialog.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if keycode = keyEscape then
  begin
    ModalResult := 2;
    consumed    := True;
  end
  else
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

{ TfrmLoadSave }

procedure TfrmLoadSave.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmLoadSave}
  SetPosition(276, 141, 300, 95);
  WindowTitle := 'Form file';

  lb1 := TfpgLabel.Create(self);
  with lb1 do
  begin
    SetPosition(8, 8, 80, 16);
    Text     := 'File name:';
    FontDesc := '#Label1';
  end;

  edFileName := TfpgEdit.Create(self);
  with edFileName do
  begin
    SetPosition(8, 28, 280, 22);
    Anchors  := [anLeft, anRight, anTop];
    Text     := '';
    FontDesc := '#Edit1';
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    SetPosition(8, 60, 96, 24);
    Anchors     := [anLeft, anBottom];
    Text        := 'OK';
    FontDesc    := '#Label1';
    ImageName   := 'stdimg.ok';
    ShowImage   := True;
    ModalResult := 1;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    SetPosition(192, 60, 96, 24);
    Anchors     := [anRight, anBottom];
    Text        := 'Cancel';
    FontDesc    := '#Label1';
    ImageName   := 'stdimg.cancel';
    ShowImage   := True;
    ModalResult := -1;
  end;

  {@VFD_BODY_END: frmLoadSave}
end;

procedure TfrmVFDSetup.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmVFDSetup}
  SetPosition(322, 337, 237, 70);
  WindowTitle := 'General settings';

  lb1 := TfpgLabel.Create(self);
  with lb1 do
  begin
    SetPosition(8, 8, 92, 16);
    Text := 'Grid resolution:';
  end;

  chlGrid := TfpgComboBox.Create(self);
  with chlGrid do
  begin
    SetPosition(104, 4, 56, 22);
    items.Add('1');
    items.Add('4');
    items.Add('8');
    FocusItem := 2;
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    SetPosition(8, 40, 96, 24);
    Text        := 'OK';
    ImageName   := 'stdimg.ok';
    ShowImage   := True;
    ModalResult := 1;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    SetPosition(132, 40, 96, 24);
    Text        := 'Cancel';
    ImageName   := 'stdimg.cancel';
    ShowImage   := True;
    ModalResult := -1;
  end;

  {@VFD_BODY_END: frmVFDSetup}
end;


end.

