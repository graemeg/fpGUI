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
  gui_combobox;

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


var
  PaletteForm: TPaletteForm;

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

