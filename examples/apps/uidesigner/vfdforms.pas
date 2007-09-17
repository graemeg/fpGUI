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
  gui_combobox,
  gui_trackbar,
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
    {@VFD_HEAD_BEGIN: WidgetOrderForm}
    l1: TfpgLabel;
    list: TfpgListBox;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    btnUp: TfpgButton;
    btnDown: TfpgButton;
    {@VFD_HEAD_END: WidgetOrderForm}
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
    procedure   OnButtonClick(Sender: TObject);
  end;
  

  TPaletteForm = class(TfpgForm)
  public
    clab: TfpgLabel;
    clist: TfpgListBox;
    procedure AfterCreate; override;
  end;


  TfrmVFDSetup = class(TfpgForm)
  private
    procedure   LoadSettings;
    procedure   SaveSettings;
    procedure   btnOKClick(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: frmVFDSetup}
    lb1: TfpgLabel;
    chlGrid: TfpgComboBox;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    lblRecentFiles: TfpgLabel;
    tbMRUFileCount: TfpgTrackBar;
    cbFullPath: TfpgCheckBox;
    lblName1: TfpgLabel;
    lblName2: TfpgLabel;
    {@VFD_HEAD_END: frmVFDSetup}
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;


var
  PaletteForm: TPaletteForm;

implementation

uses
  vfdmain,
  fpgfx,
  gui_iniutils;


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
  Sizeable := False;

  lbPos           := CreateLabel(self, 8, 8, 'Pos:      ');
  edPos           := CreateEdit(self, 8, 28, 80, 0);
  btnOK           := CreateButton(self, 100, 8, 75, 'OK', @OnButtonClick);
  btnCancel       := CreateButton(self, 100, 36, 75, 'Cancel', @OnButtonClick);
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

constructor TWidgetOrderForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'WidgetOrderForm';
  gINI.ReadFormState(self);
end;

destructor TWidgetOrderForm.Destroy;
begin
  gINI.WriteFormState(self);
  inherited Destroy;
end;

procedure TWidgetOrderForm.AfterCreate;
begin
  inherited AfterCreate;
  {@VFD_BODY_BEGIN: WidgetOrderForm}
  SetPosition(100, 100, 312, 258);
  WindowTitle := 'Widget order';
  WindowPosition := wpScreenCenter;

  l1 := TfpgLabel.Create(self);
  with l1 do
  begin
    SetPosition(4, 4, 108, 16);
    Text := 'Form widget order:';
    FontDesc := '#Label1';
  end;

  list := TfpgListBox.Create(self);
  with list do
  begin
    SetPosition(4, 24, 220, 228);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#List';
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    SetPosition(232, 24, 75, 24);
    Anchors := [anRight,anTop];
    Text := 'OK';
    FontDesc := '#Label1';
    ImageName := 'stdimg.ok';
    ModalResult := 0;
    OnClick := @OnButtonClick;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    SetPosition(232, 52, 75, 24);
    Anchors := [anRight,anTop];
    Text := 'Cancel';
    FontDesc := '#Label1';
    ImageName := 'stdimg.cancel';
    ModalResult := 0;
    OnClick := @OnButtonClick;
  end;

  btnUp := TfpgButton.Create(self);
  with btnUp do
  begin
    SetPosition(232, 108, 75, 24);
    Anchors := [anRight,anTop];
    Text := 'Up';
    FontDesc := '#Label1';
    ImageName := '';
    ModalResult := 0;
    OnClick := @OnButtonClick;
  end;

  btnDown := TfpgButton.Create(self);
  with btnDown do
  begin
    SetPosition(232, 136, 75, 24);
    Anchors := [anRight,anTop];
    Text := 'Down';
    FontDesc := '#Label1';
    ImageName := '';
    ModalResult := 0;
    OnClick := @OnButtonClick;
  end;

  {@VFD_BODY_END: WidgetOrderForm}
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

procedure TfrmVFDSetup.LoadSettings;
begin
  chlGrid.FocusItem := gINI.ReadInteger('Options', 'GridResolution', 2);
  tbMRUFileCount.Position := gINI.ReadInteger('Options', 'MRUFileCount', 4);
  cbFullPath.Checked := gINI.ReadBool('Options', 'ShowFullPath', True);
end;

procedure TfrmVFDSetup.SaveSettings;
begin
  gINI.WriteInteger('Options', 'GridResolution', chlGrid.FocusItem);
  gINI.WriteInteger('Options', 'MRUFileCount', tbMRUFileCount.Position);
  gINI.WriteBool('Options', 'ShowFullPath', cbFullPath.Checked);
end;

procedure TfrmVFDSetup.btnOKClick(Sender: TObject);
begin
  SaveSettings;
  ModalResult := 1;
end;

constructor TfrmVFDSetup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'frmVFDSetup';
  gINI.ReadFormState(self);
  LoadSettings;
end;

destructor TfrmVFDSetup.Destroy;
begin
  gINI.WriteFormState(self);
  inherited Destroy;
end;

procedure TfrmVFDSetup.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmVFDSetup}
  SetPosition(394, 399, 252, 184);
  WindowTitle := 'General settings';
  WindowPosition := wpScreenCenter;

  lb1 := TfpgLabel.Create(self);
  with lb1 do
  begin
    SetPosition(28, 28, 116, 16);
    Text := 'Grid resolution:';
    FontDesc := '#Label1';
  end;

  chlGrid := TfpgComboBox.Create(self);
  with chlGrid do
  begin
    SetPosition(140, 24, 88, 22);
    Items.Add('1');
    Items.Add('4');
    Items.Add('8');
    FontDesc := '#List';
    FocusItem := 0;
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    SetPosition(92, 154, 75, 24);
    Anchors := [anRight,anBottom];
    Text := 'OK';
    FontDesc := '#Label1';
    ImageName := 'stdimg.ok';
    ModalResult := 0;
    ShowImage := True;
    OnClick := @btnOKClick;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    SetPosition(171, 154, 75, 24);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    ImageName := 'stdimg.cancel';
    ModalResult := -1;
    ShowImage := True;
  end;

  lblRecentFiles := TfpgLabel.Create(self);
  with lblRecentFiles do
  begin
    SetPosition(28, 88, 136, 16);
    Text := 'Recent files count:';
    FontDesc := '#Label1';
  end;

  tbMRUFileCount := TfpgTrackBar.Create(self);
  with tbMRUFileCount do
  begin
    SetPosition(156, 80, 76, 30);
    Min := 2;
    Max := 10;
    Position := 4;
    Orientation := orHorizontal;
    ShowPosition := True;
  end;

  cbFullPath := TfpgCheckBox.Create(self);
  with cbFullPath do
  begin
    SetPosition(24, 108, 204, 20);
    Text := 'Show the full file path';
    FontDesc := '#Label1';
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    SetPosition(8, 8, 176, 16);
    Text := 'Form designer';
    FontDesc := '#Label2';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    SetPosition(8, 64, 232, 16);
    Text := 'Open Recent menu settings';
    FontDesc := '#Label2';
  end;

  {@VFD_BODY_END: frmVFDSetup}
end;


end.

