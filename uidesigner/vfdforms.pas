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
      The main uiDesigner forms.
}

unit vfdforms;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_widget,
  fpg_form,
  fpg_label,
  fpg_edit,
  fpg_button,
  fpg_listbox,
  fpg_combobox,
  fpg_trackbar,
  fpg_checkbox,
  fpg_panel;

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
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    procedure AfterCreate; override;
    procedure OnButtonClick(Sender: TObject);
  end;


  TNewFormForm = class(TVFDDialog)
  private
    procedure OnedNameKeyPressed(Sender: TObject; var KeyCode: word;
      var ShiftState: TShiftState; var Consumed: boolean);
  public
    l1: TfpgLabel;
    edName: TfpgEdit;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    procedure AfterCreate; override;
    procedure OnButtonClick(Sender: TObject);
  end;


  TEditPositionForm = class(TVFDDialog)
  private
    procedure edPosKeyPressed(Sender: TObject; var KeyCode: word;
      var ShiftState: TShiftState; var Consumed: boolean);
  public
    lbPos: TfpgLabel;
    edPos: TfpgEdit;
    btnOK: TfpgButton;
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
    chkFullPath: TfpgCheckBox;
    lblName1: TfpgLabel;
    lblName2: TfpgLabel;
    edtDefaultExt: TfpgEdit;
    lblName3: TfpgLabel;
    chkUndoOnExit: TfpgCheckBox;
    chkOneClick: TfpgCheckBox;
    Bevel1: TfpgBevel;
    Bevel2: TfpgBevel;
    Bevel3: TfpgBevel;
    {@VFD_HEAD_END: frmVFDSetup}
    procedure   AfterCreate; override;
    procedure   BeforeDestruction; override;
  end;



implementation

uses
  fpg_main,
  fpg_iniutils,
  fpg_constants,
  vfdprops; // used to get Object Inspector defaults


{ TInsertCustomForm }

procedure TInsertCustomForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
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
  {%endregion}
end;

procedure TInsertCustomForm.OnButtonClick(Sender: TObject);
begin
  if Sender = btnOK then
    ModalResult := mrOK
  else
    ModalResult := mrCancel;
end;

{ TNewFormForm }

procedure TNewFormForm.OnedNameKeyPressed(Sender: TObject; var KeyCode: word;
  var ShiftState: TShiftState; var Consumed: boolean);
begin
  if (KeyCode = keyEnter) or (KeyCode = keyPEnter) then
    btnOK.Click;
end;

procedure TNewFormForm.AfterCreate;
begin
  inherited AfterCreate;
  WindowPosition := wpScreenCenter;
  SetPosition(0, 0, 286, 66);
  WindowTitle := 'New Form';

  l1           := CreateLabel(self, 8, 8, 'Form name:');
  edName       := CreateEdit(self, 8, 28, 180, 0);
  edName.Text  := 'frm';
  edName.OnKeyPress := @OnedNameKeyPressed;
  btnOK        := CreateButton(self, 196, 8, 80, rsOK, @OnButtonClick);
  btnCancel    := CreateButton(self, 196, 36, 80, rsCancel, @OnButtonClick);
end;

procedure TNewFormForm.OnButtonClick(Sender: TObject);
begin
  if Sender = btnOK then
    ModalResult := mrOK
  else
    ModalResult := mrCancel;
end;

{ TEditPositionForm }

procedure TEditPositionForm.edPosKeyPressed(Sender: TObject; var KeyCode: word;
  var ShiftState: TShiftState; var Consumed: boolean);
begin
  if (KeyCode = keyEnter) or (KeyCode = keyPEnter) then
    btnOK.Click;
end;

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
  edPos.OnKeyPress := @edPosKeyPressed;
  btnOK           := CreateButton(self, 98, 8, 80, rsOK, @OnButtonClick);
  btnCancel       := CreateButton(self, 98, 36, 80, rsCancel, @OnButtonClick);
end;

procedure TEditPositionForm.OnButtonClick(Sender: TObject);
begin
  if Sender = btnOK then
    ModalResult := mrOK
  else
    ModalResult := mrCancel;
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
  Name := 'WidgetOrderForm';
  SetPosition(534, 173, 312, 258);
  WindowTitle := 'Widget order';
  Hint := '';
  WindowPosition := wpScreenCenter;

  l1 := TfpgLabel.Create(self);
  with l1 do
  begin
    Name := 'l1';
    SetPosition(4, 4, 108, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Form widget order:';
  end;

  list := TfpgListBox.Create(self);
  with list do
  begin
    Name := 'list';
    SetPosition(4, 24, 220, 228);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#List';
    Hint := '';
    HotTrack := False;
    PopupFrame := False;
    TabOrder := 1;
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    Name := 'btnOK';
    SetPosition(232, 24, 75, 24);
    Anchors := [anRight,anTop];
    Text := 'OK';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.ok';
    TabOrder := 2;
    OnClick := @OnButtonClick;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(232, 52, 75, 24);
    Anchors := [anRight,anTop];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.cancel';
    TabOrder := 3;
    OnClick := @OnButtonClick;
  end;

  btnUp := TfpgButton.Create(self);
  with btnUp do
  begin
    Name := 'btnUp';
    SetPosition(232, 108, 75, 24);
    Anchors := [anRight,anTop];
    Text := 'Up';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
    OnClick := @OnButtonClick;
  end;

  btnDown := TfpgButton.Create(self);
  with btnDown do
  begin
    Name := 'btnDown';
    SetPosition(232, 136, 75, 24);
    Anchors := [anRight,anTop];
    Text := 'Down';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 5;
    OnClick := @OnButtonClick;
  end;

  {@VFD_BODY_END: WidgetOrderForm}
end;

procedure TWidgetOrderForm.OnButtonClick(Sender: TObject);
var
  i: integer;
  n: integer;
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
    if i < 0 then
      Exit;

    myilev := IdentLevel(list.Items[i]);

    if Sender = btnUP then
    begin
      if (i > 0) and (IdentLevel(list.Items[i - 1]) = myilev) then
      begin
        list.Items.Move(i, i - 1);

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
      if (i < list.Items.Count-1) then
      begin
        n := i;
        while (n < list.Items.Count) and (IdentLevel(list.Items[n]) > myilev) do
          Inc(n);

        if (i = n) and (i < list.Items.Count-1) and (IdentLevel(list.Items[i]) > myilev) then
          Exit;

        if (n > list.Items.Count-1) then
          Exit; //==>

        while (n >= i) do
        begin
          list.Items.Move(n, n + 1);
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
    consumed    := True;
    ModalResult := mrCancel;
  end;
  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfrmVFDSetup.LoadSettings;
begin
  chlGrid.FocusItem       := gINI.ReadInteger('Options', 'GridResolution', 2);
  tbMRUFileCount.Position := gINI.ReadInteger('Options', 'MRUFileCount', 4);
  chkFullPath.Checked     := gINI.ReadBool('Options', 'ShowFullPath', True);
  edtDefaultExt.Text      := gINI.ReadString('Options', 'DefaultFileExt', '.pas');
  chkUndoOnExit.Checked   := gINI.ReadBool('Options', 'UndoOnExit', UndoOnPropExit);
  chkOneClick.Checked     := gINI.ReadBool('Options', 'OneClickMove', True);
end;

procedure TfrmVFDSetup.SaveSettings;
begin
  gINI.WriteInteger('Options', 'GridResolution', chlGrid.FocusItem);
  gINI.WriteInteger('Options', 'MRUFileCount', tbMRUFileCount.Position);
  gINI.WriteBool('Options', 'ShowFullPath', chkFullPath.Checked);
  gINI.WriteString('Options', 'DefaultFileExt', edtDefaultExt.Text);
  gINI.WriteBool('Options', 'UndoOnExit', chkUndoOnExit.Checked);
  gINI.WriteBool('Options', 'OneClickMove', chkOneClick.Checked);
end;

procedure TfrmVFDSetup.btnOKClick(Sender: TObject);
begin
  SaveSettings;
  ModalResult := mrOK;
end;

procedure TfrmVFDSetup.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmVFDSetup}
  Name := 'frmVFDSetup';
  SetPosition(392, 386, 398, 283);
  WindowTitle := 'General settings';
  Hint := '';
  WindowPosition := wpScreenCenter;
  gINI.ReadFormState(self); // after form created but before creating widgets

  lb1 := TfpgLabel.Create(self);
  with lb1 do
  begin
    Name := 'lb1';
    SetPosition(28, 32, 112, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Grid resolution:';
  end;

  chlGrid := TfpgComboBox.Create(self);
  with chlGrid do
  begin
    Name := 'chlGrid';
    SetPosition(144, 28, 88, 22);
    FontDesc := '#List';
    Hint := '';
    Items.Add('1');
    Items.Add('4');
    Items.Add('8');
    TabOrder := 1;
    FocusItem := 0;
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    Name := 'btnOK';
    SetPosition(238, 253, 75, 24);
    Anchors := [anRight,anBottom];
    Text := 'OK';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.ok';
    TabOrder := 6;
    OnClick := @btnOKClick;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(317, 253, 75, 24);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.cancel';
    ModalResult := -1;
    TabOrder := 7;
  end;

  lblRecentFiles := TfpgLabel.Create(self);
  with lblRecentFiles do
  begin
    Name := 'lblRecentFiles';
    SetPosition(28, 132, 124, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Recent files count:';
  end;

  tbMRUFileCount := TfpgTrackBar.Create(self);
  with tbMRUFileCount do
  begin
    Name := 'tbMRUFileCount';
    SetPosition(156, 124, 76, 30);
    Hint := '';
    Max := 10;
    Min := 2;
    Position := 4;
    ShowPosition := True;
    TabOrder := 3;
  end;

  chkFullPath := TfpgCheckBox.Create(self);
  with chkFullPath do
  begin
    Name := 'chkFullPath';
    SetPosition(24, 156, 204, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 4;
    Text := 'Show the full file path';
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(8, 8, 176, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Form designer';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(8, 108, 232, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Open Recent menu settings';
  end;

  edtDefaultExt := TfpgEdit.Create(self);
  with edtDefaultExt do
  begin
    Name := 'edtDefaultExt';
    SetPosition(28, 216, 68, 24);
    Hint := '';
    TabOrder := 5;
    Text := '';
    FontDesc := '#Edit1';
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(12, 192, 152, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Default file extension';
  end;

  chkUndoOnExit := TfpgCheckBox.Create(self);
  with chkUndoOnExit do
  begin
    Name := 'chkUndoOnExit';
    SetPosition(24, 56, 204, 18);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 2;
    Text := 'Undo on property editor exit';
  end;

  chkOneClick := TfpgCheckBox.Create(self);
  with chkOneClick do
  begin
    Name := 'chkOneClick';
    SetPosition(24, 76, 224, 20);
    Checked := True;
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 12;
    Text := 'One click select and move';
  end;

  Bevel1 := TfpgBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(108, 4, 280, 14);
    Hint := '';
    Style := bsLowered;
    Shape := bsBottomLine;
  end;

  Bevel2 := TfpgBevel.Create(self);
  with Bevel2 do
  begin
    Name := 'Bevel2';
    SetPosition(192, 104, 196, 14);
    Hint := '';
    Style := bsLowered;
    Shape := bsBottomLine;
  end;

  Bevel3 := TfpgBevel.Create(self);
  with Bevel3 do
  begin
    Name := 'Bevel3';
    SetPosition(156, 188, 232, 14);
    Hint := '';
    Style := bsLowered;
    Shape := bsBottomLine;
  end;

  {@VFD_BODY_END: frmVFDSetup}

  LoadSettings;
end;

procedure TfrmVFDSetup.BeforeDestruction;
begin
  // We don't put this in SaveSettings because it needs to be called even if
  // user cancels the dialog with btnCancel or ESC key press.
  gINI.WriteFormState(self);
  inherited BeforeDestruction;
end;


end.

