{ 
This is the extended version of fpGUI uidesigner.
With window list, undo feature, integration into IDE, editor launcher,...
Fred van Stappen
fiens@hotmail.com
}
{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2013 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit defines various forms/dialogs used in the UI Designer.
}

unit vfdforms;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_form,
  fpg_label,
  fpg_edit,
  fpg_button,
  fpg_Editbtn,
  fpg_combobox,
  fpg_RadioButton,
  fpg_dialogs,
  fpg_trackbar,
  fpg_checkbox,
  newformdesigner,
  fpg_tree;

type

  TVFDDialog = class(TfpgForm)
  protected
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   SetupCaptions; virtual;
    procedure   FormShow(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TInsertCustomForm = class(TVFDDialog)
  protected
    procedure   SetupCaptions; override;
  public
    l1,
    l2: TfpgLabel;
    edClass: TfpgEdit;
    edName: TfpgEdit;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    procedure   AfterCreate; override;
    procedure   OnButtonClick(Sender: TObject);
  end;

  TNewFormForm = class(TVFDDialog)
  private
    procedure   OnedNameKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
  protected
    procedure   SetupCaptions; override;
  public
    l1: TfpgLabel;
    edName: TfpgEdit;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    procedure   AfterCreate; override;
    procedure   OnButtonClick(Sender: TObject);
  end;


   TEditPositionForm = class(TVFDDialog)
  private
    procedure   edPosKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
  protected
    procedure   SetupCaptions; override;
  public
    lbPos: TfpgLabel;
    edPos: TfpgEdit;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    procedure   AfterCreate; override;
    procedure   OnButtonClick(Sender: TObject);
  end;

  TWidgetOrderForm = class(TVFDDialog)
  private
    function    GetTitle: string;
    procedure   SetTitle(const AValue: string);
  protected
    procedure   SetupCaptions; override;
  public
    {@VFD_HEAD_BEGIN: WidgetOrderForm}
    lblTitle: TfpgLabel;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    btnUp: TfpgButton;
    btnDown: TfpgButton;
    TreeView1: TfpgTreeView;
    {@VFD_HEAD_END: WidgetOrderForm}
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
    procedure   OnButtonClick(Sender: TObject);
    property    Title: string read GetTitle write SetTitle;
  end;

  TfrmVFDSetup = class(TfpgForm)
  private
    FINIVersion: integer;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure btnOKClick(Sender: TObject);
    procedure setAlwaysToFront(Sender: TObject);
    procedure IdeIntegration(Sender: TObject);
    procedure UndoLook(Sender: TObject);

  public
    {@VFD_HEAD_BEGIN: frmVFDSetup}
    lb1: TfpgLabel;
    chlGrid: TfpgComboBox;
    btnOK: TfpgButton;
    lblRecentFiles: TfpgLabel;
    tbMRUFileCount: TfpgTrackBar;
    chkFullPath: TfpgCheckBox;
    lblName1: TfpgLabel;
    lblName2: TfpgLabel;
    edtDefaultExt: TfpgEdit;
    lblName3: TfpgLabel;
    chkUndoOnExit: TfpgCheckBox;
    chkOneClick: TfpgCheckBox;
    Label1: TfpgLabel;
    chkCodeRegions: TfpgCheckBox;
    cbIndentationType: TfpgComboBox;
    lblIndentType: TfpgLabel;
    chkAlwaystoFront: TfpgCheckBox;
    pathini: TfpgLabel;
    Label2: TfpgLabel;
    rbTyphon: TfpgRadioButton;
    rbLaz: TfpgRadioButton;
    rbNone: TfpgRadioButton;
    WidgetOrderForm: TfpgLabel;
    rbedit0: TfpgRadioButton;
    rbedit2: TfpgRadioButton;
    rbedit3: TfpgRadioButton;
    FilenameEdit1: TfpgFileNameEdit;
    rbedit4: TfpgRadioButton;
    Label3: TfpgLabel;
    CheckBox1: TfpgCheckBox;
    Label4: TfpgLabel;
    TrackBarUndo: TfpgTrackBar;
    {@VFD_HEAD_END: frmVFDSetup}
    procedure AfterCreate; override;
    // procedure BeforeDestruction; override;
  end;


implementation

uses
  fpg_main,
  fpg_iniutils,
  fpg_constants,
  fpg_utils,
  vfd_constants,
  vfdprops; // used to get Object Inspector defaults

//const
 // cDesignerINIVersion = 1;

var
  changeide: integer;


{ TInsertCustomForm }

procedure TInsertCustomForm.SetupCaptions;
begin
  inherited SetupCaptions;
  WindowTitle := rsDlgInsertCustomWidget;
  l1.Text := fpgAddColon(rsNewClassName);
  l2.Text := fpgAddColon(rsName);
end;

procedure TInsertCustomForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  inherited;
  WindowPosition := wpScreenCenter;
  WindowTitle := 'TInsertCustomForm';
  SetPosition(0, 0, 300, 100);

  l1        := CreateLabel(self, 8, 4, 'Class name:');
  edClass   := CreateEdit(self, 8, 24, 150, 0);
  edClass.Text := 'Tfpg';
  l2        := CreateLabel(self, 8, 48, 'Name:');
  edName    := CreateEdit(self, 8, 68, 150, 0);
  btnOK     := CreateButton(self, 180, 20, 100, rsOK, @OnButtonClick);
  btnCancel := CreateButton(self, 180, 52, 100, rsCancel, @OnButtonClick);
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

procedure TNewFormForm.SetupCaptions;
begin
  inherited SetupCaptions;
  WindowTitle := rsDlgNewForm;
  l1.Text := fpgAddColon(rsNewFormName);
end;

procedure TNewFormForm.AfterCreate;
begin
  inherited AfterCreate;
  WindowPosition := wpScreenCenter;
  SetPosition(0, 0, 286, 66);
  WindowTitle := 'TNewFormForm';

  l1           := CreateLabel(self, 8, 8, 'Form name:');
  edName       := CreateEdit(self, 8, 28, 180, 0);
  edName.Text  := '';
  edName.OnKeyPress := @OnedNameKeyPressed;
  btnOK        := CreateButton(self, 196, 8, 80, rsOK, @OnButtonClick);
  btnCancel    := CreateButton(self, 196, 36, 80, rsCancel, @OnButtonClick);
end;

procedure TNewFormForm.OnButtonClick(Sender: TObject);
begin
  if Sender = btnOK then
    ModalResult := mrOk
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

procedure TEditPositionForm.SetupCaptions;
begin
  inherited SetupCaptions;
  WindowTitle := rsDlgEditFormPosition;
  lbPos.Text := fpgAddColon(rsPosition);
end;

procedure TEditPositionForm.AfterCreate;
begin
  inherited AfterCreate;
  WindowPosition := wpScreenCenter;
  Width := 186;
  Height := 66;
  WindowTitle := 'TEditPositionForm';
  Sizeable := False;

  lbPos           := CreateLabel(self, 8, 8, 'Pos:');
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

function TWidgetOrderForm.GetTitle: string;
begin
  Result := lblTitle.Text;
end;

procedure TWidgetOrderForm.SetTitle(const AValue: string);
begin
  lblTitle.Text := Format(lblTitle.Text, [AValue]);
end;

procedure TWidgetOrderForm.SetupCaptions;
begin
  inherited SetupCaptions;
  WindowTitle := rsDlgWidgetOrder;
  lblTitle.Text := fpgAddColon(rsFormTitle);
  btnOK.Text := rsOK;
  btnCancel.Text := rsCancel;
  btnUp.Text := rsUp;
  btnDown.Text := rsDown;
end;

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
  SetPosition(534, 173, 426, 398);
  WindowTitle := 'TWidgetOrderForm';
  Hint := '';
  WindowPosition := wpScreenCenter;

  lblTitle := TfpgLabel.Create(self);
  with lblTitle do
  begin
    Name := 'lblTitle';
    SetPosition(4, 4, 248, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Form %s:';
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    Name := 'btnOK';
    SetPosition(346, 24, 75, 24);
    Anchors := [anRight,anTop];
    Text := 'OK';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.ok';
    ModalResult := mrOK;
    TabOrder := 2;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(346, 52, 75, 24);
    Anchors := [anRight,anTop];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.cancel';
    ModalResult := mrCancel;
    TabOrder := 3;
  end;

  btnUp := TfpgButton.Create(self);
  with btnUp do
  begin
    Name := 'btnUp';
    SetPosition(346, 108, 75, 24);
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
    SetPosition(346, 136, 75, 24);
    Anchors := [anRight,anTop];
    Text := 'Down';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 5;
    OnClick := @OnButtonClick;
  end;

  TreeView1 := TfpgTreeView.Create(self);
  with TreeView1 do
  begin
    Name := 'TreeView1';
    SetPosition(4, 24, 336, 368);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 7;
  end;

  {@VFD_BODY_END: WidgetOrderForm}
end;

procedure TWidgetOrderForm.OnButtonClick(Sender: TObject);
var
  lNode: TfpgTreeNode;
begin
  lNode := Treeview1.Selection;
  if lNode = nil then
    exit;
  
  if Sender = btnUp then
  begin
    if lNode.Prev = nil then
      exit; // nothing to do
    lNode.MoveTo(lNode.Prev, naInsert);
  end
  else
  begin   // btnDown
    if (lNode.Next = nil) then
      exit;  // nothing to do
    if (lNode.Next.Next = nil) then // the last node doesn't have a next
      lNode.MoveTo(lNode.Next, naAdd)
    else
      lNode.MoveTo(lNode.Next.Next, naInsert);  
  end;
  
  Treeview1.Invalidate;
end;

{ TVFDDialogBase }

procedure TVFDDialog.FormShow(Sender: TObject);
begin
  SetupCaptions;
end;

procedure TVFDDialog.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if keycode = keyEscape then
  begin
    consumed    := True;
    ModalResult := mrCancel;
  end;
  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TVFDDialog.SetupCaptions;
begin
  // to be implemented in descendants
end;

constructor TVFDDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnShow := @FormShow;
end;

{ TfrmVFDSetup}

procedure TfrmVFDSetup.UndoLook(Sender: TObject);
begin
  frmMain.Hide;
  if checkbox1.Checked = False then
    frmMain.MainMenu.MenuItem(1).Visible := True
  else
    frmMain.MainMenu.MenuItem(1).Visible := False;
  frmMain.Show;
end;

procedure TfrmVFDSetup.IdeIntegration(Sender: TObject);
begin
  if Sender = rbnone then
  begin
    rbnone.Checked := True;
    frmMain.btnOpen.Visible := True;
    frmMain.btnSave.Left := 69;
    frmMain.btnSave.UpdateWindowPosition;
    frmMain.btnToFront.Left := 97;
    frmMain.btnToFront.UpdateWindowPosition;
    WindowAttributes := [];
    frmMain.filemenu.MenuItem(0).Visible := True;
    frmMain.filemenu.MenuItem(1).Visible := True;
    // frmMain.filemenu.MenuItem(8).Visible:=true;
    frmMain.filemenu.MenuItem(2).Visible := True;
    // frmMain.filemenu.MenuItem(9).Visible:=true;
  end;

  if Sender = rbtyphon then
  begin
    rbtyphon.Checked := True;
    frmmain.LoadIDEparameters(2);
  end;

  if Sender = rblaz then
  begin
    rblaz.Checked := True;
    frmmain.LoadIDEparameters(1);
  end;
end;

procedure TfrmVFDSetup.LoadSettings;
var
  x: integer;
begin
  fpgapplication.ProcessMessages;

  //x := gINI.ReadInteger('Options', 'IDE', 0);

  changeIde := idetemp;

  case idetemp of
    0: rbnone.Checked := True;
    1: rblaz.Checked := True;
    2: rbtyphon.Checked := True;
  end;

  x := gINI.ReadInteger('Options', 'Editor', 0);

  case x of
    0: rbedit0.Checked := True;
    2: rbedit2.Checked := True;
    3: rbedit3.Checked := True;
    4: rbedit4.Checked := True;
  end;

  TrackBarUndo.Position := gINI.ReadInteger('Options', 'MaxUndo', 10);
  CheckBox1.Checked := gINI.ReadBool('Options', 'EnableUndo', True);
  maxundo := TrackBarUndo.Position;
  FINIVersion := gINI.ReadInteger('Designer', 'Version', 0);
  chlGrid.FocusItem := gINI.ReadInteger('Options', 'GridResolution', 2);
  tbMRUFileCount.Position := gINI.ReadInteger('Options', 'MRUFileCount', 4);
  chkFullPath.Checked := gINI.ReadBool('Options', 'ShowFullPath', True);
  edtDefaultExt.Text := gINI.ReadString('Options', 'DefaultFileExt', '.pas');
  chkUndoOnExit.Checked := gINI.ReadBool('Options', 'UndoOnExit', UndoOnPropExit);
  chkOneClick.Checked := gINI.ReadBool('Options', 'OneClickMove', True);
  chkCodeRegions.Checked := gINI.ReadBool('Options', 'UseCodeRegions', True);
  chkAlwaystoFront.Checked := gINI.ReadBool('Options', 'AlwaystoFront', False);
  cbIndentationType.FocusItem := gINI.ReadInteger('Options', 'IndentationType', 0);
  cbIndentationType.FocusItem := gINI.ReadInteger('Options', 'IndentationType', 0);
  FilenameEdit1.FileName := gINI.ReadString('Options', 'CustomEditor', '');
  ;
end;

procedure TfrmVFDSetup.SaveSettings;
begin
  fpgapplication.ProcessMessages;
  if rbnone.Checked = True then
    gINI.WriteString('Path', 'Application', '')
  else
    gINI.WriteString('Path', 'Application', ParamStr(0));

  gINI.WriteInteger('Designer', 'Version', cDesignerINIVersion);
  gINI.WriteInteger('Options', 'GridResolution', chlGrid.FocusItem);
  gINI.WriteInteger('Options', 'MRUFileCount', tbMRUFileCount.Position);
  gINI.WriteInteger('Options', 'MaxUndo', TrackBarUndo.Position);
  gINI.WriteBool('Options', 'EnableUndo', CheckBox1.Checked);
  maxundo := TrackBarUndo.Position;
  enableundo := checkBox1.Checked;

  gINI.WriteBool('Options', 'ShowFullPath', chkFullPath.Checked);
  gINI.WriteString('Options', 'DefaultFileExt', edtDefaultExt.Text);
  gINI.WriteBool('Options', 'UndoOnExit', chkUndoOnExit.Checked);
  gINI.WriteBool('Options', 'OneClickMove', chkOneClick.Checked);
  gINI.WriteBool('Options', 'UseCodeRegions', chkCodeRegions.Checked);
  gINI.WriteBool('Options', 'AlwaystoFront', chkAlwaystoFront.Checked);
  gINI.WriteInteger('Options', 'IndentationType', cbIndentationType.FocusItem);
  gINI.WriteString('Options', 'CustomEditor', FilenameEdit1.FileName);

  if rbedit0.Checked = True then
    gINI.WriteInteger('Options', 'Editor', 0);

  if rbedit2.Checked = True then
    gINI.WriteInteger('Options', 'Editor', 2);

  if rbedit3.Checked = True then
    gINI.WriteInteger('Options', 'Editor', 3);

  if rbedit4.Checked = True then
    gINI.WriteInteger('Options', 'Editor', 4);


  if rbnone.Checked = True then
    idetemp := 0;
  if rblaz.Checked = True then
    idetemp := 1;
  if rbtyphon.Checked = True then
    idetemp := 2;

  if idetemp <> changeide then
    case idetemp of
      0: ShowMessage('IDE des-integration will append after closing application');
      1: ShowMessage('IDE integration will append next run of Lazarus');
      2: ShowMessage('IDE integration will append next run of Typhon');
    end;

end;

procedure TfrmVFDSetup.btnOKClick(Sender: TObject);
begin
  SaveSettings;
  ModalResult := mrOk;
end;



procedure TfrmVFDSetup.SetAlwaysToFront(Sender: TObject);
begin
  if tag = 1 then
  begin
    hide;
    fpgapplication.ProcessMessages;
    gINI.WriteBool('Options', 'AlwaystoFront', chkAlwaystoFront.Checked);
    if chkAlwaystoFront.Checked = False then
      frmMain.onnevertofront(Sender)
    else
      frmMain.onalwaystofront(Sender);
    if rblaz.Checked = True then
      IdeIntegration(rblaz);
    if rbtyphon.Checked = True then
      IdeIntegration(rbtyphon);
    fpgapplication.ProcessMessages;
    Show;
  end;

end;

procedure TfrmVFDSetup.AfterCreate;
var
  dataf: string;
begin
  {@VFD_BODY_BEGIN: frmVFDSetup}
  Name := 'frmVFDSetup';
  SetPosition(257, 140, 549, 350);
  WindowTitle := 'General settings';
  Hint := '';
  ShowHint := True;
  WindowPosition := wpScreenCenter;
  MinHeight := 350;
  MinWidth := 549;
  Tag := 0;

  lb1 := TfpgLabel.Create(self);
  with lb1 do
  begin
    Name := 'lb1';
    SetPosition(32, 24, 100, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Grid resolution:';
  end;

  chlGrid := TfpgComboBox.Create(self);
  with chlGrid do
  begin
    Name := 'chlGrid';
    SetPosition(144, 20, 52, 24);
    ExtraHint := '';
    FontDesc := '#List';
    Hint := '';
    Items.Add('1');
    Items.Add('4');
    Items.Add('8');
    FocusItem := -1;
    TabOrder := 1;
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    Name := 'btnOK';
    SetPosition(233, 319, 75, 24);
    Anchors := [anRight, anBottom];
    Text := 'OK';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.ok';
    TabOrder := 6;
    OnClick := @btnOKClick;
  end;

  lblRecentFiles := TfpgLabel.Create(self);
  with lblRecentFiles do
  begin
    Name := 'lblRecentFiles';
    SetPosition(32, 144, 124, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Recent files count:';
  end;

  tbMRUFileCount := TfpgTrackBar.Create(self);
  with tbMRUFileCount do
  begin
    Name := 'tbMRUFileCount';
    SetPosition(160, 144, 56, 22);
    Hint := '';
    Max := 10;
    Min := 2;
    Position := 4;
    Position := 4;
    ShowPosition := True;
    TabOrder := 3;
  end;

  chkFullPath := TfpgCheckBox.Create(self);
  with chkFullPath do
  begin
    Name := 'chkFullPath';
    SetPosition(32, 168, 172, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 4;
    Text := 'Show the full file path';
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(12, 4, 164, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Form designer';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(12, 120, 232, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Open Recent menu settings';
  end;

  edtDefaultExt := TfpgEdit.Create(self);
  with edtDefaultExt do
  begin
    Name := 'edtDefaultExt';
    SetPosition(240, 212, 52, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 5;
    Text := '';
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(48, 200, 68, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Various';
  end;

  chkUndoOnExit := TfpgCheckBox.Create(self);
  with chkUndoOnExit do
  begin
    Name := 'chkUndoOnExit';
    SetPosition(32, 48, 204, 18);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 2;
    Text := 'Undo on property editor exit';
  end;

  chkOneClick := TfpgCheckBox.Create(self);
  with chkOneClick do
  begin
    Name := 'chkOneClick';
    SetPosition(32, 68, 216, 20);
    Checked := True;
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 12;
    Text := 'One click select and move';
  end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(88, 216, 144, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Default file extension:';
  end;

  chkCodeRegions := TfpgCheckBox.Create(self);
  with chkCodeRegions do
  begin
    Name := 'chkCodeRegions';
    SetPosition(88, 236, 328, 20);
    FontDesc := '#Label1';
    Hint := 'Applies to new form/dialogs only';
    TabOrder := 18;
    Text := 'Use code-folding regions in auto-generated code';
  end;

  cbIndentationType := TfpgComboBox.Create(self);
  with cbIndentationType do
  begin
    Name := 'cbIndentationType';
    SetPosition(288, 256, 152, 24);
    ExtraHint := '';
    FontDesc := '#List';
    Hint := '';
    Items.Add('Space characters');
    Items.Add('Tab characters');
    FocusItem := 0;
    TabOrder := 16;
  end;

  lblIndentType := TfpgLabel.Create(self);
  with lblIndentType do
  begin
    Name := 'lblIndentType';
    SetPosition(92, 260, 192, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Indent Type for generated code:';
  end;

  chkAlwaystoFront := TfpgCheckBox.Create(self);
  with chkAlwaystoFront do
  begin
    Name := 'chkAlwaystoFront';
    SetPosition(32, 91, 216, 19);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 19;
    Text := 'Designer always to front';
    onChange := @setAlwaysToFront;
  end;

  pathini := TfpgLabel.Create(self);
  with pathini do
  begin
    Name := 'pathini';
    SetPosition(14, 290, 524, 20);
    Alignment := taCenter;
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Label';
    Text := 'Location of ini file : ' + GetAppConfigDir(False) +
      applicationname + '.ini';
  end;

  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(244, 4, 104, 15);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'IDE Integration';
  end;

  rbTyphon := TfpgRadioButton.Create(self);
  with rbTyphon do
  begin
    Name := 'rbTyphon';
    SetPosition(252, 64, 120, 19);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 21;
    Text := 'with Typhon';
    OnClick := @IdeIntegration;
  end;

  rbLaz := TfpgRadioButton.Create(self);
  with rbLaz do
  begin
    Name := 'rbLaz';
    SetPosition(252, 44, 104, 19);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 22;
    Text := 'with Lazarus';
    OnClick := @IdeIntegration;
  end;

  rbNone := TfpgRadioButton.Create(self);
  with rbNone do
  begin
    Name := 'rbNone';
    SetPosition(252, 24, 88, 19);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 23;
    Text := 'None';
    OnClick := @IdeIntegration;
  end;

  WidgetOrderForm := TfpgLabel.Create(self);
  with WidgetOrderForm do
  begin
    Name := 'WidgetOrderForm';
    SetPosition(392, 4, 80, 15);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Code Editor';
  end;

  rbedit0 := TfpgRadioButton.Create(self);
  with rbedit0 do
  begin
    Name := 'rbedit0';
    SetPosition(396, 24, 104, 19);
    Checked := True;
    FontDesc := '#Label1';
    GroupIndex := 1;
    Hint := '';
    TabOrder := 24;
    Text := 'None';
  end;

  rbedit2 := TfpgRadioButton.Create(self);
  with rbedit2 do
  begin
    Name := 'rbedit2';
    SetPosition(396, 44, 108, 19);
    FontDesc := '#Label1';
    GroupIndex := 1;
    Hint := '';
    TabOrder := 26;
    Text := 'Gedit';
  end;

  rbedit3 := TfpgRadioButton.Create(self);
  with rbedit3 do
  begin
    Name := 'rbedit3';
    SetPosition(396, 64, 100, 19);
    FontDesc := '#Label1';
    GroupIndex := 1;
    Hint := '';
    TabOrder := 27;
    Text := 'Geany';
  end;

  FilenameEdit1 := TfpgFileNameEdit.Create(self);
  with FilenameEdit1 do
  begin
    Name := 'FilenameEdit1';
    SetPosition(420, 84, 116, 24);
    ExtraHint := '';
    FileName := '';
    Filter := '';
    InitialDir := '';
    TabOrder := 28;
  end;

  rbedit4 := TfpgRadioButton.Create(self);
  with rbedit4 do
  begin
    Name := 'rbedit4';
    SetPosition(396, 84, 16, 19);
    FontDesc := '#Label1';
    GroupIndex := 1;
    Hint := '';
    TabOrder := 29;
    Text := ' ';
  end;

  Label3 := TfpgLabel.Create(self);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(288, 120, 116, 19);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Undo Feature';
  end;

  CheckBox1 := TfpgCheckBox.Create(self);
  with CheckBox1 do
  begin
    Name := 'CheckBox1';
    SetPosition(300, 140, 120, 19);
    Checked := True;
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 30;
    Text := 'Enable Undo';
    OnClick := @UndoLook;
  end;

  Label4 := TfpgLabel.Create(self);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(304, 164, 104, 19);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Maximum Undo:';
  end;

  TrackBarUndo := TfpgTrackBar.Create(self);
  with TrackBarUndo do
  begin
    Name := 'TrackBarUndo';
    SetPosition(408, 156, 100, 30);
    Hint := '';
    Min := 10;
    Position := 20;
    Position := 20;
    ShowPosition := True;
    TabOrder := 32;
  end;

  {@VFD_BODY_END: frmVFDSetup}

    {$IFDEF windows}
  rbedit2.Text := 'NotePad';
  rbedit3.Text := 'WordPad';
      {$ENDIF}


  LoadSettings;

   {$if defined(cpu64)}
 {$IFDEF Windows}

  dataf := copy(GetAppConfigDir(False), 1, pos('Local\uidesigner',
    GetAppConfigDir(False)) - 1) + 'Roaming\typhon64\environmentoptions.xml';

     {$ENDIF}
  {$IFDEF Linux}
  dataf := GetUserDir + '.typhon64/environmentoptions.xml';
{$ENDIF}

{$else}
{$IFDEF Windows}
  dataf := copy(GetAppConfigDir(False), 1, pos('Local\uidesigner',
    GetAppConfigDir(False)) - 1) + 'Roaming\typhon32\environmentoptions.xml';
    {$ENDIF}
  {$IFDEF Linux}
  dataf := GetUserDir + '.typhon32/environmentoptions.xml';
{$ENDIF}
{$endif}

  if fileexists(PChar(dataf)) then
    rbtyphon.Enabled := True
  else
    rbtyphon.Enabled := False;


{$IFDEF Windows}
  dataf := copy(GetAppConfigDir(False), 1, pos('uidesigner', GetAppConfigDir(False)) -
    1) + 'lazarus\environmentoptions.xml';
   {$ENDIF}
 {$IFDEF Linux}
  dataf := GetUserDir + '.lazarus/environmentoptions.xml';
{$ENDIF}

  if fileexists(PChar(dataf)) then
    rblaz.Enabled := True
  else
    rblaz.Enabled := False;

{
  if gINI.ReadBool('frmVFDSetupState', 'FirstLoad', True) = False then
    gINI.ReadFormState(self)
  else
    gINI.WriteBool('frmVFDSetupState', 'FirstLoad', False);
  }
  tag := 1;
end;

{
procedure TfrmVFDSetup.BeforeDestruction;
begin
  // We don't put this in SaveSettings because it needs to be called even if
  // user cancels the dialog with btnCancel or ESC key press.
  gINI.WriteFormState(self);
  inherited BeforeDestruction;
end;
}

end.
