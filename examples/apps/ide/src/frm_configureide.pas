unit frm_configureide;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_button, fpg_editbtn,
  fpg_label, fpg_tab, fpg_edit, fpg_grid, fpg_listbox, idemacros, fpg_combobox,
  fpg_checkbox;

type
  TConfigureIDEForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: ConfigureIDEForm}
    btnCancel: TfpgButton;
    btnOK: TfpgButton;
    pcSettings: TfpgPageControl;
    tsEnvironment: TfpgTabSheet;
    tsEditor: TfpgTabSheet;
    tsShortcuts: TfpgTabSheet;
    Label1: TfpgLabel;
    edtFPCSrcDir: TfpgDirectoryEdit;
    edtFPGuiDir: TfpgDirectoryEdit;
    edtFPGuiLibDir: TfpgDirectoryEdit;
    edtSyntaxDefDir: TfpgDirectoryEdit;
    edtTempateDir: TfpgDirectoryEdit;
    edtCompiler: TfpgFileNameEdit;
    edtDebugger: TfpgFileNameEdit;
    Label2: TfpgLabel;
    Label3: TfpgLabel;
    Label4: TfpgLabel;
    Label5: TfpgLabel;
    Label6: TfpgLabel;
    Label7: TfpgLabel;
    Label8: TfpgLabel;
    Label9: TfpgLabel;
    Label11: TfpgLabel;
    edtEditorFont: TfpgFontEdit;
    edtExeExt: TfpgEdit;
    edtTarget: TfpgEdit;
    grdShortcuts: TfpgStringGrid;
    tsSyntaxDefs: TfpgTabSheet;
    tsFileFilters: TfpgTabSheet;
    grdSyntaxDefs: TfpgStringGrid;
    grdFileFilters: TfpgStringGrid;
    tsExtTools: TfpgTabSheet;
    Label10: TfpgLabel;
    edtExtToolMenu: TfpgEdit;
    Label12: TfpgLabel;
    edtExtToolFile: TfpgFileNameEdit;
    Label13: TfpgLabel;
    edtExtToolParams: TfpgEdit;
    btnExtToolAdd: TfpgButton;
    btnExtToolDel: TfpgButton;
    lbExtTools: TfpgListBox;
    cbTabPosition: TfpgComboBox;
    Label14: TfpgLabel;
    cbSyntaxHighlighting: TfpgCheckBox;
    {@VFD_HEAD_END: ConfigureIDEForm}
    // so we can get correct hints, but still undo with the Cancel button
    FInternalMacroList: TIDEMacroList;
    procedure BeforeShowHint(Sender: TObject; var AHint: TfpgString);
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SaveToMacroList(AList: TIDEMacroList);
    procedure FormKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

procedure DisplayConfigureIDE;


implementation

uses
  fpg_dialogs
  ,fpg_iniutils
  ,fpg_widget
  ,ideconst
  ;

type
  // Used to get access to the Protected properties
  TDirectoryEditFriend = class(TfpgDirectoryEdit);


procedure DisplayConfigureIDE;
var
  frm: TConfigureIDEForm;
  Result: Boolean;
begin
  frm := TConfigureIDEForm.Create(nil);
  try
    frm.LoadSettings;
    Result := frm.ShowModal = mrOK;
    if Result then
    begin
      frm.SaveSettings;
    end;
  finally
    frm.Free;
  end;
end;

{@VFD_NEWFORM_IMPL}

procedure TConfigureIDEForm.BeforeShowHint(Sender: TObject; var AHint: TfpgString);
var
  s: TfpgString;
  c: TfpgWidget;
begin
  if Sender is TfpgWidget then
    c := TfpgWidget(Sender)
  else
    Exit;    // should never occur, but lets just be safe

  if (c.Name = 'FEdit') and ((c.Parent is TfpgDirectoryEdit) or (c.Parent is TfpgFileNameEdit)) then
  begin
    if c.Parent <> nil then
      c := c.Parent
    else
      Exit; // lets just be safe again
  end;

  // controls that may contain macros
  if c is TfpgDirectoryEdit then
    s := TfpgDirectoryEdit(c).Directory
  else if c is TfpgFileNameEdit then
    s := TfpgFileNameEdit(c).FileName
  else if c is TfpgEdit then
    s := TfpgEdit(c).Text;

  AHint := s;

  if FInternalMacroList.StrHasMacros(s) then
  begin
    SaveToMacroList(FInternalMacroList);
    AHint := FInternalMacroList.ExpandMacro(s);
  end;
end;

procedure TConfigureIDEForm.LoadSettings;
begin
  edtFPCSrcDir.Directory := gINI.ReadString(cEnvironment, 'FPCSrcDir', '');
  edtFPGuiDir.Directory := gINI.ReadString(cEnvironment, 'FPGuiDir', '');
  edtFPGuiLibDir.Directory := gINI.ReadString(cEnvironment, 'FPGuiLibDir', GMacroList.FindByName(cMacro_FPGuiLibDir).Value);
  edtSyntaxDefDir.Directory := gINI.ReadString(cEnvironment, 'SyntaxDefDir', GMacroList.FindByName(cMacro_SyntaxDefDir).Value);
  edtTempateDir.Directory := gINI.ReadString(cEnvironment, 'TemplateDir', GMacroList.FindByName(cMacro_TemplateDir).Value);
  edtCompiler.Filename := gINI.ReadString(cEnvironment, 'Compiler', '');
  edtDebugger.Filename := gINI.ReadString(cEnvironment, 'Debugger', 'gdb');
  edtExeExt.Text := gINI.ReadString(cEnvironment, 'ExeExt', '');
  edtTarget.Text := gINI.ReadString(cEnvironment, 'Target', GMacroList.FindByName(cMacro_Target).Value);
  edtEditorFont.FontDesc := gINI.ReadString(cEditor, 'Font', '#Edit2');
  cbTabPosition.FocusItem := gINI.ReadInteger(cEditor, 'TabPosition', 0);
  cbSyntaxHighlighting.Checked := gINI.ReadBool(cEditor, 'SyntaxHighlighting', True);
end;

procedure TConfigureIDEForm.SaveSettings;
begin
  gINI.WriteString(cEnvironment, 'FPCSrcDir', edtFPCSrcDir.Directory);
  gINI.WriteString(cEnvironment, 'FPGuiDir', edtFPGuiDir.Directory);
  gINI.WriteString(cEnvironment, 'FPGuiLibDir', edtFPGuiLibDir.Directory);
  gINI.WriteString(cEnvironment, 'SyntaxDefDir', edtSyntaxDefDir.Directory);
  gINI.WriteString(cEnvironment, 'TemplateDir', edtTempateDir.Directory);
  gINI.WriteString(cEnvironment, 'Compiler', edtCompiler.Filename);
  gINI.WriteString(cEnvironment, 'Debugger', edtDebugger.Filename);
  gINI.WriteString(cEnvironment, 'ExeExt', edtExeExt.Text);
  gINI.WriteString(cEnvironment, 'Target', edtTarget.Text);
  gINI.WriteString(cEditor, 'Font', edtEditorFont.FontDesc);
  gINI.WriteInteger(cEditor, 'TabPosition', cbTabPosition.FocusItem);
  gINI.WriteBool(cEditor, 'SyntaxHighlighting', cbSyntaxHighlighting.Checked);

  SaveToMacroList(GMacroList);
end;

procedure TConfigureIDEForm.SaveToMacroList(AList: TIDEMacroList);
begin
  AList.SetValue(cMacro_FPCSrcDir, edtFPCSrcDir.Directory);
  AList.SetValue(cMacro_FPGuiDir, edtFPGuiDir.Directory);
  AList.SetValue(cMacro_FPGuiLibDir, edtFPGuiLibDir.Directory);
  AList.SetValue(cMacro_SyntaxDefDir, edtSyntaxDefDir.Directory);
  AList.SetValue(cMacro_TemplateDir, edtTempateDir.Directory);
  AList.SetValue(cMacro_Compiler, edtCompiler.FileName);
  AList.SetValue(cMacro_Debugger, edtDebugger.FileName);
  AList.SetValue(cMacro_ExeExt, edtExeExt.Text);
  AList.SetValue(cMacro_Target, edtTarget.Text);
end;

procedure TConfigureIDEForm.FormKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  if KeyCode = keyEscape then
    Close;
end;

constructor TConfigureIDEForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInternalMacroList := TIDEMacroList.Create;
  OnKeyPress  := @FormKeyPressed;
end;

destructor TConfigureIDEForm.Destroy;
begin
  FInternalMacroList.Free;
  inherited Destroy;
end;

procedure TConfigureIDEForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: ConfigureIDEForm}
  Name := 'ConfigureIDEForm';
  SetPosition(332, 190, 578, 480);
  WindowTitle := 'Configure IDE';
  Hint := '';
  ShowHint := True;
  WindowPosition := wpOneThirdDown;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(494, 450, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrCancel;
    TabOrder := 1;
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    Name := 'btnOK';
    SetPosition(410, 450, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'OK';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrOK;
    TabOrder := 2;
  end;

  pcSettings := TfpgPageControl.Create(self);
  with pcSettings do
  begin
    Name := 'pcSettings';
    SetPosition(4, 4, 570, 430);
    Anchors := [anLeft,anRight,anTop,anBottom];
    ActivePageIndex := 0;
    Hint := '';
    TabOrder := 3;
    TabPosition := tpRight;
  end;

  tsEnvironment := TfpgTabSheet.Create(pcSettings);
  with tsEnvironment do
  begin
    Name := 'tsEnvironment';
    SetPosition(125, 3, 442, 424);
    Text := 'Environment';
  end;

  tsEditor := TfpgTabSheet.Create(pcSettings);
  with tsEditor do
  begin
    Name := 'tsEditor';
    SetPosition(125, 3, 442, 424);
    Text := 'Editor';
  end;

  tsShortcuts := TfpgTabSheet.Create(pcSettings);
  with tsShortcuts do
  begin
    Name := 'tsShortcuts';
    SetPosition(125, 3, 442, 424);
    Text := 'Shortcuts';
  end;

  Label1 := TfpgLabel.Create(tsEnvironment);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(8, 4, 340, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'FPC Source Directory ${FPCSRCDIR}';
  end;

  edtFPCSrcDir := TfpgDirectoryEdit.Create(tsEnvironment);
  with edtFPCSrcDir do
  begin
    Name := 'edtFPCSrcDir';
    SetPosition(8, 22, 424, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    Directory := '';
    RootDirectory := '';
    TabOrder := 3;
    Hint := '*';
    OnShowHint := @BeforeShowHint;
  end;

  edtFPGuiDir := TfpgDirectoryEdit.Create(tsEnvironment);
  with edtFPGuiDir do
  begin
    Name := 'edtFPGuiDir';
    SetPosition(8, 74, 424, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    Directory := '';
    RootDirectory := '';
    TabOrder := 6;
    Hint := '*';
    OnShowHint := @BeforeShowHint;
  end;

  edtFPGuiLibDir := TfpgDirectoryEdit.Create(tsEnvironment);
  with edtFPGuiLibDir do
  begin
    Name := 'edtFPGuiLibDir';
    SetPosition(8, 122, 424, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    Directory := '${FPGUIDIR}lib/';
    RootDirectory := '';
    TabOrder := 7;
    Hint := '*';
    OnShowHint := @BeforeShowHint;
  end;

  edtSyntaxDefDir := TfpgDirectoryEdit.Create(tsEnvironment);
  with edtSyntaxDefDir do
  begin
    Name := 'edtSyntaxDefDir';
    SetPosition(8, 170, 424, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    Directory := '${FPGUIDIR}apps/ide/syntaxdefs/';
    RootDirectory := '';
    TabOrder := 8;
    Hint := '*';
    OnShowHint := @BeforeShowHint;
  end;

  edtTempateDir := TfpgDirectoryEdit.Create(tsEnvironment);
  with edtTempateDir do
  begin
    Name := 'edtTempateDir';
    SetPosition(8, 218, 424, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    Directory := '{FPGUIDIR}apps/ide/templates/';
    RootDirectory := '';
    TabOrder := 9;
    Hint := '*';
    OnShowHint := @BeforeShowHint;
  end;

  edtCompiler := TfpgFileNameEdit.Create(tsEnvironment);
  with edtCompiler do
  begin
    Name := 'edtCompiler';
    SetPosition(8, 266, 424, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FileName := '/opt/fpc_2.4.1/${TARGET}/bin/fpc';
    InitialDir := '';
    Filter := '';
    TabOrder := 10;
    Hint := '*';
    OnShowHint := @BeforeShowHint;
  end;

  edtDebugger := TfpgFileNameEdit.Create(tsEnvironment);
  with edtDebugger do
  begin
    Name := 'edtDebugger';
    SetPosition(8, 314, 426, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FileName := 'gdb';
    InitialDir := '';
    Filter := '';
    TabOrder := 11;
    Hint := '*';
    OnShowHint := @BeforeShowHint;
  end;

  Label2 := TfpgLabel.Create(tsEnvironment);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(8, 56, 340, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'fpGUI Root Directory ${FPGUIDIR}';
  end;

  Label3 := TfpgLabel.Create(tsEnvironment);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(8, 104, 340, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := '${FPGUILIBDIR}';
  end;

  Label4 := TfpgLabel.Create(tsEnvironment);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(8, 152, 344, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := '${SYNTAXDEFDIR}';
  end;

  Label5 := TfpgLabel.Create(tsEnvironment);
  with Label5 do
  begin
    Name := 'Label5';
    SetPosition(8, 200, 340, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := '${TEMPLATEDIR}';
  end;

  Label6 := TfpgLabel.Create(tsEnvironment);
  with Label6 do
  begin
    Name := 'Label6';
    SetPosition(8, 248, 340, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := '${COMPILER}';
  end;

  Label7 := TfpgLabel.Create(tsEnvironment);
  with Label7 do
  begin
    Name := 'Label7';
    SetPosition(8, 296, 340, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := '${DEBUGGER}';
  end;

  Label8 := TfpgLabel.Create(tsEnvironment);
  with Label8 do
  begin
    Name := 'Label8';
    SetPosition(8, 344, 144, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := '${EXEEXT}';
  end;

  Label9 := TfpgLabel.Create(tsEnvironment);
  with Label9 do
  begin
    Name := 'Label9';
    SetPosition(164, 344, 188, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := '${TARGET}';
  end;

  Label11 := TfpgLabel.Create(tsEditor);
  with Label11 do
  begin
    Name := 'Label11';
    SetPosition(8, 4, 224, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Font';
  end;

  edtEditorFont := TfpgFontEdit.Create(tsEditor);
  with edtEditorFont do
  begin
    Name := 'edtEditorFont';
    SetPosition(8, 22, 424, 24);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Edit1';
    TabOrder := 2;
  end;

  edtExeExt := TfpgEdit.Create(tsEnvironment);
  with edtExeExt do
  begin
    Name := 'edtExeExt';
    SetPosition(8, 362, 144, 24);
    ExtraHint := '';
    Hint := '*';
    TabOrder := 21;
    Text := '';
    FontDesc := '#Edit1';
    OnShowHint := @BeforeShowHint;
  end;

  edtTarget := TfpgEdit.Create(tsEnvironment);
  with edtTarget do
  begin
    Name := 'edtTarget';
    SetPosition(164, 362, 192, 24);
    ExtraHint := '';
    Hint := '*';
    TabOrder := 22;
    Text := 'i386-linux';
    FontDesc := '#Edit1';
    OnShowHint := @BeforeShowHint;
  end;

  grdShortcuts := TfpgStringGrid.Create(tsShortcuts);
  with grdShortcuts do
  begin
    Name := 'grdShortcuts';
    SetPosition(8, 8, 428, 408);
    Anchors := [anLeft,anRight,anTop,anBottom];
    AddColumn('Action', 180, taLeftJustify);
    AddColumn('Shortcut', 110, taLeftJustify);
    AddColumn('Alternative', 110, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 10;
    RowSelect := False;
    TabOrder := 1;
  end;

  tsSyntaxDefs := TfpgTabSheet.Create(pcSettings);
  with tsSyntaxDefs do
  begin
    Name := 'tsSyntaxDefs';
    SetPosition(125, 3, 442, 424);
    Text := 'Syntax Highlighting';
  end;

  tsFileFilters := TfpgTabSheet.Create(pcSettings);
  with tsFileFilters do
  begin
    Name := 'tsFileFilters';
    SetPosition(125, 3, 442, 424);
    Text := 'File Filters';
  end;

  grdSyntaxDefs := TfpgStringGrid.Create(tsSyntaxDefs);
  with grdSyntaxDefs do
  begin
    Name := 'grdSyntaxDefs';
    SetPosition(8, 8, 428, 408);
    Anchors := [anLeft,anRight,anTop,anBottom];
    AddColumn('Syntax Definition File', 200, taLeftJustify);
    AddColumn('File Mask', 200, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 5;
    RowSelect := False;
    TabOrder := 1;
  end;

  grdFileFilters := TfpgStringGrid.Create(tsFileFilters);
  with grdFileFilters do
  begin
    Name := 'grdFileFilters';
    SetPosition(8, 8, 428, 408);
    Anchors := [anLeft,anRight,anTop,anBottom];
    AddColumn('Name', 150, taLeftJustify);
    AddColumn('File Mask', 200, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 5;
    RowSelect := False;
    TabOrder := 1;
  end;

  tsExtTools := TfpgTabSheet.Create(pcSettings);
  with tsExtTools do
  begin
    Name := 'tsExtTools';
    SetPosition(125, 3, 442, 424);
    Text := 'External Tools';
  end;

  Label10 := TfpgLabel.Create(tsExtTools);
  with Label10 do
  begin
    Name := 'Label10';
    SetPosition(8, 234, 212, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Menu Caption';
  end;

  edtExtToolMenu := TfpgEdit.Create(tsExtTools);
  with edtExtToolMenu do
  begin
    Name := 'edtExtToolMenu';
    SetPosition(8, 252, 428, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    Hint := '';
    TabOrder := 3;
    Text := '';
    FontDesc := '#Edit1';
  end;

  Label12 := TfpgLabel.Create(tsExtTools);
  with Label12 do
  begin
    Name := 'Label12';
    SetPosition(8, 282, 428, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Program';
  end;

  edtExtToolFile := TfpgFileNameEdit.Create(tsExtTools);
  with edtExtToolFile do
  begin
    Name := 'edtExtToolFile';
    SetPosition(8, 300, 428, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FileName := '';
    InitialDir := '';
    Filter := '';
    TabOrder := 5;
  end;

  Label13 := TfpgLabel.Create(tsExtTools);
  with Label13 do
  begin
    Name := 'Label13';
    SetPosition(8, 330, 420, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Parameters';
  end;

  edtExtToolParams := TfpgEdit.Create(tsExtTools);
  with edtExtToolParams do
  begin
    Name := 'edtExtToolParams';
    SetPosition(8, 348, 428, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    Hint := '';
    TabOrder := 7;
    Text := '';
    FontDesc := '#Edit1';
  end;

  btnExtToolAdd := TfpgButton.Create(tsExtTools);
  with btnExtToolAdd do
  begin
    Name := 'btnExtToolAdd';
    SetPosition(8, 8, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := -1;
    ImageName := 'stdimg.add';
    ImageSpacing := 0;
    TabOrder := 8;
  end;

  btnExtToolDel := TfpgButton.Create(tsExtTools);
  with btnExtToolDel do
  begin
    Name := 'btnExtToolDel';
    SetPosition(34, 8, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := -1;
    ImageName := 'stdimg.remove';
    ImageSpacing := 0;
    TabOrder := 9;
  end;

  lbExtTools := TfpgListBox.Create(tsExtTools);
  with lbExtTools do
  begin
    Name := 'lbExtTools';
    SetPosition(8, 33, 428, 192);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#List';
    Hint := '';
    HotTrack := False;
    PopupFrame := False;
    TabOrder := 10;
  end;

  cbTabPosition := TfpgComboBox.Create(tsEditor);
  with cbTabPosition do
  begin
    Name := 'cbTabPosition';
    SetPosition(8, 68, 144, 22);
    FontDesc := '#List';
    Hint := '';
    Items.Add('top');
    Items.Add('bottom');
    Items.Add('left');
    Items.Add('right');
    TabOrder := 3;
  end;

  Label14 := TfpgLabel.Create(tsEditor);
  with Label14 do
  begin
    Name := 'Label14';
    SetPosition(8, 50, 404, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Tab position';
  end;

  cbSyntaxHighlighting := TfpgCheckBox.Create(tsEditor);
  with cbSyntaxHighlighting do
  begin
    Name := 'cbSyntaxHighlighting';
    SetPosition(4, 100, 404, 20);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 5;
    Text := 'Syntax Highlighting';
  end;

  {@VFD_BODY_END: ConfigureIDEForm}
  {%endregion}
end;


end.
