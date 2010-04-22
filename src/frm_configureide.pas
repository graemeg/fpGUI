unit frm_configureide;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_button, fpg_editbtn,
  fpg_label, fpg_tab, fpg_edit, fpg_grid, fpg_listbox;

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
    edtFPCDir: TfpgDirectoryEdit;
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
    {@VFD_HEAD_END: ConfigureIDEForm}
    procedure LoadSettings;
    procedure SaveSettings;
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

procedure DisplayConfigureIDE;


implementation

uses
  fpg_dialogs
  ,fpg_iniutils
  ,ideconst
  ;

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

procedure TConfigureIDEForm.LoadSettings;
begin
  edtFPCDir.Directory := gINI.ReadString(cEnvironment, 'fpcDir', '');
  edtFPGuiDir.Directory := gINI.ReadString(cEnvironment, 'fpGuiDir', '');
  edtFPGuiLibDir.Directory := gINI.ReadString(cEnvironment, 'fpGuiLibDir', '${FPGUIDIR}lib/');
  edtSyntaxDefDir.Directory := gINI.ReadString(cEnvironment, 'SyntaxDefDir', '${FPGUIDIR}apps/ide/syntaxdefs/');
  edtTempateDir.Directory := gINI.ReadString(cEnvironment, 'TemplateDir', '${FPGUIDIR}apps/ide/templates/');
  edtCompiler.Filename := gINI.ReadString(cEnvironment, 'Compiler', '');
  edtDebugger.Filename := gINI.ReadString(cEnvironment, 'Debugger', 'gdb');
  edtExeExt.Text := gINI.ReadString(cEnvironment, 'ExeExt', '');
  edtTarget.Text := gINI.ReadString(cEnvironment, 'Target', 'i386-linux');
  edtEditorFont.FontDesc := gINI.ReadString(cEditor, 'Font', '#Edit2');
end;

procedure TConfigureIDEForm.SaveSettings;
begin
  gINI.WriteString(cEnvironment, 'fpcDir', edtFPCDir.Directory);
  gINI.WriteString(cEnvironment, 'fpGuiDir', edtFPGuiDir.Directory);
  gINI.WriteString(cEnvironment, 'fpGuiLibDir', edtFPGuiLibDir.Directory);
  gINI.WriteString(cEnvironment, 'SyntaxDefDir', edtSyntaxDefDir.Directory);
  gINI.WriteString(cEnvironment, 'TemplateDir', edtTempateDir.Directory);
  gINI.WriteString(cEnvironment, 'Compiler', edtCompiler.Filename);
  gINI.WriteString(cEnvironment, 'Debugger', edtDebugger.Filename);
  gINI.WriteString(cEnvironment, 'ExeExt', edtExeExt.Text);
  gINI.WriteString(cEnvironment, 'Target', edtTarget.Text);
  gINI.WriteString(cEditor, 'Font', edtEditorFont.FontDesc);


end;

procedure TConfigureIDEForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: ConfigureIDEForm}
  Name := 'ConfigureIDEForm';
  SetPosition(332, 190, 578, 480);
  WindowTitle := 'Configure IDE';
  Hint := '';
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
    TabPosition := tpLeft;
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
    Text := '${FPCDIR}';
  end;

  edtFPCDir := TfpgDirectoryEdit.Create(tsEnvironment);
  with edtFPCDir do
  begin
    Name := 'edtFPCDir';
    SetPosition(8, 22, 424, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    Directory := '';
    RootDirectory := '';
    TabOrder := 3;
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
  end;

  Label2 := TfpgLabel.Create(tsEnvironment);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(8, 56, 340, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := '${FPGUIDIR}';
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
    SetPosition(8, 4, 80, 16);
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
    Hint := '';
    TabOrder := 21;
    Text := '';
    FontDesc := '#Edit1';
  end;

  edtTarget := TfpgEdit.Create(tsEnvironment);
  with edtTarget do
  begin
    Name := 'edtTarget';
    SetPosition(164, 362, 192, 24);
    ExtraHint := '';
    Hint := '';
    TabOrder := 22;
    Text := 'i386-linux';
    FontDesc := '#Edit1';
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

  {@VFD_BODY_END: ConfigureIDEForm}
  {%endregion}
end;


end.
