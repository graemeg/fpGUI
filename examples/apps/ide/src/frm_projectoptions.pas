{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2013 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      ---
}

unit frm_projectoptions;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_button, fpg_label,
  fpg_tab, fpg_editbtn, fpg_checkbox, fpg_grid, fpg_basegrid,
  fpg_combobox, fpg_edit, idemacros;

type

  TProjectOptionsForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: ProjectOptionsForm}
    btnCancel: TfpgButton;
    btnOK: TfpgButton;
    pcOptions: TfpgPageControl;
    tsCompiler: TfpgTabSheet;
    tsDebugger: TfpgTabSheet;
    tsMacros: TfpgTabSheet;
    tsOther: TfpgTabSheet;
    Label1: TfpgLabel;
    edtMainFile: TfpgFileNameEdit;
    Label2: TfpgLabel;
    edtTargetFile: TfpgFileNameEdit;
    edtMakeCommand: TfpgFileNameEdit;
    Label3: TfpgLabel;
    edtMakeDir: TfpgDirectoryEdit;
    Label4: TfpgLabel;
    Label5: TfpgLabel;
    FilenameEdit4: TfpgFileNameEdit;
    CheckBox1: TfpgCheckBox;
    Label6: TfpgLabel;
    cbDefaultMakeCol: TfpgComboBox;
    pcCompiler: TfpgPageControl;
    TabSheet1: TfpgTabSheet;
    TabSheet2: TfpgTabSheet;
    grdCompilerMakeOptions: TfpgStringGrid;
    grdCompilerDirs: TfpgStringGrid;
    Label11: TfpgLabel;
    Label7: TfpgLabel;
    FilenameEdit5: TfpgFileNameEdit;
    Label8: TfpgLabel;
    Edit1: TfpgEdit;
    CheckBox2: TfpgCheckBox;
    CheckBox3: TfpgCheckBox;
    CheckBox4: TfpgCheckBox;
    PageControl1: TfpgPageControl;
    TabSheet3: TfpgTabSheet;
    TabSheet4: TfpgTabSheet;
    TabSheet5: TfpgTabSheet;
    TabSheet6: TfpgTabSheet;
    TabSheet7: TfpgTabSheet;
    grdDebugSrcDirs: TfpgStringGrid;
    btnShowCmdLine: TfpgButton;
    edtUnitOutputDir: TfpgDirectoryEdit;
    Label9: TfpgLabel;
    Label10: TfpgLabel;
    grdMacroGroup: TfpgStringGrid;
    Label12: TfpgLabel;
    grdUserMacros: TfpgStringGrid;
    {@VFD_HEAD_END: ProjectOptionsForm}
    FCellEdit: TfpgEdit;
    FFocusRect: TfpgRect;
    FLastGrid: TfpgStringGrid; // reference only
    // so we can get correct hints, but still undo with the Cancel button
    FInternalMacroList: TIDEMacroList;
    procedure btnShowCmdLineClicked(Sender: TObject);
    procedure CellEditExit(Sender: TObject);
    procedure CellEditKeypressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure grdCompilerDirsDrawCell(Sender: TObject; const ARow, ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
    procedure grdCompilerDirsKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure grdCompilerMakeOptionsKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure grdCompilerMakeOptionsDrawCell(Sender: TObject; const ARow, ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
    procedure grdCompilerMakeOptionsClicked(Sender: TObject);
    procedure grdCompilerDirsClicked(Sender: TObject);
    procedure grdUserMacrosClicked(Sender: TObject);
    procedure grdUserMacrosDrawCell(Sender: TObject; const ARow, ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
    procedure grdMacroGroupCanSelectCell(Sender: TObject; const ARow, ACol: Integer; var ACanSelect: boolean);
    procedure BeforeShowHint(Sender: TObject; var AHint: TfpgString);
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SetupCellEdit(AGrid: TfpgStringGrid);
    procedure CleanupCompilerMakeOptionsGrid;
    procedure CleanupCompilerDirs;
    procedure CleanupUserMacrosGrid;
    procedure SaveToMacroList(AList: TIDEMacroList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

procedure DisplayProjectOptions;


implementation

uses
  fpg_iniutils
  ,fpg_dialogs
  ,fpg_widget
  ,Project
  ,ideconst
  ,ideutils
  ;

type
  // Used to get access to the Protected properties
  TDirectoryEditFriend = class(TfpgDirectoryEdit);


procedure DisplayProjectOptions;
var
  frm: TProjectOptionsForm;
  Result: Boolean;
begin
  frm := TProjectOptionsForm.Create(nil);
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

procedure TProjectOptionsForm.btnShowCmdLineClicked(Sender: TObject);
var
  c: TfpgString;
  b: integer;
begin
  // build compilation string
  c := gINI.ReadString(cEnvironment, 'Compiler', '') + LineEnding;
  b := cbDefaultMakeCol.FocusItem;

  c := c + GProject.GenerateCmdLine(True, b);
  try
    c := GMacroList.ExpandMacro(c);
  except
    on E: Exception do
    begin
      TfpgMessageDialog.Critical('', E.Message);
      Exit;
    end;
  end;
  ShowString(c, 'Compile command');
end;

procedure TProjectOptionsForm.CellEditExit(Sender: TObject);
begin
  FCellEdit.Visible := False;
end;

procedure TProjectOptionsForm.CellEditKeypressed(Sender: TObject;
  var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  if KeyCode = keyReturn then
  begin
    FLastGrid.Cells[FLastGrid.FocusCol, FLastGrid.FocusRow] := FCellEdit.Text;
    FCellEdit.Visible := False;
    FLastGrid.SetFocus;
  end;
end;

procedure TProjectOptionsForm.grdCompilerDirsDrawCell(Sender: TObject;
  const ARow, ACol: Integer; const ARect: TfpgRect;
  const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
var
  img: TfpgImage;
begin
  if ACol = 5 then
  begin
    grdCompilerDirs.Canvas.Color := clMedGray;
    grdCompilerDirs.Canvas.DrawLine(ARect.Right-1, ARect.Top, ARect.Right-1, ARect.Bottom);
  end
  else if ACol = 6 then
  begin
    grdCompilerDirs.Canvas.Color := clMedGray;
    grdCompilerDirs.Canvas.DrawLine(ARect.Left, ARect.Top, ARect.Left, ARect.Bottom);
  end;

  if (gdSelected in AFlags) and (ACol = 10) then
  begin
    FFocusRect := ARect;
  end;

  if ACol < 10 then
  begin
    if grdCompilerDirs.Cells[ACol, ARow] = cCheck then
    begin
      img := fpgImages.GetImage('stdimg.check');
      if (gdSelected in AFlags) and (gdFocused in AFlags) then
        img.Invert;
      grdCompilerDirs.Canvas.DrawImage(ARect.Left, ARect.Top, img);
      if (gdSelected in AFlags) and (gdFocused in AFlags) then
        img.Invert;  // restore image to original state
      ADefaultDrawing := False;
    end;
  end
  else
    grdCompilerDirs.Canvas.Setfont(grdCompilerDirs.Font);
end;

procedure TProjectOptionsForm.grdCompilerDirsKeyPressed(Sender: TObject;
  var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  CheckGridModifyKeyPresses(Sender, KeyCode, ShiftState, Consumed);
  if Consumed then
    Exit;

  if TfpgStringGrid(Sender).FocusCol < 10 then
  begin
    if (KeyCode = keySpace) then
    begin
      grdCompilerDirsClicked(Sender);
      Consumed := True;
    end;
  end
  else if TfpgStringGrid(Sender).FocusCol = 10 then
  begin
    if (KeyCode = keyF2) or (KeyCode = keyReturn) then
    begin
      // we need to edit the cell contents
      SetupCellEdit(TfpgStringGrid(Sender));
      Consumed := True;
    end;
  end;
end;

procedure TProjectOptionsForm.grdCompilerMakeOptionsKeyPressed(Sender: TObject;
  var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  CheckGridModifyKeyPresses(Sender, KeyCode, ShiftState, Consumed);
  if Consumed then
    Exit;

  if TfpgStringGrid(Sender).FocusCol < 6 then
  begin
    if (KeyCode = keySpace) then
    begin
      grdCompilerMakeOptionsClicked(Sender);
      Consumed := True;
    end;
  end
  else if TfpgStringGrid(Sender).FocusCol = 6 then
  begin
    if (KeyCode = keyF2) or (KeyCode = keyReturn) then
    begin
      // we need to edit the cell contents
      SetupCellEdit(TfpgStringGrid(Sender));
      Consumed := True;
    end;
  end;
end;

procedure TProjectOptionsForm.grdCompilerMakeOptionsDrawCell(Sender: TObject;
  const ARow, ACol: Integer; const ARect: TfpgRect;
  const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
var
  img: TfpgImage;
begin
  if (gdSelected in AFlags) and (ACol = 6) then
  begin
    FFocusRect := ARect;
  end;

  if ACol < 6 then
  begin
    if grdCompilerMakeOptions.Cells[ACol, ARow] = cCheck then
    begin
      img := fpgImages.GetImage('stdimg.check');
      if (gdSelected in AFlags) and (gdFocused in AFlags) then
        img.Invert;
      grdCompilerMakeOptions.Canvas.DrawImage(ARect.Left, ARect.Top, img);
      if (gdSelected in AFlags) and (gdFocused in AFlags) then
        img.Invert;  // restore image to original state
      ADefaultDrawing := False;
    end;
  end
  else
    grdCompilerMakeOptions.Canvas.Setfont(grdCompilerMakeOptions.Font);
end;

procedure TProjectOptionsForm.grdCompilerMakeOptionsClicked(Sender: TObject);
var
  r, c: integer;
begin
  if TfpgStringGrid(Sender).RowCount = 0 then
    TfpgStringGrid(Sender).RowCount := 1;
  r := TfpgStringGrid(Sender).FocusRow;
  c := TfpgStringGrid(Sender).FocusCol;
  if c < 6 then   // checkbox area
  begin
    if TfpgStringGrid(Sender).Cells[c, r] = '' then
      TfpgStringGrid(Sender).Cells[c, r] := cCheck
    else
      TfpgStringGrid(Sender).Cells[c, r] := '';
  end;
end;

procedure TProjectOptionsForm.grdCompilerDirsClicked(Sender: TObject);
var
  r, c: integer;
begin
  if TfpgStringGrid(Sender).RowCount = 0 then
    TfpgStringGrid(Sender).RowCount := 1;
  r := TfpgStringGrid(Sender).FocusRow;
  c := TfpgStringGrid(Sender).FocusCol;
  if c < 10 then   // checkbox area
  begin
    if TfpgStringGrid(Sender).Cells[c, r] = '' then
      TfpgStringGrid(Sender).Cells[c, r] := cCheck
    else
      TfpgStringGrid(Sender).Cells[c, r] := '';
  end;
end;

procedure TProjectOptionsForm.grdUserMacrosClicked(Sender: TObject);
var
  r, c: integer;
begin
  if TfpgStringGrid(Sender).RowCount = 0 then
    TfpgStringGrid(Sender).RowCount := 1;
  r := TfpgStringGrid(Sender).FocusRow;
  c := TfpgStringGrid(Sender).FocusCol;
  if c < 6 then   // checkbox area
  begin
    if TfpgStringGrid(Sender).Cells[c, r] = '' then
      TfpgStringGrid(Sender).Cells[c, r] := cCheck
    else
      TfpgStringGrid(Sender).Cells[c, r] := '';
  end;
end;

procedure TProjectOptionsForm.grdUserMacrosDrawCell(Sender: TObject;
  const ARow, ACol: Integer; const ARect: TfpgRect;
  const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
var
  img: TfpgImage;
begin
  if (gdSelected in AFlags) and (ACol = 6) then
  begin
    FFocusRect := ARect;
  end;

  if ACol < 6 then
  begin
    if grdUserMacros.Cells[ACol, ARow] = cCheck then
    begin
      img := fpgImages.GetImage('stdimg.check');
      if (gdSelected in AFlags) and (gdFocused in AFlags) then
        img.Invert;
      grdUserMacros.Canvas.DrawImage(ARect.Left, ARect.Top, img);
      if (gdSelected in AFlags) and (gdFocused in AFlags) then
        img.Invert;  // restore image to original state
      ADefaultDrawing := False;
    end;
  end
  else
    grdUserMacros.Canvas.Setfont(grdUserMacros.Font);
end;

procedure TProjectOptionsForm.grdMacroGroupCanSelectCell(Sender: TObject;
  const ARow, ACol: Integer; var ACanSelect: boolean);
begin
  ACanSelect := ACol > 0;
end;

procedure TProjectOptionsForm.BeforeShowHint(Sender: TObject; var AHint: TfpgString);
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
    s := TfpgEdit(c).Text
  else if c is TfpgStringGrid then
  begin
    s := TfpgStringGrid(c).Cells[TfpgStringGrid(c).FocusCol, TfpgStringGrid(c).FocusRow];
    if s = cCheck then
      s := '';
  end;

  AHint := s;

  if FInternalMacroList.StrHasMacros(s) then
  begin
    SaveToMacroList(FInternalMacroList);
    AHint := FInternalMacroList.ExpandMacro(s);
  end;
end;

procedure TProjectOptionsForm.LoadSettings;
var
  i, j: integer;
begin
  edtMainFile.FileName            := GProject.MainUnit;
  edtTargetFile.FileName          := GProject.TargetFile;
  edtMakeDir.Directory            := GProject.ProjectDir;
  edtUnitOutputDir.Directory      := GProject.UnitOutputDir;
  cbDefaultMakeCol.FocusItem      := GProject.DefaultMake;
  grdCompilerMakeOptions.RowCount := GProject.MakeOptions.Count;

  for i := 0 to GProject.MakeOptions.Count-1 do
  begin
    grdCompilerMakeOptions.Cells[6, i] := GProject.MakeOptions[i];
    for j := 0 to 5 do // we know there is only 6 boolean columns
    begin
      if GProject.MakeOptionsGrid[j, i] then
        grdCompilerMakeOptions.Cells[j, i] := cCheck;
    end;
  end;

  grdCompilerDirs.RowCount := GProject.UnitDirs.Count;
  for i := 0 to GProject.UnitDirs.Count-1 do
  begin
    grdCompilerDirs.Cells[10, i] := GProject.UnitDirs[i];
    for j := 0 to 9 do // we know there is only 10 boolean columns
    begin
      if GProject.UnitDirsGrid[j, i] then
        grdCompilerDirs.Cells[j, i] := cCheck;
    end;
  end;

  grdUserMacros.RowCount := GProject.MacroNames.Count;
  for i := 0 to GProject.MacroNames.Count-1 do
  begin
    grdUserMacros.Cells[6, i] := GProject.MacroNames.Names[i];
    grdUserMacros.Cells[7, i] := GProject.MacroNames.ValueFromIndex[i];
  end;
end;

procedure TProjectOptionsForm.SaveSettings;
var
  i, j: integer;
begin
  GProject.MainUnit          := edtMainFile.FileName;
  GProject.TargetFile        := edtTargetFile.FileName;
  GProject.ProjectDir        := edtMakeDir.Directory;
  GProject.DefaultMake       := cbDefaultMakeCol.FocusItem;
  GProject.UnitOutputDir     := edtUnitOutputDir.Directory;

  CleanupCompilerMakeOptionsGrid;
  GProject.ClearAndInitMakeOptions(grdCompilerMakeOptions.RowCount);
  for i := 0 to grdCompilerMakeOptions.RowCount-1 do
  begin
    if grdCompilerMakeOptions.Cells[6, i] = '' then
      Continue;
    GProject.MakeOptions.Add(grdCompilerMakeOptions.Cells[6, i]);
    for j := 0 to 5 do // we know there is only 6 boolean columns
    begin
      if grdCompilerMakeOptions.Cells[j, i] = cCheck then
        GProject.MakeOptionsGrid[j, i] := True;
    end;
  end;

  CleanupCompilerDirs;
  GProject.ClearAndInitUnitDirsGrid(grdCompilerDirs.RowCount);
  for i := 0 to grdCompilerDirs.RowCount-1 do
  begin
    GProject.UnitDirs.Add(grdCompilerDirs.Cells[10, i]);
    for j := 0 to 9 do // we know there is only 10 boolean columns
    begin
      if grdCompilerDirs.Cells[j, i] = cCheck then
        GProject.UnitDirsGrid[j, i] := True;
    end;
  end;

  CleanupUserMacrosGrid;
  GProject.ClearAndInitMacrosGrid(grdUserMacros.RowCount);
  for i := 0 to grdUserMacros.RowCount-1 do
  begin
    GProject.MacroNames.Values[grdUserMacros.Cells[6, i]] := grdUserMacros.Cells[7, i];
  end;
end;

procedure TProjectOptionsForm.SetupCellEdit(AGrid: TfpgStringGrid);
var
  pt: TPoint;
begin
  if Assigned(FCellEdit) then
    FCellEdit.Free;

  FLastGrid := AGrid;
  FCellEdit := TfpgEdit.Create(FLastGrid.Parent);
  pt.X := FLastGrid.Left + FFocusRect.Left;
  pt.Y := FLastGrid.Top + FFocusRect.Top;
  with FCellEdit do
  begin
    Name := 'FCellEdit';
    SetPosition(pt.X, pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    Text := AGrid.Cells[AGrid.FocusCol, AGrid.FocusRow];
    OnKeyPress := @CellEditKeypressed;
    OnExit  := @CellEditExit;
    SetFocus;
  end;
end;

// Remove all rows that have empty grid options (text)
procedure TProjectOptionsForm.CleanupCompilerMakeOptionsGrid;
var
  i: integer;
begin
  for i := grdCompilerMakeOptions.RowCount-1 downto 0 do
  begin
    if Trim(grdCompilerMakeOptions.Cells[6, i]) = '' then
      grdCompilerMakeOptions.DeleteRow(i);
  end;
end;

// Remove all rows that have empty grid options (text)
procedure TProjectOptionsForm.CleanupCompilerDirs;
var
  i: integer;
begin
  for i := grdCompilerDirs.RowCount-1 downto 0 do
  begin
    if Trim(grdCompilerDirs.Cells[10, i]) = '' then
      grdCompilerDirs.DeleteRow(i);
  end;
end;

// remove all rows that have empty macro names or values
procedure TProjectOptionsForm.CleanupUserMacrosGrid;
var
  i: integer;
begin
  for i := grdUserMacros.RowCount-1 downto 0 do
  begin
    if (Trim(grdUserMacros.Cells[6, i]) = '') or (Trim(grdUserMacros.Cells[7, i]) = '') then
      grdUserMacros.DeleteRow(i);
  end;
end;

procedure TProjectOptionsForm.SaveToMacroList(AList: TIDEMacroList);
begin
//  AList.SetValue(cMacro_FPCSrcDir, edtFPCSrcDir.Directory);
end;

constructor TProjectOptionsForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInternalMacroList := TIDEMacroList.Create;
end;

destructor TProjectOptionsForm.Destroy;
begin
  FInternalMacroList.Free;
  inherited Destroy;
end;

procedure TProjectOptionsForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: ProjectOptionsForm}
  Name := 'ProjectOptionsForm';
  SetPosition(317, 177, 609, 570);
  WindowTitle := 'Project Options';
  Hint := '';
  ShowHint := True;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(514, 540, 88, 24);
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
    SetPosition(422, 540, 88, 24);
    Anchors := [anRight,anBottom];
    Text := 'OK';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrOK;
    TabOrder := 2;
  end;

  pcOptions := TfpgPageControl.Create(self);
  with pcOptions do
  begin
    Name := 'pcOptions';
    SetPosition(4, 4, 600, 524);
    Anchors := [anLeft,anRight,anTop,anBottom];
    ActivePageIndex := 0;
    Hint := '';
    TabOrder := 3;
  end;

  tsCompiler := TfpgTabSheet.Create(pcOptions);
  with tsCompiler do
  begin
    Name := 'tsCompiler';
    SetPosition(3, 24, 594, 497);
    Text := 'Compiler';
  end;

  tsDebugger := TfpgTabSheet.Create(pcOptions);
  with tsDebugger do
  begin
    Name := 'tsDebugger';
    SetPosition(3, 24, 594, 497);
    Text := 'Debugger';
  end;

  tsMacros := TfpgTabSheet.Create(pcOptions);
  with tsMacros do
  begin
    Name := 'tsMacros';
    SetPosition(3, 24, 594, 497);
    Text := 'Macros';
  end;

  tsOther := TfpgTabSheet.Create(pcOptions);
  with tsOther do
  begin
    Name := 'tsOther';
    SetPosition(3, 24, 456, 217);
    Text := 'Other';
  end;

  Label1 := TfpgLabel.Create(tsCompiler);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(4, 10, 284, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Main file';
  end;

  edtMainFile := TfpgFileNameEdit.Create(tsCompiler);
  with edtMainFile do
  begin
    Name := 'edtMainFile';
    SetPosition(4, 28, 288, 24);
    ExtraHint := '';
    FileName := '${PROJECTNAME}.pas';
    InitialDir := '';
    Filter := '';
    TabOrder := 0;
    Hint := ' ';
    OnShowHint := @BeforeShowHint;
  end;

  Label2 := TfpgLabel.Create(tsCompiler);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(300, 10, 288, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Target file (-o)';
  end;

  edtTargetFile := TfpgFileNameEdit.Create(tsCompiler);
  with edtTargetFile do
  begin
    Name := 'edtTargetFile';
    SetPosition(300, 28, 288, 24);
    ExtraHint := '';
    FileName := '${PROJECTNAME}${EXEEXT}';
    InitialDir := '';
    Filter := '';
    TabOrder := 1;
    Hint := ' ';
    OnShowHint := @BeforeShowHint;
  end;

  edtMakeCommand := TfpgFileNameEdit.Create(tsCompiler);
  with edtMakeCommand do
  begin
    Name := 'edtMakeCommand';
    SetPosition(4, 76, 288, 24);
    ExtraHint := '';
    FileName := '${COMPILER}';
    InitialDir := '';
    Filter := '';
    TabOrder := 2;
    Hint := ' ';
    OnShowHint := @BeforeShowHint;
  end;

  Label3 := TfpgLabel.Create(tsCompiler);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(4, 58, 284, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Make command';
  end;

  edtMakeDir := TfpgDirectoryEdit.Create(tsCompiler);
  with edtMakeDir do
  begin
    Name := 'edtMakeDir';
    SetPosition(300, 76, 288, 24);
    ExtraHint := '';
    Directory := '';
    RootDirectory := '';
    TabOrder := 3;
    Hint := ' ';
    OnShowHint := @BeforeShowHint;
  end;

  Label4 := TfpgLabel.Create(tsCompiler);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(300, 58, 284, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Make directory';
  end;

  Label5 := TfpgLabel.Create(tsCompiler);
  with Label5 do
  begin
    Name := 'Label5';
    SetPosition(4, 106, 284, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Message output file';
    Enabled := False;
  end;

  FilenameEdit4 := TfpgFileNameEdit.Create(tsCompiler);
  with FilenameEdit4 do
  begin
    Name := 'FilenameEdit4';
    SetPosition(4, 124, 288, 24);
    ExtraHint := '';
    FileName := '';
    InitialDir := '';
    Filter := '';
    TabOrder := 4;
    Enabled := False;
  end;

  CheckBox1 := TfpgCheckBox.Create(tsCompiler);
  with CheckBox1 do
  begin
    Name := 'CheckBox1';
    SetPosition(300, 128, 280, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 5;
    Text := 'Copy messages to file';
  end;

  Label6 := TfpgLabel.Create(tsCompiler);
  with Label6 do
  begin
    Name := 'Label6';
    SetPosition(4, 154, 144, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Default make column on Run';
  end;

  cbDefaultMakeCol := TfpgComboBox.Create(tsCompiler);
  with cbDefaultMakeCol do
  begin
    Name := 'cbDefaultMakeCol';
    SetPosition(4, 172, 132, 24);
    FontDesc := '#List';
    Hint := '';
    Items.Add('M (Make)');
    Items.Add('B (Build)');
    Items.Add('1 (Make 1)');
    Items.Add('2 (Make 2)');
    Items.Add('3 (Make 3)');
    Items.Add('4 (Make 4)');
    TabOrder := 6;
    FocusItem := 0;
  end;

  pcCompiler := TfpgPageControl.Create(tsCompiler);
  with pcCompiler do
  begin
    Name := 'pcCompiler';
    SetPosition(4, 209, 584, 282);
    Anchors := [anLeft,anRight,anTop,anBottom];
    ActivePageIndex := 0;
    Hint := '';
    TabOrder := 9;
  end;

  TabSheet1 := TfpgTabSheet.Create(pcCompiler);
  with TabSheet1 do
  begin
    Name := 'TabSheet1';
    SetPosition(3, 24, 578, 255);
    Text := 'Make options';
  end;

  TabSheet2 := TfpgTabSheet.Create(pcCompiler);
  with TabSheet2 do
  begin
    Name := 'TabSheet2';
    SetPosition(3, 24, 578, 255);
    Text := 'Directories';
  end;

  grdCompilerMakeOptions := TfpgStringGrid.Create(TabSheet1);
  with grdCompilerMakeOptions do
  begin
    Name := 'grdCompilerMakeOptions';
    SetPosition(2, 2, 574, 251);
    Anchors := [anLeft,anRight,anTop,anBottom];
    AddColumn('M', 20, taCenter);
    AddColumn('B', 20, taCenter);
    AddColumn('1', 20, taCenter);
    AddColumn('2', 20, taCenter);
    AddColumn('3', 20, taCenter);
    AddColumn('4', 20, taCenter);
    AddColumn('Command line options', 430, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := ' ';
    RowCount := 0;
    RowSelect := False;
    TabOrder := 1;
    OnClick := @grdCompilerMakeOptionsClicked;
    OnDrawCell := @grdCompilerMakeOptionsDrawCell;
    OnKeyPress := @grdCompilerMakeOptionsKeyPressed;
    OnShowHint := @BeforeShowHint;
  end;

  grdCompilerDirs := TfpgStringGrid.Create(TabSheet2);
  with grdCompilerDirs do
  begin
    Name := 'grdCompilerDirs';
    SetPosition(2, 22, 574, 231);
    Anchors := [anLeft,anRight,anTop,anBottom];
    AddColumn('M', 20, taCenter);
    AddColumn('B', 20, taCenter);
    AddColumn('1', 20, taCenter);
    AddColumn('2', 20, taCenter);
    AddColumn('3', 20, taCenter);
    AddColumn('4', 20, taCenter);
    AddColumn('U', 20, taCenter);
    AddColumn('I', 20, taCenter);
    AddColumn('L', 20, taCenter);
    AddColumn('O', 20, taCenter);
    AddColumn('Directories', 350, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := ' ';
    ParentShowHint := False;
    RowCount := 0;
    RowSelect := False;
    ShowHint := True;
    TabOrder := 1;
    OnClick := @grdCompilerDirsClicked;
    OnDrawCell := @grdCompilerDirsDrawCell;
    OnKeyPress := @grdCompilerDirsKeyPressed;
    OnShowHint := @BeforeShowHint;
  end;

  Label11 := TfpgLabel.Create(TabSheet2);
  with Label11 do
  begin
    Name := 'Label11';
    SetPosition(4, 4, 560, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Unit (-Fu), Include (-Fi), Library (-Fl) and Object (-Fo) directories';
  end;

  Label7 := TfpgLabel.Create(tsDebugger);
  with Label7 do
  begin
    Name := 'Label7';
    SetPosition(4, 10, 296, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Debug command';
  end;

  FilenameEdit5 := TfpgFileNameEdit.Create(tsDebugger);
  with FilenameEdit5 do
  begin
    Name := 'FilenameEdit5';
    SetPosition(4, 28, 584, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FileName := '';
    InitialDir := '';
    Filter := '';
    TabOrder := 2;
  end;

  Label8 := TfpgLabel.Create(tsDebugger);
  with Label8 do
  begin
    Name := 'Label8';
    SetPosition(4, 58, 420, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Debug options';
  end;

  Edit1 := TfpgEdit.Create(tsDebugger);
  with Edit1 do
  begin
    Name := 'Edit1';
    SetPosition(4, 76, 584, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    Hint := '';
    TabOrder := 4;
    Text := '';
    FontDesc := '#Edit1';
  end;

  CheckBox2 := TfpgCheckBox.Create(tsDebugger);
  with CheckBox2 do
  begin
    Name := 'CheckBox2';
    SetPosition(4, 124, 152, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 5;
    Text := 'Activate on Break';
  end;

  CheckBox3 := TfpgCheckBox.Create(tsDebugger);
  with CheckBox3 do
  begin
    Name := 'CheckBox3';
    SetPosition(160, 124, 152, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 6;
    Text := 'Stop on Exception';
  end;

  CheckBox4 := TfpgCheckBox.Create(tsDebugger);
  with CheckBox4 do
  begin
    Name := 'CheckBox4';
    SetPosition(320, 124, 160, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 7;
    Text := 'Show Console on Run';
  end;

  PageControl1 := TfpgPageControl.Create(tsDebugger);
  with PageControl1 do
  begin
    Name := 'PageControl1';
    SetPosition(4, 153, 584, 338);
    ActivePageIndex := 0;
    Hint := '';
    TabOrder := 8;
  end;

  TabSheet3 := TfpgTabSheet.Create(PageControl1);
  with TabSheet3 do
  begin
    Name := 'TabSheet3';
    SetPosition(3, 24, 578, 311);
    Text := 'Source directories';
  end;

  TabSheet4 := TfpgTabSheet.Create(PageControl1);
  with TabSheet4 do
  begin
    Name := 'TabSheet4';
    SetPosition(3, 24, 578, 311);
    Text := 'Defines';
  end;

  TabSheet5 := TfpgTabSheet.Create(PageControl1);
  with TabSheet5 do
  begin
    Name := 'TabSheet5';
    SetPosition(3, 24, 578, 311);
    Text := 'Signals';
  end;

  TabSheet6 := TfpgTabSheet.Create(PageControl1);
  with TabSheet6 do
  begin
    Name := 'TabSheet6';
    SetPosition(3, 24, 578, 311);
    Text := 'Exceptions';
  end;

  TabSheet7 := TfpgTabSheet.Create(PageControl1);
  with TabSheet7 do
  begin
    Name := 'TabSheet7';
    SetPosition(3, 24, 578, 311);
    Text := 'Target';
  end;

  grdDebugSrcDirs := TfpgStringGrid.Create(TabSheet3);
  with grdDebugSrcDirs do
  begin
    Name := 'grdDebugSrcDirs';
    SetPosition(2, 2, 574, 307);
    AddColumn('New', 550, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 5;
    RowSelect := False;
    ShowHeader := False;
    TabOrder := 1;
  end;

  btnShowCmdLine := TfpgButton.Create(tsCompiler);
  with btnShowCmdLine do
  begin
    Name := 'btnShowCmdLine';
    SetPosition(160, 172, 132, 24);
    Text := 'Show command line';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 7;
    OnClick :=@btnShowCmdLineClicked;
  end;

  edtUnitOutputDir := TfpgDirectoryEdit.Create(tsCompiler);
  with edtUnitOutputDir do
  begin
    Name := 'edtUnitOutputDir';
    SetPosition(300, 172, 288, 24);
    ExtraHint := '';
    Directory := '';
    RootDirectory := '';
    TabOrder := 8;
    Hint := ' ';
    OnShowHint := @BeforeShowHint;
  end;

  Label9 := TfpgLabel.Create(tsCompiler);
  with Label9 do
  begin
    Name := 'Label9';
    SetPosition(300, 154, 280, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Unit output directory (-FU)';
  end;

  Label10 := TfpgLabel.Create(tsMacros);
  with Label10 do
  begin
    Name := 'Label10';
    SetPosition(4, 10, 580, 16);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Active Group';
  end;

  grdMacroGroup := TfpgStringGrid.Create(tsMacros);
  with grdMacroGroup do
  begin
    Name := 'grdMacroGroup';
    SetPosition(4, 28, 584, 104);
    Anchors := [anLeft,anRight,anTop];
    AddColumn('#', 20, taCenter);
    AddColumn('', 20, taCenter);
    AddColumn('Name', 520, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 6;
    RowSelect := False;
    ShowHeader := False;
    TabOrder := 0;
    Columns[0].BackgroundColor := clLightGray;
    Cells[0,0] := '1';
    Cells[0,1] := '2';
    Cells[0,2] := '3';
    Cells[0,3] := '4';
    Cells[0,4] := '5';
    Cells[0,5] := '6';
    FocusCol := 2;
    OnCanSelectCell := @grdMacroGroupCanSelectCell;
  end;

  Label12 := TfpgLabel.Create(tsMacros);
  with Label12 do
  begin
    Name := 'Label12';
    SetPosition(4, 144, 572, 16);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    Text := 'User defined macros';
  end;

  grdUserMacros := TfpgStringGrid.Create(tsMacros);
  with grdUserMacros do
  begin
    Name := 'grdUserMacros';
    SetPosition(4, 162, 584, 328);
    Anchors := [anLeft,anRight,anTop,anBottom];
    AddColumn('1', 20, taCenter);
    AddColumn('2', 20, taCenter);
    AddColumn('3', 20, taCenter);
    AddColumn('4', 20, taCenter);
    AddColumn('5', 20, taCenter);
    AddColumn('6', 20, taCenter);
    AddColumn('Name', 150, taLeftJustify);
    AddColumn('Value', 290, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := False;
    TabOrder := 1;
    OnClick := @grdUserMacrosClicked;
    OnDrawCell := @grdUserMacrosDrawCell;
  end;

  {@VFD_BODY_END: ProjectOptionsForm}
  {%endregion}
end;


end.
