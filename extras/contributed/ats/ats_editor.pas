program ats_editor;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_customgrid, fpg_basegrid,
  ats_main, fpg_grid, fpg_form, fpg_button, fpg_edit, fpg_menu, fpg_label,
  fpg_combobox, fpg_dialogs, fpg_utils;

const
  langtabledata:
    {$I atstable.inc}

type

  { TLangGrid }

  TLangGrid = class(TfpgCustomGrid)
  protected
    procedure DrawCell(ARow, ACol: Integer; ARect: TfpgRect; AFlags: TfpgGridDrawState); override;
    procedure HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    function GetRowCount: Integer; override;
  public
    atstable: TatsTextTable;
    procedure UpdateColumns;
  end;

  { TfrmLangTable }

  TfrmLangTable = class(TfpgForm)
  public
    menuFile: TfpgPopupMenu;
  
    {@VFD_HEAD_BEGIN: frmLangTable}
    mainmenu: TfpgMenuBar;
    grid: TLangGrid;
    btnNewRow: TfpgButton;
    btnCopyRow: TfpgButton;
    btnDeleteRow: TfpgButton;
    btnEdit: TfpgButton;
    {@VFD_HEAD_END: frmLangTable}
    
    procedure AfterCreate; override;
    
    procedure menuProcExit(Sender: TObject);
    procedure menuProcSave(Sender: TObject);
    procedure menuProcOpen(Sender: TObject);
    procedure menuProcNew(Sender: TObject);

    procedure EditClick(Sender : TObject);

  end;

  { TfrmTextEdit }

  TfrmTextEdit = class(TfpgForm)
  public
    {@VFD_HEAD_BEGIN: frmTextEdit}
    Label1: TfpgLabel;
    edID: TfpgEdit;
    cmbLang1: TfpgComboBox;
    edLang1: TfpgEdit;
    cmbLang2: TfpgComboBox;
    edLang2: TfpgEdit;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    {@VFD_HEAD_END: frmTextEdit}
    
    textrow : TatsTextRow;

    procedure AfterCreate; override;
    
    procedure OnLangChange(sender : TObject);
    
    procedure HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    
    procedure LoadTexts;
  end;

{@VFD_NEWFORM_DECL}

var
  frmMain : TfrmLangTable;

{@VFD_NEWFORM_IMPL}

{ TLangGrid }

procedure TLangGrid.DrawCell(ARow, ACol: Integer; ARect: TfpgRect; AFlags: TfpgGridDrawState);
var
  s : string;
  tr : TatsTextRow;
  b : boolean;
begin
  tr := atstable.GetRow(ARow);
  if ACol = 0 then
  begin
    s := tr.TextId;
  end
  else
  begin
    //s := 'Col '+IntToStr(ACol);
    s := tr.GetText(Columns[ACol].Title, b);
  end;
  Canvas.DrawString(ARect.Left+1, ARect.Top+1, s);

  //inherited DrawCell(ARow, ACol, ARect, AFlags);
end;

function TLangGrid.GetRowCount: Integer;
begin
  if atstable <> nil then
    result := atstable.RowCount
  else
    result := 2;
end;

procedure TLangGrid.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if keycode = keyEnter then
  begin
    frmMain.btnEdit.Click;
  end
  else
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TLangGrid.UpdateColumns;
var
  n : integer;
  gc : TfpgGridColumn;
begin
  if atstable = nil then Exit;

  ColumnCount := atstable.LangList.Count+1;

  gc := Columns[0];
  gc.Title := 'ID';
  gc.Width := 140;

  for n := 0 to atstable.LangList.Count-1 do
  begin
    gc := Columns[n+1];
    gc.Title := atstable.Langlist[n];
    gc.Width := 80;
  end;
end;

procedure TfrmTextEdit.AfterCreate;
begin
  textrow := nil;

  {@VFD_BODY_BEGIN: frmTextEdit}
  Name := 'frmTextEdit';
  SetPosition(326, 139, 466, 168);
  WindowTitle := 'Edit Text';
  Hint := '';

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(8, 8, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Text ID:';
  end;

  edID := TfpgEdit.Create(self);
  with edID do
  begin
    Name := 'edID';
    SetPosition(8, 28, 228, 24);
    ExtraHint := '';
    Hint := '';
    TabOrder := 1;
    Text := '';
    FontDesc := '#Edit1';
  end;

  cmbLang1 := TfpgComboBox.Create(self);
  with cmbLang1 do
  begin
    Name := 'cmbLang1';
    SetPosition(8, 65, 80, 22);
    FontDesc := '#List';
    Hint := '';
    TabOrder := 3;
    OnChange := @OnLangChange;
  end;

  edLang1 := TfpgEdit.Create(self);
  with edLang1 do
  begin
    Name := 'edLang1';
    SetPosition(100, 64, 356, 24);
    ExtraHint := '';
    Hint := '';
    TabOrder := 3;
    Text := '';
    FontDesc := '#Edit1';
  end;

  cmbLang2 := TfpgComboBox.Create(self);
  with cmbLang2 do
  begin
    Name := 'cmbLang2';
    SetPosition(8, 97, 80, 22);
    FontDesc := '#List';
    Hint := '';
    TabOrder := 4;
    OnChange := @OnLangChange;
  end;

  edLang2 := TfpgEdit.Create(self);
  with edLang2 do
  begin
    Name := 'edLang2';
    SetPosition(100, 96, 356, 24);
    ExtraHint := '';
    Hint := '';
    TabOrder := 4;
    Text := '';
    FontDesc := '#Edit1';
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    Name := 'btnOK';
    SetPosition(8, 133, 99, 24);
    Text := 'OK';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.ok';
    ModalResult := mrOK;
    TabOrder := 6;
    Default := true;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(356, 133, 99, 24);
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.cancel';
    ModalResult := mrCancel;
    TabOrder := 7;
  end;

  {@VFD_BODY_END: frmTextEdit}
end;

procedure TfrmTextEdit.OnLangChange(sender: TObject);
var
  b : boolean;
begin
  if sender = cmbLang1 then
  begin
    edLang1.Text := textrow.GetText(cmbLang1.Text, b);
  end
  else
  begin
    edLang2.Text := textrow.GetText(cmbLang2.Text, b);
  end;
end;

procedure TfrmTextEdit.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if keycode = keyEnter then btnOK.Click
  else if keycode = keyEscape then btnCancel.Click
  else inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfrmTextEdit.LoadTexts;
var
  b : boolean;
begin
  edLang1.Text := textrow.GetText(cmbLang1.Text, b);
  edLang2.Text := textrow.GetText(cmbLang2.Text, b);
end;


procedure TfrmLangTable.AfterCreate;
var
  mi : TfpgMenuItem;
begin
  {@VFD_BODY_BEGIN: frmLangTable}
  Name := 'frmLangTable';
  SetPosition(282, 304, 619, 513);
  WindowTitle := 'ATS Table Editor';
  Hint := '';

  mainmenu := TfpgMenuBar.Create(self);
  with mainmenu do
  begin
    Name := 'mainmenu';
    SetPosition(0, 0, 619, 28);
    Anchors := [anLeft,anRight,anTop];
  end;

  grid := TLangGrid.Create(self);
  with grid do
  begin
    Name := 'grid';
    SetPosition(0, 28, 619, 447);
    Anchors := [anLeft,anRight,anTop,anBottom];
  end;

  btnNewRow := TfpgButton.Create(self);
  with btnNewRow do
  begin
    Name := 'btnNewRow';
    SetPosition(8, 482, 75, 24);
    Anchors := [anLeft,anBottom];
    Text := 'New Row';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
  end;

  btnCopyRow := TfpgButton.Create(self);
  with btnCopyRow do
  begin
    Name := 'btnCopyRow';
    SetPosition(92, 482, 71, 24);
    Anchors := [anLeft,anBottom];
    Text := 'Copy Row';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
  end;

  btnDeleteRow := TfpgButton.Create(self);
  with btnDeleteRow do
  begin
    Name := 'btnDeleteRow';
    SetPosition(292, 482, 83, 24);
    Anchors := [anLeft,anBottom];
    Text := 'Delete Row';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
  end;

  btnEdit := TfpgButton.Create(self);
  with btnEdit do
  begin
    Name := 'btnEdit';
    SetPosition(188, 482, 79, 24);
    Anchors := [anLeft,anBottom];
    Text := 'Edit Item';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 5;
    OnClick := @EditClick;
  end;

  {@VFD_BODY_END: frmLangTable}
  
  menuFile := TfpgPopupMenu.Create(self);
  menuFile.AddMenuItem('&New', '', @menuProcNew);
  menuFile.AddMenuItem('&Open...', '', @menuProcOpen);
  menuFile.AddMenuItem('&Save...', '', @menuProcSave);
  menuFile.AddMenuItem('-', '', nil);
  menuFile.AddMenuItem('&Exit', '', @menuProcExit);

  mainmenu.AddMenuItem('&File', nil).SubMenu := menuFile;

  grid.atstable := atsTexts;
  grid.UpdateColumns;
end;

procedure TfrmLangTable.menuProcExit(Sender: TObject);
begin
  Close;
end;

procedure TfrmLangTable.menuProcSave(Sender: TObject);
var
  dlg : TfpgFileDialog;
  fname : string;
begin
  dlg := TfpgFileDialog.Create(nil);
  dlg.Filter := 'Pascal include (*.inc;*.pas)|*.inc;*.pas|ATS text (*.ats)|*.ats|CSV (*.csv)|*.csv|All Files (*)|*|';

  if dlg.RunSaveFile then fname := dlg.FileName
                     else fname := '';
                     
  dlg.Free;

  if fname <> '' then
  begin
    if ExtractFileExt(fname) = '' then fname := fname + '.inc';
    if (UpperCase(ExtractFileExt(fname)) = '.INC')
    or (UpperCase(ExtractFileExt(fname)) = '.PAS') then
    begin
      atsTexts.SaveToFile(fname, atsPascalSource);
    end
    else if UpperCase(ExtractFileExt(fname)) = '.CSV' then
    begin
      atsTexts.SaveToFile(fname, atsCSV);
    end
    else // if UpperCase(ExtractFileExt(fname)) = '.ATS' then
    begin
      atsTexts.SaveToFile(fname, atsPureText);
    end;

    ShowMessage('Save done.');
  end;
end;

procedure TfrmLangTable.menuProcOpen(Sender: TObject);
var
  dlg : TfpgFileDialog;
  fname : string;
begin
  dlg := TfpgFileDialog.Create(nil);
  dlg.Filter := 'Pascal include (*.inc;*.pas)|*.inc;*.pas|ATS text (*.ats)|*.ats|CSV (*.csv)|*.csv|All Files (*)|*|';

  if dlg.RunOpenFile then fname := dlg.FileName
                     else fname := '';

  dlg.Free;
  
  if not FileExists(fname) then
  begin
    ShowMessage('File does not exists.');
    Exit;
  end;

  if fname <> '' then
  begin
    if (UpperCase(ExtractFileExt(fname)) = '.INC')
    or (UpperCase(ExtractFileExt(fname)) = '.PAS') then
    begin
      atsTexts.LoadFromPascalFile(fname);
    end
    else if UpperCase(ExtractFileExt(fname)) = '.CSV' then
    begin
      ShowMessage('CSV loading is not supported.');
      
      //atsTexts.SaveToFile(fname, atsCSV);
    end
    else // if UpperCase(ExtractFileExt(fname)) = '.ATS' then
    begin
      atsTexts.LoadFromFile(fname);
    end;
  end;
end;

procedure TfrmLangTable.menuProcNew(Sender: TObject);
begin
  atsTexts.Clear;
  grid.Update;
end;

procedure TfrmLangTable.EditClick(Sender: TObject);
var
  frm : TfrmTextEdit;
  tr : TatsTextRow;
begin
  tr := grid.atstable.GetRow(grid.FocusRow);

  frm := TfrmTextEdit.Create(nil);
  
  frm.textrow := tr;
  
  // load
  frm.edID.Text := tr.TextID;
  
  frm.cmbLang1.Items.Assign(grid.atstable.LangList);
  frm.cmbLang2.Items.Assign(grid.atstable.LangList);
  
  frm.cmbLang1.Text := 'EN';
  
  if grid.FocusCol > 0
    then frm.cmbLang2.Text := grid.Columns[grid.FocusCol].Title
    else frm.cmbLang2.Text := 'EN';
    
  frm.LoadTexts;
  
  frm.ActiveWidget := frm.edLang2;
  
  if frm.ShowModal = mrOK then
  begin
    // store
    tr.SetText(frm.cmbLang1.Text,frm.edLang1.Text);
    if frm.cmbLang2.Text <> frm.cmbLang1.Text
      then tr.SetText(frm.cmbLang2.Text,frm.edLang2.Text);
    
    grid.Update;
  end;
  
  frm.Free;
end;

procedure MainProc;
begin
  fpgApplication.Initialize;
  
  //atsTexts.LoadFromFile('test.ats');
  
  //atsTexts.LoadFromArray(langtabledata);
  if fpgFileExists('atstable.inc')  then
    atsTexts.LoadFromPascalFile('atstable.inc');
  
  frmMain := TfrmLangTable.Create(nil);
  
  frmMain.Show;
  fpgApplication.Run;
  frmMain.Free;
end;

begin
  MainProc;
end.


