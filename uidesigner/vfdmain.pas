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
      Main window functionality and designer class.
}

unit vfdmain;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_dialogs,
  fpg_constants,
  vfdprops,
  vfdforms,
  vfddesigner,
  vfdfile,
  newformdesigner;

const
  program_version = FPGUI_VERSION;

type

  TMainDesigner = class(TObject)
  private
    procedure   SetEditedFileName(const Value: string);
    procedure   LoadDefaults;
  protected
    FDesigners: TList;
    FFile: TVFDFile;
    FEditedFileName: string;
  public
    GridResolution: integer;
    SaveComponentNames: boolean;
    selectedform: TFormDesigner;
    constructor Create;
    destructor  Destroy; override;
    procedure   CreateWindows;
    procedure   SelectForm(aform: TFormDesigner);
    function    Designer(index: integer): TFormDesigner;
    function    DesignerCount: integer;
    function    NewFormName: string;
    procedure   CreateParseForm(const FormName, FormHead, FormBody: string);
    procedure   OnNewForm(Sender: TObject);
    procedure   OnNewFile(Sender: TObject);
    procedure   OnSaveFile(Sender: TObject);
    procedure   OnLoadFile(Sender: TObject);
    procedure   OnPropTextChange(Sender: TObject);
    procedure   OnPropNameChange(Sender: TObject);
    procedure   OnPropPosEdit(Sender: TObject);
    procedure   OnOtherChange(Sender: TObject);
    procedure   OnAnchorChange(Sender: TObject);
    procedure   OnEditWidget(Sender: TObject);
    procedure   OnEditWidgetOrder(Sender: TObject);
    procedure   OnEditTabOrder(Sender: TObject);
    procedure   OnExit(Sender: TObject);
    procedure   OnOptionsClick(Sender: TObject);
    property    EditedFileName: string read FEditedFileName write SetEditedFileName;
  end;


var
  maindsgn: TMainDesigner;


implementation

uses
  vfdformparser,
  fpg_iniutils,
  fpg_utils;

var
  DefaultPasExt : String = '.pas';
  OneClickMove: Boolean;

{ TMainDesigner }

procedure TMainDesigner.OnNewFile(Sender: TObject);
var
  n: integer;
begin
  EditedFileName := '';
  for n := 0 to FDesigners.Count - 1 do
  begin
    selectedform := nil;
    TFormDesigner(FDesigners[n]).Free;
  end;
  FDesigners.Clear;
  OnNewForm(Sender);
end;


procedure TMainDesigner.OnLoadFile(Sender: TObject);
var
  n, m: integer;
  bl, bl2: TVFDFileBlock;
  fname: string;
  afiledialog: TfpgFileDialog;
begin
  fname := EditedFileName;

  if Sender <> maindsgn then
  begin
    afiledialog          := TfpgFileDialog.Create(nil);
    afiledialog.Filename := EditedFilename;
    afiledialog.WindowTitle := 'Open form file';
    afiledialog.Filter   := 'Pascal source files (*.pp;*.pas;*.inc;*.dpr;*.lpr)|*.pp;*.pas;*.inc;*.dpr;*.lpr|All Files (*)|*';
    if afiledialog.RunOpenFile then
    begin
      EditedFileName := aFileDialog.Filename;
      fname          := EditedFilename;
    end
    else
      fname          := '';
    FreeAndNil(aFileDialog);
  end;

  if fname = '' then
    Exit;

  for n := 0 to FDesigners.Count - 1 do
  begin
    selectedform := nil;
    TFormDesigner(FDesigners[n]).Free;
  end;
  FDesigners.Clear;
  
  if not fpgFileExists(fname) then
  begin
    ShowMessage('File does not exists.', 'Error loading form');
    Exit;
  end;

  FFile.LoadFile(fname);
  FFile.GetBlocks;

  for n := 0 to FFile.BlockCount-1 do
  begin
    bl := FFile.Block(n);
    if bl.BlockID = 'VFD_HEAD_BEGIN' then
      for m := n + 1 to FFile.BlockCount-1 do
      begin
        bl2 := FFile.Block(m);
        if (bl2.BlockID = 'VFD_BODY_BEGIN') and (bl2.FormName = bl.FormName) then
          CreateParseForm(bl.FormName, bl.Data, bl2.Data); // pair was found
      end;
  end;
  
  frmMain.mru.AddItem(fname);
end;

procedure TMainDesigner.OnSaveFile(Sender: TObject);
var
  n, i: integer;
  fd: TFormDesigner;
  fdata: string;
  ff: file;
  fname, uname: string;
  aFileDialog: TfpgFileDialog;
begin
  fname := EditedFileName;

  if ((Sender as TComponent).Tag = 10) and (EditedFileName <> '') then
    fname := EditedFileName
  else
  begin
    afiledialog          := TfpgFileDialog.Create(nil);
    afiledialog.Filename := EditedFilename;
    afiledialog.WindowTitle := 'Save form source';
    afiledialog.Filter   := 'Pascal source files (*.pp;*.pas;*.inc;*.dpr;*.lpr)|*.pp;*.pas;*.inc;*.dpr;*.lpr|All Files (*)|*';
    if afiledialog.RunSaveFile then
    begin
      fname:=aFileDialog.Filename;
      if (ExtractFileExt(fname)='') then
        fname:=fname+DefaultPasExt;
      EditedFileName := fname;
    end
    else
      fname := '';
    aFileDialog.Free;
  end;

  if fname = '' then
    Exit;

  EditedFileName := fname;

  if fpgFileExists(fname) then
  begin
    FFile.LoadFile(fname);
    FFile.GetBlocks;
  end
  else
  begin
    uname := fpgExtractFileName(fname);
    i     := pos('.pas', LowerCase(uname));
    if i > 0 then
      uname := copy(uname, 1, i - 1);
    FFile.NewFileSkeleton(uname);
  end;

  for n := 0 to DesignerCount-1 do
  begin
    fd := nil;
    fd := Designer(n);
    if fd = nil then
      raise Exception.Create('Failed to find Designer Form');
    FFile.SetFormData(fd.Form.Name, fd.GetFormSourceDecl, fd.GetFormSourceImpl);
  end;

  fdata := FFile.MergeBlocks;

  AssignFile(ff, fpgToOSEncoding(fname));
  try
    Rewrite(ff, 1);
    try
      BlockWrite(ff, fdata[1], length(fdata));
    finally
      CloseFile(ff);
    end;
    frmMain.mru.AddItem(fname);
  except
    on E: Exception do
      raise Exception.Create('Form save I/O failure in TMainDesigner.OnSaveFile.' + #13 +
          E.Message);
  end;
end;

procedure TMainDesigner.OnAnchorChange(Sender: TObject);
begin
  if SelectedForm <> nil then
    SelectedForm.OnAnchorChange(Sender);
end;

procedure TMainDesigner.OnOtherChange(Sender: TObject);
begin
  if SelectedForm <> nil then
    SelectedForm.OnOtherChange(Sender);
end;

procedure TMainDesigner.OnPropNameChange(Sender: TObject);
begin
  if SelectedForm <> nil then
    SelectedForm.OnPropNameChange(Sender);
end;

procedure TMainDesigner.OnPropPosEdit(Sender: TObject);
begin
  if SelectedForm <> nil then
    SelectedForm.OnPropPosEdit(Sender);
end;

procedure TMainDesigner.OnPropTextChange(Sender: TObject);
begin
  if SelectedForm <> nil then
    SelectedForm.OnPropTextChange(Sender);
end;

procedure TMainDesigner.OnNewForm(Sender: TObject);
var
  fd: TFormDesigner;
  nfrm: TNewFormForm;

  function DoesNameAlreadyExist(const AName: string): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to FDesigners.Count-1 do
    begin
      if TFormDesigner(FDesigners[i]).Form.Name = AName then
      begin
        Result := True;
        break;
      end;
    end;
  end;
begin
  nfrm := TNewFormForm.Create(nil);
  try
    if nfrm.ShowModal = mrOK then
    begin
      if DoesNameAlreadyExist(nfrm.edName.Text) then
      begin
        TfpgMessageDialog.Critical('Name Conflict','The form name already exists in the current unit, please try again');
        exit;
      end;
      fd := TFormDesigner.Create;
      if nfrm.edName.Text <> '' then
        fd.Form.Name := nfrm.edName.Text;
      fd.Form.WindowTitle := fd.Form.Name;
      fd.OneClickMove := OneClickMove;
      FDesigners.Add(fd);
      SelectedForm := fd;
      fd.Show;
    end;

  finally
    nfrm.Free;
  end;
end;

procedure TMainDesigner.CreateWindows;
begin
  frmMain := TfrmMain.Create(nil);
  frmMain.WindowTitle := 'fpGUI Designer v' + program_version;
  frmMain.Show;

  frmProperties := TfrmProperties.Create(nil);
  frmProperties.Show;
end;

constructor TMainDesigner.Create;
begin
  FDesigners   := TList.Create;
  SelectedForm := nil;
  FFile        := TVFDFile.Create;

  // options
  SaveComponentNames := True;
  LoadDefaults;
  
  FEditedFileName := '';
end;

destructor TMainDesigner.Destroy;
var
  n: integer;
begin
  for n := 0 to FDesigners.Count - 1 do
    TFormDesigner(FDesigners[n]).Free;
  FDesigners.Free;
  FFile.Free;
  
  frmProperties.Free;
  frmMain.Free;
  inherited;
end;

procedure TMainDesigner.SelectForm(aform: TFormDesigner);
begin
  if (SelectedForm <> nil) and (SelectedForm <> aform) then
    SelectedForm.DeSelectAll;
  SelectedForm := aform;
end;

function TMainDesigner.Designer(index: integer): TFormDesigner;
begin
  Result := nil;
  if (index < 0) or (index > FDesigners.Count-1) then
    Exit;
  Result := TFormDesigner(FDesigners[index]);
end;

function TMainDesigner.DesignerCount: integer;
begin
  Result := FDesigners.Count;
end;

function TMainDesigner.NewFormName: string;
var
  n, i: integer;
  s: string;
begin
  i := 0;
  repeat
    Inc(i);
    s := 'Form' + IntToStr(i);
    n := 0;
    while (n < DesignerCount) do
    begin
      if Designer(n).Form.Name = s then
        Break;
      Inc(n);
    end;
  until n > DesignerCount-1;
  Result := s;
end;

procedure TMainDesigner.CreateParseForm(const FormName, FormHead, FormBody: string);
var
  fd: TFormDesigner;
  fp: TVFDFormParser;
begin
  fp := TVFDFormParser.Create(FormName, FormHead, FormBody);
  fd := fp.ParseForm;
  fd.OneClickMove := OneClickMove;
  fp.Free;

  FDesigners.Add(fd);
  fd.Show;
end;

procedure TMainDesigner.OnEditWidget(Sender: TObject);
begin
  if SelectedForm <> nil then
    SelectedForm.OnEditWidget(Sender);
end;

procedure TMainDesigner.OnEditWidgetOrder(Sender: TObject);
begin
  if SelectedForm <> nil then
    SelectedForm.EditWidgetOrTabOrder(emWidgetOrder);
end;

procedure TMainDesigner.OnEditTabOrder(Sender: TObject);
begin
  if SelectedForm <> nil then
    SelectedForm.EditWidgetOrTabOrder(emTabOrder);
end;

procedure TMainDesigner.OnExit(Sender: TObject);
begin
  frmProperties.Close;
  frmMain.Close;
end;

procedure TMainDesigner.OnOptionsClick(Sender: TObject);
var
  frm: TfrmVFDSetup;
begin
  frm := TfrmVFDSetup.Create(nil);
  try
    if frm.ShowModal = mrOK then
    begin
      LoadDefaults;
      frmMain.mru.MaxItems      := gINI.ReadInteger('Options', 'MRUFileCount', 4);
      frmMain.mru.ShowFullPath  := gINI.ReadBool('Options', 'ShowFullPath', True);
    end;
  finally
    frm.Free;
  end;
end;

procedure TMainDesigner.SetEditedFileName(const Value: string);
var
  s: string;
begin
  FEditedFileName := Value;
  s := ExtractFileName(FEditedFileName);
  if s = '' then
    s := '[new]';
  frmMain.WindowTitle := 'fpGUI Designer v' + program_version + ' - ' + s;
end;

procedure TMainDesigner.LoadDefaults;
begin
  case gINI.ReadInteger('Options', 'GridResolution', 1) of
    0: GridResolution := 2;
    1: GridResolution := 4;
    2: GridResolution := 8;
  end;
  DefaultPasExt   := gINI.ReadString('Options', 'DefaultFileExt', '.pas');
  UndoOnPropExit  := gINI.ReadBool('Options', 'UndoOnExit', DefUndoOnPropExit);
  OneClickMove    := gINI.ReadBool('Options', 'OneClickMove', True);
  fpgApplication.HintPause := 1000;
end;

end.

