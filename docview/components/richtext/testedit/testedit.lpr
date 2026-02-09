program testedit;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_memo,
  fpg_form,
  fpg_dialogs,
  fpg_menu,
  frarichtextedit,
  fpg_utils;

type

  TMainForm = class(TfpgForm)
  private
    FFileName : string;
    procedure DoNewFile(Sender: TObject);
    procedure DoOpenFile(Sender: TObject);
    procedure DoQuit(Sender: TObject);
    procedure DoSaveFile(Sender: TObject);
    procedure LoadFile(const AFileName: String);
    procedure SaveFile(const AFileName: String);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    FMenu: TfpgMenuBar;
    pmFile: TfpgPopupMenu;
    FEdit: TRichTextEditFrame;
    {@VFD_HEAD_END: MainForm}
    procedure AfterCreate; override;
  end;

  {@VFD_NEWFORM_DECL}

resourcestring
  rsEditingCaption = 'Editing %s';


  {@VFD_NEWFORM_IMPL}

procedure TMainForm.DoNewFile(Sender: TObject);
begin
  FEdit.RichText := '';
  FFileName := '';
  WindowTitle := Format(rsEditingCaption, ['new file']);
end;

procedure TMainForm.LoadFile(COnst AFileName :String);
var
  L: TStrings;
begin
  FFileName:=AFileName;
  L := TStringList.Create;
  try
    L.LoadFromFile(AFileName);
    FEdit.RichText := L.Text;
  finally
    L.Free;
  end;
  WindowTitle := Format(rsEditingCaption, [AFileName]);
end;

procedure TMainForm.SaveFile(COnst AFileName :String);
var
  L: TStrings;
begin
  FFileName := AFileName;
  L := TStringList.Create;
  try
    L.Text := FEdit.RichText;
    L.SaveToFile(AFileName);
  finally
    L.Free;
  end;
  WindowTitle := Format(rsEditingCaption, [AFileName]);
end;


procedure TMainForm.DoOpenFile(Sender: TObject);
var
  FN: String;
begin
  FN := SelectFileDialog(sfdOpen, 'Text files|*.txt|All files|'+AllFilesMask, '');
  if (FN <> '') then
    LoadFile(FN);
end;

procedure TMainForm.DoQuit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.DoSaveFile(Sender: TObject);
var
  FN: String;
begin
  FN := SelectFileDialog(sfdSave, 'Text files|*.txt|All files|'+AllFilesMask, '');
  if (FN <> '') then
    SaveFile(FN);
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' }

  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(496, 295, 739, 502);
  WindowTitle := 'Editing new file';
  Hint := '';
  IconName := '';
  ShowHint := True;

  FMenu := TfpgMenuBar.Create(self);
  with FMenu do
  begin
    Name := 'FMenu';
    SetPosition(0, 0, 739, 24);
    Align := alTop;
  end;

  pmFile := TfpgPopupMenu.Create(self);
  with pmFile do
  begin
    Name := 'pmFile';
    SetPosition(556, 36, 164, 20);
    AddMenuItem('&New',  'Ctrl+N', @DoNewFile);
    AddMenuItem('&Open', 'Ctrl+O', @DoOpenFile);
    AddMenuItem('&Save', 'Ctrl+S', @DoSaveFile);
    AddMenuItem('&Quit', 'Ctrl+Q', @DoQuit);
  end;

  FEdit := TRichTextEditFrame.Create(self);
  with FEdit do
  begin
    Name := 'FEdit';
    SetPosition(32, 80, 371, 334);
    Align := alClient;
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}

  FMenu.AddMenuItem('File', nil).SubMenu := pmFile;
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  RegisterStdRichTextImages;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    if (ParamCount=1) and fpgFileExists(ParamStr(1)) then
      frm.LoadFile(Paramstr(1));
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.

