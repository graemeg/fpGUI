program testboard;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  fpg_base,
  fpg_tab,
  fpg_button,
  fpg_panel,
  fpg_main,
  fpg_memo,
  fpg_form,
  fpg_dialogs,
  fpg_menu,
  RichTextView, frarichtextedit,
  fpg_imagelist,
  fpg_imgfmt_bmp,
  fpg_imgfmt_png,
  fpg_imgfmt_jpg;

type

  {@VFD_NEWFORM_DECL}
  { TMainForm }

  TMainForm = class(TfpgForm)
    procedure DoNewFile(Sender: TObject);
    procedure DoOpenFile(Sender: TObject);
    procedure DoQuit(Sender: TObject);
    procedure DoSaveFile(Sender: TObject);
  private
    procedure LoadFile(const AFileName: String);
    procedure SaveFile(const AFileName: String);
  public
    FMenu : TfpgMenuBar;
    MFile,
    MNew,
    MOpen,
    MSave,
    MQuit: TfpgMenuItem;
    FFileName : string;
    {@VFD_HEAD_BEGIN: MainForm}
    FEdit : TRichTextEditFrame;
    {@VFD_HEAD_END: MainForm}
    procedure AfterCreate; override;
  end;



  {@VFD_NEWFORM_IMPL}

procedure TMainForm.DoNewFile(Sender: TObject);
begin
  FEdit.RichText:='';
  FFileName:='';
  WindowTitle:='Editing new file';
end;

procedure TMainForm.LoadFile(COnst AFileName :String);

Var
  L : TStrings;

begin
  FFileName:=AFileName;
  L:=TstringList.Create;
  try
    L.LoadFromFile(AFileName);
    fedit.RichText:=l.text;
  finally
    L.Free;
  end;
  WindowTitle:='Editing '+AFileName;
end;

procedure TMainForm.SaveFile(COnst AFileName :String);

Var
  L : TStrings;

begin
  FFileName:=AFileName;
  L:=TstringList.Create;
  try
    l.text:=fedit.RichText;
    L.SaveToFile(AFileName);
  finally
    L.Free;
  end;
  WindowTitle:='Editing '+AFileName;
end;


procedure TMainForm.DoOpenFile(Sender: TObject);

Var
  FN : String;

begin
  FN:=SelectFileDialog(sfdOpen,'text files|*.txt|All files|'+AllFilesMask,'');
  if (FN<>'') then
    LoadFile(FN);
end;

procedure TMainForm.DoQuit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.DoSaveFile(Sender: TObject);
Var
  FN : String;

begin
  FN:=SelectFileDialog(sfdSave,'text files|*.txt|All files|'+AllFilesMask,'');
  if (FN<>'') then
    SaveFile(FN);
end;

procedure TMainForm.AfterCreate;
var
  I, J: integer;
  img: tfpgimage;
  S: string;
begin
  FMenu:=TfpgMenuBar.Create(Self);
  FMenu.SetPosition(0,0,Self.width,30);
  FMenu.align:=alTop;
  MFile:=FMenu.AddMenuItem('File',Nil);
  MFile.SubMenu:=TfpgPopupMenu.Create(MFile);
  MNew:=MFile.SubMenu.AddMenuItem('&New','ctrl-n',@DoNewFile);
  MOpen:=MFile.SubMenu.AddMenuItem('&Open','ctrl-o',@DoOpenFile);
  MSave:=MFile.SubMenu.AddMenuItem('&Save','ctrl-s',@DoSaveFile);
  MQuit:=MFile.SubMenu.AddMenuItem('&Quit','ctrl-q',@DoQuit);
  Fedit:=TRichTextEditFrame.Create(Self);
  Fedit.SetPosition(0,FMenu.Height,Self.width,Self.Height-FMenu.Height);
  Fedit.align:=alClient;
  {%region 'Auto-generated GUI code' }

  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(496, 295, 739, 502);
  WindowTitle := 'Editing new file';
  Hint := '';

  {@VFD_BODY_END: MainForm}
  {%endregion}
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
    if (ParamCount=1) and FileExists(ParamStr(1)) then
      frm.LoadFile(Paramstr(1));
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.

