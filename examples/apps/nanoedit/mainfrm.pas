unit mainfrm;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main,
  fpg_form, fpg_button, fpg_menu, fpg_textedit, fpg_panel,
  fpg_label, simpleipc;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainFrom}
    menu: TfpgMenuBar;
    mnuFile: TfpgPopupMenu;
    mnuEdit: TfpgPopupMenu;
    mnuSearch: TfpgPopupMenu;
    memEditor: TfpgTextEdit;
    btnGO: TfpgButton;
    bevStatusBar: TfpgBevel;
    lblStatusText: TfpgLabel;
    {@VFD_HEAD_END: MainFrom}
    FTextToFind: TfpgString;
    FFindOptions: TfpgFindOptions;
    FIsForward: boolean;
    FFilename: TfpgString;
    FIPCServer: TSimpleIPCServer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miNewClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miGoToLineClick(Sender: TObject);
    procedure miFindClick(Sender: TObject);
    procedure miFindNextClick(Sender: TObject);
    procedure miQuitClick(Sender: TObject);
    procedure miCutClicked(Sender: TObject);
    procedure miCopyClicked(Sender: TObject);
    procedure miPasteClicked(Sender: TObject);
    procedure btnGOClick(Sender: TObject);
    procedure memEditorChanged(Sender: TObject);
    procedure UpdateStatus(const AMessage: TfpgString);
    procedure StartIPCServer;
    procedure CheckIPCMessages(Sender: TObject);
    procedure IPCMessageReceived;
    procedure LoadFile(const AFileName: TfpgString);
    procedure Bevel1DragEnter(Sender: TfpgDrop);
    procedure Bevel1DragLeave(Sender: TfpgDrop);
    procedure PanelDragDrop(Sender: TfpgDrop; AData: variant);
  public
    destructor Destroy; override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  elastictabstops,
  fpg_dialogs,
  frm_find;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  StartIPCServer;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  s: string;
begin
  if ParamCount > 0 then
  begin
//    ShowMessage(ParamStr(1));
    s := ParamStr(1);
    if Pos('file://', s) > 0 then
      s := StringReplace(s, 'file://', '', []);
    LoadFile(s);
  end;
end;

procedure TMainForm.miNewClick(Sender: TObject);
begin
  FFileName := '';
  memEditor.Clear;
  UpdateStatus('New file completed.');
end;

procedure TMainForm.miOpenClick(Sender: TObject);
var
  dlg: TfpgFileDialog;
begin
  dlg := TfpgFileDialog.Create(nil);
  try
    if dlg.RunOpenFile then
    begin
      LoadFile(dlg.FileName);
    end;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.miSaveClick(Sender: TObject);
begin
  if FFilename = '' then
  begin
    miSaveAsClick(nil);
  end
  else
  begin
    memEditor.SaveToFile(FFileName);
    UpdateStatus(Format('<%s> successfully saved.', [FFileName]));
  end;
end;

procedure TMainForm.miSaveAsClick(Sender: TObject);
var
  dlg: TfpgFileDialog;
begin
  dlg := TfpgFileDialog.Create(nil);
  try
    if FFilename <> '' then
      dlg.FileName := FFilename;
    if dlg.RunSaveFile then
    begin
      memEditor.Lines.SaveToFile(dlg.FileName);
      FFilename := dlg.FileName;
      UpdateStatus(Format('<%s> successfully saved.', [FFileName]));
    end;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.miGoToLineClick(Sender: TObject);
var
  sValue: string;
  i: integer;
begin
  if fpgInputQuery('Go to line', 'Go to line number?', sValue) then
  begin
    try
      i := StrToInt(sValue);
      memEditor.GotoLine(i);
    except
      on E: Exception do
         ShowMessage('Invalid line number.' + LineEnding + E.Message);
    end;
  end;
end;

procedure TMainForm.miFindClick(Sender: TObject);
var
  dlg: TFindForm;
begin
  FTextToFind := '';
  dlg := TFindForm.Create(nil);
  try
    if dlg.Execute then
    begin
      FTextToFind := dlg.TextToFind;
      FFindOptions := dlg.FindOptions;
      FIsForward := dlg.IsForward;
      miFindNextClick(nil);
    end;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.miFindNextClick(Sender: TObject);
begin
  if FTextToFind <> '' then
    memEditor.FindText(FTextToFind, FFindOptions, FIsForward);
end;

procedure TMainForm.miQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.miCutClicked(Sender: TObject);
begin
  memEditor.CutToClipboard;
end;

procedure TMainForm.miCopyClicked(Sender: TObject);
begin
  memEditor.CopyToClipboard;
end;

procedure TMainForm.miPasteClicked(Sender: TObject);
begin
  memEditor.PasteFromClipboard;
end;

procedure TMainForm.btnGOClick(Sender: TObject);
var
  ftr: TElasticTabstopsDocFilter;
begin
{
  ftr := TElasticTabstopsDocFilter.Create(memEditor);
  try
    try
      ftr.StretchTabstops;
    finally
      ftr.Free;
    end;
  except
    on E: exception do
      ShowMessage(e.Message);
  end;
}
  memEditor.HasOwnWindow:=not memEditor.HasOwnWindow;
end;

procedure TMainForm.memEditorChanged(Sender: TObject);
var
  ftr: TElasticTabstopsDocFilter;
begin
{
  ftr := TElasticTabstopsDocFilter.Create(memEditor);
  try
    try
      ftr.StretchTabstops;
    finally
      ftr.Free;
    end;
  except
    on E: exception do
      ShowMessage(e.Message);
  end;
}
end;

procedure TMainForm.UpdateStatus(const AMessage: TfpgString);
begin
  lblStatusText.Text := AMessage;
end;

procedure TMainForm.StartIPCServer;
begin
  FIPCServer := TSimpleIPCServer.Create(self);
  FIPCServer.ServerID := 'nanoedit';
  FIPCServer.Global := True;
  FIPCServer.StartServer;
  fpgApplication.OnIdle := @CheckIPCMessages;
end;

procedure TMainForm.CheckIPCMessages(Sender: TObject);
begin
  while FIPCServer.PeekMessage(1, True) do
    IPCMessageReceived;
end;

procedure TMainForm.IPCMessageReceived;
begin
  case FIPCServer.MsgType of
    0:
      begin
        BringToFront;
      end;
    1:
      begin
        LoadFile(FIPCServer.StringMessage);
        BringToFront;
      end;
  end;
end;

procedure TMainForm.LoadFile(const AFileName: TfpgString);
begin
  memEditor.LoadFromFile(AFilename);
  FFilename := AFileName;
  UpdateStatus(AFilename);
end;

destructor TMainForm.Destroy;
begin
  fpgApplication.OnIdle := nil;
  FIPCServer.StopServer;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainFrom}
  Name := 'MainFrom';
  SetPosition(326, 207, 717, 410);
  WindowTitle := 'fpGUI nanoedit';
  Hint := '';
  WindowPosition := wpScreenCenter;
  OnCreate := @FormCreate;
  OnShow := @FormShow;

  menu := TfpgMenuBar.Create(self);
  with menu do
  begin
    Name := 'menu';
    SetPosition(0, 0, 717, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  mnuFile := TfpgPopupMenu.Create(self);
  with mnuFile do
  begin
    Name := 'mnuFile';
    SetPosition(340, 6, 120, 20);
    AddMenuItem('New', 'Ctrl+N', @miNewClick);
    AddMenuItem('Open...', 'Ctrl+O', @miOpenClick);
    AddMenuItem('Save', 'Ctrl+S', @miSaveClick);
    AddMenuItem('Save as...', 'Ctrl+Shift+S', @miSaveAsClick);
    AddMenuItem('-', '', nil);
    AddMenuItem('Quit', 'Ctrl+Q', @miQuitClick);
  end;

  mnuEdit := TfpgPopupMenu.Create(self);
  with mnuEdit do
  begin
    Name := 'mnuEdit';
    SetPosition(340, 44, 120, 20);
    AddMenuItem('Cut', 'Ctrl+X', @miCutClicked);
    AddMenuItem('Copy', 'Ctrl+C', @miCopyClicked);
    AddMenuItem('Paste', 'Ctrl+V', @miPasteClicked);
  end;

  mnuSearch := TfpgPopupMenu.Create(self);
  with mnuSearch do
  begin
    Name := 'mnuSearch';
    SetPosition(340, 25, 120, 20);
    AddMenuItem('Find...', 'Ctrl+F', @miFindClick);
    AddMenuItem('Find next', 'F3', @miFindNextClick);
    AddMenuItem('Replace', 'Ctrl+R', nil).Enabled := False;
    AddMenuitem('-', '', nil);
    AddMenuItem('Go to line...', 'Ctrl+G', @miGoToLineClick);
  end;

  memEditor := TfpgTextEdit.Create(self);
  with memEditor do
  begin
    Name := 'memEditor';
    SetPosition(0, 52, 717, 332);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#edit2';
    GutterVisible := True;
    RightEdge := True;
    DropHandler := TfpgDropEventHandler.Create(@Bevel1DragEnter, @Bevel1DragLeave, @PanelDragDrop, nil);
  end;

  btnGO := TfpgButton.Create(self);
  with btnGO do
  begin
    Name := 'btnGO';
    SetPosition(260, 26, 75, 24);
    Text := 'GO';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
    OnClick := @btnGOClick;
  end;

  bevStatusBar := TfpgBevel.Create(self);
  with bevStatusBar do
  begin
    Name := 'bevStatusBar';
    SetPosition(1, 387, 715, 22);
    Anchors := [anLeft,anRight,anBottom];
    Hint := '';
    Style := bsLowered;
  end;

  lblStatusText := TfpgLabel.Create(bevStatusBar);
  with lblStatusText do
  begin
    Name := 'lblStatusText';
    SetPosition(4, 4, 704, 15);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    Text := '';
  end;

  {@VFD_BODY_END: MainFrom}

  menu.AddMenuItem('&File', nil).SubMenu := mnuFile;
  menu.AddMenuItem('&Edit', nil).SubMenu := mnuEdit;
  menu.AddMenuItem('&Search', nil).SubMenu := mnuSearch;
end;

procedure TMainForm.Bevel1DragEnter(Sender: TfpgDrop);
var
  s: string;
begin
//  ShowMimeList(Sender.Mimetypes);
  { the mime type we want to accept }
  s := 'text/uri-list';
  { if we will accept the drop, set CanDrop to True }
  Sender.CanDrop := Sender.AcceptMimeType([s]);
  if Sender.CanDrop then
  begin
//    Bevel1.BackgroundColor := clRed;
  end;
end;

procedure TMainForm.Bevel1DragLeave(Sender: TfpgDrop);
begin
//  Bevel1.BackgroundColor := clWindowBackground;
end;

procedure TMainForm.PanelDragDrop(Sender: TfpgDrop; AData: variant);
var
  s: string;
begin
  { TODO: handle multiple files in a single drop. }
  s := AData;
  if Pos('file:///', s) < 1 then
  begin
    exit
  end
  else
  begin
    s := StringReplace(s, 'file://', '', [rfReplaceAll]);
    s := StringReplace(s, #10, '', [rfReplaceAll]);
    s := StringReplace(s, #13, '', [rfReplaceAll]);
    writeln('<'+s+'>');
  end;
  LoadFile(s);
end;



end.
