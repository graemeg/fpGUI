unit mainfrm;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main,
  fpg_form, fpg_button, fpg_menu, fpg_textedit, fpg_panel,
  fpg_label, fpg_tab, simpleipc;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainFrom}
    menu: TfpgMenuBar;
    mnuFile: TfpgPopupMenu;
    mnuEdit: TfpgPopupMenu;
    mnuSearch: TfpgPopupMenu;
    btnGO: TfpgButton;
    bevStatusBar: TfpgBevel;
    lblStatusText: TfpgLabel;
    pcEditor: TfpgPageControl;
    TabSheet1: TfpgTabSheet;
    memEditor: TfpgTextEdit;
    {@VFD_HEAD_END: MainFrom}
    FTextToFind: TfpgString;
    FFindOptions: TfpgFindOptions;
    FIsForward: boolean;
    FIPCServer: TSimpleIPCServer;
    procedure   FormCreate(Sender: TObject);
    procedure   FormShow(Sender: TObject);
    procedure   miNewClick(Sender: TObject);
    procedure   miOpenClick(Sender: TObject);
    procedure   miSaveClick(Sender: TObject);
    procedure   miSaveAsClick(Sender: TObject);
    procedure   miGoToLineClick(Sender: TObject);
    procedure   miFindClick(Sender: TObject);
    procedure   miFindNextClick(Sender: TObject);
    procedure   miQuitClick(Sender: TObject);
    procedure   miCutClicked(Sender: TObject);
    procedure   miCopyClicked(Sender: TObject);
    procedure   miPasteClicked(Sender: TObject);
    procedure   btnGOClick(Sender: TObject);
    procedure   memEditorChanged(Sender: TObject);
    procedure   UpdateStatus(const AMessage: TfpgString);
    procedure   StartIPCServer;
    procedure   CheckIPCMessages(Sender: TObject);
    procedure   IPCMessageReceived;
    procedure   LoadFile(const AFileName: TfpgString);
    procedure   Bevel1DragEnter(Sender: TfpgDrop);
    procedure   Bevel1DragLeave(Sender: TfpgDrop);
    procedure   PanelDragDrop(Sender: TfpgDrop; AData: variant);
    function    CreateNewEditorTab(const ATitle: TfpgString): TfpgTabSheet;
    function    OpenEditorPage(const AFilename: TfpgString): TfpgTabSheet;
    function    GetEditorFromTabIndex(const AIndex: integer): TfpgTextEdit;
    function    ActiveEditor: TfpgTextEdit;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  elastictabstops,
  fpg_dialogs,
  fpg_utils,
  frm_find;

{$I images.inc}

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
  OpenEditorPage('Untitled');
//  memEditor.Clear;
//  UpdateStatus('New file completed.');
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
  if pcEditor.ActivePage.Hint = '' then
  begin
    miSaveAsClick(nil);
  end
  else
  begin
    ActiveEditor.SaveToFile(pcEditor.ActivePage.Hint);
    UpdateStatus(Format('<%s> successfully saved.', [pcEditor.ActivePage.Hint]));
  end;
end;

procedure TMainForm.miSaveAsClick(Sender: TObject);
var
  dlg: TfpgFileDialog;
begin
  dlg := TfpgFileDialog.Create(nil);
  try
    if pcEditor.ActivePage.Hint <> '' then
      dlg.FileName := pcEditor.ActivePage.Hint;
    if dlg.RunSaveFile then
    begin
      ActiveEditor.SaveToFile(dlg.FileName);
      pcEditor.ActivePage.Hint := dlg.FileName;
      UpdateStatus(Format('<%s> successfully saved.', [pcEditor.ActivePage.Hint]));
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
      if i < 1 then
        Exit;
      ActiveEditor.GotoLine(i-1);
      ActiveEditor.SetFocus;
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
    ActiveEditor.FindText(FTextToFind, FFindOptions, FIsForward);
end;

procedure TMainForm.miQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.miCutClicked(Sender: TObject);
begin
  ActiveEditor.CutToClipboard;
end;

procedure TMainForm.miCopyClicked(Sender: TObject);
begin
  ActiveEditor.CopyToClipboard;
end;

procedure TMainForm.miPasteClicked(Sender: TObject);
begin
  ActiveEditor.PasteFromClipboard;
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
  OpenEditorPage(AFileName);
//  memEditor.LoadFromFile(AFilename);
//  FFilename := AFileName;
//  UpdateStatus(AFilename);
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
  IconName := 'mainicon';
  ShowHint := True;
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

  btnGO := TfpgButton.Create(self);
  with btnGO do
  begin
    Name := 'btnGO';
    SetPosition(260, 26, 75, 24);
    Text := 'ET';
    FontDesc := '#Label1';
    Hint := 'Used for testing Elastic Tabstops implementation.';
    ImageName := '';
    TabOrder := 4;
    Enabled := False;
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

  pcEditor := TfpgPageControl.Create(self);
  with pcEditor do
  begin
    Name := 'pcEditor';
    SetPosition(0, 52, 717, 284);
    Hint := '';
    ParentShowHint := False;
    ShowHint := True;
    TabOrder := 9;
    Options := Options + [to_PMenuClose];
  end;

  TabSheet1 := TfpgTabSheet.Create(pcEditor);
  with TabSheet1 do
  begin
    Name := 'TabSheet1';
    SetPosition(3, 24, 711, 257);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := 'Untitled';
    ShowHint := True;
  end;

  memEditor := TfpgTextEdit.Create(TabSheet1);
  with memEditor do
  begin
    Name := 'memEditor';
    SetPosition(0, 0, 710, 256);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#edit2';
    GutterVisible := True;
    RightEdge := True;
    DropHandler := TfpgDropEventHandler.Create(@Bevel1DragEnter, @Bevel1DragLeave, @PanelDragDrop, nil);
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

function TMainForm.CreateNewEditorTab(const ATitle: TfpgString): TfpgTabSheet;
var
  m: TfpgTextEdit;
begin
  Result := pcEditor.AppendTabSheet(ATitle);
  m := TfpgTextEdit.Create(Result);
  m.SetPosition(1, 1, 200, 20);
  m.Align := alClient;
  m.FontDesc := '#edit2';
  m.GutterVisible := True;
  m.GutterShowLineNumbers := True;
  m.RightEdge := True;
  m.ShowHint := True;
  m.DropHandler := TfpgDropEventHandler.Create(@Bevel1DragEnter, @Bevel1DragLeave, @PanelDragDrop, nil);
end;

function TMainForm.OpenEditorPage(const AFilename: TfpgString): TfpgTabSheet;
var
  s: TfpgString;
  f: TfpgString;
  i: integer;
  found: Boolean;
  ts: TfpgTabSheet;
//  ext: TfpgString;
//  pos_h: integer;
//  pos_v: integer;
//  cur_pos_h: integer;
//  cur_pos_v: integer;
  editor: TfpgTextEdit;
begin
  s := AFilename;
  f := fpgExtractFileName(s);
  found := False;
  for i := 0 to pcEditor.PageCount-1 do
  begin
    if pcEditor.Pages[i].Text = f then
      found := True;
    if found then
      break;
  end;
  if found then
  begin
    // reuse existing tab
    editor := TfpgTextEdit(pcEditor.Pages[i].Components[0]);
//    pos_h := editor.ScrollPos_H;
//    pos_v := editor.ScrollPos_V;
//    cur_pos_h := editor.CaretPos_H;
//    cur_pos_v := editor.CaretPos_V;
    editor.Lines.BeginUpdate;
    editor.LoadFromFile(s);
//    editor.ScrollPos_H := pos_h;
//    editor.ScrollPos_V := pos_v;
//    editor.CaretPos_H := cur_pos_h;
//    editor.CaretPos_V := cur_pos_v;
//    editor.UpdateScrollBars;
    editor.Lines.EndUpdate;
    pcEditor.ActivePageIndex := i;
    ts := pcEditor.ActivePage;
    UpdateStatus('File reloaded: ' + s);
  end
  else
  begin
    // we need a new tabsheet
    ts := CreateNewEditorTab(f);
    editor := ts.Components[0] as TfpgTextEdit;
    editor.Lines.BeginUpdate;
    if fpgFileExists(s) then
    begin
      editor.Lines.LoadFromFile(s);
      UpdateStatus('Loaded: ' + s);
    end;
    editor.Lines.EndUpdate;
//    if gINI.ReadBool(cEditor, 'SyntaxHighlighting', True) then
//    begin
//      ext := fpgExtractFileExt(AFilename);
//      if (ext = '.pas') or (ext = '.pp') or (ext = '.inc') or (ext = '.lpr') or (ext = '.dpr') then
//      begin
//        TfpgTextEdit(ts.Components[0]).OnDrawLine := @HighlightObjectPascal;
//      end
//      else if (ext = '.patch') or (ext = '.diff') then
//      begin
//        TfpgTextEdit(ts.Components[0]).OnDrawLine := @HighlightPatch;
//      end;
//    end;
    ts.Realign;
    ts.Hint := s;
    pcEditor.ActivePage := ts;
//    FFileMonitor.AddFile(AFilename);
  end;
  Result := ts;
end;

function TMainForm.GetEditorFromTabIndex(const AIndex: integer): TfpgTextEdit;
begin
  if AIndex > (pcEditor.PageCount-1) then
    raise Exception.Create('Tab Index is greater than the number of tabs.');
  Result := pcEditor.Pages[AIndex].Components[0] as TfpgTextEdit;
end;

function TMainForm.ActiveEditor: TfpgTextEdit;
begin
  Result := GetEditorFromTabIndex(pcEditor.ActivePageIndex);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fpgImages.AddBMP(
    'mainicon', @nanoedit_icon, sizeof(nanoedit_icon));
end;



end.
