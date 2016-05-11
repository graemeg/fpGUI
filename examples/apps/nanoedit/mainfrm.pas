{
  Copyright (c) 2013-2016, Graeme Geldenhuys
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this
     list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  Description:
    The main form of the Nanoedit text editor.
}
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
    btnGO: TfpgButton;
    bevStatusBar: TfpgBevel;
    lblStatusText: TfpgLabel;
    pcEditor: TfpgPageControl;
    mnuFile: TfpgPopupMenu;
    mnuEdit: TfpgPopupMenu;
    mnuSearch: TfpgPopupMenu;
    mnuConvert: TfpgPopupMenu;
    mnuHelp: TfpgPopupMenu;
    mnuOptions: TfpgPopupMenu;
    {@VFD_HEAD_END: MainFrom}
    FTextToFind: TfpgString;
    FFindOptions: TfpgFindOptions;
    FIsForward: boolean;
    FIPCServer: TSimpleIPCServer;
    { don't use an existing instance of NanoEdit, and don't use IPC at all. }
    FNewInstance: boolean;
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
    procedure   miConvertB64Encode(Sender: TObject);
    procedure   miConvertB64Decode(Sender: TObject);
    procedure   HelpAboutFPGui(Sender: TObject);
    procedure   HelpProductInfo(Sender: TObject);
    procedure   miOptionsAutoIndentClicked(Sender: TObject);
    procedure   miOptionsLineNumbersClicked(Sender: TObject);
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
//  elastictabstops,
  fpg_dialogs,
  fpg_utils,
  fpg_cmdlineparams,
  frm_find,
  base64;


const
  cShortOpts = 'n';
  cDefaultLongOpts: array[1..1] of string = ('newinstance');

{$I images.inc}


{@VFD_NEWFORM_IMPL}

procedure TMainForm.FormCreate(Sender: TObject);
var
  CmdIntf: ICmdLineParams;
begin
  if Supports(fpgApplication, ICmdLineParams, CmdIntf) then
    FNewInstance := CmdIntf.HasOption('n', 'newinstance');
  if not FNewInstance then
    StartIPCServer;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  s: string;
  i: integer;
  CmdIntf: ICmdLineParams;
begin
  Supports(fpgApplication, ICmdLineParams, CmdIntf);
  if CmdIntf.ParamCount > 0 then
  begin
    for i := 1 to CmdIntf.ParamCount do
    begin
      writeln('Found: ', CmdIntf.Params[i]);
      if (CmdIntf.Params[i] <> '--newinstance') and (CmdIntf.Params[i] <> '-n') then
      begin
        s := CmdIntf.Params[i];
        writeln('using: ', s);
        Break;
      end;
    end;

    if s = '' then
    begin
      miNewClick(nil);
      Exit;
    end;

    if Pos('file://', s) > 0 then
      s := StringReplace(s, 'file://', '', []);
    LoadFile(s);
    ActiveEditor.SetFocus;
  end
  else
  begin
    if pcEditor.PageCount = 0 then
      miNewClick(nil);
  end;
end;

procedure TMainForm.miNewClick(Sender: TObject);
begin
  OpenEditorPage('Untitled');
  ActiveEditor.SetFocus;
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

procedure TMainForm.miConvertB64Encode(Sender: TObject);
var
  s: TfpgString;
begin
  s := ActiveEditor.GetSelectedText;
  try
    EncodeStringBase64(s);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
  ActiveEditor.DeleteSelection;
  ActiveEditor.InsertTextAtPos(s, ActiveEditor.CaretPos_H, ActiveEditor.CaretPos_V);
end;

procedure TMainForm.miConvertB64Decode(Sender: TObject);
var
  s: TfpgString;
begin
  DecodeStringBase64(s);
end;

procedure TMainForm.HelpAboutFPGui(Sender: TObject);
begin
  TfpgMessageDialog.AboutFPGUI();
end;

procedure TMainForm.HelpProductInfo(Sender: TObject);
begin
  TfpgMessageDialog.Information('Product Information',
    'fpGUI''s nanoedit text editor' + LineEnding + LineEnding +
    Format('Copyright (c) 2013-%d, Graeme Geldenhuys', [CurrentYear]) + LineEnding +
    'Nanoedit is distributed under the Simplified BSD License');
end;

procedure TMainForm.miOptionsAutoIndentClicked(Sender: TObject);
var
  i: integer;
  editor: TfpgTextEdit;
begin
  TfpgMenuItem(Sender).Checked := not TfpgMenuItem(Sender).Checked;
  for i := 0 to pcEditor.PageCount-1 do
  begin
    editor := TfpgTextEdit(pcEditor.Pages[i].Components[0]);
    editor.AutoIndent := TfpgMenuItem(Sender).Checked;
  end;
end;

procedure TMainForm.miOptionsLineNumbersClicked(Sender: TObject);
var
  i: integer;
  editor: TfpgTextEdit;
begin
  TfpgMenuItem(Sender).Checked := not TfpgMenuItem(Sender).Checked;
  for i := 0 to pcEditor.PageCount-1 do
  begin
    editor := TfpgTextEdit(pcEditor.Pages[i].Components[0]);
    editor.GutterVisible := TfpgMenuItem(Sender).Checked;
  end;
end;

procedure TMainForm.btnGOClick(Sender: TObject);
//var
//  ftr: TElasticTabstopsDocFilter;
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
//  memEditor.HasOwnWindow:=not memEditor.HasOwnWindow;
end;

procedure TMainForm.memEditorChanged(Sender: TObject);
//var
//  ftr: TElasticTabstopsDocFilter;
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

  btnGO := TfpgButton.Create(self);
  with btnGO do
  begin
    Name := 'btnGO';
    SetPosition(260, 26, 75, 24);
    Text := 'ET';
    Enabled := False;
    FontDesc := '#Label1';
    Hint := 'Used for testing Elastic Tabstops implementation.';
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

  pcEditor := TfpgPageControl.Create(self);
  with pcEditor do
  begin
    Name := 'pcEditor';
    SetPosition(0, 52, 717, 332);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    ParentShowHint := False;
    ShowHint := True;
    TabOrder := 9;
    Options := Options + [to_PMenuClose];
  end;

  mnuFile := TfpgPopupMenu.Create(self);
  with mnuFile do
  begin
    Name := 'mnuFile';
    SetPosition(348, 6, 120, 20);
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
    SetPosition(348, 25, 120, 20);
    AddMenuItem('Cut', 'Ctrl+X', @miCutClicked);
    AddMenuItem('Copy', 'Ctrl+C', @miCopyClicked);
    AddMenuItem('Paste', 'Ctrl+V', @miPasteClicked);
  end;

  mnuSearch := TfpgPopupMenu.Create(self);
  with mnuSearch do
  begin
    Name := 'mnuSearch';
    SetPosition(348, 44, 120, 20);
    AddMenuItem('Find...', 'Ctrl+F', @miFindClick);
    AddMenuItem('Find next', 'F3', @miFindNextClick);
    AddMenuItem('Replace', 'Ctrl+R', nil).Enabled := False;
    AddMenuitem('-', '', nil);
    AddMenuItem('Go to line...', 'Ctrl+G', @miGoToLineClick);
  end;

  mnuConvert := TfpgPopupMenu.Create(self);
  with mnuConvert do
  begin
    Name := 'mnuConvert';
    SetPosition(348, 63, 120, 20);
    AddMenuItem('Base64 Encode', '', @miConvertB64Encode);
    AddMenuItem('Base64 Decode', '', @miConvertB64Decode);
    AddMenuItem('Typography', '', nil).Enabled := False;
  end;

  mnuHelp := TfpgPopupMenu.Create(self);
  with mnuHelp do
  begin
    Name := 'mnuHelp';
    SetPosition(348, 82, 120, 20);
    AddMenuItem('About fpGUI Toolkit...', '', @HelpAboutFPGui);
    AddMenuItem('Product Information...', '', @HelpProductInfo);
  end;

  mnuOptions := TfpgPopupMenu.Create(self);
  with mnuOptions do
  begin
    Name := 'mnuOptions';
    SetPosition(348, 100, 120, 20);
    AddMenuItem('Auto Indent', '', @miOptionsAutoIndentClicked).Checked := True;
    AddMenuItem('Line Numbers', '', @miOptionsLineNumbersClicked).Checked := True;
  end;

  {@VFD_BODY_END: MainFrom}

  menu.AddMenuItem('&File', nil).SubMenu := mnuFile;
  menu.AddMenuItem('&Edit', nil).SubMenu := mnuEdit;
  menu.AddMenuItem('&Search', nil).SubMenu := mnuSearch;
  menu.AddMenuItem('&Convert', nil).SubMenu := mnuConvert;
  menu.AddMenuItem('&Options', nil).SubMenu := mnuOptions;
  menu.AddMenuItem('&Help', nil).SubMenu := mnuHelp;
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
  fpgImages.AddBMP('mainicon', @nanoedit_icon, sizeof(nanoedit_icon));
end;



end.
