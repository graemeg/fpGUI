unit mainfrm;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main,
  fpg_form, fpg_button, fpg_menu, fpg_textedit, fpg_panel,
  fpg_label;

type

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
    procedure FormShow(Sender: TObject);
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
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  elastictabstops,
  fpg_dialogs,
  frm_find;

{@VFD_NEWFORM_IMPL}

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
    memEditor.LoadFromFile(s);
    FFilename := s;
    lblStatusText.Text := FFilename;
  end;
end;

procedure TMainForm.miOpenClick(Sender: TObject);
var
  dlg: TfpgFileDialog;
begin
  dlg := TfpgFileDialog.Create(nil);
  try
    if dlg.RunOpenFile then
    begin
      memEditor.Lines.LoadFromFile(dlg.FileName);
      FFileName := dlg.FileName;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.miSaveClick(Sender: TObject);
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
    end;
  finally
    dlg.Free;
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
      lblStatusText.Text := FFilename;
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

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainFrom}
  Name := 'MainFrom';
  SetPosition(326, 207, 717, 410);
  WindowTitle := 'fpGUI nanoedit';
  Hint := '';
  WindowPosition := wpScreenCenter;
  OnShow  := @FormShow;

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


end.
