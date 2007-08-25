unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpgfx,
  gui_form,
  gui_button,
  gui_edit,
  gui_label,
  gui_menu;

type
  TMainForm = class(TfpgForm)
  private
    btnQuit: TfpgButton;
    lblXMLFile: TfpgLabel;
    edXMLFile: TfpgEdit;
    btnOpen: TfpgButton;
    menubar: TfpgMenuBar;
    miFile: TfpgPopupMenu;
    miInsert: TfpgPopupMenu;
    miExtra: TfpgPopupMenu;
    miHelp: TfpgPopupMenu;
    procedure   btnQuitClicked(Sender: TObject);
    procedure   btnOpenClicked(Sender: TObject);
    procedure   InitializeComponents;
    procedure   SetupMenuBar;
    procedure   miFileQuitClicked(Sender: TObject);
    procedure   miFileOpenClicked(Sender: TObject);
    procedure   miFileSaveAsClicked(Sender: TObject);
    procedure   miHelpAboutClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;
  

implementation

uses
  gui_dialogs;
  
  
const
  cAppName = 'FPDoc Documentation Editor';

{ TMainForm }

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnOpenClicked(Sender: TObject);
begin
  // open xml file
end;

procedure TMainForm.InitializeComponents;
begin
  SetupMenuBar;
  
  btnQuit := CreateButton(self, Width-88, Height-31, 80, 'Quit', @btnQuitClicked);
  with btnQuit do
  begin
    ImageName := 'stdimg.Quit';
    ShowImage := True;
    Anchors   := [anRight, anBottom];
  end;
  
  lblXMLFile := CreateLabel(self, 8, 38, 'XML File:');
  edXMLFile  := CreateEdit(self, lblXMLFile.Right+8, lblXMLFile.Top+36, 485, 23);
  edXMLFile.Text := '';
  
  btnOpen := CreateButton(self, edXMLFile.Right+10, edXMLFile.Top, 80, 'Open', @btnOpenClicked);
  with btnOpen do
  begin
    ImageName := 'stdimg.Open';
    ShowImage := True;
  end;
end;

procedure TMainForm.SetupMenuBar;
begin
  // create top level menus.
  miFile := TfpgPopupMenu.Create(self);
  miFile.AddMenuItem('&New...', 'Ctrl-N', nil);
  miFile.AddMenuItem('&Open..', 'Ctrl-O', @miFileOpenClicked);
  miFile.AddMenuItem('&Save', 'Ctrl-S', nil);
  miFile.AddMenuItem('S&ave As..', 'Ctrl+Shift+S', @miFileSaveAsClicked);
  miFile.AddMenuItem('-', '', nil);
  miFile.AddMenuItem('&Close', 'Ctrl-W', nil);
  miFile.AddMenuItem('&Recent', '', nil);
  miFile.AddMenuItem('-', '', nil);
  miFile.AddMenuItem('&Quit', 'Ctrl-Q', @miFileQuitClicked);

  miInsert := TfpgPopupMenu.Create(self);
  miInsert.AddMenuItem('&Package', '', nil);
  miInsert.AddMenuItem('&Module', '', nil);
  miInsert.AddMenuItem('&Topic', '', nil);
  miInsert.AddMenuItem('&Element', '', nil);
  miInsert.AddMenuItem('&Link', '', nil);
  miInsert.AddMenuItem('T&able', '', nil);
  miInsert.AddMenuItem('&Short Desc Link', '', nil);

  miExtra := TfpgPopupMenu.Create(self);
  miExtra.AddMenuItem('&Options...', '', nil);
  miExtra.AddMenuItem('&Build...', '', nil);

  miHelp := TfpgPopupMenu.Create(self);
  miHelp.AddMenuItem('About...', '', @miHelpAboutClicked);

  // create main menu bar
  menubar := TfpgMenuBar.Create(self);
  menubar.SetPosition(0, 0, Width, menubar.Height);
  menubar.Anchors := [anLeft, anTop, anRight];
  menubar.AddMenuItem('&File', nil).SubMenu     := miFile;
  menubar.AddMenuItem('&Insert', nil).SubMenu   := miInsert;
  menubar.AddMenuItem('&Extra', nil).SubMenu    := miExtra;
  menubar.AddMenuItem('&Help', nil).SubMenu     := miHelp;
end;

procedure TMainForm.miFileQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.miFileOpenClicked(Sender: TObject);
var
  dlg: TfpgFileDialog;
begin
  dlg := TfpgFileDialog.Create(nil);
  try
    dlg.Filter := 'FPDoc Desc Files (*.xml)|*.xml|All Files (*)|*';
    if dlg.RunOpenFile then
      edXMLFile.Text := dlg.FileName;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.miFileSaveAsClicked(Sender: TObject);
var
  dlg: TfpgFileDialog;
begin
  dlg := TfpgFileDialog.Create(nil);
  try
    dlg.Filter := 'FPDoc Desc Files (*.xml)|*.xml|All Files (*)|*';
    dlg.FileName := edXMLFile.Text;
    if dlg.RunSaveFile then
      edXMLFile.Text := dlg.FileName;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.miHelpAboutClicked(Sender: TObject);
begin
  ShowMessage(cAppName
      + #10
      + #10 + 'Written by Graeme Geldenhuys - 2007'
      + #10 + 'Using the fpGUI toolkit'
      ,'About');
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := cAppName;
  Sizeable := False;
  // Golden ratio 1.618
  Width   := 650;
  Height  := 402;

  InitializeComponents;
end;

end.

