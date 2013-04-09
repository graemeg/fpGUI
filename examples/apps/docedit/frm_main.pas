unit frm_main;

{$mode objfpc}{$H+}

{
  NOTE:  This is still work in progress!!!!!!!
}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_button,
  fpg_edit,
  fpg_label,
  fpg_menu,
  fpg_memo,
  fpg_tree,
  fpg_panel,
  dom, XMLWrite, XMLRead, contnrs, model;

type
  TMainForm = class(TfpgForm)
  private
    FFile: TDescDocument;
    FDescriptionNode: TDomNode;
    FDoc: TXMLDocument;
    FModified: Boolean;
    CurrentModule: TDomElement;
    FModuleTree: TObjectList;
    FElementTree: TObjectList;
    procedure   btnQuitClicked(Sender: TObject);
    procedure   miFileQuitClicked(Sender: TObject);
    procedure   miFileOpenClicked(Sender: TObject);
    procedure   miFileSaveAsClicked(Sender: TObject);
    procedure   miHelpAboutClicked(Sender: TObject);
    procedure   miExtraOptionsClicked(Sender: TObject);
    procedure   GetElementList(List: TStrings);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    menubar: TfpgMenuBar;
    miFile: TfpgPopupMenu;
    miInsert: TfpgPopupMenu;
    miExtra: TfpgPopupMenu;
    miHelp: TfpgPopupMenu;
    btnQuit: TfpgButton;
    tvXML: TfpgTreeView;
    pnlName1: TfpgBevel;
    lblFilename: TfpgLabel;
    {@VFD_HEAD_END: MainForm}
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
    property    DescriptionNode: TDomNode read FDescriptionNode write FDescriptionNode;
  end;
  
{@VFD_NEWFORM_DECL}

implementation

uses
  fpg_dialogs,
  fpg_constants,
  frm_options;
  
  
const
  cAppName = 'FPDoc Documentation Editor';
  
  
type
  TNodeTreeItem = class(TObjectList)
  private
    FNode: TDomElement;
  public
    constructor Create(ANode: TDomElement); virtual;
    procedure   CheckSubItems;
    property    Node: TDomElement Read FNode;
  end;

  TModuleTreeItem   = class(TNodeTreeItem);
  TTopicTreeItem    = class(TNodeTreeItem);
  TPackageTreeItem  = class(TNodeTreeItem);
  
{@VFD_NEWFORM_IMPL}

{ TNodeTreeItem }

constructor TNodeTreeItem.Create(ANode: TDomElement);
begin
  inherited Create(false);
  FNode := ANode;
end;

procedure TNodeTreeItem.CheckSubItems;
begin
//  If (SubTree=Nil) then
//    SubTree:=TFPgtkTree.Create;
end;

{ TMainForm }

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
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
    begin
      lblFilename.Text := dlg.FileName;
      FFile := TDescDocument.Create(dlg.Filename);
//      ProcessXMLFile(dlg.FileName);
    end
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
    dlg.FileName := lblFilename.Text;
    if dlg.RunSaveFile then
      lblFilename.Text := dlg.FileName;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.miHelpAboutClicked(Sender: TObject);
begin
  ShowMessage(cAppName
      + #10
      + #10 + 'Written by Graeme Geldenhuys - 2007'
      + #10 + 'Using the ' + fpGUIName + ' v' + fpGUI_Version
      ,'About');
end;

procedure TMainForm.miExtraOptionsClicked(Sender: TObject);
var
  frm: TfrmOptions;
begin
  frm := TfrmOptions.Create(nil);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TMainForm.GetElementList(List: TStrings);
var
  N: TDOMNode;
begin
 with List do
 begin
   Clear;
   if Assigned(CurrentModule) then
   begin
     N := Currentmodule.FirstChild;
     while (N <> Nil) do
     begin
       if (N is TDomElement) and (N.NodeName = 'element') then
         Add(TDomElement(N)['name']);
       N := N.NextSibling;
     end;
   end;
 end;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := cAppName;
  Sizeable := False;

  FModuleTree := TObjectList.Create;
  FElementTree := TObjectList.Create;
end;

destructor TMainForm.Destroy;
begin
  FModuleTree.Free;
  FElementTree.Free;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(346, 279, 650, 402);
  WindowTitle := 'fpGUI Documentation Editor';
  WindowPosition := wpScreenCenter;

  menubar := TfpgMenuBar.Create(self);
  with menubar do
  begin
    Name := 'menubar';
    SetPosition(0, 0, 650, 23);
    Anchors := [anLeft,anRight,anTop];
  end;

  miFile := TfpgPopupMenu.Create(self);
  with miFile do
  begin
    Name := 'miFile';
    SetPosition(464, 169, 160, 24);
    AddMenuItem('&New...', 'Ctrl-N', nil);
    AddMenuItem('&Open..', 'Ctrl-O', @miFileOpenClicked);
    AddMenuItem('&Save', 'Ctrl-S', nil);
    AddMenuItem('S&ave As..', 'Ctrl+Shift+S', @miFileSaveAsClicked);
    AddMenuItem('-', '', nil);
    AddMenuItem('&Close', 'Ctrl-W', nil);
    AddMenuItem('&Recent', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('&Quit', 'Ctrl-Q', @miFileQuitClicked);
  end;

  miInsert := TfpgPopupMenu.Create(self);
  with miInsert do
  begin
    Name := 'miInsert';
    SetPosition(464, 190, 160, 24);
    AddMenuItem('&Package', '', nil);
    AddMenuItem('&Module', '', nil);
    AddMenuItem('&Topic', '', nil);
    AddMenuItem('&Element', '', nil);
    AddMenuItem('&Link', '', nil);
    AddMenuItem('T&able', '', nil);
    AddMenuItem('&Short Desc Link', '', nil);
  end;

  miExtra := TfpgPopupMenu.Create(self);
  with miExtra do
  begin
    Name := 'miExtra';
    SetPosition(464, 211, 160, 24);
    AddMenuItem('&Options...', '', @miExtraOptionsClicked);
    AddMenuItem('&Build...', '', nil);
  end;

  miHelp := TfpgPopupMenu.Create(self);
  with miHelp do
  begin
    Name := 'miHelp';
    SetPosition(464, 232, 160, 24);
    AddMenuItem('About...', '', @miHelpAboutClicked);
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(566, 350, 75, 23);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    ImageName := 'stdimg.Quit';
    ShowImage := True;
    OnClick   := @btnQuitClicked;
  end;

  tvXML := TfpgTreeView.Create(self);
  with tvXML do
  begin
    Name := 'tvXML';
    SetPosition(4, 28, 226, 340);
    FontDesc := '#Label1';
    DefaultColumnWidth := 15;
    TreeLineStyle := lsDot;
    ScrollWheelDelta := 15;
  end;

  pnlName1 := TfpgBevel.Create(self);
  with pnlName1 do
  begin
    Name := 'pnlName1';
    SetPosition(0, 381, 650, 20);
    Anchors := [anLeft,anRight,anBottom];
    Shape := bsBox;
    Style := bsLowered;
    Focusable := False;
  end;

  lblFilename := TfpgLabel.Create(pnlName1);
  with lblFilename do
  begin
    Name := 'lblFilename';
    SetPosition(6, 2, 636, 16);
    Anchors := [anLeft,anRight,anTop];
    Text := '<filename>';
    FontDesc := '#Label1';
  end;

  {@VFD_BODY_END: MainForm}

  menubar.AddMenuItem('&File', nil).SubMenu     := miFile;
  menubar.AddMenuItem('&Insert', nil).SubMenu   := miInsert;
  menubar.AddMenuItem('&Extra', nil).SubMenu    := miExtra;
  menubar.AddMenuItem('&Help', nil).SubMenu     := miHelp;
end;

end.

