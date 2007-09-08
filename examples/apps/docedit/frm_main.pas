unit frm_main;

{$mode objfpc}{$H+}

{
  NOTE:  This is still work in progress!!!!!!!
}

interface

uses
  Classes,
  SysUtils,
  fpgfx,
  gui_form,
  gui_button,
  gui_edit,
  gui_label,
  gui_menu,
  gui_memo,
  dom, XMLWrite, XMLRead, contnrs;

type
  TMainForm = class(TfpgForm)
  private
    FDescriptionNode: TDomNode;
    FDoc: TXMLDocument;
    FModified: Boolean;
    CurrentModule: TDomElement;
    FModuleTree: TObjectList;
    FElementTree: TObjectList;
    procedure   btnQuitClicked(Sender: TObject);
    procedure   btnOpenClicked(Sender: TObject);
    procedure   miFileQuitClicked(Sender: TObject);
    procedure   miFileOpenClicked(Sender: TObject);
    procedure   miFileSaveAsClicked(Sender: TObject);
    procedure   miHelpAboutClicked(Sender: TObject);
    procedure   miExtraOptionsClicked(Sender: TObject);
    procedure   ProcessXMLFile(pFilename: string);
    procedure   Refresh;
    procedure   GetElementList(List: TStrings);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    menubar: TfpgMenuBar;
    miFile: TfpgPopupMenu;
    miInsert: TfpgPopupMenu;
    miExtra: TfpgPopupMenu;
    miHelp: TfpgPopupMenu;
    btnQuit: TfpgButton;
    lblXMLFile: TfpgLabel;
    edXMLFile: TfpgEdit;
    btnOpen: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
    property    DescriptionNode: TDomNode read FDescriptionNode write FDescriptionNode;
  end;
  
{@VFD_NEWFORM_DECL}

implementation

uses
  gui_dialogs, frm_options;
  
  
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

procedure TMainForm.btnOpenClicked(Sender: TObject);
begin
  // open xml file
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
      ProcessXMLFile(dlg.FileName);
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

procedure TMainForm.ProcessXMLFile(pFilename: string);
var
  dn: TDOMNode;
  ls: TStringList;
begin
  ReadXMLFile(FDoc, pFilename);
  FDescriptionNode := FDoc.DocumentElement;
  Refresh;
  ls := TStringList.Create;
  GetElementList(ls);
  writeln('.......');
  writeln(ls.Text);
  ls.Free;
end;

procedure TMainForm.Refresh;

  procedure DoTopicNode(Node: TDomElement; Parent: TNodeTreeItem);
  var
    TTreeNode: TTopicTreeItem;
    SubNode: TDomNode;
  begin
    TTreeNode := TTopicTreeItem.Create(TDomElement(Node));
//    TTreeNode.ConnectSelect(@SelectTopic,Node);
//    TTreeNode.ConnectButtonPressEvent(@PopupTopicMenu,Nil);
//    Parent.CheckSubItems;
    Parent.Add(TTreeNode);
//    TFPGtkTree(Parent.SubTree).Append(TTreeNode);
//    TTreeNode.Show;
    SubNode := Node.FirstChild;
    while (SubNode <> nil) do
    begin
      if (SubNode.NodeType = ELEMENT_NODE) and (SubNode.NodeName = 'topic') then
        DoTopicNode(SubNode as TDomElement, TTreeNode);
      SubNode := SubNode.NextSibling;
    end;
  end;

var
  Node, SubNode, SSnode: TDomNode;
  FTreeNode: TPackageTreeItem;
  MTreeNode: TModuleTreeItem;

begin
  CurrentModule := nil;

//  FModuleTree.Tree.ClearItems(0,-1);
  if Assigned(FDescriptionNode) then
  begin
    Node := FDescriptionNode.FirstChild;
    while Assigned(Node) do
    begin
      if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'package') then
      begin
        writeln('package: ', Node.Attributes.GetNamedItem('name').TextContent);
        FTreeNode := TPackageTreeItem.Create(TDomElement(Node));
//        FTreeNode.ConnectSelect(@Selectpackage,Node);
//        FTreeNode.ConnectButtonPressEvent(@PopupModuleMenu,Nil);
        FModuleTree.Add(FTreeNode);
//        FTreeNode.Show;
        SubNode := Node.FirstChild;
        while Assigned(SubNode) do
        begin
          if (SubNode.NodeType = ELEMENT_NODE) and (SubNode.NodeName = 'module') then
          begin
            writeln('  module: ', SubNode.Attributes.GetNamedItem('name').TextContent);
            if not Assigned(CurrentModule) then
              CurrentModule := TDomElement(SubNode);
            MTreeNode := TModuleTreeItem.Create(TDomElement(SubNode));
//            MtreeNode.ConnectSelect(@SelectModule,SubNode);
//            MTreeNode.ConnectButtonPressEvent(@PopupModuleMenu,Nil);
//            FTreeNode.CheckSubtree;
            FTreeNode.Add(MTreeNode);
//            TFPGtkTree(FTreeNode.SubTree).Append(MTreeNode);
            //MTreeNode.Show;
            SSNode := SubNode.FirstChild;
            while (SSNode <> nil) do
            begin
              if (SSNode.NodeType = ELEMENT_NODE) and (SSNode.NodeName = 'topic') then
              begin
                writeln('    topic: ', SSNode.Attributes.GetNamedItem('name').TextContent);
                DoTopicNode(SSNode as TDomElement, MTreeNode);
              end;
              SSNode := SSNode.NextSibling;
            end;
          end
          else if (SubNode.NodeType = ELEMENT_NODE) and (SubNode.NodeName = 'topic') then
          begin
            writeln('  topic: ', SubNode.Attributes.GetNamedItem('name').TextContent);
            DoTopicNode(SubNode as TDomElement,FTreeNode);
          end;
          SubNode := SubNode.NextSibling;
        end;
      end;
      Node := Node.NextSibling;
    end;
  end;
//  CurrentModule := Nil;
  FModified := False;
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
  SetPosition(456, 254, 650, 402);
  WindowTitle := 'frmMain';
  WindowPosition := wpScreenCenter;

  menubar := TfpgMenuBar.Create(self);
  with menubar do
  begin
    SetPosition(0, 0, 650, 23);
    Anchors := [anLeft,anRight,anTop];
  end;

  miFile := TfpgPopupMenu.Create(self);
  with miFile do
  begin
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
    SetPosition(464, 211, 160, 24);
    AddMenuItem('&Options...', '', @miExtraOptionsClicked);
    AddMenuItem('&Build...', '', nil);
  end;

  miHelp := TfpgPopupMenu.Create(self);
  with miHelp do
  begin
    SetPosition(464, 232, 160, 24);
    AddMenuItem('About...', '', @miHelpAboutClicked);
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    SetPosition(566, 370, 75, 23);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    ImageName := 'stdimg.Quit';
    ModalResult := 0;
    ShowImage := True;
    OnClick   := @btnQuitClicked;
  end;

  lblXMLFile := TfpgLabel.Create(self);
  with lblXMLFile do
  begin
    SetPosition(4, 48, 58, 19);
    Text := 'XML File:';
    FontDesc := '#Label1';
  end;

  edXMLFile := TfpgEdit.Create(self);
  with edXMLFile do
  begin
    SetPosition(62, 44, 485, 23);
    Anchors := [anLeft,anRight,anTop];
    Text := '';
    FontDesc := '#Edit1';
  end;

  btnOpen := TfpgButton.Create(self);
  with btnOpen do
  begin
    SetPosition(554, 44, 75, 23);
    Anchors := [anRight,anTop];
    Text := 'Open';
    FontDesc := '#Label1';
    ImageName := 'stdimg.Open';
    ModalResult := 0;
    ShowImage := True;
    OnClick   := @btnOpenClicked;
  end;

  {@VFD_BODY_END: MainForm}

  menubar.AddMenuItem('&File', nil).SubMenu     := miFile;
  menubar.AddMenuItem('&Insert', nil).SubMenu   := miInsert;
  menubar.AddMenuItem('&Extra', nil).SubMenu    := miExtra;
  menubar.AddMenuItem('&Help', nil).SubMenu     := miHelp;
end;

end.

