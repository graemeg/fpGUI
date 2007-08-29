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
    btnQuit: TfpgButton;
    FDescriptionNode: TDomNode;
    lblXMLFile: TfpgLabel;
    edXMLFile: TfpgEdit;
    btnOpen: TfpgButton;
    menubar: TfpgMenuBar;
    miFile: TfpgPopupMenu;
    miInsert: TfpgPopupMenu;
    miExtra: TfpgPopupMenu;
    miHelp: TfpgPopupMenu;
    
    FDoc: TXMLDocument;
    FModified: Boolean;
    CurrentModule: TDomElement;
    FModuleTree: TObjectList;
    FElementTree: TObjectList;
    
    procedure   btnQuitClicked(Sender: TObject);
    procedure   btnOpenClicked(Sender: TObject);
    procedure   InitializeComponents;
    procedure   SetupMenuBar;
    procedure   miFileQuitClicked(Sender: TObject);
    procedure   miFileOpenClicked(Sender: TObject);
    procedure   miFileSaveAsClicked(Sender: TObject);
    procedure   miHelpAboutClicked(Sender: TObject);
    procedure   ProcessXMLFile(pFilename: string);
    procedure   Refresh;
    procedure   GetElementList(List: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    DescriptionNode: TDomNode read FDescriptionNode write FDescriptionNode;
  end;
  

implementation

uses
  gui_dialogs;
  
  
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
  // Golden ratio 1.618
  Width   := 650;
  Height  := 402;

  InitializeComponents;
  
  FModuleTree := TObjectList.Create;
  FElementTree := TObjectList.Create;
end;

destructor TMainForm.Destroy;
begin
  FModuleTree.Free;
  FElementTree.Free;
  inherited Destroy;
end;

end.

