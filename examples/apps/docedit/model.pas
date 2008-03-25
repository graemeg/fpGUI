unit model;

{$mode objfpc}{$H+}

{$Define DEBUG}

interface

uses
  Classes, SysUtils, dom, contnrs;
  
type

  // forward declaration
  TPackage = class;
  

  TDescDocument = class(TObject)
  private
    FFilename: string;
    FModified: Boolean;
    FPackage: TPackage;
    FDoc: TXMLDocument;
    procedure BuildNodeTree;
  protected
    procedure ProcessXMLFile;
  public
    constructor Create(const pFilename: string);
    destructor Destroy; override;
    property Item: TPackage read FPackage write FPackage;
    property Filename: string read FFilename;
    property Modified: Boolean read FModified write FModified;
  end;
  

  TDocNode = class(TObject)
  private
    FDesc: string;
    FDomElement: TDomElement;
    FItems: TObjectList;
    FName: string;
    FShortDesc: string;
    procedure SetDesc(const AValue: string);
    procedure SetName(const AValue: string);
    procedure SetShortDesc(const AValue: string);
  public
    constructor Create(const aName: string);
    procedure AddItem(aNode: TDocNode);
    property ShortDesc: string read FShortDesc write SetShortDesc;
    property Desc: string read FDesc write SetDesc;
    property Name: string read FName write SetName;
    property Items: TObjectList read FItems;
    property DomElement: TDomElement read FDomElement write FDomElement;
  end;
  

  TPackage = class(TDocNode)        // eg: CoreLib
  end;
  
  
  TModule = class(TDocNode)         // eg: fpgfx
  end;


  TTopic = class(TDocNode)          // eg:  ????
  end;
  

  TElement = class(TDocNode)        // eg: TfpgWindow  or  TfpgWindow.Create
  end;


implementation

uses
  xmlread, xmlwrite, fpdeutil;
  
procedure DebugLn(const AText: string);
begin
  {$IFDEF DEBUG}
  writeln(AText);
  {$ENDIF}
end;

{ TDocNode }

procedure TDocNode.SetShortDesc(const AValue: string);
begin
  if FShortDesc=AValue then exit;
  FShortDesc:=AValue;
end;

constructor TDocNode.Create(const aName: string);
begin
  inherited Create;
  FName := aName;
end;

procedure TDocNode.AddItem(aNode: TDocNode);
begin
  if not Assigned(FItems) then
    FItems := TObjectList.create(True);
  FItems.Add(aNode);
end;

procedure TDocNode.SetDesc(const AValue: string);
begin
  if FDesc=AValue then exit;
  FDesc:=AValue;
end;

procedure TDocNode.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

{ TDescDocument }

procedure TDescDocument.BuildNodeTree;
var
  root: TDomNode; // <fpdoc-description> node
  node, snode, ssnode: TDomNode;
  oModule: TModule;
  oTopic: TTopic;
begin
  root := FDoc.DocumentElement;
  if Assigned(root) then
  begin
    node := root.FirstChild;
    while Assigned(node) do
    begin
      if IsPackageNode(node) then
      begin
        FPackage := TPackage.Create(Node.Attributes.GetNamedItem('name').TextContent);
        FPackage.DomElement := TDomElement(Node);
        DebugLn('found package: ' + FPackage.Name);

        snode := node.FirstChild;
        while Assigned(snode) do
        begin
          // we can have 'module' or 'topic' at this level
          if IsModuleNode(snode) then
          begin
            oModule := TModule.Create(snode.attributes.getnameditem('name').textcontent);
            oModule.DomElement := TDomElement(snode);
            DebugLn('found module: ' + oModule.Name);
            FPackage.AddItem(oModule);

            ssnode := snode.FirstChild;
            while Assigned(ssnode) do
            begin
              if IsTopicNode(ssnode) then
              begin
                oTopic := TTopic.Create(ssnode.attributes.getnameditem('name').textcontent);
                oTopic.DomElement := TDomElement(ssnode);
                DebugLn('found topic: ' + oTopic.Name);
                oModule.AddItem(oTopic);
              end;
              ssnode := ssnode.NextSibling;
            end;  { while }
          end
          else if (snode.NodeType = ELEMENT_NODE) and (snode.NodeName = 'topic') then
          begin
            DebugLn('TODO: package/topic level for node: ' + snode.attributes.getnameditem('name').textcontent);
          end;
          snode := snode.NextSibling;
        end;  { while }
      end;
      node := node.NextSibling;
    end;  { while }
  end;  { if }
end;

procedure TDescDocument.ProcessXMLFile;
begin
  ReadXMLFile(FDoc, FFilename);
  BuildNodeTree;
end;

constructor TDescDocument.Create(const pFilename: string);
begin
  inherited Create;
  FModified := False;
  FFilename := pFilename;
  FDoc := TXMLDocument.Create;
  ProcessXMLFile;
end;

destructor TDescDocument.Destroy;
begin
  FDoc.Free;
  inherited Destroy;
end;


end.

