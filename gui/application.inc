{
    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther
    Copyright (C) 2006 by Graeme Geldenhuys 
      member of the fpGUI development team.

    Application class declarations

    See the file COPYING.fpGUI, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{%mainunit fpgui.pp}

{$IFDEF read_interface}

  { Application class declarations }

  { TApplication }

  TApplication = class(TDefApplication)
  private
//    FDisplay: TGfxDisplay;
//    FDisplayName: String;
//    FForms: TList;
    FUserStyle: TStyle;
//    function GetDisplay: TGfxDisplay;
//    procedure SetDisplayName(const AValue: String);
  protected
    FDefaultStyle: TStyle;
    FTitle: String;
    XMLDoc: TXMLDocument;
    function    GetDefaultStyle: TStyle;
    procedure   SetTitle(const ATitle: String);
  public
//    constructor Create; reintroduce;
    destructor  Destroy; override;
    procedure   SetupXMLStreaming(const AXMLFilename: String);
//    procedure   CreateForm(InstanceClass: TComponentClass; var Reference);
//    procedure   AddForm(AForm: TCustomForm);
//    procedure   Run;
    procedure   SetStyle(pNewStyle: TStyle);
//    property    Display: TGfxDisplay read GetDisplay;
//    property    DisplayName: String read FDisplayName write SetDisplayName;
    property    DefaultStyle: TStyle read GetDefaultStyle;
    property    Title: String read FTitle write SetTitle;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}

// ===================================================================
//   TApplication
// ===================================================================
{
constructor TApplication.Create;
begin
  inherited Create(nil);
  FForms := TList.Create;
end;
}

destructor TApplication.Destroy;
begin
  FUserStyle := nil;
//  FForms.Free;
  // !!!: Only auto-created styles should be destroyed, not user-provided
  if Assigned(FDefaultStyle) then
    FDefaultStyle.Free;
//  if Assigned(FDisplay) then
//    FDisplay.Free;
  inherited Destroy;
end;


procedure TApplication.SetupXMLStreaming(const AXMLFilename: String);
begin
  ReadXMLFile(XMLDoc, AXMLFilename);
end;

{
procedure TApplication.CreateForm(InstanceClass: TComponentClass; var Reference);
type
  PForm = ^TCustomForm;
var
  form: PForm;
  Filename: String;
  TextStream, BinStream: TStream;
begin
  form := @Reference;
  form^ := TCustomForm(InstanceClass.Create(Self));

  Filename := LowerCase(Copy(InstanceClass.ClassName, 2, 255)) + '.frm';

  TextStream := TFileStream.Create(Filename, fmOpenRead);
  BinStream := TMemoryStream.Create;
  ObjectTextToBinary(TextStream, BinStream);
  TextStream.Free;

  BinStream.Position := 0;
  BinStream.ReadComponent(Form^);
  BinStream.Free;

  Form^.Show;
end;
}
{
procedure TApplication.AddForm(AForm: TCustomForm);
begin
  FForms.Add(AForm);
  AForm.Show;
end;
}
{
procedure TApplication.Run;
begin
  Display.Run;
end;
}

procedure TApplication.SetStyle(pNewStyle: TStyle);
begin
  if Assigned(FUserStyle) then
    FUserStyle.Free;
  FUserStyle := pNewStyle;
end;
{
function TApplication.GetDisplay: TGfxDisplay;
begin
  if FDisplay = nil then FDisplay := TDefDisplay.Create(FDisplayName);
  Result := FDisplay;
end;
}
{
procedure TApplication.SetDisplayName(const AValue: String);
begin
  // The display name cannot be changed after we have created it.
  if (FDisplayName=AValue) or (FDisplay<>nil) then exit;
  FDisplayName:=AValue;
end;
}
function TApplication.GetDefaultStyle: TStyle;
begin
  if Assigned(FUserStyle) then
    Result := FUserStyle
  else
  begin
    if not Assigned(FDefaultStyle) then
      FDefaultStyle := TDefaultStyle.Create(self);
    Result := FDefaultStyle;
  end;
end;


procedure TApplication.SetTitle(const ATitle: String);
begin
  if ATitle <> FTitle then
  begin
    FTitle := ATitle;
    // !!!: Change title of all forms with Text='' to FTitle
  end;
end;


{!!!: Remove this ASAP
function TApplication.XMLReadFormProc(Instance: TComponent): Boolean;
var
  Node: TDOMNode;
  Reader: TXMLStreamingReader;
begin
  Result := False;
  if not Assigned(XMLDoc) then
    exit;
  Node := XMLDoc.DocumentElement.FirstChild;

  // Search the correct serialisation node
  while True do
  begin
    if not Assigned(Node) then
      exit;
    if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'component') and
      (UpperCase(TDOMElement(Node).GetAttribute('class')) = Instance.ClassName) then
      break;
    Node := Node.NextSibling;
  end;
  Reader := TXMLStreamingReader.Create(Node as TDOMElement);
  try
    Reader.ReadRootComponent(Instance);
  finally
    Reader.Free;
  end;
  Result := True;
end;}

{$ENDIF read_implementation}

