{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2011 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Style Manager is implemented as a Singleton. New styles will register
      with the style manager. The style manager can also be used to populate
      widgets like a ComboBox or ListBox with available styles.
}
unit fpg_stylemanager;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,Contnrs
  ,fpg_main
  ;
  
const
  cDefaultStyle = 'auto';   // TODO: This text needs to be a resource string for translation

type
  // A class reference for the TfpgStyle descendants
  TfpgStyleClass = class of TfpgStyle;


  // A class to hold the style class mappings. The factory maintains
  // a list of these and uses the StyleClass property to create the objects.
  TfpgStyleClassMapping = class(TObject)
  private
    FsMappingName: string;
    FStyleClass: TfpgStyleClass;
  public
    constructor Create(const AMappingName: string; AStyleClass: TfpgStyleClass); overload;
    property    MappingName: string read FsMappingName;
    property    StyleClass: TfpgStyleClass read FStyleClass;
  end;


  // Style manager and factory class
  TfpgStyleManager = class(TObject)
  private
    FList : TObjectList;
    FDefaultStyle: TfpgStyle;
//    FUserStyle: TfpgStyle;
    FDefaultStyleType: string;
    function    GetStyle: TfpgStyle;
  public
    constructor Create;
    destructor  Destroy; override;
    property    Style: TfpgStyle read GetStyle;
    function    SetStyle(const AStyleName: string): boolean;
    procedure   RegisterClass(const AStyleName: string; AStyleClass : TfpgStyleClass);
    function    CreateInstance(const AStyleName: string): TfpgStyle; overload;
    function    CreateInstance: TfpgStyle; overload;
    procedure   FreeStyleInstance;
    procedure   AssignStyleTypes(const AStrings: TStrings);
  end;


{ Lazy-man's singleton }
function fpgStyleManager: TfpgStyleManager;


implementation

uses
  SysUtils
  ;

var
  uStyleManager: TfpgStyleManager;


{ Creation is deferred to the first request }
function fpgStyleManager: TfpgStyleManager;
begin
  if uStyleManager = nil then
    uStyleManager := TfpgStyleManager.Create;
  result := uStyleManager;
end;


{ TfpgStyleClassMapping }

constructor TfpgStyleClassMapping.Create(const AMappingName: string; AStyleClass: TfpgStyleClass);
begin
  inherited Create;
  FsMappingName := AMappingName;
  FStyleClass   := AStyleClass;
end;


{ TfpgStyleManager }

function TfpgStyleManager.GetStyle: TfpgStyle;
begin
  if not Assigned(FDefaultStyle) then
    FDefaultStyle := CreateInstance(FDefaultStyleType);
  Result := FDefaultStyle;
end;

constructor TfpgStyleManager.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
//  FUserStyle        := nil;
  FDefaultStyle     := nil;
  FDefaultStyleType := cDefaultStyle;    // will change later
end;

destructor TfpgStyleManager.Destroy;
begin
  FreeStyleInstance;
  FList.Free;
  inherited Destroy;
end;

function TfpgStyleManager.SetStyle(const AStyleName: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to FList.Count - 1 do
  begin
    if UpperCase(TfpgStyleClassMapping(FList.Items[i]).MappingName) = UpperCase(AStyleName) then
    begin
      FDefaultStyleType := AStyleName;
      if Assigned(FDefaultStyle) then
        FDefaultStyle.Free;
      FDefaultStyle := CreateInstance;
      Result := True;
      Break; //==>
    end;
  end;

  Assert(FDefaultStyleType <> AStyleName,
      Format('<%s> does not identify a registered style class.', [AStyleName]));
end;

// Register a TStyle class for creation by the factory
procedure TfpgStyleManager.RegisterClass(const AStyleName: string; AStyleClass: TfpgStyleClass);
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    if UpperCase(TfpgStyleClassMapping(FList.Items[i]).MappingName) = UpperCase(AStyleName) then
      Assert(false, Format('Style class <%s> already registered.', [AStyleName]));
  FList.Add(TfpgStyleClassMapping.Create(AStyleName, AStyleClass));
//  writeln('Registering style: ' + AStyleName);
end;

// Call the factory to create an instance of TStyle
function TfpgStyleManager.CreateInstance(const AStyleName: string): TfpgStyle;
var
  i: integer;
begin
  result := nil;
  for i := 0 to FList.Count - 1 do
  begin
    if UpperCase(TfpgStyleClassMapping(FList.Items[i]).MappingName) =
         UpperCase(AStyleName) then
    begin
      result := TfpgStyleClassMapping(FList.Items[i]).StyleClass.Create;
      Break; //==>
    end;
  end;

  Assert(result <> nil, Format('<%s> does not identify a registered style class.', [AStyleName]));
end;

function TfpgStyleManager.CreateInstance: TfpgStyle;
begin
  result := CreateInstance(FDefaultStyleType);
end;

procedure TfpgStyleManager.FreeStyleInstance;
begin
  FreeAndNil(FDefaultStyle);
end;

{ Assign the registered list of style names to a StringList.
  This can be used to populate a combobox with the registered style
  class types. }
procedure TfpgStyleManager.AssignStyleTypes(const AStrings: TStrings);
var
  i: integer;
begin
  AStrings.Clear;
  for i := 0 to FList.Count - 1 do
    AStrings.Add(TfpgStyleClassMapping(FList.Items[i]).MappingName);
end;


finalization
  uStyleManager.Free;

end.

