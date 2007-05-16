{
    fpGUI  -  Free Pascal GUI Library
    
    Style Manager as a Singleton
    
    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit StyleManager;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,Contnrs
  ,fpGUI
  ;
  
const
  cDefaultStyle = 'auto';

type

  // A class reference for the TStyle descendants
  TStyleClass = class of TStyleAbs;


  // A class to hold the TStyle class mappings. The factory maintains
  // a list of these and uses the StyleClass property to create the objects.
  TStyleClassMapping = class(TObject)
  private
    FsMappingName: string;
    FStyleClass: TStyleClass;
  public
    constructor Create(const AMappingName: string; AStyleClass: TStyleClass); overload;
    property    MappingName: string read FsMappingName;
    property    StyleClass: TStyleClass read FStyleClass;
  end;


  // Style manager and factory class

  { TStyleManager }

  TStyleManager = class(TObject)
  private
    FList : TObjectList;
    FDefaultStyle: TStyleAbs;
    FUserStyle: TStyleAbs;
    FDefaultStyleType: string;
    function    GetDefaultStyle: TStyleAbs;
  public
    constructor Create;
    destructor  Destroy; override;
    property    DefaultStyle: TStyleAbs read GetDefaultStyle;
    procedure   SetStyle(const AStyleName: string);
    procedure   RegisterClass(const AStyleName: string; AStyleClass : TStyleClass);
    function    CreateInstance(const AStyleName: string): TStyleAbs; overload;
    function    CreateInstance: TStyleAbs; overload;
    procedure   AssignStyleTypes(AStrings: TStrings);
  end;


// Singleton
function gStyleManager: TStyleManager;


implementation
uses
  SysUtils
  ,fpGFX
  ,WindowsStyle
  ,OpenSoftStyle
  ,MotifStyle
  ;

var
  uStyleManager: TStyleManager;


{ Creation is deferred to the first request }
function gStyleManager: TStyleManager;
begin
  if uStyleManager = nil then
    uStyleManager := TStyleManager.Create;
  result := uStyleManager;
end;


{ TStyleManager }

function TStyleManager.GetDefaultStyle: TStyleAbs;
begin
  if not Assigned(FDefaultStyle) then
//    FDefaultStyle.Free;
  FDefaultStyle := CreateInstance(FDefaultStyleType);
  Result := FDefaultStyle;
end;

constructor TStyleManager.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
  FUserStyle        := nil;
  FDefaultStyle     := nil;
  FDefaultStyleType := cDefaultStyle;    // will change later
end;

destructor TStyleManager.Destroy;
begin
  FDefaultStyle.Free;
  FList.Free;
  inherited Destroy;
end;

procedure TStyleManager.SetStyle(const AStyleName: string);
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    if UpperCase(TStyleClassMapping(FList.Items[i]).MappingName) =
         UpperCase(AStyleName) then
    begin
      FDefaultStyleType := AStyleName;
      Break; //==>
    end;

  Assert(FDefaultStyleType <> AStyleName,
          Format('<%s> does not identify a registered style class.',
                   [AStyleName]));
end;

// Register a TStyle class for creation by the factory
procedure TStyleManager.RegisterClass(const AStyleName: string;
  AStyleClass: TStyleClass);
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    if UpperCase(TStyleClassMapping(FList.Items[i]).MappingName) =
         UpperCase(AStyleName) then
      Assert(false,
              Format('Style class <%s> already registered.',
                      [AStyleName]));
  FList.Add(TStyleClassMapping.Create(AStyleName, AStyleClass));
  
//  writeln('Registering style: ' + AStyleName);
  // we will use this later
//  FDefaultStyleType := UpperCase(AStyleName);
end;

// Call the factory to create an instance of TStyle
function TStyleManager.CreateInstance(const AStyleName: string): TStyleAbs;
var
  i: integer;
begin
  result := nil;
  for i := 0 to FList.Count - 1 do
    if UpperCase(TStyleClassMapping(FList.Items[i]).MappingName) =
         UpperCase(AStyleName) then
    begin
      result := TStyleClassMapping(FList.Items[i]).StyleClass.Create;
      Break; //==>
    end;

  Assert(result <> nil,
          Format('<%s> does not identify a registered style class.',
                   [AStyleName]));
end;

function TStyleManager.CreateInstance: TStyleAbs;
begin
  result := CreateInstance(FDefaultStyleType);
end;

// Assing the registered list of TStyle names to a StringList
// This can be used to populate a combobox with the available TStyle
// class types.
procedure TStyleManager.AssignStyleTypes(AStrings: TStrings);
var
  i: integer;
begin
  AStrings.Clear;
  for i := 0 to FList.Count - 1 do
    AStrings.Add(TStyleClassMapping(FList.Items[i]).MappingName);
end;

{ TStyleClassMapping }

constructor TStyleClassMapping.Create(const AMappingName: string;
  AStyleClass: TStyleClass);
begin
  inherited Create;
  FsMappingName := AMappingName;
  FStyleClass   := AStyleClass;
end;


initialization
  gStyleManager.RegisterClass(cDefaultStyle, TWin2000Style);
  gStyleManager.RegisterClass('Windows 9x', TWin9xStyle);
  gStyleManager.RegisterClass('Windows 2000', TWin2000Style);
  gStyleManager.RegisterClass('OpenSoft', TOpenSoftStyle);
  gStyleManager.RegisterClass('Motif', TMotifStyle);

finalization
  uStyleManager.Free;

end.

