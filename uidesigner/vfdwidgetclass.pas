{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Main window functionality and designer class.
}

unit vfdwidgetclass;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_menu;

type
  TWidgetClass = class of TfpgWidget;


  TVFDWidgetProperty = class;


  TVFDPropertyEditor = class(TfpgWidget)
  private
    FProp: TVFDWidgetProperty;
  public
    OnUpdate: TNotifyEvent;
    procedure   UpdateProperty(Sender: TObject);
    property    Prop: TVFDWidgetProperty read FProp;
    constructor Create(AOwner: TComponent; aprop: TVFDWidgetProperty); reintroduce;
    procedure   CreateLayout; virtual;
    procedure   LoadValue(wg: TfpgWidget); virtual;
    procedure   StoreValue(wg: TfpgWidget); virtual;
    procedure   SetFocus; virtual;
  end;


  TVFDWidgetProperty = class(TObject)
  public
    Name: string;
    Description: string;
  public
    constructor Create(aName: string); virtual;
    function    ParseSourceLine(wg: TfpgWidget; const line: string): boolean; virtual;
    function    GetPropertySource(wg: TfpgWidget; const ident: string): string; virtual;
    function    GetValueText(wg: TfpgWidget): string; virtual;
    procedure   DrawValue(wg: TfpgWidget; Canvas: TfpgCanvas; rect: TfpgRect; flags: integer); virtual;
    function    CreateEditor(AOwner: TComponent): TVFDPropertyEditor; virtual;
    procedure   OnExternalEdit(wg: TfpgWidget); virtual;
  end;


  TVFDPropertyClass = class of TVFDWidgetProperty;


  TVFDWidgetClass = class(TObject)
  private
    FProps: TList;
  public
    WidgetClass: TWidgetClass;
    Description: string;
    WidgetIconName: string;
    NameBase: string;
    Container: boolean;
    BlockMouseMsg: boolean;
    constructor Create(aClass: TWidgetClass);
    destructor  Destroy; override;
    function    AddProperty(apropname: string; apropclass: TVFDPropertyClass; desc: string): TVFDWidgetProperty;
    function    PropertyCount: integer;
    function    GetProperty(ind: integer): TVFDWidgetProperty;
    function    CreateWidget(AOwner: TComponent): TfpgWidget;
    function    CreatePopupMenu(AWidget: TfpgWidget): TfpgPopupMenu; virtual;
  end;


implementation

uses
  TypInfo;
  

type
  // used to get to SetDesigning() in Form Designer
  TWidgetFriendClass = class(TfpgWidget);


{ TVFDWidgetClass }

function TVFDWidgetClass.AddProperty(apropname: string; apropclass: TVFDPropertyClass;
  desc: string): TVFDWidgetProperty;
begin
  Result := apropclass.Create(apropname);
  Result.Description := desc;
  FProps.Add(Result);
end;

constructor TVFDWidgetClass.Create(aClass: TWidgetClass);
begin
  WidgetClass := aClass;
  FProps      := TList.Create;
  Description := '';
  NameBase    := 'Widget';
  Container   := False;
  BlockMouseMsg := True;
end;

function TVFDWidgetClass.CreateWidget(AOwner: TComponent): TfpgWidget;
begin
  Result := WidgetClass.Create(AOwner);
  TWidgetFriendClass(Result).SetDesigning(True);
end;

function TVFDWidgetClass.CreatePopupMenu(AWidget: TfpgWidget): TfpgPopupMenu;
begin
  { descendant classed can implement this as needed }
  Result := nil;
end;

destructor TVFDWidgetClass.Destroy;
var
  n: integer;
begin
  for n := 0 to FProps.Count - 1 do
    TVFDWidgetProperty(FProps[n]).Free;
  FProps.Free;
  inherited;
end;

function TVFDWidgetClass.GetProperty(ind: integer): TVFDWidgetProperty;
begin
  Result := TVFDWidgetProperty(FProps[ind]);
end;

function TVFDWidgetClass.PropertyCount: integer;
begin
  Result := FProps.Count;
end;

{ TVFDWidgetProperty }

constructor TVFDWidgetProperty.Create(aName: string);
begin
  Name        := aName;
  Description := '';
end;

function TVFDWidgetProperty.GetPropertySource(wg: TfpgWidget; const ident: string): string;
begin

end;

function TVFDWidgetProperty.ParseSourceLine(wg: TfpgWidget; const line: string): boolean;
begin
  Result := False;
end;

function TVFDWidgetProperty.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  Result := nil;
end;

procedure TVFDWidgetProperty.DrawValue(wg: TfpgWidget; Canvas: TfpgCanvas; rect: TfpgRect; flags: integer);
var
  x, y, fy: integer;
  s: string;
begin
  x  := rect.left;
  y  := rect.top;
  fy := y + rect.Height div 2 - Canvas.Font.Height div 2;

  try
    s := GetValueText(wg);
  except
    on E: Exception do
      debugln('Detected an error: ', E.Message);
  end;
  
  Canvas.BeginDraw;
  Canvas.DrawString(x + 1, fy, s);
  Canvas.EndDraw;
end;

function TVFDWidgetProperty.GetValueText(wg: TfpgWidget): string;
begin
  Result := '[' + Name + ']';
end;

procedure TVFDWidgetProperty.OnExternalEdit(wg: TfpgWidget);
begin
  debugln('external edit');
end;

{ TVFDPropertyEditor }

constructor TVFDPropertyEditor.Create(AOwner: TComponent; aprop: TVFDWidgetProperty);
begin
  inherited Create(AOwner);
  OnUpdate := nil;
  FProp    := aprop;
end;

procedure TVFDPropertyEditor.CreateLayout;
begin
  //abstract
end;

procedure TVFDPropertyEditor.LoadValue(wg: TfpgWidget);
begin
  debugln('abstract: editor.LoadValue');
end;

procedure TVFDPropertyEditor.UpdateProperty(Sender: TObject);
begin
  if Assigned(OnUpdate) then
    OnUpdate(self);
end;

procedure TVFDPropertyEditor.StoreValue(wg: TfpgWidget);
begin
  debugln('abstract: editor.StoreValue');
  // check property type
  // the property must be published !
  // PPropInfo := GetPropInfo(object, 'propname');
  // if PPropInfo^.PropType^.name =
end;

procedure TVFDPropertyEditor.SetFocus;
begin
  // do nothing
end;

end.

