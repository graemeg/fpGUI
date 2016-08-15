{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2015 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Property editors.
}

unit vfd_props;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget,
  vfd_widgetclass,
  fpg_edit,
  fpg_button,
  fpg_combobox;

type

  TPropertyString = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg: TfpgWidget; const line: string): boolean; override;
    function GetPropertySource(wg: TfpgWidget; const ident: string): string; override;
    function GetValueText(wg: TfpgWidget): string; override;
    function CreateEditor(AOwner: TComponent): TVFDPropertyEditor; override;
  end;


  TPropertyInteger = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg: TfpgWidget; const line: string): boolean; override;
    function GetPropertySource(wg: TfpgWidget; const ident: string): string; override;
    function GetValueText(wg: TfpgWidget): string; override;
    function CreateEditor(AOwner: TComponent): TVFDPropertyEditor; override;
  end;


  TPropertyFloat = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg: TfpgWidget; const line: string): boolean; override;
    function GetPropertySource(wg: TfpgWidget; const ident: string): string; override;
    function GetValueText(wg: TfpgWidget): string; override;
    function CreateEditor(AOwner: TComponent): TVFDPropertyEditor; override;
  end;


  TPropertyEnum = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg: TfpgWidget; const line: string): boolean; override;
    function GetPropertySource(wg: TfpgWidget; const ident: string): string; override;
    function GetValueText(wg: TfpgWidget): string; override;
    function CreateEditor(AOwner: TComponent): TVFDPropertyEditor; override;
  end;


  TPropertyStringList = class(TVFDWidgetProperty)
  public
    function  ParseSourceLine(wg: TfpgWidget; const line: string): boolean; override;
    function  GetPropertySource(wg: TfpgWidget; const ident: string): string; override;
    function  GetValueText(wg: TfpgWidget): string; override;
    function  CreateEditor(AOwner: TComponent): TVFDPropertyEditor; override;
    procedure OnExternalEdit(wg: TfpgWidget); override;
  end;


  TPropertyBoolean = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg: TfpgWidget; const line: string): boolean; override;
    function GetPropertySource(wg: TfpgWidget; const ident: string): string; override;
    function GetValueText(wg: TfpgWidget): string; override;
    function CreateEditor(AOwner: TComponent): TVFDPropertyEditor; override;
  end;


  TPropertyFontDesc = class(TPropertyString)
    function  CreateEditor(AOwner: TComponent): TVFDPropertyEditor; override;
    procedure OnExternalEdit(wg: TfpgWidget); override;
  end;


  TPropertyColor = class(TVFDWidgetProperty)
  public
    procedure DrawValue(wg: TfpgWidget; Canvas: TfpgCanvas; rect: TfpgRect; flags: integer); override;
    function  ParseSourceLine(wg: TfpgWidget; const line: string): boolean; override;
    function  GetPropertySource(wg: TfpgWidget; const ident: string): string; override;
    function  GetValueText(wg: TfpgWidget): string; override;
    function  CreateEditor(AOwner: TComponent): TVFDPropertyEditor; override;
    procedure OnExternalEdit(wg: TfpgWidget); override;
  end;

  { TPropertyObject }

  TPropertyObject = class(TVFDWidgetProperty)
    function  CreateEditor(AOwner: TComponent): TVFDPropertyEditor; override;
    function  ParseSourceLine(wg: TfpgWidget; const line: string): boolean; override;
    function  GetPropertySource(wg: TfpgWidget; const ident: string; out afterObject: TObject): string; override;
    function  GetValueText(wg: TfpgWidget): string; override;
  end;

  { TPropertyInterface }

  TPropertyInterface = class(TPropertyObject)
    function  CreateEditor(AOwner: TComponent): TVFDPropertyEditor; override;
    function  GetPropertySource(wg: TfpgWidget; const ident: string; out afterObject: TObject): string; override;
    function  GetValueText(wg: TfpgWidget): string; override;
  end;

  { TVFDPropertyList }

  TVFDPropertyList = class(TVFDWidgetProperty)
    List: TList;
    EditorClass: TVFDPropertyEditorClass;
  public
    constructor Create(aName: string; alisteditorclass: TVFDPropertyEditorClass; AList: TList); reintroduce; virtual;
    function    CreateEditor(AOwner: TComponent): TVFDPropertyEditor; override;
    function    ParseSourceLine(wg: TfpgWidget; const line: string): boolean; override;
    function    GetPropertySource(wg: TfpgWidget; const ident: string; out afterObject: TObject): string; override;
    function    GetValueText(wg: TfpgWidget): string; override;
  end;


  TGPEType = (gptInteger, gptString, gptFloat);


  TGeneralPropertyEditor = class(TVFDPropertyEditor)
  private
    FOrigValue: string;
    procedure EditExit(Sender: TObject);
    procedure EditKeyPressed(Sender: TObject; var KeyCode: word;
      var ShiftState: TShiftState; var Consumed: boolean);
  public
    etype: TGPEType;
    edit: TfpgEdit;
    procedure CreateLayout; override;
    procedure LoadValue(wg: TfpgWidget); override;
    procedure StoreValue(wg: TfpgWidget); override;
    procedure LoadIntValue(wg: TfpgWidget);
    procedure StoreIntValue(wg: TfpgWidget);
    procedure LoadStrValue(wg: TfpgWidget);
    procedure StoreStrValue(wg: TfpgWidget);
    procedure LoadFloatValue(wg: TfpgWidget);
    procedure StoreFloatValue(wg: TfpgWidget);
    procedure SetFocus; override;
  end;


  TChoicePropertyEditor = class(TVFDPropertyEditor)
  public
    chl: TfpgComboBox;
    procedure CreateLayout; override;
    procedure LoadValue(wg: TfpgWidget); override;
    procedure StoreValue(wg: TfpgWidget); override;
    procedure SetFocus; override;
  end;


  TBooleanPropertyEditor = class(TChoicePropertyEditor)
  public
    procedure LoadValue(wg: TfpgWidget); override;
    procedure StoreValue(wg: TfpgWidget); override;
  end;

  { TObjectPropertyEditor }

  TObjectPropertyEditor = class(TChoicePropertyEditor)
    procedure LoadValue(wg: TfpgWidget); override;
    procedure StoreValue(wg: TfpgWidget); override;
  end;

  { TInterfacePropertyEditor }

  TInterfacePropertyEditor = class(TChoicePropertyEditor)
    procedure LoadValue(wg: TfpgWidget); override;
    procedure StoreValue(wg: TfpgWidget); override;
  end;

  { TListPropertyEditor }

  TListPropertyEditor = class(TChoicePropertyEditor)
  public
    List: TList;
    class function  GetItemString(AItem: Pointer): string; virtual;
    class function  GetItemFromString(AList: TList; AValue: String): Pointer; virtual;
    class procedure SetWidgetProperty(wg: TfpgWidget; PropName: String; PropValue: Pointer); virtual;
    class function  GetWidgetProperty(wg: TfpgWidget; PropName: String): Pointer; virtual;
    constructor Create(AOwner: TComponent; aprop: TVFDWidgetProperty; alist: TList); reintroduce;
  end;
  TListPropertyEditorClass = class of TListPropertyEditor;

  { TClassListPropertyEditor }

  TClassListPropertyEditor = class(TListPropertyEditor)
    class function GetItemString(AItem: Pointer): string; override;
    class function GetItemFromString(AList: TList; AValue: String): Pointer; override;
    class procedure SetWidgetProperty(wg: TfpgWidget; PropName: String; PropValue: Pointer); override;
    class function  GetWidgetProperty(wg: TfpgWidget; PropName: String): Pointer; override;
    procedure LoadValue(wg: TfpgWidget); override;
    procedure StoreValue(wg: TfpgWidget); override;
  end;


  TExternalPropertyEditor = class(TVFDPropertyEditor)
  protected
    procedure HandlePaint; override;
  public
    btnEdit: TfpgButton;
    Widget: TfpgWidget;
    procedure CreateLayout; override;
    procedure LoadValue(wg: TfpgWidget); override;
    procedure StoreValue(wg: TfpgWidget); override;
    procedure OnEditClick(Sender: TObject);
  end;


procedure EditStringList(sl: TStringList);
procedure GetEnumPropValueList(wg: TObject; const APropName: string; sl: TStringList);

const
  DefUndoOnPropExit = False;

var
  UndoOnPropExit: Boolean = DefUndoOnPropExit;

implementation

uses
  TypInfo,
  vfd_formparser,
  vfd_editors,
  vfd_constants,
  vfd_designer,
  fpg_dialogs;


procedure EditStringList(sl: TStringList);
var
  frm: TItemEditorForm;
begin
  frm := TItemEditorForm.Create(nil);
  try
    frm.edItems.Lines.Assign(sl);
    if frm.ShowModal = mrOK then
      sl.Assign(frm.edItems.Lines);
  finally
    frm.Free;
  end;
end;

procedure GetEnumPropValueList(wg: TObject; const APropName: string; sl: TStringList);
var
  lPropInfo: PPropInfo;
  s: string;
  lTypeData: PTypeData;
  n: integer;
begin
  lPropInfo := GetPropInfo(wg, APropName);
  lTypeData := GetTypeData(lPropInfo^.PropType);

  sl.BeginUpdate;
  try
    sl.Clear;
    for n := lTypeData^.MinValue to lTypeData^.MaxValue do
    begin
      s := GetEnumName(lPropInfo^.PropType, n);
      sl.Add(s);
    end;
  finally
    sl.EndUpdate;
  end;
end;

{ TListPropertyEditor }

class function TListPropertyEditor.GetItemString(AItem: Pointer): string;
begin
  Result := '';
end;

class function TListPropertyEditor.GetItemFromString(AList: TList; AValue: String): Pointer;
begin
  Result := nil;
end;

class procedure TListPropertyEditor.SetWidgetProperty(wg: TfpgWidget; PropName: String; PropValue: Pointer);
begin
  //
end;

class function TListPropertyEditor.GetWidgetProperty(wg: TfpgWidget; PropName: String): Pointer;
begin
  Result := nil;
end;

constructor TListPropertyEditor.Create(AOwner: TComponent;
  aprop: TVFDWidgetProperty; alist: TList);
begin
  inherited Create(AOwner, aprop);
  List := alist;
end;

{ TClassListPropertyEditor }

class function TClassListPropertyEditor.GetItemString(AItem: Pointer): string;
begin
  Result:=TClass(AItem).ClassName;
end;

class function TClassListPropertyEditor.GetItemFromString(AList: TList;
  AValue: String): Pointer;
var
  p: Pointer;
begin
  for p in AList do
    if LowerCase(TClass(p).ClassName) = Lowercase(AValue) then
      Exit(p);
  Result := nil;
end;

class procedure TClassListPropertyEditor.SetWidgetProperty(wg: TfpgWidget;
  PropName: String; PropValue: Pointer);
begin
  SetObjectProp(wg, PropName, TObject(PropValue));
end;

class function TClassListPropertyEditor.GetWidgetProperty(wg: TfpgWidget;
  PropName: String): Pointer;
begin
  Result := Pointer(GetObjectProp(wg, PropName));
end;

procedure TClassListPropertyEditor.LoadValue(wg: TfpgWidget);
var
  i: Integer;
  c: TClass;
  index: Integer = 0;
  o: TObject;
begin

  c := TClass(GetObjectProp(wg, prop.Name));

  for i := 0 to List.Count-1 do
  begin
    chl.Items.AddObject(TClass(List[i]).ClassName, TObject(List[i]));
    if List[i] = Pointer(c) then
      chl.FocusItem:=i;
  end;

  if chl.FocusItem = -1 then
    chl.FocusItem := 0;
end;

procedure TClassListPropertyEditor.StoreValue(wg: TfpgWidget);
begin
  if chl.FocusItem = -1 then
    chl.FocusItem := 0;

  SetObjectProp(wg, prop.Name, TObject(chl.Items.Objects[chl.FocusItem]));
end;

{ TVFDPropertyList }

constructor TVFDPropertyList.Create(aName: string;
  alisteditorclass: TVFDPropertyEditorClass; AList: TList);
begin
  inherited Create(aName);
  EditorClass := alisteditorclass;
  List := AList;
end;

function TVFDPropertyList.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  Result:=TListPropertyEditorClass(EditorClass).Create(AOwner, Self, List);
end;

function TVFDPropertyList.ParseSourceLine(wg: TfpgWidget; const line: string): boolean;
var
  s: string;
  pval: pointer;
  sval: String;
begin
  s      := line;
  Result := False;
  if UpperCase(GetIdentifier(s)) <> UpperCase(Name) then
    Exit;

  Result := CheckSymbol(s, ':=');
  if Result then
  begin
    sval := GetIdentifier(s);
    pval   := TListPropertyEditorClass(EditorClass).GetItemFromString(List, sval);
    Result := CheckSymbol(s, ';');
    if Result then
      TListPropertyEditorClass(EditorClass).SetWidgetProperty(wg, Name, pval);
  end;
end;

function TVFDPropertyList.GetPropertySource(wg: TfpgWidget; const ident: string;
  out afterObject: TObject): string;
var
  item: Pointer;
  s: string;
  PropInfo: PPropInfo;
  Editor: TListPropertyEditorClass;
begin
  PropInfo := GetPropInfo(wg.ClassType, Name);
  if IsStoredProp(wg, PropInfo) then
  begin
    Editor := TListPropertyEditorClass(EditorClass);
    item := Editor.GetWidgetProperty(wg, Name);
    s := Editor.GetItemString(item);
    if s <> '' then
      Result := ident + Name + ' := ' + s + ';' + LineEnding
    else
      Result := '';
  end;
end;

function TVFDPropertyList.GetValueText(wg: TfpgWidget): string;
var
  Editor: TListPropertyEditorClass;
  item: Pointer;
begin
  Editor := TListPropertyEditorClass(EditorClass);
  item := Editor.GetWidgetProperty(wg, Name);
  Result := Editor.GetItemString(item);
end;

{ TPropertyInterface }

function TPropertyInterface.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  Result := TInterfacePropertyEditor.Create(AOwner, self);
end;

function TPropertyInterface.GetPropertySource(wg: TfpgWidget;
  const ident: string; out afterObject: TObject): string;
var
  N: String;
  I: IInterface;
  O: TComponent;
begin
  I := GetInterfaceProp(wg, Name);
  if Assigned(I) then
  begin
    O := TComponent(I as TComponent);
    N := O.Name;
    if N = Name then
       N := 'Self.'+N;
    Result := ident + Name + ' := ' + N + ';' + LineEnding;
    afterObject := O;
  end
  else
  begin
    Result := '';
    afterObject := nil;
  end;
end;

function TPropertyInterface.GetValueText(wg: TfpgWidget): string;
var
  i: IInterface;
  o: TObject;
  n: String;
begin
  i := GetInterfaceProp(wg, Name);
  if Assigned(i) then
  begin
    try
      o := i as TObject;
      Result := TComponent(o).Name
    except
      i := nil;
      SetInterfaceProp(wg, Name, i);
    end;
  end
  else
  begin
    n := GetPropInfo(wg, Name)^.PropType^.Name;
    Result := '['+n+']';
  end;
end;

{ TInterfacePropertyEditor }

procedure TInterfacePropertyEditor.LoadValue(wg: TfpgWidget);
var
  FormD: TFormDesigner;
  Items: TList = nil;
  i: Integer;
  index: Integer = 0;
  intf: IInterface;
  intfid: String;
  intfguid: TGuid;
  Info: PPropInfo;
begin

  Info := GetPropInfo(wg, Prop.Name);
  intfid := GetTypeData(Info^.PropType)^.IIDStr;
  intfguid := GetTypeData(Info^.PropType)^.GUID;
  intf := GetInterfaceProp(wg, Prop.Name);

  // add empty object
  chl.Items.AddObject('(none)', nil);

  try
    FormD := GetFormDesigner(wg);
    if FormD.FindWidgetsByInterface(intfid, intfguid, Items) then
    begin
      for i := 0 to Items.Count-1 do
      begin
        chl.Items.AddObject(TfpgComponent(Items[i]).Name, TObject(Items[i]));
        if Assigned(intf) and (TObject(Items[i]) = (intf as TObject)) then
          index := i+1; // +1 is for (none) above
      end;
    end;
  finally
    if Assigned(Items) then
      Items.Free;
  end;
  // index is always good. 0 = (none)
  chl.FocusItem:=index;
end;

procedure TInterfacePropertyEditor.StoreValue(wg: TfpgWidget);
var
  i: Integer;
  Intf: IInterface = nil;
  o: TObject;
  TypeData: PTypeData;
begin
  i := chl.Items.IndexOf(chl.Text);
  //SetObjectProp(wg,prop.Name, chl.Items.Objects[i]);
  o := TObject(chl.Items.Objects[i]);

  if Assigned(o) then
  begin
    TypeData := GetTypeData(GetPropInfo(wg, prop.Name)^.PropType);
    o.GetInterface(TypeData^.GUID, Intf);
    if Intf = nil then
      o.GetInterface(TypeData^.IIDStr, Intf);
  end;


  SetInterfaceProp(wg,prop.Name, Intf);
end;

{ TPropertyObject }

function TPropertyObject.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  Result := TObjectPropertyEditor.Create(AOwner, self);
end;

function TPropertyObject.ParseSourceLine(wg: TfpgWidget; const line: string): boolean;
var
  s, sval: string;
  FormD: TFormDesigner;
  o: TfpgWidget;
begin
  s      := line;
  Result := False;
  if UpperCase(GetIdentifier(s)) <> UpperCase(Name) then
    Exit;

  Result := CheckSymbol(s, ':=');
  if Result then
  begin
    sval   := GetIdentifier(s);
    // a property name can be the same as it's value.
    // i.e. with Obj do Strings := Strings needs to be Self.Strings to get outside of Obj.
    if (UpperCase(sval) = 'SELF') and (CheckSymbol(s, '.')) then
      sval := GetIdentifier(s);
    Result := CheckSymbol(s, ';');
  end;

  if Result then
    try
      FormD := GetFormDesigner(wg);
      o := FormD.FindWidgetByName(sval);
      if Self.ClassType = TPropertyObject then
         SetObjectProp(wg, Name, o)
      else if Self.ClassType = TPropertyInterface then
        SetInterfaceProp(wg, Name, o);
    except
//      Writeln('invalid object value: "' + sval + '" for ' + Name);
      Result := False;
    end;
end;

function TPropertyObject.GetPropertySource(wg: TfpgWidget; const ident: string;
  out afterObject: TObject): string;
var
  N: String;
  O: TObject;
begin
  O := GetObjectProp(wg, Name);
  if Assigned(O) then
  begin
    N := TComponent(O).Name;
    if N = Name then
      N := 'Self.'+N;
    Result := ident + Name + ' := ' + N + ';' + LineEnding;
    afterObject := O;
  end
  else
  begin
    Result := '';
    afterObject := nil;
  end;
end;

function TPropertyObject.GetValueText(wg: TfpgWidget): string;
var
  o: TObject;
  c: TClass;
begin
  o := GetObjectProp(wg, Name);
  if Assigned(o) then
  begin
    try
      Result := TComponent(o).Name;
    except
      o := nil;
      SetObjectProp(wg, Name, o);
    end;
  end
  else
  begin
    c := GetObjectPropClass(wg, Name);
    Result := '['+c.ClassName+']';
  end;
end;

{ TObjectPropertyEditor }

procedure TObjectPropertyEditor.LoadValue(wg: TfpgWidget);
var
  FormD: TFormDesigner;
  Items: TList = nil;
  i: Integer;
  c: TClass;
  index: Integer = 0;
  o: TObject;
begin

  o := GetObjectProp(wg, prop.Name);
  c := GetObjectPropClass(wg, prop.Name);

  // add empty object
  chl.Items.AddObject('(none)', nil);

  try
    FormD := GetFormDesigner(wg);
    if Formd.FindWidgetsByClass(c, Items) then
    begin
      for i := 0 to Items.Count-1 do
      begin
        chl.Items.AddObject(TfpgComponent(Items[i]).Name, TObject(Items[i]));
        if Assigned(o) and (TObject(Items[i]) = o) then
          index := i+1 // +1 because we added a nil item at the start
      end;
    end;
  finally
    if Assigned(Items) then
      Items.Free;
  end;
  // index is always good. 0 = (none)
  chl.FocusItem:=index;
end;

procedure TObjectPropertyEditor.StoreValue(wg: TfpgWidget);
var
  i: Integer;
begin
  i := chl.Items.IndexOf(chl.Text);
  SetObjectProp(wg,prop.Name, chl.Items.Objects[i]);
end;

{ TPropertyString }

function TPropertyString.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  Result := TGeneralPropertyEditor.Create(AOwner, self);
  with TGeneralPropertyEditor(Result) do
    etype := gptString;
end;

function TPropertyString.GetPropertySource(wg: TfpgWidget; const ident: string): string;
begin
  Result := ident + Name + ' := ' + QuotedStr(GetStrProp(wg, Name)) + ';' + LineEnding;
end;

function TPropertyString.GetValueText(wg: TfpgWidget): string;
begin
  Result := GetStrProp(wg, Name);
end;

function TPropertyString.ParseSourceLine(wg: TfpgWidget; const line: string): boolean;
var
  s, sval: string;
begin
  s      := line;
  Result := False;
  if UpperCase(GetIdentifier(s)) <> UpperCase(Name) then
    Exit;

  Result := CheckSymbol(s, ':=');
  if Result then
  begin
    sval   := GetStringValue(s);
    Result := CheckSymbol(s, ';');
  end;

  if Result then
    SetStrProp(wg, Name, sval);
end;


{ TPropertyInteger }

function TPropertyInteger.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  Result := TGeneralPropertyEditor.Create(AOwner, self);
  with TGeneralPropertyEditor(Result) do
    etype := gptInteger;
end;

function TPropertyInteger.GetPropertySource(wg: TfpgWidget; const ident: string): string;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(wg.ClassType, Name);
  if PropInfo^.Default <> GetOrdProp(wg, Name) then
    Result := ident + Name + ' := ' + IntToStr(GetOrdProp(wg, Name)) + ';' + LineEnding
  else
    Result := '';
end;

function TPropertyInteger.GetValueText(wg: TfpgWidget): string;
begin
  Result := IntToStr(GetOrdProp(wg, Name));
end;

function TPropertyInteger.ParseSourceLine(wg: TfpgWidget; const line: string): boolean;
var
  s: string;
  ival: integer;
begin
  s      := line;
  Result := False;
  if UpperCase(GetIdentifier(s)) <> UpperCase(Name) then
    Exit;

  Result := CheckSymbol(s, ':=');
  if Result then
  begin
    ival   := GetIntValue(s);
    Result := CheckSymbol(s, ';');
  end
  else
    ival   := 0;

  if Result then
    SetOrdProp(wg, Name, ival);
end;

{ TGeneralPropertyEditor }

procedure TGeneralPropertyEditor.EditExit(Sender: TObject);
begin
  if FOrigValue <> edit.Text then
    if UndoOnPropExit then
      edit.Text := FOrigvalue
    else
    begin
      UpdateProperty(nil);
      FOrigValue := edit.Text;
    end;
end;

procedure TGeneralPropertyEditor.EditKeyPressed(Sender: TObject;
  var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  if (KeyCode = keyReturn) or (KeyCode = keyPEnter) then
  begin
    UpdateProperty(nil);
    FOrigValue := edit.Text;
  end
  else if (keycode=keyEscape) then
  begin
    edit.Text := FOrigValue;
  end
  else
    inherited;
end;

procedure TGeneralPropertyEditor.CreateLayout;
begin
  Anchors       := [anTop, anLeft, anRight];
  Edit          := TfpgEdit.Create(self);
  Edit.SetPosition(0, 0, Width, Height);
  Edit.Anchors  := Anchors;
//  Edit.OnChange := @UpdateProperty;
  Edit.OnKeyPress := @EditKeyPressed;
  Edit.OnExit := @EditExit;
  Edit.Visible := True;
end;

procedure TGeneralPropertyEditor.LoadIntValue(wg: TfpgWidget);
begin
  edit.Text := IntToStr(GetOrdProp(wg, prop.Name));
end;

procedure TGeneralPropertyEditor.LoadStrValue(wg: TfpgWidget);
var
  s: string;
begin
  s := GetStrProp(wg, prop.Name);
  if etype = gptString then
    edit.Text := s;
end;

procedure TGeneralPropertyEditor.LoadValue(wg: TfpgWidget);
begin
  case etype of
    gptInteger:
        LoadIntValue(wg);
    gptFloat:
        LoadFloatValue(wg);
    else
        LoadStrValue(wg);
  end;
  FOrigValue := edit.Text;
end;

procedure TGeneralPropertyEditor.StoreIntValue(wg: TfpgWidget);
var
  i: integer;
begin
  try
    i := StrToInt(edit.Text);
    SetOrdProp(wg, Prop.Name, i);
  except
    // error
  end;
end;

procedure TGeneralPropertyEditor.StoreStrValue(wg: TfpgWidget);
var
  s: string;
begin
  if etype = gptString then
    s := edit.Text;
  SetStrProp(wg, prop.Name, s);
end;

procedure TGeneralPropertyEditor.LoadFloatValue(wg: TfpgWidget);
begin
  edit.Text := FloatToStr(GetFloatProp(wg, prop.Name));
end;

procedure TGeneralPropertyEditor.StoreFloatValue(wg: TfpgWidget);
var
  i: extended;
begin
  try
    i := StrToFloat(edit.Text);
    SetFloatProp(wg, Prop.Name, i);
  except
    // error
  end;
end;

procedure TGeneralPropertyEditor.SetFocus;
begin
  Edit.SetFocus;
end;

procedure TGeneralPropertyEditor.StoreValue(wg: TfpgWidget);
begin
  case etype of
    gptInteger:
        StoreIntValue(wg);
    gptFloat:
        StoreFloatValue(wg);
    else
        StoreStrValue(wg);
  end;
end;

{ TPropertyStringList }

function TPropertyStringList.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  Result := TExternalPropertyEditor.Create(AOwner, self);
end;

function TPropertyStringList.GetPropertySource(wg: TfpgWidget; const ident: string): string;
var
  sl: TStringList;
  f: integer;
begin
  sl := TStringList(GetObjectProp(wg, Name, TStrings));
  if not Assigned(sl) then
    raise Exception.Create(rsErrNoStringsProperty);

  Result := '';

  //if sl.Text <> '' then
  //begin
    //writeln('Text = <', sl.Text, '>');
    //writeln('StringList.Count = ', sl.Count);
    for f := 0 to sl.Count - 1 do
      Result := Result + ident + Name + '.Add(' + QuotedStr(sl.Strings[f]) + ');' + LineEnding;
  //end;
end;

function TPropertyStringList.GetValueText(wg: TfpgWidget): string;
var
  sl: TStringList;
begin
  sl     := TStringList(GetObjectProp(wg, Name, TStrings));
  if not Assigned(sl) then
    raise Exception.Create(rsErrNoStringsProperty);
  Result := '[' + IntToStr(sl.Count) + ' lines]';
end;

procedure TPropertyStringList.OnExternalEdit(wg: TfpgWidget);
var
  sl: TStringList;
begin
  sl := TStringList(GetObjectProp(wg, Name, TStrings));
  if not Assigned(sl) then
    raise Exception.Create(rsErrNoStringsProperty);
  EditStringList(sl);
end;

function TPropertyStringList.ParseSourceLine(wg: TfpgWidget; const line: string): boolean;
var
  s: string;
  sval: string;
  sl: TStringList;
begin
  s      := line;
  Result := False;
  if UpperCase(GetIdentifier(s)) <> UpperCase(Name) then
    Exit;

  Result := CheckSymbol(s, '.');
  Result := Result and (UpperCase(GetIdentifier(s)) = 'ADD');
  Result := Result and CheckSymbol(s, '(');
  if Result then
  begin
    sval   := GetStringValue(s);
    Result := Result and CheckSymbol(s, ')');
    Result := Result and CheckSymbol(s, ';');
  end;

  if Result then
  begin
    sl := TStringList(GetObjectProp(wg, Name, TStrings));
    if not Assigned(sl) then
      raise Exception.Create(rsErrNoStringsProperty);
    sl.Add(sval);
  end;
end;

{ TPropertyBoolean }


function TPropertyBoolean.ParseSourceLine(wg: TfpgWidget; const line: string): boolean;
var
  s: string;
  bval: boolean;
begin
  s      := line;
  Result := False;
  if UpperCase(GetIdentifier(s)) <> UpperCase(Name) then
    Exit;

  Result := CheckSymbol(s, ':=');
  if Result then
  begin
    bval   := GetBoolValue(s);
    Result := CheckSymbol(s, ';');
  end
  else
    bval   := False;

  if Result then
    SetOrdProp(wg, Name, Ord(bval));
end;

function TPropertyBoolean.GetPropertySource(wg: TfpgWidget; const ident: string): string;
var
  i: integer;
  s: string;
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(wg.ClassType, Name);
  i := GetOrdProp(wg, Name);
  if IsStoredProp(wg, PropInfo) then
  begin
    if PropInfo^.Default <> i then
    begin
      if i = 1 then
        s := 'True'
      else
        s := 'False';
      Result := ident + Name + ' := ' + s + ';' + LineEnding;
    end
    else
      Result := '';
  end;
end;

function TPropertyBoolean.GetValueText(wg: TfpgWidget): string;
begin
  if GetOrdProp(wg, Name) = 1 then
    Result := 'True'
  else
    Result := 'False';
end;

function TPropertyBoolean.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  Result := TBooleanPropertyEditor.Create(AOwner, self);
end;

{ TExternalPropertyEditor }

procedure TExternalPropertyEditor.HandlePaint;
var
  r: TfpgRect;
begin
//  inherited HandlePaint;
//  if not Windowed then
//    Exit;
  if widget = nil then
    Exit;
  Canvas.Clear(clBoxColor);
  Canvas.GetWinRect(r);
  Canvas.SetTextColor(clText1);
  prop.DrawValue(Widget, Canvas, r, 0);
end;

procedure TExternalPropertyEditor.CreateLayout;
begin
  inherited;
  Widget      := nil;
  Anchors     := [anTop, anLeft, anRight];

  btnEdit := TfpgButton.Create(self);
  with btnEdit do
  begin
    Height  := self.Height;
    Width   := 24;
    Top     := 0;
    Left    := self.Width - btnEdit.Width;
    Text    := '...';
    UpdatePosition;
    Anchors := [anTop, anRight];
    OnClick := @OnEditClick;
    Visible := True;
  end;
end;

procedure TExternalPropertyEditor.LoadValue(wg: TfpgWidget);
begin
  Widget := wg;
  RePaint;
end;

procedure TExternalPropertyEditor.OnEditClick(Sender: TObject);
begin
  if widget = nil then
    Exit;
  prop.OnExternalEdit(widget);
  widget.Invalidate;
  RePaint;
end;

procedure TExternalPropertyEditor.StoreValue(wg: TfpgWidget);
begin
  // nothing
end;

{ TPropertyEnum }

function TPropertyEnum.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  Result := TChoicePropertyEditor.Create(AOwner, self);
end;

function TPropertyEnum.GetValueText(wg: TfpgWidget): string;
begin
  Result := GetEnumProp(wg, Name);
end;

function TPropertyEnum.GetPropertySource(wg: TfpgWidget; const ident: string): string;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(wg.ClassType, Name);
  if PropInfo^.Default <> GetOrdProp(wg, Name) then
    Result := ident + Name + ' := ' + GetEnumProp(wg, Name) + ';' + LineEnding
  else
    Result := '';
end;

function TPropertyEnum.ParseSourceLine(wg: TfpgWidget; const line: string): boolean;
var
  s, sval: string;
begin
  s      := line;
  Result := False;
  if UpperCase(GetIdentifier(s)) <> UpperCase(Name) then
    Exit;

  Result := CheckSymbol(s, ':=');
  if Result then
  begin
    sval   := GetIdentifier(s);
    Result := CheckSymbol(s, ';');
  end;

  if Result then
    try
      SetEnumProp(wg, Name, sval);
    except
//      Writeln('invalid enum value: "' + sval + '" for ' + Name);
      Result := False;
    end;
end;

{ TChoicePropertyEditor }

procedure TChoicePropertyEditor.CreateLayout;
begin
  Anchors      := [anTop, anLeft, anRight];
  chl          := TfpgComboBox.Create(self);
  chl.SetPosition(0, 0, Width, Height);
  chl.Anchors  := Anchors;
  chl.OnChange := @UpdateProperty;
  chl.Visible := True;
end;

procedure TChoicePropertyEditor.LoadValue(wg: TfpgWidget);
var
  sv: string;
  i, fi: integer;
  sl: TStringList;
begin
  sv := GetEnumProp(wg, prop.Name);
  sl := TStringList.Create;
  GetEnumPropValueList(wg, prop.Name, sl);
  fi := 0;
  for i := 0 to sl.Count - 1 do
  begin
    chl.Items.Add(sl.Strings[i]);
    if UpperCase(sv) = UpperCase(sl.Strings[i]) then
      fi := i;
  end;
  chl.FocusItem := fi;
  sl.Free;
end;

procedure TChoicePropertyEditor.StoreValue(wg: TfpgWidget);
begin
  SetEnumProp(wg, prop.Name, chl.Text);
end;

procedure TChoicePropertyEditor.SetFocus;
begin
  chl.SetFocus;
end;

{ TBooleanPropertyEditor }

procedure TBooleanPropertyEditor.LoadValue(wg: TfpgWidget);
var
  b: integer;
begin
  b := GetOrdProp(wg, prop.Name);
  chl.Items.Add('True');
  chl.Items.Add('False');
  if b = 1 then
    chl.FocusItem := 0
  else
    chl.FocusItem := 1;
end;

procedure TBooleanPropertyEditor.StoreValue(wg: TfpgWidget);
begin
  SetOrdProp(wg, prop.Name, Ord(StrToBool(chl.Text)));
end;

{ TPropertyFontDesc }

function TPropertyFontDesc.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  Result := TExternalPropertyEditor.Create(AOwner, self);
end;

procedure TPropertyFontDesc.OnExternalEdit(wg: TfpgWidget);
var
  s: string;
begin
  s := GetStrProp(wg, Name);
  if SelectFontDialog(s) then
    SetStrProp(wg, Name, s);
end;

{ TPropertyFloat }

function TPropertyFloat.ParseSourceLine(wg: TfpgWidget; const line: string): boolean;
var
  s: string;
  ival: extended;
begin
  s      := line;
  Result := False;
  if UpperCase(GetIdentifier(s)) <> UpperCase(Name) then
    Exit;

  Result := CheckSymbol(s, ':=');
  if Result then
  begin
    ival   := GetFloatValue(s);
    Result := CheckSymbol(s, ';');
  end
  else
    ival   := 0.0;

  if Result then
    SetFloatProp(wg, Name, ival);
end;

function TPropertyFloat.GetPropertySource(wg: TfpgWidget; const ident: string): string;
begin
  Result := ident + Name + ' := ' + FloatToStr(GetFloatProp(wg, Name)) + ';' + LineEnding;
end;

function TPropertyFloat.GetValueText(wg: TfpgWidget): string;
begin
  Result := FloatToStr(GetFloatProp(wg, Name));
end;

function TPropertyFloat.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  Result := TGeneralPropertyEditor.Create(AOwner, self);
  with TGeneralPropertyEditor(Result) do
    etype := gptFloat;
end;

{ TPropertyColor }

procedure TPropertyColor.DrawValue(wg: TfpgWidget; Canvas: TfpgCanvas;
  rect: TfpgRect; flags: integer);
const
  BLOCK_SIZE = 10;  { for margin and square size }
var
  s: TfpgString;
  dx: integer;
  i: integer;
  c: TfpgColor;
begin
  inherited DrawValue(wg, Canvas, rect, flags);
  try
    s := GetValueText(wg);
  except
    on E: Exception do
      debugln('Detected an error: ', E.Message);
  end;
  dx := Canvas.Font.TextWidth(s) + BLOCK_SIZE;
  i := GetOrdProp(wg, Name);
  c := fpgColorToRGB(TfpgColor(i));
  { paint the color square }
  Canvas.Color := c;
  Canvas.FillRectangle(rect.Left+dx, rect.Top+((rect.Height-BLOCK_SIZE) div 2), BLOCK_SIZE, BLOCK_SIZE);
  { paint a block border around the square }
  Canvas.Color := clBlack;
  Canvas.DrawRectangle(rect.Left+dx, rect.Top+((rect.Height-BLOCK_SIZE) div 2), BLOCK_SIZE, BLOCK_SIZE);
end;

function TPropertyColor.ParseSourceLine(wg: TfpgWidget; const line: string): boolean;
var
  s: string;
  ival: integer;
begin
  s      := line;
  Result := False;
  if UpperCase(GetIdentifier(s)) <> UpperCase(Name) then
    Exit;

  Result := CheckSymbol(s, ':=');
  if Result then
  begin
    ival := GetColorValue(s);
    Result := CheckSymbol(s, ';');
  end;

  if Result then
    try
      SetOrdProp(wg, Name, ival);
    except
//      Writeln('invalid ordinal value: "', ival, '" for ', Name);
      Result := False;
    end;
end;

function TPropertyColor.GetPropertySource(wg: TfpgWidget; const ident: string): string;
var
  PropInfo: PPropInfo;
  i: integer;
  c: TfpgColor;
begin
  PropInfo := GetPropInfo(wg.ClassType, Name);
  i := GetOrdProp(wg, Name);
  if PropInfo^.Default <> i then
  begin
    if fpgIsNamedColor(TfpgColor(i)) then
      Result := ident + Name + ' := TfpgColor($' + IntToHex(i, 8) + ');' + LineEnding
    else
    begin
      c := fpgColorToRGB(TfpgColor(i));
      Result := ident + Name + ' := TfpgColor($' + IntToHex(c, 6) + ');' + LineEnding;
    end;
  end
  else
    Result := '';
end;

function TPropertyColor.GetValueText(wg: TfpgWidget): string;
var
  PropInfo: PPropInfo;
  i: integer;
  c: TfpgColor;
begin
  PropInfo := GetPropInfo(wg.ClassType, Name);
  i := GetOrdProp(wg, Name);
  c := fpgColorToRGB(TfpgColor(i));
  Result := '$' + IntToHex(c, 6);
end;

function TPropertyColor.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  Result := TExternalPropertyEditor.Create(AOwner, self);
end;

procedure TPropertyColor.OnExternalEdit(wg: TfpgWidget);
var
  PropInfo: PPropInfo;
  i: integer;
  c: TfpgColor;
begin
  PropInfo := GetPropInfo(wg.ClassType, Name);
  i := GetOrdProp(wg, Name);
  c := fpgColorToRGB(TfpgColor(i));
  c := fpgSelectColorDialog(c);
  SetOrdProp(wg, Name, c);
end;

end.

