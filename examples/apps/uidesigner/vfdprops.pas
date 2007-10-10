{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Property editors.
}

unit vfdprops;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  gfx_widget,
  vfdwidgetclass,
  gui_edit,
  gui_button,
  gui_combobox;

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


  TPropertyEnum = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg: TfpgWidget; const line: string): boolean; override;
    function GetPropertySource(wg: TfpgWidget; const ident: string): string; override;
    function GetValueText(wg: TfpgWidget): string; override;
    function CreateEditor(AOwner: TComponent): TVFDPropertyEditor; override;
  end;


  TPropertyStringList = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg: TfpgWidget; const line: string): boolean; override;
    function GetPropertySource(wg: TfpgWidget; const ident: string): string; override;
    function GetValueText(wg: TfpgWidget): string; override;
    function CreateEditor(AOwner: TComponent): TVFDPropertyEditor; override;
    procedure OnExternalEdit(wg: TfpgWidget); override;
  end;


  TPropertyBoolean = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg: TfpgWidget; const line: string): boolean; override;
    function GetPropertySource(wg: TfpgWidget; const ident: string): string; override;
    function GetValueText(wg: TfpgWidget): string; override;
    function CreateEditor(AOwner: TComponent): TVFDPropertyEditor; override;
  end;


  TGPEType = (gptInteger, gptString);


  TGeneralPropertyEditor = class(TVFDPropertyEditor)
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
  end;


  TChoicePropertyEditor = class(TVFDPropertyEditor)
  public
    chl: TfpgComboBox;
    procedure CreateLayout; override;
    procedure LoadValue(wg: TfpgWidget); override;
    procedure StoreValue(wg: TfpgWidget); override;
  end;
  
  
  TBooleanPropertyEditor = class(TChoicePropertyEditor)
  public
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
procedure GetEnumPropValueList(wg: TObject; const propname: string; sl: TStringList);


implementation

uses
  TypInfo,
  vfdformparser,
  vfdeditors,
  fpgfx;

procedure EditStringList(sl: TStringList);
var
  frmie: TItemEditorForm;
begin
  frmie := TItemEditorForm.Create(nil);
  //GfxGetAbsolutePosition(PropertyForm.btnEdit.WinHandle, PropertyForm.btnEdit.width, 0, ax,ay);
  //frmie.Left := ax;
  //frmie.Top := ay;

  frmie.edItems.Lines.Assign(sl);
  if frmie.ShowModal = 1 then
    sl.Assign(frmie.edItems.Lines);
  frmie.Free;
end;

procedure GetEnumPropValueList(wg: TObject; const propname: string; sl: TStringList);
var
  pi: PPropInfo;
  P: ^ShortString;
  T: PTypeData;
  n: integer;
begin
  pi := GetPropInfo(wg, propname);
{$ifdef FPC}
  T  := GetTypeData(pi^.PropType);
{$else}
  T  := GetTypeData(pi^.PropType^);
{$endif}
  P  := @T^.NameList;

  for n := 0 to T^.MaxValue do
  begin
    sl.Add(P^);
    Inc(PtrInt(P), Length(P^) + 1);
  end;
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
begin
  Result := ident + Name + ' := ' + IntToStr(GetOrdProp(wg, Name)) + ';' + LineEnding;
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

procedure TGeneralPropertyEditor.CreateLayout;
begin
  Anchors       := [anTop, anLeft, anRight];
  Edit          := TfpgEdit.Create(self);
  Edit.SetPosition(0, 0, Width, Height);
  Edit.Anchors  := Anchors;
  Edit.OnChange := @UpdateProperty;
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
    gptInteger: LoadIntValue(wg);
    else
      LoadStrValue(wg);
  end;
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

procedure TGeneralPropertyEditor.StoreValue(wg: TfpgWidget);
begin
  case etype of
    gptInteger: StoreIntValue(wg);
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

  Result := '';

  for f := 0 to sl.Count - 1 do
    Result := Result + ident + Name + '.Add(' + QuotedStr(sl.Strings[f]) + ');' + LineEnding;
end;

function TPropertyStringList.GetValueText(wg: TfpgWidget): string;
var
  sl: TStringList;
begin
  sl     := TStringList(GetObjectProp(wg, Name, TStrings));
  Result := '[' + IntToStr(sl.Count) + ' lines]';
end;

procedure TPropertyStringList.OnExternalEdit(wg: TfpgWidget);
var
  sl: TStringList;
begin
  sl := TStringList(GetObjectProp(wg, Name, TStrings));
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
begin
  i := GetOrdProp(wg, Name);
  if i = 1 then
    s := 'True'
  else
    s := 'False';
  Result := ident + Name + ' := ' + s + ';' + LineEnding;
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
  Canvas.BeginDraw;
  Canvas.Clear(clBoxColor);
  Canvas.GetWinRect(r);
  Canvas.SetTextColor(clText1);
  prop.DrawValue(Widget, Canvas, r, 0);
  Canvas.EndDraw;
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
    Left    := self.Width - Width;
    Text    := '...';
    UpdateWindowPosition;
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
begin
  Result := ident + Name + ' := ' + GetEnumProp(wg, Name) + ';' + LineEnding;
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
      Writeln('invalid enum value: "' + sval + '" for ' + Name);
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
  fi := 1;
  for i := 0 to sl.Count - 1 do
  begin
    chl.Items.Add(sl.Strings[i]);
    if UpperCase(sv) = UpperCase(sl.Strings[i]) then
      fi := i + 1;
  end;
  chl.FocusItem := fi;
  sl.Free;
end;

procedure TChoicePropertyEditor.StoreValue(wg: TfpgWidget);
begin
  SetEnumProp(wg, prop.Name, chl.Text);
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
    chl.FocusItem := 1
  else
    chl.FocusItem := 2;
end;

procedure TBooleanPropertyEditor.StoreValue(wg: TfpgWidget);
begin
  SetOrdProp(wg, prop.Name, Ord(StrToBool(chl.Text)));
end;

end.

