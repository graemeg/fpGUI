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
      Property editors.
}

unit vfdprops;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget,
  vfdwidgetclass,
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
  vfdformparser,
  vfdeditors,
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
    raise Exception.Create('Failed to find TStrings type property.');

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
    raise Exception.Create('Failed to find TStrings type property.');
  Result := '[' + IntToStr(sl.Count) + ' lines]';
end;

procedure TPropertyStringList.OnExternalEdit(wg: TfpgWidget);
var
  sl: TStringList;
begin
  sl := TStringList(GetObjectProp(wg, Name, TStrings));
  if not Assigned(sl) then
    raise Exception.Create('Failed to find TStrings type property.');
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
      raise Exception.Create('Failed to find TStrings type property.');
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
  nc: TfpgColor;
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

