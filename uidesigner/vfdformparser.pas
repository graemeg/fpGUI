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
      This unit contains the Form Parser - used when loading a new
      form .pas file.
}

unit vfdformparser;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_form,
  vfddesigner,
  vfdwidgetclass,
  vfdwidgets;

type
  TVFDFormParser = class(TObject)
  private
    ffd: TFormDesigner;
    fformname: string;
    BodyLines: TStringList;
    eob: boolean;
    line: string;
    lineindex: integer;
    procedure   ParseFormProperties;
    procedure   ParseFormWidgets;
    procedure   NextLine;
    function    ReadWGProperty(propline: string; wg: TfpgWidget; wgc: TVFDWidgetClass): boolean;
  public
    constructor Create(const FormName, FormHead, FormBody: string);
    destructor  Destroy; override;
    function    ParseForm: TFormDesigner;
  end;


function  GetIdentifier(var s: string): string;
function  GetStringValue(var s: string): string;
procedure SkipSpaces(var s: string);
function  CheckSymbol(var s: string; const sym: string): boolean;
function  GetIntValue(var s: string): integer;
function  GetBoolValue(var s: string): boolean;
function  GetFloatValue(var s: string): extended;
function  GetColorValue(var s: string): integer;


implementation


{ TVFDFormParser }

constructor TVFDFormParser.Create(const FormName, FormHead, FormBody: string);
begin
  fformname := FormName;
  ffd       := nil;
  BodyLines := TStringList.Create;
  BodyLines.Text := FormBody;
  lineindex := -1;
end;

destructor TVFDFormParser.Destroy;
begin
  BodyLines.Free;
  inherited;
end;

procedure TVFDFormParser.NextLine;
begin
  repeat
    Inc(lineindex);
    eob := (lineindex > BodyLines.Count-1);
    if not eob then
      line := trim(bodylines.Strings[lineindex])
    else
      line := '';
  until eob or (line <> '');
end;

function TVFDFormParser.ParseForm: TFormDesigner;
begin
  ffd           := TFormDesigner.Create;
  ffd.Form.Name := fformname;
  // parsing line by line
  // the unknown lines will be "other properties"
  lineindex := -1;
  NextLine;
  ParseFormProperties;
  ParseFormWidgets;
  Result := ffd;
end;

procedure SkipSpaces(var s: string);
begin
  while (s <> '') and (s[1] in [' ', #9, #13, #10]) do
    Delete(s, 1, 1);
end;

function CheckSymbol(var s: string; const sym: string): boolean;
begin
  SkipSpaces(s);
  Result := (pos(sym, s) = 1);
  if Result then
    Delete(s, 1, length(sym));
end;

function GetIntValue(var s: string): integer;
var
  n: integer;
  ns: string;
begin
  SkipSpaces(s);
  ns := '';
  n  := 1;
  while (n <= length(s)) and (s[n] in ['0'..'9', '-']) do
  begin
    ns := ns + s[n];
    Inc(n);
  end;
  Result := StrToIntDef(ns, 0);
  Delete(s, 1, length(ns));
end;

function GetBoolValue(var s: string): boolean;
var
  ts: string;
  fs: string;
begin
  SkipSpaces(s);
  ts := copy(s, 1, 4);    // true string
  fs := copy(s, 1, 5);    // false string
  if UpperCase(ts) = 'TRUE' then
    Result := True
  else if UpperCase(fs) = 'FALSE' then
    Result := False
  else
    raise exception.Create('Failed to parse Boolean value <' + s + '>');
    
  if Result then
    Delete(s, 1, 4)
  else
    Delete(s, 1, 5);
end;

function GetFloatValue(var s: string): extended;
var
  n: integer;
  ns: string;
begin
  SkipSpaces(s);
  ns := '';
  n  := 1;
  while (n <= length(s)) and (s[n] in ['0'..'9', '-', '.']) do
  begin
    ns := ns + s[n];
    Inc(n);
  end;
  Result := StrToFloatDef(ns, 0.0);
  Delete(s, 1, length(ns));
end;

function GetStringValue(var s: string): string;
var
  n: integer;
  quot: boolean;
  c, prevc: char;
  ccode: string;
//  ids: string;
begin
  Result := '';
//  ids    := GetIdentifier(s);
//  if ids <> '' then
{    if ids = 'u8' then
    begin
      if not CheckSymbol(s, '(') then
        Exit;
    end
    else
      Exit;
}
  SkipSpaces(s);
  prevc := #0;
  n     := 1;
  quot  := False;
  while n <= length(s) do
  begin
    c := s[n];
    if c = '''' then
    begin
      quot := not quot;
      if quot and (prevc = '''') then
        Result := Result + c;
    end
    else if not quot then
    begin
      if not (c in ['+', ' ', #9, #13, #10]) then
        if (c = '#') then
        begin
          Inc(n);
          ccode := '';
          while (n <= length(s)) and (s[n] in ['0'..'9']) do
          begin
            ccode := ccode + s[n];
            Inc(n);
          end;
          c      := chr(StrToIntDef(ccode, Ord('?')) and $FF);
          Result := Result + c;
        end
        else
          break;
    end
    else
      Result := Result + c;
    prevc := c;
    Inc(n);
  end;
  if (n - 1) > 0 then
    Delete(s, 1, n - 1);

  SkipSpaces(s);
//  if ids <> '' then
//    CheckSymbol(s, ')');
//  if ids = 'u8' then
//    Result := u8(Result);
end;

function GetColorValue(var s: string): integer;
var
  n: integer;
  ns: string;
begin
  SkipSpaces(s);
  GetIdentifier(s);    // extract 'TfpgColor' identifier
  CheckSymbol(s, '(');
  SkipSpaces(s);
  ns := '';
  n  := 1;
  while (n <= length(s)) and (s[n] in ['$','0'..'9','a'..'f','A'..'F']) do
  begin
    ns := ns + s[n];
    Inc(n);
  end;
  Result := StrToIntDef(ns, clWindowBackground);
  Delete(s, 1, length(ns));
  SkipSpaces(s);
  CheckSymbol(s, ')');
end;

function GetIdentifier(var s: string): string;
var
  n: integer;
begin
  SkipSpaces(s);
  Result := '';
  n      := 1;
  while n <= length(s) do
  begin
    if s[n] in ['_', 'a'..'z', 'A'..'Z', '0'..'9'] then
      Result := Result + s[n]
    else
      Break;
    Inc(n);
  end;
  if length(Result) > 0 then
    Delete(s, 1, length(Result));
end;

procedure TVFDFormParser.ParseFormProperties;
var
  lok: boolean;
begin
  while not eob and (pos('.CREATE(', UpperCase(line)) = 0) do
  begin
    lok := ReadWGProperty(line, ffd.Form, VFDFormWidget);
    if not lok then
      ffd.FormOther := ffd.FormOther + line + LineEnding;
    NextLine;
  end;
end;

procedure TVFDFormParser.ParseFormWidgets;
var
  n: integer;
  lok: boolean;
  s: string;
  ident: string;
  wgname, wgclass, wgclassuc, wgparent: string;
  pwg, wg: TfpgWidget;
  wgother: string;
  wd: TWidgetDesigner;
  wgc: TVFDWidgetClass;
begin
  while not eob do
  begin
    //s := UpperCase(line);
    s := line;

    wgname := GetIdentifier(s);
    //writeln('wg: ',wgname);
    lok    := CheckSymbol(s, ':=');
    if lok then
      wgclass := GetIdentifier(s);
    lok := lok and CheckSymbol(s, '.');
    lok := lok and (UpperCase(GetIdentifier(s)) = 'CREATE');
    lok := lok and CheckSymbol(s, '(');
    if lok then
      wgparent := GetIdentifier(s);
    lok := lok and CheckSymbol(s, ')');
    lok := lok and CheckSymbol(s, ';');

    if lok then
    begin
//      writeln('wg create: ',wgname,' (',wgclass,') - ',wgparent);

      // searching for the parent ...
      pwg := nil;
      if UpperCase(wgparent) <> 'SELF' then
      begin
        pwg := ffd.FindWidgetByName(wgparent);
        if pwg = nil then
          DebugLn('Warning! Parent object "' + wgparent + '" not found for "' + wgname + '"');
      end;
      if pwg = nil then
        pwg := ffd.Form;

      wgclassuc := UpperCase(wgclass);

      wg  := nil;
      wgc := nil;
      for n := 0 to VFDWidgetCount-1 do
      begin
        wgc := VFDWidget(n);
        if wgclassuc = UpperCase(wgc.WidgetClass.ClassName) then
        begin
          wg := wgc.CreateWidget(pwg);
          break;
        end;
      end;

      if wg = nil then
      begin
        wgc := VFDOtherWidget;
        wg  := TOtherWidget.Create(pwg);
        TOtherWidget(wg).wgClassName := wgclass;
      end;

      wg.Name := wgname;
      wg.FormDesigner := ffd;
      
      NextLine;
      s     := UpperCase(line);
      ident := GetIdentifier(s);
      if ident = 'WITH' then
      begin
        // skip with line...
        NextLine;

        s     := UpperCase(line);
        ident := GetIdentifier(s);
        if ident = 'BEGIN' then
          NextLine;

        // reading widget properties...
        wgother := '';

        while (not eob) and (pos('END;', UpperCase(line)) <> 1) do
        begin
          lok := ReadWGProperty(line, wg, wgc);
          if not lok then
            wgother := wgother + line + LineEnding;
          nextline;
        end;

        if (pos('END;', UpperCase(line)) = 1) then
          nextline;

      end;

      wd           := ffd.AddWidget(wg, nil);
      wd.FVFDClass := wgc;
      wd.other.Text := wgother;

    end
    else
    begin
      ffd.FormOther := ffd.FormOther + line + LineEnding;
      NextLine;
    end;

  end;
end;

function TVFDFormParser.ReadWGProperty(propline: string; wg: TfpgWidget; wgc: TVFDWidgetClass): boolean;
var
  s: string;
  n: integer;
  ident: string;
  lok: boolean;
  sval: string;
  wga: TAnchors;
begin
  s := propline;

  ident := UpperCase(GetIdentifier(s));
  //writeln('ident: ',ident);
  sval  := '';

  lok := False;

  if ident = 'NAME' then
  begin
    lok := CheckSymbol(s, ':=');
    if lok then
    begin
      sval := GetStringValue(s);
      lok  := CheckSymbol(s, ';');
    end;
    if lok then
      wg.Name := sval;
  end
  else if ident = 'ANCHORS' then
  begin
    lok := CheckSymbol(s, ':=');
    lok := lok and CheckSymbol(s, '[');
    if lok then
    begin
      wga := [];
      repeat
        sval := UpperCase(GetIdentifier(s));
        if sval = 'ANLEFT' then
          wga := wga + [anLeft]
        else if sval = 'ANTOP' then
          wga := wga + [anTop]
        else if sval = 'ANRIGHT' then
          wga := wga + [anRight]
        else if sval = 'ANBOTTOM' then
          wga := wga + [anBottom];
      until not CheckSymbol(s, ',');
    end;
    lok := lok and CheckSymbol(s, ']');
    lok := lok and CheckSymbol(s, ';');

    if lok then
      wg.Anchors := wga;
  end
  else if ident = 'WINDOWTITLE' then
  begin
    lok := (wg is TfpgForm);
    if lok then
    begin
      lok := CheckSymbol(s, ':=');
      if lok then
      begin
        sval := GetStringValue(s);
        lok  := CheckSymbol(s, ';');
      end;
      if lok then
        TfpgForm(wg).WindowTitle := sval;
    end;
  end
  else if ident = 'SETPOSITION' then
  begin
    lok := CheckSymbol(s, '(');
    if lok then
      wg.Left := GetIntValue(s);
    lok := lok and CheckSymbol(s, ',');
    if lok then
      wg.Top := GetIntValue(s);
    lok := lok and CheckSymbol(s, ',');
    if lok then
      wg.Width := GetIntValue(s);
    lok := lok and CheckSymbol(s, ',');
    if lok then
      wg.Height := GetIntValue(s);
    lok := lok and CheckSymbol(s, ')');
    lok := lok and CheckSymbol(s, ';');
    wg.UpdateWindowPosition;
    //if lok then Writeln('sd ok.');
    //writeln('WT: ',sval);
  end;

  if not lok then
    if wgc <> nil then
      for n := 0 to wgc.PropertyCount-1 do
      begin
        lok := wgc.GetProperty(n).ParseSourceLine(wg, line);
        if lok then
          Break;
      end;

  if not lok then
    DebugLn('unknown: ', line);

  Result := lok;
end;

end.

