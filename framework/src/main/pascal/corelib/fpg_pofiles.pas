{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2015 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Methods and classes for loading translations/localizations from po files.


  Example 1: Load a specific .po file:

    procedure TForm1.FormCreate(Sender: TObject);
    var
      PODirectory: String;
    begin
      PODirectory:='/path/to/languages/';
      TranslateUnitResourceStrings('StrConsts',PODirectory+'gui.%s.po',
                                   'nl','');
      MessageDlg('Title','Text',mtInformation,[mbOk,mbCancel,mbYes],0);
    end;


  Example 2: Load the current language file using the GetLanguageIDs function
    of the gettext unit:

    procedure TForm1.FormCreate(Sender: TObject);
    var
      PODirectory, Lang, FallbackLang: String;
    begin
      PODirectory:='/path/to/languages/';
      GetLanguageIDs(Lang,FallbackLang); // in unit gettext
      TranslateUnitResourceStrings('StrConsts',PODirectory+'gui.%s.po',
                                   Lang,FallbackLang);
      MessageDlg('Title','Text',mtInformation,[mbOk,mbCancel,mbYes],0);
    end;
}

unit fpg_pofiles;

{$I fpg_defines.inc}
{$INLINE ON}

interface

uses
  Classes,
  SysUtils,
  contnrs;

type
  TPOFileItem = class(TObject)
  public
    Identifier: string;
    Original: string;
    Translation: string;
    constructor Create(const TheIdentifier, TheOriginal, TheTranslated: string);
  end;


  TPOFile = class(TObject)
  protected
    FItems: TFPList;  // list of TPOFileItem
    FIdentifierToItem: TFPDataHashTable;  // case-insensitive (keys lowercased)
    FOriginalToItem: TFPDataHashTable;    // case-sensitive
    FKnownUnits: TStringList;             // unit prefixes from PO identifiers
  public
    constructor Create(const AFilename: string);
    constructor Create(AStream: TStream);
    destructor  Destroy; override;
    procedure   ReadPOText(const s: string);
    procedure   Add(const Identifier, OriginalValue, TranslatedValue: string);
    function    Translate(const Identifier, OriginalValue: string): string;
    { Translates by identifier first. If not found, falls back to
      original-value matching only if the resource string belongs to a unit
      that has entries in this PO file. Returns empty string if no match,
      so SetResourceStrings leaves the string unchanged. }
    function    TranslateScoped(const Identifier, OriginalValue: string): string;
    procedure   AppendFile(const AFilename: string);
  end;


  EPOFileError = class(Exception);


var
  SystemCharSetIsUTF8: Boolean = True;// the fpGUI interfaces expect UTF-8 as default
 // if you don't use UTF-8, install a proper widestring manager and set this
 // to false. You're on your own then!


// translate resource strings for one unit
procedure TranslateUnitResourceStrings(const ResUnitName, BaseFilename, Lang, FallbackLang: string);
function  TranslateUnitResourceStrings(const ResUnitName, AFilename: string): boolean;
function  UTF8ToSystemCharSet(const s: string): string; {$ifndef MultiLocale} inline;{$endif}


implementation

uses
  fpg_main,
  fpg_stringutils,
  fpg_utils;


function UTF8ToSystemCharSet(const s: string): string; {$ifndef MultiLocale} inline;
{$endif}
begin
  if SystemCharSetIsUTF8 then
    Exit(s);
  {$IFDEF NoUTF8Translations}
  Result := s;
  {$ELSE}
    {$IFNDEF MultiLocale}
  Result := Utf8ToAnsi(s);
    {$ELSE}
  try
    if (LowerCase(GetDefaultCodepage) <> 'utf8') and (LowerCase(GetDefaultCodepage) <> 'utf-8') then
      Result := CPConvert(s, 'utf8', LowerCase(GetDefaultCodepage))
    else
      Result := s;
  except
    Result := s;
  end;
    {$ENDIF}
  {$ENDIF}
end;

function Translate(Name, Value: ansistring; Hash: longint; arg: Pointer): ansistring;
var
  po: TPOFile;
begin
  po := TPOFile(arg);
  // Use scoped translation: matches by identifier first, then falls back to
  // original-value matching only for units that have entries in the PO file.
  // Returns empty string for non-matches so SetResourceStrings skips them.
  Result := po.TranslateScoped(Name, Value);
  // convert UTF8 to current locale
  if Result <> '' then
    Result := UTF8ToSystemCharSet(Result);
end;

function TranslateUnitResourceStrings(const ResUnitName, AFilename: string): boolean;
var
  po: TPOFile;
  lPath, lFile: string;
  lPos: integer;
  ToolkitOnly: Boolean;
begin
  Result := False;
  ToolkitOnly := False;

  // build correct filename for fpGUI Toolkit translations.
  lPath := fpgExtractFilePath(AFilename);
  lFile := fpgExtractFileName(AFilename);
  lPos := Pos('.', lFile);
  lFile := lPath + 'fpgui' + Copy(lFile, lPos, Length(lFile)-lPos+1);

  if (AFilename = '') or (not fpgFileExists(AFilename)) then
    ToolkitOnly := True;  // we don't have a application translation file
  try
    po := nil;
    // read .po file
    if ToolkitOnly then
    begin
      if not fpgFileExists(lFile) then
        Exit;
      po := TPOFile.Create(nil);
      po.AppendFile(lFile);
    end
    else
    begin
      po := TPOFile.Create(AFilename);
      // Now append fpGUI translations
      po.AppendFile(lFile);
    end;
    try
      SetResourceStrings(@Translate, po);
    finally
      po.Free;
    end;
    Result := True;
  except
    on e: Exception do
    begin
      DebugLn('Exception while translating ', ResUnitName);
      DebugLn(e.Message);
      DumpStack;
    end;
  end;
end;

procedure TranslateUnitResourceStrings(const ResUnitName, BaseFilename, Lang, FallbackLang: string);
begin
  if (BaseFilename = '') then
    Exit;

  if (FallbackLang <> '') then
  begin
    TranslateUnitResourceStrings(ResUnitName, Format(BaseFilename, [FallbackLang]));
  end;
  if (Lang <> '') then
    TranslateUnitResourceStrings(ResUnitName, Format(BaseFilename, [Lang]));
end;

{ TPOFile }

constructor TPOFile.Create(const AFilename: string);
var
  f: TStream;
begin
  f := nil;
  if AFilename <> '' then
    f := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    Self.Create(f);
  finally
    f.Free;
  end;
end;

constructor TPOFile.Create(AStream: TStream);
var
  Size: integer;
  s: string;
begin
  inherited Create;
  FItems            := TFPList.Create;
  FIdentifierToItem := TFPDataHashTable.Create;  // case-insensitive via LowerCase
  FOriginalToItem   := TFPDataHashTable.Create;  // case-sensitive
  FKnownUnits       := TStringList.Create;
  FKnownUnits.Sorted := True;
  FKnownUnits.Duplicates := dupIgnore;

  if AStream = nil then
    Exit;

  Size := AStream.Size - AStream.Position;
  if Size <= 0 then
    Exit; //==>
  SetLength(s, Size);
  AStream.Read(s[1], Size);
  ReadPOText(s);
end;

destructor TPOFile.Destroy;
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do
    TObject(FItems[i]).Free;
  FItems.Free;
  FIdentifierToItem.Free;
  FOriginalToItem.Free;
  FKnownUnits.Free;
  inherited Destroy;
end;

procedure TPOFile.ReadPOText(const s: string);
{ Read a .po file. Structure:

Example
#: lazarusidestrconsts:lisdonotshowsplashscreen
msgid "                      Do not show splash screen"
msgstr ""

}
const
  sCommentIdentifier: PChar = '#: ';
  sMsgID: PChar  = 'msgid "';
  sMsgStr: PChar = 'msgstr "';
var
  l: integer;
  LineLen: integer;
  p: PChar;
  LineStart: PChar;
  LineEnd: PChar;
  Identifier: string;
  MsgID: string;
  MsgStr: string;
  TextEnd: PChar;
begin
  if s = '' then
    Exit; //==>
  l         := Length(s);
  p         := PChar(s);
  LineStart := p;
  TextEnd   := p + l;
  while LineStart < TextEnd do
  begin
    LineEnd := LineStart;
    while (not (LineEnd^ in [#0, #10, #13])) do
      Inc(LineEnd);
    LineLen := LineEnd - LineStart;
    if LineLen > 0 then
      if CompareMem(LineStart, sCommentIdentifier, 3) then
        Identifier := copy(s, LineStart - p + 4, LineLen - 3)
      else if CompareMem(LineStart, sMsgID, 7) then
        MsgID      := UTF8CStringToUTF8String(LineStart + 7, LineLen - 8)
      else if CompareMem(LineStart, sMsgStr, 8) then
      begin
        //MsgStr:=copy(s,LineStart-p+9,LineLen-9);
        MsgStr := UTF8CStringToUTF8String(LineStart + 8, LineLen - 9);
        Add(Identifier, MsgID, MsgStr);
      end;
    LineStart := LineEnd + 1;
    while (LineStart < TextEnd) and (LineStart^ in [#10, #13]) do
      Inc(LineStart);
  end;
end;

procedure TPOFile.Add(const Identifier, OriginalValue, TranslatedValue: string);
var
  Item: TPOFileItem;
  lColonPos: integer;
  lUnitPrefix: string;
begin
  if (TranslatedValue = '') then
    Exit; //==>
  Item := TPOFileItem.Create(Identifier, OriginalValue, TranslatedValue);
  FItems.Add(Item);
  FIdentifierToItem.Add(LowerCase(Identifier), Item);  // case-insensitive
  FOriginalToItem.Add(OriginalValue, Item);            // case-sensitive
  // Track unit prefix for scoped original-value fallback
  lColonPos := Pos(':', Identifier);
  if lColonPos > 1 then
  begin
    lUnitPrefix := LowerCase(Copy(Identifier, 1, lColonPos - 1));
    FKnownUnits.Add(lUnitPrefix);
  end;
end;

function TPOFile.Translate(const Identifier, OriginalValue: string): string;
var
  Item: TPOFileItem;
  s: string;
begin
  s := StringReplace(Identifier, '.', ':', []);
  Item := TPOFileItem(FIdentifierToItem[LowerCase(s)]);  // case-insensitive
  if Item = nil then
  begin
    Item := TPOFileItem(FOriginalToItem[OriginalValue]);  // case-sensitive
  end;
  if Item <> nil then
  begin
    Result := Item.Translation;
    if Result = '' then
      raise Exception.Create('TPOFile.Translate Inconsistency');
  end
  else
    Result := OriginalValue;
end;

function TPOFile.TranslateScoped(const Identifier, OriginalValue: string): string;
var
  Item: TPOFileItem;
  s: string;
  lDotPos: integer;
  lUnitPrefix: string;
begin
  Result := '';
  s := StringReplace(Identifier, '.', ':', []);

  // Primary match: by identifier (always safe and precise)
  Item := TPOFileItem(FIdentifierToItem[LowerCase(s)]);

  // Scoped fallback: only try original-value matching if the resource string
  // belongs to a unit that has entries in this PO file. This prevents
  // inadvertently translating strings from unrelated libraries (e.g.,
  // BGRABitmap, FPC RTL) that happen to share the same original text.
  if Item = nil then
  begin
    lDotPos := Pos(':', s);
    if lDotPos > 1 then
    begin
      lUnitPrefix := LowerCase(Copy(s, 1, lDotPos - 1));
      if FKnownUnits.IndexOf(lUnitPrefix) >= 0 then
        Item := TPOFileItem(FOriginalToItem[OriginalValue]);
    end;
  end;

  if (Item <> nil) and (Item.Translation <> '') then
    Result := Item.Translation;
end;

procedure TPOFile.AppendFile(const AFilename: string);
var
  Size: integer;
  s: string;
  f: TStream;
begin
  if (AFilename = '') or (not fpgFileExists(AFilename)) then
    Exit;
  f := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    s := '';
    Size := f.Size - f.Position;
    if Size <= 0 then
      Exit; //==>
    SetLength(s, Size);
    f.Read(s[1], Size);
    ReadPOText(s);
  finally
    f.Free;
  end;
end;

{ TPOFileItem }

constructor TPOFileItem.Create(const TheIdentifier, TheOriginal, TheTranslated: string);
begin
  Identifier  := TheIdentifier;
  Original    := TheOriginal;
  Translation := TheTranslated;
end;

end.

