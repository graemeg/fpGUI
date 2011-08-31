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

{$mode objfpc}{$H+}{$INLINE ON}

{.$Define DEBUG}

interface

uses
  Classes,
  SysUtils,
  fpg_stringhashlist;

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
    FIdentifierToItem: TStringHashList;
    FOriginalToItem: TStringHashList;
  public
    constructor Create(const AFilename: string);
    constructor Create(AStream: TStream);
    destructor  Destroy; override;
    procedure   ReadPOText(const s: string);
    procedure   Add(const Identifier, OriginalValue, TranslatedValue: string);
    function    Translate(const Identifier, OriginalValue: string): string;
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

{$ifndef ver2_0}
function Translate(Name, Value: ansistring; Hash: longint; arg: Pointer): ansistring;
var
  po: TPOFile;
begin
  po     := TPOFile(arg);
  // get UTF8 string
  Result := po.Translate(Name, Value);
  // convert UTF8 to current local
  if Result <> '' then
    Result := UTF8ToSystemCharSet(Result);
end;

{$endif ver2_0}

function TranslateUnitResourceStrings(const ResUnitName, AFilename: string): boolean;
var
{$ifdef ver2_0}
  TableID, StringID, TableCount: integer;
  s: string;
  DefValue: string;
{$endif ver2_0}
  po: TPOFile;
  lPath, lFile: string;
  lPos: integer;
  ToolkitOnly: Boolean;
begin
  {$IFDEF DEBUG}
  writeln('TranslateUnitResourceStrings:');
  {$ENDIF}
  Result := False;
  ToolkitOnly := False;

  // build correct filename for fpGUI Toolkit translations.
  lPath := fpgExtractFilePath(AFilename);
  lFile := fpgExtractFileName(AFilename);
  lPos := Pos('.', lFile);
  lFile := lPath + 'fpgui' + Copy(lFile, lPos, Length(lFile)-lPos+1);
  {$IFDEF DEBUG}
  writeln('  lFile = ', lFile);
  writeln('  ResUnitName="', ResUnitName, '"');
  writeln('  AFilename="', AFilename, '"');
  {$ENDIF}

  if {(ResUnitName = '') or} (AFilename = '') or (not fpgFileExists(AFilename)) then
    ToolkitOnly := True;  // we don't have a application translation file
  try
    po := nil;
    // read .po file
    if ToolkitOnly then
    begin
      if not fpgFileExists(lFile) then
        Exit;
      {$IFDEF DEBUG}
      writeln('  ************  Only translating the toolkit   ***********');
      {$ENDIF}
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
{$ifdef ver2_0}
      for TableID := 0 to ResourceStringTableCount - 1 do
      begin
        TableCount := ResourceStringCount(TableID);

        // check if this table belongs to the ResUnitName
        if TableCount = 0 then
          continue;
        s := GetResourceStringName(TableID, 0);
        if CompareText(ResUnitName + '.', LeftStr(s, length(ResUnitName) + 1)) <> 0 then
          continue;

        // translate all resource strings of the unit
        for StringID := 0 to TableCount - 1 do
        begin
          DefValue := GetResourceStringDefaultValue(TableID, StringID);
          // get UTF8 string
          s        := po.Translate(GetResourceStringName(TableID, StringID), DefValue);

          if Length(s) > 0 then
          begin
            // convert UTF8 to current local
            s := UTF8ToSystemCharSet(s);
            SetResourceStringValue(TableID, StringID, s);
          end;
        end;
      end;
{$else ver2_0}
//      SetUnitResourceStrings(ResUnitName, @Translate, po);
      SetResourceStrings(@Translate, po);
{$endif ver2_0}
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
  if {(ResUnitName = '') or} (BaseFilename = '') then
    Exit;

  if (FallbackLang <> '') then
  begin
    {$IFDEF DEBUG}
    writeln('1) Trying fallback language... ', Fallbacklang);
    {$ENDIF}
    TranslateUnitResourceStrings(ResUnitName, Format(BaseFilename, [FallbackLang]));
  end;
  if (Lang <> '') then
  begin
    {$IFDEF DEBUG}
    writeln('2) Trying language... ', Lang);
    {$ENDIF}
    TranslateUnitResourceStrings(ResUnitName, Format(BaseFilename, [Lang]));
  end;
end;

{ TPOFile }

constructor TPOFile.Create(const AFilename: string);
var
  f: TStream;
begin
  if AFilename <> '' then
    f := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    Self.Create(f);
  finally
    if Assigned(f) then
      f.Free;
  end;
end;

constructor TPOFile.Create(AStream: TStream);
var
  Size: integer;
  s: string;
begin
  inherited Create;
  FItems          := TFPList.Create;
  FIdentifierToItem := TStringHashList.Create(False);
  FOriginalToItem := TStringHashList.Create(True);
  
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
begin
  {$IFDEF DEBUG}
  writeln('TPOFile.Add: ' + Identifier + ' | ' + OriginalValue + ' | ' + TranslatedValue);
  {$ENDIF}
  if (TranslatedValue = '') then
    Exit; //==>
  Item := TPOFileItem.Create(Identifier, OriginalValue, TranslatedValue);
  FItems.Add(Item);
  FIdentifierToItem.Add(Identifier, Item);
  FOriginalToItem.Add(OriginalValue, Item);
end;

function TPOFile.Translate(const Identifier, OriginalValue: string): string;
var
  Item: TPOFileItem;
  s: string;
begin
  s := StringReplace(Identifier, '.', ':', []);
  {$IFDEF DEBUG}
  writeln('TPOFile.Translate: ' + s + '(' + Identifier + ') | ' + OriginalValue);
  {$ENDIF}
  Item := TPOFileItem(FIdentifierToItem.Data[s]);
  if Item = nil then
  begin
    {$IFDEF DEBUG}
    writeln('  identifier lookup failed, trying original value');
    {$ENDIF}
    Item := TPOFileItem(FOriginalToItem.Data[OriginalValue]);
  end;
  if Item <> nil then
  begin
    Result := Item.Translation;
    if Result = '' then
      raise Exception.Create('TPOFile.Translate Inconsistency');
  end
  else
  begin
    {$IFDEF DEBUG}
    writeln('  OriginalValue lookup failed, defaulting to original value');
    {$ENDIF}
    Result := OriginalValue;
  end;
end;

procedure TPOFile.AppendFile(const AFilename: string);
var
  Size: integer;
  s: string;
  f: TStream;
begin
  // Now fpGUI translation
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

