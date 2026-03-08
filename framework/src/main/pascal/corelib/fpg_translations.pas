{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2025 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Methods and classes for loading the translations/localizations.
}

unit fpg_translations;

{$I fpg_defines.inc}

interface

uses
  Classes
  ,SysUtils
  ,fpg_base
  ;


type

  TTranslation = class(TObject)
  private
    FID: string;
  public
    property ID: string read FID;
  end;
  PTranslation = ^TTranslation;


  TTranslationList = class(TObject)
  private
    FCount: integer;
    FItems: PTranslation;
    function    GetItems(Index: integer): TTranslation;
  public
    destructor  Destroy; override;
    function    IndexOf(const ID: string): integer;
    procedure   Add(const ID: string);
    procedure   Clear;
  public
    property    Count: integer read FCount;
    property    Items[Index: integer]: TTranslation read GetItems; default;
  end;


procedure TranslateResourceStrings(const BaseAppName, BaseDirectory, CustomLang: string);
function  fpgMatchLocale(const ALanguageID: TfpgString): boolean;


implementation

uses
  GetText
  ,fpg_pofiles
  ,fpg_utils
  ;


var
  TranslationList: TTranslationList;
  SystemLanguageID1: string = '';
  SystemLanguageID2: string = '';


procedure SearchForPOfiles(const BaseAppName, SearchDir: string);
var
  FileInfo: TSearchRec;
  ID: string;
  SearchMask: string;
begin
  {$IFDEF GDEBUG}
  writeln('Searching for PO files in ' + SearchDir);
  {$ENDIF}
  SearchMask := fpgAddTrailingValue(SearchDir, PathDelim, false) + BaseAppName + '.*.po';
  if SysUtils.FindFirst(SearchMask, faAnyFile, FileInfo) = 0
  then begin
    repeat
      if (FileInfo.Name = '.') or (FileInfo.Name = '..') or (FileInfo.Name = '') then
        Continue;
      ID := copy(FileInfo.Name,length(BaseAppName + '.')+1,
               length(FileInfo.Name)-length(BaseAppName + '..po'));
      if (ID <> '') and (Pos('.',ID) < 1) and (TranslationList.IndexOf(ID) < 0) then
      begin
        TranslationList.Add(ID);
      end;
    until SysUtils.FindNext(FileInfo) <> 0;
  end;
  SysUtils.FindClose(FileInfo);
end;

procedure CollectTranslations(const BaseAppName, BaseDir: string);
var
  LanguagesDir: string;
begin
  // search for all <BaseAppName>.xxx.po files
  if TranslationList = nil then
    TranslationList := TTranslationList.Create
  else
    TranslationList.Clear;
  // add automatic and english translation
  TranslationList.Add('');
  TranslationList.Add('en');

  // Search in 'languages' sub-directory for cleaner project layouts.
  LanguagesDir := fpgAddTrailingValue(BaseDir, PathDelim, false) + 'languages';
  if fpgDirectoryExists(LanguagesDir) then
    SearchForPOfiles(BaseAppName, LanguagesDir);

  // If no translations were found in the 'languages' directory, then search
  // in the application's root directory for backward compatibility.
  if TranslationList.Count < 3 then // default has 2 ('', 'en')
    SearchForPOfiles(BaseAppName, BaseDir);
end;

procedure TranslateResourceStrings(const BaseAppName, BaseDirectory, CustomLang: string);
const
  Ext = '.%s.po';
var
  Lang: string;
  FallbackLang: string;
  Dir: string;
  LangDir: string;
  POFile: string;
begin
  {$IFDEF GDEBUG}
  writeln('BaseAppName = ',BaseAppName);
  writeln('BaseDirectory = ',BaseDirectory);
  writeln('CustomLang = ',CustomLang);
  {$ENDIF}
  if TranslationList = nil then
    CollectTranslations(BaseAppName, BaseDirectory);

  if CustomLang = '' then
  begin
    Lang := SystemLanguageID1;
    FallbackLang := SystemLanguageID2;
  end
  else
  begin
    Lang := CustomLang;
    FallbackLang := '';
  end;

  Dir := fpgAddTrailingValue(BaseDirectory, PathDelim, false);
  LangDir := Dir + 'languages' + PathDelim;

  // First check for languages directory
  if fpgDirectoryExists(LangDir) then
    POFile := LangDir + BaseAppName + Ext
  else
    POFile := Dir + BaseAppName + Ext;

  {$IFDEF GDEBUG}
  writeln('Lang = ' + Lang);
  writeln('SystemLanguageID1 = ' + SystemLanguageID1);
  writeln('SystemLanguageID2 = ' + SystemLanguageID2);
  writeln('Translation file = ' + POFile);
  {$ENDIF}

  // We use one translation file for all fpGUI Toolkit related text and one
  // translation file for all fpGUI based application text
  TranslateUnitResourceStrings('fpgui', POFile, Lang, FallbackLang);
end;

// Strip the '.' onwards part. eg: en_ZA.UTF-8  ->  en_ZA
procedure FixLanguageIDs(var ALanguageID: TfpgString);
var
  lpos: integer;
begin
  lpos := Pos('.', ALanguageID);
  if lpos > 0 then
    ALanguageID := Copy(ALanguageID, 0, lpos-1);
end;

function fpgMatchLocale(const ALanguageID: TfpgString): boolean;
var
  s: TfpgString;
begin
  s := ALanguageID;
  FixLanguageIDs(s);
  Result := s = SystemLanguageID1;
  if not Result then
    Result := s = SystemLanguageID2;
end;


{ TTranslationList }

function TTranslationList.GetItems(Index: integer): TTranslation;
begin
  Result := FItems[Index];
end;

destructor TTranslationList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TTranslationList.Add(const ID: string);
var
  NewTranslation: TTranslation;
begin
  if IndexOf(ID)>=0 then
    raise Exception.Create('TTranslationList.Add '
        + 'ID="' + ID + '" already exists.');
  NewTranslation := TTranslation.Create;
  NewTranslation.FID := ID;
  inc(FCount);
  ReallocMem(FItems, SizeOf(Pointer)*FCount);
  FItems[FCount-1] := NewTranslation;
end;

function TTranslationList.IndexOf(const ID: string): integer;
begin
  Result := FCount - 1;
  while (Result >= 0) and (CompareText(ID, FItems[Result].ID) <> 0) do
    dec(Result);
end;

procedure TTranslationList.Clear;
var
  i: Integer;
begin
  for i := 0 to FCount-1 do
    FItems[i].Free;
  FCount := 0;
  ReallocMem(FItems, 0);
end;

initialization
  TranslationList := nil;
  GetLanguageIDs(SystemLanguageID1, SystemLanguageID2);
  FixLanguageIDs(SystemLanguageID1);

finalization
  TranslationList.Free;
  TranslationList := nil;

end.