{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2013 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This is a homegrown font cache, or font translation system. AggPas
      references font files (eg: *.ttf) directly, whereas the rest
      of fpGUI doesn't. Under X11 for example, the translation of
      'Aria-12' to the actual *.ttf file will be done by the fontconfig
      library. Unfortunately fontconfig doesn't have an API to give
      use that *.ttf font file it resolved too. So for AggPas (or rather
      the AggPas backend in fpGUI) we had to implement our own
      font translation system.
}

unit fpg_fontcache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, fpg_base;

type
  TFontCacheItem = class(TObject)
  private
    FAngle: double;
    FFamilyName: TfpgString;
    FFileName: TfpgString;
    FFixedWidth: boolean;
    FStyleFlags: Integer;
    function    GetIsBold: boolean;
    function    GetIsFixedWidth: boolean;
    function    GetIsItalic: boolean;
    function    GetIsRegular: boolean;
    procedure   SetIsBold(AValue: boolean);
    procedure   SetIsFixedWidth(AValue: boolean);
    procedure   SetIsItalic(AValue: boolean);
    procedure   SetIsRegular(AValue: boolean);
  public
    constructor Create(const AFilename: TfpgString);
    property    FileName: TfpgString read FFileName write FFileName;
    property    FamilyName: TfpgString read FFamilyName write FFamilyName;
    property    StyleFlags: Integer read FStyleFlags write FStyleFlags;
    property    IsFixedWidth: boolean read GetIsFixedWidth write SetIsFixedWidth;
    property    IsRegular: boolean read GetIsRegular write SetIsRegular;
    property    IsItalic: boolean read GetIsItalic write SetIsItalic;
    property    IsBold: boolean read GetIsBold write SetIsBold;
    { following properties are used by FontCacheItemFromFontDesc() only }
    property    Angle: double read FAngle write FAngle;
  end;


  TFontCacheList = class(TObject)
  private
    FList: TObjectList;
    procedure   SearchForFont(const AFontPath: TfpgString);
    function    BuildFontCacheItem(const AFontFile: TfpgString): TFontCacheItem;
    procedure   SetStyleIfExists(var AText: Ansistring; var AStyleFlags: integer; const AStyleName: AnsiString; const AStyleBit: integer);
  protected
    function    GetCount: integer; virtual;
    function    GetItem(AIndex: Integer): TFontCacheItem; virtual;
    procedure   SetItem(AIndex: Integer; AValue: TFontCacheItem); virtual;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   BuildFontCache;
    function    Add(const AObject: TFontCacheItem): integer;
    procedure   Clear;
    property    Count: integer read GetCount;
    function    IndexOf(const AObject: TFontCacheItem): integer;
    function    Find(const AFontCacheItem: TFontCacheItem): integer;
    property    Items[AIndex: Integer]: TFontCacheItem read GetItem write SetItem; default;
  end;


function gFontCache: TFontCacheList;

implementation

uses
  fpg_utils,
  agg_font_freetype_lib;

const
  FPG_FONT_STYLE_REGULAR = 1 shl 0;     { Regular, Plain, Book }
  FPG_FONT_STYLE_ITALIC = 1 shl 1;      { Itelic }
  FPG_FONT_STYLE_BOLD = 1 shl 2;        { Bold }
  FPG_FONT_STYLE_CONDENSED = 1 shl 3;   { Condensed }
  FPG_FONT_STYLE_EXTRALIGHT = 1 shl 4;  { ExtraLight }
  FPG_FONT_STYLE_LIGHT = 1 shl 5;       { Light }
  FPG_FONT_STYLE_SEMIBOLD = 1 shl 6;    { Semibold }
  FPG_FONT_STYLE_MEDIUM = 1 shl 7;      { Medium }
  FPG_FONT_STYLE_BLACK = 1 shl 8;       { Black }
  FPG_FONT_STYLE_FIXEDWIDTH = 1 shl 9;  { Fixedwidth }

var
  m_library: FT_Library_ptr;
  uFontCacheList: TFontCacheList;

function gFontCache: TFontCacheList;
begin
 if not Assigned(uFontCacheList) then
 begin
   uFontCacheList := TFontCacheList.Create;
   uFontCacheList.BuildFontCache;
 end;
 Result := uFontCacheList;
end;

{ TFontCacheItem }

function TFontCacheItem.GetIsBold: boolean;
begin
  Result := (FStyleFlags and FPG_FONT_STYLE_BOLD) <> 0;
end;

function TFontCacheItem.GetIsFixedWidth: boolean;
begin
  Result := (FStyleFlags and FPG_FONT_STYLE_FIXEDWIDTH) <> 0;
end;

function TFontCacheItem.GetIsItalic: boolean;
begin
  Result := (FStyleFlags and FPG_FONT_STYLE_ITALIC) <> 0;
end;

function TFontCacheItem.GetIsRegular: boolean;
begin
  Result := (FStyleFlags and FPG_FONT_STYLE_REGULAR) <> 0;
end;

procedure TFontCacheItem.SetIsBold(AValue: boolean);
begin
  FStyleFlags := FStyleFlags or FPG_FONT_STYLE_BOLD;
end;

procedure TFontCacheItem.SetIsFixedWidth(AValue: boolean);
begin
  FStyleFlags := FStyleFlags or FPG_FONT_STYLE_FIXEDWIDTH;
  FStyleFlags := FStyleFlags and (not FPG_FONT_STYLE_REGULAR);
end;

procedure TFontCacheItem.SetIsItalic(AValue: boolean);
begin
  FStyleFlags := FStyleFlags or FPG_FONT_STYLE_ITALIC;
end;

procedure TFontCacheItem.SetIsRegular(AValue: boolean);
begin
  FStyleFlags := FStyleFlags or FPG_FONT_STYLE_REGULAR;
  FStyleFlags := FStyleFlags and (not FPG_FONT_STYLE_FIXEDWIDTH);
end;

constructor TFontCacheItem.Create(const AFilename: TfpgString);
begin
  inherited Create;
  FFileName := AFilename;
  FStyleFlags := FPG_FONT_STYLE_REGULAR;
  FAngle := 0.0;
end;

{ TFontCacheList }

procedure TFontCacheList.SearchForFont(const AFontPath: TfpgString);
var
  sr: TSearchRec;
  lFont: TFontCacheItem;
  s: TfpgString;
begin
  // The extra 'or' includes Normal attribute files under Windows. faAnyFile doesn't return those.
  // Reported to FPC as bug 9440 in Mantis.
  if fpgFindFirst(AFontPath + AllFilesMask, faAnyFile or $00000080, sr) = 0 then
  begin
    repeat
      // check if special files to skip
      if (sr.Name = '.') or (sr.Name = '..') or (sr.Name = '') then
        Continue;
      // We got something, so lets continue
      s := fpgFromOSEncoding(sr.Name);
      if (sr.Attr and faDirectory) <> 0 then // found a directory
        SearchForFont(fpgAppendPathDelim(AFontPath + s))
      else
      begin // we have a file
        if (lowercase(fpgExtractFileExt(s)) = '.ttf') or
           (lowercase(fpgExtractFileExt(s)) = '.otf') then
        begin
          lFont := BuildFontCacheItem(AFontPath + s);
          Add(lFont);
        end;
      end;
    until fpgFindNext(sr) <> 0;
  end;
end;

function TFontCacheList.BuildFontCacheItem(const AFontFile: TfpgString): TFontCacheItem;
var
  face_ptr: FT_Face_ptr;
  s: Ansistring;
  flags: integer;
begin
  FT_New_Face(m_library, PChar(AFontFile), 0, face_ptr);
  Result := TFontCacheItem.Create(AFontFile);
  Result.FamilyName := face_ptr^.family_name;

  // extract simple styles first
//  if (face_ptr^.face_flags and FT_FACE_FLAG_FIXED_WIDTH) <> 0 then
//    Result.StyleFlags := FPG_FONT_STYLE_FIXEDWIDTH; // this should overwrite Regular style

  if (face_ptr^.style_flags and FT_STYLE_FLAG_ITALIC) <> 0 then
    Result.StyleFlags := Result.StyleFlags or FPG_FONT_STYLE_ITALIC;

  if (face_ptr^.style_flags and FT_STYLE_FLAG_BOLD) <> 0 then
    Result.StyleFlags := Result.StyleFlags or FPG_FONT_STYLE_BOLD;

  // Now to more complex styles stored in StyleName field. eg: 'Condensed Medium'
  s := face_ptr^.style_name;
  flags := Result.StyleFlags;
  SetStyleIfExists(s, flags, 'Condensed', FPG_FONT_STYLE_CONDENSED);
  SetStyleIfExists(s, flags, 'ExtraLight', FPG_FONT_STYLE_EXTRALIGHT);
  SetStyleIfExists(s, flags, 'Light', FPG_FONT_STYLE_LIGHT);
  SetStyleIfExists(s, flags, 'Semibold', FPG_FONT_STYLE_SEMIBOLD);
  SetStyleIfExists(s, flags, 'Medium', FPG_FONT_STYLE_MEDIUM);
  SetStyleIfExists(s, flags, 'Black', FPG_FONT_STYLE_BLACK);
  Result.StyleFlags := flags;

  FT_Done_Face(face_ptr);
end;

procedure TFontCacheList.SetStyleIfExists(var AText: Ansistring; var AStyleFlags: integer;
  const AStyleName: AnsiString; const AStyleBit: integer);
var
  i: integer;
begin
  i := Pos(AStyleName, AText);
  if i > 0 then
  begin
    AStyleFlags := AStyleFlags or AStyleBit;
    Delete(AText, Length(AStyleName), i);
  end;
end;

function TFontCacheList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TFontCacheList.GetItem(AIndex: Integer): TFontCacheItem;
begin
  Result := TFontCacheItem(FList.Items[AIndex]);
end;

procedure TFontCacheList.SetItem(AIndex: Integer; AValue: TFontCacheItem);
begin
  FList.Items[AIndex] := AValue;
end;

constructor TFontCacheList.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
end;

destructor TFontCacheList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TFontCacheList.BuildFontCache;
var
  lPath: TfpgString;
  lPathList: TStringList;
  i: integer;
begin
  try
    m_library := nil;
    FT_Init_FreeType(m_library);

    lPathList := TStringList.Create;
    lPathList.Add('/usr/share/cups/fonts/');
    lPathList.Add('/usr/share/fonts/truetype/');
    lPathList.Add('/usr/local/lib/X11/fonts/');
    lPathList.Add(GetUserDir + '.fonts/');
    for i := 0 to lPathList.Count-1 do
    begin
      lPath := lPathList[i];
      SearchForFont(lPath);
    end;
  finally
    FT_Done_FreeType(m_library);
    m_library := nil;
    lPathList.Free;
  end;
end;

function TFontCacheList.Add(const AObject: TFontCacheItem): integer;
begin
  Result := FList.Add(AObject);
end;

procedure TFontCacheList.Clear;
begin
  FList.Clear;
end;

function TFontCacheList.IndexOf(const AObject: TFontCacheItem): integer;
begin
  Result := FList.IndexOf(AObject);
end;

function TFontCacheList.Find(const AFontCacheItem: TFontCacheItem): integer;
var
  i: integer;
begin
  Result := -1; // nothing found
  for i := 0 to Count-1 do
  begin
    if (Items[i].FamilyName = AFontCacheItem.FamilyName) and
       (Items[i].StyleFlags = AFontCacheItem.StyleFlags) then
    begin
      Result := i;
      exit;
    end;
  end;
end;


initialization
  uFontCacheList := nil;

finalization
  uFontCacheList.Free;

end.

