{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    Description:
      Centralized font resource management with caching.
}
unit fpg_fontmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  fpg_base;

type
  TfpgFontResourceClass = class of TfpgFontResourceBase;

var
  AggFontResourceClass: TfpgFontResourceClass = nil;

type
  { TfpgFontManager - Centralized font resource management with caching }
  TfpgFontManager = class(TObject)
  private
    FFontCache: TFPHashObjectList;  // Hash table for O(1) font lookup
    FDefaultFontDesc: string;
    FFixedFontDesc: string;
    FLoadingDefaultFont: Boolean;   // Guard against GetFont <-> GetDefaultFont recursion

    function ResolveFontAlias(const ADesc: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    { Main font API - retrieves font from cache or creates new one }
    function GetFont(const ADesc: string): TfpgFontResourceBase;

    { Default fonts }
    function GetDefaultFont: TfpgFontResourceBase;
    function GetFixedFont: TfpgFontResourceBase;

    property DefaultFontDesc: string read FDefaultFontDesc write FDefaultFontDesc;
    property FixedFontDesc: string read FFixedFontDesc write FFixedFontDesc;

    { Debugging and statistics }
    function GetCacheSize: integer;
    function GetCacheStats: string;
    procedure DumpCache;
  end;

implementation

uses
  fpg_main;

{ TfpgFontManager }

constructor TfpgFontManager.Create;
begin
  inherited Create;
  FFontCache := TFPHashObjectList.Create(True);  // Owns font resources

  // Platform-specific defaults will be set by TfpgApplication
  FDefaultFontDesc := '';
  FFixedFontDesc := '';
  FLoadingDefaultFont := False;
end;

destructor TfpgFontManager.Destroy;
begin
  FFontCache.Free;
  inherited Destroy;
end;

function TfpgFontManager.ResolveFontAlias(const ADesc: string): string;
begin
  // Check if this is a named font (starts with #)
  if (Length(ADesc) > 0) and (ADesc[1] = '#') then
  begin
    // Named font: #Label1 -> resolve through fpgGetNamedFontDesc
    Result := fpgGetNamedFontDesc(Copy(ADesc, 2, Length(ADesc)));
  end
  else
    Result := ADesc;
end;

function TfpgFontManager.GetFont(const ADesc: string): TfpgFontResourceBase;
var
  fdesc: string;
  normalizedKey: string;
  fontDef: TfpgFontDefinition;
begin
  // Handle empty descriptor - return default font
  if ADesc = '' then
  begin
    Result := GetDefaultFont;
    Exit;
  end;

  // Resolve named fonts (#Label1 -> actual descriptor)
  fdesc := ResolveFontAlias(ADesc);

  // Parse font descriptor to get normalized form for cache key
  fontDef := TfpgFontDefinition.Create(fdesc);
  try
    // Use the original font descriptor as stored for cache key
    // This ensures consistent lookups
    normalizedKey := fontDef.FontDesc;

    // O(1) hash table lookup
    Result := TfpgFontResourceBase(FFontCache.Find(normalizedKey));

    if not Assigned(Result) then
    begin
      // Cache miss - create font resource.
      // When AggPas is active, use AggFontResourceClass which provides
      // FreeType-based metrics matching the rendered output. Otherwise
      // use native font resources (Xft on X11, GDI on Windows).
      if Assigned(AggFontResourceClass) then
        Result := AggFontResourceClass.Create(fdesc)
      else
        Result := TfpgFontResource.Create(fdesc);

      if Result.HandleIsValid then
      begin
        // Font creation succeeded - add to cache
        FFontCache.Add(normalizedKey, Result);
      end
      else
      begin
        // Font creation failed - free it and try default font
        Result.Free;
        if FLoadingDefaultFont then
          Result := nil  // Default font itself failed - avoid infinite recursion
        else
          Result := GetDefaultFont;
      end;
    end;
  finally
    fontDef.Free;
  end;
end;

function TfpgFontManager.GetDefaultFont: TfpgFontResourceBase;
begin
  if FDefaultFontDesc = '' then
  begin
    // Fallback to reasonable default
    FDefaultFontDesc := FPG_DEFAULT_FONT_DESC;
  end;

  FLoadingDefaultFont := True;
  try
    Result := GetFont(FDefaultFontDesc);
  finally
    FLoadingDefaultFont := False;
  end;

  if Result = nil then
    raise EfpGUIException.CreateFmt('Unable to load default font "%s". ' +
      'Please ensure font files are installed in a standard font directory.', [FDefaultFontDesc]);
end;

function TfpgFontManager.GetFixedFont: TfpgFontResourceBase;
begin
  if FFixedFontDesc = '' then
  begin
    // Fallback to reasonable default
    FFixedFontDesc := FPG_DEFAULT_FIXED_FONT_DESC;
  end;

  Result := GetFont(FFixedFontDesc);
end;

function TfpgFontManager.GetCacheSize: integer;
begin
  Result := FFontCache.Count;
end;

function TfpgFontManager.GetCacheStats: string;
var
  i: integer;
  fontBase: TfpgFontResourceBase;
begin
  Result := Format('Font Cache Statistics:'+LineEnding, []);
  Result := Result + Format('  Total cached fonts: %d'+LineEnding, [FFontCache.Count]);
  Result := Result + Format('  Default font: %s'+LineEnding, [FDefaultFontDesc]);
  Result := Result + Format('  Fixed font: %s'+LineEnding+LineEnding, [FFixedFontDesc]);

  if FFontCache.Count > 0 then
  begin
    Result := Result + 'Cached fonts:'+LineEnding;
    for i := 0 to FFontCache.Count - 1 do
    begin
      fontBase := TfpgFontResourceBase(FFontCache[i]);
      Result := Result + Format('  [%d] %s'+LineEnding, [i, fontBase.FontDesc]);
    end;
  end;
end;

procedure TfpgFontManager.DumpCache;
begin
  DebugLn(GetCacheStats);
end;

end.
