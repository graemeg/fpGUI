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

    function ResolveFontAlias(const ADesc: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    { Main font API - retrieves font from cache or creates new one }
    function GetFont(const ADesc: string): TfpgFontResourceBase;

    { Called by font resource when it's being destroyed }
    procedure NotifyFontDestroyed(AFont: TfpgFontResourceBase);

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
      // Cache miss - create new platform-specific font resource
      {$IFDEF AGGCANVAS}
      if Assigned(AggFontResourceClass) then
        Result := AggFontResourceClass.Create(fdesc)
      else
        Result := TfpgFontResource.Create(fdesc); // Fallback if not registered
      {$ELSE}
      Result := TfpgFontResource.Create(fdesc);
      {$ENDIF}

      if Result.HandleIsValid then
      begin
        // Font creation succeeded - add to cache
        FFontCache.Add(normalizedKey, Result);
      end
      else
      begin
        // Font creation failed - free it and return default font
        Result.Free;
        Result := GetDefaultFont;
      end;
    end;
  finally
    fontDef.Free;
  end;
end;

procedure TfpgFontManager.NotifyFontDestroyed(AFont: TfpgFontResourceBase);
var
  idx: integer;
begin
  // Called by font destructor when ref count reaches 0
  // Remove font from cache (but don't free it - cache owns it)
  if not Assigned(AFont) then Exit;

  idx := FFontCache.FindIndexOf(AFont.FontDesc);
  if idx >= 0 then
    FFontCache.Delete(idx);  // Remove from cache without freeing (already being destroyed)
end;

function TfpgFontManager.GetDefaultFont: TfpgFontResourceBase;
begin
  if FDefaultFontDesc = '' then
  begin
    // Fallback to reasonable default
    {$IFDEF UNIX}
    FDefaultFontDesc := 'Liberation Sans-10';
    {$ELSE}
    FDefaultFontDesc := 'Arial-10';
    {$ENDIF}
  end;

  Result := GetFont(FDefaultFontDesc);
end;

function TfpgFontManager.GetFixedFont: TfpgFontResourceBase;
begin
  if FFixedFontDesc = '' then
  begin
    // Fallback to reasonable default
    {$IFDEF UNIX}
    FFixedFontDesc := 'Liberation Mono-10';
    {$ELSE}
    FFixedFontDesc := 'Courier New-10';
    {$ENDIF}
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
  Result := Format('Font Cache Statistics:'#13#10, []);
  Result := Result + Format('  Total cached fonts: %d'#13#10, [FFontCache.Count]);
  Result := Result + Format('  Default font: %s'#13#10, [FDefaultFontDesc]);
  Result := Result + Format('  Fixed font: %s'#13#10#13#10, [FFixedFontDesc]);

  if FFontCache.Count > 0 then
  begin
    Result := Result + 'Cached fonts:'#13#10;
    for i := 0 to FFontCache.Count - 1 do
    begin
      fontBase := TfpgFontResourceBase(FFontCache[i]);
      Result := Result + Format('  [%d] %s (RefCount: %d)'#13#10,
                                [i, fontBase.FontDesc, fontBase.GetRefCount]);
    end;
  end;
end;

procedure TfpgFontManager.DumpCache;
begin
  WriteLn(GetCacheStats);
end;

end.
