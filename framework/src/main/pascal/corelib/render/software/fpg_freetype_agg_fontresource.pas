{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    Description:
      FreeType-based font resource for the hybrid canvas. Provides accurate
      font metrics (GetTextWidth, GetAscent, GetDescent, GetHeight) using the
      same FreeType engine that TGlyphCache uses for rendering, ensuring
      measurement and rendering are always consistent.
}

unit fpg_freetype_agg_fontresource;

{$mode objfpc}{$H+}

interface

uses
  fpg_base,
  fpg_glyph_cache;

type

  { TfpgFreeTypeFontResource — font resource backed by TGlyphCache/FreeType.
    Each instance owns a TGlyphCache that provides metrics matching the
    rendered output exactly. }

  TfpgFreeTypeFontResource = class(TfpgFontResourceBase)
  private
    FGlyphCache: TGlyphCache;
    FValid: Boolean;
    FScale: Integer;
    FScaled: TfpgFreeTypeFontResource;
  public
    constructor Create(const AFontDesc: string); override;
    constructor CreateScaled(const AFontDesc: string; AScale: Integer);
    destructor Destroy; override;
    function HandleIsValid: boolean; override;
    function GetAscent: integer; override;
    function GetDescent: integer; override;
    function GetHeight: integer; override;
    function GetTextWidth(const txt: string): integer; override;
    procedure DrawTextToBuffer(ABuf: PByte; AStride, ABufW, ABufH,
      AX, AY: Integer; const AText: string; AColor: TfpgColor;
      AClipX1, AClipY1, AClipX2, AClipY2: Integer); override;
    function GetScaledResource(AScale: Integer): TfpgFontResourceBase; override;
  end;


implementation


{ TfpgFreeTypeFontResource }

constructor TfpgFreeTypeFontResource.Create(const AFontDesc: string);
begin
  inherited Create(AFontDesc);
  if FScale < 1 then
    FScale := 1;
  FGlyphCache := TGlyphCache.Create;
  FGlyphCache.RenderScale := FScale;
  { SetFont triggers FreeType font loading and metric capture }
  FGlyphCache.SetFont(Self);
  FValid := (FGlyphCache.Ascent > 0) or (FGlyphCache.Descent > 0);
end;

constructor TfpgFreeTypeFontResource.CreateScaled(const AFontDesc: string;
  AScale: Integer);
begin
  FScale := AScale;
  Create(AFontDesc);
end;

destructor TfpgFreeTypeFontResource.Destroy;
begin
  FScaled.Free;
  FGlyphCache.Free;
  inherited Destroy;
end;

function TfpgFreeTypeFontResource.HandleIsValid: boolean;
begin
  Result := FValid;
end;

function TfpgFreeTypeFontResource.GetAscent: integer;
begin
  Result := FGlyphCache.Ascent;
end;

function TfpgFreeTypeFontResource.GetDescent: integer;
begin
  Result := FGlyphCache.Descent;
end;

function TfpgFreeTypeFontResource.GetHeight: integer;
begin
  Result := FGlyphCache.LineHeight;
end;

function TfpgFreeTypeFontResource.GetTextWidth(const txt: string): integer;
begin
  Result := FGlyphCache.TextWidth(txt);
end;

procedure TfpgFreeTypeFontResource.DrawTextToBuffer(ABuf: PByte;
  AStride, ABufW, ABufH, AX, AY: Integer; const AText: string;
  AColor: TfpgColor; AClipX1, AClipY1, AClipX2, AClipY2: Integer);
begin
  FGlyphCache.DrawText(ABuf, AStride, ABufW, ABufH, AX, AY, AText, AColor,
    AClipX1, AClipY1, AClipX2, AClipY2);
end;

function TfpgFreeTypeFontResource.GetScaledResource(AScale: Integer): TfpgFontResourceBase;
begin
  Result := Self;
  if (AScale <= 1) or (AScale = FScale) then
    Exit;
  { One scaled variant is kept; a window moving to a screen with a
    different backing scale replaces it. }
  if Assigned(FScaled) and (FScaled.FScale <> AScale) then
  begin
    FScaled.Free;
    FScaled := nil;
  end;
  if not Assigned(FScaled) then
    FScaled := TfpgFreeTypeFontResource.CreateScaled(FontDesc, AScale);
  if FScaled.HandleIsValid then
    Result := FScaled;
end;


end.
