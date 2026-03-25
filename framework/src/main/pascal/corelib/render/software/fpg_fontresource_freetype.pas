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

unit fpg_fontresource_freetype;

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
  public
    constructor Create(const AFontDesc: string); override;
    destructor Destroy; override;
    function HandleIsValid: boolean; override;
    function GetAscent: integer; override;
    function GetDescent: integer; override;
    function GetHeight: integer; override;
    function GetTextWidth(const txt: string): integer; override;
  end;


implementation


{ TfpgFreeTypeFontResource }

constructor TfpgFreeTypeFontResource.Create(const AFontDesc: string);
begin
  inherited Create(AFontDesc);
  FGlyphCache := TGlyphCache.Create;
  { SetFont triggers FreeType font loading and metric capture }
  FGlyphCache.SetFont(Self);
  FValid := (FGlyphCache.Ascent > 0) or (FGlyphCache.Descent > 0);
end;

destructor TfpgFreeTypeFontResource.Destroy;
begin
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
  Result := FGlyphCache.Ascent + FGlyphCache.Descent;
end;

function TfpgFreeTypeFontResource.GetTextWidth(const txt: string): integer;
begin
  Result := FGlyphCache.TextWidth(txt);
end;


end.
