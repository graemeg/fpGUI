unit gui_mig_platformdefaults;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gui_mig_unitvalue, gui_mig_incellgapprovider, contnrs, gui_mig_boundsize,
  gui_mig_componentwrapper;

type

  { TPlatformDefaults }

  TPlatformDefaults = class
  private

    class var
      DEF_H_UNIT: Integer;
      DEF_V_UNIT: Integer;
      GAP_PROVIDER: IInCellGapProvider;
      MOD_COUNT: Integer;
      LPX4,
      LPX7,
      LPX8,
      LPX9,
      LPX10,
      LPX11,
      LPX12,
      LPX14,
      LPX16,
      LPX20: TUnitValue;

      LPY4,
      LPY7,
      LPY8,
      LPY9,
      LPY10,
      LPY11,
      LPY12,
      LPY14,
      LPY16,
      LPY20: TUnitValue;
    const
      WINDOWS_XP = 0;
      MAC_OSX    = 1;
      GNOME      = 2;
      KDE        = 3;

    class var
      CUR_PLAF: Integer; // inited to 0
      PANEL_INS: TUnitValueArray;
      DIALOG_INS: TUnitValueArray;
      BUTTON_FORMAT: String;
      HOR_DEFS: TFPObjectHashTable;
      VER_DEFS: TFPObjectHashTable;
      DEF_VGAP: TBoundSize;
      DEF_HGAP: TBoundSize;
      RELATED_X: TBoundSize;
      RELATED_Y: TBoundSize;
      UNRELATED_X: TBoundSize;
      UNRELATED_Y: TBoundSize;
      BUTT_WIDTH: TUnitValue;
      horScale: Single;
      verScale: Single;
    class function GetCurrentPlatform: Integer; static;
    class function GetDefaultHorizontalUnit: Integer; static;
    class function GetDefaultVerticalUnit: Integer; static;
    class procedure SetCurrentPlatform(AValue: Integer); static;
    class procedure SetDefaultHorizontalUnit(AValue: Integer); static;
    class procedure SetDefaultVerticalUnit(AValue: Integer); static;
    public
      const
        BASE_FONT_SIZE       = 100;
        BASE_SCALE_FACTOR    = 101;
        BASE_REAL_PIXEL      = 102;
    private
      const
        LP_BASE              = BASE_SCALE_FACTOR;
        BASE_DPI             = 96;
      class procedure SetMinimumButtonWidth(AValue: TUnitValue); static;
    public
      class function  GetHorizontalScaleFactor: Single;
      class function  GetVerticalScaleFactor: Single;
      class procedure SetRelatedGap(x,y: TUnitValue);
      class procedure SetUnrelatedGap(x,y: TUnitValue);
      class procedure SetParagraphGap(x,y: TUnitValue);
      class procedure SetIndentGap(x,y: TUnitValue);
      class procedure SetGridCellGap(x,y: TUnitValue);
      class procedure SetUnitValue(AUnitStrings: array of string; x: TUnitValue; y: TUnitValue);
      class function  ConvertToPixels(AValue: Single; AUnit: String; AIsHorizontal: Boolean; ARef: Single; AParent: IContainerWrapper; AComp: IComponentWrapper): Integer;
      class property CurrentPlatform: Integer read CUR_PLAF write SetCurrentPlatform;
      class property DefaultHorizontalUnit: Integer read GetDefaultHorizontalUnit write SetDefaultHorizontalUnit;
      class property DefaultVerticalUnit: Integer read GetDefaultVerticalUnit write SetDefaultVerticalUnit;
      class property MinimumButtonWidth: TUnitValue read BUTT_WIDTH write SetMinimumButtonWidth;
      class constructor Create;



  end;

implementation

uses
  gui_mig_exceptions, gui_mig_unitconverter, math;



{ TPlatformDefaults }

class function TPlatformDefaults.GetCurrentPlatform: Integer; static;
begin
  {$IFDEF MSWINDOWS}
   Result := WINDOWS_XP;
  {$ENDIF}
  {$IFDEF DARWIN}
   Result := MAC_OSX;
  {$ELSE} //*nix
   if Lowercase(GetEnvironmentVariable('XDG_CURRENT_DESKTOP')) = 'kde' then
     Result := KDE
   else
     Result := GNOME;
  {$ENDIF}
end;

class function TPlatformDefaults.GetDefaultHorizontalUnit: Integer; static;
begin
  Result := DEF_H_UNIT;
end;

class function TPlatformDefaults.GetDefaultVerticalUnit: Integer; static;
begin
  Result := DEF_V_UNIT;
end;

class procedure TPlatformDefaults.SetCurrentPlatform(AValue: Integer); static;
begin
  if (AValue < WINDOWS_XP) or (AValue > KDE) then
    raise EIllegalArgument.CreateFmt('Unknown platform: %d', [AValue]);

  CUR_PLAF := AValue;

  case AValue of
    WINDOWS_XP:
      begin
        SetRelatedGap(LPX4, LPY4);
        SetUnrelatedGap(LPX7, LPY9);
        SetParagraphGap(LPX14, LPY14);
        SetIndentGap(LPX9,LPY9);
        SetGridCellGap(LPX4, LPY4);

        SetMinimumButtonWidth(TUnitValue.Create(75, TUnitValue.LPX, ''));
      end;
  end;

end;

class procedure TPlatformDefaults.SetDefaultHorizontalUnit(AValue: Integer);
  static;
begin
  if (AValue < TUnitValue.PIXEL) or (AValue > TUnitValue.LABEL_ALIGN) then
    raise EIllegalArgument.CreateFmt('Illegal Unit: %d', [AValue]);

  if DEF_H_UNIT <> AValue then
  begin
    DEF_H_UNIT:=AValue;
    Inc(MOD_COUNT);
  end;
end;

class procedure TPlatformDefaults.SetDefaultVerticalUnit(AValue: Integer);
  static;
begin
  if (AValue < TUnitValue.PIXEL) or (AValue > TUnitValue.LABEL_ALIGN) then
    raise EIllegalArgument.CreateFmt('Illegal Unit: %d', [AValue]);

  if DEF_V_UNIT <> AValue then
  begin
    DEF_V_UNIT:=AValue;
    Inc(MOD_COUNT);
  end;
end;

class procedure TPlatformDefaults.SetMinimumButtonWidth(AValue: TUnitValue);
begin
  if BUTT_WIDTH=AValue then Exit;
  BUTT_WIDTH:=AValue;
  Inc(MOD_COUNT);
end;

class function TPlatformDefaults.GetHorizontalScaleFactor: Single;
begin
  Result := horScale;
end;

class function TPlatformDefaults.GetVerticalScaleFactor: Single;
begin
  Result := verScale;
end;

class procedure TPlatformDefaults.SetRelatedGap(x, y: TUnitValue);
begin
  SetUnitValue(['r', 'rel', 'related'], x, y);
  RELATED_X := TBoundSize.Create(x, x, nil, 'rel:rel');
  RELATED_Y := TBoundSize.Create(y, y, nil, 'rel:rel');
end;

class procedure TPlatformDefaults.SetUnrelatedGap(x, y: TUnitValue);
begin
  SetUnitValue(['u', 'unrel', 'unrelated'], x, y);
  UNRELATED_X := TBoundSize.Create(x, x, nil, 'unrel:unrel');
  UNRELATED_Y := TBoundSize.Create(y, y, nil, 'unrel:unrel');
end;

class procedure TPlatformDefaults.SetParagraphGap(x, y: TUnitValue);
begin
  SetUnitValue(['p', 'para', 'paragraph'], x, y);
end;

class procedure TPlatformDefaults.SetIndentGap(x, y: TUnitValue);
begin
  SetUnitValue(['i', 'ind', 'indent'], x, y);
end;

class procedure TPlatformDefaults.SetGridCellGap(x, y: TUnitValue);
begin
  if x <> nil then
    DEF_HGAP := TBoundSize.Create(x,x,nil,'');
  if y <> nil then
    DEF_VGAP := TBoundSize.Create(y,y,nil,'');
  Inc(MOD_COUNT);
end;

class procedure TPlatformDefaults.SetUnitValue(AUnitStrings: array of string;
  x: TUnitValue; y: TUnitValue);
var
  i: Integer;
  s: String;
begin
  for i := 0 to High(AUnitStrings) do
  begin
    s := Trim(LowerCase(AUnitStrings[i]));
    if x <> nil then
      HOR_DEFS.Add(s, x);
    if y <> nil then
      VER_DEFS.Add(s, y);
  end;
  Inc(MOD_COUNT);
end;

class function TPlatformDefaults.ConvertToPixels(AValue: Single; AUnit: String;
  AIsHorizontal: Boolean; ARef: Single; AParent: IContainerWrapper;
  AComp: IComponentWrapper): Integer;
var
  uv: TUnitValue;
begin
  Result := TUnitConverter.UNABLE;
  if AIsHorizontal then
    uv := TUnitValue(HOR_DEFS.Items[AUnit])
  else
    uv := TUnitValue(VER_DEFS.Items[AUnit]);

   if uv <> nil then
     Result := Round(AValue * uv.GetPixels(Aref, AParent, AComp))
   else
     Result := TUnitConverter.UNABLE;
end;

class constructor TPlatformDefaults.Create;
begin
  DEF_H_UNIT := TUnitValue.LPX;
  DEF_V_UNIT := TUnitValue.LPY;
end;

end.

