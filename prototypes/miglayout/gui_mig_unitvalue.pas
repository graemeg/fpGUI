unit gui_mig_unitvalue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils{, fphash,}, contnrs, gui_mig_componentwrapper;
  
type

  { TUnitValue }
  TUnitValue = class;

  TUnitValueArray = array[0..3] of TUnitValue;

  TUnitValue = class(TObject)
  private
    FPixels: integer;
    FValue: Single;
    FOper: Integer;
    FIsHorizontal: Boolean;
    FUnitStr: String;
    FUnit: Integer;
    FSubUnits: TUnitValueArray;
    class var
      UNIT_MAP: TFPDataHashTable;
      Converters: TFPList;
    procedure InternalCreate(AValue: Single; AUnitString: String; AUnit: Integer; IsHorizontal: Boolean; Oper: Integer; sub1: TUnitValue; sub2: TUnitValue; ACreateString: String);
    function ParseUnitString: Integer;
    function LookupValue(ARefValue: Single; AParent: IContainerWrapper; AComponent: IComponentWrapper): Single;
  public
    const
      STATIC = 100;
      ADD = 101; // Must have "sub-unit values"
      SUB = 102; // Must have "sub-unit values"
      MUL = 103; // Must have "sub-unit values"
      DIV_ = 104; // Must have "sub-unit values"
      MIN = 105; // Must have "sub-unit values"
      MAX = 106; // Must have "sub-unit values"
      MID = 107; // Must have "sub-unit values"

      PIXEL = 0;
      LPX = 1;
      LPY = 2;
      MM = 3;
      CM = 4;
      INCH = 5;
      PERCENT = 6;
      PT = 7;
      SPX = 8;
      SPY = 9;
      ALIGN = 12;
      MIN_SIZE = 13;
      PREF_SIZE = 14;
      MAX_SIZE = 15;
      BUTTON = 16;
      LINK_X = 18;   // First link
      LINK_Y = 19;
      LINK_W = 20;
      LINK_H = 21;
      LINK_X2 = 22;
      LINK_Y2 = 23;
      LINK_XPOS = 24;
      LINK_YPOS = 25;    // Last link
      LOOKUP = 26;
      LABEL_ALIGN = 27;
      IDENTITY = -1;
      SCALE : array[0..4] of Single = (25.4, 2.54, 1, 0, 72);
    class var // init in class constructor
        ZERO: TUnitValue;
        TOP: TUnitValue;
        LEADING: TUnitValue;
        LEFT: TUnitValue;
        CENTER: TUnitValue;
        TRAILING: TUnitValue;
        RIGHT: TUnitValue;
        BOTTOM: TUnitValue;
        LABEL_: TUnitValue;
        INF: TUnitValue;
        BASELINE_IDENTITY: TUnitValue;

    // class constructor must be before instance constructors
    class constructor Create;
    class destructor  Destroy;
    constructor Create(AValue: Single);
    constructor Create(AValue: Single; AUnit: Integer; ACreateString: String);
    constructor Create(AValue: Single; AUnitString: String; AIsHorizontal: Boolean; AOper: Integer; ACreateString: String);
    constructor Create(AIsHorizontal: Boolean; AOper: Integer; ASub1: TUnitValue; ASub2: TUnitValue; ACreateString: String);
    function  GetPixels(ARefValue: Single; AParent: IContainerWrapper; AComp: IComponentWrapper): Integer;
    function  GetPixelsExact(ARefValue: Single; AParent: IContainerWrapper; AComp: IComponentWrapper): Single;
    property  Pixels: integer read FPixels;
  end;
  


implementation
uses
  gui_mig_exceptions, gui_mig_platformdefaults, gui_mig_unitconverter;

function EndsWith(InString: String; Lookingfor: String): Boolean;
begin
  // I got tired of doing this over and over so I added a function for endswith
  if Length(InString) < Length(Lookingfor) then
    Exit(False);
  Result := Copy(InString, Length(InString)-Length(Lookingfor), Length(Lookingfor)) = Lookingfor;
end;

{
const
    cSTATIC = 100;
    cADD = 101; // Must have "sub-unit values"
    cSUB = 102; // Must have "sub-unit values"
    cMUL = 103; // Must have "sub-unit values"
    cDIV = 104; // Must have "sub-unit values"
    cMIN = 105; // Must have "sub-unit values"
    cMAX = 106; // Must have "sub-unit values"
    cMID = 107; // Must have "sub-unit values"

    cPIXEL = 0;
    cLPX = 1;
    cLPY = 2;
    cMM = 3;
    cCM = 4;
    cINCH = 5;
    cPERCENT = 6;
    cPT = 7;
    cSPX = 8;
    cSPY = 9;
    cALIGN = 12;
    cMIN_SIZE = 13;
    cPREF_SIZE = 14;
    cMAX_SIZE = 15;
    cBUTTON = 16;
    cLINK_X = 18;   // First link
    cLINK_Y = 19;
    cLINK_W = 20;
    cLINK_H = 21;
    cLINK_X2 = 22;
    cLINK_Y2 = 23;
    cLINK_XPOS = 24;
    cLINK_YPOS = 25;    // Last link
    cLOOKUP = 26;
    cLABEL_ALIGN = 27;
    cIDENTITY = -1;
}
{ TUnitValue }

procedure TUnitValue.InternalCreate(AValue: Single; AUnitString: String;
  AUnit: Integer; IsHorizontal: Boolean; Oper: Integer; sub1: TUnitValue;
  sub2: TUnitValue; ACreateString: String);
begin
  if (Oper < STATIC) or (oper > MID) then
    raise EIllegalArgument.CreateFmt('Unknown Operation: %d', [oper]);
  if (Oper >= ADD) and (Oper <= MID) and ((sub1 = nil) or (sub2 = nil)) then
    raise EIllegalArgument.CreateFmt('%d Operation may not have null sub-UnitValues.', [oper]);

  FValue:=AValue;
  FOper:=Oper;
  FIsHorizontal:=IsHorizontal;
  FUnitStr:=AUnitString;
  if AUnitString <> '' then
    FUnit:=ParseUnitString
  else
    FUnit := AUnit;

  if (sub1 <> nil) and (sub2 <> nil) then
  begin
    FSubUnits[0] := sub1;
    FSubUnits[1] := sub2;
  end;

  {$NOTE Layoututil todo}
  //LayoutUtil.PusCCString(Self, ACreateString);

end;

function TUnitValue.ParseUnitString: Integer;
var
  u: Integer;
begin
  if Length(FUnitStr) = 0 then
    if FIsHorizontal then
      Exit(TPlatformDefaults.DefaultHorizontalUnit)
    else
      Exit(TPlatformDefaults.DefaultVerticalUnit);

  u := PtrInt(UNIT_MAP.Items[FUnitStr]);

  {$NOTE UNIT_MAP should probably be it's own hash type for int's instead of typecasting pointers}
  if u <> 0 then
    Exit(u);

  if FUnitStr = 'lp' then
    if FIsHorizontal then Exit(LPX) else Exit(LPY);
  if FUnitStr = 'sp' then
    if FIsHorizontal then Exit(SPX) else Exit(SPY);

  if LookupValue(0, nil, nil) <> TUnitConverter.UNABLE then
    Exit(LOOKUP);

  if EndsWith(FUnitStr, '.x') then
    Exit(LINK_X);
  if EndsWith(FUnitStr, '.y') then
    Exit(LINK_Y);
  if EndsWith(FUnitStr, '.w') or EndsWith(FUnitStr, '.width') then
    Exit(LINK_W);
  if EndsWith(FUnitStr, '.h') or EndsWith(FUnitStr, '.height') then
    Exit(LINK_H);
  if EndsWith(FUnitStr, '.x2') then
    Exit(LINK_X2);
  if EndsWith(FUnitStr, '.y2') then
    Exit(LINK_Y2);
  if EndsWith(FUnitStr, '.xpos') then
    Exit(LINK_XPOS);
  if EndsWith(FUnitStr, '.ypos') then
    Exit(LINK_YPOS);

  raise EIllegalArgument.CreateFmt('Unknown keyword: %s', [FUnitStr]);
end;

function TUnitValue.LookupValue(ARefValue: Single; AParent: IContainerWrapper;
  AComponent: IComponentWrapper): Single;
var
  i: Integer;
begin
  Result := TUnitConverter.UNABLE;
  for i := 0 to Converters.Count-1 do
  begin
    Result := TUnitConverter(Converters.Items[i]).ConvertToPixels(FValue, FUnitStr, FIsHorizontal, ARefValue, AParent, AComponent);
    if Result <> TUnitConverter.UNABLE then
      Exit;
  end;

  Result := TPlatformDefaults.ConvertToPixels(FValue, FUnitStr, FIsHorizontal, ARefValue, AParent, AComponent);
end;

constructor TUnitValue.Create(AValue: Single);
begin
  InternalCreate(AValue, '', PIXEL, True, STATIC, nil, nil, FormatFloat('0.000', AValue)+'px');
end;

constructor TUnitValue.Create(AValue: Single; AUnit: Integer;
  ACreateString: String);
begin
  InternalCreate(AValue, '', AUnit, True, STATIC, nil, nil, ACreateString);
end;

constructor TUnitValue.Create(AValue: Single; AUnitString: String;
  AIsHorizontal: Boolean; AOper: Integer; ACreateString: String);
begin
  InternalCreate(AValue, AUnitString, -1, AIsHorizontal, AOper, nil, nil, ACreateString);
end;

constructor TUnitValue.Create(AIsHorizontal: Boolean; AOper: Integer;
  ASub1: TUnitValue; ASub2: TUnitValue; ACreateString: String);
begin
  InternalCreate(0, '', -1, AIsHorizontal, AOper, ASub1, ASub2, ACreateString);
  if (ASub1 = nil) or (ASub2 = nil) then
    raise EIllegalArgument.Create('Sub unit(s) is nil!');
end;


class constructor TUnitValue.Create;
begin
  UNIT_MAP := TFPDataHashTable.Create;
  Converters := TFPList.Create;
end;

class destructor TUnitValue.Destroy;
begin
  UNIT_MAP.Free;
  Converters.Free;
end;

function TUnitValue.GetPixels(ARefValue: Single; AParent: IContainerWrapper;
  AComp: IComponentWrapper): Integer;
begin
  Result := Round(GetPixelsExact(ARefValue, AParent, AComp));
end;

function TUnitValue.GetPixelsExact(ARefValue: Single;
  AParent: IContainerWrapper; AComp: IComponentWrapper): Single;
var
  F,
  S: Single;

begin
  if AParent = nil then
    Exit(1);
  if FOper = STATIC then
    case FUnit of
      PIXEL    : Exit(FValue);
      LPX, LPY : Exit(AParent.GetPixelUnitFactor(FUnit = LPX) * FValue);
      MM, CM, INCH, PT:
        begin
          F := SCALE[FUnit - MM];
          if FIsHorizontal then
            s := TPlatformDefaults.GetHorizontalScaleFactor
          else
            s := TPlatformDefaults.GetVerticalScaleFactor;
        end;
    end;
end;


end.

