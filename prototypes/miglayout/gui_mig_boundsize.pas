unit gui_mig_boundsize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gui_mig_unitvalue;
  
type

  { TBoundSize }

  TBoundSize = class(TObject)
  private
    FMin: TUnitValue;
    FPref: TUnitValue;
    FMax: TUnitValue;
    FGapPush: Boolean;
  public
    constructor Create(AMinMaxPref: TUnitValue; ACreateString: string); // overloaded;
    constructor Create(AMin: TUnitValue; APreferred: TUnitValue; AMax: TUnitValue; ACreateString: string); // overloaded;
    constructor Create(AMin: TUnitValue; APreferred: TUnitValue; AMax: TUnitValue; AGapPush: Boolean; ACreateString: string); // overloaded;
    function    GetSize(const ASizeType: integer): TUnitValue;
    function    IsUnset: Boolean;
    function    GetConstraintString: string;
    property    Min: TUnitValue read FMin;
    property    Preferred: TUnitValue read FPref;
    property    Max: TUnitValue read FMax;
    property    GapPush: Boolean read FGapPush;
  end;

implementation

uses
  gui_mig_exceptions;
  

{ TBoundSize }

function TBoundSize.IsUnset: Boolean;
begin
  Result := (FMin = nil) and (FPref = nil) and (FMax = nil);
end;

function TBoundSize.GetConstraintString: string;
begin
  Result := 'null';
  {$Note TBoundSize.GetConstraintString still needs to be implemented. }
end;

constructor TBoundSize.Create(AMinMaxPref: TUnitValue; ACreateString: string);
begin
  Create(AMinMaxPref, AMinMaxPref, AMinMaxPref, ACreateString);
end;

constructor TBoundSize.Create(AMin: TUnitValue; APreferred: TUnitValue;
  AMax: TUnitValue; ACreateString: string);
begin
  Create(AMin, APreferred, AMax, False, ACreateString);
end;

constructor TBoundSize.Create(AMin: TUnitValue; APreferred: TUnitValue;
  AMax: TUnitValue; AGapPush: Boolean; ACreateString: string);
begin
  inherited Create;
  FMin := AMin;
  FPref := APreferred;
  FMax := AMax;
  FGapPush := AGapPush;
  
  {$Note TBoundSize.Create is incompleted, we still need to handle ACreateString. }
end;

function TBoundSize.GetSize(const ASizeType: integer): TUnitValue;
begin
  case ASizetype of
    0{MIN}:
        result := FMin;
    1{PREF}:
        result := FPref;
    2{MAX}:
        result := FMax;
  else
    raise EIllegalArgument.CreateFmt('Unknown size: %d', [ASizeType]);
  end;
end;

end.

