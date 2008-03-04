{
  Layout Manager Constraint.
}

unit gui_mig_lc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gui_mig_unitvalue, gui_mig_boundsize, gui_mig_exceptions;
  
type

  { TLC }

  TLC = class(TObject)
  private
    FDebugMillis: integer;
    FFillX: Boolean;
    FFillY: Boolean;
    FFlowX: Boolean;
    FGridGapX: TBoundSize;
    FGridGapY: TBoundSize;
    FHideMode: integer;
    FNoCache: Boolean;
    FAlignX: TUnitValue;
    FAlignY: TUnitValue;
    FInsets: TUnitValueArray;
    FLeftToRight: Boolean;
    FTopToBottom: Boolean;
    function GetInsets: TUnitValueArray;
    procedure SetHideMode(const AValue: integer);
    procedure SetInsets(const AValue: TUnitValueArray);
  public
    constructor Create;
    property    NoCache: Boolean read FNoCache write FNoCache default False;
    property    AlignX: TUnitValue read FAlignX write FAlignX;
    property    AlignY: TUnitValue read FAlignY write FAlignY;
    property    DebugMillis: integer read FDebugMillis write FDebugMillis default 0;
    property    FillX: Boolean read FFillX write FFillX default False;
    property    FillY: Boolean read FFillY write FFillY default False;
    property    FlowX: Boolean read FFlowX write FFlowX default True;
    property    GridGapX: TBoundSize read FGridGapX write FGridGapX;
    property    GridGapY: TBoundSize read FGridGapY write FGridGapY;
    property    HideMode: integer read FHideMode write SetHideMode;
    property    Insets: TUnitValueArray read GetInsets write SetInsets;
    property    LeftToRight: Boolean read FLeftToRight write FLeftToRight;
    property    TopToBottom: Boolean read FTopToBottom write FTopToBottom;
  end;
  

implementation

{ TLC }

procedure TLC.SetHideMode(const AValue: integer);
begin
  if FHideMode=AValue then exit;
  
  if (AValue < 0) or (AValue > 3) then
    raise EIllegalArgument.CreateFmt('Wrong HideMode: %d', [AValue]);
  
  FHideMode:=AValue;
end;

function TLC.GetInsets: TUnitValueArray;
begin

end;

procedure TLC.SetInsets(const AValue: TUnitValueArray);
begin

end;

constructor TLC.Create;
begin
  FNoCache    := False;
  FAlignX     := nil;
  FAlignY     := nil;
  FDebugMillis := 0;
  FFillX := False;
  FFillY := False;
  FFlowX := True;
  FGridGapX := nil;
  FGridGapY := nil;
  FLeftToRight := True;
  FTopToBottom := True;
end;

end.

