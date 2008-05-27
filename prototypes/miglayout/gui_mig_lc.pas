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
    FNoGrid: Boolean;
    FTopToBottom: Boolean;
    FVisualPadding: Boolean;
    FWrapAfter: Integer;
    function    GetInsets: TUnitValueArray;
    procedure   SetHideMode(const AValue: integer);
    procedure   SetInsets(const AValue: TUnitValueArray);
  public
    constructor Create;
    function    Wrap: TLC;
    function    WrapAfter(const ACount: Integer): TLC;
    function    NoCache: TLC;
    function    FlowY: TLC;
    function    FlowX: TLC;
    function    Fill: TLC;
    function    FillX: TLC;
    function    FillY: TLC;
    function    LeftToRight(AValue: Boolean): TLC;
    function    BottomToTop: TLC;
    function    NoGrid: TLC;
    function    NoVisualPadding: TLC;
    function    InsetsAll(AllSides: string): TLC;
    property    NoCache_prop: Boolean read FNoCache write FNoCache default False;
    property    AlignX: TUnitValue read FAlignX write FAlignX;
    property    AlignY: TUnitValue read FAlignY write FAlignY;
    property    DebugMillis: integer read FDebugMillis write FDebugMillis default 0;
    property    FillX_prop: Boolean read FFillX write FFillX default False;
    property    FillY_prop: Boolean read FFillY write FFillY default False;
    property    FlowX_prop: Boolean read FFlowX write FFlowX default True;
    property    GridGapX: TBoundSize read FGridGapX write FGridGapX;
    property    GridGapY: TBoundSize read FGridGapY write FGridGapY;
    property    HideMode: integer read FHideMode write SetHideMode;
    property    Insets: TUnitValueArray read GetInsets write SetInsets;
    property    LeftToRight_prop: Boolean read FLeftToRight write FLeftToRight;
    property    TopToBottom_prop: Boolean read FTopToBottom write FTopToBottom;
    property    NoGrid_prop: Boolean read FNoGrid write FNoGrid default False;
    property    VisualPadding_prop: Boolean read FVisualPadding write FVisualPadding;
    property    WrapAfter_prop: Integer read FWrapAfter write FWrapAfter;
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
  FNoGrid := False;
end;

function TLC.Wrap: TLC;
begin
  WrapAfter_prop := 0;
  Result := self;
end;

function TLC.WrapAfter(const ACount: Integer): TLC;
begin
  WrapAfter_prop := ACount;
  Result := self;
end;

function TLC.NoCache: TLC;
begin
  NoCache_prop := True;
  Result := self;
end;

function TLC.FlowY: TLC;
begin
  FlowX_prop := False;
  Result := self;
end;

function TLC.FlowX: TLC;
begin
  FlowX_prop := True;
  Result := self;
end;

function TLC.Fill: TLC;
begin
  FillX_prop := True;
  FillY_prop := True;
  Result := self;
end;

function TLC.FillX: TLC;
begin
  FillX_prop := True;
  Result := self;
end;

function TLC.FillY: TLC;
begin
  FillY_prop := True;
  Result := self;
end;

function TLC.LeftToRight(AValue: Boolean): TLC;
begin
  LeftToRight_prop := AValue;
  Result := self;
end;

function TLC.BottomToTop: TLC;
begin
  TopToBottom_prop := False;
  Result := self;
end;

function TLC.NoGrid: TLC;
begin
  NoGrid_prop := True;
  Result := self;
end;

function TLC.NoVisualPadding: TLC;
begin
  VisualPadding_prop := False;
  Result := self;
end;

function TLC.InsetsAll(AllSides: string): TLC;
begin
  { TODO : Start Here!!!!!!!!!!!! }
end;

end.

