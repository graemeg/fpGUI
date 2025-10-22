unit fpg_migconstraint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_layouttypes;

type
  TfpgMigAlignX = (axLeft, axCenter, axRight, axFill);
  TfpgMigAlignY = (ayTop, ayCenter, ayBottom, ayFill);

  { TfpgMigConstraint }

  TfpgMigConstraint = class(TfpgLayoutConstraint)
  private
    FSpanX: Integer;
    FSpanY: Integer;
    FGrowX: Boolean;
    FGrowY: Boolean;
    FAlignX: TfpgMigAlignX;
    FAlignY: TfpgMigAlignY;
  public
    constructor Create;
  published
    property SpanX: Integer read FSpanX write FSpanX default 1;
    property SpanY: Integer read FSpanY write FSpanY default 1;
    property GrowX: Boolean read FGrowX write FGrowX default False;
    property GrowY: Boolean read FGrowY write FGrowY default False;
    property AlignX: TfpgMigAlignX read FAlignX write FAlignX default axLeft;
    property AlignY: TfpgMigAlignY read FAlignY write FAlignY default ayTop;
  end;

implementation

{ TfpgMigConstraint }

constructor TfpgMigConstraint.Create;
begin
  inherited Create;
  FSpanX := 1;
  FSpanY := 1;
  FGrowX := False;
  FGrowY := False;
  FAlignX := axLeft;
  FAlignY := ayTop;
end;

end.
