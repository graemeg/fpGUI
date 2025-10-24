unit fpg_migconstraint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_layouttypes;

type
  TfpgMigAlignX = (axLeft, axCenter, axRight, axFill);
  TfpgMigAlignY = (ayTop, ayCenter, ayBottom, ayFill);

  TfpgMigConstraint = class(TfpgLayoutConstraint)
  private
    FSpanX: Integer; // Column span
    FSpanY: Integer; // Row span
    FGrowX: Integer; // Grow horizontally
    FGrowY: Integer; // Grow vertically
    FAlignX: TfpgMigAlignX;
    FAlignY: TfpgMigAlignY;
  public
    constructor Create;
  published
    property SpanX: Integer read FSpanX write FSpanX default 1;
    property SpanY: Integer read FSpanY write FSpanY default 1;
    property GrowX: Integer read FGrowX write FGrowX default 0;
    property GrowY: Integer read FGrowY write FGrowY default 0;
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
  FGrowX := 0;
  FGrowY := 0;
  FAlignX := axLeft;
  FAlignY := ayTop;
end;

end.
