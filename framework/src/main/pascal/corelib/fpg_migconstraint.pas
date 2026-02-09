unit fpg_migconstraint;

{$I fpg_defines.inc}

interface

uses
  Classes, SysUtils, fpg_layouttypes;

type
  { Horizontal alignment - ported from AlignX.java (v11.4.2)

    Note: Java v11 uses LEADING, LEFT, CENTER, RIGHT, TRAILING
    Leading/Trailing are commented out until fpGUI supports BiDi
  }
  TfpgMigAlignX = (
    axLeft,      // Align to left edge
    axCenter,    // Align to horizontal center
    axRight,     // Align to right edge
    axFill       // Fill entire cell width
    // TODO: Enable when fpGUI supports BiDi
    // axLeading,   // Align to leading edge (left in LTR, right in RTL)
    // axTrailing   // Align to trailing edge (right in LTR, left in RTL)
  );

  { Vertical alignment - ported from AlignY.java (v11.4.2)

    Note: Java v11 uses TOP, CENTER, BOTTOM, BASELINE
    We keep existing values and add new ones for compatibility
  }
  TfpgMigAlignY = (
    ayTop,       // Align to top edge
    ayCenter,    // Align to vertical center
    ayBottom,    // Align to bottom edge
    ayFill,      // Fill entire cell height
    ayBaseline   // Align to text baseline
  );

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
