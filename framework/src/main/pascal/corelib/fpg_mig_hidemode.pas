unit fpg_mig_hidemode;

{
  MigLayout v11 HideMode enum for fpGUI

  Ported from: net.miginfocom.layout.HideMode.java (v11.4.2)

  Defines how hidden components should be handled in the layout.
}

{$mode objfpc}{$H+}

interface

type
  { TfpgMigHideMode - Defines behavior of hidden widgets in layout

    NORMAL: Bounds calculated as if component was visible
    SIZE_0_RETAIN_GAPS: Hidden component has size 0,0 but gaps remain
    SIZE_0_GAPS_0: Hidden component has size 0,0 and gaps are zero
    DISREGARD: Hidden component completely disregarded, doesn't take grid cell
  }
  TfpgMigHideMode = (
    hmNormal,           // Code: 0
    hmSize0RetainGaps,  // Code: 1
    hmSize0Gaps0,       // Code: 2
    hmDisregard         // Code: 3
  );

  { HideMode helper functions }
  function HideModeToCode(AMode: TfpgMigHideMode): Integer;
  function CodeToHideMode(ACode: Integer): TfpgMigHideMode;

implementation

uses
  SysUtils;

function HideModeToCode(AMode: TfpgMigHideMode): Integer;
begin
  Result := Ord(AMode);
end;

function CodeToHideMode(ACode: Integer): TfpgMigHideMode;
begin
  if (ACode < 0) or (ACode > 3) then
    raise Exception.CreateFmt('Invalid HideMode code: %d', [ACode]);
  Result := TfpgMigHideMode(ACode);
end;

end.
