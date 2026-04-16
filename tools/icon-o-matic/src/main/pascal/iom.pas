program iom;

{
  fpGUI Icon-O-Matic — HVIF icon editor.

  Main entry point. Initialises the fpGUI application and opens the
  main editor window.
}

{$mode objfpc}{$H+}

uses
  fpg_base,
  fpg_main,
  iom.frm.main;


{ ── Entry point ──────────────────────────────────────────────────────────── }

procedure MainProc;
var
  frm: TIomMainForm;
begin
  fpgApplication.Initialize;
  frm := TIomMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.
