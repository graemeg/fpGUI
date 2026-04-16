program vertex;

{
  Vertex — fpGUI HVIF icon editor.

  Main entry point. Initialises the fpGUI application and opens the
  main editor window.
}

{$mode objfpc}{$H+}

uses
  fpg_base,
  fpg_main,
  vertex.frm.main;


{ ── Entry point ──────────────────────────────────────────────────────────── }

procedure MainProc;
var
  frm: TVertexMainForm;
begin
  fpgApplication.Initialize;
  frm := TVertexMainForm.Create(nil);
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
