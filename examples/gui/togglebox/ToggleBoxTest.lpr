program ToggleBoxTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  fpg_main,
  mainfrm;

procedure MainProc;
var
  frmMain: TfrmMain;
begin
  fpgApplication.Initialize;
  frmMain:= TfrmMain.Create(nil);
  try
    frmMain.Show;
    fpgApplication.Run;
  finally
    frmMain.Free;
  end;
end;

begin
  MainProc;
end.

