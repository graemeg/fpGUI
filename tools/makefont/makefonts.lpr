program makefonts;

{$mode objfpc}{$H+}

uses
  Classes,
  fpg_main, u_main, u_parsettf, u_data ;

{.$R *.res}

procedure MainProc;
begin
fpgApplication.Initialize;
F_MainForm:= TF_MainForm.Create(nil);
try
  F_MainForm.Show;
  fpgApplication.Run;
finally
  F_MainForm.Free;
  end;
end;

begin
MainProc;
end.

