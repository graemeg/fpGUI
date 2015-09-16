program EditGridDemo;

{$mode objfpc}{$H+}

uses
  Classes,
  fpg_main,
  u_demo,
  u_editgrid;

procedure MainProc;
begin
  fpgApplication.Initialize;
  F_Demo := TF_Demo.Create(nil);
  try
    F_Demo.Show;
    fpgApplication.Run;
  finally
    F_Demo.Free;
  end;
end;

begin
  MainProc;
end.

