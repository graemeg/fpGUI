program webdemo;

{$mode objfpc}{$H+}

uses
  fphttpapp, regreports, wmreports;

begin
  Application.Port:=8080;
  Application.AllowDefaultModule:=True;
  Application.DefaultModuleName:='Page';
  Application.PreferModuleName:=True;
  Application.Initialize;
  Application.Run;
end.

