program nanoedit;

{$mode objfpc}{$H+}
{$ifdef mswindows} {$apptype gui} {$endif}

uses
  Classes,
  SysUtils,
  fpg_main,
  fpg_cmdlineparams,
  mainfrm,
  simpleipc;

function AnotherInstance: Boolean;
var
  aClient: TSimpleIPCClient;
begin
  if (ParamCount > 0) then
  begin
    aClient := TSimpleIPCClient.Create(nil);
    try
      aClient.ServerID := 'nanoedit';
      Result := aClient.ServerRunning;  //There is another instance
      if Result then
      begin
        aClient.Connect;
        try
          aClient.SendStringMessage(1, ParamStr(1));
        finally
          aClient.Disconnect;
        end;
      end
    finally
      aClient.Free;
    end;
  end
  else
  begin
    Result := False;
  end
end;

procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

var
  cmd: ICmdLineParams;
begin
  if Supports(fpgApplication, ICmdLineParams, cmd) then
  begin
    if cmd.HasOption('n', 'newinstance') then
      MainProc
    else if not AnotherInstance then
      MainProc;
  end
  else
  begin
    if not AnotherInstance then
      MainProc;
  end;
end.


