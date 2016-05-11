{
  Copyright (c) 2013-2016, Graeme Geldenhuys
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this
     list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  Description:
    This is a small yet useful text editor.
}

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


