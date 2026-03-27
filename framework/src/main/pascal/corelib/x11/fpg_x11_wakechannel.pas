{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      X11 wake channel implementation using a self-pipe.

      When Signal() is called from any thread, a single byte is written
      to the write end of the pipe. The read end is monitored by
      fpSelect() in DoWaitWindowMessage alongside the X11 display fd.
      When select() returns due to the pipe being readable, Drain()
      consumes all pending bytes, resetting the channel for the next
      signal.

      This is the same pattern used by Qt, GTK, and libev on Unix.
}

unit fpg_x11_wakechannel;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  BaseUnix,
  fpg_wakeChannel;

type

  { TfpgX11WakeChannel }

  TfpgX11WakeChannel = class(TInterfacedObject, IWakeChannel)
  private
    FPipeFds: array[0..1] of cint;  { [0]=read, [1]=write }
    FIsOpen: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    { IWakeChannel }
    procedure Open;
    procedure Close;
    procedure Signal;
    procedure Drain;
    function GetPollFd: Integer;
  end;


implementation

uses
  Unix;


constructor TfpgX11WakeChannel.Create;
begin
  inherited Create;
  FPipeFds[0] := -1;
  FPipeFds[1] := -1;
  FIsOpen := False;
end;

destructor TfpgX11WakeChannel.Destroy;
begin
  if FIsOpen then
    Close;
  inherited Destroy;
end;

procedure TfpgX11WakeChannel.Open;
var
  flags: cint;
begin
  if FIsOpen then
    Exit;

  if fpPipe(FPipeFds) <> 0 then
    raise Exception.Create('fpGUI: failed to create wake channel pipe');

  { Set both ends to non-blocking }
  flags := FpFcntl(FPipeFds[0], F_GETFL);
  FpFcntl(FPipeFds[0], F_SETFL, flags or O_NONBLOCK);

  flags := FpFcntl(FPipeFds[1], F_GETFL);
  FpFcntl(FPipeFds[1], F_SETFL, flags or O_NONBLOCK);

  FIsOpen := True;
end;

procedure TfpgX11WakeChannel.Close;
begin
  if not FIsOpen then
    Exit;
  FpClose(FPipeFds[0]);
  FpClose(FPipeFds[1]);
  FPipeFds[0] := -1;
  FPipeFds[1] := -1;
  FIsOpen := False;
end;

procedure TfpgX11WakeChannel.Signal;
var
  buf: Byte;
begin
  if not FIsOpen then
    Exit;
  buf := 1;
  { Non-blocking write. If the pipe is already full (64 KB on Linux),
    the write silently fails — that's fine because Signal is idempotent:
    one or more bytes in the pipe all mean "wake up". }
  FpWrite(FPipeFds[1], buf, 1);
end;

procedure TfpgX11WakeChannel.Drain;
var
  buf: array[0..63] of Byte;
begin
  if not FIsOpen then
    Exit;
  { Read until empty. Non-blocking, so this returns immediately when
    no more bytes are available. }
  while FpRead(FPipeFds[0], buf, SizeOf(buf)) > 0 do
    { drain };
end;

function TfpgX11WakeChannel.GetPollFd: Integer;
begin
  Result := FPipeFds[0];
end;


end.
