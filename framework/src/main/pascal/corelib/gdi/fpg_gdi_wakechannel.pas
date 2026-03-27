{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      GDI (Windows) wake channel implementation.

      Posts a custom WM_USER message to a hidden message-only window.
      MsgWaitForMultipleObjects already monitors QS_ALLINPUT, so the
      posted message wakes it instantly. The message is consumed by
      the PeekMessageW loop in DoWaitWindowMessage as a no-op.
}

unit fpg_gdi_wakechannel;

{$mode objfpc}{$H+}

interface

uses
  Windows,
  fpg_wakeChannel;

const
  WM_FPGUI_WAKE = WM_USER + $F001;
  { HWND_MESSAGE is not defined in FPC 3.2.x Windows unit }
  {$IF NOT DECLARED(HWND_MESSAGE)}
  HWND_MESSAGE = HWND(-3);
  {$ENDIF}

type

  { TfpgGDIWakeChannel }

  TfpgGDIWakeChannel = class(TInterfacedObject, IWakeChannel)
  private
    FWakeWindow: HWND;
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


function WakeWndProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  if uMsg = WM_FPGUI_WAKE then
    Result := 0
  else
    Result := DefWindowProcW(hwnd, uMsg, wParam, lParam);
end;

const
  WakeClassName = 'fpGUIWakeChannel';

constructor TfpgGDIWakeChannel.Create;
begin
  inherited Create;
  FWakeWindow := 0;
end;

destructor TfpgGDIWakeChannel.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TfpgGDIWakeChannel.Open;
var
  wc: WNDCLASSW;
begin
  if FWakeWindow <> 0 then
    Exit;

  FillChar(wc, SizeOf(wc), 0);
  wc.lpfnWndProc := @WakeWndProc;
  wc.hInstance := MainInstance;
  wc.lpszClassName := WakeClassName;
  RegisterClassW(@wc);

  { HWND_MESSAGE creates a message-only window — invisible, no painting }
  FWakeWindow := CreateWindowExW(
    0,
    WakeClassName,
    nil,
    0,
    0, 0, 0, 0,
    HWND_MESSAGE,
    0,
    MainInstance,
    nil);
end;

procedure TfpgGDIWakeChannel.Close;
begin
  if FWakeWindow <> 0 then
  begin
    DestroyWindow(FWakeWindow);
    FWakeWindow := 0;
    UnregisterClassW(WakeClassName, MainInstance);
  end;
end;

procedure TfpgGDIWakeChannel.Signal;
begin
  if FWakeWindow <> 0 then
    PostMessage(FWakeWindow, WM_FPGUI_WAKE, 0, 0);
end;

procedure TfpgGDIWakeChannel.Drain;
begin
  { Nothing to drain — the PeekMessageW loop in DoWaitWindowMessage
    already consumes the posted WM_FPGUI_WAKE as a regular Windows
    message (handled by WakeWndProc as a no-op). }
end;

function TfpgGDIWakeChannel.GetPollFd: Integer;
begin
  { Windows does not use file descriptors for its message loop }
  Result := -1;
end;


end.
