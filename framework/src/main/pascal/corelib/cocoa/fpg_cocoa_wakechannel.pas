{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Cocoa (macOS) wake channel implementation using a self-pipe.

      The self-pipe approach works on macOS just as it does on X11.
      NSApp.nextEventMatchingMask:untilDate: blocks on the Cocoa run
      loop. We cannot easily inject a CFRunLoopSource from Object
      Pascal, but we can post a dummy NSEvent of type NSApplicationDefined
      to wake the run loop. The Cocoa backend's DoWaitWindowMessage
      will receive this event and treat it as a no-op.
}

unit fpg_cocoa_wakechannel;

{$mode objfpc}{$H+}

interface

uses
  CocoaAll,
  fpg_wakeChannel;

type

  { TfpgCocoaWakeChannel }

  TfpgCocoaWakeChannel = class(TInterfacedObject, IWakeChannel)
  private
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


constructor TfpgCocoaWakeChannel.Create;
begin
  inherited Create;
  FIsOpen := False;
end;

destructor TfpgCocoaWakeChannel.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TfpgCocoaWakeChannel.Open;
begin
  FIsOpen := True;
end;

procedure TfpgCocoaWakeChannel.Close;
begin
  FIsOpen := False;
end;

procedure TfpgCocoaWakeChannel.Signal;
var
  event: NSEvent;
begin
  if not FIsOpen then
    Exit;

  { Post a dummy application-defined event to the Cocoa event queue.
    This wakes NSApp.nextEventMatchingMask immediately. The event
    is consumed by DoWaitWindowMessage and discarded. }
  event := NSEvent.otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2(
    NSApplicationDefined,
    NSMakePoint(0, 0),
    0,
    0,
    0,
    nil,
    0,   { subtype }
    0,   { data1 }
    0    { data2 }
  );
  NSApp.postEvent_atStart(event, True);
end;

procedure TfpgCocoaWakeChannel.Drain;
begin
  { The event is consumed by the normal Cocoa event dispatch in
    DoWaitWindowMessage. Nothing to drain here. }
end;

function TfpgCocoaWakeChannel.GetPollFd: Integer;
begin
  { macOS does not use file descriptors for the Cocoa event loop }
  Result := -1;
end;


end.
