{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Wake channel interface for cross-thread event loop notification.

      Platform backends implement IWakeChannel to provide a mechanism
      for worker threads to wake the main GUI thread's event loop
      immediately, without waiting for a platform event or timeout.

      This is the same pattern used by Qt (eventfd/self-pipe),
      Java Swing (EDT notification), and GTK (g_main_context_wakeup).
}

unit fpg_wakeChannel;

{$mode objfpc}{$H+}

interface


type

  { IWakeChannel - Platform-independent wake mechanism.

    Signal() is thread-safe and idempotent: multiple calls before a
    single Drain() collapse into one wake event. Implementations must
    ensure Signal() never blocks the caller. }

  IWakeChannel = interface
    ['{A3E2F8D1-7C4B-4A9E-B6D5-1F3E8C2A9B70}']

    { Called once during application startup to create the OS resource
      (pipe, eventfd, posted message, CFRunLoopSource, etc.) }
    procedure Open;

    { Called during application shutdown to release the OS resource. }
    procedure Close;

    { Thread-safe, non-blocking, idempotent.
      Wakes the event loop by signalling the OS primitive. }
    procedure Signal;

    { Called by the main thread after the platform wait returns.
      Consumes any pending signal bytes/messages so the channel
      is ready for the next Signal(). }
    procedure Drain;

    { Returns the file descriptor (or INVALID_HANDLE on non-fd platforms)
      that the event loop should monitor in select()/poll().
      On Windows and Cocoa this returns -1 because the wake mechanism
      uses native message posting or CFRunLoop signalling instead of
      a pollable fd. }
    function GetPollFd: Integer;
  end;


implementation


end.
