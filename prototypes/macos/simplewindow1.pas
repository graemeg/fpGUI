{ A simple MacOS Cocoa application with no menu bar.

  Use Ctrl+C from inside the console that launched the app to kill it. }
program simplewindow1;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

uses
  CocoaAll;

var
  window: NSWindow;
//  pool: NSAutoreleasePool;
begin
//  pool := NSAutoreleasePool.new;
  NSApp := NSApplication.sharedApplication;
  NSApp.setActivationPolicy(NSApplicationActivationPolicyRegular);

  window := NSWindow.alloc.initWithContentRect_styleMask_backing_defer(NSMakeRect(0, 0, 200, 200),
    NSTitledWindowMask, NSBackingStoreBuffered, False).autorelease;
  window.center;
  window.setTitle(NSStr('Simple Window demo 1'));
  window.makeKeyAndOrderFront(nil);
  NSApp.activateIgnoringOtherApps(true);
  NSApp.run;
end.
