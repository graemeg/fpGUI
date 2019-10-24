{ A simple MacOS Cocoa application with a menu bar.
  To get the menu to work, the application must be run from 
  a application bundle. }
program simplewindow2;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

uses
  CocoaAll;

var
  menubar: NSMenu;
  appMenu: NSMenu;
  appName: NSString;
  quitMenuItem: NSMenuItem;
  appMenuItem: NSMenuItem;
  quitTitle: NSString;
  window: NSWindow;
  pool: NSAutoreleasePool;
begin
  pool := NSAutoreleasePool.new;
  NSApp := NSApplication.sharedApplication;
  NSApp.setActivationPolicy(NSApplicationActivationPolicyRegular);
  menubar := NSMenu.alloc.autorelease;
  appMenuItem := NSMenuItem.alloc.autorelease;
  menubar.addItem(appMenuItem);
  NSApp.setMainMenu(Menubar);
  appMenu := NSMenu.new.autorelease;
  // appName := NSProcessInfo.processInfo.processName;
  appName := NSSTR('Simple Window 2 demo 2');
  quitTitle := NSSTR('Quit ').stringByAppendingString(appName);
  quitMenuItem := NSMenuItem.alloc.initWithTitle_action_keyEquivalent(quitTitle,
    sel_registerName(PChar('terminate:')),
    NSSTR('q'));
  appMenu.addItem(quitMenuItem);
  appMenuitem.setSubmenu(appMenu);
  window := NSWindow.alloc.initWithContentRect_styleMask_backing_defer(NSMakeRect(0, 0, 200, 200),
    NSTitledWindowMask, NSBackingStoreBuffered, False).autorelease;
  window.center;
  window.setTitle(appName);
  window.makeKeyAndOrderFront(nil);
  NSApp.activateIgnoringOtherApps(true);
  NSApp.run;
end.