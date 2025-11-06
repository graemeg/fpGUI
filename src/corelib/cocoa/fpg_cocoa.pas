{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2019 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit implements MacOS Cocoa support for fpGUI.
}

unit fpg_cocoa;

{$I fpg_defines.inc}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  ctypes,
  CocoaAll,
  fpg_base,
  fpg_impl;

type
  // Forward declarations
  TfpgCocoaWindow = class;

  TfpgCocoaFontResource = class(TfpgFontResourceBase)
  public
    constructor Create(const afontdesc: string); override;
    function    GetAscent: integer; override;
    function    GetDescent: integer; override;
    function    GetHeight: integer; override;
    function    GetTextWidth(const txt: string): integer; override;
    function    HandleIsValid: boolean; override;
  end;


  TfpgCocoaImage = class(TfpgImageBase)
  protected
    procedure   DoFreeImage; override;
    procedure   DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: Pointer); override;
    procedure   DoInitImageMask(awidth, aheight: integer; aimgdata: Pointer); override;
  end;
  
  // graemeg: Since we are going to use AggCanvas, this is probably not needed at all.
  TfpgCocoaCanvas = class(TfpgCanvasBase)
  protected
    procedure   DoSetFontRes(fntres: TfpgFontResourceBase); override;
    procedure   DoSetTextColor(cl: TfpgColor); override;
    procedure   DoSetColor(cl: TfpgColor); override;
    procedure   DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle); override;
    procedure   DoFillRectangle(x, y, w, h: TfpgCoord); override;
    procedure   DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord); override;
    procedure   DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord); override;
    procedure   DoDrawRectangle(x, y, w, h: TfpgCoord); override;
    procedure   DoDrawLine(x1, y1, x2, y2: TfpgCoord); override;
    procedure   DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer); override;
    procedure   DoDrawString(x, y: TfpgCoord; const txt: string); override;
    procedure   DoSetClipRect(const ARect: TfpgRect); override;
    function    DoGetClipRect: TfpgRect; override;
    procedure   DoAddClipRect(const ARect: TfpgRect); override;
    procedure   DoClearClipRect; override;
    procedure   DoBeginDraw(awidget: TfpgWidgetBase; CanvasTarget: TfpgCanvasBase); override;
    procedure   DoPutBufferToScreen(x, y, w, h: TfpgCoord); override;
    procedure   DoEndDraw; override;
    function    GetPixel(X, Y: integer): TfpgColor; override;
    procedure   SetPixel(X, Y: integer; const AValue: TfpgColor); override;
    procedure   DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: Extended); override;
    procedure   DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended); override;
    procedure   DoDrawPolygon(const Points: array of TPoint); override;
    function    GetBufferAllocated: Boolean; override;
    procedure   DoAllocateBuffer; override;
  end;
  
  
  { Window delegate for handling window events }
  TfpgCocoaWindowDelegate = objcclass(NSObject, NSWindowDelegateProtocol)
  private
    FWindow: TfpgCocoaWindow;
  public
    procedure setWindow(AWindow: TfpgCocoaWindow); message 'setWindow:';
    procedure windowDidResize(notification: NSNotification); message 'windowDidResize:';
    procedure windowDidMove(notification: NSNotification); message 'windowDidMove:';
    procedure windowDidBecomeKey(notification: NSNotification); message 'windowDidBecomeKey:';
    procedure windowDidResignKey(notification: NSNotification); message 'windowDidResignKey:';
    function windowShouldClose(sender: id): Boolean; message 'windowShouldClose:';
  end;

  { Custom NSView subclass for handling rendering and events }
  TfpgCocoaView = objcclass(NSView)
  private
    FWindow: TfpgCocoaWindow;
  public
    procedure drawRect(dirtyRect: NSRect); override;
    function  acceptsFirstResponder: Boolean; override;
    function  isFlipped: Boolean; override;
    procedure setWindow(AWindow: TfpgCocoaWindow); message 'setWindow:';

    // Mouse events
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;

    // Keyboard events
    procedure keyDown(event: NSEvent); override;
    procedure keyUp(event: NSEvent); override;
  end;

  TfpgCocoaWindow = class(TfpgWindowBase)
  private
    FWinHandle: NSWindow;
    FView: TfpgCocoaView;
    FDelegate: TfpgCocoaWindowDelegate;
  protected
    FModalForWin: TfpgCocoaWindow;
    function    HandleIsValid: boolean; override;
    procedure   DoUpdateWindowPosition; override;
    procedure   DoAllocateWindowHandle(AParent: TfpgWidgetBase); override;
    procedure   DoReleaseWindowHandle; override;
    procedure   DoRemoveWindowLookup; override;
    procedure   DoSetWindowAttributes(const AOldAtributes, ANewAttributes: TWindowAttributes; const AForceAll: Boolean); override;
    procedure   DoSetWindowVisible(const AValue: Boolean); override;
    procedure   DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord); override;
    function    DoWindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint; override;
    procedure   DoSetWindowTitle(const ATitle: string); override;
    procedure   DoSetMouseCursor; override;
    procedure   DoDNDEnabled(const AValue: boolean); override;
    property    WinHandle: NSWindow read FWinHandle;
    property    View: TfpgCocoaView read FView;
  end;
  
  
  TfpgCocoaApplication = class(TfpgApplicationBase)
  private
    function    ConvertShiftState(modifierFlags: NSUInteger): TShiftState;
    function    ConvertKeyCode(keyCode: cushort): Word;
  protected
    function    DoGetFontFaceList: TStringList; override;
    procedure   DoWaitWindowMessage(atimeoutms: integer); override;
    function    MessagesPending: boolean; override;
    procedure   DoFlush; override;
  public
    constructor Create(const AParams: string); override;
    function    GetScreenWidth: TfpgCoord; override;
    function    GetScreenHeight: TfpgCoord; override;
    function    GetScreenPixelColor(APos: TPoint): TfpgColor; override;
    function    Screen_dpi_x: integer; override;
    function    Screen_dpi_y: integer; override;
    function    Screen_dpi: integer; override;
  end;
  
  
  TfpgCocoaClipboard = class(TfpgClipboardBase)
  protected
    function    DoGetText: TfpgString; override;
    procedure   DoSetText(const AValue: TfpgString); override;
    procedure   InitClipboard; override;
  end;
  
  
  TfpgCocoaFileList = class(TfpgFileListBase)
  end;
  
  
  TfpgCocoaMimeData = class(TfpgMimeDataBase)
  end;
  
  
  TfpgCocoaDrag = class(TfpgDragBase)
  public
    function    Execute(const ADropActions: TfpgDropActions; const ADefaultAction: TfpgDropAction=daCopy): TfpgDropAction; override;
  end;
  
  
  TfpgCocoaDrop = class(TfpgDropBase)
  protected
    function    GetDropAction: TfpgDropAction; override;
    procedure   SetDropAction(AValue: TfpgDropAction); override;
    function    GetWindowForDrop: TfpgWindowBase; override;
  end;
  
  
  TfpgCocoaTimer = class(TfpgBaseTimer)
  end;
  

  TfpgCocoaSystemTrayHandler = class(TfpgSystemTrayHandlerBase)
  public
    procedure   Show; override;
    procedure   Hide; override;
    function    IsSystemTrayAvailable: boolean; override;
    function    SupportsMessages: boolean; override;
  end;
  
  
  
implementation

uses
  baseunix,
  unix,
  fpg_main,
  fpg_widget,
  fpg_popupwindow,
  fpg_window,       // used for window attributes changed callback
  fpg_stringutils,  // used for GetTextWidth
  fpg_utils,
  fpg_form,         // for modal event support
  fpg_cmdlineparams,
  fpg_constants;

{ Helper function to convert NSString to String }
function NSStringToString(ns: NSString): String;
begin
  if Assigned(ns) then
    Result := String(ns.UTF8String)
  else
    Result := '';
end; 

{ TfpgCocoaFontResource }
{ NOTE: When using AggCanvas, TfpgAgg2DFontResource is used instead }
{ This implementation is here for completeness but won't be used with AggCanvas }

constructor TfpgCocoaFontResource.Create(const afontdesc: string);
begin
  inherited Create(afontdesc);  // Call base constructor to set FFontDesc
  // When using AggCanvas, font rendering is handled by TfpgAgg2DFontResource
  // This class would only be used if implementing native Cocoa font rendering
end;

function TfpgCocoaFontResource.GetAscent: integer;
begin
  // Stub - not used with AggCanvas
  Result := 10;
end;

function TfpgCocoaFontResource.GetDescent: integer;
begin
  // Stub - not used with AggCanvas
  Result := 2;
end;

function TfpgCocoaFontResource.GetHeight: integer;
begin
  // Stub - not used with AggCanvas
  Result := 12;
end;

function TfpgCocoaFontResource.GetTextWidth(const txt: string): integer;
begin
  // Stub - not used with AggCanvas
  Result := Length(txt) * 8;  // Rough estimate
end;

function TfpgCocoaFontResource.HandleIsValid: boolean;
begin
  // Stub - not used with AggCanvas
  Result := True;
end;

{ TfpgCocoaImage }

procedure TfpgCocoaImage.DoFreeImage;
begin
  // When using AggCanvas, image cleanup is handled by base class
  // No platform-specific cleanup needed for Cocoa
end;

procedure TfpgCocoaImage.DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: Pointer);
begin
  // When using AggCanvas, images are rendered through Agg2D
  // The image data is stored in the base class (ImageData pointer)
  // and rendered via DoPutBufferToScreen

  // Just store the basic properties
  FMasked := False;
  // Image data is already handled by base class TfpgImageBase
end;

procedure TfpgCocoaImage.DoInitImageMask(awidth, aheight: integer; aimgdata: Pointer);
begin
  // Set up alpha mask for transparency
  FMasked := True;
  // Mask data handling would go here if needed for platform-specific rendering
  // With AggCanvas, transparency is handled through the RGBA buffer
end;

{ TfpgCocoaWindowDelegate }

procedure TfpgCocoaWindowDelegate.setWindow(AWindow: TfpgCocoaWindow);
begin
  FWindow := AWindow;
end;

procedure TfpgCocoaWindowDelegate.windowDidResize(notification: NSNotification);
var
  msgp: TfpgMessageParams;
  frame: NSRect;
begin
  if not Assigned(FWindow) then
    exit;

  frame := NSWindow(notification.object_).frame;

  fillchar(msgp, sizeof(msgp), 0);
  msgp.rect.Width := Round(frame.size.width);
  msgp.rect.Height := Round(frame.size.height);

  fpgPostMessage(nil, FWindow, FPGM_RESIZE, msgp);
end;

procedure TfpgCocoaWindowDelegate.windowDidMove(notification: NSNotification);
var
  msgp: TfpgMessageParams;
  frame: NSRect;
begin
  if not Assigned(FWindow) then
    exit;

  frame := NSWindow(notification.object_).frame;

  fillchar(msgp, sizeof(msgp), 0);
  msgp.rect.Left := Round(frame.origin.x);
  msgp.rect.Top := Round(NSScreen.mainScreen.frame.size.height - frame.origin.y - frame.size.height);

  fpgPostMessage(nil, FWindow, FPGM_MOVE, msgp);
end;

procedure TfpgCocoaWindowDelegate.windowDidBecomeKey(notification: NSNotification);
var
  msgp: TfpgMessageParams;
begin
  if not Assigned(FWindow) then
    exit;

  fillchar(msgp, sizeof(msgp), 0);
  fpgPostMessage(nil, FWindow, FPGM_ACTIVATE, msgp);
end;

procedure TfpgCocoaWindowDelegate.windowDidResignKey(notification: NSNotification);
var
  msgp: TfpgMessageParams;
begin
  if not Assigned(FWindow) then
    exit;

  fillchar(msgp, sizeof(msgp), 0);
  fpgPostMessage(nil, FWindow, FPGM_DEACTIVATE, msgp);
end;

function TfpgCocoaWindowDelegate.windowShouldClose(sender: id): Boolean;
var
  msgp: TfpgMessageParams;
begin
  Result := True;  // Default: allow close

  if not Assigned(FWindow) then
    exit;

  // Post close message to allow fpGUI to handle it
  fillchar(msgp, sizeof(msgp), 0);
  fpgPostMessage(nil, FWindow, FPGM_CLOSE, msgp);

  // Return false to prevent automatic close - fpGUI will handle it
  Result := False;
end;

{ TfpgCocoaView }

procedure TfpgCocoaView.drawRect(dirtyRect: NSRect);
var
  msgp: TfpgMessageParams;
begin
  // This will be called by Cocoa when the view needs to be redrawn
  if Assigned(FWindow) then
  begin
    // Trigger fpGUI paint event
    fillchar(msgp, sizeof(msgp), 0);
    msgp.rect.Left := Round(dirtyRect.origin.x);
    msgp.rect.Top := Round(dirtyRect.origin.y);
    msgp.rect.Width := Round(dirtyRect.size.width);
    msgp.rect.Height := Round(dirtyRect.size.height);

    fpgPostMessage(nil, FWindow, FPGM_PAINT, msgp);
  end;
end;

function TfpgCocoaView.acceptsFirstResponder: Boolean;
begin
  Result := True;  // Allow view to receive keyboard events
end;

function TfpgCocoaView.isFlipped: Boolean;
begin
  Result := True;  // Use top-left origin like fpGUI expects
end;

procedure TfpgCocoaView.setWindow(AWindow: TfpgCocoaWindow);
begin
  FWindow := AWindow;
end;

procedure TfpgCocoaView.mouseDown(event: NSEvent);
var
  msgp: TfpgMessageParams;
  pt: NSPoint;
begin
  if not Assigned(FWindow) then
    exit;

  pt := convertPoint_fromView(event.locationInWindow, nil);
  fillchar(msgp, sizeof(msgp), 0);
  msgp.mouse.x := Round(pt.x);
  msgp.mouse.y := Round(pt.y);
  msgp.mouse.Buttons := MOUSE_LEFT;
  msgp.mouse.shiftstate := TfpgCocoaApplication(fpgApplication).ConvertShiftState(event.modifierFlags);

  fpgPostMessage(nil, FWindow, FPGM_MOUSEDOWN, msgp);
end;

procedure TfpgCocoaView.mouseUp(event: NSEvent);
var
  msgp: TfpgMessageParams;
  pt: NSPoint;
begin
  if not Assigned(FWindow) then
    exit;

  pt := convertPoint_fromView(event.locationInWindow, nil);
  fillchar(msgp, sizeof(msgp), 0);
  msgp.mouse.x := Round(pt.x);
  msgp.mouse.y := Round(pt.y);
  msgp.mouse.Buttons := MOUSE_LEFT;
  msgp.mouse.shiftstate := TfpgCocoaApplication(fpgApplication).ConvertShiftState(event.modifierFlags);

  fpgPostMessage(nil, FWindow, FPGM_MOUSEUP, msgp);
end;

procedure TfpgCocoaView.mouseMoved(event: NSEvent);
var
  msgp: TfpgMessageParams;
  pt: NSPoint;
begin
  if not Assigned(FWindow) then
    exit;

  pt := convertPoint_fromView(event.locationInWindow, nil);
  fillchar(msgp, sizeof(msgp), 0);
  msgp.mouse.x := Round(pt.x);
  msgp.mouse.y := Round(pt.y);
  msgp.mouse.shiftstate := TfpgCocoaApplication(fpgApplication).ConvertShiftState(event.modifierFlags);

  fpgPostMessage(nil, FWindow, FPGM_MOUSEMOVE, msgp);
end;

procedure TfpgCocoaView.mouseDragged(event: NSEvent);
var
  msgp: TfpgMessageParams;
  pt: NSPoint;
begin
  if not Assigned(FWindow) then
    exit;

  pt := convertPoint_fromView(event.locationInWindow, nil);
  fillchar(msgp, sizeof(msgp), 0);
  msgp.mouse.x := Round(pt.x);
  msgp.mouse.y := Round(pt.y);
  msgp.mouse.Buttons := MOUSE_LEFT;  // Left button held during drag
  msgp.mouse.shiftstate := TfpgCocoaApplication(fpgApplication).ConvertShiftState(event.modifierFlags);

  fpgPostMessage(nil, FWindow, FPGM_MOUSEMOVE, msgp);
end;

procedure TfpgCocoaView.mouseEntered(event: NSEvent);
var
  msgp: TfpgMessageParams;
  pt: NSPoint;
begin
  if not Assigned(FWindow) then
    exit;

  pt := convertPoint_fromView(event.locationInWindow, nil);
  fillchar(msgp, sizeof(msgp), 0);
  msgp.mouse.x := Round(pt.x);
  msgp.mouse.y := Round(pt.y);
  msgp.mouse.shiftstate := TfpgCocoaApplication(fpgApplication).ConvertShiftState(event.modifierFlags);

  fpgPostMessage(nil, FWindow, FPGM_MOUSEENTER, msgp);
end;

procedure TfpgCocoaView.mouseExited(event: NSEvent);
var
  msgp: TfpgMessageParams;
  pt: NSPoint;
begin
  if not Assigned(FWindow) then
    exit;

  pt := convertPoint_fromView(event.locationInWindow, nil);
  fillchar(msgp, sizeof(msgp), 0);
  msgp.mouse.x := Round(pt.x);
  msgp.mouse.y := Round(pt.y);
  msgp.mouse.shiftstate := TfpgCocoaApplication(fpgApplication).ConvertShiftState(event.modifierFlags);

  fpgPostMessage(nil, FWindow, FPGM_MOUSEEXIT, msgp);
end;

procedure TfpgCocoaView.rightMouseDown(event: NSEvent);
var
  msgp: TfpgMessageParams;
  pt: NSPoint;
begin
  if not Assigned(FWindow) then
    exit;

  pt := convertPoint_fromView(event.locationInWindow, nil);
  fillchar(msgp, sizeof(msgp), 0);
  msgp.mouse.x := Round(pt.x);
  msgp.mouse.y := Round(pt.y);
  msgp.mouse.Buttons := MOUSE_RIGHT;
  msgp.mouse.shiftstate := TfpgCocoaApplication(fpgApplication).ConvertShiftState(event.modifierFlags);

  fpgPostMessage(nil, FWindow, FPGM_MOUSEDOWN, msgp);
end;

procedure TfpgCocoaView.rightMouseUp(event: NSEvent);
var
  msgp: TfpgMessageParams;
  pt: NSPoint;
begin
  if not Assigned(FWindow) then
    exit;

  pt := convertPoint_fromView(event.locationInWindow, nil);
  fillchar(msgp, sizeof(msgp), 0);
  msgp.mouse.x := Round(pt.x);
  msgp.mouse.y := Round(pt.y);
  msgp.mouse.Buttons := MOUSE_RIGHT;
  msgp.mouse.shiftstate := TfpgCocoaApplication(fpgApplication).ConvertShiftState(event.modifierFlags);

  fpgPostMessage(nil, FWindow, FPGM_MOUSEUP, msgp);
end;

procedure TfpgCocoaView.scrollWheel(event: NSEvent);
var
  msgp: TfpgMessageParams;
  pt: NSPoint;
  delta: Double;
begin
  if not Assigned(FWindow) then
    exit;

  pt := convertPoint_fromView(event.locationInWindow, nil);
  delta := event.deltaY;

  fillchar(msgp, sizeof(msgp), 0);
  msgp.mouse.x := Round(pt.x);
  msgp.mouse.y := Round(pt.y);
  msgp.mouse.delta := Round(delta);
  msgp.mouse.shiftstate := TfpgCocoaApplication(fpgApplication).ConvertShiftState(event.modifierFlags);

  fpgPostMessage(nil, FWindow, FPGM_SCROLL, msgp);
end;

procedure TfpgCocoaView.keyDown(event: NSEvent);
var
  msgp: TfpgMessageParams;
  keyCode: Word;
  keyChar: string;
begin
  if not Assigned(FWindow) then
    exit;

  fillchar(msgp, sizeof(msgp), 0);
  keyCode := TfpgCocoaApplication(fpgApplication).ConvertKeyCode(event.keyCode);
  keyChar := NSStringToString(event.characters);

  msgp.keyboard.keycode := keyCode;
  if Length(keyChar) > 0 then
    msgp.keyboard.keychar := keyChar[1]
  else
    msgp.keyboard.keychar := #0;
  msgp.keyboard.shiftstate := TfpgCocoaApplication(fpgApplication).ConvertShiftState(event.modifierFlags);

  fpgPostMessage(nil, FWindow, FPGM_KEYPRESS, msgp);
end;

procedure TfpgCocoaView.keyUp(event: NSEvent);
var
  msgp: TfpgMessageParams;
  keyCode: Word;
begin
  if not Assigned(FWindow) then
    exit;

  fillchar(msgp, sizeof(msgp), 0);
  keyCode := TfpgCocoaApplication(fpgApplication).ConvertKeyCode(event.keyCode);

  msgp.keyboard.keycode := keyCode;
  msgp.keyboard.shiftstate := TfpgCocoaApplication(fpgApplication).ConvertShiftState(event.modifierFlags);

  fpgPostMessage(nil, FWindow, FPGM_KEYRELEASE, msgp);
end;

{ TfpgCocoaWindow }

function TfpgCocoaWindow.HandleIsValid: boolean;
begin
  Result := Assigned(FWinHandle);
end;

procedure TfpgCocoaWindow.DoUpdateWindowPosition;
var
  r: NSRect;
begin
  if not HandleIsValid then
    Exit;
  r := FWinHandle.frame;
  FPosition.X := Round(r.origin.x);
  // Cocoa uses bottom-left origin, fpGUI uses top-left
  FPosition.Y := Round(NSScreen.mainScreen.frame.size.height - r.origin.y - r.size.height);
  FSize.W := Round(r.size.width);
  FSize.H := Round(r.size.height);
end;

procedure TfpgCocoaWindow.DoAllocateWindowHandle(AParent: TfpgWidgetBase);
var
  styleMask: NSUInteger;
  contentRect: NSRect;
  parentWin: TfpgCocoaWindow;
begin
  // Don't create if already exists
  if HandleIsValid then
    Exit;

  // Determine window style
  styleMask := NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask;

  if waSizeable in FWindowAttributes then
    styleMask := styleMask or NSResizableWindowMask;

  // Create content rect - convert fpGUI top-left to Cocoa bottom-left
  contentRect := NSMakeRect(FPosition.X,
                            NSScreen.mainScreen.frame.size.height - FPosition.Y - FSize.H,
                            FSize.W, FSize.H);

  // Create the window
  FWinHandle := NSWindow.alloc.initWithContentRect_styleMask_backing_defer(
    contentRect, styleMask, NSBackingStoreBuffered, False);

  if not Assigned(FWinHandle) then
    raise Exception.Create('Failed to create Cocoa window');

  // Create and set delegate for window events
  FDelegate := TfpgCocoaWindowDelegate.alloc.init;
  FDelegate.setWindow(Self);
  FWinHandle.setDelegate(FDelegate);

  // Create custom view for rendering
  FView := TfpgCocoaView.alloc.initWithFrame(NSMakeRect(0, 0, FSize.W, FSize.H));
  FView.setWindow(Self);

  FWinHandle.setContentView(FView);
  FWinHandle.setAcceptsMouseMovedEvents(True);

  // Handle parent window relationship
  if Assigned(AParent) then
  begin
    parentWin := TfpgCocoaWindow(AParent.Window);
    if Assigned(parentWin) and parentWin.HandleIsValid then
      parentWin.FWinHandle.addChildWindow_ordered(FWinHandle, NSWindowAbove);
  end;
end;

procedure TfpgCocoaWindow.DoReleaseWindowHandle;
begin
  if not HandleIsValid then
    Exit;

  // Clear delegate
  if Assigned(FDelegate) then
  begin
    FWinHandle.setDelegate(nil);
    FDelegate.release;
    FDelegate := nil;
  end;

  // Close and release window
  FWinHandle.close;
  FWinHandle.release;
  FWinHandle := nil;

  // Release view
  if Assigned(FView) then
  begin
    FView.release;
    FView := nil;
  end;
end;

procedure TfpgCocoaWindow.DoRemoveWindowLookup;
begin
  // Cleanup window lookup table if we implement one
end;

procedure TfpgCocoaWindow.DoSetWindowAttributes(const AOldAtributes, ANewAttributes: TWindowAttributes; const AForceAll: Boolean);
var
  styleMask: NSUInteger;
begin
  if not HandleIsValid then
    Exit;

  // Update window style if resizable attribute changed
  if (waSizeable in ANewAttributes) <> (waSizeable in AOldAtributes) then
  begin
    styleMask := FWinHandle.styleMask;
    if waSizeable in ANewAttributes then
      styleMask := styleMask or NSResizableWindowMask
    else
      styleMask := styleMask and (not NSResizableWindowMask);
    FWinHandle.setStyleMask(styleMask);
  end;

  // Handle other attributes as needed
  if waFullScreen in ANewAttributes then
  begin
    if not (waFullScreen in AOldAtributes) then
      FWinHandle.toggleFullScreen(nil);
  end;
end;

procedure TfpgCocoaWindow.DoSetWindowVisible(const AValue: Boolean);
begin
  if not HandleIsValid then
    Exit;

  if AValue then
  begin
    FWinHandle.makeKeyAndOrderFront(nil);
    FWinHandle.orderFrontRegardless;
  end
  else
    FWinHandle.orderOut(nil);
end;

procedure TfpgCocoaWindow.DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord);
var
  screenHeight: Double;
  cocoaY: Double;
  newOrigin: NSPoint;
begin
  if not HandleIsValid then
    Exit;

  // Convert fpGUI top-left to Cocoa bottom-left
  screenHeight := NSScreen.mainScreen.frame.size.height;
  cocoaY := screenHeight - y - FSize.H;
  newOrigin := NSMakePoint(x, cocoaY);

  FWinHandle.setFrameOrigin(newOrigin);
end;

function TfpgCocoaWindow.DoWindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint;
var
  winFrame: NSRect;
begin
  Result := AScreenPos;
  if not HandleIsValid then
    Exit;

  winFrame := FWinHandle.frame;
  Result.X := AScreenPos.X + Round(winFrame.origin.x);
  // Convert Cocoa bottom-left to fpGUI top-left
  Result.Y := AScreenPos.Y + Round(NSScreen.mainScreen.frame.size.height -
                                    winFrame.origin.y - winFrame.size.height);
end;

procedure TfpgCocoaWindow.DoSetWindowTitle(const ATitle: string);
begin
  if not HandleIsValid then
    Exit;
  FWinHandle.setTitle(NSString(NSSTR(PChar(ATitle))));
end;

procedure TfpgCocoaWindow.DoSetMouseCursor;
begin
  // TODO: Implement mouse cursor changes
  // Will need to map fpGUI cursor types to NSCursor types
end;

procedure TfpgCocoaWindow.DoDNDEnabled(const AValue: boolean);
begin
  // TODO: Implement drag-and-drop support
end;

{ TfpgCocoaApplication }

constructor TfpgCocoaApplication.Create(const AParams: string);
begin
  inherited Create(AParams);

  // Initialize NSApplication if not already done
  NSApp := NSApplication.sharedApplication;
  NSApp.setActivationPolicy(NSApplicationActivationPolicyRegular);

  FIsInitialized := True;
end;

function TfpgCocoaApplication.DoGetFontFaceList: TStringList;
var
  fontManager: NSFontManager;
  fontFamilies: NSArray;
  i: Integer;
  fontName: NSString;
begin
  Result := TStringList.Create;
  try
    fontManager := NSFontManager.sharedFontManager;
    fontFamilies := fontManager.availableFontFamilies;

    for i := 0 to fontFamilies.count - 1 do
    begin
      fontName := NSString(fontFamilies.objectAtIndex(i));
      Result.Add(NSStringToString(fontName));
    end;

    Result.Sort;
  except
    Result.Free;
    raise;
  end;
end;

procedure TfpgCocoaApplication.DoWaitWindowMessage(atimeoutms: integer);
var
  event: NSEvent;
  pool: NSAutoreleasePool;
  timeoutDate: NSDate;
begin
  pool := NSAutoreleasePool.alloc.init;
  try
    // Set timeout
    if atimeoutms > 0 then
      timeoutDate := NSDate.dateWithTimeIntervalSinceNow(atimeoutms / 1000.0)
    else
      timeoutDate := NSDate.distantFuture;

    // Get next event
    event := NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(
      NSAnyEventMask,
      timeoutDate,
      NSDefaultRunLoopMode,
      True);

    if Assigned(event) then
    begin
      // Process the event
      NSApp.sendEvent(event);
      NSApp.updateWindows;
    end;
  finally
    pool.release;
  end;
end;

function TfpgCocoaApplication.MessagesPending: boolean;
var
  event: NSEvent;
  pool: NSAutoreleasePool;
begin
  pool := NSAutoreleasePool.alloc.init;
  try
    // Check if there's an event without removing it from queue
    event := NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(
      NSAnyEventMask,
      NSDate.distantPast,  // Don't wait
      NSDefaultRunLoopMode,
      False);  // Don't dequeue

    Result := Assigned(event);
  finally
    pool.release;
  end;
end;

procedure TfpgCocoaApplication.DoFlush;
begin
  // Flush any pending drawing operations
  NSApp.updateWindows;
end;

function TfpgCocoaApplication.GetScreenWidth: TfpgCoord;
var
  screenRect: NSRect;
begin
  screenRect := NSScreen.mainScreen.frame;
  Result := Round(screenRect.size.width);
end;

function TfpgCocoaApplication.GetScreenHeight: TfpgCoord;
var
  screenRect: NSRect;
begin
  screenRect := NSScreen.mainScreen.frame;
  Result := Round(screenRect.size.height);
end;

function TfpgCocoaApplication.GetScreenPixelColor(APos: TPoint): TfpgColor;
begin
  // TODO: Implement screen pixel color reading
  // This would require using CGWindowListCreateImage or similar
  Result := 0;
end;

function TfpgCocoaApplication.Screen_dpi_x: integer;
begin
  // macOS standard DPI
  // Note: Real DPI calculation would need CGDisplayScreenSize
  // which requires additional APIs not exposed in CocoaAll
  Result := 72;
end;

function TfpgCocoaApplication.Screen_dpi_y: integer;
begin
  Result := Screen_dpi_x;  // macOS uses square pixels
end;

function TfpgCocoaApplication.Screen_dpi: integer;
begin
  Result := Screen_dpi_x;
end;

function TfpgCocoaApplication.ConvertShiftState(modifierFlags: NSUInteger): TShiftState;
begin
  Result := [];

  if (modifierFlags and NSShiftKeyMask) <> 0 then
    Include(Result, ssShift);

  if (modifierFlags and NSControlKeyMask) <> 0 then
    Include(Result, ssCtrl);

  if (modifierFlags and NSAlternateKeyMask) <> 0 then
    Include(Result, ssAlt);

  if (modifierFlags and NSCommandKeyMask) <> 0 then
    Include(Result, ssMeta);  // Map Command key to Meta

  // Mouse buttons are handled separately in mouse events
end;

function TfpgCocoaApplication.ConvertKeyCode(keyCode: cushort): Word;
begin
  // Map Cocoa virtual key codes to fpGUI key codes
  // This is a simplified mapping - a complete implementation would need more key codes
  case keyCode of
    // Function keys
    122: Result := keyF1;
    120: Result := keyF2;
    99:  Result := keyF3;
    118: Result := keyF4;
    96:  Result := keyF5;
    97:  Result := keyF6;
    98:  Result := keyF7;
    100: Result := keyF8;
    101: Result := keyF9;
    109: Result := keyF10;
    103: Result := keyF11;
    111: Result := keyF12;

    // Navigation keys
    126: Result := keyUp;
    125: Result := keyDown;
    123: Result := keyLeft;
    124: Result := keyRight;
    116: Result := keyPageUp;
    121: Result := keyPageDown;
    115: Result := keyHome;
    119: Result := keyEnd;

    // Editing keys
    51:  Result := keyBackSpace;
    117: Result := keyDelete;
    36:  Result := keyReturn;
    76:  Result := keyReturn;  // Numpad Enter
    48:  Result := keyTab;
    53:  Result := keyEscape;

    // Special keys
    114: Result := keyInsert;
    71:  Result := keyNUMLOCK;
    107: Result := keySCROLLLOCK;

    // Default: pass through the keycode
    else
      Result := keyCode;
  end;
end;

{ TfpgCocoaClipboard }

function TfpgCocoaClipboard.DoGetText: TfpgString;
var
  pasteboard: NSPasteboard;
  nsStr: NSString;
begin
  Result := '';
  pasteboard := NSPasteboard.generalPasteboard;

  // Check if pasteboard contains string data
  if pasteboard.availableTypeFromArray(NSArray.arrayWithObject(NSStringPboardType)) <> nil then
  begin
    nsStr := NSString(pasteboard.stringForType(NSStringPboardType));
    if Assigned(nsStr) then
      Result := NSStringToString(nsStr);
  end;
end;

procedure TfpgCocoaClipboard.DoSetText(const AValue: TfpgString);
var
  pasteboard: NSPasteboard;
  nsStr: NSString;
  types: NSArray;
begin
  pasteboard := NSPasteboard.generalPasteboard;

  // Clear the pasteboard
  pasteboard.clearContents;

  // Set the string
  nsStr := NSString(NSSTR(PChar(AValue)));
  types := NSArray.arrayWithObject(NSStringPboardType);
  pasteboard.declareTypes_owner(types, nil);
  pasteboard.setString_forType(nsStr, NSStringPboardType);
end;

procedure TfpgCocoaClipboard.InitClipboard;
begin
  // Nothing special needed for Cocoa clipboard initialization
  // NSPasteboard is accessed on demand
end;

{ TfpgCocoaDrag }

function TfpgCocoaDrag.Execute(const ADropActions: TfpgDropActions; const ADefaultAction: TfpgDropAction=daCopy): TfpgDropAction;
begin
  // TODO: Implement drag and drop using NSDraggingSession
  Result := daIgnore;
end;

{ TfpgCocoaDrop }

function TfpgCocoaDrop.GetDropAction: TfpgDropAction;
begin
  // TODO: Implement drop action
  Result := daIgnore;
end;

procedure TfpgCocoaDrop.SetDropAction(AValue: TfpgDropAction);
begin
  // TODO: Implement drop action setting
end;

function TfpgCocoaDrop.GetWindowForDrop: TfpgWindowBase;
begin
  // TODO: Implement window for drop
  Result := nil;
end;

{ TfpgCocoaCanvas }
{ NOTE: When using AggCanvas, TAgg2D is used instead }
{ These are stub implementations that won't be called with AggCanvas }

procedure TfpgCocoaCanvas.DoSetFontRes(fntres: TfpgFontResourceBase);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoSetTextColor(cl: TfpgColor);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoSetColor(cl: TfpgColor);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoFillRectangle(x, y, w, h: TfpgCoord);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoDrawRectangle(x, y, w, h: TfpgCoord);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoDrawLine(x1, y1, x2, y2: TfpgCoord);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoDrawString(x, y: TfpgCoord; const txt: string);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoSetClipRect(const ARect: TfpgRect);
begin
  // Stub - not used with AggCanvas
end;

function TfpgCocoaCanvas.DoGetClipRect: TfpgRect;
begin
  // Stub - not used with AggCanvas
  Result := fpgRect(0, 0, 0, 0);
end;

procedure TfpgCocoaCanvas.DoAddClipRect(const ARect: TfpgRect);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoClearClipRect;
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoBeginDraw(awidget: TfpgWidgetBase; CanvasTarget: TfpgCanvasBase);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoEndDraw;
begin
  // Stub - not used with AggCanvas
end;

function TfpgCocoaCanvas.GetPixel(X, Y: integer): TfpgColor;
begin
  // Stub - not used with AggCanvas
  Result := 0;
end;

procedure TfpgCocoaCanvas.SetPixel(X, Y: integer; const AValue: TfpgColor);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
begin
  // Stub - not used with AggCanvas
end;

procedure TfpgCocoaCanvas.DoDrawPolygon(const Points: array of TPoint);
begin
  // Stub - not used with AggCanvas
end;

function TfpgCocoaCanvas.GetBufferAllocated: Boolean;
begin
  // Stub - not used with AggCanvas
  Result := False;
end;

procedure TfpgCocoaCanvas.DoAllocateBuffer;
begin
  // Stub - not used with AggCanvas
end;

{ TfpgCocoaSystemTrayHandler }

procedure TfpgCocoaSystemTrayHandler.Show;
begin
  // TODO: Implement using NSStatusBar
  // NSStatusBar.systemStatusBar.statusItemWithLength(NSVariableStatusItemLength)
end;

procedure TfpgCocoaSystemTrayHandler.Hide;
begin
  // TODO: Implement status item removal
end;

function TfpgCocoaSystemTrayHandler.IsSystemTrayAvailable: boolean;
begin
  // macOS always has a status bar (menu bar)
  Result := True;
end;

function TfpgCocoaSystemTrayHandler.SupportsMessages: boolean;
begin
  // macOS status bar items support notifications
  Result := True;
end;

end.
