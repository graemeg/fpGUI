{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Xlib netlayer support
}

unit fpg_netlayer_x11;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, X, XLib, XUtil, XAtom, ctypes;
  
type
  TNetMaximizedState = (nmsNone, nmsHorz, nmsVert, nmsBoth);
  TNetWindowType = (nwtDesktop, nwtDock, nwtToolBar, nwtMenu, nwtUtility, nwtSplash,
                    nwtDialog, nwtDropdownMenu, nwtPopupMenu, nwtToolTip,
                    nwtNotification, nwtCombo, nwtDND, nwtNormal);
  TNetWindowTypes = set of TNetWindowType;
  
  TNetWindowState = (nwsModal, nwsSticky, nwsMaxVert, nwsMaxHorz, nwsShaded, nwsSkipTaskBar,
                     nwsSkipPager, nwsHidden, nwsFullScreen, nwsAbove, nwsBelow, nwsDemandsAttn);
  TNetWindowStates = set of TNetWindowState;
  
  TNetWindowAction = (nwaMove, nwaResize, nwaMinimize, nwaShade, nwaStick, nwaMaxHorz,
                      nwaMaxVert, nwaFullscreen, nwaChangeDesktop, nwaClose);
                      
  TNetWindowActions = set of TNetWindowAction;
  
  PNetAtom = ^TNetAtom;
  TNetAtom = TAtom;
  
  TNetAtomEnum = (
               naSUPPORTED,
               naCLIENT_LIST, // array of TWindow
               naCLIENT_LIST_STACKING, // array of TWindow (bottom to top)
               naNUMBER_OF_DESKTOPS, // cardinal
               naDESKTOP_GEOMETRY, // array [0..1] of cardinal (width height)
               naDESKTOP_VIEWPORT, // array of cardinal array [0..1] viewports (top left position)
               naCURRENT_DESKTOP, // cardinal (index of desktop)
               naDESKTOP_NAMES, // array of null terminated UTF8 Strings
               naACTIVE_WINDOW, // TWindow
               naWORKAREA, // array [0..3] of cardinal (x, y, width, height)
               naSUPPORTING_WM_CHECK, // TWindow
               naVIRTUAL_ROOTS, // array of TWindow
               naDESKTOP_LAYOUT, // array [0..3] of cardinal (orientation, columns, rows, starting_corner)
               naSHOWING_DESKTOP, // ca
               //Root Window Messages
               naCLOSE_WINDOW,
               naMOVERESIZE_WINDOW,
               naWM_MOVERESIZE,
               naRESTACK_WINDOW,
               naREQUEST_FRAME_EXTENTS,
               //Application Window Properties
               naWM_NAME, // UTF8 String
               naWM_VISIBLE_NAME, //UTF8 String
               naWM_ICON_NAME,  // UTF8 String
               naWM_VISIBLE_ICON_NAME, // UTF8 String
               naWM_DESKTOP, // cardinal (index of the desktop of the window) $FFFFFFFF for all desktops
               naWM_WINDOW_TYPE, // TAtom of the different types below
               naWM_WINDOW_TYPE_DESKTOP,
               naWM_WINDOW_TYPE_DOCK,
               naWM_WINDOW_TYPE_TOOLBAR,
               naWM_WINDOW_TYPE_MENU,
               naWM_WINDOW_TYPE_UTILITY,
               naWM_WINDOW_TYPE_SPLASH,
               naWM_WINDOW_TYPE_DIALOG,
               naWM_WINDOW_TYPE_NORMAL,
               naWM_WINDOW_TYPE_DROPDOWN_MENU,
               naWM_WINDOW_TYPE_POPUP_MENU,
               naWM_WINDOW_TYPE_TOOLTIP,
               naWM_WINDOW_TYPE_NOTIFICATION,
               naWM_WINDOW_TYPE_COMBO,
               naWM_WINDOW_TYPE_DND,
               naWM_STATE, // array of TAtoms. Possible members are listed below. others should be ignored
               naWM_STATE_MODAL,
               naWM_STATE_STICKY,
               naWM_STATE_MAXIMIZED_VERT,
               naWM_STATE_MAXIMIZED_HORZ,
               naWM_STATE_SHADED,
               naWM_STATE_SKIP_TASKBAR,
               naWM_STATE_SKIP_PAGER,
               naWM_STATE_HIDDEN,
               naWM_STATE_FULLSCREEN,
               naWM_STATE_ABOVE,
               naWM_STATE_BELOW,
               naWM_STATE_DEMANDS_ATTENTION,
               naWM_ALLOWED_ACTIONS, //array of TAtoms below. unknown atoms are ignored
               naWM_ACTION_MOVE,
               naWM_ACTION_RESIZE,
               naWM_ACTION_MINIMIZE,
               naWM_ACTION_SHADE,
               naWM_ACTION_STICK,
               naWM_ACTION_MAXIMIZE_HORZ,
               naWM_ACTION_MAXIMIZE_VERT,
               naWM_ACTION_FULLSCREEN,
               naWM_ACTION_CHANGE_DESKTOP,
               naWM_ACTION_CLOSE,
               naWM_STRUT, // array [0..3] of cardinal (left right top bottom)
               naWM_STRUT_PARTIAL, // array [0..11] of cardinal (left, right, top, bottom,
                                   // left_start_y, left_end_y, right_start_y, right_end_y,
                                   // top_start_x, top_end_x, bottom_start_x, bottom_end_x )
               naWM_ICON_GEOMETRY, // array [0..3] of cardinal (x, y, width, height)
               naWM_ICON, // array of cardinal the first two in the array are the width height
                          // and the rest of the array is the icon data in BGRA order
               naWM_PID, // cardinal (process id of the window)
               naWM_HANDLED_ICONS,
               naWM_USER_TIME, // cardinal (XServer time of last user activity)

               naFRAME_EXTENTS, // array [0..3] of cardinal (left, right, top ,bottom)

               //Window Manager Protocols
               naWM_PING,
               naWM_SYNC_REQUEST
             );

  TNetAtomsSet = set of TNetAtomEnum;

  { TNETWindowLayer }

  TNETWindowLayer = class
  private
    FDisplay: PXDisplay;
    FRootWindow: TWindow;
    FNetAtoms: array[TNetAtomEnum] of TNetAtom;
    UTF8_STRING: TAtom;
    FAtomSupported: array[TNetAtomEnum] of Boolean;
    FTimeStamp: LongInt;
    function GetNetAtom(AAtom: TNetAtomEnum): TNetAtom;
    procedure InitNetAtoms;
    procedure UpdateSupportedAtoms;
  public
    // window related functions
    function    WindowSetName(const AWindow: TWindow; AName: PChar): Boolean;
    function    WindowGetHidden(const AWindow: TWindow; out AValue: Boolean): Boolean;
    function    WindowSetHidden(const AWindow: TWindow; const AValue: Boolean): Boolean;
    function    WindowGetMaximizedState(const AWindow: TWindow; out AValue: TNetMaximizedState): Boolean;
    function    WindowSetMaximizedState(const AWindow: TWindow; const AValue: TNetMaximizedState): Boolean;
    function    WindowSetFullscreen(const AWindow: TWindow; const AValue: Boolean): Boolean;
    function    WindowGetFullscreen(const AWindow: TWindow; out AValue: Boolean): Boolean;
    function    WindowSetDesktop(const AWindow: TWindow; const ADesktopIndex: Integer): Boolean;
    function    WindowGetDesktop(const AWindow: TWindow; out ADesktopIndex: Integer): Boolean;
    function    WindowMoveResize(const AWindow: TWindow; const AX, AY, AWidth, AHeight: Integer): Boolean;
    function    WindowMove(const AWindow: TWindow; const AX, AY: Integer): Boolean;
    function    WindowSetSticky(const AWindow: TWindow; const AValue: Boolean): Boolean;
    function    WindowGetSticky(const AWindow: TWindow; out AValue: Boolean): Boolean;
    procedure   WindowSetPID(const AWindow: TWindow; const APID: Cardinal);
    function    WindowGetFrameExtents(const AWindow: TWindow; out ATopHeight, ALeftWidth, ARightWidth, ABottomHeight: Integer): Boolean;
    procedure   WindowSetSupportPING(const AWindow: TWindow);
    procedure   WindowReplyToPING(const AWindow: TWindow; AClientMessage: PXClientMessageEvent);
    function    WindowGetState(const AWindow: TWindow; out AWindowState: TNetWindowStates): Boolean;
    function    WindowSetModal(const AWindow: TWindow; const AValue: Boolean): Boolean;
    procedure   WindowDemandsAttention(const AWindow: TWindow);
    procedure   WindowSetSkipTaskbar(const AWindow: TWindow; const AValue: Boolean);
    procedure   WindowSetSkipPager(const AWindow: TWindow; const AValue: Boolean);
    function    WindowGetType(const AWindow: TWindow; out AWindowType: TNetWindowTypes): Boolean;
    procedure   WindowSetType(const AWindow: TWindow; const AWindowType: TNetWindowTypes);
    procedure   WindowAddProtocol(const AWindow: TWindow; AProtocol: TAtom);
    function    WindowGetAllowedActions(const AWindow: TWindow; var AActions: TNetWindowActions): Boolean;
    // windowmanager functions
    function    ManagerCloseWindow(const AWindow: TWindow): Boolean;
    function    ManagerGetActiveWindow(out AWindow: TWindow): Boolean;
    function    ManagerSetActiveWindow(const AWindow: TWindow): Boolean;
    function    ManagerIsValid: Boolean;
    // desktop functions
    function    DesktopGetSize(out AWidth, AHeight: Integer): Boolean;
    function    DesktopGetCurrent(out AIndex: Integer): Boolean;
    function    DesktopSetCurrent(const AIndex: Integer): Boolean;
    function    DesktopGetCount(out Desktops: Integer): Boolean;
    function    DesktopIsShowing: Boolean;
    // misc
    function    SendMessage(AWindow: TWindow; APropagate: Boolean; AMask: LongInt; AMessage: PXEvent): TStatus;
    procedure   SendRootWindowMessage(AMessage: PXEvent);
    procedure   SendRootWindowClientMessage(AMessage: PXClientMessageEvent);
    // property setting and getting routines
    // the "WindowSetPropertyXX procedures replace the entire property so if
    // the property is an array of items then you should copy the old property
    // value and add the new item to the list and pass that list to the Set procedure
    function    WindowGetPropertyAtom(const AWindow: TWindow; AProperty: TAtom; var Count: Integer; var Atoms: PAtom): Boolean;
    procedure   WindowSetPropertyAtom(const AWindow: TWindow; AProperty: TAtom; Count: Integer; Atoms: PAtom);
    procedure   WindowAppendPropertyAtom(const AWindow: TWindow; AProperty: TAtom; Count: Integer; Atoms: PAtom);
    function    WindowGetPropertyCardinal(const AWindow: TWindow; AProperty: TAtom; var Count: Integer; var Cards: PLongWord): Boolean;
    procedure   WindowSetPropertyCardinal(const AWindow: TWindow; AProperty: TAtom; Count: Integer; Cards: PLongInt);
    function    WindowGetPropertyWindow(const AWindow: TWindow; AProperty: TAtom; var Count: Integer; var Windows: PWindow): Boolean;
    procedure   WindowSetPropertyWindow(const AWindow: TWindow; AProperty: TAtom; Count: Integer; Windows: PWindow);
    function    WindowGetPropertyUTF8(const AWindow: TWindow; AProperty: TAtom; var ALength: Integer; var UTF8Text: String): Boolean;
    procedure   WindowSetPropertyUTF8(const AWindow: TWindow; AProperty: TAtom; ALength: Integer; UTF8Text: String);

    constructor Create(ADisplay: PXDisplay);
    destructor  Destroy; override;
    
    property    NetAtom[AAtom: TNetAtomEnum]: TNetAtom read GetNetAtom;
  end;
  
  const
    NetAtomStr: array[TNetAtomEnum] of String = (
    '_NET_SUPPORTED',
    '_NET_CLIENT_LIST',
    '_NET_CLIENT_LIST_STACKING',
    '_NET_NUMBER_OF_DESKTOPS',
    '_NET_DESKTOP_GEOMETRY',
    '_NET_DESKTOP_VIEWPORT',
    '_NET_CURRENT_DESKTOP',
    '_NET_DESKTOP_NAMES',
    '_NET_ACTIVE_WINDOW',
    '_NET_WORKAREA',
    '_NET_SUPPORTING_WM_CHECK',
    '_NET_VIRTUAL_ROOTS',
    '_NET_DESKTOP_LAYOUT',
    '_NET_SHOWING_DESKTOP',
    '_NET_CLOSE_WINDOW',
    '_NET_MOVERESIZE_WINDOW',
    '_NET_WM_MOVERESIZE',
    '_NET_RESTACK_WINDOW',
    '_NET_REQUEST_FRAME_EXTENTS',
    '_NET_WM_NAME',
    '_NET_WM_VISIBLE_NAME',
    '_NET_WM_ICON_NAME',
    '_NET_WM_VISIBLE_ICON_NAME',
    '_NET_WM_DESKTOP',
    '_NET_WM_WINDOW_TYPE',
    '_NET_WM_WINDOW_TYPE_DESKTOP',
    '_NET_WM_WINDOW_TYPE_DOCK',
    '_NET_WM_WINDOW_TYPE_TOOLBAR',
    '_NET_WM_WINDOW_TYPE_MENU',
    '_NET_WM_WINDOW_TYPE_UTILITY',
    '_NET_WM_WINDOW_TYPE_SPLASH',
    '_NET_WM_WINDOW_TYPE_DIALOG',
    '_NET_WM_WINDOW_TYPE_NORMAL',
    '_NET_WM_WINDOW_TYPE_DROPDOWN_MENU',
    '_NET_WM_WINDOW_TYPE_POPUP_MENU',
    '_NET_WM_WINDOW_TYPE_TOOLTIP',
    '_NET_WM_WINDOW_TYPE_NOTIFICATION',
    '_NET_WM_WINDOW_TYPE_COMBO',
    '_NET_WM_WINDOW_TYPE_DND',

    '_NET_WM_STATE',
    '_NET_WM_STATE_MODAL',
    '_NET_WM_STATE_STICKY',
    '_NET_WM_STATE_MAXIMIZED_VERT',
    '_NET_WM_STATE_MAXIMIZED_HORZ',
    '_NET_WM_STATE_SHADED',
    '_NET_WM_STATE_SKIP_TASKBAR',
    '_NET_WM_STATE_SKIP_PAGER',
    '_NET_WM_STATE_HIDDEN',
    '_NET_WM_STATE_FULLSCREEN',
    '_NET_WM_STATE_ABOVE',
    '_NET_WM_STATE_BELOW',
    '_NET_WM_STATE_DEMANDS_ATTENTION',
    
    '_NET_WM_ALLOWED_ACTIONS',
    '_NET_WM_ACTION_MOVE',
    '_NET_WM_ACTION_RESIZE',
    '_NET_WM_ACTION_MINIMIZE',
    '_NET_WM_ACTION_SHADE',
    '_NET_WM_ACTION_STICK',
    '_NET_WM_ACTION_MAXIMIZE_HORZ',
    '_NET_WM_ACTION_MAXIMIZE_VERT',
    '_NET_WM_ACTION_FULLSCREEN',
    '_NET_WM_ACTION_CHANGE_DESKTOP',
    '_NET_WM_ACTION_CLOSE',
    
    '_NET_WM_STRUT',
    '_NET_WM_STRUT_PARTIAL',
    '_NET_WM_ICON_GEOMETRY',
    '_NET_WM_ICON',
    '_NET_WM_PID',
    '_NET_WM_HANDLED_ICONS',
    '_NET_WM_USER_TIME',
    '_NET_FRAME_EXTENTS',
    '_NET_WM_PING',
    '_NET_WM_SYNC_REQUEST');

  _NET_SOURCE_APPLICATION = 1;
  _NET_SOURCE_PAGER       = 2;
  
  _NET_WM_STATE_REMOVE       = 0;    // remove/unset property
  _NET_WM_STATE_ADD          = 1;    // add/set property
  _NET_WM_STATE_TOGGLE       = 2;    // toggle property


implementation
{ TNETWindowLayer }

procedure TNETWindowLayer.InitNetAtoms;
var
  ANetAtom: TNetAtomEnum;
begin
  for ANetAtom := Low(TNetAtomEnum) to High(TNetAtomEnum) do begin
    FNetAtoms[ANetAtom] := XInternAtom(FDisplay, PChar(NetAtomStr[ANetAtom]), True)
  end;
  UTF8_STRING := XInternAtom(FDisplay, 'UTF8_STRING', True);
end;

function TNETWindowLayer.GetNetAtom(AAtom: TNetAtomEnum): TNetAtom;
begin
  Result := FNetAtoms[AAtom];
end;

procedure TNETWindowLayer.UpdateSupportedAtoms;
var
  AtomCount: Integer;
  Atoms: PNetAtom;
  I: Integer;
  ANetAtom: TNetAtomEnum;
begin
  if WindowGetPropertyAtom(FRootWindow, FNetAtoms[naSUPPORTED], AtomCount, Atoms) = False then
    Exit;

//  WriteLn('RootWindow Atom Count = ',AtomCount);
  FillChar(FAtomSupported, SizeOf(Boolean) * Length(FAtomSupported), 0);;
  for I := 0 to AtomCount-1 do
  begin
    for ANetAtom := Low(TNetAtomEnum) to High(TNetAtomEnum) do
    begin
      if Atoms[I] = FNetAtoms[ANetAtom] then
      begin
        FAtomSupported[ANetAtom] := True;
//        WriteLn('Found ', NetAtomStr[ANetAtom]);
      end;
    end;
  end;
  if AtomCount > 0 then
    XFree(Atoms);
end;

function TNETWindowLayer.WindowSetName(const AWindow: TWindow; AName: PChar): Boolean;
begin
  Result := True; //????
  WindowSetPropertyUTF8(AWindow, FNetAtoms[naWM_NAME], Length(AName), AName);
end;

function TNETWindowLayer.WindowGetHidden(const AWindow: TWindow; out AValue: Boolean): Boolean;
var
  WinState: TNetWindowStates;
begin
  Result := FAtomSupported[naWM_STATE] and FAtomSupported[naWM_STATE_HIDDEN]
  and WindowGetState(AWindow, WinState);
  if not Result then Exit;
  AValue := nwsHidden in WinState;
end;

function TNETWindowLayer.WindowSetHidden(const AWindow: TWindow; const AValue: Boolean): Boolean;
var
  Msg: TXClientMessageEvent;
begin
  Result := FAtomSupported[naWM_STATE] and FAtomSupported[naWM_STATE_HIDDEN];
  if Result = False then Exit;
  Result := True;
  FillChar(Msg, SizeOf(Msg), 0);

  Msg.message_type := FNetAtoms[naWM_STATE];
  Msg.window := AWindow;
  Msg.data.l[0] := Ord(AValue);
  Msg.data.l[1] := FNetAtoms[naWM_STATE_HIDDEN];
  Msg.data.l[3] := _NET_SOURCE_APPLICATION;
  
  SendRootWindowClientMessage(@Msg);
end;

function TNETWindowLayer.WindowGetMaximizedState(const AWindow: TWindow; out
  AValue: TNetMaximizedState): Boolean;
var
  WinState: TNetWindowStates;
begin
  Result := WindowGetState(AWindow, WinState);
  AValue := nmsNone;
  if Result then begin
    if nwsMaxHorz in WinState then AValue := nmsHorz;
    if (nwsMaxVert in WinState) then begin
      if (AValue = nmsHorz) then AValue := nmsBoth
      else AValue := nmsVert;
    end;
  end;
end;

function TNETWindowLayer.WindowSetMaximizedState(const AWindow: TWindow;
  const AValue: TNetMaximizedState): Boolean;
var
  Msg: TXClientMessageEvent;
begin
  Result := FAtomSupported[naWM_STATE]
        and FAtomSupported[naWM_STATE_MAXIMIZED_HORZ]
        and FAtomSupported[naWM_STATE_MAXIMIZED_VERT];
  if Result = False then Exit;
  FillChar(Msg, SizeOf(Msg), 0);

  Msg.message_type := FNetAtoms[naWM_STATE];
  Msg.window := AWindow;
  
  if AValue <> nmsNone then begin
    Msg.data.l[0] := _NET_WM_STATE_ADD;
    Msg.data.l[3] := _NET_SOURCE_APPLICATION;
  
    if AValue = nmsHorz then Msg.data.l[1] := FNetAtoms[naWM_STATE_MAXIMIZED_HORZ];
    if AValue = nmsVert then Msg.data.l[1] := FNetAtoms[naWM_STATE_MAXIMIZED_VERT];
    if AValue = nmsBoth then begin
      Msg.data.l[1] := FNetAtoms[naWM_STATE_MAXIMIZED_VERT];
      Msg.data.l[2] := FNetAtoms[naWM_STATE_MAXIMIZED_HORZ];
    end;
    SendRootWindowClientMessage(@Msg);
  end;

  if AValue = nmsBoth then Exit;
  // now remove properties we dont want

  Msg.data.l[0] := _NET_WM_STATE_REMOVE;
  Msg.data.l[1] := 0;
  Msg.data.l[2] := 0;
  Msg.data.l[3] := _NET_SOURCE_APPLICATION;

  if AValue = nmsHorz then Msg.data.l[1] := FNetAtoms[naWM_STATE_MAXIMIZED_VERT];
  if AValue = nmsVert then Msg.data.l[1] := FNetAtoms[naWM_STATE_MAXIMIZED_HORZ];
  if AValue = nmsNone then begin
    Msg.data.l[1] := FNetAtoms[naWM_STATE_MAXIMIZED_HORZ];
    Msg.data.l[2] := FNetAtoms[naWM_STATE_MAXIMIZED_VERT];
  end;
  SendRootWindowClientMessage(@Msg);
end;

function TNETWindowLayer.WindowSetFullscreen(const AWindow: TWindow;
  const AValue: Boolean): Boolean;
var
  Msg: TXClientMessageEvent;
begin
  Result := FAtomSupported[naWM_STATE] and FAtomSupported[naWM_STATE_FULLSCREEN];
  if Result = False then Exit;
  FillChar(Msg, SizeOf(Msg), 0);

  Msg.message_type := FNetAtoms[naWM_STATE];
  Msg.window := AWindow;
  if AValue then
    Msg.data.l[0] := _NET_WM_STATE_ADD
  else
    Msg.data.l[0] := _NET_WM_STATE_REMOVE;
  Msg.data.l[1] := FNetAtoms[naWM_STATE_FULLSCREEN];
  Msg.data.l[3] := _NET_SOURCE_APPLICATION;

  SendRootWindowClientMessage(@Msg);
end;

function TNETWindowLayer.WindowGetFullscreen(const AWindow: TWindow; out
  AValue: Boolean): Boolean;
var
  WinState: TNetWindowStates;
begin
  Result := WindowGetState(AWindow, WinState);

  if Result then AValue := nwsFullScreen in WinState;
end;

function TNETWindowLayer.WindowSetDesktop(const AWindow: TWindow;
  const ADesktopIndex: Integer): Boolean;
var
  Msg: TXClientMessageEvent;
begin
  Result := FAtomSupported[naWM_DESKTOP];
  if Result = False then Exit;

  Msg.message_type := FNetAtoms[naWM_DESKTOP];
  Msg.window := AWindow;
  Msg.data.l[0] := ADesktopIndex;
  Msg.data.l[1] := _NET_SOURCE_APPLICATION;
  
  SendRootWindowClientMessage(@Msg);
end;

function TNETWindowLayer.WindowGetDesktop(const AWindow: TWindow; out
  ADesktopIndex: Integer): Boolean;
var
  Index: PCardinal;
  Count: LongInt;
begin
  Result := FAtomSupported[naWM_DESKTOP]
  and WindowGetPropertyCardinal(FRootWindow, FNetAtoms[naWM_DESKTOP], Count, Index);
  if Result = False then Exit;
  
  ADesktopIndex := Index^;
  if Count > 0 then XFree(Index);
end;

function TNETWindowLayer.WindowMoveResize(const AWindow: TWindow; const AX, AY,
  AWidth, AHeight: Integer): Boolean;
var
  Msg: TXClientMessageEvent;
const
  WSet      = 1 shl 7;
  HSet      = 1 shl 6;
  XSet      = 1 shl 5;
  YSet      = 1 shl 4;

  FromPager = 1 shl 1;
  FromApp   = 1 shl 0;
begin
  Result := FAtomSupported[naMOVERESIZE_WINDOW];
  if Result = False then Exit;
  FillChar(Msg, SizeOf(Msg), 0);

  Msg.message_type := FNetAtoms[naMOVERESIZE_WINDOW];
  Msg.window := AWindow;
  Msg.data.s[0] := WSet or HSet or XSet or YSet or FromApp; // Gravity and flags
  Msg.data.l[1] := AX;
  Msg.data.l[2] := AY;
  Msg.data.l[3] := AWidth;
  Msg.data.l[4] := AHeight;

  SendRootWindowClientMessage(@Msg);
end;

function TNETWindowLayer.WindowMove(const AWindow: TWindow; const AX, AY: Integer): Boolean;
var
  Msg: TXClientMessageEvent;
const
  WSet      = 1 shl 7;
  HSet      = 1 shl 6;
  XSet      = 1 shl 5;
  YSet      = 1 shl 4;
  FromApp   = 1 shl 0;
  FromPager = 1 shl 1;
begin
  Result := FAtomSupported[naMOVERESIZE_WINDOW];
  if Result = False then Exit;
  FillChar(Msg, SizeOf(Msg), 0);

  Msg.message_type := FNetAtoms[naMOVERESIZE_WINDOW];
  Msg.window := AWindow;
  Msg.data.b[0] := 0; // Gravity and flags
  Msg.data.b[1] := XSet or YSet or FromApp;
  Msg.data.b[2] := 0;
  Msg.data.b[3] := 0; // this does nothing
  Msg.data.l[1] := AX;
  Msg.data.l[2] := AY;

  SendRootWindowClientMessage(@Msg);

end;

function TNETWindowLayer.WindowSetSticky(const AWindow: TWindow;
  const AValue: Boolean): Boolean;
var
  Msg: TXClientMessageEvent;
begin
  Result := FAtomSupported[naWM_STATE] and FAtomSupported[naWM_STATE_STICKY];
  if Result = False then Exit;
  FillChar(Msg, SizeOf(Msg), 0);

  Msg.window := AWindow;
  Msg.message_type := FNetAtoms[naWM_STATE];
  Msg.data.l[0] := Ord(AValue);
  Msg.data.l[1] := FNetAtoms[naWM_STATE_STICKY];
  Msg.data.l[3] := _NET_SOURCE_APPLICATION;

  SendRootWindowClientMessage(@Msg);
end;

function TNETWindowLayer.WindowGetSticky(const AWindow: TWindow; out AValue: Boolean
  ): Boolean;
var
  WinState: TNetWindowStates;
begin
  Result := WindowGetState(AWindow, WinState);

  if Result then AValue := nwsSticky in WinState;
end;

procedure TNETWindowLayer.WindowSetPID(const AWindow: TWindow; const APID: Cardinal);
begin
  WindowSetPropertyCardinal(AWindow, FNetAtoms[naWM_PID], 1, @APID);
end;

function TNETWindowLayer.WindowGetFrameExtents(const AWindow: TWindow; out
  ATopHeight, ALeftWidth, ARightWidth, ABottomHeight: Integer): Boolean;
var
  Sizes: PCardinal;
  Count: LongInt;
begin
  Result := FAtomSupported[naFRAME_EXTENTS];
  if Result = False then Exit;
  Result := WindowGetPropertyCardinal(FRootWindow, FNetAtoms[naFRAME_EXTENTS], Count, Sizes);
  if Count = 4 then begin
    ALeftWidth := Sizes[0];
    ARightWidth := Sizes[1];
    ATopHeight := Sizes[2];
    ABottomHeight := Sizes[3];
  end
  else Result := False;
  if Count > 0 then XFree(Sizes);
end;

procedure TNETWindowLayer.WindowSetSupportPING(const AWindow: TWindow);
var
  WM_PROTOCOLS: TAtom;
begin
  //WM_PROTOCOLS := XInternAtom(FDisplay, 'WM_PROTOCOLS', True);
  WindowAddProtocol(AWindow, FNetAtoms[naWM_PING]);
  //WindowAppendPropertyAtom(AWindow, WM_PROTOCOLS, 1, @FNetAtoms[naWM_PING]);
end;

procedure TNETWindowLayer.WindowReplyToPING(const AWindow: TWindow;
  AClientMessage: PXClientMessageEvent);
begin
  AClientMessage^.window := FRootWindow;
  SendRootWindowMessage(PXEvent(AClientMessage));
end;

function TNETWindowLayer.ManagerCloseWindow(const AWindow: TWindow): Boolean;
var
  Msg: TXClientMessageEvent;
begin
  Result := FAtomSupported[naCLOSE_WINDOW];
  if Result =False then Exit;
  FillChar(Msg, SizeOf(Msg), 0);
  
  Msg.window := AWindow;
  Msg.message_type := FNetAtoms[naCLOSE_WINDOW];
  Msg.data.l[0] := FTimeStamp;
  Msg.data.l[1] := _NET_SOURCE_APPLICATION;
  
  SendRootWindowClientMessage(@Msg);
end;

function TNETWindowLayer.ManagerGetActiveWindow(out AWindow: TWindow): Boolean;
var
  ActiveWindow: PWindow;
  Count: LongInt;
begin
  Result := FAtomSupported[naACTIVE_WINDOW];
  if Result = False then Exit;
  Result := WindowGetPropertyWindow(FRootWindow, FNetAtoms[naACTIVE_WINDOW], Count, ActiveWindow);
  if Count = 1 then
    AWindow := ActiveWindow^;
  if Count > 0 then XFree(ActiveWindow);
end;

function TNETWindowLayer.ManagerSetActiveWindow(const AWindow: TWindow): Boolean;
var
  Msg: TXClientMessageEvent;
begin
  Result := FAtomSupported[naACTIVE_WINDOW];
  if Result = False then Exit;
  FillChar(Msg, SizeOf(Msg), 0);

  Msg.window := AWindow;
  Msg.message_type := FNetAtoms[naACTIVE_WINDOW];
  Msg.data.l[0] := _NET_SOURCE_APPLICATION;
  Msg.data.l[1] := FTimeStamp;
  Msg.data.l[2] := 0; // our current active window

  SendRootWindowClientMessage(@Msg);
end;

function TNETWindowLayer.ManagerIsValid: Boolean;
begin
  // if the window manager changes we need to refresh the list of atoms supported by it
  Result := False;  // ?????  Todo
end;

procedure TNETWindowLayer.SendRootWindowMessage(AMessage: PXEvent);
begin
  SendMessage(FRootWindow, False, SubstructureNotifyMask or SubstructureRedirectMask, AMessage);
end;

procedure TNETWindowLayer.SendRootWindowClientMessage(AMessage: PXClientMessageEvent
  );
begin
  AMessage^.format := 32;
  AMessage^._type  := ClientMessage;
  SendRootWindowMessage(PXEvent(AMessage));
end;

function TNETWindowLayer.WindowGetPropertyAtom(const AWindow: TWindow; AProperty: TAtom;
  var Count: Integer; var Atoms: PAtom): Boolean;
var
  atomtype: TAtom;
  format: cint;
  nitems: culong;
  bytes_after: culong;
begin
  Result := False;
  XGetWindowProperty (FDisplay, AWindow, AProperty, 0, MaxInt, TBool(False), XA_ATOM, @atomtype, @format, @nitems,
             @bytes_after, @Atoms);

  if (atomtype = XA_ATOM) and (format = 32) then begin
    Result := True;
    Count := nitems;
  end;
end;

procedure TNETWindowLayer.WindowSetPropertyAtom(const AWindow: TWindow;
  AProperty: TAtom; Count: Integer; Atoms: PAtom);
begin
  XChangeProperty(FDisplay, AWindow, AProperty, XA_ATOM, 32, PropModeReplace, Pointer(Atoms), Count);
end;

procedure TNETWindowLayer.WindowAppendPropertyAtom(const AWindow: TWindow;
  AProperty: TAtom; Count: Integer; Atoms: PAtom);
var
  AtomCount: Integer;
  SetAtoms: PAtom;
  I: Integer;
  NewAtoms: array of TAtom;
  NewCount: Integer;
     function AtomInList(AAtom: PAtom): Boolean;
     var
       J: Integer;
     begin
       Result := False;
       for J := 0 to AtomCount-1 do
         if SetAtoms^ = AAtom^ then Exit(True);
     end;
begin
  if WindowGetPropertyAtom(AWindow, AProperty, AtomCount, SetAtoms) = False then
  begin
    SetAtoms := nil;
    AtomCount := 0;
  end;
  NewCount := AtomCount;
  SetLength(NewAtoms, AtomCount + Count);
  for I := 0 to Count-1 do
  begin
    if AtomInList(@Atoms[I]) = False then
    begin
      NewAtoms[NewCount] := Atoms[I];
      Inc(NewCount);
    end;
  end;
  if AtomCount > 0 then XFree(SetAtoms);
  if NewCount > 0 then
    WindowSetPropertyAtom(AWindow, AProperty, NewCount, @NewAtoms[0]);
end;

procedure TNETWindowLayer.WindowSetPropertyCardinal(const AWindow: TWindow;
  AProperty: TAtom; Count: Integer; Cards: PLongInt);
begin
  XChangeProperty(FDisplay, AWindow, AProperty, XA_CARDINAL, 32, PropModeReplace, Pointer(Cards), Count);
end;

function TNETWindowLayer.WindowGetPropertyWindow(const AWindow: TWindow;
  AProperty: TAtom; var Count: Integer; var Windows: PWindow): Boolean;
var
  atomtype: TAtom;
  format: cint;
  nitems: culong;
  bytes_after: culong;
begin
  Result := False;
  XGetWindowProperty (FDisplay, AWindow, AProperty, 0, MaxInt, TBool(False), XA_ATOM, @atomtype, @format, @nitems,
             @bytes_after, @Windows);

  if (atomtype = XA_WINDOW) and (format = 32) then begin
    Result := True;
    Count := nitems;
  end;

end;

procedure TNETWindowLayer.WindowSetPropertyWindow(const AWindow: TWindow;
  AProperty: TAtom; Count: Integer; Windows: PWindow);
begin
  XChangeProperty(FDisplay, AWindow, AProperty, XA_WINDOW, 32, PropModeReplace, Pointer(Windows), Count);
end;

function TNETWindowLayer.WindowGetPropertyUTF8(const AWindow: TWindow;
  AProperty: TAtom; var ALength: Integer; var UTF8Text: String): Boolean;
var
  atomtype: TAtom;
  format: cint;
  nitems: culong;
  bytes_after: culong;
  Utf8Str: PChar;
begin
  Result := False;
  XGetWindowProperty (FDisplay, AWindow, AProperty, 0, MaxInt, TBool(False), XA_ATOM, @atomtype, @format, @nitems,
             @bytes_after, @Utf8Str);

  if (atomtype = XA_WINDOW) and (format = 32) then begin
    Result := True;
    UTF8Text := Copy(Utf8Str, 0, nitems);
    ALength := nitems;
  end;
  if nitems > 0 then XFree(Utf8Str);
end;

procedure TNETWindowLayer.WindowSetPropertyUTF8(const AWindow: TWindow;
  AProperty: TAtom; ALength: Integer; UTF8Text: String);
begin
  XChangeProperty(FDisplay, AWindow, AProperty, UTF8_STRING, 8, PropModeReplace, @UTF8Text[1], ALength);
end;

function TNETWindowLayer.WindowGetPropertyCardinal(const AWindow: TWindow;
  AProperty: TAtom; var Count: Integer; var Cards: PLongWord): Boolean;
var
  atomtype: TAtom;
  format: cint;
  nitems: culong;
  bytes_after: culong;
begin
  Result := False;
  XGetWindowProperty (FDisplay, AWindow, AProperty, 0, MaxInt, TBool(False), XA_ATOM, @atomtype, @format, @nitems,
             @bytes_after, @Cards);

  if (atomtype = XA_CARDINAL) and (format = 32) then begin
    Result := True;
    Count := nitems;
  end;
end;

procedure TNETWindowLayer.WindowAddProtocol(const AWindow: TWindow; AProtocol: TAtom);
var
  Count: cint;
  Protocols: PAtom;
  NewProtocols: array of TAtom;
  I: Integer;
begin
  Count := 0;
  Protocols := nil;

  XGetWMProtocols(FDisplay, AWindow, @Protocols, @Count);
  SetLength(NewProtocols, Count+1);
  if Protocols <> nil then
  begin
    for I := 0 to Count -1 do
      if Protocols[I] = AProtocol then
      begin
        XFree(Protocols);
        Exit;
      end;
    Move(Protocols[0], NewProtocols[0], SizeOf(TAtom)* Count);
  end;

  NewProtocols[Count] := AProtocol;
  XSetWMProtocols(FDisplay, AWindow, @NewProtocols[0], Count+1);

  if Count > 0 then
    XFree(Protocols);
end;

function TNETWindowLayer.WindowGetAllowedActions(const AWindow: TWindow;
  var AActions: TNetWindowActions): Boolean;
var
  AtomCount: Integer;
  ActionAtoms: PAtom;
  I: Integer;
begin
  Result := FAtomSupported[naWM_ALLOWED_ACTIONS];
  if Result = False then Exit;
  if WindowGetPropertyAtom(AWindow, FNetAtoms[naWM_ALLOWED_ACTIONS], AtomCount, ActionAtoms) = False then Exit(False);
  AActions := [];
  
  //WriteLn('Getting Allowed Actions. ', AtomCount);

  for I := 0 to AtomCount-1 do begin
    //WriteLn('Allowed ', XGetAtomName(FDisplay,ActionAtoms[I]));
    if ActionAtoms[I] = FNetAtoms[naWM_ACTION_MOVE] then Include(AActions, nwaMove)
    else if ActionAtoms[I] = FNetAtoms[naWM_ACTION_RESIZE] then Include(AActions, nwaResize)
    else if ActionAtoms[I] = FNetAtoms[naWM_ACTION_MINIMIZE] then Include(AActions, nwaMinimize)
    else if ActionAtoms[I] = FNetAtoms[naWM_ACTION_SHADE] then Include(AActions, nwaShade)
    else if ActionAtoms[I] = FNetAtoms[naWM_ACTION_STICK] then Include(AActions, nwaStick)
    else if ActionAtoms[I] = FNetAtoms[naWM_ACTION_MAXIMIZE_HORZ] then Include(AActions, nwaMaxHorz)
    else if ActionAtoms[I] = FNetAtoms[naWM_ACTION_MAXIMIZE_VERT] then Include(AActions, nwaMaxVert)
    else if ActionAtoms[I] = FNetAtoms[naWM_ACTION_FULLSCREEN] then Include(AActions, nwaFullscreen)
    else if ActionAtoms[I] = FNetAtoms[naWM_ACTION_CHANGE_DESKTOP] then Include(AActions, nwaChangeDesktop)
    else if ActionAtoms[I] = FNetAtoms[naWM_ACTION_CLOSE] then Include(AActions, nwaClose);
  end;
  if AtomCount > 0 then XFree(ActionAtoms);
end;

function TNETWindowLayer.WindowGetState(const AWindow: TWindow; out
  AWindowState: TNetWindowStates): Boolean;
var
  AtomCount: Integer;
  StateAtoms: PAtom;
  I: Integer;
begin
  Result := FAtomSupported[naWM_STATE];
  if Result = False then Exit;
  if WindowGetPropertyAtom(AWindow, FNetAtoms[naWM_STATE], AtomCount, StateAtoms) = False then
    Exit(False);
    
  AWindowState := [];

  for I := 0 to AtomCount-1 do begin
    if StateAtoms[I] = FNetAtoms[naWM_STATE_MODAL] then Include(AWindowState, nwsModal)
    else if StateAtoms[I] = FNetAtoms[naWM_STATE_STICKY] then Include(AWindowState, nwsSticky)
    else if StateAtoms[I] = FNetAtoms[naWM_STATE_MAXIMIZED_VERT] then Include(AWindowState, nwsMaxVert)
    else if StateAtoms[I] = FNetAtoms[naWM_STATE_MAXIMIZED_HORZ] then Include(AWindowState, nwsMaxHorz)
    else if StateAtoms[I] = FNetAtoms[naWM_STATE_SHADED] then Include(AWindowState, nwsShaded)
    else if StateAtoms[I] = FNetAtoms[naWM_STATE_SKIP_TASKBAR] then Include(AWindowState, nwsSkipTaskBar)
    else if StateAtoms[I] = FNetAtoms[naWM_STATE_SKIP_PAGER] then Include(AWindowState, nwsSkipPager)
    else if StateAtoms[I] = FNetAtoms[naWM_STATE_HIDDEN] then Include(AWindowState, nwsHidden)
    else if StateAtoms[I] = FNetAtoms[naWM_STATE_FULLSCREEN] then Include(AWindowState, nwsFullScreen)
    else if StateAtoms[I] = FNetAtoms[naWM_STATE_ABOVE] then Include(AWindowState, nwsAbove)
    else if StateAtoms[I] = FNetAtoms[naWM_STATE_BELOW] then Include(AWindowState, nwsBelow)
    else if StateAtoms[I] = FNetAtoms[naWM_STATE_DEMANDS_ATTENTION] then Include(AWindowState, nwsDemandsAttn);
  end;
  if AtomCount > 0 then
    XFree(StateAtoms);
end;

function TNETWindowLayer.WindowSetModal(const AWindow: TWindow;
  const AValue: Boolean): Boolean;
begin
  Result := FAtomSupported[naWM_STATE] and FAtomSupported[naWM_STATE_MODAL];
  if not Result then
    Exit;
  WindowAppendPropertyAtom(AWindow, FNetAtoms[naWM_STATE], 1, @FNetAtoms[naWM_STATE_MODAL]);
end;

procedure TNETWindowLayer.WindowDemandsAttention(const AWindow: TWindow);
var
  Msg: TXClientMessageEvent;
begin
  if FAtomSupported[naWM_STATE] and FAtomSupported[naWM_STATE_DEMANDS_ATTENTION] = False then Exit;

  FillChar(Msg, SizeOf(Msg), 0);

  Msg.message_type := FNetAtoms[naWM_STATE];
  Msg.window := AWindow;
  Msg.data.l[0] := _NET_WM_STATE_ADD;
  Msg.data.l[1] := FNetAtoms[naWM_STATE_DEMANDS_ATTENTION];
  Msg.data.l[3] := _NET_SOURCE_APPLICATION;

  SendRootWindowClientMessage(@Msg);
end;

procedure TNETWindowLayer.WindowSetSkipTaskbar(const AWindow: TWindow;
  const AValue: Boolean);
var
  Msg: TXClientMessageEvent;
begin
  if FAtomSupported[naWM_STATE] and FAtomSupported[naWM_STATE_SKIP_TASKBAR] = False then Exit;
  FillChar(Msg, SizeOf(Msg), 0);

  Msg.message_type := FNetAtoms[naWM_STATE];
  Msg.window := AWindow;
  Msg.data.l[0] := Ord(AValue);
  Msg.data.l[1] := FNetAtoms[naWM_STATE_SKIP_TASKBAR];
  Msg.data.l[3] := _NET_SOURCE_APPLICATION;

  SendRootWindowClientMessage(@Msg);
end;

procedure TNETWindowLayer.WindowSetSkipPager(const AWindow: TWindow;
  const AValue: Boolean);
var
  Msg: TXClientMessageEvent;
begin
  if FAtomSupported[naWM_STATE] and FAtomSupported[naWM_STATE_SKIP_PAGER] = False then Exit;

  FillChar(Msg, SizeOf(Msg), 0);

  Msg.message_type := FNetAtoms[naWM_STATE];
  Msg.window := AWindow;
  Msg.data.l[0] := Ord(AValue);
  Msg.data.l[1] := FNetAtoms[naWM_STATE_SKIP_PAGER];
  Msg.data.l[3] := _NET_SOURCE_APPLICATION;

  SendRootWindowClientMessage(@Msg);
end;

function TNETWindowLayer.WindowGetType(const AWindow: TWindow; out
  AWindowType: TNetWindowTypes): Boolean;
var
  WindowTypes: PAtom;
  Count: LongInt;
  I : Integer;
begin
  Result := FAtomSupported[naWM_WINDOW_TYPE]
  and WindowGetPropertyAtom(AWindow, FNetAtoms[naWM_WINDOW_TYPE], Count, WindowTypes);
  if not Result then Exit;
  
  AWindowType := [];
  for I := 0 to Count -1 do begin
    if WindowTypes[I] = FNetAtoms[naWM_WINDOW_TYPE_DESKTOP] then Include(AWindowType, nwtDesktop)
    else if WindowTypes[I] = FNetAtoms[naWM_WINDOW_TYPE_DOCK] then Include(AWindowType, nwtDock)
    else if WindowTypes[I] = FNetAtoms[naWM_WINDOW_TYPE_TOOLBAR] then Include(AWindowType, nwtToolBar)
    else if WindowTypes[I] = FNetAtoms[naWM_WINDOW_TYPE_MENU] then Include(AWindowType, nwtMenu)
    else if WindowTypes[I] = FNetAtoms[naWM_WINDOW_TYPE_UTILITY] then Include(AWindowType, nwtUtility)
    else if WindowTypes[I] = FNetAtoms[naWM_WINDOW_TYPE_SPLASH] then Include(AWindowType, nwtSplash)
    else if WindowTypes[I] = FNetAtoms[naWM_WINDOW_TYPE_DIALOG] then Include(AWindowType, nwtDialog)
    else if WindowTypes[I] = FNetAtoms[naWM_WINDOW_TYPE_NORMAL] then Include(AWindowType, nwtNormal)
    else if WindowTypes[I] = FNetAtoms[naWM_WINDOW_TYPE_DROPDOWN_MENU] then Include(AWindowType, nwtDropdownMenu)
    else if WindowTypes[I] = FNetAtoms[naWM_WINDOW_TYPE_POPUP_MENU] then Include(AWindowType, nwtPopupMenu)
    else if WindowTypes[I] = FNetAtoms[naWM_WINDOW_TYPE_TOOLTIP] then Include(AWindowType, nwtToolTip)
    else if WindowTypes[I] = FNetAtoms[naWM_WINDOW_TYPE_NOTIFICATION] then Include(AWindowType, nwtNotification)
    else if WindowTypes[I] = FNetAtoms[naWM_WINDOW_TYPE_COMBO] then Include(AWindowType, nwtCombo)
    else if WindowTypes[I] = FNetAtoms[naWM_WINDOW_TYPE_DND] then Include(AWindowType, nwtDND);
  end;
  


end;

procedure TNETWindowLayer.WindowSetType(const AWindow: TWindow;
  const AWindowType: TNetWindowTypes);
var
  WindowAtoms: array of TNetAtom;

  procedure AddWindowType(AType: TNetAtom);
  var
    CurrentLength: Integer;
  begin
    CurrentLength := Length(WindowAtoms);
    SetLength(WindowAtoms, CurrentLength+1);
    WindowAtoms[CurrentLength] := AType;
  end;
var
  WindowType: TNetWindowType;
  AtomEnum: TNetAtomEnum;
begin
  for WindowType := Low(TNetWindowType) to High(TNetWindowType) do begin
    AtomEnum := TNetAtomEnum(Ord(naWM_WINDOW_TYPE_DESKTOP) + Ord(WindowType));
    if WindowType in AWindowType then AddWindowType(FNetAtoms[AtomEnum]);
  end;
end;

function TNETWindowLayer.DesktopGetSize(out AWidth, AHeight: Integer): Boolean;
var
  Count : LongInt;
  Sizes: PLongWord;
begin
  Result := FAtomSupported[naDESKTOP_GEOMETRY]
  and WindowGetPropertyCardinal(FRootWindow, FNetAtoms[naDESKTOP_GEOMETRY], Count, Sizes);
  if not Result then Exit;

  if Count > 0 then begin
    Result := False;
    if Count = 2 then begin
      AWidth := Sizes[0];
      AHeight := Sizes[1];
      Result := True;
    end;
    XFree(Sizes);
  end;
end;

function TNETWindowLayer.DesktopGetCurrent(out AIndex: Integer): Boolean;
var
  Count : LongInt;
  Index: PLongWord;
begin
  Result := FAtomSupported[naCURRENT_DESKTOP]
  and WindowGetPropertyCardinal(FRootWindow, FNetAtoms[naCURRENT_DESKTOP], Count, Index);
  if not Result then Exit;
  
  AIndex := Index^;
  
  if Count > 0 then XFree(Index);
end;

function TNETWindowLayer.DesktopSetCurrent(const AIndex: Integer): Boolean;
var
  Msg: TXClientMessageEvent;
begin
  Result := FAtomSupported[naCURRENT_DESKTOP];
  if Result = False then Exit;
  FillChar(Msg, SizeOf(Msg), 0);

  Msg.message_type := FNetAtoms[naCURRENT_DESKTOP];
  Msg.data.l[0] := AIndex;
  Msg.data.l[1] := FTimeStamp;

  SendRootWindowClientMessage(@Msg);
end;

function TNETWindowLayer.DesktopGetCount(out Desktops: Integer): Boolean;
var
  Count : LongInt;
  Number: PLongWord;
begin
  Result := FAtomSupported[naNUMBER_OF_DESKTOPS]
              and WindowGetPropertyCardinal(FRootWindow, FNetAtoms[naNUMBER_OF_DESKTOPS], Count, Number);
  if not Result then Exit;

  Desktops := PLongInt(Number)^;

  if Count > 0 then XFree(Number);
end;

function TNETWindowLayer.DesktopIsShowing: Boolean;
var
  Count : LongInt;
  Showing: PLongWord;
begin
  Result := FAtomSupported[naSHOWING_DESKTOP]
  and WindowGetPropertyCardinal(FRootWindow, FNetAtoms[naSHOWING_DESKTOP], Count, Showing);
  if not Result then Exit;

  Result := Showing^ = 1;

  if Count > 0 then XFree(Showing);
end;

function TNETWindowLayer.SendMessage(AWindow: TWindow; APropagate: Boolean;
  AMask: LongInt; AMessage: PXEvent): TStatus;
begin
  Inc(FTimeStamp);
  AMessage^.xany.display := FDisplay;
  Result := XSendEvent(FDisplay, AWindow, APropagate, AMask, AMessage);
end;

constructor TNETWindowLayer.Create(ADisplay: PXDisplay);
begin
  FDisplay := ADisplay;
  FRootWindow := XDefaultRootWindow(FDisplay);
  InitNetAtoms;
  UpdateSupportedAtoms;
end;

destructor TNETWindowLayer.Destroy;
begin
  inherited Destroy;
end;

end.

