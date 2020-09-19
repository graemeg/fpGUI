{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2020 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit implements Wayland/Shell support for fpGUI.
}   unit fpg_wayland;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base, fpg_wayland_classes,
  libfontconfig,
  dynlibs,
  freetypeh,
  fpg_fontcache,
  wayland_util,
  libxkbcommon,
  xkb_classes,
  //Agg2D,
  agg_font_freetype,
  agg_font_cache_manager;

type

  { TfpgWaylandFontResource }

  TfpgWaylandFontResource = class (TfpgFontResourceBase)
  private
    FSize: Double;
    FDesc: String;
    FFontEngine: agg_font_freetype.font_engine_freetype_int32;
    FFontCacheManager: font_cache_manager;
    FLastMetricStr: String;
    FLastMetric: TfpgSize;
    procedure   UpdateTextMetric(AStr: String);
  public
    constructor Create(const afontdesc: string); virtual;
    destructor  Destroy; override;
    function    HandleIsValid: Boolean;
    function    GetAscent: integer; override;
    function    GetDescent: integer; override;
    function    GetHeight: integer; override;
    function    GetTextWidth(const txt: string): integer; override;
  end;

  { TfpgWaylandImage }

  TfpgWaylandImage = class(TfpgImageBase)
  private
    FRawImageData: PLongWord;
    FRawMaskData: PLongWord;
  protected
    procedure   DoFreeImage; override;
    procedure   DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: Pointer); override;
    procedure   DoInitImageMask(awidth, aheight: integer; aimgdata: Pointer); override;
    property    RawImageData: PLongWord read FRawImageData;
    property    RawMaskData: PLongWord read FRawMaskData;
  end;


  TfpgWaylandCanvas = class(TfpgCanvasBase)

  end;

  { TfpgWaylandWindow }

  TfpgWaylandWindow = class (TfpgWindowBase)
  private
    FWindowTitle: String;
    FFont: TfpgFontBase;
    //FDecoratorHandle: TfpgwWindowDecorator;
    //FDecoratorCanvas: TObject; {Tagg2dWaylandBufferCanvas}
    FDecor: TObject;
    FWinHandle: TfpgwWindow;
    FMousePos: TfpgPoint;
    procedure DecoratorConfigure(Sender: TObject; AEdges: LongWord; AWidth,
      AHeight: LongInt);
    procedure DecoratorPaint(Sender: TObject);
    // these 'Send___Message procedures are meant to convert the wayland events into FPGM_XXX messages
    procedure SendCloseWindowMessage(Sender: TObject);
    procedure SendConfigureMessage(Sender: TObject; AEdges: LongWord; AWidth, AHeight: LongInt);
    procedure SendPaintMessage(Sender: TObject);
  protected
    FModalForWin: TfpgWaylandWindow;
    procedure   DoAllocateWindowHandle(AParent: TfpgWidgetBase); override;
    procedure   DoReleaseWindowHandle; override;
    procedure   DoRemoveWindowLookup; override;
    procedure   DoSetWindowAttributes(const AOldAtributes, ANewAttributes: TWindowAttributes; const AForceAll: Boolean); override;
    procedure   DoSetWindowVisible(const AValue: Boolean); override;
    function    HandleIsValid: boolean; override;
    procedure   DoSetWindowTitle(const ATitle: string); override;
    procedure   DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord); override;
    function    DoWindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint; override;
    procedure   DoUpdateWindowPosition; override;
    procedure   DoSetMouseCursor; override;
    procedure   DoDNDEnabled(const AValue: boolean); override;
    function    GetWindowState: TfpgWindowState; override;
    procedure   SetWindowState(const AValue: TfpgWindowState); override;
    procedure   SetWindowOpacity(AValue: Single); override;
    // to do with the decorator and the difficulties it causes
    function    GetBufferDrawOffset: DWord;
    procedure   AdjustMousePos(var AX, AY: Integer);
    procedure   AdjustPaintPos(var AX, AY: Integer);
    procedure   DecoratorDraw(ABuffer: TfpgwBuffer);
    procedure   HandleDecorationButton(AMsg: DWord; AParams: TfpgMessageParams);
    procedure   HandleDecorationMove;

    property    WinHandle: TfpgwWindow read FWinHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   ActivateWindow; override;
    procedure   CaptureMouse(AForWidget: TfpgWidgetBase); override;
    procedure   ReleaseMouse; override;
    procedure   SetFullscreen(AValue: Boolean); override;
    procedure   BringToFront; override;
  end;


  { TfpgWaylandApplication }

  TfpgWaylandApplication = class (TfpgApplicationBase)
  private
    FDisplay: TfpgwDisplay;
    FFontConfig: PFcConfig;
    FFreeType: PFT_Library;
    FKeyboardRepeatDelay: Integer;
    FKeyboardRepeatRate: Integer;
    FKeyboard: TXKBHelper;
    FKeyTimer: TObject;
    FShiftState: TShiftState;
    FPopupStack: TFPList;
    procedure KeyboardRepeatDelayExpired(Sender: TObject);
    procedure KeyboardRepeatKeyTimer(Sender: TObject);
    procedure SendKeyboardEnterMessage(Sender: TObject; AKeys: Pwl_array);
    procedure SendKeyboardKey(Sender: TObject; ATime, AKey, AState: LongWord);
    procedure SendKeyboardLeaveMessage(Sender: TObject);
    procedure SendMouseAxisMessage(Sender: TObject; ATime: LongWord;  AAxis: LongWord; AValue: LongInt);
    procedure SendMouseButtonMessage(Sender: TObject; ATime: LongWord; AButton: LongWord; AState: LongInt);
    procedure SendMouseEnterMessage(Sender: TObject; AX, AY: Integer);
    procedure SendMouseLeaveMessage(Sender: TObject);
    procedure SendMouseMotionMessage(Sender: TObject; ATime: LongWord; AX, AY: Integer);
    procedure SetKeyboardRepeat(Sender: TObject; ARate, ADelay: LongInt);
    procedure SetupKeymap(Sender: TObject; AFormat: LongWord; AFileDesc: LongInt; ASize: LongInt);
    procedure UpdateKeyState(Sender: TObject; AModsDepressed, AModsLatched, AModsLocked, AGroup: LongWord);
    procedure StartRepeatDelay(AKeyCode: Word);
  protected
    procedure   DoFlush;
    function    DoGetFontFaceList: TStringList; override;
    procedure   DoWaitWindowMessage(atimeoutms: integer); override;
    function    MessagesPending: boolean; override;
    procedure   ClosePopups;
    function    WindowInPopupStack(AWindow: TfpgWaylandWindow): Boolean;
    procedure   RemoveWindowFromPopupStack(Awindow: TfpgWaylandWindow);
  public
    constructor Create(const AParams: string = ''); virtual;
    destructor  Destroy; override;
    function    GetScreenWidth: TfpgCoord; override;
    function    GetScreenHeight: TfpgCoord; override;
    function    GetScreenPixelColor(APos: TPoint): TfpgColor; override;
    function    Screen_dpi_x: integer; override;
    function    Screen_dpi_y: integer; override;
    function    Screen_dpi: integer; override;


    property Display: TfpgwDisplay read FDisplay;


  end;

  { TfpgWaylandClipboard }

  TfpgWaylandClipboard = class(TfpgClipboardBase)
  protected
    function    DoGetText: TfpgString; override;
    procedure   DoSetText(const AValue: TfpgString); override;
    procedure   InitClipboard; override;
  end;

  TfpgWaylandFileList = class(TfpgFileListBase)

  end;

  TfpgWaylandMimeData = class(TfpgMimeDataBase)

  end;

  { TfpgWaylandDrag }

  TfpgWaylandDrag = class(TfpgDragBase)
    function Execute(const ADropActions: TfpgDropActions = [daCopy]; const ADefaultAction: TfpgDropAction = daCopy): TfpgDropAction; virtual;

  end;

  TfpgWaylandDrop = class (TfpgDropBase)

  end;

  TfpgWaylandTimer = class (TfpgBaseTimer)

  end;

  { TfpgWaylandSystemTrayHandler }

  TfpgWaylandSystemTrayHandler = class(TfpgComponent)
    function IsSystemTrayAvailable: Boolean;
    function SupportsMessages: Boolean;
    procedure Show;

  end;

  function FontCacheItemFromFontDesc(const desc: string; var asize: double): TFontCacheItem;

  function WApplication: TfpgWaylandApplication;

implementation
uses
  fpg_cmdlineparams, fpg_main, Agg2D, ctypes, fpg_widget, libharfbuzz,
  wayland_protocol, fpg_stringutils, fpg_popupwindow, fpg_agg2d_canvas_wayland,
  fpg_wayland_decorations,
  agg_basics;

function KeySymToKeycode(KeySym: LongWord): Word; forward;

type

  { TKeyboardTimer }

  TKeyboardTimer = class(TfpgTimer)
  private
    FKeyCode: DWord;
  published
    property KeyCode: DWord read FKeyCode write FKeyCode;
  end;

var
  lDisplay: TfpgWaylandApplication;


  function FontCacheItemFromFontDesc(const desc: string; var asize: double): TFontCacheItem;
  var
    facename: string;
    cp, i: integer;
    c: char;
    token: string;
    prop, propval: string;

    function NextC: char;
    begin
      Inc(cp);
      if cp > length(desc) then
        c := #0
      else
        c := desc[cp];
      Result := c;
    end;

    procedure NextToken;
    begin
      token := '';
      while (c <> #0) and (c in [' ', 'a'..'z', 'A'..'Z', '_', '0'..'9', '.']) do
      begin
        token := token + c;
        NextC;
      end;
    end;

  begin
    Result := TFontCacheItem.Create('');

    cp := 0;
    NextC;
    NextToken;

    facename := token;
    // Add known substites
    if lowercase(facename) = 'times' then
      facename := 'Times New Roman'
    else if lowercase(facename) = 'courier' then
      facename := 'Courier New'
    else if lowercase(facename) = 'monospace' then
      facename := 'Courier New';
    Result.FamilyName := facename;

    if c = '-' then
    begin
      NextC;
      NextToken;
      asize := StrToIntDef(token, 0);
    end;

    while c = ':' do
    begin
      NextC;
      NextToken;

      prop    := UpperCase(token);
      propval := '';

      if c = '=' then
      begin
        NextC;
        NextToken;
        propval := UpperCase(token);
      end;

      if prop = 'BOLD' then
        Result.IsBold := True
      else if prop = 'ITALIC' then
        Result.IsItalic := True
      else if prop = 'ANGLE' then
        Result.Angle := StrToFloatDef(propval, 0.0);
  //    else if prop = 'ANTIALIAS' then
  //      if propval = 'FALSE' then
  //        lf.lfQuality := NONANTIALIASED_QUALITY else
  //      if propval = 'DEFAULT' then
  //        lf.lfQuality := DEFAULT_QUALITY;
    end;
    i := gFontCache.Find(Result);
    if i > -1 then
      Result.FileName:=gFontCache.Items[i].FileName;
  end;

  function WApplication: TfpgWaylandApplication;
  begin
    Result := lDisplay;
  end;

{ TfpgWaylandClipboard }

procedure SendKeyboardKey(Sender: TObject; ATime, AKey, AState: LongWord);
begin
  case AState of
    WL_KEYBOARD_KEY_STATE_PRESSED:
      begin

      end;
    WL_KEYBOARD_KEY_STATE_RELEASED:
      begin

      end;
  end;

end;

function TfpgWaylandClipboard.DoGetText: TfpgString;
begin
  Result := '';
end;

procedure TfpgWaylandClipboard.DoSetText(const AValue: TfpgString);
begin

end;

procedure TfpgWaylandClipboard.InitClipboard;
begin

end;

{ TfpgWaylandImage }

procedure TfpgWaylandImage.DoFreeImage;
begin
  if Assigned(FRawImageData) then
    Freemem(FRawImageData);

   if Assigned(FRawMaskData) then
    Freemem(FRawMaskData);
end;

procedure TfpgWaylandImage.DoInitImage(acolordepth, awidth, aheight: integer;
  aimgdata: Pointer);
begin
  if acolordepth <> 32 then
    raise EfpGUIException.Create('only 32bit images are implemented on wayland');

  if Assigned(FRawImageData) then
    Freemem(FRawImageData);

  FRawImageData := GetMem(awidth* aheight *4);
  Move(aimgdata^, FRawImageData^, awidth*aheight*4);
end;

procedure TfpgWaylandImage.DoInitImageMask(awidth, aheight: integer;
  aimgdata: Pointer);
begin
         exit;//
  if Assigned(FRawMaskData) then
    Freemem(FRawMaskData);

  FRawMaskData := GetMem(awidth* aheight *4);
  Move(aimgdata^, FRawMaskData^, awidth*aheight*4);

end;

{ TfpgWaylandWindow  }

procedure TfpgWaylandWindow.SendPaintMessage(Sender: TObject);
var
  msgp: TfpgMessageParams;
begin
  msgp.rect.SetRect(0,0,Width, Height);
  //fpgPostMessage(Self, TfpgWidget(Owner), FPGM_PAINT, msgp);
  //fpgDeliverMessages;

  // eventually this results in the canvas calling TAgg2D.DoPutBufferToScreen
end;

function KeySymToKeycode(KeySym: LongWord): Word;
const
  Table_20aX: array[$20a0..$20ac] of Word = (keyEcuSign, keyColonSign,
    keyCruzeiroSign, keyFFrancSign, keyLiraSign, keyMillSign, keyNairaSign,
    keyPesetaSign, keyRupeeSign, keyWonSign, keyNewSheqelSign, keyDongSign,
    keyEuroSign);
  Table_feXX: array[$fe50..$fe60] of Word = (keyDeadGrave, keyDeadAcute,
    keyDeadCircumflex, keyDeadTilde, keyDeadMacron,keyDeadBreve,
    keyDeadAbovedot, keyDeadDiaeresis, keyDeadRing, keyDeadDoubleacute,
    keyDeadCaron, keyDeadCedilla, keyDeadOgonek, keyDeadIota,
    keyDeadVoicedSound, keyDeadSemivoicedSound, keyDeadBelowdot);
  Table_ff5X: array[$ff50..$ff58] of Word = (keyHome, keyLeft, keyUp, keyRight,
    keyDown, keyPrior, keyNext, keyEnd, keyBegin);
  Table_ff6X: array[$ff60..$ff6b] of Word = (keySelect, keyPrintScreen,
    keyExecute, keyInsert, keyNIL, keyUndo, keyRedo, keyMenu, keyFind,
    keyCancel, keyHelp, keyBreak);
  Table_ff9X: array[$ff91..$ff9f] of Word = (keyPF1, keyPF2, keyPF3, keyPF4,
    keyP7, keyP4, keyP8, keyP6, keyP2, keyP9, keyP3, keyP1, keyP5, keyP0,
    keyPDecimal);
  Table_ffeX: array[$ffe1..$ffee] of Word = (keyShiftL, keyShiftR, keyCtrlL,
    keyCtrlR, keyCapsLock, keyShiftLock, keyMetaL, keyMetaR, keyAltL, keyAltR,
    keySuperL, keySuperR, keyHyperL, keyHyperR);
begin
  case KeySym of
    0..Ord('a')-1, Ord('z')+1..$bf, $f7:
      Result := KeySym;
    Ord('a')..Ord('z'), $c0..$f6, $f8..$ff:
      Result := KeySym - 32;  // ignore case: convert lowercase a-z to A-Z keysyms;
    $20a0..$20ac: Result := Table_20aX[KeySym];
    $fe20: Result := keyTab;
    $fe50..$fe60: Result := Table_feXX[KeySym];
    XKB_KEY_BackSpace:  Result := keyBackspace;
    XKB_KEY_Tab:        Result := keyTab;
    XKB_KEY_Linefeed:   Result := keyLinefeed;
    $ff0b: Result := keyClear;
    $ff0d: Result := keyReturn;
    $ff13: Result := keyPause;
    $ff14: Result := keyScrollLock;
    $ff15: Result := keySysRq;
    $ff1b: Result := keyEscape;
    $ff50..$ff58: Result := Table_ff5X[KeySym];
    $ff60..$ff6b: Result := Table_ff6X[KeySym];
    $ff7e: Result := keyModeSwitch;
    $ff7f: Result := keyNumLock;
    $ff80: Result := keyPSpace;
    $ff89: Result := keyPTab;
    $ff8d: Result := keyPEnter;
    $ff91..$ff9f: Result := Table_ff9X[KeySym];
    $ffaa: Result := keyPAsterisk;
    $ffab: Result := keyPPlus;
    $ffac: Result := keyPSeparator;
    $ffad: Result := keyPMinus;
    $ffae: Result := keyPDecimal;
    $ffaf: Result := keyPSlash;
    $ffb0..$ffb9: Result := keyP0 + KeySym - $ffb0;
    $ffbd: Result := keyPEqual;
    $ffbe..$ffe0: Result := keyF1 + KeySym - $ffbe;
    $ffe1..$ffee: Result := Table_ffeX[KeySym];
    $ffff: Result := keyDelete;
  else
    Result := keyNIL;
  end;

{$IFDEF GDebug}
  if Result = keyNIL then
    DebugLn('fpGui/Wayand: Unknown KeySym: $' + IntToHex(KeySym, 4));
{$ENDIF}


end;

function MouseCursorToXcursorName(ACursor: TMouseCursor): String;
begin
  case ACursor of
     mcDefault  :  Result := 'left_ptr';
     mcArrow    :  Result := 'arrow';
     mcCross    :  Result := 'crosshair';
     mcIBeam    :  Result := 'xterm';
     mcSizeEW   :  Result := 'sb_h_double_arrow';
     mcSizeNS   :  Result := 'sb_v_double_arrow';
     mcSizeNWSE :  Result := 'bottom_right_corner';
     mcSizeNESW :  Result := 'bottom_left_corner';
     mcSizeSWNE :  Result := 'top_right_corner';
     mcSizeSENW :  Result := 'top_left_corner';
     mcMove     :  Result := 'fleur';
     mcHourGlass:  Result := 'watch';
     mcHand     :  Result := 'hand2';
     mcDrag     :  Result := 'target';
     mcNoDrop   :  Result := 'pirate';
  end;
end;


procedure TfpgWaylandWindow.SendConfigureMessage(Sender: TObject;
  AEdges: LongWord; AWidth, AHeight: LongInt);
var
  msgp: TfpgMessageParams;
  lWidget: TfpgWidget;
  lWindowSize: TfpgSize; // the size our window is without decorations
  lDecor: TfpgWaylandDecorator;
begin
  lWidget := TfpgWidget(Owner);

  lWindowSize.SetSize(AWidth, AHeight);

  if Assigned(FDecor) then
  begin
    lDecor := TfpgWaylandDecorator(FDecor);
    Dec(lWindowSize.W, lDecor.BorderWidthIncrease);
    Dec(lWindowSize.H, lDecor.BorderHeightIncrease);
  end;

  if AWidth + AHeight = 0 then
  begin

    if (lWindowSize.W <> lWidget.Width) or (lWindowSize.H <> lWidget.Height) then
    begin
      msgp.rect.SetRect(0,0, lWindowSize.W, lWindowSize.H);

      // change size to exclude decoration area
      {if Assigned(FDecor) then
        msgp.rect.InflateRect(-lDecor.BorderWidthIncrease, -lDecor.BorderHeightIncrease);}

      fpgSendMessage(Self, Self, FPGM_RESIZE, msgp);
      //fpgPostMessage(Self, Self, FPGM_PAINT, msgp);
      //DoUpdateWindowPosition;
    end
    else
    begin
      msgp.rect.SetRect(0,0, lWidget.Width, lWidget.Height);
      //fpgSendMessage(Self, Self, FPGM_PAINT, msgp);

    end;
    Exit; // means no changes are needed
  end;
  // how to get x and y? I think compositors don't tell where the surface is mapped!

  WinHandle.SetClientSize(lWindowSize.W, lWindowSize.H);

  msgp.rect.SetRect(Left, Top, lWindowSize.W, lWindowSize.H);

  {if Assigned(FDecor) then
    msgp.rect.InflateRect(- lDecor.BorderWidthIncrease, -lDecor.BorderHeightIncrease);}

  fpgPostMessage(Self, Self, FPGM_RESIZE, msgp);
  FSize.SetSize(lWidget.Width, lWidget.Height);
  msgp.rect.SetRect(0, 0, lWindowSize.W, lWindowSize.H);
  //fpgSendMessage(Self, Self, FPGM_PAINT, msgp);
  fpgPostMessage(Self, Self, FPGM_PAINT, msgp);
end;

procedure TfpgWaylandWindow.DecoratorPaint(Sender: TObject);
var
  lCanvas: TAgg2dWaylandBufferCanvas;
begin
  {lCanvas := TAgg2dWaylandBufferCanvas(FDecoratorCanvas);


  lCanvas.Begindraw(lCanvas,0 , 0);
  lCanvas.SetFont(FFont);
  lCanvas.Color:=clRed;
  lCanvas.FillRectangle(0,0, FDecoratorHandle.Width, FDecoratorHandle.Height);

  lCanvas.SetTextColor(clWhiteSmoke);

  lcanvas.DrawString(5,5, FWindowTitle);
  fpgStyle.DrawButtonFace(TfpgCanvas(lCanvas), fpgRect(FDecoratorHandle.Width-FDecoratorHandle.BorderRight-15, FDecoratorHandle.BorderBottom,15, 15), [btfHover]);

  lCanvas.EndDraw(0,0, FDecoratorHandle.Width, FDecoratorHandle.Height);}
end;

procedure TfpgWaylandWindow.SendCloseWindowMessage(Sender: TObject);
begin
  fpgSendMessage(Self, Self, FPGM_CLOSE);
end;

procedure TfpgWaylandWindow.DecoratorConfigure(Sender: TObject;
  AEdges: LongWord; AWidth, AHeight: LongInt);
begin
  //
  WriteLn('Decorator Configure');
end;

procedure TfpgWaylandWindow.DoAllocateWindowHandle(AParent: TfpgWidgetBase);
var
  lPopupFor :TfpgwWindow = nil;
  lParentWin : TfpgwWindow = nil;
  lName: String;
  lWidth,
  lHeight: Integer;
  msgp: TfpgMessageParams;
  lActiveWindow: TfpgwWindow;
begin
  if FWinHandle = nil then
  begin
    if Assigned(AParent) then
      lParentWin := TfpgWaylandWindow(AParent.Window).WinHandle;
    // wayland needs a window that the popup is positioned relative to
    if WindowType = wtPopup then
    begin
      lActiveWindow := lDisplay.Display.ActiveMouseWin;
      if not Assigned(lActiveWindow) then
        lActiveWindow := TfpgWaylandWindow(fpgApplication.MainForm.Window).WinHandle;
      if not Assigned(lActiveWindow) then
        raise Exception.Create('Unable to find a window to associate with popup window');

      lPopupFor := lActiveWindow;
    end;

    lHeight := Height;
    lWidth  := Width;
    if WindowType in [wtWindow, wtModalForm] then
    begin
      Inc(lHeight, TfpgWaylandDecorator.BorderHeightIncrease);
      Inc(lWidth, TfpgWaylandDecorator.BorderWidthIncrease);
      //msgp.rect.SetRect(left,top, lWidth,lHeight);
      //fpgSendMessage(nil, Owner, FPGM_RESIZE, msgp);
    end;

    FWinHandle := TfpgwWindow.Create(Self, lDisplay.Display, lParentWin, Left, Top, lWidth, lHeight, lPopupFor);
    {if Assigned(FDecoratorHandle) then
    begin
      FDecoratorHandle.Host:= FWinHandle;
      wl_surface_commit(FWinHandle.SurfaceShell.Surface);
      //lDisplay.Display.Dispatch;
    end;}

    if WindowType in [wtWindow, wtModalForm] then
    begin
      FDecor := TfpgWaylandDecorator.Create(Self, FWinHandle);
      FWinHandle.SurfaceShell.SetTitle(FWindowTitle);
      {if FWinHandle.SurfaceShell is TfpgwXDGShellSurface then
        TfpgwXDGShellSurface(FWinHandle.SurfaceShell).Toplevel.SetAppId(ApplicationName);}


      {lName := 'Liberation Sans-12:bold';//FPG_DEFAULT_FIXED_FONT_DESC+'-16:bold';
      FFont := fpgGetFont(lName);
      FDecoratorHandle := TfpgwWindowDecorator.Create(Self, lDisplay.Display, Width, Height, 5,5,25,5);
      FDecoratorCanvas := TAgg2dWaylandBufferCanvas.Create(FDecoratorHandle);
      TAgg2dWaylandBufferCanvas(FDecoratorCanvas).SetFont(FFont);
      lParentWin := FDecoratorHandle;
      FDecoratorHandle.OnPaint:=@DecoratorPaint;
      FDecoratorHandle.OnConfigure:=@DecoratorConfigure;}
    end;

    if WindowType = wtPopup then
      lDisplay.FPopupStack.Add(Self);
    FWinHandle.OnPaint:=@SendPaintMessage;
    FWinHandle.OnConfigure:=@SendConfigureMessage;
    FWinHandle.OnClose:=@SendCloseWindowMessage;
    FWinHandle.Redraw;
    {if Assigned(FDecoratorHandle) then
    begin

      FDecoratorHandle.Redraw;
    end;}
  end;
  SetWindowParameters;
end;

procedure TfpgWaylandWindow.DoReleaseWindowHandle;
begin
  if FWinHandle <> nil then
  begin
    if WindowType = wtPopup then
      lDisplay.RemoveWindowFromPopupStack(Self);
    FWinHandle.Free;
    if Assigned(FDecor) then
      FreeAndNil(FDecor);
  end;
  FWinHandle := nil;
end;

procedure TfpgWaylandWindow.DoRemoveWindowLookup;
begin

end;

procedure TfpgWaylandWindow.DoSetWindowAttributes(const AOldAtributes,
  ANewAttributes: TWindowAttributes; const AForceAll: Boolean);
begin

end;

procedure TfpgWaylandWindow.DoSetWindowVisible(const AValue: Boolean);
begin

end;

function TfpgWaylandWindow.HandleIsValid: boolean;
begin
  Result := FWinHandle <> nil;
end;

procedure TfpgWaylandWindow.DoSetWindowTitle(const ATitle: string);
var
  lWin: TfpgwWindow;
begin
  if Assigned(FDecor) then
    TfpgWaylandDecorator(FDecor).Title:=ATitle;
  if ATitle = FWindowTitle then
    Exit;
  FWindowTitle:=ATitle;
  if Assigned(FDecor) then
    DecoratorPaint(Self);
  if HasHandle then
  begin

    {if FDecoratorHandle <> nil then
      lWin := FDecoratorHandle
    else}
      lWin := WinHandle;
    lWin.SurfaceShell.SetTitle(ATitle);

  end;
end;

procedure TfpgWaylandWindow.DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord);
var
  lXDGSurface: TfpgwXDGShellSurface;
begin

  // not really supported.... we can start a move from a button press...
  if FWinHandle.SurfaceShell is TfpgwXDGShellSurface then
  begin

    //WriteLn(Format('DoMoveWindow = %d:%d',[x, y]));
    //lXDGSurface := TfpgwXDGShellSurface(FWinHandle.SurfaceShell);
    // this is only to tell the compositor about the area of the window that the
    // user can interact with! Has nothing to do with window placement. The
    // examples I found use this to exclude window effects like shadows on the
    // exterior.
    //lXDGSurface.Surface.SetWindowGeometry(x,y, FWinHandle.GetWidth, FWinHandle.GetHeight);
  end;
end;

function TfpgWaylandWindow.DoWindowToScreen(ASource: TfpgWindowBase;
  const AScreenPos: TPoint): TPoint;
begin
  WriteLn(Format('Window to  screen Screenpos = %d:%d',[AScreenPos.x, AScreenPos.y]));
  Result := AScreenPos;
end;

procedure TfpgWaylandWindow.DoUpdateWindowPosition;
var
  lXDGSurface: TfpgwXDGShellSurface;
begin
  Writeln('window wants to update position');
  if not Assigned(FWinHandle) then
    Exit;

  if FWinHandle.SurfaceShell is TfpgwXDGShellSurface then
  begin
    WriteLn(Format('DoUpdateWindowPosition = x%d:y%d[w%d:h%d]',[Left, Top, FWinHandle.GetWidth, FWinHandle.GetHeight]));
    lXDGSurface := TfpgwXDGShellSurface(FWinHandle.SurfaceShell);
    //lXDGSurface.Surface.SetWindowGeometry(0,0, FWinHandle.GetWidth, FWinHandle.GetHeight);
  end;
end;

procedure TfpgWaylandWindow.DoSetMouseCursor;
begin
  lDisplay.Display.SetCursor(MouseCursorToXcursorName(FMouseCursor));
end;

procedure TfpgWaylandWindow.DoDNDEnabled(const AValue: boolean);
begin

end;

function TfpgWaylandWindow.GetWindowState: TfpgWindowState;
begin
  if not HandleIsValid then
    Exit;
  Result := wsNormal;
  if WinHandle.IsMaximized then
    Result := wsMaximized;
end;

procedure TfpgWaylandWindow.SetWindowState(const AValue: TfpgWindowState);
begin
  inherited SetWindowState(AValue);
end;

procedure TfpgWaylandWindow.SetWindowOpacity(AValue: Single);
begin
  inherited SetWindowOpacity(AValue);
end;

function TfpgWaylandWindow.GetBufferDrawOffset: DWord;
var
  lDecor: TfpgWaylandDecorator;
begin
  REsult := 0;
  if not Assigned(FDecor) then
    Exit;

  lDecor := TfpgWaylandDecorator(FDecor);

  Result := FWinHandle.GetWidth * lDecor.BorderTop + lDecor.BorderLeft;
end;

procedure TfpgWaylandWindow.AdjustMousePos(var AX, AY: Integer);
var
  lDecor: TfpgWaylandDecorator;
begin
  if not Assigned(FDecor) then
    Exit;

  lDecor := TfpgWaylandDecorator(FDecor);
  Dec(AX, lDecor.BorderLeft);
  Dec(AY, lDecor.BorderTop);
end;

procedure TfpgWaylandWindow.AdjustPaintPos(var AX, AY: Integer);
var
  lDecor: TfpgWaylandDecorator;
begin
  if not Assigned(FDecor) then
    Exit;

  lDecor := TfpgWaylandDecorator(FDecor);
  Inc(AX, lDecor.BorderLeft);
  Inc(AY, lDecor.BorderTop);
end;

procedure TfpgWaylandWindow.DecoratorDraw(ABuffer: TfpgwBuffer);
begin
  if not Assigned(FDecor) then
    Exit;

  TfpgWaylandDecorator(FDecor).Draw(ABuffer);
end;

procedure TfpgWaylandWindow.HandleDecorationButton(AMsg: DWord;
  AParams: TfpgMessageParams);
begin
  if AMsg = FPGM_MOUSEDOWN then
  begin
    if (FMousePos.X < 0 ) and (FMousePos.Y < 0) then
      FWinHandle.SurfaceShell.Resize(FWinHandle.Display.EventSerial, WL_SHELL_SURFACE_RESIZE_TOP_LEFT)
    else if (FMousePos.X < 0 ) and (FMousePos.Y > Height) then
      FWinHandle.SurfaceShell.Resize(FWinHandle.Display.EventSerial, WL_SHELL_SURFACE_RESIZE_BOTTOM_LEFT)
    else if (FMousePos.X > Width) and (FMousePos.Y < 0) then
      FWinHandle.SurfaceShell.Resize(FWinHandle.Display.EventSerial, WL_SHELL_SURFACE_RESIZE_TOP_RIGHT)
    else if (FMousePos.X > Width) and (FMousePos.Y > Height) then
      FWinHandle.SurfaceShell.Resize(FWinHandle.Display.EventSerial, WL_SHELL_SURFACE_RESIZE_BOTTOM_RIGHT)
    else if (FMousePos.X < 0 ) then
      FWinHandle.SurfaceShell.Resize(FWinHandle.Display.EventSerial, WL_SHELL_SURFACE_RESIZE_LEFT)
    else if (FMousePos.X > Width ) then
      FWinHandle.SurfaceShell.Resize(FWinHandle.Display.EventSerial, WL_SHELL_SURFACE_RESIZE_RIGHT)
    else if (FMousePos.Y < -15)  then
      FWinHandle.SurfaceShell.Resize(FWinHandle.Display.EventSerial, WL_SHELL_SURFACE_RESIZE_TOP)
    else if (FMousePos.Y > Height) then
      FWinHandle.SurfaceShell.Resize(FWinHandle.Display.EventSerial, WL_SHELL_SURFACE_RESIZE_BOTTOM)
    else
      FWinHandle.SurfaceShell.Move(FWinHandle.Display.EventSerial);
  end;
end;

procedure TfpgWaylandWindow.HandleDecorationMove;
var
  lDisplay: TfpgwDisplay;
begin
  lDisplay := FWinHandle.Display;

  // Top Left
  if (FMousePos.X < 0 ) and (FMousePos.Y < 0) then
    lDisplay.SetCursor(['top_left_arrow', 'top_left_corner'])
  // Top Right
  else if (FMousePos.X > Width) and (FMousePos.Y < 0) then
    lDisplay.SetCursor(['top_right_arrow', 'top_right_corner'])
  // Bottom Left
  else if (FMousePos.X < 0 ) and (FMousePos.Y > Height) then
    lDisplay.SetCursor(['bottom_left_arrow', 'bottom_left_corner', 'll_angle'])
  // Bottom Right
  else if (FMousePos.X > Width) and (FMousePos.Y > Height) then
    lDisplay.SetCursor(['bottom_right_arrow', 'bottom_right_corner'])
  // Left
  else if (FMousePos.X < 0 ) then
      lDisplay.SetCursor(['left_arrow', 'left_side'])
  // Right
  else if (FMousePos.X > Width ) then
      lDisplay.SetCursor(['right_arrow', 'right_side'])
  // Top
  else if (FMousePos.Y < -15)  then
      lDisplay.SetCursor(['sb_up_arrow', 'top_side', 'based_arrow_up'])
  // Bottom
  else if (FMousePos.Y > Height) then
      lDisplay.SetCursor(['sb_down_arrow', 'bottom_side', 'based_arrow_down'])
    else
      lDisplay.SetCursor(['left_ptr', 'arrow'])


end;

constructor TfpgWaylandWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);


end;

destructor TfpgWaylandWindow.Destroy;
begin
  fpgDeleteMessagesForTarget(Self);
  inherited Destroy;
end;

procedure TfpgWaylandWindow.ActivateWindow;
begin

end;

procedure TfpgWaylandWindow.CaptureMouse(AForWidget: TfpgWidgetBase);
begin

end;

procedure TfpgWaylandWindow.ReleaseMouse;
begin

end;

procedure TfpgWaylandWindow.SetFullscreen(AValue: Boolean);
begin
  if not HasHandle then
    Exit;

  WinHandle.SurfaceShell.SetFullScreen(AValue);
end;

procedure TfpgWaylandWindow.BringToFront;
begin

end;

{ TfpgWaylandSystemTrayHandler }

function TfpgWaylandSystemTrayHandler.IsSystemTrayAvailable: Boolean;
begin
  // wayland howto?
  Result := False;
end;

function TfpgWaylandSystemTrayHandler.SupportsMessages: Boolean;
begin
  Result := False
end;

procedure TfpgWaylandSystemTrayHandler.Show;
begin
  // :)
end;

{ TfpgWaylandDrag }

function TfpgWaylandDrag.Execute(const ADropActions: TfpgDropActions;
  const ADefaultAction: TfpgDropAction): TfpgDropAction;
begin

end;

{ TfpgWaylandApplication }

procedure TfpgWaylandApplication.KeyboardRepeatDelayExpired(Sender: TObject);
begin
  TfpgTimer(Sender).OnTimer:=@KeyboardRepeatKeyTimer;
  KeyboardRepeatKeyTimer(Self);
  TfpgTimer(Sender).Interval:=FKeyboardRepeatRate;
end;

procedure TfpgWaylandApplication.KeyboardRepeatKeyTimer(Sender: TObject);
var
  msgp: TfpgMessageParams;
  lKeyChar: UTF8String;
begin
  msgp.keyboard.shiftstate := FKeyboard.ModState;
  msgp.keyboard.keycode:=KeySymToKeycode(TKeyboardTimer(Sender).KeyCode);
  fpgPostMessage(nil, nil {win}, FPGM_KEYPRESS, msgp);
  lKeyChar := FKeyboard.KeySymToUtf8(TKeyboardTimer(Sender).KeyCode);
  if lKeyChar <> '' then
  begin
    msgp.keyboard.keychar:= lKeyChar[1];
    fpgPostMessage(nil, nil{win}, FPGM_KEYCHAR, msgp);
  end;


end;

procedure TfpgWaylandApplication.SendKeyboardEnterMessage(Sender: TObject;
  AKeys: Pwl_array);
begin
  if not Assigned(FKeyTimer) then
  begin
    FKeyTimer := TfpgTimer.Create(1000);
    TfpgTimer(FKeyTimer).Enabled:=False;
  end;
  fpgPostMessage(nil, Sender, FPGM_ACTIVATE);
end;

procedure TfpgWaylandApplication.SendKeyboardKey(Sender: TObject; ATime, AKey,
  AState: LongWord);
var
  msg: DWord;
  msgp: TfpgMessageParams;
  lKeySym: xkb_keysym_t;
  lChars: UTF8String;
  i, lNumSyms: Integer;
  lCode: LongWord;
  lKeySyms: Pxkb_keysym_t;
begin
  lCode := AKey+8; // yes I know....
  lNumSyms := FKeyboard.KeyGetSyms(lCode, @lKeySyms);
  lKeySym := lKeySyms[0];

  case AState of
    WL_KEYBOARD_KEY_STATE_PRESSED:
      begin
        msg := FPGM_KEYPRESS;
        case FKeyboard.Feed(AKey) of
          XKB_COMPOSE_FEED_IGNORED : ;
          XKB_COMPOSE_FEED_ACCEPTED:
          begin
            case FKeyboard.ComposeStatus of
              XKB_COMPOSE_NOTHING: ;
              XKB_COMPOSE_COMPOSING: Exit;
              XKB_COMPOSE_COMPOSED: lKeySym := FKeyboard.LookupSym;
              XKB_COMPOSE_CANCELLED: ;
            end;
          end;
        end;
      end;
    WL_KEYBOARD_KEY_STATE_RELEASED:
      begin
        TfpgTimer(FKeyTimer).Enabled:=False;
        msg := FPGM_KEYRELEASE;
        if FKeyboard.ComposeStatus = XKB_COMPOSE_COMPOSED then
        begin
          lKeySym := FKeyboard.LookupSym;
          FKeyboard.ResetCompose;
        end;
      end;
  end;


  msgp.keyboard.keycode :=  KeySymToKeycode(lKeySym);
  msgp.keyboard.shiftstate:=FKeyboard.ModState;

  fpgPostMessage(nil, Sender, msg, msgp);
  if msg = FPGM_KEYPRESS then
  begin
    StartRepeatDelay(lKeySym);
    lChars := FKeyboard.KeySymToUtf8(lKeySym);
    for i := 1 to UTF8Length(lChars) do
    begin
      msgp.keyboard.keychar := UTF8Copy(lChars, i, 1);
      fpgPostMessage(nil, Sender, FPGM_KEYCHAR, msgp);
    end;
  end;
end;

procedure TfpgWaylandApplication.SendKeyboardLeaveMessage(Sender: TObject);
begin
  fpgPostMessage(nil, Sender, FPGM_DEACTIVATE);
end;

procedure TfpgWaylandApplication.SendMouseAxisMessage(Sender: TObject;
  ATime: LongWord; AAxis: LongWord; AValue: LongInt);
var
  msgp: TfpgMessageParams;
  msg: Integer;
  lDest: TfpgWidgetBase;
  lWin: TfpgWaylandWindow absolute Sender;
begin
  //WriteLn('Axis: ', AAxis, ' value ', AValue);
  case AAxis of
    WL_POINTER_AXIS_VERTICAL_SCROLL: msg:=FPGM_SCROLL;
    WL_POINTER_AXIS_HORIZONTAL_SCROLL: msg:=FPGM_HSCROLL;
  else
    Exit;
  end;


  msgp.mouse.x          := lWin.FMousePos.X;
  msgp.mouse.y          := lWin.FMousePos.Y;
  msgp.mouse.Buttons    := 0;//
  msgp.mouse.delta    := AValue shr 8;//
  msgp.mouse.shiftstate := FShiftState;

  fpgPostMessage(nil, Sender, msg, msgp);


end;

procedure TfpgWaylandApplication.SendMouseButtonMessage(Sender: TObject; ATime: LongWord; AButton: LongWord; AState: LongInt);
var
  lWin: TfpgWaylandWindow absolute Sender;
  lButton, lMsg: Integer;
  lEnum: TShiftStateEnum;
  msgp: TfpgMessageParams;
begin
  if not WindowInPopupStack(lWin) then
    ClosePopups;
  // update mouse state
  case AButton of
     BTN_LEFT:
       begin
         lButton:= MOUSE_LEFT;
         lEnum:=ssLeft;
       end;
     BTN_RIGHT:
       begin
         lButton:= MOUSE_RIGHT;
         lEnum:= ssRight;
       end;
     BTN_MIDDLE:
       begin
         lButton:= MOUSE_MIDDLE;
         lEnum:=ssMiddle;
       end
  else
    lButton:=0;
  end;
  if lButton = 0 then
    exit;

  case AState of
    WL_POINTER_BUTTON_STATE_PRESSED :
      begin
        Include(FShiftState, lEnum);
        lMsg:= FPGM_MOUSEDOWN;
      end;
    WL_POINTER_BUTTON_STATE_RELEASED:
      begin
        Exclude(FShiftState, lEnum);
        lMsg:= FPGM_MOUSEUP;
      end;
  end;

  msgp.mouse.Buttons:=lButton;
  msgp.mouse.x:= lWin.FMousePos.X;
  msgp.mouse.y:= lWin.FMousePos.Y;
  msgp.mouse.shiftstate:=FShiftState;

  if (lWin.FMousePos.X<0)
  or (lWin.FMousePos.Y<0)
  or (lWin.FMousePos.X>lWin.Width)
  or (lWin.FMousePos.Y>lWin.Height)
  then
    lWin.HandleDecorationButton(lMsg, msgp)
  else
    fpgPostMessage(nil, lWin, lMsg, msgp);
end;

procedure TfpgWaylandApplication.SendMouseEnterMessage(Sender: TObject; AX,
  AY: Integer);
var
  lWin: TfpgWaylandWindow absolute Sender;
begin
  lWin.AdjustMousePos(AX, AY); { for decorations }
  lWin.FMousePos.SetPoint(AX, AY);
  fpgPostMessage(nil, lWin, FPGM_MOUSEENTER);
  lwin.DoSetMouseCursor;
end;

procedure TfpgWaylandApplication.SendMouseLeaveMessage(Sender: TObject);
var
  lWin: TfpgWaylandWindow absolute Sender;
begin
  fpgPostMessage(nil, lWin, FPGM_MOUSEEXIT);
end;

procedure TfpgWaylandApplication.SendMouseMotionMessage(Sender: TObject;
  ATime: LongWord; AX, AY: Integer);
var
  msgp: TfpgMessageParams;
  lWin: TfpgWaylandWindow absolute Sender;
begin
  lWin.AdjustMousePos(AX, AY); { for decorations }

  lWin.FMousePos.SetPoint(AX, AY);
  msgp.mouse.x          := ax;
  msgp.mouse.y          := ay;
  if ssLeft in FShiftState then
    msgp.mouse.Buttons := MOUSE_LEFT
  else if ssMiddle in FShiftState then
    msgp.mouse.Buttons := MOUSE_MIDDLE
  else if ssRight in FShiftState then
    msgp.mouse.Buttons := MOUSE_RIGHT
  else
    msgp.mouse.Buttons    := 0;//

  msgp.mouse.shiftstate := FShiftState;

  if (lWin.FMousePos.X<0)
  or (lWin.FMousePos.Y<0)
  or (lWin.FMousePos.X>lWin.Width)
  or (lWin.FMousePos.Y>lWin.Height)
  then
    lWin.HandleDecorationMove
  else
    fpgPostMessage(nil, lWin, FPGM_MOUSEMOVE, msgp);


end;

procedure TfpgWaylandApplication.StartRepeatDelay(AKeyCode: Word);
var
  lTimer: TKeyboardTimer;
begin
  lTimer:= TKeyboardTimer(FKeyTimer);
  lTimer.KeyCode := AKeyCode;
  lTimer.Enabled := False;
  lTimer.Interval:=FKeyboardRepeatDelay ;
  lTimer.Enabled := True;
  lTimer.OnTimer:=@KeyboardRepeatDelayExpired;

end;

procedure TfpgWaylandApplication.SetKeyboardRepeat(Sender: TObject; ARate,
  ADelay: LongInt);
begin
  FKeyboardRepeatDelay:=ADelay;
  FKeyboardRepeatRate:=ARate;
end;

procedure TfpgWaylandApplication.SetupKeymap(Sender: TObject;
  AFormat: LongWord; AFileDesc: LongInt; ASize: LongInt);
begin
  case AFormat of
    WL_KEYBOARD_KEYMAP_FORMAT_NO_KEYMAP: { dunno };
    WL_KEYBOARD_KEYMAP_FORMAT_XKB_V1:
      begin
        FKeyboard := TxkbHelper.create(AFileDesc, ASize);
      end;
    else
      raise EfpGUIException.CreateFmt('fpGui/Wayland: Unexpected keymap format "%d"', [AFormat]);
  end;


end;

procedure TfpgWaylandApplication.UpdateKeyState(Sender: TObject;
  AModsDepressed, AModsLatched, AModsLocked, AGroup: LongWord);
begin
  FKeyboard.UpdateKeyState(AModsDepressed, AModsLatched, AmodsLocked, AGroup);
end;

procedure TfpgWaylandApplication.DoFlush;
begin
  Display.Flush;
end;

function TfpgWaylandApplication.DoGetFontFaceList: TStringList;
var
  config: PFcConfig;
  pat: PFcPattern;
  os: PFcObjectSet;
  fs: PFcFontSet;
  font: PFcPattern;
  f, style, family: PFcChar8;
  i: Integer;
begin
  Result := TStringList.Create;

  config := FcInitLoadConfigAndFonts();
  pat := FcPatternCreate();
  os := FcObjectSetBuild (pcchar(PChar(FC_FAMILY)), [FC_STYLE, FC_LANG, FC_FILE, nil]);
  fs := FcFontList(config, pat, os);

  for i := 0 to fs^.nfont -1 do
  begin
    font := fs^.fonts[i];
    if  (FcPatternGetString(font, pcchar(PChar(FC_FILE)), 0, @f) = FcResultMatch)
    and (FcPatternGetString(font,  pcchar(PChar(FC_FAMILY)), 0, @family) = FcResultMatch)
    and (FcPatternGetString(font,  pcchar(PChar(FC_STYLE)), 0, @style) = FcResultMatch)
    then
    begin
      //printf("Filename: %s (family %s, style %s)\n", file, family, style);
      REsult.Add(family+' '+style);
      //WriteLn(family+' '+style);
    end;
  end;

  if Assigned(fs) then
    FcFontSetDestroy(fs);
end;

procedure TfpgWaylandApplication.DoWaitWindowMessage(atimeoutms: integer);
begin
  FDisplay.WaitEvent(atimeoutms);
end;

function TfpgWaylandApplication.MessagesPending: boolean;
begin
  Result := FDisplay.HasEvent();
end;

procedure TfpgWaylandApplication.ClosePopups;
var
  i: Integer;
begin
  for i := FPopupStack.Count-1 downto 0 do
  begin
     TfpgWidget(TfpgWaylandWindow(FPopupStack[i]).Owner).Visible:=False;
  end;
  FPopupStack.Clear;
end;

function TfpgWaylandApplication.WindowInPopupStack(AWindow: TfpgWaylandWindow): Boolean;
var
  w: TfpgWaylandWindow;
begin
  Result := False;
  for Pointer(w) in FPopupStack do
    if w = AWindow then
      Exit(True);
end;

procedure TfpgWaylandApplication.RemoveWindowFromPopupStack(
  Awindow: TfpgWaylandWindow);
var
  FReleaseIndex: Integer = MaxInt;
  i: Integer;
begin
  for i := 0 to FPopupStack.Count-1 do
  begin
    if TfpgWaylandWindow(FPopupStack[i]) = Awindow then
    begin
      FReleaseIndex:=i;
    end;
    if i >= FReleaseIndex then
      TfpgWidget(TfpgWaylandWindow(FPopupStack[i]).Owner).Visible:=False;
  end;

  if FReleaseIndex <> MaxInt then
    FPopupStack.Count:=FReleaseIndex;
end;

constructor TfpgWaylandApplication.Create(const AParams: string);
var
  s: String = '';
begin
  inherited Create(AParams);
  lDisplay := Self;
  FIsInitialized:=False;
  FPopupStack := TFPList.Create;

  FKeyboardRepeatDelay:=300;
  FKeyboardRepeatRate:=40;
  if gCommandLineParams.IsParam('display') then
    s := gCommandLineParams.GetParam('display');

  FDisplay := TfpgwDisplay.TryCreate(Self, '');

  if FDisplay = nil then
    raise Exception.Create('fpGUI-Wayland: Could not open the display. Is your Wayland compositor running?');

  FDisplay.OnMouseEnter:=@SendMouseEnterMessage;
  FDisplay.OnMouseLeave:=@SendMouseLeaveMessage;
  FDisplay.OnMouseMotion:=@SendMouseMotionMessage;
  FDisplay.OnMouseButton:=@SendMouseButtonMessage;
  FDisplay.OnMouseAxis:=@SendMouseAxisMessage;
  FDisplay.OnKeyboardEnter:=@SendKeyboardEnterMessage;
  FDisplay.OnKeyboardLeave:=@SendKeyboardLeaveMessage;
  FDisplay.OnKeyboardKeymap:=@SetupKeymap;
  FDisplay.OnKeyboardKey:=@SendKeyboardKey;
  FDisplay.OnKeyboardModifiers:=@UpdateKeyState;
  // tells us how to repeat keys
  FDisplay.OnKeyBoardRepeatInfo:=@SetKeyboardRepeat;

  FDisplay.AfterCreate;

  Terminated := False;

  LoadFontConfigLib('');

  FFontConfig := FcInitLoadConfigAndFonts();
  FT_Init_FreeType(FFreeType);

  FIsInitialized:=True;

  FSelection := TfpgWaylandClipboard.Create;

end;

destructor TfpgWaylandApplication.Destroy;
begin
  FDisplay.Free;
  FPopupStack.Free;
  UnLoadFontConfigLib;
  inherited Destroy;
end;

function TfpgWaylandApplication.GetScreenWidth: TfpgCoord;
begin
  Result:= 1650;
end;

function TfpgWaylandApplication.GetScreenHeight: TfpgCoord;
begin
  Result:= 1280;
end;

function TfpgWaylandApplication.GetScreenPixelColor(APos: TPoint): TfpgColor;
begin

end;

function TfpgWaylandApplication.Screen_dpi_x: integer;
begin
  Result := 96;

end;

function TfpgWaylandApplication.Screen_dpi_y: integer;
begin
  Result := 96;

end;

function TfpgWaylandApplication.Screen_dpi: integer;
begin
  Result := 96;
end;

{ TfpgWaylandFontResource }

type
  hackFcObjectSetBuild = function(first:pchar):PFcObjectSet; varargs; cdecl;

procedure TfpgWaylandFontResource.UpdateTextMetric(AStr: String);
var
  lText: String;
  pTxt: PChar;
  lGlyph: glyph_cache_ptr;
  lFirst: Boolean = True;
  lX, lY: Double;
begin
  if AStr = FLastMetricStr then
    Exit;
  pTxt := PChar(AStr);
  lX := 0.0;
  lY := 0.0;
  while pTxt^ <> #0 do
  begin
    lGlyph := FFontCacheManager.glyph(int32u(pTxt^));
    if Assigned(lGlyph) then
    begin
      if lFirst then
        FFontCacheManager.add_kerning(@lX, @lY);

      lFirst := False;
      lX+= lGlyph^.advance_x;
      lY+= lGlyph^.advance_y;
    end;
  {  if Trunc(lGlyph^.bounds.y2 - lGlyph^.bounds.y1 + lY+2) > FLastMetric.H then
      FLastMetric.H:=Trunc(lGlyph^.bounds.y2 - lGlyph^.bounds.y1 + lY+2);
    WriteLn('g: ', lGlyph^.bounds.y1,'..',lGlyph^.bounds.y2);}
    Inc(ptrcomp(pTxt));
  end;

  FLastMetric.SetSize(Trunc(lX),FLastMetric.H);
  FLastMetricStr:=AStr;
end;

constructor TfpgWaylandFontResource.Create(const afontdesc: string);
var
 config:PFcConfig;
 pat: PFcPattern;
 os: PFcObjectSet;
 fs: PFcFontSet = nil;
 font: PFcPattern;
 f, style, family: PFcChar8;
 i: Integer;
 lFamily, lLang, lFile, lStyle: String;
 lFcObjectSetBuild: hackFcObjectSetBuild;
 lFontCacheItem : TFontCacheItem;
 lValue: TFcValue;
 searchresult: TFcResult;
begin
  FDesc := afontdesc;
  lFontCacheItem := FontCacheItemFromFontDesc(afontdesc, FSize);
  //writeln('Creating font resource: ', afontdesc, ' size=', Trunc(FSize));

  FFontEngine.Construct;
  FFontCacheManager.Construct(@FFontEngine);
  FFontEngine.load_font(PChar(lFontCacheItem.FileName) ,0 ,glyph_ren_outline);
  FFontEngine.hinting_(true);
  FFontEngine.height_(FSize *96 / 72);

  {Pointer(lFcObjectSetBuild) := FcObjectSetBuild;
  config := FcInitLoadConfigAndFonts();
  pat := FcPatternCreate();
  os := lFcObjectSetBuild (PChar(FC_FAMILY), PChar(FC_STYLE), PChar(FC_LANG), PChar(FC_FILE), nil);



  FcPatternAddString(pat, Pointer(PChar(FC_FAMILY)), PChar(lFontCacheItem.FamilyName));

  if lFontCacheItem.IsRegular then
    FcPatternAddString(pat, Pointer(PChar(FC_STYLE)), PChar('Regular'))
  else if lFontCacheItem.IsBold and lFontCacheItem.IsItalic then
    FcPatternAddString(pat, Pointer(PChar(FC_STYLE)), PChar('BoldItalic'))
  else if lFontCacheItem.IsBold then
    FcPatternAddString(pat, Pointer(PChar(FC_STYLE)), PChar('Bold'))
  else if lFontCacheItem.IsItalic then
    FcPatternAddString(pat, Pointer(PChar(FC_STYLE)), PChar('Italic'));

  FcConfigSubstitute(FcConfigGetCurrent(), pat, FcMatchPattern);

  font:=FcFontMatch(FcConfigGetCurrent(), pat, @searchresult);
  //lFontCacheItem

  if searchresult = FcResultMatch then
  begin
    FcPatternGet(font, Pointer(PChar(FC_FILE)), 0, @lValue);
    lFontCacheItem.FileName:=PChar(lValue.u.s);
    FcPatternGet(font, Pointer(PChar(FC_INDEX)), 0, @lValue);
    FT_New_Face(lDisplay.FFreeType, PChar(lFontCacheItem.FileName), lValue.u.i, FFont);
    //FT_Set_Char_Size(FFont, 0, 16*64, 300, 300);
    //FT_Set_Char_Size(FFont, 0, 16*64, 300, 300);
    //FT_Set_Pixel_Sizes(FFont, 0 shr 6 , 853 shr 6);
    FT_Set_Pixel_Sizes(FFont, 0, Trunc(FSize * 96 / 72));
  end;
   }
end;

destructor TfpgWaylandFontResource.Destroy;
begin
  FFontEngine.Destruct;
  FFontCacheManager.Destruct;
  inherited Destroy;
end;

function TfpgWaylandFontResource.HandleIsValid: Boolean;
begin
  //Result := FFont <> nil;
  Result := FFontEngine.m_last_error = 0;
end;

function TfpgWaylandFontResource.GetAscent: integer;
begin
  //Result := Trunc(FFont^.size^.metrics.ascender / 64);
  REsult := Trunc(FFontEngine.m_faces^^.size^.metrics.ascender / 64);
end;

function TfpgWaylandFontResource.GetDescent: integer;
begin
  //Result := Trunc(FFont^.size^.metrics.descender / 64);
end;

function TfpgWaylandFontResource.GetHeight: integer;
begin
  Exit(Trunc((FSize / 64) * 96{dpi}));
  {Result := 0;
  if FLastMetric.H = 0 then
    UpdateTextMetric('WxyGiZz,j');
  Result := FLastMetric.H;}
end;

function TfpgWaylandFontResource.GetTextWidth(const txt: string): integer;

begin
  Result := 0;
  UpdateTextMetric(txt);
  if txt = '' then
    Exit;

  Result := FLastMetric.W;
end;
initialization
  DefaultCanvasClass := TAgg2D;

end.

