unit gfxbase;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TfpgCoord = integer;     // we might use floating point coordinates in the future...
  TfpgColor = longword;

type
  TWindowType = (wtChild, wtWindow, wtModalForm, wtPopup);

  TWindowAttribute = (waSizeable, waAutoPos, waScreenCenterPos);
  TWindowAttributes = set of TWindowAttribute;

  TMouseCursor = (mcDefault, mcArrow, mcCross, mcIBeam, mcSizeEW, mcSizeNS,
      mcSizeNWSE, mcSizeNESW, mcMove, mcHourGlass);

const
  MOUSE_LEFT       = 1;
  MOUSE_RIGHT      = 2;
  MOUSE_MIDDLE     = 4;


  ss_Shift         = $0001;
  ss_Control       = $0004;
  ss_Alt           = $0008;
  ss_CapsLock      = $0002;
  ss_NumLock       = $0010;
  ss_ScrollLock    = $0080;

  // Platform independent messages used by fpGUI (TfpgWidget)
  FPGM_PAINT       = 1;
  FPGM_ACTIVATE    = 2;
  FPGM_DEACTIVATE  = 3;
  FPGM_KEYPRESS    = 4;
  FPGM_KEYRELEASE  = 5;
  FPGM_KEYCHAR     = 6;
  FPGM_MOUSEDOWN   = 7;
  FPGM_MOUSEUP     = 8;
  FPGM_MOUSEMOVE   = 9;
  FPGM_DOUBLECLICK = 10;
  FPGM_MOUSEENTER  = 11;
  FPGM_MOUSEEXIT   = 12;
  FPGM_CLOSE       = 13;
  FPGM_SCROLL      = 14;
  FPGM_RESIZE      = 15;
  FPGM_MOVE        = 16;
  FPGM_POPUPCLOSE  = 17;
  FPGM_KILLME      = 9999;


  // The special keys, based on the well-known keyboard scan codes
  KEY_LEFT         = $FF4B;
  KEY_RIGHT        = $FF4D;
  KEY_DOWN         = $FF50;
  KEY_UP           = $FF48;
  KEY_END          = $FF4F;
  KEY_HOME         = $FF47;
  KEY_PGUP         = $FF49;
  KEY_PGDN         = $FF51;
  KEY_INSERT       = $FF52;
  KEY_DELETE       = $FF53;
  KEY_F1           = $FF3B;
  KEY_F2           = KEY_F1 + 1;
  KEY_F3           = KEY_F1 + 2;
  KEY_F4           = KEY_F1 + 3;
  KEY_F5           = KEY_F1 + 4;
  KEY_F6           = KEY_F1 + 5;
  KEY_F7           = KEY_F1 + 6;
  KEY_F8           = KEY_F1 + 7;
  KEY_F9           = KEY_F1 + 8;
  KEY_F10          = KEY_F1 + 9;
  KEY_F11          = $FF57;
  KEY_F12          = $FF58;
  // some general keys
  KEY_TAB          = $0009;
  KEY_ENTER        = $000D;
  KEY_SPACE        = $0020;
  KEY_ESC          = $001B;
  KEY_BACKSPACE    = $0008;


  // scan codes for KeyPress/KeyRelease
  KEYSC_ENTER      = $1C;
  KEYSC_SPACE      = $39;

type
  TfpgRect = object  // not class for static allocations
    Top: TfpgCoord;
    Left: TfpgCoord;
    Width: TfpgCoord;
    Height: TfpgCoord;
    procedure SetRect(aleft, atop, awidth, aheight: TfpgCoord);
    function  Bottom: TfpgCoord;
    function  Right: TfpgCoord;
    procedure SetBottom(Value: TfpgCoord);
    procedure SetRight(Value: TfpgCoord);
  end;

type
  TfpgMsgParmMouse = record
    x: TfpgCoord;
    y: TfpgCoord;
    Buttons: word;
    shiftstate: word;
  end;

  TfpgMsgParmKeyboard = record
    keycode: word;
    shiftstate: word;
  end;

  TfpgMessageParams = record
    case integer of
      0: (mouse: TfpgMsgParmMouse);
      1: (keyboard: TfpgMsgParmKeyboard);
      2: (rect: TfpgRect);
  end;

  TfpgMessageRec = record
    MsgCode: integer;
    Sender: TObject;
    Dest: TObject;
    Params: TfpgMessageParams;
  end;
  PfpgMessageRec = ^TfpgMessageRec;

const
  FPG_DEFAULT_FONT_DESC = 'Arial-10';

const
  UserNamedColorStart   = 128;

// named color identifiers
const
  clWindowBackground = $80000001;
  clBoxColor        = $80000002;
  clButtonFace      = $80000003;
  clShadow1         = $80000004;
  clShadow2         = $80000005;
  clHilite1         = $80000006;
  clHilite2         = $80000007;
  clText1           = $80000008;
  clText2           = $80000009;
  clText3           = $8000000A;
  clText4           = $8000000B;
  clSelection       = $8000000C;
  clSelectionText   = $8000000D;
  clInactiveSel     = $8000000E;
  clInactiveSelText = $8000000F;
  clScrollBar       = $80000010;
  clListBox         = $80000011;
  clGridLines       = $80000012;
  clGridHeader      = $80000013;
  clWidgetFrame     = $80000014;
  clInactiveWgFrame = $80000015;
  clTextCursor      = $80000016;
  clChoiceListBox   = $80000017;
  clUnset           = $80000018;
  clMenuText        = $80000019;
  clMenuDisabled    = $8000001A;

type
  TfpgLineStyle = (lsSolid, lsDash, lsDot);


  TfpgImageBase = class(TObject)
  end;


  TfpgCanvasBase = class(TObject)
  end;


  TfpgFontResourceBase = class(TObject)
  end;


  TfpgFontBase = class(TObject)
  end;


  TfpgWindowBase = class(TComponent)
  protected
    FWindowType: TWindowType;
    FWindowAttributes: TWindowAttributes;
    FTop: TfpgCoord;
    FLeft: TfpgCoord;
    FWidth: TfpgCoord;
    FHeight: TfpgCoord;
    FMinWidth: TfpgCoord;
    FMinHeight: TfpgCoord;
    function    HandleIsValid: boolean; virtual; abstract;
  public
    // make some setup before the window shows
    procedure   AdjustWindowStyle; virtual;    // forms modify the window creation parameters
    procedure   SetWindowParameters; virtual;  // invoked after the window is created
    // general properties
    property    HasHandle: boolean read HandleIsValid;
    property    WindowType: TWindowType read FWindowType write FWindowType;
    property    WindowAttributes: TWindowAttributes read FWindowAttributes write FWindowAttributes;
    property    Left: TfpgCoord read FLeft write FLeft;
    property    Top: TfpgCoord read FTop write FTop;
    property    Width: TfpgCoord read FWidth write FWidth;
    property    Height: TfpgCoord read FHeight write FHeight;
    property    MinWidth: TfpgCoord read FMinWidth write FMinWidth;
    property    MinHeight: TfpgCoord read FMinHeight write FMinHeight;
  end;


  TfpgApplicationBase = class(TObject)
  protected
    FIsInitialized: Boolean;
  public
    constructor Create(const AParams: string); virtual; abstract;
    property    IsInitialized: boolean read FIsInitialized;
  end;


implementation

{ TfpgRect }

procedure TfpgRect.SetRect(aleft, atop, awidth, aheight: TfpgCoord);
begin
  Left   := aleft;
  Top    := atop;
  Width  := awidth;
  Height := aheight;
end;

function TfpgRect.Bottom: TfpgCoord;
begin
  Result := Top + Height - 1;
end;

function TfpgRect.Right: TfpgCoord;
begin
  Result := Left + Width - 1;
end;

procedure TfpgRect.SetBottom(Value: TfpgCoord);
begin
  Height := Value - Top + 1;
end;

procedure TfpgRect.SetRight(Value: TfpgCoord);
begin
  Width := Value - Left + 1;
end;

{ TfpgWindowBase }

procedure TfpgWindowBase.AdjustWindowStyle;
begin
  // does nothing here
end;

procedure TfpgWindowBase.SetWindowParameters;
begin
  // does nothing
end;

end.

