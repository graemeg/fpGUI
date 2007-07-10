{
    fpGUI  -  Free Pascal Graphical User Interface

    GFX_Carbon  -  Carbon (Mac OS X) target implementation

    Copyright (C) 2006 - 2007 See the file AUTHORS, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit gfx_carbon;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  { Pascal RTL Units }
  SysUtils, Classes,
  { Carbon headers }
  FPCMacOSAll,
  { fpGfx units }
  GfxBase; 

type

  ECarbonError = class(EGfxError);

  { TCarbonFont }

  TCarbonFont = class(TFCustomFont)
  public
    class function GetDefaultFontName(const AFontClass: TGfxFontClass): String; override;
    constructor Create(const Descriptor: String);
    destructor  Destroy; override;
  end;


  { TCarbonCanvas }

  TCarbonCanvas = class(TFCustomCanvas)
  private
  protected
    function    DoExcludeClipRect(const ARect: TRect): Boolean; override;
    function    DoIntersectClipRect(const ARect: TRect): Boolean; override;
    function    DoUnionClipRect(const ARect: TRect): Boolean; override;
    function    DoGetClipRect: TRect; override;
    procedure   DoDrawArc(const ARect: TRect; StartAngle, EndAngle: Single); override;
    procedure   DoDrawCircle(const ARect: TRect); override;
    procedure   DoDrawLine(const AFrom, ATo: TPoint); override;
    procedure   DoDrawPoint(const APoint: TPoint); override;
    procedure   DoFillRect(const ARect: TRect); override;
    procedure   DoTextOut(const APosition: TPoint; const AText: String); override;
    procedure   DoCopyRect(ASource: TFCustomCanvas; const ASourceRect: TRect; const ADestPos: TPoint); override;
    procedure   DoMaskedCopyRect(ASource, AMask: TFCustomCanvas; const ASourceRect: TRect; const AMaskPos, ADestPos: TPoint); override;
    procedure   DoDrawImageRect(AImage: TFCustomBitmap; ASourceRect: TRect; const ADestPos: TPoint); override;
  public
    constructor Create;
    destructor  Destroy; override;
    function    MapColor(const AColor: TGfxColor): TGfxPixel; override;
    function    FontCellHeight: Integer; override;
    function    TextExtent(const AText: String): TSize; override;
    procedure   SaveState; override;
    procedure   RestoreState; override;
    procedure   EmptyClipRect; override;
    procedure   DoSetColor(AColor: TGfxPixel); override;
    procedure   SetFont(AFont: TFCustomFont); override;
    procedure   SetLineStyle(ALineStyle: TGfxLineStyle); override;
  end;


  { TCarbonWindowCanvas }

  TCarbonWindowCanvas = class(TCarbonCanvas)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TCarbonBitmapCanvas }

  TCarbonBitmapCanvas = class(TCarbonCanvas)
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
  end;

  { TCarbonBitmap }

  TCarbonBitmap = class(TFCustomBitmap)
  private
    IsLocked: Boolean;
  public
    constructor Create(AWidth, AHeight: Integer; APixelFormat: TGfxPixelFormat); override;
    destructor Destroy; override;
    procedure Lock(out AData: Pointer; out AStride: LongWord); override;
    procedure Unlock; override;
  end;

  { TCarbonScreen }

  TCarbonScreen = class(TFCustomScreen)
  public
    constructor Create; override;
  end;


  { TCarbonApplication }

  TCarbonApplication = class(TFCustomApplication)
  private
  public
    { default methods }
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Initialize(ADisplayName: String = ''); override;
    procedure   Run; override;
    procedure   Quit; override;
  end;

  { TCarbonWindow }

  TCarbonWindow = class(TFCustomWindow)
  private
    FHandle: WindowRef;
    contentView: HIViewRef;
  protected
    function    GetTitle: String; override;
    procedure   SetTitle(const ATitle: String); override;
    procedure   DoSetCursor; override;
    function    GetHandle: PtrUInt; override;
  public
    constructor Create(AParent: TFCustomWindow; AWindowOptions: TFWindowOptions); override;
    destructor  Destroy; override;
    procedure   DefaultHandler(var Message); override;
    procedure   SetPosition(const APosition: TPoint); override;
    procedure   SetSize(const ASize: TSize); override;
    procedure   SetMinMaxSize(const AMinSize, AMaxSize: TSize); override;
    procedure   SetClientSize(const ASize: TSize); override;
    procedure   SetMinMaxClientSize(const AMinSize, AMaxSize: TSize); override;
    procedure   Show; override;
    procedure   Invalidate(const ARect: TRect); override;
    procedure   PaintInvalidRegion; override;
    procedure   CaptureMouse; override;
    procedure   ReleaseMouse; override;
  end;


implementation

{ TCarbonFont }

class function TCarbonFont.GetDefaultFontName(const AFontClass: TGfxFontClass): String;
begin
  Result:=inherited GetDefaultFontName(AFontClass);
end;

constructor TCarbonFont.Create(const Descriptor: String);
begin

end;

destructor TCarbonFont.Destroy;
begin
  inherited Destroy;
end;

{ TCarbonCanvas }

function TCarbonCanvas.DoExcludeClipRect(const ARect: TRect): Boolean;
begin

end;

function TCarbonCanvas.DoIntersectClipRect(const ARect: TRect): Boolean;
begin

end;

function TCarbonCanvas.DoUnionClipRect(const ARect: TRect): Boolean;
begin

end;

function TCarbonCanvas.DoGetClipRect: TRect;
begin

end;

procedure TCarbonCanvas.DoDrawArc(const ARect: TRect; StartAngle, EndAngle: Single);
begin

end;

procedure TCarbonCanvas.DoDrawCircle(const ARect: TRect);
begin

end;

procedure TCarbonCanvas.DoDrawLine(const AFrom, ATo: TPoint);
begin

end;

procedure TCarbonCanvas.DoDrawPoint(const APoint: TPoint);
begin

end;

procedure TCarbonCanvas.DoFillRect(const ARect: TRect);
begin

end;

procedure TCarbonCanvas.DoTextOut(const APosition: TPoint; const AText: String);
begin

end;

procedure TCarbonCanvas.DoCopyRect(ASource: TFCustomCanvas;
  const ASourceRect: TRect; const ADestPos: TPoint);
begin

end;

procedure TCarbonCanvas.DoMaskedCopyRect(ASource, AMask: TFCustomCanvas;
  const ASourceRect: TRect; const AMaskPos, ADestPos: TPoint);
begin

end;

procedure TCarbonCanvas.DoDrawImageRect(AImage: TFCustomBitmap;
  ASourceRect: TRect; const ADestPos: TPoint);
begin

end;

constructor TCarbonCanvas.Create;
begin
  inherited Create;
  
end;

destructor TCarbonCanvas.Destroy;
begin
  inherited Destroy;
end;

function TCarbonCanvas.MapColor(const AColor: TGfxColor): TGfxPixel;
begin

end;

function TCarbonCanvas.FontCellHeight: Integer;
begin

end;

function TCarbonCanvas.TextExtent(const AText: String): TSize;
begin

end;

procedure TCarbonCanvas.SaveState;
begin

end;

procedure TCarbonCanvas.RestoreState;
begin

end;

procedure TCarbonCanvas.EmptyClipRect;
begin

end;

procedure TCarbonCanvas.DoSetColor(AColor: TGfxPixel);
begin

end;

procedure TCarbonCanvas.SetFont(AFont: TFCustomFont);
begin

end;

procedure TCarbonCanvas.SetLineStyle(ALineStyle: TGfxLineStyle);
begin

end;

{ TCarbonWindowCanvas }

constructor TCarbonWindowCanvas.Create;
begin

end;

destructor TCarbonWindowCanvas.Destroy;
begin
  inherited Destroy;
end;

{ TCarbonBitmapCanvas }

constructor TCarbonBitmapCanvas.Create(AWidth, AHeight: Integer);
begin

end;

destructor TCarbonBitmapCanvas.Destroy;
begin
  inherited Destroy;
end;

{ TCarbonBitmap }

constructor TCarbonBitmap.Create(AWidth, AHeight: Integer;
  APixelFormat: TGfxPixelFormat);
begin
  inherited Create(AWidth, AHeight, APixelFormat);
end;

destructor TCarbonBitmap.Destroy;
begin
  inherited Destroy;
end;

procedure TCarbonBitmap.Lock(out AData: Pointer; out AStride: LongWord);
begin

end;

procedure TCarbonBitmap.Unlock;
begin

end;

{ TCarbonScreen }

constructor TCarbonScreen.Create;
begin
  inherited Create;
end;

{ TCarbonApplication }

constructor TCarbonApplication.Create;
begin
  inherited Create;
end;

destructor TCarbonApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TCarbonApplication.Initialize(ADisplayName: String);
begin

end;

procedure TCarbonApplication.Run;
begin
  RunApplicationEventLoop();
end;

procedure TCarbonApplication.Quit;
begin

end;

{ TCarbonWindow }

function WindowCommandHandler(nextHandler: EventHandlerCallRef; theEvent: EventRef; userDataPtr: UnivPtr): OSStatus;
var
  status: OSStatus;
  ignoreResult: OSStatus;
  aCommand: HICommand;
  theAssociatedControl: ControlRef;
begin
  status := eventNotHandledErr;

  ignoreResult := GetEventParameter(theEvent, kEventParamDirectObject, typeHICommand, NIL, sizeof(aCommand), NIL, @aCommand);

//  if aCommand.commandID = UInt32(FourCharCode(kButtonHello)) then status := ButtonHelloPressed()
//  else if aCommand.commandID = UInt32(FourCharCode(kButtonMessage)) then status := ButtonMessagePressed();

  Result := status;
end;

function TCarbonWindow.GetTitle: String;
begin

end;

procedure TCarbonWindow.SetTitle(const ATitle: String);
begin

end;

procedure TCarbonWindow.DoSetCursor;
begin

end;

function TCarbonWindow.GetHandle: PtrUInt;
begin
  Result := PtrUInt(FHandle);
end;

constructor TCarbonWindow.Create(AParent: TFCustomWindow; AWindowOptions: TFWindowOptions);
var
  status, ignoreResult: OSStatus;
  cmdEvent: EventTypeSpec;
  eventHandler: EventHandlerUPP;
  QDRect: FPCMacOSAll.Rect;
begin
  QDRect.left := 50;
  QDRect.Top := 50;
  QDRect.right := 300;
  QDRect.bottom := 300;

  status := CreateNewWindow(kDocumentWindowClass,
   (kWindowStandardDocumentAttributes or kWindowStandardHandlerAttribute
    or kWindowCompositingAttribute),
   QDRect, FHandle);

  if (status <> noErr) or (FHandle = nil) then
  begin
//    DoShowMessage('Error', 'CreateNewWindow failed');
  end;

  ignoreResult := SetWindowTitleWithCFString(FHandle, CFSTRP('Carbon FPC Hello World'));

  ignoreResult := HIViewFindByID(HIViewGetRoot(FHandle), kHIViewWindowContentID, contentView);

  { Add events }

  cmdEvent.eventClass := kEventClassCommand;
  cmdEvent.eventKind := kEventCommandProcess;
  eventHandler := NewEventHandlerUPP(@WindowCommandHandler);
  ignoreResult := InstallEventHandler(GetWindowEventTarget(FHandle),
   eventHandler, 1, @cmdEvent, nil, nil);

  { Creates a canvas }
  FCanvas := TCarbonCanvas.Create;
end;

destructor TCarbonWindow.Destroy;
begin

end;

procedure TCarbonWindow.DefaultHandler(var Message);
begin

end;

procedure TCarbonWindow.SetPosition(const APosition: TPoint);
begin

end;

procedure TCarbonWindow.SetSize(const ASize: TSize);
begin

end;

procedure TCarbonWindow.SetMinMaxSize(const AMinSize, AMaxSize: TSize);
begin

end;

procedure TCarbonWindow.SetClientSize(const ASize: TSize);
begin

end;

procedure TCarbonWindow.SetMinMaxClientSize(const AMinSize, AMaxSize: TSize);
begin

end;

procedure TCarbonWindow.Show;
begin
  ShowWindow(FHandle);
end;

procedure TCarbonWindow.Invalidate(const ARect: TRect);
begin

end;

procedure TCarbonWindow.PaintInvalidRegion;
begin

end;

procedure TCarbonWindow.CaptureMouse;
begin

end;

procedure TCarbonWindow.ReleaseMouse;
begin

end;

end.
