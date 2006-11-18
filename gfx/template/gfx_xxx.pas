{
    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org
    Copyright (C) 2006 by Graeme Geldenhuys 
      member of the fpGFX development team.

    Template for new target implementations

    See the file COPYING.fpGFX, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit GFX_xxx;

interface

uses
  SysUtils, Classes,	// FPC units
                  	// xxx (insert target dependent units here)
  GfxBase;		// fpGUI units

type

  ExxxError = class(EGfxError);

  TxxxDrawable = class;
  TxxxDisplay = class;

  TxxxFont = class(TGfxFont)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  PxxxDrawableState = ^TxxxDrawableState;
  TxxxDrawableState = record
    Prev: PxxxDrawableState;
    Matrix: TGfxMatrix;
    // xxx Region data etc.
  end;

  TxxxDrawable = class(TGfxDrawable)
  private
    FDisplay: TxxxDisplay;
    FStateStackpointer: PxxxDrawableState;
    procedure Resized(NewWidth, NewHeight: Integer);
  public
    constructor Create(ADisplay: TxxxDisplay);	// xxx And other arguments optionally
    destructor Destroy; override;

    function CreateMemoryDrawable(AWidth, AHeight: Cardinal;
      const APixelFormat: TGfxPixelFormat;
      AStride: LongWord; AData: Pointer): TGfxDrawable; override;

    procedure SaveState; override;
    procedure RestoreState; override;
    function ExcludeClipRect(const ARect: TRect): Boolean; override;
    function IntersectClipRect(const ARect: TRect): Boolean; override;
    function GetClipRect: TRect; override;
    function MapColor(const AColor: TGfxColor): TGfxPixel; override;
    procedure SetColor(AColor: TGfxPixel); override;
    procedure SetFont(AFont: TGfxFont); override;
    procedure SetLineStyle(ALineStyle: TGfxLineStyle); override;

    procedure DrawArc(const Rect: TRect; StartAngle, EndAngle: Single); override;
    procedure DrawCircle(const Rect: TRect); override;
    procedure DrawLine(x1, y1, x2, y2: Integer); override;
    procedure FillRect(const Rect: TRect); override;
    function FontCellHeight: Integer; override;
    function TextWidth(const AText: String): Cardinal; override;
    procedure TextOut(x, y: Integer; const AText: String); override;

    procedure CopyRect(ASource: TGfxDrawable; const ASourceRect: TRect;
      ADestX, ADestY: Integer); override;


    property Display: TxxxDisplay read FDisplay;
  end;


  TxxxWindow = class;

  TxxxDisplay = class(TGfxDisplay)
  public
    destructor Destroy; override;
    function CreateFont(const Descriptor: String): TGfxFont; override;
    function CreateWindow: TGfxWindow; override;
    procedure Run; override;
  end;


  xxxSomeHandleType = Pointer;	// !!!: Remove this in your implementation

  TxxxWindow = class(TGfxWindow)
  private
    FHandle: xxxSomeHandleType;
  protected
    function GetTitle: String; override;
    procedure SetTitle(const ATitle: String); override;
  private
    constructor Create(ADisplay: TxxxDisplay);
  public
    destructor Destroy; override;

    procedure SetSize(AWidth, AHeight: Cardinal); override;
    procedure SetMinMaxSize(AMinWidth, AMinHeight,
      AMaxWidth, AMaxHeight: Cardinal); override;
    procedure Show; override;
    procedure Invalidate(const ARect: TRect); override;
    procedure CaptureMouse; override;
    procedure ReleaseMouse; override;

    property Handle: xxxSomeHandleType read FHandle;
  end;


// ===================================================================
// ===================================================================

implementation


// -------------------------------------------------------------------
//   TxxxFont
// -------------------------------------------------------------------

constructor TxxxFont.Create;
begin
  inherited Create;
  // !!!: Implement this
end;

destructor TxxxFont.Destroy;
begin
  // !!!: Implement this
  inherited Destroy;
end;


// -------------------------------------------------------------------
//   TxxxDrawable
// -------------------------------------------------------------------



// -------------------------------------------------------------------
//   TxxxDrawable
// -------------------------------------------------------------------

constructor TxxxDrawable.Create(ADisplay: TxxxDisplay);
begin
  inherited Create;
  FDisplay := ADisplay;
  // !!!: Create handle, init graphics state (line attributes, font etc.)
end;

destructor TxxxDrawable.Destroy;
begin
  // !!!: Implement this
  inherited Destroy;
end;

function TxxxDrawable.CreateMemoryDrawable(AWidth, AHeight: Cardinal;
  const APixelFormat: TGfxPixelFormat;
  AStride: LongWord; AData: Pointer): TGfxDrawable;
begin
  // !!!: Implement this
  raise EGfxError.Create(SUnsupportedPixelFormat);
  Result := nil;
end;

procedure TxxxDrawable.SaveState;
var
  SavedState: PxxxDrawableState;
begin
  New(SavedState);
  SavedState^.Prev := FStateStackpointer;
  SavedState^.Matrix := Matrix;
  // !!!: Save additional state informations
  FStateStackpointer := SavedState;
end;

procedure TxxxDrawable.RestoreState;
var
  SavedState: PxxxDrawableState;
begin
  SavedState := FStateStackpointer;
  FStateStackpointer := SavedState^.Prev;
  Matrix := SavedState^.Matrix;
  // !!!: Restore additional state informations
  Dispose(SavedState);
end;

function TxxxDrawable.ExcludeClipRect(const ARect: TRect): Boolean;
var
  x1, y1, x2, y2: Integer;
begin
  Transform(ARect.Left, ARect.Top, x1, y1);
  Transform(ARect.Right, ARect.Bottom, x2, y2);

  if (x2 > x1) and (y2 > y1) then
  begin
    // !!!: Implement this
    Result := True; // !!!: Return False if region is empty
  end else
    Result := False;
end;

function TxxxDrawable.IntersectClipRect(const ARect: TRect): Boolean;
var
  x1, y1, x2, y2: Integer;
begin
  Transform(ARect.Left, ARect.Top, x1, y1);
  Transform(ARect.Right, ARect.Bottom, x2, y2);

  if (x2 > x1) and (y2 > y1) then
  begin
    // !!!: Implement this
    Result := True; // !!!: Return False if region is empty
  end else
    Result := False;
end;

function TxxxDrawable.GetClipRect: TRect;
begin
  // !!!: Implement this
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := 0;
  Result.Bottom := 0;
end;

function TxxxDrawable.MapColor(const AColor: TGfxColor): TGfxPixel;
begin
  // !!!: Implement this
  Result := 0;
end;

procedure TxxxDrawable.SetColor(AColor: TGfxPixel);
begin
  // !!!: Implement this
end;

procedure TxxxDrawable.SetFont(AFont: TGfxFont);
begin
  // !!!: Implement this
end;

procedure TxxxDrawable.SetLineStyle(ALineStyle: TGfxLineStyle);
begin
  // !!!: Implement this
end;

procedure TxxxDrawable.DrawArc(const Rect: TRect; StartAngle, EndAngle: Single);
var
  x1, y1, x2, y2: Integer;
begin
  Transform(Rect.Left, Rect.Top, x1, y1);
  Transform(Rect.Right, Rect.Bottom, x2, y2);
  // !!!: Implement this
end;

procedure TxxxDrawable.DrawCircle(const Rect: TRect);
var
  x1, y1, x2, y2: Integer;
begin
  Transform(Rect.Left, Rect.Top, x1, y1);
  Transform(Rect.Right, Rect.Bottom, x2, y2);
  // !!!: Implement this
end;

procedure TxxxDrawable.DrawLine(x1, y1, x2, y2: Integer);
begin
  Transform(x1, y1, x1, y1);
  Transform(x2, y2, x2, y2);
  // !!!: Implement this
end;

procedure TxxxDrawable.FillRect(const Rect: TRect);
var
  r: TRect;
begin
  Transform(Rect.Left, Rect.Top, r.Left, r.Top);
  Transform(Rect.Right, Rect.Bottom, r.Right, r.Bottom);
  // !!!: Implement this
end;

function TxxxDrawable.FontCellHeight: Integer;
begin
  // !!!: Implement this
  Result := 16;
end;

function TxxxDrawable.TextWidth(const AText: String): Cardinal;
begin
  // !!!: Implement this
  Result := 16 * Length(AText);
end;

procedure TxxxDrawable.TextOut(x, y: Integer; const AText: String);
begin
  Transform(x, y, x, y);
  // !!!: Implement this
end;

procedure TxxxDrawable.CopyRect(ASource: TGfxDrawable; const ASourceRect: TRect;
  DestX, DestY: Integer);
begin
  Transform(DestX, DestY, DestX, DestY);
  // !!!: Implement this
end;

procedure TxxxDrawable.Resized(NewWidth, NewHeight: Integer);
begin
  FWidth := NewWidth;
  FHeight := NewHeight;
end;


// -------------------------------------------------------------------
//   TxxxDisplay
// -------------------------------------------------------------------

destructor TxxxDisplay.Destroy;
begin
  // !!!: Implement this
  inherited Destroy;
end;

function TxxxDisplay.CreateFont(const Descriptor: String): TGfxFont;
begin
  Result := TxxxFont.Create;
end;

function TxxxDisplay.CreateWindow: TGfxWindow;
begin
  Result := TxxxWindow.Create(Self);
  // !!!: Implement this
end;

procedure TxxxDisplay.Run;
begin
  // !!!: Implement this
end;


// -------------------------------------------------------------------
//   TxxxWindow
// -------------------------------------------------------------------

function TxxxWindow.GetTitle: String;
begin
  // !!!: Implement this
  Result := inherited;
end;

procedure TxxxWindow.SetTitle(const ATitle: String);
begin
  // !!!: Implement this
end;

constructor TxxxWindow.Create(ADisplay: TxxxDisplay);
begin
  inherited Create;
  FDisplay := ADisplay;
  // !!!: Implement this
  FDrawable := TxxxDrawable.Create(ADisplay); // !!!: Create a suitable drawable
end;

destructor TxxxWindow.Destroy;
begin
  if Assigned(OnClose) then
    OnClose(Self);

  Drawable.Free;

  // !!!: Clean up

  inherited Destroy;
end;

procedure TxxxWindow.SetSize(AWidth, AHeight: Cardinal);
begin
  // !!!: Implement this
end;

procedure TxxxWindow.SetMinMaxSize(AMinWidth, AMinHeight,
  AMaxWidth, AMaxHeight: Cardinal);
begin
  // !!!: Implement this
end;

procedure TxxxWindow.Show;
begin
  // !!!: Implement this
end;

procedure TxxxWindow.Invalidate(const ARect: TRect);
begin
  // !!!: Implement this
end;

procedure TxxxWindow.CaptureMouse;
begin
  // !!!: Implement this
end;

procedure TxxxWindow.ReleaseMouse;
begin
  // !!!: Implement this
end;


end.


{
  $Log: gfx_xxx.pp,v $
  Revision 1.3  2000/12/23 23:07:24  sg
  *** empty log message ***

  Revision 1.2  2000/10/28 20:28:27  sg
  * First version

  Revision 1.1  2000/08/04 21:05:53  sg
  * First version in CVS

}
