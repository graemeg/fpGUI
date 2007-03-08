{
    fpGUI  -  Free Pascal GUI Library
    
    fpGUI master file
    
    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      It is fully object-oriented; its main goal is to provide its services
      independent of any operating system or graphics environment. All painting
      is done using fpGFX, while fpGUI provides a comfortable widget set and
      other high-level classes.
}


unit fpGUI;

{.$DEFINE LAYOUTTRACES}
{.$DEFINE TRACEEVENTS}

{$IFDEF Debug}
  {$ASSERTIONS On}
{$ENDIF}
{$mode objfpc}{$h+}

interface

uses
{$IFDEF mswindows}
  Windows, GFX_GDI,       // This must be removed!!!
{$ENDIF}
  SysUtils
  ,Classes
  ,GFXBase
  ,fpGFX
//  ,Types
  ;

type
  TColor = type LongWord;

const

  InfiniteSize = 16383;

// Insert loads of named colors
{$I colors.inc}


resourcestring
  mbText_Yes        = 'Yes';
  mbText_No         = 'No';
  mbText_Ok         = 'Ok';
  mbText_Cancel     = 'Cancel';
  mbText_Apply      = 'Apply';
  mbText_Abort      = 'Abort';
  mbText_Retry      = 'Retry';
  mbText_Ignore     = 'Ignore';
  mbText_All        = 'All';
  mbText_NoToAll    = 'No to all';
  mbText_YesToAll   = 'Yes to all';
  mbText_Help       = 'Help';


type
  TWidget       = class;
  TEventObj     = class;
  TCustomForm   = class;


  TWidgetState = set of (
    wsEnabled,
    wsIsVisible,
    wsSizeIsForced,
    wsHasFocus,
    wsMouseInside,
    wsClicked
    );


  TOrientation = (Horizontal, Vertical);


  // The following flags are used for styles

  TButtonFlags = set of (btnIsEmbedded, btnIsDefault, btnIsPressed,
                         btnIsSelected, btnHasFocus);

  TCheckboxFlags = set of (cbIsPressed, cbHasFocus, cbIsEnabled, cbIsChecked);


  // Other stuff

  TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbApply, mbAbort, mbRetry,
                mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp);
  TMsgDlgButtons = set of TMsgDlgBtn;
  

  // Panel & Frame types

  TBevelStyle = (bsPlain, bsLowered, bsRaised);

  TBevelShape = (bsNoFrame, bsBox, bsFrame, bsBottomLine, bsLeftLine,
                  bsRightLine, bsTopLine);



{ This lets us use a single include file for both the Interface and
  Implementation sections. }
{$define read_interface}
{$undef read_implementation}


{$I style.inc}
{$I widget.inc}
{$I container.inc}
{$I bin.inc}
{$I layouts.inc}
{$I form.inc}
{$I popupwindow.inc}
{$I label.inc}
{$I edit.inc}
{$I buttons.inc}
{$I scrollbar.inc}
{$I scrollbox.inc}
{$I checkbox.inc}
{$I radiobutton.inc}
{$I separator.inc}
{$I groupbox.inc}
{$I listbox.inc}
{$I combobox.inc}
{$I grid.inc}
{$I dialogs.inc}
{$I panel.inc}
{$I menus.inc}
{$I progressbar.inc}


function Min(a, b: Integer): Integer; inline;
function Max(a, b: Integer): Integer; inline;
function ClipMinMax(val, min, max: Integer): Integer; inline;

{ This will change at a later date! }
procedure LoadForm(AForm: TComponent);


implementation
uses
  Math
  ,stylemanager
  ,MotifStyle       // so they register with style manager
  ,OpenSoftStyle    // so they register with style manager
  ,WindowsStyle     // so they register with style manager
  ;


resourcestring
  sListIndexError = 'List index exceeds bounds (%d)';


{$IFDEF TraceEvents}
var
  EventNestingLevel: Integer;
{$ENDIF}


function Min(a, b: Integer): Integer;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;


function Max(a, b: Integer): Integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;


function ClipMinMax(val, min, max: Integer): Integer;
begin
  if val < min then
    Result := min
  else if val > max then
  begin
    Result := max;
    if Result < min then
      Result := min;
  end else
    Result := val;
end;

procedure LoadForm(AForm: TComponent);
type
  PForm = ^TCustomForm;
var
  lForm: PForm;
  Filename: string;
  TextStream, BinStream: TStream;
begin
  Filename    := LowerCase(Copy(AForm.ClassName, 2, 255)) + '.frm';
  TextStream  := TFileStream.Create(Filename, fmOpenRead);
  BinStream   := TMemoryStream.Create;
  ObjectTextToBinary(TextStream, BinStream);
  TextStream.Free;

  lForm := @AForm;
  BinStream.Position := 0;
  BinStream.ReadComponent(lForm^);
  BinStream.Free;
end;

{$IFDEF LAYOUTTRACES}
procedure LAYOUTTRACE(const Position: String; const args: array of const);
{$IFDEF TraceEvents}
var
  i: Integer;
{$ENDIF}
begin
  {$IFDEF TraceEvents}
  for i := 1 to EventNestingLevel do
    Write('  ');
  {$ENDIF}
  WriteLn(Format(Position, args));
end;
{$ELSE}
procedure LAYOUTTRACE(const Position: String; const args: array of const);
begin
end;
{$ENDIF}

function InflateRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean;
begin
  if Assigned(@Rect) then
  begin
    with Rect do
    begin
      dec(Left, dx);
      dec(Top, dy);
      inc(Right, dx);
      inc(Bottom, dy);
    end;
    Result := True;
  end
  else
    Result := False;
end;

function OffsetRect(var Rect : TRect;DX : Integer;DY : Integer) : Boolean;
begin
  if assigned(@Rect) then
  begin
    with Rect do
    begin
      inc(Left,dx);
      inc(Top,dy);
      inc(Right,dx);
      inc(Bottom,dy);
    end;
    OffsetRect := True;
  end
  else
    OffsetRect := False;
end;

function CenterPoint(const Rect: TRect): TPoint;
begin
  with Rect do
  begin
    Result.X := (Left+Right) div 2;
    Result.Y := (Top+Bottom) div 2;
  end;
end;

{ This lets us use a single include file for both the Interface and
  Implementation sections. }
{$undef read_interface}
{$define read_implementation}


{$I style.inc}
{$I widget.inc}
{$I container.inc}
{$I bin.inc}
{$I layouts.inc}
{$I form.inc}
{$I popupwindow.inc}
{$I label.inc}
{$I edit.inc}
{$I buttons.inc}
{$I scrollbar.inc}
{$I scrollbox.inc}
{$I checkbox.inc}
{$I radiobutton.inc}
{$I separator.inc}
{$I groupbox.inc}
{$I listbox.inc}
{$I combobox.inc}
{$I grid.inc}
{$I dialogs.inc}
{$I panel.inc}
{$I menus.inc}
{$I progressbar.inc}


const
  Orientations: array[TOrientation] of TIdentMapEntry = (
      (Value: Ord(Horizontal); Name: 'Horizontal'),
      (Value: Ord(Vertical); Name: 'Vertical')
    );


function IdentToOrientation(const Ident: String; var Orientation: LongInt): Boolean;
begin
  Result := IdentToInt(Ident, Orientation, Orientations);
end;

function OrientationToIdent(Orientation: LongInt; var Ident: String): Boolean;
begin
  Result := IntToIdent(Orientation, Ident, Orientations);
end;


initialization
  RegisterIntegerConsts(TypeInfo(TOrientation),
    @IdentToOrientation, @OrientationToIdent);

end.

