{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2008 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a Text Edit control. Also known a Text Entry control.
}

unit gui_edit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_widget,
  gui_menu;

type
  TfpgEditBorderStyle = (ebsNone, ebsDefault, ebsSingle);


  TfpgBaseEdit = class(TfpgWidget)
  private
    FAutoSelect: Boolean;
    FHideSelection: Boolean;
    FPopupMenu: TfpgPopupMenu;
    FDefaultPopupMenu: TfpgPopupMenu;
    FText: string;
    FFont: TfpgFont;
    FPasswordMode: Boolean;
    FBorderStyle: TfpgEditBorderStyle;
    FOnChange: TNotifyEvent;
    FMaxLength: integer;
    FSelecting: Boolean;
    procedure   Adjust(UsePxCursorPos: boolean = false);
    procedure   AdjustTextOffset(UsePxCursorPos: boolean);
    procedure   AdjustDrawingInfo;
    // function    PointToCharPos(x, y: integer): integer;
    procedure   DeleteSelection;
    procedure   DoCopy;
    procedure   DoPaste;
    procedure   SetAutoSelect(const AValue: Boolean);
    procedure   SetBorderStyle(const AValue: TfpgEditBorderStyle);
    procedure   SetHideSelection(const AValue: Boolean);
    procedure   SetPasswordMode(const AValue: boolean);
    function    GetFontDesc: string;
    procedure   SetFontDesc(const AValue: string);
    procedure   SetText(const AValue: string);
    procedure   DefaultPopupCut(Sender: TObject);
    procedure   DefaultPopupCopy(Sender: TObject);
    procedure   DefaultPopupPaste(Sender: TObject);
    procedure   DefaultPopupClearAll(Sender: TObject);
    procedure   SetDefaultPopupMenuItemsState;
  protected
    FSideMargin: integer;
    FMouseDragPos: integer;
    FSelStart: integer;
    FSelOffset: integer;
    FCursorPos: integer; // Caret position (characters)
    FCursorPx: integer;  // Caret position (pixels)
    FTextOffset: integer;
    FDrawOffset: integer;
    FVisibleText: TfpgString;
    FVisSelStartPx: integer;
    FVisSelEndPx: integer;
    procedure   DoOnChange; virtual;
    procedure   ShowDefaultPopupMenu(const x, y: integer; const shiftstate: TShiftState); virtual;
    procedure   HandlePaint; override;
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
    procedure   HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: Boolean); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: Boolean); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleRMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleDoubleClick(x, y: integer; button: word; shiftstate: TShiftState); override;
    procedure   HandleMouseEnter; override;
    procedure   HandleMouseExit; override;
    procedure   HandleSetFocus; override;
    procedure   HandleKillFocus; override;
    function    GetDrawText: String;
    property    AutoSelect: Boolean read FAutoSelect write SetAutoSelect default True;
    property    BorderStyle: TfpgEditBorderStyle read FBorderStyle write SetBorderStyle default ebsDefault;
    property    Font: TfpgFont read FFont;
    property    FontDesc: String read GetFontDesc write SetFontDesc;
    property    HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property    MaxLength: Integer read FMaxLength write FMaxLength;
    property    PasswordMode: Boolean read FPasswordMode write SetPasswordMode default False;
    property    PopupMenu: TfpgPopupMenu read FPopupMenu write FPopupMenu;
    property    Text: String read FText write SetText;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    SelectionText: string;
    procedure   SelectAll;
    procedure   Clear;
    procedure   ClearSelection;
    procedure   CopyToClipboard;
    procedure   CutToClipboard;
    procedure   PasteFromClipboard;
  end;


  TfpgEdit = class(TfpgBaseEdit)
  public
    property    Font;
    property    PopupMenu;  // UI Designer doesn't fully support it yet
  published
    property    AutoSelect;
    property    BackgroundColor default clBoxColor;
    property    BorderStyle;
    property    FontDesc;
    property    HideSelection;
    property    MaxLength;
    property    PasswordMode;
    property    TabOrder;
    property    Text;
    property    TextColor;
    property    OnChange;
    property    OnEnter;
    property    OnExit;
    property    OnKeyPress;
    property    OnMouseEnter;
    property    OnMouseExit;
    property    OnPaint;
  end;


  TfpgBaseNumericEdit = class(TfpgBaseEdit)
  private
    fOldColor: TfpgColor;
    fAlignment: TAlignment;
    fDecimalSeparator: char;
    fNegativeColor: TfpgColor;
    fThousandSeparator: char;
    fShowThousand: boolean;
    procedure   SetOldColor(const AValue: TfpgColor);
    procedure   SetAlignment(const AValue: TAlignment);
    procedure   SetDecimalSeparator(const AValue: char);
    procedure   SetNegativeColor(const AValue: TfpgColor);
    procedure   SetThousandSeparator(const AValue: char);
  protected
    procedure   HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: Boolean); override;
    procedure   HandlePaint; override;
    procedure   Format; virtual;
    procedure   Justify; virtual; // to implement in derived classes
    property    OldColor: TfpgColor read fOldColor write SetOldColor;
    property    Alignment: TAlignment read fAlignment write SetAlignment default taRightJustify;
    property    AutoSelect;
    property    BackgroundColor default clBoxColor;
    property    BorderStyle;
    {Someone likes to use English operating system but localized decimal and thousand separators
     Still to implement !!}
    property    DecimalSeparator: char read fDecimalSeparator write SetDecimalSeparator;
    property    ThousandSeparator: char read fThousandSeparator write SetThousandSeparator;
    property    NegativeColor: TfpgColor read fNegativeColor write SetNegativeColor;
    property    HideSelection;
//    property    MaxLength;  { probably MaxValue and MinValue }
    property    TabOrder;
    property    TextColor;
    property    ShowThousand: boolean read fShowThousand write fShowThousand default False;
    property    OnChange;
    property    OnEnter;
    property    OnExit;
    property    OnKeyPress;
    property    OnMouseEnter;
    property    OnMouseExit;
    property    OnPaint;
    property    Text;   { this should become Value }
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    FontDesc;
  end;


  TfpgEditInteger = class(TfpgBaseNumericEdit)
  protected
    function    GetValue: integer; virtual;
    procedure   SetValue(const AValue: integer); virtual;
    procedure   SetShowThousand;
    procedure   Format; override;
    procedure   HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
     property   Text;
  published
    property    Alignment;
    property    NegativeColor;
    property    Value: integer read GetValue write SetValue;
    property    ShowThousand;
    property    TabOrder;
    property    TextColor;
    property    ThousandSeparator;
    property    OnChange;
    property    OnEnter;
    property    OnExit;
    property    OnKeyPress;
    property    OnMouseEnter;
    property    OnMouseExit;
  end;


  TfpgEditFloat = class(TfpgBaseNumericEdit)
  private
    fDecimals: integer;
  protected
    function    GetValue: extended; virtual;
    procedure   SetValue(const AValue: extended); virtual;
    procedure   SetShowThousand;
    procedure   SetDecimals(AValue: integer);
    procedure   Format; override;
    procedure   HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
     property   Text;
  published
    property    Alignment;
    property    Decimals: integer read fDecimals write SetDecimals;
    property    NegativeColor;
    property    DecimalSeparator;
    property    Value: extended read GetValue write SetValue;
    property    ShowThousand;
    property    TabOrder;
    property    TextColor;
    property    ThousandSeparator;
    property    OnChange;
    property    OnEnter;
    property    OnExit;
    property    OnKeyPress;
    property    OnMouseEnter;
    property    OnMouseExit;
  end;


  TfpgEditCurrency = class(TfpgBaseNumericEdit)
  private
    fDecimals: integer;
  protected
    function    GetValue: Currency; virtual;
    procedure   SetValue(const AValue: Currency); virtual;
    procedure   SetShowThousand;
    procedure   SetDecimals(AValue: integer);
    procedure   Format; override;
    procedure   HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: Boolean); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    property    Text;
  published
    property    Alignment;
    property    Decimals: integer read fDecimals write SetDecimals;
    property    NegativeColor;
    property    OldColor;
    property    DecimalSeparator;
    property    ThousandSeparator;
    property    ShowThousand;
    property    Value: Currency read GetValue write SetValue;
    property    OnChange;
    property    OnEnter;
    property    OnExit;
    property    OnKeyPress;
    property    OnMouseEnter;
    property    OnMouseExit;
  end;


function CreateEdit(AOwner: TComponent; x, y, w, h: TfpgCoord): TfpgEdit;

function CreateEditInteger(AOwner: TComponent; x, y, w, h: TfpgCoord;
    AShowThousand: boolean= True): TfpgEditInteger;

function CreateEditFloat(AOwner: TComponent; x, y, w, h: TfpgCoord;
    AShowThousand: boolean= True; ADecimals: Integer= -1): TfpgEditFloat;

function CreateEditCurrency(AOwner: TComponent; x, y, w, h: TfpgCoord;
    AShowThousand: boolean= True; ADecimals: Integer= 2): TfpgEditCurrency;


implementation

uses
  gfx_UTF8utils,
  gfx_constants;

const
  // internal popupmenu item names
  ipmCut        = 'miDefaultCut';
  ipmCopy       = 'miDefaultCopy';
  ipmPaste      = 'miDefaultPaste';
  ipmClearAll   = 'miDefaultClearAll';


function CreateEdit(AOwner: TComponent; x, y, w, h: TfpgCoord): TfpgEdit;
begin
  Result       := TfpgEdit.Create(AOwner);
  Result.Left  := x;
  Result.Top   := y;
  if w > 0 then
    Result.Width := w;
  if h < TfpgEdit(Result).FFont.Height + 6 then
    Result.Height:= TfpgEdit(Result).FFont.Height + 6
  else
    Result.Height:= h;
end;

function CreateEditInteger(AOwner: TComponent; x, y, w, h: TfpgCoord; AShowThousand: boolean= True): TfpgEditInteger;
begin
  Result       := TfpgEditInteger.Create(AOwner);
  Result.Left  := x;
  Result.Top   := y;
  Result.Width := w;
  Result.ShowThousand:= AShowThousand;
  if h < TfpgEditInteger(Result).FFont.Height + 6 then
    Result.Height:= TfpgEditInteger(Result).FFont.Height + 6
  else
    Result.Height:= h;
end;

function CreateEditFloat(AOwner: TComponent; x, y, w, h: TfpgCoord; AShowThousand: boolean= True;
         ADecimals: Integer= -1): TfpgEditFloat;
begin
  Result       := TfpgEditFloat.Create(AOwner);
  Result.Left  := x;
  Result.Top   := y;
  Result.Width := w;
  Result.ShowThousand:= AShowThousand;
  Result.Decimals := ADecimals;
  if h < TfpgEditFloat(Result).FFont.Height + 6 then
    Result.Height:= TfpgEditFloat(Result).FFont.Height + 6
  else
    Result.Height:= h;
end;

function CreateEditCurrency(AOwner: TComponent; x, y, w, h: TfpgCoord; AShowThousand: boolean= True;
         ADecimals: Integer= 2): TfpgEditCurrency;
begin
  Result          := TfpgEditCurrency.Create(AOwner);
  Result.Left     := x;
  Result.Top      := y;
  Result.Width    := w;
  Result.ShowThousand:= AShowThousand;
  Result.Decimals := ADecimals;
  if h < TfpgEditCurrency(Result).FFont.Height + 6 then
    Result.Height:= TfpgEditCurrency(Result).FFont.Height + 6
  else
    Result.Height:= h;
end;


{ TfpgBaseEdit }

procedure TfpgBaseEdit.Adjust(UsePxCursorPos: boolean = false);
begin
  AdjustTextOffset(False);
  AdjustDrawingInfo;
end;

procedure TfpgBaseEdit.AdjustTextOffset(UsePxCursorPos: boolean);
{If UsePxCursorPos then determines FCursorPos from FCursorPx (that holds mouse pointer coordinates)
 Calculates exact FCursorPx (relative to the widget bounding box) from FCursorPos
 Calculates FTextOffset based on FCursorPx}
var
  dtext: string;
  ch: string;     // current character
  chnum: integer; // its ordinal number
  chx: integer;   // its X position relative to widget
  bestchx: integer; // chx, nearest to the mouse position (indicated by FCursorPx if UsePxCursorPos = True)
  tw: integer;      // total characters width, that becomes FCursorPx relative to the beginning of the text
  ptw: integer;
  dpos: integer;  // helps to pass through an utf-8 string quickly
  VisibleWidth: integer; // width of the edit field minus side margins
begin
  if UsePxCursorPos then
  begin
    if FCursorPx > 0 then // bestchx < chx minimum
      bestchx := Low(chx)  + 1 + FCursorPx
    else                  // bestchx > chx maximum
      bestchx := High(chx) - 1 + FCursorPx;
  end else
    FCursorPx := 0;

  dtext := GetDrawText;
  ch    := '';
  chnum := 0;
  tw    := 0;
  dpos  := 0;

  while dpos <= Length(dtext) do
  begin
    dpos := UTF8CharAtByte(dtext, dpos, ch);
    ptw := tw;
    tw  := tw + FFont.TextWidth(ch);
    chx := tw - FTextOffset + FSideMargin;
    if UsePxCursorPos then
    begin
      if abs(chx - FCursorPx) < abs(bestchx - FCursorPx) then
      begin
        bestchx := chx;
        FCursorPos := chnum;
      end else
      begin
        tw := ptw;
        break;
      end;
    end else
    begin
      if chnum >= FCursorPos then
        break;
    end;
    Inc(chnum);
  end;

  VisibleWidth := (FWidth - 2 * FSideMargin);
  if tw - FTextOffset > VisibleWidth - 2 then
    FTextOffset := tw - VisibleWidth + 2
  else if tw - FTextOffset < 0 then
  begin
    FTextOffset := tw;
    if tw <> 0 then
      Dec(FTextOffset, 2);
  end;

  FCursorPx := tw - FTextOffset + FSideMargin;
end;

procedure TfpgBaseEdit.AdjustDrawingInfo;
// Calculates FVisSelStartPx, FVisSelEndPx, FVisibleText, FDrawOffset
var
  // fvc, lvc: integer; // first/last visible characters
  vtstartbyte, vtendbyte: integer; // visible characters' start/end in utf-8 string, bytes
  bestfx, bestlx: integer;
  dtext: string;
  ch: string;     // current character
  chnum: integer; // its ordinal number
  chx: integer;   // its X position relative to widget
  tw: integer;    // total characters width, that becomes FCursorPx relative to the beginning of the text
  ptw: integer;   // total width on the previous step
  dpos: integer;  // helps to pass through an utf-8 string quickly
  pdp: integer;   // dpos on the previous step
  vstart, vend: integer;    // visible area start and end, pixels
  slstart, slend: integer;  // selection start and end, pixels
begin
  vstart  := FSideMargin;
  vend    := FWidth - FSideMargin;
  if FSelOffset > 0 then
  begin
    slstart := FSelStart;
    slend   := FSelStart + FSelOffset;
  end else
  begin
    slstart := FSelStart + FSelOffset;
    slend   := FSelStart;
  end;
  FVisSelStartPx := vend; // because we stop the search
  FVisSelEndPx   := vend; // after last visible character is found
  bestfx := High(chx) - 1 + vstart;
  bestlx := Low(chx)  + 1 + vend;

  dtext := GetDrawText;
  ch    := '';
  chnum := 0;
  tw    := 0;
  dpos  := 0;
  {fvc   := 0;
  lvc   := 0;}
  FDrawOffset := 0;
  while dpos <= Length(dtext) do
  begin
    pdp := dpos;
    dpos := UTF8CharAtByte(dtext, dpos, ch);
    ptw := tw;
    tw  := tw + FFont.TextWidth(ch);
    chx := tw - FTextOffset + FSideMargin;

    // calculate selection-related fields
    if chnum = slstart then
      FVisSelStartPx := chx;
    if chnum = slend then
      FVisSelEndPx := chx;

    // search for the first/last visible characters
    if abs(chx - vstart) < abs(bestfx - vstart) then
    begin
      bestfx := chx;
      // fvc    := chnum;
      vtstartbyte := pdp;
      FDrawOffset := ptw;
    end;
    // in small edit field the same character can be both the first and the last, so no 'else' allowed
    if abs(chx - vend) < abs(bestlx - vend) then
    begin
      bestlx := chx;
      // lvc    := chnum;
      vtendbyte := UTF8CharAtByte(dtext, dpos, ch); // plus one more character
    end else
      break; // we can safely break after last visible character is found
    Inc(chnum);
  end;

  if FVisSelStartPx < vstart then
    FVisSelStartPx := vstart;
  if FVisSelEndPx > vend then
    FVisSelEndPx := vend;

  // FVisibleText := UTF8Copy(dtext, fvc, lvc - fvc + 2);
  FVisibleText := Copy(dtext, vtstartbyte, vtendbyte - vtstartbyte);
  FDrawOffset := FTextOffset - FDrawOffset;
end;

{function TfpgBaseEdit.PointToCharPos(x, y: integer): integer;
var
  n: integer;
  cx: integer; // character X position
  bestcx: integer;
  dtext: string;
  tw, dpos: integer;
  ch: string;
begin
  ch     := '';
  dtext  := GetDrawText;
  if x > 0 then // bestcx < cx minimum
    bestcx := Low(cx) + 1 + x
  else          // bestcx > cx maximum
    bestcx := High(cx) - 1 + x;
    
  tw   := 0;
  dpos := 0;
  n    := 0;
  Result := n;
  // searching the appropriate character position
  while dpos <= Length(dtext) do
  begin
    dpos := UTF8CharAtByte(dtext, dpos, ch);
    tw := tw + FFont.TextWidth(ch);
    cx := tw - FTextOffset + FSideMargin;
    if abs(cx - x) < abs(bestcx - x) then
    begin
      bestcx := cx;
      Result := n;
    end else
      Exit; //==>
    Inc(n);
  end;
end;}

procedure TfpgBaseEdit.SetBorderStyle(const AValue: TfpgEditBorderStyle);
begin
  if FBorderStyle = AValue then
    Exit; //==>
  FBorderStyle := AValue;
  RePaint;
end;

procedure TfpgBaseEdit.SetHideSelection(const AValue: Boolean);
begin
  if FHideSelection = AValue then
    Exit;
  FHideSelection := AValue;
end;

procedure TfpgBaseEdit.HandlePaint;
var
  r: TfpgRect;

  // paint selection rectangle
  procedure DrawSelection;
  var
    lcolor: TfpgColor;
    r: TfpgRect;
  begin
    if Focused then
    begin
      lcolor := clSelection;
      Canvas.SetTextColor(clSelectionText);
    end
    else
    begin
      lcolor := clInactiveSel;
      Canvas.SetTextColor(clText1);
    end;

    r.SetRect(FVisSelStartPx, 3, FVisSelEndPx - FVisSelStartPx, FFont.Height);
    Canvas.SetColor(lcolor);
    Canvas.FillRectangle(r);
    Canvas.SetTextColor(clWhite);
    Canvas.AddClipRect(r);
    fpgStyle.DrawString(Canvas, -FDrawOffset + FSideMargin, 3, FVisibleText, Enabled);
    Canvas.ClearClipRect;
  end;
  
begin
  Canvas.ClearClipRect;
  r.SetRect(0, 0, Width, Height);
  case BorderStyle of
    ebsNone:
        begin
          // do nothing
        end;
    ebsDefault:
        begin
          Canvas.DrawControlFrame(r);
          InflateRect(r, -2, -2);
        end;
    ebsSingle:
        begin
          Canvas.SetColor(clShadow2);
          Canvas.DrawRectangle(r);
          InflateRect(r, -1, -1);
        end;
  end;
  Canvas.SetClipRect(r);

  if Enabled then
    Canvas.SetColor(FBackgroundColor)
  else
    Canvas.SetColor(clWindowBackground);

  Canvas.FillRectangle(r);

  Canvas.SetFont(FFont);
  Canvas.SetTextColor(FTextColor);
  fpgStyle.DrawString(Canvas, -FDrawOffset + FSideMargin, 3, FVisibleText, Enabled);

  if Focused then
  begin
    // drawing selection
    if FSelOffset <> 0 then
      DrawSelection;

    // drawing cursor
    fpgCaret.SetCaret(Canvas, FCursorPx, 3, fpgCaret.Width, FFont.Height);
  end
  else
  begin
    // drawing selection
    if (AutoSelect = False) and (FSelOffset <> 0) and (HideSelection = False) then
      DrawSelection;
    fpgCaret.UnSetCaret(Canvas);
  end;
end;

procedure TfpgBaseEdit.HandleResize(awidth, aheight: TfpgCoord);
begin
  inherited HandleResize(awidth, aheight);
  AdjustDrawingInfo;
end;

procedure TfpgBaseEdit.HandleKeyChar(var AText: TfpgChar;
  var shiftstate: TShiftState; var consumed: Boolean);
var
  s: TfpgChar;
  prevval: string;
begin
  prevval   := Text;
  s         := AText;

  if not consumed then
  begin
    // Handle only printable characters
    // UTF-8 characters beyond ANSI range are supposed to be printable
    if ((Ord(AText[1]) > 31) and (Ord(AText[1]) < 127)) or (Length(AText) > 1) then
    begin
      if (FMaxLength <= 0) or (UTF8Length(FText) < FMaxLength) then
      begin
        DeleteSelection;
        UTF8Insert(s, FText, FCursorPos + 1);
        Inc(FCursorPos);
        FSelStart := FCursorPos;
        Adjust;
      end;
      consumed := True;
    end;

    if prevval <> Text then
      DoOnChange;
  end;
  
  if consumed then
    RePaint;

  inherited HandleKeyChar(AText, shiftstate, consumed);
end;

procedure TfpgBaseEdit.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  hasChanged: boolean;

  procedure StopSelection;
  begin
    FSelStart  := FCursorPos;
    FSelOffset := 0;
  end;

begin
  hasChanged := False;

  Consumed := True;
  case CheckClipBoardKey(keycode, shiftstate) of
    ckCopy:
        begin
          DoCopy;
        end;
    ckPaste:
        begin
          DoPaste;
          hasChanged := True;
        end;
    ckCut:
        begin
          DoCopy;
          DeleteSelection;
          Adjust;
          hasChanged := True;
        end;
  else
    Consumed := False;
  end;


  if not Consumed then
  begin
    // checking for movement keys:
    case keycode of
      keyLeft:
        if FCursorPos > 0 then
        begin
          consumed := True;
          Dec(FCursorPos);

          if (ssCtrl in shiftstate) then
            // word search...
            //                    while (FCursorPos > 0) and not ptkIsAlphaNum(copy(FText,FCursorPos,1))
            //                      do Dec(FCursorPos);
            //                    while (FCursorPos > 0) and ptkIsAlphaNum(copy(FText,FCursorPos,1))
            //                      do Dec(FCursorPos);
          ;

        end;

      keyRight:
        if FCursorPos < UTF8Length(FText) then
        begin
          consumed := True;
          Inc(FCursorPos);

          if (ssCtrl in shiftstate) then
            // word search...
            //                    while (FCursorPos < Length(FText)) and ptkIsAlphaNum(copy(FText,FCursorPos+1,1))
            //                      do Inc(FCursorPos);
            //                    while (FCursorPos < Length(FText)) and not ptkIsAlphaNum(copy(FText,FCursorPos+1,1))
            //                      do Inc(FCursorPos);
          ;
        end;

      keyHome:
        begin
          consumed := True;
          FCursorPos := 0;
        end;

      keyEnd:
        begin
          consumed := True;
          FCursorPos := UTF8Length(FText);
        end;
    end;

    if Consumed then
    begin
      FSelecting := (ssShift in shiftstate);

      if FSelecting then
        FSelOffset := FCursorPos - FSelStart
      else
        StopSelection;
        
      Adjust;
    end;
  end; // movement key checking

  if not Consumed then
  begin
    consumed := True;

    case keycode of
      keyBackSpace:
          begin
            if FCursorPos > 0 then
            begin
              UTF8Delete(FText, FCursorPos, 1);
              Dec(FCursorPos);
              hasChanged := True;
            end;// backspace
          end;


      keyDelete:
          begin
            if FSelOffset <> 0 then
              DeleteSelection
            else if FCursorPos < UTF8Length(FText) then
              UTF8Delete(FText, FCursorPos + 1, 1);
            hasChanged := True;
          end;
      else
        Consumed := False;
    end;

    if Consumed then
    begin
      StopSelection;
      Adjust;
    end;
  end;  { if }

  if consumed then
    RePaint
  else
    inherited;

  if hasChanged then
    if Assigned(FOnChange) then
      FOnChange(self);
end;

procedure TfpgBaseEdit.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
{var
  cp: integer;}
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  {cp := PointToCharPos(x, y);
  FMouseDragPos := cp;
  FCursorPos    := cp;
  if (ssShift in shiftstate) then
    FSelOffset := FCursorPos - FSelStart
  else
  begin
    FSelStart  := cp;
    FSelOffset := 0;
  end;
  Adjust;
  Repaint;}
  
  FCursorPx := x;
  AdjustTextOffset(True);
  FMouseDragPos := FCursorPos;
  if (ssShift in shiftstate) then
    FSelOffset := FCursorPos - FSelStart
  else
  begin
    FSelStart  := FCursorPos;
    FSelOffset := 0;
  end;
  AdjustDrawingInfo;
  RePaint;
end;

procedure TfpgBaseEdit.HandleRMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleRMouseUp(x, y, shiftstate);
  if Assigned(PopupMenu) then
    PopupMenu.ShowAt(self, x, y)
  else
    ShowDefaultPopupMenu(x, y, ShiftState);
end;

procedure TfpgBaseEdit.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  cp: integer;
begin
  if (btnstate and MOUSE_LEFT) = 0 then
    Exit;

  {cp := PointToCharPos(x, y);

  //FMouseDragPos := cp;
  FSelOffset := cp - FSelStart;
  if FCursorPos <> cp then
  begin
    FCursorPos := cp;
    Adjust;
    Repaint;
  end;}
  
  cp := FCursorPos;
  FCursorPx := x;
  AdjustTextOffset(True);
  if FCursorPos <> cp then
  begin
    FSelOffset := FCursorPos - FSelStart;
    AdjustDrawingInfo;
    Repaint;
  end;
end;

procedure TfpgBaseEdit.HandleDoubleClick(x, y: integer; button: word; shiftstate: TShiftState);
begin
  // button is always Mouse_Left, but lets leave this test here for good measure
  if button = MOUSE_LEFT then
    SelectAll
  else
    inherited;
end;

procedure TfpgBaseEdit.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  if (csDesigning in ComponentState) then
    Exit;
  if Enabled then
    MouseCursor := mcIBeam;
end;

procedure TfpgBaseEdit.HandleMouseExit;
begin
  inherited HandleMouseExit;
  if (csDesigning in ComponentState) then
    Exit;
  MouseCursor := mcDefault;
end;

procedure TfpgBaseEdit.HandleSetFocus;
begin
  inherited HandleSetFocus;
  if AutoSelect then
    SelectAll;
end;

procedure TfpgBaseEdit.HandleKillFocus;
begin
  inherited HandleKillFocus;
  if AutoSelect then
    FSelOffset := 0;
end;

function TfpgBaseEdit.GetDrawText: string;
begin
  if not PassWordMode then
    Result := FText
  else
    Result := StringOfChar('*', UTF8Length(FText));
end;

constructor TfpgBaseEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont             := fpgGetFont('#Edit1');  // owned object !
  Focusable         := True;
  FHeight           := FFont.Height + 6;
  FWidth            := 120;
  FTextColor        := Parent.TextColor;
  FBackgroundColor  := clBoxColor;
  FAutoSelect       := True;
  FSelecting        := False;
  FHideSelection    := True;
  FSideMargin       := 3;
  FMaxLength        := 0; // no limit
  FText             := '';
  FCursorPos        := UTF8Length(FText);
  FSelStart         := FCursorPos;
  FSelOffset        := 0;
  FTextOffset       := 0;
  FPasswordMode     := False;
  FBorderStyle      := ebsDefault;
  FPopupMenu        := nil;
  FDefaultPopupMenu := nil;
  FOnChange         := nil;
end;

destructor TfpgBaseEdit.Destroy;
begin
  if Assigned(FDefaultPopupMenu) then
    FDefaultPopupMenu.Free;
  FFont.Free;
  inherited Destroy;
end;

function TfpgBaseEdit.SelectionText: string;
begin
  if FSelOffset <> 0 then
  begin
    if FSelOffset < 0 then
      Result := UTF8Copy(FText, 1 + FSelStart + FSelOffset, -FSelOffset)
    else
    begin
      Result := UTF8Copy(FText, 1 + FSelStart, FSelOffset);
    end;
  end
  else
    Result := '';
end;

procedure TfpgBaseEdit.SetPasswordMode (const AValue: boolean );
begin
  if FPasswordMode = AValue then
    Exit; //==>
  FPasswordMode := AValue;
  Adjust;
  RePaint;
end;

function TfpgBaseEdit.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TfpgBaseEdit.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  if Height < FFont.Height + 6 then
    Height:= FFont.Height + 6;
  Adjust;
  RePaint;
end;

procedure TfpgBaseEdit.SetText(const AValue: string);
var
  s: string;
begin
  if FText = AValue then
    Exit;

  if FMaxLength <> 0 then
  begin
    if UTF8Length(FText) > FMaxLength then
      s := UTF8Copy(AValue, 1, FMaxLength)
    else
      s := AValue;
  end
  else
    s := AValue;

  FText       := s;
  FCursorPos  := UTF8Length(FText);
  FSelStart   := FCursorPos;
  FSelOffset  := 0;
  FTextOffset := 0;

  Adjust;
  RePaint;
end;

procedure TfpgBaseEdit.DefaultPopupCut(Sender: TObject);
begin
  CutToClipboard;
end;

procedure TfpgBaseEdit.DefaultPopupCopy(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TfpgBaseEdit.DefaultPopupPaste(Sender: TObject);
begin
  PasteFromClipboard
end;

procedure TfpgBaseEdit.DefaultPopupClearAll(Sender: TObject);
begin
  Clear;
end;

procedure TfpgBaseEdit.SetDefaultPopupMenuItemsState;
var
  i: integer;
  itm: TfpgMenuItem;
begin
  for i := 0 to FDefaultPopupMenu.ComponentCount-1 do
  begin
    if FDefaultPopupMenu.Components[i] is TfpgMenuItem then
    begin
      itm := TfpgMenuItem(FDefaultPopupMenu.Components[i]);
      // enabled/disable menu items
      if itm.Name = ipmCut then
        itm.Enabled := FSelOffset <> 0
      else if itm.Name = ipmCopy then
        itm.Enabled := FSelOffset <> 0
      else if itm.Name = ipmPaste then
        itm.Enabled := fpgClipboard.Text <> ''
      else if itm.Name = ipmClearAll then
        itm.Enabled := Text <> '';
    end;
  end;
end;

procedure TfpgBaseEdit.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TfpgBaseEdit.ShowDefaultPopupMenu(const x, y: integer;
  const shiftstate: TShiftState);
var
  itm: TfpgMenuItem;
begin
  if not Assigned(FDefaultPopupMenu) then
  begin
    { todo: This text needs to be localized }
    FDefaultPopupMenu := TfpgPopupMenu.Create(nil);
    itm := FDefaultPopupMenu.AddMenuItem(rsCut, '', @DefaultPopupCut);
    itm.Name := ipmCut;
    itm := FDefaultPopupMenu.AddMenuItem(rsCopy, '', @DefaultPopupCopy);
    itm.Name := ipmCopy;
    itm := FDefaultPopupMenu.AddMenuItem(rsPaste, '', @DefaultPopupPaste);
    itm.Name := ipmPaste;
    itm := FDefaultPopupMenu.AddMenuItem(rsDelete, '', @DefaultPopupClearAll);
    itm.Name := ipmClearAll;
  end;
  
  SetDefaultPopupMenuItemsState;
  FDefaultPopupMenu.ShowAt(self, x, y);
end;

procedure TfpgBaseEdit.DeleteSelection;
begin
  if FSelOffset <> 0 then
  begin
    if FSelOffset < 0 then
    begin
      UTF8Delete(FText, 1 + FSelStart + FSelOffset, -FSelOffset);
      FCurSorPos := FSelStart + FSelOffset;
    end
    else
    begin
      UTF8Delete(FText, 1 + FSelStart, FSelOffset);
      FCurSorPos := FSelStart;
    end;
    FSelOffset := 0;
    FSelStart := FCursorPos;
  end;
end;

procedure TfpgBaseEdit.DoCopy;
begin
  if FSelOffset = 0 then
    Exit; //==>
  fpgClipboard.Text := SelectionText;
end;

procedure TfpgBaseEdit.DoPaste;
var
  s: string;
begin
  DeleteSelection;
  s := fpgClipboard.Text;

  if (FMaxLength > 0) then
    if UTF8Length(FText) + UTF8Length(s) > FMaxLength then
      s := UTF8Copy(s, 1, FMaxLength - UTF8Length(FText));  // trim the clipboard text if needed

  if UTF8Length(s) < 1 then
    Exit; //==>

  UTF8Insert(s, FText, FCursorPos + 1);
  FCursorPos := FCursorPos + UTF8Length(s);
  FSelStart  := FCursorPos;
  Adjust;
  Repaint;
end;

procedure TfpgBaseEdit.SetAutoSelect(const AValue: Boolean);
begin
  if FAutoSelect = AValue then
    Exit; //==>
  FAutoSelect := AValue;
end;

procedure TfpgBaseEdit.SelectAll;
begin
  FSelecting  := True;
  FSelStart   := 0;
  FSelOffset  := UTF8Length(FText);
  FCursorPos  := FSelOffset;
  Adjust;
  Repaint;
end;

procedure TfpgBaseEdit.Clear;
begin
  Text := '';
end;

procedure TfpgBaseEdit.ClearSelection;
begin
  DeleteSelection;
  Adjust;
  RePaint;
end;

procedure TfpgBaseEdit.CopyToClipboard;
begin
  DoCopy;
end;

procedure TfpgBaseEdit.CutToClipboard;
begin
  DoCopy;
  DeleteSelection;
  Adjust;
  RePaint;
end;

procedure TfpgBaseEdit.PasteFromClipboard;
begin
  DoPaste;
end;

{ TfpgBaseNumericEdit }

procedure TfpgBaseNumericEdit.SetOldColor(const AValue: TfpgColor);
begin
  if fOldColor=AValue then exit;
  fOldColor:=AValue;
end;

procedure TfpgBaseNumericEdit.SetAlignment(const AValue: TAlignment);
begin
  if fAlignment=AValue then exit;
  fAlignment:=AValue;
end;

procedure TfpgBaseNumericEdit.SetDecimalSeparator(const AValue: char);
begin
  if fDecimalSeparator=AValue then exit;
  fDecimalSeparator:=AValue;
end;

procedure TfpgBaseNumericEdit.SetNegativeColor(const AValue: TfpgColor);
begin
  if fNegativeColor=AValue then exit;
  fNegativeColor:=AValue;
end;

procedure TfpgBaseNumericEdit.SetThousandSeparator(const AValue: char);
begin
  if fThousandSeparator=AValue then exit;
  fThousandSeparator:=AValue;
end;

procedure TfpgBaseNumericEdit.Justify;
begin
  //based on Alignment property this method will align the derived edit correctly.
end;

procedure TfpgBaseNumericEdit.HandleKeyChar(var AText: TfpgChar;
  var shiftstate: TShiftState; var consumed: Boolean);
begin
  inherited HandleKeyChar(AText, shiftstate, consumed);
  Format; // just call format virtual procedure to have a simple way to manage polymorphism here
end;

procedure TfpgBaseNumericEdit.HandlePaint;
var
  x: TfpgCoord;
begin
  if Alignment = taRightJustify then
  begin
    Canvas.BeginDraw;
    inherited HandlePaint;
    //  Canvas.ClearClipRect;
    //  r.SetRect(0, 0, Width, Height);
    Canvas.Clear(BackgroundColor);
    Canvas.SetFont(Font);
    Canvas.SetTextColor(TextColor);
    x := Width - Font.TextWidth(Text) - 3;
    Canvas.DrawString(x,3,Text);
    Canvas.EndDraw;
    if Focused then
      fpgCaret.SetCaret(Canvas, x + Font.TextWidth(Text) - 1, 3, fpgCaret.Width, Font.Height);
  end
  else
  inherited;
end;

procedure TfpgBaseNumericEdit.Format;
begin
  // Colour negative number
  if LeftStr(Text,1) = '-' then
    TextColor := NegativeColor
  else
    TextColor := OldColor;
end;

constructor TfpgBaseNumericEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAlignment := taRightJustify;
  DecimalSeparator := SysUtils.DecimalSeparator;
  ThousandSeparator := SysUtils.ThousandSeparator;
  NegativeColor := clRed;
  OldColor := TextColor;
end;

{ TfpgEditInteger }

function TfpgEditInteger.GetValue: integer;
var
  txt: string;
begin
  if ShowThousand then
  begin
    if Copy(fText, 1, 1) = '-' then
      txt := Copy(ftext, 2, Length(fText) - 1)
    else
      txt := fText;
  	while Pos(ThousandSeparator, txt) > 0 do
  		txt := Copy(txt, 1, Pred(Pos(ThousandSeparator, txt)))
             +Copy(txt, Succ(Pos(ThousandSeparator, txt)), Length(txt) - Pos(ThousandSeparator, txt));
    if Copy(fText, 1, 1) = '-' then
      fText := '-' + txt
    else
      fText := txt;
  end;
  if fText = '-' then
  begin
    Result := 0;
    Text:= fText;
  end
  else
    if Text > '' then
      try
        Result := StrToInt(fText);
      except
        on E: EConvertError do
        begin
          Result := 0;
          Text := '';
          Invalidate;
        end;
      end
    else
      Result := 0;
end;

procedure TfpgEditInteger.SetValue(const AValue: integer);
begin
  try
    Text := IntToStr(AValue);
  except
    on E: EConvertError do
      Text := '';
  end;
end;

procedure TfpgEditInteger.SetShowThousand;
var
	i,long: integer;
  txt, texte: string;
begin
  if ShowThousand then
  begin
    if fText > '' then
      if fText[1] = '-' then
        txt:= UTF8Copy(fText, 2, UTF8Length(fText)-1)
      else
        txt:= fText;
  	long := UTF8Length(txt);
  	if long = 0 then
  		texte := ''
  	else
  	begin
      for i := 1 to UTF8Length(txt) do
        if txt[i] = ThousandSeparator then
          Exit;                               // avoids additional separators when pressing return
  		i := 0;
  		texte := '';
  		repeat
  			if i > 0 then
  				if ((i mod 3) = 0) and (txt[UTF8Length(txt)-UTF8Length(texte)] <> ThousandSeparator) then
          begin
  					texte := ThousandSeparator + texte;
            UTF8Insert(texte, txt, FCursorPos + 1);
            if fText[1] = '-' then
            begin
              if Pred(FCursorPos) <= UTF8Length(texte) then
                Inc(FCursorPos);
            end
            else
              if FCursorPos <= UTF8Length(texte) then
                Inc(FCursorPos);
          end;
  			texte := Copy(txt, long - i, 1) + texte;
  			inc(i);
  		until i = long;
  	end;
  if fText > '' then
    if fText[1] = '-' then
      fText:= '-' + texte
    else
      fText := texte;
  end;
end;

procedure TfpgEditInteger.Format;
begin
  SetShowThousand;
  inherited Format;
end;

procedure TfpgEditInteger.HandleKeyChar(var AText: TfpgChar;
  var shiftstate: TShiftState; var consumed: Boolean);
var
  n: integer;
begin
  n := Ord(AText[1]);
  if ((n >= 48) and (n <= 57) or (n = Ord('-')) and (Pos(AText[1], Self.Text) <= 0)) then
    consumed := False
  else
    consumed := True;
  inherited HandleKeyChar(AText, shiftstate, consumed);
end;

constructor TfpgEditInteger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fShowThousand := True;
end;

{ TfpgEditFloat }

function TfpgEditFloat.GetValue: extended;
var
  txt: string;
begin
  if fDecimals > 0 then
  begin
    if Pos(DecimalSeparator, fText) > 0 then
      if UTF8Length(fText)-Pos(DecimalSeparator, fText) > fDecimals then
        fText := Copy(fText, 1, UTF8Length(fText) - 1);
  end
  else
    if fDecimals = 0 then
      if Pos(DecimalSeparator, fText) > 0 then
        fText := Copy(fText, 1, UTF8Length(fText) - 1);
  if ShowThousand then
  begin
    if Copy(fText, 1, 1) = '-' then
      txt := Copy(ftext, 2, Length(fText) - 1)
    else
      txt := fText;
  	while Pos(ThousandSeparator, txt) > 0 do
  		txt := Copy(txt, 1, Pred(Pos(ThousandSeparator, txt)))
             +Copy(txt, Succ(Pos(ThousandSeparator, txt)), Length(txt) - Pos(ThousandSeparator, txt));
    if Copy(fText, 1, 1) = '-' then
      fText := '-' + txt
    else
      fText := txt;
  end;
  if fText = '-' then
  begin
    Result := 0;
    Text:= fText;
  end
  else
    if fText > '' then
    try
      Result := StrToFloat(fText);
    except
      on E: EConvertError do
      begin
        Result := 0;
        Text := '';
        Invalidate;
      end;
    end
  else
    Result := 0;
end;

procedure TfpgEditFloat.SetValue(const AValue: extended);
begin
  try
    Text := FloatToStr(AValue);
  except
    on E: EConvertError do
      Text := '';
  end;
end;

procedure TfpgEditFloat.SetShowThousand;
var
	i,long: integer;
  txt, texte, decimal: string;
begin
  if fDecimals > 0 then
    if Pos(DecimalSeparator, fText) > 0 then
    begin
      txt := UTF8Copy(fText, 1, Pred(Pos(DecimalSeparator, fText)));
      if UTF8Length(fText)-Pos(DecimalSeparator, fText) > fDecimals then
        decimal := UTF8Copy(fText, Succ(Pos(DecimalSeparator, fText)), fDecimals)
      else
        decimal := UTF8Copy(fText, Succ(Pos(DecimalSeparator, fText)), UTF8Length(fText)-Pos(DecimalSeparator, fText));
    end
    else
      txt := fText
  else
    if fDecimals = 0 then
      if Pos(DecimalSeparator, fText) > 0 then
        txt := UTF8Copy(fText, 1, Pred(Pos(DecimalSeparator, fText)))
      else
        txt := fText
    else
      if Pos(DecimalSeparator, fText) > 0 then
      begin
        txt := UTF8Copy(fText, 1, Pred(Pos(DecimalSeparator, fText)));
        decimal := UTF8Copy(fText, Succ(Pos(DecimalSeparator, fText)), UTF8Length(fText)-Pos(DecimalSeparator, fText));
      end
      else
        txt := fText;
  if ShowThousand then
  begin
    if fText > '' then
      if fText[1] = '-' then
        txt:= UTF8Copy(txt, 2, UTF8Length(txt)-1);
  	long := UTF8Length(txt);
  	if long = 0 then
  		texte := ''
  	else
  	begin
      for i := 1 to UTF8Length(txt) do
        if txt[i] = ThousandSeparator then
          Exit;                               // avoids additional separators when pressing return
  		i := 0;
  		texte := '';
  		repeat
  			if i > 0 then
  				if ((i mod 3) = 0) and (txt[UTF8Length(txt)-UTF8Length(texte)] <> ThousandSeparator) then
          begin
  					texte := ThousandSeparator + texte;
            UTF8Insert(texte, txt, FCursorPos + 1);
            if fText[1] = '-' then
            begin
              if Pred(FCursorPos) <= UTF8Length(texte) then
                Inc(FCursorPos);
            end
            else
              if FCursorPos <= UTF8Length(texte) then
                Inc(FCursorPos);
          end;
  			texte := Copy(txt, long - i, 1) + texte;
  			inc(i);
  		until i = long;
  	end;
  if fText > '' then
    if fText[1] = '-' then
      if Pos(DecimalSeparator, fText) > 0 then
        fText := '-' + texte + DecimalSeparator + decimal
      else
        fText := '-' + texte
    else
      if Pos(DecimalSeparator, fText) > 0 then
        fText := texte + DecimalSeparator + decimal
      else
        fText := texte + decimal;
  end;
end;

procedure TfpgEditFloat.SetDecimals(AValue: integer);
begin
  if AValue < -1 then
    Exit; // =>
  if fDecimals <> AValue then
    fDecimals := AValue
end;

procedure TfpgEditFloat.Format;
begin
  SetShowThousand;
  inherited Format;
end;

procedure TfpgEditFloat.HandleKeyChar(var AText: TfpgChar;
  var shiftstate: TShiftState; var consumed: Boolean);
var
  n: integer;
begin
  n := Ord(AText[1]);
  if ((n >= 48) and (n <= 57) or (n = Ord('-')) and (Pos(AText[1], Self.Text) <= 0))
     or ((n = Ord(Self.DecimalSeparator)) and (Pos(AText[1], Self.Text) <= 0)) then
    consumed := False
  else
    consumed := True;
  inherited HandleKeyChar(AText, shiftstate, consumed);
end;

constructor TfpgEditFloat.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDecimals := -1;
  fShowThousand := True;
end;

{ TfpgEditCurrency }

function TfpgEditCurrency.GetValue: Currency;
var
  txt: string;
begin
  if fDecimals > 0 then
    if Pos(DecimalSeparator, fText) > 0 then
      if UTF8Length(fText)-Pos(DecimalSeparator, fText) > fDecimals then
        fText := Copy(fText, 1, UTF8Length(fText) - 1);
  if ShowThousand then
  begin
    if Copy(fText, 1, 1) = '-' then
      txt := Copy(ftext, 2, Length(fText) - 1)
    else
      txt := fText;
  	while Pos(ThousandSeparator, txt) > 0 do
  		txt := Copy(txt, 1, Pred(Pos(ThousandSeparator, txt)))
             +Copy(txt, Succ(Pos(ThousandSeparator, txt)), Length(txt) - Pos(ThousandSeparator, txt));
    if Copy(fText, 1, 1) = '-' then
      fText := '-' + txt
    else
      fText := txt;
  end;
  if fText = '-' then
  begin
    Result := 0;
    Text:= fText;
  end
  else
    if fText > '' then
    try
      Result := StrToCurr(fText);
    except
      on E: EConvertError do
      begin
        Result := 0;
        Text := '';
        Invalidate;
      end;
    end
  else
    Result := 0;
end;

procedure TfpgEditCurrency.SetValue(const AValue: Currency);
var
	i,long: integer;
  txt, texte, decimal: string;
begin
  try
    fText := CurrToStr(AValue);
    if ShowThousand then
    begin
      if Pos(DecimalSeparator, fText) = 0 then
        txt := fText
      else
        begin
        txt := UTF8Copy(fText, 1, Pred(Pos(DecimalSeparator, fText)));
        decimal := UTF8Copy(fText, Succ(Pos(DecimalSeparator, fText)), UTF8Length(fText) - Pos(DecimalSeparator, fText));
        end;
      if AValue < 0 then
        txt:= UTF8Copy(txt, 2, UTF8Length(txt)-1);
    	long := UTF8Length(txt);
  		i := 0;
  		texte := '';
  		repeat
  			if i > 0 then
  				if ((i mod 3) = 0) and (txt[UTF8Length(txt)-UTF8Length(texte)] <> ThousandSeparator) then
          begin
  					texte := ThousandSeparator + texte;
            if AValue < 0 then
            begin
              if Pred(FCursorPos) <= UTF8Length(texte) then
                Inc(FCursorPos);
            end
            else
              if FCursorPos <= UTF8Length(texte) then
                Inc(FCursorPos);
          end;
  			texte := Copy(txt, long - i, 1) + texte;
  			inc(i);
  		until i = long;
      if Pos(DecimalSeparator, fText) = 0 then
      begin
        if AValue < 0 then
        begin
          fText := '-' + texte;
          Inc(FCursorPos);
        end
        else
          fText := texte;
      end
      else
      begin
        if AValue < 0 then
        begin
          fText := '-' + texte + DecimalSeparator + decimal;
          Inc(FCursorPos);
        end
        else
          fText := texte + DecimalSeparator + decimal;
        FCursorPos := FCursorPos + Succ(Length(decimal));
      end;
    end;
    if fDecimals > 0 then
    begin
      if Pos(DecimalSeparator, fText) = 0 then
        begin
        fText := fText + DecimalSeparator;
        Inc(FCursorPos);
        end;
      if UTF8Length(fText)-Pos(DecimalSeparator, fText) < fDecimals then
        while UTF8Length(fText)-Pos(DecimalSeparator, fText) < fDecimals do
          begin
          fText := fText + '0';
          Inc(FCursorPos);
          end;
    end;
    if AValue < 0 then
      TextColor := NegativeColor
    else
      TextColor := OldColor;
  except
    on E: EConvertError do
      Text := '';
  end;
end;

procedure TfpgEditCurrency.SetShowThousand;
var
	i,long: integer;
  txt, texte, decimal: string;
begin
  if fDecimals > 0 then
    if Pos(DecimalSeparator, fText) > 0 then
    begin
      txt := UTF8Copy(fText, 1, Pred(Pos(DecimalSeparator, fText)));
      if UTF8Length(fText)-Pos(DecimalSeparator, fText) > fDecimals then
        decimal := UTF8Copy(fText, Succ(Pos(DecimalSeparator, fText)), fDecimals)
      else
        decimal := UTF8Copy(fText, Succ(Pos(DecimalSeparator, fText)), UTF8Length(fText)-Pos(DecimalSeparator, fText));
    end
    else
      txt := fText;
  if ShowThousand then
  begin
    if fText > '' then
      if fText[1] = '-' then
        txt:= UTF8Copy(txt, 2, UTF8Length(txt)-1);
  	long := UTF8Length(txt);
  	if long = 0 then
  		texte := ''
  	else
  	begin
      for i := 1 to UTF8Length(txt) do
        if txt[i] = ThousandSeparator then
          Exit;                               // avoids additional separators when pressing return
  		i := 0;
  		texte := '';
  		repeat
  			if i > 0 then
  				if ((i mod 3) = 0) and (txt[UTF8Length(txt)-UTF8Length(texte)] <> ThousandSeparator) then
          begin
  					texte := ThousandSeparator + texte;
            UTF8Insert(texte, txt, FCursorPos + 1);
            if fText[1] = '-' then
            begin
              if Pred(FCursorPos) <= UTF8Length(texte) then
                Inc(FCursorPos);
            end
            else
              if FCursorPos <= UTF8Length(texte) then
                Inc(FCursorPos);
          end;
  			texte := Copy(txt, long - i, 1) + texte;
  			inc(i);
  		until i = long;
  	end;
  if fText > '' then
    if fText[1] = '-' then
      if Pos(DecimalSeparator, fText) > 0 then
        fText := '-' + texte + DecimalSeparator + decimal
      else
        fText := '-' + texte
    else
      if Pos(DecimalSeparator, fText) > 0 then
        fText := texte + DecimalSeparator + decimal
      else
        fText := texte + decimal;
  end;
end;

procedure TfpgEditCurrency.SetDecimals(AValue: integer);
begin
  if (AValue < 0) or (AValue > 4) then
    Exit; // =>
  if fDecimals <> AValue then
    fDecimals := AValue
end;

procedure TfpgEditCurrency.Format;
begin
  SetShowThousand;
  inherited Format;
end;

procedure TfpgEditCurrency.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: Boolean);
begin
  case keycode of
    keyReturn, keyPEnter, keyTab:
      if fDecimals > 0 then
      begin
        if Pos(DecimalSeparator, fText) = 0 then
          begin
          fText := fText + DecimalSeparator;
          Inc(FCursorPos);
          end;
        if UTF8Length(fText)-Pos(DecimalSeparator, fText) < fDecimals then
          while UTF8Length(fText)-Pos(DecimalSeparator, fText) < fDecimals do
          begin
            fText := fText + '0';
            Inc(FCursorPos);
          end;
      end;
    end;
  inherited HandleKeyPress(keycode,shiftstate,consumed);
end;

procedure TfpgEditCurrency.HandleKeyChar(var AText: TfpgChar;
  var shiftstate: TShiftState; var consumed: Boolean);
var
  n: integer;
begin
  n := Ord(AText[1]);
  if ((n >= 48) and (n <= 57) or (n = Ord('-')) and (Pos(AText[1], Self.Text) <= 0))
     or ((n = Ord(Self.DecimalSeparator)) and (Pos(AText[1], Self.Text) <= 0)) then
    consumed := False
  else
    consumed := True;
  inherited HandleKeyChar(AText, shiftstate, consumed);
end;

constructor TfpgEditCurrency.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDecimals := 2;
  fShowThousand := True;
end;


end.

