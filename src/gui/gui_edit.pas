{
    fpGUI  -  Free Pascal GUI Library

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
  TfpgEditBorderStyle = (bsNone, bsDefault, bsSingle);


  { TfpgCustomEdit }

  TfpgCustomEdit = class(TfpgWidget)
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
    procedure   AdjustCursor;
    function    CursorPosToCharPos(x, y: integer): integer;
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
    FMouseDragPos: integer;
    FDrawOffset: integer;
    FSideMargin: integer;
    FSelStart: integer;
    FSelOffset: integer;
    FCursorPos: integer;
    procedure   ShowDefaultPopupMenu(const x, y: integer; const shiftstate: TShiftState); virtual;
    procedure   HandlePaint; override;
    procedure   HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: Boolean); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: Boolean); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleRMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleDoubleClick(x, y: integer; button: word; shiftstate: TShiftState); override;
    procedure   HandleMouseEnter; override;
    procedure   HandleMouseExit; override;
    procedure   HandleSetFocus; override;
    procedure   HandleKillFocus; override;
    function    GetDrawText: String;
    property    AutoSelect: Boolean read FAutoSelect write SetAutoSelect default True;
    property    BorderStyle: TfpgEditBorderStyle read FBorderStyle write SetBorderStyle default bsDefault;
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


  TfpgEdit = class(TfpgCustomEdit)
  public
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


  { TfpgBaseNumericEdit }

  TfpgBaseNumericEdit = class(TfpgCustomEdit)
  private
    fOldColor: TfpgColor;
    fAlignment: TAlignment;
    fDecimalSeparator: char;
    fNegativeColor: TfpgColor;
    fThousandSeparator: char;
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


  { TfpgEditInteger }

  TfpgEditInteger = class(TfpgBaseNumericEdit)
  protected
    function    GetValue: integer; virtual;
    procedure   SetValue(const AValue: integer); virtual;
    procedure   Format; override;
    procedure   HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: Boolean); override;
  published
     property   Alignment;
     property   NegativeColor;
     property   Value: integer read GetValue write SetValue;
  end;


  { TfpgEditFloat }

  TfpgEditFloat = class(TfpgBaseNumericEdit)
  protected
    function    GetValue: extended; virtual;
    procedure   SetValue(const AValue: extended); virtual;
    procedure   HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: Boolean); override;
  published
     property   Alignment;
     property   NegativeColor;
     property   DecimalSeparator;
     property   Value: extended read GetValue write SetValue;
  end;


function CreateEdit(AOwner: TComponent; x, y, w, h: TfpgCoord): TfpgEdit;


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
  Result.Width := w;
  if h < TfpgEdit(Result).FFont.Height + 6 then
    Result.Height:= TfpgEdit(Result).FFont.Height + 6
  else
    Result.Height:= h;
end;


{ TfpgCustomEdit }

procedure TfpgCustomEdit.AdjustCursor;
var
  tw: integer;
  VisibleWidth: integer;
begin
  tw           := FFont.TextWidth(UTF8Copy(GetDrawText, 1, FCursorPos));
  VisibleWidth := (FWidth - 2 * FSideMargin);

  if tw - FDrawOffset > VisibleWidth - 2 then
    FDrawOffset := tw - VisibleWidth + 2
  else if tw - FDrawOffset < 0 then
  begin
    FDrawOffset := tw;
    if tw <> 0 then
      Dec(FDrawOffset, 2);
  end;
end;

function TfpgCustomEdit.CursorPosToCharPos(x, y: integer): integer;
var
  n: integer;
  cpx: integer;
  cx: integer;
  dtext: string;
  tw, dpos: integer;
  // tc: Cardinal;
  ch: TfpgChar;
begin
  // searching the appropriate character position
  ch     := '';
  dtext  := GetDrawText;
  cpx    := FFont.TextWidth(UTF8Copy(dtext, 1, FCursorPos)) - FDrawOffset + FSideMargin;
  Result := FCursorPos;
  
  // tc := fpgGetTickCount;
  {for n := 0 to UTF8Length(dtext) do
  begin
    cx := FFont.TextWidth(UTF8Copy(dtext, 1, n)) - FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx    := cx;
      Result := n;
    end;
  end;}
  
  tw   := 0;
  n    := 0;
  dpos := 0;
  while dpos <= Length(dtext) do
  begin
    dpos := UTF8CharAtByte(dtext, dpos, ch);
    tw := tw + FFont.TextWidth(ch);
    cx := tw - FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx    := cx;
      Result := n;
    end;
    Inc(n);
  end;
  // writeln(fpgGetTickCount - tc);
end;

procedure TfpgCustomEdit.SetBorderStyle(const AValue: TfpgEditBorderStyle);
begin
  if FBorderStyle = AValue then
    Exit; //==>
  FBorderStyle := AValue;
  RePaint;
end;

procedure TfpgCustomEdit.SetHideSelection(const AValue: Boolean);
begin
  if FHideSelection = AValue then
    Exit;
  FHideSelection := AValue;
end;

procedure TfpgCustomEdit.HandlePaint;
var
  r: TfpgRect;
  tw, tw2, st, len: integer;
  dtext: string;

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

    len := FSelOffset;
    st  := FSelStart;
    if len < 0 then
    begin
      st  := st + len;
      len := -len;
    end;
    tw  := FFont.TextWidth(UTF8copy(dtext, 1, st));
    tw2 := FFont.TextWidth(UTF8copy(dtext, 1, st + len));

    Canvas.SetColor(lcolor);
    Canvas.FillRectangle(-FDrawOffset + FSideMargin + tw, 3, tw2 - tw, FFont.Height);
    r.SetRect(-FDrawOffset + FSideMargin + tw, 3, tw2 - tw, FFont.Height);
    Canvas.AddClipRect(r);
    Canvas.SetTextColor(clWhite);
    fpgStyle.DrawString(Canvas, -FDrawOffset + FSideMargin, 3, dtext, Enabled);
    Canvas.ClearClipRect;
  end;
  
begin
  Canvas.BeginDraw;

  Canvas.ClearClipRect;
  r.SetRect(0, 0, Width, Height);
  case BorderStyle of
    bsNone:
        begin
          // do nothing
        end;
    bsDefault:
        begin
          Canvas.DrawControlFrame(r);
          InflateRect(r, -2, -2);
        end;
    bsSingle:
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
  dtext := GetDrawText;
  Canvas.SetFont(FFont);
  Canvas.SetTextColor(FTextColor);
  fpgStyle.DrawString(Canvas, -FDrawOffset + FSideMargin, 3, dtext, Enabled);


  if Focused then
  begin
    // drawing selection
    if FSelOffset <> 0 then
      DrawSelection;

    // drawing cursor
    tw := FFont.TextWidth(UTF8copy(dtext, 1, FCursorPos));
    fpgCaret.SetCaret(Canvas, -FDrawOffset + FSideMargin + tw, 3, fpgCaret.Width, FFont.Height);
  end
  else
  begin
    // drawing selection
    if (AutoSelect = False) and (FSelOffset <> 0) and (HideSelection = False) then
      DrawSelection;
    fpgCaret.UnSetCaret(Canvas);
  end;

  Canvas.EndDraw;
end;

procedure TfpgCustomEdit.HandleKeyChar(var AText: TfpgChar;
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
    // Note: This is now UTF-8 compliant!
    if ((Ord(AText[1]) > 31) and (Ord(AText[1]) < 127)) or (Length(AText) > 1) then
    begin
      if (FMaxLength <= 0) or (UTF8Length(FText) < FMaxLength) then
      begin
        DeleteSelection;
        UTF8Insert(s, FText, FCursorPos + 1);
        Inc(FCursorPos);
        FSelStart := FCursorPos;
        AdjustCursor;
      end;
      consumed := True;
    end;

    if prevval <> Text then
      if Assigned(FOnChange) then
        FOnChange(self);
  end;
  
  if consumed then
    RePaint;

  inherited HandleKeyChar(AText, shiftstate, consumed);
end;

procedure TfpgCustomEdit.HandleKeyPress(var keycode: word;
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
      AdjustCursor;

      FSelecting := (ssShift in shiftstate);

      if FSelecting then
        FSelOffset := FCursorPos - FSelStart
      else
        StopSelection;
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
      AdjustCursor;
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

procedure TfpgCustomEdit.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  n: integer;
  cpx: integer;
  cp: integer;
  cx: integer;
  dtext: string;
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  cp := CursorPosToCharPos(x, y);

  FMouseDragPos := cp;
  FCursorPos    := cp;

  if (ssShift in shiftstate) then
    FSelOffset := FCursorPos - FSelStart
  else
  begin
    FSelStart  := cp;
    FSelOffset := 0;
  end;
  Repaint;
end;

procedure TfpgCustomEdit.HandleRMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleRMouseDown(x, y, shiftstate);
  if Assigned(PopupMenu) then
    PopupMenu.ShowAt(self, x, y)
  else
    ShowDefaultPopupMenu(x, y, ShiftState);
end;

procedure TfpgCustomEdit.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  cp: integer;
begin
  if (btnstate and MOUSE_LEFT) = 0 then
    Exit;
    
  cp := CursorPosToCharPos(x, y);

  //FMouseDragPos := cp;
  FSelOffset := cp - FSelStart;
  if FCursorPos <> cp then
  begin
    FCursorPos := cp;
    AdjustCursor;
    Repaint;
  end;
end;

procedure TfpgCustomEdit.HandleDoubleClick(x, y: integer; button: word; shiftstate: TShiftState);
begin
  // button is always Mouse_Left, but lets leave this test here for good measure
  if button = MOUSE_LEFT then
    SelectAll
  else
    inherited;
end;

procedure TfpgCustomEdit.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  if (csDesigning in ComponentState) then
    Exit;
  if Enabled then
    MouseCursor := mcIBeam;
end;

procedure TfpgCustomEdit.HandleMouseExit;
begin
  inherited HandleMouseExit;
  if (csDesigning in ComponentState) then
    Exit;
  MouseCursor := mcDefault;
end;

procedure TfpgCustomEdit.HandleSetFocus;
begin
  inherited HandleSetFocus;
  if AutoSelect then
    SelectAll;
end;

procedure TfpgCustomEdit.HandleKillFocus;
begin
  inherited HandleKillFocus;
  if AutoSelect then
    FSelOffset := 0;
end;

function TfpgCustomEdit.GetDrawText: string;
begin
  if not PassWordMode then
    Result := FText
  else
    Result := StringOfChar('*', UTF8Length(FText));
end;

constructor TfpgCustomEdit.Create(AOwner: TComponent);
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
  FDrawOffset       := 0;
  FPasswordMode     := False;
  FBorderStyle      := bsDefault;
  FPopupMenu        := nil;
  FDefaultPopupMenu := nil;
  FOnChange         := nil;
end;

destructor TfpgCustomEdit.Destroy;
begin
  if Assigned(FDefaultPopupMenu) then
    FDefaultPopupMenu.Free;
  FFont.Free;
  inherited Destroy;
end;

function TfpgCustomEdit.SelectionText: string;
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

procedure TfpgCustomEdit.SetPasswordMode (const AValue: boolean );
begin
  if FPasswordMode = AValue then
    Exit; //==>
  FPasswordMode := AValue;
  RePaint;
end;

function TfpgCustomEdit.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TfpgCustomEdit.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  if Height < FFont.Height + 6 then
    Height:= FFont.Height + 6;
  RePaint;
end;

procedure TfpgCustomEdit.SetText(const AValue: string);
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
  FDrawOffset := 0;

  AdjustCursor;
  RePaint;
end;

procedure TfpgCustomEdit.DefaultPopupCut(Sender: TObject);
begin
  CutToClipboard;
end;

procedure TfpgCustomEdit.DefaultPopupCopy(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TfpgCustomEdit.DefaultPopupPaste(Sender: TObject);
begin
  PasteFromClipboard
end;

procedure TfpgCustomEdit.DefaultPopupClearAll(Sender: TObject);
begin
  Clear;
end;

procedure TfpgCustomEdit.SetDefaultPopupMenuItemsState;
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

procedure TfpgCustomEdit.ShowDefaultPopupMenu(const x, y: integer;
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

procedure TfpgCustomEdit.DeleteSelection;
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

procedure TfpgCustomEdit.DoCopy;
begin
  if FSelOffset = 0 then
    Exit; //==>
  fpgClipboard.Text := SelectionText;
end;

procedure TfpgCustomEdit.DoPaste;
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
  AdjustCursor;
  Repaint;
end;

procedure TfpgCustomEdit.SetAutoSelect(const AValue: Boolean);
begin
  if FAutoSelect = AValue then
    Exit; //==>
  FAutoSelect := AValue;
end;

procedure TfpgCustomEdit.SelectAll;
begin
  FSelecting  := True;
  FSelStart   := 0;
  FSelOffset  := UTF8Length(FText);
  FCursorPos  := FSelOffset;
  Repaint;
end;

procedure TfpgCustomEdit.Clear;
begin
  Text := '';
end;

procedure TfpgCustomEdit.ClearSelection;
begin
  DeleteSelection;
  RePaint;
end;

procedure TfpgCustomEdit.CopyToClipboard;
begin
  DoCopy;
end;

procedure TfpgCustomEdit.CutToClipboard;
begin
  DoCopy;
  DeleteSelection;
  RePaint;
end;

procedure TfpgCustomEdit.PasteFromClipboard;
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
  Format; // just call format virtual procedure to have a simple way to manage polymorphism here
  inherited HandleKeyChar(AText, shiftstate, consumed);
end;

procedure TfpgBaseNumericEdit.HandlePaint;
var
  x: TfpgCoord;
  s: string;
  r: TfpgRect;
  tw: integer;
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
    x := Width - Font.TextWidth(Text) - 1;
    Canvas.DrawString(x,1,Text);
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
begin
  try
    Result := StrToInt(Text);
  except
    on E: EConvertError do
    begin
      Result := 0;
      Text := '';
      Invalidate;
    end;
  end;
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

procedure TfpgEditInteger.Format;
begin
// here there will be, for example, thousand separator integer formatting routine
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

{ TfpgEditFloat }

function TfpgEditFloat.GetValue: extended;
begin
  try
    Result := StrToFloat(Text);
  except
    on E: EConvertError do
    begin
      Result := 0;
      Text := FloatToStr(Result);
    end;
  end;
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


end.

