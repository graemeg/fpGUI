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
  gfx_widget;

type
  TfpgEditBorderStyle = (bsNone, bsDefault, bsSingle);


  TfpgCustomEdit = class(TfpgWidget)
  private
    FAutoSelect: Boolean;
    FHideSelection: Boolean;
    FText: string;
    FBackgroundColor: TfpgColor;
    FFont: TfpgFont;
    FPasswordMode: Boolean;
    FBorderStyle: TfpgEditBorderStyle;
    FOnChange: TNotifyEvent;
    FMaxLength: integer;
    FSelecting: Boolean;
    procedure   AdjustCursor;
    procedure   DeleteSelection;
    procedure   DoCopy;
    procedure   DoPaste;
    procedure   SetAutoSelect(const AValue: Boolean);
    procedure   SetBackgroundColor(const AValue: TfpgColor);
    procedure   SetBorderStyle(const AValue: TfpgEditBorderStyle);
    procedure   SetHideSelection(const AValue: Boolean);
    procedure   SetPasswordMode(const AValue: boolean);
    function    GetFontDesc: string;
    procedure   SetFontDesc(const AValue: string);
    procedure   SetText(const AValue: string);
  protected
    FMouseDragPos: integer;
    FDrawOffset: integer;
    FSideMargin: integer;
    FSelStart: integer;
    FSelOffset: integer;
    FCursorPos: integer;
    procedure   HandlePaint; override;
    procedure   HandleKeyChar(var AText: String; var shiftstate: TShiftState; var consumed: Boolean); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: Boolean); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleDoubleClick(x, y: integer; button: word; shiftstate: TShiftState); override;
    procedure   HandleMouseEnter; override;
    procedure   HandleMouseExit; override;
    procedure   HandleSetFocus; override;
    procedure   HandleKillFocus; override;
    function    GetDrawText: String;
    property    AutoSelect: Boolean read FAutoSelect write SetAutoSelect default True;
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor default clBoxColor;
    property    Font: TfpgFont read FFont;
    property    FontDesc: String read GetFontDesc write SetFontDesc;
    property    HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property    PasswordMode: Boolean read FPasswordMode write SetPasswordMode default False;
    property    BorderStyle: TfpgEditBorderStyle read FBorderStyle write SetBorderStyle default bsDefault;
    property    Text: String read FText write SetText;
    property    MaxLength: Integer read FMaxLength write FMaxLength;
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
  published
    property    AutoSelect;
    property    BackgroundColor;
    property    BorderStyle;
    property    FontDesc;
    property    HideSelection;
    property    MaxLength;
    property    PasswordMode;
    property    Text;
    property    OnChange;
    property    OnEnter;
    property    OnExit;
    property    OnKeyPress;
    property    OnMouseEnter;
    property    OnMouseExit;
    property    OnPaint;
  end;
  

function CreateEdit(AOwner: TComponent; x, y, w, h: TfpgCoord): TfpgEdit;


implementation

uses
  gfx_UTF8utils,
  gfx_clipboard;

function CreateEdit(AOwner: TComponent; x, y, w, h: TfpgCoord): TfpgEdit;
begin
  Result       := TfpgEdit.Create(AOwner);
  Result.Left  := x;
  Result.Top   := y;
  Result.Width := w;
  if h > 0 then
    Result.Height := h;
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

procedure TfpgCustomEdit.SetBackgroundColor(const AValue: TfpgColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    Repaint;
  end;
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
  begin
    if Focused then
      lcolor := clSelection
    else
      lcolor := clInactiveSel;

    len := FSelOffset;
    st  := FSelStart;
    if len < 0 then
    begin
      st  := st + len;
      len := -len;
    end;
    tw  := FFont.TextWidth(UTF8copy(dtext, 1, st));
    tw2 := FFont.TextWidth(UTF8copy(dtext, 1, st + len));
    Canvas.XORFillRectangle(fpgColorToRGB(lcolor) xor $FFFFFF,
      -FDrawOffset + FSideMargin + tw, 3, tw2 - tw, FFont.Height);
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
  Canvas.SetTextColor(clText1);
  Canvas.SetFont(FFont);
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

procedure TfpgCustomEdit.HandleKeyChar(var AText: String;
  var shiftstate: TShiftState; var consumed: Boolean);
var
  s: string;
  prevval: string;
begin
  prevval := Text;
  s       := AText;
  consumed := False;

  // Handle only printable characters
  // Note: This is not UTF-8 compliant!
  if (Ord(AText[1]) > 31) and (Ord(AText[1]) < 127) then
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

  if consumed then
    RePaint
  else
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
  Consumed := False;
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

  // searching the appropriate character position
  dtext := GetDrawText;
  cpx   := FFont.TextWidth(UTF8Copy(dtext, 1, FCursorPos)) - FDrawOffset + FSideMargin;
  cp    := FCursorPos;

  for n := 0 to UTF8Length(dtext) do
  begin
    cx := FFont.TextWidth(UTF8Copy(dtext, 1, n)) - FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp  := n;
    end;
  end;

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

procedure TfpgCustomEdit.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  n: integer;
  cpx: integer;
  cp: integer;
  cx: integer;
  dtext: string;
begin
  if (btnstate and MOUSE_LEFT) = 0 then
    Exit;

  // searching the appropriate character position
  dtext := GetDrawText;
  cpx   := FFont.TextWidth(UTF8Copy(dtext, 1, FCursorPos)) - FDrawOffset + FSideMargin;
  cp    := FCursorPos;

  for n := 0 to UTF8Length(dtext) do
  begin
    cx := FFont.TextWidth(UTF8Copy(dtext, 1, n)) - FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp  := n;
    end;
  end;

  //FMouseDragPos := cp;
  FSelOffset := cp - FSelStart;
  if FCursorPos <> cp then
  begin
    FCursorPos := cp;
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
  FOnChange         := nil;
end;

destructor TfpgCustomEdit.Destroy;
begin
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
end;

procedure TfpgCustomEdit.CopyToClipboard;
begin
  DoCopy;
end;

procedure TfpgCustomEdit.CutToClipboard;
begin
  DoCopy;
  DeleteSelection;
end;

procedure TfpgCustomEdit.PasteFromClipboard;
begin
  DoPaste;
end;

end.

