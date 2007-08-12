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
  TfpgCustomEdit = class(TfpgWidget)
  end;

  TfpgEdit = class(TfpgCustomEdit)
  private
    FText: string;
    FMaxLength: integer;
    FCursorPos: integer;
    FSideMargin: integer;
    FBackgroundColor: TfpgColor;
    FSelStart, FSelOffset: integer;
    FSelecting: boolean;
    FMouseDragPos: integer;
    FFont: TfpgFont;
    FDrawOffset: integer;
    function    GetFontDesc: string;
    procedure   SetFontDesc(const AValue: string);
    procedure   SetText(const AValue: string);
    procedure   DeleteSelection;
    procedure   DoCopy;
    procedure   DoPaste;
    procedure   AdjustCursor;
    function    GetDrawText: string;
    procedure   HandlePaint; override;
    procedure   HandleKeyChar(var AText: string; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleMouseEnter; override;
    procedure   HandleMouseExit; override;
  public
    PasswordMode: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    SelectionText: string;
    property    Font: TfpgFont read FFont;
    OnChange: TNotifyEvent;
  published
    property    Text: string read FText write SetText;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
  end;

function CreateEdit(AOwner: TComponent; x, y, w, h: TfpgCoord): TfpgEdit;

implementation

uses
  gfx_UTF8utils;

function CreateEdit(AOwner: TComponent; x, y, w, h: TfpgCoord): TfpgEdit;
begin
  Result       := TfpgEdit.Create(AOwner);
  Result.Left  := x;
  Result.Top   := y;
  Result.Width := w;
  if h > 0 then
    Result.Height := h;
end;

{ TfpgEdit }

constructor TfpgEdit.Create(AOwner: TComponent);
begin
  inherited;
  Focusable := True;

  FFont := fpgGetFont('#Edit1');  // owned object !

  FHeight      := FFont.Height + 6;
  FWidth       := 120;
  FBackgroundColor := clBoxColor;
  FSelecting   := False;
  FSideMargin  := 3;
  FMaxLength   := 0;
  FText        := '';
  FCursorPos   := UTF8Length(FText);
  FSelStart    := FCursorPos;
  FSelOffset   := 0;
  FDrawOffset  := 0;
  PasswordMode := False;

  OnChange := nil;
end;

destructor TfpgEdit.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TfpgEdit.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit;

  FText       := AValue;
  FCursorPos  := UTF8Length(FText);
  FSelStart   := FCursorPos;
  FSelOffset  := 0;
  FDrawOffset := 0;

  AdjustCursor;
  if FWinHandle > 0 then
    RePaint;
end;

function TfpgEdit.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TfpgEdit.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  RePaint;
end;

procedure TfpgEdit.DeleteSelection;
begin
  if FSelOffset <> 0 then
  begin
    if FSelOffset < 0 then
    begin
      Delete(FText, 1 + FSelStart + FSelOffset, -FSelOffset);
      FCurSorPos := FSelStart + FSelOffset;
    end
    else
    begin
      Delete(FText, 1 + FSelStart, FSelOffset);
      FCurSorPos := FSelStart;
    end;
    FSelOffset := 0;
    FSelStart := FCursorPos;
  end;
end;

procedure TfpgEdit.DoCopy;
begin
  if FSelOffset = 0 then
    Exit;
  //SetClipboardText(SelectionText);
end;

procedure TfpgEdit.DoPaste;
var
  s: string;
begin
  DeleteSelection;
          //s := GetClipboardText;
  if (FMaxLength > 0) then
    if UTF8Length(FText) + UTF8Length(s) > FMaxLength then
      s := UTF8Copy(s, 1, FMaxLength - UTF8Length(FText));  // trim the clipboard text if needed

  if UTF8Length(s) < 1 then
    Exit; //==>
  {$Note Is Insert() UTF-8 safe? }
  Insert(s, FText, FCursorPos + 1);
  FCursorPos := FCursorPos + UTF8Length(s);
  AdjustCursor;
  Repaint;
end;

procedure TfpgEdit.AdjustCursor;
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

function TfpgEdit.GetDrawText: string;
begin
  if not PassWordMode then
    Result := FText
  else
    Result := StringOfChar('*', UTF8Length(FText));
end;

procedure TfpgEdit.HandlePaint;
var
  r: TfpgRect;
  tw, tw2, st, len: integer;
  dtext: string;
begin
  Canvas.BeginDraw;
  Canvas.ClearClipRect;
  r.SetRect(0, 0, Width, Height);
  Canvas.DrawControlFrame(r);

  InflateRect(r, -2, -2);
  Canvas.SetClipRect(r);

  if Enabled then
    Canvas.SetColor(FBackgroundColor)
  else
    Canvas.SetColor(clWindowBackground);
  Canvas.FillRectAngle(r);

  dtext := GetDrawText;
  Canvas.SetTextColor(clText1);
  Canvas.SetFont(FFont);
  fpgStyle.DrawString(Canvas, -FDrawOffset + FSideMargin, 3, dtext, Enabled);

  if Focused then
  begin
    // drawing selection
    if FSelOffset <> 0 then
    begin
      len := FSelOffset;
      st  := FSelStart;
      if len < 0 then
      begin
        st  := st + len;
        len := -len;
      end;

      tw  := FFont.TextWidth(UTF8copy(dtext, 1, st));
      tw2 := FFont.TextWidth(UTF8copy(dtext, 1, st + len));
      Canvas.XORFillRectangle(fpgColorToRGB(clSelection) xor $FFFFFF, -FDrawOffset +
        FSideMargin + tw, 3, tw2 - tw, FFont.Height);
    end;

    // drawing cursor
    tw := FFont.TextWidth(UTF8copy(dtext, 1, FCursorPos));
    fpgCaret.SetCaret(Canvas, -FDrawOffset + FSideMargin + tw, 3, fpgCaret.Width, FFont.Height);
  end
  else
    fpgCaret.UnSetCaret(Canvas);

  Canvas.EndDraw;
end;

procedure TfpgEdit.HandleKeyChar(var AText: string;
  var shiftstate: TShiftState; var consumed: boolean);
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
      Insert(s, FText, FCursorPos + 1);
      Inc(FCursorPos);
      FSelStart := FCursorPos;
      AdjustCursor;
    end;
    consumed := True;
  end;

  if prevval <> Text then
    if Assigned(OnChange) then
      OnChange(self);

  if consumed then
    RePaint
  else
    inherited HandleKeyChar(AText, shiftstate, consumed);
end;

procedure TfpgEdit.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  lpos: integer;
  
  procedure StopSelection;
  begin
    FSelStart  := FCursorPos;
    FSelOffset := 0;
  end;

begin
//  writeln(Classname, '.Keypress');
  Consumed := False;
  lpos := FCursorPos;
{
  Consumed := true;
  case ptkCheckClipBoardKey(keycode, shiftstate) of
    ckCopy:   DoCopy;
    ckPaste:  DoPaste;
    ckCut:    begin
                DoCopy;
                DeleteSelection;
              end;
  else
    Consumed := false;
  end;
}

  if not Consumed then
  begin
    // checking for movement keys:
    consumed := True;

    case keycode of
      keyLeft:
        if FCursorPos > 0 then
        begin
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
        FCursorPos := 0;

      keyEnd:
        FCursorPos := UTF8Length(FText);
      else
        Consumed   := False;
    end;

    if lpos = FCursorPos then // nothing changed so reset consumed
      consumed := False;

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
        if FCursorPos > 0 then
        begin
          Delete(FText, FCursorPos, 1);
          Dec(FCursorPos);
        end;// backspace


      keyDelete:
        if FSelOffset <> 0 then
          DeleteSelection
        else if FCursorPos < UTF8Length(FText) then
          Delete(FText, FCursorPos + 1, 1);
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
end;

procedure TfpgEdit.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  s: string;
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
  s     := '';

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

procedure TfpgEdit.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  s: string;
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
  s     := '';

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

procedure TfpgEdit.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  MouseCursor := mcIBeam;
end;

procedure TfpgEdit.HandleMouseExit;
begin
  inherited HandleMouseExit;
  MouseCursor := mcDefault;
end;

function TfpgEdit.SelectionText: string;
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

end.

