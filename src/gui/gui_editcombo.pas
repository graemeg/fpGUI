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
      Defines a edit ComboBox control with auto-complete feature.
}

unit gui_editcombo;

{$mode objfpc}{$H+}

{.$Define DEBUG}

{
    ***********************************************************
    **********   This is still under development!   ***********
    ***********************************************************

    It needs lots of testing and debugging.
}


{ TODO: Needs a lot of refactoring to get rid of code duplication. }

{
This is an example of what we can aim for:
You need a mono font to see the correct layout.


               TfpgBaseComboBox
              _________|______________
             |                        |
   TfpgBaseStaticCombo        TfpgBaseEditCombo
       ______|_________               |
      |                |         TfpgEditCombo
      |                |
 TfpgComboBox   TfpgBaseColorCombo
                       |
                 TfpgColorComboBox
}

interface

uses
  Classes,
  SysUtils,
  gfx_widget,
  gfxbase,
  gui_button,
  fpgfx,
  gfx_popupwindow;

type

  TfpgAbstractEditCombo = class(TfpgWidget)
  private
    FAutoCompletion: Boolean;
    FDropDownCount: integer;
    FBackgroundColor: TfpgColor;
    FTextColor: TfpgColor;
    FText: string;
    FSelectedItem: integer;
    FMaxLength: integer;
    FFocusItem: integer;
    FFont: TfpgFont;
    FInternalBtnRect: TfpgRect;
    FItems: TStringList;
    FOnChange: TNotifyEvent;
    function    GetFontDesc: string;
    procedure   SetBackgroundColor(const AValue: TfpgColor);
    procedure   SetTextColor(const AValue: TfpgColor);
    procedure   SetDropDownCount(const AValue: integer);
    procedure   InternalBtnClick(Sender: TObject);
    procedure   InternalListBoxSelect(Sender: TObject);
    procedure   SetFocusItem(const AValue: integer);
    procedure   SetFontDesc(const AValue: string);
    procedure   CalculateInternalButtonRect;
    procedure   MsgPopupClose(var msg: TfpgMessageRec); message FPGM_POPUPCLOSE;
  protected
    FMargin: integer;
    FBtnPressed: Boolean;
    FDropDown: TfpgPopupWindow;
    FDrawOffset: integer;
    FSelStart: integer;
    FSelOffset: integer;
    FCursorPos: integer;
    procedure   DoDropDown; virtual;
    function    GetText: string; virtual;
    function    HasText: boolean; virtual;
    procedure   SetText(const AValue: string); virtual;
    procedure   SetHeight(const AValue: TfpgCoord); override;
    procedure   SetWidth(const AValue: TfpgCoord); override;
    procedure   HandleKeyChar(var AText: String; var shiftstate: TShiftState; var consumed: Boolean); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: Boolean); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
    procedure   HandlePaint; override;
    procedure   PaintInternalButton; virtual;
    property    DropDownCount: integer read FDropDownCount write SetDropDownCount default 8;
    property    Items: TStringList read FItems;    {$Note Make this read/write}
    // property is 1-based
    property    FocusItem: integer read FFocusItem write SetFocusItem;
    property    AutoCompletion: Boolean read FAutocompletion write FAutoCompletion default False;
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor default clBoxColor;
    property    TextColor: TfpgColor read FTextColor write SetTextColor default clText1;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
    property    Text: string read GetText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Update;
    property    Font: TfpgFont read FFont;
  end;


  TfpgEditCombo = class(TfpgAbstractEditCombo)
  published
    property    AutoCompletion;
    property    BackgroundColor;
    property    DropDownCount;
    property    FocusItem;
    property    FontDesc;
    property    Height;
    property    Items;
    property    Text;
    property    TextColor;
    property    Width;
    property    OnChange;
  end;


function CreateEditCombo(AOwner: TComponent; x, y, w: TfpgCoord; AList:TStringList): TfpgEditCombo;


implementation

uses
  gui_listbox,
  gfx_UTF8utils,
  math;

var
  OriginalFocusRoot: TfpgWidget;

type
  { This is the class representing the dropdown window of the combo box. }
  TDropDownWindow = class(TfpgPopupWindow)
  private
    FCallerWidget: TfpgWidget;
  protected
    procedure   HandlePaint; override;
    procedure   HandleKeyChar(var AText: String; var shiftstate: TShiftState; var consumed: Boolean); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleShow; override;
    procedure   HandleHide; override;
  public
    constructor Create(AOwner: TComponent); override;
    ListBox:    TfpgListBox;
    property    CallerWidget: TfpgWidget read FCallerWidget write FCallerWidget;
  end;


{ TDropDownWindow }

procedure TDropDownWindow.HandlePaint;
begin
  Canvas.BeginDraw;
//  inherited HandlePaint;
  Canvas.Clear(clWhite);
  Canvas.EndDraw;
end;

procedure TDropDownWindow.HandleKeyChar(var AText: String;
  var shiftstate: TShiftState; var consumed: Boolean);
begin
  if TfpgEditCombo(FCallerWidget).FAutoCompletion then
    TfpgEditCombo(FCallerWidget).HandleKeyChar(AText,shiftstate,consumed);
end;

procedure TDropDownWindow.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  if TfpgEditCombo(FCallerWidget).FAutoCompletion then
  begin
    TfpgEditCombo(FCallerWidget).HandleKeyPress(keycode,shiftstate,consumed);
//    consumed:= True;
  end
  else
  begin
    inherited HandleKeyPress(keycode, shiftstate, consumed);
    if keycode = keyEscape then
    begin
      consumed := True;
      Close;
    end;
  end;
end;

procedure TDropDownWindow.HandleShow;
begin
  ListBox.SetPosition(0, 0, Width, Height);
  inherited HandleShow;
  ActiveWidget := ListBox;
end;

procedure TDropDownWindow.HandleHide;
begin
  // HandleHide also gets called in TfpgWidget.Destroy so we need a few
  // if Assigned() tests here. This should be improved on.
  if Assigned(FocusRootWidget) then
    FocusRootWidget.ReleaseMouse;  // for internal ListBox

  FocusRootWidget   := OriginalFocusRoot;
  OriginalFocusRoot := nil;
  inherited HandleHide;

  if Assigned(FocusRootWidget) then
    FocusRootWidget.SetFocus;
end;

constructor TDropDownWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ListBox := TfpgListBox.Create(self);
  ListBox.PopupFrame := True;
end;

function CreateEditCombo(AOwner: TComponent; x, y, w: TfpgCoord;
    AList: TStringList): TfpgEditCombo;
begin
  Result           := TfpgEditCombo.Create(AOwner);
  Result.Left      := x;
  Result.Top       := y;
  Result.Width     := w;
  Result.Focusable := True;

  Result.Height := 23;  // replace this with font height + margins
  {$Note We still need to handle the AList param as well.}
end;

{ TfpgAbstractEditCombo }

procedure TfpgAbstractEditCombo.SetDropDownCount(const AValue: integer);
begin
  if FDropDownCount = AValue then
    Exit;
  FDropDownCount := AValue;
end;

procedure TfpgAbstractEditCombo.SetBackgroundColor(const AValue: TfpgColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    Repaint;
  end;
end;

procedure TfpgAbstractEditCombo.SetTextColor(const AValue: TfpgColor);
begin
  if FTextColor <> AValue then
  begin
    FTextColor := AValue;
    Repaint;
  end;
end;

function TfpgAbstractEditCombo.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

function TfpgAbstractEditCombo.GetText: string;
var
  i: integer;
begin
  if FAutoCompletion then
  begin
    if (FocusItem > 0) and (FocusItem <= FItems.Count) then
    begin
      FText := FItems.Strings[FocusItem-1];
      FSelectedItem:= FocusItem-1;
    end
    else if FText <> '' then
    begin
      for i := 0 to FItems.Count - 1 do
      begin
        if SameText(UTF8Copy(FItems.Strings[i], 1, UTF8Length(FText)), FText) then
        begin
          FSelectedItem := i;
          if FDropDown= nil then
            DoDropDown;
          TDropDownWindow(FDropDown).ListBox.SetFirstItem(FSelectedItem+1);
          TDropDownWindow(FDropDown).ListBox.Invalidate;
          Break;
        end;
        FSelectedItem := -1;
      end;
    end;
    FCursorPos := UTF8Length(FText);
    FSelStart := FCursorPos;
    Result := FText;
  end
  else
    if (FocusItem > 0) and (FocusItem <= FItems.Count) then
      Result := FItems.Strings[FocusItem-1]
    else
      Result := '';
end;

function TfpgAbstractEditCombo.HasText: boolean;
begin
  Result := FocusItem > 0;
end;

procedure TfpgAbstractEditCombo.DoDropDown;
var
  ddw: TDropDownWindow;
  rowcount: integer;
begin
  if (not Assigned(FDropDown)) or (not FDropDown.HasHandle) then
  begin
    FreeAndNil(FDropDown);
    OriginalFocusRoot := FocusRootWidget;
    FDropDown       := TDropDownWindow.Create(nil);
    ddw := TDropDownWindow(FDropDown);
    ddw.Width := Width;
    // adjust the height of the dropdown
    rowcount := FItems.Count;
    if rowcount > FDropDownCount then
      rowcount := FDropDownCount;
    if rowcount < 1 then
      rowcount := 1;
    ddw.Height            := (ddw.ListBox.RowHeight * rowcount) + 4;
    ddw.CallerWidget      := self;
    ddw.ListBox.OnSelect  := @InternalListBoxSelect;

    // Assign combobox text items to internal listbox and set default focusitem
    ddw.ListBox.Items.Assign(FItems);
    ddw.ListBox.FocusItem := FFocusItem;

    ddw.DontCloseWidget := self;  // now we can control when the popup window closes
    ddw.ShowAt(Parent, Left, Top+Height);
//    ddw.ListBox.SetFocus;
  end
  else
  begin
    FBtnPressed := False;
    ddw := TDropDownWindow(FDropDown);
    ddw.Close;
    FreeAndNil(FDropDown);
  end;
end;

procedure TfpgAbstractEditCombo.InternalBtnClick(Sender: TObject);
begin
  DoDropDown;
end;

procedure TfpgAbstractEditCombo.InternalListBoxSelect(Sender: TObject);
var
  msgp: TfpgMessageParams;
begin
  FFocusItem := TDropDownWindow(FDropDown).ListBox.FocusItem;
  FDropDown.Close;

  if HasHandle then
    Repaint;
  if Assigned(FOnChange) then
    FOnChange(self);
end;

{ Focusitem is 1 based and NOT 0 based like the Delphi ItemIndex property.
  So at startup, FocusItem = 0 which means nothing is selected. If
  FocusItem = 1 it means the first item is selected etc. }
procedure TfpgAbstractEditCombo.SetFocusItem(const AValue: integer);
begin
  if FFocusItem = AValue then
    Exit; //==>
  FFocusItem := AValue;

  // do some limit check corrections
  if FFocusItem < 0 then
    FFocusItem := 0   // nothing is selected
  else if FFocusItem > FItems.Count then
    FFocusItem := FItems.Count;

  RePaint;
end;

procedure TfpgAbstractEditCombo.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  RePaint;
end;

procedure TfpgAbstractEditCombo.SetText(const AValue: string);
var
  i: integer;
begin
  if AValue = '' then
    SetFocusItem(0)  // nothing selected
  else
  begin
    for i := 0 to FItems.Count - 1 do
    begin
      if SameText(UTF8Copy(FItems.Strings[i],1,UTF8Length(AVAlue)), AValue) then
      begin
  SetFocusItem(i+1); // our FocusItem is 1-based. TStringList is 0-based.
        Break;
      end;
    end;
    // if we get here, we didn't find a match
    SetFocusItem(0);
  end;
end;

procedure TfpgAbstractEditCombo.SetWidth(const AValue: TfpgCoord);
begin
  inherited;
  CalculateInternalButtonRect;
  RePaint;
end;

procedure TfpgAbstractEditCombo.CalculateInternalButtonRect;
begin
  FInternalBtnRect.SetRect(Width - Min(Height, 20), 2, Min(Height, 20)-2,
      Height-4);
end;

procedure TfpgAbstractEditCombo.MsgPopupClose(var msg: TfpgMessageRec);
begin
  DoDropDown;
end;

procedure TfpgAbstractEditCombo.SetHeight(const AValue: TfpgCoord);
begin
  inherited;
  CalculateInternalButtonRect;
  RePaint;
end;

procedure TfpgAbstractEditCombo.HandleKeyChar(var AText: String;
    var shiftstate: TShiftState; var consumed: Boolean);
var
  s: string;
  prevval: string;
begin
  prevval := FText;
  s       := AText;
  consumed := False;

  // Handle only printable characters
  // Note: This is not UTF-8 compliant!
  if (Ord(AText[1]) > 31) and (Ord(AText[1]) < 127) then
  begin
    if (FMaxLength <= 0) or (UTF8Length(FText) < FMaxLength) then
    begin
      UTF8Insert(s, FText, FCursorPos + 1);
      Inc(FCursorPos);
      FSelStart := FCursorPos;
    end;
    consumed := True;
  end;

  if prevval <> FText then
    if Assigned(FOnChange) then
      FOnChange(self);

  if consumed then
    RePaint
  else
    inherited HandleKeyChar(AText, shiftstate, consumed);
end;

procedure TfpgAbstractEditCombo.HandleKeyPress(var keycode: word;
    var shiftstate: TShiftState; var consumed: boolean);
var
  hasChanged: boolean;
begin
  hasChanged := False;

  consumed := True;

  case keycode of
    keyBackSpace:
        begin
          if HasText then
            FocusItem:= 0;
          if FCursorPos > 0 then
          begin
            UTF8Delete(FText, FCursorPos, 1);
            Dec(FCursorPos);
            hasChanged := True;
          end;// backspace
        end;

    keyDelete:
        begin
          if HasText then
            FocusItem:= 0;
          FSelectedItem:= -1;
          hasChanged := True;
        end;

    keyReturn:
        begin
          if FSelectedItem > -1 then
            FText:= Items[FSelectedItem];
        end;

    else
    begin
      Consumed := False;
    end;
  end;

  if Consumed then
  begin
    FSelStart  := FCursorPos;
    FSelOffset := 0;
  end;

  if consumed then
    RePaint
  else
    inherited;

  if hasChanged then
    if Assigned(FOnChange) then
      FOnChange(self);
end;

procedure TfpgAbstractEditCombo.HandleLMouseDown(x, y: integer;
    shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  // button state is down only if user clicked in the button rectangle.
  FBtnPressed := PtInRect(FInternalBtnRect, Point(x, y));
  if not FAutoCompletion then
  begin
    PaintInternalButton;
    DoDropDown;
  end
  else if FBtnPressed then
    begin
      PaintInternalButton;
      DoDropDown;
    end;
end;

procedure TfpgAbstractEditCombo.HandleLMouseUp(x, y: integer;
    shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  FBtnPressed := False;
  PaintInternalButton;
end;

procedure TfpgAbstractEditCombo.HandleResize(awidth, aheight: TfpgCoord);
begin
  inherited HandleResize(awidth, aheight);
  CalculateInternalButtonRect;
end;

procedure TfpgAbstractEditCombo.HandlePaint;
var
  r: TfpgRect;
  tw, tw2, st, len: integer;

  // paint selection rectangle
  procedure DrawSelection;
  var
    lcolor: TfpgColor;
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
    tw  := FFont.TextWidth(UTF8Copy(Items[FSelectedItem], 1, st));
    tw2 := FFont.TextWidth(UTF8Copy(Items[FSelectedItem], 1, st + len));
    Canvas.XORFillRectangle(fpgColorToRGB(lcolor) xor $FFFFFF,
      -FDrawOffset + FMargin + tw, 3, tw2 - tw, FFont.Height);
  end;

begin
  Canvas.BeginDraw;
//  inherited HandlePaint;
  Canvas.ClearClipRect;
  r.SetRect(0, 0, Width, Height);
  Canvas.DrawControlFrame(r);

  // internal background rectangle (without frame)
  InflateRect(r, -2, -2);
  Canvas.SetClipRect(r);

  // paint the fake dropdown button
  PaintInternalButton;

  Dec(r.Width, FInternalBtnRect.Width);
  Canvas.SetClipRect(r);
  Canvas.SetFont(Font);

  if not AutoCompletion then
    if Focused then
    begin
      Canvas.SetColor(clSelection);
      Canvas.SetTextColor(clSelectionText);
      InflateRect(r, -1, -1);
    end
    else
    begin
      if Enabled then
        Canvas.SetColor(FBackgroundColor)
      else
        Canvas.SetColor(clWindowBackground);
      Canvas.SetTextColor(FTextColor);
    end
  else
  begin
    if Enabled then
      Canvas.SetColor(FBackgroundColor)
    else
      Canvas.SetColor(clWindowBackground);
    Canvas.SetTextColor(FTextColor);
  end;
  Canvas.FillRectangle(r);

  // Draw select item's text
  if not AutoCompletion then
  begin
    if HasText then
      fpgStyle.DrawString(Canvas, FMargin+1, FMargin, Text, Enabled);
  end
  else
  begin
    if HasText then
    begin
      FSelOffset:= 0;
      fpgStyle.DrawString(Canvas, FMargin+1, FMargin, Text, Enabled);
    end
    else
      if Text<> '' then
        if FSelectedItem> -1 then
        begin
          FSelOffset:= FFont.TextWidth(UTF8Copy(Items[FSelectedItem], UTF8Length(FText) + 1,
            UTF8Length(Items[FSelectedItem]) - UTF8Length(FText)));
          fpgStyle.DrawString(Canvas, FMargin+1, FMargin, FText + UTF8Copy(Items[FSelectedItem],
            UTF8Length(FText) + 1, UTF8Length(Items[FSelectedItem]) - UTF8Length(FText)), Enabled);
        end
        else
        begin
          FSelOffset:= 0;
          fpgStyle.DrawString(Canvas, FMargin+1, FMargin, FText, Enabled);
        end;

    if Focused then
    begin
      // drawing selection
      if FSelOffset <> 0 then
        DrawSelection;

      // drawing cursor
      FCursorPos:= UTF8Length(FText);
      tw := FFont.TextWidth(UTF8Copy(FText, 1, FCursorPos));
      fpgCaret.SetCaret(Canvas, -FDrawOffset + FMargin + tw, 3, fpgCaret.Width, FFont.Height);
    end
    else
      fpgCaret.UnSetCaret(Canvas);
  end;

  Canvas.EndDraw;
end;

procedure TfpgAbstractEditCombo.PaintInternalButton;
var
  ar: TfpgRect;
  btnflags: TFButtonFlags;
begin
  Canvas.BeginDraw;
  btnflags := [];
  ar := FInternalBtnRect;
  InflateRect(ar, -2, -2);
  if FBtnPressed then
  begin
    Include(btnflags, btnIsPressed);
    OffsetRect(ar, 1, 1);
  end;
  // paint button face
  fpgStyle.DrawButtonFace(Canvas,
      FInternalBtnRect.Left,
      FInternalBtnRect.Top,
      FInternalBtnRect.Width,
      FInternalBtnRect.Height, btnflags);
  if Enabled then
    Canvas.SetColor(clText1)
  else
  begin
    Canvas.SetColor(clShadow1);
  end;
  // paint arrow
  fpgStyle.DrawDirectionArrow(Canvas, ar.Left, ar.Top, ar.Width, ar.Height, 1);
  Canvas.EndDraw(FInternalBtnRect);
end;

constructor TfpgAbstractEditCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont             := fpgGetFont('#List');
  FBackgroundColor  := clBoxColor;
  FTextColor        := clText1;
  FDropDownCount    := 8;
  FWidth            := 120;
  FHeight           := FFont.Height + 6;
  FFocusItem        := 0; // nothing is selected
  FMargin           := 3;
  FFocusable        := True;
  FBtnPressed       := False;
  FAutocompletion := False;

  FText             := '';
  FCursorPos        := UTF8Length(FText);
  FSelStart         := FCursorPos;
  FSelOffset        := 0;
  FDrawOffset       := 0;
  FSelectedItem     := 0;

  FItems  := TStringList.Create;
  CalculateInternalButtonRect;

  FOnChange := nil;
end;

destructor TfpgAbstractEditCombo.Destroy;
begin
  FDropDown.Free;
  FItems.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TfpgAbstractEditCombo.Update;
begin
  FFocusItem := 0;
  Repaint;
end;

end.
