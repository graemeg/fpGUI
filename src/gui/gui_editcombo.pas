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
  fpgfx,
  gfx_popupwindow,
  gui_combobox;

type
  TAllowNew = (anNo, anYes, anAsk);


  TfpgAbstractEditCombo = class(TfpgBaseComboBox)
  private
    FAutoCompletion: Boolean;
    FAutoDropDown: Boolean;
    FAllowNew: TAllowNew;
    FText: string;
    FSelectedItem: integer;
    FMaxLength: integer;
    FNewItem: boolean;
    procedure   SetAllowNew(const AValue: TAllowNew);
    procedure   InternalBtnClick(Sender: TObject);
    procedure   InternalListBoxSelect(Sender: TObject);
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
    procedure   DoDropDown; override;
    function    GetText: string; virtual;
    function    HasText: boolean; virtual;
    procedure   SetText(const AValue: string); virtual;
    procedure   SetHeight(const AValue: TfpgCoord); override;
    procedure   SetWidth(const AValue: TfpgCoord); override;
    procedure   HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: Boolean); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: Boolean); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
    procedure   HandlePaint; override;
    procedure   PaintInternalButton; virtual;
    property    AutoCompletion: Boolean read FAutocompletion write FAutoCompletion default False;
    property    AutoDropDown: Boolean read FAutoDropDown write FAutoDropDown default False;
    property    AllowNew: TAllowNew read FAllowNew write SetAllowNew default anNo;
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor default clBoxColor;
    property    TextColor: TfpgColor read FTextColor write SetTextColor default clText1;
    property    Text: string read GetText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Update;
    property    NewText: boolean read FNewItem;
    property    OnKeyPress;
  end;


  TfpgEditCombo = class(TfpgAbstractEditCombo)
  published
    property    AutoCompletion;
    property    AutoDropDown;
    property    AllowNew;
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
    property    OnCloseUp;
    property    OnDropDown;
  end;


function CreateEditCombo(AOwner: TComponent; x, y, w: TfpgCoord; AList:TStringList;
      h: TfpgCoord = 0): TfpgEditCombo;


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
    procedure   HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: Boolean); override;
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

procedure TDropDownWindow.HandleKeyChar(var AText: TfpgChar;
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
//  if Assigned(FocusRootWidget) then
//    FocusRootWidget.ReleaseMouse;  // for internal ListBox

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

function CreateEditCombo(AOwner: TComponent; x, y, w: TfpgCoord; AList: TStringList;
      h: TfpgCoord = 0): TfpgEditCombo;
begin
  Result           := TfpgEditCombo.Create(AOwner);
  Result.Left      := x;
  Result.Top       := y;
  Result.Width     := w;
  Result.Focusable := True;
  if h < TfpgEditCombo(Result).Font.Height + 6 then
    Result.Height:= TfpgEditCombo(Result).Font.Height + 6
  else
    Result.Height:= h;

  if Assigned(AList) then
    Result.Items.Assign(AList);
end;

{ TfpgAbstractEditCombo }

procedure TfpgAbstractEditCombo.SetAllowNew(const AValue: TAllowNew);
begin
  if FAllowNew <> AValue then
    FAllowNew:= AValue;
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
    else
      if FText <> '' then
        if FSelectedItem < -1 then
        begin
          if Assigned(FDropDown) then
            FDropDown.Close;
          inc(FSelectedItem);      // with FSelectedItem set to -2 for delete key and -4 for return key
        end
        else
        begin
          FSelectedItem := -1;
          for i := 0 to FItems.Count - 1 do
          begin
            if SameText(UTF8Copy(FItems.Strings[i], 1, UTF8Length(FText)), FText) then
            begin
              FSelectedItem := i;
              if AutoDropDown then
                DoDropDown;
              Break;
            end;
          end;
          case FAllowNew of
            anNo:
              if FSelectedItem= -1 then
              begin
                UTF8Delete(FText, FCursorPos, 1);
                Dec(FCursorPos);
              end;
            anAsk,anYes:
              if FSelectedItem= -1 then
              begin
                FNewItem:= True;
              end;
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
  Result := FFocusItem > 0;
end;

procedure TfpgAbstractEditCombo.DoDropDown;
var
  ddw: TDropDownWindow;
  rowcount, i: integer;
  r: TfpgRect;
begin
  if (not Assigned(FDropDown)) or (not FDropDown.HasHandle) then
  begin
    FreeAndNil(FDropDown);
    OriginalFocusRoot := FocusRootWidget;
    FDropDown := TDropDownWindow.Create(nil);
    ddw := TDropDownWindow(FDropDown);
    ddw.Width := Width;
    ddw.CallerWidget := self;
    ddw.ListBox.OnSelect := @InternalListBoxSelect;

    // Assign combobox text items to internal listbox
    if FAutoCompletion then
    begin
      for i := 0 to FItems.Count - 1 do
        if SameText(UTF8Copy(FItems.Strings[i], 1, UTF8Length(FText)), FText) then
          ddw.ListBox.Items.Add(FItems.Strings[i]);
    end
    else
      ddw.ListBox.Items.Assign(FItems);

    // adjust the height of the dropdown
    rowcount := ddw.ListBox.Items.Count;
    if rowcount > DropDownCount then
      rowcount := DropDownCount;
    if rowcount < 1 then
      rowcount := 1;
    ddw.Height := (ddw.ListBox.RowHeight * rowcount) + 4;
    ddw.ListBox.Height := ddw.Height;   // needed in follow focus, otherwise, the default value (80) is used

    // set default focusitem
    ddw.ListBox.FocusItem := FFocusItem;

    ddw.DontCloseWidget := self;  // now we can control when the popup window closes
    r := GetDropDownPos(Parent, self, ddw);
    ddw.Height := r.Height;

    if (FItems.Count > 0) and Assigned(OnDropDown) then
      OnDropDown(self);
    ddw.OnClose := @InternalOnClose;

    ddw.ShowAt(Parent, r.Left, r.Top);
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
  i: Integer;
begin
  for i := 0 to Items.Count-1 do
  begin
    // Items is 0-based and FocusItem is 1-based
    if Items[i]= TDropDownWindow(FDropDown).ListBox.Items[TDropDownWindow(FDropDown).ListBox.FocusItem-1] then
    begin
      FocusItem := i+1;
      Break;
    end;
  end;
  FDropDown.Close;

  if HasHandle then
    Repaint;
end;

procedure TfpgAbstractEditCombo.SetText(const AValue: string);
var
  i: integer;
begin
  if AValue = '' then
  begin
    FText:= '';
    FocusItem := 0;  // nothing selected
  end
  else
  begin
    for i := 0 to Items.Count - 1 do
    begin
      if SameText(UTF8Copy(Items.Strings[i], 1, UTF8Length(AVAlue)), AValue) then
      begin
        FocusItem := i+1; // our FocusItem is 1-based. TStringList is 0-based.
        Exit;
      end;
    end;
    // if we get here, we didn't find a match
    FocusItem := 0;
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

procedure TfpgAbstractEditCombo.HandleKeyChar(var AText: TfpgChar;
    var shiftstate: TShiftState; var consumed: Boolean);
var
  s: string;
  prevval: string;
begin
  prevval   := FText;
  s         := AText;
  consumed  := False;

  // Handle only printable characters
  // Note: This is not UTF-8 compliant!
  if (Ord(AText[1]) > 31) and (Ord(AText[1]) < 127) or (Length(AText) > 1) then
  begin
    if (FMaxLength <= 0) or (UTF8Length(FText) < FMaxLength) then
    begin
      UTF8Insert(s, FText, FCursorPos + UTF8Length(s));
      Inc(FCursorPos);
      FSelStart := FCursorPos;
    end;
    consumed := True;
  end;

  if prevval <> FText then
    DoOnChange;

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
            FocusItem := 0;
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
            FocusItem := 0;
          FSelectedItem := -2;      // detects delete has been pressed
          hasChanged := True;
        end;

    keyReturn, keyPEnter:
        begin
          if FSelectedItem > -1 then
            SetText(Items[FSelectedItem])
          else
            SetText(FText);
          FSelectedItem:= -4;      // detects return has been pressed (must be 4 due to number of repaints)
          if FNewItem and (FAllowNew = anYes) then
            FItems.Add(FText);
          if Assigned(FDropDown) then
            FDropDown.Close;
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

  if consumed and hasChanged then
    RePaint;

  if hasChanged then
    DoOnChange;
  inherited HandleKeyPress(keycode, shiftstate, consumed);
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
  Texte: string;

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
    tw  := Font.TextWidth(UTF8Copy(Items[FSelectedItem], 1, st));
    tw2 := Font.TextWidth(UTF8Copy(Items[FSelectedItem], 1, st + len));
    Canvas.XORFillRectangle(fpgColorToRGB(lcolor) xor $FFFFFF,
      -FDrawOffset + FMargin + tw, 3, tw2 - tw, Font.Height);
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
    begin
      Texte:= Text;
      if Texte <> '' then
        if FSelectedItem > -1 then
        begin
          FSelOffset:= Font.TextWidth(UTF8Copy(Items[FSelectedItem], UTF8Length(FText) + 1,
            UTF8Length(Items[FSelectedItem]) - UTF8Length(FText)));
          fpgStyle.DrawString(Canvas, FMargin+1, FMargin, FText + UTF8Copy(Items[FSelectedItem],
            UTF8Length(FText) + 1, UTF8Length(Items[FSelectedItem]) - UTF8Length(FText)), Enabled);
        end
        else
        begin
          FSelOffset:= 0;
          fpgStyle.DrawString(Canvas, FMargin+1, FMargin, FText, Enabled);
        end;
    end;

    if Focused then
    begin
      // drawing selection
      if FSelOffset <> 0 then
        DrawSelection;

      // drawing cursor
      FCursorPos:= UTF8Length(FText);
      tw := Font.TextWidth(UTF8Copy(FText, 1, FCursorPos));
      fpgCaret.SetCaret(Canvas, -FDrawOffset + FMargin + tw, 3, fpgCaret.Width, Font.Height);
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
  FBackgroundColor  := clBoxColor;
  FTextColor        := Parent.TextColor;
  FWidth            := 120;
  FHeight           := Font.Height + 6;
  FMargin           := 3;
  FFocusable        := True;
  FBtnPressed       := False;
  FAutocompletion   := False;
  AutoDropDown      := False;

  FText             := '';
  FCursorPos        := UTF8Length(FText);
  FSelStart         := FCursorPos;
  FSelOffset        := 0;
  FDrawOffset       := 0;
  FSelectedItem     := -1;       // to allow typing if list is empty
  FNewItem          := False;

  CalculateInternalButtonRect;
end;

destructor TfpgAbstractEditCombo.Destroy;
begin
  FDropDown.Free;
  inherited Destroy;
end;

procedure TfpgAbstractEditCombo.Update;
begin
  FFocusItem := 0;
  Repaint;
end;

end.
