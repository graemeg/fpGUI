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
      Defines a ComboBox control. Also known as a Choice List control.
}

unit gui_combobox;

{$mode objfpc}{$H+}

{.$Define DEBUG}

{ TODO: When combobox Items changes, the combobox needs to refresh. We need a
      custom StringItems class to notify us of changes. See TfpgListBox for
      an example. }
      
{ TODO: Implement .BeginUpdate and .EndUpdate methods so we know when to refresh
      the items list. }

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

  TfpgAbstractComboBox = class(TfpgWidget)
  private
    FDropDownCount: integer;
    FFocusItem: integer;
    FFont: TfpgFont;
    FInternalBtnRect: TfpgRect;
    FItems: TStringList;
    FOnChange: TNotifyEvent;
    function    GetFontDesc: string;
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
    procedure   DoDropDown; virtual;
    function    GetText: string; virtual;
    function    HasText: boolean; virtual;
    procedure   SetText(const AValue: string); virtual;
    procedure   SetHeight(const AValue: TfpgCoord); override;
    procedure   SetWidth(const AValue: TfpgCoord); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
    procedure   HandlePaint; override;
    procedure   PaintInternalButton; virtual;
    property    DropDownCount: integer read FDropDownCount write SetDropDownCount default 8;
    property    Items: TStringList read FItems;    {$Note Make this read/write }
    // property is 1-based
    property    FocusItem: integer read FFocusItem write SetFocusItem;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
    property    Text: string read GetText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Update;
    property    Font: TfpgFont read FFont;
  end;


  TfpgComboBox = class(TfpgAbstractComboBox)
  published
    property    BackgroundColor default clBoxColor;
    property    DropDownCount;
    property    FocusItem;
    property    FontDesc;
    property    Height;
    property    Items;
    property    TabOrder;
    property    Text;
    property    TextColor;
    property    Width;
    property    OnChange;
  end;
  

function CreateComboBox(AOwner: TComponent; x, y, w: TfpgCoord; AList: TStringList): TfpgComboBox;


implementation

uses
  gui_listbox,
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

procedure TDropDownWindow.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);
  if keycode = keyEscape then
  begin
    consumed := True;
    Close;
  end;
end;

procedure TDropDownWindow.HandleShow;
begin
  ListBox.SetPosition(0, 0, Width, Height);
  inherited HandleShow;
  ActiveWidget := ListBox;
//  ActiveWidget.CaptureMouse;
end;

procedure TDropDownWindow.HandleHide;
begin
  // HandleHide also gets called in TfpgWidget.Destroy so we need a few
  // if Assigned() tests here. This should be improved on.
//  if Assigned(ActiveWidget) then
//    ActiveWidget.ReleaseMouse;
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

function CreateComboBox(AOwner: TComponent; x, y, w: TfpgCoord; AList: TStringList): TfpgComboBox;
begin
  Result           := TfpgComboBox.Create(AOwner);
  Result.Left      := x;
  Result.Top       := y;
  Result.Width     := w;
  Result.Focusable := True;

  Result.Height := 23;  // replace this with font height + margins
  { TODO : We still need to handle the AList param as well.}
end;

{ TfpgAbstractComboBox }

procedure TfpgAbstractComboBox.SetDropDownCount(const AValue: integer);
begin
  if FDropDownCount = AValue then
    Exit;
  FDropDownCount := AValue;
end;

function TfpgAbstractComboBox.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

function TfpgAbstractComboBox.GetText: string;
begin
  if (FocusItem > 0) and (FocusItem <= FItems.Count) then
    Result := FItems.Strings[FocusItem-1]
  else
    Result := '';
end;

function TfpgAbstractComboBox.HasText: boolean;
begin
  Result := FocusItem > 0;
end;

procedure TfpgAbstractComboBox.DoDropDown;
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
    ddw.ListBox.SetFocus;
  end
  else
  begin
    FBtnPressed := False;
    ddw := TDropDownWindow(FDropDown);
    ddw.Close;
    FreeAndNil(FDropDown);
  end;
end;

procedure TfpgAbstractComboBox.InternalBtnClick(Sender: TObject);
begin
  DoDropDown;
end;

procedure TfpgAbstractComboBox.InternalListBoxSelect(Sender: TObject);
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
  So at startup, FocusItem = 0 which means nothing is selected. If FocusItem = 1
  it means the first item is selected etc. }
procedure TfpgAbstractComboBox.SetFocusItem(const AValue: integer);
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

procedure TfpgAbstractComboBox.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  RePaint;
end;

procedure TfpgAbstractComboBox.SetText(const AValue: string);
var
  i: integer;
begin
  if AValue = '' then
    SetFocusItem(0)  // nothing selected
  else
  begin
    for i := 0 to FItems.Count - 1 do
    begin
      if SameText(FItems.Strings[i], AValue) then
      begin
        SetFocusItem(i+1); // our FocusItem is 1-based. TStringList is 0-based.
        Break;
      end;
    end;
    // if we get here, we didn't find a match
    SetFocusItem(0);
  end;
end;

procedure TfpgAbstractComboBox.SetWidth(const AValue: TfpgCoord);
begin
  inherited;
  CalculateInternalButtonRect;
  RePaint;
end;

procedure TfpgAbstractComboBox.CalculateInternalButtonRect;
begin
  FInternalBtnRect.SetRect(Width - Min(Height, 20), 2, Min(Height, 20)-2, Height-4);
end;

procedure TfpgAbstractComboBox.MsgPopupClose(var msg: TfpgMessageRec);
begin
  DoDropDown;
end;

procedure TfpgAbstractComboBox.SetHeight(const AValue: TfpgCoord);
begin
  inherited;
  CalculateInternalButtonRect;
  RePaint;
end;

procedure TfpgAbstractComboBox.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  // button state is down only if user clicked in the button rectangle.
  FBtnPressed := PtInRect(FInternalBtnRect, Point(x, y));
  PaintInternalButton;
  DoDropDown;
end;

procedure TfpgAbstractComboBox.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  FBtnPressed := False;
//  DoDropDown;
  PaintInternalButton;
end;

procedure TfpgAbstractComboBox.HandleResize(awidth, aheight: TfpgCoord);
begin
  inherited HandleResize(awidth, aheight);
  CalculateInternalButtonRect;
end;

procedure TfpgAbstractComboBox.HandlePaint;
var
  r: TfpgRect;
begin
  Canvas.BeginDraw;
//  inherited HandlePaint;
  Canvas.ClearClipRect;
  r.SetRect(0, 0, Width, Height);
  Canvas.DrawControlFrame(r);

  // internal background rectangle (without frame)
  InflateRect(r, -2, -2);
  Canvas.SetClipRect(r);

  if Enabled then
    Canvas.SetColor(FBackgroundColor)
  else
    Canvas.SetColor(clWindowBackground);

  Canvas.FillRectangle(r);

  // paint the fake dropdown button
  PaintInternalButton;

  Dec(r.Width, FInternalBtnRect.Width);
  Canvas.SetClipRect(r);
  Canvas.SetFont(Font);

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
  end;
  Canvas.FillRectangle(r);

  // Draw select item's text
  if HasText then
    fpgStyle.DrawString(Canvas, FMargin+1, FMargin, Text, Enabled);

  Canvas.EndDraw;
end;

procedure TfpgAbstractComboBox.PaintInternalButton;
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

constructor TfpgAbstractComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont   := fpgGetFont('#List');
  FBackgroundColor  := clBoxColor;
  FTextColor        := Parent.TextColor;
  FDropDownCount    := 8;
  FWidth            := 120;
  FHeight           := FFont.Height + 6;
  FFocusItem        := 0; // nothing is selected
  FMargin           := 3;
  FFocusable        := True;
  FBtnPressed       := False;

  FItems  := TStringList.Create;
  CalculateInternalButtonRect;

  FOnChange := nil;
end;

destructor TfpgAbstractComboBox.Destroy;
begin
  FDropDown.Free;
  FItems.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TfpgAbstractComboBox.Update;
begin
  FFocusItem := 0;
  Repaint;
end;

end.

