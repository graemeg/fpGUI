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

  TfpgBaseComboBox = class(TfpgWidget)
  private
    FDropDownCount: integer;
    FFont: TfpgFont;
    FOnChange: TNotifyEvent;
    function    GetFontDesc: string;
    procedure   SetDropDownCount(const AValue: integer);
    procedure   SetFocusItem(const AValue: integer);
    procedure   SetFontDesc(const AValue: string);
  protected
    FFocusItem: integer;
    FItems: TStringList;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   DoOnChange;
    procedure   DoDropDown; virtual; abstract;
    function    GetDropDownPos(AParent, AComboBox, ADropDown: TfpgWidget): TfpgRect; virtual;
    property    DropDownCount: integer read FDropDownCount write SetDropDownCount default 8;
    property    FocusItem: integer read FFocusItem write SetFocusItem;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    Items: TStringList read FItems;    {$Note Make this read/write }
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Font: TfpgFont read FFont;
  end;
  

  TfpgAbstractComboBox = class(TfpgBaseComboBox)
  private
    FInternalBtnRect: TfpgRect;
    procedure   InternalBtnClick(Sender: TObject);
    procedure   SetFocusItem(const AValue: integer);
    procedure   CalculateInternalButtonRect;
  protected
    FMargin: integer;
    FBtnPressed: Boolean;
    FDropDown: TfpgPopupWindow;
    procedure   DoDropDown; override;
    function    GetText: string; virtual;
    function    HasText: boolean; virtual;
    procedure   SetText(const AValue: string); virtual;
    procedure   SetHeight(const AValue: TfpgCoord); override;
    procedure   SetWidth(const AValue: TfpgCoord); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
    procedure   HandlePaint; override;
    procedure   PaintInternalButton; virtual;
    property    Text: string read GetText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Update;
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
  

function CreateComboBox(AOwner: TComponent; x, y, w: TfpgCoord; AList: TStringList;
      h: TfpgCoord = 0): TfpgComboBox;


implementation

uses
  gui_listbox,
  math;
  
var
  OriginalFocusRoot: TfpgWidget;

type
  { This is the class representing the dropdown window of the combo box. }
  TComboboxDropdownWindow = class(TfpgPopupWindow)
  private
    FCallerWidget: TfpgAbstractComboBox;
    FListBox: TfpgListBox;
    procedure   SetFirstItem;
  protected
    procedure   ListBoxSelect(Sender: TObject);
    procedure   HandleShow; override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
  public
    constructor Create(AOwner: TComponent; ACallerWidget: TfpgAbstractComboBox); reintroduce;
    property    ListBox: TfpgListBox read FListBox;
  end;


{ TfpgBaseComboBox }

procedure TfpgBaseComboBox.SetDropDownCount(const AValue: integer);
begin
  if FDropDownCount = AValue then
    Exit;
  FDropDownCount := AValue;
end;

function TfpgBaseComboBox.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

{ Focusitem is 1 based and NOT 0 based like the Delphi ItemIndex property.
  So at startup, FocusItem = 0 which means nothing is selected. If FocusItem = 1
  it means the first item is selected etc. }
procedure TfpgBaseComboBox.SetFocusItem(const AValue: integer);
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
  DoOnChange;
end;

procedure TfpgBaseComboBox.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  if Height < FFont.Height + 6 then
    Height:= FFont.Height + 6;
  RePaint;
end;

procedure TfpgBaseComboBox.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);
  if not consumed then
  begin
    case keycode of
      keyDown:
        begin
          if (shiftstate = [ssAlt]) then
            DoDropDown
          else
          begin
            FocusItem := FocusItem + 1;
            consumed := True;
          end;
        end;

      keyUp:
        begin
          FocusItem := FocusItem - 1;
          consumed := True;
        end;
    end;  { case }
  end;  { if }
end;

procedure TfpgBaseComboBox.DoOnChange;
begin
  if Assigned(OnChange) then
    FOnChange(self);
end;

function TfpgBaseComboBox.GetDropDownPos(AParent, AComboBox, ADropDown: TfpgWidget): TfpgRect;
var
  pt: TPoint;
begin
  // translate ComboBox coordinates
  pt := WindowToScreen(AParent, Point(AComboBox.Left, AComboBox.Bottom));

  // dropdown will not fit below combobox so we place it above
  if (pt.y + ADropDown.Height) > fpgApplication.ScreenHeight then
    Result.Top := AComboBox.Top - ADropDown.Height
  else
    Result.Top := AComboBox.Bottom;

  // dropdown height doesn't fit in screen height so shrink it
  if (ADropDown.Height > fpgApplication.ScreenHeight) then
  begin
    // 50 is just some spacing for taskbars (top or bottom aligned)
    Result.Top    := AComboBox.Top - pt.y + 50;
    Result.Height := fpgApplication.ScreenHeight - 50;
  end
  else
    Result.Height := ADropDown.Height;

  Result.Left   := AComboBox.Left;
  Result.Width  := ADropDown.Width;

//  writeln('H:', fpgApplication.ScreenHeight, '  W:', fpgApplication.ScreenWidth);
//  writeln('Point x:', pt.x, '  y:', pt.y);
//  PrintRect(Result);
end;

constructor TfpgBaseComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDropDownCount := 8;
  FFocusItem := 0; // nothing is selected
  FItems  := TStringList.Create;
  FFont   := fpgGetFont('#List');
  FOnChange := nil;
end;

destructor TfpgBaseComboBox.Destroy;
begin
  FFont.Free;
  FItems.Free;
  inherited Destroy;
end;


{ TComboboxDropdownWindow }

procedure TComboboxDropdownWindow.SetFirstItem;
var
  i: integer;
begin
  // If FocusItem is less than DropDownCount FirsItem = 1
  if ListBox.FocusItem <= FCallerWidget.DropDownCount then
    ListBox.SetFirstItem(1)
  // If FocusItem is in the last DropDownCount of items
  else if (ListBox.ItemCount - ListBox.FocusItem) < FCallerWidget.DropDownCount then
    ListBox.SetFirstItem(ListBox.ItemCount - FCallerWidget.DropDownCount+1)
  else
  // Try and centre FocusItem in the drow down window
    ListBox.SetFirstItem(ListBox.FocusItem - (FCallerWidget.DropDownCount div 2));
end;

procedure TComboboxDropdownWindow.ListBoxSelect(Sender: TObject);
begin
  FCallerWidget.FocusItem := ListBox.FocusItem;
  Close;
end;

procedure TComboboxDropdownWindow.HandleShow;
begin
  ListBox.SetPosition(0, 0, Width, Height);
  inherited HandleShow;
  SetFirstItem;
  ListBox.SetFocus;
end;

procedure TComboboxDropdownWindow.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);
  if KeyCode = keyEscape then
  begin
    Close;
  end
end;

constructor TComboboxDropdownWindow.Create(AOwner: TComponent; ACallerWidget: TfpgAbstractComboBox);
begin
  inherited Create(nil);
  Name := '_ComboboxDropdownWindow';
  if not Assigned(ACallerWidget) then
    raise Exception.Create('ACallerWidget may not be <nil>');
  FCallerWidget := ACallerWidget;

  FListBox := CreateListBox(self, 0, 0, 80, 100);
  FListBox.PopupFrame := True;
  FListBox.Items.Assign(FCallerWidget.Items);
  FListBox.FocusItem := FCallerWidget.FocusItem;
  FListBox.OnSelect := @ListBoxSelect;
end;



function CreateComboBox(AOwner: TComponent; x, y, w: TfpgCoord; AList: TStringList;
      h: TfpgCoord = 0): TfpgComboBox;
begin
  Result           := TfpgComboBox.Create(AOwner);
  Result.Left      := x;
  Result.Top       := y;
  Result.Width     := w;
  Result.Focusable := True;

  if h < TfpgComboBox(Result).FFont.Height + 6 then
    Result.Height:= TfpgComboBox(Result).FFont.Height + 6
  else
    Result.Height:= h;

  if Assigned(AList) then
    Result.Items.Assign(AList);
end;

{ TfpgAbstractComboBox }

function TfpgAbstractComboBox.GetText: string;
begin
  if (FocusItem > 0) and (FocusItem <= FItems.Count) then
    Result := Items.Strings[FocusItem-1]
  else
    Result := '';
end;

function TfpgAbstractComboBox.HasText: boolean;
begin
  Result := FocusItem > 0;
end;

procedure TfpgAbstractComboBox.DoDropDown;
var
  ddw: TComboboxDropdownWindow;
  rowcount: integer;
  r: TfpgRect;
begin
  {$IFDEF DEBUG}
  write('DoDropDown');
  {$ENDIF}
  if (not Assigned(FDropDown)) or (not FDropDown.HasHandle) then
  begin
    {$IFDEF DEBUG}
    writeln('.... creating');
    {$ENDIF}
    FreeAndNil(FDropDown);
    OriginalFocusRoot := FocusRootWidget;

    FDropDown := TComboboxDropdownWindow.Create(nil, self);
    ddw := TComboboxDropdownWindow(FDropDown);

    // adjust the height of the dropdown
    rowcount := FItems.Count;
    if rowcount > FDropDownCount then
      rowcount := FDropDownCount;
    if rowcount < 1 then
      rowcount := 1;  // Even if empty at least show one line dropdown

    ddw.Width   := Width;
    ddw.Height  := (ddw.ListBox.RowHeight * rowcount) + 4;
    ddw.DontCloseWidget := self;  // now we can control when the popup window closes
    r := GetDropDownPos(Parent, self, ddw);  // find suitable position
    ddw.Height := r.Height;  // in case GetDropDownPos resized us
    ddw.ShowAt(Parent, r.Left, r.Top);
  end
  else
  begin
    {$IFDEF DEBUG}
    writeln('.... destroying');
    {$ENDIF}
    FBtnPressed := False;
    ddw := TComboboxDropdownWindow(FDropDown);
    ddw.Close;
    FreeAndNil(FDropDown);
  end;
end;

procedure TfpgAbstractComboBox.InternalBtnClick(Sender: TObject);
begin
  DoDropDown;
end;

procedure TfpgAbstractComboBox.SetFocusItem(const AValue: integer);
begin

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
      if SameText(Items.Strings[i], AValue) then
      begin
        SetFocusItem(i+1); // our FocusItem is 1-based. TStringList is 0-based.
        Exit;
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

procedure TfpgAbstractComboBox.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);
  if consumed then
    RePaint
end;

procedure TfpgAbstractComboBox.CalculateInternalButtonRect;
begin
  FInternalBtnRect.SetRect(Width - Min(Height, 20), 2, Min(Height, 20)-2, Height-4);
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
  FBackgroundColor  := clBoxColor;
  FTextColor        := Parent.TextColor;
  FWidth            := 120;
  FHeight           := Font.Height + 6;
  FMargin           := 3;
  FFocusable        := True;
  FBtnPressed       := False;

  CalculateInternalButtonRect;
end;

destructor TfpgAbstractComboBox.Destroy;
begin
  FDropDown.Free;
  inherited Destroy;
end;

procedure TfpgAbstractComboBox.Update;
begin
  FFocusItem := 0;
  Repaint;
end;

end.

