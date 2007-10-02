{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
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

{
  TODO:
    * When combobox Items changes, the combobox needs to refresh. We need a
      custom StringItems class to notify us of changes. See TfpgListBox for
      an example.
    * Implement .BeginUpdate and .EndUpdate methods so we know when to refresh
      the items list.

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

  TfpgCustomComboBox = class(TfpgWidget)
  private
    FDropDownCount: integer;
    FDropDown: TfpgPopupWindow;
    FBackgroundColor: TfpgColor;
    FFocusItem: integer;
    FFont: TfpgFont;
    FInternalBtnRect: TfpgRect;
    FBtnPressed: Boolean;
    FItems: TStringList;
    FOnChange: TNotifyEvent;
    function    GetFontDesc: string;
    function    GetText: string;
    procedure   SetBackgroundColor(const AValue: TfpgColor);
    procedure   SetDropDownCount(const AValue: integer);
    procedure   DoDropDown;
    procedure   InternalBtnClick(Sender: TObject);
    procedure   InternalListBoxSelect(Sender: TObject);
    procedure   SetFocusItem(const AValue: integer);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetText(const AValue: string);
    procedure   CalculateInternalButtonRect;
  protected
    FMargin: integer;
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
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
    property    Text: string read GetText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Update;
    property    Font: TfpgFont read FFont;
  end;


  TfpgComboBox = class(TfpgCustomComboBox)
  published
    property    BackgroundColor;
    property    DropDownCount;
    property    FocusItem;
    property    FontDesc;
    property    Items;
    property    Text;
    property    Width;
    property    Height;
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
    destructor  Destroy; override;
    ListBox:    TfpgListBox;
    property    CallerWidget: TfpgWidget read FCallerWidget write FCallerWidget;
  end;

{ TDropDownWindow }

procedure TDropDownWindow.HandlePaint;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;
  Canvas.Clear(clWhite);
  Canvas.EndDraw;
end;

procedure TDropDownWindow.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);
  if keycode = keyEscape then
  begin
    Close;
    consumed := True;
  end;
end;

procedure TDropDownWindow.HandleShow;
begin
  FocusRootWidget := ListBox;

  ListBox.Left := 0;
  ListBox.Top  := 0;
  ListBox.Width := Width;
  ListBox.Height := Height;

  inherited HandleShow;
//  CaptureMouse;
end;

procedure TDropDownWindow.HandleHide;
begin
  FocusRootWidget := OriginalFocusRoot;
  OriginalFocusRoot := nil;
  inherited HandleHide;
  if Assigned(FocusRootWidget) then
    FocusRootWidget.SetFocus;
end;

constructor TDropDownWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  WindowType        := wtPopup;
//  WindowAttributes  := [];
//  WindowPosition    := wpUser;

  ListBox := TfpgListBox.Create(self);
  ListBox.PopupFrame := True;
end;

destructor TDropDownWindow.Destroy;
begin
//  ReleaseMouse;
  inherited Destroy;
end;
  

function CreateComboBox(AOwner: TComponent; x, y, w: TfpgCoord; AList: TStringList): TfpgComboBox;
begin
  Result           := TfpgComboBox.Create(AOwner);
  Result.Left      := x;
  Result.Top       := y;
  Result.Width     := w;
  Result.Focusable := True;

  Result.Height := 23;  // replace this with font height + margins
  {$Note We still need to handle the AList param as well.}
end;

{ TfpgCustomComboBox }

procedure TfpgCustomComboBox.SetDropDownCount(const AValue: integer);
begin
  if FDropDownCount = AValue then
    Exit;
  FDropDownCount := AValue;
end;

procedure TfpgCustomComboBox.SetBackgroundColor(const AValue: TfpgColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    Repaint;
  end;
end;

function TfpgCustomComboBox.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

function TfpgCustomComboBox.GetText: string;
begin
  if (FocusItem > 0) and (FocusItem <= FItems.Count) then
    Result := FItems.Strings[FocusItem-1]
  else
    Result := '';
end;

procedure TfpgCustomComboBox.DoDropDown;
var
  ddw: TDropDownWindow;
  rowcount: integer;
begin
  if (not Assigned(FDropDown)) or (not FDropDown.HasHandle) then
  begin
    OriginalFocusRoot := FocusRootWidget;
    FDropDown     := TDropDownWindow.Create(nil);
    ddw           := TDropDownWindow(FDropDown);
    ddw.Width     := Width;
    // adjust the height of the dropdown
    rowcount := FItems.Count;
    if rowcount > FDropDownCount then
      rowcount := FDropDownCount;
    if rowcount < 1 then
      rowcount := 1;
    ddw.Height    := (ddw.ListBox.RowHeight * rowcount) + 4;
    ddw.CallerWidget := self;
    ddw.ListBox.OnSelect := @InternalListBoxSelect;

    // Assign combobox text items to internal listbox and set default focusitem
    ddw.ListBox.Items.Assign(FItems);
    ddw.ListBox.FocusItem := FFocusItem;

    FDropDown.ShowAt(Parent, Left, Top+Height);
    ddw.ListBox.SetFocus;
  end
  else
  begin
    FBtnPressed := False;
    FDropDown.Close;
    FreeAndNil(FDropDown);
  end;
end;

procedure TfpgCustomComboBox.InternalBtnClick(Sender: TObject);
begin
  DoDropDown;
end;

procedure TfpgCustomComboBox.InternalListBoxSelect(Sender: TObject);
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
procedure TfpgCustomComboBox.SetFocusItem(const AValue: integer);
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

procedure TfpgCustomComboBox.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  RePaint;
end;

procedure TfpgCustomComboBox.SetText(const AValue: string);
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
        SetFocusItem(i+1);
        Break;
      end;
    end;
    // if we get here, we didn't find a match
    SetFocusItem(0);
  end;
end;

procedure TfpgCustomComboBox.SetWidth(const AValue: TfpgCoord);
begin
  inherited;
  CalculateInternalButtonRect;
  RePaint;
end;

procedure TfpgCustomComboBox.CalculateInternalButtonRect;
begin
  FInternalBtnRect.SetRect(Width - Min(Height, 20), 2, Min(Height, 20)-2, Height-4);
end;

procedure TfpgCustomComboBox.SetHeight(const AValue: TfpgCoord);
begin
  inherited;
  CalculateInternalButtonRect;
  RePaint;
end;

procedure TfpgCustomComboBox.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  // botton down only if user clicked on the button.
  if PtInRect(FInternalBtnRect, Point(x, y)) then
    FBtnPressed := True;
  PaintInternalButton;
end;

procedure TfpgCustomComboBox.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  FBtnPressed := False;
  DoDropDown;
  PaintInternalButton;
end;

procedure TfpgCustomComboBox.HandleResize(awidth, aheight: TfpgCoord);
begin
  inherited HandleResize(awidth, aheight);
  CalculateInternalButtonRect;
end;

procedure TfpgCustomComboBox.HandlePaint;
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
    Canvas.SetTextColor(clText1);
  end;
  Canvas.FillRectangle(r);

  // Draw select item's text
  if FocusItem > 0 then
    fpgStyle.DrawString(Canvas, FMargin+1, FMargin, Text, Enabled);

  Canvas.EndDraw;
end;

procedure TfpgCustomComboBox.PaintInternalButton;
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

constructor TfpgCustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackgroundColor  := clBoxColor;
  FDropDownCount    := 8;
  FWidth            := 120;
  FHeight           := 23;
  FFocusItem        := 0; // nothing is selected
  FMargin           := 3;
  FFocusable        := True;
  FBtnPressed       := False;
  
  FFont   := fpgGetFont('#List');
  FItems  := TStringList.Create;
  CalculateInternalButtonRect;

  FOnChange := nil;
end;

destructor TfpgCustomComboBox.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TfpgCustomComboBox.Update;
begin
  FFocusItem := 1;
  Repaint;
end;

end.

