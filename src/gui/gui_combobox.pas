unit gui_combobox;

{$mode objfpc}{$H+}

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

  { TfpgCustomComboBox }

  TfpgCustomComboBox = class(TfpgWidget)
  private
    FDropDownCount: integer;
    FDropDown: TfpgPopupWindow;
    FBackgroundColor: TfpgColor;
    FFocusItem: integer;
    FFont: TfpgFont;
    FInternalBtn: TfpgButton;
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
  protected
    FMargin: integer;
    procedure   SetEnabled(const AValue: boolean); override;
    property    DropDownCount: integer read FDropDownCount write SetDropDownCount default 8;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandlePaint; override;
    property    Items: TStringList read FItems;    {$Note Make this read/write }
    property    FocusItem: integer read FFocusItem write SetFocusItem;
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
    property    Text: string read GetText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterConstruction; override;
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
    property    OnChange;
  end;
  

function CreateComboBox(AOwner: TComponent; x, y, w: TfpgCoord; AList: TStringList): TfpgComboBox;


implementation

uses
  gui_listbox;
  
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

procedure TfpgCustomComboBox.SetEnabled(const AValue: boolean);
begin
  inherited SetEnabled(AValue);
  FInternalBtn.Enabled := AValue;
end;

procedure TfpgCustomComboBox.HandleLMouseUp(x, y: integer;
  shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  DoDropDown;
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

//  fpgStyle.DrawButtonFace(canvas, width - min(height, 20)-3, 2, height-4, height-4, [btnIsEmbedded]);
//  fpgStyle.DrawDirectionArrow(canvas, width - height + 1, 1, height-2, height-2, 1);
  Canvas.SetFont(Font);

  if Focused then
  begin
    Canvas.SetColor(clSelection);
    Canvas.SetTextColor(clSelectionText);
    r.Width := r.Width - FInternalBtn.Width;
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

constructor TfpgCustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackgroundColor  := clBoxColor;
  FDropDownCount    := 8;
  FWidth            := 120;
  FHeight           := 23;
  FFocusItem        := 0; // nothing is selected
  FMargin           := 3;
  
  FFont   := fpgGetFont('#List');
  FItems  := TStringList.Create;
  
  FOnChange := nil;
end;

destructor TfpgCustomComboBox.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TfpgCustomComboBox.AfterConstruction;
begin
  inherited AfterConstruction;

  if not Assigned(FInternalBtn) then
  begin
    FInternalBtn           := CreateButton(self, (Width-20), 2, 18, '', @InternalBtnClick);
    FInternalBtn.Height    := 19;
    FInternalBtn.Embedded  := True;
    FInternalBtn.Parent    := self;
    FInternalBtn.ImageName := 'sys.sb.down';
    FInternalBtn.ShowImage := True;
    FInternalBtn.Anchors   := [anRight, anTop];
  end;
end;

end.

