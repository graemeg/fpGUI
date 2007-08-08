unit gui_combobox;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gfx_widget,
  gui_form,
  gfxbase,
  gui_button,
  fpgfx;

type

  { TfpgCustomComboBox }

  TfpgCustomComboBox = class(TfpgWidget)
  private
    FDropDownCount: integer;
    FDropDown: TfpgForm;
    FBackgroundColor: TfpgColor;
    FFocusItem: integer;
    FFont: TfpgFont;
    FInternalBtn: TfpgButton;
    FItems: TStringList;
    FOnChange: TNotifyEvent;
    procedure   SetBackgroundColor(const AValue: TfpgColor);
    procedure   SetDropDownCount(const AValue: integer);
    procedure   DoDropDown;
    procedure   InternalBtnClick(Sender: TObject);
    procedure   InternalListBoxSelect(Sender: TObject);
    procedure   SetFocusItem(const AValue: integer);
  protected
    FMargin: integer;
    procedure   SetEnabled(const AValue: boolean); override;
    property    DropDownCount: integer read FDropDownCount write SetDropDownCount default 8;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandlePaint; override;
    property    Items: TStringList read FItems;
    property    FocusItem: integer read FFocusItem write SetFocusItem;
    property    Font: TfpgFont read FFont;
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor;
    function    Text: string;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterConstruction; override;
  end;


  TfpgComboBox = class(TfpgCustomComboBox)
  published
    property    DropDownCount;
    property    Items;
    property    FocusItem;
    property    BackgroundColor;
    property    OnChange;
  end;
  

function CreateComboBox(AOwner: TComponent; x, y, w: TfpgCoord; AList: TStringList): TfpgComboBox;


implementation

uses
  Math,
  gui_listbox;
  
var
  OriginalFocusRoot: TfpgWidget;

type
  // This is so we can access protected methods
  TPrivateWidget = class(TfpgWidget)
  end;
  
  { TDropDownWindow }

  TDropDownWindow = class(TfpgForm)
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
  CaptureMouse;
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
  WindowType        := wtPopup;
  WindowAttributes  := [];
  WindowPosition    := wpUser;

  ListBox := TfpgListBox.Create(self);
  ListBox.PopupFrame := True;
end;

destructor TDropDownWindow.Destroy;
begin
  ReleaseMouse;
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

procedure TfpgCustomComboBox.DoDropDown;
var
  pt: TPoint;
  ddw: TDropDownWindow;
  rowcount: integer;
begin
  if (not Assigned(FDropDown)) or (not FDropDown.HasHandle) then
  begin
    OriginalFocusRoot := FocusRootWidget;
    pt := WindowToScreen(Parent, Point(Left, Top+Height));
    FDropDown     := TDropDownWindow.Create(nil);
    ddw           := TDropDownWindow(FDropDown);
    ddw.Left      := pt.X;
    ddw.Top       := pt.Y;
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

    FDropDown.Show;
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

procedure TfpgCustomComboBox.SetFocusItem(const AValue: integer);
begin
  if FFocusItem = AValue then
    Exit; //==>
  FFocusItem := AValue;
end;

procedure TfpgCustomComboBox.SetEnabled(const AValue: boolean);
begin
  inherited SetEnabled(AValue);
  FInternalBtn.Enabled := AValue;
end;

procedure TfpgCustomComboBox.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
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
  if FocusItem > -1 then
    fpgStyle.DrawString(Canvas, FMargin+1, FMargin, Text, Enabled);

  Canvas.EndDraw;
end;

function TfpgCustomComboBox.Text: string;
begin
  if (FocusItem > 0) and (FocusItem <= FItems.Count) then
    Result := FItems.Strings[FocusItem-1]
  else
    Result := '';
end;

constructor TfpgCustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackgroundColor  := clBoxColor;
  FDropDownCount    := 8;
  FWidth            := 120;
  FHeight           := 23;
  FFocusItem        := 0;
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
  end;
end;

end.

