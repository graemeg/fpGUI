{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2012 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a Page Control and Tab Sheets.
}

unit fpg_tab;

{$mode objfpc}{$H+}

{
  TODO:
    * Tab Styles (tab, button, flat button, angled)
    * Better keyboard support
    * Focus rectangle drawn on tabs itself
    * FindNextPage() must be implemented
    * Popup menu for tab selection. Should occur with RClick on tabs.
}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_button,
  fpg_menu;
  
type
  // forward declaration
  TfpgPageControl = class;
  
  TfpgTabStyle    = (tsTabs, tsButtons, tsFlatButtons);
  TfpgTabPosition = (tpTop, tpBottom, tpLeft, tpRight, tpNone);
  TfpgTabOption   = (to_PMenuClose, to_PMenuShowAvailTabs);

  TfpgTabOptions = set of TfpgTabOption;


  TfpgTabSheet = class(TfpgWidget)
  private
    FPageControl: TfpgPageControl;
    FText: string;
    FTabVisible: boolean;
    function    GetPageControl: TfpgPageControl;
    function    GetPageIndex: Integer;
    function    GetText: string;
    procedure   SetPageIndex(const AValue: Integer);
    procedure   SetText(const AValue: string);
    procedure   SetPageControl(APageControl: TfpgPageControl);
  protected
    procedure   HandlePaint; override;
    procedure   SetName(const NewName: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateWithTitle(AOwner: TComponent; const AText: TfpgString = ''); virtual;
    destructor  Destroy; override;
    procedure   AfterConstruction; override;
    property    PageIndex: Integer read GetPageIndex write SetPageIndex;
    property    PageControl: TfpgPageControl read FPageControl write SetPageControl;
    property    TabVisible: boolean read FTabVisible write FTabVisible;
  published
    property    BackgroundColor;
    property    Enabled;
    property    Text: string read GetText write SetText;
    property    OnPaint;
  end;


  TTabSheetChange = procedure(Sender: TObject; NewActiveSheet: TfpgTabSheet) of object;
  TTabSheetClosing = procedure(Sender: TObject; ATabSheet: TfpgTabSheet) of object;
  
  
  TfpgPageControl = class(TfpgWidget)
  private
    FFont: TfpgFont;
    FActivePage: TfpgTabSheet;
    FMargin: integer;
    FFixedTabWidth: integer;
    FFixedTabHeight: Integer;
    FOnClosingTabSheet: TTabSheetClosing;
    FPages: TList;
    FActivePageIndex: integer;
    FOnChange: TTabSheetChange;
    FRightButton: TfpgButton;         // bottom/right
    FLeftButton: TfpgButton;          // left/top
    FFirstTabButton: TfpgTabSheet;    // when tabs don't fit in screen this is the first button on screen when tabs are scrolled
    FSortPages: boolean;
    FStyle: TfpgTabStyle;
    FTabPosition: TfpgTabPosition;
    FPopupMenu: TfpgPopupMenu;
    FTabOptions: TfpgTabOptions;
    FLastRClickPos: TfpgPoint;
    FUpdateCount: Integer;
    FActiveTabColor: TfpgColor;
    function    GetActivePageIndex: integer;
    function    GetPage(AIndex: integer): TfpgTabSheet;
    function    GetPageCount: Integer;
    procedure   InsertPage(var APage: TfpgTabSheet; SuppressOnChangeEvent: boolean = False);
    procedure   RemovePage(const APage: TfpgTabSheet);
    procedure   SetActivePageIndex(const AValue: integer);
    procedure   SetActivePage(const AValue: TfpgTabSheet);
    procedure   PositionTabSheets;
    procedure   PositionTabSheet(var APage: TfpgTabSheet);
    function    MaxButtonWidthSum: integer;
    function    MaxButtonHeightSum: integer;
    function    MaxButtonWidth: integer;
    function    ButtonHeight: integer;
    function    ButtonWidth(AText: string): integer;
    procedure   SetFixedTabWidth(const AValue: integer);
    procedure   SetFixedTabHeight(const AValue: integer);
    function    GetTabText(AText: string): string;
    procedure   LeftButtonClick(Sender: TObject);
    procedure   RightButtonClick(Sender: TObject);
    function    FindNextPage(ACurrent: TfpgTabSheet; AForward: boolean): TfpgTabSheet;
    procedure   SetSortPages(const AValue: boolean);
    procedure   SetStyle(const AValue: TfpgTabStyle);
    procedure   SetTabPosition(const AValue: TfpgTabPosition);
    procedure   DoPageChange(ATabSheet: TfpgTabSheet);
    procedure   DoTabSheetClosing(ATabSheet: TfpgTabSheet);
    function    DrawTab(const rect: TfpgRect; const Selected: Boolean = False; const Mode: Integer = 1): TfpgRect;
    procedure   pmCloseTab(Sender: TObject);
    function    GetActiveTabColor: TfpgColor;
    procedure   SetActiveTabColor(AValue: TfpgColor);
  protected
    procedure   SetBackgroundColor(const AValue: TfpgColor); override;
    procedure   OrderSheets; // currently using bubblesort
    procedure   RePaintTitles; virtual;
    procedure   HandlePaint; override;
    procedure   HandleShow; override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleRMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   RePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   BeginUpdate;
    procedure   EndUpdate;
    function    TabSheetAtPos(const x, y: integer): TfpgTabSheet;
    function    AppendTabSheet(ATitle: string): TfpgTabSheet;
    procedure   RemoveTabSheet(ATabSheet: TfpgTabSheet);
    property    PageCount: Integer read GetPageCount;
    property    ActivePage: TfpgTabSheet read FActivePage write SetActivePage;
    property    Pages[AIndex: integer]: TfpgTabSheet read GetPage;
    property    OnChange: TTabSheetChange read FOnChange write FOnChange;
    property    OnClosingTabSheet: TTabSheetClosing read FOnClosingTabSheet write FOnClosingTabSheet;
  published
    property    ActivePageIndex: integer read GetActivePageIndex write SetActivePageIndex default 0;
    property    ActiveTabColor: TfpgColor read GetActiveTabColor write SetActiveTabColor default clWindowBackground;
    property    Align;
    property    BackgroundColor;
    property    Enabled;
    property    FixedTabWidth: integer read FFixedTabWidth write SetFixedTabWidth default 0;
    property    FixedTabHeight: integer read FFixedTabHeight write SetFixedTabHeight default 21;
    property    Hint;
    property    Options: TfpgTabOptions read FTabOptions write FTabOptions;
    property    ParentShowHint;
    property    ShowHint;
    property    SortPages: boolean read FSortPages write SetSortPages default False;
    property    Style: TfpgTabStyle read FStyle write SetStyle default tsTabs;
    property    TabOrder;
    property    TabPosition: TfpgTabPosition read FTabPosition write SetTabPosition default tpTop;
    property    TextColor;
    property    OnShowHint;
  end;


implementation

uses
  fpg_stringutils;
  
  
// compare function used by FPages.Sort

function SortCompare(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(TfpgTabSheet(Item1).Text, TfpgTabSheet(Item2).Text);
end;

{ TfpgTabSheet }

function TfpgTabSheet.GetPageControl: TfpgPageControl;
begin
  if Owner is TfpgPageControl then
    Result := TfpgPageControl(Owner)
  else
    Result := nil;
end;

function TfpgTabSheet.GetPageIndex: Integer;
begin
  if PageControl <> nil then
    Result := PageControl.FPages.IndexOf(Self)
  else
    Result := -1;
end;

function TfpgTabSheet.GetText: string;
begin
  Result := FText;
end;

procedure TfpgTabSheet.SetPageIndex(const AValue: Integer);
begin
  if PageControl <> nil then
  begin
    PageControl.FPages.Move(PageIndex, AValue);
    PageControl.RePaint;//Titles;
  end;
end;

procedure TfpgTabSheet.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit; //==>
  FText := AValue;
  if PageControl <> nil then
    PageControl.Invalidate;
end;

procedure TfpgTabSheet.HandlePaint;
begin
  inherited HandlePaint;
  Canvas.Clear(FBackgroundColor);
end;

procedure TfpgTabSheet.SetName(const NewName: TComponentName);
var
  old: String;
begin
  old := NewName;
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
  begin
    if (Text = '') or (Text = old) then
      Text := NewName;
  end;
end;

constructor TfpgTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText := '';
  FTabVisible:= True;
  FFocusable := True;
  FBackgroundColor := Parent.BackgroundColor;
  FTextColor := Parent.TextColor;
  FIsContainer := True;
end;

constructor TfpgTabSheet.CreateWithTitle(AOwner: TComponent; const AText: TfpgString);
begin
  Create(AOwner);
  FText := AText;
end;

destructor TfpgTabSheet.Destroy;
begin
  if FPageControl <> nil then
    FPageControl.RemovePage(self);
  inherited Destroy;
end;

procedure TfpgTabSheet.AfterConstruction;
begin
  if (Owner <> nil) and (Owner is TfpgPageControl) then
  begin
    FPageControl:=TfpgPageControl(Owner);
    FPageControl.InsertPage(self, True);
  end;
  inherited AfterConstruction;
end;

procedure TfpgTabSheet.SetPageControl(APageControl: TfpgPageControl);
begin
  FPageControl := APageControl;
  if APageControl <> nil then
    FPageControl.InsertPage(Self);
end;

  
{ TfpgPageControl }

function TfpgPageControl.GetActivePageIndex: integer;
begin
  Result := FActivePageIndex;
end;

function TfpgPageControl.GetPage(AIndex: integer): TfpgTabSheet;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FPages.Count) then
    Result := TfpgTabSheet(FPages[AIndex]);
end;

function TfpgPageControl.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TfpgPageControl.InsertPage(var APage: TfpgTabSheet; SuppressOnChangeEvent: boolean = False);
begin
  if FPages.IndexOf(APage) <> -1 then
    Exit; //==>   The page has already been added.
  FPages.Add(APage);
  PositionTabSheets;
  { TODO: This behaviour could maybe be controlled by a Options property }
  if FPages.Count=1 then
  begin
    if SuppressOnChangeEvent then
      Loading;
    ActivePage := APage;
    if SuppressOnChangeEvent then
      Loaded;
  end;
end;

procedure TfpgPageControl.RemovePage(const APage: TfpgTabSheet);
var
  i: integer;
begin
  if APage = nil then
    Exit; // ==>
  if FPages.Count =0 then
    Exit; // ==>
  
  if FPages.Count > 1 then              
  begin
     i:=FPages.IndexOf(APage);
     FPages.Remove(APage);
    APage.PageControl:=nil;
    APage.Visible:=false;
    if i = ActivePageIndex then
    begin	    
      if i > FPages.Count-1 then
         ActivePage:=TfpgTabSheet(FPages.Last)
      else if i = 0 then
        ActivePage:= TfpgTabSheet(FPages.First)
      else
        ActivePage:=TfpgTabSheet(FPages[i]);
    end
    else if i < ActivePageIndex then
      ActivePage:=TfpgTabSheet(Pages[i-1]);	      
  end      
  else
  begin
    FPages.Remove(APage);
    APage.PageControl := nil;
    APage.Visible := False;
    ActivePage := nil;
  end;
end;

procedure TfpgPageControl.SetActivePageIndex(const AValue: integer);
begin
  if FPages.Count = 0 then
    exit;
  if (AValue >= 0) or (AValue < FPages.Count) then
    ActivePage := TfpgTabSheet(FPages[AValue]);
end;

procedure TfpgPageControl.SetActivePage(const AValue: TfpgTabSheet);
begin
  if FActivePage = AValue then
    Exit; //==>
  FActivePage := AValue;
  ActiveWidget := AValue;
  if AValue <> nil then
    FActivePageIndex := FPages.IndexOf(AValue);
  RePaint;
  DoPageChange(FActivePage);
end;

procedure TfpgPageControl.PositionTabSheets;
var
  i: integer;
  t: TfpgTabSheet;
begin
  for i := 0 to FPages.Count-1 do
  begin
    t := TfpgTabSheet(FPages[i]);
    PositionTabSheet(t);
    t.Anchors := [anLeft, anTop, anRight, anBottom];
  end;
end;

procedure TfpgPageControl.PositionTabSheet(var APage: TfpgTabSheet);
var
  r: TRect;
  w: integer;
  wd: integer;  { width delta }
  h: integer;
  hd: integer;  { height delta }
  msg: TfpgMessageParams;
begin
  // PageControl has bevelled edges in some themes
  r := fpgStyle.GetControlFrameBorders;

  { Calculate and set Width and Height }
  if TabPosition in [tpTop, tpBottom] then
  begin
    w := Width - (FMargin*2) - r.Left - r.Right;
    wd := APage.Width - w;
    APage.Width   := w;
    h := Height - ButtonHeight - (FMargin*2) - r.Top - r.Bottom;
    hd := APage.Height - h;
    APage.Height  := h;
  end
  else if TabPosition in [tpLeft, tpRight] then
  begin
    w := Width - MaxButtonWidth - (FMargin*2) - r.Left - r.Right;
    wd := APage.Width - w;
    APage.Width   := w;
    h := Height - (FMargin*2) - r.Top - r.Bottom;
    hd := APage.Height - h;
    APage.Height  := h;
  end
  else
  begin   // tpNone
    w := Width - (FMargin*2) - r.Left - r.Right;
    wd := APage.Width - w;
    APage.Width   := w;
    h := Height - (FMargin*2) - r.Top - r.Bottom;
    hd := APage.Height - h;
    APage.Height  := h;
  end;

  { Calculate and set Top and Left }
  if TabPosition = tpTop then
  begin
    APage.Left    := FMargin + r.Left;
    APage.Top     := ButtonHeight + FMargin + r.Top;
  end
  else if TabPosition = tpBottom then
  begin
    APage.Left    := FMargin + r.Left;
    APage.Top     := FMargin + r.Top;
  end
  else if TabPosition = tpLeft then
  begin
    APage.Left    := MaxButtonWidth + FMargin + r.Left;
    APage.Top     := FMargin + r.Top;
  end
  else if TabPosition = tpRight then
  begin
    APage.Left    := FMargin + r.Left;
    APage.Top     := FMargin + r.Top;
  end;

  if TabPosition in [tpNone] then
  begin
    APage.Left    := FMargin + r.Left;
    APage.Top     := FMargin + r.Top;
  end;

  APage.UpdateWindowPosition; { Internal state is now resolved }
end;

function TfpgPageControl.MaxButtonWidthSum: integer;
var
  i: integer;
  t: TfpgTabSheet;
begin
  {$IFDEF DEBUG}writeln(Classname + '.MaxButtonWidthSum');{$ENDIF}
  Result := 0;
  
  for i := 0 to FPages.Count-1 do
  begin
    t := TfpgTabSheet(FPages[i]);
    Result := Result + ButtonWidth(t.Text);
  end;
end;

function TfpgPageControl.MaxButtonHeightSum: integer;
begin
  result := PageCount * ButtonHeight;
end;

function TfpgPageControl.MaxButtonWidth: integer;
var
  t: TfpgTabSheet;
  i: integer;
begin
  Result := 0;
  if FixedTabWidth > 0 then
  begin
    Result := FixedTabWidth;
  end
  else
  begin
    for i := 0 to FPages.Count-1 do
    begin
      t := TfpgTabSheet(FPages[i]);
      if ButtonWidth(t.Text) > Result then
        Result := ButtonWidth(t.Text);
    end;
  end;
end;

function TfpgPageControl.ButtonHeight: integer;
begin
  if FFixedTabHeight > 0 then
    result := FFixedTabHeight
  else
    result := FFont.Height + 10;   { TODO: correct this }
end;

function TfpgPageControl.ButtonWidth(AText: string): integer;
begin
  if FFixedTabWidth > 0 then
    result := FFixedTabWidth
  else
    result := FFont.TextWidth(AText) + 10;
end;

procedure TfpgPageControl.SetFixedTabWidth(const AValue: integer);
begin
  if FFixedTabWidth = AValue then
    Exit; //==>
  if AValue > 5 then
  begin
    FFixedTabWidth := AValue;
    RePaint;
  end;
end;

procedure TfpgPageControl.SetFixedTabHeight(const AValue: integer);
begin
  if FFixedTabHeight = AValue then
    Exit; //==>
  if AValue > 5 then
  begin
    FFixedTabHeight := AValue;
    RePaint;
  end;
end;

function TfpgPageControl.GetTabText(AText: string): string;
var
  s, s1: string;
  i: integer;
begin
  {$IFDEF DEBUG}writeln(Classname + '.GetTabText');{$ENDIF}
  Result  := AText;
  s       := AText;
  s1      := '';
  i       := 1;
  if FFixedTabWidth > 0 then
  begin
    while FFont.TextWidth(s1) < (FFixedTabWidth-10) do
    begin
      if Length(s1) = Length(s) then
        Break;
      s1 := UTF8Copy(s, 1, i);
      inc(i);
    end;
    if FFont.TextWidth(s1) > (FFixedTabWidth-10) then
      UTF8Delete(s1, UTF8Length(s1), 1);
    if Length(s1) > 0 then
      s1 := Trim(s1);
    Result := s1;
  end;
end;

procedure TfpgPageControl.LeftButtonClick(Sender: TObject);
begin
  {$IFDEF DEBUG}writeln(Classname + '.LeftButtonClick');{$ENDIF}
  if FFirstTabButton <> nil then
  begin
    if TfpgTabSheet(FPages.First) <> FFirstTabButton then
    begin
      FFirstTabButton := TfpgTabSheet(FPages[FPages.IndexOf(FFirstTabButton)-1]);
      RePaint;
    end;
  end;
end;

procedure TfpgPageControl.RightButtonClick(Sender: TObject);
begin
  {$IFDEF DEBUG}writeln(Classname + '.RightButtonClick');{$ENDIF}
  if FFirstTabButton <> nil then
  begin
    if TfpgTabSheet(FPages.Last) <> FFirstTabButton then
    begin
      FFirstTabButton := TfpgTabSheet(FPages[FPages.IndexOf(FFirstTabButton)+1]);
      RePaint;
    end;
  end;
end;

function TfpgPageControl.FindNextPage(ACurrent: TfpgTabSheet; AForward: boolean): TfpgTabSheet;
begin
  // To be completed
  result := nil;
end;

procedure TfpgPageControl.SetSortPages(const AValue: boolean);
begin
  if FSortPages = AValue then
    Exit; //==>
  FSortPages := AValue;
  RePaint;
end;

procedure TfpgPageControl.SetStyle(const AValue: TfpgTabStyle);
begin
  if FStyle = AValue then
    Exit; //==>
  FStyle := AValue;
  Invalidate;
end;

procedure TfpgPageControl.SetTabPosition(const AValue: TfpgTabPosition);
begin
  if FTabPosition = AValue then
    Exit; //==>
  FTabPosition := AValue;
  RePaint;
end;

procedure TfpgPageControl.DoPageChange(ATabSheet: TfpgTabSheet);
begin
  if (csLoading in ComponentState) then
    Exit;
  if (csDesigning in ComponentState) then
    Exit;
  if Assigned(FOnChange) then
    FOnChange(self, ATabSheet);
end;

procedure TfpgPageControl.DoTabSheetClosing(ATabSheet: TfpgTabSheet);
begin
  if (csLoading in ComponentState) then
    Exit;
  if (csDesigning in ComponentState) then
    Exit;
  if Assigned(FOnClosingTabSheet) then
    FOnClosingTabSheet(self, ATabSheet);
end;

{ Mode = 1 means the background tabs. Mode = 2 means the Active Tab }
function TfpgPageControl.DrawTab(const rect: TfpgRect; const Selected: Boolean = False; const Mode: Integer = 1): TfpgRect;
var
  r: TfpgRect;
begin
  r := rect;
  if Selected then
  begin
    Result := rect;
    InflateRect(Result, 2, 2);
    Exit; //==>
  end;

  if Mode = 2 then
  begin
    r.Height -= 1;
    Canvas.SetColor(ActiveTabColor);
  end
  else
    Canvas.SetColor(BackgroundColor);

  case TabPosition of
    tpTop:
      begin
        Canvas.FillRectangle(r.Left+1, r.Top+1, r.Width-3, r.Height-2);     // fill tab background
        Canvas.SetColor(clHilite2);
        Canvas.DrawLine(r.Left, r.Bottom-2 , r.Left, r.Top+2);        // left edge
        Canvas.DrawLine(r.Left, r.Top+2 , r.Left+2, r.Top);           // left rounder edge
        Canvas.DrawLine(r.Left+2,  r.Top, r.Right-1, r.Top);          // top edge
        Canvas.SetColor(clShadow1);
        Canvas.DrawLine(r.Right-1, r.Top+1, r.Right-1, r.Bottom-1);   // right inner edge
        Canvas.SetColor(clShadow2);
        Canvas.DrawLine(r.Right-1, r.Top+1, r.Right, r.Top+2);        // right rounded edge (1px)
        Canvas.DrawLine(r.Right, r.Top+2, r.Right, r.Bottom-1);       // right outer edge
      end;

    tpBottom:
      begin
        Canvas.FillRectangle(r.Left, r.Top+1, r.Width-2, r.Height-3);   // fill tab background
        Canvas.SetColor(clHilite2);
        Canvas.DrawLine(r.Left, r.Top, r.Left, r.Bottom-1);           // left edge
        Canvas.SetColor(clShadow2);
        Canvas.DrawLine(r.Left+2,  r.Bottom, r.Right-1, r.Bottom);    // bottom outer edge
        Canvas.SetColor(clShadow1);
        Canvas.DrawLine(r.Right-1, r.Bottom-1, r.Right-1, r.Top+1);   // right inner edge
        Canvas.DrawLine(r.Left+1,  r.Bottom-1, r.Right-1, r.Bottom-1);// bottom inner edge
        Canvas.SetColor(clShadow2);
        Canvas.DrawLine(r.Right-1, r.Bottom-1, r.Right, r.Bottom-2);  // right rounded edge (1px)
        Canvas.DrawLine(r.Right, r.Bottom-2, r.Right, r.Top+1);       // right outer edge
      end;

    tpLeft:
      begin
        if Mode = 2 then
        begin
          r.Width  := r.Width - 1;
          r.Height := r.Height + 2;
        end;
        with Canvas do
        begin
          FillRectangle(r.Left+1, r.Top+1, r.Width-2, r.Height-3);
          SetColor(clHilite2);
          DrawLine(r.Left, r.Bottom-2, r.Left, r.Top+2);
          DrawLine(r.Left, r.Top+2, r.Left+2, r.Top);
          DrawLine(r.Left+2, r.Top, r.Right-1, r.Top);
          SetColor(clShadow1);
          DrawLine(r.Left+2, r.Bottom-1, r.Right-1, r.Bottom-1);
          SetColor(clShadow2);
          DrawLine(r.Left+1, r.Bottom-1, r.Left+3, r.Bottom);
          DrawLine(r.Left+2, r.Bottom, r.Right, r.Bottom);
        end;
      end;

    tpRight:
      begin
        if Mode = 2 then
        begin
          r.Height := r.Height + 2;
        end;
        with Canvas do
        begin
          FillRectangle(r.Left+1, r.Top+1, r.Width-2, r.Height-3);
          SetColor(clHilite2);
          DrawLine(r.Left+1, r.Top, r.Right-2, r.Top);
          SetColor(clShadow1);
          DrawLine(r.Right-2,r.Top,r.Right-1,r.Top+1);
          DrawLine(r.Left+2, r.Bottom-1, r.Right-2, r.Bottom-1);
          DrawLine(r.Right-3, r.Bottom-1, r.Right-1, r.Bottom-3);
          DrawLine(r.Right-1, r.Bottom-3, r.Right-1, r.Top);
          SetColor(clShadow2);
          DrawLine(r.Left+2,r.Bottom,r.Right-3, r.Bottom);
          DrawLine(r.Right-3, r.Bottom, r.Right, r.Bottom-3);
          DrawLine(r.Right, r.Top+2, r.Right, r.Bottom-2);
        end;
      end;
  end;  { case }
end;

procedure TfpgPageControl.pmCloseTab(Sender: TObject);
var
  ts: TfpgTabSheet;
begin
  ts := TabSheetAtPos(FLastRClickPos.x, FLastRClickPos.y);
  if not Assigned(ts) then
    ts := ActivePage;
  if ts = nil then
    exit;
  RemovePage(ts);
  DoTabSheetClosing(ts);
  ts.Free;
end;

function TfpgPageControl.GetActiveTabColor: TfpgColor;
begin
  Result := FActiveTabColor;
end;

procedure TfpgPageControl.SetActiveTabColor(AValue: TfpgColor);
begin
  if FActiveTabColor <> AValue then
  begin
    FActiveTabColor := AValue;
    RePaint;
  end;
end;

procedure TfpgPageControl.SetBackgroundColor(const AValue: TfpgColor);
var
  lWasMatch: boolean;
begin
  lWasMatch := FBackgroundColor = FActiveTabColor;
  inherited SetBackgroundColor(AValue);
  if lWasMatch then
    ActiveTabColor := FBackgroundColor;
end;

procedure TfpgPageControl.OrderSheets;
begin
  FPages.Sort(@SortCompare);
  FActivePageIndex := FPages.IndexOf(ActivePage);
end;

procedure TfpgPageControl.RePaintTitles;
const
  TabHeight = 21;
var
  TabW, TabH: Integer;
  r2: TfpgRect;
  r3: TfpgRect;
  h: TfpgTabSheet;
  lp: integer;
  toffset: integer;
  TextLeft, TextTop: Integer;
  dx: integer;
  lTxtFlags: TfpgTextFlags;
  ActivePageVisible: Boolean;
begin
  if not HasHandle then
    Exit; //==>
    
  if PageCount = 0 then
    Exit; //==>

  TabW:=FixedTabWidth;
  TabH:=FixedTabHeight;
  ActivePageVisible := false;
  If TabH = 0 then
    TabH := TabHeight;
  h := TfpgTabSheet(FPages.First);
  if h = nil then
    Exit; //==>
  
  Canvas.SetTextColor(TextColor);
  lTxtFlags := [];
  if not Enabled then
    Include(lTxtFlags, txtDisabled);


  if TabPosition in [tpTop, tpBottom] then
  begin
    if MaxButtonWidthSum > (Width-(FMargin*2)) then
    begin
      if FFirstTabButton = nil then
        FFirstTabButton := h
      else
        h := FFirstTabButton;
      if TabPosition = tpTop then
      begin
        FLeftButton.SetPosition(Width - (FRightButton.Width * 2), FMargin, FRightButton.Height, FRightButton.Height);
        FRightButton.SetPosition(Width - FRightButton.Width, FMargin, FRightButton.Height, FRightButton.Height);
      end
      else
      begin
        FLeftButton.SetPosition(Width - (FRightButton.Width * 2), Height - ButtonHeight - FMargin, FRightButton.Height, FRightButton.Height);
        FRightButton.SetPosition(Width - FRightButton.Width, Height - ButtonHeight - FMargin, FRightButton.Height, FRightButton.Height);
      end;
      FLeftButton.Visible   := True;
      FRightButton.Visible  := True;
    end
    else
    begin
      FLeftButton.Visible   := False;
      FRightButton.Visible  := False;
    end;
  end;

  if TabPosition in [tpLeft, tpRight] then
  begin
    if MaxButtonHeightSum > (Height-(FMargin*2)) then
    begin
      if FFirstTabButton = nil then
        FFirstTabButton := h
      else
        h := FFirstTabButton;
      if TabPosition = tpLeft then
      begin
        FLeftButton.SetPosition(MaxButtonWidth - (FRightButton.Width * 2), Height - ButtonHeight - FMargin, FRightButton.Height, FRightButton.Height);
        FRightButton.SetPosition(MaxButtonWidth - FRightButton.Width, Height - ButtonHeight - FMargin, FRightButton.Height, FRightButton.Height);
      end
      else
      begin
        FLeftButton.SetPosition(Width - MaxButtonWidth, Height - ButtonHeight - FMargin, FRightButton.Height, FRightButton.Height);
        FRightButton.SetPosition(Width - MaxButtonWidth + FRightButton.Width, Height - ButtonHeight - FMargin, FRightButton.Height, FRightButton.Height);
      end;
      FLeftButton.Visible   := True;
      FRightButton.Visible  := True;
    end
    else
    begin
      FLeftButton.Visible   := False;
      FRightButton.Visible  := False;
    end;
  end;

  case TabPosition of
    tpNone:
      begin
        while h <> nil do
        begin
          if h <> ActivePage then
            h.Visible:=false
          else
            h.Visible:=True;
          h.SetPosition(FMargin+2, FMargin+2 , Width - (FMargin*2) - 4, Height - ((FMargin+2)*2));
          if h <> TfpgTabSheet(FPages.Last) then
            h := TfpgTabSheet(FPages[FPages.IndexOf(h)+1])
          else
            h := nil;
        end;
        r2.Left    := 0;
        r2.Top     := 0;
        r2.Width   := Width;
        r2.Height  := Height;
        Canvas.DrawButtonFace(r2, []);
      end;

    tpBottom:
      begin
        lTxtFlags += TextFlagsDflt;
        lp := 0;
        r2.SetRect(2, Height - ButtonHeight-3, 50, 21);
        while h <> nil do
        begin
          if h <> ActivePage then
          begin
            toffset := 2;
            h.Visible := False;
          end
          else
          begin
            toffset := 4;
            h.Visible := True;
            h.SetPosition(FMargin+2, FMargin+2 , Width - (FMargin*2) - 4, Height - r2.Height - (FMargin+2)*2);
          end;
          // paint tab button
          r2.Width := ButtonWidth(h.Text);
          r3 := DrawTab(r2, h = ActivePage);
          // paint text on non-active tabs
          if h <> ActivePage then
          Canvas.DrawText(lp + (ButtonWidth(h.Text) div 2) - FFont.TextWidth(GetTabText(h.Text)) div 2,
              Height-r2.Height-toffset, GetTabText(h.Text), lTxtFlags);

          r2.Left := r2.Left + r2.Width;
          lp := lp + ButtonWidth(h.Text);
          if h <> TfpgTabSheet(FPages.Last) then
            h := TfpgTabSheet(FPages[FPages.IndexOf(h)+1])
          else
            h := nil;
        end;
        // Draw Page Control body rectangle (client area)
        r2.Left    := 0;
        r2.Top     := 0;
        r2.Width   := Width;
        r2.Height  := Height - r2.Height;
        Canvas.DrawButtonFace(r2, []);
        // Draw text of ActivePage, because we didn't before.
        DrawTab(r3, false, 2);
        Canvas.DrawText(r3.Left+4, r3.Top+5, r3.Width, r3.Height, ActivePage.Text, lTxtFlags);
      end;

    tpTop:
      begin
        lTxtFlags += TextFlagsDflt;
        lp := 0;
        r2.SetRect(2, 2, 50, 21);
        while h <> nil do
        begin
          if h <> ActivePage then
          begin
            toffset := 4;
            h.Visible := False;
          end
          else
          begin
            toffset := 2;
            h.Visible := True;
            h.SetPosition(FMargin+2, FMargin+2 + r2.Height, Width - (FMargin*2) - 4, Height - r2.Height - ((FMargin+2)*2));
          end;
          // paint tab button
          r2.Width := ButtonWidth(h.Text);
          r3 := DrawTab(r2, h = ActivePage);

          // paint text on non-active tabs
          if h <> ActivePage then
            Canvas.DrawText(lp + (ButtonWidth(h.Text) div 2) - FFont.TextWidth(GetTabText(h.Text)) div 2,
                FMargin+toffset, GetTabText(h.Text), lTxtFlags);
          r2.Left := r2.Left + r2.Width;
          lp := lp + ButtonWidth(h.Text);
          if h <> TfpgTabSheet(FPages.Last) then
            h := TfpgTabSheet(FPages[FPages.IndexOf(h)+1])
          else
            h := nil;
        end;
        // Draw Page Control body rectangle (client area)
        r2.Left    := 0;
        r2.Top     := r2.Top + r2.Height-2;
        r2.Width   := Width;
        r2.Height  := Height - r2.Height;
        Canvas.DrawButtonFace(r2, []);

        // Draw text of ActivePage, because we didn't before.
        DrawTab(r3, false, 2);
        Canvas.DrawText(r3.Left+4, r3.Top+3, r3.Width, r3.Height, ActivePage.Text, lTxtFlags);
      end;

    tpRight:
      begin
        lTxtFlags += [txtVCenter, txtLeft];
        lp := 0;
        TabW := MaxButtonWidth;
        r2.SetRect(Width - 2 - TabW, 2, TabW, 21);
        while h <> nil do
        begin
          if h <> ActivePage then
          begin
            toffset := 4;
            h.Visible := False;
          end
          else
          begin
            toffset := 2;
            h.Visible := True;
            { set tab content page (client area) size }
            h.SetPosition(FMargin+2, FMargin+2, Width - ((FMargin+2)*2) - TabW, Height - ((FMargin+2)*2));
          end;
          // paint tab button
          r3 := DrawTab(r2, h = ActivePage);

          // paint text on non-active tabs
          if h <> ActivePage then
            Canvas.DrawText(r2.left+toffset, r2.Top, r2.Width, r2.Height, GetTabText(h.Text), lTxtFlags);
          r2.Top += r2.Height;
          lp := r2.Top;
          if h <> TfpgTabSheet(FPages.Last) then
            h := TfpgTabSheet(FPages[FPages.IndexOf(h)+1])
          else
            h := nil;
        end;
        // Draw Page Control body rectangle (client area)
        r2.Left    := 0;
        r2.Top     := 0;
        r2.Width   := Width - TabW;
        r2.Height  := Height;
        Canvas.DrawButtonFace(r2, []);

        // Draw text of ActivePage, because we didn't before.
        DrawTab(r3, false, 2);
        Canvas.DrawText(r3.left+toffset, r3.Top, r3.Width, r3.Height, ActivePage.Text, lTxtFlags);
      end;

    tpLeft:
      begin
        lTxtFlags += [txtVCenter, txtLeft];
        lp := 0;
        TabW := MaxButtonWidth;
        r2.SetRect(2, 2, TabW, 21);
        while h <> nil do
        begin
          if h <> ActivePage then
          begin
            toffset := 4;
            h.Visible := False;
          end
          else
          begin
            toffset := 2;
            h.Visible := True;
            { set tab content page (client area) size }
            h.SetPosition(FMargin+2+TabW, FMargin+2, Width - ((FMargin+2)*2) - TabW, Height - ((FMargin+2)*2));
          end;
          // paint tab button
          r3 := DrawTab(r2, h = ActivePage);

          // paint text on non-active tabs
          if h <> ActivePage then
            Canvas.DrawText(r2.left+toffset, r2.Top, r2.Width, r2.Height, GetTabText(h.Text), lTxtFlags);
          r2.Top += r2.Height;
          lp := r2.Top;
          if h <> TfpgTabSheet(FPages.Last) then
            h := TfpgTabSheet(FPages[FPages.IndexOf(h)+1])
          else
            h := nil;
        end;
        // Draw Page Control body rectangle (client area)
        r2.Left    := TabW;
        r2.Top     := 0;
        r2.Width   := Width - TabW;
        r2.Height  := Height;
        Canvas.DrawButtonFace(r2, []);

        // Draw text of ActivePage, because we didn't before.
        DrawTab(r3, false, 2);
        Canvas.DrawText(r3.left+toffset, r3.Top, r3.Width, r3.Height, ActivePage.Text, lTxtFlags);
      end;
  end; { case }

end;

procedure TfpgPageControl.HandlePaint;
begin
  if SortPages then
    OrderSheets;
  Canvas.ClearClipRect;
  Canvas.Clear(FBackgroundColor);
  
  // To make it more visible in the UI Designer
  if csDesigning in ComponentState then
  begin
    Canvas.SetColor(clInactiveWgFrame);
    Canvas.DrawRectangle(0, 0, Width, Height);
    if PageCount = 0 then
    begin
      Canvas.SetTextColor(clText1);
      Canvas.DrawString(2, 2, Name + ': ' + Classname);
    end;
  end;
  RePaintTitles;
end;

procedure TfpgPageControl.HandleShow;
begin
  inherited HandleShow;
  FLeftButton.Visible := False;
  FRightButton.Visible := False;
end;

procedure TfpgPageControl.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
var
  ts: TfpgTabSheet;
begin
//  debugln('>> TfpgPageControl.HandleLMouseUp');
  ts := TfpgTabSheet(FPages.First);
  if ts = nil then
    exit; //==>  { This means there are no tabs }
  
  ts := TabSheetAtPos(x, y);
  
  if Assigned(ts) then
    ActivePage := ts;

  inherited HandleLMouseUp(x, y, shiftstate);
end;

procedure TfpgPageControl.HandleRMouseUp(x, y: integer; shiftstate: TShiftState);
var
  ts: TfpgTabSheet;
  s: TfpgString;
begin
  inherited HandleRMouseUp(x, y, shiftstate);

  { store the position for later usage }
  FLastRClickPos := fpgPoint(x,y);

  if to_PMenuClose in FTabOptions then
  begin
    ts := TabSheetAtPos(x, y);
    {$NOTE TODO: This text needs to become a resource string }
    if Assigned(ts) then
      s := Format('Close "%s" Tab', [ts.Text])
    else
      s := 'Close Tab';
      
    if not Assigned(FPopupMenu) then
    begin
      FPopupMenu := TfpgPopupMenu.Create(self);
      FPopupMenu.AddMenuItem(s, '', @pmCloseTab);
    end
    else
    begin
      FPopupMenu.MenuItem(0).Text := s;    { This is dangerous but works for now }
    end;
    FPopupMenu.ShowAt(self, x, y);
  end;
end;

procedure TfpgPageControl.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  i: integer;
begin
  i := ActivePageIndex;
  if ssAlt in shiftstate then
  case keycode of
    keyLeft:
        begin
          if ActivePage <> TfpgTabSheet(FPages.First) then
          begin
            ActivePage := TfpgTabSheet(FPages[i-1]);
            consumed := True;
          end;
        end;

    keyRight:
        begin
          if ActivePage <> TfpgTabSheet(FPages.Last) then
          begin
            ActivePage := TfpgTabSheet(FPages[i+1]);
            consumed := True;
          end;
        end;
  end;  { case/else }
  if not consumed then
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfpgPageControl.RePaint;
begin
  if FUpdateCount > 0 then
    Exit;
  inherited RePaint;
end;

constructor TfpgPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont   := fpgStyle.DefaultFont;
  FPages  := TList.Create;
  Width   := 150;
  Height  := 100;
  FIsContainer := True;
  FTabOptions  := [];
  FActivePageIndex := 0;

  FTextColor        := Parent.TextColor;
  FBackgroundColor  := Parent.BackgroundColor;
  FActiveTabColor   := FBackgroundColor;
  FFocusable        := True;
  FOnChange         := nil;
  FFixedTabWidth    := 0;
  FFixedTabHeight   := 21;
  FFirstTabButton   := nil;
  FStyle            := tsTabs;
  FTabPosition      := tpTop;
  FMargin           := 1;
  FSortPages        := False;

  FLeftButton := TfpgButton.Create(self);
  FLeftButton.Text      := '<';
  FLeftButton.Height    := 20;
  FLeftButton.Width     := 20;
  FLeftButton.OnClick   := @LeftButtonClick;

  FRightButton := TfpgButton.Create(self);
  FRightButton.Text     := '>';
  FRightButton.Height   := 20;
  FRightButton.Width    := 20;
  FRightButton.OnClick  := @RightButtonClick;
end;

destructor TfpgPageControl.Destroy;
var i: integer;
begin
  FOnChange := nil;
  for i:=0 to FPages.Count-1 do
    TfpgTabSheet(FPages[i]).PageControl:=nil;
  FPages.Free;
  ActiveWidget := nil;
  FFirstTabButton := nil;
  inherited Destroy;
end;

procedure TfpgPageControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TfpgPageControl.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then
    RePaint;
end;

function TfpgPageControl.TabSheetAtPos(const x, y: integer): TfpgTabSheet;
var
  h: TfpgTabSheet;
  lp: integer;  // left position
  bw: integer;  // button width
  bh: integer;  // button height
  p1, p2: integer;    // tab boundaries for mouse click to take affect
begin
  Result := nil;
  h := TfpgTabSheet(FPages.First);

  lp := FMargin;
  if MaxButtonWidthSum > (Width-(FMargin*2)) then
    h := FFirstTabButton;

  case TabPosition of
    tpTop:
      begin
        p1 := FMargin;
        p2 := ButtonHeight;
      end;

    tpBottom:
      begin
        p1 := Height - FMargin - ButtonHeight;
        p2 := Height - FMargin;
      end;

    tpRight:
      begin
        p1 := Width - MaxButtonWidth;
        p2 := Width;
      end;

    tpLeft:
      begin
        p1 := FMargin;
        p2 := FMargin + MaxButtonWidth;
      end;
  end;

  if TabPosition in [tpTop, tpBottom] then
  begin
    if (y > p1) and (y < p2) then
    begin
       while h <> nil do
       begin
          bw := ButtonWidth(h.Text);  // initialize button width
          if (x > lp) and (x < lp + bw) then
          begin
            if h <> ActivePage then
              Result := h;
            exit;
          end;  { if }
          lp := lp + bw;
          if h <> TfpgTabSheet(FPages.Last) then
            h := TfpgTabSheet(FPages[FPages.IndexOf(h)+1])
          else
            h := nil;
       end;  { while }
    end;  { if }
  end;

  if TabPosition in [tpLeft, tpRight] then
  begin
    if (x > p1) and (x < p2) then
    begin
      while h <> nil do
      begin
        bh := ButtonHeight;  // initialize button height
        if (y > lp) and (y < lp + bh) then
        begin
          if h <> ActivePage then
            Result := h;
          exit;
        end;  { if }
        lp := lp + bh;
        if h <> TfpgTabSheet(FPages.Last) then
          h := TfpgTabSheet(FPages[FPages.IndexOf(h)+1])
        else
          h := nil;
      end;  { while }
    end;  { if }
  end;
end;

function TfpgPageControl.AppendTabSheet(ATitle: string): TfpgTabSheet;
begin
  Result := TfpgTabSheet.Create(self);
  Result.Text := ATitle;
  InsertPage(Result);
end;

procedure TfpgPageControl.RemoveTabSheet(ATabSheet: TfpgTabSheet);
begin
  RemovePage(ATabSheet);
end;

end.

