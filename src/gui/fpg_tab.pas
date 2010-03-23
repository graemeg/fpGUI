{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
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
    * Tab Position (top, bottom, left, right)
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
  fpg_button;
  
type
  // forward declaration
  TfpgPageControl = class;
  
  TfpgTabStyle    = (tsTabs, tsButtons, tsFlatButtons);
  TfpgTabPosition = (tpTop, tpBottom, tpNone{, tpLeft, tpRight});


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
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    PageIndex: Integer read GetPageIndex write SetPageIndex;
    property    PageControl: TfpgPageControl read FPageControl write SetPageControl;
    property    TabVisible: boolean read FTabVisible write FTabVisible;
  published
    property    Text: string read GetText write SetText;
  end;


  TTabSheetChange = procedure(Sender: TObject; NewActiveSheet: TfpgTabSheet) of object;
  
  
  TfpgPageControl = class(TfpgWidget)
  private
    FFont: TfpgFont;
    FActivePage: TfpgTabSheet;
    FMargin: integer;
    FFixedTabWidth: integer;
    FPages: TList;
    FActivePageIndex: integer;
    FOnChange: TTabSheetChange;
    FRightButton: TfpgButton;
    FLeftButton: TfpgButton;
    FFirstTabButton: TfpgTabSheet;
    FSortPages: boolean;
    FStyle: TfpgTabStyle;
    FTabPosition: TfpgTabPosition;
    function    GetActivePageIndex: integer;
    function    GetPage(AIndex: integer): TfpgTabSheet;
    function    GetPageCount: Integer;
    procedure   InsertPage(const APage: TfpgTabSheet);
    procedure   RemovePage(const APage: TfpgTabSheet);
    procedure   SetActivePageIndex(const AValue: integer);
    procedure   SetActivePage(const AValue: TfpgTabSheet);
    function    MaxButtonWidthSum: integer;
    function    MaxButtonHeight: integer;
    function    MaxButtonWidth: integer;
    function    ButtonHeight: integer;
    function    ButtonWidth(AText: string): integer;
    procedure   SetFixedTabWidth(const AValue: integer);
    function    GetTabText(AText: string): string;
    procedure   LeftButtonClick(Sender: TObject);
    procedure   RightButtonClick(Sender: TObject);
    function    FindNextPage(ACurrent: TfpgTabSheet; AForward: boolean): TfpgTabSheet;
    procedure   SetSortPages(const AValue: boolean);
    procedure   SetStyle(const AValue: TfpgTabStyle);
    procedure   SetTabPosition(const AValue: TfpgTabPosition);
    procedure   DoChange(ATabSheet: TfpgTabSheet);
    function    DrawTab(const rect: TfpgRect; const Selected: Boolean = False; const Mode: Integer = 1): TfpgRect;
  protected
    procedure   OrderSheets; // currently using bubblesort
    procedure   RePaintTitles; virtual;
    procedure   HandlePaint; override;
    procedure   HandleShow; override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    AppendTabSheet(ATitle: string): TfpgTabSheet;
    procedure   RemoveTabSheet(ATabSheet: TfpgTabSheet);
    property    PageCount: Integer read GetPageCount;
    property    ActivePage: TfpgTabSheet read FActivePage write SetActivePage;
    property    Pages[AIndex: integer]: TfpgTabSheet read GetPage;
    property    OnChange: TTabSheetChange read FOnChange write FOnChange;
  published
    property    ActivePageIndex: integer read GetActivePageIndex write SetActivePageIndex;
    property    BackgroundColor;
    property    FixedTabWidth: integer read FFixedTabWidth write SetFixedTabWidth default 0;
    property    Hint;
    property    ParentShowHint;
    property    ShowHint;
    property    SortPages: boolean read FSortPages write SetSortPages default False;
    property    Style: TfpgTabStyle read FStyle write SetStyle default tsTabs;
    property    TabOrder;
    property    TabPosition: TfpgTabPosition read FTabPosition write SetTabPosition default tpTop;
    property    TextColor;
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
    PageControl.RePaintTitles;
end;

procedure TfpgTabSheet.HandlePaint;
begin
  inherited HandlePaint;
  Canvas.Clear(FBackgroundColor);
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
  if (AOwner <> nil) and (AOwner is TfpgPageControl) then
  begin
    FPageControl:=TfpgPageControl(AOwner);  
    FPageControl.InsertPage(self);
  end;
end;

destructor TfpgTabSheet.Destroy;
begin
  if FPageControl <> nil then
    FPageControl.RemovePage(self);
  inherited Destroy;
end;

procedure TfpgTabSheet.SetPageControl(APageControl: TfpgPageControl);
begin
   FPageControl:=APageControl;
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

procedure TfpgPageControl.InsertPage(const APage: TfpgTabSheet);
begin
  if FPages.IndexOf(APage) <> -1 then
    Exit; //==>   The page has already been added.
  FPages.Add(APage);
  { TODO: This behaviour could maybe be controlled by a Options property }
  if FPages.Count=1 then
    ActivePage := APage;
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

function TfpgPageControl.MaxButtonHeight: integer;
begin
  result := PageCount * ButtonHeight;
end;

function TfpgPageControl.MaxButtonWidth: integer;
var
   t: TfpgTabSheet;
   i: integer;
begin
  Result := 0;
  for i := 0 to FPages.Count-1 do
  begin
    t := TfpgTabSheet(FPages[i]);
    if ButtonWidth(t.Text) > Result then
      Result := ButtonWidth(t.Text);
  end;
end;

function TfpgPageControl.ButtonHeight: integer;
begin
  Result := FRightButton.Height;
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

function TfpgPageControl.FindNextPage(ACurrent: TfpgTabSheet; AForward: boolean
  ): TfpgTabSheet;
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
  RePaintTitles;
end;

procedure TfpgPageControl.SetTabPosition(const AValue: TfpgTabPosition);
begin
  if FTabPosition = AValue then
    Exit; //==>
  FTabPosition := AValue;
  RePaint;
end;

procedure TfpgPageControl.DoChange(ATabSheet: TfpgTabSheet);
begin
  if Assigned(FOnChange) then
    FOnChange(self, ATabSheet);
end;

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
    r.Height -= 1;

  Canvas.SetColor(clWindowBackground);
  case TabPosition of
  tpTop:
    begin  
      Canvas.FillRectangle(r.Left, r.Top, r.Width, r.Height-2);     // fill tab background
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
      Canvas.FillRectangle(r.Left, r.Top, r.Width-2, r.Height-2);   // fill tab background
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
      Canvas.SetColor(clRed);
    end;
  end; 
end;

procedure TfpgPageControl.OrderSheets;
begin
  FPages.Sort(@SortCompare);
end;

procedure TfpgPageControl.RePaintTitles;
var
  r2: TfpgRect;
  r3: TfpgRect;
  h: TfpgTabSheet;
  lp: integer;
  toffset: integer;
  dx: integer;
  lTxtFlags: TFTextFlags;
begin
  if not HasHandle then
    Exit; //==>
    
  if PageCount = 0 then
    Exit; //==>

  h := TfpgTabSheet(FPages.First);
  if h = nil then
    Exit; //==>
  
  Canvas.BeginDraw;
  Canvas.SetTextColor(TextColor);
  lTxtFlags := TextFlagsDflt;
  if not Enabled then
    Include(lTxtFlags, txtDisabled);

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
        if MaxButtonWidthSum > (Width-(FMargin*2)) then
        begin
          if FFirstTabButton = nil then
            FFirstTabButton := h
          else
            h := FFirstTabButton;
          FLeftButton.SetPosition(Width - FRightButton.Width * 2, Height - ButtonHeight, FRightButton.Height, FRightButton.Height);
          FRightButton.SetPosition(Width - FrightButton.Width, Height - ButtonHeight, FRightButton.Height, FRightButton.Height);
          FLeftButton.Visible   := True;
          FRightButton.Visible  := True;
        end
        else
        begin
          FLeftButton.Visible   := False;
          FRightButton.Visible  := False;
        end;
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
        if MaxButtonWidthSum > (Width-(FMargin*2)) then
        begin
          if FFirstTabButton = nil then
            FFirstTabButton := h
          else
            h := FFirstTabButton;
          FLeftButton.SetPosition(Width - FRightButton.Width * 2, FMargin, FRightButton.Height, FRightButton.Height);
          FRightButton.SetPosition(Width - FrightButton.Width, FMargin, FRightButton.Height, FRightButton.Height);
          FLeftButton.Visible   := True;
          FRightButton.Visible  := True;
        end
        else
        begin
          FLeftButton.Visible   := False;
          FRightButton.Visible  := False;
        end;

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
  end;  { case }
  Canvas.EndDraw;
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
  h: TfpgTabSheet;
  lp: integer;  // left position
  bw: integer;  // button width
  topy: integer;      // topy & bottomy is the y-range to detect clicks
  bottomy: integer;
begin
//  debugln('>> TfpgPageControl.HandleLMouseUp');
  h := TfpgTabSheet(FPages.First);
  if h = nil then
    Exit; //==>

  lp := FMargin;
  if MaxButtonWidthSum > (Width-(FMargin*2)) then
    h := FFirstTabButton;

  case TabPosition of
    tpTop:
      begin
        topy  := FMargin;
        bottomy   := ButtonHeight;
      end;
    tpBottom:
      begin
        topy  := Height - FMargin - Buttonheight;
        bottomy   := Height - FMargin;
      end;
  end;
  
  if (y > topy) and (y < bottomy) then
  begin
     while h <> nil do
     begin
        bw := ButtonWidth(h.Text);  // initialize button width
        if (x > lp) and (x < lp + bw) then
        begin
           if h <> ActivePage then
           begin
              ActivePage := h;
              DoChange(ActivePage);
           end;
           exit;
        end;  { if }
        lp := lp + bw;
        if h <> TfpgTabSheet(FPages.Last) then
           h := TfpgTabSheet(FPages[FPages.IndexOf(h)+1])
        else
           h := nil;
     end;  { while }
  end;  { if }
  inherited HandleLMouseUp(x, y, shiftstate);
end;

procedure TfpgPageControl.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  i: integer;
begin
//  writeln(Classname, '.Keypress');
  consumed := True;
  i := ActivePageIndex;
  if ssAlt in shiftstate then
  case keycode of
    keyLeft:
        begin
          if ActivePage <> TfpgTabSheet(FPages.First) then
          begin
            ActivePage := TfpgTabSheet(FPages[i-1]);
            DoChange(ActivePage);
          end;
        end;

    keyRight:
        begin
          if ActivePage <> TfpgTabSheet(FPages.Last) then
          begin
            ActivePage := TfpgTabSheet(FPages[i+1]);
            DoChange(ActivePage);
          end;
        end;

  else
    consumed := False;
  end;  { case/else }
  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

constructor TfpgPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont   := fpgStyle.DefaultFont;
  FPages  := TList.Create;
  FWidth  := 150;
  FHeight := 100;
  FIsContainer := True;

  FTextColor        := Parent.TextColor;
  FBackgroundColor  := Parent.BackgroundColor;
  FFocusable        := True;
  FOnChange         := nil;
  FFixedTabWidth    := 0;
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

