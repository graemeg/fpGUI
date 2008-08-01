{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2008 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a Page Control and Tab Sheets.
}

unit gui_tab;

{$mode objfpc}{$H+}

{
  TODO:
    * Tab Styles (tab, button, flat button, angled)
    * Tab Position (top, bottom, left, right)
    * Better keyboard support
    * Focus rectangle drawn on tabs itself.gui_tab
    * FindNextPage() must be implemented
}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_widget,
  gui_button;
  
type
  // forward declaration
  TfpgPageControl = class;
  
  TfpgTabStyle    = (tsTabs, tsButtons, tsFlatButtons);
  TfpgTabPosition = (tpTop, tpBottom{, tpLeft, tpRight});


  TfpgTabSheet = class(TfpgWidget)
  private
    FText: string;
    function    GetPageControl: TfpgPageControl;
    function    GetPageIndex: Integer;
    function    GetText: string;
    procedure   SetPageIndex(const AValue: Integer);
    procedure   SetText(const AValue: string);
  protected
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterConstruction; override;
    property    Text: string read GetText write SetText;
    property    PageIndex: Integer read GetPageIndex write SetPageIndex;
    property    PageControl: TfpgPageControl read GetPageControl;
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
    property    PageCount: Integer read GetPageCount;
    property    ActivePage: TfpgTabSheet read FActivePage write SetActivePage;
    property    Pages[AIndex: integer]: TfpgTabSheet read GetPage;
    property    OnChange: TTabSheetChange read FOnChange write FOnChange;
  published
    property    ActivePageIndex: integer read GetActivePageIndex write SetActivePageIndex;
    property    BackgroundColor;
    property    FixedTabWidth: integer read FFixedTabWidth write SetFixedTabWidth default 0;
    property    SortPages: boolean read FSortPages write SetSortPages default False;
    property    Style: TfpgTabStyle read FStyle write SetStyle default tsTabs;
    property    TabOrder;
    property    TabPosition: TfpgTabPosition read FTabPosition write SetTabPosition default tpTop;
    property    TextColor;
  end;


implementation

uses
  gfx_UTF8utils;
  
  
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
  FFocusable := True;
  FBackgroundColor := Parent.BackgroundColor;
  FTextColor := Parent.TextColor;
  FIsContainer := True;
end;

destructor TfpgTabSheet.Destroy;
begin
  if Owner is TfpgPageControl then
    TfpgPageControl(Owner).RemovePage(self);
  inherited Destroy;
end;

procedure TfpgTabSheet.AfterConstruction;
begin
  inherited AfterConstruction;
  if Owner is TfpgPageControl then
    TfpgPageControl(Owner).InsertPage(self);
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
  ActivePage := APage;
end;

procedure TfpgPageControl.RemovePage(const APage: TfpgTabSheet);
begin
  FPages.Remove(APage);
  {$Note This still needs to be fixed.}
  if APage = FActivePage then
  begin
//    FActivePage := FindNextPage(APage, True);
//    if FPages.Count > 0 then
      ActivePage := TfpgTabSheet(FPages.First);
//    else
//      ActivePage := nil;
  end;
end;

procedure TfpgPageControl.SetActivePageIndex(const AValue: integer);
begin
  if (AValue >= 0) or (AValue < FPages.Count) then
    ActivePage := TfpgTabSheet(FPages[AValue]);
end;

procedure TfpgPageControl.SetActivePage(const AValue: TfpgTabSheet);
begin
  if FActivePage = AValue then
    Exit; //==>
  FActivePage := AValue;
  ActiveWidget := AValue;
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
      Delete(s1, length(s1), 1);    {$Note This must become a UTF8 function}
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
    r.Height := r.Height - 1;

  Canvas.SetColor(clButtonFace);
  Canvas.FillRectangle(r.Left, r.Top, r.Width, r.Height-2);
  Canvas.SetColor(clHilite2);
  Canvas.DrawLine(r.Left, r.Bottom-2, r.Left, r.Top+2);
  Canvas.DrawLine(r.Left, r.Top+2, r.Left+2, r.Top);
  Canvas.DrawLine(r.Left+2, r.Top, r.Right-1, r.Top);
  Canvas.SetColor(clShadow1);
  Canvas.DrawLine(r.Right-1, r.Top+1, r.Right-1, r.Bottom-1);
  Canvas.SetColor(clShadow2);
  Canvas.DrawLine(r.Right-1, r.Top+1, r.Right, r.Top+2);
  Canvas.DrawLine(r.Right, r.Top+2, r.Right, r.Bottom-1);
end;

procedure TfpgPageControl.OrderSheets;
begin
  FPages.Sort(@SortCompare);
end;

procedure TfpgPageControl.RePaintTitles;
var
  r: TfpgRect;
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
    Exit;
  Canvas.BeginDraw;
  Canvas.SetTextColor(TextColor);
  lTxtFlags := TextFlagsDflt;
  if not Enabled then
    Include(lTxtFlags, txtDisabled);

  case TabPosition of
    tpBottom:
        begin
(*
          if MaxButtonWidthSum > (Width-(FMargin*2)) then
          begin
            if FFirstTabButton = nil then
              FFirstTabButton := h
            else
              h := FFirstTabButton;
            r.SetRect(FMargin, FMargin, Width-(FMargin*2)-(FRightButton.Width*2)-1, FRightButton.Height);
            FLeftButton.SetPosition(Width - FMargin * 2 - FRightButton.Width * 2, FMargin, FRightButton.Height, FRightButton.Height);
            FRightButton.SetPosition(Width - FMargin * 2 - FrightButton.Width, FMargin, FRightButton.Height, FRightButton.Height);
            FLeftButton.Visible   := True;
            FRightButton.Visible  := True;
          end
          else
          begin
            r.SetRect(FMargin, FMargin, Width-(FMargin*2), ButtonHeight);
            FLeftButton.Visible   := False;
            FRightButton.Visible  := False;
          end;
          // tabsheet area - left outer line
          Canvas.SetColor(clHilite1);
          Canvas.DrawLine(FMargin, ButtonHeight, FMargin, Height-(FMargin*2));
          // tabsheet area - left inner line
          Canvas.SetColor(clHilite2);
          Canvas.DrawLine(FMargin+1, ButtonHeight+1, FMargin+1, Height - (FMargin*2) - 1);
          // tabsheet area - outer bottom & right line
          Canvas.SetColor(clShadow2);
          Canvas.DrawLine(FMargin, Height - (FMargin*2), Width - (FMargin*2), Height - (FMargin*2));
          Canvas.DrawLine(Width - (FMargin*2), Height - (FMargin*2), Width - (FMargin*2), FMargin + ButtonHeight - 3);
          // tabsheet area - inner bottom & right line
          Canvas.SetColor(clShadow1);
          Canvas.DrawLine(FMargin + 1, Height - (FMargin*2) - 1, Width - (FMargin*2) - 1, Height - (FMargin*2) - 1);
          Canvas.DrawLine(Width - FMargin - 2, Height - FMargin - 2, Width - FMargin - 2, FMargin + ButtonHeight - 2);
          Canvas.SetClipRect(r);
          lp := 0;
          while h <> nil do
          begin
            if h <> ActivePage then
            begin
              toffset := 4;
              // tabsheet area - top lines under inactive tabs
              Canvas.SetColor(clHilite1);
              Canvas.DrawLine(FMargin + lp, FMargin + ButtonHeight - 2, FMargin + lp + ButtonWidth(h.Text), FMargin + ButtonHeight - 2);
              Canvas.SetColor(clHilite2);
              if TfpgTabSheet(FPages.First) = h then
                dx := 1
              else
                dx := -1;
              Canvas.DrawLine(FMargin + lp+dx, FMargin + ButtonHeight - 1, FMargin + lp + ButtonWidth(h.Text) + 1, FMargin + ButtonHeight - 1);
              // vertical divider line between inactive tabs
              Canvas.SetColor(clShadow1);
              Canvas.DrawLine(lp + FMargin + ButtonWidth(h.Text), FMargin, lp + FMargin + ButtonWidth(h.Text), FMargin + ButtonHeight - 2);
              h.Visible := False;
            end
            else
            begin
              toffset := 2;
              h.Visible := True;
              h.SetPosition(FMargin+2, FMargin + ButtonHeight, Width - (FMargin*2) - 4, Height - (FMargin*2) - ButtonHeight - 2);
              // tab outer left & top line
              Canvas.SetColor(clHilite1);
              Canvas.DrawLine(lp + FMargin, FMargin + ButtonHeight - 2, lp + FMargin, FMargin);
              Canvas.DrawLine(lp + FMargin, FMargin, lp + FMargin + ButtonWidth(h.Text)-1, FMargin);
              // tab inner left & top line
              Canvas.SetColor(clHilite2);
              Canvas.DrawLine(lp + FMargin + 1, FMargin + ButtonHeight - 1, lp + FMargin + 1, FMargin + 1);
              Canvas.DrawLine(lp + FMargin + 1, FMargin + 1, lp + FMargin + ButtonWidth(h.Text) - 2, FMargin + 1);
              // tab inner right line
              Canvas.SetColor(clShadow1);
              Canvas.DrawLine(lp + FMargin + ButtonWidth(h.Text) - 2, FMargin + 1, lp + FMargin + ButtonWidth(h.Text) - 2, FMargin + ButtonHeight);
              // tab outer right line
              Canvas.SetColor(clShadow2);
              Canvas.DrawLine(lp + FMargin + ButtonWidth(h.Text) - 1, FMargin, lp + FMargin + ButtonWidth(h.Text) - 1, FMargin + ButtonHeight-1);
            end;
            // paint text
            Canvas.DrawString(lp + (ButtonWidth(h.Text) div 2) - FFont.TextWidth(GetTabText(h.Text)) div 2, FMargin+toffset, GetTabText(h.Text));

            lp := lp + ButtonWidth(h.Text);
            if h <> TfpgTabSheet(FPages.Last) then
              h := TfpgTabSheet(FPages[FPages.IndexOf(h)+1])
            else
              h := nil;
          end;  { while }
          // tabsheet area - top lines on right of tabs
          Canvas.SetColor(clHilite1);
          Canvas.Drawline(lp + 1, FMargin + ButtonHeight - 2, Width - (FMargin*2), FMargin + ButtonHeight - 2);
          Canvas.SetColor(clHilite2);
          Canvas.Drawline(lp , FMargin + ButtonHeight - 1, Width - (FMargin*2)-1, FMargin + ButtonHeight - 1);
*)
        end;

    tpTop:
        begin
          if MaxButtonWidthSum > (Width-(FMargin*2)) then
          begin
            if FFirstTabButton = nil then
              FFirstTabButton := h
            else
              h := FFirstTabButton;
            r.SetRect(FMargin, FMargin, Width-(FMargin*2)-(FRightButton.Width*2)-1, FRightButton.Height);
            FLeftButton.SetPosition(Width - FRightButton.Width * 2, FMargin, FRightButton.Height, FRightButton.Height);
            FRightButton.SetPosition(Width - FrightButton.Width, FMargin, FRightButton.Height, FRightButton.Height);
            FLeftButton.Visible   := True;
            FRightButton.Visible  := True;
          end
          else
          begin
            r.SetRect(FMargin, FMargin, Width-(FMargin*2), ButtonHeight);
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
              Canvas.DrawText(lp + (ButtonWidth(h.Text) div 2) - FFont.TextWidth(GetTabText(h.Text)) div 2, FMargin+toffset, GetTabText(h.Text), lTxtFlags);

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
  end;
  
  Canvas.EndDraw;
end;

procedure TfpgPageControl.HandlePaint;
begin
  inherited HandlePaint;
  if SortPages then
    OrderSheets;
  Canvas.ClearClipRect;
  Canvas.Clear(FBackgroundColor);
  
  // To make it more visible in the UI Designer
  if csDesigning in ComponentState then
  begin
    Canvas.SetColor(clInactiveWgFrame);
    Canvas.DrawRectangle(0, 0, Width, Height);
    Canvas.SetTextColor(clText1);
    Canvas.DrawString(2, 2, Name + ': ' + Classname);
  end;
  
  if TabPosition = tpBottom then
  begin
    if Focused then
      Canvas.SetColor(clWidgetFrame)
    else
      Canvas.SetColor(clInactiveWgFrame);
    Canvas.DrawRectangle(0, 0, Width, Height);
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
begin
  h := TfpgTabSheet(FPages.First);
  if h = nil then
    Exit; //==>

  lp := FMargin;
  if MaxButtonWidthSum > (Width-(FMargin*2)) then
    h := FFirstTabButton;

  case TabPosition of
    tpTop:
        begin
          if (y > FMargin) and (y < ButtonHeight) then
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
        end;
        
    tpBottom:
        begin
(*
          if (y > Height - FMargin - buttonheight) and (y < height - FMargin) then
          begin
            while h <> nil do
            begin
              bw := ButtonWidth(h^.TabSheet.Text);  // initialize button width
              if (x > lp) and (x < lp + bw) then
              begin
                if h^.TabSheet <> ActiveTabSheet then
                begin
                    ActiveTabSheet := h^.TabSheet;
                    DoChange(ActiveTabSheet);
                end;
                exit;
              end;
              lp := lp + bw;
              h := h^.next;
            end;  { while }
          end;  { if }
*)
        end;
  end;  { case }
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
var
  ts: TfpgTabSheet;
begin
  FOnChange := nil;
  if FPages.Count > 0 then
    FActivePage := TfpgTabSheet(FPages[0]);
  ActiveWidget := nil;
  while FPages.Count > 0 do
  begin
    ts := TfpgTabSheet(FPages.Last);
    FPages.Remove(ts);
    ts.Free;
  end;
  FPages.Free;
  FFirstTabButton := nil;
  inherited Destroy;
end;

function TfpgPageControl.AppendTabSheet(ATitle: string): TfpgTabSheet;
begin
  Result := TfpgTabSheet.Create(self);
  Result.Text := ATitle;
  InsertPage(Result);
end;

end.

