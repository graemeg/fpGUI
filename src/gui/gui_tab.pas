unit gui_tab;

{$mode objfpc}{$H+}

{
  Incomplete:  I'm currently busy developing this component.
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
  
  TfpgTabStyle = (tsTabs, tsButtons, tsFlatButtons);
  TfpgTabPosition = (tpTop, tpBottom{, tpLeft, tpRight});

  TfpgTabSheet = class(TfpgWidget)
  private
    FBackgroundColor: TfpgColor;
    FText: string;
    function    GetPageControl: TfpgPageControl;
    function    GetPageIndex: Integer;
    function    GetText: string;
    procedure   SetBackgroundColor(const AValue: TfpgColor);
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
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor;
  end;


  TTabSheetChange = procedure(Sender: TObject; NewActiveSheet: TfpgTabSheet);
  
  
  TfpgPageControl = class(TfpgWidget)
  private
    FBackgroundColor: TfpgColor;
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
    procedure   SetBackgroundColor(const AValue: TfpgColor);
    procedure   SetFixedTabWidth(const AValue: integer);
    function    GetTabText(AText: string): string;
    procedure   LeftButtonClick(Sender: TObject);
    procedure   RightButtonClick(Sender: TObject);
    function    FindNextPage(ACurrent: TfpgTabSheet; AForward: boolean): TfpgTabSheet;
    procedure   SetSortPages(const AValue: boolean);
    procedure   SetStyle(const AValue: TfpgTabStyle);
    procedure   SetTabPosition(const AValue: TfpgTabPosition);
    procedure   DoChange(ATabSheet: TfpgTabSheet);
  protected
    procedure   OrderSheets; // currently using bubblesort
    procedure   RePaintTitles; virtual;
    procedure   HandlePaint; override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    AppendTabSheet(ATitle: string): TfpgTabSheet;
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor;
    property    PageCount: Integer read GetPageCount;
    property    ActivePageIndex: integer read GetActivePageIndex write SetActivePageIndex;
    property    ActivePage: TfpgTabSheet read FActivePage write SetActivePage;
    property    FixedTabWidth: integer read FFixedTabWidth write SetFixedTabWidth;
    property    Style: TfpgTabStyle read FStyle write SetStyle;
    property    TabPosition: TfpgTabPosition read FTabPosition write SetTabPosition;
    property    SortPages: boolean read FSortPages write SetSortPages;
    property    OnChange: TTabSheetChange read FOnChange write FOnChange;
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

procedure TfpgTabSheet.SetBackgroundColor(const AValue: TfpgColor);
begin
  if FBackgroundColor = AValue then
    Exit; //==>
  FBackgroundColor := AValue;
  RePaint;
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
  Canvas.BeginDraw;
  inherited HandlePaint;
  Canvas.Clear(FBackgroundColor);
  Canvas.EndDraw;
end;

constructor TfpgTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText := '';
  FFocusable := True;
  FBackgroundColor := clWindowBackground;
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
//    FActivePage := FindNextPage(APage, True);
    ActivePage := TfpgTabSheet(FPages.First);
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

procedure TfpgPageControl.SetBackgroundColor(const AValue: TfpgColor);
begin
  if FBackgroundColor = AValue then
    Exit; //==>
  FBackgroundColor := AValue;
  RePaint;
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

procedure TfpgPageControl.OrderSheets;
begin
  FPages.Sort(@SortCompare);
end;

procedure TfpgPageControl.RePaintTitles;
var
  i: integer;
  r: TfpgRect;
  h: TfpgTabSheet;
  lp: integer;
  toffset: integer;
begin
  if not HasHandle then
    Exit; //==>
    
  if PageCount = 0 then
    Exit; //==>

  h := TfpgTabSheet(FPages.First);
  Canvas.BeginDraw;
  Canvas.SetTextColor(clText1);
  
  case TabPosition of
    tpTop:
        begin
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
          Canvas.SetColor(clHilite1);
          Canvas.DrawLine(FMargin, ButtonHeight, FMargin, Height-(FMargin*2));
          Canvas.SetColor(clHilite2);
          Canvas.DrawLine(FMargin+1, ButtonHeight+1, FMargin+1, Height - (FMargin*2) - 1);
          Canvas.SetColor(clShadow2);
          Canvas.DrawLine(FMargin, Height - (FMargin*2), Width - (FMargin*2), Height - (FMargin*2));
          Canvas.DrawLine(Width - FMargin - 1, FMargin + ButtonHeight - 1, Width - FMargin - 1, Height - FMargin);
          Canvas.SetColor(clShadow1);
          Canvas.DrawLine(FMargin + 1, Height - (FMargin*2) - 1, Width - (FMargin*2) - 1, Height - (FMargin*2) - 1);
          Canvas.DrawLine(Width - FMargin - 2, FMargin + ButtonHeight - 1, Width - FMargin - 2, Height - FMargin - 2);
          Canvas.SetClipRect(r);
          lp := 0;
          while h <> nil do
          begin
            if h <> ActivePage then
            begin
              toffset := 4;
              Canvas.SetColor(clHilite1);
              Canvas.DrawLine(FMargin + lp, FMargin + ButtonHeight - 2, FMargin + lp + ButtonWidth(h.Text), FMargin + ButtonHeight - 2);
              Canvas.SetColor(clHilite2);
              Canvas.DrawLine(FMargin + lp, FMargin + ButtonHeight - 1, FMargin + lp + ButtonWidth(h.Text) + 1, FMargin + ButtonHeight - 1);
              Canvas.SetColor(clShadow1);
              Canvas.DrawLine(lp + FMargin + ButtonWidth(h.Text), FMargin, lp + FMargin + ButtonWidth(h.Text), FMargin + ButtonHeight - 3);
              h.Visible := False;
            end
            else
            begin
              toffset := 2;
              h.Visible := True;
              h.SetPosition(FMargin+2, FMargin + ButtonHeight, Width - (FMargin*2) - 4, Height - (FMargin*2) - ButtonHeight - 2);
              Canvas.SetColor(clHilite1);
              Canvas.DrawLine(lp + FMargin, FMargin, lp + FMargin + ButtonWidth(h.Text)-1, FMargin);
              Canvas.DrawLine(lp + FMargin, FMargin, lp + FMargin, FMargin + ButtonHeight - 2);
              Canvas.SetColor(clHilite2);
              Canvas.DrawLine(lp + FMargin + 1, FMargin + 1, lp + FMargin + ButtonWidth(h.Text) - 2, FMargin + 1);
              Canvas.DrawLine(lp + FMargin + 1, FMargin + 1, lp + FMargin + 1, FMargin + ButtonHeight - 1);
              Canvas.SetColor(clShadow1);
              Canvas.DrawLine(lp + FMargin + ButtonWidth(h.Text) - 2,FMargin + 1, lp + FMargin + ButtonWidth(h.Text) - 2, FMargin + ButtonHeight-1);
              Canvas.SetColor(clShadow2);
              Canvas.DrawLine(lp + FMargin + ButtonWidth(h.Text) - 1,FMargin + 1, lp + FMargin + ButtonWidth(h.Text) - 1, FMargin + ButtonHeight - 2);
            end;
            // paint text
            Canvas.DrawString(lp + (ButtonWidth(h.Text) div 2) - FFont.TextWidth(GetTabText(h.Text)) div 2, FMargin+toffset, GetTabText(h.Text));

            lp := lp + ButtonWidth(h.Text);
            if h <> TfpgTabSheet(FPages.Last) then
              h := TfpgTabSheet(FPages[FPages.IndexOf(h)+1])
            else
              h := nil;
          end;  { while }
          Canvas.SetColor(clHilite1);
          Canvas.Drawline(lp + 1, FMargin + ButtonHeight - 2, Width, FMargin + ButtonHeight - 2);
          Canvas.SetColor(clHilite2);
          Canvas.Drawline(lp + 1, FMargin + ButtonHeight - 1, Width, FMargin + ButtonHeight - 1);
        end;

    tpBottom:
        begin
        end;
  end;
  
  Canvas.EndDraw;
end;

procedure TfpgPageControl.HandlePaint;
begin
  Canvas.BeginDraw;
//  inherited HandlePaint;
  
  if SortPages then
    OrderSheets;
  Canvas.ClearClipRect;
  Canvas.Clear(FBackgroundColor);
  if Focused then
    Canvas.SetColor(clWidgetFrame)
  else
    Canvas.SetColor(clInactiveWgFrame);
  Canvas.DrawRectangle(0, 0, Width-1, Height-1);
  RePaintTitles;

  Canvas.EndDraw;
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
{          if (y > Height - FMargin - buttonheight) and (y < height - FMargin) then
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
}
        end;
  end;  { case }
  inherited HandleLMouseUp(x, y, shiftstate);
end;

procedure TfpgPageControl.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  t: TfpgTabSheet;
  i: integer;
begin
//  writeln(Classname, '.Keypress');
  consumed := True;
  i := ActivePageIndex;
  t := ActivePage;
  
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

  FBackgroundColor  := clWindowBackground;
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
  FLeftButton.Width     := FLeftButton.Height;
  FLeftButton.Visible   := False;
  FLeftButton.OnClick   := @LeftButtonClick;

  FRightButton := TfpgButton.Create(self);
  FRightButton.Text     := '>';
  FRightButton.Width    := FRightButton.Height;
  FRightButton.Visible  := False;
  FRightButton.OnClick  := @RightButtonClick;
end;

destructor TfpgPageControl.Destroy;
var
  ts: TfpgTabSheet;
begin
  while FPages.Count > 0 do
  begin
    ts := TfpgTabSheet(FPages.Last);
    FPages.Remove(ts);
    ts.Free;
  end;
  FPages.Free;

  FFirstTabButton := nil;
  FOnChange := nil;
  inherited Destroy;
end;

function TfpgPageControl.AppendTabSheet(ATitle: string): TfpgTabSheet;
var
//  h: PTabSheetList;
  nt: TfpgTabSheet;
begin
//  h := FFirstTabSheet;
  nt := TfpgTabSheet.Create(self);
  nt.Text := ATitle;
  //if h = nil then
    //FFirstTabSheet := nl
  //else
  //begin
    //while h^.next <> nil do
      //h := h^.next;
    //h^.next := nl;
    //nl^.prev := h;
  //end;
  result := nt;
end;

end.

