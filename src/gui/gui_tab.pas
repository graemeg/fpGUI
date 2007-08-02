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
    FText: string;
    function    GetPageControl: TfpgPageControl;
    function    GetPageIndex: Integer;
    function    GetText: string;
//    procedure   SetPageControl(const AValue: TfpgPageControl);
    procedure   SetPageIndex(const AValue: Integer);
    procedure   SetText(const AValue: string);
  protected
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Text: string read GetText write SetText;
    property    PageIndex: Integer read GetPageIndex write SetPageIndex stored False;
    property    PageControl: TfpgPageControl read GetPageControl; //write SetPageControl;
  end;


  TTabSheetChange = procedure(Sender: TObject; NewActiveSheet: TfpgTabSheet);
  
  
  TfpgPageControl = class(TfpgWidget)
  private
    FBackgroundColor: TfpgColor;
    FFont: TfpgFont;
    FActiveSheet: TfpgTabSheet;
    FMargin: integer;
    FFixedTabWidth: integer;
    FPages: TList;
    FActivePageIndex: integer;
    FOnChange: TTabSheetChange;
    FRightButton: TfpgButton;
    FLeftButton: TfpgButton;
    FFirstTabSheet: TfpgTabSheet;
    FFirstTabButton: TfpgTabSheet;
    FStyle: TfpgTabStyle;
    FTabPosition: TfpgTabPosition;
    function    GetActivePageIndex: integer;
    function    GetPageCount: Integer;
    procedure   InsertPage(const APage: TfpgTabSheet);
    procedure   RemovePage(const APage: TfpgTabSheet);
    procedure   SetActiveSheet(const AValue: TfpgTabSheet);
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
    procedure   SetStyle(const AValue: TfpgTabStyle);
    procedure   SetTabPosition(const AValue: TfpgTabPosition);
  protected
//    procedure   UnregisterTabSheet(ATabSheet: TfpgTabSheet);
//    procedure   RegisterTabSheet(ATabSheet: TfpgTabSheet);
    procedure   OrderSheets; // currently using bubblesort
    procedure   RePaintTitles; virtual;
    procedure   HandlePaint; override;
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    AppendTabSheet(ATitle: string): TfpgTabSheet;
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor;
    property    PageCount: Integer read GetPageCount;
    property    ActivePageIndex: integer read GetActivePageIndex write FActivePageIndex;
    property    ActivePage: TfpgTabSheet read FActiveSheet write SetActiveSheet;
    property    FixedTabWidth: integer read FFixedTabWidth write SetFixedTabWidth;
    property    Style: TfpgTabStyle read FStyle write SetStyle;
    property    TabPosition: TfpgTabPosition read FTabPosition write SetTabPosition;
    property    OnChange: TTabSheetChange read FOnChange write FOnChange;
  end;


implementation

uses
  gfx_UTF8utils;

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
{
procedure TfpgTabSheet.SetPageControl(const AValue: TfpgPageControl);
begin
  if PageControl <> AValue then
  begin
    if PageControl <> nil then
      PageControl.RemovePage(self);
//    Owner := AValue;
    if AValue <> nil then
      AValue.InsertPage(self);
  end;
end;
}
procedure TfpgTabSheet.SetPageIndex(const AValue: Integer);
begin

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
//  inherited HandlePaint;
  Canvas.Clear(clWindowBackground);
  Canvas.EndDraw;
end;

constructor TfpgTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFocusable := True;
  if Owner is TfpgPageControl then
  begin
    TfpgPageControl(Owner).InsertPage(self);
//    TfpgPageControl(Owner).RegisterTabSheet(self);
//    FPageIndex := TfpgPageControl(Owner).PageCount + 1;
  end;
end;

destructor TfpgTabSheet.Destroy;
begin
  if Owner is TfpgPageControl then
    TfpgPageControl(Owner).RemovePage(self);
//    TfpgPageControl(Owner).UnregisterTabSheet(self);
  inherited Destroy;
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
var
  i: integer;
begin
  if FPages.IndexOf(APage) <> -1 then
    Exit; //==>   The page has already been added.
  i := FPages.Add(APage);
  ActivePageIndex := i;
  FActiveSheet := APage;
  RePaint;
end;

procedure TfpgPageControl.RemovePage(const APage: TfpgTabSheet);
begin
  FPages.Remove(APage);
  {$Note This still needs to be fixed.}
  if APage = FActiveSheet then
//    FActiveSheet := FindNextPage(APage, True);
    FActiveSheet := TfpgTabSheet(FPages.First);
end;

procedure TfpgPageControl.SetActiveSheet(const AValue: TfpgTabSheet);
begin
  if FActiveSheet = AValue then
    Exit; //==>
  FActiveSheet := AValue;
  ActiveWidget := AValue;
  RePaint;
end;

function TfpgPageControl.MaxButtonWidthSum: integer;
begin
  Result := 0;
end;

function TfpgPageControl.MaxButtonHeight: integer;
begin

end;

function TfpgPageControl.MaxButtonWidth: integer;
begin

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
//    if FPages.IndexOf(FFirstTabButton) <> 0 then
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
//    if FPages.IndexOf(FFirstTabButton) <> (FPages.Count-1) then
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

procedure TfpgPageControl.OrderSheets;
begin

end;

procedure TfpgPageControl.RePaintTitles;
var
  i: integer;
  r: TRect;
  h: TfpgTabSheet;
  lp: integer;
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
            r := Rect(FMargin, FMargin, Width - FMargin * 2 - FRightButton.Width * 2 - 1, FRightButton.Height);
            FLeftButton.SetPosition(Width - FMargin * 2 - FRightButton.Width * 2, FMargin, FRightButton.Height, FRightButton.Height);
            FRightButton.SetPosition(Width - FMargin * 2 - FrightButton.Width, FMargin, FRightButton.Height, FRightButton.Height);
            FLeftButton.Visible := True;
            FRightButton.Visible := True;
          end
          else
          begin
            r := Rect(FMargin, FMargin, Width - (FMargin*2), ButtonHeight);
            FLeftButton.Visible := False;
            FRightButton.Visible := False;
          end;
          Canvas.SetColor(clHilite1);
          Canvas.DrawLine(FMargin,ButtonHeight, FMargin, Height - FMargin * 2);
          Canvas.SetColor(clHilite2);
          Canvas.DrawLine(FMargin+1,ButtonHeight+1, FMargin+1, Height - FMargin * 2 - 1);
          Canvas.SetColor(clShadow2);
          Canvas.DrawLine(FMargin, Height - FMargin * 2, Width - FMargin * 2, Height - FMargin * 2);
          Canvas.DrawLine(Width - FMargin - 1, FMargin + ButtonHeight - 1, Width - FMargin - 1, Height - FMargin);
          Canvas.SetColor(clShadow1);
          Canvas.DrawLine(FMargin + 1, Height - FMargin * 2 - 1, Width - FMargin * 2 - 1, Height - FMargin * 2 - 1);
          Canvas.DrawLine(Width - FMargin - 2, FMargin + ButtonHeight - 1, Width - FMargin - 2, Height - FMargin - 2);
          Canvas.SetClipRect(r);
          lp := 0;
          while h <> nil do
          begin
            if h <> ActivePage then
            begin
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
                h.Visible := True;
                h.SetPosition(FMargin+2, FMargin + ButtonHeight, Width - FMargin * 2 - 4, Height - FMargin * 2 - ButtonHeight - 2);
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
            Canvas.DrawString(lp + (ButtonWidth(h.Text) div 2) - FFont.TextWidth(GetTabText(h.Text)) div 2, FMargin, GetTabText(h.Text));
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

procedure TfpgPageControl.HandleResize(awidth, aheight: TfpgCoord);
begin
  inherited HandleResize(awidth, aheight);
  RePaint;
end;

constructor TfpgPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackgroundColor := clWindowBackground;
  FFocusable := True;
  FPages := TList.Create;
  FOnChange := nil;
  FFixedTabWidth := 0;
  FFont := fpgStyle.DefaultFont;
  FFirstTabButton := nil;
  FStyle := tsTabs;
  FTabPosition := tpTop;
  FMargin := 1;

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

