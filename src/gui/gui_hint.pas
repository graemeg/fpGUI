unit gui_hint; 

{$mode objfpc}{$H+}

{.$Define Debug}

interface

uses
  Classes, SysUtils,
  fpgfx, gfxbase,
  gui_form, gui_label;
  
type
  TF_Hint = class(TfpgForm)
  private
    FText: string;
    FFont: TfpgFont;
    FTime: Integer;
    FShadow: Integer;
    FBorder: Integer;
    FMargin: Integer;
    L_Hint: TfpgLabel;
    T_Chrono: TfpgTimer;
    procedure   FormShow(Sender: TObject);
    procedure   FormHide(Sender: TObject);
    procedure   T_ChronoFini(Sender: TObject);
    procedure   SetShadow(AValue: Integer);
    procedure   SetBorder(AValue: Integer);
    procedure   SetTime(AValue: Integer);
    procedure   SetLTextColor(AValue: Tfpgcolor);
    procedure   SetLBackgroundColor(AValue: Tfpgcolor);
    procedure   SetShadowColor(AValue: TfpgColor);
  protected
    property    Font: TfpgFont read FFont;
  public
    constructor Create(AOwner: TComponent); override;
    property    Text: string read FText write FText;
    property    Shadow: Integer read FShadow write SetShadow default 5;
    property    Border: Integer read FBorder write SetBorder default 1;
    property    Margin: Integer read FMargin write FMargin default 3;
    property    LTextColor: TfpgColor write SetLTextColor default clBlack;
    property    LBackgroundColor: TfpgColor write SetLBackgroundColor default clHintWindow;
    property    ShadowColor: TfpgColor write SetShadowColor default clGray;
    property    Time: Integer write SetTime default 5000;
  end;


  TF_Shadow = class(TfpgForm)
  public
    constructor Create(AOwner: TComponent); override;
  end;


var
  F_Hint: TF_Hint;
  F_Shadow: TF_Shadow;


procedure DisplayHint(Pt: TPoint; AHint: string);
procedure HideHint;


implementation


procedure DisplayHint(Pt: TPoint; AHint: string);
begin
  {$IFDEF DEBUG}
  writeln('DisplayHint');
  {$ENDIF}
  if Assigned(F_Hint) and F_Hint.Visible then
    Exit; //==>  Nothing to do

  with F_Hint do
  begin
    L_Hint.Text := AHint;
    Width := FFont.TextWidth(AHint) + (Border * 2) + (Margin * 2);
    Height := FFont.Height + (Border * 2) + (Margin * 2);
    if Shadow > 0 then
    begin
      F_Shadow.SetPosition(Pt.X+Shadow, Pt.Y+Shadow, Width, Height);
      F_Shadow.Show;
    end;
    L_Hint.SetPosition(Border, Border, Width - (Border * 2), Height - (Border * 2));
    SetPosition(Pt.X, Pt.Y, Width, Height);
    Show;
  end;
end;

procedure HideHint;
begin
  {$IFDEF DEBUG}
  writeln('HideHint');
  {$ENDIF}
  if Assigned(F_Hint) and F_Hint.Visible then
    F_Hint.Hide;
end;


{ TF_Hint }

procedure TF_Hint.FormShow(Sender: TObject);
begin
  T_Chrono.Enabled:= True;
end;

procedure TF_Hint.FormHide(Sender: TObject);
begin
  T_Chrono.Enabled := False;
  if Assigned(F_Shadow) then
    F_Shadow.Hide;
end;

procedure TF_Hint.T_ChronoFini(Sender: TObject);
begin
  {$IFDEF DEBUG}
  writeln('TF_Hint.T_ChronoFini timer fired');
  {$ENDIF}
  Hide;
end;

procedure TF_Hint.SetShadow(AValue: Integer);
begin
  if FShadow <> AValue then
    FShadow := AValue;
end;

procedure TF_Hint.SetBorder(AValue: Integer);
begin
  if FBorder <> AValue then
    FBorder := AValue;
end;

procedure TF_Hint.SetTime(AValue: Integer);
begin
  if FTime <> AValue then
  begin
    FTime := AValue;
    T_Chrono.Interval := FTime;
  end;
end;

procedure TF_Hint.SetLTextColor(AValue: Tfpgcolor);
begin
  if L_Hint.TextColor <> AValue then
    L_Hint.TextColor := AValue
end;

procedure TF_Hint.SetLBackgroundColor(AValue: Tfpgcolor);
begin
  if L_Hint.BackgroundColor <> AValue then
    L_Hint.BackgroundColor := AValue
end;

procedure TF_Hint.SetShadowColor(AValue: Tfpgcolor);
begin
  if F_Shadow.BackgroundColor <> AValue then
    F_Shadow.BackgroundColor := AValue;
end;

constructor TF_Hint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'F_Hint';
  WindowPosition := wpUser;
  WindowType := wtPopup;
//  WindowAttributes := [waBorderless];
//  BorderLess := True;
  Sizeable := False;
  BackgroundColor:= clBlack;
  FFont := fpgGetFont('#Label1');
  FMargin := 3;
  FBorder := 1;
  FShadow := 5;
  FTime := 5000;
  L_Hint := CreateLabel(Self, FBorder, FBorder, '', Width - FBorder * 2, Height - FBorder * 2, taCenter, tlCenter);
  L_Hint.BackgroundColor := clHintWindow;
  L_Hint.OnClick := @T_ChronoFini;
  T_Chrono := TfpgTimer.Create(FTime);
  T_Chrono.OnTimer := @T_ChronoFini;
  F_Shadow:= TF_Shadow.Create(nil);
  OnShow := @FormShow;
  OnHide := @FormHide;
end;

constructor TF_Shadow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'F_Shadow';
  WindowPosition := wpUser;
  WindowType := wtPopup;
//  BorderLess := True;
  Sizeable := False;
  BackgroundColor := clGray;
end;

end.

