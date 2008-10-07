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
      Defines a window that gets used to display help hints (aka a HintWindow)
}

unit fpg_hint;

{$mode objfpc}{$H+}

{.$Define Debug}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_label;
  
type
  TfpgHintWindow = class(TfpgForm)
  private
    FFont: TfpgFont;
    FTime: Integer;
    FShadow: Integer;
    FBorder: Integer;
    FMargin: Integer;
    L_Hint: TfpgLabel;
    T_Chrono: TfpgTimer;
    procedure   FormShow(Sender: TObject);
    procedure   FormHide(Sender: TObject);
    function    GetText: TfpgString;
    procedure   SetText(const AValue: TfpgString);
    procedure   T_ChronoFini(Sender: TObject);
    procedure   SetShadow(AValue: Integer);
    procedure   SetBorder(AValue: Integer);
    procedure   SetTime(AValue: Integer);
    procedure   SetLTextColor(AValue: Tfpgcolor);
    procedure   SetLBackgroundColor(AValue: Tfpgcolor);
    procedure   SetShadowColor(AValue: TfpgColor);
  protected
    procedure   HandleShow; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   SetPosition(aleft, atop, awidth, aheight: TfpgCoord); override;
    property    Font: TfpgFont read FFont;
    property    Text: TfpgString read GetText write SetText;
    property    Shadow: Integer read FShadow write SetShadow default 5;
    property    Border: Integer read FBorder write SetBorder default 1;
    property    Margin: Integer read FMargin write FMargin default 3;
    property    LTextColor: TfpgColor write SetLTextColor default clBlack;
    property    LBackgroundColor: TfpgColor write SetLBackgroundColor default clHintWindow;
    property    ShadowColor: TfpgColor write SetShadowColor default clGray;
    property    Time: Integer write SetTime default 5000;
  end;


  TfpgHintWindowClass = class of TfpgHintWindow;
  

var
  HintWindowClass: TfpgHintWindowClass = TfpgHintWindow;


implementation

type
  TfpgHintShadow = class(TfpgForm)
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
  
var
  uShadowForm: TfpgHintShadow;


{ TfpgHintWindow }

procedure TfpgHintWindow.FormShow(Sender: TObject);
begin
  T_Chrono.Enabled:= True;
end;

procedure TfpgHintWindow.FormHide(Sender: TObject);
begin
  T_Chrono.Enabled := False;
  if Assigned(uShadowForm) then
    uShadowForm.Hide;
end;

function TfpgHintWindow.GetText: TfpgString;
begin
  Result := L_Hint.Text;
end;

procedure TfpgHintWindow.SetText(const AValue: TfpgString);
begin
  L_Hint.Text := AValue;
end;

procedure TfpgHintWindow.T_ChronoFini(Sender: TObject);
begin
  {$IFDEF DEBUG}
  writeln('TF_Hint.T_ChronoFini timer fired');
  {$ENDIF}
  Hide;
end;

procedure TfpgHintWindow.SetShadow(AValue: Integer);
begin
  if FShadow <> AValue then
    FShadow := AValue;
end;

procedure TfpgHintWindow.SetBorder(AValue: Integer);
begin
  if FBorder <> AValue then
    FBorder := AValue;
end;

procedure TfpgHintWindow.SetTime(AValue: Integer);
begin
  if FTime <> AValue then
  begin
    FTime := AValue;
    T_Chrono.Interval := FTime;
  end;
end;

procedure TfpgHintWindow.SetLTextColor(AValue: Tfpgcolor);
begin
  if L_Hint.TextColor <> AValue then
    L_Hint.TextColor := AValue
end;

procedure TfpgHintWindow.SetLBackgroundColor(AValue: Tfpgcolor);
begin
  if L_Hint.BackgroundColor <> AValue then
    L_Hint.BackgroundColor := AValue
end;

procedure TfpgHintWindow.SetShadowColor(AValue: Tfpgcolor);
begin
  if uShadowForm.BackgroundColor <> AValue then
    uShadowForm.BackgroundColor := AValue;
end;

procedure TfpgHintWindow.HandleShow;
begin
  // This is so the Shadow Window is below the Hint Window.
  if Shadow > 0 then
  begin
    uShadowForm.SetPosition(Left+Shadow, Top+Shadow, Width, Height);
    uShadowForm.Show;
  end;
  inherited HandleShow;
end;

constructor TfpgHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'F_Hint';
  WindowPosition := wpUser;
  WindowType := wtPopup;
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
  uShadowForm:= TfpgHintShadow.Create(nil);
  OnShow := @FormShow;
  OnHide := @FormHide;
end;

destructor TfpgHintWindow.Destroy;
begin
  T_Chrono.Free;
  FFont.Free;
  inherited Destroy;
  uShadowForm.Destroy;
end;

procedure TfpgHintWindow.SetPosition(aleft, atop, awidth, aheight: TfpgCoord);
begin
  inherited SetPosition(aleft, atop, awidth, aheight);
  L_Hint.SetPosition(Border, Border, Width - (Border * 2), Height - (Border * 2));
end;

constructor TfpgHintShadow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'F_Shadow';
  WindowPosition := wpUser;
  WindowType := wtPopup;
  Sizeable := False;
  BackgroundColor := clGray;
end;


end.

