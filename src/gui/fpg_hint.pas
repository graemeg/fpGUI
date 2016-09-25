{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2016 by Graeme Geldenhuys.

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
  fpg_form;
  
type

  TfpgHintWindow = class(TfpgForm)
  private
    FFont: TfpgFont;
    FTime: Integer;
    FShadow: Integer;
    FBorder: Integer;
    FMargin: Integer;
    FTimer: TfpgTimer;
    FHintTextRec: TfpgRect;
    FText: TfpgString;
    procedure   FormShow(Sender: TObject);
    procedure   FormHide(Sender: TObject);
    function    GetText: TfpgString;
    procedure   SetText(const AValue: TfpgString);
    procedure   HintTimerFired(Sender: TObject);
    procedure   SetShadow(AValue: Integer);
    procedure   SetBorder(AValue: Integer);
    procedure   SetTime(AValue: Integer);
    procedure   SetShadowColor(AValue: TfpgColor);
    function    GetFontDesc: string;
    procedure   SetFontDesc(const AValue: string);
  protected
    procedure   HandleShow; override;
    procedure   HandlePaint; override;
    procedure   PaintBorder; virtual;
    procedure   PaintHintText; virtual;
    procedure   DoAllocateWindowHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Font: TfpgFont read FFont;
    property    Text: TfpgString read GetText write SetText;
    property    Shadow: Integer read FShadow write SetShadow default 0;
    property    Border: Integer read FBorder write SetBorder default 1;
    property    Margin: Integer read FMargin write FMargin default 3;
    property    ShadowColor: TfpgColor write SetShadowColor default clGray;
    property    Time: Integer read FTime write SetTime;
  published
    property    BackgroundColor;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    TextColor;
    //property    OnActivate;
    property    OnClose;
    //property    OnCloseQuery;
    property    OnCreate;
    //property    OnDeactivate;
    property    OnDestroy;
    property    OnHide;
    property    OnPaint;
    property    OnResize;
    property    OnShow;
  end;


  TfpgHintWindowClass = class of TfpgHintWindow;
  

var
  HintWindowClass: TfpgHintWindowClass = TfpgHintWindow;


implementation


type

  { TfpgHintShadow }

  TfpgHintShadow = class(TfpgForm)
  protected
    procedure   DoAllocateWindowHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
  
var
  uShadowForm: TfpgHintShadow;


{ TfpgHintWindow }

procedure TfpgHintWindow.FormShow(Sender: TObject);
begin
  FTimer.Enabled := True;
end;

procedure TfpgHintWindow.FormHide(Sender: TObject);
begin
  if Assigned(uShadowForm) then
    uShadowForm.Hide;
end;

function TfpgHintWindow.GetText: TfpgString;
begin
  Result := FText;
end;

procedure TfpgHintWindow.SetText(const AValue: TfpgString);
begin
  FText := AValue;
end;

procedure TfpgHintWindow.HintTimerFired(Sender: TObject);
begin
  {$IFDEF DEBUG}
  DebugLn('DEBUG:  TfpgHintWindow.HintTimerFired timer fired');
  {$ENDIF}
  FTimer.Enabled := False;
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
    FTimer.Interval := FTime;
  end;
end;

procedure TfpgHintWindow.SetShadowColor(AValue: TfpgColor);
begin
  if uShadowForm.BackgroundColor <> AValue then
    uShadowForm.BackgroundColor := AValue;
end;

function TfpgHintWindow.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TfpgHintWindow.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
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

procedure TfpgHintWindow.HandlePaint;
var
  r: TfpgRect;
begin
  inherited HandlePaint;  // background is set
  Canvas.Font := FFont;
  // Do we need to resize?
  PaintBorder;
  if FBorder > 0 then
  begin
    r.SetRect(FBorder, FBorder, Width-(FBorder*2), Height-(FBorder*2));
    Canvas.SetClipRect(r);
  end;
  PaintHintText;
end;

procedure TfpgHintWindow.PaintBorder;
var
  i: integer;
begin
  if FBorder = 0 then  // no border
    Exit;
  Canvas.Color := clBlack;
  for i := 0 to FBorder-1 do
  begin
    Canvas.DrawRectangle(i, i, Width-(i*2), Height-(i*2));
  end;
end;

procedure TfpgHintWindow.PaintHintText;
begin
  FHintTextRec.SetRect(FBorder, FBorder, Width-(FBorder*2), Height-(FBorder*2));
  Canvas.TextColor := FTextColor;
  Canvas.DrawText(FHintTextRec, Text, [txtHCenter, txtVCenter, txtWrap]);
end;

procedure TfpgHintWindow.DoAllocateWindowHandle;
begin
  inherited DoAllocateWindowHandle;
  Window.WindowType := wtPopup;
end;

constructor TfpgHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'F_Hint';
  WindowPosition := wpUser;
  Sizeable := False;
  BackgroundColor := clHintWindow;
  FFont := fpgGetFont('#Label1');
  FMargin := 2;
  FBorder := 1;
  FShadow := 0; // no shadow by default
  FTime := 5000; // show hint for 5 seconds then close
  FHintTextRec.SetRect(FBorder, FBorder, Width-(FBorder*2), Height-(FBorder*2));
  FTimer := TfpgTimer.Create(FTime);
  FTimer.OnTimer := @HintTimerFired;
  uShadowForm:= TfpgHintShadow.Create(nil);
  OnClick := @HintTimerFired;
  OnShow := @FormShow;
  OnHide := @FormHide;
end;

destructor TfpgHintWindow.Destroy;
begin
  FTimer.Free;
  FFont.Free;
  inherited Destroy;
  uShadowForm.Free;
end;


{ TfpgHintShadow }

procedure TfpgHintShadow.DoAllocateWindowHandle;
begin
  inherited DoAllocateWindowHandle;
  Window.WindowType := wtPopup;
end;

constructor TfpgHintShadow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'F_Shadow';
  WindowPosition := wpUser;
  Sizeable := False;
  BackgroundColor := clGray;
end;


end.

