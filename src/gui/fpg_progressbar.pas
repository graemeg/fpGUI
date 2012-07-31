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
      Defines a component for a progress bar.
}

unit fpg_progressbar;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget;
  
type
  TfpgCustomProgressBar = class(TfpgWidget)
  private
    FMax: longint;
    FMin: longint;
    FPosition: longint;
    FShowCaption: boolean;
    FStep: longint;
    FFont: TfpgFont;
    procedure   SetMax(const AValue: longint);
    procedure   SetMin(const AValue: longint);
    procedure   SetPBPosition(const AValue: longint);
    procedure   SetShowCaption(const AValue: boolean);
    procedure   SetStep(const AValue: longint);
  protected
    procedure   HandlePaint; override;
    property    Max: longint read FMax write SetMax default 100;
    property    Min: longint read FMin write SetMin default 0;
    property    Position: longint read FPosition write SetPBPosition default 0;
    property    Step: longint read FStep write SetStep;
//    property    FontName: string read GetFontName write SetFontName;
    property    ShowCaption: boolean read FShowCaption write SetShowCaption default False;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   StepIt;
    procedure   StepBy(AStep: integer);
    property    Font: TfpgFont read FFont;
  end;
  
  
  TfpgProgressBar = class(TfpgCustomProgressBar)
  published
    property    Align;
    property    BackgroundColor default $c4c4c4;
    property    Enabled;
    property    Hint;
    property    ShowCaption;
    property    Max;
    property    Min;
    property    ParentShowHint;
    property    Position;
    property    ShowHint;
    property    Step;
    property    TextColor;
    property    OnShowHint;
  end;


implementation


{ TfpgCustomProgressBar }

procedure TfpgCustomProgressBar.SetMax(const AValue: longint);
begin
  if FMax = AValue then
    Exit; //==>

  // correct wrong inputs
  if FMin > AValue then
    FMin := AValue - 1;
  if FPosition > AValue then
    FPosition := AValue;

  FMax := AValue;
  RePaint;
end;

procedure TfpgCustomProgressBar.SetMin(const AValue: longint);
begin
  if FMin = AValue then
    Exit; //==>

  // correct wrong inputs
  if AValue > FPosition then
    FPosition := AValue;
  if AValue > FMax then
    FMax := AValue+1;

  FMin := AValue;
  RePaint;
end;

procedure TfpgCustomProgressBar.SetPBPosition(const AValue: longint);
begin
  if FPosition = AValue then
    Exit; //==>

  // correct limits
  if AValue < Min then
    FPosition := Min
  else if AValue > Max then
    FPosition := Max
  else
    FPosition := AValue;

  RePaint;
end;

procedure TfpgCustomProgressBar.SetShowCaption(const AValue: boolean);
begin
  if FShowCaption = AValue then
    Exit; //==>
  FShowCaption := AValue;
  RePaint;
end;

procedure TfpgCustomProgressBar.SetStep(const AValue: longint);
begin
  if AValue < 1 then
    Exit; //==>
  if FStep = AValue then
    Exit; //==>
  FStep := AValue;
end;

procedure TfpgCustomProgressBar.HandlePaint;
var
  r: TfpgRect;
  diff: integer;
  aPos: integer;  // absolute position
  pos: integer;
  percent: integer;
  txt: string;
  x: TfpgCoord;
  y: TfpgCoord;
begin
  inherited HandlePaint;
  Canvas.ClearClipRect;
  r.SetRect(0, 0, Width, Height);
  
  Canvas.Clear(BackgroundColor);
//  Canvas.SetColor(clInactiveWgFrame);

  // calculate position
  diff    := Max - Min; // diff..
  aPos    := Position - Min;  // absolute position
  percent := round(((100 / diff) * aPos));
  pos     := round(percent * (Width-2) / 100);

  // Bluecurve theme  :)
  // outer dark border
  Canvas.SetColor(TfpgColor($999999));
  Canvas.SetLineStyle(1, lsSolid);
  Canvas.DrawRectangle(r);
  InflateRect(r, -1, -1);
  r.Width := pos;
  if FPosition > 0 then
  begin
    // left top
    Canvas.SetColor(TfpgColor($98b2ed));
    Canvas.DrawLine(r.Left, r.Bottom, r.Left, r.Top);  // left
    Canvas.DrawLine(r.Left, r.Top, r.Right, r.Top);    // top
    // right bottom
    Canvas.SetColor(TfpgColor($3b4c71));
    Canvas.DrawLine(r.Right, r.Top, r.Right, r.Bottom);   // right
    Canvas.DrawLine(r.Right, r.Bottom, r.Left, r.Bottom);   // bottom
    // inside gradient fill
    InflateRect(r, -1, -1);
    Canvas.GradientFill(r, TfpgColor($425d9b), TfpgColor($97b0e8), gdVertical);
  end;
  // paint percentage if required
  if FShowCaption then
  begin
    txt := IntToStr(percent) + '%';
    x := (Width - FFont.TextWidth(txt)) div 2;
    y := (Height - FFont.Height) div 2;
    Canvas.SetTextColor(TextColor);
    Canvas.Font := FFont;
    Canvas.DrawString(x, y, txt);
  end;
end;

constructor TfpgCustomProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Focusable := False;
  Width := 150;
  Height := 22;
  FMin      := 0;
  FMax      := 100;
  FStep     := 1;
  FPosition := 0;
  FBackgroundColor := TfpgColor($c4c4c4); // clListBox;
  FTextColor := Parent.TextColor;
  FShowCaption := False;
  FFont     := fpgStyle.DefaultFont;
end;

procedure TfpgCustomProgressBar.StepIt;
begin
  Position := Position + Step;
end;

procedure TfpgCustomProgressBar.StepBy(AStep: integer);
begin
  Position := Position + AStep;
end;

end.

