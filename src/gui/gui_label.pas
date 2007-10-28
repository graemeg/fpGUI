{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a basic Label control. Also known as a Caption component.
}

unit gui_label;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_widget;

type

  TfpgCustomLabel = class(TfpgWidget)
  private
    FAutoSize: boolean;
    FBackgroundColor: TfpgColor;
    FColor: TfpgColor;
    function    GetFontDesc: string;
    procedure   SetAutoSize(const AValue: boolean);
    procedure   SetBackgroundColor(const AValue: TfpgColor);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetColor(const AValue: TfpgColor);
    procedure   SetText(const AValue: string);
    procedure   ResizeLabel;
  protected
    FText: string;
    FFont: TfpgFont;
    procedure   HandlePaint; override;
    property    AutoSize: boolean read FAutoSize write SetAutoSize default False;
    property    Text: string read FText write SetText;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    Color: TfpgColor read FColor write SetColor;
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Font: TfpgFont read FFont;
  end;
  
  
  TfpgLabel = class(TfpgCustomLabel)
  published
    property    AutoSize;
    property    BackgroundColor;
    property    Color;
    property    FontDesc;
    property    Text;
    property    OnMouseEnter;
    property    OnMouseExit;
    property    OnMouseDown;
    property    OnMouseUp;
    property    OnMouseMove;
  end;


// A convenience function to create a TfpgLabel instance
function CreateLabel(AOwner: TComponent; x, y: TfpgCoord; AText: string): TfpgLabel;


implementation


function CreateLabel(AOwner: TComponent; x, y: TfpgCoord; AText: string): TfpgLabel;
begin
  Result       := TfpgLabel.Create(AOwner);
  Result.Left  := x;
  Result.Top   := y;
  Result.Text  := AText;
  Result.Width := Result.Font.TextWidth(Result.Text);
end;


{ TfpgCustomLabel }

function TfpgCustomLabel.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TfpgCustomLabel.SetAutoSize(const AValue: boolean);
begin
  if FAutoSize = AValue then
    Exit; //==>
  FAutoSize := AValue;
  if FAutoSize then
  begin
    ResizeLabel;
    RePaint;
  end;
end;

procedure TfpgCustomLabel.SetBackgroundColor(const AValue: TfpgColor);
begin
  if FBackgroundColor = AValue then
    Exit; //==>
  FBackgroundColor := AValue;
  RePaint;
end;

procedure TfpgCustomLabel.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  if FAutoSize then
    ResizeLabel;
  RePaint;
end;

procedure TfpgCustomLabel.SetColor(const AValue: TfpgColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
  RePaint;
end;

procedure TfpgCustomLabel.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit; //==>
  FText := AValue;
  if FAutoSize then
    ResizeLabel;
  RePaint;
end;

procedure TfpgCustomLabel.ResizeLabel;
begin
  Width   := FFont.TextWidth(FText);
  Height  := FFont.Height;
  SetPosition(Left, Top, Width, Height);
end;

constructor TfpgCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText   := 'Label';
  FFont   := fpgGetFont('#Label1');
  FHeight := FFont.Height;
  FWidth  := 80;
  FColor  := clText1;
  FBackgroundColor := clWindowBackground;
  FAutoSize := False;
end;

destructor TfpgCustomLabel.Destroy;
begin
  FText := '';
  FFont.Free;
  inherited Destroy;
end;

procedure TfpgCustomLabel.HandlePaint;
begin
  Canvas.BeginDraw;
  inherited;
  Canvas.Clear(FBackgroundColor);
  Canvas.SetFont(Font);
  Canvas.SetTextColor(FColor);
  fpgStyle.DrawString(Canvas, 0, 0, FText, Enabled);
  Canvas.EndDraw;
end;

end.

