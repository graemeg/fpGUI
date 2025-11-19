{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2025 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a basic Label control. Also known as a Caption component.
}

unit fpg_label;

{$I fpg_defines.inc}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget;

type


  TfpgCustomLabel = class(TfpgWidget)
  private
    FAutoSize: boolean;
    FAlignment: TAlignment;
    FLayout: TLayout;
    FWrapText: boolean;
    FLineSpace: integer;
    procedure   SetWrapText(const AValue: boolean);
    procedure   SetAlignment(const AValue: TAlignment);
    procedure   SetLayout(const AValue: TLayout);
    function    GetFontDesc: string;
    procedure   SetAutoSize(const AValue: boolean);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetText(const AValue: TfpgString);
    procedure   ResizeLabel;
  protected
    FText: TfpgString;
    FFont: TfpgFontResourceBase;
    FTextHeight: integer;
    procedure   DoCalculatePreferredSize(var ASize: TfpgSize); override;
    procedure   HandlePaint; override;
    property    WrapText: boolean read FWrapText write SetWrapText default False;
    property    Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property    AutoSize: boolean read FAutoSize write SetAutoSize default False;
    property    Layout: TLayout read FLayout write SetLayout default tlTop;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    Text: TfpgString read FText write SetText;
    property    LineSpace: integer read FLineSpace write FLineSpace default 2;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Font: TfpgFontResourceBase read FFont;
    property    TextHeight: integer read FTextHeight;
  end;


  TfpgLabel = class(TfpgCustomLabel)
  published
    property    AcceptDrops;
    property    Align;
    property    Alignment;
    property    AutoSize;
    property    BackgroundColor;
    property    Enabled;
    property    FontDesc;
    property    Height;
    property    Hint;
    property    Layout;
    property    Left;
    property    LineSpace;
    property    MaxHeight;
    property    MaxWidth;
    property    MinHeight default 2;
    property    MinWidth default 2;
    property    Parent;
    property    ParentShowHint;
    property    ShowHint;
    property    Text;
    property    TextColor;
    property    Top;
    property    Width;
    property    WrapText;
    property    OnClick;
    property    OnDragStartDetected;
    property    OnDoubleClick;
    property    OnMouseDown;
    property    OnMouseEnter;
    property    OnMouseExit;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnMultiClick;
    property    OnShowHint;
  end;


// A convenience function to create a TfpgLabel instance
function CreateLabel(AOwner: TComponent; x, y: TfpgCoord; AText: string; w: TfpgCoord= 0; h: TfpgCoord= 0;
          HAlign: TAlignment= taLeftJustify; VAlign: TLayout= tlTop; ALineSpace: integer= 2): TfpgLabel; overload;

implementation


function CreateLabel(AOwner: TComponent; x, y: TfpgCoord; AText: string; w: TfpgCoord; h: TfpgCoord;
          HAlign: TAlignment; VAlign: TLayout; ALineSpace: integer): TfpgLabel;
begin
  Result       := TfpgLabel.Create(AOwner);
  Result.Left  := x;
  Result.Top   := y;
  Result.Text  := AText;
  Result.LineSpace := ALineSpace;
  if h < Result.Font.GetHeight then
    Result.Height:= Result.Font.GetHeight
  else
    Result.Height:= h;
  Result.Alignment:= HAlign;
  Result.Layout:= VAlign;
  if w = 0 then
  begin
    Result.Width := Result.Font.GetTextWidth(Result.Text);
    Result.AutoSize := True;
  end
  else
    Result.Width := w;
end;

{ TfpgCustomLabel }

procedure TfpgCustomLabel.SetWrapText(const AValue: boolean);
begin
  if FWrapText <> AValue then
  begin
    FWrapText := AValue;
    ResizeLabel;
  end;
end;

procedure TfpgCustomLabel.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment <> AValue then
  begin
    FAlignment := AValue;
    ResizeLabel;
  end;
end;

procedure TfpgCustomLabel.SetLayout(const AValue: TLayout);
begin
  if FLayout <> AValue then
  begin
    FLayout := AValue;
    ResizeLabel;
  end;
end;

function TfpgCustomLabel.GetFontDesc: string;
begin
  if Assigned(FFont) then
    Result := FFont.FontDesc
  else
    Result := '';
end;

procedure TfpgCustomLabel.SetAutoSize(const AValue: boolean);
begin
  if FAutoSize <> AValue then
  begin
    FAutoSize := AValue;
    ResizeLabel;
  end;
end;

procedure TfpgCustomLabel.SetFontDesc(const AValue: string);
begin
  FFont := nil;  // Release old font (automatic ref count decrement)
  FFont := fpgApplication.FontManager.GetFont(AValue);
  ResizeLabel;
end;

procedure TfpgCustomLabel.SetText(const AValue: TfpgString);
begin
  if FText <> AValue then
  begin
    FText := AValue;
    ResizeLabel;
  end;
end;

procedure TfpgCustomLabel.ResizeLabel;
begin
  if FAutoSize and (not FWrapText) then
  begin
    Width := FFont.GetTextWidth(FText);
    Height:= FFont.GetHeight;
  end;
  UpdatePosition;
  RePaint;
end;

constructor TfpgCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText             := 'Label';
  FFont             := fpgApplication.FontManager.GetFont('#Label1');
  FHeight            := FFont.GetHeight;
  FWidth             := 80;
  FTextColor        := Parent.TextColor;
  FBackgroundColor  := Parent.BackgroundColor;
  FAutoSize         := False;
  FLayout           := tlTop;
  FAlignment        := taLeftJustify;
  FWrapText         := False;
  FLineSpace        := 2;
end;

destructor TfpgCustomLabel.Destroy;
begin
  FText := '';
  FFont := nil;  // Automatic ref count decrement and cleanup
  inherited Destroy;
end;

procedure TfpgCustomLabel.DoCalculatePreferredSize(var ASize: TfpgSize);
var
  CalculatedW, CalculatedH: integer;
begin
  // 1. First, determine the natural size based on content (text and font).
  if Assigned(FFont) then
  begin
    CalculatedW := FFont.GetTextWidth(FText);
    CalculatedH := FFont.GetHeight;
  end
  else
  begin
    // As a fallback, use the minimum size.
    CalculatedW := FMinWidth;
    CalculatedH := FMinHeight;
  end;

  // 2. Use the explicitly set PreferredSize.W, otherwise use the calculated width.
  if FPreferredSize.W > 0 then
    ASize.W := FPreferredSize.W
  else
    ASize.W := CalculatedW;

  // 3. Use the explicitly set PreferredSize.H, otherwise use the calculated height.
  if FPreferredSize.H > 0 then
    ASize.H := FPreferredSize.H
  else
    ASize.H := CalculatedH;
end;

procedure TfpgCustomLabel.HandlePaint;
var
  lTxtFlags: TfpgTextFlags;
begin
  inherited HandlePaint;
  Canvas.SetFont(Font);
  if Enabled then
    Canvas.SetTextColor(FTextColor)
  else
    Canvas.SetTextColor(clShadow1);

  lTxtFlags:= [];
  if not Enabled then
    Include(lTxtFlags, txtDisabled);

  if FWrapText then
    Include(lTxtFlags, txtWrap);
  case FAlignment of
    taLeftJustify:
      Include(lTxtFlags, txtLeft);
    taRightJustify:
      Include(lTxtFlags, txtRight);
    taCenter:
      Include(lTxtFlags, txtHCenter);
  end;
  case FLayout of
    tlTop:
      Include(lTxtFlags, txtTop);
    tlBottom:
      Include(lTxtFlags, txtBottom);
    tlCenter:
      Include(lTxtFlags, txtVCenter);
  end;
  FTextHeight := Canvas.DrawText(0, 0, ActualWidth, ActualHeight, FText, lTxtFlags);
end;

end.

