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
      Defines a basic Label control. Also known as a Caption component.
}

unit fpg_label;

{$mode objfpc}{$H+}

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
    FFont: TfpgFont;
    FTextHeight: integer;
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
    property    Font: TfpgFont read FFont;
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
    property    MinHeight;
    property    MinWidth;
    property    Parent;
    property    ParentShowHint;
    property    ShowHint;
    property    Text;
    property    TextColor;
    property    Top;
    property    Width;
    property    WrapText;
    property    OnClick;
    property    OnDragEnter;
    property    OnDragLeave;
    property    OnDragDrop;
    property    OnDragStartDetected;
    property    OnDoubleClick;
    property    OnMouseDown;
    property    OnMouseEnter;
    property    OnMouseExit;
    property    OnMouseMove;
    property    OnMouseUp;
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
  if h < Result.Font.Height then
    Result.Height:= Result.Font.Height
  else
    Result.Height:= h;
  Result.Alignment:= HAlign;
  Result.Layout:= VAlign;
  if w = 0 then
  begin
    Result.Width := Result.Font.TextWidth(Result.Text);
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
  Result := FFont.FontDesc;
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
  FFont.Free;
  FFont := fpgGetFont(AValue);
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
    Width := FFont.TextWidth(FText);
    Height:= FFont.Height;
  end;
  UpdateWindowPosition;
  RePaint;
end;

constructor TfpgCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText             := 'Label';
  FFont             := fpgGetFont('#Label1');
  Height            := FFont.Height;
  Width             := 80;
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
  FFont.Free;
  inherited Destroy;
end;

procedure TfpgCustomLabel.HandlePaint;
var
  r: TfpgRect;
  lTxtFlags: TfpgTextFlags;
begin
  inherited HandlePaint;
  Canvas.ClearClipRect;
  r.SetRect(0, 0, Width, Height);
  Canvas.Clear(FBackgroundColor);
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
  FTextHeight := Canvas.DrawText(0, 0, Width, Height, FText, lTxtFlags);
end;

end.

