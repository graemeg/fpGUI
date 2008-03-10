{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2008 See the file AUTHORS.txt, included in this
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
  gfx_UTF8utils,
  gfx_widget;

type
  TLayout = (tlTop, tlCenter, tlBottom);

  TfpgCustomLabel = class(TfpgWidget)
  private
    FAutoSize: boolean;
    FAlignment: TAlignment;
    FLayout: TLayout;
    FWrapText: boolean;
    FWrappedText: TStringList;
    procedure   Wrap(MaxLength: integer; AText: string);
    procedure   SetWrapText(const AValue: boolean);
    procedure   SetAlignment(const AValue: TAlignment);
    procedure   SetLayout(const AValue: TLayout);
    function    GetFontDesc: string;
    procedure   SetAutoSize(const AValue: boolean);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetText(const AValue: string);
    procedure   ResizeLabel;
  protected
    FText: string;
    FFont: TfpgFont;
    procedure   HandlePaint; override;
    property    WrapText: boolean read FWrapText write SetWrapText default False;
    property    Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property    AutoSize: boolean read FAutoSize write SetAutoSize default False;
    property    Layout: TLayout read FLayout write SetLayout default tlTop;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    Text: string read FText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Font: TfpgFont read FFont;
  end;
  
  
  TfpgLabel = class(TfpgCustomLabel)
  published
    property    Alignment;
    property    AutoSize;
    property    BackgroundColor;
    property    FontDesc;
    property    Layout;
    property    Text;
    property    TextColor;
    property    Width;
    property    WrapText;
    property    OnClick;
    property    OnDoubleClick;
    property    OnMouseDown;
    property    OnMouseEnter;
    property    OnMouseExit;
    property    OnMouseMove;
    property    OnMouseUp;
  end;


// A convenience function to create a TfpgLabel instance
function CreateLabel(AOwner: TComponent; x, y: TfpgCoord; AText: string; w: TfpgCoord= 0; h: TfpgCoord= 0;
          HAlign: TAlignment= taLeftJustify; VAlign: TLayout= tlTop): TfpgLabel; overload;

implementation


function CreateLabel(AOwner: TComponent; x, y: TfpgCoord; AText: string; w: TfpgCoord= 0; h: TfpgCoord= 0;
          HAlign: TAlignment= taLeftJustify; VAlign: TLayout= tlTop): TfpgLabel; overload;
begin
  Result       := TfpgLabel.Create(AOwner);
  Result.Left  := x;
  Result.Top   := y;
  Result.Text  := AText;
  if w = 0 then
  begin
    Result.Width := Result.Font.TextWidth(Result.Text) + 5;  // 5 is some extra spacing
    Result.FAutoSize := True;
  end
  else
  begin
    Result.Width := w;
    Result.WrapText := True;
  end;
  if h < Result.Font.Height then
    Result.Height:= Result.Font.Height
  else
    Result.Height:= h;
  Result.Alignment:= HAlign;
  Result.Layout:= VAlign;
end;

{ TfpgCustomLabel }

procedure TfpgCustomLabel.Wrap(MaxLength: integer; AText: string);
begin
  FWrappedText.Clear;
  repeat
  if UTF8Pos(' ', AText) > 0 then
  begin
    if Font.TextWidth(UTF8Copy(AText, 1, UTF8Pos(' ', AText))) < MaxLength then
    begin
      if FWrappedText.Count > 0 then
        if (Font.TextWidth(FWrappedText[Pred(FWrappedText.Count)] + ' ' +
            UTF8Copy(AText, 1, UTF8Pos(' ', AText)))) < MaxLength then
          FWrappedText[Pred(FWrappedText.Count)] := FWrappedText[Pred(FWrappedText.Count)] + ' '
              + UTF8Copy(AText, 1, Pred(UTF8Pos(' ', AText)))
        else
          FWrappedText.Add(UTF8Copy(AText, 1, Pred(UTF8Pos(' ', AText))))
      else
        FWrappedText.Add(UTF8Copy(AText, 1, Pred(UTF8Pos(' ', AText))));
      AText := UTF8Copy(AText, Succ(UTF8Pos(' ', AText)), UTF8Length(AText) - Pred(UTF8Pos(' ', AText)));
    end;
  end
  else
  begin
    FWrappedText.Add(AText);
    AText:= '';
  end;
  until UTF8Pos(' ', AText) = 0;
  if FWrappedText.Count > 0 then
  begin
    if (Font.TextWidth(FWrappedText[Pred(FWrappedText.Count)] + ' ' + AText)) < MaxLength then
      FWrappedText[Pred(FWrappedText.Count)] := FWrappedText[Pred(FWrappedText.Count)] + ' ' + AText
    else
      FWrappedText.Add(AText);
  end;
  if Height < FWrappedText.Count * (Font.Height + 2) then
    Height := FWrappedText.Count * (Font.Height + 2);
end;

procedure TfpgCustomLabel.SetWrapText(const AValue: boolean);
begin
  FWrapText := AValue;
  if FWrapText then
    Wrap(Width, FText);
  RePaint;
end;

procedure TfpgCustomLabel.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment = AValue then
    Exit;
  FAlignment := AValue;
  if FAlignment <> taLeftJustify then
    FAutoSize := False;
  ResizeLabel;
end;

procedure TfpgCustomLabel.SetLayout(const AValue: TLayout);
begin
  if FLayout = AValue then
    Exit;
  FLayout := AValue;
  ResizeLabel;
end;

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
    FAlignment := taLeftJustify;
  ResizeLabel;
end;

procedure TfpgCustomLabel.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  ResizeLabel;
end;

procedure TfpgCustomLabel.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit; //==>
  FText := AValue;
  ResizeLabel;
end;

procedure TfpgCustomLabel.ResizeLabel;
begin
  if FAutoSize then
  begin
    Width   := FFont.TextWidth(FText);
  end
  else if FWrapText then
    Wrap(Width, FText);

  UpdateWindowPosition;
  RePaint;
end;

constructor TfpgCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText       := 'Label';
  FFont       := fpgGetFont('#Label1');
  FHeight     := FFont.Height;
  FWidth      := 80;
  FTextColor  := Parent.TextColor;
  FBackgroundColor := Parent.BackgroundColor;
  FAutoSize   := False;

  FLayout      := tlTop;
  FAlignment   := taLeftJustify;
  FWrapText    := False;
  FWrappedText := TStringList.Create;
end;

destructor TfpgCustomLabel.Destroy;
begin
  FText := '';
  FFont.Free;
  FWrappedText.Free;
  inherited Destroy;
end;

procedure TfpgCustomLabel.HandlePaint;
var
  i: integer;
  r: TfpgRect;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;
  Canvas.ClearClipRect;
  r.SetRect(0, 0, Width, Height);
  Canvas.Clear(FBackgroundColor);
  Canvas.SetFont(Font);
  Canvas.SetTextColor(FTextColor);
  if WrapText then
  begin
    if FWrappedText.Count> 0 then
      for i:= 0 to Pred(FWrappedText.Count) do
        case FAlignment of
          taLeftJustify:
            case FLayout of
              tlTop:
                fpgStyle.DrawString(Canvas, 0, (Font.Height + 2) * i, FWrappedText[i], Enabled);
              tlBottom:
                fpgStyle.DrawString(Canvas, 0,
                    Height - ((Font.Height + 2) * FWrappedText.Count)+ ((Font.Height + 2) * i),
                    FWrappedText[i], Enabled);
              tlCenter:
                fpgStyle.DrawString(Canvas, 0,
                    ((Height - (Font.Height + 2) * FWrappedText.Count) div 2) + ((Font.Height + 2) * i),
                    FWrappedText[i], Enabled);
              end;

          taRightJustify:
            case FLayout of
              tlTop:
                fpgStyle.DrawString(Canvas, Width - Font.TextWidth(FWrappedText[i]),
                    (Font.Height + 2) * i, FWrappedText[i], Enabled);
              tlBottom:
                fpgStyle.DrawString(Canvas, Width - Font.TextWidth(FWrappedText[i]),
                    Height - ((Font.Height + 2) * FWrappedText.Count)+ ((Font.Height + 2) * i),
                    FWrappedText[i], Enabled);
              tlCenter:
                fpgStyle.DrawString(Canvas, Width - Font.TextWidth(FWrappedText[i]),
                    ((Height - (Font.Height + 2) * FWrappedText.Count) div 2) + ((Font.Height + 2) * i),
                    FWrappedText[i], Enabled);
              end;

          taCenter:
            case FLayout of
              tlTop:
                fpgStyle.DrawString(Canvas, (Width - Font.TextWidth(FWrappedText[i])) div 2,
                    (Font.Height + 2) * i, FWrappedText[i], Enabled);
              tlBottom:
                fpgStyle.DrawString(Canvas, (Width - Font.TextWidth(FWrappedText[i])) div 2,
                    Height - ((Font.Height + 2) * FWrappedText.Count)+ ((Font.Height + 2) * i),
                    FWrappedText[i], Enabled);
              tlCenter:
                fpgStyle.DrawString(Canvas, (Width - Font.TextWidth(FWrappedText[i])) div 2,
                    ((Height - (Font.Height + 2) * FWrappedText.Count) div 2) + ((Font.Height + 2) * i),
                    FWrappedText[i], Enabled);
              end;
        end;
  end
  else
    case FAlignment of
      taLeftJustify:
        case FLayout of
          tlTop:
            fpgStyle.DrawString(Canvas, 0, 0, FText, Enabled);
          tlBottom:
            fpgStyle.DrawString(Canvas, 0, Height - Font.Height, FText, Enabled);
          tlCenter:
            fpgStyle.DrawString(Canvas, 0, (Height - Font.Height) div 2, FText, Enabled);
          end;

      taRightJustify:
        case FLayout of
          tlTop:
            fpgStyle.DrawString(Canvas, Width - Font.TextWidth(FText), 0, FText, Enabled);
          tlBottom:
            fpgStyle.DrawString(Canvas, Width - Font.TextWidth(FText), Height - Font.Height, FText, Enabled);
          tlCenter:
            fpgStyle.DrawString(Canvas, Width - Font.TextWidth(FText), (Height - Font.Height) div 2, FText, Enabled);
          end;

      taCenter:
        case FLayout of
          tlTop:
            fpgStyle.DrawString(Canvas, (Width - Font.TextWidth(FText)) div 2, 0, FText, Enabled);
          tlBottom:
            fpgStyle.DrawString(Canvas, (Width - Font.TextWidth(FText)) div 2, Height - Font.Height, FText, Enabled);
          tlCenter:
            fpgStyle.DrawString(Canvas, (Width - Font.TextWidth(FText)) div 2, (Height - Font.Height) div 2, FText, Enabled);
          end;
    end;
  Canvas.EndDraw;
end;

end.

