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
      Defines a Panel control. Also known as a Bevel or Frame control.
      This control can also draw itself like a GroupBox component.
}

unit gui_panel;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpgfx,
  gfxbase,
  gfx_widget,
  gui_Label;

type

  TPanelShape = (bsBox, bsFrame, bsTopLine, bsBottomLine, bsLeftLine,
    bsRightLine, bsSpacer);

  TPanelStyle = (bsLowered, bsRaised);

  TPanelType = (ptBevel, ptPanel, ptGroupBox);


  TfpgPanel = class(TfpgWidget)
  private
    FAlignment: TAlignment;
    FLabel: TfpgLabel;
    FLayout: TLayout;
    FPanelShape: TPanelShape;
    FPanelStyle: TPanelStyle;
    FPanelType: TPanelType;
    procedure   AlignText;
    procedure   SetPanelShape(const AValue: TPanelShape);
    procedure   SetPanelStyle(const AValue: TPanelStyle);
    function    GetAlignment: TAlignment;
    procedure   SetAlignment(const AValue: TAlignment);
    function    GetLayout: TLayout;
    procedure   SetLayout(const AValue: TLayout);
    function    GetText: string;
    procedure   SetText(const AValue: string);
    function    GetTextColor: Tfpgcolor;
    procedure   SetTextColor(const AValue: Tfpgcolor);
    function    GetBackgroundColor: Tfpgcolor;
    procedure   SetBackgroundColor(const AValue: Tfpgcolor);
    function    GetFontDesc: string;
    procedure   SetFontDesc(const AValue: string);
    function    GetTextLength: integer;
    procedure   SetTextLength(const AValue: integer);
    function    GetTextAlignment: TAlignment;
    procedure   SetTextAlignment(const AValue: TAlignment);
    function    GetTextLineSpace: integer;
    procedure   SetTextLineSpace(const AValue: integer);
    function    GetWrapText: boolean;
    procedure   SetWrapText(const AValue: boolean);
  protected
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    Alignment: TAlignment read GetAlignment write SetAlignment default taCenter;
    property    BackgroundColor: TfpgColor read GetBackgroundColor write SetBackgroundColor;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    Layout: TLayout read GetLayout write SetLayout default tlCenter;
    property    Shape: TPanelShape read FPanelShape write SetPanelShape default bsBox;
    property    Style: TPanelStyle read FPanelStyle write SetPanelStyle default bsRaised;
    property    Text: string read GetText write SetText;
    property    TextAlignment: TAlignment read GetTextAlignment write SetTextAlignment default taCenter;
    property    TextColor: Tfpgcolor read GetTextColor write SetTextColor;
    property    TextLength: integer read GetTextLength write SetTextLength;
    property    TextLineSpace: integer read GetTextLineSpace write SetTextLineSpace default 2;
    property    WrapText: boolean read GetWrapText write SetWrapText default False;
    property    OnClick;
    property    OnDoubleClick;
  end;


function CreateBevel(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TfpgCoord;
  AShape: TPanelShape; AStyle: TPanelStyle): TfpgPanel;

function CreatePanel(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TfpgCoord; AText: string;
  AStyle: TPanelStyle): TfpgPanel;

function CreateGroupBox(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TfpgCoord;
  AText: string; AStyle: TPanelStyle): TfpgPanel;


implementation


function CreateBevel(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TfpgCoord;
  AShape: TPanelShape; AStyle: TPanelStyle): TfpgPanel;
begin
  Result        := TfpgPanel.Create(AOwner);
  Result.FPanelType := ptBevel;
  Result.Left   := ALeft;
  Result.Top    := ATop;
  Result.Width  := AWidth;
  Result.Height := AHeight;
  Result.Shape  := AShape;
  Result.Style  := AStyle;
end;

function CreatePanel(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TfpgCoord; AText: string;
  AStyle: TPanelStyle): TfpgPanel;

  function DoCreate: TfpgPanel;
  begin
    Result        := TfpgPanel.Create(AOwner);
    Result.FPanelType := ptPanel;
    Result.Left   := ALeft;
    Result.Top    := ATop;
    Result.Width  := AWidth;
    Result.Height := AHeight;
    Result.Shape  := bsBox;
    Result.Style  := AStyle;
    Result.FLabel := TfpgLabel.Create(Result);
    Result.FLabel.Text := AText;
  end;

begin
  Result := DoCreate;
  Result.FLabel.SetPosition((Result.Width - Result.FLabel.Font.TextWidth(Result.FLabel.Text)) div 2,
    (Result.Height - Result.FLabel.Font.Height) div 2,
    Result.FLabel.Font.TextWidth(Result.FLabel.Text),
    Result.FLabel.Font.Height);
end;

function CreateGroupBox(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TfpgCoord;
  AText: string; AStyle: TPanelStyle): TfpgPanel;

  function DoCreate: TfpgPanel;
  begin
    Result        := TfpgPanel.Create(AOwner);
    Result.FPanelType := ptGroupBox;
    Result.Left   := ALeft;
    Result.Top    := ATop;
    Result.Width  := AWidth;
    Result.Height := AHeight;
    Result.Shape  := bsBox;
    Result.Style  := AStyle;
    Result.FLabel := TfpgLabel.Create(Result);
    Result.FLabel.Text := AText;
  end;

begin
  Result := DoCreate;
  Result.FLabel.Width := Result.FLabel.Font.TextWidth(Result.FLabel.Text) + 4;
  Result.FLabel.Height := Result.FLabel.Font.Height + 2;
  Result.FLabel.Alignment := taCenter;
  Result.FLabel.Layout := tlBottom;
  Result.FLabel.SetPosition(5, -3, Result.FLabel.Width, Result.FLabel.Height);
end;

{ TfpgPanel }

procedure TfpgPanel.AlignText;
begin
  case FPanelType of
    ptGroupBox:
    begin
      FLabel.Width  := FLabel.Font.TextWidth(FLabel.Text) + 4;
      FLabel.Height := FLabel.TextHeight + 2;
    end;
    ptPanel:
      if WrapText then
      begin
        FLabel.Width  := TextLength;
        FLabel.Height := FLabel.TextHeight;
      end;
    else
    begin
      FLabel.Width  := FLabel.Font.TextWidth(FLabel.Text);
      FLabel.Height := FLabel.TextHeight;
    end;
  end;
  case FAlignment of
    taLeftJustify:
      case FLayout of
        tlTop:
          if FPanelType = ptGroupBox then
            FLabel.SetPosition(5, -3, FLabel.Width, FLabel.Height)
          else
            FLabel.SetPosition(1, 1, FLabel.Width, FLabel.Height);
        tlBottom:
          FLabel.SetPosition(1, FHeight - FLabel.Height - 1, FLabel.Width, FLabel.Height);
        tlCenter:
          FLabel.SetPosition(1, (FHeight - FLabel.Height) div 2, FLabel.Width, FLabel.Height);
      end;
    taRightJustify:
      case FLayout of
        tlTop:
          if FPanelType = ptGroupBox then
            FLabel.SetPosition(FWidth - FLabel.Width - 5, -3, FLabel.Width, FLabel.Height)
          else
            FLabel.SetPosition(FWidth - FLabel.Width - 1, 1, FLabel.Width, FLabel.Height);
        tlBottom:
          FLabel.SetPosition(FWidth - FLabel.Width - 1, FHeight - FLabel.Height - 1, FLabel.Width, FLabel.Height);
        tlCenter:
          FLabel.SetPosition(FWidth - FLabel.Width - 1, (FHeight - FLabel.Height) div 2, FLabel.Width, FLabel.Height);
      end;
    taCenter:
      case FLayout of
        tlTop:
          if FPanelType = ptGroupBox then
            FLabel.SetPosition((FWidth - FLabel.Width) div 2, -3, FLabel.Width, FLabel.Height)
          else
            FLabel.SetPosition((FWidth - FLabel.Width) div 2, 1, FLabel.Width, FLabel.Height);
        tlBottom:
          FLabel.SetPosition((FWidth - FLabel.Width) div 2, FHeight - FLabel.Height - 1, FLabel.Width, FLabel.Height);
        tlCenter:
          FLabel.SetPosition((FWidth - FLabel.Width) div 2, (FHeight - FLabel.Height) div 2, FLabel.Width,
            FLabel.Height);
      end;
  end;
end;

procedure TfpgPanel.SetPanelShape(const AValue: TPanelShape);
begin
  if FPanelShape = AValue then
    Exit; //==>
  FPanelShape := AValue;
  Repaint;
end;

procedure TfpgPanel.SetPanelStyle(const AValue: TPanelStyle);
begin
  if FPanelStyle = AValue then
    Exit; //==>
  FPanelStyle := AValue;
  Repaint;
end;

function TfpgPanel.GetAlignment: TAlignment;
begin
  Result := FAlignment;
end;

procedure TfpgPanel.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment <> AValue then
  begin
    FAlignment := AValue;
    AlignText;
  end;
end;

function TfpgPanel.GetLayout: TLayout;
begin
  Result := FLayout;
end;

procedure TfpgPanel.SetLayout(const AValue: TLayout);
begin
  if FLayout <> AValue then
  begin
    FLayout := AValue;
    AlignText;
  end;
end;

function TfpgPanel.GetText: string;
begin
  Result := FLabel.Text;
end;

procedure TfpgPanel.SetText(const AValue: string);
begin
  if FLabel.Text <> AValue then
  begin
    FLabel.Text := AValue;
    AlignText;
  end;
end;

function TfpgPanel.GetTextColor: Tfpgcolor;
begin
  Result := FLabel.TextColor;
end;

procedure TfpgPanel.SetTextColor(const AValue: Tfpgcolor);
begin
  if FLabel.TextColor <> AValue then
    FLabel.TextColor := AValue;
end;

function TfpgPanel.GetBackgroundColor: Tfpgcolor;
begin
  Result := FBackgroundColor;
end;

procedure TfpgPanel.SetBackgroundColor(const AValue: Tfpgcolor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    if Assigned(FLabel) then
      FLabel.BackgroundColor := AValue;
    Repaint;
  end;
end;

function TfpgPanel.GetFontDesc: string;
begin
  Result := FLabel.FontDesc;
end;

procedure TfpgPanel.SetFontDesc(const AValue: string);
begin
  if FLabel.FontDesc <> AValue then
  begin
    FLabel.FontDesc := AValue;
    AlignText;
  end;
end;

function TfpgPanel.GetTextLength: integer;
begin
  Result := FLabel.Width;
end;

procedure TfpgPanel.SetTextLength(const AValue: integer);
begin
  if FLabel.Width <> AValue then
  begin
    FLabel.Width := AValue;
    AlignText;
  end;
end;

function TfpgPanel.GetTextAlignment: TAlignment;
begin
  Result := FLabel.Alignment;
end;

procedure TfpgPanel.SetTextAlignment(const AValue: TAlignment);
begin
  if FLabel.Alignment <> AValue then
  begin
    FLabel.Alignment := AValue;
    AlignText;
  end;
end;

function TfpgPanel.GetTextLineSpace: integer;
begin
  Result := FLabel.LineSpace;
end;

procedure TfpgPanel.SetTextLineSpace(const AValue: integer);
begin
  if FLabel.LineSpace <> AValue then
  begin
    FLabel.LineSpace := AValue;
    AlignText;
  end;
end;

function Tfpgpanel.GetWrapText: boolean;
begin
  Result := FLabel.WrapText;
end;

procedure Tfpgpanel.SetWrapText(const AValue: boolean);
begin
  if FLabel.WrapText <> AValue then
  begin
    FLabel.WrapText := AValue;
    AlignText;
  end;
end;

procedure TfpgPanel.HandlePaint;
var
  r: TfpgRect;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;

  if FPanelType = ptGroupBox then
  begin
    Canvas.Clear(Parent.BackgroundColor);
    Canvas.ClearClipRect;
    r.SetRect(0, 5, Width, Height - 5);
    Canvas.SetClipRect(r);
    Canvas.Clear(FBackgroundColor);
    Canvas.ClearClipRect;
  end
  else
    Canvas.Clear(BackgroundColor);

  //  Canvas.SetLineStyle(2, lsSolid);
  //  Canvas.SetColor(clWindowBackground);
  //  Canvas.DrawRectangle(1, 1, Width - 1, Height - 1);
  Canvas.SetLineStyle(1, lsSolid);

  if Style = bsRaised then
    Canvas.SetColor(clHilite2)
  else
    Canvas.SetColor(clShadow2);

  if FPanelType = ptGroupBox then
  begin
    if Shape in [bsBox, bsFrame, bsTopLine] then
      Canvas.DrawLine(0, 5, Width - 1, 5);
    if Shape in [bsBox, bsFrame, bsLeftLine] then
      Canvas.DrawLine(0, 6, 0, Height);
    if Shape in [bsFrame, bsRightLine] then
      Canvas.DrawLine(Width - 2, 6, Width - 2, Height);
    if Shape in [bsFrame, bsBottomLine] then
      Canvas.DrawLine(1, Height - 2, Width - 1, Height - 2);
  end
  else
  begin
    if Shape in [bsBox, bsFrame, bsTopLine] then
      Canvas.DrawLine(0, 0, Width - 1, 0);
    if Shape in [bsBox, bsFrame, bsLeftLine] then
      Canvas.DrawLine(0, 1, 0, Height - 1);
    if Shape in [bsFrame, bsRightLine] then
      Canvas.DrawLine(Width - 2, 1, Width - 2, Height - 1);
    if Shape in [bsFrame, bsBottomLine] then
      Canvas.DrawLine(1, Height - 2, Width - 1, Height - 2);
  end;

  if Style = bsRaised then
    Canvas.SetColor(clShadow2)
  else
    Canvas.SetColor(clHilite2);

  if FPanelType = ptGroupBox then
  begin
    if Shape in [bsFrame, bsTopLine] then
      Canvas.DrawLine(1, 6, Width - 2, 6);
    if Shape in [bsFrame, bsLeftLine] then
      Canvas.DrawLine(1, 7, 1, Height - 1);
    if Shape in [bsBox, bsFrame, bsRightLine] then
      Canvas.DrawLine(Width - 1, 5, Width - 1, Height - 1);
    if Shape in [bsBox, bsFrame, bsBottomLine] then
      Canvas.DrawLine(0, Height - 1, Width, Height - 1);
  end
  else
  begin
    if Shape in [bsFrame, bsTopLine] then
      Canvas.DrawLine(1, 1, Width - 2, 1);
    if Shape in [bsFrame, bsLeftLine] then
      Canvas.DrawLine(1, 2, 1, Height - 2);
    if Shape in [bsBox, bsFrame, bsRightLine] then
      Canvas.DrawLine(Width - 1, 0, Width - 1, Height - 1);
    if Shape in [bsBox, bsFrame, bsBottomLine] then
      Canvas.DrawLine(0, Height - 1, Width, Height - 1);
  end;

  Canvas.EndDraw;
end;

constructor TfpgPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPanelShape      := bsBox;
  FPanelStyle      := bsRaised;
  FWidth           := 80;
  FHeight          := 80;
  FFocusable       := True;  // otherwise children can't get focus
  FBackgroundColor := Parent.BackgroundColor;
end;

end.

