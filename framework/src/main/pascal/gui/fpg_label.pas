{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 by Graeme Geldenhuys.

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
  fpg_widget,
  fpg_accelerator_intf;

type


  TfpgCustomLabel = class(TfpgWidget, IfpgAcceleratorTarget)
  private
    FAutoSize: boolean;
    FAlignment: TAlignment;
    FLayout: TLayout;
    FWrapText: boolean;
    FLineSpace: integer;
    FFocusWidget: TfpgWidget;
    procedure   SetFocusWidget(const AValue: TfpgWidget);
    procedure   SetWrapText(const AValue: boolean);
    procedure   SetAlignment(const AValue: TAlignment);
    procedure   SetLayout(const AValue: TLayout);
    procedure   SetAutoSize(const AValue: boolean);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetText(const AValue: TfpgString);
    procedure   ResizeLabel;
  protected
    FText: TfpgString;
    FTextHeight: integer;
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure   DoCalculatePreferredSize(var ASize: TfpgSize); override;
    procedure   HandlePaint; override;
    { Text as actually painted - with any '&' accelerator markers resolved. }
    function    GetDisplayText: TfpgString; virtual;
    { The widget an accelerator on this label gives focus to. When nil the
      label has no accelerator, even if its text contains an '&'. }
    property    FocusWidget: TfpgWidget read FFocusWidget write SetFocusWidget;
    property    WrapText: boolean read FWrapText write SetWrapText default False;
    property    Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property    AutoSize: boolean read FAutoSize write SetAutoSize default False;
    property    Layout: TLayout read FLayout write SetLayout default tlTop;
    property    Text: TfpgString read FText write SetText;
    property    LineSpace: integer read FLineSpace write FLineSpace default 2;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    TextHeight: integer read FTextHeight;
    { IfpgAcceleratorTarget }
    function    GetAcceleratorChar: TfpgString;
    function    CanAcceptAccelerator: boolean;
    procedure   ExecuteAccelerator;
  end;


  TfpgLabel = class(TfpgCustomLabel)
  published
    property    AcceptDrops;
    property    Align;
    property    Alignment;
    property    AutoSize;
    property    BackgroundColor;
    property    Enabled;
    property    FocusWidget;
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

uses
  fpg_stringutils;


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
    Result.Width := Result.Font.GetTextWidth(fpgStripAccelChars(Result.Text));
    Result.AutoSize := True;
  end
  else
    Result.Width := w;
end;

{ TfpgCustomLabel }

procedure TfpgCustomLabel.SetFocusWidget(const AValue: TfpgWidget);
begin
  if FFocusWidget = AValue then
    Exit; //==>
  FFocusWidget := AValue;
  if FFocusWidget <> nil then
    { FocusWidget is a plain reference we do not own, so ask to be told when
      it is destroyed - otherwise we would be left pointing at freed memory }
    FFocusWidget.FreeNotification(self);
end;

procedure TfpgCustomLabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusWidget) then
    FFocusWidget := nil;
end;

function TfpgCustomLabel.GetDisplayText: TfpgString;
begin
  Result := fpgStripAccelChars(FText);
end;

function TfpgCustomLabel.GetAcceleratorChar: TfpgString;
begin
  { a label is only an accelerator target when it has something to focus }
  if FFocusWidget = nil then
    Result := ''
  else
    Result := fpgExtractAccelChar(FText);
end;

function TfpgCustomLabel.CanAcceptAccelerator: boolean;
begin
  { the label itself is never focused - what matters is whether the widget it
    points at can actually take focus }
  Result := Visible and Assigned(FFocusWidget)
      and FFocusWidget.Enabled and FFocusWidget.Visible;
end;

procedure TfpgCustomLabel.ExecuteAccelerator;
begin
  { Matching the VCL: a label's accelerator only moves focus, it never
    activates the target. }
  if Assigned(FFocusWidget) and FFocusWidget.Focusable then
    FFocusWidget.SetFocus;
end;

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
  inherited SetFontDesc(AValue);  // Call base class to update Font property
  ResizeLabel;  // Label-specific: resize based on new font
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
    Width := Font.GetTextWidth(GetDisplayText);
    Height:= Font.GetHeight;
  end
  else if (FPreferredSize.W = 0) and (FPreferredSize.H = 0) then
  begin
    { No explicit preferred size — preferred size is calculated dynamically from
      text content. Notify the parent layout manager to re-run layout so it can
      query the updated preferred size (via GetPreferredSize -> DoCalculatePreferredSize). }
    if Assigned(Parent) and (Parent is TfpgWidget) and
       Assigned(TfpgWidget(Parent).LayoutManager) then
      TfpgWidget(Parent).Realign;
  end;
  UpdatePosition;
  RePaint;
end;

constructor TfpgCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText             := 'Label';
  FontDesc          := '#Label1';  // Use property to set font (calls inherited SetFontDesc)
  FHeight           := Font.GetHeight;
  FWidth            := 80;
  { Do not set FPreferredSize here. Leaving it at (0,0) allows GetPreferredSize
    to call DoCalculatePreferredSize, which computes width dynamically from the
    text content. If a developer needs a fixed size they can set PreferredSize
    explicitly after construction. }
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
  // Font is now managed by TfpgWidgetBase - no need to clean up here
  inherited Destroy;
end;

procedure TfpgCustomLabel.DoCalculatePreferredSize(var ASize: TfpgSize);
begin
  { This method is only called by GetPreferredSize when FPreferredSize is (0,0),
    meaning no explicit size was set. Calculate purely from text content. }
  if Assigned(Font) then
  begin
    ASize.W := Font.GetTextWidth(GetDisplayText);
    ASize.H := Font.GetHeight;
  end
  else
  begin
    ASize.W := FMinWidth;
    ASize.H := FMinHeight;
  end;
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

  { only underline the accelerator when there is something for it to focus }
  if (FFocusWidget <> nil) and (fpgExtractAccelChar(FText) <> '') then
    Include(lTxtFlags, txtAccel);

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

