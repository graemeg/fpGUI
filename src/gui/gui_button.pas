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
      Defines a push button control.
}

unit gui_button;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_widget,
  gfx_command_intf;

type

  TfpgButton = class(TfpgWidget, ICommandHolder)
  private
    FCommand: ICommand;
    FImageName: string;
    FClicked: Boolean;
    FShowImage: Boolean;
    FClickOnPush: Boolean;
    FGroupIndex: integer;
    FAllowAllUp: boolean;
    FModalResult: integer;
    function    GetFontDesc: string;
    procedure   SetDefault(const AValue: boolean);
    procedure   SetEmbedded(const AValue: Boolean);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetImageName(const AValue: string);
    procedure   SetText(const AValue: string);
    procedure   SetDown(AValue: Boolean);
    procedure   SetImageMargin(const Value: integer);
    procedure   SetImageSpacing(const Value: integer);
    function    GetAllowDown: Boolean;
    procedure   SetAllowDown(const Value: Boolean);
    procedure   SetAllowAllUp(const Value: boolean);
    procedure   DoPush;
    procedure   DoRelease(x, y: integer);
  protected
    FImageMargin: integer;
    FImageSpacing: integer;
    FEmbedded: Boolean;
    FDown: Boolean;
    FImage: TfpgImage;
    FText: string;
    FFont: TfpgFont;
    FDefault: boolean;
    procedure   SetShowImage(AValue: Boolean);
    procedure   HandlePaint; override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleLMouseDown(X, Y: integer; ShiftState: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseExit; override;
    procedure   HandleMouseEnter; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Click;
    function    GetCommand: ICommand;   // ICommandHolder interface
    procedure   SetCommand(ACommand: ICommand); // ICommandHolder interface
    property    Down: Boolean read FDown write SetDown;
    property    Font: TfpgFont read FFont;
    { Buttons behave like toggle buttons. This is an alias for GroupIndex > 0 }
    property    AllowDown: Boolean read GetAllowDown write SetAllowDown;
  published
    { When buttons are in a toggle state (GroupIndex > 0), are all buttons in the group
      allowed to be up. }
    property    AllowAllUp: boolean read FAllowAllUp write SetAllowAllUp default False;
    property    BackgroundColor default clButtonFace;
    property    Default: boolean read FDefault write SetDefault default False;
    { The button will not show focus. It might also have a different down state (look).
      This is similar to Focusable = False, but the appearance of the down state might differ. }
    property    Embedded: Boolean read FEmbedded write SetEmbedded default False;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    { Used in combination with AllowDown and AllowAllUp. Allows buttons in the same
      group to work together. }
    property    GroupIndex: integer read FGroupIndex write FGroupIndex default 0;
    property    ImageMargin: integer read FImageMargin write SetImageMargin default 3;
    property    ImageName: string read FImageName write SetImageName;
    property    ImageSpacing: integer read FImageSpacing write SetImageSpacing default -1;
    property    ModalResult: integer read FModalResult write FModalResult default 0;
    property    ShowImage: Boolean read FShowImage write SetShowImage default True;
    property    TabOrder;
    property    Text: string read FText write SetText;
    property    TextColor;
    property    OnMouseExit;
    property    OnMouseEnter;
    property    OnClick;
  end;


function CreateButton(AOwner: TComponent; x, y, w: TfpgCoord; AText: string;
  AOnClickEvent: TNotifyEvent; AImage: string = ''): TfpgButton;


implementation

uses
  gui_form; {$Note Try and remove this gui_form dependency.}

function CreateButton(AOwner: TComponent; x, y, w: TfpgCoord; AText: string;
  AOnClickEvent: TNotifyEvent; AImage: string): TfpgButton;
begin
  Result         := TfpgButton.Create(AOwner);
  Result.Text    := AText;
  Result.SetPosition(x, y, w, Result.Height); // font was used to calculate height.
  Result.OnClick := AOnClickEvent;
  Result.ImageName := AImage;
end;

{ TfpgButton }

procedure TfpgButton.SetDown(AValue: Boolean);
begin
  if AValue <> FDown then
  begin
    FDown := AValue;
    if AllowDown then
      RePaint;
  end;
end;

procedure TfpgButton.SetShowImage(AValue: Boolean);
begin
  if AValue <> FShowImage then
  begin
    FShowImage := AValue;
    if (FImage <> nil) and ShowImage then
      RePaint;
  end;
end;

procedure TfpgButton.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
  RePaint;
end;

procedure TfpgButton.SetImageName(const AValue: string);
begin
  FImageName := AValue;
  FImage     := fpgImages.GetImage(FImageName);
  Repaint;
end;

function TfpgButton.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TfpgButton.SetDefault(const AValue: boolean);
var
  i: integer;
  wg: TfpgWidget;
begin
  if FDefault = AValue then
    Exit; //==>
  FDefault := AValue;

  // Clear other buttons Default state
  if FDefault and (Parent <> nil) then
  begin
    for i := 0 to Parent.ComponentCount-1 do
    begin
      wg := TfpgWidget(Parent.Components[i]);
      if (wg <> nil) and (wg <> self) and (wg is TfpgButton) then
      begin
        TfpgButton(wg).Default := False;
      end;
    end;  { for }
  end;  { if }

  RePaint;
end;

procedure TfpgButton.SetEmbedded(const AValue: Boolean);
begin
  if FEmbedded = AValue then
    Exit;
  FEmbedded := AValue;
end;

procedure TfpgButton.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  RePaint;
end;

constructor TfpgButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText         := 'Button';
  FFont         := fpgGetFont('#Label1');
  FHeight       := FFont.Height + 8;
  FWidth        := 80;
  FFocusable    := True;
  FTextColor    := Parent.TextColor;
  FBackgroundColor := clButtonFace;
  OnClick       := nil;
  FDown         := False;
  FClicked      := False;
  FDown         := False;
  FClickOnPush  := False;
  FGroupIndex   := 0;
  FImage        := nil;
  FImageName    := '';
  FShowImage    := True;
  FImageMargin  := 3;   // image is 3 pixels from edge of button. -1 will centre image.
  FImageSpacing := -1;  // text is centered in remaining space
  FModalResult  := 0;
  FEmbedded     := False;
  FDefault      := False;
  FAllowAllUp   := False;
end;

destructor TfpgButton.Destroy;
begin
  FImage := nil;
  FText  := '';
  FFont.Free;
  inherited Destroy;
end;

procedure TfpgButton.HandlePaint;
var
  AText: string;
  x, y, iy, w: integer;
  r: TfpgRect;
  pofs: integer;
  lBtnFlags: TFButtonFlags;
  clr: TfpgColor;
begin
//  inherited HandlePaint;
//  Canvas.Clear(FBackgroundColor);  // Do we need this?
  Canvas.ClearClipRect;

  r.SetRect(0, 0, Width, Height);

  lBtnFlags := [];
  if FDown then
    Include(lBtnFlags, btnIsPressed);

  if FFocused and (not FEmbedded) then
    Include(lBtnFlags, btnHasFocus);

  if FEmbedded then
    Include(lBtnFlags, btnIsEmbedded);

  if FDefault then
    Include(lBtnFlags, btnIsDefault);

  if FBackgroundColor <> clButtonFace then
  begin
    clr := fpgColorToRGB(clButtonFace);
    fpgSetNamedColor(clButtonface, FBackgroundColor);
    Canvas.DrawButtonFace(r, lBtnFlags);
    fpgSetNamedColor(clButtonface, clr);
  end
  else
    Canvas.DrawButtonFace(r, lBtnFlags);

  if FFocused and (not FEmbedded) then
  begin
    InflateRect(r, -3, -3);
    Canvas.DrawFocusRect(r);
  end;

  Canvas.SetTextColor(FTextColor);
  Canvas.SetColor(clText1);

  Canvas.SetClipRect(r);
  Canvas.SetFont(Font);
  AText := FText;

  y := (Height div 2) - (FFont.Height div 2);
  if y < 3 then
    y := 3;

  if FDown then
    pofs := 1
  else
    pofs := 0;

  if (ShowImage) and (FImage <> nil) then
  begin
    iy := Height div 2 - FImage.Height div 2;
    if ImageMargin = -1 then // centered
    begin
      w := FFont.TextWidth(AText) + FImage.Width;
      if FImageSpacing > 0 then
        Inc(w, FImageSpacing);
      x := (Width div 2) - (w div 2);
      if x < 3 then
        x := 3;
    end
    else
    begin
      x := FImageMargin + 3;
    end;

    Canvas.DrawImage(x + pofs, iy + pofs, FImage);
    Inc(x, FImage.Width);
    if FImageSpacing > 0 then
      Inc(x, FImageSpacing);

    if (FImageSpacing = -1) and (FImageMargin >= 0) then
    begin
      w := (Width - 3 - x) div 2 - FFont.TextWidth(AText) div 2;
      if w < 1 then
        w := 1; // minimal spacing
      x := x + w;
    end;
  end
  else
    x := (Width div 2) - (FFont.TextWidth(AText) div 2);

  if x < 3 then
    x := 3;

  fpgStyle.DrawString(Canvas, x+pofs, y+pofs, AText, Enabled);
end;

procedure TfpgButton.DoPush;
var
  n: integer;
  c: TComponent;
begin
  FClickOnPush := (not FDown) and AllowDown;

  // search the other buttons in the group
  for n := 0 to Parent.ComponentCount - 1 do
  begin
    c := Parent.Components[n];
    if (c <> self) and (c is TfpgButton) then
      with TfpgButton(c) do
        if GroupIndex = self.GroupIndex then
          Down := False;
  end;

  FDown    := True;
  FClicked := True;

  RePaint;
  if FClickOnPush then
    Click;
end;

procedure TfpgButton.DoRelease(x, y: integer);
var
  r: TfpgRect;
begin
  r.SetRect(0, 0, Width, Height);
  if AllowDown then
  begin
    if FDown and (not FClickOnPush) and FAllowAllUp then
    begin
      FDown := False;
      RePaint;
      if PtInRect(r, Point(x, y)) then
        Click;
    end;
  end
  else
  begin
    if FDown and FClicked then
      if PtInRect(r, Point(x, y)) then
        Click;
    FDown := False;
    RePaint;
  end;

  FClickOnPush := False;
  FClicked     := False;
end;

procedure TfpgButton.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if (keycode = keyReturn) or (keycode = keySpace) or (keycode = keyPEnter) then
  begin
    DoPush;
    Consumed := True;
  end
  else
    inherited;
end;

procedure TfpgButton.HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if (keycode = keyReturn) or (keycode = keySpace) or (keycode = keyPEnter) then
  begin
    DoRelease(1, 1); // fake co-ordinates to it executes the Click
    Consumed := True;
  end
  else
    inherited;
end;

procedure TfpgButton.HandleLMouseDown(X, Y: integer; ShiftState: TShiftState);
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;
  CaptureMouse;
  DoPush;
end;

procedure TfpgButton.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
//  inherited;
  if (csDesigning in ComponentState) then
    Exit;
  ReleaseMouse;
  DoRelease(x, y);
end;

procedure TfpgButton.HandleMouseExit;
begin
  inherited HandleMouseExit;
  if FDown and (not AllowDown) then
  begin
    FDown := False;
    RePaint;
  end;
end;

procedure TfpgButton.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  if FClicked and (not AllowDown) then
  begin
    FDown := True;
    RePaint;
  end;
end;

procedure TfpgButton.Click;
var
  pform: TfpgForm;
begin
  if (not AllowDown) then
  begin
    FDown    := False;
    FClicked := False;
  end;

  pform := WidgetParentForm(self);
  if pform <> nil then
    pform.ModalResult := ModalResult;

  if Assigned(OnClick) then
    OnClick(self);
end;

function TfpgButton.GetCommand: ICommand;
begin
  Result := FCommand;
end;

procedure TfpgButton.SetCommand(ACommand: ICommand);
begin
  FCommand := ACommand;
end;

procedure TfpgButton.SetImageMargin(const Value: integer);
begin
  FImageMargin := Value;
  Repaint;
end;

procedure TfpgButton.SetImageSpacing(const Value: integer);
begin
  FImageSpacing := Value;
  Repaint;
end;

function TfpgButton.GetAllowDown: Boolean;
begin
  Result := GroupIndex > 0;
end;

procedure TfpgButton.SetAllowDown(const Value: Boolean);
begin
  GroupIndex := 1;
end;

procedure TfpgButton.SetAllowAllUp(const Value: boolean);
begin
  FAllowAllUp := Value;
end;

end.

