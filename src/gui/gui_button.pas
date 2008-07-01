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

  TfpgBaseButton = class(TfpgWidget, ICommandHolder)
  private
    FCommand: ICommand;
    FFlat: Boolean;
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
    procedure   SetFlat(const AValue: Boolean);
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
    FState: integer;  // 0 - normal  // 1 - hover
    procedure   SetShowImage(AValue: Boolean);
    procedure   HandlePaint; override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleLMouseDown(X, Y: integer; ShiftState: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseExit; override;
    procedure   HandleMouseEnter; override;
    { When buttons are in a toggle state (GroupIndex > 0), are all buttons in the group
      allowed to be up. }
    property    AllowAllUp: boolean read FAllowAllUp write SetAllowAllUp default False;
    { Buttons behave like toggle buttons. This is an alias for GroupIndex > 0 }
    property    AllowDown: Boolean read GetAllowDown write SetAllowDown;
    property    Default: boolean read FDefault write SetDefault default False;
    property    Down: Boolean read FDown write SetDown;
    { The button will not show focus. It might also have a different down state (look).
      This is similar to Focusable = False, but the appearance of the down state might differ. }
    property    Embedded: Boolean read FEmbedded write SetEmbedded default False;
    property    Flat: Boolean read FFlat write SetFlat default False;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    { Used in combination with AllowDown and AllowAllUp. Allows buttons in the same
      group to work together. }
    property    GroupIndex: integer read FGroupIndex write FGroupIndex default 0;
    property    ImageMargin: integer read FImageMargin write SetImageMargin default 3;
    property    ImageName: string read FImageName write SetImageName;
    property    ImageSpacing: integer read FImageSpacing write SetImageSpacing default -1;
    property    ModalResult: integer read FModalResult write FModalResult default 0;
    property    ShowImage: Boolean read FShowImage write SetShowImage default True;
    property    Text: string read FText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Click;
    function    GetCommand: ICommand;   // ICommandHolder interface
    procedure   SetCommand(ACommand: ICommand); // ICommandHolder interface
    property    Font: TfpgFont read FFont;
  end;
  
  
  TfpgButton = class(TfpgBaseButton)
  published
    property    AllowAllUp;
    property    AllowDown;
    property    BackgroundColor default clButtonFace;
    property    Default;
    property    Down;
    property    Embedded;
    property    Flat;
    property    FontDesc;
    property    GroupIndex;
    property    ImageMargin;
    property    ImageName;
    property    ImageSpacing;
    property    ModalResult;
    property    ShowImage;
    property    Text;
    property    TextColor;
    property    TabOrder;
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

{ TfpgBaseButton }

procedure TfpgBaseButton.SetDown(AValue: Boolean);
begin
  if AValue <> FDown then
  begin
    FDown := AValue;
    if AllowDown then
      RePaint;
  end;
end;

procedure TfpgBaseButton.SetShowImage(AValue: Boolean);
begin
  if AValue <> FShowImage then
  begin
    FShowImage := AValue;
    if (FImage <> nil) and ShowImage then
      RePaint;
  end;
end;

procedure TfpgBaseButton.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
  RePaint;
end;

procedure TfpgBaseButton.SetImageName(const AValue: string);
begin
  FImageName := AValue;
  FImage     := fpgImages.GetImage(FImageName);
  Repaint;
end;

function TfpgBaseButton.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TfpgBaseButton.SetDefault(const AValue: boolean);
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
      if (wg <> nil) and (wg <> self) and (wg is TfpgBaseButton) then
      begin
        TfpgBaseButton(wg).Default := False;
      end;
    end;  { for }
  end;  { if }

  RePaint;
end;

procedure TfpgBaseButton.SetEmbedded(const AValue: Boolean);
begin
  if FEmbedded = AValue then
    Exit;
  FEmbedded := AValue;
end;

procedure TfpgBaseButton.SetFlat(const AValue: Boolean);
begin
  if FFlat = AValue then
    Exit; //==>
  FFlat := AValue;
  if FFlat then
    FDefault := False;  // you can't have it all!
end;

procedure TfpgBaseButton.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  RePaint;
end;

constructor TfpgBaseButton.Create(AOwner: TComponent);
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
  FState        := 0;
end;

destructor TfpgBaseButton.Destroy;
begin
  FImage := nil;
  FText  := '';
  FFont.Free;
  inherited Destroy;
end;

procedure TfpgBaseButton.HandlePaint;
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
    Include(lBtnFlags, btfIsPressed);

  if FFocused and (not FEmbedded) then
    Include(lBtnFlags, btfHasFocus);

  if FEmbedded then
    Include(lBtnFlags, btfIsEmbedded);

  // In the UI Designer we want the button more visible
  if not (csDesigning in ComponentState) then
  begin
    if FFlat and (FState = 1) then  // mouse over
      Include(lBtnFlags, btfHover)
    else if FFlat then
      Include(lBtnFlags, btfFlat);
  end;

  if not FFlat and FDefault then
    Include(lBtnFlags, btfIsDefault);

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

procedure TfpgBaseButton.DoPush;
var
  n: integer;
  c: TComponent;
begin
  FClickOnPush := (not FDown) and AllowDown;

  // search the other buttons in the group
  for n := 0 to Parent.ComponentCount - 1 do
  begin
    c := Parent.Components[n];
    if (c <> self) and (c is TfpgBaseButton) then
      with TfpgBaseButton(c) do
        if GroupIndex = self.GroupIndex then
          Down := False;
  end;

  FDown    := True;
  FClicked := True;

  RePaint;
  if FClickOnPush then
    Click;
end;

procedure TfpgBaseButton.DoRelease(x, y: integer);
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
      fpgApplication.ProcessMessages;
      if PtInRect(r, Point(x, y)) then
        Click;
    end;
  end
  else
  begin
    if FDown and FClicked then
    begin
      FDown := False;
      RePaint;
      fpgApplication.ProcessMessages;
      if PtInRect(r, Point(x, y)) then
        Click;
    end;
  end;

  FClickOnPush := False;
  FClicked     := False;
end;

procedure TfpgBaseButton.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if (keycode = keyReturn) or (keycode = keySpace) or (keycode = keyPEnter) then
  begin
    DoPush;
    Consumed := True;
  end
  else
    inherited;
end;

procedure TfpgBaseButton.HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if (keycode = keyReturn) or (keycode = keySpace) or (keycode = keyPEnter) then
  begin
    DoRelease(1, 1); // fake co-ordinates to it executes the Click
    Consumed := True;
  end
  else
    inherited;
end;

procedure TfpgBaseButton.HandleLMouseDown(X, Y: integer; ShiftState: TShiftState);
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;
  CaptureMouse;
  DoPush;
end;

procedure TfpgBaseButton.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
//  inherited;
  if (csDesigning in ComponentState) then
    Exit;
  ReleaseMouse;
  DoRelease(x, y);
end;

procedure TfpgBaseButton.HandleMouseExit;
begin
  inherited HandleMouseExit;
  if (csDesigning in ComponentState) then
    Exit;
  if Enabled then
    FState := 0;
  if FDown and (not AllowDown) then
  begin
    FDown := False;
    Repaint;
  end
  else if FFlat then
  begin
    if Enabled then
      Repaint;
  end;
end;

procedure TfpgBaseButton.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  if (csDesigning in ComponentState) then
    Exit;
  if Enabled then
    FState := 1;
  if FClicked and (not AllowDown) then
  begin
    FDown := True;
    Repaint;
  end
  else if FFlat then
  begin
    if Enabled then
      Repaint;
  end;
end;

procedure TfpgBaseButton.Click;
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

function TfpgBaseButton.GetCommand: ICommand;
begin
  Result := FCommand;
end;

procedure TfpgBaseButton.SetCommand(ACommand: ICommand);
begin
  FCommand := ACommand;
end;

procedure TfpgBaseButton.SetImageMargin(const Value: integer);
begin
  FImageMargin := Value;
  Repaint;
end;

procedure TfpgBaseButton.SetImageSpacing(const Value: integer);
begin
  FImageSpacing := Value;
  Repaint;
end;

function TfpgBaseButton.GetAllowDown: Boolean;
begin
  Result := GroupIndex > 0;
end;

procedure TfpgBaseButton.SetAllowDown(const Value: Boolean);
begin
  GroupIndex := 1;
end;

procedure TfpgBaseButton.SetAllowAllUp(const Value: boolean);
begin
  FAllowAllUp := Value;
end;

end.

