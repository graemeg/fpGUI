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

  TImageLayout = (ilImageLeft, ilImageTop, ilImageRight, ilImageBottom);

  { TfpgBaseButton }

  TfpgBaseButton = class(TfpgWidget, ICommandHolder)
  private
    FCommand: ICommand;
    FImageLayout: TImageLayout;
    FFlat: Boolean;
    FImageName: string;
    FClicked: Boolean;
    FShowImage: Boolean;
    FClickOnPush: Boolean;
    FGroupIndex: integer;
    FAllowAllUp: boolean;
    FModalResult: TfpgModalResult;
    function    GetFontDesc: string;
    procedure   SetDefault(const AValue: boolean);
    procedure   SetEmbedded(const AValue: Boolean);
    procedure   SetFlat(const AValue: Boolean);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetImageLayout(const AValue: TImageLayout);
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
    property    ImageLayout: TImageLayout read FImageLayout write SetImageLayout default ilImageLeft;
    property    ModalResult: TfpgModalResult read FModalResult write FModalResult default mrNone;
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
    property    Hint;
    property    ImageLayout;
    property    ImageMargin;
    property    ImageName;
    property    ImageSpacing;
    property    ModalResult;
    property    ShowHint;
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
  FImageLayout  := ilImageLeft;
  FImageMargin  := 3;   // image is 3 pixels from edge of button. -1 will centre image.
  FImageSpacing := -1;  // text is centered in remaining space
  FModalResult  := mrNone;
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

  procedure CalculatePositions (var ImageX, ImageY, TextX, TextY : integer);
  var {clientHeight, clientWidth,} textWidth, textHeight : integer;
      w : integer;
  begin
    if text = '' then
      begin
      textWidth := 0;
      textHeight := 0;
      end
    else
      begin
      textWidth := FFont.TextWidth (Text);
      textHeight := FFont.Height;
      // Only single line texts will be placed correctly.
      // Normally FFont.TextHeight should be used (not yet implemented)
      end;
    if FImageLayout in [ilImageLeft, ilImageRight] then
      begin
      TextY := (Height - textHeight) div 2;
      // center vertically
      if FShowImage and assigned (FImage) then
        begin
        ImageY := (Height - FImage.Height) div 2;
        // horizontal places if image and text
        if FImageMargin = -1 then
          begin  // Free space between border and image is the same as between border and text
          if FImageSpacing = -1 then // free space between image/text = border/text = border/image
            begin
            w := (Width - FImage.Width - textWidth) div 3;
            if w < 3 then  // minimal margin from border for rectangle/focusrect/...
              w := 3;
            if FImageLayout = ilImageLeft then
              begin
              ImageX := w;
              TextX := Width - w - textWidth;
              end
            else // if FImageLayout = ilImageRight then
              begin
              ImageX := Width - w - FImage.width;
              TextX := w;
              end;
            end
          else // fixed space between image/text
            begin
            w := (Width - FImageSpacing - FImage.width - textWidth) div 2;
            if w < 3 then  // minimal margin from border for rectangle/focusrect/...
              w := 3;
            if FImageLayout = ilImageLeft then
              begin
              ImageX := w;
              TextX := w + FImage.width + FImageSpacing;
              end
            else // if FImageLayout = ilImageRight then
              begin
              ImageX := width - w - FImage.Width;
              TextX := w;
              end;
            end;
          end
        else  // Fixed image
          begin
          if FImageLayout = ilImageLeft then
            begin
            ImageX := FImageMargin + 3;
            if FImageSpacing = -1 then
              begin
              w := (Width - FImage.Width - ImageX - textWidth) div 2;
              if w < 0 then
                w := 0;
              end
            else
              w := FImageSpacing;
            TextX := ImageX + FImage.width + w;
            end
          else // if FImageLayout = ilImageRight then
            begin
            ImageX := Width - FImageMargin - 3 - FImage.width;
            if FImageSpacing = -1 then
              begin
              w := (Width - FImageMargin - FImage.width - textWidth) div 2;
              if w < 3 then
                w := 3;
              TextX := w;
              end
            else
              begin
              textX := ImageX - textWidth - FImageSpacing;
              if textX < 3 then
                textX := 3;
              end;
            end;
          end;
        end
      else
        begin  // no image,
        ImageY := 0;
        ImageX := 0;
        TextX := (Width - textWidth) div 2;
        end;
      end
    else // if ImageLayout in [ilImageTop, ilImageBottom] then
      begin
      TextX := (Width - textWidth) div 2;
      // center horizontaly
      if FShowImage and assigned (FImage) then
        begin
        ImageX := (Width - FImage.Width) div 2;
        // vertical places if image and text
        if FImageMargin = -1 then
          begin  // Free space between border and image is the same as between border and text
          if FImageSpacing = -1 then // free space between image/text = border/text = border/image
            begin
            w := (Height - FImage.Height - textHeight) div 3;
            if w < 3 then  // minimal margin from border for rectangle/focusrect/...
              w := 3;
            if FImageLayout = ilImageTop then
              begin
              ImageY := w;
              TextY := Height - w - textHeight;
              end
            else // if FImageLayout = ilImageBottom then
              begin
              ImageY := Height - w - FImage.Height;
              TextY := w;
              end;
            end
          else // fixed space between image/text
            begin
            w := (Height - FImageSpacing - FImage.Height - textHeight) div 2;
            if w < 3 then  // minimal margin from border for rectangle/focusrect/...
              w := 3;
            if FImageLayout = ilImageTop then
              begin
              ImageY := w;
              TextY := w + FImage.Height + FImageSpacing;
              end
            else // if FImageLayout = ilImageRight then
              begin
              ImageY := Height - w - FImage.Height;
              TextY := w;
              end;
            end;
          end
        else  // Fixed image
          begin
          if FImageLayout = ilImageTop then
            begin
            ImageY := FImageMargin + 3;
            if FImageSpacing = -1 then
              begin
              w := (Height - FImage.Height - ImageY - textHeight) div 2;
              if w < 0 then
                w := 0;
              end
            else
              w := FImageSpacing;
            TextY := ImageY + FImage.Height + w;
            end
          else // if FImageLayout = ilImageRight then
            begin
            ImageY := Height - FImageMargin - 3 - FImage.Height;
            if FImageSpacing = -1 then
              begin
              w := (Height - FImageMargin - FImage.Height - textHeight) div 2;
              if w < 3 then
                w := 3;
              TextY := w;
              end
            else
              begin
              textY := ImageY - textHeight - FImageSpacing;
              if textY < 3 then
                textY := 3;
              end;
            end;
          end;
        end
      else
        begin  // no image,
        ImageY := 0;
        ImageX := 0;
        TextY := (Height - textHeight) div 2;
        end;
      end;
  end;


var
  AText: string;
  tx, ty, ix, iy: integer;
  r: TfpgRect;
  pofs: integer;
  lBtnFlags: TFButtonFlags;
  clr: TfpgColor;

begin
//  inherited HandlePaint;
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

  if FDown then
    pofs := 1
  else
    pofs := 0;

  CalculatePositions (ix, iy, tx, ty);

  if FShowImage and assigned (FImage) then
    Canvas.DrawImage(ix + pofs, iy + pofs, FImage);

  fpgStyle.DrawString(Canvas, tx+pofs, ty+pofs, Text, Enabled);
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

procedure TfpgBaseButton.SetImageLayout(const AValue: TImageLayout);
begin
  if FImageLayout <> AValue then
    begin
    FImageLayout := AValue;
    Repaint;  //Isn't Invalidate better ?
    end;
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

