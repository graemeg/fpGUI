unit gui_button;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_widget;

type

  TfpgButton = class(TfpgWidget)
  private
    FEmbedded: Boolean;
    FImageName: string;
    FClicked: Boolean;
    FImage: TfpgImage;
    FShowImage: Boolean;
    FClickOnPush: Boolean;
    FDown: Boolean;
    FImageMargin: integer;
    FImageSpacing: integer;
    FGroupIndex: integer;
    FAllowAllUp: boolean;
    FModalResult: integer;
    function    GetFontDesc: string;
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
  protected
    FText: string;
    FFont: TfpgFont;
    procedure   SetShowImage(AValue: Boolean);
    procedure   HandlePaint; override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleLMouseDown(X, Y: integer; ShiftState: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseExit; override;
    procedure   HandleMouseEnter; override;
  public
    OnClick: TNotifyEvent;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   DoPush;
    procedure   DoRelease;
    procedure   Click;
    property    Down: Boolean read FDown write SetDown;
    property    Font: TfpgFont read FFont;
    property    AllowDown: Boolean read GetAllowDown write SetAllowDown;
  published
    property    Text: string read FText write SetText;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    ImageName: string read FImageName write SetImageName;
    property    ImageMargin: integer read FImageMargin write SetImageMargin;
    property    ImageSpacing: integer read FImageSpacing write SetImageSpacing;
    property    GroupIndex: integer read FGroupIndex write FGroupIndex;
    property    AllowAllUp: boolean read FAllowAllUp write SetAllowAllUp;
    property    ModalResult: integer read FModalResult write FModalResult;
    property    Embedded: Boolean read FEmbedded write SetEmbedded default False;
    property    ShowImage: Boolean read FShowImage write SetShowImage default True;
  end;

function CreateButton(AOwner: TComponent; x, y, w: TfpgCoord; AText: string;
  AOnClickEvent: TNotifyEvent): TfpgButton;

implementation

uses
  gui_form; {$Note Try and remove this gui_form dependency.}

function CreateButton(AOwner: TComponent; x, y, w: TfpgCoord; AText: string;
  AOnClickEvent: TNotifyEvent): TfpgButton;
begin
  Result         := TfpgButton.Create(AOwner);
  Result.Left    := x;
  Result.Top     := y;
  Result.Text    := AText;
  Result.Width   := w;
  Result.OnClick := AOnClickEvent;
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
  FWidth        := 75;
  FFocusable    := True;
  OnClick       := nil;
  FDown         := False;
  FClicked      := False;
  FDown         := False;
  FClickOnPush  := False;
  FGroupIndex   := 0;
  FImage        := nil;
  FImageName    := '';
  FShowImage    := True;
  FImageMargin  := 3;
  FImageSpacing := -1;
  FModalResult  := 0;
  FEmbedded     := False;
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
begin
  Canvas.BeginDraw;
  inherited HandlePaint;
  Canvas.Clear(clButtonFace);
  Canvas.ClearClipRect;

  lBtnFlags := [];
  if FDown then
    Include(lBtnFlags, btnIsPressed);

  if FFocused and (not FEmbedded) then
    Include(lBtnFlags, btnHasFocus);

  if FEmbedded then
    Include(lBtnFlags, btnIsEmbedded);

  fpgStyle.DrawButtonFace(Canvas, 0, 0, Width, Height, lBtnFlags);

  if FFocused and (not FEmbedded) then
  begin
    Canvas.SetColor(clText1);
    Canvas.SetLineStyle(1, lsDot);
    Canvas.DrawRectangle(3, 3, Width - 6, Height - 6);
  end
  else
  begin
    Canvas.SetTextColor(clText1);
    Canvas.SetColor(clText1);
  end;

  r.left   := 2;
  r.top    := 2;
  r.Width  := Width - 4;
  r.Height := Height - 4;
  Canvas.SetClipRect(r);

  Canvas.SetFont(Font);
  AText := FText;
  y     := Height div 2 - FFont.Height div 2;
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

  Canvas.DrawString(x + pofs, y + pofs, AText);
  Canvas.EndDraw;
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

procedure TfpgButton.DoRelease;
begin
  if AllowDown then
  begin
    if FDown and (not FClickOnPush) and FAllowAllUp then
    begin
      FDown := False;
      RePaint;
      Click;
    end;
  end
  else
  begin
    if FDown and FClicked then
      Click;
    FDown := False;
    RePaint;
  end;

  FClickOnPush := False;
  FClicked     := False;
end;

procedure TfpgButton.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if (keycode = keyReturn) or (keycode = keySpace) then
  begin
    DoPush;
    Consumed := True;
  end
  else
    inherited;
end;

procedure TfpgButton.HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if (keycode = keyReturn) or (keycode = keySpace) then
  begin
    DoRelease;
    Consumed := True;
  end
  else
    inherited;
end;

procedure TfpgButton.HandleLMouseDown(X, Y: integer; ShiftState: TShiftState);
begin
  inherited;
  DoPush;
end;

procedure TfpgButton.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited;
  DoRelease;
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
  pform := WidgetParentForm(self);
  if pform <> nil then
    pform.ModalResult := ModalResult;

  if Assigned(OnClick) then
    OnClick(self);
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

