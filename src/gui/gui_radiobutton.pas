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
      Defines a Radio Button control.
}

unit gui_radiobutton;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpgfx,
  gfxbase,
  gfx_widget;

type

  { TfpgRadioButton }

  TfpgRadioButton = class(TfpgWidget)
  private
    FBackgroundColor: TfpgColor;
    FChecked: boolean;
    FTextColor: TfpgColor;
    FFont: TfpgFont;
    FGroupIndex: integer;
    FOnChange: TNotifyEvent;
    FText: string;
    FBoxSize: integer;
    FIsPressed: boolean;
    function    GetFontDesc: string;
    procedure   SetBackgroundColor(const AValue: TfpgColor);
    procedure   SetChecked(const AValue: boolean);
    procedure   SetTextColor(const AValue: TfpgColor);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetText(const AValue: string);
  protected
    procedure   HandlePaint; override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Font: TfpgFont read FFont;
  published
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor default clWindowBackground;
    property    Checked: boolean read FChecked write SetChecked default False;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    GroupIndex: integer read FGroupIndex write FGroupIndex;
    property    Text: string read FText write SetText;
    property    TextColor: TfpgColor read FTextColor write SetTextColor default clText1;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
  
  
function CreateRadioButton(AOwner: TComponent; x, y: TfpgCoord; AText: string): TfpgRadioButton;

implementation

function CreateRadioButton(AOwner: TComponent; x, y: TfpgCoord; AText: string): TfpgRadioButton;
begin
  Result := TfpgRadioButton.Create(AOwner);
  Result.Top    := y;
  Result.Left   := x;
  Result.Text   := AText;
  Result.Width  := Result.Font.TextWidth(Result.Text) + 24;
end;

{ TfpgRadioButton }

function TfpgRadioButton.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TfpgRadioButton.SetBackgroundColor(const AValue: TfpgColor);
begin
  if FBackgroundColor = AValue then
    Exit; //==>
  FBackgroundColor := AValue;
  RePaint;
end;

procedure TfpgRadioButton.SetChecked(const AValue: boolean);
var
  i: integer;
  wg: TfpgWidget;
begin
  if FChecked = AValue then
    Exit; //==>
  FChecked := AValue;
  
  // Clear other radio buttons in the same group
  if FChecked and (Parent <> nil) then
  begin
    for i := 0 to Parent.ComponentCount-1 do
    begin
      wg := TfpgWidget(Parent.Components[i]);
      if (wg <> nil) and (wg <> self) and (wg is TfpgRadioButton) and
          (TfpgRadioButton(wg).GroupIndex = GroupIndex) then
      begin
        TfpgRadioButton(wg).Checked := False;
      end;
    end;  { for }
  end;  { if }

  RePaint;
end;

procedure TfpgRadioButton.SetTextColor(const AValue: TfpgColor);
begin
  if FTextColor <> AValue then
  begin
    FTextColor := AValue;
    Repaint;
  end;
end;

procedure TfpgRadioButton.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  RePaint;
end;

procedure TfpgRadioButton.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit; //==>
  FText := AValue;
  RePaint;
end;

procedure TfpgRadioButton.HandlePaint;
var
  r: TfpgRect;
  ty: integer;
  tx: integer;
  img: TfpgImage;
  ix: integer;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;

  Canvas.SetColor(FBackgroundColor);
  Canvas.FillRectangle(0, 0, Width, Height);
  Canvas.SetFont(Font);

  if FFocused then
  begin
    Canvas.SetColor(clText1);
    Canvas.SetLineStyle(1, lsDot);
    Canvas.DrawRectangle(1, 1, Width-1, Height-1);
  end;
  Canvas.SetLineStyle(1, lsSolid);

  r.SetRect(2, (Height div 2) - (FBoxSize div 2), FBoxSize, FBoxSize);
  if r.top < 0 then
    r.top := 0;

  // calculate which image to paint.
  if Enabled then
  begin
    ix := Ord(FChecked);
    if FIsPressed then
      Inc(ix, 2);
  end
  else
    ix := (2 + (Ord(FChecked) * 2)) - Ord(FChecked);

  // paint the radio button
  tx := r.right + 8;
  inc(r.left, 2);
  inc(r.top, 1);
  img := fpgImages.GetImage('sys.radiobuttons');
  Canvas.DrawImagePart(r.Left, r.Top, img, ix*12, 0, 12, 12);

  ty := (Height div 2) - (Font.Height div 2);
  if ty < 0 then
    ty := 0;
  Canvas.SetTextColor(FTextColor);
  fpgStyle.DrawString(Canvas, tx, ty, FText, Enabled);

  Canvas.EndDraw;
end;

procedure TfpgRadioButton.HandleLMouseDown(x, y: integer;
  shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  FIsPressed := True;
  Repaint;
end;

procedure TfpgRadioButton.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  FIsPressed := False;
  if not Checked then
  begin
    Checked := not FChecked;
    if Assigned(FOnChange) then
      FOnChange(self);
  end
  else
    RePaint;
end;

procedure TfpgRadioButton.HandleKeyRelease(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  if (keycode = keySpace) then
  begin
    consumed := True;
    Checked := not FChecked;
    if Assigned(FOnChange) then
      FOnChange(self);
  end;

  if consumed then
    Exit; //==>

  inherited HandleKeyRelease(keycode, shiftstate, consumed);
end;

constructor TfpgRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText       := 'RadioButton';
  FFont       := fpgGetFont('#Label1');
  FHeight     := FFont.Height + 4;
  FWidth      := 120;
  FTextColor  := clText1;
  FBackgroundColor := clWindowBackground;
  FFocusable  := True;
  FBoxSize    := 12;
  FChecked    := False;
  FGroupIndex := 0;
  FIsPressed  := False;
  FOnChange   := nil;
end;

destructor TfpgRadioButton.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

end.

