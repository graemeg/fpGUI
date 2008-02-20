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
    FChecked: boolean;
    FFont: TfpgFont;
    FGroupIndex: integer;
    FOnChange: TNotifyEvent;
    FText: string;
    FBoxSize: integer;
    FIsPressed: boolean;
    function    GetFontDesc: string;
    procedure   SetChecked(const AValue: boolean);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetText(const AValue: string);
  protected
    procedure   HandlePaint; override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    function    FindNeighbor(direction: TFocusSearchDirection): TfpgRadioButton;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Font: TfpgFont read FFont;
  published
    property    BackgroundColor;
    property    Checked: boolean read FChecked write SetChecked default False;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    GroupIndex: integer read FGroupIndex write FGroupIndex;
    property    TabOrder;
    property    Text: string read FText write SetText;
    property    TextColor;
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

  if Assigned(FOnChange) then
      FOnChange(self);
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
  end
  else
    RePaint;
end;

procedure TfpgRadioButton.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  nbr: TfpgRadioButton;
begin
  if (keycode = keyUp) then
  begin
    consumed := True;
    nbr := FindNeighbor(fsdPrev);
    if nbr = Self then
      nbr := FindNeighbor(fsdLast);
    nbr.SetFocus;
    nbr.Checked := True;
  end else
  if (keycode = keyDown) then
  begin
    consumed := True;
    nbr := FindNeighbor(fsdNext);
    if nbr = Self then
      nbr := FindNeighbor(fsdFirst);
    nbr.SetFocus;
    nbr.Checked := True;
  end;

  if consumed then
    Exit; //==>

  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfpgRadioButton.HandleKeyRelease(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  if (keycode = keySpace) then
  begin
    consumed := True;
    Checked := true;
  end;

  if consumed then
    Exit; //==>

  inherited HandleKeyRelease(keycode, shiftstate, consumed);
end;

function TfpgRadioButton.FindNeighbor(direction: TFocusSearchDirection): TfpgRadioButton;
var
  i: integer;
  wg, bestwg: TfpgWidget;
  bestdsp, dsp: integer; // spacing delta
begin
  if (Parent <> nil) then
  begin
    case direction of
      fsdNext: bestdsp := High(integer); // or "-999999" like that in TfpgWidget.FindFocusWidget?
      fsdPrev: bestdsp := Low(integer);  // or "999999"?
    end;

    bestwg := Self;
    for i := 0 to Parent.ComponentCount-1 do
    begin
      wg := TfpgWidget(Parent.Components[i]);
      if (wg <> nil) and (wg <> self) and (wg is TfpgRadioButton) and
          (TfpgRadioButton(wg).GroupIndex = GroupIndex) then
      begin
        case direction of
          fsdNext:
            begin
              dsp := (wg.Top - Self.Top) + (wg.Left - Self.Left);
              if (dsp > 0) and (dsp < bestdsp) then
              begin
                bestwg := wg;
                bestdsp := dsp;
              end;
            end;
          fsdPrev:
            begin
              dsp := (wg.Top - Self.Top) + (wg.Left - Self.Left);
              if (dsp < 0) and (dsp > bestdsp) then
              begin
                bestwg := wg;
                bestdsp := dsp;
              end;
            end;
          fsdFirst:
            if (wg.Top < bestwg.Top) then
              bestwg := wg;
          fsdLast:
            if (wg.Top > bestwg.Top) then
              bestwg := wg;
        end;
        Result := TfpgRadioButton(bestwg);
      end;
    end;  { for }
  end;  { if }
end;

constructor TfpgRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText       := 'RadioButton';
  FFont       := fpgGetFont('#Label1');
  FHeight     := FFont.Height + 4;
  FWidth      := 120;
  FTextColor  := Parent.TextColor;
  FBackgroundColor := Parent.BackgroundColor;
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

