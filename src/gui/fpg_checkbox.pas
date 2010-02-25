{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2009 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a CheckBox control. Also known as a Check Button control.
}

unit fpg_checkbox;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget;
  
type

  TfpgBaseCheckBox = class(TfpgWidget)
  private
    FChecked: boolean;
    FOnChange: TNotifyEvent;
    FText: string;
    FFont: TfpgFont;
    FBoxLayout: TBoxLayout;
    FBoxSize: integer;
    FIsPressed: boolean;
    function    GetBoxLayout: TBoxLayout;
    function    GetFontDesc: string;
    procedure   SetBoxLayout(const AValue: TBoxLayout);
    procedure   SetChecked(const AValue: boolean);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetText(const AValue: string);
    procedure   DoOnChange;
  protected
    procedure   HandlePaint; override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    property    Checked: boolean read FChecked write SetChecked default False;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    BoxLayout: TBoxLayout read GetBoxLayout write SetBoxLayout default tbLeftBox;
    property    Text: string read FText write SetText;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Font: TfpgFont read FFont;
  end;


  TfpgCheckBox = class(TfpgBaseCheckBox)
  published
    property    BackgroundColor;
    property    BoxLayout;
    property    Checked;
    property    FontDesc;
    property    Hint;
    property    ParentShowHint;
    property    ShowHint;
    property    TabOrder;
    property    Text;
    property    TextColor;
    property    OnChange;
    property    OnEnter;
    property    OnExit;
  end;


function CreateCheckBox(AOwner: TComponent; x, y: TfpgCoord; AText: string): TfpgCheckBox;


implementation


function CreateCheckBox(AOwner: TComponent; x, y: TfpgCoord; AText: string): TfpgCheckBox;
begin
  Result := TfpgCheckBox.Create(AOwner);
  Result.Top    := y;
  Result.Left   := x;
  Result.Text   := AText;
  Result.Width  := Result.Font.TextWidth(Result.Text) + 24;
end;

{ TfpgBaseCheckBox }

procedure TfpgBaseCheckBox.SetChecked(const AValue: boolean);
begin
  if FChecked = AValue then
    Exit; //==>
  FChecked := AValue;
  RePaint;
end;

function TfpgBaseCheckBox.GetBoxLayout: TBoxLayout;
begin
  Result := FBoxLayout;
end;

function TfpgBaseCheckBox.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TfpgBaseCheckBox.SetBoxLayout(const AValue: TBoxLayout);
begin
  if FBoxLayout = AValue then
    Exit; //==>
  FBoxLayout := AValue;
  RePaint;
end;

procedure TfpgBaseCheckBox.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  RePaint;
end;

procedure TfpgBaseCheckBox.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit; //==>
  FText := AValue;
  RePaint;
end;

procedure TfpgBaseCheckBox.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TfpgBaseCheckBox.HandlePaint;
var
  r: TfpgRect;
  ty: integer;
  tx: integer;
  ix: integer;
  img: TfpgImage;
  cliprect: TfpgRect;
begin
  inherited HandlePaint;
  
  Canvas.SetColor(FBackgroundColor);
  Canvas.FillRectangle(0, 0, Width, Height);
  Canvas.SetFont(Font);
  cliprect.SetRect(1, 1, Width-2, Height-2);

  if FFocused then
  begin
    Canvas.SetColor(clText1);
    Canvas.SetLineStyle(1, lsDot);
    Canvas.DrawRectangle(cliprect);
    InflateRect(cliprect, 1, 1);
  end;
  Canvas.SetClipRect(cliprect);
  Canvas.SetLineStyle(1, lsSolid);

  if FBoxLayout = tbLeftBox then
    r.SetRect(2, (Height div 2) - (FBoxSize div 2), FBoxSize, FBoxSize)
  else
    r.SetRect(Width - FBoxSize - 2, (Height div 2) - (FBoxSize div 2), FBoxSize, FBoxSize);
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

  // calc the text offset and checkbox offset
  if FBoxLayout = tbLeftBox then
  begin
    tx := r.right + 8;
    inc(r.left, 2);
  end
  else
    tx := 3;  // leave space for focus rectangle
  inc(r.top, 1);
  // paint the check (in this case a X)
  img := fpgImages.GetImage('sys.checkboxes');    // Do NOT localize
  Canvas.DrawImagePart(r.Left, r.Top, img, ix*13, 0, 13, 13);

  ty := (Height div 2) - (Font.Height div 2);
  if ty < 0 then
    ty := 0;
  Canvas.SetTextColor(FTextColor);
  Canvas.ClearClipRect;
  cliprect.SetRect(tx, ty, Width-FBoxSize-8, cliprect.Height);
  Canvas.SetClipRect(cliprect);
  fpgStyle.DrawString(Canvas, tx, ty, FText, Enabled);
end;

procedure TfpgBaseCheckBox.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  FIsPressed := True;
  Repaint;
end;

procedure TfpgBaseCheckBox.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  FIsPressed := False;
  Checked := not FChecked;
  DoOnChange;
end;

procedure TfpgBaseCheckBox.HandleKeyRelease(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  if (keycode = keySpace) or (keycode = keyReturn) or (keycode = keyPEnter) then
  begin
    consumed := True;
    Checked := not FChecked;
    DoOnChange;
  end;

  if consumed then
    Exit; //==>

  inherited HandleKeyRelease(keycode, shiftstate, consumed);
end;

constructor TfpgBaseCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText       := 'CheckBox';
  FFont       := fpgGetFont('#Label1');
  FHeight     := FFont.Height + 4;
  FWidth      := 120;
  FTextColor  := Parent.TextColor;
  FBackgroundColor := Parent.BackgroundColor;
  FFocusable  := True;
  FBoxSize    := 14;
  FChecked    := False;
  FIsPressed  := False;
  FOnChange   := nil;
  FBoxLayout  := tbLeftBox;
end;

destructor TfpgBaseCheckBox.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

end.

