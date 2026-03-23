{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2025 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a CheckBox control. Also known as a Check Button control.
}

unit fpg_checkbox;

{$I fpg_defines.inc}

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
    FReadOnly: Boolean;
    FText: string;
    FBoxLayout: TBoxLayout;
    FBoxSize: integer;
    FImgTextSpacing: integer;
    FIsPressed: boolean;
    function    GetBoxLayout: TBoxLayout;
    procedure   SetBoxLayout(const AValue: TBoxLayout);
    procedure   SetChecked(const AValue: boolean);
    procedure   SetReadOnly(const AValue: Boolean);
    procedure   SetText(const AValue: string);
    procedure   DoOnChange;
  protected
    procedure   SetFontDesc(const AValue: string); override;
    procedure   HandleCheckChanged; virtual;
    procedure   HandlePaint; override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    property    Checked: boolean read FChecked write SetChecked default False;
    property    BoxLayout: TBoxLayout read GetBoxLayout write SetBoxLayout default tbLeftBox;
    property    ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property    Text: string read FText write SetText;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;


  TfpgCheckBox = class(TfpgBaseCheckBox)
  published
    property    Align;
    property    BackgroundColor;
    property    BoxLayout;
    property    Checked;
    property    Enabled;
    property    FontDesc;
    property    Height;
    property    Hint;
    property    Left;
    property    MaxHeight;
    property    MaxWidth;
    property    MinHeight;
    property    MinWidth;
    property    ParentShowHint;
    property    ReadOnly;
    property    ShowHint;
    property    TabOrder;
    property    Text;
    property    TextColor;
    property    Top;
    property    Width;
    property    OnChange;
    property    OnEnter;
    property    OnExit;
    property    OnMouseDown;
    property    OnMouseExit;
    property    OnMouseEnter;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnShowHint;
  end;


function CreateCheckBox(AOwner: TComponent; x, y: TfpgCoord; AText: string): TfpgCheckBox;


implementation


function CreateCheckBox(AOwner: TComponent; x, y: TfpgCoord; AText: string): TfpgCheckBox;
begin
  Result := TfpgCheckBox.Create(AOwner);
  Result.Top    := y;
  Result.Left   := x;
  Result.Text   := AText;
  Result.Width  := Result.Font.GetTextWidth(Result.Text) + 24;
end;

{ TfpgBaseCheckBox }

procedure TfpgBaseCheckBox.SetChecked(const AValue: boolean);
begin
  if ReadOnly then
    Exit; //==>
  if FChecked = AValue then
    Exit; //==>
  FChecked := AValue;
  HandleCheckChanged;
  RePaint;
  if not (csDesigning in ComponentState) then
    DoOnChange;
end;

function TfpgBaseCheckBox.GetBoxLayout: TBoxLayout;
begin
  Result := FBoxLayout;
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
  inherited SetFontDesc(AValue);
  { TODO: Implement AutoSize property, then adjust width here if True }
  RePaint;
end;

procedure TfpgBaseCheckBox.SetReadOnly(const AValue: Boolean);
begin
  if FReadOnly=AValue then exit;
  FReadOnly:=AValue;
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

procedure TfpgBaseCheckBox.HandleCheckChanged;
begin
  // nothing here for us
end;

procedure TfpgBaseCheckBox.HandlePaint;
var
  r: TfpgRect;
  cbFlags: TfpgCheckBoxFlags;
  LFlags: TfpgTextFlags;
begin
  inherited HandlePaint;
  Canvas.SetFont(Font);
  Canvas.SetLineStyle(1, lsSolid);

  if FBoxLayout = tbLeftBox then
    r.SetRect(2, ((ActualHeight - FBoxSize) div 2), FBoxSize, FBoxSize)
  else
    r.SetRect(ActualWidth - FBoxSize - 2, ((ActualHeight - FBoxSize) div 2), FBoxSize, FBoxSize);
  if r.top < 0 then
    r.top := 0;

  { Build checkbox state flags }
  cbFlags := [];
  if FChecked then
    Include(cbFlags, cbfChecked);
  if Enabled then
    Include(cbFlags, cbfEnabled);
  if FIsPressed then
    Include(cbFlags, cbfPressed);
  if FReadOnly then
    Include(cbFlags, cbfReadOnly);
  if FFocused then
    Include(cbFlags, cbfHasFocus);

  fpgStyle.DrawCheckBox(Canvas, r, cbFlags);

  r := GetClientRect;
  { max focus rectangle and text boundry }
  r.InflateRect(-1, -1);
  { exclude the checkbox image and spacing from rectangle }
  if FBoxLayout = tbLeftBox then
  begin
    r.Left  := r.Left + FBoxSize + FImgTextSpacing;
    r.Width := r.Width - FBoxSize - FImgTextSpacing;
  end
  else
    r.Width := r.Width - FBoxSize - FImgTextSpacing;

  Canvas.SetTextColor(FTextColor);
  Canvas.SetClipRect(r);

  if Enabled then
    LFlags := [txtLeft, txtVCenter]
  else
    LFlags := [txtLeft, txtVCenter, txtDisabled];
  Canvas.DrawText(r, FText, LFlags);   { internally this still calls fpgStyle.DrawString(), so theming will be applied }

  if FFocused then
  begin
    Canvas.ClearClipRect;
    { adjust focusrect-to-text margin }
    if FBoxLayout = tbLeftBox then
    begin
      r.Left := r.Left - 2;
      r.Width := r.Width + 2;
    end
    else
    begin
      r.Width := r.Width + 2;
    end;
    { undo the 2px focusrect-to-text margin, so we simply use the clip rect }
    fpgStyle.DrawFocusRect(Canvas, r);
  end;
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
end;

procedure TfpgBaseCheckBox.HandleKeyRelease(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  if (keycode = keySpace) or (keycode = keyReturn) or (keycode = keyPEnter) then
  begin
    consumed := True;
    Checked := not FChecked;
  end;

  if consumed then
    Exit; //==>

  inherited HandleKeyRelease(keycode, shiftstate, consumed);
end;

constructor TfpgBaseCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText       := 'CheckBox';
  FontDesc    := '#Label1';  // Use inherited property setter
  FHeight     := Font.GetHeight + 4;
  FWidth      := 120;
  FPreferredSize.SetSize(FWidth, FHeight);
  FTextColor  := Parent.TextColor;
  FBackgroundColor := Parent.BackgroundColor;
  FFocusable  := True;
  FBoxSize    := fpgStyle.GetCheckBoxSize;
  FImgTextSpacing := 6;
  FChecked    := False;
  FIsPressed  := False;
  FOnChange   := nil;
  FBoxLayout  := tbLeftBox;
  FReadOnly   := False;
end;

destructor TfpgBaseCheckBox.Destroy;
begin
  inherited Destroy;
end;

end.

