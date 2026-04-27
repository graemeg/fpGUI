{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2025 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a Radio Button control.
}

unit fpg_radiobutton;

{$I fpg_defines.inc}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget;

type

  TfpgRadioButton = class(TfpgWidget)
  private
    FAutoSize: boolean;
    FChecked: boolean;
    FFont: TfpgFontResourceBase;
    FGroupIndex: integer;
    FOnChange: TNotifyEvent;
    FText: string;
    FBoxLayout: TBoxLayout;
    FBoxSize: integer;
    FIsPressed: boolean;
    FImgTextSpacing: integer;
    function    GetBoxLayout: TBoxLayout;
    function    GetFontDesc: string;
    procedure   SetBoxLayout(const AValue: TBoxLayout);
    procedure   SetAutoSize(const AValue: boolean);
    procedure   SetChecked(const AValue: boolean);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetText(const AValue: string);
    procedure   DoAdjustWidth;
    function    FindNeighbour(direction: TFocusSearchDirection): TfpgRadioButton;
  protected
    procedure   HandlePaint; override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Font: TfpgFontResourceBase read FFont;
  published
    property    Align;
    property    AutoSize: boolean read FAutoSize write SetAutoSize default False;
    property    BackgroundColor;
    property    Checked: boolean read FChecked write SetChecked default False;
    property    Enabled;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    Hint;
    property    BoxLayout: TBoxLayout read GetBoxLayout write SetBoxLayout default tbLeftBox;
    property    GroupIndex: integer read FGroupIndex write FGroupIndex;
    property    ParentShowHint;
    property    ShowHint;
    property    TabOrder;
    property    Text: string read FText write SetText;
    property    TextColor;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
    property    OnDragStartDetected;
    property    OnEnter;
    property    OnExit;
    property    OnMouseDown;
    property    OnMouseExit;
    property    OnMouseEnter;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnShowHint;
  end;
  
  
function CreateRadioButton(AOwner: TComponent; x, y: TfpgCoord; AText: string): TfpgRadioButton;


implementation


function CreateRadioButton(AOwner: TComponent; x, y: TfpgCoord; AText: string): TfpgRadioButton;
begin
  Result := TfpgRadioButton.Create(AOwner);
  Result.Top    := y;
  Result.Left   := x;
  Result.Text   := AText;
  Result.Width  := Result.Font.GetTextWidth(Result.Text) + 24;
end;

{ TfpgRadioButton }

function TfpgRadioButton.GetBoxLayout: TBoxLayout;
begin
  Result := FBoxLayout;
end;

function TfpgRadioButton.GetFontDesc: string;
begin
  if Assigned(FFont) then
    Result := FFont.FontDesc
  else
    Result := '';
end;

procedure TfpgRadioButton.SetBoxLayout(const AValue: TBoxLayout);
begin
  if FBoxLayout = AValue then
    Exit; //==>
  FBoxLayout := AValue;
  RePaint;
end;

procedure TfpgRadioButton.SetAutoSize(const AValue: boolean);
begin
  if FAutoSize = AValue then
    Exit; //==>
  FAutoSize := AValue;
  if FAutoSize then
    DoAdjustWidth;
  Repaint;
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
      if (Parent.Components[i] is TfpgWidget) then
      begin
        wg := TfpgWidget(Parent.Components[i]);
        if (wg <> nil) and (wg <> self) and (wg is TfpgRadioButton) and
            (TfpgRadioButton(wg).GroupIndex = GroupIndex) then
        begin
          TfpgRadioButton(wg).FChecked := False;
        end;
      end;  { if }
  end;  { if }

  RePaint;

  if Assigned(FOnChange) then
      FOnChange(self);
end;

procedure TfpgRadioButton.SetFontDesc(const AValue: string);
begin
  FFont := nil;  // Release old font (automatic ref count decrement)
  FFont := fpgApplication.FontManager.GetFont(AValue);
  RePaint;
end;

procedure TfpgRadioButton.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit; //==>
  FText := AValue;
  if AutoSize then
    DoAdjustWidth;
  RePaint;
end;

procedure TfpgRadioButton.DoAdjustWidth;
begin
  if AutoSize then
  begin
    Width := Font.GetTextWidth(FText) + 24; // 24 is extra padding for image
    UpdatePosition;
  end;
end;

procedure TfpgRadioButton.HandlePaint;
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

  { Build radio button state flags }
  cbFlags := [];
  if FChecked then
    Include(cbFlags, cbfChecked);
  if Enabled then
    Include(cbFlags, cbfEnabled);
  if FIsPressed then
    Include(cbFlags, cbfPressed);
  if FFocused then
    Include(cbFlags, cbfHasFocus);

  fpgStyle.DrawRadioButton(Canvas, r, cbFlags);

  r := GetClientRect;
  { max focus rectangle and text boundry }
  r.InflateRect(-1, -1);
  { exclude the radiobutton image and spacing from rectangle }
  if FBoxLayout = tbLeftBox then
  begin
    r.Left  := r.Left + FBoxSize+1 + FImgTextSpacing;   { +1 or -1 is so checkbox and radiobutton text align }
    r.Width := r.Width - FBoxSize-1 - FImgTextSpacing;
  end
  else
    r.Width := r.Width - FBoxSize-1 - FImgTextSpacing;

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
  if not FChecked then
    Checked := true
  else
    RePaint;
end;

procedure TfpgRadioButton.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  nbr: TfpgRadioButton;
begin
  case keycode of
    keyUp, keyLeft:
      begin
        consumed := True;
        nbr := FindNeighbour(fsdPrev);
        if nbr = Self then
          nbr := FindNeighbour(fsdLast);
        nbr.SetFocus;
        nbr.Checked := True;
      end;
    keyDown, keyRight:
      begin
        consumed := True;
        nbr := FindNeighbour(fsdNext);
        if nbr = Self then
          nbr := FindNeighbour(fsdFirst);
        nbr.SetFocus;
        nbr.Checked := True;
      end;
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

function TfpgRadioButton.FindNeighbour(direction: TFocusSearchDirection): TfpgRadioButton;
var
  i: integer;
  wg: TfpgWidget;
  bestdtab: integer;
  FoundIt: boolean;
begin
  Result := Self;
  if (Parent <> nil) then
  begin
    FoundIt := False;
    if direction in [fsdLast, fsdPrev] then
      bestdtab := Low(integer)
    else
      bestdtab := High(integer);
    
    for i := 0 to Parent.ComponentCount-1 do
    begin
      if (Parent.Components[i] is TfpgWidget) then
      begin
        wg := TfpgWidget(Parent.Components[i]);
        if (wg <> nil) and (wg is TfpgRadioButton) and
           wg.Visible and wg.Enabled and wg.Focusable and
           (TfpgRadioButton(wg).GroupIndex = GroupIndex) then
        begin
          case direction of
            fsdFirst:
              if (wg.TabOrder < bestdtab) then
              begin
                Result   := TfpgRadioButton(wg);
                bestdtab := wg.TabOrder;
              end;

            fsdLast:
              if (wg.TabOrder >= bestdtab) then
              begin
                Result   := TfpgRadioButton(wg);
                bestdtab := wg.TabOrder;
              end;

            fsdNext:
              if wg = Self then
                FoundIt := True
              else
              begin
                if ((wg.TabOrder > Self.TabOrder) and (wg.TabOrder < bestdtab)) or
                   ((wg.TabOrder = Self.TabOrder) and FoundIt) then
                begin
                  Result   := TfpgRadioButton(wg);
                  bestdtab := wg.TabOrder;
                end;
              end;

            fsdPrev:
              if wg = Self then
                FoundIt := True
              else
              begin
                if ((wg.TabOrder < Self.TabOrder) and (wg.TabOrder >= bestdtab)) or
                   ((wg.TabOrder = Self.TabOrder) and not FoundIt) then
                begin
                  Result   := TfpgRadioButton(wg);
                  bestdtab := wg.TabOrder;
                end;
              end;
          end; { case }
        end; { if }
      end; { if is TfpgWidget }
    end;  { for ComponentCount }
  end;  { if }
end;

constructor TfpgRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText       := 'RadioButton';
  FFont       := fpgApplication.FontManager.GetFont('#Label1');
  FHeight     := FFont.GetHeight + 4;
  FWidth      := 120;
  FPreferredSize.SetSize(FWidth, FHeight);
  FTextColor  := Parent.TextColor;
  FBackgroundColor := Parent.BackgroundColor;
  FFocusable  := True;
  FBoxSize    := fpgStyle.GetRadioButtonSize;
  FChecked    := False;
  FGroupIndex := 0;
  FIsPressed  := False;
  FAutoSize   := False;
  FOnChange   := nil;
  FBoxLayout  := tbLeftBox;
  FImgTextSpacing := 6;
end;

destructor TfpgRadioButton.Destroy;
begin
  FFont := nil;
  inherited Destroy;
end;

end.

