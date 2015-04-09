{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2015 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Original author: Andrew Haines

    Description:
      Defines a ToggleBox control. A Checkbox like control that has an
      animated bar that slides side to side when toggled.
}
unit fpg_toggle;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_stylemanager,
  fpg_checkbox;

type

  TfpgToggle = class(TfpgCheckBox)
  private
    FCheckedTextColor: TfpgColor;
    FToggleWidth: TfpgCoord;
    FToggleButtonWidth: TfpgCoord;
    FAnimateTimer: TfpgTimer;
    FCheckedCaption: TfpgString;
    FCheckedColor: TfpgColor;
    FSliderPosition: TfpgCoord;
    FPaintedSliderPosition: TfpgCoord;
    FUnCheckedCaption: TfpgString;
    FUnCheckedColor: TfpgColor;
    FUnCheckedTextColor: TfpgColor;
    FUseAnimation: Boolean;
    procedure   SetCheckedCaption(AValue: TfpgString);
    procedure   SetCheckedColor(AValue: TfpgColor);
    procedure   SetCheckedTextColor(AValue: TfpgColor);
    procedure   SetToggleWidth(AValue: TfpgCoord);
    procedure   SetUnCheckedCaption(AValue: TfpgString);
    procedure   SetUnCheckedColor(AValue: TfpgColor);
    procedure   AnimateTimer(Sender: TObject);
    procedure   SetUnCheckedTextColor(AValue: TfpgColor);
    function    ToggleLeft: TfpgCoord; inline;
  protected
    procedure   HandlePaint; override;
    procedure   HandleCheckChanged; override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property    UseAnimation: Boolean read FUseAnimation write FUseAnimation;
    property    ToggleWidth: TfpgCoord read FToggleWidth write SetToggleWidth default 45;
    property    CheckedCaption : TfpgString read FCheckedCaption write SetCheckedCaption;
    property    CheckedColor: TfpgColor read FCheckedColor write SetCheckedColor default clLime;
    property    CheckedTextColor: TfpgColor read FCheckedTextColor write SetCheckedTextColor default clHilite2;
    property    UnCheckedCaption: TfpgString read FUnCheckedCaption write SetUnCheckedCaption;
    property    UnCheckedColor: TfpgColor read FUnCheckedColor write SetUnCheckedColor default clWindowBackground;
    property    UnCheckedTextColor: TfpgColor read FUnCheckedTextColor write SetUnCheckedTextColor default clText1;
  end;

implementation

{ TfpgToggle }

procedure TfpgToggle.SetCheckedColor(AValue: TfpgColor);
begin
  if FCheckedColor=AValue then Exit;
  FCheckedColor:=AValue;
  Invalidate;
end;

procedure TfpgToggle.SetCheckedTextColor(AValue: TfpgColor);
begin
  if FCheckedTextColor=AValue then Exit;
  FCheckedTextColor:=AValue;
  Invalidate;
end;

procedure TfpgToggle.SetToggleWidth(AValue: TfpgCoord);
begin
  if FToggleWidth=AValue then Exit;
  FToggleWidth:=AValue;
  FToggleButtonWidth:=AValue - 10;
  Invalidate;
end;

procedure TfpgToggle.SetCheckedCaption(AValue: TfpgString);
begin
  if FCheckedCaption=AValue then Exit;
  FCheckedCaption:=AValue;
  Invalidate;
end;

procedure TfpgToggle.SetUnCheckedCaption(AValue: TfpgString);
begin
  if FUnCheckedCaption=AValue then Exit;
  FUnCheckedCaption:=AValue;
  Invalidate;
end;

procedure TfpgToggle.SetUnCheckedColor(AValue: TfpgColor);
begin
  if FUnCheckedColor=AValue then Exit;
  FUnCheckedColor:=AValue;
  Invalidate;
end;

procedure TfpgToggle.AnimateTimer(Sender: TObject);
begin
  if csDestroying in ComponentState then
    Exit;
  if not Checked then
  begin // not checked
    Dec(FSliderPosition, 1);
    if FSliderPosition < 1 then
      FSliderPosition:=0;
  end
  else  // checked
  begin
    Inc(FSliderPosition);
    if FSliderPosition >= FToggleWidth - FToggleButtonWidth -2then
      FSliderPosition := FToggleWidth - FToggleButtonWidth -2;
  end;
  Invalidate;
end;

procedure TfpgToggle.SetUnCheckedTextColor(AValue: TfpgColor);
begin
  if FUnCheckedTextColor=AValue then Exit;
  FUnCheckedTextColor:=AValue;
  Invalidate;
end;

function TfpgToggle.ToggleLeft: TfpgCoord;
begin
  if BoxLayout = tbLeftBox then
    Result := 1
  else
    Result := Width - FToggleWidth;
end;

procedure TfpgToggle.HandlePaint;
var
  ToggleText: TfpgString;
  TextEnabled: TfpgTextFlags;
  BvlWdth: TfpgCoord;
  ButtonRect: TfpgRect;
begin
  Canvas.Clear(BackgroundColor);

  // Text
  Canvas.SetFont(Font);
  if Enabled then
    TextEnabled := []
  else
    TextEnabled := [txtDisabled];

  BvlWdth := fpgStyleManager.Style.GetBevelWidth;

  if BoxLayout = tbRightBox then
    Canvas.DrawText(fpgRect(0,0,FWidth-FToggleWidth, FHeight), Text, [txtLeft, txtVCenter] + TextEnabled)   { internally this still calls fpgStyle.DrawString(), so theming will be applied }
  else
    Canvas.DrawText(fpgRect(ToggleWidth,0,FWidth-ToggleWidth, FHeight), Text, [txtRight, txtVCenter] + TextEnabled);   { internally this still calls fpgStyle.DrawString(), so theming will be applied }

  // Toggle Stuff

  // Toggle area bevel
  fpgStyleManager.Style.DrawBevel(Canvas,ToggleLeft,0,FToggleWidth, Height, False);

  // Toggle Button
  ButtonRect := fpgRect(ToggleLeft+FSliderPosition+BvlWdth,BvlWdth,FToggleButtonWidth, Height -(BvlWdth*2));
  fpgStyleManager.Style.DrawBevel(Canvas,ButtonRect.Left, ButtonRect.Top, ButtonRect.Width, ButtonRect.Height, True);


  // unchecked text
  if FSliderPosition < (FToggleWidth - FToggleButtonWidth) div 2 then
  begin
    ToggleText := FUnCheckedCaption;
    Canvas.SetTextColor(FUnCheckedTextColor);
  end
  // checked text
  else
  begin
    ToggleText := FCheckedCaption;
    Canvas.SetTextColor(FCheckedTextColor);
  end;

  // Toggle Text (inside 2 bevels)
  Canvas.DrawText(fpgRect(ToggleLeft+FSliderPosition+BvlWdth*2,BvlWdth*2,FToggleButtonWidth-BvlWdth*4, Height-BvlWdth*4),ToggleText, [txtVCenter, txtHCenter] + TextEnabled);

  // Paint on either side of the button part of the toggle
  if FSliderPosition > 0 then
  begin
    Canvas.SetColor(CheckedColor);
    Canvas.FillRectangle(fpgRect(ToggleLeft+1,1, FSliderPosition, FHeight - BvlWdth*2));
  end;

  if FSliderPosition < FToggleWidth - FToggleButtonWidth -2 then
  begin
    Canvas.SetColor(UnCheckedColor);
    Canvas.FillRectangle(fpgRect(ToggleLeft + FSliderPosition + FToggleButtonWidth+BvlWdth, BvlWdth, FToggleWidth - FToggleButtonWidth - FSliderPosition -(BvlWdth*2), FHeight - BvlWdth*2));
  end;

  // lastly draw focus
  if FFocusable and FFocused then
  begin
    InflateRect(ButtonRect, -1,-1);
    fpgStyleManager.Style.DrawFocusRect(Canvas, ButtonRect);
  end;


  if FPaintedSliderPosition = FSliderPosition then
    FAnimateTimer.Enabled:=False;

  FPaintedSliderPosition := FSliderPosition;
end;

procedure TfpgToggle.HandleCheckChanged;
begin
  if FUseAnimation then
    FAnimateTimer.Enabled := True
  else
  begin
    if Checked then
       FSliderPosition := FToggleWidth - FToggleButtonWidth -2
    else
       FSliderPosition := 0;
  end;
  FPaintedSliderPosition := -1;
end;

procedure TfpgToggle.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  if ((BoxLayout = tbRightBox) and (x > Width - FToggleWidth))
  or ((BoxLayout = tbLeftBox)  and (x <= FToggleWidth))
  then
    inherited HandleLMouseUp(x, y, shiftstate);
end;

constructor TfpgToggle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text := 'ToggleBox';
  ToggleWidth         := 45;
  BoxLayout           := tbRightBox;
  FUseAnimation       := True;
  FUnCheckedCaption   := 'OFF';
  FCheckedCaption     := 'ON';
  FUnCheckedColor     := FBackgroundColor;
  FCheckedColor       := clLime;
  FUnCheckedTextColor := clText1;
  FCheckedTextColor   := clHilite2;
  FAnimateTimer       := TfpgTimer.Create(12);
  FAnimateTimer.Enabled := False;
  FAnimateTimer.OnTimer := @AnimateTimer;
end;

destructor TfpgToggle.Destroy;
begin
  FAnimateTimer.Free;
  inherited Destroy;
end;

end.

