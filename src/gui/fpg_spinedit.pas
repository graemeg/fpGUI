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
      Defines a spinedit control.
}

unit fpg_spinedit;

{$mode objfpc}{$H+}

{
    *****************************************************************
    **********   This is still under heavy development!   ***********
    *****************************************************************
}

{ TODO : Base classes need to be abstracted from final classes. }
{ TODO : Up/Down keyboard input needs to be corrected. }
{ TODO : Step size needs to be implemented (small and large) }
{ TODO : PgUp/PgDn keyboard needs to be supported. }
{ TODO : Improve Timer and Step support. If the mouse is kept down on
         a button, it should increment by small steps. After a certain
         period, it should start incrementing by large steps. }
{ TODO : Text cursor positioning should be fixed. }

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_panel,
  fpg_edit,
  fpg_button;

type


  TfpgAbstractSpinEdit = class(TfpgBevel)
  private
    FButtonUp: TfpgButton;
    FButtonDown: TfpgButton;
    FArrowUpColor: Tfpgcolor;
    FArrowDownColor: Tfpgcolor;
    FTimer: TfpgTimer;
    FUp: Boolean;
    FDown: Boolean;
    FSteps: integer;
    FSpeedUpSteps: integer;
    procedure   SetButtonWidth(const AValue: integer);
  protected
    FButtonWidth: integer;
    procedure   ResizeChildren; virtual;
    procedure   HandlePaint; override;
    procedure   HandleResize(AWidth, AHeight: TfpgCoord); override;
    function    IsMinLimitReached: Boolean; virtual; abstract;
    function    IsMaxLimitReached: Boolean; virtual; abstract;
    function    GetButtonsBackgroundColor: TfpgColor;
    procedure   SetButtonsBackgroundColor(const AValue: Tfpgcolor);
    procedure   SetArrowUpColor(const AValue: Tfpgcolor);
    procedure   SetArrowDownColor(const AValue: Tfpgcolor);
    procedure   SetSpeedUp(const AValue: integer);
    procedure   ButtonUpPaint(Sender: TObject);
    procedure   ButtonDownPaint(Sender: TObject);
    property    ButtonsBackgroundColor: Tfpgcolor read GetButtonsBackgroundColor write SetButtonsBackgroundColor default clButtonFace;
    property    ArrowUpColor: TfpgColor read FArrowUpColor write SetArrowUpColor;
    property    ArrowDownColor: TfpgColor read FArrowDownColor write SetArrowDownColor;
    property    ButtonWidth: integer read FButtonWidth write SetButtonWidth default 13;
    property    StepsSpeedUp: integer read FSpeedUpSteps write SetSpeedUp;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TfpgSpinEditFloat = class(TfpgAbstractSpinEdit)
  private
    FEdit: TfpgEditFloat;
    FLargeIncrement: extended;
    FMaxValue: extended;
    FMinValue: extended;
    FIncrement: extended;
    FTempIncrement: extended;   // value varies depending on increment speed
    FValue: extended;
    procedure EnableButtons;
  protected
    function IsMinLimitReached: Boolean; override;
    function IsMaxLimitReached: Boolean; override;
    function GetEditBackgroundColor: TfpgColor;
    function GetTextColor: TfpgColor;
    function GetNegativeColor: TfpgColor;
    function GetFontDesc: string;
    function GetDecimals: integer;
    function GetFixedDecimals: Boolean;
    procedure ResizeChildren; override;
    procedure SetEditBackgroundColor(const AValue: Tfpgcolor);
    procedure SetTextColor(const AValue: Tfpgcolor);
    procedure SetNegativeColor(const AValue: Tfpgcolor);
    procedure SetFontDesc(const AValue: string);
    procedure SetMaxValue(const AValue: extended);
    procedure SetMinValue(const AValue: extended);
    procedure SetIncrement(const AValue: extended);
    procedure SetValue(const AValue: extended);
    procedure SetDecimals(const AValue: integer);
    procedure SetFixedDecimals(const AValue: Boolean);
    procedure ButtonUpClick(Sender: TObject);
    procedure ButtonDownClick(Sender: TObject);
    procedure ButtonUpMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonUpMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonDownMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonDownMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure EditKeyPress(Sender: TObject; var keycode: word; var shiftstate: TShiftState; var consumed: Boolean);
    procedure EditExit(Sender: TObject);
    procedure TimerStep(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property EditBackgroundColor: Tfpgcolor read GetEditBackgroundColor write SetEditBackgroundColor default clBoxColor;
    property ButtonsBackgroundColor;
    property TextColor: Tfpgcolor read GetTextColor write SetTextColor;
    property NegativeColor: TfpgColor read GetNegativeColor write SetNegativeColor;
    property ArrowUpColor;
    property ArrowDownColor;
    property FontDesc: string read GetFontDesc write SetFontDesc;
    property MaxValue: extended read FMaxValue write SetMaxValue;
    property MinValue: extended read FMinValue write SetMinValue;
    property Increment: extended read FIncrement write SetIncrement;
    property LargeIncrement: extended read FLargeIncrement write FLargeIncrement;
    property Value: extended read FValue write SetValue;
    property Decimals: integer read GetDecimals write SetDecimals;
    property FixedDecimals: Boolean read GetFixedDecimals write SetFixedDecimals;
  end;


  TfpgSpinEdit = class(TfpgAbstractSpinEdit)
  private
    FEdit: TfpgEditInteger;
    FLargeIncrement: integer;
    FMaxValue: integer;
    FMinValue: integer;
    FIncrement: integer;
    FTempIncrement: integer;  // value varies depending on increment speed
    FValue: integer;
    procedure EnableButtons;
  protected
    function IsMinLimitReached: Boolean; override;
    function IsMaxLimitReached: Boolean; override;
    function GetEditBackgroundColor: TfpgColor;
    function GetTextColor: TfpgColor;
    function GetNegativeColor: TfpgColor;
    function GetFontDesc: string;
    procedure ResizeChildren; override;
    procedure SetEditBackgroundColor(const AValue: Tfpgcolor);
    procedure SetTextColor(const AValue: Tfpgcolor);
    procedure SetNegativeColor(const AValue: Tfpgcolor);
    procedure SetFontDesc(const AValue: string);
    procedure SetMaxValue(const AValue: integer);
    procedure SetMinValue(const AValue: integer);
    procedure SetIncrement(const AValue: integer);
    procedure SetValue(const AValue: integer);
    procedure ButtonUpClick(Sender: TObject);
    procedure ButtonDownClick(Sender: TObject);
    procedure ButtonUpMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonUpMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonDownMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonDownMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure EditKeyPress(Sender: TObject; var keycode: word; var shiftstate: TShiftState; var consumed: Boolean);
    procedure EditExit(Sender: TObject);
    procedure TimerStep(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property EditBackgroundColor: Tfpgcolor read GetEditBackgroundColor write SetEditBackgroundColor default clBoxColor;
    property ButtonsBackgroundColor;
    property ButtonWidth;
    property TextColor: Tfpgcolor read GetTextColor write SetTextColor;
    property NegativeColor: TfpgColor read GetNegativeColor write SetNegativeColor;
    property ArrowUpColor;
    property ArrowDownColor;
    property FontDesc: string read GetFontDesc write SetFontDesc;
    property MaxValue: integer read FMaxValue write SetMaxValue default 100;
    property MinValue: integer read FMinValue write SetMinValue default 0;
    property Increment: integer read FIncrement write SetIncrement default 1;
    property LargeIncrement: integer read FLargeIncrement write FLargeIncrement default 10;
    property Value: integer read FValue write SetValue default 0;
  end;


function CreateSpinEditFloat(AOwner: TComponent; x, y, w, h: TfpgCoord;
         AMinValue: extended = 0; AMaxValue: extended = 100; AIncrement: extended = 1; ADecimals: integer = 1;
         AValue: extended = 0): TfpgSpinEditFloat;
function CreateSpinEdit(AOwner: TComponent; x, y, w, h: TfpgCoord; AMinValue: integer = 0;
         AMaxValue: integer = 100; AIncrement: integer = 1; AValue: integer = 0): TfpgSpinEdit;


implementation

uses
  fpg_extgraphics, math;


function CreateSpinEditFloat(AOwner: TComponent; x, y, w, h: TfpgCoord;
         AMinValue: extended = 0; AMaxValue: extended = 100; AIncrement: extended = 1; ADecimals: integer = 1;
         AValue: extended = 0): TfpgSpinEditFloat;
var
  newh: TfpgCoord;
begin
  Result       := TfpgSpinEditFloat.Create(AOwner);
  if h < Result.FEdit.Font.Height + 6 then
    newh := Result.FEdit.Font.Height + 6
  else
    newh := h;
  Result.SetPosition(x, y, w, newh);

  if AMaxValue > AMinValue then
  begin
    Result.MinValue := AMinValue;
    Result.MaxValue := AMaxValue;
  end;
  Result.Increment := AIncrement;
  Result.FEdit.Decimals := ADecimals;
  if (AValue <= Result.MaxValue) and (AValue >= Result.MinValue) then
    Result.Value := AValue;
end;

function CreateSpinEdit(AOwner: TComponent; x, y, w, h: TfpgCoord; AMinValue: integer = 0;
         AMaxValue: integer = 100; AIncrement: integer = 1; AValue: integer = 0): TfpgSpinEdit;
var
  newh: TfpgCoord;
begin
  Result       := TfpgSpinEdit.Create(AOwner);
  if h < Result.FEdit.Font.Height + 6 then
    newh := Result.FEdit.Font.Height + 6
  else
    newh := h;
  Result.SetPosition(x, y, w, newh);

  if AMaxValue > AMinValue then
  begin
    Result.MinValue := AMinValue;
    Result.MaxValue := AMaxValue;
  end;
  Result.Increment := AIncrement;
  if (AValue <= Result.MaxValue) and (AValue >= Result.MinValue) then
    Result.Value := AValue;
end;


{ TfpgAbstractSpinEdit }

procedure TfpgAbstractSpinEdit.SetButtonWidth(const AValue: integer);
begin
  if FButtonWidth = AValue then
    Exit;
  FButtonWidth := AValue;
  { Apply some limits for sanity sake }
  if FButtonWidth < 5 then
    FButtonWidth := 5;

  ResizeChildren;
  RePaint;
end;

procedure TfpgAbstractSpinEdit.ResizeChildren;
begin
  FButtonUp.SetPosition(Width - FButtonWidth, 0, FButtonWidth, Height div 2);
  FButtonDown.SetPosition(Width - FButtonWidth, Height div 2, FButtonWidth, Height div 2);
end;

procedure TfpgAbstractSpinEdit.HandlePaint;
begin
    Canvas.Clear(BackgroundColor);
    if FButtonUp.HasHandle then
      fpgPostMessage(self, FButtonUp, FPGM_PAINT);
//    FButtonDown.Invalidate;
end;

procedure TfpgAbstractSpinEdit.HandleResize(AWidth, AHeight: TfpgCoord);
begin
  inherited HandleResize(AWidth, AHeight);
  ResizeChildren;
end;

function TfpgAbstractSpinEdit.GetButtonsBackgroundColor: TfpgColor;
begin
  Result := FButtonUp.BackgroundColor;
end;

procedure TfpgAbstractSpinEdit.SetButtonsBackgroundColor(const AValue: Tfpgcolor);
begin
  if FButtonUp.BackgroundColor <> AValue then
  begin
    FButtonUp.BackgroundColor   := AValue;
    FButtonDown.BackgroundColor := AValue;
  end;
end;

procedure TfpgAbstractSpinEdit.SetArrowUpColor(const AValue: Tfpgcolor);
begin
  if FArrowUpColor <> AValue then
    FArrowUpColor := AValue;
end;

procedure TfpgAbstractSpinEdit.SetArrowDownColor(const AValue: Tfpgcolor);
begin
  if FArrowDownColor <> AValue then
    FArrowDownColor := AValue;
end;

procedure TfpgAbstractSpinEdit.SetSpeedUp(const AValue: integer);
begin
  if FSpeedUpSteps <> AValue then
    FSpeedUpSteps := AValue;
end;

function GetButtonRect(AButton: TfpgButton): TRect;
var
  r: TfpgRect;
begin
  r := AButton.GetClientRect;

  InflateRect(r, -1, -1); // button borders
  if AButton.Down then
    OffsetRect(r, 1, 1);

  // TfpgRect to TRect
  Result.Left := r.Left;
  Result.Top := r.Top;
  Result.Right := r.Right;
  Result.Bottom := r.Bottom;
end;

procedure TfpgAbstractSpinEdit.ButtonUpPaint(Sender: TObject);
var
  btn: TfpgButton;
  r: TRect;
begin
  btn := TfpgButton(Sender);
  if btn.Enabled then
    btn.Canvas.SetColor(FArrowUpColor)
  else
    btn.Canvas.SetColor(clShadow1);

  r := GetButtonRect(btn);
  PaintTriangle(btn.Canvas, r, degtorad(90.0));
//  fpgStyle.DrawDirectionArrow(btn.Canvas, r.Top, r.Left, r.Width, r.Height, adUp);
end;

procedure TfpgAbstractSpinEdit.ButtonDownPaint(Sender: TObject);
var
  btn: TfpgButton;
  r: TRect;
begin
  btn := TfpgButton(Sender);
  if btn.Enabled {and (not IsMinLimitReached)} then
    btn.Canvas.SetColor(FArrowDownColor)
  else
    btn.Canvas.SetColor(clShadow1);

  r := GetButtonRect(btn);
  PaintTriangle(btn.Canvas, r, degtorad(270.0));
//  fpgStyle.DrawDirectionArrow(TfpgButton(Sender).Canvas, 0, 0, TfpgButton(Sender).Width - 3, TfpgButton(Sender).Height, adDown);
end;

constructor TfpgAbstractSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonWidth := 13; // width of spin buttons
  Shape := bsSpacer;

  FButtonUp := TfpgButton.Create(Self);
  with FButtonUp do
  begin
    SetPosition(Width - FButtonWidth, 0, FButtonWidth, Height div 2);
    Text      := '';
    BackgroundColor := clButtonFace;
    Focusable := False;
    OnPaint   := @ButtonUpPaint;
  end;
  FArrowUpColor := clText1;
  FButtonDown := TfpgButton.Create(Self);
  with FButtonDown do
  begin
    SetPosition(Width - FButtonWidth, Height div 2, FButtonWidth, Height div 2);
    Text      := '';
    BackgroundColor := clButtonFace;
    Focusable := False;
    OnPaint   := @ButtonDownPaint;
  end;
  FArrowDownColor := clText1;
  FTimer         := TfpgTimer.Create(200);
  FTimer.Enabled := False;
  FSpeedUpSteps  := 10;
end;


{ TfpgSpinEditFloat }

procedure TfpgSpinEditFloat.EnableButtons;
begin
  if FValue + FIncrement < FMaxValue then
    FButtonUp.Enabled := True
  else
  begin
    FUp := False;
    if Assigned(FTimer) then
      FTimer.Enabled := False;
  end;
  if FValue - FIncrement > FMinValue then
    FButtonDown.Enabled := True
  else
  begin
    FDown := False;
    if Assigned(FTimer) then
      FTimer.Enabled := False;
  end;
end;

function TfpgSpinEditFloat.IsMinLimitReached: Boolean;
begin
  Result := Value = MinValue;
end;

function TfpgSpinEditFloat.IsMaxLimitReached: Boolean;
begin
  Result := Value = MaxValue;
end;

function TfpgSpinEditFloat.GetEditBackgroundColor: TfpgColor;
begin
  Result := FEdit.BackgroundColor;
end;

function TfpgSpinEditFloat.GetTextColor: TfpgColor;
begin
  Result := FEdit.TextColor;
end;

function TfpgSpinEditFloat.GetNegativeColor: TfpgColor;
begin
  Result := FEdit.NegativeColor;
end;

function TfpgSpinEditFloat.GetFontDesc: string;
begin
  Result := FEdit.FontDesc;
end;

function TfpgSpinEditFloat.GetDecimals: integer;
begin
  Result := FEdit.Decimals;
end;

function TfpgSpinEditFloat.GetFixedDecimals: Boolean;
begin
  Result := FEdit.FixedDecimals;
end;

procedure TfpgSpinEditFloat.ResizeChildren;
begin
  FEdit.SetPosition(0, 0, Width - FButtonWidth, Height);
  inherited ResizeChildren;
end;

procedure TfpgSpinEditFloat.SetEditBackgroundColor(const AValue: Tfpgcolor);
begin
  if FEdit.BackgroundColor <> AValue then
    FEdit.BackgroundColor := AValue;
end;

procedure TfpgSpinEditFloat.SetTextColor(const AValue: Tfpgcolor);
begin
  if FEdit.OldColor <> AValue then
    FEdit.OldColor := AValue;
end;

procedure TfpgSpinEditFloat.SetNegativeColor(const AValue: Tfpgcolor);
begin
  if FEdit.NegativeColor <> AValue then
    FEdit.NegativeColor := AValue;
end;

procedure TfpgSpinEditFloat.SetFontDesc(const AValue: string);
begin
  if FEdit.FontDesc <> AValue then
  begin
    FEdit.FontDesc := AValue;
    if Height < FEdit.Height then
    begin
      Height           := FEdit.Height;
      FButtonUp.Height := Height div 2;
      FButtonDown.Height := Height div 2;
      FButtonDown.Top  := FButtonUp.Height + 1;
    end;
  end;
end;

procedure TfpgSpinEditFloat.SetMaxValue(const AValue: extended);
begin
  if (FMaxValue <> AValue) and (AValue > FMinValue) then
  begin
    FMaxValue := AValue;
    if FValue > FMaxValue then
    begin
      FValue      := FMaxValue;
      FEdit.Value := FValue;
    end;
    EnableButtons;
  end;
end;

procedure TfpgSpinEditFloat.SetMinValue(const AValue: extended);
begin
  if (FMinValue <> AValue) and (AValue < FMinValue) then
  begin
    FMinValue := AValue;
    if FValue < FMinValue then
    begin
      FValue      := FMinValue;
      FEdit.Value := FValue;
    end;
    EnableButtons;
  end;
end;

procedure TfpgSpinEditFloat.SetIncrement(const AValue: extended);
begin
  if FIncrement <> AValue then
    FIncrement := AValue;
end;

procedure TfpgSpinEditFloat.SetValue(const AValue: extended);
begin
  if (FValue <> AValue) and (AValue <= FMaxValue) and (AValue >= FMinValue) then
  begin
    FValue      := AValue;
    FEdit.Value := FValue;
    EnableButtons;
  end;
end;

procedure TfpgSpinEditFloat.SetDecimals(const AValue: integer);
begin
  if AValue < 0 then
    Exit; // =>
  if FEdit.Decimals <> AValue then
    FEdit.Decimals := AValue;
end;

procedure TfpgSpinEditFloat.SetFixedDecimals(const AValue: Boolean);
begin
  if FEdit.FixedDecimals <> AValue then
    FEdit.FixedDecimals := AValue;
end;

procedure TfpgSpinEditFloat.ButtonUpClick(Sender: TObject);
begin
  if FValue + FIncrement <= FMaxValue then
    Value := FValue + FIncrement;
end;

procedure TfpgSpinEditFloat.ButtonDownClick(Sender: TObject);
begin
  if FValue - FIncrement >= FMinValue then
    Value := FValue - FIncrement;
end;

procedure TfpgSpinEditFloat.ButtonUpMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  FUp := True;
  FTimer.Enabled := True;
  FSteps := 0;
  FTempIncrement := Increment;
end;

procedure TfpgSpinEditFloat.ButtonUpMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  FUp := False;
  if Assigned(FTimer) then
    FTimer.Enabled := False;
end;

procedure TfpgSpinEditFloat.ButtonDownMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  FDown          := True;
  FTimer.Enabled := True;
  FSteps := 0;
  FTempIncrement := Increment;
end;

procedure TfpgSpinEditFloat.ButtonDownMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  FDown := False;
  if Assigned(FTimer) then
    FTimer.Enabled := False;
end;

procedure TfpgSpinEditFloat.EditKeyPress(Sender: TObject; var keycode: word; var shiftstate: TShiftState; var consumed: Boolean);
begin
  if (KeyCode = KeyReturn) or (KeyCode = KeyPEnter) then
    if FEdit.Text = '' then
    begin
      FValue      := 0.0;
      FEdit.Value := FValue;
    end
    else if (StrToFloat(FEdit.Text) <= FMaxValue) and (StrToFloat(FEdit.Text) >= FMinValue) then
      FValue      := StrToFloat(FEdit.Text)
    else
      FEdit.Value := FValue;

  if KeyCode = KeyUp then
    if FEdit.Value + Increment <= FMaxValue then
    begin
      FValue      := FValue + FIncrement;
      FEdit.Value := FValue;
    end;

  if KeyCode = KeyDown then
    if FEdit.Value - Increment >= FMinValue then
    begin
      FValue      := FValue - FIncrement;
      FEdit.Value := FValue;
    end;

  if KeyCode = KeyPageUp then
  begin
    FValue      := FMaxValue;
    FEdit.Value := FValue;
  end;

  if KeyCode = KeyPageDown then
  begin
    FValue      := FMinValue;
    FEdit.Value := FValue;
  end;

  EnableButtons;
end;

procedure TfpgSpinEditFloat.EditExit(Sender: TObject);
begin
  if FEdit.Text = '' then
  begin
    FValue      := 0.0;
    FEdit.Value := FValue;
  end
  else if (StrToFloat(FEdit.Text) <= FMaxValue) and (StrToFloat(FEdit.Text) >= FMinValue) then
    FValue      := StrToFloat(FEdit.Text)
  else
    FEdit.Value := FValue;
  EnableButtons;
end;

procedure TfpgSpinEditFloat.TimerStep(Sender: TObject);
begin
  if FUp then
  begin
    if FValue + FTempIncrement <= FMaxValue then
    begin
      Value       := FValue + FTempIncrement;
      FEdit.Value := FValue;
      if FSteps <= FSpeedUpSteps then
        Inc(FSteps);
      if FSteps > FSpeedUpSteps then
        FTempIncrement := LargeIncrement;
    end;
  end
  else if FDown then
  begin
    if FValue - FTempIncrement >= FMinValue then
    begin
      FValue      := FValue - FTempIncrement;
      FEdit.Value := FValue;
      if FSteps <= FSpeedUpSteps then
        Inc(FSteps);
      if FSteps > FSpeedUpSteps then
        FTempIncrement := LargeIncrement;
    end;
  end;
  EnableButtons;
end;

constructor TfpgSpinEditFloat.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEdit := CreateEditFloat(Self, 0, 0, Width - FButtonWidth, Height, False, 2);

  FMaxValue       := 100.0;
  FMinValue       := 0.0;
  FIncrement      := 1.0;
  FLargeIncrement := 10.0;
  FValue          := 0.0;
  FUp             := False;
  FDown           := False;

  FEdit.Decimals := 1;
  FEdit.Value    := FValue;

  FButtonUp.OnClick := @ButtonUpClick;
  FButtonDown.OnClick := @ButtonDownClick;
  FButtonUp.OnMouseDown := @ButtonUpMouseDown;
  FButtonUp.OnMouseUp := @ButtonUpMouseUp;
  FButtonDown.OnMouseDown := @ButtonDownMouseDown;
  FButtonDown.OnMouseUp := @ButtonDownMouseUp;
  FEdit.OnKeyPress := @EditKeyPress;
  FEdit.OnExit   := @EditExit;
  FTimer.OnTimer := @TimerStep;
  EnableButtons;
end;


{ TfpgSpinEdit }

procedure TfpgSpinEdit.EnableButtons;
begin
  if not IsMaxLimitReached then
    FButtonUp.Enabled := True
  else
  begin
    FButtonUp.Enabled := False;
    FUp := False;
    if Assigned(FTimer) then
      FTimer.Enabled := False;
  end;
  if not IsMinLimitReached then
    FButtonDown.Enabled := True
  else
  begin
    FButtonDown.Enabled := False;
    FDown := False;
    if Assigned(FTimer) then
      FTimer.Enabled := False;
  end;
end;

function TfpgSpinEdit.IsMinLimitReached: Boolean;
begin
  Result := Value = MinValue;
end;

function TfpgSpinEdit.IsMaxLimitReached: Boolean;
begin
  Result:= Value = MaxValue;
end;

function TfpgSpinEdit.GetEditBackgroundColor: TfpgColor;
begin
  Result := FEdit.BackgroundColor;
end;

function TfpgSpinEdit.GetTextColor: TfpgColor;
begin
  Result := FEdit.TextColor;
end;

function TfpgSpinEdit.GetNegativeColor: TfpgColor;
begin
  Result := FEdit.NegativeColor;
end;

function TfpgSpinEdit.GetFontDesc: string;
begin
  Result := FEdit.FontDesc;
end;

procedure TfpgSpinEdit.ResizeChildren;
begin
  FEdit.SetPosition(0, 0, Width - FButtonWidth, Height);
  inherited ResizeChildren;
end;

procedure TfpgSpinEdit.SetEditBackgroundColor(const AValue: Tfpgcolor);
begin
  if FEdit.BackgroundColor <> AValue then
    FEdit.BackgroundColor := AValue;
end;

procedure TfpgSpinEdit.SetTextColor(const AValue: Tfpgcolor);
begin
  if FEdit.OldColor <> AValue then
    FEdit.OldColor := AValue;
end;

procedure TfpgSpinEdit.SetNegativeColor(const AValue: Tfpgcolor);
begin
  if FEdit.NegativeColor <> AValue then
    FEdit.NegativeColor := AValue;
end;

procedure TfpgSpinEdit.SetFontDesc(const AValue: string);
begin
  if FEdit.FontDesc <> AValue then
  begin
    FEdit.FontDesc := AValue;
    if Height < FEdit.Height then
    begin
      Height           := FEdit.Height;
      FButtonUp.Height := Height div 2;
      FButtonDown.Height := Height div 2;
      FButtonDown.Top  := FButtonUp.Height + 1;
    end;
  end;
end;

procedure TfpgSpinEdit.SetMaxValue(const AValue: integer);
begin
  if (FMaxValue <> AValue) and (AValue > FMinValue) then
  begin
    FMaxValue := AValue;
    if FValue > FMaxValue then
    begin
      FValue      := FMaxValue;
      FEdit.Value := FValue;
    end;
    EnableButtons;
  end;
end;

procedure TfpgSpinEdit.SetMinValue(const AValue: integer);
begin
  if (FMinValue <> AValue) and (AValue < FMaxValue) then
  begin
    FMinValue := AValue;
    if FValue < FMinValue then
    begin
      FValue      := FMinValue;
      FEdit.Value := FValue;
    end;
    EnableButtons;
  end;
end;

procedure TfpgSpinEdit.SetIncrement(const AValue: integer);
begin
  if FIncrement <> AValue then
    FIncrement := AValue;
end;

procedure TfpgSpinEdit.SetValue(const AValue: integer);
begin
  if (FValue <> AValue) and (AValue <= FMaxValue) and (AValue >= FMinValue) then
  begin
    FValue      := AValue;
    FEdit.Value := FValue;
    EnableButtons;
  end;
end;

procedure TfpgSpinEdit.ButtonUpClick(Sender: TObject);
begin
  if FValue + FIncrement <= FMaxValue then
    Value := FValue + FIncrement;
end;

procedure TfpgSpinEdit.ButtonDownClick(Sender: TObject);
begin
  if FValue - FIncrement >= FMinValue then
    Value := FValue - FIncrement;
end;

procedure TfpgSpinEdit.ButtonUpMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  FUp := True;
  FTimer.Enabled := True;
  FSteps := 0;
end;

procedure TfpgSpinEdit.ButtonUpMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  FUp := False;
  FTempIncrement := Increment;
  if Assigned(FTimer) then
    FTimer.Enabled := False;
end;

procedure TfpgSpinEdit.ButtonDownMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  FDown          := True;
  FTimer.Enabled := True;
  FSteps := 0;
end;

procedure TfpgSpinEdit.ButtonDownMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  FDown := False;
  FTempIncrement := Increment;
  if Assigned(FTimer) then
    FTimer.Enabled := False;
end;

procedure TfpgSpinEdit.EditKeyPress(Sender: TObject; var keycode: word; var shiftstate: TShiftState; var consumed: Boolean);
begin
  if (KeyCode = KeyReturn) or (KeyCode = KeyPEnter) then
    if FEdit.Text = '' then
    begin
      FValue      := 0;
      FEdit.Value := FValue;
    end
    else if (StrToInt(FEdit.Text) <= FMaxValue) and (StrToInt(FEdit.Text) >= FMinValue) then
      FValue      := FEdit.Value
    else
      FEdit.Value := FValue;

  if KeyCode = KeyUp then
    if FEdit.Value + Increment <= FMaxValue then
    begin
      Inc(FValue, FIncrement);
      FEdit.Value := FValue;
    end;

  if KeyCode = KeyDown then
    if FEdit.Value - Increment >= FMinValue then
    begin
      Dec(FValue, FIncrement);
      FEdit.Value := FValue;
    end;

  if KeyCode = KeyPageUp then
  begin
    FValue      := FMaxValue;
    FEdit.Value := FValue;
  end;

  if KeyCode = KeyPageDown then
  begin
    FValue      := FMinValue;
    FEdit.Value := FValue;
  end;

  EnableButtons;
end;

procedure TfpgSpinEdit.EditExit(Sender: TObject);
begin
  if FEdit.Text = '' then
  begin
    FValue      := 0;
    FEdit.Value := FValue;
  end
  else if (StrToInt(FEdit.Text) <= FMaxValue) and (StrToInt(FEdit.Text) >= FMinValue) then
    FValue      := FEdit.Value
  else
    FEdit.Value := FValue;
  EnableButtons;
end;

procedure TfpgSpinEdit.TimerStep(Sender: TObject);
begin
  if FUp then
  begin
    if FValue + FTempIncrement <= FMaxValue then
    begin
      Value       := FValue + FTempIncrement;
      FEdit.Value := FValue;
      if FSteps <= FSpeedUpSteps then
        Inc(FSteps);
      if FSteps > FSpeedUpSteps then
        FTempIncrement := LargeIncrement;
    end;
  end
  else if FDown then
  begin
    if FValue - FTempIncrement >= FMinValue then
    begin
      Value       := FValue - FTempIncrement;
      FEdit.Value := FValue;
      if FSteps <= FSpeedUpSteps then
        Inc(FSteps);
      if FSteps > FSpeedUpSteps then
        FTempIncrement := LargeIncrement;
    end;
  end;
  EnableButtons;
end;

constructor TfpgSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEdit := CreateEditInteger(Self, 0, 0, Width - FButtonWidth, Height);

  FMaxValue       := 100;
  FMinValue       := 0;
  FIncrement      := 1;
  FLargeIncrement := 10;
  FValue          := 0;
  FUp             := False;
  FDown           := False;

  FEdit.Value := FValue;

  FButtonUp.OnClick := @ButtonUpClick;
  FButtonDown.OnClick := @ButtonDownClick;
  FButtonUp.OnMouseDown := @ButtonUpMouseDown;
  FButtonUp.OnMouseUp := @ButtonUpMouseUp;
  FButtonDown.OnMouseDown := @ButtonDownMouseDown;
  FButtonDown.OnMouseUp := @ButtonDownMouseUp;
  FEdit.OnKeyPress := @EditKeyPress;
  FEdit.OnExit   := @EditExit;
  FTimer.OnTimer := @TimerStep;
  EnableButtons;
end;

end.

