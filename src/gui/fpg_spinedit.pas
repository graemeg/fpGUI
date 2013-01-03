{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
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
    FArrowUpColor: TfpgColor;
    FArrowDownColor: TfpgColor;
    FOnChange: TNotifyEvent;
    FTimer: TfpgTimer;
    FUp: Boolean;
    FDown: Boolean;
    FSteps: integer;
    FSpeedUpSteps: integer;
    procedure   SetButtonWidth(const AValue: integer);
  protected
    FButtonWidth: integer;
    procedure   DoOnChange; virtual;
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
    procedure   DisableTimer;
    procedure   ButtonUpPaint(Sender: TObject);
    procedure   ButtonDownPaint(Sender: TObject);
    property    ButtonsBackgroundColor: Tfpgcolor read GetButtonsBackgroundColor write SetButtonsBackgroundColor default clButtonFace;
    property    ArrowUpColor: TfpgColor read FArrowUpColor write SetArrowUpColor;
    property    ArrowDownColor: TfpgColor read FArrowDownColor write SetArrowDownColor;
    property    ButtonWidth: integer read FButtonWidth write SetButtonWidth default 13;
    property    StepsSpeedUp: integer read FSpeedUpSteps write SetSpeedUp;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
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
    function GetFixedDecimals: integer;
    procedure ResizeChildren; override;
    procedure SetEditBackgroundColor(const AValue: Tfpgcolor);
    procedure SetTextColor(const AValue: Tfpgcolor); override;
    procedure SetNegativeColor(const AValue: Tfpgcolor);
    procedure SetFontDesc(const AValue: string);
    procedure SetMaxValue(const AValue: extended);
    procedure SetMinValue(const AValue: extended);
    procedure SetIncrement(const AValue: extended);
    procedure SetLargeIncrement(const AValue: extended);
    procedure SetValue(const AValue: extended);
    procedure SetDecimals(const AValue: integer);
    procedure SetFixedDecimals(const AValue: integer);
    procedure SetHint(const AValue: TfpgString); override;
    procedure ButtonUpClick(Sender: TObject);
    procedure ButtonDownClick(Sender: TObject);
    procedure ButtonUpMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonUpMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonDownMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonDownMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure EditKeyPress(Sender: TObject; var keycode: word; var shiftstate: TShiftState; var consumed: Boolean);
    procedure EditExit(Sender: TObject);
    procedure MouseEnter(Sender: TObject);
    procedure MouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
    procedure MouseExit(Sender: TObject);
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
    property MaxValue: extended read FMaxValue write SetMaxValue;
    property MinValue: extended read FMinValue write SetMinValue;
    property Increment: extended read FIncrement write SetIncrement;
    property LargeIncrement: extended read FLargeIncrement write SetLargeIncrement;
    property Value: extended read FValue write SetValue;
    property Decimals: integer read GetDecimals write SetDecimals;
    property FixedDecimals: integer read GetFixedDecimals write SetFixedDecimals;
    property Hint;
    property TabOrder;
    property    OnChange;
    property    OnEnter;
    property    OnExit;
    property    OnKeyPress;
    property    OnMouseEnter;
    property    OnMouseExit;
    property    OnPaint;
    property    OnShowHint;
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
    procedure SetTextColor(const AValue: Tfpgcolor); override;
    procedure SetNegativeColor(const AValue: Tfpgcolor);
    procedure SetFontDesc(const AValue: string);
    procedure SetMaxValue(const AValue: integer);
    procedure SetMinValue(const AValue: integer);
    procedure SetIncrement(const AValue: integer);
    procedure SetLargeIncrement(const AValue: integer);
    procedure SetValue(const AValue: integer);
    procedure SetHint(const AValue: TfpgString); override;
    procedure ButtonUpClick(Sender: TObject);
    procedure ButtonDownClick(Sender: TObject);
    procedure ButtonUpMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonUpMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonDownMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonDownMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure EditKeyPress(Sender: TObject; var keycode: word; var shiftstate: TShiftState; var consumed: Boolean);
    procedure EditExit(Sender: TObject);
    procedure MouseEnter(Sender: TObject);
    procedure MouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
    procedure MouseExit(Sender: TObject);
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
    property LargeIncrement: integer read FLargeIncrement write SetLargeIncrement default 10;
    property Value: integer read FValue write SetValue default 0;
    property Hint;
    property TabOrder;
    property    OnChange;
    property    OnEnter;
    property    OnExit;
    property    OnKeyPress;
    property    OnMouseEnter;
    property    OnMouseExit;
    property    OnPaint;
    property    OnShowHint;
  end;


function CreateSpinEditFloat(AOwner: TComponent; x, y, w, h: TfpgCoord;
         AMinValue: extended = 0; AMaxValue: extended = 100; AIncrement: extended = 1; ALargeIncrement: extended = 10.0;
         AFixedDecimals: integer = 1; AValue: extended = 0; ADecimals: integer = -1): TfpgSpinEditFloat;
function CreateSpinEdit(AOwner: TComponent; x, y, w, h: TfpgCoord; AMinValue: integer = 0;
         AMaxValue: integer = 100; AIncrement: integer = 1; ALargeIncrement: integer = 10;
         AValue: integer = 0): TfpgSpinEdit;


implementation

uses
  fpg_extgraphics,
  fpg_stringutils,
  math;


function CreateSpinEditFloat(AOwner: TComponent; x, y, w, h: TfpgCoord;
         AMinValue: extended = 0; AMaxValue: extended = 100; AIncrement: extended = 1; ALargeIncrement: extended = 10.0;
         AFixedDecimals: integer = 1; AValue: extended = 0; ADecimals: integer = -1): TfpgSpinEditFloat;
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
  Result.LargeIncrement := ALargeIncrement;
  Result.FEdit.FixedDecimals := AFixedDecimals;
  Result.FEdit.Decimals := ADecimals;
  if (AValue <= Result.MaxValue) and (AValue >= Result.MinValue) then
    Result.Value := AValue;
end;

function CreateSpinEdit(AOwner: TComponent; x, y, w, h: TfpgCoord; AMinValue: integer = 0;
         AMaxValue: integer = 100; AIncrement: integer = 1; ALargeIncrement: integer = 10;
         AValue: integer = 0): TfpgSpinEdit;
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
  Result.LargeIncrement := ALargeIncrement;
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

procedure TfpgAbstractSpinEdit.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TfpgAbstractSpinEdit.ResizeChildren;
begin
  FButtonUp.SetPosition(Width - FButtonWidth, 0, FButtonWidth, Height div 2);
  FButtonDown.SetPosition(Width - FButtonWidth, Height div 2, FButtonWidth, Height div 2);
end;

procedure TfpgAbstractSpinEdit.HandlePaint;
begin
  Canvas.Clear(BackgroundColor);
end;

procedure TfpgAbstractSpinEdit.HandleResize(AWidth, AHeight: TfpgCoord);
begin
  inherited HandleResize(AWidth, AHeight);
  if not (csLoading in ComponentState) then
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

procedure TfpgAbstractSpinEdit.DisableTimer;
begin
  FUp:= False;
  FDown:= False;
  if Assigned(FTimer) then
    FTimer.Enabled:= False;
end;

function GetButtonRect(AButton: TfpgButton): TRect;
var
  r: TfpgRect;
begin
  r := AButton.GetClientRect;

  InflateRect(r, -2, -2); // button borders
  if AButton.Down then
    OffsetRect(r, 1, 1);

  Result := fpgRectToRect(r);
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
  FButtonUp.Enabled := True;
  FButtonDown.Enabled := True;
  if IsMaxLimitReached then
    FButtonUp.Enabled := False
  else
    if IsMinLimitReached then
      FButtonDown.Enabled := False;
end;

function TfpgSpinEditFloat.IsMinLimitReached: Boolean;
begin
  Result := FValue = FMinValue;
end;

function TfpgSpinEditFloat.IsMaxLimitReached: Boolean;
begin
  Result := FValue = FMaxValue;
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

function TfpgSpinEditFloat.GetFixedDecimals: integer;
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
  end;
end;

procedure TfpgSpinEditFloat.SetMinValue(const AValue: extended);
begin
  if (FMinValue <> AValue) and (AValue < FMaxValue) then
  begin
    FMinValue := AValue;
    if FValue < FMinValue then
    begin
      FValue      := FMinValue;
      FEdit.Value := FValue;
    end;
  end;
end;

procedure TfpgSpinEditFloat.SetIncrement(const AValue: extended);
begin
  if FIncrement <> AValue then
  begin
    FIncrement := AValue;
    if FLargeIncrement < AValue then
      FLargeIncrement := AValue;
  end;
end;

procedure TfpgSpinEditFloat.SetLargeIncrement(const AValue: extended);
begin
  if FLargeIncrement <> AValue then
    if FIncrement <= AValue then
      FLargeIncrement := AValue;
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

procedure TfpgSpinEditFloat.SetFixedDecimals(const AValue: integer);
begin
  if AValue < 0 then
    Exit; // =>
  if FEdit.FixedDecimals <> AValue then
    FEdit.FixedDecimals := AValue;
end;

procedure TfpgSpinEditFloat.SetHint(const AValue: TfpgString);
begin
  inherited SetHint(AValue);
  // let child component use the same hint
  FEdit.Hint := AValue;
  FButtonUp.Hint := AValue;
  FButtonDown.Hint := AValue;
end;

procedure TfpgSpinEditFloat.ButtonUpClick(Sender: TObject);
begin
  if FValue + FIncrement <= FMaxValue then
  begin
    FValue := FValue + FIncrement;
    FEdit.Value := FValue;
  end
  else if not IsMaxLimitReached then
  begin
    FValue := FMaxValue;
    FEdit.Value := FValue;
  end;
  DoOnChange;
  EnableButtons;
end;

procedure TfpgSpinEditFloat.ButtonDownClick(Sender: TObject);
begin
  if FValue - FIncrement >= FMinValue then
  begin
    FValue := FValue - FIncrement;
    FEdit.Value := FValue;
  end
  else if not IsMinLimitReached then
  begin
    FValue := FMinValue;
    FEdit.Value := FValue;
  end;
  DoOnChange;
  EnableButtons;
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
  DisableTimer;
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
  DisableTimer;
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
    end
    else if not IsMaxLimitReached then
    begin
      FValue      := FMaxValue;
      FEdit.Value := FValue;
    end;

  if KeyCode = KeyDown then
    if FEdit.Value - Increment >= FMinValue then
    begin
      FValue      := FValue - FIncrement;
      FEdit.Value := FValue;
    end
    else if not IsMinLimitReached then
    begin
      FValue      := FMinValue;
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

procedure TfpgSpinEditFloat.MouseEnter(Sender: TObject);
var
  msgp: TfpgMessageParams;
  b: boolean;
begin
  fillchar(msgp, sizeof(msgp), 0);
  if Sender is TfpgEditFloat then
    with Sender as TfpgEditFloat do
      if Assigned(Parent) then
        b := Enabled and fpgApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.Parent.ShowHint)) and (FHint <> '')
      else
        b := Enabled and fpgApplication.ShowHint and FShowHint and (FHint <> '');
  if Sender is TfpgButton then
    with Sender as TfpgButton do
      if Assigned(Parent) then
        b := Enabled and fpgApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.Parent.ShowHint)) and (FHint <> '')
      else
        b := Enabled and fpgApplication.ShowHint and FShowHint and (FHint <> '');

  msgp.user.Param1 := Ord(b);
  fpgPostMessage(Sender, fpgApplication, FPGM_HINTTIMER, msgp);
end;

procedure TfpgSpinEditFloat.MouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
var
  msgp: TfpgMessageParams;
begin
  fillchar(msgp, sizeof(msgp), 0);
  msgp.user.Param1 := 2;
  msgp.user.Param2 := AMousePos.x+10;
  msgp.user.Param3 := AMousePos.y+2;

  { Only send message if really needed. }
  if Sender is TfpgEditFloat then
    with Sender as TfpgEditFloat do
      if Assigned(Parent) then
      begin
        if fpgApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.Parent.ShowHint)) and (FHint <> '') then
          fpgPostMessage(Sender, fpgApplication, FPGM_HINTTIMER, msgp);
      end
      else
        if fpgApplication.ShowHint and FShowHint and (FHint <> '') then
          fpgPostMessage(Sender, fpgApplication, FPGM_HINTTIMER, msgp);
  if Sender is TfpgButton then
    with Sender as TfpgButton do
      if Assigned(Parent) then
      begin
        if fpgApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.Parent.ShowHint)) and (FHint <> '') then
          fpgPostMessage(Sender, fpgApplication, FPGM_HINTTIMER, msgp);
      end
      else
        if fpgApplication.ShowHint and FShowHint and (FHint <> '') then
          fpgPostMessage(Sender, fpgApplication, FPGM_HINTTIMER, msgp);
end;

procedure TfpgSpinEditFloat.MouseExit(Sender: TObject);
begin
  if Sender is TfpgEditFloat then
    with Sender as TfpgEditFloat do
      if FShowHint then
        fpgApplication.HideHint;
  if Sender is TfpgButton then
    with Sender as TfpgButton do
      if FShowHint then
        fpgApplication.HideHint;
end;

procedure TfpgSpinEditFloat.TimerStep(Sender: TObject);
begin
  if FUp then
  begin
    if FValue + FTempIncrement <= FMaxValue then
    begin
      FValue       := FValue + FTempIncrement;
      FEdit.Value := FValue;
      if FSteps <= FSpeedUpSteps then
        Inc(FSteps);
      if FSteps > FSpeedUpSteps then
        FTempIncrement := LargeIncrement;
      DoOnChange;
      if FValue= FMaxValue then
        DisableTimer;
    end
    else if not IsMaxLimitReached then
    begin
      FValue := FMaxValue;
      FEdit.Value := FValue;
      DoOnChange;
      DisableTimer;
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
      DoOnChange;
      if FValue= FMinValue then
        DisableTimer;
    end
    else if not IsMinLimitReached then
    begin
      FValue := FMinValue;
      FEdit.Value := FValue;
      DoOnChange;
      DisableTimer;
    end;
  end;
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

  FEdit.Decimals := -1;
  FEdit.FixedDecimals := 1;
  FEdit.Value    := FValue;

  FButtonUp.OnClick := @ButtonUpClick;
  FButtonDown.OnClick := @ButtonDownClick;
  FButtonUp.OnMouseDown := @ButtonUpMouseDown;
  FButtonUp.OnMouseUp := @ButtonUpMouseUp;
  FButtonDown.OnMouseDown := @ButtonDownMouseDown;
  FButtonDown.OnMouseUp := @ButtonDownMouseUp;
  FEdit.OnKeyPress  := @EditKeyPress;
  FEdit.OnExit      := @EditExit;
  FEdit.OnMouseEnter:= @MouseEnter;
  FButtonUp.OnMouseEnter:= @MouseEnter;
  FButtonDown.OnMouseEnter:= @MouseEnter;
  FEdit.OnMouseMove:= @MouseMove;
  FButtonUp.OnMouseMove:= @MouseMove;
  FButtonDown.OnMouseMove:= @MouseMove;
  FEdit.OnMouseExit:= @MouseExit;
  FButtonUp.OnMouseExit:= @MouseExit;
  FButtonDown.OnMouseExit:= @MouseExit;

  FTimer.OnTimer := @TimerStep;
  EnableButtons;
end;


{ TfpgSpinEdit }

procedure TfpgSpinEdit.EnableButtons;
begin
  FButtonUp.Enabled := True;
  FButtonDown.Enabled := True;
  if IsMaxLimitReached then
    FButtonUp.Enabled := False
  else
    if IsMinLimitReached then
      FButtonDown.Enabled := False;
end;

function TfpgSpinEdit.IsMinLimitReached: Boolean;
begin
  Result := FValue = FMinValue;
end;

function TfpgSpinEdit.IsMaxLimitReached: Boolean;
begin
  Result:= FValue = FMaxValue;
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
  end;
end;

procedure TfpgSpinEdit.SetIncrement(const AValue: integer);
begin
  if FIncrement <> AValue then
  begin
    FIncrement := AValue;
    if FLargeIncrement < AValue then
      FLargeIncrement := AValue;
  end;
end;

procedure TfpgSpinEdit.SetLargeIncrement(const AValue: integer);
begin
  if FLargeIncrement <> AValue then
    if FIncrement <= AValue then
      FLargeIncrement := AValue;
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

procedure TfpgSpinEdit.SetHint(const AValue: TfpgString);
begin
  inherited SetHint(AValue);
  // let child component use the same hint
  FEdit.Hint := AValue;
  FButtonUp.Hint := AValue;
  FButtonDown.Hint := AValue;
end;

procedure TfpgSpinEdit.ButtonUpClick(Sender: TObject);
begin
  if FValue + FIncrement <= FMaxValue then
  begin
    Value := FValue + FIncrement;
    FEdit.Value:= FValue;
  end
  else if not IsMaxLimitReached then
  begin
    Value := FMaxValue;
    FEdit.Value:= FValue;
  end;
  DoOnChange;
  EnableButtons;
end;

procedure TfpgSpinEdit.ButtonDownClick(Sender: TObject);
begin
  if FValue - FIncrement >= FMinValue then
  begin
    Value := FValue - FIncrement;
    FEdit.Value:= FValue;
  end
  else if not IsMinLimitReached then
  begin
    Value := FMinValue;
    FEdit.Value:= FValue;
  end;
  DoOnChange;
  EnableButtons;
end;

procedure TfpgSpinEdit.ButtonUpMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  FUp := True;
  FTimer.Enabled := True;
  FSteps := 0;
  FTempIncrement := Increment;
end;

procedure TfpgSpinEdit.ButtonUpMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  DisableTimer;
end;

procedure TfpgSpinEdit.ButtonDownMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  FDown          := True;
  FTimer.Enabled := True;
  FSteps := 0;
  FTempIncrement := Increment;
end;

procedure TfpgSpinEdit.ButtonDownMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  DisableTimer;
end;

procedure TfpgSpinEdit.EditKeyPress(Sender: TObject; var keycode: word; var shiftstate: TShiftState; var consumed: Boolean);
begin
  if (KeyCode = KeyReturn) or (KeyCode = KeyPEnter) then
    if FEdit.Text = '' then
    begin
      FValue      := 0;
      FEdit.Value := FValue;
      DoOnChange;
    end
    else if (StrToInt(FEdit.Text) <= FMaxValue) and (StrToInt(FEdit.Text) >= FMinValue) then
    begin
      FValue      := FEdit.Value;
      DoOnChange;
    end
    else
      FEdit.Value := FValue;

  if KeyCode = KeyUp then
    if FEdit.Value + Increment <= FMaxValue then
    begin
      Inc(FValue, FIncrement);
      FEdit.Value := FValue;
      DoOnChange;
    end
    else if not IsMaxLimitReached then
    begin
      FValue      := FMaxValue;
      FEdit.Value := FValue;
      DoOnChange;
    end;

  if KeyCode = KeyDown then
    if FEdit.Value - Increment >= FMinValue then
    begin
      Dec(FValue, FIncrement);
      FEdit.Value := FValue;
      DoOnChange;
    end
    else if not IsMinLimitReached then
    begin
      FValue      := FMinValue;
      FEdit.Value := FValue;
      DoOnChange;
    end;

  if KeyCode = KeyPageUp then
  begin
    FValue      := FMaxValue;
    FEdit.Value := FValue;
    DoOnChange;
  end;

  if KeyCode = KeyPageDown then
  begin
    FValue      := FMinValue;
    FEdit.Value := FValue;
    DoOnChange;
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

procedure TfpgSpinEdit.MouseEnter(Sender: TObject);
var
  msgp: TfpgMessageParams;
  b: boolean;
begin
  fillchar(msgp, sizeof(msgp), 0);
  if Sender is TfpgEditInteger then
    with Sender as TfpgEditInteger do
      if Assigned(Parent) then
        b := Enabled and fpgApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.Parent.ShowHint)) and (FHint <> '')
      else
        b := Enabled and fpgApplication.ShowHint and FShowHint and (FHint <> '');
  if Sender is TfpgButton then
    with Sender as TfpgButton do
      if Assigned(Parent) then
        b := Enabled and fpgApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.Parent.ShowHint)) and (FHint <> '')
      else
        b := Enabled and fpgApplication.ShowHint and FShowHint and (FHint <> '');

  msgp.user.Param1 := Ord(b);
  fpgPostMessage(Sender, fpgApplication, FPGM_HINTTIMER, msgp);
end;

procedure TfpgSpinEdit.MouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
var
  msgp: TfpgMessageParams;
begin
  fillchar(msgp, sizeof(msgp), 0);
  msgp.user.Param1 := 2;
  msgp.user.Param2 := AMousePos.x+10;
  msgp.user.Param3 := AMousePos.y+2;

  { Only send message if really needed. }
  if Sender is TfpgEditInteger then
    with Sender as TfpgEditInteger do
      if Assigned(Parent) then
      begin
        if fpgApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.Parent.ShowHint)) and (FHint <> '') then
          fpgPostMessage(Sender, fpgApplication, FPGM_HINTTIMER, msgp);
      end
      else
        if fpgApplication.ShowHint and FShowHint and (FHint <> '') then
          fpgPostMessage(Sender, fpgApplication, FPGM_HINTTIMER, msgp);
  if Sender is TfpgButton then
    with Sender as TfpgButton do
      if Assigned(Parent) then
      begin
        if fpgApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.Parent.ShowHint)) and (FHint <> '') then
          fpgPostMessage(Sender, fpgApplication, FPGM_HINTTIMER, msgp);
      end
      else
        if fpgApplication.ShowHint and FShowHint and (FHint <> '') then
          fpgPostMessage(Sender, fpgApplication, FPGM_HINTTIMER, msgp);
end;

procedure TfpgSpinEdit.MouseExit(Sender: TObject);
begin
  if Sender is TfpgEditInteger then
    with Sender as TfpgEditInteger do
      if FShowHint then
        fpgApplication.HideHint;
  if Sender is TfpgButton then
    with Sender as TfpgButton do
      if FShowHint then
        fpgApplication.HideHint;
end;

procedure TfpgSpinEdit.TimerStep(Sender: TObject);
begin
  if FUp then
  begin
    if FValue + FTempIncrement <= FMaxValue then
    begin
      FValue       := FValue + FTempIncrement;
      FEdit.Value := FValue;
      if FSteps <= FSpeedUpSteps then
        Inc(FSteps);
      if FSteps > FSpeedUpSteps then
        FTempIncrement := LargeIncrement;
      DoOnChange;
      if FValue= FMaxValue then
        DisableTimer;
    end
    else if not IsMaxLimitreached then
    begin
      FValue := FMaxValue;
      FEdit.Value := FValue;
      DoOnChange;
      DisableTimer;
    end;
  end
  else if FDown then
  begin
    if FValue - FTempIncrement >= FMinValue then
    begin
      FValue       := FValue - FTempIncrement;
      FEdit.Value := FValue;
      if FSteps <= FSpeedUpSteps then
        Inc(FSteps);
      if FSteps > FSpeedUpSteps then
        FTempIncrement := LargeIncrement;
      DoOnChange;
      if FValue= FMinValue then
        DisableTimer;
    end
    else
    begin
      FValue := FMinValue;
      FEdit.Value := FValue;
      DoOnChange;
      DisableTimer;
    end;
  end;
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
  FEdit.OnKeyPress  := @EditKeyPress;
  FEdit.OnExit      := @EditExit;
  FEdit.OnMouseEnter:= @MouseEnter;
  FButtonUp.OnMouseEnter:= @MouseEnter;
  FButtonDown.OnMouseEnter:= @MouseEnter;
  FEdit.OnMouseMove:= @MouseMove;
  FButtonUp.OnMouseMove:= @MouseMove;
  FButtonDown.OnMouseMove:= @MouseMove;
  FEdit.OnMouseExit:= @MouseExit;
  FButtonUp.OnMouseExit:= @MouseExit;
  FButtonDown.OnMouseExit:= @MouseExit;

  FTimer.OnTimer := @TimerStep;
  EnableButtons;
end;

end.

