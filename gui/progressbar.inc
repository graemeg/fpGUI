{
    fpGUI  -  Free Pascal GUI Library
    
    Progress Bar class declarations
    
    Copyright (C) 2000 - 2006 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{%mainunit fpgui.pp}

{
  Progress Bar implementation
}

{$IFDEF read_interface}

  { TCustomProgressBar }

  TCustomProgressBar = class(TCustomPanel)
  private
    FFillColor: TColor;
    FMax: integer;
    FMin: integer;
    FPosition: integer;
    FShowPercentage: Boolean;
    procedure   SetFillColor(const AValue: TColor);
    procedure   SetMax(const AValue: integer);
    procedure   SetMin(const AValue: integer);
    procedure   SetPosition(const AValue: integer);
    procedure   SetShowPercentage(const AValue: Boolean);
  protected
    procedure   Paint(Canvas: TFCanvas); override;
    property    FillColor: TColor read FFillColor write SetFillColor default clRed;
    property    Position: integer read FPosition write SetPosition;
    property    Min: integer read FMin write SetMin default 0;
    property    Max: integer read FMax write SetMax default 100;
    property    ShowPercentage: Boolean read FShowPercentage write SetShowPercentage default True;
  public
    constructor Create(const pText: string; pOwner: TComponent); overload;
  end;


  TProgressBar = class(TCustomProgressBar)
  published
    property    CanExpandWidth;
    property    CanExpandHeight;
    property    Enabled;
//    property    Text;
    property    FillColor;
    property    Position;
    property    Min;
    property    Max;
    property    ShowPercentage;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}

procedure TCustomProgressBar.SetFillColor(const AValue: TColor);
begin
  if FFillColor = AValue then exit;
  FFillColor := AValue;
  Redraw;
end;

procedure TCustomProgressBar.SetMax(const AValue: integer);
begin
  if FMax = AValue then exit;
  FMax := AValue;
  if FPosition > FMax then
    FPosition := FMax;
  Redraw;
end;

procedure TCustomProgressBar.SetMin(const AValue: integer);
begin
  if FMin = AValue then exit;
  FMin := AValue;
  if FPosition < FMin then
    FPosition := FMin;
  Redraw;
end;

procedure TCustomProgressBar.SetPosition(const AValue: integer);
begin
  if FPosition = AValue then
    exit; //==>
  if (AValue >= Min) and (AValue <= Max) then
  begin
    FPosition := AValue;
    Redraw;
  end;
end;

procedure TCustomProgressBar.SetShowPercentage(const AValue: Boolean);
begin
  if FShowPercentage = AValue then
    Exit; //==>
  FShowPercentage := AValue;
  Redraw;
end;

procedure TCustomProgressBar.Paint(Canvas: TFCanvas);
var
  Pt: TPoint;
  r: TRect;
  p: integer;
  percent: integer;
  t: string;
begin
  FText := '';
  inherited Paint(Canvas);
  Canvas.SetColor(Style.GetUIColor(FFillColor));
  
  percent := (100 div (Max - Min)) * FPosition;
  p := (percent * (Width - 3)) div 100;
  
  r := Rect(
        ClientRect.Left + 3,
        ClientRect.Top + 3,
        p,
        ClientRect.Bottom - 3);
  Canvas.FillRect(r);
  
  if FShowPercentage then
  begin
    t := IntToStr(percent) + '%';
    Pt.x := (Width - Canvas.TextWidth(t)) div 2;
    Pt.y := (Height - Canvas.FontCellHeight) div 2;
    Canvas.SetColor(Style.GetUIColor(clBtnText));
    Style.DrawText(Canvas, Pt, t, WidgetState);
  end;
end;

constructor TCustomProgressBar.Create(const pText: string; pOwner: TComponent);
begin
  inherited Create(pText, pOwner);
  FCanExpandHeight  := False;
  FBevelStyle       := bsLowered;
  FFillColor        := clRed;
  FMin              := 0;
  FMax              := 100;
  FShowPercentage   := True;
end;


{$ENDIF read_implementation}

