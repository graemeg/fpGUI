{$ifndef READ_IMPLEMENTATION}

type

  { TFButton }

  TFButton = class(TFWidget)
  public
    Text: string;
    constructor Create(AParent: TFCustomWindow); override;
    constructor Create(ACaption: string; AParent: TFCustomWindow);
    procedure   EvPaint; override;
    procedure   EvMouseReleased(AButton: TMouseButton; const AMousePos: TPoint); override;
  end;


{$else}

constructor TFButton.Create(AParent: TFCustomWindow);
begin
  inherited Create(AParent);

  SetClientSize(Size(100, 50));
end;

constructor TFButton.Create(ACaption: string; AParent: TFCustomWindow);
begin
  Create(AParent);
  
  DrawBackground := False;

  Text := ACaption;
end;

procedure TFButton.EvPaint;
var
  r: TRect;
  tw: integer;
begin
  Canvas.SetColor(colBlue);
  r.Left    := 0;
  r.Top     := 0;
  r.Right   := Width;
  r.Bottom  := Height;
  Canvas.FillRect(r);

  Canvas.SetColor(colWhite);
  r.Left    := 1;
  r.Top     := 1;
  r.Right   := Width + 1;
  r.Bottom  := Height + 1;
  Canvas.FillRect(r);

  inherited EvPaint();
end;

procedure TFButton.EvMouseReleased(AButton: TMouseButton;
  const AMousePos: TPoint);
begin
  WriteLn('Mouse released on child window');
  
  inherited EvMouseReleased(AButton, AMousePos);
end;

{$endif}

