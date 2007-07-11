{$ifndef READ_IMPLEMENTATION}

type
  TFWidget = class(TFWindow)
  public
    constructor Create(AParent: TFCustomWindow); virtual;
    procedure   EvPaint; override;
  end;

{$else}

constructor TFWidget.Create(AParent: TFCustomWindow);
begin
  if (AParent = nil) then inherited Create(nil, [woWindow])
  else inherited Create(AParent, [woChildWindow]);

  SetClientSize(Size(125, 125));
end;

procedure TFWidget.EvPaint;
var
  r: TRect;
begin
  Canvas.SetColor(colLtGray);
  r.Left    := 0;
  r.Top     := 0;
  r.Right   := Width;
  r.Bottom  := Height;
  Canvas.FillRect(r);
  
  inherited EvPaint();
end;

{$endif}

