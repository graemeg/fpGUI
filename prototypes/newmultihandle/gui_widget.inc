{%mainunit fpgui.pas}

{$ifndef READ_IMPLEMENTATION}

type
  TFWidget = class(TFWindow)
  protected
    FDrawBackground: Boolean;

    procedure   EvPaint; override;
  public
    constructor Create(AParent: TFCustomWindow); virtual;
  end;

{$else}

constructor TFWidget.Create(AParent: TFCustomWindow);
begin
  if (AParent = nil) then inherited Create(nil, [woWindow])
  else inherited Create(AParent, [woChildWindow]);

  { Inherited components should set this to False,
    or the background will override their painting }
  FDrawBackground := True;

  SetClientSize(Size(125, 125));
end;

procedure TFWidget.EvPaint;
begin
  if FDrawBackground then
  begin
    Canvas.SetColor(colLtGray);
    Canvas.FillRect(Bounds(0, 0, Width, Height));
  end;
  
  inherited EvPaint();
end;

{$endif}

