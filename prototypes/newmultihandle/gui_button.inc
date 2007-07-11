{%mainunit fpgui.pas}

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
  rt: TRect;
  Flags: TFButtonFlags;
begin
  rt.Left := 0;
  rt.Top := 0;
  rt.Right := Width;
  rt.Bottom := Height;

  { Prepare the flags }
  
  Flags := [];

 { if FDown then
    Include(lBtnFlags, btnIsPressed);

  if FFocused and (not FEmbedded) then
    Include(lBtnFlags, btnHasFocus);

  if FEmbedded then
    Include(lBtnFlags, btnIsEmbedded); }

  { Draw the button }

  GFDefaultStyle.DrawButtonFace(Canvas, rt, Flags);

  inherited EvPaint();
end;

procedure TFButton.EvMouseReleased(AButton: TMouseButton;
  const AMousePos: TPoint);
begin
  WriteLn('Mouse released on child window');
  
  inherited EvMouseReleased(AButton, AMousePos);
end;

{$endif}

