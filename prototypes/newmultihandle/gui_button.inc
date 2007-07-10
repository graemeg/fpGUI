{$ifndef READ_IMPLEMENTATION}

type
  TFButton = class(TFWidget)
  public
    Text: string;
    constructor Create(AParent: TFCustomWindow); override;
    constructor Create(ACaption: string; AParent: TFCustomWindow);
    procedure   Paint(Sender: TObject; const Rect: TRect); override;
    procedure   MouseReleased(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
  end;


{$else}

constructor TFButton.Create(AParent: TFCustomWindow);
begin
  inherited Create(AParent);

  OnMouseReleased := @MouseReleased;
  OnPaint := @Paint;

  SetClientSize(Size(125, 125));
  SetMinMaxClientSize(Size(125, 125), Size(125, 125));
end;

constructor TFButton.Create(ACaption: string; AParent: TFCustomWindow);
begin
  Create(AParent);
  
  Text := ACaption;
end;

procedure TFButton.MouseReleased(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  WriteLn('Mouse released on child window');
end;

procedure TFButton.Paint(Sender: TObject; const Rect: TRect);
var
  r: TRect;
  tw: integer;
begin
  Canvas.SetColor(colGray);
  r.Left    := 0;
  r.Top     := 0;
  r.Right   := Width;
  r.Bottom  := Height;
  Canvas.DrawRect(r);

  Canvas.SetColor(colWhite);
  r.Left    := 1;
  r.Top     := 1;
  r.Right   := Width + 1;
  r.Bottom  := Height + 1;
  Canvas.DrawRect(r);
end;

{$endif}

