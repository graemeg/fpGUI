{%mainunit fpgui.pas}

{$ifndef READ_IMPLEMENTATION}

type

  { TFButton }

  TFButton = class(TFWidget)
  private
    FIsPressed: Boolean;
    FText: string;
    procedure SetText(const AValue: string);
  protected
    procedure   EvFocusIn; override;
    procedure   EvFocusOut; override;
    procedure   EvKeyPressed(AKey: Word); override;
    procedure   EvKeyReleased(AKey: Word); override;
    procedure   EvMouseEnter(const AMousePos: TPoint); override;
    procedure   EvMouseLeave; override;
    procedure   EvMousePressed(AButton: TMouseButton; const AMousePos: TPoint); override;
    procedure   EvMouseReleased(AButton: TMouseButton; const AMousePos: TPoint); override;
    procedure   EvPaint; override;
  public
    { Constructors and Destructors }
    constructor Create(AParent: TFCustomWindow); override;
    constructor Create(ACaption: string; AParent: TFCustomWindow);
    { Properties }
    property Text: string read FText write SetText;
  end;


{$else}

constructor TFButton.Create(AParent: TFCustomWindow);
begin
  inherited Create(AParent);

  FDrawBackground := False;

  SetClientSize(Size(100, 50));
end;

constructor TFButton.Create(ACaption: string; AParent: TFCustomWindow);
begin
  Create(AParent);
  
  Text := ACaption;
end;

procedure TFButton.SetText(const AValue: string);
begin
  FText := AValue;
  
  Invalidate;
end;

procedure TFButton.EvFocusIn;
begin
  inherited EvFocusIn;
end;

procedure TFButton.EvFocusOut;
begin
  inherited EvFocusOut;
end;

procedure TFButton.EvKeyPressed(AKey: Word);
begin
  inherited EvKeyPressed(AKey);
end;

procedure TFButton.EvKeyReleased(AKey: Word);
begin
  inherited EvKeyReleased(AKey);
end;

procedure TFButton.EvMouseEnter(const AMousePos: TPoint);
begin
  inherited EvMouseEnter(AMousePos);
end;

procedure TFButton.EvMouseLeave;
begin
  FIsPressed := False;
  
  Invalidate;

  inherited EvMouseLeave;
end;

procedure TFButton.EvMousePressed(AButton: TMouseButton; const AMousePos: TPoint
 );
begin
  WriteLn('Mouse Pressed');

  FIsPressed := True;

  Invalidate;

  inherited EvMousePressed(AButton, AMousePos);
end;

procedure TFButton.EvMouseReleased(AButton: TMouseButton;
  const AMousePos: TPoint);
begin
  WriteLn('Mouse Released');

  FIsPressed := False;

  Invalidate;

  inherited EvMouseReleased(AButton, AMousePos);
end;

procedure TFButton.EvPaint;
var
  Flags: TFButtonFlags;
begin
  { Prepare the flags }
  
  Flags := [];

  if FIsPressed then
    Include(Flags, fbfIsPressed);

{  if FFocused then
    Include(lBtnFlags, btnHasFocus);  }

  { Draw the button }

  GFDefaultStyle.DrawButtonFace(Canvas, Bounds(0, 0, Width, Height), Flags);

  inherited EvPaint();
end;

{$endif}

