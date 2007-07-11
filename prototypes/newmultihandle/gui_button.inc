{%mainunit fpgui.pas}

{$ifndef READ_IMPLEMENTATION}

type

  { TFButton }

  TFButton = class(TFWidget)
  private
    FIsPressed: Boolean;
  public
    Text: string;
    constructor Create(AParent: TFCustomWindow); override;
    constructor Create(ACaption: string; AParent: TFCustomWindow);
    procedure   EvFocusIn; override;
    procedure   EvFocusOut; override;
    procedure   EvKeyPressed(AKey: Word); override;
    procedure   EvKeyReleased(AKey: Word); override;
    procedure   EvMouseEnter(const AMousePos: TPoint); override;
    procedure   EvMouseLeave; override;
    procedure   EvMousePressed(AButton: TMouseButton; const AMousePos: TPoint); override;
    procedure   EvMouseReleased(AButton: TMouseButton; const AMousePos: TPoint); override;
    procedure   EvPaint; override;
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
  FIsPressed := True;

  Invalidate;

  inherited EvMousePressed(AButton, AMousePos);
end;

procedure TFButton.EvMouseReleased(AButton: TMouseButton;
  const AMousePos: TPoint);
begin
  FIsPressed := False;

  Invalidate;

  inherited EvMouseReleased(AButton, AMousePos);
end;

procedure TFButton.EvPaint;
var
  rt: TRect;
  Flags: TFButtonFlags;
begin
  WriteLn('Paint event');

  rt.Left := 0;
  rt.Top := 0;
  rt.Right := Width;
  rt.Bottom := Height;

  { Prepare the flags }
  
  Flags := [];

  if FIsPressed then
    Include(Flags, fbfIsPressed);

{  if FFocused and (not FEmbedded) then
    Include(lBtnFlags, btnHasFocus);

  if FEmbedded then
    Include(lBtnFlags, btnIsEmbedded); }

  { Draw the button }

  GFDefaultStyle.DrawButtonFace(Canvas, rt, Flags);

  inherited EvPaint();
end;

{$endif}

