{%mainunit fpgui.pas}

{$ifndef READ_IMPLEMENTATION}

type

  { TFEdit }

  TFEdit = class(TFWidget)
  private
    FText: String;
    procedure SetText(const AValue: String);
  protected
    procedure   EvFocusIn; override;
    procedure   EvFocusOut; override;
    procedure   EvKeyPressed(AKey: Word); override;
    procedure   EvKeyReleased(AKey: Word); override;
    procedure   EvKeyChar(AKeyChar: Char); override;
    procedure   EvMousePressed(AButton: TMouseButton; const AMousePos: TPoint); override;
    procedure   EvMouseReleased(AButton: TMouseButton; const AMousePos: TPoint); override;
    procedure   EvPaint; override;
  public
    { Constructors and Destructors }
    constructor Create(AParent: TFCustomWindow); override;
    { Properties }
    property Text: String read FText write SetText;
  end;


{$else}

constructor TFEdit.Create(AParent: TFCustomWindow);
begin
  inherited Create(AParent);

  FDrawBackground := False;

  SetClientSize(Size(100, 50));
end;

procedure TFEdit.SetText(const AValue: String);
begin
  if FText = AValue then exit;
  
  FText := AValue;
  
  Invalidate;
end;

procedure TFEdit.EvFocusIn;
begin
  inherited EvFocusIn;
end;

procedure TFEdit.EvFocusOut;
begin
  inherited EvFocusOut;
end;

procedure TFEdit.EvKeyPressed(AKey: Word);
begin
  inherited EvKeyPressed(AKey);
end;

procedure TFEdit.EvKeyReleased(AKey: Word);
begin
  inherited EvKeyReleased(AKey);
end;

procedure TFEdit.EvKeyChar(AKeyChar: Char);
begin
  WriteLn('KeyChar');

  FText := FText + AKeyChar;
  
  Invalidate;
end;

procedure TFEdit.EvMousePressed(AButton: TMouseButton; const AMousePos: TPoint);
begin
  Self.CaptureMouse;
end;

procedure TFEdit.EvMouseReleased(AButton: TMouseButton;
  const AMousePos: TPoint);
begin
  inherited EvMouseReleased(AButton, AMousePos);
end;

procedure TFEdit.EvPaint;
begin
  { Draw the edit }

  GFDefaultStyle.DrawEdit(Canvas, Bounds(0, 0, Width, Height), FText);

  inherited EvPaint();
end;

{$endif}

