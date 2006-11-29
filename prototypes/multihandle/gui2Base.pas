{
  Proof of concept one handle per widget GUI.
  Graeme Geldenhuys
}

unit gui2Base;

{$mode objfpc}{$H+}

{$Define DEBUG}

interface

uses
  Classes
  ,fpgfx
  ,GFXBase
  ;

  
type

  { TWidget }

  TWidget = class(TFWindow)
  private
    FColor: TGfxColor;
    FOnClick: TNotifyEvent;
    FOnPainting: TNotifyEvent;
    procedure   EvOnPaint(Sender: TObject; const Rect: TRect); virtual;
    procedure   EvOnMousePress(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   SetColor(const AValue: TGfxColor);
  protected
    procedure   Paint; virtual;
    property    OnPainting: TNotifyEvent read FOnPainting write FOnPainting;
    property    OnClick: TNotifyEvent read FOnClick write FOnClick;
    property    Color: TGfxColor read FColor write SetColor;
  public
    constructor Create(AParent: TFCustomWindow; AWindowOptions: TFWindowOptions); override;
    constructor Create(AParent: TFCustomWindow); overload;
  end;

  { TForm }
  
  TForm = class(TWidget)
  public
    constructor Create(AParent: TFCustomWindow; AWindowOptions: TFWindowOptions); override;
    constructor Create; virtual; reintroduce;
    property    Color;
  end;

  { TPopupWindow }
  {$Note TPopupWindow is still work in progess. }
  TPopupWindow = class(TForm)
  public
    constructor Create; override;
  end;

  { TButton }

  TButton = class(TWidget)
  private
    FCaption: string;
    procedure   EvOnPaint(Sender: TObject; const Rect: TRect); override;
    procedure   SetCaption(const AValue: string);
  protected
    procedure   Paint; override;
  public
    constructor Create(AParent: TFCustomWindow; APosition: TPoint); overload; reintroduce;
    property    Caption: string read FCaption write SetCaption;
  published
    property    OnClick;
  end;
  
  { TLabel }

  TLabel = class(TWidget)
  private
    FCaption: string;
    procedure   SetCaption(const AValue: string);
  protected
    procedure   Paint; override;
  public
    constructor Create(AParent: TFCustomWindow; APosition: TPoint); overload; reintroduce;
    property    Caption: string read FCaption write SetCaption;
  end;

  { TCustomEdit }

  TCustomEdit = class(TWidget)
  private
    FText: string;
    procedure   SetText(const AValue: string);
  protected
    procedure   Paint; override;
  public
    constructor Create(AParent: TFCustomWindow; APosition: TPoint); overload; reintroduce;
    property    Text: string read FText write SetText;
  end;

  { TEdit }
  
  TEdit = class(TCustomEdit)
  public
    property    Text;
  end;


implementation

const
  clDkWhite: TGfxColor      = (Red: $e000; Green: $e000; Blue: $e000; Alpha: 0);
  cl3DShadow: TGfxColor     = (Red: $8000; Green: $8000; Blue: $8000; Alpha: 0);
  cl3DDkShadow: TGfxColor   = (Red: $0000; Green: $0000; Blue: $0000; Alpha: 0);
  cl3DHighlight: TGfxColor  = (Red: $FF00; Green: $FF00; Blue: $FF00; Alpha: 0);
  cl3DFace: TGfxColor		    = (Red: $c000; Green: $c000; Blue: $c000; Alpha: 0);
  clWindow: TGfxColor       = (Red: $FF00; Green: $FF00; Blue: $FF00; Alpha: 0);
  cl3DLight: TGfxColor      = (Red: $e000; Green: $e000; Blue: $e000; Alpha: 0);


// Helper functions, that will actually be in a style class.

procedure Draw3DFrame(Canvas: TFCanvas; const ARect: TRect; Color1, Color2, Color3, Color4: TGfxColor);
begin
  with ARect do
  begin
    Canvas.SetColor(Color1);
    Canvas.DrawLine(Point(Left, Bottom - 2), TopLeft);
    Canvas.DrawLine(TopLeft, Point(Right - 1, Top));

    Canvas.SetColor(Color2);
    Canvas.DrawLine(Point(Left + 1, Bottom - 3), Point(Left + 1, Top + 1));
    Canvas.DrawLine(Point(Left + 1, Top + 1), Point(Right - 2, Top + 1));

    Canvas.SetColor(Color3);
    Canvas.DrawLine(Point(Left, Bottom - 1), Point(Right - 1, Bottom - 1));
    Canvas.DrawLine(Point(Right - 1, Bottom - 1), Point(Right - 1, Top - 1));

    Canvas.SetColor(Color4);
    Canvas.DrawLine(Point(Left + 1, Bottom - 2), Point(Right - 2, Bottom - 2));
    Canvas.DrawLine(Point(Right - 2, Bottom - 2), Point(Right - 2, Top));
  end;
end;

procedure DrawEditBox(Canvas: TFCanvas; const ARect: TRect);
begin
  Draw3DFrame(Canvas, ARect, cl3DShadow, cl3DDkShadow, cl3DHighlight, cl3DFace);
  Canvas.SetColor(clWindow);
  with ARect do
    Canvas.FillRect(Rect(Left + 2, Top + 2, Right - 2, Bottom - 2));
end;

{ TWidget }

procedure TWidget.EvOnPaint(Sender: TObject; const Rect: TRect);
begin
  {$IFDEF DEBUG} Writeln(ClassName + '.Paint'); {$ENDIF}
  if Assigned(OnPainting) then
    OnPainting(self);
  Paint;
end;

procedure TWidget.EvOnMousePress(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  if AButton = mbLeft then
  begin
    if Assigned(OnClick) then
      OnClick(self);
  end;
end;

procedure TWidget.SetColor(const AValue: TGfxColor);
begin
  if FColor = AValue then exit;
  FColor := AValue;
  Paint;
end;

procedure TWidget.Paint;
var
  r: TRect;
begin
  Canvas.SetColor(FColor);
  r.Left    := 0;
  r.Top     := 0;
  r.Right   := Width;
  r.Bottom  := Height;
  Canvas.FillRect(r);
end;

constructor TWidget.Create(AParent: TFCustomWindow;
  AWindowOptions: TFWindowOptions);
begin
  inherited Create(AParent, AWindowOptions);
  FColor := colLtGray;
  OnPaint := @EvOnPaint;
  OnMouseReleased := @EvOnMousePress;
  Title := ClassName;
end;

constructor TWidget.Create(AParent: TFCustomWindow);
begin
  Create(AParent, [woChildWindow]);
end;

{ TForm }

constructor TForm.Create(AParent: TFCustomWindow;
  AWindowOptions: TFWindowOptions);
begin
  inherited Create(AParent, AWindowOptions);
end;

constructor TForm.Create;
begin
  inherited Create(nil, [woWindow]);
end;

{ TButton }

procedure TButton.EvOnPaint(Sender: TObject; const Rect: TRect);
begin
  inherited EvOnPaint(Sender, Rect);
  {$IFDEF DEBUG} Writeln('  - Painting ' + Caption); {$ENDIF}
end;

procedure TButton.SetCaption(const AValue: string);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
  Paint;
end;

procedure TButton.Paint;
var
  Pt: TPoint;
begin
  inherited Paint;
  Draw3DFrame(TFCanvas(Canvas), Rect(0, 0, Width, Height), cl3DHighlight, cl3DLight, cl3DDkShadow, cl3DShadow);
  
  Canvas.SetColor(colBlack);
  Pt.x := (Width - Canvas.TextWidth(FCaption)) div 2;
  Pt.y := ((Height - Canvas.FontCellHeight) div 2) + 1;
  Canvas.TextOut(Pt, FCaption);
end;

constructor TButton.Create(AParent: TFCustomWindow; APosition: TPoint);
begin
  inherited Create(AParent);
  SetPosition(APosition);
  SetClientSize(Size(75, 25));
end;

{ TLabel }

procedure TLabel.SetCaption(const AValue: string);
var
  w, h: integer;
begin
  if FCaption=AValue then exit;
  FCaption := AValue;

  w := Canvas.TextWidth(FCaption) + 6;
  h := Canvas.FontCellHeight + 4;
  SetClientSize(Size(w, h));
  Paint;
end;

procedure TLabel.Paint;
begin
//  Color := FParent.Canvas.GetColor;
//  inherited Paint;
  Canvas.SetColor(colWhite);
  Canvas.FillRect(Rect(0,0,Width,Height));
  Canvas.SetColor(colBlack);
  Canvas.TextOut(Point(0, 0), FCaption);
end;

constructor TLabel.Create(AParent: TFCustomWindow; APosition: TPoint);
begin
  inherited Create(AParent);
  SetPosition(APosition);
  SetClientSize(Size(75, 22));
end;

{ TPopupWindow }

constructor TPopupWindow.Create;
begin
//  inherited Create(nil, [woPopup]);
  inherited Create(nil, [woWindow]);
end;

{ TCustomEdit }

procedure TCustomEdit.SetText(const AValue: string);
var
  w: integer;
begin
  if FText=AValue then exit;
  FText:=AValue;
  w := Canvas.TextWidth(FText) + 6;
  SetClientSize(Size(w, Height));
  Paint;
end;

procedure TCustomEdit.Paint;
begin
  inherited Paint;
  DrawEditBox(TFCanvas(Canvas), Rect(0, 0, Width, Height));
  Canvas.SetColor(colBlack);
  if FText <> '' then
    Canvas.TextOut(Point(2, 2), FText);
end;

constructor TCustomEdit.Create(AParent: TFCustomWindow; APosition: TPoint);
begin
  inherited Create(AParent);
//  OnMouseEntered := @EvMouseEntered
  SetPosition(APosition);
  SetClientSize(Size(100, 25));
end;

end.

