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

  TWidgetStyle = set of (
      wsCaptureMouse,
      wsClickable,
      wsOpaque
      );
      
  
  TWidgetState = set of (
      wsEnabled,
      wsIsVisible,
      wsSizeIsForced,
      wsHasFocus,
      wsMouseInside,
      wsClicked
      );
      
  // The following flags are used for styles

  TButtonFlags = set of (
      btnIsEmbedded,
      btnIsDefault,
      btnIsPressed,
      btnIsSelected,
      btnHasFocus
      );


  { TWidget }

  TWidget = class(TFWindow)
  private
    FColor: TGfxColor;
    FOnClick: TNotifyEvent;
    procedure   EvOnMouseReleased(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint); virtual;
    procedure   EvOnMousePressed(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint); virtual;
    procedure   EvOnMouseLeave(Sender: TObject); virtual;
    procedure   SetColor(const AValue: TGfxColor);
  protected
    FWidgetStyle: TWidgetStyle;
    FWidgetState: TWidgetState;
    procedure   Paint; virtual;
    property    OnClick: TNotifyEvent read FOnClick write FOnClick;
    property    Color: TGfxColor read FColor write SetColor;
  public
    constructor Create(AParent: TFCustomWindow; AWindowOptions: TFWindowOptions); override;
    constructor Create(AParent: TFCustomWindow); overload;
    destructor  Destroy; override;
    procedure   ProcessEvent(AEvent: TFEvent); override;
    procedure   SetFocus;
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

procedure TWidget.EvOnMouseReleased(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  {$IFDEF DEBUG} Writeln(ClassName + '.EvOnMouseReleased'); {$ENDIF}
  if (wsClickable in FWidgetStyle) and (wsEnabled in FWidgetState) and
    (AButton = mbLeft) then
  begin
    if wsClicked in FWidgetState then
    begin
      Exclude(FWidgetState, wsClicked);
      Paint;
      if Assigned(OnClick) then
        OnClick(self);
    end;
  end;
end;

procedure TWidget.EvOnMousePressed(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  {$IFDEF DEBUG} Writeln(ClassName + '.EvOnMousePressed'); {$ENDIF}
  if (wsClickable in FWidgetStyle) and (wsEnabled in FWidgetState) and
    (AButton = mbLeft) then
  begin
    Include(FWidgetState, wsClicked);
    SetFocus;
//    Paint;
  end;
end;

procedure TWidget.EvOnMouseLeave(Sender: TObject);
begin
  Exclude(FWidgetState, wsHasFocus);
//  Paint;
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
  {$IFDEF DEBUG} Writeln(ClassName + '.Paint'); {$ENDIF}
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

  FWidgetState  := [wsEnabled];
  FColor        := colLtGray;
  Title         := ClassName;
  
  // Assign some event handlers
  OnMouseReleased   := @EvOnMouseReleased;
  OnMousePressed    := @EvOnMousePressed;
  OnMouseLeave      := @EvOnMouseLeave;
end;

constructor TWidget.Create(AParent: TFCustomWindow);
begin
  Create(AParent, [woChildWindow]);
end;

destructor TWidget.Destroy;
begin
  OnMouseReleased   := nil;
  OnMousePressed    := nil;
  inherited Destroy;
end;

procedure TWidget.ProcessEvent(AEvent: TFEvent);
begin
  inherited ProcessEvent(AEvent);
  case AEvent.EventType of
   etPaint:
     begin
       Paint;
     end;
  end;  { case }

end;

procedure TWidget.SetFocus;
begin
  Include(FWidgetState, wsHasFocus);
  Paint;
//  FindForm.FocusedWidget := Self;
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

procedure TButton.SetCaption(const AValue: string);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
  Paint;
end;

procedure TButton.Paint;
var
  Pt: TPoint;
  lFlags: TButtonFlags;
  r: TRect;
begin
  inherited Paint;
  {$IFDEF DEBUG} Writeln('  - Painting ' + Caption); {$ENDIF}
  lFlags := [];
  r := Rect(0, 0, Width, Height);

  if (wsClicked in FWidgetState) then
    Include(lFlags, btnIsPressed);
  if (wsHasFocus in FWidgetState) {and not Embedded} then
  begin
    Include(lFlags, btnIsSelected);
  end;

  { draw actual button }
  if btnIsPressed in lFlags then
  begin
    Canvas.SetColor(cl3DShadow);
    Canvas.DrawRect(r);
    Inc(r.Left);
    Inc(r.Top);
    Dec(r.Right);
    Dec(r.Bottom);
  end
  else
  begin
    Draw3DFrame(TFCanvas(Canvas), r, cl3DHighlight, cl3DLight, cl3DDkShadow, cl3DShadow);
  end;

  { draw focus rectangle }
  if (btnIsSelected in lFlags) and not (btnIsPressed in lFlags) then
  begin
    Inc(r.Left, 2);
    Inc(r.Top, 2);
    Dec(r.Right, 2);
    Dec(r.Bottom, 2);
    Canvas.SetColor(cl3DDkShadow);
    Canvas.SetLineStyle(lsDot);
    Canvas.DrawRect(r);
    Canvas.SetLineStyle(lsSolid);
  end;

  Canvas.SetColor(colBlack);
  Pt.x := (Width - Canvas.TextWidth(FCaption)) div 2;
  Pt.y := ((Height - Canvas.FontCellHeight) div 2) + 1;
  if (wsClicked in FWidgetState) {and (wsMouseInside in FWidgetState)} then
    Pt := Pt + Point(1, 1);
  Canvas.TextOut(Pt, FCaption);
end;

constructor TButton.Create(AParent: TFCustomWindow; APosition: TPoint);
begin
  inherited Create(AParent);
  Include(FWidgetStyle, wsClickable);
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
  inherited Paint;
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

