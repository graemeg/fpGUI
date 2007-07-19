program themetest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  fpgfx,
  gfxbase,
  gui_form,
  gui_scrollbar,
  gui_button,
  gui_label,
  gfx_imgfmt_bmp;

type
  { Note:
    I am only creating new classes to test my drawing routines in HandlePaint.
    The final themeing will be done inside the TfpgXXXX classes. }

  { Concept theme button }
  TThemeButton = class(TfpgButton)
  private
    State: integer;
      // 0 - normal
      // 1 - hover
      // 2 - mouse down
      // 3 - disabled
      // 4 - got focus or default
    image: TfpgImage;
    procedure   SetThemeImage(const AValue: TfpgImage);
  protected
    procedure   HandlePaint; override;
    procedure   HandleLMouseDown(X, Y: integer; ShiftState: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseExit; override;
    procedure   HandleMouseEnter; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    { this property is only for demo purposes! }
    property    ThemeImage: TfpgImage read image write SetThemeImage;
  end;


  TThemeScrollbar = class(TfpgScrollBar)
  private
    TopRect: TfpgRect;
    BottomRect: TfpgRect;
    ThumbRect: TfpgRect;
    State: integer;
      // 0 - normal
      // 1 - hover
      // 2 - mouse down
      // 3 - disabled
      // 4 - got focus or default
    image: TfpgImage;
    procedure   SetThemeImage(const AValue: TfpgImage);
  protected
    procedure   HandlePaint; override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    { this property is only for demo purposes! }
    property    ThemeImage: TfpgImage read image write SetThemeImage;
  end;


  TMainForm = class(TfpgForm)
  private
    btnClose: TfpgButton;
    lblLuna: TfpgLabel;
    lblSilver: TfpgLabel;
    xpluna: TThemeButton;
    xpsilver: TThemeButton;
    sbluna: TThemeScrollbar;
    sbsilver: TThemeScrollbar;
    sblunaHor: TThemeScrollbar;
    sbsilverHor: TThemeScrollbar;
  private
    procedure   btnCloseClick(Sender: TObject);
    procedure   CreateButtons;
    procedure   CreateScrollbars;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TThemeScrollbar }

procedure TThemeScrollbar.SetThemeImage(const AValue: TfpgImage);
begin
  if Assigned(image) then
    image.Free;
  image := AValue;
  Repaint;
end;

procedure TThemeScrollbar.HandlePaint;
var
  imgwidth: integer;
  x: integer;
begin
  Canvas.BeginDraw;
//  inherited HandlePaint;
  Canvas.ClearClipRect;
  Canvas.Clear(clWindowBackground);

  imgwidth := image.Width div 34;
  
  //if State <> 1 then
  //begin
    //if Down then
      //State := 2
    //else if Focused then
      //State := 4
    //else if not Enabled then
      //State := 3
    //else
      //State := 0;
  //end;


  if Orientation = orVertical then
  begin
//    DrawButton(0, 0, Width, Width, 'sys.sb.up' ,FStartBtnPressed);
    { top button }
//    if Pressed then
//      Canvas.DrawButtonFace(x, y, w, h, [btnIsEmbedded, btnIsPressed])
      Canvas.DrawImagePart(0, 0, image, state*imgwidth, 0, imgwidth, 21);
//    else
//      Canvas.DrawButtonFace(x, y, w, h, [btnIsEmbedded]);

    { bottom button }
    DrawButton(0, Height - Width, Width, Width, 'sys.sb.down', FEndBtnPressed);
  end
  else
  begin
    DrawButton(0, 0, Height, Height, 'sys.sb.left', FStartBtnPressed);
    DrawButton(Width - Height, 0, Height, Height, 'sys.sb.right', FEndBtnPressed);
  end;

  DrawSlider(True);

  Canvas.EndDraw;
end;

procedure TThemeScrollbar.HandleMouseMove(x, y: integer; btnstate: word;
  shiftstate: TShiftState);
var
  Pt: TPoint;
  NewState: Integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  Pt := Point(X, Y);
  NewState := 0;
  if PtInRect(TopRect, Pt) then
    NewState := 1
  else if PtInRect(ThumbRect, Pt) then
    NewState := 2
  else if PtInRect(BottomRect, Pt) then
    NewState := 3;

  if NewState <> State then
  begin
    State := NewState;
    Repaint;
  end;
end;

constructor TThemeScrollbar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  State := 0;
  image := LoadImage_BMP(SetDirSeparators('../images/themes/luna/scrollbar.bmp'));
  image.UpdateImage;
end;

destructor TThemeScrollbar.Destroy;
begin
  image.Free;
  inherited Destroy;
end;



{ TXPButton }

procedure TThemeButton.SetThemeImage(const AValue: TfpgImage);
begin
  if Assigned(image) then
    image.Free;
  image := AValue;
  Repaint;
end;

procedure TThemeButton.HandlePaint;
var
  x, i: integer;
  r: TfpgRect;
  iy, y: integer;
  w: integer;
  pofs: integer;
begin
  Canvas.BeginDraw;
//  inherited HandlePaint;
  Canvas.ClearClipRect;
  Canvas.Clear(clButtonFace);

  if State <> 1 then
  begin
    if Down then
      State := 2
    else if Focused then
      State := 4
    else if not Enabled then
      State := 3
    else
      State := 0;
  end;

  x := 0;
  { left }
  Canvas.DrawImagePart(x, 0, image, state*32, 0, 3, 21);
  { body }
  for i := (x+3) to (x+3+69) do
    Canvas.DrawImagePart(i, 0, image, (state*32)+3, 0, 1, 21);
  { right }
  Canvas.DrawImagePart(i, 0, image, (state*32)+29, 0, 3, 21);

  if Focused and (not Embedded) then
  begin
    Canvas.SetColor(clText1);
    Canvas.SetLineStyle(1, lsDot);
    Canvas.DrawRectangle(3, 3, Width - 6, Height - 6);
  end
  else
  begin
    Canvas.SetTextColor(clText1);
    Canvas.SetColor(clText1);
  end;

  if not Enabled then
    Canvas.SetTextColor(clShadow1);

  r.left   := 2;
  r.top    := 2;
  r.Width  := Width - 4;
  r.Height := Height - 4;
  Canvas.SetClipRect(r);

  Canvas.SetFont(Font);
  y := Height div 2 - FFont.Height div 2;
  if y < 3 then
    y := 3;

  // offset text and image
  if Down then
    pofs := 1
  else
    pofs := 0;

  if (ShowImage) and (FImage <> nil) then
  begin
    iy := Height div 2 - FImage.Height div 2;
    if ImageMargin = -1 then // centered
    begin
      w := FFont.TextWidth(FText) + FImage.Width;
      if FImageSpacing > 0 then
        Inc(w, FImageSpacing);
      x := (Width div 2) - (w div 2);
      if x < 3 then
        x := 3;
    end
    else
    begin
      x := FImageMargin + 3;
    end;

    Canvas.DrawImage(x + pofs, iy + pofs, FImage);
    Inc(x, FImage.Width);
    if FImageSpacing > 0 then
      Inc(x, FImageSpacing);

    if (FImageSpacing = -1) and (FImageMargin >= 0) then
    begin
      w := (Width - 3 - x) div 2 - FFont.TextWidth(FText) div 2;
      if w < 1 then
        w := 1; // minimal spacing
      x := x + w;
    end;
  end
  else
    x := (Width div 2) - (FFont.TextWidth(FText) div 2);

  if x < 3 then
    x := 3;

  Canvas.DrawString(x + pofs, y + pofs, FText);

  Canvas.EndDraw;
end;

procedure TThemeButton.HandleLMouseDown(X, Y: integer; ShiftState: TShiftState);
begin
  State := 2;
  Repaint;
  inherited HandleLMouseDown(X, Y, ShiftState);
end;

procedure TThemeButton.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  State := 1;
  Repaint;
  inherited HandleLMouseUp(x, y, shiftstate);
end;

procedure TThemeButton.HandleMouseExit;
begin
  inherited HandleMouseExit;
  if Enabled then
  begin
    State := 0;
    Repaint;
  end;
end;

procedure TThemeButton.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  if Enabled then
  begin
    State := 1;
    Repaint;
  end;
end;

constructor TThemeButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 75;
  Height := 21;
  State := 0;

  image := LoadImage_BMP(SetDirSeparators('../images/themes/luna/button.bmp'));
  image.CreateMaskFromSample(0, 0);
  image.UpdateImage;
  if not Assigned(image) then
     writeln('Image is nil');
end;

destructor TThemeButton.Destroy;
begin
  image.Free;
  inherited Destroy;
end;


{ TMainForm }

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.CreateButtons;
var
  bmp: TfpgImage;
begin
  btnClose := TfpgButton.Create(self);
  btnClose.Width      := 75;
  btnClose.Left       := Width - btnClose.Width - 6;
  btnClose.Top        := Height - btnClose.Height - 6;
  btnClose.Text       := 'Quit';
  btnClose.ImageName  := 'stdimg.Quit';
  btnClose.ShowImage  := True;
  btnClose.OnClick    := @btnCloseClick;

  xpluna := TThemeButton.Create(self);
  xpluna.Left         := 80;
  xpluna.Top          := 45;
  xpluna.Width        := 75;
  xpluna.Text         := 'XP Luna';

  xpsilver := TThemeButton.Create(self);
  xpsilver.Left       := 230;
  xpsilver.Top        := 45;
  xpsilver.Width      := 75;
  xpsilver.Text       := 'XP Silver';
  bmp := LoadImage_BMP(SetDirSeparators('../images/themes/silver/button.bmp'));
  bmp.CreateMaskFromSample(0, 0);
  bmp.UpdateImage;
  xpsilver.ThemeImage := bmp;
end;

procedure TMainForm.CreateScrollbars;
var
  bmp: TfpgImage;
begin
  bmp := LoadImage_BMP(SetDirSeparators('../images/themes/silver/scrollbar.bmp'));
  bmp.UpdateImage;

  sbluna            := TThemeScrollBar.Create(self);
  sbluna.Top        := 80;
  sbluna.Left       := 130;
  sbluna.Height     := 100;
  sbluna.Max        := 15;

  sbsilver          := TThemeScrollBar.Create(self);
  sbsilver.Top      := 80;
  sbsilver.Left     := 310;
  sbsilver.Height   := 100;
  sbsilver.Max      := 15;
  sbsilver.ThemeImage := bmp;

  sblunaHor         := TThemeScrollBar.Create(self);
  sblunaHor.Top     := 100;
  sblunaHor.Left    := 20;
  sblunaHor.Width   := 100;
  sblunaHor.Max     := 15;
  sblunaHor.Orientation := orHorizontal;

  sbsilverHor       := TThemeScrollBar.Create(self);
  sbsilverHor.Top   := 110;
  sbsilverHor.Left  := 200;
  sbsilverHor.Width := 100;
  sbsilverHor.Max   := 15;
  sbsilverHor.Orientation := orHorizontal;
  sbsilverHor.ThemeImage  := bmp;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Theme test';
  SetPosition(100, 100, 400, 300);
  
  lblLuna := CreateLabel(self, 100, 5, 'Luna');
  lblLuna.FontDesc    := 'Sans-12:bold:underline';
  lblLuna.Height      := lblLuna.Font.Height;
  lblLuna.Width       := lblLuna.Font.TextWidth(lblLuna.Text);
  
  lblSilver := CreateLabel(self, 250, 5, 'Silver');
  lblSilver.FontDesc  := 'Sans-12:bold:underline';
  lblSilver.Height    := lblSilver.Font.Height;
  lblSilver.Width     := lblSilver.Font.TextWidth(lblSilver.Text);
  
  CreateButtons;
  CreateScrollbars;
end;
  

  
procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
end;

begin
  MainProc;
end.

