program themetest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_scrollbar,
  fpg_button,
  fpg_label,
  fpg_imgfmt_bmp,
  fpg_extinterpolation,
  fpg_trackbar,
  fpg_styler,
  fpg_dialogs;

type
  { Note:
    I am only creating new classes to test my drawing routines in HandlePaint.
    The final themeing will be done inside the standard gui TfpgXXXX classes. }

  { Concept theme button }
  TThemeButton = class(TfpgButton)
  private
    FMasked: Boolean;
    FThemeBorder: Integer;
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
    property    Masked: Boolean read FMasked write FMasked default False;
    property    ThemeBorder: Integer read FThemeBorder write FThemeBorder default 3;
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
    FStartBtnPressed: Boolean;
    FEndBtnPressed: Boolean;
    procedure   SetThemeImage(const AValue: TfpgImage);
  protected
    procedure   HandlePaint; override;
    procedure   DrawSlider(recalc: boolean); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleMouseExit; override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    { this property is only for demo purposes! }
    property    ThemeImage: TfpgImage read image write SetThemeImage;
  end;
  
  
  { A button using the TfpgCommonStyle descendants }
  TStyledButton = class(TfpgButton)
  private
    FStyle: TfpgBaseStyle;
  protected
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;


  TMainForm = class(TfpgForm)
  private
    btnClose: TfpgButton;
    lblLuna: TfpgLabel;
    lblSilver: TfpgLabel;
    xpluna: TThemeButton;
    xpsilver: TThemeButton;
    styledbutton: TStyledButton;
    sbluna: TThemeScrollbar;
    sbsilver: TThemeScrollbar;
    sblunaHor: TThemeScrollbar;
    sbsilverHor: TThemeScrollbar;
    trackbar: TfpgTrackBar;
    lblTrackBar: TfpgLabel;
    FIndex: integer;
    procedure   TrackBarChange(Sender: TObject; APosition: integer);
    procedure   btnCloseClick(Sender: TObject);
    procedure   ButtonClicked(Sender: TObject);
    procedure   CreateButtons;
    procedure   CreateScrollbars;
  protected
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TStyledButton }

procedure TStyledButton.HandlePaint;
var
  buttonoptions: TfpgButtonStyleOption;
  tx, ty, ix, iy: integer;
  offset: integer;
  img: TfpgImage;
  r: TfpgRect;
  lTextFlags: TfpgTextFlags;
begin
  writeln('TStyledButton.HandlePaint');
  Canvas.BeginDraw;
  
  Canvas.Clear(clButtonFace);
  Canvas.ClearClipRect;

  lTextFlags := [];

  // Setup all button options that we need
  buttonoptions := TfpgButtonStyleOption.Create;
  buttonoptions.Rect.SetRect(0, 0, Width, Height);
  buttonoptions.StyleOption := soButton;
  buttonoptions.State := [];
  buttonoptions.ButtonFeatures := [];
  
  if Enabled then
    buttonoptions.State := buttonoptions.State + [stEnabled];

  if FDown then
    buttonoptions.State := buttonoptions.State + [stLowered]
  else
    buttonoptions.State := buttonoptions.State + [stRaised];

  if FFocused then
    buttonoptions.State := buttonoptions.State + [stHasFocus];

  if FEmbedded then
    buttonoptions.ButtonFeatures := buttonoptions.ButtonFeatures + [bfEmbedded];

  if FDefault then
    buttonoptions.ButtonFeatures := buttonoptions.ButtonFeatures + [bfDefault];

  // Now let the Style do ALL the drawing. Nothing must be done here!
  FStyle.DrawControl(cePushButtonBevel, buttonoptions, Canvas, self);
  FStyle.DrawPrimitive(peFocusRectangle, buttonoptions, Canvas, self);

  if FDown then
    offset := 1
  else
    offset := 0;
  CalculatePositions(ix, iy, tx, ty);

  if ShowImage and Assigned(FImage) then
  begin
    if Enabled then
      Canvas.DrawImage(ix+offset, iy+offset, FImage)
    else
    begin
      img := FImage.CreateDisabledImage;
      Canvas.DrawImage(ix+offset, iy+offset, img);
      img.Free;
    end;
  end;

  { EXPERIMENTAL: multi-line button text
      Only in this condition do we support multi-line text }
  if AllowMultiLineText and (ImageLayout = ilImageLeft) then
  begin
    r := buttonoptions.Rect;
    InflateRect(r, -3, -3);   { same as focus rectangle }
    if ShowImage and Assigned(FImage) then
    begin
      ix := ImageMargin + FImage.Width;
      if ImageSpacing > 0 then
        ix += ImageSpacing;
      OffsetRect(r, ix, 0);
      r.Width -= ix;
    end;

    if FDown then
     OffsetRect(r, offset, offset);

    lTextFlags := [txtHCenter, txtVCenter{, txtWrap}];
    if not Enabled then
      lTextFlags += [txtDisabled];

//    Canvas.DrawText(r, Text, lTextFlags);
    buttonoptions.Rect := r;
  end
  else
  begin
    buttonoptions.Rect.Left := tx+offset;
    buttonoptions.Rect.Top := ty+offset;
//    fpgStyle.DrawString(Canvas, tx+pofs, ty+pofs, Text, Enabled);
  end;

  FStyle.DrawControl(cePushButtonLabel, buttonoptions, Canvas, self);

  buttonoptions.Free;
  Canvas.EndDraw;
end;

constructor TStyledButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyle := TfpgWin2000Style.Create;
end;

destructor TStyledButton.Destroy;
begin
  FStyle.Free;
  inherited Destroy;
end;



  procedure PaintPartScaledImage(Image: TfpgImage; Canvas: TfpgCanvas; x, y: TfpgCoord; OrigWidth, OrigHeight: TfpgCoord; NewWidth, NewHeight: TfpgCoord; Border: TfpgCoord; ImgIndex: integer; Masked: Boolean = False);
  var
    rect: TfpgRect;
    img: TfpgImage;
    part: TfpgImage;
  begin
    // Get correct image
    rect.SetRect(OrigWidth * ImgIndex, 0, OrigWidth, OrigHeight);
    img := image.ImageFromRect(rect);

    //  Painting Anti-Clockwise
    // top-left
    rect.SetRect(0, 0, Border, Border);
    part := img.ImageFromRect(rect);
    if Masked then
    begin
      part.CreateMaskFromSample(0, 0);
      part.UpdateImage;
    end;
    Canvas.DrawImage(x, y, part);

    // left
    rect.SetRect(0, Border, Border, OrigHeight-(Border*2));
    part := img.ImageFromRect(rect);
    Canvas.StretchDraw(x, y+Border, Border, NewHeight-(Border*2), part);

    // bottom-left
    rect.SetRect(0, OrigHeight-Border, Border, Border);
    part := img.ImageFromRect(rect);
    if Masked then
    begin
      part.CreateMaskFromSample(0, Border-1);
      part.UpdateImage;
    end;
    Canvas.DrawImage(x, y+(NewHeight-Border), part);

    // bottom
    rect.SetRect(Border, OrigHeight-Border, OrigWidth-(Border*2), Border);
    part := img.ImageFromRect(rect);
    Canvas.StretchDraw(x+Border, y+(NewHeight-Border), NewWidth-(Border*2), Border, part);

    // bottom-right
    rect.SetRect(OrigWidth-Border, OrigHeight-Border, Border, Border);
    part := img.ImageFromRect(rect);
    if Masked then
    begin
      part.CreateMaskFromSample(Border-1, Border-1);
      part.UpdateImage;
    end;
    Canvas.DrawImage(x+(NewWidth-Border), y+(NewHeight-Border), part);

    // right
    rect.SetRect(OrigWidth-Border, Border, Border, OrigHeight-(Border*2));
    part := img.ImageFromRect(rect);
    Canvas.StretchDraw(x+(NewWidth-Border), y+Border, Border, NewHeight-(Border*2), part);

    // top-right
    rect.SetRect(OrigWidth-Border, 0, Border, Border);
    part := img.ImageFromRect(rect);
    if Masked then
    begin
      part.CreateMaskFromSample(Border-1, 0);
      part.UpdateImage;
    end;
    Canvas.DrawImage(x+(NewWidth-Border), y, part);

    // top
    rect.SetRect(Border, 0, OrigWidth-(Border*2), Border);
    part := img.ImageFromRect(rect);
    Canvas.StretchDraw(x+Border, y, NewWidth-(Border*2), Border, part);

    // client area
    rect.SetRect(Border, Border, OrigWidth-(Border*2), OrigHeight-(Border*2));
    part := img.ImageFromRect(rect);
    Canvas.StretchDraw(x+Border, y+Border, NewWidth-(Border*2), NewHeight-(Border*2), part);

    part.Free;
    img.Free;
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
  part: TfpgImage;
  r: TRect;
begin
  TopRect.SetRect(0, 0, Width, Width);
  BottomRect.SetRect(0, Height-Width, Width, Width);

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
    PaintPartScaledImage(image, Canvas, 0, 0, 32, 21, Width, Width, 3, state);
    PaintPartScaledImage(image, Canvas, 0, Height-Width, 32, 21, Width, Width, 3, state);
  end
  else
  begin
    PaintPartScaledImage(image, Canvas, 0, 0, 32, 21, Height, Height, 3, state+4);
    PaintPartScaledImage(image, Canvas, Width-Height, 0, 32, 21, Height, Height, 3, state+4);

    DrawButton(0, 0, Height, Height, 'sys.sb.left', FStartBtnPressed);
    DrawButton(Width - Height, 0, Height, Height, 'sys.sb.right', FEndBtnPressed);
  end;

  DrawSlider(True);

  Canvas.EndDraw;
end;

procedure TThemeScrollbar.DrawSlider(recalc: boolean);
begin
  inherited DrawSlider(recalc);
{
  // Paint the slider button
  if Orientation = orVertical then
  begin
    PaintPartScaledImage(Image, Canvas, 0, Width+FSliderPos, 32, 21, Width, FSliderLength, 3, 4);
//    Canvas.EndDraw(0, Width, Width, Height - Width - Width);
  end
  else
  begin
    PaintPartScaledImage(Image, Canvas, Height+FSliderPos, 0, 32, 21, FSliderLength, Height, 3, 0);
//    Canvas.EndDraw(Height, 0, Width - Height - Height, Height);
  end;
  Canvas.EndDraw;
}
end;

procedure TThemeScrollbar.HandleMouseMove(x, y: integer; btnstate: word;
  shiftstate: TShiftState);
var
  Pt: TPoint;
  NewState: Integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);
//  exit;
  
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

procedure TThemeScrollbar.HandleMouseExit;
begin
  State := 0;
  Repaint;
  inherited HandleMouseExit;
end;

procedure TThemeScrollbar.HandleLMouseDown(x, y: integer;
  shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  if PtInRect(TopRect, Point(x,y)) then
  begin
    State := 2;
    Repaint;
  end;
end;

procedure TThemeScrollbar.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  if PtInRect(TopRect, Point(x,y)) then
  begin
    State := 1;
    Repaint;
  end;
end;

constructor TThemeScrollbar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 17;
  State := 0;
  image := LoadImage_BMP(SetDirSeparators('../../../images/themes/luna/scrollbar.bmp'));
//  image.CreateMaskFromSample(0, 0);
  image.UpdateImage;
  
  Canvas.InterpolationFilter := TBilinearInterpolation.Create;
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
  ow, oh: integer;
begin
  Canvas.BeginDraw;
//  inherited HandlePaint;
  Canvas.ClearClipRect;
  Canvas.Clear(clButtonFace);
  r.SetRect(0, 0, Width, Height);

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

  ow := image.Width div 5;  // 5 states
  oh := image.Height;
  PaintPartScaledImage(image, Canvas, 0, 0, ow, oh, Width, Height, FThemeBorder, state, FMasked);
(*
  x := 0;
  { left }
  Canvas.DrawImagePart(x, 0, image, state*32, 0, 3, 21);
  { body }
  for i := (x+3) to (x+3+69) do
    Canvas.DrawImagePart(i, 0, image, (state*32)+3, 0, 1, 21);
  { right }
  Canvas.DrawImagePart(i, 0, image, (state*32)+29, 0, 3, 21);
*)
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

  InflateRect(r, 2, 2);
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

  Canvas.TextColor := TextColor;
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
    if not FDown then
      State := 1
    else
      State := 2;
    Repaint;
  end;
end;

constructor TThemeButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 75;
  Height := 21;
  State := 0;
  FMasked := False;
  FThemeBorder := 3;

  image := LoadImage_BMP(SetDirSeparators('../../../images/themes/luna/button.bmp'));
  if not Assigned(image) then
     writeln('Image is nil');
end;

destructor TThemeButton.Destroy;
begin
  image.Free;
  inherited Destroy;
end;


{ TMainForm }

procedure TMainForm.TrackBarChange(Sender: TObject; APosition: integer);
begin
  lblTrackBar.Text := IntToStr(APosition);
  RePaint;
end;

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ButtonClicked(Sender: TObject);
begin
  ShowMessage('Hello world!');
end;

procedure TMainForm.CreateButtons;
var
  bmp: TfpgImage;
  vista: TThemeButton;
begin
  btnClose := TfpgButton.Create(self);
  btnClose.Width      := 80;
  btnClose.Left       := Width - btnClose.Width - 6;
  btnClose.Top        := Height - btnClose.Height - 6;
  btnClose.Text       := 'Quit';
  btnClose.ImageName  := 'stdimg.Quit';
  btnClose.ShowImage  := True;
  btnClose.OnClick    := @btnCloseClick;

  xpluna := TThemeButton.Create(self);
  xpluna.Left         := 80;
  xpluna.Top          := 45;
  xpluna.Width        := 85;
  xpluna.Height       := 28;
  xpluna.Text         := 'XP Luna (-)';
  xpluna.Masked := True;
  xpluna.OnClick := @ButtonClicked;

  xpsilver := TThemeButton.Create(self);
  xpsilver.Left       := 230;
  xpsilver.Top        := 45;
  xpsilver.Width      := 85;
  xpsilver.Height     := 28;
  xpsilver.Text       := 'XP Silver (+)';
  bmp := LoadImage_BMP(SetDirSeparators('../../../images/themes/silver/button.bmp'));
  xpsilver.ThemeImage := bmp;
  xpsilver.Masked := True;

  vista := TThemeButton.Create(self);
  vista.Left         := 20;
  vista.Top          := 145;
  vista.Width        := 101;  //75;
  vista.Height       := 41;   //24;
  vista.TextColor    := clWhite;
  vista.FontDesc     := 'Arial-10:bold';
  vista.Text         := 'Vista';
  bmp := LoadImage_BMP(SetDirSeparators('../../../images/themes/vista/button.bmp'));
  vista.ThemeImage := bmp;

  styledbutton := TStyledButton.Create(self);
  styledbutton.SetPosition(btnClose.Left, btnClose.Top-80, 80, 24);
//      styledbutton.Default := True;
  styledbutton.ImageName := 'stdimg.quit';
  styledbutton.ShowImage := True;
  styledbutton.Text := 'Styled';
end;

procedure TMainForm.CreateScrollbars;
var
  bmp: TfpgImage;
begin
  bmp := LoadImage_BMP(SetDirSeparators('../../../images/themes/silver/scrollbar.bmp'));
  bmp.UpdateImage;

  sbluna := TThemeScrollBar.Create(self);
  with sbluna do
  begin
    SetPosition(130, 80, 24, 100);
    Max := 15;
  end;

  sbsilver          := TThemeScrollBar.Create(self);
  sbsilver.Top      := 80;
  sbsilver.Left     := 310;
  sbsilver.Height   := 100;
  sbsilver.Max      := 15;
  sbsilver.ThemeImage := bmp;
{
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
}
end;

procedure TMainForm.HandlePaint;
var
  image: TfpgImage;
  img: TfpgImage;
  r: TRect;
  r2: TfpgRect;
  x, y: TfpgCoord;
  nr: TfpgRect;
  

begin
  Canvas.BeginDraw;
  inherited HandlePaint;
  
  
  image := LoadImage_BMP(SetDirSeparators('../../../images/themes/luna/scrollbar.bmp'));
//  image.CreateMaskFromSample(0, 0);
  image.UpdateImage;

  Canvas.InterpolationFilter := TfpgMitchelInterpolation.Create;
  Canvas.StretchDraw(0, 0, Width, 21, image);

  Canvas.InterpolationFilter := TBilinearInterpolation.Create;
  Canvas.StretchDraw(0, 23, Width, 21, image);
  
  r.Left    := 0;
  r.Top     := 0;
  r.Right   := 32;
  r.Bottom  := 21;
  img := image.ImageFromRect(r);    // now we have the complete widget 32x21
  // we need 17x17 size

  Canvas.DrawImage(5, 46, img);

  x := 5;
  y := 75;
  
  // left border
  //r.Left    := 0;
  //r.Top     := 0;
  //r.Right   := 2;
  //r.Bottom  := 21;
  PaintPartScaledImage(image, Canvas, x, y, 32, 21, 90, 24, 3, trackbar.Position);

  PaintPartScaledImage(image, Canvas, x, 100, 32, 21, 17, 17, 3, trackbar.Position);

  img.Free;
  
  nr.SetRect(20, 250, 100, 4);
  Canvas.DrawControlFrame(nr);
  nr.SetRect(70, 241, 11, 21);
  Canvas.DrawButtonFace(nr, []);


  Canvas.EndDraw;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Theme test';
  SetPosition(100, 100, 400, 300);
  
  // image index
  FIndex := 0;
  
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
  
  lblTrackBar := CreateLabel(self, 190, 265, '--');
  
  trackbar := TfpgTrackBar.Create(self);
  trackbar.Width := 150;
  trackbar.Orientation := orHorizontal;
  trackbar.Min := 0;
  trackbar.Max := 8;
  trackbar.Top := 265;
  trackbar.Left := 20;
  trackbar.Position := 0;
  trackbar.OnChange := @TrackBarChange;
  trackbar.ShowPosition := True;
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

