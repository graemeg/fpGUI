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
  end;


  TMainForm = class(TfpgForm)
  private
    lblLuna: TfpgLabel;
    lblSilver: TfpgLabel;
  public
    constructor Create(AOwner: TComponent); override;
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

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Theme test';
  SetPosition(100, 100, 400, 300);
  
  lblLuna := CreateLabel(self, 100, 5, 'Luna');
  lblLuna.FontDesc := 'Sans-12:bold:underline';
  lblLuna.Height := lblLuna.Font.Height;
  lblLuna.Width := lblLuna.Font.TextWidth(lblLuna.Text);
  
  lblSilver := CreateLabel(self, 250, 5, 'Silver');
  lblSilver.FontDesc := 'Sans-12:bold:underline';
  lblSilver.Height := lblSilver.Font.Height;
  lblSilver.Width := lblSilver.Font.TextWidth(lblSilver.Text);
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

