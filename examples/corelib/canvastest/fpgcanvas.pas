program fpgcanvas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_imgfmt_bmp;


type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    {@VFD_HEAD_END: MainForm}
    bmp: TfpgImage;
    dst: TfpgImage;
    procedure   FormPaint(Sender: TObject);
    procedure   CustomPaintJob;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{ TMainForm }

procedure TMainForm.FormPaint(Sender: TObject);
begin
  CustomPaintJob;
end;

// We can now call all the paint methods from HandlePaint or
// the OnPaint event.fpgcanvas
procedure TMainForm.CustomPaintJob;
var
  r: TfpgRect;
  fnt: TfpgFont;
  y: integer;
  c: TfpgColor;
  lImage: TfpgImage;
begin
  // Testing Rectangles
  Canvas.SetColor(clBlack);
  r.SetRect(0, 0, 1, 1);     // 1x1  (this is really a dot)
  Canvas.DrawRectangle(r);
  Canvas.SetColor(clRed);
  r.SetRect(0, 1, 1, 5);    // 1x5  (this is really a vertical line)
  Canvas.DrawRectangle(r);
  Canvas.SetColor(clMagenta);
  r.SetRect(1, 0, 5, 1);    // 5x1  (this is really a horizontal line)
  Canvas.DrawRectangle(r);

  Canvas.SetColor(clBlack);
  r.Top       := 5;
  r.Left      := 60;
  r.Width     := 50;
  r.Height    := 50;
  Canvas.DrawRectangle(r);

  r.Left      := 120;
  Canvas.SetLineStyle(2, lsDash);
  Canvas.DrawRectangle(r);

  r.Left      := 180;
  Canvas.SetColor(clGreen);
  Canvas.SetLineStyle(1, lsDot);
  Canvas.DrawRectangle(r);

  r.Left      := 240;
  Canvas.SetColor(clBlue);
  Canvas.SetLineStyle(1, lsSolid);
  Canvas.FillRectangle(r);


  // Testing line drawing
  Canvas.DrawLine(5, 5, 54, 54);
  Canvas.SetColor(clBlue);
  Canvas.DrawLine(54, 5, 5, 54);
  Canvas.SetColor(clRed);
  { Diagonal line }
  r.SetRect(60, 5, 50, 50);
  Canvas.DrawLine(r.Left, r.Top, r.Right, r.Bottom);
  { Horizontal line }
  Canvas.DrawLine(r.Left, r.Top-2, r.Right, r.Top-2);
  { Vertical line }
  Canvas.DrawLine(r.Left-2, r.Top, r.Left-2, r.Bottom);


  // Testing Text and Fonts
  y := 60;
  Canvas.SetTextColor(clBlack);
  Canvas.DrawString(5, y, 'This text must be black and default font (' + fpgApplication.DefaultFont.FontDesc + ')');

  // red dot indicates top/left corner of where previous text was started
  Canvas.Pixels[5,y] := clRed;
//  Canvas.DrawLine(5,y-4, 5,y+5);
//  Canvas.DrawLine(1,y, 10, y);

  Canvas.SetTextColor(clRed);
  y := y + Canvas.Font.Height;  // fonts are different sizes on differet OS's
  Canvas.DrawString(5, y, 'This text must be red.');
  Canvas.SetTextColor(clBlack);
  y := y + Canvas.Font.Height;
  Canvas.DrawString(5, y, 'Russian (UTF-8) text -> Òåñò');
  y := y + Canvas.Font.Height;
  fnt := fpgApplication.GetFont('Times-14:bold');
  Canvas.Font := fnt;
  Canvas.DrawString(5, y, 'Font used is ' + Canvas.Font.FontDesc);
  y := y + Canvas.Font.Height;


  // Testing basic style drawings
  Canvas.Font := fpgApplication.DefaultFont;
  Canvas.DrawString(320, 3, 'DrawButtonFace():');

  r.SetRect(300, 20, 75, 25);
  Canvas.DrawButtonFace(r, []);
  Canvas.DrawString(385, 20, '= []');
  r.Top := 50;
  Canvas.DrawButtonFace(r, [btfIsDefault]);
  Canvas.DrawString(385, 50, '= [btnIsDefault]');
  r.Top := 80;
  Canvas.DrawButtonFace(r, [btfIsPressed]);
  Canvas.DrawString(385, 80, '= [btnIsPressed]');
  r.Top := 110;
  Canvas.DrawButtonFace(r, [btfIsEmbedded, btfIsPressed]);
  Canvas.DrawString(385, 110, '= [embed & press]');
  r.Top := 140;
  Canvas.DrawButtonFace(r, [btfIsEmbedded]);
  Canvas.DrawString(385, 140, '= [btnIsEmbedded]');

  Canvas.DrawString(45, y, 'DrawControlFrame():');
  y := y + Canvas.Font.Height;
  Canvas.DrawControlFrame(5, y, 200, 23);


  // Testing Bitmap painting
  Canvas.DrawString(5, 180, 'Single BMP file:');
  Canvas.DrawString(310, 210, '(mask enabled for all images)');
  Canvas.DrawImage(150, 180, bmp);
  Canvas.DrawString(5, 210, 'Parts of BMP file:');
  Canvas.DrawImagePart(150, 210, bmp, 0, 0, 32, 21);
  Canvas.DrawImagePart(190, 210, bmp, 32, 0, 32, 21);
  Canvas.DrawImagePart(230, 210, bmp, 64, 0, 32, 21);
  // create image from an image
  r.SetRect(32, 0, 32, 21); // second button in image
  lImage := bmp.ImageFromRect(r);
  try
    lImage.CreateMaskFromSample(0, 0);
    lImage.UpdateImage;
    Canvas.DrawImage(270, 215, lImage);
  finally
    lImage.Free;
  end;


  // Testing Bitmap strechdraw
  Canvas.StretchDraw(150, 240, 300, 50, bmp);
  Canvas.DrawImage(150, 300, dst);
  Canvas.StretchDraw(180, 300, 70, 70, dst);
  Canvas.StretchDraw(265, 300, 230, 25, bmp);


  // testing accuracy of line/rectangle drawing
  Canvas.SetColor(clBlack);
  Canvas.SetLineStyle(1, lsSolid);
  r.SetRect(182, 302, 66, 66);
  Canvas.DrawRectangle(r);

  Canvas.SetColor(clBlue);
  Canvas.DrawLine(183, 303, 247, 303);


  // Testing Canvas.Pixels[]
  // two pixels should have changed color in the top left of the form
  Canvas.Pixels[7,5] := clBlue;   // This tests consistant bit order handling (RGB)
  Canvas.Pixels[8,5] := clRed;
  c := Canvas.Pixels[192, 227];   // should be orange like color
  Canvas.Pixels[9,5] := c;
  c := Canvas.Pixels[150 + (32*4) + 3, 199];   // should be lightblue like color
  Canvas.Pixels[10,5] := c;


  // Arc drawing tests
  Canvas.SetColor(clBlack);
  Canvas.DrawRectangle(5, 235, 50, 50);
  Canvas.SetColor(clRed);
  Canvas.DrawArc(5, 235, 50, 50, 0, 270);  // should overlap rectangle pixels
  Canvas.SetColor(clBlack);
  Canvas.DrawRectangle(5, 290, 50, 50);
  Canvas.SetColor(clRed);
  Canvas.FillArc(5, 290, 50, 50, 0, 270);  // should overlap rectangle pixels


  // Gradient testing
  r.SetRect(265, 340, 185, 35);
  Canvas.GradientFill(r, clBlue, clMagenta, gdHorizontal);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  bmp := LoadImage_BMP('button.bmp');
  if not Assigned(bmp) then
    raise Exception.Create('Failed to load button.bmp');
  bmp.CreateMaskFromSample(0,0);
  bmp.UpdateImage;

  dst := LoadImage_BMP('gears2.bmp');
  dst.CreateMaskFromSample(0,0);
  dst.UpdateImage;
end;

destructor TMainForm.Destroy;
begin
  dst.Free;
  bmp.Free;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(357, 214, 500, 400);
  WindowTitle := 'fpGUI Canvas Test';
  Hint := '';
  WindowPosition := wpOneThirdDown;
  OnPaint := @FormPaint;

  {@VFD_BODY_END: MainForm}
  {%endregion}
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
  frm.Free;
end;


begin
  MainProc;
end.

