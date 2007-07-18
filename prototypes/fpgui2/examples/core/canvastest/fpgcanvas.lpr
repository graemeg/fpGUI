program fpgcanvas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  fpgfx,
  gfxbase,
  gui_form,
  gfx_imgfmt_bmp;


type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    bmp: TfpgImage;
    dst: TfpgImage;
  protected
    procedure HandlePaint; override;
  public
    procedure AfterCreate; override;
    procedure BeforeDestruction; override;
  end;

{ TMainForm }

procedure TMainForm.HandlePaint;
var
  r: TfpgRect;
  fnt: TfpgFont;
  y: integer;
  c: TfpgColor;
begin
  // Enable double buffering. Must be before 'inherited' to prevent form
  // clearing itself.
  Canvas.BeginDraw;
  inherited HandlePaint;

  // Testing Rectangles
  Canvas.SetColor(clBlack);
  r.Top       := 5;
  r.Left      := 60;
  r.Width     := 50;
  r.Height    := 50;
  Canvas.DrawRectangle(r);
  
  r.Left := 120;
  Canvas.SetLineStyle(2, lsDash);
  Canvas.DrawRectangle(r);

  r.Left := 180;
  Canvas.SetColor(clGreen);
  Canvas.SetLineStyle(1, lsDot);
  Canvas.DrawRectangle(r);

  r.Left := 240;
  Canvas.SetColor(clBlue);
  Canvas.SetLineStyle(1, lsSolid);
  Canvas.FillRectangle(r);


  // Testing line drawing
  Canvas.DrawLine(5, 5, 50, 50);
  Canvas.SetColor(clBlue);
  Canvas.DrawLine(50, 5, 5, 50);
  Canvas.SetColor(clRed);
  { Diagonal line }
  Canvas.DrawLine(60, 5, 110, 55);
  { Horizontal line }
  Canvas.DrawLine(60, 3, 110, 3);
  { Vertical line }
  Canvas.DrawLine(58, 5, 58, 55);


  // Testing Text and Fonts
  y := 60;  
  Canvas.SetTextColor(clBlack);
  Canvas.DrawString(5, y, 'This text must be black and default font (' + fpgApplication.DefaultFont.FontDesc + ')');
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
  Canvas.DrawButtonFace(300, 20, 75, 25, []);
  Canvas.DrawString(385, 20, '= []');
  Canvas.DrawButtonFace(300, 50, 75, 25, [btnIsDefault]);
  Canvas.DrawString(385, 50, '= [btnIsDefault]');
  Canvas.DrawButtonFace(300, 80, 75, 25, [btnIsPressed]);
  Canvas.DrawString(385, 80, '= [btnIsPressed]');
  Canvas.DrawButtonFace(300, 110, 75, 25, [btnIsEmbedded, btnIsPressed]);
  Canvas.DrawString(385, 110, '= [embed & press]');
  Canvas.DrawButtonFace(300, 140, 75, 25, [btnIsEmbedded]);
  Canvas.DrawString(385, 140, '= [btnIsEmbedded]');
  
  Canvas.DrawString(45, y, 'DrawControlFrame():');
  y := y + Canvas.Font.Height;
  Canvas.DrawControlFrame(5, y, 200, 23);
  
  
  // Testing Bitmap painting
  Canvas.DrawString(5, 180, 'Single BMP file:');
  Canvas.DrawString(300, 210, '(mask enabled for all images)');
  Canvas.DrawImage(150, 180, bmp);
  Canvas.DrawString(5, 210, 'Parts of BMP file:');
  Canvas.DrawImagePart(150, 210, bmp, 0, 0, 32, 21);
  Canvas.DrawImagePart(190, 210, bmp, 32, 0, 32, 21);
  Canvas.DrawImagePart(230, 210, bmp, 64, 0, 32, 21);
  
//  Canvas.StretchDraw(150, 240, 160, 21, bmp);
  Canvas.StretchDraw(150, 240, 300, 50, bmp);
  
  Canvas.DrawImage(150, 300, dst);
  Canvas.StretchDraw(180, 300, 70, 70, dst);
  Canvas.StretchDraw(265, 300, 230, 25, bmp);

//  Stretch(bmp, dst, ResampleFilters[6].Filter, ResampleFilters[6].Width);
////  Canvas.DrawImage(150, 240, bmp);
//  Canvas.DrawImage(160, 250, dst);

  
  
  // Testing Canvas.Pixels[]
  // two pixels should have changed color in the top left of the form
  Canvas.Pixels[7,5] := clBlue;   // This tests consistant bit order handling (RGB)
  Canvas.Pixels[8,5] := clRed;
  c := Canvas.Pixels[192, 227];   // should be orange like color
  Canvas.Pixels[9,5] := c;
  c := Canvas.Pixels[150 + (32*4) + 3, 199];   // should be lightblue like color
  Canvas.Pixels[10,5] := c;

  Canvas.EndDraw;
end;

procedure TMainForm.AfterCreate;
begin
  inherited AfterCreate;
  SetPosition(100, 100, 500, 400);
  WindowTitle := 'fpGFX Canvas Test';

  bmp := LoadImage_BMP('button.bmp');
  if not Assigned(bmp) then
    raise Exception.Create('Failed to load button.bmp');
  bmp.CreateMaskFromSample(0,0);
  bmp.UpdateImage;
  
//  dst := TfpgImage.Create;
//  dst.AllocateImage(bmp.ColorDepth, 200, 50);
  dst := LoadImage_BMP('gears2.bmp');
  dst.CreateMaskFromSample(0,0);
  dst.UpdateImage;
end;

procedure TMainForm.BeforeDestruction;
begin
  dst.Free;
  bmp.Free;
  inherited BeforeDestruction;
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

