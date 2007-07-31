program drawtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpgfx,
  gfxbase,
  gui_form,
  gfx_imgfmt_bmp;
  
type

  TMainForm = class(TfpgForm)
  private
    img: TfpgImage;
    finalimg: TfpgImage;
  protected
    procedure   HandlePaint; override;
    procedure   HandleShow; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

{ TMainForm }

procedure TMainForm.HandlePaint;
var
  r: TfpgRect;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;
  Canvas.Clear(clSteelBlue);

  Canvas.DrawString(10, 10, 'Original 50x50 image:');
  Canvas.DrawImage(200, 10, img);
  Canvas.DrawString(10, 60, 'All line and pixel drawing must only appear');
  Canvas.DrawString(10, 60+Canvas.Font.Height, 'on black rectangles. Watch for line start and');
  Canvas.DrawString(10, 60+(Canvas.Font.Height*2), 'ending points.');

  // final image
  Canvas.DrawString(10, 195-Canvas.Font.Height, 'The final images should look line this:');
  Canvas.DrawImage(100, 200, finalimg);

  // blank rectangle image
  Canvas.DrawImage(100, 100, img);

  // rectangle
  Canvas.SetColor(clRed);
  Canvas.DrawRectangle(102, 102, 147, 147);  // (over second outer black rectangle)
  Canvas.SetColor(clMagenta);
  Canvas.DrawRectangle(104, 104, 145, 145);  // (over third outer black rectangle)

  // Point (outer 4 corners)
  Canvas.Pixels[100, 100] := clBlue;
  Canvas.Pixels[149, 100] := clBlue;
  Canvas.Pixels[100, 149] := clBlue;
  Canvas.Pixels[149, 149] := clBlue;
  
  // FillRect
  Canvas.SetColor(clYellow);
  Canvas.FillRectangle(111, 111, 138, 138);  // inner white rectangle

  // line diagonal
  Canvas.SetColor(clRed);
  Canvas.DrawLine(110, 110, 139, 139);  // top/left of inner black rect to bottom/right
  Canvas.SetColor(clGreen);
  Canvas.DrawLine(139, 110, 110, 139);  // top/right of inner black rect to bottom/left

  // line vertical
  Canvas.SetColor(clBlue);
  Canvas.DrawLine(108, 108, 108, 141);
  Canvas.DrawLine(141, 108, 141, 141);

  // line horizontal
  Canvas.SetColor(clGreen);
  Canvas.DrawLine(106, 106, 143, 106);
  Canvas.DrawLine(106, 143, 143, 143);


  Canvas.EndDraw;
end;

procedure TMainForm.HandleShow;
begin
  img := LoadImage_BMP('square.bmp');
  finalimg := LoadImage_BMP('drawtest.bmp');
  inherited HandleShow;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Draw test';
  SetPosition(100, 100, 300, 300);

end;

destructor TMainForm.Destroy;
begin
  img.Free;
  finalimg.Free;
  inherited Destroy;
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

