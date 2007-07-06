program bitmaptest;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_imgfmt_bmp,
  gui_form;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
  protected
    procedure HandlePaint; override;
  public
    procedure AfterCreate; override;
  end;


  { TMainForm }

  procedure TMainForm.AfterCreate;
  begin
    SetPosition(100, 100, 256, 256);
    WindowTitle := 'fpGUI2 Bitmap Test';
  end;

  procedure TMainForm.HandlePaint;
  var
    img: TfpgImage;
    i, j: integer;
  begin
    Canvas.BeginDraw; // activate double buffering in time.
    inherited HandlePaint;
    
    img := TfpgImage.Create;
    img.AllocateImage(32, 256, 256);
    img.UpdateImage;
    // populate the bitmap with pretty colors :-)
    for j := 0 to 255 do
      for i := 0 to 255 do
        PLongWord(img.ImageData)[j * 256 + i] := (i shl 16) or (j shl 8);

    Canvas.DrawImage(0, 0, img);
    img.Free;
    Canvas.EndDraw;
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

