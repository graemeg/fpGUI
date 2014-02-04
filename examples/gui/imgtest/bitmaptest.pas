{
  This demo shows how you can manipulate the data of a TfpgImage
  directly.
}
program bitmaptest;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_imgfmt_bmp,
  fpg_form;

type

  TMainForm = class(TfpgForm)
  private
    img: TfpgImage;
    procedure   FormPaint(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;


{ TMainForm }

procedure TMainForm.FormPaint(Sender: TObject);
var
  i, j: integer;
begin
  if not Assigned(img) then // we only need to create the image once
  begin
    img := TfpgImage.Create;
    img.AllocateImage(32, 256, 256);
    // populate the bitmap with pretty colors :-)
    for j := 0 to 255 do
      for i := 0 to 255 do
        PLongWord(img.ImageData)[j * 256 + i] := (i shl 16) or (j shl 8);
    img.UpdateImage; // now only do we allocate OS resources
  end;
  Canvas.DrawImage(0, 0, img);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetPosition(100, 100, 256, 256);
  WindowTitle := 'fpGUI Bitmap Test';
  WindowPosition := wpOneThirdDown;
  Sizeable := False;
  OnPaint := @FormPaint;
end;

destructor TMainForm.Destroy;
begin
  img.Free;
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
  frm.Free;
end;


begin
  MainProc;
end.

