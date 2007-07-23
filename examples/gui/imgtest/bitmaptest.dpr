program bitmaptest;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_imgfmt_bmp;

type

  TMainForm = class(TfpgWindow)
  private
    img: TfpgImage;
    procedure   MsgPaint(var msg: TfpgMessageRec); message FPGM_PAINT;
    procedure   MsgClose(var msg: TfpgMessageRec); message FPGM_CLOSE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Show;
  end;


{ TMainForm }

procedure TMainForm.MsgPaint(var msg: TfpgMessageRec);
begin
  Canvas.BeginDraw;
  Canvas.DrawImage(0, 0, img);
  Canvas.EndDraw;
end;

procedure TMainForm.MsgClose(var msg: TfpgMessageRec);
begin
  ReleaseWindowHandle;
  Halt(0);
end;

constructor TMainForm.Create(AOwner: TComponent);
var
  i, j: integer;
begin
  inherited Create(AOwner);
  FWidth    := 256;
  FHeight   := 256;
  WindowAttributes := [waScreenCenterPos];
  
  img := TfpgImage.Create;
  img.AllocateImage(32, 256, 256);
  img.UpdateImage;
  // populate the bitmap with pretty colors :-)
  for j := 0 to 255 do
    for i := 0 to 255 do
      PLongWord(img.ImageData)[j * 256 + i] := (i shl 16) or (j shl 8);
end;

destructor TMainForm.Destroy;
begin
  img.Free;
  inherited Destroy;
end;

procedure TMainForm.Show;
begin
  AllocateWindowHandle;
  // We can only set the title once we have a window handle.
  SetWindowTitle('fpGUI Bitmap Test');
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

