program stdimglist;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  fpgfx, gfxbase, gui_form, gfx_imgfmt_bmp, gui_button, fpgui_toolkit;

type

  TMainForm = class(TfpgForm)
  private
    btnClose: TfpgButton;
    procedure   btnCloseClick(Sender: TObject);
  protected
    procedure   HandlePaint; override;
  public
    constructor Create(aowner: TComponent); override;
    procedure   AfterCreate; override;
  end;

{ TMainForm }

procedure TMainForm.AfterCreate;
begin
  SetPosition(100,100,700,500);
  WindowTitle := 'fpGUI Standard Image Listing';
end;

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.HandlePaint;
var
  n: integer;
  x: TfpgCoord;
  y: TfpgCoord;
  sl: TStringList;
  img: TfpgImage;
begin
  Canvas.BeginDraw; // begin double buffering
  inherited HandlePaint;

  sl  := TStringList.Create;
  x   := 8;
  y   := 8;
  fpgImages.ListImages(sl);
  
  for n := 0 to sl.Count-1 do
  begin
    Canvas.DrawString(x, y, sl[n]+':');
    
    img := TfpgImage(sl.Objects[n]);
    if img <> nil then
      Canvas.DrawImage(x+130, y, img);

    inc(y, img.Height+8);
    if y > Height-32 then // largest images are 32 in height
    begin
      inc(x, 200);
      y := 8;
    end;
  end;

  Canvas.EndDraw;
  sl.Free;
end;

constructor TMainForm.Create(aowner: TComponent);
begin
  inherited Create(aowner);
  // Place button in bottom right corner.
  btnClose := CreateButton(self, Width-90, Height-35, 75, 'Quit', @btnCloseClick);
  btnClose.ImageName := 'stdimg.quit';
//  btnClose.Focusable := False;
  btnClose.Anchors := [anRight, anBottom];
end;

procedure MainProc;
var
  frm : TMainForm;
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
