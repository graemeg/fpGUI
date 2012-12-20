{%mainunit Agg2D.pas}

{$ifdef uses_interface}
  Xlib,
{$endif}


{$ifdef agg_platform_interface}
  protected
//    m_ximg_window : PXImage;
//    m_buf_window: pointer;
//    m_buf_alloc: unsigned;
//    m_bpp: unsigned;
//    m_byte_order: int;
//    m_gc: TGC;
{$endif}


{$ifdef uses_implementation}
  fpg_x11,
{$endif}


{$ifdef agg_platform_implementation}

type
  // to get access to protected methods (seeing that FPC doesn't support Friend-classes)
  TImageHack = class(TfpgImage);

procedure TAgg2D.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
var
  drawgc: Tgc;
  GcValues: TXGcValues;
begin
  if TfpgWindow(FWindow).WinHandle <= 0 then
  begin
    debugln('    no winhandle available');
    exit;
  end;

  FImg.UpdateImage;
  drawgc := XCreateGc(fpgApplication.Display, TfpgWindow(FWindow).WinHandle, 0, @GcValues);

  // finally dump the image to screen!
  XPutImage(fpgApplication.Display, TfpgWindow(FWindow).WinHandle,
      drawgc, TImageHack(FImg).XImage, 0, 0, 0, 0,
      FWindow.Width, FWindow.Height);

  XFreeGc(fpgApplication.Display, drawgc);
end;


{$endif}

