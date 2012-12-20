{%mainunit Agg2D.pas}

{$ifdef uses_interface}
  Windows,
{$endif}


{$ifdef agg_platform_interface}
  protected
//    m_ximg_window : PXImage;
{$endif}


{$ifdef uses_implementation}
  fpg_gdi,
{$endif}


{$ifdef agg_platform_implementation}
type
  // to get access to protected methods (seeing that FPC doesn't support Friend-classes)
  TImageHack = class(TfpgImage);

procedure TAgg2D.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
var
  srcdc: HDC;
  destdc: HDC;
begin
  if TfpgWindow(FWindow).WinHandle <= 0 then
  begin
    debugln('    no winhandle available');
    exit;
  end;

  FImg.UpdateImage;
  destdc := Windows.GetDC(TfpgWindow(FWindow).WinHandle);
  srcdc := CreateCompatibleDC(fpgApplication.Display);
  SelectObject(srcdc, TImageHack(FImg).BMPHandle);
  BitBlt(destdc, x, y, w, h, srcdc, 0, 0, SRCCOPY);
  DeleteDC(srcdc);
  ReleaseDC(TfpgWindow(FWindow).WinHandle, destdc);

(*
  if (dvc_width <> bmp_width ) or
     (dvc_height <> bmp_height ) then
  begin
    SetStretchBltMode(h_dc ,COLORONCOLOR );

    StretchDIBits(
     h_dc ,            // handle of device context
     dvc_x ,           // x-coordinate of upper-left corner of source rect.
     dvc_y ,           // y-coordinate of upper-left corner of source rect.
     dvc_width ,       // width of source rectangle
     dvc_height ,      // height of source rectangle
     bmp_x ,
     bmp_y ,           // x, y -coordinates of upper-left corner of dest. rect.
     bmp_width ,       // width of destination rectangle
     bmp_height ,      // height of destination rectangle
     m_buf ,           // address of bitmap bits
     m_bmp^ ,          // address of bitmap data
     DIB_RGB_COLORS ,  // usage
     SRCCOPY );        // raster operation code
  end
  else
  begin
    err:=SetDIBitsToDevice(
     h_dc ,            // handle to device context
     dvc_x ,           // x-coordinate of upper-left corner of
     dvc_y ,           // y-coordinate of upper-left corner of
     dvc_width ,       // source rectangle width
     dvc_height ,      // source rectangle height
     bmp_x ,           // x-coordinate of lower-left corner of
     bmp_y ,           // y-coordinate of lower-left corner of
     0 ,               // first scan line in array
     bmp_height ,      // number of scan lines
     m_buf ,           // address of array with DIB bits
     m_bmp^ ,          // address of structure with bitmap info.
     DIB_RGB_COLORS ); // RGB or palette indexes
  end
*)
end;

{$endif}

