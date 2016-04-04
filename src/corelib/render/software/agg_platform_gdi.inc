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

function GetWindowsFontDir: string;
var
  lWinFontPath: array[0..MAX_PATH] of WideChar;
  lPasWinFontPath: string;
  i: Integer;
begin
  // Find for example C:\Windows\Fonts or C:\WINNT\Fonts
  Windows.GetWindowsDirectoryW(@lWinFontPath[0], MAX_PATH);
  lPasWinFontPath := lWinFontPath;
  lPasWinFontPath := IncludeTrailingPathDelimiter(lPasWinFontPath) + 'Fonts' + PathDelim;
  Result := lPasWinFontPath;
end;

procedure TAgg2D.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
var
  srcdc: HDC;
  destdc: HDC;
{$IFDEF CStackDebug}
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TAgg2D.DoPutBufferToScreen - ' + ClassName);
  DebugLn(Format('x:%d  y:%d  w:%d  h:%d', [x, y, w, h]));
  {$ENDIF}
  // Only the top level window canvas puts the buffer to the screen so no delta needed
  if not FWidget.WindowAllocated or not FWidget.Window.HasHandle then
  begin
    debugln('TAgg2D/GDI DoPutBufferToScreen:  No WinHandle available.');
    exit;
  end;

  FImg.UpdateImage;
  destdc := Windows.GetDC(TfpgNativeWindow(FWidget.Window).WinHandle);
  srcdc := CreateCompatibleDC(fpgApplication.Display);
  SelectObject(srcdc, TImageHack(FImg).BMPHandle);
  BitBlt(destdc, x, y, w, h, srcdc, x, y, SRCCOPY);
  DeleteDC(srcdc);
  ReleaseDC(TfpgNativeWindow(FWidget.Window).WinHandle, destdc);
end;

{$endif}

