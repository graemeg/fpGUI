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
  fpg_fontcache,
{$endif}


{$ifdef agg_platform_implementation}
type
  // to get access to protected methods (seeing that FPC doesn't support Friend-classes)
  TImageHack = class(TfpgImage);

function FontCacheItemFromFontDesc(const desc: string; var asize: double): TFontCacheItem;
var
  facename: string;
  cp: integer;
  c: char;
  token: string;
  prop, propval: string;

  function NextC: char;
  begin
    Inc(cp);
    if cp > length(desc) then
      c := #0
    else
      c := desc[cp];
    Result := c;
  end;

  procedure NextToken;
  begin
    token := '';
    while (c <> #0) and (c in [' ', 'a'..'z', 'A'..'Z', '_', '0'..'9', '.']) do
    begin
      token := token + c;
      NextC;
    end;
  end;

begin
  Result := TFontCacheItem.Create('');

  cp := 0;
  NextC;
  NextToken;

  facename := token;
  // Substitute common bitmap font names (from OS/2 INF files) to outline equivalents.
  // Must stay consistent with CanvasFontManager.SubstituteBitmapFontToOutline().
  if CompareText(facename, 'Helv') = 0 then
    facename := FPG_DEFAULT_SANS
  else if CompareText(facename, 'Helvetica') = 0 then
    facename := FPG_DEFAULT_SANS
  else if CompareText(facename, 'Tms Rmn') = 0 then
    facename := 'Times New Roman'
  else if CompareText(facename, 'Times') = 0 then
    facename := 'Times New Roman'
  else if CompareText(facename, 'System Proportional') = 0 then
    facename := FPG_DEFAULT_SANS
  else if CompareText(facename, 'System Monospaced') = 0 then
    facename := FPG_DEFAULT_FIXED
  else if CompareText(facename, 'System VIO') = 0 then
    facename := FPG_DEFAULT_FIXED
  else if CompareText(facename, 'Courier') = 0 then
    facename := 'Courier New'
  else if CompareText(facename, 'Monospace') = 0 then
    facename := FPG_DEFAULT_FIXED;
  Result.FamilyName := facename;

  if c = '-' then
  begin
    NextC;
    NextToken;
    asize := StrToIntDef(token, 0);
  end;

  while c = ':' do
  begin
    NextC;
    NextToken;

    prop    := UpperCase(token);
    propval := '';

    if c = '=' then
    begin
      NextC;
      NextToken;
      propval := UpperCase(token);
    end;

    if prop = 'BOLD' then
      Result.IsBold := True
    else if prop = 'ITALIC' then
      Result.IsItalic := True
    else if prop = 'ANGLE' then
      Result.Angle := StrToFloatDef(propval, 0.0);
//    else if prop = 'ANTIALIAS' then
//      if propval = 'FALSE' then
//        lf.lfQuality := NONANTIALIASED_QUALITY else
//      if propval = 'DEFAULT' then
//        lf.lfQuality := DEFAULT_QUALITY;
  end;
end;

function GetWindowsFontDir: string;
var
  lWinFontPath: array[0..MAX_PATH] of WideChar;
  lPasWinFontPath: string;
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
begin
  // Only the top level window canvas puts the buffer to the screen so no delta needed
  if not FWidget.WindowAllocated or not FWidget.Window.HasHandle then
    exit;

  FImg.UpdateImage;
  destdc := Windows.GetDC(TfpgNativeWindow(FWidget.Window).WinHandle);
  srcdc := CreateCompatibleDC(fpgApplication.Display);
  SelectObject(srcdc, TImageHack(FImg).BMPHandle);
  BitBlt(destdc, x, y, w, h, srcdc, x, y, SRCCOPY);
  DeleteDC(srcdc);
  ReleaseDC(TfpgNativeWindow(FWidget.Window).WinHandle, destdc);
end;

procedure TAgg2D.DoRestoreFromBuffer(const ARect: TfpgRect);
var
  srcdc: HDC;
  destdc: HDC;
begin
  if (ARect.Width < 1) or (ARect.Height < 1) then
    Exit;
  if not Assigned(FImg) then
    Exit;
  if not FWidget.WindowAllocated or not FWidget.Window.HasHandle then
    Exit;

  FImg.UpdateImage;
  destdc := Windows.GetDC(TfpgNativeWindow(FWidget.Window).WinHandle);
  srcdc := CreateCompatibleDC(fpgApplication.Display);
  SelectObject(srcdc, TImageHack(FImg).BMPHandle);
  BitBlt(destdc, ARect.Left, ARect.Top, ARect.Width, ARect.Height,
         srcdc, ARect.Left, ARect.Top, SRCCOPY);
  DeleteDC(srcdc);
  ReleaseDC(TfpgNativeWindow(FWidget.Window).WinHandle, destdc);
end;

{$endif}

