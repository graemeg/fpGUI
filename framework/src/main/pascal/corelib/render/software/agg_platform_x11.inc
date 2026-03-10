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
  fpg_impl,
  fpg_fontcache,
{$endif}


{$ifdef agg_platform_implementation}

type
  // to get access to protected methods (seeing that FPC doesn't support Friend-classes)
  TImageHack = class(TfpgImage);
  TfpgX11WindowHack = class(TfpgX11Window);


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

procedure TAgg2D.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
var
  drawgc: Tgc;
  GcValues: TXGcValues;
  X11Handle: TfpgWinHandle;
  win: TfpgX11WindowHack;
begin
  win := TfpgX11WindowHack(TfpgWidgetBase(FWidget).Window);
  X11Handle := win.WinHandle;
  if X11Handle <= 0 then
    exit;

  FImg.UpdateImage;
  drawgc := XCreateGc(fpgApplication.Display, X11Handle, 0, @GcValues);

  // finally dump the image to screen!
  XPutImage(fpgApplication.Display, X11Handle,
      drawgc, TImageHack(FImg).XImage, x,y,x,y, w, h);

  XFreeGc(fpgApplication.Display, drawgc);
  win.TriggerSyncCounter;
end;

procedure TAgg2D.DoRestoreFromBuffer(const ARect: TfpgRect);
var
  drawgc: Tgc;
  GcValues: TXGcValues;
  X11Handle: TfpgWinHandle;
  win: TfpgX11WindowHack;
begin
  if (ARect.Width < 1) or (ARect.Height < 1) then
    Exit;
  if not Assigned(FImg) then
    Exit;

  win := TfpgX11WindowHack(TfpgWidgetBase(FWidget).Window);
  X11Handle := win.WinHandle;
  if X11Handle <= 0 then
    Exit;

  FImg.UpdateImage;
  drawgc := XCreateGc(fpgApplication.Display, X11Handle, 0, @GcValues);
  XPutImage(fpgApplication.Display, X11Handle,
      drawgc, TImageHack(FImg).XImage,
      ARect.Left, ARect.Top, ARect.Left, ARect.Top,
      ARect.Width, ARect.Height);
  XFreeGc(fpgApplication.Display, drawgc);
end;


{$endif}

