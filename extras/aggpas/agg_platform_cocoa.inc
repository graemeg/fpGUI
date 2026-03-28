{%mainunit Agg2D.pas}

{$ifdef uses_interface}
  CocoaAll,
{$endif}


{$ifdef agg_platform_interface}
  protected
    // No platform-specific members needed for Cocoa
{$endif}


{$ifdef uses_implementation}
  fpg_cocoa,
  fpg_impl,
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
  end;
end;

type
  TWindowHack = class(TfpgCocoaWindow);

procedure TAgg2D.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
var
  win: TWindowHack;
  imageData: Pointer;
  bytesPerRow: Integer;
  rect: NSRect;
  i, j: Integer;
  srcPixel: PLongWord;
  context: NSGraphicsContext;
  color: NSColor;
  r, g, b, a: Byte;
begin
  // Get the window
  win := TWindowHack(TfpgWidgetBase(FWidget).Window);
  if not Assigned(win) or not win.HandleIsValid then
    exit;

  FImg.UpdateImage;

  // Get image data from FImg
  imageData := TImageHack(FImg).ImageData;
  if not Assigned(imageData) then
    exit;

  // Store the image buffer in the view so it can be drawn in drawRect
  if Assigned(win.View) then
  begin
    win.View.setImageBuffer(imageData, FImg.Width, FImg.Height);

    // Mark the dirty rectangle as needing display
    rect := NSMakeRect(x, y, w, h);
    win.View.setNeedsDisplayInRect(rect);

    // Force immediate display
    win.View.displayIfNeeded;
  end;
end;

procedure TAgg2D.DoRestoreFromBuffer(const ARect: TfpgRect);
var
  win: TWindowHack;
  imageData: Pointer;
  rect: NSRect;
begin
  if (ARect.Width < 1) or (ARect.Height < 1) then
    Exit;
  if not Assigned(FImg) then
    Exit;

  win := TWindowHack(TfpgWidgetBase(FWidget).Window);
  if not Assigned(win) or not win.HandleIsValid then
    Exit;

  FImg.UpdateImage;

  imageData := TImageHack(FImg).ImageData;
  if not Assigned(imageData) then
    Exit;

  if Assigned(win.View) then
  begin
    win.View.setImageBuffer(imageData, FImg.Width, FImg.Height);
    rect := NSMakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height);
    win.View.setNeedsDisplayInRect(rect);
    win.View.displayIfNeeded;
  end;
end;


{$endif}

