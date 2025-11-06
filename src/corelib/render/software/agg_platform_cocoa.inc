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
  // Add known substites
  if lowercase(facename) = 'times' then
    facename := 'Times New Roman'
  else if lowercase(facename) = 'courier' then
    facename := 'Courier New'
  else if lowercase(facename) = 'monospace' then
    facename := 'Courier New';
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
  rect, srcRect: NSRect;
  bitmapRep: NSBitmapImageRep;
  planes: array[0..0] of PByte;
{$IFDEF CStackDebug}
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TAgg2D.DoPutBufferToScreen - ' + ClassName);
  DebugLn(Format('x:%d  y:%d  w:%d  h:%d', [x, y, w, h]));
  {$ENDIF}

  // Get the window
  win := TWindowHack(TfpgWidgetBase(FWidget).Window);
  if not Assigned(win) or not win.HandleIsValid then
  begin
    {$IFDEF CStackDebug}
    DebugLn('TAgg2D/Cocoa DoPutBufferToScreen: No valid window handle.');
    {$ENDIF}
    exit;
  end;

  // Update the image data
  FImg.UpdateImage;

  // Get image data from FImg
  imageData := TImageHack(FImg).ImageData;
  if not Assigned(imageData) then
    exit;

  // Calculate bytes per row (assuming 32-bit RGBA)
  bytesPerRow := FImg.Width * 4;

  // Lock focus and draw
  if Assigned(win.View) then
  begin
    win.View.lockFocus;
    try
      // Setup planes array - points to our image data
      planes[0] := PByte(imageData);

      // Create NSBitmapImageRep with our data buffer
      // Using the simple init without the long parameter name
      bitmapRep := NSBitmapImageRep.alloc.initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitsPerPixel_bytesPerRow(
        @planes,           // planes - pointer to our data
        FImg.Width,        // pixelsWide
        FImg.Height,       // pixelsHigh
        8,                 // bitsPerSample
        4,                 // samplesPerPixel (RGBA)
        True,              // hasAlpha
        False,             // isPlanar
        NSDeviceRGBColorSpace,  // colorSpaceName
        32,                // bitsPerPixel
        bytesPerRow        // bytesPerRow
      );

      if Assigned(bitmapRep) then
      begin
        // Draw the bitmap representation directly
        srcRect := NSMakeRect(0, 0, FImg.Width, FImg.Height);
        rect := NSMakeRect(x, y, w, h);
        bitmapRep.drawInRect(rect);
        bitmapRep.release;
      end;

    finally
      win.View.unlockFocus;
    end;

    // Mark the rect as needing display
    rect := NSMakeRect(x, y, w, h);
    win.View.setNeedsDisplayInRect(rect);
  end;
end;


{$endif}


