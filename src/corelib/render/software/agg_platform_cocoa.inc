{%mainunit Agg2D.pas}

{$ifdef uses_interface}
  // CocoaAll,
{$endif}


{$ifdef agg_platform_interface}
  protected
    // nothing here
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
//    else if prop = 'ANTIALIAS' then
//      if propval = 'FALSE' then
//        lf.lfQuality := NONANTIALIASED_QUALITY else
//      if propval = 'DEFAULT' then
//        lf.lfQuality := DEFAULT_QUALITY;
  end;
end;

type
  TWindowHack = class(TfpgCocoaWindow);
  TImageHack = class(TfpgImage);

procedure TAgg2D.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
var
  win: TWindowHack;
  context: NSGraphicsContext;
  cgContext: CGContextRef;
  imageData: Pointer;
  bytesPerRow: Integer;
  colorSpace: CGColorSpaceRef;
  cgImage: CGImageRef;
  destRect: NSRect;
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

  // Lock focus on the view
  if not Assigned(win.View) then
    exit;

  win.View.lockFocus;
  try
    // Get the current graphics context
    context := NSGraphicsContext.currentContext;
    if not Assigned(context) then
      exit;

    cgContext := CGContextRef(context.graphicsPort);

    // Get image data from FImg
    imageData := TImageHack(FImg).ImageData;
    if not Assigned(imageData) then
      exit;

    // Calculate bytes per row (assuming 32-bit RGBA)
    bytesPerRow := FImg.Width * 4;

    // Create a color space
    colorSpace := CGColorSpaceCreateDeviceRGB();
    try
      // Create CGImage from raw data
      cgImage := CGImageCreate(
        FImg.Width,                          // width
        FImg.Height,                         // height
        8,                                   // bits per component
        32,                                  // bits per pixel
        bytesPerRow,                         // bytes per row
        colorSpace,                          // color space
        kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big,  // bitmap info
        nil,                                 // data provider (use imageData directly)
        nil,                                 // decode array
        false,                               // should interpolate
        kCGRenderingIntentDefault            // rendering intent
      );

      // Note: We need a data provider for CGImageCreate
      // For now, use a simpler approach with NSBitmapImageRep
      // This is a placeholder - actual implementation would create proper CGImage

      // Set destination rectangle
      destRect := NSMakeRect(x, y, w, h);

      // For now, mark the view as needing display
      win.View.setNeedsDisplayInRect(destRect);

    finally
      CGColorSpaceRelease(colorSpace);
      if Assigned(cgImage) then
        CGImageRelease(cgImage);
    end;
  finally
    win.View.unlockFocus;
  end;
end;


{$endif}

