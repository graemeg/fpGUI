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
  rect: NSRect;
  i, j: Integer;
  srcPixel: PLongWord;
  context: NSGraphicsContext;
  color: NSColor;
  r, g, b, a: Byte;
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

  // Draw pixel by pixel using lockFocus
  // NOTE: This is a simple but slow implementation for initial testing
  // TODO: Optimize by using a more efficient bitmap rendering method
  if Assigned(win.View) and Assigned(win.WinHandle) then
  begin
    // Lock focus on the view to draw
    if win.View.lockFocusIfCanDraw then
    try
      // Draw the rectangle of pixels
      srcPixel := PLongWord(imageData);
      Inc(srcPixel, x + (y * FImg.Width)); // Start at the source position

      for j := 0 to h - 1 do
      begin
        for i := 0 to w - 1 do
        begin
          // Extract RGBA components (assuming BGRA byte order on little-endian)
          b := PByte(srcPixel)^;
          g := PByte(PtrUInt(srcPixel) + 1)^;
          r := PByte(PtrUInt(srcPixel) + 2)^;
          a := PByte(PtrUInt(srcPixel) + 3)^;

          // Create NSColor and set it
          color := NSColor.colorWithDeviceRed_green_blue_alpha(
            r / 255.0,
            g / 255.0,
            b / 255.0,
            a / 255.0
          );
          color.set_;

          // Draw a 1x1 rectangle (pixel)
          NSRectFill(NSMakeRect(x + i, y + j, 1, 1));

          Inc(srcPixel);
        end;
        // Move to next row in source image
        Inc(srcPixel, FImg.Width - w);
      end;

    finally
      win.View.unlockFocus;
    end;

    // Force immediate display - flush the window to show rendered content
    win.WinHandle.flushWindow;
  end;
end;


{$endif}


