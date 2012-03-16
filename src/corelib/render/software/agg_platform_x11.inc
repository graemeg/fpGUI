{%mainunit Agg2D.pas}

{$ifdef uses_interface}
  X,
  Xlib,
  Xutil,
  Xatom,
{$endif}


{$ifdef agg_platform_interface}
  protected
//    m_ximg_window : PXImage;
//    m_buf_window: pointer;
//    m_buf_alloc: unsigned;
//    m_bpp: unsigned;
//    m_byte_order: int;
//    m_gc: TGC;
    FImg: TfpgImage;
{$endif}


{$ifdef uses_implementation}
  fpg_x11,
{$endif}


{$ifdef agg_platform_implementation}

type
  // to get access to protected methods
  TX11AppHack = class(TfpgApplication);
  TX11ImageHack = class(TfpgImage);

procedure TAgg2D.DoSetFontRes(fntres: TfpgFontResourceBase);
begin
  Font('/usr/share/fonts/truetype/ttf-liberation/LiberationSans-Regular.ttf', 13);
end;

procedure TAgg2D.DoSetTextColor(cl: TfpgColor);
var
  t: TRGBTriple;
  c: TfpgColor;
begin
  c := fpgColorToRGB(cl);
  t := fpgColorToRGBTriple(c);

  FillColor(t.Red, t.Green, t.Blue{, t.Alpha});
end;

procedure TAgg2D.DoSetColor(cl: TfpgColor);
var
  t: TRGBTriple;
  c: TfpgColor;
begin
  c := fpgColorToRGB(cl);
  t := fpgColorToRGBTriple(c);

  LineColor(t.Red, t.Green, t.Blue{, t.Alpha});
end;

procedure TAgg2D.DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle);
begin
//  LineWidth(awidth);
  case astyle of
    lsSolid:
      begin
        m_convDash.remove_all_dashes;
        m_convDash.add_dash(600, 0);  {$NOTE Find a better way to prevent dash generation }
      end;
    lsDash:
      begin
        m_convDash.remove_all_dashes;
        m_convDash.add_dash(3, 3);
      end;
    lsDot:
      begin
        m_convDash.remove_all_dashes;
        m_convDash.add_dash(1, 1.5);
      end;
    lsDashDot:
      begin
        m_convDash.remove_all_dashes;
        m_convDash.add_dash(3, 1);
      end;
    lsDashDotDot:
      begin
        m_convDash.add_dash(3, 1);
        m_convDash.add_dash(1, 1);
      end;
  end;
end;

procedure TAgg2D.DoGetWinRect(out r: TfpgRect);
begin
  r.Left    := 0;
  r.Top     := 0;
  r.Width := FWindow.Width;
  r.Height := FWindow.Height;
end;

procedure TAgg2D.DoFillRectangle(x, y, w, h: TfpgCoord);
begin
  FillColor(LineColor);
  NoLine;
//  LineWidth(FLineWidth);
  if (w = 1) or (h = 1) then
  begin
    // we have a line
    LineCap(AGG_CapButt);
    if w = 1 then
      Line(x, y, x, y+h, True)
    else
      Line(x, y, x+w, y, True);
  end
  else
    Rectangle(x, y, x+w-1, y+h-1, True);
end;

procedure TAgg2D.DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord);
begin

end;

procedure TAgg2D.DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord);
begin

end;

procedure TAgg2D.DoDrawRectangle(x, y, w, h: TfpgCoord);
begin
//  LineWidth(FLineWidth);
  DoSetColor(FColor);
  NoFill;
  if (w = 1) or (h = 1) then
  begin
    // we have a line
    LineCap(AGG_CapButt);
    if w = 1 then
      Line(x, y, x, y+h, True)
    else
      Line(x, y, x+w, y, True);
  end
  else
    Rectangle(x, y, x+w-1, y+h-1, True);
end;

procedure TAgg2D.DoDrawLine(x1, y1, x2, y2: TfpgCoord);
begin
  Line(x1, y1, x2, y2, True);
end;

procedure TAgg2D.DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi,
  w, h: integer);
begin
  ImageFilter(AGG_NoFilter);
  TransformImage(
    TfpgImage(img) ,
    xi ,yi ,
    xi+w-1 ,
    yi+h-1 ,
    x ,y ,
    x+w-1 ,
    y+h-1 );
end;

procedure TAgg2D.DoDrawString(x, y: TfpgCoord; const txt: string);
begin
  DoSetTextColor(FTextColor);
  NoLine;
  TextHints(True);
  Text(x, y+FontHeight, txt);
end;

procedure TAgg2D.DoSetClipRect(const ARect: TfpgRect);
begin

end;

function TAgg2D.DoGetClipRect: TfpgRect;
begin

end;

procedure TAgg2D.DoAddClipRect(const ARect: TfpgRect);
begin

end;

procedure TAgg2D.DoClearClipRect;
begin

end;

procedure TAgg2D.DoBeginDraw(awin: TfpgWindowBase; buffered: boolean);
begin
  if not Assigned(FImg) then
  begin
    FImg := TfpgImage.Create;
    FImg.AllocateImage(32, FWindow.Width, FWindow.Height);
    Attach(FImg);
  end;
end;

procedure TAgg2D.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
var
  drawgc: Tgc;
  GcValues: TXGcValues;
begin
  if TfpgX11Window(FWindow).WinHandle <= 0 then
  begin
    writeln('    no winhandle available');
    exit;
  end;

  FImg.UpdateImage;
  drawgc := XCreateGc(fpgApplication.Display, TfpgX11Window(FWindow).WinHandle, 0, @GcValues);

  // finally dump the image to screen!
  XPutImage(fpgApplication.Display, TfpgX11Window(FWindow).WinHandle,
      drawgc, TX11ImageHack(FImg).XImage, 0, 0, 0, 0,
      FWindow.Width, FWindow.Height);

  XFreeGc(fpgApplication.Display, drawgc);
end;

procedure TAgg2D.DoEndDraw;
begin
  // nothing to do here
end;

function TAgg2D.GetPixel(X, Y: integer): TfpgColor;
begin
  Result := FImg.Colors[y, y];
end;

procedure TAgg2D.SetPixel(X, Y: integer; const AValue: TfpgColor);
begin
  FImg.Colors[x, y] := AValue;
end;

procedure TAgg2D.DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
begin

end;

procedure TAgg2D.DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
begin

end;

procedure TAgg2D.DoDrawPolygon(Points: PPoint; NumPts: Integer; Winding: boolean
  );
begin

end;

{$endif}

