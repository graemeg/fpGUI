unit fpg_cairocanvas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Cairo, fpg_base{$IFDEF UNIX}, CairoXlib{$ENDIF}, pango, fpg_impl;

type
  TfpgCairoCanvas = class(TfpgCanvasBase)
  private
    FBufferPixmap: TfpgDCHandle;
    FSurface: Pcairo_surface_t;
    FSurfaceSize: TfpgSize;
    FContext: Pcairo_t;
    FPixmapSize: TfpgSize;
    FTextCol: TfpgColor;
    FDrawCol: TfpgColor;
    FCurrentColor: TfpgColor;
    FPangoLayout: PPangoLayout;
    procedure   CheckAllocateSurface(AW, AH: Integer);
    function    TargetSurface: Pcairo_surface_t;
    procedure   SetCairoColor(AColor: TfpgColor);
    procedure   TryFreePixmap;

  protected
    procedure   DoSetFontRes(fntres: TfpgFontResourceBase); override;
    procedure   DoSetTextColor(cl: TfpgColor); override;
    procedure   DoSetColor(cl: TfpgColor); override;
    procedure   DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle); override;
    procedure   DoFillRectangle(x, y, w, h: TfpgCoord); override;
    procedure   DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord); override;
    procedure   DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord); override;
    procedure   DoDrawRectangle(x, y, w, h: TfpgCoord); override;
    procedure   DoDrawLine(x1, y1, x2, y2: TfpgCoord); override;
    procedure   DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer); override;
    procedure   DoDrawString(x, y: TfpgCoord; const txt: string); override;
    procedure   DoSetClipRect(const ARect: TfpgRect); override;
    function    DoGetClipRect: TfpgRect; override;
    procedure   DoAddClipRect(const ARect: TfpgRect); override;
    procedure   DoClearClipRect; override;
    procedure   DoBeginDraw(awidget: TfpgWidgetBase; CanvasTarget: TfpgCanvasBase); override;
    procedure   DoPutBufferToScreen(x, y, w, h: TfpgCoord); override;
    procedure   DoEndDraw; override;
    function    GetPixel(X, Y: integer): TfpgColor; override;
    procedure   SetPixel(X, Y: integer; const AValue: TfpgColor); override;
    procedure   DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: Extended); override;
    procedure   DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended); override;
    procedure   DoDrawPolygon(const Points: array of TPoint); override;
    function    GetBufferAllocated: Boolean; override;
    procedure   DoAllocateBuffer; override;
  public
    constructor Create(awidget: TfpgWidgetBase); override;
    destructor  Destroy; override;
  end;

const
  PIXMAP_RESIZE_SIZE = 50;

implementation
uses
  fpg_main,
  fpg_x11,
  x, xlib,
  glib2,
  pangocairo;

type
  TfpgX11ApplicationHack = class(TfpgX11Application);
  TfpgX11WindowHack = class(TfpgX11Window);

  function xApplication: TfpgX11ApplicationHack;
  begin
    result := TfpgX11ApplicationHack(fpgApplication);
  end;

{ TfpgCairoCanvas }

procedure TfpgCairoCanvas.CheckAllocateSurface(AW, AH: Integer);
var
  Display: xlib.PXDisplay;
  Visual: xlib.PVisual;
  Drawable: x.TDrawable;
begin
  if WeAreTopLevelCanvas then
  begin
    // the toplevel widget canvas has the 'surface'
    if Assigned(FSurface) and (FSurfaceSize.H <> AH) and (FSurfaceSize.W <> AW)then
      Exit; // ==>

    if Assigned(FContext) then
    begin
      cairo_destroy(FContext);
      FContext:=nil;
      g_object_unref(FPangoLayout);
      FPangoLayout:=nil;
    end;

    if Assigned(FSurface) then
    begin
      cairo_surface_destroy(FSurface);
      FSurface:=nil;
    end;

    Display := fpgApplication.Display;
    Visual := xapplication.DefaultVisual;
    Drawable := FBufferPixmap;

    FSurface := cairo_xlib_surface_create(Display, Drawable, Visual, AW, AH);
    FSurfaceSize.SetSize(AW, AH);
  end
  else if Assigned(FContext) then
  begin
    // each canvas has a context.
    cairo_destroy(FContext);
    FContext:=nil;
    // free font layout object
    g_object_unref(FPangoLayout);
    FPangoLayout:=nil;
  end;


  if not Assigned(FContext) then
  begin
    FContext := cairo_create(TargetSurface);
    cairo_translate(FContext, FDeltaX, FDeltaY);
    SetCairoColor(FDrawCol);
    FPangoLayout := pango_cairo_create_layout(FContext);
  end;
end;

function TfpgCairoCanvas.TargetSurface: Pcairo_surface_t;
begin
  if WeAreTopLevelCanvas then
    Result := FSurface
  else
    Result := TfpgCairoCanvas(FCanvasTarget).FSurface;
end;

procedure TfpgCairoCanvas.SetCairoColor(AColor: TfpgColor);
var
  c: TfpgColor;

  function C2D(AShift: Byte): Double; inline;
  begin
    Result := ((c shr AShift) and $FF) / $FF;
    //WriteLn(Format('Shift %d = %f', [AShift, Result]));
  end;
begin
  if FCurrentColor = AColor then
    Exit; // ==>

  FCurrentColor:=AColor;

  c := fpgColorToRGB(AColor);
  cairo_set_source_rgba(FContext, C2D(16), C2D(8), C2D(0), C2D(24));
end;

procedure TfpgCairoCanvas.TryFreePixmap;
begin
  if FBufferPixmap > 0 then
    XFreePixmap(xapplication.Display, FBufferPixmap);
  FBufferPixmap := 0;
  if Assigned(FContext) then
    begin
      cairo_destroy(FContext);
      FContext:=nil;
    end;
  if Assigned(FPangoLayout) then
  begin
    g_object_unref(FPangoLayout);
    FPangoLayout:=nil;
  end;

  if Assigned(FSurface) then
  begin
    cairo_surface_destroy(FSurface);
    FSurface:=nil;
  end;
end;

function ReworkFont(ADesc: String): String;
var
  Family: String;
  lAttrs: String;
  lSize: String;
  lPos, lEnd: SizeInt;
begin
  lPos := Pos('-', ADesc);
  Family := Copy(ADesc,1, lPos-1);
  lPos := lPos+1;
  lEnd := Pos(':',ADesc, lPos);
  if lEnd=0 then
    lEnd:=MaxInt;
  lSize:=Copy(ADesc, lPos, lEnd-lPos);
  if Pos('bold', ADesc) > 0 then
    lAttrs:='bold';
  //lAttrs:=lAttrs+ 'antialias ';


  Result := Format('%s %s %s', [Family, lAttrs, lSize]);

end;

procedure TfpgCairoCanvas.DoSetFontRes(fntres: TfpgFontResourceBase);
var
  fnt: TfpgFontResource absolute fntres;
  lPangoFont: PPangoFontDescription;
begin
  //WriteLn('Font = ', fnt.FontDesc);
  lPangoFont := pango_font_description_from_string(PChar(ReworkFont(fnt.FontDesc)));
  pango_layout_set_font_description(FPangoLayout, lPangoFont);
  pango_font_description_free(lPangoFont);
end;

procedure TfpgCairoCanvas.DoSetTextColor(cl: TfpgColor);
begin
  FTextCol:= cl;
end;

procedure TfpgCairoCanvas.DoSetColor(cl: TfpgColor);
begin
  FDrawCol:=cl;
  SetCairoColor(FDrawCol);
end;

procedure TfpgCairoCanvas.DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle);
var
  DashArr: array[0..2] of Double;
begin
  cairo_set_line_width(FContext, awidth-0.1);

  DashArr[0]:= 3; // dash
  DashArr[1]:= 1; // dot
  DashArr[2]:= 1; // dot

  //writeln('line width = ', awidth, ' style = ', astyle);
  case astyle of
    lsSolid     : cairo_set_dash(FContext, nil, 0,0);
    lsDot       : cairo_set_dash(FContext, @DashArr[1], 1,0);
    lsDash      : cairo_set_dash(FContext, @DashArr[0], 1,0);
    lsDashDot   : cairo_set_dash(FContext, @DashArr[0], 2,0);
    lsDashDotDot: cairo_set_dash(FContext, @DashArr[0], 3,0);
  end;

  //lsSolid, lsDash, lsDot, lsDashDot, lsDashDotDot

end;

procedure TfpgCairoCanvas.DoFillRectangle(x, y, w, h: TfpgCoord);
begin
  SetCairoColor(FDrawCol);
  cairo_rectangle(FContext, x,y,w,h);
  cairo_fill(FContext);
end;

procedure TfpgCairoCanvas.DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord);
begin
  cairo_save(FContext);
  SetCairoColor(FDrawCol);
  cairo_rectangle(FContext, x,y,w,h);
  cairo_set_operator(FContext, CAIRO_OPERATOR_XOR);
  cairo_fill(FContext);
  cairo_restore(FContext);
end;

procedure TfpgCairoCanvas.DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord);
var
  Points: Array[0..2] of TPoint;
begin
  cairo_move_to(FContext, x1, y1);
  cairo_line_to(FContext, x2, y2);
  cairo_line_to(FContext, x3, y3);
  //cairo_line_to(FContext, x1, y1); // needed?

  cairo_close_path(FContext);
end;

procedure TfpgCairoCanvas.DoDrawRectangle(x, y, w, h: TfpgCoord);
begin
  cairo_rectangle(FContext, x, y, w, h);
  cairo_stroke(FContext);
end;

procedure TfpgCairoCanvas.DoDrawLine(x1, y1, x2, y2: TfpgCoord);
begin
  cairo_move_to(FContext, x1+0.5, y1+0.5);
  cairo_line_to(FContext, x2-0.5, y2-0.5);
  cairo_stroke(FContext);
end;





procedure TfpgCairoCanvas.DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer);
var
  lSurface: Pcairo_surface_t;
  lMask: Pcairo_surface_t;
  lStride: ptrint;
  lFormat: cairo_format_t;
  mask: pbyte;
  i: Integer;
begin
  img.CreateMaskFromSample(15,15);
 { if img.Masked then
    lFormat:=CAIRO_FORMAT_RGB24
  else}
    lFormat:=CAIRO_FORMAT_ARGB32;

  // for i := 0 to img.ImageDataSize div 4 do PLongWord(img.ImageData)[i] := PLongWord(img.ImageData)[i] and $00FFFFFF;



  lStride:=cairo_format_stride_for_width(lFormat, img.Width);

  //mask :=MaskAsA1(img.MaskData, img.Width, img.Height);

  lSurface := cairo_image_surface_create_for_data(img.ImageData, lFormat, img.Width, img.Height, lStride);

  cairo_save(FContext);
  cairo_set_source_surface(FContext, lSurface, x,y);
  if img.Masked then
  begin
    lStride:=cairo_format_stride_for_width(CAIRO_FORMAT_ARGB32, img.Width);
   // writeln('stride=', lStride,' w=',img.Width);
    lMask := cairo_image_surface_create_for_data(img.MaskData, CAIRO_FORMAT_ARGB32, img.Width, img.Height, lStride);
    cairo_mask_surface(FContext, lMask,x,y);
  end;

  cairo_rectangle(FContext, x,y,w,h);
  cairo_set_operator(FContext, CAIRO_OPERATOR_SOURCE);
  cairo_fill(FContext);

  cairo_restore(FContext);


  cairo_surface_destroy(lSurface);
  if img.Masked then
    cairo_surface_destroy(lMask);

end;

procedure TfpgCairoCanvas.DoDrawString(x, y: TfpgCoord; const txt: string);
var
  extents: cairo_text_extents_t;
begin
  SetCairoColor(FTextCol);
  //cairo_text_extents(FContext, PChar(txt), @extents);
  //cairo_move_to(FContext, x+extents.x_bearing, y+extents.height);
  //cairo_show_text(FContext, PChar(Txt));
  cairo_set_antialias(FContext, CAIRO_ANTIALIAS_BEST);
  pango_layout_set_text(FPangoLayout, PChar(txt),-1);
  pango_cairo_update_layout(FContext, FPangoLayout);
  cairo_move_to(FContext, x, y);
  pango_cairo_show_layout(FContext, FPangoLayout);

  SetCairoColor(FDrawCol);
end;

procedure TfpgCairoCanvas.DoSetClipRect(const ARect: TfpgRect);
var
  Clip: TfpgRect;
begin
  cairo_translate(FContext, -FDeltaX,-FDeltaY); // remove transformation
  cairo_reset_clip(FContext);
  Clip := ARect;
  Clip.OffsetRect(FDeltaX, FDeltaY);
  GetWidgetWindowRect.IntersectRect(Clip, Clip);

  cairo_rectangle(FContext, Clip.Left, Clip.Top, Clip.Width, Clip.Height);
  cairo_clip(FContext);

  cairo_translate(FContext, FDeltaX,FDeltaY); // restore transformation

end;

function TfpgCairoCanvas.DoGetClipRect: TfpgRect;
begin

end;

procedure TfpgCairoCanvas.DoAddClipRect(const ARect: TfpgRect);
begin

end;

procedure TfpgCairoCanvas.DoClearClipRect;
var
  R: TfpgRect;
begin
  cairo_reset_clip(FContext);
  if not WeAreTopLevelCanvas then
  begin
    R := GetWidgetWindowRect;
    cairo_translate(FContext, -FDeltaX,-FDeltaY); // remove transformation
    cairo_rectangle(FContext, R.Left, R.Top, R.Width, R.Height);
    cairo_clip(FContext);
    cairo_translate(FContext, FDeltaX,FDeltaY); // restore transformation
  end;

end;

procedure TfpgCairoCanvas.DoBeginDraw(awidget: TfpgWidgetBase; CanvasTarget: TfpgCanvasBase);
begin
  CheckAllocateSurface(awidget.Width, awidget.Height);
end;

procedure TfpgCairoCanvas.DoPutBufferToScreen(x, y, w, h: TfpgCoord);
var
 cgc: TGC;
  GcValues: TXGCValues;
  drawgc: TfpgWinHandle;
begin
  if not WeAreTopLevelCanvas then
    Exit; // ==>

  drawgc := TfpgX11Windowhack(FWidget.Window).WinHandle;

  // finish any queued drawing
  cairo_surface_flush(FSurface);

  cgc := XCreateGc(xapplication.display, FBufferPixmap, 0, @GcValues);
  XCopyArea(xapplication.Display, FBufferPixmap, drawgc, cgc, x+FDeltaX, y+FDeltaY, w, h, x+FDeltaX, y+FDeltaY);
  XFreeGc(xapplication.display, cgc);

  TfpgX11Windowhack(FWidget.Window).TriggerSyncCounter;
end;

procedure TfpgCairoCanvas.DoEndDraw;
begin

end;

function TfpgCairoCanvas.GetPixel(X, Y: integer): TfpgColor;
begin

end;

procedure TfpgCairoCanvas.SetPixel(X, Y: integer; const AValue: TfpgColor);
begin
  SetCairoColor(AValue);
  DoDrawRectangle(x,y,1,1);
end;

procedure TfpgCairoCanvas.DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
begin

end;

procedure TfpgCairoCanvas.DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
begin

end;

procedure TfpgCairoCanvas.DoDrawPolygon(const Points: array of TPoint);
var
  i: Integer;
begin
  if High(Points) < 0 then
    Exit; //==>

  cairo_move_to(FContext, Points[High(Points)].x, Points[High(Points)].y);
  for i := Low(Points) to High(Points) do
  begin
    cairo_line_to(FContext, Points[i].x, Points[i].y);
  end;
  cairo_stroke(FContext);
end;

function TfpgCairoCanvas.GetBufferAllocated: Boolean;
var
  x: integer;
  y: integer;
  rw: TXID;
  d: TXID;
  w, wp: longword;
  h, hp: longword;
  bw: longword;
begin
  if FCanvasTarget <> Self then
    Result := TfpgCairoCanvas(FCanvasTarget).GetBufferAllocated
  else
  begin

    Result := FBufferPixmap > 0;
    if Result then
    begin
      XGetGeometry(xapplication.display, FBufferPixmap, @rw, @x, @y, @wp, @hp, @bw, @d);
      if (wp - FWidget.Width > PIXMAP_RESIZE_SIZE*2) or (hp - FWidget.Height > PIXMAP_RESIZE_SIZE*2) or (FWidget.Width > wp) or (FWidget.Height > hp) then
      begin
        TryFreePixmap;
        Result := False;
      end;
    end;
  end;
end;

procedure TfpgCairoCanvas.DoAllocateBuffer;
begin
  if FBufferPixmap <> 0 then
      TryFreePixmap;
  FPixmapSize.W:=FWidget.Width+PIXMAP_RESIZE_SIZE;
  FPixmapSize.H:= FWidget.Height+PIXMAP_RESIZE_SIZE;
  FBufferPixmap := XCreatePixmap(xapplication.display, TfpgX11WindowHack(FWidget.Window).WinHandle, FPixmapSize.W,  FPixmapSize.H, xapplication.DisplayDepth);

  CheckAllocateSurface(FPixmapSize.W, FPixmapSize.H);
end;

constructor TfpgCairoCanvas.Create(awidget: TfpgWidgetBase);
begin
  Inherited Create(awidget);

end;

destructor TfpgCairoCanvas.Destroy;
begin
  TryFreePixmap;
  inherited Destroy;
end;

end.

