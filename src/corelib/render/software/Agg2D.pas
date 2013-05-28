//----------------------------------------------------------------------------
// Agg2D - Version 1.0
// Based on Anti-Grain Geometry
// Copyright (C) 2005 Maxim Shemanarev (http://www.antigrain.com)
//
// TAgg2D - Version 1.0 Release Milano 3 (AggPas 2.4 RM3)
// Pascal Port By: Milan Marusinec alias Milano
//                 milan@marusinec.sk
//                 http://www.aggpas.org
// Copyright (c) 2007 - 2008
//
// Permission to copy, use, modify, sell and distribute this software
// is granted provided this copyright notice appears in all copies.
// This software is provided "as is" without express or implied
// warranty, and with no claim as to its suitability for any purpose.
//
//
//

unit Agg2D;

interface

{$I agg_mode.inc }

{ With this define you can switch use of FreeType or Win32 TrueType font
  engine.

  NOTE:
  The FreeType font engine is recommended, even under Windows - simply
  because it has more functionality and generates better looking text. If
  enabled under Windows, your application will require the freetype.dll
  though. The freetype.dll is included in the fpGUI repository for your
  convenience. }

{.$DEFINE AGG2D_USE_FREETYPE }

{$IFDEF AGG2D_USE_FREETYPE}
  {$UNDEF AGG2D_USE_WINFONTS}
{$ENDIF}

uses
  agg_basics ,
  agg_array ,
  agg_trans_affine ,
  agg_trans_viewport ,
  agg_path_storage ,
  agg_conv_stroke ,
  agg_conv_transform ,
  agg_conv_curve ,
  agg_conv_dash ,
  agg_rendering_buffer ,
  agg_renderer_base ,
  agg_renderer_scanline ,
  agg_span_gradient ,
  agg_span_image_filter_rgba ,
  agg_span_image_resample_rgba ,
  agg_span_converter ,
  agg_span_interpolator_linear ,
  agg_span_allocator ,
  agg_rasterizer_scanline_aa ,
  agg_gamma_functions ,
  agg_scanline_u ,
  agg_arc ,
  agg_bezier_arc ,
  agg_rounded_rect ,
  agg_font_engine ,
  agg_font_cache_manager ,
  agg_pixfmt ,
  agg_pixfmt_rgb ,
  agg_pixfmt_rgba ,
  agg_color ,
  agg_math_stroke ,
  agg_image_filters ,
  agg_vertex_source ,
  agg_render_scanlines ,

{$IFDEF AGG2D_USE_FREETYPE }
  agg_font_freetype ,
{$ENDIF }
{$IFDEF AGG2D_USE_WINFONTS}
  agg_font_win32_tt ,
{$ENDIF }

  Math,
  Classes,
  SysUtils,

  // This allows for platform specific uses clauses
  {$define uses_interface}
  {$undef agg_platform_interface}
  {$undef uses_implementation}
  {$undef agg_platform_implementation}
  {$IFDEF WINDOWS}
    {$I agg_platform_gdi.inc}
  {$ENDIF}
  {$IFDEF UNIX}
    {$I agg_platform_x11.inc}
  {$ENDIF}

  fpg_base,
  fpg_main;

{ GLOBAL VARIABLES & CONSTANTS }
const
// LineJoin
 AGG_JoinMiter = miter_join;
 AGG_JoinRound = round_join;
 AGG_JoinBevel = bevel_join;

// LineCap
 AGG_CapButt   = butt_cap;
 AGG_CapSquare = square_cap;
 AGG_CapRound  = round_cap;

// TextAlignment
 AGG_AlignLeft   = 0;
 AGG_AlignRight  = 1;
 AGG_AlignCenter = 2;
 AGG_AlignTop    = AGG_AlignRight;
 AGG_AlignBottom = AGG_AlignLeft;

// BlendMode
 AGG_BlendAlpha      = end_of_comp_op_e;
 AGG_BlendClear      = comp_op_clear;
 AGG_BlendSrc        = comp_op_src;
 AGG_BlendDst        = comp_op_dst;
 AGG_BlendSrcOver    = comp_op_src_over;
 AGG_BlendDstOver    = comp_op_dst_over;
 AGG_BlendSrcIn      = comp_op_src_in;
 AGG_BlendDstIn      = comp_op_dst_in;
 AGG_BlendSrcOut     = comp_op_src_out;
 AGG_BlendDstOut     = comp_op_dst_out;
 AGG_BlendSrcAtop    = comp_op_src_atop;
 AGG_BlendDstAtop    = comp_op_dst_atop;
 AGG_BlendXor        = comp_op_xor;
 AGG_BlendAdd        = comp_op_plus;
 AGG_BlendSub        = comp_op_minus;
 AGG_BlendMultiply   = comp_op_multiply;
 AGG_BlendScreen     = comp_op_screen;
 AGG_BlendOverlay    = comp_op_overlay;
 AGG_BlendDarken     = comp_op_darken;
 AGG_BlendLighten    = comp_op_lighten;
 AGG_BlendColorDodge = comp_op_color_dodge;
 AGG_BlendColorBurn  = comp_op_color_burn;
 AGG_BlendHardLight  = comp_op_hard_light;
 AGG_BlendSoftLight  = comp_op_soft_light;
 AGG_BlendDifference = comp_op_difference;
 AGG_BlendExclusion  = comp_op_exclusion;
 AGG_BlendContrast   = comp_op_contrast;

{ TYPES DEFINITION }
type
 PAggColor = ^TAggColor;
 TAggColor = rgba8;

 TAggRectD = agg_basics.rect_d;

 TAggAffine = trans_affine;
 PAggAffine = trans_affine_ptr;

 TAggFontRasterizer = gray8_adaptor_type;
 PAggFontRasterizer = gray8_adaptor_type_ptr;

 TAggFontScanline = gray8_scanline_type;
 PAggFontScanline = gray8_scanline_type_ptr;

{$IFDEF AGG2D_USE_FREETYPE }
 TAggFontEngine = font_engine_freetype_int32;
{$ENDIF }
{$IFDEF AGG2D_USE_WINFONTS}
 TAggFontEngine = font_engine_win32_tt_int32;
{$ENDIF }

 TAggGradient  = (
   AGG_Solid ,
   AGG_Linear ,
   AGG_Radial );

 TAggDirection = (
   AGG_CW,
   AGG_CCW );

 TAggLineJoin  = int;
 TAggLineCap   = int;
 TAggBlendMode = comp_op_e;

 TAggTextAlignment = int;

 TAggDrawPathFlag = (
  AGG_FillOnly ,
  AGG_StrokeOnly ,
  AGG_FillAndStroke ,
  AGG_FillWithLineColor );

 TAggViewportOption = (
  AGG_Anisotropic ,
  AGG_XMinYMin ,
  AGG_XMidYMin ,
  AGG_XMaxYMin ,
  AGG_XMinYMid ,
  AGG_XMidYMid ,
  AGG_XMaxYMid ,
  AGG_XMinYMax ,
  AGG_XMidYMax ,
  AGG_XMaxYMax );

 TAggImageFilter = (
  AGG_NoFilter ,
  AGG_Bilinear ,
  AGG_Hanning ,
  AGG_Hermite ,
  AGG_Quadric ,
  AGG_Bicubic ,
  AGG_Catrom ,
  AGG_Spline16 ,
  AGG_Spline36 ,
  AGG_Blackman144 );

 TAggImageResample = (
  AGG_NoResample ,
  AGG_ResampleAlways ,
  AGG_ResampleOnZoomOut );

 TAggFontCacheType = (
  AGG_RasterFontCache ,
  AGG_VectorFontCache );

 TPixelFormat = (pf8bit, pf16bit, pf24bit, pf32bit);

 PAggTransformations = ^TAggTransformations;
 TAggTransformations = record
   affineMatrix : array[0..5 ] of double;

  end;

 TAggRasterizerGamma = object(vertex_source )
   m_alpha : gamma_multiply;
   m_gamma : gamma_power;

   constructor Construct(alpha ,gamma : double );

   function func_operator_gamma(x : double ) : double; virtual;

  end;

 PAggImage = ^TAggImage;
 TAggImage = object
   renBuf : rendering_buffer;

   constructor Construct;
   destructor  Destruct;

   function  attach(bitmap : TfpgImage; flip : boolean ) : boolean;

   function  width : int;
   function  height : int;

  end;

 TAgg2D = class(TfpgCanvasBase)
  private
   m_rbuf : rendering_buffer;
   m_pixf : TPixelFormat;

   m_pixFormat ,m_pixFormatComp ,m_pixFormatPre ,m_pixFormatCompPre : pixel_formats;
   m_renBase   ,m_renBaseComp   ,m_renBasePre   ,m_renBaseCompPre   : renderer_base;

   m_renSolid ,m_renSolidComp : renderer_scanline_aa_solid;

   m_allocator : span_allocator;
   m_clipBox   : TAggRectD;

   m_blendMode ,m_imageBlendMode : TAggBlendMode;

   m_imageBlendColor : TAggColor;

   m_scanline   : scanline_u8;
   m_rasterizer : rasterizer_scanline_aa;

   m_masterAlpha ,m_antiAliasGamma : double;

   m_fillColor ,m_lineColor : TAggColor;

   m_fillGradient ,m_lineGradient : pod_auto_array;

   m_lineCap  : TAggLineCap;
   m_lineJoin : TAggLineJoin;

   m_fillGradientFlag ,m_lineGradientFlag : TAggGradient;

   m_fillGradientMatrix ,m_lineGradientMatrix : trans_affine;

   m_fillGradientD1 ,
   m_lineGradientD1 ,
   m_fillGradientD2 ,
   m_lineGradientD2 ,
   m_textAngle      : double;
   m_textAlignX     ,
   m_textAlignY     : TAggTextAlignment;
   m_textHints      : boolean;
   m_fontHeight     ,
   m_fontAscent     ,
   m_fontDescent    : double;
   m_fontCacheType  : TAggFontCacheType;

   m_imageFilter    : TAggImageFilter;
   m_imageResample  : TAggImageResample;
   m_imageFilterLut : image_filter_lut;

   m_fillGradientInterpolator ,
   m_lineGradientInterpolator : span_interpolator_linear;

   m_linearGradientFunction : gradient_x;
   m_radialGradientFunction : gradient_circle;

   m_lineWidth   : double;
   m_evenOddFlag : boolean;

   m_path      : path_storage;
   m_transform : trans_affine;

   m_convCurve  : conv_curve;
   m_convStroke : conv_stroke;
   m_convDash: conv_dash;

   m_pathTransform ,m_strokeTransform : conv_transform;

   m_imageFlip : boolean;

  {$IFDEF AGG2D_USE_WINFONTS }
   m_fontDC : HDC;
  {$ENDIF }

  {$IFNDEF AGG2D_NO_FONT}
   m_fontEngine       : TAggFontEngine;
   m_fontCacheManager : font_cache_manager;
  {$ENDIF}

  // Other Pascal-specific members
   m_gammaNone  : gamma_none;
   m_gammaAgg2D : TAggRasterizerGamma;

   m_ifBilinear    : image_filter_bilinear;
   m_ifHanning     : image_filter_hanning;
   m_ifHermite     : image_filter_hermite;
   m_ifQuadric     : image_filter_quadric;
   m_ifBicubic     : image_filter_bicubic;
   m_ifCatrom      : image_filter_catrom;
   m_ifSpline16    : image_filter_spline16;
   m_ifSpline36    : image_filter_spline36;
   m_ifBlackman144 : image_filter_blackman144;
  protected
    FImg: TfpgImage;

  {$undef uses_interface}
  {$define agg_platform_interface}
  {$undef uses_implementation}
  {$undef agg_platform_implementation}
    {$IFDEF WINDOWS}
      {$I agg_platform_gdi.inc}
    {$ENDIF}
    {$IFDEF UNIX}
      {$I agg_platform_x11.inc}
    {$ENDIF}

    // ------ TfpgCanvasBase implementation requirements ---------
    procedure   DoSetFontRes(fntres: TfpgFontResourceBase); override;
    procedure   DoSetTextColor(cl: TfpgColor); override;
    procedure   DoSetColor(cl: TfpgColor); override;
    procedure   DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle); override;
    procedure   DoGetWinRect(out r: TfpgRect); override;
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
    procedure   DoBeginDraw(awin: TfpgWindowBase; buffered: boolean); override;
    procedure   DoPutBufferToScreen(x, y, w, h: TfpgCoord); override;
    procedure   DoEndDraw; override;
    function    GetPixel(X, Y: integer): TfpgColor; override;
    procedure   SetPixel(X, Y: integer; const AValue: TfpgColor); override;
    procedure   DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: Extended); override;
    procedure   DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended); override;
    procedure   DoDrawPolygon(Points: PPoint; NumPts: Integer; Winding: boolean = False); override;
    // -------- TfpgCanvasBase  end  ---------------

  public
   constructor Create(awin: TfpgWindowBase); override;
   destructor  Destroy; override;

  // Vector Graphics Engine Initialization
   function  Attach(bitmap : TfpgImage; flip_y : boolean = false ) : boolean;

   procedure ClearAll(c : TAggColor ); overload;
   procedure ClearAll(r ,g ,b : byte; a : byte = 255 ); overload;

  // Master Rendering Properties
   procedure BlendMode(m : TAggBlendMode ); overload;
   function  BlendMode : TAggBlendMode; overload;

   procedure MasterAlpha(a : double ); overload;
   function  MasterAlpha : double; overload;

   procedure AntiAliasGamma(g : double ); overload;
   function  AntiAliasGamma : double; overload;

   procedure FillColor(c : TAggColor ); overload;
   procedure FillColor(r ,g ,b : byte; a : byte = 255 ); overload;
   procedure NoFill;

   procedure LineColor(c : TAggColor ); overload;
   procedure LineColor(r ,g ,b : byte; a : byte = 255 ); overload;
   procedure NoLine;

   function  FillColor : TAggColor; overload;
   function  LineColor : TAggColor; overload;

   procedure FillLinearGradient(const x1 ,y1 ,x2 ,y2 : double; c1 ,c2 : TAggColor; profile : double = 1.0 );
   procedure LineLinearGradient(const x1 ,y1 ,x2 ,y2 : double; c1 ,c2 : TAggColor; profile : double = 1.0 );

   procedure FillRadialGradient(const x ,y ,r : double; c1 ,c2 : TAggColor; profile : double = 1.0 ); overload;
   procedure LineRadialGradient(const x ,y ,r : double; c1 ,c2 : TAggColor; profile : double = 1.0 ); overload;

   procedure FillRadialGradient(const x ,y ,r : double; c1 ,c2 ,c3 : TAggColor ); overload;
   procedure LineRadialGradient(const x ,y ,r : double; c1 ,c2 ,c3 : TAggColor ); overload;

   procedure FillRadialGradient(const x ,y ,r : double ); overload;
   procedure LineRadialGradient(const x ,y ,r : double ); overload;

   procedure LineWidth(const w : double ); overload;
   function  LineWidth : double; overload;

   procedure LineCap(cap : TAggLineCap ); overload;
   function  LineCap : TAggLineCap; overload;

   procedure LineJoin(join : TAggLineJoin ); overload;
   function  LineJoin : TAggLineJoin; overload;

   procedure FillEvenOdd(evenOddFlag : boolean ); overload;
   function  FillEvenOdd : boolean; overload;

  // Affine Transformations
   function  Transformations : TAggTransformations; overload;
   procedure Transformations(tr : PAggTransformations ); overload;
   procedure ResetTransformations;

   procedure Affine(const tr : PAggAffine ); overload;
   procedure Affine(const tr : PAggTransformations ); overload;

   procedure Rotate   (const angle : double );
   procedure Scale    (const sx ,sy : double );
   procedure Skew     (const sx ,sy : double );
   procedure Translate(const x ,y : double );

   procedure Parallelogram(const x1 ,y1 ,x2 ,y2 : double; para : PDouble );

   procedure Viewport(const
              worldX1  ,worldY1  ,worldX2  ,worldY2 ,
              screenX1 ,screenY1 ,screenX2 ,screenY2 : double;
              const opt : TAggViewportOption = AGG_XMidYMid );

  // Coordinates Conversions
   procedure WorldToScreen(x ,y : PDouble ); overload;
   procedure ScreenToWorld(x ,y : PDouble ); overload;
   function  WorldToScreen(scalar : double ) : double; overload;
   function  ScreenToWorld(scalar : double ) : double; overload;

   procedure AlignPoint(x ,y : PDouble );

  // Clipping
   procedure ClipBox(x1 ,y1 ,x2 ,y2 : double ); overload;
   function  ClipBox : TAggRectD; overload;

   procedure ClearClipBox(c : TAggColor ); overload;
   procedure ClearClipBox(r ,g ,b : byte; a : byte = 255 ); overload;

   function  InBox(worldX ,worldY : double ) : boolean;

  // Basic Shapes
   procedure Line(const x1, y1, x2, y2: double; AFixAlignment: boolean = false );
   procedure Triangle (const x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double );
   procedure Rectangle(const x1 ,y1 ,x2 ,y2 : double; AFixAlignment: boolean = false);

   procedure RoundedRect(const x1 ,y1 ,x2 ,y2 ,r : double ); overload;
   procedure RoundedRect(const x1 ,y1 ,x2 ,y2 ,rx ,ry : double ); overload;
   procedure RoundedRect(const
              x1 ,y1 ,x2 ,y2 ,
              rxBottom ,ryBottom ,
              rxTop ,ryTop : double ); overload;

   procedure Ellipse(const cx ,cy ,rx ,ry : double );

   procedure Arc (const cx ,cy ,rx ,ry ,start ,sweep : double );
   procedure Star(const cx ,cy ,r1 ,r2 ,startAngle : double; const numRays : integer );

   procedure Curve(const x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double ); overload;
   procedure Curve(const x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double ); overload;

   procedure Polygon (const xy : PDouble; numPoints : integer );
   procedure Polyline(const xy : PDouble; numPoints : integer );

  // Path Commands
   procedure ResetPath;

   procedure MoveTo (const x ,y : double );
   procedure MoveRel(const dx ,dy : double );

   procedure LineTo (const x ,y : double );
   procedure LineRel(const dx ,dy : double );

   procedure HorLineTo (const x : double );
   procedure HorLineRel(const dx : double );

   procedure VerLineTo (const y : double );
   procedure VerLineRel(const dy : double );

   procedure ArcTo(
              rx ,ry ,angle : double;
              largeArcFlag ,sweepFlag : boolean;
              x ,y : double );

   procedure ArcRel(
              rx ,ry ,angle : double;
              largeArcFlag ,sweepFlag : boolean;
              dx ,dy : double );

   procedure QuadricCurveTo (xCtrl ,yCtrl ,xTo ,yTo : double ); overload;
   procedure QuadricCurveRel(dxCtrl ,dyCtrl ,dxTo ,dyTo : double ); overload;
   procedure QuadricCurveTo (xTo ,yTo : double ); overload;
   procedure QuadricCurveRel(dxTo ,dyTo : double ); overload;

   procedure CubicCurveTo (xCtrl1 ,yCtrl1 ,xCtrl2 ,yCtrl2 ,xTo ,yTo : double ); overload;
   procedure CubicCurveRel(dxCtrl1 ,dyCtrl1 ,dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo : double ); overload;
   procedure CubicCurveTo (xCtrl2 ,yCtrl2 ,xTo ,yTo : double ); overload;
   procedure CubicCurveRel(dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo : double ); overload;

   procedure AddEllipse(cx ,cy ,rx ,ry : double; dir : TAggDirection );
   procedure ClosePolygon;

   procedure DrawPath(flag : TAggDrawPathFlag = AGG_FillAndStroke );

  // Text Rendering
   procedure FlipText(const flip : boolean );

   procedure Font(
              fileName : AnsiString; height : double;
              bold : boolean = false;
              italic : boolean = false;
              cache : TAggFontCacheType = AGG_VectorFontCache;
              angle : double = 0.0 );

   function  FontHeight : double;

   procedure TextAlignment(alignX ,alignY : TAggTextAlignment );

   function  TextHints : boolean; overload;
   procedure TextHints(hints : boolean ); overload;
   function  TextWidth(str : AnsiString ) : double;

   procedure Text(
              x ,y : double; str : AnsiString;
              roundOff : boolean = false;
              ddx : double = 0.0;
              ddy : double = 0.0 );

  // Image Rendering
   procedure ImageFilter(f : TAggImageFilter ); overload;
   function  ImageFilter : TAggImageFilter; overload;

   procedure ImageResample(f : TAggImageResample ); overload;
   function  ImageResample : TAggImageResample; overload;

   procedure ImageFlip(f : boolean );

   procedure TransformImage(
              bitmap : TfpgImage;
              imgX1 ,imgY1 ,imgX2 ,imgY2 : integer;
              dstX1 ,dstY1 ,dstX2 ,dstY2 : double ); overload;

   procedure TransformImage(
              bitmap : TfpgImage;
              dstX1 ,dstY1 ,dstX2 ,dstY2 : double ); overload;

   procedure TransformImage(
              bitmap : TfpgImage;
              imgX1 ,imgY1 ,imgX2 ,imgY2 : integer;
              parallelo : PDouble ); overload;

   procedure TransformImage(bitmap : TfpgImage; parallelo : PDouble ); overload;

   procedure TransformImagePath(
              bitmap : TfpgImage;
              imgX1 ,imgY1 ,imgX2 ,imgY2 : integer;
              dstX1 ,dstY1 ,dstX2 ,dstY2 : double ); overload;

   procedure TransformImagePath(
              bitmap : TfpgImage;
              dstX1 ,dstY1 ,dstX2 ,dstY2 : double ); overload;

   procedure TransformImagePath(
              bitmap : TfpgImage;
              imgX1 ,imgY1 ,imgX2 ,imgY2 : integer;
              parallelo : PDouble ); overload;

   procedure TransformImagePath(bitmap : TfpgImage; parallelo : PDouble ); overload;

   procedure CopyImage(
              bitmap : TfpgImage;
              imgX1 ,imgY1 ,imgX2 ,imgY2 : integer;
              dstX ,dstY : double ); overload;

   procedure CopyImage(bitmap : TfpgImage; dstX ,dstY : double ); overload;

  private
   procedure render(fillColor_ : boolean ); overload;
   procedure render(ras : PAggFontRasterizer; sl : PAggFontScanline ); overload;

   procedure addLine(x1 ,y1 ,x2 ,y2 : double );
   procedure updateRasterizerGamma;
   procedure renderImage(
              img : PAggImage;
              x1 ,y1 ,x2 ,y2 : integer;
              parl : PDouble );

  end;

{ GLOBAL PROCEDURES }
// Standalone API
 function  Deg2Rad(v : double ) : double;
 function  Rad2Deg(v : double ) : double;

 function  Agg2DUsesFreeType : boolean;
 function  Agg2DUsesWin32TrueType : boolean;

 function  BitmapAlphaTransparency(bitmap : TfpgImage; alpha : byte ) : boolean;

 function  fpgColor2AggColor(c: TfpgColor): TAggColor;
 

IMPLEMENTATION

uses
  // This allows for platform specific uses clauses
  {$undef uses_interface}
  {$undef agg_platform_interface}
  {$define uses_implementation}
  {$undef agg_platform_implementation}
  {$IFDEF WINDOWS}
    {$I agg_platform_gdi.inc}
  {$ENDIF}
  {$IFDEF UNIX}
    {$I agg_platform_x11.inc}
  {$ENDIF}
  fpg_stringutils;

{ LOCAL VARIABLES & CONSTANTS }
var
 g_approxScale : double = 2.0;

type
 PAggSpanConvImageBlend = ^TAggSpanConvImageBlend;
 TAggSpanConvImageBlend = object(span_convertor )
  private
   m_mode  : TAggBlendMode;
   m_color : TAggColor;
   m_pixel : pixel_formats_ptr; // m_pixFormatCompPre

  public
   constructor Construct(m : TAggBlendMode; c : TAggColor; p : pixel_formats_ptr );

   procedure convert(span : aggclr_ptr; x ,y : int; len : unsigned ); virtual;

  end;

{ OPERATOR_IS_EQUAL }
function operator_is_equal(c1 ,c2 : PAggColor ) : boolean;
begin
 result:=
  (c1.r = c2.r ) and
  (c1.g = c2.g ) and
  (c1.b = c2.b ) and
  (c1.a = c2.a );

end;

{ OPERATOR_IS_NOT_EQUAL }
function operator_is_not_equal(c1 ,c2 : PAggColor ) : boolean;
begin
 result:=not operator_is_equal(c1 ,c2 );

end;

{ AGG2DRENDERER_RENDER }
procedure Agg2DRenderer_render(
           gr : TAgg2D;
           renBase : renderer_base_ptr;
           renSolid : renderer_scanline_aa_solid_ptr;
           fillColor_ : boolean ); overload;
var
 span : span_gradient;
 ren  : renderer_scanline_aa;
 clr  : aggclr;

begin
 if (fillColor_ and
     (gr.m_fillGradientFlag = AGG_Linear ) ) or
    (not fillColor_ and
     (gr.m_lineGradientFlag = AGG_Linear ) ) then
  if fillColor_ then
   begin
    span.Construct(
     @gr.m_allocator ,
     @gr.m_fillGradientInterpolator ,
     @gr.m_linearGradientFunction ,
     @gr.m_fillGradient ,
     gr.m_fillGradientD1 ,
     gr.m_fillGradientD2 );

    ren.Construct   (renBase ,@span );
    render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ren );

   end
  else
   begin
    span.Construct(
     @gr.m_allocator ,
     @gr.m_lineGradientInterpolator ,
     @gr.m_linearGradientFunction ,
     @gr.m_lineGradient ,
     gr.m_lineGradientD1 ,
     gr.m_lineGradientD2 );

    ren.Construct   (renBase ,@span );
    render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ren );

   end
 else
  if (fillColor_ and
      (gr.m_fillGradientFlag = AGG_Radial ) ) or
     (not fillColor_ and
      (gr.m_lineGradientFlag = AGG_Radial ) ) then
   if fillColor_ then
    begin
     span.Construct(
      @gr.m_allocator ,
      @gr.m_fillGradientInterpolator ,
      @gr.m_radialGradientFunction ,
      @gr.m_fillGradient ,
      gr.m_fillGradientD1 ,
      gr.m_fillGradientD2 );

      ren.Construct   (renBase ,@span );
      render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ren );

    end
   else
    begin
     span.Construct(
      @gr.m_allocator ,
      @gr.m_lineGradientInterpolator ,
      @gr.m_radialGradientFunction ,
      @gr.m_lineGradient ,
      gr.m_lineGradientD1 ,
      gr.m_lineGradientD2 );

     ren.Construct   (renBase ,@span );
     render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ren );

    end
  else
   begin
    if fillColor_ then
     clr.Construct(gr.m_fillColor )
    else
     clr.Construct(gr.m_lineColor );

    renSolid.color_ (@clr );
    render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,renSolid );

   end;

end;

{ AGG2DRENDERER_RENDER }
procedure Agg2DRenderer_render(
           gr : TAgg2D;
           renBase : renderer_base_ptr;
           renSolid : renderer_scanline_aa_solid_ptr;
           ras : gray8_adaptor_type_ptr;
           sl : gray8_scanline_type_ptr ); overload;
var
 span : span_gradient;
 ren  : renderer_scanline_aa;
 clr  : aggclr;

begin
 if gr.m_fillGradientFlag = AGG_Linear then
  begin
   span.Construct(
    @gr.m_allocator ,
    @gr.m_fillGradientInterpolator ,
    @gr.m_linearGradientFunction ,
    @gr.m_fillGradient ,
    gr.m_fillGradientD1 ,
    gr.m_fillGradientD2 );

   ren.Construct   (renBase ,@span );
   render_scanlines(ras ,sl ,@ren );

  end
 else
  if gr.m_fillGradientFlag = AGG_Radial then
   begin
    span.Construct(
     @gr.m_allocator ,
     @gr.m_fillGradientInterpolator ,
     @gr.m_radialGradientFunction ,
     @gr.m_fillGradient ,
     gr.m_fillGradientD1 ,
     gr.m_fillGradientD2 );

    ren.Construct   (renBase ,@span );
    render_scanlines(ras ,sl ,@ren );

   end
  else
   begin
    clr.Construct   (gr.m_fillColor );
    renSolid.color_ (@clr );
    render_scanlines(ras ,sl ,renSolid );

   end;

end;

{ AGG2DRENDERER_RENDERIMAGE }
procedure Agg2DRenderer_renderImage(
           gr : TAgg2D;
           img : PAggImage;
           renBase : renderer_base_ptr;
           interpolator : span_interpolator_linear_ptr );
var
 blend : TAggSpanConvImageBlend;

 si : span_image_filter_rgba;
 sg : span_image_filter_rgba_nn;
 sb : span_image_filter_rgba_bilinear;
 s2 : span_image_filter_rgba_2x2;
 sa : span_image_resample_rgba_affine;
 sc : span_converter;
 ri : renderer_scanline_aa;

 clr : aggclr;

 resample : boolean;

 sx ,sy : double;

begin
 case gr.m_pixf of
  pf32bit :
   blend.Construct(gr.m_imageBlendMode ,gr.m_imageBlendColor ,@gr.m_pixFormatCompPre );

  else
   blend.Construct(gr.m_imageBlendMode ,gr.m_imageBlendColor ,NIL );

 end;

 if gr.m_imageFilter = AGG_NoFilter then
  begin
   clr.ConstrInt(0 ,0 ,0 ,0 );
   sg.Construct (@gr.m_allocator ,@img.renBuf ,@clr ,interpolator ,rgba_order );
   sc.Construct (@sg ,@blend );
   ri.Construct (renBase ,@sc );

   render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ri );

  end
 else
  begin
   resample:=gr.m_imageResample = AGG_ResampleAlways;

   if gr.m_imageResample = AGG_ResampleOnZoomOut then
    begin
     interpolator._transformer.scaling_abs(@sx ,@sy );

     if (sx > 1.125 ) or
        (sy > 1.125 ) then
      resample:=true;

    end;

   if resample then
    begin
     clr.ConstrInt(0 ,0 ,0 ,0 );
     sa.Construct(
      @gr.m_allocator ,
      @img.renBuf ,
      @clr ,
      interpolator ,
      @gr.m_imageFilterLut ,
      rgba_order );

     sc.Construct(@sa ,@blend );
     ri.Construct(renBase ,@sc );

     render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ri );

    end
   else
    if gr.m_imageFilter = AGG_Bilinear then
     begin
      clr.ConstrInt(0 ,0 ,0 ,0 );
      sb.Construct(
       @gr.m_allocator ,
       @img.renBuf ,
       @clr ,
       interpolator ,
       rgba_order );

      sc.Construct(@sb ,@blend );
      ri.Construct(renBase ,@sc );

      render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ri );

     end
    else
     if gr.m_imageFilterLut.diameter = 2 then
      begin
       clr.ConstrInt(0 ,0 ,0 ,0 );
       s2.Construct(
        @gr.m_allocator ,
        @img.renBuf ,
        @clr ,
        interpolator,
        @gr.m_imageFilterLut ,
        rgba_order );

       sc.Construct(@s2 ,@blend );
       ri.Construct(renBase ,@sc );

       render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ri );

      end
     else
      begin
       clr.ConstrInt(0 ,0 ,0 ,0 );
       si.Construct(
        @gr.m_allocator ,
        @img.renBuf ,
        @clr ,
        interpolator ,
        @gr.m_imageFilterLut ,
        rgba_order );

       sc.Construct(@si ,@blend );
       ri.Construct(renBase ,@sc );

       render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ri );

      end;

  end;

end;

{ AGG2DUSESFREETYPE }
function Agg2DUsesFreeType : boolean;
begin
{$IFDEF AGG2D_USE_FREETYPE }
 result:=true;
{$ELSE }
 result:=false;
{$ENDIF }
end;

function Agg2DUsesWin32TrueType: boolean;
begin
{$IFDEF AGG2D_USE_WINFONTS }
 result:=true;
{$ELSE}
 result:=false;
{$ENDIF }
end;

function fpgColor2AggColor(c: TfpgColor): TAggColor;
var
  t: TRGBTriple;
  c1: TfpgColor;
begin
  t := fpgColorToRGBTriple(c);
  Result.Construct(t.Red, t.Green, t.Blue, t.Alpha);
end;

function ColorDepthToPixelFormat(const AColorDepth: integer): TPixelFormat;
begin
  case AColorDepth of
    8: Result := pf8bit;
    16: Result := pf16bit;
    24: Result := pf24bit;
    32: Result := pf32bit;
    else
      raise Exception.Create('Unknown ColorDepth parameter in ColorDepthToPixelFormat');
  end;
end;

{ CONSTRUCT }
constructor TAggSpanConvImageBlend.Construct(m : TAggBlendMode; c : TAggColor; p : pixel_formats_ptr );
begin
 m_mode :=m;
 m_color:=c;
 m_pixel:=p;

end;

{ CONVERT }
procedure TAggSpanConvImageBlend.convert(span : aggclr_ptr; x ,y : int; len : unsigned );
var
 l2 ,a : unsigned;

 s2 : PAggColor;

begin
 if (m_mode <> AGG_BlendDst ) and
    (m_pixel <> NIL ) then
  begin{!}
   l2:=len;
   s2:=PAggColor(span );

   repeat
    comp_op_adaptor_clip_to_dst_rgba_pre(
     m_pixel ,
     unsigned(m_mode ) ,
     int8u_ptr(s2 ) ,
     m_color.r ,
     m_color.g ,
     m_color.b ,
     base_mask ,
     cover_full );

    inc(ptrcomp(s2 ) ,sizeof(aggclr ) );
    dec(l2 );

   until l2 = 0;

  end;

 if m_color.a < base_mask then
  begin
   l2:=len;
   s2:=PAggColor(span );
   a :=m_color.a;

   repeat
    s2.r:=(s2.r * a ) shr base_shift;
    s2.g:=(s2.g * a ) shr base_shift;
    s2.b:=(s2.b * a ) shr base_shift;
    s2.a:=(s2.a * a ) shr base_shift;

    inc(ptrcomp(s2 ) ,sizeof(aggclr ) );
    dec(l2 );

   until l2 = 0;

  end;

end;

{ DEG2RAD }
function deg2Rad(v : double ) : double;
begin
 result:=v * agg_basics.pi / 180.0;

end;

{ RAD2DEG }
function rad2Deg(v : double ) : double;
begin
 result:=v * 180.0 / agg_basics.pi;

end;


{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor TAggImage.Construct;
begin
 renBuf.Construct;

end;

{ DESTRUCT }
destructor TAggImage.Destruct;
begin
 renBuf.Destruct;

end;

{ ATTACH }
function TAggImage.attach(bitmap : TfpgImage; flip : boolean ) : boolean;
var
 buffer : pointer;
 stride : integer;

begin
 result:=false;

 if Assigned(bitmap ) (* and
    not bitmap.Empty *)then        {$Note Implement TfpgImage.Empty }
  case bitmap.ColorDepth of
   32 :
    begin
    { Rendering Buffer }
     stride:=integer(bitmap.ScanLine[1 ] ) - integer(bitmap.ScanLine[0 ] );

     if stride < 0 then
      buffer:=bitmap.ScanLine[bitmap.Height - 1 ]
     else
      buffer:=bitmap.ScanLine[0 ];

     if flip then
      stride:=stride * -1;

     renBuf.attach(
      buffer ,
      bitmap.Width ,
      bitmap.Height ,
      stride );

    { OK }
     result:=true; 

    end;

  end;

end;

{ WIDTH }
function TAggImage.width : int;
begin
 result:=renBuf._width;

end;

{ HEIGHT }
function TAggImage.height : int;
begin
 result:=renBuf._height;

end;

{ CONSTRUCT }
constructor TAggRasterizerGamma.Construct(alpha ,gamma : double );
begin
 m_alpha.Construct(alpha );
 m_gamma.Construct(gamma );

end;

{ FUNC_OPERATOR_GAMMA }
function TAggRasterizerGamma.func_operator_gamma(x : double ) : double;
begin
 result:=m_alpha.func_operator_gamma(m_gamma.func_operator_gamma(x ) );

end;

{$undef uses_interface}
{$undef agg_platform_interface}
{$undef uses_implementation}
{$define agg_platform_implementation}
{$IFDEF WINDOWS}
  {$I agg_platform_gdi.inc}
{$ENDIF}
{$IFDEF UNIX}
  {$I agg_platform_x11.inc}
{$ENDIF}


{ CREATE }
constructor TAgg2D.Create(awin: TfpgWindowBase);
begin
  inherited Create(awin);

  FLineWidth := 1;
 m_rbuf.Construct;

 m_pixf:=pf32bit;

 pixfmt_rgba32           (m_pixFormat ,@m_rbuf );
 pixfmt_custom_blend_rgba(m_pixFormatComp ,@m_rbuf ,@comp_op_adaptor_rgba ,rgba_order );
 pixfmt_rgba32           (m_pixFormatPre ,@m_rbuf );
 pixfmt_custom_blend_rgba(m_pixFormatCompPre ,@m_rbuf ,@comp_op_adaptor_rgba ,rgba_order );

 m_renBase.Construct       (@m_pixFormat );
 m_renBaseComp.Construct   (@m_pixFormatComp );
 m_renBasePre.Construct    (@m_pixFormatPre );
 m_renBaseCompPre.Construct(@m_pixFormatCompPre );

 m_renSolid.Construct    (@m_renBase );
 m_renSolidComp.Construct(@m_renBaseComp );

 m_allocator.Construct;
 m_clipBox.Construct(0 ,0 ,0 ,0 );

 m_blendMode     :=AGG_BlendAlpha;
 m_imageBlendMode:=AGG_BlendDst;

 m_imageBlendColor.Construct(0 ,0 ,0 );

 m_scanline.Construct;
 m_rasterizer.Construct;

 m_masterAlpha   :=1.0;
 m_antiAliasGamma:=1.0;

 m_fillColor.Construct(255 ,255 ,255 );
 m_lineColor.Construct(0   ,0   ,0 );

 m_fillGradient.Construct(256 ,sizeof(aggclr ) );
 m_lineGradient.Construct(256 ,sizeof(aggclr ) );

 m_lineCap :=AGG_CapRound;
 m_lineJoin:=AGG_JoinRound;

 m_fillGradientFlag:=AGG_Solid;
 m_lineGradientFlag:=AGG_Solid;

 m_fillGradientMatrix.Construct;
 m_lineGradientMatrix.Construct;

 m_fillGradientD1:=0.0;
 m_lineGradientD1:=0.0;
 m_fillGradientD2:=100.0;
 m_lineGradientD2:=100.0;

 m_textAngle  :=0.0;
 m_textAlignX :=AGG_AlignLeft;
 m_textAlignY :=AGG_AlignBottom;
 m_textHints  :=true;
 m_fontHeight :=0.0;
 m_fontAscent :=0.0;
 m_fontDescent:=0.0;

 m_fontCacheType:=AGG_RasterFontCache;
 m_imageFilter  :=AGG_Bilinear;
 m_imageResample:=AGG_NoResample;

 m_gammaNone.Construct;

 m_ifBilinear.Construct;
 m_ifHanning.Construct;
 m_ifHermite.Construct;
 m_ifQuadric.Construct;
 m_ifBicubic.Construct;
 m_ifCatrom.Construct;
 m_ifSpline16.Construct;
 m_ifSpline36.Construct;
 m_ifBlackman144.Construct;

 m_imageFilterLut.Construct(@m_ifBilinear ,true );

 m_linearGradientFunction.Construct;
 m_radialGradientFunction.Construct;

 m_fillGradientInterpolator.Construct(@m_fillGradientMatrix );
 m_lineGradientInterpolator.Construct(@m_lineGradientMatrix );

 m_lineWidth  :=1;
 m_evenOddFlag:=false;

 m_imageFlip:=false;

 m_path.Construct;
 m_transform.Construct;

 m_convCurve.Construct (@m_path );
 m_convDash.Construct(@m_convCurve);
 m_convStroke.Construct(@m_convDash );

 m_pathTransform.Construct  (@m_convCurve ,@m_transform );
 m_strokeTransform.Construct(@m_convStroke ,@m_transform );

{$IFDEF AGG2D_USE_FREETYPE }
 m_fontEngine.Construct;
{$ENDIF }
{$IFDEF AGG2D_USE_WINFONTS}
 m_fontDC:=GetDC(0 );
 m_fontEngine.Construct(m_fontDC );
{$ENDIF }

{$IFNDEF AGG2D_NO_FONT}
 m_fontCacheManager.Construct(@m_fontEngine );
{$ENDIF}

 LineCap (m_lineCap );
 LineJoin(m_lineJoin );
end;

{ DESTROY }
destructor TAgg2D.Destroy;
begin
 m_rbuf.Destruct;

 m_allocator.Destruct;

 m_scanline.Destruct;
 m_rasterizer.Destruct;

 m_fillGradient.Destruct;
 m_lineGradient.Destruct;

 m_imageFilterLut.Destruct;
 m_path.Destruct;

 m_convCurve.Destruct;
 m_convStroke.Destruct;
 m_convDash.Destruct;

 {$IFNDEF AGG2D_NO_FONT}
 m_fontEngine.Destruct;
 m_fontCacheManager.Destruct;
 {$ENDIF}

 {$IFDEF AGG2D_USE_WINFONTS}
 ReleaseDC(0 ,m_fontDC );
 {$ENDIF }

 if Assigned(FImg) then
   FImg.Free;
end;

{ ATTACH }
function TAgg2D.Attach(bitmap : TfpgImage; flip_y : boolean = false ) : boolean;
var
 buffer : pointer;
 stride : integer;

begin
 result:=false;

 if Assigned(bitmap )
  {and
    not bitmap.Empty }then    {$Warning Implement bitmap.Emtpy }
  case bitmap.ColorDepth of
   24,
   32:
    begin
    { Rendering Buffer }
     stride:=integer(bitmap.ScanLine[1 ] ) - integer(bitmap.ScanLine[0 ] );

     if stride < 0 then
      buffer:=bitmap.ScanLine[bitmap.Height - 1 ]
     else
      buffer:=bitmap.ScanLine[0 ];

     if flip_y then
      stride:=stride * -1;

     m_rbuf.attach(
      buffer ,
      bitmap.Width ,
      bitmap.Height ,
      stride );

     { Pixel Format }
     m_pixf :=  ColorDepthToPixelFormat(bitmap.ColorDepth);

     case m_pixf of
      pf24bit :
       begin
        pixfmt_rgb24(m_pixFormat ,@m_rbuf );
        pixfmt_rgb24(m_pixFormatPre ,@m_rbuf );

       end;

      pf32bit :
       begin
        pixfmt_rgba32           (m_pixFormat ,@m_rbuf );
        pixfmt_custom_blend_rgba(m_pixFormatComp ,@m_rbuf ,@comp_op_adaptor_rgba ,rgba_order );
        pixfmt_rgba32           (m_pixFormatPre ,@m_rbuf );
        pixfmt_custom_blend_rgba(m_pixFormatCompPre ,@m_rbuf ,@comp_op_adaptor_rgba ,rgba_order );

       end;

     end;

    { Reset state }
     m_renBase.reset_clipping       (true );
     m_renBaseComp.reset_clipping   (true );
     m_renBasePre.reset_clipping    (true );
     m_renBaseCompPre.reset_clipping(true );

     ResetTransformations;

     LineWidth(1.0 );
     LineColor(0   ,0   ,0 );
     FillColor(255 ,255 ,255 );

     TextAlignment(AGG_AlignLeft ,AGG_AlignBottom );

     ClipBox (0 ,0 ,bitmap.Width ,bitmap.Height );
     LineCap (AGG_CapRound );
     LineJoin(AGG_JoinRound );
     FlipText(false );

     ImageFilter  (AGG_Bilinear );
     ImageResample(AGG_NoResample );
     ImageFlip    (false );

     m_masterAlpha   :=1.0;
     m_antiAliasGamma:=1.0;

     m_rasterizer.gamma(@m_gammaNone );

     m_blendMode:=AGG_BlendAlpha;

     FillEvenOdd(false );
     BlendMode  (AGG_BlendAlpha );

     FlipText(false );
     ResetPath;

     ImageFilter  (AGG_Bilinear );
     ImageResample(AGG_NoResample );

    { OK }
     result:=true;

    end;

  end;

end;

{ CLIPBOX }
procedure TAgg2D.ClipBox(x1 ,y1 ,x2 ,y2 : double );
var
 rx1 ,ry1 ,rx2 ,ry2 : int;

begin
 m_clipBox.Construct(x1 ,y1 ,x2 ,y2 );

 rx1:=Trunc(x1 );
 ry1:=Trunc(y1 );
 rx2:=Trunc(x2 );
 ry2:=Trunc(y2 );

 m_renBase.clip_box_       (rx1 ,ry1 ,rx2 ,ry2 );
 m_renBaseComp.clip_box_   (rx1 ,ry1 ,rx2 ,ry2 );
 m_renBasePre.clip_box_    (rx1 ,ry1 ,rx2 ,ry2 );
 m_renBaseCompPre.clip_box_(rx1 ,ry1 ,rx2 ,ry2 );

 m_rasterizer.clip_box(x1 ,y1 ,x2 ,y2 );

end;

{ CLIPBOX }
function TAgg2D.ClipBox : TAggRectD;
begin
 result:=m_clipBox;

end;

{ CLEARALL }
procedure TAgg2D.ClearAll(c : TAggColor );
var
 clr : aggclr;

begin
 clr.Construct  (c );
 m_renBase.clear(@clr );

end;

{ CLEARALL }
procedure TAgg2D.ClearAll(r ,g ,b : byte; a : byte = 255 );
var
 clr : TAggColor;

begin
 clr.Construct(r ,g ,b ,a );
 ClearAll     (clr );

end;

{ CLEARCLIPBOX }
procedure TAgg2D.ClearClipBox(c : TAggColor );
var
 clr : aggclr;

begin
 clr.Construct(c );

 m_renBase.copy_bar(0 ,0 ,m_renBase.width ,m_renBase.height ,@clr );

end;

{ CLEARCLIPBOX }
procedure TAgg2D.ClearClipBox(r ,g ,b : byte; a : byte = 255 );
var
 clr : TAggColor;

begin
 clr.Construct(r ,g ,b ,a );
 ClearClipBox (clr );

end;

{ WORLDTOSCREEN }
procedure TAgg2D.WorldToScreen(x ,y : PDouble );
begin
 m_transform.transform(@m_transform ,double_ptr(x ) ,double_ptr(y ) );

end;

{ SCREENTOWORLD }
procedure TAgg2D.ScreenToWorld(x ,y : PDouble );
begin
 m_transform.inverse_transform(@m_transform ,double_ptr(x ) ,double_ptr(y ) );

end;

{ WORLDTOSCREEN }
function TAgg2D.WorldToScreen(scalar : double ) : double;
var
 x1 ,y1 ,x2 ,y2 : double;

begin
 x1:=0;
 y1:=0;
 x2:=scalar;
 y2:=scalar;

 WorldToScreen(@x1 ,@y1 );
 WorldToScreen(@x2 ,@y2 );

 result:=Sqrt((x2 - x1 ) * (x2 - x1 ) + (y2 - y1 ) * (y2 - y1 ) ) * 0.7071068;

end;

{ SCREENTOWORLD }
function TAgg2D.ScreenToWorld(scalar : double ) : double;
var
 x1 ,y1 ,x2 ,y2 : double;

begin
 x1:=0;
 y1:=0;
 x2:=scalar;
 y2:=scalar;

 ScreenToWorld(@x1 ,@y1 );
 ScreenToWorld(@x2 ,@y2 );

 result:=Sqrt((x2 - x1 ) * (x2 - x1 ) + (y2 - y1 ) * (y2 - y1 ) ) * 0.7071068;

end;

{ ALIGNPOINT }
procedure TAgg2D.AlignPoint(x ,y : PDouble );
begin
 WorldToScreen(x ,y );

 x^:=Floor(x^ ) + 0.5;
 y^:=Floor(y^ ) + 0.5;

 ScreenToWorld(x ,y );

end;

{ INBOX }
function TAgg2D.InBox(worldX ,worldY : double ) : boolean;
begin
 WorldToScreen(@worldX ,@worldY );

 result:=m_renBase.inbox(Trunc(worldX ) ,Trunc(worldY ) );

end;

{ BLENDMODE }
procedure TAgg2D.BlendMode(m : TAggBlendMode );
begin
 m_blendMode:=m;

 m_pixFormatComp.comp_op_   (unsigned(m ) );
 m_pixFormatCompPre.comp_op_(unsigned(m ) );

end;

{ BLENDMODE }
function TAgg2D.BlendMode : TAggBlendMode;
begin
 result:=m_blendMode;

end;

{ MASTERALPHA }
procedure TAgg2D.MasterAlpha(a : double );
begin
 m_masterAlpha:=a;

 UpdateRasterizerGamma;

end;

{ MASTERALPHA }
function TAgg2D.MasterAlpha : double;
begin
 result:=m_masterAlpha;

end;

{ ANTIALIASGAMMA }
procedure TAgg2D.AntiAliasGamma(g : double );
begin
 m_antiAliasGamma:=g;

 UpdateRasterizerGamma;

end;

{ ANTIALIASGAMMA }
function TAgg2D.AntiAliasGamma : double;
begin
 result:=m_antiAliasGamma;

end;

{ FILLCOLOR }
procedure TAgg2D.FillColor(c : TAggColor );
begin
 m_fillColor       :=c;
 m_fillGradientFlag:=AGG_Solid;

end;

{ FILLCOLOR }
procedure TAgg2D.FillColor(r ,g ,b : byte; a : byte = 255 );
var
 clr : TAggColor;

begin
 clr.Construct(r ,g ,b ,a );
 FillColor    (clr );

end;

{ NOFILL }
procedure TAgg2D.NoFill;
var
 clr : TAggColor;

begin
 clr.Construct(0 ,0 ,0 ,0 );
 FillColor    (clr );

end;

{ LINECOLOR }
procedure TAgg2D.LineColor(c : TAggColor );
begin
 m_lineColor       :=c;
 m_lineGradientFlag:=AGG_Solid;

end;

{ LINECOLOR }
procedure TAgg2D.LineColor(r ,g ,b : byte; a : byte = 255 );
var
 clr : TAggColor;

begin
 clr.Construct(r ,g ,b ,a );
 LineColor    (clr );

end;

{ NOLINE }
procedure TAgg2D.NoLine;
var
 clr : TAggColor;

begin
 clr.Construct(0 ,0 ,0 ,0 );
 LineColor    (clr );

end;

{ FILLCOLOR }
function TAgg2D.FillColor : TAggColor;
begin
 result:=m_fillColor;

end;

{ LINECOLOR }
function TAgg2D.LineColor : TAggColor;
begin
 result:=m_lineColor;

end;

{ FILLLINEARGRADIENT }
procedure TAgg2D.FillLinearGradient(const x1 ,y1 ,x2 ,y2 : double; c1 ,c2 : TAggColor; profile : double = 1.0 );
var
 i ,startGradient ,endGradient : int;

 k ,angle : double;

 c : TAggColor;

 clr : aggclr;
 tar : trans_affine_rotation;
 tat : trans_affine_translation;

begin
 startGradient:=128 - Trunc(profile * 127.0 );
 endGradient  :=128 + Trunc(profile * 127.0 );

 if endGradient <= startGradient then
  endGradient:=startGradient + 1;

 k:=1.0 / (endGradient - startGradient );
 i:=0;

 while i < startGradient do
  begin
   clr.Construct(c1 );

   move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < endGradient do
  begin
   c:=c1.gradient(c2 ,(i - startGradient ) * k );

   clr.Construct(c );

   move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < 256 do
  begin
   clr.Construct(c2 );

   move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 angle:=ArcTan2(y2 - y1 ,x2 - x1 );

 m_fillGradientMatrix.reset;

 tar.Construct(angle );

 m_fillGradientMatrix.multiply(@tar );

 tat.Construct(x1 ,y1 );

 m_fillGradientMatrix.multiply(@tat );
 m_fillGradientMatrix.multiply(@m_transform );
 m_fillGradientMatrix.invert;

 m_fillGradientD1  :=0.0;
 m_fillGradientD2  :=Sqrt((x2 - x1 ) * (x2 - x1 ) + (y2 - y1 ) * (y2 - y1 ) );
 m_fillGradientFlag:=AGG_Linear;

 m_fillColor.Construct(0 ,0 ,0 );  // Set some real color

end;

{ LINELINEARGRADIENT }
procedure TAgg2D.LineLinearGradient(const x1 ,y1 ,x2 ,y2 : double; c1 ,c2 : TAggColor; profile : double = 1.0 );
var
 i ,startGradient ,endGradient : int;

 k ,angle : double;

 c : TAggColor;

 clr : aggclr;
 tar : trans_affine_rotation;
 tat : trans_affine_translation;

begin
 startGradient:=128 - Trunc(profile * 128.0 );
 endGradient  :=128 + Trunc(profile * 128.0 );

 if endGradient <= startGradient then
  endGradient:=startGradient + 1;

 k:=1.0 / (endGradient - startGradient );
 i:=0;

 while i < startGradient do
  begin
   clr.Construct(c1 );

   move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < endGradient do
  begin
   c:=c1.gradient(c2 ,(i - startGradient) * k );

   clr.Construct(c );

   move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < 256 do
  begin
   clr.Construct(c2 );

   move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 angle:=ArcTan2(y2 - y1 ,x2 - x1 );

 m_lineGradientMatrix.reset;

 tar.Construct(angle );

 m_lineGradientMatrix.multiply(@tar );

 tat.Construct(x1 ,y1 );

 m_lineGradientMatrix.multiply(@tat );
 m_lineGradientMatrix.multiply(@m_transform );
 m_lineGradientMatrix.invert;

 m_lineGradientD1  :=0.0;
 m_lineGradientD2  :=Sqrt((x2 - x1 ) * (x2 - x1 ) + (y2 - y1 ) * (y2 - y1 ) );
 m_lineGradientFlag:=AGG_Linear;

 m_lineColor.Construct(0 ,0 ,0 );  // Set some real color

end;

{ FILLRADIALGRADIENT }
procedure TAgg2D.FillRadialGradient(const x ,y ,r : double; c1 ,c2 : TAggColor; profile : double = 1.0 );
var
 i ,startGradient ,endGradient : int;

 k : double;
 c : TAggColor;

 clr : aggclr;
 tat : trans_affine_translation;

begin
 startGradient:=128 - Trunc(profile * 127.0 );
 endGradient  :=128 + Trunc(profile * 127.0 );

 if endGradient <= startGradient then
  endGradient:=startGradient + 1;

 k:=1.0 / (endGradient - startGradient );
 i:=0;

 while i < startGradient do
  begin
   clr.Construct(c1 );

   move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < endGradient do
  begin
   c:=c1.gradient(c2 ,(i - startGradient ) * k );

   clr.Construct(c );

   move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < 256 do
  begin
   clr.Construct(c2 );

   move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 m_fillGradientD2:=worldToScreen(r );

 WorldToScreen(@x ,@y );

 m_fillGradientMatrix.reset;

 tat.Construct(x ,y );

 m_fillGradientMatrix.multiply(@tat );
 m_fillGradientMatrix.invert;

 m_fillGradientD1  :=0;
 m_fillGradientFlag:=AGG_Radial;

 m_fillColor.Construct(0 ,0 ,0 );  // Set some real color

end;

{ LINERADIALGRADIENT }
procedure TAgg2D.LineRadialGradient(const x ,y ,r : double; c1 ,c2 : TAggColor; profile : double = 1.0 );
var
 i ,startGradient ,endGradient : int;

 k : double;
 c : TAggColor;

 clr : aggclr;
 tat : trans_affine_translation;

begin
 startGradient:=128 - Trunc(profile * 128.0 );
 endGradient  :=128 + Trunc(profile * 128.0 );

 if endGradient <= startGradient then
  endGradient:=startGradient + 1;

 k:=1.0 / (endGradient - startGradient );
 i:=0;

 while i < startGradient do
  begin
   clr.Construct(c1 );

   move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < endGradient do
  begin
   c:=c1.gradient(c2 ,(i - startGradient ) * k );

   clr.Construct(c );

   move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < 256 do
  begin
   clr.Construct(c2 );

   move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 m_lineGradientD2:=worldToScreen(r );

 WorldToScreen(@x ,@y );

 m_lineGradientMatrix.reset;

 tat.Construct(x ,y );

 m_lineGradientMatrix.multiply(@tat );
 m_lineGradientMatrix.invert;

 m_lineGradientD1  :=0;
 m_lineGradientFlag:=AGG_Radial;

 m_lineColor.Construct(0 ,0 ,0 );  // Set some real color

end;

{ FILLRADIALGRADIENT }
procedure TAgg2D.FillRadialGradient(const x ,y ,r : double; c1 ,c2 ,c3 : TAggColor );
var
 i : int;
 c : TAggColor;

 clr : aggclr;
 tat : trans_affine_translation;

begin
 i:=0;

 while i < 128 do
  begin
   c:=c1.gradient(c2 ,i / 127.0 );

   clr.Construct(c );

   move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < 256 do
  begin
   c:=c2.gradient(c3 ,(i - 128 ) / 127.0 );

   clr.Construct(c );

   move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 m_fillGradientD2:=worldToScreen(r );

 WorldToScreen(@x ,@y );

 m_fillGradientMatrix.reset;

 tat.Construct(x ,y );

 m_fillGradientMatrix.multiply(@tat );
 m_fillGradientMatrix.invert;

 m_fillGradientD1  :=0;
 m_fillGradientFlag:=AGG_Radial;

 m_fillColor.Construct(0 ,0 ,0 ); // Set some real color

end;

{ LINERADIALGRADIENT }
procedure TAgg2D.LineRadialGradient(const x ,y ,r : double; c1 ,c2 ,c3 : TAggColor );
var
 i : int;
 c : TAggColor;

 clr : aggclr;
 tat : trans_affine_translation;

begin
 i:=0;

 while i < 128 do
  begin
   c:=c1.gradient(c2 ,i / 127.0 );

   clr.Construct(c );

   move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < 256 do
  begin
   c:=c2.gradient(c3 ,(i - 128 ) / 127.0 );

   clr.Construct(c );

   move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 m_lineGradientD2:=worldToScreen(r );

 WorldToScreen(@x ,@y );

 m_lineGradientMatrix.reset;

 tat.Construct(x ,y );

 m_lineGradientMatrix.multiply(@tat );
 m_lineGradientMatrix.invert;

 m_lineGradientD1  :=0;
 m_lineGradientFlag:=AGG_Radial;

 m_lineColor.Construct(0 ,0 ,0 ); // Set some real color

end;

{ FILLRADIALGRADIENT }
procedure TAgg2D.FillRadialGradient(const x ,y ,r : double );
var
 tat : trans_affine_translation;

begin
 m_fillGradientD2:=worldToScreen(r );

 WorldToScreen(@x ,@y );

 m_fillGradientMatrix.reset;

 tat.Construct(x ,y );

 m_fillGradientMatrix.multiply(@tat );
 m_fillGradientMatrix.invert;

 m_fillGradientD1:=0;

end;

{ LINERADIALGRADIENT }
procedure TAgg2D.LineRadialGradient(const x ,y ,r : double );
var
 tat : trans_affine_translation;

begin
 m_lineGradientD2:=worldToScreen(r );

 WorldToScreen(@x ,@y );

 m_lineGradientMatrix.reset;

 tat.Construct(x ,y );

 m_lineGradientMatrix.multiply(@tat );
 m_lineGradientMatrix.invert;

 m_lineGradientD1:=0;

end;

{ LINEWIDTH }
procedure TAgg2D.LineWidth(const w : double );
begin
 m_lineWidth:=w;

 m_convStroke.width_(w );

end;

{ LINEWIDTH }
function TAgg2D.LineWidth : double;
begin
 result:=m_lineWidth;

end;

{ LINECAP }
procedure TAgg2D.LineCap(cap : TAggLineCap );
begin
 m_lineCap:=cap;

 m_convStroke.line_cap_(cap );

end;

{ LINECAP }
function TAgg2D.LineCap : TAggLineCap;
begin
 result:=m_lineCap;

end;

{ LINEJOIN }
procedure TAgg2D.LineJoin(join : TAggLineJoin );
begin
 m_lineJoin:=join;

 m_convStroke.line_join_(join );

end;

{ LINEJOIN }
function TAgg2D.LineJoin : TAggLineJoin;
begin
 result:=m_lineJoin;

end;

{ FILLEVENODD }
procedure TAgg2D.FillEvenOdd(evenOddFlag : boolean );
begin
 m_evenOddFlag:=evenOddFlag;

 if evenOddFlag then
  m_rasterizer.filling_rule(fill_even_odd )
 else
  m_rasterizer.filling_rule(fill_non_zero );

end;

{ FILLEVENODD }
function TAgg2D.FillEvenOdd : boolean;
begin
 result:=m_evenOddFlag;

end;

{ TRANSFORMATIONS }
function TAgg2D.Transformations : TAggTransformations;
begin
 m_transform.store_to(@result.affineMatrix[0 ] );

end;

{ TRANSFORMATIONS }
procedure TAgg2D.Transformations(tr : PAggTransformations );
begin
 m_transform.load_from(@tr.affineMatrix[0 ] );

 m_convCurve.approximation_scale_ (worldToScreen(1.0 ) * g_approxScale );
 m_convStroke.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );

end;

{ RESETTRANSFORMATIONS }
procedure TAgg2D.ResetTransformations;
begin
 m_transform.reset;

end;

{ AFFINE }
procedure TAgg2D.Affine(const tr : PAggAffine );
begin
 m_transform.multiply(tr );

 m_convCurve.approximation_scale_ (WorldToScreen(1.0 ) * g_approxScale );
 m_convStroke.approximation_scale_(WorldToScreen(1.0 ) * g_approxScale );

end;

{ AFFINE }
procedure TAgg2D.Affine(const tr : PAggTransformations );
var
 ta : trans_affine;

begin
 ta.Construct(
  tr.affineMatrix[0 ] ,tr.affineMatrix[1 ] ,tr.affineMatrix[2 ] ,
  tr.affineMatrix[3 ] ,tr.affineMatrix[4 ] ,tr.affineMatrix[5 ] );

 affine(PAggAffine(@ta ) );

end;

{ ROTATE }
procedure TAgg2D.Rotate(const angle : double );
var
 tar : trans_affine_rotation;

begin
 tar.Construct(angle );

 m_transform.multiply(@tar );

end;

{ SCALE }
procedure TAgg2D.Scale(const sx ,sy : double );
var
 tas : trans_affine_scaling;

begin
 tas.Construct(sx ,sy );

 m_transform.multiply(@tas );

 m_convCurve.approximation_scale_ (worldToScreen(1.0 ) * g_approxScale );
 m_convStroke.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );

end;

{ SKEW }
procedure TAgg2D.Skew(const sx ,sy : double );
var
 tas : trans_affine_skewing;

begin
 tas.Construct(sx ,sy );

 m_transform.multiply(@tas );

end;

{ TRANSLATE }
procedure TAgg2D.Translate(const x ,y : double );
var
 tat : trans_affine_translation;

begin
 tat.Construct(x ,y );

 m_transform.multiply(@tat );

end;

{ PARALLELOGRAM }
procedure TAgg2D.Parallelogram(const x1 ,y1 ,x2 ,y2 : double; para : PDouble );
var
 ta : trans_affine;

begin
 ta.Construct(x1 ,y1 ,x2 ,y2 ,parallelo_ptr(para ) );

 m_transform.multiply(@ta );

 m_convCurve.approximation_scale_ (worldToScreen(1.0 ) * g_approxScale );
 m_convStroke.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );

end;

{ VIEWPORT }
procedure TAgg2D.Viewport(const
           worldX1  ,worldY1  ,worldX2  ,worldY2 ,
           screenX1 ,screenY1 ,screenX2 ,screenY2 : double;
           const opt : TAggViewportOption = AGG_XMidYMid );
var
 vp : trans_viewport;
 mx : trans_affine;

begin
 vp.Construct;

 case opt of
  AGG_Anisotropic :
   vp.preserve_aspect_ratio(0.0 ,0.0 ,aspect_ratio_stretch );

  AGG_XMinYMin :
   vp.preserve_aspect_ratio(0.0 ,0.0 ,aspect_ratio_meet );

  AGG_XMidYMin :
   vp.preserve_aspect_ratio(0.5 ,0.0 ,aspect_ratio_meet );

  AGG_XMaxYMin :
   vp.preserve_aspect_ratio(1.0 ,0.0 ,aspect_ratio_meet );

  AGG_XMinYMid :
   vp.preserve_aspect_ratio(0.0 ,0.5 ,aspect_ratio_meet );

  AGG_XMidYMid :
   vp.preserve_aspect_ratio(0.5 ,0.5 ,aspect_ratio_meet );

  AGG_XMaxYMid :
   vp.preserve_aspect_ratio(1.0 ,0.5 ,aspect_ratio_meet );

  AGG_XMinYMax :
   vp.preserve_aspect_ratio(0.0 ,1.0 ,aspect_ratio_meet );

  AGG_XMidYMax :
   vp.preserve_aspect_ratio(0.5 ,1.0 ,aspect_ratio_meet );

  AGG_XMaxYMax :
   vp.preserve_aspect_ratio(1.0 ,1.0 ,aspect_ratio_meet );

 end;

 vp.world_viewport (worldX1  ,worldY1  ,worldX2  ,worldY2 );
 vp.device_viewport(screenX1 ,screenY1 ,screenX2 ,screenY2 );

 mx.Construct;

 vp.to_affine        (@mx );
 m_transform.multiply(@mx );

 m_convCurve.approximation_scale_ (WorldToScreen(1.0 ) * g_approxScale );
 m_convStroke.approximation_scale_(WorldToScreen(1.0 ) * g_approxScale );

end;

{ LINE }
procedure TAgg2D.Line(const x1, y1, x2, y2: double; AFixAlignment: boolean = false);
var
  lx1, ly1, lx2, ly2: double;
begin
 m_path.remove_all;

 lx1 := x1;
 ly1 := y1;
 lx2 := x2;
 ly2 := y2;

 if AFixAlignment then
 begin
   AlignPoint(@lx1, @ly1);
   AlignPoint(@lx2, @ly2);
 end;

 addLine(lx1, ly1, lx2, ly2);
 DrawPath(AGG_StrokeOnly );
end;

{ TRIANGLE }
procedure TAgg2D.Triangle(const x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double );
begin
 m_path.remove_all;
 m_path.move_to(x1 ,y1 );
 m_path.line_to(x2 ,y2 );
 m_path.line_to(x3 ,y3 );
 m_path.close_polygon;

 DrawPath(AGG_FillAndStroke );

end;

{ RECTANGLE }
procedure TAgg2D.Rectangle(const x1 ,y1 ,x2 ,y2 : double; AFixAlignment: boolean);
var
  lx1, ly1, lx2, ly2: double;
begin
 m_path.remove_all;

 lx1 := x1;
 ly1 := y1;
 lx2 := x2;
 ly2 := y2;

 if AFixAlignment then
 begin
   AlignPoint(@lx1, @ly1);
   AlignPoint(@lx2, @ly2);
 end;

 m_path.move_to(lx1 ,ly1 );
 m_path.line_to(lx2 ,ly1 );
 m_path.line_to(lx2 ,ly2 );
 m_path.line_to(lx1 ,ly2 );
 m_path.close_polygon;

 DrawPath(AGG_FillAndStroke );

end;

{ ROUNDEDRECT }
procedure TAgg2D.RoundedRect(const x1 ,y1 ,x2 ,y2 ,r : double );
var
 rc : rounded_rect;

begin
 m_path.remove_all;
 rc.Construct(x1 ,y1 ,x2 ,y2 ,r );

 rc.normalize_radius;
 rc.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );

 m_path.add_path(@rc ,0 ,false );

 DrawPath(AGG_FillAndStroke );

end;

{ ROUNDEDRECT }
procedure TAgg2D.RoundedRect(const x1 ,y1 ,x2 ,y2 ,rx ,ry : double );
var
 rc : rounded_rect;

begin
 m_path.remove_all;
 rc.Construct;

 rc.rect  (x1 ,y1 ,x2 ,y2 );
 rc.radius(rx ,ry );
 rc.normalize_radius;

 m_path.add_path(@rc ,0 ,false );

 DrawPath(AGG_FillAndStroke );

end;

{ ROUNDEDRECT }
procedure TAgg2D.RoundedRect(const
           x1 ,y1 ,x2 ,y2 ,
           rxBottom ,ryBottom ,
           rxTop ,ryTop : double );
var
 rc : rounded_rect;

begin
 m_path.remove_all;
 rc.Construct;

 rc.rect  (x1 ,y1 ,x2 ,y2 );
 rc.radius(rxBottom ,ryBottom ,rxTop ,ryTop );
 rc.normalize_radius;

 rc.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );

 m_path.add_path(@rc ,0 ,false );

 DrawPath(AGG_FillAndStroke );

end;

{ ELLIPSE }
procedure TAgg2D.Ellipse(const cx ,cy ,rx ,ry : double );
var
 el : bezier_arc;

begin
 m_path.remove_all;

 el.Construct(cx ,cy ,rx ,ry ,0 ,2 * pi );

 m_path.add_path(@el ,0 ,false );
 m_path.close_polygon;

 DrawPath(AGG_FillAndStroke );

end;

{ ARC }
procedure TAgg2D.Arc(const cx ,cy ,rx ,ry ,start ,sweep : double );
var
 ar : {bezier_}agg_arc.arc;

begin
 m_path.remove_all;

 ar.Construct(cx ,cy ,rx ,ry ,sweep ,start ,false );

 m_path.add_path(@ar ,0 ,false );

 DrawPath(AGG_StrokeOnly );

end;

{ STAR }
procedure TAgg2D.Star(const cx ,cy ,r1 ,r2 ,startAngle : double; const numRays : integer );
var
 da ,a ,x ,y : double;

 i : int;

begin
 m_path.remove_all;

 da:=pi / numRays;
 a :=startAngle;

 i:=0;

 while i < numRays do
  begin
   x:=Cos(a ) * r2 + cx;
   y:=Sin(a ) * r2 + cy;

   if i <> 0 then
    m_path.line_to(x ,y )
   else
    m_path.move_to(x ,y );

   a:=a + da;

   m_path.line_to(Cos(a ) * r1 + cx ,Sin(a ) * r1 + cy );

   a:=a + da;

   inc(i );

  end;

 ClosePolygon;
 DrawPath(AGG_FillAndStroke );

end;

{ CURVE }
procedure TAgg2D.Curve(const x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double );
begin
 m_path.remove_all;
 m_path.move_to(x1 ,y1 );
 m_path.curve3 (x2 ,y2 ,x3 ,y3 );

 DrawPath(AGG_StrokeOnly );

end;

{ CURVE }
procedure TAgg2D.Curve(const x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double );
begin
 m_path.remove_all;
 m_path.move_to(x1 ,y1 );
 m_path.curve4 (x2 ,y2 ,x3 ,y3 ,x4 ,y4 );

 DrawPath(AGG_StrokeOnly );

end;

{ POLYGON }
procedure TAgg2D.Polygon(const xy : PDouble; numPoints : integer );
begin
 m_path.remove_all;
 m_path.add_poly(double_2_ptr(xy ) ,numPoints );

 ClosePolygon;
 DrawPath(AGG_FillAndStroke );

end;

{ POLYLINE }
procedure TAgg2D.Polyline(const xy : PDouble; numPoints : integer );
begin
 m_path.remove_all;
 m_path.add_poly(double_2_ptr(xy ) ,numPoints );

 DrawPath(AGG_StrokeOnly );

end;

{ FLIPTEXT }
procedure TAgg2D.FlipText(const flip : boolean );
begin
  {$IFNDEF AGG2D_NO_FONT}
  m_fontEngine.flip_y_(not flip );
  {$ENDIF}
end;

{ FONT }
procedure TAgg2D.Font(
           fileName : AnsiString; height : double;
           bold : boolean = false;
           italic : boolean = false;
           cache : TAggFontCacheType = AGG_VectorFontCache;
           angle : double = 0.0 );
var
 b : int;

begin
 m_textAngle    :=angle;
 m_fontHeight   :=height;
 m_fontCacheType:=cache;

{$IFDEF AGG2D_USE_FREETYPE }
 if cache = AGG_VectorFontCache then
  m_fontEngine.load_font(PChar(fileName) ,0 ,glyph_ren_outline )
 else
  m_fontEngine.load_font(PChar(fileName) ,0 ,glyph_ren_agg_gray8 );

 m_fontEngine.hinting_(m_textHints );

 if cache = AGG_VectorFontCache then
  m_fontEngine.height_(height )
 else
  m_fontEngine.height_(worldToScreen(height ) );
{$ENDIF}
{$IFDEF AGG2D_USE_WINFONTS}
 m_fontEngine.hinting_(m_textHints );

 if bold then
  b:=700
 else
  b:=400;

 if cache = AGG_VectorFontCache then
  m_fontEngine.create_font_(PChar(@fileName[1 ] ) ,glyph_ren_outline ,height ,0.0 ,b ,italic )
 else
  m_fontEngine.create_font_(PChar(@fileName[1 ] ) ,glyph_ren_agg_gray8 ,worldToScreen(height) ,0.0 ,b ,italic );
{$ENDIF }
end;

{ FONTHEIGHT }
function TAgg2D.FontHeight : double;
begin
 result:=m_fontHeight;

end;

{ TEXTALIGNMENT }
procedure TAgg2D.TextAlignment(alignX ,alignY : TAggTextAlignment );
begin
 m_textAlignX:=alignX;
 m_textAlignY:=alignY;

end;

{ TEXTHINTS }
function TAgg2D.TextHints : boolean;
begin
 result:=m_textHints;

end;

{ TEXTHINTS }
procedure TAgg2D.TextHints(hints : boolean );
begin
 m_textHints:=hints;
 {$IFNDEF AGG2D_NO_FONT}
 m_fontEngine.hinting_(m_textHints );
 {$ENDIF}
end;

{ TEXTWIDTH }
function TAgg2D.TextWidth(str : AnsiString ) : double;
{$IFDEF AGG2D_NO_FONT}
begin
  Result := 0;
end;
{$ELSE}
var
 x ,y  : double;
 first : boolean;
 glyph : glyph_cache_ptr;
 str_  : PChar;

begin
 if str = '' then exit(0);
 x:=0;
 y:=0;

 first:=true;
 str_ := PChar(str);

 while str_^ <> #0 do
  begin
   glyph:=m_fontCacheManager.glyph(int32u(str_^ ) );

   if glyph <> NIL then
    begin
     if not first then
      m_fontCacheManager.add_kerning(@x ,@y );

     x:=x + glyph.advance_x;
     y:=y + glyph.advance_y;

     first:=false;

    end;

   inc(ptrcomp(str_ ) );

  end;

 if m_fontCacheType = AGG_VectorFontCache then
  result:=x
 else
  result:=ScreenToWorld(x );
end;
{$ENDIF}

{ TEXT }
procedure TAgg2D.Text(
           x ,y : double; str : AnsiString;
           roundOff : boolean = false;
           ddx : double = 0.0;
           ddy : double = 0.0 );
{$IFDEF AGG2D_NO_FONT}
begin

end;
{$ELSE}
var
 dx ,dy ,asc ,start_x ,start_y : double;

 glyph : glyph_cache_ptr;

 mtx  : trans_affine;
 str_ : PChar;

 tat : trans_affine_translation;
 tar : trans_affine_rotation;

  tr : conv_transform;
  charlen: int;
  char_id: int32u;
  First: Boolean;

begin
 if Str='' then exit;

 dx:=0.0;
 dy:=0.0;

 case m_textAlignX of
  AGG_AlignCenter :
   dx:=-textWidth(str ) * 0.5;

  AGG_AlignRight :
   dx:=-textWidth(str );

 end;

 asc  :=fontHeight;
 glyph:=m_fontCacheManager.glyph(int32u('H' ) );

 if glyph <> NIL then
  asc:=glyph.bounds.y2 - glyph.bounds.y1;

 if m_fontCacheType = AGG_RasterFontCache then
  asc:=screenToWorld(asc );

 case m_textAlignY of
  AGG_AlignCenter :
   dy:=-asc * 0.5;

  AGG_AlignTop :
   dy:=-asc;

 end;

 if m_fontEngine._flip_y then
  dy:=-dy;

 mtx.Construct;

 start_x:=x + dx;
 start_y:=y + dy;

 if roundOff then
  begin
   start_x:=Trunc(start_x );
   start_y:=Trunc(start_y );

  end;

 start_x:=start_x + ddx;
 start_y:=start_y + ddy;

 tat.Construct(-x ,-y );
 mtx.multiply (@tat );

 tar.Construct(m_textAngle );
 mtx.multiply (@tar );

 tat.Construct(x ,y );
 mtx.multiply (@tat );

 tr.Construct(m_fontCacheManager.path_adaptor ,@mtx );

 if m_fontCacheType = AGG_RasterFontCache then
  WorldToScreen(@start_x ,@start_y );

  str_:=@str[1 ];
  First:=true;

  while str_^ <> #0 do
  begin
    char_id := UTF8CharToUnicode(str_, charlen);
    inc(str_, charlen);
    glyph := m_fontCacheManager.glyph(char_id);

    if glyph <> NIL then
    begin
      if First then
      begin
        m_fontCacheManager.add_kerning(@x ,@y );
        First:=false;
      end;

      m_fontCacheManager.init_embedded_adaptors(glyph ,start_x ,start_y );

      if glyph.data_type = glyph_data_outline then
      begin
       m_path.remove_all;
       m_path.add_path(@tr ,0 ,false );

       drawPath;

      end;

     if glyph.data_type = glyph_data_gray8 then
      begin
        Render(
          m_fontCacheManager.gray8_adaptor ,
          m_fontCacheManager.gray8_scanline );
      end;

     start_x := start_x + glyph.advance_x;
     start_y := start_y + glyph.advance_y;

    end;
  end;
end;
{$ENDIF}

{ RESETPATH }
procedure TAgg2D.ResetPath;
begin
 m_path.remove_all;
 m_path.move_to(0 ,0 );

end;

{ MOVETO }
procedure TAgg2D.MoveTo(const x ,y : double );
begin
 m_path.move_to(x ,y );

end;

{ MOVEREL }
procedure TAgg2D.MoveRel(const dx ,dy : double );
begin
 m_path.move_rel(dx ,dy );

end;

{ LINETO }
procedure TAgg2D.LineTo(const x ,y : double );
begin
 m_path.line_to(x ,y );

end;

{ LINEREL }
procedure TAgg2D.LineRel(const dx ,dy : double );
begin
 m_path.line_rel(dx ,dy );

end;

{ HORLINETO }
procedure TAgg2D.HorLineTo(const x : double );
begin
 m_path.hline_to(x );

end;

{ HORLINEREL }
procedure TAgg2D.HorLineRel(const dx : double );
begin
 m_path.hline_rel(dx );

end;

{ VERLINETO }
procedure TAgg2D.VerLineTo(const y : double );
begin
 m_path.vline_to(y );

end;

{ VERLINEREL }
procedure TAgg2D.VerLineRel(const dy : double );
begin
 m_path.vline_rel(dy );

end;

{ ARCTO }
procedure TAgg2D.ArcTo(
           rx ,ry ,angle : double;
           largeArcFlag ,sweepFlag : boolean;
           x ,y : double );
begin
 m_path.arc_to(rx ,ry ,angle ,largeArcFlag ,sweepFlag ,x ,y );

end;

{ ARCREL }
procedure TAgg2D.ArcRel(
           rx ,ry ,angle : double;
           largeArcFlag ,sweepFlag : boolean;
           dx ,dy : double );
begin
 m_path.arc_rel(rx ,ry ,angle ,largeArcFlag ,sweepFlag ,dx ,dy );

end;

{ QUADRICCURVETO }
procedure TAgg2D.QuadricCurveTo (xCtrl ,yCtrl ,xTo ,yTo : double );
begin
 m_path.curve3(xCtrl ,yCtrl ,xTo ,yTo );

end;

{ QUADRICCURVEREL }
procedure TAgg2D.QuadricCurveRel(dxCtrl ,dyCtrl ,dxTo ,dyTo : double );
begin
 m_path.curve3_rel(dxCtrl ,dyCtrl ,dxTo ,dyTo );

end;

{ QUADRICCURVETO }
procedure TAgg2D.QuadricCurveTo (xTo ,yTo : double );
begin
 m_path.curve3(xTo ,yTo );

end;

{ QUADRICCURVEREL }
procedure TAgg2D.QuadricCurveRel(dxTo ,dyTo : double );
begin
 m_path.curve3_rel(dxTo ,dyTo );

end;

{ CUBICCURVETO }
procedure TAgg2D.CubicCurveTo (xCtrl1 ,yCtrl1 ,xCtrl2 ,yCtrl2 ,xTo ,yTo : double );
begin
 m_path.curve4(xCtrl1 ,yCtrl1 ,xCtrl2 ,yCtrl2 ,xTo ,yTo );

end;

{ CUBICCURVEREL }
procedure TAgg2D.CubicCurveRel(dxCtrl1 ,dyCtrl1 ,dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo : double );
begin
 m_path.curve4_rel(dxCtrl1 ,dyCtrl1 ,dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo );

end;

{ CUBICCURVETO }
procedure TAgg2D.CubicCurveTo (xCtrl2 ,yCtrl2 ,xTo ,yTo : double );
begin
 m_path.curve4(xCtrl2 ,yCtrl2 ,xTo ,yTo );

end;

{ CUBICCURVEREL }
procedure TAgg2D.CubicCurveRel(dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo : double );
begin
 m_path.curve4_rel(dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo );

end;

{ ADDELLIPSE }
procedure TAgg2D.AddEllipse(cx ,cy ,rx ,ry : double; dir : TAggDirection );
var
 ar : bezier_arc;

begin
 if dir = AGG_CCW then
  ar.Construct(cx ,cy ,rx ,ry ,0 ,2 * pi )
 else
  ar.Construct(cx ,cy ,rx ,ry ,0 ,-2 * pi );

 m_path.add_path(@ar ,0 ,false );
 m_path.close_polygon;

end;

{ CLOSEPOLYGON }
procedure TAgg2D.ClosePolygon;
begin
 m_path.close_polygon;

end;

{ DRAWPATH }
procedure TAgg2D.DrawPath(flag : TAggDrawPathFlag = AGG_FillAndStroke );
begin
 m_rasterizer.reset;

 case flag of
  AGG_FillOnly :
   if m_fillColor.a <> 0 then
    begin
     m_rasterizer.add_path(@m_pathTransform );

     render(true );

    end;

  AGG_StrokeOnly :
   if (m_lineColor.a <> 0 ) and
      (m_lineWidth > 0.0 ) then
    begin
     m_rasterizer.add_path(@m_strokeTransform );

     render(false );

    end;

  AGG_FillAndStroke :
   begin
    if m_fillColor.a <> 0 then
     begin
      m_rasterizer.add_path(@m_pathTransform );

      render(true );

     end;

    if (m_lineColor.a <> 0 ) and
       (m_lineWidth > 0.0 ) then
     begin
      m_rasterizer.add_path(@m_strokeTransform );

      render(false );

     end;

   end;

  AGG_FillWithLineColor :
   if m_lineColor.a <> 0 then
    begin
     m_rasterizer.add_path(@m_pathTransform );

     render(false );

    end;

 end;

end;

{ IMAGEFILTER }
procedure TAgg2D.ImageFilter(f : TAggImageFilter );
begin
 m_imageFilter:=f;

 case f of
  AGG_Bilinear :
   m_imageFilterLut.calculate(@m_ifBilinear ,true );

  AGG_Hanning :
   m_imageFilterLut.calculate(@m_ifHanning ,true );

  AGG_Hermite :
   m_imageFilterLut.calculate(@m_ifHermite ,true );

  AGG_Quadric :
   m_imageFilterLut.calculate(@m_ifQuadric ,true );

  AGG_Bicubic :
   m_imageFilterLut.calculate(@m_ifBicubic ,true );

  AGG_Catrom :
   m_imageFilterLut.calculate(@m_ifCatrom ,true );

  AGG_Spline16 :
   m_imageFilterLut.calculate(@m_ifSpline16 ,true );

  AGG_Spline36 :
   m_imageFilterLut.calculate(@m_ifSpline36 ,true );

  AGG_Blackman144 :
   m_imageFilterLut.calculate(@m_ifBlackman144 ,true );

 end;

end;

{ IMAGEFILTER }
function TAgg2D.ImageFilter : TAggImageFilter;
begin
 result:=m_imageFilter;

end;

{ IMAGERESAMPLE }
procedure TAgg2D.ImageResample(f : TAggImageResample );
begin
 m_imageResample:=f;

end;

{ IMAGEFLIP }
procedure TAgg2D.ImageFlip(f : boolean );
begin
 m_imageFlip:=f;

end;

{ IMAGERESAMPLE }
function TAgg2D.ImageResample : TAggImageResample;
begin
 result:=m_imageResample;

end;

{ TRANSFORMIMAGE }
procedure TAgg2D.TransformImage(
           bitmap : TfpgImage;
           imgX1 ,imgY1 ,imgX2 ,imgY2 : integer;
           dstX1 ,dstY1 ,dstX2 ,dstY2 : double );
var
 parall : array[0..5 ] of double;
 image  : TAggImage;

begin
 image.Construct;

 if image.attach(bitmap ,m_imageFlip ) then
  begin
   resetPath;
   moveTo(dstX1 ,dstY1 );
   lineTo(dstX2 ,dstY1 );
   lineTo(dstX2 ,dstY2 );
   lineTo(dstX1 ,dstY2 );
   closePolygon;

   parall[0 ]:=dstX1;
   parall[1 ]:=dstY1;
   parall[2 ]:=dstX2;
   parall[3 ]:=dstY1;
   parall[4 ]:=dstX2;
   parall[5 ]:=dstY2;

   renderImage(@image ,imgX1 ,imgY1 ,imgX2 ,imgY2 ,@parall[0 ] );

   image.Destruct;

  end;

end;

{ TRANSFORMIMAGE }
procedure TAgg2D.TransformImage(
           bitmap : TfpgImage;
           dstX1 ,dstY1 ,dstX2 ,dstY2 : double );
var
 parall : array[0..5 ] of double;
 image  : TAggImage;

begin
 image.Construct;

 if image.attach(bitmap ,m_imageFlip ) then
  begin
   ResetPath;
   MoveTo(dstX1 ,dstY1 );
   LineTo(dstX2 ,dstY1 );
   LineTo(dstX2 ,dstY2 );
   LineTo(dstX1 ,dstY2 );
   ClosePolygon;

   parall[0 ]:=dstX1;
   parall[1 ]:=dstY1;
   parall[2 ]:=dstX2;
   parall[3 ]:=dstY1;
   parall[4 ]:=dstX2;
   parall[5 ]:=dstY2;

   renderImage(@image ,0 ,0 ,image.renBuf._width ,image.renBuf._height ,@parall[0 ] );

   image.Destruct;

  end;

end;

{ TRANSFORMIMAGE }
procedure TAgg2D.TransformImage(
           bitmap : TfpgImage;
           imgX1 ,imgY1 ,imgX2 ,imgY2 : integer;
           parallelo : PDouble );
var
 image : TAggImage;

begin
 image.Construct;

 if image.attach(bitmap ,m_imageFlip ) then
  begin
   ResetPath;

   MoveTo(
    PDouble(ptrcomp(parallelo ) + 0 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 1 * sizeof(double ) )^ );

   LineTo(
    PDouble(ptrcomp(parallelo ) + 2 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 3 * sizeof(double ) )^ );

   LineTo(
    PDouble(ptrcomp(parallelo ) + 4 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 5 * sizeof(double ) )^ );

   LineTo(
    PDouble(ptrcomp(parallelo ) + 0 * sizeof(double ) )^ +
    PDouble(ptrcomp(parallelo ) + 4 * sizeof(double ) )^ -
    PDouble(ptrcomp(parallelo ) + 2 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 1 * sizeof(double ) )^ +
    PDouble(ptrcomp(parallelo ) + 5 * sizeof(double ) )^ -
    PDouble(ptrcomp(parallelo ) + 3 * sizeof(double ) )^ );

   ClosePolygon;

   renderImage(@image ,imgX1 ,imgY1 ,imgX2 ,imgY2 ,parallelo );

   image.Destruct;

  end;

end;

{ TRANSFORMIMAGE }
procedure TAgg2D.TransformImage(bitmap : TfpgImage; parallelo : PDouble );
var
 image : TAggImage;

begin
 image.Construct;

 if image.attach(bitmap ,m_imageFlip ) then
  begin
   ResetPath;

   MoveTo(
    PDouble(ptrcomp(parallelo ) + 0 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 1 * sizeof(double ) )^ );

   LineTo(
    PDouble(ptrcomp(parallelo ) + 2 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 3 * sizeof(double ) )^ );

   LineTo(
    PDouble(ptrcomp(parallelo ) + 4 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 5 * sizeof(double ) )^ );

   LineTo(
    PDouble(ptrcomp(parallelo ) + 0 * sizeof(double ) )^ +
    PDouble(ptrcomp(parallelo ) + 4 * sizeof(double ) )^ -
    PDouble(ptrcomp(parallelo ) + 2 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 1 * sizeof(double ) )^ +
    PDouble(ptrcomp(parallelo ) + 5 * sizeof(double ) )^ -
    PDouble(ptrcomp(parallelo ) + 3 * sizeof(double ) )^ );

   ClosePolygon;

   renderImage(@image ,0 ,0 ,image.renBuf._width ,image.renBuf._height ,parallelo );

   image.Destruct;

  end;

end;

{ TRANSFORMIMAGEPATH }
procedure TAgg2D.TransformImagePath(
           bitmap : TfpgImage;
           imgX1 ,imgY1 ,imgX2 ,imgY2 : integer;
           dstX1 ,dstY1 ,dstX2 ,dstY2 : double );
var
 parall : array[0..5 ] of double;
 image  : TAggImage;

begin
 image.Construct;

 if image.attach(bitmap ,m_imageFlip ) then
  begin
   parall[0 ]:=dstX1;
   parall[1 ]:=dstY1;
   parall[2 ]:=dstX2;
   parall[3 ]:=dstY1;
   parall[4 ]:=dstX2;
   parall[5 ]:=dstY2;

   renderImage(@image ,imgX1 ,imgY1 ,imgX2 ,imgY2 ,@parall[0 ] );

   image.Destruct;

  end;

end;

{ TRANSFORMIMAGEPATH }
procedure TAgg2D.TransformImagePath(
           bitmap : TfpgImage;
           dstX1 ,dstY1 ,dstX2 ,dstY2 : double );
var
 parall : array[0..5 ] of double;
 image  : TAggImage;

begin
 image.Construct;

 if image.attach(bitmap ,m_imageFlip ) then
  begin
   parall[0 ]:=dstX1;
   parall[1 ]:=dstY1;
   parall[2 ]:=dstX2;
   parall[3 ]:=dstY1;
   parall[4 ]:=dstX2;
   parall[5 ]:=dstY2;

   renderImage(@image ,0 ,0 ,image.renBuf._width ,image.renBuf._height ,@parall[0 ] );

   image.Destruct;

  end;

end;

{ TRANSFORMIMAGEPATH }
procedure TAgg2D.TransformImagePath(
           bitmap : TfpgImage;
           imgX1 ,imgY1 ,imgX2 ,imgY2 : integer;
           parallelo : PDouble );
var
 image : TAggImage;

begin
 image.Construct;

 if image.attach(bitmap ,m_imageFlip ) then
  begin
   renderImage(@image ,imgX1 ,imgY1 ,imgX2 ,imgY2 ,parallelo );

   image.Destruct;

  end;

end;

{ TRANSFORMIMAGEPATH }
procedure TAgg2D.TransformImagePath(bitmap : TfpgImage; parallelo : PDouble );
var
 image : TAggImage;

begin
 image.Construct;

 if image.attach(bitmap ,m_imageFlip ) then
  begin
   renderImage(@image ,0 ,0 ,image.renBuf._width ,image.renBuf._height ,parallelo );

   image.Destruct;

  end;

end;

{ COPYIMAGE }
procedure TAgg2D.CopyImage(
           bitmap : TfpgImage;
           imgX1 ,imgY1 ,imgX2 ,imgY2 : integer;
           dstX ,dstY : double );
var
 r     : agg_basics.rect;
 image : TAggImage;

begin
 image.Construct;

 if image.attach(bitmap ,m_imageFlip ) then
  begin
   WorldToScreen(@dstX ,@dstY );
   r.Construct  (imgX1 ,imgY1 ,imgX2 ,imgY2 );

   m_renBase.copy_from(@image.renBuf ,@r ,Trunc(dstX ) - imgX1 ,Trunc(dstY ) - imgY1 );

   image.Destruct;

  end;

end;

{ COPYIMAGE }
procedure TAgg2D.CopyImage(bitmap : TfpgImage; dstX ,dstY : double );
var
 image : TAggImage;

begin
 image.Construct;

 if image.attach(bitmap ,m_imageFlip ) then
  begin
   WorldToScreen(@dstX ,@dstY );

   m_renBase.copy_from(@image.renBuf ,NIL ,Trunc(dstX ) ,Trunc(dstY ) );

   image.Destruct;

  end;

end;

{ RENDER }
procedure TAgg2D.render(fillColor_ : boolean );
begin
 if (m_blendMode = AGG_BlendAlpha ) or
    (m_pixf = pf24bit ) then
  Agg2DRenderer_render(self ,@m_renBase ,@m_renSolid ,fillColor_ )
 else
  Agg2DRenderer_render(self ,@m_renBaseComp ,@m_renSolidComp ,fillColor_ );

end;

{ RENDER }
procedure TAgg2D.render(ras : PAggFontRasterizer; sl : PAggFontScanline );
begin
 if (m_blendMode = AGG_BlendAlpha ) or
    (m_pixf = pf24bit ) then
  Agg2DRenderer_render(self ,@m_renBase ,@m_renSolid ,ras ,sl )
 else
  Agg2DRenderer_render(self ,@m_renBaseComp ,@m_renSolidComp ,ras ,sl );

end;

{ ADDLINE }
procedure TAgg2D.addLine(x1 ,y1 ,x2 ,y2 : double );
begin
  m_path.move_to(x1 ,y1 );
  m_path.line_to(x2 ,y2 );
end;

{ UPDATERASTERIZERGAMMA }
procedure TAgg2D.updateRasterizerGamma;
begin
 m_gammaAgg2D.Construct(m_masterAlpha ,m_antiAliasGamma );
 m_rasterizer.gamma    (@m_gammaAgg2D );

end;

{ RENDERIMAGE }
procedure TAgg2D.renderImage(
           img : PAggImage;
           x1 ,y1 ,x2 ,y2 : integer;
           parl : PDouble );
var
 mtx : trans_affine;

 interpolator : span_interpolator_linear;

begin
 mtx.Construct(x1 ,y1 ,x2 ,y2 ,parallelo_ptr(parl ) );
 mtx.multiply (@m_transform );
 mtx.invert;

 m_rasterizer.reset;
 m_rasterizer.add_path(@m_pathTransform );

 interpolator.Construct(@mtx );

 if (m_blendMode = AGG_BlendAlpha ) or
    (m_pixf = pf24bit ) then
  Agg2DRenderer_renderImage(self ,img ,@m_renBasePre ,@interpolator )
 else
  Agg2DRenderer_renderImage(self ,img ,@m_renBaseCompPre ,@interpolator );

end;

procedure TAgg2D.DoSetFontRes(fntres: TfpgFontResourceBase);
begin
  {$NOTE This is only temporary until I can correctly query font names }
  {$IFDEF WINDOWS}
  Font('Arial', 13);
  {$ELSE}
    {$IFDEF BSD}
    Font('/usr/local/lib/X11/fonts/Liberation/LiberationSans-Regular.ttf', 13);
    {$ELSE}
    Font('/usr/share/fonts/truetype/ttf-liberation/LiberationSans-Regular.ttf', 13);
    {$ENDIF}
  {$ENDIF}
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
  LineColor(LineColor);
//  NoLine;
  LineWidth(1);
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
  { We use TransformImage so we can get alpha blending support.
    CopyImage doesn't use image blending, when it does painting. }
  TransformImage(TfpgImage(img), xi, yi, xi+w, yi+h, x, y, x+w, y+h);
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
  ClipBox(ARect.Left, ARect.Top, ARect.Right+1, ARect.Bottom+1);
end;

function TAgg2D.DoGetClipRect: TfpgRect;
begin
  Result.SetRect(Round(ClipBox.x1), Round(ClipBox.y1), Round(ClipBox.x2 - ClipBox.x1), Round(ClipBox.y2 - ClipBox.y1));
end;

procedure TAgg2D.DoAddClipRect(const ARect: TfpgRect);
begin
  {$NOTE TAgg2D.DoAddClipRect must still be implemented }
end;

procedure TAgg2D.DoClearClipRect;
begin
  ClipBox(0, 0, FWindow.width, FWindow.height);
  m_rasterizer.m_clipping := false;
end;

procedure TAgg2D.DoBeginDraw(awin: TfpgWindowBase; buffered: boolean);
begin
  if Assigned(FImg) then
  begin
    { if the window was resized }
    if (FImg.Width <> FWindow.Width) or (FImg.Height <> FWindow.Height) then
    begin
      FImg.Free;
      FImg := nil;
    end;
  end;

  if not Assigned(FImg) then
  begin
    FImg := TfpgImage.Create;
    FImg.AllocateImage(32, FWindow.Width, FWindow.Height);
    Attach(FImg);
  end;
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
  NoFill;
  LineColor(LineColor);
  Arc(x+(w/2), y+(h/2), w/2, h/2, Deg2Rad(a1+90), Deg2Rad(a2+90));
end;

procedure TAgg2D.DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended);
begin
  {$Note AggPas's Arc only does stroking, not filling. Make another plan }
  NoFill;
  LineColor(LineColor);
  Arc(x+(w/2), y+(h/2), w/2, h/2, Deg2Rad(a1+90), Deg2Rad(a2+90));
end;

procedure TAgg2D.DoDrawPolygon(Points: PPoint; NumPts: Integer; Winding: boolean);
var
  i: integer;
  poly: array of double;
begin
  SetLength(poly, (NumPts*2)+1); // dynamic arrays start at 0, but we want to use 1..NumPts
  for i := 1 to NumPts do
  begin
    poly[i * 2 - 1] := Points[i-1].X + 0.5;
    poly[i * 2] := Points[i-1].Y + 0.5;
  end;
  // Draw Polygon
  LineWidth(1);
  LineColor(LineColor);
  FillColor($00, $00, $00);  // clBlack for now
  Polygon(@poly[1], NumPts);
end;


{ BITMAPALPHATRANSPARENCY }
function BitmapAlphaTransparency(bitmap : TfpgImage; alpha : byte ) : boolean;
var
 fcx ,fcy : integer;
 transp   : ^byte;

begin
 result:=false;

 if Assigned(bitmap )
  { and
    not bitmap.Empty} and      {$Warning Implement bitmap.Empty }
    (bitmap.ColorDepth = 32 ) then
 begin
   for fcy:=0 to bitmap.Height - 1 do
    begin
     transp:=pointer(ptrcomp(bitmap.ScanLine[fcy ] ) + 3 );

     for fcx:=0 to bitmap.Width - 1 do
      begin
       transp^:=alpha;

       inc(ptrcomp(transp ) ,4 );

      end;

    end;

  { OK }
   result:=true;

 end;

end;

end.

 
