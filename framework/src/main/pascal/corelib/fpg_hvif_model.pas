{
    This unit is part of the fpGUI Toolkit project.

    Description:
      HVIF (Haiku Vector Icon Format) data model — shared types used by both
      the renderer (fpg_hvif) and the serialiser (fpg_hvif_writer).

      This unit has no dependencies on the fpGUI framework or AggPas, so it
      can be compiled as part of build tools (e.g. svg2hvif) without requiring
      the full framework to be present.
}

unit fpg_hvif_model;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;


{ ==================== Exception types ==================== }

type
  EHvifError       = class(Exception);
  EHvifFormatError = class(EHvifError);
  EHvifParseError  = class(EHvifFormatError);


{ ==================== Public constants ==================== }

const
  { HVIF magic bytes as they appear in the file stream (LE uint32 = 'ficn') }
  HVIF_MAGIC: array[0..3] of Byte = ($6E, $63, $69, $66);


{ ==================== Data model ==================== }

type
  { Gradient types (from Haiku GradientTransformable.h) }
  THvifGradientType = (
    hgtLinear   = 0,
    hgtCircular = 1,
    hgtDiamond  = 2,
    hgtConic    = 3,
    hgtXY       = 4,
    hgtSqrtXY   = 5
  );

  { Style (color/fill) types -- from Haiku FlatIconFormat.h }
  THvifStyleType = (
    hstSolidColor        = 1,  { 4 bytes: R G B A }
    hstGradient          = 2,
    hstSolidColorNoAlpha = 3,  { 3 bytes: R G B    (alpha=255) }
    hstSolidGray         = 4,  { 2 bytes: K A }
    hstSolidGrayNoAlpha  = 5   { 1 byte:  K         (alpha=255) }
  );

  THvifColor = record
    R, G, B, A: Byte;
  end;

  THvifGradientStop = record
    Offset: Single;   { normalised 0.0..1.0 }
    Color: THvifColor;
  end;

  THvifStyle = record
    StyleType:    THvifStyleType;
    { For solid styles: }
    Color: THvifColor;
    { For gradient styles: }
    GradientType: THvifGradientType;
    { Gradient transform: maps gradient parameter space -> icon (64-unit) space.
      Stored as [sx, shy, shx, sy, tx, ty] in agg trans_affine order. }
    GradTransform: array[0..5] of Single;
    HasGradTransform: Boolean;
    Stops: array of THvifGradientStop;
  end;

  { Unified path point: stores main point + incoming/outgoing control points.
    For line points: InX=X, InY=Y, OutX=X, OutY=Y. }
  THvifPoint = record
    X, Y: Single;
    InX, InY: Single;
    OutX, OutY: Single;
  end;

  THvifPath = record
    Closed: Boolean;
    Points: array of THvifPoint;
  end;

  THvifShape = record
    StyleIndex:  Byte;
    PathIndices: array of Byte;
    { Affine shape transform (6 float24 values, agg order) }
    Transform:    array[0..5] of Single;
    HasTransform: Boolean;
    { Translation-only (2 coords, applied when HasTransform=False) }
    TranslateX, TranslateY: Single;
    HasTranslation: Boolean;
    { Stroke transformer (TRANSFORMER_TYPE_STROKE = 23).
      Width and MiterLimit are in HVIF 64-unit icon space.
      LineCap:  0=butt, 1=square, 2=round  (AggPas agg_math_stroke constants)
      LineJoin: 0=miter, 2=round, 3=bevel  (AggPas agg_math_stroke constants) }
    HasStroke:        Boolean;
    StrokeWidth:      Single;
    StrokeLineCap:    Byte;
    StrokeLineJoin:   Byte;
    StrokeMiterLimit: Single;
    { LOD: min/max visibility scale (0.0..4.0, where scale = rendered_px / 64).
      HasLODScale=False means always visible (defaults: min=0, max=4). }
    HasLODScale:  Boolean;
    MinVisScale:  Single;
    MaxVisScale:  Single;
  end;


implementation

end.
