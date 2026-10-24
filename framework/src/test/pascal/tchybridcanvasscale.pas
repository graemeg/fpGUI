unit tchybridcanvasscale;

{$mode objfpc}{$H+}

{ Tests for THybridCanvas rendering at a backing scale factor (Retina).

  Drawing coordinates stay in logical units (points); the canvas renders
  into a pixel buffer that is AScale times larger in each direction. The
  canvas is attached to a caller-owned buffer via AttachBuffer, so no
  native window is needed. }

interface

uses
  Classes, SysUtils,
  fpcunit, testregistry,
  fpg_base, fpg_hybrid_canvas;

const
  cBufW = 20;  { device pixels; 10x10 logical units at scale 2 }
  cBufH = 20;
  cRed = TfpgColor($FF0000);

type
  TTestHybridCanvasScale = class(TTestCase)
  private
    FBuf: array[0..cBufW * cBufH - 1] of LongWord;
    FCanvas: THybridCanvas;
    function Px(X, Y: Integer): LongWord;
    procedure Attach(AScale: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestScale1FillRectangleUnchanged;
    procedure TestFillRectangleCoversScaledArea;
    procedure TestClipRectIsScaled;
    procedure TestXORFillRectangleCoversScaledArea;
    procedure TestSetPixelFillsScaledBlock;
    procedure TestGetPixelReadsScaledLocation;
    procedure TestBlitImagePartScalesEachPixel;
    procedure TestHorizontalLineIsCrisp;
    procedure TestGetClipRectReturnsLogicalUnits;
  end;

  TTestScaledFontResource = class(TTestCase)
  published
    procedure TestScale1ReturnsSelf;
    procedure TestScaledResourceHasScaledMetrics;
    procedure TestScaledResourceIsCached;
  end;


implementation

uses
  fpg_main;

{ TTestHybridCanvasScale }

function TTestHybridCanvasScale.Px(X, Y: Integer): LongWord;
begin
  { Buffer is BGRA; on little-endian the LongWord is $AARRGGBB }
  Result := FBuf[Y * cBufW + X] and $00FFFFFF;
end;

procedure TTestHybridCanvasScale.Attach(AScale: Integer);
begin
  FCanvas.AttachBuffer(@FBuf[0], cBufW, cBufH, cBufW * 4, AScale);
end;

procedure TTestHybridCanvasScale.SetUp;
begin
  if not fpgApplication.IsInitialized then
    fpgApplication.Initialize;
  FillChar(FBuf, SizeOf(FBuf), 0);
  FCanvas := THybridCanvas.Create(nil);
end;

procedure TTestHybridCanvasScale.TearDown;
begin
  FCanvas.Free;
end;

procedure TTestHybridCanvasScale.TestScale1FillRectangleUnchanged;
begin
  Attach(1);
  FCanvas.SetColor(cRed);
  FCanvas.FillRectangle(1, 1, 2, 2);
  AssertEquals('(0,0) untouched', 0, Px(0, 0));
  AssertEquals('(1,1) filled', cRed, Px(1, 1));
  AssertEquals('(2,2) filled', cRed, Px(2, 2));
  AssertEquals('(3,3) untouched', 0, Px(3, 3));
end;

procedure TTestHybridCanvasScale.TestFillRectangleCoversScaledArea;
begin
  Attach(2);
  FCanvas.SetColor(cRed);
  FCanvas.FillRectangle(1, 1, 2, 2);
  AssertEquals('(1,1) untouched', 0, Px(1, 1));
  AssertEquals('(2,2) filled', cRed, Px(2, 2));
  AssertEquals('(5,5) filled', cRed, Px(5, 5));
  AssertEquals('(6,6) untouched', 0, Px(6, 6));
  AssertEquals('(5,6) untouched', 0, Px(5, 6));
end;

procedure TTestHybridCanvasScale.TestClipRectIsScaled;
begin
  Attach(2);
  FCanvas.SetClipRect(fpgRect(0, 0, 2, 2));
  FCanvas.SetColor(cRed);
  FCanvas.FillRectangle(0, 0, 10, 10);
  AssertEquals('(3,3) inside clip', cRed, Px(3, 3));
  AssertEquals('(4,4) outside clip', 0, Px(4, 4));
  AssertEquals('(4,0) outside clip', 0, Px(4, 0));
end;

procedure TTestHybridCanvasScale.TestXORFillRectangleCoversScaledArea;
begin
  Attach(2);
  FCanvas.XORFillRectangle(TfpgColor($FFFFFF), 1, 1, 1, 1);
  AssertEquals('(1,1) untouched', 0, Px(1, 1));
  AssertEquals('(2,2) xored', $FFFFFF, Px(2, 2));
  AssertEquals('(3,3) xored', $FFFFFF, Px(3, 3));
  AssertEquals('(4,4) untouched', 0, Px(4, 4));
end;

procedure TTestHybridCanvasScale.TestSetPixelFillsScaledBlock;
begin
  Attach(2);
  FCanvas.Pixels[3, 4] := cRed;
  AssertEquals('(6,8)', cRed, Px(6, 8));
  AssertEquals('(7,8)', cRed, Px(7, 8));
  AssertEquals('(6,9)', cRed, Px(6, 9));
  AssertEquals('(7,9)', cRed, Px(7, 9));
  AssertEquals('(8,8) untouched', 0, Px(8, 8));
  AssertEquals('(5,8) untouched', 0, Px(5, 8));
end;

procedure TTestHybridCanvasScale.TestGetPixelReadsScaledLocation;
begin
  Attach(2);
  FBuf[8 * cBufW + 6] := cRed;
  AssertEquals('Pixels[3,4]', cRed, FCanvas.Pixels[3, 4]);
  AssertEquals('Pixels[2,4]', 0, FCanvas.Pixels[2, 4]);
end;

procedure TTestHybridCanvasScale.TestBlitImagePartScalesEachPixel;
var
  img: TfpgImage;
begin
  img := TfpgImage.Create;
  try
    img.AllocateImage(32, 2, 1);
    img.Colors[0, 0] := TfpgColor($FF0000);
    img.Colors[1, 0] := TfpgColor($0000FF);
    img.UpdateImage;
    Attach(2);
    FCanvas.BlitImagePart(1, 1, img, 0, 0, 2, 1);
    AssertEquals('(2,2) first pixel', $FF0000, Px(2, 2));
    AssertEquals('(3,3) first pixel', $FF0000, Px(3, 3));
    AssertEquals('(4,2) second pixel', $0000FF, Px(4, 2));
    AssertEquals('(5,3) second pixel', $0000FF, Px(5, 3));
    AssertEquals('(6,2) untouched', 0, Px(6, 2));
    AssertEquals('(2,4) untouched', 0, Px(2, 4));
  finally
    img.Free;
  end;
end;

procedure TTestHybridCanvasScale.TestHorizontalLineIsCrisp;
begin
  Attach(2);
  FCanvas.SetColor(cRed);
  FCanvas.SetLineStyle(1, lsSolid);
  FCanvas.DrawLine(0, 5, 9, 5);
  { A 1-unit line at logical y=5 covers device rows 10 and 11 exactly }
  AssertEquals('row 10 fully covered', cRed, Px(8, 10));
  AssertEquals('row 11 fully covered', cRed, Px(8, 11));
  AssertEquals('row 9 untouched', 0, Px(8, 9));
  AssertEquals('row 12 untouched', 0, Px(8, 12));
end;

procedure TTestHybridCanvasScale.TestGetClipRectReturnsLogicalUnits;
var
  r: TfpgRect;
begin
  Attach(2);
  FCanvas.SetClipRect(fpgRect(1, 2, 3, 4));
  r := FCanvas.GetClipRect;
  AssertEquals('Left', 1, r.Left);
  AssertEquals('Top', 2, r.Top);
  AssertEquals('Width', 3, r.Width);
  AssertEquals('Height', 4, r.Height);
end;

{ TTestScaledFontResource }

procedure TTestScaledFontResource.TestScale1ReturnsSelf;
var
  fnt: TfpgFontResourceBase;
begin
  fnt := fpgApplication.FontManager.GetFont(FPG_DEFAULT_FONT_DESC);
  AssertSame('scale 1 is the font itself', fnt, fnt.GetScaledResource(1));
end;

procedure TTestScaledFontResource.TestScaledResourceHasScaledMetrics;
var
  fnt, scaled: TfpgFontResourceBase;
begin
  fnt := fpgApplication.FontManager.GetFont(FPG_DEFAULT_FONT_DESC);
  scaled := fnt.GetScaledResource(2);
  AssertTrue('distinct resource', scaled <> fnt);
  { Hinting rounds per size, so allow one pixel of slack }
  AssertTrue(Format('ascent %d ~ 2 x %d', [scaled.GetAscent, fnt.GetAscent]),
    Abs(scaled.GetAscent - 2 * fnt.GetAscent) <= 1);
  AssertTrue(Format('height %d ~ 2 x %d', [scaled.GetHeight, fnt.GetHeight]),
    Abs(scaled.GetHeight - 2 * fnt.GetHeight) <= 2);
end;

procedure TTestScaledFontResource.TestScaledResourceIsCached;
var
  fnt: TfpgFontResourceBase;
begin
  fnt := fpgApplication.FontManager.GetFont(FPG_DEFAULT_FONT_DESC);
  AssertSame('same scaled instance', fnt.GetScaledResource(2), fnt.GetScaledResource(2));
end;


initialization
  RegisterTest(TTestHybridCanvasScale);
  RegisterTest(TTestScaledFontResource);

end.
