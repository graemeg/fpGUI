{
  FPReport AggPas Export filter.

  The latest maintained AggPas code can be found as part of the fpGUI Toolkit
  project. This fpReport export filter uses the non-GUI "agg_2D.pas" unit, so
  it is not tied to any GUI framework, thus 100% non GUI and console friendly.

  AggPas located inside fpGUI:
    https://github.com/graemeg/fpGUI/tree/develop/src/corelib/render/software

  Usage in your project:
    Simply add the following path to your project's Unit Search Path:
      <fpgui>/src/corelib/render/software/

}
unit fpreport_export_aggpas;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpReport,
  fpImage,
  agg_2D;

type
  TRGBTriple = packed record
    Red: byte;
    Green: byte;
    Blue: byte;
    Alpha: byte;
  end;


  TFPReportExportAggPas = class(TFPReportExporter)
  private
    FBaseFileName: string;
    FDPI: integer;
    FImageWidth: integer;
    FImageHeight: integer;
    FOldFontDPI: integer;
    FTextHinting: boolean;
    function    FindFontFile(const AFontName: string): string;
    procedure   RenderBand(ABand: TFPReportCustomBand);
    procedure   RenderElement(ABand: TFPReportCustomBand; AElement: TFPReportElement);
  protected
    FRenderBuffer: TBytes;
    FAgg: Agg2D;
    Procedure   RenderImage(aPos : TFPReportRect; var AImage: TFPCustomImage) ; override;
    procedure   BufferToFile(const APageNo: integer); virtual;
    procedure   DoExecute(const ARTObjects: TFPList); override;
    procedure   SetupRenderBuffer(const APage: TFPReportPage); virtual;
    procedure   RenderFrame(const ABand: TFPReportCustomBand; const AFrame: TFPReportFrame; const APos: TFPReportPoint; const AWidth, AHeight: TFPReportUnits); virtual;
    procedure   RenderMemo(const ABand: TFPReportCustomBand; const AMemo: TFPReportCustomMemo); virtual;
    procedure   RenderShape(const ABand: TFPReportCustomBand; const AShape: TFPReportCustomShape); virtual;
    procedure   RenderImage(const ABand: TFPReportCustomBand; const AImage: TFPReportCustomImage); virtual;
    procedure   RenderCheckbox(const ABand: TFPReportCustomBand; const ACheckbox: TFPReportCustomCheckbox); virtual;
    procedure   RenderShape(const lpt1 : TFPReportPoint; const AShape: TFPReportCustomShape); virtual;
    procedure   RenderShapeCircle(const lpt1: TFPReportPoint; const ALayout: TFPReportLayout);
    procedure   RenderShapeEllipse(const lpt1: TFPReportPoint; const ALayout: TFPReportLayout);
    procedure   RenderShapeLine(lpt1: TFPReportPoint;  const AOrientation: TFPReportOrientation; const ALayout: TFPReportLayout);
    procedure   RenderShapeRect(const lpt1: TFPReportPoint; const ALayout: TFPReportLayout);
    procedure   RenderShapeTriangle(Alpt: TFPReportPoint; const AOrientation: TFPReportOrientation; const ALayout: TFPReportLayout);
    procedure   RenderShapeRoundedRect(const lpt1: TFPReportPoint; const ARadius: TFPReportUnits; const ALayout: TFPReportLayout);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    Class Function Name : String; override;
    Class Function Description : String; override;
    class function ColorToRGBTriple(const AColor: UInt32): TRGBTriple;
    property    BaseFileName: string read FBaseFileName write FBaseFileName;
    property    DPI: integer read FDPI write FDPI;
    property    ImageWidth: integer read FImageWidth write FImageWidth;
    property    ImageHeight: integer read FImageHeight write FImageHeight;
    property    RenderBuffer: TBytes read FRenderBuffer;
    property    TextHinting: boolean read FTextHinting write FTextHinting default False;
    function    mmToPixels(const AValue: TFPReportUnits): single;
    Property    Agg : Agg2D Read FAgg;
  end;



implementation

uses
  FPCanvas,
  FPWritePNG,
  fpTTF,
  fpparsettf,
  math;


const
  cInchToMM = 25.4;
  RGBA_Width = 4;


Type
  { for access to Protected methods }
  TReportImageFriend = class(TFPReportCustomImage);
  TReportCheckboxFriend = class(TFPReportCustomCheckbox);

  TFPImageFriend = class(TFPCompactImgRGBA8Bit)
  private
    procedure ReversePixelColorOrder;
  end;

{ TFPImageFriend }

procedure TFPImageFriend.ReversePixelColorOrder;
var
  x, y: UInt32;
  v: TFPCompactImgRGBA8BitValue;
  n: TFPCompactImgRGBA8BitValue;
begin
  for x := 0 to Width-1 do
    for y := 0 to Height-1 do
    begin
      v := FData[x+y*Width];
      n.b := v.r;
      n.g := v.g;
      n.r := v.b;
      n.a := v.a;
      FData[x+y*Width] := n;
    end;
end;



function GetRed(const AColor: UInt32): byte;
begin
  // AARRGGBB format
  Result := (AColor shr 16) and $FF;
end;

function GetGreen(const AColor: UInt32): byte;
begin
  // AARRGGBB format
  Result := (AColor shr 8) and $FF;
end;

function GetBlue(const AColor: UInt32): byte;
begin
  // AARRGGBB format
  Result := AColor and $FF;
end;

function GetAlpha(const AColor: UInt32): byte;
begin
  // AARRGGBB format
  Result := (AColor shr 24) and $FF;
end;

class function TFPReportExportAggPas.ColorToRGBTriple(const AColor: UInt32): TRGBTriple;
begin
  with Result do
  begin
    Red   := GetRed(AColor);
    Green := GetGreen(AColor);
    Blue  := GetBlue(AColor);
    Alpha := GetAlpha(AColor);
  end
end;


{ TFPReportExportAggPas }

function TFPReportExportAggPas.FindFontFile(const AFontName: string): string;
var
  fnt: TFPFontCacheItem;
begin
  fnt := gTTFontCache.Find(AFontName); // we are doing a PostScript Name lookup (it contains Bold, Italic info)
  if Assigned(fnt) then
    Result := fnt.FileName
  else
    raise Exception.CreateFmt('fpreport: Could not find the font <%s> in the font cache.', [AFontName]);
end;

procedure TFPReportExportAggPas.RenderBand(ABand: TFPReportCustomBand);
var
  lPt1: TFPReportPoint;  // original Report point
  i: integer;
begin
  lPt1.Left := ABand.RTLayout.Left;
  lPt1.Top := ABand.RTLayout.Top;
  RenderFrame(ABand, ABand.Frame, lPt1, ABand.RTLayout.Width, ABand.RTLayout.Height);
  for i := 0 to ABand.ChildCount-1 do
    RenderElement(ABand, ABand.Child[i]);
end;

procedure TFPReportExportAggPas.RenderElement(ABand: TFPReportCustomBand; AElement: TFPReportElement);
var
  C : TFPReportPoint;
begin
  if AElement is TFPReportCustomMemo then
    RenderMemo(ABand, TFPReportCustomMemo(AElement))
  else if AElement is TFPReportCustomShape then
    RenderShape(ABand, TFPReportCustomShape(AElement))
  else if AElement is TFPReportCustomImage then
    RenderImage(ABand, TFPReportCustomImage(AElement))
  else if AElement is TFPReportCustomCheckbox then
    RenderCheckbox(ABand, TFPReportCustomCheckbox(AElement))
  else
  begin
    C.Left := ABand.RTLayout.Left + aElement.RTLayout.Left;
    C.Top := ABand.RTLayout.Top + aElement.RTLayout.Top ; // + Element.RTLayout.Height;
    RenderFrame(ABand, AElement.Frame, C, AElement.RTLayout.Width, AElement.RTLayout.Height);
    C.Left := ABand.RTLayout.Left;
    C.Top := ABand.RTLayout.Top;
    RenderUnknownElement(C, AElement, DPI);
  end;
end;

procedure TFPReportExportAggPas.RenderImage(aPos: TFPReportRect; var AImage: TFPCustomImage);
var
  lPt: TFPReportPoint;
  img: TFPImageFriend;
  lAggImg: Image;
begin
  img := TFPImageFriend(AImage);  { for access to Protected methods }
  lPt.Left := aPos.Left;
  lPt.Top := aPos.Top;
  img.ReversePixelColorOrder;
  lAggImg.Construct(@img.FData[0], img.Width, img.Height, img.Width * Sizeof(UInt32));
  FAgg.copyImage(@lAggImg, mmToPixels(lPt.Left), mmToPixels(lPt.Top));
  lAggImg.Destruct;
end;

function TFPReportExportAggPas.mmToPixels(const AValue: TFPReportUnits): single;
begin
  Result := AValue * (DPI / cInchToMM);
end;

procedure TFPReportExportAggPas.BufferToFile(const APageNo: integer);
var
  image: TFPMemoryImage;
  writer: TFPWriterPNG;
  x, y: Integer;
  c: TFPColor;

  function getBufItemAsWord(aDelta: byte): Word;
  var
    actualY: Integer;
  begin
    actualY := ImageHeight - y - 1;
    result := Word(RenderBuffer[x * RGBA_Width + actualY * ImageWidth * RGBA_Width + aDelta] shl 8) or Word(128);
  end;

begin
  image := TFPMemoryImage.Create(ImageWidth, ImageHeight);
  try
    for x := 0 to ImageWidth - 1 do
    begin
      for y := 0 to ImageHeight - 1 do
      begin
        c.red := getBufItemAsWord(2);
        c.green := getBufItemAsWord(1);
        c.blue := getBufItemAsWord(0);
        c.alpha := getBufItemAsWord(3);
        image.Colors[x, y] := c;
      end;
    end;

    writer := TFPWriterPNG.Create;
    try
      image.SaveToFile(ExtractFilePath(ParamStr(0)) + Format(BaseFileName, [APageNo]), writer);
    finally
      writer.Free;
    end;
  finally
    image.Free;
  end;
end;

procedure TFPReportExportAggPas.SetupRenderBuffer(const APage: TFPReportPage);
var
  lStride: Integer;
begin
  SetLength(FRenderBuffer, 0);

  if APage.Orientation = poLandscape then
  begin
    ImageWidth := Round(mmToPixels(APage.PageSize.Height));
    ImageHeight := Round(mmToPixels(APage.PageSize.Width));
  end
  else
  begin
    ImageWidth := Round(mmToPixels(APage.PageSize.Width));
    ImageHeight := Round(mmToPixels(APage.PageSize.Height));
  end;

  SetLength(FRenderBuffer, ImageWidth * ImageHeight * RGBA_Width);

  // we use the -1 multiplier to flip the AggPas image co-ordinate system
  lStride := (ImageWidth * RGBA_Width) * -1;
  FAgg.Attach(@(RenderBuffer[0]), ImageWidth, ImageHeight, lStride);

  FAgg.clearAll(255, 255, 255);
  FAgg.flipText(True);
end;

procedure TFPReportExportAggPas.RenderFrame(const ABand: TFPReportCustomBand; const AFrame: TFPReportFrame;
  const APos: TFPReportPoint; const AWidth, AHeight: TFPReportUnits);
var
  bStroke, bFill: boolean;
  c: TRGBTriple;
begin
  bStroke := AFrame.Color <> clNone;
  bFill := AFrame.BackgroundColor <> clNone;

  // We only support TPDFPenStyle types. (ppsSolid,ppsDash,ppsDot,ppsDashDot,ppsDashDotDot)
  case AFrame.Pen of
    psSolid:      FAgg.SetLineStyle(AFrame.Width, stySolid);
    psDash:       FAgg.SetLineStyle(AFrame.Width, styDash);
    psDot:        FAgg.SetLineStyle(AFrame.Width, styDot);
    psDashDot:    FAgg.SetLineStyle(AFrame.Width, styDashDot);
    psDashDotDot: FAgg.SetLineStyle(AFrame.Width, styDashDotDot);
    // These FPCanvas pen styles are unsupported
//    psInsideFrame: ;
//    psPattern: ;
//    psClear: ;
    else
      // give a sane fallback for now
      FAgg.SetLineStyle(AFrame.Width, stySolid);
  end;

  if (AFrame.Shape = fsRectangle) and (bStroke or bFill) then
  begin
    if not bFill then
      FAgg.noFill
    else
    begin
      c := ColorToRGBTriple(AFrame.BackgroundColor);
      FAgg.fillColor(c.Red, c.Green, c.Blue); // ignore Alpha for now
    end;

    if not bStroke then
      FAgg.noLine
    else
    begin
      c := ColorToRGBTriple(AFrame.Color);
      FAgg.lineColor(c.Red, c.Green, c.Blue); // ignore Alpha for now
    end;

    FAgg.rectangle(mmToPixels(APos.Left), mmToPixels(APos.Top), mmToPixels(APos.Left+AWidth), mmToPixels(APos.Top+AHeight), True);
  end;

  if AFrame.Shape = fsNone then
  begin
    if AFrame.Lines <> [] then
    begin
      c := ColorToRGBTriple(AFrame.Color);
      FAgg.lineColor(c.Red, c.Green, c.Blue); // ignore Alpha for now
    end;

    if flTop in AFrame.Lines then
      FAgg.line(mmToPixels(APos.Left), mmToPixels(APos.Top), mmToPixels(APos.Left+AWidth), mmToPixels(APos.Top));

    if flBottom in AFrame.Lines then
      FAgg.line(mmToPixels(APos.Left), mmToPixels(APos.Top+AHeight), mmToPixels(APos.Left+AWidth), mmToPixels(APos.Top+AHeight));

    if flLeft in AFrame.Lines then
      FAgg.line(mmToPixels(APos.Left), mmToPixels(APos.Top), mmToPixels(APos.Left), mmToPixels(APos.Top+AHeight));

    if flRight in AFrame.Lines then
      FAgg.line(mmToPixels(APos.Left+AWidth), mmToPixels(APos.Top), mmToPixels(APos.Left+AWidth), mmToPixels(APos.Top+AHeight));
  end;  { Frame.Shape = fsNone }
end;

procedure TFPReportExportAggPas.RenderMemo(const ABand: TFPReportCustomBand; const AMemo: TFPReportCustomMemo);
var
  lPt1: TFPReportPoint;  // original Report point
  lFont: string;
  lMemo: TFPReportMemo;
  i: integer;
  lXPos: TFPReportUnits;
  lYPos: TFPReportUnits;
  txtblk: TFPTextBlock;
  c: TRGBTriple;
begin
  lMemo := TFPReportMemo(AMemo);

  { Store the Top-Left coordinate of the Memo. We will be reusing this info. }
  lPt1.Left := ABand.RTLayout.Left + AMemo.RTLayout.Left;
  lPt1.Top := ABand.RTLayout.Top + AMemo.RTLayout.Top;

  { Frame must be drawn before the text as it could have a fill color. }
  RenderFrame(ABand, AMemo.Frame, lPt1, AMemo.RTLayout.Width, AMemo.RTLayout.Height);

  { Text color is always the Fill color }
  FAgg.noLine;
  { TextHinting = False gives much more true-to-life glyph shapes as the font
    designers intended. No pixel grid fitting nonsense. }
  FAgg.textHints(FTextHinting);

  { render the TextBlocks as-is. }
  for i := 0 to lMemo.TextBlockList.Count-1 do
  begin
    txtblk := lMemo.TextBlockList[i];
    lFont := FindFontFile(txtblk.FontName);
    FAgg.font(PChar(lFont), gTTFontCache.PointSizeInPixels(lMemo.Font.Size));
    lXPos := lPt1.Left + txtblk.Pos.Left;
    lYPos := lPt1.Top + txtblk.Pos.Top;

    if txtblk.BGColor <> clNone then
    begin
      c := ColorToRGBTriple(txtblk.BGColor);
      FAgg.fillColor(c.Red, c.Green, c.Blue);
      { draw highlighting background rectangle }
      FAgg.rectangle(
          mmToPixels(lXPos),
          mmToPixels(lYPos - txtblk.Descender),
          mmToPixels(lXPos + txtblk.Width),
          mmToPixels(lYPos + txtblk.Height + txtblk.Descender)
      );
    end;

    c := ColorToRGBTriple(txtblk.FGColor);
    { Text color is always the Fill color. Also, ignore Alpha channel for now }
    FAgg.fillColor(c.Red, c.Green, c.Blue);
    { AggPas text origin coordinate is Bottom-Left, and Report Layout is Top-Left }
    lYPos := lYPos + txtblk.Height;
    FAgg.text(
        mmToPixels(lXPos),
        mmToPixels(lYPos),
        PChar(txtblk.Text)
    );
  end;
end;

procedure TFPReportExportAggPas.RenderShape(const ABand: TFPReportCustomBand; const AShape: TFPReportCustomShape);
var
  lPt1: TFPReportPoint;  // original Report point
begin
  lPt1.Left := ABand.RTLayout.Left + AShape.RTLayout.Left;
  lPt1.Top := ABand.RTLayout.Top + AShape.RTLayout.Top;
  { Frame must be drawn before the text as it could have a fill color. }
  RenderFrame(ABand, AShape.Frame, lPt1, AShape.RTLayout.Width, AShape.RTLayout.Height);
  RenderShape(lPt1, AShape);
end;

procedure TFPReportExportAggPas.RenderImage(const ABand: TFPReportCustomBand; const AImage: TFPReportCustomImage);
var
  lPt: TFPReportPoint;
  img: TReportImageFriend;
  rptimg: TFPImageFriend;
  lAggImg: Image;
begin
  img := TReportImageFriend(AImage);  { for access to Protected methods }
  lPt.Left := ABand.RTLayout.Left + AImage.RTLayout.Left;
  lPt.Top := ABand.RTLayout.Top + AImage.RTLayout.Top;

  { Frame must be drawn before the Image as it could have a fill color. }
  RenderFrame(ABand, AImage.Frame, lPt, AImage.RTLayout.Width, AImage.RTLayout.Height);

  if not Assigned(img.Image) then
    Exit; { nothing further to do }

  rptimg := TFPImageFriend(img.Image);
  rptimg.ReversePixelColorOrder;
  try
    lAggImg.Construct(@rptimg.FData[0], img.Image.Width, img.Image.Height, img.Image.Width * Sizeof(UInt32));

    if img.Stretched then
      FAgg.transformImage(@lAggImg, mmToPixels(lPt.Left), mmToPixels(lPt.Top), mmToPixels(lPt.Left+AImage.RTLayout.Width), mmToPixels(lPt.Top+AImage.RTLayout.Height))
    else
      FAgg.copyImage(@lAggImg, mmToPixels(lPt.Left), mmToPixels(lPt.Top));

    lAggImg.Destruct;
  finally
    rptimg.ReversePixelColorOrder; // undo what we did earlier
  end;
end;

procedure TFPReportExportAggPas.RenderCheckbox(const ABand: TFPReportCustomBand; const ACheckbox: TFPReportCustomCheckbox);
var
  lPt: TFPReportPoint;
  cb: TReportCheckboxFriend;
  lImage: TFPImageFriend;
  lAggImg: Image;
begin
  cb := TReportCheckboxFriend(ACheckbox);  { for access to Protected methods }
  lPt.Left := ABand.RTLayout.Left + ACheckbox.RTLayout.Left;
  lPt.Top := ABand.RTLayout.Top + ACheckbox.RTLayout.Top;

//  { Even though CheckBox has a Frame, we don't atually want to draw it }
//  RenderFrame(ABand, AImage.Frame, lPt, AImage.RTLayout.Width, AImage.RTLayout.Height);

  lImage := TFPImageFriend(cb.GetRTImage);

  lAggImg.Construct(pointer(lImage.FData), lImage.Width, lImage.Height, lImage.Width * Sizeof(UInt32));
  FAgg.transformImage(@lAggImg, mmToPixels(lPt.Left), mmToPixels(lPt.Top), mmToPixels(lPt.Left+ACheckBox.RTLayout.Width), mmToPixels(lPt.Top+ACheckBox.RTLayout.Height));
  lAggImg.Destruct;
end;

procedure TFPReportExportAggPas.RenderShape(const lpt1: TFPReportPoint; const AShape: TFPReportCustomShape);
var
  c: TRGBTriple;
begin
  c := ColorToRGBTriple(TFPReportShape(AShape).Color);
  FAgg.noFill;
  FAgg.lineColor(c.Red, c.Green, c.Blue);
  FAgg.lineWidth(1);

  case TFPReportShape(AShape).ShapeType of
    stEllipse:      RenderShapeEllipse(lpt1, AShape.RTLayout);
    stCircle:       RenderShapeCircle(lpt1, AShape.RTLayout);
    stLine:         RenderShapeLine(lpt1, TFPReportShape(AShape).Orientation, AShape.RTLayout);
    stSquare:       RenderShapeRect(lpt1, AShape.RTLayout);
    stTriangle:     RenderShapeTriangle(lpt1, TFPReportShape(AShape).Orientation, AShape.RTLayout);
    stRoundedRect:  RenderShapeRoundedRect(lpt1, TFPReportShape(AShape).CornerRadius, AShape.RTLayout);
  end;
end;

procedure TFPReportExportAggPas.RenderShapeCircle(const lpt1: TFPReportPoint; const ALayout: TFPReportLayout);
var
  lw: TFPReportUnits;
begin
  lw := Min(ALayout.Width, ALayout.Height);
  FAgg.Ellipse(
      mmToPixels(lPt1.Left+(ALayout.Width / 2)),
      mmToPixels(lPt1.Top+(ALayout.Height / 2)),
      mmToPixels(lw / 2),
      mmToPixels(lw / 2)
  );
end;

procedure TFPReportExportAggPas.RenderShapeEllipse(const lpt1: TFPReportPoint;
    const ALayout: TFPReportLayout);
begin
  FAgg.Ellipse(
      mmToPixels(lPt1.Left+(ALayout.Width / 2)),
      mmToPixels(lPt1.Top+(ALayout.Height / 2)),
      mmToPixels(ALayout.Width / 2),
      mmToPixels(ALayout.Height / 2)
  );
end;

procedure TFPReportExportAggPas.RenderShapeLine(lpt1: TFPReportPoint;
    const AOrientation: TFPReportOrientation; const ALayout: TFPReportLayout);
var
  lPt2: TFPReportPoint;
begin
  case AOrientation of
  orNorth, orSouth:
   begin                                           //   |
     lPt1.Left := lPt1.Left + (ALayout.Width / 2); //   |
     lPt2.Left := lPt1.Left ;                      //   |
     lPt2.Top := LPT1.Top + ALayout.Height;        //   |
   end;
  orNorthEast, orSouthWest:
   begin                                           //    /
     lPt2.Left := lPt1.Left;                       //   /
     lPt1.Left := lPt1.Left + ALayout.Width;       //  /
     lPt2.Top := lPt1.Top + ALayout.Height;        // /
   end;
  orEast, orWest:
   begin                                           //
     lPt2.Left := lPt1.Left + ALayout.Width;       // ----
     lPt1.Top := lPt1.Top + (ALayout.Height / 2);  //
     lPt2.Top := lPt1.Top;                         //
   end;
  orSouthEast, orNorthWest:
   begin                                           // \
     lPt2.Left := lPt1.Left + ALayout.Width;       //  \
     lPt2.Top := lPt1.Top + ALayout.Height;        //   \
   end;                                            //    \
  end;
  FAgg.Line(mmToPixels(lPt1.Left), mmToPixels(lPt1.Top), mmToPixels(lPt2.Left), mmToPixels(lPt2.Top), False);
end;

procedure TFPReportExportAggPas.RenderShapeRect(const lpt1: TFPReportPoint; const ALayout: TFPReportLayout);
var
  ldx, ldy, lw: TFPReportUnits;
  P: TFPReportPoint;
begin
  lw := Min(ALayout.Width, ALayout.Height);
  if ALayout.Width = ALayout.Height then
  begin
    ldx := 0;
    ldy := 0;
  end
  else if ALayout.Width > ALayout.Height then
  begin
    ldx := (ALayout.Width - ALayout.Height) / 2;
    ldy := 0;
  end
  else if ALayout.Width < ALayout.Height then
  begin
    ldx := 0;
    ldy := (ALayout.Height - ALayout.Width) / 2;
  end;
  P.Left := lPt1.Left + ldx;
  { PDF origin coordinate is Bottom-Left, and Report Layout is Top-Left }
  P.Top := lPt1.Top + ldy;
  FAgg.Rectangle(mmToPixels(P.Left), mmToPixels(P.Top), mmToPixels(P.Left+lw), mmToPixels(P.Top+lw), False);
end;

procedure TFPReportExportAggPas.RenderShapeTriangle(Alpt: TFPReportPoint;
    const AOrientation: TFPReportOrientation; const ALayout: TFPReportLayout);
var
  lPt1, lPt2, lPt3: TFPReportPoint;  // original Report points for 3 corners of triangle.
begin
  case AOrientation of
    orNorth:
    begin
      lPt1.Left := ALPT.Left + (ALayout.Width / 2); //      1
      lPt1.Top  := ALPT.Top;                        //      /\
      lPt2.Left := ALPT.Left;                       //     /  \
      lPt2.Top  := ALPT.Top + ALayout.Height;       //    /____\
      lPt3.Left := ALPT.Left + ALayout.Width;       //  2       3
      lPt3.Top  := lPt2.Top;
    end;
    orNorthEast:
    begin
      lPt1.Left := ALPT.Left + (ALayout.Width );    //   +-------1
      lPt1.Top  := ALPT.Top;                        //   |       |
      lPt2.Left := ALPT.Left;                       //   2       |
      lPt2.Top  := ALPT.Top + ALayout.Height / 2;   //   |       |
      lPt3.Left := ALPT.Left + ALayout.Width / 2;   //   +---3---+
      lPt3.Top  := lPt1.Top + ALayout.height;
    end;
    orSouth:
    begin
      lPt1.Left := ALPT.Left;                        //  1 ------ 2
      lPt1.Top  := ALPT.Top;                         //    \    /
      lPt2.Left := ALPT.Left + ALayout.Width;        //     \  /
      lPt2.Top  := ALPT.Top;                         //      \/
      lPt3.Left := ALPT.Left + (ALayout.Width / 2);  //      3
      lPt3.Top  := ALPT.Top + ALayout.Height;
    end;
    orSouthEast:
    begin
      lPt1.Left := ALPT.Left + (ALayout.Width / 2);   //   +---1---+
      lPt1.Top  := ALPT.Top;                          //   |       |
      lPt2.Left := ALPT.Left;                         //   2       |
      lPt2.Top  := ALPT.Top + ALayout.Height / 2;     //   |       |
      lPt3.Left := ALPT.Left + ALayout.Width;         //   +-------3
      lPt3.Top  := lPt1.Top + ALayout.Height;
    end;
    orEast:
    begin
      lPt1.Left := ALPT.Left;                       //   1
      lPt1.Top  := Alpt.Top;                        //   |\
      lPt2.Left := ALPT.Left + ALayout.Width;       //   | \ 2
      lPt2.Top  := ALPT.Top + (ALayout.Height / 2); //   | /
      lPt3.Left := ALPT.Left;                       //   |/
      lPt3.Top  := Alpt.Top + ALayout.Height;       //   3
    end;
    orNorthWest:
    begin
      lPt1.Left := ALPT.Left;                       //   1-------+
      lPt1.Top  := ALPT.Top;                        //   |       |
      lPt2.Left := ALPT.Left + ALayout.width;       //   |       2
      lPt2.Top  := ALPT.Top + ALayout.Height / 2;   //   |       |
      lPt3.Left := ALPT.Left + ALayout.Width / 2;   //   +---3---+
      lPt3.Top  := lPt1.Top + ALayout.height;
    end;
    orWest:
    begin
      lPt1.Left := ALPT.Left + ALayout.Width;      //       1
      lPt1.Top  := ALPT.Top;                       //      /|
      lPt2.Left := ALPT.Left;                      //   2 / |
      lPt2.Top  := ALPT.Top + ALayout.Height / 2;  //     \ |
      lPt3.Left := ALPT.Left + ALayout.Width;      //      \|
      lPt3.Top  := ALPT.Top + ALayout.Height;      //       3
    end;
    orSouthWest:
    begin
      lPt1.Left := ALPT.Left + ALayout.Height / 2;   //   +---1---+
      lPt1.Top  := ALPT.Top;                         //   |       |
      lPt2.Left := ALPT.Left + ALayout.Width;        //   |       2
      lPt2.Top  := ALPT.Top + ALayout.Height / 2;    //   |       |
      lPt3.Left := ALPT.Left;                        //   3-------+
      lPt3.Top  := lPt1.Top + ALayout.height;
    end;
  end; { case - Orientation }
  FAgg.line(mmToPixels(lPt1.Left), mmToPixels(lPt1.Top), mmToPixels(lPt2.Left), mmToPixels(lPt2.Top));
  FAgg.line(mmToPixels(lPt2.Left), mmToPixels(lPt2.Top), mmToPixels(lPt3.Left), mmToPixels(lPt3.Top));
  FAgg.line(mmToPixels(lPt3.Left), mmToPixels(lPt3.Top), mmToPixels(lPt1.Left), mmToPixels(lPt1.Top));
end;

procedure TFPReportExportAggPas.RenderShapeRoundedRect(const lpt1: TFPReportPoint;
  const ARadius: TFPReportUnits; const ALayout: TFPReportLayout);
var
  lPt2: TFPReportPoint;
begin
  lPt2.Left := lPt1.Left + ALayout.Width;
  lPt2.Top := lPt1.Top + ALayout.Height;
  FAgg.roundedRect(mmToPixels(lpt1.Left), mmToPixels(lpt1.Top), mmToPixels(lPt2.Left), mmToPixels(lPt2.Top), mmToPixels(ARadius));
end;

constructor TFPReportExportAggPas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAgg.Construct;
  FBaseFileName := ApplicationName + '-%.2d.png';
  FDPI := 96;
  FImageWidth := 0;
  FImageHeight := 0;
  FTextHinting := False;
  SetLength(FRenderBuffer, 0);

  // store the original DPI, we will restore it later
  FOldFontDPI := gTTFontCache.DPI;
  // fonts always seem to be in 72 dpi
  gTTFontCache.DPI := 72;
end;

destructor TFPReportExportAggPas.Destroy;
begin
  FAgg.Destruct;
  SetLength(FRenderBuffer, 0);
  // restore original DPI value
  gTTFontCache.DPI := FOldFontDPI;
  inherited Destroy;
end;

class function TFPReportExportAggPas.Name: String;
begin
  Result:='AggPas';
end;

class function TFPReportExportAggPas.Description: String;
begin
  Result:='Image file export (using AggPas)';
end;

procedure TFPReportExportAggPas.DoExecute(const ARTObjects: TFPList);
var
  p, b: integer;
  rpage: TFPReportPage;
begin
  for p := 0 to (ARTObjects.Count - 1) do { pages }
  begin
    rpage := TFPReportPage(ARTObjects[p]);
    SetupRenderBuffer(rpage);
    for b := 0 to (rpage.BandCount - 1) do
      RenderBand(rpage.Bands[b]);
    BufferToFile(p+1);
  end;
end;

initialization
  TFPReportExportAggPas.RegisterExporter;
end.

