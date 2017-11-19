{
    This file is part of the Free Component Library.
    Copyright (c) 2016 Michael Van Canneyt,
    member of the Free Pascal development team

    FPReport FPImage export filter.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  FPImage Export filter.

  FPImage is included as standard with FPC. This exporter uses those classes
  to generate image output. One image per report page, and by default as a
  PNG image.

}
unit fpreportfpimageexport;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpReport,
  fpimage,
  fpcanvas,
  fpreportcanvashelper,
  contnrs,
  dom,
  dom_html,
  htmwrite,
  FPImgCmn,
  fpimgcanv,
  fpreporthtmlutil;

type

  { TFPReportExportfpImage }
  THTMLOption = (hoEnabled,hoTOCPage,hoFramePage,hoExternalJS);
  THTMLOptions = set of THTMLOption;

  TFPReportExportfpImage = class(TFPReportExporter)
  private
    FBaseFileName: string;
    FImage : TFPCustomImage;
    FCanvas : TFPCustomCanvas;
    FDPI: integer;
    FImageWidth: integer;
    FImageHeight: integer;
    FOldFontDPI: integer;
    FFonts : TFPObjectHashTable;
    FSequenceFormat: string;
    FWriterClass : TFPCustomImageWriterClass;
    FHelper : TFPReportCanvasHelper;
    // Html support
    FCSSDir: String;
    FFramePage: TFramePageOptions;
    FHTMLOptions: THTMLOptions;
    FPageNavigator: TPageNavigatorOptions;
    FStyleEmbedding: TStyleEmbedding;
    FTOCPage: TTOCPageOptions;
    FContext : TGenerateHTMLContext;
    FDoc : THTMLDocument;
    FHeadElement : THTMLElement;
    FBodyElement : THTMLElement;
    procedure FinishHTMLPage(AFileName: String);
    procedure SetCanvas(AValue: TFPCustomCanvas);
    procedure SetDPI(AValue: integer);
    procedure SetFramePage(AValue: TFramePageOptions);
    procedure SetPageNavigator(AValue: TPageNavigatorOptions);
    procedure SetTOCPage(AValue: TTOCPageOptions);
  protected
    function CreateFramePageOptions: TFramePageOptions; virtual;
    function CreatePageNavigatorOptions: TPageNavigatorOptions; virtual;
    function CreateTOCPageOptions: TTOCPageOptions; virtual;
    Procedure RenderImage(aRect : TFPReportRect; var AImage: TFPCustomImage) ; override;
    procedure RenderBand(Aband: TFPReportCustomBand); virtual;
    procedure RenderElement(ABand: TFPReportCustomBand; Element: TFPReportElement); virtual;
    procedure ConfigWriter(AWriter: TFPCustomImageWriter); virtual;
    procedure SetBaseFileName(AValue: string); override;
    function CoordToPoint(const APos: TFPReportPoint;  const AHOffset: TFPReportUnits=0; const AVoffset: TFPReportUnits=0): TPoint;
    function CoordToRect(const APos: TFPReportPoint; const AWidth: TFPReportUnits=0; const AHeight: TFPReportUnits=0): TRect;
    function  FindFontFile(const AFontName: string): string;
    Function BufferToFile(const APageNo: integer) : String; virtual;
    procedure SetupPageRender(const APage: TFPReportPage);
    function  CreateCanvas(AImage: TFPCustomImage): TFPCustomCanvas; virtual;
    function  CreateImage(const AWidth, AHeight: Integer): TFPCustomImage; virtual;
    procedure PrepareCanvas; virtual;
    procedure DoExecute(const ARTObjects: TFPList); override;
    procedure RenderFrame(const ABand: TFPReportCustomBand; const AFrame: TFPReportFrame; const APos: TFPReportPoint; const AWidth, AHeight: TFPReportUnits); virtual;
    procedure RenderMemo(const ABand: TFPReportCustomBand; const AMemo: TFPReportCustomMemo); virtual;
    procedure RenderShape(const ABand: TFPReportCustomBand; const AShape: TFPReportCustomShape); virtual;
    procedure RenderImage(const ABand: TFPReportCustomBand; const AImage: TFPReportCustomImage); virtual;
    procedure RenderCheckbox(const ABand: TFPReportCustomBand; const ACheckbox: TFPReportCustomCheckbox); virtual;
    procedure RenderShape(const lpt1 : TFPReportPoint; const AShape: TFPReportCustomShape); virtual;
    // HTML support
    function SetupHTMLPageRender(const APage: TFPReportPage): THTMLElement; virtual;
    // HTML Wrapper page. Return file name
    function CreateHTMLFileForImage(const ImageFile: String; APage: TFPReportPage; APageNo: Integer): String; virtual;
    // This creates the actual element that contains the image
    function CreateImageElement(const ImageFile: String): THTMLElement; virtual;
    // Frame page
    procedure CreateFramePage(PageNames: TStrings; const ATOCPageFileName: String); virtual;
    // TOC page
    function CreateTOCPage(APageNames: TStrings; ForFrame: Boolean): String; virtual;
    // Navigator
    Procedure CreatePageNavigator(aPosition : TNavigatorPosition; AParentDiV : THTMLElement; APage : TFPReportPage; APageNo : integer); virtual;
    Property Context : TGenerateHTMLContext Read FContext;
    Property Doc : THTMLDocument Read FDoc;
    Property HeadElement : THTMLElement Read FHeadElement;
    Property BodyElement : THTMLElement Read FBodyElement;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    Class Function Name : String; override;
    Class Function Description : String; override;
    Class Function DefaultExtension: String; override;
    Class Function MultiFile: Boolean; override;
    Procedure GenerateHTML(ImageFiles : TStrings);
    Procedure SetFileName(const aFileName: String); override;
    function GetFont(const AFontName: String): TFPCustomFont;
    function mmToPixels(const AValue: TFPReportUnits): Integer;
    class function ColorToRGBTriple(const AColor: UInt32): TFPColor;
    Property Canvas : TFPCustomCanvas Read FCanvas Write SetCanvas;
    Property Image : TFPCustomImage Read FImage;
    Property Helper : TFPReportCanvasHelper Read FHelper;
  Published
    Property BaseFileName : String Read FBaseFIleName Write SetBaseFileName;
    property SequenceFormat : string read FSequenceFormat write FSequenceFormat;
    property DPI: integer read FDPI write SetDPI;
    property ImageWidth: integer read FImageWidth;
    property ImageHeight: integer read FImageHeight;
    Property HTMLOptions : THTMLOptions Read FHTMLOptions Write FHTMLOptions;
    Property StyleEmbedding : TStyleEmbedding Read FStyleEmbedding Write FStyleEmbedding;
    Property CSSDir : String Read FCSSDir Write FCSSDir;
    Property TOCPage : TTOCPageOptions Read FTOCPage Write SetTOCPage;
    Property FramePage: TFramePageOptions Read FFramePage Write SetFramePage;
    Property PageNavigator: TPageNavigatorOptions Read FPageNavigator Write SetPageNavigator;
  end;

implementation

uses
  ftFont,
  fpTTF,
  fpparsettf,
  fpwritepng;


//  RGBA_Width = 4;

type

  { for access to Protected methods }
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



{ TFPReportExportfpImage }

function TFPReportExportfpImage.FindFontFile(const AFontName: string): string;
var
  fnt: TFPFontCacheItem;
begin
  fnt := gTTFontCache.Find(AFontName); // we are doing a PostScript Name lookup (it contains Bold, Italic info)
  if Assigned(fnt) then
    Result := fnt.FileName
  else
    raise EReportExportError.CreateFmt('fpreport: Could not find the font <%s> in the font cache.', [AFontName]);
end;

function TFPReportExportfpImage.mmToPixels(const AValue: TFPReportUnits): Integer;
begin
  Result := Round(AValue * (DPI / cInchToMM));
end;

class function TFPReportExportfpImage.ColorToRGBTriple(const AColor: UInt32
  ): TFPColor;
begin
  Result:=TFPReportCanvasHelper.ColorToRGBTriple(AColor);
end;

procedure TFPReportExportfpImage.ConfigWriter(AWriter: TFPCustomImageWriter);

begin
  {
    Do nothing.
    Maybe at a later point set published properties of writer based on a Params Tstrings property
    With Name=Value pairs.
  }
end;

function TFPReportExportfpImage.BufferToFile(const APageNo: integer): String;

var
  Writer: TFPCustomImageWriter;
  E,SN,FN : String;

begin
  SN:=Format(SequenceFormat,[APageNo]);
  E:=ExtractFileExt(BaseFileName);
  FN:=ChangeFileExt(BaseFileName,SN+E);
  Writer:=FWriterClass.Create;
  try
    ConfigWriter(Writer);
    FImage.SaveToFile(FN,writer);
    Result:=FN;
  finally
    Writer.Free;
  end;
end;

procedure TFPReportExportfpImage.SetupPageRender(const APage: TFPReportPage);
begin
  FreeAndNil(FCanvas);
  FreeAndNil(FHelper);
  FreeAndNil(FImage);
  if APage.Orientation = poLandscape then
  begin
    FImageWidth := mmToPixels(APage.PageSize.Height);
    FImageHeight := mmToPixels(APage.PageSize.Width);
  end
  else
  begin
    FImageWidth := mmToPixels(APage.PageSize.Width);
    FImageHeight := mmToPixels(APage.PageSize.Height);
  end;
  FImage:=CreateImage(FImageWidth,FImageHeight);
  FCanvas:=CreateCanvas(FImage);
  FHelper:=TFPReportCanvasHelper.Create(FCanvas,DPI);
  PrepareCanvas;
end;

function TFPReportExportfpImage.CreateImage(const AWidth, AHeight: Integer
  ): TFPCustomImage;

begin
  Result:=TFPCompactImgRGB8Bit.Create(AWidth,AHeight);
end;


function TFPReportExportfpImage.CreateCanvas(AImage: TFPCustomImage
  ): TFPCustomCanvas;

begin
  Result:=TFPImageCanvas.create(FImage);
end;

procedure TFPReportExportfpImage.PrepareCanvas;

begin
  Canvas.Pen.Style:=psSolid;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.FPColor:=colWhite;
  Canvas.FillRect(0,0,ImageWidth-1,ImageHeight-1);
end;

function TFPReportExportfpImage.CoordToPoint(const APos: TFPReportPoint;
  const AHOffset: TFPReportUnits; const AVoffset: TFPReportUnits): TPoint;

begin
  Result:=FHelper.CoordToPoint(aPos,aHOffset,aVoffset);
end;

function TFPReportExportfpImage.CoordToRect(const APos: TFPReportPoint;
  const AWidth: TFPReportUnits; const AHeight: TFPReportUnits): TRect;

begin
  Result:=FHelper.CoordToRect(aPos,AWidth,AHeight);
end;

procedure TFPReportExportfpImage.RenderFrame(const ABand: TFPReportCustomBand; const AFrame: TFPReportFrame;
  const APos: TFPReportPoint; const AWidth, AHeight: TFPReportUnits);

var
  bStroke, bFill: boolean;
  FR : TRect;

begin
  bStroke := AFrame.Color <> clNone;
  bFill := AFrame.BackgroundColor <> clNone;
  if not (bStroke or bFill) then
    exit;
  FR:=CoordToRect(APos,AWidth,AHeight);
  Canvas.Pen.Style:=AFrame.Pen;
  Canvas.Pen.FPColor:=ColorToRGBTriple(AFrame.Color);
  Canvas.Pen.Width:=AFrame.Width;
  if (AFrame.Shape=fsRectangle) and (bStroke or bFill) then
    begin
    if bFill then
      begin
      Canvas.Brush.Style:=bsSolid;
      Canvas.Brush.FPColor:=ColorToRGBTriple(AFrame.BackgroundColor);
      FCanvas.FillRect(FR);
      end;
    if bStroke then
      begin
      Canvas.Brush.Style:=bsClear;
      FCanvas.Rectangle(FR);
      end;
    end;
  if (AFrame.Shape=fsNone) and bStroke then
    begin
    if (flTop in AFrame.Lines) then
      FCanvas.line(FR.Left, fr.Top,fr.Right,fr.Top);
    if (flbottom in AFrame.Lines) then
      FCanvas.line(FR.Left, fr.Bottom,fr.Right,fr.Bottom);
    if (flLeft in AFrame.Lines) then
      FCanvas.line(FR.Left, fr.Top,FR.Left,fr.Bottom);
    if (flRight in AFrame.Lines) then
      FCanvas.line(FR.Right, fr.Top,FR.Right,fr.Bottom);
    end;  { Frame.Shape = fsNone }
end;

Type
  THackReportMemo = class(TFPReportCustomMemo)
  public
    property  Font;
  end;

function TFPReportExportfpImage.GetFont(const AFontName: String): TFPCustomFont;

Var
  FFN : String;
  lFTFont : TFreeTypeFont;

begin
  Result:=Nil;
  Result:=TFPCustomFont(FFonts.Items[AFontName]);
  If (Result=Nil) then
    begin
    FFN:=FindFontFile(AFontName);
    lFTFont:=TFreeTypeFont.create;
    lFTFont.Name:=FFN;
    Result:=lFTFont;
    FFonts.Add(AFontName,Result);
    end;
end;

procedure TFPReportExportfpImage.RenderMemo(const ABand: TFPReportCustomBand; const AMemo: TFPReportCustomMemo);
var
  lPt1: TFPReportPoint;  // original Report point
  lMemo: THackReportMemo;
  i: integer;
  lXPos: TFPReportUnits;
  lYPos: TFPReportUnits;
  txtblk: TFPTextBlock;
begin
  lMemo := THackReportMemo(AMemo);

  { Store the Top-Left coordinate of the Memo. We will be reusing this info. }
  lPt1.Left := ABand.RTLayout.Left + AMemo.RTLayout.Left;
  lPt1.Top := ABand.RTLayout.Top + AMemo.RTLayout.Top;

  { Frame must be drawn before the text as it could have a fill color. }
  RenderFrame(ABand, AMemo.Frame, lPt1, AMemo.RTLayout.Width, AMemo.RTLayout.Height);

  { render the TextBlocks as-is. }
  for i := 0 to lMemo.TextBlockList.Count-1 do
  begin
    txtblk := lMemo.TextBlockList[i];
    Canvas.Font := GetFont(txtblk.FontName);
    Canvas.Font.Size := Round(gTTFontCache.PointSizeInPixels(lMemo.Font.Size));
    lXPos := lPt1.Left + txtblk.Pos.Left;
    lYPos := lPt1.Top + txtblk.Pos.Top;

    if txtblk.BGColor <> clNone then
    begin
      Canvas.Pen.Style := psClear;
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.FPColor := ColorToRGBTriple(txtblk.BGColor);
      Canvas.Rectangle(
          mmToPixels(lXPos),
          mmToPixels(lYPos - txtblk.Descender),
          mmToPixels(lXPos + txtblk.Width),
          mmToPixels(lYPos + txtblk.Height + txtblk.Descender)
      );
    end;
    Canvas.Font.FPColor := ColorToRGBTriple(txtblk.FGColor);
    { FPImage text origin coordinate is Bottom-Left, and Report Layout is Top-Left }
    lYPos := lYPos + txtblk.Height;
    Canvas.TextOut(
        mmToPixels(lXPos),
        mmToPixels(lYPos),
        txtblk.Text
    );
  end;
end;

procedure TFPReportExportfpImage.RenderShape(const ABand: TFPReportCustomBand; const AShape: TFPReportCustomShape);

var
  lPt1: TFPReportPoint;  // original Report point

begin
  lPt1.Left := ABand.RTLayout.Left + AShape.RTLayout.Left;
  lPt1.Top := ABand.RTLayout.Top + AShape.RTLayout.Top;
  { Frame must be drawn before the text as it could have a fill color. }
  RenderFrame(ABand, AShape.Frame, lPt1, AShape.RTLayout.Width, AShape.RTLayout.Height);
  RenderShape(lpt1,AShape);
end;

procedure TFPReportExportfpImage.RenderImage(const ABand: TFPReportCustomBand; const AImage: TFPReportCustomImage);

var
  lPt: TFPReportPoint;

begin
  lPt.Left := ABand.RTLayout.Left + AImage.RTLayout.Left;
  lPt.Top := ABand.RTLayout.Top + AImage.RTLayout.Top;
  { Frame must be drawn before the Image as it could have a fill color. }
  RenderFrame(ABand, AImage.Frame, lPt, AImage.RTLayout.Width, AImage.RTLayout.Height);
  FHelper.RenderImage(lpt,AImage);
end;

procedure TFPReportExportfpImage.RenderCheckbox(const ABand: TFPReportCustomBand; const ACheckbox: TFPReportCustomCheckbox);

var
  lPt: TFPReportPoint;

begin
  lPt.Left := ABand.RTLayout.Left + ACheckbox.RTLayout.Left;
  lPt.Top := ABand.RTLayout.Top + ACheckbox.RTLayout.Top;
  FHelper.RenderCheckBox(lpt,aCheckbox);
end;

procedure TFPReportExportfpImage.RenderShape(const lpt1: TFPReportPoint; const AShape: TFPReportCustomShape);
begin
  FHelper.RenderShape(lpt1,AShape);
end;

procedure TFPReportExportfpImage.CreateFramePage(PageNames: TStrings; const ATOCPageFileName: String);
Var
  TFCP : TTOCFramePageCreator;

begin
  TFCP:=Nil;
  try
    TFCP:=TTOCFramePageCreator.Create(FContext,FramePage);
    TFCP.CreateFramePage(PageNames,ATocPageFileName);
  finally
    TFCP.Free;
  end;
end;

function TFPReportExportfpImage.CreateTOCPage(APageNames: TStrings; ForFrame: Boolean): String;
Var
  TPC : TTOCPageCreator;

begin
  TPC:=TTOCPageCreator.Create(FCOntext,TOCPage);
  try
    Result:=TPC.CreateTOCPage(APageNames,ForFrame);
  finally
    TPC.Free;
  end;
end;

procedure TFPReportExportfpImage.CreatePageNavigator(aPosition: TNavigatorPosition; AParentDiV: THTMLElement; APage : TFPReportPage; APageNo :Integer);

Var
  PNEC : TPageNavigatorElementCreator;

begin
  PNEC:=TPageNavigatorElementCreator.Create(FContext,APage,PageNavigator);
  try
    FContext.CurrentPageNo:=APageNo;
    PNec.CreatePageNavigator(aPosition,aParentDiv);
  finally
    PNEC.Free;
  end;
end;

constructor TFPReportExportfpImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetBaseFileName(ApplicationName + '-'+DefaultExtension);
  SequenceFormat := '%.2d';
  FDPI := 96;
  FImageWidth := 0;
  FImageHeight := 0;
  // store the original DPI, we will restore it later
  FOldFontDPI := gTTFontCache.DPI;
  // fonts always seem to be in 72 dpi
  gTTFontCache.DPI := 72;
  FFonts:=TFPObjectHashTable.Create(True);
  FFramePage:=CreateFramePageOptions;
  FPageNavigator:=CreatePageNavigatorOptions;
  FTOCPage:=CreateTOCPageOptions;
end;

destructor TFPReportExportfpImage.Destroy;
begin
  FreeAndNil(FFramePage);
  FreeAndNil(FPageNavigator);
  FreeAndNil(FTOCPage);
  FreeAndNil(FFonts);
  FreeAndNil(FHelper);
  FreeAndNil(FCanvas);
  FreeAndNil(FImage);
  gTTFontCache.DPI := FOldFontDPI;
  inherited Destroy;
end;

function TFPReportExportfpImage.CreateTOCPageOptions: TTOCPageOptions;
begin
  Result:=TTOCPageOptions.Create;
end;

function TFPReportExportfpImage.CreateFramePageOptions: TFramePageOptions;

begin
  Result:=TFramePageOptions.Create;
  Result.TOCZoneSize:=10;
end;


function TFPReportExportfpImage.CreatePageNavigatorOptions: TPageNavigatorOptions;
begin
  Result:=TPageNavigatorOptions.Create;
  Result.Positions:=[npTop];
  Result.Options:=[hnoFirstLast,hnoPageNo];
  Result.FixedWidth:=80;
  Result.FixedHeight:=32;
  Result.FixedMargin:=8;
end;

class function TFPReportExportfpImage.Name: String;
begin
  Result:='FPImage';
end;

class function TFPReportExportfpImage.Description: String;
begin
  Result:='Image file export (using FPImage)';
end;

class function TFPReportExportfpImage.DefaultExtension: String;
begin
  Result:='.png';
end;

class function TFPReportExportfpImage.MultiFile: Boolean;
begin
  Result:=True
end;

function TFPReportExportfpImage.SetupHTMLPageRender(const APage: TFPReportPage): THTMLElement;

Var
  TitleElement,HTMLEl, El : TDomElement;
  D : String;

begin
  FDoc:=THTMLDocument.Create;
  FDoc.AppendChild(FDoc.Impl.CreateDocumentType(
    'HTML', '-//W3C//DTD HTML 4.01 Transitional//EN',
    'http://www.w3.org/TR/html4/loose.dtd'));
  HTMLEl := FDoc.CreateHtmlElement;
  FDoc.AppendChild(HTMLEl);
  FHeadElement:=FDoc.CreateHeadElement;
  HTMLEl.AppendChild(FHeadElement);
  El := FDoc.CreateElement('meta');
  El['http-equiv'] := 'Content-Type';
  El['content'] := 'text/html; charset=utf-8';
  FHeadElement.AppendChild(El);
  FContext.ResetPage(FDoc);
  Case StyleEmbedding of
    seInline : ;
    seCSSFile :
      begin
      El:=FDoc.CreateElement('link');
      El['rel']:='stylesheet';
      D:=CSSDir;
      if (D<>'') then
        D:=D+'/'; // not pathdelim !
      El['href']:=D+ChangeFileExt(ExtractFileName(BaseFileName),'.css');
      FHeadElement.AppendChild(El);
      end
  end;
  TitleElement := FDoc.CreateElement('title');
  FHeadElement.AppendChild(TitleElement);
  FBodyElement := FDoc.CreateElement('body');
  HTMLEl.AppendChild(FBodyElement);
  Result:=Context.CreateDiv(Nil,aPage);
end;

Procedure TFPReportExportfpImage.FinishHTMLPage(AFileName : String);

Var
  F,D : String;
  Sel,El : THTMLElement;
  Script : TStrings;

begin
  Case StyleEmbedding of
     seInline : ;
      seCSSFile :
        begin
        D:=ExtractFilePath(AFilename);
        if (CSSDir<>'') then
          D:=D+CSSDir+PathDelim;
        FContext.StyleContent.SaveToFile(D+ChangeFileExt(ExtractFileName(AFilename),'.css'));
        end;
      seStyleTag :
        begin;
        El:=FDoc.CreateElement('style');
        EL.AppendChild(FDOC.CreateTextNode(FContext.StyleContent.Text));
        FHeadElement.AppendChild(El);
        end
    end;
  if (hnoPageNoEdit in PageNavigator.Options) then
     begin
     Script:=TStringList.Create;
     try
       TPageNavigatorElementCreator.WriteDefaultScript(FContext,Script);
      if (Script.Count>0) then
        begin
        Sel:=FDoc.CreateElement('script');
        Sel['type']:='application/javascript';
        if (hoExternalJS in HTMLOptions) then
          begin
          F:=ChangeFileExt(AFileName,'.js');
          Script.SaveToFile(F);
          Sel['src']:=ExtractFileName(F);
          end
        else
          Sel.AppendChild(FDoc.CreateCDATASection(Script.Text));
        HeadElement.AppendChild(Sel);
        end;
     finally
       Script.Free;
     end;
     end;
end;

Function TFPReportExportfpImage.CreateImageElement(Const ImageFile: String) : THTMLElement;

begin
  Result:=Doc.CreateElement('img');
  Result['src']:=ExtractFileName(ImageFile);
end;

Function TFPReportExportfpImage.CreateHTMLFileForImage(Const ImageFile: String; APage : TFPReportPage; APageNo : Integer) : String;

Var
  PageDiv : THTMLElement;

begin
  Result:=ChangeFileExt(ImageFile,'.html');
  PageDiv:=SetupHTMLPageRender(APage);
  try
   if npTop in PageNavigator.Positions then
     CreatePageNavigator(npTop,BodyElement,aPage,aPageNo);
   if npLeft in PageNavigator.Positions then
     CreatePageNavigator(npLeft,BodyElement,aPage,aPageNo);
   BodyElement.AppendChild(PageDiv);
   PageDiv.AppendChild(CreateImageElement(ImageFile));
   if (npRight in PageNavigator.Positions) then
     CreatePageNavigator(npRight,BodyElement,aPage,aPageNo);
   if (npBottom in PageNavigator.Positions) then
     CreatePageNavigator(npBottom,BodyElement,aPage,aPageNo);
   FinishHTMLPage(Result);
   WriteHTMLFile(Doc,Result);
  finally
    FreeAndNil(FDoc);
  end;
end;

procedure TFPReportExportfpImage.GenerateHTML(ImageFiles: TStrings);

Var
  FN : String;
  HFN : TStrings;
  I : integer;

begin
  HFN:=Nil;
  FContext:=TGenerateHTMLContext.Create(Report,Nil,DPI);
  try
    FContext.SequenceFormat:=Self.SequenceFormat;
    FContext.SequenceDigits:=2;
    FContext.TotalPageCount:=ImageFiles.Count;
    Context.BaseFileName:=ChangeFileExt(Self.BaseFileName,'.html');
    HFN:=TStringList.Create;
    For I:=0 to ImageFiles.Count-1 do
      begin
      FContext.CurrentPageNo:=I;
      HFN.Add(CreateHTMLFileForImage(ImageFiles[i],TFPReportPage(ImageFiles.Objects[i]),I+1));
      end;
   if (hoTOCPage in HTMLOptions) then
     FN:=CreateTOCPage(HFN,(hoFramePage in HTMLOptions));
    If hoFramePage in HTMLOptions then
      CreateFramePage(HFN,FN);
  finally
    FreeAndNil(FContext);
    FreeAndNil(HFN);
  end;
end;

procedure TFPReportExportfpImage.SetFileName(const aFileName: String);
begin
  BaseFileName:=aFileName;
end;

procedure TFPReportExportfpImage.RenderElement(ABand : TFPReportCustomBand; Element : TFPReportElement);

Var
  C : TFPReportPoint;

begin
  if Element is TFPReportCustomMemo then
    RenderMemo(Aband,TFPReportCustomMemo(Element))
  else if Element is TFPReportCustomShape then
    RenderShape(ABand,TFPReportCustomShape(Element))
  else if Element is TFPReportCustomImage then
    RenderImage(Aband,TFPReportCustomImage(Element))
  else if Element is TFPReportCustomCheckbox then
    RenderCheckbox(ABand,TFPReportCustomCheckbox(Element))
  else
    begin
    C.Left := ABand.RTLayout.Left + Element.RTLayout.Left;
    C.Top := ABand.RTLayout.Top + Element.RTLayout.Top ; // + Element.RTLayout.Height;
    RenderFrame(ABand, Element.Frame, C, Element.RTLayout.Width, Element.RTLayout.Height);
    C.Left:=aband.RTLayout.Left;
    C.Top:=aband.RTLayout.Top;
    RenderUnknownElement(C,Element,Dpi);
    end;
end;

procedure TFPReportExportfpImage.SetBaseFileName(AValue: string);

{$IF NOT (FPC_FULLVERSION >= 30101)}
 function FindWriterFromExtension(extension: String): TFPCustomImageWriterClass;
 var
   s: string;
   r: integer;
 begin
   extension := lowercase (extension);
   if (extension <> '') and (extension[1] = '.') then
     system.delete (extension,1,1);
   with ImageHandlers do
   begin
     r := count-1;
     s := extension + ';';
     while (r >= 0) do
     begin
       Result := ImageWriter[TypeNames[r]];
       if (pos(s,{$if (FPC_FULLVERSION = 20604)}Extentions{$else}Extensions{$endif}[TypeNames[r]]+';') <> 0) then
         Exit;
       dec (r);
     end;
   end;
   Result := nil;
 end;
 {$ELSE}
 function FindWriterFromExtension(extension: String): TFPCustomImageWriterClass;
 begin
   Result:=TFPCustomImage.FindWriterFromExtension(extension);
 end;
 {$ENDIF}

Var
  Ext : String;

begin
  if FBaseFileName=AValue then Exit;
  Ext:=ExtractFileExt(AValue);
  FWriterClass:=FindWriterFromExtension(Ext);
  if (FWriterClass=Nil) then
     begin
     Delete(Ext,1,1);
     Raise EReportExportError.CreateFmt('Image format "%s" not supported.',[Ext]);
     end;
  FBaseFileName:=AValue;
end;

procedure TFPReportExportfpImage.SetCanvas(AValue: TFPCustomCanvas);
begin
  if FCanvas=AValue then Exit;
  FCanvas:=AValue;
  if Assigned(FHelper) then
    FHelper.Canvas:=aValue;
end;

procedure TFPReportExportfpImage.SetDPI(AValue: integer);
begin
  if FDPI=AValue then Exit;
  FDPI:=AValue;
  if Assigned(FHelper) then
    FHelper.DPI:=AValue;
end;

procedure TFPReportExportfpImage.SetFramePage(AValue: TFramePageOptions);
begin
  if FFramePage=AValue then Exit;
  FFramePage.Assign(AValue);
end;

procedure TFPReportExportfpImage.SetPageNavigator(AValue: TPageNavigatorOptions);
begin
  if FPageNavigator=AValue then Exit;
  FPageNavigator.Assign(AValue);
end;

procedure TFPReportExportfpImage.SetTOCPage(AValue: TTOCPageOptions);
begin
  if FTOCPage=AValue then Exit;
  FTOCPage.Assign(AValue);
end;

procedure TFPReportExportfpImage.RenderImage(aRect: TFPReportRect; var AImage: TFPCustomImage);

begin
  FHelper.RenderImage(aRect,aImage);
end;

procedure TFPReportExportfpImage.RenderBand(Aband: TFPReportCustomBand);

Var
  lPt1: TFPReportPoint;  // original Report point
  I : integer;

begin
  lPt1.Left := Aband.RTLayout.Left;
  lPt1.Top := Aband.RTLayout.Top;
  RenderFrame(Aband, Aband.Frame, lPt1, Aband.RTLayout.Width, Aband.RTLayout.Height);
  for I := 0 to Aband.ChildCount-1 do
    RenderElement(ABAnd,Aband.Child[i]);
end;

procedure TFPReportExportfpImage.DoExecute(const ARTObjects: TFPList);
var
  p, b: integer;
  rpage: TFPReportPage;
  ImageFiles : TStringList;
  D : String;
begin
//  HTMLOptions:=[hoEnabled,hoTOCPage,hoFramePage,hoExternalJS];
//  BaseFileName:='mydemo/image.png';
  D:=ExtractFilePath(BaseFileName);
  if (D<>'') and not DirectoryExists(D) then
    If not ForceDirectories(D) then
      Raise EReportError.CreateFmt('Cannot create output directory "%s"',[D]);
  if SequenceFormat='' then
    Sequenceformat:='%.2d';
  ImageFiles:=TStringList.Create;
  try
    for p := 0 to (ARTObjects.Count - 1) do { pages }
      begin
      rpage := TFPReportPage(ARTObjects[p]);
      SetupPageRender(rpage);
      for b := 0 to (rpage.BandCount - 1) do
        RenderBand(rpage.Bands[b]);
      ImageFiles.AddObject(BufferToFile(p+1),rPage);
      end;
    if hoEnabled in HTMLOptions then
      GenerateHTML(ImageFiles);
  finally
    FreeAndNil(ImageFiles)
  end;
end;

initialization
  TFPReportExportfpImage.RegisterExporter;
end.

