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
  HTML Export filter.

  FPImage is included as standard with FPC. This exporter uses those classes
  to generate image output. 

}
unit fpreporthtmlexport;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpReport,
  fpimage,
  fpcanvas,
  contnrs,
  FPImgCmn,
  dom,
  dom_html,
  HTMWrite,
  fpreportcanvashelper,
  fpreporthtmlutil,
  fpimgcanv;

type
  TStyleElement = (sePosition,seSize, seFrame, seBackGroundColor);
  TStyleElements = Set of TStyleElement;

Const
  seAll = [sePosition,seSize, seFrame, seBackGroundColor];

Type
  THTMLExportOption = (heoFixedPositioning,heoInlineImage,heoUseIMGtag,heoTOCPage,heoTOCPageFrame,heoMemoAsIs,heoExternalJS);
  THTMLExportOptions = set of THTMLExportOption;


  { TFixedOffset }

  TFixedOffset  = Class(TPersistent)
  private
    FLeft: Integer;
    FTop: Integer;
  Public
    Procedure Assign(Source : TPersistent) ; override;
  Published
    Property Left : Integer Read FLeft Write FLeft;
    Property Top : Integer Read FTop Write FTop;
  end;



  THTMLCanvasHelper = Class(TFPReportCanvasHelper)
  Public
    Procedure RenderShape(AImage : TFPCustomImage; S : TFPReportCustomShape); overload;
  end;

  { TFPReportExportHTML }

  TFPReportExportHTML = class(TFPReportExporter)
  private
    FBaseFileName: string;
    FCurrentBand: TFPReportCustomBand;
    FCurrentBandDIV: THTMLElement;
    FCurrentDIV: THTMLElement;
    FCurrentElement: TFPReportElement;
    FCurrentPage: TFPReportPage;
    FDoc: THTMLDocument;
    FFixedOffset: TFixedOffset;
    FFramePage: TFramePageOptions;
    FOptions: THTMLExportOptions;
    FPageNavigator: TPageNavigatorOptions;
    FPictureDir: String;
    FCSSDir : String;
    FHeadElement,
    FBodyElement,
    FTitleElement: THTMLElement;
    FSequenceDigits: Integer;
    FScript: TStrings;
    FStyleEmbedding: TStyleEmbedding;
    FDPI: integer;
    FSequenceFormat: string;
    FImageIDCount : Integer;
    FBasePageFileName : String;
    FStyleContent : Tstrings;
    FPageNames : TStrings;
    FReportImages : TFPStringHashTable;
    FTOCPage: TTOCPageOptions;
    FCanvasHelper : THTMLCanvasHelper;
    FCustomKeyCount: Integer;
    FContext : TGenerateHTMLContext;
    class function ColorToRGBString(AColor: TFPReportColor; UseHex: Boolean): String;
    function GetCurrentPageNo: Integer;
    function GetRunOffsetX: Integer;
    function GetRunOffsetY: Integer;
    function GetTotalPageCount: Integer;
    procedure SetFixedOffset(AValue: TFixedOffset);
    procedure SetFramePage(AValue: TFramePageOptions);
    procedure SetPageNavigator(AValue: TPageNavigatorOptions);
    procedure SetTocPage(AValue: TTOCPageOptions);
  protected
    function CreateContext(AReport: TFPCustomReport; ADoc: THTMLDocument): TGenerateHTMLContext;
    // General setup and entry point
    procedure DoExecute(const ARTObjects: TFPList); override;
    function InitSequence(ARTObjects: TFPList): Boolean; virtual;
    Procedure CreateDirs; virtual;
    function CreateTOCPageOptions: TTOCPageOptions; virtual;
    function CreateFramePageOptions: TFramePageOptions; virtual;
    function CreatePageNavigatorOptions: TPageNavigatorOptions; virtual;
    procedure AllocateHelpers; virtual;
    procedure ReleaseHelpers; virtual;
    // General HTML generation
    function FontNameToFontFamilyAndStyle(const AFontName: String; out aFamilyName, AStyle, aWeight: String): Boolean;
    function AllocateID(aElement: THTMLElement; AReportElement: TFPReportElement): String; virtual;
    function AllocatePageName(APageNo: Integer): string; virtual;
    procedure ElementToStyle(aDiv: THTMLElement; AElement: TFPReportElement; StyleElements: TStyleElements=seAll; ABand : TFPReportCustomBand = Nil; AExtra : String = ''); virtual;
    function CreateDiv(aParent: TDOMElement; AReportElement: TFPReportElement = Nil): THTMLElement;virtual;
    function CreateDiv(aParent: TDOMElement; AID : String): THTMLElement;virtual;
    procedure ApplyStyle(aElement: THTMLElement; const aStyle: String);virtual;
    procedure ApplyStyle(aElement: THTMLElement; const aStyle: TStrings);virtual;
    function LayoutToPosition(ALayout,AParentLayout: TFPReportLayout): String; virtual;
    function LayoutToSize(ALayout: TFPReportLayout): String; virtual;
    function ColorToStyle(aName: String; AColor: TFPReportColor): String;virtual;
    function CalcFontSize(aSizeInPoints: Integer): Integer; virtual;
    function mmToPixels(const AValue: TFPReportUnits): Integer; virtual;
    procedure WriteDefaultScript; virtual;
    // Image support
    function CreateScaledImg(Img: TFPCustomImage; ALayout: TFPReportLayout): TFPCustomImage; virtual;
    function CreateScaledImg(Img: TFPCustomImage; ARect: TFPReportRect): TFPCustomImage; virtual;
    function CreateScaledImg(Img: TFPCustomImage; W, H: Integer): TFPCustomImage;
    function AllocateImageFileName(AIndex: Integer): String;virtual;
    function CreateImageData(Img: TFPCustomImage; AIndex: integer=-1): String;virtual;
    procedure RenderImageFromData(aDiv: THTMLElement; aEl: TFPReportElement; const ABand: TFPReportCustomBand; const Data: String); virtual;
    // Frame page
    procedure CreateFramePage(PageNames: TStrings; const ATOCPageFileName: String); virtual;
    // TOC page
    function CreateTOCPage(APageNames: TStrings; ForFrame: Boolean): String; virtual;
    // Navigator
    Procedure CreatePageNavigator(aPosition : TNavigatorPosition; AParentDiV : THTMLElement); virtual;
    // Page generation
    procedure SetupPageVariables(aPage: TFPReportPage; APageNo: Integer); virtual;
    procedure CleanUpPageVariables; virtual;
    Function SetupPageRender(const APage: TFPReportPage) : THTMLElement; virtual;
    procedure RenderPage(aDiv: THTMLElement; APage: TFPReportPage); virtual;
    procedure PageToFile; virtual;
    // Various elements
    procedure RenderBand(Aband: TFPReportCustomBand); virtual;
    procedure RenderElement(ABand: TFPReportCustomBand; Element: TFPReportElement); virtual;
    procedure RenderMemo(const ABand: TFPReportCustomBand; const AMemo: TFPReportCustomMemo); virtual;
    procedure RenderShape(const ABand: TFPReportCustomBand; const AShape: TFPReportCustomShape); virtual;
    procedure RenderImage(const ABand: TFPReportCustomBand; const AImage: TFPReportCustomImage); virtual; overload;
    Procedure RenderImage(aPos : TFPReportRect; var AImage: TFPCustomImage) ; override; overload;
    procedure RenderCheckbox(const ABand: TFPReportCustomBand; const ACheckbox: TFPReportCustomCheckbox); virtual;
    // Some properties which remain constant during generation of 1 page.
    property RunOffsetX : Integer Read GetRunOffsetX;
    property RunOffsetY : Integer Read GetRunOffsetY;
    Property PageDoc : THTMLDocument Read FDoc;
    property PageHeadElement : THTMLElement Read FHeadElement;
    property PageBodyElement : THTMLElement Read FBodyElement;
    property PageTitleElement : THTMLElement Read FTitleElement;
    Property TotalPageCount : Integer Read GetTotalPageCount;
    Property CurrentPageNo : Integer Read GetCurrentPageNo;
    Property CurrentPage : TFPReportPage Read FCurrentPage;
    Property CurrentPageScript : TStrings Read FScript;
    Property CurrentPageFileName : String Read FBasePageFileName;
    Property SequenceDigits : Integer Read FSequenceDigits;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    Class Function Name : String; override;
    Class Function Description : String; override;
    Class Function DefaultExtension: String; override;
    Class Function MultiFile: Boolean; override;
    Procedure SetFileName(const aFileName: String); override;
    // Helper for color strings
    // Current element DIV
    Property CurrentDIV : THTMLElement Read FCurrentDIV;
    // Current band DIV
    Property CurrentBandDIV : THTMLElement Read FCurrentBandDIV;
    // CurrentElement. Set while RenderElement is called
    Property CurrentElement : TFPReportElement Read FCurrentElement;
    // CurrentBand. Set while RenderBand is called
    Property CurrentBand : TFPReportCustomBand Read FCurrentBand;
  published
    // Base filename. Can contain path.
    property BaseFileName : string read FBaseFileName write FBaseFileName;
    // Frame Page options.
    property FramePage : TFramePageOptions Read FFramePage Write SetFramePage;
    // TOC Page options.
    property TOCPage : TTOCPageOptions Read FTOCPage Write SetTocPage;
    // Relative to path of base filename.
    Property PictureDir : String Read FPictureDir Write FPictureDir;
    // Relative to path of base filename.
    Property CSSDir : String Read FCSSDir Write FCSSDir;
    // Format for formatting file numbers.
    property SequenceFormat : string read FSequenceFormat write FSequenceFormat;
    // DPI to use when converting to pixels
    property DPI: integer read FDPI write FDPI;
    // How to embed CSS
    property StyleEmbedding : TStyleEmbedding Read FStyleEmbedding Write FStyleEmbedding;
    // Various miscellaneous options
    Property Options : THTMLExportOptions Read FOptions Write FOptions;
    // Offset to be used when using fixed positioning.
    Property FixedOffset : TFixedOffset Read FFixedOffset Write SetFixedOffset;
    // Page navigator options
    Property PageNavigator : TPageNavigatorOptions Read FPageNavigator Write SetPageNavigator;
  end;

implementation

uses
  // ftFont,
  fpTTF,
  fpparsettf,
  base64,
  fpwritepng;

//  RGBA_Width = 4;

type

  { for access to Protected methods }
  TReportImageFriend = class(TFPReportCustomImage);
  TReportFriend = class(TFPCustomReport);


  { THTMLExportfpImage }



{ THTMLExportfpImage }

procedure THTMLCanvasHelper.RenderShape(AImage: TFPCustomImage; S: TFPReportCustomShape);

Var
  L : TFPReportPoint;

begin
  Canvas:=TFPImageCanvas.Create(AImage);
  try
    L.Top:=0;
    L.Left:=0;
    Canvas.Brush.Style:=bsSolid;
    Canvas.Brush.FPColor:=colWhite;// colTransparent;
    Canvas.Clear;
    RenderShape(L,S);
  finally
    Canvas.Free;
    Canvas:=Nil;
  end;
end;

{ TFixedOffset }

procedure TFixedOffset.Assign(Source: TPersistent);
Var
  O : TFixedOffset;

begin
  if (Source is TFixedOffset) then
    begin
    O:=Source as TFixedOffset;
    Top:=O.Top;
    Left:=O.Left;
    end;
  inherited Assign(Source);
end;


{ TFPReportExportHTML }

{ ---------------------------------------------------------------------
  General routines
  ---------------------------------------------------------------------}
class function TFPReportExportHTML.Name: String;
begin
  Result:='HTML';
end;

class function TFPReportExportHTML.Description: String;
begin
  Result:='HTML files export';
end;

class function TFPReportExportHTML.DefaultExtension: String;
begin
  Result:='.html';
end;

class function TFPReportExportHTML.MultiFile: Boolean;
begin
  Result:=True;
end;

procedure TFPReportExportHTML.SetFileName(const aFileName: String);
begin
  BaseFileName:=aFileName;
end;

constructor TFPReportExportHTML.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BaseFileName:=ApplicationName + '-.html';
  SequenceFormat := '%.2d';
  FDPI := 96;
  FFramePage:=CreateFramePageOptions;
  FTOCPage:=CreateTOCPageOptions;
  FFixedOffset:=TFixedOffset.Create;
  FPageNavigator:=CreatePageNavigatorOptions;
end;

destructor TFPReportExportHTML.Destroy;

begin
  ReleaseHelpers; // To be sure
  FreeAndNil(FContext);
  FreeAndNil(FFixedOffset);
  FreeAndNil(FTOCPage);
  FreeAndNil(FFramePage);
  FreeAndNil(FPageNavigator);
  FreeAndNil(FStyleContent);
  inherited Destroy;
end;

procedure TFPReportExportHTML.SetFramePage(AValue: TFramePageOptions);
begin
  if FFramePage=AValue then Exit;
  FFramePage.Assign(AValue);
end;

procedure TFPReportExportHTML.SetPageNavigator(AValue: TPageNavigatorOptions);
begin
  if FPageNavigator=AValue then Exit;
  FPageNavigator.Assign(AValue);
end;

procedure TFPReportExportHTML.SetFixedOffset(AValue: TFixedOffset);
begin
  if FFixedOffset=AValue then Exit;
  FFixedOffset.Assign(AValue);
end;

function TFPReportExportHTML.GetRunOffsetX: Integer;
begin
  Result:=FContext.RunOffsetX;
end;

function TFPReportExportHTML.GetRunOffsetY: Integer;
begin
  Result:=FContext.RunOffsetY;
end;

function TFPReportExportHTML.GetTotalPageCount: Integer;
begin
  Result:=FContext.TotalPageCount;
end;

procedure TFPReportExportHTML.SetTocPage(AValue: TTOCPageOptions);
begin
  if FTocPage=AValue then Exit;
  FTocPage.Assign(AValue);
end;

function TFPReportExportHTML.CreateContext(AReport : TFPCustomReport; ADoc : THTMLDocument): TGenerateHTMLContext;
begin
  Result:=TGenerateHTMLContext.Create(AReport,ADoc,DPI);
  Result.CSSDir:=CSSDir;
  Result.BaseFileName:=BaseFileName;
  Result.SequenceFormat:=SequenceFormat;
end;

function TFPReportExportHTML.CreateTOCPageOptions: TTOCPageOptions;
begin
  Result:=TTOCPageOptions.Create;
end;

function TFPReportExportHTML.CreateFramePageOptions: TFramePageOptions;

begin
  Result:=TFramePageOptions.Create;
  Result.TOCZoneSize:=10;
end;

function TFPReportExportHTML.CreatePageNavigatorOptions: TPageNavigatorOptions;
begin
  Result:=TPageNavigatorOptions.Create;
  Result.Positions:=[npTop];
  Result.Options:=[hnoFirstLast,hnoPageNo];
  Result.FixedWidth:=80;
  Result.FixedHeight:=32;
  Result.FixedMargin:=8;
end;

Function TFPReportExportHTML.InitSequence(ARTObjects : TFPList) : Boolean;

Var
  P,D,MC : Integer;

begin
  Result:=(SequenceFormat='');
  // Determine number of zeroes in filenames

  if Result then
    begin
    if TReportFriend(Report).Images.Count>ARTObjects.Count then
      MC:=TReportFriend(Report).Images.Count
    else
      MC:=ARTObjects.Count;
    D:=1;
    While MC>10 do
      begin
      Inc(D);
      MC:=MC div 10;
      end;
    FSequenceDigits:=D;
    SequenceFormat:='%.'+IntToStr(D)+'d';
    end
  else
    begin
    P:=Pos('%.',SequenceFormat);
    if P=0 then
      FSequenceDigits:=0
    else
      FSequenceDigits:=StrToIntDef(Copy(SequenceFormat,P+2,1),0);
    end;
end;

procedure TFPReportExportHTML.DoExecute(const ARTObjects: TFPList);
var
  X,Y,P,B : integer;
  rpage: TFPReportPage;
  PageDiv : THTMLElement;
  FN : String;
  SequenceEmpty,BaseEmpty : Boolean;

begin
//  Options:=Options+[heoExternalJS,heoTOCPageFrame];
//  PageNavigator.Options:=PageNavigator.Options+[hnoPageNoEdit,hnoUsePageNOfM];
//  BaseFileName:='shapedemos/report.html';
  // Check base name
  BaseEmpty:=BaseFileName='';
  if BaseEmpty then
    BaseFileName:=ApplicationName+'-.html';
  SequenceEmpty:=InitSequence(ARTObjects);
  FreeAndNil(FContext);
  FContext:=CreateContext(Report,FDoc);
  X:=FixedOffset.Left;
  if (npLeft in PageNavigator.Positions) then
    X:=X+PageNavigator.FixedWidth+PageNavigator.FixedMargin;
  Y:=FixedOffset.Top;
  if (npTop in PageNavigator.Positions) then
    Y:=Y+PageNavigator.FixedHeight+PageNavigator.FixedMargin;
  FContext.InitOffsets(X,Y);
  FCustomKeyCount:=0;
  try
    CreateDirs;
    AllocateHelpers;
    FContext.TotalPageCount:=ARTObjects.Count;
    for p := 0 to (ARTObjects.Count - 1) do { pages }
      begin
      rpage := TFPReportPage(ARTObjects[p]);
      SetUpPageVariables(rPage,P+1);
      try
        PageDiv:=SetupPageRender(rpage);
        if npTop in PageNavigator.Positions then
          CreatePageNavigator(npTop,PageBodyElement);
        if npLeft in PageNavigator.Positions then
          CreatePageNavigator(npLeft,PageBodyElement);
        PageBodyElement.AppendChild(PageDiv);
        RenderPage(PageDiv,rPage);
        for b := 0 to (rpage.BandCount - 1) do
          begin
          FCurrentBandDIV:=CreateDiv(PageDiv,rpage.Bands[b]);
          RenderBand(rpage.Bands[b]);
          end;
        if (npRight in PageNavigator.Positions) then
          CreatePageNavigator(npRight,PageBodyElement);
        if (npBottom in PageNavigator.Positions) then
          CreatePageNavigator(npBottom,PageBodyElement);
        PageToFile;
      finally
        CleanUpPageVariables;
      end;
      end;
    if (([heoTOCPage,heoTOCPageFrame] * Options)<>[]) then
      FN:=CreateTOCPage(FPageNames,(heoTOCPageFrame in Options));
    if (heoTOCPageFrame in Options) then
      CreateFramePage(FPageNames,FN);
  finally
    ReleaseHelpers;
    if BaseEmpty then
      BaseFileName:='';
    if SequenceEmpty then
      SequenceFormat:='';
  end;
end;
procedure TFPReportExportHTML.CreateDirs;

Var
  BD,D : String;

begin
  BD:=ExtractFilePath(FBaseFileName);
  if (BD<>'') then
    If not ForceDirectories(BD) then
      Raise EReportError.CreateFmt('Cannot create output directory "%s"',[BD]);
  if (PictureDir<>'') then
    begin
    D:=BD+PictureDir;
    If not ForceDirectories(D) then
      Raise EReportError.CreateFmt('Cannot create picture output directory "%s"',[D]);
    end;
  if (CSSDir<>'') then
    begin
    D:=BD+CSSDir;
    If not ForceDirectories(D) then
      Raise EReportError.CreateFmt('Cannot create picture output directory "%s"',[BD]);
    end;
end;


procedure TFPReportExportHTML.ReleaseHelpers;

begin
  FreeAndNil(FCanvasHelper);
  FreeAndNil(FReportImages);
  FreeAndNil(FPageNames);
end;

procedure TFPReportExportHTML.AllocateHelpers;

begin
  FPageNames:=TStringList.Create;
  FReportImages:=TFPStringHashTable.Create;
  FCanvasHelper:=THTMLCanvasHelper.Create(Nil,96);
  FImageIDCount:=0;
end;


{ ---------------------------------------------------------------------
  HTML Generation
  ---------------------------------------------------------------------}

function TFPReportExportHTML.mmToPixels(const AValue: TFPReportUnits): Integer;
begin
  Result := FContext.mmToPixels(aValue); // Round(AValue * (DPI / cInchToMM));
end;

function TFPReportExportHTML.AllocatePageName(APageNo: Integer): string;

begin
  Result:=FContext.AllocatePageName(APageNo);
end;

function TFPReportExportHTML.AllocateID(aElement: THTMLElement;AReportElement: TFPReportElement) : String;

begin
  Result:=FContext.AllocateID(aElement,aReportElement);
end;

function TFPReportExportHTML.CreateDiv(aParent: TDOMElement;
  AReportElement: TFPReportElement): THTMLElement;

begin
  Result:=FContext.CreateDiv(AParent,AReportElement);
end;

function TFPReportExportHTML.CreateDiv(aParent: TDOMElement; AID: String): THTMLElement;
begin
  Result:=FContext.CreateDiv(AParent,AID);
end;

function TFPReportExportHTML.LayoutToPosition(ALayout,AParentLayout: TFPReportLayout): String;


Var
  P : String;
  RL,RT : TFPReportUnits;
  L,T : Integer;

begin
  RL:=ALayout.Left;
  RT:=ALayout.Top;
  if Assigned(AParentLayout) and (heoFixedPositioning in Options) then
    begin
    RL:=RL+AParentLayout.Left;
    RT:=RT+AParentLayout.Top;
    end;
  L:=mmToPixels(RL);
  T:=mmToPixels(RT);
  if (heoFixedPositioning in Options) then
    begin
    L:=L+RunOffsetX;
    T:=T+RunOffsetY;
    end;
  P:=PosStrings[heoFixedPositioning in Options];
  Result:=Format('position: %s; left: %dpx; top: %dpx;',[P,L,T]);
end;

function TFPReportExportHTML.LayoutToSize(ALayout: TFPReportLayout): String;

begin
  Result:=Format('width: %dpx; height: %dpx;',[mmToPixels(ALayout.Width),mmToPixels(ALayout.Height)]);
end;


function TFPReportExportHTML.ColorToStyle(aName: String; AColor: TFPReportColor
  ): String;


begin
  Result:=Format('%s: %s;',[aName,ColorToRGBString(aColor,False)]);
end;

class function TFPReportExportHTML.ColorToRGBString(AColor: TFPReportColor;
  UseHex: Boolean): String;

begin
  Result:=TGenerateHTMLContext.ColorToRGBString(aColor,useHex);
end;

function TFPReportExportHTML.GetCurrentPageNo: Integer;
begin
  Result:=FContext.CurrentPageNo;
end;

function TFPReportExportHTML.CalcFontSize(aSizeInPoints: Integer): Integer;

begin
//  Result:=aSizeInPoints;
  Result:=Round(aSizeInPoints/72*DPI);
end;

procedure TFPReportExportHTML.ElementToStyle(aDiv: THTMLElement;
  AElement: TFPReportElement; StyleElements: TStyleElements;
  ABand: TFPReportCustomBand; AExtra: String);

Const
  FNames : Array[TFPReportFrameLine] of string = ('top', 'bottom', 'left', 'right');
  FStyles : Array[TFPPenStyle] of string = ('solid', 'dashed', 'dotted', 'dashed', 'dashed', 'inset', 'none','hidden');
Var
  Style : TStrings;
  L : TFPreportLayout;
  FL : TFPReportFrameLine;

begin
  Style:=TStringList.Create;
  try
    if (seSize in StyleElements) then
      Style.Add(LayoutToSize(AElement.RTLayout));
    if (sePosition in StyleElements) then
      begin
      if Assigned(ABand) then
        L:=ABand.RTLayout
      else
        L:=Nil;
      Style.Add(LayoutToPosition(AElement.RTLayout,L));
      end;
    if (seBackGroundColor in StyleElements) then
      Style.Add(ColorToStyle('background-color',AElement.Frame.BackgroundColor));
    if (seFrame in StyleElements) and (AElement.Frame.Color<>clNone) then
      begin
      for FL in TFPReportFrameLine do
        if (AElement.Frame.Shape=fsRectangle) or (FL in AElement.Frame.Lines) then
          begin
          Style.Add(ColorToStyle(Format('border-%s-color',[FNames[FL]]),AElement.Frame.Color));
          Style.Add('border-%s-width: %dpx;',[FNames[FL],AElement.Frame.Width]);
          Style.Add('border-%s-style: %s;',[FNames[FL],FStyles[AElement.Frame.Pen]]);
          end;
      end;
    if AExtra<>'' then
      Style.Add(AExtra);
    ApplyStyle(aDiv,Style);
  finally
    STyle.Free;
  end;
end;

procedure TFPReportExportHTML.ApplyStyle(aElement: THTMLElement;
  const aStyle: TStrings);

begin
  FContext.ApplyStyle(aElement,aStyle);
end;

procedure TFPReportExportHTML.ApplyStyle(aElement: THTMLElement;
  const aStyle: String);

begin
  FContext.ApplyStyle(aElement,aStyle);
end;

{ ---------------------------------------------------------------------
  Page generation
  ---------------------------------------------------------------------}

procedure TFPReportExportHTML.RenderPage(aDiv : THTMLElement; APage : TFPReportPage);

Var
  P,S: String;

begin
  // Div with edge of the page.
  P:=PosStrings[heoFixedPositioning in Options];
  S:=Format('position: %s; ',[P]);
  S:=S+'border-color: #000000; border-style: ridge; border-width: 1px; ';
  S:=S+Format('top: %dpx; left: %dpx; width: %dpx; height: %dpx; ',[RunOffsetY,RunOffsetX,mmToPixels(APage.PageSize.Width),mmToPixels(APage.PageSize.Height)]);
  ApplyStyle(aDiv,S);
end;


procedure TFPReportExportHTML.SetupPageVariables(aPage: TFPReportPage;
  APageNo: Integer);
begin
  FCurrentPage:=aPage;
  FBasePageFileName:=AllocatePageName(aPageNo);
  FPageNames.Add(FBasePageFileName);
  FDoc := THTMLDocument.Create;
  FScript:=TStringList.Create;
  if (StyleEmbedding<>seInline) then
    begin
    FStyleContent:=TStringList.Create;
    FContext.StyleContent:=FStyleContent
    end
  else
    FContext.StyleContent:=Nil;
  FContext.CurrentPageNo:=APageNo;
end;

procedure TFPReportExportHTML.CleanUpPageVariables;

begin
  FreeAndNil(FDoc);
  FreeAndNil(FStyleContent);
  FreeAndNil(FScript);
  FCurrentPage:=Nil;
  FContext.CurrentPageNo:=-1;
  FBasePageFileName:='';
end;

function TFPReportExportHTML.SetupPageRender(const APage: TFPReportPage): THTMLElement;

Var
  HTMLEl,El : TDomElement;
  D : String;

begin
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
      El['href']:=D+ChangeFileExt(ExtractFileName(FBasePageFileName),'.css');
      FHeadElement.AppendChild(El);
      end
  end;
  FTitleElement := FDoc.CreateElement('title');
  FHeadElement.AppendChild(FTitleElement);
  FBodyElement := FDoc.CreateElement('body');
  HTMLEl.AppendChild(FBodyElement);
  Result:=CreateDiv(Nil,aPage);
end;

procedure TFPReportExportHTML.WriteDefaultScript;

begin
  TPageNavigatorElementCreator.WriteDefaultScript(FContext,FScript);
end;

procedure TFPReportExportHTML.PageToFile;

Var
  Sel,El : THTMLElement;
  F,D : String;

begin
  Case StyleEmbedding of
    seInline : ;
    seCSSFile :
      begin
      D:=ExtractFilePath(FBasePageFileName);
      if (CSSDir<>'') then
        D:=D+CSSDir+PathDelim; 
      FStyleContent.SaveToFile(D+ChangeFileExt(ExtractFileName(FBasePageFileName),'.css'));
      end;
    seStyleTag :
      begin;
      El:=FDoc.CreateElement('style');
      EL.AppendChild(FDOC.CreateTextNode(FStyleContent.Text));
      FHeadElement.AppendChild(El);
      end
  end;
  if (hnoPageNoEdit in PageNavigator.Options) then
    WriteDefaultScript;
  if Assigned(FScript) and (FScript.Count>0) then
    begin
    Sel:=FDoc.CreateElement('script');
    Sel['type']:='application/javascript';
    if (heoExternalJS in Options) then
      begin
      F:=ChangeFileExt(CurrentPageFileName,'.js');
      FScript.SaveToFile(F);
      Sel['src']:=ExtractFileName(F);
      end
    else
      Sel.AppendChild(FDoc.CreateCDATASection(FScript.Text));
    FHeadElement.AppendChild(Sel);
    end;
  WriteHTMLFile(FDoc,FBasePageFileName);
end;

procedure TFPReportExportHTML.CreatePageNavigator(aPosition: TNavigatorPosition; AParentDiV: THTMLElement);

Var
  PNEC : TPageNavigatorElementCreator;

begin
  PNEC:=TPageNavigatorElementCreator.Create(FContext,CurrentPage,PageNavigator);
  try
    PNec.CreatePageNavigator(aPosition,aParentDiv);
  finally
    PNEC.Free;
  end;
end;


{ ---------------------------------------------------------------------
  Image support
  ---------------------------------------------------------------------}

function TFPReportExportHTML.AllocateImageFileName(AIndex : Integer) : String;


begin
  if (AIndex<>-1) then
    begin
    Result:=Format(SequenceFormat,[AIndex])+'.png';
    end
  else
    begin
    Inc(FImageIDCount);
    Result:='extra-'+Format(SequenceFormat,[FImageIDCount])+'.png';
    end;
  Result:=ChangeFileExt(ExtractFileName(FBaseFileName),Result);
  if (PictureDir<>'') then
    Result:=ExtractFilePath(Result)+StringReplace(PictureDir,'\','/',[rfReplaceAll])+'/'+ExtractFileName(Result);
end;

function TFPReportExportHTML.CreateImageData(Img: TFPCustomImage; AIndex : integer = -1): String;

Var
  P : TFPWriterPNG;
  S : TStringStream;
  B : TBase64EncodingStream;
  F : TFileStream;

begin
  S:=Nil;
  F:=Nil;
  B:=Nil;
  P:=TFPWriterPNG.Create;
  try
  //  P.UseAlpha:=True;
    if heoInlineImage in options then
      begin
      S:=TStringStream.Create('');
      B:=TBase64EncodingStream.Create(S);
      P.ImageWrite(B,Img);
      B.Flush;
      Result:='data:image/png;base64,'+S.DataString;
      end
    else
      begin
      Result:=AllocateImageFileName(AIndex);
      F:=TFileStream.Create(ExtractFilePath(FBaseFileName)+Result,fmCreate);
      P.ImageWrite(F,Img);
      end;
  finally
    F.Free;
    P.Free;
    S.Free;
    B.free;
  end
end;

procedure TFPReportExportHTML.RenderImageFromData(aDiv : THTMLElement; aEl : TFPReportElement; const ABand: TFPReportCustomBand; const Data : String);

Var
  imgEl: THTMLElement;

begin
  if (Data='') then
    begin
    ElementToStyle(aDiv,AEl,seAll,ABand);
    Exit; { nothing further to do }
    end;
  if heoUseIMGtag in Options then
    begin
    imgEl:=FDoc.CreateElement('img');
    adiv.AppendChild(imgEl);
    imgEl['width']:=Format('%dpx',[mmToPixels(aEl.RTLayout.Width)]);
    imgEl['height']:=Format('%dpx',[mmToPixels(aEl.RTLayout.Height)]);
    imgEl['src']:=Data
    end
  else
    ElementToStyle(aDiv,AEl,seAll,ABand,'background-image: url('+Data+'); background-repeat: no-repeat; background-position: left top; background-size: contain;');
end;

function TFPReportExportHTML.CreateScaledImg(Img: TFPCustomImage; W,H : Integer): TFPCustomImage;

Var
  Canv : TFPImageCanvas;

begin
  if (H=Img.Height) and (W=Img.Width) then
    Result:=Img
  else
    begin
    Result:=TFPCompactImgRGBA8Bit.Create(W,H);
    Canv:=TFPImageCanvas.Create(Result);
    try
      Canv.StretchDraw(0,0,w,h,Img);
    finally
      Canv.Free;
    end;
    end;
end;

function TFPReportExportHTML.CreateScaledImg(Img: TFPCustomImage;
  ALayout: TFPReportLayout): TFPCustomImage;

Var
  W,H : Integer;

begin
  H:=mmToPixels(ALayout.Height);
  W:=mmToPixels(ALayout.Width);
  Result:=CreateScaledImg(Img,W,H);
end;

function TFPReportExportHTML.CreateScaledImg(Img: TFPCustomImage; ARect: TFPReportRect): TFPCustomImage;
Var
  W,H : Integer;

begin
  H:=mmToPixels(ARect.Height);
  W:=mmToPixels(ARect.Width);
  Result:=CreateScaledImg(Img,W,H);
end;

{ ---------------------------------------------------------------------
  Render elements
  ---------------------------------------------------------------------}
procedure TFPReportExportHTML.RenderBand(Aband: TFPReportCustomBand);

Var
  I : integer;

begin
  ElementToStyle(CurrentBandDiv,aBand);
  try
    for I := 0 to Aband.ChildCount-1 do
      begin
      FCurrentElement:=Aband.Child[i];
      FCurrentDIV:=CreateDiv(CurrentBandDiv,FCurrentElement);
      CurrentBandDiv.AppendChild(FCurrentDIV);
      RenderElement(ABand,FCurrentElement);
      end;
  finally
    FCurrentDiv:=Nil;
    FCurrentElement:=nil;
  end;
end;

procedure TFPReportExportHTML.RenderElement(ABand : TFPReportCustomBand; Element : TFPReportElement);

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
    // C.Left := ABand.RTLayout.Left + Element.RTLayout.Left;
    // C.Top := ABand.RTLayout.Top + Element.RTLayout.Top ; // + Element.RTLayout.Height;
    // (ABand, Element.Frame, C, Element.RTLayout.Width, Element.RTLayout.Height);
    C.Left:=aband.RTLayout.Left;
    C.Top:=aband.RTLayout.Top;
    RenderUnknownElement(C,Element,Dpi);
    end;
end;

function TFPReportExportHTML.FontNameToFontFamilyAndStyle(
  const AFontName: String; out aFamilyName, AStyle, aWeight: String): Boolean;

Var
  CI : TFPFontCacheItem;

begin
  CI:=gTTFontCache.Find(AFontName);
  Result:=Assigned(CI);
  if Result then
    begin
    aFamilyName:=CI.FamilyName;
    if CI.IsBold then
      AWeight:='bold';
    if CI.IsItalic then
      AStyle:='italic';
    end
  else
    begin
    aFamilyName:=AFontName;
    aStyle:='';
    aWeight:='';
    end
end;

procedure TFPReportExportHTML.RenderMemo(const ABand: TFPReportCustomBand; const AMemo: TFPReportCustomMemo);

Const
  SAligns : Array[TFPReportHorzTextAlignment] of string
          =  ('left', 'right', 'center', 'justify');

var
  lPt1: TFPReportPoint;  // original Report point
  lMemo: TFPReportMemo;
  i,xOffset,yOffset: integer;
  txtblk: TFPTextBlock;
  BS,S,aFamily,aStyle,aWeight : String;
  bDiv,span : THTMLElement;
  FixedPos : Boolean;

begin
  lMemo := TFPReportMemo(AMemo);
  FixedPos:=heoFixedPositioning in Options;
  { Store the Top-Left coordinate of the Memo. We will be reusing this info. }
  if FixedPos then
    begin
    lPt1.Left := ABand.RTLayout.Left + AMemo.RTLayout.Left;
    lPt1.Top := ABand.RTLayout.Top + AMemo.RTLayout.Top;
    XOffSet:=RunOffsetX;
    YOffset:=RunOffsetY;
    end
  else
    begin
    lPt1.Left := 0;
    lPt1.Top := 0;
    XOffset:=0;
    YOffset:=0;
    end;
  BS:=Format('position: %s; ',[PosStrings[FixedPos]]);
  BS:=BS+Format(' font-size: %dpx;',[CalcFontSize(lMemo.Font.Size)]);
  // BS:=BS+' overflow: hidden;';
  { Frame must be drawn before the text as it could have a fill color. }
//  Writeln('Memo : ',lMemo.Text);
  if (heoMemoAsIs in Options) then
    begin
    S:=Format('position: %s; ',[PosStrings[FixedPos]]);
    S:=S+Format(' font-size: %dpx;',[CalcFontSize(lMemo.Font.Size)]);
    if FontNameToFontFamilyAndStyle(lMemo.Font.Name,aFamily,aStyle,AWeight) then
      begin
      S:=S+Format(' font-family: "%s";',[aFamily]);
      if (aStyle<>'') then
        S:=S+Format(' font-style: %s;',[aStyle]);
      if (aWeight<>'') then
        S:=S+Format(' font-weight: %s;',[aWeight]);
      end
    else // Hope for the best...
      S:=S+Format(' font-family: "%s";',[lMemo.Font.Name]);
// TODO: Vertical
// TODO: LineSpacing;
    S:=S+Format('text-align: %s;',[SAligns[lMemo.TextAlignment.Horizontal]]);
    S:=S+Format('padding: %dpx %dpx %dpx %dpx;',[
                 mmToPixels(lMemo.TextAlignment.TopMargin),
                 mmToPixels(lMemo.TextAlignment.RightMargin),
                 mmToPixels(lMemo.TextAlignment.BottomMargin),
                 mmToPixels(lMemo.TextAlignment.LeftMargin)
    ]);

    ElementToStyle(CurrentDiv,AMemo,seAll-[seBackGroundColor],ABand,S);
    span:=FDoc.CreateSpanElement;
    CurrentDiv.appendChild(FDoc.CreateTextNode(lMemo.Text));
    end
  else
    begin
    ElementToStyle(CurrentDiv,AMemo,seAll-[seBackGroundColor],ABand);
    { render the TextBlocks as-is. }
    for i := 0 to lMemo.TextBlockList.Count-1 do
      begin
      txtblk := lMemo.TextBlockList[i];
      bDiv:=CreateDiv(CurrentDiv,lMemo);
      S:=BS+Format('top: %.dpx; left: %.dpx;',[YoffSet+mmToPixels(lPt1.Top+txtblk.Pos.Top-txtblk.Descender),XOffset+mmToPixels(lPt1.Left+txtblk.Pos.Left)]);
      S:=S+Format('width: %.dpx; height: %.dpx;',[mmToPixels(txtblk.Width),mmToPixels(txtblk.Height+txtblk.Descender)]);
      if FontNameToFontFamilyAndStyle(txtblk.FontName,aFamily,aStyle,AWeight) then
        begin
        S:=S+Format(' font-family: "%s";',[aFamily]);
        if (aStyle<>'') then
          S:=S+Format(' font-style: %s;',[aStyle]);
        if (aWeight<>'') then
          S:=S+Format(' font-weight: %s;',[aWeight]);
        end
      else // Hope for the best...
        S:=S+Format(' font-family: "%s";',[txtblk.FontName]);
      S:=S+ColorToStyle('color',TxtBlk.FGColor);
      ApplyStyle(bDiv,S);
      span:=FDoc.CreateSpanElement;
      span.appendChild(FDoc.CreateTextNode(txtBlk.Text));
      bDiv.AppendChild(span);
      end;
    end;
end;


procedure TFPReportExportHTML.RenderShape(const ABand: TFPReportCustomBand; const AShape: TFPReportCustomShape);

var
  key,data : string;
  H,W : Integer;
  Img : TFPCustomImage;

begin
  { Frame must be drawn before the text as it could have a fill color. }
  Key:=AShape.CreatePropertyHash;
  Data:=FReportImages.Items[Key];
  if (Data='') then
    begin
    H:=mmToPixels(AShape.RTLayout.Height)+1;
    W:=mmToPixels(AShape.RTLayout.Width)+1;
    Img:=TFPMemoryImage.Create(W,H);
    try
      FCanvasHelper.RenderShape(Img,AShape);
      Data:=CreateImageData(Img,-1);
    finally
      Img.Free;
    end;
    FReportImages.Add(Key,Data);
    end;
  RenderImageFromData(CurrentDiv,AShape,ABand,Data)
end;

procedure TFPReportExportHTML.RenderImage(const ABand: TFPReportCustomBand; const AImage: TFPReportCustomImage);
var
  img: TReportImageFriend;
  I,IID : Integer;
  Key,Data : string;

begin
  img := TReportImageFriend(AImage);  { for access to Protected methods }
  // Determine image data
  if Assigned(img.Image) then
    begin
    IID:=img.ImageID;
    if (IID>=0) then
      begin
      I:=TReportFriend(Report).images.GetIndexFromID(IID);
      Key:=Format('img%d',[i]);
      Data:=FReportImages.Items[Key];
      if (Data='') then
        begin
        Data:=CreateImageData(TReportFriend(Report).Images[i].Image,I);
        FReportImages.Add(Key,Data);
        end;
      end;
    end;
  RenderImageFromData(CurrentDiv,AImage,ABand,Data)
end;

procedure TFPReportExportHTML.RenderImage(aPos: TFPReportRect;
  var AImage: TFPCustomImage);

var
  Data : String;
  ScaledImg : TFPCustomImage;

begin
  ScaledImg:=CreateScaledImg(aImage,aPos);
  try
    Data:=CreateImageData(ScaledImg,-1);
  Finally
    If (ScaledImg<>AImage) then
      FreeAndNil(ScaledImg);
  end;
  Inc(FCustomKeyCount);
  FReportImages.Add('CustomImage'+IntToStr(FCustomKeyCount),Data);
  RenderImageFromData(CurrentDiv,CurrentElement,CurrentBand,Data);
end;


procedure TFPReportExportHTML.RenderCheckbox(const ABand: TFPReportCustomBand; const ACheckbox: TFPReportCustomCheckbox);

var
  Data,Key : String;
  ScaledImg,OrigImg : TFPCustomImage;

begin
  Key:=ACheckBox.CreatePropertyHash;
  Data:=FReportImages.Items[Key];
  if (Data='') then
    begin
    OrigImg:=ACheckBox.GetRTImage;
    if Assigned(OrigImg) then
      begin
      ScaledImg:=CreateScaledImg(OrigImg,ACheckBox.RTLayout);
      try
        Data:=CreateImageData(ScaledImg,-1);
      Finally
        If (ScaledImg<>OrigImg) then
          FreeAndNil(ScaledImg);
      end;
      end;
    FReportImages.Add(Key,Data);
    end;
  RenderImageFromData(CurrentDiv,ACheckBox,ABand,Data)
end;

{ ---------------------------------------------------------------------
  TOC frame
  ---------------------------------------------------------------------}


function TFPReportExportHTML.CreateTOCPage(APageNames: TStrings; ForFrame: Boolean): String;

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

{ ---------------------------------------------------------------------
  Frame page
  ---------------------------------------------------------------------}

procedure TFPReportExportHTML.CreateFramePage(PageNames: TStrings; const ATOCPageFileName: String);

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



initialization
  TFPReportExportHTML.RegisterExporter;
end.

