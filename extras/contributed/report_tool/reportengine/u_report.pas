{
    << Impressions >>  U_Imprime.pas

    Copyright (C) 2010 - JM.Levecque - <jmarc.levecque@jmlesite.fr>

   This library is a free software coming as a add-on to fpGUI toolkit
   See the copyright included in the fpGUI distribution for details about redistribution

   This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit interfaces with the user program
}

unit U_Report;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  fpg_base, fpg_main,
  fpg_panel, fpg_dialogs, fpg_imgfmt_bmp, fpg_imgfmt_jpg,
  U_Command, U_Pdf;

type
  TPaperType= (A4,Letter,Legal,Executive,Comm10,Monarch,DL,C5,B5);
  TOrient= (oPortrait,oLandscape);
  TMeasureUnit = (msMM,msInch);
  TPreparation= (ppPrepare,ppVisualize,ppPdfFile);

  T_Report = class(TObject)
    private
      OldSeparator: Char;
      FVersion: Char;
      FPaper: TPaper;
      FPaperType: TPaperType;
      FOrientation: TOrient;
      FCurrentMargin: TDimensions;
      FMeasureUnit: TMeasureUnit;
      FPreparation: TPreparation;
      FVisualization: Boolean;
      FCanvas: TfpgCanvas;
      FCurrentFont: Integer;
      FCurrentLineSpace: Integer;
      FCurrentColor: Integer;
      FNmSection: Integer;
      FNmPage: Integer;
      FNmPageSect: Integer;
      FPosRef: TRefPos;            // absolute writting position
      FHeaderHeight: Single;       // end of text vertical position in the header
      FPageHeight: Single;         // end of text vertical position in the page
      FFooterHeight: Single;       // beginning of text vertical position in the footer
      FGroup: Boolean;
      FDefaultFile: string;
      function Dim2Pixels(Value: Single): Single;
      function Pixels2Dim(Value: Single): Single;
      function AddLineBreaks(const Txt: TfpgString; AMaxLineWidth: integer; AFnt: TfpgFont): string;
      function TxtHeight(AWid: Integer; const ATxt: TfpgString; AFnt: TfpgFont; ALSpace: Integer= 2): Integer;
      function Convert2Alpha(Valeur: Integer): string;
      function GetPaperHeight: Integer;
      function GetPaperWidth: Integer;
      procedure Bv_VisuPaint(Sender: TObject);
      procedure PrepareFormat;
      procedure CreateVisu;
      procedure PrintPage(PageNumero: Integer);
      procedure ShiftFooterLines(Shift: Single);
      procedure ShiftPageLines(Shift: Single);
      procedure ShiftGroup(Shift: Single);
      function WriteText(PosX,PosY: Single; Column,Text,FontNum,BkColorNum,BordNum,SpLine: Integer;
                TxtFlags: TfpgTextFlags; Zone: TZone): Single;
      function WriteNumber(PosX,PosY: Single; Column,TextNum,TextTot,FontNum,BkColorNum,BordNum,SpLine: Integer;
                TxtFlags: TfpgTextFlags; Total,Alpha: Boolean; Zone: TZone; SPNum: TSectPageNum): Single;
      function InsertSpace(PosY: Single; Column: Integer; SpaceHeight: Single; BkColorNum: Integer; Zone: TZone): Single;
      procedure LineEnd(Zone: TZone);
      procedure DrawAFrame(StyLine: Integer; Zone: TZone);
      procedure DrawALine(XBegin,YBegin,XEnd,YEnd: Single; StyLine: Integer);
      procedure DrawAHorizLine(XBegin,YBegin: Single; Column: Integer; XEnd: Single; StyLine: Integer; Zone: TZone);
      procedure PaintSurface(Points: T_Points; Couleur: TfpgColor);
      procedure PaintImage(PosX,PosY: Single; Column,ImgNum: Integer; Zone: TZone);
      function GetSectionTitle: string;
      procedure SetSectionTitle(ATitle: string);
    public
      constructor Create;
      destructor Destroy; override;
      procedure BeginWrite(IniOrientation: TOrient= oPortrait; IniPaperType: TPaperType= A4;
                IniMeasure: TMeasureUnit= msMM; IniVersion: Char= 'F'; IniVisu: Boolean= True);
                // starts preview and printing process with initializations
                // IniOrientation = paper orientation >> oPortrait or oLandscape
                // IniPaperType = (A4, Letter,Legal,Executive,Comm10,Monarch,DL,C5,B5)
                // IniMeasure = millimeters (msMM) or inches (msInches)
                // IniVersion = version française 'F' or version English 'E', or other, to come
                // IniVisu = True (Preview) or False (direct printing or PDF generation)
      procedure EndWrite;
      procedure WriteDocument;
      procedure PagePreview;
      procedure Section(MgLeft,MgRight,MgTop,MgBottom: Single; BackPos: Single= 0;
                IniOrientation: TOrient= oPortrait);
                // new section with initialization of margins
                // BackPos = additional margin which can be necessary when frames are drawn
                // IniOrientation = paper orientation >> oPortrait or oLandscape
      procedure Page;
                // new page in the current section
      function BackColor(FdColor: TfpgColor): Integer;
               // returns the number allocated to the color
               // FdColor = background color
      function Font(FtNom: string; FtColor: TfpgColor): Integer;
               // returns the number allocated to the font
               // FtNom = FontDesc of the font
               // FtColor = font color
      function LineStyle(StThick: Single; StColor: Tfpgcolor; StStyle: TfpgLineStyle): Integer;
               // returns the number allocated to the line style
               // StThick = thickness of the line in pixels
               // StColor = line color
               // StStyle = line style
      function Border(BdFlags: TBorderFlags; BdStyle: Integer): Integer;
               // returns the number allocated to the border
               // BdFlags = position of the border (bdTop,bdBottom,bdLeft,bdRight)
               // BdStyle = border line style: thickness, color, style
      function Column(ClnPos,ClnWidth: Single; ClnMargin: Single= 0; ClnColor: TfpgColor= clWhite): Integer;
               // returns the number allocated to the column
               // ClnPos = left position in numeric value in the measurement unit (msMM or msInch)
               // ClnWidth = width in numeric value in the measurement unit (msMM or msInch)
               // ClnMargin = left and right margins in numeric value in the measurement unit (msMM or msInch)
               // ClnColor = column background color
      procedure WriteHeader(Horiz,Verti: Single; Text: string; ColNum: Integer= 0; FontNum: Integer= 0;
                LineSpNum: Integer= 0; BkColorNum: Integer= -1; BordNum: Integer= -1);
                // Horiz = horizontal position in column (cnLeft,cnCenter,cnRight)
                //         or numeric value in the measurement unit (msMM or msInch)
                // Verti = line position in column (lnCourante,lnFin)
                //         or numeric value in the measurement unit (msMM or msInch)
                // Text = text to be written
                // ColNum = column reference, default between left and right margins
                // FontNum = font reference
                // LineSpNum = space between lines reference
                // BkColorNum = background color reference, if > -1, replaces the column background color if any
                // BordNum = border reference, if > -1
      function WritePage(Horiz,Verti: Single; Text: string; ColNum: Integer= 0; FontNum: Integer= 0;
                LineSpNum: Integer= 0; BkColorNum: Integer= -1; BordNum: Integer= -1): Single;
                // Horiz = horizontal position in column (cnLeft,cnCenter,cnRight)
                //         or numeric value in the measurement unit (msMM or msInch)
                // Verti = line position in column (lnCourante,lnFin)
                //         or numeric value in the measurement unit (msMM or msInch)
                // Text = text to be written
                // ColNum = column reference, default between left and right margins
                // FontNum = font reference
                // LineSpNum = space between lines reference
                // BkColorNum = background color reference, if > -1, replaces the column background color if any
                // BordNum = border reference, if > -1
      procedure WriteFooter(Horiz,Verti: Single; Text: string; ColNum: Integer= 0; FontNum: Integer= 0;
                LineSpNum: Integer= 0; BkColorNum: Integer= -1; BordNum: Integer= -1);
                // Horiz = horizontal position in column (cnLeft,cnCenter,cnRight)
                //         or numeric value in the measurement unit (msMM or msInch)
                // Verti = line position in column (lnCourante,lnFin)
                //         or numeric value in the measurement unit (msMM or msInch)
                // Text = text to be written
                // ColNum = column reference, default between left and right margins
                // FontNum = font reference
                // LineSpNum = space between lines reference
                // BkColorNum = background color reference, if > -1, replaces the column background color if any
                // BordNum = border reference, if > -1
      procedure NumSectionHeader(Horiz,Verti: Single; TexteSect: string= ''; TextTot: string= '';
                Total: Boolean= False; Alpha: Boolean= False; ColNum: Integer= 0; FontNum: Integer= 0;
                LineSpNum: Integer= 0; BkColorNum: Integer= -1; BordNum: Integer= -1);
                // Horiz = horizontal position in column (cnLeft,cnCenter,cnRight)
                //         or numeric value in the measurement unit (msMM or msInch)
                // Verti = line position in column (lnCourante,lnFin)
                //         or numeric value in the measurement unit (msMM or msInch)
                // TexteSection = text to be written before the section number
                // TextTot = text to be written before the number of sections
                // Total= True => displays the number of sections
                // Alpha= True => displays the number of sections using letters in alphabetic order
                // ColNum = column reference, default between left and right margins
                // FontNum = font reference
                // LineSpNum = space between lines reference
                // BkColorNum = background color reference, if > -1, replaces the column background color if any
                // BordNum = border reference, if > -1
      procedure NumSectionFooter(Horiz,Verti: Single; TexteSect: string= ''; TextTot: string= '';
                Total: Boolean= False; Alpha: Boolean= False; ColNum: Integer= 0; FontNum: Integer= 0;
                LineSpNum: Integer= 0; BkColorNum: Integer= -1; BordNum: Integer= -1);
                // Horiz = horizontal position in column (cnLeft,cnCenter,cnRight)
                //         or numeric value in the measurement unit (msMM or msInch)
                // Verti = line position in column (lnCourante,lnFin)
                //         or numeric value in the measurement unit (msMM or msInch)
                // TexteSection = text to be written before the section number
                // TextTot = text to be written before the number of sections
                // Total= True => displays the number of sections
                // Alpha= True => displays the number of sections using letters in alphabetic order
                // ColNum = column reference, default between left and right margins
                // FontNum = font reference
                // LineSpNum = space between lines reference
                // BkColorNum = background color reference, if > -1, replaces the column background color if any
                // BordNum = border reference, if > -1
      procedure NumPageHeader(Horiz,Verti: Single; TextePage: string= ''; TextTot: string= '';
                Total: Boolean= False; ColNum: Integer= 0; FontNum: Integer= 0; LineSpNum: Integer= 0;
                BkColorNum: Integer= -1; BordNum: Integer= -1);
                // Horiz = horizontal position in column (cnLeft,cnCenter,cnRight)
                //         or numeric value in the measurement unit (msMM or msInch)
                // Verti = line position in column (lnCourante,lnFin)
                //         or numeric value in the measurement unit (msMM or msInch)
                // TextePage = text to be written before the page number in the document
                // TextTot = text to be written before the number of pages of the document
                // Total= True > displays the number of pages of the document
                // ColNum = column reference, default between left and right margins
                // FontNum = font reference
                // LineSpNum = space between lines reference
                // BkColorNum = background color reference, if > -1, replaces the column background color if any
                // BordNum = border reference, if > -1
      procedure NumPageFooter(Horiz,Verti: Single; TextePage: string= ''; TextTot: string= '';
                Total: Boolean= False; ColNum: Integer= 0; FontNum: Integer= 0; LineSpNum: Integer= 0;
                BkColorNum: Integer= -1; BordNum: Integer= -1);
                // Horiz = horizontal position in column (cnLeft,cnCenter,cnRight)
                //         or numeric value in the measurement unit (msMM or msInch)
                // Verti = line position in column (lnCourante,lnFin)
                //         or numeric value in the measurement unit (msMM or msInch)
                // TextePage = text to be written before the page number in the document
                // TextTot = text to be written before the number of pages of the document
                // Total= True > displays the number of pages of the document
                // ColNum = column reference, default between left and right margins
                // FontNum = font reference
                // LineSpNum = space between lines reference
                // BkColorNum = background color reference, if > -1, replaces the column background color if any
                // BordNum = border reference, if > -1
      procedure NumPageSectionHeader(Horiz,Verti: Single; TexteSect: string= ''; TextTot: string= '';
                Total: Boolean= False; Alpha: Boolean= False; ColNum: Integer= 0; FontNum: Integer= 0;
                LineSpNum: Integer= 0; BkColorNum: Integer= -1; BordNum: Integer= -1);
                // Horiz = horizontal position in column (cnLeft,cnCenter,cnRight)
                //         or numeric value in the measurement unit (msMM or msInch)
                // Verti = line position in column (lnCourante,lnFin)
                //         or numeric value in the measurement unit (msMM or msInch)
                // TextePage = text to ba written before the page number in the section
                // TextTot = text to be written before the number of pages of the section
                // Total= True > displays the number of pages of the section
                // ColNum = column reference, default between left and right margins
                // FontNum = font reference
                // LineSpNum = space between lines reference
                // BkColorNum = background color reference, if > -1, replaces the column background color if any
                // BordNum = border reference, if > -1
      procedure NumPageSectionFooter(Horiz,Verti: Single; TexteSect: string= ''; TextTot: string= '';
                Total: Boolean= False; Alpha: Boolean= False; ColNum: Integer= 0; FontNum: Integer= 0;
                LineSpNum: Integer= 0; BkColorNum: Integer= -1; BordNum: Integer= -1);
                // Horiz = horizontal position in column (cnLeft,cnCenter,cnRight)
                //         or numeric value in the measurement unit (msMM or msInch)
                // Verti = line position in column (lnCourante,lnFin)
                //         or numeric value in the measurement unit (msMM or msInch)
                // TextePage = text to ba written before the page number in the section
                // TextTot = text to be written before the number of pages of the section
                // Total= True > displays the number of pages of the section
                // ColNum = column reference, default between left and right margins
                // FontNum = font reference
                // LineSpNum = space between lines reference
                // BkColorNum = background color reference, if > -1, replaces the column background color if any
                // BordNum = border reference, if > -1
      procedure HorizLineHeader(SpBefore,SpAfter: Single; ColNum: Integer= 0; StyleNum: Integer= 0);
                // SpBefore = empty space before the horizontal line : numeric value in the measurement unit (msMM or msInch)
                // SpAfter =  empty space after the horizontal line : numeric value in the measurement unit (msMM or msInch)
                // ColNum = column reference, default between left and right margins
                // StyleNum = reference of the line style
      procedure HorizLinePage(SpBefore,SpAfter: Single; ColNum: Integer= 0; StyleNum: Integer= 0);
                // SpBefore = empty space before the horizontal line : numeric value in the measurement unit (msMM or msInch)
                // SpAfter =  empty space after the horizontal line : numeric value in the measurement unit (msMM or msInch)
                // ColNum = column reference, default between left and right margins
                // StyleNum = reference of the line style
      procedure HorizLineFooter(SpBefore,SpAfter: Single; ColNum: Integer= 0; StyleNum: Integer= 0);
                // SpBefore = empty space before the horizontal line : numeric value in the measurement unit (msMM or msInch)
                // SpAfter =  empty space after the horizontal line : numeric value in the measurement unit (msMM or msInch)
                // ColNum = column reference, default between left and right margins
                // StyleNum = reference of the line style
      procedure SpaceHeader(Verti: Single; ColNum: Integer= 0; BkColorNum: Integer= -1);
                // Verti = height of the empty space : numeric value in the measurement unit (msMM or msInch)
                // ColNum = column reference, default between left and right margins
                // BkColorNum = background color reference, if > -1, replaces the column background color if any
      procedure SpacePage(Verti: Single; ColNum: Integer= 0; BkColorNum: Integer= -1);
                // Verti = height of the empty space : numeric value in the measurement unit (msMM or msInch)
                // ColNum = column reference, default between left and right margins
                // BkColorNum = background color reference, if > -1, replaces the column background color if any
      procedure SpaceFooter(Verti: Single; ColNum: Integer= 0; BkColorNum: Integer= -1);
                // Verti = height of the empty space : numeric value in the measurement unit (msMM or msInch)
                // ColNum = column reference, default between left and right margins
                // BkColorNum = background color reference, if > -1, replaces the column background color if any
      function LineSpace(SpSup,SpInt,SpInf: Single): Integer;
               // SpSup = space between lines, top : numeric value in the measurement unit (msMM or msInch)
               // SpInt = space between lines, internal if wrapping : numeric value in the measurement unit (msMM or msInch)
               // SpInf = space between lines, botom : numeric value in the measurement unit (msMM or msInch)
      procedure BeginGroup(PageJump: Boolean= False);
                // PageJump = True >> forces new page before the group
                //          = False >> does not create a new page if the whole group can stand on the same page as the preceding text
      procedure EndGroup(PageJump: Boolean= False);
                // PageJump = True >> forces new page after the group
                //          = False >> lets continue on the same page after the group
      procedure ColorColChange(ColNum: Integer; ColColor: TfpgColor);
                // Changes the background color of a column
                // ColNum = column reference
                // ColColor = new background color for the column
      procedure FrameMargins(AStyle: Integer);
                // draw a frame at the page margins
                // AStyle = reference of the line style of the frame
      procedure FrameHeader(AStyle: Integer);
                // draw a frame at the limits of the header
                // AStyle = reference of the line style of the frame
      procedure FramePage(AStyle: Integer);
                // draw a frame at the page limits : left and right margins, header bottom and footer top
                // AStyle = reference of the line style of the frame
      procedure FrameFooter(AStyle: Integer);
                // draw a frame at the limits of the footer
                // AStyle = reference of the line style of the frame
      procedure LinePage(XBegin,YBegin,XEnd,YEnd: Single; AStyle: Integer);
                // draw a line at absolute position
                // XBegin = horizontal position of starting point in numeric value in the measurement unit (msMM or msInch)
                // YBegin = vertical position of starting point in numeric value in the measurement unit (msMM or msInch)
                // XEnd = horizontal position of ending point in numeric value in the measurement unit (msMM or msInch)
                // YEnd = vertical position of ending point in numeric value in the measurement unit (msMM or msInch)
                // AStyle = reference of the line style of the line
      procedure SurfPage(XLimits,YLimits: array of Single; AColor: TfpgColor);
                // draw a coloured surface inside the defined limit points
                // XLimits = list of horizontal positions of limit points
                // YLimits = list of vertical positions of limit points
                // AColor = colour to be painted within the limits
      procedure ImageHeader(Horiz,Verti: Single; ImgFileName: string; ColNum: Integer= 0; Scale: Integer= 1);
                // draw a bmp or jpg image at the defined position
                // Horiz = horizontal position in numeric value in the measurement unit (msMM or msInch)
                // Verti = vertical position in numeric value in the measurement unit (msMM or msInch)
                // ImgFileName = name of the image file
                // ColNum = column reference, default between left and right margins
                // Scale =  1 for full size
                //          2 for 1/2 size
                //          3 for 1/3 size
                //          4 for 1/4 size
      procedure ImagePage(Horiz,Verti: Single; ImgFileName: string; ColNum: Integer= 0; Scale: Integer= 1);
                // draw a bmp or jpg image at the defined position
                // Horiz = horizontal position in numeric value in the measurement unit (msMM or msInch)
                // Verti = vertical position in numeric value in the measurement unit (msMM or msInch)
                // ImgFileName = name of the image file
                // ColNum = column reference, default between left and right margins
                // Scale =  1 for full size
                //          2 for 1/2 size
                //          3 for 1/3 size
                //          4 for 1/4 size
      procedure ImageFooter(Horiz,Verti: Single; ImgFileName: string; ColNum: Integer= 0; Scale: Integer= 1);
                // draw a bmp or jpg image at the defined position
                // Horiz = horizontal position in numeric value in the measurement unit (msMM or msInch)
                // Verti = vertical position in numeric value in the measurement unit (msMM or msInch)
                // ImgFileName = name of the image file
                // ColNum = column reference, default between left and right margins
                // Scale =  1 for full size
                //          2 for 1/2 size
                //          3 for 1/3 size
                //          4 for 1/4 size
      property Language: Char read FVersion write FVersion;
      property Visualiser: Boolean read FVisualization write FVisualization;
      property NumSection: Integer read FNmSection write FNmSection;
      property NumPage: Integer read FNmPage write FNmPage;
      property NumPageSection: Integer read FNmPageSect write FNmPageSect;
      property PaperHeight: Integer read GetPaperHeight;
      property PagerWidth: Integer read GetPaperWidth;
      property DefaultFile: string read FDefaultFile write FDefaultFile;
      property CurrentColor: Integer read FCurrentColor write FCurrentColor;
      property SectionTitle: string read GetSectionTitle write SetSectionTitle;
    end;

  // classes for interface with PDF generation

  TPdfElement = class
    end;

  TPdfTexte= class(TPdfElement)
    private
      FPage: Integer;
      FFont: Integer;
      FSize: string;
      FPosX: Single;
      FPosY: Single;
      FWidth: Single;
      FText: string;
      FColor: TfpgColor;
    public
      property PageId: Integer read FPage write FPage;
      property FontName: Integer read FFont write FFont;
      property FontSize: string read FSize write FSize;
      property TextPosX: Single read FPosX write FPosX;
      property TextPosY: Single read FPosY write FPosY;
      property TextWidt: Single read FWidth write FWidth;
      property Writting: string read FText write FText;
      property Couleur: TfpgColor read FColor write FColor;
    end;

  TPdfRect = class(TPdfElement)
    private
      FPage: Integer;
      FThick: Single;
      FLeft: Single;
      FBottom: Single;
      FHeight: Single;
      FWidth: Single;
      FColor: Integer;
      FFill: Boolean;
      FStroke: Boolean;
      FLineStyle: TfpgLineStyle;
    protected
    public
      property PageId: Integer read FPage write FPage;
      property RectThickness: Single read FThick write FThick;
      property RectLeft: Single read FLeft write FLeft;
      property RectBottom: Single read FBottom write FBottom;
      property RectHeight: Single read FHeight write FHeight;
      property RectWidth: Single read FWidth write FWidth;
      property RectColor: Integer read FColor write FColor;
      property RectFill: Boolean read FFill write FFill;
      property RectStroke: Boolean read FStroke write FStroke;
      property RectLineStyle: TfpgLineStyle read FLineStyle write FLineStyle;
    end;

  TPdfLine = class(TPdfElement)
    private
      FPage: Integer;
      FThick: Single;
      FBeginX: Single;
      FBeginY: Single;
      FEndX: Single;
      FEndY: Single;
      FColor: Integer;
      FStyle: TfpgLineStyle;
    protected
    public
      property PageId: Integer read FPage write FPage;
      property LineThikness: Single read FThick write FThick;
      property LineBeginX: Single read FBeginX write FBeginX;
      property LineBeginY: Single read FBeginY write FBeginY;
      property LineEndX: Single read FEndX write FEndX;
      property LineEndY: Single read FEndY write FEndY;
      property LineColor: Integer read FColor write FColor;
      property LineStyle: TfpgLineStyle read FStyle write FStyle;
    end;

  TPdfSurf = class(TPdfElement)
    private
      FPage: Integer;
      FPoints: T_Points;
      FColor: Integer;
    protected
    public
      property PageId: Integer read FPage write FPage;
      property Points: T_Points read FPoints;
      property SurfColor: Integer read FColor write FColor;
    end;

  TPdfImg = class(TPdfElement)
    private
      FPage: Integer;
      FNumber: Integer;
      FLeft: Single;
      FBottom: Single;
      FWidth: Integer;
      FHeight: Integer;
    protected
    public
      property PageId: Integer read FPage write FPage;
      property ImgNumber: Integer read FNumber write FNumber;
      property ImgLeft: Single read FLeft write FLeft;
      property ImgBottom: Single read FBottom write FBottom;
      property ImgWidth: Integer read FWidth write FWidth;
      property ImgHeight: Integer read FHeight write FHeight;
    end;

var
  Infos: record
    Titre: string;
    Auteur: string;
    end;

  PdfPage: TList;
  PdfTexte: TPdfTexte;
  PdfRect: TPdfRect;
  PdfLine: TPdfLine;
  PdfSurf: TPdfSurf;
  PdfImg: TPdfImg;

const
  PPI= 72;
  FontDefaut= 0;
  ColDefaut= 0;
  lnCurrent= -1;
  lnEnd= -2;
//  cnSuite= -1;
  cnLeft= -2;
  cnCenter= -3;
  cnRight= -4;

implementation

uses
  U_Visu;

const
  InchToMM= 25.4;

function T_Report.Dim2Pixels(Value: Single): Single;
begin
if FMeasureUnit= msMM
then
  Result:= Value*PPI/InchToMM
else
  Result:= Value*PPI;
end;

function T_Report.Pixels2Dim(Value: Single): Single;
begin
if FMeasureUnit= msMM
then
  Result:= Value*InchToMM/PPI
else
  Result:= Value/PPI;
end;

function T_Report.AddLineBreaks(const Txt: TfpgString; AMaxLineWidth: integer; AFnt: TfpgFont): string;
var
  i,n,ls: integer;
  sub: string;
  lw,tw: integer;
begin
Result:= '';
ls:= Length(Txt);
lw:= 0;
i:= 1;
while i<= ls do
  begin
  if (Txt[i] in txtWordDelims)
  then       // read the delimeter only
    begin
    sub:= Txt[i];
    Inc(i);
    end
  else                                // read the whole word
    begin
    n:= PosSetEx(txtWordDelims,Txt,i);
    if n> 0
    then
      begin
      sub:= Copy(Txt,i,n-i);
      i:= n;
      end
    else
      begin
      sub:= Copy(Txt,i,MaxInt);
      i:= ls+1;
      end;
    end;
  tw:= AFnt.TextWidth(sub);            // wrap if needed
  if (lw+tw> aMaxLineWidth) and (lw> 0)
  then
    begin
    lw:= tw;
    Result:= TrimRight(Result)+sLineBreak;
    end
  else
    Inc(lw,tw);
  Result:= Result+sub;
  end;
end;

function T_Report.TxtHeight(AWid: Integer; const ATxt: TfpgString; AFnt: TfpgFont; ALSpace: Integer= 2): Integer;
var
  Cpt: Integer;
  Wraplst: TStringList;
begin
Wraplst:= TStringList.Create;
Wraplst.Text := ATxt;
for Cpt:= 0 to Pred(Wraplst.Count) do
  Wraplst[Cpt] := AddLineBreaks(Wraplst[Cpt],AWid,AFnt);
Wraplst.Text := Wraplst.Text;
Result:= (AFnt.Height*Wraplst.Count)+(ALSpace*Pred(Wraplst.Count));
WrapLst.Free;
end;

function T_Report.Convert2Alpha(Valeur: Integer): string;
var
  Cpt: Byte;
begin
Result:= '';
Cpt:= 0;
repeat
  if Valeur> 26
  then
    begin
    Valeur:= Valeur-26;
    Inc(Cpt);
    Result:= Chr(Cpt+64);
    end
  else
    begin
    Result:= Chr(Valeur+64);
    Valeur:= 0;
    end;
until Valeur< 1;
end;

function T_Report.GetPaperHeight: Integer;
begin
Result:= FPaper.H;
end;

function T_Report.GetPaperWidth: Integer;
begin
Result:= FPaper.W;
end;

procedure T_Report.Bv_VisuPaint(Sender: TObject);
begin
PrintPage(NumPage);
end;

procedure T_Report.PrepareFormat;
var
  TempH,TempW: Integer;
  TempT,TempL,TempR,TempB: Single;
begin
with FPaper do
  begin
  case FPaperType of
    A4:
      begin
      H:= 842;
      W:= 595;
      with Printable do
        begin
        T:= 10;
        L:= 11;
        R:= 586;
        B:= 822;
        end;
      end;
    Letter:
      begin
      H:= 792;
      W:= 612;
      with Printable do
        begin
        T:= 13;
        L:= 13;
        R:= 599;
        B:= 780;
        end;
      end;
    Legal:
      begin
      H:= 1008;
      W:= 612;
      with Printable do
        begin
        T:= 13;
        L:= 13;
        R:= 599;
        B:= 996;
        end;
      end;
    Executive:
      begin
      H:= 756;
      W:= 522;
      with Printable do
        begin
        T:= 14;
        L:= 13;
        R:= 508;
        B:= 744;
        end;
      end;
    Comm10:
      begin
      H:= 684;
      W:= 297;
      with Printable do
        begin
        T:= 13;
        L:= 13;
        R:= 284;
        B:= 672;
        end;
      end;
    Monarch:
      begin
      H:= 540;
      W:= 279;
      with Printable do
        begin
        T:= 13;
        L:= 13;
        R:= 266;
        B:= 528;
        end;
      end;
    DL:
      begin
      H:= 624;
      W:= 312;
      with Printable do
        begin
        T:= 14;
        L:= 13;
        R:= 297;
        B:= 611;
        end;
      end;
    C5:
      begin
      H:= 649;
      W:= 459;
      with Printable do
        begin
        T:= 13;
        L:= 13;
        R:= 446;
        B:= 637;
        end;
      end;
    B5:
      begin
      H:= 708;
      W:= 499;
      with Printable do
        begin
        T:= 14;
        L:= 13;
        R:= 485;
        B:= 696;
        end;
      end;
    end;
  if FOrientation= oLandscape
  then
    begin
    TempH:= H;
    TempW:= W;
    H:= TempW;
    W:= TempH;
    with Printable do
      begin
      TempT:= T;
      TempL:= L;
      TempR:= R;
      TempB:= B;
      T:= TempL;
      L:= TempT;
      R:= TempB;
      B:= TempR;
      end;
    end;
  end;
end;

procedure T_Report.CreateVisu;
begin
F_Visu:= TF_Visu.Create(nil, self);
with F_Visu do
  begin
  Bv_Visu:= CreateBevel(F_Visu,(F_Visu.Width-FPaper.W) div 2,((F_Visu.Height+50-FPaper.H) div 2),
            FPaper.W,FPaper.H,bsBox,bsRaised);
  Bv_Visu.BackgroundColor:= clWhite;
  Bv_Visu.OnPaint:= @Bv_VisuPaint;
  end;
end;

procedure T_Report.PrintPage(PageNumero: Integer);
var
  CptSect,CptPage,CptCmd: Integer;
  ThePage: T_Page;
  Cmd: T_Command;
begin
CptSect:= 0;
repeat
  Inc(CptSect);
  CptPage:= 0;
  with T_Section(Sections[Pred(CptSect)]) do
    repeat
      Inc(CptPage);
      ThePage:= T_Page(Pages.Items[Pred(CptPage)]);
    until (ThePage.PagesTot= PageNumero) or (CptPage= Pages.Count);
until (ThePage.PagesTot= PageNumero) or (CptSect= Sections.Count);
NumPage:= PageNumero;
NumSection:= CptSect;
NumPageSection:= ThePage.PagesSect;
with T_Section(Sections[Pred(NumSection)]) do
  begin
  if CmdHeader.Count> 0
  then
    for CptCmd:= 0 to Pred(CmdHeader.Count) do
      begin
      Cmd:= T_Command(CmdHeader.Items[CptCmd]);
      if Cmd is T_WriteText
      then
        with Cmd as T_WriteText do
          WriteText(GetPosX,GetPosY,GetColumn,GetText,GetFont,GetBackColor,GetBorder,GetLineSpace,GetFlags,ZHeader);
      if Cmd is T_Number
      then
        with Cmd as T_Number do
          WriteNumber(GetPosX,GetPosY,GetColumn,GetTextNum,GetTextTot,GetFont,GetBackColor,GetBorder,GetLineSpace,
                   GetFlags,GetTotal,GetAlpha,zHeader,GetTypeNum);
      if Cmd is T_Space
      then
        with Cmd as T_Space do
          InsertSpace(GetPosY,GetColumn,GetHeight,GetBackColor,zHeader);
      if Cmd is T_Line
      then
        with Cmd as T_Line do
          DrawALine(GetPosX,GetPosY,GetEndX,GetEndY,GetStyle);
      if Cmd is T_Image
      then
        with Cmd as T_Image do
          PaintImage(GetPosX,GetPosY,GetColumn,GetImage,zHeader);
      end;
  if GetCmdPage(NumPageSection).Count> 0
  then
    for CptCmd:= 0 to Pred(GetCmdPage(NumPageSection).Count) do
      begin
      Cmd:= T_Command(GetCmdPage(NumPageSection).Items[CptCmd]);
      if Cmd is T_WriteText
      then
        with Cmd as T_WriteText do
          WriteText(GetPosX,GetPosY,GetColumn,GetText,GetFont,GetBackColor,GetBorder,GetLineSpace,GetFlags,ZPage);
      if Cmd is T_Space
      then
        with Cmd as T_Space do
          InsertSpace(GetPosY,GetColumn,GetHeight,GetBackColor,zPage);
      if Cmd is T_Line
      then
        with Cmd as T_Line do
          DrawALine(GetPosX,GetPosY,GetEndX,GetEndY,GetStyle);
      if Cmd is T_Surface
      then
        with Cmd as T_Surface do
          PaintSurface(GetPoints,GetColor);
      if Cmd is T_Image
      then
        with Cmd as T_Image do
          PaintImage(GetPosX,GetPosY,GetColumn,GetImage,zPage);
      end;
  if CmdFooter.Count> 0
  then
    for CptCmd:= 0 to Pred(CmdFooter.Count) do
      begin
      Cmd:= T_Command(CmdFooter.Items[CptCmd]);
      if Cmd is T_WriteText
      then
        with Cmd as T_WriteText do
          WriteText(GetPosX,GetPosY,GetColumn,GetText,GetFont,GetBackColor,GetBorder,GetLineSpace,GetFlags,ZFooter);
      if Cmd is T_Number
      then
        with Cmd as T_Number do
          WriteNumber(GetPosX,GetPosY,GetColumn,GetTextNum,GetTextTot,GetFont,GetBackColor,GetBorder,GetLineSpace,
                   GetFlags,GetTotal,GetAlpha,zFooter,GetTypeNum);
      if Cmd is T_Space
      then
        with Cmd as T_Space do
          InsertSpace(GetPosY,GetColumn,GetHeight,GetBackColor,zFooter);
      if Cmd is T_Line
      then
        with Cmd as T_Line do
          DrawALine(GetPosX,GetPosY,GetEndX,GetEndY,GetStyle);
      if Cmd is T_Image
      then
        with Cmd as T_Image do
          PaintImage(GetPosX,GetPosY,GetColumn,GetImage,zFooter);
      end;
  if CmdFrames.Count> 0
  then
    for CptCmd:= 0 to Pred(CmdFrames.Count) do
      begin
      Cmd:= T_Command(CmdFrames.Items[CptCmd]);
      if Cmd is T_Frame
      then
        with Cmd as T_Frame do
          DrawAFrame(GetStyle,GetZone);
      end;
  end;
end;

procedure T_Report.ShiftFooterLines(Shift: Single);
var
  Cpt: Integer;
  Cmd: T_Command;
begin
with T_Section(Sections[Pred(NumSection)]) do
  if CmdFooter.Count> 0
  then
    for Cpt:= 0 to Pred(CmdFooter.Count) do
      begin
      Cmd:= T_Command(CmdFooter.Items[Cpt]);
      if Cmd is T_WriteText
      then
        with Cmd as T_WriteText do
          SetPosY(GetPosY-Shift);
      if Cmd is T_Number
      then
        with Cmd as T_Number do
          SetPosY(GetPosY-Shift);
      if Cmd is T_Space
      then
        with Cmd as T_Space do
          SetPosY(GetPosY-Shift);
      end;
end;

procedure T_Report.ShiftPageLines(Shift: Single);
var
  Cpt: Integer;
  Cmd: T_Command;
begin
with VWriteLine do
  for Cpt:= 0 to Pred(Commands.Count) do
    begin
    Cmd:= T_Command(Commands.Items[Cpt]);
    if Cmd is T_WriteText
    then
      with Cmd as T_WriteText do
        SetPosY(GetPosY-Shift);
    end;
end;

procedure T_Report.ShiftGroup(Shift: Single);
var
  Cpt: Integer;
  Cmd: T_Command;
begin
with VGroup do
  for Cpt:= 0 to Pred(Commands.Count) do
    begin
    Cmd:= T_Command(Commands.Items[Cpt]);
    if Cmd is T_WriteText
    then
      with Cmd as T_WriteText do
        SetPosY(GetPosY-Shift);
    end;
end;

function T_Report.WriteText(PosX,PosY: Single; Column,Text,FontNum,BkColorNum,BordNum,SpLine: Integer;
          TxtFlags: TfpgTextFlags; Zone: TZone): Single;
var
  PosH,PosV,LnSpInt,LnSpSup,LnSpInf,ThickLine: Single;
  HTxt,HeighTxt,Half,ColorLine,Cpt: Integer;
  EndOfLine,UseCurFont: Boolean;
  Fnt: TfpgFont;
  StyleLine: TfpgLineStyle;
  Wraplst: TStringList;
begin
with T_Section(Sections[Pred(NumSection)]) do
  begin
  EndOfLine:= False;
  if FPreparation= ppPrepare
  then
    if FCurrentFont<> FontNum
    then
      begin
      FCurrentFont:= FontNum;
      UseCurFont:= False;
      end
    else
      UseCurFont:= True;
  Fnt:= T_Font(Fonts[FontNum]).GetFont;
  if LineSpaces.Count= 0
  then
    LineSpace(0,0,0);
  if FCurrentLineSpace<> SpLine
  then
    FCurrentLineSpace:= SpLine;
  with T_LineSpace(LineSpaces[FCurrentLineSpace]) do
    begin
    LnSpSup:= GetSup;
    LnSpInt:= GetInt;
    LnSpInf:= GetInf;
    end;
  if Column> -1
  then
    HeighTxt:= TxtHeight(Round(T_Column(Columns[Column]).GetTextWidth),Texts[Text],Fnt,Round(LnSpInt))+Round(LnSpSup+LnSpInf)
  else
    HeighTxt:= TxtHeight(Paper.W,Texts[Text],Fnt,Round(LnSpInt))+Round(LnSpSup+LnSpInf);
  if (Column> -1) and (BordNum> -1)
  then
    Half:= Round(T_LineStyle(LineStyles[T_Border(Borders[BordNum]).GetStyle]).GetThick) div 2
  else
    Half:= 0;
  case FPreparation of
    ppPrepare:
      begin
      if NbPages= 0
      then
        Page;
      if Column> -1
      then
        begin
        HTxt:= VWriteLine.LineHeight;
        if HTxt< HeighTxt
        then
          HTxt:= HeighTxt;
        end
      else
        if HTxt< Fnt.Height
        then
          HTxt:= Fnt.Height;
      case Zone of
        zHeader:
          FPosRef.Y:= FCurrentMargin.T+FHeaderHeight;
        zPage:
          FPosRef.Y:= FCurrentMargin.T+FHeaderHeight+FPageHeight;
        zFooter:
          begin
          FPosRef.Y:= FCurrentMargin.B-HTxt;
          FFooterHeight:= FFooterHeight+HTxt;
          ShiftFooterLines(HTxt);
          end;
        end;
      if PosY= lnCurrent
      then
        PosV:= FPosRef.Y+LnSpSup
      else
        begin
        EndOfLine:= True;
        if PosY= lnEnd
        then
          begin
          PosV:= FPosRef.Y+LnSpSup;
          case Zone of
            zHeader:
              FPosRef.Y:= FPosRef.Y+HTxt;
            zPage:
              begin
              if FPosRef.Y+HTxt> FCurrentMargin.B-FFooterHeight
              then
                if FGroup
                then
                  begin
                  if VGroup.GroupeHeight+HTxt< FCurrentMargin.B-FCurrentMargin.T-FHeaderHeight-FFooterHeight
                  then
                    begin
                    Page;
                    if VGroup.Commands.Count> 0
                    then
                      begin
                      FPosRef.Y:= FCurrentMargin.T+FHeaderHeight;
                      ShiftGroup(T_WriteText(VGroup.Commands[0]).GetPosY-FPosRef.Y);
                      FPosRef.Y:= FPosRef.Y+VGroup.GroupeHeight+Succ(Half);
                      if VWriteLine.Commands.Count> 0
                      then
                        ShiftPageLines(T_WriteText(VWriteLine.Commands[0]).GetPosY-FPosRef.Y);
                      PosV:= FPosRef.Y+LnSpSup;
                      FPosRef.Y:= FPosRef.Y+HTxt+Succ(Half);
                      end
                    else
                      begin
                      if VWriteLine.Commands.Count> 0
                      then
                        ShiftPageLines(T_WriteText(VWriteLine.Commands[0]).GetPosY-FPosRef.Y);
                      PosV:= FPosRef.Y+LnSpSup;
                      FPosRef.Y:= FPosRef.Y+HTxt+Succ(Half);
                      end;
                    end
                  else
                    begin
                    LoadCmdGroupToPage;
//                    VGroup.Commands.Clear;
                    Page;
                    FPosRef.Y:= FCurrentMargin.T+FHeaderHeight;
                    if VWriteLine.Commands.Count> 0
                    then
                      ShiftPageLines(T_WriteText(VWriteLine.Commands[0]).GetPosY-FPosRef.Y);
                    PosV:= FPosRef.Y+LnSpSup;
                    FPosRef.Y:= FPosRef.Y+HTxt+Succ(Half);
                    end;
                  end
                else
                  begin
                  Page;
                  FPosRef.Y:= FCurrentMargin.T+FHeaderHeight;
                  if VWriteLine.Commands.Count> 0
                  then
                    ShiftPageLines(T_WriteText(VWriteLine.Commands[0]).GetPosY-FPosRef.Y);
                  PosV:= FPosRef.Y+LnSpSup;
                  FPosRef.Y:= FPosRef.Y+HTxt+Succ(Half);
                  end
              else
                FPosRef.Y:= FPosRef.Y+HTxt;
              end;
            end;
          if BordNum> -1
          then
            with T_Border(Borders[BordNum]) do
              if bfBottom in GetFlags
              then
                FPosRef.Y:= FPosRef.Y+1;
          end
        else
          begin
          PosV:= PosY;
          FPosRef.Y:= PosV+LnSpInf;
          end;
        case Zone of
          zHeader:
            FHeaderHeight:= FPosRef.Y-FCurrentMargin.T;
          zPage:
            FPageHeight:= FPosRef.Y-FHeaderHeight-FCurrentMargin.T;
          end;
        end;
      //if PosX= cnSuite
      //then
        //PosH:= FPosRef.X
      //else
        if Column= -1
        then
          if PosX> 0
          then
            PosH:= PosX
          else
            begin
            PosH:= T_Column(Columns[ColDefaut]).GetTextPos;
            if (txtRight in TxtFlags)
            then
              PosH:= PosH+T_Column(Columns[ColDefaut]).ColWidth-Fnt.TextWidth(Texts[Text])-T_Column(Columns[ColDefaut]).ColMargin;
            if (txtHCenter in TxtFlags)
            then
              PosH:= PosH+(T_Column(Columns[ColDefaut]).ColWidth-Fnt.TextWidth(Texts[Text]))/2;
            end
        else
          if PosX> 0
          then
            begin
            if (PosX< T_Column(Columns[Column]).GetTextPos)
               or (PosX> (T_Column(Columns[Column]).GetTextPos+T_Column(Columns[Column]).GetTextWidth))
            then
              PosH:= T_Column(Columns[Column]).GetTextPos
            else
              PosH:= PosX;
            end
          else
            begin
            PosH:= T_Column(Columns[Column]).GetTextPos;
            if (txtRight in TxtFlags)
            then
              PosH:= PosH+T_Column(Columns[Column]).ColWidth-Fnt.TextWidth(Texts[Text])-T_Column(Columns[Column]).ColMargin;
            if (txtHCenter in TxtFlags)
            then
              PosH:= PosH+(T_Column(Columns[Column]).ColWidth-Fnt.TextWidth(Texts[Text]))/2;
            end;
      FPosRef.X:= PosH+Fnt.TextWidth(Texts[Text]+' ');
      VWriteLine.LoadText(PosH,PosV,Column,Text,FontNum,HTxt,BkColorNum,BordNum,SpLine,UseCurFont,TxtFlags);
      Result:= Pixels2Dim(FPosRef.Y);
      if EndOfLine
      then
        begin
        HTxt:= 0;
        LineEnd(Zone);
        end;
      end;
    ppVisualize:
      with FCanvas do
        begin
        Font:= T_Font(Fonts[FontNum]).GetFont;
        SetTextColor(T_Font(Fonts[FontNum]).GetColor);
        if Column> -1
        then
          with T_Column(Columns[Column]) do
            begin
            if BkColorNum> -1
            then
              SetColor(T_BackColor(BackColors[BkColorNum]).GetColor)
            else
              SetColor(GetColor);
            FillRectangle(Round(ColPos),Round(PosY-LnSpSup),Round(ColWidth),HeighTxt);
            if BordNum> -1
            then
              with T_Border(Borders[BordNum]) do
                begin
                SetLineStyle(Round(T_LineStyle(LineStyles[GetStyle]).GetThick),T_LineStyle(LineStyles[GetStyle]).GetStyle);
                SetColor(T_LineStyle(LineStyles[GetStyle]).GetColor);
                if bfLeft in GetFlags
                then
                  DrawLine(Round(ColPos)+Half,Round(PosY-LnSpSup),Round(ColPos)+Half,Round(PosY-LnSpSup)+HeighTxt);
                if bfRight in GetFlags
                then
                  DrawLine(Round(ColPos+ColWidth)-Succ(Half),Round(PosY-LnSpSup),Round(ColPos+ColWidth)-Succ(Half),Round(PosY-LnSpSup)+HeighTxt);
                if bfTop in GetFlags
                then
                  DrawLine(Round(ColPos),Round(PosY-LnSpSup)+Half,Round(ColPos+ColWidth),Round(PosY-LnSpSup)+Half);
                if bfBottom in GetFlags
                then
                  DrawLine(Round(ColPos),Round(PosY-LnSpSup)+HeighTxt-Half,Round(ColPos+ColWidth),Round(PosY-LnSpSup)+HeighTxt-Half);
                end;
            DrawText(Round(GetTextPos),Round(PosY),Round(GetTextWidth),0,Texts[Text],TxtFlags,Round(LnSpInt));
            end
        else
          DrawText(Round(PosX),Round(PosY)-Fnt.Ascent,Round(Paper.W-PosX),0,Texts[Text],TxtFlags);
        end;
    ppPdfFile:
      if Column> -1
      then
        with T_Column(Columns[Column]) do
          begin
          if (GetColor<> clWhite) or (BkColorNum> -1)
          then
            begin
            PdfRect:= TPdfRect.Create;
            with PdfRect do
              begin
              PageId:= NumPage;
              FLeft:= ColPos;
              FBottom:= Paper.H-PosY+LnSpSup-HeighTxt;
              FHeight:= HeighTxt;
              FWidth:= ColWidth;
              if BkColorNum> -1
              then
                FColor:= T_BackColor(BackColors[BkColorNum]).GetColor
              else
                FColor:= GetColor;
              FFill:= True;
              FStroke:= False;
              end;
            PdfPage.Add(PdfRect);
            end;
          if BordNum> -1
          then
            with T_Border(Borders[BordNum]) do
              begin
              StyleLine:= T_LineStyle(LineStyles[GetStyle]).GetStyle;
              ColorLine:= T_LineStyle(LineStyles[GetStyle]).GetColor;
              ThickLine:= T_LineStyle(LineStyles[GetStyle]).GetThick;
              if bfLeft in GetFlags
              then
                begin
                PdfLine:= TPdfLine.Create;
                with PdfLine do
                  begin
                  PageId:= NumPage;
                  FBeginX:= ColPos;
                  FBeginY:= Paper.H-PosY+LnSpSup;
                  FEndX:= ColPos;
                  FEndY:= Paper.H-PosY+LnSpSup-HeighTxt;
                  FStyle:= StyleLine;
                  FColor:= ColorLine;
                  FThick:= ThickLine;
                  end;
                PdfPage.Add(PdfLine);
                end;
              if bfRight in GetFlags
              then
                begin
                PdfLine:= TPdfLine.Create;
                with PdfLine do
                  begin
                  PageId:= NumPage;
                  FBeginX:= ColPos+ColWidth;
                  FBeginY:= Paper.H-PosY+LnSpSup;
                  FEndX:= ColPos+ColWidth;
                  FEndY:= Paper.H-PosY+LnSpSup-HeighTxt;
                  FStyle:= StyleLine;
                  FColor:= ColorLine;
                  FThick:= ThickLine;
                  end;
                PdfPage.Add(PdfLine);
                end;
              if bfTop in GetFlags
              then
                begin
                PdfLine:= TPdfLine.Create;
                with PdfLine do
                  begin
                  PageId:= NumPage;
                  FBeginX:= ColPos;
                  FBeginY:= Paper.H-PosY+LnSpSup;
                  FEndX:= ColPos+ColWidth;
                  FEndY:= Paper.H-PosY+LnSpSup;
                  FStyle:= StyleLine;
                  FColor:= ColorLine;
                  FThick:= ThickLine;
                  end;
                PdfPage.Add(PdfLine);
                end;
              if bfBottom in GetFlags
              then
                begin
                PdfLine:= TPdfLine.Create;
                with PdfLine do
                  begin
                  PageId:= NumPage;
                  FBeginX:= ColPos;
                  FBeginY:= Paper.H-PosY+LnSpSup-HeighTxt;
                  FEndX:= ColPos+ColWidth;
                  FEndY:= Paper.H-PosY+LnSpSup-HeighTxt;
                  FStyle:= StyleLine;
                  FColor:= ColorLine;
                  FThick:= ThickLine;
                  end;
                PdfPage.Add(PdfLine);
                end;
              end;
          if Fnt.TextWidth(Texts[Text])< GetTextWidth
          then
            begin
            PdfTexte:= TPdfTexte.Create;
            with PdfTexte do
              begin
              PageId:= NumPage;
              FFont:= FontNum;
              FSize:= T_Font(Fonts[FontNum]).GetSize;
              FColor:= T_Font(Fonts[FontNum]).GetColor;
              TextPosX:= GetTextPos;
              if (txtRight in TxtFlags)
              then
                TextPosX:= ColPos+ColWidth-ColMargin-Fnt.TextWidth(Texts[Text]);
              if (txtHCenter in TxtFlags)
              then
                TextPosX:= GetTextPos+(ColWidth-Fnt.TextWidth(Texts[Text]))/2;
              TextPosY:= Paper.H-PosY-Fnt.Ascent;
              TextWidt:= ColWidth;
              Writting:= Texts[Text];
              end;
            PdfPage.Add(PdfTexte);
            end
          else
            begin
            Wraplst:= TStringList.Create;
            Wraplst.Text:= Texts[Text];
            for Cpt:= 0 to Pred(Wraplst.Count) do
              Wraplst[Cpt]:= AddLineBreaks(Wraplst[Cpt],Round(GetTextWidth),Fnt);
            Wraplst.Text:= Wraplst.Text;
            for Cpt:= 0 to Pred(Wraplst.Count) do
              begin
                PdfTexte:= TPdfTexte.Create;
                with PdfTexte do
                  begin
                  PageId:= NumPage;
                  FFont:= FontNum;
                  FSize:= T_Font(Fonts[FontNum]).GetSize;
                  FColor:= T_Font(Fonts[FontNum]).GetColor;
                  TextPosX:= GetTextPos;
                  if (txtRight in TxtFlags)
                  then
                    TextPosX:= ColPos+ColWidth-ColMargin-Fnt.TextWidth(Wraplst[Cpt]);
                  if (txtHCenter in TxtFlags)
                  then
                    TextPosX:= GetTextPos+(ColWidth-Fnt.TextWidth(Wraplst[Cpt]))/2;
                  TextPosY:= Paper.H-PosY-Fnt.Ascent-(Fnt.Height+LnSpInt)*Cpt;
                  TextWidt:= ColWidth;
                  Writting:= Wraplst[Cpt];
                  end;
                PdfPage.Add(PdfTexte);
              end;
            WrapLst.Free;
            end;
          end
      else
        if Fnt.TextWidth(Texts[Text])< Paper.W-PosX
        then
          begin
          PdfTexte:= TPdfTexte.Create;
          with PdfTexte do
            begin
            PageId:= NumPage;
            FFont:= FontNum;
            FSize:= T_Font(Fonts[FontNum]).GetSize;
            FColor:= T_Font(Fonts[FontNum]).GetColor;
            FPosX:= PosX;
            FPosY:= Paper.H-PosY;
            FWidth:= Paper.W;
            FText:= Texts[Text];
            end;
          PdfPage.Add(PdfTexte);
          end
        else
          begin
          Wraplst:= TStringList.Create;
          Wraplst.Text:= Texts[Text];
          for Cpt:= 0 to Pred(Wraplst.Count) do
            Wraplst[Cpt]:= AddLineBreaks(Wraplst[Cpt],Round(Paper.W-PosX),Fnt);
          Wraplst.Text:= Wraplst.Text;
          for Cpt:= 0 to Pred(Wraplst.Count) do
            begin
            PdfTexte:= TPdfTexte.Create;
            with PdfTexte do
              begin
              PageId:= NumPage;
              FFont:= FontNum;
              FSize:= T_Font(Fonts[FontNum]).GetSize;
              FColor:= T_Font(Fonts[FontNum]).GetColor;
              FPosX:= PosX;
              FPosY:= Paper.H-PosY-Fnt.Ascent-(Fnt.Height+LnSpInt)*Cpt;
              FWidth:= Paper.W;
              FText:= Wraplst[Cpt];
              end;
            PdfPage.Add(PdfTexte);
            end;
          WrapLst.Free;
          end;
    end;
  end;
end;

function T_Report.WriteNumber(PosX,PosY: Single; Column,TextNum,TextTot,FontNum,BkColorNum,BordNum,SpLine: Integer;
          TxtFlags: TfpgTextFlags; Total,Alpha: Boolean; Zone: TZone; SPNum: TSectPageNum): Single;

  function BuildChaine: string;
  var
    NumAlpha: string;
  begin
  case SPNum of
    PageNum:
      if Total
      then
        Result:= Texts[TextNum]+' '+IntToStr(NumPage)+' '+Texts[TextTot]+' '
                 +IntToStr(T_Section(Sections[Pred(Sections.Count)]).TotPages)
      else
        Result:= Texts[TextNum]+' '+IntToStr(NumPage);
    SectNum:
      begin
      if Alpha
      then
        NumAlpha:= Convert2Alpha(NumSection)
      else
        NumAlpha:= IntToStr(NumSection);
      if Total
      then
        Result:= Texts[TextNum]+' '+NumAlpha+' '+Texts[TextTot]+' '+IntToStr(Sections.Count)
      else
        Result:= Texts[TextNum]+' '+NumAlpha;
      end;
    PSectNum:
      begin
      if Alpha
      then
        NumAlpha:= Convert2Alpha(NumPageSection)
      else
        NumAlpha:= IntToStr(NumPageSection);
      if Total
      then
        Result:= Texts[TextNum]+' '+NumAlpha+' '+Texts[TextTot]+' '
                 +IntToStr(T_Section(Sections[Pred(NumSection)]).NbPages)
      else
        Result:= Texts[TextNum]+' '+NumAlpha;
      end;
    end;
  end;

var
  PosH,PosV,LnSpInt,LnSpSup,LnSpInf,ThickLine: Single;
  HTxt,HeighTxt,Half,ColorLine: Integer;
  EndOfLine,UseCurFont: Boolean;
  Fnt: TfpgFont;
  StyleLine: TfpgLineStyle;
  Chaine: string;
begin
with T_Section(Sections[Pred(NumSection)]) do
  begin
  EndOfLine:= False;
  if FPreparation= ppPrepare
  then
    if FCurrentFont<> FontNum
    then
      begin
      FCurrentFont:= FontNum;
      UseCurFont:= False;
      end
    else
      UseCurFont:= True;
  Fnt:= T_Font(Fonts[FontNum]).GetFont;
  if LineSpaces.Count= 0
  then
    LineSpace(0,0,0);
  if FCurrentLineSpace<> SpLine
  then
    FCurrentLineSpace:= SpLine;
  with T_LineSpace(LineSpaces[FCurrentLineSpace]) do
    begin
    LnSpSup:= GetSup;
    LnSpInt:= GetInt;
    LnSpInf:= GetInf;
    end;
  if Column> -1
  then
    HeighTxt:= TxtHeight(Round(T_Column(Columns[Column]).GetTextWidth),Texts[TextNum]+' 0 '+Texts[TextTot]+' 0',Fnt,Round(LnSpInt))+Round(LnSpSup+LnSpInf)
  else
    HeighTxt:= TxtHeight(Paper.W,Texts[TextNum]+' 0 '+Texts[TextTot]+' 0',Fnt,Round(LnSpInt))+Round(LnSpSup+LnSpInf);
  if (Column> -1) and (BordNum> -1)
  then
    Half:= Round(T_LineStyle(LineStyles[T_Border(Borders[BordNum]).GetStyle]).GetThick) div 2;
  case FPreparation of
    ppPrepare:
      begin
      if NbPages= 0
      then
        Page;
      if Column> -1
      then
        begin
        HTxt:= VWriteLine.LineHeight;
        if HTxt< HeighTxt
        then
          HTxt:= HeighTxt;
        end
      else
        if HTxt< Fnt.Height
        then
          HTxt:= Fnt.Height;
      case Zone of
        zHeader:
          FPosRef.Y:= FCurrentMargin.T+FHeaderHeight;
        zFooter:
          begin
          FPosRef.Y:= FCurrentMargin.B-HTxt;
          FFooterHeight:= FFooterHeight+HTxt;
          ShiftFooterLines(HTxt);
          end;
        end;
      if PosY= lnCurrent
      then
        PosV:= FPosRef.Y+LnSpSup
      else
        begin
        EndOfLine:= True;
        if PosY= lnEnd
        then
          begin
          PosV:= FPosRef.Y+LnSpSup;
          case Zone of
            zHeader:
              FPosRef.Y:= FPosRef.Y+HTxt;
            end;
          if BordNum> -1
          then
            with T_Border(Borders[BordNum]) do
              if bfBottom in GetFlags
              then
                FPosRef.Y:= FPosRef.Y+1;
          end
        else
          begin
          PosV:= PosY;
          FPosRef.Y:= PosV+LnSpInf;
          end;
        case Zone of
          zHeader:
            FHeaderHeight:= FPosRef.Y-FCurrentMargin.T;
          end;
        end;
        if Column= -1
        then
          if PosX> 0
          then
            PosH:= PosX
          else
            begin
            PosH:= T_Column(Columns[ColDefaut]).GetTextPos-T_Column(Columns[0]).ColMargin;
            if (txtRight in TxtFlags)
            then
              if Total
              then
                PosH:= PosH+T_Column(Columns[ColDefaut]).ColWidth-Fnt.TextWidth(Texts[TextNum]+' 0 '+Texts[TextTot]+' 0 ')-T_Column(Columns[ColDefaut]).ColMargin
              else
                PosH:= PosH+T_Column(Columns[ColDefaut]).ColWidth-Fnt.TextWidth(Texts[TextNum]+' 0 ')-T_Column(Columns[ColDefaut]).ColMargin;
            if (txtHCenter in TxtFlags)
            then
              if Total
              then
                PosH:= PosH+(T_Column(Columns[ColDefaut]).ColWidth-Fnt.TextWidth(Texts[TextNum]+' 0 '+Texts[TextTot]+' 0 '))/2
              else
                PosH:= PosH+(T_Column(Columns[ColDefaut]).ColWidth-Fnt.TextWidth(Texts[TextNum]+' 0 '))/2;
            end
        else
          if PosX> 0
          then
            if (PosX< T_Column(Columns[Column]).GetTextPos)
               or (PosX> (T_Column(Columns[Column]).GetTextPos+T_Column(Columns[Column]).GetTextWidth))
            then
              PosH:= T_Column(Columns[Column]).GetTextPos
            else
              PosH:= PosX
          else
            begin
            PosH:= T_Column(Columns[Column]).GetTextPos-T_Column(Columns[Column]).ColMargin;
            if (txtRight in TxtFlags)
            then
              if Total
              then
                PosH:= PosH+T_Column(Columns[Column]).ColWidth-Fnt.TextWidth(Texts[TextNum]+' 0 '+Texts[TextTot]+' 0 ')-T_Column(Columns[Column]).ColMargin
              else
                PosH:= PosH+T_Column(Columns[Column]).ColWidth-Fnt.TextWidth(Texts[TextNum]+' 0 ')-T_Column(Columns[Column]).ColMargin;
            if (txtHCenter in TxtFlags)
            then
              if Total
              then
                PosH:= PosH+(T_Column(Columns[Column]).ColWidth-Fnt.TextWidth(Texts[TextNum]+' 0 '+Texts[TextTot]+' 0 '))/2
              else
                PosH:= PosH+(T_Column(Columns[Column]).ColWidth-Fnt.TextWidth(Texts[TextNum]+' 0 '))/2;
            end;
      FPosRef.X:= PosH+Fnt.TextWidth(Texts[TextNum]+' 0 '+Texts[TextTot]+' 0 ');
      VWriteLine.LoadNumber(PosH,PosV,Column,TextNum,TextTot,FontNum,HTxt,BkColorNum,BordNum,SpLine,UseCurFont,TxtFlags,Total,Alpha,SPNum);
      Result:= Pixels2Dim(FPosRef.Y);
      if EndOfLine
      then
        begin
        HTxt:= 0;
        LineEnd(Zone);
        end;
      end;
    ppVisualize:
      with FCanvas do
        begin
        Chaine:= BuildChaine;
        Font:= T_Font(Fonts[FontNum]).GetFont;
        SetTextColor(T_Font(Fonts[FontNum]).GetColor);
        if Column> -1
        then
          with T_Column(Columns[Column]) do
            begin
            if BkColorNum> -1
            then
              SetColor(T_BackColor(BackColors[BkColorNum]).GetColor)
            else
              SetColor(GetColor);
            FillRectangle(Round(ColPos),Round(PosY-LnSpSup),Round(ColWidth),HeighTxt);
            if BordNum> -1
            then
              with T_Border(Borders[BordNum]) do
                begin
                SetLineStyle(Round(T_LineStyle(LineStyles[GetStyle]).GetThick),T_LineStyle(LineStyles[GetStyle]).GetStyle);
                SetColor(T_LineStyle(LineStyles[GetStyle]).GetColor);
                if bfLeft in GetFlags
                then
                  DrawLine(Round(ColPos)+Half,Round(PosY-LnSpSup),Round(ColPos)+Half,Round(PosY-LnSpSup)+HeighTxt);
                if bfRight in GetFlags
                then
                  DrawLine(Round(ColPos+ColWidth)-Half,Round(PosY-LnSpSup),Round(ColPos+ColWidth)-Half,Round(PosY-LnSpSup)+HeighTxt);
                if bfTop in GetFlags
                then
                  DrawLine(Round(ColPos),Round(PosY-LnSpSup)+Half,Round(ColPos+ColWidth),Round(PosY-LnSpSup)+Half);
                if bfBottom in GetFlags
                then
                  DrawLine(Round(ColPos),Round(PosY-LnSpSup)+HeighTxt-Succ(Half),Round(ColPos+ColWidth),Round(PosY-LnSpSup)+HeighTxt-Succ(Half));
                end;
            DrawText(Round(GetTextPos),Round(PosY),Round(GetTextWidth),0,Chaine,TxtFlags,Round(LnSpInt));
            end
        else
          DrawText(Round(PosX),Round(PosY),Chaine,TxtFlags);
        end;
    ppPdfFile:
      begin
      Chaine:= BuildChaine;
        if Column> -1
        then
          with T_Column(Columns[Column]) do
            begin
            if (GetColor<> clWhite) or (BkColorNum> -1)
            then
              begin
              PdfRect:= TPdfRect.Create;
              with PdfRect do
                begin
                PageId:= NumPage;
                FLeft:= ColPos;
                FBottom:= Paper.H-PosY+LnSpSup-HeighTxt;
                FHeight:= HeighTxt;
                FWidth:= ColWidth;
                if BkColorNum> -1
                then
                  FColor:= T_BackColor(BackColors[BkColorNum]).GetColor
                else
                  FColor:= GetColor;
                FFill:= True;
                FStroke:= False;
                end;
              PdfPage.Add(PdfRect);
              end;
            if BordNum> -1
            then
              with T_Border(Borders[BordNum]) do
                begin
                StyleLine:= T_LineStyle(LineStyles[GetStyle]).GetStyle;
                ColorLine:= T_LineStyle(LineStyles[GetStyle]).GetColor;
                ThickLine:= T_LineStyle(LineStyles[GetStyle]).GetThick;
                if bfLeft in GetFlags
                then
                  begin
                  PdfLine:= TPdfLine.Create;
                  with PdfLine do
                    begin
                    PageId:= NumPage;
                    FBeginX:= ColPos;
                    FBeginY:= Paper.H-PosY+LnSpSup;
                    FEndX:= ColPos;
                    FEndY:= Paper.H-PosY+LnSpSup-HeighTxt;
                    FStyle:= StyleLine;
                    FColor:= ColorLine;
                    FThick:= ThickLine;
                    end;
                  PdfPage.Add(PdfLine);
                  end;
                if bfRight in GetFlags
                then
                  begin
                  PdfLine:= TPdfLine.Create;
                  with PdfLine do
                    begin
                    PageId:= NumPage;
                    FBeginX:= ColPos+ColWidth;
                    FBeginY:= Paper.H-PosY+LnSpSup;
                    FEndX:= ColPos+ColWidth;
                    FEndY:= Paper.H-PosY+LnSpSup-HeighTxt;
                    FStyle:= StyleLine;
                    FColor:= ColorLine;
                    FThick:= ThickLine;
                    end;
                  PdfPage.Add(PdfLine);
                  end;
                if bfTop in GetFlags
                then
                  begin
                  PdfLine:= TPdfLine.Create;
                  with PdfLine do
                    begin
                    PageId:= NumPage;
                    FBeginX:= ColPos;
                    FBeginY:= Paper.H-PosY+LnSpSup;
                    FEndX:= ColPos+ColWidth;
                    FEndY:= Paper.H-PosY+LnSpSup;
                    FStyle:= StyleLine;
                    FColor:= ColorLine;
                    FThick:= ThickLine;
                    end;
                  PdfPage.Add(PdfLine);
                  end;
                if bfBottom in GetFlags
                then
                  begin
                  PdfLine:= TPdfLine.Create;
                  with PdfLine do
                    begin
                    PageId:= NumPage;
                    FBeginX:= ColPos;
                    FBeginY:= Paper.H-PosY+LnSpSup-HeighTxt;
                    FEndX:= ColPos+ColWidth;
                    FEndY:= Paper.H-PosY+LnSpSup-HeighTxt;
                    FStyle:= StyleLine;
                    FColor:= ColorLine;
                    FThick:= ThickLine;
                    end;
                  PdfPage.Add(PdfLine);
                  end;
                end;
            PdfTexte:= TPdfTexte.Create;
            with PdfTexte do
              begin
              PageId:= NumPage;
              FFont:= FontNum;
              FSize:= T_Font(Fonts[FontNum]).GetSize;
              FColor:= T_Font(Fonts[FontNum]).GetColor;
              TextPosX:= GetTextPos;
              if (txtRight in TxtFlags)
              then
                TextPosX:= ColPos+ColWidth-ColMargin-Fnt.TextWidth(Chaine);
              if (txtHCenter in TxtFlags)
              then
                TextPosX:= GetTextPos+(ColWidth-Fnt.TextWidth(Chaine))/2;
              TextPosY:= Paper.H-PosY-Fnt.Ascent;
              TextWidt:= ColWidth;
              Writting:= Chaine;
              end;
            PdfPage.Add(PdfTexte);
            end
        else
          begin
          PdfTexte:= TPdfTexte.Create;
          with PdfTexte do
            begin
            PageId:= NumPage;
            FFont:= FontNum;
            FSize:= T_Font(Fonts[FontNum]).GetSize;
            FColor:= T_Font(Fonts[FontNum]).GetColor;
            FPosX:= PosX;
            FPosY:= PosY-Fnt.Ascent;
            FWidth:= Paper.W;
            FText:= Chaine;
            end;
          PdfPage.Add(PdfTexte);
          end;
        end;
    end;
  end;
end;

function T_Report.InsertSpace(PosY: Single; Column: Integer; SpaceHeight: Single; BkColorNum: Integer; Zone: TZone): Single;
var
  PosV: Single;
begin
with T_Section(Sections[Pred(NumSection)]) do
  begin
  if PosY> -1
  then
    PosV:= PosY
  else
    PosV:= FPosRef.Y;
  case FPreparation of
    ppPrepare:
      begin
      case Zone of
        zHeader:
          begin
          FPosRef.Y:= FCurrentMargin.T+FHeaderHeight;
          FPosRef.Y:= FPosRef.Y+SpaceHeight;
          FHeaderHeight:= FPosRef.Y-FCurrentMargin.T;
          LoadSpaceHeader(PosV,Column,SpaceHeight,BkColorNum);
          end;
        zPage:
          begin
          FPosRef.Y:= FCurrentMargin.T+FHeaderHeight+FPageHeight;
          if FPosRef.Y+SpaceHeight> FCurrentMargin.B-FFooterHeight
          then
            begin
            FPosRef.Y:= FCurrentMargin.T+FHeaderHeight;
            Page;
            end
          else
            FPosRef.Y:= FPosRef.Y+SpaceHeight;
          FPageHeight:= FPosRef.Y-FHeaderHeight-FCurrentMargin.T;
          LoadSpacePage(PosV,Column,SpaceHeight,BkColorNum);
          end;
        zFooter:
          begin
          FPosRef.Y:= FCurrentMargin.B-SpaceHeight;
          FFooterHeight:= FFooterHeight+SpaceHeight;
          PosV:= FPosRef.Y;
          ShiftFooterLines(SpaceHeight);
          LoadSpaceFooter(PosV,Column,SpaceHeight,BkColorNum);
          end;
        end;
      if FGroup
      then
        LoadSpaceGroup(SpaceHeight);
      Result:= Pixels2Dim(FPosRef.Y);
      LineEnd(Zone);
      end;
    ppVisualize:
      with FCanvas,T_Column(Columns[Column]) do
        begin
        if BkColorNum> -1
        then
          SetColor(T_BackColor(BackColors[BkColorNum]).GetColor)
        else
          SetColor(GetColor);
        FillRectangle(Round(ColPos),Round(PosV),Round(ColWidth),Round(SpaceHeight));
        end;
    ppPdfFile:
      begin
      if Column> -1
      then
        with T_Column(Columns[Column]) do
          begin
          if (GetColor<> clWhite) or (BkColorNum> -1)
          then
            begin
            PdfRect:= TPdfRect.Create;
            with PdfRect do
              begin
              PageId:= NumPage;
              FLeft:= ColPos;
              FBottom:= Paper.H-PosY-SpaceHeight;
              FHeight:= SpaceHeight;
              FWidth:= ColWidth;
              if BkColorNum> -1
              then
                FColor:= T_BackColor(BackColors[BkColorNum]).GetColor
              else
                FColor:= GetColor;
              FFill:= True;
              FStroke:= False;
              end;
            PdfPage.Add(PdfRect);
            end;
          end;
      end;
    end;
  end;
end;

procedure T_Report.LineEnd(Zone: TZone);
begin
with T_Section(Sections[Pred(NumSection)]) do
  case Zone of
    zHeader:
      LoadCmdHeader;
    zPage:
      if FGroup
      then
        LoadCmdGroup
      else
        LoadCmdPage;
    zFooter:
      LoadCmdFooter;
    end;
end;

procedure T_Report.DrawAFrame(StyLine: Integer; Zone: TZone);
var
  Half,MarginL,MarginR,MarginT,MarginB,HeaderH,FooterH: Integer;
begin
with T_Section(Sections[Pred(NumSection)]) do
  case FPreparation of
    ppPrepare:
      LoadFrame(StyLine,Zone);
    ppVisualize:
      with FCanvas do
        begin
        with T_LineStyle(LineStyles[StyLine]) do
          begin
          SetLineStyle(Round(GetThick),GetStyle);
          Half:= Round(GetThick) div 2;
          SetColor(GetColor);
          end;
        with FCurrentMargin do
          begin
          MarginL:= Round(L);
          MarginR:= Round(R);
          MarginT:= Round(T);
          MarginB:= Round(B);
          HeaderH:= Round(FHeaderHeight);
          FooterH:= Round(FFooterHeight);
          case Zone of
            zHeader:
              begin
              DrawLine(MarginL+Half,MarginT,MarginL+Half,MarginT+HeaderH);          // left
              DrawLine(MarginR-Half,MarginT,MarginR-Half,MarginT+HeaderH);          // right
              DrawLine(MarginL,MarginT+Half,MarginR,MarginT+Half);                  // top
              DrawLine(MarginL,MarginT+HeaderH-Half,MarginR,MarginT+HeaderH-Half);  // bottom
              end;
            zPage:
              begin
              DrawLine(MarginL+Half,MarginT+HeaderH,MarginL+Half,MarginB-FooterH);  // left
              DrawLine(MarginR-Half,MarginT+HeaderH,MarginR-Half,MarginB-FooterH);  // right
              DrawLine(MarginL,MarginT+HeaderH-Half,MarginR,MarginT+HeaderH-Half);  // top
              DrawLine(MarginL,MarginB-FooterH+Half,MarginR,MarginB-FooterH+Half);  // bottom
              end;
            zFooter:
              begin
              DrawLine(MarginL+Half,MarginB-FooterH,MarginL+Half,MarginB);          // left
              DrawLine(MarginR-Half,MarginB-FooterH,MarginR-Half,MarginB);          // right
              DrawLine(MarginL,MarginB-FooterH+Half,MarginR,MarginB-FooterH+Half);  // top
              DrawLine(MarginL,MarginB-Half,MarginR,MarginB-Half);                  // bottom
              end;
            zMargins:
              begin
              DrawLine(MarginL+Half,MarginT,MarginL+Half,MarginB-Succ(Half));       // left
              DrawLine(MarginR-Half,MarginT,MarginR-Half,MarginB-Succ(Half));       // right
              DrawLine(MarginL,MarginT+Half,MarginR,MarginT+Half);                  // top
              DrawLine(MarginL,MarginB-Half,MarginR,MarginB-Half);                  // bottom
              end;
            end;
          end;
        end;
    ppPdfFile:
      begin
      PdfRect:= TPdfRect.Create;
      with PdfRect do
        begin
        PageId:= NumPage;
        with T_LineStyle(LineStyles[StyLine]) do
          begin
          FThick:= GetThick;
          FColor:= GetColor;
          FLineStyle:= GetStyle;
          end;
        with FCurrentMargin do
          case Zone of
            zHeader:
              begin
              FLeft:= L;
              FBottom:= Paper.H-T-FHeaderHeight;
              FHeight:= FHeaderHeight;
              FWidth:= R-L;
              end;
            zPage:
              begin
              FLeft:= L;
              FBottom:= Paper.H-B+FFooterHeight;
              FHeight:= B-T-FHeaderHeight-FFooterHeight;
              FWidth:= R-L;
              end;
            zFooter:
              begin
              FLeft:= L;
              FBottom:= Paper.H-B;
              FHeight:= FFooterHeight;
              FWidth:= R-L;
              end;
            zMargins:
              begin
              FLeft:= L;
              FBottom:= Paper.H-B;
              FHeight:= B-T;
              FWidth:= R-L;
              end;
            end;
        FFill:= False;
        FStroke:= True;
        PdfPage.Add(PdfRect);
        end;
      end;
    end;
end;

procedure T_Report.DrawALine(XBegin,YBegin,XEnd,YEnd: Single; StyLine: Integer);
begin
with T_Section(Sections[Pred(NumSection)]) do
  case FPreparation of
    ppPrepare:
      LoadLine(XBegin,YBegin,ColDefaut,XEnd,YEnd,StyLine);
    ppVisualize:
      with FCanvas do
        begin
        with T_LineStyle(LineStyles[StyLine]) do
          begin
          SetLineStyle(Round(GetThick),GetStyle);
          SetColor(GetColor);
          end;
        DrawLine(Round(XBegin),Round(YBegin),Round(XEnd),Round(YEnd));
        end;
    ppPdfFile:
      begin
      PdfLine:= TPdfLine.Create;
      with PdfLine do
        begin
        PageId:= NumPage;
        FBeginX:= XBegin;
        FBeginY:= Paper.H-YBegin;
        FEndX:= XEnd;
        FEndY:= Paper.H-YEnd;
        FStyle:= T_LineStyle(LineStyles[StyLine]).GetStyle;;
        FColor:= T_LineStyle(LineStyles[StyLine]).GetColor;
        FThick:= T_LineStyle(LineStyles[StyLine]).GetThick;
        end;
      PdfPage.Add(PdfLine);
      end;
    end;
end;

procedure T_Report.DrawAHorizLine(XBegin,YBegin: Single; Column: Integer; XEnd: Single; StyLine: Integer; Zone: TZone);
var
  PosV: Single;
begin
with T_Section(Sections[Pred(NumSection)]) do
  case FPreparation of
    ppPrepare:
      begin
      case Zone of
        zHeader:
          begin
          FPosRef.Y:= FCurrentMargin.T+FHeaderHeight;
          PosV:= FPosRef.Y+XBegin;
          FPosRef.Y:= FPosRef.Y+XBegin+YBegin+T_LineStyle(LineStyles[StyLine]).GetThick;
          FHeaderHeight:= FPosRef.Y-FCurrentMargin.T;
          with T_Column(Columns[Column]) do
            LoadLineHorizHeader(ColPos,PosV,Column,ColPos+ColWidth,PosV,StyLine);
          end;
        zPage:
          begin
          FPosRef.Y:= FCurrentMargin.T+FHeaderHeight+FPageHeight;
          PosV:= FPosRef.Y+XBegin;
          FPosRef.Y:= FPosRef.Y+XBegin+YBegin+T_LineStyle(LineStyles[StyLine]).GetThick;
          FPageHeight:= FPosRef.Y-FHeaderHeight-FCurrentMargin.T;
          with T_Column(Columns[Column]) do
            LoadLineHorizPage(ColPos,PosV,Column,ColPos+ColWidth,PosV,StyLine);
          end;
        zFooter:
          begin
          FPosRef.Y:= FCurrentMargin.B-XBegin;
          PosV:= FPosRef.Y;
          FPosRef.Y:= FPosRef.Y-YBegin-T_LineStyle(LineStyles[StyLine]).GetThick;
          FFooterHeight:= FFooterHeight+XBegin+YBegin+T_LineStyle(LineStyles[StyLine]).GetThick;
          ShiftFooterLines(XBegin+YBegin+T_LineStyle(LineStyles[StyLine]).GetThick);
          with T_Column(Columns[Column]) do
            LoadLineHorizFooter(ColPos,PosV,Column,ColPos+ColWidth,PosV,StyLine);
          end;
        end;
      if FGroup
      then
        LoadLineHorizGroupe(T_LineStyle(LineStyles[StyLine]).GetThick);
      end;
    ppVisualize:
      with FCanvas do
        begin
        with T_LineStyle(LineStyles[StyLine]) do
          begin
          SetLineStyle(Round(GetThick),GetStyle);
          SetColor(GetColor);
          end;
        DrawLine(Round(XBegin),Round(YBegin),Round(XEnd),Round(YBegin));
        end;
    ppPdfFile:
      begin
      PdfLine:= TPdfLine.Create;
      with PdfLine do
        begin
        PageId:= NumPage;
        FBeginX:= XBegin;
        FBeginY:= Paper.H-YBegin;
        FEndX:= XEnd;
        FEndY:= Paper.H-YBegin;
        FStyle:= T_LineStyle(LineStyles[StyLine]).GetStyle;;
        FColor:= T_LineStyle(LineStyles[StyLine]).GetColor;
        FThick:= T_LineStyle(LineStyles[StyLine]).GetThick;
        end;
      PdfPage.Add(PdfLine);
      end;
    end;
end;

procedure T_Report.PaintSurface(Points: T_Points; Couleur: TfpgColor);
var
  OldColor: TfpgColor;
  Cpt: Integer;
  Pts: array of TPoint;
begin
with T_Section(Sections[Pred(NumSection)]) do
  case FPreparation of
    ppPrepare:
      LoadSurf(Points,Couleur);
    ppVisualize:
      begin
      OldColor:= FCanvas.Color;
      FCanvas.SetColor(Couleur);
      SetLength(Pts,Length(Points));
      for Cpt:= 0 to Pred(Length(Pts)) do
        begin
        Pts[Cpt].X:= Round(Points[Cpt].X);
        Pts[Cpt].Y:= Round(Points[Cpt].Y);
        end;
      FCanvas.DrawPolygon(Pts);
      FCanvas.SetColor(OldColor);
      end;
    ppPdfFile:
      begin
      PdfSurf:= TPdfSurf.Create;
      SetLength(PdfSurf.FPoints,Length(Points));
      for Cpt:= 0 to Pred(Length(Points)) do
        begin
        PdfSurf.FPoints[Cpt].X:= Points[Cpt].X;
        PdfSurf.FPoints[Cpt].Y:= Paper.H-Points[Cpt].Y;
        end;
      with PdfSurf do
        begin
        PageId:= NumPage;
        //SetLength(FPoints,Length(Points));
        //for Cpt:= 0 to Pred(Length(Points)) do   // weird behaviour: points gets length= 0 inside the with clause !
        //  begin
        //  FPoints[Cpt].X:= Points[Cpt].X;
        //  FPoints[Cpt].Y:= Paper.H-Points[Cpt].Y;
        //  end;
        FColor:= Couleur;
        end;
      PdfPage.Add(PdfSurf);
      end;
    end;
end;

procedure T_Report.PaintImage(PosX,PosY: Single; Column,ImgNum: Integer; Zone: TZone);
begin
with T_Section(Sections[Pred(NumSection)]) do
  case FPreparation of
    ppPrepare:
      begin
      if Column> -1
      then
        PosX:= T_Column(Columns[Column]).ColPos+PosX;
      case Zone of
        zHeader:
          begin
          PosY:= FCurrentMargin.T+PosY;
          LoadImgHeader(PosX,PosY,Column,ImgNum);
          end;
        zPage:
          begin
          PosY:= FCurrentMargin.T+FHeaderHeight+PosY;
          LoadImgPage(PosX,PosY,Column,ImgNum);
          end;
        zFooter:
          begin
          PosY:= FCurrentMargin.B-FFooterHeight+PosY;
          LoadImgFooter(PosX,PosY,Column,ImgNum);
          end;
        end;
      end;
    ppVisualize:
      FCanvas.DrawImage(Round(PosX),Round(PosY),TfpgImage(Images[ImgNum]));
    ppPdfFile:
      begin
      PdfImg:= TPdfImg.Create;
      with PdfImg do
        begin
        PageId:= NumPage;
        ImgNumber:= ImgNum;
        ImgLeft:= PosX;
        ImgBottom:= Paper.H-PosY-TfpgImage(Images[ImgNum]).Height;
        ImgWidth:= TfpgImage(Images[ImgNum]).Width;
        ImgHeight:= TfpgImage(Images[ImgNum]).Height;
        end;
      PdfPage.Add(PdfImg);
      end;
    end;
end;

function T_Report.GetSectionTitle: string;
begin
Result:= T_Section(Sections[Pred(Sections.Count)]).Title;
end;

procedure T_Report.SetSectionTitle(ATitle: string);
begin
T_Section(Sections[Pred(Sections.Count)]).Title:= ATitle;
end;

{ public methods }

constructor T_Report.Create;
begin
inherited Create;
OldSeparator:= DecimalSeparator;
DecimalSeparator:= '.';
Sections:= TList.Create;
Fonts:= TList.Create;
LineSpaces:= TList.Create;
BackColors:= TList.Create;
LineStyles:= TList.Create;
Borders:= TList.Create;
Images:= TList.Create;
ImageNames:= TStringList.Create;
Texts:= TStringList.Create;
VWriteLine:= T_WriteLine.Create;
PdfPage:= TList.Create;
Outline:= False;
end;

destructor T_Report.Destroy;
var
  Cpt: Integer;
begin
if Sections.Count> 0
then
  for Cpt:= 0 to Pred(Sections.Count) do
    T_Section(Sections[Cpt]).Free;
Sections.Free;
if Fonts.Count> 0
then
  for Cpt:= 0 to Pred(Fonts.Count) do
    T_Font(Fonts[Cpt]).Free;
Fonts.Free;
if LineSpaces.Count> 0
then
  for Cpt:= 0 to Pred(LineSpaces.Count) do
    T_LineSpace(LineSpaces[Cpt]).Free;
LineSpaces.Free;
if BackColors.Count> 0
then
  for Cpt:= 0 to Pred(BackColors.Count) do
    T_BackColor(BackColors[Cpt]).Free;
BackColors.Free;
if LineStyles.Count> 0
then
  for Cpt:= 0 to Pred(LineStyles.Count) do
    T_LineStyle(LineStyles[Cpt]).Free;
LineStyles.Free;
if Borders.Count> 0
then
  for Cpt:= 0 to Pred(Borders.Count) do
    T_Border(Borders[Cpt]).Free;
Borders.Free;
if Images.Count> 0
then
  for Cpt:= 0 to Pred(Images.Count) do
    TfpgImage(Images[Cpt]).Free;
Images.Free;
ImageNames.Free;
Texts.Free;
VWriteLine.Free;
if PdfPage.Count> 0
then
  for Cpt:= 0 to Pred(PdfPage.Count) do
    if TPdfElement(PdfPage[Cpt]) is TPdfTexte
    then
      TPdfTexte(PdfPage[Cpt]).Free
    else
      if TPdfElement(PdfPage[Cpt]) is TPdfRect
      then
        TPdfRect(PdfPage[Cpt]).Free
      else
        if TPdfElement(PdfPage[Cpt]) is TPdfLine
        then
          TPdfLine(PdfPage[Cpt]).Free
        else
          if TPdfElement(PdfPage[Cpt]) is TPdfSurf
          then
            TPdfSurf(PdfPage[Cpt]).Free
          else
            if TPdfElement(PdfPage[Cpt]) is TPdfImg
            then
              TPdfImg(PdfPage[Cpt]).Free;
PdfPage.Free;
DecimalSeparator:= OldSeparator;
inherited;
end;

procedure T_Report.BeginWrite(IniOrientation: TOrient= oPortrait; IniPaperType: TPaperType= A4;
          IniMeasure: TMeasureUnit= msMM; IniVersion: Char= 'F'; IniVisu: Boolean= True);
begin
FVersion:= IniVersion;
FOrientation:= IniOrientation;
FPaperType:= IniPaperType;
FMeasureUnit:= IniMeasure;
FPreparation:= ppPrepare;
FVisualization:= IniVisu;
PrepareFormat;
if IniVisu
then
  CreateVisu;
FCurrentFont:= -1;
FCurrentLineSpace:= -1;
FGroup:= False;
end;

procedure T_Report.EndWrite;
var
  Cpt: Integer;
begin
FPreparation:= ppPdfFile;
if Sections.Count> 0
then
  for Cpt:= 1 to Sections.Count do
    begin
    NumSection:= Cpt;
    if T_Section(Sections[Pred(NumSection)]).TotPages> 0
    then
      begin
      NumPageSection:= 1;
      NumPage:= 1;
      end;
    end
else
  Exit;
for Cpt:= 1 to T_Section(Sections[Pred(NumSection)]).TotPages do
  PrintPage(Cpt);
if FVisualization
then
  begin
  FPreparation:= ppVisualize;
  try
    WriteDocument;
    F_Visu.ShowModal;
  finally
    F_Visu.Free;
    end;
  end;
end;

procedure T_Report.WriteDocument;
begin
if FVisualization
then
  FCanvas:= Bv_Visu.Canvas;
end;

procedure T_Report.PagePreview;
begin
FVisualization:= not FVisualization;
if FVisualization
then
  FCanvas:= Bv_Visu.Canvas;
end;

procedure T_Report.Section(MgLeft,MgRight,MgTop,MgBottom: Single; BackPos: Single;
          IniOrientation: TOrient= oPortrait);
var
  CMargin: Single;
begin
case FPreparation of
  ppPrepare:
    begin
    FOrientation:= IniOrientation;
    PrepareFormat;
    with FCurrentMargin,FPaper do
      begin
      if Dim2Pixels(MgLeft)> Printable.L
      then
        L:= Dim2Pixels(MgLeft)
      else
        L:= Printable.L;
      if (W-Dim2Pixels(MgRight))< Printable.R
      then
        R:= W-Dim2Pixels(MgRight)
      else
        R:= Printable.R;
      if Dim2Pixels(MgTop)> Printable.T
      then
        T:= Dim2Pixels(MgTop)
      else
        T:= Printable.T;
      if (H-Dim2Pixels(MgBottom))< Printable.B
      then
        B:= H-Dim2Pixels(MgBottom)
      else
        B:= Printable.B;
      end;
    FPosRef.X:= FCurrentMargin.L;
    FHeaderHeight:= 0;
    FPageHeight:= 0;
    FFooterHeight:= 0;
    NumSection:= NumSection+1;
    VSection:= T_Section.Create(FPaper,FCurrentMargin,NumSection);
    Sections.Add(VSection);
    CMargin:= Dim2Pixels(BackPos);
    VColumn:= T_Column.Create(FCurrentMargin.L,FCurrentMargin.R-FCurrentMargin.L,CMargin,clWhite);
    T_Section(Sections[Pred(Sections.Count)]).Columns.Add(VColumn);
    end;
  end;
end;

procedure T_Report.Page;
begin
if FPreparation= ppPrepare
then
  begin
  NumPage:= NumPage+1;
  T_Section(Sections[Pred(Sections.Count)]).LoadPage(NumPage);
  FPosRef.Y:= FCurrentMargin.T+FHeaderHeight;
  FPageHeight:= 0;
  end;
end;

function T_Report.BackColor(FdColor: TfpgColor): Integer;
begin
VBackColor:= T_BackColor.Create(FdColor);
Result:= BackColors.Add(VBackColor);
end;

function T_Report.Font(FtNom: string; FtColor: TfpgColor): Integer;
begin
VFont:= T_Font.Create(FtNom,FtColor);
Result:= Fonts.Add(VFont);
end;

function T_Report.LineStyle(StThick: Single; StColor: Tfpgcolor; StStyle: TfpgLineStyle): Integer;
begin
VLineStyle:= T_LineStyle.Create(StThick,StColor,StStyle);
Result:= LineStyles.Add(VLineStyle);
end;

function T_Report.Border(BdFlags: TBorderFlags; BdStyle: Integer): Integer;
begin
VBorder:= T_Border.Create(BdFlags,BdStyle);
Result:= Borders.Add(VBorder);
end;

//function T_Report.Border(BdFlags: TBorderFlags; StFlags: array of Integer): Integer;
//begin
//VBorder:= T_Border.Create(BdFlags,BdStyle);
//Result:= Borders.Add(VBorder);
//end;

function T_Report.Column(ClnPos,ClnWidth: Single; ClnMargin: Single= 0; ClnColor: TfpgColor= clWhite): Integer;
var
  CPos,CWidth,CMargin: Single;
begin
CPos:= Dim2Pixels(ClnPos);
with T_Section(Sections[Pred(NumSection)]) do
  begin
  if CPos< Margins.L
  then
    CPos:= Margins.L;
  CWidth:= Dim2Pixels(ClnWidth);
  if CWidth> (Margins.R-Margins.L)
  then
    CWidth:= Margins.R-Margins.L;
  end;
CMargin:= Dim2Pixels(ClnMargin);
VColumn:= T_Column.Create(CPos,CWidth,CMargin,ClnColor);
Result:= T_Section(Sections[Pred(Sections.Count)]).Columns.Add(VColumn);
end;

procedure T_Report.WriteHeader(Horiz,Verti: Single; Text: string; ColNum: Integer= 0; FontNum: Integer= 0;
          LineSpNum: Integer= 0; BkColorNum: Integer= -1; BordNum: Integer= -1);
var
  RefText: Integer;
  Flags: TfpgTextFlags;
begin
Flags:= [txtWrap];
if Horiz< 0
then
  begin
  if Horiz= cnLeft
  then
    Include(Flags,txtLeft);
  if Horiz= cnCenter
  then
    Include(Flags,txtHCenter);
  if Horiz= cnRight
  then
    Include(Flags,txtRight);
  end
else
  Horiz:= Dim2Pixels(Horiz);
if Verti> 0
then
  Verti:= Dim2Pixels(Verti);
RefText:= Texts.IndexOf(Text);
if RefText= -1
then
  RefText:= Texts.Add(Text);
WriteText(Horiz,Verti,ColNum,RefText,FontNum,BkColorNum,BordNum,LineSpNum,Flags,zHeader);
end;

function T_Report.WritePage(Horiz,Verti: Single; Text: string; ColNum: Integer= 0; FontNum: Integer= 0;
          LineSpNum: Integer= 0; BkColorNum: Integer= -1; BordNum: Integer= -1): Single;
var
  RefText: Integer;
  Flags: TfpgTextFlags;
begin
Flags:= [txtWrap];
if Horiz< 0
then
  begin
  if Horiz= cnLeft
  then
    Include(Flags,txtLeft);
  if Horiz= cnCenter
  then
    Include(Flags,txtHCenter);
  if Horiz= cnRight
  then
    Include(Flags,txtRight);
  end
else
  Horiz:= Dim2Pixels(Horiz);
if Verti> 0
then
  Verti:= Dim2Pixels(Verti);
RefText:= Texts.IndexOf(Text);
if RefText= -1
then
  RefText:= Texts.Add(Text);
Result:= WriteText(Horiz,Verti,ColNum,RefText,FontNum,BkColorNum,BordNum,LineSpNum,Flags,ZPage);
end;

procedure T_Report.WriteFooter(Horiz,Verti: Single; Text: string; ColNum: Integer= 0; FontNum: Integer= 0;
          LineSpNum: Integer= 0; BkColorNum: Integer= -1; BordNum: Integer= -1);
var
  RefText: Integer;
  Flags: TfpgTextFlags;
begin
Flags:= [txtWrap];
if Horiz< 0
then
  begin
  if Horiz= cnLeft
  then
    Include(Flags,txtLeft);
  if Horiz= cnCenter
  then
    Include(Flags,txtHCenter);
  if Horiz= cnRight
  then
    Include(Flags,txtRight);
  end
else
  Horiz:= Dim2Pixels(Horiz);
if Verti> 0
then
  Verti:= Dim2Pixels(Verti);
RefText:= Texts.IndexOf(Text);
if RefText= -1
then
  RefText:= Texts.Add(Text);
WriteText(Horiz,Verti,ColNum,RefText,FontNum,BkColorNum,BordNum,LineSpNum,Flags,zFooter);
end;

procedure T_Report.NumSectionHeader(Horiz,Verti: Single; TexteSect: string= ''; TextTot: string= '';
          Total: Boolean= False; Alpha: Boolean= False; ColNum: Integer= 0; FontNum: Integer= 0;
          LineSpNum: Integer= 0; BkColorNum: Integer= -1; BordNum: Integer= -1);
var
  RefTextPage,RefTextTot: Integer;
  Flags: TfpgTextFlags;
begin
Flags:= [];
if Horiz< 0
then
  begin
  if Horiz= cnLeft
  then
    Include(Flags,txtLeft);
  if Horiz= cnCenter
  then
    Include(Flags,txtHCenter);
  if Horiz= cnRight
  then
    Include(Flags,txtRight);
  end
else
  Horiz:= Dim2Pixels(Horiz);
if Verti> 0
then
  Verti:= Dim2Pixels(Verti);
RefTextPage:= Texts.IndexOf(TexteSect);
if RefTextPage= -1
then
  RefTextPage:= Texts.Add(TexteSect);
RefTextTot:= Texts.IndexOf(TextTot);
if RefTextTot= -1
then
  RefTextTot:= Texts.Add(TextTot);
WriteNumber(Horiz,Verti,ColNum,RefTextPage,RefTextTot,FontNum,BkColorNum,BordNum,LineSpNum,Flags,Total,Alpha,zHeader,SectNum);
end;

procedure T_Report.NumSectionFooter(Horiz,Verti: Single; TexteSect: string= ''; TextTot: string= '';
          Total: Boolean= False; Alpha: Boolean= False; ColNum: Integer= 0; FontNum: Integer= 0;
          LineSpNum: Integer= 0;BkColorNum: Integer= -1; BordNum: Integer= -1);
var
  RefTextPage,RefTextTot: Integer;
  Flags: TfpgTextFlags;
begin
Flags:= [];
if Horiz< 0
then
  begin
  if Horiz= cnLeft
  then
    Include(Flags,txtLeft);
  if Horiz= cnCenter
  then
    Include(Flags,txtHCenter);
  if Horiz= cnRight
  then
    Include(Flags,txtRight);
  end
else
  Horiz:= Dim2Pixels(Horiz);
if Verti> 0
then
  Verti:= Dim2Pixels(Verti);
RefTextPage:= Texts.IndexOf(TexteSect);
if RefTextPage= -1
then
  RefTextPage:= Texts.Add(TexteSect);
RefTextTot:= Texts.IndexOf(TextTot);
if RefTextTot= -1
then
  RefTextTot:= Texts.Add(TextTot);
WriteNumber(Horiz,Verti,ColNum,RefTextPage,RefTextTot,FontNum,BkColorNum,BordNum,LineSpNum,Flags,Total,Alpha,zFooter,SectNum);
end;

procedure T_Report.NumPageHeader(Horiz,Verti: Single; TextePage: string= ''; TextTot: string= '';
          Total: Boolean= False; ColNum: Integer= 0; FontNum: Integer= 0; LineSpNum: Integer= 0;
          BkColorNum: Integer= -1; BordNum: Integer= -1);
var
  RefTextPage,RefTextTot: Integer;
  Flags: TfpgTextFlags;
begin
Flags:= [];
if Horiz< 0
then
  begin
  if Horiz= cnLeft
  then
    Include(Flags,txtLeft);
  if Horiz= cnCenter
  then
    Include(Flags,txtHCenter);
  if Horiz= cnRight
  then
    Include(Flags,txtRight);
  end
else
  Horiz:= Dim2Pixels(Horiz);
if Verti> 0
then
  Verti:= Dim2Pixels(Verti);
RefTextPage:= Texts.IndexOf(TextePage);
if RefTextPage= -1
then
  RefTextPage:= Texts.Add(TextePage);
RefTextTot:= Texts.IndexOf(TextTot);
if RefTextTot= -1
then
  RefTextTot:= Texts.Add(TextTot);
WriteNumber(Horiz,Verti,ColNum,RefTextPage,RefTextTot,FontNum,BkColorNum,BordNum,LineSpNum,Flags,Total,False,zHeader,PageNum);
end;

procedure T_Report.NumPageFooter(Horiz,Verti: Single; TextePage: string= ''; TextTot: string= '';
          Total: Boolean= False; ColNum: Integer= 0; FontNum: Integer= 0; LineSpNum: Integer= 0;
          BkColorNum: Integer= -1; BordNum: Integer= -1);
var
  RefTextPage,RefTextTot: Integer;
  Flags: TfpgTextFlags;
begin
Flags:= [];
if Horiz< 0
then
  begin
  if Horiz= cnLeft
  then
    Include(Flags,txtLeft);
  if Horiz= cnCenter
  then
    Include(Flags,txtHCenter);
  if Horiz= cnRight
  then
    Include(Flags,txtRight);
  end
else
  Horiz:= Dim2Pixels(Horiz);
if Verti> 0
then
  Verti:= Dim2Pixels(Verti);
RefTextPage:= Texts.IndexOf(TextePage);
if RefTextPage= -1
then
  RefTextPage:= Texts.Add(TextePage);
RefTextTot:= Texts.IndexOf(TextTot);
if RefTextTot= -1
then
  RefTextTot:= Texts.Add(TextTot);
WriteNumber(Horiz,Verti,ColNum,RefTextPage,RefTextTot,FontNum,BkColorNum,BordNum,LineSpNum,Flags,Total,False,zFooter,PageNum);
end;

procedure T_Report.NumPageSectionHeader(Horiz,Verti: Single; TexteSect: string= ''; TextTot: string= '';
          Total: Boolean= False; Alpha: Boolean= False; ColNum: Integer= 0; FontNum: Integer= 0;
          LineSpNum: Integer= 0; BkColorNum: Integer= -1; BordNum: Integer= -1);
var
  RefTextPage,RefTextTot: Integer;
  Flags: TfpgTextFlags;
begin
Flags:= [];
if Horiz< 0
then
  begin
  if Horiz= cnLeft
  then
    Include(Flags,txtLeft);
  if Horiz= cnCenter
  then
    Include(Flags,txtHCenter);
  if Horiz= cnRight
  then
    Include(Flags,txtRight);
  end
else
  Horiz:= Dim2Pixels(Horiz);
if Verti> 0
then
  Verti:= Dim2Pixels(Verti);
RefTextPage:= Texts.IndexOf(TexteSect);
if RefTextPage= -1
then
  RefTextPage:= Texts.Add(TexteSect);
RefTextTot:= Texts.IndexOf(TextTot);
if RefTextTot= -1
then
  RefTextTot:= Texts.Add(TextTot);
WriteNumber(Horiz,Verti,ColNum,RefTextPage,RefTextTot,FontNum,BkColorNum,BordNum,LineSpNum,Flags,Total,Alpha,zHeader,PSectNum);
end;

procedure T_Report.NumPageSectionFooter(Horiz,Verti: Single; TexteSect: string= ''; TextTot: string= '';
          Total: Boolean= False; Alpha: Boolean= False; ColNum: Integer= 0; FontNum: Integer= 0;
          LineSpNum: Integer= 0; BkColorNum: Integer= -1; BordNum: Integer= -1);
var
  RefTextPage,RefTextTot: Integer;
  Flags: TfpgTextFlags;
begin
Flags:= [];
if Horiz< 0
then
  begin
  if Horiz= cnLeft
  then
    Include(Flags,txtLeft);
  if Horiz= cnCenter
  then
    Include(Flags,txtHCenter);
  if Horiz= cnRight
  then
    Include(Flags,txtRight);
  end
else
  Horiz:= Dim2Pixels(Horiz);
if Verti> 0
then
  Verti:= Dim2Pixels(Verti);
RefTextPage:= Texts.IndexOf(TexteSect);
if RefTextPage= -1
then
  RefTextPage:= Texts.Add(TexteSect);
RefTextTot:= Texts.IndexOf(TextTot);
if RefTextTot= -1
then
  RefTextTot:= Texts.Add(TextTot);
WriteNumber(Horiz,Verti,ColNum,RefTextPage,RefTextTot,FontNum,BkColorNum,BordNum,LineSpNum,Flags,Total,Alpha,zFooter,PSectNum);
end;

procedure T_Report.HorizLineHeader(SpBefore,SpAfter: Single; ColNum: Integer= 0; StyleNum: Integer= 0);
begin
DrawAHorizLine(Dim2Pixels(SpBefore),Dim2Pixels(SpAfter),ColNum,-1,StyleNum,zHeader);
end;

procedure T_Report.HorizLinePage(SpBefore,SpAfter: Single; ColNum: Integer= 0; StyleNum: Integer= 0);
begin
DrawAHorizLine(Dim2Pixels(SpBefore),Dim2Pixels(SpAfter),ColNum,-1,StyleNum,zPage);
end;

procedure T_Report.HorizLineFooter(SpBefore,SpAfter: Single; ColNum: Integer= 0; StyleNum: Integer= 0);
begin
DrawAHorizLine(Dim2Pixels(SpBefore),Dim2Pixels(SpAfter),ColNum,-1,StyleNum,zFooter);
end;

procedure T_Report.SpaceHeader(Verti: Single; ColNum: Integer=0; BkColorNum: Integer= -1);
begin
InsertSpace(-1,ColNum,Dim2Pixels(Verti),BkColorNum,zHeader);
end;

procedure T_Report.SpacePage(Verti: Single; ColNum: Integer=0; BkColorNum: Integer= -1);
begin
InsertSpace(-1,ColNum,Dim2Pixels(Verti),BkColorNum,zPage);
end;

procedure T_Report.SpaceFooter(Verti: Single; ColNum: Integer=0; BkColorNum: Integer= -1);
begin
InsertSpace(-1,ColNum,Dim2Pixels(Verti),BkColorNum,zFooter);
end;

function T_Report.LineSpace(SpSup,SpInt,SpInf: Single): Integer;
var
  Sup,Int,Inf: Integer;
begin
if SpSup> 0
then
  Sup:= Round(Dim2Pixels(SpSup))
else
  Sup:= 0;
if SpInt> 0
then
  Int:= Round(Dim2Pixels(SpInt))
else
  Int:= 0;
if SpInf> 0
then
  Inf:= Round(Dim2Pixels(SpInf))
else
  Inf:= 0;
VLineSpace:= T_LineSpace.Create(Sup,Int,Inf);
Result:= LineSpaces.Add(VLineSpace);
end;

procedure T_Report.BeginGroup(PageJump: Boolean= False);
begin
VGroup:= T_Group.Create;
FGroup:= True;
if PageJump
then
  Page;
end;

procedure T_Report.EndGroup(PageJump: Boolean= False);
begin
T_Section(Sections[Pred(Sections.Count)]).LoadCmdGroupToPage;
FGroup:= False;
VGroup.Free;
if PageJump
then
  Page;
end;

procedure T_Report.ColorColChange(ColNum: Integer; ColColor: TfpgColor);
begin
T_Column(T_Section(Sections[Pred(Sections.Count)]).Columns[ColNum]).SetColColor(ColColor);
end;

procedure T_Report.FrameMargins(AStyle: Integer);
begin
DrawAFrame(AStyle,zMargins);
end;

procedure T_Report.FrameHeader(AStyle: Integer);
begin
DrawAFrame(AStyle,zHeader);
end;

procedure T_Report.FramePage(AStyle: Integer);
begin
DrawAFrame(AStyle,zPage);
end;

procedure T_Report.FrameFooter(AStyle: Integer);
begin
DrawAFrame(AStyle,zFooter);
end;

procedure T_Report.LinePage(XBegin,YBegin,XEnd,YEnd: Single; AStyle: Integer);
begin
DrawALine(Dim2Pixels(XBegin),Dim2Pixels(YBegin),Dim2Pixels(XEnd),Dim2Pixels(YEnd),AStyle);
end;

procedure T_Report.SurfPage(XLimits,YLimits: array of Single; AColor: TfpgColor);
var
  Size,Cpt: Integer;
  Ends: array of TRefPos;
begin
if Length(XLimits)< Length(YLimits)
then
  Size:= Length(XLimits)
else
  if Length(XLimits)> Length(YLimits)
  then
    Size:= Length(YLimits)
  else
    Size:= Length(XLimits);
SetLength(Ends,Size);
for Cpt:= 0 to Pred(Size) do
  begin
  Ends[Cpt].X:= Dim2Pixels(XLimits[Cpt]);
  Ends[Cpt].Y:= Dim2Pixels(YLimits[Cpt]);
  end;
PaintSurface(Ends,AColor);
end;

procedure T_Report.ImageHeader(Horiz,Verti: Single; ImgFileName: string; ColNum,Scale: Integer);
var
  RefImage: Integer;
  Image: TfpgImage;
begin
Horiz:= Dim2Pixels(Horiz);
Verti:= Dim2Pixels(Verti);
if FileExists(ImgFileName)
then
  begin
  RefImage:= ImageNames.IndexOf(IntToStr(Scale)+ImgFileName);
  if RefImage= -1
  then
    begin
    if Copy(ImgFileName,Succ(Pos('.',ImgFileName)),3)= 'bmp'
    then
      begin
      Image:= LoadImage_BMP(ImgFileName);
      Scale:= 1;
      end;
    if (Copy(ImgFileName,Succ(Pos('.',ImgFileName)),3)= 'jpg') or (Copy(ImgFileName,Succ(Pos('.',ImgFileName)),4)= 'jpeg')
    then
      Image:= LoadImage_JPG(ImgFileName,Scale);
    RefImage:= Images.Add(Image);
    end;
  PaintImage(Horiz,Verti,ColNum,RefImage,zHeader);
  end
else
  ShowMessage('Image '+ImgFileName+' is missing');
end;

procedure T_Report.ImagePage(Horiz,Verti: Single; ImgFileName: string; ColNum,Scale: Integer);
var
  RefImage: Integer;
  Image: TfpgImage;
begin
Horiz:= Dim2Pixels(Horiz);
Verti:= Dim2Pixels(Verti);
if FileExists(ImgFileName)
then
  begin
  RefImage:= ImageNames.IndexOf(IntToStr(Scale)+ImgFileName);
  if RefImage= -1
  then
    begin
    if Copy(ImgFileName,Succ(Pos('.',ImgFileName)),3)= 'bmp'
    then
      begin
      Image:= LoadImage_BMP(ImgFileName);
      Scale:= 1;
      end;
    if (Copy(ImgFileName,Succ(Pos('.',ImgFileName)),3)= 'jpg') or (Copy(ImgFileName,Succ(Pos('.',ImgFileName)),4)= 'jpeg')
    then
      Image:= LoadImage_JPG(ImgFileName,Scale);
    RefImage:= ImageNames.Add(IntToStr(Scale)+ImgFileName);
    Images.Add(Image);
    end;
  PaintImage(Horiz,Verti,ColNum,RefImage,zPage);
  end
else
  ShowMessage('Image '+ImgFileName+' is missing');
end;

procedure T_Report.ImageFooter(Horiz,Verti: Single; ImgFileName: string; ColNum,Scale: Integer);
var
  RefImage: Integer;
  Image: TfpgImage;
begin
Horiz:= Dim2Pixels(Horiz);
Verti:= Dim2Pixels(Verti);
if FileExists(ImgFileName)
then
  begin
  RefImage:= ImageNames.IndexOf(IntToStr(Scale)+ImgFileName);
  if RefImage= -1
  then
    begin
    if Copy(ImgFileName,Succ(Pos('.',ImgFileName)),3)= 'bmp'
    then
      begin
      Image:= LoadImage_BMP(ImgFileName);
      Scale:= 1;
      end;
    if (Copy(ImgFileName,Succ(Pos('.',ImgFileName)),3)= 'jpg') or (Copy(ImgFileName,Succ(Pos('.',ImgFileName)),4)= 'jpeg')
    then
      Image:= LoadImage_JPG(ImgFileName,Scale);
    RefImage:= Images.Add(Image);
    end;
  PaintImage(Horiz,Verti,ColNum,RefImage,zFooter);
  end
else
  ShowMessage('Image '+ImgFileName+' is missing');
end;

end.

