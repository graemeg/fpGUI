unit fpreporthtmlutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpimage, fpreport, dom, dom_html;

Type

  TStyleEmbedding = (seInline,seStyleTag,seCSSFile);
  TTOCPosition = (tpLeft,tpRight,tpTop,tpBottom);

  TNavigatorPosition = (npLeft,npRight,npTop,npBottom);
  TNavigatorPositions = set of TNavigatorPosition;
  THTMLNavigatorOption = (hnoFirstLast,hnoAlwaysFirstLast,hnoPageNo,hnoImage,hnoSkipStyling,hnoUsePageNOfM,hnoPageNoEdit);
  THTMLNavigatorOptions = set of THTMLNavigatorOption;
  TNavigatorButton = (nbFirst,nbPrevious,nbPage,nbNext,nbLast);
  TNavigatorButtons = set of TNavigatorButton;

  { TGenerateHTMLContext }

  TGenerateHTMLContext = Class(TObject)
  private
    FDPI: Integer;
    FIDCount : Integer;
    FBaseFileName: String;
    FCSSDir: String;
    FCurrentPageNo: Integer;
    FReport: TFPCustomReport;
    FDoc: THTMLDocument;
    FSequenceDigits: Integer;
    FSequenceFormat: String;
    FStyleContent: TStrings;
    FTotalPageCount: Integer;
    FRunOffsetX : Integer;
    FRunOffsetY : Integer;
  Public
    Constructor Create(AReport : TFPCustomReport; ADoc : THTMLDocument; ADPI : Integer);
    Procedure Resetpage(ADoc : THTMLDocument);
    function AllocateID(aElement: THTMLElement; AReportElement: TFPReportElement): String;
    function CreateDiv(aParent: TDOMElement; AID: String): THTMLElement;
    function CreateDiv(aParent: TDOMElement; AReportElement: TFPReportElement): THTMLElement;
    procedure ApplyStyle(aElement: THTMLElement; const aStyle: TStrings);
    procedure ApplyStyle(aElement: THTMLElement; const aStyle: String);
    Procedure InitOffsets(aX,aY : Integer);
    function mmToPixels(const AValue: TFPReportUnits): Integer;
    class function ColorToRGBString(AColor: TFPReportColor; UseHex: Boolean=True): String;
    function AllocatePageName(APageNo: Integer): string;
    Property Report : TFPCustomReport Read FReport;
    Property BaseFileName : String Read FBaseFileName Write FBaseFileName;
    Property CSSDir : String Read FCSSDir Write FCSSDir;
    Property CurrentPageNo : Integer Read FCurrentPageNo Write FCurrentPageNo;
    Property TotalPageCount : Integer Read FTotalPageCount Write FTotalPageCount;
    Property SequenceFormat : String Read FSequenceFormat Write FSequenceFormat;
    Property SequenceDigits : Integer Read FSequenceDigits Write FSequenceDigits;
    Property StyleContent : TStrings Read FStyleContent Write FStyleContent;
    Property Doc : THTMlDocument Read FDoc;
    Property DPI : Integer Read FDPI;
    Property RunOffsetX : Integer Read FRunOffsetX;
    Property RunOffsetY : Integer Read FRunOffsetY;

  end;

  { TTOCPageOptions }

  TTOCPageOptions  = Class(TPersistent)
  private
    FCSSFileName: string;
    FEvenPageStyle: string;
    FFileName: string;
    FOddPageStyle: string;
    FSkipStyling: Boolean;
  Public
    Procedure Assign(Source : TPersistent) ; override;
  Published
    // TOC page filename.
    // If empty, 'index.html' is used, unless used for frame page , then 'toc.html' is used.
    // TOC CSS page filename.
    Property FileName : string read FFileName write FFileName;
    // Frame page CSS filename. If empty, no <link> is added. Relative to CSSDir
    property CSSFileName: string read FCSSFileName write FCSSFileName;
    // Odd page style elements.
    property OddPageStyle : string read FOddPageStyle write FOddPageStyle;
    // Even page style elements.
    property EvenPageStyle : string read FEvenPageStyle write FEvenPageStyle;
    // Skip styling alltogether
    Property SkipStyling : Boolean Read FSkipStyling Write FSkipStyling;
  end;

  TFramePageOptions  = Class(TPersistent)
  private
    FCSSFileName: string;
    FFileName: string;
    FTOCZonePosition: TTOCPosition;
    FTOCZoneSize: Integer;
  Public
    Procedure Assign(Source : TPersistent) ; override;
  Published
    // Frame page filename. If not set, it will be 'index.html'
    Property FileName : string read FFileName write FFileName;
    // Frame page CSS filename. If empty, no <link> is added. Relative to CSSDir
    property CSSFileName: string read FCSSFileName write FCSSFileName;
    // Size, in percentage, of the TOC zone
    Property TOCZoneSize : Integer Read FTOCZoneSize Write FTOCZoneSize;
    // Position of TOC zone
    Property TOCZonePosition : TTOCPosition Read FTOCZonePosition Write FTOCZonePosition;
  end;

  { TPageNavigatorOptions }

  TPageNavigatorOptions = class(TPersistent)
  private
    FActiveBGColor: TFPReportColor;
    FFixedHeight: Integer;
    FFixedMargin: Integer;
    FFixedWidth: Integer;
    FInActiveBGColor: TFPReportColor;
    FOptions: THTMLNavigatorOptions;
    FPositions: TNavigatorPositions;
  Public
    Procedure Assign(Source : TPersistent) ; override;
  Published
    Property Positions : TNavigatorPositions Read FPositions Write FPositions;
    Property Options : THTMLNavigatorOptions Read FOptions Write FOptions;
    Property FixedWidth : Integer Read FFixedWidth Write FFixedWidth;
    Property FixedHeight : Integer Read FFixedHeight Write FFixedHeight;
    Property FixedMargin : Integer Read FFixedMargin Write FFixedMargin;
    Property ActiveBGColor : TFPReportColor Read FActiveBGColor Write FActiveBGColor;
    Property InActiveBGColor : TFPReportColor Read FInActiveBGColor Write FInActiveBGColor;
  end;

  { TTOCPageCreator }

  TTOCPageCreator = class
  private
    FContext: TGenerateHTMLContext;
    FTOCPage: TTocPageOptions;
    FDoc : THTMLDocument;
    function GetReport: TFPCustomReport;
  Protected
    function CreatePageLink(APageName: String; APageNo: Integer; ADoc: THTMLDocument; ForFrame: Boolean): THTMLElement; virtual;
  Public
    Constructor Create(AContext : TGenerateHTMLContext; AOptions : TTocPageOptions); virtual;
    Destructor Destroy; override;
    function CreateTOCPage(APageNames: TStrings; ForFrame: Boolean): String; virtual;
    Property Context : TGenerateHTMLContext read FContext;
    Property TOCPage : TTocPageOptions Read FTOCPage;
    Property Report : TFPCustomReport Read GetReport;
    Property Doc : THTMLDocument Read FDoc;
  end;

  { TTOCFramePageCreator }

  TTOCFramePageCreator = class
  private
    FFramePage: TFramePageOptions;
    FContext : TGenerateHTMLContext;
    FDoc : THTMLDocument;
    function GetCSSDIR: String;
    function GetReport: TFPCustomReport;
  Public
    Constructor Create(AContext : TGenerateHTMLContext; AOptions : TFramePageOptions); virtual;
    Destructor Destroy; override;
    procedure CreateFramePage(PageNames: TStrings; const ATOCPageFileName: String); virtual;
    procedure ConfigureFrameSet(FramesEl: THTMLElement); virtual;
    Property Context : TGenerateHTMLContext read FContext;
    Property FramePage : TFramePageOptions Read FFramePage;
    Property Report : TFPCustomReport Read GetReport;
    Property CSSDIR : String Read GetCSSDIR;
  end;

  { TPageNavigatorElementCreator }

  TPageNavigatorElementCreator = Class
  Private
    FContext : TGenerateHTMLContext;
    FCurrentPage: TFPReportPage;
    FPageNavigator : TPageNavigatorOptions;
    function GetDoc: THTMLDocument;
  Protected
    function StyleNavigatorButton(aPosition: TNavigatorPosition; AButton: TNavigatorButton; ActiveLink: Boolean): String; virtual;
    procedure CreatePageNoNavigatorElement(AParentDiV: THTMLElement; aPosition: TNavigatorPosition); virtual;
    procedure ApplyStyle(aElement: THTMLElement; const aStyle: TStrings); inline;
    procedure ApplyStyle(aElement: THTMLElement; const aStyle: String); inline;
  Public
    Constructor Create(AContext : TGenerateHTMLContext; APage : TFPReportPage; AOptions : TPageNavigatorOptions); virtual;
    Procedure CreatePageNavigator(aPosition : TNavigatorPosition; AParentDiV : THTMLElement); virtual;
    procedure StyleNavigator(ND: THTMLElement; aPosition: TNavigatorPosition); virtual;
    class procedure WriteDefaultScript(AContext: TGenerateHTMLContext; AScript: TStrings);
    Property Context : TGenerateHTMLContext read FContext;
    Property Doc : THTMLDocument Read GetDoc;
    Property PageNavigator : TPageNavigatorOptions Read FPageNavigator;
    Property CurrentPage : TFPReportPage Read FCurrentPage;
  end;

Function Coalesce(S1,S2 : String) : String;
Function GetColorComponent(Var AColor: UInt32; Full : Boolean = true): Word; inline;
Function ColorToRGBTriple(const AColor: UInt32; Full : Boolean = true): TFPColor;
Function RGBTripleToColor(AColor : TFPColor) : UINT32;

Const
  cInchToMM = 25.4;
  PosStrings : Array[Boolean] of string = ('absolute','fixed');
  NavigatorPositionNames : Array[TNavigatorPosition] of string =
          ('Left','Right','Top','Bottom');

resourcestring
  SFirstPage = 'First';
  SPreviousPage = 'Previous';
  SLastPage = 'Last';
  SNextPage = 'Next';
  SCurrentPage = 'Page %d';
  SCurrentPageOf = 'Page %d of %d';
  STotalPages = 'total: %d';
  SNoFrameSupport = 'Your browser must support frames to view this';


implementation

uses htmwrite;

{ Auxiliary functions }

function GetColorComponent(Var AColor: UInt32; Full : Boolean): Word; inline;

begin
  Result:=(AColor and $FF);
  if Full then
    Result:=Result or (Result shl 8);
  AColor:=(AColor shr 8);
end;


function ColorToRGBTriple(const AColor: UInt32; Full : Boolean = True): TFPColor;

Var
  C : UInt32;

begin
  C:=AColor;
  with Result do
    begin
    Blue  := GetColorComponent(C,Full);
    Green := GetColorComponent(C,Full);
    Red   := GetColorComponent(C,Full);
    Alpha := GetColorComponent(C,Full);
    end
end;

Function RGBTripleToColor(AColor : TFPColor) : UINT32;

  Function BS(C : Word; Sh : Byte) : UINT32;
  begin
    Result:=C shr 8;
    If (Sh<>0) then
      Result:=Result shl SH
  end;

begin
  Result:=BS(AColor.blue,0) or BS(AColor.Green,8) or BS(AColor.Red,16) or BS(AColor.alpha,24);
end;

Function Coalesce(S1,S2 : String) : String;

begin
  if S1<>'' then
    Result:=S1
  else
    Result:=S2;
end;

{ TTOCPageCreator }

function TTOCPageCreator.GetReport: TFPCustomReport;
begin
  Result:=FContext.Report;
end;

constructor TTOCPageCreator.Create(AContext: TGenerateHTMLContext; AOptions: TTocPageOptions);
begin
  FContext:=AContext;
  FTOCPage:=AOptions;
  FDoc := THTMLDocument.Create;
  FDoc.AppendChild(FDoc.Impl.CreateDocumentType(
    'HTML', '-//W3C//DTD HTML 4.01 Frameset//EN"',
    'http://www.w3.org/TR/html4/loose.dtd'));
end;

destructor TTOCPageCreator.Destroy;
begin
  FreeAndNil(FDoc);
  inherited Destroy;
end;

function TTOCPageCreator.CreatePageLink(APageName : String;APageNo : Integer; ADoc : THTMLDocument; ForFrame : Boolean) : THTMLElement;

Const
  DefaultOddPageStyle = 'width: 72px; height:100px; background-color: rgb(255,241,215); border: solid; margin: 5px;';
  DefaultEvenPageStyle = 'width: 72px; height:100px; background-color: rgb(155,155,155); border: solid; margin: 5px;';

Var
  L : THTMLLinkElement;
  S,D : String;

begin
  Result:=ADoc.CreateElement('div');
  Result.ID:=format('page-%d',[APageNo]);
  if ((APageNo mod 2)=1) then
    begin
    S:=TOCPage.OddPageStyle;
    D:=DefaultOddPageStyle;
    end
  else
    begin
    S:=TOCPage.EvenPageStyle;
    D:=DefaultEvenPageStyle;
    end;
  if Not TOCPage.SkipStyling then
    begin
    S:=Coalesce(S,D);
    if (S<>'') then
      Result['style']:=S;
    end;
  L:=ADoc.CreateLinkElement;
  L.AppendChild(ADoc.CreateTextNode(Format('Page %d',[APAgeNo])));
  L.HRef:=ExtractFileName(APageName);
  if ForFrame then
    L.Target:='reportpage';
  Result.AppendChild(l);
end;

function TTOCPageCreator.CreateTOCPage(APageNames: TStrings; ForFrame: Boolean): String;
Var

  AHeadElement,ABodyElement,AHTMLEl,El,PEL : THTMLElement;
  D,FN : String;
  I : Integer;

begin
  AHTMLEl := FDoc.CreateHtmlElement;
  FDoc.AppendChild(AHTMLEl);
  AHeadElement:=FDoc.CreateHeadElement;
  AHTMLEl.AppendChild(AHeadElement);
  El := Doc.CreateElement('meta');
  El['http-equiv'] := 'Content-Type';
  El['content'] := 'text/html; charset=utf-8';
  AHeadElement.AppendChild(El);
  El := Doc.CreateElement('title');
  AHeadElement.AppendChild(el);
  el.AppendChild(Doc.CreateTextNode(Report.Title));
  if (TOCPage.CSSFileName<>'') then
    begin
    El:=Doc.CreateElement('link');
    El['rel']:='stylesheet';
    D:=Context.CSSDir;
    if (D<>'') then
      D:=D+'/'; // not pathdelim !
    El['href']:=D+TOCPage.CSSFileName;
    AHeadElement.AppendChild(El);
    end;
  ABodyElement := Doc.CreateElement('body');
  AHTMLEl.AppendChild(ABodyElement);
  PEL:=Doc.CreateElement('div');
  PEl.ID:='toctable';
  ABodyElement.AppendChild(pel);
  For I:=0 to APageNames.Count-1 do
    begin
    el:=CreatePageLink(APageNames[i],I+1,Doc,ForFrame);
    if I=0 then
      el.ClassName:=EL.ClassName+' '+'firstpage'
    else if I= (APageNames.Count-1) then
      EL.ClassName:=EL.ClassName+' '+'lastpage';
    PEL.AppendChild(el);
    end;
  FN:=TOCPage.FileName;
  if (FN='') then
    if ForFrame then
      FN:='toc.html'
    else
      FN:='index.html';
  D:=ExtractFilePath(Context.BaseFileName);
  FN:=D+FN;
  WriteHTML(Doc,FN);
  Result:=FN;
end;


{ TGenerateHTMLContext }

function TGenerateHTMLContext.mmToPixels(const AValue: TFPReportUnits): Integer;
begin
  Result := Round(AValue * (DPI / cInchToMM));
end;

function TGenerateHTMLContext.AllocateID(aElement: THTMLElement; AReportElement: TFPReportElement): String;

Var
  N : String;

begin
  if (AElement.ID<>'') then
    exit(AElement.ID);
  if AReportElement<>Nil then
    N:=AReportElement.ClassName
  else if AElement.TagName='div' then
    N:='div'
  else
    N:='el';
  Result:=N+'-'+IntToStr(FIDCount);
  AElement.ID:=Result
end;


function TGenerateHTMLContext.CreateDiv(aParent: TDOMElement; AReportElement: TFPReportElement): THTMLElement;

begin
  inc(FIDCount);
  Result:=FDoc.CreateElement('div');
  AllocateID(Result,AReportElement);
  if (AParent<>Nil) then
    AParent.AppendChild(Result);
end;

function TGenerateHTMLContext.CreateDiv(aParent: TDOMElement; AID: String): THTMLElement;

begin
  Result:=FDoc.CreateElement('div');
  Result.ID:=AID;
  if (AParent<>Nil) then
    AParent.AppendChild(Result);
end;

constructor TGenerateHTMLContext.Create(AReport: TFPCustomReport; ADoc: THTMLDocument; ADPI: Integer);
begin
  FReport:=AReport;
  FDoc:=ADoc;
  FIDCount:=0;
  FDPI:=ADPI;
end;

procedure TGenerateHTMLContext.Resetpage(ADoc: THTMLDocument);
begin
  FDoc:=ADoc;
  FIDCount:=0;
end;

function TGenerateHTMLContext.AllocatePageName(APageNo: Integer): string;

Var
  E,SN : String;
begin
  SN:=Format(SequenceFormat,[APageNo]);
  E:=ExtractFileExt(BaseFileName);
  Result:=ChangeFileExt(BaseFileName,SN+E);
end;

procedure TGenerateHTMLContext.ApplyStyle(aElement: THTMLElement; const aStyle: TStrings);

Var
  aID : String;

begin
  if Not Assigned(FStyleContent) then
    begin
    aStyle.LineBreak:=' ';
    AElement['style']:=aStyle.text
    end
  else
    begin
    AID:=AllocateID(AElement,nil);
    FStyleContent.Add('#'+aID+' {');
    FStyleContent.AddStrings(aStyle);
    FStyleContent.Add('}');
    end
end;

procedure TGenerateHTMLContext.ApplyStyle(aElement: THTMLElement;
  const aStyle: String);

Var
  aID : String;

begin
  if Not Assigned(FStyleContent) then
    AElement['style']:=aStyle
  else
    begin
    AID:=AllocateID(AElement,Nil);
    FStyleContent.Add('#'+aID+' {');
    FStyleContent.Add(aStyle);
    FStyleContent.Add('}');
    end
end;

procedure TGenerateHTMLContext.InitOffsets(aX, aY: Integer);
begin
  FRunOffsetX:=aX;
  FRunOffsetY:=aY;
end;

class function TGenerateHTMLContext.ColorToRGBString(AColor: TFPReportColor;
  UseHex: Boolean): String;

Var
  S : TFPColor;
begin
  S:=ColorToRGBTriple(aColor,False);
  if AColor=clNone then
    exit;
  if UseHex then
    Result:=Format('#%.2x,%.2x,%2.x);',[S.red,S.green,S.Blue])
  else
    Result:=Format('rgb(%d,%d,%d);',[S.red,S.green,S.Blue]);
end;

{ TPageNavigatorElementCreator }

function TPageNavigatorElementCreator.GetDoc: THTMLDocument;
begin
  Result:=Context.Doc;
end;

procedure TPageNavigatorElementCreator.ApplyStyle(aElement: THTMLElement; const aStyle: TStrings);
begin
  FContext.ApplyStyle(aElement,AStyle);
end;

procedure TPageNavigatorElementCreator.ApplyStyle(aElement: THTMLElement; const aStyle: String);
begin
  FContext.ApplyStyle(aElement,AStyle);
end;

constructor TPageNavigatorElementCreator.Create(AContext: TGenerateHTMLContext; APage : TFPReportPage; AOptions: TPageNavigatorOptions);
begin
  FContext:=AContext;
  FPageNavigator:=AOptions;
  FCurrentPage:=APage;
end;

procedure TPageNavigatorElementCreator.CreatePageNavigator(aPosition: TNavigatorPosition; AParentDiV: THTMLElement);

Var
  ND,N : THTMLElement;
  BN : String;
  ActiveLink : Boolean;

  Function PageLinkNode(Target : Integer; AText : String; Out IsActive: Boolean) : THTMLElement;

  var
    L : THTMLLinkElement;

  begin
    if (Target<1) or (Target=Context.CurrentPageNo) or (Target>Context.TotalPageCount) then
      begin
      Result:=Doc.CreateSpanElement;
      Result.ID:=ND.ID+'-deadlink';
      IsActive:=False;
      end
    else
      begin
      L:=Doc.CreateLinkElement;
      L.HRef:=ExtractFileName(Context.AllocatePageName(Target));
      L.ID:=ND.ID+'-link';
      IsActive:=True;
      Result:=L;
      end;
    Result.AppendChild(Doc.CreateTextNode(AText));
  end;

begin
  BN:='navigator-'+NavigatorPositionNames[aPosition];
  ND:=Context.CreateDiv(aParentDIV,BN);
  ND.ClassName:='navigator '+NavigatorPositionNames[aPosition]+'navigation';
  if (Context.CurrentPageNo>1) or (hnoAlwaysFirstLast in PageNavigator.Options) then
    begin
    if (hnoFirstLast in PageNavigator.Options) then
      begin
      N:=Context.CreateDiv(nd,bn+'first');
      N.AppendChild(PageLinkNode(1,SFirstPage,ActiveLink));
      ApplyStyle(N,StyleNavigatorButton(aPosition,nbFirst,ActiveLink));
      end;
    N:=Context.CreateDiv(nd,bn+'prev');
    N.AppendChild(PageLinkNode(Context.CurrentPageNo-1,SPreviousPage,ActiveLink));
    ApplyStyle(N,StyleNavigatorButton(aPosition,nbPrevious,ActiveLink));
    end;
  if hnoPageNo in PageNavigator.Options then
    begin
    N:=Context.CreateDiv(ND,bn+'pageno');
    CreatePageNoNavigatorElement(N,aPosition);
    ApplyStyle(N,StyleNavigatorButton(aPosition,nbPage,True));
    end;
  if (Context.CurrentPageNo<Context.TotalPageCount) or (hnoAlwaysFirstLast in PageNavigator.Options) then
    begin
    N:=Context.CreateDiv(nd,bn+'next');
    N.AppendChild(PageLinkNode(Context.CurrentPageNo+1,SNextPage,ActiveLink));
    ApplyStyle(N,StyleNavigatorButton(aPosition,nbNext,ActiveLink));
    if (hnoFirstLast in PageNavigator.Options) then
      begin
      N:=Context.CreateDiv(nd,bn+'last');
      N.AppendChild(PageLinkNode(Context.TotalPageCount,SLastPage,ActiveLink));
      ApplyStyle(N,StyleNavigatorButton(aPosition,nbLast,ActiveLink));
      end
    end;
  If not (hnoSkipStyling in PageNavigator.Options) then
    StyleNavigator(ND,aPosition);
end;

procedure TPageNavigatorElementCreator.StyleNavigator(ND : THTMLElement; aPosition : TNavigatorPosition);

Var
  S : String;

begin
    begin
    if (aPosition in [npTop,npBottom]) then
      begin
      S:='position: absolute; display: flex; width: 100%; align-items: center; justify-content: center; ';
      S:=S+Format('height: %dpx;',[PageNavigator.FixedHeight]);
      if (aPosition = npBottom) then
        S:=S+Format('top: %dpx;',[Context.RunOffsetY+Context.mmToPixels(CurrentPage.PageSize.Height)]);
      ApplyStyle(ND,S);
      end;
    if (aPosition in [npLeft,npRight]) then
      begin
      S:='position: absolute; top: 25%; align-content: center; ';
      S:=S+Format('width: %dpx;',[PageNavigator.FixedWidth]);
      if (aPosition = npRight) then
        S:=S+Format('left: %dpx;',[Context.RunOffsetX+Context.mmToPixels(CurrentPage.PageSize.Width)+PageNavigator.FixedMargin]);
      ApplyStyle(ND,S);
      end;
    end;
end;

class procedure TPageNavigatorElementCreator.WriteDefaultScript(AContext : TGenerateHTMLContext; AScript: TStrings);

Const
  SScript = 'function maybesetpage(ev) {'+sLineBreak+
            '  if (ev.keyCode==13) {' +sLineBreak+
            '    var pageno= ""+ev.target.value;'+sLineBreak+
            '    while (pageno.length<SequenceDigits) { pageno = "0"+ pageno; }; '+sLineBreak+
            '    if ((pageno>0) && (pageno<=TotalPageCount)) {'+sLineBreak+
            '      var url = PageNameFormat.replace(/%d/,pageno);'+sLineBreak+
            '      window.location.href=url;'+sLineBreak+
            '    }'+sLineBreak+
            '  }'+sLineBreak+
            '}';

Var
  S : TStrings;
  FN : String;
  P : Integer;
begin
  FN:=ChangeFileExt(extractFileName(AContext.BaseFileName),AContext.SequenceFormat+ExtractFileExt(AContext.BaseFileName));
  P:=pos('%',FN);
  if FN[P+1]='.' then
    Delete(FN,P+1,2);
  AScript.Add(Format('var PageNameFormat = "%s";',[FN]));
  AScript.Add(Format('var TotalPageCount = %d;',[AContext.TotalPageCount]));
  AScript.Add(Format('var CurrentPage = %d;',[AContext.CurrentPageNo]));
  AScript.Add(Format('var SequenceDigits = %d;',[AContext.SequenceDigits]));
  S:=TStringList.Create;
  try
    S.Text:=SScript;
    AScript.AddStrings(S);
  finally
    S.Free;
  end;
end;


function TPageNavigatorElementCreator.StyleNavigatorButton(aPosition: TNavigatorPosition; AButton: TNavigatorButton; ActiveLink: Boolean): String;

Var
  C : String;

begin
  if (hnoSkipStyling in PageNavigator.Options) then
    exit;
  if aPosition in [npTop,npBottom] then
    begin
    Result:='margin: 0 4px 0 4px;';
    Result:=Result+' float: left;';
    end
  else
    Result:='margin: 4px 0 4px 0;';
  if aButton<>nbPage then
    begin
    if ActiveLink then
      C:=Coalesce(Context.ColorToRGBString(PageNavigator.ActiveBGColor,True),'#63CF80')
    else
      C:=Coalesce(Context.ColorToRGBString(PageNavigator.InActiveBGColor,True),'#BABABA');
    end;
  Result:=Result+' background-color: '+C+';';
  Result:=Result+' border: solid;';
  Result:=Result+' border-width: 1px;';
end;

procedure TPageNavigatorElementCreator.CreatePageNoNavigatorElement(AParentDiV: THTMLElement; aPosition: TNavigatorPosition);
Var
  N2 : THTMLElement;
  IE : THTMLElement;
  Fmt: String;

begin
  if (hnoPageNoEdit in PageNavigator.Options) then
    begin
    IE:=Doc.CreateElement('input');
    IE['type']:='number';
    IE['id']:='pageedit'+NavigatorPositionNames[aPosition];
    IE['onkeypress']:='maybesetpage(event)';
    IE['value']:=IntToStr(Context.CurrentPageNo);
    if not (hnoSkipStyling in PageNavigator.Options) then
      ApplyStyle(IE,'width: 50px;');
    AParentDiV.AppendChild(IE);
    if hnoUsePageNOfM in PageNavigator.Options then
      begin
      N2:=Doc.CreateSpanElement;
      N2.AppendChild(Doc.CreateTextNode(Format(STotalPages,[Context.TotalPageCount])));
      AParentDiV.AppendChild(N2);
      end;
    end
  else
    begin
    N2:=Doc.CreateSpanElement;
    AParentDiv.AppendChild(N2);
    if hnoUsePageNOfM in PageNavigator.Options then
      Fmt:=SCurrentPageOf
    else
      Fmt:=SCurrentPage;
    N2.AppendChild(Doc.CreateTextNode(Format(Fmt,[Context.CurrentPageNo,Context.TotalPageCount])));
    end;
end;



{ TTOCFramePageCreator }

function TTOCFramePageCreator.GetReport: TFPCustomReport;
begin
  Result:=Context.Report;
end;

function TTOCFramePageCreator.GetCSSDIR: String;
begin
  Result:=FContext.CSSDir;
end;

constructor TTOCFramePageCreator.Create(AContext: TGenerateHTMLContext; AOptions: TFramePageOptions);
begin
  FContext:=AContext;
  FFramePage:=AOptions;
  FDoc := THTMLDocument.Create;
  FDoc.AppendChild(FDoc.Impl.CreateDocumentType(
    'HTML', '-//W3C//DTD HTML 4.01 Frameset//EN"',
    'http://www.w3.org/TR/html4/loose.dtd'));
end;

destructor TTOCFramePageCreator.Destroy;
begin
  FreeAndNil(FDoc);
  inherited Destroy;
end;

procedure TTOCFramePageCreator.ConfigureFrameSet(FramesEl : THTMLElement);

Const
  ColRows : Array[Boolean] of string = ('rows','cols');

Var
  IsCols, DirInvert : Boolean;
  P : String;

begin
  DirInvert:=(FramePage.TOCZonePosition in [tpRight,tpBottom]);
  IsCols:=FramePage.TOCZonePosition in [tpLeft,tpRight];
  if Not DirInvert then
    P:=Format('%d%%,%d%%',[FramePage.TOCZoneSize,100-FramePage.TOCZoneSize])
  else
    P:=Format('%d%%,%d%%',[100-FramePage.TOCZoneSize,FramePage.TOCZoneSize]);
  FramesEl[ColRows[isCols]]:=P;
end;

procedure TTOCFramePageCreator.CreateFramePage(PageNames: TStrings; const ATOCPageFileName: String);

Var
  aTocPage,aReportPage,FrameSet,AHeadElement,AHTMLEl,El : THTMLElement;
  FN,D,P : String;
  DirInvert : Boolean;

begin
  AHTMLEl := FDoc.CreateHtmlElement;
  FDoc.AppendChild(AHTMLEl);
  AHeadElement:=FDoc.CreateHeadElement;
  AHTMLEl.AppendChild(AHeadElement);
  El := FDoc.CreateElement('meta');
  El['http-equiv'] := 'Content-Type';
  El['content'] := 'text/html; charset=utf-8';
  AHeadElement.AppendChild(El);
  El := FDoc.CreateElement('title');
  AHeadElement.AppendChild(el);
  el.AppendChild(FDoc.CreateTextNode(Report.Title));
  if (FFramePage.CSSFileName<>'') then
    begin
    El:=FDoc.CreateElement('link');
    El['rel']:='stylesheet';
    D:=CSSDir;
    if (D<>'') then
      D:=D+'/'; // not pathdelim !
    El['href']:=D+FramePage.CSSFIleName;
    AHeadElement.AppendChild(El);
    end;
  FrameSet := FDoc.CreateElement('frameset');
  // No body element when using framesets
  AHTMLEl.AppendChild(FrameSet);
  ConfigureFrameSet(FrameSet) ;
  el := FDoc.CreateNoframesElement;
  El.AppendChild(FDoc.CreateTextNode(SNoFrameSupport));
  FrameSet.AppendChild(el);
  dirInvert:=FramePage.TOCZonePosition in [tpRight,tpBottom];
  aTocPage:=FDoc.CreateElement('frame');
  aTocPage['name']:='reporttoc';
  aTocPage['src']:=ExtractFileName(ATocPageFileName);
  aReportPage:=FDoc.CreateElement('frame');
  aReportPage['name']:='reportpage';
  if PageNames.Count>0 then
    P:=ExtractFileName(PageNames[0]);
  aReportPage['src']:=P;
  if dirInvert then
    begin
    FrameSet.AppendChild(aReportPage);
    FrameSet.AppendChild(aTOCPage);
    end
  else
    begin
    FrameSet.AppendChild(aTOCPage);
    FrameSet.AppendChild(aReportPage);
    end;
  FN:=FramePage.FileName;
  if (FN='') then
    FN:='index.html';
  D:=ExtractFilePath(Context.BaseFileName);
  FN:=D+FN;
  WriteHTML(FDoc,FN);
end;

{ TPageNavigatorOptions }

procedure TPageNavigatorOptions.Assign(Source: TPersistent);

Var
  NO : TPageNavigatorOptions;

begin
  if (source is TPageNavigatorOptions) then
    begin
    NO:=source as TPageNavigatorOptions;
    Positions:=NO.Positions;
    Options:=NO.Options;
    FixedWidth:=NO.FixedWidth;
    FixedMargin:=NO.FixedMargin;
    FixedHeight:=NO.FixedHeight;
    FActiveBGColor:=NO.ActiveBGColor;
    FInActiveBGColor:=NO.InActiveBGColor;
    end
  else
    inherited Assign(Source);
end;

{ TTOCPageOptions }

procedure TTOCPageOptions.Assign(Source: TPersistent);

Var
  O : TTOCPageOptions;

begin
  if (Source is TTOCPageOptions) then
    begin
    O:=Source as TTOCPageOptions;
    FFileName:=O.FileName;
    FCSSFileName:=O.CSSFileName;
    OddPageStyle:=O.OddPageStyle;
    EvenPageStyle:=O.EvenPageStyle;
    SkipStyling:=SkipStyling;
    end
  else
  inherited Assign(Source);
end;

{ TFramePageOptions }

procedure TFramePageOptions.Assign(Source: TPersistent);

Var
  O : TFramePageOptions;

begin
  if (Source is TFramePageOptions) then
    begin
    O:=Source as TFramePageOptions;
    FFileName:=O.FileName;
    FCSSFileName:=O.CSSFileName;
    FTOCZoneSize:=O.TOCZoneSize;
    FTOCZonePosition:=O.TOCZonePosition;
    end
  else
    inherited Assign(Source);
end;

end.

