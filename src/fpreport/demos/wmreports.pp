unit wmreports;

{$mode objfpc}
{$H+}
{$I demos.inc}

{$IFDEF ExportHTML}
{$DEFINE HTMLHELPERS}
{$ENDIF}
{$IFDEF ExportFPIMAGE}
{$DEFINE HTMLHELPERS}
{$ENDIF}

interface

uses
  Classes, SysUtils, httpdefs, fphttp, fpreport, httproute;

Type

  { TGenerateReportModule }

  TGenerateReportModule = class(TCustomHTTPModule)
  Private
  Public
    Procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TPageReportModule }

  TPageReportModule = class(TCustomHTTPModule,IRouteInterface)
  private
    prefix: string;
    L : TStrings;
    procedure AddCb(const N, aLabel: String);
    procedure AddColor(const N, aLabel: String);
    procedure AddCombo(const N, aLabel: String; AValues: array of String);
    procedure AddConfigureFramePage;
    procedure AddConfigurePageNavigator;
    procedure AddConfigureTOCPage;
    procedure AddEdit(const N, aLabel: String);
    procedure AddNumber(const N, aLabel: String);
    Procedure AddPDFOptions;
    Procedure AddHTMLOptions;
    Procedure AddFPImageOptions;
    procedure AddStyleEmbedding;
  Public
    Procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TViewReportModule }

  TViewReportModule = class(TCustomHTTPModule)
  Public
    Procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TReportListModule }

  TReportListModule = class(TCustomHTTPModule)
  Public
    Procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  procedure ShowPage(ARequest : TRequest; AResponse : TResponse);

implementation


uses
  udapp,
  {$IFDEF HTMLHELPERS}
  fpreporthtmlutil, fpimage,
  {$ENDIF}
  {$IFDEF ExportFPImage}
  fpreportfpimageexport,
  {$ENDIF}
  {$IFDEF ExportHTML}
  fpreporthtmlexport,
  {$ENDIF}
  {$IFDEF ExportPDF}
  fppdf,
  fpreportpdfexport,
  {$ENDIF}
  fpjson,
  fpmimetypes;

Type
  { TReportConfigurator }

  TReportConfigurator = Class
  Private
    FStartFileName: String;
    FVars: TStrings;
    Function GetVar(S : String; ADefault : String = '') : String;
    Function GetBoolean(S : String) : Boolean;
    Function GetInteger(S : String; aDefault: integer) : Integer;
  {$IFDEF HTMLHELPERS}
    Procedure ConfigureTOCPage(Prefix : String; aTOCPage : TTOCPageOptions);
    Procedure ConfigureFramePage(Prefix : String; aFramePage : TFramePageOptions);
    Procedure ConfigurePageNavigator(Prefix : String; aPageNavigator : TPageNavigatorOptions);
  {$ENDIF}
  {$IFDEF ExportHTML}
    procedure ConfigHTMLExporter(Exporter: TFPReportExportHTML);
  {$ENDIF}
  {$IFDEF ExportFPImage}
    procedure ConfigImageExporter(Exporter: TFPReportExportFPImage);
  {$ENDIF}
  {$IFDEF ExportPDF}
    procedure ConfigPDFExporter(Exporter: TFPReportExportPDF);
  {$ENDIF}
  Public
    Constructor Create(AVar : TStrings);
    Procedure ConfigReport(Sender : TObject; Exporter : TFPReportExporter);
    Property StartFileName : String Read FStartFileName Write FStartFileName;
  end;


Var Counter : Integer;

{ TReportListModule }

procedure TReportListModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);

Var
  O : TJSONObject;
  A : TJSONArray;
  L : TStrings;
  I : integer;
  R : TReportDef;
  S,D : String;

begin
  L:=Nil;
  A:=Nil;
  O:=TJSONObject.Create();
  try
    A:=TJSONArray.Create;
    O.Add('data',A);
    L:=TStringList.Create;
    TReportDemoApplication.GetRegisteredReports(L);
    For I:=0 to L.Count-1 do
      begin
      R:=TReportDef(L.Objects[i]);
      D:=R.ReportClass.Description;
      S:=L[i];
      if D='' then D:=S;
        A.Add(TJSONObject.Create(['name',S,'description',D]));
      end;
    AResponse.ContentType:='application/json';
    AResponse.Content:=O.AsJSON;
    AResponse.ContentLength:=Length(AResponse.Content);
    AResponse.SendContent;
  finally
    L.Free;
    O.Free;
  end;
end;

{ TViewReportModule }

procedure TViewReportModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);

Var
  FN,TFN : String;

begin
  FN:=ARequest.PathInfo;
  if (FN<>'') and (FN[1]='/') then
    Delete(FN,1,1);
  Delete(FN,1,Pos('/',FN)); // Strip /View
  TFN:=GetTempDir+FN;
  If FileExists(TFN) then
    begin
    AResponse.ContentStream:=TFileStream.Create(GetTempDir+FN,fmOpenRead or fmShareDenyWrite);
    AResponse.FreeContentStream:=True;
    case lowercase(extractfileext(FN)) of
      '.png': AResponse.ContentType:='image/png';
      '.pdf' : AResponse.ContentType:='application/pdf';
      '.html' : AResponse.ContentType:='text/html';
    end;
    end
  else
    begin
    AResponse.Code:=404;
    AResponse.CodeText:='Not found';
    AResponse.Content:='File '+FN+' not found';
    AResponse.SendResponse;
    end;
end;

{ TReportConfigurator }

constructor TReportConfigurator.Create(AVar: TStrings);
begin
  FVars:=AVar;
end;

procedure TReportConfigurator.ConfigReport(Sender: TObject; Exporter: TFPReportExporter);
begin
  {$IFDEF ExportHTML}
  if (Exporter is TFPReportExportHTML) then
    ConfigHTMLExporter(Exporter as TFPReportExportHTML);
  {$ENDIF}
  {$IFDEF ExportFPImage}
  if (Exporter is TFPReportExportFPImage) then
    ConfigImageExporter(Exporter as TFPReportExportfpImage);
  {$ENDIF}
  {$IFDEF ExportPDF}
  if (Exporter is TFPReportExportPDF) then
    ConfigPDFExporter(Exporter as TFPReportExportPDF);
  {$ENDIF}
end;


function TReportConfigurator.GetVar(S: String; ADefault: String): String;
begin
  Result:=FVars.Values[S];
  if (Result='') and (FVars.IndexOfName(S)=-1) then
    Result:=ADefault;
end;

function TReportConfigurator.GetInteger(S: String; aDefault: integer): Integer;
begin
  Result:=StrToIntDef(GetVar(S),aDefault);
end;


function TReportConfigurator.GetBoolean(S: String): Boolean;

Var
  v : String;

begin
  v:=LowerCase(GetVar(S));
  Result:=(v<>'') and ((v='1') or (v='t') or (v='true') or (v='y') or (v='yes'));
end;

{$IFDEF HTMLHELPERS}
procedure TReportConfigurator.ConfigureTOCPage(Prefix: String; aTOCPage: TTOCPageOptions);
begin
  With aTOCPage do
    begin
    // We don't allow this Property FileName : string read FFileName write FFileName;
    CSSFileName:=GetVar(Prefix+'toccssfilename',CSSFileName);
    OddPageStyle:=GetVar(Prefix+'oddpagestyle',OddPageStyle);
    EvenPageStyle:=GetVar(Prefix+'evenpagestyle',EvenPageStyle);
    SkipStyling:=GetBoolean(Prefix+'skipstyling');
    end;
end;

procedure TReportConfigurator.ConfigureFramePage(Prefix: String; aFramePage: TFramePageOptions);
begin
  With aFramePage do
    begin
    // We do not allow setting Frame page filename.
    // Frame page CSS filename. If empty, no <link> is added. Relative to CSSDir
    CSSFileName:=GetVar(Prefix+'framecssfilename',CSSFileName);
    TOCZoneSize:=GetInteger(Prefix+'toczonesize',TOCZoneSize);
    Case lowerCase(prefix+'toczoneposition') of
      'right': TOCZonePosition:=tpRight;
      'top' : TOCZonePosition:=tpTop;
      'bottom': TOCZonePosition:=tpBottom;
    else
      TOCZonePosition:=tpLeft;
    end;
    end;
end;

Function RGBTripleToColor(AColor : TFPColor) : Cardinal;

  Function BS(C : Word; Sh : Byte) : Cardinal;
  begin
    Result:=C shr 8;
    If (Sh<>0) then
      Result:=Result shl SH
  end;

begin
  Result:=BS(AColor.blue,0) or BS(AColor.Green,8) or BS(AColor.Red,16) or BS(AColor.alpha,24);
end;


procedure TReportConfigurator.ConfigurePageNavigator(Prefix: String; aPageNavigator: TPageNavigatorOptions);

Var
  NP : TNavigatorPositions;
  NO : THTMLNavigatorOptions;
  S: String;


  Procedure MaybeAdd(aVar : String; aOption: TNavigatorPosition);

  begin
    If GetBoolean(Prefix+'nav'+aVar) then
      Include(NP,aOption);
  end;

  Procedure MaybeAdd(aVar : String; aOption: THTMLNavigatorOption);

  begin
    If GetBoolean(Prefix+aVar) then
      Include(NO,aOption);
  end;

begin
  NP:=[];
  MaybeAdd('topnavigator',npTop);
  MaybeAdd('leftnavigator',npLeft);
  MaybeAdd('rightnavigator',npRight);
  MaybeAdd('bottomnavigator',npBottom);
  NO:=[];
  MaybeAdd('firstlast',hnoFirstLast);
  MaybeAdd('alwaysfirstlast',hnoAlwaysFirstLast);
  MaybeAdd('pageno',hnoPageNo);
  MaybeAdd('image',hnoImage);
  MaybeAdd('skipstyling',hnoSkipStyling);
  MaybeAdd('usepagenofm',hnoUsePageNOfM);
  MaybeAdd('pagenoedit',hnoPageNoEdit);
  With APageNavigator do
    begin
    if (NP<>[]) then
      Positions:=NP;
    if (NO<>[]) then
      Options:=NO;
    FixedWidth:=GetInteger(Prefix+'navigatorfixedwidth',FixedWidth);
    FixedHeight:=GetInteger(Prefix+'navigatorfixedheight',FixedHeight);
    FixedMargin:=GetInteger(Prefix+'navigatorfixedmargin',FixedMargin);
    S:=GetVar(Prefix+'navigatorbgcolor');
    if (S<>'') then
      ActiveBGColor:= RGBTripleToColor(HtmlToFpColor(S));
    S:=GetVar(Prefix+'navigatorinactivebgcolor');
    if (S<>'') then
      InActiveBGColor:= RGBTripleToColor(HtmlToFpColor(S));
    end;
end;
{$ENDIF}

{$IFDEF ExportHTML}
procedure TReportConfigurator.ConfigHTMLExporter(Exporter : TFPReportExportHTML);

Const
  Prefix = 'html.';

Var
  O : THTMLExportOptions;

  Procedure MaybeAdd(aVar : String; aOption: THTMLExportOption);

  begin
    If GetBoolean(Prefix+aVar) then
      Include(O,aOption);
  end;

begin
  O:=[heoTOCPage];
  MaybeAdd('fixedpositioning',heoFixedPositioning);
  MaybeAdd('inlineimage',heoInlineImage);
  MaybeAdd('useimgtag',heoUseIMGtag);
  MaybeAdd('tocpageframe',heoTOCPageFrame);
  MaybeAdd('memoasis',heoMemoAsIs);
  MaybeAdd('externaljs',heoExternalJS);
  Exporter.Options:=O;
  Exporter.DPI:=GetInteger(Prefix+'DPI',Exporter.DPI);
  Exporter.SequenceFormat:=GetVar(Prefix+'sequence',Exporter.SequenceFormat);
  Case LowerCase(GetVar(Prefix+'styleembedding')) of
    'styletag' : Exporter.StyleEmbedding:=seStyleTag;
    'cssfile'  : Exporter.StyleEmbedding:=seCSSFile;
  else
    Exporter.StyleEmbedding:=seInline;
  end;
  Exporter.FixedOffset.Top:=GetInteger('offsettop',Exporter.FixedOffset.Top);
  Exporter.FixedOffset.Left:=GetInteger('offsetleft',Exporter.FixedOffset.Left);
  ConfigureTOCPage(Prefix,Exporter.TOCPage);
  ConfigureFramePage(Prefix,Exporter.FramePage);
  ConfigurePageNavigator(Prefix,Exporter.PageNavigator);
  StartFileName:='index.html';
end;
{$ENDIF}

{$IFDEF ExportFPImage}
procedure TReportConfigurator.ConfigImageExporter(Exporter : TFPReportExportFPImage);

Const
  Prefix = 'image.';

Var
  HO : THTMLOptions;

begin
  HO:=[hoEnabled,hoTOCPage];
  if GetBoolean(Prefix+'useframes') then
    Include(HO,hoFramePage);
  if GetBoolean(Prefix+'externaljs') then
    Include(HO,hoExternalJS);
  Exporter.HTMLOptions:=HO;
  Exporter.DPI:=GetInteger(Prefix+'DPI',Exporter.DPI);
  Exporter.SequenceFormat:=GetVar(Prefix+'sequence',Exporter.SequenceFormat);
  Case LowerCase(GetVar(Prefix+'styleembedding')) of
    'styletag' : Exporter.StyleEmbedding:=seStyleTag;
    'cssfile'  : Exporter.StyleEmbedding:=seCSSFile;
  else
    Exporter.StyleEmbedding:=seInline;
  end;
  ConfigureTOCPage(Prefix,Exporter.TOCPage);
  ConfigureFramePage(Prefix,Exporter.FramePage);
  ConfigurePageNavigator(Prefix,Exporter.PageNavigator);
  StartFileName:='index.html';
end;
{$ENDIF}

{$IFDEF ExportPDF}
procedure TReportConfigurator.ConfigPDFExporter(Exporter: TFPReportExportPDF);

Const
  Prefix = 'pdf.';

Var
  O : TPDFOptions;

  Procedure MaybeAdd(aVar : String; aOption: TPDFOption);

  begin
    If GetBoolean(Prefix+aVar) then
      Include(O,aOption);
  end;

begin
  Exporter.AutoSave:=True;
  O:=[];
  MaybeAdd('pagelayout',poOutLine);
  MaybeAdd('compresstext',poCompressText);
  MaybeAdd('compressfonts',poCompressFonts);
  MaybeAdd('compressimages',poCompressImages);
  MaybeAdd('userawjpeg',poUseRawJPEG);
  MaybeAdd('noembeddedfonts',poNoEmbeddedFonts);
  MaybeAdd('pageoriginattop',poPageOriginAtTop);
  MaybeAdd('subsetfont',poSubsetFont);
  Exporter.Options:=O;
  Case GetVar(Prefix+'pagelayout') of
    'two':   Exporter.PageLayout:=lTwo;
    'continuous' : Exporter.PageLayout:=lContinuous;
  else
    Exporter.PageLayout:=lSingle;
  end;
end;
{$ENDIF}

{ TGenerateReportModule }

procedure TGenerateReportModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
Var
  F,D,FN : String;
  Conf : TReportConfigurator;
  Fmt : TRenderFormat;
  FRunner : TReportRunner;
  RC  : TFPReportExporterClass;
  Flds : TStrings;

begin
  if SameText(ARequest.Method,'GET') then
    flds:=ARequest.QueryFields
  else
    flds:=ARequest.ContentFields;
//  flds.SaveToFile('/tmp/vars.txt');
  D:=Flds.Values['demo'];
  if (D='') or (TReportDemoApplication.GetReportClass(D)=Nil) then
    Raise Exception.CreateFmt('Invalid or empty demo name : "%s"',[D]);
  F:=flds.Values['format'];
  Fmt:=High(TRenderFormat);
  While (fmt>rfDefault) and (CompareText(TReportDemoApplication.FormatName(fmt),F)<>0) do
    fmt:=Pred(fmt);
  if (fmt=rfDefault) then
    Raise Exception.CreateFmt('Invalid or empty format name : "%s"',[F]);
  FRunner:=TReportRunner.Create(Self);
  FRunner.Location:=ExtractFilePath(ParamStr(0));;
  FRunner.ReportApp:=TReportDemoApplication.GetReportClass(D).Create(Self);
  FRunner.ReportApp.rpt:=TFPReport.Create(FRunner.ReportApp);
  FRunner.Format:=Fmt ;
  FRunner.location:=ExtractFilePath(ParamStr(0));
  RC:=TReportDemoApplication.GetRenderClass(Fmt);
  Inc(Counter);
  FN:=D+IntToStr(Counter);
  FN:=FN+PathDelim+FN+RC.DefaultExtension;
  FRunner.BaseOutputFileName:=GetTempDir+FN;
  Conf:= TReportConfigurator.Create(flds);
  Try
    FRunner.OnInitExporter:=@Conf.ConfigReport;
    FRunner.Execute;
    if (Conf.StartFileName<>'') then
      FN:=ExtractFilePath(FN)+Conf.StartFileName;
  Finally
    Conf.Free;
  end;
  AResponse.SendRedirect('../View/'+FN);
end;

{ TPageReportModule }

procedure TPageReportModule.AddCb(Const N,aLabel: String);

begin
  L.Add(Format('<INPUT TYPE="CHECKBOX" id="CB%s" name="%s" value="1">%s<BR>',[Prefix+N,Prefix+N,aLabel]));
end;

procedure TPageReportModule.AddCombo(Const N,aLabel: String; AValues : Array of String);

Var
  I : Integer;

begin
  L.Add(aLabel+':&nbsp;&nbsp;');
  L.Add(Format('<SELECT ID="CBX%s" NAME="%s">',[Prefix+N,Prefix+N]));
  I:=0;
  While I<Length(AValues)-1 do
    begin
    L.Add(Format('<OPTION value="%s">%s</option>',[AValues[i],AValues[i+1]]));
    Inc(I,2);
    end;
  L.Add('</SELECT>');
  L.Add('<BR>');
end;

procedure TPageReportModule.AddNumber(Const N,aLabel: String);

begin
  L.Add(aLabel+':&nbsp;&nbsp;');
  L.Add(Format('<INPUT TYPE="NUMBER" id="NE%s" name="%s"><BR>',[Prefix+N,Prefix+N]));
end;

procedure TPageReportModule.AddColor(Const N,aLabel: String);

begin
  L.Add(aLabel+':&nbsp;&nbsp;');
  L.Add(Format('<INPUT TYPE="Color" id="NC%s" name="%s"><BR>',[Prefix+N,Prefix+N]));
end;

procedure TPageReportModule.AddPDFOptions;
begin
  L.Add('<H1>PDF options</H1>');
  prefix:='pdf.';
  AddCB('pagelayout','Create outLine');
  AddCB('compresstext','Compress text');
  AddCB('compressfonts','Compress fonts');
  AddCB('compressimages','Compress images');
  AddCB('userawjpeg','use raw JPEG');
  AddCB('noembeddedfonts','Do not embed fonts');
  AddCB('pageoriginattop','Page origin at top');
  AddCB('subsetfont','Embed only used subset of font');
  L.Add('Page layout:<p>');
  AddCombo('pagelayout','Page layout',['single','Single page','two','Two pages','continuous','Continuous layout']);
end;

procedure TPageReportModule.AddStyleEmbedding;

begin
  AddCombo('styleembedding','Style embedding',[
    'inline','Inline, in HTML element',
    'styletag','In separate style tag',
    'cssfile','In separate CSS file'
  ]);
end;

procedure TPageReportModule.AddHTMLOptions;
begin
  L.Add('<H1>HTML options</H1>');
  prefix:='html.';
  L.Add('<TABLE BORDER="1">');
  L.Add('<TR><TD valign="top">');
  L.Add('<H2>General options</H2>');
  AddCB('fixedpositioning','Use fixed positioning');
  AddCB('inlineimage','Use inline images');
  AddCB('useimgtag','Use IMG tag');
  AddCB('tocpageframe','Create TOC Frame');
  AddCB('memoasis','Insert memos as-is (let browser handle layout)');
  AddCB('externaljs','Use external file for JS');
  AddNumber('DPI','DPI (resolution)');
  AddEdit('sequence','Sequence format');
  AddStyleEmbedding;
  AddNumber('offsettop','Fixed positioning, offset from top');
  AddNumber('offsetleft','Fixed positioning, offset from left');
  AddConfigureTOCPage;
  AddConfigureFramePage;
  L.Add('</TD><TD valign="top">');
  AddConfigurePageNavigator;
  L.Add('</TD></TR>');
  L.Add('</TABLE>');
end;

procedure TPageReportModule.AddFPImageOptions;

begin
  Prefix:='image.';
  L.Add('<H1>Image options</H1>');
  L.Add('<TABLE BORDER="1">');
  L.Add('<TR><TD valign="top">');
  L.Add('<H2>General options</H2>');
  AddCB('useframes','Use frames');
  AddCB('externaljs','Use external Javascript file');
  AddNumber('DPI','Image DPI');
  AddEdit('sequence','Page number sequence format');
  AddStyleEmbedding;
  AddConfigureTOCPage;
  AddConfigureFramePage;
  L.Add('</TD><TD valign="top">');
  AddConfigurePageNavigator;
  L.Add('</TD></TR>');
  L.Add('</TABLE>');
end;

procedure TPageReportModule.AddEdit(Const N,aLabel: String);

begin
  L.Add(aLabel+':&nbsp;&nbsp;');
  L.Add(Format('<INPUT TYPE="EDIT" id="NE%s" name="%s"><BR>',[Prefix+N,Prefix+N]));
end;

procedure TPageReportModule.AddConfigureTOCPage;

begin
  L.Add('<H2>TOC Page options:</H2>');
  AddEdit('toccssfilename','CSS file name');
  AddEdit('oddpagestyle','Odd page style elements');
  AddEdit('evenpagestyle','Even page style elements');
  AddCB('skipstyling','Skip Styling');
end;

procedure TPageReportModule.AddConfigureFramePage;

begin
  L.Add('<H2>Frame options:</H2>');
  AddEdit('framecssfilename','CSS file name');
  AddNumber('toczonesize','TOC Zone size (percentage)');
  AddCombo('toczoneposition','Position of TOC zone',[
  'left','Left',
  'right', 'Right',
  'top' , 'Top',
  'bottom', 'Bottom'
  ])
end;

procedure TPageReportModule.AddConfigurePageNavigator;

begin
//  L.Add('<DIV>');
  L.Add('<H2>Navigator</H2>');
  L.Add('<H3>Navigator positions:</H3>');
  AddCB('topnavigator','Top');
  AddCB('leftnavigator','Left');
  AddCB('rightnavigator','Right');
  AddCB('bottomnavigator','Bottom');
  L.Add('<H3>Navigator options:</H3>');
  AddCB('firstlast','Add First/Last buttons');
  AddCB('alwaysfirstlast','Always add First/Last buttons');
  AddCB('pageno','Add page number');
  AddCB('image','Use images (Not yet implemented)');
  AddCB('skipstyling','Skip all styling');
  AddCB('usepagenofm','Use Page N/M display');
  AddCB('pagenoedit','Allow page number editing');
  L.Add('<H3>Width/Color:</H3>');
  AddNumber('navigatorfixedwidth','Fixed width');
  AddNumber('navigatorfixedheight','Fixed height');
  AddNumber('navigatorfixedmargin','Fixed margin');
  AddColor('navigatorbgcolor','Active link color');
  AddColor('navigatorinactivebgcolor','Inactive link color');
//  L.Add('</DIV>');
end;

procedure TPageReportModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);

Var
  RL : TStrings;
  I : Integer;
  F : TRenderFormat;
  RC : TFPReportExporterClass;
  A : String;

begin
  RL:=Nil;
  L:=TStringList.Create;
  try
    RL:=TStringList.Create;
    L.Add('<HTML><HEAD><TITLE>FPReport web demo</TITLE></HEAD>');
    L.Add('<BODY>');
    L.Add('<H1>Select report and output type</H1>');
    A:='/Generate';
    if Pos('/path',lowerCase(ARequest.PathInfo))<>0 then
      A:='..'+A;
    L.Add(Format('<FORM ACTION="%s" METHOD=POST>',[A]));
    L.Add('Report: ');
    L.Add('<SELECT NAME="demo">');
    //
    TReportDemoApplication.GetRegisteredReports(RL);
    For I:=0 to RL.Count-1 do
      L.Add('<OPTION>'+RL[i]+'</option>');
    L.Add('</SELECT>');
    L.Add('</p>');
    L.Add('Format: ');
    L.Add('<SELECT NAME="format">');
    for F in TRenderFormat do
      begin
      RC:=TReportDemoApplication.GetRenderClass(F);
      if (RC<>Nil) and (RC.DefaultExtension<>'') then
        L.Add('<OPTION>'+TReportDemoApplication.FormatName(F)+'</option>');
      end;
    L.Add('</SELECT>');
    L.Add('</p>');
    L.Add('<INPUT TYPE="Submit" Value="Generate"/>');
    L.Add('<HR>');
    AddPDFOptions;
    L.Add('<INPUT TYPE="Submit" Value="Generate"/>');
    L.Add('<HR>');
    AddHTMLOptions;
    L.Add('<INPUT TYPE="Submit" Value="Generate"/>');
    L.Add('<HR>');
    AddFPImageOptions;
    // Finish it off
    L.Add('<INPUT TYPE="Submit" Value="Generate"/>');
    L.Add('</FORM>');
    L.Add('</BODY>');
    L.Add('</HTML>');
    AResponse.Content:=L.Text;
  finally
    L.Free;
  end;
end;

procedure ShowPage(ARequest : TRequest; AResponse : TResponse);

begin
  With TPageReportModule.CreateNew(Nil,0) do
    try
      HandleRequest(ARequest,AResponse);
      if Not AResponse.ContentSent then
        AResponse.SendContent;
    finally
      Free;
    end;
end;


end.

