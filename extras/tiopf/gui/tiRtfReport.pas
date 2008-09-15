{

Revision history:
  2005-07-05: First release by Marius Ellen (mariusellen@home.nl)
  2007-04-18: Ported to Free Pascal and fpGUI by Graeme Geldenhuys (graemeg@gmail.com)

Purpose:
  Create reports with RTF documents with access to dataset and framework objects.

ToDo:
  Better exception handling. (saw some unexpected errors while parsing)
    corrupting the resulting rtf.
  Show errors when trying to past eof in a TtiObjectList (its now ignored)
  Suppress null dates (0 date are displayed as 1899-xx-xx)

  And if anybody got ideas for this section, please email them!
}

unit tiRtfReport;

{$mode objfpc}{$H+}
{.$I tiDefines.inc}

interface

uses
  Classes, SysUtils, contnrs, TypInfo{, Jpeg},
  Db, Variants, tiObject, gfxbase;

type
  TtiRtfParser = class;
  TRtfArgument = class;
  TRtfException = class(Exception);

  TRtfClass =(RtfNothing, RtfUnknown, RtfGroup, RtfText, RtfControl,
    RtfExpression, RtfBranche, RtfParseBegin, RtfParseEnd, RtfEOF);

  TRtfToken =(etNothing, etComma, etFunction, etProcedure, etParenthesis,
    etADD, etSUB, etMUL, etDIV, etEQ, etNE, etGE, etLE, etGT, etLT, etNot,
    etAnd, etOr, etAssign, etFieldName, etVariable, etDataset, etLitString,
    etLitInt, etLitFloat, etLitDate, etLitFalse, etLitTrue);
  TRtfTokenSet = set of TRtfToken;

  TRtfPictureOption =(poMetafile, poBinary);
  TRtfPictureOptions = set of TRtfPictureOption;
  TRtfPictureBorder =(brNone, brSingle, brDouble, brThick, brShadow, brDot, brHair);

  TColor = TfpgColor;
  TPicture = TMemoryStream;  // fake it until we can implement image support


  TRtfPictureAttr = class(TObject)
  private
    FWidth: integer;
    FHeigth: integer;
    FScaleX: integer;
    FScaleY: integer;
    FWidthmm: Double;
    FHeigthmm: Double;
    FBorderWidth: integer;
    FBorderColor: TColor;
    FProportional: boolean;
    FBorderType: TRtfPictureBorder;
    procedure SetScaleX(Value: integer);
    procedure SetScaleY(Value: integer);
  public
    constructor Create(AWidth, AHeigth: word);
    //Width and height are in pixels
    property Width: integer read FWidth;
    property Heigth: integer read FHeigth;
    //Widthmm and heightmm are in milimeters
    property Widthmm: Double read FWidthmm;
    property Heigthmm: Double read FHeigthmm;
    //Scale from 1 to 100
    property ScaleX: integer read FScaleX write SetScaleX;
    property ScaleY: integer read FScaleY write SetScaleY;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property BorderWidth: integer read FBorderWidth write FBorderWidth; //in points
    property BorderType: TRtfPictureBorder read FBorderType write FBorderType;
    property Proportional: boolean read FProportional write FProportional;
  end;


  TRtfPicturePath = procedure(var AFilename: string)of object;
  TRtfFunctionExecute = procedure(AArgument: TRtfArgument)of object;
  TRtfOnPictureAttr = procedure(APictureAttr: TRtfPictureAttr)of object;
  TRtfArgumentEvent = procedure(APrevItem, AArgument, ANextItem: TRtfArgument)of object;
  TRtfOnCreateDataset = procedure(ADatabase, AAlias, ASql: string; AArgument: TRtfArgument)of object;


  //Basic Rtf control word
  TRtfItem = class(TList)
  private
    FNext: TRtfItem;
    FPrev: TRtfItem;
    FParent: TRtfItem;
    FRtfMajor: integer;
    FRtfMinor: integer;
    FRtfClass: TRtfClass;
    FRtfTextBuf: string;
    function GetItem(Index: integer): TRtfItem;
    function CheckItem(AClass: TRtfClass; Major: integer): boolean;
  protected
    procedure Notify(Ptr: pointer; Action: TListNotification); override;
  public
    procedure Assign(ASource: TRtfItem); virtual;
    property Next: TRtfItem read FNext;
    property Prev: TRtfItem read FPrev;
    property Parent: TRtfItem read FParent;
    //Weak names, but it keeps the RawRtfParser simple
    property RtfClass: TRtfClass read FRtfClass write FRtfClass;
    property RtfMajor: integer read FRtfMajor write FRtfMajor;
    property RtfMinor: integer read FRtfMinor write FRtfMinor;
    property RtfTextBuf: string read FRtfTextBuf write FRtfTextBuf;
    property Items[Index: integer]: TRtfItem read GetItem; default;
  end;


  //Old style Double linked list (but still usefull)
  TRtfItemList = class(TObject)
  private
    FHead: TRtfItem;
    FTail: TRtfItem;
    FCount: integer;
  protected
    procedure dlRemove(AItem: TRtfItem; DisposeIt: boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;
    function Add(AItem: TRtfItem): TRtfItem;
    procedure SaveToStream(AStream: TStream; AColors: string);
    procedure Insert(AItem: TRtfItem); {-Insert element at start of list}
    procedure Delete(AItem: TRtfItem); {-Delete existing element in list, disposing of its contents}
    procedure Extract(AItem: TRtfItem); {-Extract existing element from list without disposing of it}
    procedure PlaceAfter(AItem, AAfter: TRtfItem); {-Place element P into list _after_ existing element L}
    procedure PlaceBefore(AItem, ABefore: TRtfItem); {-Place element P into list _before_ existing element L}

    property Head: TRtfItem read FHead; {-Return TRtfItem to head of list}
    property Tail: TRtfItem read FTail; {-Return TRtfItem to tail of list}
    property Count: integer read FCount;
  end;


  //Variable definition
  TRtfVariable = class(TObject)
  private
    FName: string;
    FValue: variant;
    FToken: TRtfToken;
  public
    property Name: string read FName write FName;
    property Value: variant read FValue write FValue;
    property Token: TRtfToken read FToken write FToken;
  end;


  TRtfVariableList = class(TObjectList)
  private
    function GetItem(Index: integer): TRtfVariable;
  public
    destructor Destroy; override;
    function Find(AName: string): TRtfVariable;
    function Add(AName: string; AValue: variant; AToken: TRtfToken): TRtfVariable;
    property Items[Index: integer]: TRtfVariable read GetItem; default;
  end;


  //Function definition
  TRtfFunction = class(TObject)
  private
    FMin: smallint;
    FMax: smallint;
    FName: string;
    FToken: TRtfToken;
    FOnExecute: TRtfFunctionExecute;
  public
    property Name: string read FName write FName; //Function name
    property Min: smallint read FMin write FMin; //Function minimal parameters
    property Max: smallint read FMax write FMax; //Function maximal parameters
    property Token: TRtfToken read FToken write FToken; //Function type (=weak name)
    property OnExecute: TRtfFunctionExecute read FOnExecute write FOnExecute;
  end;


  TRtfFunctionList = class(TObjectList)
  private
    function GetItem(Index: integer): TRtfFunction;
  public
    function Find(AName: string): TRtfFunction;
    function Add(ATokenType: TRtfToken; AName: string; AMin, AMax: smallint; AOnexecute: TRtfFunctionExecute): TRtfFunction;
    property Items[Index: integer]: TRtfFunction read GetItem; default;
  end;


  //Dataset defintion (Simply a wrap around the TDataset compatible and framework objects)
  TRtfDataset = class(TObjectList)
  private
    FName: string;
    FDataset: TObject;
    FParent: TRtfDataset;
    FTableIndex: integer;
    FFreeDataset: boolean;
    function GetItem(Index: integer): TRtfDataset;
    function ResolveNestedFields(ATable: TRtfDataset; AName: string; var AFieldName: string): TRtfDataset;
  protected
    procedure Notify(Ptr: pointer; Action: TListNotification); override;
  public
    destructor Destroy; override;
    function Find(AName: string): TRtfDataset; overload;
    function Find(AName: string; var AFieldName: string): TRtfDataset; overload;
    function Add(ADataset: TObject; AName: string; AFreeDataset: boolean = false): TRtfDataset;
    property Items[Index: integer]: TRtfDataset read GetItem; default;

    procedure Open;
    procedure Next;
    procedure Prior;
    procedure First;
    procedure Last;
    function Eof: boolean;
    function Bof: boolean;
    function IsEmpty: boolean;
    function RecordCount: integer;

    property Parent: TRtfDataset read FParent;
    property Name: string read FName write FName; //TableName
    property FreeDataset: boolean read FFreeDataset;
    property Dataset: TObject read FDataset write FDataset;
    property TableIndex: integer read FTableIndex write FTableIndex; //Record index
  end;


  //Argument definition (For evaluating of expressions)
  TRtfArgument = class(TObjectList)
  private
    FValue: variant;
    FParent: TRtfArgument;
    FToken: TRtfToken;
    FParam: integer;
    FParser: TtiRtfParser;
    procedure ResolveVariable;
    procedure EvaluateExpression;
    function GetItem(Index: integer): TRtfArgument;
    function Add(AArgument: TRtfArgument): TRtfArgument; overload;
    function Add(AValue: variant; ATokenType: TRtfToken): TRtfArgument; overload;
    procedure Walk(ATokenset: TRtfTokenSet; AExecproc: TRtfArgumentEvent);
    procedure EvaluateAssign(APrevItem, AArgument, ANextItem: TRtfArgument);
    procedure EvaluateComparison(APrevItem, AArgument, ANextItem: TRtfArgument);
    procedure EvaluateUnaryBinary(APrevItem, AArgument, ANextItem: TRtfArgument);
  protected
    procedure Notify(Ptr: pointer; Action: TListNotification); override;
    //Dataset stuff
    procedure ResolveFieldName;
    function GetPicture(APicture: TPicture): string;
    procedure GetGraphicsValue(ADataset: TRtfDataset; AFieldName: string);
    procedure ResolveFieldValue(ADataset: TRtfDataset; AFieldName: string);
    procedure GetPictureData(ABuffer: pointer; ALength: cardinal; var Result: string);
  public
    constructor Create(AParser: TtiRtfParser); overload;
    procedure Evaluate;
    function Check(AParam: integer; ATokens: TRtfTokenSet): boolean; overload;
    function Check(ATokens: array of TRtfTokenSet): boolean; overload;
    procedure ParseExpression(AExpression: string); virtual;

    property Parser: TtiRtfParser read FParser;
    property Parent: TRtfArgument read FParent; //Parent argument list
    property Value: variant read FValue write FValue; //Argument value
    property Param: integer read FParam write FParam; //Just for Scan(Dataset)
    property Token: TRtfToken read FToken write FToken; //Argument type
    property Items[Index: integer]: TRtfArgument read GetItem; default;
  end;


  //Color definition
  TRtfColor = class(TObject)
  private
    FBlue: integer;
    FRed: integer;
    FGreen: integer;
    function GetAsString: string;
  public
    property Red: integer read FRed write FRed;
    property Green: integer read FGreen write FGreen;
    property Blue: integer read FBlue write FBlue;
    property AsString: string read GetAsString;
  end;


  TRtfColorList = class(TObjectlist)
  private
    function GetItem(Index: integer): TRtfColor;
    function GetAsString: string;
  public
    property AsString: string read GetAsString;
    procedure Clear; override;
    function UseColor(AColor: TColor): integer; overload;
    function Add(ARed, AGreen, ABlue: integer): integer;
    function Find(ARed, AGreen, ABlue: integer): integer;
    function UseColor(ARed, AGreen, ABlue: integer): integer; overload;
    property Items[Index: integer]: TRtfColor read GetItem; default;
  end;


  //Parser definition
  TtiRtfParser = class(TObject)
  private
    FBoolTrue: string;
    FBoolFalse: string;
    FHlpItems: TRtfItem;
    TmpItems: TObjectlist;
    FDatasets: TRtfDataset;
    FRtfItems: TRtfItemList;
    FErrorBackColor: TColor;
    FErrorForeColor: TColor;
    FRawItems: TRtfItemList;
    FColorList: TRtfColorList;
    FFunctions: TRtfFunctionList;
    FVariables: TRtfVariableList;
    FOnPicturePath: TRtfPicturePath;
    FOnPictureAttr: TRtfOnPictureAttr;
    FOnEvalutate: TRtfFunctionExecute;
    FPictureOptions: TRtfPictureOptions;
    FOnCreateDataset: TRtfOnCreateDataset;
    procedure PreParse;
    procedure Parse(AItems: TRtfItem);
    procedure ParseExpression(AItem: TRtfItem);
    function AddToRtfItems(AItem: TRtfItem): TRtfItem;
    function SkipParagraph(AItem: TRtfItem): TRtfItem;
    procedure UdfDateTimeTo(AArgument: TRtfArgument; AFormat: string);
  protected
    procedure AddFunctions; virtual;
    procedure UdfDummy(AArgument: TRtfArgument);

    //routines for date time
    procedure UdfNow(AArgument: TRtfArgument);
    procedure UdfDate(AArgument: TRtfArgument);
    procedure UdfTime(AArgument: TRtfArgument);
    procedure UdfYear(AArgument: TRtfArgument);
    procedure UdfMonth(AArgument: TRtfArgument);
    procedure UdfDay(AArgument: TRtfArgument);
    procedure UdfShortDayName(AArgument: TRtfArgument);
    procedure UdfShortMonthName(AArgument: TRtfArgument);
    procedure UdfLongDayName(AArgument: TRtfArgument);
    procedure UdfLongMonthName(AArgument: TRtfArgument);
    procedure UdfSYear(AArgument: TRtfArgument);
    procedure UdfSMonth(AArgument: TRtfArgument);
    procedure UdfSDay(AArgument: TRtfArgument);
    procedure UdfStod(AArgument: TRtfArgument);
    procedure UdfDtos(AArgument: TRtfArgument);
    procedure UdfDateToStr(AArgument: TRtfArgument);
    procedure UdfTimeToStr(AArgument: TRtfArgument);
    procedure UdfDateTimeToStr(AArgument: TRtfArgument);
    procedure UdfStrToDate(AArgument: TRtfArgument);
    procedure UdfStrToTime(AArgument: TRtfArgument);
    procedure UdfStrToDateTime(AArgument: TRtfArgument);

    //routines for strings, int, float etc.
    procedure UdfInt(AArgument: TRtfArgument);
    procedure UdfStr(AArgument: TRtfArgument);
    procedure UdfVal(AArgument: TRtfArgument);
    procedure UdfChr(AArgument: TRtfArgument);
    procedure UdfNul(AArgument: TRtfArgument);
    procedure UdfFrac(AArgument: TRtfArgument);
    procedure UdfEmpty(AArgument: TRtfArgument);
    procedure UdfPadr(AArgument: TRtfArgument);
    procedure UdfPadl(AArgument: TRtfArgument);
    procedure UdfLower(AArgument: TRtfArgument);
    procedure UdfUpper(AArgument: TRtfArgument);
    procedure UdfTrunc(AArgument: TRtfArgument);
    procedure UdfRound(AArgument: TRtfArgument);
    procedure UdfTrim(AArgument: TRtfArgument);
    procedure UdfPower(AArgument: TRtfArgument);
    procedure UdfIntPower(AArgument: TRtfArgument);
    procedure UdfTrimLeft(AArgument: TRtfArgument);
    procedure UdfTrimRight(AArgument: TRtfArgument);
    procedure UdfSubStr(AArgument: TRtfArgument);
    procedure UdfIntToStr(AArgument: TRtfArgument);
    procedure UdfStrToInt(AArgument: TRtfArgument);
    procedure UdfFloatToStr(AArgument: TRtfArgument);
    procedure UdfStrToFloat(AArgument: TRtfArgument);
    procedure UdfFormatFloat(AArgument: TRtfArgument);
    procedure UdfFBool(AArgument: TRtfArgument);

    //routines for conditinals
    procedure UdfIf(AArgument: TRtfArgument);
    procedure UdfIif(AArgument: TRtfArgument);

    //routines for datasets
    procedure UdfDataset(AArgument: TRtfArgument);
    procedure UdfScan(AArgument: TRtfArgument);
    procedure UdfBof(AArgument: TRtfArgument);
    procedure UdfEof(AArgument: TRtfArgument);
    procedure UdfNext(AArgument: TRtfArgument);
    procedure UdfPrior(AArgument: TRtfArgument);
    procedure UdfFirst(AArgument: TRtfArgument);
    procedure UdfLast(AArgument: TRtfArgument);
    procedure UdfOpen(AArgument: TRtfArgument);
    procedure UdfIsEmpty(AArgument: TRtfArgument);
    procedure UdfRecordCount(AArgument: TRtfArgument);

    //misch routines
    procedure UdfPicture(AArgument: TRtfArgument);
    procedure UdfDbPicture(AArgument: TRtfArgument);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;
    procedure Execute; virtual;

    procedure LoadFromFile(AFilename: string);
    procedure LoadFromString(AString: string);
    procedure LoadFromStream(AStream: TMemoryStream);
    procedure LoadFromBuffer(ABuffer: pchar; ASize: integer);

    function SaveToString: string;
    procedure SaveToFile(AFileName: string);
    procedure SaveToStream(AStream: TMemoryStream);

    function AddVariable(AName: string; AValue: variant; AToken: TRtfToken): TRtfVariable;
    function AddDataset(ATable: TObject; AName: string; AFreeDataset: boolean = false): TRtfDataset;
    function AddFunction(AName: string; ATokenType: TRtfToken; AMin, AMax: smallint; AOnexecute: TRtfFunctionExecute): TRtfFunction;


    //these should all be hidden in component style
    property RawItems: TRtfItemList read FRawItems;
    property HlpItems: TRtfItem read FHlpItems;
    property RtfItems: TRtfItemList read FRtfItems;
    property Datasets: TRtfDataset read FDatasets;
    property Functions: TRtfFunctionList read FFunctions;
    property Variables: TRtfVariableList read FVariables;
    property ColorList: TRtfColorList read FColorList;
    property OnEvalutate: TRtfFunctionExecute read FOnEvalutate write FOnEvalutate; //For debug only
  published
    property BoolTrue: string read FBoolTrue write FBoolTrue;
    property BoolFalse: string read FBoolFalse write FBoolFalse;
    property ErrorForeColor: TColor read FErrorForeColor write FErrorForeColor default clRed;
    property ErrorBackColor: TColor read FErrorBackColor write FErrorBackColor default clYellow;
    property PictureOptions: TRtfPictureOptions read FPictureOptions write FPictureOptions default[poMetafile, poBinary];

    property OnPicturePath: TRtfPicturePath read FOnPicturePath write FOnPicturePath;
    property OnPictureAttr: TRtfOnPictureAttr read FOnPictureAttr write FOnPictureAttr;
    property OnCreateDataset: TRtfOnCreateDataset read FOnCreateDataset write FOnCreateDataset;
  end;

implementation

uses
  math        // IntPower() function
  ,fpgfx      // fpgApplication.HandleExeception()
  ;

resourcestring
  rsNotImplemented = 'Not implemented';
  rsInvalidDateConstant = 'Invalid date constant';
  rsInvalidTimeConstant = 'Invalid time constant';
  rsInvalidDateSeparator = 'Invalid date separator';
  rsInvalidTimeSeparator = 'Invalid time separator';
  rsInvalidDateTimeConstant = 'Invalid date/time constant';
  rsInvalidExpressionCharacter = 'Invalid expression character "%s"';
  rsUnterminatedStringConstant = 'Unterminated string constant';
  rsToManyClosingParenthesis = 'Unexpected parenthesis';
  rsExpectedClosingParenthesis = 'Missing closing parenthesis';
  rsUnexpectedParameterType = 'Unexpected parameter type';

type
  TRtfKey = record
    RtfKMajor: integer;
    RtfKMinor: integer;
    RtfKStr: string;
  end;


  TRawRtfParser = class(TObject)
  private
    APtr, AEnd: pchar;
    APushedChar: char;
    FRtfMajor: integer;
    FRtfMinor: integer;
    FRtfTextBuf: string;
    AParseItem: TRtfItem;
    FRtfClass: TRtfClass;
    FColorTable: TRtfItem;
    RawItems: TRtfItemList;
    procedure RtfHook;
    procedure GetRtfToken;
    function GetRtfChar: char;
  protected
    property RtfClass: TRtfClass read FRtfClass;
    property RtfMajor: integer read FRtfMajor;
    property RtfMinor: integer read FRtfMinor;
    property RtfTextBuf: string read FRtfTextBuf;
  public
    procedure Execute(ARawItems: TRtfItemList; ARtfPtr: pchar; ARtfSize: integer);

    property ColorTable: TRtfItem read FColorTable;
  end;



const
  {@indent off}
  //Control class major numbers
  //RtfVersion            = 01;
  //RtfDefFont            = 02;
  //RtfCharSet            = 03;
  RtfDestination        = 04;
  //RtfFontFamily         = 05;
  //RtfColorName          = 06;
  RtfSpecialChar        = 07;
  //RtfStyleAttr          = 08;
  //RtfDocAttr            = 09;
  //RtfSectAttr           = 10;
  //RtfTblAttr            = 11;
  RtfParAttr            = 12;
  //RtfCharAttr           = 13;
  //RtfPictAttr           = 14;
  //RtfNeXTGrAttr         = 15;
  //RtfFieldAttr          = 16;
  //RtfTOCAttr            = 17;
  //RtfPosAttr            = 18;

  //RtfExpression major numbers
  RtfNormalExpression   = 1;
  RtfIfExpression       = 2;
  RtfThenExpression     = 3;
  RtfElseExpression     = 4;
  RtfEndifExpression    = 5;
  RtfScan               = 6;
  RtfScanEntry          = 7;
  RtfScanFooter         = 8;
  RtfScanEnd            = 9;

  //Group class major numbers
  RtfBeginGroup         = 01;
  RtfEndGroup           = 02;

  //Control class minor numbers
  {RtfAnsiCharSet = 0;
  RtfMacCharSet = 1;
  RtfPcCharSet = 2;
  RtfPcaCharSet = 3;}


  //Destination attributes minor numbers
  {RtfPict = 0;
  RtfNeXTGraphic = 1;
  RtfFootnote = 2;
  RtfHeader = 3;
  RtfHeaderLeft = 4;
  RtfHeaderRight = 5;
  RtfHeaderFirst = 6;
  RtfFooter = 7;
  RtfFooterLeft = 8;
  RtfFooterRight = 9;
  RtfFooterFirst = 10;
  RtfFNSep = 11;
  RtfFNContSep = 12;
  RtfFNContNotice = 13;}
  RtfInfo = 14;
  RtfStyleSheet = 15;
  RtfFontTbl = 16;
  RtfColorTbl = 17;
  RtfField = 18;
  {RtfFieldInst = 19;
  RtfFieldResult = 20;
  RtfIndex = 21;
  RtfIndexBold = 22;
  RtfIndexItalic = 23;
  RtfIndexText = 24;
  RtfIndexRange = 25;
  RtfTOC = 26;
  RtfBookmarkStart = 27;
  RtfBookmarkEnd = 28;
  RtfITitle = 29;
  RtfISubject = 30;
  RtfIAuthor = 31;
  RtfIOperator = 32;
  RtfIKeywords = 33;
  RtfIComment = 34;
  RtfIVersion = 35;
  RtfIDoccomm = 36;}

  //Fonts minor numbers
  {RtfFFNil = 0;
  RtfFFRoman = 1;
  RtfFFSwiss = 2;
  RtfFFModern = 3;
  RtfFFScript = 4;
  RtfFFDecor = 5;
  RtfFFTech = 6;}

  //Color attributes minor numbers
  {RtfRed = 0;
  RtfGreen = 1;
  RtfBlue = 2;}

  //Style attributes minor numbers
  {RtfBasedOn = 0;
  RtfNext = 1;}

  //Special characters minor numbers
  {RtfCurHeadPage = 0;
  RtfCurFNote = 1;
  RtfCurHeadPict = 2;
  RtfCurHeadDate = 3;
  RtfCurHeadTime = 4;}
  RtfFormula = 5;
  RtfNoBrkSpace = 6;
  RtfNoReqHyphen = 7;
  RtfNoBrkHyphen = 8;
  {RtfPage = 9;
  RtfLine = 10;}
  RtfPar = 11;
  {RtfSect = 12;}
  RtfTab = 13;
  {RtfCell = 14;
  RtfRow = 15;
  RtfCurAnnot = 16;
  RtfAnnotation = 17;
  RtfAnnotID = 18;
  RtfCurAnnotRef = 19;
  RtfFNoteSep = 20;
  RtfFNoteCont = 21;
  RtfColumn = 22;}
  RtfOptDest = 23;
  {RtfIIntVersion = 24;
  RtfICreateTime = 25;
  RtfIRevisionTime = 26;
  RtfIPrintTime = 27;
  RtfIBackupTime = 28;
  RtfIEditTime = 29;
  RtfIYear = 30;
  RtfIMonth = 31;
  RtfIDay = 32;
  RtfIHour = 33;
  RtfIMinute = 34;
  RtfINPages = 35;
  RtfINWords = 36;
  RtfINChars = 37;
  RtfIIntID = 38;}
  RtflQuote = 39;
  RtfrQuote = 40;
  RtflDblQuote = 41;
  RtfrDblQuote = 42;


  //Document atributes minor numbers
  {RtfPaperWidth = 0;
  RtfPaperHeight = 1;
  RtfLeftMargin = 2;
  RtfRightMargin = 3;
  RtfTopMargin = 4;
  RtfBottomMargin = 5;
  RtfFacingPage = 6;
  RtfGutterWid = 7;
  RtfDefTab = 8;
  RtfWidowCtrl = 9;
  RtfHyphHotZone = 10;
  RtfFNoteEndSect = 11;
  RtfFNoteEndDoc = 12;
  RtfFNoteText = 13;
  RtfFNoteBottom = 14;
  RtfFNoteStart = 15;
  RtfFNoteRestart = 16;
  RtfPageStart = 17;
  RtfLineStart = 18;
  RtfLandscape = 19;
  RtfFracWidth = 20;
  RtfNextFile = 21;
  RtfTemplate = 22;
  RtfMakeBackup = 23;
  RtfRtfDefault = 24;
  RtfRevisions = 25;
  RtfMirrorMargin = 26;
  RtfRevDisplay = 27;
  RtfRevBar = 28;}

  //Sector attributes minor numbers
  {RtfSectDef = 0;
  RtfNoBreak = 1;
  RtfColBreak = 2;
  RtfPageBreak = 3;
  RtfEvenBreak = 4;
  RtfOddBreak = 5;
  RtfPageStarts = 6;
  RtfPageCont = 7;
  RtfPageRestart = 8;
  RtfPageDecimal = 9;
  RtfPageURoman = 10;
  RtfPageLRoman = 11;
  RtfPageULetter = 12;
  RtfPageLLetter = 13;
  RtfPageNumLeft = 14;
  RtfPageNumTop = 15;
  RtfHeaderY = 16;
  RtfFooterY = 17;
  RtfLineModulus = 18;
  RtfLineDist = 19;
  RtfLineStarts = 20;
  RtfLineRestart = 21;
  RtfLineRestartPg = 22;
  RtfLineCont = 23;
  RtfTopVAlign = 24;
  RtfBottomVAlign = 25;
  RtfCenterVAlign = 26;
  RtfJustVAlign = 27;
  RtfColumns = 28;
  RtfColumnSpace = 29;
  RtfColumnLine = 30;
  RtfENoteHere = 31;
  RtfTitleSpecial = 32;}

  //Table attributes minor numbers
  {RtfCellBordBottom = 0;
  RtfCellBordTop = 1;
  RtfCellBordLeft = 2;
  RtfCellBordRight = 3;
  RtfRowDef = 4;
  RtfRowLeft = 5;
  RtfRowRight = 6;
  RtfRowCenter = 7;
  RtfRowGapH = 8;
  RtfRowHt = 9;
  RtfRowLeftEdge = 10;
  RtfCellPos = 11;
  RtfMergeRngFirst = 12;
  RtfMergePrevious = 13;}

  //Paragrapgh attributes minor numbers
  RtfParDef = 0;
  {RtfStyleNum = 1;
  RtfQuadLeft = 2;
  RtfQuadRight = 3;
  RtfQuadJust = 4;
  RtfQuadCenter = 5;
  RtfFirstIndent = 6;
  RtfLeftIndent = 7;
  RtfRightIndent = 8;
  RtfSpaceBefore = 9;
  RtfSpaceAfter = 10;
  RtfSpaceBetween = 11;
  RtfInTable = 12;
  RtfKeep = 13;
  RtfKeepNext = 14;
  RtfSideBySide = 15;
  RtfPBBefore = 16;
  RtfNoLineNum = 17;
  RtfTabPos = 18;
  RtfTabRight = 19;
  RtfTabCenter = 20;
  RtfTabDecimal = 21;
  RtfTabBar = 22;
  RtfBorderTop = 23;
  RtfBorderBottom = 24;
  RtfBorderLeft = 25;
  RtfBorderRight = 26;
  RtfBorderBox = 27;
  RtfBorderBar = 28;
  RtfBorderBetween = 29;
  RtfBorderSingle = 30;
  RtfBorderThick = 31;
  RtfBorderShadow = 32;
  RtfBorderDouble = 33;
  RtfBorderDot = 34;
  RtfBorderHair = 35;
  RtfBorderSpace = 36;
  RtfLeaderDot = 37;
  RtfLeaderHyphen = 38;
  RtfLeaderUnder = 39;
  RtfLeaderThick = 40;}

  //Character attributes minor numbers
  {RtfPlain = 0;
  RtfBold = 1;
  RtfItalic = 2;
  RtfStrikeThru = 3;
  RtfOutline = 4;
  RtfShadow = 5;
  RtfSmallCaps = 6;
  RtfAllCaps = 7;
  RtfInvisible = 8;
  RtfFontNum = 9;
  RtfFontSize = 10;
  RtfExpand = 11;
  RtfUnderline = 12;
  RtfWUnderline = 13;
  RtfDUnderline = 14;
  RtfDbUnderline = 15;
  RtfNoUnderline = 16;
  RtfSuperScript = 17;
  RtfSubScript = 18;
  RtfRevised = 19;
  RtfForeColor = 20;
  RtfBackColor = 21;
  RtfGray = 22; }

  //Picture attributes minor numbers
  {RtfMacQD = 0;
  RtfWinMetafile = 1;
  RtfWinBitmap = 2;
  RtfPicWid = 3;
  RtfPicHt = 4;
  RtfPicGoalWid = 5;
  RtfPicGoalHt = 6;
  RtfPicScaleX = 7;
  RtfPicScaleY = 8;
  RtfPicScaled = 9;
  RtfPicCropTop = 10;
  RtfPicCropBottom = 11;
  RtfPicCropLeft = 12;
  RtfPicCropRight = 13;
  RtfPixelBits = 14;
  RtfBitmapPlanes = 15;
  RtfBitmapWid = 16;
  RtfPicBinary = 17;}

  //
  {RtfNeXTGWidth = 0;
  RtfNeXTGHeight = 1;}

  //Field attributes minor numbers
  {RtfFieldDirty = 0;
  RtfFieldEdited = 1;
  RtfFieldLocked = 2;
  RtfFieldPrivate = 3;}

  //Toc attributes minor numbers
  {RtfTOCType = 0;
  RtfTOCLevel = 1;}

  //Position attributes minor numbers
  {RtfPosX = 0;
  RtfPosXCenter = 1;
  RtfPosXInside = 2;
  RtfPosXLeft = 3;
  RtfPosXOutSide = 4;
  RtfPosXRight = 5;
  RtfPosY = 6;
  RtfPosYInline = 7;
  RtfPosYTop = 8;
  RtfPosYCenter = 9;
  RtfPosYBottom = 10;
  RtfAbsWid = 11;
  RtfTextDist = 12;
  RtfRPosMargV = 13;
  RtfRPosPageV = 14;
  RtfRPosMargH = 15;
  RtfRPosPageH = 16;
  RtfRPosColH = 17;}

const
  //A reduced set of control words
  RtfKey: array[0..15]of TRtfKey =
  (
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfParDef;         RtfKStr: '\pard'         ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfPar;            RtfKStr: '\par'          ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfTab;            RtfKStr: '\tab'          ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtflQuote;         RtfKStr: '\lquote'       ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfrQuote;         RtfKStr: '\rquote'       ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtflQuote;         RtfKStr: '\'+Chr(39)+'91'),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfrQuote;         RtfKStr: '\'+Chr(39)+'92'),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtflDblQuote;      RtfKStr: '\ldblquote'    ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfrDblQuote;      RtfKStr: '\rdblquote'    ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtflDblQuote;      RtfKStr: '\'+Chr(39)+'93'),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfrDblQuote;      RtfKStr: '\'+Chr(39)+'94'),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfInfo;           RtfKStr: '\info'         ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfStyleSheet;     RtfKStr: '\stylesheet'   ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfFontTbl;        RtfKStr: '\fonttbl'      ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfColorTbl;       RtfKStr: '\colortbl'     ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfField;          RtfKStr: '\field'        )
  );

  {  You could also add the following (but i don't need all that):
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfCurHeadPict;    RtfKStr: '\chpict'       ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfCurHeadDate;    RtfKStr: '\chdate'       ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfCurHeadTime;    RtfKStr: '\chtime'       ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfCurHeadPage;    RtfKStr: '\chpgn'        ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfCurFNote;       RtfKStr: '\chftn'        ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfCurAnnotRef;    RtfKStr: '\chatn'        ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfFNoteSep;       RtfKStr: '\chftnsep'     ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfFNoteCont;      RtfKStr: '\chftnsepc'    ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfCell;           RtfKStr: '\cell'         ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfRow;            RtfKStr: '\row'          ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfSect;           RtfKStr: '\sect'         ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfPage;           RtfKStr: '\page'         ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfColumn;         RtfKStr: '\column'       ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfLine;           RtfKStr: '\line'         ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfIIntVersion;    RtfKStr: '\vern'         ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfICreateTime;    RtfKStr: '\creatim'      ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfIRevisionTime;  RtfKStr: '\revtim'       ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfIPrintTime;     RtfKStr: '\printim'      ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfIBackupTime;    RtfKStr: '\buptim'       ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfIEditTime;      RtfKStr: '\edmins'       ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfIYear;          RtfKStr: '\yr'           ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfIMonth;         RtfKStr: '\mo'           ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfIDay;           RtfKStr: '\dy'           ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfIHour;          RtfKStr: '\hr'           ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfIMinute;        RtfKStr: '\min'          ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfINPages;        RtfKStr: '\nofpages'     ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfINWords;        RtfKStr: '\nofwords'     ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfINChars;        RtfKStr: '\nofchars'     ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfIIntID;         RtfKStr: '\id'           ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfPict;           RtfKStr: '\pict'         ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfNeXTGraphic;    RtfKStr: '\nextgraphic'  ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfFootnote;       RtfKStr: '\footnote'     ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfHeader;         RtfKStr: '\header'       ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfHeaderLeft;     RtfKStr: '\headerl'      ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfHeaderRight;    RtfKStr: '\headerr'      ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfHeaderFirst;    RtfKStr: '\headerf'      ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfFooter;         RtfKStr: '\footer'       ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfFooterLeft;     RtfKStr: '\footerl'      ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfFooterRight;    RtfKStr: '\footerr'      ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfFooterFirst;    RtfKStr: '\footerf'      ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfFNSep;          RtfKStr: '\ftnsep'       ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfFNContSep;      RtfKStr: '\ftnsepc'      ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfFNContNotice;   RtfKStr: '\ftncn'        ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfAnnotation;     RtfKStr: '\annotation'   ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfAnnotID;        RtfKStr: '\atnid'        ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfFieldInst;      RtfKStr: '\fldinst'      ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfFieldResult;    RtfKStr: '\fldrslt'      ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfIndex;          RtfKStr: '\xe'           ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfIndexBold;      RtfKStr: '\bxe'          ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfIndexItalic;    RtfKStr: '\ixe'          ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfIndexText;      RtfKStr: '\txe'          ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfIndexRange;     RtfKStr: '\rxe'          ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfTOC;            RtfKStr: '\tc'           ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfBookmarkStart;  RtfKStr: '\bkmkstart'    ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfBookmarkEnd;    RtfKStr: '\bkmkend'      ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfITitle;         RtfKStr: '\title'        ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfISubject;       RtfKStr: '\subject'      ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfIAuthor;        RtfKStr: '\author'       ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfIOperator;      RtfKStr: '\operator'     ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfIKeywords;      RtfKStr: '\keywords'     ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfIComment;       RtfKStr: '\comment'      ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfIVersion;       RtfKStr: '\version'      ),
    (RtfKMajor: RtfDestination; RtfKMinor: RtfIDoccomm;       RtfKStr: '\doccomm'      ),
    (RtfKMajor: RtfVersion;     RtfKMinor: - 1;               RtfKStr: '\rtf'          ),
    (RtfKMajor: RtfDefFont;     RtfKMinor: - 1;               RtfKStr: '\deff'         ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfStyleNum;       RtfKStr: '\s'            ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfQuadLeft;       RtfKStr: '\ql'           ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfQuadRight;      RtfKStr: '\qr'           ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfQuadJust;       RtfKStr: '\qj'           ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfQuadCenter;     RtfKStr: '\qc'           ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfFirstIndent;    RtfKStr: '\fi'           ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfLeftIndent;     RtfKStr: '\li'           ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfRightIndent;    RtfKStr: '\ri'           ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfSpaceBefore;    RtfKStr: '\sb'           ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfSpaceAfter;     RtfKStr: '\sa'           ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfSpaceBetween;   RtfKStr: '\sl'           ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfInTable;        RtfKStr: '\intbl'        ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfKeep;           RtfKStr: '\keep'         ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfKeepNext;       RtfKStr: '\keepn'        ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfSideBySide;     RtfKStr: '\sbys'         ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfPBBefore;       RtfKStr: '\pagebb'       ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfNoLineNum;      RtfKStr: '\noline'       ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfTabPos;         RtfKStr: '\tx'           ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfTabRight;       RtfKStr: '\tqr'          ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfTabCenter;      RtfKStr: '\tqc'          ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfTabDecimal;     RtfKStr: '\tqdec'        ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfTabBar;         RtfKStr: '\tb'           ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfBorderTop;      RtfKStr: '\brdrt'        ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfBorderBottom;   RtfKStr: '\brdrb'        ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfBorderLeft;     RtfKStr: '\brdrl'        ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfBorderRight;    RtfKStr: '\brdrr'        ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfBorderBar;      RtfKStr: '\bar'          ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfBorderBox;      RtfKStr: '\box'          ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfBorderBetween;  RtfKStr: '\brdrbtw'      ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfBorderSingle;   RtfKStr: '\brdrs'        ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfBorderThick;    RtfKStr: '\brdrth'       ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfBorderShadow;   RtfKStr: '\brdrsh'       ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfBorderDouble;   RtfKStr: '\brdrdb'       ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfBorderDot;      RtfKStr: '\brdrdot'      ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfBorderHair;     RtfKStr: '\brdrhair'     ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfLeaderDot;      RtfKStr: '\tldot'        ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfLeaderHyphen;   RtfKStr: '\tlhyph'       ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfLeaderUnder;    RtfKStr: '\tlul'         ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfLeaderThick;    RtfKStr: '\tlth'         ),
    (RtfKMajor: RtfParAttr;     RtfKMinor: RtfBorderSpace;    RtfKStr: '\brsp'         ),
    (RtfKMajor: RtfTblAttr;     RtfKMinor: RtfCellBordBottom; RtfKStr: '\clbrdrb'      ),
    (RtfKMajor: RtfTblAttr;     RtfKMinor: RtfCellBordTop;    RtfKStr: '\clbrdrt'      ),
    (RtfKMajor: RtfTblAttr;     RtfKMinor: RtfCellBordLeft;   RtfKStr: '\clbrdrl'      ),
    (RtfKMajor: RtfTblAttr;     RtfKMinor: RtfCellBordRight;  RtfKStr: '\clbrdrr'      ),
    (RtfKMajor: RtfTblAttr;     RtfKMinor: RtfRowDef;         RtfKStr: '\trowd'        ),
    (RtfKMajor: RtfTblAttr;     RtfKMinor: RtfRowLeft;        RtfKStr: '\trql'         ),
    (RtfKMajor: RtfTblAttr;     RtfKMinor: RtfRowRight;       RtfKStr: '\trqr'         ),
    (RtfKMajor: RtfTblAttr;     RtfKMinor: RtfRowCenter;      RtfKStr: '\trqc'         ),
    (RtfKMajor: RtfTblAttr;     RtfKMinor: RtfRowGapH;        RtfKStr: '\trgaph'       ),
    (RtfKMajor: RtfTblAttr;     RtfKMinor: RtfRowHt;          RtfKStr: '\trrh'         ),
    (RtfKMajor: RtfTblAttr;     RtfKMinor: RtfRowLeftEdge;    RtfKStr: '\trleft'       ),
    (RtfKMajor: RtfTblAttr;     RtfKMinor: RtfCellPos;        RtfKStr: '\cellx'        ),
    (RtfKMajor: RtfTblAttr;     RtfKMinor: RtfMergeRngFirst;  RtfKStr: '\clmgf'        ),
    (RtfKMajor: RtfTblAttr;     RtfKMinor: RtfMergePrevious;  RtfKStr: '\clmrg'        ),
    (RtfKMajor: RtfTOCAttr;     RtfKMinor: RtfTOCType;        RtfKStr: '\tcf'          ),
    (RtfKMajor: RtfTOCAttr;     RtfKMinor: RtfTOCLevel;       RtfKStr: '\tcl'          ),
    (RtfKMajor: RtfFontFamily;  RtfKMinor: RtfFFNil;          RtfKStr: '\fnil'         ),
    (RtfKMajor: RtfFontFamily;  RtfKMinor: RtfFFRoman;        RtfKStr: '\froman'       ),
    (RtfKMajor: RtfFontFamily;  RtfKMinor: RtfFFSwiss;        RtfKStr: '\fswiss'       ),
    (RtfKMajor: RtfFontFamily;  RtfKMinor: RtfFFModern;       RtfKStr: '\fmodern'      ),
    (RtfKMajor: RtfFontFamily;  RtfKMinor: RtfFFScript;       RtfKStr: '\fscript'      ),
    (RtfKMajor: RtfFontFamily;  RtfKMinor: RtfFFDecor;        RtfKStr: '\fdecor'       ),
    (RtfKMajor: RtfFontFamily;  RtfKMinor: RtfFFTech;         RtfKStr: '\ftech'        ),
    (RtfKMajor: RtfCharSet;     RtfKMinor: RtfMacCharSet;     RtfKStr: '\mac'          ),
    (RtfKMajor: RtfCharSet;     RtfKMinor: RtfAnsiCharSet;    RtfKStr: '\ansi'         ),
    (RtfKMajor: RtfCharSet;     RtfKMinor: RtfPcCharSet;      RtfKStr: '\pc'           ),
    (RtfKMajor: RtfCharSet;     RtfKMinor: RtfPcaCharSet;     RtfKStr: '\pca'          ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfPlain;          RtfKStr: '\plain'        ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfBold;           RtfKStr: '\b'            ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfItalic;         RtfKStr: '\i'            ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfStrikeThru;     RtfKStr: '\strike'       ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfOutline;        RtfKStr: '\outl'         ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfShadow;         RtfKStr: '\shad'         ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfSmallCaps;      RtfKStr: '\scaps'        ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfAllCaps;        RtfKStr: '\caps'         ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfInvisible;      RtfKStr: '\v'            ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfFontNum;        RtfKStr: '\f'            ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfFontSize;       RtfKStr: '\fs'           ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfExpand;         RtfKStr: '\expnd'        ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfUnderline;      RtfKStr: '\ul'           ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfWUnderline;     RtfKStr: '\ulw'          ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfDUnderline;     RtfKStr: '\uld'          ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfDbUnderline;    RtfKStr: '\uldb'         ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfNoUnderline;    RtfKStr: '\ulnone'       ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfSuperScript;    RtfKStr: '\up'           ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfSubScript;      RtfKStr: '\dn'           ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfRevised;        RtfKStr: '\revised'      ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfForeColor;      RtfKStr: '\cf'           ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfBackColor;      RtfKStr: '\cb'           ),
    (RtfKMajor: RtfCharAttr;    RtfKMinor: RtfGray;           RtfKStr: '\gray'         ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfFormula;        RtfKStr: '\|'            ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfNoBrkSpace;     RtfKStr: '\~'            ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfNoReqHyphen;    RtfKStr: '\-'            ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfNoBrkHyphen;    RtfKStr: '\_'            ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfOptDest;        RtfKStr: '\*'            ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfPar;            RtfKstr: #10             ),
    (RtfKMajor: RtfSpecialChar; RtfKMinor: RtfPar;            RtfKstr: #13             ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfPosX;           RtfKStr: '\posx'         ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfPosXCenter;     RtfKStr: '\posxc'        ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfPosXInside;     RtfKStr: '\posxi'        ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfPosXLeft;       RtfKStr: '\posxl'        ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfPosXOutSide;    RtfKStr: '\posxo'        ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfPosXRight;      RtfKStr: '\posxr'        ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfPosY;           RtfKStr: '\posy'         ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfPosYInline;     RtfKStr: '\posyil'       ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfPosYTop;        RtfKStr: '\posyt'        ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfPosYCenter;     RtfKStr: '\posyc'        ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfPosYBottom;     RtfKStr: '\posyb'        ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfAbsWid;         RtfKStr: '\absw'         ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfTextDist;       RtfKStr: '\dxfrtext'     ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfRPosMargV;      RtfKStr: '\pvmrg'        ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfRPosPageV;      RtfKStr: '\pvpg'         ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfRPosMargH;      RtfKStr: '\phmrg'        ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfRPosPageH;      RtfKStr: '\phpg'         ),
    (RtfKMajor: RtfPosAttr;     RtfKMinor: RtfRPosColH;       RtfKStr: '\phcol'        ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfSectDef;        RtfKStr: '\sectd'        ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfNoBreak;        RtfKStr: '\sbknone'      ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfColBreak;       RtfKStr: '\sbkcol'       ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfPageBreak;      RtfKStr: '\sbkpage'      ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfEvenBreak;      RtfKStr: '\sbkeven'      ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfOddBreak;       RtfKStr: '\sbkodd'       ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfPageCont;       RtfKStr: '\pgncont'      ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfPageStarts;     RtfKStr: '\pgnstarts'    ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfPageRestart;    RtfKStr: '\pgnrestart'   ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfPageDecimal;    RtfKStr: '\pgndec'       ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfPageURoman;     RtfKStr: '\pgnucrm'      ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfPageLRoman;     RtfKStr: '\pgnlcrm'      ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfPageULetter;    RtfKStr: '\pgnucltr'     ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfPageLLetter;    RtfKStr: '\pgnlcltr'     ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfPageNumLeft;    RtfKStr: '\pgnx'         ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfPageNumTop;     RtfKStr: '\pgny'         ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfHeaderY;        RtfKStr: '\headery'      ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfFooterY;        RtfKStr: '\footery'      ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfLineModulus;    RtfKStr: '\linemod'      ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfLineDist;       RtfKStr: '\linex'        ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfLineStarts;     RtfKStr: '\linestarts'   ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfLineRestart;    RtfKStr: '\linerestart'  ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfLineRestartPg;  RtfKStr: '\lineppage'    ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfLineCont;       RtfKStr: '\linecont'     ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfTopVAlign;      RtfKStr: '\vertalt'      ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfBottomVAlign;   RtfKStr: '\vertal'       ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfCenterVAlign;   RtfKStr: '\vertalc'      ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfJustVAlign;     RtfKStr: '\vertalj'      ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfColumns;        RtfKStr: '\cols'         ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfColumnSpace;    RtfKStr: '\colsx'        ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfColumnLine;     RtfKStr: '\linebetcol'   ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfENoteHere;      RtfKStr: '\endnhere'     ),
    (RtfKMajor: RtfSectAttr;    RtfKMinor: RtfTitleSpecial;   RtfKStr: '\titlepg'      )
    (RtfKMajor: RtfFieldAttr;   RtfKMinor: RtfFieldDirty;     RtfKStr: '\flddirty'     ),
    (RtfKMajor: RtfFieldAttr;   RtfKMinor: RtfFieldEdited;    RtfKStr: '\fldedit'      ),
    (RtfKMajor: RtfFieldAttr;   RtfKMinor: RtfFieldLocked;    RtfKStr: '\fldlock'      ),
    (RtfKMajor: RtfFieldAttr;   RtfKMinor: RtfFieldPrivate;   RtfKStr: '\fldpriv'      ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfPaperWidth;     RtfKStr: '\paperw'       ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfPaperHeight;    RtfKStr: '\paperh'       ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfLeftMargin;     RtfKStr: '\margl'        ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfRightMargin;    RtfKStr: '\margr'        ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfTopMargin;      RtfKStr: '\margt'        ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfBottomMargin;   RtfKStr: '\margb'        ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfFacingPage;     RtfKStr: '\facingp'      ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfGutterWid;      RtfKStr: '\gutter'       ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfDefTab;         RtfKStr: '\deftab'       ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfWidowCtrl;      RtfKStr: '\widowctrl'    ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfHyphHotZone;    RtfKStr: '\hyphhotz'     ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfFNoteEndSect;   RtfKStr: '\endnotes'     ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfFNoteEndDoc;    RtfKStr: '\enddoc'       ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfFNoteBottom;    RtfKStr: '\ftnbj'        ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfFNoteText;      RtfKStr: '\ftntj'        ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfFNoteStart;     RtfKStr: '\ftnstart'     ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfFNoteRestart;   RtfKStr: '\ftnrestart'   ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfPageStart;      RtfKStr: '\pgnstart'     ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfLineStart;      RtfKStr: '\linestart'    ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfLandscape;      RtfKStr: '\landscape'    ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfFracWidth;      RtfKStr: '\fracwidth'    ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfNextFile;       RtfKStr: '\nextfile'     ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfTemplate;       RtfKStr: '\template'     ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfMakeBackup;     RtfKStr: '\makeback'     ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfRtfDefault;     RtfKStr: '\defformat'    ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfRevisions;      RtfKStr: '\revisions'    ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfMirrorMargin;   RtfKStr: '\margmirror'   ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfRevDisplay;     RtfKStr: '\revprop'      ),
    (RtfKMajor: RtfDocAttr;     RtfKMinor: RtfRevBar;         RtfKStr: '\revbar'       ),
    (RtfKMajor: RtfStyleAttr;   RtfKMinor: RtfBasedOn;        RtfKStr: '\sbasedon'     ),
    (RtfKMajor: RtfStyleAttr;   RtfKMinor: RtfNext;           RtfKStr: '\snext'        ),
    (RtfKMajor: RtfColorName;   RtfKMinor: RtfRed;            RtfKStr: '\red'          ),
    (RtfKMajor: RtfColorName;   RtfKMinor: RtfGreen;          RtfKStr: '\green'        ),
    (RtfKMajor: RtfColorName;   RtfKMinor: RtfBlue;           RtfKStr: '\blue'         ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfMacQD;          RtfKStr: '\macpict'      ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfWinMetafile;    RtfKStr: '\wmetafile'    ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfWinBitmap;      RtfKStr: '\wbitmap'      ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfPicWid;         RtfKStr: '\picw'         ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfPicHt;          RtfKStr: '\pich'         ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfPicGoalWid;     RtfKStr: '\picwgoal'     ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfPicGoalWid;     RtfKStr: '\picwGoal'     ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfPicGoalHt;      RtfKStr: '\pichgoal'     ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfPicGoalHt;      RtfKStr: '\pichGoal'     ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfPicScaleX;      RtfKStr: '\picscalex'    ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfPicScaleY;      RtfKStr: '\picscaley'    ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfPicScaled;      RtfKStr: '\picscaled'    ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfPicCropTop;     RtfKStr: '\piccropt'     ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfPicCropBottom;  RtfKStr: '\piccropb'     ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfPicCropLeft;    RtfKStr: '\piccropl'     ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfPicCropRight;   RtfKStr: '\piccropr'     ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfPixelBits;      RtfKStr: '\wbmbitspixel' ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfBitmapPlanes;   RtfKStr: '\wbmplanes'    ),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfBitmapWid;      RtfKStr: '\wbmwidthbytes'),
    (RtfKMajor: RtfPictAttr;    RtfKMinor: RtfPicBinary;      RtfKStr: '\bin'          ),
    (RtfKMajor: RtfNeXTGrAttr;  RtfKMinor: RtfNeXTGWidth;     RtfKStr: '\width'        ),
    (RtfKMajor: RtfNeXTGrAttr;  RtfKMinor: RtfNeXTGHeight;    RtfKStr: '\height'       ),
    }
  {@indent on}

const
  TRtfAnyType: TRtfTokenSet =[etDataset..etLitTrue];

function ExtractFieldName(const Fields: string; var Pos: integer): string;
var i: integer;
begin
  i := Pos;
  while(i <= Length(Fields))and(Fields[i] <> '.')do Inc(i);
  Result := Trim(Copy(Fields, Pos, i - Pos));
  if(i <= Length(Fields))and(Fields[i] = '.')
  then Inc(i);
  Pos := i;
end;

function Pwr(const Base, Exponent: Double): Double;
begin
  if Exponent = 0.0 then
    Result := 1.0 { n**0 = 1 }
  else if(Base = 0.0)and(Exponent > 0.0) then
    Result := 0.0 { 0**n = 0, n > 0 }
  else if (Frac(Exponent) = 0.0) and (Abs(Exponent) <= MaxInt) then
    Result := IntPower(Base, integer(Trunc(Exponent)))
  else
    Result := Exp(Exponent * Ln(Base))
end;

{ TRtfPictureAttr }

constructor TRtfPictureAttr.Create(AWidth, AHeigth: word);
begin
  inherited Create;
  FScaleX := 100;
  FScaleY := 100;
  FWidth := AWidth;
  FHeigth := AHeigth;
  FWidthmm := AWidth * 0.264596930676;
  FHeigthmm := AHeigth * 0.264596930676;

  FBorderWidth := 0;
  FBorderType := brNone;
  FBorderColor := clBlack;
  FProportional := true;
end;

procedure TRtfPictureAttr.SetScaleX(Value: integer);
var
  AScale: Double;
begin
  if Value < 1 then
    Value := 1
  else if Value > 100 then
    Value := 100;
  AScale := Double(Value) / FScaleX;
  FScaleX := Value;
  FWidth := Round(FWidth * AScale);
  FWidthmm := FWidthmm * AScale;
  if FProportional then
  begin
    FScaleY := Round(FScaleY * AScale);
    FHeigth := Round(FHeigth * AScale);
    FHeigthmm := FHeigthmm * AScale;
  end;
end;

procedure TRtfPictureAttr.SetScaleY(Value: integer);
var
  AScale: Double;
begin
  if Value < 1 then
    Value := 1
  else if Value > 100 then
    Value := 100;
  AScale := Double(Value) / FScaleY;
  FScaleY := Value;
  FHeigth := Round(FHeigth * AScale);
  FHeigthmm := FHeigthmm * AScale;
  if FProportional then
  begin
    FScaleX := Round(FScaleX * AScale);
    FWidth := Round(FWidth * AScale);
    FWidthmm := FWidthmm * AScale;
  end;
end;


{ TRtfItem }

procedure TRtfItem.Assign(ASource: TRtfItem);
begin
  RtfClass := ASource.RtfClass;
  RtfMajor := ASource.RtfMajor;
  RtfMinor := ASource.RtfMinor;
  RtfTextBuf := ASource.RtfTextBuf;
end;

function TRtfItem.CheckItem(AClass: TRtfClass; Major: integer): boolean;
begin
  Result :=(RtfClass = AClass)and(RtfMajor = Major);
end;

function TRtfItem.GetItem(Index: integer): TRtfItem;
begin
  Result := TRtfItem(inherited Items[Index]);
end;

procedure TRtfItem.Notify(Ptr: pointer; Action: TListNotification);
begin
  inherited;
  case Action of
    lnAdded: TRtfItem(Ptr).FParent := Self;
    lnExtracted: TRtfItem(Ptr).FParent := nil;
  end;
end;

{ TRtfItemList }

constructor TRtfItemList.Create;
//Initialize an empty list
begin
  inherited Create;
end;

destructor TRtfItemList.Destroy;
{-Destroy a list}
var n: TRtfItem;
  p: TRtfItem;
begin
  n := FTail;
  while n <> nil do begin
    {Get TRtfItem to previous node}
    p := n.FPrev;
    {Deallocate and destroy this node}
    n.Free;
    {Do the previous node}
    n := p;
  end;
  FTail := nil;
  FHead := nil;
  FCount := 0;
  inherited Destroy;
end;

function TRtfItemList.Add(AItem: TRtfItem): TRtfItem;
{-Add element to end of list}
begin
  Result := AItem;
  {Exit for bad input}
  if AItem = nil
  then Exit;
  AItem.FPrev := FTail;
  AItem.FNext := nil;
  if FHead = nil then begin
    {Special case for first node}
    FHead := AItem;
    FTail := AItem;
  end else begin
    {Add at end of existing list}
    FTail.FNext := AItem;
    FTail := AItem;
  end;
  Inc(FCount);
end;

procedure TRtfItemList.Insert(AItem: TRtfItem);
{-Insert element at start of list}
begin
  {Exit for bad input}
  if AItem = nil
  then Exit;
  AItem.FPrev := nil;
  AItem.FNext := FHead;
  if FHead = nil
  then FTail := AItem {Special case for first node}
  else FHead.FPrev := AItem; {Add at start of existing list}
  FHead := AItem;
  Inc(FCount);
end;

procedure TRtfItemList.PlaceAfter(AItem: TRtfItem; AAfter: TRtfItem);
{-Place element P into list _after_ existing element L}
begin
  {Exit for bad input}
  if(AItem = nil)or(AItem = AAfter)
  then Exit;
  if AAfter = nil
  then Insert(AItem)
  else if AAfter = FTail
  then Add(AItem)
  else begin
    AItem.FPrev := AAfter;
    AItem.FNext := AAfter.FNext;
    AAfter.FNext.FPrev := AItem;
    AAfter.FNext := AItem;
    Inc(FCount);
  end;
end;

procedure TRtfItemList.PlaceBefore(AItem, ABefore: TRtfItem);
{-Place element P into list _before_ existing element L}
begin
  {Exit for bad input}
  if(AItem = nil)or(AItem = ABefore)
  then Exit;
  if(ABefore = nil)or(ABefore = Head)
  then Insert(AItem) {Place the new element at the start of the list}
  else begin
    {Patch in the new element}
    AItem.FNext := ABefore;
    AItem.FPrev := ABefore.FPrev;
    ABefore.FPrev.FNext := AItem;
    ABefore.FPrev := AItem;
    Inc(FCount);
  end;
end;

procedure TRtfItemList.dlRemove(AItem: TRtfItem; DisposeIt: boolean);
{-Delete existing node from list, optionally disposing of it}
var This: TRtfItem;
begin
  {Exit for bad input}
  if(AItem = nil)or(FCount = 0)
  then Exit;

  This := AItem;
  with This do begin
    {Fix pointers of surrounding nodes}
    if FNext <> nil
    then FNext.FPrev := FPrev;
    if FPrev <> nil
    then FPrev.FNext := FNext;
  end;

  {Fix head and tail of list}
  if FTail = This
  then FTail := FTail.FPrev;
  if FHead = This
  then FHead := FHead.FNext;

  Dec(FCount);
  if DisposeIt
  then This.Free;
end;

procedure TRtfItemList.Extract(AItem: TRtfItem);
{-Extract existing element from list without disposing of it}
begin
  dlRemove(AItem, false);
end;

procedure TRtfItemList.Delete(AItem: TRtfItem);
{-Delete an existing node, disposing of its contents}
begin
  dlRemove(AItem, true);
end;

procedure TRtfItemList.Clear;
begin
  while Assigned(FHead)do begin
    dlRemove(FHead, true);
  end;
end;

procedure TRtfItemList.SaveToStream(AStream: TStream; AColors: string);
var AItem: TRtfItem;
  ALine: string;
begin
  AItem := Head;
  while Assigned(AItem)do begin
    if AItem.CheckItem(RtfControl, RtfDestination)and(AItem.RtfMinor = RtfColorTbl)
    then ALine := Format('\colortbl;%s}',[AColors])
    else ALine := AItem.RtfTextBuf;
    if ALine <> ''
    then AStream.Write(ALine[1], Length(ALine));
    AItem := AItem.Next;
  end;
end;

{ TRawRtfParser }

procedure TRawRtfParser.RtfHook;
//Build a tree from the rtf-code tokens
var AItem: TRtfItem;


  function NewItem(AText: string): TRtfItem;
  begin
    Result := TRtfItem.Create;
    Result.RtfClass := RtfClass;
    Result.RtfMajor := RtfMajor;
    Result.RtfMinor := RtfMinor;
    Result.RtfTextBuf := AText;
  end;
begin
  case RtfClass of
    RtfParseBegin: begin
      if Assigned(AParseItem) then begin
        //Then close it
        AParseItem := nil;
        AItem := RawItems.Add(NewItem(''));
        AItem.RtfClass := RtfParseEnd;
      end else begin
        //Otherwise open an item
        AParseItem := RawItems.Add(NewItem(''));
        AParseItem.RtfClass := RtfParseBegin;
      end;
    end;
    RtfControl: begin
      if(RtfMajor = RtfDestination)and(RtfMinor = RtfColorTbl)
      then FColorTable := RawItems.Add(NewItem(RtfTextBuf))
      else RawItems.Add(NewItem(RtfTextBuf));
    end;
    RtfText: begin
      if RawItems.Tail.RtfClass = RtfClass
      then RawItems.Tail.RtfTextBuf := RawItems.Tail.RtfTextBuf + RtfTextBuf
      else RawItems.Add(NewItem(RtfTextBuf));
    end;
    RtfGroup, RtfUnknown: begin
      if RawItems.Tail.RtfClass in[RtfGroup, RtfUnknown]
      then RawItems.Tail.RtfTextBuf := RawItems.Tail.RtfTextBuf + RtfTextBuf
      else RawItems.Add(NewItem(RtfTextBuf));
      RawItems.Tail.RtfClass := RtfUnknown; //Dont use these for compares
    end;
  end;
end;

function TRawRtfParser.GetRtfChar: char;
begin
  if APtr >= AEnd
  then Result := #0
  else begin
    Result := APtr^;
    Inc(APtr);
  end;
end;

procedure TRawRtfParser.GetRtfToken;
var c: char;
  i, ALevel: integer;
begin
  FRtfTextBuf := ''; //not really needed

  //Get first character, which may be a pushback from previous token
  if APushedChar <> #0 then begin
    c := APushedChar;
    APushedChar := #0;
  end
  else c := GetRtfChar;


  case c of
    #0: begin
      FRtfClass := RtfEof;
      FRtfTextBuf := '';
    end;
    #8: begin //effectively a \tab control symbol
      FRtfClass := RtfControl;
      FRtfMajor := RtfSpecialChar;
      FRtfMinor := RtfTab;
      FRtfTextBuf := c;
      c := #0;
    end;
    '{': begin
      FRtfClass := RtfGroup;
      FRtfMajor := RtfBeginGroup;
      FRtfTextBuf := c;
      c := #0;
    end;
    '}': begin
      FRtfClass := RtfGroup;
      FRtfMajor := RtfEndGroup;
      FRtfTextBuf := c;
      c := #0;
    end;
    '\': begin //We have the backslash, advance to next character
      FRtfTextBuf := c;
      c := GetRtfChar;
      case c of
        chr(39): begin //Hex encoded text char, e.g., \'d5, \'d3
          FRtfClass := RtfUnknown;
          FRtfTextBuf := FRtfTextBuf + c;
          c := GetRtfChar;
          if c <> #0 then begin
            FRtfTextBuf := FRtfTextBuf + c;
            c := GetRtfChar;
            if c <> #0 then begin
              FRtfClass := RtfText;
              FRtfTextBuf := FRtfTextBuf + c;
              //It can still be a special character..
              for i := Low(Rtfkey)to High(Rtfkey)do begin
                if FRtfTextBuf = Rtfkey[i].RtfKStr then begin
                  FRtfClass := RtfControl;
                  FRtfMajor := Rtfkey[i].RtfKMajor;
                  FRtfMinor := Rtfkey[i].RtfKMinor;
                  break;
                end;
              end;
              c := #0;
            end;
          end;
        end;
        ':', '{', '}', ';', '\': begin //special escaped text char, e.g., \, \;
          FRtfTextBuf := FRtfTextBuf + c;
          //"\" Marks the start and end of an expression with the RtfParseBegin
          //RtfClass. They will later be removed and replaced by RtfExpression
          //Items between the opening and closing \ will be the expression text
          if c = '\'
          then FRtfClass := RtfParseBegin
          else FRtfClass := RtfText;
          c := #0;
        end;
        '|': begin
          FRtfTextBuf := FRtfTextBuf + c;
          FRtfClass := RtfControl;
          FRtfMajor := RtfSpecialChar;
          FRtfMinor := RtfFormula;
          c := #0;
        end;
        '~': begin
          FRtfTextBuf := FRtfTextBuf + c;
          FRtfClass := RtfControl;
          FRtfMajor := RtfSpecialChar;
          FRtfMinor := RtfNoBrkSpace;
          c := #0;
        end;
        '-': begin
          FRtfTextBuf := FRtfTextBuf + c;
          FRtfClass := RtfControl;
          FRtfMajor := RtfSpecialChar;
          FRtfMinor := RtfNoReqHyphen;
          c := #0;
        end;
        '_': begin
          FRtfTextBuf := FRtfTextBuf + c;
          FRtfClass := RtfControl;
          FRtfMajor := RtfSpecialChar;
          FRtfMinor := RtfNoBrkHyphen;
          c := #0;
        end;
        '*': begin
          FRtfTextBuf := FRtfTextBuf + c;
          FRtfClass := RtfControl;
          FRtfMajor := RtfSpecialChar;
          FRtfMinor := RtfOptDest;
          c := #0;
        end;
        #13, #10: begin
          FRtfTextBuf := FRtfTextBuf + c;
          FRtfClass := RtfControl;
          FRtfMajor := RtfSpecialChar;
          FRtfMinor := RtfPar;
          c := #0;
        end;
        else begin //Wasn't anything special, continue with control word

          while(c in['A'..'Z', 'a'..'z'])do begin
            FRtfTextBuf := FRtfTextBuf + c;
            c := GetRtfChar;
          end;

          //Find the control word in the key array
          FRtfClass := RtfUnknown;
          for i := Low(Rtfkey)to High(Rtfkey)do begin
            if FRtfTextBuf = Rtfkey[i].RtfKStr then begin
              FRtfClass := RtfControl;
              FRtfMajor := Rtfkey[i].RtfKMajor;
              FRtfMinor := Rtfkey[i].RtfKMinor;
              break;
            end;
          end;

          //Parse the word parameter negative sign
          if c = '-' then begin
            FRtfTextBuf := FRtfTextBuf + c;
            c := GetRtfChar;
          end;
          //Parse the word parameter number
          while(c in['0'..'9'])do begin
            FRtfTextBuf := FRtfTextBuf + c;
            c := GetRtfChar;
          end;

          //Append control symbol delimiter (i need it for writing)
          if c = ' ' then begin
            FRtfTextBuf := FRtfTextBuf + c;
            c := GetRtfChar;
          end;

          //Crap1: Fix for {\field{\*\fldinst SYMBOL 32 \\f "Symbol" \\s 12}{\fldrslt\...
          //RTF text gets corrupted by this parser (notice the Double backslash!)
          //Crap2:Also INFO since it can contain the first line from rtf as docinfo.
          if(FRtfClass = RtfControl)and(FRtfMajor = RtfDestination) then begin
            if FRtfMinor in[RtfField, RtfInfo, RtfStyleSheet, RtfFontTbl, RtfColorTbl] then begin
              //Just include the whole field Group to the TextBuf (who cares..?)
              ALevel := 1; //All these items have a "group open" before them
              while c <> #0 do begin
                FRtfTextBuf := FRtfTextBuf + c;
                case c of
                  '{': Inc(ALevel);
                  '}': Dec(ALevel);
                end;
                c := GetRtfChar;
                if ALevel = 0
                then break;
              end;
              FRtfClass := RtfControl;
            end;
          end;
        end;
      end;
    end;
    else
    begin
      //literal text char. This will give one character per call (which is slow)
      FRtfTextBuf := c;
      FRtfClass := RtfText;

      c := GetRtfChar;
      while not(c in[#0, #8, '\', '{', '}']) do
      begin
        FRtfTextBuf := FRtfTextBuf + c;
        c := GetRtfChar;
      end;
    end;
  end;

  //Push character back if we read one to much
  if c <> #0 then
    APushedChar := c;
end;

procedure TRawRtfParser.Execute(ARawItems: TRtfItemList; ARtfPtr: pchar; ARtfSize: integer);
var AItem: TRtfItem;
begin
  APtr := ARtfPtr;
  APushedChar := #0;
  RawItems := ARawItems;
  FRtfClass := RtfNothing;
  AEnd := APtr + ARtfSize;

  //Dummy item so tail of list is always assigned
  AItem := TRtfItem.Create;
  AItem.RtfClass := RtfText;
  ARawItems.Add(AItem);

  while true do
  begin
    GetRtfToken;
    if RtfClass = RtfEOF then
      break;
    RtfHook;
  end;
end;

{ TRtfVariableList }

function TRtfVariableList.Add(AName: string; AValue: variant; AToken: TRtfToken): TRtfVariable;
begin
  Result := Find(AName);
  if Assigned(Result) then
    raise TRtfException.CreateFmt('Variable %s already exists',[AName]);
  Result := TRtfVariable.Create;
  inherited Add(Result);
  Result.Name := AName;
  Result.Value := AValue;
  Result.Token := AToken;
end;

destructor TRtfVariableList.Destroy;
begin
  inherited;
end;

function TRtfVariableList.Find(AName: string): TRtfVariable;
var i: integer;
begin
  for i := 0 to Count - 1 do begin
    Result := Items[i];
    if SameText(Result.Name, AName)
    then exit;
  end;
  Result := nil;
end;

function TRtfVariableList.GetItem(Index: integer): TRtfVariable;
begin
  Result := TRtfVariable(inherited Items[Index]);
end;

{ TRtfFunctionList }

function TRtfFunctionList.GetItem(Index: integer): TRtfFunction;
begin
  Result := TRtfFunction(inherited Items[Index]);
end;

function TRtfFunctionList.Add(ATokenType: TRtfToken; AName: string; AMin, AMax: smallint; AOnexecute: TRtfFunctionExecute): TRtfFunction;
//Add a new function or even an additional token to the function list
begin
  Result := Find(AName);
  if Assigned(Result)
  then raise TRtfException.CreateFmt('Function already exists',[AName]);

  Result := TRtfFunction.Create;
  Result.Name := AName;
  Result.Min := AMin;
  Result.Max := AMax;
  Result.Token := ATokenType;
  Result.Onexecute := AOnexecute;
  inherited Add(Result);
end;

function TRtfFunctionList.Find(AName: string): TRtfFunction;
var i: integer;
begin
  for i := 0 to Count - 1 do begin
    Result := Items[i];
    if SameText(Result.Name, AName)
    then exit;
  end;
  Result := nil;
end;

{ TRtfDataset }

procedure TRtfDataset.Notify(Ptr: pointer; Action: TListNotification);
begin
  inherited;
  case Action of
    lnAdded: TRtfDataset(Ptr).FParent := Self;
    lnExtracted: TRtfDataset(Ptr).FParent := nil;
  end;
end;

function TRtfDataset.Bof: boolean;
begin
  if Dataset is TDataset then begin
    with Dataset as TDataset do begin
      Result := Bof;
    end
  end
  else if Dataset is TtiObjectList then
    Result := TableIndex = 0
  else if Dataset is TtiObject then
    Result := false
  else
    raise TRtfException.Create(rsNotImplemented);
end;

function TRtfDataset.Eof: boolean;
begin
  if Dataset is TDataset then begin
    with Dataset as TDataset do begin
      Result := Eof;
    end
  end
  else if Dataset is TtiObjectList then
    Result :=(TableIndex >= RecordCount)or(TableIndex < 0)
  else if Dataset is TtiObject then
    Result := false
  else
    Result := false;
end;

function TRtfDataset.Find(AName: string): TRtfDataset;
var i: integer;
begin
  for i := 0 to Count - 1 do begin
    Result := Items[i];
    if SameText(Result.Name, AName)
    then exit;
  end;
  Result := nil;
end;

function TRtfDataset.Add(ADataset: TObject; AName: string; AFreeDataset: boolean = false): TRtfDataset;
begin
  Result := Find(AName);
  if Assigned(Result) then
    raise TRtfException.CreateFmt('Dataset already exists',[AName]);

  Result := TRtfDataset.Create;
  Result.Dataset := ADataset;
  Result.Name := AName;
  Result.FFreeDataset := AFreeDataset;
  inherited Add(Result);
end;

function TRtfDataset.ResolveNestedFields(ATable: TRtfDataset; AName: string; var AFieldName: string): TRtfDataset;
//Advance to field level (skipping nested dataset objects)
var ATableName: string;
  APropInfo: PPropInfo;
  ASubTable: TRtfDataset;
  AIndex, i: integer;
  AObject: TObject;
begin
  AIndex := 1;
  Result := ATable;
  AFieldName := AName;
  while AIndex < Length(AName)do begin

    //Check object dataset and eof state
    AObject := nil; 
    if Result.Dataset is TDataset
    then exit; //A TDataset is never nested
    if Result.Dataset is TtiObjectList then begin
      //Get the right record from the array
      if Result.TableIndex <(Result.Dataset as TtiObjectList).Count
      then AObject :=(Result.Dataset as TtiObjectList)[Result.TableIndex]
      else exit; //Trying beyond eof (or empty table) big problem; nah?
    end else if Result.Dataset is TtiObject
    then AObject := Result.Dataset
    else raise TRtfException.Create('Unknown object');

    //If the next field is a class then advance
    ASubTable := nil;
    ATableName := ExtractFieldName(AName, AIndex);
    APropInfo := GetPropInfo(AObject, ATableName);
    if not Assigned(APropInfo)or(APropInfo^.PropType^.Kind <> tkClass)
    then exit; //As long as it's an object continue parsing..

    //Advance fieldname and find nested table
    AFieldName := Copy(AName, AIndex, Maxint);
    for i := 0 to Result.Count - 1 do begin
      if SameText(Result[i].Name, ATableName) then begin
        ASubTable := Result[i];
        break;
      end;
    end;

    //Add the nested table (for administration of the TableIndex)
    if not Assigned(ASubTable) then begin
      APropInfo := GetPropInfo(AObject, ATableName);
      if not Assigned(APropInfo)
      then raise TRtfException.CreateFmt('property %s not found',[ATableName]);
      if APropInfo^.PropType^.Kind = tkClass
      then AObject := GetObjectProp(AObject, APropInfo)
      else exit; //Just a plain property field (returned via AFieldName)
      ASubTable := Result.Add(AObject, ATableName);
    end;

    //Advance to a deeper table level
    Result := ASubTable;
  end;
end;

function TRtfDataset.Find(AName: string; var AFieldName: string): TRtfDataset;
//Find the requested dataset
var ATableName: string;
  AIndex, i: integer;
begin
  //MainTable must be in the list of tables
  //Otherwise no point of reference.
  AIndex := 1;
  Result := nil;
  ATableName := ExtractFieldName(AName, AIndex);
  AFieldName := Copy(AName, AIndex, Maxint);
  for i := 0 to Count - 1 do begin
    if SameText(Items[i].Name, ATableName) then begin
      Result := Items[i];
      break;
    end;
  end;
  if not Assigned(Result)or not Assigned(Result.Dataset)
  then raise TRtfException.CreateFmt('Unable to resolve %s',[ATableName]);
  Result := ResolveNestedFields(Result, AFieldName, AFieldName);
end;

procedure TRtfDataset.Open;
begin
  Clear; //Clear nested tables
  if Dataset is TDataset then begin
    with Dataset as TDataset do begin
      if not Active
      then Open;
    end
  end;
  First;
end;

procedure TRtfDataset.First;
begin
  Clear; //Clear nested tables
  if Dataset is TDataset then
  begin
    with Dataset as TDataset do
    begin
      First;
    end
  end
  else if Dataset is TtiObjectList then
  begin
    TableIndex := 0;
  end;
end;

function TRtfDataset.GetItem(Index: integer): TRtfDataset;
begin
  Result := TRtfDataset(inherited Items[Index]);
end;

function TRtfDataset.IsEmpty: boolean;
begin
  if Dataset is TDataset then
  begin
    with Dataset as TDataset do
    begin
      Result := IsEmpty;
    end
  end
  else if Dataset is TtiObjectList then
    Result := RecordCount = 0
  else if Dataset is TtiObject then
    Result := false
  else
    raise TRtfException.Create(rsNotImplemented);
end;

procedure TRtfDataset.Last;
begin
  Clear; //Clear nested tables
  if Dataset is TDataset then begin
    with Dataset as TDataset do begin
      Last;
    end
  end else if Dataset is TtiObjectList then begin
    TableIndex := RecordCount - 1;
  end;
end;

procedure TRtfDataset.Next;
begin
  Clear; //Clear nested tables
  if Dataset is TDataset then begin
    with Dataset as TDataset do begin
      Next;
    end
  end else if Dataset is TtiObjectList then begin
    if not Eof then begin
      TableIndex := TableIndex + 1;
    end;
  end;
end;

procedure TRtfDataset.Prior;
begin
  Clear; //Clear nested tables
  if Dataset is TDataset then begin
    with Dataset as TDataset do begin
      Prior;
    end
  end else if Dataset is TtiObjectList then begin
    if not Bof then begin
      TableIndex := TableIndex - 1;
    end;
  end;
end;

function TRtfDataset.RecordCount: integer;
//This can give problems with Sql..
begin
  if Dataset is TDataset
  then Result :=(Dataset as TDataset).RecordCount
  else if Dataset is TtiObjectList
  then Result :=(Dataset as TtiObjectList).Count
  else Result := 1; //Single OpfRecord
end;

destructor TRtfDataset.Destroy;
begin
  if FreeDataset
  then FreeAndNil(FDataset);
  inherited;
end;


{ TRtfArgument }

constructor TRtfArgument.Create(AParser: TtiRtfParser);
begin
  inherited Create(true);
  FParser := AParser;
end;

function TRtfArgument.Add(AValue: variant; ATokenType: TRtfToken): TRtfArgument;
begin
  Result := TRtfArgument.Create(Parser);
  inherited Add(Result);
  Result.Value := AValue;
  Result.Token := ATokenType;
end;

function TRtfArgument.Add(AArgument: TRtfArgument): TRtfArgument;
begin
  inherited Add(AArgument);
  Result := AArgument;
end;

function TRtfArgument.GetItem(Index: integer): TRtfArgument;
begin
  Result := TRtfArgument(inherited Items[Index]);
end;

procedure TRtfArgument.Notify(Ptr: pointer; Action: TListNotification);
//Nice way of setting the parent reference
begin
  inherited;
  case Action of
    lnAdded: TRtfArgument(Ptr).FParent := Self;
    lnExtracted: TRtfArgument(Ptr).FParent := nil;
  end;
end;

function TRtfArgument.Check(AParam: integer; ATokens: TRtfTokenSet): boolean;
begin
  if AParam >= Count then begin
    Result := false;
    exit;
  end;
  if not(Items[AParam].Token in ATokens) then begin
    Result := false;
    exit;
  end;
  Result := true;
end;

function TRtfArgument.Check(ATokens: array of TRtfTokenSet): boolean;
var i: integer;
begin
  for i := Low(ATokens)to High(ATokens)do begin
    Result := Check(i, ATokens[i]);
    if not Result
    then exit;
  end;
  Result := true;
end;

procedure TRtfArgument.ParseExpression(AExpression: string);
//Add token to the argument list. Parameters surrounded by parentheses will
//be added to the nested arguments. This will make a nice argument tree.
//Constant .9 wont be parse, but that's ok
var FSourcePtr, p, TokenStart: PChar;
  ALastArgument, ALast: TRtfArgument;
  AInts: array[0..6]of integer;
  ASeps: array[0..6]of char;
  AFunction: TRtfFunction;
  AFloatResult: boolean;
  AIntIndex: integer;
  AValue: variant;
  AToken: string;
begin
  ALast := nil;
  Value := varEmpty;
  Token := etNothing;
  ALastArgument := Self;
  FSourcePtr := PChar(AExpression);
  while FSourcePtr^ <> #0 do begin

    p := FSourcePtr;
    while(P^ <> #0)and(P^ <= ' ')do begin
      ALast := nil;
      Inc(p);
    end;

    TokenStart := p;
    case P^ of
      'A'..'Z', 'a'..'z', '_', '@': begin //De '@' voor referentie variabelen (nieuw)
        Inc(p);
        ALast := nil;
        while P^ in['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '@']do Inc(p);
        if TokenStart^ = '@' then begin
          Inc(TokenStart); //Remove the '@' prefix
          SetString(AToken, TokenStart, p - TokenStart);
          ALast := ALastArgument.Add(AToken, etVariable)
        end else begin
          SetString(AToken, TokenStart, p - TokenStart);
          AFunction := Parser.Functions.Find(AToken);
          if Assigned(AFunction)
          then ALastArgument.Add(AFunction.Name, AFunction.Token)
          else ALast := ALastArgument.Add(AToken, etFieldName); //Field or tablename, what else can it be?
        end;
      end;
      Chr(39): begin
        Inc(p);
        TokenStart := p;
        while true do begin
          if P^ = Chr(39) then begin
            SetString(AToken, TokenStart, p - TokenStart);
            //Concatenate etFieldName."Field with spaces"
            if Assigned(ALast)and(ALast.Token in[etVariable, etFieldName])
            then ALast.Value := ALast.Value + AToken
            else ALastArgument.Add(AToken, etLitString);
            Inc(p);
            Break;
          end;
          if P^ = #0
          then raise TRtfException.Create(rsUnterminatedStringConstant);
          Inc(p);
        end;
      end;
      '"': begin
        Inc(p);
        TokenStart := p;
        while true do begin
          if P^ = '"' then begin
            SetString(AToken, TokenStart, p - TokenStart);
            //Concatenate etFieldName."Field with spaces"
            if Assigned(ALast)and(ALast.Token in[etVariable, etFieldName])
            then ALast.Value := ALast.Value + AToken
            else ALastArgument.Add(AToken, etLitString);
            Inc(p);
            Break;
          end;
          if P^ = #0
          then raise TRtfException.Create(rsUnterminatedStringConstant);
          Inc(p);
        end;
      end;
      '-': begin
        Inc(p);
        ALast := nil;
        ALastArgument.Add(AToken, etSUB);
      end;
      '0'..'9': begin //Ik ga ervan uit dat alles in het amerikaans genoteerd word
        Inc(p);
        ALast := nil;
        AFloatResult := false;
        while P^ in['0'..'9']do Inc(p);
        if P^ = '.' then begin //Floating point
          Inc(p);
          AFloatResult := true;
          while P^ in['0'..'9']do Inc(p);
        end;
        if P^ in['e', 'E'] then begin //1.700000E+308
          AFloatResult := true;
          Inc(p);
          if P^ in['+', '-']
          then Inc(p);
          while P^ in['0'..'9']do Inc(p);
        end;
        SetString(AToken, TokenStart, p - TokenStart);
        if AFloatResult
        then AValue := StrToFloat(AToken)
        else AValue := StrToInt(AToken);
        if AFloatResult
        then ALastArgument.Add(AValue, etLitFloat)
        else ALastArgument.Add(AValue, etLitInt);
      end;
      '(': begin
        Inc(p);
        ALast := nil;
        //Add them to the last function if possible..
        if(ALastArgument.Count > 0)and(ALastArgument[ALastArgument.Count - 1].Token in[etFunction, etProcedure])
        then ALastArgument := ALastArgument[ALastArgument.Count - 1]
        else ALastArgument := ALastArgument.Add('()', etParenthesis);
      end;
      ')': begin
        Inc(p);
        ALast := nil;
        ALastArgument := ALastArgument.Parent;
        if not Assigned(ALastArgument)
        then raise TRtfException.Create(rsToManyClosingParenthesis);
      end;
      '<': begin
        Inc(p);
        ALast := nil;
        case P^ of
          '=': begin
            Inc(p);
            ALastArgument.Add('<=', etLE);
          end;
          '>': begin
            Inc(p);
            ALastArgument.Add('<>', etNE);
          end;
          else ALastArgument.Add('<', etLT);
        end;
      end;
      '=': begin
        Inc(p);
        ALast := nil;
        ALastArgument.Add('=', etEq);
      end;
      ':': begin
        Inc(p);
        ALast := nil;
        if P^ = '=' then begin
          Inc(p);
          ALastArgument.Add(':=', etAssign);
        end
        else raise TRtfException.CreateFmt(rsInvalidExpressionCharacter,[P^]);
      end;
      '&': begin
        Inc(p);
        ALast := nil;
        if P^ = '&' then begin
          Inc(p);
          ALastArgument.Add('and', etAnd);
        end
        else raise TRtfException.CreateFmt(rsInvalidExpressionCharacter,[P^]);
      end;
      '|': begin
        Inc(p);
        ALast := nil;
        if P^ = '|' then begin
          Inc(p);
          ALastArgument.Add('or', etOr);
        end
        else raise TRtfException.CreateFmt(rsInvalidExpressionCharacter,[P^]);
      end;
      '!': begin
        Inc(p);
        ALast := nil;
        if P^ = '=' then begin
          Inc(p);
          ALastArgument.Add('!=', etNe);
        end
        else ALastArgument.Add(AToken, etNot);
      end;
      '>': begin
        Inc(p);
        ALast := nil;
        if P^ = '=' then begin
          Inc(p);
          ALastArgument.Add('>=', etGE);
        end
        else ALastArgument.Add('>', etGT);
      end;
      '+': begin
        Inc(p);
        ALast := nil;
        ALastArgument.Add('+', etADD);
      end;
      '*': begin
        Inc(p);
        ALast := nil;
        ALastArgument.Add('*', etMUL);
      end;
      '/': begin
        Inc(p);
        ALast := nil;
        ALastArgument.Add('/', etDIV);
      end;
      ',': begin
        Inc(p);
        ALast := nil;
        ALastArgument.Add(',', etComma);
      end;
      '\': begin //a date/time constant. Since these are
      //in a fixed format we need to parse them ourselves.
        Inc(p);
        ALast := nil;
        if P^ = '{' then begin
          Inc(p);
          FillChar(AInts[0], SizeOf(AInts), 0);
          FillChar(ASeps[0], SizeOf(ASeps), 0);

          AIntIndex := 0;
          while(p^ <> #0)and(p^ in['0'..'9', ' ', ':', '-'])do begin
            //read integer parts
            TokenStart := p;
            while P^ in['0'..'9']do Inc(p);
            SetString(AToken, TokenStart, p - TokenStart);
            if AToken = ''
            then raise TRtfException.Create(rsInvalidDateConstant);
            ASeps[AIntIndex] := p^;
            AInts[AIntIndex] := StrToInt(AToken);
            Inc(AIntIndex);
            if AIntIndex > 6
            then raise TRtfException.Create(rsInvalidDateConstant);
            if(p^ in[' ', '-', ':'])
            then Inc(p);
          end;

          //There shoud be at least two values entered
          if AIntIndex < 2
          then raise TRtfException.Create(rsInvalidDateConstant);

          if not(p^ in['\'])
          then raise TRtfException.Create(rsInvalidDateConstant);
          Inc(p);
          if P^ <> '}'
          then raise TRtfException.Create(rsInvalidDateConstant);
          Inc(p);

          if ASeps[0] = '-' then begin
            //Its a date/time constant.. There should be at least 3 ints
            if AIntIndex < 3
            then raise TRtfException.Create(rsInvalidDateConstant);
            if ASeps[1] <> '-'
            then raise TRtfException.Create(rsInvalidDateSeparator);
            if not(ASeps[2]in[' ', '\'])
            then raise TRtfException.Create(rsInvalidDateSeparator);
            AValue := EncodeDate(AInts[0], AInts[1], AInts[2]);

            //Followed by a optional time constant? There should be at least 2 ints in it
            if AIntIndex > 3 then begin
              if AIntIndex < 5
              then raise TRtfException.Create(rsInvalidTimeConstant);
              if not(ASeps[2]in[' '])
              then raise TRtfException.Create(rsInvalidTimeSeparator);
              if not(ASeps[3]in[':', ' ', '\'])
              then raise TRtfException.Create(rsInvalidTimeSeparator);
              AValue := AValue + EncodeTime(AInts[3], AInts[4], AInts[5], 0);
            end;
          end else if ASeps[0] = ':' then begin
            //Its a time constant.. There should be at least 2 ints
            if AIntIndex < 1
            then raise TRtfException.Create(rsInvalidTimeConstant);
            if not(ASeps[1]in[':', ' ', '\'])
            then raise TRtfException.Create(rsInvalidTimeSeparator);
            AValue := EncodeTime(AInts[0], AInts[1], AInts[2], 0);
          end;
          ALastArgument.Add(AValue, etLitDate)
        end
        else raise TRtfException.CreateFmt(rsInvalidExpressionCharacter,[P^]);
      end;
      else begin
        if p^ <> #0
        then raise TRtfException.CreateFmt(rsInvalidExpressionCharacter,[P^]);
      end;
    end;
    FSourcePtr := p;
  end;
  if ALastArgument <> Self
  then raise TRtfException.Create(rsExpectedClosingParenthesis);
end;

procedure TRtfArgument.EvaluateUnaryBinary(APrevItem, AArgument, ANextItem: TRtfArgument);
begin
  case APrevItem.Token of
    etLitString: begin
      case ANextItem.Token of
        etLitString: begin
          if AArgument.Token <> etAdd
          then raise TRtfException.Create('Unable to combine strings with other types');
          APrevItem.Value := APrevItem.Value + ANextItem.Value;
          Remove(AArgument);
          Remove(ANextItem);
        end;
        else raise TRtfException.Create('Unable to combine strings with other types');
      end;
    end;
    etLitInt: begin
      case ANextItem.Token of
        etLitInt, etLitDate, etLitFloat: begin
          case AArgument.Token of
            etAdd: APrevItem.Value := APrevItem.Value + ANextItem.Value;
            etSub: APrevItem.Value := APrevItem.Value - ANextItem.Value;
            etMul: APrevItem.Value := APrevItem.Value * ANextItem.Value;
            etDiv: APrevItem.Value := APrevItem.Value / ANextItem.Value;
          end;
          if ANextItem.Token = etLitDate
          then APrevItem.Token := etLitDate
          else if ANextItem.Token = etLitFloat
          then APrevItem.Token := etLitFloat
          else if AArgument.Token = etDiv
          then APrevItem.Token := etLitFloat
          else APrevItem.Token := etLitInt;
          Remove(AArgument);
          Remove(ANextItem);
        end;
        else raise TRtfException.Create('Invalid formula');
      end;
    end;
    etLitFloat: begin
      case ANextItem.Token of
        etLitInt, etLitFloat, etLitDate: begin
          case AArgument.Token of
            etAdd: APrevItem.Value := APrevItem.Value + ANextItem.Value;
            etSub: APrevItem.Value := APrevItem.Value - ANextItem.Value;
            etMul: APrevItem.Value := APrevItem.Value * ANextItem.Value;
            etDiv: APrevItem.Value := APrevItem.Value / ANextItem.Value;
          end;
          if ANextItem.Token = etLitDate
          then APrevItem.Token := etLitDate
          else APrevItem.Token := etLitFloat;
          Remove(AArgument);
          Remove(ANextItem);
        end;
        else raise TRtfException.Create('Invalid formula');
      end;
    end;
    etLitDate: begin
      case ANextItem.Token of
        etLitInt, etLitFloat, etLitDate: begin
          case AArgument.Token of
            etAdd: APrevItem.Value := APrevItem.Value + ANextItem.Value;
            etSub: APrevItem.Value := APrevItem.Value - ANextItem.Value;
            etMul: APrevItem.Value := APrevItem.Value * ANextItem.Value;
            etDiv: APrevItem.Value := APrevItem.Value / ANextItem.Value;
          end;
          APrevItem.Token := etLitDate;
          Remove(AArgument);
          Remove(ANextItem);
        end;
        else raise TRtfException.Create('Invalid formula');
      end;
    end;
    etComma: begin
    //Fix for constant int's and float's
      case ANextItem.Token of
        etLitInt, etLitFloat, etLitDate: begin
          case AArgument.Token of
            etAdd:; //nothing just a (..., +Value)
            etSub: ANextItem.Value := ANextItem.Value * - 1;
            else raise TRtfException.Create('Invalid formula');
          end;
          Remove(AArgument);
        end;
        else raise TRtfException.Create('Invalid formula');
      end;
    end;
    etAssign: begin
    //Fix for constant int's and float's
      case ANextItem.Token of
        etLitInt, etLitFloat, etLitDate: begin
          case AArgument.Token of
            etAdd:; //nothing just a @Variable := +Value
            etSub: ANextItem.Value := ANextItem.Value * - 1;
            else raise TRtfException.Create('Invalid formula');
          end;
          Remove(AArgument);
        end;
        else raise TRtfException.Create('Invalid formula');
      end;
    end;
    else raise TRtfException.Create('Invalid formula'); //Otherwise it wont advance
  end;
  if Assigned(Parser.OnEvalutate)
  then Parser.OnEvalutate(Self);
end;

procedure TRtfArgument.EvaluateComparison(APrevItem, AArgument, ANextItem: TRtfArgument);
begin
  if(APrevItem.Token = etLitString)and(ANextItem.Token <> etLitString)
  then raise TRtfException.Create('Invalid comparison');
  if(ANextItem.Token = etLitString)and(APrevItem.Token <> etLitString)
  then raise TRtfException.Create('Invalid comparison');

  case AArgument.Token of
    etEQ: APrevItem.Value := APrevItem.Value = ANextItem.Value;
    etNE: APrevItem.Value := APrevItem.Value <> ANextItem.Value;
    etGE: APrevItem.Value := APrevItem.Value >= ANextItem.Value;
    etLE: APrevItem.Value := APrevItem.Value <= ANextItem.Value;
    etGT: APrevItem.Value := APrevItem.Value > ANextItem.Value;
    etLT: APrevItem.Value := APrevItem.Value < ANextItem.Value;
    etOr: APrevItem.Value := APrevItem.Value or ANextItem.Value;
    etAnd: APrevItem.Value := APrevItem.Value and ANextItem.Value;
    else raise TRtfException.Create('Invalid comparison');
  end;
  if APrevItem.Value
  then APrevItem.Token := etLitTrue
  else APrevItem.Token := etLitFalse;

  Remove(AArgument);
  Remove(ANextItem);
  if Assigned(Parser.OnEvalutate)
  then Parser.OnEvalutate(Self);
end;

procedure TRtfArgument.EvaluateAssign(APrevItem, AArgument, ANextItem: TRtfArgument);
var AVariable: TRtfVariable;
begin
  if APrevItem.Token <> etVariable
  then raise TRtfException.Create('Invalid assignment');

  AVariable := Parser.Variables.Find(APrevItem.Value);
  if Assigned(AVariable) then begin
    AVariable.Value := ANextItem.Value;
    AVariable.Token := ANextItem.Token;
  end
  else AVariable := Parser.Variables.Add(APrevItem.Value, ANextItem.Value, ANextItem.Token);

  APrevItem.Value := AVariable.Value;
  APrevItem.Token := AVariable.Token;

  Remove(AArgument);
  Remove(ANextItem);
  if Assigned(Parser.OnEvalutate)
  then Parser.OnEvalutate(Self);
end;

procedure TRtfArgument.GetPictureData(ABuffer: pointer; ALength: cardinal; var Result: string);
var ASrc: Pbyte;
  ADst: pchar;
  AIndex: cardinal;
  i: integer;
  s: string;
begin
  if poBinary in Parser.PictureOptions then begin
    //Save graphic binary
    Result := Format('%s\bin%d ',[Result, ALength]);
    AIndex := Length(Result);
    SetLength(Result, AIndex + ALength);
    ADst := @Result[AIndex + 1];
    System.Move(ABuffer^, ADst^, ALength);
  end else begin
    //Save graphics as hex
    ASrc := ABuffer;
    Result := Result + ' ';
    AIndex := Length(Result);
    SetLength(Result, AIndex + 2 *(ALength +(ALength div 128)));
    ADst := @Result[AIndex + 1];
    for i := 0 to ALength - 1 do begin
      s := IntToHex(ASrc^, 2);
      ADst^ := s[1];
      Inc(ADst);
      ADst^ := s[2];
      Inc(ADst);
      if(i > 0)and(i and 127 = 0) then begin
        ADst^ := #13;
        Inc(ADst);
        ADst^ := #10;
        Inc(ADst);
      end;
      Inc(ASrc);
    end;
  end;
  Result := Result + #13#10 + '}';
end;

function TRtfArgument.GetPicture(APicture: TPicture): string;
//Convert a picture to a string suitable for in the Rtf
(*
var
  xw, yh: word;
  Rect: TRect;
  ARefDC: HDC;
  ABuffer: pointer;
  ALength: cardinal;
  AMetafile: TMetafile;
  ABorderWidth: integer;
  APictureAttr: TRtfPictureAttr;
  ppi, AColorIndex, xtw, ytw: integer;
*)
begin
  Result := '';
  (*
  ppi := Screen.PixelsPerInch;

  AMetafile := TMetaFile.Create;
  try
    AMetafile.Enhanced := true;

    if poMetafile in Parser.PictureOptions then begin
      AMetafile.Width := Round(APicture.Graphic.Width * ppi / 96);
      AMetafile.Height := Round(APicture.Graphic.Height * ppi / 96);
    end else begin
      if(APicture.graphic is TMetafile)and not APicture.Metafile.Enhanced then begin
        AMetafile.Height := APicture.graphic.Height;
        AMetafile.Width := APicture.graphic.Width;
      end else begin
        AMetafile.Height := Round(APicture.graphic.Height * ppi / 96);
        AMetafile.Width := Round(APicture.graphic.Width * ppi / 96);
      end;
    end;

    Rect.Top := 0;
    Rect.Left := 0;
    Rect.Right := AMetafile.Width - 1;
    Rect.Bottom := AMetafile.Height - 1;

    with TMetaFileCanvas.Create(AMetafile, 0)do try
      if ppi = 96
      then Draw(0, 0, APicture.Graphic)
      else StretchDraw(Rect, APicture.Graphic);
    finally
      Free;
    end;

    if(APicture.Graphic is TMetafile)and not APicture.Metafile.Enhanced then begin
      xw := Round(APicture.Width * 96 / ppi);
      yh := Round(APicture.Height * 96 / ppi);
    end else begin
      xw := APicture.Width;
      yh := APicture.Height;
    end;

    APictureAttr := TRtfPictureAttr.Create(xw, yh);
    try
      AMetafile.MMWidth := Round(APictureAttr.Widthmm * 100);
      AMetafile.MMHeight := Round(APictureAttr.Heigthmm * 100);

      if poMetafile in Parser.PictureOptions then begin
        xtw := Round(APictureAttr.Width * 26.4596930676);
        ytw := Round(APictureAttr.Heigth * 26.4596930676);
      end else begin
        xtw := Round(5669 * APictureAttr.Widthmm / 100);
        ytw := Round(5669 * APictureAttr.Heigthmm / 100);
      end;

      if Assigned(Parser.OnPictureAttr)
      then Parser.OnPictureAttr(APictureAttr);

      Result := Format('{\pict\picscalex%d\picscaley%d\piccropl0' +
        '\piccropr0\piccropt0\piccropb0\picw%d\pich%d',
        [APictureAttr.ScaleX, APictureAttr.ScaleY, xtw, ytw]);
      if poMetafile in Parser.PictureOptions
      then Result := Result + '\wmetafile8'
      else Result := Result + '\emfblip';

      if(APictureAttr.BorderType <> brNone)and(APictureAttr.BorderWidth > 0) then begin
        case APictureAttr.BorderType of
          brSingle: Result := Result + '\brdrs';
          brDouble: Result := Result + '\brdrdb';
          brThick: Result := Result + '\brdrth';
          brShadow: Result := Result + '\brdrsh';
          brDot: Result := Result + '\brdrdot';
          brHair: Result := Result + '\brdrhair';
        end;

        ABorderWidth := APictureAttr.BorderWidth;
        if ABorderWidth > 75
        then ABorderWidth := 75;

        AColorIndex := Parser.ColorList.UseColor(APictureAttr.BorderColor);
        Result := Format('%s\brdrw%d\brdrcf%d',[Result, ABorderWidth, AColorIndex]);
      end;
    finally
      APictureAttr.Free;
    end;


    if poMetafile in Parser.PictureOptions then begin
      ARefDC := GetDC(0);
      try
        ALength := GetWinMetaFileBits(AMetafile.Handle, 0, nil, MM_ANISOTROPIC, ARefDC);
        GetMem(ABuffer, ALength);
        try
          GetWinMetaFileBits(AMetafile.Handle, ALength, ABuffer, MM_ANISOTROPIC, ARefDC);
          GetPictureData(ABuffer, ALength, Result);
        finally
          FreeMem(ABuffer);
        end;
      finally
        ReleaseDC(0, ARefDc);
      end;
    end else begin
      ALength := GetEnhMetaFileBits(AMetafile.Handle, 0, nil);
      GetMem(ABuffer, ALength);
      try
        GetEnhMetaFileBits(AMetafile.Handle, ALength, ABuffer);
        GetPictureData(ABuffer, ALength, Result);
      finally
        FreeMem(ABuffer);
      end;
    end;
  finally
    AMetafile.Free;
  end;
  *)
end;

procedure TRtfArgument.GetGraphicsValue(ADataset: TRtfDataset; AFieldName: string);
(*
var
  APicture: TPicture;
  ABlob: TBlobField;
  APhoto: TJPEGImage;
  AStream: TStringStream;
  s: string[20];
  AField: TField;
*)
begin
(*
  Value := varEmpty;
  Token := etNothing;
  if ADataset.Dataset is TDataset then begin
    AField :=(ADataset.Dataset as TDataset).FieldByName(AFieldName);

    APicture := TPicture.Create;
    try
      ABlob := AField as TBlobField;
      s := Copy(ABlob.Value, 1, 20);
      if(Pos('GIF8', s) > 0)or(Pos('JFIF', s) > 0) then begin
        APhoto := TJPEGImage.Create;
        try
          AStream := TStringStream.Create(ABlob.AsString);
          APhoto.LoadFromStream(AStream);
          APicture.Assign(APhoto);
        finally
          FreeAndNil(APhoto);
          FreeAndNil(AStream);
        end;
      end
      else APicture.Assign(ABlob);

      if Assigned(APicture.Graphic) then begin
        Value := GetPicture(APicture);
        Token := etLitString;
      end;

    finally
      APicture.Free;
    end;
  end
  else
    raise TRtfException.Create('Graphic fields worden alleen ondersteund via de tdataset!');
*)
end;

procedure TRtfArgument.ResolveFieldValue(ADataset: TRtfDataset; AFieldName: string);
//Get field data from a dataset object
var
  AObject: TObject;
  APropInfo: PPropInfo;
  ATypedata: PTypeData;
  ATable: TDataset;
  AField: TField;
begin
  //Can't resolve that
  if AFieldName = '' then
    raise TRtfException.Create('invalid fieldname');

  if ADataset.Dataset is TDataset then
  begin
    ATable := ADataset.Dataset as TDataset;
    AField := ATable.FieldByName(AFieldName);
    case AField.DataType of
      ftMemo, ftFmtMemo, ftFixedChar, ftWideString, ftString: begin
        Token := etLitString;
        Value := AField.AsString;
      end;
      ftLargeint, ftAutoInc, ftSmallint, ftInteger, ftWord: begin
        Token := etLitInt;
        Value := AField.AsInteger;
      end;
      ftBoolean: begin
        Value := AField.AsBoolean;
        if Value then
          Token := etLitTrue
        else
          Token := etLitFalse;
      end;
      ftFloat, ftCurrency, ftBCD: begin
        Token := etLitFloat;
        Value := AField.AsFloat;
      end;
      ftDate, ftTime, ftDateTime, ftTimeStamp: begin
        Token := etLitDate;
        Value := AField.AsDateTime;
      end;
      ftGraphic: begin
        GetGraphicsValue(ADataset, AFieldName);
      end;
      else
        raise TRtfException.CreateFmt('Unable to convert field "%s" value',[AFieldName]);
    end;
  end
  else
  begin
    AObject := ADataset.Dataset;
    if AObject is TtiObjectList then
    begin
      //Get the right record from the array
      if ADataset.TableIndex <(AObject as TtiObjectList).Count then
        AObject :=(AObject as TtiObjectList)[ADataset.TableIndex]
      else
      begin //Trying beyond eof (or empty dataset) big problem; nah?
        Token := etNothing;
        Value := varEmpty;
        exit;
      end;
    end;
    APropInfo := GetPropInfo(AObject, AFieldName);
    if not Assigned(APropInfo) then
      raise TRtfException.CreateFmt('Field "%s" does not exist',[AFieldName]);
    if not Assigned(APropInfo^.GetProc) then
      raise TRtfException.CreateFmt('Cannot access field "%s"',[AFieldName]);
    {$IFDEF FPC}
    ATypeData := GetTypeData(APropInfo^.PropType);
    {$ELSE}
    ATypeData := GetTypeData(APropInfo^.PropType^);
    {$ENDIF}

    case APropInfo^.PropType^.Kind of
      tkChar, tkString, tkLString, tkWString{$IFDEF FPC},tkAString{$ENDIF}: begin
        Token := etLitString;
        Value := GetStrProp(AObject, APropInfo);
      end;
      tkInt64: begin
        Token := etLitInt;
        Value := GetInt64Prop(AObject, APropInfo);
      end;
      tkSet, tkInteger: begin
        Token := etLitInt;
        Value := GetOrdProp(AObject, APropInfo);
      end;
      tkEnumeration: begin
        {$IFDEF FPC}
        if ATypeData^.BaseType = TypeInfo(boolean) then
        {$ELSE}
        if ATypeData^.BaseType^ = TypeInfo(boolean) then
        {$ENDIF}
        begin
          Value := GetOrdProp(AObject, APropInfo) = 1;
          if Value then
            Token := etLitTrue
          else
            Token := etLitFalse;
        end
        else
        begin
          Token := etLitInt;
          Value := GetOrdProp(AObject, APropInfo);
        end;
      end;
      tkFloat: begin
        if SameText(APropInfo^.PropType^.Name, 'TDate')
            or SameText(APropInfo^.PropType^.Name, 'TTime')
            or SameText(APropInfo^.PropType^.Name, 'TDateTime') then
          Token := etLitDate
        else
          Token := etLitFloat;
        Value := GetFloatProp(AObject, APropInfo);
      end;
      else
        raise TRtfException.CreateFmt('Unable to convert field "%s" value',[AFieldName]);
    end;  { case }
  end;  { if/else }
end;

procedure TRtfArgument.ResolveVariable;
var AVariable: TRtfVariable;
  AText, AFieldName: string;
  ADataset: TRtfDataset;
  AIndex: integer;
begin
  AText := Value;
  AIndex := Pos('.', AText);
  if AIndex > 0 then begin
    //Its a dataset reference @Table.Fieldname
    AFieldName := Copy(AText, AIndex + 1, Maxint);
    AText := Copy(AText, 1, AIndex - 1);

    AVariable := Parser.Variables.Find(AText);
    if not Assigned(AVariable)
    then raise TRtfException.Create('variable does not exist');
    Token := AVariable.Token;
    Value := AVariable.Value;
    if Token <> etDataset
    then raise TRtfException.Create('variable is not a dataset');

    ADataset := TRtfDataset(ptrint(Value));
    ADataset := ADataset.ResolveNestedFields(ADataset, AFieldName, AFieldName);
    if AFieldName = '' then begin
      //Its a (nested) dataset
      Token := etDataset;
      Value := integer(ADataset);
    end
    else ResolveFieldValue(ADataset, AFieldName);
  end else begin
    AVariable := Parser.Variables.Find(AText);
    if not Assigned(AVariable)
    then raise TRtfException.Create('variable does not exist');
    Token := AVariable.Token;
    Value := AVariable.Value;
  end;
end;

procedure TRtfArgument.ResolveFieldName;
//Resolve dataset names or fieldnames (seperated with ".")
var ADataset: TRtfDataset;
  AFieldName: string;
begin
  //Otherwise it should be a table reference or fieldname
  ADataset := Parser.Datasets.Find(Value, AFieldName);
  if not Assigned(ADataset) //There should be at least a table reference.
  then raise TRtfException.CreateFmt('Dataset "%s" not found',[Value]);
  if AFieldName <> ''
  then ResolveFieldValue(ADataset, AFieldName)
  else begin
    //Its a dataset or objectlist reference
    Token := etDataset;
    Value := integer(ADataset);
  end;
end;

procedure TRtfArgument.Walk(ATokenset: TRtfTokenSet; AExecproc: TRtfArgumentEvent);
//Not really neat (what can you expect in 1 day)
var i: integer;
  AText: string;
  AResolve: boolean;
  AParent, APrevItem, AArgument, ANextItem: TRtfArgument;
begin
  if Count > 0 then begin
    i := 0;
    while i < Count do begin
      AArgument := Items[i];

      if AArgument.Token = etVariable then begin
        if(i >= Count - 1)or(Items[i + 1].Token <> etAssign)
        then AArgument.ResolveVariable;
      end else if AArgument.Token = etFieldName then begin
        //An exception is the DbPicture(Table.Field). It shoud not evaluate
        //since the DbPicture function should be responsible for that.
        AResolve := true;
        AParent := AArgument.Parent;
        if Assigned(AParent) then begin
          AText := AParent.Value;
          if SameText(AText, 'DbPicture')
          then AResolve := false;
        end;
        if AResolve
        then AArgument.ResolveFieldName;
      end;

      if AArgument.Token = etNot then begin
        if i < Count - 1
        then ANextItem := Items[i + 1]
        else raise TRtfException.Create('invalid formula');
        if ANextItem.Token = etLitFalse
        then ANextItem.Value := true
        else if ANextItem.Token = etLitTrue
        then ANextItem.Value := false
        else raise TRtfException.Create('invalid formula');
        Remove(AArgument);
      end else if AArgument.Token in ATokenSet then begin
        if i > 0
        then APrevItem := Items[i - 1]
        else raise TRtfException.Create('invalid formula');
        if i < Count - 1
        then ANextItem := Items[i + 1]
        else raise TRtfException.Create('invalid formula');
        AExecproc(APrevItem, AArgument, ANextItem);
      end
      else Inc(i);
    end;

    if(Count = 1)and(Token = etParenthesis) then begin
      //Advance the answer from between parenthesis to parent.
      AArgument := Items[0];
      Value := AArgument.Value;
      Token := AArgument.Token;
      Remove(AArgument);
      if Assigned(Parser.OnEvalutate)
      then Parser.OnEvalutate(Self);
    end;
  end;
end;

procedure TRtfArgument.EvaluateExpression;
//Not really neat (what can you expect in 1 day)
//Evaluate the entered expression via ParseExpression()
var AFunction: TRtfFunction;
  AArgument: TRtfArgument;
  i: integer;
begin
  if Count > 0 then begin
    //First the binary operators
    Walk([etMul, etDiv], {$IFDEF FPC}@{$ENDIF}EvaluateUnaryBinary);
    //Secondly the unary operators
    Walk([etAdd, etSub], {$IFDEF FPC}@{$ENDIF}EvaluateUnaryBinary);
    //And finally the simple comparisons
    Walk([etEQ, etNE, etGE, etLE, etGT, etLT], {$IFDEF FPC}@{$ENDIF}EvaluateComparison);
    //And finally the more "complicated" comparisons
    Walk([etAnd, etOr], {$IFDEF FPC}@{$ENDIF}EvaluateComparison);
  end;


  if Token in[etFunction, etProcedure] then begin
    AFunction := Parser.Functions.Find(Value);
    if not Assigned(AFunction)
    then raise TRtfException.Create('Cannot resolve function');

    if not Assigned(AFunction.OnExecute)
    then raise TRtfException.Create('Cannot resolve function');

    //Remove now redundant comma's
    for i := Count - 1 downto 0 do begin
      AArgument := Items[i];
      if AArgument.Token = etComma
      then Remove(AArgument)
      else if VarIsEmpty(AArgument.Value)
      then raise TRtfException.Create('Function needs valid parameters');
    end;

    if Count < AFunction.Min
    then raise TRtfException.Create('expected more parameters');
    if Count > AFunction.Max
    then raise TRtfException.Create('to many parameters');

    Value := varEmpty;
    Token := etNothing;
    if Assigned(Parser.OnEvalutate)
    then Parser.OnEvalutate(Self);
    AFunction.OnExecute(Self);
    Clear; //Clear parameters
    if Assigned(Parser.OnEvalutate)
    then Parser.OnEvalutate(Self);
  end;

  //And not to forget  any optional assignments
  if Count > 0
  then Walk([etAssign], {$IFDEF FPC}@{$ENDIF}EvaluateAssign);
end;

procedure TRtfArgument.Evaluate;
//Not really neat (what can you expect in 1 day)
//Evaluate the expression (not very efficient, but what the hack)
var i: integer;
  AText: string;
  AArgument: TRtfArgument;
begin
  for i := 0 to Count - 1 do begin
    AArgument := Items[i];
    AArgument.Evaluate;
  end;
  EvaluateExpression;

  if(Parent = nil)and(Count > 1)
  then raise TRtfException.Create('invalid formula');

  if Count = 1 then begin
    //Advance the answer from between parenthesis to parent.
    AArgument := Items[0];
    Value := AArgument.Value;
    Token := AArgument.Token;
    Param := AArgument.Param;
    Clear;
    if Assigned(Parser.OnEvalutate)
    then Parser.OnEvalutate(Self);
  end;

  if Token = etLitString then begin
    //Fix: DbExpres appends a #0 character to memo fields
    AText := Value;
    if(Length(AText) > 0)and(AText[Length(AText)] = #0)
    then Value := Copy(AText, 1, Length(AText) - 1);
  end;
end;


{ TRtfColor }

function TRtfColor.GetAsString: string;
begin
  Result := Format('\red%d\green%d\blue%d;',[Red, Green, Blue]);
end;

{ TRtfColorList }

function TRtfColorList.GetItem(Index: integer): TRtfColor;
begin
  Result := TRtfColor(inherited Items[Index]);
end;

procedure TRtfColorList.Clear;
begin
  inherited;
  Add(0, 0, 0); //Dummy color..
end;

function TRtfColorList.Add(ARed, AGreen, ABlue: integer): integer;
var AColor: TRtfColor;
begin
  AColor := TRtfColor.Create;
  AColor.Red := ARed;
  AColor.Green := AGreen;
  AColor.Blue := ABlue;
  Result := inherited Add(AColor);
end;

function TRtfColorList.Find(ARed, AGreen, ABlue: integer): integer;
var
  i: integer;
  AColor: TRtfColor;
begin
  //0 is a dummy color
  for i := 1 to Count - 1 do
  begin
    AColor := Items[i];
    if(AColor.Red = ARed)and(AColor.Green = AGreen)and(AColor.Blue = ABlue) then
    begin
      Result := i;
      exit;
    end;
  end;
  Result := - 1;
end;

function TRtfColorList.GetAsString: string;
var
  i: integer;
  AColor: TRtfColor;
begin
  Result := '';
  //0 is a dummy color
  for i := 1 to Count - 1 do
  begin
    AColor := Items[i];
    Result := Result + AColor.AsString;
  end;
end;

function TRtfColorList.UseColor(ARed, AGreen, ABlue: integer): integer;
begin
  Result := Find(ARed, AGreen, ABlue);
  if Result < 0
  then Result := Add(ARed, AGreen, ABlue);
end;

function TRtfColorList.UseColor(AColor: TColor): integer;
var ARed, AGreen, ABlue: integer;
begin
  ARed := AColor and 255;
  AColor := AColor shr 8;
  AGreen := AColor and 255;
  AColor := AColor shr 8;
  ABlue := AColor and 255;
  Result := UseColor(ARed, AGreen, ABlue);
end;


{ TtiRtfParser }

constructor TtiRtfParser.Create;
begin
  inherited Create;
  FBoolTrue := 'Ja';
  FBoolFalse := 'Nee';
  FErrorForeColor := clRed;
  FErrorBackColor := clYellow;
  FPictureOptions :=[poMetafile, poBinary];
  FHlpItems := TRtfItem.Create;
  TmpItems := TObjectlist.Create;
  FDatasets := TRtfDataset.Create;
  FRtfItems := TRtfItemList.Create;
  FRawItems := TRtfItemList.Create;
  FColorList := TRtfColorList.Create;
  FVariables := TRtfVariableList.Create;
  FFunctions := TRtfFunctionList.Create;
  AddFunctions;
end;

destructor TtiRtfParser.Destroy;
begin
  FDatasets.Free;
  FRtfItems.Free;
  TmpItems.Free;
  FHlpItems.Free;
  FVariables.Free;
  FFunctions.Free;
  FRawItems.Free;
  FColorList.Free;
  inherited;
end;

procedure TtiRtfParser.Clear;
begin
  TmpItems.Clear;
  RawItems.Clear;
  RtfItems.Clear;
  HlpItems.Clear;
  ColorList.Clear;
end;

procedure TtiRtfParser.SaveToFile(AFileName: string);
var AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    RtfItems.SaveToStream(AStream, ColorList.AsString);
    AStream.SaveToFile(AFileName);
  finally
    AStream.Free;
  end;
end;

function TtiRtfParser.SaveToString: string;
var AStream: TMemoryStream;
begin
  Result := '';
  AStream := TMemoryStream.Create;
  try
    RtfItems.SaveToStream(AStream, ColorList.AsString);
    SetLength(Result, AStream.Size);
    if AStream.Size > 0 //Humble admitted its a bit dirty
    then Move(AStream.Memory^, Result[1], AStream.Size);
  finally
    AStream.Free;
  end;
end;

procedure TtiRtfParser.SaveToStream(AStream: TMemoryStream);
begin
  RtfItems.SaveToStream(AStream, ColorList.AsString);
end;

function TtiRtfParser.AddVariable(AName: string; AValue: variant; AToken: TRtfToken): TRtfVariable;
begin
  Result := Variables.Add(AName, AValue, AToken);
end;

function TtiRtfParser.AddDataset(ATable: TObject; AName: string; AFreeDataset: boolean = false): TRtfDataset;
begin
  Result := Datasets.Add(ATable, AName, AFreeDataset);
end;

function TtiRtfParser.AddFunction(AName: string; ATokenType: TRtfToken; AMin, AMax: smallint; AOnexecute: TRtfFunctionExecute): TRtfFunction;
begin
  Result := Functions.Add(ATokenType, AName, AMin, AMax, AOnexecute);
end;

function TtiRtfParser.AddToRtfItems(AItem: TRtfItem): TRtfItem;
begin
  Result := TRtfItem.Create;
  Result.Assign(AItem);
  RtfItems.Add(Result);
end;

procedure TtiRtfParser.LoadFromFile(AFilename: string);
//Tokenize the raw rtf and put it into rawitems
var
  AStream: TMemoryStream;
begin
  Clear;
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    LoadFromBuffer(AStream.Memory, AStream.Size);
  finally
    FreeAndNil(AStream);
  end;
end;

procedure TtiRtfParser.LoadFromStream(AStream: TMemoryStream);
//Tokenize the raw rtf and put it into rawitems
begin
  LoadFromBuffer(AStream.Memory, AStream.Size);
end;

procedure TtiRtfParser.LoadFromString(AString: string);
//Tokenize the raw rtf and put it into rawitems
begin
  LoadFromBuffer(pchar(AString), Length(AString));
end;

procedure TtiRtfParser.LoadFromBuffer(ABuffer: pchar; ASize: integer);
//Tokenize the raw rtf and put it into rawitems
var AColorTable: TRtfItem;
  AColors, AToken: string;
  ANumber, ARed, AGreen, ABlue: integer;
  p: pchar;
begin
  Clear;
  with TRawRtfParser.Create do try
    Execute(FRawItems, ABuffer, ASize);
    AColorTable := ColorTable;
  finally
    Free;
  end;

  if not Assigned(AColorTable)
  then raise TRtfException.Create('no colortbl found');

  //Parse the color table (tiny parser)
  ARed := 0;
  ABlue := 0;
  AGreen := 0;
  AColors := Copy(AColorTable.RtfTextBuf, 11, Maxint);
  p := pchar(AColors);
  while p <> #0 do begin
    case p^ of
      '\': begin
        inc(p);
        AToken := '';
        ANumber := 0;
        while(p^ in['A'..'Z', 'a'..'z'])do begin
          AToken := AToken + p^;
          inc(p);
        end;
        while(p^ in['0'..'9'])do begin
          ANumber :=(ANumber * 10) +(ord(p^) - ord('0'));
          inc(p);
        end;
        ANumber := ANumber and 255;
        if SameText(AToken, 'red')
        then ARed := ANumber
        else if SameText(AToken, 'blue')
        then ABlue := ANumber
        else if SameText(AToken, 'green')
        then AGreen := ANumber;
      end;
      ';': begin
        inc(p);
        ColorList.UseColor(ARed, AGreen, ABlue);
        ARed := 0;
        ABlue := 0;
        AGreen := 0;
      end;
      #13, #10: inc(p);
      '}': break;
      else raise TRtfException.Create('Invalid character in colortbl');
    end;
  end;

  (*
  {\colortbl;\red0\green0\blue;\red255\green0\blue;\red255\green255\blue;\red0\green255\blue;\red255\green0\blue;\red0\green0\blue;\red0\green255\blue;\red255\green255\blue;\red128\green0\blue;\red128\green128\blue;\red0\green128\blue;\red128\green0\blue;\red0\green0\blue;\red0\green128\blue;\red128\green128\blue;\red192\green192\blue;}
  *)
end;

procedure TtiRtfParser.Execute;
//Rtf has been already been loaded
begin
  PreParse; //Create a tree for easy parsing of if-then-else and scan-endscan etc.
  Parse(HlpItems); //Parse the tree items in the structure..
end;

procedure TtiRtfParser.PreParse;
//Preparse the items from Items to Structure
//This will make a tree from (sub)expressions and code.
//Specially for the if/else/endif and scan/scanend
var AIndex, AForeColor, ABackColor: integer;
  AItem, AStart, ABranche: TRtfItem;
  AArguments: TRtfArgument;
  ARemoveParagraph: boolean;
  AFirstArgument: string;
  AFirstToken: TRtfToken;
  AText: string;
begin
  AItem := RawItems.Head;
  AArguments := TRtfArgument.Create(Self);
  try
    //Round 1: create expression nodes
    while Assigned(AItem)do begin
      if AItem.RtfClass = RtfParsebegin then begin

        //Items starts with a RtfParseBegin
        AText := AItem.RtfTextBuf; //Initial expression
        AStart := HlpItems[HlpItems.Add(AItem)];
        AStart.RtfClass := RtfExpression;
        AStart.RtfMajor := RtfNormalExpression;
        AStart.RtfTextBuf := '';
        AItem := AItem.Next;
        while Assigned(AItem)do begin
          case AItem.RtfClass of
            RtfParseEnd: begin //End of expression
              AText := AText + AItem.RtfTextBuf;
              AItem := AItem.Next; //Skip's ParseEnd
              break;
            end;
            RtfParseBegin: begin //WTF: Start of expression, this should not happen. Inform user
              raise TRtfException.Create('Error in preparse? at ' + AText);
            end;
            RtfText: AText := AText + AItem.RtfTextBuf;
            RtfControl: begin
              case AItem.RtfMajor of
                RtfSpecialChar: begin //Add some special characters
                  case AItem.RtfMinor of
                    RtfPar, RtfParDef:; //Ignore these
                    RtflQuote, RtfrQuote: AText := AText + Chr(39);
                    RtflDblQuote, RtfrDblQuote: AText := AText + '"';
                    else AText := AText + AItem.RtfTextBuf; //Hope this is Ok?
                  end;
                end;
                else HlpItems.Add(AItem);
              end;
            end;
            else HlpItems.Add(AItem);
          end;
          AItem := AItem.Next;
        end;

        AStart.RtfTextBuf := StringReplace(AText, #13#10, '',[rfReplaceAll]);
        AArguments.Clear;
        ARemoveParagraph := true;
        try
          AArguments.ParseExpression(AText);
        except
          on e: Exception do begin
            //Ignore at this moment, it will be triggered again in the Parse()
          end;
        end;
        if AArguments.Count > 0 then begin
          AFirstToken := AArguments[0].Token;
          AFirstArgument := AArguments[0].Value;
          if SameText(AFirstArgument, 'if')
          then AStart.RtfMajor := RtfIfExpression
          else if SameText(AFirstArgument, 'else')
          then AStart.RtfMajor := RtfElseExpression
          else if SameText(AFirstArgument, 'endif')
          then AStart.RtfMajor := RtfEndifExpression
          else if SameText(AFirstArgument, 'scan')
          then AStart.RtfMajor := RtfScan
          else if SameText(AFirstArgument, 'endscan')
          then AStart.RtfMajor := RtfScanEnd
          else if SameText(AFirstArgument, 'scanentry')
          then AStart.RtfMajor := RtfScanEntry
          else if SameText(AFirstArgument, 'scanfooter')
          then AStart.RtfMajor := RtfScanFooter;
          ARemoveParagraph := AFirstToken in[etVariable, etProcedure, etNothing];
          if(AFirstToken = etVariable)and(AArguments.Count = 1)
          then ARemoveParagraph := false; //One exception: variable reference
        end;

        if ARemoveParagraph
        then AItem := SkipParagraph(AItem);
      end else begin
        HlpItems.Add(AItem);
        AItem := AItem.Next;
      end;
    end;

  finally
    AArguments.Free;
  end;

  //Round 2: create if then else expression sub-nodes
  AIndex := 0;
  ABranche := nil;
  while AIndex < HlpItems.Count do begin
    AItem := HlpItems[AIndex];
    try

      if AItem.CheckItem(RtfExpression, RtfIfExpression) then begin
        if Assigned(ABranche) then begin
          HlpItems.Extract(AItem);
          ABranche.Add(AItem);
        end
        else Inc(AIndex);

        ABranche := TRtfItem.Create;
        ABranche.RtfClass := RtfBranche;
        ABranche.RtfMajor := RtfThenExpression;
        TmpItems.Add(ABranche); //Otherwise memory leak
        AItem.Add(ABranche);
      end else if AItem.CheckItem(RtfExpression, RtfElseExpression) then begin
        if not Assigned(ABranche)
        then raise TRtfException.Create('unexpected else');

        HlpItems.Extract(AItem); //Remove else item
        AItem := ABranche.Parent; //Points to "if" statement
        if AItem.RtfClass = RtfBranche
        then AItem := AItem.Parent; //Points to "if" statement
        if not Assigned(AItem)
        then raise TRtfException.Create('unexpected else');
        if not AItem.CheckItem(RtfExpression, RtfIfExpression)
        then raise TRtfException.Create('unexpected scanentry');

        ABranche := TRtfItem.Create;
        ABranche.RtfClass := RtfBranche;
        ABranche.RtfMajor := RtfElseExpression;
        TmpItems.Add(ABranche); //Otherwise memory leak
        AItem.Add(ABranche);
      end else if AItem.CheckItem(RtfExpression, RtfEndifExpression) then begin
        if not Assigned(ABranche)
        then raise TRtfException.Create('unexpected endif');

        HlpItems.Extract(AItem); //Remove item
        ABranche := ABranche.Parent;
        ABranche := ABranche.Parent;
        if ABranche = HlpItems
        then ABranche := nil;
      end else if AItem.CheckItem(RtfExpression, RtfScan) then begin

        if Assigned(ABranche) then begin
          HlpItems.Extract(AItem);
          ABranche.Add(AItem);
        end
        else Inc(AIndex);

        ABranche := TRtfItem.Create;
        ABranche.RtfClass := RtfBranche;
        ABranche.RtfMajor := RtfScan;
        TmpItems.Add(ABranche); //Otherwise memory leak
        AItem.Add(ABranche);
      end else if AItem.CheckItem(RtfExpression, RtfScanEntry) then begin
        if not Assigned(ABranche)
        then raise TRtfException.Create('unexpected scanentry');

        HlpItems.Extract(AItem); //Remove else item
        AItem := ABranche.Parent; //Points to "scan" statement
        if AItem.RtfClass = RtfBranche
        then AItem := AItem.Parent; //Points to "scan" statement
        if not Assigned(AItem)
        then raise TRtfException.Create('unexpected scanentry');
        if not AItem.CheckItem(RtfExpression, RtfScan)
        then raise TRtfException.Create('unexpected scanentry');

        ABranche := TRtfItem.Create;
        ABranche.RtfClass := RtfBranche;
        ABranche.RtfMajor := RtfScanEntry;
        TmpItems.Add(ABranche); //Otherwise memory leak
        AItem.Add(ABranche);
      end else if AItem.CheckItem(RtfExpression, RtfScanFooter) then begin
        if not Assigned(ABranche)
        then raise TRtfException.Create('unexpected scanfooter');

        HlpItems.Extract(AItem); //Remove item
        AItem := ABranche.Parent; //Points to "scan" statement
        if AItem.RtfClass = RtfBranche
        then AItem := AItem.Parent; //Points to "scan" statement
        if not Assigned(AItem)
        then raise TRtfException.Create('unexpected scanfooter');
        if not AItem.CheckItem(RtfExpression, RtfScan)
        then raise TRtfException.Create('unexpected scanfooter');

        ABranche := TRtfItem.Create;
        ABranche.RtfClass := RtfBranche;
        ABranche.RtfMajor := RtfScanFooter;
        TmpItems.Add(ABranche); //Otherwise memory leak
        AItem.Add(ABranche);
      end else if AItem.CheckItem(RtfExpression, RtfScanEnd) then begin
        if not Assigned(ABranche)
        then raise TRtfException.Create('unexpected scanend');

        HlpItems.Extract(AItem); //Remove item
        ABranche := ABranche.Parent;
        ABranche := ABranche.Parent;
        if ABranche = HlpItems
        then ABranche := nil;
      end else begin
        if Assigned(ABranche) then begin
          HlpItems.Extract(AItem);
          ABranche.Add(AItem);
        end
        else Inc(AIndex);
      end;
    except
      on e: Exception do begin
        fpgApplication.HandleException(e);
{
        if Assigned(AItem)
        then AItem.RtfTextBuf := AItem.RtfTextBuf + ' ' + E.Message;
}
      end;
    end;
  end;

  if Assigned(ABranche) then begin
    if Assigned(AItem) then begin
      AForeColor := ColorList.UseColor(ErrorForeColor);
      ABackColor := ColorList.UseColor(ErrorBackColor);
      AItem.FRtfTextBuf := Format('{\b\ul\highlight%d\cf%d <<<<endif or endscan missing}%s',[ABackColor, AForeColor, AItem.RtfTextBuf]);
    end;
  end;
end;

function TtiRtfParser.SkipParagraph(AItem: TRtfItem): TRtfItem;
//Remove paragraph breaks if it was just a procedure (or a VAR/SET etc.)
//This can be multiple breaks (CrLf and /Par controls) but
//do not remove more than a single item from each of them
var AEnter, APar, AParDef: boolean;
begin
  APar := false;
  AEnter := false;
  AParDef := false;
  while Assigned(AItem) do
  begin
    case AItem.RtfClass of
      RtfText: begin
        if not AEnter and(Pos(#13#10, AItem.RtfTextBuf) > 0) then
        begin
          //Crap, it can contain multiple #13#10's
          AItem.RtfTextBuf := StringReplace(AItem.RtfTextBuf, #13#10, '',[rfReplaceAll]);
          AItem := AItem.Next;
          AEnter := true;
          continue;
        end
        else
          break;
      end;
      RtfControl: begin
        case AItem.RtfMajor of
          RtfSpecialChar: begin
            case AItem.RtfMinor of
              RtfPar: begin
                if not APar then
                begin
                  AItem := AItem.Next;
                  APar := true;
                  continue;
                end
                else
                  break;
              end;
              RtfParDef: begin
                if not AParDef then
                begin
                  AItem := AItem.Next;
                  AParDef := true;
                  continue;
                end
                else
                  break;
              end;
            end;
          end;
        end;
      end;
    end;
    break;
  end;
  Result := AItem;
end;

procedure TtiRtfParser.Parse(AItems: TRtfItem);
//Forward all stuff to the output list
var
  i: integer;
  AItem: TRtfItem;
begin
  for i := 0 to AItems.Count - 1 do
  begin
    AItem := AItems[i];
    if AItem.RtfClass = RtfExpression then
      ParseExpression(AItem)
    else
      AddToRtfItems(AItem);
  end;
end;

procedure TtiRtfParser.ParseExpression(AItem: TRtfItem);
var
  i, ABackColor, AForeColor: integer;
  ARemove: boolean;
  ADataset: TRtfDataset;
  AScanNoEof: boolean;
  AExpression: string;
  AResultValue: variant;
  AResultParam: integer;
  AResultToken: TRtfToken;
  AArguments: TRtfArgument;
  AAnswer, ATemp, AHeader, ARecord, AFooter: TRtfItem;
begin
  AExpression := AItem.RtfTextBuf;
  AAnswer := AddToRtfItems(AItem);
  AAnswer.RtfTextBuf := '';

  try
    ARemove := true;
    AArguments := TRtfArgument.Create(Self);
    try
      AArguments.ParseExpression(AExpression);
      if AArguments.Count > 0 then
      begin
        //procedures and variables have no returning value and etNothing, well
        ARemove := AArguments[0].Token in[etVariable, etProcedure, etNothing];
        if (AArguments[0].Token = etVariable) and (AArguments.Count = 1) then
          ARemove := false; //But show a single variable
      end;
      if Assigned(OnEvalutate) then //debug
        OnEvalutate(AArguments);
      AArguments.Evaluate;
      AResultToken := AArguments.Token;
      AResultValue := AArguments.Value;
      AResultParam := AArguments.Param;
      if Assigned(OnEvalutate) then //debug
        OnEvalutate(AArguments);

      case AItem.RtfMajor of
        RtfNormalExpression: begin
          if not ARemove then begin
            //Expand booleans and date to text values (date because Variant
            //have no typing for it. Place the text into the output rtf
            if AResultToken = etLitTrue then
              AAnswer.RtfTextBuf := BoolTrue
            else if AResultToken = etLitFalse then
              AAnswer.RtfTextBuf := BoolFalse
            else if AResultToken = etLitDate then
            begin
              if Frac(AResultValue) = 0 then
                AAnswer.RtfTextBuf := DateToStr(AResultValue)
              else if Int(AResultValue) = 0 then
                AAnswer.RtfTextBuf := TimeToStr(AResultValue)
              else
                AAnswer.RtfTextBuf := DateTimeToStr(AResultValue);
            end
            else
              AAnswer.RtfTextBuf := VarToStr(AResultValue);
          end;
        end;
        
        RtfIfExpression:
            begin
              if AResultToken = etLitTrue then
                Parse(AItem[0])
              else if(AResultToken = etLitFalse)and(AItem.Count > 1) then
                Parse(AItem[1]);
            end;

        RtfScan:
            begin
        //Scan(Dataset). Find out what items is header, record and footer
          ARecord := nil;
          AHeader := nil;
          AFooter := nil;
          ADataset := TRtfDataset(PtrInt(AResultValue)); //Dirty!

          for i := 0 to AItem.Count - 1 do begin
            ATemp := AItem[i];
            case ATemp.RtfMajor of
              RtfScan: AHeader := ATemp;
              RtfScanEntry: ARecord := ATemp;
              RtfScanFooter: AFooter := ATemp;
            end;
          end;

          if AHeader = ARecord
          then AHeader := nil;
          if ARecord = nil then begin
            ARecord := AHeader;
            AHeader := nil;
          end;

          AScanNoEof := AResultParam = 1;

          //And evaluate the Scan(dataset)
          ADataset.Open;
          if Assigned(AHeader)
          then Parse(AHeader);
          ADataset.First;

          if AScanNoEof and ADataset.Eof
          then exit; //Also no footer..

          while not ADataset.Eof do begin
            if Assigned(ARecord)
            then Parse(ARecord);
            ADataset.Next;
          end;
          if Assigned(AFooter)
          then Parse(AFooter);
        end;
      end;
    finally
      AArguments.Free;
    end;

  except
    on e: Exception do begin
      AForeColor := ColorList.UseColor(ErrorForeColor);
      ABackColor := ColorList.UseColor(ErrorBackColor);
      AAnswer.FRtfTextBuf := Format('{\b\ul\highlight%d\cf%d %s %s}',[ABackColor, AForeColor, AExpression, E.Message]);
    end;
  end;
end;

procedure TtiRtfParser.AddFunctions;
begin
  //Extra tokens not tokenized by RawParser
  FFunctions.Add(etOr, 'Or', 0, 0, nil);
  FFunctions.Add(etNot, 'Not', 0, 0, nil);
  FFunctions.Add(etAnd, 'And', 0, 0, nil);
  FFunctions.Add(etLitTrue, 'True', 0, 0, nil);
  FFunctions.Add(etLitFalse, 'False', 0, 0, nil);

  //Date time functions/conversions
  FFunctions.Add(etFunction, 'Year', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfYear);
  FFunctions.Add(etFunction, 'Month', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfMonth);
  FFunctions.Add(etFunction, 'Day', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfDay);
  FFunctions.Add(etFunction, 'SYear', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfSYear);
  FFunctions.Add(etFunction, 'SMonth', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfSMonth);
  FFunctions.Add(etFunction, 'SDay', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfSDay);
  FFunctions.Add(etFunction, 'Dtos', 1, 2, {$IFDEF FPC}@{$ENDIF}UdfDtos);
  FFunctions.Add(etFunction, 'Stod', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfStod);
  FFunctions.Add(etFunction, 'DateToStr', 1, 2, {$IFDEF FPC}@{$ENDIF}UdfDateToStr);
  FFunctions.Add(etFunction, 'TimeToStr', 1, 2, {$IFDEF FPC}@{$ENDIF}UdfTimeToStr);
  FFunctions.Add(etFunction, 'DateTimeToStr', 1, 2, {$IFDEF FPC}@{$ENDIF}UdfDateTimeToStr);
  FFunctions.Add(etFunction, 'StrToDate', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfStrToDate);
  FFunctions.Add(etFunction, 'StrToTime', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfStrToTime);
  FFunctions.Add(etFunction, 'StrToDateTime', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfStrToDateTime);
  FFunctions.Add(etFunction, 'ShortMonthName', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfShortMonthName);
  FFunctions.Add(etFunction, 'ShortDayName', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfShortDayName);
  FFunctions.Add(etFunction, 'LongMonthName', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfLongMonthName);
  FFunctions.Add(etFunction, 'LongDayName', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfLongDayName);

  //String, int, float functions/conversions
  FFunctions.Add(etFunction, 'Int', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfInt);
  FFunctions.Add(etFunction, 'Chr', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfChr);
  FFunctions.Add(etFunction, 'Iif', 3, 3, {$IFDEF FPC}@{$ENDIF}UdfIif);
  FFunctions.Add(etFunction, 'Str', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfStr);
  FFunctions.Add(etFunction, 'Val', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfVal);
  FFunctions.Add(etFunction, 'Nul', 2, 2, {$IFDEF FPC}@{$ENDIF}UdfNul);
  FFunctions.Add(etFunction, 'Now', 0, 0, {$IFDEF FPC}@{$ENDIF}UdfNow);
  FFunctions.Add(etFunction, 'Frac', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfFrac);
  FFunctions.Add(etFunction, 'Padr', 2, 2, {$IFDEF FPC}@{$ENDIF}UdfPadr);
  FFunctions.Add(etFunction, 'Padl', 2, 2, {$IFDEF FPC}@{$ENDIF}UdfPadl);
  FFunctions.Add(etFunction, 'Date', 0, 0, {$IFDEF FPC}@{$ENDIF}UdfDate);
  FFunctions.Add(etFunction, 'Time', 0, 0, {$IFDEF FPC}@{$ENDIF}UdfTime);
  FFunctions.Add(etFunction, 'Mid', 2, 3, {$IFDEF FPC}@{$ENDIF}UdfSubStr);
  FFunctions.Add(etFunction, 'Copy', 2, 3, {$IFDEF FPC}@{$ENDIF}UdfSubStr);
  FFunctions.Add(etFunction, 'SubStr', 2, 3, {$IFDEF FPC}@{$ENDIF}UdfSubStr);
  FFunctions.Add(etFunction, 'Trunc', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfTrunc);
  FFunctions.Add(etFunction, 'Round', 1, 2, {$IFDEF FPC}@{$ENDIF}UdfRound);
  FFunctions.Add(etFunction, 'Upper', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfUpper);
  FFunctions.Add(etFunction, 'Lower', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfLower);
  FFunctions.Add(etFunction, 'Trim', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfTrim);
  FFunctions.Add(etFunction, 'Empty', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfEmpty);
  FFunctions.Add(etFunction, 'TrimLeft', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfTrimLeft);
  FFunctions.Add(etFunction, 'TrimRight', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfTrimRight);
  FFunctions.Add(etFunction, 'IntToStr', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfIntToStr);
  FFunctions.Add(etFunction, 'StrToInt', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfStrToInt);
  FFunctions.Add(etFunction, 'FloatToStr', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfFloatToStr);
  FFunctions.Add(etFunction, 'StrToFloat', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfStrToFloat);
  FFunctions.Add(etFunction, 'Power', 2, 2, {$IFDEF FPC}@{$ENDIF}UdfIntPower);
  FFunctions.Add(etFunction, 'IntPower', 2, 2, {$IFDEF FPC}@{$ENDIF}UdfIntPower);
  FFunctions.Add(etFunction, 'FormatFloat', 2, 2, {$IFDEF FPC}@{$ENDIF}UdfFormatFloat);
  FFunctions.Add(etFunction, 'FBool', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfFbool);

  //Dataset
  FFunctions.Add(etFunction, 'Bof', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfBof);
  FFunctions.Add(etFunction, 'Eof', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfEof);
  FFunctions.Add(etProcedure, 'Next', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfNext);
  FFunctions.Add(etProcedure, 'Prev', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfPrior);
  FFunctions.Add(etProcedure, 'Open', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfOpen);
  FFunctions.Add(etProcedure, 'First', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfFirst);
  FFunctions.Add(etProcedure, 'Last', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfLast);
  FFunctions.Add(etFunction, 'IsEmpty', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfIsEmpty);
  FFunctions.Add(etFunction, 'RecordCount', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfRecordCount);
  FFunctions.Add(etProcedure, 'Scan', 1, 9999, {$IFDEF FPC}@{$ENDIF}UdfScan);
  FFunctions.Add(etProcedure, 'EndScan', 0, 0, {$IFDEF FPC}@{$ENDIF}UdfDummy);
  FFunctions.Add(etProcedure, 'ScanEntry', 0, 0, {$IFDEF FPC}@{$ENDIF}UdfDummy);
  FFunctions.Add(etProcedure, 'ScanFooter', 0, 0, {$IFDEF FPC}@{$ENDIF}UdfDummy);
  FFunctions.Add(etProcedure, 'Dataset', 3, 9999, {$IFDEF FPC}@{$ENDIF}UdfDataset);

  //Misc routines
  FFunctions.Add(etProcedure, 'If', 1, 9999, {$IFDEF FPC}@{$ENDIF}UdfIf);
  FFunctions.Add(etProcedure, 'Else', 0, 0, {$IFDEF FPC}@{$ENDIF}UdfDummy);
  FFunctions.Add(etProcedure, 'Endif', 0, 0, {$IFDEF FPC}@{$ENDIF}UdfDummy);
  FFunctions.Add(etProcedure, 'NoPar', 0, 0, {$IFDEF FPC}@{$ENDIF}UdfDummy);
  FFunctions.Add(etFunction, 'Picture', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfPicture);
  FFunctions.Add(etFunction, 'DbPicture', 1, 1, {$IFDEF FPC}@{$ENDIF}UdfDbPicture);
end;

procedure TtiRtfParser.UdfDummy(AArgument: TRtfArgument);
begin
  //Nothing, since its an etProcedure the Rtf paragraph will be removed
end;

procedure TtiRtfParser.UdfChr(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitInt])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AArgument.Value := Chr(StrToInt(AArgument[0].Value));
  AArgument.Token := etLitString;
end;

procedure TtiRtfParser.UdfNow(AArgument: TRtfArgument);
begin
  AArgument.Value := Now;
  AArgument.Token := etLitDate;
end;

procedure TtiRtfParser.UdfDate(AArgument: TRtfArgument);
begin
  AArgument.Value := Date;
  AArgument.Token := etLitDate;
end;

procedure TtiRtfParser.UdfTime(AArgument: TRtfArgument);
begin
  AArgument.Value := Time;
  AArgument.Token := etLitDate;
end;

procedure TtiRtfParser.UdfIntToStr(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitInt])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AArgument.Value := IntToStr(AArgument[0].Value);
  AArgument.Token := etLitString;
end;

procedure TtiRtfParser.UdfStrToInt(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitString])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AArgument.Value := StrToInt(AArgument[0].Value);
  AArgument.Token := etLitInt;
end;

procedure TtiRtfParser.UdfFloatToStr(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitFloat])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AArgument.Value := FloatToStr(Extended(AArgument[0].Value));
  AArgument.Token := etLitString;
end;

procedure TtiRtfParser.UdfStrToFloat(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitString])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AArgument.Value := StrToFloat(AArgument[0].Value);
  AArgument.Token := etLitFloat;
end;

procedure TtiRtfParser.UdfNul(AArgument: TRtfArgument);
var AStr: string;
  ACnt: integer;
begin
  if not AArgument.Check([[etLitInt],[etLitInt]])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AStr := AArgument[0].Value;
  ACnt := AArgument[1].Value;
  while Length(AStr) < ACnt do AStr := '0' + AStr;
  while Length(AStr) > ACnt do Delete(AStr, 1, 1);
  AArgument.Value := AStr;
  AArgument.Token := etLitString;
end;

procedure TtiRtfParser.UdfPadl(AArgument: TRtfArgument);
var AStr: string;
  ACnt: integer;
begin
  if not AArgument.Check([[etLitString],[etLitInt]])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AStr := AArgument[0].Value;
  ACnt := AArgument[1].Value;
  AStr := Copy(AStr, 1, ACnt);
  while Length(AStr) < ACnt do begin
    AStr := ' ' + AStr;
  end;
  AArgument.Value := AStr;
  AArgument.Token := etLitString;
end;

procedure TtiRtfParser.UdfPadr(AArgument: TRtfArgument);
var AStr: string;
  ACnt: integer;
begin
  if not AArgument.Check([[etLitString],[etLitInt]])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AStr := AArgument[0].Value;
  ACnt := AArgument[1].Value;
  AStr := Copy(AStr, 1, ACnt);
  while Length(AStr) < ACnt do begin
    AStr := AStr + ' ';
  end;
  AArgument.Value := AStr;
  AArgument.Token := etLitString;
end;

procedure TtiRtfParser.UdfIif(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitFalse, etLitTrue])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  if AArgument[0].Token = etLitTrue then begin
    AArgument.Value := AArgument[1].Value;
    AArgument.Token := AArgument[1].Token;
  end else begin
    AArgument.Value := AArgument[2].Value;
    AArgument.Token := AArgument[2].Token;
  end;
end;

procedure TtiRtfParser.UdfIf(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitFalse, etLitTrue])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AArgument.Value := AArgument[0].Value;
  AArgument.Token := AArgument[0].Token;
end;

procedure TtiRtfParser.UdfScan(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etDataset])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AArgument.Value := AArgument[0].Value;
  AArgument.Token := AArgument[0].Token;
  if AArgument.Count > 1 then begin
    if AArgument[1].Token = etLitTrue
    then AArgument.Param := 1;
  end;
end;

procedure TtiRtfParser.UdfDataset(AArgument: TRtfArgument);
var ADatabase, AAliasName, ASqlScript: string;
  ADataset: TRtfDataset;
  AObject: TObject;
begin
  if not AArgument.Check([[etLitString],[etLitString],[etLitString]])
  then raise TRtfException.Create(rsUnexpectedParameterType);

  ADatabase := AArgument[0].Value;
  AAliasName := AArgument[1].Value;
  ASqlScript := AArgument[2].Value;

  //Delete first argument so Sql params become first
  AArgument.Delete(0);
  AArgument.Delete(0);
  AArgument.Delete(0);

  //Remove existing dataset with same aliasname
  ADataset := Datasets.Find(AAliasName);
  if Assigned(ADataset)
  then Datasets.Remove(ADataset);

  //Create the new query or dataset (whatever)
  if Assigned(OnCreateDataset)
  then OnCreateDataset(ADatabase, AAliasName, ASqlScript, AArgument)
  else raise TRtfException.Create('OnCreateDataset is not assigned');
  if VarIsEmpty(AArgument.Value)
  then raise TRtfException.Create('OnCreateDataset did not return a dataset object');

  AObject := TObject(Ptrint(AArgument.Value));
  ADataset := Datasets.Add(AObject, AAliasname, true);
  AArgument.Token := etDataset;
  AArgument.Value := integer(ADataset);
end;

procedure TtiRtfParser.UdfLower(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitString])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AArgument.Token := etLitString;
  AArgument.Value := LowerCase(AArgument[0].Value);
end;

procedure TtiRtfParser.UdfUpper(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitString])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AArgument.Token := etLitString;
  AArgument.Value := UpperCase(AArgument[0].Value);
end;

procedure TtiRtfParser.UdfTrim(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitString])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AArgument.Token := etLitString;
  AArgument.Value := Trim(AArgument[0].Value);
end;

procedure TtiRtfParser.UdfTrimLeft(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitString])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AArgument.Token := etLitString;
  AArgument.Value := TrimLeft(AArgument[0].Value);
end;

procedure TtiRtfParser.UdfTrimRight(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitString])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AArgument.Token := etLitString;
  AArgument.Value := TrimRight(AArgument[0].Value);
end;

procedure TtiRtfParser.UdfSubStr(AArgument: TRtfArgument);
var AStr: string;
  APos, ACnt: integer;
begin
  ACnt := Maxint;
  if not AArgument.Check([[etLitString],[etLitInt]])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  if AArgument.Count > 2 then begin
    if not AArgument.Check(2,[etLitInt]) then
      raise TRtfException.Create(rsUnexpectedParameterType);
    ACnt := AArgument[2].Value;
  end;
  AStr := AArgument[0].Value;
  APos := AArgument[1].Value;
  AStr := Copy(AStr, APos, ACnt);
  AArgument.Value := AStr;
  AArgument.Token := etLitString;
end;

procedure TtiRtfParser.UdfStr(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitFloat, etLitInt]) then
    raise TRtfException.Create(rsUnexpectedParameterType);
  if AArgument[0].Token = etLitFloat then
  begin
    AArgument.Token := etLitString;
    AArgument.Value := FloatToStr(AArgument[0].Value);
  end
  else
  begin
    AArgument.Token := etLitString;
    AArgument.Value := IntToStr(AArgument[0].Value);
  end;
end;

procedure TtiRtfParser.UdfVal(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitString]) then
    raise TRtfException.Create(rsUnexpectedParameterType);
  if (Pos('.', AArgument[0].Value) > 0)or(Pos(',', AArgument[0].Value) > 0) then
  begin
    AArgument.Token := etLitString;
    AArgument.Value := StrToFloat(AArgument[0].Value)
  end
  else
  begin
    AArgument.Token := etLitString;
    AArgument.Value := StrToInt(AArgument[0].Value)
  end;
end;

procedure TtiRtfParser.UdfBof(AArgument: TRtfArgument);
var ATable: TRtfDataset;
begin
  if not AArgument.Check(0,[etDataset]) then
    raise TRtfException.Create(rsUnexpectedParameterType);
  ATable := TRtfDataset(Ptrint(AArgument[0].Value));
  if ATable.Bof then
  begin
    AArgument.Token := etLitTrue;
    AArgument.Value := true;
  end
  else
  begin
    AArgument.Token := etLitFalse;
    AArgument.Value := false;
  end;
end;

procedure TtiRtfParser.UdfEof(AArgument: TRtfArgument);
var ATable: TRtfDataset;
begin
  if not AArgument.Check(0,[etDataset]) then
    raise TRtfException.Create(rsUnexpectedParameterType);
  ATable := TRtfDataset(Ptrint(AArgument[0].Value));
  if ATable.Eof then
  begin
    AArgument.Token := etLitTrue;
    AArgument.Value := true;
  end
  else
  begin
    AArgument.Token := etLitFalse;
    AArgument.Value := false;
  end;
end;


procedure TtiRtfParser.UdfRecordCount(AArgument: TRtfArgument);
var ATable: TRtfDataset;
begin
  if not AArgument.Check(0,[etDataset])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  ATable := TRtfDataset(Ptrint(AArgument[0].Value));
  AArgument.Token := etLitInt;
  AArgument.Value := ATable.RecordCount
end;

procedure TtiRtfParser.UdfIsEmpty(AArgument: TRtfArgument);
var ATable: TRtfDataset;
begin
  if not AArgument.Check(0,[etDataset])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  ATable := TRtfDataset(Ptrint(AArgument[0].Value));
  if ATable.IsEmpty then begin
    AArgument.Token := etLitTrue;
    AArgument.Value := true;
  end else begin
    AArgument.Token := etLitFalse;
    AArgument.Value := false;
  end;
end;

procedure TtiRtfParser.UdfFirst(AArgument: TRtfArgument);
var ATable: TRtfDataset;
begin
  if not AArgument.Check(0,[etDataset])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  ATable := TRtfDataset(Ptrint(AArgument[0].Value));
  ATable.First;
end;

procedure TtiRtfParser.UdfLast(AArgument: TRtfArgument);
var ATable: TRtfDataset;
begin
  if not AArgument.Check(0,[etDataset])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  ATable := TRtfDataset(Ptrint(AArgument[0].Value));
  ATable.Last;
end;

procedure TtiRtfParser.UdfNext(AArgument: TRtfArgument);
var ATable: TRtfDataset;
begin
  if not AArgument.Check(0,[etDataset])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  ATable := TRtfDataset(Ptrint(AArgument[0].Value));
  ATable.Next;
end;

procedure TtiRtfParser.UdfOpen(AArgument: TRtfArgument);
var ATable: TRtfDataset;
begin
  if not AArgument.Check(0,[etDataset])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  ATable := TRtfDataset(Ptrint(AArgument[0].Value));
  ATable.Open;
end;

procedure TtiRtfParser.UdfPrior(AArgument: TRtfArgument);
var ATable: TRtfDataset;
begin
  if not AArgument.Check(0,[etDataset])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  ATable := TRtfDataset(Ptrint(AArgument[0].Value));
  ATable.Prior;
end;

procedure TtiRtfParser.UdfInt(AArgument: TRtfArgument);
//Returns the integer part of a float
begin
  if not AArgument.Check(0,[etLitFloat, etLitInt])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  if AArgument[0].Token = etLitFloat then begin
    AArgument.Token := etLitFloat;
    AArgument.Value := Int(AArgument[0].Value);
  end else begin
    AArgument.Token := etLitInt;
    AArgument.Value := AArgument[0].Value;
  end;
end;

procedure TtiRtfParser.UdfFrac(AArgument: TRtfArgument);
//Returns the fractional part of a float
begin
  if not AArgument.Check(0,[etLitFloat, etLitInt])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  if AArgument[0].Token = etLitFloat then begin
    AArgument.Token := etLitFloat;
    AArgument.Value := Frac(AArgument[0].Value);
  end else begin
    AArgument.Token := etLitInt;
    AArgument.Value := 0;
  end;
end;

procedure TtiRtfParser.UdfRound(AArgument: TRtfArgument);
//0.5 is always rounded to largest integer number
var
  ASign, ADecimals, AInt: integer;
  AFloat, APower, AFrac: Double;
begin
  ADecimals := 0;
  if not AArgument.Check(0,[etLitFloat, etLitInt]) then
    raise TRtfException.Create(rsUnexpectedParameterType);
  if AArgument.Count > 1 then
  begin
    if not AArgument.Check(1,[etLitInt]) then
      raise TRtfException.Create(rsUnexpectedParameterType);
    ADecimals := AArgument[1].Value;
  end;

  if AArgument[0].Token = etLitFloat then
  begin
    AFloat := AArgument[0].Value;

    ASign := 1;
    if AFloat < 0 then
    begin
      AFloat := - AFloat;
      ASign := - 1;
    end;

    if ADecimals = 0 then
    begin
      AInt := Trunc(AFloat); //integer part
      AFrac := AFloat - AInt; //fractional part
      if AFrac >= 0.5 then
        AArgument.Value := ASign *(AInt + 1)
      else
        AArgument.Value := ASign * AInt;
      AArgument.Token := etLitFloat;
    end
    else
    begin
      APower := IntPower(10, ADecimals);
      AInt := Trunc(AFloat * APower); //integer part * 10^ADecimals
      AFrac := AFloat * APower - AInt; //fractional part
      if AFrac >= 0.5 then
        AArgument.Value := ASign * IntPower(10, - ADecimals) *(AInt + 1)
      else
        AArgument.Value := ASign * IntPower(10, - ADecimals) * AInt;
      AArgument.Token := etLitFloat;
    end;
  end
  else
  begin
    AArgument.Token := etLitInt;
    AArgument.Value := AArgument[0].Value;
  end;
end;

procedure TtiRtfParser.UdfTrunc(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitFloat, etLitInt])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  if AArgument[0].Token = etLitFloat then begin
    AArgument.Token := etLitFloat;
    AArgument.Value := Trunc(AArgument[0].Value);
  end else begin
    AArgument.Token := etLitInt;
    AArgument.Value := AArgument[0].Value;
  end;
end;

procedure TtiRtfParser.UdfIntPower(AArgument: TRtfArgument);
var AFloat: Double;
  AExponent: integer;
begin
  if not AArgument.Check([[etLitInt, etLitFloat],[etLitInt]]) then
    raise TRtfException.Create(rsUnexpectedParameterType);
  AFloat := AArgument[0].Value;
  AExponent := AArgument[1].Value;
  AArgument.Token := etLitFloat;
  AArgument.Value := IntPower(AFloat, AExponent);
end;

procedure TtiRtfParser.UdfPower(AArgument: TRtfArgument);
var AFloat, AExponent: Double;
begin
  if not AArgument.Check([[etLitInt, etLitFloat],[etLitInt, etLitFloat]])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AFloat := AArgument[0].Value;
  AExponent := AArgument[1].Value;
  AArgument.Token := etLitFloat;
  AArgument.Value := Pwr(AFloat, AExponent);
end;

procedure TtiRtfParser.UdfEmpty(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitString])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  if Trim(AArgument[0].Value) = '' then begin
    AArgument.Token := etLitTrue;
    AArgument.Value := true;
  end else begin
    AArgument.Token := etLitFalse;
    AArgument.Value := false;
  end;
end;

procedure TtiRtfParser.UdfDay(AArgument: TRtfArgument);
var AYear, AMonth, ADay: word;
begin
  if not AArgument.Check(0,[etLitDate])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  DecodeDate(AArgument[0].Value, AYear, AMonth, ADay);
  AArgument.Token := etLitInt;
  AArgument.Value := AYear;
end;

procedure TtiRtfParser.UdfMonth(AArgument: TRtfArgument);
var AYear, AMonth, ADay: word;
begin
  if not AArgument.Check(0,[etLitDate])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  DecodeDate(AArgument[0].Value, AYear, AMonth, ADay);
  AArgument.Token := etLitInt;
  AArgument.Value := AMonth;
end;

procedure TtiRtfParser.UdfYear(AArgument: TRtfArgument);
var AYear, AMonth, ADay: word;
begin
  if not AArgument.Check(0,[etLitDate])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  DecodeDate(AArgument[0].Value, AYear, AMonth, ADay);
  AArgument.Token := etLitInt;
  AArgument.Value := ADay;
end;

procedure TtiRtfParser.UdfShortDayName(AArgument: TRtfArgument);
var AYear, AMonth, ADay: word;
begin
  if not AArgument.Check(0,[etLitDate])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  DecodeDate(AArgument[0].Value, AYear, AMonth, ADay);
  AArgument.Token := etLitString;
  AArgument.Value := ShortDayNames[ADay];
end;

procedure TtiRtfParser.UdfShortMonthName(AArgument: TRtfArgument);
var AYear, AMonth, ADay: word;
begin
  if not AArgument.Check(0,[etLitDate])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  DecodeDate(AArgument[0].Value, AYear, AMonth, ADay);
  AArgument.Token := etLitString;
  AArgument.Value := ShortMonthNames[AMonth];
end;

procedure TtiRtfParser.UdfLongDayName(AArgument: TRtfArgument);
var AYear, AMonth, ADay: word;
begin
  if not AArgument.Check(0,[etLitDate])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  DecodeDate(AArgument[0].Value, AYear, AMonth, ADay);
  AArgument.Token := etLitString;
  AArgument.Value := LongMonthNames[AMonth];
end;

procedure TtiRtfParser.UdfLongMonthName(AArgument: TRtfArgument);
var AYear, AMonth, ADay: word;
begin
  if not AArgument.Check(0,[etLitDate])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  DecodeDate(AArgument[0].Value, AYear, AMonth, ADay);
  AArgument.Token := etLitString;
  AArgument.Value := LongMonthNames[AMonth];
end;

procedure TtiRtfParser.UdfSDay(AArgument: TRtfArgument);
var AYear, AMonth, ADay: word;
begin
  if not AArgument.Check(0,[etLitDate])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  DecodeDate(AArgument[0].Value, AYear, AMonth, ADay);
  AArgument.Token := etLitString;
  AArgument.Value := Format('%0.4d',[AYear]);
end;

procedure TtiRtfParser.UdfSMonth(AArgument: TRtfArgument);
var AYear, AMonth, ADay: word;
begin
  if not AArgument.Check(0,[etLitDate])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  DecodeDate(AArgument[0].Value, AYear, AMonth, ADay);
  AArgument.Token := etLitString;
  AArgument.Value := Format('%0.2d',[AMonth]);
end;

procedure TtiRtfParser.UdfSYear(AArgument: TRtfArgument);
var AYear, AMonth, ADay: word;
begin
  if not AArgument.Check(0,[etLitDate])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  DecodeDate(AArgument[0].Value, AYear, AMonth, ADay);
  AArgument.Token := etLitString;
  AArgument.Value := Format('%0.2d',[ADay]);
end;

procedure TtiRtfParser.UdfStod(AArgument: TRtfArgument);
var AYear, AMonth, ADay: word;
  AStr: string;
begin
  if not AArgument.Check(0,[etLitString])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AStr := AArgument[0].Value;
  AYear := StrToInt(Copy(AStr, 1, 4));
  AMonth := StrToInt(Copy(AStr, 5, 2));
  ADay := StrToInt(Copy(AStr, 7, 2));
  AArgument.Token := etLitDate;
  AArgument.Value := EncodeDate(AYear, AMonth, ADay);
end;

procedure TtiRtfParser.UdfDateTimeTo(AArgument: TRtfArgument; AFormat: string);
begin
  if not AArgument.Check(0,[etLitDate]) then
    raise TRtfException.Create(rsUnexpectedParameterType);
  if AArgument.Count > 1 then
  begin
    if not AArgument.Check(1,[etLitString]) then
      raise TRtfException.Create(rsUnexpectedParameterType);
    AFormat := AArgument[1].Value;
  end;
  AArgument.Token := etLitString;
  AArgument.Value := FormatDateTime(AFormat, AArgument[0].Value);
end;

procedure TtiRtfParser.UdfDtos(AArgument: TRtfArgument);
begin
  UdfDateTimeTo(AArgument, 'YYYYMMDD');
end;

procedure TtiRtfParser.UdfDateToStr(AArgument: TRtfArgument);
begin
  UdfDateTimeTo(AArgument, ShortDateFormat);
end;

procedure TtiRtfParser.UdfTimeToStr(AArgument: TRtfArgument);
begin
  UdfDateTimeTo(AArgument, ShortTimeFormat);
end;

procedure TtiRtfParser.UdfDateTimeToStr(AArgument: TRtfArgument);
begin
  UdfDateTimeTo(AArgument, ShortDateFormat + ' ' + ShortTimeFormat);
end;

procedure TtiRtfParser.UdfStrToDate(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitString]) then
    raise TRtfException.Create(rsUnexpectedParameterType);
  AArgument.Token := etLitString;
  AArgument.Value := StrToDate(AArgument[0].Value);
end;

procedure TtiRtfParser.UdfStrToTime(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitString])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AArgument.Token := etLitString;
  AArgument.Value := StrToTime(AArgument[0].Value);
end;

procedure TtiRtfParser.UdfStrToDateTime(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitString])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  AArgument.Token := etLitString;
  AArgument.Value := StrToDateTime(AArgument[0].Value);
end;

procedure TtiRtfParser.UdfFormatFloat(AArgument: TRtfArgument);
var AFormat: string;
  AFloat: Double;
begin
  if not AArgument.Check([[etLitString],[etLitFloat, etLitInt]]) then
    raise TRtfException.Create(rsUnexpectedParameterType);
  AFormat := AArgument[0].Value;
  AFloat := AArgument[1].Value;
  AArgument.Token := etLitString;
  AArgument.Value := FormatFloat(AFormat, AFloat);
end;

procedure TtiRtfParser.UdfFBool(AArgument: TRtfArgument);
begin
  if not AArgument.Check(0,[etLitFalse, etLitTrue]) then
    raise TRtfException.Create(rsUnexpectedParameterType);
  if AArgument[0].Token = etLitFalse then
  begin
    AArgument.Token := etLitString;
    AArgument.Value := BoolFalse;
  end else begin
    AArgument.Token := etLitString;
    AArgument.Value := BoolTrue;
  end;
end;

procedure TtiRtfParser.UdfPicture(AArgument: TRtfArgument);
var
  APicture: TPicture;
  AFilename: string;
begin
(*
  if not AArgument.Check(0,[etLitString])
  then raise TRtfException.Create(rsUnexpectedParameterType);
  APicture := TPicture.Create;
  try
    AFilename := AArgument[0].Value;
    if Assigned(OnPicturePath)
    then OnPicturePath(AFilename);

    APicture.LoadFromFile(AFilename);
    if not Assigned(APicture.Graphic)or APicture.Graphic.Empty then begin
      AArgument.Token := etNothing;
      AArgument.Value := varEmpty;
    end else begin
      AArgument.Value := AArgument.GetPicture(APicture);
      AArgument.Token := etLitString;
    end;
  finally
    APicture.Free;
  end;
  *)
end;

procedure TtiRtfParser.UdfDbPicture(AArgument: TRtfArgument);
//Since a simple blob (not ftGraphic field) can contain an image
//It has become kind of a typecast for a dataset field.
var ADataset: TRtfDataset;
  AFieldName: string;
begin
  if not AArgument.Check(0,[etFieldName]) then
    raise TRtfException.Create(rsUnexpectedParameterType);
  ADataset := Datasets.Find(AArgument[0].Value, AFieldName);
  if not Assigned(ADataset) then //There should be at least a table reference.
    raise TRtfException.Create('Dataset not found');
  AArgument.GetGraphicsValue(ADataset, AFieldName);
end;

end.





