{
    This file is part of the Free Component Library

    Pascal source lexical scanner
    Copyright (c) 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit PScanner;
{$ENDIF FPC_DOTTEDUNITS}

{$i fcl-passrc.inc}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  {$ifdef pas2js}
  js,
  {$IFDEF NODEJS}
  Node.FS,
  {$ENDIF}
  System.Types,
  {$endif}
  System.SysUtils, System.Classes, System.Types;
{$ELSE FPC_DOTTEDUNITS}
uses
  {$ifdef pas2js}
  js,
  {$IFDEF NODEJS}
  Node.FS,
  {$ENDIF}
  {$endif}
  SysUtils, Classes, Types;
{$ENDIF FPC_DOTTEDUNITS}

// message numbers
const
  nErrInvalidCharacter = 1001;
  nErrOpenString = 1002;
  nErrIncludeFileNotFound = 1003;
  nErrIfXXXNestingLimitReached = 1004;
  nErrInvalidPPElse = 1005;
  nErrInvalidPPEndif = 1006;
  nLogOpeningFile = 1007;
  nLogLineNumber = 1008; // same as FPC
  nLogIFDefAccepted = 1009;
  nLogIFDefRejected = 1010;
  nLogIFNDefAccepted = 1011;
  nLogIFNDefRejected = 1012;
  nLogIFAccepted = 1013;
  nLogIFRejected = 1014;
  nLogIFOptAccepted = 1015;
  nLogIFOptRejected = 1016;
  nLogELSEIFAccepted = 1017;
  nLogELSEIFRejected = 1018;
  nErrInvalidMode = 1019;
  nErrInvalidModeSwitch = 1020;
  nErrXExpectedButYFound = 1021;
  nErrRangeCheck = 1022;
  nErrDivByZero = 1023;
  nErrOperandAndOperatorMismatch = 1024;
  nUserDefined = 1025;
  nLogMacroDefined = 1026; // FPC=3101
  nLogMacroUnDefined = 1027; // FPC=3102
  nWarnIllegalCompilerDirectiveX = 1028;
  nIllegalStateForWarnDirective = 1027;
  nErrIncludeLimitReached = 1028;
  nMisplacedGlobalCompilerSwitch = 1029;
  nLogMacroXSetToY = 1030;
  nInvalidDispatchFieldName = 1031;
  nErrWrongSwitchToggle = 1032;
  nNoResourceSupport = 1033;
  nResourceFileNotFound = 1034;
  nErrInvalidMultiLineLineEnding = 1035;
  nWarnIgnoringLinkLib = 1036;
  nErrInvalidIndent = 1037;
  nErrMultilineNonWhiteSpaceBeforeClosing = 1038;
  nErrInvalidMultiLineTrimLeft = 1039;

// resourcestring patterns of messages
resourcestring
  SErrInvalidCharacter = 'Invalid character ''%s''';
  SErrOpenString = 'string exceeds end of line';
  SErrIncludeFileNotFound = 'Could not find include file ''%s''';
  SErrResourceFileNotFound = 'Could not find resource file ''%s''';
  SErrIfXXXNestingLimitReached = 'Nesting of $IFxxx too deep';
  SErrInvalidPPElse = '$ELSE without matching $IFxxx';
  SErrInvalidPPEndif = '$ENDIF without matching $IFxxx';
  SLogOpeningFile = 'Opening source file "%s".';
  SLogLineNumber = 'Reading line %d.';
  SLogIFDefAccepted = 'IFDEF %s found, accepting.';
  SLogIFDefRejected = 'IFDEF %s found, rejecting.';
  SLogIFNDefAccepted = 'IFNDEF %s found, accepting.';
  SLogIFNDefRejected = 'IFNDEF %s found, rejecting.';
  SLogIFAccepted = 'IF %s found, accepting.';
  SLogIFRejected = 'IF %s found, rejecting.';
  SLogIFOptAccepted = 'IFOpt %s found, accepting.';
  SLogIFOptRejected = 'IFOpt %s found, rejecting.';
  SLogELSEIFAccepted = 'ELSEIF %s found, accepting.';
  SLogELSEIFRejected = 'ELSEIF %s found, rejecting.';
  SErrInvalidMode = 'Invalid mode: "%s"';
  SErrInvalidModeSwitch = 'Invalid mode switch: "%s"';
  SErrXExpectedButYFound = '"%s" expected, but "%s" found';
  SErrRangeCheck = 'range check failed';
  SErrDivByZero = 'division by zero';
  SErrOperandAndOperatorMismatch = 'operand and operator mismatch';
  SUserDefined = 'User defined: "%s"';
  SLogMacroDefined = 'Macro defined: %s';
  SLogMacroUnDefined = 'Macro undefined: %s';
  SWarnIllegalCompilerDirectiveX = 'Illegal compiler directive "%s"';
  SIllegalStateForWarnDirective = 'Illegal state "%s" for $WARN directive';
  SErrIncludeLimitReached = 'Include file limit reached';
  SMisplacedGlobalCompilerSwitch = 'Misplaced global compiler switch, ignored';
  SLogMacroXSetToY = 'Macro %s set to %s';
  SInvalidDispatchFieldName = 'Invalid Dispatch field name';
  SErrWrongSwitchToggle = 'Wrong switch toggle, use ON/OFF or +/-';
  SNoResourceSupport = 'No support for resources of type "%s"';
  SErrInvalidMultiLineLineEnding = 'Invalid multilinestring line ending type: use one of CR/LF/CRLF/SOURCE/PLATFORM' ;
  SWarnIgnoringLinkLib = 'Ignoring LINKLIB directive %s -> %s (Options: %s)';
  SErrInvalidIndent = ' Inconsistent indent characters';
  SErrMultilineNonWhiteSpaceBeforeClosing = 'There should be no white-space characters before closing quotes of the text block';
  SErrInvalidMultiLineTrimLeft = 'Invalid MultiLineStringTrimLeft value: "%s", use ALL/AUTO/NONE or 0..65535';

type
  {$IFDEF PAS2JS}
    RTLString = string;
    TRTLStringDynArray = array of RTLString;
    TPasScannerString = String;
    AnsiChar = Char;
  {$ELSE}
    {$IF NOT DECLARED(RTLSTRING) }
      RTLString = ansistring;
      TRTLStringDynArray = array of RTLString;
    {$ENDIF}
    // String used for scanning
    TPasScannerString = RawByteString;
  {$ENDIF}

  // String used for interfacing with PasTree
  TPasTreeString = String;

  TMessageType = (
    mtFatal,
    mtError,
    mtWarning,
    mtNote,
    mtHint,
    mtInfo,
    mtDebug
    );
  TMessageTypes = set of TMessageType;

  TMessageArgs = array of String;

  TToken = (
    tkEOF,
    tkWhitespace,
    tkComment,
    tkIdentifier,
    tkString, // string literal including quotes, e.g. 'a'#13^M''''
    tkStringMultiLine, // string literal in raw format
    tkNumber,
    tkChar, // ^A .. ^Z
    // Simple (one-character) tokens
    tkBraceOpen,             // '('
    tkBraceClose,            // ')'
    tkMul,                   // '*'
    tkPlus,                  // '+'
    tkComma,                 // ','
    tkMinus,                 // '-'
    tkDot,                   // '.'
    tkDivision,              // '/'
    tkColon,                 // ':'
    tkSemicolon,             // ';'
    tkLessThan,              // '<'
    tkEqual,                 // '='
    tkGreaterThan,           // '>'
    tkAt,                    // '@'
    tkSquaredBraceOpen,      // '['
    tkSquaredBraceClose,     // ']'
    tkCaret,                 // '^'
    tkBackslash,             // '\'
    // Two-character tokens
    tkDotDot,                // '..'
    tkAssign,                // ':='
    tkNotEqual,              // '<>'
    tkLessEqualThan,         // '<='
    tkGreaterEqualThan,      // '>='
    tkPower,                 // '**'
    tkSymmetricalDifference, // '><'
    tkAssignPlus,            // +=
    tkAssignMinus,           // -=
    tkAssignMul,             // *=
    tkAssignDivision,        // /=
    tkAtAt,                  // @@
    // Three-character tokens
    tkDotDotDot,             // ... (mac mode)
    // Reserved words
    tkabsolute,
    tkand,
    tkarray,
    tkas,
    tkasm,
    tkbegin,
    tkbitpacked,
    tkcase,
    tkclass,
    tkconst,
    tkconstref,
    tkconstructor,
    tkcontains,
    tkdestructor,
    tkdispinterface,
    tkdiv,
    tkdo,
    tkdownto,
    tkelse,
    tkend,
    tkexcept,
    tkexports,
    tkfalse,
    tkfile,
    tkfinalization,
    tkfinally,
    tkfor,
    tkfunction,
    tkgeneric,
    tkgoto,
    tkif,
    tkimplementation,
    tkin,
    tkinherited,
    tkinitialization,
    tkinline,
    tkinterface,
    tkis,
    tklabel,
    tklibrary,
    tkmod,
    tknil,
    tknot,
    tkobjccategory,
    tkobjcclass,
    tkobjcprotocol,
    tkobject,
    tkof,
    tkoperator,
    tkor,
    tkotherwise,
    tkpacked,
    tkPackage,
    tkprocedure,
    tkprogram,
    tkproperty,
    tkraise,
    tkrecord,
    tkrepeat,
    tkrequires,
    tkResourceString,
    tkself,
    tkset,
    tkshl,
    tkshr,
    tkspecialize,
//    tkstring,
    tkthen,
    tkthreadvar,
    tkto,
    tktrue,
    tktry,
    tktype,
    tkunit,
    tkuntil,
    tkuses,
    tkvar,
    tkwhile,
    tkwith,
    tkxor,
    tkLineEnding,
    tkTab
    );
  TTokens = set of TToken;

  // for the fpc counterparts see fpc/compiler/globtype.pas
  TModeSwitch = (
    msNone,
    { generic }
    msFpc, msObjfpc, msDelphi, msDelphiUnicode, msTP7, msMac, msIso, msExtpas, msGPC,
    { more specific }
    msClass,               { delphi class model }
    msObjpas,              { load objpas unit }
    msResult,              { result in functions }
    msStringPchar,         { PAnsiChar 2 TPasScannerString conversion }
    msCVarSupport,         { cvar variable directive }
    msNestedComment,       { nested comments }
    msTPProcVar,           { tp style procvars (no @ needed) }
    msMacProcVar,          { macpas style procvars }
    msRepeatForward,       { repeating forward declarations is needed }
    msPointer2Procedure,   { allows the assignment of pointers to
                             procedure variables                     }
    msAutoDeref,           { does auto dereferencing of struct. vars }
    msInitFinal,           { initialization/finalization for units }
    msDefaultAnsistring,   { ansistring turned on by default }
    msOut,                 { support the calling convention OUT }
    msDefaultPara,         { support default parameters }
    msHintDirective,       { support hint directives }
    msDuplicateNames,      { allow locals/paras to have duplicate names of globals }
    msProperty,            { allow properties }
    msDefaultInline,       { allow inline proc directive }
    msExcept,              { allow exception-related keywords }
    msObjectiveC1,         { support interfacing with Objective-C (1.0) }
    msObjectiveC2,         { support interfacing with Objective-C (2.0) }
    msNestedProcVars,      { support nested procedural variables }
    msNonLocalGoto,        { support non local gotos (like iso pascal) }
    msAdvancedRecords,     { advanced record syntax with visibility sections, methods and properties }
    msISOLikeUnaryMinus,   { unary minus like in iso pascal: same precedence level as binary minus/plus }
    msSystemCodePage,      { use system codepage as compiler codepage by default, emit ansistrings with system codepage }
    msFinalFields,         { allows declaring fields as "final", which means they must be initialised
                             in the (class) constructor and are constant from then on (same as final
                             fields in Java) }
    msDefaultUnicodestring, { makes the default TPasScannerString type in $h+ mode unicodestring rather than
                               ansistring; similarly, AnsiChar becomes unicodechar rather than ansichar }
    msTypeHelpers,         { allows the declaration of "type helper" (non-Delphi) or "record helper"
                             (Delphi) for primitive types }
    msCBlocks,             { 'cblocks', support for http://en.wikipedia.org/wiki/Blocks_(C_language_extension) }
    msISOLikeIO,           { I/O as it required by an ISO compatible compiler }
    msISOLikeProgramsPara, { program parameters as it required by an ISO compatible compiler }
    msISOLikeMod,          { mod operation as it is required by an iso compatible compiler }
    msArrayOperators,      { use Delphi compatible array operators instead of custom ones ("+") }
    msMultiHelpers,        { off=only one helper per type, on=all }
    msArray2DynArray,      { regular arrays can be implicitly converted to dynamic arrays }
    msPrefixedAttributes,  { Allow attributes, disable proc modifier [] }
    msUnderscoreIsSeparator, { _ can be used as separator to group digits in numbers }
    msImplicitFunctionSpec,{ implicit function specialization }
    msFunctionReferences,  { enable Delphi-style function references }
    msAnonymousFunctions,  { enable Delphi-style anonymous functions }

    msExternalClass,       { pas2js: Allow external class definitions }
    msOmitRTTI,            { pas2js: treat class section 'published' as 'public' and typeinfo does not work on symbols declared with this switch }
    msMultiLineStrings,     { pas2js: Multiline strings }
    msDelphiMultiLineStrings, { Delpi-compatible multiline strings }
    msInlineVars              { Allow inline var declarations }
    );
  TModeSwitches = Set of TModeSwitch;

  // switches, that can be 'on' or 'off'
  TBoolSwitch = (
    bsNone,
    bsAlign,          // A   align fields
    bsBoolEval,       // B   complete boolean evaluation
    bsAssertions,     // C   generate code for assertions
    bsDebugInfo,      // D   generate debuginfo (debug lines), OR: $description 'text'
    bsExtension,      // E   output file extension
                      // F
    bsImportedData,   // G
    bsLongStrings,    // H   TPasScannerString=AnsiString
    bsIOChecks,       // I   generate EInOutError
    bsWriteableConst, // J   writable typed const
                      // K
    bsLocalSymbols,   // L   generate local symbol information (debug, requires $D+)
    bsTypeInfo,       // M   allow published members OR $M minstacksize,maxstacksize
                      // N
    bsOptimization,   // O   enable safe optimizations (-O1)
    bsOpenStrings,    // P   deprecated Delphi directive
    bsOverflowChecks, // Q   or $OV
    bsRangeChecks,    // R
                      // S
    bsTypedAddress,   // T   enabled: @variable gives typed pointer, otherwise untyped pointer
    bsSafeDivide,     // U
    bsVarStringChecks,// V   strict shortstring checking, e.g. cannot pass shortstring[3] to shortstring
    bsStackframes,    // W   always generate stackframes (debugging)
    bsExtendedSyntax, // X   deprecated Delphi directive
    bsReferenceInfo,  // Y   store for each identifier the declaration location
                      // Z
    bsHints,
    bsNotes,
    bsWarnings,
    bsMacro,
    bsScopedEnums,
    bsObjectChecks,   // check methods 'Self' and object type casts
    bsPointerMath,    // pointer arithmetic
    bsGoto       // support label and goto, set by {$goto on|off}
    );
  TBoolSwitches = set of TBoolSwitch;
const
  LetterToBoolSwitch: array['A'..'Z'] of TBoolSwitch = (
    bsAlign,          // A
    bsBoolEval,       // B
    bsAssertions,     // C
    bsDebugInfo,      // D or $description
    bsExtension,      // E
    bsNone,           // F
    bsImportedData,   // G
    bsLongStrings,    // H
    bsIOChecks,       // I or $include
    bsWriteableConst, // J
    bsNone,           // K
    bsLocalSymbols,   // L
    bsTypeInfo,       // M or $M minstacksize,maxstacksize
    bsNone,           // N
    bsOptimization,   // O
    bsOpenStrings,    // P
    bsOverflowChecks, // Q
    bsRangeChecks,    // R or $resource
    bsNone,           // S
    bsTypedAddress,   // T
    bsSafeDivide,     // U
    bsVarStringChecks,// V
    bsStackframes,    // W
    bsExtendedSyntax, // X
    bsReferenceInfo,  // Y
    bsNone            // Z
    );

  bsAll = [low(TBoolSwitch)..high(TBoolSwitch)];
  bsFPCMode: TBoolSwitches = [bsPointerMath,bsWriteableConst];
  bsObjFPCMode: TBoolSwitches = [bsPointerMath,bsWriteableConst];
  bsDelphiMode: TBoolSwitches = [bsWriteableConst,bsGoto];
  bsDelphiUnicodeMode: TBoolSwitches = [bsWriteableConst,bsGoto];
  bsMacPasMode: TBoolSwitches = [bsPointerMath,bsWriteableConst];

type
  TValueSwitch = (
    vsInterfaces,
    vsDispatchField,
    vsDispatchStrField
    );
  TValueSwitches = set of TValueSwitch;
  TValueSwitchArray = array[TValueSwitch] of TPasScannerString;
const
  vsAllValueSwitches = [low(TValueSwitch)..high(TValueSwitch)];
  DefaultValueSwitches: array[TValueSwitch] of TPasScannerString = (
     'com', // vsInterfaces
     'Msg', // vsDispatchField
     'MsgStr' // vsDispatchStrField
     );
  DefaultMaxIncludeStackDepth = 20;

type
  TWarnMsgState = (
    wmsDefault,
    wmsOn,
    wmsOff,
    wmsError
  );

type
  TTokenOption = (toForceCaret,toOperatorToken);
  TTokenOptions = Set of TTokenOption;


  { TMacroDef }

  TMacroDef = Class(TObject)
  Private
    FName: TPasTreeString;
    FValue: TPasTreeString;
  Public
    Constructor Create(Const AName,AValue : TPasTreeString);
    Property Name  : TPasTreeString Read FName;
    Property Value : TPasTreeString Read FValue Write FValue;
  end;

  { TLineReader }
  TEOLStyle = (elPlatform,elSource,elLF,elCR,elCRLF);

  TLineReader = class
  Private
    FFilename: String;
  Protected
    EOLStyle : TEOLStyle;
  public
    constructor Create(const AFilename: String); virtual;
    function IsEOF: Boolean; virtual; abstract;
    function ReadLine: TPasScannerString; virtual; abstract;
    function LastEOLStyle: TEOLStyle; virtual;
    property Filename: String read FFilename;
  end;

  { TFileLineReader }

  TFileLineReader = class(TLineReader)
  private
    {$ifdef pas2js}
    {$else}
    FTextFile: Text;
    FFileOpened: Boolean;
    FBuffer : Array[0..4096-1] of byte;
    {$endif}
  public
    constructor Create(const AFilename: String); override;
    destructor Destroy; override;
    function IsEOF: Boolean; override;
    function ReadLine: TPasScannerString; override;
  end;

  { TStreamLineReader }

  TStreamLineReader = class(TLineReader)
  private
    {$ifndef pas2js}
    FContent: RawByteString;
    {$ELSE}
    FContent: String;
    {$ENDIF}
    FPos : Integer;
  public
    Procedure InitFromStream(AStream : TStream);
    Procedure InitFromString(const s: TPasScannerString);
    function IsEOF: Boolean; override;
    function ReadLine: TPasScannerString; override;
  end;

  { TFileStreamLineReader }

  TFileStreamLineReader = class(TStreamLineReader)
  Public
    constructor Create(const AFilename: String); override;
  end;

  { TStringStreamLineReader }

  TStringStreamLineReader = class(TStreamLineReader)
  Public
    constructor Create(const AFilename: String; Const ASource: TPasScannerString); reintroduce;
  end;

  { TMacroReader }

  TMacroReader = Class(TStringStreamLineReader)
  private
    FCurCol: Integer;
    FCurRow: Integer;
  Public
    Property CurCol : Integer Read FCurCol Write FCurCol;
    Property CurRow : Integer Read FCurRow Write FCurRow;
  end;

  { TBaseFileResolver }

  TBaseFileResolver = class
  private
    FBaseDirectory: String;
    FMode: TModeSwitch;
    FModuleDirectory: String;
    FResourcePaths,
    FIncludePaths: TStringList;
    FStrictFileCase : Boolean;
  Protected
    function FindIncludeFileName(const aFilename: String): String; virtual; abstract;
    procedure SetBaseDirectory(AValue: String); virtual;
    procedure SetModuleDirectory(AValue: String); virtual;
    procedure SetStrictFileCase(AValue: Boolean); virtual;
    Property IncludePaths: TStringList Read FIncludePaths;
    Property ResourcePaths: TStringList Read FResourcePaths;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddIncludePath(const APath: String); virtual;
    procedure AddResourcePath(const APath: String); virtual;
    function FindResourceFileName(const AName: String): String; virtual; abstract;
    function FindSourceFile(const AName: String): TLineReader; virtual; abstract;
    function FindIncludeFile(const AName: String): TLineReader; virtual; abstract;
    property BaseDirectory: String read FBaseDirectory write SetBaseDirectory; // e.g. current path of include file
    property Mode: TModeSwitch read FMode write FMode;
    property ModuleDirectory: String read FModuleDirectory write SetModuleDirectory; // e.g. path of module file
    property StrictFileCase : Boolean Read FStrictFileCase Write SetStrictFileCase;
  end;
  TBaseFileResolverClass = Class of TBaseFileResolver;

{$IFDEF HASFS}
  { TFileResolver }

  TFileResolver = class(TBaseFileResolver)
  private
    {$ifdef HasStreams}
    FUseStreams: Boolean;
    {$endif}
  Protected
    function SearchLowUpCase(FN: String): String;
    Function FindIncludeFileName(const AName: String): String; override;
    Function CreateFileReader(Const AFileName : String) : TLineReader; virtual;
  Public
    function FindResourceFileName(const AFileName: String): String; override;
    function FindSourceFile(const AName: String): TLineReader; override;
    function FindIncludeFile(const AName: String): TLineReader; override;
    {$ifdef HasStreams}
    Property UseStreams : Boolean Read FUseStreams Write FUseStreams;
    {$endif}
  end;
{$ENDIF}

  { TStreamResolver }

  TStreamResolver = class(TBaseFileResolver)
  Private
    FOwnsStreams: Boolean;
    FStreams : TStringList;
    function FindStream(const AName: String; ScanIncludes: Boolean): TStream;
    function FindStreamReader(const AName: String; ScanIncludes: Boolean): TLineReader;
    procedure SetOwnsStreams(AValue: Boolean);
  Protected
    function FindIncludeFileName(const aFilename: String): String; override;
  Public
    constructor Create; override;
    destructor Destroy; override;
    Procedure Clear;
    function FindResourceFileName(const AFileName: String): String; override;
    Procedure AddStream(Const AName : String; AStream : TStream);
    function FindSourceFile(const AName: String): TLineReader; override;
    function FindIncludeFile(const AName: String): TLineReader; override;
    Property OwnsStreams : Boolean Read FOwnsStreams write SetOwnsStreams;
    Property Streams: TStringList read FStreams;
  end;


const
  CondDirectiveBool: array[boolean] of TPasScannerString = (
    '0', // false
    '1'  // true  Note: True is <>'0'
    );
  MACDirectiveBool: array[boolean] of TPasScannerString = (
    'FALSE', // false
    'TRUE'  // true  Note: True is <>'0'
    );

type
  TMaxPrecInt = {$ifdef fpc}int64{$else}NativeInt{$endif};
  TMaxFloat = {$ifdef fpc}extended{$else}double{$endif};

  TCondDirectiveEvaluator = class;

  TCEEvalVarEvent = function(Sender: TCondDirectiveEvaluator; Name: String; out Value: String): boolean of object;
  TCEEvalFunctionEvent = function(Sender: TCondDirectiveEvaluator; Name, Param: String; out Value: String): boolean of object;
  TCELogEvent = procedure(Sender: TCondDirectiveEvaluator; Args : Array of const) of object;

  { TCondDirectiveEvaluator - evaluate $IF expression }

  TCondDirectiveEvaluator = class
  private
    FOnEvalFunction: TCEEvalFunctionEvent;
    FOnEvalVariable: TCEEvalVarEvent;
    FOnLog: TCELogEvent;
  protected
    type
      TPrecedenceLevel = (
        ceplFirst, // tkNot
        ceplSecond, // *, /, div, mod, and, shl, shr
        ceplThird, // +, -, or, xor
        ceplFourth // =, <>, <, >, <=, >=
        );
      TStackItem = record
        Level: TPrecedenceLevel;
        Operathor: TToken;
        Operand: TPasScannerString;
        OperandPos: integer;
      end;
  protected
    {$ifdef UsePChar}
    FTokenStart: PAnsiChar;
    FTokenEnd: PAnsiChar;
    {$else}
    FTokenStart: integer; // position in Expression
    FTokenEnd: integer; // position in Expression
    {$endif}
    FToken: TToken;
    FStack: array of TStackItem;
    FStackTop: integer;
    function IsFalse(const Value: TPasScannerString): boolean; inline;
    function IsTrue(const Value: TPasScannerString): boolean; inline;
    function IsInteger(const Value: TPasScannerString; out i: TMaxPrecInt): boolean;
    function IsExtended(const Value: TPasScannerString; out e: TMaxFloat): boolean;
    procedure NextToken;
    procedure Log(aMsgType: TMessageType; aMsgNumber: integer;
      const aMsgFmt: String; const Args: array of const; MsgPos: integer = 0);
    procedure LogXExpectedButTokenFound(const X: TPasScannerString; ErrorPos: integer = 0);
    procedure ReadOperand(Skip: boolean = false); // unary operators plus one operand
    procedure ReadExpression; // binary operators
    procedure ResolveStack(MinStackLvl: integer; Level: TPrecedenceLevel;
      NewOperator: TToken);
    function GetTokenString: TPasScannerString;
    function GetStringLiteralValue: TPasScannerString; // read value of tkString
    procedure Push(const AnOperand: TPasScannerString; OperandPosition: integer);
  public
    Expression: TPasScannerString;
    MsgCurLine : Integer;
    MsgPos: integer;
    MsgNumber: integer;
    MsgType: TMessageType;
    MsgPattern: String; // Format parameter
    isMac : Boolean;
    constructor Create(aIsMac : Boolean = False);
    destructor Destroy; override;
    function Eval(const Expr: TPasScannerString): boolean;
    property OnEvalVariable: TCEEvalVarEvent read FOnEvalVariable write FOnEvalVariable;
    property OnEvalFunction: TCEEvalFunctionEvent read FOnEvalFunction write FOnEvalFunction;
    property OnLog: TCELogEvent read FOnLog write FOnLog;
  end;

  EScannerError       = class(Exception);
  EFileNotFoundError  = class(Exception);

  TPascalScannerPPSkipMode = (ppSkipNone, ppSkipIfBranch, ppSkipElseBranch, ppSkipAll);

  TPOption = (
    po_delphi,               // DEPRECATED since fpc 3.1.1: Delphi mode: forbid nested comments
    po_KeepScannerError,     // default: catch EScannerError and raise an EParserError instead
    po_CAssignments,         // allow C-operators += -= *= /=
    po_ResolveStandardTypes, // search for 'longint', 'TPasScannerString', etc., do not use dummies, TPasResolver sets this to use its declarations
    po_AsmWhole,             // store whole text between asm..end in TPasImplAsmStatement.Tokens
    po_NoOverloadedProcs,    // do not create TPasOverloadedProc for procs with same name
    po_KeepClassForward,     // disabled: delete class fowards when there is a class declaration
    po_ArrayRangeExpr,       // enable: create TPasArrayType.IndexRange, disable: create TPasArrayType.Ranges
    po_SelfToken,            // Self is a token. For backward compatibility.
    po_CheckModeSwitches,    // error on unknown modeswitch with an error
    po_CheckCondFunction,    // error on unknown function in conditional expression, default: return '0'
    po_StopOnErrorDirective, // error on user $Error, $message error|fatal
    po_ExtConstWithoutExpr,  // allow typed const without expression in external class and with external modifier
    po_StopOnUnitInterface,  // parse only a unit name and stop at interface keyword
    po_IgnoreUnknownResource,// Ignore resources for which no handler is registered.
    po_AsyncProcs,           // allow async procedure modifier
    po_DisableResources,     // Disable resources altogether
    po_AsmPascalComments,    // Allow pascal comments/directives in asm blocks
    po_AllowMem,             // Allow use of meml, mem, memw arrays
    po_WarnResourceNotFound, // Do not raise error if resource not found.
    po_CheckDirectiveRTTI    // parse $RTTI directive and error on invalid
    );
  TPOptions = set of TPOption;

type
  TPasSourcePos = Record
    FileName: TPasScannerString;
    Row, Column: Cardinal;
  end;
const
  DefPasSourcePos: TPasSourcePos = (Filename:''; Row:0; Column:0);

type
  { TPascalScanner }

  TPScannerLogHandler = Procedure (Sender : TObject; Const Msg : String) of object;
  TPScannerLogEvent = (sleFile,sleLineNumber,sleConditionals,sleDirective);
  TPScannerLogEvents = Set of TPScannerLogEvent;
  TPScannerDirectiveEvent = procedure(Sender: TObject; Directive, Param: TPasScannerString; var Handled: boolean) of object;
  TPScannerCommentEvent = procedure(Sender: TObject; aComment : TPasScannerString) of object;
  TPScannerFormatPathEvent = function(const aPath: String): String of object;
  TPScannerWarnEvent = procedure(Sender: TObject; Identifier: TPasScannerString; State: TWarnMsgState; var Handled: boolean) of object;
  TPScannerModeDirective = procedure(Sender: TObject; NewMode: TModeSwitch; Before: boolean; var Handled: boolean) of object;
  TPScannerLinkLibEvent = procedure(Sender: TObject; Const aLibName,aLibAlias,aLibOptions : TPasScannerString; var Handled: boolean) of object;

  // aFileName: full filename (search is already done) aOptions: list of name:value pairs.
  TResourceHandler = Procedure (Sender : TObject; const aFileName : String; aOptions : TStrings) of object;

  TPasScannerTokenPos = {$ifdef UsePChar}PAnsiChar{$else}integer{$endif};

  TPascalScanner = class
  private
    type
      TResourceHandlerRecord = record
        Ext : TPasScannerString;
        Handler : TResourceHandler;
      end;
      TDirectiveHandlerRecord = record
        Directive : TPasScannerString;
        Handler : TPScannerDirectiveEvent;
      end;
      TWarnMsgNumberState = record
        Number: integer;
        State: TWarnMsgState;
      end;
      TWarnMsgNumberStateArr = array of TWarnMsgNumberState;
    procedure HandleTextBlock(const AParam: TPasScannerString);
  private
    FAllowedBoolSwitches: TBoolSwitches;
    FAllowedModeSwitches: TModeSwitches;
    FAllowedValueSwitches: TValueSwitches;
    FConditionEval: TCondDirectiveEvaluator;
    FCurModulename: TPasTreeString;
    FCurrentBoolSwitches: TBoolSwitches;
    FCurrentModeSwitches: TModeSwitches;
    FCurrentValueSwitches: TValueSwitchArray;
    FCurtokenEscaped: Boolean;
    FCurTokenPos: TPasSourcePos;
    FLastMsg: String;
    FLastMsgArgs: TMessageArgs;
    FLastMsgNumber: integer;
    FLastMsgPattern: String;
    FLastMsgType: TMessageType;
    FFileResolver: TBaseFileResolver;
    FCurSourceFile: TLineReader;
    FCurFilename: String;
    FCurRow: Integer;
    FCurColumnOffset: integer;
    FCurToken: TToken;
    FCurTokenString: TPasScannerString;
    FCurLine: TPasScannerString;
    FMaxIncludeStackDepth: integer;
    FModuleRow: Integer;
    FMacros: TStrings; // Objects are TMacroDef
    FDefines: TStrings;
    FMultilineStringsEOLStyle: TEOLStyle;
    FMultilineStringsTrimLeft: Integer;
    FNonTokens: TTokens;
    FOnComment: TPScannerCommentEvent;
    FOnDirective: TPScannerDirectiveEvent;
    FOnDirectiveForConditionals: Boolean;
    FOnEvalFunction: TCEEvalFunctionEvent;
    FOnEvalVariable: TCEEvalVarEvent;
    FOnFormatPath: TPScannerFormatPathEvent;
    FOnLinkLib: TPScannerLinkLibEvent;
    FOnModeChanged: TPScannerModeDirective;
    FOnWarnDirective: TPScannerWarnEvent;
    FOptions: TPOptions;
    FLogEvents: TPScannerLogEvents;
    FOnLog: TPScannerLogHandler;
    FPreviousToken: TToken;
    FReadOnlyBoolSwitches: TBoolSwitches;
    FReadOnlyModeSwitches: TModeSwitches;
    FReadOnlyValueSwitches: TValueSwitches;
    FSkipComments: Boolean;
    FSkipGlobalSwitches: boolean;
    FSkipWhiteSpace: Boolean;
    FTokenOptions: TTokenOptions;
    FTokenPos: TPasScannerTokenPos; // position in FCurLine }
    FIncludeStack: TFPList;
    FFiles: TStrings;
    FWarnMsgStates: TWarnMsgNumberStateArr;
    FResourceHandlers: Array of TResourceHandlerRecord;
    FDirectiveHandles: Array of TDirectiveHandlerRecord;

    // Preprocessor $IFxxx skipping data
    PPSkipMode: TPascalScannerPPSkipMode;
    PPIsSkipping: Boolean;
    PPSkipStackIndex: Integer;
    PPSkipModeStack: array[0..255] of TPascalScannerPPSkipMode;
    PPIsSkippingStack: array[0..255] of Boolean;
    function GetCurColumn: Integer;
    function GetCurrentValueSwitch(V: TValueSwitch): TPasScannerString;
    function GetForceCaret: Boolean;
    function GetMacrosOn: boolean;
    function GetTokenString: TPasTreeString; inline;
    function IndexOfWarnMsgState(Number: integer; InsertPos: boolean): integer;
    function OnCondEvalFunction(Sender: TCondDirectiveEvaluator; Name,  Param: String; out Value: String): boolean;
    procedure OnCondEvalLog(Sender: TCondDirectiveEvaluator; Args: array of const);
    function OnCondEvalVar(Sender: TCondDirectiveEvaluator; Name: String; out Value: String): boolean;
    procedure SetAllowedBoolSwitches(const AValue: TBoolSwitches);
    procedure SetAllowedModeSwitches(const AValue: TModeSwitches);
    procedure SetAllowedValueSwitches(const AValue: TValueSwitches);
    procedure SetMacrosOn(const AValue: boolean);
    procedure SetOptions(AValue: TPOptions);
    procedure SetReadOnlyBoolSwitches(const AValue: TBoolSwitches);
    procedure SetReadOnlyModeSwitches(const AValue: TModeSwitches);
    procedure SetReadOnlyValueSwitches(const AValue: TValueSwitches);
  protected
    // extension without initial dot (.)
    Function IndexOfResourceHandler(Const aExt : TPasScannerString) : Integer;
    Function FindResourceHandler(Const aExt : TPasScannerString) : TResourceHandler;
    function IndexOfDirectiveHandle(const aDirective: TPasScannerString; ForInsert: boolean = false): Integer;
    function ReadIdentifier(const AParam: TPasScannerString): TPasScannerString;
    function FetchLine: boolean;
    procedure AddFile(aFilename: TPasScannerString); virtual;
    function GetMacroName(const Param: TPasScannerString): TPasScannerString;
    procedure SetCurMsg(MsgType: TMessageType; MsgNumber: integer; Const Fmt : TPasScannerString; Args : Array of const);
    procedure SetCurMsg(MsgType: TMessageType; MsgNumber: integer; Const Msg : TPasScannerString);
    Procedure DoLog(MsgType: TMessageType; MsgNumber: integer; Const Msg : TPasScannerString; SkipSourceInfo : Boolean = False); overload;
    Procedure DoLog(MsgType: TMessageType; MsgNumber: integer; Const Fmt : TPasScannerString; Args : Array of const;SkipSourceInfo : Boolean = False); overload;
    procedure ErrorAt(MsgNumber: integer; const Msg: TPasScannerString; aRow,ACol : Integer); overload;
    procedure Error(MsgNumber: integer; const Msg: TPasScannerString); overload;
    procedure Error(MsgNumber: integer; const Fmt: TPasScannerString; Args: array of const); overload;
    procedure PushSkipMode;
    function GetMultiLineStringLineEnd(aReader: TLineReader): TPasScannerString;
    function MakeLibAlias(const LibFileName: TPasScannerString): TPasScannerString; virtual;

    function HandleDirective(const ADirectiveText: TPasScannerString): TToken; virtual;
    function HandleLetterDirective(Letter: AnsiChar; Enable: boolean): TToken; virtual;
    procedure HandleBoolDirective(bs: TBoolSwitch; const Param: TPasScannerString); virtual;
    procedure DoHandleComment(Sender: TObject; const aComment : TPasScannerString); virtual;
    procedure DoHandleDirective(Sender: TObject; Directive, Param: TPasScannerString;
      var Handled: boolean); virtual;

    procedure HandleMultilineStringTrimLeft(const AParam : TPasScannerString);
    procedure HandleMultilineStringLineEnding(const AParam : TPasScannerString);
    function HandleMultilineComment: TToken;
    function HandleMultilineCommentOldStyle: TToken;
    procedure HandleIFDEF(const AParam: TPasScannerString);
    procedure HandleIFNDEF(const AParam: TPasScannerString);
    procedure HandleIFOPT(const AParam: TPasScannerString);
    procedure HandleIF(const AParam: TPasScannerString; aIsMac : Boolean);
    procedure HandleELSEIF(const AParam: TPasScannerString; aIsMac : Boolean);
    procedure HandleELSE(const AParam: TPasScannerString);
    procedure HandleENDIF(const AParam: TPasScannerString);
    procedure HandleDefine(Param: TPasScannerString); virtual;
    procedure HandleDispatchField(Param: TPasScannerString; vs: TValueSwitch); virtual;
    procedure HandleError(Param: TPasScannerString); virtual;
    procedure HandleMessageDirective(Param: TPasScannerString); virtual;
    procedure HandleIncludeFile(Param: TPasScannerString); virtual;
    procedure HandleIncludeString(Param: TPasScannerString); virtual;
    procedure HandleResource(Param : TPasScannerString); virtual;
    procedure HandleLinkLib(Param : TPasScannerString); virtual;
    procedure HandleOptimizations(Param : TPasScannerString); virtual;
    procedure DoHandleOptimization(OptName, OptValue: TPasScannerString); virtual;

    procedure HandleUnDefine(Param: TPasScannerString); virtual;

    function HandleInclude(const Param: TPasScannerString): TToken; virtual;
    procedure HandleMode(const Param: TPasScannerString); virtual;
    procedure HandleModeSwitch(const Param: TPasScannerString); virtual;
    function HandleMacro(AIndex: integer): TToken; virtual;
    procedure HandleInterfaces(const Param: TPasScannerString); virtual;
    procedure HandleWarn(Param: TPasScannerString); virtual;
    procedure HandleWarnIdentifier(Identifier, Value: TPasScannerString); virtual;
    procedure PushStackItem; virtual;
    procedure PopStackItem; virtual;
    function DoFetchTextToken: TToken; // including quotes
    function DoFetchMultilineTextToken: TToken; // back ticks are converted to apostrophes, unindented
    function DoFetchDelphiMultiLineTextToken(QuoteLen: Integer): TToken;
    function DoFetchToken: TToken;
    procedure ClearFiles;
    Procedure ClearMacros;
    Procedure SetCurToken(const AValue: TToken);
    Procedure SetCurTokenString(const AValue: TPasScannerString);
    procedure SetCurrentBoolSwitches(const AValue: TBoolSwitches); virtual;
    procedure SetCurrentModeSwitches(AValue: TModeSwitches); virtual;
    procedure SetCurrentValueSwitch(V: TValueSwitch; const AValue: TPasScannerString);
    procedure SetWarnMsgState(Number: integer; State: TWarnMsgState); virtual;
    function GetWarnMsgState(Number: integer): TWarnMsgState; virtual;
    function LogEvent(E : TPScannerLogEvent) : Boolean; inline;
    property TokenPos: TPasScannerTokenPos read FTokenPos write FTokenPos;
  public
    constructor Create(AFileResolver: TBaseFileResolver);
    destructor Destroy; override;
    // extension without initial dot  (.), case insensitive
    procedure RegisterResourceHandler(aExtension : String; const aHandler : TResourceHandler); overload;
    procedure RegisterResourceHandler(const aExtensions : Array of String; const aHandler : TResourceHandler); overload;
    procedure RegisterDirectiveHandler(const aDirective: String; const aHandler : TPScannerDirectiveEvent); overload;
    procedure RegisterDirectiveHandler(const aDirectives : TStringDynArray; const aHandler : TPScannerDirectiveEvent); overload;
    procedure OpenFile(AFilename: TPasScannerString);
    procedure FinishedModule; virtual; // called by parser after end.
    function FormatPath(const aFilename: String): String; virtual;
    function FormatSrcPos(const p: TPasSourcePos): String;
    function FormatCurrentSrcPos: String;
    procedure DisablePackageTokens;
    procedure SetNonToken(aToken : TToken);
    procedure UnsetNonToken(aToken : TToken);
    procedure SetTokenOption(aOption : TTokenoption);
    procedure UnSetTokenOption(aOption : TTokenoption);
    function CheckToken(aToken : TToken; const ATokenString : TPasScannerString) : TToken;
    function FetchToken: TToken;
    function ReadNonPascalTillEndToken(StopAtLineEnd: boolean): TToken; virtual;
    function AddDefine(const aName: TPasScannerString; Quiet: boolean = false): boolean;
    function RemoveDefine(const aName: TPasScannerString; Quiet: boolean = false): boolean;
    function UnDefine(const aName: TPasScannerString; Quiet: boolean = false): boolean; // check defines and macros
    function IsDefined(const aName: TPasScannerString): boolean; // check defines and macros
    function IfOpt(Letter: AnsiChar): boolean;
    function AddMacro(const aName, aValue: TPasScannerString; Quiet: boolean = false): boolean;
    function RemoveMacro(const aName: TPasScannerString; Quiet: boolean = false): boolean;
    procedure SetCompilerMode(S : TPasScannerString);
    procedure SetModeSwitch(S : TPasScannerString);
    function CurSourcePos: TPasSourcePos;
    function SetForceCaret(AValue : Boolean) : Boolean; // returns old state
    function IgnoreMsgType(MsgType: TMessageType): boolean; virtual;
    property FileResolver: TBaseFileResolver read FFileResolver;
    property Files: TStrings read FFiles;
    property CurSourceFile: TLineReader read FCurSourceFile;
    property CurFilename: String read FCurFilename;
    property CurModuleName: TPasTreeString read FCurModulename Write FCurModuleName;
    property CurLine: TPasScannerString read FCurLine;
    property CurRow: Integer read FCurRow;
    property CurColumn: Integer read GetCurColumn;
    property CurToken: TToken read FCurToken;
    property CurTokenEscaped : Boolean Read FCurTokenEscaped;
    property RawCurTokenString: TPasScannerString read FCurTokenString;
    property CurTokenString: TPasTreeString read GetTokenString;
    property CurTokenPos: TPasSourcePos read FCurTokenPos;
    property PreviousToken : TToken Read FPreviousToken;
    property ModuleRow: Integer read FModuleRow;
    property NonTokens : TTokens Read FNonTokens;
    Property TokenOptions : TTokenOptions Read FTokenOptions Write FTokenOptions;
    property Defines: TStrings read FDefines;
    property Macros: TStrings read FMacros;
    property MacrosOn: boolean read GetMacrosOn write SetMacrosOn;
    property AllowedModeSwitches: TModeSwitches read FAllowedModeSwitches Write SetAllowedModeSwitches;
    property ReadOnlyModeSwitches: TModeSwitches read FReadOnlyModeSwitches Write SetReadOnlyModeSwitches;// always set, cannot be disabled
    property CurrentModeSwitches: TModeSwitches read FCurrentModeSwitches Write SetCurrentModeSwitches;
    property AllowedBoolSwitches: TBoolSwitches read FAllowedBoolSwitches Write SetAllowedBoolSwitches;
    property ReadOnlyBoolSwitches: TBoolSwitches read FReadOnlyBoolSwitches Write SetReadOnlyBoolSwitches;// cannot be changed by code
    property CurrentBoolSwitches: TBoolSwitches read FCurrentBoolSwitches Write SetCurrentBoolSwitches;
    property AllowedValueSwitches: TValueSwitches read FAllowedValueSwitches Write SetAllowedValueSwitches;
    property ReadOnlyValueSwitches: TValueSwitches read FReadOnlyValueSwitches Write SetReadOnlyValueSwitches;// cannot be changed by code
    property CurrentValueSwitch[V: TValueSwitch]: TPasScannerString read GetCurrentValueSwitch Write SetCurrentValueSwitch;
    property WarnMsgState[Number: integer]: TWarnMsgState read GetWarnMsgState write SetWarnMsgState;
    property Options : TPOptions read FOptions write SetOptions;
    property SkipWhiteSpace : Boolean Read FSkipWhiteSpace Write FSkipWhiteSpace;
    property SkipComments : Boolean Read FSkipComments Write FSkipComments;
    property SkipGlobalSwitches: Boolean read FSkipGlobalSwitches write FSkipGlobalSwitches;
    property MaxIncludeStackDepth: integer read FMaxIncludeStackDepth write FMaxIncludeStackDepth default DefaultMaxIncludeStackDepth;
    property ForceCaret : Boolean read GetForceCaret;
    Property MultilineStringsEOLStyle : TEOLStyle Read FMultilineStringsEOLStyle Write FMultilineStringsEOLStyle;
    Property MultilineStringsTrimLeft : Integer Read FMultilineStringsTrimLeft Write FMultilineStringsTrimLeft; // All=-2, Auto=-1, None=0, >1 fixed amount
    Property OnDirectiveForConditionals : Boolean Read FOnDirectiveForConditionals Write FOnDirectiveForConditionals;
    property LogEvents : TPScannerLogEvents read FLogEvents write FLogEvents;
    property OnLog : TPScannerLogHandler read FOnLog write FOnLog;
    property OnFormatPath: TPScannerFormatPathEvent read FOnFormatPath write FOnFormatPath;
    property ConditionEval: TCondDirectiveEvaluator read FConditionEval;
    property OnEvalVariable: TCEEvalVarEvent read FOnEvalVariable write FOnEvalVariable;
    property OnEvalFunction: TCEEvalFunctionEvent read FOnEvalFunction write FOnEvalFunction;
    property OnWarnDirective: TPScannerWarnEvent read FOnWarnDirective write FOnWarnDirective;
    property OnModeChanged: TPScannerModeDirective read FOnModeChanged write FOnModeChanged; // set by TPasParser
    property OnDirective: TPScannerDirectiveEvent read FOnDirective write FOnDirective;
    property OnComment: TPScannerCommentEvent read FOnComment write FOnComment;
    Property OnLinkLib : TPScannerLinkLibEvent Read FOnLinkLib Write FOnLinkLib;
    property LastMsg: String read FLastMsg write FLastMsg;
    property LastMsgNumber: integer read FLastMsgNumber write FLastMsgNumber;
    property LastMsgType: TMessageType read FLastMsgType write FLastMsgType;
    property LastMsgPattern: String read FLastMsgPattern write FLastMsgPattern;
    property LastMsgArgs: TMessageArgs read FLastMsgArgs write FLastMsgArgs;
  end;

const
  TokenInfos: array[TToken] of TPasScannerString = (
    'EOF',
    'Whitespace',
    'Comment',
    'Identifier',
    'String',
    'StringMultiLine',
    'Number',
    'Character',
    '(',
    ')',
    '*',
    '+',
    ',',
    '-',
    '.',
    '/',
    ':',
    ';',
    '<',
    '=',
    '>',
    '@',
    '[',
    ']',
    '^',
    '\',
    '..',
    ':=',
    '<>',
    '<=',
    '>=',
    '**',
    '><',
    '+=',
    '-=',
    '*=',
    '/=',
    '@@',
    '...',
    // Reserved words
    'absolute',
    'and',
    'array',
    'as',
    'asm',
    'begin',
    'bitpacked',
    'case',
    'class',
    'const',
    'constref',
    'constructor',
    'contains',
    'destructor',
    'dispinterface',
    'div',
    'do',
    'downto',
    'else',
    'end',
    'except',
    'exports',
    'false',
    'file',
    'finalization',
    'finally',
    'for',
    'function',
    'generic',
    'goto',
    'if',
    'implementation',
    'in',
    'inherited',
    'initialization',
    'inline',
    'interface',
    'is',
    'label',
    'library',
    'mod',
    'nil',
    'not',
    'objccategory',
    'objcclass',
    'objcprotocol',
    'object',
    'of',
    'operator',
    'or',
    'otherwise',
    'packed',
    'package',
    'procedure',
    'program',
    'property',
    'raise',
    'record',
    'repeat',
    'requires',
    'resourcestring',
    'self',
    'set',
    'shl',
    'shr',
    'specialize',
//    'TPasScannerString',
    'then',
    'threadvar',
    'to',
    'true',
    'try',
    'type',
    'unit',
    'until',
    'uses',
    'var',
    'while',
    'with',
    'xor',
    'LineEnding',
    'Tab'
  );

  SModeSwitchNames : array[TModeSwitch] of TPasScannerString =
  ( '', // msNone
    '', // Fpc,
    '', // Objfpc,
    '', // Delphi,
    '', // DelphiUnicode,
    '', // TP7,
    '', // Mac,
    '', // Iso,
    '', // Extpas,
    '', // GPC,
    { more specific }
    'CLASS',
    'OBJPAS',
    'RESULT',
    'PCHARTOSTRING',
    'CVAR',
    'NESTEDCOMMENTS',
    'CLASSICPROCVARS',
    'MACPROCVARS',
    'REPEATFORWARD',
    'POINTERTOPROCVAR',
    'AUTODEREF',
    'INITFINAL',
    'ANSISTRINGS',
    'OUT',
    'DEFAULTPARAMETERS',
    'HINTDIRECTIVE',
    'DUPLICATELOCALS',
    'PROPERTIES',
    'ALLOWINLINE',
    'EXCEPTIONS',
    'OBJECTIVEC1',
    'OBJECTIVEC2',
    'NESTEDPROCVARS',
    'NONLOCALGOTO',
    'ADVANCEDRECORDS',
    'ISOUNARYMINUS',
    'SYSTEMCODEPAGE',
    'FINALFIELDS',
    'UNICODESTRINGS',
    'TYPEHELPERS',
    'CBLOCKS',
    'ISOIO',
    'ISOPROGRAMPARAS',
    'ISOMOD',
    'ARRAYOPERATORS',
    'MULTIHELPERS',
    'ARRAY2DYNARRAYS',
    'PREFIXEDATTRIBUTES',
    'UNDERSCOREISSEPARARTOR',
    'IMPLICITFUNCTIONSPECIALIZATION',
    'FUNCTIONREFERENCES',
    'ANONYMOUSFUNCTIONS',
    'EXTERNALCLASS',
    'OMITRTTI',
    'MULTILINESTRINGS',
    'DELPHIMULTILINESTRINGS',
    'INLINEVARS'
    );

  LetterSwitchNames: array['A'..'Z'] of TPasScannerString=(
     'ALIGN'          // A   align fields
    ,'BOOLEVAL'       // B   complete boolean evaluation
    ,'ASSERTIONS'     // C   generate code for assertions
    ,'DEBUGINFO'      // D   generate debuginfo (debug lines), OR: $description 'text'
    ,'EXTENSION'      // E   output file extension
    ,''               // F
    ,'IMPORTEDDATA'   // G
    ,'LONGSTRINGS'    // H   TPasScannerString=AnsiString
    ,'IOCHECKS'       // I   generate EInOutError
    ,'WRITEABLECONST' // J   writable typed const
    ,''               // K
    ,'LOCALSYMBOLS'   // L   generate local symbol information (debug, requires $D+)
    ,'TYPEINFO'       // M   allow published members OR $M minstacksize,maxstacksize
    ,''               // N
    ,'OPTIMIZATION'   // O   enable safe optimizations (-O1)
    ,'OPENSTRINGS'    // P   deprecated Delphi directive
    ,'OVERFLOWCHECKS' // Q
    ,'RANGECHECKS'    // R   OR resource
    ,''               // S
    ,'TYPEDADDRESS'   // T   enabled: @variable gives typed pointer, otherwise untyped pointer
    ,'SAFEDIVIDE'     // U
    ,'VARSTRINGCHECKS'// V   strict shortstring checking, e.g. cannot pass shortstring[3] to shortstring
    ,'STACKFRAMES'    // W   always generate stackframes (debugging)
    ,'EXTENDEDSYNTAX' // X   deprecated Delphi directive
    ,'REFERENCEINFO'  // Y   store for each identifier the declaration location
    ,''               // Z
   );

  BoolSwitchNames: array[TBoolSwitch] of TPasScannerString = (
    // letter directives
    'None',
    'Align',
    'BoolEval',
    'Assertions',
    'DebugInfo',
    'Extension',
    'ImportedData',
    'LongStrings',
    'IOChecks',
    'WriteableConst',
    'LocalSymbols',
    'TypeInfo',
    'Optimization',
    'OpenStrings',
    'OverflowChecks',
    'RangeChecks',
    'TypedAddress',
    'SafeDivide',
    'VarStringChecks',
    'Stackframes',
    'ExtendedSyntax',
    'ReferenceInfo',
    // other bool directives
    'Hints',
    'Notes',
    'Warnings',
    'Macro',
    'ScopedEnums',
    'ObjectChecks',
    'PointerMath',
    'Goto'
    );

  ValueSwitchNames: array[TValueSwitch] of TPasScannerString = (
    'Interfaces', // vsInterfaces
    'DispatchField', // vsDispatchField
    'DispatchStrField' // vsDispatchStrField
    );

const
  MessageTypeNames : Array[TMessageType] of TPasScannerString = (
    'Fatal','Error','Warning','Note','Hint','Info','Debug'
  );

const
  // all mode switches supported by FPC
  msAllModeSwitches = [low(TModeSwitch)..High(TModeSwitch)];
  AllLanguageModes = [msFPC..msGPC];

  DelphiModeSwitches = [msDelphi,msClass,msObjpas,msResult,msStringPchar,
     msPointer2Procedure,msAutoDeref,msTPProcVar,msInitFinal,msDefaultAnsistring,
     msOut,msDefaultPara,msDuplicateNames,msHintDirective,
     msProperty,msDefaultInline,msExcept,msAdvancedRecords,msTypeHelpers,
     msPrefixedAttributes,msArrayOperators,msImplicitFunctionSpec,
     msFunctionReferences,msAnonymousFunctions,msDelphiMultiLineStrings,
     msInlineVars
     ];

  DelphiUnicodeModeSwitches = delphimodeswitches + [msSystemCodePage,msDefaultUnicodestring];

  // mode switches of $mode FPC, don't confuse with msAllModeSwitches
  FPCModeSwitches = [msFpc,msStringPchar,msNestedComment,msRepeatForward,
    msCVarSupport,msInitFinal,msHintDirective,msProperty,msDefaultInline];
  //FPCBoolSwitches bsObjectChecks

  OBJFPCModeSwitches =  [msObjfpc,msClass,msObjpas,msResult,msStringPchar,msNestedComment,
    msRepeatForward,msCVarSupport,msInitFinal,msOut,msDefaultPara,msHintDirective,
    msProperty,msDefaultInline,msExcept,msDelphiMultiLineStrings];

  TPModeSwitches = [msTP7,msTPProcVar,msDuplicateNames];

  GPCModeSwitches = [msGPC,msTPProcVar];

  MacModeSwitches = [msMac,msCVarSupport,msMacProcVar,msNestedProcVars,
    msNonLocalGoto,msISOLikeUnaryMinus,msDefaultInline];

  ISOModeSwitches =  [msIso,msTPProcVar,msDuplicateNames,msNestedProcVars,
    msNonLocalGoto,msISOLikeUnaryMinus,msISOLikeIO,msISOLikeProgramsPara,
    msISOLikeMod];

  ExtPasModeSwitches = [msExtpas,msTPProcVar,msDuplicateNames,msNestedProcVars,
    msNonLocalGoto,msISOLikeUnaryMinus,msISOLikeIO,msISOLikeProgramsPara,
    msISOLikeMod];

function StrToModeSwitch(aName: TPasScannerString): TModeSwitch;
function ModeSwitchesToStr(Switches: TModeSwitches): TPasScannerString;
function BoolSwitchesToStr(Switches: TBoolSwitches): TPasScannerString;

function FilenameIsAbsolute(const TheFilename: String):boolean;
function FilenameIsWinAbsolute(const TheFilename: String): boolean;
function FilenameIsUnixAbsolute(const TheFilename: String): boolean;
function IsNamedToken(Const AToken : TPasScannerString; Out T : TToken) : Boolean;
Function ExtractFilenameOnly(Const AFileName : String) : String;
function ExtractFileUnitName(const aFilename: String): String;

procedure CreateMsgArgs(var MsgArgs: TMessageArgs; Args: array of const);
function SafeFormat(const Fmt: String; Args: array of const): String;

{$IFNDEF Pas2js}
procedure ReadNextPascalToken(var Position: PChar; out TokenStart: PChar;
  NestedComments: boolean; SkipDirectives: boolean);
{$ENDIF}

implementation

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.StrUtils;
  {$ELSE}
  strutils;
  {$ENDIF}
const
  IdentChars = ['0'..'9', 'A'..'Z', 'a'..'z','_'];
  Digits = ['0'..'9'];
  Letters = ['a'..'z','A'..'Z'];
  HexDigits = ['0'..'9','a'..'f','A'..'F'];
  SingleQuote = #39;

Var
  SortedTokens : array of TToken;
  LowerCaseTokens  : Array[ttoken] of TPasScannerString;

Function ExtractFilenameOnly(Const AFileName : String) : String;

begin
  Result:=ChangeFileExt(ExtractFileName(aFileName),'');
end;

function ExtractFileUnitName(const aFilename: String): String;
var
  p: Integer;
begin
  Result:=ExtractFileName(aFilename);
  if Result='' then
    exit;
  p:=rpos('.',Result);
  if p>0 then
    SetLength(Result, p-1);
end;

Procedure SortTokenInfo;

Var
  tk: tToken;
  I,J,K, l: integer;

begin
  for tk:=Low(TToken) to High(ttoken) do
    LowerCaseTokens[tk]:=LowerCase(TokenInfos[tk]);
  SetLength(SortedTokens,Ord(tkXor)-Ord(tkAbsolute)+1);
  I:=0;
  for tk := tkAbsolute to tkXOR do
    begin
    SortedTokens[i]:=tk;
    Inc(i);
    end;
  l:=Length(SortedTokens)-1;
  k:=l shr 1;
  while (k>0) do
    begin
    for i:=0 to l-k do
      begin
      j:=i;
      while (J>=0) and (LowerCaseTokens[SortedTokens[J]]>LowerCaseTokens[SortedTokens[J+K]]) do
        begin
        tk:=SortedTokens[J];
        SortedTokens[J]:=SortedTokens[J+K];
        SortedTokens[J+K]:=tk;
        if (J>K) then
          Dec(J,K)
        else
          J := 0
        end;
      end;
      K:=K shr 1;
    end;
end;

function IndexOfToken(Const AToken : TPasScannerString) : Integer;

var
  B,T,M : Integer;
  N : TPasScannerString;
begin
  B:=0;
  T:=Length(SortedTokens)-1;
  while (B<=T) do
    begin
    M:=(B+T) div 2;
    N:=LowerCaseTokens[SortedTokens[M]];
    if (AToken<N) then
      T:=M-1
    else if (AToken=N) then
      Exit(M)
    else
      B:=M+1;
    end;
  Result:=-1;
end;

function IsNamedToken(Const AToken : TPasScannerString; Out T : TToken) : Boolean;

Var
  I : Integer;

begin
  if (Length(SortedTokens)=0) then
    SortTokenInfo;
  I:=IndexOfToken(LowerCase(AToken));
  Result:=I<>-1;
  If Result then
    T:=SortedTokens[I];
end;

procedure CreateMsgArgs(var MsgArgs: TMessageArgs; Args: array of const);
var
  i: Integer;
  {$ifdef pas2js}
  v: jsvalue;
  {$endif}
begin
  SetLength(MsgArgs, High(Args)-Low(Args)+1);
  for i:=Low(Args) to High(Args) do
    {$ifdef pas2js}
    begin
    v:=Args[i];
    if isBoolean(v) then
      MsgArgs[i] := BoolToStr(Boolean(v))
    else if isString(v) then
      MsgArgs[i] := TPasScannerString(v)
    else if isNumber(v) then
      begin
      if IsInteger(v) then
        MsgArgs[i] := str(NativeInt(v))
      else
        MsgArgs[i] := str(double(v));
      end
    else
      MsgArgs[i]:='';
    end;
    {$else}
    case Args[i].VType of
      vtInteger:      MsgArgs[i] := IntToStr(Args[i].VInteger);
      vtBoolean:      MsgArgs[i] := BoolToStr(Args[i].VBoolean);
      vtChar:         MsgArgs[i] := Args[i].VChar;
      {$ifndef FPUNONE}
      vtExtended:     ; //  Args[i].VExtended^;
      {$ENDIF}
      vtString:       MsgArgs[i] := Args[i].VString^;
      vtPointer:      ; //  Args[i].VPointer;
      vtPChar:        MsgArgs[i] := Args[i].VPChar;
      vtObject:       ; //  Args[i].VObject;
      vtClass:        ; //  Args[i].VClass;
      vtWideChar:     MsgArgs[i] := AnsiString(Args[i].VWideChar);
      vtPWideChar:    MsgArgs[i] := Args[i].VPWideChar;
      vtAnsiString:   MsgArgs[i] := AnsiString(Args[i].VAnsiString);
      vtCurrency:     ; //  Args[i].VCurrency^);
      vtVariant:      ; //  Args[i].VVariant^);
      vtInterface:    ; //  Args[i].VInterface^);
      vtWidestring:   MsgArgs[i] := AnsiString(WideString(Args[i].VWideString));
      vtInt64:        MsgArgs[i] := IntToStr(Args[i].VInt64^);
      vtQWord:        MsgArgs[i] := IntToStr(Args[i].VQWord^);
      vtUnicodeString:MsgArgs[i] := AnsiString(UnicodeString(Args[i].VUnicodeString));
    end;
    {$endif}
end;

function SafeFormat(const Fmt: String; Args: array of const): String;
var
  MsgArgs: TMessageArgs;
  i: Integer;
begin
  try
    Result:=Format(Fmt,Args);
  except
    Result:='';
    MsgArgs:=nil;
    CreateMsgArgs(MsgArgs,Args);
    for i:=0 to length(MsgArgs)-1 do
      begin
      if i>0 then
        Result:=Result+',';
      Result:=Result+MsgArgs[i];
      end;
    Result:='{'+Fmt+'}['+Result+']';
  end;
end;

{$IFNDEF Pas2js}
procedure ReadNextPascalToken(var Position: PChar; out TokenStart: PChar;
  NestedComments: boolean; SkipDirectives: boolean);
const
  IdentChars = ['a'..'z','A'..'Z','_','0'..'9'];
  HexNumberChars = ['0'..'9','a'..'f','A'..'F'];
var
  c1:AnsiChar;
  CommentLvl: Integer;
  Src: PChar;
begin
  Src:=Position;
  // read till next atom
  while true do
    begin
    case Src^ of
    #0: break;
    #1..#32:  // spaces and special characters
      inc(Src);
    #$EF:
      if (Src[1]=#$BB)
      and (Src[2]=#$BF) then
        begin
        // skip UTF BOM
        inc(Src,3);
        end
      else
        break;
    '{':    // comment start or compiler directive
      if (Src[1]='$') and (not SkipDirectives) then
        // compiler directive
        break
      else begin
        // Pascal comment => skip
        CommentLvl:=1;
        while true do
          begin
          inc(Src);
          case Src^ of
          #0: break;
          '{':
            if NestedComments then
              inc(CommentLvl);
          '}':
            begin
            dec(CommentLvl);
            if CommentLvl=0 then
              begin
              inc(Src);
              break;
              end;
            end;
          end;
        end;
      end;
    '/':  // comment or real division
      if (Src[1]='/') then
        begin
        // comment start -> read til line end
        inc(Src);
        while not (Src^ in [#0,#10,#13]) do
          inc(Src);
        end
      else
        break;
    '(':  // comment, bracket or compiler directive
      if (Src[1]='*') then
        begin
        if (Src[2]='$') and (not SkipDirectives) then
          // compiler directive
          break
        else
          begin
          // comment start -> read til comment end
          inc(Src,2);
          CommentLvl:=1;
          while true do
            begin
            case Src^ of
            #0: break;
            '(':
              if NestedComments and (Src[1]='*') then
                inc(CommentLvl);
            '*':
              if (Src[1]=')') then
                begin
                dec(CommentLvl);
                if CommentLvl=0 then
                  begin
                  inc(Src,2);
                  break;
                  end;
                inc(Position);
                end;
            end;
            inc(Src);
            end;
        end;
      end else
        // round bracket open
        break;
    else
      break;
    end;
    end;
  // read token
  TokenStart:=Src;
  c1:=Src^;
  case c1 of
  #0:
    ;
  'A'..'Z','a'..'z','_':
    begin
    // identifier
    inc(Src);
    while Src^ in IdentChars do
      inc(Src);
    end;
  '0'..'9': // number
    begin
    inc(Src);
    // read numbers
    while (Src^ in ['0'..'9']) do
      inc(Src);
    if (Src^='.') and (Src[1]<>'.') then
      begin
      // real type number
      inc(Src);
      while (Src^ in ['0'..'9']) do
        inc(Src);
      end;
    if (Src^ in ['e','E']) then
      begin
      // read exponent
      inc(Src);
      if (Src^='-') then inc(Src);
      while (Src^ in ['0'..'9']) do
        inc(Src);
      end;
    end;
  '''','#','`':  // TPasScannerString constant
    while true do
      case Src^ of
      #0: break;
      '#':
        begin
        inc(Src);
        while Src^ in ['0'..'9'] do
          inc(Src);
        end;
      '''':
        begin
        inc(Src);
        while not (Src^ in ['''',#0,#10,#13]) do
          inc(Src);
        if Src^='''' then
          inc(Src);
        end;
      '`':
        begin
        inc(Src);
        while not (Src^ in ['`',#0]) do
          inc(Src);
        if Src^='''' then
          inc(Src);
        end;
      else
        break;
      end;
  '$':  // hex constant
    begin
    inc(Src);
    while Src^ in HexNumberChars do
      inc(Src);
    end;
  '&':  // octal constant or keyword as identifier (e.g. &label)
    begin
    inc(Src);
    if Src^ in ['0'..'7'] then
      while Src^ in ['0'..'7'] do
        inc(Src)
    else
      while Src^ in IdentChars do
        inc(Src);
    end;
  '{':  // compiler directive (it can't be a comment, because see above)
    begin
    CommentLvl:=1;
    while true do
      begin
      inc(Src);
      case Src^ of
      #0: break;
      '{':
        if NestedComments then
          inc(CommentLvl);
      '}':
        begin
        dec(CommentLvl);
        if CommentLvl=0 then
          begin
          inc(Src);
          break;
          end;
        end;
      end;
      end;
    end;
  '(':  // bracket or compiler directive
    if (Src[1]='*') then
      begin
      // compiler directive -> read til comment end
      inc(Src,2);
      while (Src^<>#0) and ((Src^<>'*') or (Src[1]<>')')) do
        inc(Src);
      inc(Src,2);
      end
    else
      // round bracket open
      inc(Src);
  #192..#255:
    begin
    // read UTF8 character
    inc(Src);
    if ((ord(c1) and %11100000) = %11000000) then
      begin
      // could be 2 byte character
      if (ord(Src[0]) and %11000000) = %10000000 then
        inc(Src);
      end
    else if ((ord(c1) and %11110000) = %11100000) then
      begin
      // could be 3 byte character
      if ((ord(Src[0]) and %11000000) = %10000000)
      and ((ord(Src[1]) and %11000000) = %10000000) then
        inc(Src,2);
      end
    else if ((ord(c1) and %11111000) = %11110000) then
      begin
      // could be 4 byte character
      if ((ord(Src[0]) and %11000000) = %10000000)
      and ((ord(Src[1]) and %11000000) = %10000000)
      and ((ord(Src[2]) and %11000000) = %10000000) then
        inc(Src,3);
      end;
    end;
  else
    inc(Src);
    case c1 of
    '<': if Src^ in ['>','='] then inc(Src);
    '.': if Src^='.' then inc(Src);
    '@':
      if Src^='@' then
        begin
        // @@ label
        repeat
          inc(Src);
        until not (Src^ in IdentChars);
        end
    else
      if (Src^='=') and (c1 in [':','+','-','/','*','<','>']) then
        inc(Src);
    end;
  end;
  Position:=Src;
end;
{$ENDIF}

type
  TIncludeStackItem = class
    SourceFile: TLineReader;
    Filename: String;
    Token: TToken;
    TokenString: TPasScannerString;
    Line: TPasScannerString;
    Row: Integer;
    ColumnOffset: integer;
    TokenPos: {$ifdef UsePChar}PAnsiChar;{$else}integer; { position in Line }{$endif}
  end;

function StrToModeSwitch(aName: TPasScannerString): TModeSwitch;
var
  ms: TModeSwitch;
begin
  aName:=UpperCase(aName);
  if aName='' then exit(msNone);
  for ms in TModeSwitch do
    if SModeSwitchNames[ms]=aName then exit(ms);
  Result:=msNone;
end;

function ModeSwitchesToStr(Switches: TModeSwitches): TPasScannerString;
var
  ms: TModeSwitch;
begin
  Result:='';
  for ms in Switches do
    Result:=Result+SModeSwitchNames[ms]+',';
  Result:='['+LeftStr(Result,length(Result)-1)+']';
end;

function BoolSwitchesToStr(Switches: TBoolSwitches): TPasScannerString;
var
  bs: TBoolSwitch;
begin
  Result:='';
  for bs in Switches do
    Result:=Result+BoolSwitchNames[bs]+',';
  Result:='['+LeftStr(Result,length(Result)-1)+']';
end;

function FilenameIsAbsolute(const TheFilename: String):boolean;
begin
  {$IFDEF WINDOWS}
  // windows
  Result:=FilenameIsWinAbsolute(TheFilename);
  {$ELSE}
  // unix
  Result:=FilenameIsUnixAbsolute(TheFilename);
  {$ENDIF}
end;

function FilenameIsWinAbsolute(const TheFilename: String): boolean;
begin
  Result:=((length(TheFilename)>=2) and (TheFilename[1] in ['A'..'Z','a'..'z'])
           and (TheFilename[2]=':'))
     or ((length(TheFilename)>=2)
         and (TheFilename[1]='\') and (TheFilename[2]='\'));
end;

function FilenameIsUnixAbsolute(const TheFilename: String): boolean;
begin
  Result:=(TheFilename<>'') and (TheFilename[1]='/');
end;

{ TCondDirectiveEvaluator }

// inline
function TCondDirectiveEvaluator.IsFalse(const Value: TPasScannerString): boolean;
begin
  Result:=Value=CondDirectiveBool[false];
  if (not Result) and isMac then
    Result:=Value=MacDirectiveBool[false];
end;

// inline
function TCondDirectiveEvaluator.IsTrue(const Value: TPasScannerString): boolean;
begin
  Result:=Value<>CondDirectiveBool[false];
  if Result and isMac then
    Result:=Value<>MacDirectiveBool[False];
end;

function TCondDirectiveEvaluator.IsInteger(const Value: TPasScannerString; out i: TMaxPrecInt
  ): boolean;
var
  Code: integer;
begin
  val(Value,i,Code);
  Result:=Code=0;
end;

function TCondDirectiveEvaluator.IsExtended(const Value: TPasScannerString; out e: TMaxFloat
  ): boolean;
var
  Code: integer;
begin
  val(Value,e,Code);
  Result:=Code=0;
end;

procedure TCondDirectiveEvaluator.NextToken;
const
  IdentChars = ['a'..'z','A'..'Z','_','0'..'9'];

  {$ifdef UsePChar}
  function IsIdentifier(a,b: PAnsiChar): boolean;
  var
    ac: AnsiChar;
  begin
    repeat
      ac:=a^;
      if (ac in IdentChars) and (upcase(ac)=upcase(b^)) then
        begin
        inc(a);
        inc(b);
        end
      else
        begin
        Result:=(not (ac in IdentChars)) and (not (b^ in IdentChars));
        exit;
        end;
    until false;
  end;
  {$endif}

  function ReadIdentifier: TToken;
  begin
    Result:=tkIdentifier;
    {$ifdef UsePChar}
    case FTokenEnd-FTokenStart of
    2:
      if IsIdentifier(FTokenStart,'or') then
        Result:=tkor;
    3:
      if IsIdentifier(FTokenStart,'not') then
        Result:=tknot
      else if IsIdentifier(FTokenStart,'and') then
        Result:=tkand
      else if IsIdentifier(FTokenStart,'xor') then
        Result:=tkxor
      else if IsIdentifier(FTokenStart,'shl') then
        Result:=tkshl
      else if IsIdentifier(FTokenStart,'shr') then
        Result:=tkshr
      else if IsIdentifier(FTokenStart,'mod') then
        Result:=tkmod
      else if IsIdentifier(FTokenStart,'div') then
        Result:=tkdiv;
    end;
    {$else}
    case lowercase(copy(Expression,FTokenStart,FTokenEnd-FTokenStart)) of
    'or': Result:=tkor;
    'not': Result:=tknot;
    'and': Result:=tkand;
    'xor': Result:=tkxor;
    'shl': Result:=tkshl;
    'shr': Result:=tkshr;
    'mod': Result:=tkmod;
    'div': Result:=tkdiv;
    end;
    {$endif}
  end;

{$ifndef UsePChar}
const
  AllSpaces = [#9,#10,#13,' '];
  Digits = ['0'..'9'];
  HexDigits = ['0'..'9'];
var
  l: integer;
  Src: TPasScannerString;
{$endif}
begin
  FTokenStart:=FTokenEnd;

  // skip white space
  {$ifdef UsePChar}
  repeat
    case FTokenStart^ of
      #0:
      if FTokenStart-PAnsiChar(Expression)>=length(Expression) then
        begin
        FToken:=tkEOF;
        FTokenEnd:=FTokenStart;
        exit;
        end
      else
        inc(FTokenStart);
      #9,#10,#13,' ':
        inc(FTokenStart);
      else break;
    end;
  until false;
  {$else}
  Src:=Expression;
  l:=length(Src);
  while (FTokenStart<=l) and (Src[FTokenStart] in AllSpaces) do
    inc(FTokenStart);
  if FTokenStart>l then
    begin
    FToken:=tkEOF;
    FTokenEnd:=FTokenStart;
    exit;
    end;
  {$endif}

  // read token
  FTokenEnd:=FTokenStart;
  case {$ifdef UsePChar}FTokenEnd^{$else}Src[FTokenEnd]{$endif} of
  'a'..'z','A'..'Z','_':
    begin
    inc(FTokenEnd);
    {$ifdef UsePChar}
    while FTokenEnd^ in IdentChars do inc(FTokenEnd);
    {$else}
    while (FTokenEnd<=l) and (Src[FTokenEnd] in IdentChars) do inc(FTokenEnd);
    {$endif}
    FToken:=ReadIdentifier;
    end;
  '0'..'9':
    begin
    FToken:=tkNumber;
    // examples: 1, 1.2, 1.2E3, 1E-2
    inc(FTokenEnd);
    {$ifdef UsePChar}
    while FTokenEnd^ in Digits do inc(FTokenEnd);
    if (FTokenEnd^='.') and (FTokenEnd[1]<>'.') then
      begin
      inc(FTokenEnd);
      while FTokenEnd^ in Digits do inc(FTokenEnd);
      end;
    if FTokenEnd^ in ['e','E'] then
      begin
      inc(FTokenEnd);
      if FTokenEnd^ in ['-','+'] then inc(FTokenEnd);
      while FTokenEnd^ in Digits do inc(FTokenEnd);
      end;
    {$else}
    while (FTokenEnd<=l) and (Src[FTokenEnd] in Digits) do inc(FTokenEnd);
    if (FTokenEnd<=l) and (Src[FTokenEnd]='.')
        and ((FTokenEnd=l) or (Src[FTokenEnd+1]<>'.')) then
      begin
      inc(FTokenEnd);
      while (FTokenEnd<=l) and (Src[FTokenEnd] in Digits) do inc(FTokenEnd);
      end;
    if (FTokenEnd<=l) and (Src[FTokenEnd] in ['e','E']) then
      begin
      inc(FTokenEnd);
      if (FTokenEnd<=l) and (Src[FTokenEnd] in ['-','+']) then inc(FTokenEnd);
      while (FTokenEnd<=l) and (Src[FTokenEnd] in Digits) do inc(FTokenEnd);
      end;
    {$endif}
    end;
  '$':
    begin
    FToken:=tkNumber;
    inc(FTokenEnd);
    {$ifdef UsePChar}
    while FTokenEnd^ in HexDigits do inc(FTokenEnd);
    {$else}
    while (FTokenEnd<=l) and (Src[FTokenEnd] in HexDigits) do inc(FTokenEnd);
    {$endif}
    end;
  '%':
    begin
    FToken:=tkNumber;
    {$ifdef UsePChar}
    while FTokenEnd^ in ['0','1'] do inc(FTokenEnd);
    {$else}
    while (FTokenEnd<=l) and (Src[FTokenEnd] in ['0','1']) do inc(FTokenEnd);
    {$endif}
    end;
  '(':
    begin
    FToken:=tkBraceOpen;
    inc(FTokenEnd);
    end;
  ')':
    begin
    FToken:=tkBraceClose;
    inc(FTokenEnd);
    end;
  '=':
    begin
    FToken:=tkEqual;
    inc(FTokenEnd);
    end;
  '<':
    begin
    inc(FTokenEnd);
    case {$ifdef UsePChar}FTokenEnd^{$else}copy(Src,FTokenEnd,1){$endif} of
    '=':
      begin
      FToken:=tkLessEqualThan;
      inc(FTokenEnd);
      end;
    '<':
      begin
      FToken:=tkshl;
      inc(FTokenEnd);
      end;
    '>':
      begin
      FToken:=tkNotEqual;
      inc(FTokenEnd);
      end;
    else
      FToken:=tkLessThan;
    end;
    end;
  '>':
    begin
    inc(FTokenEnd);
    case {$ifdef UsePChar}FTokenEnd^{$else}copy(Src,FTokenEnd,1){$endif} of
    '=':
      begin
      FToken:=tkGreaterEqualThan;
      inc(FTokenEnd);
      end;
    '>':
      begin
      FToken:=tkshr;
      inc(FTokenEnd);
      end;
    else
      FToken:=tkGreaterThan;
    end;
    end;
  '+':
    begin
    FToken:=tkPlus;
    inc(FTokenEnd);
    end;
  '-':
    begin
    FToken:=tkMinus;
    inc(FTokenEnd);
    end;
  '*':
    begin
    FToken:=tkMul;
    inc(FTokenEnd);
    end;
  '/':
    begin
    FToken:=tkDivision;
    inc(FTokenEnd);
    end;
  '''':
    begin
    FToken:=tkString;
    repeat
      inc(FTokenEnd);
      {$ifdef UsePChar}
      if FTokenEnd^='''' then
        begin
        inc(FTokenEnd);
        if FTokenEnd^<>'''' then break;
        end
      else if FTokenEnd^ in [#0,#10,#13] then
        Log(mtError,nErrOpenString,SErrOpenString,[]);
      {$else}
      if FTokenEnd>l then
        Log(mtError,nErrOpenString,SErrOpenString,[]);
      case Src[FTokenEnd] of
      '''':
        begin
        inc(FTokenEnd);
        if (FTokenEnd>l) or (Src[FTokenEnd]<>'''') then break;
        end;
      #10,#13:
        Log(mtError,nErrOpenString,SErrOpenString,[]);
      end;
      {$endif}
    until false;
    end
  else
    FToken:=tkEOF;
  end;
  {$IFDEF VerbosePasDirectiveEval}
  writeln('TCondDirectiveEvaluator.NextToken END Token[',FTokenStart-PAnsiChar(Expression)+1,']="',GetTokenString,'" ',FToken);
  {$ENDIF}
end;

procedure TCondDirectiveEvaluator.Log(aMsgType: TMessageType;
  aMsgNumber: integer; const aMsgFmt: String;
  const Args: array of const;
  MsgPos: integer);
begin
  if MsgPos<1 then
    MsgPos:=FTokenEnd{$ifdef UsePChar}-PAnsiChar(Expression)+1{$endif};
  MsgType:=aMsgType;
  MsgNumber:=aMsgNumber;
  MsgPattern:=aMsgFmt;
  if Assigned(OnLog) then
    begin
    OnLog(Self,Args);
    if not (aMsgType in [mtError,mtFatal]) then exit;
    end;
  raise EScannerError.CreateFmt(MsgPattern+' at pos '+IntToStr(MsgPos)+' line '+IntToStr(MsgCurLine),Args);
end;

procedure TCondDirectiveEvaluator.LogXExpectedButTokenFound(const X: TPasScannerString;
  ErrorPos: integer);

Var
  S : String;

begin
  S:=X;
  Log(mtError,nErrXExpectedButYFound,SErrXExpectedButYFound,
      [S,TokenInfos[FToken]],ErrorPos);
end;

procedure TCondDirectiveEvaluator.ReadOperand(Skip: boolean);
{ Read operand and put it on the stack
  Examples:
   Variable
   not Variable
   not not undefined Variable
   defined(Variable)
   !Variable
   unicodestring
   123
   $45
   'Abc'
   (expression)
}

  Function IsMacNoArgFunction(aName : String) : Boolean;
  begin
    Result:=SameText(aName,'DEFINED') or SameText(aName,'UNDEFINED');
  end;

var
  i: TMaxPrecInt;
  e: TMaxFloat;
  S, aName, Param: String;
  Code: integer;
  NameStartP: {$ifdef UsePChar}PAnsiChar{$else}integer{$endif};
  p, Lvl: integer;

begin
  {$IFDEF VerbosePasDirectiveEval}
  writeln('TCondDirectiveEvaluator.ReadOperand START Token[',FTokenStart-PAnsiChar(Expression)+1,']="',GetTokenString,'" ',FToken,BoolToStr(Skip,' SKIP',''));
  {$ENDIF}
  case FToken of
    tknot:
      begin
      // boolean not
      NextToken;
      ReadOperand(Skip);
      if not Skip then
        FStack[FStackTop].Operand:=CondDirectiveBool[IsFalse(FStack[FStackTop].Operand)];
      end;
    tkMinus:
      begin
      // unary minus
      NextToken;
      ReadOperand(Skip);
      if not Skip then
        begin
        i:=StrToInt64Def(FStack[FStackTop].Operand,0);
        FStack[FStackTop].Operand:=IntToStr(-i);
        end;
      end;
    tkPlus:
      begin
      // unary plus
      NextToken;
      ReadOperand(Skip);
      if not Skip then
        begin
        i:=StrToInt64Def(FStack[FStackTop].Operand,0);
        FStack[FStackTop].Operand:=IntToStr(i);
        end;
      end;
    tkNumber:
      begin
      // number: convert to decimal
      if not Skip then
        begin
        S:=GetTokenString;
        val(S,i,Code);
        if Code=0 then
          begin
          // integer
          Push(IntToStr(i),FTokenStart{$ifdef UsePChar}-PAnsiChar(Expression)+1{$endif});
          end
        else
          begin
          val(S,e,Code);
          if Code>0 then
            Log(mtError,nErrRangeCheck,sErrRangeCheck,[]);
          if e=0 then ;
          // float
          Push(S,FTokenStart{$ifdef UsePChar}-PAnsiChar(Expression)+1{$endif});
          end;
        end;
      NextToken;
      end;
    tkString:
      begin
      // TPasScannerString literal
      if not Skip then
        Push(GetStringLiteralValue,FTokenStart{$ifdef UsePChar}-PAnsiChar(Expression)+1{$endif});
      NextToken;
      end;
    tkIdentifier:
      if Skip then
        begin
        aName:=GetTokenString;
        NextToken;
        // for macpas IFC we can have DEFINED A or DEFINED(A)...
        if FToken=tkBraceOpen then
          begin
          // only one parameter is supported
          NextToken;
          if FToken=tkIdentifier then
            NextToken;
          if FToken<>tkBraceClose then
            LogXExpectedButTokenFound(')');
          NextToken;
          end
        else if (IsMac and IsMacNoArgFunction(aName)) then
          begin
          NextToken;
          end;
        end
      else
        begin
        aName:=GetTokenString;
        p:=FTokenStart{$ifdef UsePChar}-PAnsiChar(Expression)+1{$endif};
        NextToken;
        if FToken=tkBraceOpen then
          begin
          // function
          NameStartP:=FTokenStart;
          NextToken;
          // only one parameter is supported
          Param:='';
          if FToken=tkIdentifier then
            begin
            Param:=GetTokenString;
            NextToken;
            end;
          if FToken<>tkBraceClose then
            LogXExpectedButTokenFound(')');
          if not OnEvalFunction(Self,aName,Param,S) then
            begin
            FTokenStart:=NameStartP;
            FTokenEnd:=FTokenStart+length(aName);
            LogXExpectedButTokenFound('function');
            end;
          Push(S,p);
          NextToken;
          end
        else if (IsMac and IsMacNoArgFunction(aName)) then
          begin
          if FToken<>tkIdentifier then
            LogXExpectedButTokenFound('identifier');
          aName:=GetTokenString;
          Push(CondDirectiveBool[OnEvalVariable(Self,aName,S)],p);
          NextToken;
          end
        else
          begin
          // variable
          if OnEvalVariable(Self,aName,S) then
            Push(S,p)
          else
            begin
            // variable does not exist -> evaluates to false
            Push(CondDirectiveBool[false],p);
            end;
          end;
        end;
    tkBraceOpen:
      begin
      NextToken;
      if Skip then
        begin
        Lvl:=1;
        repeat
          case FToken of
          tkEOF:
            LogXExpectedButTokenFound(')');
          tkBraceOpen: inc(Lvl);
          tkBraceClose:
            begin
            dec(Lvl);
            if Lvl=0 then break;
            end;
          else
            // Do nothing, satisfy compiler
          end;
          NextToken;
        until false;
        end
      else
        begin
        ReadExpression;
        if FToken<>tkBraceClose then
          LogXExpectedButTokenFound(')');
        end;
      NextToken;
      end;
  else
    LogXExpectedButTokenFound('identifier');
  end;
  {$IFDEF VerbosePasDirectiveEval}
  writeln('TCondDirectiveEvaluator.ReadOperand END Top=',FStackTop,' Value="',FStack[FStackTop].Operand,'" Token[',FTokenStart-PAnsiChar(Expression)+1,']="',GetTokenString,'" ',FToken);
  {$ENDIF}
end;

procedure TCondDirectiveEvaluator.ReadExpression;
// read operand operator operand ... til tkEOF or tkBraceClose
var
  OldStackTop: Integer;

  procedure ReadBinary(Level: TPrecedenceLevel; NewOperator: TToken);
  begin
    ResolveStack(OldStackTop,Level,NewOperator);
    NextToken;
    ReadOperand;
  end;

begin
  OldStackTop:=FStackTop;
  {$IFDEF VerbosePasDirectiveEval}
  writeln('TCondDirectiveEvaluator.ReadExpression START Top=',FStackTop,' Token[',FTokenStart-PAnsiChar(Expression)+1,']="',GetTokenString,'" ',FToken);
  {$ENDIF}
  ReadOperand;
  repeat
    {$IFDEF VerbosePasDirectiveEval}
    writeln('TCondDirectiveEvaluator.ReadExpression NEXT Top=',FStackTop,' Token[',FTokenStart-PAnsiChar(Expression)+1,']="',GetTokenString,'" ',FToken);
    {$ENDIF}
    case FToken of
    tkEOF,tkBraceClose:
      begin
      ResolveStack(OldStackTop,high(TPrecedenceLevel),tkEOF);
      exit;
      end;
    tkand:
      begin
      ResolveStack(OldStackTop,ceplSecond,tkand);
      NextToken;
      if (FStackTop=OldStackTop+1) and IsFalse(FStack[FStackTop].Operand) then
        begin
        // false and ...
        // -> skip all "and"
        repeat
          ReadOperand(true);
          if FToken<>tkand then break;
          NextToken;
        until false;
        FStack[FStackTop].Operathor:=tkEOF;
        end
      else
        ReadOperand;
      end;
    tkMul,tkDivision,tkdiv,tkmod,tkshl,tkshr:
      ReadBinary(ceplSecond,FToken);
    tkor:
      begin
      ResolveStack(OldStackTop,ceplThird,tkor);
      NextToken;
      if (FStackTop=OldStackTop+1) and IsTrue(FStack[FStackTop].Operand) then
        begin
        // true or ...
        // -> skip all "and" and "or"
        repeat
          ReadOperand(true);
          if not (FToken in [tkand,tkor]) then break;
          NextToken;
        until false;
        FStack[FStackTop].Operathor:=tkEOF;
        end
      else
        ReadOperand;
      end;
    tkPlus,tkMinus,tkxor:
      ReadBinary(ceplThird,FToken);
    tkEqual,tkNotEqual,tkLessThan,tkGreaterThan,tkLessEqualThan,tkGreaterEqualThan:
      ReadBinary(ceplFourth,FToken);
    else
      LogXExpectedButTokenFound('operator');
    end;
  until false;
  {$IFDEF VerbosePasDirectiveEval}
  writeln('TCondDirectiveEvaluator.ReadExpression END Top=',FStackTop,' Value="',FStack[FStackTop].Operand,'" Token[',FTokenStart-PAnsiChar(Expression)+1,']=',GetTokenString,' ',FToken);
  {$ENDIF}
end;

procedure TCondDirectiveEvaluator.ResolveStack(MinStackLvl: integer;
  Level: TPrecedenceLevel; NewOperator: TToken);
var
  A, B, R: TPasScannerString;
  Op: TToken;
  AInt, BInt: TMaxPrecInt;
  AFloat, BFloat: TMaxFloat;
  BPos: Integer;
begin
  // resolve all higher or equal level operations
  // Note: the stack top contains operand B
  //       the stack second contains operand A and the operator between A and B

  //writeln('TCondDirectiveEvaluator.ResolveStack FStackTop=',FStackTop,' MinStackLvl=',MinStackLvl);
  //if FStackTop>MinStackLvl+1 then
  //  writeln('  FStack[FStackTop-1].Level=',FStack[FStackTop-1].Level,' Level=',Level);
  while (FStackTop>MinStackLvl+1) and (FStack[FStackTop-1].Level<=Level) do
    begin
    // pop last operand and operator from stack
    B:=FStack[FStackTop].Operand;
    BPos:=FStack[FStackTop].OperandPos;
    dec(FStackTop);
    Op:=FStack[FStackTop].Operathor;
    A:=FStack[FStackTop].Operand;
    {$IFDEF VerbosePasDirectiveEval}
    writeln('  ResolveStack Top=',FStackTop,' A="',A,'" ',Op,' B="',B,'"');
    {$ENDIF}
    {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
    {$R+}
    try
      case Op of
      tkand: // boolean and
        R:=CondDirectiveBool[IsTrue(A) and IsTrue(B)];
      tkor: // boolean or
        R:=CondDirectiveBool[IsTrue(A) or IsTrue(B)];
      tkxor: // boolean xor
        R:=CondDirectiveBool[IsTrue(A) xor IsTrue(B)];
      tkMul, tkdiv, tkmod, tkshl, tkshr, tkPlus, tkMinus:
        if IsInteger(A,AInt) then
          begin
          if IsInteger(B,BInt) then
            case Op of
              tkMul: R:=IntToStr(AInt*BInt);
              tkdiv: R:=IntToStr(AInt div BInt);
              tkmod: R:=IntToStr(AInt mod BInt);
              tkshl: R:=IntToStr(AInt shl BInt);
              tkshr: R:=IntToStr(AInt shr BInt);
              tkPlus: R:=IntToStr(AInt+BInt);
              tkMinus: R:=IntToStr(AInt-BInt);
            else
              // Do nothing, satisfy compiler
            end
          else if IsExtended(B,BFloat) then
            case Op of
              tkMul: R:=FloatToStr(Extended(AInt)*BFloat);
              tkPlus: R:=FloatToStr(Extended(AInt)+BFloat);
              tkMinus: R:=FloatToStr(Extended(AInt)-BFloat);
            else
              LogXExpectedButTokenFound('integer',BPos);
            end
          else
            LogXExpectedButTokenFound('integer',BPos);
          end
        else if IsExtended(A,AFloat) then
          begin
          if IsExtended(B,BFloat) then
            case Op of
              tkMul: R:=FloatToStr(AFloat*BFloat);
              tkPlus: R:=FloatToStr(AFloat+BFloat);
              tkMinus: R:=FloatToStr(AFloat-BFloat);
            else
              LogXExpectedButTokenFound('float',BPos);
            end
          else
            LogXExpectedButTokenFound('float',BPos);
          end
        else
          Log(mtError,nErrOperandAndOperatorMismatch,sErrOperandAndOperatorMismatch,[]);
      tkDivision:
        if IsExtended(A,AFloat) then
          begin
          if IsExtended(B,BFloat) then
            R:=FloatToStr(AFloat/BFloat)
          else
            LogXExpectedButTokenFound('float',BPos);
          end
        else
          Log(mtError,nErrOperandAndOperatorMismatch,sErrOperandAndOperatorMismatch,[]);
      tkEqual,
      tkNotEqual,
      tkLessThan,tkGreaterThan,
      tkLessEqualThan,tkGreaterEqualThan:
        begin
        if IsInteger(A,AInt) and IsInteger(B,BInt) then
          case Op of
          tkEqual: R:=CondDirectiveBool[AInt=BInt];
          tkNotEqual: R:=CondDirectiveBool[AInt<>BInt];
          tkLessThan: R:=CondDirectiveBool[AInt<BInt];
          tkGreaterThan: R:=CondDirectiveBool[AInt>BInt];
          tkLessEqualThan: R:=CondDirectiveBool[AInt<=BInt];
          tkGreaterEqualThan: R:=CondDirectiveBool[AInt>=BInt];
          else
          // Do nothing, satisfy compiler
          end
        else if IsExtended(A,AFloat) and IsExtended(B,BFloat) then
          case Op of
          tkEqual: R:=CondDirectiveBool[AFloat=BFloat];
          tkNotEqual: R:=CondDirectiveBool[AFloat<>BFloat];
          tkLessThan: R:=CondDirectiveBool[AFloat<BFloat];
          tkGreaterThan: R:=CondDirectiveBool[AFloat>BFloat];
          tkLessEqualThan: R:=CondDirectiveBool[AFloat<=BFloat];
          tkGreaterEqualThan: R:=CondDirectiveBool[AFloat>=BFloat];
          else
          // Do nothing, satisfy compiler
          end
        else
          case Op of
          tkEqual: R:=CondDirectiveBool[A=B];
          tkNotEqual: R:=CondDirectiveBool[A<>B];
          tkLessThan: R:=CondDirectiveBool[A<B];
          tkGreaterThan: R:=CondDirectiveBool[A>B];
          tkLessEqualThan: R:=CondDirectiveBool[A<=B];
          tkGreaterEqualThan: R:=CondDirectiveBool[A>=B];
          else
          // Do nothing, satisfy compiler
          end;
        end;
      else
        Log(mtError,nErrOperandAndOperatorMismatch,sErrOperandAndOperatorMismatch,[]);
      end;
    except
      on E: EDivByZero do
        Log(mtError,nErrDivByZero,sErrDivByZero,[]);
      on E: EZeroDivide do
        Log(mtError,nErrDivByZero,sErrDivByZero,[]);
      on E: EMathError do
        Log(mtError,nErrRangeCheck,sErrRangeCheck+' '+E.Message,[]);
      on E: EInterror do
        Log(mtError,nErrRangeCheck,sErrRangeCheck+' '+E.Message,[]);
    end;
    {$IFNDEF RangeChecking}{$R-}{$UNDEF RangeChecking}{$ENDIF}
    {$IFDEF VerbosePasDirectiveEval}
    writeln('  ResolveStack Top=',FStackTop,' A="',A,'" ',Op,' B="',B,'" = "',R,'"');
    {$ENDIF}
    FStack[FStackTop].Operand:=R;
    FStack[FStackTop].OperandPos:=BPos;
    end;
  FStack[FStackTop].Operathor:=NewOperator;
  FStack[FStackTop].Level:=Level;
end;

function TCondDirectiveEvaluator.GetTokenString: TPasScannerString;
begin
  Result:=copy(Expression,FTokenStart{$ifdef UsePChar}-PAnsiChar(Expression)+1{$endif},
               FTokenEnd-FTokenStart);
end;

function TCondDirectiveEvaluator.GetStringLiteralValue: TPasScannerString;
var
  {$ifdef UsePChar}
  p, StartP: PAnsiChar;
  {$else}
  Src: TPasScannerString;
  p, l, StartP: Integer;
  c: AnsiChar;
  {$endif}
begin
  Result:='';
  p:=FTokenStart;
  {$ifdef UsePChar}
  repeat
    case p^ of
    '''':
      begin
      inc(p);
      StartP:=p;
      repeat
        case p^ of
        #0,#10,#13: Log(mtError,nErrInvalidCharacter,SErrInvalidCharacter,['#0']);
        '''': break;
        else inc(p);
        end;
      until false;
      if p>StartP then
        Result:=Result+copy(Expression,StartP-PAnsiChar(Expression)+1,p-StartP);
      inc(p);
      end;
    '`':
      begin
      inc(p);
      StartP:=p;
      repeat
        case p^ of
        #0: Log(mtError,nErrInvalidCharacter,SErrInvalidCharacter,['#0']);
        '`': break;
        else inc(p);
        end;
      until false;
      if p>StartP then
        Result:=Result+copy(Expression,StartP-PAnsiChar(Expression)+1,p-StartP);
      inc(p);
      end;
    else
      Log(mtError,nErrInvalidCharacter,SErrInvalidCharacter,['#0']);
    end;
  until false;
  {$else}
  Src:=Expression;
  l:=length(Src);
  repeat
    if (p>l) or not (Src[p] in ['''','`']) then
      Log(mtError,nErrInvalidCharacter,SErrInvalidCharacter,['#0'])
    else
      begin
      c:=Src[p];
      inc(p);
      StartP:=p;
      repeat
        if (p>l) then
          Log(mtError,nErrInvalidCharacter,SErrInvalidCharacter,['#0'])
        else if Src[p]=c then
          break
        else if (c='''') and (Src[p] in [#10,#13]) then
          Log(mtError,nErrInvalidCharacter,SErrInvalidCharacter,['#'+IntToStr(ord(Src[p]))])
        else
          inc(p);
      until false;
      if p>StartP then
        Result:=Result+copy(Expression,StartP,p-StartP);
      inc(p);
      end;
  until false;
  {$endif}
end;

procedure TCondDirectiveEvaluator.Push(const AnOperand: TPasScannerString;
  OperandPosition: integer);
begin
  inc(FStackTop);
  if FStackTop>=length(FStack) then
    SetLength(FStack,length(FStack)*2+4);
  with FStack[FStackTop] do
    begin
    Operand:=AnOperand;
    OperandPos:=OperandPosition;
    Operathor:=tkEOF;
    Level:=ceplFourth;
    end;
  {$IFDEF VerbosePasDirectiveEval}
  writeln('TCondDirectiveEvaluator.Push Top=',FStackTop,' Operand="',AnOperand,'" Pos=',OperandPosition);
  {$ENDIF}
end;

constructor TCondDirectiveEvaluator.Create(aIsMac: Boolean);
begin
  IsMac:=aIsMac
end;

destructor TCondDirectiveEvaluator.Destroy;
begin
  inherited Destroy;
end;

function TCondDirectiveEvaluator.Eval(const Expr: TPasScannerString): boolean;
begin
  {$IFDEF VerbosePasDirectiveEval}
  writeln('TCondDirectiveEvaluator.Eval Expr="',Expr,'"');
  {$ENDIF}
  Expression:=Expr;
  MsgType:=mtInfo;
  MsgNumber:=0;
  MsgPattern:='';
  if Expr='' then exit(false);
  FTokenStart:={$ifdef UsePChar}PAnsiChar(Expr){$else}1{$endif};
  FTokenEnd:=FTokenStart;
  FStackTop:=-1;
  NextToken;
  ReadExpression;
  Result:=IsTrue(FStack[0].Operand);
  {$IFDEF VerbosePasDirectiveEval}
  Writeln('COND Eval: ', Expr,' -> ',Result);
  {$ENDIF}
end;

{ TMacroDef }

constructor TMacroDef.Create(const AName, AValue: TPasTreeString);
begin
  FName:=AName;
  FValue:=AValue;
end;

{ TLineReader }

constructor TLineReader.Create(const AFilename: String);
begin
  FFileName:=AFileName;
  if LineEnding=#13 then
    {%H-}EOLStyle:=elCR
  else if LineEnding=#13#10 then
    {%H-}EOLStyle:=elCRLF
   else
    EOLStyle:=elLF
end;

function TLineReader.LastEOLStyle: TEOLStyle;
begin
  Result:=EOLStyle;
end;

{ ---------------------------------------------------------------------
  TFileLineReader
  ---------------------------------------------------------------------}

constructor TFileLineReader.Create(const AFilename: String);

begin
  inherited Create(AFileName);
  {$ifdef pas2js}
  raise Exception.Create('ToDo TFileLineReader.Create');
  {$else}
  Assign(FTextFile, AFilename);
  Reset(FTextFile);
  SetTextBuf(FTextFile,FBuffer,SizeOf(FBuffer));
  FFileOpened := true;
  {$endif}
end;

destructor TFileLineReader.Destroy;
begin
  {$ifdef pas2js}
  // ToDo
  {$else}
  if FFileOpened then
    Close(FTextFile);
  {$endif}
  inherited Destroy;
end;

function TFileLineReader.IsEOF: Boolean;
begin
  {$ifdef pas2js}
  Result:=true;// ToDo
  {$else}
  Result := EOF(FTextFile);
  {$endif}
end;

function TFileLineReader.ReadLine: TPasScannerString;
begin
  {$ifdef pas2js}
  Result:='';// ToDo
  {$else}
  ReadLn(FTextFile, Result);
  {$endif}
end;

{ TStreamLineReader }


Procedure TStreamLineReader.InitFromStream(AStream : TStream);

{$IFDEF PAS2JS}
  function BufferToString(aBuffer: TBytes): String;

  var
    a : TJSUint16Array;
    i,len: Integer;

  begin
    Result:=''; // Silence warning
    len:=Length(aBuffer);
    a:=TJSUint16Array.New(Len);
    for I:=0 to Len-1 do
      a[i]:=aBuffer[i];
    if a<>nil then
      Result:=String(TJSFunction(@TJSString.fromCharCode).apply(nil,TJSValueDynArray(JSValue(a))));
  end;
{$ENDIF}

Var
  B : TBytes;

begin
  SetLength(B{%H-},AStream.Size);
  if Length(B)>0 then
    {$ifdef pas2js}
    AStream.Read(B,length(B));
    {$ELSE}
    AStream.Read(B[0],length(B));
    {$ENDIF}
  {$IFNDEF PAS2JS}
  FContent:=TEncoding.Default.GetAnsiString(B);
  {$ELSE}
  FContent:=BufferToString(B);
  {$ENDIF}

  FPos:=0;
end;


procedure TStreamLineReader.InitFromString(const s: TPasScannerString);
begin
{$IFDEF PAS2JS}
  FContent:=S;
{$ELSE}
{$IF SIZEOF(CHAR)=2}
  FContent:=UTF8Encode(s);
{$ELSE}
  FContent:=S;
{$ENDIF}
{$ENDIF}
  FPos:=0;
end;

function TStreamLineReader.IsEOF: Boolean;
begin
  Result:=FPos>=Length(FContent);
end;

function TStreamLineReader.ReadLine: TPasScannerString;

Var
  LPos : Integer;
  EOL : Boolean;

begin
  If isEOF then
    exit('');
  LPos:=FPos+1;
  Repeat
    Inc(FPos);
    EOL:=(FContent[FPos] in [#10,#13]);
  until isEOF or EOL;
  If EOL then
    begin
    if FContent[FPOS]=#10 then
      EOLSTYLE:=elLF
    else
      EOLStyle:=elCR;
    Result:=Copy(FContent,LPos,FPos-LPos)
    end
  else
    Result:=Copy(FContent,LPos,FPos-LPos+1);
  If (not isEOF) and (FContent[FPos]=#13) and (FContent[FPos+1]=#10) then
    begin
    inc(FPos);
    EOLStyle:=elCRLF;
    end;
end;

{ TFileStreamLineReader }

constructor TFileStreamLineReader.Create(const AFilename: String);
{$ifdef HasStreams}
Var
  S : TFileStream;
{$endif}
begin
  inherited Create(AFilename);
  {$ifdef HasStreams}
  S:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
     InitFromStream(S);
  finally
    S.Free;
  end;
  {$else}
  raise Exception.Create('TFileStreamLineReader.Create');
  {$endif}
end;

{ TStringStreamLineReader }

constructor TStringStreamLineReader.Create(const AFilename: String; const ASource: TPasScannerString);
begin
  inherited Create(AFilename);
  InitFromString(ASource);
end;

{ ---------------------------------------------------------------------
  TBaseFileResolver
  ---------------------------------------------------------------------}

procedure TBaseFileResolver.SetBaseDirectory(AValue: String);
begin
  AValue:=IncludeTrailingPathDelimiter(AValue);
  if FBaseDirectory=AValue then Exit;
  FBaseDirectory:=AValue;
end;

procedure TBaseFileResolver.SetModuleDirectory(AValue: String);
begin
  AValue:=IncludeTrailingPathDelimiter(AValue);
  if FModuleDirectory=AValue then Exit;
  FModuleDirectory:=AValue;
end;

procedure TBaseFileResolver.SetStrictFileCase(AValue: Boolean);
begin
  if FStrictFileCase=AValue then Exit;
  FStrictFileCase:=AValue;
end;

constructor TBaseFileResolver.Create;
begin
  inherited Create;
  FIncludePaths := TStringList.Create;
  FResourcePaths := TStringList.Create;
  FMode:=msFPC;
end;

destructor TBaseFileResolver.Destroy;
begin
  FResourcePaths.Free;
  FIncludePaths.Free;
  inherited Destroy;
end;

procedure TBaseFileResolver.AddIncludePath(const APath: String);

Var
  FP : TPasScannerString;

begin
  if (APath='') then
    FIncludePaths.Add('./')
  else
    begin
{$IFDEF HASFS}
    FP:=IncludeTrailingPathDelimiter(ExpandFileName(APath));
{$ELSE}
    FP:=APath;
{$ENDIF}
    FIncludePaths.Add(FP);
    end;
end;

procedure TBaseFileResolver.AddResourcePath(const APath: String);
Var
  FP : String;

begin
  if (APath='') then
    FResourcePaths.Add('./')
  else
    begin
{$IFDEF HASFS}
    FP:=IncludeTrailingPathDelimiter(ExpandFileName(APath));
{$ELSE}
    FP:=APath;
{$ENDIF}
    FResourcePaths.Add(FP);
    end;
end;


{$IFDEF HASFS}

{ ---------------------------------------------------------------------
  TFileResolver
  ---------------------------------------------------------------------}


function TFileResolver.SearchLowUpCase(FN: String): String;

var
  Dir: TPasScannerString;

begin
  If FileExists(FN) then
    Result:=FN
  else if StrictFileCase then
    Result:=''
  else
    begin
    Dir:=ExtractFilePath(FN);
    FN:=ExtractFileName(FN);
    Result:=Dir+LowerCase(FN);
    If FileExists(Result) then exit;
    Result:=Dir+uppercase(Fn);
    If FileExists(Result) then exit;
    Result:='';
    end;
end;

function TFileResolver.FindIncludeFileName(const AName: String): String;


  Function FindInPath(FN : String) : String;

  var
    I : integer;

  begin
    Result:='';
    // search in BaseDirectory (not in mode Delphi)
    if (BaseDirectory<>'')
        and ((ModuleDirectory='') or not (Mode in [msDelphi,msDelphiUnicode])) then
      begin
      Result:=SearchLowUpCase(BaseDirectory+FN);
      if Result<>'' then exit;
      end;
    // search in ModuleDirectory
    if (ModuleDirectory<>'') then
      begin
      Result:=SearchLowUpCase(ModuleDirectory+FN);
      if Result<>'' then exit;
      end;
    // search in include paths
    I:=0;
    While (I<FIncludePaths.Count) do
      begin
      Result:=SearchLowUpCase(FIncludePaths[i]+FN);
      if Result<>'' then exit;
      Inc(I);
      end;
  end;

var
  FN : TPasScannerString;

begin
  Result := '';
  // convert pathdelims to system
  FN:=SetDirSeparators(AName);
  If FilenameIsAbsolute(FN) then
    begin
    Result := SearchLowUpCase(FN);
    if (Result='') and (ExtractFileExt(FN)='') then
      begin
      Result:=SearchLowUpCase(FN+'.inc');
      if Result='' then
        begin
        Result:=SearchLowUpCase(FN+'.pp');
        if Result='' then
          Result:=SearchLowUpCase(FN+'.pas');
        end;
      end;
    end
  else
    begin
    // file name is relative
    // search in include path
    Result:=FindInPath(FN);
    // No extension, try default extensions
    if (Result='') and (ExtractFileExt(FN)='') then
      begin
      Result:=FindInPath(FN+'.inc');
      if Result='' then
        begin
        Result:=FindInPath(FN+'.pp');
        if Result='' then
          Result:=FindInPath(FN+'.pas');
        end;
      end;
    end;
end;

function TFileResolver.CreateFileReader(const AFileName: String): TLineReader;
begin
  {$ifdef HasStreams}
  If UseStreams then
    Result:=TFileStreamLineReader.Create(AFileName)
  else
  {$endif}
    Result:=TFileLineReader.Create(AFileName);
end;

function TFileResolver.FindResourceFileName(const AFileName: String): String;

  Function FindInPath(FN : String) : String;

  var
    I : integer;

  begin
    Result:='';
    I:=0;
    While (Result='') and (I<FResourcePaths.Count) do
      begin
      Result:=SearchLowUpCase(FResourcePaths[i]+FN);
      Inc(I);
      end;
    // search in BaseDirectory
    if (Result='') and (BaseDirectory<>'') then
      Result:=SearchLowUpCase(BaseDirectory+FN);
  end;

var
  FN : TPasScannerString;

begin
  Result := '';
  // convert pathdelims to system
  FN:=SetDirSeparators(AFileName);
  If FilenameIsAbsolute(FN) then
    begin
    Result := SearchLowUpCase(FN);
    end
  else
    begin
    // file name is relative
    // search in include path
    Result:=FindInPath(FN);
    end;
end;

function TFileResolver.FindSourceFile(const AName: String): TLineReader;
begin
  Result := nil;
  if not FileExists(AName) then
    Raise EFileNotFoundError.create(AName)
  else
    try
      Result := CreateFileReader(AName)
    except
      Result := nil;
    end;
end;

function TFileResolver.FindIncludeFile(const AName: String): TLineReader;

Var
  FN : String;

begin
  Result:=Nil;
  FN:=FindIncludeFileName(AName);
  If (FN<>'') then
    try
      Result := TFileLineReader.Create(FN);
    except
      Result:=Nil;
    end;
end;
{$ENDIF}

{ TStreamResolver }

procedure TStreamResolver.SetOwnsStreams(AValue: Boolean);
begin
  if FOwnsStreams=AValue then Exit;
  FOwnsStreams:=AValue;
end;

function TStreamResolver.FindIncludeFileName(const aFilename: String): String;
begin
  raise EFileNotFoundError.Create('TStreamResolver.FindIncludeFileName not supported '+aFilename);
  Result:='';
end;

function TStreamResolver.FindResourceFileName(const AFileName: String): String;
begin
  raise EFileNotFoundError.Create('TStreamResolver.FindResourceFileName not supported '+aFileName);
  Result:='';
end;

constructor TStreamResolver.Create;
begin
  Inherited;
  FStreams:=TStringList.Create;
  FStreams.Sorted:=True;
  FStreams.Duplicates:=dupError;
end;

destructor TStreamResolver.Destroy;
begin
  Clear;
  FreeAndNil(FStreams);
  inherited Destroy;
end;

procedure TStreamResolver.Clear;

Var
  I : integer;
  Obj : TObject;
begin
  if OwnsStreams then
    begin
    For I:=0 to FStreams.Count-1 do
      begin
      Obj:=Fstreams.Objects[i];
      Fstreams.Objects[i]:=nil;
      Obj.Free;
      end;
    end;
  FStreams.Clear;
end;

procedure TStreamResolver.AddStream(const AName: String; AStream: TStream);
begin
  FStreams.AddObject(AName,AStream);
end;

function TStreamResolver.FindStream(const AName: String; ScanIncludes : Boolean) : TStream;

Var
  I,J : Integer;
  FN : String;
begin
  Result:=Nil;
  I:=FStreams.IndexOf(AName);
  If (I=-1) and ScanIncludes then
    begin
    J:=0;
    While (I=-1) and (J<IncludePaths.Count-1) do
      begin
      FN:=IncludeTrailingPathDelimiter(IncludePaths[i])+AName;
      I:=FStreams.IndexOf(FN);
      Inc(J);
      end;
    end;
  if (I=-1) and (BaseDirectory<>'') then
    I:=FStreams.IndexOf(IncludeTrailingPathDelimiter(BaseDirectory)+aName);
  If (I<>-1) then
    Result:=FStreams.Objects[i] as TStream;
end;

function TStreamResolver.FindStreamReader(const AName: String; ScanIncludes : Boolean) : TLineReader;

Var
  S : TStream;
  SL : TStreamLineReader;

begin
  Result:=Nil;
  S:=FindStream(AName,ScanIncludes);
  If (S<>Nil) then
    begin
    S.Position:=0;
    SL:=TStreamLineReader.Create(AName);
    try
      SL.InitFromStream(S);
      Result:=SL;
    except
      FreeAndNil(SL);
      Raise;
    end;
    end;
end;

function TStreamResolver.FindSourceFile(const AName: String): TLineReader;

begin
  Result:=FindStreamReader(AName,False);
end;

function TStreamResolver.FindIncludeFile(const AName: String): TLineReader;
begin
  Result:=FindStreamReader(AName,True);
end;


{ ---------------------------------------------------------------------
  TPascalScanner
  ---------------------------------------------------------------------}

constructor TPascalScanner.Create(AFileResolver: TBaseFileResolver);

  Function CS : TStringList;

  begin
    Result:=TStringList.Create;
    Result.Sorted:=True;
    Result.Duplicates:=dupError;
  end;

var
  vs: TValueSwitch;
begin
  inherited Create;
  FFileResolver := AFileResolver;
  FFiles:=TStringList.Create;
  FIncludeStack := TFPList.Create;
  FDefines := CS;
  FMacros:=CS;
  FMaxIncludeStackDepth:=DefaultMaxIncludeStackDepth;

  FCurrentModeSwitches:=FPCModeSwitches;
  FAllowedModeSwitches:=msAllModeSwitches;
  FCurrentBoolSwitches:=bsFPCMode;
  FAllowedBoolSwitches:=bsAll;
  FAllowedValueSwitches:=vsAllValueSwitches;
  for vs in TValueSwitch do
    FCurrentValueSwitches[vs]:=DefaultValueSwitches[vs];

  FConditionEval:=TCondDirectiveEvaluator.Create;
  FConditionEval.OnLog:=@OnCondEvalLog;
  FConditionEval.OnEvalVariable:=@OnCondEvalVar;
  FConditionEval.OnEvalFunction:=@OnCondEvalFunction;
end;

destructor TPascalScanner.Destroy;
begin
  while FIncludeStack.Count>0 do
    PopStackItem;
  FreeAndNil(FConditionEval);
  ClearMacros;
  FreeAndNil(FMacros);
  FreeAndNil(FDefines);
  ClearFiles;
  FreeAndNil(FFiles);
  FreeAndNil(FIncludeStack);
  inherited Destroy;
end;

procedure TPascalScanner.RegisterResourceHandler(aExtension: String;
  const aHandler: TResourceHandler);

Var
  Idx: Integer;

begin
  if (aExtension='') then
    exit;
  if (aExtension[1]='.') then
    aExtension:=copy(aExtension,2,Length(aExtension)-1);
  Idx:=IndexOfResourceHandler(lowerCase(aExtension));
  if Idx=-1 then
    begin
    Idx:=Length(FResourceHandlers);
    SetLength(FResourceHandlers,Idx+1);
    FResourceHandlers[Idx].Ext:=LowerCase(aExtension);
    end;
  FResourceHandlers[Idx].handler:=aHandler;
end;

procedure TPascalScanner.RegisterResourceHandler(const aExtensions: array of String;
  const aHandler: TResourceHandler);

Var
  S : TPasScannerString;

begin
  For S in aExtensions do
    RegisterResourceHandler(S,aHandler);
end;

procedure TPascalScanner.RegisterDirectiveHandler(const aDirective: String;
  const aHandler: TPScannerDirectiveEvent);
var
  i: Integer;
  Item: TDirectiveHandlerRecord;
begin
  if aDirective='' then exit;
  i:=IndexOfDirectiveHandle(aDirective,true);
  if (i<length(FDirectiveHandles))
      and (CompareText(aDirective,FDirectiveHandles[i].Directive)=0) then
    begin
    // replace
    FDirectiveHandles[i].Directive:=aDirective;
    FDirectiveHandles[i].Handler:=aHandler;
    end
  else
    begin
    Item.Directive:=aDirective;
    Item.Handler:=aHandler;
    Insert(Item,FDirectiveHandles,i);
    end;
end;

procedure TPascalScanner.RegisterDirectiveHandler(const aDirectives: TStringDynArray;
  const aHandler: TPScannerDirectiveEvent);
var
  S: String;
begin
  for S in aDirectives do
    RegisterDirectiveHandler(S,aHandler);
end;

procedure TPascalScanner.ClearFiles;

begin
  // Dont' free the first element, because it is CurSourceFile
  while FIncludeStack.Count > 1 do
    begin
    TBaseFileResolver(FIncludeStack[1]).{$ifdef pas2js}Destroy{$else}Free{$endif};
    FIncludeStack.Delete(1);
    end;
  FIncludeStack.Clear;
  FreeAndNil(FCurSourceFile);
  FFiles.Clear;
  FModuleRow:=0;
end;

procedure TPascalScanner.ClearMacros;

Var
  I : Integer;

begin
  For I:=0 to FMacros.Count-1 do
    FMacros.Objects[i].{$ifdef pas2js}Destroy{$else}Free{$endif};
  FMacros.Clear;
end;

procedure TPascalScanner.SetCurToken(const AValue: TToken);
begin
  FCurToken:=AValue;
end;

procedure TPascalScanner.SetCurTokenString(const AValue: TPasScannerString);
begin
  FCurTokenString:=AValue;
end;

procedure TPascalScanner.OpenFile(AFilename: TPasScannerString);

Var
  aPath : TPasScannerString;

begin
  Clearfiles;
  FCurSourceFile := FileResolver.FindSourceFile(AFilename);
  FCurFilename := AFilename;
  AddFile(FCurFilename);
  {$IFDEF HASFS}
  aPath:=ExtractFilePath(FCurFilename);
  if (aPath<>'') then
    aPath:=IncludeTrailingPathDelimiter(aPath);
  FileResolver.ModuleDirectory := aPath;
  FileResolver.BaseDirectory := aPath;
  {$ENDIF}
  if LogEvent(sleFile) then
    DoLog(mtInfo,nLogOpeningFile,SLogOpeningFile,[FormatPath(AFileName)],True);
end;

procedure TPascalScanner.FinishedModule;
begin
  if (sleLineNumber in LogEvents)
      and (not CurSourceFile.IsEOF)
      and ((FCurRow Mod 100) > 0) then
    DoLog(mtInfo,nLogLineNumber,SLogLineNumber,[CurRow],True);
end;

function TPascalScanner.FormatPath(const aFilename: String): String;
begin
  if Assigned(OnFormatPath) then
    Result:=OnFormatPath(aFilename)
  else
    Result:=aFilename;
end;

function TPascalScanner.FormatSrcPos(const p: TPasSourcePos): String;
begin
  Result:=FormatPath(p.FileName)+'('+IntToStr(p.Row);
  if p.Column>0 then
    Result:=Result+','+IntToStr(p.Column);
  Result:=Result+')';
end;

function TPascalScanner.FormatCurrentSrcPos: String;
begin
  Result:=FormatSrcPos(CurSourcePos);
end;

procedure TPascalScanner.DisablePackageTokens;
begin
  FNonTokens:=FNonTokens+[tkContains,tkPackage,tkrequires];
end;

procedure TPascalScanner.SetNonToken(aToken: TToken);
begin
  Include(FNonTokens,aToken);
end;

procedure TPascalScanner.UnsetNonToken(aToken: TToken);
begin
  Exclude(FNonTokens,aToken);
end;

procedure TPascalScanner.SetTokenOption(aOption: TTokenoption);
begin
  Include(FTokenOptions,aOption);
end;

procedure TPascalScanner.UnSetTokenOption(aOption: TTokenoption);
begin
  Exclude(FTokenOptions,aOption);
end;

function TPascalScanner.CheckToken(aToken: TToken; const ATokenString: TPasScannerString): TToken;
begin
  Result:=atoken;
  if (aToken=tkIdentifier) and (CompareText(aTokenString,'operator')=0) then
    if (toOperatorToken in TokenOptions) then
      Result:=tkoperator;
end;

procedure TPascalScanner.PopStackItem;

var
  IncludeStackItem: TIncludeStackItem;
begin
  IncludeStackItem :=
    TIncludeStackItem(FIncludeStack[FIncludeStack.Count - 1]);
  FIncludeStack.Delete(FIncludeStack.Count - 1);
  CurSourceFile.{$ifdef pas2js}Destroy{$else}Free{$endif};
  FCurSourceFile := IncludeStackItem.SourceFile;
  FCurFilename := IncludeStackItem.Filename;
  FileResolver.BaseDirectory:=ExtractFilePath(FCurFilename);
  FCurToken := IncludeStackItem.Token;
  FCurTokenString := IncludeStackItem.TokenString;
  FCurLine := IncludeStackItem.Line;
  FCurRow := IncludeStackItem.Row;
  FCurColumnOffset := IncludeStackItem.ColumnOffset;
  FTokenPos := IncludeStackItem.TokenPos;
  IncludeStackItem.Free;
end;

function TPascalScanner.FetchToken: TToken;

begin
  if Not (FCurToken in [tkWhiteSpace,tkLineEnding]) then
    FPreviousToken:=FCurToken;
  while true do
  begin
    Result := DoFetchToken;
    Case FCurToken of
    tkEOF:
      begin
      if FIncludeStack.Count > 0 then
        begin
        PopStackItem;
        Result := FCurToken;
        end
      else
        break;
      end;
    tkWhiteSpace,
    tkLineEnding:
      if not (FSkipWhiteSpace or PPIsSkipping) then
        Break;
    tkComment:
      if not (FSkipComments or PPIsSkipping) then
        Break;
    tkSelf:
      begin
      if Not (po_selftoken in Options) then
        begin
        FCurToken:=tkIdentifier;
        Result:=FCurToken;
        end;
      if not (FSkipComments or PPIsSkipping) then
        Break;
      end;
    tkOperator:
      begin
      if Not (toOperatorToken in FTokenOptions) then
        begin
        FCurToken:=tkIdentifier;
        Result:=FCurToken;
        end;
      if not (FSkipComments or PPIsSkipping) then
        Break;
      end;

    else
      if not PPIsSkipping then
        break;
    end; // Case
  end;
//  Writeln(Result, '(',CurTokenString,')');
end;

function TPascalScanner.ReadNonPascalTillEndToken(StopAtLineEnd: boolean
  ): TToken;

var
  StartPos: {$ifdef UsePChar}PAnsiChar{$else}integer{$endif};
  {$ifndef UsePChar}
  var
    s: TPasScannerString;
    l: integer;
  {$endif}

  Procedure Add;
  var
    AddLen: PtrInt;
    {$ifdef UsePChar}
    OldLen: Integer;
    {$endif}
  begin
    AddLen:=FTokenPos-StartPos;
    if AddLen=0 then
      FCurTokenString:=''
    else
      begin
      {$ifdef UsePChar}
      OldLen:=length(FCurTokenString);
      SetLength(FCurTokenString,OldLen+AddLen);
      Move(StartPos^,PAnsiChar(PAnsiChar(FCurTokenString)+OldLen)^,AddLen);
      {$else}
      FCurTokenString:=FCurTokenString+copy(FCurLine,StartPos,AddLen);
      {$endif}
      StartPos:=FTokenPos;
      end;
  end;

  function DoEndOfLine: boolean;
  begin
    Add;
    if StopAtLineEnd then
      begin
      ReadNonPascalTillEndToken := tkLineEnding;
      FCurToken := tkLineEnding;
      FetchLine;
      exit(true);
      end;
    if not FetchLine then
      begin
      ReadNonPascalTillEndToken := tkEOF;
      FCurToken := tkEOF;
      exit(true);
      end;
    {$ifndef UsePChar}
    s:=FCurLine;
    l:=length(s);
    {$endif}
    StartPos:=FTokenPos;
    Result:=false;
  end;

begin
  Result:=tkEOF;
  FCurTokenString := '';
  StartPos:=FTokenPos;
  {$ifndef UsePChar}
  s:=FCurLine;
  l:=length(s);
  {$endif}
  repeat
    {$ifndef UsePChar}
    if FTokenPos>l then
      if DoEndOfLine then exit;
    {$endif}
    case {$ifdef UsePChar}FTokenPos^{$else}s[FTokenPos]{$endif} of
      {$ifdef UsePChar}
      #0: // end of line
        if DoEndOfLine then exit;
      {$endif}
      '{': // Pascal comments are supported.
        begin
        If po_AsmPascalComments in Options then
          begin
          Result:=HandleMultilineComment;
          Break;
          end
        else
          Inc(FTokenPos);
        end;
      '''':
        begin
        // Notes:
        // 1. Eventually there should be a mechanism to override parsing non-pascal
        // 2. By default skip Pascal TPasScannerString literals, as this is more intuitive
        //    in IDEs with Pascal highlighters
        inc(FTokenPos);
        repeat
          {$ifndef UsePChar}
          if FTokenPos>l then
            Error(nErrOpenString,SErrOpenString);
          {$endif}
          case {$ifdef UsePChar}FTokenPos^{$else}s[FTokenPos]{$endif} of
          {$ifdef UsePChar}
          #0: Error(nErrOpenString,SErrOpenString);
          {$endif}
          '''':
            begin
            inc(FTokenPos);
            break;
            end;
          #10,#13:
            begin
            // TPasScannerString literal missing closing apostroph
            break;
            end
          else
            inc(FTokenPos);
          end;
        until false;
        end;
      '"': // string literals: labels, section names etc.
        begin
        inc(FTokenPos);
        repeat
          {$ifndef UsePChar}
          if FTokenPos>l then
            Error(nErrOpenString,SErrOpenString);
          {$endif}
          case {$ifdef UsePChar}FTokenPos^{$else}s[FTokenPos]{$endif} of
          {$ifdef UsePChar}
          #0: Error(nErrOpenString,SErrOpenString);
          {$endif}
          '"':
            begin
            inc(FTokenPos);
            break;
            end;
          #10,#13:
            begin
            // String literal missing closing quote
            break;
            end
          else
            inc(FTokenPos);
          end;
        until false;
        end;
      '/':
        begin
        inc(FTokenPos);
        if {$ifdef UsePChar}FTokenPos^='/'{$else}(FTokenPos<=l) and (s[FTokenPos]='/'){$endif} then
          begin
          // skip Delphi comment //, see Note above
          repeat
            inc(FTokenPos);
          until {$ifdef UsePChar}FTokenPos^ in [#0,#10,#13]{$else}(FTokenPos>l) or (s[FTokenPos] in [#10,#13]){$endif};
          end;
        end;
      '@','0'..'9', 'A'..'Z', 'a'..'z','_':
        begin
        // number or identifier
        if {$ifdef UsePChar}
            (FTokenPos[0] in ['e','E'])
            and (FTokenPos[1] in ['n','N'])
            and (FTokenPos[2] in ['d','D'])
            and not (FTokenPos[3] in IdentChars)
            {$else}
            (TJSString(copy(s,FTokenPos,3)).toLowerCase='end')
            and ((FTokenPos+3>l) or not (s[FTokenPos+3] in IdentChars))
            {$endif}
            then
          begin
          // 'end' found
          Add;
          if FCurTokenString<>'' then
            begin
            // return characters in front of 'end'
            Result:=tkWhitespace;
            FCurToken:=Result;
            exit;
            end;
          // return 'end'
          if PPIsSkipping then
            Result := tkWhitespace
          else
            Result := tkend;
          {$ifdef UsePChar}
          SetLength(FCurTokenString, 3);
          Move(FTokenPos^, FCurTokenString[1], 3);
          {$else}
          FCurTokenString:=copy(s,FTokenPos,3);
          {$endif}
          inc(FTokenPos,3);
          FCurToken := Result;
          exit;
          end
        else
          begin
          // skip identifier
          if {$ifdef UsePChar}FTokenPos[0]='@'{$ELSE} (FTokenPos<=l) and (s[FTokenPos]='@'){$ENDIF} then
            inc(FTokenPos);
          while {$ifdef UsePChar}FTokenPos[0] in IdentChars{$else}(FTokenPos<=l) and (s[FTokenPos] in IdentChars){$endif} do
            inc(FTokenPos);
          end;
        end;
      else
        // Else case FTokenPos
        inc(FTokenPos);
    end;
  until false;
end;

procedure TPascalScanner.ErrorAt(MsgNumber: integer; const Msg: TPasScannerString; aRow, ACol: Integer);
begin
  SetCurMsg(mtError,MsgNumber,Msg,[]);
  raise EScannerError.CreateFmt('%s(%d,%d) Error: %s',
    [FormatPath(CurFilename),aRow,aCol,FLastMsg]);
end;

procedure TPascalScanner.Error(MsgNumber: integer; const Msg: TPasScannerString);
begin
  ErrorAt(MsgNumber,Msg,CurRow,CurColumn);
end;

procedure TPascalScanner.Error(MsgNumber: integer; const Fmt: TPasScannerString;
  Args: array of const);
begin
  SetCurMsg(mtError,MsgNumber,Fmt,Args);
  raise EScannerError.CreateFmt('%s(%d,%d) Error: %s',
    [FormatPath(CurFilename),CurRow,CurColumn,FLastMsg]);
end;

function TPascalScanner.GetMultiLineStringLineEnd(aReader : TLineReader) : TPasScannerString;

Var
  aLF : TPasScannerString;
  aStyle: TEOLStyle;


begin
  aStyle:=MultilineStringsEOLStyle;
  if aStyle=elSource then
    aStyle:=aReader.LastEOLStyle;
  case aStyle of
    elCR : aLF:=#13;
    elCRLF : aLF:=#13#10;
    elLF : aLF:=#10;
    elPlatform : alf:=sLineBreak;
  else
    aLF:=#10;
  end;
  Result:=aLF;
end;

function TPascalScanner.DoFetchTextToken:TToken;
var
  TokenStart, StartP : {$ifdef UsePChar}PAnsiChar{$else}integer{$endif};
  SectionLength : Integer;
  {$ifndef UsePChar}
  s: TPasScannerString;
  l: integer;
  {$endif}
begin
  Result:=tkEOF;
  FCurTokenString := '';
  {$ifndef UsePChar}
  s:=FCurLine;
  l:=length(s);
  {$endif}

  StartP := FTokenPos;
  repeat
    {$ifndef UsePChar}
    if FTokenPos>l then break;
    {$endif}
    case {$ifdef UsePChar}FTokenPos[0]{$else}s[FTokenPos]{$endif} of
      '^' :
        begin
        Inc(FTokenPos);
        if {$ifdef UsePChar}FTokenPos[0] in Letters{$else}(FTokenPos<l) and (s[FTokenPos] in Letters){$endif} then
          Inc(FTokenPos);
        if Result=tkEOF then
          Result := tkChar
        else
          Result := tkString;
        end;
      '#':
        begin
        Inc(FTokenPos);
        if {$ifdef UsePChar}FTokenPos[0]='$'{$else}(FTokenPos<l) and (s[FTokenPos]='$'){$endif} then
        begin
          Inc(FTokenPos);
          repeat
            Inc(FTokenPos);
          until {$ifdef UsePChar}not (FTokenPos[0] in HexDigits){$else}(FTokenPos>l) or not (s[FTokenPos] in HexDigits){$endif};
        end else
          repeat
            Inc(FTokenPos);
          until {$ifdef UsePChar}not (FTokenPos[0] in Digits){$else}(FTokenPos>l) or not (s[FTokenPos] in Digits){$endif};
        if Result=tkEOF then
          Result := tkChar
        else
          Result := tkString;
        end;
      '''':
        begin
          TokenStart := FTokenPos;
          Inc(FTokenPos);

          while true do
          begin
            if {$ifdef UsePChar}FTokenPos[0] = ''''{$else}(FTokenPos<=l) and (s[FTokenPos]=''''){$endif} then
              if {$ifdef UsePChar}FTokenPos[1] = ''''{$else}(FTokenPos<l) and (s[FTokenPos+1]=''''){$endif} then
                begin
                Inc(FTokenPos);
                if Result=tkEOF then
                  Result:=tkChar
                else
                  Result:=tkString;
                end
              else
                break;

            if {$ifdef UsePChar}FTokenPos[0] = #0{$else}FTokenPos>l{$endif} then
              Error(nErrOpenString,SErrOpenString);

            Inc(FTokenPos);
          end;
          Inc(FTokenPos);
          if (Result=tkEOF) and ((FTokenPos - TokenStart)=3) then // 'z'
            Result := tkChar
          else
            Result := tkString;
        end;
      '`':
        if (msMultiLineStrings in CurrentModeSwitches) then
        begin
          // Backtick string as continuation: #$41` text `#$42
          // Flush raw segment before the backtick
          SectionLength := FTokenPos - StartP;
          {$ifdef UsePChar}
          if SectionLength > 0 then
          begin
            SetLength(FCurTokenString, Length(FCurTokenString) + SectionLength);
            Move(StartP^, FCurTokenString[Length(FCurTokenString) - SectionLength + 1], SectionLength);
          end;
          {$else}
          if SectionLength > 0 then
            FCurTokenString := FCurTokenString + copy(s, StartP, SectionLength);
          {$endif}
          // Convert backtick content to apostrophe-delimited form
          FCurTokenString := FCurTokenString + '''';
          Inc(FTokenPos); // skip opening backtick
          {$ifndef UsePChar}
          while FTokenPos <= l do
          begin
            case s[FTokenPos] of
              '`':
                if (FTokenPos < l) and (s[FTokenPos+1] = '`') then
                begin
                  // escaped backtick ``
                  FCurTokenString := FCurTokenString + '`';
                  Inc(FTokenPos, 2);
                end
                else
                begin
                  // closing backtick
                  Inc(FTokenPos);
                  break;
                end;
              '''':
                begin
                  // escape apostrophe inside backtick content
                  FCurTokenString := FCurTokenString + '''''';
                  Inc(FTokenPos);
                end;
            else
              FCurTokenString := FCurTokenString + s[FTokenPos];
              Inc(FTokenPos);
            end;
          end;
          {$else}
          while FTokenPos[0] <> #0 do
          begin
            case FTokenPos[0] of
              '`':
                if FTokenPos[1] = '`' then
                begin
                  FCurTokenString := FCurTokenString + '`';
                  Inc(FTokenPos, 2);
                end
                else
                begin
                  Inc(FTokenPos);
                  break;
                end;
              '''':
                begin
                  FCurTokenString := FCurTokenString + '''''';
                  Inc(FTokenPos);
                end;
            else
              FCurTokenString := FCurTokenString + FTokenPos[0];
              Inc(FTokenPos);
            end;
          end;
          {$endif}
          FCurTokenString := FCurTokenString + '''';
          // Reset StartP so subsequent segments are captured correctly
          StartP := FTokenPos;
          Result := tkString;
        end
        else
          Break;
    else
      Break;
    end;
  until false;
  SectionLength := FTokenPos - StartP;
  {$ifdef UsePChar}
  if SectionLength > 0 then
  begin
    SetLength(FCurTokenString, Length(FCurTokenString) + SectionLength);
    Move(StartP^, FCurTokenString[Length(FCurTokenString) - SectionLength + 1], SectionLength);
  end;
  {$else}
  if SectionLength > 0 then
    FCurTokenString := FCurTokenString + copy(FCurLine, StartP, SectionLength);
  {$endif}
end;

function TPascalScanner.DoFetchMultilineTextToken:TToken;
// works similar to DoFetchTextToken, except changes indentation

var
  StartPos: Integer;
  TokenStart: {$ifdef UsePChar}PAnsiChar{$else}integer{$endif};
  {$ifdef UsePChar}
  OldLength: integer;
  {$else}
  s: TPasScannerString;
  l: integer;
  {$endif}
  Apostroph, CurLF : TPasScannerString;

  procedure Add(const S: TPasScannerString);
  begin
    if S='' then exit;
    FCurTokenString:=FCurTokenString+S;
    {$IFDEF UsePChar}
    OldLength:=length(FCurTokenString);
    {$ENDIF}
  end;

  Procedure AddToCurString(addLF : Boolean);
  var
    i : Integer;
    {$ifdef UsePChar}
    TokenOffset, Cnt: Integer;
    {$endif}

  begin
    // Start of line, take indent into account
    if ({$ifdef UsePChar}TokenStart=PAnsichar(FCurLine){$ELSE}Tokenstart=1{$ENDIF}) then
      begin
      i:=MultilineStringsTrimLeft;
      if I=-1 then
        // auto unindent -> use line indent of first line
        I:=StartPos+1;
      if I>0 then
        begin
        // fixed unindent -> remove up to I leading spaces
        While ({$ifdef UsePChar} TokenStart^{$ELSE}FCurLine[TokenStart]{$ENDIF} in [' ',#9]) and (TokenStart<=FTokenPos) and (I>0) do
          begin
          Inc(TokenStart);
          Dec(I);
          end;
        end
      else if I=-2 then
        begin
        // no indent -> remove all leading spaces
        While ({$ifdef UsePChar} TokenStart^{$ELSE}FCurLine[TokenStart]{$ENDIF} in [' ',#9]) and (TokenStart<=FTokenPos) do
          Inc(TokenStart);
        end;
      end;
    {$ifdef UsePChar}
    TokenOffset := TokenStart - PAnsiChar(FCurLine) + 1;
    Cnt := FTokenPos - TokenStart;
    if Cnt > 0 then
      Add(copy(FCurLine, TokenOffset, Cnt));
    {$else}
    Add(copy(FCurLine,TokenStart,FTokenPos - TokenStart));
    {$ENDIF}
    if addLF then
      Add(CurLF);
  end;

  procedure AddApostroph;
  begin
    Add(Apostroph);
  end;

begin
  Result:=tkEOF;
  FCurTokenString := '';
  {$ifndef UsePChar}
  s:=FCurLine;
  l:=length(s);
  StartPos:=FTokenPos;
  {$ELSE}
  OldLength:=0;
  StartPos:=FTokenPos-PAnsiChar(FCurLine);
  {$endif}
  Apostroph:='''';
  CurLF:=GetMultiLineStringLineEnd(FCurSourceFile);

  repeat
    {$ifndef UsePChar}
    if FTokenPos>l then break;
    {$endif}
    case {$ifdef UsePChar}FTokenPos[0]{$else}s[FTokenPos]{$endif} of
      '^' :
        begin
        TokenStart := FTokenPos;
        Inc(FTokenPos);
        if {$ifdef UsePChar}FTokenPos[0] in Letters{$else}(FTokenPos<l) and (s[FTokenPos] in Letters){$endif} then
          Inc(FTokenPos);
        {$IFDEF UsePChar}
        Add(copy(FCurLine, TokenStart - PAnsiChar(FCurLine) + 1, FTokenPos-TokenStart));
        {$ELSE}
        Add(copy(FCurLine,TokenStart,FTokenPos-TokenStart));
        {$ENDIF}
        if Result=tkEOF then
          Result := tkChar
        else
          Result := tkString;
        end;
      '#':
        begin
        TokenStart := FTokenPos;
        Inc(FTokenPos);
        if {$ifdef UsePChar}FTokenPos[0]='$'{$else}(FTokenPos<l) and (s[FTokenPos]='$'){$endif} then
        begin
          Inc(FTokenPos);
          repeat
            Inc(FTokenPos);
          until {$ifdef UsePChar}not (FTokenPos[0] in HexDigits){$else}(FTokenPos>l) or not (s[FTokenPos] in HexDigits){$endif};
        end else
          repeat
            Inc(FTokenPos);
          until {$ifdef UsePChar}not (FTokenPos[0] in Digits){$else}(FTokenPos>l) or not (s[FTokenPos] in Digits){$endif};
        {$IFDEF UsePChar}
        Add(copy(FCurLine, TokenStart - PAnsiChar(FCurLine) + 1, FTokenPos-TokenStart));
        {$ELSE}
        Add(copy(FCurLine,TokenStart,FTokenPos-TokenStart));
        {$ENDIF}
        if Result=tkEOF then
          Result := tkChar
        else
          Result := tkString;
        end;
      '`':
        begin
          AddApostroph;
          Inc(FTokenPos);
          TokenStart := FTokenPos;

          while true do
          begin
            if {$ifdef UsePChar}FTokenPos[0] = #0{$else}FTokenPos>l{$endif} then
              begin
              AddToCurString(true);
              // Writeln('Curtokenstring : >>',FCurTokenString,'<<');
              if not Self.FetchLine then
                begin
                {$IFDEF UsePChar}
                SetLength(FCurTokenString,OldLength);
                {$ENDIF}
                Error(nErrOpenString,SErrOpenString);
                end;
              // Writeln('Current line is now : ',FCurLine);
              {$ifndef UsePChar}
              s:=FCurLine;
              l:=length(s);
              {$ELSE}
              FTokenPos:=PAnsiChar(FCurLine);
              {$endif}
              TokenStart:=FTokenPos;
              end
            else
              begin
              case {$ifdef UsePChar}FTokenPos^{$else}s[FTokenPos]{$endif} of
              '`':
                if {$ifdef UsePChar}FTokenPos[1] = '`'{$else}(FTokenPos<l) and (s[FTokenPos+1]='`'){$endif} then
                  begin
                  // treat two backticks as one
                  Inc(FTokenPos);
                  AddToCurString(false);
                  Inc(FTokenPos);
                  TokenStart := FTokenPos;
                  continue;
                  end
                else
                  begin
                  AddToCurString(false);
                  AddApostroph;
                  break;
                  end;
              '''':
                begin
                // convert apostroph to two apostrophes
                Inc(FTokenPos);
                AddToCurString(false);
                AddApostroph;
                TokenStart := FTokenPos;
                // Re-enter loop without extra Inc(FTokenPos) so that
                // the character after the apostrophe (possibly closing
                // backtick or end-of-line) is processed correctly.
                continue;
                end;
              end;
              Inc(FTokenPos);
              end;
          end;
          Inc(FTokenPos);
          Result := tkStringMultiLine;
        end;
    else
      {$IFDEF UsePChar}
      SetLength(FCurTokenString,OldLength);
      {$ENDIF}
      Break;
    end;
  until false;
end;

function TPascalScanner.DoFetchDelphiMultiLineTextToken(QuoteLen : Integer): TToken;
// works similar to DoFetchTextToken, except changes indentation

var
  s: TPasScannerString;
  CurLF : TPasScannerString;
  Lines : Array of String;
  l, I, SpaceCount, QuoteCount, WhiteSpaces, CurLineCount , Cnt: Integer;
  HasNonWhiteSpace: Boolean;

  Procedure AddToLines;

  var
    L : Integer;

  begin
    L:=Length(Lines);
    if CurLineCount=L then
      SetLength(Lines,L*2+10);
    Lines[CurLineCount]:=FCurLine;
    Inc(CurLineCount);
  end;

  Function LocalFetchLine : Boolean;

  begin
    // Writeln('Curtokenstring : >>',FCurTokenString,'<<');
    Result:=Self.FetchLine;
    if not Result then
      Error(nErrOpenString,SErrOpenString);
    // Writeln('Current line is now : ',FCurLine);
    {$IFDEF UsePChar}
    FTokenPos:=PAnsiChar(FCurLine);
    {$ELSE}
    s:=FCurLine;
    l:=length(s);
    {$ENDIF}
  end;

begin
  Lines:=[];
  CurLineCount:=0;
  Result:=tkEOF;
  FCurTokenString := '';
  // On entry, we know that the current position is the start of the multiline quoted string.
  // the strings are added as-is.
  repeat
    QuoteCount:=0;
    WhiteSpaces:=0;
    if not LocalFetchLine then
      exit(tkEOF);
    // Skip whitespace, but count, as the last line defines the unindented WhiteSpaces.
    {$IFDEF USEPCHAR}
    While (FTokenPos[0]=' ') do
    {$ELSE}
    While (FTokenPos<=l) and (s[FTokenPos]=' ') do
    {$ENDIF}
      begin
      Inc(FTokenPos);
      Inc(WhiteSpaces);
      end;
    // check for the end sequence of quotes
    HasNonWhiteSpace:=false;
    repeat
      {$IFDEF USEPCHAR}
      case FTokenPos[0] of
      #0:
        break;
      {$ELSE}
      if FTokenPos>l then
        break;
      case s[FTokenPos] of
      {$ENDIF}
      SingleQuote:
        begin
          repeat
            inc(FTokenPos);
            inc(QuoteCount);
          {$IFDEF UsePChar}
          until (FTokenPos[0]<>SingleQuote) or (QuoteCount=QuoteLen);
          {$ELSE}
          until (FTokenPos>l) or (s[FTokenPos]<>SingleQuote) or (QuoteCount=QuoteLen);
          {$ENDIF}
          if QuoteCount=QuoteLen then
            begin
            if HasNonWhiteSpace then
              Error(nErrMultilineNonWhiteSpaceBeforeClosing,sErrMultilineNonWhiteSpaceBeforeClosing);
            break;
            end;
          HasNonWhiteSpace:=true;
        end;
      else
        HasNonWhiteSpace:=true;
        inc(FTokenPos);
      end;
    until false;
    if QuoteCount<>QuoteLen then
      AddToLines // another multiline
    else
      break;
  Until false;
  // Note: the last line defines the needed whitespaces of all lines

  CurLF:=GetMultiLineStringLineEnd(FCurSourceFile);

  // unindent
  Cnt:=0;
  For I:=0 to CurLineCount-1 do
    begin
    // cut whitespaces
    s:=Lines[I];
    SpaceCount:=0;
    l:=length(s);
    while (SpaceCount<l) and (s[SpaceCount+1]=' ') do
      inc(SpaceCount);
    if SpaceCount=l then
      begin
      // empty line
      s:='';
      end
    else if SpaceCount<WhiteSpaces then
      ErrorAt(nErrInvalidIndent,SErrInvalidIndent,CurRow-CurLineCount+I,SpaceCount)
    else
      s:=copy(s,WhiteSpaces+1,l);
    Lines[I]:=s;
    if I>0 then
      inc(Cnt,length(CurLF));
    inc(Cnt,length(s));
    end;

  // build final string
  {$IFNDEF PAS2JS}
  SetLength(FCurTokenString,Cnt);
  {$ENDIF}
  Cnt:=0;
  For I:=0 to CurLineCount-1 do
    begin
    s:=Lines[I];
    l:=length(s);
    if l>0 then
      begin
      {$IFDEF PAS2JS}
      FCurTokenString:=FCurTokenString+S;
      {$ELSE}
      System.Move(s[1],FCurTokenString[Cnt+1],l);
      inc(Cnt,l);
      {$ENDIF}
      end;
    if I<CurLineCount-1 then
      begin
      {$IFDEF PAS2JS}
      FCurTokenString:=FCurTokenString+CurLF;
      {$ELSE}
      l:=length(CurLF);
      System.Move(CurLF[1],FCurTokenString[Cnt+1],l);
      inc(Cnt,l);
      {$ENDIF}
      end;
    end;
  Result:=tkStringMultiLine;
end;

procedure TPascalScanner.PushStackItem;

Var
  SI: TIncludeStackItem;

begin
  if FIncludeStack.Count>=MaxIncludeStackDepth then
    Error(nErrIncludeLimitReached,SErrIncludeLimitReached);
  SI := TIncludeStackItem.Create;
  SI.SourceFile := CurSourceFile;
  SI.Filename := CurFilename;
  SI.Token := CurToken;
  SI.TokenString := CurTokenString;
  SI.Line := CurLine;
  SI.Row := CurRow;
  SI.ColumnOffset := FCurColumnOffset;
  SI.TokenPos := FTokenPos;
  FIncludeStack.Add(SI);
  FTokenPos:={$ifdef UsePChar}Nil{$else}-1{$endif};
  FCurRow := 0;
  FCurColumnOffset := 1;
end;

procedure TPascalScanner.HandleIncludeFile(Param: TPasScannerString);

var
  NewSourceFile: TLineReader;
  aFileName : TPasScannerString;

begin
  Param:=Trim(Param);
  if Length(Param)>1 then
    begin
    if (Param[1]='''') then
      begin
      if Param[length(Param)]<>'''' then
        Error(nErrOpenString,SErrOpenString,[]);
      Param:=copy(Param,2,length(Param)-2);
      end;
    end;
  NewSourceFile := FileResolver.FindIncludeFile(Param);
  if not Assigned(NewSourceFile) then
    Error(nErrIncludeFileNotFound, SErrIncludeFileNotFound, [Param]);


  PushStackItem;
  FCurSourceFile:=NewSourceFile;
  FCurFilename := Param;
  if FCurSourceFile is TLineReader then
    begin
    aFileName:=TLineReader(FCurSourceFile).Filename;
    FileResolver.BaseDirectory := ExtractFilePath(aFileName);
    FCurFilename := aFileName; // nicer error messages
    end;
  AddFile(FCurFilename);
  If LogEvent(sleFile) then
    DoLog(mtInfo,nLogOpeningFile,SLogOpeningFile,[FormatPath(FCurFileName)],True);
end;

procedure TPascalScanner.HandleIncludeString(Param: TPasScannerString);

var
  NewSourceFile: TLineReader;
  aString,aLine: TPasScannerString;

begin
  Param:=Trim(Param);
  if Length(Param)>1 then
    begin
    if (Param[1]='''') then
      begin
      if Param[length(Param)]<>'''' then
        Error(nErrOpenString,SErrOpenString,[]);
      Param:=copy(Param,2,length(Param)-2);
      end;
    end;
  NewSourceFile := FileResolver.FindIncludeFile(Param);
  if not Assigned(NewSourceFile) then
    Error(nErrIncludeFileNotFound, SErrIncludeFileNotFound, [Param]);
  try
    AString:='';
    While not NewSourceFile.IsEOF Do
      begin
      ALine:=NewSourceFile.ReadLine;
      if aString<>'' then
        aString:=aString+GetMultiLineStringLineEnd(NewSourceFile);
      AString:=aString+aLine;
      end;
  finally
    NewSourceFile.Free;
  end;
  FCurTokenString:=''''+AString+'''';
  FCurToken:=tkString;
end;

procedure TPascalScanner.HandleResource(Param: TPasScannerString);

Var
  Ext,aFullFileName,aFilename,aOptions : TPasScannerString;
  P: Integer;
  H : TResourceHandler;
  OptList : TStrings;

begin
  aFilename:='';
  aOptions:='';
  P:=Pos(';',Param);
  If P=0 then
    aFileName:=Trim(Param)
  else
    begin
    aFileName:=Trim(Copy(Param,1,P-1));
    aOptions:=Copy(Param,P+1,Length(Param)-P);
    end;
  Ext:=ExtractFileExt(aFileName);
  // Construct & find filename
  If (ChangeFileExt(aFileName,RTLString(''))='*') then
    aFileName:=ChangeFileExt(ExtractFileName(CurFilename),Ext);
  aFullFileName:=FileResolver.FindResourceFileName(aFileName);
  if (aFullFileName='') then
    if (po_WarnResourceNotFound in Options) then
      Self.DoLog(mtWarning,nResourceFileNotFound,SErrResourceFileNotFound,[aFileName])
    else
      Error(nResourceFileNotFound,SErrResourceFileNotFound,[aFileName]);
  // Check if we can find a handler.
  if Ext<>'' then
    Ext:=Copy(Ext,2,Length(Ext)-1);
  H:=FindResourceHandler(LowerCase(Ext));
  if (H=Nil) then
    H:=FindResourceHandler('*');
  if (H=Nil) then
    begin
    if not (po_IgnoreUnknownResource in Options) then
      Error(nNoResourceSupport,SNoResourceSupport,[Ext]);
    exit;
    end;
  // Let the handler take care of the rest.
  OptList:=TStringList.Create;
  try
    OptList.NameValueSeparator:=':';
    OptList.Delimiter:=';';
    OptList.StrictDelimiter:=True;
    OptList.DelimitedText:=aOptions;
    H(Self,aFullFileName,OptList);
  finally
    OptList.Free;
  end;
end;

function TPascalScanner.MakeLibAlias(const LibFileName: TPasScannerString): TPasScannerString;

Var
  p,l,d : integer;

begin
  l:=Length(LibFileName);
  p:=l;
  d:=0;
  while (p>0) and not (LibFileName[p]='/') do
    begin
    if (LibFileName[p]='.') and (d=0) then
      d:=p;
    dec(P);
    end;
  if d=0 then
    d:=l+1;
  Result:=LowerCase(Copy(LibFileName,P+1,D-P-1));
  for p:=1 to length(Result) do
    if not (result[P] in ['a'..'z','A'..'Z','0'..'9','_']) then
      Result[p]:='_';
end;

procedure TPascalScanner.HandleLinkLib(Param: TPasScannerString);

Var
  P,L : Integer;
  LibFileName,LibAlias,LibOptions : TPasScannerString;
  IsHandled: Boolean;

  Function NextWord : TPasScannerString;

  Var
    lp : integer;

  begin
    lP:=P;
    while (lp<=l) and not (Param[lp]  in [' ',#9,#10,#13]) do
      inc(lp);
    Result:=Copy(Param,P,lp-P);
    P:=LP;
  end;

  Procedure DoSkipwhitespace;
  begin
    while (p<=l) and (Param[p]  in [' ',#9,#10,#13]) do
      inc(p);
  end;

begin
  Param:=Trim(Param);
  L:=Length(Param);
  P:=1;
  LibFileName:=NextWord;
  DoSkipWhiteSpace;
  if P<=L then
    LibAlias:=NextWord
  else
    LibAlias:=MakeLibAlias(LibFileName);
  LibOptions:=Trim(Copy(Param,P,L-P+1));
  IsHandled:=False;
  if Assigned(OnLinkLib) then
    OnLinkLib(Self,LibFileName,LibAlias,LibOptions,IsHandled);
  if not IsHandled then
    DoLog(mtNote,nWarnIgnoringLinkLib,SWarnIgnoringLinkLib,[LibFileName,LibAlias,LibOptions]);
end;

procedure TPascalScanner.HandleOptimizations(Param: TPasScannerString);
// $optimization A,B-,C+
var
  p, StartP, l: Integer;
  OptName, Value: TPasScannerString;
begin
  p:=1;
  l:=length(Param);
  while p<=l do
    begin
    // read next flag
    // skip whitespace
    while (p<=l) and (Param[p] in [' ',#9,#10,#13]) do
      inc(p);
    // read name
    StartP:=p;
    while (p<=l) and (Param[p] in ['a'..'z','A'..'Z','0'..'9','_']) do
      inc(p);
    if p=StartP then
      Error(nWarnIllegalCompilerDirectiveX,sWarnIllegalCompilerDirectiveX,['optimization']);
    OptName:=copy(Param,StartP,p-StartP);
    if lowercase(LeftStr(OptName,2))='no' then
      begin
      Delete(OptName,1,2);
      DoHandleOptimization(OptName,'-');
      exit;
      end;
    // skip whitespace
    while (p<=l) and (Param[p] in [' ',#9,#10,#13]) do
      inc(p);
    // read value
    StartP:=p;
    while (p<=l) and (Param[p]<>',') do
      inc(p);
    Value:=TrimRight(copy(Param,StartP,p-StartP));
    DoHandleOptimization(OptName,Value);
    inc(p);
    end;
end;

procedure TPascalScanner.DoHandleOptimization(OptName, OptValue: TPasScannerString);
begin
  // default: skip any optimization directive
  if OptName='' then ;
  if OptValue='' then ;
end;

function TPascalScanner.HandleMacro(AIndex : integer) : TToken;

Var
  M : TMacroDef;
  ML : TMacroReader;
  OldRow, OldCol: Integer;

begin
  OldRow:=CurRow;
  OldCol:=CurColumn;
  PushStackItem;
  M:=FMacros.Objects[AIndex] as TMacroDef;
  ML:=TMacroReader.Create(FCurFileName,M.Value);
  ML.CurRow:=OldRow;
  ML.CurCol:=OldCol-length(M.Name);
  FCurSourceFile:=ML;
  Result:=DoFetchToken;
//  Writeln(Result,Curtoken);
end;

procedure TPascalScanner.HandleInterfaces(const Param: TPasScannerString);
var
  s, NewValue: TPasScannerString;
  p: SizeInt;
begin
  if not (vsInterfaces in AllowedValueSwitches) then
    Error(nWarnIllegalCompilerDirectiveX,sWarnIllegalCompilerDirectiveX,['interfaces']);
  s:=Uppercase(Param);
  p:=Pos(' ',s);
  if p>0 then
    s:=LeftStr(s,p-1);
  case s of
  'COM','DEFAULT': NewValue:='COM';
  'CORBA': NewValue:='CORBA';
  else
    Error(nWarnIllegalCompilerDirectiveX,sWarnIllegalCompilerDirectiveX,['interfaces '+s]);
    exit;
  end;
  if SameText(NewValue,CurrentValueSwitch[vsInterfaces]) then exit;
  if vsInterfaces in ReadOnlyValueSwitches then
    begin
    Error(nWarnIllegalCompilerDirectiveX,sWarnIllegalCompilerDirectiveX,['interfaces']);
    exit;
    end;
  CurrentValueSwitch[vsInterfaces]:=NewValue;
end;

procedure TPascalScanner.HandleWarn(Param: TPasScannerString);
// $warn identifier on|off|default|error
var
  p, StartPos: Integer;
  Identifier, Value: TPasScannerString;
begin
  p:=1;
  while (p<=length(Param)) and (Param[p] in [' ',#9]) do inc(p);
  StartPos:=p;
  while (p<=length(Param)) and (Param[p] in ['a'..'z','A'..'Z','0'..'9','_']) do inc(p);
  Identifier:=copy(Param,StartPos,p-StartPos);
  while (p<=length(Param)) and (Param[p] in [' ',#9]) do inc(p);
  StartPos:=p;
  while (p<=length(Param)) and (Param[p] in ['a'..'z','A'..'Z','_']) do inc(p);
  Value:=copy(Param,StartPos,p-StartPos);
  HandleWarnIdentifier(Identifier,Value);
end;

procedure TPascalScanner.HandleWarnIdentifier(Identifier,
  Value: TPasScannerString);
var
  Number: LongInt;
  State: TWarnMsgState;
  Handled: Boolean;
begin
  if Identifier='' then
    Error(nIllegalStateForWarnDirective,SIllegalStateForWarnDirective,['']);
  if Value='' then
    begin
    DoLog(mtWarning,nIllegalStateForWarnDirective,SIllegalStateForWarnDirective,['']);
    exit;
    end;
  case lowercase(Value) of
  'on': State:=wmsOn;
  'off': State:=wmsOff;
  'default': State:=wmsDefault;
  'error': State:=wmsError;
  else
    DoLog(mtWarning,nIllegalStateForWarnDirective,SIllegalStateForWarnDirective,[Value]);
    exit;
  end;

  if Assigned(OnWarnDirective) then
    begin
    Handled:=false;
    OnWarnDirective(Self,Identifier,State,Handled);
    if Handled then
      exit;
    end;

  if Identifier[1] in ['0'..'9'] then
    begin
    // fpc number
    Number:=StrToIntDef(Identifier,-1);
    if Number<0 then
      begin
      DoLog(mtWarning,nIllegalStateForWarnDirective,SIllegalStateForWarnDirective,[Identifier]);
      exit;
      end;
    SetWarnMsgState(Number,State);
    end;
end;

procedure TPascalScanner.HandleDefine(Param: TPasScannerString);

Var
  Index : Integer;
  MName,MValue : TPasScannerString;

begin
  // Param is already trimmed on entry.
  Index:=Pos(':=',Param);
  If (Index=0) then
    AddDefine(GetMacroName(Param))
  else
    begin
    MValue:=Param;
    MName:=UpperCase(Trim(Copy(MValue,1,Index-1)));
    Delete(MValue,1,Index+1);
    AddMacro(MName,Trim(MValue));
    end;
end;

procedure TPascalScanner.HandleDispatchField(Param: TPasScannerString; vs: TValueSwitch);
var
  NewValue: TPasScannerString;
begin
  if not (vs in AllowedValueSwitches) then
    Error(nWarnIllegalCompilerDirectiveX,sWarnIllegalCompilerDirectiveX,[ValueSwitchNames[vs]]);
  NewValue:=ReadIdentifier(Param);
  if NewValue='-' then
    NewValue:=''
  else if not IsValidIdent(NewValue,false) then
    DoLog(mtWarning,nInvalidDispatchFieldName,SInvalidDispatchFieldName,[]);
  if SameText(NewValue,CurrentValueSwitch[vs]) then exit;
  if vs in ReadOnlyValueSwitches then
    begin
    Error(nWarnIllegalCompilerDirectiveX,sWarnIllegalCompilerDirectiveX,[ValueSwitchNames[vs]]);
    exit;
    end;
  CurrentValueSwitch[vs]:=NewValue;
end;

procedure TPascalScanner.HandleError(Param: TPasScannerString);
begin
  if po_StopOnErrorDirective in Options then
    Error(nUserDefined, SUserDefined,[Param])
  else
    DoLog(mtWarning,nUserDefined,SUserDefined+' error',[Param]);
end;

procedure TPascalScanner.HandleMessageDirective(Param: TPasScannerString);
var
  p: Integer;
  Kind: TPasScannerString;
  MsgType: TMessageType;
begin
  if Param='' then exit;
  p:=1;
  while (p<=length(Param)) and (Param[p] in ['a'..'z','A'..'Z']) do inc(p);
  Kind:=LeftStr(Param,p-1);
  MsgType:=mtHint;
  case UpperCase(Kind) of
  'HINT': MsgType:=mtHint;
  'NOTE': MsgType:=mtNote;
  'WARN': MsgType:=mtWarning;
  'ERROR': MsgType:=mtError;
  'FATAL': MsgType:=mtFatal;
  else
    // $Message 'hint text'
    p:=1;
  end;
  while (p<=length(Param)) and (Param[p] in [' ',#9]) do inc(p);
  Delete(Param,1,p-1);
  if MsgType in [mtFatal,mtError] then
    HandleError(Param)
  else
    DoLog(MsgType,nUserDefined,SUserDefined,[Param]);
end;

procedure TPascalScanner.HandleUnDefine(Param: TPasScannerString);
begin
  UnDefine(GetMacroName(Param));
end;

function TPascalScanner.HandleInclude(const Param: TPasScannerString): TToken;

begin
  Result:=tkComment;
  if (Param<>'') and (Param[1]='%') then
    begin
    FCurTokenString:=''''+Param+'''';
    FCurToken:=tkString;
    Result:=FCurToken;
    end
  else
    HandleIncludeFile(Param);
end;

procedure TPascalScanner.HandleMode(const Param: TPasScannerString);

  procedure SetMode(const LangMode: TModeSwitch;
    const NewModeSwitches: TModeSwitches; IsDelphi: boolean;
    const AddBoolSwitches: TBoolSwitches = [];
    const RemoveBoolSwitches: TBoolSwitches = [];
    UseOtherwise: boolean = true
    );
  var
    Handled: Boolean;
  begin
    if not (LangMode in AllowedModeSwitches) then
      Error(nErrInvalidMode,SErrInvalidMode,[Param]);
    Handled:=false;
    if Assigned(OnModeChanged) then
      OnModeChanged(Self,LangMode,true,Handled);
    if not Handled then
      begin
      CurrentModeSwitches:=(NewModeSwitches+ReadOnlyModeSwitches)*AllowedModeSwitches;
      CurrentBoolSwitches:=CurrentBoolSwitches+(AddBoolSwitches*AllowedBoolSwitches)
        -(RemoveBoolSwitches*AllowedBoolSwitches);
      if IsDelphi then
        FOptions:=FOptions+[po_delphi]
      else
        FOptions:=FOptions-[po_delphi];
      if UseOtherwise then
        UnsetNonToken(tkotherwise)
      else
        SetNonToken(tkotherwise);
      end;
    Handled:=false;
    FileResolver.Mode:=LangMode;
    if Assigned(OnModeChanged) then
      OnModeChanged(Self,LangMode,false,Handled);
  end;

Var
  P : TPasScannerString;
begin
  if SkipGlobalSwitches then
    begin
    DoLog(mtWarning,nMisplacedGlobalCompilerSwitch,SMisplacedGlobalCompilerSwitch,[]);
    exit;
    end;
  P:=Trim(UpperCase(Param));
  Case P of
  'FPC','DEFAULT':
    begin
    SetMode(msFpc,FPCModeSwitches,false,bsFPCMode);
    SetNonToken(tkobjcclass);
    SetNonToken(tkobjcprotocol);
    SetNonToken(tkobjcCategory);
    end;
  'OBJFPC':
    begin
    SetMode(msObjfpc,OBJFPCModeSwitches,true,bsObjFPCMode);
    UnsetNonToken(tkgeneric);
    UnsetNonToken(tkspecialize);
    SetNonToken(tkobjcclass);
    SetNonToken(tkobjcprotocol);
    SetNonToken(tkobjcCategory);
    end;
  'DELPHI':
    begin
    SetMode(msDelphi,DelphiModeSwitches,true,bsDelphiMode,[bsPointerMath]);
    SetNonToken(tkgeneric);
    SetNonToken(tkspecialize);
    SetNonToken(tkobjcclass);
    SetNonToken(tkobjcprotocol);
    SetNonToken(tkobjcCategory);
    end;
  'DELPHIUNICODE':
    begin
    SetMode(msDelphiUnicode,DelphiUnicodeModeSwitches,true,bsDelphiUnicodeMode,[bsPointerMath]);
    SetNonToken(tkgeneric);
    SetNonToken(tkspecialize);
    SetNonToken(tkobjcclass);
    SetNonToken(tkobjcprotocol);
    SetNonToken(tkobjcCategory);
    end;
  'TP':
    SetMode(msTP7,TPModeSwitches,false);
  'MACPAS':
    SetMode(msMac,MacModeSwitches,false,bsMacPasMode);
  'ISO':
    SetMode(msIso,ISOModeSwitches,false,[],[],false);
  'EXTENDEDPASCAL':
    SetMode(msExtpas,ExtPasModeSwitches,false);
  'GPC':
    SetMode(msGPC,GPCModeSwitches,false);
  else
    Error(nErrInvalidMode,SErrInvalidMode,[Param])
  end;
end;

procedure TPascalScanner.HandleModeSwitch(const Param: TPasScannerString);
// $modeswitch param
// name, name-, name+, name off, name on, name- comment, name on comment
Var
  MS : TModeSwitch;
  MSN,PM : TPasScannerString;
  p : Integer;
  Enable: Boolean;

begin
  Enable:=False;
  PM:=Param;
  p:=1;
  while (p<=length(PM)) and (PM[p] in ['a'..'z','A'..'Z','_','0'..'9']) do
    inc(p);
  MSN:=LeftStr(PM,p-1);
  Delete(PM,1,p-1);
  MS:=StrToModeSwitch(MSN);
  if (MS=msNone) or not (MS in AllowedModeSwitches) then
    begin
    if po_CheckModeSwitches in Options then
      Error(nErrInvalidModeSwitch,SErrInvalidModeSwitch,[MSN])
    else
      DoLog(mtWarning,nErrInvalidModeSwitch,SErrInvalidModeSwitch,[MSN]);
    exit; // ignore
    end;
  if PM='' then
    Enable:=true
  else
    case PM[1] of
    '+','-':
      begin
      Enable:=PM[1]='+';
      p:=2;
      if (p<=length(PM)) and not (PM[p] in [' ',#9]) then
        Error(nErrWrongSwitchToggle,SErrWrongSwitchToggle,[]);
      end;
    ' ',#9:
      begin
      PM:=TrimLeft(PM);
      if PM<>'' then
        begin
        p:=1;
        while (p<=length(PM)) and (PM[p] in ['A'..'Z']) do inc(p);
        if (p<=length(PM)) and not (PM[p] in [' ',#9]) then
          Error(nErrWrongSwitchToggle,SErrWrongSwitchToggle,[]);
        PM:=LeftStr(PM,p-1);
        if PM='ON' then
          Enable:=true
        else if PM='OFF' then
          Enable:=false
        else
          Error(nErrWrongSwitchToggle,SErrWrongSwitchToggle,[]);
        end;
      end;
    else
      Error(nErrWrongSwitchToggle,SErrWrongSwitchToggle,[]);
    end;

  if MS in CurrentModeSwitches=Enable then
    exit; // no change
  if MS in ReadOnlyModeSwitches then
    begin
    DoLog(mtWarning,nErrInvalidModeSwitch,SErrInvalidModeSwitch,[MSN]);
    exit;
    end;
  if Enable then
    CurrentModeSwitches:=CurrentModeSwitches+[MS]
  else
    CurrentModeSwitches:=CurrentModeSwitches-[MS];
end;

procedure TPascalScanner.PushSkipMode;

begin
  if PPSkipStackIndex = High(PPSkipModeStack) then
    Error(nErrIfXXXNestingLimitReached,SErrIfXXXNestingLimitReached);
  PPSkipModeStack[PPSkipStackIndex] := PPSkipMode;
  PPIsSkippingStack[PPSkipStackIndex] := PPIsSkipping;
  Inc(PPSkipStackIndex);
end;

procedure TPascalScanner.HandleIFDEF(const AParam: TPasScannerString);
var
  aName: TPasScannerString;
begin
  PushSkipMode;
  if PPIsSkipping then
    PPSkipMode := ppSkipAll
  else
    begin
    aName:=ReadIdentifier(AParam);
    if IsDefined(aName) then
      PPSkipMode := ppSkipElseBranch
    else
      begin
      PPSkipMode := ppSkipIfBranch;
      PPIsSkipping := true;
      end;
    If LogEvent(sleConditionals) then
      if PPSkipMode=ppSkipElseBranch then
        DoLog(mtInfo,nLogIFDefAccepted,sLogIFDefAccepted,[aName])
      else
        DoLog(mtInfo,nLogIFDefRejected,sLogIFDefRejected,[aName]);
    end;
end;

procedure TPascalScanner.HandleIFNDEF(const AParam: TPasScannerString);
var
  aName: TPasScannerString;
begin
  PushSkipMode;
  if PPIsSkipping then
    PPSkipMode := ppSkipAll
  else
    begin
    aName:=ReadIdentifier(AParam);
    if IsDefined(aName) then
      begin
      PPSkipMode := ppSkipIfBranch;
      PPIsSkipping := true;
      end
    else
      PPSkipMode := ppSkipElseBranch;
    If LogEvent(sleConditionals) then
      if PPSkipMode=ppSkipElseBranch then
        DoLog(mtInfo,nLogIFNDefAccepted,sLogIFNDefAccepted,[aName])
      else
        DoLog(mtInfo,nLogIFNDefRejected,sLogIFNDefRejected,[aName]);
    end;
end;

procedure TPascalScanner.HandleIFOPT(const AParam: TPasScannerString);

begin
  PushSkipMode;
  if PPIsSkipping then
    PPSkipMode := ppSkipAll
  else
    begin
    if (length(AParam)<>2) or not (AParam[1] in ['a'..'z','A'..'Z'])
        or not (AParam[2] in ['+','-']) then
      Error(nErrXExpectedButYFound,sErrXExpectedButYFound,['letter[+|-]',AParam]);
    if IfOpt(AParam[1])=(AParam[2]='+') then
      PPSkipMode := ppSkipElseBranch
    else
      begin
      PPSkipMode := ppSkipIfBranch;
      PPIsSkipping := true;
      end;
    If LogEvent(sleConditionals) then
      if PPSkipMode=ppSkipElseBranch then
        DoLog(mtInfo,nLogIFOptAccepted,sLogIFOptAccepted,[AParam])
      else
        DoLog(mtInfo,nLogIFOptRejected,sLogIFOptRejected,[AParam]);
    end;
end;

procedure TPascalScanner.HandleIF(const AParam: TPasScannerString; aIsMac: Boolean);

begin
  PushSkipMode;
  if PPIsSkipping then
    PPSkipMode := ppSkipAll
  else
    begin
    ConditionEval.MsgCurLine:=CurTokenPos.Row;
    ConditionEval.isMac:=aIsMac;
    if ConditionEval.Eval(AParam) then
      PPSkipMode := ppSkipElseBranch
    else
      begin
      PPSkipMode := ppSkipIfBranch;
      PPIsSkipping := true;
      end;
    If LogEvent(sleConditionals) then
      if PPSkipMode=ppSkipElseBranch then
        DoLog(mtInfo,nLogIFAccepted,sLogIFAccepted,[AParam])
      else
        DoLog(mtInfo,nLogIFRejected,sLogIFRejected,[AParam]);
    end;
end;

procedure TPascalScanner.HandleELSEIF(const AParam: TPasScannerString; aIsMac : Boolean);
begin
  if PPSkipStackIndex = 0 then
    Error(nErrInvalidPPElse,sErrInvalidPPElse);
  if PPSkipMode = ppSkipIfBranch then
    begin
    ConditionEval.isMac:=aIsMac;
    if ConditionEval.Eval(AParam) then
      begin
      PPSkipMode := ppSkipElseBranch;
      PPIsSkipping := false;
      end
    else
      PPIsSkipping := true;
    If LogEvent(sleConditionals) then
      if PPSkipMode=ppSkipElseBranch then
        DoLog(mtInfo,nLogELSEIFAccepted,sLogELSEIFAccepted,[AParam])
      else
        DoLog(mtInfo,nLogELSEIFRejected,sLogELSEIFRejected,[AParam]);
    end
  else if PPSkipMode=ppSkipElseBranch then
    begin
    PPIsSkipping := true;
    end;
end;

procedure TPascalScanner.HandleELSE(const AParam: TPasScannerString);

begin
  if AParam='' then;
  if PPSkipStackIndex = 0 then
    Error(nErrInvalidPPElse,sErrInvalidPPElse);
  if PPSkipMode = ppSkipIfBranch then
    PPIsSkipping := false
  else if PPSkipMode = ppSkipElseBranch then
    PPIsSkipping := true;
end;


procedure TPascalScanner.HandleENDIF(const AParam: TPasScannerString);

begin
  if AParam='' then;
  if PPSkipStackIndex = 0 then
    Error(nErrInvalidPPEndif,sErrInvalidPPEndif);
  Dec(PPSkipStackIndex);
  PPSkipMode := PPSkipModeStack[PPSkipStackIndex];
  PPIsSkipping := PPIsSkippingStack[PPSkipStackIndex];
end;

function TPascalScanner.HandleDirective(const ADirectiveText: TPasScannerString): TToken;

Var
  Directive,Param : TPasScannerString;
  P : Integer;
  IsFlowControl,Handled: Boolean;

  procedure DoBoolDirective(bs: TBoolSwitch);
  begin
    if bs in AllowedBoolSwitches then
      begin
      Handled:=true;
      HandleBoolDirective(bs,Param);
      end
    else
      Handled:=false;
  end;

begin
  Result:=tkComment;
  P:=Pos(' ',ADirectiveText);
  If P=0 then
    begin
    P:=Pos(#9,ADirectiveText);
    If P=0 then
      P:=Length(ADirectiveText)+1;
    end;
  Directive:=Copy(ADirectiveText,2,P-2); // 1 is $
  Param:=ADirectiveText;
  Delete(Param,1,P);
  {$IFDEF VerbosePasDirectiveEval}
  Writeln('TPascalScanner.HandleDirective.Directive: "',Directive,'", Param : "',Param,'"');
  {$ENDIF}
  Handled:=true;
  IsFlowControl:=True;
  Case UpperCase(Directive) of
  'IFDEF':
     HandleIFDEF(Param);
  'IFNDEF':
     HandleIFNDEF(Param);
  'IFOPT':
     HandleIFOPT(Param);
  'IFC',
  'IF':
     HandleIF(Param,UpperCase(Directive)='IFC');
  'ELIFC',
  'ELSEIF':
     HandleELSEIF(Param,UpperCase(Directive)='ELIFC');
  'ELSEC',
  'ELSE':
     HandleELSE(Param);
  'ENDC',
  'ENDIF':
    HandleENDIF(Param);
  'IFEND':
    HandleENDIF(Param);
  else
    if PPIsSkipping then exit;
    IsFlowControl:=False;
    Handled:=false;
    if (length(Directive)=2)
        and (Directive[1] in ['a'..'z','A'..'Z'])
        and (Directive[2] in ['-','+']) then
      begin
      Handled:=true;
      Result:=HandleLetterDirective(Directive[1],Directive[2]='+');
      end;

    if not Handled then
      begin
      Handled:=true;
      Param:=Trim(Param);
      Case UpperCase(Directive) of
      'ASSERTIONS':
        DoBoolDirective(bsAssertions);
      'DEFINE',
      'DEFINEC',
      'SETC':
        HandleDefine(Param);
      'GOTO':
        DoBoolDirective(bsGoto);
      'DISPATCHFIELD':
        HandleDispatchField(Param,vsDispatchField);
      'DISPATCHSTRFIELD':
        HandleDispatchField(Param,vsDispatchStrField);
      'ERROR':
        HandleError(Param);
      'HINT':
        DoLog(mtHint,nUserDefined,SUserDefined,[Param]);
      'HINTS':
        DoBoolDirective(bsHints);
      'I','INCLUDE':
        Result:=HandleInclude(Param);
      'INCLUDESTRING','INCLUDESTRINGFILE':
        begin
        HandleIncludeString(Param);
        Result:=tkString;
        end;
      'INTERFACES':
        HandleInterfaces(Param);
      'LONGSTRINGS':
        DoBoolDirective(bsLongStrings);
      'LINKLIB':
        HandleLinkLib(Param);
      'MACRO':
        DoBoolDirective(bsMacro);
      'MESSAGE':
        HandleMessageDirective(Param);
      'MODE':
        HandleMode(Param);
      'MODESWITCH':
        HandleModeSwitch(Param);
      'MULTILINESTRINGLINEENDING':
        HandleMultilineStringLineEnding(Param);
      'MULTILINESTRINGTRIMLEFT':
        HandleMultilineStringTrimLeft(Param);
      'NOTE':
        DoLog(mtNote,nUserDefined,SUserDefined,[Param]);
      'NOTES':
        DoBoolDirective(bsNotes);
      'OBJECTCHECKS':
        DoBoolDirective(bsObjectChecks);
      'OPTIMIZATION':
        HandleOptimizations(Param);
      'OVERFLOWCHECKS','OV':
        DoBoolDirective(bsOverflowChecks);
      'POINTERMATH':
        DoBoolDirective(bsPointerMath);
      'R' :
        if not (po_DisableResources in Options) then
          HandleResource(Param);
      'RANGECHECKS':
        DoBoolDirective(bsRangeChecks);
      'SCOPEDENUMS':
        DoBoolDirective(bsScopedEnums);
      'TEXTBLOCK':
        HandleTextBlock(Param);
      'TYPEDADDRESS':
        DoBoolDirective(bsTypedAddress);
      'TYPEINFO':
        DoBoolDirective(bsTypeInfo);
      'UNDEF':
        HandleUnDefine(Param);
      'WARN':
        HandleWarn(Param);
      'WARNING':
        DoLog(mtWarning,nUserDefined,SUserDefined,[Param]);
      'WARNINGS':
        DoBoolDirective(bsWarnings);
      'WRITEABLECONST':
        DoBoolDirective(bsWriteableConst);
      'ALIGN',
      'CALLING',
      'INLINE',
      'PACKRECORDS',
      'PACKENUM' : ;
      else
        Handled:=false;
      end;
      end;
  end;
  if (Not IsFlowControl) or OnDirectiveForConditionals then
    DoHandleDirective(Self,Directive,Param,Handled);
  if not (Handled or IsFlowControl) then // in case of flowcontrol, it is definitely handled
    if LogEvent(sleDirective) then
      DoLog(mtWarning,nWarnIllegalCompilerDirectiveX,sWarnIllegalCompilerDirectiveX,
        [Directive]);
end;

function TPascalScanner.HandleLetterDirective(Letter: AnsiChar; Enable: boolean): TToken;
var
  bs: TBoolSwitch;
begin
  Result:=tkComment;
  Letter:=upcase(Letter);
  bs:=LetterToBoolSwitch[Letter];
  if bs=bsNone then
    DoLog(mtWarning,nWarnIllegalCompilerDirectiveX,sWarnIllegalCompilerDirectiveX,
      [Letter]);
  if not (bs in AllowedBoolSwitches) then
    begin
    DoLog(mtWarning,nWarnIllegalCompilerDirectiveX,sWarnIllegalCompilerDirectiveX,
      [Letter]);
    end;
  if (bs in FCurrentBoolSwitches)<>Enable then
    begin
    if bs in FReadOnlyBoolSwitches then
      begin
      DoLog(mtWarning,nWarnIllegalCompilerDirectiveX,sWarnIllegalCompilerDirectiveX,
        [Letter+BoolToStr(Enable,'+','-')]);
      exit;
      end;
    if Enable then
      begin
      AddDefine(LetterSwitchNames[Letter]);
      Include(FCurrentBoolSwitches,bs);
      end
    else
      begin
      UnDefine(LetterSwitchNames[Letter]);
      Exclude(FCurrentBoolSwitches,bs);
      end;
    end;
end;

procedure TPascalScanner.HandleBoolDirective(bs: TBoolSwitch;
  const Param: TPasScannerString);
var
  NewValue: Boolean;

begin
  if CompareText(Param,'on')=0 then
    NewValue:=true
  else if CompareText(Param,'off')=0 then
    NewValue:=false
  else
    begin
    NewValue:=True;// Fool compiler
    Error(nErrXExpectedButYFound,SErrXExpectedButYFound,['on',Param]);
    end;
  if (bs in CurrentBoolSwitches)=NewValue then exit;
  if bs in ReadOnlyBoolSwitches then
    DoLog(mtWarning,nWarnIllegalCompilerDirectiveX,sWarnIllegalCompilerDirectiveX,
      [BoolSwitchNames[bs]])
  else if NewValue then
    CurrentBoolSwitches:=CurrentBoolSwitches+[bs]
  else
    CurrentBoolSwitches:=CurrentBoolSwitches-[bs];
end;

procedure TPascalScanner.DoHandleComment(Sender: TObject; const aComment: TPasScannerString);
begin
  if Assigned(OnComment) then
    OnComment(Sender,aComment);
end;

procedure TPascalScanner.DoHandleDirective(Sender: TObject; Directive,
  Param: TPasScannerString; var Handled: boolean);
var
  i: Integer;
begin
  i:=IndexOfDirectiveHandle(Directive);
  if i>=0 then
    FDirectiveHandles[i].Handler(Sender,Directive,Param,Handled);
  if Assigned(OnDirective) then
    begin
    OnDirective(Sender,Directive,Param,Handled);
    if Handled then exit;
    end;
end;

procedure TPascalScanner.HandleMultilineStringTrimLeft(const AParam: TPasScannerString);

Var
  S : TPasScannerString;
  i : integer;

begin
  S:=UpperCase(Trim(aParam));
  Case UpperCase(S) of
    'ALL'  : I:=-2;
    'AUTO' : I:=-1;
    'NONE' : I:=0;
  else
    If not TryStrToInt(S,I) then
      Error(nErrInvalidMultiLineTrimLeft,SErrInvalidMultiLineTrimLeft,[aParam])
    else if (I<0) or (I>65535) then
      Error(nErrInvalidMultiLineTrimLeft,SErrInvalidMultiLineTrimLeft,[aParam]);
  end;
  MultilineStringsTrimLeft:=I;
end;

procedure TPascalScanner.HandleTextBlock(const AParam: TPasScannerString);

Var
  S : TEOLStyle;
  P : integer;
  Parm : TPasScannerString;

begin
  Parm:=UpperCase(Trim(aParam));
  P:=Pos(' ',Parm);
  if P>1 then
    Parm:=Copy(Parm,1,P-1);
  Case Parm of
    'CR' : s:=elCR;
    'LF' : s:=elLF;
    'CRLF' : s:=elCRLF;
    'NATIVE' : s:=elPlatform;
  else
    Error(nErrInvalidMultiLineLineEnding,sErrInvalidMultiLineLineEnding);
  end;
  MultilineStringsEOLStyle:=S;
end;

procedure TPascalScanner.HandleMultilineStringLineEnding(const AParam: TPasScannerString);

Var
  S : TEOLStyle;

begin
  Case UpperCase(Trim(aParam)) of
    'CR' : s:=elCR;
    'LF' : s:=elLF;
    'CRLF' : s:=elCRLF;
    'SOURCE' : s:=elSource;
    'PLATFORM' : s:=elPlatform;
  else
    Error(nErrInvalidMultiLineLineEnding,sErrInvalidMultiLineLineEnding);
  end;
  MultilineStringsEOLStyle:=S;
end;

function TPascalScanner.HandleMultilineCommentOldStyle: TToken;

var
  {$ifdef UsePChar}
  TokenStart: PAnsiChar;
  OldLength: integer;
  LE: String[2];
  I : Integer;
  {$else}
  TokenStart: Integer;
  s: String;
  l: integer;
  {$endif}
  SectionLength, NestingLevel: Integer;

  function FetchLocalLine: boolean; inline;
  begin
    Result:=FetchLine;
    {$ifndef UsePChar}
    if not Result then exit;
    s:=FCurLine;
    l:=length(s);
    {$endif}
  end;

begin
  {$ifdef UsePChar}
  LE:=LineEnding;
  {$endif}
  // Old-style multi-line comment
  Inc(FTokenPos);
  TokenStart := FTokenPos;
  FCurTokenString := '';
  {$ifdef UsePChar}
  OldLength := 0;
  {$endif}
  NestingLevel:=0;
  repeat
    if {$ifdef UsePChar}FTokenPos[0] = #0{$else}FTokenPos>l{$endif} then
      begin
      SectionLength:=FTokenPos - TokenStart;
      {$ifdef UsePChar}
      SetLength(FCurTokenString, OldLength + SectionLength + length(LE)); // Corrected JC
      if SectionLength > 0 then
        Move(TokenStart^, FCurTokenString[OldLength + 1],SectionLength);
      Inc(OldLength, SectionLength);
      for I:=1 to Length(LE) do
        begin
        Inc(OldLength);
        FCurTokenString[OldLength] := LE[i];
        end;
      {$else}
      FCurTokenString:=FCurTokenString+copy(FCurLine,TokenStart,SectionLength)+LineEnding; // Corrected JC
      {$endif}
      if not FetchLocalLine then
        begin
        Result := tkEOF;
        FCurToken := Result;
        exit;
        end;
      TokenStart:=FTokenPos;
      end
    else if {$ifdef UsePChar}(FTokenPos[0] = '*') and (FTokenPos[1] = ')')
        {$else}(FTokenPos<l) and (s[FTokenPos]='*') and (s[FTokenPos+1]=')'){$endif}
      then begin
      dec(NestingLevel);
      if NestingLevel<0 then
        break;
      inc(FTokenPos,2);
      end
    else if (msNestedComment in CurrentModeSwitches)
        and {$ifdef UsePChar}(FTokenPos[0] = '(') and (FTokenPos[1] = '*')
        {$else}(FTokenPos<l) and (s[FTokenPos]='(') and (s[FTokenPos+1]='*'){$endif}
      then begin
      inc(FTokenPos,2);
      Inc(NestingLevel);
      end
    else
      Inc(FTokenPos);
  until false;
  SectionLength := FTokenPos - TokenStart;
  {$ifdef UsePChar}
  SetLength(FCurTokenString, OldLength + SectionLength);
  if SectionLength > 0 then
    Move(TokenStart^, FCurTokenString[OldLength + 1], SectionLength);
  {$else}
  FCurTokenString:=FCurTokenString+copy(FCurLine,TokenStart,SectionLength);
  {$endif}
  Inc(FTokenPos, 2);
  Result := tkComment;
  if Copy(CurTokenString,1,1)='$' then
    Result := HandleDirective(CurTokenString)
  else
    DoHandleComment(Self,CurTokenString);
end;


function TPascalScanner.HandleMultilineComment: TToken;

var
  {$ifdef UsePChar}
  TokenStart: PAnsiChar;
  OldLength: integer;
  I : Integer;
  LE: String[2];
  {$else}
  TokenStart: Integer;
  s: String;
  l: integer;
  {$endif}
  SectionLength, NestingLevel: Integer;

  function FetchLocalLine: boolean; inline;
  begin
    Result:=FetchLine;
    {$ifndef UsePChar}
    if not Result then exit;
    s:=FCurLine;
    l:=length(s);
    {$endif}
  end;

begin
  Inc(FTokenPos);
  TokenStart := FTokenPos;
  FCurTokenString := '';
  {$ifdef UsePChar}
  LE:=LineEnding;
  OldLength := 0;
  {$else}
  s:=FCurLine;
  l:=length(FCurLine);
  {$endif}
  NestingLevel := 0;

  repeat
    if {$ifdef UsePChar}FTokenPos[0] = #0{$else}FTokenPos>l{$endif} then
      begin
      SectionLength := FTokenPos - TokenStart;
      {$ifdef UsePChar}
      SetLength(FCurTokenString, OldLength + SectionLength + length(LE)); // Corrected JC
      if SectionLength > 0 then
        Move(TokenStart^, FCurTokenString[OldLength + 1],SectionLength);

      // Corrected JC: Append the correct lineending
      Inc(OldLength, SectionLength);
      for I:=1 to length(LE) do
        begin
          Inc(OldLength);
          FCurTokenString[OldLength] := LE[i];
        end;
      {$else}
      FCurTokenString:=FCurTokenString+copy(S,TokenStart,SectionLength)+LineEnding; // Corrected JC
      {$endif}
      if not FetchLocalLine then
      begin
        Result := tkEOF;
        FCurToken := Result;
        exit;
      end;
      TokenStart := FTokenPos;
      end
    else if {$ifdef UsePChar}(FTokenPos[0] = '}'){$else}(s[FTokenPos]='}'){$endif} then
      begin
      Dec(NestingLevel);
      if NestingLevel<0 then
        break;
      Inc(FTokenPos);
      end
    else if {$ifdef UsePChar}(FTokenPos[0] = '{'){$else}(s[FTokenPos]='{'){$endif}
        and (msNestedComment in CurrentModeSwitches) then
      begin
      inc(FTokenPos);
      Inc(NestingLevel);
      end
    else
      Inc(FTokenPos);
  until false;
  SectionLength := FTokenPos - TokenStart;
  {$ifdef UsePChar}
  SetLength(FCurTokenString, OldLength + SectionLength);
  if SectionLength > 0 then
    Move(TokenStart^, FCurTokenString[OldLength + 1], SectionLength);
  {$else}
  FCurTokenString:=FCurTokenString+copy(s,TokenStart,SectionLength);
  {$endif}
  Inc(FTokenPos);
  Result := tkComment;
  if (length(CurTokenString)>0) and (CurTokenString[1]='$') then
    Result:=HandleDirective(CurTokenString)
  else
    DoHandleComment(Self, CurTokenString)
end;

function TPascalScanner.DoFetchToken: TToken;

var
  TokenStart: {$ifdef UsePChar}PAnsiChar{$else}integer{$endif};
  i: TToken;
  QuoteLen, SectionLength, Index: Integer;
  {$ifdef UsePChar}
  //
  {$else}
  s: TPasScannerString;
  l: integer;
  {$endif}

  procedure FetchCurTokenString; inline;
  begin
    {$ifdef UsePChar}
    SetLength(FCurTokenString, SectionLength);
    if SectionLength > 0 then
      Move(TokenStart^, FCurTokenString[1], SectionLength);
    {$else}
    FCurTokenString:=copy(FCurLine,TokenStart,SectionLength);
    {$endif}
  end;

  function FetchLocalLine: boolean; inline;
  begin
    Result:=FetchLine;
    {$ifndef UsePChar}
    if not Result then exit;
    s:=FCurLine;
    l:=length(s);
    {$endif}
  end;

  {$ifdef UsePChar}
  Function IsDelphiMultiLine (out QuoteLen : integer): Boolean;
  var
    P : PAnsiChar;
  begin
    P:=FTokenPos;
    QuoteLen:=0;
    While P[0]<>#0 do
      begin
      inc(QuoteLen);
      if P[0]<>SingleQuote then
        Exit(false);
      Inc(P);
      end;
    Result:=(P[0]=#0) and (QuoteLen>2) and ((QuoteLen mod 2) = 1);
  end;
  {$ELSE}
  Function IsDelphiMultiLine(out Quotelen : integer) : Boolean;
  var
    P : Integer;
  begin
    P:=FTokenPos;
    QuoteLen:=0;
    While (P<=L) do
      begin
      inc(QuoteLen);
      if (S[P]<>SingleQuote) then
        Exit(false);
      Inc(P);
      end;
    // Accessing single char is more expensive than a copy
    Result:=(P>L) and (QuoteLen>2) and ((QuoteLen mod 2) = 1);
  end;
  {$ENDIF}

begin
  FCurtokenEscaped:=False;
  TokenStart:={$ifdef UsePChar}nil{$else}0{$endif};
  Result:=tkLineEnding;
  if FTokenPos {$ifdef UsePChar}= nil{$else}<1{$endif} then
    if not FetchLine then
      begin
      Result := tkEOF;
      FCurToken := Result;
      FCurTokenString := '';
      exit;
      end;
  FCurTokenString := '';
  FCurTokenPos.FileName:=CurFilename;
  FCurTokenPos.Row:=CurRow;
  FCurTokenPos.Column:=CurColumn;
  {$ifndef UsePChar}
  s:=FCurLine;
  l:=length(s);
  if FTokenPos>l then
    begin
    FetchLine;
    Result := tkLineEnding;
    FCurToken := Result;
    exit;
    end;
  {$endif}
  case {$ifdef UsePChar}FTokenPos[0]{$else}s[FTokenPos]{$endif} of
    {$ifdef UsePChar}
    #0:         // Empty line
      begin
      FetchLine;
      Result := tkLineEnding;
      end;
    {$endif}
    ' ':
      begin
      Result := tkWhitespace;
      repeat
        Inc(FTokenPos);
        if {$ifdef UsePChar}FTokenPos[0] = #0{$else}FTokenPos>l{$endif} then
          if not FetchLocalLine then
            begin
            FCurToken := Result;
            exit;
            end;
      until not ({$ifdef UsePChar}FTokenPos[0]{$else}s[FTokenPos]{$endif}=' ');
      end;
    #9:
      begin
      Result := tkTab;
      repeat
        Inc(FTokenPos);
        if {$ifdef UsePChar}FTokenPos[0] = #0{$else}FTokenPos>l{$endif} then
          if not FetchLocalLine then
            begin
            FCurToken := Result;
            exit;
            end;
      until not ({$ifdef UsePChar}FTokenPos[0]{$else}s[FTokenPos]{$endif}=#9);
      end;
    '#':
      Result:=DoFetchTextToken;
    '''':
      if (msDelphiMultiLineStrings in CurrentModeSwitches) and IsDelphiMultiLine(Quotelen) then
        Result:=DoFetchDelphiMultiLineTextToken(Quotelen)
      else
        Result:=DoFetchTextToken;
    '`' :
      begin
      If not (msMultiLineStrings in CurrentModeSwitches) then
         Error(nErrInvalidCharacter, SErrInvalidCharacter,
        [{$ifdef UsePChar}FTokenPos[0]{$else}s[FTokenPos]{$endif}]);
      Result:=DoFetchMultilineTextToken;
      end;
    '&':
      begin
      TokenStart := FTokenPos;
      repeat
        Inc(FTokenPos);
      until {$ifdef UsePChar}not (FTokenPos[0] in ['0'..'7']){$else}(FTokenPos>l) or not (s[FTokenPos] in ['0'..'7']){$endif};
      SectionLength := FTokenPos - TokenStart;
      if (SectionLength=1)
          and ({$ifdef UsePChar}FTokenPos^{$else}s[FTokenPos]{$endif} in IdentChars) then
        begin
        // &Keyword
        DoFetchToken();
        Result:=tkIdentifier;
        FCurtokenEscaped:=True;
        end
      else
        begin
        FetchCurTokenString;
        Result := tkNumber;
        end;
      end;
    '$':
      begin
      TokenStart := FTokenPos;
      repeat
        Inc(FTokenPos);
      until {$ifdef UsePChar}not (FTokenPos[0] in HexDigits){$else}(FTokenPos>l) or not (s[FTokenPos] in HexDigits){$endif};
      SectionLength := FTokenPos - TokenStart;
      FetchCurTokenString;
      Result := tkNumber;
      end;
    '%':
      begin
      TokenStart := FTokenPos;
      repeat
        Inc(FTokenPos);
      until {$ifdef UsePChar}not (FTokenPos[0] in ['0','1']){$else}(FTokenPos>l) or not (s[FTokenPos] in ['0','1']){$endif};
      SectionLength := FTokenPos - TokenStart;
      FetchCurTokenString;
      Result := tkNumber;
      end;
    '(':
      begin
      Inc(FTokenPos);
      if {$ifdef UsePChar}FTokenPos[0] = '.'{$else}(FTokenPos<=l) and (s[FTokenPos]='.'){$endif} then
        begin
        Inc(FTokenPos);
        Result := tkSquaredBraceOpen;
        end
      else if {$ifdef UsePChar}FTokenPos[0] <> '*'{$else}(FTokenPos>l) or (s[FTokenPos]<>'*'){$endif} then
        Result := tkBraceOpen
      else
        Result:=HandleMultilineCommentOldStyle;
      end;
    ')':
      begin
      Inc(FTokenPos);
      Result := tkBraceClose;
      end;
    '*':
      begin
      Result:=tkMul;
      Inc(FTokenPos);
      if {$ifdef UsePChar}FTokenPos[0]='*'{$else}(FTokenPos<=l) and (s[FTokenPos]='*'){$endif} then
        begin
        Inc(FTokenPos);
        Result := tkPower;
        end
      else if (po_CAssignments in options) then
        begin
        if {$ifdef UsePChar}FTokenPos[0]='='{$else}(FTokenPos<=l) and (s[FTokenPos]='='){$endif} then
          begin
          Inc(FTokenPos);
          Result:=tkAssignMul;
          end;
        end;
      end;
    '+':
      begin
      Result:=tkPlus;
      Inc(FTokenPos);
      if (po_CAssignments in options) then
        begin
        if {$ifdef UsePChar}FTokenPos[0]='='{$else}(FTokenPos<=l) and (s[FTokenPos]='='){$endif} then
          begin
          Inc(FTokenPos);
          Result:=tkAssignPlus;
          end;
        end
      end;
    ',':
      begin
      Inc(FTokenPos);
      Result := tkComma;
      end;
    '-':
      begin
      Result := tkMinus;
      Inc(FTokenPos);
      if (po_CAssignments in options) then
        begin
        if {$ifdef UsePChar}FTokenPos[0]='='{$else}(FTokenPos<=l) and (s[FTokenPos]='='){$endif} then
          begin
          Inc(FTokenPos);
          Result:=tkAssignMinus;
          end;
        end
      end;
    '.':
      begin
      Inc(FTokenPos);
      if {$ifdef UsePChar}FTokenPos[0]=')'{$else}(FTokenPos<=l) and (s[FTokenPos]=')'){$endif} then
        begin
        Inc(FTokenPos);
        Result := tkSquaredBraceClose;
        end
      else if {$ifdef UsePChar}FTokenPos[0]='.'{$else}(FTokenPos<=l) and (s[FTokenPos]='.'){$endif} then
        begin
        Inc(FTokenPos);
        if {$ifdef UsePChar}FTokenPos[0]='.'{$else}(FTokenPos<=l) and (s[FTokenPos]='.'){$endif} then
          begin
          Inc(FTokenPos);
          Result:=tkDotDotDot;
          end
        else
          Result := tkDotDot;
        end
      else
        Result := tkDot;
      end;
    '/':
      begin
      Result := tkDivision;
      Inc(FTokenPos);
      if {$ifdef UsePChar}FTokenPos[0]='/'{$else}(FTokenPos<=l) and (s[FTokenPos]='/'){$endif} then
        begin
        // Single-line comment
        Inc(FTokenPos);
        TokenStart := FTokenPos;
        FCurTokenString := '';
        while {$ifdef UsePChar}FTokenPos[0] <> #0{$else}(FTokenPos<=l) and (s[FTokenPos]<>#0){$endif} do
          Inc(FTokenPos);
        SectionLength := FTokenPos - TokenStart;
        FetchCurTokenString;
        // Handle macro which is //
        if FCurSourceFile is TMacroReader then
          begin
          // exhaust till eof of macro stream
          Repeat
            I:=Fetchtoken;
          until (i<>tkLineEnding);
          FetchLocalLine;
          end;
        Result := tkComment;
        end
      else if (po_CAssignments in options) then
        begin
        if {$ifdef UsePChar}FTokenPos[0]='='{$else}(FTokenPos<=l) and (s[FTokenPos]='='){$endif} then
          begin
          Inc(FTokenPos);
          Result:=tkAssignDivision;
          end;
        end
      end;
    '0'..'9':
      begin
      // 1, 12, 1.2, 1.2E3, 1.E2, 1E2, 1.2E-3, 1E+2 and .)
      // beware of 1..2
      TokenStart := FTokenPos;
      repeat
        Inc(FTokenPos);
      until {$ifdef UsePChar}not (FTokenPos[0] in Digits){$else}(FTokenPos>l) or not (s[FTokenPos] in Digits){$endif};
      if {$ifdef UsePChar}(FTokenPos[0]='.') and (FTokenPos[1]<>'.') and (FTokenPos[1]<>')'){$else}
          (FTokenPos<=l) and (s[FTokenPos]='.') and ((FTokenPos=l) or ((s[FTokenPos+1]<>'.') and (s[FTokenPos+1]<>')'))){$endif}then
        begin
        inc(FTokenPos);
        while {$ifdef UsePChar}FTokenPos[0] in Digits{$else}(FTokenPos<=l) and (s[FTokenPos] in Digits){$endif} do
          Inc(FTokenPos);
        end;
      if {$ifdef UsePChar}FTokenPos[0] in ['e', 'E']{$else}(FTokenPos<=l) and (s[FTokenPos] in ['e', 'E']){$endif} then
      begin
        Inc(FTokenPos);
        if {$ifdef UsePChar}FTokenPos[0] in ['-','+']{$else}(FTokenPos<=l) and (s[FTokenPos] in ['-','+']){$endif} then
          inc(FTokenPos);
        while {$ifdef UsePChar}FTokenPos[0] in Digits{$else}(FTokenPos<=l) and (s[FTokenPos] in Digits){$endif} do
          Inc(FTokenPos);
      end;
      SectionLength := FTokenPos - TokenStart;
      FetchCurTokenString;
      Result := tkNumber;
      end;
    ':':
      begin
      Inc(FTokenPos);
      if {$ifdef UsePChar}FTokenPos[0]='='{$else}(FTokenPos<=l) and (s[FTokenPos]='='){$endif} then
        begin
        Inc(FTokenPos);
        Result := tkAssign;
        end
      else
        Result := tkColon;
      end;
    ';':
      begin
      Inc(FTokenPos);
      Result := tkSemicolon;
      end;
    '<':
      begin
      Inc(FTokenPos);
      {$ifndef UsePChar}
      if FTokenPos>l then
        Result := tkLessThan
      else
      {$endif}
      case {$ifdef UsePChar}FTokenPos^{$else}s[FTokenPos]{$endif} of
      '>':
        begin
        Inc(FTokenPos);
        Result := tkNotEqual;
        end;
      '=':
        begin
        Inc(FTokenPos);
        Result := tkLessEqualThan;
        end;
      '<':
        begin
        Inc(FTokenPos);
        Result := tkshl;
        end;
      else
        Result := tkLessThan;
      end;
      end;
    '=':
      begin
      Inc(FTokenPos);
      Result := tkEqual;
      end;
    '>':
      begin
      Inc(FTokenPos);
      {$ifndef UsePChar}
      if FTokenPos>l then
        Result := tkGreaterThan
      else
      {$endif}
      case {$ifdef UsePChar}FTokenPos^{$else}s[FTokenPos]{$endif} of
      '=':
        begin
        Inc(FTokenPos);
        Result := tkGreaterEqualThan;
        end;
      '<':
        begin
        Inc(FTokenPos);
        Result := tkSymmetricalDifference;
        end;
      '>':
        begin
        Inc(FTokenPos);
        Result := tkshr;
        end;
      else
        Result := tkGreaterThan;
      end;
      end;
    '@':
      begin
      Inc(FTokenPos);
      Result := tkAt;
      if {$ifdef UsePChar}FTokenPos^='@'{$else}(FTokenPos<=l) and (s[FTokenPos]='@'){$endif} then
        begin
        Inc(FTokenPos);
        Result:=tkAtAt;
        end;
      end;
    '[':
      begin
      Inc(FTokenPos);
      Result := tkSquaredBraceOpen;
      end;
    ']':
      begin
      Inc(FTokenPos);
      Result := tkSquaredBraceClose;
      end;
    '^':
      begin
      if ForceCaret or PPisSkipping or
         (PreviousToken in [tkeof,tkTab,tkLineEnding,tkComment,tkIdentifier,
                   tkNil,tkOperator,tkBraceClose,tkSquaredBraceClose,tkCaret]) then
        begin
        Inc(FTokenPos);
        Result := tkCaret;
        end
      else
        Result:=DoFetchTextToken;
      end;
    '\':
      begin
      Inc(FTokenPos);
      Result := tkBackslash;
      end;
    '{':        // Multi-line comment
      begin
      // HandleMultilineComment calls Directive handling
      Result:=HandleMultilineComment;
      end;
    'A'..'Z', 'a'..'z', '_':
      begin
      TokenStart := FTokenPos;
      repeat
        Inc(FTokenPos);
      until {$ifdef UsePChar}not (FTokenPos[0] in IdentChars){$else}(FTokenPos>l) or not (s[FTokenPos] in IdentChars){$endif};
      SectionLength := FTokenPos - TokenStart;
      FetchCurTokenString;
      Result:=tkIdentifier;
      for i:=tkAbsolute to tkXor do
        begin
        if (CompareText(CurTokenString, TokenInfos[i])=0) then
          begin
          Result:=I;
          break;
          end;
        end;
      if (Result<>tkIdentifier) and (Result in FNonTokens) then
        Result:=tkIdentifier;
      FCurToken := Result;
      if MacrosOn then
        begin
        Index:=FMacros.IndexOf(CurTokenString);
        if Index>=0 then
          Result:=HandleMacro(Index);
        end;
      end;
  else
    if PPIsSkipping then
      Inc(FTokenPos)
    else
      Error(nErrInvalidCharacter, SErrInvalidCharacter,
        [{$ifdef UsePChar}FTokenPos[0]{$else}s[FTokenPos]{$endif}]);
  end;

  FCurToken := Result;
end;

function TPascalScanner.LogEvent(E: TPScannerLogEvent): Boolean;
begin
  Result:=E in FLogEvents;
end;

function TPascalScanner.GetCurColumn: Integer;
begin
  If {$ifdef UsePChar}(FTokenPos<>Nil){$else}FTokenPos>0{$endif} then
    Result := FTokenPos {$ifdef UsePChar}- PAnsiChar(CurLine){$else}-1{$endif} + FCurColumnOffset
  else
    Result := FCurColumnOffset;
end;

function TPascalScanner.GetCurrentValueSwitch(V: TValueSwitch): TPasScannerString;
begin
  Result:=FCurrentValueSwitches[V];
end;

function TPascalScanner.GetForceCaret: Boolean;
begin
  Result:=toForceCaret in FTokenOptions;
end;

function TPascalScanner.GetMacrosOn: boolean;
begin
  Result:=bsMacro in FCurrentBoolSwitches;
end;

function TPascalScanner.GetTokenString: TPasTreeString;
begin
{$IFDEF PAS2JS}
  Result:=RawCurTokenString;
{$ELSE}
{$IF SIZEOF(Char)=2}
  Result:=UTF8Decode(RawCurTokenString);
{$ELSE}
  Result:=RawCurTokenString;
{$ENDIF}
{$ENDIF}
end;

function TPascalScanner.IndexOfWarnMsgState(Number: integer; InsertPos: boolean
  ): integer;
var
  l, r, m, CurNumber: Integer;
begin
  l:=0;
  r:=length(FWarnMsgStates)-1;
  m:=0;
  while l<=r do
    begin
    m:=(l+r) div 2;
    CurNumber:=FWarnMsgStates[m].Number;
    if Number>CurNumber then
      l:=m+1
    else if Number<CurNumber then
      r:=m-1
    else
      exit(m);
    end;
  if not InsertPos then
    exit(-1);
  if length(FWarnMsgStates)=0 then
    exit(0);
  if (m<length(FWarnMsgStates)) and (FWarnMsgStates[m].Number<=Number) then
    inc(m);
  Result:=m;
end;

function TPascalScanner.OnCondEvalFunction(Sender: TCondDirectiveEvaluator;
  Name, Param: String; out Value: String): boolean;
begin
  {$IFDEF VerbosePasDirectiveEval}
  writeln('TPascalScanner.OnCondEvalFunction Func="',Name,'" Param="',Param,'"');
  {$ENDIF}
  if CompareText(Name,'defined')=0 then
    begin
    if not IsValidIdent(Param) then
      Sender.Log(mtError,nErrXExpectedButYFound,SErrXExpectedButYFound,
        ['identifier',Param]);
    Value:=CondDirectiveBool[IsDefined(Param)];
    exit(true);
    end
  else if CompareText(Name,'undefined')=0 then
    begin
    if not IsValidIdent(Param) then
      Sender.Log(mtError,nErrXExpectedButYFound,SErrXExpectedButYFound,
        ['identifier',Param]);
    Value:=CondDirectiveBool[not IsDefined(Param)];
    exit(true);
    end
  else if CompareText(Name,'option')=0 then
    begin
    if (length(Param)<>1) or not (Param[1] in ['a'..'z','A'..'Z']) then
      Sender.Log(mtError,nErrXExpectedButYFound,SErrXExpectedButYFound,
        ['letter',Param]);
    Value:=CondDirectiveBool[IfOpt(Param[1])];
    exit(true);
    end;
  // last check user hook
  if Assigned(OnEvalFunction) then
    begin
    Result:=OnEvalFunction(Sender,Name,Param,Value);
    if not (po_CheckCondFunction in Options) then
      begin
      Value:='0';
      Result:=true;
      end;
    exit;
    end;
  if (po_CheckCondFunction in Options) then
    begin
    Value:='';
    Result:=false;
    end
  else
    begin
    Value:='0';
    Result:=true;
    end;
end;

procedure TPascalScanner.OnCondEvalLog(Sender: TCondDirectiveEvaluator;
  Args: array of const);

Var
  Msg : TPasScannerString;

begin
  {$IFDEF VerbosePasDirectiveEval}
  writeln('TPascalScanner.OnCondEvalLog "',Sender.MsgPattern,'"');
  {$ENDIF}
  // ToDo: move CurLine/CurRow to Sender.MsgPos
  if Sender.MsgType<=mtError then
    begin
    SetCurMsg(Sender.MsgType,Sender.MsgNumber,Sender.MsgPattern,Args);
    Msg:=Format('%s(%d,%d) : %s',[FormatPath(FCurFileName),CurRow,CurColumn,FLastMsg]);
    raise EScannerError.Create(Msg);
    end
  else
    DoLog(Sender.MsgType,Sender.MsgNumber,Sender.MsgPattern,Args,true);
end;

function TPascalScanner.OnCondEvalVar(Sender: TCondDirectiveEvaluator;
  Name: String; out Value: String): boolean;
var
  i: Integer;
  M: TMacroDef;
begin
  {$IFDEF VerbosePasDirectiveEval}
  writeln('TPascalScanner.OnCondEvalVar "',Name,'"');
  {$ENDIF}
  // first check defines
  if FDefines.IndexOf(Name)>=0 then
    begin
    Value:='1';
    exit(true);
    end;
  // then check macros
  i:=FMacros.IndexOf(Name);
  if i>=0 then
    begin
    M:=FMacros.Objects[i] as TMacroDef;
    Value:=M.Value;
    exit(true);
    end;
  // last check user hook
  if Assigned(OnEvalVariable) then
    begin
    Result:=OnEvalVariable(Sender,Name,Value);
    exit;
    end;
  Value:='';
  Result:=false;
end;

procedure TPascalScanner.SetAllowedBoolSwitches(const AValue: TBoolSwitches);
begin
  if FAllowedBoolSwitches=AValue then Exit;
  FAllowedBoolSwitches:=AValue;
end;

procedure TPascalScanner.SetAllowedModeSwitches(const AValue: TModeSwitches);
begin
  if FAllowedModeSwitches=AValue then Exit;
  FAllowedModeSwitches:=AValue;
  CurrentModeSwitches:=FCurrentModeSwitches*AllowedModeSwitches;
end;

procedure TPascalScanner.SetAllowedValueSwitches(const AValue: TValueSwitches);
begin
  if FAllowedValueSwitches=AValue then Exit;
  FAllowedValueSwitches:=AValue;
end;

procedure TPascalScanner.SetCurrentBoolSwitches(const AValue: TBoolSwitches);
var
  OldBS, Removed, Added: TBoolSwitches;
begin
  if FCurrentBoolSwitches=AValue then Exit;
  OldBS:=FCurrentBoolSwitches;
  FCurrentBoolSwitches:=AValue;
  Removed:=OldBS-FCurrentBoolSwitches;
  Added:=FCurrentBoolSwitches-OldBS;
  if bsGoto in Added then
    begin
    UnsetNonToken(tklabel);
    UnsetNonToken(tkgoto);
    end;
  if bsGoto in Removed then
    begin
    SetNonToken(tklabel);
    SetNonToken(tkgoto);
    end;
end;

procedure TPascalScanner.SetCurrentModeSwitches(AValue: TModeSwitches);
var
  Old, AddedMS, RemovedMS: TModeSwitches;
begin
  AValue:=AValue*AllowedModeSwitches;
  if FCurrentModeSwitches=AValue then Exit;
  Old:=FCurrentModeSwitches;
  FCurrentModeSwitches:=AValue;
  AddedMS:=FCurrentModeSwitches-Old;
  RemovedMS:=Old-FCurrentModeSwitches;
  if msDefaultUnicodestring in AddedMS then
    begin
    AddDefine('UNICODE');
    AddDefine('FPC_UNICODESTRINGS');
    end
  else if msDefaultUnicodestring in RemovedMS then
    begin
    UnDefine('UNICODE');
    UnDefine('FPC_UNICODESTRINGS');
    end;
  if msDefaultAnsistring in AddedMS then
    begin
    AddDefine(LetterSwitchNames['H'],true);
    Include(FCurrentBoolSwitches,bsLongStrings);
    end
  else if msDefaultAnsistring in RemovedMS then
    begin
    UnDefine(LetterSwitchNames['H'],true);
    Exclude(FCurrentBoolSwitches,bsLongStrings);
    end;
  if ([msObjectiveC1,msObjectiveC2] * FCurrentModeSwitches) = [] then
    begin
    SetNonToken(tkobjcclass);
    SetNonToken(tkobjcprotocol);
    SetNonToken(tkobjccategory);
    end
  else
    begin
    UnSetNonToken(tkobjcclass);
    UnSetNonToken(tkobjcprotocol);
    UnSetNonToken(tkobjccategory);
    end
end;

procedure TPascalScanner.SetCurrentValueSwitch(V: TValueSwitch;
  const AValue: TPasScannerString);
begin
  if not (V in AllowedValueSwitches) then exit;
  if FCurrentValueSwitches[V]=AValue then exit;
  FCurrentValueSwitches[V]:=AValue;
end;

procedure TPascalScanner.SetWarnMsgState(Number: integer; State: TWarnMsgState);

  {$IFDEF EmulateArrayInsert}
  procedure Delete(var A: TWarnMsgNumberStateArr; Index, Count: integer); overload;
  var
    i: Integer;
  begin
    if Index<0 then
      Error(nErrDivByZero,'[20180627142123]');
    if Index+Count>length(A) then
      Error(nErrDivByZero,'[20180627142127]');
    for i:=Index+Count to length(A)-1 do
      A[i-Count]:=A[i];
    SetLength(A,length(A)-Count);
  end;

  procedure Insert(Item: TWarnMsgNumberState; var A: TWarnMsgNumberStateArr; Index: integer); overload;
  var
    i: Integer;
  begin
    if Index<0 then
      Error(nErrDivByZero,'[20180627142133]');
    if Index>length(A) then
      Error(nErrDivByZero,'[20180627142137]');
    SetLength(A,length(A)+1);
    for i:=length(A)-1 downto Index+1 do
      A[i]:=A[i-1];
    A[Index]:=Item;
  end;
  {$ENDIF}

var
  i: Integer;
  Item: TWarnMsgNumberState;
begin
  i:=IndexOfWarnMsgState(Number,true);
  if (i<length(FWarnMsgStates)) and (FWarnMsgStates[i].Number=Number) then
    begin
    // already exists
    if State=wmsDefault then
      Delete(FWarnMsgStates,i,1)
    else
      FWarnMsgStates[i].State:=State;
    end
  else if State<>wmsDefault then
    begin
    // new state
    Item.Number:=Number;
    Item.State:=State;
    Insert(Item,FWarnMsgStates,i);
    end;
end;

function TPascalScanner.GetWarnMsgState(Number: integer): TWarnMsgState;
var
  i: Integer;
begin
  i:=IndexOfWarnMsgState(Number,false);
  if i<0 then
    Result:=wmsDefault
  else
    Result:=FWarnMsgStates[i].State;
end;

procedure TPascalScanner.SetMacrosOn(const AValue: boolean);
begin
  if AValue then
    Include(FCurrentBoolSwitches,bsMacro)
  else
    Exclude(FCurrentBoolSwitches,bsMacro);
end;

procedure TPascalScanner.DoLog(MsgType: TMessageType; MsgNumber: integer;
  const Msg: TPasScannerString; SkipSourceInfo: Boolean);
begin
  DoLog(MsgType,MsgNumber,Msg,[],SkipSourceInfo);
end;

procedure TPascalScanner.DoLog(MsgType: TMessageType; MsgNumber: integer;
  const Fmt: TPasScannerString; Args: array of const;
  SkipSourceInfo: Boolean);

Var
  Msg : TPasScannerString;

begin
  if IgnoreMsgType(MsgType) then exit;
  SetCurMsg(MsgType,MsgNumber,Fmt,Args);
  If Assigned(FOnLog) then
    begin
    Msg:=MessageTypeNames[MsgType]+': ';
    if SkipSourceInfo then
      Msg:=Msg+FLastMsg
    else
      Msg:=Msg+Format('%s(%d,%d) : %s',[FormatPath(FCurFileName),CurRow,CurColumn,FLastMsg]);
    FOnLog(Self,Msg);
    end;
end;


procedure TPascalScanner.SetOptions(AValue: TPOptions);

Var
  isModeSwitch : Boolean;

begin
  if FOptions=AValue then Exit;
  // Change of mode ?
  IsModeSwitch:=(po_delphi in Avalue) <> (po_delphi in FOptions);
  FOptions:=AValue;
  if isModeSwitch then
    if (po_delphi in FOptions) then
      CurrentModeSwitches:=DelphiModeSwitches
    else
      CurrentModeSwitches:=FPCModeSwitches
end;

procedure TPascalScanner.SetReadOnlyBoolSwitches(const AValue: TBoolSwitches);
begin
  if FReadOnlyBoolSwitches=AValue then Exit;
  FReadOnlyBoolSwitches:=AValue;
end;

procedure TPascalScanner.SetReadOnlyModeSwitches(const AValue: TModeSwitches);
begin
  if FReadOnlyModeSwitches=AValue then Exit;
  FReadOnlyModeSwitches:=AValue;
  FAllowedModeSwitches:=FAllowedModeSwitches+FReadOnlyModeSwitches;
  FCurrentModeSwitches:=FCurrentModeSwitches+FReadOnlyModeSwitches;
end;

procedure TPascalScanner.SetReadOnlyValueSwitches(const AValue: TValueSwitches);
begin
  if FReadOnlyValueSwitches=AValue then Exit;
  FReadOnlyValueSwitches:=AValue;
end;

function TPascalScanner.IndexOfResourceHandler(const aExt: TPasScannerString): Integer;

begin
  Result:=Length(FResourceHandlers)-1;
  While (Result>=0) and (FResourceHandlers[Result].Ext<>aExt) do
    Dec(Result);
end;

function TPascalScanner.FindResourceHandler(const aExt: TPasScannerString): TResourceHandler;

Var
  Idx : Integer;

begin
  Idx:=IndexOfResourceHandler(aExt);
  if Idx=-1 then
    Result:=Nil
  else
    Result:=FResourceHandlers[Idx].handler;
end;

function TPascalScanner.IndexOfDirectiveHandle(const aDirective: TPasScannerString;
  ForInsert: boolean): Integer;
var
  l, r, m, cmp: Integer;
begin
  l:=0;
  r:=length(FDirectiveHandles)-1;
  m:=0;
  while l<=r do begin
    m:=(l+r) div 2;
    cmp:=CompareText(aDirective,FDirectiveHandles[m].Directive);
    if cmp>0 then
      l:=m+1
    else if cmp<0 then
      r:=m-1
    else
      exit(m);
  end;
  if not ForInsert then exit(-1);
  Result:=m;
  if length(FDirectiveHandles)=0 then
    exit;
  if cmp>0 then
    inc(Result);
end;

function TPascalScanner.ReadIdentifier(const AParam: TPasScannerString): TPasScannerString;
var
  p, l: Integer;
begin
  p:=1;
  l:=length(AParam);
  while (p<=l) and (AParam[p] in IdentChars) do inc(p);
  Result:=LeftStr(AParam,p-1);
end;

function TPascalScanner.FetchLine: boolean;
begin
  if CurSourceFile.IsEOF then
  begin
    if {$ifdef UsePChar}FTokenPos<>nil{$else}FTokenPos>0{$endif} then
      begin
      FCurLine := '';
      FTokenPos := {$ifdef UsePChar}nil{$else}-1{$endif};
      inc(FCurRow); // set CurRow to last line+1
      inc(FModuleRow);
      FCurColumnOffset:=1;
      end;
    Result := false;
  end else
  begin
    FCurLine := CurSourceFile.ReadLine;
    FTokenPos := {$ifdef UsePChar}PAnsiChar(CurLine){$else}1{$endif};
    Result := true;
    {$ifdef UseAnsiStrings}
    if (FCurRow = 0)
    and (Length(CurLine) >= 3)
    and (FTokenPos[0] = #$EF)
    and (FTokenPos[1] = #$BB)
    and (FTokenPos[2] = #$BF) then
      // ignore UTF-8 Byte Order Mark
      inc(FTokenPos, 3);
    {$endif}
    Inc(FCurRow);
    inc(FModuleRow);
    FCurColumnOffset:=1;
    if (FCurSourceFile is TMacroReader) and (FCurRow=1) then
    begin
      FCurRow:=TMacroReader(FCurSourceFile).CurRow;
      FCurColumnOffset:=TMacroReader(FCurSourceFile).CurCol;
    end;
    if LogEvent(sleLineNumber)
        and (((FCurRow Mod 100) = 0)
          or CurSourceFile.IsEOF) then
      DoLog(mtInfo,nLogLineNumber,SLogLineNumber,[FCurRow],True); // log last line
  end;
end;

procedure TPascalScanner.AddFile(aFilename: TPasScannerString);
var
  i: Integer;
begin
  for i:=0 to FFiles.Count-1 do
    if FFiles[i]=aFilename then exit;
  FFiles.Add(aFilename);
end;

function TPascalScanner.GetMacroName(const Param: TPasScannerString): TPasScannerString;
var
  p: Integer;
begin
  Result:=Trim(Param);
  p:=1;
  while (p<=length(Result)) and (Result[p] in ['a'..'z','A'..'Z','0'..'9','_']) do
    inc(p);
  SetLength(Result,p-1);
  Result:=UpperCase(Result);
end;

procedure TPascalScanner.SetCurMsg(MsgType: TMessageType; MsgNumber: integer;
  const Fmt: TPasScannerString; Args: array of const);
begin
  FLastMsgType := MsgType;
  FLastMsgNumber := MsgNumber;
  FLastMsgPattern := Fmt;
  FLastMsg := SafeFormat(Fmt,Args);
  CreateMsgArgs(FLastMsgArgs,Args);
end;

procedure TPascalScanner.SetCurMsg(MsgType: TMessageType; MsgNumber: integer; const Msg: TPasScannerString);
begin
  FLastMsgType := MsgType;
  FLastMsgNumber := MsgNumber;
  FLastMsgPattern := '';
  FLastMsgArgs:=[];
  FLastMsg := Msg;

end;

function TPascalScanner.AddDefine(const aName: TPasScannerString; Quiet: boolean): boolean;

begin
  If FDefines.IndexOf(aName)>=0 then exit(false);
  Result:=true;
  FDefines.Add(aName);
  if (not Quiet) and LogEvent(sleConditionals) then
    DoLog(mtInfo,nLogMacroDefined,sLogMacroDefined,[aName])
end;

function TPascalScanner.RemoveDefine(const aName: TPasScannerString; Quiet: boolean
  ): boolean;

Var
  I : Integer;

begin
  I:=FDefines.IndexOf(aName);
  if (I<0) then exit(false);
  Result:=true;
  FDefines.Delete(I);
  if (not Quiet) and LogEvent(sleConditionals) then
    DoLog(mtInfo,nLogMacroUnDefined,sLogMacroUnDefined,[aName])
end;

function TPascalScanner.UnDefine(const aName: TPasScannerString; Quiet: boolean): boolean;
begin
  // Important: always call both, do not use OR
  Result:=RemoveDefine(aName,Quiet);
  if RemoveMacro(aName,Quiet) then Result:=true;
end;

function TPascalScanner.IsDefined(const aName: TPasScannerString): boolean;
begin
  Result:=(FDefines.IndexOf(aName)>=0) or (FMacros.IndexOf(aName)>=0);
end;

function TPascalScanner.IfOpt(Letter: AnsiChar): boolean;
begin
  Letter:=upcase(Letter);
  Result:=(Letter in ['A'..'Z']) and (LetterSwitchNames[Letter]<>'')
    and IsDefined(LetterSwitchNames[Letter]);
end;

function TPascalScanner.AddMacro(const aName, aValue: TPasScannerString; Quiet: boolean
  ): boolean;
var
  Index: Integer;
begin
  Index:=FMacros.IndexOf(aName);
  If (Index=-1) then
    FMacros.AddObject(aName,TMacroDef.Create(aName,aValue))
  else
    begin
    if TMacroDef(FMacros.Objects[Index]).Value=aValue then exit(false);
    TMacroDef(FMacros.Objects[Index]).Value:=aValue;
    end;
  Result:=true;
  if (not Quiet) and LogEvent(sleConditionals) then
    DoLog(mtInfo,nLogMacroXSetToY,SLogMacroXSetToY,[aName,aValue])
end;

function TPascalScanner.RemoveMacro(const aName: TPasScannerString; Quiet: boolean
  ): boolean;
var
  Index: Integer;
begin
  Index:=FMacros.IndexOf(aName);
  if Index<0 then exit(false);
  Result:=true;
  TMacroDef(FMacros.Objects[Index]).{$ifdef pas2js}Destroy{$else}Free{$endif};
  FMacros.Delete(Index);
  if (not Quiet) and LogEvent(sleConditionals) then
    DoLog(mtInfo,nLogMacroUnDefined,sLogMacroUnDefined,[aName])
end;

procedure TPascalScanner.SetCompilerMode(S: TPasScannerString);
begin
  HandleMode(S);
end;

procedure TPascalScanner.SetModeSwitch(S: TPasScannerString);
begin
  HandleModeSwitch(S);
end;

function TPascalScanner.CurSourcePos: TPasSourcePos;
begin
  Result.FileName:=CurFilename;
  Result.Row:=CurRow;
  Result.Column:=CurColumn;
end;

function TPascalScanner.SetForceCaret(AValue: Boolean): Boolean;

begin
  Result:=toForceCaret in FTokenOptions;
  if aValue then
    Include(FTokenOptions,toForceCaret)
  else
    Exclude(FTokenOptions,toForceCaret)
end;

function TPascalScanner.IgnoreMsgType(MsgType: TMessageType): boolean;
begin
  Result:=false;
  case MsgType of
    mtWarning: if not (bsWarnings in FCurrentBoolSwitches) then exit(true);
    mtNote: if not (bsNotes in FCurrentBoolSwitches) then exit(true);
    mtHint: if not (bsHints in FCurrentBoolSwitches) then exit(true);
  else
    // Do nothing, satisfy compiler
  end;
end;

end.
