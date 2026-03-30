{
    This file is part of the Free Component Library

    Pascal parse tree classes
    Copyright (c) 2000-2005 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit PasTree;
{$ENDIF FPC_DOTTEDUNITS}

{$i fcl-passrc.inc}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes;
{$ELSE FPC_DOTTEDUNITS}
uses SysUtils, Classes;
{$ENDIF FPC_DOTTEDUNITS}

resourcestring
  // Parse tree node type names
  SPasTreeElement = 'generic element';
  SPasTreeSection = 'unit section';
  SPasTreeProgramSection = 'program section';
  SPasTreeLibrarySection = 'library section';
  SPasTreePackageSection = 'package section';
  SPasTreeInterfaceSection = 'interface section';
  SPasTreeImplementationSection = 'implementation section';
  SPasTreeRequiredPackage = 'Required package';
  SPasTreeUsesUnit = 'uses unit';
  SPasTreeModule = 'module';
  SPasTreeUnit = 'unit';
  SPasTreeProgram = 'program';
  SPasTreePackage = 'package';
  SPasTreeResString = 'resource string';
  SPasTreeType = 'generic type';
  SPasTreePointerType = 'pointer type';
  SPasTreeAliasType = 'alias type';
  SPasTreeTypeAliasType = '"type" alias type';
  SPasTreeClassOfType = '"class of" type';
  SPasTreeRangeType = 'range type';
  SPasTreeArrayType = 'array type';
  SPasTreeFileType = 'file type';
  SPasTreeEnumValue = 'enumeration value';
  SPasTreeEnumType = 'enumeration type';
  SPasTreeSetType = 'set type';
  SPasTreeRecordType = 'record type';
  SPasStringType = 'string type';
  SPasTreeObjectType = 'object';
  SPasTreeClassType = 'class';
  SPasTreeInterfaceType = 'interface';
  SPasTreeSpecializedType = 'specialized class type';
  SPasTreeSpecializedExpr = 'specialize expr';
  SPasClassHelperType = 'class helper type';
  SPasRecordHelperType = 'record helper type';
  SPasTypeHelperType = 'type helper type';
  SPasTreeArgument = 'argument';
  SPasTreeProcedureType = 'procedure type';
  SPasTreeResultElement = 'function result';
  SPasTreeConstructorType = 'constructor type';
  SPasTreeDestructorType = 'destructor type';
  SPasTreeFunctionType = 'function type';
  SPasTreeUnresolvedTypeRef = 'unresolved type reference';
  SPasTreeVariable = 'variable';
  SPasTreeConst = 'constant';
  SPasTreeProperty = 'property';
  SPasTreeOverloadedProcedure = 'overloaded procedure';
  SPasTreeProcedure = 'procedure';
  SPasTreeFunction = 'function';
  SPasTreeOperator = 'operator';
  SPasTreeClassOperator = 'class operator';
  SPasTreeClassProcedure = 'class procedure';
  SPasTreeClassFunction = 'class function';
  SPasTreeClassConstructor = 'class constructor';
  SPasTreeClassDestructor = 'class destructor';
  SPasTreeConstructor = 'constructor';
  SPasTreeDestructor = 'destructor';
  SPasTreeAnonymousProcedure = 'anonymous procedure';
  SPasTreeAnonymousFunction = 'anonymous function';
  SPasTreeProcedureImpl = 'procedure/function implementation';
  SPasTreeConstructorImpl = 'constructor implementation';
  SPasTreeDestructorImpl = 'destructor implementation';

type
  EPasTree = Class(Exception);

  TPastreeString = string;

  // Visitor pattern.
  TPassTreeVisitor = class;

  { TPasElementBase }

  TPasElementBase = class
  private
    FData: TObject;
  protected
    procedure Accept(Visitor: TPassTreeVisitor); virtual;
  public
    Property CustomData: TObject Read FData Write FData;
  end;
  TPasElementBaseClass = class of TPasElementBase;


  TPasModule = class;

  TPasMemberVisibility = (visDefault, visPrivate, visProtected, visPublic,
    visPublished, visAutomated,
    visStrictPrivate, visStrictProtected,
    visRequired, visOptional);

  TCallingConvention = (ccDefault,ccRegister,ccPascal,ccCDecl,ccStdCall,
                        ccOldFPCCall,ccSafeCall,ccSysCall,ccMWPascal,
                        ccHardFloat,ccSysV_ABI_Default,ccSysV_ABI_CDecl,
                        ccMS_ABI_Default,ccMS_ABI_CDecl,
                        ccVectorCall, ccWinApi);
  TProcTypeModifier = (ptmOfObject,ptmIsNested,ptmStatic,ptmVarargs,
                       ptmReferenceTo,ptmAsync,ptmFar,ptmCblock);
  TProcTypeModifiers = set of TProcTypeModifier;
  TPackMode = (pmNone,pmPacked,pmBitPacked);

  TPasMemberVisibilities = set of TPasMemberVisibility;
  TPasMemberHint = (hDeprecated,hLibrary,hPlatform,hExperimental,hUnimplemented);
  TPasMemberHints = set of TPasMemberHint;

  TPasElement = class;
  TPTreeElement = class of TPasElement;
  TPasElementArray = array of TPasElement;

  TOnForEachPasElement = procedure(El: TPasElement; arg: pointer) of object;

  { TPasElement }

  TPasElement = class(TPasElementBase)
  private
    FDocComment: TPasTreeString;
    FName: TPasTreeString;
    FParent: TPasElement;
    FHints: TPasMemberHints;
    FHintMessage: TPasTreeString;
    {$ifdef pas2js}
    FPasElementId: NativeInt;
    class var FLastPasElementId: NativeInt;
    {$endif}
  protected
    procedure ProcessHints(const ASemiColonPrefix: boolean; var AResult: TPasTreeString); virtual;
    procedure SetParent(const AValue: TPasElement); virtual;
  public
    SourceFilename: TPasTreeString;
    SourceLinenumber: Integer;
    SourceEndLinenumber: Integer;
    Visibility: TPasMemberVisibility;
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); virtual;
    destructor Destroy; override;
    Class Function IsKeyWord(Const S : TPasTreeString) : Boolean;
    Class Function EscapeKeyWord(Const S : TPasTreeString) : TPasTreeString;
    function FreeChild(Child: TPasElement; Prepare: boolean): TPasElement;
    procedure FreeChildList(List: TFPList; Prepare: boolean);
    procedure FreeChildArray(A: TPasElementArray; Prepare: boolean);
    procedure FreeChildren(Prepare: boolean); virtual;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); virtual;
    procedure ForEachChildCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer; Child: TPasElement; CheckParent: boolean); virtual;
    Function SafeName : TPasTreeString; virtual;                // Name but with & prepended if name is a keyword.
    function FullPath: TPasTreeString;                  // parent's names, until parent is not TPasDeclarations
    function ParentPath: TPasTreeString;                // parent's names
    function FullName: TPasTreeString; virtual;         // FullPath + Name
    function PathName: TPasTreeString; virtual;         // = Module.Name + ParentPath
    function GetModule: TPasModule;
    function ElementTypeName: TPasTreeString; virtual;
    Function HintsString : TPasTreeString;
    function GetDeclaration(full : Boolean) : TPasTreeString; virtual;
    procedure Accept(Visitor: TPassTreeVisitor); override;
    procedure ClearTypeReferences(aType: TPasElement); virtual;
    function HasParent(aParent: TPasElement): boolean;
    property Name: TPasTreeString read FName write FName;
    property Parent: TPasElement read FParent Write SetParent;
    property Hints : TPasMemberHints Read FHints Write FHints;
    property HintMessage : TPasTreeString Read FHintMessage Write FHintMessage;
    property DocComment : TPasTreeString Read FDocComment Write FDocComment;
    {$ifdef pas2js}
    property PasElementId: NativeInt read FPasElementId; // global unique id
    {$endif}
  end;

  TPasExprKind = (pekIdent, pekNumber, pekString, pekStringMultiLine, pekSet,
     pekNil, pekBoolConst,
     pekRange, pekUnary, pekBinary, pekFuncParams, pekArrayParams, pekListOfExp,
     pekInherited, pekSelf, pekSpecialize, pekProcedure, pekNamedArg);

  TExprOpCode = (eopNone,
                 eopAdd,eopSubtract,eopMultiply,eopDivide{/}, eopDiv{div},eopMod, eopPower,// arithmetic
                 eopShr,eopShl, // bit operations
                 eopNot,eopAnd,eopOr,eopXor, // logical/bit
                 eopEqual, eopNotEqual,  // Logical
                 eopLessThan,eopGreaterThan, eopLessthanEqual,eopGreaterThanEqual, // ordering
                 eopIn,eopIs,eopAs, eopSymmetricaldifference, // Specials
                 eopAddress, eopDeref, eopMemAddress, // Pointers  eopMemAddress=**
                 eopSubIdent); // SomeRec.A, A is subIdent of SomeRec

  { TPasExpr }

  TPasExpr = class(TPasElement)
    Kind      : TPasExprKind;
    OpCode    : TExprOpCode;
    Format1,Format2 : TPasExpr; // write, writeln, str
    constructor Create(AParent : TPasElement; AKind: TPasExprKind; AOpCode: TExprOpCode); virtual; overload;
    procedure FreeChildren(Prepare: boolean); override;
  end;

  { TUnaryExpr }

  TUnaryExpr = class(TPasExpr)
    Operand   : TPasExpr;
    constructor Create(AParent : TPasElement; AOperand: TPasExpr; AOpCode: TExprOpCode); overload;
    function GetDeclaration(full : Boolean) : TPasTreeString; override;
    procedure FreeChildren(Prepare: boolean); override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  { TBinaryExpr }

  TBinaryExpr = class(TPasExpr)
    Left      : TPasExpr;
    Right     : TPasExpr;
    constructor Create(AParent : TPasElement; xleft, xright: TPasExpr; AOpCode: TExprOpCode); overload;
    constructor CreateRange(AParent : TPasElement; xleft, xright: TPasExpr); overload;
    function GetDeclaration(full : Boolean) : TPasTreeString; override;
    procedure FreeChildren(Prepare: boolean); override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
    class function IsRightSubIdent(El: TPasElement): boolean;
  end;

  { TPrimitiveExpr }

  TPrimitiveExpr = class(TPasExpr)
    Value     : TPasTreeString;
    constructor Create(AParent : TPasElement; AKind: TPasExprKind; const AValue : TPasTreeString); overload;
    function GetDeclaration(full : Boolean) : TPasTreeString; override;
  end;

  { TBoolConstExpr }

  TBoolConstExpr = class(TPasExpr)
    Value     : Boolean;
    constructor Create(AParent : TPasElement; AKind: TPasExprKind; const ABoolValue : Boolean); overload;
    function GetDeclaration(full : Boolean) : TPasTreeString; override;
  end;

  { TNilExpr }

  TNilExpr = class(TPasExpr)
    constructor Create(AParent : TPasElement); overload;
    function GetDeclaration(full : Boolean) : TPasTreeString; override;
  end;

  { TInheritedExpr }

  TInheritedExpr = class(TPasExpr)
  Public
    constructor Create(AParent : TPasElement); overload;
    function GetDeclaration(full : Boolean) : TPasTreeString; override;
  end;

  { TSelfExpr }

  TSelfExpr = class(TPasExpr)
    constructor Create(AParent : TPasElement); overload;
    function GetDeclaration(full : Boolean) : TPasTreeString; override;
  end;

  TPasExprArray = array of TPasExpr;

  { TNamedArgExpr }

  TNamedArgExpr = class(TPasExpr)
    NameExpr  : TPrimitiveExpr;
    ValueExpr : TPasExpr;
    constructor Create(AParent: TPasElement; AName: TPrimitiveExpr; AValue: TPasExpr); overload;
    function GetDeclaration(full: Boolean): TPasTreeString; override;
    procedure FreeChildren(Prepare: boolean); override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;


  { TParamsExpr - source position is the opening bracket }

  TParamsExpr = class(TPasExpr)
    Value     : TPasExpr;
    Params    : TPasExprArray;
    // Kind: pekArrayParams, pekFuncParams, pekSet
    constructor Create(AParent : TPasElement; AKind: TPasExprKind); overload;
    function GetDeclaration(full : Boolean) : TPasTreeString; override;
    procedure FreeChildren(Prepare: boolean); override;
    procedure AddParam(xp: TPasExpr);
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  { TRecordValues }

  TRecordValuesItem = record
    Name      : TPasTreeString;
    NameExp   : TPrimitiveExpr;
    ValueExp  : TPasExpr;
  end;
  PRecordValuesItem = ^TRecordValuesItem;
  TRecordValuesItemArray = array of TRecordValuesItem;

  TRecordValues = class(TPasExpr)
    Fields    : TRecordValuesItemArray;
    constructor Create(AParent : TPasElement); overload;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    procedure AddField(AName: TPrimitiveExpr; Value: TPasExpr);
    function GetDeclaration(full : Boolean) : TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  { TArrayValues }

  TArrayValues = class(TPasExpr)
    Values    : TPasExprArray;
    constructor Create(AParent : TPasElement); overload;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    procedure AddValues(AValue: TPasExpr);
    function GetDeclaration(full : Boolean) : TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  { TPasDeclarations - base class of TPasSection, TProcedureBody }

  TPasDeclarations = class(TPasElement)
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Declarations: TFPList; // list of TPasElement
    // Declarations contains all the following:
    Attributes, // TPasAttributes
    Classes,    // TPasClassType, TPasRecordType
    Consts,     // TPasConst
    ExportSymbols,// TPasExportSymbol
    Functions,  // TPasProcedure
    Properties, // TPasProperty
    ResStrings, // TPasResString
    Labels,     // TPasLabel
    Types,      // TPasType, except TPasClassType, TPasRecordType
    Variables   // TPasVariable, not descendants
      : TFPList;
  end;

  { TPasUsesUnit - Parent is TPasSection }

  TPasUsesUnit = class(TPasElement)
  public
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Expr: TPasExpr; // name expression
    InFilename: TPrimitiveExpr; // Kind=pekString, can be nil
    Module: TPasElement; // TPasUnresolvedUnitRef or TPasModule
  end;
  TPasUsesClause = array of TPasUsesUnit;

  { TPasSection }

  TPasSection = class(TPasDeclarations)
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    function AddUnitToUsesList(const AUnitName: TPasTreeString; aName: TPasExpr = nil;
      InFilename: TPrimitiveExpr = nil; aModule: TPasElement = nil;
      UsesUnit: TPasUsesUnit = nil): TPasUsesUnit;
    function ElementTypeName: TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    UsesList: TFPList; // kept for compatibility, see TPasUsesUnit.Module
    UsesClause: TPasUsesClause;
    PendingUsedIntf: TPasUsesUnit; // <>nil while resolving a uses cycle
  end;
  TPasSectionClass = class of TPasSection;

  { TInterfaceSection }

  TInterfaceSection = class(TPasSection)
  public
    function ElementTypeName: TPasTreeString; override;
  end;

  { TImplementationSection }

  TImplementationSection = class(TPasSection)
  public
    function ElementTypeName: TPasTreeString; override;
  end;

  { TProgramSection }

  TProgramSection = class(TImplementationSection)
  public
    function ElementTypeName: TPasTreeString; override;
  end;

  { TLibrarySection }

  TLibrarySection = class(TImplementationSection)
  public
    function ElementTypeName: TPasTreeString; override;
  end;

  { TPackageSection }

  TPasPackageSection = class(TInterfaceSection)
  public
    Requires : TFPList; // Array of TRequiredPackage;
  Public
    Constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    Destructor Destroy; override;
    Procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
  end;


  TPasImplCommandBase = class;
  TInitializationSection = class;
  TFinalizationSection = class;

  { TPasModule }

  TPasModule = class(TPasElement)
  public
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : boolean) : TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    GlobalDirectivesSection: TPasImplCommandBase; // not used by pparser
    InterfaceSection: TInterfaceSection;
    ImplementationSection: TImplementationSection;
    InitializationSection: TInitializationSection; // in TPasProgram the begin..end.
    FinalizationSection: TFinalizationSection;
    PackageName: TPasTreeString;
    Filename   : TPasTreeString;  // the IN filename, only written when not empty.
  end;
  TPasModuleClass = class of TPasModule;

  { TPasUnitModule }

  TPasUnitModule = Class(TPasModule)
  public
    function ElementTypeName: TPasTreeString; override;
  end;

  { TPasProgram }

  TPasProgram = class(TPasModule)
  Public
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  Public
    ProgramSection: TProgramSection;
    InputFile,OutPutFile : TPasTreeString;
    // Note: the begin..end. block is in the InitializationSection
  end;

  { TPasLibrary }

  TPasLibrary = class(TPasModule)
  Public
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  Public
    LibrarySection: TLibrarySection;
    InputFile,OutPutFile : TPasTreeString;
  end;

  { TPasPackage }

  TPasPackage = class(TPasElement)
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Modules: TFPList;     // List of TPasModule objects
  end;

  { TRequiredPackage }

  TPasRequiredPackage = Class(TPasElement)
    function ElementTypeName: TPasTreeString; override;
  end;

  { TPasDynamicPackage }

  TPasDynamicPackage = class(TPasModule)
  Public
    PackageSection : TPasPackageSection;
    procedure FreeChildren(Prepare: boolean); override;
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
  end;


  { TPasResString }

  TPasResString = class(TPasElement)
  public
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : Boolean) : TPasTreeString; Override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Expr: TPasExpr;
  end;

  { TPasType }

  TPasType = class(TPasElement)
  Protected
    Function FixTypeDecl(aDecl: TPasTreeString) : TPasTreeString; virtual;
  public
    Function SafeName : TPasTreeString; override;
    function ElementTypeName: TPasTreeString; override;
  end;
  TPasTypeArray = array of TPasType;

  { TPasAliasType }

  TPasAliasType = class(TPasType)
  protected
    Function FixTypeDecl(aDecl: TPasTreeString) : TPasTreeString; override;
  public
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : Boolean): TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
    procedure ClearTypeReferences(aType: TPasElement); override;
  public
    DestType: TPasType;
    SubType: TPasType;
    Expr: TPasExpr;
    CodepageExpr: TPasExpr;
  end;

  { TPasPointerType - todo: change it TPasAliasType }

  TPasPointerType = class(TPasType)
  public
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : Boolean): TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
    procedure ClearTypeReferences(aType: TPasElement); override;
  public
    DestType: TPasType;
  end;

  { TPasTypeAliasType }

  TPasTypeAliasType = class(TPasAliasType)
  public
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(Full : boolean) : TPastreeString; override;
  end;

  { TPasGenericTemplateType - type param of a generic }

  TPasGenericTemplateType = Class(TPasType)
  public
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    function GetDeclaration(full : boolean) : TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
    procedure AddConstraint(El: TPasElement);
    procedure ClearTypeReferences(aType: TPasElement); override;
  Public
    IsConst: Boolean; // true for const generic parameters
    TypeConstraint: TPasTreeString deprecated; // deprecated in fpc 3.3.1
    Constraints: TPasElementArray; // list of TPasExpr or TPasType, can be nil!
  end;

  { TPasGenericType - abstract base class for all types which can be generics }

  TPasGenericType = class(TPasType)
  public
    GenericTemplateTypes: TFPList; // list of TPasGenericTemplateType, can be nil
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
    procedure SetGenericTemplates(AList: TFPList); virtual;
  end;

  { TPasSpecializeType DestType<Params> }

  TPasSpecializeType = class(TPasAliasType)
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    procedure ClearTypeReferences(aType: TPasElement); override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full: boolean) : TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Params: TFPList; // list of TPasType or TPasExpr
  end;

  { TInlineSpecializeExpr - A<B,C> }

  TInlineSpecializeExpr = class(TPasExpr)
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    procedure ClearTypeReferences(aType: TPasElement); override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : Boolean): TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    NameExpr: TPasExpr;
    Params: TFPList; // list of TPasType
  end;

  { TPasClassOfType }

  TPasClassOfType = class(TPasAliasType)
  public
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full: boolean) : TPasTreeString; override;
  end;

  { TPasRangeType }

  TPasRangeType = class(TPasType)
  public
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : boolean) : TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    RangeExpr : TBinaryExpr; // Kind=pekRange
    procedure FreeChildren(Prepare: boolean); override;
    Function RangeStart : TPasTreeString;
    Function RangeEnd : TPasTreeString;
  end;

  { TPasArrayType }

  TPasArrayType = class(TPasGenericType)
  public
    procedure FreeChildren(Prepare: boolean); override;
    procedure ClearTypeReferences(aType: TPasElement); override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : boolean) : TPasTreeString; override;
  public
    IndexRange : TPasTreeString; // only valid if Parser po_arrayrangeexpr disabled
    Ranges: TPasExprArray; // only valid if Parser po_arrayrangeexpr enabled
    PackMode : TPackMode;
    ElType: TPasType; // nil means array-of-const
    function IsGenericArray : Boolean; inline;
    function IsPacked : Boolean; inline;
    procedure AddRange(Range: TPasExpr);
  end;

  { TPasFileType }

  TPasFileType = class(TPasType)
  public
    procedure FreeChildren(Prepare: boolean); override;
    procedure ClearTypeReferences(aType: TPasElement); override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : boolean) : TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    ElType: TPasType;
  end;

  { TPasEnumValue - Parent is TPasEnumType }

  TPasEnumValue = class(TPasElement)
  public
    function ElementTypeName: TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Value: TPasExpr;
    procedure FreeChildren(Prepare: boolean); override;
    Function AssignedValue : TPasTreeString;
  end;

  { TPasEnumType }

  TPasEnumType = class(TPasType)
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : boolean) : TPasTreeString; override;
    Procedure GetEnumNames(Names : TStrings);
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Values: TFPList;      // List of TPasEnumValue
  end;

  { TPasSetType }

  TPasSetType = class(TPasType)
  public
    procedure FreeChildren(Prepare: boolean); override;
    procedure ClearTypeReferences(aType: TPasElement); override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : boolean) : TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    EnumType: TPasType; // alias or enumtype
    IsPacked : Boolean;
  end;

  TPasRecordType = class;

  { TPasVariant }

  TPasVariant = class(TPasElement)
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    function GetDeclaration(full : boolean) : TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Values: TFPList; // list of TPasExpr
    Members: TPasRecordType;
  end;

  { TPasMembersType - base type for TPasRecordType and TPasClassType }

  TPasMembersType = class(TPasGenericType)
  public
    type
      TRTTIVisibilitySection = (vcPrivate,vcProtected,vcPublic,vcPublished);
      TRTTIVisibilitySections = set of TRTTIVisibilitySection;
      TRTTIVisibility = record
        Explicit: boolean; // inherit or explicit
        Fields: TRTTIVisibilitySections;
        Methods: TRTTIVisibilitySections;
        Properties: TRTTIVisibilitySections;
      end;
    const
      VisibilityToExtRTTI: array[TPasMemberVisibility] of TRTTIVisibilitySection = (
        vcPublic, // visDefault,
        vcPrivate, // visPrivate,
        vcProtected, // visProtected,
        vcPublic, // visPublic,
        vcPublished, // visPublished,
        vcPublic, // visAutomated,
        vcPrivate, // visStrictPrivate,
        vcProtected, // visStrictProtected,
        vcPublic, // visRequired,
        vcPublic // visOptional
        );
  public
    PackMode: TPackMode;
    Members: TFPList;
    RTTIVisibility: TRTTIVisibility; // set by $RTTI directive
    Constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    Destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    Function IsPacked: Boolean; inline;
    Function IsBitPacked : Boolean; inline;
    Procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
    Function HasExtRTTI(El: TPasElement): boolean; virtual;
  end;

  { TPasRecordType }

  TPasRecordType = class(TPasMembersType)
  private
    procedure GetMembers(S: TStrings; aSkipSection: Boolean=false);
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    procedure ClearTypeReferences(aType: TPasElement); override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : boolean) : TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    VariantEl: TPasElement; // nil or TPasVariable or TPasType
    Variants: TFPList;	// list of TPasVariant elements, may be nil!
    Align : Integer;
    Function IsAdvancedRecord : Boolean;

  end;

  TPasObjKind = (
    okObject, okClass, okInterface,
    // okGeneric  removed in FPC 3.3.1  check instead GenericTemplateTypes<>nil
    // okSpecialize removed in FPC 3.1.1
    okClassHelper, okRecordHelper, okTypeHelper,
    okDispInterface, okObjcClass, okObjcCategory,
    okObjcProtocol);
const
  okWithFields = [okObject, okClass, okObjcClass, okObjcCategory];
  okAllHelpers = [okClassHelper,okRecordHelper,okTypeHelper];
  okWithClassFields = okWithFields+okAllHelpers;
  okObjCClasses = [okObjcClass, okObjcCategory, okObjcProtocol];

type

  TPasClassInterfaceType = (
    citCom, // default
    citCorba
    );

  { TPasClassType }

  TPasClassType = class(TPasMembersType)
  protected
    procedure GetMembers(S: TStrings); virtual;
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    procedure ClearTypeReferences(aType: TPasElement); override;
    function ElementTypeName: TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    ObjKind: TPasObjKind;
    AncestorType: TPasType;   // TPasClassType or TPasUnresolvedTypeRef or TPasAliasType or TPasTypeAliasType
                              // Note: AncestorType can be nil even though it has a default ancestor
    HelperForType: TPasType;  // any type, except helper
    IsForward: Boolean;
    IsExternal : Boolean;
    IsShortDefinition: Boolean;//class(ancestor); without end
    GUIDExpr : TPasExpr;
    Modifiers: TStringList;
    Interfaces : TFPList; // list of TPasType
    ExternalNameSpace : TPasTreeString;
    ExternalName : TPasTreeString;
    InterfaceType: TPasClassInterfaceType;
    Function IsObjCClass : Boolean;
    function GetDeclaration(full: boolean): TPasTreeString; override;
    Function FindMember(MemberClass : TPTreeElement; Const MemberName : TPasTreeString) : TPasElement;
    Function FindMemberInAncestors(MemberClass : TPTreeElement; Const MemberName : TPasTreeString) : TPasElement;
    Function InterfaceGUID : TPasTreeString;
    Function IsSealed : Boolean;
    Function IsAbstract : Boolean;
    Function HasModifier(const aModifier: TPasTreeString): Boolean;
  end;

  TArgumentAccess = (argDefault, argConst, argVar, argOut, argConstRef);

  { TPasAttributes }

  TPasAttributes = class(TPasElement)
  public
    procedure FreeChildren(Prepare: boolean); override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
    procedure AddCall(Expr: TPasExpr);
  public
    Calls: TPasExprArray;
  end;

  { TPasArgument }

  TPasArgument = class(TPasElement)
  public
    procedure FreeChildren(Prepare: boolean); override;
    procedure ClearTypeReferences(aType: TPasElement); override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : boolean) : TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Access: TArgumentAccess;
    Attributes: TPasAttributes;
    ArgType: TPasType; // can be nil, when Access<>argDefault
    ValueExpr: TPasExpr; // the default value
    Function Value : TPasTreeString;
  end;

  { TPasProcedureType }

  TPasProcedureType = class(TPasGenericType)
  private
    function GetIsAsync: Boolean; inline;
    function GetIsNested: Boolean; inline;
    function GetIsOfObject: Boolean; inline;
    function GetIsReference: Boolean; inline;
    procedure SetIsAsync(const AValue: Boolean);
    procedure SetIsNested(const AValue: Boolean);
    procedure SetIsOfObject(const AValue: Boolean);
    procedure SetIsReference(AValue: Boolean);
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    procedure ClearTypeReferences(aType: TPasElement); override;
    class function TypeName: TPasTreeString; virtual;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : boolean) : TPasTreeString; override;
    procedure GetArguments(List : TStrings);
    function CreateArgument(const AName, AUnresolvedTypeName: TPasTreeString): TPasArgument; // not used by TPasParser
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Args: TFPList;        // List of TPasArgument objects
    CallingConvention: TCallingConvention;
    Modifiers: TProcTypeModifiers;
    VarArgsType: TPasType;
    property IsOfObject: Boolean read GetIsOfObject write SetIsOfObject;
    property IsNested : Boolean read GetIsNested write SetIsNested;
    property IsReferenceTo : Boolean Read GetIsReference write SetIsReference;
    property IsAsync: Boolean read GetIsAsync write SetIsAsync;
  end;
  TPasProcedureTypeClass = class of TPasProcedureType;

  { TPasResultElement - parent is TPasFunctionType }

  TPasResultElement = class(TPasElement)
  public
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName : TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
    procedure ClearTypeReferences(aType: TPasElement); override;
  public
    ResultType: TPasType;
  end;

  { TPasFunctionType }

  TPasFunctionType = class(TPasProcedureType)
  public
    procedure FreeChildren(Prepare: boolean); override;
    class function TypeName: TPasTreeString; override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(Full : boolean) : TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    ResultEl: TPasResultElement;
  end;

  TPasUnresolvedSymbolRef = class(TPasType)
  end;

  TPasUnresolvedTypeRef = class(TPasUnresolvedSymbolRef)
  public
    // Typerefs cannot be parented! -> AParent _must_ be NIL
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : Boolean) : TPasTreeString; override;
  end;

  { TPasUnresolvedUnitRef }

  TPasUnresolvedUnitRef = Class(TPasUnresolvedSymbolRef)
  public
    FileName : TPasTreeString;
    function ElementTypeName: TPasTreeString; override;
  end;

  { TPasStringType - e.g. TPasTreeString[len] }

  TPasStringType = class(TPasUnresolvedTypeRef)
  public
    LengthExpr : TPasTreeString;
    CodePageExpr : TPasTreeString;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full: Boolean): TPasTreeString; override;
  end;

  { TPasTypeRef  - not used by TPasParser }

  TPasTypeRef = class(TPasUnresolvedTypeRef)
  public
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    RefType: TPasType;
  end;

  { TPasVariable }
  TVariableModifier = (vmCVar, vmExternal, vmPublic, vmExport, vmClass, vmStatic, vmfar, vmThread);
  TVariableModifiers = set of TVariableModifier;

  TPasVariable = class(TPasElement)
  public
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : boolean) : TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
    procedure ClearTypeReferences(aType: TPasElement); override;
  public
    VarType: TPasType;
    VarModifiers : TVariableModifiers;
    LibraryName : TPasExpr; // libname of modifier external
    ExportName : TPasExpr; // symbol name of modifier external, export and public
    Modifiers : TPasTreeString;
    AbsoluteLocation : TPasTreeString deprecated; // deprecated in fpc 3.1.1
    AbsoluteExpr: TPasExpr;
    Expr: TPasExpr;
    Function Value : TPasTreeString;
  end;

  { TPasExportSymbol }

  TPasExportSymbol = class(TPasElement)
  public
    NameExpr: TPasExpr; // only if name is not a simple identifier
    ExportName : TPasExpr;
    ExportIndex : TPasExpr;
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : boolean) : TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  { TPasConst }

  TPasConst = class(TPasVariable)
  public
    IsConst: boolean; // true iff untyped const or typed with $WritableConst off
    function ElementTypeName: TPasTreeString; override;
  end;

  { TPasProperty }

  TPasProperty = class(TPasVariable)
  private
    FArgs: TFPList;
    FResolvedType : TPasType;
    function GetIsClass: boolean; inline;
    procedure SetIsClass(AValue: boolean);
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
    function GetDeclaration(full : boolean) : TPasTreeString; override; overload;
    function GetDeclaration(full: boolean; WithAccessor: Boolean): TPasTreeString; overload;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    IndexExpr: TPasExpr;
    ReadAccessor: TPasExpr;
    WriteAccessor: TPasExpr;
    DispIDExpr : TPasExpr;   // Can be nil.
    Implements: TPasExprArray;
    StoredAccessor: TPasExpr;
    DefaultExpr: TPasExpr;
    ReadAccessorName: TPasTreeString; // not used by resolver
    WriteAccessorName: TPasTreeString; // not used by resolver
    ImplementsName: TPasTreeString; // not used by resolver
    StoredAccessorName: TPasTreeString; // not used by resolver
    DispIDReadOnly,
    IsDefault, IsNodefault: Boolean;
    property Args: TFPList read FArgs; // List of TPasArgument objects
    property IsClass: boolean read GetIsClass write SetIsClass;
    Function ResolvedType : TPasType;
    Function IndexValue : TPasTreeString;
    Function DefaultValue : TPasTreeString;
  end;

  TProcType = (ptProcedure, ptFunction,
               ptOperator, ptClassOperator,
               ptConstructor, ptDestructor,
               ptClassProcedure, ptClassFunction,
               ptClassConstructor, ptClassDestructor,
               ptAnonymousProcedure, ptAnonymousFunction);

  { TPasProcedureBase }

  TPasProcedureBase = class(TPasElement)
  public
    function TypeName: TPasTreeString; virtual; abstract;
  end;

  { TPasOverloadedProc - not used by resolver }

  TPasOverloadedProc = class(TPasProcedureBase)
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
    function TypeName: TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Overloads: TFPList;           // List of TPasProcedure nodes
  end;

  { TPasProcedure }

  TProcedureModifier = (pmVirtual, pmDynamic, pmAbstract, pmOverride,
                        pmExport, pmOverload, pmMessage, pmReintroduce,
                        pmInline, pmAssembler, pmPublic,
                        pmCompilerProc, pmExternal, pmForward, pmDispId,
                        pmNoReturn, pmFar, pmFinal, pmDiscardResult,
                        pmNoStackFrame, pmsection, pmRtlProc, pmInternProc,
                        pmWeakExternal);
  TProcedureModifiers = Set of TProcedureModifier;
  TProcedureMessageType = (pmtNone,pmtInteger,pmtString);

  { TProcedureNamePart }

  TProcedureNamePart = class
    Name: TPasTreeString;
    Templates: TFPList; // optional list of TPasGenericTemplateType, can be nil!
  end;
  TProcedureNameParts = TFPList; // list of TProcedureNamePart

  TProcedureBody = class;

  { TPasProcedure - named procedure, not anonymous }

  TPasProcedure = class(TPasProcedureBase)
  Private
    FModifiers : TProcedureModifiers;
    FMessageName : TPasTreeString;
    FMessageType : TProcedureMessageType;
    function GetCallingConvention: TCallingConvention;
    procedure SetCallingConvention(AValue: TCallingConvention);
  public
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
    function TypeName: TPasTreeString; override;
    function GetDeclaration(full: Boolean): TPasTreeString; overload; override;
    function GetDeclaration(full, AddArgs, AddModifiers, AddParent: Boolean): TPasTreeString; overload; virtual;
    procedure GetModifiers(List: TStrings);
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    PublicName, // e.g. public PublicName;
    LibrarySymbolIndex : TPasExpr;
    LibrarySymbolName,
    LibraryExpr : TPasExpr; // e.g. external LibraryExpr name LibrarySymbolName;
    DispIDExpr :  TPasExpr;
    MessageExpr: TPasExpr;
    CompProcID : String;
    AliasName : TPasTreeString;
    ProcType : TPasProcedureType;
    Body : TProcedureBody;
    NameParts: TProcedureNameParts; // only used for generic aka parameterized functions
    Procedure AddModifier(AModifier : TProcedureModifier);
    Function CanParseImplementation : Boolean;
    Function HasNoImplementation : Boolean;
    Function IsVirtual : Boolean; inline;
    Function IsDynamic : Boolean; inline;
    Function IsAbstract : Boolean; inline;
    Function IsOverride : Boolean; inline;
    Function IsExported : Boolean; inline;
    Function IsExternal : Boolean; inline;
    Function IsOverload : Boolean; inline;
    Function IsMessage: Boolean; inline;
    Function IsReintroduced : Boolean; inline;
    Function IsStatic : Boolean; inline;
    Function IsForward: Boolean; inline;
    Function IsCompilerProc: Boolean; inline;
    Function IsInternProc: Boolean; inline;
    Function IsAssembler: Boolean; inline;
    Function IsAsync: Boolean; inline;
    Function GetProcTypeEnum: TProcType; virtual;
    procedure SetNameParts(Parts: TProcedureNameParts);
    Property Modifiers : TProcedureModifiers Read FModifiers Write FModifiers;
    Property CallingConvention : TCallingConvention Read GetCallingConvention Write SetCallingConvention;
    Property MessageName : TPasTreeString Read FMessageName Write FMessageName;
    property MessageType : TProcedureMessageType Read FMessageType Write FMessageType;
  end;
  TPasProcedureClass = class of TPasProcedure;

  TArrayOfPasProcedure = array of TPasProcedure;

  { TPasFunction - named function, not anonymous function}

  TPasFunction = class(TPasProcedure)
  private
    function GetFT: TPasFunctionType; inline;
  public
    function ElementTypeName: TPasTreeString; override;
    function TypeName: TPasTreeString; override;
    Property FuncType : TPasFunctionType Read GetFT;
    function GetProcTypeEnum: TProcType; override;
  end;

  { TPasOperator }

  TOperatorType = (
    otUnknown,
    otImplicit, otExplicit,
    otMul, otPlus, otMinus, otDivision,
    otLessThan, otEqual, otGreaterThan,
    otAssign, otNotEqual, otLessEqualThan, otGreaterEqualThan,
    otPower, otSymmetricalDifference,
    otInc, otDec,
    otMod,
    otNegative, otPositive,
    otBitWiseOr,
    otDiv,
    otLeftShift,
    otLogicalOr,
    otBitwiseAnd, otbitwiseXor,
    otLogicalAnd, otLogicalNot, otLogicalXor,
    otRightShift,
    otEnumerator, otIn,
    // Management operators
    otInitialize,
    otFinalize,
    otAddRef,
    otCopy
    );
  TOperatorTypes = set of TOperatorType;

  TPasOperator = class(TPasFunction)
  private
    FOperatorType: TOperatorType;
    FTokenBased: Boolean;
    function NameSuffix: TPasTreeString;
  public
    Class Function OperatorTypeToToken(T : TOperatorType) : TPasTreeString;
    Class Function OperatorTypeToOperatorName(T: TOperatorType) : TPasTreeString;
    Class Function TokenToOperatorType(S : TPasTreeString) : TOperatorType;
    Class Function NameToOperatorType(S : TPasTreeString) : TOperatorType;
    Procedure CorrectName;
    // For backwards compatibility the old name can still be used to search on.
    function GetOperatorDeclaration(Full: Boolean): TPasTreeString;
    Function OldName(WithPath : Boolean) : TPasTreeString;
    function ElementTypeName: TPasTreeString; override;
    function TypeName: TPasTreeString; override;
    function GetProcTypeEnum: TProcType; override;
    function GetDeclaration(full, AddArgs, AddModifiers, AddParent: Boolean): TPasTreeString; override;
    Property OperatorType : TOperatorType Read FOperatorType Write FOperatorType;
    // True if the declaration was using a token instead of an identifier
    Property TokenBased : Boolean Read FTokenBased Write FTokenBased;
  end;

  { TPasClassOperator }

  TPasClassOperator = class(TPasOperator)
  public
    function TypeName: TPasTreeString; override;
    function GetProcTypeEnum: TProcType; override;
  end;

  { TPasConstructor }

  TPasConstructor = class(TPasProcedure)
  public
    function ElementTypeName: TPasTreeString; override;
    function TypeName: TPasTreeString; override;
    function GetProcTypeEnum: TProcType; override;
  end;

  { TPasClassConstructor }

  TPasClassConstructor  = class(TPasConstructor)
  public
    function ElementTypeName: TPasTreeString; override;
    function TypeName: TPasTreeString; override;
    function GetProcTypeEnum: TProcType; override;
  end;

  { TPasDestructor }

  TPasDestructor = class(TPasProcedure)
  public
    function ElementTypeName: TPasTreeString; override;
    function TypeName: TPasTreeString; override;
    function GetProcTypeEnum: TProcType; override;
  end;

  { TPasClassDestructor }

  TPasClassDestructor  = class(TPasDestructor)
  public
    function ElementTypeName: TPasTreeString; override;
    function TypeName: TPasTreeString; override;
    function GetProcTypeEnum: TProcType; override;
  end;

  { TPasClassProcedure }

  TPasClassProcedure = class(TPasProcedure)
  public
    function ElementTypeName: TPasTreeString; override;
    function TypeName: TPasTreeString; override;
    function GetProcTypeEnum: TProcType; override;
  end;

  { TPasClassFunction }

  TPasClassFunction = class(TPasFunction)
  public
    function ElementTypeName: TPasTreeString; override;
    function TypeName: TPasTreeString; override;
    function GetProcTypeEnum: TProcType; override;
  end;

  { TPasAnonymousProcedure - parent is TProcedureExpr }

  TPasAnonymousProcedure = class(TPasProcedure)
  public
    function ElementTypeName: TPasTreeString; override;
    function TypeName: TPasTreeString; override;
    function GetProcTypeEnum: TProcType; override;
  end;

  { TPasAnonymousFunction - parent is TProcedureExpr and ProcType is TPasFunctionType}

  TPasAnonymousFunction = class(TPasAnonymousProcedure)
  private
    function GetFT: TPasFunctionType; inline;
  public
    function ElementTypeName: TPasTreeString; override;
    function TypeName: TPasTreeString; override;
    Property FuncType : TPasFunctionType Read GetFT;
    function GetProcTypeEnum: TProcType; override;
  end;

  { TProcedureExpr }

  TProcedureExpr = class(TPasExpr)
  public
    Proc: TPasAnonymousProcedure;
    constructor Create(AParent: TPasElement); overload;
    procedure FreeChildren(Prepare: boolean); override;
    function GetDeclaration(full: Boolean): TPasTreeString; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  { TPasMethodResolution }

  TPasMethodResolution = class(TPasElement)
  public
    procedure FreeChildren(Prepare: boolean); override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    ProcClass: TPasProcedureClass;
    InterfaceName: TPasExpr;
    InterfaceProc: TPasExpr;
    ImplementationProc: TPasExpr;
  end;

  TPasImplBlock = class;

  { TProcedureBody - the var+type+const+begin, without the header, child of TPasProcedure }

  TProcedureBody = class(TPasDeclarations)
  public
    procedure FreeChildren(Prepare: boolean); override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Body: TPasImplBlock;
  end;

  { TPasProcedureImpl - used by mkxmlrpc, not by pparser }

  TPasProcedureImpl = class(TPasElement)
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    function ElementTypeName: TPasTreeString; override;
    function TypeName: TPasTreeString; virtual;
  public
    ProcType: TPasProcedureType;
    Locals: TFPList;
    Body: TPasImplBlock;
    IsClassMethod: boolean;
  end;

  { TPasConstructorImpl - used by mkxmlrpc, not by pparser }

  TPasConstructorImpl = class(TPasProcedureImpl)
  public
    function ElementTypeName: TPasTreeString; override;
    function TypeName: TPasTreeString; override;
  end;

  { TPasDestructorImpl - used by mkxmlrpc, not by pparser }

  TPasDestructorImpl = class(TPasProcedureImpl)
  public
    function ElementTypeName: TPasTreeString; override;
    function TypeName: TPasTreeString; override;
  end;

  { TPasImplElement - implementation element }

  TPasImplElement = class(TPasElement)
  end;

  { TPasImplCommandBase }

  TPasImplCommandBase = class(TPasImplElement)
  public
    SemicolonAtEOL: boolean;
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
  end;

  { TPasImplCommand - currently used as empty statement, e.g. if then else ; }

  TPasImplCommand = class(TPasImplCommandBase)
  public
    Command: TPasTreeString; // never set by TPasParser
  end;

  { TPasImplCommands - used by mkxmlrpc, not used by pparser }

  TPasImplCommands = class(TPasImplCommandBase)
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
  public
    Commands: TStrings;
  end;

  { TPasLabels }

  TPasLabels = class(TPasImplElement)
  public
    Labels: TStrings;
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    function GetDeclaration(full : Boolean) : TPasTreeString; override;
  end;

  TPasImplBeginBlock = class;
  TPasImplRepeatUntil = class;
  TPasImplIfElse = class;
  TPasImplWhileDo = class;
  TPasImplWithDo = class;
  TPasImplCaseOf = class;
  TPasImplForLoop = class;
  TPasImplTry = class;
  TPasImplExceptOn = class;
  TPasImplRaise = class;
  TPasImplAssign = class;
  TPasImplSimple = class;
  TPasImplLabelMark = class;

  { TPasImplBlock }

  TPasImplBlock = class(TPasImplElement)
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    procedure AddElement(Element: TPasImplElement); virtual;
    function AddCommand(const ACommand: TPasTreeString): TPasImplCommand;
    function AddCommands: TPasImplCommands; // used by mkxmlrpc, not by pparser
    function AddBeginBlock: TPasImplBeginBlock;
    function AddRepeatUntil: TPasImplRepeatUntil;
    function AddIfElse(const ACondition: TPasExpr): TPasImplIfElse;
    function AddWhileDo(const ACondition: TPasExpr): TPasImplWhileDo;
    function AddWithDo(const Expression: TPasExpr): TPasImplWithDo;
    function AddCaseOf(const Expression: TPasExpr): TPasImplCaseOf;
    function AddForLoop(AVar: TPasVariable;
      const AStartValue, AEndValue: TPasExpr): TPasImplForLoop;
    function AddForLoop(AVarName : TPasExpr; AStartValue, AEndValue: TPasExpr;
      ADownTo: Boolean = false): TPasImplForLoop;
    function AddTry: TPasImplTry;
    function AddExceptOn(const VarName, TypeName: TPasTreeString): TPasImplExceptOn;
    function AddExceptOn(const VarName: TPasTreeString; VarType: TPasType): TPasImplExceptOn;
    function AddExceptOn(const VarEl: TPasVariable): TPasImplExceptOn;
    function AddExceptOn(const TypeEl: TPasType): TPasImplExceptOn;
    function AddRaise: TPasImplRaise;
    function AddLabelMark(const Id: TPasTreeString): TPasImplLabelMark;
    function AddAssign(Left, Right: TPasExpr): TPasImplAssign;
    function AddSimple(Expr: TPasExpr): TPasImplSimple;
    function CloseOnSemicolon: boolean; virtual;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Elements: TFPList;    // list of TPasImplElement
  end;
  TPasImplBlockClass = class of TPasImplBlock;

  { TPasImplStatement - base class }

  TPasImplStatement = class(TPasImplBlock)
  public
    function CloseOnSemicolon: boolean; override;
  end;

  { TPasImplBeginBlock }

  TPasImplBeginBlock = class(TPasImplBlock)
  end;

  { TInitializationSection }

  TInitializationSection = class(TPasImplBlock)
  end;

  { TFinalizationSection }

  TFinalizationSection = class(TPasImplBlock)
  end;

  { TPasImplAsmStatement }

  TPasImplAsmStatement = class (TPasImplStatement)
  private
    FModifierTokens: TStrings;
    FTokens: TStrings;
  Public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    Property Tokens : TStrings Read FTokens;
    // ['register']
    Property ModifierTokens : TStrings Read FModifierTokens;
  end;

  { TPasImplRepeatUntil }

  TPasImplRepeatUntil = class(TPasImplBlock)
  public
    ConditionExpr : TPasExpr;
    procedure FreeChildren(Prepare: boolean); override;
    Function Condition: TPasTreeString;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  { TPasImplIfElse }

  TPasImplIfElse = class(TPasImplBlock)
  public
    procedure FreeChildren(Prepare: boolean); override;
    procedure AddElement(Element: TPasImplElement); override;
    function CloseOnSemicolon: boolean; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    ConditionExpr: TPasExpr;
    IfBranch: TPasImplElement;
    ElseBranch: TPasImplElement; // can be nil
    Function Condition: TPasTreeString;
  end;

  { TPasImplWhileDo }

  TPasImplWhileDo = class(TPasImplStatement)
  public
    procedure FreeChildren(Prepare: boolean); override;
    procedure AddElement(Element: TPasImplElement); override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    ConditionExpr : TPasExpr;
    Body: TPasImplElement;
    function Condition: TPasTreeString;
  end;

  { TPasImplWithDo }

  TPasImplWithDo = class(TPasImplStatement)
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    procedure AddElement(Element: TPasImplElement); override;
    procedure AddExpression(const Expression: TPasExpr);
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Expressions: TFPList; // list of TPasExpr
    Body: TPasImplElement;
  end;

  { TPasInlineVarDeclStatement }

  TPasInlineVarDeclStatement = class(TPasImplStatement)
  public
    Declarations: TFPList; // list of TPasVariable
  Public
    constructor Create(const aName : TPasTreeString; aParent: TPasElement); override;
    procedure FreeChildren(Prepare: boolean); override;
    destructor Destroy; override;
  end;


  TPasImplCaseStatement = class;
  TPasImplCaseElse = class;

  { TPasImplCaseOf - Elements are TPasImplCaseStatement }

  TPasImplCaseOf = class(TPasImplBlock)
  public
    procedure FreeChildren(Prepare: boolean); override;
    function AddCase(const Expression: TPasExpr): TPasImplCaseStatement;
    function AddElse: TPasImplCaseElse;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    CaseExpr : TPasExpr;
    ElseBranch: TPasImplCaseElse; // this is also in Elements
    function Expression: TPasTreeString;
  end;

  { TPasImplCaseStatement }

  TPasImplCaseStatement = class(TPasImplStatement)
  public
    constructor Create(const AName: TPasTreeString; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure FreeChildren(Prepare: boolean); override;
    procedure AddElement(Element: TPasImplElement); override;
    procedure AddExpression(const Expr: TPasExpr);
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Expressions: TFPList; // list of TPasExpr
    Body: TPasImplElement;
  end;

  { TPasImplCaseElse }

  TPasImplCaseElse = class(TPasImplBlock)
  end;

  { TPasImplForLoop
    - for VariableName in StartExpr do Body
    - for VariableName := StartExpr to EndExpr do Body }

  TLoopType = (ltNormal,ltDown,ltIn);
  TPasImplForLoop = class(TPasImplStatement)
  public
    procedure FreeChildren(Prepare: boolean); override;
    procedure AddElement(Element: TPasImplElement); override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    VariableName : TPasExpr;
    LoopType : TLoopType;
    StartExpr : TPasExpr;
    EndExpr : TPasExpr; // if LoopType=ltIn this is nil
    Variable: TPasVariable; // not used by TPasParser
    VarType : TPasType; // For initialized variables
    ImplicitTyped : Boolean;
    Body: TPasImplElement;
    Function Down: boolean; inline;// downto, backward compatibility
    Function StartValue : TPasTreeString;
    Function EndValue: TPasTreeString;
  end;

  { TPasImplAssign }

  TAssignKind = (akDefault,akAdd,akMinus,akMul,akDivision);
  TPasImplAssign = class (TPasImplStatement)
  public
    Left  : TPasExpr;
    Right : TPasExpr;
    Kind : TAssignKind;
    procedure FreeChildren(Prepare: boolean); override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  { TPasImplSimple }

  TPasImplSimple = class (TPasImplStatement)
  public
    Expr  : TPasExpr;
    procedure FreeChildren(Prepare: boolean); override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  TPasImplTryHandler = class;
  TPasImplTryFinally = class;
  TPasImplTryExcept = class;
  TPasImplTryExceptElse = class;

  { TPasImplTry }

  TPasImplTry = class(TPasImplBlock)
  public
    procedure FreeChildren(Prepare: boolean); override;
    function AddFinally: TPasImplTryFinally;
    function AddExcept: TPasImplTryExcept;
    function AddExceptElse: TPasImplTryExceptElse;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    FinallyExcept: TPasImplTryHandler; // not in Elements
    ElseBranch: TPasImplTryExceptElse; // not in Elements
  end;

  TPasImplTryHandler = class(TPasImplBlock)
  end;

  { TPasImplTryFinally }

  TPasImplTryFinally = class(TPasImplTryHandler)
  end;

  { TPasImplTryExcept }

  TPasImplTryExcept = class(TPasImplTryHandler)
  end;

  { TPasImplTryExceptElse }

  TPasImplTryExceptElse = class(TPasImplTryHandler)
  end;

  { TPasImplExceptOn - Parent is TPasImplTryExcept }

  TPasImplExceptOn = class(TPasImplStatement)
  public
    procedure FreeChildren(Prepare: boolean); override;
    procedure AddElement(Element: TPasImplElement); override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
    procedure ClearTypeReferences(aType: TPasElement); override;
  public
    VarEl: TPasVariable; // can be nil
    TypeEl : TPasType; // if VarEl<>nil then TypeEl=VarEl.VarType
    Body: TPasImplElement;
    Function VariableName : TPasTreeString;
    Function TypeName: TPasTreeString;
  end;

  { TPasImplRaise }

  TPasImplRaise = class(TPasImplStatement)
  public
    procedure FreeChildren(Prepare: boolean); override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  Public
    ExceptObject,
    ExceptAddr : TPasExpr;
  end;

  { TPasImplLabelMark }

  TPasImplLabelMark = class(TPasImplElement)
  public
    LabelId: TPasTreeString;
  end;

  { TPasImplGoto }

  TPasImplGoto = class(TPasImplStatement)
  public
    LabelName: TPasTreeString;
  end;

  { TPassTreeVisitor }

  TPassTreeVisitor = class
  public
    procedure Visit(obj: TPasElement); virtual;
  end;


const
  AccessNames: array[TArgumentAccess] of TPasTreeString = ('', 'const ', 'var ', 'out ','constref ');
  AccessDescriptions: array[TArgumentAccess] of TPasTreeString = ('default', 'const', 'var', 'out','constref');
  AllVisibilities: TPasMemberVisibilities =
     [visDefault, visPrivate, visProtected, visPublic,
      visPublished, visAutomated];

  VisibilityNames: array[TPasMemberVisibility] of TPasTreeString = (
    'default','private', 'protected', 'public', 'published', 'automated',
    'strict private', 'strict protected','required','optional');

  ObjKindNames: array[TPasObjKind] of TPasTreeString = (
    'object', 'class', 'interface',
    'class helper','record helper','type helper',
    'dispinterface', 'ObjcClass', 'ObjcCategory',
    'ObjcProtocol');

  InterfaceTypeNames: array[TPasClassInterfaceType] of TPasTreeString = (
    'COM',
    'Corba'
    );

  ExprKindNames : Array[TPasExprKind] of TPasTreeString = (
      'Ident',
      'Number',
      'String',
      'StringMultiLine',
      'Set',
      'Nil',
      'BoolConst',
      'Range',
      'Unary',
      'Binary',
      'FuncParams',
      'ArrayParams',
      'ListOfExp',
      'Inherited',
      'Self',
      'Specialize',
      'Procedure',
      'NamedArg');

  OpcodeStrings : Array[TExprOpCode] of TPasTreeString = (
        '','+','-','*','/','div','mod','**',
        'shr','shl',
        'not','and','or','xor',
        '=','<>',
        '<','>','<=','>=',
        'in','is','as','><',
        '@','^','@@',
        '.');


  UnaryOperators = [otImplicit,otExplicit,otAssign,otNegative,otPositive,otEnumerator];

  OperatorTokens : Array[TOperatorType] of TPasTreeString
       =  ('','','','*','+','-','/','<','=',
           '>',':=','<>','<=','>=','**',
           '><','Inc','Dec','mod','-','+','Or','div',
           'shl','or','and','xor','and','not','xor',
           'shr','enumerator','in','','','','');
  OperatorNames : Array[TOperatorType] of TPasTreeString
       =  ('','implicit','explicit','multiply','add','subtract','divide','lessthan','equal',
           'greaterthan','assign','notequal','lessthanorequal','greaterthanorequal','power',
           'symmetricaldifference','inc','dec','modulus','negative','positive','bitwiseor','intdivide',
           'leftshift','logicalor','bitwiseand','bitwisexor','logicaland','logicalnot','logicalxor',
           'rightshift','enumerator','in','initialize','finalize','addref','copy');

  AssignKindNames : Array[TAssignKind] of TPasTreeString = (':=','+=','-=','*=','/=' );

  cPasMemberHint : Array[TPasMemberHint] of TPasTreeString =
      ( 'deprecated', 'library', 'platform', 'experimental', 'unimplemented' );
  cCallingConventions : Array[TCallingConvention] of TPasTreeString =
      ( '', 'Register','Pascal','cdecl','stdcall','OldFPCCall','safecall','SysCall','MWPascal',
                        'HardFloat','SysV_ABI_Default','SysV_ABI_CDecl',
                        'MS_ABI_Default','MS_ABI_CDecl',
                        'VectorCall','WinApi');
  ProcTypeModifiers : Array[TProcTypeModifier] of TPasTreeString =
      ('of Object', 'is nested','static','varargs','reference to','async','far','cblock');

  ModifierNames : Array[TProcedureModifier] of TPasTreeString
                = ('virtual', 'dynamic','abstract', 'override',
                   'export', 'overload', 'message', 'reintroduce',
                   'inline','assembler','public',
                   'compilerproc','external','forward','dispid',
                   'noreturn','far','final','discardresult','nostackframe',
                   'section','rtlproc','internproc','weakexternal');

  VariableModifierNames : Array[TVariableModifier] of TPasTreeString
     = ('cvar', 'external', 'public', 'export', 'class', 'static','far','thread');

procedure FreeProcNameParts(var NameParts: TProcedureNameParts);
procedure FreePasExprArray(Parent: TPasElement; var A: TPasExprArray; Prepare: boolean);

function GenericTemplateTypesAsString(List: TFPList): TPasTreeString;

function dbgs(const s: TProcTypeModifiers): TPasTreeString; overload;
function dbgs(const v: TPasMembersType.TRTTIVisibilitySection): TPasTreeString; overload;
function dbgs(const Sections: TPasMembersType.TRTTIVisibilitySections): TPasTreeString; overload;
function WritePasElTree(Expr: TPasExpr; FollowPrefix: TPasTreeString = ''): TPasTreeString;
function GetPasElementDesc(El: TPasElement): TPasTreeString;

{$IFDEF HasPTDumpStack}
procedure PTDumpStack;
function GetPTDumpStack: TPasTreeString;
{$ENDIF}

implementation

procedure FreeProcNameParts(var NameParts: TProcedureNameParts);
var
  i: Integer;
  p: TProcedureNamePart;
begin
  if NameParts=nil then exit;
  for i:=0 to NameParts.Count-1 do
    begin
    p:=TProcedureNamePart(NameParts[i]);
    p.Templates.Free;
    p.Free;
    end;
  NameParts.Free;
  NameParts:=nil;
end;

procedure FreePasExprArray(Parent: TPasElement; var A: TPasExprArray;
  Prepare: boolean);
var
  i: Integer;
begin
  for i:=0 to High(A) do
    Parent.FreeChild(A[i],Prepare);
  A:=nil;
end;

function GenericTemplateTypesAsString(List: TFPList): TPasTreeString;
var
  i, j: Integer;
  T: TPasGenericTemplateType;
begin
  Result:='';
  for i:=0 to List.Count-1 do
    begin
    T:=TPasGenericTemplateType(List[i]);
    if i>0 then
      if length(T.Constraints)>0 then
        Result:=Result+';'
      else
        Result:=Result+',';
    Result:=Result+T.Name;
    if length(T.Constraints)>0 then
      begin
      for j:=0 to length(T.Constraints)-1 do
        begin
        if j>0 then
          Result:=Result+',';
        Result:=Result+T.GetDeclaration(false);
        end;
      end;
    end;
  Result:='<'+Result+'>';
end;

function dbgs(const s: TProcTypeModifiers): TPasTreeString;
var
  m: TProcTypeModifier;
begin
  Result:='';
  for m in s do
    begin
    if Result<>'' then Result:=Result+',';
    Result:=Result+ProcTypeModifiers[m];
    end;
  Result:='['+Result+']';
end;

function dbgs(const v: TPasMembersType.TRTTIVisibilitySection): TPasTreeString;
begin
  str(v,Result);
end;

function dbgs(const Sections: TPasMembersType.TRTTIVisibilitySections): TPasTreeString;
var
  s: TPasMembersType.TRTTIVisibilitySection;
begin
  Result:='';
  for s in Sections do
    begin
    if Result<>'' then Result:=Result+',';
    Result:=Result+dbgs(s);
    end;
  Result:='['+Result+']';
end;

function WritePasElTree(Expr: TPasExpr; FollowPrefix: TPasTreeString): TPasTreeString;
{  TBinary Kind= OpCode=
    +Left=TBinary Kind= OpCode=
    | +Left=TParamsExpr[]
    | | +Value=Prim Kind= Value=
    | | +Params[1]=Prim Kind= Value=
    +Right=Prim
}
var
  C: TClass;
  s: TPasTreeString;
  ParamsExpr: TParamsExpr;
  InlineSpecExpr: TInlineSpecializeExpr;
  SubEl: TPasElement;
  ArrayValues: TArrayValues;
  i: Integer;
begin
  if Expr=nil then exit('nil');
  C:=Expr.ClassType;

  Result:=C.ClassName;
  str(Expr.Kind,s);
  Result:=Result+' '+s;
  str(Expr.OpCode,s);
  Result:=Result+' '+s;

  if C=TPrimitiveExpr then
    Result:=Result+' Value="'+TPrimitiveExpr(Expr).Value+'"'
  else if C=TUnaryExpr then
    Result:=Result+' Operand='+WritePasElTree(TUnaryExpr(Expr).Operand,FollowPrefix)
  else if C=TBoolConstExpr then
    Result:=Result+' Value='+BoolToStr(TBoolConstExpr(Expr).Value,'True','False')
  else if C=TArrayValues then
    begin
    ArrayValues:=TArrayValues(Expr);
    for i:=0 to length(ArrayValues.Values)-1 do
      Result:=Result+sLineBreak+FollowPrefix+'+Values['+IntToStr(i)+']='+WritePasElTree(ArrayValues.Values[i],FollowPrefix+'| ');
    end
  else if C=TBinaryExpr then
    begin
    Result:=Result+sLineBreak+FollowPrefix+'+Left='+WritePasElTree(TBinaryExpr(Expr).Left,FollowPrefix+'| ');
    Result:=Result+sLineBreak+FollowPrefix+'+Right='+WritePasElTree(TBinaryExpr(Expr).Right,FollowPrefix+'| ');
    end
  else if C=TParamsExpr then
    begin
    ParamsExpr:=TParamsExpr(Expr);
    Result:=Result+sLineBreak+FollowPrefix+'+Value='+WritePasElTree(ParamsExpr.Value,FollowPrefix+'| ');
    for i:=0 to length(ParamsExpr.Params)-1 do
      Result:=Result+sLineBreak+FollowPrefix+'+Params['+IntToStr(i)+']='+WritePasElTree(ParamsExpr.Params[i],FollowPrefix+'| ');
    end
  else if C=TInlineSpecializeExpr then
    begin
    InlineSpecExpr:=TInlineSpecializeExpr(Expr);
    Result:=Result+sLineBreak+FollowPrefix+'+Name='+WritePasElTree(InlineSpecExpr.NameExpr,FollowPrefix+'| ');
    if InlineSpecExpr.Params<>nil then
      for i:=0 to InlineSpecExpr.Params.Count-1 do
        begin
        Result:=Result+sLineBreak+FollowPrefix+'+Params['+IntToStr(i)+']=';
        SubEl:=TPasElement(InlineSpecExpr.Params[i]);
        if SubEl=nil then
          Result:=Result+'nil'
        else if SubEl is TPasExpr then
          Result:=Result+WritePasElTree(TPasExpr(SubEl),FollowPrefix+'| ')
        else
          Result:=Result+SubEl.Name+':'+SubEl.ClassName;
        end;
    end
  else
    Result:=C.ClassName+' Kind=';
end;

function GetPasElementDesc(El: TPasElement): TPasTreeString;
begin
  if El=nil then exit('nil');
  Result:=El.Name+':'+El.ClassName+'['+El.SourceFilename+','+IntToStr(El.SourceLinenumber)+']';
end;

Function IndentStrings(S : TStrings; indent : Integer) : TPasTreeString;
Var
  I,CurrLen,CurrPos : Integer;
begin
  Result:='';
  CurrLen:=0;
  CurrPos:=0;
  For I:=0 to S.Count-1 do
    begin
    CurrLen:=Length(S[i]);
    If (CurrLen+CurrPos)>72 then
      begin
      Result:=Result+LineEnding+StringOfChar(' ',Indent);
      CurrPos:=Indent;
      end;
    Result:=Result+S[i];
    CurrPos:=CurrPos+CurrLen;
    end;
end;

{ TPasGenericType }

destructor TPasGenericType.Destroy;
begin
  FreeAndNil(GenericTemplateTypes);
  inherited Destroy;
end;

procedure TPasGenericType.FreeChildren(Prepare: boolean);
begin
  FreeChildList(GenericTemplateTypes,Prepare);
  inherited FreeChildren(Prepare);
end;

procedure TPasGenericType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  if GenericTemplateTypes<>nil then
    for i:=0 to GenericTemplateTypes.Count-1 do
      ForEachChildCall(aMethodCall,Arg,TPasElement(GenericTemplateTypes[i]),false);
end;

procedure TPasGenericType.SetGenericTemplates(AList: TFPList);
var
  I: Integer;
  El: TPasElement;
begin
  if GenericTemplateTypes=nil then
    GenericTemplateTypes:=TFPList.Create;
  For I:=0 to AList.Count-1 do
    begin
    El:=TPasElement(AList[i]);
    El.Parent:=Self;
    GenericTemplateTypes.Add(El);
    end;
  AList.Clear;
end;

{ TPasGenericTemplateType }

destructor TPasGenericTemplateType.Destroy;
begin
  inherited Destroy;
end;

procedure TPasGenericTemplateType.FreeChildren(Prepare: boolean);
begin
  FreeChildArray(Constraints,Prepare);
  inherited FreeChildren(Prepare);
end;

function TPasGenericTemplateType.GetDeclaration(full: boolean): TPasTreeString;
var
  i: Integer;
begin
  Result:=inherited GetDeclaration(full);
  if IsConst then
    Result:='const '+Result;
  if length(Constraints)>0 then
    begin
    Result:=Result+': ';
    for i:=0 to length(Constraints)-1 do
      begin
      if i>0 then
        Result:=Result+',';
      Result:=Result+Constraints[i].GetDeclaration(True);
      end;
    end;
end;

procedure TPasGenericTemplateType.ForEachCall(
  const aMethodCall: TOnForEachPasElement; const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to length(Constraints)-1 do
    ForEachChildCall(aMethodCall,Arg,Constraints[i],false);
end;

procedure TPasGenericTemplateType.AddConstraint(El: TPasElement);
var
  l: Integer;
begin
  l:=Length(Constraints);
  SetLength(Constraints,l+1);
  Constraints[l]:=El;
end;

procedure TPasGenericTemplateType.ClearTypeReferences(aType: TPasElement);
var
  i: SizeInt;
  aConstraint: TPasElement;
begin
  for i:=length(Constraints)-1 downto 0 do
    begin
    aConstraint:=Constraints[i];
    if aConstraint=aType then
      Constraints[i]:=nil;
    end;
end;

{$IFDEF HasPTDumpStack}
procedure PTDumpStack;
begin
  {AllowWriteln}
  writeln(GetPTDumpStack);
  {AllowWriteln-}
end;

function GetPTDumpStack: TPasTreeString;
var
  bp: Pointer;
  addr: Pointer;
  oldbp: Pointer;
  CurAddress: Shortstring;
begin
  Result:='';
  { retrieve backtrace info }
  bp:=get_caller_frame(get_frame);
  while bp<>nil do begin
    addr:=get_caller_addr(bp);
    CurAddress:=BackTraceStrFunc(addr);
    Result:=Result+CurAddress+LineEnding;
    oldbp:=bp;
    bp:=get_caller_frame(bp);
    if (bp<=oldbp) or (bp>(StackBottom + StackLength)) then
      bp:=nil;
  end;
end;
{$ENDIF}

{ TPasAttributes }

procedure TPasAttributes.FreeChildren(Prepare: boolean);
begin
  FreePasExprArray(Self,Calls,Prepare);
  inherited FreeChildren(Prepare);
end;

procedure TPasAttributes.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to length(Calls)-1 do
    ForEachChildCall(aMethodCall,Arg,Calls[i],false);
end;

procedure TPasAttributes.AddCall(Expr: TPasExpr);
var
  i : Integer;
begin
  i:=Length(Calls);
  SetLength(Calls, i+1);
  Calls[i]:=Expr;
end;

{ TPasMethodResolution }

procedure TPasMethodResolution.FreeChildren(Prepare: boolean);
begin
  InterfaceName:=TPasExpr(FreeChild(InterfaceName,Prepare));
  InterfaceProc:=TPasExpr(FreeChild(InterfaceProc,Prepare));
  ImplementationProc:=TPasExpr(FreeChild(ImplementationProc,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasMethodResolution.ForEachCall(
  const aMethodCall: TOnForEachPasElement; const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,InterfaceName,false);
  ForEachChildCall(aMethodCall,Arg,InterfaceProc,false);
  ForEachChildCall(aMethodCall,Arg,ImplementationProc,false);
end;

{ TPasImplCommandBase }

constructor TPasImplCommandBase.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  SemicolonAtEOL := true;
end;

{ TInlineSpecializeExpr }

constructor TInlineSpecializeExpr.Create(const AName: TPasTreeString;
  AParent: TPasElement);
begin
  if AName='' then ;
  inherited Create(AParent, pekSpecialize, eopNone);
  Params:=TFPList.Create;
end;

destructor TInlineSpecializeExpr.Destroy;
begin
  FreeAndNil(Params);
  inherited Destroy;
end;

procedure TInlineSpecializeExpr.FreeChildren(Prepare: boolean);
begin
  NameExpr:=TPasExpr(FreeChild(NameExpr,Prepare));
  FreeChildList(Params,Prepare);
  inherited FreeChildren(Prepare);
end;

procedure TInlineSpecializeExpr.ClearTypeReferences(aType: TPasElement);
var
  i: Integer;
  El: TPasElement;
begin
  for i:=Params.Count-1 downto 0 do
    begin
    El:=TPasElement(Params[i]);
    if El=aType then
      Params.Delete(i);
    end;
end;

function TInlineSpecializeExpr.ElementTypeName: TPasTreeString;
begin
  Result:=SPasTreeSpecializedExpr;
end;

function TInlineSpecializeExpr.GetDeclaration(full: Boolean): TPasTreeString;
var
  i: Integer;
  aParam: TPasElement;
  lTmp : String;
begin
  Result:='specialize '+NameExpr.GetDeclaration(false)+'<';
  for i:=0 to Params.Count-1 do
    begin
    if i>0 then
      Result:=Result+',';
    aParam:=TPasElement(Params[i]);
    if aParam is TPasMembersType then
      Result:=Result+aParam.FullName
    else
      begin
      lTmp:=aParam.GetDeclaration(aParam is TPasUnresolvedTypeRef);
      lTmp[1]:=UpCase(lTmp[1]);
      Result:=Result+lTmp;
      end;
    end;
  Result:=Result+'>';
  if full then ;
end;

procedure TInlineSpecializeExpr.ForEachCall(
  const aMethodCall: TOnForEachPasElement; const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,NameExpr,false);
  for i:=0 to Params.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Params[i]),true);
end;

{ TPasSpecializeType }

constructor TPasSpecializeType.Create(const AName: TPasTreeString; AParent: TPasElement
  );
begin
  inherited Create(AName, AParent);
  Params:=TFPList.Create;
end;

destructor TPasSpecializeType.Destroy;
begin
  FreeAndNil(Params);
  inherited Destroy;
end;

procedure TPasSpecializeType.FreeChildren(Prepare: boolean);
begin
  FreeChildList(Params,Prepare);
  inherited FreeChildren(Prepare);
end;

procedure TPasSpecializeType.ClearTypeReferences(aType: TPasElement);
var
  i: Integer;
  El: TPasElement;
begin
  inherited ClearTypeReferences(aType);
  for i:=Params.Count-1 downto 0 do
    begin
    El:=TPasElement(Params[i]);
    if El=aType then
      Params.Delete(i);
    end;
end;

function TPasSpecializeType.ElementTypeName: TPasTreeString;
begin
  Result:=SPasTreeSpecializedType;
end;

function TPasSpecializeType.GetDeclaration(full: boolean): TPasTreeString;
var
  i: Integer;
  aParam: TPasElement;
  lTmp : String;
begin
  Result:='specialize '+DestType.Name+'<';
  for i:=0 to Params.Count-1 do
    begin
    if i>0 then
      Result:=Result+',';
    aParam:=TPasElement(Params[i]);
    if aParam is TPasMembersType then
      Result:=Result+aParam.FullName
    else
      begin
      lTmp:=aParam.GetDeclaration(aParam is TPasUnresolvedTypeRef);
      lTmp[1]:=Upcase(lTmp[1]);
      Result:=Result+lTmp;
      end;
    end;
  Result:=Result+'>';
  If Full and (Name<>'') then
    begin
    Result:=Name+' = '+Result;
    ProcessHints(False,Result);
    end;
end;

procedure TPasSpecializeType.ForEachCall(
  const aMethodCall: TOnForEachPasElement; const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Params.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Params[i]),true);
end;

{ TInterfaceSection }

function TInterfaceSection.ElementTypeName: TPasTreeString;
begin
  Result:=SPasTreeInterfaceSection;
end;

{ TLibrarySection }

function TLibrarySection.ElementTypeName: TPasTreeString;
begin
  Result:=SPasTreeLibrarySection;
end;

{ TPasPackageSection }

constructor TPasPackageSection.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Requires:=TFPList.Create;
end;

destructor TPasPackageSection.Destroy;
begin
  FreeandNil(Requires);
  inherited Destroy;
end;

procedure TPasPackageSection.FreeChildren(Prepare: boolean);
begin
  FreeChildList(Requires,Prepare);
  inherited FreeChildren(Prepare);
end;

function TPasPackageSection.ElementTypeName: TPasTreeString;
begin
  Result:=SPasTreePackageSection;
end;

{ TProgramSection }

function TProgramSection.ElementTypeName: TPasTreeString;
begin
  Result:=SPasTreeProgramSection;
end;

{ TImplementationSection }

function TImplementationSection.ElementTypeName: TPasTreeString;
begin
  Result:=SPasTreeImplementationSection;
end;

{ TPasUsesUnit }

procedure TPasUsesUnit.FreeChildren(Prepare: boolean);
begin
  Expr:=TPasExpr(FreeChild(Expr,Prepare));
  InFilename:=TPrimitiveExpr(FreeChild(InFilename,Prepare));
  Module:=TPasModule(FreeChild(Module,Prepare));
  inherited FreeChildren(Prepare);
end;

function TPasUsesUnit.ElementTypeName: TPasTreeString;
begin
  Result := SPasTreeUsesUnit;
end;

procedure TPasUsesUnit.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Expr,false);
  ForEachChildCall(aMethodCall,Arg,InFilename,false);
  ForEachChildCall(aMethodCall,Arg,Module,true);
end;

{ TPasElementBase }

procedure TPasElementBase.Accept(Visitor: TPassTreeVisitor);
begin
  if Visitor=nil then ;
end;

{ TPasTypeRef }

procedure TPasTypeRef.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,RefType,true);
end;

{ TPasClassOperator }

function TPasClassOperator.TypeName: TPasTreeString;
begin
  Result:='class operator';
end;

function TPasClassOperator.GetProcTypeEnum: TProcType;
begin
  Result:=ptClassOperator;
end;

{ TPasImplAsmStatement }

constructor TPasImplAsmStatement.Create(const AName: TPasTreeString;
  AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  FTokens:=TStringList.Create;
  FModifierTokens:=TStringList.Create;
end;

destructor TPasImplAsmStatement.Destroy;
begin
  FreeAndNil(FTokens);
  FreeAndNil(FModifierTokens);
  inherited Destroy;
end;

{ TPasClassConstructor }

function TPasClassConstructor.TypeName: TPasTreeString;
begin
  Result:='class '+ inherited TypeName;
end;

function TPasClassConstructor.GetProcTypeEnum: TProcType;
begin
  Result:=ptClassConstructor;
end;

{ TPasAnonymousProcedure }

function TPasAnonymousProcedure.ElementTypeName: TPasTreeString;
begin
  Result:=SPasTreeAnonymousProcedure;
end;

function TPasAnonymousProcedure.TypeName: TPasTreeString;
begin
  Result:='anonymous procedure';
end;

function TPasAnonymousProcedure.GetProcTypeEnum: TProcType;
begin
  Result:=ptAnonymousProcedure;
end;

{ TPasAnonymousFunction }

function TPasAnonymousFunction.GetFT: TPasFunctionType;
begin
  Result:=ProcType as TPasFunctionType;
end;

function TPasAnonymousFunction.ElementTypeName: TPasTreeString;
begin
  Result := SPasTreeAnonymousFunction;
end;

function TPasAnonymousFunction.TypeName: TPasTreeString;
begin
  Result:='anonymous function';
end;

function TPasAnonymousFunction.GetProcTypeEnum: TProcType;
begin
  Result:=ptAnonymousFunction;
end;

{ TProcedureExpr }

constructor TProcedureExpr.Create(AParent: TPasElement);
begin
  inherited Create(AParent,pekProcedure,eopNone);
end;

procedure TProcedureExpr.FreeChildren(Prepare: boolean);
begin
  Proc:=TPasAnonymousProcedure(FreeChild(Proc,Prepare));
  inherited FreeChildren(Prepare);
end;

function TProcedureExpr.GetDeclaration(full: Boolean): TPasTreeString;
begin
  if Proc<>nil then
    Result:=Proc.GetDeclaration(full)
  else
    Result:='procedure-expr';
end;

procedure TProcedureExpr.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Proc,false);
end;

{ TPasImplRaise }

procedure TPasImplRaise.FreeChildren(Prepare: boolean);
begin
  ExceptObject:=TPasExpr(FreeChild(ExceptObject,Prepare));
  ExceptAddr:=TPasExpr(FreeChild(ExceptAddr,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasImplRaise.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,ExceptObject,false);
  ForEachChildCall(aMethodCall,Arg,ExceptAddr,false);
end;

{ TPasImplRepeatUntil }

procedure TPasImplRepeatUntil.FreeChildren(Prepare: boolean);
begin
  ConditionExpr:=TPasExpr(FreeChild(ConditionExpr,Prepare));
  inherited FreeChildren(Prepare);
end;

function TPasImplRepeatUntil.Condition: TPasTreeString;
begin
  If Assigned(ConditionExpr) then
    Result:=ConditionExpr.GetDeclaration(True)
  else
    Result:='';
end;

procedure TPasImplRepeatUntil.ForEachCall(
  const aMethodCall: TOnForEachPasElement; const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,ConditionExpr,false);
end;

{ TPasImplSimple }

procedure TPasImplSimple.FreeChildren(Prepare: boolean);
begin
  Expr:=TPasExpr(FreeChild(Expr,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasImplSimple.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Expr,false);
end;

{ TPasImplAssign }

procedure TPasImplAssign.FreeChildren(Prepare: boolean);
begin
  Left:=TPasExpr(FreeChild(Left,Prepare));
  Right:=TPasExpr(FreeChild(Right,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasImplAssign.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Left,false);
  ForEachChildCall(aMethodCall,Arg,Right,false);
end;

{ TPasExportSymbol }

procedure TPasExportSymbol.FreeChildren(Prepare: boolean);
begin
  NameExpr:=TPasExpr(FreeChild(NameExpr,Prepare));
  ExportName:=TPasExpr(FreeChild(ExportName,Prepare));
  ExportIndex:=TPasExpr(FreeChild(ExportIndex,Prepare));
  inherited FreeChildren(Prepare);
end;

function TPasExportSymbol.ElementTypeName: TPasTreeString;
begin
  Result:='Export'
end;

function TPasExportSymbol.GetDeclaration(full: boolean): TPasTreeString;
begin
  Result:=Name;
  if (ExportName<>Nil) then
    Result:=Result+' name '+ExportName.GetDeclaration(Full)
  else if (ExportIndex<>Nil) then
    Result:=Result+' index '+ExportIndex.GetDeclaration(Full);
end;

procedure TPasExportSymbol.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,NameExpr,false);
  ForEachChildCall(aMethodCall,Arg,ExportName,false);
  ForEachChildCall(aMethodCall,Arg,ExportIndex,false);
end;

{ TPasUnresolvedUnitRef }

function TPasUnresolvedUnitRef.ElementTypeName: TPasTreeString;
begin
  Result:=SPasTreeUnit;
end;

{ TPasLibrary }

procedure TPasLibrary.FreeChildren(Prepare: boolean);
begin
  LibrarySection:=TLibrarySection(FreeChild(LibrarySection,Prepare));
  inherited FreeChildren(Prepare);
end;

function TPasLibrary.ElementTypeName: TPasTreeString;
begin
  Result:=inherited ElementTypeName;
end;

procedure TPasLibrary.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  ForEachChildCall(aMethodCall,Arg,LibrarySection,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

{ TPasProgram }

procedure TPasProgram.FreeChildren(Prepare: boolean);
begin
  ProgramSection:=TProgramSection(FreeChild(ProgramSection,Prepare));
  inherited FreeChildren(Prepare);
end;

function TPasProgram.ElementTypeName: TPasTreeString;
begin
  Result:=inherited ElementTypeName;
end;

procedure TPasProgram.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  ForEachChildCall(aMethodCall,Arg,ProgramSection,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

{ TPasUnitModule }

function TPasUnitModule.ElementTypeName: TPasTreeString;
begin
  Result:=SPasTreeUnit;
end;

{ Parse tree element type name functions }
function TPasElement.ElementTypeName: TPasTreeString; begin Result := SPasTreeElement end;

function TPasElement.HintsString: TPasTreeString;

Var
  H : TPasmemberHint;

begin
  Result:='';
  For H := Low(TPasmemberHint) to High(TPasMemberHint) do
    if H in Hints then
      begin
      If (Result<>'') then
        Result:=Result+'; ';
      Result:=Result+cPasMemberHint[h];
      end;
end;

function TPasDeclarations.ElementTypeName: TPasTreeString; begin Result := SPasTreeSection end;

procedure TPasDeclarations.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Declarations.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Declarations[i]),false);
end;

function TPasModule.ElementTypeName: TPasTreeString; begin Result := SPasTreeModule end;
function TPasPackage.ElementTypeName: TPasTreeString; begin Result := SPasTreePackage end;

procedure TPasPackage.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Modules.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasModule(Modules[i]),true);
end;

function TPasResString.ElementTypeName: TPasTreeString; begin Result := SPasTreeResString; end;

function TPasType.FixTypeDecl(aDecl: TPasTreeString): TPasTreeString;
begin
  Result:=aDecl;
  if (Name<>'') then
    Result:=SafeName+' = '+Result;
  ProcessHints(false,Result);
end;

function TPasType.SafeName: TPasTreeString;
begin
  if SameText(Name,'TPasTreeString') then
    Result:=Name
  else
    Result:=inherited SafeName;
end;

function TPasType.ElementTypeName: TPasTreeString; begin Result := SPasTreeType; end;
function TPasPointerType.ElementTypeName: TPasTreeString; begin Result := SPasTreePointerType; end;
function TPasAliasType.ElementTypeName: TPasTreeString; begin Result := SPasTreeAliasType; end;
function TPasTypeAliasType.ElementTypeName: TPasTreeString; begin Result := SPasTreeTypeAliasType; end;

function TPasTypeAliasType.GetDeclaration(Full: boolean): TPastreeString;


begin
  Result:='type '+DestType.GetDeclaration(False);
  if Full then
    Result:=FixTypeDecl(Result);
end;

function TPasClassOfType.ElementTypeName: TPasTreeString; begin Result := SPasTreeClassOfType; end;
function TPasRangeType.ElementTypeName: TPasTreeString; begin Result := SPasTreeRangeType; end;
function TPasArrayType.ElementTypeName: TPasTreeString; begin Result := SPasTreeArrayType; end;
function TPasFileType.ElementTypeName: TPasTreeString; begin Result := SPasTreeFileType; end;
function TPasEnumValue.ElementTypeName: TPasTreeString; begin Result := SPasTreeEnumValue; end;

procedure TPasEnumValue.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Value,false);
end;

procedure TPasEnumValue.FreeChildren(Prepare: boolean);
begin
  Value:=TPasExpr(FreeChild(Value,Prepare));
  inherited FreeChildren(Prepare);
end;

function TPasEnumValue.AssignedValue: TPasTreeString;
begin
  If Assigned(Value) then
    Result:=Value.GetDeclaration(True)
  else
    Result:='';
end;

function TPasEnumType.ElementTypeName: TPasTreeString; begin Result := SPasTreeEnumType end;
function TPasSetType.ElementTypeName: TPasTreeString; begin Result := SPasTreeSetType end;
function TPasRecordType.ElementTypeName: TPasTreeString; begin Result := SPasTreeRecordType end;
function TPasArgument.ElementTypeName: TPasTreeString; begin Result := SPasTreeArgument end;
function TPasProcedureType.ElementTypeName: TPasTreeString; begin Result := SPasTreeProcedureType end;
function TPasResultElement.ElementTypeName: TPasTreeString; begin Result := SPasTreeResultElement end;

procedure TPasResultElement.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,ResultType,true);
end;

procedure TPasResultElement.ClearTypeReferences(aType: TPasElement);
begin
  if ResultType=aType then
    ResultType:=nil
end;

function TPasFunctionType.ElementTypeName: TPasTreeString; begin Result := SPasTreeFunctionType end;
function TPasUnresolvedTypeRef.ElementTypeName: TPasTreeString; begin Result := SPasTreeUnresolvedTypeRef end;
function TPasVariable.ElementTypeName: TPasTreeString; begin Result := SPasTreeVariable end;
function TPasConst.ElementTypeName: TPasTreeString; begin Result := SPasTreeConst end;
function TPasProperty.ElementTypeName: TPasTreeString; begin Result := SPasTreeProperty end;
function TPasOverloadedProc.ElementTypeName: TPasTreeString; begin Result := SPasTreeOverloadedProcedure end;
function TPasProcedure.ElementTypeName: TPasTreeString; begin Result := SPasTreeProcedure end;

function TPasFunction.GetFT: TPasFunctionType;
begin
  Result:=ProcType as TPasFunctionType;
end;

function TPasFunction.ElementTypeName: TPasTreeString; begin Result := SPasTreeFunction; end;
function TPasClassProcedure.ElementTypeName: TPasTreeString; begin Result := SPasTreeClassProcedure; end;
function TPasClassConstructor.ElementTypeName: TPasTreeString; begin Result := SPasTreeClassConstructor; end;
function TPasClassDestructor.ElementTypeName: TPasTreeString; begin Result := SPasTreeClassDestructor; end;

function TPasClassDestructor.TypeName: TPasTreeString;
begin
  Result:='destructor';
end;

function TPasClassDestructor.GetProcTypeEnum: TProcType;
begin
  Result:=ptClassDestructor;
end;

function TPasClassFunction.ElementTypeName: TPasTreeString; begin Result := SPasTreeClassFunction; end;

class function TPasOperator.OperatorTypeToToken(T: TOperatorType): TPasTreeString;
begin
  Result:=OperatorTokens[T];
end;

class function TPasOperator.OperatorTypeToOperatorName(T: TOperatorType
  ): TPasTreeString;
begin
  Result:=OperatorNames[T];
end;

class function TPasOperator.TokenToOperatorType(S: TPasTreeString): TOperatorType;
begin
  Result:=High(TOperatorType);
  While (Result>otUnknown) and (CompareText(S,OperatorTokens[Result])<>0) do
    Result:=Pred(Result);
end;

class function TPasOperator.NameToOperatorType(S: TPasTreeString): TOperatorType;
begin
  Result:=High(TOperatorType);
  While (Result>otUnknown) and (CompareText(S,OperatorNames[Result])<>0) do
    Result:=Pred(Result);
end;

function TPasOperator.NameSuffix: TPasTreeString;

Var
  I : Integer;

begin
  Result:='(';
  if Assigned(ProcType) and Assigned(ProcType.Args) then
  for i:=0 to ProcType.Args.Count-1 do
    begin
    if i>0 then
      Result:=Result+',';
    Result:=Result+TPasArgument(ProcType.Args[i]).ArgType.Name;
    end;
  Result:=Result+')';
  if Assigned(TPasFunctionType(ProcType)) and
     Assigned(TPasFunctionType(ProcType).ResultEl) and
     Assigned(TPasFunctionType(ProcType).ResultEl.ResultType) then
    Result:=Result+':'+TPasFunctionType(ProcType).ResultEl.ResultType.Name;
end;

procedure TPasOperator.CorrectName;

var
  DotPos: Integer;

begin
  DotPos:=Pos('.',Name);
  if DotPos>0 then
    Name:=Copy(Name,1,DotPos)+OperatorNames[OperatorType]+NameSuffix
  else
    Name:=OperatorNames[OperatorType]+NameSuffix;
end;

function TPasOperator.OldName(WithPath : Boolean): TPasTreeString;

Var
  I : Integer;
  S : TPasTreeString;
begin
  Result:=TypeName+' '+OperatorTokens[OperatorType];
  Result := Result + '(';
  if Assigned(ProcType) then
    begin
    for i := 0 to ProcType.Args.Count - 1 do
      begin
      if i > 0 then
        Result := Result + ', ';
      Result := Result + TPasArgument(ProcType.Args[i]).ArgType.Name;
      end;
    Result := Result + ')';
    if (OperatorType<>otInitialize) and Assigned(TPasFunctionType(ProcType).ResultEl.ResultType) then
       Result:=Result+': ' + TPasFunctionType(ProcType).ResultEl.ResultType.Name;
    If WithPath then
      begin
      S:=Self.ParentPath;
      if (S<>'') then
        Result:=S+'.'+Result;
      end;
    end;
end;

function TPasOperator.ElementTypeName: TPasTreeString;
begin
  Result := SPasTreeOperator
end;

function TPasConstructor.ElementTypeName: TPasTreeString; begin Result := SPasTreeConstructor end;
function TPasDestructor.ElementTypeName: TPasTreeString; begin Result := SPasTreeDestructor end;
function TPasProcedureImpl.ElementTypeName: TPasTreeString; begin Result := SPasTreeProcedureImpl end;
function TPasConstructorImpl.ElementTypeName: TPasTreeString; begin Result := SPasTreeConstructorImpl end;
function TPasDestructorImpl.ElementTypeName: TPasTreeString; begin Result := SPasTreeDestructorImpl end;
function TPasStringType.ElementTypeName: TPasTreeString; begin Result:=SPasStringType;end;

function TPasStringType.GetDeclaration(full: Boolean): TPasTreeString;
begin
  Result:='string';
  if full then
    begin
    if LengthExpr<>'' then
       Result:=Result+'['+LengthExpr+']';
    if CodePageExpr<>'' then
       Result:=Result+'('+CodePageExpr+')';
    end;
end;


{ All other stuff: }

procedure TPasElement.ProcessHints(const ASemiColonPrefix: boolean; var AResult: TPasTreeString);
var
  S : TPasTreeString;
begin
  if Hints <> [] then
    begin
    if ASemiColonPrefix then
      AResult := AResult + ';';
    S:=HintsString;
    if (S<>'') then
      AResult:=AResult+' '+S;
    if ASemiColonPrefix then
      AResult:=AResult+';';
    end;
end;

procedure TPasElement.SetParent(const AValue: TPasElement);
begin
  FParent:=AValue;
end;

constructor TPasElement.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create;
  FName := AName;
  FParent := AParent;
  {$ifdef pas2js}
  inc(FLastPasElementId);
  FPasElementId:=FLastPasElementId;
  //writeln('TPasElement.Create ',Name,':',ClassName,' ID=[',FPasElementId,']');
  {$endif}
end;

destructor TPasElement.Destroy;
begin
  FParent:=nil;
  inherited Destroy;
end;

class function TPasElement.IsKeyWord(const S: TPasTreeString): Boolean;

Const
   KW=';absolute;and;array;asm;begin;case;const;constructor;destructor;div;do;'+
       'downto;else;end;file;for;function;goto;if;implementation;in;inherited;'+
       'inline;interface;label;mod;nil;not;object;of;on;operator;or;packed;'+
       'procedure;program;record;reintroduce;repeat;self;set;shl;shr;TPasTreeString;then;'+
       'to;type;unit;until;uses;var;while;with;xor;dispose;exit;false;new;true;'+
       'as;class;dispinterface;except;exports;finalization;finally;initialization;'+
       'inline;is;library;on;out;packed;property;raise;resourcestring;threadvar;try;'+
       'private;published;length;setlength;';

begin
  Result:=Pos(';'+lowercase(S)+';',KW)<>0;
end;

class function TPasElement.EscapeKeyWord(const S: TPasTreeString): TPasTreeString;
begin
  Result:=S;
  If IsKeyWord(Result) then
    Result:='&'+Result;
end;

function TPasElement.FreeChild(Child: TPasElement; Prepare: boolean
  ): TPasElement;
begin
  if Child=nil then
    exit(nil)
  else if Prepare then
    begin
    if Child.Parent=Self then
      begin
      Child.FreeChildren(true);
      exit(Child); // keep reference
      end
    else
      exit(nil); // clear reference
    end
  else
    begin
    Child.FreeChildren(false);
    Child.Free;
    Result:=nil;
    end;
end;

procedure TPasElement.FreeChildList(List: TFPList; Prepare: boolean);
var
  i: Integer;
begin
  if List=nil then exit;
  for i:=0 to List.Count-1 do
    List[i]:=FreeChild(TPasElement(List[i]),Prepare);
  List.Clear;
end;

procedure TPasElement.FreeChildArray(A: TPasElementArray; Prepare: boolean);
var
  i: Integer;
begin
  for i:=0 to High(A) do
    A[i]:=FreeChild(A[i],Prepare);
end;

procedure TPasElement.FreeChildren(Prepare: boolean);
begin
  if Prepare then ;
end;

procedure TPasElement.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  aMethodCall(Self,Arg);
end;

procedure TPasElement.ForEachChildCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer; Child: TPasElement; CheckParent: boolean);
begin
  if (Child=nil) then exit;
  if CheckParent and (not Child.HasParent(Self)) then exit;
  Child.ForEachCall(aMethodCall,Arg);
end;

function TPasElement.SafeName: TPasTreeString;
begin
  Result:=Name;
  if IsKeyWord(Result) then
    Result:='&'+Result;
end;

function TPasElement.FullPath: TPasTreeString;

var
  p: TPasElement;

begin
  Result := '';
  p := Parent;
  while Assigned(p) and not p.InheritsFrom(TPasDeclarations) do
  begin
    if (p.Name<>'') and (Not (p is TPasOverloadedProc)) then
      if Length(Result) > 0 then
        Result := p.Name + '.' + Result
      else
        Result := p.Name;
    p := p.Parent;
  end;
end;

function TPasElement.FullName: TPasTreeString;


begin
  Result := FullPath;
  if Result<>'' then
    Result:=Result+'.'+Name
  else
    Result:=Name;
end;

function TPasElement.ParentPath: TPasTreeString;

var
  p: TPasElement;
begin
  Result:='';
  p := Parent;
  while Assigned(p) do
  begin
    if (p.Name<>'') and (Not (p is TPasOverloadedProc)) then
      if Length(Result) > 0 then
        Result := p.Name + '.' + Result
      else
        Result := p.Name;
    p := p.Parent;
  end;
end;

function TPasElement.PathName: TPasTreeString;

begin
  Result := ParentPath;
  if Result<>'' then
    Result:=Result+'.'+Name
  else
    Result:=Name;
end;

function TPasElement.GetModule: TPasModule;

Var
  p : TPaselement;
begin
  if Self is TPasPackage then
    Result := nil
  else
    begin
    P:=Self;
    While (P<>Nil) and Not (P is TPasModule) do
      P:=P.Parent;
    Result:=TPasModule(P);
    end;
end;

function TPasElement.GetDeclaration(full: Boolean): TPasTreeString;

begin
  if Full then
    Result := SafeName
  else
    Result := '';
end;

procedure TPasElement.Accept(Visitor: TPassTreeVisitor);
begin
  Visitor.Visit(Self);
end;

procedure TPasElement.ClearTypeReferences(aType: TPasElement);
begin
  if aType=nil then ;
end;

function TPasElement.HasParent(aParent: TPasElement): boolean;
var
  El: TPasElement;
begin
  El:=Parent;
  while El<>nil do
    begin
    if El=aParent then exit(true);
    El:=El.Parent;
    end;
  Result:=false;
end;

constructor TPasDeclarations.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Declarations := TFPList.Create;
  Attributes := TFPList.Create;
  Classes := TFPList.Create;
  Consts := TFPList.Create;
  ExportSymbols := TFPList.Create;
  Functions := TFPList.Create;
  Properties := TFPList.Create;
  ResStrings := TFPList.Create;
  Types := TFPList.Create;
  Labels := TFPList.Create;
  Variables := TFPList.Create;
end;

destructor TPasDeclarations.Destroy;
begin
  {$IFDEF VerbosePasTreeMem}writeln('TPasDeclarations.Destroy START');{$ENDIF}
  FreeAndNil(Variables);
  FreeAndNil(Types);
  FreeAndNil(ResStrings);
  FreeAndNil(Properties);
  FreeAndNil(Functions);
  FreeAndNil(ExportSymbols);
  FreeAndNil(Consts);
  FreeAndNil(Classes);
  FreeAndNil(Attributes);
  FreeAndNil(Labels);
  FreeAndNil(Declarations);

  {$IFDEF VerbosePasTreeMem}writeln('TPasDeclarations.Destroy inherited');{$ENDIF}
  inherited Destroy;
  {$IFDEF VerbosePasTreeMem}writeln('TPasDeclarations.Destroy END');{$ENDIF}
end;

procedure TPasDeclarations.FreeChildren(Prepare: boolean);
begin
  FreeChildList(Declarations,Prepare);
  inherited FreeChildren(Prepare);
end;

procedure TPasModule.FreeChildren(Prepare: boolean);
begin
  GlobalDirectivesSection:=TPasImplCommandBase(FreeChild(GlobalDirectivesSection,Prepare));
  InterfaceSection:=TInterfaceSection(FreeChild(InterfaceSection,Prepare));
  ImplementationSection:=TImplementationSection(FreeChild(ImplementationSection,Prepare));
  InitializationSection:=TInitializationSection(FreeChild(InitializationSection,Prepare));
  FinalizationSection:=TFinalizationSection(FreeChild(FinalizationSection,Prepare));
  inherited FreeChildren(Prepare);
end;

constructor TPasPackage.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  if (Length(AName) > 0) and (AName[1] <> '#') then
    inherited Create('#' + AName, AParent)
  else
    inherited Create(AName, AParent);
  Modules := TFPList.Create;
end;

destructor TPasPackage.Destroy;
begin
  FreeAndNil(Modules);
  inherited Destroy;
end;

procedure TPasPackage.FreeChildren(Prepare: boolean);
begin
  FreeChildList(Modules,Prepare);
  inherited FreeChildren(Prepare);
end;

{ TPasRequiredPackage }

function TPasRequiredPackage.ElementTypeName: TPasTreeString;
begin
  Result:=SPasTreeRequiredPackage;
end;

{ TPasDynamicPackage }

procedure TPasDynamicPackage.FreeChildren(Prepare: boolean);
begin
  PackageSection:=TPasPackageSection(FreeChild(PackageSection,Prepare));
  inherited FreeChildren(Prepare);
end;

constructor TPasDynamicPackage.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  PackageSection:=TPasPackageSection.Create(aName,Self);
end;

destructor TPasDynamicPackage.Destroy;
begin
  FreeAndNil(PackageSection);
  inherited Destroy;
end;

procedure TPasPointerType.FreeChildren(Prepare: boolean);
begin
  DestType:=TPasType(FreeChild(DestType,Prepare));
  inherited FreeChildren(Prepare);
end;

function TPasAliasType.FixTypeDecl(aDecl: TPasTreeString): TPasTreeString;
begin
  Result:=aDecl;
  if (Name<>'') then
    Result:=SafeName+' = '+Result;
  ProcessHints(false,Result);
end;

procedure TPasAliasType.FreeChildren(Prepare: boolean);
begin
  SubType:=TPasType(FreeChild(SubType,Prepare));
  DestType:=TPasType(FreeChild(DestType,Prepare));
  Expr:=TPasExpr(FreeChild(Expr,Prepare));
  CodepageExpr:=TPasExpr(FreeChild(CodepageExpr,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasArrayType.FreeChildren(Prepare: boolean);
begin
  FreePasExprArray(Self,Ranges,Prepare);
  ElType:=TPasTypeRef(FreeChild(ElType,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasArrayType.ClearTypeReferences(aType: TPasElement);
begin
  inherited ClearTypeReferences(aType);
  if ElType=aType then
    ElType:=nil;
end;

procedure TPasFileType.FreeChildren(Prepare: boolean);
begin
  ElType:=TPasType(FreeChild(ElType,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasFileType.ClearTypeReferences(aType: TPasElement);
begin
  if aType=ElType then
    ElType:=nil;
end;

constructor TPasEnumType.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Values := TFPList.Create;
end;

destructor TPasEnumType.Destroy;
begin
  FreeAndNil(Values);
  inherited Destroy;
end;

procedure TPasEnumType.FreeChildren(Prepare: boolean);
begin
  FreeChildList(Values,Prepare);
  inherited FreeChildren(Prepare);
end;

procedure TPasEnumType.GetEnumNames(Names: TStrings);
var
  i: Integer;
begin
  with Values do
  begin
    for i := 0 to Count - 2 do
      Names.Add(TPasEnumValue(Items[i]).Name + ',');
    if Count > 0 then
      Names.Add(TPasEnumValue(Items[Count - 1]).Name);
  end;
end;

procedure TPasEnumType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Values.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasEnumValue(Values[i]),false);
end;


constructor TPasVariant.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Values := TFPList.Create;
end;

destructor TPasVariant.Destroy;
begin
  FreeAndNil(Values);
  inherited Destroy;
end;

procedure TPasVariant.FreeChildren(Prepare: boolean);
begin
  FreeChildList(Values,Prepare);
  Members:=TPasRecordType(FreeChild(Members,Prepare));
  inherited FreeChildren(Prepare);
end;

function TPasVariant.GetDeclaration(full: boolean): TPasTreeString;

Var
  i : Integer;
  S : TStrings;

begin
  Result:='';
  For I:=0 to Values.Count-1 do
    begin
    if (Result<>'') then
      Result:=Result+', ';
    Result:=Result+TPasElement(Values[i]).GetDeclaration(False);
    Result:=Result+': ('+sLineBreak;
    S:=TStringList.Create;
    try
      Members.GetMembers(S,True);
      Result:=Result+S.Text;
    finally
      S.Free;
    end;
    Result:=Result+');';
    if Full then ;
    end;
end;

procedure TPasVariant.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Values.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Values[i]),false);
  ForEachChildCall(aMethodCall,Arg,Members,false);
end;

{ TPasRecordType }

constructor TPasRecordType.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
end;

destructor TPasRecordType.Destroy;
begin
  FreeAndNil(Variants);
  inherited Destroy;
end;

procedure TPasRecordType.FreeChildren(Prepare: boolean);
begin
  VariantEl:=FreeChild(VariantEl,Prepare);
  FreeChildList(Variants,Prepare);
  inherited FreeChildren(Prepare);
end;

procedure TPasRecordType.ClearTypeReferences(aType: TPasElement);
begin
  inherited ClearTypeReferences(aType);
  if VariantEl=aType then
    VariantEl:=nil;
end;

{ TPasClassType }

constructor TPasClassType.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  IsShortDefinition := False;
  Modifiers := TStringList.Create;
  Interfaces:= TFPList.Create;
end;

destructor TPasClassType.Destroy;
begin
  FreeAndNil(Interfaces);
  FreeAndNil(Modifiers);
  inherited Destroy;
end;

procedure TPasClassType.FreeChildren(Prepare: boolean);
begin
  AncestorType:=TPasType(FreeChild(AncestorType,Prepare));
  HelperForType:=TPasType(FreeChild(HelperForType,Prepare));
  GUIDExpr:=TPasExpr(FreeChild(GUIDExpr,Prepare));
  FreeChildList(Interfaces,Prepare);
  inherited FreeChildren(Prepare);
end;

procedure TPasClassType.ClearTypeReferences(aType: TPasElement);
var
  i: Integer;
  El: TPasElement;
begin
  inherited ClearTypeReferences(aType);
  if AncestorType=aType then
    AncestorType:=nil;
  if HelperForType=aType then
    HelperForType:=nil;
  for i := Interfaces.Count - 1 downto 0 do
    begin
    El:=TPasElement(Interfaces[i]);
    if El=aType then
      Interfaces[i]:=nil;
    end;
end;

procedure TPasClassType.GetMembers(S: TStrings);

Var
  T : TStringList;
  temp : TPasTreeString;
  I,J : integer;
  E : TPasElement;
  CV : TPasMemberVisibility ;

begin
  T:=TStringList.Create;
  try
  CV:=visDefault;
  For I:=0 to Members.Count-1 do
    begin
    E:=TPasElement(Members[i]);
    if E.Visibility<>CV then
      begin
      CV:=E.Visibility;
      if CV<>visDefault then
        S.Add(VisibilityNames[CV]);
      end;
    Temp:=E.GetDeclaration(True);
    If E is TPasProperty then
      Temp:='property '+Temp;
    If Pos(LineEnding,Temp)>0 then
      begin
      T.Text:=Temp;
      For J:=0 to T.Count-1 do
        if J=T.Count-1 then
          S.Add('  '+T[J]+';')
        else
          S.Add('  '+T[J])
      end
    else
      S.Add('  '+Temp+';');
    end;
  finally
    T.Free;
  end;
end;


function TPasClassType.GetDeclaration(full: boolean): TPasTreeString;

Var
  S : TStringList;
  temp : TPasTreeString;

begin
  S:=TStringList.Create;
  Try
    Temp:='class';
    if IsAbstract then
      Temp:=Temp+' abstract';
    if IsSealed then
      Temp:=Temp+' sealed';
    if Assigned(AncestorType) then
      Temp:=Temp+'('+AncestorType.Name+')';
    If Full and (Name<>'') then
      begin
      if GenericTemplateTypes.Count>0 then
        Temp:=SafeName+GenericTemplateTypesAsString(GenericTemplateTypes)+' = '+Temp
      else
        Temp:=SafeName+' = '+Temp;
      end;
    S.Add(Temp);
    GetMembers(S);
    S.Add('end');
    Result:=S.Text;
    if Full then
      ProcessHints(False, Result);
  finally
    S.free;
  end;
end;

function TPasClassType.ElementTypeName: TPasTreeString;
begin
  case ObjKind of
    okObject: Result := SPasTreeObjectType;
    okClass: Result := SPasTreeClassType;
    okInterface: Result := SPasTreeInterfaceType;
    okClassHelper : Result:=SPasClassHelperType;
    okRecordHelper : Result:=SPasRecordHelperType;
    okTypeHelper : Result:=SPasTypeHelperType;
  else
    Result:='ObjKind('+IntToStr(ord(ObjKind))+')';
  end;
end;

procedure TPasClassType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);

  ForEachChildCall(aMethodCall,Arg,AncestorType,true);
  for i:=0 to Interfaces.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Interfaces[i]),true);
  ForEachChildCall(aMethodCall,Arg,HelperForType,true);
  ForEachChildCall(aMethodCall,Arg,GUIDExpr,false);
end;

function TPasClassType.IsObjCClass: Boolean;

begin
  Result:=ObjKind in okObjCClasses;
end;

function TPasClassType.FindMember(MemberClass: TPTreeElement; const MemberName: TPasTreeString): TPasElement;

Var
  I : Integer;

begin
//  Writeln('Looking for ',MemberName,'(',MemberClass.ClassName,') in ',Name);
  Result:=Nil;
  I:=0;
  While (Result=Nil) and (I<Members.Count) do
    begin
    Result:=TPasElement(Members[i]);
    if (Result.ClassType<>MemberClass) or (CompareText(Result.Name,MemberName)<>0) then
      Result:=Nil;
    Inc(I);
    end;
end;

function TPasClassType.FindMemberInAncestors(MemberClass: TPTreeElement;
  const MemberName: TPasTreeString): TPasElement;

  Function A (C : TPasClassType) : TPasClassType;

  begin
    if C.AncestorType is TPasClassType then
      result:=TPasClassType(C.AncestorType)
    else
      result:=Nil;
  end;

Var
  C : TPasClassType;

begin
  Result:=Nil;
  C:=A(Self);
  While (Result=Nil) and (C<>Nil) do
    begin
    Result:=C.FindMember(MemberClass,MemberName);
    C:=A(C);
    end;
end;

function TPasClassType.InterfaceGUID: TPasTreeString;
begin
  If Assigned(GUIDExpr) then
    Result:=GUIDExpr.GetDeclaration(True)
  else
    Result:=''
end;

function TPasClassType.IsSealed: Boolean;
begin
  Result:=HasModifier('sealed');
end;

function TPasClassType.IsAbstract: Boolean;
begin
  Result:=HasModifier('abstract');
end;

function TPasClassType.HasModifier(const aModifier: TPasTreeString): Boolean;
var
  i: Integer;
begin
  for i:=0 to Modifiers.Count-1 do
    if CompareText(aModifier,Modifiers[i])=0 then
      exit(true);
  Result:=false;
end;

{ TPasArgument }

procedure TPasArgument.FreeChildren(Prepare: boolean);
begin
  Attributes:=TPasAttributes(FreeChild(Attributes,Prepare));
  ArgType:=TPasType(FreeChild(ArgType,Prepare));
  ValueExpr:=TPasExpr(FreeChild(ValueExpr,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasArgument.ClearTypeReferences(aType: TPasElement);
begin
  if ArgType=aType then
    ArgType:=nil;
end;

function TPasArgument.GetDeclaration (full : boolean) : TPasTreeString;
begin
  If Assigned(ArgType) then
    begin
    If ArgType.Name<>'' then
      Result:=ArgType.SafeName
    else
      Result:=ArgType.GetDeclaration(False);
    If Full and (Name<>'') then
      begin
      Result:=SafeName+': '+Result;
      if Value<>'' then
        Result:=Result+'='+Value;
      end;
    end
  else If Full then
    Result:=SafeName
  else
    Result:='';
  If Full and Assigned(Attributes) and (Attributes.Parent=Self) then
    Result:=Attributes.GetDeclaration(full)+' '+Result;
end;

procedure TPasArgument.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  ForEachChildCall(aMethodCall,Arg,Attributes,true);
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,ArgType,true);
  ForEachChildCall(aMethodCall,Arg,ValueExpr,true);
end;

function TPasArgument.Value: TPasTreeString;
begin
  If Assigned(ValueExpr) then
    Result:=ValueExpr.GetDeclaration(true)
  else
    Result:='';
end;

{ TPasProcedureType }

// inline
function TPasProcedureType.GetIsAsync: Boolean;
begin
  Result:=ptmAsync in Modifiers;
end;

// inline
function TPasProcedureType.GetIsNested: Boolean;
begin
  Result:=ptmIsNested in Modifiers;
end;

// inline
function TPasProcedureType.GetIsOfObject: Boolean;
begin
  Result:=ptmOfObject in Modifiers;
end;

// inline
function TPasProcedureType.GetIsReference: Boolean;
begin
  Result:=ptmReferenceTo in Modifiers;
end;

procedure TPasProcedureType.SetIsAsync(const AValue: Boolean);
begin
  if AValue then
    Include(Modifiers,ptmAsync)
  else
    Exclude(Modifiers,ptmAsync);
end;

procedure TPasProcedureType.SetIsNested(const AValue: Boolean);
begin
  if AValue then
    Include(Modifiers,ptmIsNested)
  else
    Exclude(Modifiers,ptmIsNested);
end;

procedure TPasProcedureType.SetIsOfObject(const AValue: Boolean);
begin
  if AValue then
    Include(Modifiers,ptmOfObject)
  else
    Exclude(Modifiers,ptmOfObject);
end;

procedure TPasProcedureType.SetIsReference(AValue: Boolean);
begin
  if AValue then
    Include(Modifiers,ptmReferenceTo)
  else
    Exclude(Modifiers,ptmReferenceTo);
end;

constructor TPasProcedureType.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Args := TFPList.Create;
end;

destructor TPasProcedureType.Destroy;
begin
  FreeAndNil(Args);
  inherited Destroy;
end;

procedure TPasProcedureType.FreeChildren(Prepare: boolean);
begin
  FreeChildList(Args,Prepare);
  VarArgsType:=TPasType(FreeChild(VarArgsType,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasProcedureType.ClearTypeReferences(aType: TPasElement);
begin
  inherited ClearTypeReferences(aType);
  if VarArgsType=aType then
    VarArgsType:=nil;
end;

class function TPasProcedureType.TypeName: TPasTreeString;
begin
  Result := 'procedure';
end;

function TPasProcedureType.CreateArgument(const AName,
  AUnresolvedTypeName: TPasTreeString): TPasArgument;
begin
  Result := TPasArgument.Create(AName, Self);
  Args.Add(Result);
  if AUnresolvedTypeName<>'' then
    Result.ArgType := TPasUnresolvedTypeRef.Create(AUnresolvedTypeName, Result);
end;

procedure TPasProcedureType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Args.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Args[i]),false);
  ForEachChildCall(aMethodCall,Arg,VarArgsType,false);
end;

{ TPasResultElement }

procedure TPasResultElement.FreeChildren(Prepare: boolean);
begin
  ResultType:=TPasType(FreeChild(ResultType,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasFunctionType.FreeChildren(Prepare: boolean);
begin
  ResultEl:=TPasResultElement(FreeChild(ResultEl,Prepare));
  inherited FreeChildren(Prepare);
end;


class function TPasFunctionType.TypeName: TPasTreeString;
begin
  Result := 'function';
end;

constructor TPasUnresolvedTypeRef.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, nil);
  if AParent=nil then ;
end;

function TPasUnresolvedTypeRef.GetDeclaration(full: Boolean): TPasTreeString;
begin
  Result:=Name;
  if Full then
    Result:=FixTypeDecl(Result);
end;


procedure TPasVariable.FreeChildren(Prepare: boolean);
begin
  VarType:=TPasType(FreeChild(VarType,Prepare));
  LibraryName:=TPasExpr(FreeChild(LibraryName,Prepare));
  ExportName:=TPasExpr(FreeChild(ExportName,Prepare));
  AbsoluteExpr:=TPasExpr(FreeChild(AbsoluteExpr,Prepare));
  Expr:=TPasExpr(FreeChild(Expr,Prepare));
  inherited FreeChildren(Prepare);
end;

function TPasProperty.GetIsClass: boolean;
begin
  Result:=vmClass in VarModifiers;
end;

procedure TPasProperty.SetIsClass(AValue: boolean);
begin
   if AValue then
    Include(VarModifiers,vmClass)
  else
    Exclude(VarModifiers,vmClass);
end;

constructor TPasProperty.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  FArgs := TFPList.Create;
end;

destructor TPasProperty.Destroy;
begin
  FreeAndNil(FArgs);
  SetLength(Implements,0);
  inherited Destroy;
end;

procedure TPasProperty.FreeChildren(Prepare: boolean);
begin
  IndexExpr:=TPasExpr(FreeChild(IndexExpr,Prepare));
  ReadAccessor:=TPasExpr(FreeChild(ReadAccessor,Prepare));
  WriteAccessor:=TPasExpr(FreeChild(WriteAccessor,Prepare));
  DispIDExpr:=TPasExpr(FreeChild(DispIDExpr,Prepare));
  FreePasExprArray(Self,Implements,Prepare);
  StoredAccessor:=TPasExpr(FreeChild(StoredAccessor,Prepare));
  DefaultExpr:=TPasExpr(FreeChild(DefaultExpr,Prepare));
  inherited FreeChildren(Prepare);
end;

constructor TPasOverloadedProc.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Overloads := TFPList.Create;
end;

destructor TPasOverloadedProc.Destroy;
begin
  FreeAndNil(Overloads);
  inherited Destroy;
end;

procedure TPasOverloadedProc.FreeChildren(Prepare: boolean);
begin
  FreeChildList(Overloads,Prepare);
  inherited FreeChildren(Prepare);
end;

function TPasOverloadedProc.TypeName: TPasTreeString;
begin
  if Assigned(TPasProcedure(Overloads[0]).ProcType) then
    Result := TPasProcedure(Overloads[0]).ProcType.TypeName
  else
    SetLength(Result, 0);
end;

procedure TPasOverloadedProc.ForEachCall(
  const aMethodCall: TOnForEachPasElement; const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Overloads.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasProcedure(Overloads[i]),false);
end;

function TPasProcedure.GetCallingConvention: TCallingConvention;
begin
  Result:=ccDefault;
  if Assigned(ProcType) then
    Result:=ProcType.CallingConvention;
end;

procedure TPasProcedure.SetCallingConvention(AValue: TCallingConvention);
begin
  if Assigned(ProcType) then
    ProcType.CallingConvention:=AValue;
end;

destructor TPasProcedure.Destroy;
begin
  FreeProcNameParts(NameParts);
  inherited Destroy;
end;

procedure TPasProcedure.FreeChildren(Prepare: boolean);
begin
  PublicName:=TPasExpr(FreeChild(PublicName,Prepare));
  LibrarySymbolIndex:=TPasExpr(FreeChild(LibrarySymbolIndex,Prepare));
  LibrarySymbolName:=TPasExpr(FreeChild(LibrarySymbolName,Prepare));
  LibraryExpr:=TPasExpr(FreeChild(LibraryExpr,Prepare));
  DispIDExpr:=TPasExpr(FreeChild(DispIDExpr,Prepare));
  MessageExpr:=TPasExpr(FreeChild(MessageExpr,Prepare));
  ProcType:=TPasProcedureType(FreeChild(ProcType,Prepare));
  Body:=TProcedureBody(FreeChild(Body,Prepare));
  //FreeProcNameParts(Self,NameParts,Prepare);
  inherited FreeChildren(Prepare);
end;

function TPasProcedure.TypeName: TPasTreeString;
begin
  Result := 'procedure';
end;

constructor TPasProcedureImpl.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Locals := TFPList.Create;
end;

destructor TPasProcedureImpl.Destroy;
begin
  FreeAndNil(Locals);
  inherited Destroy;
end;

procedure TPasProcedureImpl.FreeChildren(Prepare: boolean);
begin
  ProcType:=TPasProcedureType(FreeChild(ProcType,Prepare));
  FreeChildList(Locals,Prepare);
  Body:=TPasImplBlock(FreeChild(Body,Prepare));
  inherited FreeChildren(Prepare);
end;

function TPasProcedureImpl.TypeName: TPasTreeString;
begin
  Result := ProcType.TypeName;
end;


function TPasConstructorImpl.TypeName: TPasTreeString;
begin
  Result := 'constructor';
end;

function TPasDestructorImpl.TypeName: TPasTreeString;
begin
  Result := 'destructor';
end;

constructor TPasImplCommands.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Commands := TStringList.Create;
end;

destructor TPasImplCommands.Destroy;
begin
  FreeAndNil(Commands);
  inherited Destroy;
end;

procedure TPasImplIfElse.FreeChildren(Prepare: boolean);
begin
  ConditionExpr:=TPasExpr(FreeChild(ConditionExpr,Prepare));
  IfBranch:=TPasImplElement(FreeChild(IfBranch,Prepare));
  ElseBranch:=TPasImplElement(FreeChild(ElseBranch,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasImplIfElse.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if IfBranch=nil then
    begin
    IfBranch:=Element;
    end
  else if ElseBranch=nil then
    begin
    ElseBranch:=Element;
    end
  else
    raise EPasTree.Create('TPasImplIfElse.AddElement if and else already set - please report this bug');
end;

function TPasImplIfElse.CloseOnSemicolon: boolean;
begin
  Result:=ElseBranch<>nil;
end;

procedure TPasImplIfElse.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  ForEachChildCall(aMethodCall,Arg,ConditionExpr,false);
  if Elements.IndexOf(IfBranch)<0 then
    ForEachChildCall(aMethodCall,Arg,IfBranch,false);
  if Elements.IndexOf(ElseBranch)<0 then
    ForEachChildCall(aMethodCall,Arg,ElseBranch,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

function TPasImplIfElse.Condition: TPasTreeString;
begin
  If Assigned(ConditionExpr) then
    Result:=ConditionExpr.GetDeclaration(True)
  else
    Result:='';
end;

procedure TPasImplForLoop.FreeChildren(Prepare: boolean);
begin
  VariableName:=TPasExpr(FreeChild(VariableName,Prepare));
  StartExpr:=TPasExpr(FreeChild(StartExpr,Prepare));
  EndExpr:=TPasExpr(FreeChild(EndExpr,Prepare));
  Variable:=TPasVariable(FreeChild(Variable,Prepare));
  VarType:=TPasType(FreeChild(VarType,Prepare));
  Body:=TPasImplElement(FreeChild(Body,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasImplForLoop.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if Body=nil then
    begin
    Body:=Element;
    end
  else
    raise EPasTree.Create('TPasImplForLoop.AddElement body already set - please report this bug');
end;

procedure TPasImplForLoop.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  ForEachChildCall(aMethodCall,Arg,VariableName,false);
  ForEachChildCall(aMethodCall,Arg,Variable,false);
  ForEachChildCall(aMethodCall,Arg,StartExpr,false);
  ForEachChildCall(aMethodCall,Arg,EndExpr,false);
  if Elements.IndexOf(Body)<0 then
    ForEachChildCall(aMethodCall,Arg,Body,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

function TPasImplForLoop.Down: boolean;
begin
  Result:=(LoopType=ltDown);
end;

function TPasImplForLoop.StartValue: TPasTreeString;
begin
  If Assigned(StartExpr) then
    Result:=StartExpr.GetDeclaration(true)
  else
    Result:='';
end;

function TPasImplForLoop.EndValue: TPasTreeString;
begin
  If Assigned(EndExpr) then
    Result:=EndExpr.GetDeclaration(true)
  else
    Result:='';
end;

constructor TPasImplBlock.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Elements := TFPList.Create;
end;

destructor TPasImplBlock.Destroy;
begin
  FreeAndNil(Elements);
  inherited Destroy;
end;

procedure TPasImplBlock.FreeChildren(Prepare: boolean);
begin
  FreeChildList(Elements,Prepare);
  inherited FreeChildren(Prepare);
end;

procedure TPasImplBlock.AddElement(Element: TPasImplElement);
begin
  Elements.Add(Element);
end;

function TPasImplBlock.AddCommand(const ACommand: TPasTreeString): TPasImplCommand;
begin
  Result := TPasImplCommand.Create('', Self);
  Result.Command := ACommand;
  AddElement(Result);
end;

function TPasImplBlock.AddCommands: TPasImplCommands;
begin
  Result := TPasImplCommands.Create('', Self);
  AddElement(Result);
end;

function TPasImplBlock.AddBeginBlock: TPasImplBeginBlock;
begin
  Result := TPasImplBeginBlock.Create('', Self);
  AddElement(Result);
end;

function TPasImplBlock.AddRepeatUntil: TPasImplRepeatUntil;
begin
  Result := TPasImplRepeatUntil.Create('', Self);
  AddElement(Result);
end;

function TPasImplBlock.AddIfElse(const ACondition: TPasExpr): TPasImplIfElse;
begin
  Result := TPasImplIfElse.Create('', Self);
  Result.ConditionExpr := ACondition;
  ACondition.Parent:=Result;
  AddElement(Result);
end;

function TPasImplBlock.AddWhileDo(const ACondition: TPasExpr): TPasImplWhileDo;
begin
  Result := TPasImplWhileDo.Create('', Self);
  Result.ConditionExpr := ACondition;
  ACondition.Parent:=Result;
  AddElement(Result);
end;

function TPasImplBlock.AddWithDo(const Expression: TPasExpr): TPasImplWithDo;
begin
  Result := TPasImplWithDo.Create('', Self);
  Result.AddExpression(Expression);
  AddElement(Result);
end;

function TPasImplBlock.AddCaseOf(const Expression: TPasExpr): TPasImplCaseOf;
begin
  Result := TPasImplCaseOf.Create('', Self);
  Result.CaseExpr:= Expression;
  Expression.Parent:=Result;
  AddElement(Result);
end;

function TPasImplBlock.AddForLoop(AVar: TPasVariable; const AStartValue,
  AEndValue: TPasExpr): TPasImplForLoop;
begin
  Result := TPasImplForLoop.Create('', Self);
  Result.Variable := AVar;
  Result.StartExpr := AStartValue;
  AStartValue.Parent := Result;
  Result.EndExpr := AEndValue;
  AEndValue.Parent := Result;
  AddElement(Result);
end;

function TPasImplBlock.AddForLoop(AVarName: TPasExpr; AStartValue,
  AEndValue: TPasExpr; ADownTo: Boolean): TPasImplForLoop;
begin
  Result := TPasImplForLoop.Create('', Self);
  Result.VariableName := AVarName;
  Result.StartExpr := AStartValue;
  AStartValue.Parent := Result;
  Result.EndExpr := AEndValue;
  AEndValue.Parent := Result;
  if ADownto then
    Result.Looptype := ltDown;
  AddElement(Result);
end;

function TPasImplBlock.AddTry: TPasImplTry;
begin
  Result := TPasImplTry.Create('', Self);
  AddElement(Result);
end;

function TPasImplBlock.AddExceptOn(const VarName, TypeName: TPasTreeString
  ): TPasImplExceptOn;
begin
  Result:=AddExceptOn(VarName,TPasUnresolvedTypeRef.Create(TypeName,nil));
end;

function TPasImplBlock.AddExceptOn(const VarName: TPasTreeString; VarType: TPasType
  ): TPasImplExceptOn;
var
  V: TPasVariable;
begin
  V:=TPasVariable.Create(VarName,nil);
  V.VarType:=VarType;
  if VarType.Parent=nil then
    VarType.Parent:=V;
  Result:=AddExceptOn(V);
end;

function TPasImplBlock.AddExceptOn(const VarEl: TPasVariable): TPasImplExceptOn;
begin
  Result:=TPasImplExceptOn.Create('',Self);
  Result.VarEl:=VarEl;
  VarEl.Parent:=Result;
  Result.TypeEl:=VarEl.VarType;
  AddElement(Result);
end;

function TPasImplBlock.AddExceptOn(const TypeEl: TPasType): TPasImplExceptOn;
begin
  Result:=TPasImplExceptOn.Create('',Self);
  Result.TypeEl:=TypeEl;
  if TypeEl.Parent=nil then
    TypeEl.Parent:=Result;
  AddElement(Result);
end;

function TPasImplBlock.AddRaise: TPasImplRaise;
begin
  Result:=TPasImplRaise.Create('',Self);
  AddElement(Result);
end;

function TPasImplBlock.AddLabelMark(const Id: TPasTreeString): TPasImplLabelMark;
begin
  Result:=TPasImplLabelMark.Create('', Self);
  Result.LabelId:=Id;
  AddElement(Result);
end;

function TPasImplBlock.AddAssign(Left,Right:TPasExpr):TPasImplAssign;
begin
  Result:=TPasImplAssign.Create('', Self);
  Result.Left:=Left;
  Left.Parent:=Result;
  Result.Right:=Right;
  Right.Parent:=Result;
  AddElement(Result);
end;

function TPasImplBlock.AddSimple(Expr:TPasExpr):TPasImplSimple;
begin
  Result:=TPasImplSimple.Create('', Self);
  Result.Expr:=Expr;
  Expr.Parent:=Result;
  AddElement(Result);
end;

function TPasImplBlock.CloseOnSemicolon: boolean;
begin
  Result:=false;
end;

procedure TPasImplBlock.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Elements.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Elements[i]),false);
end;



{ ---------------------------------------------------------------------

  ---------------------------------------------------------------------}

function TPasModule.GetDeclaration(full : boolean): TPasTreeString;
begin
  Result := 'Unit ' + SafeName;
  if full then ;
end;

procedure TPasModule.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,InterfaceSection,false);
  ForEachChildCall(aMethodCall,Arg,ImplementationSection,false);
  ForEachChildCall(aMethodCall,Arg,InitializationSection,false);
  ForEachChildCall(aMethodCall,Arg,FinalizationSection,false);
end;

function TPasResString.GetDeclaration(full: Boolean): TPasTreeString;
begin
  Result:=Expr.GetDeclaration(true);
  If Full Then
    begin
    Result:=SafeName+' = '+Result;
    ProcessHints(False,Result);
    end;
end;

procedure TPasResString.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Expr,false);
end;

procedure TPasResString.FreeChildren(Prepare: boolean);
begin
  Expr:=TPasExpr(FreeChild(Expr,Prepare));
  inherited FreeChildren(Prepare);
end;

function TPasPointerType.GetDeclaration(full: Boolean): TPasTreeString;
begin
  Result:='^'+DestType.SafeName;
  If Full then
    begin
    Result:=SafeName+' = '+Result;
    ProcessHints(False,Result);
    end;
end;

procedure TPasPointerType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,DestType,true);
end;

procedure TPasPointerType.ClearTypeReferences(aType: TPasElement);
begin
  if DestType=aType then
    DestType:=nil;
end;

function TPasAliasType.GetDeclaration(full: Boolean): TPasTreeString;
begin
  if DestType is TPasStringType then
    Result:=DestType.GetDeclaration(True)
  else
    Result:=DestType.SafeName;
  If Full then
    Result:=FixTypeDecl(Result);
end;

procedure TPasAliasType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,DestType,true);
  ForEachChildCall(aMethodCall,Arg,Expr,false);
end;

procedure TPasAliasType.ClearTypeReferences(aType: TPasElement);
begin
  if DestType=aType then
    DestType:=nil;
end;

function TPasClassOfType.GetDeclaration (full : boolean) : TPasTreeString;
begin
  Result:='class of '+DestType.SafeName;
  If Full then
    Result:=FixTypeDecl(Result);
end;

function TPasRangeType.GetDeclaration (full : boolean) : TPasTreeString;
begin
  Result:=RangeStart+'..'+RangeEnd;
  If Full then
    Result:=FixTypeDecl(Result);
end;

procedure TPasRangeType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,RangeExpr,false);
end;

procedure TPasRangeType.FreeChildren(Prepare: boolean);
begin
  RangeExpr:=TBinaryExpr(FreeChild(RangeExpr,Prepare));
  inherited FreeChildren(Prepare);
end;

function TPasRangeType.RangeStart: TPasTreeString;
begin
  Result:=RangeExpr.Left.GetDeclaration(False);
end;

function TPasRangeType.RangeEnd: TPasTreeString;
begin
  Result:=RangeExpr.Right.GetDeclaration(False);
end;

function TPasArrayType.GetDeclaration (full : boolean) : TPasTreeString;
var
  i : Integer;
begin
  Result:='Array';
  if Full then
    begin
    if GenericTemplateTypes<>nil then
      Result:=SafeName+GenericTemplateTypesAsString(GenericTemplateTypes)+' = '+Result
    else
      Result:=SafeName+' = '+Result;
    end;
  If (IndexRange<>'') then
    Result:=Result+'['+IndexRange+']'
  else if Length(Ranges)>0 then
    begin
      Result:=Result+'[';
      for i:=0 to Length(Ranges)-1 do
        begin
          if i>0 then
            Result:=Result+',';
          Result:=Result+Ranges[i].GetDeclaration(True);
        end;
      Result:=Result+']';
    end;
  Result:=Result+' of ';
  If IsPacked then
    Result := 'packed '+Result;      // 12/04/04 Dave - Added
  If Assigned(Eltype) then
    Result:=Result+ElType.GetDeclaration(Not (ElType is TPasUnresolvedTypeRef))
  else
    Result:=Result+'const';
end;

function TPasArrayType.IsGenericArray: Boolean;
begin
  Result:=GenericTemplateTypes<>nil;
end;

function TPasArrayType.IsPacked: Boolean;
begin
  Result:=PackMode=pmPacked;
end;

procedure TPasArrayType.AddRange(Range: TPasExpr);
var
  i: Integer;
begin
  i:=Length(Ranges);
  SetLength(Ranges, i+1);
  Ranges[i]:=Range;
end;

function TPasFileType.GetDeclaration (full : boolean) : TPasTreeString;
begin
  Result:='File';
  If Assigned(Eltype) then
    Result:=Result+' of '+ElType.SafeName;
  If Full Then
    Result:=FixTypeDecl(Result);
end;

procedure TPasFileType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,ElType,true);
end;

function TPasEnumType.GetDeclaration (full : boolean) : TPasTreeString;

Var
  S : TStringList;

begin
  S:=TStringList.Create;
  Try
    If Full and (Name<>'') then
      S.Add(SafeName+' = (')
    else
      S.Add('(');
    GetEnumNames(S);
    S[S.Count-1]:=S[S.Count-1]+')';
    If Full then
      Result:=IndentStrings(S,Length(SafeName)+4)
    else
      Result:=IndentStrings(S,1);
    if Full then
      ProcessHints(False,Result);
  finally
    S.Free;
  end;
end;

procedure TPasSetType.FreeChildren(Prepare: boolean);
begin
  EnumType:=TPasTypeRef(FreeChild(EnumType,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasSetType.ClearTypeReferences(aType: TPasElement);
begin
  if EnumType=aType then
    EnumType:=nil;
end;

function TPasSetType.GetDeclaration (full : boolean) : TPasTreeString;

Var
  S : TStringList;
  i : Integer;

begin
  If (EnumType is TPasEnumType) and (EnumType.Name='') then
    begin
    S:=TStringList.Create;
    Try
      If Full and (Name<>'') then
        S.Add(SafeName+'= Set of (')
      else
        S.Add('Set of (');
      TPasEnumType(EnumType).GetEnumNames(S);
      S[S.Count-1]:=S[S.Count-1]+')';
      I:=Pos('(',S[0]);
      Result:=IndentStrings(S,i);
    finally
      S.Free;
    end;
    end
  else
    begin
    Result:='Set of '+EnumType.GetDeclaration(True);
    If Full then
      Result:=SafeName+' = '+Result;
    end;
  If Full then
    ProcessHints(False,Result);
end;

procedure TPasSetType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,EnumType,true);
end;

{ TPasMembersType }

constructor TPasMembersType.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  PackMode:=pmNone;
  Members := TFPList.Create;
  GenericTemplateTypes:=TFPList.Create;
end;

destructor TPasMembersType.Destroy;
begin
  FreeAndNil(GenericTemplateTypes);
  FreeAndNil(Members);
  inherited Destroy;
end;

procedure TPasMembersType.FreeChildren(Prepare: boolean);
begin
  FreeChildList(GenericTemplateTypes,Prepare);
  FreeChildList(Members,Prepare);
  inherited FreeChildren(Prepare);
end;

function TPasMembersType.IsPacked: Boolean;
begin
  Result:=(PackMode <> pmNone);
end;

function TPasMembersType.IsBitPacked: Boolean;
begin
  Result:=(PackMode=pmBitPacked)
end;

procedure TPasMembersType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Members.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Members[i]),false);
end;

function TPasMembersType.HasExtRTTI(El: TPasElement): boolean;
var
  C: TClass;
begin
  if El.Visibility=visPublished then
    exit(true);
  C:=El.ClassType;
  if C=TPasVariable then
    begin
    if VisibilityToExtRTTI[El.Visibility] in RTTIVisibility.Fields then
      exit(true);
    end
  else if C=TPasProperty then
    begin
    if VisibilityToExtRTTI[El.Visibility] in RTTIVisibility.Properties then
      exit(true);
    end
  else if C.InheritsFrom(TPasProcedure) then
    begin
    if VisibilityToExtRTTI[El.Visibility] in RTTIVisibility.Methods then
      exit(true);
    end;
  Result:=false;
end;

{ TPasRecordType }

procedure TPasRecordType.GetMembers(S: TStrings; aSkipSection : Boolean = false);

Var
  T : TStringList;
  temp : TPasTreeString;
  I,J : integer;
  E : TPasElement;
  CV : TPasMemberVisibility ;

begin
  T:=TStringList.Create;
  try

  CV:=visDefault;
  For I:=0 to Members.Count-1 do
    begin
    E:=TPasElement(Members[i]);
    if E.Visibility<>CV then
      begin
      CV:=E.Visibility;
      if (CV<>visDefault) and not aSkipSection then
        S.Add(VisibilityNames[CV]);
      end;
    Temp:=E.GetDeclaration(True);
    If E is TPasProperty then
      Temp:='property '+Temp;
    If Pos(LineEnding,Temp)>0 then
      begin
      T.Text:=Temp;
      For J:=0 to T.Count-1 do
        if J=T.Count-1 then
          S.Add('  '+T[J]+';')
        else
          S.Add('  '+T[J])
      end
    else
      S.Add('  '+Temp+';');
    end;
  if Variants<>nil then
    begin
    temp:='case ';
    if (VariantEl is TPasVariable) then
      temp:=Temp+VariantEl.Name+' : '+TPasVariable(VariantEl).VarType.Name
    else if (VariantEl<>Nil) then
      temp:=temp+VariantEl.Name;
    S.Add(temp+' of');
    T.Clear;
    For I:=0 to Variants.Count-1 do
      T.Add(TPasVariant(Variants[i]).GetDeclaration(True));
    S.AddStrings(T);
    end;
  finally
    T.Free;
  end;
end;

function TPasRecordType.GetDeclaration (full : boolean) : TPasTreeString;

Var
  S : TStringList;
  temp : TPasTreeString;
begin
  S:=TStringList.Create;
  Try
    Temp:='record';
    If IsPacked then
      if IsBitPacked then
        Temp:='bitpacked '+Temp
      else
        Temp:='packed '+Temp;
    If Full and (Name<>'') then
      begin
      if GenericTemplateTypes.Count>0 then
        Temp:=SafeName+GenericTemplateTypesAsString(GenericTemplateTypes)+' = '+Temp
      else
        Temp:=SafeName+' = '+Temp;
      end;
    S.Add(Temp);
    GetMembers(S);
    S.Add('end');
    Result:=S.Text;
    if Full then
      ProcessHints(False, Result);
  finally
    S.free;
  end;
end;

procedure TPasRecordType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,VariantEl,true);
  if Variants<>nil then
    for i:=0 to Variants.Count-1 do
      ForEachChildCall(aMethodCall,Arg,TPasElement(Variants[i]),false);
end;

function TPasRecordType.IsAdvancedRecord: Boolean;

Var
  I : Integer;
  Member: TPasElement;

begin
  Result:=False;
  For I:=0 to Members.Count-1 do
    begin
    Member:=TPasElement(Members[i]);
    if (Member.Visibility<>visPublic) then
      Exit(True);
    if (Member.ClassType<>TPasVariable) then
      Exit(True);
    end;
end;

procedure TPasProcedureType.GetArguments(List : TStrings);

Var
  T : TPasTreeString;
  I : Integer;

begin
  For I:=0 to Args.Count-1 do
    begin
    T:=AccessNames[TPasArgument(Args[i]).Access];
    T:=T+TPasArgument(Args[i]).GetDeclaration(True);
    If I=0 then
      T:='('+T;
    If I<Args.Count-1 then
      List.Add(T+'; ')
    else
      List.Add(T+')');
    end;
end;

function TPasProcedureType.GetDeclaration (full : boolean) : TPasTreeString;

Var
  S : TStringList;

begin
  S:=TStringList.Create;
  Try
    If Full then
      S.Add(Format('%s = ',[SafeName]));
    S.Add(TypeName);
    GetArguments(S);
    If IsOfObject then
      S.Add(' of object')
    else if IsNested then
      S.Add(' is nested');
    If Full then
      Result:=IndentStrings(S,Length(S[0])+Length(S[1])+1)
    else
      Result:=IndentStrings(S,Length(S[0])+1);
  finally
    S.Free;
  end;
end;

function TPasFunctionType.GetDeclaration(Full: boolean): TPasTreeString;

Var
  S : TStringList;
  T : TPasTreeString;

begin
  S:=TStringList.Create;
  Try
    If Full then
      S.Add(Format('%s = ',[SafeName]));
    S.Add(TypeName);
    GetArguments(S);
    If Assigned(ResultEl) then
      begin
      T:=' : ';
      If (ResultEl.ResultType.Name<>'') then
        T:=T+ResultEl.ResultType.SafeName
      else
        T:=T+ResultEl.ResultType.GetDeclaration(False);
      S.Add(T);
      end;
    If IsOfObject then
      S.Add(' of object');
    If Full then
      Result:=IndentStrings(S,Length(S[0])+Length(S[1])+1)
    else
      Result:=IndentStrings(S,Length(S[0])+1);
  finally
    S.Free;
  end;
end;

procedure TPasFunctionType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,ResultEl,false);
end;

function TPasVariable.GetDeclaration (full : boolean) : TPasTreeString;

Const
 Seps : Array[Boolean] of Char = ('=',':');

begin
  If Assigned(VarType) then
    begin
    // Todo: need something better than this.
    If (VarType.Name='')
       or ((VarType is TPasAliasType) and (TPasAliasType(VarType).DestType is TPasStringType)) then
      Result:=VarType.GetDeclaration(False)
    else
      Result:=VarType.SafeName;
    Result:=Result+Modifiers;
    if (Value<>'') then
      Result:=Result+' = '+Value;
    end
  else
    Result:=Value;
  If Full then
    begin
    Result:=SafeName+' '+Seps[Assigned(VarType)]+' '+Result;
    Result:=Result+HintsString;
    end;
end;

procedure TPasVariable.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,VarType,true);
  ForEachChildCall(aMethodCall,Arg,Expr,false);
  ForEachChildCall(aMethodCall,Arg,LibraryName,false);
  ForEachChildCall(aMethodCall,Arg,ExportName,false);
  ForEachChildCall(aMethodCall,Arg,AbsoluteExpr,false);
end;

procedure TPasVariable.ClearTypeReferences(aType: TPasElement);
begin
  if VarType=aType then
    VarType:=nil;
end;


function TPasVariable.Value: TPasTreeString;
begin
  If Assigned(Expr) then
    Result:=Expr.GetDeclaration(True)
  else
    Result:='';
end;

function TPasProperty.GetDeclaration (full : boolean) : TPasTreeString;

begin
  Result:=GetDeclaration(Full,False);
end;

function TPasProperty.GetDeclaration (full : boolean; WithAccessor : Boolean) : TPasTreeString;

Var
  S : TPasTreeString;
  I : Integer;

begin
  Result:='';
  If Assigned(VarType) then
    begin
    If VarType.Name='' then
      Result:=VarType.GetDeclaration(False)
    else
      Result:=VarType.SafeName;
    end
  else if Assigned(Expr) then
    Result:=Expr.GetDeclaration(True);
  S:='';
  If Assigned(Args) and (Args.Count>0) then
    begin
    For I:=0 to Args.Count-1 do
      begin
      If (S<>'') then
        S:=S+';';
      S:=S+TPasElement(Args[i]).GetDeclaration(true);
      end;
    end;
  If S<>'' then
    S:='['+S+']'
  else
    S:=' ';
  If Full then
    begin
    Result:=SafeName+S+': '+Result;
    if WithAccessor then
      begin
      if Assigned(ReadAccessor) then
         Result:=Result+' read '+ReadAccessor.GetDeclaration(True);
      if Assigned(WriteAccessor) then
         Result:=Result+' write '+WriteAccessor.GetDeclaration(True);
      end;
    If (ImplementsName<>'') then
       Result:=Result+' implements '+EscapeKeyWord(ImplementsName);
    end;
  If IsDefault then
    Result:=Result+'; default';
  ProcessHints(True, Result);
end;

procedure TPasProperty.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,IndexExpr,false);
  for i:=0 to Args.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Args[i]),false);
  ForEachChildCall(aMethodCall,Arg,ReadAccessor,false);
  ForEachChildCall(aMethodCall,Arg,WriteAccessor,false);
  for i:=0 to length(Implements)-1 do
    ForEachChildCall(aMethodCall,Arg,Implements[i],false);
  ForEachChildCall(aMethodCall,Arg,StoredAccessor,false);
  ForEachChildCall(aMethodCall,Arg,DefaultExpr,false);
end;

function TPasProperty.ResolvedType: TPasType;

  Function GC(P : TPasProperty) : TPasClassType;

  begin
    if Assigned(P) and Assigned(P.Parent) and (P.Parent is TPasClassType) then
      Result:=P.Parent as TPasClassType
    else
      Result:=Nil;
  end;


Var
  P : TPasProperty;
  C : TPasClassType;

begin
  Result:=FResolvedType;
  if Result=Nil then
    Result:=VarType;
  P:=Self;
  While (Result=Nil) and (P<>Nil) do
    begin
    C:=GC(P);
//    Writeln('Looking for ',Name,' in ancestor ',C.Name);
    P:=TPasProperty(C.FindMemberInAncestors(TPasProperty,Name));
    if Assigned(P) then
      begin
//      Writeln('Found ',Name,' in ancestor : ',P.Name);
      Result:=P.ResolvedType;
      end
    end;
end;

function TPasProperty.IndexValue: TPasTreeString;
begin
  If Assigned(IndexExpr) then
    Result:=IndexExpr.GetDeclaration(true)
  else
    Result:='';
end;

function TPasProperty.DefaultValue: TPasTreeString;
begin
  If Assigned(DefaultExpr) then
    Result:=DefaultExpr.GetDeclaration(true)
  else
    Result:='';
end;

procedure TPasProcedure.GetModifiers(List: TStrings);

  Procedure DoAdd(B : Boolean; S : TPasTreeString);

  begin
    if B then
      List.add('; '+S);
  end;

begin
  Doadd(IsVirtual,' virtual');
  DoAdd(IsDynamic,' dynamic');
  DoAdd(IsOverride,' override');
  DoAdd(IsAbstract,' abstract');
  DoAdd(IsReintroduced,' reintroduce');
  DoAdd(IsOverload,' overload');
  DoAdd(IsStatic,' static');
  DoAdd(IsMessage,' message');
end;

procedure TPasProcedure.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i, j: Integer;
  Templates: TFPList;
begin
  inherited ForEachCall(aMethodCall, Arg);
  if NameParts<>nil then
    for i:=0 to NameParts.Count-1 do
      begin
      Templates:=TProcedureNamePart(NameParts[i]).Templates;
      if Templates<>nil then
        for j:=0 to Templates.Count-1 do
          ForEachChildCall(aMethodCall,Arg,TPasElement(Templates[j]),false);
      end;
  ForEachChildCall(aMethodCall,Arg,ProcType,false);
  ForEachChildCall(aMethodCall,Arg,PublicName,false);
  ForEachChildCall(aMethodCall,Arg,LibraryExpr,false);
  ForEachChildCall(aMethodCall,Arg,LibrarySymbolName,false);
  ForEachChildCall(aMethodCall,Arg,MessageExpr,false);
  ForEachChildCall(aMethodCall,Arg,Body,false);
end;

procedure TPasProcedure.AddModifier(AModifier: TProcedureModifier);
begin
  Include(FModifiers,AModifier);
end;

function TPasProcedure.CanParseImplementation: Boolean;
begin
  Result:=not HasNoImplementation
          and ((Parent is TImplementationSection) or (Parent is TProcedureBody));
end;

function TPasProcedure.HasNoImplementation: Boolean;
begin
  Result:=IsExternal or IsForward or IsInternProc;
end;

function TPasProcedure.IsVirtual: Boolean;
begin
  Result:=pmVirtual in FModifiers;
end;

function TPasProcedure.IsDynamic: Boolean;
begin
  Result:=pmDynamic in FModifiers;
end;

function TPasProcedure.IsAbstract: Boolean;
begin
  Result:=pmAbstract in FModifiers;
end;

function TPasProcedure.IsOverride: Boolean;
begin
  Result:=pmOverride in FModifiers;
end;

function TPasProcedure.IsExported: Boolean;
begin
  Result:=pmExport in FModifiers;
end;

function TPasProcedure.IsExternal: Boolean;
begin
  Result:=pmExternal in FModifiers;
end;

function TPasProcedure.IsOverload: Boolean;
begin
  Result:=pmOverload in FModifiers;
end;

function TPasProcedure.IsMessage: Boolean;
begin
  Result:=pmMessage in FModifiers;
end;

function TPasProcedure.IsReintroduced: Boolean;
begin
  Result:=pmReintroduce in FModifiers;
end;

function TPasProcedure.IsStatic: Boolean;

begin
  Result:=ptmStatic in ProcType.Modifiers;
end;

function TPasProcedure.IsForward: Boolean;
begin
  Result:=pmForward in FModifiers;
end;

function TPasProcedure.IsCompilerProc: Boolean;
begin
  Result:=pmCompilerProc in FModifiers;
end;

function TPasProcedure.IsInternProc: Boolean;
begin
  Result:=pmInternProc in FModifiers;
end;

function TPasProcedure.IsAssembler: Boolean;
begin
  Result:=pmAssembler in FModifiers;
end;

function TPasProcedure.IsAsync: Boolean;
begin
  Result:=ProcType.IsAsync;
end;

function TPasProcedure.GetProcTypeEnum: TProcType;
begin
  Result:=ptProcedure;
end;

procedure TPasProcedure.SetNameParts(Parts: TProcedureNameParts);
var
  i, j: Integer;
  El: TPasElement;
begin
  if NameParts<>nil then
    FreeProcNameParts(NameParts);
  NameParts:=TFPList.Create;
  NameParts.Assign(Parts);
  Parts.Clear;
  for i:=0 to NameParts.Count-1 do
    with TProcedureNamePart(NameParts[i]) do
      if Templates<>nil then
        for j:=0 to Templates.Count-1 do
          begin
          El:=TPasElement(Templates[j]);
          El.Parent:=Self;
          end;
end;

function TPasProcedure.GetDeclaration(full: Boolean): TPasTreeString;

begin
  Result:=GetDeclaration(Full,True,Full,False);
end;

function TPasProcedure.GetDeclaration(full, AddArgs, AddModifiers, AddParent: Boolean): TPasTreeString;

  function GetName(t : string) : String;
  begin
    Result:=T;
    if Name='' then
      exit;
    Result:=Result+' ';
    if addParent and (Parent is TPasType) then
      Result:=Result+Parent.Name+'.';
    Result:=Result+SafeName;
  end;

Var
  S : TStringList;
  T: TPasTreeString;
  i: Integer;
begin
  S:=TStringList.Create;
  try
    T:=TypeName;
    If (NameParts=Nil) or not Full then
      T:=GetName(T)
    else
      begin
      T:=T+' ';
      for i:=0 to NameParts.Count-1 do
        begin
        if i>0 then
          T:=T+'.';
        with TProcedureNamePart(NameParts[i]) do
          begin
          T:=T+Name;
          if Templates<>nil then
            T:=T+GenericTemplateTypesAsString(Templates);
          end;
        end;
      end;
    S.Add(T);
    if Assigned(ProcType) then
      begin
      if AddArgs then
        ProcType.GetArguments(S);
      If (ProcType is TPasFunctionType)
          and Assigned(TPasFunctionType(Proctype).ResultEl) then
        With TPasFunctionType(ProcType).ResultEl.ResultType do
          begin
          T:=' : ';
          If (Name<>'') then
            T:=T+SafeName
          else
            T:=T+GetDeclaration(False);
          S.Add(T);
          end;
      if AddModifiers then
        GetModifiers(S); // needs proctype
      end;
    if s.Count>0 then
      Result:=IndentStrings(S,Length(S[0]));
  finally
    S.Free;
  end;
end;


function TPasFunction.TypeName: TPasTreeString;
begin
  Result:='function';
end;

function TPasFunction.GetProcTypeEnum: TProcType;
begin
  Result:=ptFunction;
end;

function TPasOperator.GetOperatorDeclaration(Full : Boolean) : TPasTreeString;

begin
  if Full then
    begin
    Result:=FullPath;
    if (Result<>'') then
      Result:=Result+'.';
    end
  else
    Result:='';
  if TokenBased then
    Result:=Result+TypeName+' '+OperatorTypeToToken(OperatorType)
  else
    Result:=Result+TypeName+' '+OperatorTypeToOperatorName(OperatorType);
end;

function TPasOperator.GetDeclaration(full, AddArgs, AddModifiers, AddParent: Boolean): TPasTreeString;

Var
  S : TStringList;
  T : TPasTreeString;

begin
  S:=TStringList.Create;
  try
    S.Add(GetOperatorDeclaration(Full));
    ProcType.GetArguments(S);
    If Assigned((Proctype as TPasFunctionType).ResultEl) then
      if Assigned(TPasFunctionType(ProcType).ResultEl.ResultType) then
      With TPasFunctionType(ProcType).ResultEl.ResultType do
        begin
        T:=' : ';
        If (Name<>'') then
          T:=T+SafeName
        else
          T:=T+GetDeclaration(False);
        S.Add(T);
        end;
    GetModifiers(S);
    Result:=IndentStrings(S,Length(S[0]));
  finally
    S.Free;
  end;
end;

function TPasOperator.TypeName: TPasTreeString;
begin
  Result:='operator';
end;

function TPasOperator.GetProcTypeEnum: TProcType;
begin
  Result:=ptOperator;
end;

function TPasClassProcedure.TypeName: TPasTreeString;
begin
  Result:='class procedure';
end;

function TPasClassProcedure.GetProcTypeEnum: TProcType;
begin
  Result:=ptClassProcedure;
end;

function TPasClassFunction.TypeName: TPasTreeString;
begin
  Result:='class function';
end;

function TPasClassFunction.GetProcTypeEnum: TProcType;
begin
  Result:=ptClassFunction;
end;

function TPasConstructor.TypeName: TPasTreeString;
begin
  Result:='constructor';
end;

function TPasConstructor.GetProcTypeEnum: TProcType;
begin
  Result:=ptConstructor;
end;

function TPasDestructor.TypeName: TPasTreeString;
begin
  Result:='destructor';
end;

function TPasDestructor.GetProcTypeEnum: TProcType;
begin
  Result:=ptDestructor;
end;

{ TPassTreeVisitor }

procedure TPassTreeVisitor.Visit(obj: TPasElement);
begin
  // Needs to be implemented by descendents.
  if Obj=nil then ;
end;

{ TPasSection }

constructor TPasSection.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  UsesList := TFPList.Create;
end;

destructor TPasSection.Destroy;
begin
  FreeAndNil(UsesList);
  {$IFDEF VerbosePasTreeMem}writeln('TPasSection.Destroy inherited');{$ENDIF}
  inherited Destroy;
  {$IFDEF VerbosePasTreeMem}writeln('TPasSection.Destroy END');{$ENDIF}
end;

procedure TPasSection.FreeChildren(Prepare: boolean);
var
  i: Integer;
begin
  FreeChildList(UsesList,Prepare);
  for i := 0 to high(UsesClause) do
    UsesClause[i]:=TPasUsesUnit(FreeChild(UsesClause[i],Prepare));
  inherited FreeChildren(Prepare);
end;

function TPasSection.AddUnitToUsesList(const AUnitName: TPasTreeString;
  aName: TPasExpr; InFilename: TPrimitiveExpr; aModule: TPasElement;
  UsesUnit: TPasUsesUnit): TPasUsesUnit;
var
  l: Integer;
begin
  if (InFilename<>nil) and (InFilename.Kind<>pekString) then
    raise EPasTree.Create('Wrong In expression for '+aUnitName);
  if aModule=nil then
    aModule:=TPasUnresolvedUnitRef.Create(AUnitName, Self);
  l:=length(UsesClause);
  SetLength(UsesClause,l+1);
  if UsesUnit=nil then
    begin
    UsesUnit:=TPasUsesUnit.Create(AUnitName,Self);
    if aName<>nil then
      begin
      UsesUnit.SourceFilename:=aName.SourceFilename;
      UsesUnit.SourceLinenumber:=aName.SourceLinenumber;
      end;
    end;
  UsesClause[l]:=UsesUnit;
  UsesUnit.Expr:=aName;
  UsesUnit.InFilename:=InFilename;
  UsesUnit.Module:=aModule;
  Result:=UsesUnit;

  UsesList.Add(aModule);
end;

function TPasSection.ElementTypeName: TPasTreeString;
begin
  Result := SPasTreeSection;
end;

procedure TPasSection.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to length(UsesClause)-1 do
    ForEachChildCall(aMethodCall,Arg,UsesClause[i],false);
end;

{ TProcedureBody }

procedure TProcedureBody.FreeChildren(Prepare: boolean);
begin
  Body:=TPasImplBlock(FreeChild(Body,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TProcedureBody.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Body,false);
end;

{ TPasImplWhileDo }

procedure TPasImplWhileDo.FreeChildren(Prepare: boolean);
begin
  ConditionExpr:=TPasExpr(FreeChild(ConditionExpr,Prepare));
  Body:=TPasImplElement(FreeChild(Body,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasImplWhileDo.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if Body=nil then
    begin
    Body:=Element;
    end
  else
    raise EPasTree.Create('TPasImplWhileDo.AddElement body already set');
end;

procedure TPasImplWhileDo.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  ForEachChildCall(aMethodCall,Arg,ConditionExpr,false);
  if Elements.IndexOf(Body)<0 then
    ForEachChildCall(aMethodCall,Arg,Body,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

function TPasImplWhileDo.Condition: TPasTreeString;
begin
  If Assigned(ConditionExpr) then
    Result:=ConditionExpr.GetDeclaration(True)
  else
    Result:='';
end;

{ TPasImplCaseOf }

procedure TPasImplCaseOf.FreeChildren(Prepare: boolean);
begin
  CaseExpr:=TPasExpr(FreeChild(CaseExpr,Prepare));
  ElseBranch:=TPasImplCaseElse(FreeChild(ElseBranch,Prepare));
  inherited FreeChildren(Prepare);
end;

function TPasImplCaseOf.AddCase(const Expression: TPasExpr
  ): TPasImplCaseStatement;
begin
  Result:=TPasImplCaseStatement.Create('',Self);
  Result.AddExpression(Expression);
  AddElement(Result);
end;

function TPasImplCaseOf.AddElse: TPasImplCaseElse;
begin
  Result:=TPasImplCaseElse.Create('',Self);
  ElseBranch:=Result;
  AddElement(Result);
end;

procedure TPasImplCaseOf.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  ForEachChildCall(aMethodCall,Arg,CaseExpr,false);
  if Elements.IndexOf(ElseBranch)<0 then
    ForEachChildCall(aMethodCall,Arg,ElseBranch,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

function TPasImplCaseOf.Expression: TPasTreeString;
begin
  if Assigned(CaseExpr) then
    Result:=CaseExpr.GetDeclaration(True)
  else
    Result:='';
end;

{ TPasImplCaseStatement }

constructor TPasImplCaseStatement.Create(const AName: TPasTreeString;
  AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Expressions:=TFPList.Create;
end;

destructor TPasImplCaseStatement.Destroy;
begin
  FreeAndNil(Expressions);
  inherited Destroy;
end;

procedure TPasImplCaseStatement.FreeChildren(Prepare: boolean);
begin
  FreeChildList(Expressions,Prepare);
  Body:=TPasImplElement(FreeChild(Body,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasImplCaseStatement.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if Body=nil then
    begin
    Body:=Element;
    end
  else
    raise EPasTree.Create('TPasImplCaseStatement.AddElement body already set');
end;

procedure TPasImplCaseStatement.AddExpression(const Expr: TPasExpr);
begin
  Expressions.Add(Expr);
  Expr.Parent:=Self;
end;

procedure TPasImplCaseStatement.ForEachCall(
  const aMethodCall: TOnForEachPasElement; const Arg: Pointer);
var
  i: Integer;
begin
  for i:=0 to Expressions.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Expressions[i]),false);
  if Elements.IndexOf(Body)<0 then
    ForEachChildCall(aMethodCall,Arg,Body,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

{ TPasImplWithDo }

constructor TPasImplWithDo.Create(const AName: TPasTreeString; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Expressions:=TFPList.Create;
end;

destructor TPasImplWithDo.Destroy;
begin
  FreeAndNil(Expressions);
  inherited Destroy;
end;

procedure TPasImplWithDo.FreeChildren(Prepare: boolean);
begin
  FreeChildList(Expressions,Prepare);
  Body:=TPasImplElement(FreeChild(Body,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasImplWithDo.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if Body=nil then
    begin
    Body:=Element;
    end
  else
    raise EPasTree.Create('TPasImplWithDo.AddElement body already set');
end;

procedure TPasImplWithDo.AddExpression(const Expression: TPasExpr);
begin
  Expressions.Add(Expression);
  if Expression.Parent=nil then
    Expression.Parent:=Self;
end;

procedure TPasImplWithDo.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  for i:=0 to Expressions.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Expressions[i]),false);
  if Elements.IndexOf(Body)<0 then
    ForEachChildCall(aMethodCall,Arg,Body,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

{ TPasInlineVarDeclStatement }

constructor TPasInlineVarDeclStatement.Create(const aName: TPasTreeString; aParent: TPasElement);
begin
  inherited Create(aName,aParent);
  Declarations:=TFPList.Create;
end;

procedure TPasInlineVarDeclStatement.FreeChildren(Prepare: boolean);
begin
  FreeChildList(Declarations,Prepare);
  inherited FreeChildren(Prepare);
end;

destructor TPasInlineVarDeclStatement.Destroy;
begin
  inherited Destroy;
  FreeAndNil(Declarations)
end;

{ TPasImplTry }

procedure TPasImplTry.FreeChildren(Prepare: boolean);
begin
  FinallyExcept:=TPasImplTryHandler(FreeChild(FinallyExcept,Prepare));
  ElseBranch:=TPasImplTryExceptElse(FreeChild(ElseBranch,Prepare));
  inherited FreeChildren(Prepare);
end;

function TPasImplTry.AddFinally: TPasImplTryFinally;
begin
  Result:=TPasImplTryFinally.Create('',Self);
  FinallyExcept:=Result;
end;

function TPasImplTry.AddExcept: TPasImplTryExcept;
begin
  Result:=TPasImplTryExcept.Create('',Self);
  FinallyExcept:=Result;
end;

function TPasImplTry.AddExceptElse: TPasImplTryExceptElse;
begin
  Result:=TPasImplTryExceptElse.Create('',Self);
  ElseBranch:=Result;
end;

procedure TPasImplTry.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,FinallyExcept,false);
  ForEachChildCall(aMethodCall,Arg,ElseBranch,false);
end;

{ TPasImplExceptOn }

procedure TPasImplExceptOn.FreeChildren(Prepare: boolean);
begin
  VarEl:=TPasVariable(FreeChild(VarEl,Prepare));
  TypeEl:=TPasType(FreeChild(TypeEl,Prepare));
  Body:=TPasImplElement(FreeChild(Body,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TPasImplExceptOn.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if Body=nil then
    Body:=Element;
end;

procedure TPasImplExceptOn.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  ForEachChildCall(aMethodCall,Arg,VarEl,false);
  ForEachChildCall(aMethodCall,Arg,TypeEl,true);
  if Elements.IndexOf(Body)<0 then
    ForEachChildCall(aMethodCall,Arg,Body,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

procedure TPasImplExceptOn.ClearTypeReferences(aType: TPasElement);
begin
  if TypeEl=aType then
    TypeEl:=nil;
end;

function TPasImplExceptOn.VariableName: TPasTreeString;
begin
  If assigned(VarEl) then
    Result:=VarEl.Name
  else
    Result:='';
end;

function TPasImplExceptOn.TypeName: TPasTreeString;
begin
  If assigned(TypeEl) then
    Result:=TypeEl.GetDeclaration(false)
  else
    Result:='';
end;

{ TPasImplStatement }

function TPasImplStatement.CloseOnSemicolon: boolean;
begin
  Result:=true;
end;

{ TPasExpr }

constructor TPasExpr.Create(AParent: TPasElement; AKind: TPasExprKind;
  AOpCode: TExprOpCode);
begin
  inherited Create(ClassName, AParent);
  Kind:=AKind;
  OpCode:=AOpCode;
end;

procedure TPasExpr.FreeChildren(Prepare: boolean);
begin
  Format1:=TPasExpr(FreeChild(Format1,Prepare));
  Format2:=TPasExpr(FreeChild(Format2,Prepare));
  inherited FreeChildren(Prepare);
end;

{ TPrimitiveExpr }

function TPrimitiveExpr.GetDeclaration(full: Boolean): TPasTreeString;
begin
  Result:=Value;
  if full then ;
end;

constructor TPrimitiveExpr.Create(AParent : TPasElement; AKind: TPasExprKind; const AValue : TPasTreeString);
begin
  inherited Create(AParent,AKind, eopNone);
  Value:=AValue;
end;

{ TBoolConstExpr }

constructor TBoolConstExpr.Create(AParent : TPasElement; AKind: TPasExprKind; const ABoolValue : Boolean);
begin
  inherited Create(AParent,AKind, eopNone);
  Value:=ABoolValue;
end;

function TBoolConstExpr.GetDeclaration(full: Boolean): TPasTreeString;

begin
  If Value then
    Result:='True'
  else
    Result:='False';
  if full then ;
end;



{ TUnaryExpr }

function TUnaryExpr.GetDeclaration(full: Boolean): TPasTreeString;

Const
  WordOpcodes = [eopDiv,eopMod,eopshr,eopshl,eopNot,eopAnd,eopOr,eopXor];

begin
  Result:=OpCodeStrings[Opcode];
  if OpCode in WordOpCodes  then
    Result:=Result+' ';
  If Assigned(Operand) then
    Result:=Result+' '+Operand.GetDeclaration(Full);
end;

constructor TUnaryExpr.Create(AParent : TPasElement; AOperand: TPasExpr; AOpCode: TExprOpCode);
begin
  inherited Create(AParent,pekUnary, AOpCode);
  Operand:=AOperand;
  Operand.Parent:=Self;
end;

procedure TUnaryExpr.FreeChildren(Prepare: boolean);
begin
  Operand:=TPasExpr(FreeChild(Operand,Prepare));
  inherited FreeChildren(Prepare);
end;

procedure TUnaryExpr.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Operand,false);
end;

{ TBinaryExpr }

function TBinaryExpr.GetDeclaration(full: Boolean): TPasTreeString;
  function OpLevel(op: TPasExpr): Integer;
  begin
    case op.OpCode of
      eopNot,eopAddress:
        Result := 4;
      eopMultiply, eopDivide, eopDiv, eopMod, eopAnd, eopShl,
      eopShr, eopAs, eopPower:
        Result := 3;
      eopAdd, eopSubtract, eopOr, eopXor:
        Result := 2;
      eopEqual, eopNotEqual, eopLessThan, eopLessthanEqual, eopGreaterThan,
      eopGreaterThanEqual, eopIn, eopIs:
        Result := 1;
    else
      Result := 5; // Numbers and Identifiers
    end;
  end;
var op: TPasTreeString;
begin
  If Kind=pekRange then
    Result:='..'
  else
    begin
    Result:=OpcodeStrings[Opcode];
    if Not (OpCode in [eopAddress,eopDeref,eopSubIdent]) then
      Result:=' '+Result+' ';
    end;
  If Assigned(Left) then
  begin
    op := Left.GetDeclaration(Full);
    if OpLevel(Left) < OpLevel(Self) then
      Result := '(' + op + ')' + Result
    else
      Result := op + Result;
  end;
  If Assigned(Right) then
  begin
    op := Right.GetDeclaration(Full);
    if OpLevel(Left) < OpLevel(Self) then
      Result := Result + '(' + op + ')'
    else
      Result := Result + op;
  end;
end;

constructor TBinaryExpr.Create(AParent : TPasElement; xleft,xright:TPasExpr; AOpCode:TExprOpCode);
begin
  inherited Create(AParent,pekBinary, AOpCode);
  Left:=xleft;
  Left.Parent:=Self;
  Right:=xright;
  Right.Parent:=Self;
end;

constructor TBinaryExpr.CreateRange(AParent : TPasElement; xleft,xright:TPasExpr);
begin
  inherited Create(AParent,pekRange, eopNone);
  Left:=xleft;
  Left.Parent:=Self;
  Right:=xright;
  Right.Parent:=Self;
end;

procedure TBinaryExpr.FreeChildren(Prepare: boolean);
var
  El: TPasExpr;
  SubBin: TBinaryExpr;
begin
  // handle Left of binary chains without stack
  El:=Left;
  while El is TBinaryExpr do
    begin
    SubBin:=TBinaryExpr(El);
    El:=SubBin.Left;
    if (El=nil) or (El.Parent<>SubBin) then
      begin
      El:=SubBin;
      break;
      end;
    end;

  repeat
    if El=Left then
      SubBin:=Self
    else
      SubBin:=TBinaryExpr(El.Parent);
    if SubBin.Left<>nil then
      begin
      if Prepare then
        begin
        if SubBin.Left.Parent<>SubBin then
          SubBin.Left:=nil; // clear reference
        end
      else
        begin
        SubBin.Left.FreeChildren(false);
        SubBin.Left.Free;
        SubBin.Left:=nil;
        end;
      end;
    SubBin.Right:=TPasExpr(SubBin.FreeChild(SubBin.Right,Prepare));
    El:=SubBin;
  until El=Self;

  inherited FreeChildren(Prepare);
end;

procedure TBinaryExpr.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Left,false);
  ForEachChildCall(aMethodCall,Arg,Right,false);
end;

class function TBinaryExpr.IsRightSubIdent(El: TPasElement): boolean;
var
  Bin: TBinaryExpr;
begin
  if (El=nil) or not (El.Parent is TBinaryExpr) then exit(false);
  Bin:=TBinaryExpr(El.Parent);
  Result:=(Bin.Right=El) and (Bin.OpCode=eopSubIdent);
end;

{ TParamsExpr }

function TParamsExpr.GetDeclaration(full: Boolean): TPasTreeString;

Var
  I : Integer;

begin
  Result := '';
  For I:=0 to High(Params) do
    begin
    If (Result<>'')  then
      Result:=Result+', ';
    Result:=Result+Params[I].GetDeclaration(Full);
    if Assigned(Params[I].Format1) then
      Result:=Result+':'+Params[I].Format1.GetDeclaration(false);
    if Assigned(Params[I].Format2) then
      Result:=Result+':'+Params[I].Format2.GetDeclaration(false);
    end;
  if Kind in [pekSet,pekArrayParams] then
    Result := '[' + Result + ']'
  else
    Result := '(' + Result + ')';
  if full and Assigned(Value) then
    Result:=Value.GetDeclaration(True)+Result;
end;

procedure TParamsExpr.AddParam(xp:TPasExpr);
var
  i : Integer;
begin
  i:=Length(Params);
  SetLength(Params, i+1);
  Params[i]:=xp;
end;

procedure TParamsExpr.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Value,false);
  for i:=0 to High(Params) do
    ForEachChildCall(aMethodCall,Arg,Params[i],false);
end;

constructor TParamsExpr.Create(AParent : TPasElement; AKind: TPasExprKind);
begin
  inherited Create(AParent,AKind, eopNone);
end;

procedure TParamsExpr.FreeChildren(Prepare: boolean);
begin
  Value:=TPasExpr(FreeChild(Value,Prepare));
  FreePasExprArray(Self,Params,Prepare);
  inherited FreeChildren(Prepare);
end;

{ TRecordValues }

function TRecordValues.GetDeclaration(full: Boolean): TPasTreeString;

Var
  I : Integer;
begin
  Result := '';
  For I:=0 to High(Fields) do
    begin
    If Result<>'' then
      Result:=Result+'; ';
    Result:=Result+EscapeKeyWord(Fields[I].Name)+': '+Fields[i].ValueExp.getDeclaration(Full);
    end;
  Result:='('+Result+')';
end;

procedure TRecordValues.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to length(Fields)-1 do
    with Fields[i] do
      begin
      if NameExp<>nil then
        ForEachChildCall(aMethodCall,Arg,NameExp,false);
      if ValueExp<>nil then
        ForEachChildCall(aMethodCall,Arg,ValueExp,false);
      end;
end;

constructor TRecordValues.Create(AParent : TPasElement);
begin
  inherited Create(AParent,pekListOfExp, eopNone);
end;

destructor TRecordValues.Destroy;
begin
  Fields:=nil;
  inherited Destroy;
end;

procedure TRecordValues.FreeChildren(Prepare: boolean);
var
  i: Integer;
begin
  for i:=0 to High(Fields) do
    begin
    Fields[i].NameExp:=TPrimitiveExpr(FreeChild(Fields[i].NameExp,Prepare));
    Fields[i].ValueExp:=TPasExpr(FreeChild(Fields[i].ValueExp,Prepare));
    end;
  inherited FreeChildren(Prepare);
end;

procedure TRecordValues.AddField(AName: TPrimitiveExpr; Value: TPasExpr);
var
  i : Integer;
begin
  i:=length(Fields);
  SetLength(Fields, i+1);
  Fields[i].Name:=AName.Value;
  Fields[i].NameExp:=AName;
  AName.Parent:=Self;
  Fields[i].ValueExp:=Value;
  Value.Parent:=Self;
end;

{ TNilExpr }

function TNilExpr.GetDeclaration(full: Boolean): TPasTreeString;
begin
  Result:='Nil';
  if full then ;
end;

{ TInheritedExpr }

function TInheritedExpr.GetDeclaration(full: Boolean): TPasTreeString;
begin
  Result:='Inherited';
  if full then ;
end;

{ TSelfExpr }

function TSelfExpr.GetDeclaration(full: Boolean): TPasTreeString;
begin
  Result:='Self';
  if full then ;
end;

{ TArrayValues }

function TArrayValues.GetDeclaration(full: Boolean): TPasTreeString;

Var
  I : Integer;

begin
  Result := '';
  For I:=0 to High(Values) do
    begin
    If Result<>'' then
      Result:=Result+', ';
    Result:=Result+Values[i].getDeclaration(Full);
    end;
  Result:='('+Result+')';
end;

procedure TArrayValues.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to length(Values)-1 do
    ForEachChildCall(aMethodCall,Arg,Values[i],false);
end;

constructor TArrayValues.Create(AParent : TPasElement);
begin
  inherited Create(AParent,pekListOfExp, eopNone);
end;

destructor TArrayValues.Destroy;
begin
  Values:=nil;
  inherited Destroy;
end;

procedure TArrayValues.FreeChildren(Prepare: boolean);
begin
  FreePasExprArray(Self,Values,Prepare);
  inherited FreeChildren(Prepare);
end;

procedure TArrayValues.AddValues(AValue:TPasExpr);
var
  i : Integer;
begin
  i:=length(Values);
  SetLength(Values, i+1);
  Values[i]:=AValue;
  AValue.Parent:=Self;
end;

{ TNilExpr }

constructor TNilExpr.Create(AParent : TPasElement);
begin
  inherited Create(AParent,pekNil, eopNone);
end;

{ TInheritedExpr }

constructor TInheritedExpr.Create(AParent : TPasElement);
begin
  inherited Create(AParent,pekInherited, eopNone);
end;


{ TSelfExpr }

constructor TSelfExpr.Create(AParent : TPasElement);
begin
  inherited Create(AParent,pekSelf, eopNone);
end;

{ TNamedArgExpr }

constructor TNamedArgExpr.Create(AParent: TPasElement; AName: TPrimitiveExpr; AValue: TPasExpr);
begin
  NameExpr:=aName;
  ValueExpr:=aValue;
end;

function TNamedArgExpr.GetDeclaration(full: Boolean): TPasTreeString;
begin
  Result:=NameExpr.GetDeclaration(True)+':='+ValueExpr.GetDeclaration(True);
end;

procedure TNamedArgExpr.FreeChildren(Prepare: boolean);
begin
  inherited FreeChildren(Prepare);
  NameExpr:=TPrimitiveExpr(FreeChild(NameExpr,Prepare));
  ValueExpr:=TPasExpr(FreeChild(ValueExpr,Prepare));
end;

procedure TNamedArgExpr.ForEachCall(const aMethodCall: TOnForEachPasElement; const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,NameExpr,False);
  ForEachChildCall(aMethodCall,Arg,ValueExpr,False);
end;

{ TPasLabels }

constructor TPasLabels.Create(const AName:TPasTreeString;AParent:TPasElement);
begin
  inherited Create(AName,AParent);
  Labels := TStringList.Create;
end;

destructor TPasLabels.Destroy;
begin
  FreeAndNil(Labels);
  inherited Destroy;
end;

function TPasLabels.GetDeclaration(full: Boolean): TPasTreeString;
begin
  if Full then ;
  Result:=Labels.CommaText;
end;

end.
