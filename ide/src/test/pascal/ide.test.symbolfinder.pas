{
    fpGUI IDE - Symbol Finder Tests

    Tests for symbol extraction and fuzzy filtering logic used by
    the Navigate to Symbol dialog (Ctrl+N).
}
unit ide.test.symbolfinder;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  ide.filefinder,
  ide.symbolfinder;

type

  { TTestScanSourceForSymbols }

  TTestScanSourceForSymbols = class(TTestCase)
  private
    FSymbols: TSymbolEntryArray;
    procedure ScanSource(const ASource: string);
    function FindSymbol(const AName: string): Integer;
  published
    procedure TestExtractProcedure;
    procedure TestExtractFunction;
    procedure TestExtractClassType;
    procedure TestExtractRecordType;
    procedure TestExtractEnumType;
    procedure TestExtractConst;
    procedure TestExtractVar;
    procedure TestExtractMultipleSymbols;
    procedure TestIgnoresImplementation;
    procedure TestClassMethods;
    procedure TestConstructorDestructor;
    procedure TestProgramFile;
    procedure TestEmptySource;
    procedure TestLineNumbers;
  end;

  { TTestFilterSymbols }

  TTestFilterSymbols = class(TTestCase)
  private
    FSymbols: TSymbolEntryArray;
    function MakeSymbol(const AName: string; AKind: TSymbolKind;
      ALine: Integer): TSymbolEntry;
    procedure SetupTestSymbols;
  protected
    procedure SetUp; override;
  published
    procedure TestFilterByName;
    procedure TestFilterCamelCase;
    procedure TestFilterEmptyReturnsAll;
    procedure TestFilterNoMatch;
    procedure TestFilterSortedByScore;
  end;

  { TTestSymbolKindToStr }

  TTestSymbolKindToStr = class(TTestCase)
  published
    procedure TestAllKinds;
  end;


implementation

{ TTestScanSourceForSymbols }

procedure TTestScanSourceForSymbols.ScanSource(const ASource: string);
begin
  SetLength(FSymbols, 0);
  ScanSourceForSymbols(ASource, 'test.pas', FSymbols);
end;

function TTestScanSourceForSymbols.FindSymbol(const AName: string): Integer;
var
  i: Integer;
begin
  for i := 0 to High(FSymbols) do
    if FSymbols[i].Name = AName then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure TTestScanSourceForSymbols.TestExtractProcedure;
begin
  ScanSource(
    'unit test;' + LineEnding +
    'interface' + LineEnding +
    'procedure DoSomething;' + LineEnding +
    'implementation' + LineEnding +
    'end.');
  CheckTrue(Length(FSymbols) >= 1,
    Format('Expected at least 1 symbol, got %d', [Length(FSymbols)]));
  CheckEquals('DoSomething', FSymbols[FindSymbol('DoSomething')].Name);
  CheckTrue(FSymbols[FindSymbol('DoSomething')].Kind = skProcedure,
    'Should be skProcedure');
end;

procedure TTestScanSourceForSymbols.TestExtractFunction;
begin
  ScanSource(
    'unit test;' + LineEnding +
    'interface' + LineEnding +
    'function GetValue: Integer;' + LineEnding +
    'implementation' + LineEnding +
    'end.');
  CheckTrue(FindSymbol('GetValue') >= 0, 'Should find GetValue');
  CheckTrue(FSymbols[FindSymbol('GetValue')].Kind = skFunction,
    'Should be skFunction');
end;

procedure TTestScanSourceForSymbols.TestExtractClassType;
begin
  ScanSource(
    'unit test;' + LineEnding +
    'interface' + LineEnding +
    'type' + LineEnding +
    '  TMyClass = class(TObject)' + LineEnding +
    '  end;' + LineEnding +
    'implementation' + LineEnding +
    'end.');
  CheckTrue(FindSymbol('TMyClass') >= 0, 'Should find TMyClass');
  CheckTrue(FSymbols[FindSymbol('TMyClass')].Kind = skType,
    'Should be skType');
end;

procedure TTestScanSourceForSymbols.TestExtractRecordType;
begin
  ScanSource(
    'unit test;' + LineEnding +
    'interface' + LineEnding +
    'type' + LineEnding +
    '  TPoint = record' + LineEnding +
    '    X, Y: Integer;' + LineEnding +
    '  end;' + LineEnding +
    'implementation' + LineEnding +
    'end.');
  CheckTrue(FindSymbol('TPoint') >= 0, 'Should find TPoint');
  CheckTrue(FSymbols[FindSymbol('TPoint')].Kind = skType,
    'Should be skType');
end;

procedure TTestScanSourceForSymbols.TestExtractEnumType;
begin
  ScanSource(
    'unit test;' + LineEnding +
    'interface' + LineEnding +
    'type' + LineEnding +
    '  TColor = (clRed, clGreen, clBlue);' + LineEnding +
    'implementation' + LineEnding +
    'end.');
  CheckTrue(FindSymbol('TColor') >= 0, 'Should find TColor');
  CheckTrue(FSymbols[FindSymbol('TColor')].Kind = skType,
    'Should be skType');
end;

procedure TTestScanSourceForSymbols.TestExtractConst;
begin
  ScanSource(
    'unit test;' + LineEnding +
    'interface' + LineEnding +
    'const' + LineEnding +
    '  MaxItems = 100;' + LineEnding +
    'implementation' + LineEnding +
    'end.');
  CheckTrue(FindSymbol('MaxItems') >= 0, 'Should find MaxItems');
  CheckTrue(FSymbols[FindSymbol('MaxItems')].Kind = skConst,
    'Should be skConst');
end;

procedure TTestScanSourceForSymbols.TestExtractVar;
begin
  ScanSource(
    'unit test;' + LineEnding +
    'interface' + LineEnding +
    'var' + LineEnding +
    '  GlobalCount: Integer;' + LineEnding +
    'implementation' + LineEnding +
    'end.');
  CheckTrue(FindSymbol('GlobalCount') >= 0, 'Should find GlobalCount');
  CheckTrue(FSymbols[FindSymbol('GlobalCount')].Kind = skVar,
    'Should be skVar');
end;

procedure TTestScanSourceForSymbols.TestExtractMultipleSymbols;
begin
  ScanSource(
    'unit test;' + LineEnding +
    'interface' + LineEnding +
    'type' + LineEnding +
    '  TFoo = class' + LineEnding +
    '  end;' + LineEnding +
    'const' + LineEnding +
    '  Version = 1;' + LineEnding +
    'procedure Init;' + LineEnding +
    'function Done: Boolean;' + LineEnding +
    'implementation' + LineEnding +
    'end.');
  CheckTrue(FindSymbol('TFoo') >= 0, 'Should find TFoo');
  CheckTrue(FindSymbol('Version') >= 0, 'Should find Version');
  CheckTrue(FindSymbol('Init') >= 0, 'Should find Init');
  CheckTrue(FindSymbol('Done') >= 0, 'Should find Done');
  CheckTrue(Length(FSymbols) >= 4,
    Format('Expected at least 4 symbols, got %d', [Length(FSymbols)]));
end;

procedure TTestScanSourceForSymbols.TestIgnoresImplementation;
begin
  ScanSource(
    'unit test;' + LineEnding +
    'interface' + LineEnding +
    'procedure PublicProc;' + LineEnding +
    'implementation' + LineEnding +
    'procedure PrivateProc;' + LineEnding +
    'begin end;' + LineEnding +
    'end.');
  CheckTrue(FindSymbol('PublicProc') >= 0, 'Should find PublicProc');
  CheckTrue(FindSymbol('PrivateProc') < 0, 'Should NOT find PrivateProc');
end;

procedure TTestScanSourceForSymbols.TestClassMethods;
begin
  ScanSource(
    'unit test;' + LineEnding +
    'interface' + LineEnding +
    'type' + LineEnding +
    '  TFoo = class' + LineEnding +
    '    procedure Bar;' + LineEnding +
    '    function Baz: Integer;' + LineEnding +
    '  end;' + LineEnding +
    'implementation' + LineEnding +
    'end.');
  CheckTrue(FindSymbol('TFoo') >= 0, 'Should find TFoo');
  { Class methods should appear with qualified display name }
  CheckTrue(FindSymbol('Bar') >= 0, 'Should find Bar');
  CheckTrue(FSymbols[FindSymbol('Bar')].DisplayName = 'TFoo.Bar',
    'DisplayName should be TFoo.Bar');
end;

procedure TTestScanSourceForSymbols.TestConstructorDestructor;
begin
  ScanSource(
    'unit test;' + LineEnding +
    'interface' + LineEnding +
    'type' + LineEnding +
    '  TFoo = class' + LineEnding +
    '    constructor Create;' + LineEnding +
    '    destructor Destroy; override;' + LineEnding +
    '  end;' + LineEnding +
    'implementation' + LineEnding +
    'end.');
  CheckTrue(FindSymbol('Create') >= 0, 'Should find Create');
  CheckTrue(FSymbols[FindSymbol('Create')].Kind = skConstructor,
    'Should be skConstructor');
  CheckTrue(FindSymbol('Destroy') >= 0, 'Should find Destroy');
  CheckTrue(FSymbols[FindSymbol('Destroy')].Kind = skDestructor,
    'Should be skDestructor');
end;

procedure TTestScanSourceForSymbols.TestProgramFile;
begin
  ScanSource(
    'program test;' + LineEnding +
    'procedure DoWork;' + LineEnding +
    'begin end;' + LineEnding +
    'begin' + LineEnding +
    'end.');
  CheckTrue(FindSymbol('DoWork') >= 0,
    'Program files should still extract top-level procedures');
end;

procedure TTestScanSourceForSymbols.TestEmptySource;
begin
  ScanSource('');
  CheckEquals(0, Length(FSymbols), 'Empty source should produce no symbols');
end;

procedure TTestScanSourceForSymbols.TestLineNumbers;
begin
  ScanSource(
    'unit test;' + LineEnding +       { line 1 }
    'interface' + LineEnding +         { line 2 }
    'procedure First;' + LineEnding +  { line 3 }
    'procedure Second;' + LineEnding + { line 4 }
    'implementation' + LineEnding +
    'end.');
  CheckTrue(FindSymbol('First') >= 0, 'Should find First');
  CheckEquals(3, FSymbols[FindSymbol('First')].Line,
    'First should be on line 3');
  CheckTrue(FindSymbol('Second') >= 0, 'Should find Second');
  CheckEquals(4, FSymbols[FindSymbol('Second')].Line,
    'Second should be on line 4');
end;

{ TTestFilterSymbols }

function TTestFilterSymbols.MakeSymbol(const AName: string;
  AKind: TSymbolKind; ALine: Integer): TSymbolEntry;
begin
  Result.Name := AName;
  Result.DisplayName := AName;
  Result.Kind := AKind;
  Result.FileName := 'test.pas';
  Result.RelativePath := 'src/';
  Result.FullPath := '/project/src/test.pas';
  Result.Line := ALine;
end;

procedure TTestFilterSymbols.SetupTestSymbols;
begin
  SetLength(FSymbols, 5);
  FSymbols[0] := MakeSymbol('TMainForm', skType, 10);
  FSymbols[1] := MakeSymbol('DoSomething', skProcedure, 20);
  FSymbols[2] := MakeSymbol('GetMainValue', skFunction, 30);
  FSymbols[3] := MakeSymbol('MaxCount', skConst, 5);
  FSymbols[4] := MakeSymbol('Initialize', skProcedure, 40);
end;

procedure TTestFilterSymbols.SetUp;
begin
  SetupTestSymbols;
end;

procedure TTestFilterSymbols.TestFilterByName;
var
  Results: TFilteredSymbolArray;
begin
  Results := FilterSymbolsEx('DoSome', FSymbols);
  CheckTrue(Length(Results) >= 1, 'Should match at least 1 symbol');
  CheckEquals('DoSomething', Results[0].Entry.Name);
end;

procedure TTestFilterSymbols.TestFilterCamelCase;
var
  Results: TFilteredSymbolArray;
begin
  Results := FilterSymbolsEx('MF', FSymbols);
  CheckTrue(Length(Results) >= 1, 'Should match at least 1 symbol for "MF"');
  CheckEquals('TMainForm', Results[0].Entry.Name,
    'TMainForm should match "MF" via word-start');
end;

procedure TTestFilterSymbols.TestFilterEmptyReturnsAll;
var
  Results: TFilteredSymbolArray;
begin
  Results := FilterSymbolsEx('', FSymbols);
  CheckEquals(Length(FSymbols), Length(Results),
    'Empty pattern should return all symbols');
end;

procedure TTestFilterSymbols.TestFilterNoMatch;
var
  Results: TFilteredSymbolArray;
begin
  Results := FilterSymbolsEx('zzzzz', FSymbols);
  CheckEquals(0, Length(Results), 'Non-matching pattern should return empty');
end;

procedure TTestFilterSymbols.TestFilterSortedByScore;
var
  Results: TFilteredSymbolArray;
begin
  Results := FilterSymbolsEx('Main', FSymbols);
  CheckTrue(Length(Results) >= 2, 'Should match at least 2 symbols');
  { TMainForm should score higher than GetMainValue (prefix vs substring) }
  CheckEquals('TMainForm', Results[0].Entry.Name,
    'TMainForm should rank first (prefix match)');
end;

{ TTestSymbolKindToStr }

procedure TTestSymbolKindToStr.TestAllKinds;
begin
  CheckEquals('procedure', SymbolKindToStr(skProcedure));
  CheckEquals('function', SymbolKindToStr(skFunction));
  CheckEquals('constructor', SymbolKindToStr(skConstructor));
  CheckEquals('destructor', SymbolKindToStr(skDestructor));
  CheckEquals('type', SymbolKindToStr(skType));
  CheckEquals('const', SymbolKindToStr(skConst));
  CheckEquals('var', SymbolKindToStr(skVar));
  CheckEquals('property', SymbolKindToStr(skProperty));
end;


initialization
  RegisterTest(TTestScanSourceForSymbols);
  RegisterTest(TTestFilterSymbols);
  RegisterTest(TTestSymbolKindToStr);

end.
