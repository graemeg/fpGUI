{
    fpGUI IDE - Variables Panel Logic Tests

    Tests for the pure-logic functions in ide.variables:
      - BuildVarNodeText
      - VarValueIsExpandable
      - ExtractVarTypeName
      - ParseVarChildren
}
unit ide.test.variables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpcunit, testregistry,
  ide.variables;

type

  { TTestBuildVarNodeText }

  TTestBuildVarNodeText = class(TTestCase)
  published
    procedure TestSimpleWithType;
    procedure TestSimpleWithoutType;
    procedure TestEmptyTypeNameIgnoredWhenShowTypeTrue;
    procedure TestStringValue;
    procedure TestCompositeValueSummary;
  end;

  { TTestVarValueIsExpandable }

  TTestVarValueIsExpandable = class(TTestCase)
  published
    procedure TestSimpleIntegerNotExpandable;
    procedure TestNilNotExpandable;
    procedure TestEllipsisNotExpandable;
    procedure TestErrorStringNotExpandable;
    procedure TestEmptyStringNotExpandable;
    procedure TestBooleanNotExpandable;
    procedure TestQuotedStringNotExpandable;
    procedure TestRecordIsExpandable;
    procedure TestClassWithAddressIsExpandable;
    procedure TestNestedRecordIsExpandable;
  end;

  { TTestExtractVarTypeName }

  TTestExtractVarTypeName = class(TTestCase)
  published
    procedure TestSimpleIntegerReturnsEmpty;
    procedure TestNilReturnsEmpty;
    procedure TestQuotedStringReturnsEmpty;
    procedure TestRecordReturnsTypeName;
    procedure TestClassReturnsTypeName;
    procedure TestNestedDoesNotLeakParenthesis;
  end;

  { TTestParseVarChildren }

  TTestParseVarChildren = class(TTestCase)
  private
    procedure AssertChildCount(AExpected: Integer; const AParentPath, AValue: string);
    procedure AssertChild(const AChildren: TVarNodeDataArray; AIndex: Integer;
        const AExpectedName, AExpectedValue: string);
  published
    { Empty / degenerate inputs }
    procedure TestEmptyValueReturnsNoChildren;
    procedure TestNilReturnsNoChildren;
    procedure TestSimpleIntegerReturnsNoChildren;

    { Single field }
    procedure TestSingleFieldRecord;

    { Multiple fields }
    procedure TestTwoFieldRecord;
    procedure TestThreeFieldRecord;

    { Nested composite }
    procedure TestNestedRecordChildIsExpandable;
    procedure TestNestedRecordChildParsesCorrectly;

    { String values containing commas }
    procedure TestStringWithCommaNotSplit;

    { FullPath construction }
    procedure TestChildFullPathWithParent;
    procedure TestChildFullPathNoParent;

    { Class values }
    procedure TestClassNonNilIsExpandable;

    { Type extraction on children }
    procedure TestChildTypeExtractedFromValue;
  end;


implementation


{ TTestBuildVarNodeText }

procedure TTestBuildVarNodeText.TestSimpleWithType;
begin
  AssertEquals('i = 42 : Integer', BuildVarNodeText('i', '42', 'Integer', True));
end;

procedure TTestBuildVarNodeText.TestSimpleWithoutType;
begin
  AssertEquals('i = 42', BuildVarNodeText('i', '42', 'Integer', False));
end;

procedure TTestBuildVarNodeText.TestEmptyTypeNameIgnoredWhenShowTypeTrue;
begin
  { When TypeName is empty, type suffix must not appear even if ShowType=True }
  AssertEquals('x = 5', BuildVarNodeText('x', '5', '', True));
end;

procedure TTestBuildVarNodeText.TestStringValue;
begin
  AssertEquals('s = ''hello'' : String', BuildVarNodeText('s', '''hello''', 'String', True));
end;

procedure TTestBuildVarNodeText.TestCompositeValueSummary;
begin
  { Composite: the full value summary is the display value }
  AssertEquals(
    'r = TPoint { X: 10, Y: 20 } : TPoint',
    BuildVarNodeText('r', 'TPoint { X: 10, Y: 20 }', 'TPoint', True));
end;


{ TTestVarValueIsExpandable }

procedure TTestVarValueIsExpandable.TestSimpleIntegerNotExpandable;
begin
  AssertFalse(VarValueIsExpandable('42'));
end;

procedure TTestVarValueIsExpandable.TestNilNotExpandable;
begin
  AssertFalse(VarValueIsExpandable('nil'));
end;

procedure TTestVarValueIsExpandable.TestEllipsisNotExpandable;
begin
  AssertFalse(VarValueIsExpandable('...'));
end;

procedure TTestVarValueIsExpandable.TestErrorStringNotExpandable;
begin
  AssertFalse(VarValueIsExpandable('<error: type not found>'));
end;

procedure TTestVarValueIsExpandable.TestEmptyStringNotExpandable;
begin
  AssertFalse(VarValueIsExpandable(''));
end;

procedure TTestVarValueIsExpandable.TestBooleanNotExpandable;
begin
  AssertFalse(VarValueIsExpandable('True'));
  AssertFalse(VarValueIsExpandable('False'));
end;

procedure TTestVarValueIsExpandable.TestQuotedStringNotExpandable;
begin
  AssertFalse(VarValueIsExpandable('''hello world'''));
end;

procedure TTestVarValueIsExpandable.TestRecordIsExpandable;
begin
  AssertTrue(VarValueIsExpandable('TPoint { X: 10, Y: 20 }'));
end;

procedure TTestVarValueIsExpandable.TestClassWithAddressIsExpandable;
begin
  AssertTrue(VarValueIsExpandable('TMyClass(@$1234ABCD) { FName: ''hello'', FCount: 3 }'));
end;

procedure TTestVarValueIsExpandable.TestNestedRecordIsExpandable;
begin
  AssertTrue(VarValueIsExpandable('TOuter { Inner: TInner { A: 1 }, B: 2 }'));
end;


{ TTestExtractVarTypeName }

procedure TTestExtractVarTypeName.TestSimpleIntegerReturnsEmpty;
begin
  AssertEquals('', ExtractVarTypeName('42'));
end;

procedure TTestExtractVarTypeName.TestNilReturnsEmpty;
begin
  AssertEquals('', ExtractVarTypeName('nil'));
end;

procedure TTestExtractVarTypeName.TestQuotedStringReturnsEmpty;
begin
  AssertEquals('', ExtractVarTypeName('''hello'''));
end;

procedure TTestExtractVarTypeName.TestRecordReturnsTypeName;
begin
  AssertEquals('TPoint', ExtractVarTypeName('TPoint { X: 10, Y: 20 }'));
end;

procedure TTestExtractVarTypeName.TestClassReturnsTypeName;
begin
  AssertEquals('TMyClass',
    ExtractVarTypeName('TMyClass(@$1234ABCD) { FField: 99 }'));
end;

procedure TTestExtractVarTypeName.TestNestedDoesNotLeakParenthesis;
begin
  { The result must not contain '@' or parentheses }
  AssertEquals('TObj',
    ExtractVarTypeName('TObj(@$DEADBEEF) { X: TPoint { A: 1, B: 2 } }'));
end;


{ TTestParseVarChildren }

procedure TTestParseVarChildren.AssertChildCount(AExpected: Integer;
    const AParentPath, AValue: string);
var
  Children: TVarNodeDataArray;
  i: Integer;
begin
  Children := ParseVarChildren(AParentPath, AValue);
  try
    AssertEquals('child count', AExpected, Length(Children));
  finally
    for i := 0 to High(Children) do
      Children[i].Free;
  end;
end;

procedure TTestParseVarChildren.AssertChild(const AChildren: TVarNodeDataArray;
    AIndex: Integer; const AExpectedName, AExpectedValue: string);
begin
  AssertEquals('child[' + IntToStr(AIndex) + '].Name',  AExpectedName,  AChildren[AIndex].Name);
  AssertEquals('child[' + IntToStr(AIndex) + '].Value', AExpectedValue, AChildren[AIndex].Value);
end;

procedure TTestParseVarChildren.TestEmptyValueReturnsNoChildren;
begin
  AssertChildCount(0, 'v', '');
end;

procedure TTestParseVarChildren.TestNilReturnsNoChildren;
begin
  AssertChildCount(0, 'obj', 'nil');
end;

procedure TTestParseVarChildren.TestSimpleIntegerReturnsNoChildren;
begin
  AssertChildCount(0, 'n', '42');
end;

procedure TTestParseVarChildren.TestSingleFieldRecord;
var
  Children: TVarNodeDataArray;
begin
  Children := ParseVarChildren('r', 'TPoint1D { X: 5 }');
  try
    AssertEquals(1, Length(Children));
    AssertChild(Children, 0, 'X', '5');
  finally
    if Length(Children) > 0 then Children[0].Free;
  end;
end;

procedure TTestParseVarChildren.TestTwoFieldRecord;
var
  Children: TVarNodeDataArray;
begin
  Children := ParseVarChildren('p', 'TPoint { X: 10, Y: 20 }');
  try
    AssertEquals(2, Length(Children));
    AssertChild(Children, 0, 'X', '10');
    AssertChild(Children, 1, 'Y', '20');
  finally
    if Length(Children) > 0 then Children[0].Free;
    if Length(Children) > 1 then Children[1].Free;
  end;
end;

procedure TTestParseVarChildren.TestThreeFieldRecord;
var
  Children: TVarNodeDataArray;
begin
  Children := ParseVarChildren('r', 'TRect { Left: 0, Top: 0, Width: 100 }');
  try
    AssertEquals(3, Length(Children));
    AssertChild(Children, 0, 'Left',  '0');
    AssertChild(Children, 1, 'Top',   '0');
    AssertChild(Children, 2, 'Width', '100');
  finally
    if Length(Children) > 0 then Children[0].Free;
    if Length(Children) > 1 then Children[1].Free;
    if Length(Children) > 2 then Children[2].Free;
  end;
end;

procedure TTestParseVarChildren.TestNestedRecordChildIsExpandable;
var
  Children: TVarNodeDataArray;
begin
  { The nested child 'Inner: TInner { A: 1 }' should be expandable }
  Children := ParseVarChildren('r', 'TOuter { Inner: TInner { A: 1 }, B: 2 }');
  try
    AssertEquals(2, Length(Children));
    AssertTrue('Inner should be expandable', Children[0].IsExpandable);
    AssertFalse('B should not be expandable', Children[1].IsExpandable);
  finally
    if Length(Children) > 0 then Children[0].Free;
    if Length(Children) > 1 then Children[1].Free;
  end;
end;

procedure TTestParseVarChildren.TestNestedRecordChildParsesCorrectly;
var
  Children: TVarNodeDataArray;
begin
  Children := ParseVarChildren('r', 'TOuter { Inner: TInner { A: 1 }, B: 42 }');
  try
    AssertEquals(2, Length(Children));
    AssertChild(Children, 0, 'Inner', 'TInner { A: 1 }');
    AssertChild(Children, 1, 'B', '42');
  finally
    if Length(Children) > 0 then Children[0].Free;
    if Length(Children) > 1 then Children[1].Free;
  end;
end;

procedure TTestParseVarChildren.TestStringWithCommaNotSplit;
var
  Children: TVarNodeDataArray;
begin
  { A string value containing ', ' must not be treated as a separator }
  Children := ParseVarChildren('r', 'TRec { Name: ''hello, world'', Age: 25 }');
  try
    AssertEquals(2, Length(Children));
    AssertChild(Children, 0, 'Name', '''hello, world''');
    AssertChild(Children, 1, 'Age',  '25');
  finally
    if Length(Children) > 0 then Children[0].Free;
    if Length(Children) > 1 then Children[1].Free;
  end;
end;

procedure TTestParseVarChildren.TestChildFullPathWithParent;
var
  Children: TVarNodeDataArray;
begin
  Children := ParseVarChildren('myVar', 'TPoint { X: 1, Y: 2 }');
  try
    AssertEquals(2, Length(Children));
    AssertEquals('myVar.X', Children[0].FullPath);
    AssertEquals('myVar.Y', Children[1].FullPath);
  finally
    if Length(Children) > 0 then Children[0].Free;
    if Length(Children) > 1 then Children[1].Free;
  end;
end;

procedure TTestParseVarChildren.TestChildFullPathNoParent;
var
  Children: TVarNodeDataArray;
begin
  { When AParentPath is empty, FullPath is just the field name }
  Children := ParseVarChildren('', 'TPoint { X: 1, Y: 2 }');
  try
    AssertEquals(2, Length(Children));
    AssertEquals('X', Children[0].FullPath);
    AssertEquals('Y', Children[1].FullPath);
  finally
    if Length(Children) > 0 then Children[0].Free;
    if Length(Children) > 1 then Children[1].Free;
  end;
end;

procedure TTestParseVarChildren.TestClassNonNilIsExpandable;
var
  Children: TVarNodeDataArray;
begin
  Children := ParseVarChildren('obj',
    'TMyClass(@$1234ABCD) { FName: ''test'', FCount: 7 }');
  try
    AssertEquals(2, Length(Children));
    AssertChild(Children, 0, 'FName',  '''test''');
    AssertChild(Children, 1, 'FCount', '7');
  finally
    if Length(Children) > 0 then Children[0].Free;
    if Length(Children) > 1 then Children[1].Free;
  end;
end;

procedure TTestParseVarChildren.TestChildTypeExtractedFromValue;
var
  Children: TVarNodeDataArray;
begin
  { A child whose value is itself a composite should have TypeName extracted }
  Children := ParseVarChildren('r', 'TOuter { Sub: TPoint { X: 1, Y: 2 }, N: 99 }');
  try
    AssertEquals(2, Length(Children));
    AssertEquals('TPoint', Children[0].TypeName);
    AssertEquals('',       Children[1].TypeName);
  finally
    if Length(Children) > 0 then Children[0].Free;
    if Length(Children) > 1 then Children[1].Free;
  end;
end;


initialization
  RegisterTest(TTestBuildVarNodeText);
  RegisterTest(TTestVarValueIsExpandable);
  RegisterTest(TTestExtractVarTypeName);
  RegisterTest(TTestParseVarChildren);

end.
