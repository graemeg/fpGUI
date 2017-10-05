{
    CSV Parser unit tests
}
unit CsvParser_Test;

{$mode objfpc}{$H+}

interface
uses
  Classes
  ,SysUtils
  ,CsvParser
  ,TestFramework
  ; 


type
  TTestCSVParser= class(TTestCase)
  private
    FParser: TCsvParser;
    FFieldList: TStringList;
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
    procedure BadChar;
    procedure MissingQuote;
  published
    procedure TestExtractFields;
    procedure TestBadChar;
    procedure TestMissingQuote;
  end; 


procedure RegisterTests;


implementation

uses
  StrUtils
  ;

procedure RegisterTests;
begin
  AddToNonPersistentTestSuite(TTestCSVParser);
end;


procedure TTestCSVParser.SetUp; 
begin
  FParser    := TCsvParser.Create;
  FFieldList := TStringList.Create;
end; 


procedure TTestCSVParser.TearDown; 
begin
  FParser.Free;
  FFieldList.Free;
end; 


procedure TTestCSVParser.BadChar; 
const
  TestLine = '"This is a bad string"see?';
begin
  FParser.ExtractFields(TestLine,FFieldList);
end;


procedure TTestCSVParser.MissingQuote; 
const
  TestLine = '"This is a line with a missing quote';
begin
  FParser.ExtractFields(TestLine,FFieldList);
end;


procedure TTestCSVParser.TestExtractFields; 
const
  Field1   = '123';
  Field2   = '"Field with comma, in it"';
  Field3   = '';
  Field4   = 'LastField';
  TestLine = Field1 + ',' + Field2 + ',' + Field3 + ',' + Field4;
begin
  FParser.ExtractFields(TestLine,FFieldList);
  CheckEquals(Field1, FFieldList[0], 'Failing on 1');
  { Remove quotes to test Field2 - parser remove outer double quotes } 
  CheckEquals(MidStr(Field2, 2, Length(Field2)-2), FFieldList[1], 'Failing on 2');
  CheckEquals(Field3, FFieldList[2], 'Failing on 3');
  CheckEquals(Field4, FFieldList[3], 'Failing on 4');
end;


procedure TTestCSVParser.TestBadChar; 
begin
  CheckException(@BadChar, Exception, 'Did not raise exception for extra chars after end quote');
end;


procedure TTestCSVParser.TestMissingQuote; 
begin
  CheckException(@MissingQuote, Exception, 'Did not raise exception for missing quote');
end;


end.

