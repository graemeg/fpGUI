{
    fpGUI IDE - Go to Declaration Tests

    Tests for identifier resolution and Go to Declaration functionality.
}
unit ide.test.declaration;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  ide.highlighter,
  ide.declaration;

type

  { TTestDeclaration }

  TTestDeclaration = class(TTestCase)
  private
    FHL: TPascalHighlighter;
    FLines: TStringList;
    procedure SetSource(const ASource: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Phase A -- Identifier extraction }
    procedure TestGetIdentAtCursor_OnIdentifier;
    procedure TestGetIdentAtCursor_OnKeyword;
    procedure TestGetIdentAtCursor_OnWhitespace;
    procedure TestGetIdentAtCursor_StartOfIdent;
    procedure TestGetIdentAtCursor_EndOfIdent;
  end;


implementation

const
  cSimpleProgram =
    'program Test;'              + LineEnding +   // 0
    'begin'                      + LineEnding +   // 1
    '  WriteLn(''hello'');'      + LineEnding +   // 2
    'end.';                                        // 3

  cVarProgram =
    'program Test;'              + LineEnding +   // 0
    'var'                        + LineEnding +   // 1
    '  MyCounter: Integer;'      + LineEnding +   // 2
    'begin'                      + LineEnding +   // 3
    '  MyCounter := 1;'          + LineEnding +   // 4
    'end.';                                        // 5


{ TTestDeclaration }

procedure TTestDeclaration.SetSource(const ASource: string);
begin
  FLines.Text := ASource;
  FHL.Tokenise(ASource);
end;

procedure TTestDeclaration.SetUp;
begin
  FHL := TPascalHighlighter.Create;
  FLines := TStringList.Create;
end;

procedure TTestDeclaration.TearDown;
begin
  FLines.Free;
  FHL.Free;
end;

procedure TTestDeclaration.TestGetIdentAtCursor_OnIdentifier;
begin
  SetSource(cSimpleProgram);
  // Line 2: '  WriteLn(''hello'');'  -- WriteLn starts at column 2
  CheckEquals('WriteLn',
    GetIdentifierAtCursor(FHL, FLines, 2, 4),
    'Cursor in middle of WriteLn should return WriteLn');
end;

procedure TTestDeclaration.TestGetIdentAtCursor_OnKeyword;
begin
  SetSource(cSimpleProgram);
  // Line 1: 'begin' -- keyword, not an identifier
  CheckEquals('',
    GetIdentifierAtCursor(FHL, FLines, 1, 2),
    'Cursor on keyword begin should return empty');
end;

procedure TTestDeclaration.TestGetIdentAtCursor_OnWhitespace;
begin
  SetSource(cSimpleProgram);
  // Line 2: '  WriteLn(''hello'');' -- column 0 is whitespace
  CheckEquals('',
    GetIdentifierAtCursor(FHL, FLines, 2, 0),
    'Cursor on whitespace should return empty');
end;

procedure TTestDeclaration.TestGetIdentAtCursor_StartOfIdent;
begin
  SetSource(cVarProgram);
  // Line 4: '  MyCounter := 1;'  -- MyCounter starts at column 2
  CheckEquals('MyCounter',
    GetIdentifierAtCursor(FHL, FLines, 4, 2),
    'Cursor at first char of MyCounter should return MyCounter');
end;

procedure TTestDeclaration.TestGetIdentAtCursor_EndOfIdent;
begin
  SetSource(cVarProgram);
  // Line 4: '  MyCounter := 1;'  -- MyCounter ends at column 10
  CheckEquals('MyCounter',
    GetIdentifierAtCursor(FHL, FLines, 4, 10),
    'Cursor at last char of MyCounter should return MyCounter');
end;


initialization
  RegisterTest(TTestDeclaration);

end.
