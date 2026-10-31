unit tcwraptext;

{$mode objfpc}{$H+}

{ Regression tests for fpgWrapText (fpg_dialogs.pas), used by the message
  dialogs to wrap informative text (GitHub issue #205).

  Bugs in the original implementation:
  - A word wider than the line width (eg: a long file path without spaces)
    caused an endless loop while searching backwards for a space.
  - A single character wider than the line width caused an endless loop,
    because the line was shrunk to zero characters.

  A mock font with a fixed character width keeps the tests deterministic
  and platform independent. }

interface

uses
  Classes,
  SysUtils,
  fpcunit, testregistry,
  fpg_base;

type
  { Every character is CharWidth pixels wide. }
  TWrapMockFont = class(TfpgFontResourceBase)
  private
    FCharWidth: integer;
  public
    constructor Create(ACharWidth: integer); reintroduce;
    function GetAscent: integer; override;
    function GetDescent: integer; override;
    function GetHeight: integer; override;
    function GetTextWidth(const txt: string): integer; override;
    function GetCanvasRef: TObject; override;
    function HandleIsValid: boolean; override;
  end;

  TTestWrapText = class(TTestCase)
  private
    FFont: TWrapMockFont;
    FLines: TStringList;
    FWidth: integer;
    procedure Wrap(const AText: string; ALineWidth: integer);
    procedure CheckLinesRebuildText(const AText: string);
    procedure CheckLinesFit(ALineWidth: integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Short text is returned unchanged as a single line. }
    procedure TestShortTextSingleLine;
    { Normal text is wrapped at word boundaries. }
    procedure TestWrapsAtSpaces;
    { Explicit line breaks still create separate lines. }
    procedure TestHonoursLineBreaks;
    { A word wider than the line is broken at the last position that fits. }
    procedure TestLongWordWithoutSpaces;
    { A long path after a few words: the original DocView hang. }
    procedure TestLongPathAfterWords;
    { A character wider than the line still produces one character per line. }
    procedure TestCharacterWiderThanLine;
  end;

implementation

uses
  fpg_dialogs;

{ TWrapMockFont }

constructor TWrapMockFont.Create(ACharWidth: integer);
begin
  inherited Create('Mock-10');
  FCharWidth := ACharWidth;
end;

function TWrapMockFont.GetAscent: integer;
begin
  Result := 8;
end;

function TWrapMockFont.GetDescent: integer;
begin
  Result := 2;
end;

function TWrapMockFont.GetHeight: integer;
begin
  Result := 10;
end;

function TWrapMockFont.GetTextWidth(const txt: string): integer;
begin
  Result := Length(txt) * FCharWidth;
end;

function TWrapMockFont.GetCanvasRef: TObject;
begin
  Result := nil;
end;

function TWrapMockFont.HandleIsValid: boolean;
begin
  Result := True;
end;

{ TTestWrapText }

procedure TTestWrapText.SetUp;
begin
  inherited SetUp;
  FFont := TWrapMockFont.Create(10);
  FLines := TStringList.Create;
end;

procedure TTestWrapText.TearDown;
begin
  FLines.Free;
  FFont.Free;  // test owned mock, not managed by the font manager
  inherited TearDown;
end;

procedure TTestWrapText.Wrap(const AText: string; ALineWidth: integer);
begin
  fpgWrapText(AText, FLines, FFont, ALineWidth, FWidth);
end;

procedure TTestWrapText.CheckLinesRebuildText(const AText: string);
var
  i: integer;
  s: string;
begin
  s := '';
  for i := 0 to FLines.Count-1 do
    s := s + FLines[i];
  AssertEquals('Concatenated lines must equal the original text', AText, s);
end;

procedure TTestWrapText.CheckLinesFit(ALineWidth: integer);
var
  i: integer;
begin
  for i := 0 to FLines.Count-1 do
  begin
    AssertTrue(Format('Line %d is empty', [i]), FLines[i] <> '');
    AssertTrue(Format('Line %d "%s" is wider than %d', [i, FLines[i], ALineWidth]),
        FFont.GetTextWidth(FLines[i]) <= ALineWidth);
  end;
end;

procedure TTestWrapText.TestShortTextSingleLine;
begin
  Wrap('hello world', 200);
  AssertEquals('line count', 1, FLines.Count);
  AssertEquals('hello world', FLines[0]);
  AssertEquals('width', 110, FWidth);
end;

procedure TTestWrapText.TestWrapsAtSpaces;
begin
  Wrap('one two three four', 90);
  AssertEquals('line count', 3, FLines.Count);
  AssertEquals('one two ', FLines[0]);
  AssertEquals('three ', FLines[1]);
  AssertEquals('four', FLines[2]);
end;

procedure TTestWrapText.TestHonoursLineBreaks;
begin
  Wrap('first' + LineEnding + LineEnding + 'second', 200);
  AssertEquals('line count', 3, FLines.Count);
  AssertEquals('first', FLines[0]);
  AssertEquals('', FLines[1]);
  AssertEquals('second', FLines[2]);
end;

procedure TTestWrapText.TestLongWordWithoutSpaces;
const
  cText = 'abcdefghijklmnopqrstuvwxyz0123456789ABCD'; // 40 characters
begin
  Wrap(cText, 100);
  AssertEquals('line count', 4, FLines.Count);
  AssertEquals('abcdefghij', FLines[0]);
  AssertEquals('456789ABCD', FLines[3]);
  CheckLinesFit(100);
  CheckLinesRebuildText(cText);
  AssertEquals('width', 100, FWidth);
end;

procedure TTestWrapText.TestLongPathAfterWords;
var
  lText: string;
begin
  lText := 'Failed to find the help file: /tmp/' + StringOfChar('a', 70) + '.inf';
  Wrap(lText, 200);
  AssertEquals('first line breaks at a space', 'Failed to find the ', FLines[0]);
  CheckLinesFit(200);
  CheckLinesRebuildText(lText);
end;

procedure TTestWrapText.TestCharacterWiderThanLine;
begin
  Wrap('abc', 5);
  AssertEquals('line count', 3, FLines.Count);
  AssertEquals('a', FLines[0]);
  AssertEquals('b', FLines[1]);
  AssertEquals('c', FLines[2]);
  CheckLinesRebuildText('abc');
end;

initialization
  RegisterTest(TTestWrapText);

end.
