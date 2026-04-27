{
    fpGUI IDE - Call Stack Panel Logic Tests

    Tests for the pure-logic functions in ide.callstack:
      - ParseCallStackFrame
      - ParseCallStack
      - CallStackFrameDisplay
}
unit ide.test.callstack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpcunit, testregistry,
  ide.callstack;

type

  { TTestParseCallStackFrame }

  TTestParseCallStackFrame = class(TTestCase)
  private
    procedure AssertFrame(const AText: string;
        AExpectedIndex: Integer;
        const AExpectedFunc, AExpectedFile: string;
        AExpectedLine: Integer;
        AExpectedHasSource: Boolean);
  published
    { Empty / degenerate input }
    procedure TestEmptyString;
    procedure TestNoHash;
    procedure TestHashOnly;

    { Full format: #N FuncName at file.pas:line (0xADDR) }
    procedure TestFrame0WithSource;
    procedure TestFrame1WithSource;
    procedure TestHighFrameIndex;
    procedure TestLongAddress;

    { Function known, no source: #N FuncName (0xADDR) }
    procedure TestFrameNoSource;
    procedure TestFrameUnknown;

    { Edge cases }
    procedure TestFuncNameWithSpaces;
    procedure TestFileNameWithColon;    { e.g. Windows path C:\foo.pas }
  end;

  { TTestParseCallStack }

  TTestParseCallStack = class(TTestCase)
  published
    procedure TestEmptyArray;
    procedure TestSingleFrame;
    procedure TestMultipleFrames;
    procedure TestFrameCountMatchesInput;
  end;

  { TTestCallStackFrameDisplay }

  TTestCallStackFrameDisplay = class(TTestCase)
  private
    function MakeFrame(AIndex: Integer; const AFunc, AFile: string;
        ALine: Integer; AAddr: QWord): TCallStackFrame;
  published
    procedure TestDisplayWithSource;
    procedure TestDisplayAddressOnly;
    procedure TestDisplayUnknownNoAddress;
    procedure TestDisplayZeroAddress;
    procedure TestDisplayHighFrameIndex;
  end;

implementation

{ ---------------------------------------------------------------------------
  TTestParseCallStackFrame
  --------------------------------------------------------------------------- }

procedure TTestParseCallStackFrame.AssertFrame(const AText: string;
    AExpectedIndex: Integer;
    const AExpectedFunc, AExpectedFile: string;
    AExpectedLine: Integer;
    AExpectedHasSource: Boolean);
var
  F: TCallStackFrame;
begin
  F := ParseCallStackFrame(AText);
  AssertEquals('Index',     AExpectedIndex,     F.Index);
  AssertEquals('FuncName',  AExpectedFunc,       F.FuncName);
  AssertEquals('FileName',  AExpectedFile,       F.FileName);
  AssertEquals('LineNumber', AExpectedLine,      F.LineNumber);
  AssertEquals('HasSource', AExpectedHasSource,  F.HasSource);
end;

procedure TTestParseCallStackFrame.TestEmptyString;
var
  F: TCallStackFrame;
begin
  F := ParseCallStackFrame('');
  AssertEquals('FuncName on empty', '', F.FuncName);
  AssertEquals('Index on empty', 0, F.Index);
  AssertFalse('HasSource on empty', F.HasSource);
end;

procedure TTestParseCallStackFrame.TestNoHash;
var
  F: TCallStackFrame;
begin
  F := ParseCallStackFrame('0 MyProc (0x0000000000000001)');
  { No leading '#' — falls back to AText as FuncName }
  AssertFalse('HasSource', F.HasSource);
end;

procedure TTestParseCallStackFrame.TestHashOnly;
var
  F: TCallStackFrame;
begin
  F := ParseCallStackFrame('#');
  AssertFalse('HasSource', F.HasSource);
end;

procedure TTestParseCallStackFrame.TestFrame0WithSource;
begin
  AssertFrame(
    '#0 MyProc at myfile.pas:42 (0x00007F1234ABCDEF)',
    0, 'MyProc', 'myfile.pas', 42, True
  );
end;

procedure TTestParseCallStackFrame.TestFrame1WithSource;
begin
  AssertFrame(
    '#1 CallerProc at caller.pas:30 (0x00007F0000001234)',
    1, 'CallerProc', 'caller.pas', 30, True
  );
end;

procedure TTestParseCallStackFrame.TestHighFrameIndex;
begin
  AssertFrame(
    '#15 DeepProc at deep.pas:1 (0x0000000000000001)',
    15, 'DeepProc', 'deep.pas', 1, True
  );
end;

procedure TTestParseCallStackFrame.TestLongAddress;
var
  F: TCallStackFrame;
begin
  F := ParseCallStackFrame('#0 MyProc at foo.pas:10 (0x00007FFFFFFFFFFF)');
  AssertEquals('Address', QWord($00007FFFFFFFFFFF), F.Address);
end;

procedure TTestParseCallStackFrame.TestFrameNoSource;
begin
  AssertFrame(
    '#2 LibProc (0x00007F9900000000)',
    2, 'LibProc', '', 0, False
  );
end;

procedure TTestParseCallStackFrame.TestFrameUnknown;
begin
  AssertFrame(
    '#3 <unknown> (0x00007F1122334455)',
    3, '<unknown>', '', 0, False
  );
end;

procedure TTestParseCallStackFrame.TestFuncNameWithSpaces;
begin
  { FPC-generated names can contain spaces in some edge cases;
    the ' at ' separator must still be found correctly. }
  AssertFrame(
    '#0 TMyClass.MyProc at myunit.pas:100 (0x0000000000000001)',
    0, 'TMyClass.MyProc', 'myunit.pas', 100, True
  );
end;

procedure TTestParseCallStackFrame.TestFileNameWithColon;
var
  F: TCallStackFrame;
begin
  { On Windows paths contain 'C:\' — the last ':' must be used to split. }
  F := ParseCallStackFrame('#0 WinProc at C:\src\win.pas:55 (0x0000000000000001)');
  AssertEquals('FileName', 'C:\src\win.pas', F.FileName);
  AssertEquals('LineNumber', 55, F.LineNumber);
  AssertTrue('HasSource', F.HasSource);
end;

{ ---------------------------------------------------------------------------
  TTestParseCallStack
  --------------------------------------------------------------------------- }

procedure TTestParseCallStack.TestEmptyArray;
var
  Frames: TCallStackFrameArray;
begin
  Frames := ParseCallStack([]);
  AssertEquals('Empty array length', 0, Length(Frames));
end;

procedure TTestParseCallStack.TestSingleFrame;
var
  Frames: TCallStackFrameArray;
begin
  Frames := ParseCallStack(['#0 MyProc at foo.pas:1 (0x0000000000000001)']);
  AssertEquals('Length', 1, Length(Frames));
  AssertEquals('FuncName', 'MyProc', Frames[0].FuncName);
end;

procedure TTestParseCallStack.TestMultipleFrames;
var
  Frames: TCallStackFrameArray;
begin
  Frames := ParseCallStack([
    '#0 Inner at a.pas:10 (0x0000000000000001)',
    '#1 Middle at b.pas:20 (0x0000000000000002)',
    '#2 Outer at c.pas:30 (0x0000000000000003)'
  ]);
  AssertEquals('Length', 3, Length(Frames));
  AssertEquals('Frame 0 func', 'Inner',  Frames[0].FuncName);
  AssertEquals('Frame 1 func', 'Middle', Frames[1].FuncName);
  AssertEquals('Frame 2 func', 'Outer',  Frames[2].FuncName);
  AssertEquals('Frame 2 line', 30, Frames[2].LineNumber);
end;

procedure TTestParseCallStack.TestFrameCountMatchesInput;
var
  Frames: TCallStackFrameArray;
begin
  Frames := ParseCallStack([
    '#0 A (0x0000000000000001)',
    '#1 B (0x0000000000000002)',
    '#2 C (0x0000000000000003)',
    '#3 D (0x0000000000000004)',
    '#4 E (0x0000000000000005)'
  ]);
  AssertEquals('Count', 5, Length(Frames));
end;

{ ---------------------------------------------------------------------------
  TTestCallStackFrameDisplay
  --------------------------------------------------------------------------- }

function TTestCallStackFrameDisplay.MakeFrame(AIndex: Integer;
    const AFunc, AFile: string; ALine: Integer; AAddr: QWord): TCallStackFrame;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Index      := AIndex;
  Result.FuncName   := AFunc;
  Result.FileName   := AFile;
  Result.LineNumber := ALine;
  Result.Address    := AAddr;
  Result.HasSource  := (AFile <> '') and (ALine > 0);
end;

procedure TTestCallStackFrameDisplay.TestDisplayWithSource;
var
  F: TCallStackFrame;
begin
  F := MakeFrame(0, 'MyProc', 'myfile.pas', 42, QWord($00007F1234ABCDEF));
  AssertEquals(
    '#0  MyProc  (myfile.pas:42)',
    CallStackFrameDisplay(F)
  );
end;

procedure TTestCallStackFrameDisplay.TestDisplayAddressOnly;
var
  F: TCallStackFrame;
begin
  F := MakeFrame(2, 'LibProc', '', 0, QWord($00007F9900000000));
  AssertEquals(
    '#2  LibProc  (0x00007F9900000000)',
    CallStackFrameDisplay(F)
  );
end;

procedure TTestCallStackFrameDisplay.TestDisplayUnknownNoAddress;
var
  F: TCallStackFrame;
begin
  { Address = 0 and no source — display just index and name }
  F := MakeFrame(5, '<unknown>', '', 0, 0);
  AssertEquals(
    '#5  <unknown>',
    CallStackFrameDisplay(F)
  );
end;

procedure TTestCallStackFrameDisplay.TestDisplayZeroAddress;
var
  F: TCallStackFrame;
begin
  { A frame with HasSource=False and Address=0 shows no location suffix }
  F := MakeFrame(1, 'SomeFunc', '', 0, 0);
  AssertEquals('#1  SomeFunc', CallStackFrameDisplay(F));
end;

procedure TTestCallStackFrameDisplay.TestDisplayHighFrameIndex;
var
  F: TCallStackFrame;
begin
  F := MakeFrame(15, 'DeepProc', 'deep.pas', 1, QWord($0000000000000001));
  AssertEquals(
    '#15  DeepProc  (deep.pas:1)',
    CallStackFrameDisplay(F)
  );
end;

initialization

  RegisterTest(TTestParseCallStackFrame);
  RegisterTest(TTestParseCallStack);
  RegisterTest(TTestCallStackFrameDisplay);

end.
