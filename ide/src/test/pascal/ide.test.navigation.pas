{
    fpGUI IDE - Navigation Tests

    Tests for interface/implementation navigation using tokenised
    highlight data.
}
unit ide.test.navigation;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  ide.highlighter,
  ide.navigation;

type

  { TTestNavigation }

  TTestNavigation = class(TTestCase)
  private
    FHL: TPascalHighlighter;
    FLines: TStringList;
    procedure SetSource(const ASource: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetSectionAtLine_Interface;
    procedure TestGetSectionAtLine_Implementation;
    procedure TestGetSectionAtLine_BeforeInterface;
    procedure TestNavigateFromImplToIntf_StandaloneProc;
    procedure TestNavigateFromIntfToImpl_StandaloneProc;
    procedure TestNavigateFromImplToIntf_ClassMethod;
    procedure TestNavigateFromIntfToImpl_ClassMethod;
    procedure TestNavigateFromImplBody_FindsEnclosingMethod;
    procedure TestToggle_FromInterface;
    procedure TestToggle_FromImplementation;
    procedure TestNavigateFromImpl_NoEnclosingMethod;
    procedure TestNoSections_ReturnsFalse;
  end;


implementation

const
  cSimpleUnit =
    'unit TestUnit;'          + LineEnding +   // 0
    ''                        + LineEnding +   // 1
    '{$mode objfpc}{$H+}'    + LineEnding +   // 2
    ''                        + LineEnding +   // 3
    'interface'               + LineEnding +   // 4
    ''                        + LineEnding +   // 5
    'procedure DoSomething;'  + LineEnding +   // 6
    'function GetValue: Integer;' + LineEnding + // 7
    ''                        + LineEnding +   // 8
    'implementation'          + LineEnding +   // 9
    ''                        + LineEnding +   // 10
    'procedure DoSomething;'  + LineEnding +   // 11
    'begin'                   + LineEnding +   // 12
    '  WriteLn;'              + LineEnding +   // 13
    'end;'                    + LineEnding +   // 14
    ''                        + LineEnding +   // 15
    'function GetValue: Integer;' + LineEnding + // 16
    'begin'                   + LineEnding +   // 17
    '  Result := 42;'         + LineEnding +   // 18
    'end;'                    + LineEnding +   // 19
    ''                        + LineEnding +   // 20
    'end.';                                      // 21

  cClassUnit =
    'unit ClassUnit;'           + LineEnding +   // 0
    ''                          + LineEnding +   // 1
    '{$mode objfpc}{$H+}'      + LineEnding +   // 2
    ''                          + LineEnding +   // 3
    'interface'                 + LineEnding +   // 4
    ''                          + LineEnding +   // 5
    'type'                      + LineEnding +   // 6
    '  TMyClass = class'        + LineEnding +   // 7
    '    procedure DoWork;'     + LineEnding +   // 8
    '    function GetName: string;' + LineEnding + // 9
    '  end;'                    + LineEnding +   // 10
    ''                          + LineEnding +   // 11
    'implementation'            + LineEnding +   // 12
    ''                          + LineEnding +   // 13
    'procedure TMyClass.DoWork;' + LineEnding +  // 14
    'var'                       + LineEnding +   // 15
    '  i: Integer;'             + LineEnding +   // 16
    'begin'                     + LineEnding +   // 17
    '  for i := 0 to 9 do'     + LineEnding +   // 18
    '    WriteLn(i);'           + LineEnding +   // 19
    'end;'                      + LineEnding +   // 20
    ''                          + LineEnding +   // 21
    'function TMyClass.GetName: string;' + LineEnding + // 22
    'begin'                     + LineEnding +   // 23
    '  Result := ''test'';'    + LineEnding +   // 24
    'end;'                      + LineEnding +   // 25
    ''                          + LineEnding +   // 26
    'end.';                                        // 27


{ TTestNavigation }

procedure TTestNavigation.SetSource(const ASource: string);
begin
  FLines.Text := ASource;
  FHL.Tokenise(ASource);
end;

procedure TTestNavigation.SetUp;
begin
  FHL := TPascalHighlighter.Create;
  FLines := TStringList.Create;
end;

procedure TTestNavigation.TearDown;
begin
  FLines.Free;
  FHL.Free;
end;

procedure TTestNavigation.TestGetSectionAtLine_Interface;
begin
  SetSource(cSimpleUnit);
  CheckEquals(Ord(usInterface), Ord(GetSectionAtLine(FHL, FLines, 6)),
    'Line 6 should be in interface section');
end;

procedure TTestNavigation.TestGetSectionAtLine_Implementation;
begin
  SetSource(cSimpleUnit);
  CheckEquals(Ord(usImplementation), Ord(GetSectionAtLine(FHL, FLines, 11)),
    'Line 11 should be in implementation section');
end;

procedure TTestNavigation.TestGetSectionAtLine_BeforeInterface;
begin
  SetSource(cSimpleUnit);
  CheckEquals(Ord(usUnknown), Ord(GetSectionAtLine(FHL, FLines, 0)),
    'Line 0 should be usUnknown (before interface)');
end;

procedure TTestNavigation.TestNavigateFromImplToIntf_StandaloneProc;
var
  nav: TNavigationResult;
begin
  SetSource(cSimpleUnit);
  { Cursor on 'procedure DoSomething' in implementation (line 11) }
  nav := NavigateToInterface(FHL, FLines, 11);
  CheckTrue(nav.Found, 'Should find interface declaration');
  CheckEquals(6, nav.Line, 'Should jump to line 6 (interface declaration)');
end;

procedure TTestNavigation.TestNavigateFromIntfToImpl_StandaloneProc;
var
  nav: TNavigationResult;
begin
  SetSource(cSimpleUnit);
  { Cursor on 'procedure DoSomething' in interface (line 6) }
  nav := NavigateToImplementation(FHL, FLines, 6);
  CheckTrue(nav.Found, 'Should find implementation');
  CheckEquals(11, nav.Line, 'Should jump to line 11 (implementation)');
end;

procedure TTestNavigation.TestNavigateFromImplToIntf_ClassMethod;
var
  nav: TNavigationResult;
begin
  SetSource(cClassUnit);
  { Cursor on 'procedure TMyClass.DoWork' in implementation (line 14) }
  nav := NavigateToInterface(FHL, FLines, 14);
  CheckTrue(nav.Found, 'Should find interface declaration');
  CheckEquals(8, nav.Line, 'Should jump to line 8 (class method declaration)');
end;

procedure TTestNavigation.TestNavigateFromIntfToImpl_ClassMethod;
var
  nav: TNavigationResult;
begin
  SetSource(cClassUnit);
  { Cursor on 'procedure DoWork' inside class declaration (line 8) }
  nav := NavigateToImplementation(FHL, FLines, 8);
  CheckTrue(nav.Found, 'Should find implementation');
  CheckEquals(14, nav.Line, 'Should jump to line 14 (implementation)');
end;

procedure TTestNavigation.TestNavigateFromImplBody_FindsEnclosingMethod;
var
  nav: TNavigationResult;
begin
  SetSource(cClassUnit);
  { Cursor inside the method body at line 18 (for i := 0 to 9 do) }
  nav := NavigateToInterface(FHL, FLines, 18);
  CheckTrue(nav.Found, 'Should find interface declaration from method body');
  CheckEquals(8, nav.Line, 'Should jump to line 8 (class method declaration)');
end;

procedure TTestNavigation.TestToggle_FromInterface;
var
  nav: TNavigationResult;
begin
  SetSource(cSimpleUnit);
  nav := NavigateInterfaceImplementation(FHL, FLines, 7);
  CheckTrue(nav.Found, 'Toggle from interface should find implementation');
  CheckEquals(16, nav.Line, 'Should jump to GetValue implementation (line 16)');
end;

procedure TTestNavigation.TestToggle_FromImplementation;
var
  nav: TNavigationResult;
begin
  SetSource(cSimpleUnit);
  { Line 18 is inside GetValue body — toggle should jump to interface }
  nav := NavigateInterfaceImplementation(FHL, FLines, 18);
  CheckTrue(nav.Found, 'Toggle from implementation should find interface');
  CheckEquals(7, nav.Line, 'Should jump to GetValue interface declaration (line 7)');
  { Now from the interface line, toggle back to implementation }
  nav := NavigateInterfaceImplementation(FHL, FLines, 7);
  CheckTrue(nav.Found, 'Toggle from interface should find implementation');
  CheckEquals(16, nav.Line, 'Should jump to line 16 (implementation)');
end;

procedure TTestNavigation.TestNavigateFromImpl_NoEnclosingMethod;
var
  nav: TNavigationResult;
begin
  SetSource(cSimpleUnit);
  { Cursor on line 10 (empty line right after 'implementation') }
  nav := NavigateToInterface(FHL, FLines, 10);
  CheckTrue(nav.Found, 'Should still navigate');
  CheckEquals(4, nav.Line, 'Should jump to interface keyword (line 4)');
end;

procedure TTestNavigation.TestNoSections_ReturnsFalse;
var
  nav: TNavigationResult;
begin
  SetSource('program Test;' + LineEnding + 'begin' + LineEnding + 'end.');
  nav := NavigateInterfaceImplementation(FHL, FLines, 1);
  CheckFalse(nav.Found, 'Program without interface/implementation should return not found');
end;

initialization
  RegisterTest(TTestNavigation);

end.
