{
    fpGUI IDE - Editor Tab Tests

    Tests for pure functions in ide.editor.tabs: highlight kind detection
    from file extensions.
}
unit ide.test.editor.tabs;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  ide.editor.tabs;

type

  { TTestHighlightKindForExtension }

  TTestHighlightKindForExtension = class(TTestCase)
  published
    procedure TestPascalExtensions;
    procedure TestPatchExtensions;
    procedure TestINIExtensions;
    procedure TestXMLExtensions;
    procedure TestUnknownExtension;
    procedure TestEmptyExtension;
  end;

implementation

procedure TTestHighlightKindForExtension.TestPascalExtensions;
begin
  AssertTrue('.pas', HighlightKindForExtension('.pas') = hkPascal);
  AssertTrue('.pp', HighlightKindForExtension('.pp') = hkPascal);
  AssertTrue('.inc', HighlightKindForExtension('.inc') = hkPascal);
  AssertTrue('.lpr', HighlightKindForExtension('.lpr') = hkPascal);
  AssertTrue('.dpr', HighlightKindForExtension('.dpr') = hkPascal);
end;

procedure TTestHighlightKindForExtension.TestPatchExtensions;
begin
  AssertTrue('.patch', HighlightKindForExtension('.patch') = hkPatch);
  AssertTrue('.diff', HighlightKindForExtension('.diff') = hkPatch);
end;

procedure TTestHighlightKindForExtension.TestINIExtensions;
begin
  AssertTrue('.ini', HighlightKindForExtension('.ini') = hkINI);
  AssertTrue('.cfg', HighlightKindForExtension('.cfg') = hkINI);
  AssertTrue('.conf', HighlightKindForExtension('.conf') = hkINI);
end;

procedure TTestHighlightKindForExtension.TestXMLExtensions;
begin
  AssertTrue('.xml', HighlightKindForExtension('.xml') = hkXML);
  AssertTrue('.html', HighlightKindForExtension('.html') = hkXML);
  AssertTrue('.htm', HighlightKindForExtension('.htm') = hkXML);
  AssertTrue('.xhtml', HighlightKindForExtension('.xhtml') = hkXML);
  AssertTrue('.svg', HighlightKindForExtension('.svg') = hkXML);
  AssertTrue('.xsd', HighlightKindForExtension('.xsd') = hkXML);
  AssertTrue('.xsl', HighlightKindForExtension('.xsl') = hkXML);
  AssertTrue('.xslt', HighlightKindForExtension('.xslt') = hkXML);
  AssertTrue('.lpi', HighlightKindForExtension('.lpi') = hkXML);
  AssertTrue('.lpk', HighlightKindForExtension('.lpk') = hkXML);
end;

procedure TTestHighlightKindForExtension.TestUnknownExtension;
begin
  AssertTrue('.txt', HighlightKindForExtension('.txt') = hkNone);
  AssertTrue('.md', HighlightKindForExtension('.md') = hkNone);
  AssertTrue('.log', HighlightKindForExtension('.log') = hkNone);
end;

procedure TTestHighlightKindForExtension.TestEmptyExtension;
begin
  AssertTrue('empty', HighlightKindForExtension('') = hkNone);
end;

initialization
  RegisterTest(TTestHighlightKindForExtension);

end.
