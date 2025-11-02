unit tcfontdefinition;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit, testutils, testregistry,
  fpg_base;

type
  { Test suite for TfpgFontDefinition parsing and normalization }
  TTestFontDefinition = class(TTestCase)
  published
    { Basic parsing tests }
    procedure TestParseSimpleDescriptor;
    procedure TestParseBoldDescriptor;
    procedure TestParseItalicDescriptor;
    procedure TestParseBoldItalicDescriptor;
    procedure TestParseUnderlineDescriptor;
    procedure TestParseMultipleAttributes;

    { Normalization tests }
    procedure TestNormalizationUpperCase;
    procedure TestNormalizationLowerCase;
    procedure TestNormalizationMixedCase;
    procedure TestNormalizationAttributeOrder;

    { Edge cases }
    procedure TestEmptyDescriptor;
    procedure TestDescriptorWithoutSize;
    procedure TestDescriptorWithSpaces;
    procedure TestDescriptorWithDuplicateAttributes;

    { Property access tests }
    procedure TestFaceNameExtraction;
    procedure TestSizeExtraction;
    procedure TestAttributeFlags;
  end;

procedure RegisterTests;

implementation

{ TTestFontDefinition }

procedure TTestFontDefinition.TestParseSimpleDescriptor;
var
  fontDef: TfpgFontDefinition;
begin
  fontDef := TfpgFontDefinition.Create('Arial-12');
  try
    CheckEquals('Arial', fontDef.FaceName, 'Face name should be Arial');
    CheckEquals(12, fontDef.Size, 'Size should be 12');
    CheckFalse(fpgFontBold in fontDef.Attributes, 'Should not be bold');
    CheckFalse(fpgFontItalic in fontDef.Attributes, 'Should not be italic');
    CheckFalse(fpgFontUnderline in fontDef.Attributes, 'Should not be underline');
  finally
    fontDef.Free;
  end;
end;

procedure TTestFontDefinition.TestParseBoldDescriptor;
var
  fontDef: TfpgFontDefinition;
begin
  fontDef := TfpgFontDefinition.Create('Arial-12:bold');
  try
    CheckEquals('Arial', fontDef.FaceName);
    CheckEquals(12, fontDef.Size);
    CheckTrue(fpgFontBold in fontDef.Attributes, 'Should be bold');
    CheckFalse(fpgFontItalic in fontDef.Attributes, 'Should not be italic');
  finally
    fontDef.Free;
  end;
end;

procedure TTestFontDefinition.TestParseItalicDescriptor;
var
  fontDef: TfpgFontDefinition;
begin
  fontDef := TfpgFontDefinition.Create('Arial-12:italic');
  try
    CheckFalse(fpgFontBold in fontDef.Attributes, 'Should not be bold');
    CheckTrue(fpgFontItalic in fontDef.Attributes, 'Should be italic');
  finally
    fontDef.Free;
  end;
end;

procedure TTestFontDefinition.TestParseBoldItalicDescriptor;
var
  fontDef: TfpgFontDefinition;
begin
  fontDef := TfpgFontDefinition.Create('Arial-12:bold:italic');
  try
    CheckTrue(fpgFontBold in fontDef.Attributes, 'Should be bold');
    CheckTrue(fpgFontItalic in fontDef.Attributes, 'Should be italic');
  finally
    fontDef.Free;
  end;
end;

procedure TTestFontDefinition.TestParseUnderlineDescriptor;
var
  fontDef: TfpgFontDefinition;
begin
  fontDef := TfpgFontDefinition.Create('Arial-12:underline');
  try
    CheckTrue(fpgFontUnderline in fontDef.Attributes, 'Should be underline');
  finally
    fontDef.Free;
  end;
end;

procedure TTestFontDefinition.TestParseMultipleAttributes;
var
  fontDef: TfpgFontDefinition;
begin
  fontDef := TfpgFontDefinition.Create('Liberation Sans-14:bold:italic:underline');
  try
    CheckEquals('Liberation Sans', fontDef.FaceName);
    CheckEquals(14, fontDef.Size);
    CheckTrue(fpgFontBold in fontDef.Attributes, 'Should be bold');
    CheckTrue(fpgFontItalic in fontDef.Attributes, 'Should be italic');
    CheckTrue(fpgFontUnderline in fontDef.Attributes, 'Should be underline');
  finally
    fontDef.Free;
  end;
end;

procedure TTestFontDefinition.TestNormalizationUpperCase;
var
  fontDef1, fontDef2: TfpgFontDefinition;
begin
  fontDef1 := TfpgFontDefinition.Create('Arial-12:bold');
  fontDef2 := TfpgFontDefinition.Create('ARIAL-12:BOLD');
  try
    CheckEquals(fontDef1.FontDesc, fontDef2.FontDesc,
      'Normalized descriptors should be identical');
  finally
    fontDef1.Free;
    fontDef2.Free;
  end;
end;

procedure TTestFontDefinition.TestNormalizationLowerCase;
var
  fontDef1, fontDef2: TfpgFontDefinition;
begin
  fontDef1 := TfpgFontDefinition.Create('Arial-12:bold');
  fontDef2 := TfpgFontDefinition.Create('arial-12:bold');
  try
    CheckEquals(fontDef1.FontDesc, fontDef2.FontDesc,
      'Normalized descriptors should be identical');
  finally
    fontDef1.Free;
    fontDef2.Free;
  end;
end;

procedure TTestFontDefinition.TestNormalizationMixedCase;
var
  fontDef1, fontDef2: TfpgFontDefinition;
begin
  fontDef1 := TfpgFontDefinition.Create('Liberation Sans-12:bold:italic');
  fontDef2 := TfpgFontDefinition.Create('Liberation SANS-12:BOLD:italic');
  try
    CheckEquals(fontDef1.FontDesc, fontDef2.FontDesc,
      'Mixed case should normalize to same descriptor');
  finally
    fontDef1.Free;
    fontDef2.Free;
  end;
end;

procedure TTestFontDefinition.TestNormalizationAttributeOrder;
var
  fontDef1, fontDef2: TfpgFontDefinition;
begin
  fontDef1 := TfpgFontDefinition.Create('Arial-12:bold:italic');
  fontDef2 := TfpgFontDefinition.Create('Arial-12:italic:bold');
  try
    CheckEquals(fontDef1.FontDesc, fontDef2.FontDesc,
      'Attribute order should normalize to canonical form');
  finally
    fontDef1.Free;
    fontDef2.Free;
  end;
end;

procedure TTestFontDefinition.TestEmptyDescriptor;
var
  fontDef: TfpgFontDefinition;
begin
  { Empty descriptor should use defaults }
  fontDef := TfpgFontDefinition.Create('');
  try
    CheckTrue(fontDef.FaceName <> '', 'Should have default face name');
    CheckTrue(fontDef.Size > 0, 'Should have default size');
  finally
    fontDef.Free;
  end;
end;

procedure TTestFontDefinition.TestDescriptorWithoutSize;
var
  fontDef: TfpgFontDefinition;
begin
  { Descriptor without size should use default }
  fontDef := TfpgFontDefinition.Create('Arial');
  try
    CheckEquals('Arial', fontDef.FaceName);
    CheckTrue(fontDef.Size > 0, 'Should have default size');
  finally
    fontDef.Free;
  end;
end;

procedure TTestFontDefinition.TestDescriptorWithSpaces;
var
  fontDef: TfpgFontDefinition;
begin
  { Font names with spaces should be handled correctly }
  fontDef := TfpgFontDefinition.Create('Liberation Sans-12:bold');
  try
    CheckEquals('Liberation Sans', fontDef.FaceName,
      'Face name with spaces should be preserved');
    CheckEquals(12, fontDef.Size);
    CheckTrue(fpgFontBold in fontDef.Attributes, 'Should be bold');
  finally
    fontDef.Free;
  end;
end;

procedure TTestFontDefinition.TestDescriptorWithDuplicateAttributes;
var
  fontDef: TfpgFontDefinition;
begin
  { Duplicate attributes should be deduplicated }
  fontDef := TfpgFontDefinition.Create('Arial-12:bold:bold:italic');
  try
    CheckTrue(fpgFontBold in fontDef.Attributes, 'Should be bold');
    CheckTrue(fpgFontItalic in fontDef.Attributes, 'Should be italic');
    { Normalized descriptor should not have duplicates }
    CheckTrue(Pos(':bold:bold:', fontDef.FontDesc) = 0,
      'Normalized descriptor should not contain duplicate :bold:');
  finally
    fontDef.Free;
  end;
end;

procedure TTestFontDefinition.TestFaceNameExtraction;
var
  fontDef: TfpgFontDefinition;
begin
  fontDef := TfpgFontDefinition.Create('Courier New-10:bold');
  try
    CheckEquals('Courier New', fontDef.FaceName,
      'Should extract face name with spaces correctly');
  finally
    fontDef.Free;
  end;
end;

procedure TTestFontDefinition.TestSizeExtraction;
var
  fontDef: TfpgFontDefinition;
begin
  fontDef := TfpgFontDefinition.Create('Arial-18');
  try
    CheckEquals(18, fontDef.Size, 'Should extract size correctly');
  finally
    fontDef.Free;
  end;
end;

procedure TTestFontDefinition.TestAttributeFlags;
var
  fontDef: TfpgFontDefinition;
begin
  fontDef := TfpgFontDefinition.Create('Arial-12:bold:italic:underline');
  try
    CheckTrue(fpgFontBold in fontDef.Attributes, 'Bold flag should be set');
    CheckTrue(fpgFontItalic in fontDef.Attributes, 'Italic flag should be set');
    CheckTrue(fpgFontUnderline in fontDef.Attributes, 'Underline flag should be set');
  finally
    fontDef.Free;
  end;
end;

procedure RegisterTests;
begin
  RegisterTest(TTestFontDefinition);
end;

initialization
  RegisterTests;

end.
