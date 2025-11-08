program test_adapter;

{$mode objfpc}{$H+}

uses
  SysUtils,
  agg_basics,
  agg_font_freetype_pas;

var
  ft_library: FT_Library_ptr;
  face: FT_Face_ptr;
  test_font: string;
  err: FT_Error;
  char_index: FT_UInt;
  kerning: FT_Vector;
  test_passed: Integer;
  test_failed: Integer;

procedure TestPassed(const test_name: string);
begin
  WriteLn('[PASS] ', test_name);
  Inc(test_passed);
end;

procedure TestFailed(const test_name, reason: string);
begin
  WriteLn('[FAIL] ', test_name, ': ', reason);
  Inc(test_failed);
end;

procedure TestInitLibrary;
begin
  err := FT_Init_FreeType(ft_library);
  if err = 0 then
    TestPassed('FT_Init_FreeType')
  else
    TestFailed('FT_Init_FreeType', Format('Error code: %d', [err]));
end;

procedure TestLoadFace;
begin
  err := FT_New_Face(ft_library, PChar(test_font), 0, face);
  if err = 0 then
  begin
    TestPassed('FT_New_Face');

    // Check basic face properties
    if face^.num_glyphs > 0 then
      TestPassed('Face has glyphs (num_glyphs > 0)')
    else
      TestFailed('Face has glyphs', Format('num_glyphs = %d', [face^.num_glyphs]));

    if face^.family_name <> nil then
      WriteLn('  Font family: ', face^.family_name)
    else
      TestFailed('Face has family_name', 'family_name is nil');
  end
  else
    TestFailed('FT_New_Face', Format('Error code: %d', [err]));
end;

procedure TestCharIndex;
var
  test_chars: array[0..5] of Char = ('A', 'B', 'a', 'b', '0', '1');
  i: Integer;
  idx: FT_UInt;
begin
  for i := 0 to High(test_chars) do
  begin
    idx := FT_Get_Char_Index(face, Ord(test_chars[i]));
    if idx > 0 then
      WriteLn(Format('  Char ''%s'' -> glyph index %d', [test_chars[i], idx]))
    else
      TestFailed(Format('FT_Get_Char_Index(''%s'')', [test_chars[i]]),
                 'Returned 0 (should be > 0 for ASCII chars)');
  end;
  TestPassed('FT_Get_Char_Index for ASCII characters');
end;

procedure TestSetCharSize;
const
  TEST_SIZE = 12 * 64; // 12pt in 26.6 fixed-point
begin
  err := FT_Set_Char_Size(face, TEST_SIZE, TEST_SIZE, 72, 72);
  if err = 0 then
    TestPassed('FT_Set_Char_Size (12pt @ 72dpi)')
  else
    TestFailed('FT_Set_Char_Size', Format('Error code: %d', [err]));
end;

procedure TestLoadGlyph;
var
  glyph_index: FT_UInt;
begin
  // Load glyph for 'A'
  glyph_index := FT_Get_Char_Index(face, Ord('A'));

  if glyph_index = 0 then
  begin
    TestFailed('TestLoadGlyph', 'Could not get glyph index for ''A''');
    Exit;
  end;

  err := FT_Load_Glyph(face, glyph_index, FT_LOAD_NO_SCALE);
  if err <> 0 then
  begin
    TestFailed('FT_Load_Glyph', Format('Error code: %d', [err]));
    Exit;
  end;

  TestPassed('FT_Load_Glyph for ''A''');

  // Verify outline data
  WriteLn(Format('  Outline: %d points, %d contours',
    [face^.glyph^.outline.n_points, face^.glyph^.outline.n_contours]));

  if face^.glyph^.outline.n_points > 0 then
    TestPassed('Glyph outline has points')
  else
    TestFailed('Glyph outline has points',
               Format('n_points = %d', [face^.glyph^.outline.n_points]));

  if face^.glyph^.outline.n_contours > 0 then
    TestPassed('Glyph outline has contours')
  else
    TestFailed('Glyph outline has contours',
               Format('n_contours = %d', [face^.glyph^.outline.n_contours]));

  // Verify arrays are allocated
  if Length(face^.glyph^.outline.points) = face^.glyph^.outline.n_points then
    TestPassed('Points array correctly sized')
  else
    TestFailed('Points array correctly sized',
               Format('Expected %d, got %d',
                 [face^.glyph^.outline.n_points, Length(face^.glyph^.outline.points)]));

  if Length(face^.glyph^.outline.tags) = face^.glyph^.outline.n_points then
    TestPassed('Tags array correctly sized')
  else
    TestFailed('Tags array correctly sized',
               Format('Expected %d, got %d',
                 [face^.glyph^.outline.n_points, Length(face^.glyph^.outline.tags)]));

  if Length(face^.glyph^.outline.contours) = face^.glyph^.outline.n_contours then
    TestPassed('Contours array correctly sized')
  else
    TestFailed('Contours array correctly sized',
               Format('Expected %d, got %d',
                 [face^.glyph^.outline.n_contours, Length(face^.glyph^.outline.contours)]));

  // Display first few points for debugging
  if face^.glyph^.outline.n_points > 0 then
  begin
    WriteLn('  First 5 points:');
    for var i := 0 to Min(4, face^.glyph^.outline.n_points - 1) do
      WriteLn(Format('    Point %d: (%d, %d) tag=%d', [
        i,
        face^.glyph^.outline.points[i].x,
        face^.glyph^.outline.points[i].y,
        Ord(face^.glyph^.outline.tags[i])
      ]));
  end;
end;

procedure TestKerning;
var
  left_idx, right_idx: FT_UInt;
begin
  // Test kerning between 'A' and 'V' (common kerning pair)
  left_idx := FT_Get_Char_Index(face, Ord('A'));
  right_idx := FT_Get_Char_Index(face, Ord('V'));

  if (left_idx = 0) or (right_idx = 0) then
  begin
    WriteLn('  [SKIP] TestKerning: Could not get glyph indices');
    Exit;
  end;

  err := FT_Get_Kerning(face, left_idx, right_idx, FT_KERNING_DEFAULT, @kerning);
  if err = 0 then
  begin
    WriteLn(Format('  Kerning between ''A'' and ''V'': (%d, %d)',
      [kerning.x, kerning.y]));
    TestPassed('FT_Get_Kerning');
  end
  else
    TestFailed('FT_Get_Kerning', Format('Error code: %d', [err]));
end;

procedure TestMetrics;
var
  glyph_index: FT_UInt;
begin
  // Set a specific size first
  FT_Set_Char_Size(face, 12 * 64, 12 * 64, 72, 72);

  // Load glyph for 'M' (typically one of the widest characters)
  glyph_index := FT_Get_Char_Index(face, Ord('M'));

  if glyph_index = 0 then
  begin
    WriteLn('  [SKIP] TestMetrics: Could not get glyph index for ''M''');
    Exit;
  end;

  err := FT_Load_Glyph(face, glyph_index, FT_LOAD_DEFAULT);
  if err <> 0 then
  begin
    TestFailed('TestMetrics - FT_Load_Glyph', Format('Error code: %d', [err]));
    Exit;
  end;

  WriteLn(Format('  Glyph ''M'' metrics:', []));
  WriteLn(Format('    Width: %d', [face^.glyph^.metrics.width]));
  WriteLn(Format('    Height: %d', [face^.glyph^.metrics.height]));
  WriteLn(Format('    horiBearingX: %d', [face^.glyph^.metrics.horiBearingX]));
  WriteLn(Format('    horiBearingY: %d', [face^.glyph^.metrics.horiBearingY]));
  WriteLn(Format('    horiAdvance: %d', [face^.glyph^.metrics.horiAdvance]));

  if face^.glyph^.metrics.horiAdvance > 0 then
    TestPassed('Glyph metrics have positive advance')
  else
    TestFailed('Glyph metrics have positive advance',
               Format('horiAdvance = %d', [face^.glyph^.metrics.horiAdvance]));
end;

procedure TestCleanup;
begin
  err := FT_Done_Face(face);
  if err = 0 then
    TestPassed('FT_Done_Face')
  else
    TestFailed('FT_Done_Face', Format('Error code: %d', [err]));

  err := FT_Done_FreeType(ft_library);
  if err = 0 then
    TestPassed('FT_Done_FreeType')
  else
    TestFailed('FT_Done_FreeType', Format('Error code: %d', [err]));
end;

begin
  test_passed := 0;
  test_failed := 0;

  WriteLn('FreeType 1 Pascal Adapter Unit Tests');
  WriteLn('=====================================');
  WriteLn;

  // Get font file from command line or use default
  if ParamCount >= 1 then
    test_font := ParamStr(1)
  else
    test_font := '/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf';

  if not FileExists(test_font) then
  begin
    WriteLn('Error: Font file not found: ', test_font);
    WriteLn('Usage: test_adapter [font_file.ttf]');
    Halt(1);
  end;

  WriteLn('Testing with font: ', test_font);
  WriteLn;

  try
    TestInitLibrary;
    TestLoadFace;
    TestCharIndex;
    TestSetCharSize;
    TestLoadGlyph;
    TestKerning;
    TestMetrics;
    TestCleanup;

    WriteLn;
    WriteLn('=====================================');
    WriteLn(Format('Tests passed: %d', [test_passed]));
    WriteLn(Format('Tests failed: %d', [test_failed]));
    WriteLn;

    if test_failed > 0 then
    begin
      WriteLn('SOME TESTS FAILED!');
      Halt(1);
    end
    else
    begin
      WriteLn('ALL TESTS PASSED!');
      Halt(0);
    end;

  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('EXCEPTION: ', E.Message);
      Halt(1);
    end;
  end;
end.
