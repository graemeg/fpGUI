program test_ft1;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Classes,
  // AggPas units
  agg_basics,
  agg_color,
  agg_pixfmt,
  agg_pixfmt_rgb,
  agg_pixfmt_rgba,
  agg_rendering_buffer,
  agg_renderer_base,
  agg_renderer_scanline,
  agg_rasterizer_scanline_aa,
  agg_scanline_u,
  agg_render_scanlines,
  agg_conv_curve,
  agg_font_pasfreetype,  // FreeType 1 Pascal adapter
  agg_font_cache_manager,
  // FPImage for PNG output
  FPImage,
  FPWritePNG;

const
  IMG_WIDTH = 800;
  IMG_HEIGHT = 600;
  DEFAULT_TEXT = 'The quick brown fox jumps over the lazy dog. 1234567890';
  DEFAULT_FONT_SIZE = 24.0;

type
  TRGBA32 = packed record
    b, g, r, a: Byte;
  end;
  PRGBA32 = ^TRGBA32;

var
  text_to_render: string;
  font_file: string;
  font_size: Double;
  output_file: string;

  // AggPas objects
  buffer: array of TRGBA32;
  rbuf: rendering_buffer;
  pixf: pixel_formats;
  rb: renderer_base;
  ren_solid: renderer_scanline_aa_solid;
  ras: rasterizer_scanline_aa;
  sl: scanline_u8;

  // Font objects
  feng: font_engine_freetype_int32;
  fman: font_cache_manager;
  curves: conv_curve;

  // Variables
  rgba_white, rgba_black: aggclr;
  x, y, text_width, text_height: Double;
  p: PChar;
  glyph: glyph_cache_ptr;
  i: Integer;

  // FPImage objects
  img: TFPMemoryImage;
  writer: TFPWriterPNG;
  src_pixel: PRGBA32;
  dst_pixel: TFPColor;

procedure ParseCommandLine;
begin
  // Default values
  text_to_render := DEFAULT_TEXT;
  font_size := DEFAULT_FONT_SIZE;
  font_file := '';
  output_file := 'output_ft1.png';

  // Parse arguments
  if ParamCount >= 1 then
    font_file := ParamStr(1);
  if ParamCount >= 2 then
  begin
    try
      font_size := StrToFloat(ParamStr(2));
    except
      font_size := DEFAULT_FONT_SIZE;
    end;
  end;
  if ParamCount >= 3 then
    text_to_render := ParamStr(3);
  if ParamCount >= 4 then
    output_file := ParamStr(4);

  // Validate
  if font_file = '' then
  begin
    WriteLn('Usage: test_ft1 <font_file.ttf> [font_size] [text] [output_file]');
    WriteLn('  font_file   - Path to TrueType font file (required)');
    WriteLn('  font_size   - Font size in points (default: ', DEFAULT_FONT_SIZE:0:1, ')');
    WriteLn('  text        - Text to render (default: "', DEFAULT_TEXT, '")');
    WriteLn('  output_file - Output PNG file (default: output_ft1.png)');
    WriteLn;
    WriteLn('Example: test_ft1 /usr/share/fonts/truetype/dejavu/DejaVuSans.ttf 24 "Hello World"');
    Halt(1);
  end;

  if not FileExists(font_file) then
  begin
    WriteLn('Error: Font file not found: ', font_file);
    Halt(1);
  end;
end;

procedure InitializeAggPas;
begin
  // Allocate buffer (BGRA format)
  SetLength(buffer, IMG_WIDTH * IMG_HEIGHT);

  // Initialize rendering buffer
  rbuf.Construct;
  rbuf.attach(@buffer[0], IMG_WIDTH, IMG_HEIGHT, IMG_WIDTH * 4);

  // Initialize pixel format (BGRA32)
  pixfmt_bgra32(pixf, @rbuf);

  // Initialize renderer base
  rb.Construct(@pixf);

  // Clear to white
  rgba_white.ConstrDbl(1.0, 1.0, 1.0, 1.0);
  rb.clear(@rgba_white);

  // Initialize solid renderer
  ren_solid.Construct(@rb);

  // Initialize rasterizer and scanline
  ras.Construct;
  sl.Construct;

  // Initialize font engine (FreeType 1 Pascal)
  feng.Construct;
  fman.Construct(@feng);
  curves.Construct(fman.path_adaptor);
  curves.approximation_scale_(2.0);

  WriteLn('Using FreeType 1 Pascal implementation');
end;

procedure LoadAndRenderText;
var
  start_x, start_y: Double;
  line_height: Double;
begin
  // Load font
  if not feng.load_font(PChar(font_file), 0, glyph_ren_outline) then
  begin
    WriteLn('Error: Failed to load font: ', font_file);
    Halt(1);
  end;

  feng.height_(font_size);
  feng.width_(font_size);
  feng.hinting_(True);
  feng.flip_y_(True);

  WriteLn('Font loaded: ', font_file);
  WriteLn('Font size: ', font_size:0:1);
  WriteLn('Text: ', text_to_render);

  // Measure text width
  text_width := 0;
  y := 0;
  p := PChar(text_to_render);
  while p^ <> #0 do
  begin
    glyph := fman.glyph(Byte(p^));
    if glyph <> nil then
    begin
      text_width := text_width + glyph^.advance_x;
      if p[1] <> #0 then
        fman.add_kerning(@text_width, @y);  // Apply kerning for next char
    end;
    Inc(p);
  end;

  text_height := font_size;
  line_height := font_size * 1.2;

  // Center text
  start_x := (IMG_WIDTH - text_width) / 2.0;
  start_y := (IMG_HEIGHT + text_height) / 2.0;

  WriteLn('Text width: ', text_width:0:1, ' pixels');
  WriteLn('Rendering at: (', start_x:0:1, ', ', start_y:0:1, ')');

  // Render text
  x := start_x;
  y := start_y;
  p := PChar(text_to_render);

  rgba_black.ConstrInt(0, 0, 0);
  ren_solid.color_(@rgba_black);

      while p^ <> #0 do

      begin

        glyph := fman.glyph(Byte(p^));

          if glyph <> nil then

          begin
      // Apply kerning
      fman.add_kerning(@x, @y);

      // Initialize glyph adaptors
      fman.init_embedded_adaptors(glyph, x, y);

      // Render outline glyph
      if glyph^.data_type = glyph_data_outline then
      begin
        ras.reset;
        ras.add_path(@curves);
        render_scanlines(@ras, @sl, @ren_solid);
      end;

      // Advance pen
      x := x + glyph^.advance_x;
      y := y + glyph^.advance_y;
    end;

    Inc(p);
  end;

  WriteLn('Text rendered successfully');
  WriteLn('LoadAndRenderText finished');
end;

procedure SaveToPNG;
var
  ix: Integer;
begin
  WriteLn('Saving to PNG: ', output_file);

  // Create FPImage
  img := TFPMemoryImage.Create(IMG_WIDTH, IMG_HEIGHT);
  try
    // Copy from AggPas buffer to FPImage (convert BGRA to RGBA)
    for i := 0 to IMG_HEIGHT - 1 do
    begin
      src_pixel := @buffer[i * IMG_WIDTH];
      for ix := 0 to IMG_WIDTH - 1 do
      begin
        // Convert BGRA to FPColor (16-bit per channel)
        dst_pixel.red   := src_pixel^.r shl 8 or src_pixel^.r;
        dst_pixel.green := src_pixel^.g shl 8 or src_pixel^.g;
        dst_pixel.blue  := src_pixel^.b shl 8 or src_pixel^.b;
        dst_pixel.alpha := src_pixel^.a shl 8 or src_pixel^.a;

        img.Colors[ix, i] := dst_pixel;
        Inc(src_pixel);
      end;
    end;

    // Write PNG
    writer := TFPWriterPNG.Create;
    try
      writer.Indexed := False;
      writer.UseAlpha := True;
      img.SaveToFile(output_file, writer);
      WriteLn('PNG saved successfully: ', output_file);
    finally
      writer.Free;
    end;
  finally
    img.Free;
  end;
end;



begin
  try
    WriteLn('FreeType 1 Pascal Font Rendering Test');
    WriteLn('======================================');
    WriteLn;

    ParseCommandLine;
    InitializeAggPas;
    LoadAndRenderText;
    SaveToPNG;

    WriteLn;
    WriteLn('Done! Compare this output with FreeType 2 output.');

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      Halt(1);
    end;
  end;
end.
