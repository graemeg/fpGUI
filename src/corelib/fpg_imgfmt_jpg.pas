{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      JPEG format image parser
}


unit fpg_imgfmt_jpg;

{$mode objfpc}{$H+}


interface

uses
  Classes,
  SysUtils,
  fpg_main,
  fpg_base,
  fpg_dialogs;
  
type 
  EJPEG = class(Exception);
  
procedure ReadImage_JPG(img: TfpgImage; bmp: TStream; const AScale: integer = 1);
function  LoadImage_JPG(const AFileName: String; const AScale: integer = 1): TfpgImage;
  
implementation
uses
  {PASJPG10 library}
  jmorecfg,
  jpeglib,
  jerror,
  jdeferr,
  jdmarker,
  jdmaster,
  jdapimin,
  jdapistd;

type
	
  my_src_ptr = ^my_source_mgr;
  my_source_mgr = record
    pub    : jpeg_source_mgr;   {public fields}
    infile : TStream;           {source stream}
    buffer : JOCTET_FIELD_PTR;  {start of buffer}
    start_of_file : boolean;    {have we gotten any data yet?}
  end;

//  my_error_ptr = ^my_error_mgr;
  my_error_mgr = record
    pub: jpeg_error_mgr;
  end;

  bmp_dest_ptr = ^bmp_dest_struct;
  bmp_dest_struct = record
    {image info}
    data_width : JDIMENSION;        {JSAMPLEs per row}
    row_width : JDIMENSION;         {physical width of one row in the BMP file}
    pad_bytes : INT;                {number of padding bytes needed per row}
    grayscale : boolean;            {grayscale or quantized color table ?}
    {pixelrow buffer}
    buffer : JSAMPARRAY;            {pixelrow buffer}
    buffer_height : JDIMENSION;     {normally, we'll use 1}
    cur_output_row : JDIMENSION;    {next row# to write to virtual array}
  end;


  
const
  INPUT_BUF_SIZE = 4096;

procedure init_source(cinfo : j_decompress_ptr); 
var
  src : my_src_ptr;
begin
  src := my_src_ptr(cinfo^.src);
  src^.start_of_file := TRUE;
end;

function fill_input_buffer(cinfo : j_decompress_ptr) : boolean; 
var
  src : my_src_ptr;
  nbytes : size_t;
begin
  src := my_src_ptr(cinfo^.src);
  nbytes := src^.infile.Read(src^.buffer^, INPUT_BUF_SIZE);
  if (nbytes <= 0) then begin
    if (src^.start_of_file) then   {Treat empty input file as fatal error}
      ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EMPTY);
    WARNMS(j_common_ptr(cinfo), JWRN_JPEG_EOF);
    {Insert a fake EOI marker}
    src^.buffer^[0] := JOCTET ($FF);
    src^.buffer^[1] := JOCTET (JPEG_EOI);
    nbytes := 2;
  end;
  src^.pub.next_input_byte := JOCTETptr(src^.buffer);
  src^.pub.bytes_in_buffer := nbytes;
  src^.start_of_file := FALSE;
  fill_input_buffer := TRUE;
end;

procedure skip_input_data(cinfo : j_decompress_ptr;
                      num_bytes : long); 
var
  src : my_src_ptr;
begin
  src := my_src_ptr (cinfo^.src);
  if (num_bytes > 0) then begin
    while (num_bytes > long(src^.pub.bytes_in_buffer)) do begin
      Dec(num_bytes, long(src^.pub.bytes_in_buffer));
      fill_input_buffer(cinfo);
      { note we assume that fill_input_buffer will never return FALSE,
        so suspension need not be handled. }
    end;
    Inc( src^.pub.next_input_byte, size_t(num_bytes) );
    Dec( src^.pub.bytes_in_buffer, size_t(num_bytes) );
  end;
end;    
 
procedure term_source(cinfo : j_decompress_ptr); 
begin
  { no work necessary here }
end;
    
procedure jpeg_stream_src(cinfo : j_decompress_ptr; const infile: TStream);
var
  src : my_src_ptr;
begin
  if (cinfo^.src = nil) then begin {first time for this JPEG object?}

    cinfo^.src := jpeg_source_mgr_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
                                  SIZEOF(my_source_mgr)) );
    src := my_src_ptr (cinfo^.src);
    src^.buffer := JOCTET_FIELD_PTR(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
                                  INPUT_BUF_SIZE * SIZEOF(JOCTET)) );
  end;
  src := my_src_ptr (cinfo^.src);
  {override pub's method pointers}
  src^.pub.init_source := @init_source;
  src^.pub.fill_input_buffer := @fill_input_buffer;
  src^.pub.skip_input_data := @skip_input_data;
  src^.pub.resync_to_restart := @jpeg_resync_to_restart; {use default method}
  src^.pub.term_source := @term_source;
  {define our fields}
  src^.infile := infile;
  src^.pub.bytes_in_buffer := 0;   {forces fill_input_buffer on first read}
  src^.pub.next_input_byte := nil; {until buffer loaded}
end;  

procedure error_exit (cinfo : j_common_ptr); 
var
  buffer : string;
begin
  buffer := '';
  cinfo^.err^.format_message(cinfo, buffer);
  raise EJPEG.Create(buffer);
end;

procedure emit_message (cinfo : j_common_ptr; msg_level : int); 
var
  err : jpeg_error_mgr_ptr;
begin
  err := cinfo^.err;
  if (msg_level < 0) then begin
    {It's a warning message. Since corrupt files may generate many warnings,}
    {the policy implemented here is to show only the first warning,}
    {unless trace_level >= 3}
    if (err^.num_warnings = 0) or (err^.trace_level >= 3) then
      err^.output_message(cinfo);
    {Always count warnings in num_warnings}
    Inc( err^.num_warnings );
  end else
    {It's a trace message. Show it if trace_level >= msg_level}
    if (err^.trace_level >= msg_level) then
      err^.output_message (cinfo);
end;

procedure output_message (cinfo : j_common_ptr); 
var
  buffer : string;
begin
  buffer := '';
  cinfo^.err^.format_message (cinfo, buffer);
  {message dialog}
  ShowMessage(buffer);
end;

procedure format_message (cinfo : j_common_ptr; var buffer : string); 
begin
  buffer := 'JPEG ERROR -- #' + IntToStr(cinfo^.err^.msg_code);
end;

procedure reset_error_mgr (cinfo : j_common_ptr); 
begin
  cinfo^.err^.num_warnings := 0;
  {trace_level is not reset since it is an application-supplied parameter}
  cinfo^.err^.msg_code := 0;      {may be useful as a flag for "no error"}
end;

function jpeg_my_error (var err : my_error_mgr) : jpeg_error_mgr_ptr;
begin
  {methods}
  err.pub.error_exit := @error_exit;
  err.pub.emit_message := @emit_message;
  err.pub.output_message := @output_message;
  err.pub.format_message := @format_message;
  err.pub.reset_error_mgr := @reset_error_mgr;
  {fields}
  err.pub.trace_level := 0;         {default := no tracing}
  err.pub.num_warnings := 0;        {no warnings emitted yet}
  err.pub.msg_code := 0;            {may be useful as a flag for "no error"}
  {message table(s)}
  err.pub.jpeg_message_table := nil;    {we don't want to use a static table}
  err.pub.last_jpeg_message := pred(JMSG_LASTMSGCODE);
  err.pub.addon_message_table := nil;
  err.pub.first_addon_message := JMSG_NOMESSAGE;   {for safety}
  err.pub.last_addon_message := JMSG_NOMESSAGE;
  {return result}
  jpeg_my_error := @err;
end;

function jinit_write_bmp (cinfo : j_decompress_ptr) : bmp_dest_ptr;
var
  dest : bmp_dest_ptr;
begin
  dest := bmp_dest_ptr (
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                  SIZEOF(bmp_dest_struct)) );
  jpeg_calc_output_dimensions(cinfo);
  dest^.data_width := cinfo^.output_width * cinfo^.output_components;
  dest^.row_width := dest^.data_width;
  while ((dest^.row_width and 3) <> 0) do
    Inc(dest^.row_width);
  dest^.pad_bytes := int(dest^.row_width-dest^.data_width);
  if (cinfo^.out_color_space = JCS_GRAYSCALE) then
    dest^.grayscale := True
  else if (cinfo^.out_color_space = JCS_RGB) then
    if (cinfo^.quantize_colors) then
      dest^.grayscale := True
    else
      dest^.grayscale := False
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BMP_COLORSPACE);
  {decompress buffer}
  dest^.buffer := cinfo^.mem^.alloc_sarray
    (j_common_ptr(cinfo), JPOOL_IMAGE, dest^.row_width, JDIMENSION (1));
  dest^.buffer_height := 1;
  dest^.cur_output_row := 0;
  {result}
  jinit_write_bmp := dest;
end;

procedure write_jpeg_pixelrow (cinfo : j_decompress_ptr;
                                            dest : bmp_dest_ptr;
                                            rows_supplied : JDIMENSION;
                                            img : TfpgImage);
var
  inptr: JSAMPLE_PTR;
  col : JDIMENSION;
 // pad : int;
  NewBGR: TRGBTriple;
  PDest: PLongWord;
begin
  inptr := JSAMPLE_PTR(dest^.buffer^[0]);
  
  PDest:= img.ImageData;
  inc(PDest, img.Width *rows_supplied);
  if not dest^.grayscale then
  begin
    for col := pred(cinfo^.output_width) downto 0 do
    begin
      fillchar(NewBGR,sizeof(NewBGR),0);
      NewBGR.Red:=inptr^;
      Inc(inptr);
      NewBGR.Green:=inptr^;
      Inc(inptr);
      NewBGR.Blue:=inptr^;
      Inc(inptr);
      PDest^ := RGBTripleTofpgColor(NewBGR);
      inc(PDest);
    end;
  end
  else
  begin
    for col := pred(cinfo^.output_width) downto 0 do
    begin
      NewBGR.Red:=inptr^;
      NewBGR.Green:=inptr^;
      NewBGR.Blue:=inptr^;
      NewBGR.Alpha:=inptr^;
      Inc(inptr);
      PDest^ := RGBTripleTofpgColor(NewBGR);
      inc(PDest);
    end;  
  end;
end;
      

procedure ReadImage_JPG(img: TfpgImage; bmp: TStream; const AScale: integer);
var
  cinfo : jpeg_decompress_struct;
  err   : my_error_mgr;
  dest  : bmp_dest_ptr;
begin
  if img = nil then
    Exit; //==>

  img.FreeImage;
      {initialize the JPEG decompression object with default error handling.}
  cinfo.err := jpeg_my_error(err);
  jpeg_create_decompress(@cinfo);
 try
    {specify the source of the compressed data}
  jpeg_stream_src(@cinfo, bmp);
    {obtain image info from header, set default decompression parameters}
  jpeg_read_header(@cinfo, TRUE);
 
  cinfo.scale_num := 1;
  case AScale of
    1: cinfo.scale_denom := 1;    // full size
    2: cinfo.scale_denom := 2;    // 1/2 size
    3: cinfo.scale_denom := 4;    // 1/4 size
    4: cinfo.scale_denom := 8;    // 1/8 size
    else
      cinfo.scale_denom := 1;     // defaults to full size
  end;

  dest := jinit_write_bmp(@cinfo); 
 
  img.AllocateImage(32, (cinfo.image_width + (cinfo.scale_denom-1)) div cinfo.scale_denom ,
                                (cinfo.image_height+ (cinfo.scale_denom-1)) div cinfo.scale_denom);  // color image 
    {prepare for decompression, initialize internal state}
  jpeg_start_decompress(@cinfo) ;
    {process data}
  while (cinfo.output_scanline < cinfo.output_height) do
  begin
     jpeg_read_scanlines(@cinfo, dest^.buffer, dest^.buffer_height);
     write_jpeg_pixelrow(@cinfo, dest,cinfo.output_scanline-1,img);
  end;
    {finish}
  jpeg_finish_decompress(@cinfo);
 finally
    {destroy}
  jpeg_destroy_decompress(@cinfo);
 end;
  img.UpdateImage;   
end;

function LoadImage_JPG(const AFileName: String; const AScale: integer): TfpgImage;
var
  inFile: TStream;
begin
  Result := nil;
  if not FileExists(AFileName) then
    Exit; //==>
  
  inFile:=TFileStream.Create(AFileName,fmOpenRead);
 try
  Result:=TfpgImage.Create;	
  ReadImage_JPG(Result, inFile, AScale);
 finally
  inFile.Free;
 end;
end;


end.

