unit readheader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, filestreamhelper;

procedure ProcessHeader(AIn: TFileStream; AOut: TFileTextStream);

implementation

uses
  IPFFileFormatUnit;

type
  TWord = record
    b1: AnsiChar;
    b2: AnsiChar;
  end;

  Unsigned_31 = 0 .. (1 shl 31) - 1;  // 31 bit type

  TOverlayID = packed record
    b1: Byte;
    b2: Byte;
    b3: Byte;
  end;

  TOverlaySearchStart = bitpacked record
     SearchOffset: Unsigned_31;
     IsRec16bitSize: boolean;
  end;


procedure ProcessHeader(AIn: TFileStream; AOut: TFileTextStream);
var
  hdr: TPHelpFileHeader;
  bytes: integer;
  s: string;
  w: TWord;
  i: uint32;
  t0: TOverlayID;
  t1: TOverlaySearchStart;
begin
  New(hdr);
  try
    AIn.Seek(0, soBeginning);
    bytes := AIn.Read(hdr^, SizeOf(THelpFileHeader));
    if bytes <> SizeOf(THelpFileHeader) then
      raise Exception.Create('Failed to read complete file header');

    if hdr^.ID <> INF_HEADER_ID then
      raise Exception.Create('This is not an OS/2 help file');

    AOut.WriteLn('Header Section');
    t0 := TOverlayID(hdr^.ID);
    s := hdr^.ID;
    AOut.WriteLn(Format('  hdr.id:              %4.2x %2x %2x ("%s")            : Magic word' ,[Byte(hdr^.id[0]), Byte(hdr^.id[1]), Byte(hdr^.id[2]), s]));
    if (hdr^.flags and $01) > 0 then
      s := 'INF'
    else
      s := 'HLP';
    AOut.WriteLn(Format('  hdr.flags:             %8.2x (%s format)       : File format' ,[hdr^.flags, s]));
    AOut.WriteLn(Format('  hdr.size:              %8.4x (%0:7d bytes)    : Size of this header structure', [hdr^.hdrsize]));
    AOut.WriteLn(Format('  hdr.version:           %6d.%d                    : version of file format?', [hdr^.version_hi, hdr^.version_lo]));
    AOut.WriteLn(Format('  hdr.ntoc:              %8.4x (%0:13d)    : No of TOC entries', [hdr^.ntoc]));
    AOut.WriteLn(Format('  hdr.tocstart:          %8.8x (%0:7d bytes)    : 32bit file offset to start of TOC', [hdr^.tocstart]));
    AOut.WriteLn(Format('  hdr.toclen:            %8.8x (%0:7d bytes)    : bytes occupied by TOC entries', [hdr^.toclen]));
    AOut.WriteLn(Format('  hdr.tocoffsetsstart:   %8.8x (%0:7d bytes)    : file offset to array of TOC offsets', [hdr^.tocoffsetsstart]));
    AOut.WriteLn(Format('  hdr.nres:              %8.4x (%0:13d)    : number of panels with resource numbers', [hdr^.nres]));
    AOut.WriteLn(Format('  hdr.resstart:          %8.8x (%0:7d bytes)    : 32bit file offset of ressource number table', [hdr^.resstart]));
    AOut.WriteLn(Format('  hdr.nname:             %8.4x (%0:13d)    : number of panels with textual name', [hdr^.nname]));
    AOut.WriteLn(Format('  hdr.namestart:         %8.8x (%0:7d bytes)    : 32bit file offset to panel name table', [hdr^.namestart]));
    AOut.WriteLn(Format('  hdr.nindex:            %8.4x (%0:13d)    : number of index entries', [hdr^.nindex]));
    AOut.WriteLn(Format('  hdr.indexstart:        %8.8x (%0:7d bytes)    : 32bit file offset to index table', [hdr^.indexstart]));
    AOut.WriteLn(Format('  hdr.indexlen:          %8.8x (%0:7d bytes)    : size of index table', [hdr^.indexlen]));
    AOut.WriteLn(Format('  hdr.icmdCount:         %8.4x (%0:13d)    : number of icmd index items', [hdr^.icmdCount]));
    AOut.WriteLn(Format('  hdr.icmdOffset:        %8.8x (%0:7d bytes)    : file offset to icmd index items', [hdr^.icmdOffset]));
    AOut.WriteLn(Format('  hdr.icmdSize:          %8.8x (%0:7d bytes)    : size of icmd index table', [hdr^.icmdSize]));
    t1 := TOverlaySearchStart(hdr^.searchstart);
    i := t1.SearchOffset;
    AOut.WriteLn(Format('  hdr.searchstart :31    %8.8x (%0:7d bytes)    : 31 bit file offset of full text search table', [i, i]));
    if t1.IsRec16bitSize then
      s := 'search rec is 16bit size'
    else
      s := 'search rec is 8bit size';
    AOut.WriteLn(Format('  hdr.recSize :1     %s (%s) : if high bit set, search record size is 16bit', [BoolToStr(t1.IsRec16bitSize, True), s]));
    AOut.WriteLn(Format('  hdr.searchlen:         %8.8x (%0:7d bytes)    : size of full text search table', [hdr^.searchlen]));
    AOut.WriteLn(Format('  hdr.nslots:            %8.4x (%0:13d)    : number of "slots"', [hdr^.nslots]));
    AOut.WriteLn(Format('  hdr.slotsstart:        %8.8x (%0:7d bytes)    : 32bit file offset of the slots array', [hdr^.slotsstart]));
    AOut.WriteLn(Format('  hdr.dictlen:           %8.8x (%0:7d bytes)    : bytes occupied by the "dictionary"', [hdr^.dictlen]));
    AOut.WriteLn(Format('  hdr.ndict:             %8.4x (%0:13d)    : number of entries in the dictionary', [hdr^.ndict]));
    AOut.WriteLn(Format('  hdr.dictstart:         %8.8x (%0:7d bytes)    : 32bit file offset to start of dictionary', [hdr^.dictstart]));
    AOut.WriteLn(Format('  hdr.imgstart:          %8.8x (%0:7d bytes)    : 32bit file offset to image data', [hdr^.imgstart]));
    AOut.WriteLn(Format('  hdr.maxCVTIndex:       %8.2x (%0:13d)    : highest index inside panel''s local dictionary', [hdr^.maxCVTIndex]));
    AOut.WriteLn(Format('  hdr.nlsstart:          %8.8x (%0:7d bytes)    : 32bit file offset of NLS table', [hdr^.nlsstart, hdr^.nlsstart]));
    AOut.WriteLn(Format('  hdr.nlslen:            %8.8x (%0:7d bytes)    : size of NLS table', [hdr^.nlslen]));
    AOut.WriteLn(Format('  hdr.extstart:          %8.8x (%0:7d bytes)    : 32bit file offset of extended data block', [hdr^.extstart]));
    AOut.WriteLn(Format('  hdr.reserved: %2.2x %2.2x %2.2x %2.2x %2.2x %2.2x %2.2x %2.2x %2.2x %2.2x %2.2x %2.2x  : for future use. set to zero.',
        [hdr^.reserved[0], hdr^.reserved[1], hdr^.reserved[2], hdr^.reserved[3], hdr^.reserved[4], hdr^.reserved[5],
        hdr^.reserved[6], hdr^.reserved[7], hdr^.reserved[8], hdr^.reserved[9], hdr^.reserved[10], hdr^.reserved[11] ]));
    AOut.WriteLn(Format('  hdr.title:     "%s"  : ASCII title of database', [hdr^.title]));

{   title: array[ 0..47 ] of char;    // ASCII title of database
}
  finally
    Dispose(hdr);
  end;
end;

end.

