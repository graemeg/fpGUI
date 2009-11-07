{
  Dump the INF header & extended header structures to a text file
}
unit readheader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, filestreamhelper, IPFFileFormatUnit;

procedure ProcessHeader(AIn: TFileStream; AOut: TFileTextStream);

var
  hdr: THelpFileHeader;
  eHdr: TExtendedHelpFileHeader;


implementation


type
  TWord = record
    b1: AnsiChar;
    b2: AnsiChar;
  end;

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
  bytes: integer;
  s: string;
  w: TWord;
  i: uint32;
  t0: TOverlayID;
  t1: TOverlaySearchStart;
begin
  try
    AIn.Seek(0, soBeginning);
    bytes := AIn.Read(hdr, SizeOf(THelpFileHeader));
    if bytes <> SizeOf(THelpFileHeader) then
      raise Exception.Create('Failed to read complete file header');

    if hdr.ID <> INF_HEADER_ID then
      raise Exception.Create('This is not an OS/2 help file');

    AOut.WriteLn('Header Section');
    t0 := TOverlayID(hdr.ID);
    s := hdr.ID;
    AOut.WriteLn(Format('  ipfheader.id:              %4.2x %2x %2x ("%s")            : Magic word' ,[Byte(hdr.id[0]), Byte(hdr.id[1]), Byte(hdr.id[2]), s]));
    if (hdr.flags and $01) > 0 then
      s := 'INF'
    else
      s := 'HLP';
    AOut.WriteLn(Format('  ipfheader.flags:             %8.2x (%s format)       : File format' ,[hdr.flags, s]));
    AOut.WriteLn(Format('  ipfheader.size:              %8.4x (%0:7d bytes)    : Size of this header structure', [hdr.hdrsize]));
    AOut.WriteLn(Format('  ipfheader.version:           %6d.%d                    : version of file format?', [hdr.version_hi, hdr.version_lo]));
    AOut.WriteLn(Format('  ipfheader.ntoc:              %8.4x (%0:13d)    : No of TOC entries', [hdr.ntoc]));
    AOut.WriteLn(Format('  ipfheader.tocstart:          %8.8x (%0:7d bytes)    : 32bit file offset to start of TOC', [hdr.tocstart]));
    AOut.WriteLn(Format('  ipfheader.toclen:            %8.8x (%0:7d bytes)    : bytes occupied by TOC entries', [hdr.toclen]));
    AOut.WriteLn(Format('  ipfheader.tocoffsetsstart:   %8.8x (%0:7d bytes)    : file offset to array of TOC offsets', [hdr.tocoffsetsstart]));
    AOut.WriteLn(Format('  ipfheader.nres:              %8.4x (%0:13d)    : number of panels with resource numbers', [hdr.nres]));
    AOut.WriteLn(Format('  ipfheader.resstart:          %8.8x (%0:7d bytes)    : 32bit file offset of ressource number table', [hdr.resstart]));
    AOut.WriteLn(Format('  ipfheader.nname:             %8.4x (%0:13d)    : number of panels with textual name', [hdr.nname]));
    AOut.WriteLn(Format('  ipfheader.namestart:         %8.8x (%0:7d bytes)    : 32bit file offset to panel name table', [hdr.namestart]));
    AOut.WriteLn(Format('  ipfheader.nindex:            %8.4x (%0:13d)    : number of index entries', [hdr.nindex]));
    AOut.WriteLn(Format('  ipfheader.indexstart:        %8.8x (%0:7d bytes)    : 32bit file offset to index table', [hdr.indexstart]));
    AOut.WriteLn(Format('  ipfheader.indexlen:          %8.8x (%0:7d bytes)    : size of index table', [hdr.indexlen]));
    AOut.WriteLn(Format('  ipfheader.icmdCount:         %8.4x (%0:13d)    : number of icmd index items', [hdr.icmdCount]));
    AOut.WriteLn(Format('  ipfheader.icmdOffset:        %8.8x (%0:7d bytes)    : file offset to icmd index items', [hdr.icmdOffset]));
    AOut.WriteLn(Format('  ipfheader.icmdSize:          %8.8x (%0:7d bytes)    : size of icmd index table', [hdr.icmdSize]));
    t1 := TOverlaySearchStart(hdr.searchstart);
    i := t1.SearchOffset;
    AOut.WriteLn(Format('  ipfheader.searchstart :31    %8.8x (%0:7d bytes)    : 31bit file offset of full text search table', [i, i]));
    if t1.IsRec16bitSize then
      s := 'search rec is 16bit size'
    else
      s := 'search rec is 8bit size';
    AOut.WriteLn(Format('  ipfheader.recSize :1     %s (%s) : if high bit set, search record size is 16bit', [BoolToStr(t1.IsRec16bitSize, True), s]));
    AOut.WriteLn(Format('  ipfheader.searchlen:         %8.8x (%0:7d bytes)    : size of full text search table', [hdr.searchlen]));
    AOut.WriteLn(Format('  ipfheader.nslots:            %8.4x (%0:13d)    : number of "slots"', [hdr.nslots]));
    AOut.WriteLn(Format('  ipfheader.slotsstart:        %8.8x (%0:7d bytes)    : 32bit file offset of the slots array', [hdr.slotsstart]));
    AOut.WriteLn(Format('  ipfheader.dictlen:           %8.8x (%0:7d bytes)    : bytes occupied by the "dictionary"', [hdr.dictlen]));
    AOut.WriteLn(Format('  ipfheader.ndict:             %8.4x (%0:13d)    : number of entries in the dictionary', [hdr.ndict]));
    AOut.WriteLn(Format('  ipfheader.dictstart:         %8.8x (%0:7d bytes)    : 32bit file offset to start of dictionary', [hdr.dictstart]));
    AOut.WriteLn(Format('  ipfheader.imgstart:          %8.8x (%0:7d bytes)    : 32bit file offset to image data', [hdr.imgstart]));
    AOut.WriteLn(Format('  ipfheader.maxCVTIndex:       %8.2x (%0:13d)    : highest index inside panel''s local dictionary', [hdr.maxCVTIndex]));
    AOut.WriteLn(Format('  ipfheader.nlsstart:          %8.8x (%0:7d bytes)    : 32bit file offset of NLS table', [hdr.nlsstart, hdr.nlsstart]));
    AOut.WriteLn(Format('  ipfheader.nlslen:            %8.8x (%0:7d bytes)    : size of NLS table', [hdr.nlslen]));
    AOut.WriteLn(Format('  ipfheader.extstart:          %8.8x (%0:7d bytes)    : 32bit file offset of extended data block', [hdr.extstart]));
    AOut.WriteLn(Format('  ipfheader.reserved: %2.2x %2.2x %2.2x %2.2x %2.2x %2.2x %2.2x %2.2x %2.2x %2.2x %2.2x %2.2x  : for future use. set to zero.',
        [hdr.reserved[0], hdr.reserved[1], hdr.reserved[2], hdr.reserved[3], hdr.reserved[4], hdr.reserved[5],
        hdr.reserved[6], hdr.reserved[7], hdr.reserved[8], hdr.reserved[9], hdr.reserved[10], hdr.reserved[11] ]));
    AOut.WriteLn(Format('  ipfheader.title:     "%s"  : ASCII title of database', [hdr.title]));

    AOut.WriteLn('');
    AOut.WriteLn('Extended Header Section');
    AIn.Seek(hdr.extstart, soBeginning);
    AIn.Read(eHdr, SizeOf(TExtendedHelpFileHeader));
    AOut.WriteLn(Format('  extheader.NumFontEntry       %8.4x (%0:13d)   : Font Table - number of entries', [eHdr.NumFontEntry]));
    AOut.WriteLn(Format('  extheader.FontTableOffset    %8.8x (%0:7d bytes)   : Font Table - 32bit offset in file', [eHdr.FontTableOffset]));
    AOut.WriteLn(Format('  extheader.NumDataBase        %8.4x (%0:13d)   : Data Base - No of files', [eHdr.NumDataBase]));
    AOut.WriteLn(Format('  extheader.DataBaseOffset     %8.8x (%0:7d bytes)   : Data Base - 32bit offset in file', [eHdr.DataBaseOffset]));
    AOut.WriteLn(Format('  extheader.DataBaseSize       %8.8x (%0:7d bytes)   : Data Base - Size in bytse', [eHdr.DataBaseSize]));
    AOut.WriteLn(Format('  extheader.EntryInGNameTable  %8.4x (%0:13d)   : Global Names - No entries', [eHdr.EntryInGNameTable]));
    AOut.WriteLn(Format('  extheader.HelpPanelGNameTblOffset %8.8x (%0:7d bytes)   : Global Names - 32bit offset in file', [eHdr.HelpPanelGNameTblOffset]));
    AOut.WriteLn(Format('  extheader.StringsOffset      %8.8x (%0:7d bytes)   : Strings - 32bit offset in file', [eHdr.StringsOffset]));
    AOut.WriteLn(Format('  extheader.StringsSize        %8.4x (%0:7d bytes)   : Strings - Total bytes of all strings', [eHdr.StringsSize]));
    AOut.WriteLn(Format('  extheader.ChildPagesOffset   %8.8x (%0:7d bytes)   : Child Pages - 32bit offset in file', [eHdr.ChildPagesOffset]));
    AOut.WriteLn(Format('  extheader.ChildPagesSize     %8.8x (%0:7d bytes)   : Child Pages - Total bytes of all strings', [eHdr.ChildPagesSize]));
    AOut.WriteLn(Format('  extheader.NumGIndexEntry     %8.8x (%0:13d)   : Total number of Global Index items', [eHdr.NumGIndexEntry]));
    AOut.WriteLn(Format('  extheader.CtrlOffset         %8.8x (%0:7d bytes)   : Ctrl Buttons : offset in file', [eHdr.CtrlOffset]));
    AOut.WriteLn(Format('  extheader.CtrlSize           %8.8x (%0:7d bytes)   : Ctrl Buttons : size in bytes', [eHdr.CtrlSize]));
    AOut.WriteLn(Format('  extheader.reserved:   %8.8x %8.8x %8.8x %8.8x  : for future use. set to zero.',
        [eHdr.reserved[0], eHdr.reserved[1], eHdr.reserved[2], eHdr.reserved[3]]));

  finally
    // no nothing
  end;
end;

end.

