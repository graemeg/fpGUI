{
  Dump the Table of Contents data
}
unit readtoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, filestreamhelper;

procedure ProcessTOC(AIn: TFileStream; AOut: TFileTextStream);

implementation

uses
  IPFFileFormatUnit, readheader, u_Tools;

type
  TTOCOverlay = bitpacked record
    length: uint8; // length of the entry including this byte (but not including extended data)
    nestlevel: Unsigned_4;
    unknown: boolean;
    extended: boolean;
    hidden: boolean;
    haschildren: boolean;
    numSlots: uint8; // number of "slots" occupied by the text for this toc entry
  end;


  TTOCExtendedOverlay = bitpacked record
    setPos: boolean;
    setSize: boolean;
    setView: boolean;
    setStyle: boolean;
    noSearch: boolean;
    noPrint: boolean;
    setCtrl: boolean;
    setTutor: boolean;
    clear: boolean;
    unknown1: Unsigned_1;
    setGroup: boolean;
    isParent: boolean;
    unknown2: Unsigned_4;
  end;


procedure ProcessTOC(AIn: TFileStream; AOut: TFileTextStream);
var
  Count: integer;
  VisCount: integer;
  pOffsets: UInt32ArrayPointer;
  toc: TTOCEntryStart;
  olay: TTOCOverlay;
  tocextolay: TTOCExtendedOverlay;
  pData: pointer;
  pEntry: pTTOCEntryStart;
  pExtendedInfo: pExtendedTOCEntry;
  p: PByte;
  titleLen: integer;
  title: string;
  lOffset: uint32;
begin
  AOut.WriteLn('');
  AOut.WriteLn('Table of Contents');
  VisCount := 0;
  GetMem(pOffsets, SizeOf(uint32) * hdr.ntoc);
  AIn.Seek(hdr.tocoffsetsstart, soBeginning);
  AIn.Read(pOffsets^, SizeOf(uint32) * hdr.ntoc); // now we have array of toc offsets

  AIn.Seek(hdr.tocstart, soBeginning);
  GetMem(pData, hdr.toclen);
  AIn.Read(pData^, hdr.toclen);
  pEntry := pData;
  for count := 0 to hdr.ntoc-1 do
  begin
//    AIn.Read(toc, SizeOf(TTOCEntryStart));
//    FillChar(olay, SizeOf(TTOCOverlay), 0);
    p := PByte(pEntry) + sizeof(TTOCEntryStart);

    olay.extended  :=  (pEntry^.flags and TOCEntryExtended ) = TOCEntryExtended;
    olay.nestlevel := (pEntry^.flags and TOCEntryLevelMask);
    olay.hidden := (pEntry^.flags and TOCEntryHidden) = TOCEntryHidden;
    olay.haschildren := (pEntry^.flags and TOCEntryHasChildren) = TOCEntryHasChildren;

    lOffset := hdr.tocstart + (pEntry-pData);
    AOut.WriteLn(Format('  TOC Entry #%d at offset %8.8x (%d bytes)', [count, lOffset, lOffset]));
    AOut.WriteLn(Format('    tocentry.length:      %2.2x (%0:d bytes)', [pEntry^.length]));
    AOut.WriteLn(Format('    tocentry.nestlevel:   %d', [olay.nestlevel]));
    AOut.WriteLn(Format('    tocentry.unknown:     %s', [iif(olay.unknown, 'set', 'clear')]));
    AOut.WriteLn(Format('    tocentry.extended:    %s', [iif(olay.extended, 'yes', 'no')]));
    AOut.WriteLn(Format('    tocentry.hidden:      %s', [iif(olay.hidden, 'yes', 'no')]));
    AOut.WriteLn(Format('    tocentry.haschildren: %s', [iif(olay.haschildren, 'yes', 'no')]));
    AOut.WriteLn(Format('    tocentry.numSlots:    %d', [pEntry^.numSlots]));
    if not olay.hidden then
      inc(VisCount);
    if olay.extended then
    begin
      pExtendedInfo := pExtendedTOCEntry( p );  // next data to follow must be Extended TOC Entry

      AOut.WriteLn('    Extended TOC Entry');
      tocextolay := TTOCExtendedOverlay(pExtendedInfo^);

      AOut.Writeln(Format('      ExtTocEntry.setPos:    %s', [iif(tocextolay.setPos, 'yes', 'no')]));
      AOut.Writeln(Format('      ExtTocEntry.setSize:   %s', [iif(tocextolay.setSize, 'yes', 'no')]));
      AOut.Writeln(Format('      ExtTocEntry.setView:   %s', [iif(tocextolay.setView, 'yes', 'no')]));
      AOut.Writeln(Format('      ExtTocEntry.setStyle:  %s', [iif(tocextolay.setStyle, 'yes', 'no')]));
      AOut.Writeln(Format('      ExtTocEntry.noSearch:  %s', [iif(tocextolay.noSearch, 'yes', 'no')]));
      AOut.Writeln(Format('      ExtTocEntry.noPrint:   %s', [iif(tocextolay.noPrint, 'yes', 'no')]));
      AOut.Writeln(Format('      ExtTocEntry.setCtrl:   %s', [iif(tocextolay.setCtrl, 'yes', 'no')]));
      AOut.Writeln(Format('      ExtTocEntry.setTutor:  %s', [iif(tocextolay.setTutor, 'yes', 'no')]));
      AOut.Writeln(Format('      ExtTocEntry.clear:     %s', [iif(tocextolay.clear, 'clear', 'set')]));
      AOut.Writeln(Format('      ExtTocEntry.unknown1:  %1.1x', [tocextolay.unknown1]));
      AOut.Writeln(Format('      ExtTocEntry.setGroup:  %s', [iif(tocextolay.setGroup, 'yes', 'no')]));
      AOut.Writeln(Format('      ExtTocEntry.isParent:  %s', [iif(tocextolay.isParent, 'yes', 'no')]));
      AOut.Writeln(Format('      ExtTocEntry.unknown2:  %1.1x', [tocextolay.unknown2]));

      inc( p, sizeof( TExtendedTOCEntry ) );    // move p past two flag bytes

      if ( pExtendedInfo^.w1 and 1 ) > 0 then
        // skip position
        inc( p, sizeof( THelpXYPair ) );

      if ( pExtendedInfo^.w1 and 2 ) > 0 then
        // skip size
        inc( p, sizeof( THelpXYPair ) );

      if ( pExtendedInfo^.w1 and 8 ) > 0 then
        // skip window controls
        inc( p, sizeof(word) );    // increment by 2

      if ( pExtendedInfo^.w1 and $40 ) > 0 then
        // skip something else, unknown... style? 2 bytes
        inc( p, sizeof(word) );    // increment by 2

      if ( pExtendedInfo^.w2 and 4 ) > 0 then
      begin
//        _ContentsGroupIndex := pUInt16(p)^;
        // read group
        inc( p, sizeof( uint16 ) );
      end;
    end;

    // skip slot numbers for now.
//    _pSlotNumbers := pUInt16(p);
    inc( p, pEntry^.numSlots * sizeof(uint16) );

  // Calculate the remainder of the tocentry length - that is the bytes used for TOC topic (title) text
  titleLen := pEntry^.length - ( longword( p ) - longword( pEntry ) );

  // Read title
  if TitleLen > 0 then
    SetString(Title, Pointer(p), TitleLen)
  else
    Title := '<unknown>';
  AOut.WriteLn(Format('    toc Title:            %s', [title]));


    p := PByte(pEntry);
    inc(p, pEntry^.Length);
    pEntry := pTTOCEntryStart(p);
  end;

  AOut.WriteLn(Format('  TOC visible count:  %d', [VisCount]));
  FreeMem(pOffsets, SizeOf(uint32) * hdr.ntoc);
  FreeMem(pData, hdr.toclen);
end;

end.

