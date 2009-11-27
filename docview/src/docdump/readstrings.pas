{
  Dump the String table data
}
unit readstrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, filestreamhelper;

procedure ProcessStringsTable(AIn: TFileStream; AOut: TFileTextStream);


implementation

uses
  readheader;

procedure ProcessStringsTable(AIn: TFileStream; AOut: TFileTextStream);
var
  name: string;
  pData: pointer;
  p: pointer;
  pLength: pByte;
  bytes: integer;
begin
  AOut.WriteLn('');
  AOut.WriteLn('Strings Data');

  if eHdr.StringsSize > 0 then
  begin
    pData := nil;
    AIn.Seek(eHdr.StringsOffset, soBeginning);
    GetMem(pData, eHdr.StringsSize);     // allocate temp space for data
    AIn.Read(pData^, eHdr.StringsSize);  // read all data in one shot
    p := pData; // p is our incrementing position in the data
    bytes := 0;
    while bytes < eHdr.StringsSize do;
    begin
      pLength := p;       // length byte, including itself
      bytes := bytes + pLength^;
      SetString(name, p+1, pLength^-1);   // use length value minus the length byte to get the string length
      AOut.WriteLn(Format('  %s', [name]));
      inc(p, pLength^);   // skip to next entry using full length (including length byte)
    end;
    FreeMem(pData, eHdr.StringsSize);    // free allocated space
  end
  else
    AOut.WriteLn('  There are no strings');

end;

end.

