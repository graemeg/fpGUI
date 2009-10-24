{
  Dumps the names of external database (help) files referenced by this file
}
unit readextfiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IPFFileFormatUnit, filestreamhelper;

procedure ProcessExtFiles(AIn: TFileStream; AOut: TFileTextStream);


implementation

uses
  readheader;

procedure ProcessExtFiles(AIn: TFileStream; AOut: TFileTextStream);
var
  count: integer;
  name: string;
  pData: pointer;
  p: pointer;
  pLength: pByte;
begin
  AOut.WriteLn('');
  AOut.WriteLn('External File References');

  if eHdr.NumDataBase > 0 then
  begin
    pData := nil;
    AIn.Seek(eHdr.DataBaseOffset, soBeginning);
    GetMem(pData, eHdr.DataBaseSize);     // allocate temp space for data
    AIn.Read(pData^, eHdr.DataBaseSize);  // read all data in one shot
    p := pData; // p is our incrementing position in the data
    for count := 0 to eHdr.NumDataBase-1 do
    begin
      pLength := p;       // length byte, including itself
      SetString(name, p+1, pLength^-1);   // use length value minus the length byte to get the string length
      AOut.WriteLn(Format('  File #%d: %s', [count, name]));
      inc(p, pLength^);   // skip to next entry using full length (including length byte)
    end;
    FreeMem(pData, eHdr.DataBaseSize);    // free allocated space
  end
  else
    AOut.WriteLn('  No external file references found');
end;

end.

