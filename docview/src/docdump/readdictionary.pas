{
  Dump the dictionary data
}
unit readdictionary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, filestreamhelper;

procedure ProcessDictionary(AIn: TFileStream; AOut: TFileTextStream);


implementation

uses
  IPFFileFormatUnit, readheader, u_Tools;


{ We read one dictionary string at a time. Not very efficient, but
  explains the workings of the dictionary in easy terms. }
function readDictString(AIn: TFileStream; out AText: string): integer;
var
  Len: uint8;
  p: pbyte;
  c: array[0..255] of char;
begin
    // adjust length so we can use as a Pascal string
    // (file uses length including length byte,
    //  Pascal string have length excluding length byte)
  FillChar(c, sizeof(c), 0);
  Len := AIn.ReadByte;            // first byte is the length of string + length byte
  Result := Len;
  Len := Len-1;                   // adjust length to exclude the length byte
  p := GetMem(Len);
  AIn.Read(p^, Len);            // read string of dictionary
  Move(p^, c, Len);             // copy string to char array
  AText := c;                   // convert Pchar to String type
  FreeMem(p);
end;

procedure ProcessDictionary(AIn: TFileStream; AOut: TFileTextStream);
var
  count: integer;
  t: string;
  size: integer;
begin
  AOut.WriteLn('');
  AOut.WriteLn('Dictionary (vocabulary list)');
  AIn.Seek(hdr.dictstart, soBeginning);
  for count := 0 to hdr.ndict-1 do
  begin
    size := readDictString(AIn, t);
    AOut.WriteLn(Format('  %4.4x (%4.0d):  %2.2x (%.2d)  [%s]', [count, count, size, size, t]));
  end;
end;

end.

